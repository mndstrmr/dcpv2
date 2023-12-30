use std::collections::{HashMap, HashSet};

use crate::{Cfg, Instr, Name, Binding, Expr, CfgBlock, Abi, FuncArg, Typ};

/// Determine if there is any control flow path in which the given name is read before it is written
fn might_read_before_write(abi: &Abi, cfg: &Cfg, blocks: &[CfgBlock], name: Name, node: usize, idx: usize, visited: &mut HashSet<usize>) -> bool {
    if !visited.insert(node) {
        return false
    }

    for stmt in idx..blocks[node].code.len() {
        let mut vars = blocks[node].code[stmt].vars();

        if let Instr::Return { .. } = &blocks[node].code[stmt] {
            vars.add_all_reads(abi.caller_read.iter().copied());
        }

        let mut contains_call = false;
        blocks[node].code[stmt].visit_top_exprs(&mut |e| contains_call = contains_call || e.contains_call());
        if contains_call && abi.called_read.contains(&name) {
            return true
        }
        
        if vars.reads_before_writes(name) {
            return true
        } else if vars.writes(name) {
            return false
        }
    }

    for outgoing in cfg.outgoing_for(node) {
        if might_read_before_write(abi, cfg, blocks, name, *outgoing, 0, visited) {
            return true
        }
    }

    false
}

/// Determine if in all control flow paths to this point the given name is written to directly, assuming name is in abi.func_args
pub(crate) fn did_controlled_write(abi: &Abi, cfg: &Cfg, blocks: &[CfgBlock], own_args: &[FuncArg], name: Name, node: usize, idx: usize, visited: &mut HashSet<usize>) -> bool {
    if !visited.insert(node) {
        return false
    }

    for stmt in (0..idx).rev() {
        let vars = blocks[node].code[stmt].vars();

        if vars.writes(name) {
            return true
        }

        let mut contains_call = false;
        blocks[node].code[stmt].visit_top_exprs(&mut |e| contains_call = contains_call || e.contains_call());

        if contains_call {
            return false
        }
    }

    if cfg.incoming_for(node).is_empty() {
        // This is the entry node for this function. Name could be an argument to this function
        for own in own_args {
            if own.name == name {
                return true
            }
        }

        return false
    }

    for incoming in cfg.incoming_for(node) {
        if !did_controlled_write(abi, cfg, blocks, own_args, name, *incoming, blocks[*incoming].code.len(), visited) {
            return false
        }
    }

    true
}

fn maybe_inline_single_use_pair(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], node: usize, src: usize, dst: usize) -> bool {
    assert!(src < dst);

    let Instr::Store { dest: Binding::Name(name), src: val, .. } = &blocks[node].code[src] else {
        panic!("Bad pair");
    };
    let name = *name;

    let dst_vars = blocks[node].code[dst].vars();
    if dst_vars.read_count(name) != 1 {
        // name is used twice in the destination
        return false
    }

    let mut deps = val.vars();
    let mut has_call = false;
    val.visit(&mut |e| if let Expr::Call(_, _) = e {
        has_call = true;
    });
    if has_call {
        deps.add_all_reads(abi.caller_read.iter().copied());
    }

    for i in src + 1..dst {
        let vars = blocks[node].code[i].vars();

        if vars.writes(name) {
            // name was overwritten
            return false
        }

        for dep in deps.read_names() {
            if vars.writes(*dep) {
                // Dependency was overwritten
                return false
            }
        }

        let mut contains_call = false;
        blocks[node].code[i].visit_top_exprs(&mut |e| contains_call = contains_call || e.contains_call());
        if contains_call && abi.called_read.contains(&name) {
            // name not single use due to being read by a called function (.e.g the sp)
            return false
        }

        if vars.reads_before_writes(name) {
            // name was not single use
            return false
        }
    }
    
    if !dst_vars.writes(name) {
        let mut visited = HashSet::new();
        if might_read_before_write(abi, cfg, blocks, name, node, dst + 1, &mut visited) {
            return false
        }
    }

    // Between src and dst,
    // - name is never overwritten
    // - none of the dependencies of in src was overwritten
    // - name is not read
    // After dst
    // - name is never read (before it is written)

    let Instr::Store { src: val, .. } = blocks[node].code.remove(src) else { unreachable!() };
    let mut val = Some(val);

    blocks[node].code[dst - 1].visit_top_exprs_mut(&mut |e| {
        e.visit_mut_post(&mut |e| if let Expr::Name(nm) = e && *nm == name {
            *e = val.take().unwrap();
        });
    });

    true
}

fn inline_single_use_pairs_in(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], node: usize) {
    let mut last_write_locations = HashMap::<Name, usize>::new();
    let mut i = 0;

    'outer: while i < blocks[node].code.len() {
        let vars = blocks[node].code[i].vars();
        for var in vars.read_names() {
            if let Some(location) = last_write_locations.get(var) {
                if maybe_inline_single_use_pair(abi, cfg, blocks, node, *location, i) {
                    last_write_locations.clear();
                    i = 0;
                    continue 'outer
                }
            }
        }

        if let Instr::Store { dest: Binding::Name(name), src, .. } = &blocks[node].code[i] {
            let name = *name;

            // Special case where the assignment is just name1 = name2
            // If a write to name1 or name2 can be found in the following code we can replace all occurrences
            // of name1 with name2 between the two
            // We could also sometimes remove the name1 = name2 statement, but we will let dead writes do that
            if let Expr::Name(old_name) = src {
                let mut j = i + 1;
                while j < blocks[node].code.len() {
                    if blocks[node].code[j].vars().writes(name) || blocks[node].code[j].vars().writes(*old_name) {
                        break;
                    }
                    j += 1;
                }

                if j != blocks[node].code.len() || cfg.outgoing_for(node).is_empty() {
                    // i. name1 = name2
                    // ...
                    // j. name1 = ... or name2 = ...
                    // Replace name1 with name2 in (i, j] and remove i

                    let range = (i + 1)..=j.min(blocks[node].code.len() - 1);

                    let old_name = *old_name;
                    for instr in &mut blocks[node].code[range] {
                        instr.visit_top_exprs_mut(&mut |e| {
                            e.visit_mut_post(&mut |e| if let Expr::Name(nm) = e && *nm == name {
                                *nm = old_name;
                            });
                        });
                    }
                    i += 1;
                    continue 'outer
                }
            }

            last_write_locations.insert(name, i);
        }

        i += 1;
    }
}

/// Reduce a = b; x = a into x = b when
/// - The two statements are in the same basic block
/// - There is exactly one read of a before it is next written to (unless b is a name, in which case
/// every occurrence of a will be replaced by b in this basic block)
/// - None of the variables read in b are changed between a and b
pub fn inline_single_use_pairs(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        inline_single_use_pairs_in(abi, cfg, blocks, i)
    }
}

fn no_remove_inline_strings_in(blocks: &mut [CfgBlock], node: usize) {
    let mut write_values = HashMap::new();

    for instr in &mut blocks[node].code {
        if let Instr::Store { dest: Binding::Name(name), src: Expr::StringLit(str), .. } = instr {
            write_values.insert(*name, str.clone());
            continue
        }

        instr.visit_top_exprs_mut(&mut |e| {
            e.visit_mut_post(&mut |e| if let Expr::Name(name) = e && let Some(value) = write_values.get(name) {
                *e = Expr::StringLit(value.clone());
            })
        });

        if let Instr::Store { dest: Binding::Name(name), .. } = instr {
            write_values.remove(name);
        }
    }
}

/// Inline string constant assignments
pub fn no_remove_inline_strings(blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        no_remove_inline_strings_in(blocks, i)
    }
}

fn remove_dead_writes_in(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], node: usize) {
    let mut i = 0;
    while i < blocks[node].code.len() {
        let Instr::Store { dest: Binding::Name(name), src, .. } = &blocks[node].code[i] else {
            i += 1;
            continue
        };

        let mut has_call = false;
        src.visit(&mut |e| if let Expr::Call(_, _) = e {
            has_call = true;
        });

        if has_call {
            i += 1;
            continue
        }

        let mut visited = HashSet::new();
        if might_read_before_write(abi, cfg, blocks, *name, node, i + 1, &mut visited) {
            i += 1;
            continue
        }

        blocks[node].code.remove(i);
        i = 0;
    }
}

/// Remove writes where the value is never read
pub fn remove_dead_writes(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        remove_dead_writes_in(abi, cfg, blocks, i)
    }
}

fn demote_dead_calls_in(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], node: usize, sometimes_read: &mut HashSet<Name>) {
    let mut i = 0;
    while i < blocks[node].code.len() {
        let Instr::Store { dest: Binding::Name(name), src: Expr::Call(func, _), .. } = &mut blocks[node].code[i] else {
            i += 1;
            continue
        };
        let name = *name;

        let func_name = match &**func {
            Expr::Name(name) => Some(*name),
            _ => None
        };

        let mut visited = HashSet::new();
        if might_read_before_write(abi, cfg, blocks, name, node, i + 1, &mut visited) {
            i += 1;
            if let Some(name) = func_name {
                sometimes_read.insert(name);
            }
            continue
        }

        let Instr::Store { src, loc, .. } = &mut blocks[node].code[i] else {
            unreachable!()
        };
        blocks[node].code[i] = Instr::Expr { loc: *loc, expr: src.take() };
        i += 1;
    }
}

/// Replace x = a() with a() if x is never read
pub fn demote_dead_calls(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], sometimes_read: &mut HashSet<Name>) {
    for i in 0..blocks.len() {
        demote_dead_calls_in(abi, cfg, blocks, i, sometimes_read)
    }
}

pub fn infer_func_args(abi: &Abi, cfg: &Cfg, blocks: &[CfgBlock]) -> Vec<FuncArg> {
    let mut args = Vec::new();

    for (a, arg) in abi.func_args.iter().enumerate() {
        let mut visited = HashSet::new();
        if might_read_before_write(abi, cfg, blocks, *arg, 0, 0, &mut visited) {
            for i in args.len()..=a {
                args.push(FuncArg {
                    name: abi.func_args[i],
                    typ: Typ::N64 // Not sure how best to obtain this
                });
            }
        }
    }

    args
}
