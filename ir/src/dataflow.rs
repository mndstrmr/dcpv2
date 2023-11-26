use std::collections::{HashMap, HashSet};

use crate::{Cfg, Instr, Name, Binding, Expr, CfgBlock, Abi, FuncArg, Typ};

fn might_read_before_write(abi: &Abi, cfg: &Cfg, blocks: &[CfgBlock], name: Name, node: usize, idx: usize, visited: &mut HashSet<usize>) -> bool {
    if !visited.insert(node) {
        return false
    }

    for stmt in idx..blocks[node].code.len() {
        let mut vars = blocks[node].code[stmt].vars();

        if let Instr::Return { .. } = &blocks[node].code[stmt] {
            vars.add_all_reads(abi.caller_read.iter().copied());
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
            if let Expr::Name(old_name) = src {
                let mut j = i + 1;
                while j < blocks[node].code.len() {
                    if blocks[node].code[j].vars().writes(name) {
                        break;
                    }
                    j += 1;
                }

                if j != blocks[node].code.len() || cfg.outgoing_for(node).is_empty() {
                    let old_name = *old_name;
                    blocks[node].code.remove(i);
                    j -= 1;
                    for k in i..=j.min(blocks[node].code.len() - 1) {
                        blocks[node].code[k].visit_top_exprs_mut(&mut |e| {
                            e.visit_mut_post(&mut |e| if let Expr::Name(nm) = e && *nm == name {
                                *nm = old_name;
                            });
                        });
                    }
                    continue 'outer
                }
            }

            last_write_locations.insert(name, i);
        }

        i += 1;
    }
}

pub fn inline_single_use_pairs(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        inline_single_use_pairs_in(abi, cfg, blocks, i)
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

pub fn remove_dead_writes(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        remove_dead_writes_in(abi, cfg, blocks, i)
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
