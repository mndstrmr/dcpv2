use std::collections::{HashMap, HashSet};

use crate::{Cfg, Instr, Name, Binding, Expr, CfgBlock};

fn might_read_before_write(cfg: &Cfg, blocks: &[CfgBlock], name: Name, node: usize, idx: usize, visited: &mut HashSet<usize>) -> bool {
    if !visited.insert(node) {
        return false
    }

    for stmt in idx..blocks[node].code.len() {
        let vars = blocks[node].code[stmt].vars();
        
        if vars.reads_before_writes(name) {
            return true
        } else if vars.writes(name) {
            return false
        }
    }

    for outgoing in cfg.outgoing_for(node) {
        if might_read_before_write(cfg, blocks, name, *outgoing, 0, visited) {
            return true
        }
    }

    false
}

fn maybe_inline_single_use_pair(cfg: &Cfg, blocks: &mut [CfgBlock], node: usize, src: usize, dst: usize) -> bool {
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

    let deps = val.vars();

    for i in src + 1..dst {
        let vars = blocks[node].code[i].vars();

        if vars.writes(name) {
            // name was overwritten
            return false
        }

        for dep in &deps {
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
        if might_read_before_write(cfg, blocks, name, node, dst + 1, &mut visited) {
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

fn inline_single_use_pairs_in(cfg: &Cfg, blocks: &mut [CfgBlock], node: usize) {
    let mut last_write_locations = HashMap::<Name, usize>::new();
    let mut i = 0;

    'outer: while i < blocks[node].code.len() {
        let vars = blocks[node].code[i].vars();
        for var in vars.read_names() {
            if let Some(location) = last_write_locations.get(var) {
                if maybe_inline_single_use_pair(cfg, blocks, node, *location, i) {
                    // Safe to leave the write in the map since it was proved that the name is never read
                    continue 'outer
                }
            }
        }

        if let Instr::Store { dest: Binding::Name(name), .. } = &blocks[node].code[i] {
            last_write_locations.insert(*name, i);
        }

        i += 1;
    }
}

pub fn inline_single_use_pairs(cfg: &Cfg, blocks: &mut [CfgBlock]) {
    for i in 0..blocks.len() {
        inline_single_use_pairs_in(cfg, blocks, i)
    }
}
