use std::collections::HashSet;

use crate::{Cfg, CfgBlock, Instr, Label};

fn explore_reachable(cfg: &Cfg, bubble: &HashSet<usize>, root: usize) -> HashSet<usize> {
    let mut reachable = HashSet::new();

    if !bubble.contains(&root) {
        return reachable
    }
    reachable.insert(root);
    let mut to_visit = vec![root];

    while let Some(node) = to_visit.pop() {
        for child in cfg.outgoing_for(node) {
            if !cfg.is_backward_edge(node, *child) && reachable.insert(*child) && bubble.contains(child) {
                to_visit.push(*child);
            }
        }
    }

    reachable
}

fn generate_ifs_in(blocks: &mut Vec<CfgBlock>, bubble: &HashSet<usize>, cfg: &Cfg, root: usize) -> Vec<Instr> {
    assert!(!blocks.is_empty());
    let mut node_id = root;
    let mut code = Vec::<Instr>::new();

    let mut outgoing = cfg.outgoing_for(node_id);

    let can_branch_to = |src, dst| !cfg.is_backward_edge(src, dst) && bubble.contains(&dst);

    loop {
        let block = blocks.get_mut(node_id).expect("block does not exist");
        if block.code.is_empty() {
            code.push(Instr::Branch { loc: block.loc, cond: None, label: Label(node_id) });
            break code
        }
        code.push(Instr::Label { loc: block.loc, label: Label(block.label) });
        code.extend(block.code.drain(..));

        match outgoing.len() {
            0 => break code,
            1 => {
                let new_node_id = *outgoing.iter().next().unwrap();
                if !can_branch_to(node_id, new_node_id) {
                    code.push(Instr::Branch {
                        loc: code.last().unwrap().loc(),
                        label: Label(new_node_id),
                        cond: None
                    });
                    break code
                }
                outgoing = cfg.outgoing_for(new_node_id);
                node_id = new_node_id;
            }
            2 => {
                let mut root_a = *outgoing.iter().nth(0).unwrap();
                let mut root_b = *outgoing.iter().nth(1).unwrap();

                let Instr::Branch { label, cond: Some(cond), loc } = code.pop().unwrap() else {
                    panic!("bad terminator")
                };

                if root_a != label.0 {
                    (root_a, root_b) = (root_b, root_a)
                }
                assert_eq!(root_a, label.0);

                // Not wrong, but probably could be improved
                if !can_branch_to(node_id, root_a) && !can_branch_to(node_id, root_b) {
                    code.push(Instr::Branch { label, cond: Some(cond), loc });
                    code.push(Instr::Branch { label: Label(root_b), cond: None, loc });
                    break code
                } else if !can_branch_to(node_id, root_a) && can_branch_to(node_id, root_b) {
                    code.push(Instr::Branch { label, cond: Some(cond), loc });
                    node_id = root_b;
                    outgoing = cfg.outgoing_for(node_id)
                } else if can_branch_to(node_id, root_a) && !can_branch_to(node_id, root_b) {
                    code.push(Instr::Branch { label: Label(root_b), cond: Some(cond.not()), loc });
                    node_id = root_a;
                    outgoing = cfg.outgoing_for(node_id)
                } else {
                    let reachable_a = explore_reachable(cfg, bubble, root_a);
                    let reachable_b = explore_reachable(cfg, bubble, root_b);

                    let reachable_both = reachable_a.intersection(&reachable_b).copied().collect::<HashSet<_>>();

                    let true_then = reachable_a.difference(&reachable_both).copied().collect::<HashSet<_>>();
                    let false_then = reachable_b.difference(&reachable_both).copied().collect::<HashSet<_>>();

                    let true_then_code = generate_ifs_in(blocks, &true_then, cfg, root_a);
                    let false_then_code = generate_ifs_in(blocks, &false_then, cfg, root_b);

                    code.push(Instr::If {
                        loc, cond,
                        true_case: true_then_code,
                        false_case: false_then_code
                    });

                    if reachable_both.is_empty() {
                        break code
                    }

                    let mut common_root = None;
                    for node in &reachable_both {
                        let c = cfg.incoming_for(*node).iter().filter(|x| reachable_both.contains(*x) && !cfg.is_backward_edge(**x, *node)).count();
                        if c == 0 {
                            common_root = Some(*node);
                        }
                    }

                    code.extend(generate_ifs_in(blocks, &reachable_both, cfg, common_root.expect("Need more complex common branch root detection")));
                    break code
                }
            },
            _ => unreachable!()
        }
    }
}

pub fn generate_ifs(blocks: &mut Vec<CfgBlock>, cfg: &Cfg) -> Vec<Instr> {
    let all = (0..blocks.len()).collect::<HashSet<_>>();
    generate_ifs_in(blocks, &all, cfg, cfg.root())
}
