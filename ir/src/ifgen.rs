use std::collections::HashSet;

use crate::{BinOp, Cfg, CfgBlock, Expr, Instr, Label, Loc};

fn explore_reachable(cfg: &Cfg, bubble: &HashSet<usize>, root: usize) -> HashSet<usize> {
    let mut reachable = HashSet::new();

    if !bubble.contains(&root) {
        return reachable
    }
    reachable.insert(root);
    let mut to_visit = vec![root];

    while let Some(node) = to_visit.pop() {
        for child in cfg.outgoing_for(node) {
            if !cfg.is_backward_edge(node, *child) && bubble.contains(child) && reachable.insert(*child) {
                to_visit.push(*child);
            }
        }
    }

    reachable
}

fn generate_ifs_in(blocks: &mut Vec<CfgBlock>, bubble: &HashSet<usize>, fallthrough: Option<usize>, cfg: &Cfg, root: usize) -> Vec<Instr> {
    assert!(!blocks.is_empty());
    let mut node_id = root;
    let mut code = Vec::<Instr>::new();

    if bubble.is_empty() {
        code.push(Instr::Branch {
            loc: Loc { addr: 0 },
            label: Label(node_id),
            cond: None
        });
        return code
    }

    let mut outgoing = cfg.outgoing_for(node_id);

    let can_branch_to = |src, dst| !cfg.is_backward_edge(src, dst) && bubble.contains(&dst);

    loop {
        assert!(bubble.contains(&node_id));

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

                let Instr::Branch { label, cond: Some(mut cond), loc } = code.pop().unwrap() else {
                    panic!("bad terminator")
                };

                if root_a != label.0 {
                    (root_a, root_b) = (root_b, root_a)
                }
                assert_eq!(root_a, label.0);

                let mut reachable_a = explore_reachable(cfg, bubble, root_a);
                let mut reachable_b = explore_reachable(cfg, bubble, root_b);

                let mut reachable_both = reachable_a.intersection(&reachable_b).copied().collect::<HashSet<_>>();

                let mut true_then = reachable_a.difference(&reachable_both).copied().collect::<HashSet<_>>();
                let mut false_then = reachable_b.difference(&reachable_both).copied().collect::<HashSet<_>>();

                if Some(root_a) == fallthrough {
                    let false_then_code = generate_ifs_in(blocks, &false_then, fallthrough, cfg, root_b);

                    code.push(Instr::If {
                        loc,
                        cond: cond.not(),
                        true_case: false_then_code,
                        false_case: Vec::new()
                    });
                    break code
                }

                if Some(root_b) == fallthrough {
                    let true_then_code = generate_ifs_in(blocks, &true_then, fallthrough, cfg, root_a);

                    code.push(Instr::If {
                        loc, cond,
                        true_case: true_then_code,
                        false_case: Vec::new()
                    });
                    break code
                }

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
                    loop {
                        if cfg.outgoing_for(root_b).len() == 2 && cfg.outgoing_for(root_b).contains(&root_a) && blocks[root_b].code.len() == 1 {
                            cond = cond.not();
                            (root_a, root_b) = (root_b, root_a);
                            std::mem::swap(&mut false_then, &mut true_then);

                            continue;
                        }

                        if cfg.outgoing_for(root_a).len() == 2 && cfg.outgoing_for(root_a).contains(&root_b) && blocks[root_a].code.len() == 1 {
                            let v: Vec<_> = cfg.outgoing_for(root_a).iter().copied().collect();
                            let other = if v[0] == root_b { v[1] } else { v[0] };

                            let Instr::Branch { label, cond: Some(mut clause), .. } = blocks[root_a].code.pop().unwrap() else {
                                unreachable!()
                            };
                            if label.0 != other {
                                clause = clause.not();
                            }

                            cond = Expr::BinOp(BinOp::And, Box::new(cond), Box::new(clause));
                            root_a = other;

                            // Kinda annoying to have to do this
                            reachable_a = explore_reachable(cfg, bubble, root_a);
                            reachable_b = explore_reachable(cfg, bubble, root_b);
                            reachable_both = reachable_a.intersection(&reachable_b).copied().collect::<HashSet<_>>();
                            true_then = reachable_a.difference(&reachable_both).copied().collect::<HashSet<_>>();
                            false_then = reachable_b.difference(&reachable_both).copied().collect::<HashSet<_>>();

                            continue;
                        }

                        break
                    }

                    let mut common_root = None;
                    for node in &reachable_both {
                        let c = cfg.incoming_for(*node).iter().filter(|x| reachable_both.contains(*x) && !cfg.is_backward_edge(**x, *node)).count();
                        if c == 0 {
                            common_root = Some(*node);
                        }
                    }

                    let true_then_code = generate_ifs_in(blocks, &true_then, common_root.or(fallthrough), cfg, root_a);
                    let false_then_code = generate_ifs_in(blocks, &false_then, common_root.or(fallthrough), cfg, root_b);

                    code.push(Instr::If {
                        loc, cond,
                        true_case: true_then_code,
                        false_case: false_then_code
                    });

                    if !reachable_both.is_empty() {
                        code.extend(generate_ifs_in(blocks, &reachable_both, fallthrough, cfg, common_root.expect("Need more complex common branch root detection")));
                    }
                    break code
                }
            },
            _ => unreachable!()
        }
    }
}

pub fn generate_ifs(blocks: &mut Vec<CfgBlock>, cfg: &Cfg) -> Vec<Instr> {
    let all = (0..blocks.len()).collect::<HashSet<_>>();
    let instrs = generate_ifs_in(blocks, &all, None, cfg, cfg.root());

    for (b, block) in blocks.iter().enumerate() {
        if !block.code.is_empty() {
            eprintln!("Block {b} not empty @ {:x}:", block.code[0].loc().addr);
            Instr::dump_block(&block.code);
            println!();
        }
    }

    instrs
}
