use std::collections::HashSet;

use crate::{Instr, Label};


pub fn loop_gen(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        'outer: loop {
            for i in 0..block.len() {
                let mut labels = Vec::new();
                block[i].visit(&mut |i| if let Instr::Label { label, .. } = i {
                    labels.push(*label);
                });

                for label in labels {
                    for j in (i + 1..block.len()).rev() {
                        let mut found = false;
                        block[j].visit(&mut |instr| if let Instr::Branch { label: lab, .. } = instr && *lab == label {
                            found = true;
                        });
                        
                        if found {
                            let loc = block[i].loc();
                            let mut body = block.drain(i..=j).collect::<Vec<_>>();
                            body.push(Instr::Break(body.last().unwrap().loc()));
                            block.insert(i, Instr::Loop { loc, body });
                            continue 'outer
                        }
                    }
                }
            }

            break
        }
    });
}

fn loop_jump_to_continue_in(code: &mut Vec<Instr>, start_labels: &HashSet<Label>) {
    for i in 0..code.len() {
        let instr = &mut code[i];
        match instr {
            Instr::Branch { label, cond: None, loc } if start_labels.contains(label) => {
                *instr = Instr::Continue(*loc);
            }
            Instr::Branch { label, cond: Some(cond), loc } if start_labels.contains(label) => {
                *instr = Instr::If {
                    loc: *loc,
                    cond: cond.take(),
                    true_case: vec![Instr::Continue(*loc)],
                    false_case: vec![]
                };
            }
            Instr::Loop { body, .. } | Instr::While { body, .. } => {
                let mut labels = HashSet::new();
                let mut j = 0;
                while let Some(Instr::Label { label, .. }) = body.get(j) {
                    labels.insert(*label);
                    j += 1;
                }

                loop_jump_to_continue_in(body, &labels)
            }
            _ => instr.visit_depth_one_bodies_mut(&mut |b| loop_jump_to_continue_in(b, start_labels))
        }
    }
}

pub fn loop_jump_to_continue(code: &mut Vec<Instr>) {
    loop_jump_to_continue_in(code, &HashSet::new())
}

fn loop_jump_to_break_in(code: &mut Vec<Instr>, end_labels: &HashSet<Label>) {
    for i in 0..code.len() {
        let instr = &mut code[i];
        match instr {
            Instr::Branch { label, cond: None, loc } if end_labels.contains(label) => {
                *instr = Instr::Break(*loc);
            }
            Instr::Branch { label, cond: Some(cond), loc } if end_labels.contains(label) => {
                *instr = Instr::If {
                    loc: *loc,
                    cond: cond.take(),
                    true_case: vec![Instr::Break(*loc)],
                    false_case: vec![]
                };
            }
            Instr::Loop { .. } | Instr::While { .. } => {
                let mut labels = HashSet::new();
                let mut j = 0;
                while let Some(Instr::Label { label, .. }) = code.get(i + 1 + j) {
                    labels.insert(*label);
                    j += 1;
                }

                if let Instr::Loop { body, .. } | Instr::While { body, .. } = &mut code[i] {
                    loop_jump_to_break_in(body, &labels)
                }
            }
            _ => instr.visit_depth_one_bodies_mut(&mut |b| loop_jump_to_break_in(b, end_labels))
        }
    }
}

pub fn loop_jump_to_break(code: &mut Vec<Instr>) {
    loop_jump_to_break_in(code, &HashSet::new())
}

pub fn if_break_negate(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        let mut i = 0;
        while i < block.len() {
            if let Instr::If { false_case, .. } = &block[i] && false_case.is_empty() && let Some(Instr::Break(_)) = block.get(i + 1) {
                let Instr::Break(bloc) = block.remove(i + 1) else { unreachable!() };
                let Instr::If { cond, loc, true_case, .. } = block.remove(i) else { unreachable!() };
                block.insert(i, Instr::If { loc, cond: cond.not(), true_case: vec![Instr::Break(bloc)], false_case: vec![] });
                block.splice(i + 1..i + 1, true_case);
            }

            i += 1;
        }
    });
}

pub fn final_continue(code: &mut Vec<Instr>) {
    Instr::visit_mut_all(code, &mut |instr| if let Instr::Loop { body, .. } = instr {
        while let Some(Instr::Continue(_)) = body.last() {
            body.pop();
        }
    });
}
