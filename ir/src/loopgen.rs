use std::collections::{HashMap, HashSet};

use crate::{Instr, Label, Name, Expr};

fn jumps_to_any(instr: &Instr, dsts: &HashMap<Label, usize>) -> Option<usize> {
    let mut found = None;
    instr.visit(&mut |instr| if let Instr::Branch { label, .. } = instr && let Some(i) = dsts.get(&label) {
        found = Some(*i);
    });
    found
}

fn extend_labels(instr: &Instr, labels: &mut HashMap<Label, usize>, value: usize) {
    instr.visit(&mut |i| if let Instr::Label { label, .. } = i {
        labels.insert(*label, value);
    });
}

pub fn loop_gen(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        'outer: loop {
            let mut own_labels = HashMap::new();
            for i in 0..block.len() {
                if let Some(loop_start) = jumps_to_any(&block[i], &own_labels) {
                    let loc = block[loop_start].loc();
                    let mut body = block.drain(loop_start..=i).collect::<Vec<_>>();
                    body.push(Instr::Break(body.last().unwrap().loc()));
                    block.insert(loop_start, Instr::Loop { loc, body });
                    continue 'outer
                }
                extend_labels(&block[i], &mut own_labels, i);
            }

            break
        }
    });
}

pub fn loop_jump_to_continue(code: &mut Vec<Instr>) {
    let mut loops = Vec::new();
    Instr::filter_mut_all_pre_post(code, &mut |instr, v| match instr {
        Instr::Loop { body, .. } if v.is_open() => {
            let mut labels = HashSet::new();
            let mut i = 0;
            while let Some(Instr::Label { label, .. }) = body.get(i) {
                labels.insert(*label);
                i += 1;
            }

            loops.push(labels);

            false
        }
        Instr::Loop { .. } if v.is_close() => {
            loops.pop();
            false
        }
        Instr::Branch { label, loc, cond: None } if v.is_open() && let Some(top_loop) = loops.last() && top_loop.contains(&label) => {
            *instr = Instr::Continue(*loc);
            false
        }
        Instr::Branch { label, loc, cond: Some(cond) } if v.is_open() && let Some(top_loop) = loops.last() && top_loop.contains(&label) => {
            let cond = std::mem::replace(cond, Expr::Name(Name(0)));
            *instr = Instr::If {
                cond, loc: *loc,
                true_case: vec![Instr::Continue(*loc)],
                false_case: vec![]
            };
            false
        }
        _ => false
    });
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
    Instr::filter_mut_all(code, &mut |instr| match instr {
        Instr::Loop { body, .. } => {
            while let Some(Instr::Continue(_)) = body.last() {
                body.pop();
            }
            false
        }
        _ => false
    });
}
