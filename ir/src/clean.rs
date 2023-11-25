use std::collections::HashSet;

use crate::{Instr, Label, Expr, BinOp, MonOp};

pub fn used_labels(code: &[Instr]) -> HashSet<Label> {
    let mut set = HashSet::new();
    Instr::visit_all(code, &mut |i: &Instr| if let Instr::Branch { label, .. } = i {
        set.insert(*label);
    });
    set
}

pub fn clean_dead_labels(code: &mut Vec<Instr>) {
    let used = used_labels(code);
    Instr::filter_mut_all(code, &mut |i| match i {
        Instr::Label { label, .. } if !used.contains(label) => true,
        _ => false
    })
}

fn equivalent_labels_at(code: &[Instr]) -> HashSet<Label> {
    let mut labels = HashSet::new();

    for instr in code {
        match instr {
            Instr::Label { label, .. } => {
                labels.insert(*label);
            }
            Instr::Loop { body, .. } => labels.extend(equivalent_labels_at(body)),
            _ => break
        }
    }

    labels
}

pub fn clean_dead_jumps(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        let mut i = 0;
        while i < block.len() {
            match &block[i] {
                Instr::If { .. } => {
                    let labels = equivalent_labels_at(&block[i + 1..]);
                    let Instr::If { true_case, false_case, .. } = &mut block[i] else {
                        unreachable!()
                    };
        
                    while let Some(Instr::Branch { label, .. }) = true_case.last() && labels.contains(label) {
                        true_case.pop();
                    }
                    while let Some(Instr::Branch { label, .. }) = false_case.last() && labels.contains(label) {
                        false_case.pop();
                    }
        
                    i += 1;
                }
                Instr::Branch { .. } => {
                    let labels = equivalent_labels_at(&block[i + 1..]);
                    let Instr::Branch { label, .. } = &mut block[i] else {
                        unreachable!()
                    };
                    if labels.contains(label) {
                        block.remove(i);
                    } else {
                        i += 1;
                    }
                }
                _ => i += 1
            }
        }
    });
}

pub fn clean_unreachable_elses(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        let mut i = 0;
        while i < block.len() {
            let Instr::If { true_case, false_case, .. } = &mut block[i] else {
                i += 1;
                continue
            };

            if let Some(last) = true_case.last() && !last.fallsthrough() {
                let code = false_case.drain(..).collect::<Vec<_>>();
                block.splice(i + 1..i + 1, code);
            }

            i += 1;
        }
    });
}

pub fn reduce_cmp(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_non_nested_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| if let Expr::MonOp(m, box Expr::BinOp(BinOp::Cmp, l, r)) = e {
                match m {
                    MonOp::CmpEq => *e = Expr::BinOp(BinOp::Eq, Box::new(l.take()), Box::new(r.take())),
                    MonOp::CmpNe => *e = Expr::BinOp(BinOp::Ne, Box::new(l.take()), Box::new(r.take())),
                    MonOp::CmpLt => *e = Expr::BinOp(BinOp::Lt, Box::new(l.take()), Box::new(r.take())),
                    MonOp::CmpLe => *e = Expr::BinOp(BinOp::Le, Box::new(l.take()), Box::new(r.take())),
                    MonOp::CmpGt => *e = Expr::BinOp(BinOp::Gt, Box::new(l.take()), Box::new(r.take())),
                    MonOp::CmpGe => *e = Expr::BinOp(BinOp::Ge, Box::new(l.take()), Box::new(r.take())),
                    _ => {}
                }
            });
        });
    }
}
