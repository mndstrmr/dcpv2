use std::collections::HashSet;

use crate::{Instr, Label, Expr, BinOp, MonOp, Binding};

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
        instr.visit_top_exprs_mut(&mut |expr| {
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

pub fn reduce_binop_assoc(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| if let Expr::BinOp(op2, box Expr::BinOp(op1, l, box Expr::Lit(x2, t)), box Expr::Lit(x1, _)) = e {
                match (op1, op2) {
                    (BinOp::Sub, BinOp::Add) if x2 <= x1 => {
                        *e = Expr::BinOp(BinOp::Add, Box::new(l.take()), Box::new(Expr::Lit(*x1 - *x2, *t)));
                    }
                    (BinOp::Sub, BinOp::Add) if x2 > x1 => {
                        *e = Expr::BinOp(BinOp::Sub, Box::new(l.take()), Box::new(Expr::Lit(*x2 - *x1, *t)));
                    }
                    _ => {}
                }
            });
        });
    }
}

pub fn reduce_binop_identities(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| if let Expr::BinOp(op, l, box Expr::Lit(x1, _)) = e {
                match (op, x1) {
                    (BinOp::Add, 0) => *e = l.take(),
                    (BinOp::Sub, 0) => *e = l.take(),
                    (BinOp::Mul, 1) => *e = l.take(),
                    _ => {}
                }
            });
        });
    }
}

pub fn clean_self_writes(code: &mut Vec<Instr>) {
    let mut i = 0;
    while i < code.len() {
        if let Instr::Store { dest: Binding::Name(dest), src: Expr::Name(src), .. } = code[i] && dest == src {
            code.remove(i);
        } else {
            i += 1;
        }
    }
}

pub fn move_constants_right(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| match e {
                Expr::BinOp(BinOp::Add, box Expr::Lit(l, t), r) if *l >= 0 =>
                    *e = Expr::BinOp(BinOp::Add, Box::new(r.take()), Box::new(Expr::Lit(*l, *t))),
                Expr::BinOp(BinOp::Add, box Expr::Lit(l, t), r) if *l < 0 =>
                    *e = Expr::BinOp(BinOp::Sub, Box::new(r.take()), Box::new(Expr::Lit(-*l, *t))),
                _ => {}
            })
        })
    }
}
