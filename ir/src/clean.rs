use std::collections::HashSet;

use crate::{Instr, Label, Expr, BinOp, MonOp, Binding, CfgBlock, Typ};

/// Return the set of labels which are branches to in the given code
pub fn used_labels(code: &[Instr]) -> HashSet<Label> {
    let mut set = HashSet::new();
    Instr::visit_all(code, &mut |i: &Instr| if let Instr::Branch { label, .. } = i {
        set.insert(*label);
    });
    set
}

/// Remove all labels in the given code which are never branched to
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

/// Remove jumps which don't do anything:
/// 1. Branches forward nowhere
/// 2. Branches out of the end of if statements (but only one layer)
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

/// Replace if x { X; return } else { Y } with if x { X; return } Y and equivalents
pub fn clean_else_to_fallthrough(code: &mut Vec<Instr>) {
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

/// Replace (==)(a - b) with a == b, etc
pub fn reduce_cmp(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| {
                if let Expr::MonOp(m, box Expr::BinOp(c, l, r)) = e {
                    match (*m, *c) {
                        (MonOp::CmpEq, BinOp::And) if *l == *r && !l.contains_call() => *e = Expr::BinOp(BinOp::Eq, Box::new(l.take()), Box::new(Expr::Lit(0, Typ::N64))),
                        (MonOp::CmpNe, BinOp::And) if *l == *r && !l.contains_call() => *e = Expr::BinOp(BinOp::Ne, Box::new(l.take()), Box::new(Expr::Lit(0, Typ::N64))),

                        (MonOp::CmpEq, BinOp::Xor) => *e = Expr::BinOp(BinOp::Eq, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpNe, BinOp::Xor) => *e = Expr::BinOp(BinOp::Ne, Box::new(l.take()), Box::new(r.take())),

                        (MonOp::CmpEq, BinOp::Sub) => *e = Expr::BinOp(BinOp::Eq, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpNe, BinOp::Sub) => *e = Expr::BinOp(BinOp::Ne, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpLt, BinOp::Sub) => *e = Expr::BinOp(BinOp::Lt, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpLe, BinOp::Sub) => *e = Expr::BinOp(BinOp::Le, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpGt, BinOp::Sub) => *e = Expr::BinOp(BinOp::Gt, Box::new(l.take()), Box::new(r.take())),
                        (MonOp::CmpGe, BinOp::Sub) => *e = Expr::BinOp(BinOp::Ge, Box::new(l.take()), Box::new(r.take())),
                        _ => {}
                    }
                }

                if let Expr::MonOp(m, l) = e {
                    match m {
                        MonOp::CmpEq => *e = Expr::BinOp(BinOp::Eq, Box::new(l.take()), Box::new(Expr::Lit(0, Typ::N64))),
                        MonOp::CmpNe => *e = Expr::BinOp(BinOp::Ne, Box::new(l.take()), Box::new(Expr::Lit(0, Typ::N64))),
                        _ => {}
                    }
                }
            });
        });
    }
}

/// Replace *&x and &*x with x
pub fn reduce_ref_deref(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| match e {
                Expr::Deref(box Expr::Ref(box x, _), _) => *e = x.take(),
                Expr::Ref(box Expr::Deref(box x, _), _) => *e = x.take(),
                _ => {}
            });
        });
    }
}

pub fn reduce_binop_assoc(expr: &mut Expr) {
    expr.visit_mut_post(&mut |e| if let Expr::BinOp(op2, box Expr::BinOp(op1, l, box Expr::Lit(x2, t)), box Expr::Lit(x1, _)) = e {
        // (l <op1> x1) <op2> x1
        match (op1, op2) {
            (BinOp::Sub, BinOp::Sub) => {
                *e = Expr::BinOp(BinOp::Sub, Box::new(l.take()), Box::new(Expr::Lit(*x1 + *x2, *t)));
            }
            (BinOp::Sub, BinOp::Add) if x2 <= x1 => {
                *e = Expr::BinOp(BinOp::Add, Box::new(l.take()), Box::new(Expr::Lit(*x1 - *x2, *t)));
            }
            (BinOp::Sub, BinOp::Add) if x2 > x1 => {
                *e = Expr::BinOp(BinOp::Sub, Box::new(l.take()), Box::new(Expr::Lit(*x2 - *x1, *t)));
            }
            (BinOp::Add, BinOp::Add) => {
                *e = Expr::BinOp(BinOp::Add, Box::new(l.take()), Box::new(Expr::Lit(*x1 + *x2, *t)));
            }
            _ => {}
        }
    });
}

/// Replace e.g. (x - a) - b with x - (a + b) etc
pub fn reduce_binop_assoc_all(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut reduce_binop_assoc);
    }
}

/// Replace a - a and a ^ a with 0
pub fn reduce_binop_to_identities(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| if let Expr::BinOp(op, l, r) = e && l == r && !l.contains_call() {
                match op {
                    BinOp::Sub => *e = Expr::Lit(0, Typ::N64),
                    BinOp::Xor => *e = Expr::Lit(0, Typ::N64),
                    _ => {}
                }
            });
        });
    }
}

/// Replace a + 0 with a, etc
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

/// Replace the constants a + b with the value of a + b
pub fn reduce_binop_constants(code: &mut Vec<Instr>) {
    for instr in code {
        instr.visit_top_exprs_mut(&mut |expr| {
            expr.visit_mut_post(&mut |e| if let Expr::BinOp(op, box Expr::Lit(x1, t), box Expr::Lit(x2, _)) = e {
                match op {
                    BinOp::Add => *e = Expr::Lit(*x1 + *x2, *t),
                    BinOp::Sub => *e = Expr::Lit(*x1 - *x2, *t),
                    BinOp::Mul => *e = Expr::Lit(*x1 * *x2, *t),
                    BinOp::Lsl => *e = Expr::Lit(((*x1 as u64) << *x2 as u64) as i64, *t),
                    BinOp::Lsr => *e = Expr::Lit((*x1 as u64 >> *x2 as u64) as i64, *t),
                    BinOp::Asr => *e = Expr::Lit(*x1 >> *x2, *t),
                    _ => {}
                }
            });
        });
    }
}

/// Remove a = a
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

/// Replace a + x with x + a for constants x
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

/// Replace if x {} else { A } with if !x { A }
pub fn clean_empty_true_then(code: &mut Vec<Instr>) {
    Instr::visit_mut_all(code, &mut |instr|
        if
        let Instr::If { true_case, false_case, cond, .. } =
            instr && true_case.is_empty()
        {
            true_case.extend(false_case.drain(..));
            *cond = cond.take().not();
        }
    );
}

/// Remove the return; B from A; return; B
pub fn clean_unreachable(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |instrs| {
        let mut i = 0;
        let mut clear = false;
        while i < instrs.len() {
            if clear {
                if let Instr::Label { .. } = &instrs[i] {
                    clear = false;
                    i += 1;
                    continue
                }

                instrs.remove(i);
                continue
            }

            if !instrs[i].fallsthrough() {
                clear = true;
            }
            i += 1;
        }
    });
}

/// Replace if x { A; jump L1 } if y { B } with if x { A; jump L1 } else if y { B }
pub fn generate_elseif(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |instrs| {
        let mut i = instrs.len();
        while i > 0 {
            i -= 1;
            let Instr::If { true_case, false_case, .. } = &instrs[i] else {
                continue
            };

            if !false_case.is_empty() {
                continue
            }

            let Some(Instr::Branch { .. }) = true_case.last() else {
                continue
            };

            let Some(Instr::If { .. }) = instrs.get(i + 1) else {
                continue
            };

            let instr = vec![instrs.remove(i + 1)];
            let Instr::If { false_case, .. } = &mut instrs[i] else {
                unreachable!()
            };
            *false_case = instr;
            i += 1;
        }
    });
}

fn clean_dead_fallthrough_jumps_in(code: &mut Vec<Instr>, end: &mut HashSet<Label>) {
    if code.is_empty() {
        return
    }

    let mut i = code.len();
    while i > 0 {
        if let Some(Instr::Label { label, .. }) = code.get(i - 1) {
            end.insert(*label);
            i -= 1;
            continue
        }

        if let Some(Instr::Branch { label, .. }) = code.get(i - 1) && end.contains(label) {
            code.remove(i - 1);
            i -= 1;
            continue
        }

        break
    }

    if i > 0 && let Some(Instr::If { true_case, false_case, .. }) = code.get_mut(i - 1) {
        clean_dead_fallthrough_jumps_in(true_case, end);
        clean_dead_fallthrough_jumps_in(false_case, end);
    }

    let mut i = 0;
    while i < code.len() {
        let Instr::If { .. } = &code[i] else {
            code[i].visit_depth_one_bodies_mut(&mut |body| clean_dead_fallthrough_jumps_in(body, &mut HashSet::new()));

            i += 1;
            continue
        };

        let mut new_end = HashSet::new();
        let mut j = i + 1;
        while let Some(Instr::Label { label, .. }) = code.get(j) {
            new_end.insert(*label);
            j += 1;
        }

        let Instr::If { true_case, false_case, .. } = &mut code[i] else {
            unreachable!()
        };

        clean_dead_fallthrough_jumps_in(true_case, &mut new_end);
        clean_dead_fallthrough_jumps_in(false_case, &mut new_end);
        i += 1;
    }
}

/// Remove the if from x { if y { jump L1 } }; L1:. This is a more advanced form of clean_dead_jumps
pub fn clean_dead_fallthrough_jumps(code: &mut Vec<Instr>) {
    clean_dead_fallthrough_jumps_in(code, &mut HashSet::new())
}

/// Replace return x; with return;
pub fn demote_to_return_void(blocks: &mut [CfgBlock]) {
    for block in blocks {
        Instr::visit_mut_all_blocks_post(&mut block.code, &mut |block| {
            let mut i = 0;
            while i < block.len() {
                let Instr::Return { value: Some(inner), typ, loc } = &mut block[i] else {
                    i += 1;
                    continue
                };

                if !inner.contains_call() {
                    block[i] = Instr::Return { value: None, loc: *loc, typ: *typ };
                    i += 1;
                    continue
                }

                let expr = inner.take();
                let loc = *loc;
                let typ = *typ;
                block.insert(i, Instr::Expr { expr, loc });
                block[i + 1] = Instr::Return { value: None, loc, typ };
                i += 2;
                continue
            }
        });
    }
}