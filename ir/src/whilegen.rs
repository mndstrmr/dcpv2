use crate::{Instr, Binding, Expr, BinOp};

pub fn while_gen(code: &mut Vec<Instr>) {
    Instr::visit_mut_all(code, &mut |instr|
        if
            let Instr::Loop { body, .. } = instr &&
            let Some(Instr::If { true_case, false_case, .. }) = body.first() &&
            true_case.len() == 1 && false_case.len() == 0 &&
            let Some(Instr::Break(_)) = true_case.first() {
        
            let Instr::Loop { loc, body } = instr else { unreachable!() };
            let Instr::If { cond, .. } = body.remove(0) else { unreachable!() };

            *instr = Instr::While { loc: *loc, cond: cond.not(), body: body.drain(..).collect() };
        }
    );
}

fn update_for_cond(cond: Expr) -> Expr {
    match cond {
        Expr::BinOp(BinOp::Le, l, box Expr::Lit(n, t)) =>
            Expr::BinOp(BinOp::Lt, l, Box::new(Expr::Lit(n + 1, t))),
        _ => cond
    }
}

pub fn for_gen(code: &mut Vec<Instr>) {
    Instr::visit_mut_all(code, &mut |instr|
        if
            let Instr::While { body, cond, loc } = instr &&
            let Some(Instr::Store { dest: Binding::Name(name), .. }) = body.last() &&
            cond.vars().reads_before_writes(*name) {
            
            *instr = Instr::For {
                init: vec![],
                body: body.drain(..body.len() - 1).collect(),
                cond: update_for_cond(cond.take()),
                loc: *loc,
                step: vec![body.pop().unwrap()]
            }
        }
    );
}

pub fn for_init_search(code: &mut Vec<Instr>) {
    Instr::visit_mut_all_blocks_post(code, &mut |block| {
        let mut i = 1;
        while i < block.len() {
            let Instr::For { step, init, .. } = &block[i] else {
                i += 1;
                continue
            };

            if init.len() != 0 {
                i += 1;
                continue
            }

            assert_eq!(step.len(), 1);
            let Instr::Store { dest: Binding::Name(name), .. } = &step[0] else {
                i += 1;
                continue
            };

            let Instr::Store { dest: Binding::Name(nm), .. } = &block[i - 1] else {
                i += 1;
                continue
            };

            if nm == name {
                let new_init = block.remove(i - 1);
                let Instr::For { init, .. } = &mut block[i - 1] else {
                    unreachable!()
                };
                init.push(new_init);
            }
        }
    })
}
