use crate::Instr;

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
