use std::collections::HashMap;

use crate::{CfgBlock, Func, Expr, Instr};

pub fn insert_args(blocks: &mut [CfgBlock], funcs: &HashMap<u64, &Func>) {
    for block in blocks {
        Instr::visit_mut_all(&mut block.code, &mut |instr| {
            instr.visit_top_exprs_mut(&mut |e| {
                e.visit_mut_post(&mut |expr| if let Expr::Call(box Expr::Lit(addr, _), args) = expr && args.is_empty() {
                    if let Some(func) = funcs.get(&(*addr as u64)) {
                        args.extend(func.args.iter().map(|x| Expr::Name(x.name)));
                    }
                });
            });
        });
    }
}

pub fn replace_names(blocks: &mut [CfgBlock], funcs: &HashMap<u64, &Func>) {
    for block in blocks {
        Instr::visit_mut_all(&mut block.code, &mut |instr| {
            instr.visit_top_exprs_mut(&mut |e| {
                e.visit_mut_post(&mut |expr| if let Expr::Lit(addr, _) = expr {
                    if let Some(func) = funcs.get(&(*addr as u64)) {
                        *expr = Expr::Name(func.short_name);
                    }
                });
            });
        });
    }
}
