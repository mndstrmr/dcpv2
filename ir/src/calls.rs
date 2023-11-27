use std::collections::{HashMap, HashSet};

use crate::{CfgBlock, Expr, Instr, Name, FuncArg, Abi, format_string_arg_count};

pub fn insert_args(blocks: &mut [CfgBlock], funcs: &HashMap<Name, &[FuncArg]>) {
    for block in blocks {
        Instr::visit_mut_all(&mut block.code, &mut |instr| {
            instr.visit_top_exprs_mut(&mut |e| {
                e.visit_mut_post(&mut |expr| if let Expr::Call(box Expr::Name(name), args) = expr && args.is_empty() {
                    if let Some(new_args) = funcs.get(name) {
                        args.extend(new_args.iter().map(|x| Expr::Name(x.name)));
                    }
                });
            });
        });
    }
}

pub fn replace_names(block: &mut Vec<Instr>, funcs: &HashMap<u64, Name>) {
    Instr::visit_mut_all(block, &mut |instr| {
        instr.visit_top_exprs_mut(&mut |e| {
            e.visit_mut_post(&mut |expr| if let Expr::Lit(addr, _) = expr {
                if let Some(func) = funcs.get(&(*addr as u64)) {
                    *expr = Expr::Name(*func);
                }
            });
        });
    });
}

pub fn update_format_string_args(abi: &Abi, blocks: &mut [CfgBlock], names: &HashSet<Name>) {
    for block in blocks {
        Instr::visit_mut_all(&mut block.code, &mut |instr| {
            instr.visit_top_exprs_mut(&mut |e| {
                e.visit_mut_post(&mut |e| if let Expr::Call(box Expr::Name(name), args) = e {
                    if !names.contains(name) {
                        return
                    }

                    let Some(Expr::StringLit(lit)) = &args.last() else {
                        return
                    };

                    args.extend(
                        abi.func_args[args.len()..format_string_arg_count(lit) + args.len()]
                            .iter().map(|x| Expr::Name(*x))
                    )
                });
            })
        })
    }
}