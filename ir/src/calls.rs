use std::collections::{HashMap, HashSet};

use crate::{CfgBlock, Expr, Instr, Name, Abi, format_string_arg_count, FuncArg, Cfg};
use crate::dataflow::did_controlled_write;

pub fn insert_args(abi: &Abi, cfg: &Cfg, blocks: &mut [CfgBlock], own_args: &[FuncArg], funcs: &HashMap<Name, (usize, bool)>) {
    let mut b = 0;
    while b < blocks.len() {
        let mut c = 0;
        while c < blocks[b].code.len() {
            let Instr::Store { src: Expr::Call(box f, args), .. } = &mut blocks[b].code[c] else {
                c += 1;
                continue
            };

            if !args.is_empty() {
                // Already handled somewhere perhaps
                c += 1;
                continue
            }

            if let Expr::Name(name) = f && let Some((arg_count, _)) = funcs.get(name) {
                args.extend(abi.func_args[0..*arg_count].iter().map(|x| Expr::Name(*x)));
                c += 1;
                continue
            }

            let mut i = 0;
            while i < abi.func_args.len() && did_controlled_write(abi, cfg, blocks, own_args, abi.func_args[i], b, c, &mut HashSet::new()) {
                i += 1
            }

            let Instr::Store { src: Expr::Call(_, args), .. } = &mut blocks[b].code[c] else {
                unreachable!()
            };

            args.extend(abi.func_args[0..i].iter().map(|x| Expr::Name(*x)));
            c += 1;
        }

        b += 1;
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

pub fn update_format_string_args(abi: &Abi, blocks: &mut [CfgBlock], names: &HashMap<Name, (usize, bool)>) {
    for block in blocks {
        Instr::visit_mut_all(&mut block.code, &mut |instr| {
            instr.visit_top_exprs_mut(&mut |e| {
                e.visit_mut_post(&mut |e| if let Expr::Call(box Expr::Name(name), args) = e {
                    let Some((n, true)) = names.get(name) else {
                        return
                    };

                    let Some(Expr::StringLit(lit)) = &args.last() else {
                        return
                    };

                    args.extend(
                        abi.func_args[*n..format_string_arg_count(lit) + *n]
                            .iter().map(|x| Expr::Name(*x))
                    )
                });
            })
        })
    }
}