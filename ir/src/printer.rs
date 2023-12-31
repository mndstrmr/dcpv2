use std::collections::HashMap;

use crate::{Instr, Name, Expr, Binding, BinOp, Typ};

pub struct CodeFormatConfig {
    pub name_map: HashMap<Name, String>,
    pub mem_types: bool,
    pub lit_types: bool,
    pub write_types: bool,
    pub return_types: bool,
    pub indent: String,
    pub loc: bool,
    pub loc_gap: usize,
    pub indent_lab: bool
}

impl Default for CodeFormatConfig {
    fn default() -> Self {
        CodeFormatConfig {
            indent: "    ".to_string(),
            indent_lab: false,
            lit_types: false, mem_types: false, return_types: false, write_types: false,
            loc: true,
            loc_gap: 4,
            name_map: HashMap::new()
        }
    }
}

fn write_expr_at_prec<W: std::fmt::Write>(f: &mut W, expr: &Expr, config: &CodeFormatConfig, prec: usize) -> std::fmt::Result {
    match expr {
        Expr::Name(name) => {
            if let Some(name) = config.name_map.get(name) {
                write!(f, "{name}")?;
            } else {
                write!(f, "{name}")?;
            }
        }
        Expr::StringLit(string) => {
            write!(f, "\"")?;
            for chr in string.chars() {
                match chr {
                    '\\' => write!(f, "\\\\")?,
                    '\"' => write!(f, "\\\"")?,
                    '\n' => write!(f, "\\n")?,
                    _ => write!(f, "{chr}")?
                }
            }
            write!(f, "\"")?;
        }
        Expr::Deref(expr, typ) => {
            if prec > 10 {
                write!(f, "(")?;
            }
            write!(f, "*")?;
            if config.mem_types {
                write!(f, "({typ})")?;
            }
            write_expr_at_prec(f, expr, config, 10)?;
            if prec > 10 {
                write!(f, ")")?;
            }
        }
        Expr::Ref(expr, typ) => {
            if prec > 10 {
                write!(f, "(")?;
            }
            write!(f, "&")?;
            if config.mem_types {
                write!(f, "({typ})")?;
            }
            write_expr_at_prec(f, expr, config, 10)?;
            if prec > 10 {
                write!(f, ")")?;
            }
        }
        Expr::Call(func, args) => {
            if prec > 11 {
                write!(f, "(")?;
            }
            write_expr_at_prec(f, func, config, 11)?;
            write!(f, "(")?;
            for (a, arg) in args.iter().enumerate() {
                if a != 0 {
                    write!(f, ", ")?;
                }
                write_expr_at_prec(f, arg, config, 0)?;
            }
            write!(f, ")")?;

            if prec > 11 {
                write!(f, ")")?;
            }
        }
        Expr::Lit(n, typ) => {
            if config.lit_types {
                write!(f, "({typ}).")?;
            }
            write!(f, "0x{n:x}")?;
        }
        Expr::BinOp(op, l, r) => {
            let new_prec = match op {
                BinOp::Add => 5,
                BinOp::Sub => 5,
                BinOp::Mul => 6,
                BinOp::Div => 7,
                BinOp::Mod => 7,
                BinOp::And => 2,
                BinOp::Xor => 3,
                BinOp::Or => 1,
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Ge | BinOp::Gt => 4,
                BinOp::Asr | BinOp::Lsl | BinOp::Lsr => 8,
            };

            if prec > new_prec {
                write!(f, "(")?;
            }
            write_expr_at_prec(f, l, config, new_prec)?;
            write!(f, " {op} ")?;
            write_expr_at_prec(f, r, config, new_prec)?;
            if prec > new_prec {
                write!(f, ")")?;
            }
        }
        Expr::MonOp(op, expr) => {
            if prec > 10 {
                write!(f, "(")?;
            }
            write!(f, "{op}")?;
            write_expr_at_prec(f, expr, config, 10)?;
            if prec > 10 {
                write!(f, ")")?;
            }
        }
    }

    Ok(())
}

fn write_expr<W: std::fmt::Write>(f: &mut W, expr: &Expr, config: &CodeFormatConfig) -> std::fmt::Result {
    write_expr_at_prec(f, expr, config, 0)
}

fn write_store<W: std::fmt::Write>(f: &mut W, typ: Typ, dest: &Binding, src: &Expr, config: &CodeFormatConfig) -> std::fmt::Result {
    match (dest, src) {
        (Binding::Name(nm1), Expr::BinOp(op, box Expr::Name(nm2), r)) if nm1 == nm2 => {
            if let Some(name) = config.name_map.get(nm1) {
                write!(f, "{name}")?;
            } else {
                write!(f, "r{}", nm1.0)?;
            }

            write!(f, " {op}= ")?;
            write_expr(f, r, config)?;
            return Ok(())
        }
        _ => {}
    }

    match dest {
        Binding::Name(name) => {
            if config.write_types {
                write!(f, "({typ}).")?;
            }

            if let Some(name) = config.name_map.get(name) {
                write!(f, "{name}")?;
            } else {
                write!(f, "{name}")?;
            }
        }
        Binding::Deref(expr, _) => {
            write!(f, "*")?;
            if config.write_types || config.mem_types {
                write!(f, "({typ})")?;
            }

            write_expr_at_prec(f, expr, config, 10)?;
        }
    }

    write!(f, " = ")?;
    write_expr(f, src, config)?;

    Ok(())
}

fn write_code_at_depth<W: std::fmt::Write>(f: &mut W, code: &[Instr], config: &CodeFormatConfig, depth: usize) -> std::fmt::Result {
    let indent = |f: &mut W| -> std::fmt::Result {
        for _ in 0..depth {
            write!(f, "{}", config.indent)?;
        };
        Ok(())
    };

    for instr in code {
        let write_loc = |f: &mut W| -> std::fmt::Result {
            if config.loc {
                write!(f, "0x{:x}", instr.loc().addr)?;

                for _ in 0..config.loc_gap {
                    write!(f, " ")?;
                }
            }

            Ok(())
        };

        match instr {
            Instr::Break(_) => {
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "break;")?
            }
            Instr::Continue(_) => {
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "continue;")?;
            }
            Instr::Label { label, .. } => {
                if config.indent_lab {
                    indent(f)?;
                }
                writeln!(f, "L{}:", label.0)?;
            }
            Instr::Store { typ, dest, src, .. } => {
                write_loc(f)?;
                indent(f)?;
                write_store(f, *typ, dest, src, config)?;
                writeln!(f, ";")?;
            }
            Instr::Return { value: Some(value), typ, .. } => {
                write_loc(f)?;
                indent(f)?;
                if config.return_types {
                    write!(f, "return.({typ}) ")?;
                } else {
                    write!(f, "return ")?;
                }
                write_expr(f, value, config)?;
                writeln!(f, ";")?;
            }
            Instr::Return { value: None, .. } => {
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "return;")?;
            }
            Instr::Branch { label, cond: None, .. } => {
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "goto L{};", label.0)?;
            }
            Instr::Branch { label, cond: Some(cond), .. } => {
                write_loc(f)?;
                indent(f)?;
                write!(f, "if ")?;
                write_expr(f, cond, config)?;
                writeln!(f, " goto L{};", label.0)?;
            }
            Instr::Expr { expr, .. } => {
                write_loc(f)?;
                indent(f)?;
                write_expr(f, expr, config)?;
                writeln!(f, ";")?;
            }
            Instr::If { cond, true_case, false_case, .. } => {
                write_loc(f)?;
                indent(f)?;

                let mut cond = cond;
                let mut true_case = true_case;
                let mut false_case = false_case;

                loop {
                    write!(f, "if ")?;
                    write_expr(f, cond, config)?;
                    writeln!(f, " {{")?;
                    write_code_at_depth(f, &true_case, config, depth + 1)?;
                    if !false_case.is_empty() {
                        write_loc(f)?;
                        indent(f)?;

                        if let Some(Instr::If { true_case: inner_true, false_case: inner_false, cond: inner_cond, .. }) = false_case.first() && false_case.len() == 1 {
                            write!(f, "}} else ")?;
                            true_case = inner_true;
                            false_case = inner_false;
                            cond = inner_cond;
                            continue
                        }

                        writeln!(f, "}} else {{")?;
                        write_code_at_depth(f, &false_case, config, depth + 1)?;
                    }
                    write_loc(f)?;
                    indent(f)?;
                    writeln!(f, "}}")?;
                    break;
                }
            }
            Instr::Loop { body, .. } => {
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "loop {{")?;
                write_code_at_depth(f, &body, config, depth + 1)?;
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "}}")?;
            }
            Instr::While { cond, body, .. } => {
                write_loc(f)?;
                indent(f)?;
                write!(f, "while ")?;
                write_expr(f, cond, config)?;
                writeln!(f, " {{")?;
                write_code_at_depth(f, &body, config, depth + 1)?;
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "}}")?;
            }
            Instr::For { init, cond, step, body, .. } => {
                write_loc(f)?;
                indent(f)?;
                write!(f, "for ")?;
                if !init.is_empty() {
                    assert_eq!(init.len(), 1);
                    let Instr::Store { typ, dest, src, .. } = &init[0] else {
                        panic!("Malformed for init");
                    };
                    write_store(f, *typ, dest, src, config)?;
                    write!(f, "; ")?;
                }
                write_expr(f, cond, config)?;
                write!(f, "; ")?;
                assert_eq!(step.len(), 1);
                let Instr::Store { typ, dest, src, .. } = &step[0] else {
                    panic!("Malformed for step");
                };
                write_store(f, *typ, dest, src, config)?;
                
                writeln!(f, " {{")?;
                write_code_at_depth(f, &body, config, depth + 1)?;
                write_loc(f)?;
                indent(f)?;
                writeln!(f, "}}")?;
            }
        }
    }

    Ok(())
}

pub fn write_code<W: std::fmt::Write>(f: &mut W, code: &[Instr], config: &CodeFormatConfig) -> std::fmt::Result {
    write_code_at_depth(f, code, config, 0)
}
