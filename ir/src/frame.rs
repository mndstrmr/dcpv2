use crate::{Instr, Frame, Expr, BinOp, Binding, Abi};

pub fn gen_frame(code: &[Instr], abi: &Abi, names_start: usize) -> Frame {
    let mut frame = Frame::new(names_start);

    let mut add_el = |op, r: &Expr, typ| match (op, r) {
        (BinOp::Sub, Expr::ILit(off, _)) => {
            frame.add(-off, typ);
        }
        (BinOp::Add, Expr::ILit(off, _)) => {
            frame.add(*off, typ);
        }
        _ => frame.set_not_fully_understood()
    };

    for instr in code {
        instr.visit_top_exprs(&mut |e| {
            e.visit(&mut |e| if let Expr::Deref(box Expr::BinOp(op, box Expr::Name(nm), r), typ) = e && *nm == abi.fp {
                add_el(*op, r, *typ)
            })
        });

        if let Instr::Store { dest: Binding::Deref(Expr::BinOp(op, box Expr::Name(nm), r), typ), .. } = instr && *nm == abi.fp {
            add_el(*op, r, *typ)
        }
    }

    frame
}

pub fn apply_frame_names(abi: &Abi, code: &mut [Instr], frame: &Frame) {
    if !frame.is_fully_understood() {
        return
    }

    let get_name = |op, r: &Expr| match (op, r) {
        (BinOp::Sub, Expr::ILit(off, _)) => frame.name_for(-*off),
        (BinOp::Add, Expr::ILit(off, _)) => frame.name_for(*off),
        _ => panic!("Unrecoginised frame access")
    };

    for instr in code {
        instr.visit_top_exprs_mut(&mut |e| {
            e.visit_mut_post(&mut |e| if let Expr::Deref(box Expr::BinOp(op, box Expr::Name(nm), r), _) = e && *nm == abi.fp {
                *e = Expr::Name(get_name(*op, r));
            })
        });

        if let Instr::Store { dest: Binding::Deref(Expr::BinOp(op, box Expr::Name(nm), r), _), loc, src, typ } = instr && *nm == abi.fp {
            *instr = Instr::Store {
                dest: Binding::Name(get_name(*op, r)),
                loc: *loc, typ: *typ,
                src: src.take()
            };
        }
    }
}
