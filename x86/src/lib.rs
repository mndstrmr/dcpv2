use bin::BinFunc;
use capstone::{Capstone, arch::{self, BuildsCapstoneSyntax, BuildsCapstone, x86::X86OpMem}, InsnDetail, RegId};
use ir::{Func, Name, Loc, Typ, Binding, Expr, Instr, BinOp, MonOp, Label};

#[derive(Debug)]
pub enum X86Error {
    Disasm(String)
}

impl From<capstone::Error> for X86Error {
    fn from(value: capstone::Error) -> Self {
        X86Error::Disasm(format!("{value}"))
    }
}

const RSP: Name = Name(0);
const RBP: Name = Name(1);
const RDI: Name = Name(2);
const RSI: Name = Name(3);
const RAX: Name = Name(4);
const RCX: Name = Name(5);
const RBX: Name = Name(6);
const RDX: Name = Name(7);
const R8: Name = Name(8);
const R9: Name = Name(9);
const R10: Name = Name(10);
const R11: Name = Name(11);
const R12: Name = Name(12);
const R13: Name = Name(13);
const R14: Name = Name(14);
const R15: Name = Name(15);

const FLAGS: Name = Name(16);

fn reg_to_name(reg: RegId) -> Option<Name> {
    match reg.0 as u32 {
        arch::x86::X86Reg::X86_REG_SP | arch::x86::X86Reg::X86_REG_ESP | arch::x86::X86Reg::X86_REG_RSP => Some(RSP),
        arch::x86::X86Reg::X86_REG_BP | arch::x86::X86Reg::X86_REG_EBP | arch::x86::X86Reg::X86_REG_RBP => Some(RBP),
        arch::x86::X86Reg::X86_REG_DI | arch::x86::X86Reg::X86_REG_EDI | arch::x86::X86Reg::X86_REG_RDI => Some(RDI),
        arch::x86::X86Reg::X86_REG_SI | arch::x86::X86Reg::X86_REG_ESI | arch::x86::X86Reg::X86_REG_RSI => Some(RSI),
        arch::x86::X86Reg::X86_REG_AX | arch::x86::X86Reg::X86_REG_EAX | arch::x86::X86Reg::X86_REG_RAX => Some(RAX),
        arch::x86::X86Reg::X86_REG_CX | arch::x86::X86Reg::X86_REG_ECX | arch::x86::X86Reg::X86_REG_RCX => Some(RCX),
        arch::x86::X86Reg::X86_REG_BX | arch::x86::X86Reg::X86_REG_EBX | arch::x86::X86Reg::X86_REG_RBX => Some(RBX),
        arch::x86::X86Reg::X86_REG_DX | arch::x86::X86Reg::X86_REG_EDX | arch::x86::X86Reg::X86_REG_RDX => Some(RDX),
        arch::x86::X86Reg::X86_REG_R8W | arch::x86::X86Reg::X86_REG_R8D | arch::x86::X86Reg::X86_REG_R8 => Some(R8),
        arch::x86::X86Reg::X86_REG_R9W | arch::x86::X86Reg::X86_REG_R9D | arch::x86::X86Reg::X86_REG_R9 => Some(R9),
        arch::x86::X86Reg::X86_REG_R10W | arch::x86::X86Reg::X86_REG_R10D | arch::x86::X86Reg::X86_REG_R10 => Some(R10),
        arch::x86::X86Reg::X86_REG_R11W | arch::x86::X86Reg::X86_REG_R11D | arch::x86::X86Reg::X86_REG_R11 => Some(R11),
        arch::x86::X86Reg::X86_REG_R12W | arch::x86::X86Reg::X86_REG_R12D | arch::x86::X86Reg::X86_REG_R12 => Some(R12),
        arch::x86::X86Reg::X86_REG_R13W | arch::x86::X86Reg::X86_REG_R13D | arch::x86::X86Reg::X86_REG_R13 => Some(R13),
        arch::x86::X86Reg::X86_REG_R14W | arch::x86::X86Reg::X86_REG_R14D | arch::x86::X86Reg::X86_REG_R14 => Some(R14),
        arch::x86::X86Reg::X86_REG_R15W | arch::x86::X86Reg::X86_REG_R15D | arch::x86::X86Reg::X86_REG_R15 => Some(R15),
        arch::x86::X86Reg::X86_REG_INVALID => None,
        x => panic!("Bad reg {x}")
    }
}

fn mem_addr(mem: &X86OpMem) -> Expr {
    let base = reg_to_name(mem.base());
    let index = reg_to_name(mem.index());
    
    let mut addr = Expr::ILit(mem.disp(), Typ::N64);
    if let Some(base) = base {
        addr = Expr::BinOp(BinOp::Add, Box::new(addr), Box::new(Expr::Name(base)));
    }
    if let Some(index) = index {
        addr = Expr::BinOp(BinOp::Add, Box::new(addr), Box::new(
            Expr::BinOp(BinOp::Lsl, Box::new(Expr::Name(index)), Box::new(Expr::ULit(mem.scale() as u64, Typ::N64)))
        ));
    }

    addr
}

fn op_expr(op: &arch::x86::X86Operand, typ: Typ) -> Expr {
    match op.op_type {
        arch::x86::X86OperandType::Reg(reg) => Expr::Name(reg_to_name(reg).unwrap()),
        arch::x86::X86OperandType::Imm(n) => Expr::ILit(n, typ),
        arch::x86::X86OperandType::Mem(mem) => Expr::Deref(Box::new(mem_addr(&mem))),
        arch::x86::X86OperandType::Invalid => panic!("Invalid")
    }
}

fn op_binding(op: &arch::x86::X86Operand) -> Binding {
    match op.op_type {
        arch::x86::X86OperandType::Reg(reg) => Binding::Name(reg_to_name(reg).unwrap()),
        arch::x86::X86OperandType::Imm(_) => panic!("Bad lhs"),
        arch::x86::X86OperandType::Mem(mem) => Binding::Deref(mem_addr(&mem)),
        arch::x86::X86OperandType::Invalid => panic!("Invalid")
    }
}

fn op_label(op: &arch::x86::X86Operand) -> Label {
    match op.op_type {
        arch::x86::X86OperandType::Imm(n) => Label(n as usize),
        _ => panic!("Not immediate")
    }
}

fn op_typ(op: &arch::x86::X86Operand) -> Option<Typ> {
    match op.op_type {
        arch::x86::X86OperandType::Reg(reg) => match reg.0 as u32 {
            arch::x86::X86Reg::X86_REG_SP | arch::x86::X86Reg::X86_REG_BP | arch::x86::X86Reg::X86_REG_DI |
            arch::x86::X86Reg::X86_REG_SI | arch::x86::X86Reg::X86_REG_AX | arch::x86::X86Reg::X86_REG_CX |
            arch::x86::X86Reg::X86_REG_BX | arch::x86::X86Reg::X86_REG_DX | arch::x86::X86Reg::X86_REG_R8W |
            arch::x86::X86Reg::X86_REG_R9W | arch::x86::X86Reg::X86_REG_R10W | arch::x86::X86Reg::X86_REG_R11W |
            arch::x86::X86Reg::X86_REG_R12W | arch::x86::X86Reg::X86_REG_R13W | arch::x86::X86Reg::X86_REG_R14W |
            arch::x86::X86Reg::X86_REG_R15W => Some(Typ::N16),
            
            arch::x86::X86Reg::X86_REG_ESP | arch::x86::X86Reg::X86_REG_EBP | arch::x86::X86Reg::X86_REG_EDI |
            arch::x86::X86Reg::X86_REG_ESI | arch::x86::X86Reg::X86_REG_EAX | arch::x86::X86Reg::X86_REG_ECX |
            arch::x86::X86Reg::X86_REG_EBX | arch::x86::X86Reg::X86_REG_EDX | arch::x86::X86Reg::X86_REG_R8D |
            arch::x86::X86Reg::X86_REG_R9D | arch::x86::X86Reg::X86_REG_R10D | arch::x86::X86Reg::X86_REG_R11D |
            arch::x86::X86Reg::X86_REG_R12D | arch::x86::X86Reg::X86_REG_R13D | arch::x86::X86Reg::X86_REG_R14D |
            arch::x86::X86Reg::X86_REG_R15D => Some(Typ::N32),

            arch::x86::X86Reg::X86_REG_RSP | arch::x86::X86Reg::X86_REG_RBP | arch::x86::X86Reg::X86_REG_RDI |
            arch::x86::X86Reg::X86_REG_RSI | arch::x86::X86Reg::X86_REG_RAX | arch::x86::X86Reg::X86_REG_RCX |
            arch::x86::X86Reg::X86_REG_RBX | arch::x86::X86Reg::X86_REG_RDX | arch::x86::X86Reg::X86_REG_R8 |
            arch::x86::X86Reg::X86_REG_R9 | arch::x86::X86Reg::X86_REG_R10 | arch::x86::X86Reg::X86_REG_R11 |
            arch::x86::X86Reg::X86_REG_R12 | arch::x86::X86Reg::X86_REG_R13 | arch::x86::X86Reg::X86_REG_R14 |
            arch::x86::X86Reg::X86_REG_R15 => Some(Typ::N64),
            
            x => panic!("Bad reg {x}")
        },
        arch::x86::X86OperandType::Imm(_) => None,
        arch::x86::X86OperandType::Mem(_) => None,
        arch::x86::X86OperandType::Invalid => panic!("Invalid mem type")
    }
}

pub fn gen_ir_func(raw: &BinFunc) -> Result<Func, X86Error> {
    let mut func = Func {
        args: vec![],
        ret: None,
        name: raw.name.clone(),
        code: vec![]
    };

    let cs = arch::BuildsCapstone::mode(Capstone::new()
        .x86(), arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()?;

    let insns = cs.disasm_all(&raw.code, raw.addr)?;

    for i in insns.as_ref() {
        // println!("{}", i);

        let detail: InsnDetail = cs.insn_detail(&i)?;
        let arch_detail: arch::ArchDetail = detail.arch_detail();
        let ops = arch_detail.operands();

        let op = |n: usize| match &ops[n] {
            arch::ArchOperand::X86Operand(op) => op,
            _ => panic!("Not and x86 operand?")
        };

        let loc = Loc { addr: i.address() };

        func.code.push(Instr::Label {
            loc,
            label: Label(i.address() as usize)
        });

        let opcode = arch::x86::X86Insn::from(i.id().0);
        match opcode {
            arch::x86::X86Insn::X86_INS_PUSH => {
                let typ = op_typ(op(0)).unwrap();
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Deref(Expr::Name(RSP)),
                    src: op_expr(op(0), typ)
                });
                func.code.push(Instr::Store {
                    loc,
                    typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::BinOp(BinOp::Sub, Box::new(Expr::Name(RSP)), Box::new(Expr::ULit(8, Typ::N64)))
                });
            }
            arch::x86::X86Insn::X86_INS_POP => {
                func.code.push(Instr::Store {
                    loc,
                    typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::BinOp(BinOp::Add, Box::new(Expr::Name(RSP)), Box::new(Expr::ULit(8, Typ::N64)))
                });

                let typ = op_typ(op(0)).unwrap();
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(0)),
                    src: Expr::Deref(Box::new(Expr::Name(RSP)))
                });
            }
            arch::x86::X86Insn::X86_INS_RET => {
                func.code.push(Instr::Return { loc, typ: Typ::N64, value: Expr::Name(RAX) });
            }
            arch::x86::X86Insn::X86_INS_MOV => {
                let typ = op_typ(op(0)).or_else(|| op_typ(op(1))).unwrap_or(Typ::N64);
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(1)),
                    src: op_expr(op(0), typ)
                });
            }
            arch::x86::X86Insn::X86_INS_ADD => {
                let typ = op_typ(op(0)).or_else(|| op_typ(op(1))).unwrap_or(Typ::N64);
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(1)),
                    src: Expr::BinOp(BinOp::Add, Box::new(op_expr(op(1), typ)), Box::new(op_expr(op(0), typ)))
                });
            }
            arch::x86::X86Insn::X86_INS_SUB => {
                let typ = op_typ(op(0)).or_else(|| op_typ(op(1))).unwrap_or(Typ::N64);
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(1)),
                    src: Expr::BinOp(BinOp::Sub, Box::new(op_expr(op(1), typ)), Box::new(op_expr(op(0), typ)))
                });
            }
            arch::x86::X86Insn::X86_INS_CMP => {
                let typ = op_typ(op(0)).or_else(|| op_typ(op(1))).unwrap_or(Typ::N64);
                func.code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(FLAGS),
                    src: Expr::BinOp(BinOp::Cmp, Box::new(op_expr(op(1), typ)), Box::new(op_expr(op(0), typ)))
                });
            }
            arch::x86::X86Insn::X86_INS_JL | arch::x86::X86Insn::X86_INS_JLE |
            arch::x86::X86Insn::X86_INS_JG | arch::x86::X86Insn::X86_INS_JGE |
            arch::x86::X86Insn::X86_INS_JE | arch::x86::X86Insn::X86_INS_JNE => {
                let rel = match opcode {
                    arch::x86::X86Insn::X86_INS_JL => MonOp::CmpLt,
                    arch::x86::X86Insn::X86_INS_JLE => MonOp::CmpLe,
                    arch::x86::X86Insn::X86_INS_JG => MonOp::CmpGt,
                    arch::x86::X86Insn::X86_INS_JGE => MonOp::CmpGe,
                    arch::x86::X86Insn::X86_INS_JE => MonOp::CmpEq,
                    arch::x86::X86Insn::X86_INS_JNE => MonOp::CmpNe,
                    _ => unreachable!()
                };
                func.code.push(Instr::Branch {
                    loc,
                    label: op_label(op(0)),
                    cond: Some(Expr::MonOp(rel, Box::new(Expr::Name(FLAGS))))
                });
            }
            arch::x86::X86Insn::X86_INS_JMP => {
                func.code.push(Instr::Branch {
                    loc,
                    label: op_label(op(0)),
                    cond: None
                });
            }
            _ => panic!("Instruction '{}' not translated", i)
        }
    }

    Ok(func)
}
