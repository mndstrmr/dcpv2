#![feature(let_chains)]

use std::collections::{HashSet, HashMap};

use bin::{DataBlock};
use capstone::{Capstone, arch::{self, BuildsCapstoneSyntax, BuildsCapstone, x86::X86OpMem}, InsnDetail, RegId};
use ir::{Name, Loc, Typ, Binding, Expr, Instr, BinOp, MonOp, Label, Namespace};

#[derive(Debug)]
pub enum X86Error {
    Disasm(String)
}

impl From<capstone::Error> for X86Error {
    fn from(value: capstone::Error) -> Self {
        X86Error::Disasm(format!("{value}"))
    }
}

const RSP: Name = Name(0, Namespace::Register);
const RBP: Name = Name(1, Namespace::Register);
const RDI: Name = Name(2, Namespace::Register);
const RSI: Name = Name(3, Namespace::Register);
const RAX: Name = Name(4, Namespace::Register);
const RCX: Name = Name(5, Namespace::Register);
const RBX: Name = Name(6, Namespace::Register);
const RDX: Name = Name(7, Namespace::Register);
const R8: Name = Name(8, Namespace::Register);
const R9: Name = Name(9, Namespace::Register);
const R10: Name = Name(10, Namespace::Register);
const R11: Name = Name(11, Namespace::Register);
const R12: Name = Name(12, Namespace::Register);
const R13: Name = Name(13, Namespace::Register);
const R14: Name = Name(14, Namespace::Register);
const R15: Name = Name(15, Namespace::Register);

const XMM0: Name = Name(15, Namespace::Register);
const XMM1: Name = Name(16, Namespace::Register);
const XMM2: Name = Name(17, Namespace::Register);
const XMM3: Name = Name(18, Namespace::Register);
const XMM4: Name = Name(19, Namespace::Register);
const XMM5: Name = Name(20, Namespace::Register);
const XMM6: Name = Name(21, Namespace::Register);
const XMM7: Name = Name(22, Namespace::Register);

const FLAGS: Name = Name(23, Namespace::Register);

const DF: Name = Name(24, Namespace::Register);

const FS: Name = Name(25, Namespace::Register);

pub fn frame_ptr_name() -> Name {
    RBP
}

pub fn caller_read() -> HashSet<Name> {
    let mut set = HashSet::new();
    set.insert(RBP);
    set.insert(RSP);
    set
}

pub fn called_read() -> HashSet<Name> {
    let mut set = HashSet::new();
    set.insert(RBP);
    set.insert(RSP);
    set
}

pub fn func_args() -> Vec<Name> {
    vec![
        RDI, RSI, RDX, RCX, R8, R9
    ]
}

pub fn name_map() -> HashMap<Name, String> {
    let mut map = HashMap::new();
    map.insert(RSP, "sp".to_string());
    map.insert(RBP, "fp".to_string());
    map.insert(RDI, "rdi".to_string());
    map.insert(RSI, "rsi".to_string());
    map.insert(RDX, "rdx".to_string());
    map.insert(RCX, "rcx".to_string());
    map.insert(R8, "r8".to_string());
    map.insert(R9, "r9".to_string());
    map.insert(RAX, "rax".to_string());
    map.insert(RBX, "rbx".to_string());
    map.insert(FS, "fs".to_string());
    map.insert(XMM0, "xmm0".to_string());
    map.insert(XMM1, "xmm1".to_string());
    map.insert(XMM2, "xmm2".to_string());
    map.insert(XMM3, "xmm3".to_string());
    map.insert(XMM4, "xmm4".to_string());
    map.insert(XMM5, "xmm5".to_string());
    map.insert(XMM6, "xmm6".to_string());
    map.insert(XMM7, "xmm7".to_string());
    map
}

fn reg_to_name(reg: RegId) -> Option<Name> {
    match reg.0 as u32 {
        arch::x86::X86Reg::X86_REG_SP | arch::x86::X86Reg::X86_REG_ESP | arch::x86::X86Reg::X86_REG_RSP => Some(RSP),
        arch::x86::X86Reg::X86_REG_BP | arch::x86::X86Reg::X86_REG_EBP | arch::x86::X86Reg::X86_REG_RBP => Some(RBP),
        arch::x86::X86Reg::X86_REG_DIL | arch::x86::X86Reg::X86_REG_DI | arch::x86::X86Reg::X86_REG_EDI | arch::x86::X86Reg::X86_REG_RDI => Some(RDI),
        arch::x86::X86Reg::X86_REG_SIL | arch::x86::X86Reg::X86_REG_SI | arch::x86::X86Reg::X86_REG_ESI | arch::x86::X86Reg::X86_REG_RSI => Some(RSI),
        arch::x86::X86Reg::X86_REG_AL | arch::x86::X86Reg::X86_REG_AX | arch::x86::X86Reg::X86_REG_EAX | arch::x86::X86Reg::X86_REG_RAX => Some(RAX),
        arch::x86::X86Reg::X86_REG_CL | arch::x86::X86Reg::X86_REG_CX | arch::x86::X86Reg::X86_REG_ECX | arch::x86::X86Reg::X86_REG_RCX => Some(RCX),
        arch::x86::X86Reg::X86_REG_BL | arch::x86::X86Reg::X86_REG_BX | arch::x86::X86Reg::X86_REG_EBX | arch::x86::X86Reg::X86_REG_RBX => Some(RBX),
        arch::x86::X86Reg::X86_REG_DL | arch::x86::X86Reg::X86_REG_DX | arch::x86::X86Reg::X86_REG_EDX | arch::x86::X86Reg::X86_REG_RDX => Some(RDX),
        arch::x86::X86Reg::X86_REG_R8W | arch::x86::X86Reg::X86_REG_R8D | arch::x86::X86Reg::X86_REG_R8 => Some(R8),
        arch::x86::X86Reg::X86_REG_R9W | arch::x86::X86Reg::X86_REG_R9D | arch::x86::X86Reg::X86_REG_R9 => Some(R9),
        arch::x86::X86Reg::X86_REG_R10W | arch::x86::X86Reg::X86_REG_R10D | arch::x86::X86Reg::X86_REG_R10 => Some(R10),
        arch::x86::X86Reg::X86_REG_R11W | arch::x86::X86Reg::X86_REG_R11D | arch::x86::X86Reg::X86_REG_R11 => Some(R11),
        arch::x86::X86Reg::X86_REG_R12W | arch::x86::X86Reg::X86_REG_R12D | arch::x86::X86Reg::X86_REG_R12 => Some(R12),
        arch::x86::X86Reg::X86_REG_R13W | arch::x86::X86Reg::X86_REG_R13D | arch::x86::X86Reg::X86_REG_R13 => Some(R13),
        arch::x86::X86Reg::X86_REG_R14W | arch::x86::X86Reg::X86_REG_R14D | arch::x86::X86Reg::X86_REG_R14 => Some(R14),
        arch::x86::X86Reg::X86_REG_R15W | arch::x86::X86Reg::X86_REG_R15D | arch::x86::X86Reg::X86_REG_R15 => Some(R15),
        arch::x86::X86Reg::X86_REG_XMM0 => Some(XMM0), arch::x86::X86Reg::X86_REG_XMM1 => Some(XMM1),
        arch::x86::X86Reg::X86_REG_XMM2 => Some(XMM2), arch::x86::X86Reg::X86_REG_XMM3 => Some(XMM3),
        arch::x86::X86Reg::X86_REG_XMM4 => Some(XMM4), arch::x86::X86Reg::X86_REG_XMM5 => Some(XMM5),
        arch::x86::X86Reg::X86_REG_XMM6 => Some(XMM6), arch::x86::X86Reg::X86_REG_XMM7 => Some(XMM7),

        arch::x86::X86Reg::X86_REG_INVALID => None,
        x => panic!("Bad reg {x}")
    }
}

fn reg_to_expr(reg: RegId, addr: u64) -> Option<Expr> {
    match reg.0 as u32 {
        arch::x86::X86Reg::X86_REG_RIP => Some(Expr::Lit(addr as i64, Typ::N64)),
        _ => reg_to_name(reg).map(Expr::Name)
    }
}

fn mem_addr(mem: &X86OpMem, addr: u64) -> Expr {
    let base = reg_to_expr(mem.base(), addr);
    let index = reg_to_expr(mem.index(), addr);
    
    let mut addr = Expr::Lit(mem.disp(), Typ::N64);
    if let Some(base) = base {
        addr = Expr::BinOp(BinOp::Add, Box::new(addr), Box::new(base));
    }
    if let Some(index) = index {
        addr = Expr::BinOp(BinOp::Add, Box::new(addr), Box::new(
            Expr::BinOp(BinOp::Lsl, Box::new(index), Box::new(Expr::Lit(mem.scale() as i64, Typ::N64)))
        ));
    }

    match mem.segment().0 as u32 {
        0 | arch::x86::X86Reg::X86_REG_DS => addr,
        arch::x86::X86Reg::X86_REG_FS => Expr::BinOp(BinOp::Add, Box::new(Expr::Name(FS)), Box::new(addr)),
        seg => panic!("Unknown segment {seg}")
    }
}

fn op_expr(op: &arch::x86::X86Operand, typ: Typ, addr: u64) -> Expr {
    match op.op_type {
        arch::x86::X86OperandType::Reg(reg) => Expr::Name(reg_to_name(reg).unwrap()),
        arch::x86::X86OperandType::Imm(n) => Expr::Lit(n, typ),
        arch::x86::X86OperandType::Mem(mem) => Expr::Deref(Box::new(mem_addr(&mem, addr)), typ),
        arch::x86::X86OperandType::Invalid => panic!("Invalid")
    }
}

fn op_addr_expr(op: &arch::x86::X86Operand, addr: u64) -> Expr {
    match op.op_type {
        arch::x86::X86OperandType::Reg(_) => panic!("reg addr expr"),
        arch::x86::X86OperandType::Imm(_) => panic!("imm addr expr"),
        arch::x86::X86OperandType::Mem(mem) => mem_addr(&mem, addr),
        arch::x86::X86OperandType::Invalid => panic!("Invalid")
    }
}

fn op_binding(op: &arch::x86::X86Operand, typ: Typ, addr: u64) -> Binding {
    match op.op_type {
        arch::x86::X86OperandType::Reg(reg) => Binding::Name(reg_to_name(reg).unwrap()),
        arch::x86::X86OperandType::Imm(_) => panic!("Bad lhs"),
        arch::x86::X86OperandType::Mem(mem) => Binding::Deref(mem_addr(&mem, addr), typ),
        arch::x86::X86OperandType::Invalid => panic!("Invalid")
    }
}

fn op_label(op: &arch::x86::X86Operand) -> Label {
    match op.op_type {
        arch::x86::X86OperandType::Imm(n) => Label(n as usize),
        _ => {
            eprintln!("Warning: Non-immediate label: {op:?}");
            Label(usize::MAX)
        }
    }
}

fn op_typ(op: &arch::x86::X86Operand) -> Typ {
    match op.size {
        1 => Typ::N8,
        2 => Typ::N16,
        4 => Typ::N32,
        8 => Typ::N64,
        16 => Typ::N128,
        _ => panic!("Bad type")
    }
}

pub fn gen_plt_data(plt: &[DataBlock], plt_deref_map: &HashMap<u64, Name>) -> Result<HashMap<u64, Name>, X86Error> {
    let cs = arch::BuildsCapstone::mode(Capstone::new().x86(), arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()?;

    let mut map = HashMap::new();

    for block in plt {
        let mut fallthroughs = Vec::new();
        for instr in cs.disasm_all(&block.data, block.base_addr)?.as_ref() {
            let opcode = arch::x86::X86Insn::from(instr.id().0);
            if opcode == arch::x86::X86Insn::X86_INS_JMP {
                let detail: InsnDetail = cs.insn_detail(&instr)?;
                let arch_detail: arch::ArchDetail = detail.arch_detail();
                let ops = arch_detail.operands();

                let arch::ArchOperand::X86Operand(op) = &ops[0] else {
                    panic!("plt instr not x86 op")
                };

                let arch::x86::X86OperandType::Mem(mem) = op.op_type else {
                    continue
                };

                assert_eq!(mem.base().0 as u32, arch::x86::X86Reg::X86_REG_RIP as u32);

                let addr = instr.address() as i64 + mem.disp() + instr.len() as i64;
                let Some(name) = plt_deref_map.get(&(addr as u64)) else {
                    continue
                };

                for addr in fallthroughs.drain(..) {
                    map.insert(addr, *name);
                }
                map.insert(instr.address(), *name);
            } else {
                fallthroughs.push(instr.address());
            }
        }
    }

    Ok(map)
}

pub fn gen_ir(raw: &[u8], base: u64) -> Result<Vec<Instr>, X86Error> {
    let mut code = Vec::new();

    let cs = arch::BuildsCapstone::mode(Capstone::new().x86(), arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()?;

    let insns = cs.disasm_all(raw, base)?;

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

        code.push(Instr::Label {
            loc,
            label: Label(i.address() as usize)
        });

        let instr_rip = i.address() + i.len() as u64;

        let opcode = arch::x86::X86Insn::from(i.id().0);
        match opcode {
            arch::x86::X86Insn::X86_INS_PUSH => {
                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Deref(Expr::Name(RSP), typ),
                    src: op_expr(op(0), typ, instr_rip)
                });
                code.push(Instr::Store {
                    loc,
                    typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::BinOp(BinOp::Sub, Box::new(Expr::Name(RSP)), Box::new(Expr::Lit(8, Typ::N64)))
                });
            }
            arch::x86::X86Insn::X86_INS_LEAVE => {
                code.push(Instr::Store {
                    loc, typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::Name(RBP)
                });

                code.push(Instr::Store {
                    loc,
                    typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::BinOp(BinOp::Add, Box::new(Expr::Name(RSP)), Box::new(Expr::Lit(8, Typ::N64)))
                });

                code.push(Instr::Store {
                    loc, typ: Typ::N64,
                    dest: Binding::Name(RBP),
                    src: Expr::Deref(Box::new(Expr::Name(RSP)), Typ::N64)
                });
            }
            arch::x86::X86Insn::X86_INS_POP => {
                code.push(Instr::Store {
                    loc,
                    typ: Typ::N64,
                    dest: Binding::Name(RSP),
                    src: Expr::BinOp(BinOp::Add, Box::new(Expr::Name(RSP)), Box::new(Expr::Lit(8, Typ::N64)))
                });

                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(0), typ, instr_rip),
                    src: Expr::Deref(Box::new(Expr::Name(RSP)), typ)
                });
            }
            arch::x86::X86Insn::X86_INS_RET => {
                code.push(Instr::Return { loc, typ: Typ::N64, value: Some(Expr::Name(RAX)) });
            }
            arch::x86::X86Insn::X86_INS_MOV | arch::x86::X86Insn::X86_INS_MOVZX |
            arch::x86::X86Insn::X86_INS_MOVABS | arch::x86::X86Insn::X86_INS_MOVSD |
            arch::x86::X86Insn::X86_INS_MOVSB | arch::x86::X86Insn::X86_INS_MOVSQ |
            arch::x86::X86Insn::X86_INS_MOVSX | arch::x86::X86Insn::X86_INS_MOVSXD |
            arch::x86::X86Insn::X86_INS_MOVAPS | arch::x86::X86Insn::X86_INS_MOVSS | arch::x86::X86Insn::X86_INS_MOVQ => {
                code.push(Instr::Store {
                    loc, typ: op_typ(op(1)),
                    dest: op_binding(op(1), op_typ(op(1)), instr_rip),
                    src: op_expr(op(0), op_typ(op(0)), instr_rip)
                });
            }
            arch::x86::X86Insn::X86_INS_CBW | arch::x86::X86Insn::X86_INS_CWDE | arch::x86::X86Insn::X86_INS_CDQE => {
                // RAX = signextend(EAX) etc
            }
            arch::x86::X86Insn::X86_INS_CMOVL | arch::x86::X86Insn::X86_INS_CMOVLE |
            arch::x86::X86Insn::X86_INS_CMOVG | arch::x86::X86Insn::X86_INS_CMOVGE |
            arch::x86::X86Insn::X86_INS_CMOVE | arch::x86::X86Insn::X86_INS_CMOVNE |
            arch::x86::X86Insn::X86_INS_CMOVB | arch::x86::X86Insn::X86_INS_CMOVBE |
            arch::x86::X86Insn::X86_INS_CMOVA | arch::x86::X86Insn::X86_INS_CMOVAE => {
                let inv_cond = match opcode {
                    arch::x86::X86Insn::X86_INS_CMOVL | arch::x86::X86Insn::X86_INS_CMOVB => MonOp::CmpGe,
                    arch::x86::X86Insn::X86_INS_CMOVLE | arch::x86::X86Insn::X86_INS_CMOVBE => MonOp::CmpGt,
                    arch::x86::X86Insn::X86_INS_CMOVG | arch::x86::X86Insn::X86_INS_CMOVA => MonOp::CmpLe,
                    arch::x86::X86Insn::X86_INS_CMOVGE | arch::x86::X86Insn::X86_INS_CMOVAE => MonOp::CmpLt,
                    arch::x86::X86Insn::X86_INS_CMOVE => MonOp::CmpNe,
                    arch::x86::X86Insn::X86_INS_CMOVNE => MonOp::CmpEq,
                    _ => unreachable!()
                };

                code.push(Instr::Branch {
                    loc,
                    label: Label(i.address() as usize + i.len()),
                    cond: Some(Expr::MonOp(inv_cond, Box::new(Expr::Name(FLAGS))))
                });
                code.push(Instr::Store {
                    loc, typ: op_typ(op(1)),
                    dest: op_binding(op(1), op_typ(op(1)), instr_rip),
                    src: op_expr(op(0), op_typ(op(0)), instr_rip)
                });
            }
            arch::x86::X86Insn::X86_INS_LEA => {
                code.push(Instr::Store {
                    loc, typ: op_typ(op(1)),
                    dest: op_binding(op(1), op_typ(op(1)), instr_rip),
                    src: op_addr_expr(op(0), instr_rip)
                });
            }
            arch::x86::X86Insn::X86_INS_IDIV => {
                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(RDX),
                    src: Expr::BinOp(BinOp::Mod, Box::new(Expr::Name(RAX)), Box::new(op_expr(op(0), typ, instr_rip)))
                });
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(RAX),
                    src: Expr::BinOp(BinOp::Div, Box::new(Expr::Name(RAX)), Box::new(op_expr(op(0), typ, instr_rip)))
                });
            }
            arch::x86::X86Insn::X86_INS_MUL => {
                let typ = op_typ(op(0));
                // FIXME: The destination is technically RDX:RAX
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(RAX),
                    src: Expr::BinOp(BinOp::Mul, Box::new(Expr::Name(RAX)), Box::new(op_expr(op(0), typ, instr_rip)))
                });
            }
            arch::x86::X86Insn::X86_INS_SUB | arch::x86::X86Insn::X86_INS_ADD | arch::x86::X86Insn::X86_INS_IMUL |
            arch::x86::X86Insn::X86_INS_XOR | arch::x86::X86Insn::X86_INS_AND |
            arch::x86::X86Insn::X86_INS_OR | arch::x86::X86Insn::X86_INS_SHL | arch::x86::X86Insn::X86_INS_SAR |
            arch::x86::X86Insn::X86_INS_SHR | arch::x86::X86Insn::X86_INS_PXOR |
            arch::x86::X86Insn::X86_INS_SUBSS | arch::x86::X86Insn::X86_INS_ADDSS |
            arch::x86::X86Insn::X86_INS_MULSS | arch::x86::X86Insn::X86_INS_DIVSS => {
                let binop = match opcode {
                    arch::x86::X86Insn::X86_INS_ADD | arch::x86::X86Insn::X86_INS_ADDSS => BinOp::Add,
                    arch::x86::X86Insn::X86_INS_SUB | arch::x86::X86Insn::X86_INS_SUBSS => BinOp::Sub,
                    arch::x86::X86Insn::X86_INS_IMUL | arch::x86::X86Insn::X86_INS_MULSS => BinOp::Mul,
                    arch::x86::X86Insn::X86_INS_IDIV | arch::x86::X86Insn::X86_INS_DIVSS => BinOp::Div,
                    arch::x86::X86Insn::X86_INS_AND => BinOp::And,
                    arch::x86::X86Insn::X86_INS_OR => BinOp::Or,
                    arch::x86::X86Insn::X86_INS_XOR | arch::x86::X86Insn::X86_INS_PXOR => BinOp::Xor,
                    arch::x86::X86Insn::X86_INS_SHL => BinOp::Lsl,
                    arch::x86::X86Insn::X86_INS_SAR => BinOp::Asr,
                    arch::x86::X86Insn::X86_INS_SHR => BinOp::Lsr,
                    _ => unreachable!()
                };

                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(1), typ, instr_rip),
                    src: Expr::BinOp(binop, Box::new(op_expr(op(1), typ, instr_rip)), Box::new(op_expr(op(0), typ, instr_rip)))
                });
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(FLAGS),
                    src: op_expr(op(1), typ, instr_rip)
                });
            }
            arch::x86::X86Insn::X86_INS_CMP | arch::x86::X86Insn::X86_INS_TEST | arch::x86::X86Insn::X86_INS_COMISS => {
                let binop = match opcode {
                    arch::x86::X86Insn::X86_INS_CMP | arch::x86::X86Insn::X86_INS_COMISS => BinOp::Sub,
                    arch::x86::X86Insn::X86_INS_TEST => BinOp::And,
                    _ => unreachable!()
                };

                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ,
                    dest: Binding::Name(FLAGS),
                    src: Expr::BinOp(binop, Box::new(op_expr(op(1), typ, instr_rip)), Box::new(op_expr(op(0), typ, instr_rip)))
                });
            }
            arch::x86::X86Insn::X86_INS_JL | arch::x86::X86Insn::X86_INS_JLE |
            arch::x86::X86Insn::X86_INS_JG | arch::x86::X86Insn::X86_INS_JGE |
            arch::x86::X86Insn::X86_INS_JE | arch::x86::X86Insn::X86_INS_JNE |
            arch::x86::X86Insn::X86_INS_JB | arch::x86::X86Insn::X86_INS_JBE |
            arch::x86::X86Insn::X86_INS_JA | arch::x86::X86Insn::X86_INS_JAE => {
                let rel = match opcode {
                    arch::x86::X86Insn::X86_INS_JL | arch::x86::X86Insn::X86_INS_JB => MonOp::CmpLt,
                    arch::x86::X86Insn::X86_INS_JLE | arch::x86::X86Insn::X86_INS_JBE => MonOp::CmpLe,
                    arch::x86::X86Insn::X86_INS_JG | arch::x86::X86Insn::X86_INS_JA => MonOp::CmpGt,
                    arch::x86::X86Insn::X86_INS_JGE | arch::x86::X86Insn::X86_INS_JAE => MonOp::CmpGe,
                    arch::x86::X86Insn::X86_INS_JE => MonOp::CmpEq,
                    arch::x86::X86Insn::X86_INS_JNE => MonOp::CmpNe,
                    _ => unreachable!()
                };
                code.push(Instr::Branch {
                    loc,
                    label: op_label(op(0)),
                    cond: Some(Expr::MonOp(rel, Box::new(Expr::Name(FLAGS))))
                });
            }
            arch::x86::X86Insn::X86_INS_JNS => {
                code.push(Instr::Branch {
                    loc,
                    label: op_label(op(0)),
                    cond: Some(Expr::BinOp(BinOp::Ge, Box::new(Expr::Name(FLAGS)), Box::new(Expr::Lit(0, Typ::N64))))
                });
            }
            arch::x86::X86Insn::X86_INS_JS => {
                code.push(Instr::Branch {
                    loc,
                    label: op_label(op(0)),
                    cond: Some(Expr::BinOp(BinOp::Lt, Box::new(Expr::Name(FLAGS)), Box::new(Expr::Lit(0, Typ::N64))))
                });
            }
            arch::x86::X86Insn::X86_INS_JMP => {
                match op(0).op_type {
                    arch::x86::X86OperandType::Imm(n) => code.push(Instr::Branch {
                        loc,
                        label: Label(n as usize),
                        cond: None
                    }),
                    _ => {
                        // FIXME: Assuming tail call for now
                        // FIXME: Done as store then return, since return-call is not really supported by ir::calls
                        code.push(Instr::Store {
                            loc, typ: Typ::N64,
                            dest: Binding::Name(RAX),
                            src: Expr::Call(Box::new( op_expr(op(0), Typ::N64, instr_rip)), vec![])
                        });
                        code.push(Instr::Return {
                            loc,
                            typ: Typ::N64,
                            value: Some(Expr::Name(RAX))
                        })
                    }
                }
            }
            arch::x86::X86Insn::X86_INS_CALL => {
                code.push(Instr::Store {
                    loc, typ: Typ::N64,
                    dest: Binding::Name(RAX),
                    src: Expr::Call(Box::new( op_expr(op(0), Typ::N64, instr_rip)), vec![])
                });
            }
            arch::x86::X86Insn::X86_INS_SETG | arch::x86::X86Insn::X86_INS_SETA |
            arch::x86::X86Insn::X86_INS_SETGE | arch::x86::X86Insn::X86_INS_SETAE |
            arch::x86::X86Insn::X86_INS_SETL | arch::x86::X86Insn::X86_INS_SETB |
            arch::x86::X86Insn::X86_INS_SETLE | arch::x86::X86Insn::X86_INS_SETBE |
            arch::x86::X86Insn::X86_INS_SETNE | arch::x86::X86Insn::X86_INS_SETE => {
                let cmp = match opcode {
                    arch::x86::X86Insn::X86_INS_SETG | arch::x86::X86Insn::X86_INS_SETA => MonOp::CmpGt,
                    arch::x86::X86Insn::X86_INS_SETGE | arch::x86::X86Insn::X86_INS_SETAE => MonOp::CmpGe,
                    arch::x86::X86Insn::X86_INS_SETL | arch::x86::X86Insn::X86_INS_SETB => MonOp::CmpLt,
                    arch::x86::X86Insn::X86_INS_SETLE | arch::x86::X86Insn::X86_INS_SETBE => MonOp::CmpLe,
                    arch::x86::X86Insn::X86_INS_SETNE => MonOp::CmpNe,
                    arch::x86::X86Insn::X86_INS_SETE => MonOp::CmpEq,
                    _ => unreachable!()
                };

                code.push(Instr::Store {
                    loc, typ: Typ::N8,
                    dest: op_binding(op(0), Typ::N8, instr_rip),
                    src: Expr::MonOp(cmp, Box::new(Expr::Name(FLAGS)))
                });
            }
            arch::x86::X86Insn::X86_INS_NEG | arch::x86::X86Insn::X86_INS_NOT => {
                let monop = match opcode {
                    arch::x86::X86Insn::X86_INS_NEG => MonOp::Neg,
                    arch::x86::X86Insn::X86_INS_NOT => MonOp::Not,
                    _ => unreachable!()
                };

                let typ = op_typ(op(0));
                code.push(Instr::Store {
                    loc, typ: Typ::N8,
                    dest: op_binding(op(0), typ, instr_rip),
                    src: Expr::MonOp(monop, Box::new(op_expr(op(0), typ, instr_rip)))
                });
            }
            arch::x86::X86Insn::X86_INS_STOSQ if arch_detail.x86().unwrap().prefix()[0] == arch::x86::X86Prefix::X86_PREFIX_REP as u32 as u8 => {
                let typ = op_typ(op(0));
                // if counter = 0, finish
                code.push(Instr::Branch {
                    loc,
                    label: Label(i.address() as usize + i.len()),
                    cond: Some(Expr::BinOp(BinOp::Eq, Box::new(Expr::Name(RCX)), Box::new(Expr::Lit(0, Typ::N32))))
                });
                // Do the store
                code.push(Instr::Store {
                    loc, typ,
                    dest: op_binding(op(0), op_typ(op(0)), instr_rip),
                    src: Expr::Name(RAX)
                });
                // Update RDI
                code.push(Instr::Store {
                    loc, typ: Typ::N64,
                    dest: Binding::Name(RDI),
                    src: Expr::BinOp(BinOp::Sub, Box::new(Expr::Name(RDI)), Box::new(
                        Expr::BinOp(
                            BinOp::Sub,
                            Box::new(Expr::BinOp(BinOp::Mul, Box::new(Expr::Name(DF)), Box::new(Expr::Lit(typ.width() * 2, Typ::N64)))),
                            Box::new(Expr::Lit(typ.width(), Typ::N64))
                        )
                    ))
                });
                // Decrement counter
                code.push(Instr::Store {
                    loc, typ: Typ::N64,
                    dest: Binding::Name(RCX),
                    src: Expr::BinOp(BinOp::Sub, Box::new(Expr::Name(RCX)), Box::new(Expr::Lit(1, Typ::N32)))
                });
                // redo
                code.push(Instr::Branch {
                    loc,
                    label: Label(i.address() as usize),
                    cond: None
                });
            }
            arch::x86::X86Insn::X86_INS_CDQ => {
                // todo!("cld")
            }
            arch::x86::X86Insn::X86_INS_NOP | arch::x86::X86Insn::X86_INS_ENDBR64 => {}
            arch::x86::X86Insn::X86_INS_HLT => code.push(Instr::Return { value: None, loc, typ: Typ::N64 }), // FIXME: ???
            _ => {
                eprintln!("Warning: Instruction '{}' not translated", i)
            }
        }
    }

    Ok(code)
}
