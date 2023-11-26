#![feature(let_chains)]
#![feature(if_let_guard)]
#![feature(box_patterns)]

mod clean;
pub use clean::*;

mod dataflow;
pub use dataflow::*;

mod cfggen;
pub use cfggen::*;

mod ifgen;
pub use ifgen::*;

mod loopgen;
pub use loopgen::*;

mod whilegen;
pub use whilegen::*;

mod frame;
pub use frame::*;

mod calls;
pub use calls::*;

pub mod visitor;

use std::{fmt::{Display, Write}, collections::{HashMap, HashSet}};

#[derive(Clone, Copy, Debug)]
pub struct Loc {
    pub addr: u64
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Typ {
    N8, N16, N32, N64
}

impl Typ {
    pub fn width(&self) -> i64 {
        match self {
            Typ::N8 => 1,
            Typ::N16 => 2,
            Typ::N32 => 4,
            Typ::N64 => 8,
        }
    }
}

impl Display for Typ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Typ::N64 => "n64",
            Typ::N32 => "n32",
            Typ::N16 => "n16",
            Typ::N8 => "n8",
        })
    }
}

#[derive(Debug)]
pub enum Instr {
    Store {
        loc: Loc,
        typ: Typ,
        dest: Binding,
        src: Expr
    },
    Label {
        loc: Loc,
        label: Label,
    },
    Branch {
        loc: Loc,
        label: Label,
        cond: Option<Expr>
    },
    Return {
        loc: Loc,
        typ: Typ,
        value: Expr
    },
    If {
        loc: Loc,
        cond: Expr,
        true_case: Vec<Instr>,
        false_case: Vec<Instr>,
    },
    Loop {
        loc: Loc,
        body: Vec<Instr>
    },
    While {
        loc: Loc,
        cond: Expr,
        body: Vec<Instr>
    },
    Break(Loc),
    Continue(Loc)
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Store { dest, src, .. } => write!(f, "{dest} = {src};"),
            Instr::Label { label, .. } => write!(f, "@{:x}:", label.0),
            Instr::Branch { label, cond: None, .. } => write!(f, "jump @0x{:x};", label.0),
            Instr::Branch { label, cond: Some(cond), .. } => write!(f, "if {cond} then jump @0x{:x};", label.0),
            Instr::Return { value, .. } => write!(f, "return {value};"),
            Instr::If { cond, true_case, false_case, .. } => {
                writeln!(f, "if {cond} {{")?;
                for stmt in true_case {
                    writeln!(f, "\t{}", stmt.to_string().replace('\n', "\n\t"))?;
                }
                if !false_case.is_empty() {
                    writeln!(f, "}} else {{")?;
                    for stmt in false_case {
                        writeln!(f, "\t{}", stmt.to_string().replace('\n', "\n\t"))?;
                    }
                }
                write!(f, "}}")?;
                Ok(())
            },
            Instr::Loop { body, .. } => {
                writeln!(f, "loop {{")?;
                for stmt in body {
                    writeln!(f, "\t{}", stmt.to_string().replace('\n', "\n\t"))?;
                }
                write!(f, "}}")?;
                Ok(())
            },
            Instr::While { body, cond, .. } => {
                writeln!(f, "while {cond} {{")?;
                for stmt in body {
                    writeln!(f, "\t{}", stmt.to_string().replace('\n', "\n\t"))?;
                }
                write!(f, "}}")?;
                Ok(())
            },
            Instr::Break(_) => write!(f, "break;"),
            Instr::Continue(_) => write!(f, "continue;"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(pub usize);

#[derive(Debug)]
pub enum Binding {
    Name(Name),
    Deref(Expr, Typ)
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::Name(n) => write!(f, "r{}", n.0),
            Binding::Deref(d, _) => write!(f, "*{d}")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add, Sub, Mul, Div,
    Lsl, Lsr, Asr,
    Cmp,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => f.write_char('+'),
            BinOp::Sub => f.write_char('-'),
            BinOp::Mul => f.write_char('*'),
            BinOp::Div => f.write_char('/'),
            BinOp::Cmp => f.write_str("cmp"),
            BinOp::Eq => f.write_str("=="),
            BinOp::Ne => f.write_str("!="),
            BinOp::Lt => f.write_str("<"),
            BinOp::Le => f.write_str("<="),
            BinOp::Gt => f.write_str(">"),
            BinOp::Ge => f.write_str(">="),
            BinOp::And => f.write_str("&"),
            BinOp::Or => f.write_str("|"),
            BinOp::Lsl => f.write_str("<<"),
            BinOp::Lsr => f.write_str(">>"),
            BinOp::Asr => f.write_str(">>>"),
        }
    }
}

#[derive(Debug)]
pub enum MonOp {
    Neg,
    Not,
    CmpEq, CmpNe, CmpLt, CmpLe, CmpGt, CmpGe,
}

impl Display for MonOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonOp::Neg => f.write_char('-'),
            MonOp::Not => f.write_char('!'),
            MonOp::CmpEq => f.write_str("(==)"),
            MonOp::CmpNe => f.write_str("(!=)"),
            MonOp::CmpLt => f.write_str("(<)"),
            MonOp::CmpLe => f.write_str("(<=)"),
            MonOp::CmpGt => f.write_str("(>)"),
            MonOp::CmpGe => f.write_str("(>=)"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Name(Name),
    Lit(i64, Typ),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    MonOp(MonOp, Box<Expr>),
    Deref(Box<Expr>, Typ),
    Call(Box<Expr>, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Name(name) => write!(f, "r{}", name.0),
            Expr::Lit(i, _) => write!(f, "{i}"),
            Expr::BinOp(op, l, r) => write!(f, "({l} {op} {r})"),
            Expr::MonOp(op, r) => write!(f, "{op}{r}"),
            Expr::Deref(e, _) => write!(f, "*{e}"),
            Expr::Call(func, args) => {
                write!(f, "{func}(")?;
                for (a, arg) in args.iter().enumerate() {
                    if a != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            },
        }
    }
}

#[derive(Debug)]
pub struct Func {
    pub addr: u64,
    pub short_name: Name,
    pub name: Option<String>,
    pub args: Vec<FuncArg>,
    pub ret: Option<Typ>,
    pub code: Vec<Instr>
}

#[derive(Debug)]
pub struct FuncArg {
    pub name: Name,
    pub typ: Typ
}

impl Instr {
    pub fn loc(&self) -> Loc {
        match self {
            Instr::Branch { loc, .. } | Instr::If { loc, .. } | Instr::Return { loc, .. } |
            Instr::Store { loc, .. } | Instr::Label { loc, .. } | Instr::Loop { loc, .. } |
            Instr::While { loc, .. } | Instr::Break(loc) | Instr::Continue(loc) => *loc,
        }
    }

    pub fn fallsthrough(&self) -> bool {
        match self {
            Instr::Branch { cond: None, .. } | Instr::Return { .. } | Instr::Break(_) | Instr::Continue(_) => false,
            Instr::Branch { .. } | Instr::If { .. } | Instr::Store { .. } |
            Instr::Label { .. } | Instr::Loop { .. } | Instr::While { .. } => true 
        }
    }

    pub fn dump_block(block: &[Instr]) {
        for instr in block {
            println!("{instr}");
        }
    }
}

impl Expr {
    pub fn not(self) -> Expr {
        match self {
            Expr::MonOp(MonOp::CmpEq, e) => Expr::MonOp(MonOp::CmpNe, e),
            Expr::MonOp(MonOp::CmpNe, e) => Expr::MonOp(MonOp::CmpEq, e),
            Expr::MonOp(MonOp::CmpLt, e) => Expr::MonOp(MonOp::CmpGe, e),
            Expr::MonOp(MonOp::CmpGe, e) => Expr::MonOp(MonOp::CmpLt, e),
            Expr::MonOp(MonOp::CmpLe, e) => Expr::MonOp(MonOp::CmpGt, e),
            Expr::MonOp(MonOp::CmpGt, e) => Expr::MonOp(MonOp::CmpLe, e),
            Expr::MonOp(MonOp::Not, e) => *e,
            
            Expr::BinOp(BinOp::Eq, l, r) => Expr::BinOp(BinOp::Ne, l, r),
            Expr::BinOp(BinOp::Ne, l, r) => Expr::BinOp(BinOp::Eq, l, r),
            Expr::BinOp(BinOp::Lt, l, r) => Expr::BinOp(BinOp::Ge, l, r),
            Expr::BinOp(BinOp::Ge, l, r) => Expr::BinOp(BinOp::Lt, l, r),
            Expr::BinOp(BinOp::Le, l, r) => Expr::BinOp(BinOp::Gt, l, r),
            Expr::BinOp(BinOp::Gt, l, r) => Expr::BinOp(BinOp::Le, l, r),

            e => Expr::MonOp(MonOp::Not, Box::new(e))
        }
    }

    pub fn take(&mut self) -> Expr {
        std::mem::replace(self, Expr::Name(Name(0)))
    }
}

#[derive(Debug)]
pub struct CfgBlock {
    pub label: usize,
    pub loc: Loc,
    pub code: Vec<Instr>
}

#[derive(Debug)]
pub struct Cfg {
    root: usize,
    incoming: HashMap<usize, HashSet<usize>>,
    outgoing: HashMap<usize, HashSet<usize>>,
    backward_edges: Option<HashSet<(usize, usize)>>
}

impl Cfg {
    pub fn new(root: usize) -> Cfg {
        Cfg {
            root,
            incoming: HashMap::new(),
            outgoing: HashMap::new(),
            backward_edges: None
        }
    }

    pub fn root(&self) -> usize {
        self.root
    }

    pub fn add_node(&mut self, node: usize) {
        self.incoming.insert(node, HashSet::new());
        self.outgoing.insert(node, HashSet::new());
    }

    pub fn add_edge(&mut self, src: usize, dst: usize) {
        self.outgoing.get_mut(&src).expect("src not a node").insert(dst);
        self.incoming.get_mut(&dst).expect("dst not a node").insert(src);
        self.backward_edges = None;
    }

    pub fn outgoing_for(&self, src: usize) -> &HashSet<usize> {
        self.outgoing.get(&src).expect("not a node")
    }

    pub fn incoming_for(&self, dst: usize) -> &HashSet<usize> {
        self.incoming.get(&dst).expect("not a node")
    }

    pub fn generate_backward_edges(&mut self) {
        let mut stack_parents = HashSet::new();
        let mut stack = vec![(self.root, false)];
        let mut visited = HashSet::new();
        visited.insert(self.root);

        let mut backward_edges = HashSet::new();

        while let Some((node, remove)) = stack.pop() {
            if remove {
                stack_parents.remove(&node);
                continue
            }

            stack.push((node, true));
            stack_parents.insert(node);

            for child in self.outgoing_for(node) {
                if stack_parents.contains(child) {
                    backward_edges.insert((node, *child));
                    continue;
                }

                if visited.insert(*child) {
                    stack.push((*child, false));
                }
            }
        }

        self.backward_edges = Some(backward_edges);
    }

    pub fn is_backward_edge(&self, src: usize, dst: usize) -> bool {
        self.backward_edges.as_ref().expect("call generate_backward_edges first").contains(&(src, dst))
    }
}

#[derive(Debug)]
pub struct FrameElement {
    pub offset: i64,
    pub typ: Typ,
    pub name: Name
}

#[derive(Debug)]
pub struct Frame {
    fully_understood: bool,
    elements: Vec<FrameElement>,
    next_name_idx: usize
}

impl Frame {
    pub fn new(next_name_idx: usize) -> Frame {
        Frame {
            fully_understood: true,
            elements: Vec::new(),
            next_name_idx
        }
    }

    pub fn is_fully_understood(&self) -> bool {
        self.fully_understood
    }

    pub fn set_not_fully_understood(&mut self) {
        self.fully_understood = false;
    }

    pub fn add(&mut self, offset: i64, typ: Typ) -> bool {
        match self.elements.binary_search_by_key(&offset, |e| e.offset) {
            Ok(idx) => {
                if self.elements[idx].typ != typ {
                    self.set_not_fully_understood();
                }
                self.fully_understood
            },
            Err(idx) => {
                if idx != 0 {
                    let prev = self.elements.get(idx - 1).unwrap();
                    if prev.offset + prev.typ.width() > offset {
                        self.set_not_fully_understood();
                        return false
                    }
                }
                if !self.elements.is_empty() && idx != self.elements.len() {
                    let next = self.elements.get(idx).unwrap();
                    if offset + typ.width() > next.offset {
                        self.set_not_fully_understood();
                        return false
                    }
                }

                self.elements.insert(idx, FrameElement {
                    name: Name(self.next_name_idx),
                    offset, typ
                });
                self.next_name_idx += 1;
                self.fully_understood
            }
        }
    }

    pub fn name_for(&self, offset: i64) -> Name {
        self.elements[self.elements.binary_search_by_key(&offset, |e| e.offset).expect("frame element not found")].name
    }
}

pub struct Abi {
    pub caller_read: HashSet<Name>,
    pub fp: Name,
    pub func_args: Vec<Name>
}
