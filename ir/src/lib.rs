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

mod printer;
pub use printer::*;

mod string;
pub use string::*;

pub mod visitor;

mod funcdetect;
pub use funcdetect::*;

use std::{fmt::{Display, Write}, collections::{HashMap, HashSet}};
use std::fmt::Formatter;
use std::hash::Hash;

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
        value: Option<Expr>
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
    For {
        loc: Loc,
        cond: Expr,
        init: Vec<Instr>,
        step: Vec<Instr>,
        body: Vec<Instr>
    },
    Break(Loc),
    Continue(Loc),
    Expr {
        loc: Loc,
        expr: Expr
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Store { dest, src, .. } => write!(f, "{dest} = {src};"),
            Instr::Label { label, .. } => write!(f, "@{:x}:", label.0),
            Instr::Branch { label, cond: None, .. } => write!(f, "jump @0x{:x};", label.0),
            Instr::Branch { label, cond: Some(cond), .. } => write!(f, "if {cond} then jump @0x{:x};", label.0),
            Instr::Return { value: Some(value), .. } => write!(f, "return {value};"),
            Instr::Return { value: None, .. } => write!(f, "return;"),
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
            Instr::For { init, body, cond, step, .. } => {
                writeln!(f, "for ")?;
                if !init.is_empty() {
                    assert_eq!(init.len(), 1);
                    write!(f, "{}; ", init[0])?;
                }
                write!(f, "; {}", cond)?;
                assert_eq!(step.len(), 1);
                write!(f, "; {}", step[0])?;
                write!(f, "{{")?;
                for stmt in body {
                    writeln!(f, "\t{}", stmt.to_string().replace('\n', "\n\t"))?;
                }
                write!(f, "}}")?;
                Ok(())
            },
            Instr::Break(_) => write!(f, "break;"),
            Instr::Continue(_) => write!(f, "continue;"),
            Instr::Expr { expr, .. } => writeln!(f, "{expr}")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Name(pub u32, pub Namespace);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Local,
    Global,
    Register,
    External
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            Namespace::Local => write!(f, "local{}", self.0),
            Namespace::Global => write!(f, "global{}", self.0),
            Namespace::Register => write!(f, "r{}", self.0),
            Namespace::External => write!(f, "ext{}", self.0),
        }
    }
}

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
            Binding::Name(n) => write!(f, "{n}"),
            Binding::Deref(d, _) => write!(f, "*{d}")
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div,
    Lsl, Lsr, Asr,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or, Xor
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => f.write_char('+'),
            BinOp::Sub => f.write_char('-'),
            BinOp::Mul => f.write_char('*'),
            BinOp::Div => f.write_char('/'),
            BinOp::Eq => f.write_str("=="),
            BinOp::Ne => f.write_str("!="),
            BinOp::Lt => f.write_str("<"),
            BinOp::Le => f.write_str("<="),
            BinOp::Gt => f.write_str(">"),
            BinOp::Ge => f.write_str(">="),
            BinOp::And => f.write_str("&"),
            BinOp::Or => f.write_str("|"),
            BinOp::Xor => f.write_str("^"),
            BinOp::Lsl => f.write_str("<<"),
            BinOp::Lsr => f.write_str(">>"),
            BinOp::Asr => f.write_str(">>>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Expr {
    Name(Name),
    Lit(i64, Typ),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    MonOp(MonOp, Box<Expr>),
    Deref(Box<Expr>, Typ),
    Call(Box<Expr>, Vec<Expr>),
    StringLit(String)
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Name(name) => write!(f, "{name}"),
            Expr::Lit(i, _) => write!(f, "{i:x}"),
            Expr::BinOp(op, l, r) => write!(f, "({l} {op} {r})"),
            Expr::MonOp(op, r) => write!(f, "{op}{r}"),
            Expr::Deref(e, _) => write!(f, "(*{e})"),
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
            Expr::StringLit(str) => write!(f, "\"{}\"", str)
        }
    }
}

#[derive(Debug)]
pub struct Func {
    pub addr: u64,
    pub short_name: Name,
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
            Instr::While { loc, .. } | Instr::Break(loc) | Instr::Continue(loc) |
            Instr::For { loc, .. } | Instr::Expr { loc, .. } => *loc,
        }
    }

    pub fn fallsthrough(&self) -> bool {
        match self {
            Instr::Branch { cond: None, .. } | Instr::Return { .. } | Instr::Break(_) | Instr::Continue(_) => false,
            Instr::Branch { .. } | Instr::If { .. } | Instr::Store { .. } |
            Instr::Label { .. } | Instr::Loop { .. } | Instr::While { .. } |
            Instr::For { .. } | Instr::Expr { .. } => true
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
        std::mem::replace(self, Expr::Name(Name(0, Namespace::Register)))
    }
}

#[derive(Debug)]
pub struct CfgBlock {
    pub label: usize,
    pub loc: Loc,
    pub code: Vec<Instr>
}

#[derive(Debug, Clone)]
pub struct Graph<T> {
    root: T,
    incoming: HashMap<T, HashSet<T>>,
    outgoing: HashMap<T, HashSet<T>>,
    backward_edges: Option<HashSet<(T, T)>>
}

pub type Cfg = Graph<usize>;
pub type CallGraph = Graph<Name>;

impl<T: Clone + Copy + PartialEq + Eq + Hash> Graph<T> {
    pub fn new(root: T) -> Graph<T> {
        Graph {
            root,
            incoming: HashMap::new(),
            outgoing: HashMap::new(),
            backward_edges: None
        }
    }

    pub fn root(&self) -> T {
        self.root
    }

    pub fn add_node(&mut self, node: T) {
        if !self.incoming.contains_key(&node) {
            self.incoming.insert(node, HashSet::new());
            self.outgoing.insert(node, HashSet::new());
        }
    }

    pub fn remove_node(&mut self, node: T) {
        for outgoing in self.outgoing_for(node).clone() {
            self.incoming.get_mut(&outgoing).unwrap().remove(&node);
        }
        for incoming in self.incoming_for(node).clone() {
            self.outgoing.get_mut(&incoming).unwrap().remove(&node);
        }

        self.incoming.remove(&node);
        self.outgoing.remove(&node);
    }

    pub fn add_edge(&mut self, src: T, dst: T) {
        self.outgoing.get_mut(&src).expect("src not a node").insert(dst);
        self.incoming.get_mut(&dst).expect("dst not a node").insert(src);
        self.backward_edges = None;
    }

    pub fn add_edge_add_nodes(&mut self, src: T, dst: T) {
        if !self.outgoing.contains_key(&src) {
            self.outgoing.insert(src, HashSet::new());
            self.incoming.insert(src, HashSet::new());
        }

        if !self.outgoing.contains_key(&dst) {
            self.outgoing.insert(dst, HashSet::new());
            self.incoming.insert(dst, HashSet::new());
        }

        self.outgoing.get_mut(&src).expect("src not a node").insert(dst);
        self.incoming.get_mut(&dst).expect("dst not a node").insert(src);
        self.backward_edges = None;
    }

    pub fn remove_edge(&mut self, src: T, dst: T) {
        self.outgoing.get_mut(&src).expect("src not a node").remove(&dst);
        self.incoming.get_mut(&dst).expect("dst not a node").remove(&src);
        self.backward_edges = None;
    }

    pub fn outgoing_for(&self, src: T) -> &HashSet<T> {
        self.outgoing.get(&src).expect("not a node")
    }

    pub fn incoming_for(&self, dst: T) -> &HashSet<T> {
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

    pub fn is_backward_edge(&self, src: T, dst: T) -> bool {
        self.backward_edges.as_ref().expect("call generate_backward_edges first").contains(&(src, dst))
    }

    pub fn topological_sort(&self) -> Vec<T> {
        let mut new_graph = self.clone();

        let mut sorted = Vec::new();
        let mut nodes_with_no_incoming = vec![self.root];

        while let Some(node) = nodes_with_no_incoming.pop() {
            sorted.push(node);

            for outgoing in new_graph.outgoing_for(node).clone() {
                if self.is_backward_edge(node, outgoing) {
                    continue
                }

                new_graph.remove_edge(node, outgoing);
                if new_graph.incoming_for(outgoing).is_empty() {
                    nodes_with_no_incoming.push(outgoing);
                }
            }
        }

        sorted
    }

    pub fn trim_unreachable(&mut self) {
        for node in self.incoming.keys().cloned().collect::<Vec<_>>() {
            if self.incoming_for(node).is_empty() && node != self.root() {
                self.remove_node(node)
            }
        }
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
    next_name_idx: u32
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            fully_understood: true,
            elements: Vec::new(),
            next_name_idx: 0
        }
    }

    pub fn locals(&self) -> &[FrameElement] {
        &self.elements
    }

    pub fn is_fully_understood(&self) -> bool {
        self.fully_understood
    }

    pub fn set_not_fully_understood(&mut self) {
        self.fully_understood = false;
    }

    pub fn add(&mut self, offset: i64, typ: Typ) -> bool {
        match self.elements.binary_search_by_key(&offset, |e| e.offset) {
            Ok(_) => {
                // FIXME: This seems to be incorrectly blocking at the moment
                // if self.elements[idx].typ != typ {
                //     self.set_not_fully_understood();
                // }
                self.fully_understood
            }
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
                    name: Name(self.next_name_idx, Namespace::Local),
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
    pub called_read: HashSet<Name>,
    pub fp: Name,
    pub func_args: Vec<Name>
}
