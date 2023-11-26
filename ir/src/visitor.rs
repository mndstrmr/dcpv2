use std::collections::{HashSet, HashMap, hash_map::Keys};

use crate::{Instr, Loc, Label, Expr, Name, Binding};

pub enum Visit {
    Open,
    Close
}

impl Visit {
    pub fn is_open(&self) -> bool {
        match self {
            Visit::Open => true,
            Visit::Close => false
        }
    }

    pub fn is_close(&self) -> bool {
        match self {
            Visit::Open => false,
            Visit::Close => true
        }
    }
}

pub struct UpdateSet {
    // all reads are assumed to occur before any writes
    reads: HashMap<Name, usize>,
    writes: Option<Name>
}

impl UpdateSet {
    pub fn new() -> UpdateSet {
        UpdateSet {
            reads: HashMap::new(),
            writes: None
        }
    }

    pub fn add_read(&mut self, name: Name) {
        let n = self.reads.entry(name).or_insert(0);
        *n = *n + 1;
    }

    pub fn add_all_reads(&mut self, it: impl Iterator<Item=Name>) {
        for item in it {
            self.add_read(item);
        }
    }

    pub fn write(&mut self, name: Name) {
        assert!(self.writes.is_none());
        self.writes = Some(name);
    }

    pub fn extend(&mut self, other: UpdateSet) {
        for (name, count) in other.reads {
            let n = self.reads.entry(name).or_insert(0);
            *n = *n + count;
        }

        assert!(self.writes.is_none() || other.writes.is_none());
        self.writes = self.writes.or(other.writes);
    }

    pub fn reads_before_writes(&self, name: Name) -> bool {
        self.reads.contains_key(&name)
    }

    pub fn writes_doesnt_read(&self, name: Name) -> bool {
        !self.reads.contains_key(&name) && self.writes == Some(name)
    }

    pub fn writes(&self, name: Name) -> bool {
        self.writes == Some(name)
    }

    pub fn read_count(&self, name: Name) -> usize {
        self.reads.get(&name).copied().unwrap_or(0)
    }

    pub fn read_names<'a>(&'a self) -> Keys<'a, Name, usize> {
        self.reads.keys()
    }

    pub fn write_name(&self) -> Option<Name> {
        self.writes
    }
}

impl Expr {
    pub fn visit<F>(&self, f: &mut F) where F: FnMut(&Expr) {
        f(self);

        match self {
            Expr::BinOp(_, l, r) => {
                l.visit(f);
                r.visit(f);
            }
            Expr::MonOp(_, e) => e.visit(f),
            Expr::Deref(e, _) => e.visit(f),
            Expr::Call(func, args) => {
                func.visit(f);
                for arg in args {
                    arg.visit(f);
                }
            }
            Expr::Name(_) | Expr::ILit(_, _) | Expr::ULit(_, _) => {}
        }
    }

    pub fn visit_mut_post<F>(&mut self, f: &mut F) where F: FnMut(&mut Expr) {
        match self {
            Expr::BinOp(_, l, r) => {
                l.visit_mut_post(f);
                r.visit_mut_post(f);
            }
            Expr::MonOp(_, e) => e.visit_mut_post(f),
            Expr::Deref(e, _) => e.visit_mut_post(f),
            Expr::Call(func, args) => {
                func.visit_mut_post(f);
                for arg in args {
                    arg.visit_mut_post(f);
                }
            }
            Expr::Name(_) | Expr::ILit(_, _) | Expr::ULit(_, _) => {}
        }

        f(self);
    }

    pub fn vars(&self) -> HashSet<Name> {
        let mut names = HashSet::new();
        self.visit(&mut |e| if let Expr::Name(name) = e {
            names.insert(*name);
        });
        names
    }
}

impl Binding {
    pub fn vars(&self) -> UpdateSet {
        let mut names = UpdateSet::new();
        
        match self {
            Binding::Deref(e, _) => names.add_all_reads(e.vars().into_iter()),
            Binding::Name(name) => names.write(*name)
        }

        names
    }
}

impl Instr {
    fn has_body(&self) -> bool {
        match self {
            Instr::If { .. } | Instr::Loop { .. } | Instr::While { .. } => true,
        
            Instr::Branch { .. } | Instr::Return { .. } | Instr::Label { .. } |
            Instr::Store { .. } | Instr::Break(_) | Instr::Continue(_) => false
        }
    }
    
    pub fn vars(&self) -> UpdateSet {
        let mut vars = UpdateSet::new();
        self.visit_top_exprs(&mut |e: &Expr| {
            vars.add_all_reads(e.vars().into_iter());
        });

        if let Instr::Store { dest, .. } = self {
            vars.extend(dest.vars());
        }

        vars
    }

    pub fn visit_top_exprs<F>(&self, f: &mut F) where F: FnMut(&Expr) {
        self.visit(&mut |instr| match instr {
            Instr::Branch { cond: Some(cond), .. } => f(cond),
            Instr::Store { src, dest: Binding::Deref(e, _), .. } => {
                f(src);
                f(e);
            }
            Instr::Store { src, .. } => f(src),
            Instr::Return { value, .. } => f(value),
            Instr::While { cond, .. } => f(cond),
            Instr::If { cond, .. } => f(cond),
            Instr::Loop { .. } | Instr::Break(_) | Instr::Continue(_) |
            Instr::Label { .. } | Instr::Branch { cond: None, .. } => {}
        })
    }

    pub fn visit_top_exprs_mut<F>(&mut self, f: &mut F) where F: FnMut(&mut Expr) {
        self.visit_mut(&mut |instr| match instr {
            Instr::Branch { cond: Some(cond), .. } => f(cond),
            Instr::Store { src, dest: Binding::Deref(e, _), .. } => {
                f(src);
                f(e);
            }
            Instr::Store { src, .. } => f(src),
            Instr::Return { value, .. } => f(value),
            Instr::While { cond, .. } => f(cond),
            Instr::If { cond, .. } => f(cond),
            Instr::Loop { .. } | Instr::Break(_) | Instr::Continue(_) |
            Instr::Label { .. } | Instr::Branch { cond: None, .. } => {}
        })
    }

    // Needs to be &mut F since otherwise we get weird type errors (most likely a compiler bug I think)
    pub fn visit<F>(&self, f: &mut F) where F: FnMut(&Instr) {
        f(self);

        match self {
            Instr::If { true_case, false_case, .. } => {
                Instr::visit_all(true_case, f);
                Instr::visit_all(false_case, f);
            }
            Instr::Loop { body, .. } => Instr::visit_all(body, f),
            Instr::While { body, .. } => Instr::visit_all(body, f),
            _ => assert!(!self.has_body())
        }
    }

    pub fn visit_all<F>(instrs: &[Instr], f: &mut F) where F: FnMut(&Instr) {
        for instr in instrs {
            instr.visit(f);
        }
    }

    pub fn filter_mut<F>(&mut self, f: &mut F) where F: FnMut(&mut Instr) -> bool {
        match self {
            Instr::If { true_case, false_case, .. } => {
                Instr::filter_mut_all(true_case, f);
                Instr::filter_mut_all(false_case, f);
            }
            Instr::Loop { body, .. } => Instr::filter_mut_all(body, f),
            Instr::While { body, .. } => Instr::filter_mut_all(body, f),
            _ => assert!(!self.has_body())
        }
    }

    pub fn filter_mut_all<F>(instrs: &mut Vec<Instr>, f: &mut F) where F: FnMut(&mut Instr) -> bool {
        let mut i = 0;
        while i < instrs.len() {
            instrs[i].filter_mut(f);
            if f(&mut instrs[i]) {
                instrs.remove(i);
            } else {
                i += 1;
            }
        }
    }

    pub fn visit_mut<F>(&mut self, f: &mut F) where F: FnMut(&mut Instr) {
        f(self);
        self.filter_mut(&mut |i| {
            f(i);
            false
        })
    }
    
    pub fn visit_mut_all<F>(instrs: &mut Vec<Instr>, f: &mut F) where F: FnMut(&mut Instr) {
        Instr::filter_mut_all(instrs, &mut |i| {
            f(i);
            false
        })
    }

    pub fn filter_mut_pre_post<F>(&mut self, f: &mut F) where F: FnMut(&mut Instr, Visit) -> bool {
        match self {
            Instr::If { true_case, false_case, .. } => {
                Instr::filter_mut_all_pre_post(true_case, f);
                Instr::filter_mut_all_pre_post(false_case, f);
            }
            Instr::Loop { body, .. } => Instr::filter_mut_all_pre_post(body, f),
            Instr::While { body, .. } => Instr::filter_mut_all_pre_post(body, f),
            _ => assert!(!self.has_body())
        }
    }

    pub fn filter_mut_all_pre_post<F>(instrs: &mut Vec<Instr>, f: &mut F) where F: FnMut(&mut Instr, Visit) -> bool {
        let mut i = 0;
        while i < instrs.len() {
            if f(&mut instrs[i], Visit::Open) {
                instrs.remove(i);
                continue;
            }
            instrs[i].filter_mut_pre_post(f);
            if f(&mut instrs[i], Visit::Close) {
                instrs.remove(i);
                continue;
            }

            i += 1;
        }
    }

    pub fn visit_mut_blocks_pre<F>(&mut self, f: &mut F) where F: FnMut(&mut Vec<Instr>) {
        match self {
            Instr::If { true_case, false_case, .. } => {
                Instr::visit_mut_all_blocks_pre(true_case, f);
                Instr::visit_mut_all_blocks_pre(false_case, f);
            }
            Instr::Loop { body, .. } => Instr::visit_mut_all_blocks_pre(body, f),
            Instr::While { body, .. } => Instr::visit_mut_all_blocks_pre(body, f),
            _ => assert!(!self.has_body())
        }
    }

    pub fn visit_mut_all_blocks_pre<F>(instrs: &mut Vec<Instr>, f: &mut F) where F: FnMut(&mut Vec<Instr>) {
        f(instrs);
        for instr in instrs.iter_mut() {
            instr.visit_mut_blocks_pre(f);
        }
    }

    pub fn visit_mut_blocks_post<F>(&mut self, f: &mut F) where F: FnMut(&mut Vec<Instr>) {
        match self {
            Instr::If { true_case, false_case, .. } => {
                Instr::visit_mut_all_blocks_post(true_case, f);
                Instr::visit_mut_all_blocks_post(false_case, f);
            }
            Instr::Loop { body, .. } => Instr::visit_mut_all_blocks_post(body, f),
            Instr::While { body, .. } => Instr::visit_mut_all_blocks_post(body, f),
            _ => assert!(!self.has_body())
        }
    }

    pub fn visit_mut_all_blocks_post<F>(instrs: &mut Vec<Instr>, f: &mut F) where F: FnMut(&mut Vec<Instr>) {
        for instr in instrs.iter_mut() {
            instr.visit_mut_blocks_post(f);
        }
        f(instrs);
    }

    pub fn map<F>(&mut self, f: F) where F: Fn(Instr) -> Instr {
        self.filter_mut(&mut |g| {
            // I don't like this
            let x = std::mem::replace(g, Instr::Label { loc: Loc { addr: 0 }, label: Label(0) });
            *g = f(x);
            false
        });
    }

    pub fn map_all<F>(instrs: &mut [Instr], f: F) where F: Fn(Instr) -> Instr {
        for instr in instrs {
            instr.map(&f);
        }
    }
}
