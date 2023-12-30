use std::collections::HashMap;
use crate::{Expr, Func, Instr, Name, Namespace, Typ};

struct FunctionDetector<'a> {
    global_idx: &'a mut u32,
    min_addr: u64,
    func_starts: HashMap<u64, Name>
}

impl<'a> FunctionDetector<'a> {
    fn has(&self, addr: u64) -> bool {
        self.func_starts.contains_key(&addr)
    }

    fn alloc_name(&mut self) -> Name {
        *self.global_idx += 1;
        Name(*self.global_idx - 1, Namespace::Global)
    }

    fn add_alloc_if_not_present(&mut self, addr: u64) -> bool {
        if !self.has(addr) {
            let name = self.alloc_name();
            self.add(addr, name);
            true
        } else {
            false
        }
    }

    fn add(&mut self, addr: u64, name: Name) {
        if addr < self.min_addr {
            return
        }

        self.func_starts.insert(addr, name);
    }

    fn ordered(&self) -> Vec<(u64, Name)> {
        let mut vec: Vec<_> = self.func_starts.iter().map(|(x, y)| (*x, *y)).collect();
        vec.sort_by(|a, b| a.0.cmp(&b.0));
        vec
    }

    fn add_out_of_bounds(&mut self, code: &[Instr]) -> bool {
        let mut changed = false;

        let start_addr = code[0].loc().addr;
        let end_addr = code[code.len() - 1].loc().addr;

        for instr in code {
            let Instr::Branch { label, .. } = instr else {
                continue
            };
            let addr = label.0 as u64;

            if start_addr <= addr && addr <= end_addr {
                continue
            }

            // label points to a new function
            changed = changed || self.add_alloc_if_not_present(addr);
        }

        changed
    }
}

pub fn gen_funcs(mut code: Vec<Instr>, known_starts: &HashMap<u64, Name>, global_idx: &mut u32) -> Vec<Func> {
    if code.is_empty() {
        return Vec::new()
    }

    let mut detector = FunctionDetector {
        global_idx,
        min_addr: code[0].loc().addr,
        func_starts: HashMap::new()
    };

    // 1. Start from known symbols
    for (addr, name) in known_starts {
        detector.add(*addr, *name);
    }

    // 2. Walk over the code, find all call targets
    for instr in &code {
        let Instr::Store { src: Expr::Call(box Expr::Lit(addr, _), _), .. } = instr else {
            continue
        };

        detector.add_alloc_if_not_present(*addr as u64);
    }

    // 3. Walk over code again now we have a guess at what a function is, add function starts for
    //    branches to outside of the function
    let mut changed = true;
    while changed {
        changed = false;

        let vec = detector.ordered();
        assert_eq!(vec[0].0, code[0].loc().addr);

        let mut start = 0;
        'outer: for j in 0..vec.len() {
            for i in start..code.len() {
                if code[i].loc().addr == vec[j + 1].0 {
                    // The region start..i is a candidate for a function
                    changed = changed || detector.add_out_of_bounds(&code[start..i]);
                    start = i;
                    continue 'outer
                }
            }

            // The region start..end is a candidate
            changed = changed || detector.add_out_of_bounds(&code[start..]);
            break
        }
    }

    let vec = detector.ordered();

    // Now finally collect into functions
    let mut funcs = Vec::new();
    'outer: for j in 0..vec.len() {
        for i in 0..code.len() {
            if code[i].loc().addr == vec[j + 1].0 {
                funcs.push(Func {
                    code: code.drain(0..i).collect(),
                    addr: vec[j].0,
                    short_name: vec[j].1,
                    args: vec![],
                    ret: None
                });
                continue 'outer
            }
        }

        funcs.push(Func {
            code: code.drain(..).collect(),
            addr: vec[j].0,
            short_name: vec[j].1,
            args: vec![],
            ret: None
        });
        break
    }

    assert!(code.is_empty());

    funcs
}

// Expects that labels are addresses
pub fn tail_call_to_call_return(code: &mut Vec<Instr>) {
    assert!(!code.is_empty());

    // let used_labels = used_labels(code);
    let start = code[0].loc().addr;
    let end = code[code.len() - 1].loc().addr;

    let mut i = 0;
    while i < code.len() {
        let Instr::Branch { label, cond, loc } = &mut code[i] else {
            i += 1;
            continue
        };
        let target_loc = label.0 as u64;

        if target_loc >= start && target_loc <= end {
            i += 1;
            continue
        }

        // Assume this is a function call
        if let Some(_cond) = cond {
            todo!()
        } else {
            code[i] = Instr::Return {
                value: Some(Expr::Call(
                    Box::new(Expr::Lit(target_loc as i64, Typ::N64)),
                    vec![]
                )),
                loc: *loc,
                typ: Typ::N64
            };
            i += 1;
        }
    }
}