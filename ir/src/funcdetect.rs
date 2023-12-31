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

    fn split_tail_calls(&mut self, code: &[Instr]) -> bool {
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

    fn split_return_ends(&mut self, code: &[Instr]) -> bool {
        let mut furthest_branch = code[0].loc().addr;
        let start_addr = code[0].loc().addr;
        let end_addr = code[code.len() - 1].loc().addr;

        for (i, instr) in code.iter().enumerate() {
            let mut is_return = false;

            match instr {
                Instr::Branch { label, .. } => {
                    let addr = label.0 as u64;

                    if addr < start_addr || addr > end_addr {
                        // Then it's a tail-call-return, as per split_tail_calls
                        is_return = true;
                    } else {
                        furthest_branch = furthest_branch.max(addr);
                    }
                }
                Instr::Return { .. } => {
                    is_return = true;
                }
                _ => {}
            }

            if is_return {
                if i < code.len() - 1 && instr.loc().addr >= furthest_branch {
                    self.add_alloc_if_not_present(code[i + 1].loc().addr);
                    return true
                }
            }
        }

        false
    }
}

fn is_only_labels(code: &[Instr]) -> bool {
    for instr in code {
        let Instr::Label { .. } = instr else {
            return false
        };
    }

    true
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
    //    branches to outside of the function.
    // 4. In the same pass, if all the branches in a function end before a return (i.e. it would seem
    //    that code under the return is not reachable) then split there as well.
    'outer: loop {
        let vec = detector.ordered();
        assert_eq!(vec[0].0, code[0].loc().addr);

        let mut start = 0;
        'inner: for j in 0..vec.len() {
            if j != vec.len() - 1 {
                for i in start..code.len() {
                    if code[i].loc().addr == vec[j + 1].0 {
                        // The region start..i is a candidate for a function
                        if detector.split_tail_calls(&code[start..i]) || detector.split_return_ends(&code[start..i]) {
                            continue 'outer
                        }

                        start = i;
                        continue 'inner
                    }
                }
            }

            // The region start..end is a candidate
            if detector.split_tail_calls(&code[start..]) || detector.split_return_ends(&code[start..]) {
                continue 'outer
            }
            break
        }

        break
    }

    let vec = detector.ordered();

    // Now finally collect into functions
    let mut funcs = Vec::new();
    'outer: for j in 0..vec.len() {
        if j != vec.len() - 1 {
            for i in 0..code.len() {
                if code[i].loc().addr == vec[j + 1].0 {
                    let code = code.drain(0..i).collect::<Vec<_>>();
                    assert!(!code.is_empty());
                    if !is_only_labels(&code) {
                        funcs.push(Func {
                            code,
                            addr: vec[j].0,
                            short_name: vec[j].1,
                            args: vec![],
                            ret: None
                        });
                    }
                    continue 'outer
                }
            }
        }

        let code = code.drain(..).collect::<Vec<_>>();
        assert!(!code.is_empty());
        if !is_only_labels(&code) {
            funcs.push(Func {
                code,
                addr: vec[j].0,
                short_name: vec[j].1,
                args: vec![],
                ret: None
            });
        }
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