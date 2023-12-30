use std::collections::HashMap;
use crate::{Expr, Func, Instr, Name, Namespace};

pub fn gen_funcs(mut code: Vec<Instr>, known_starts: &HashMap<u64, Name>, global_idx: &mut u32) -> Vec<Func> {
    if code.is_empty() {
        return Vec::new()
    }

    let mut func_starts = known_starts.clone();

    for instr in &code {
        let Instr::Store { src: Expr::Call(box Expr::Lit(addr, _), _), .. } = instr else {
            continue
        };

        if !func_starts.contains_key(&(*addr as u64)) {
            func_starts.insert(*addr as u64, Name(*global_idx, Namespace::Global));
            *global_idx += 1;
        }
    }

    let mut vec: Vec<_> = func_starts.into_iter().collect();
    vec.sort_by(|a, b| a.0.cmp(&b.0).reverse());
    while code[0].loc().addr > vec[vec.len() - 1].0 {
        vec.pop();
    }

    let mut funcs = Vec::new();
    'outer: for j in (1..vec.len()).rev() {
        for i in 0..code.len() {
            if code[i].loc().addr == vec[j - 1].0 {
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
    }
    funcs.push(Func {
        code: code.drain(..).collect(),
        addr: vec.first().unwrap().0,
        short_name: vec.first().unwrap().1,
        args: vec![],
        ret: None
    });


    assert!(code.is_empty());

    funcs
}