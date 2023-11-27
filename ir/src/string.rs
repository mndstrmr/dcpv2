use crate::{Expr, Instr, Typ};

pub fn insert_string_lits(code: &mut Vec<Instr>, rodata: &[u8], rodata_addr: u64) {
    Instr::visit_mut_all(code, &mut |instr| {
        instr.visit_top_exprs_mut(&mut |e| {
            e.visit_mut_post(&mut |e| if let Expr::Lit(addr, Typ::N64) = e {
                if (*addr as u64) < rodata_addr || (*addr as u64) >= rodata_addr + rodata.len() as u64 {
                    return
                }

                let starti = (*addr as u64 - rodata_addr) as usize;
                for i in starti..rodata.len() as usize {
                    if rodata[i] != 0 {
                        continue
                    }

                    if let Ok(str) = std::str::from_utf8(&rodata[starti..i]) {
                        *e = Expr::StringLit(str.to_string())
                    }
                }
            });
        })
    })
}