use crate::{RawBinary, FuncsBinary, BinMeta, BinFunc};

impl RawBinary {
    pub fn functions_from_meta(self) -> FuncsBinary {
        let mut function_starts = Vec::new();

        for meta in &self.meta {
            let BinMeta::Name { name, location } = meta else {
                continue;
            };

            if *location >= self.base_addr && *location < self.base_addr + self.code.len() as u64 {
                function_starts.push((*location, Some(name)));
            }
        }

        if function_starts.len() == 0  {
            return FuncsBinary {
                arch: self.arch,
                plt: self.plt,
                rodata: self.rodata,
                funcs: vec![BinFunc {
                    addr: self.base_addr,
                    code: self.code,
                    name: None
                }],
                meta: self.meta
            }
        }

        function_starts.sort();
        function_starts.push((self.base_addr + self.code.len() as u64, None));

        let mut funcs = Vec::new();
        for i in 0..function_starts.len() - 1 {
            let start = function_starts[i].0;
            let end = function_starts[i + 1].0;
            funcs.push(BinFunc {
                name: function_starts[i].1.cloned(),
                addr: start,
                code: self.code[(start - self.base_addr) as usize..(end - self.base_addr) as usize].into()
            });
        }

        FuncsBinary {
            arch: self.arch,
            plt: self.plt,
            rodata: self.rodata,
            funcs,
            meta: self.meta
        }
    }
}
