use std::path::Path;

use bin::{RawBinary, Arch, BinMeta};
use elf::{ElfBytes, abi::{EM_X86_64}, endian::AnyEndian};

#[derive(Debug)]
pub enum ElfError {
    Io(std::io::Error),
    Parse(String),
    UnknownArch
}

impl From<elf::ParseError> for ElfError {
    fn from(value: elf::ParseError) -> Self {
        ElfError::Parse(format!("{value}"))
    }
}

pub fn read<P: AsRef<Path>>(path: P) -> Result<RawBinary, ElfError> {
    let data = match std::fs::read(path) {
        Ok(data) => data,
        Err(err) => return Err(ElfError::Io(err))
    };

    let res = ElfBytes::<AnyEndian>::minimal_parse(&data)?;

    let arch = match res.ehdr.e_machine {
        EM_X86_64 => Arch::X86_64,
        _ => return Err(ElfError::UnknownArch)
    };

    let (code, base_addr) = match res.section_header_by_name(".text")? {
        // Note: This does not handle compression
        Some(text) => (res.section_data(&text)?.0.into(), text.sh_addr),
        None => (vec![], 0)
    };

    let mut meta = Vec::new();
    if let Some((symtab, strtab)) = res.symbol_table()? {
        for symbol in symtab {
            if symbol.st_value == 0 || symbol.st_name == 0 {
                continue;
            }

            meta.push(BinMeta::Name {
                name: strtab.get(symbol.st_name as usize)?.to_string(),
                location: symbol.st_value
            });
        }
    }

    Ok(RawBinary {
        arch,
        base_addr,
        code,
        meta
    })
}
