#![feature(let_chains)]

use std::path::Path;

use bin::{RawBinary, Arch, BinMeta, DataBlock};
use elf::{ElfBytes, abi::{EM_X86_64}, endian::AnyEndian};
use elf::abi::{DT_INIT_ARRAY, DT_INIT_ARRAYSZ, DT_FINI_ARRAY, DT_FINI_ARRAYSZ, STT_FUNC};
use elf::endian::EndianParse;

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

fn vaddr_to_data<'a>(vaddr: u64, size: u64, res: &ElfBytes<AnyEndian>, data: &'a [u8]) -> Result<&'a [u8], ElfError> {
    for segment in res.segments().ok_or_else(|| ElfError::Parse("no segments".to_string()))? {
        if segment.p_vaddr <= vaddr && vaddr < segment.p_vaddr + segment.p_filesz {
            assert!(vaddr + size <= segment.p_vaddr + segment.p_filesz);

            let start = (segment.p_offset + (vaddr - segment.p_vaddr)) as usize;
            return Ok(&data[start..start + size as usize])
        }
    }

    Err(ElfError::Parse(format!("virtual addr 0x{vaddr:x} not mapped")))
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

    meta.push(BinMeta::Entry {
        location: res.ehdr.e_entry
    });

    if let Some((symtab, strtab)) = res.symbol_table()? {
        for symbol in symtab.iter() {
            if symbol.st_name == 0 || symbol.st_symtype() != STT_FUNC {
                continue;
            }

            meta.push(BinMeta::Name {
                name: strtab.get(symbol.st_name as usize)?.to_string(),
                location: symbol.st_value
            });
        }
    }

    if
        let Some((symtab, strtab)) = res.dynamic_symbol_table()? &&
        let Some(relas) = res.section_header_by_name(".rela.plt")?
    {
        for x in res.section_data_as_relas(&relas)? {
            let sym = symtab.get(x.r_sym as usize)?;
            meta.push(BinMeta::PltElement {
                name: strtab.get(sym.st_name as usize)?.to_string(),
                location: x.r_offset
            });
        }
    }

    let mut plt_blocks = Vec::new();
    for name in &[".plt", ".plt.got", ".plt.sec"] {
        if let Some(header) = res.section_header_by_name(name)? {
            plt_blocks.extend(Some(DataBlock {
                base_addr: header.sh_addr,
                data: res.section_data(&header)?.0.to_vec()
            }));
        }
    }

    let mut rodata = None;
    if let Some(header) = res.section_header_by_name(".rodata")? {
        rodata = Some(DataBlock {
            base_addr: header.sh_addr,
            data: res.section_data(&header)?.0.to_vec()
        });
    }

    if let Some(dynamic) = res.dynamic()? {
        let mut init_array = None;
        let mut init_array_sz = None;
        let mut fini_array = None;
        let mut fini_array_sz = None;

        for entry in dynamic {
            match entry.d_tag {
                DT_INIT_ARRAY => init_array = Some(entry.d_ptr()),
                DT_INIT_ARRAYSZ => init_array_sz = Some(entry.d_val()),
                DT_FINI_ARRAY => fini_array = Some(entry.d_ptr()),
                DT_FINI_ARRAYSZ => fini_array_sz = Some(entry.d_val()),
                _ => {}
            }
        }

        if let Some(init_array) = init_array && let Some(init_array_sz) = init_array_sz {
            for i in 0..init_array_sz / 8 {
                meta.push(BinMeta::InitArrFunc {
                    location: if res.ehdr.endianness.is_little() {
                        u64::from_le_bytes(vaddr_to_data(init_array + i*8, 8, &res, &data)?.try_into().unwrap())
                    } else {
                        u64::from_be_bytes(vaddr_to_data(init_array + i*8, 8, &res, &data)?.try_into().unwrap())
                    }
                })
            }
        }

        if let Some(fini_array) = fini_array && let Some(fini_array_sz) = fini_array_sz {
            for i in 0..fini_array_sz / 8 {
                meta.push(BinMeta::FiniArrFunc {
                    location: if res.ehdr.endianness.is_little() {
                        u64::from_le_bytes(vaddr_to_data(fini_array + i*8, 8, &res, &data)?.try_into().unwrap())
                    } else {
                        u64::from_be_bytes(vaddr_to_data(fini_array + i*8, 8, &res, &data)?.try_into().unwrap())
                    }
                })
            }
        }
    }

    Ok(RawBinary {
        arch,
        plt: plt_blocks,
        rodata,
        base_addr,
        code,
        meta
    })
}
