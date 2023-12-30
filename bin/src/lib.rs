mod func_detect;

#[derive(Debug)]
pub enum Arch {
    ArmV8,
    X86_64,
}

#[derive(Debug)]
pub struct RawBinary {
    pub arch: Arch,
    pub base_addr: u64,
    pub plt: Vec<DataBlock>,
    pub rodata: Option<DataBlock>,
    pub code: Vec<u8>,
    pub meta: Vec<BinMeta>
}

#[derive(Debug)]
pub struct DataBlock {
    pub base_addr: u64,
    pub data: Vec<u8>
}

#[derive(Debug)]
pub enum BinMeta {
    Region {
        typ: RegionType,
        start: u64,
        length: u64
    },
    Name {
        name: String,
        location: u64
    },
    PltElement {
        name: String,
        location: u64
    },
    Entry {
        location: u64
    }
}

#[derive(Debug)]
pub enum RegionType {
    Code,
    Zero,
    RoData,
    Data
}

#[derive(Debug)]
pub struct FuncsBinary {
    pub arch: Arch,
    pub plt: Vec<DataBlock>,
    pub rodata: Option<DataBlock>,
    pub funcs: Vec<BinFunc>,
    pub meta: Vec<BinMeta>
}

#[derive(Debug)]
pub struct BinFunc {
    pub name: Option<String>,
    pub addr: u64,
    pub code: Vec<u8>
}
