use phf_derive::PerfectHash;

#[derive(Debug, Copy, Clone, PartialEq, Hash, PerfectHash)]
pub enum Primitive {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}
