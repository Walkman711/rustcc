#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReturnType {
    Numeric(NumericType),
    Void,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumericType {
    Int(IntegerType),
    Float(FloatingType),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntegerType {
    // Char,
    // SignedChar,
    // UnsignedChar,
    // Short,
    // ShortInt,
    // SignedShort,
    // SignedShortInt,
    // UnsignedShort,
    // UnsignedShortInt,
    Int,
    // Signed,
    // SignedInt,
    // Unsigned,
    // UnsignedInt,
    // Long,
    // LongInt,
    // SignedLong,
    // SignedLongInt,
    // UnsignedLong,
    // UnsignedLongInt,
    // LongLong,
    // LongLongInt,
    // SignedLongLong,
    // SignedLongLongInt,
    // UnsignedLongLong,
    // UnsignedLongLongInt,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FloatingType {
    // Float,
    // Double,
    // LongDouble,
}
