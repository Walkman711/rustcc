#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReturnType {
    NonVoid(VariableType),
    Void,
}

impl ReturnType {
    pub fn binop(&self, rh_ret: ReturnType) -> Self {
        match (self, rh_ret) {
            (ReturnType::NonVoid(lh), ReturnType::NonVoid(rh)) => ReturnType::NonVoid(lh.binop(rh)),
            (ReturnType::NonVoid(_), ReturnType::Void) => todo!(),
            (ReturnType::Void, ReturnType::NonVoid(_)) => todo!(),
            (ReturnType::Void, ReturnType::Void) => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VariableType {
    Num(NumericType),
    // Ptr,
    // Array,
    // Enum,
    // Union,
    // Struct,
}

impl VariableType {
    pub fn binop(&self, rh_vt: VariableType) -> Self {
        match (self, rh_vt) {
            (VariableType::Num(lh), VariableType::Num(rh)) => VariableType::Num(lh.binop(rh)),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NumericType {
    Int(IntegerType),
    Float(FloatingType),
}

impl NumericType {
    pub fn binop(&self, rh_nt: NumericType) -> Self {
        match (self, rh_nt) {
            (NumericType::Int(lh), NumericType::Int(rh)) => NumericType::Int(lh.binop(rh)),
            (NumericType::Int(_), NumericType::Float(_)) => todo!(),
            (NumericType::Float(_), NumericType::Int(_)) => todo!(),
            (NumericType::Float(_), NumericType::Float(_)) => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IntegerType {
    Char,
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

impl IntegerType {
    pub fn binop(&self, rh_it: IntegerType) -> Self {
        match (self, rh_it) {
            (IntegerType::Char, IntegerType::Char) => IntegerType::Char,
            (IntegerType::Char, IntegerType::Int) => IntegerType::Int,
            (IntegerType::Int, IntegerType::Char) => IntegerType::Int,
            (IntegerType::Int, IntegerType::Int) => IntegerType::Int,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FloatingType {
    // Float,
    // Double,
    // LongDouble,
}
