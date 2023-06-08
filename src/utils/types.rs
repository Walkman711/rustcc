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

    pub fn sizeof(&self) -> usize {
        match self {
            ReturnType::NonVoid(nv) => nv.sizeof(),
            ReturnType::Void => unreachable!("Expressions should never be Void"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VariableType {
    Basic(BasicType),
    // Enum(EnumeratedType),
    // Derived(DerivedType),
    // Ptr,
    // Array,
    // Enum,
    // Union,
    // Struct,
}

impl VariableType {
    pub fn binop(&self, rh_vt: VariableType) -> Self {
        match (self, rh_vt) {
            (VariableType::Basic(lh), VariableType::Basic(rh)) => VariableType::Basic(lh.binop(rh)),
        }
    }

    pub fn sizeof(&self) -> usize {
        match self {
            VariableType::Basic(n) => n.sizeof(),
        }
    }
}

impl From<BasicType> for VariableType {
    fn from(value: BasicType) -> Self {
        Self::Basic(value)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BasicType {
    Int(IntegerType),
    Float(FloatingType),
}

impl BasicType {
    pub fn binop(&self, rh_nt: BasicType) -> Self {
        match (self, rh_nt) {
            (BasicType::Int(lh), BasicType::Int(rh)) => BasicType::Int(lh.binop(rh)),
            (BasicType::Int(_), BasicType::Float(_)) => todo!(),
            (BasicType::Float(_), BasicType::Int(_)) => todo!(),
            (BasicType::Float(_), BasicType::Float(_)) => todo!(),
        }
    }

    pub fn sizeof(&self) -> usize {
        match self {
            BasicType::Int(i) => i.sizeof(),
            BasicType::Float(_) => todo!(),
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

    pub fn sizeof(&self) -> usize {
        match self {
            IntegerType::Char => 1,
            // TODO: this should be platform specific, but will need to
            // redo some codegen stuff where i use w-registers instead
            // of x-registers.
            IntegerType::Int => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FloatingType {
    // Float,
    // Double,
    // LongDouble,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DerivedType {
    Ptr,
    Array,
    Struct,
    Union(Vec<VariableType>),
    FunType,
}
