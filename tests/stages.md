- 1: Basic lexing, parsing, generate enough assembly to have a return statement
- 2: Unary operators
- 3: Binary operators Pt. 1
- 4: Binary operators Pt. 2
- 5: Local variables
- 6: Conditionals
- 7: Compound Statements
- 8: Loop constructs
- 9: Functions
- 10: Global variables
- 11: Typing, Pt. 1
    * enforce proper args for main
    * allow `int main(void)` style definition in addition to `int main()`.
    * don't handle arrays or pointers yet, so `int(argc, argv)` will have to happen later
- 12: Typing, Pt. 2 - sizeof
    * implement enough of a type system that I can distinguish between `int` and `char`
    * use 4-bytes for int size, probably should have some `#[cfg]` stuff going on
    * able to get the type of an expression
    * can check that a function's return value has the correct type
    * type-checking for initialization and assignment statements
- 13: Identifiers
    * wasn't handling these properly
    * leading char must be [a-Z] or '_'
    * rest can be [a-Z], [0-9], '_'
- 14: Enums
