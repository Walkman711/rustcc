# Rustcc

## Features

### Implemented

### In Progress 
[] Switch Statements (6.8.1)
[] Goto, labels (6.8.1) 

### Backlog
[] Universal chars (6.4.3)
[] Constant expressions (6.4.4) -> revisit globals
[] Comma operator (6.5.17)
    - Context could have a parse fn decl mode/parse normal where we handle commas differently
[] type qualifiers (6.7.3)
    - const -> add info to var details, return an error if we attempt to assign to a const var
    - restrict -> need to deal with pointers. Stored per scope
    - volatile (5.1.2.3)
[] inlining (6.7.4) -> very low priority; seems like it introduces a fair amount of complexity
[] arrays (6.7.5.2)
    - very complex, could start with fixed size arrays and then look into VMA
    - functions can't return arrays
[] more numeric types
    - just integer types for now
[] floating point
    - different ball game, probably a never situation
[] ptrs
    - parsing will be a nightmare, but the asm seems reasonable
[] complex types
    - enums
    - struct
    - unions
[] typedef
