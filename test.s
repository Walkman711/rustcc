.global _main
.align 2

_main:
  mov w0, 2 
  cmp w0, wzr
  cset w0, eq
  uxtb w0, w0
  ret
