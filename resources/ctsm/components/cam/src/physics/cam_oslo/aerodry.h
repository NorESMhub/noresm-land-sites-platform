!    For subroutine initdryp and intdrypar:

       common /dryarr1/                        &
       a0cintbg, a0cintbg05, a0cintbg125,      &
       a0aaeros, a0aaerol, a0vaeros, a0vaerol, &
       a1var, a2to3var, a4var, a5to10var

  real(r8)  a0cintbg, a0cintbg05, a0cintbg125, &
            a0aaeros, a0aaerol, a0vaeros, a0vaerol
  real(r8)  a1var(19,6,16,6)
  real(r8)  a2to3var(19,16,6,2:3)
  real(r8)  a4var(19,6,16,6,6)
  real(r8)  a5to10var(19,6,6,6,6,5:10)
