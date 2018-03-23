000100051108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200051108     fFIAR800F_Oip   e             disk    rename(FIAR8000:FIAR8old)
000300051108     f                                     prefix(O_)
000400051108     fFIAR800F  o    e             disk    rename(FIAR8000:FIAR8new)
000500051108     f                                     prefix(N_)
000600051108     c                   eval      N_AR8atb = O_AR8atb
000700051108     c                   eval      N_AR8aas = O_AR8aas
000800051108     c                   eval      N_AR8lnp = O_AR8lnp
000900051108     c                   eval      N_AR8nrs = O_AR8nrs
001000051108     c                   eval      N_AR8nsp = O_AR8nsp
001100051108     c                   eval      N_AR8tmd = O_AR8tmd
001200051108     c                   eval      N_AR8prr = O_AR8prr
001300051108     c                   eval      N_AR8trk = O_AR8trk
001400051108     c                   eval      N_AR8uni = %trimr(O_ar8uni)
001500051108     c                   write     FIAR8new
