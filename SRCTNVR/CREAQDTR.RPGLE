000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300050428     ffnfvv02l  if   e           k disk
000400050428     ffndst01l  if   e           k disk
000500050428     ffiqdt01l  uf a e           k disk
000600001127     d knpg            s                   like(fvvnpg) inz(4)
000700050428     d kddc            s                   like(fvvdfv) inz(20050502)
000800000607      *--------------------------------------------------------------------
000900050428     c     kfvv          klist
001000050428     c                   kfld                    knpg
001100050428     c                   kfld                    kddc
001200050428     c
001300050428     c     kqdt          klist
001400050428     c                   kfld                    fvvfgs
001500050428     c                   kfld                    knfv              6 0
001600050428     c                   kfld                    fvvdfv
001700050428     c
001800050428     c     kdst          klist
001900050428     c                   kfld                    fvvnfv
002000050428     c                   kfld                    fvvfgs
002100050428     c
002200000608      * reperimento dati foglio
002300050428     c     kfvv          setll     fnfvv02l
002400001127     c                   do        *hival
002500050428     c     knpg          reade     fnfvv02l                               99
002600001127     c   99              leave
002700050428     c                   z-add     fvvnfv        knfv
002800050428      * verifico che non esista già su fiqdt
002900050428     c     kqdt          chain     fiqdt01l                           98
003000050428     c  n98              iter
003100050428      * reperisco codice autista
003200050428     c     kdst          chain     fndst01l
003300050428      * scrittura rekord fiqdt
003400050428     c                   clear                   fiqdt000
003500050428     C                   z-add     fvvfgs        qdtfgs
003600050428     C                   z-add     fvvnfv        qdtnfv
003700050428     C                   z-add     fvvdfv        qdtddc
003800050428     C                   z-add     dstpdr        qdtpdr
003900050428     C                   WRITE     Fiqdt000
004000001127     c                   enddo
004100000607     c                   seton                                        lr
