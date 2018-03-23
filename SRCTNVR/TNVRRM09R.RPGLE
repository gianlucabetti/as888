000100940224     H*--------------------------------------------------------------*
000200060614     Fazorg01l  iF   E           k DISK
000300060614     Ftitas35c  iF   E           k DISK
000400060614     f                                     prefix(arb:3)
000500060614     Ftita430c  UF A E           k DISK
000600060512     d dsbl4I        e ds
000700060614     d og143         e ds
000800060614     D wtrc            S                   LIKE(ta4trc) inz('I')
000900060614     ititas000      01
001000060614     ititas010      02
001100060614     ititasp00      03
001200900131     C*---------------------------------------------------------------*
001300060512     c     kar4          klist
001400060614     c                   kfld                    arbaas
001500060614     c                   kfld                    arblnp
001600060614     c                   kfld                    arbnrs
001700060614     c                   kfld                    arbnsp
001800060512     c                   kfld                    wtrc
001900060614     c     *loval        setll     azorg01l
002000060614     c                   read      azorg01l
002100060614     c                   dow       not %eof(azorg01l)
002200060614     c                   movel     orgde3        og143
002300060614     c                   if        §ogntw='DPD'
002400060614     c                   setoff                                       010203
002500060614     c     orgfil        setll     titas35c
002600060614     c     orgfil        reade     titas35c
002700060614     c
002800060614     c                   dow       not %eof(titas35c) and arbaas<=2005
002900060614     c* Verifico se presente record I
003000060614     c     kar4          chain(N)  tita430c
003100060614     c                   if        not %found(tita430c)
003200060614     c* Imposto il riferimento numerico nel record I
003300060614     c                   move      arbrmn        w0120            12 0
003400060614     c                   movel     w0120         w011a            11
003500060614     c                   move      w0120         w001a             1
003600060512     c* DSBL4I
003700060614     c                   clear                   tita4000
003800060614     c                   clear                   tita4p00
003900060614     c                   clear                   dsbl4i
004000060614     c                   movel     w011a         §b4ipn
004100060614     c                   eval      §b4icd = w001a
004200060614     c                   eval      ta4aas=arbaas
004300060614     c                   eval      ta4lnp=arblnp
004400060614     c                   eval      ta4nrs=arbnrs
004500060614     c                   eval      ta4nsp=arbnsp
004600060615     c                   eval      ta4trc='I'
004700060614     c                   movel(P)  dsbl4i        ta4not
004800060614     c  n03              write     tita4000
004900060614     c   03              write     tita4p00
005000060512     c                   endif
005100060512      *
005200060614     c                   setoff                                       010203
005300060614     c     orgfil        reade     titas35c
005400060614     C                   ENDDO
005500060614     c                   endif
005600060614     c                   read      azorg01l
005700060614     C                   ENDDO
005800030205      *
005900000000     C                   SETON                                        LR
