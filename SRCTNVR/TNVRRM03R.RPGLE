000100940224     H*--------------------------------------------------------------*
000200060517     Ftita434c  IF   E           K DISK    ignore(tita4000)
000300060517     Ftita430c  UF A E           K DISK    ignore(tita4000)
000400060517     f                                     rename(tita4p00:tita430)
000500060512     f                                     prefix(N_)
000600060512     d dsbl4f        e ds
000700060512     d dsbl4I        e ds
000800060517     D wtrc            S                   LIKE(ta4trc)
000900900131     C*---------------------------------------------------------------*
001000060517     c     kta4          klist
001100060517     c                   kfld                    ta4aas
001200060517     c                   kfld                    ta4lnp
001300060517     c                   kfld                    ta4nrs
001400060517     c                   kfld                    ta4nsp
001500060512     c                   kfld                    wtrc
001600060512     c
001700060512
001800060517     c     *loval        setll     tita434c
001900060517    1C                   DO        *hival
002000060517     c                   read      tita434c
002100060517    2c                   if        %eof(tita434c)
002200060517     c                   leave
002300060517    2c                   endif
002400060517     c                   movel     ta4not        dsbl4f
002500060512     c* DSBL4I
002600060512     c                   clear                   dsbl4i
002700060512     c                   movel     §b4fnp        §b4ipn
002800060512     c* tolgo il check digit dal parcel number
002900060512     c                   eval      %subst(§b4ipn:12:1)=' '
003000060512     c                   eval      §b4icd = %subst(§b4fnp:12:1)
003100060512     c* depot di arrivo --> muovo uno 0 in prima posizione
003200060517     c                   if        §b4rtn <> *blanks
003300060512     c                   movel     '0'           §b4ida
003400060517     c                   endif
003500060512     C                   MOVE      §b4RTN        §B4ida
003600060512     C                   MOVE      §b4ZON        §B4iza
003700060512     c                   eval      wtrc = 'I'
003800060517     c     kta4          chain     tita430c
003900060517     c                   movel(P)  dsbl4i        N_ta4not
004000060517    2c                   if        %found(tita430c)
004100060517     c                   update    tita430
004200060512     c                   else
004300060517     c                   z-add     ta4aas        N_ta4aas
004400060517     c                   z-add     ta4lnp        N_ta4lnp
004500060517     c                   z-add     ta4nrs        N_ta4nrs
004600060517     c                   z-add     ta4nsp        N_ta4nsp
004700060517     c                   movel     'I'           N_ta4TRC
004800060517     c                   write     tita430
004900060517    2c                   endif
005000060512      *
005100060517    1C                   ENDDO
005200030205      *
005300000000     C                   SETON                                        LR
