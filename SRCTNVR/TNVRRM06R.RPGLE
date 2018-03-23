000100940224     H*--------------------------------------------------------------*
000200060517     Ftita434c  IF   E           K DISK    ignore(tita4p00)
000300060517     Ftita430c  UF A E           K DISK    ignore(tita4p00)
000400060517     f                                     rename(tita4000:tita430)
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
002400060614     c                   if        ta4aas < 2006
002500060614     c                   iter
002600060614     c                   endif
002700060614     c                   if        ta4lnp < 190 or ta4lnp>199
002800060614     c                   iter
002900060614     c                   endif
003000060614     c
003100060517     c                   movel     ta4not        dsbl4f
003200060512     c* DSBL4I
003300060512     c                   clear                   dsbl4i
003400060512     c                   movel     §b4fnp        §b4ipn
003500060512     c* tolgo il check digit dal parcel number
003600060512     c                   eval      %subst(§b4ipn:12:1)=' '
003700060512     c                   eval      §b4icd = %subst(§b4fnp:12:1)
003800060512     c* depot di arrivo --> muovo uno 0 in prima posizione
003900060517     c                   if        §b4rtn <> *blanks
004000060512     c                   movel     '0'           §b4ida
004100060517     c                   endif
004200060512     C                   MOVE      §b4RTN        §B4ida
004300060512     C                   MOVE      §b4ZON        §B4iza
004400060512     c                   eval      wtrc = 'I'
004500060517     c     kta4          chain     tita430c
004600060517     c                   movel(P)  dsbl4i        N_ta4not
004700060517    2c                   if        %found(tita430c)
004800060517     c                   update    tita430
004900060512     c                   else
005000060517     c                   z-add     ta4aas        N_ta4aas
005100060517     c                   z-add     ta4lnp        N_ta4lnp
005200060517     c                   z-add     ta4nrs        N_ta4nrs
005300060517     c                   z-add     ta4nsp        N_ta4nsp
005400060517     c                   movel     'I'           N_ta4TRC
005500060517     c                   write     tita430
005600060517    2c                   endif
005700060512      *
005800060517    1C                   ENDDO
005900030205      *
006000000000     C                   SETON                                        LR
