000100940224     H*--------------------------------------------------------------*
000200060614     Fazorg01l  iF   E           k DISK
000300060614     Ffnarb27l  iF   E           k DISK
000400060614     Ffiar401l  UF A E           k DISK
000500060512     d dsbl4I        e ds
000600060614     d og143         e ds
000700060614     D wtrc            S                   LIKE(ar4trc) inz('I')
000800900131     C*---------------------------------------------------------------*
000900060512     c     kar4          klist
001000060614     c                   kfld                    arbaas
001100060614     c                   kfld                    arblnp
001200060614     c                   kfld                    arbnrs
001300060614     c                   kfld                    arbnsp
001400060512     c                   kfld                    wtrc
001500060614     c     *loval        setll     azorg01l
001600060614     c                   read      azorg01l
001700060614     c                   dow       not %eof(azorg01l)
001800060614     c                   movel     orgde3        og143
001900060614     c                   if        §ogntw='DPD'
002000060614     c     orgfil        setll     fnarb27l
002100060614     c     orgfil        reade     fnarb27l
002200060614     c
002300060614     c                   dow       not %eof(fnarb27l) and arbaas<=2005
002400060614     c* Verifico se presente record I
002500060614     c     kar4          chain(N)  fiar401l
002600060614     c                   if        not %found(fiar401l)
002700060614     c* Imposto il riferimento numerico nel record I
002800060614     c                   move      arbrmn        w0120            12 0
002900060614     c                   movel     w0120         w011a            11
003000060614     c                   move      w0120         w001a             1
003100060512     c* DSBL4I
003200060614     c                   clear                   fiar4000
003300060614     c                   clear                   dsbl4i
003400060614     c                   movel     w011a         §b4ipn
003500060614     c                   eval      §b4icd = w001a
003600060614     c                   eval      ar4aas=arbaas
003700060614     c                   eval      ar4lnp=arblnp
003800060614     c                   eval      ar4nrs=arbnrs
003900060614     c                   eval      ar4nsp=arbnsp
004000060614     c                   movel(P)  dsbl4i        ar4not
004100060614     c                   movel     'T'           ar4ftr
004200060614     c                   z-add     arbmgs        ar4dtr
004300060614     c                   movel     arbaas        ar4dtr
004400060614     c                   movel     'I'           AR4TRC
004500060614     c                   write     fiar4000
004600060512     c                   endif
004700060512      *
004800060614     c     orgfil        reade     fnarb27l
004900060614     C                   ENDDO
005000060614     c                   endif
005100060614     c                   read      azorg01l
005200060614     C                   ENDDO
005300030205      *
005400000000     C                   SETON                                        LR
