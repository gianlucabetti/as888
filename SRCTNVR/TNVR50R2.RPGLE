000100990910
000200000210     fFNspp00T  IP   E             DISK
000300000210     fFIspp00T  O    E             DISK
000400990910
000500000210     dfnlv55ds       E ds
000600000211     D WLBDAT          DS                  INZ
000700000211     D  G02DAT                 1      8  0
000800000211     D  G02INV                 9     16  0
000900000211     D  G02ERR                17     17
001000000211     D  G02TGI                18     22  0
001100000210     IFnspp000
001200000210     I              sppnfv                      sppnfo
001300990914
001400000211     c     *in99         ifeq      *off
001500000211     c                   time                    w0140            14 0
001600000211     c* trasformo la data in aaaa/mm/gg
001700000211     c                   clear                   wlbdat
001800000211     C                   move      W0140         G02DAT
001900000211     C                   MOVEL     *BLANK        G02ERR
002000000211     C                   CALL      'XSRDA8'
002100000211     C                   PARM                    WLBDAT
002200000211     C                   z-add     G02INV        wdateu            8 0
002300000211     c                   call      'XSRDA8'
002400000211     c                   parm                    wlbdat
002500000211     c                   seton                                        99
002600000211     c                   endif
002700000210     c*
002800000210     c                   eval      sppnfv = sppnfo
002900000210     c                   eval      spppes = sppfgs
003000000211     c                   move      wdateu        sppdfs
003100000210     c                   movel     w0140         spphms
003200000211     c                   move      wdateu        sppdcs
003300000210     c                   movel     w0140         spphcs
003400000210     c                   clear                   sppdtr
003500000210     c                   clear                   spphtr
003600000211     c* se categoria 2 3 6 in spppuc e' memorizzata data/ora caricamento
003700000211     c* spunta
003800000210     c     sppnpg        ifeq      2
003900000210     c     sppnpg        oreq      6
004000000210     c     sppnpg        oreq      3
004100000210     c                   movel     spppuc        wdat4             4
004200000210     C                   MOVE      SPPPUC        WORA              3
004300000210     c     wora          ifgt      *zeros
004400000210     c     wdat4         andgt     *zeros
004500000210     C                   MOVEL     WDAT4         W0020             2 0
004600000210     C                   MOVE      WDAT4         sppdcs
004700000210    3C     w0020         IFEQ      01
004800000210     C     w0020         OREQ      02
004900000210     C                   MOVEL     2000          sppdcs
005000000210     C                   ELSE
005100000210     C                   MOVEL     1999          sppdcs
005200000210    3C                   endif
005300000210     c                   clear                   spphcs
005400000210     c                   movel     wora          spphcs
005500000210     c*
005600000210     c                   z-add     sppdcs        sppdfs
005700000210     c                   z-add     spphcs        spphms
005800000211     c                   clear                   spppuc
005900000210     c                   endif
006000000210     c                   endif
006100000211     c* In sppfle metto il terminal di partenza del p.o. gestione
006200000210     c                   movel     'P'           d55tpt
006300000210     c                   z-add     sppfgs        d55lin
006400000210     c                   z-add     sppdfv        d55drf
006500000210     c                   call      'FNLV55R'
006600000210     c                   parm                    fnlv55ds
006700000210     c                   z-add     d55tfp        sppfle
006800000210     c                   WRITE     fispp000
006900990914
