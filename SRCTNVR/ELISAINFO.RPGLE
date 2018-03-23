000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500061009     D kpjba         E DS
000600080702     D trmk50ds      E DS
000700080702     c     *entry        plist
000800080911     c                   parm                    kpjba
000900080911     c                   movel     kpjbu         cpo              11
001000080702     C                   CLEAR                   trmk50ds
001100080702     c                   if        cpo=*blanks
001200080702     c                   move      31764923      i50cpo
001300080702     c                   else
001400080702     c                   movel     cpo           i50cpo
001500080702     c                   endif
001600080702     c                   movel     'PROVA'       i50rag
001700081215     c                   movel     'C'           i50mod
001800080702     C                   movel     'S'           i50obl
001801081215     C                   movel     ' '           i50tla
001900080702     C                   movel     'TRMK50R'     i50pgm
002000080702     C                   CALL      'TRMK50R'
002100061009     C                   PARM                    kpjba
002200080702     C                   PARM                    trmk50ds
002300061006     C*
002400020502     C                   SETON                                        LR
