000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500061009     D kpjba         E DS
000600061009     D tisie8ds      E DS
000700061009     C                   CLEAR                   tisie8ds
000800061009     c                   movel     'D01'         isie8op0
000900061009     C                   MOVEL     'GB'          isie8nar
001000061009     C                   MOVEL     '2 '          isie8cap
001100061009     C                   CALL      'TISIE8R'
001200061009     C                   PARM                    kpjba
001300061009     C                   PARM                    tisie8ds
001400061006     C*
001500020502     C                   SETON                                        LR
