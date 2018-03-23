000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500061009     D kpjba         E DS
000600090907     D tntl35ds      E DS
000700090907     C                   CLEAR                   tntl35ds
000800090907     c                   movel     'D'           d35tup
000900090907     C                   MOVEL     'A'           d35epa
001000090907     C                   z-add     604           d35trn
001100090907     C                   z-add     20090825      d35dde
001200090907     C                   z-add     20090904      d35dtn
001300090907     C                   z-add     002           d35nfm
001400090907     c                   movel     tntl35ds      kpjbu
001500090907     C                   CALL      'TNTL35R'
001600061009     C                   PARM                    kpjba
001700061006     C*
001800020502     C                   SETON                                        LR
