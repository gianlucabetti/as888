000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500061009     D kpjba         E DS
000600080702     c     *entry        plist
000700080911     c                   parm                    kpjba
000800080911     c                   movel     kpjbu         cpo              11
000900090521     c
001000080702     c                   if        cpo=*blanks
001100110707     c                   move      '00003890426' parmcpo          11
001200080702     c                   else
001300090521     c                   movel     cpo           parmcpo          11
001400080702     c                   endif
001500090521     C                   CALL      'TRMK08R'
001600061009     C                   PARM                    kpjba
001700090521     C                   PARM                    parmcpo
001800061006     C*
001900020502     C                   SETON                                        LR
