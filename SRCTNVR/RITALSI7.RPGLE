000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980129     H* FNLS03R *----------------------------------------------------*
000300980129     H*         - MANUTENZIONE DATA SPEDIZIONE IN BOLLA -
000400000000     H*--------------------------------------------------------------*
000500061009     D kpjba         E DS
000600160329     D fnlsi7ds      E DS
000700160329     C                   CLEAR                   fnlsi7ds
000800170926     c                   movel     2017          ilsi7aas
000900170927     C                   MOVEL     194           ilsi7lnp
001000160329     C                   MOVEL     00            ilsi7nrs
001100170927     C                   MOVEL     0042384       ilsi7nsp
001200160330     c                   eval      ilsi7rsd='RITA  PROVA'
001300160330     c                   eval      ilsi7rD2='RITA  PROVA2'
001400160330     c                   eval      ilsi7IND='VIA BIANCHI, 31'
001500170927     c                   eval      ilsi7CAD='84121'
001600170927     c                   eval      ilsi7LOD='SALERNO'
001700170927     c                   eval      ilsi7PRD='SA'
001800160329     C                   CALL      'FNLSI7R'
001900061009     C                   PARM                    kpjba
002000160329     C                   PARM                    fnlsi7ds
002100061006     C*
002200020502     C                   SETON                                        LR
