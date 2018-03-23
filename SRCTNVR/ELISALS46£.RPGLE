000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300061009     D kpjba         E DS
000400110114     D fnls46ds      E DS
000500110114     C                   CLEAR                   fnls46ds
000600110304     c                   movel     001           ils46fls
000700110114     c                   movel     001           ils46lna
000800110114     c                   z-add     00            ils46nrs
000900110304     c                   z-add     58690         ils46nsc
001000110114     c                   movel     4             ils46npga
001100110114     c                   movel     001           ils46fgsa
001200110304     c                   z-add     11860         ils46nfva
001300110114     c
001400110119     c                   movel     3             ils46npgb
001500110119     c                   movel     001           ils46fgsb
001600110304     c                   z-add     40104         ils46nfvb
001700110114     c
001800110304     c                   movel     'M'           ils46tela
001900110114     c                   movel     'QD'          ils46amb
002000110304     c                   movel     'SE'          ils46aupr
002100110114
002200110114     C                   CALL      'FNLS46R'
002300061009     C                   PARM                    kpjba
002400110114     C                   PARM                    fnls46ds
002500061006     C*
002600020502     C                   SETON                                        LR
