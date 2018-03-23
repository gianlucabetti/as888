000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300061009     D kpjba         E DS
000400140611     D fnls46ds1     E DS
000500140611     C                   CLEAR                   fnls46ds1
000600140611     c                   movel     063           ils46fls
000700130613     c                   movel     043           ils46lna
000800110114     c                   z-add     00            ils46nrs
000900140611     c                   z-add     27516         ils46nsc
001000110114     c                   movel     4             ils46npga
001100130613     c                   movel     043           ils46fgsa
001200140611     c                   z-add     63187         ils46nfva
001300110114     c
001400110119     c                   movel     3             ils46npgb
001500130613     c                   movel     043           ils46fgsb
001600140611     c                   z-add     34299         ils46nfvb
001700110114     c
001800110304     c                   movel     'M'           ils46tela
001900110114     c                   movel     'QD'          ils46amb
002000110304     c                   movel     'SE'          ils46aupr
002100110114
002200140611     C                   CALL      'FNLS46R1'
002300061009     C                   PARM                    kpjba
002400140611     C                   PARM                    fnls46ds1
002500061006     C*
002600020502     C                   SETON                                        LR
