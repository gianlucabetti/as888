000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020322     D KPJBA         E DS
000500980528     I***************************************************************
001000160127     C                   MOVEL     'IN'          prmfnz            2
001001160127     C                   MOVEL     0010076       prmksc            7 0
001002160127     C                   MOVEL     0010076       prmrmn           15 0
001003160127     C                   MOVEL     ' '           prmcmt            1
001004160127     C                   MOVEL     'L'           prmtla            1
001005160127     C                   MOVEL     '0'           prmesi            1
001006160127     C                   MOVEL     0000          prmaas            4 0
001007160127     C                   MOVEL     0000          prmlnp            3 0
001008160127     C                   MOVEL     0000          prmnrs            2 0
001009160127     C                   MOVEL     0000000       prmnsp            7 0
001800160127     C                   CALL      'TNSB55R'
001901160127     C                   PARM                    PrmFnz
001902160127     C                   PARM                    PrmKsc
001903160127     C                   PARM                    PrmRMN
001904160127     C                   PARM                    PrmCmt
001905160127     C                   PARM                    PrmTLa
001906160127     C                   PARM                    PrmEsi
001907160127     C                   PARM                    PrmAAS
001908160127     C                   PARM                    PrmLnP
001909160127     C                   PARM                    PrmNrS
001910160127     C                   PARM                    PrmNSp
002000020322     C*
002200020322     C                   SETON                                        LR
