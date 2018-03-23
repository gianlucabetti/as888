000100120110     h dftactgrp(*no) actgrp('QILE') bnddir('UBBNDDIR')
000200120118     D    Anno         S              4s 0
000300120118     D    LineaPar     S              3s 0
000400120118     D    Serie        S              2s 0
000500120118     D    NumSped      S              7s 0
000600120118     D    TpFunz       S              1
000700120118     D    Esito        S              1
000800120110     D/COPY GAITRASRC/SRCPROTOPR,UBTISTB
000900120118     C     *entry        PLIST
001000120118     C                   PARM                    Anno
001100120118     C                   PARM                    LineaPar
001200120118     C                   PARM                    Serie
001300120118     C                   PARM                    NumSped
001400120118     C                   PARM                    TpFunz
001500120118     C                   IF        TpFunz = '1'
001600120118     C                   EVAL      Esito = UBTISTB_Ctrl_Shipment_Sent(
001700120118     C                              Anno : LineaPar : Serie : NumSped : ' ' )
001800120118     C                   ELSE
001900120118     C                   EVAL      Esito = UBTISTB_Upd_Shipment_Sent(
002000120118     C                              Anno : LineaPar : Serie : NumSped : ' ' )
002100120110     C                   ENDIF
002200120118     C     Esito         DSPLY
002300111220     c                   seton                                        lr
