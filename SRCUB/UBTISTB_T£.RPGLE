000100120110     h dftactgrp(*no) actgrp('QILE') bnddir('UBBNDDIR')
000200111221     D pInAnno         S              4s 0
000300111221     D pInLineaPar     S              3s 0
000400111221     D pInSerie        S              2s 0
000500111221     D pInNumSped      S              7s 0
000600120110     D/COPY GAITRASRC/SRCPROTOPR,UBTISTB
000700120110     C                   IF        UBTISTB_Ctrl_Shipment_Sent(
000800120110     C                              2008 : 63 : 15 : 391606 : ' ' ) = '0'
000900120110     C                   IF        UBTISTB_Upd_Shipment_Sent(
001000120110     C                              2008 : 63 : 15 : 391606 : 'X') = '1'
001100120110     C     'OK'          DSPLY
001200120110     C                   ENDIF
001300120110     C                   ENDIF
001400111220     c                   seton                                        lr
