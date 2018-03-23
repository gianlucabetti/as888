000001171108     H DFTACTGRP(*NO) BNDDIR('PJXBND':'YDR') ACTGRP('QILE')
000002171108     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     d kpjba         e ds
000600170421     D*
000700020624     D param           DS
000800171108     D  parflg                       10                                         riferimento riga
000900171108     D  pardft                        8s 0                                      data fattura
000901171108     D  parnft                        9s 0                                      numero fattura
000902171108     D  pardpt                        8s 0                                      data protocollo
000903171108     D  parnpr                        9s 0                                      numero protocollo
000904171108     d  Oesito                       10i 0                                      esito sqlcod
001002171108     D*
001500170323     d
001600011026
001700000000     C*---------------------------------------------------------------*
001800020624     c     *entry        plist
001900020624     c                   parm                    kpjba
002000020624     c                   movel     kpjbu         param
002100020624
002700171108      *  Aggiorna righe corrispondenti al riferimento passato se in fase congrua
002800020624     C/EXEC SQL
002900171108     C+ update tmmof00F set moffas = 'FA', mofdft = :pardft,
002901171108     C+ mofnft = :parnft, mofdpt = :pardpt, mofnpt = :parnpt
003000171108     c+ where mofflg = :parflg  and moffas = 'BB'
003700020624     C/END-EXEC
008100170323      *
008102171108     c                   eval      oesito = sqlcod
008200020624     c                   seton                                        lr
008300011129     C*--------------------------------------------------------------------
