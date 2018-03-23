000001171114     H DFTACTGRP(*NO) ACTGRP(*caller)
000002171114     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300171114     H*--------------------------------------------------------------*
000400171114      *
000500171114     d kpjba         e ds
000600171114     D*
000700171114     D param           DS
000800171114     D  parflg                       10                                         riferimento riga
000900171114     D  pardft                        8s 0                                      data fattura
000901171114     D  parnft                        9s 0                                      numero fattura
000904171114     d  Oesito                       10i 0                                      esito sqlcod
001002171114     D*
001500171114     d
001600171114
001700171114     C*---------------------------------------------------------------*
001800171114     c     *entry        plist
001900171114     c                   parm                    kpjba
002000171114     c                   movel     kpjbu         param
002100171114
002700171114      *  Aggiorna righe corrispondenti al riferimento passato se in fase congrua
002800171114     C/EXEC SQL
002900171114     C+ update tmmof00F set moffas = 'FA', mofdft = :pardft,
002901171115     C+ mofnft = :parnft
003000171114     c+ where mofflg = :parflg  and moffas = 'BB' with ur
003700171114     C/END-EXEC
008100171114      *
008102171114     c                   eval      oesito = sqlcod
008200171114     c                   seton                                        lr
008300171114     C*--------------------------------------------------------------------
