000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400040615     fsisdp01l  Uf   e           k disk
000500040615     d titas           ds
000600040615     d comksc                              like(sdpksc)
000700040615     d comann                              like(sdpann)
000800040615     d commes                         2
000900040615     d comctr                              like(sdpctr)
001000040615     d comnsp                              like(sdpnsp)
001100040615     d comsvr          s                   like(sdpsvr) inz('O')
001200040615     d comftc          s                   like(sdpftc) inz
001300040615     C     Keysdp        KLIST
001400040615     C                   KFLD                    comksc
001500040615     C                   KFLD                    comann
001600040615     C                   KFLD                    sdpmes
001700040615     C                   KFLD                    comctr
001800040615     C                   KFLD                    comsvr
001900040615     C                   KFLD                    comftc
002000020624      *
002100020313     C/EXEC SQL
002200040615     C+ DECLARE A1 CURSOR FOR SELECT TASKSC, TASAAS,
002300040615     C+ substr(digits(tasmgs), 1, 2), TASCTR, count(*) FROM titas10f WHERE
002400040615     C+ tasaas = 2004 and tastbl = 'A3' and tascbo = 'FO' and (tassv1=
002500040615     C+ 'O' or tassv2 = 'O' or tassv3= 'O') GROUP BY TASKSC, TASAAS,
002600040615     C+ substr(digits(TASMGS), 1, 2), TASCTR
002700020313     C/END-EXEC
002800030109      *
002900030109     C/EXEC SQL
003000030109     C+ OPEN A1
003100030109     C/END-EXEC
003200030109      *
003300040615     C                   DOU       SqlCod <> 0
003400030109     C/EXEC SQL
003500040615     C+ FETCH NEXT FROM A1 INTO :titas
003600030109     C/END-EXEC
003700040615     c                   select
003800040615     C                   WHEN      SqlCod = 0
003900040615     c                   move      commes        sdpmes
004000040615     c     keysdp        chain     sisdp01l
004100040615     c                   if        %found(sisdp01l) and comnsp < sdpnsp
004200040615     c                   sub       comnsp        sdpnsp
004300040615     c                   update    sisdp000
004400040615     c                   end
004500040615     C                   WHEN      SqlCod = 100
004600040615     C                   WHEN      SqlCod <> 0
004700040615     C                   ENDSL
004800040615     C                   ENDDO
004900020624      *
005000030109     C/EXEC SQL
005100030109     C+ CLOSE A1
005200030109     C/END-EXEC
005300030109     c                   seton                                        lr
005400011129     C*--------------------------------------------------------------------
