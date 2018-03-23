000100000000     H Option( *SrcStmt )
000200000000     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400000000
000500101004     D/COPY GAITRASRC/SRCPROTOPI,UBBVSQL01
000600101004     D/COPY GAITRASRC/SRCPROTOPR,UBBVSQL01
000700000000     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET
000800000000     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET
000900000000
001000000000     D ProcessoAlfa    S              5
001100000000     D Processo        S              5  0
001200000000
001300000000
001400000000
001500000000     C     *ENTRY        PLIST
001600000000     C                   PARM                    ProcessoAlfa
001700000000     C*
001800000000     C                   move(p)   ProcessoAlfa  Processo
001900000000     C*
002000000000
002100000000      /FREE
002200000000
002300000000
002400101004         if UBBVSQL01_Connect('BARTVAS' : pOutConn) = 0;
002500000000
002600000000              pInConn = pOutConn;
002700000000
002800000000              pInCodCli  = '01020001';
002900000000              pInTipFile = 'VB';
003000101004
003100101004
003200101004              if UBBVSQL01_RtvInfoJob(pInConn : pInCodCli : pInTipFile : 1 :
003300101004                 pOutProcesso : pOutNumJob : pOutTipTrasf : pOutStsJob ) = 0;
003400101004              endif;
003500101004
003600101004
003700000000
003800101004              if UBBVSQL01_ChkUtente(pInConn : 'TEST001' : pOutNumRec) = 0;
003900000000                 if pOutNumRec = 0;
004000101004                    if UBBVSQL01_InsUtente(
004100000000                                   pInConn
004200000000                                  : 'TEST001'
004300000000                                  : '0TEST001'
004400000000                                  : ' '
004500000000                                  : '\\BartVAS2\BartVAS2\BartFTP\TEST001'
004600000000                                  : 'TEST EDPFG'
004700000000                                  : '0'
004800000000                                  : '1'
004900000000                                  : '0'
005000000000                                  : 'EDPFG') = 0;
005100000000                        dsply 'HURRA !!!';
005200000000                    endif;
005300000000                 endif;
005400000000              endif;
005500000000
005600000000
005700101004              if UBBVSQL01_ChkCliTipFile(pInConn : pInCodCli : pInTipFile :
005800000000                                         pOutNumRec) = 0;
005900000000                 if pOutNumRec > 0;
006000000000                    dsply pOutNumRec;
006100000000                 endif;
006200000000              endif;
006300000000
006400101004              if UBBVSQL01_RtvProcessAtr(pInConn : pInProcesso :
006500000000                       pOutIpAddress : pOutPorta) = 0;
006600000000
006700101004                 pInHost     = pOutIpAddress;
006800000000                 pInPort     = pOutPorta;
006900000000                 pInSktCmd   = '<root from="*">'+
007000000000                               '<m cmd="SETSTATUS" stato="P"/>' +
007100000000                               '</root>';
007200000000
007300101004                 if UBSOCKET_Create(pOutSktDescr) = 0;
007400000000                 endif;
007500000000
007600000000
007700101004                 if UBSOCKET_Connect(pOutSktDescr : pInHost : pInPort) = 0;
007800000000                 endif;
007900000000
008000000000
008100000000                 if UBSOCKET_Send(pOutSktDescr : pInSktCmd : 'QTCPASC') = 0;
008200000000                 endif;
008300000000
008400000000
008500101004                 if UBSOCKET_Garbage(pOutSktDescr) = 0;
008600000000                 endif;
008700000000              endif;
008800000000
008900000000              pInConn = pOutConn;
009000000000
009100101004              if UBBVSQL01_Disconnect(pInConn) = 0;
009200000000              endif;
009300000000         endif;
009400000000
009500000000
009600000000      /END-FREE
009700000000     **
009800000000     C                   Seton                                        LR
009900000000     **
