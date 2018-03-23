000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500151008     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET2
000600151008     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET2
000700151014
000800151014     D wMsgSockCli     S          65536    based(pOutRespPtr)
000900151014     D wPause          S              1    inz
001000151014
001100090521
001200090520
001300090520     C*
001400090610
001500090610      /FREE
001600151014         pInHost     = '10.46.251.161';
001700151014         pInPort     = 40021;
001800151014         //pInHost     = 'as888';
001900151014         //pInPort     = 4000;
002000151008         pInSktCmd   = '<m cmd="RELOAD"/>';
002100151008         // pInSktCmd   = '<root from="*">'+
002200151008         //              '<m cmd="SETSTATUS" stato="P"/>' +
002300151008         //              '</root>';
002400090610
002500090710
002600151008         if UBSOCKET2_Create(pOutSktDescr) = 0;
002700090710         endif;
002800090610
002900090710
003000151008         if UBSOCKET2_Connect(pOutSktDescr : pInHost : pInPort) = 0;
003100090710         endif;
003200090710
003300090710
003400151008         if UBSOCKET2_Send(pOutSktDescr : pInSktCmd : 'QTCPASC') = 0;
003500090710         endif;
003600090710
003700151014
003800151014
003900151014         if UBSOCKET2_Receive(pOutSktDescr : 256 : '3' : ' ' :
004000151014                              pOutRespPtr : pOutRespLen) = 0;
004100151014            dsply %subst(wMsgSockCli:1:50) wPause;
004200151014         endif;
004300151014
004400090710
004500151008         if UBSOCKET2_Garbage(pOutSktDescr) = 0;
004600090710         endif;
004700090710
004800090610
004900090610      /END-FREE
005000000000     **
005100090520     C                   Seton                                        LR
005200000000     **
