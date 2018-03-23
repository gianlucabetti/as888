000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500090710     D/COPY GAITRASRC/SRCPROTOPI,UBSOCKET
000600090710     D/COPY GAITRASRC/SRCPROTOPR,UBSOCKET
000700090521
000800090520
000900090520     C*
001000090610
001100090610      /FREE
001200101001         pInHost     = '10.46.250.161';
001300101001         pInPort     = 40021;
001400151008         pInSktCmd   = '<m cmd="RELOAD"/>';
001500151008         // pInSktCmd   = '<root from="*">'+
001600151008         //              '<m cmd="SETSTATUS" stato="P"/>' +
001700151008         //              '</root>';
001800090610
001900090710
002000100922         if UBSOCKET_Create(pOutSktDescr) = 0;
002100090710         endif;
002200090610
002300090710
002400100922         if UBSOCKET_Connect(pOutSktDescr : pInHost : pInPort) = 0;
002500090710         endif;
002600090710
002700090710
002800090713         if UBSOCKET_Send(pOutSktDescr : pInSktCmd : 'QTCPASC') = 0;
002900090710         endif;
003000090710
003100090710
003200100922         if UBSOCKET_Garbage(pOutSktDescr) = 0;
003300090710         endif;
003400090710
003500090610
003600090610      /END-FREE
003700000000     **
003800090520     C                   Seton                                        LR
003900000000     **
