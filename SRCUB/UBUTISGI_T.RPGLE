000100000000     H Option( *SrcStmt )
000200090610     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500090610     D/COPY GAITRASRC/SRCPROTOPI,UBUTISGI
000600090610     D/COPY GAITRASRC/SRCPROTOPR,UBUTISGI
000700090521
000800090610     D a               s              1    inz
000900090520
001000090520     C*
001100090610
001200090610      /FREE
001300090610         pInSUN = '000000003';
001400090610         pInKSU = '00010076';
001500090611         pInRGR = 'S';
001600090610
001700090610
001800090610         if UBUTISGI_RtvSrvSts(pInSUN:pInKSU:pInRGR:pOutEsito) = 0;
001900090610            a = a;
002000090610         endif;
002100090610
002200090610
002300090610         if pOutEsito = '0';
002400090610            pInSUN = '000079763';
002500090610            pInKSU = '09999999';
002600090610
002700090610            if UBUTISGI_DisableSgiUsr(pInSUN:pInKSU:pOutEsito) = 0;
002800090610               a = a;
002900090610            endif;
003000090610         endif;
003100090610
003200090610
003300090610         if UBUTISGI_Finalize = 0;
003400090610            a = a;
003500090610         endif;
003600090610
003700090610
003800090610
003900090610      /END-FREE
004000000000     **
004100090520     C                   Seton                                        LR
004200000000     **
