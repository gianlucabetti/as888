000100000000     H Option( *SrcStmt )
000200170314     H DFTACTGRP(*NO) ACTGRP('UBTEST')
000300000000     **-- Parameters:  -------------------------------------------------------**
000400090610
000500090521
000600170314     D pInValore       S             30A
000700170314     D pInBase         S              2A
000800170314     D pOutConv        S             30A
000900170314     D pOutErrore      S              1S 0
001000170314
001100090520
001200090520     C*
001300161026     C     *entry        plist
001400170314     C                   parm                    pInValore
001500170314     C                   parm                    pInBase
001600170314     C                   parm                    pOutConv
001700170314     C                   parm                    pOutErrore
001800090610
001900090610      /FREE
002000170314
002100161024
002200170314        // Inizializzo parametri di output
002300170314        clear pOutConv;
002400170314
002500161026
002600170314        // Inizializzo l'esito chiamata ad errore
002700170314        pOutErrore = -1;
002800170314
002900170314
003000170314        // Encoda il valore input in Base16
003100170314     C                   eval      pInDec   = %dec(pInValore:30:0)
003200170314     C                   eval      pInBaseX = %dec(pInBase:2:0)
003300170314     C                   call      'UBDEC2HEXR'
003400170314     C                   parm                    pInDec           30 0
003500170314     C                   parm                    pInBaseX          2 0
003600170314     C                   parm                    pOutConv
003700170314
003800170314
003900170314        // Visualizzo il testo encodato in Base16
004000170314     C     pOutConv      dsply
004100170314
004200170314
004300170314        // Uscita
004400170314        *inlr = *on;
004500090710
004600090610
004700090610      /END-FREE
004800161025
