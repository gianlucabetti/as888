000100090923     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO) BNDDIR(PJXBND)
000200060824     H*ACTGRP(QILE)
000300090923      **********************************************************************
000400060824     H DECEDIT('0,') DATEDIT(*DMY.)
000500060824      **********************************************************************
000600060824      *
000700060824      **********************************************************************
000800060824      *====================================================*
000900060824      * PARAMETRI DI INGRESSO
001000060824      *----------*
001100090923      * Dati di ambiente
001200090923     D SDGTAB        E DS                  EXTNAME(SDGTABDS)
001300090923     D*-------------
001400060824      *====================================================*
001500090923      *
001600060824      *====================================================*
001700060824      *----------*
001800090923     D WInterSW        S              1
001900060824      ******************************************************
002000060824      *                MAIN LINE PROGRAM
002100060824      ******************************************************
002200060824      *
002300060824      * inizializzazione variabili
002400090923     C                   Exsr      InzVar
002500060824      *
002600060824      * chiamata pgm specifico
002700090923   B1C                   Select
002800090923     C*
002900090923    >C                   WHEN      WInterSW   = '1'
003000090923     C                   Exsr      CallPgmSW1
003100060824      *
003200090923    >C                   WHEN      WInterSW   = '2'
003300090923     C                   Exsr      CallPgmSW2
003400090923      *
003500090923    >C                   WHEN      WInterSW   = '3'
003600090923     C                   Exsr      CallPgmSW3
003700090923      *
003800090923     C                   EndSl
003900090923      *
004000060824      * fine pgm
004100090923     C                   Exsr      EndPgm
004200060824      ************************************************************
004300060824      * Operazioni iniziali
004400060824      ************************************************************
004500060824     C     *Inzsr        Begsr
004600060824      *
004700060824      * Reperimento parametri
004800090923     C     *Entry        Plist
004900090923     C                   Parm                    SDGTAB
005000060824      *
005100060824     C     XInzSr        Endsr
005200060824      ************************************************************
005300060824      * Inizializzazione variabili
005400060824      ************************************************************
005500060824     C     InzVar        Begsr
005600060824      *
005700060824      * reperisco da ANMPE parametro da passare
005800090923     C                   Exsr      RieParamIn
005900060824      *
006000060824     C     XInzVar       Endsr
006100060824      ******************************************************
006200090923      * Reperisco da SDGPAR il tipo di ERP
006300060824      ******************************************************
006400060824     C     RieParamIn    Begsr
006500060824      *
006600090923     C                   Eval      WInterSW  = '1'
006700090923      *
006800060824     C     XRieParamIn   Endsr
006900060824      ************************************************************
007000090923      * Chiamo programma specifico per collegamento ERP
007100060824      ************************************************************
007200090923     C     CallPgmSW1    Begsr
007300060824      *
007400090923     C                   CALLB     'XATB'
007500090923     C                   PARM                    SDGTAB
007600060824      *
007700090923     C     XCallPgmSW1   Endsr
007800090923      ************************************************************
007900090923      * Chiamo programma specifico per collegamento ERP
008000090923      ************************************************************
008100090923     C     CallPgmSW2    Begsr
008200090923      *
008300090923      *
008400090923     C     XCallPgmSW2   Endsr
008500090923      ************************************************************
008600090923      * Chiamo programma specifico per collegamento ERP
008700090923      ************************************************************
008800090923     C     CallPgmSW3    Begsr
008900090923      *
009000090923      *
009100090923     C     XCallPgmSW3   Endsr
009200060824      ************************************************************
009300060824      * FINE PROGRAMMA
009400060824      ************************************************************
009500060824     C     EndPgm        Begsr
009600060824      *
009700090923     C                   Move      *on           *inRT
009800060824     C                   RETURN
009900060824      *
010000060824     C     XEndPgm       Endsr
010100060824      ************************************************************
010200060824      * Definizione Klist
010300060824      ************************************************************
010400060824     C     DefKList      Begsr
010500060824      *
010600060824      *
010700060824     C     XDefKlist     Endsr
