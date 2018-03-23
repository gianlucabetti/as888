000100000414
000200000414     H DECEDIT('0,') DATEDIT(*DMY.)
000300000414
000400030716     D*------------
000500030716     D* COSTANTI X TEST
000600030716     D*------------
000700030717     D IN_Volume       S             30  9 INZ(1064832000,584)
000800030716     D IN_Step         S             30  9 INZ(0,1)
000900030717     D IN_FlgInt       S              1    INZ(*blanks)
001000030717     D OUT_LatoL       S             30  9 INZ(*zeros)
001100030717     D OUT_LatoH       S             30  9 INZ(*zeros)
001200030717     D OUT_LatoW       S             30  9 INZ(*zeros)
001300030716     D OUT_Errore      S              1    INZ(*blanks)
001400030716
001500030716     D*------------
001600030716     D* VARIABILI DI WRK
001700030716     D*------------
001800030716     D wStart          S               Z
001900030716     D wEnd            S               Z
002000030716     D wDur            S             30  0
002100030716
002200030716
002300000414     C*------------------------------------------------------------------------*
002400000414     C* MAIN LINE
002500000414     C*------------------------------------------------------------------------*
002600030716     C*
002700030716     C* Imposto l'ora d inizio elaborazione
002800030716     C                   TIME                    wStart
002900030716     C*
003000030717     C                   CALL      'XDEVOLR'
003100030716     C                   PARM                    IN_Volume
003200030716     C                   PARM                    IN_Step
003300030717     C                   PARM                    IN_FlgInt
003400030717     C                   PARM                    OUT_LatoL
003500030717     C                   PARM                    OUT_LatoH
003600030717     C                   PARM                    OUT_LatoW
003700030716     C                   PARM                    OUT_Errore
003800030716     C*
003900030716     C* Imposto l'ora d fine elaborazione
004000030716     C                   TIME                    wEnd
004100030716     C*
004200030716     C* Calcolo la differenza in millisecondi
004300030716     C     wEnd          SUBDUR    wStart        wDur:*MS
004400030716     C*
004500030716     C* Emetto il risultato del lato calcolato
004600030717     C     OUT_LatoL     DSPLY
004700030717     C     OUT_LatoH     DSPLY
004800030717     C     OUT_LatoW     DSPLY
004900030716     C     wDur          DSPLY
005000000414     C*
005100000414     C                   SETON                                        LR
005200000411
005300000414     C*------------------------------------------------------------------------*
005400000414     C* *INZSR - OPERAZIONI INIZIALI
005500000414     C*------------------------------------------------------------------------*
005600000414     C     *INZSR        BEGSR
005700030716     C*
005800000414     C                   ENDSR
