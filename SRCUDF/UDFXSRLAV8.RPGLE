000100020712     ***********************************************************************
000200020712     ** Dopo aver creato il programma, creare la User Defined Function
000300020712     ** con la seguente istruzione SQL:
000400020712     **
000500030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000600030701     **  (DATADA  DECIMAL(8, 0)
000700030709     ** , DATAA   DECIMAL(8, 0)
000800030709     ** , SUBGG   DECIMAL(1, 0))
000900030701     ** RETURNS DECIMAL(5, 0)
001000020712     ** RETURNS NULL ON NULL INPUT
001100020712     ** LANGUAGE RPGLE
001200020712     ** NO SQL
001300020712     ** PARAMETER STYLE SQL
001400030702     **
001500030702     ** Per rimuovere un UDF usare la seguente istruzione:
001600030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
001700020712     ***********************************************************************
001800020712
001900030701     D*------------------
002000030701     D* DS X PASSAGGIO PARAMETRI TRUL XSRLAV8
002100030701     D*------------------
002200030701     D WDAT8           DS
002300030701     D  DATADA                        8  0
002400030701     D  DATAA                         8  0
002500030701     D  GGLAV                         5  0
002600030701     D*------------------
002700030701     D* VARIABILI DI WRK
002800030701     D*------------------
002900030701     D InDataDa        S              8  0
003000030701     D InDataA         S              8  0
003100030702     D InSubGg         S              1  0
003200030701     D OutGgLav        S              5  0
003300030701     D NullInDataDa    S              5I 0
003400030701     D NullInDataA     S              5I 0
003500030702     D NullInSubGg     S              5I 0
003600030701     D NullOutGgLav    S              5I 0
003700030701     D SQLState        S              5
003800020705     D FunctionName    S            139
003900020705     D SpecificName    S            128
004000020705     D MsgText         S             70
004100020705     D                                     VARYING
004200020705
004300020705     C     *ENTRY        PLIST
004400020705     ** Parametri di input.
004500030701     C                   PARM                    InDataDa
004600030701     C                   PARM                    InDataA
004700030702     C                   PARM                    InSubGg
004800020705     ** Parametri di output.
004900030701     C                   PARM                    OutGgLav
005000020705     ** Parametri di indicatore nullo.
005100030701     C                   PARM                    NullInDataDa
005200030701     C                   PARM                    NullInDataA
005300030702     C                   PARM                    NullInSubGg
005400030701     C                   PARM                    NullOutGgLav
005500020705     ** Parametri di funzione SQL.
005600020705     C                   PARM                    SQLState
005700020705     C                   PARM                    FunctionName
005800020705     C                   PARM                    SpecificName
005900020705     C                   PARM                    MsgText
006000020705
006100030701     C                   CLEAR                   NullOutGgLav
006200020705     C                   EVAL      SQLState = *ZERO
006300020705
006400030701     C*
006500030701     C* VALORIZZO LA DS X IL PASSAGGIO PARAMETRI AL PGM DI CALCOLO GG LAVORATIVI TRA 2 DATE
006600030701     C                   CLEAR                   WDAT8
006700030701     C                   EVAL      DATADA = InDataDa
006800030701     C                   EVAL      DATAA  = InDataA
006900030701     C*
007000030701     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
007100030701     C                   CALL      'XSRLAV8'
007200030701     C                   PARM                    WDAT8
007300030701     C*
007400030701     C* VERIFICO I PARAMETRI DI RITORNO
007500030701     C                   IF        GGLAV < 0
007600030701     C                   EVAL      NullOutGgLav = -1
007700030701     C                   CLEAR                   OutGgLav
007800030701     C                   EVAL      SQLState = '38I01'
007900030701     C                   EVAL      MsgText = 'Errore funzione SUBDATLAV0'
008000030701     C                   ELSE
008100030702     C                   EVAL      GGLAV = GGLAV - InSubGg
008200030702     C                   IF        GGLAV >= 0
008300030701     C                   EVAL      OutGgLav = GGLAV
008400030702     C                   ELSE
008500030702     C                   EVAL      OutGgLav = 0
008600030702     C                   ENDIF
008700030701     C                   ENDIF
008800020705
008900020705     C                   RETURN
