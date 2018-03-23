000100070223     H dftactgrp(*no) actgrp(*caller)
000200020712     ***********************************************************************
000300020712     ** Dopo aver creato il programma, creare la User Defined Function
000400020712     ** con la seguente istruzione SQL:
000500020712     **
000600030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000700061221     **  (COLLI_BOL      DECIMAL(5, 0)
000800061221     ** , COLLI_RIL_PES  DECIMAL(5, 0)
000900061221     ** , PESO_RIL       DECIMAL(7, 1)
001000061221     ** , PESO_FAT       DECIMAL(7, 1))
001100061221     ** RETURNS DECIMAL(7, 1)
001200020712     ** RETURNS NULL ON NULL INPUT
001300020712     ** LANGUAGE RPGLE
001400020712     ** NO SQL
001500020712     ** PARAMETER STYLE SQL
001600030702     **
001700030702     ** Per rimuovere un UDF usare la seguente istruzione:
001800030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
001900020712     ***********************************************************************
002000020712
002100030701     D*------------------
002200030701     D* VARIABILI DI WRK
002300030701     D*------------------
002400061221     D InColliBol      S              5  0
002500061221     D InColliRilPes   S              5  0
002600061221     D InPesoRil       S              7  1
002700061221     D InPesoFat       S              7  1
002800061221     D OutPeso         S              7  1
002900061221     D NullIn_CB       S              5I 0
003000061221     D NullIn_CRP      S              5I 0
003100061221     D NullIn_PR       S              5I 0
003200061221     D NullIn_PF       S              5I 0
003300061221     D NullOut_P       S              5I 0
003400030701     D SQLState        S              5
003500020705     D FunctionName    S            139
003600020705     D SpecificName    S            128
003700020705     D MsgText         S             70
003800020705     D                                     VARYING
003900020705
004000020705     C     *ENTRY        PLIST
004100020705     ** Parametri di input.
004200061221     C                   PARM                    InColliBol
004300061221     C                   PARM                    InColliRilPes
004400061221     C                   PARM                    InPesoRil
004500061221     C                   PARM                    InPesoFat
004600020705     ** Parametri di output.
004700061221     C                   PARM                    OutPeso
004800020705     ** Parametri di indicatore nullo.
004900061221     C                   PARM                    NullIn_CB
005000061221     C                   PARM                    NullIn_CRP
005100061221     C                   PARM                    NullIn_PR
005200061221     C                   PARM                    NullIn_PF
005300061221     C                   PARM                    NullOut_P
005400020705     ** Parametri di funzione SQL.
005500020705     C                   PARM                    SQLState
005600020705     C                   PARM                    FunctionName
005700020705     C                   PARM                    SpecificName
005800020705     C                   PARM                    MsgText
005900020705
006000061221     C                   CLEAR                   NullOut_P
006100020705     C                   EVAL      SQLState = *ZERO
006200020705
006300030701     C*
006400070223     C* CHIAMO IL *PGM CHE EFFETTUA LE CONSIDERAZIONI
006500070223     C                   CALL      'RTVPESR'
006600070223     C                   PARM                    InColliBol
006700070223     C                   PARM                    InColliRilPes
006800070223     C                   PARM                    InPesoRil
006900070223     C                   PARM                    InPesoFat
007000070223     C                   PARM                    OutPeso
007100030701     C*
007200030701     C* VERIFICO I PARAMETRI DI RITORNO
007300120426     C***                IF        OutPeso   = *zeros
007400120426     C***                EVAL      NullOut_P = -1
007500120426     C***                EVAL      SQLState = '38I01'
007600120426     C***                EVAL      MsgText = 'Errore funzione RTVPES'
007700120426     C***                ENDIF
007800061221     C*
007900020705     C                   RETURN
