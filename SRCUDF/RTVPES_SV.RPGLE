000100020712     ***********************************************************************
000200020712     ** Dopo aver creato il programma, creare la User Defined Function
000300020712     ** con la seguente istruzione SQL:
000400020712     **
000500030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000600061221     **  (COLLI_BOL      DECIMAL(5, 0)
000700061221     ** , COLLI_RIL_PES  DECIMAL(5, 0)
000800061221     ** , PESO_RIL       DECIMAL(7, 1)
000900061221     ** , PESO_FAT       DECIMAL(7, 1))
001000061221     ** RETURNS DECIMAL(7, 1)
001100020712     ** RETURNS NULL ON NULL INPUT
001200020712     ** LANGUAGE RPGLE
001300020712     ** NO SQL
001400020712     ** PARAMETER STYLE SQL
001500030702     **
001600030702     ** Per rimuovere un UDF usare la seguente istruzione:
001700030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
001800020712     ***********************************************************************
001900020712
002000030701     D*------------------
002100030701     D* VARIABILI DI WRK
002200030701     D*------------------
002300061221     D InColliBol      S              5  0
002400061221     D InColliRilPes   S              5  0
002500061221     D InPesoRil       S              7  1
002600061221     D InPesoFat       S              7  1
002700061221     D OutPeso         S              7  1
002800061221     D NullIn_CB       S              5I 0
002900061221     D NullIn_CRP      S              5I 0
003000061221     D NullIn_PR       S              5I 0
003100061221     D NullIn_PF       S              5I 0
003200061221     D NullOut_P       S              5I 0
003300030701     D SQLState        S              5
003400020705     D FunctionName    S            139
003500020705     D SpecificName    S            128
003600020705     D MsgText         S             70
003700020705     D                                     VARYING
003800020705
003900020705     C     *ENTRY        PLIST
004000020705     ** Parametri di input.
004100061221     C                   PARM                    InColliBol
004200061221     C                   PARM                    InColliRilPes
004300061221     C                   PARM                    InPesoRil
004400061221     C                   PARM                    InPesoFat
004500020705     ** Parametri di output.
004600061221     C                   PARM                    OutPeso
004700020705     ** Parametri di indicatore nullo.
004800061221     C                   PARM                    NullIn_CB
004900061221     C                   PARM                    NullIn_CRP
005000061221     C                   PARM                    NullIn_PR
005100061221     C                   PARM                    NullIn_PF
005200061221     C                   PARM                    NullOut_P
005300020705     ** Parametri di funzione SQL.
005400020705     C                   PARM                    SQLState
005500020705     C                   PARM                    FunctionName
005600020705     C                   PARM                    SpecificName
005700020705     C                   PARM                    MsgText
005800020705
005900061221     C                   CLEAR                   NullOut_P
006000020705     C                   EVAL      SQLState = *ZERO
006100020705
006200030701     C*
006300061221     C* STABILISCO PESO DA CONSIDERARE
006400061221     C*
006500061221     C* - PESO   => VERIFICO
006600061221     C                   IF        InColliRilPes = InColliBol
006700061221     C                   EVAL      OutPeso = InPesoRil
006800061221     C                   ELSE
006900061221     C                   IF        InPesoRil >= InPesoFat
007000061221     C                   EVAL      OutPeso = InPesoRil
007100061221     C                   ELSE
007200061221     C                   EVAL      OutPeso = InPesoFat
007300061221     C                   ENDIF
007400061221     C                   ENDIF
007500030701     C*
007600030701     C* VERIFICO I PARAMETRI DI RITORNO
007700061221     C                   IF        OutPeso   = *zeros
007800061221     C                   EVAL      NullOut_P = -1
007900030701     C                   EVAL      SQLState = '38I01'
008000061221     C                   EVAL      MsgText = 'Errore funzione RTVPES'
008100030701     C                   ENDIF
008200061221     C*
008300020705     C                   RETURN
