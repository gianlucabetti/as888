000100020712     ***********************************************************************
000200020712     ** Dopo aver creato il programma, creare la User Defined Function
000300020712     ** con la seguente istruzione SQL:
000400020712     **
000500030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000600061221     **  (COLLI_BOL      DECIMAL(5, 0)
000700061221     ** , COLLI_RIL_VOL  DECIMAL(5, 0)
000800061221     ** , VOL_FLG        CHAR(1 )
000900061221     ** , VOL_RIL        DECIMAL(5, 3)
001000061221     ** , VOL_FAT        DECIMAL(5, 3))
001100061221     ** RETURNS DECIMAL(5, 3)
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
002500061221     D InColliRilVol   S              5  0
002600061221     D InVolFlg        S              1
002700061221     D InVolRil        S              5  3
002800061221     D InVolFat        S              5  3
002900061221     D OutVolume       S              5  3
003000061221     D NullIn_CB       S              5I 0
003100061221     D NullIn_CRV      S              5I 0
003200061221     D NullIn_VFLG     S              5I 0
003300061221     D NullIn_VR       S              5I 0
003400061221     D NullIn_VF       S              5I 0
003500061221     D NullOut_V       S              5I 0
003600030701     D SQLState        S              5
003700020705     D FunctionName    S            139
003800020705     D SpecificName    S            128
003900020705     D MsgText         S             70
004000020705     D                                     VARYING
004100020705
004200020705     C     *ENTRY        PLIST
004300020705     ** Parametri di input.
004400061221     C                   PARM                    InColliBol
004500061221     C                   PARM                    InColliRilVol
004600061221     C                   PARM                    InVolFlg
004700061221     C                   PARM                    InVolRil
004800061221     C                   PARM                    InVolFat
004900020705     ** Parametri di output.
005000061221     C                   PARM                    OutVolume
005100020705     ** Parametri di indicatore nullo.
005200061221     C                   PARM                    NullIn_CB
005300061221     C                   PARM                    NullIn_CRV
005400061221     C                   PARM                    NullIn_VFLG
005500061221     C                   PARM                    NullIn_VR
005600061221     C                   PARM                    NullIn_VF
005700061221     C                   PARM                    NullOut_V
005800020705     ** Parametri di funzione SQL.
005900020705     C                   PARM                    SQLState
006000020705     C                   PARM                    FunctionName
006100020705     C                   PARM                    SpecificName
006200020705     C                   PARM                    MsgText
006300020705
006400061221     C                   CLEAR                   NullOut_V
006500020705     C                   EVAL      SQLState = *ZERO
006600020705
006700030701     C*
006800061221     C* STABILISCO VOLUME DA CONSIDERARE
006900061221     C*
007000061221     C* - VOLUME => VERIFICO
007100061221     C                   IF        InVolFlg = 'T'
007200061221     C                   EVAL      OutVolume = InVolFat
007300061221     C                   ELSE
007400061221     C                   IF        InColliRilVol = InColliBol
007500061221     C                   EVAL      OutVolume = InVolRil
007600061221     C                   ELSE
007700061221     C                   IF        InVolRil >= InVolFat
007800061221     C                   EVAL      OutVolume = InVolRil
007900061221     C                   ELSE
008000061221     C                   EVAL      OutVolume = InVolFat
008100061221     C                   ENDIF
008200061221     C                   ENDIF
008300061221     C                   ENDIF
008400030701     C*
008500030701     C* VERIFICO I PARAMETRI DI RITORNO
008600061221     C                   IF        OutVolume = *zeros
008700061221     C                   EVAL      NullOut_V = -1
008800030701     C                   EVAL      SQLState = '38I01'
008900061221     C                   EVAL      MsgText = 'Errore funzione RTVVOL'
009000030701     C                   ENDIF
009100061221     C*
009200020705     C                   RETURN
