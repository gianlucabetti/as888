000100070223     H dftactgrp(*no) actgrp(*caller)
000200020712     ***********************************************************************
000300020712     ** Dopo aver creato il programma, creare la User Defined Function
000400020712     ** con la seguente istruzione SQL:
000500020712     **
000600030701     ** CREATE FUNCTION [Libreria]/[Nome *PGM]
000700061221     **  (COLLI_BOL      DECIMAL(5, 0)
000800061221     ** , COLLI_RIL_VOL  DECIMAL(5, 0)
000900061221     ** , VOL_FLG        CHAR(1 )
001000061221     ** , VOL_RIL        DECIMAL(5, 3)
001100061221     ** , VOL_FAT        DECIMAL(5, 3))
001200061221     ** RETURNS DECIMAL(5, 3)
001300020712     ** RETURNS NULL ON NULL INPUT
001400020712     ** LANGUAGE RPGLE
001500020712     ** NO SQL
001600020712     ** PARAMETER STYLE SQL
001700030702     **
001800030702     ** Per rimuovere un UDF usare la seguente istruzione:
001900030702     ** DROP FUNCTION [Libreria]/[Nome *PGM]
002000020712     ***********************************************************************
002100020712
002200030701     D*------------------
002300030701     D* VARIABILI DI WRK
002400030701     D*------------------
002500061221     D InColliBol      S              5  0
002600061221     D InColliRilVol   S              5  0
002700061221     D InVolFlg        S              1
002800061221     D InVolRil        S              5  3
002900061221     D InVolFat        S              5  3
003000061221     D OutVolume       S              5  3
003100061221     D NullIn_CB       S              5I 0
003200061221     D NullIn_CRV      S              5I 0
003300061221     D NullIn_VFLG     S              5I 0
003400061221     D NullIn_VR       S              5I 0
003500061221     D NullIn_VF       S              5I 0
003600061221     D NullOut_V       S              5I 0
003700030701     D SQLState        S              5
003800020705     D FunctionName    S            139
003900020705     D SpecificName    S            128
004000020705     D MsgText         S             70
004100020705     D                                     VARYING
004200020705
004300020705     C     *ENTRY        PLIST
004400020705     ** Parametri di input.
004500061221     C                   PARM                    InColliBol
004600061221     C                   PARM                    InColliRilVol
004700061221     C                   PARM                    InVolFlg
004800061221     C                   PARM                    InVolRil
004900061221     C                   PARM                    InVolFat
005000020705     ** Parametri di output.
005100061221     C                   PARM                    OutVolume
005200020705     ** Parametri di indicatore nullo.
005300061221     C                   PARM                    NullIn_CB
005400061221     C                   PARM                    NullIn_CRV
005500061221     C                   PARM                    NullIn_VFLG
005600061221     C                   PARM                    NullIn_VR
005700061221     C                   PARM                    NullIn_VF
005800061221     C                   PARM                    NullOut_V
005900020705     ** Parametri di funzione SQL.
006000020705     C                   PARM                    SQLState
006100020705     C                   PARM                    FunctionName
006200020705     C                   PARM                    SpecificName
006300020705     C                   PARM                    MsgText
006400020705
006500061221     C                   CLEAR                   NullOut_V
006600020705     C                   EVAL      SQLState = *ZERO
006700020705
006800070223     C*
006900070223     C* CHIAMO IL *PGM CHE EFFETTUA LE CONSIDERAZIONI
007000070223     C                   CALL      'RTVVOLR'
007100070223     C                   PARM                    InColliBol
007200070223     C                   PARM                    InColliRilVol
007300070223     C                   PARM                    InVolFlg
007400070223     C                   PARM                    InVolRil
007500070223     C                   PARM                    InVolFat
007600070223     C                   PARM                    OutVolume
007700030701     C*
007800030701     C* VERIFICO I PARAMETRI DI RITORNO
007900120426     C***                IF        OutVolume = *zeros
008000120426     C***                EVAL      NullOut_V = -1
008100120426     C***                EVAL      SQLState = '38I01'
008200120426     C***                EVAL      MsgText = 'Errore funzione RTVVOL'
008300120426     C***                ENDIF
008400061221     C*
008500020705     C                   RETURN
