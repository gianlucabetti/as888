000100031010      /TITLE UDF Converte data da AAAAMMGG a *ISO.
000200020712
000300031010     ** CREATE FUNCTION GAITRAOBJ/CVTD80ISO
000400031010     ** (DATA80 DECIMAL(8, 0))
000500020712     ** RETURNS DATE
000600020712     ** RETURNS NULL ON NULL INPUT
000700020712     ** LANGUAGE RPGLE
000800020712     ** NO SQL
000900020712     ** PARAMETER STYLE SQL
001000020712
001100031010     D Data80          S              8  0
001200020705     D DataISO         S               D
001300031010     D INData80        S              5I 0
001400020705     D INDataISO       S              5I 0
001500020705     D SQLState        S              5
001600020705     D FunctionName    S            139
001700020705     D SpecificName    S            128
001800020705     D MsgText         S             70
001900020705     D                                     VARYING
002000020705
002100020705     C     *ENTRY        PLIST
002200020705     ** Parametri di input.
002300031010     C                   PARM                    Data80
002400020705     ** Parametri di output.
002500020705     C                   PARM                    DataISO
002600020705     ** Parametri di indicatore nullo.
002700031010     C                   PARM                    INData80
002800020705     C                   PARM                    INDataISO
002900020705     ** Parametri di funzione SQL.
003000020705     C                   PARM                    SQLState
003100020705     C                   PARM                    FunctionName
003200020705     C                   PARM                    SpecificName
003300020705     C                   PARM                    MsgText
003400031110
003500031110     C                   RESET                   INDataISO
003600031110     C                   EVAL      SQLState = *ZERO
003700031110
003800031110     C                   IF        INData80 = 0
003900031110     C
004000031110     C     *YMD          TEST(DE)                Data80
004100031110     C
004200031110     C                   SELECT
004300031110     C                   WHEN      Data80 = *HIVAL
004400031110     C                   EVAL      DataISO = *HIVAL
004500031110     C                   WHEN      Data80 <= 0
004600031110     C                   EVAL      DataISO = *LOVAL
004700031110     C                   WHEN      %ERROR
004800031110     C                   EVAL      INDataISO = -1
004900031110     C                   OTHER
005000031010     C                   MOVE      Data80        DataISO
005100031110     C                   ENDSL
005200031110     C
005300031110     C                   ELSE
005400031110     C
005500031110     C                   EVAL      INDataISO = -1
005600031110     C
005700031110     C                   ENDIF
005800020705
005900020705     C                   RETURN
