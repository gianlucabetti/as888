000100070223     H dftactgrp(*no) actgrp(*caller)
000200030701     D*------------------
000300030701     D* VARIABILI DI WRK
000400030701     D*------------------
000500061221     D InColliBol      S              5  0
000600061221     D InColliRilPes   S              5  0
000700061221     D InPesoRil       S              7  1
000800061221     D InPesoFat       S              7  1
000900061221     D OutPeso         S              7  1
001000020705
001100020705     C     *ENTRY        PLIST
001200070223     ** Parametri di input.
001300061221     C                   PARM                    InColliBol
001400061221     C                   PARM                    InColliRilPes
001500061221     C                   PARM                    InPesoRil
001600061221     C                   PARM                    InPesoFat
001700020705     ** Parametri di output.
001800061221     C                   PARM                    OutPeso
001900030701     C*
002000061221     C* STABILISCO PESO DA CONSIDERARE
002100061221     C*
002200061221     C* - PESO   => VERIFICO
002300061221     C                   IF        InColliRilPes = InColliBol
002400061221     C                   EVAL      OutPeso = InPesoRil
002500061221     C                   ELSE
002600061221     C                   IF        InPesoRil >= InPesoFat
002700061221     C                   EVAL      OutPeso = InPesoRil
002800061221     C                   ELSE
002900061221     C                   EVAL      OutPeso = InPesoFat
003000061221     C                   ENDIF
003100061221     C                   ENDIF
003200061221     C*
003300070223     C                   SETON                                        LR
