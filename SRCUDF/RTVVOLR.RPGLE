000100070223     H dftactgrp(*no) actgrp(*caller)
000200030701     D*------------------
000300030701     D* VARIABILI DI WRK
000400030701     D*------------------
000500061221     D InColliBol      S              5  0
000600061221     D InColliRilVol   S              5  0
000700061221     D InVolFlg        S              1
000800061221     D InVolRil        S              5  3
000900061221     D InVolFat        S              5  3
001000061221     D OutVolume       S              5  3
001100020705
001200020705     C     *ENTRY        PLIST
001300020705     ** Parametri di input.
001400061221     C                   PARM                    InColliBol
001500061221     C                   PARM                    InColliRilVol
001600061221     C                   PARM                    InVolFlg
001700061221     C                   PARM                    InVolRil
001800061221     C                   PARM                    InVolFat
001900020705     ** Parametri di output.
002000061221     C                   PARM                    OutVolume
002100020705
002200030701     C*
002300061221     C* STABILISCO VOLUME DA CONSIDERARE
002400061221     C*
002500061221     C* - VOLUME => VERIFICO
002600061221     C                   IF        InVolFlg = 'T'
002700061221     C                   EVAL      OutVolume = InVolFat
002800061221     C                   ELSE
002900061221     C                   IF        InColliRilVol = InColliBol
003000061221     C                   EVAL      OutVolume = InVolRil
003100061221     C                   ELSE
003200061221     C                   IF        InVolRil >= InVolFat
003300061221     C                   EVAL      OutVolume = InVolRil
003400061221     C                   ELSE
003500061221     C                   EVAL      OutVolume = InVolFat
003600061221     C                   ENDIF
003700061221     C                   ENDIF
003800061221     C                   ENDIF
003900061221     C*
004000070223     C                   SETON                                        LR
