002300030701     D*------------------
002400030701     D* VARIABILI DI WRK
002500030701     D*------------------
002600070126     D InTipAnag       S              1
002700070126     D InCodAutotr     S              7  0
002800070126     D InFlgPrestaz    S              1
002900070129     D InFgsGiro       S              3  0
003000070126     D InCodGiro       S             10
003100070223     D InDataFvv       S              8  0
003101070223     D InNumFvv        S              6  0
003102070313     D InTipPig        S              1  0
003200070126     D OutPriorit      S              2  0
003300070126     D NullIn_TANA     S              5I 0
003400070126     D NullIn_FLGP     S              5I 0
003500070126     D NullIn_CAUT     S              5I 0
003600070129     D NullIn_FGSG     S              5I 0
003700070126     D NullIn_CCGI     S              5I 0
003800070126     D NullIn_DATA     S              5I 0
003801070223     D NullIn_NFVV     S              5I 0
003802070223     D NullIn_TPIG     S              5I 0
003900070126     D NullOut_PRIO    S              5I 0
004000070126     D wSQLState       S              5
004100020705     D FunctionName    S            139
004200020705     D SpecificName    S            128
004300020705     D MsgText         S             70
004400020705     D                                     VARYING
004500020705
004600070223     C                   CALL      'GETCGIPTY'
004700020705     ** Parametri di input.
004800070223     C                   PARM      'A'           InTipAnag
004900070223     C                   PARM      10435         InCodAutotr
005000070223     C                   PARM      'M'           InFlgPrestaz
005100070223     C                   PARM      001           InFgsGiro
005200070223     C                   PARM      '00011'       InCodGiro
005300070223     C                   PARM      20070201      InDataFvv
005301070223     C                   PARM      0             InNumFvv
005302070313     C                   PARM      1             InTipPig
005400020705     ** Parametri di output.
005500070126     C                   PARM                    OutPriorit
005600020705     ** Parametri di indicatore nullo.
005700070126     C                   PARM                    NullIn_TANA
005800070126     C                   PARM                    NullIn_FLGP
005900070126     C                   PARM                    NullIn_CAUT
006000070129     C                   PARM                    NullIn_FGSG
006100070129     C                   PARM                    NullIn_CCGI
006200070126     C                   PARM                    NullIn_DATA
006201070223     C                   PARM                    NullIn_NFVV
006202070223     C                   PARM                    NullIn_TPIG
006300070126     C                   PARM                    NullOut_PRIO
006400020705     ** Parametri di funzione SQL.
006500070126     C                   PARM                    wSQLState
006600020705     C                   PARM                    FunctionName
006700020705     C                   PARM                    SpecificName
006800020705     C                   PARM                    MsgText
011300061221     C*
011400070223     C                   SETON                                        LR
