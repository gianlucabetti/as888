000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020909
000300020909      * -------------------------------------------------------------*
000400030922      * Crea numeratore ORM da FLNUF a AZNUM
000500030922      * Fisso 300 - 301
000600020114      *--------------------------------------------------------------*
000700020909
000800020909     fFlnuf01l  if   e           k Disk                                         Numeratore vecchio
000900020909     fAznum01L  uf a e           k Disk                                         Numeratore nuovo
001000020909     fQsysprt   o    f  198        Printer Oflind(*InOf)
001100020909
001200030922     d xx              s              3  0
001300020909
001400020909     d Ut§dse0f      e ds
001500020909     d Cncr80        e ds
001600030922     d Tibs56Ds      e ds
001700030922     d  Ski                    7    756  0 Dim(250)
001800020909     d Trul33Ds      e ds
001900020911     d Og143         e ds
002000020909     d Kpjba         e ds
002100020909
002200020909      *Klist
002300020909
002400020909     c     Knuf          Klist
002500020909     c                   Kfld                    NufAaa
002600020909     c                   Kfld                    NufCnu
002700030922     c                   Kfld                    NufFil
002800020909
002900020909     c                   Z-Add     1             Codut
003000020909     c                   Call      'X§PARUT'
003100020909     c                   Parm                    Ut§dse0f
003200020909     c                   Movel     Rec80         Cncr80
003300020909
003400020909     c                   Except    Testa
003500020909
003600030922      * Carico tutti i p.o. presenti sullo stesso S.I.
003700030922     c                   Clear                   Tibs56Ds
003800030922     c                   Eval      I56Ppo = Simfel
003900030922     c                   Eval      I56Mod = 'POS'
004000030922     c                   Call      'TIBS56R'
004100030922     c                   Parm                    Tibs56Ds
004200020910
004300030922      *-----------------------------
004400030922      * N U M E R A T O R E   3 0 0
004500030922      *-----------------------------
004600020910      * Cerco il vecchio numero
004700020910     c                   Clear                   NufAaa
004800030922     c                   Z-Add     300           NufCnu
004900030922      * Devo creare un numeratore per ogni p.o. della sk
005000030922Do  1c                   Do        250           xx
005100030922If  2c                   If        Ski(xx) = *zeros
005200030922     c                   Leave
005300030922    2c                   EndIf
005400030922     c                   Eval      NufFil = Ski(xx)
005500020910     c     KNuf          Chain     Flnuf01l
005600030922If  2c                   If        %Found(Flnuf01l)
005700020909     c                   Clear                   Trul33Ds
005800020909     c                   Eval      I33Ope = 2
005900020910     c                   Eval      I33Cnu = NufCnu
006000030922     c                   Eval      I33Po1 = NufFil
006100020909     c                   Eval      I33Unp = NufNum
006200020909     c                   Movel(p)  Trul33Ds      Kpjbu
006300020909     c                   Call      'TRUL33R'
006400020909     c                   Parm                    Kpjba
006500020909     c                   Movel     Kpjbu         Trul33Ds
006600020909If  3c                   If        O33Err <> *Zeros
006700020909     c                   Except    RigaErr
006800020909  x 3c                   Else
006900020909     c                   Except    RigaOk
007000020909    3c                   EndIf
007100030922    2c                   EndIf
007200030922    1c                   EndDo
007300020909
007400030922      *-----------------------------
007500030922      * N U M E R A T O R E   3 0 1
007600030922      *-----------------------------
007700030922      * Cerco il vecchio numero
007800030922     c                   Clear                   NufAaa
007900030922     c                   Z-Add     301           NufCnu
008000030922      * Devo creare un numeratore per ogni p.o. della sk
008100030922Do  1c                   Do        250           xx
008200030922If  2c                   If        Ski(xx) = *zeros
008300030922     c                   Leave
008400030922    2c                   EndIf
008500030922     c                   Eval      NufFil = Ski(xx)
008600030922     c     KNuf          Chain     Flnuf01l
008700030922If  2c                   If        %Found(Flnuf01l)
008800030922     c                   Clear                   Trul33Ds
008900030922     c                   Eval      I33Ope = 2
009000030922     c                   Eval      I33Cnu = NufCnu
009100030922     c                   Eval      I33Po1 = NufFil
009200030922     c                   Eval      I33Unp = NufNum
009300030922     c                   Movel(p)  Trul33Ds      Kpjbu
009400030922     c                   Call      'TRUL33R'
009500030922     c                   Parm                    Kpjba
009600030922     c                   Movel     Kpjbu         Trul33Ds
009700030922If  3c                   If        O33Err <> *Zeros
009800030922     c                   Except    RigaErr
009900030922  x 3c                   Else
010000030922     c                   Except    RigaOk
010100030922    3c                   EndIf
010200030922    2c                   EndIf
010300030922    1c                   EndDo
010400030922
010500020909     c                   Clear                   Trul33Ds
010600020909     c                   Eval      I33Tla = 'C'
010700020909     c                   Movel(p)  Trul33Ds      Kpjbu
010800020909     c                   Call      'TRUL33R'
010900020909     c                   Parm                    Kpjba
011000020909
011100020909     c                   Eval      *InLr = *On
011200020909
011300020909     oQsysprt   e            Testa          2 02
011400020909     o                                         +  2 'Creazione nuovi'
011500030922     o                                         +  1 'Numeratori ORM'
011600020909     o                                         +  2 'Nel p.o.'
011700020909     o                       Simfel            +  1
011800030922     o                                         +  5 'TNVRNORM'
011900020909     o                       Udate             +  2 '  /  /  '
012000020909     o                                         +  5 'Pag.'
012100020909     o                       Page          Z
012200020909     o          e            Testa          2
012300020909     o                                         +  5 'Punto Operativo'
012400020909     o                                         +  3 'Numeratore'
012500020909     o                                         +  3 'Numero Vecchio'
012600020909     o                                         +  3 'Numero Nuovo'
012700020909     o          e            RigaOk
012800020909     o                       O33Po1            + 12
012900020909     o                       I33Cnu            +  9
013000020909     o                       I33Unp            +  9
013100020909     o                       O33Nri            +  9
013200020909     o          e            RigaErr
013300020909     o                       O33Po1            + 12
013400020909     o                       I33Cnu            +  9
013500020909     o                       I33Unp            +  9
013600020909     o                       O33Nri            +  9
013700020909     o                       O33Err            +  1
013800020909     o                       O33Msg            +  1
