000100020115     H DECEDIT('0,') DATEDIT(*DMY.)
000200020909
000300020909      * -------------------------------------------------------------*
000400020909      * Crea numeratori fogli vari fa FLNUF a AZNUM
000500020910      * Fissi 10 - 11 - 12 - 16 - 17
000600020909      * Prima 1 numeratore per ogni elaboratore
000700020909      * Ora 1 numeratore per p.o.
000800020910      * I numeri vanno scaglionati per p.o.
000900020114      *--------------------------------------------------------------*
001000020909
001100020909     fFlnuf01l  if   e           k Disk                                         Numeratore vecchio
001200020909     fAznum01L  uf a e           k Disk                                         Numeratore nuovo
001300020910     fAzorg01l  if   e           k Disk
001400020909     fQsysprt   o    f  198        Printer Oflind(*InOf)
001500020909
001600020909     d xx              s              2  0
001700020910     d Primo           s              1    Inz('0')
001800020909
001900020909     d Ut§dse0f      e ds
002000020909     d Cncr80        e ds
002100020909     d Trul06Ds      e ds
002200020909     d  Lin                    1     90  0 Dim(30)
002300020909     d Trul33Ds      e ds
002400020911     d Og143         e ds
002500020909     d Kpjba         e ds
002600020909
002700020909      *Klist
002800020909
002900020909     c     Knuf          Klist
003000020909     c                   Kfld                    NufAaa
003100020909     c                   Kfld                    NufCnu
003200020909
003300020909     c                   Z-Add     1             Codut
003400020909     c                   Call      'X§PARUT'
003500020909     c                   Parm                    Ut§dse0f
003600020909     c                   Movel     Rec80         Cncr80
003700020909
003800020909     c                   Except    Testa
003900020909
004000020909      * Carico schiera P.O.
004100020909     c                   Clear                   Trul06Ds
004200020909     c                   Movel     'L'           D06Tla
004300020909     c                   Movel     '£1'          D06Cod
004400020909     c                   Movel     Simfel        D06Key
004500020909     c                   Movel(p)  Trul06Ds      Kpjbu
004600020909     c                   Call      'TRUL06R'
004700020909     c                   Parm                    Kpjba
004800020909     c                   Movel     Kpjbu         Trul06Ds
004900020909
005000020910      *-------------------------
005100020910      * N U M E R A T O R E  1 0
005200020910      *-------------------------
005300020910      * Fare solo per simfel
005400020909      * Cerco il vecchio numero
005500020909     c                   Clear                   NufAaa
005600020910     c                   Z-Add     10            NufCnu
005700020909     c     KNuf          Chain     Flnuf01l
005800020910If  1c                   If        %Found(Flnuf01l)
005900020910      * Creo il nuovo numero
006000020910     c                   Clear                   Trul33Ds
006100020910     c                   Eval      I33Ope = 2
006200020910     c                   Eval      I33Cnu = NufCnu
006300020910     c                   Eval      I33Po1 = Simfel
006400020910     c                   Eval      I33Unp = NufNum
006500020910     c                   Movel(p)  Trul33Ds      Kpjbu
006600020910     c                   Call      'TRUL33R'
006700020910     c                   Parm                    Kpjba
006800020910     c                   Movel     Kpjbu         Trul33Ds
006900020910If  2c                   If        O33Err <> *Zeros
007000020910     c                   Except    RigaErr
007100020910  x 2c                   Else
007200020910     c                   Except    RigaOk
007300020910    2c                   EndIf
007400020910    1c                   EndIf
007500020910
007600020910      *-------------------------
007700020910      * N U M E R A T O R E  1 1
007800020910      *-------------------------
007900020910      * Cerco il vecchio numero
008000020910     c                   Clear                   NufAaa
008100020910     c                   Z-Add     11            NufCnu
008200020910     c     KNuf          Chain     Flnuf01l
008300020910If  1c                   If        %Found(Flnuf01l)
008400020909      * Devo creare un numeratore per ogni p.o. della sk Lin
008500020910Do  2c                   Do        30            xx
008600020909If  3c                   If        Lin(xx) = *zeros
008700020909     c                   Leave
008800020909    3c                   EndIf
008900020910      * Se simfel lo salto xchè lo faccio alla fine
009000020910If  3c                   If        Lin(xx) = Simfel
009100020910     c                   Iter
009200020910    3c                   EndIf
009300020910      * Se P.o. Estero non creo il numeratore
009400020910     c     Lin(xx)       Chain     Azorg01l
009500020911If  3c                   If        %found(Azorg01l)
009600020911     c                   Eval      Og143 = OrgDe3
009700020911If  4c                   If        §ogntw = 'EEX' or §ogntw = 'DPD' or
009800020911     c                             §ogntw = 'FED' or §ogntw = 'EUP'
009900020910     c                   Iter
010000020911    4c                   EndIf
010100020910    3c                   EndIf
010200020909     c                   Clear                   Trul33Ds
010300020909     c                   Eval      I33Ope = 2
010400020910     c                   Eval      I33Cnu = NufCnu
010500020909     c                   Eval      I33Po1 = Lin(xx)
010600020910      * Se primo p.o. imposto il vecchio numero
010700020910If  4c                   If        Primo = *off
010800020909     c                   Eval      I33Unp = NufNum
010900020910     c                   Eval      Primo = *on
011000020910      * Se non è il primo p.o. scagliono di 3000 in 3000
011100020910   x4c                   Else
011200020910     c                   add       3000          NufNum
011300020910      * Se supero il range di 99999 imposto 3000
011400020910If  5c                   If        NufNum > 99999
011500020910     c                   Eval      NufNum = 3000
011600020910    5c                   EndIf
011700020910     c                   Eval      I33Unp = NufNum
011800020910    4c                   EndIf
011900020909     c                   Movel(p)  Trul33Ds      Kpjbu
012000020909     c                   Call      'TRUL33R'
012100020909     c                   Parm                    Kpjba
012200020909     c                   Movel     Kpjbu         Trul33Ds
012300020909If  3c                   If        O33Err <> *Zeros
012400020909     c                   Except    RigaErr
012500020909  x 3c                   Else
012600020909     c                   Except    RigaOk
012700020909    3c                   EndIf
012800020909    2c                   EndDo
012900020910      * Creo il nuovo numero per simfel
013000020910     c                   Clear                   Trul33Ds
013100020910     c                   Eval      I33Ope = 2
013200020910     c                   Eval      I33Cnu = NufCnu
013300020910     c                   Eval      I33Po1 = Simfel
013400020910     c                   add       3000          NufNum
013500020910      * Se supero il range di 99999 imposto 3000
013600020910If  2c                   If        NufNum > 99999
013700020910     c                   Eval      NufNum = 3000
013800020910    2c                   EndIf
013900020910     c                   Eval      I33Unp = NufNum
014000020910     c                   Movel(p)  Trul33Ds      Kpjbu
014100020910     c                   Call      'TRUL33R'
014200020910     c                   Parm                    Kpjba
014300020910     c                   Movel     Kpjbu         Trul33Ds
014400020910If  2c                   If        O33Err <> *Zeros
014500020910     c                   Except    RigaErr
014600020910  x 2c                   Else
014700020910     c                   Except    RigaOk
014800020910    2c                   EndIf
014900020910
015000020910    1c                   EndIf
015100020910
015200020910      *-------------------------
015300020910      * N U M E R A T O R E  1 2
015400020910      *-------------------------
015500020910      * Cerco il vecchio numero
015600020910     c                   Clear                   NufAaa
015700020910     c                   Z-Add     12            NufCnu
015800020910     c                   Eval      Primo = *Off
015900020910     c     KNuf          Chain     Flnuf01l
016000020910If  1c                   If        %Found(Flnuf01l)
016100020910      * Devo creare un numeratore per ogni p.o. della sk Lin
016200020910Do  2c                   Do        30            xx
016300020910If  3c                   If        Lin(xx) = *zeros
016400020910     c                   Leave
016500020910    3c                   EndIf
016600020910      * Se simfel lo salto xchè lo faccio alla fine
016700020910If  3c                   If        Lin(xx) = Simfel
016800020910     c                   Iter
016900020910    3c                   EndIf
017000020910     c                   Clear                   Trul33Ds
017100020910     c                   Eval      I33Ope = 2
017200020910     c                   Eval      I33Cnu = NufCnu
017300020910     c                   Eval      I33Po1 = Lin(xx)
017400020910      * Se primo p.o. imposto il vecchio numero
017500020910If  4c                   If        Primo = *off
017600020910     c                   Eval      I33Unp = NufNum
017700020910     c                   Eval      Primo = *on
017800020910      * Se non è il primo p.o. scagliono di 3000 in 3000
017900020910   x4c                   Else
018000020910     c                   add       3000          NufNum
018100020910      * Se supero il range di 99999 imposto 3000
018200020910If  5c                   If        NufNum > 99999
018300020910     c                   Eval      NufNum = 3000
018400020910    5c                   EndIf
018500020910     c                   Eval      I33Unp = NufNum
018600020910    4c                   EndIf
018700020910     c                   Movel(p)  Trul33Ds      Kpjbu
018800020910     c                   Call      'TRUL33R'
018900020910     c                   Parm                    Kpjba
019000020910     c                   Movel     Kpjbu         Trul33Ds
019100020910If  3c                   If        O33Err <> *Zeros
019200020910     c                   Except    RigaErr
019300020910  x 3c                   Else
019400020910     c                   Except    RigaOk
019500020910    3c                   EndIf
019600020910    2c                   EndDo
019700020910      * Creo il nuovo numero per simfel
019800020910     c                   Clear                   Trul33Ds
019900020910     c                   Eval      I33Ope = 2
020000020910     c                   Eval      I33Cnu = NufCnu
020100020910     c                   Eval      I33Po1 = Simfel
020200020910     c                   add       3000          NufNum
020300020910      * Se supero il range di 99999 imposto 3000
020400020910If  2c                   If        NufNum > 99999
020500020910     c                   Eval      NufNum = 3000
020600020910    2c                   EndIf
020700020910     c                   Eval      I33Unp = NufNum
020800020910     c                   Movel(p)  Trul33Ds      Kpjbu
020900020910     c                   Call      'TRUL33R'
021000020910     c                   Parm                    Kpjba
021100020910     c                   Movel     Kpjbu         Trul33Ds
021200020910If  2c                   If        O33Err <> *Zeros
021300020910     c                   Except    RigaErr
021400020910  x 2c                   Else
021500020910     c                   Except    RigaOk
021600020910    2c                   EndIf
021700020910
021800020910    1c                   EndIf
021900020910
022000020910      *-------------------------
022100020910      * N U M E R A T O R E  1 6
022200020910      *-------------------------
022300020910      * Cerco il vecchio numero
022400020910     c                   Clear                   NufAaa
022500020910     c                   Z-Add     16            NufCnu
022600020910     c                   Eval      Primo = *Off
022700020910     c     KNuf          Chain     Flnuf01l
022800020910If  1c                   If        %Found(Flnuf01l)
022900020910      * Devo creare un numeratore per ogni p.o. della sk Lin
023000020910Do  2c                   Do        30            xx
023100020910If  3c                   If        Lin(xx) = *zeros
023200020910     c                   Leave
023300020910    3c                   EndIf
023400020910      * Se simfel lo salto xchè lo faccio alla fine
023500020910If  3c                   If        Lin(xx) = Simfel
023600020910     c                   Iter
023700020910    3c                   EndIf
023800020910      * Se P.o. Estero non creo il numeratore
023900020910     c     Lin(xx)       Chain     Azorg01l
024000020911If  3c                   If        %found(Azorg01l)
024100020911     c                   Eval      Og143 = OrgDe3
024200020911If  4c                   If        §ogntw = 'EEX' or §ogntw = 'DPD' or
024300020911     c                             §ogntw = 'FED' or §ogntw = 'EUP'
024400020910     c                   Iter
024500020911    4c                   EndIf
024600020910    3c                   EndIf
024700020910     c                   Clear                   Trul33Ds
024800020910     c                   Eval      I33Ope = 2
024900020910     c                   Eval      I33Cnu = NufCnu
025000020910     c                   Eval      I33Po1 = Lin(xx)
025100020910      * Se primo p.o. imposto il vecchio numero
025200020910If  4c                   If        Primo = *off
025300020910     c                   Eval      I33Unp = NufNum
025400020910     c                   Eval      Primo = *on
025500020910      * Se non è il primo p.o. scagliono di 3000 in 3000
025600020910   x4c                   Else
025700020910     c                   add       3000          NufNum
025800020910      * Se supero il range di 99999 imposto 3000
025900020910If  5c                   If        NufNum > 99999
026000020910     c                   Eval      NufNum = 3000
026100020910    5c                   EndIf
026200020910     c                   Eval      I33Unp = NufNum
026300020910    4c                   EndIf
026400020910     c                   Movel(p)  Trul33Ds      Kpjbu
026500020910     c                   Call      'TRUL33R'
026600020910     c                   Parm                    Kpjba
026700020910     c                   Movel     Kpjbu         Trul33Ds
026800020910If  3c                   If        O33Err <> *Zeros
026900020910     c                   Except    RigaErr
027000020910  x 3c                   Else
027100020910     c                   Except    RigaOk
027200020910    3c                   EndIf
027300020910    2c                   EndDo
027400020910      * Creo il nuovo numero per simfel
027500020910     c                   Clear                   Trul33Ds
027600020910     c                   Eval      I33Ope = 2
027700020910     c                   Eval      I33Cnu = NufCnu
027800020910     c                   Eval      I33Po1 = Simfel
027900020910     c                   add       3000          NufNum
028000020910      * Se supero il range di 99999 imposto 3000
028100020910If  2c                   If        NufNum > 99999
028200020910     c                   Eval      NufNum = 3000
028300020910    2c                   EndIf
028400020910     c                   Eval      I33Unp = NufNum
028500020910     c                   Movel(p)  Trul33Ds      Kpjbu
028600020910     c                   Call      'TRUL33R'
028700020910     c                   Parm                    Kpjba
028800020910     c                   Movel     Kpjbu         Trul33Ds
028900020910If  2c                   If        O33Err <> *Zeros
029000020910     c                   Except    RigaErr
029100020910  x 2c                   Else
029200020910     c                   Except    RigaOk
029300020910    2c                   EndIf
029400020910
029500020910    1c                   EndIf
029600020910
029700020910      *-------------------------
029800020910      * N U M E R A T O R E  1 7
029900020910      *-------------------------
030000020910      * Cerco il vecchio numero
030100020910     c                   Clear                   NufAaa
030200020910     c                   Z-Add     17            NufCnu
030300020910     c                   Eval      Primo = *Off
030400020910     c     KNuf          Chain     Flnuf01l
030500020910If  1c                   If        %Found(Flnuf01l)
030600020910      * Devo creare un numeratore per ogni p.o. della sk Lin
030700020910Do  2c                   Do        30            xx
030800020910If  3c                   If        Lin(xx) = *zeros
030900020910     c                   Leave
031000020910    3c                   EndIf
031100020910      * Se simfel lo salto xchè lo faccio alla fine
031200020910If  3c                   If        Lin(xx) = Simfel
031300020910     c                   Iter
031400020910    3c                   EndIf
031500020910      * Se P.o. Estero non creo il numeratore
031600020910     c     Lin(xx)       Chain     Azorg01l
031700020911If  3c                   If        %found(Azorg01l)
031800020911     c                   Eval      Og143 = OrgDe3
031900020911If  4c                   If        §ogntw = 'EEX' or §ogntw = 'DPD' or
032000020911     c                             §ogntw = 'FED' or §ogntw = 'EUP'
032100020911     c                   Iter
032200020911    4c                   EndIf
032300020910    3c                   EndIf
032400020910     c                   Clear                   Trul33Ds
032500020910     c                   Eval      I33Ope = 2
032600020910     c                   Eval      I33Cnu = NufCnu
032700020910     c                   Eval      I33Po1 = Lin(xx)
032800020910      * Se primo p.o. imposto il vecchio numero
032900020910If  4c                   If        Primo = *off
033000020910     c                   Eval      I33Unp = NufNum
033100020910     c                   Eval      Primo = *on
033200020910      * Se non è il primo p.o. scagliono di 3000 in 3000
033300020910   x4c                   Else
033400020910     c                   add       3000          NufNum
033500020910      * Se supero il range di 99999 imposto 3000
033600020910If  5c                   If        NufNum > 99999
033700020910     c                   Eval      NufNum = 3000
033800020910    5c                   EndIf
033900020910     c                   Eval      I33Unp = NufNum
034000020910    4c                   EndIf
034100020910     c                   Movel(p)  Trul33Ds      Kpjbu
034200020910     c                   Call      'TRUL33R'
034300020910     c                   Parm                    Kpjba
034400020910     c                   Movel     Kpjbu         Trul33Ds
034500020910If  3c                   If        O33Err <> *Zeros
034600020910     c                   Except    RigaErr
034700020910  x 3c                   Else
034800020910     c                   Except    RigaOk
034900020910    3c                   EndIf
035000020910    2c                   EndDo
035100020910      * Creo il nuovo numero per simfel
035200020910     c                   Clear                   Trul33Ds
035300020910     c                   Eval      I33Ope = 2
035400020910     c                   Eval      I33Cnu = NufCnu
035500020910     c                   Eval      I33Po1 = Simfel
035600020910     c                   add       3000          NufNum
035700020910      * Se supero il range di 99999 imposto 3000
035800020910If  2c                   If        NufNum > 99999
035900020910     c                   Eval      NufNum = 3000
036000020910    2c                   EndIf
036100020910     c                   Eval      I33Unp = NufNum
036200020910     c                   Movel(p)  Trul33Ds      Kpjbu
036300020910     c                   Call      'TRUL33R'
036400020910     c                   Parm                    Kpjba
036500020910     c                   Movel     Kpjbu         Trul33Ds
036600020910If  2c                   If        O33Err <> *Zeros
036700020910     c                   Except    RigaErr
036800020910  x 2c                   Else
036900020910     c                   Except    RigaOk
037000020910    2c                   EndIf
037100020910
037200020910    1c                   EndIf
037300020909
037400020909     c                   Clear                   Trul33Ds
037500020909     c                   Eval      I33Tla = 'C'
037600020909     c                   Movel(p)  Trul33Ds      Kpjbu
037700020909     c                   Call      'TRUL33R'
037800020909     c                   Parm                    Kpjba
037900020909
038000020909     c                   Eval      *InLr = *On
038100020909
038200020909     oQsysprt   e            Testa          2 02
038300020909     o                                         +  2 'Creazione nuovi'
038400020909     o                                         +  1 'Numeratori Fogli Vari'
038500020909     o                                         +  2 'Nel p.o.'
038600020909     o                       Simfel            +  1
038700020909     o                                         +  5 'TNVRNUFA'
038800020909     o                       Udate             +  2 '  /  /  '
038900020909     o                                         +  5 'Pag.'
039000020909     o                       Page          Z
039100020909     o          e            Testa          2
039200020909     o                                         +  5 'Punto Operativo'
039300020909     o                                         +  3 'Numeratore'
039400020909     o                                         +  3 'Numero Vecchio'
039500020909     o                                         +  3 'Numero Nuovo'
039600020909     o          e            RigaOk
039700020909     o                       O33Po1            + 12
039800020909     o                       I33Cnu            +  9
039900020909     o                       I33Unp            +  9
040000020909     o                       O33Nri            +  9
040100020909     o          e            RigaErr
040200020909     o                       O33Po1            + 12
040300020909     o                       I33Cnu            +  9
040400020909     o                       I33Unp            +  9
040500020909     o                       O33Nri            +  9
040600020909     o                       O33Err            +  1
040700020909     o                       O33Msg            +  1
