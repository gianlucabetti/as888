000100031003     h Decedit('0,') Datedit(*ymd.)
000200001113
000300001113      *****************************************************************
000400001113      *                                                               *
000500031003      *       Modifica Fase ORM                                       *
000600001113      *                                                               *
000700001113      *****************************************************************
000800001113
000900031003     fFnOrm01L  uf   e           k Disk
001000031003     fFnArb01L  if   e           k Disk
001100031003     fFnOrf01L  if a e           k Disk
001200031003     fTnvrRccD  cf   e             Workstn
001300010205
001400031003     d OrmRfa          ds
001500031003     d  B                      1      1
001600031003     d  Anno                   2      3  0
001700031003     d  Lnp                    4      6  0
001800031003     d  Nrs                    7      8  0
001900031003     d  Nsp                    9     15  0
002000031003
002100031003     d Aas             s              4  0
002200031003     d fase            s              3
002300001113
002400031003     d Msg             s             40    Dim(7) Ctdata Perrcd(1)
002500001113
002600031003     d ut§dse0f      e ds
002700031003     d cncr80        e ds
002800031003
002900031003
003000001113      *---------------------------------------------------------------*
003100001113      *                                                               *
003200001113      *    UTILIZZO DEGLI INDICATORI                                  *
003300001113      *                                                               *
003400001113      *---------------------------------------------------------------*
003500010205      *
003600010205      * 28 - ERRORE GENERICO DSPF
003700010205      * 30 - Comodo
003800031003      * 40 - ORM errato
003900010205      *
004000001113      *---------------------------------------------------------------*
004100001113
004200010205      * Pulizia Videata
004300031003     c                   ExSr      Sr_Pulvid
004400031003
004500010206      * Emissione Videata
004600031003do  1c                   Do        *Hival
004700010205
004800031003     c                   Exfmt     TnvrRcc1
004900031003
005000010205      * F3=Fine
005100031003     c   kc              Leave
005200010205
005300031003     c                   Setoff                                       28
005400010205
005500010205      * F6=Conferma
005600031003if  2c                   If        *InKf = *On
005700031003     c                   ExSr      Sr_Contr
005800031003     c   28              Iter
005900031003     c                   ExSr      Sr_Conferma
006000031003     c   28              Iter
006100031003     c                   ExSr      Sr_Pulvid
006200031003   x2c                   Else
006300010205
006400010205      * Controlli videata
006500031003     c                   ExSr      Sr_Contr
006600031003     c   28              Iter
006700031003    2c                   EndIf
006800010205
006900031003    1c                   EndDo
007000010205
007100031003     c                   Eval      *InLr = *On
007200031003      *---------------------------------------------------------------*
007300010205      * PULIZIA VIDEATA
007400031003      *---------------------------------------------------------------*
007500031003     c     Sr_Pulvid     BegSr
007600010205
007700031003     c                   Clear                   V1cPoe
007800031003     c                   Clear                   V1cNsr
007900031003     c                   Clear                   V1cNor
008000031003     c                   Clear                   V1cNrv
008100010205
008200031003     c                   EndSr
008300031003      *---------------------------------------------------------------*
008400010205      * CONTROLLI VIDEO
008500031003      *---------------------------------------------------------------*
008600031003     c     Sr_Contr      BegSr
008700010205
008800031003      * Reset indicatori e campi di comodo
008900010205
009000031003     c                   Setoff                                       40
009100031003     c                   Clear                   fase
009200010205
009300031003      * Controllo se esiste l'ORM
009400031003     c     kOrm          Chain     FnOrm01L
009500031003if  1c                   If        Not %Found(FnOrm01L)
009600031003     c                   Eval      *In40 = *On
009700031003     c                   Eval      *In28 = *On
009800031003     c                   Eval      V1cMsg = Msg(1)
009900031003     c                   GoTo      EndsrContr
010000031003   x1c                   Else
010100031003if  2c                   If        OrmFao <> 100
010200031003     c                   Move      OrmFao        fase
010300031003     c                   Eval      *In40 = *On
010400031003     c                   Eval      *In28 = *On
010500031003     c                   Eval      V1cMsg = Msg(7)
010600031003     c                   Eval      %Subst(V1cMsg:13:3) = fase
010700031003     c                   GoTo      EndsrContr
010800031003    1c                   EndIf
010900031003    1c                   EndIf
011000031003
011100031003      * Controllo se esiste la bolla
011200031003     c                   Eval      Aas = 2000
011300031003     c                   Add       Anno          Aas
011400031003     c     kArb          Chain     FnArb01L
011500031003     c                   If        Not %Found(FnArb01L)
011600031003     c                   Eval      *In28 = *On
011700031003     c                   Eval      V1cMsg = Msg(2)
011800031003     c                   GoTo      EndsrContr
011900031003     c                   EndIf
012000031003
012100031003     c     EndsrContr    EndSr
012200010208
012300031003      *---------------------------------------------------------------*
012400031003      * Aggiorno la fase dell'ORM in base alla bolla
012500031003      *---------------------------------------------------------------*
012600031003     c     Sr_Conferma   BegSr
012700010205
012800031003      * Bolla consegnata
012900031003if  1c                   If        ArbDcm <> *Zeros
013000031003
013100031003      * Bolla con Consegna Anomala
013200031003if  2c                   If        ArbCca <> *Blanks
013300031003
013400031003      * Bolla dirottata --> aggiorno
013500031003if  3c                   If        ArbCca = '1'
013600031003      * Aggiorno FnOrm00F
013700031003     c                   Eval      OrmFao = 999
013800031003     c                   Eval      OrmDfo = ArbDcm
013900031003     c                   Movel     ArbHmc        OrmOfo
014000031003     c                   If        OrmOfo = *Zeros
014100031003     c                   Eval      OrmOfo = 120000
014200031003     c                   EndIf
014300040920     c                   Z-Add     *Date         OrmDtt
014400031003     c                   Update    FnOrM000
014500031003      * Scrivo FnOrf00F
014600031003     c                   Clear                   FnOrf000
014700031003     c                   Eval      OrfPoe = OrmPoe
014800031003     c                   Eval      OrfNsr = OrmNsr
014900031003     c                   Eval      OrfNor = OrmNor
015000031003     c                   Eval      OrfNrv = OrmNrv
015100031003     c                   Eval      OrfPog = OrmPor
015200031003     c                   Eval      OrfDae = OrmDfo
015300031003     c                   Eval      OrfOre = OrmOfo
015400031003     c                   Eval      OrfFar = OrmFao
015500031003     c                   Eval      OrfPue = 'EDPCED'
015600031003     c                   Eval      OrfCar = '90'
015700040920     c                   If        OrmNdc > *Zeros
015800040920     c                   Eval      OrfFgs = OrmPor
015900040920     c                   Eval      OrfNdc = OrmNdc
016000040920     c                   Eval      OrfDdc = OrmDdc
016100040920     c                   EndIf
016200031003     c                   Write     FnOrf000
016300031003      * Msg a Video
016400031003     c                   Eval      *In28 = *On
016500031003     c                   Eval      V1cMsg = Msg(6)
016600031003     c                   GoTo      EndsrConf
016700031003
016800031003      * Consegna Anomala diversa da dirottamento non aggiorno
016900031003   x3c                   Else
017000031003      * Msg a Video
017100031003     c                   Eval      *In28 = *On
017200031003     c                   Eval      V1cMsg = Msg(4)
017300031003     c                   Eval      %Subst(V1cMsg:28:1) = ArbCca
017400031003     c                   GoTo      EndsrConf
017500031003    3c                   EndIf
017600031003    2c                   EndIf
017700031003
017800031003      * Bolla consegnata --> aggiorno
017900031003      * Aggiorno FnOrm00F
018000031003     c                   Eval      OrmFao = 600
018100031003     c                   Eval      OrmDfo = ArbDcm
018200031003     c                   Movel     ArbHmc        OrmOfo
018300031003     c                   If        OrmOfo = *Zeros
018400031003     c                   Eval      OrmOfo = 120000
018500031003     c                   EndIf
018600031003     c                   Eval      OrmNPg = 4
018700031003     c                   Eval      OrmNdc = ArbNdc
018800031003     c                   Eval      OrmDdc = ArbDdc
018900040920     c                   Z-add     *Date         OrmDtt
019000031003     c                   Update    FnOrM000
019100031003      * Scrivo FnOrf00F
019200031003     c                   Clear                   FnOrf000
019300031003     c                   Eval      OrfPoe = OrmPoe
019400031003     c                   Eval      OrfNsr = OrmNsr
019500031003     c                   Eval      OrfNor = OrmNor
019600031003     c                   Eval      OrfNrv = OrmNrv
019700031003     c                   Eval      OrfPog = OrmPor
019800031003     c                   Eval      OrfDae = OrmDfo
019900031003     c                   Eval      OrfOre = OrmOfo
020000031003     c                   Eval      OrfFar = OrmFao
020100031003     c                   Eval      OrfPue = 'EDPCED'
020200040920     c                   If        OrmNdc > *Zeros
020300040920     c                   Eval      OrfFgs = OrmPor
020400040920     c                   Eval      OrfNdc = OrmNdc
020500040920     c                   Eval      OrfDdc = OrmDdc
020600040920     c                   EndIf
020700031003     c                   Write     FnOrf000
020800031003      * Msg a Video
020900031003     c                   Eval      *In28 = *On
021000031003     c                   Eval      V1cMsg = Msg(5)
021100031003     c                   GoTo      EndsrConf
021200031003
021300031003      * Bolla non consegnata
021400031003   x1c                   Else
021500031003      * Msg a Video
021600031003     c                   Eval      *In28 = *On
021700031003     c                   Eval      V1cMsg = Msg(3)
021800031003     c                   GoTo      EndsrConf
021900031003    1c                   EndIf
022000010205
022100031003     c     EndsrConf     EndSr
022200031003      *---------------------------------------------------------------*
022300031003      * ROUTINE INIZIALE
022400031003      *---------------------------------------------------------------*
022500031003     c     *Inzsr        BegSr
022600031003
022700031003     c     kOrm          Klist
022800031003     c                   Kfld                    V1cPoe
022900031003     c                   Kfld                    V1cNsr
023000031003     c                   Kfld                    V1cNor
023100031003     c                   Kfld                    V1cNrv
023200031003
023300031003     c     kArb          Klist
023400031003     c                   Kfld                    Aas
023500031003     c                   Kfld                    Lnp
023600031003     c                   Kfld                    Nrs
023700031003     c                   Kfld                    Nsp
023800031003
023900031003     c                   z-add     1             codut
024000031003     c                   call      'X§PARUT'
024100031003     c                   parm                    ut§dse0f
024200031003     c                   movel     rec80         cncr80
024300031003
024400031003     c                   EndSr
024500031003
024600031003**   Msg - Messaggi a video
024700031003ORM non trovato                                                                     1
024800031003Bolla non trovata                                                                   2
024900031003ORM non aggiornato Bolla NON consegnata                                             3
025000031003Bolla con consegna anomala x                                                        4
025100031003Creata fase 600 Chiudere o Bollettare                                               5
025200031003Creata fase 999 x bolla dirottata                                                   6
025300031003ORM in fase xxx                                                                     7
