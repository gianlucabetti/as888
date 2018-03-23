000100030221     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200030221
000300030221      * TRUL09R  *---------------------------------------------------*
000400030221      * - CARICAMENTO P.O. DEL TERMINAL ARRIVO
000500020412      *--------------------------------------------------------------*
000600030221
000700030221     fAzcae05l  if   e           k Disk
000800030224
000900030224     d msg             s             78    dim(10) ctdata perrcd(1)
001000020412
001100030221     d kCaeEpa         s                   Like(CaeEpa) Inz('A')
001200030221     d kCaeTfe         s                   Like(CaeTfe)
001300030221     d kCaeTfp         s                   Like(CaeTfp)
001400020412
001500020412     d xxx             s              2  0
001600030225     d yyy             s              3  0
001700020412
001800030224     d Tibs56Ds      e ds
001900030224     d  Ski                    7    756  0 dim(250)
002000030225
002100031111     d*Trul26Ds      e ds
002200031111     d* skpo                  11    760  0 dim(250)
002300031111     d* skpot                761   1510  0 dim(250)
002400030224
002500030221     d Trul09ds      e ds
002600030224     d  Lin                   19    108  0 Dim(30)
002700020412
002800020412      *---------------------------------------------------------------
002900030221     c     *Entry        Plist
003000030221     c                   Parm                    Trul09ds
003100020412
003200030221     c     kCae          Klist
003300030221     c                   Kfld                    kCaeEpa
003400030221     c                   Kfld                    kCaeTfe
003500020412
003600020412     c     kcaep         klist
003700020412     c                   kfld                    kcaeepa
003800020412     c                   kfld                    kcaetfp
003900020412
004000030221      * I09Tla = ' '  Elaboro e chiudo con RETURN
004100030221      * I09Tla = 'L'  Elaboro e chiudo con LR
004200030221      * I09Tla = 'C'            chiudo con LR  solo per chiusura file
004300030221
004400030221If  1c                   If        I09Tla = 'C'
004500030221     c                   Eval      *InLr = *On
004600020412
004700030221   x1c                   Else
004800031210     c* clearo i campi di output
004900031210     c                   clear                   o09lin
005000061218     c                   clear                   lin
005100031210     c                   clear                   o09err
005200031210     c                   clear                   o09msg
005300030221
005400030225      * Controllo il terminal di arrivo passato
005500040225     c                   Eval      kCaeTfe = I09Tfa
005600040225     c     kCae          Setll     Azcae05l
005700040225     c***                If        Not %Equal(Azcae05l)
005800040225     c***                Clear                   Lin
005900040225     c***                Eval      O09Err = 'E'
006000040225     c***                Eval      O09Msg = Msg(1)
006100040225     c***                GoTo      NoElab
006200040225     c***                EndIf
006300030225
006400030225      * Carico tutti i p.o. gestiti con il relativo p.o. gestore
006500031111     c**                 Clear                   Trul26Ds
006600031111     c**                 Eval      I26tla = 'L'
006700031111     c**                 Eval      I26tpt = '6'
006800031111     c**                 Call      'TRUL26R'
006900031111     c**                 Parm                    Trul26Ds
007000020412
007100030221      * Se non passata data dal chiamante prendo udate
007200030221     c                   If        I09Dta = *Zeros
007300030221     c                   Eval      I09Dta = *Date
007400030221     c                   EndIf
007500030224
007600030224      * Carico tutti i p.o. presenti sullo stesso S.I.
007700030224     c                   Clear                   Tibs56Ds
007800030224     c                   Eval      I56Ppo = I09Tfa
007900030224     c                   Eval      I56Mod = 'POS'
008000030224     c                   Call      'TIBS56R'
008100030224     c                   Parm                    Tibs56Ds
008200020412
008300030224     c                   Select
008400020412
008500030224      * Richiamato con mod = 'TFA' - tutti i p.o. del terminal di arrivo
008600030224     c                   When      I09Mod = 'TFA'
008700030224     c                   ExSr      Sr_CarTfa
008800030224      * Richiamato con mod = 'TFU' - tutti i p.o. del terminal di arrivo sullo stesso AS
008900030224     c                   When      i09Mod = 'TFU'
009000030224     c                   ExSr      Sr_CarTfu
009100031111     c
009200030224      * Richiamato con mod = 'POG' - tutti i p.o. gestiti del terminal di arrivo
009300031111      * richiamo senza senso almeno al momento ... vedremo se ne servono alt
009400031111
009500031111     c***                When      I09Mod = 'POG'
009600031111     c***                ExSr      Sr_CarPog
009700020412
009800030224     c                   EndSl
009900020509
010000040225     c***  NoElab        Tag
010100040225      *  Se non ho caricato niente significa che il p.o. passato non è ter-a
010200040225     c                   if        lin(1) = 0
010300040225     c                   Eval      O09Err = 'E'
010400040225     c                   Eval      O09Msg = Msg(1)
010500040225     c                   endif
010600020412
010700030224    2c                   If        I09Tla = 'L'
010800030224     c                   Eval      *InLR = *On
010900030224   X2c                   Else
011000030224     c                   Return
011100030224    2c                   EndIf
011200020412
011300030224     c                   EndIf
011400020412
011500020412      *---------------------------------------------------------------*
011600030224      * Sr_CarTfa - carico tutti i p.o. del terminal di arrivo
011700020412      *---------------------------------------------------------------*
011800030224     c     Sr_CarTfa     BegSr
011900020412
012000030224     c                   Clear                   xxx
012100030224     c                   Clear                   Lin
012200030224     c                   clear                   O09Err
012300030224     c                   Clear                   O09Msg
012400020412
012500030224     c                   Do        *Hival
012600030224     c     kCae          Reade     Azcae05l
012700030224      * Fine file
012800030224     c                   If        %Eof(Azcae05l)
012900030224     c                   Leave
013000030224     c                   EndIf
013100030224      * Record annullato
013200030224     c                   If        CaeAtb <> *Blanks
013300030224     c                   Iter
013400030224     c                   EndIf
013500030224      * Controllo la data validità
013600030224     c                   If        CaeDde > I09Dta or CaeDsc < I09Dta
013700030224     c                   Iter
013800030224     c                   EndIf
013900030225      * Al primo posto metto il terminal di arrivo
014000030225     c                   If        xxx = *Zeros
014100030225     c                   Eval      xxx = 1
014200030225     c                   Eval      Lin(xxx) = CaeTfe
014300030225     c                   EndIf
014400030224      * Carico la schiera
014500030225     c                   If        CaeTfa <> CaeTfe
014600030224     c                   Add       1             xxx
014700030225     c                   Eval      Lin(xxx) = CaeTfa
014800030225     c                   EndIf
014900020412
015000030224     c                   EndDo
015100020412
015200030224     c                   EndSr
015300020412      *---------------------------------------------------------------*
015400030224      * Sr_CarTfu - carico tutti i p.o. del terminal di arrivo
015500030224      *             sullo stesso AS
015600020412      *---------------------------------------------------------------*
015700030224     c     Sr_CarTfu     BegSr
015800020412
015900030224     c                   Clear                   xxx
016000030224     c                   Clear                   Lin
016100030224     c                   clear                   O09Err
016200030224     c                   Clear                   O09Msg
016300030224
016400030224     c                   Do        *Hival
016500030224     c     kCae          Reade     Azcae05l
016600030224      * Fine file
016700030224     c                   If        %Eof(Azcae05l)
016800030224     c                   Leave
016900030224     c                   EndIf
017000030224      * Record annullato
017100030224     c                   If        CaeAtb <> *Blanks
017200030224     c                   Iter
017300030224     c                   EndIf
017400030224      * Controllo la data validità
017500030224     c                   If        CaeDde > I09Dta or CaeDsc < I09Dta
017600030224     c                   Iter
017700030224     c                   EndIf
017800030225      * Al primo posto metto il terminal di arrivo
017900030225     c                   If        xxx = *Zeros
018000030225     c                   Eval      xxx = 1
018100030225     c                   Eval      Lin(xxx) = CaeTfe
018200030225     c                   EndIf
018300030224      * Carico la schiera
018400030225     c                   If        CaeTfa <> CaeTfe
018500030225     c     CaeTfa        Lookup    Ski                                    30
018600030224     c                   If        Not *In30
018700030224     c                   Iter
018800030224     c                   EndIf
018900030224     c                   Add       1             xxx
019000030225     c                   Eval      Lin(xxx) = CaeTfa
019100030225     c                   EndIf
019200030224
019300030224     c                   EndDo
019400030224
019500030224     c                   EndSr
019600020412      *---------------------------------------------------------------*
019700030224      * Sr_CarPog - carico tutti i p.o. gestiti del terminal di arrivo
019800020412      *---------------------------------------------------------------*
019900031111     c**   Sr_CarPog     BegSr
020000020412
020100031111     c**                 Clear                   xxx
020200031111     c**                 Clear                   Lin
020300031111     c**                 clear                   O09Err
020400031111     c*+                 Clear                   O09Msg
020500030225
020600031111     c**                 Do        *Hival
020700031111     c**   kCae          Reade     Azcae05l
020800030225      * Fine file
020900031111     c**                 If        %Eof(Azcae05l)
021000031111     c**                 Leave
021100031111     c**                 EndIf
021200030225      * Record annullato
021300031111     c**                 If        CaeAtb <> *Blanks
021400031111     c**                 Iter
021500031111     c**                 EndIf
021600030225      * Controllo la data validità
021700031111     c**                 If        CaeDde > I09Dta or CaeDsc < I09Dta
021800031111     c**                 Iter
021900031111     c**                 EndIf
022000030225      * Carico la schiera
022100031111     c**                 If        CaeTfa <> CaeTfe
022200031111     c**                 Z-Add     1             yyy
022300031111     c**   CaeTfa        Lookup    Skpo(yyy)                              30
022400031111     c**                 If        Not *In30
022500031111     c**                 Iter
022600031111     c**                 EndIf
022700031111     c**                 Add       1             xxx
022800031111     c**                 Movel     Skpo(yyy)     Lin(xxx)
022900031111     c**                 EndIf
023000030225
023100031111     c**                 EndDo
023200030224
023300031111     c**                 EndSr
023400031210**
023500031210Trul09r - Il p.o. passato non è un terminal di arrivo
