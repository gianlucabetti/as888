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
001700030224
001800100630     d xgiolavds     e ds
001900100630     d Trul09ds      e ds
002000030224     d  Lin                   19    108  0 Dim(30)
002100020412
002200020412      *---------------------------------------------------------------
002300030221     c     *Entry        Plist
002400030221     c                   Parm                    Trul09ds
002500020412
002600030221     c     kCae          Klist
002700030221     c                   Kfld                    kCaeEpa
002800030221     c                   Kfld                    kCaeTfe
002900020412
003000020412     c     kcaep         klist
003100020412     c                   kfld                    kcaeepa
003200020412     c                   kfld                    kcaetfp
003300020412
003400030221      * I09Tla = ' '  Elaboro e chiudo con RETURN
003500030221      * I09Tla = 'L'  Elaboro e chiudo con LR
003600030221      * I09Tla = 'C'            chiudo con LR  solo per chiusura file
003700030221
003800030221If  1c                   If        I09Tla = 'C'
003900030221     c                   Eval      *InLr = *On
004000020412
004100030221   x1c                   Else
004200031210     c* clearo i campi di output
004300031210     c                   clear                   o09lin
004400061218     c                   clear                   lin
004500031210     c                   clear                   o09err
004600031210     c                   clear                   o09msg
004700030221
004800030225      * Controllo il terminal di arrivo passato
004900040225     c                   Eval      kCaeTfe = I09Tfa
005000040225     c     kCae          Setll     Azcae05l
005100040225     c***                If        Not %Equal(Azcae05l)
005200040225     c***                Clear                   Lin
005300040225     c***                Eval      O09Err = 'E'
005400040225     c***                Eval      O09Msg = Msg(1)
005500040225     c***                GoTo      NoElab
005600040225     c***                EndIf
005700030225
005800020412
005900030221      * Se non passata data dal chiamante prendo udate
006000030221     c                   If        I09Dta = *Zeros
006100030221     c                   Eval      I09Dta = *Date
006200030221     c                   EndIf
006300020412
006400030224     c                   Select
006500020412
006600030224      * Richiamato con mod = 'TFA' - tutti i p.o. del terminal di arrivo
006700100630      * Richiamato con mod = 'TFU' - tutti i p.o. del terminal di arrivo sullo stesso AS
006800100630      *  sono diventati uguali per cui unica routine
006900100630     c                   When      I09Mod = 'TFA'  or   i09Mod = 'TFU'
007000030224     c                   ExSr      Sr_CarTfa
007100100630     c
007200100630      * Richiamato con mod = 'DEF' - tutti i p.o. del terminal di arrivo conme defluenza quindi alla data
007300100630      *                              elaborazione - 1 gg lavorativo
007400100630     c                   When      i09Mod = 'DEF'
007500100630     c                   ExSr      Sr_CarDef
007600031111     c
007700030224      * Richiamato con mod = 'POG' - tutti i p.o. gestiti del terminal di arrivo
007800031111      * richiamo senza senso almeno al momento ... vedremo se ne servono alt
007900031111
008000031111     c***                When      I09Mod = 'POG'
008100031111     c***                ExSr      Sr_CarPog
008200020412
008300030224     c                   EndSl
008400020509
008500040225     c***  NoElab        Tag
008600040225      *  Se non ho caricato niente significa che il p.o. passato non è ter-a
008700040225     c                   if        lin(1) = 0
008800040225     c                   Eval      O09Err = 'E'
008900040225     c                   Eval      O09Msg = Msg(1)
009000040225     c                   endif
009100020412
009200030224    2c                   If        I09Tla = 'L'
009300030224     c                   Eval      *InLR = *On
009400030224   X2c                   Else
009500030224     c                   Return
009600030224    2c                   EndIf
009700020412
009800030224     c                   EndIf
009900020412
010000020412      *---------------------------------------------------------------*
010100030224      * Sr_CarTfa - carico tutti i p.o. del terminal di arrivo
010200020412      *---------------------------------------------------------------*
010300030224     c     Sr_CarTfa     BegSr
010400020412
010500030224     c                   Clear                   xxx
010600030224     c                   Clear                   Lin
010700030224     c                   clear                   O09Err
010800030224     c                   Clear                   O09Msg
010900020412
011000030224     c                   Do        *Hival
011100030224     c     kCae          Reade     Azcae05l
011200030224      * Fine file
011300030224     c                   If        %Eof(Azcae05l)
011400030224     c                   Leave
011500030224     c                   EndIf
011600030224      * Record annullato
011700030224     c                   If        CaeAtb <> *Blanks
011800030224     c                   Iter
011900030224     c                   EndIf
012000030224      * Controllo la data validità
012100030224     c                   If        CaeDde > I09Dta or CaeDsc < I09Dta
012200030224     c                   Iter
012300030224     c                   EndIf
012400030225      * Al primo posto metto il terminal di arrivo
012500030225     c                   If        xxx = *Zeros
012600030225     c                   Eval      xxx = 1
012700030225     c                   Eval      Lin(xxx) = CaeTfe
012800030225     c                   EndIf
012900030224      * Carico la schiera
013000030225     c                   If        CaeTfa <> CaeTfe
013100030224     c                   Add       1             xxx
013200030225     c                   Eval      Lin(xxx) = CaeTfa
013300030225     c                   EndIf
013400020412
013500030224     c                   EndDo
013600020412
013700030224     c                   EndSr
013800020412      *---------------------------------------------------------------*
013900100630      * Sr_CarTDef- carico tutti i p.o. del terminal di arrivo
014000100630      *             per la defluenza quindi alla data - 1
014100020412      *---------------------------------------------------------------*
014200100630     c     Sr_CarDef     BegSr
014300020412
014400030224     c                   Clear                   xxx
014500030224     c                   Clear                   Lin
014600030224     c                   clear                   O09Err
014700030224     c                   Clear                   O09Msg
014800100630     c
014900100630     c* cerco la data - 1 gg lavorativo
015000100630     c                   clear                   xgiolavds
015100100630     c                   eval      ixgldata=I09DTA
015200100630     c                   eval      ixglfil =I09TFA
015300100630     c                   eval      ixglpa  ='A'
015400100630     c                   eval      ixglsub ='S'
015500100630     c                   eval      ixglgg  =1
015600100630     c                   eval      ixgllav ='S'
015700100630     c
015800100630     c                   call      'XGIOLAV'
015900100630     c                   parm                    xgiolavds
016000100630     c
016100100630     c                   if        oxglerr<>' ' or oxgldata=0
016200100630     c                   eval      oxgldata=I09DTA
016300100630     c                   endif
016400030224
016500030224     c                   Do        *Hival
016600030224     c     kCae          Reade     Azcae05l
016700030224      * Fine file
016800030224     c                   If        %Eof(Azcae05l)
016900030224     c                   Leave
017000030224     c                   EndIf
017100030224      * Record annullato
017200030224     c                   If        CaeAtb <> *Blanks
017300030224     c                   Iter
017400030224     c                   EndIf
017500030224      * Controllo la data validità
017600100630     c                   If        CaeDde > oxgldata or CaeDsc < oxgldata
017700030224     c                   Iter
017800030224     c                   EndIf
017900030225      * Al primo posto metto il terminal di arrivo
018000030225     c                   If        xxx = *Zeros
018100030225     c                   Eval      xxx = 1
018200030225     c                   Eval      Lin(xxx) = CaeTfe
018300030225     c                   EndIf
018400030224      * Carico la schiera
018500030225     c                   If        CaeTfa <> CaeTfe
018600030224     c                   Add       1             xxx
018700030225     c                   Eval      Lin(xxx) = CaeTfa
018800030225     c                   EndIf
018900030224
019000030224     c                   EndDo
019100030224
019200030224     c                   EndSr
019300020412      *---------------------------------------------------------------*
019400030224      * Sr_CarPog - carico tutti i p.o. gestiti del terminal di arrivo
019500020412      *---------------------------------------------------------------*
019600031111     c**   Sr_CarPog     BegSr
019700020412
019800031111     c**                 Clear                   xxx
019900031111     c**                 Clear                   Lin
020000031111     c**                 clear                   O09Err
020100031111     c*+                 Clear                   O09Msg
020200030225
020300031111     c**                 Do        *Hival
020400031111     c**   kCae          Reade     Azcae05l
020500030225      * Fine file
020600031111     c**                 If        %Eof(Azcae05l)
020700031111     c**                 Leave
020800031111     c**                 EndIf
020900030225      * Record annullato
021000031111     c**                 If        CaeAtb <> *Blanks
021100031111     c**                 Iter
021200031111     c**                 EndIf
021300030225      * Controllo la data validità
021400031111     c**                 If        CaeDde > I09Dta or CaeDsc < I09Dta
021500031111     c**                 Iter
021600031111     c**                 EndIf
021700030225      * Carico la schiera
021800031111     c**                 If        CaeTfa <> CaeTfe
021900031111     c**                 Z-Add     1             yyy
022000031111     c**   CaeTfa        Lookup    Skpo(yyy)                              30
022100031111     c**                 If        Not *In30
022200031111     c**                 Iter
022300031111     c**                 EndIf
022400031111     c**                 Add       1             xxx
022500031111     c**                 Movel     Skpo(yyy)     Lin(xxx)
022600031111     c**                 EndIf
022700030225
022800031111     c**                 EndDo
022900030224
023000031111     c**                 EndSr
023100031210**
023200031210Trul09r - Il p.o. passato non è un terminal di arrivo
