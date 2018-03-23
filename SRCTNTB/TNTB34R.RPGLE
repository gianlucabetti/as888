000100040407     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000200040407
000300040407      *------------------------------------------------------------------------*
000400040407      * Interrogazione tabella "CMR" = Causali ORM
000500040407      *------------------------------------------------------------------------*
000600040407
000700040407     fTntbe01l  if   e           k disk
000800040407     fTntb34d   cf   e             workstn sfile(Tb34s01:nrr)
000900040407
001000040407      *------------------------------------------------------------------------*
001100040407      *   V A R I A B I L I
001200040407      *------------------------------------------------------------------------*
001300040407     d kcod            s                   like(TbeCod) Inz('CMR')
001400040407     d nrr             s              4  0
001500040407     d wabi            s                   like(UteAut)
001600040407     d wcau            s                   like(vs1cau)
001700040407     d $errute         s              1    inz(*off)
001800040407
001900040407      *------------------------------------------------------------------------*
002000040407      *   D S   I N T E R N E / E S T E R N E
002100040407      *------------------------------------------------------------------------*
002200040407     d Azuteds       e ds                  extname(Azute00f)
002300040407     d dCmr          e ds
002400040407     d dFar          e ds
002500040407     d dLat          e ds
002600040407     d dDatiute      e ds
002700040407     d kpjba         e ds
002800040407     d Tibs02ds      e ds
002900040407     d Tibs34ds      e ds
003000040414     d Tntb341ds     e ds
003100040414
003200040414      * Tracciato record file TNTBE00F
003300040414     d Tntbeds       e ds                  extname(Tntbe00f) inz
003400040414     d xTntbeds      e ds                  extname(Tntbe00f) inz
003500040414     d                                     prefix(TBX:3)
003600040407
003700040407     d                sds
003800040407     d  VTCpgm           *proc
003900040407
004000040407      *------------------------------------------------------------------------*
004100040407      *  RIEPILOGO INDICATORI
004200040407      *------------------------------------------------------------------------*
004300040414      *  01 - R.I. causale se annullata
004400040407      *  20 - SflDsp/SflEnd
004500040407      *  21 - SflDspCtl
004600040407      * n21 - SflClr
004700040407      *------------------------------------------------------------------------*
004800040407
004900040407      * Errore sull'utente esco dal pgm
005000040407if  1c                   If        $ErrUte = *On
005100040407     c                   Goto      Fine
005200040407e   1c                   EndIf
005300040407
005400040407      * Carico subfile ?
005500040407     c     Carica        Tag
005600040407     c                   ExSr      Sr_CarSfl
005700040407
005800040407      * Emetto il subfile ?
005900040407     c     EmiS01        Tag
006000040407     c                   ExSr      Sr_EmiSfl
006100040407
006200040407sel 1c                   Select
006300040407      * F3=Fine
006400040407w   1c                   When      *InKc
006500040407     c                   GoTo      Fine
006600040407      * F5=Rivisualizza
006700040407w   1c                   When      *InKe
006800040407     c                   GoTo      Carica
006900040407      * F10=Immissione
007000040407w   1c                   When      *InKj
007100040414     c                   Clear                   Tntb341ds
007200040414     c                   Eval      B341Opz = '1'
007300040407     c                   ExSr      Sr_Call
007400040414     c                   Goto      Carica
007500040414      * F12=Ritorno
007600040414w   1c                   When      *InKl
007700040414     c                   GoTo      Fine
007800040407e   1c                   Endsl
007900040407
008000040407      * Leggo il subfile ?
008100040407if  1c                   If        nrr > *Zeros
008200040407     c                   ExSr      Sr_LetSfl
008300040414     c                   Goto      Carica
008400040407e   1c                   EndIf
008500040407
008600040407     c     Fine          Tag
008700040407
008800040407     c                   Eval      *InLr = *On
008900040407
009000040407      *------------------------------------------------------------------------*
009100040407      * - CARICO IL SUBFILE
009200040407      *------------------------------------------------------------------------*
009300040407     c     Sr_CarSfl     BegSr
009400040407
009500040407     c                   Eval      recsf = 1
009600040407
009700040407      * Pulisco subfile
009800040407     c                   Clear                   nrr
009900040407     c                   Eval      *In21 = *Off
010000040407     c                   Write     Tb34c01
010100040407     c                   Eval      *In20 = *On
010200040407     c                   Eval      *In21 = *On
010300040407
010400040407      * Carico il subfile
010500040407     c     kcod          Setll     Tntbe01l
010600040407do  1c                   Do        *Hival
010700040407     c     kcod          Reade     Tntbe01l
010800040407      * Fine file
010900040407if  2c                   If        %Eof(Tntbe01l)
011000040407     c                   Leave
011100040407e   2c                   EndIf
011200040414      * Escludo se non del S.I.
011300040414if  2c                   If        TbeSif <> TbxSif
011400040407     c                   Iter
011500040407e   2c                   EndIf
011600040414      * Escludo se non della stessa lingua
011700040414if  2c                   If        TbeLin <> TbxLin
011800040414     c                   Iter
011900040414e   2c                   EndIf
012000030806      * Imposto campi chiave del record selezionato
012100040407     c                   Movel     TbeKe1        Vs1Cau
012200030806      * Imposto dati del record selezionato
012300040407     c                   Movel     TbeUni        dCmr
012400040407     c                   Eval      Vs1Des = d§CmrDes
012500040407     c                   Eval      Vs1Far = d§CmrFar
012600040407     c                   Exsr      Sr_DecFar
012700040407     c                   Eval      Vs1Noc = d§CmrNoc
012800040407     c                   Eval      Vs1Noq = d§CmrNoq
012900040407     c                   Eval      Vs1Nmf = d§CmrNmf
013000040414      * Se annullato metto la causale in R.I. nel subfile
013100040414     c                   If        TbeAtb <> *Blanks
013200040414     c                   Eval      *In01 = *On
013300040414     c                   Else
013400040414     c                   Eval      *In01 = *Off
013500040414     c                   EndIf
013600040407     c                   Add       1             Nrr
013700040407     c                   Write     Tb34s01
013800040407e   1c                   EndDo
013900040407
014000040407     c                   EndSr
014100040407
014200040407      *------------------------------------------------------------------------*
014300040407      * - EMETTO IL SUBFILE
014400040407      *------------------------------------------------------------------------*
014500040407     c     Sr_EmiSfl     BegSr
014600040407
014700040407      * Controllo se recsf è maggiore di nrr
014800040407if  1c                   If        recsf > nrr
014900040407     c                   Eval      recsf = 1
015000040407e   1c                   EndIf
015100040407
015200040407     c                   Write     Tb34t01
015300040407     c                   Write     Tb34z01
015400040407
015500040407      * Se non ho caricato emetto una videata di segnalazione
015600040407if  1c                   If        nrr = *Zeros
015700040407     c                   Exfmt     Tb34d01
015800040407e   1c                   EndIf
015900040407
016000040407      * Se ho caricato emetto il subfile
016100040407if  1c                   If        nrr > *Zeros
016200040407     c                   Exfmt     Tb34c01
016300040407e   1c                   EndIf
016400040407
016500040407     c                   EndSr
016600040407
016700040407      *------------------------------------------------------------------------*
016800040407      * - LEGGO IL SUBFILE
016900040407      *------------------------------------------------------------------------*
017000040407     c     Sr_LetSfl     BegSr
017100040407
017200040407     c                   Clear                   nrr
017300040407
017400040407do  1c                   Do        *Hival
017500040407     c                   Eval      nrr = nrr +1
017600040407     c     nrr           Chain     Tb34s01                            30
017700040407     c   30              Leave
017800040407
017900040407      * Scelta
018000040414if  2c                   If        Vs1Opz = '2' or Vs1Opz = '5'
018100040407     c                   Eval      recsf = nrr
018200040414     c                   Clear                   Tntb341ds
018300040414     c                   Eval      B341Opz = Vs1Opz
018400040414     c                   Eval      B341Ke1 = Vs1Cau
018500040407     c                   ExSr      Sr_Call
018600040407     c                   Eval      recsf = nrr
018700040407     c                   Clear                   Vs1Opz
018800040407     c                   Update    Tb34s01
018900040407    2c                   EndIf
019000040407
019100040407e   1c                   EndDo
019200040407
019300040407     c                   EndSr
019400040407
019500040407      *------------------------------------------------------------------------*
019600040407      * - RICHIAMO IL PROGRAMMA DI MANUTENZIONE/IMMISSIONE TABELLA CMR
019700040407      *------------------------------------------------------------------------*
019800040407     c     Sr_Call       BegSr
019900040407
020000040414     c                   Eval      B341Lin = TbxLin
020100040414     c                   Eval      B341Sif = TbxSif
020200040407     c                   Call      'TNTB341R'
020300040414     c                   Parm                    kpjba
020400040414     c                   Parm                    Tntb341ds
020500040414     c                   If        B341Fxx = 'F03'
020600040414     c                   Goto      Fine
020700040414     c                   EndIf
020800040407
020900040407     c                   EndSr
021000040407
021100040407      *------------------------------------------------------------------------*
021200040407      * - DECODIFICO LA FASE
021300040407      *------------------------------------------------------------------------*
021400040407     c     Sr_DecFar     BegSr
021500040407
021600040407     c                   Clear                   dFar
021700040407     c                   Clear                   Vs1dFar
021800040407if  1c                   If        d§CmrFar <> *Blanks
021900040407     c                   Clear                   Tibs02ds
022000040407     c                   Eval      T02Mod = 'C'
022100040407     c                   Eval      T02Sif = knsif
022200040407     c                   Eval      T02Cod = 'FAR'
022300040407     c                   Movel(p)  Vs1Far        T02Ke1
022400040407     c                   Call      'TIBS02R'
022500040407     c                   Parm                    kpjba
022600040407     c                   Parm                    Tibs02ds
022700040407if  2c                   If        T02Err = *Blanks
022800040407     c                   Eval      dFar = T02Uni
022900040407e   2c                   EndIf
023000040407e   1c                   EndIf
023100040407
023200040407     c                   EndSr
023300040407
023400040407      *------------------------------------------------------------------------*
023500040407      * - ROUTINE INIZIALE
023600040407      *------------------------------------------------------------------------*
023700040407     c     *InzSr        BegSr
023800040407
023900040407     c     *Entry        Plist
024000040407     c                   Parm                    Kpjba
024100040407
024200040407     c     *dtaara       define    §azute        azuteds
024300040407     c     *dtaara       define    §datiute      ddatiute
024400040407     c                   in(E)     *dtaara
024500040407     c                   If        %error  or RSUT = *blanks
024600040407     c                   Clear                   Tibs34ds
024700040407     c                   Call      'TIBS34R'
024800040407     c                   Parm                    Tibs34ds
024900040407     c                   In        *dtaara
025000040407     c                   EndIf
025100040407
025200040407     c                   Clear                   wabi
025300040407     c                   Clear                   dLat
025400040407
025500040407      * Verifica errori e autorità profilo
025600040407s   1c                   Select
025700040407      * se ho errori nei dati utente esco dal pgm
025800040407w   1c                   When      DutErr = 'E'
025900040407     c                   Eval      $errute = *On
026000040407      * se non sono in sede (filiale remota)
026100040407      * --> se 1° livello, abilitazioni al terminal
026200040407      *     se 2° livello, abilitazioni al punto operativo
026300040407w   1c                   When      UteAut = *Blanks
026400040407if  2c                   If        DutLpo = '1'                                 *1° LIVELLO
026500040407     c                   Eval      wabi   = 'TP'
026600040407e   2c                   EndIf
026700040407if  2c                   If        DutLpo = '2'                                 *2° LIVELLO
026800040407     c                   Eval      wabi   = 'PO'
026900040407e   2c                   EndIf
027000040407      * sono in sede carica le abilitazioni del profilo
027100040407x   1c                   Other
027200040407     c                   Eval      wabi = UteAut
027300040407e   1c                   EndSl
027400040407      * controllo se ok l'abilitazione dell'utente
027500040407     c                   Clear                   Tibs02ds
027600040407     c                   Eval      T02Mod = 'C'
027700040407     c                   Eval      T02Sif = knsif
027800040407     c                   Eval      T02Cod = 'LAT'
027900040407     c                   Movel(p)  wabi          T02Ke1
028000040407     c                   Call      'TIBS02R'
028100040407     c                   Parm                    kpjba
028200040407     c                   Parm                    Tibs02ds
028300040407if  1c                   If        T02Err = *Blanks
028400040407     c                   Eval      dLat = T02Uni
028500040407e   1c                   EndIf
028600040407if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
028700040407     c                   Eval      $errute = *On
028800040407e   1c                   EndIf
028900040414
029000040414if  1c                   If        $errute = *Off
029100040414      * Aggancio dati generali della tabella in esame
029200040414     c                   Clear                   TbeCod
029300040414     c                   Move      *Zeros        TbeKe1
029400040414     c                   Move      'CMR'         TbeKe1
029500040414     c                   Clear                   TbeKe2
029600040414     c                   Clear                   TbeLin
029700040414     c                   Movel     knsif         TbeSif
029800040414     c     kTbe          Chain     Tntbe01l
029900040414     c                   If        Not %found(Tntbe01l)
030000040414     c                   Clear                   TbeSif
030100040414     c     kTbe          Chain     Tntbe01l
030200040414     c                   EndIf
030300040414     c                   If        %Found(Tntbe01l)
030400040414     c                   Movel     Tntbeds       xTntbeds
030500040414     c                   Else
030600040414     c                   Clear                   xTntbeds
030700040414     c                   EndIf
030800040414e   1c                   EndIf
030900040407
031000040414      * Definizioni chiavi di accesso
031100040414     c     kTbe          klist
031200040414     c                   kfld                    TbeCod
031300040414     c                   kfld                    TbeKe1
031400040414     c                   kfld                    TbeKe2
031500040414     c                   kfld                    TbeLin
031600040414     c                   kfld                    TbeSif
031700040414
031800040407     c                   EndSr
