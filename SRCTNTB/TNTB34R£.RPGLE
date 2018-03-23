000100040407     h decedit('0,') datedit(*dmy.) option(*nodebugio)
000200040407
000300040407      *---------------------------------------------------------------*
000400040407      * Interrogazione tabella "CMR" = Causali ORM                    *
000500040407      *---------------------------------------------------------------*
000600040407
000700040407     fTntbe01l  if   e           k disk
000800040407
000900040407     fTntb341d  cf   e             workstn usropn
001000040407     f                                     sfile(Tb341s1:nrr)
001100040407
001200040407      *---------------------------------------------------------------*
001300040407      *   C O S T A N T I                                             *
001400040407      *---------------------------------------------------------------*
001500040407     d kcod            c                   'CMR'
001600040407
001700040407      *---------------------------------------------------------------*
001800040407      *   S T R U T T U R E   D A T I                                 *
001900040407      *---------------------------------------------------------------*
002000040407     d Azuteds       e ds                  extname(Azute00f)
002100040407     d dCmr          e ds
002200040407     d dFar          e ds
002300040407     d dLat          e ds
002400040407     d dDatiute      e ds
002500040407     d kpjba         e ds
002600040407     d Tibs02ds      e ds
002700040407     d Tibs34ds      e ds
002800040407     d Tntb341ds     e ds
002900040407
003000040407     d                sds
003100040407     d  VTCpgm           *proc
003200040407
003300040407      *---------------------------------------------------------------*
003400040407      *   V A R I A B I L I                                           *
003500040407      *---------------------------------------------------------------*
003600040407     d strke1          s                   like(b34ke1)
003700040407     d wabi            s                   like(UteAut)
003800040407
003900040407      *---------------------------------------------------------------*
004000040407      *   M A I N   L I N E                                           *
004100040407      *---------------------------------------------------------------*
004200040407      *  Riepilogo indicatori utilizzati:                             *
004300040407      *  --------------------------------                             *
004400040407      *  20 - Comodo                                                  *
004500040407      *  40 - SflClr                                                  *
004600040407      *  41 - NOT SflDsp                                              *
004700040407      *  42 - SflEnd                                                  *
004800040407      *---------------------------------------------------------------*
004900040407      *  PARAMETRI:                                                   *
005000040407      *  ----------                                                   *
005100040407      *  Opzione richiesta             1  I                           *
005200040407      *  Chiave 1 tabella             15  I                           *
005300040407      *  Chiave 2 tabella             15  I                           *
005400040407      *  Codice lingua                 1  I                           *
005500040407      *  Sistema Informativo           5  I                           *
005600040407      *  Dati tabella selezionata    256  O                           *
005700040407      *  Tasto funzionale premuto      3  O      F03, F12             *
005800040407      *  Flag di ritorno               1  O      1=Err                *
005900040407      *  Messaggio di ritorno         78  O                           *
006000040407      *---------------------------------------------------------------*
006100040407
006200040407      * Errore sull'utente esco dal pgm
006300040407if  1c                   If        $ErrUte = *On
006400040407     c                   Goto      Fine
006500040407e   1c                   EndIf
006600040407
006700040407if  1c                   If        Not %Open(TnTb341d)
006800040407     c                   Open      TnTb341d
006900040407e   1c                   EndIf
007000040407
007100040407do  1c                   Do        *Hival
007200040407
007300040407      * Emetto videata
007400040407     c                   Write     Tb341t1
007500040407     c                   Write     Tb341z1
007600040407     c                   Exfmt     Tb341c1
007700040407
007800040407sel 2c                   Select
007900040407      * F3=Fine
008000040407w   2c                   When      *InKC
008100040407     c                   Eval      b34fxx = 'F03'
008200040407     c                   Leave
008300040407      * F12=Ritorno
008400040407w   2c                   When      *InKL
008500040407     c                   Eval      b34fxx = 'F12'
008600040407     c                   Leave
008700040407      * F5=Rivisualizza
008800040407w   2c                   When      *InKE
008900040407     c                   Exsr      CarSfl
009000040407e   2c                   Endsl
009100040407
009200040407      * Controlli
009300040407     c                   ExSr      CtrSfl
009400040407
009500030806      * Effettuata una scelta
009600040407if  2c                   If        b34ke1 <> *blanks
009700040407     c                   Leave
009800040407e   2c                   EndIf
009900040407
010000040407e   1c                   EndDo
010100040407
010200030806      * Fine
010300040407     c     Fine          tag
010400040407     c                   movel(p)  Tntb341ds     kpjbu
010500040407     c                   Eval      *InLr = *On
010600040407
010700040407      *---------------------------------------------------------------*
010800040407      * - CARICO SUBFILE
010900040407      *----------------------------------------------------------------*
011000040407     c     CarSfl        BegSr
011100040407
011200030806      * Inizializzazione videata
011300040407     c                   Eval      *in40 = *on
011400040407     c                   Write     tb341c1
011500040407     c                   Eval      *in40 = *off
011600040407     c                   Clear                   Nrr
011700040407
011800040407      * Carico il subfile
011900040407     c                   Movel     strke1        TbeKe1
012000040407     c     kTbe          Setll     Tntbe01l
012100040407do  1c                   Do        *Hival
012200040407     c     kcod          Reade     Tntbe01l
012300040407      * Fine file
012400040407if  2c                   If        %Eof(Tntbe01l)
012500040407     c                   Leave
012600040407e   2c                   EndIf
012700030806      * Escludo annullati
012800040407if  2c                   If        TbeAtb <> *Blanks
012900040407     c                   Iter
013000040407e   2c                   EndIf
013100030806      * Escludo se non del S.I. passato
013200040407if  2c                   If        TbeSif <> *Blanks and TbeSif <> B34Sif
013300040407     c                   Iter
013400040407e   2c                   EndIf
013500030806      * Imposto campi chiave del record selezionato
013600040407     c                   Movel     TbeKe1        Vs1Cau
013700030806      * Imposto dati del record selezionato
013800040407     c                   Movel     TbeUni        dCmr
013900040407     c                   Eval      Vs1Des = d§CmrDes
014000040407     c                   Eval      Vs1Far = d§CmrFar
014100040407     c                   Exsr      DecFar
014200040407     c                   Eval      Vs1Noc = d§CmrNoc
014300040407     c                   Eval      Vs1Noq = d§CmrNoq
014400040407     c                   Eval      Vs1Nmf = d§CmrNmf
014500040407     c                   Add       1             Nrr
014600040407     c                   Write     tb341s1
014700040407e   1c                   EndDo
014800040407
014900040407     c                   Eval      *in42 = *On
015000030806     c                   eval      RrnLast = Nrr
015100030806     c                   eval      RcdNbr  = 1
015200040407
015300040407     c                   EndSr
015400040407
015500040407      *---------------------------------------------------------------*
015600040407      * - CONTROLLO LE SCELTE DEL SUBFILE
015700040407      *---------------------------------------------------------------*
015800040407     c     CtrSfl        BegSr
015900040407
016000040407     c                   Eval      *In41 = *Off
016100030806      *
016200030806do  1c                   DO        *hival        xx
016300030806      *
016400030806     c     xx            chain     tb301s1                            01
016500030806if  2c                   if        NOT %found(tntb301d)
016600030806     c                   leave
016700030806e   2c                   endif
016800030806      *
016900030806if  2c                   if        VS1sce = '1'
017000030806     c                   move      VS1cd1        DSKcd1
017100030806     c                   move      VS1cd2        DSKcd2
017200030806     c                   movel(p)  DSkeyOSR      B30ke1
017300030806     c                   clear                   B30ke2
017400030806     c                   leave
017500030806e   2c                   endif
017600030806      *
017700030806      * Controllo se effettuata ricerca
017800030806if  1c                   if           (VC1cd1 <> *zeros
017900030806     c                             and VC1cd1 <> SAVcd1)
018000030806     c                             or (VC1cd2 <> *zeros
018100030806     c                             and VC1cd2 <> SAVcd2)
018200030806     c                             or (VC1cli <> *zeros
018300030806     c                             and VC1cli <> SAVcli)
018400030806     c                   eval      *in41 = *on
018500030806if  2c                   if           (WrkSort = *blanks
018600030806     c                             and VC1cd1 <= VS1cd1
018700030806     c                             and VC1cd2 <= VS1cd2)
018800030806     c                             or (WrkSort = Ord_CodCli
018900030806     c                             and VC1cli <= VS1cli)
019000030806     c                   eval      *in41 = *off
019100030806     c                   z-add     xx            RcdNbr
019200030806sel 3c                   select
019300030806w   3c                   when      WrkSort = *blanks
019400030806     c                   move      VC1cd1        SAVcd1
019500030806     c                   move      VC1cd2        SAVcd2
019600030806w   3c                   when      WrkSort = Ord_CodCli
019700030806     c                   movel     VC1cli        SAVcli
019800030806e   3c                   endsl
019900030806     c                   leave
020000030806     c                   endif
020100030806     c                   endif
020200030806      *
020300030806e   1c                   ENDDO
020400030806      *
020500030806if  1c                   if        not *in41
020600030806     c                   clear                   VC1cd1
020700030806     c                   clear                   VC1cd2
020800030806     c                   clear                   VC1cli
020900030806e   1c                   endif
021000030806      *
021100030806     c                   EndSr
021200030806      *
021300040407      *---------------------------------------------------------------*
021400040407      * - DECODIFICO LA FASE
021500040407      *---------------------------------------------------------------*
021600040407     c     DecFar        BegSr
021700040407
021800040407     c                   Clear                   dFar
021900040407     c                   Clear                   Vs1dFar
022000040407if  1c                   If        d§CmrFar <> *Blanks
022100040407     c                   Clear                   Tibs02ds
022200040407     c                   Eval      T02Mod = 'C'
022300040407     c                   Eval      T02Sif = knsif
022400040407     c                   Eval      T02Cod = 'FAR'
022500040407     c                   Movel(p)  Vs1Far        T02Ke1
022600040407     c                   Call      'TIBS02R'
022700040407     c                   Parm                    kpjba
022800040407     c                   Parm                    Tibs02ds
022900040407if  2c                   If        T02Err = *Blanks
023000040407     c                   Eval      dFar = T02Uni
023100040407e   2c                   EndIf
023200040407e   1c                   EndIf
023300040407
023400040407     c                   EndSr
023500040407
023600040407      *---------------------------------------------------------------*
023700040407      * - ROUTINE INIZIALE                                            *
023800040407      *---------------------------------------------------------------*
023900040407     c     *InzSr        BegSr
024000040407
024100040407     c     *Entry        Plist
024200040407     c                   Parm                    Kpjba
024300040407
024400040407     c                   Movel     kpjbu         Tntb341ds
024500040407     c                   Clear                   B30fxx
024600040407     c                   Clear                   B30err
024700040407     c                   Clear                   B30msg
024800040407     c                   Movel     B30ke1        STRke1
024900040407     c                   Clear                   B30ke1
025000040407
025100040407     c     *dtaara       define    §azute        azuteds
025200040407     c     *dtaara       define    §datiute      ddatiute
025300040407     c                   in(E)     *dtaara
025400040407     c                   If        %error  or RSUT = *blanks
025500040407     c                   Clear                   Tibs34ds
025600040407     c                   Call      'TIBS34R'
025700040407     c                   Parm                    Tibs34ds
025800040407     c                   In        *dtaara
025900040407     c                   EndIf
026000040407
026100040407     c                   Clear                   wabi
026200040407     c                   Clear                   dLat
026300040407
026400040407      * Verifica errori e autorità profilo
026500040407s   1c                   Select
026600040407      * se ho errori nei dati utente esco dal pgm
026700040407w   1c                   When      DutErr = 'E'
026800040407     c                   Eval      $errute = *On
026900040407     c                   GoTo      EndInz
027000040407      * se non sono in sede (filiale remota)
027100040407      * --> se 1° livello, abilitazioni al terminal
027200040407      *     se 2° livello, abilitazioni al punto operativo
027300040407w   1c                   When      UteAut = *Blanks
027400040407if  2c                   If        DutLpo = '1'                                 *1° LIVELLO
027500040407     c                   Eval      wabi   = 'TP'
027600040407e   2c                   EndIf
027700040407if  2c                   If        DutLpo = '2'                                 *2° LIVELLO
027800040407     c                   Eval      wabi   = 'PO'
027900040407e   2c                   EndIf
028000040407      * sono in sede carica le abilitazioni del profilo
028100040407x   1c                   Other
028200040407     c                   Eval      wabi = UteAut
028300040407e   1c                   EndSl
028400040407      * controllo se ok l'abilitazione dell'utente
028500040407     c                   Clear                   Tibs02ds
028600040407     c                   Eval      T02Mod = 'C'
028700040407     c                   Eval      T02Sif = knsif
028800040407     c                   Eval      T02Cod = 'LAT'
028900040407     c                   Movel(p)  wabi          T02Ke1
029000040407     c                   Call      'TIBS02R'
029100040407     c                   Parm                    kpjba
029200040407     c                   Parm                    Tibs02ds
029300040407if  1c                   If        T02Err = *Blanks
029400040407     c                   Eval      dLat = T02Uni
029500040407e   1c                   EndIf
029600040407if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
029700040407     c                   Eval      $errute = *On
029800040407e   1c                   EndIf
029900040407      * per errore esco dal pgm
030000040407if  1c                   If        $ErrUte = *On
030100040407     c                   GoTo      EndInz
030200040407e   1c                   EndIf
030300040407
030400040407     c     kTbe          klist
030500040407     c                   kfld                    TbeCod
030600040407     c                   kfld                    TbeKe1
030700040407
030800040407     c     EndInz        EndSr
030900040407      *------------------------------------------------------------------------*
