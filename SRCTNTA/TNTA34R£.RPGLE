000100060804      *===============================================================*
000200140226      *?TNTA34R * Gestione Data prezzo base e V.M.A. tariffa FUEL    ?*
000300060804      *===============================================================*
000400060804
000500060804     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060804
000700060804      *---------------------------------------------------------------*
000800060804
000900060807     fTNTBE01L  if   e           k disk
001000060807      *
001100131213     fTIPMG01L  if   e           k disk
001200131216      *
001300131216     fTIDPB01L  if A e           k disk
001400060807      *
001500131213     fTNTA34D   cf   e             workstn
001600080226     f                                     infds(InfDspF)
001700131213     f                                     sfile(TA34S01:S01nrr)
001800060804
001900060804      *---------------------------------------------------------------*
002000060804
002100060807      *
002200131216      *?Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
002300060807      *
002400131216      * -?Tasti di funzione?
002500060807     d RollUp          c                   const(x'F5')
002600131216      * -?Nr. di righe del sfl (SFLPAG)?
002700060807     d C_SflPag        c                   const(14)
002800131216      *
002900131216      *?Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
003000060804      *
003100131216      * -?Messaggi di errore?
003200140220     d $Msg            s             78    dim(10)  ctdata  perrcd(1)
003300131216      *
003400131216      * -?Dati da tab. "IPG" = Scaglioni di incremento del prezzo del gasolio?
003500060807     d $IPG            s                   dim(999) like(PMGsca)  inz
003600060807     d                                              ascend
003700060807     d $IPGmin         s                   dim(999) like(PMGpmg)  inz
003800060807     d                                              ascend
003900060807     d $IPGmax         s                   dim(999) like(PMGpmg)  inz
004000060807     d                                              ascend
004100060804      *
004200131216      *?Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
004300060804      *
004400060804     d KPJBA         e ds
004500060804      *
004600131216      * -?Parametri x Controllo profilo utenti?
004700060804     d TIBS34ds      e ds
004800131216      * -?Ds di riferimento al file esterno AZUTE00F?
004900060804     d AZUTEds       e ds                  extname(AZUTE00F)
005000131216      * -?Ds per dati organigramma?
005100060804     d DDatiUte      e ds
005200060804      *
005300131216      * -?Parametri x interrogazione tabelle?
005400060804     d TIBS02ds      e ds                  inz
005500060804     d   T02mod      e                     inz('C')
005600060804     d   T02cod      e                     inz('IPG')
005700131216      * -?Tab. "IPG" = Scaglioni di incremento del prezzo del gasolio?
005800060810     d dIPG          e ds                  inz
005900131216      *
006000131216      * -?Parametri x selezione prezzo medio del gasolio?
006100131216     d TNTA30ds      e ds                  inz
006200131216     d   iTA30opz    e                     inz('1')
006300131216     d   oTA30err    e                     inz(*off)
006400140224      *
006500140224      * -?Recupero Data/Scaglione da Prezzo gasolio?
006600140224     d TRULPMGds     e ds                  inz
006700140224     d   oPMGerr     e                     inz(*off)
006800060804      *
006900060804     d Status         sds           333
007000060804     d   SDSpgm          *proc
007100060807      *
007200080226     d InfDspF         ds           512
007300131216     d   £Tasto              369    369
007400131216     d   £SFLnrr             378    379b 0
007500060804      *
007600131216      * -?Parametri per controllo data e transform in giorni - XSRDA8 -?
007700060804     d WLBdat          ds                  inz
007800131216     d   G08dat                       8  0 inz
007900131216     d   G08inv                       8  0 inz
008000131216     d   G08err                       1    inz('3')
008100131216     d   G08tgi                       5  0 inz
008200060804      *
008300131216      *?Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*?
008400131216      *
008500131216      * -?Flags booleani?
008600141124     d wPgmRic         s               n   inz(*off)
008700131216     d $Fine           s               n   inz(*off)
008800131216     d $F10            s               n   inz(*off)
008900140116     d $Bypass         s               n   inz(*off)
009000140220     d $Bypass2        s               n   inz(*off)
009100131216     d $InzS01         s               n   inz(*on)
009200131216     d $InzD01         s               n   inz(*on)
009300131216     d $InzD02         s               n   inz(*on)
009400080506     d $First          s               n   inz(*on)
009500060807      *
009600060804     d $Video          s              1    inz('1')
009700131216      *
009800131216      * -?Indici di schiere?
009900060807     d xx              s              5  0 inz
010000060807      *
010100131216      * -?Campi associati al SFL?
010200060807     d S01nrr          s                   like(C01rcd) inz
010300060807     d SavS01csr       s                   like(C01rcd) inz
010400060807     d W_SflPag        s              3  0 inz
010500060807     d wPag            s              4  0 inz
010600060807     d wRig            s              3  0 inz
010700060804      *
010800131216      * -?Altri campi?
010900060804     d wDate           s              8  0 inz
011000141124     d wMsg            s             78    inz
011100131216     d W1Cdtd          s                   inz  like(DPBdtd)
011200131216     d W1Cdpb          s                   inz  like(DPBdpb)
011300140224     d wVMAsca         s                   inz  like(DPBsvm)
011400060804
011500060804      *---------------------------------------------------------------*
011600131216      *?I N D I C A T O R I                                          ?*
011700060804      *---------------------------------------------------------------*
011800141124      *? 01?    - Abilitato F10=Immissione in S01
011900131216      *? 06?    - Abilitato F6=Conferma in D02
012000131216      *? 10?    - Comodo                                              *
012100131216      *? 28?    - Emissione messaggio di errore a video               *
012200131216      *? 30?    - Pulizia del subfile                                 *
012300131216      *? 31?    - NON emissione del subfile                           *
012400131216      *? 32?    - Record modificato nel subfile (SflNxtChg)           *
012500131216      *? 33?    - Fine dati nel subfile         (SflEnd)              *
012600131216      *? 50?    - Opzione errata                                      *
012700131216      *? 51?    - Data decorrenza errata                              *
012800131216      *? 52?    - Non reperito scaglione per il prezzo medio indicato *
012900140115      *? 53?    - Percentuale minima di applicazione errata           *
013000131216      *? 90?    - Generico di errore                                  *
013100060804      *---------------------------------------------------------------*
013200060804
013300060804     c     *Entry        plist
013400060804     c                   parm                    KPJBA
013500060804      *
013600131216      * -?Operazioni Iniziali?
013700060804     c                   exsr      RoutInz
013800060804      *
013900131216      * -?Gestione Video?
014000060804do  1c                   dow       $Fine   = *off
014100060807     c     $Video        caseq     '1'           GesS01
014200060807     c     $Video        caseq     '2'           GesD01
014300060807     c     $Video        caseq     '3'           GesD02
014400060807     c                   endcs
014500060807e   1c                   enddo
014600060804      *
014700131216      * -?Fine?
014800060804     c                   movel     *on           *inLR
014900060804      *
015000060804      *---------------------------------------------------------------*
015100131216      *?Operazioni Iniziali                                          ?*
015200060804      *---------------------------------------------------------------*
015300060804     c     RoutInz       BEGSR
015400060804      *
015500131216      * -?Reperimento dati job?
015600060804     c                   exsr      DatiJob
015700060804      *
015800060804     c                   movel     SDSpgm        V1Tpgm
015900060807     c                   clear                   WLBdat
016000060807     c                   eval      G08dat  = *date
016100060807     c                   call      'XSRDA8'
016200060807     c                   parm                    WLBdat
016300060807     c                   movel     G08inv        wDate
016400060804      *
016500131216      * -?Intabellamento tab. "IPG"?
016600060804     c                   clear                   xx
016700060807     c                   clear                   $IPG
016800060807     c                   clear                   $IPGmin
016900060807     c                   clear                   $IPGmax
017000060804     c                   eval      TBEcod  = 'IPG'
017100060804     c     TBEcod        setll     TNTBE000
017200060804     c     TBEcod        reade     TNTBE000
017300060804      *
017400060804do  1c                   DOW       NOT  %eof(TNTBE01L)
017500060804      *
017600060804if  2c                   if        TBEatb  = *blanks
017700060807     c                   movel     TBEuni        dIPG
017800060804     c                   add       1             xx
017900060807     c                   movel     TBEke1        $IPG(xx)
018000060810     c                   z-add     §IPGmin       $IPGmin(xx)
018100060810     c                   z-add     §IPGmax       $IPGmax(xx)
018200060804e   2c                   endif
018300060804      *
018400060804     c     TBEcod        reade     TNTBE000
018500060804      *
018600060804e   1c                   ENDDO
018700141124      /free
018800141124       //?se il primo byte della kpjbu è = 'R' vuol dire che il pgm
018900141124       //?è stato richiamato dal TNTA30R
019000141124       //?quindi impedisco l'immissione
019100141124         *in01 = (%subst(kpjbu:1:1) = 'R');
019200141124         wPgmRic = (%subst(kpjbu:1:1) = 'R');
019300141124       //?imposto errore
019400141124         wMsg = (%subst(kpjbu:2:78));
019500141203
019600141203       //?se utente <> da EDP solo visualizzazione
019700141203         IF  not wPgmRic;
019800141203           *in01 = (%subst(knmus:1:3) <> 'EDP');
019900141203         ENDIF;
020000141124      /end-free
020100060804      *
020200060804     c                   ENDSR
020300060804      *
020400060804      *---------------------------------------------------------------*
020500131216      *?Reperimento Dati del job (Utente/Operativi)                  ?*
020600060804      *---------------------------------------------------------------*
020700060804     c     DatiJob       BEGSR
020800060804      *
020900060804     c     *dtaara       define    §azute        azuteds
021000060804     c     *dtaara       define    §datiute      ddatiute
021100060804      *
021200060804     c                   in(E)     *dtaara
021300060804     c                   IF        %ERROR or RSUT = *blanks
021400060804     c                   clear                   Tibs34Ds
021500060804     c                   call      'TIBS34R'
021600060804     c                   parm                    Tibs34Ds
021700060804     c                   in        *dtaara
021800060804     c                   ENDIF
021900060804      *
022000060804     c                   ENDSR
022100060807      *
022200060807      *---------------------------------------------------------------*
022300131216      *?Gestione videata C01/S01                                     ?*
022400060807      *---------------------------------------------------------------*
022500060807     c     GesS01        BEGSR
022600060807      *
022700131216      * -?Inizializzazione della videata?
022800060807if  1c                   if        $InzS01 = *on
022900060807     c                   exsr      InzS01
023000060807     c                   eval      $InzS01     = *off
023100060807e   1c                   endif
023200060807      *
023300131216      * -?Posizionameno del cursore?
023400060807if  1c                   if        C01csr  > *zeros
023500060807     c                   z-add     C01csr        C01rcd
023600060807x   1c                   else
023700060807     c                   z-add     1             C01rcd
023800060807e   1c                   endif
023900141124      /free
024000141124       //?Se richiamato emetto errore se presente?
024100141124         IF  *in01 and wMsg <> *blanks;
024200141124           V1Dmsg = wMsg;
024300141124           *in28 = *on;
024400141124         ENDIF;
024500141124      /end-free
024600060807      *
024700131216      * -?Emissione Testata e Piede con tasti funzionali abilitati?
024800131213     c                   write     TA34T01
024900131213     c                   write     TA34P01
025000131216      * -?Emissione videata?
025100131213     c                   exfmt     TA34C01
025200060807     c                   setoff                                       28  90
025300060807     c                   clear                   V1Dmsg
025400060807      *
025500060807sel 1c                   SELECT
025600131216      * -?F3=Fine?
025700060807w   1c                   WHEN      *inKC
025800060807     c                   exsr      F03S01
025900131216      * -?F10=Inserimento?
026000060807w   1c                   WHEN      *inKJ
026100060807     c                   exsr      F10S01
026200131216      * -?Roll-UP?
026300060807w   1c                   WHEN      £Tasto      = RollUp
026400060807     c                   exsr      RollUpS01
026500060807      *
026600060807x   1c                   OTHER
026700131216      * -?Gestione Opzioni?
026800060807     c                   exsr      OpzS01
026900060807if  2c                   if        *in90
027000060807     c                   leavesr
027100060807e   2c                   endif
027200060807      *
027300060807e   1c                   ENDSL
027400060807      *
027500060807     c                   ENDSR
027600060807      *
027700060807      *---------------------------------------------------------------*
027800131216      *?Inizializzazione videata C01/S01                             ?*
027900060807      *---------------------------------------------------------------*
028000060807     c     InzS01        BEGSR
028100060807      *
028200131216      * -?Pulizia subfile?
028300060807     c                   seton                                        3031
028400131216     c                   write     TA34C01
028500060807     c                   setoff                                         3133
028600060807      *
028700060807     c                   clear                   C01rcd
028800060807     c                   clear                   C01csr
028900060807     c                   clear                   S01nrr
029000060807     c                   clear                   V1Dmsg
029100060807     c                   setoff                                       28  90
029200060807     c                   movea     *zeros        *in(51)
029300060807      *
029400131216      * -?Caricamento dati dal file TIDPB00F?
029500131213     c     *hival        setgt     TIDPB000
029600131213     c                   read      TIDPB000
029700060807      *
029800060807     c                   exsr      RollUpS01
029900060807      *
030000060807     c                   ENDSR
030100060807      *
030200060807      *---------------------------------------------------------------*
030300131216      *?Caricamento pagina successiva di dati                        ?*
030400060807      *---------------------------------------------------------------*
030500060807     c     RollUpS01     BEGSR
030600070105      *
030700060807     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
030800060807     c                   add       1             W_SflPag
030900060807     c                   eval      *in32   = *off
031000060807      *
031100131216      * -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
031200131213do  1c                   DOW       NOT  %eof(TIDPB01L)
031300060807     c                             and  S01nrr < (W_SflPag * C_SflPag)
031400060807      *
031500060807     c                   exsr      RieS01
031600060807     c                   add       1             S01nrr
031700131216     c                   write     TA34S01
031800060807      *
031900131213     c                   read      TIDPB000
032000060807      *
032100060807e   1c                   ENDDO
032200080226      *
032300131216      * -?Visualizzazione del SFL (se ci sono dati)?
032400080226     c                   eval      *in30   = (S01nrr = *zeros)
032500080226      *
032600131216      * -?Attivazione (eventuale) del SFLEND?
032700131213     c                   eval      *in33   = %eof(TIDPB01L)
032800080226      *
032900131216      * -?Posizionamento cursore al 1° rcd della pagina?
033000080226if  1c                   if        S01nrr  > *zeros
033100080226     c     S01nrr        div       C_SflPag      wPag
033200080226     c                   mvr                     wRig
033300080226     c                   eval      C01rcd  = wPag * C_SflPag
033400080226     c                   if        wRig    > *zeros
033500080226     c                   eval      C01rcd  = C01rcd + 1
033600080226     c                   else
033700080226     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
033800080226     c                   endif
033900080226x   1c                   else
034000080226     c                   clear                   C01rcd
034100080226e   1c                   endif
034200080226     c                   eval      C01csr  = C01rcd
034300060807      *
034400060807     c                   ENDSR
034500060807      *
034600060807      *---------------------------------------------------------------*
034700131216      *?Impostazione dati in singolo record del sfl S01              ?*
034800060807      *---------------------------------------------------------------*
034900060807     c     RieS01        BEGSR
035000060807      *
035100131213     c                   clear                   TA34S01
035200060807      *
035300131216      * -?Campi hidden?
035400131213     c                   eval      H1Ddtd  = DPBdtd
035500131216     c                   eval      H1Ddpb  = DPBdpb
035600131216      *
035700131216      * -?Data prezzo base gasolio?
035800131216     c                   reset                   WLBdat
035900131216     c                   eval      G08inv  = DPBdpb
036000131216     c                   call      'XSRDA8'
036100131216     c                   parm                    WLBdat
036200131216     c                   eval      S1Ddpb  = G08dat
036300060807      *
036400131216      * -?Data inizio validità?
036500060807     c                   reset                   WLBdat
036600131213     c                   eval      G08inv  = DPBdtd
036700060807     c                   call      'XSRDA8'
036800060807     c                   parm                    WLBdat
036900060807     c                   eval      S1Cdtd  = G08dat
037000140115      *
037100140115      * -?Percentuale minima di applicazione?
037200140220     c                   eval      S1Npma  = DPBpma
037300140220      *
037400140220      * -?Valore minimo di applicazione?
037500140220     c                   eval      S1Nvma  = DPBvma
037600131216      *
037700131216      * -?Profilo utente inserimento?
037800131216     c                   eval      S1Cute  = DPBute
037900131216      *
038000131216      * -?Data inserimento?
038100060807     c                   reset                   WLBdat
038200131216     c                   eval      G08inv  = DPBdti
038300060807     c                   call      'XSRDA8'
038400060807     c                   parm                    WLBdat
038500060807     c                   eval      S1Cdti  = G08dat
038600131216      *
038700131216      * -?RECUPERO DATI DAL FILE TIPMG01L?
038800131216     c     DPBdpb        chain     TIPMG000
038900131216     c                   if        Not %found(TIPMG01L)
039000131216     c                   leavesr
039100131216     c                   endif
039200131216      *
039300131216      * -?Prezzo medio gasolio?
039400131216     c                   eval      S1Cpmg  = PMGpmg
039500131216      * -?Scaglione incrementi?
039600131216     c*//                eval      S1Csca  = PMGsca
039700060807      *
039800060807     c                   ENDSR
039900060807      *
040000060807      *---------------------------------------------------------------*
040100131216      *?Gestione tasto funzionale F03 da videata S01                 ?*
040200060807      *---------------------------------------------------------------*
040300060807     c     F03S01        BEGSR
040400060807      *
040500131216      * -?Chiusura del programma?
040600060807     c                   eval      $Fine   = *on
040700060807      *
040800060807     c                   ENDSR
040900060807      *
041000060807      *---------------------------------------------------------------*
041100131216      *?Gestione tasto funzionale F10 da videata S01                 ?*
041200060807      *---------------------------------------------------------------*
041300060807     c     F10S01        BEGSR
041400060807      *
041500131216      * -?Passaggio alla videata di immissione (D01)?
041600060807     c                   eval      $Video  = '2'
041700060807     c                   eval      $inzD01 = *on
041800060807     c                   eval      $inzD02 = *on
041900060808      *
042000060808     c                   eval      $F10    = *on
042100060807      *
042200060807     c                   ENDSR
042300060807      *
042400060807      *---------------------------------------------------------------*
042500131216      *?Gestione opzioni subfile                                     ?*
042600060807      *---------------------------------------------------------------*
042700060807     c     OpzS01        BEGSR
042800060807      *
042900060807     c                   clear                   SavS01csr
043000060807      *
043100131216     c                   readc     TA34S01
043200060807      *
043300131216do  1c                   DOW       NOT %eof(TNTA34D)
043400060807      *
043500060807     c                   eval      *in32   = *off
043600060807     c                   movea     *zeros        *in(51)
043700060807     c                   z-add     S01nrr        C01rcd
043800060807      *
043900060807sel 2c                   SELECT
044000131216      * -?Nessuna opzione?
044100060807w   2c                   WHEN      S1Copz  = *blanks
044200131216      * -?5 = Visualizzazione prezzo medio gasolio?
044300060807w   2c                   WHEN      S1Copz  = '5'
044400060807     c                   exsr      sr_OpzS1_5
044500131216      * -?? = Opzione NON valida?
044600060807x   2c                   OTHER
044700060807     c                   seton                                        285190
044800070110     c                   eval      V1Dmsg  = $Msg(1)
044900060807e   2c                   ENDSL
045000060807      *
045100131216      * -?Memorizzazione nrr del sfl con la 1ª opzione?
045200131216      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
045300060807if  1c                   if             S1Copz   <> *blanks
045400060807     c                             and (SavS01csr = *zeros
045500060807     c                              or  SavS01csr < C01rcd)
045600060807     c                   eval      SavS01csr   = C01rcd
045700060807e   1c                   endif
045800060807      *
045900131216      * -?Aggiornamento sfl?
046000060807sel 2c                   select
046100060807w   2c                   when      *in28
046200060807     c                   eval      *in32       = *on
046300060807     c                   z-add     C01rcd        C01csr
046400060807w   2c                   when      *in90       = *on
046500060807     c                   z-add     C01rcd        C01csr
046600060807     c                   clear                   S1Copz
046700060807x   2c                   other
046800060807     c                   clear                   S1Copz
046900060807e   2c                   endsl
047000131216     c                   UPDATE    TA34S01
047100060807if  2c                   if        *in28  OR  *in90
047200060807     c                   leave
047300060807e   2c                   endif
047400060807      *
047500131216     c                   readc     TA34S01
047600060807      *
047700060807e   1c                   ENDDO
047800060807      *
047900131216      * -?Riposiziono il cursore sul record contenente la prima opzione?
048000131216      *  ?(se non sono stati rilevati errori)?
048100060807if  1c                   if        NOT *in28  and  NOT *in90
048200060807     c                             and SavS01csr  > *zeros
048300060807     c                   z-add     SavS01csr     C01csr
048400060807e   1c                   endif
048500060807      *
048600060807     c                   ENDSR
048700060807      *
048800060807      *---------------------------------------------------------------*
048900131216      *?S1_Opz.5 = Visualizzazione singola data Prezzo Base Gasolio  ?*
049000060807      *---------------------------------------------------------------*
049100060807     c     sr_OpzS1_5    BEGSR
049200060807      *
049300131216      * -?Impostazione dati in D01 (solo visualizzato - protetto)?
049400131216     c                   clear                   TA34D01
049500060807     c                   eval      V1Cdtd  = S1Cdtd
049600060807     c*** NO !!!         exsr      CtrD01
049700060807     c                   eval      $F10    = *off
049800060807      *
049900131216      * -?Passaggio alla videata D02 per visualizzazione singolo?
050000131216      *  ?prezzo medio di gasolio?
050100060807     c                   eval      $Video  = '3'
050200060807     c                   eval      $InzD02 = *on
050300060807      *
050400060807do  1c                   doW       $Video  = '3'
050500060807     c                   exsr      GesD02
050600060807e   1c                   enddo
050700060807      *
050800060807     c                   ENDSR
050900060804      *
051000060804      *---------------------------------------------------------------*
051100131216      *?Gestione videata D01                                         ?*
051200060804      *---------------------------------------------------------------*
051300060804     c     GesD01        BEGSR
051400060804      *
051500131216      * -?Inizializzazione videata?
051600060804if  1c                   if        $InzD01 = *on
051700060804     c                   exsr      InzD01
051800060804     c                   movel     *off          $InzD01
051900060804e   1c                   endif
052000060804      *
052100131216      * -?Emissione Testata e Piede con tasti funzionali abilitati?
052200060807     c                   eval      *in03   = *off
052300060807     c                   eval      *in06   = *off
052400131216     c                   write     TA34T01
052500131216     c                   write     TA34P02
052600131216      * -?Emissione videata?
052700131216     c                   exfmt     TA34D01
052800060804     c                   setoff                                       28  90
052900060804     c                   clear                   V1Dmsg
053000060804      *
053100060804sel 1c                   SELECT
053200131216      * -?F12=Ritorno?
053300060807w   1c                   WHEN      *inKL
053400060807     c                   exsr      F12D01
053500060804      *
053600060804x   1c                   OTHER
053700060804     c                   exsr      CtrD01
053800060804      *
053900060804if  2c                   if        *in90
054000060804     c                   leavesr
054100060804e   2c                   endif
054200060804      *
054300060804     c                   eval      $InzD02 = *on
054400060807     c                   eval      $Video  = '3'
054500060804      *
054600060804e   1c                   ENDSL
054700060804      *
054800060804     c                   ENDSR
054900060807      *
055000060807      *---------------------------------------------------------------*
055100131216      *?Inizializzazione videata D01                                 ?*
055200060807      *---------------------------------------------------------------*
055300060807     c     InzD01        BEGSR
055400060807      *
055500131216     c                   clear                   TA34D01
055600060807      *
055700060807     c                   ENDSR
055800060807      *
055900060807      *---------------------------------------------------------------*
056000131216      *?Gestione tasto funzionale F12 da videata D01                 ?*
056100060807      *---------------------------------------------------------------*
056200060807     c     F12D01        BEGSR
056300060807      *
056400131216      * -?Ritorno alla videata precedente?
056500060807     c                   eval      $Video  = '1'
056600060807      *
056700060807     c                   ENDSR
056800060804      *
056900060804      *---------------------------------------------------------------*
057000131216      *?Controllo dati immessi in videata D01                        ?*
057100060804      *---------------------------------------------------------------*
057200060804     c     CtrD01        BEGSR
057300060804      *
057400060804     c                   movea     *zeros        *in(51)
057500131216      *
057600131216      * -?Controllo correttezza data inizio validità - in INSERIMENTO?
057700131216      *  ?(questo controllo viene eseguito SOLO in inserimento - $F10):?
057800060804      *
057900131216      * -?Controllo formale data inizio validità?
058000060804     c                   clear                   WLBdat
058100060804if  1c                   if        V1Cdtd  > *zeros
058200060804     c                   eval      G08dat  = V1Cdtd
058300060804     c                   call      'XSRDA8'
058400060804     c                   parm                    WLBdat
058500060804x   1c                   else
058600060804     c                   eval      G08err  = *on
058700060804e   1c                   endif
058800060804if  1c                   if        G08err  = *on
058900060804     c                   seton                                        285190
059000070110     c                   eval      V1Dmsg  = $Msg(2)
059100060804     c                   leavesr
059200060804e   1c                   endif
059300060804     c                   eval      V1Cdtd  = G08dat
059400060804     c                   eval      W1Cdtd  = G08inv
059500131216      *
059600131216      * -?DEVE essere più recente dell'ultima già inserita in TIDPB00F?
059700131216     c     W1Cdtd        setgt     TIDPB000
059800131216     c                   readp     TIDPB000
059900131216if  1c                   if        NOT  %eof(TIDPB01L)
060000131216     c                             and  DPBdtd >= W1Cdtd
060100060808     c                   reset                   WLBdat
060200131216     c                   eval      G08inv  = DPBdtd
060300060808     c                   call      'XSRDA8'
060400060808     c                   parm                    WLBdat
060500060804     c                   seton                                        285190
060600070110     c                   eval      V1Dmsg  = %trim($Msg(3))
060700060808     c                                     + ' '
060800060808     c                                     + %editc(G08dat:'Y')
060900060804     c                   leavesr
061000060808x   1c***                else
061100131216     c***?già così:?     eval      $F10    = *on
061200060807e   1c                   endif
061300131216      *
061400140226      * -?La decorrenza NON può essere "passata"?
061500140226if  1c                   if        W1Cdtd  < Wdate
061600131216     c                   reset                   WLBdat
061700131216     c                   seton                                        285190
061800131216     c                   eval      V1Dmsg  = $Msg(4)
061900131216     c                   leavesr
062000131216e   1c                   endif
062100060804      *
062200060804     c                   ENDSR
062300060804      *
062400060804      *---------------------------------------------------------------*
062500131216      *?Gestione videata D02                                         ?*
062600060804      *---------------------------------------------------------------*
062700060804     c     GesD02        BEGSR
062800060804      *
062900131216      * -?Inizializzazione videata?
063000060804if  1c                   if        $InzD02 = *on
063100060804     c                   exsr      InzD02
063200060804     c                   movel     *off          $InzD02
063300060804e   1c                   endif
063400060804      *
063500131216      * -?Emissione Testata e Piede con tasti funzionali abilitati?
063600140221      *          ?e videata precedente (D01)?
063700060808     c                   if        NOT  *in28
063800131216     c                   write     TA34T01
063900131216     c                   write     TA34P02
064000131216     c                   write     TA34D01
064100060808     c                   endif
064200131216      * -?Emissione videata?
064300060807if  1c                   if        $F10    = *off
064400131216     c                   write     TA34D02
064500060807     c                   exfmt     PROTECT
064600060807     c                   else
064700060807     c                   write     PROTECT
064800131216     c                   exfmt     TA34D02
064900060807     c                   endif
065000060804     c                   setoff                                       28  90
065100060804     c                   clear                   V1Dmsg
065200060804      *
065300060804sel 1c                   SELECT
065400131216      * -?F3=Fine visualizzazioni?
065500060807w   1c                   WHEN      *inKC
065600060807     c                   exsr      F03D02
065700131216      * -?F12=Ritorno?
065800060804w   1c                   WHEN      *inKL
065900060804     c                   exsr      F12D02
066000131216      * -?F13=Interrogazione tab. IPG?
066100061221w   1c                   WHEN      *inKM
066200061221     c                   exsr      F13D02
066300131216      * -?Invio (in visualizzazione)?
066400060807w   1c                   WHEN      *in06   = *off
066500060807     c***                leavesr
066600131216      * -?Invio / F6 (in immissione)?
066700060804x   1c                   OTHER
066800060804     c                   exsr      CtrD02
066900060807if  2c                   if        *in90
067000060804     c                   leavesr
067100060804e   2c                   endif
067200131216      * -?F6=Conferma?
067300060804if  2c                   if             *inKF   = *on
067400060804     c                             and  *in06   = *on
067500131216     c                   exsr      Wrt_TIDPB
067600131216if  3c                   if        *in90
067700131216     c                   leavesr
067800131216e   3c                   endif
067900060807     c                   eval      $InzS01 = *on
068000060804     c                   eval      $Video  = '1'
068100060804e   2c                   endif
068200060804      *
068300060804e   1c                   ENDSL
068400060804      *
068500060804     c                   ENDSR
068600060807      *
068700060807      *---------------------------------------------------------------*
068800131216      *?Inizializzazione videata D02                                 ?*
068900060807      *---------------------------------------------------------------*
069000060807     c     InzD02        BEGSR
069100060807      *
069200131216     c                   clear                   TA34D02
069300080506     c                   reset                   $First
069400140116     c                   reset                   $Bypass
069500140220     c                   reset                   $Bypass2
069600060807      *
069700131216      * -?Impostazione indicatori:?
069800131216      *  ?*in03?= abilita F3 per ritorno a S01 senza proseguire con?
069900131216      *          ?l'elaborazione delle opzioni successive?
070000131216      *          ?In fase di immissione consente il ritorno al subfile?
070100131216      *          ?senza passare dalla videata D01...?
070200141124      *  ?NON abilito se PGM richiamato     ?
070300141124     c                   IF        not wPgmRic
070400060808     c                   eval      *in03   = *on
070500141124     c                   ENDIF
070600131216      * -?*in06?= abilita F6 per inserimento record?
070700060807     c                   eval      *in06   = ($F10  = *on)
070800060807      *
070900131216      * -?Se è stata richiesta l'opzione 5=Visualizzazione?
071000131216      *  ?si reperiscono i dati da visualizzare dal sfl:?
071100060807if  1c                   IF             S1Copz = '5'
071200060807     c                             and  $F10   = *off
071300060807      *
071400131216      * -?Visualizzazione dati?
071500131216     c                   eval      V1Ddpb  = S1Ddpb
071600131216     c*//                eval      V1Cpmg  = S1Cpmg
071700140115     c                   eval      V1Cpma  = S1Npma
071800140220     c                   eval      V1Cvma  = S1Nvma
071900060807     c                   eval      V1Cute  = S1Cute
072000060807     c                   eval      V1Cdti  = S1Cdti
072100131216      *
072200131216      * -?Reperimento altri dati dal file "Prezzo Medio Gasolio"?
072300131216     c     H1Ddpb        chain     TIPMG000
072400131216     c                   if        %found(TIPMG01L)
072500131216     c                   eval      V1Cpmg  = PMGpmg
072600131216     c                   eval      V1Csca  = PMGsca
072700131216     c                   endif
072800131216      *
072900131216      * -?Decodifica dati?
073000060807     c                   eval      xx      = 1
073100060807     c                   eval      *in10   = *off
073200060807     c     V1Csca        lookup    $IPG(xx)                               10
073300060807if  2c                   if        *in10
073400060807     c                   eval      V1Dsca  = 'Valido da '
073500060807     c                                     + %trim(
073600060807     c                                         %editc( $IPGmin(xx) :
073700060807     c                                         '1') )
073800060807     c                                     + ' a '
073900060807     c                                     + %trim(
074000060807     c                                         %editc( $IPGmax(xx) :
074100060807     c                                         '1') )
074200060808     c                                     + ' Euro/Litro'
074300060807e   2c                   endif
074400060807      *
074500060807e   1c                   ENDIF
074600060807      *
074700060807     c                   ENDSR
074800060807      *
074900060807      *---------------------------------------------------------------*
075000131216      *?Gestione tasto funzionale F03 da videata D02                 ?*
075100060807      *---------------------------------------------------------------*
075200060807     c     F03D02        BEGSR
075300060807      *
075400131216      * -?Ritorno alla 1ª videata (subfile)?
075500060807     c                   eval      $Video  = '1'
075600060807     c                   eval      *in90   = *on
075700060807      *
075800060807     c                   ENDSR
075900060804      *
076000060804      *---------------------------------------------------------------*
076100131216      *?Gestione tasto funzionale F12 da videata D02                 ?*
076200060804      *---------------------------------------------------------------*
076300060804     c     F12D02        BEGSR
076400060804      *
076500131216      * -?Ritorno alla videata precedente?
076600060807     c                   if        *in06   = *on
076700060807     c                   eval      $Video  = '2'
076800060807     c                   else
076900060807     c                   eval      $Video  = '1'
077000060807     c                   endif
077100060804      *
077200060804     c                   ENDSR
077300061221      *
077400061221      *---------------------------------------------------------------*
077500131216      *?Gestione tasto funzionale F13 da videata D02                 ?*
077600061221      *---------------------------------------------------------------*
077700061221     c     F13D02        BEGSR
077800061221      *
077900131216      * -?Richiamo pgm. TNTA30R per interrogaz. prezzi medi settimanali?
078000131216      *  ?(file TIPMG00F)?
078100131216     c                   clear                   KPJBU
078200131216      *
078300131216if  1c                   If        $F10    = *off
078400131216      *
078500131216     c                   clear                   TNTA30ds
078600131216     c                   call      'TNTA30R'
078700131216     c                   parm                    KPJBA
078800131216     c                   parm                    TNTA30ds
078900131216      *
079000131216x   1c                   Else
079100131216      *
079200131216     c                   reset                   TNTA30ds
079300131216     c                   call      'TNTA30R'
079400061221     c                   parm                    KPJBA
079500131216     c                   parm                    TNTA30ds
079600131216      *
079700131216if  2c                   if        oTA30fxx = *blank  and
079800131216     c                             oTA30err = *off    and
079900131216     c                             oTA30dtd > *zero
080000131216     c                   reset                   WLBdat
080100131216     c                   eval      G08inv  = oTA30dtd
080200131216     c                   call      'XSRDA8'
080300131216     c                   parm                    WLBdat
080400131216     c                   eval      V1Ddpb  = G08dat
080500131216e   2c                   endif
080600131216      *
080700131216e   1c                   EndIf
080800061221      *
080900061221     c                   ENDSR
081000060804      *
081100060804      *---------------------------------------------------------------*
081200131216      *?Controllo dati immessi in videata D02                        ?*
081300060804      *---------------------------------------------------------------*
081400060804     c     CtrD02        BEGSR
081500060804      *
081600060804     c                   movea     *zeros        *in(51)
081700080506     c                   clear                   V1Dtxt
081800060804      *
081900131216      * -?Controllo formale data prezzo base?
082000131216     c                   clear                   WLBdat
082100131216if  1c                   if        V1Ddpb  > *zeros
082200131216     c                   eval      G08dat  = V1Ddpb
082300131216     c                   call      'XSRDA8'
082400131216     c                   parm                    WLBdat
082500131216x   1c                   else
082600131216     c                   eval      G08err  = *on
082700131216e   1c                   endif
082800131216if  1c                   if        G08err  = *on
082900131216     c                   seton                                        285290
083000131216     c                   eval      V1Dmsg  = $Msg(2)
083100131216     c                   leavesr
083200131216e   1c                   endif
083300131216     c                   eval      V1Ddpb  = G08dat
083400131216     c                   eval      W1Cdpb  = G08inv
083500131216      *
083600131216      * -?Controllo correttezza data prezzo base:?
083700131216      *  ?DEVE essere già inserita in TIPMG00F?
083800131216     c     W1Cdpb        chain     TIPMG000
083900131216if  1c                   if        NOT  %found(TIPMG01L)
084000131216     c                   seton                                        285290
084100131216     c                   eval      V1Dmsg  = $Msg(5)
084200131216     c                   leavesr
084300131216e   1c                   endif
084400131216      *
084500131216     c                   eval      V1Cpmg  = PMGpmg
084600140224     c                   eval      V1Csca  = PMGsca
084700140224      *
084800140224      * -?Decodifica scaglione?
084900140224     c                   clear                   V1Dsca
085000140224     c                   eval      xx      = 1
085100140224     c                   eval      *in10   = *off
085200140224     c     V1Csca        lookup    $IPG(xx)                               10
085300140224if  1c                   if        *in10
085400140224     c                   eval      V1Dsca  = 'Valido da '
085500140224     c                                     + %trim(
085600140224     c                                         %editc( $IPGmin(xx) :
085700140224     c                                         '1') )
085800140224     c                                     + ' a '
085900140224     c                                     + %trim(
086000140224     c                                         %editc( $IPGmax(xx) :
086100140224     c                                         '1') )
086200140224     c                                     + ' Euro/Litro'
086300140224e   1c                   endif
086400140115      *
086500140115      * -?Controllo percentuale minima di applicazione?
086600140116sel 1c                   select
086700140116w   1c                   when      V1Cpma  < *zeros  or
086800140116     c                             V1Cpma  > 99,999
086900140115     c                   seton                                        285390
087000140115     c                   eval      V1Dmsg  = $Msg(6)
087100140115     c                   leavesr
087200140116w   1c                   when      V1Cpma  = *zeros  and
087300140116     c                             Not $Bypass
087400140116     c                   eval      $Bypass = *on
087500140116     c                   seton                                        285390
087600140116     c                   eval      V1Dmsg  = $Msg(7)
087700140116     c
087800140116     c                   leavesr
087900140116e   1c                   endsl
088000140220      *
088100140220      * -?Controllo valore minimo di applicazione?
088200140220sel 1c                   select
088300140220w   1c                   when      V1Cvma  < *zeros  or
088400140220     c                             V1Cvma  > 9,999
088500140220     c                   seton                                        285490
088600140220     c                   eval      V1Dmsg  = $Msg(8)
088700140220     c                   leavesr
088800140220w   1c                   when      V1Cvma  = *zeros  and
088900140220     c                             Not $Bypass2
089000140220     c                   eval      $Bypass2 = *on
089100140220     c                   seton                                        285490
089200140220     c                   eval      V1Dmsg  = $Msg(9)
089300140220     c
089400140220     c                   leavesr
089500140220e   1c                   endsl
089600140224      *
089700140224      * -?Recupero scaglione relativo al V.M.A.?
089800140224     c                   clear                   wVMAsca
089900140224     c                   clear                   trulPMGds
090000140224     c                   eval      iPMGpmg = V1Cvma
090100140224     c                   clear                   KPJBU
090200140224     c                   eval      KPJBU = trulPMGds
090300140224     c                   call      'TRULPMGR'
090400140224     c                   parm                    KPJBA
090500140224     c                   eval      trulPMGds = KPJBU
090600140224     c                   Select
090700140224      * -?NON reperito alcuno scaglione per il V.M.A. indicato?
090800140224     c                   When      oPMGerr = '1'
090900140224     c                   seton                                        285490
091000140224     c                   eval      V1Dmsg  = oPMGmsg
091100140224     c                   leavesr
091200140224      * -?Prezzo mai rilevato settimanalmente dal Ministero?
091300140224     c*//                When      oPMGerr = '2'
091400140224     c*//                seton                                        285490
091500140224     c*//                eval      V1Dmsg  = oPMGmsg
091600140224     c*//                leavesr
091700140224     c                   EndSl
091800140224     c                   eval      wVMAsca = oPMGsca
091900060804      *
092000060804     c                   ENDSR
092100060804      *
092200060804      *---------------------------------------------------------------*
092300131216      *?Scrittura record in file TIDPB00F                            ?*
092400060804      *---------------------------------------------------------------*
092500131216     c     Wrt_TIDPB     BEGSR
092600060804      *
092700131216     c                   clear                   TIDPB000
092800060804      *
092900131216     c                   eval      DPBdtd = W1Cdtd
093000131216     c                   eval      DPBdpb = W1Cdpb
093100140115     c                   eval      DPBpma = V1Cpma
093200140220     c                   eval      DPBvma = V1Cvma
093300140224     c                   eval      DPBsvm = wVMAsca
093400131216     c                   eval      DPBute = KNMUS
093500131216     c                   eval      DPBdti = wDate
093600060804      *                  __________________
093700131216     c                   WRITE(e)  TIDPB000
093800060804      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093900060807     c                   if        %error
094000060807     c                   eval      $Video = '2'
094100060807     c                   seton                                        285190
094200140220     c                   eval      V1Dmsg = $Msg(10)
094300060807     c*** superfluo:     leavesr
094400060807     c                   endif
094500060807      *
094600060804     c                   ENDSR
094700060804
094800131216** -?$Msg?-?Messaggi di errore?----------------------------------------------*
094900070110Opzione errata                                                                 1
095000070110Data formalmente errata                                                        2
095100131216Data inizio validità NON succesiva all'ultima reperita:                        3
095200140226Data inizio validità precedente a quella attuale                               4
095300131216Data prezzo base inesistente                                                   5
095400140116Percentuale minima di applicazione errata                                      6
095500140116Percentuale minima di applicazione mancante: premere "Invio" per continuare    7
095600140220Valore minimo di applicazione errato                                           8
095700140220Valore minimo di applicazione mancante: premere "Invio" per continuare         9
095800140220Rilevati errori in fase di scrittura record nel file TIDPB00F                 10
