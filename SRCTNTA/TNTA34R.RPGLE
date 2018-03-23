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
011300150114     d********* wVMAsca         s                   inz  like(DPBsvm)
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
035600150114     c                   eval      H1Dsvm  = DPBsvm
035700131216      *
035800131216      * -?Data prezzo base gasolio?
035900131216     c                   reset                   WLBdat
036000131216     c                   eval      G08inv  = DPBdpb
036100131216     c                   call      'XSRDA8'
036200131216     c                   parm                    WLBdat
036300131216     c                   eval      S1Ddpb  = G08dat
036400060807      *
036500131216      * -?Data inizio validità?
036600060807     c                   reset                   WLBdat
036700131213     c                   eval      G08inv  = DPBdtd
036800060807     c                   call      'XSRDA8'
036900060807     c                   parm                    WLBdat
037000060807     c                   eval      S1Cdtd  = G08dat
037100140115      *
037200140115      * -?Percentuale minima di applicazione?
037300140220     c                   eval      S1Npma  = DPBpma
037400140220      *
037500140220      * -?Valore minimo di applicazione?
037600140220     c                   eval      S1Nvma  = DPBvma
037700131216      *
037800131216      * -?Profilo utente inserimento?
037900131216     c                   eval      S1Cute  = DPBute
038000131216      *
038100131216      * -?Data inserimento?
038200060807     c                   reset                   WLBdat
038300131216     c                   eval      G08inv  = DPBdti
038400060807     c                   call      'XSRDA8'
038500060807     c                   parm                    WLBdat
038600060807     c                   eval      S1Cdti  = G08dat
038700131216      *
038800131216      * -?RECUPERO DATI DAL FILE TIPMG01L?
038900131216     c     DPBdpb        chain     TIPMG000
039000131216     c                   if        Not %found(TIPMG01L)
039100131216     c                   leavesr
039200131216     c                   endif
039300131216      *
039400131216      * -?Prezzo medio gasolio?
039500131216     c                   eval      S1Cpmg  = PMGpmg
039600131216      * -?Scaglione incrementi?
039700131216     c*//                eval      S1Csca  = PMGsca
039800060807      *
039900060807     c                   ENDSR
040000060807      *
040100060807      *---------------------------------------------------------------*
040200131216      *?Gestione tasto funzionale F03 da videata S01                 ?*
040300060807      *---------------------------------------------------------------*
040400060807     c     F03S01        BEGSR
040500060807      *
040600131216      * -?Chiusura del programma?
040700060807     c                   eval      $Fine   = *on
040800060807      *
040900060807     c                   ENDSR
041000060807      *
041100060807      *---------------------------------------------------------------*
041200131216      *?Gestione tasto funzionale F10 da videata S01                 ?*
041300060807      *---------------------------------------------------------------*
041400060807     c     F10S01        BEGSR
041500060807      *
041600131216      * -?Passaggio alla videata di immissione (D01)?
041700060807     c                   eval      $Video  = '2'
041800060807     c                   eval      $inzD01 = *on
041900060807     c                   eval      $inzD02 = *on
042000060808      *
042100060808     c                   eval      $F10    = *on
042200060807      *
042300060807     c                   ENDSR
042400060807      *
042500060807      *---------------------------------------------------------------*
042600131216      *?Gestione opzioni subfile                                     ?*
042700060807      *---------------------------------------------------------------*
042800060807     c     OpzS01        BEGSR
042900060807      *
043000060807     c                   clear                   SavS01csr
043100060807      *
043200131216     c                   readc     TA34S01
043300060807      *
043400131216do  1c                   DOW       NOT %eof(TNTA34D)
043500060807      *
043600060807     c                   eval      *in32   = *off
043700060807     c                   movea     *zeros        *in(51)
043800060807     c                   z-add     S01nrr        C01rcd
043900060807      *
044000060807sel 2c                   SELECT
044100131216      * -?Nessuna opzione?
044200060807w   2c                   WHEN      S1Copz  = *blanks
044300131216      * -?5 = Visualizzazione prezzo medio gasolio?
044400060807w   2c                   WHEN      S1Copz  = '5'
044500060807     c                   exsr      sr_OpzS1_5
044600131216      * -?? = Opzione NON valida?
044700060807x   2c                   OTHER
044800060807     c                   seton                                        285190
044900070110     c                   eval      V1Dmsg  = $Msg(1)
045000060807e   2c                   ENDSL
045100060807      *
045200131216      * -?Memorizzazione nrr del sfl con la 1ª opzione?
045300131216      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
045400060807if  1c                   if             S1Copz   <> *blanks
045500060807     c                             and (SavS01csr = *zeros
045600060807     c                              or  SavS01csr < C01rcd)
045700060807     c                   eval      SavS01csr   = C01rcd
045800060807e   1c                   endif
045900060807      *
046000131216      * -?Aggiornamento sfl?
046100060807sel 2c                   select
046200060807w   2c                   when      *in28
046300060807     c                   eval      *in32       = *on
046400060807     c                   z-add     C01rcd        C01csr
046500060807w   2c                   when      *in90       = *on
046600060807     c                   z-add     C01rcd        C01csr
046700060807     c                   clear                   S1Copz
046800060807x   2c                   other
046900060807     c                   clear                   S1Copz
047000060807e   2c                   endsl
047100131216     c                   UPDATE    TA34S01
047200060807if  2c                   if        *in28  OR  *in90
047300060807     c                   leave
047400060807e   2c                   endif
047500060807      *
047600131216     c                   readc     TA34S01
047700060807      *
047800060807e   1c                   ENDDO
047900060807      *
048000131216      * -?Riposiziono il cursore sul record contenente la prima opzione?
048100131216      *  ?(se non sono stati rilevati errori)?
048200060807if  1c                   if        NOT *in28  and  NOT *in90
048300060807     c                             and SavS01csr  > *zeros
048400060807     c                   z-add     SavS01csr     C01csr
048500060807e   1c                   endif
048600060807      *
048700060807     c                   ENDSR
048800060807      *
048900060807      *---------------------------------------------------------------*
049000131216      *?S1_Opz.5 = Visualizzazione singola data Prezzo Base Gasolio  ?*
049100060807      *---------------------------------------------------------------*
049200060807     c     sr_OpzS1_5    BEGSR
049300060807      *
049400131216      * -?Impostazione dati in D01 (solo visualizzato - protetto)?
049500131216     c                   clear                   TA34D01
049600060807     c                   eval      V1Cdtd  = S1Cdtd
049700060807     c*** NO !!!         exsr      CtrD01
049800060807     c                   eval      $F10    = *off
049900060807      *
050000131216      * -?Passaggio alla videata D02 per visualizzazione singolo?
050100131216      *  ?prezzo medio di gasolio?
050200060807     c                   eval      $Video  = '3'
050300060807     c                   eval      $InzD02 = *on
050400060807      *
050500060807do  1c                   doW       $Video  = '3'
050600060807     c                   exsr      GesD02
050700060807e   1c                   enddo
050800060807      *
050900060807     c                   ENDSR
051000060804      *
051100060804      *---------------------------------------------------------------*
051200131216      *?Gestione videata D01                                         ?*
051300060804      *---------------------------------------------------------------*
051400060804     c     GesD01        BEGSR
051500060804      *
051600131216      * -?Inizializzazione videata?
051700060804if  1c                   if        $InzD01 = *on
051800060804     c                   exsr      InzD01
051900060804     c                   movel     *off          $InzD01
052000060804e   1c                   endif
052100060804      *
052200131216      * -?Emissione Testata e Piede con tasti funzionali abilitati?
052300060807     c                   eval      *in03   = *off
052400060807     c                   eval      *in06   = *off
052500131216     c                   write     TA34T01
052600131216     c                   write     TA34P02
052700131216      * -?Emissione videata?
052800131216     c                   exfmt     TA34D01
052900060804     c                   setoff                                       28  90
053000060804     c                   clear                   V1Dmsg
053100060804      *
053200060804sel 1c                   SELECT
053300131216      * -?F12=Ritorno?
053400060807w   1c                   WHEN      *inKL
053500060807     c                   exsr      F12D01
053600060804      *
053700060804x   1c                   OTHER
053800060804     c                   exsr      CtrD01
053900060804      *
054000060804if  2c                   if        *in90
054100060804     c                   leavesr
054200060804e   2c                   endif
054300060804      *
054400060804     c                   eval      $InzD02 = *on
054500060807     c                   eval      $Video  = '3'
054600060804      *
054700060804e   1c                   ENDSL
054800060804      *
054900060804     c                   ENDSR
055000060807      *
055100060807      *---------------------------------------------------------------*
055200131216      *?Inizializzazione videata D01                                 ?*
055300060807      *---------------------------------------------------------------*
055400060807     c     InzD01        BEGSR
055500060807      *
055600131216     c                   clear                   TA34D01
055700060807      *
055800060807     c                   ENDSR
055900060807      *
056000060807      *---------------------------------------------------------------*
056100131216      *?Gestione tasto funzionale F12 da videata D01                 ?*
056200060807      *---------------------------------------------------------------*
056300060807     c     F12D01        BEGSR
056400060807      *
056500131216      * -?Ritorno alla videata precedente?
056600060807     c                   eval      $Video  = '1'
056700060807      *
056800060807     c                   ENDSR
056900060804      *
057000060804      *---------------------------------------------------------------*
057100131216      *?Controllo dati immessi in videata D01                        ?*
057200060804      *---------------------------------------------------------------*
057300060804     c     CtrD01        BEGSR
057400060804      *
057500060804     c                   movea     *zeros        *in(51)
057600131216      *
057700131216      * -?Controllo correttezza data inizio validità - in INSERIMENTO?
057800131216      *  ?(questo controllo viene eseguito SOLO in inserimento - $F10):?
057900060804      *
058000131216      * -?Controllo formale data inizio validità?
058100060804     c                   clear                   WLBdat
058200060804if  1c                   if        V1Cdtd  > *zeros
058300060804     c                   eval      G08dat  = V1Cdtd
058400060804     c                   call      'XSRDA8'
058500060804     c                   parm                    WLBdat
058600060804x   1c                   else
058700060804     c                   eval      G08err  = *on
058800060804e   1c                   endif
058900060804if  1c                   if        G08err  = *on
059000060804     c                   seton                                        285190
059100070110     c                   eval      V1Dmsg  = $Msg(2)
059200060804     c                   leavesr
059300060804e   1c                   endif
059400060804     c                   eval      V1Cdtd  = G08dat
059500060804     c                   eval      W1Cdtd  = G08inv
059600131216      *
059700131216      * -?DEVE essere più recente dell'ultima già inserita in TIDPB00F?
059800131216     c     W1Cdtd        setgt     TIDPB000
059900131216     c                   readp     TIDPB000
060000131216if  1c                   if        NOT  %eof(TIDPB01L)
060100131216     c                             and  DPBdtd >= W1Cdtd
060200060808     c                   reset                   WLBdat
060300131216     c                   eval      G08inv  = DPBdtd
060400060808     c                   call      'XSRDA8'
060500060808     c                   parm                    WLBdat
060600060804     c                   seton                                        285190
060700070110     c                   eval      V1Dmsg  = %trim($Msg(3))
060800060808     c                                     + ' '
060900060808     c                                     + %editc(G08dat:'Y')
061000060804     c                   leavesr
061100060808x   1c***                else
061200131216     c***?già così:?     eval      $F10    = *on
061300060807e   1c                   endif
061400131216      *
061500140226      * -?La decorrenza NON può essere "passata"?
061600140226if  1c                   if        W1Cdtd  < Wdate
061700131216     c                   reset                   WLBdat
061800131216     c                   seton                                        285190
061900131216     c                   eval      V1Dmsg  = $Msg(4)
062000131216     c                   leavesr
062100131216e   1c                   endif
062200060804      *
062300060804     c                   ENDSR
062400060804      *
062500060804      *---------------------------------------------------------------*
062600131216      *?Gestione videata D02                                         ?*
062700060804      *---------------------------------------------------------------*
062800060804     c     GesD02        BEGSR
062900060804      *
063000131216      * -?Inizializzazione videata?
063100060804if  1c                   if        $InzD02 = *on
063200060804     c                   exsr      InzD02
063300060804     c                   movel     *off          $InzD02
063400060804e   1c                   endif
063500060804      *
063600131216      * -?Emissione Testata e Piede con tasti funzionali abilitati?
063700140221      *          ?e videata precedente (D01)?
063800060808     c                   if        NOT  *in28
063900131216     c                   write     TA34T01
064000131216     c                   write     TA34P02
064100131216     c                   write     TA34D01
064200060808     c                   endif
064300131216      * -?Emissione videata?
064400060807if  1c                   if        $F10    = *off
064500131216     c                   write     TA34D02
064600060807     c                   exfmt     PROTECT
064700060807     c                   else
064800060807     c                   write     PROTECT
064900131216     c                   exfmt     TA34D02
065000060807     c                   endif
065100060804     c                   setoff                                       28  90
065200060804     c                   clear                   V1Dmsg
065300060804      *
065400060804sel 1c                   SELECT
065500131216      * -?F3=Fine visualizzazioni?
065600060807w   1c                   WHEN      *inKC
065700060807     c                   exsr      F03D02
065800131216      * -?F12=Ritorno?
065900060804w   1c                   WHEN      *inKL
066000060804     c                   exsr      F12D02
066100131216      * -?F13=Interrogazione tab. IPG?
066200061221w   1c                   WHEN      *inKM
066300061221     c                   exsr      F13D02
066400131216      * -?Invio (in visualizzazione)?
066500060807w   1c                   WHEN      *in06   = *off
066600060807     c***                leavesr
066700131216      * -?Invio / F6 (in immissione)?
066800060804x   1c                   OTHER
066900060804     c                   exsr      CtrD02
067000060807if  2c                   if        *in90
067100060804     c                   leavesr
067200060804e   2c                   endif
067300131216      * -?F6=Conferma?
067400060804if  2c                   if             *inKF   = *on
067500060804     c                             and  *in06   = *on
067600131216     c                   exsr      Wrt_TIDPB
067700131216if  3c                   if        *in90
067800131216     c                   leavesr
067900131216e   3c                   endif
068000060807     c                   eval      $InzS01 = *on
068100060804     c                   eval      $Video  = '1'
068200060804e   2c                   endif
068300060804      *
068400060804e   1c                   ENDSL
068500060804      *
068600060804     c                   ENDSR
068700060807      *
068800060807      *---------------------------------------------------------------*
068900131216      *?Inizializzazione videata D02                                 ?*
069000060807      *---------------------------------------------------------------*
069100060807     c     InzD02        BEGSR
069200060807      *
069300131216     c                   clear                   TA34D02
069400080506     c                   reset                   $First
069500140116     c                   reset                   $Bypass
069600140220     c                   reset                   $Bypass2
069700060807      *
069800131216      * -?Impostazione indicatori:?
069900131216      *  ?*in03?= abilita F3 per ritorno a S01 senza proseguire con?
070000131216      *          ?l'elaborazione delle opzioni successive?
070100131216      *          ?In fase di immissione consente il ritorno al subfile?
070200131216      *          ?senza passare dalla videata D01...?
070300141124      *  ?NON abilito se PGM richiamato     ?
070400141124     c                   IF        not wPgmRic
070500060808     c                   eval      *in03   = *on
070600141124     c                   ENDIF
070700131216      * -?*in06?= abilita F6 per inserimento record?
070800060807     c                   eval      *in06   = ($F10  = *on)
070900060807      *
071000131216      * -?Se è stata richiesta l'opzione 5=Visualizzazione?
071100131216      *  ?si reperiscono i dati da visualizzare dal sfl:?
071200060807if  1c                   IF             S1Copz = '5'
071300060807     c                             and  $F10   = *off
071400060807      *
071500131216      * -?Visualizzazione dati?
071600131216     c                   eval      V1Ddpb  = S1Ddpb
071700131216     c*//                eval      V1Cpmg  = S1Cpmg
071800140115     c                   eval      V1Cpma  = S1Npma
071900140220     c                   eval      V1Cvma  = S1Nvma
072000150114     c                   eval      V1Csvm  = H1Dsvm
072100060807     c                   eval      V1Cute  = S1Cute
072200060807     c                   eval      V1Cdti  = S1Cdti
072300131216      *
072400131216      * -?Reperimento altri dati dal file "Prezzo Medio Gasolio"?
072500131216     c     H1Ddpb        chain     TIPMG000
072600131216     c                   if        %found(TIPMG01L)
072700131216     c                   eval      V1Cpmg  = PMGpmg
072800131216     c                   eval      V1Csca  = PMGsca
072900131216     c                   endif
073000131216      *
073100131216      * -?Decodifica dati?
073200060807     c                   eval      xx      = 1
073300060807     c                   eval      *in10   = *off
073400060807     c     V1Csca        lookup    $IPG(xx)                               10
073500060807if  2c                   if        *in10
073600060807     c                   eval      V1Dsca  = 'Valido da '
073700060807     c                                     + %trim(
073800060807     c                                         %editc( $IPGmin(xx) :
073900060807     c                                         '1') )
074000060807     c                                     + ' a '
074100060807     c                                     + %trim(
074200060807     c                                         %editc( $IPGmax(xx) :
074300060807     c                                         '1') )
074400060808     c                                     + ' Euro/Litro'
074500060807e   2c                   endif
074600150114      *
074700150114      * -?Decodifica scaglione VMA?
074800150114     c                   eval      xx      = 1
074900150114     c                   eval      *in10   = *off
075000150114     c     V1Csvm        lookup    $IPG(xx)                               10
075100150114if  2c                   if        *in10
075200150114     c                   eval      V1Dsvm  = 'Valido da '
075300150114     c                                     + %trim(
075400150114     c                                         %editc( $IPGmin(xx) :
075500150114     c                                         '1') )
075600150114     c                                     + ' a '
075700150114     c                                     + %trim(
075800150114     c                                         %editc( $IPGmax(xx) :
075900150114     c                                         '1') )
076000150114     c                                     + ' Euro/Litro'
076100150114e   2c                   endif
076200060807      *
076300060807e   1c                   ENDIF
076400060807      *
076500060807     c                   ENDSR
076600060807      *
076700060807      *---------------------------------------------------------------*
076800131216      *?Gestione tasto funzionale F03 da videata D02                 ?*
076900060807      *---------------------------------------------------------------*
077000060807     c     F03D02        BEGSR
077100060807      *
077200131216      * -?Ritorno alla 1ª videata (subfile)?
077300060807     c                   eval      $Video  = '1'
077400060807     c                   eval      *in90   = *on
077500060807      *
077600060807     c                   ENDSR
077700060804      *
077800060804      *---------------------------------------------------------------*
077900131216      *?Gestione tasto funzionale F12 da videata D02                 ?*
078000060804      *---------------------------------------------------------------*
078100060804     c     F12D02        BEGSR
078200060804      *
078300131216      * -?Ritorno alla videata precedente?
078400060807     c                   if        *in06   = *on
078500060807     c                   eval      $Video  = '2'
078600060807     c                   else
078700060807     c                   eval      $Video  = '1'
078800060807     c                   endif
078900060804      *
079000060804     c                   ENDSR
079100061221      *
079200061221      *---------------------------------------------------------------*
079300131216      *?Gestione tasto funzionale F13 da videata D02                 ?*
079400061221      *---------------------------------------------------------------*
079500061221     c     F13D02        BEGSR
079600061221      *
079700131216      * -?Richiamo pgm. TNTA30R per interrogaz. prezzi medi settimanali?
079800131216      *  ?(file TIPMG00F)?
079900131216     c                   clear                   KPJBU
080000131216      *
080100131216if  1c                   If        $F10    = *off
080200131216      *
080300131216     c                   clear                   TNTA30ds
080400131216     c                   call      'TNTA30R'
080500131216     c                   parm                    KPJBA
080600131216     c                   parm                    TNTA30ds
080700131216      *
080800131216x   1c                   Else
080900131216      *
081000131216     c                   reset                   TNTA30ds
081100131216     c                   call      'TNTA30R'
081200061221     c                   parm                    KPJBA
081300131216     c                   parm                    TNTA30ds
081400131216      *
081500131216if  2c                   if        oTA30fxx = *blank  and
081600131216     c                             oTA30err = *off    and
081700131216     c                             oTA30dtd > *zero
081800131216     c                   reset                   WLBdat
081900131216     c                   eval      G08inv  = oTA30dtd
082000131216     c                   call      'XSRDA8'
082100131216     c                   parm                    WLBdat
082200131216     c                   eval      V1Ddpb  = G08dat
082300131216e   2c                   endif
082400131216      *
082500131216e   1c                   EndIf
082600061221      *
082700061221     c                   ENDSR
082800060804      *
082900060804      *---------------------------------------------------------------*
083000131216      *?Controllo dati immessi in videata D02                        ?*
083100060804      *---------------------------------------------------------------*
083200060804     c     CtrD02        BEGSR
083300060804      *
083400060804     c                   movea     *zeros        *in(51)
083500080506     c                   clear                   V1Dtxt
083600060804      *
083700131216      * -?Controllo formale data prezzo base?
083800131216     c                   clear                   WLBdat
083900131216if  1c                   if        V1Ddpb  > *zeros
084000131216     c                   eval      G08dat  = V1Ddpb
084100131216     c                   call      'XSRDA8'
084200131216     c                   parm                    WLBdat
084300131216x   1c                   else
084400131216     c                   eval      G08err  = *on
084500131216e   1c                   endif
084600131216if  1c                   if        G08err  = *on
084700131216     c                   seton                                        285290
084800131216     c                   eval      V1Dmsg  = $Msg(2)
084900131216     c                   leavesr
085000131216e   1c                   endif
085100131216     c                   eval      V1Ddpb  = G08dat
085200131216     c                   eval      W1Cdpb  = G08inv
085300131216      *
085400131216      * -?Controllo correttezza data prezzo base:?
085500131216      *  ?DEVE essere già inserita in TIPMG00F?
085600131216     c     W1Cdpb        chain     TIPMG000
085700131216if  1c                   if        NOT  %found(TIPMG01L)
085800131216     c                   seton                                        285290
085900131216     c                   eval      V1Dmsg  = $Msg(5)
086000131216     c                   leavesr
086100131216e   1c                   endif
086200131216      *
086300131216     c                   eval      V1Cpmg  = PMGpmg
086400140224     c                   eval      V1Csca  = PMGsca
086500140224      *
086600140224      * -?Decodifica scaglione?
086700140224     c                   clear                   V1Dsca
086800140224     c                   eval      xx      = 1
086900140224     c                   eval      *in10   = *off
087000140224     c     V1Csca        lookup    $IPG(xx)                               10
087100140224if  1c                   if        *in10
087200140224     c                   eval      V1Dsca  = 'Valido da '
087300140224     c                                     + %trim(
087400140224     c                                         %editc( $IPGmin(xx) :
087500140224     c                                         '1') )
087600140224     c                                     + ' a '
087700140224     c                                     + %trim(
087800140224     c                                         %editc( $IPGmax(xx) :
087900140224     c                                         '1') )
088000140224     c                                     + ' Euro/Litro'
088100140224e   1c                   endif
088200140115      *
088300140115      * -?Controllo percentuale minima di applicazione?
088400140116sel 1c                   select
088500140116w   1c                   when      V1Cpma  < *zeros  or
088600140116     c                             V1Cpma  > 99,999
088700140115     c                   seton                                        285390
088800140115     c                   eval      V1Dmsg  = $Msg(6)
088900140115     c                   leavesr
089000140116w   1c                   when      V1Cpma  = *zeros  and
089100140116     c                             Not $Bypass
089200140116     c                   eval      $Bypass = *on
089300140116     c                   seton                                        285390
089400140116     c                   eval      V1Dmsg  = $Msg(7)
089500140116     c
089600140116     c                   leavesr
089700140116e   1c                   endsl
089800140220      *
089900140220      * -?Controllo valore minimo di applicazione?
090000140220sel 1c                   select
090100140220w   1c                   when      V1Cvma  < *zeros  or
090200140220     c                             V1Cvma  > 9,999
090300140220     c                   seton                                        285490
090400140220     c                   eval      V1Dmsg  = $Msg(8)
090500140220     c                   leavesr
090600140220w   1c                   when      V1Cvma  = *zeros  and
090700140220     c                             Not $Bypass2
090800140220     c                   eval      $Bypass2 = *on
090900140220     c                   seton                                        285490
091000140220     c                   eval      V1Dmsg  = $Msg(9)
091100140220     c
091200140220     c                   leavesr
091300140220e   1c                   endsl
091400140224      *
091500140224      * -?Recupero scaglione relativo al V.M.A.?
091600150114     c*******            clear                   wVMAsca
091700140224     c                   clear                   trulPMGds
091800140224     c                   eval      iPMGpmg = V1Cvma
091900140224     c                   clear                   KPJBU
092000140224     c                   eval      KPJBU = trulPMGds
092100140224     c                   call      'TRULPMGR'
092200140224     c                   parm                    KPJBA
092300140224     c                   eval      trulPMGds = KPJBU
092400140224     c                   Select
092500140224      * -?NON reperito alcuno scaglione per il V.M.A. indicato?
092600140224     c                   When      oPMGerr = '1'
092700140224     c                   seton                                        285490
092800140224     c                   eval      V1Dmsg  = oPMGmsg
092900140224     c                   leavesr
093000140224      * -?Prezzo mai rilevato settimanalmente dal Ministero?
093100140224     c*//                When      oPMGerr = '2'
093200140224     c*//                seton                                        285490
093300140224     c*//                eval      V1Dmsg  = oPMGmsg
093400140224     c*//                leavesr
093500140224     c                   EndSl
093600150114     c******             eval      wVMAsca = oPMGsca
093700150114     c                   eval      V1Csvm = oPMGsca
093800150114      * -?Decodifica scaglione?
093900150114     c                   clear                   V1Dsvm
094000150114     c                   eval      xx      = 1
094100150114     c                   eval      *in10   = *off
094200150114     c     V1Csvm        lookup    $IPG(xx)                               10
094300150114if  1c                   if        *in10
094400150114     c                   eval      V1Dsvm  = 'Valido da '
094500150114     c                                     + %trim(
094600150114     c                                         %editc( $IPGmin(xx) :
094700150114     c                                         '1') )
094800150114     c                                     + ' a '
094900150114     c                                     + %trim(
095000150114     c                                         %editc( $IPGmax(xx) :
095100150114     c                                         '1') )
095200150114     c                                     + ' Euro/Litro'
095300150114e   1c                   endif
095400060804      *
095500060804     c                   ENDSR
095600060804      *
095700060804      *---------------------------------------------------------------*
095800131216      *?Scrittura record in file TIDPB00F                            ?*
095900060804      *---------------------------------------------------------------*
096000131216     c     Wrt_TIDPB     BEGSR
096100060804      *
096200131216     c                   clear                   TIDPB000
096300060804      *
096400131216     c                   eval      DPBdtd = W1Cdtd
096500131216     c                   eval      DPBdpb = W1Cdpb
096600140115     c                   eval      DPBpma = V1Cpma
096700140220     c                   eval      DPBvma = V1Cvma
096800150114     c*****              eval      DPBsvm = wVMAsca
096900150114     c                   eval      DPBsvm = V1Csvm
097000131216     c                   eval      DPBute = KNMUS
097100131216     c                   eval      DPBdti = wDate
097200060804      *                  __________________
097300131216     c                   WRITE(e)  TIDPB000
097400060804      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097500060807     c                   if        %error
097600060807     c                   eval      $Video = '2'
097700060807     c                   seton                                        285190
097800140220     c                   eval      V1Dmsg = $Msg(10)
097900060807     c*** superfluo:     leavesr
098000060807     c                   endif
098100060807      *
098200060804     c                   ENDSR
098300060804
098400131216** -?$Msg?-?Messaggi di errore?----------------------------------------------*
098500070110Opzione errata                                                                 1
098600070110Data formalmente errata                                                        2
098700131216Data inizio validità NON succesiva all'ultima reperita:                        3
098800140226Data inizio validità precedente a quella attuale                               4
098900131216Data prezzo base inesistente                                                   5
099000140116Percentuale minima di applicazione errata                                      6
099100140116Percentuale minima di applicazione mancante: premere "Invio" per continuare    7
099200140220Valore minimo di applicazione errato                                           8
099300140220Valore minimo di applicazione mancante: premere "Invio" per continuare         9
099400140220Rilevati errori in fase di scrittura record nel file TIDPB00F                 10
