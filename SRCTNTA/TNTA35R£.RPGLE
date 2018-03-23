000100140127     /*END
000200140127      *===============================================================*
000300140127      *?TNTA35R * Manutenzione Tariffa Cartello AC Base              ?*
000400140127      *===============================================================*
000500140127
000600140127     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700140127
000800140127      *---------------------------------------------------------------*
000900140127
001000140127     fTABEL00F  if   e           k disk
001100140127      *
001200140127     fTIACB01L  if A e           k disk
001300140127      *
001400140127     fTNTA35D   cf   e             workstn
001500140127     f                                     infds(InfDspF)
001600140127     f                                     sfile(TA35S01:S01nrr)
001700140128     f                                     sfile(TA35S03:S03nrr)
001800140127
001900140127      *---------------------------------------------------------------*
002000140127
002100140127      *
002200140127      *?Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
002300140127      *
002400140128       // -?Tasti funzionali a video?
002500140128     d c_F01           c                   const(x'31')
002600140128     d c_F02           c                   const(x'32')
002700140128     d c_F03           c                   const(x'33')
002800140128     d c_F04           c                   const(x'34')
002900140128     d c_F05           c                   const(x'35')
003000140128     d c_F06           c                   const(x'36')
003100140128     d c_F07           c                   const(x'37')
003200140128     d c_F08           c                   const(x'38')
003300140128     d c_F09           c                   const(x'39')
003400140128     d c_F10           c                   const(x'3A')
003500140128     d c_F11           c                   const(x'3B')
003600140128     d c_F12           c                   const(x'3C')
003700140128     d c_F13           c                   const(x'B1')
003800140128     d c_F14           c                   const(x'B2')
003900140128     d c_F15           c                   const(x'B3')
004000140128     d c_F16           c                   const(x'B4')
004100140128     d c_F17           c                   const(x'B5')
004200140128     d c_F18           c                   const(x'B6')
004300140128     d c_F19           c                   const(x'B7')
004400140128     d c_F20           c                   const(x'B8')
004500140128     d c_F21           c                   const(x'B9')
004600140128     d c_F22           c                   const(x'BA')
004700140128     d c_F23           c                   const(x'BB')
004800140128     d c_F24           c                   const(x'BC')
004900140128     d c_Enter         c                   const(x'F1')
005000140128     d c_RollDown      c                   const(x'F4')
005100140128     d c_RollUp        c                   const(x'F5')
005200140127      * -?Nr. di righe del sfl (SFLPAG)?
005300140128     d c_SflPag        c                   const(15)
005400140127      *
005500140127      *?Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
005600140127      *
005700140127      * -?Messaggi di errore?
005800140127     d sk_Msg          s             78    dim( 8)  ctdata  perrcd(1)
005900140127      *
006000140127      * -?Codici Importanza Cliente?
006100140128     d c_elem_IC       c                   const(10)
006200140128     d sk_IC_cod       s                   like(ACBclv)
006300140128     d                                     dim(c_elem_IC)  inz
006400140128     d sk_IC_des       s                   like(dsIC.§sICde)
006500140128     d                                     dim(c_elem_IC)  inz
006600140128     d sk_IC_ord       s                   like(dsIC.§sICor)
006700140128     d                                     dim(c_elem_IC)  inz
006800140127      *
006900140127      *?Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
007000140127      *
007100140127     d KPJBA         e ds
007200140127      *
007300140127      * -?Parametri x Controllo profilo utenti?
007400140127     d TIBS34ds      e ds
007500140127      * -?Ds di riferimento al file esterno AZUTE00F?
007600140127     d AZUTEds       e ds                  extname(AZUTE00F)
007700140127      * -?Ds per dati organigramma?
007800140127     d dDatiUte      e ds
007900140127      *
008000140127      * -?Tab. "IC" = Importanza Cliente?
008100140127     d dsIC          e ds                  inz   qualified
008200140127      *
008300140127      * -?Status ds?
008400140127     d Status         sds           333
008500140127     d   SDSpgm          *proc
008600140127      *
008700140127      * -?InfDs del file video?
008800140127     d InfDspF         ds           512
008900140127     d   £Tasto              369    369
009000140127     d   £SFLnrr             378    379b 0
009100140127      *
009200140127      * -?Parametri per controllo data e transform in giorni - XSRDA8 -?
009300140127     d WLBdat          ds                  inz
009400140127     d   G08dat                       8  0 inz
009500140127     d   G08inv                       8  0 inz
009600140127     d   G08err                       1    inz('3')
009700140127     d   G08tgi                       5  0 inz
009800140127      *
009900140127      *?Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*?
010000140127      *
010100140127      * -?Flags booleani?
010200140127     d $Fine           s               n   inz(*off)
010300140127     d $F10            s               n   inz(*off)
010400140127     d $Bypass         s               n   inz(*off)
010500140127     d $InzS01         s               n   inz(*on)
010600140127     d $InzD02         s               n   inz(*on)
010700140128     d $InzS03         s               n   inz(*on)
010800140127      *
010900140127     d $Video          s              1    inz('1')
011000140127      *
011100140127      * -?Indici di schiere?
011200140127     d xx              s              3  0 inz
011300140127      *
011400140127      * -?Campi associati al SFL?
011500140127     d S01nrr          s                   like(C01rcd)  inz
011600140127     d SavS01csr       s                   like(C01rcd)  inz
011700140128     d S03nrr          s                   like(C03rcd)  inz
011800140128     d SavS03csr       s                   like(C03rcd)  inz
011900140128     d MaxS03nrr       s                   like(C03rcd)  inz
012000140127     d W_SflPag        s              3  0 inz
012100140127     d wPag            s              4  0 inz
012200140127     d wRig            s              3  0 inz
012300140127      *
012400140127      * -?Altri campi?
012500140127     d wDate           s              8  0 inz
012600140127     d W1Cdtd          s                   inz  like(ACBdtd)
012700140127      *
012800140127     d k_TBLkut        s                   like(TBLkut)  inz(1)
012900140127     d k_TBLcod        s                   like(TBLcod)  inz('IC')
013000140128     d k_ACBclv        s                   like(ACBclv)  inz
013100140128     d k_ACBdtd        s                   like(ACBdtd)  inz
013200140127      *
013300140127      *?Chiavi di Lettura (Key-List) - - - - - - - - - - - - - - - - -*?
013400140127      *
013500140127     c     k02tabel00    klist
013600140127     c                   kfld                    k_TBLkut
013700140127     c                   kfld                    k_TBLcod
013800140128     c     k02tiacb01    klist
013900140128     c                   kfld                    k_ACBclv
014000140128     c                   kfld                    k_ACBdtd
014100140127
014200140127      *---------------------------------------------------------------*
014300140127      *?I N D I C A T O R I                                          ?*
014400140127      *---------------------------------------------------------------*
014500140127      *? 06?    - Abilitato F6=Conferma in D02
014600140127      *? 10?    - Comodo                                              *
014700140127      *? 28?    - Emissione messaggio di errore a video               *
014800140127      *? 30?    - Pulizia del subfile                                 *
014900140127      *? 31?    - NON emissione del subfile                           *
015000140127      *? 32?    - Record modificato nel subfile (SflNxtChg)           *
015100140127      *? 33?    - Fine dati nel subfile         (SflEnd)              *
015200140127      *? 50?    - Opzione errata                                      *
015300140127      *? 51?    - Codice Importanza Cliente errato                    *
015400140127      *? 52?    - Data Inizio Validità errata                         *
015500140127      *? 53?    - Importo Tariffa errato                              *
015600140127      *? 90?    - Generico di errore                                  *
015700140127      *---------------------------------------------------------------*
015800140127
015900140127     c     *Entry        plist
016000140127     c                   parm                    KPJBA
016100140127      *
016200140127      * -?Operazioni Iniziali?
016300140127     c                   exsr      sr_RoutInz
016400140127      *
016500140127      * -?Gestione Video?
016600140127do  1c                   dow       $Fine   = *off
016700140127     c     $Video        caseq     '1'           sr_GesS01
016800140128     c     $Video        caseq     '2'           sr_GesD02
016900140128     c     $Video        caseq     '3'           sr_GesS03
017000140127     c                   endcs
017100140127e   1c                   enddo
017200140127      *
017300140127      * -?Fine?
017400140127     c                   movel     *on           *inLR
017500140127      *
017600140127      *---------------------------------------------------------------*
017700140127      *?Operazioni Iniziali                                          ?*
017800140127      *---------------------------------------------------------------*
017900140127     c     sr_RoutInz    BEGSR
018000140127      *
018100140127      * -?Reperimento dati job?
018200140127     c                   exsr      sr_DatiJob
018300140127      *
018400140127     c                   movel     SDSpgm        V1Tpgm
018500140127     c                   clear                   WLBdat
018600140127     c                   eval      G08dat  = *date
018700140127     c                   call      'XSRDA8'
018800140127     c                   parm                    WLBdat
018900140127     c                   movel     G08inv        wDate
019000140127      *
019100140127      * -?Caricamento tab. IC = Importanza Cliente?
019200140127     c                   clear                   xx
019300140127     c     k02tabel00    setll     TABEL
019400140127     c     k02tabel00    reade     TABEL
019500140127do  1c                   DoW       Not %eof(TABEL00F)
019600140127if  2c                   if        TBLflg = *blank
019700140127     c                   add       1             xx
019800140127     c                   eval      dsIC = TBLuni
019900140127     c                   eval      sk_IC_cod(xx) = TBLkey
020000140127     c                   eval      sk_IC_des(xx) = dsIC.§sICde
020100140127     c                   eval      sk_IC_ord(xx) = dsIC.§sICor
020200140127e   2c                   endif
020300140127     c     k02tabel00    reade     TABEL
020400140127e   1c                   EndDo
020500140127      *
020600140127     c                   ENDSR
020700140127      *
020800140127      *---------------------------------------------------------------*
020900140127      *?Reperimento Dati del job (Utente/Operativi)                  ?*
021000140127      *---------------------------------------------------------------*
021100140127     c     sr_DatiJob    BEGSR
021200140127      *
021300140127     c     *dtaara       define    §azute        azuteds
021400140127     c     *dtaara       define    §datiute      ddatiute
021500140127      *
021600140127     c                   in(E)     *dtaara
021700140127     c                   IF        %ERROR or RSUT = *blanks
021800140127     c                   clear                   Tibs34Ds
021900140127     c                   call      'TIBS34R'
022000140127     c                   parm                    Tibs34Ds
022100140127     c                   in        *dtaara
022200140127     c                   ENDIF
022300140127      *
022400140127     c                   ENDSR
022500140127      *
022600140127      *---------------------------------------------------------------*
022700140127      *?Gestione videata C01/S01                                     ?*
022800140127      *---------------------------------------------------------------*
022900140127     c     sr_GesS01     BEGSR
023000140127      *
023100140127      * -?Inizializzazione della videata?
023200140127if  1c                   if        $InzS01 = *on
023300140127     c                   exsr      sr_InzS01
023400140127     c                   eval      $InzS01     = *off
023500140127e   1c                   endif
023600140127      *
023700140127      * -?Posizionameno del cursore?
023800140127if  1c                   if        C01csr  > *zeros
023900140127     c                   z-add     C01csr        C01rcd
024000140127x   1c                   else
024100140127     c                   z-add     1             C01rcd
024200140127e   1c                   endif
024300140127      *
024400140127      * -?Emissione Testata e Piede con tasti funzionali abilitati?
024500140127     c                   write     TA35T01
024600140127     c                   write     TA35P01
024700140127      * -?Emissione videata?
024800140127     c                   exfmt     TA35C01
024900140127     c                   setoff                                       28  90
025000140127     c                   clear                   V1Dmsg
025100140127      *
025200140127sel 1c                   SELECT
025300140127      * -?F3=Fine?
025400140127w   1c                   WHEN      *inKC
025500140127     c                   exsr      sr_F03S01
025600140127      * -?F10=Inserimento?
025700140127w   1c                   WHEN      *inKJ
025800140127     c                   exsr      sr_F10S01
025900140127      * -?Roll-UP?
026000140128w   1c                   WHEN      £Tasto      = c_RollUp
026100140127     c                   exsr      sr_RollUpS01
026200140127      *
026300140127x   1c                   OTHER
026400140127      * -?Gestione Opzioni?
026500140127     c                   exsr      sr_OpzS01
026600140127if  2c                   if        *in90
026700140127     c                   leavesr
026800140127e   2c                   endif
026900140127      *
027000140127e   1c                   ENDSL
027100140127      *
027200140127     c                   ENDSR
027300140127      *
027400140127      *---------------------------------------------------------------*
027500140127      *?Inizializzazione videata C01/S01                             ?*
027600140127      *---------------------------------------------------------------*
027700140127     c     sr_InzS01     BEGSR
027800140127      *
027900140127      * -?Pulizia subfile?
028000140127     c                   seton                                        3031
028100140127     c                   write     TA35C01
028200140127     c                   setoff                                         3133
028300140127      *
028400140128     c                   clear                   W_SflPag
028500140127     c                   clear                   C01rcd
028600140127     c                   clear                   C01csr
028700140127     c                   clear                   S01nrr
028800140127     c                   clear                   V1Dmsg
028900140127     c                   setoff                                       28  90
029000140127     c                   movea     *zeros        *in(51)
029100140127      *
029200140127      * -?Caricamento dati dal file TIACB00F?
029300140127     c     *loval        setgt     TIACB000
029400140127     c                   read      TIACB000
029500140127      *
029600140127     c                   exsr      sr_RollUpS01
029700140127      *
029800140127     c                   ENDSR
029900140127      *
030000140127      *---------------------------------------------------------------*
030100140127      *?Caricamento pagina successiva di dati                        ?*
030200140127      *---------------------------------------------------------------*
030300140127     c     sr_RollUpS01  BEGSR
030400140127      *
030500140127     c                   eval      S01nrr  = (W_SflPag * c_SflPag)
030600140127     c                   add       1             W_SflPag
030700140127     c                   eval      *in32   = *off
030800140127      *
030900140127      * -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
031000140127do  1c                   DOW       NOT  %eof(TIACB01L)
031100140127     c                             and  S01nrr < (W_SflPag * c_SflPag)
031200140127      *
031300140127     c                   exsr      sr_RieS01
031400140127     c                   add       1             S01nrr
031500140127     c                   write     TA35S01
031600140127      *
031700140127     c                   read      TIACB000
031800140127      *
031900140127e   1c                   ENDDO
032000140127      *
032100140127      * -?Visualizzazione del SFL (se ci sono dati)?
032200140127     c                   eval      *in30   = (S01nrr = *zeros)
032300140127      *
032400140127      * -?Attivazione (eventuale) del SFLEND?
032500140127     c                   eval      *in33   = %eof(TIACB01L)
032600140127      *
032700140127      * -?Posizionamento cursore al 1° rcd della pagina?
032800140127if  1c                   if        S01nrr  > *zeros
032900140127     c     S01nrr        div       c_SflPag      wPag
033000140127     c                   mvr                     wRig
033100140127     c                   eval      C01rcd  = wPag * c_SflPag
033200140127     c                   if        wRig    > *zeros
033300140127     c                   eval      C01rcd  = C01rcd + 1
033400140127     c                   else
033500140127     c                   eval      C01rcd  = C01rcd - c_SflPag + 1
033600140127     c                   endif
033700140127x   1c                   else
033800140127     c                   clear                   C01rcd
033900140127e   1c                   endif
034000140127     c                   eval      C01csr  = C01rcd
034100140127      *
034200140127     c                   ENDSR
034300140127      *
034400140127      *---------------------------------------------------------------*
034500140127      *?Impostazione dati in singolo record del sfl S01              ?*
034600140127      *---------------------------------------------------------------*
034700140127     c     sr_RieS01     BEGSR
034800140127      *
034900140127     c                   clear                   TA35S01
035000140127      *
035100140127     c                   eval      xx = %lookup( ACBclv : sk_IC_cod )
035200140127      *
035300140127      * -?Campi hidden?
035400140127     c                   eval      H1Cdtd  = ACBdtd
035500140127     c                   if        xx > *zero
035600140127     c                   eval      H1CicOr = sk_IC_ord(xx)
035700140127     c                   endif
035800140127      *
035900140127      * -?Importanza?
036000140127     c                   eval      S1Cclv = ACBclv
036100140127     c                   if        xx > *zero
036200140127     c                   eval      S1Dclv = sk_IC_des(xx)
036300140127     c                   else
036400140127     c                   eval      S1Dclv = *all'? '
036500140127     c                   endif
036600140127      *
036700140127      * -?Data Decorrenza?
036800140127     c                   reset                   WLBdat
036900140127     c                   eval      G08inv = ACBdtd
037000140127     c                   call      'XSRDA8'
037100140127     c                   parm                    WLBdat
037200140127     c                   eval      S1Cdtd = G08dat
037300140127      *
037400140127      * -?Importo?
037500140127     c                   eval      S1Citr = ACBitr
037600140127      *
037700140127      * -?Profilo utente inserimento?
037800140127     c                   eval      S1Cute = ACBute
037900140127      *
038000140127      * -?Data inserimento?
038100140127     c                   reset                   WLBdat
038200140127     c                   eval      G08inv  = ACBdti
038300140127     c                   call      'XSRDA8'
038400140127     c                   parm                    WLBdat
038500140127     c                   eval      S1Cdti  = G08dat
038600140127      *
038700140127     c                   ENDSR
038800140127      *
038900140127      *---------------------------------------------------------------*
039000140127      *?Gestione tasto funzionale F03 da videata S01                 ?*
039100140127      *---------------------------------------------------------------*
039200140127     c     sr_F03S01     BEGSR
039300140127      *
039400140127      * -?Chiusura del programma?
039500140127     c                   eval      $Fine   = *on
039600140127      *
039700140127     c                   ENDSR
039800140127      *
039900140127      *---------------------------------------------------------------*
040000140127      *?Gestione tasto funzionale F10 da videata S01                 ?*
040100140127      *---------------------------------------------------------------*
040200140127     c     sr_F10S01     BEGSR
040300140127      *
040400140128      * -?Passaggio alla videata di immissione (D02)?
040500140127     c                   eval      $Video  = '2'
040600140127     c                   eval      $InzD02 = *on
040700140128     c                   eval      $InzS03 = *on
040800140127      *
040900140127     c                   eval      $F10    = *on
041000140127      *
041100140127     c                   ENDSR
041200140127      *
041300140127      *---------------------------------------------------------------*
041400140127      *?Gestione opzioni subfile                                     ?*
041500140127      *---------------------------------------------------------------*
041600140127     c     sr_OpzS01     BEGSR
041700140127      *
041800140127     c                   clear                   SavS01csr
041900140127      *
042000140127     c                   readc     TA35S01
042100140127      *
042200140127do  1c                   DOW       NOT %eof(TNTA35D)
042300140127      *
042400140127     c                   eval      *in32   = *off
042500140127     c                   movea     *zeros        *in(51)
042600140127     c                   z-add     S01nrr        C01rcd
042700140127      *
042800140127sel 2c                   SELECT
042900140127      * -?Nessuna opzione?
043000140127w   2c                   WHEN      S1Copz  = *blanks
043100140127      * -?5 = Visualizzazione prezzo medio gasolio?
043200140127w   2c                   WHEN      S1Copz  = '5'
043300140127     c                   exsr      sr_OpzS1_5
043400140127      * -?? = Opzione NON valida?
043500140127x   2c                   OTHER
043600140127     c                   seton                                        285190
043700140127     c                   eval      V1Dmsg  = sk_Msg(01)
043800140127e   2c                   ENDSL
043900140127      *
044000140127      * -?Memorizzazione nrr del sfl con la 1ª opzione?
044100140127      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
044200140127if  1c                   if             S1Copz   <> *blanks
044300140127     c                             and (SavS01csr = *zeros
044400140127     c                              or  SavS01csr < C01rcd)
044500140127     c                   eval      SavS01csr   = C01rcd
044600140127e   1c                   endif
044700140127      *
044800140127      * -?Aggiornamento sfl?
044900140127sel 2c                   select
045000140127w   2c                   when      *in28
045100140127     c                   eval      *in32       = *on
045200140127     c                   z-add     C01rcd        C01csr
045300140127w   2c                   when      *in90       = *on
045400140127     c                   z-add     C01rcd        C01csr
045500140127     c                   clear                   S1Copz
045600140127x   2c                   other
045700140127     c                   clear                   S1Copz
045800140127e   2c                   endsl
045900140127     c                   UPDATE    TA35S01
046000140127if  2c                   if        *in28  OR  *in90
046100140127     c                   leave
046200140127e   2c                   endif
046300140127      *
046400140127     c                   readc     TA35S01
046500140127      *
046600140127e   1c                   ENDDO
046700140127      *
046800140127      * -?Riposiziono il cursore sul record contenente la prima opzione?
046900140127      *  ?(se non sono stati rilevati errori)?
047000140127if  1c                   if        NOT *in28  and  NOT *in90
047100140127     c                             and SavS01csr  > *zeros
047200140127     c                   z-add     SavS01csr     C01csr
047300140127e   1c                   endif
047400140127      *
047500140127     c                   ENDSR
047600140127      *
047700140127      *---------------------------------------------------------------*
047800140127      *?S1_Opz.5 = Visualizzazione singola Tariffa Cartello AC base  ?*
047900140127      *---------------------------------------------------------------*
048000140127     c     sr_OpzS1_5    BEGSR
048100140127      *
048200140127      * -?Impostazione dati in D02 (solo visualizzato - protetto)?
048300140127     c                   clear                   TA35D02
048400140127     c                   eval      V1Cdtd  = S1Cdtd
048500140127      *
048600140127      * -?Passaggio alla videata D03 per visualizzazione singola?
048700140128      *  ?tariffa cartello AC base?
048800140127     c                   eval      $F10    = *off
048900140128     c                   eval      $Video  = '3'
049000140128     c                   eval      $InzS03 = *on
049100140127      *
049200140128do  1c                   doW       $Video  = '3'
049300140128     c                   exsr      sr_GesS03
049400140127e   1c                   enddo
049500140127      *
049600140127     c                   ENDSR
049700140127      *
049800140127      *---------------------------------------------------------------*
049900140127      *?Gestione videata D02                                         ?*
050000140127      *---------------------------------------------------------------*
050100140127     c     sr_GesD02     BEGSR
050200140127      *
050300140127      * -?Inizializzazione videata?
050400140127if  1c                   if        $InzD02 = *on
050500140127     c                   exsr      sr_InzD02
050600140128     c                   movel     *off          $InzD02
050700140127e   1c                   endif
050800140127      *
050900140127      * -?Emissione Testata e Piede con tasti funzionali abilitati?
051000140127      *  ?oltre alla videata precedente?
051100140127     c                   eval      *in03   = *off
051200140127     c                   eval      *in06   = *off
051300140127     c                   write     TA35T01
051400140128     c                   write     TA35P02
051500140127      * -?Emissione videata?
051600140127     c                   exfmt     TA35D02
051700140127     c                   setoff                                       28  90
051800140127     c                   clear                   V1Dmsg
051900140127      *
052000140127sel 1c                   SELECT
052100140127      * -?F12=Ritorno?
052200140127w   1c                   WHEN      *inKL
052300140127     c                   exsr      sr_F12D02
052400140127      *
052500140127x   1c                   OTHER
052600140127     c                   exsr      sr_CtrD02
052700140127      *
052800140127if  2c                   if        *in90
052900140127     c                   leavesr
053000140127e   2c                   endif
053100140127      *
053200140128     c                   eval      $InzS03 = *on
053300140128     c                   eval      $Video  = '3'
053400140127      *
053500140127e   1c                   ENDSL
053600140127      *
053700140127     c                   ENDSR
053800140127      *
053900140127      *---------------------------------------------------------------*
054000140127      *?Inizializzazione videata D02                                 ?*
054100140127      *---------------------------------------------------------------*
054200140127     c     sr_InzD02     BEGSR
054300140127      *
054400140127     c                   clear                   TA35D02
054500140127      *
054600140127     c                   ENDSR
054700140127      *
054800140127      *---------------------------------------------------------------*
054900140127      *?Gestione tasto funzionale F12 da videata D02                 ?*
055000140127      *---------------------------------------------------------------*
055100140127     c     sr_F12D02     BEGSR
055200140127      *
055300140127      * -?Ritorno alla videata precedente?
055400140128     c                   eval      $Video  = '1'
055500140127      *
055600140127     c                   ENDSR
055700140127      *
055800140127      *---------------------------------------------------------------*
055900140127      *?Controllo dati immessi in videata D02                        ?*
056000140127      *---------------------------------------------------------------*
056100140127     c     sr_CtrD02     BEGSR
056200140127      *
056300140127     c                   movea     *zeros        *in(51)
056400140127      *
056500140127      * -?Controllo correttezza data inizio validità - in INSERIMENTO?
056600140127      *  ?(questo controllo viene eseguito SOLO in inserimento - $F10):?
056700140127      *
056800140127      * -?Controllo formale data inizio validità?
056900140127     c                   clear                   WLBdat
057000140127if  1c                   if        V1Cdtd  > *zeros
057100140127     c                   eval      G08dat  = V1Cdtd
057200140127     c                   call      'XSRDA8'
057300140127     c                   parm                    WLBdat
057400140127x   1c                   else
057500140127     c                   eval      G08err  = *on
057600140127e   1c                   endif
057700140127if  1c                   if        G08err  = *on
057800140127     c                   seton                                        285190
057900140127     c                   eval      V1Dmsg  = sk_Msg(03)
058000140127     c                   leavesr
058100140127e   1c                   endif
058200140127     c                   eval      V1Cdtd  = G08dat
058300140127     c                   eval      W1Cdtd  = G08inv
058400140127      *
058500140127      * -?DEVE essere più recente dell'ultima già inserita in TIACB00F?
058600140128for 1c                   For       xx = 1  To  %elem(sk_IC_cod)
058700140128if  2c                   if        sk_IC_cod(xx) = *blank
058800140128     c                   leave
058900140128e   2c                   endif
059000140128     c     sk_IC_cod(xx) chain     TIACB000
059100140128if  2c                   if             %found(TIACB01L)
059200140127     c                             and  ACBdtd >= W1Cdtd
059300140127     c                   reset                   WLBdat
059400140127     c                   eval      G08inv  = ACBdtd
059500140127     c                   call      'XSRDA8'
059600140127     c                   parm                    WLBdat
059700140127     c                   seton                                        285190
059800140127     c                   eval      V1Dmsg  = %trim(sk_Msg(04))
059900140127     c                                     + ' '
060000140127     c                                     + %editc(G08dat:'Y')
060100140127     c                   leavesr
060200140128e   2c                   endif
060300140128e   1c                   EndFor
060400140127      *
060500140127      * -?NON può essere futura?
060600140128if  1c*//                if        W1Cdtd  > Wdate
060700140128     c*//                reset                   WLBdat
060800140128     c*//                seton                                        285190
060900140128     c*//                eval      V1Dmsg  = sk_Msg(05)
061000140128     c*//                leavesr
061100140128e   1c*//                endif
061200140127      *
061300140127     c                   ENDSR
061400140128      *
061500140128      *---------------------------------------------------------------*
061600140128      *?Gestione videata C03/S03                                     ?*
061700140128      *---------------------------------------------------------------*
061800140128     c     sr_GesS03     BEGSR
061900140128      *
062000140128      * -?Inizializzazione della videata?
062100140128if  1c                   if        $InzS03 = *on
062200140128     c                   exsr      sr_InzS03
062300140128     c                   eval      $InzS03     = *off
062400140128e   1c                   endif
062500140128      *
062600140128      * -?Posizionameno del cursore?
062700140128if  1c                   if        C03csr  > *zeros
062800140128     c                   z-add     C03csr        C03rcd
062900140128x   1c                   else
063000140128     c                   z-add     1             C03rcd
063100140128e   1c                   endif
063200140128      *
063300140128     c                   if        Not *in28
063400140128      * -?Emissione Testata e Piede con tasti funzionali abilitati?
063500140128     c                   write     TA35T01
063600140128     c                   write     TA35P02
063700140128      * -?Emissione videata con data inizio validità?
063800140128     c                   write     TA35D02
063900140128     c                   endif
064000140128      * -?Emissione videata?
064100140128     c                   write     TA35D02
064200140128if  1c                   if        $F10    = *off
064300140128     c                   write     TA35C03
064400140128     c                   exfmt     Protect
064500140128     c                   else
064600140128     c                   write     Protect
064700140128     c                   exfmt     TA35C03
064800140128     c                   endif
064900140128      *
065000140128     c                   setoff                                       28  90
065100140128     c                   clear                   V1Dmsg
065200140128      *
065300140128     c                   Select
065400140128      *
065500140128      * -?F3=Fine?
065600140128     c                   When      £Tasto = c_F03
065700140128     c                   exsr      sr_F03S03
065800140128      *
065900140128      * -?F12=Ritorno?
066000140128     c                   When      £Tasto = c_F12
066100140128     c                   exsr      sr_F12S03
066200140128      *
066300140128     c                   Other
066400140128      * -?Controllo dati?
066500140128     c                   exsr      sr_CtrS03
066600140128if  2c                   if        *in90
066700140128     c                   leavesr
066800140128e   2c                   endif
066900140128      * -?F6=Conferma?
067000140128if  2c                   if        £Tasto = c_F06
067100140128     c                   exsr      sr_Wrt_TIACB
067200140128if  3c                   if        Not *in90
067300140128     c                   eval      $Video  = '1'
067400140128     c                   eval      $InzS01 = *on
067500140128e   3c                   endif
067600140128e   2c                   endif
067700140128      *
067800140128     c                   EndSl
067900140128      *
068000140128     c                   ENDSR
068100140128      *
068200140128      *---------------------------------------------------------------*
068300140128      *?Inizializzazione videata C03/S03                             ?*
068400140128      *---------------------------------------------------------------*
068500140128     c     sr_InzS03     BEGSR
068600140128      *
068700140128      * -?Pulizia subfile?
068800140128     c                   seton                                        3435
068900140128     c                   write     TA35C03
069000140128     c                   setoff                                         3537
069100140128      *
069200140128     c                   clear                   C03rcd
069300140128     c                   clear                   C03csr
069400140128     c                   clear                   S03nrr
069500140128     c                   clear                   V1Dmsg
069600140128     c                   setoff                                       28  90
069700140128     c                   movea     *zeros        *in(51)
069800140128     c                   reset                   $Bypass
069900140128      *
070000140128      * -?Caricamento dati dal file TIACB00F?
070100140128for 1c                   For       S03nrr = 1  To  %elem(sk_IC_cod)
070200140128if  2c                   if        sk_IC_cod(S03nrr) = *blank
070300140128     c                   leave
070400140128e   2c                   endif
070500140128     c                   eval      *in36  = *off
070600140128     c                   exsr      sr_RieS03
070700140128     c                   write     TA35S03
070800140128e   1c                   EndFor
070900140128     c                   eval      MaxS03nrr = S03nrr
071000140128      *
071100140128      * -?Visualizzazione del SFL (se ci sono dati)?
071200140128     c                   eval      *in34   = (S03nrr = *zeros)
071300140128      *
071400140128      * -?Attivazione del SFLEND (già caricato tutto)?
071500140128     c                   eval      *in37   = *on
071600140128      *
071700140128      * -?Posizionamento cursore al 1° rcd del subfile?
071800140128if  1c                   if        S03nrr  > *zeros
071900140128     c                   eval      C01rcd  = 1
072000140128x   1c                   else
072100140128     c                   clear                   C01rcd
072200140128e   1c                   endif
072300140128     c                   eval      C01csr  = C01rcd
072400140128      *
072500140128      * -?Impostazione indicatori:?
072600140128      *  ?*in03?= abilita F3 per ritorno a S01 senza proseguire con?
072700140128      *          ?l'elaborazione delle opzioni successive?
072800140128      *          ?In fase di immissione consente il ritorno al subfile?
072900140128      *          ?senza passare dalla videata D01...?
073000140128     c                   eval      *in03 = *on
073100140128      * -?*in06?= abilita F6 per inserimento record?
073200140128     c                   eval      *in06 = ($F10  = *on)
073300140128      *
073400140128     c                   ENDSR
073500140128      *
073600140128      *---------------------------------------------------------------*
073700140128      *?Impostazione dati in singolo record del sfl S03              ?*
073800140128      *---------------------------------------------------------------*
073900140128     c     sr_RieS03     BEGSR
074000140128      *
074100140128     c                   clear                   TA35S03
074200140128      *
074300140128      * -?Importanza?
074400140128     c                   eval      S3Cclv = sk_IC_cod(S03nrr)
074500140128     c                   eval      S3Dclv = sk_IC_des(S03nrr)
074600140128      *
074700140128      * -?Se è stata richiesta l'opzione 5=Visualizzazione?
074800140128      *  ?si reperiscono i dati da visualizzare dal file?
074900140128if  1c                   If             S1Copz = '5'
075000140128     c                             and  $F10   = *off
075100140128      *
075200140128     c                   eval      k_ACBclv = sk_IC_cod(S03nrr)
075300140128     c                   eval      k_ACBdtd = H1Cdtd
075400140128     c     k02tiacb01    chain     TIACB000
075500140128if  2c                   if        %found(TIACB01L)
075600140128      *
075700140128      * -?Importo?
075800140128     c                   eval      S3Citr = ACBitr
075900140128      * -?Utente inserimento?
076000140128     c                   eval      S3Cute = ACBute
076100140128      * -?Data inserimento?
076200140128     c                   reset                   WLBdat
076300140128     c                   eval      G08inv = ACBdti
076400140128     c                   call      'XSRDA8'
076500140128     c                   parm                    WLBdat
076600140128     c                   eval      S3Cdti = G08dat
076700140128      *
076800140128e   2c                   endif
076900140128      * -?Impostazione indicatori x attributi video?
077000140128      * -?*in41?= Visualizzazione?
077100140128if  1c                   eval      *in41 = (S1Copz = '5'  and  Not $F10)
077200140128      * -?*in42?= evidenzia il Codice Importanza selezionato?
077300140128     c                   eval      *in42 = (S3Cclv = S1Cclv)
077400140128      *
077500140128e   1c                   EndIf
077600140127      *
077700140127     c                   ENDSR
077800140127      *
077900140127      *---------------------------------------------------------------*
078000140128      *?Gestione tasto funzionale F03 da videata D03                 ?*
078100140127      *---------------------------------------------------------------*
078200140128     c     sr_F03S03     BEGSR
078300140127      *
078400140128      * -?Ritorno al 1° subfile?
078500140128     c                   eval      $Video = '1'
078600140128      *
078700140128      * -?Se visualizzazione: blocco elaborazione opzioni?
078800140128if  1c                   if             S1Copz = '5'
078900140128     c                             and  $F10   = *off
079000140128     c                   eval      *in90  = *on
079100140128e   1c                   endif
079200140127      *
079300140127     c                   ENDSR
079400140127      *
079500140127      *---------------------------------------------------------------*
079600140128      *?Gestione tasto funzionale F12 da videata S03                 ?*
079700140127      *---------------------------------------------------------------*
079800140128     c     sr_F12S03     BEGSR
079900140127      *
080000140128      * -?Se visualizzazione: ritorno al 1° subfile?
080100140128if  1c                   If             S1Copz = '5'
080200140128     c                             and  $F10   = *off
080300140128     c                   eval      $Video = '1'
080400140128      * -?Se inserimento: ritorno alla videata precedente?
080500140128x   1c                   else
080600140128     c                   eval      $Video = '2'
080700140128e   1c                   endif
080800140127      *
080900140127     c                   ENDSR
081000140127      *
081100140127      *---------------------------------------------------------------*
081200140128      *?Controllo dati immessi in videata S03                        ?*
081300140127      *---------------------------------------------------------------*
081400140128     c     sr_CtrS03     BEGSR
081500140127      *
081600140128     c                   clear                   SavS03csr
081700140128      *
081800140128      * -?Ciclo di lettura subfile?
081900140128     c                   readc     TA35S03
082000140128      *
082100140128     c                   DoW       Not %eof(TNTA35D)
082200140128      *
082300140128     c                   eval      *in36  = *off
082400140128     c                   movea     *zeros        *in(51)
082500140128     c                   eval      C03rcd = S03nrr
082600140127      *
082700140127      * -?Controllo importo tariffa?
082800140128sel 1c                   Select
082900140128w   1c                   When      S3Citr  <  *zeros  or
083000140128     c                             S3Citr  >= *hival
083100140127     c                   seton                                        285390
083200140127     c                   eval      V1Dmsg  = sk_Msg(06)
083300140128w   1c*//                When      S03itr  = *zeros  and
083400140128     c*//                          Not $Bypass
083500140128     c*//                eval      $Bypass = *on
083600140128     c*//                seton                                        285390
083700140128     c*//                eval      V1Dmsg  = sk_Msg(07)
083800140128e   1c                   EndSl
083900140128      *
084000140128      * -?Memorizzazione nrr del sfl con il 1° importo per?
084100140128      *  ?riposizionarvici il cursore dopo il tasto "Invio"?
084200140128     c                   if         S3Citr   <> *zero  and
084300140128     c                             (SavS03csr = *zero  or
084400140128     c                              SavS03csr < C03rcd)
084500140128     c                   eval      SavS03csr   = C03rcd
084600140128     c                   endif
084700140128      *
084800140128      * -?Aggiornamento sfl?
084900140128     c                   select
085000140128     c                   when      *in28  or  S1Copz <> *zero
085100140128     c                   eval      *in36  = *on
085200140128     c                   eval      C03csr = C03rcd
085300140128     c                   when      *in90
085400140128     c                   eval      C03csr = C03rcd
085500140128     c                   endsl
085600140128      *
085700140128     c                   UPDATE    TA35S03
085800140128      *
085900140128     c                   if        *in28  or  *in90
086000140128     c                   leave
086100140128     c                   endif
086200140128      *
086300140128     c                   readc     TA35S03
086400140128      *
086500140128     c                   ENDDO
086600140128      *
086700140128      * -?Riposizionam. cursore sul record contenente la prima opz.?
086800140128      *  ?(se non sono stati rilevati errori)?
086900140128     c                   if        NOT *in28  and  NOT *in90  and
087000140128     c                             SavS03csr > *zero
087100140128     c                   eval      C03csr = SavS03csr
087200140128     c                   endif
087300140127      *
087400140127     c                   ENDSR
087500140127      *
087600140127      *---------------------------------------------------------------*
087700140127      *?Scrittura record in file TIACB00F                            ?*
087800140127      *---------------------------------------------------------------*
087900140127     c     sr_Wrt_TIACB  BEGSR
088000140127      *
088100140128for 1c                   For       S03nrr = 1  To  MaxS03nrr
088200140128      *
088300140128     c     S03nrr        chain     TA35S03
088400140128      *
088500140128if  2c                   if        Not %found(TNTA35D)
088600140128     c                   iter
088700140128e   2c                   endif
088800140128      *
088900140128if  2c                   if        S3Citr = *zeros
089000140128     c                   iter
089100140128e   2c                   endif
089200140128      *
089300140127     c                   clear                   TIACB000
089400140127      *
089500140127     c                   eval      ACBdtd = W1Cdtd
089600140128     c                   eval      ACBclv = S3Cclv
089700140128     c                   eval      ACBitr = S3Citr
089800140127     c                   eval      ACBute = KNMUS
089900140127     c                   eval      ACBdti = wDate
090000140127      *                  __________________
090100140127     c                   WRITE(e)  TIACB000
090200140127      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
090300140128if  2c                   if        %error
090400140128     c                   eval      $Video = '3'
090500140128     c                   eval      C03rcd = S03nrr
090600140128     c                   eval      C03csr = C03rcd
090700140127     c                   seton                                        285190
090800140127     c                   eval      V1Dmsg = sk_Msg(08)
090900140128     c                   leavesr
091000140128e   2c                   endif
091100140127      *
091200140128     c                   EndFor
091300140128      *
091400140127     c                   ENDSR
091500140127
091600140127** -?sk_Msg?-?Messaggi di errore?--------------------------------------------*
091700140127Opzione errata                                                                 1
091800140128Codice Importanza Cliente errato                                      -LIBERO- 2
091900140127Data formalmente errata                                                        3
092000140127Data inizio validità NON succesiva all'ultima reperita:                        4
092100140128Non ammesse decorrenze "future"                                       -LIBERO- 5
092200140127Importo tariffa errato                                                         6
092300140128Importo tariffa mancante: premere "Invio" per continuare              -LIBERO- 7
092400140127Rilevati errori in fase di scrittura record nel file TIACB00F                  8
