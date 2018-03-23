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
001200140226     fTIACB02L  if A e           k disk
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
006200140227     d sk_IC           s                   like(wIC_ds)    descend
006300140227     d                                     dim(c_elem_IC)  inz
006400140127      *
006500140127      *?Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*?
006600140127      *
006700140127     d KPJBA         e ds
006800140127      *
006900140127      * -?Parametri x Controllo profilo utenti?
007000140127     d TIBS34ds      e ds
007100140127      * -?Ds di riferimento al file esterno AZUTE00F?
007200140127     d AZUTEds       e ds                  extname(AZUTE00F)
007300140127      * -?Ds per dati organigramma?
007400140127     d dDatiUte      e ds
007500140127      *
007600140127      * -?Tab. "IC" = Importanza Cliente?
007700140127     d dsIC          e ds                  inz   qualified
007800140127      *
007900140127      * -?Status ds?
008000140127     d Status         sds           333
008100140127     d   SDSpgm          *proc
008200140127      *
008300140127      * -?InfDs del file video?
008400140127     d InfDspF         ds           512
008500140127     d   £Tasto              369    369
008600140127     d   £SFLnrr             378    379b 0
008700140127      *
008800140127      * -?Parametri per controllo data e transform in giorni - XSRDA8 -?
008900140127     d WLBdat          ds                  inz
009000140127     d   G08dat                       8  0 inz
009100140127     d   G08inv                       8  0 inz
009200140127     d   G08err                       1    inz('3')
009300140127     d   G08tgi                       5  0 inz
009400140227      *
009500140227      * -?Dati della tab. "IC" in schiera (per ordinamento)?
009600140227     d wIC_ds          ds                  inz   qualified
009700140227     d   wIC_ord                           inz   like(dsIC.§sICor)
009800140227     d   wIC_cod                           inz   like(ACBclv)
009900140227     d   wIC_des                           inz   like(dsIC.§sICde)
010000140127      *
010100140127      *?Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*?
010200140127      *
010300140127      * -?Flags booleani?
010400140127     d $Fine           s               n   inz(*off)
010500140127     d $F10            s               n   inz(*off)
010600140127     d $Bypass         s               n   inz(*off)
010700140127     d $InzS01         s               n   inz(*on)
010800140127     d $InzD02         s               n   inz(*on)
010900140128     d $InzS03         s               n   inz(*on)
011000140127      *
011100140127     d $Video          s              1    inz('1')
011200140127      *
011300140127      * -?Indici di schiere?
011400140127     d xx              s              3  0 inz
011500140127      *
011600140127      * -?Campi associati al SFL?
011700140127     d S01nrr          s                   like(C01rcd)  inz
011800140127     d SavS01csr       s                   like(C01rcd)  inz
011900140128     d S03nrr          s                   like(C03rcd)  inz
012000140128     d SavS03csr       s                   like(C03rcd)  inz
012100140128     d MaxS03nrr       s                   like(C03rcd)  inz
012200140127     d W_SflPag        s              3  0 inz
012300140127     d wPag            s              4  0 inz
012400140127     d wRig            s              3  0 inz
012500140127      *
012600140127      * -?Altri campi?
012700140127     d wDate           s              8  0 inz
012800140127     d W1Cdtd          s                   inz  like(ACBdtd)
012900140127      *
013000140127     d k_TBLkut        s                   like(TBLkut)  inz(1)
013100140127     d k_TBLcod        s                   like(TBLcod)  inz('IC')
013200140128     d k_ACBclv        s                   like(ACBclv)  inz
013300140128     d k_ACBdtd        s                   like(ACBdtd)  inz
013400140127      *
013500140127      *?Chiavi di Lettura (Key-List) - - - - - - - - - - - - - - - - -*?
013600140127      *
013700140127     c     k02tabel00    klist
013800140127     c                   kfld                    k_TBLkut
013900140127     c                   kfld                    k_TBLcod
014000140226     c     k02tiacb02    klist
014100140128     c                   kfld                    k_ACBdtd
014200140226     c                   kfld                    k_ACBclv
014300140127
014400140127      *---------------------------------------------------------------*
014500140127      *?I N D I C A T O R I                                          ?*
014600140127      *---------------------------------------------------------------*
014700140127      *? 06?    - Abilitato F6=Conferma in D02
014800140127      *? 10?    - Comodo                                              *
014900140127      *? 28?    - Emissione messaggio di errore a video               *
015000140127      *? 30?    - Pulizia del subfile                                 *
015100140127      *? 31?    - NON emissione del subfile                           *
015200140127      *? 32?    - Record modificato nel subfile (SflNxtChg)           *
015300140127      *? 33?    - Fine dati nel subfile         (SflEnd)              *
015400140127      *? 50?    - Opzione errata                                      *
015500140127      *? 51?    - Codice Importanza Cliente errato                    *
015600140127      *? 52?    - Data Inizio Validità errata                         *
015700140127      *? 53?    - Importo Tariffa errato                              *
015800140127      *? 90?    - Generico di errore                                  *
015900140127      *---------------------------------------------------------------*
016000140127
016100140127     c     *Entry        plist
016200140127     c                   parm                    KPJBA
016300140127      *
016400140127      * -?Operazioni Iniziali?
016500140127     c                   exsr      sr_RoutInz
016600140127      *
016700140127      * -?Gestione Video?
016800140127do  1c                   dow       $Fine   = *off
016900140127     c     $Video        caseq     '1'           sr_GesS01
017000140128     c     $Video        caseq     '2'           sr_GesD02
017100140128     c     $Video        caseq     '3'           sr_GesS03
017200140127     c                   endcs
017300140127e   1c                   enddo
017400140127      *
017500140127      * -?Fine?
017600140127     c                   movel     *on           *inLR
017700140127      *
017800140127      *---------------------------------------------------------------*
017900140127      *?Operazioni Iniziali                                          ?*
018000140127      *---------------------------------------------------------------*
018100140127     c     sr_RoutInz    BEGSR
018200140127      *
018300140127      * -?Reperimento dati job?
018400140127     c                   exsr      sr_DatiJob
018500140127      *
018600140127     c                   movel     SDSpgm        V1Tpgm
018700140127     c                   clear                   WLBdat
018800140127     c                   eval      G08dat  = *date
018900140127     c                   call      'XSRDA8'
019000140127     c                   parm                    WLBdat
019100140127     c                   movel     G08inv        wDate
019200140127      *
019300140127      * -?Caricamento tab. IC = Importanza Cliente?
019400140127     c                   clear                   xx
019500140127     c     k02tabel00    setll     TABEL
019600140127     c     k02tabel00    reade     TABEL
019700140127do  1c                   DoW       Not %eof(TABEL00F)
019800140127if  2c                   if        TBLflg = *blank
019900140127     c                   eval      dsIC = TBLuni
020000140227     c                   clear                   wIC_ds
020100140227     c                   eval      wIC_ds.wIC_ord = dsIC.§sICor
020200140227     c                   eval      wIC_ds.wIC_cod = TBLkey
020300140227     c                   eval      wIC_ds.wIC_des = dsIC.§sICde
020400140227     c                   add       1             xx
020500140227     c                   eval      sk_IC(xx) = wIC_ds
020600140127e   2c                   endif
020700140127     c     k02tabel00    reade     TABEL
020800140127e   1c                   EndDo
020900140227      *
021000140227     c                   sortA     sk_IC
021100140127      *
021200140127     c                   ENDSR
021300140127      *
021400140127      *---------------------------------------------------------------*
021500140127      *?Reperimento Dati del job (Utente/Operativi)                  ?*
021600140127      *---------------------------------------------------------------*
021700140127     c     sr_DatiJob    BEGSR
021800140127      *
021900140127     c     *dtaara       define    §azute        azuteds
022000140127     c     *dtaara       define    §datiute      ddatiute
022100140127      *
022200140127     c                   in(E)     *dtaara
022300140127     c                   IF        %ERROR or RSUT = *blanks
022400140127     c                   clear                   Tibs34Ds
022500140127     c                   call      'TIBS34R'
022600140127     c                   parm                    Tibs34Ds
022700140127     c                   in        *dtaara
022800140127     c                   ENDIF
022900140127      *
023000140127     c                   ENDSR
023100140127      *
023200140127      *---------------------------------------------------------------*
023300140127      *?Gestione videata C01/S01                                     ?*
023400140127      *---------------------------------------------------------------*
023500140127     c     sr_GesS01     BEGSR
023600140127      *
023700140127      * -?Inizializzazione della videata?
023800140127if  1c                   if        $InzS01 = *on
023900140127     c                   exsr      sr_InzS01
024000140127     c                   eval      $InzS01     = *off
024100140127e   1c                   endif
024200140127      *
024300140127      * -?Posizionameno del cursore?
024400140127if  1c                   if        C01csr  > *zeros
024500140127     c                   z-add     C01csr        C01rcd
024600140127x   1c                   else
024700140127     c                   z-add     1             C01rcd
024800140127e   1c                   endif
024900140127      *
025000140127      * -?Emissione Testata e Piede con tasti funzionali abilitati?
025100140127     c                   write     TA35T01
025200140127     c                   write     TA35P01
025300140127      * -?Emissione videata?
025400140127     c                   exfmt     TA35C01
025500140127     c                   setoff                                       28  90
025600140127     c                   clear                   V1Dmsg
025700140127      *
025800140127sel 1c                   SELECT
025900140127      * -?F3=Fine?
026000140127w   1c                   WHEN      *inKC
026100140127     c                   exsr      sr_F03S01
026200140127      * -?F10=Inserimento?
026300140127w   1c                   WHEN      *inKJ
026400140127     c                   exsr      sr_F10S01
026500140127      * -?Roll-UP?
026600140128w   1c                   WHEN      £Tasto      = c_RollUp
026700140127     c                   exsr      sr_RollUpS01
026800140127      *
026900140127x   1c                   OTHER
027000140127      * -?Gestione Opzioni?
027100140127     c                   exsr      sr_OpzS01
027200140127if  2c                   if        *in90
027300140127     c                   leavesr
027400140127e   2c                   endif
027500140127      *
027600140127e   1c                   ENDSL
027700140127      *
027800140127     c                   ENDSR
027900140127      *
028000140127      *---------------------------------------------------------------*
028100140127      *?Inizializzazione videata C01/S01                             ?*
028200140127      *---------------------------------------------------------------*
028300140127     c     sr_InzS01     BEGSR
028400140127      *
028500140127      * -?Pulizia subfile?
028600140127     c                   seton                                        3031
028700140127     c                   write     TA35C01
028800140127     c                   setoff                                         3133
028900140127      *
029000140128     c                   clear                   W_SflPag
029100140127     c                   clear                   C01rcd
029200140127     c                   clear                   C01csr
029300140127     c                   clear                   S01nrr
029400140127     c                   clear                   V1Dmsg
029500140127     c                   setoff                                       28  90
029600140127     c                   movea     *zeros        *in(51)
029700140127      *
029800140127      * -?Caricamento dati dal file TIACB00F?
029900140226     c     *hival        setgt     TIACB000
030000140127     c                   read      TIACB000
030100140127      *
030200140127     c                   exsr      sr_RollUpS01
030300140127      *
030400140127     c                   ENDSR
030500140127      *
030600140127      *---------------------------------------------------------------*
030700140127      *?Caricamento pagina successiva di dati                        ?*
030800140127      *---------------------------------------------------------------*
030900140127     c     sr_RollUpS01  BEGSR
031000140127      *
031100140127     c                   eval      S01nrr  = (W_SflPag * c_SflPag)
031200140127     c                   add       1             W_SflPag
031300140127     c                   eval      *in32   = *off
031400140127      *
031500140127      * -?Ciclo di caricamento dati nel sfl / lettura rec. successivo?
031600140226do  1c                   DOW       NOT  %eof(TIACB02L)
031700140127     c                             and  S01nrr < (W_SflPag * c_SflPag)
031800140127      *
031900140127     c                   exsr      sr_RieS01
032000140127     c                   add       1             S01nrr
032100140127     c                   write     TA35S01
032200140127      *
032300140226      *?N.B. - La v.l., pur essendo "descending" x data, richiede un?
032400140226      *     ?SETGT per passare alla data successiva, seppur minore?
032500140226     c     ACBdtd        setgt     TIACB000
032600140127     c                   read      TIACB000
032700140127      *
032800140127e   1c                   ENDDO
032900140127      *
033000140127      * -?Visualizzazione del SFL (se ci sono dati)?
033100140127     c                   eval      *in30   = (S01nrr = *zeros)
033200140127      *
033300140127      * -?Attivazione (eventuale) del SFLEND?
033400140226     c                   eval      *in33   = %eof(TIACB02L)
033500140127      *
033600140127      * -?Posizionamento cursore al 1° rcd della pagina?
033700140127if  1c                   if        S01nrr  > *zeros
033800140127     c     S01nrr        div       c_SflPag      wPag
033900140127     c                   mvr                     wRig
034000140127     c                   eval      C01rcd  = wPag * c_SflPag
034100140127     c                   if        wRig    > *zeros
034200140127     c                   eval      C01rcd  = C01rcd + 1
034300140127     c                   else
034400140127     c                   eval      C01rcd  = C01rcd - c_SflPag + 1
034500140127     c                   endif
034600140127x   1c                   else
034700140127     c                   clear                   C01rcd
034800140127e   1c                   endif
034900140127     c                   eval      C01csr  = C01rcd
035000140127      *
035100140127     c                   ENDSR
035200140127      *
035300140127      *---------------------------------------------------------------*
035400140127      *?Impostazione dati in singolo record del sfl S01              ?*
035500140127      *---------------------------------------------------------------*
035600140127     c     sr_RieS01     BEGSR
035700140127      *
035800140127     c                   clear                   TA35S01
035900140226      *
036000140226      * -?Campi hidden?
036100140226     c                   eval      H1Cdtd  = ACBdtd
036200140127      *
036300140127      * -?Data Decorrenza?
036400140127     c                   reset                   WLBdat
036500140127     c                   eval      G08inv = ACBdtd
036600140127     c                   call      'XSRDA8'
036700140127     c                   parm                    WLBdat
036800140127     c                   eval      S1Cdtd = G08dat
036900140127      *
037000140127      * -?Profilo utente inserimento?
037100140127     c                   eval      S1Cute = ACBute
037200140127      *
037300140127      * -?Data inserimento?
037400140127     c                   reset                   WLBdat
037500140127     c                   eval      G08inv  = ACBdti
037600140127     c                   call      'XSRDA8'
037700140127     c                   parm                    WLBdat
037800140127     c                   eval      S1Cdti  = G08dat
037900140127      *
038000140127     c                   ENDSR
038100140127      *
038200140127      *---------------------------------------------------------------*
038300140127      *?Gestione tasto funzionale F03 da videata S01                 ?*
038400140127      *---------------------------------------------------------------*
038500140127     c     sr_F03S01     BEGSR
038600140127      *
038700140127      * -?Chiusura del programma?
038800140127     c                   eval      $Fine   = *on
038900140127      *
039000140127     c                   ENDSR
039100140127      *
039200140127      *---------------------------------------------------------------*
039300140127      *?Gestione tasto funzionale F10 da videata S01                 ?*
039400140127      *---------------------------------------------------------------*
039500140127     c     sr_F10S01     BEGSR
039600140127      *
039700140128      * -?Passaggio alla videata di immissione (D02)?
039800140127     c                   eval      $Video  = '2'
039900140127     c                   eval      $InzD02 = *on
040000140128     c                   eval      $InzS03 = *on
040100140127      *
040200140127     c                   eval      $F10    = *on
040300140127      *
040400140127     c                   ENDSR
040500140127      *
040600140127      *---------------------------------------------------------------*
040700140127      *?Gestione opzioni subfile                                     ?*
040800140127      *---------------------------------------------------------------*
040900140127     c     sr_OpzS01     BEGSR
041000140127      *
041100140127     c                   clear                   SavS01csr
041200140127      *
041300140127     c                   readc     TA35S01
041400140127      *
041500140127do  1c                   DOW       NOT %eof(TNTA35D)
041600140127      *
041700140127     c                   eval      *in32   = *off
041800140127     c                   movea     *zeros        *in(51)
041900140127     c                   z-add     S01nrr        C01rcd
042000140127      *
042100140127sel 2c                   SELECT
042200140127      * -?Nessuna opzione?
042300140127w   2c                   WHEN      S1Copz  = *blanks
042400140127      * -?5 = Visualizzazione prezzo medio gasolio?
042500140127w   2c                   WHEN      S1Copz  = '5'
042600140127     c                   exsr      sr_OpzS1_5
042700140127      * -?? = Opzione NON valida?
042800140127x   2c                   OTHER
042900140127     c                   seton                                        285190
043000140127     c                   eval      V1Dmsg  = sk_Msg(01)
043100140127e   2c                   ENDSL
043200140127      *
043300140127      * -?Memorizzazione nrr del sfl con la 1ª opzione?
043400140127      *  ?per riposizionarvici il cursore dopo il tasto "Invio"?
043500140127if  1c                   if             S1Copz   <> *blanks
043600140127     c                             and (SavS01csr = *zeros
043700140127     c                              or  SavS01csr < C01rcd)
043800140127     c                   eval      SavS01csr   = C01rcd
043900140127e   1c                   endif
044000140127      *
044100140127      * -?Aggiornamento sfl?
044200140127sel 2c                   select
044300140127w   2c                   when      *in28
044400140127     c                   eval      *in32       = *on
044500140127     c                   z-add     C01rcd        C01csr
044600140127w   2c                   when      *in90       = *on
044700140127     c                   z-add     C01rcd        C01csr
044800140127     c                   clear                   S1Copz
044900140127x   2c                   other
045000140127     c                   clear                   S1Copz
045100140127e   2c                   endsl
045200140127     c                   UPDATE    TA35S01
045300140127if  2c                   if        *in28  OR  *in90
045400140127     c                   leave
045500140127e   2c                   endif
045600140127      *
045700140127     c                   readc     TA35S01
045800140127      *
045900140127e   1c                   ENDDO
046000140127      *
046100140127      * -?Riposiziono il cursore sul record contenente la prima opzione?
046200140127      *  ?(se non sono stati rilevati errori)?
046300140127if  1c                   if        NOT *in28  and  NOT *in90
046400140127     c                             and SavS01csr  > *zeros
046500140127     c                   z-add     SavS01csr     C01csr
046600140127e   1c                   endif
046700140127      *
046800140127     c                   ENDSR
046900140127      *
047000140127      *---------------------------------------------------------------*
047100140127      *?S1_Opz.5 = Visualizzazione singola Tariffa Cartello AC base  ?*
047200140127      *---------------------------------------------------------------*
047300140127     c     sr_OpzS1_5    BEGSR
047400140127      *
047500140127      * -?Impostazione dati in D02 (solo visualizzato - protetto)?
047600140127     c                   clear                   TA35D02
047700140127     c                   eval      V1Cdtd  = S1Cdtd
047800140127      *
047900140127      * -?Passaggio alla videata D03 per visualizzazione singola?
048000140128      *  ?tariffa cartello AC base?
048100140127     c                   eval      $F10    = *off
048200140128     c                   eval      $Video  = '3'
048300140128     c                   eval      $InzS03 = *on
048400140127      *
048500140128do  1c                   doW       $Video  = '3'
048600140128     c                   exsr      sr_GesS03
048700140127e   1c                   enddo
048800140127      *
048900140127     c                   ENDSR
049000140127      *
049100140127      *---------------------------------------------------------------*
049200140127      *?Gestione videata D02                                         ?*
049300140127      *---------------------------------------------------------------*
049400140127     c     sr_GesD02     BEGSR
049500140127      *
049600140127      * -?Inizializzazione videata?
049700140127if  1c                   if        $InzD02 = *on
049800140127     c                   exsr      sr_InzD02
049900140128     c                   movel     *off          $InzD02
050000140127e   1c                   endif
050100140127      *
050200140127      * -?Emissione Testata e Piede con tasti funzionali abilitati?
050300140127      *  ?oltre alla videata precedente?
050400140127     c                   eval      *in03   = *off
050500140127     c                   eval      *in06   = *off
050600140127     c                   write     TA35T01
050700140128     c                   write     TA35P02
050800140127      * -?Emissione videata?
050900140127     c                   exfmt     TA35D02
051000140127     c                   setoff                                       28  90
051100140127     c                   clear                   V1Dmsg
051200140127      *
051300140127sel 1c                   SELECT
051400140127      * -?F12=Ritorno?
051500140127w   1c                   WHEN      *inKL
051600140127     c                   exsr      sr_F12D02
051700140127      *
051800140127x   1c                   OTHER
051900140127     c                   exsr      sr_CtrD02
052000140127      *
052100140127if  2c                   if        *in90
052200140127     c                   leavesr
052300140127e   2c                   endif
052400140127      *
052500140128     c                   eval      $InzS03 = *on
052600140128     c                   eval      $Video  = '3'
052700140127      *
052800140127e   1c                   ENDSL
052900140127      *
053000140127     c                   ENDSR
053100140127      *
053200140127      *---------------------------------------------------------------*
053300140127      *?Inizializzazione videata D02                                 ?*
053400140127      *---------------------------------------------------------------*
053500140127     c     sr_InzD02     BEGSR
053600140127      *
053700140127     c                   clear                   TA35D02
053800140127      *
053900140127     c                   ENDSR
054000140127      *
054100140127      *---------------------------------------------------------------*
054200140127      *?Gestione tasto funzionale F12 da videata D02                 ?*
054300140127      *---------------------------------------------------------------*
054400140127     c     sr_F12D02     BEGSR
054500140127      *
054600140127      * -?Ritorno alla videata precedente?
054700140128     c                   eval      $Video  = '1'
054800140127      *
054900140127     c                   ENDSR
055000140127      *
055100140127      *---------------------------------------------------------------*
055200140127      *?Controllo dati immessi in videata D02                        ?*
055300140127      *---------------------------------------------------------------*
055400140127     c     sr_CtrD02     BEGSR
055500140127      *
055600140127     c                   movea     *zeros        *in(51)
055700140127      *
055800140127      * -?Controllo correttezza data inizio validità - in INSERIMENTO?
055900140127      *  ?(questo controllo viene eseguito SOLO in inserimento - $F10):?
056000140127      *
056100140127      * -?Controllo formale data inizio validità?
056200140127     c                   clear                   WLBdat
056300140127if  1c                   if        V1Cdtd  > *zeros
056400140127     c                   eval      G08dat  = V1Cdtd
056500140127     c                   call      'XSRDA8'
056600140127     c                   parm                    WLBdat
056700140127x   1c                   else
056800140127     c                   eval      G08err  = *on
056900140127e   1c                   endif
057000140127if  1c                   if        G08err  = *on
057100140127     c                   seton                                        285190
057200140127     c                   eval      V1Dmsg  = sk_Msg(03)
057300140127     c                   leavesr
057400140127e   1c                   endif
057500140127     c                   eval      V1Cdtd  = G08dat
057600140127     c                   eval      W1Cdtd  = G08inv
057700140127      *
057800140127      * -?DEVE essere più recente dell'ultima già inserita in TIACB00F?
057900140226     c     W1Cdtd        setgt     TIACB000
058000140226     c                   read      TIACB000
058100140226if  1c                   if        NOT  %eof(TIACB02L)
058200140127     c                             and  ACBdtd >= W1Cdtd
058300140127     c                   reset                   WLBdat
058400140127     c                   eval      G08inv  = ACBdtd
058500140127     c                   call      'XSRDA8'
058600140127     c                   parm                    WLBdat
058700140127     c                   seton                                        285190
058800140127     c                   eval      V1Dmsg  = %trim(sk_Msg(04))
058900140127     c                                     + ' '
059000140127     c                                     + %editc(G08dat:'Y')
059100140127     c                   leavesr
059200140226e   1c                   endif
059300140127      *
059400140127      * -?NON può essere futura?
059500140128if  1c*//                if        W1Cdtd  > Wdate
059600140128     c*//                reset                   WLBdat
059700140128     c*//                seton                                        285190
059800140128     c*//                eval      V1Dmsg  = sk_Msg(05)
059900140128     c*//                leavesr
060000140128e   1c*//                endif
060100140127      *
060200140127     c                   ENDSR
060300140128      *
060400140128      *---------------------------------------------------------------*
060500140128      *?Gestione videata C03/S03                                     ?*
060600140128      *---------------------------------------------------------------*
060700140128     c     sr_GesS03     BEGSR
060800140128      *
060900140128      * -?Inizializzazione della videata?
061000140128if  1c                   if        $InzS03 = *on
061100140128     c                   exsr      sr_InzS03
061200140128     c                   eval      $InzS03     = *off
061300140128e   1c                   endif
061400140128      *
061500140128      * -?Posizionameno del cursore?
061600140128if  1c                   if        C03csr  > *zeros
061700140128     c                   z-add     C03csr        C03rcd
061800140128x   1c                   else
061900140128     c                   z-add     1             C03rcd
062000140128e   1c                   endif
062100140128      *
062200140128     c                   if        Not *in28
062300140128      * -?Emissione Testata e Piede con tasti funzionali abilitati?
062400140128     c                   write     TA35T01
062500140128     c                   write     TA35P02
062600140128      * -?Emissione videata con data inizio validità?
062700140128     c                   write     TA35D02
062800140128     c                   endif
062900140128      * -?Emissione videata?
063000140128     c                   write     TA35D02
063100140128if  1c                   if        $F10    = *off
063200140128     c                   write     TA35C03
063300140128     c                   exfmt     Protect
063400140128     c                   else
063500140128     c                   write     Protect
063600140128     c                   exfmt     TA35C03
063700140128     c                   endif
063800140128      *
063900140128     c                   setoff                                       28  90
064000140128     c                   clear                   V1Dmsg
064100140128      *
064200140128     c                   Select
064300140128      *
064400140128      * -?F3=Fine?
064500140128     c                   When      £Tasto = c_F03
064600140128     c                   exsr      sr_F03S03
064700140128      *
064800140128      * -?F12=Ritorno?
064900140128     c                   When      £Tasto = c_F12
065000140128     c                   exsr      sr_F12S03
065100140128      *
065200140128     c                   Other
065300140128      * -?Controllo dati?
065400140128     c                   exsr      sr_CtrS03
065500140128if  2c                   if        *in90
065600140128     c                   leavesr
065700140128e   2c                   endif
065800140128      * -?F6=Conferma?
065900140128if  2c                   if        £Tasto = c_F06
066000140128     c                   exsr      sr_Wrt_TIACB
066100140128if  3c                   if        Not *in90
066200140128     c                   eval      $Video  = '1'
066300140128     c                   eval      $InzS01 = *on
066400140128e   3c                   endif
066500140128e   2c                   endif
066600140128      *
066700140128     c                   EndSl
066800140128      *
066900140128     c                   ENDSR
067000140128      *
067100140128      *---------------------------------------------------------------*
067200140128      *?Inizializzazione videata C03/S03                             ?*
067300140128      *---------------------------------------------------------------*
067400140128     c     sr_InzS03     BEGSR
067500140128      *
067600140128      * -?Pulizia subfile?
067700140128     c                   seton                                        3435
067800140128     c                   write     TA35C03
067900140128     c                   setoff                                         3537
068000140128      *
068100140128     c                   clear                   C03rcd
068200140128     c                   clear                   C03csr
068300140128     c                   clear                   S03nrr
068400140128     c                   clear                   V1Dmsg
068500140128     c                   setoff                                       28  90
068600140128     c                   movea     *zeros        *in(51)
068700140128     c                   reset                   $Bypass
068800140128      *
068900140128      * -?Caricamento dati dal file TIACB00F?
069000140227for 1c                   For       S03nrr = 1  To  %elem(sk_IC)
069100140227if  2c                   if        sk_IC(S03nrr) = *blank
069200140128     c                   leave
069300140128e   2c                   endif
069400140128     c                   eval      *in36  = *off
069500140128     c                   exsr      sr_RieS03
069600140128     c                   write     TA35S03
069700140128e   1c                   EndFor
069800140128     c                   eval      MaxS03nrr = S03nrr
069900140128      *
070000140128      * -?Visualizzazione del SFL (se ci sono dati)?
070100140128     c                   eval      *in34   = (S03nrr = *zeros)
070200140128      *
070300140128      * -?Attivazione del SFLEND (già caricato tutto)?
070400140128     c                   eval      *in37   = *on
070500140128      *
070600140128      * -?Posizionamento cursore al 1° rcd del subfile?
070700140128if  1c                   if        S03nrr  > *zeros
070800140128     c                   eval      C01rcd  = 1
070900140128x   1c                   else
071000140128     c                   clear                   C01rcd
071100140128e   1c                   endif
071200140128     c                   eval      C01csr  = C01rcd
071300140128      *
071400140128      * -?Impostazione indicatori:?
071500140128      *  ?*in03?= abilita F3 per ritorno a S01 senza proseguire con?
071600140128      *          ?l'elaborazione delle opzioni successive?
071700140128      *          ?In fase di immissione consente il ritorno al subfile?
071800140128      *          ?senza passare dalla videata D01...?
071900140128     c                   eval      *in03 = *on
072000140128      * -?*in06?= abilita F6 per inserimento record?
072100140128     c                   eval      *in06 = ($F10  = *on)
072200140128      *
072300140128     c                   ENDSR
072400140128      *
072500140128      *---------------------------------------------------------------*
072600140128      *?Impostazione dati in singolo record del sfl S03              ?*
072700140128      *---------------------------------------------------------------*
072800140128     c     sr_RieS03     BEGSR
072900140128      *
073000140128     c                   clear                   TA35S03
073100140128      *
073200140128      * -?Importanza?
073300140227     c                   eval      wIC_ds = sk_IC(S03nrr)
073400140227     c                   eval      S3Cclv = wIC_ds.wIC_cod
073500140227     c                   eval      S3Dclv = wIC_ds.wIC_des
073600140128      *
073700140128      * -?Se è stata richiesta l'opzione 5=Visualizzazione?
073800140128      *  ?si reperiscono i dati da visualizzare dal file?
073900140128if  1c                   If             S1Copz = '5'
074000140128     c                             and  $F10   = *off
074100140128      *
074200140128     c                   eval      k_ACBdtd = H1Cdtd
074300140227     c                   eval      wIC_ds   = sk_IC(S03nrr)
074400140227     c                   eval      k_ACBclv = wIC_ds.wIC_cod
074500140226     c     k02tiacb02    chain     TIACB000
074600140226if  2c                   if        %found(TIACB02L)
074700140128      *
074800140128      * -?Importo?
074900140128     c                   eval      S3Citr = ACBitr
075000140128      * -?Utente inserimento?
075100140128     c                   eval      S3Cute = ACBute
075200140128      * -?Data inserimento?
075300140128     c                   reset                   WLBdat
075400140128     c                   eval      G08inv = ACBdti
075500140128     c                   call      'XSRDA8'
075600140128     c                   parm                    WLBdat
075700140128     c                   eval      S3Cdti = G08dat
075800140128      *
075900140128e   2c                   endif
076000140128      * -?Impostazione indicatori x attributi video?
076100140128      * -?*in41?= Visualizzazione?
076200140128if  1c                   eval      *in41 = (S1Copz = '5'  and  Not $F10)
076300140128      *
076400140128e   1c                   EndIf
076500140127      *
076600140127     c                   ENDSR
076700140127      *
076800140127      *---------------------------------------------------------------*
076900140128      *?Gestione tasto funzionale F03 da videata D03                 ?*
077000140127      *---------------------------------------------------------------*
077100140128     c     sr_F03S03     BEGSR
077200140127      *
077300140128      * -?Ritorno al 1° subfile?
077400140128     c                   eval      $Video = '1'
077500140128      *
077600140128      * -?Se visualizzazione: blocco elaborazione opzioni?
077700140128if  1c                   if             S1Copz = '5'
077800140128     c                             and  $F10   = *off
077900140128     c                   eval      *in90  = *on
078000140128e   1c                   endif
078100140127      *
078200140127     c                   ENDSR
078300140127      *
078400140127      *---------------------------------------------------------------*
078500140128      *?Gestione tasto funzionale F12 da videata S03                 ?*
078600140127      *---------------------------------------------------------------*
078700140128     c     sr_F12S03     BEGSR
078800140127      *
078900140128      * -?Se visualizzazione: ritorno al 1° subfile?
079000140128if  1c                   If             S1Copz = '5'
079100140128     c                             and  $F10   = *off
079200140128     c                   eval      $Video = '1'
079300140128      * -?Se inserimento: ritorno alla videata precedente?
079400140128x   1c                   else
079500140128     c                   eval      $Video = '2'
079600140128e   1c                   endif
079700140127      *
079800140127     c                   ENDSR
079900140127      *
080000140127      *---------------------------------------------------------------*
080100140128      *?Controllo dati immessi in videata S03                        ?*
080200140127      *---------------------------------------------------------------*
080300140128     c     sr_CtrS03     BEGSR
080400140127      *
080500140128     c                   clear                   SavS03csr
080600140128      *
080700140128      * -?Ciclo di lettura subfile?
080800140128     c                   readc     TA35S03
080900140128      *
081000140128     c                   DoW       Not %eof(TNTA35D)
081100140128      *
081200140128     c                   eval      *in36  = *off
081300140128     c                   movea     *zeros        *in(51)
081400140128     c                   eval      C03rcd = S03nrr
081500140127      *
081600140127      * -?Controllo importo tariffa?
081700140128sel 1c                   Select
081800140128w   1c                   When      S3Citr  <  *zeros  or
081900140128     c                             S3Citr  >= *hival
082000140127     c                   seton                                        285390
082100140127     c                   eval      V1Dmsg  = sk_Msg(06)
082200140128w   1c*//                When      S03itr  = *zeros  and
082300140128     c*//                          Not $Bypass
082400140128     c*//                eval      $Bypass = *on
082500140128     c*//                seton                                        285390
082600140128     c*//                eval      V1Dmsg  = sk_Msg(07)
082700140128e   1c                   EndSl
082800140128      *
082900140128      * -?Memorizzazione nrr del sfl con il 1° importo per?
083000140128      *  ?riposizionarvici il cursore dopo il tasto "Invio"?
083100140128     c                   if         S3Citr   <> *zero  and
083200140128     c                             (SavS03csr = *zero  or
083300140128     c                              SavS03csr < C03rcd)
083400140128     c                   eval      SavS03csr   = C03rcd
083500140128     c                   endif
083600140128      *
083700140128      * -?Aggiornamento sfl?
083800140128     c                   select
083900140128     c                   when      *in28  or  S1Copz <> *zero
084000140128     c                   eval      *in36  = *on
084100140128     c                   eval      C03csr = C03rcd
084200140128     c                   when      *in90
084300140128     c                   eval      C03csr = C03rcd
084400140128     c                   endsl
084500140128      *
084600140128     c                   UPDATE    TA35S03
084700140128      *
084800140128     c                   if        *in28  or  *in90
084900140128     c                   leave
085000140128     c                   endif
085100140128      *
085200140128     c                   readc     TA35S03
085300140128      *
085400140128     c                   ENDDO
085500140128      *
085600140128      * -?Riposizionam. cursore sul record contenente la prima opz.?
085700140128      *  ?(se non sono stati rilevati errori)?
085800140128     c                   if        NOT *in28  and  NOT *in90  and
085900140128     c                             SavS03csr > *zero
086000140128     c                   eval      C03csr = SavS03csr
086100140128     c                   endif
086200140127      *
086300140127     c                   ENDSR
086400140127      *
086500140127      *---------------------------------------------------------------*
086600140127      *?Scrittura record in file TIACB00F                            ?*
086700140127      *---------------------------------------------------------------*
086800140127     c     sr_Wrt_TIACB  BEGSR
086900140127      *
087000140128for 1c                   For       S03nrr = 1  To  MaxS03nrr
087100140128      *
087200140128     c     S03nrr        chain     TA35S03
087300140128      *
087400140128if  2c                   if        Not %found(TNTA35D)
087500140128     c                   iter
087600140128e   2c                   endif
087700140128      *
087800140128if  2c                   if        S3Citr = *zeros
087900140128     c                   iter
088000140128e   2c                   endif
088100140128      *
088200140127     c                   clear                   TIACB000
088300140127      *
088400140127     c                   eval      ACBdtd = W1Cdtd
088500140128     c                   eval      ACBclv = S3Cclv
088600140128     c                   eval      ACBitr = S3Citr
088700140127     c                   eval      ACBute = KNMUS
088800140127     c                   eval      ACBdti = wDate
088900140127      *                  __________________
089000140127     c                   WRITE(e)  TIACB000
089100140127      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
089200140128if  2c                   if        %error
089300140128     c                   eval      $Video = '3'
089400140128     c                   eval      C03rcd = S03nrr
089500140128     c                   eval      C03csr = C03rcd
089600140127     c                   seton                                        285190
089700140127     c                   eval      V1Dmsg = sk_Msg(08)
089800140128     c                   leavesr
089900140128e   2c                   endif
090000140127      *
090100140128     c                   EndFor
090200140128      *
090300140127     c                   ENDSR
090400140127
090500140127** -?sk_Msg?-?Messaggi di errore?--------------------------------------------*
090600140127Opzione errata                                                                 1
090700140128Codice Importanza Cliente errato                                      -LIBERO- 2
090800140127Data formalmente errata                                                        3
090900140127Data inizio validità NON succesiva all'ultima reperita:                        4
091000140128Non ammesse decorrenze "future"                                       -LIBERO- 5
091100140127Importo tariffa errato                                                         6
091200140128Importo tariffa mancante: premere "Invio" per continuare              -LIBERO- 7
091300140127Rilevati errori in fase di scrittura record nel file TIACB00F                  8
