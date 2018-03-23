000100060109      *===============================================================*
000200060109      *?TISITSR * Stampa anagrafico destinatari particolari          ?*
000300060109      *===============================================================*
000400060109
000500060109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060109
000700060109     fAZORG01L  if   e           k disk
000800060109     fTABEL00F  if   e           k disk
000900060109     fAZDSTP1L  if   e           k disk    usropn
001000060109      *
001100060109     fTISITSD   cf   e             workstn
001200060109
001300060109      *---------------------------------------------------------------*
001400060109
001500060109      *
001600060109      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700060109      *
001800070913      * - Decodifica giorni di chiusura
001900070913     d $GgS            s              1    dim(9)   ctdata  perrcd(1)
002000070913     d $Gio            s             20    dim(9)   alt($GgS)
002100060109      * - Messaggi
002200091009     d $Msg            s             78    dim(11)  ctdata  perrcd(1)
002300060109      *
002400060109      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002500060109      *
002600060109     d KPJBA         e ds
002700060109     d Param           ds
002800060414     d  PARnaz                        3                                         Nazione
002900060414     d  PARprv                        3                                         Provincia
003000091009     d  PARcapI                       9                                         C.a.p. inizio range
003100091009     d  PARcapF                       9                                         C.a.p. fine   range
003200060414     d  PARrag                       35                                         Ragione Sociale
003300060414     d  PARdos                        1                                         Tipo Destinatario
003400060414     d  PARsin                        1                                         Dettaglio Sinonimi
003500060414     d  PARddt                        1                                         Dest. con obbl.DDT
003600060414     d  PARips                        1                                         Dest. con inst.prest
003700091008     d  PARipsF                       3s 0                                      FilDestconInstrPrest
003800070913     d  PARfgi                        1                                         Dest. con forz.giro
003900091008     d  PARfgiF                       3s 0                                      FilDestconForzGiroCo
004000060414     d  PARric                        7s 0                                      Ricorrenza > di
004100060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
004200060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
004300060414     d  PARrot                        1                                         Salto pag. x Prov.
004400060414     d  PARrtpo                       1                                         Salto pag. x P.O.
004500070913     d  PARgcx                        1                                         Giorno di chiusura
004600090708     d  PARfic                        1                                         Dest. con forz.t.i.c
004700060109      * - Parametri x Controllo profilo utenti
004800060109     d TIBS34ds      e ds
004900060109      * - Ds di riferimento al file esterno AZUTE00F
005000060109     d AZUTEds       e ds                  extname(AZUTE00F)
005100060109      * - Ds per dati organigramma
005200070913     d dDatiUte      e ds
005300060104      *
005400070917      * - Parametri x ricerca/controllo tabelle
005500060104     d TIBS02ds      e ds                  inz
005600060104     d   T02mod      e                     inz('C')
005700060104     d   T02cod      e                     inz('TDP')
005800060104      * - Tab. "TDP" = Tipi Destinatari Particolari
005900060104     d dTDP          e ds                  inz
006000070917      * - Tab. "VPO/DECO" = Variabili Programmi Operativi: Decorrenze
006100070917     d dVPOdeco      e ds                  inz
006200060109      *
006300060109     d Status         sds           333
006400060109     d   SDSpgm          *proc
006500060109      *
006600060109      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006700060109      *
006800060109      * - Flags
006900060109     d $Fine           s              1    inz(*off)
007000060109     d $InzD01         s              1    inz(*on)
007100070913      *
007200070913      * - Indici
007300070913     d xx              s              3  0 inz
007400070913      *
007500070913      * - Campi di comodo
007600070917     d wDate_Iso       s               d   inz   datfmt(*iso)
007700070917     d wDate           s              8  0 inz
007800070913     d w001a           s              1    inz
007900060109      *
008000060109      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
008100060109      *
008200060109      * - TABEL00F
008300060109     c     K03tabel      klist
008400060109     c                   kfld                    TBLkut
008500060109     c                   kfld                    TBLcod
008600060109     c                   kfld                    TBLkey
008700060109
008800060109      *---------------------------------------------------------------*
008900060109      *  RIEPILOGO INDICATORI                                         *
009000060109      *---------------------------------------------------------------*
009100060109      * 28    - Visualizza messaggio di errore                        *
009200070917      * 41    - Richiesto "Tipo stampa: C=Confermati"                 *
009300070917      * 42    - Inesistenza WrkF proposte destinatari:                *
009400060414      *         consentiti solo tipi "C"                              *
009500070917      * 43    - Disabilita selezione "SOLO destinatari con gg chius." *
009600060414      * 51    - Nazione                  errata o mancante            *
009700060414      * 52    - Provincia                errata o mancante            *
009800060414      * 53    - Ragione sociale          errata o mancante            *
009900060414      * 54    - Tipo destinatario        errato o mancante            *
010000060414      * 55    - P.O. utente ins/mod      errato o mancante            *
010100060414      * 56    - Salto pagina x Prov/P.O  errato o mancante            *
010200060109      * 90    - Errore generico                                       *
010300060109      *---------------------------------------------------------------*
010400060109
010500060109     c     *Entry        plist
010600060109     c                   parm                    KPJBA
010700060109     c                   movel     KPJBU         Param
010800060109      *
010900060109      * Operazioni Iniziali
011000060109     c                   exsr      RoutInz
011100060109      *
011200060109      * Gestione Video
011300060109do  1c                   DOW       $Fine   = *off
011400060109     c                   exsr      GesD01
011500060109e   1c                   ENDDO
011600060109      *
011700060109      * Fine
011800060109     c                   clear                   TIBS02ds
011900060109     c                   eval      T02tla = 'C'
012000060109     c                   call      'TIBS02R'
012100060109     c                   parm                    KPJBA
012200060109     c                   parm                    TIBS02ds
012300060109      *
012400060109     c                   movel     *on           *inLR
012500060109      *
012600060109      *---------------------------------------------------------------*
012700060109      * Operazioni Iniziali                                           *
012800060109      *---------------------------------------------------------------*
012900060109     c     RoutInz       BEGSR
013000060109      *
013100060109      * Reperisco dati job
013200060109     c                   exsr      DatiJob
013300060109      *
013400060109     c                   movel     SDSpgm        V1Tpgm
013500060109     c                   eval      TBLkut  = 1
013600070917      *
013700070917      * Reperimento data corrente
013800070917     c                   movel     *date         wDate_Iso
013900070917     c                   move      wDate_Iso     wDate
014000070917      *
014100070917      * Aggancio tabella "VPO/DECO" per reperimento data attivazione
014200070917      *   dati in trk "XT1": filiale giro  e  giorni chiusura
014300070917     c                   clear                   dVPOdeco
014400070917     c                   clear                   TIBS02ds
014500070917     c                   eval      T02mod  = 'C'
014600070917     c                   eval      T02sif  = KNSIF
014700070917     c                   eval      T02cod  = 'VPO'
014800070917     c                   movel(P)  'DECO'        T02ke1
014900070917     c                   call      'TIBS02R'
015000070917     c                   parm                    KPJBA
015100070917     c                   parm                    TIBS02ds
015200070917    2c                   if        T02err  = *blanks
015300070917     c                   movel     T02uni        dVPOdeco
015400070917    2c                   endif
015500060109      *
015600060109     c                   ENDSR
015700060109      *
015800060109      *---------------------------------------------------------------*
015900060109      * Reperimento Dati del job (Utente/Operativi)                   *
016000060109      *---------------------------------------------------------------*
016100060109     c     DatiJob       BEGSR
016200060109      *
016300060109     c     *dtaara       define    §azute        azuteds
016400060109     c     *dtaara       define    §datiute      ddatiute
016500060109      *
016600060109     c                   in(E)     *dtaara
016700060109     c                   IF        %ERROR or RSUT = *blanks
016800060109     c                   clear                   Tibs34Ds
016900060109     c                   call      'TIBS34R'
017000060109     c                   parm                    Tibs34Ds
017100060109     c                   in        *dtaara
017200060109     c                   ENDIF
017300060109      *
017400060109     c                   ENDSR
017500060109      *
017600060109      *---------------------------------------------------------------*
017700060109      * Gestione videata D01                                          *
017800060109      *---------------------------------------------------------------*
017900060109     c     GesD01        BEGSR
018000060109      *
018100060109      * Inizializzo la videata
018200060109if  1c                   if        $InzD01 = *on
018300060109     c                   exsr      InzD01
018400060109     c                   movel     *off          $InzD01
018500060109e   1c                   endif
018600060109      *
018700060109      * Scrivo la testata e la riga tasti funzionali abilitati
018800060109if  1c                   if        NOT *in90
018900060109     c                   write     SITST01
019000060109     c                   write     SITSP01
019100060109e   1c                   endif
019200060109      * Emetto la videata
019300060109     c                   exfmt     SITSD01
019400060109     c                   setoff                                       28  90
019500060109     c                   clear                   V1Dmsg
019600060109      *
019700060109sel 1c                   select
019800060109      * F3=Fine
019900060109w   1c                   when      *inKC
020000060109     c                   exsr      F03D01
020100060109     c                   leavesr
020200060109      * F12=Ritorno
020300060109w   1c                   when      *inKL
020400060109     c                   exsr      F03D01
020500060109     c                   leavesr
020600060109      *
020700060109e   1c                   endsl
020800060109      *
020900060109      * Invio o F6
021000060109     c                   exsr      CtrD01
021100060109if  1c                   if        *in90
021200060109     c                   leavesr
021300060109e   1c                   endif
021400060109      * F6=Conferma
021500060109if  1c                   if        *inKF   = *on
021600060109     c                   exsr      SbmJob
021700060109e   1c                   endif
021800060109      *
021900060109     c                   ENDSR
022000060109      *
022100060109      *---------------------------------------------------------------*
022200060109      * Inizializzazione videata D01                                  *
022300060109      *---------------------------------------------------------------*
022400060109     c     InzD01        BEGSR
022500060109      *
022600060109     c                   clear                   SITSD01
022700060109      *
022800060109     c                   eval      V1Ctip  =  'C'
022900060109      *
023000060109      * Verifica esistenza WrkFile proposte destinatari
023100060109      * => SE non esiste: consento solo tipi "C"
023200060109     c                   open(e)   AZDSTP1L
023300060109     c                   if        %error
023400070917     c                   eval      *in42   =  *on
023500060109     c                   exsr      CtrD01
023600060109     c                   setoff                                       28  90
023700060109     c                   movea     *zeros        *in(51)
023800060109     c                   endif
023900070917      *
024000070917      * Impostazione indicatori
024100070917      * - Test se richiesta stampa di clienti potenziali
024200070917     c                   eval      *in41  =  (V1Ctip = 'C')
024300070917      * - Test se iniziata nuova gestione del trk "XT1" di AZDSE00F
024400070917     c                   eval      *in43   = (§VPOxt1 > Wdate)
024500060109      *
024600060109     c                   ENDSR
024700060109      *
024800060109      *---------------------------------------------------------------*
024900060109      * Gestione tasto funzionale F03 da videata D01                  *
025000060109      *---------------------------------------------------------------*
025100060109     c     F03D01        BEGSR
025200060109      *
025300060109      * Chiudo il programma
025400060109     c                   eval      $Fine   = *on
025500060109      *
025600060109     c                   ENDSR
025700060109      *
025800060109      *---------------------------------------------------------------*
025900060109      * Controllo dati immessi in videata D01                         *
026000060109      *---------------------------------------------------------------*
026100060109     c     CtrD01        BEGSR
026200060109      *
026300070917     c                   eval      *in41  =  (V1Ctip = 'C')
026400060109      *
026500060109     c                   movea     *zeros        *in(51)
026600060109      *
026700060109      * NAZIONE
026800060109     c     '?'           scan      V1Cnaz
026900060109sel 1c                   select
027000060109      * - ricerca
027100060109w   1c                   when      %found
027200060109     c                   clear                   V1Cnaz
027300060109     c                   call      'X§TABER'
027400060109     c                   parm                    TBLkut
027500060109     c                   parm      '15'          TBLcod
027600060109     c                   parm      *blanks       TBLkey
027700060109     c                   parm      *blanks       wDES             25
027800060109     c                   movel     TBLkey        V1Cnaz
027900060109     c                   eval      *in90    =  *on
028000060109     c                   leavesr
028100060109      * - "999" = Tutti
028200060829w   1c                   when      V1Cnaz   =  '999'
028300060109      * - controllo validità
028400060109x   1c                   other
028500060109     c                   eval      TBLcod   =  '15'
028600060109     c                   movel(p)  V1Cnaz        TBLkey
028700060109     c     K03tabel      chain     TABEL
028800060109if  2c                   if        NOT %found(TABEL00F)
028900060109     c                              or TBLflg <> *blanks
029000060109     c                   seton                                        285190
029100060414     c                   movel     $Msg(1)       V1Dmsg
029200060109     c                   leavesr
029300060109e   2c                   endif
029400060109e   1c                   endsl
029500060109      *
029600060109      * PROVINCIA
029700060109     c     '?'           scan      V1Cprv
029800060109sel 1c                   select
029900060109      * - ricerca
030000060109w   1c                   when      %found
030100060109     c                   clear                   V1Cprv
030200060109     c                   call      'X§TABER'
030300060109     c                   parm                    TBLkut
030400060109     c                   parm      'PR'          TBLcod
030500060109     c                   parm      *blanks       TBLkey
030600060109     c                   parm      *blanks       wDES
030700060109     c                   movel     TBLkey        V1Cprv
030800060109     c                   eval      *in90    =  *on
030900060109     c                   leavesr
031000060109      * - "  " = Tutte
031100060109w   1c                   when      V1Cprv   =  *blanks
031200060109      * - controllo validità
031300060109x   1c                   other
031400060109     c                   eval      TBLcod   =  'PR'
031500060109     c                   movel(p)  V1Cprv        TBLkey
031600060109     c     K03tabel      chain     TABEL
031700060109if  2c                   if        NOT %found(TABEL00F)
031800060109     c                              or TBLflg <> *blanks
031900060109     c                   seton                                        285290
032000060414     c                   movel     $Msg(2)       V1Dmsg
032100060109     c                   leavesr
032200060109e   2c                   endif
032300060109e   1c                   endsl
032400091009      /free
032500091009         if  V1CcapI > V1CcapF;
032600091009           *in28  = *on;
032700091009           *in90  = *on;
032800091009           *in57  = *on;
032900091009           V1Dmsg = $Msg(9);
033000091009           leavesr;
033100091009         endif;
033200091009      /end-free
033300060109      *
033400060109      * RAGIONE SOCIALE
033500060109     c                   if             V1Crag <> *blanks
033600060109     c                             and  %len(%trim(V1Crag)) < 3
033700060109     c                   seton                                        285390
033800060414     c                   movel     $Msg(3)       V1Dmsg
033900060109     c                   leavesr
034000060109     c                   endif
034100060109      *
034200060109      * TIPO DESTINATARIO
034300060104     c                   clear                   V1Ddos
034400060109     c     '?'           scan      V1Cdos
034500060104sel 1c                   select
034600060109      * - ricerca
034700060104w   1c                   when      %found
034800060109     c                   clear                   V1Cdos
034900060109     c                   clear                   dTDP
035000060104     c                   clear                   TIBS02ds
035100060829     c                   eval      T02mod   =  'R'
035200060829     c                   eval      T02cod   =  'TDP'
035300060104     c                   call      'TIBS02R'
035400060104     c                   parm                    KPJBA
035500060104     c                   parm                    TIBS02ds
035600060104if  2c                   if             T02err  = *blanks
035700060104     c                             and  T02ke1 <> *blanks
035800060109     c                   movel     T02ke1        V1Cdos
035900060104     c                   movel     T02uni        dTDP
036000060104     c                   movel     §TDPdes       V1Ddos
036100060829     c                   eval      *in90    =  *on
036200060109     c                   leavesr
036300060104e   2c                   endif
036400060104      * - " " = Tutti
036500060829w   1c                   when      V1Cdos   =  *blanks
036600060109      * - controllo validità
036700060104x   1c                   other
036800060109     c                   clear                   dTDP
036900060109     c                   reset                   TIBS02ds
037000060109     c                   movel(p)  V1Cdos        T02ke1
037100060109     c                   call      'TIBS02R'
037200060109     c                   parm                    KPJBA
037300060109     c                   parm                    TIBS02ds
037400060829     c                   if        T02err   =  *blanks
037500060109     c                   movel     T02uni        dTDP
037600060109     c                   endif
037700060109     c                   movel     §TDPdes       V1Ddos
037800060829if  2c                   if        T02err   =  *blanks
037900060104     c                   movel     §TDPdes       V1Ddos
038000060104x   2c                   else
038100060109     c                   seton                                        285490
038200060414     c                   movel     $Msg(4)       V1Dmsg
038300060109     c                   leavesr
038400060104e   2c                   endif
038500060104e   1c                   endsl
038600091009      /free
038700091009         // - Parzializzazione per filiale in selezione
038800091009         //   "SOLO destinatari con instradamento prestabilito"
038900091009         select;
039000091009           when  V1Ctip = 'P';
039100091009           when  V1Cips = *blank   and   V1CipsF > *zero;
039200091009             *in28  = *on;
039300091009             *in90  = *on;
039400091009             *in58  = *on;
039500091009             V1Dmsg = $Msg(10);
039600091009             leavesr;
039700091009           when  V1CipsF > *zero;
039800091009             chain  (V1CipsF)  AZORG;
039900091009             if  Not %found(AZORG01L);
040000091009               *in28  = *on;
040100091009               *in90  = *on;
040200091009               *in58  = *on;
040300091009               V1Dmsg = $Msg(5);
040400091009               leavesr;
040500091009             endif;
040600091009         endsl;
040700091009
040800091009         // - Parzializzazione per filiale in selezione
040900091009         //   "SOLO destinatari con forzatura giro consegna"
041000091009         select;
041100091009           when  V1Ctip = 'P';
041200091009           when  V1Cfgi = *blank   and   V1CfgiF > *zero;
041300091009             *in28  = *on;
041400091009             *in90  = *on;
041500091009             *in59  = *on;
041600091009             V1Dmsg = $Msg(11);
041700091009             leavesr;
041800091009           when  V1CfgiF > *zero;
041900091009             chain  (V1CfgiF)  AZORG;
042000091009             if  Not %found(AZORG01L);
042100091009               *in28  = *on;
042200091009               *in90  = *on;
042300091009               *in59  = *on;
042400091009               V1Dmsg = $Msg(5);
042500091009               leavesr;
042600091009             endif;
042700091009         endsl;
042800091009      /end-free
042900060109      *
043000060109      * P.O. DI INS/MOD
043100060109if  1c                   if             V1Ctip  = 'P'
043200060109     c                             and  V1Cpou <> *zeros
043300060109     c                             and  V1Cpou <> 999
043400060109     c     V1Cpou        chain     AZORG
043500060109if  2c                   if        NOT %found(AZORG01L)
043600060109     c                              or  ORGfva <> *blanks
043700060414     c***                           or (ORGfag <> 'F'
043800060414     c***                           and ORGfag <> 'A')
043900060109     c                   seton                                        285590
044000060414     c                   movel     $Msg(5)       V1Dmsg
044100060109     c                   leavesr
044200060109e   2c                   endif
044300060109e   1c                   endif
044400060109      *
044500060109      * OBBLIGATORIETA' P.O. PER TIPOLOGIA PROPOSTE 'A' (APPUNTAMENTO)
044600060109if  1c                   if             V1Ctip  = 'P'
044700060109     c                             and  V1Cdos  = 'A'
044800060109     c                             and  V1Cpou  = *zeros
044900060109     c                   seton                                        285590
045000060414     c                   movel     $Msg(6)       V1Dmsg
045100060109     c                   leavesr
045200060109e   1c                   endif
045300060109      *
045400060109      * OBBLIGATORIETA' PARAMETRIZZAZIONE PER LANCIO PROPOSTE
045500060109if  1c                   if             V1Ctip  = 'P'
045600060109     c                             and  V1Cprv  = *blanks
045700060109     c                             and  V1Cpou  = *zeros
045800060109     c                   seton                                        28  90
045900060109     c                   seton                                        5255
046000060414     c                   movel     $Msg(7)       V1Dmsg
046100060109     c                   leavesr
046200060109e   1c                   endif
046300060109      *
046400060109      * SALTI PAGINE PROVINCIA O P.O. ALTERNATIVI
046500060109if  1c                   if             V1Ctip  = 'P'
046600060109     c                             and  V1Crot  = 'S'
046700060109     c                             and  V1Crtpo = 'S'
046800060109     c                   seton                                        285690
046900060414     c                   movel     $Msg(8)       V1Dmsg
047000060109     c                   leavesr
047100060109e   1c                   endif
047200060109      *
047300060109     c                   ENDSR
047400060109      *
047500060109      *---------------------------------------------------------------*
047600060109      * Sottomissione lavoro batch                                    *
047700060109      *---------------------------------------------------------------*
047800060109     c     SbmJob        BEGSR
047900060109      *
048000060109     c                   eval      PARnaz    = V1Cnaz
048100060109     c                   eval      PARprv    = V1Cprv
048200060109     c                   eval      PARrag    = V1Crag
048300091009     c                   eval      PARcapI   = V1CcapI
048400091009     c                   eval      PARcapF   = V1CcapF
048500060109     c                   eval      PARdos    = V1Cdos
048600060109     c                   eval      PARsin    = V1Csin
048700060414     c                   eval      PARddt    = V1Cddt
048800060414     c                   eval      PARips    = V1Cips
048900091009     c                   eval      PARipsF   = V1CipsF
049000070326     c                   eval      PARfgi    = V1Cfgi
049100091009     c                   eval      PARfgiF   = V1CfgiF
049200090708     c                   eval      PARfic    = V1Cfic
049300060414     c                   eval      PARric    = V1Cric
049400060109     c                   eval      PARpou    = DUTpou
049500060109     c                   eval      PARpouvid = V1Cpou
049600060414     c                   eval      PARrot    = V1Crot
049700060109     c                   eval      PARrtpo   = V1Crtpo
049800070913     c                   eval      PARgcx    = V1Cgcx
049900060109     c                   movel(p)  Param         KPJBU
050000060109      *
050100070326     c                   if        KNMUS     = *all'1'
050200060109     c                   if        V1Ctip    = 'C'
050300060109     c                   call      'TISITCR'
050400060109     c                   parm                    KPJBA
050500060109     c                   else
050600060109     c                   call      'TISITPR'
050700060109     c                   parm                    KPJBA
050800060109     c                   endif
050900060109     c                   else
051000060109     c                   if        V1Ctip    = 'C'
051100060109     c                   eval      KCOAZ     = 'SITC'
051200060109     c                   else
051300060109     c                   eval      KCOAZ     = 'SITP'
051400060109     c                   endif
051500060109     c                   call      'BCH10'
051600060109     c                   parm                    KPJBA
051700060109     c                   endif
051800060109      *
051900060109      * Imposto il flag per l'uscita automatica a fine elaborazione
052000060109     c                   eval      $Fine     = *on
052100060109      *
052200060109     c                   ENDSR
052300070913
052400070913** - $GGS/$GIO
0525000709131LUNEDI
0526000709132MARTEDI
0527000709133MERCOLEDI
0528000709134GIOVEDI
0529000709135VENERDI
0530000709136SABATO
0531000709137DOMENICA
053200070913M          MATTINA
053300070913P          POMERIGGIO
053400060109** - $MSG -------------------------------------------------------------------*
053500060109TISITSR- Nazione inesistente, correggere o non indicare.                       1
053600060109TISITSR- Provincia inesistente, correggere o non indicare.                     2
053700060109TISITSR- Indicare almeno 3 caratteri.                                          3
053800060109TISITSR- Tipo destinatario errato                                              4
053900091009TISITSR- Filiale indicata inesistente, correggere.                             5
054000060109TISITSR- Per tipo particolarità 'A' è obbligatorio indicare il P.O. utente.    6
054100060109TISITSR- E' obbligatorio indicare o la provincia o il P.O. utente              7
054200060109TISITSR- Possibile un solo salto pagina                                        8
054300091009TISITSR- Range di C.a.p. errato                                                9
054400091009Fil. inseribile solo se "SOLO DESTINATARI con instradamento prestabilito" = S  10
054500091009Fil. inseribile solo se "SOLO DESTINATARI con forzatura giro cnsegna" = S      11
