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
002200070913     d $Msg            s             78    dim( 9)  ctdata  perrcd(1)
002300060109      *
002400060109      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002500060109      *
002600060109     d KPJBA         e ds
002700060109     d Param           ds
002800060414     d  PARnaz                        3                                         Nazione
002900060414     d  PARprv                        3                                         Provincia
003000060414     d  PARrag                       35                                         Ragione Sociale
003100060414     d  PARdos                        1                                         Tipo Destinatario
003200060414     d  PARsin                        1                                         Dettaglio Sinonimi
003300060414     d  PARddt                        1                                         Dest. con obbl.DDT
003400060414     d  PARips                        1                                         Dest. con inst.prest
003500070913     d  PARfgi                        1                                         Dest. con forz.giro
003600060414     d  PARric                        7s 0                                      Ricorrenza > di
003700060414     d  PARpou                        3s 0                                      P.O. utente ins/mod
003800060414     d  PARpouvid                     3s 0                                      P.O. utente ins/mod
003900060414     d  PARrot                        1                                         Salto pag. x Prov.
004000060414     d  PARrtpo                       1                                         Salto pag. x P.O.
004100070913     d  PARgcx                        1                                         Giorno di chiusura
004200090708     d  PARfic                        1                                         Dest. con forz.t.i.c
004300060109      * - Parametri x Controllo profilo utenti
004400060109     d TIBS34ds      e ds
004500060109      * - Ds di riferimento al file esterno AZUTE00F
004600060109     d AZUTEds       e ds                  extname(AZUTE00F)
004700060109      * - Ds per dati organigramma
004800070913     d dDatiUte      e ds
004900060104      *
005000070917      * - Parametri x ricerca/controllo tabelle
005100060104     d TIBS02ds      e ds                  inz
005200060104     d   T02mod      e                     inz('C')
005300060104     d   T02cod      e                     inz('TDP')
005400060104      * - Tab. "TDP" = Tipi Destinatari Particolari
005500060104     d dTDP          e ds                  inz
005600070917      * - Tab. "VPO/DECO" = Variabili Programmi Operativi: Decorrenze
005700070917     d dVPOdeco      e ds                  inz
005800060109      *
005900060109     d Status         sds           333
006000060109     d   SDSpgm          *proc
006100060109      *
006200060109      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006300060109      *
006400060109      * - Flags
006500060109     d $Fine           s              1    inz(*off)
006600060109     d $InzD01         s              1    inz(*on)
006700070913      *
006800070913      * - Indici
006900070913     d xx              s              3  0 inz
007000070913      *
007100070913      * - Campi di comodo
007200070917     d wDate_Iso       s               d   inz   datfmt(*iso)
007300070917     d wDate           s              8  0 inz
007400070913     d w001a           s              1    inz
007500060109      *
007600060109      * Key-List - - - - - - - - - - - - - - - - - - - - - - - - - - -*
007700060109      *
007800060109      * - TABEL00F
007900060109     c     K03tabel      klist
008000060109     c                   kfld                    TBLkut
008100060109     c                   kfld                    TBLcod
008200060109     c                   kfld                    TBLkey
008300060109
008400060109      *---------------------------------------------------------------*
008500060109      *  RIEPILOGO INDICATORI                                         *
008600060109      *---------------------------------------------------------------*
008700060109      * 28    - Visualizza messaggio di errore                        *
008800070917      * 41    - Richiesto "Tipo stampa: C=Confermati"                 *
008900070917      * 42    - Inesistenza WrkF proposte destinatari:                *
009000060414      *         consentiti solo tipi "C"                              *
009100070917      * 43    - Disabilita selezione "SOLO destinatari con gg chius." *
009200060414      * 51    - Nazione                  errata o mancante            *
009300060414      * 52    - Provincia                errata o mancante            *
009400060414      * 53    - Ragione sociale          errata o mancante            *
009500060414      * 54    - Tipo destinatario        errato o mancante            *
009600060414      * 55    - P.O. utente ins/mod      errato o mancante            *
009700060414      * 56    - Salto pagina x Prov/P.O  errato o mancante            *
009800060109      * 90    - Errore generico                                       *
009900060109      *---------------------------------------------------------------*
010000060109
010100060109     c     *Entry        plist
010200060109     c                   parm                    KPJBA
010300060109     c                   movel     KPJBU         Param
010400060109      *
010500060109      * Operazioni Iniziali
010600060109     c                   exsr      RoutInz
010700060109      *
010800060109      * Gestione Video
010900060109do  1c                   DOW       $Fine   = *off
011000060109     c                   exsr      GesD01
011100060109e   1c                   ENDDO
011200060109      *
011300060109      * Fine
011400060109     c                   clear                   TIBS02ds
011500060109     c                   eval      T02tla = 'C'
011600060109     c                   call      'TIBS02R'
011700060109     c                   parm                    KPJBA
011800060109     c                   parm                    TIBS02ds
011900060109      *
012000060109     c                   movel     *on           *inLR
012100060109      *
012200060109      *---------------------------------------------------------------*
012300060109      * Operazioni Iniziali                                           *
012400060109      *---------------------------------------------------------------*
012500060109     c     RoutInz       BEGSR
012600060109      *
012700060109      * Reperisco dati job
012800060109     c                   exsr      DatiJob
012900060109      *
013000060109     c                   movel     SDSpgm        V1Tpgm
013100060109     c                   eval      TBLkut  = 1
013200070917      *
013300070917      * Reperimento data corrente
013400070917     c                   movel     *date         wDate_Iso
013500070917     c                   move      wDate_Iso     wDate
013600070917      *
013700070917      * Aggancio tabella "VPO/DECO" per reperimento data attivazione
013800070917      *   dati in trk "XT1": filiale giro  e  giorni chiusura
013900070917     c                   clear                   dVPOdeco
014000070917     c                   clear                   TIBS02ds
014100070917     c                   eval      T02mod  = 'C'
014200070917     c                   eval      T02sif  = KNSIF
014300070917     c                   eval      T02cod  = 'VPO'
014400070917     c                   movel(P)  'DECO'        T02ke1
014500070917     c                   call      'TIBS02R'
014600070917     c                   parm                    KPJBA
014700070917     c                   parm                    TIBS02ds
014800070917    2c                   if        T02err  = *blanks
014900070917     c                   movel     T02uni        dVPOdeco
015000070917    2c                   endif
015100060109      *
015200060109     c                   ENDSR
015300060109      *
015400060109      *---------------------------------------------------------------*
015500060109      * Reperimento Dati del job (Utente/Operativi)                   *
015600060109      *---------------------------------------------------------------*
015700060109     c     DatiJob       BEGSR
015800060109      *
015900060109     c     *dtaara       define    §azute        azuteds
016000060109     c     *dtaara       define    §datiute      ddatiute
016100060109      *
016200060109     c                   in(E)     *dtaara
016300060109     c                   IF        %ERROR or RSUT = *blanks
016400060109     c                   clear                   Tibs34Ds
016500060109     c                   call      'TIBS34R'
016600060109     c                   parm                    Tibs34Ds
016700060109     c                   in        *dtaara
016800060109     c                   ENDIF
016900060109      *
017000060109     c                   ENDSR
017100060109      *
017200060109      *---------------------------------------------------------------*
017300060109      * Gestione videata D01                                          *
017400060109      *---------------------------------------------------------------*
017500060109     c     GesD01        BEGSR
017600060109      *
017700060109      * Inizializzo la videata
017800060109if  1c                   if        $InzD01 = *on
017900060109     c                   exsr      InzD01
018000060109     c                   movel     *off          $InzD01
018100060109e   1c                   endif
018200060109      *
018300060109      * Scrivo la testata e la riga tasti funzionali abilitati
018400060109if  1c                   if        NOT *in90
018500060109     c                   write     SITST01
018600060109     c                   write     SITSP01
018700060109e   1c                   endif
018800060109      * Emetto la videata
018900060109     c                   exfmt     SITSD01
019000060109     c                   setoff                                       28  90
019100060109     c                   clear                   V1Dmsg
019200060109      *
019300060109sel 1c                   select
019400060109      * F3=Fine
019500060109w   1c                   when      *inKC
019600060109     c                   exsr      F03D01
019700060109     c                   leavesr
019800060109      * F12=Ritorno
019900060109w   1c                   when      *inKL
020000060109     c                   exsr      F03D01
020100060109     c                   leavesr
020200060109      *
020300060109e   1c                   endsl
020400060109      *
020500060109      * Invio o F6
020600060109     c                   exsr      CtrD01
020700060109if  1c                   if        *in90
020800060109     c                   leavesr
020900060109e   1c                   endif
021000060109      * F6=Conferma
021100060109if  1c                   if        *inKF   = *on
021200060109     c                   exsr      SbmJob
021300060109e   1c                   endif
021400060109      *
021500060109     c                   ENDSR
021600060109      *
021700060109      *---------------------------------------------------------------*
021800060109      * Inizializzazione videata D01                                  *
021900060109      *---------------------------------------------------------------*
022000060109     c     InzD01        BEGSR
022100060109      *
022200060109     c                   clear                   SITSD01
022300060109      *
022400060109     c                   eval      V1Ctip  =  'C'
022500060109      *
022600060109      * Verifica esistenza WrkFile proposte destinatari
022700060109      * => SE non esiste: consento solo tipi "C"
022800060109     c                   open(e)   AZDSTP1L
022900060109     c                   if        %error
023000070917     c                   eval      *in42   =  *on
023100060109     c                   exsr      CtrD01
023200060109     c                   setoff                                       28  90
023300060109     c                   movea     *zeros        *in(51)
023400060109     c                   endif
023500070917      *
023600070917      * Impostazione indicatori
023700070917      * - Test se richiesta stampa di clienti potenziali
023800070917     c                   eval      *in41  =  (V1Ctip = 'C')
023900070917      * - Test se iniziata nuova gestione del trk "XT1" di AZDSE00F
024000070917     c                   eval      *in43   = (§VPOxt1 > Wdate)
024100060109      *
024200060109     c                   ENDSR
024300060109      *
024400060109      *---------------------------------------------------------------*
024500060109      * Gestione tasto funzionale F03 da videata D01                  *
024600060109      *---------------------------------------------------------------*
024700060109     c     F03D01        BEGSR
024800060109      *
024900060109      * Chiudo il programma
025000060109     c                   eval      $Fine   = *on
025100060109      *
025200060109     c                   ENDSR
025300060109      *
025400060109      *---------------------------------------------------------------*
025500060109      * Controllo dati immessi in videata D01                         *
025600060109      *---------------------------------------------------------------*
025700060109     c     CtrD01        BEGSR
025800060109      *
025900070917     c                   eval      *in41  =  (V1Ctip = 'C')
026000060109      *
026100060109     c                   movea     *zeros        *in(51)
026200060109      *
026300060109      * NAZIONE
026400060109     c     '?'           scan      V1Cnaz
026500060109sel 1c                   select
026600060109      * - ricerca
026700060109w   1c                   when      %found
026800060109     c                   clear                   V1Cnaz
026900060109     c                   call      'X§TABER'
027000060109     c                   parm                    TBLkut
027100060109     c                   parm      '15'          TBLcod
027200060109     c                   parm      *blanks       TBLkey
027300060109     c                   parm      *blanks       wDES             25
027400060109     c                   movel     TBLkey        V1Cnaz
027500060109     c                   eval      *in90    =  *on
027600060109     c                   leavesr
027700060109      * - "999" = Tutti
027800060829w   1c                   when      V1Cnaz   =  '999'
027900060109      * - controllo validità
028000060109x   1c                   other
028100060109     c                   eval      TBLcod   =  '15'
028200060109     c                   movel(p)  V1Cnaz        TBLkey
028300060109     c     K03tabel      chain     TABEL
028400060109if  2c                   if        NOT %found(TABEL00F)
028500060109     c                              or TBLflg <> *blanks
028600060109     c                   seton                                        285190
028700060414     c                   movel     $Msg(1)       V1Dmsg
028800060109     c                   leavesr
028900060109e   2c                   endif
029000060109e   1c                   endsl
029100060109      *
029200060109      * PROVINCIA
029300060109     c     '?'           scan      V1Cprv
029400060109sel 1c                   select
029500060109      * - ricerca
029600060109w   1c                   when      %found
029700060109     c                   clear                   V1Cprv
029800060109     c                   call      'X§TABER'
029900060109     c                   parm                    TBLkut
030000060109     c                   parm      'PR'          TBLcod
030100060109     c                   parm      *blanks       TBLkey
030200060109     c                   parm      *blanks       wDES
030300060109     c                   movel     TBLkey        V1Cprv
030400060109     c                   eval      *in90    =  *on
030500060109     c                   leavesr
030600060109      * - "  " = Tutte
030700060109w   1c                   when      V1Cprv   =  *blanks
030800060109      * - controllo validità
030900060109x   1c                   other
031000060109     c                   eval      TBLcod   =  'PR'
031100060109     c                   movel(p)  V1Cprv        TBLkey
031200060109     c     K03tabel      chain     TABEL
031300060109if  2c                   if        NOT %found(TABEL00F)
031400060109     c                              or TBLflg <> *blanks
031500060109     c                   seton                                        285290
031600060414     c                   movel     $Msg(2)       V1Dmsg
031700060109     c                   leavesr
031800060109e   2c                   endif
031900060109e   1c                   endsl
032000060109      *
032100060109      * RAGIONE SOCIALE
032200060109     c                   if             V1Crag <> *blanks
032300060109     c                             and  %len(%trim(V1Crag)) < 3
032400060109     c                   seton                                        285390
032500060414     c                   movel     $Msg(3)       V1Dmsg
032600060109     c                   leavesr
032700060109     c                   endif
032800060109      *
032900060109      * TIPO DESTINATARIO
033000060104     c                   clear                   V1Ddos
033100060109     c     '?'           scan      V1Cdos
033200060104sel 1c                   select
033300060109      * - ricerca
033400060104w   1c                   when      %found
033500060109     c                   clear                   V1Cdos
033600060109     c                   clear                   dTDP
033700060104     c                   clear                   TIBS02ds
033800060829     c                   eval      T02mod   =  'R'
033900060829     c                   eval      T02cod   =  'TDP'
034000060104     c                   call      'TIBS02R'
034100060104     c                   parm                    KPJBA
034200060104     c                   parm                    TIBS02ds
034300060104if  2c                   if             T02err  = *blanks
034400060104     c                             and  T02ke1 <> *blanks
034500060109     c                   movel     T02ke1        V1Cdos
034600060104     c                   movel     T02uni        dTDP
034700060104     c                   movel     §TDPdes       V1Ddos
034800060829     c                   eval      *in90    =  *on
034900060109     c                   leavesr
035000060104e   2c                   endif
035100060104      * - " " = Tutti
035200060829w   1c                   when      V1Cdos   =  *blanks
035300060109      * - controllo validità
035400060104x   1c                   other
035500060109     c                   clear                   dTDP
035600060109     c                   reset                   TIBS02ds
035700060109     c                   movel(p)  V1Cdos        T02ke1
035800060109     c                   call      'TIBS02R'
035900060109     c                   parm                    KPJBA
036000060109     c                   parm                    TIBS02ds
036100060829     c                   if        T02err   =  *blanks
036200060109     c                   movel     T02uni        dTDP
036300060109     c                   endif
036400060109     c                   movel     §TDPdes       V1Ddos
036500060829if  2c                   if        T02err   =  *blanks
036600060104     c                   movel     §TDPdes       V1Ddos
036700060104x   2c                   else
036800060109     c                   seton                                        285490
036900060414     c                   movel     $Msg(4)       V1Dmsg
037000060109     c                   leavesr
037100060104e   2c                   endif
037200060104e   1c                   endsl
037300060109      *
037400060109      * P.O. DI INS/MOD
037500060109if  1c                   if             V1Ctip  = 'P'
037600060109     c                             and  V1Cpou <> *zeros
037700060109     c                             and  V1Cpou <> 999
037800060109     c     V1Cpou        chain     AZORG
037900060109if  2c                   if        NOT %found(AZORG01L)
038000060109     c                              or  ORGfva <> *blanks
038100060414     c***                           or (ORGfag <> 'F'
038200060414     c***                           and ORGfag <> 'A')
038300060109     c                   seton                                        285590
038400060414     c                   movel     $Msg(5)       V1Dmsg
038500060109     c                   leavesr
038600060109e   2c                   endif
038700060109e   1c                   endif
038800060109      *
038900060109      * OBBLIGATORIETA' P.O. PER TIPOLOGIA PROPOSTE 'A' (APPUNTAMENTO)
039000060109if  1c                   if             V1Ctip  = 'P'
039100060109     c                             and  V1Cdos  = 'A'
039200060109     c                             and  V1Cpou  = *zeros
039300060109     c                   seton                                        285590
039400060414     c                   movel     $Msg(6)       V1Dmsg
039500060109     c                   leavesr
039600060109e   1c                   endif
039700060109      *
039800060109      * OBBLIGATORIETA' PARAMETRIZZAZIONE PER LANCIO PROPOSTE
039900060109if  1c                   if             V1Ctip  = 'P'
040000060109     c                             and  V1Cprv  = *blanks
040100060109     c                             and  V1Cpou  = *zeros
040200060109     c                   seton                                        28  90
040300060109     c                   seton                                        5255
040400060414     c                   movel     $Msg(7)       V1Dmsg
040500060109     c                   leavesr
040600060109e   1c                   endif
040700060109      *
040800060109      * SALTI PAGINE PROVINCIA O P.O. ALTERNATIVI
040900060109if  1c                   if             V1Ctip  = 'P'
041000060109     c                             and  V1Crot  = 'S'
041100060109     c                             and  V1Crtpo = 'S'
041200060109     c                   seton                                        285690
041300060414     c                   movel     $Msg(8)       V1Dmsg
041400060109     c                   leavesr
041500060109e   1c                   endif
041600060109      *
041700060109     c                   ENDSR
041800060109      *
041900060109      *---------------------------------------------------------------*
042000060109      * Sottomissione lavoro batch                                    *
042100060109      *---------------------------------------------------------------*
042200060109     c     SbmJob        BEGSR
042300060109      *
042400060109     c                   eval      PARnaz    = V1Cnaz
042500060109     c                   eval      PARprv    = V1Cprv
042600060109     c                   eval      PARrag    = V1Crag
042700060109     c                   eval      PARdos    = V1Cdos
042800060109     c                   eval      PARsin    = V1Csin
042900060414     c                   eval      PARddt    = V1Cddt
043000060414     c                   eval      PARips    = V1Cips
043100070326     c                   eval      PARfgi    = V1Cfgi
043200090708     c                   eval      PARfic    = V1Cfic
043300060414     c                   eval      PARric    = V1Cric
043400060109     c                   eval      PARpou    = DUTpou
043500060109     c                   eval      PARpouvid = V1Cpou
043600060414     c                   eval      PARrot    = V1Crot
043700060109     c                   eval      PARrtpo   = V1Crtpo
043800070913     c                   eval      PARgcx    = V1Cgcx
043900060109     c                   movel(p)  Param         KPJBU
044000060109      *
044100070326     c                   if        KNMUS     = *all'1'
044200060109     c                   if        V1Ctip    = 'C'
044300060109     c                   call      'TISITCR'
044400060109     c                   parm                    KPJBA
044500060109     c                   else
044600060109     c                   call      'TISITPR'
044700060109     c                   parm                    KPJBA
044800060109     c                   endif
044900060109     c                   else
045000060109     c                   if        V1Ctip    = 'C'
045100060109     c                   eval      KCOAZ     = 'SITC'
045200060109     c                   else
045300060109     c                   eval      KCOAZ     = 'SITP'
045400060109     c                   endif
045500060109     c                   call      'BCH10'
045600060109     c                   parm                    KPJBA
045700060109     c                   endif
045800060109      *
045900060109      * Imposto il flag per l'uscita automatica a fine elaborazione
046000060109     c                   eval      $Fine     = *on
046100060109      *
046200060109     c                   ENDSR
046300070913
046400070913** - $GGS/$GIO
0465000709131LUNEDI
0466000709132MARTEDI
0467000709133MERCOLEDI
0468000709134GIOVEDI
0469000709135VENERDI
0470000709136SABATO
0471000709137DOMENICA
047200070913M          MATTINA
047300070913P          POMERIGGIO
047400060109** - $MSG -------------------------------------------------------------------*
047500060109TISITSR- Nazione inesistente, correggere o non indicare.                       1
047600060109TISITSR- Provincia inesistente, correggere o non indicare.                     2
047700060109TISITSR- Indicare almeno 3 caratteri.                                          3
047800060109TISITSR- Tipo destinatario errato                                              4
047900060109TISITSR- P.O. indicato inesistente, correggere.                                5
048000060109TISITSR- Per tipo particolarità 'A' è obbligatorio indicare il P.O. utente.    6
048100060109TISITSR- E' obbligatorio indicare o la provincia o il P.O. utente              7
048200060109TISITSR- Possibile un solo salto pagina                                        8
048300070913TISITSR- Giorno di chiusura errato                                             9
