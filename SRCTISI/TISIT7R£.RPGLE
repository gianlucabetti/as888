000100060109      *===============================================================*
000200060109      *?TISIT7R * Creazione proposte destinatari particolari         ?*
000300060109      *===============================================================*
000400060109
000500060109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060109
000700060109     fTISIT7D   cf   e             workstn
000800060109
000900060109      *---------------------------------------------------------------*
001000060109
001100060109      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001200060109      *
001300060109      * - Tasti di funzione
001400060109     d Enter           c                   const(x'F1')
001500060109      *
001600060109      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001700060109      *
001800060109      * - Messaggi
001900060109     d $Msg            s             78    dim( 3)  ctdata  perrcd(1)
002000060109      *
002100060109      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200060109      *
002300060109     d KPJBA         e ds
002400060109     d Param           ds
002500060109     d  PARdai                        8s 0 inz(*zeros)
002600060109     d  PARdaf                        8s 0 inz(*zeros)
002700060109     d  PARdlf                        8s 0 inz(*zeros)
002800060109     d  PARpou                             inz like(DUTpou)
002900060109      * - Parametri x Controllo profilo utenti
003000060109     d TIBS34ds      e ds
003100060109      * - Ds di riferimento al file esterno AZUTE00F
003200060109     d AZUTEds       e ds                  extname(AZUTE00F)
003300060109      * - Ds per dati organigramma
003400060109     d DDatiUte      e ds
003500060109      * - Ds flags abilitazioni utenti aziendali
003600060109     d dUTE01        e ds                                                       *Abilitazioni
003700060109      *
003800060109     d Status         sds           333
003900060109     d   SDSpgm          *proc
004000060109      *
004100060109      * - Controllo/Inversione date
004200060109     d WLBdat          ds                  inz
004300060109     d   G08dat                       8  0 inz
004400060109     d   G08inv                       8  0 inz
004500060109     d   G08err                       1    inz('3')
004600060109     d   G08tgi                       5  0 inz
004700060109      *
004800060109      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
004900060109      *
005000060109      * - Flags
005100060109     d $Fine           s              1    inz(*off)
005200060109     d $InzD01         s              1    inz(*on)
005300060109
005400060109      *---------------------------------------------------------------*
005500060109      *  RIEPILOGO INDICATORI                                         *
005600060109      *---------------------------------------------------------------*
005700060109      * 10    - Comodo                                                *
005800060109      * 28    - Visualizza messaggio di errore                        *
005900060109      * 51    - Data iniziale bolle errata o mancante                 *
006000060109      * 52    - Data finale   bolle errata o mancante                 *
006100060109      * 53    - Data finale mantenimento lanci errata o mancante      *
006200060109      * 90    - Errore generico                                       *
006300060109      *---------------------------------------------------------------*
006400060109
006500060109     c     *Entry        plist
006600060109     c                   parm                    KPJBA
006700060109     c                   movel     KPJBU         Param
006800060109      *
006900060109      * Operazioni Iniziali
007000060109     c                   exsr      RoutInz
007100060109      *
007200060109      * Gestione Video
007300060109do  1c                   DOW       $Fine   = *off
007400060109     c                   exsr      GesD01
007500060109e   1c                   ENDDO
007600060109      *
007700060109      * Fine
007800060109     c                   movel     *on           *inLR
007900060109      *
008000060109      *---------------------------------------------------------------*
008100060109      * Operazioni Iniziali                                           *
008200060109      *---------------------------------------------------------------*
008300060109     c     RoutInz       BEGSR
008400060109      *
008500060109      * Reperisco dati job
008600060109     c                   exsr      DatiJob
008700060109      *
008800060109     c                   movel     SDSpgm        V1Tpgm
008900060109      *
009000060109     c                   ENDSR
009100060109      *
009200060109      *---------------------------------------------------------------*
009300060109      * Reperimento Dati del job (Utente/Operativi)                   *
009400060109      *---------------------------------------------------------------*
009500060109     c     DatiJob       BEGSR
009600060109      *
009700060109     c     *dtaara       define    §azute        azuteds
009800060109     c     *dtaara       define    §datiute      ddatiute
009900060109      *
010000060109     c                   in(E)     *dtaara
010100060109     c                   IF        %ERROR or RSUT = *blanks
010200060109     c                   clear                   Tibs34Ds
010300060109     c                   call      'TIBS34R'
010400060109     c                   parm                    Tibs34Ds
010500060109     c                   in        *dtaara
010600060109     c                   ENDIF
010700060109      *
010800060109     c                   ENDSR
010900060109      *
011000060109      *---------------------------------------------------------------*
011100060109      * Gestione videata D01                                          *
011200060109      *---------------------------------------------------------------*
011300060109     c     GesD01        BEGSR
011400060109      *
011500060109      * Inizializzo la videata
011600060109if  1c                   if        $InzD01 = *on
011700060109     c                   exsr      InzD01
011800060109     c                   movel     *off          $InzD01
011900060109e   1c                   endif
012000060109      *
012100060109      * Scrivo la testata e la riga tasti funzionali abilitati
012200060109if  1c                   if        NOT *in90
012300060109     c                   write     SIT7T01
012400060109     c                   write     SIT7P01
012500060109e   1c                   endif
012600060109      * Emetto la videata
012700060109     c                   exfmt     SIT7D01
012800060109     c                   setoff                                       28  90
012900060109     c                   clear                   V1Dmsg
013000060109      *
013100060109sel 1c                   select
013200060109      * F3=Fine
013300060109w   1c                   when      *inKC
013400060109     c                   exsr      F03D01
013500060109     c                   leavesr
013600060109      * F12=Ritorno
013700060109w   1c                   when      *inKL
013800060109     c                   exsr      F03D01
013900060109     c                   leavesr
014000060109      *
014100060109e   1c                   endsl
014200060109      *
014300060109      * Invio o F6
014400060109     c                   exsr      CtrD01
014500060109if  1c                   if        *in90
014600060109     c                   leavesr
014700060109e   1c                   endif
014800060109      * F6
014900060109if  1c                   if        *inKF   = *on
015000060109     c                   exsr      SbmJob
015100060109e   1c                   endif
015200060109      *
015300060109     c                   ENDSR
015400060109      *
015500060109      *---------------------------------------------------------------*
015600060109      * Inizializzazione videata D01                                  *
015700060109      *---------------------------------------------------------------*
015800060109     c     InzD01        BEGSR
015900060109      *
016000060109     c                   clear                   SIT7D01
016100060109      *
016200060109     c                   eval      V1Cdlf = 01012000
016300060109      *
016400060109     c                   ENDSR
016500060109      *
016600060109      *---------------------------------------------------------------*
016700060109      * Gestione tasto funzionale F03 da videata D01                  *
016800060109      *---------------------------------------------------------------*
016900060109     c     F03D01        BEGSR
017000060109      *
017100060109      * Chiudo il programma
017200060109     c                   eval      $Fine   = *on
017300060109      *
017400060109     c                   ENDSR
017500060109      *
017600060109      *---------------------------------------------------------------*
017700060109      * Controllo dati immessi in videata D01                         *
017800060109      *---------------------------------------------------------------*
017900060109     c     CtrD01        BEGSR
018000060109      *
018100060109     c                   movea     *zeros        *in(51)
018200060109      *
018300060109      * Controllo data iniziale
018400060109     c                   if        V1Cdai =  *zeros
018500060109     c                   eval      V1Dmsg =  $Msg(1)
018600060109     c                   seton                                        285190
018700060109     c                   leavesr
018800060109     c                   endif
018900060109     c                   clear                   WLBdat
019000060109     c                   eval      G08dat =  V1Cdai
019100060109     c                   call      'XSRDA8'
019200060109     c                   parm                    WLBdat
019300060109     c                   if        G08err =  '1'
019400060109     c                   eval      V1Dmsg =  $Msg(2)
019500060109     c                   seton                                        285190
019600060109     c                   leavesr
019700060109     c                   endif
019800060109     c                   eval      V1Cdai =  G08dat
019900060109     c                   eval      PARdai =  G08inv
020000060109      *
020100060109      * Controllo data finale
020200060109     c                   if        V1Cdaf =  *zeros
020300060109     c                   eval      V1Dmsg =  $Msg(1)
020400060109     c                   seton                                        285290
020500060109     c                   leavesr
020600060109     c                   endif
020700060109     c                   clear                   WLBdat
020800060109     c                   eval      G08dat =  V1Cdaf
020900060109     c                   call      'XSRDA8'
021000060109     c                   parm                    WLBdat
021100060109     c                   if        G08err =  '1'
021200060109     c                   eval      V1Dmsg =  $Msg(2)
021300060109     c                   seton                                        285290
021400060109     c                   leavesr
021500060109     c                   endif
021600060109     c                   eval      V1Cdaf =  G08dat
021700060109     c                   eval      PARdaf =  G08inv
021800060109      *
021900060109      * Controllo sequenza data iniziale - data finale
022000060109     c                   if        PARdai >  PARdaf
022100060109     c                   eval      V1Dmsg =  $Msg(3)
022200060109     c                   seton                                        28  90
022300060109     c                   seton                                        5253
022400060109     c                   leavesr
022500060109     c                   endif
022600060109      *
022700060109      * Controllo data finale mantenimento lanci precedenti
022800060109     c                   clear                   WLBdat
022900060109     c                   eval      G08dat =  V1Cdlf
023000060109     c                   call      'XSRDA8'
023100060109     c                   parm                    WLBdat
023200060109     c                   if        G08err =  '1'
023300060109     c                   eval      V1Dmsg =  $Msg(2)
023400060109     c                   seton                                        285390
023500060109     c                   leavesr
023600060109     c                   endif
023700060109     c                   eval      V1Cdlf =  G08dat
023800060109     c                   eval      PARdlf =  G08inv
023900060109      *
024000060109     c                   ENDSR
024100060109      *
024200060109      *---------------------------------------------------------------*
024300060109      * Sottomissione lavoro                                          *
024400060109      *---------------------------------------------------------------*
024500060109     c     SbmJob        BEGSR
024600060109      *
024700060109      * Imposto i parametri di lancio
024800060109     c*** già girata:    eval      PARdai =  V1Cdai
024900060109     c*** già girata:    eval      PARdaf =  V1Cdaf
025000060109     c*** già girata:    eval      PARdlf =  V1Cdlf
025100060109     c                   eval      PARpou =  DUTpou
025200060109      *
025300060109      * Lancio il programma di elaborazione
025400060109     c                   eval      KCOAZ  =  'SIT8'
025500060109     c                   movel(p)  Param         KPJBU
025600060109     c                   if        KNMUS  =  '1111111111'
025700060109     c                   call      'TISIT8C'
025800060109     c                   parm                    KPJBA
025900060109     c                   else
026000060109     c                   call      'BCH10'
026100060109     c                   parm                    KPJBA
026200060109     c                   endif
026300060109      *
026400060109      * Imposto il flag per l'uscita automatica a fine elaborazione
026500060109     c                   eval      $Fine  =  *on
026600060109      *
026700060109     c                   ENDSR
026800060109
026900060109      *---------------------------------------------------------------*
027000060109
027100060109** - $MSG -------------------------------------------------------------------*
027200060109TISIT7R- Data obbligatoria.                                                    1
027300060109TISIT7R- Data non accettabile.                                                 2
027400060109TISIT7R- Data inizio maggiore di data fine.                                    3
