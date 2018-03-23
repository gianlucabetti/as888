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
001900060711     d $Msg            s             78    dim( 4)  ctdata  perrcd(1)
002000060109      *
002100060109      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200060109      *
002300060109     d KPJBA         e ds
002400060711     d Param           ds                  inz
002500060711     d  PARtla                        1    inz
002600060109     d  PARdai                        8s 0 inz(*zeros)
002700060109     d  PARdaf                        8s 0 inz(*zeros)
002800060109     d  PARdlf                        8s 0 inz(*zeros)
002900060109     d  PARpou                             inz like(DUTpou)
003000060109      * - Parametri x Controllo profilo utenti
003100060109     d TIBS34ds      e ds
003200060109      * - Ds di riferimento al file esterno AZUTE00F
003300060109     d AZUTEds       e ds                  extname(AZUTE00F)
003400060109      * - Ds per dati organigramma
003500060109     d DDatiUte      e ds
003600060109      * - Ds flags abilitazioni utenti aziendali
003700060109     d dUTE01        e ds                                                       *Abilitazioni
003800060109      *
003900060109     d Status         sds           333
004000060109     d   SDSpgm          *proc
004100060109      *
004200060109      * - Controllo/Inversione date
004300060109     d WLBdat          ds                  inz
004400060109     d   G08dat                       8  0 inz
004500060109     d   G08inv                       8  0 inz
004600060109     d   G08err                       1    inz('3')
004700060109     d   G08tgi                       5  0 inz
004800060109      *
004900060109      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
005000060109      *
005100060109      * - Flags
005200060109     d $Fine           s              1    inz(*off)
005300060109     d $InzD01         s              1    inz(*on)
005400060109
005500060109      *---------------------------------------------------------------*
005600060109      *  RIEPILOGO INDICATORI                                         *
005700060109      *---------------------------------------------------------------*
005800060109      * 10    - Comodo                                                *
005900060109      * 28    - Visualizza messaggio di errore                        *
006000060109      * 51    - Data iniziale bolle errata o mancante                 *
006100060109      * 52    - Data finale   bolle errata o mancante                 *
006200060109      * 53    - Data finale mantenimento lanci errata o mancante      *
006300060109      * 90    - Errore generico                                       *
006400060109      *---------------------------------------------------------------*
006500060109
006600060109     c     *Entry        plist
006700060109     c                   parm                    KPJBA
006800060109     c                   movel     KPJBU         Param
006900060109      *
007000060109      * Operazioni Iniziali
007100060109     c                   exsr      RoutInz
007200060109      *
007300060109      * Gestione Video
007400060109do  1c                   DOW       $Fine   = *off
007500060109     c                   exsr      GesD01
007600060109e   1c                   ENDDO
007700060109      *
007800060109      * Fine
007900060109     c                   movel     *on           *inLR
008000060109      *
008100060109      *---------------------------------------------------------------*
008200060109      * Operazioni Iniziali                                           *
008300060109      *---------------------------------------------------------------*
008400060109     c     RoutInz       BEGSR
008500060109      *
008600060109      * Reperisco dati job
008700060109     c                   exsr      DatiJob
008800060109      *
008900060109     c                   movel     SDSpgm        V1Tpgm
009000060109      *
009100060109     c                   ENDSR
009200060109      *
009300060109      *---------------------------------------------------------------*
009400060109      * Reperimento Dati del job (Utente/Operativi)                   *
009500060109      *---------------------------------------------------------------*
009600060109     c     DatiJob       BEGSR
009700060109      *
009800060109     c     *dtaara       define    §azute        azuteds
009900060109     c     *dtaara       define    §datiute      ddatiute
010000060109      *
010100060109     c                   in(E)     *dtaara
010200060109     c                   IF        %ERROR or RSUT = *blanks
010300060109     c                   clear                   Tibs34Ds
010400060109     c                   call      'TIBS34R'
010500060109     c                   parm                    Tibs34Ds
010600060109     c                   in        *dtaara
010700060109     c                   ENDIF
010800060109      *
010900060109     c                   ENDSR
011000060109      *
011100060109      *---------------------------------------------------------------*
011200060109      * Gestione videata D01                                          *
011300060109      *---------------------------------------------------------------*
011400060109     c     GesD01        BEGSR
011500060109      *
011600060109      * Inizializzo la videata
011700060109if  1c                   if        $InzD01 = *on
011800060109     c                   exsr      InzD01
011900060109     c                   movel     *off          $InzD01
012000060109e   1c                   endif
012100060109      *
012200060109      * Scrivo la testata e la riga tasti funzionali abilitati
012300060109if  1c                   if        NOT *in90
012400060109     c                   write     SIT7T01
012500060109     c                   write     SIT7P01
012600060109e   1c                   endif
012700060109      * Emetto la videata
012800060109     c                   exfmt     SIT7D01
012900060109     c                   setoff                                       28  90
013000060109     c                   clear                   V1Dmsg
013100060109      *
013200060109sel 1c                   select
013300060109      * F3=Fine
013400060109w   1c                   when      *inKC
013500060109     c                   exsr      F03D01
013600060109     c                   leavesr
013700060109      * F12=Ritorno
013800060109w   1c                   when      *inKL
013900060109     c                   exsr      F03D01
014000060109     c                   leavesr
014100060109      *
014200060109e   1c                   endsl
014300060109      *
014400060109      * Invio o F6
014500060109     c                   exsr      CtrD01
014600060109if  1c                   if        *in90
014700060109     c                   leavesr
014800060109e   1c                   endif
014900060109      * F6
015000060109if  1c                   if        *inKF   = *on
015100060109     c                   exsr      SbmJob
015200060109e   1c                   endif
015300060109      *
015400060109     c                   ENDSR
015500060109      *
015600060109      *---------------------------------------------------------------*
015700060109      * Inizializzazione videata D01                                  *
015800060109      *---------------------------------------------------------------*
015900060109     c     InzD01        BEGSR
016000060109      *
016100060109     c                   clear                   SIT7D01
016200060109      *
016300060109     c                   eval      V1Cdlf = 01012000
016400060109      *
016500060109     c                   ENDSR
016600060109      *
016700060109      *---------------------------------------------------------------*
016800060109      * Gestione tasto funzionale F03 da videata D01                  *
016900060109      *---------------------------------------------------------------*
017000060109     c     F03D01        BEGSR
017100060109      *
017200060109      * Chiudo il programma
017300060109     c                   eval      $Fine   = *on
017400060109      *
017500060109     c                   ENDSR
017600060109      *
017700060109      *---------------------------------------------------------------*
017800060109      * Controllo dati immessi in videata D01                         *
017900060109      *---------------------------------------------------------------*
018000060109     c     CtrD01        BEGSR
018100060109      *
018200060109     c                   movea     *zeros        *in(51)
018300060711      *
018400060711      * Controllo "Tipo Lavoro"
018500060711     c                   if             V1Ctla <> *blanks
018600060711     c                             and  V1Ctla <> 'U'
018700060711     c                   eval      V1Dmsg =  $Msg(4)
018800060711     c                   seton                                        285490
018900060711     c                   leavesr
019000060711     c                   endif
019100060109      *
019200060109      * Controllo data iniziale
019300060109     c                   if        V1Cdai =  *zeros
019400060109     c                   eval      V1Dmsg =  $Msg(1)
019500060109     c                   seton                                        285190
019600060109     c                   leavesr
019700060109     c                   endif
019800060109     c                   clear                   WLBdat
019900060109     c                   eval      G08dat =  V1Cdai
020000060109     c                   call      'XSRDA8'
020100060109     c                   parm                    WLBdat
020200060109     c                   if        G08err =  '1'
020300060109     c                   eval      V1Dmsg =  $Msg(2)
020400060109     c                   seton                                        285190
020500060109     c                   leavesr
020600060109     c                   endif
020700060109     c                   eval      V1Cdai =  G08dat
020800060109     c                   eval      PARdai =  G08inv
020900060109      *
021000060109      * Controllo data finale
021100060109     c                   if        V1Cdaf =  *zeros
021200060109     c                   eval      V1Dmsg =  $Msg(1)
021300060109     c                   seton                                        285290
021400060109     c                   leavesr
021500060109     c                   endif
021600060109     c                   clear                   WLBdat
021700060109     c                   eval      G08dat =  V1Cdaf
021800060109     c                   call      'XSRDA8'
021900060109     c                   parm                    WLBdat
022000060109     c                   if        G08err =  '1'
022100060109     c                   eval      V1Dmsg =  $Msg(2)
022200060109     c                   seton                                        285290
022300060109     c                   leavesr
022400060109     c                   endif
022500060109     c                   eval      V1Cdaf =  G08dat
022600060109     c                   eval      PARdaf =  G08inv
022700060109      *
022800060109      * Controllo sequenza data iniziale - data finale
022900060109     c                   if        PARdai >  PARdaf
023000060109     c                   eval      V1Dmsg =  $Msg(3)
023100060109     c                   seton                                        28  90
023200060109     c                   seton                                        5253
023300060109     c                   leavesr
023400060109     c                   endif
023500060109      *
023600060109      * Controllo data finale mantenimento lanci precedenti
023700060109     c                   clear                   WLBdat
023800060109     c                   eval      G08dat =  V1Cdlf
023900060109     c                   call      'XSRDA8'
024000060109     c                   parm                    WLBdat
024100060109     c                   if        G08err =  '1'
024200060109     c                   eval      V1Dmsg =  $Msg(2)
024300060109     c                   seton                                        285390
024400060109     c                   leavesr
024500060109     c                   endif
024600060109     c                   eval      V1Cdlf =  G08dat
024700060109     c                   eval      PARdlf =  G08inv
024800060109      *
024900060109     c                   ENDSR
025000060109      *
025100060109      *---------------------------------------------------------------*
025200060109      * Sottomissione lavoro                                          *
025300060109      *---------------------------------------------------------------*
025400060109     c     SbmJob        BEGSR
025500060109      *
025600060109      * Imposto i parametri di lancio
025700060109     c*** già girata:    eval      PARdai =  V1Cdai
025800060109     c*** già girata:    eval      PARdaf =  V1Cdaf
025900060109     c*** già girata:    eval      PARdlf =  V1Cdlf
026000060109     c                   eval      PARpou =  DUTpou
026100060711     c                   eval      PARtla =  V1Ctla
026200060109      *
026300060109      * Lancio il programma di elaborazione
026400060109     c                   eval      KCOAZ  =  'SIT8'
026500060109     c                   movel(p)  Param         KPJBU
026600060109     c                   if        KNMUS  =  '1111111111'
026700060109     c                   call      'TISIT8C'
026800060109     c                   parm                    KPJBA
026900060109     c                   else
027000060109     c                   call      'BCH10'
027100060109     c                   parm                    KPJBA
027200060109     c                   endif
027300060109      *
027400060109      * Imposto il flag per l'uscita automatica a fine elaborazione
027500060109     c                   eval      $Fine  =  *on
027600060109      *
027700060109     c                   ENDSR
027800060109
027900060109      *---------------------------------------------------------------*
028000060109
028100060109** - $MSG -------------------------------------------------------------------*
028200060109TISIT7R- Data obbligatoria.                                                    1
028300060109TISIT7R- Data non accettabile.                                                 2
028400060109TISIT7R- Data inizio maggiore di data fine.                                    3
028500060711TISIT7R- Tipo lavoro errato.                                                   4
