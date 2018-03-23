000100050811      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050811      *===============================================================*
000300050831      *?FIDN8AR * Lista Rimborsi C.A. affidate ad A.I.G.             ?*
000400050811      *===============================================================*
000500050811
000600050811     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700050811
000800050811      *---------------------------------------------------------------*
000900050811
001000050831     fFIDN8AD   cf   e             workstn
001100050811
001200050811      *---------------------------------------------------------------*
001300050811
001400050811      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001500050811      *
001600050811     d C_DataLimite    c                   const(20050418)
001700050811      *
001800050811      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001900050811      *
002000050811      * - Messaggi di errore
002100050905     d $Msg            s             78    dim(8) ctdata  perrcd(1)
002200050811      *
002300050811      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002400050811      *
002500050811     d KPJBA         e ds
002600050831     d FIDN8Bds      e ds                  inz
002700050811      * - Parametri x Controllo profilo utenti
002800050811     d TIBS34DS      e ds                  inz
002900050811      * - Ds di riferimento al file esterno AZUTE00F
003000050811     d AZUTEds       e ds                  extname(AZUTE00F)
003100050811      * - Ds per dati organigramma
003200050811     d DDatiUte      e ds
003300050811      *
003400050811     d Status         sds           333
003500050811     d   SDSpgm          *proc
003600050811      *
003700050811      * - Controllo/Inversione date
003800050811     d WLBdat          ds                  inz
003900050811     d   G08dat                       8  0 inz
004000050811     d   G08inv                       8  0 inz
004100050811     d   G08err                       1    inz('3')
004200050811     d   G08tgi                       5  0 inz
004300050811      *
004400050811      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
004500050811      *
004600050811      * - Flags
004700050811     d $Fine           s              1    inz(*off)
004800050811     d $InzD01         s              1    inz(*on)
004900050811      *
005000050811      * - Variabili riferite al data base o al display file
005100050905     d W1Cdci          s                   like(V1Cdci) inz
005200050905     d W1Cdcf          s                   like(V1Cdcf) inz
005300050811
005400050811      *---------------------------------------------------------------*
005500050811      *  RIEPILOGO INDICATORI                                         *
005600050811      *---------------------------------------------------------------*
005700050811      * 10    - Comodo                                                *
005800050905      * 28    - Emette il messaggio di errore a video                 *
005900050905      * 51    - Data chiusura C.A. iniziale formalmente errata        *
006000050905      * 52    - Data chiusura C.A. finale   formalmente errata        *
006100051011      * 53    - Flag C.A. con richiesta di rimborso da eseguire       *
006200051011      * 54    - Flag C.A. con richiesta di rimborso già eseguita      *
006300051011      * 55    - Flag C.A. già rimborsateci                            *
006400051011     *** * 53    - Flag C.A. già rimborsatici                            *
006500051011     *** * 54    - Flag C.A. ancora da rimborsarci                       *
006600051011     *** * 55    - Flag C.A. per le quali è già stato chiesto il         *
006700051011     *** *         rimborso                                              *
006800051011     *** * 56    - Flag C.A. per le quali si deve ancora chiedere il     *
006900051011     *** *         rimborso                                              *
007000050905      * 90    - Generico di errore                                    *
007100050811      *---------------------------------------------------------------*
007200050811
007300050811     c     *Entry        plist
007400050811     c                   parm                    KPJBA
007500050811      *
007600050811      * Operazioni Iniziali
007700050811     c                   exsr      RoutInz
007800050811      *
007900050811      * Gestione Video
008000050811do  1c                   dow       $Fine    = *off
008100050811     c                   exsr      GesD01
008200050811e   1c                   enddo
008300050811      *
008400050811      * Fine
008500050811     c                   movel     *on           *inLR
008600050811      *
008700050811      *---------------------------------------------------------------*
008800050811      * Operazioni Iniziali                                           *
008900050811      *---------------------------------------------------------------*
009000050811     c     RoutInz       BEGSR
009100050811      *
009200050811      * Reperisco dati job
009300050811     c                   exsr      DatiJob
009400050811      *
009500050811     c                   movel     SDSpgm        V1Tpgm
009600050811      *
009700050811     c                   ENDSR
009800050811      *
009900050811      *---------------------------------------------------------------*
010000050811      * Reperimento Dati del job (Utente/Operativi)                   *
010100050811      *---------------------------------------------------------------*
010200050811     c     DatiJob       BEGSR
010300050811      *
010400050811     c     *dtaara       define    §azute        azuteds
010500050811     c     *dtaara       define    §datiute      ddatiute
010600050811      *
010700050811     c                   in(E)     *dtaara
010800050811     c                   IF        %ERROR or RSUT = *blanks
010900050811     c                   clear                   Tibs34Ds
011000050811     c                   call      'TIBS34R'
011100050811     c                   parm                    Tibs34Ds
011200050811     c                   in        *dtaara
011300050811     c                   ENDIF
011400050811      *
011500050811     c                   ENDSR
011600050811      *
011700050811      *---------------------------------------------------------------*
011800050811      * Gestione videata D01                                          *
011900050811      *---------------------------------------------------------------*
012000050811     c     GesD01        BEGSR
012100050811      *
012200050811      * Inizializzo la videata
012300050811if  1c                   if        $InzD01  = *on
012400050811     c                   exsr      InzD01
012500050811     c                   eval      $InzD01  = *off
012600050811e   1c                   endif
012700050811      *
012800050811      * Scrivo la testata e la riga tasti funzionali abilitati
012900050811if  1c                   if        NOT *in90
013000050831     c                   write     FI8AT01
013100050831     c                   write     FI8AZ01
013200050811e   1c                   endif
013300050811      * Emetto la videata
013400050831     c                   exfmt     FI8AD01
013500050811     c                   setoff                                       28  90
013600050811     c                   clear                   V1Dmsg
013700050811      *
013800050811      * F3=Fine
013900050811if  1c                   if        *inKC
014000050811     c                   exsr      F03D01
014100050811     c                   leavesr
014200050811e   1c                   endif
014300050811      *
014400050811      * Controllo dati immessi a video
014500050811     c                   exsr      CtrD01
014600050811      *
014700050811      * F6=Conferma
014800050811if  1c                   if            *inKF
014900050811     c                             and NOT *in90
015000050811     c                   exsr      F06D01
015100050811e   1c                   endif
015200050811      *
015300050811     c                   ENDSR
015400050811      *
015500050811      *---------------------------------------------------------------*
015600050811      * Inizializzazione videata D01                                  *
015700050811      *---------------------------------------------------------------*
015800050811     c     InzD01        BEGSR
015900050811      *
016000050831     c                   clear                   FI8AD01
016100050811      *
016200050811     c                   ENDSR
016300050811      *
016400050811      *---------------------------------------------------------------*
016500050811      * Gestione tasto funzionale F03 da videata D01                  *
016600050811      *---------------------------------------------------------------*
016700050811     c     F03D01        BEGSR
016800050811      *
016900050811      * Chiudo il programma
017000050811     c                   eval      $Fine    = *on
017100050811      *
017200050811     c                   ENDSR
017300050811      *
017400050811      *---------------------------------------------------------------*
017500050811      * Gestione tasto funzionale F06 da videata D01                  *
017600050811      *---------------------------------------------------------------*
017700050811     c     F06D01        BEGSR
017800050811      *
017900050901     c                   eval      KCOAZ   =  'DN8B'
018000050811      *
018100050811      * Preparazione ds parametri
018200050831     c                   clear                   FIDN8Bds
018300050905     c                   eval      D8Bdci  =  W1Cdci
018400050905     c                   eval      D8Bdcf  =  W1Cdcf
018500050831     c                   eval      D8Bse1  =  V1Cse1
018600051011     c                   eval      D8Bse2  =  V1Cse2
018700050831     c                   eval      D8Bse3  =  V1Cse3
018800050831     c                   movel     FIDN8Bds      KPJBU
018900050811      *
019000050811      * Sottomissione lavoro batch
019100050831     c***                movel(p)  'FIDN8BR'     Wpgm             10
019200050811     c                   movel(p)  'BCH10'       Wpgm             10
019300050811     c                   call      Wpgm
019400050811     c                   parm                    KPJBA
019500050811      *
019600050811      * Chiusura programma
019700050811     c                   exsr      F03D01
019800050811      *
019900050811     c                   ENDSR
020000050811      *
020100050811      *---------------------------------------------------------------*
020200050811      * Controllo dati immessi in videata D01                         *
020300050811      *---------------------------------------------------------------*
020400050811     c     CtrD01        BEGSR
020500050811      *
020600050811     c                   movea     *zeros        *in(50)
020700050905     c                   clear                   W1Cdci
020800050905     c                   clear                   W1Cdcf
020900050811      *
021000050905      * Data chiusura C.A. iniziale
021100050905if  1c                   if        V1Cdci      = *zeros
021200050811     c                   seton                                        512890
021300050811     c                   eval      V1Dmsg      = $Msg(1)
021400050811     c                   leavesr
021500050811x   1c                   else
021600050811     c                   clear                   WLBdat
021700050905     c                   eval      G08dat      = V1Cdci
021800050811     c                   call      'XSRDA8'
021900050811     c                   parm                    WLBdat
022000050811if  2c                   if        G08err      = *on
022100050811     c                   seton                                        512890
022200050811     c                   eval      V1Dmsg      = $Msg(2)
022300050811     c                   leavesr
022400050811e   2c                   endif
022500050905     c                   eval      V1Cdci      = G08dat
022600050905     c                   eval      W1Cdci      = G08inv
022700050811e   1c                   endif
022800050905if  1c                   if        W1Cdci      < C_DataLimite
022900050901     c                   seton                                        512890
023000050811     c                   eval      V1Dmsg      = $Msg(3)
023100050811     c                   leavesr
023200050811e   1c                   endif
023300050905      *
023400050905      * Data chiusura C.A. finale
023500050905if  1c                   if        V1Cdcf      = *zeros
023600050905     c                   eval      W1Cdcf      = W1Cdci
023700050905x   1c                   else
023800050905     c                   clear                   WLBdat
023900050905     c                   eval      G08dat      = V1Cdcf
024000050905     c                   call      'XSRDA8'
024100050905     c                   parm                    WLBdat
024200050905if  2c                   if        G08err      = *on
024300050905     c                   seton                                        522890
024400050905     c                   eval      V1Dmsg      = $Msg(2)
024500050905     c                   leavesr
024600050905e   2c                   endif
024700050905     c                   eval      V1Cdcf      = G08dat
024800050905     c                   eval      W1Cdcf      = G08inv
024900050905e   1c                   endif
025000050905if  1c                   if        W1Cdcf      < C_DataLimite
025100050905     c                   seton                                        522890
025200050905     c                   eval      V1Dmsg      = $Msg(3)
025300050905     c                   leavesr
025400050905e   1c                   endif
025500050905      *
025600050905      * Range di date chiusura C.A.
025700050905if  1c                   if        W1Cdci      > W1Cdcf
025800050905     c                   seton                                        512890
025900051011     c                   eval      V1Dmsg      = $Msg(4)
026000050905     c                   leavesr
026100050905e   1c                   endif
026200050811      *
026300050811      * Selezioni
026400050831sel 1c                   select
026500050831w   1c                   when          V1Cse1  = *blanks
026600051011     c                             and V1Cse2  = *blanks
026700051011     c                             and V1Cse3  = *blanks
026800050905     c                   seton                                        532890
026900051011     c                   eval      V1Dmsg      = $Msg(5)
027000050811     c                   leavesr
027100051011w   1c                   when          (V1Cse1 = 'S'
027200051011     c                              or  V1Cse2 = 'S')
027300051011     c                             and  V1Cse3 = 'S'
027400051011     c                   seton                                        552890
027500051011     c                   eval      V1Dmsg      = $Msg(6)
027600050831     c                   leavesr
027700050831e   1c                   endsl
027800050811      *
027900050811     c                   ENDSR
028000050811**   $MSG -------------------------------------------------------------------*
028100050811Immettere una data chiusura C.A.                                               1
028200050811Data chiusura C.A. formalmente errata                                          2
028300050811Data chiusura C.A. antecedente il giorno 18/04/2005                            3
028400051011Data chiusura C.A. iniziale   >   Data chiusura C.A. finale                    4
028500051011Effettuare almeno una selezione                                                5
028600051011La selezione delle C.A. già rimborsateci è incompatibile con altre selezioni   6
