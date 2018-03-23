000100050811      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200050811      *===============================================================*
000300051025      *?FIDN8AR * Lista Recuperi C.A. affidate ad A.I.G.             ?*
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
006100051025      * 53    - Flag C.A. con richiesta di recupero da eseguire       *
006200051025      * 54    - Flag C.A. con richiesta di recupero eseguita          *
006300051025      * 55    - Flag C.A. recuperate                                  *
006400050905      * 90    - Generico di errore                                    *
006500050811      *---------------------------------------------------------------*
006600050811
006700050811     c     *Entry        plist
006800050811     c                   parm                    KPJBA
006900050811      *
007000050811      * Operazioni Iniziali
007100050811     c                   exsr      RoutInz
007200050811      *
007300050811      * Gestione Video
007400050811do  1c                   dow       $Fine    = *off
007500050811     c                   exsr      GesD01
007600050811e   1c                   enddo
007700050811      *
007800050811      * Fine
007900050811     c                   movel     *on           *inLR
008000050811      *
008100050811      *---------------------------------------------------------------*
008200050811      * Operazioni Iniziali                                           *
008300050811      *---------------------------------------------------------------*
008400050811     c     RoutInz       BEGSR
008500050811      *
008600050811      * Reperisco dati job
008700050811     c                   exsr      DatiJob
008800050811      *
008900050811     c                   movel     SDSpgm        V1Tpgm
009000050811      *
009100050811     c                   ENDSR
009200050811      *
009300050811      *---------------------------------------------------------------*
009400050811      * Reperimento Dati del job (Utente/Operativi)                   *
009500050811      *---------------------------------------------------------------*
009600050811     c     DatiJob       BEGSR
009700050811      *
009800050811     c     *dtaara       define    §azute        azuteds
009900050811     c     *dtaara       define    §datiute      ddatiute
010000050811      *
010100050811     c                   in(E)     *dtaara
010200050811     c                   IF        %ERROR or RSUT = *blanks
010300050811     c                   clear                   Tibs34Ds
010400050811     c                   call      'TIBS34R'
010500050811     c                   parm                    Tibs34Ds
010600050811     c                   in        *dtaara
010700050811     c                   ENDIF
010800050811      *
010900050811     c                   ENDSR
011000050811      *
011100050811      *---------------------------------------------------------------*
011200050811      * Gestione videata D01                                          *
011300050811      *---------------------------------------------------------------*
011400050811     c     GesD01        BEGSR
011500050811      *
011600050811      * Inizializzo la videata
011700050811if  1c                   if        $InzD01  = *on
011800050811     c                   exsr      InzD01
011900050811     c                   eval      $InzD01  = *off
012000050811e   1c                   endif
012100050811      *
012200050811      * Scrivo la testata e la riga tasti funzionali abilitati
012300050811if  1c                   if        NOT *in90
012400050831     c                   write     FI8AT01
012500050831     c                   write     FI8AZ01
012600050811e   1c                   endif
012700050811      * Emetto la videata
012800050831     c                   exfmt     FI8AD01
012900050811     c                   setoff                                       28  90
013000050811     c                   clear                   V1Dmsg
013100050811      *
013200050811      * F3=Fine
013300050811if  1c                   if        *inKC
013400050811     c                   exsr      F03D01
013500050811     c                   leavesr
013600050811e   1c                   endif
013700050811      *
013800050811      * Controllo dati immessi a video
013900050811     c                   exsr      CtrD01
014000050811      *
014100050811      * F6=Conferma
014200050811if  1c                   if            *inKF
014300050811     c                             and NOT *in90
014400050811     c                   exsr      F06D01
014500050811e   1c                   endif
014600050811      *
014700050811     c                   ENDSR
014800050811      *
014900050811      *---------------------------------------------------------------*
015000050811      * Inizializzazione videata D01                                  *
015100050811      *---------------------------------------------------------------*
015200050811     c     InzD01        BEGSR
015300050811      *
015400050831     c                   clear                   FI8AD01
015500050811      *
015600050811     c                   ENDSR
015700050811      *
015800050811      *---------------------------------------------------------------*
015900050811      * Gestione tasto funzionale F03 da videata D01                  *
016000050811      *---------------------------------------------------------------*
016100050811     c     F03D01        BEGSR
016200050811      *
016300050811      * Chiudo il programma
016400050811     c                   eval      $Fine    = *on
016500050811      *
016600050811     c                   ENDSR
016700050811      *
016800050811      *---------------------------------------------------------------*
016900050811      * Gestione tasto funzionale F06 da videata D01                  *
017000050811      *---------------------------------------------------------------*
017100050811     c     F06D01        BEGSR
017200050811      *
017300050901     c                   eval      KCOAZ   =  'DN8B'
017400050811      *
017500050811      * Preparazione ds parametri
017600050831     c                   clear                   FIDN8Bds
017700050905     c                   eval      D8Bdci  =  W1Cdci
017800050905     c                   eval      D8Bdcf  =  W1Cdcf
017900050831     c                   eval      D8Bse1  =  V1Cse1
018000051011     c                   eval      D8Bse2  =  V1Cse2
018100050831     c                   eval      D8Bse3  =  V1Cse3
018200050831     c                   movel     FIDN8Bds      KPJBU
018300050811      *
018400050811      * Sottomissione lavoro batch
018500050831     c***                movel(p)  'FIDN8BR'     Wpgm             10
018600050811     c                   movel(p)  'BCH10'       Wpgm             10
018700050811     c                   call      Wpgm
018800050811     c                   parm                    KPJBA
018900050811      *
019000050811      * Chiusura programma
019100050811     c                   exsr      F03D01
019200050811      *
019300050811     c                   ENDSR
019400050811      *
019500050811      *---------------------------------------------------------------*
019600050811      * Controllo dati immessi in videata D01                         *
019700050811      *---------------------------------------------------------------*
019800050811     c     CtrD01        BEGSR
019900050811      *
020000050811     c                   movea     *zeros        *in(50)
020100050905     c                   clear                   W1Cdci
020200050905     c                   clear                   W1Cdcf
020300050811      *
020400050905      * Data chiusura C.A. iniziale
020500050905if  1c                   if        V1Cdci      = *zeros
020600050811     c                   seton                                        512890
020700050811     c                   eval      V1Dmsg      = $Msg(1)
020800050811     c                   leavesr
020900050811x   1c                   else
021000050811     c                   clear                   WLBdat
021100050905     c                   eval      G08dat      = V1Cdci
021200050811     c                   call      'XSRDA8'
021300050811     c                   parm                    WLBdat
021400050811if  2c                   if        G08err      = *on
021500050811     c                   seton                                        512890
021600050811     c                   eval      V1Dmsg      = $Msg(2)
021700050811     c                   leavesr
021800050811e   2c                   endif
021900050905     c                   eval      V1Cdci      = G08dat
022000050905     c                   eval      W1Cdci      = G08inv
022100050811e   1c                   endif
022200050905if  1c                   if        W1Cdci      < C_DataLimite
022300050901     c                   seton                                        512890
022400050811     c                   eval      V1Dmsg      = $Msg(3)
022500050811     c                   leavesr
022600050811e   1c                   endif
022700050905      *
022800050905      * Data chiusura C.A. finale
022900050905if  1c                   if        V1Cdcf      = *zeros
023000050905     c                   eval      W1Cdcf      = W1Cdci
023100050905x   1c                   else
023200050905     c                   clear                   WLBdat
023300050905     c                   eval      G08dat      = V1Cdcf
023400050905     c                   call      'XSRDA8'
023500050905     c                   parm                    WLBdat
023600050905if  2c                   if        G08err      = *on
023700050905     c                   seton                                        522890
023800050905     c                   eval      V1Dmsg      = $Msg(2)
023900050905     c                   leavesr
024000050905e   2c                   endif
024100050905     c                   eval      V1Cdcf      = G08dat
024200050905     c                   eval      W1Cdcf      = G08inv
024300050905e   1c                   endif
024400050905if  1c                   if        W1Cdcf      < C_DataLimite
024500050905     c                   seton                                        522890
024600050905     c                   eval      V1Dmsg      = $Msg(3)
024700050905     c                   leavesr
024800050905e   1c                   endif
024900050905      *
025000050905      * Range di date chiusura C.A.
025100050905if  1c                   if        W1Cdci      > W1Cdcf
025200050905     c                   seton                                        512890
025300051011     c                   eval      V1Dmsg      = $Msg(4)
025400050905     c                   leavesr
025500050905e   1c                   endif
025600050811      *
025700050811      * Selezioni
025800050831sel 1c                   select
025900050831w   1c                   when          V1Cse1  = *blanks
026000051011     c                             and V1Cse2  = *blanks
026100051011     c                             and V1Cse3  = *blanks
026200050905     c                   seton                                        532890
026300051011     c                   eval      V1Dmsg      = $Msg(5)
026400050811     c                   leavesr
026500051011w   1c                   when          (V1Cse1 = 'S'
026600051011     c                              or  V1Cse2 = 'S')
026700051011     c                             and  V1Cse3 = 'S'
026800051011     c                   seton                                        552890
026900051011     c                   eval      V1Dmsg      = $Msg(6)
027000050831     c                   leavesr
027100050831e   1c                   endsl
027200050811      *
027300050811     c                   ENDSR
027400050811**   $MSG -------------------------------------------------------------------*
027500050811Immettere una data chiusura C.A.                                               1
027600050811Data chiusura C.A. formalmente errata                                          2
027700050811Data chiusura C.A. antecedente il giorno 18/04/2005                            3
027800051011Data chiusura C.A. iniziale   >   Data chiusura C.A. finale                    4
027900051011Effettuare almeno una selezione                                                5
028000051025La selezione delle C.A. recuperate è incompatibile con altre selezioni         6
