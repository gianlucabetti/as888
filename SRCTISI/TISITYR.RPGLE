000100060801      *===============================================================*
000200060801      *?TISITYR * Pulizia  Destinatari Particolari  &  Sinonimi      ?*
000300060801      *===============================================================*
000400060801
000500060801     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060801
000700060801      *---------------------------------------------------------------*
000800060801
000900060801     fTISITYD   cf   e             workstn
001000060801
001100060801      *---------------------------------------------------------------*
001200060801
001300060801      *
001400060801      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001500060801      *
001600060913     d $Msg            s             78    dim( 7)  ctdata  perrcd(1)
001700060801      *
001800060801      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
001900060801      *
002000060801     d KPJBA         e ds
002100060801      *
002200060801      * - Parametri x Controllo profilo utenti
002300060801     d TIBS34ds      e ds
002400060801      * - Ds di riferimento al file esterno AZUTE00F
002500060801     d AZUTEds       e ds                  extname(AZUTE00F)
002600060801     d dUTE01        e ds
002700060801      * - Ds per dati organigramma
002800060801     d DDatiUte      e ds
002900060802      *
003000060802      * - Parametri x interrogazione tabelle
003100060802     d TIBS02ds      e ds                  inz
003200060802     d   T02mod      e                     inz('C')
003300060802     d   T02cod      e                     inz('TDP')
003400060802      * - Tab. "TDP" = Tipi Destinatari Particolari
003500060802     d dTDP          e ds                  inz
003600060801      *
003700060801     d Status         sds           333
003800060801     d   SDSpgm          *proc
003900060801      *
004000060801      * - Parametri
004100060802     d Param           ds                  inz
004200060802     d   V1Cdos1                           inz
004300060802     d   V1Cdos2                           inz
004400060802     d   V1Cdos3                           inz
004500060802     d   V1Cdos4                           inz
004600060802     d   V1Cdos5                           inz
004700060802     d  $V1Cdos                1      5    dim( 5)
004800060803     d  V1Cnum                             inz
004900060913     d  PRMdiv                        8  0 inz
005000060802      *
005100060802      * - di comodo: Decodifiche "Tipi destinatari particolari"
005200060802     d                 ds
005300060802     d   V1Ddos1                           inz
005400060802     d   V1Ddos2                           inz
005500060802     d   V1Ddos3                           inz
005600060802     d   V1Ddos4                           inz
005700060802     d   V1Ddos5                           inz
005800060802     d  $V1Ddos                1    100    dim( 5)
005900060913      *
006000060913      * Parametri per controllo/inversione data (8)
006100060913     d WLBda8          ds                  inz
006200060913     d  G08dat                 1      8  0 inz
006300060913     d  G08inv                 9     16  0 inz
006400060913     d  G08err                17     17    inz
006500060913     D  G08tgi                18     22  0 inz
006600060801      *
006700060801      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006800060801      *
006900060801      * - Flags
007000060801     d $Fine           s              1    inz(*off)
007100060801     d $InzD01         s              1    inz(*on)
007200060802      *
007300060802      * - Indici di schiera
007400060802     d xx              s              3  0 inz
007500060802     d yy              s              3  0 inz
007600060801
007700060801      *---------------------------------------------------------------*
007800060802      * I N D I C A T O R I                                           *
007900060802      *---------------------------------------------------------------*
008000060802      *  10     - Comodo                                              *
008100060802      *  51     - 1° Tipo destinatario errato                         *
008200060802      *  52     - 2° Tipo destinatario errato                         *
008300060802      *  53     - 3° Tipo destinatario errato                         *
008400060802      *  54     - 4° Tipo destinatario errato                         *
008500060802      *  55     - 5° Tipo destinatario errato                         *
008600060802      *  61     - Ricorrenza limite per sinonimi    errata            *
008700060802      *  62     - Ricorrenza limite per destinatari errata            *
008800060802      *---------------------------------------------------------------*
008900060801
009000060801     c     *Entry        plist
009100060801     c                   parm                    KPJBA
009200060801      *
009300060801      * Operazioni Iniziali
009400060801     c                   exsr      RoutInz
009500060801      *
009600060801      * Gestione Video
009700060801do  1c                   dow       $Fine   = *off
009800060802     c                   exsr      GesD01
009900060801e   1c                   enddo
010000060801      *
010100060801      * Fine
010200060801     c                   movel     *on           *inLR
010300060801      *
010400060801      *---------------------------------------------------------------*
010500060801      * Operazioni Iniziali                                           *
010600060801      *---------------------------------------------------------------*
010700060801     c     RoutInz       BEGSR
010800060801      *
010900060801      * Reperisco dati job
011000060801     c                   exsr      DatiJob
011100060801      *
011200060801     c                   movel     SDSpgm        V1Tpgm
011300060801      *
011400060801     c                   ENDSR
011500060801      *
011600060801      *---------------------------------------------------------------*
011700060801      * Reperimento Dati del job (Utente/Operativi)                   *
011800060801      *---------------------------------------------------------------*
011900060801     c     DatiJob       BEGSR
012000060801      *
012100060801     c     *dtaara       define    §azute        azuteds
012200060801     c     *dtaara       define    §datiute      ddatiute
012300060801      *
012400060801     c                   in(E)     *dtaara
012500060801     c                   IF        %ERROR or RSUT = *blanks
012600060801     c                   clear                   Tibs34Ds
012700060801     c                   call      'TIBS34R'
012800060801     c                   parm                    Tibs34Ds
012900060801     c                   in        *dtaara
013000060801     c                   ENDIF
013100060801      *
013200060801     c                   movel     UTEfaf        dUTE01
013300060801      *
013400060801     c                   ENDSR
013500060801      *
013600060801      *---------------------------------------------------------------*
013700060801      * Inizializzazione videata D01                                  *
013800060801      *---------------------------------------------------------------*
013900060801     c     InzD01        BEGSR
014000060801      *
014100060801     c                   clear                   SITYD01
014200060801      *
014300060802     c                   eval      V1Cdos1 = 'D'
014400060802     c                   eval      V1Cdos2 = 'S'
014500060802     c***                eval      V1Cdos3 = 'V'
014600060802     c***                eval      V1Cdos4 = 'A'
014700060802     c***                clear                   V1Cdos5
014800060801      *
014900060803     c***                clear                   V1Cnum
015000060913     c***                clear                   V1Cdiv
015100060803      *
015200060803      * Decodifica "tipi destinatario"
015300060803     c                   clear                   xx
015400060803do  1c                   dow       xx      < %elem($V1Cdos)
015500060803     c                   eval      xx      = xx + 1
015600060803if  2c                   if        $V1Cdos(xx) <> *blanks
015700060803     c                   exsr      sr_CtrDOS
015800060803e   2c                   endif
015900060803e   1c                   enddo
016000060801      *
016100060801     c                   ENDSR
016200060801      *
016300060801      *---------------------------------------------------------------*
016400060801      * Gestione videata D01                                          *
016500060801      *---------------------------------------------------------------*
016600060801     c     GesD01        BEGSR
016700060801      *
016800060801      * Inizializzazione videata
016900060801if  1c                   if        $InzD01 = *on
017000060801     c                   exsr      InzD01
017100060801     c                   movel     *off          $InzD01
017200060801e   1c                   endif
017300060801      *
017400060801      * Emissione Testata e Riga con tasti funzionali abilitati
017500060913     c                   if        NOT *in28
017600060801     c                   write     SITYT01
017700060801     c                   write     SITYP01
017800060913     c                   endif
017900060801      * Emissione videata
018000060801     c                   exfmt     SITYD01
018100060801     c                   setoff                                       28  90
018200060801     c                   clear                   V1Dmsg
018300060801      *
018400060801sel 1c                   SELECT
018500060801      * F3=Fine
018600060801w   1c                   WHEN      *inKC
018700060801     c                   exsr      F03D01
018800060801      *
018900060801x   1c                   OTHER
019000060801     c                   exsr      CtrD01
019100060801if  2c                   if        *in90
019200060801     c                   leavesr
019300060801e   2c                   endif
019400060801      * F6=Conferma Proposta
019500060801if  2c                   if        *inKF
019600060801     c                   exsr      sr_SbmJob
019700060801if  3c                   if        *in90   = *off
019800060801     c                   eval      $Fine   = *on
019900060801e   3c                   endif
020000060801e   2c                   endif
020100060801      *
020200060801e   1c                   ENDSL
020300060801      *
020400060801     c                   ENDSR
020500060801      *
020600060801      *---------------------------------------------------------------*
020700060801      * Gestione tasto funzionale F03 da videata D01                  *
020800060801      *---------------------------------------------------------------*
020900060801     c     F03D01        BEGSR
021000060801      *
021100060801      * Chiudo il programma
021200060801     c                   eval      $Fine   = *on
021300060801      *
021400060801     c                   ENDSR
021500060801      *
021600060801      *---------------------------------------------------------------*
021700060801      * Controllo dati immessi in videata D01                         *
021800060801      *---------------------------------------------------------------*
021900060801     c     CtrD01        BEGSR
022000060801      *
022100060801     c                   movea     *zeros        *in(51)
022200060802      *
022300060802      * Selezionare almeno un "tipo destinatario"
022400060802     c                   if             V1Cdos1  = *blanks
022500060802     c                             and  V1Cdos2  = *blanks
022600060802     c                             and  V1Cdos3  = *blanks
022700060802     c                             and  V1Cdos4  = *blanks
022800060802     c                             and  V1Cdos5  = *blanks
022900060802     c                   seton                                        285190
023000060802     c                   eval      V1Dmsg  = $Msg(1)
023100060802     c                   leavesr
023200060802     c                   endif
023300060802      *
023400060802      * "Tipo destinatario" già impostato
023500060802     c                   eval      xx      = 1
023600060802     c                   clear                   yy
023700060802do  1c                   dow       xx      < %elem($V1Cdos)
023800060802     c                   eval      yy      = xx + 1
023900060802if  2c                   if        $V1Cdos(xx) = *blanks
024000060802     c                   clear                   $V1Ddos(xx)
024100060802x   2c                   else
024200060802     c     $V1Cdos(xx)   lookup    $V1Cdos(yy)                            10
024300060802if  3c                   if        *in10
024400060802     c                   seton                                        28  90
024500060802     c                   eval      *in( 50 + yy ) = *on
024600060802     c                   eval      V1Dmsg  = $Msg(2)
024700060802     c                   leavesr
024800060802e   3c                   endif
024900060802e   2c                   endif
025000060802     c                   eval      xx      = xx + 1
025100060802e   1c                   enddo
025200060802      *
025300060802      * "Tipo destinatario" errato
025400060802     c                   clear                   xx
025500060802do  1c                   dow       xx      < %elem($V1Cdos)
025600060802     c                   eval      xx      = xx + 1
025700060802if  2c                   if        $V1Cdos(xx) = *blanks
025800060802     c                   clear                   $V1Ddos(xx)
025900060802x   2c                   else
026000060802     c                   exsr      sr_CtrDOS
026100060913if  3c                   if        *in90
026200060913     c                   leavesr
026300060913e   3c                   endif
026400060802e   2c                   endif
026500060802e   1c                   enddo
026600060913      *
026700060913      * Ricorrenza limite errata
026800060913sel 1c                   select
026900060913w   1c                   when      V1Cnum  = *zeros
027000060913     c                   seton                                        286190
027100060913     c                   eval      V1Dmsg  = $Msg(4)
027200060913     c                   leavesr
027300060913w   1c                   when      V1Cnum  < *zeros
027400060913     c                   seton                                        286190
027500060913     c                   eval      V1Dmsg  = $Msg(5)
027600060913     c                   leavesr
027700060913e   1c                   endsl
027800060913      *
027900060913      * Data inserimento/variazione errata
028000060913if  1c                   if        V1Cdiv  = *zeros
028100060913     c                   seton                                        286290
028200060913     c                   eval      V1Dmsg  = $Msg(6)
028300060913     c                   leavesr
028400060913x   1c                   else
028500060913     c                   clear                   WLBda8
028600060913     c                   eval      G08dat  = V1Cdiv
028700060913     c                   call      'XSRDA8'
028800060913     c                   parm                    WLBda8
028900060913if  2c                   if        G08err  = *on
029000060913     c                   seton                                        286290
029100060913     c                   eval      V1Dmsg  = $Msg(7)
029200060913     c                   leavesr
029300060913e   2c                   endif
029400060913     c                   eval      V1Cdiv  = G08dat
029500060913     c                   eval      PRMdiv  = G08inv
029600060913e   1c                   endif
029700060801      *
029800060801     c                   ENDSR
029900060802      *
030000060802      *---------------------------------------------------------------*
030100060802      * Controllo/Decodifica Tipo Destinatario                        *
030200060802      *---------------------------------------------------------------*
030300060802     c     sr_CtrDOS     BEGSR
030400060802      *
030500060802     c*** meglio di no:  clear                   $V1Ddos(xx)
030600060802     c     '?'           scan      $V1Cdos(xx)
030700060802      *
030800060802      * - Ricerca
030900060802if  1c                   IF        %found
031000060802      *
031100060802     c                   clear                   $V1Cdos(xx)
031200060802     c                   reset                   TIBS02ds
031300060802     c                   eval      T02mod  = 'R'
031400060802     c                   call      'TIBS02R'
031500060802     c                   parm                    KPJBA
031600060802     c                   parm                    TIBS02ds
031700060802if  2c                   if             T02err  = *blanks
031800060802     c                             and  T02ke1 <> *blanks
031900060802     c                   movel     T02ke1        $V1Cdos(xx)
032000060802     c                   movel     T02uni        dTDP
032100060802     c                   movel     §TDPdes       $V1Ddos(xx)
032200060802e   2c                   endif
032300060802     c                   eval      *in90   = *on
032400060802     c                   eval      *in( 50 + xx ) = *on
032500060802     c*** inutile qui:   leavesr
032600060802      *
032700060802      * - Controllo
032800060802x   1c                   ELSE
032900060802      *
033000060802     c                   clear                   dTDP
033100060802     c                   reset                   TIBS02ds
033200060802     c                   movel(p)  $V1Cdos(xx)   T02ke1
033300060802     c                   call      'TIBS02R'
033400060802     c                   parm                    KPJBA
033500060802     c                   parm                    TIBS02ds
033600060802if  2c                   if        T02err  = *blanks
033700060802     c                   movel     T02uni        dTDP
033800060802     c                   movel     §TDPdes       $V1Ddos(xx)
033900060802x   2c                   else
034000060802     c                   seton                                        28  90
034100060802     c                   eval      *in( 50 + xx ) = *on
034200060802     c                   movel     $Msg(3)       V1Dmsg
034300060802     c*** inutile qui:   leavesr
034400060802e   2c                   endif
034500060802      *
034600060802e   1c                   ENDIF
034700060802      *
034800060802     c                   ENDSR
034900060801      *
035000060801      *---------------------------------------------------------------*
035100060801      * Sottomissione lavoro in batch                                 *
035200060801      *---------------------------------------------------------------*
035300060801     c     sr_SbmJob     BEGSR
035400060801      *
035500060801     c                   movel(p)  Param         KPJBU
035600060801      *
035700060801if  1c                   if        KNMUS   = *all'1'
035800060803     c                   call      'TISITYR1'
035900060801     c                   parm                    KPJBA
036000060801x   1c                   else
036100060803     c                   eval      KCOAZ   = 'SIT¥'
036200060801     c                   call      'BCH10'
036300060801     c                   parm                    KPJBA
036400060801e   1c                   endif
036500060801      *
036600060801     c                   ENDSR
036700060801
036800060801      *---------------------------------------------------------------*
036900060801
037000060803** - $MSG -------------------------------------------------------------------*
037100060803Selezionare ALMENO un tipo destinatario                                        1
037200060803Tipo destinatario già selezionato                                              2
037300060803Tipo destinatario errato                                                       3
037400060913Ricorrenza obbligatoria                                                        4
037500060913Ricorrenza errata ( < 0 )                                                      5
037600060913Data obbligatoria                                                              6
037700060913Data formalmente errata                                                        7
