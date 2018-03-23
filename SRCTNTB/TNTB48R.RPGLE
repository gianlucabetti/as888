000100060217      *---------------------------------------------------------------*
000200060217      *?Gestione tabella "RCV" : storico importi limite risarcibili  ?*
000300060217      *---------------------------------------------------------------*
000400060217
000500060217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060217
000700060217      *---------------------------------------------------------------*
000800060217      *   A R C H I V I                                               *
000900060217      *---------------------------------------------------------------*
001000060217
001100060217     fTNTBE01L  if   e           k disk
001200060217      *
001300060217     fTNTB48D   cf   e             workstn
001400060217
001500060217      *---------------------------------------------------------------*
001600060217      *   C O S T A N T I                                             *
001700060217      *---------------------------------------------------------------*
001800060217
001900060217      * Codice tabella in esame
002000060217     d C_tab           c                   const('RCV')
002100060217
002200060217      *---------------------------------------------------------------*
002300060217      *   S C H I E R E                                               *
002400060217      *---------------------------------------------------------------*
002500060217
002600060217      * Messaggi a video
002700060217     d $Msg            s             78    dim( 1) ctdata perrcd(1)
002800060217
002900060217      *---------------------------------------------------------------*
003000060217      *   S T R U T T U R E   D A T I                                 *
003100060217      *---------------------------------------------------------------*
003200060217
003300060217      * Parametri
003400060217     d KPJBA         e ds
003500060217      *
003600060217     d TIBS34ds      e ds
003700060217     d dDatiUte      e ds
003800060217     d AZUTEds       e ds                  extname(AZUTE00F)
003900060217      *
004000060217      * Tabella RCV = Storico importi limite risarcibili
004100060217     d dRCV          e ds                  inz
004200060217      *
004300060217      * Tracciato record file TNTBE00F
004400060217     d TNTBEds       e ds                  inz  extname(TNTBE00F)
004500060217     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
004600060217     d                                          prefix(TBX:3)
004700060217      *
004800060217      * Passaggio Parametri al pgm TIBS02R
004900060217     d TIBS02ds      e ds                  inz
005000060217     d  T02mod       e                     inz('R')
005100060217     d  T02cod       e                     inz('GEI')
005200060217      *
005300060217     d Status         sds
005400060217     d  SDSpgm           *proc
005500060217      *
005600060217     d WLBdat          ds                  inz
005700060217     d  G08dat                 1      8  0 inz
005800060217     d  G08inv                 9     16  0 inz
005900060217     d  G08err                17     17    inz('3')
006000060217     d  G08tgi                18     22  0 inz
006100060217
006200060217      *---------------------------------------------------------------*
006300060217      *   V A R I A B I L I                                           *
006400060217      *---------------------------------------------------------------*
006500060217
006600060217     d $Called         s              1a   inz(*off)
006700060217     d $Fine           s              1a   inz(*off)
006800060217     d $Found          s              1a   inz(*off)
006900060217     d $InzD01         s              1a   inz(*on)
007000060217     d $InzD02         s              1a   inz(*on)
007100060217     d $Tibs02         s              1a   inz(*off)
007200060217      *
007300060217     d $Video          s              1a   inz('1')
007400060217
007500060217      *---------------------------------------------------------------*
007600060217      *  Riepilogo indicatori utilizzati:                             *
007700060217      *  --------------------------------                             *
007800060217      *  01 - Record inesistente (inserimento)                        *
007900060217      *  02 - Record esistente   (modifica)                           *
008000060217      *  04 - Record annullato   (ripristino)                         *
008100060217      *  22 - Errori in scrittura record (WRITE)                      *
008200060217      *  28 - Visualizzazione messaggio di errore                     *
008300060217      *  90 - Errore generico                                         *
008400060217      *---------------------------------------------------------------*
008500060217
008600060217      *---------------------------------------------------------------*
008700060217      *   M A I N   L I N E                                           *
008800060217      *---------------------------------------------------------------*
008900060217
009000060217      * Ricezione parametri
009100060217     c     *entry        plist
009200060217     c                   parm                    KPJBA
009300060217      *
009400060217      * Operazioni iniziali
009500060217     c                   exsr      RoutInz
009600060217      *
009700060217      * Gestione video
009800060217     c                   dow       $Fine   = *off
009900060217     c     $Video        caseq     '1'           GesD01
010000060217     c     $Video        caseq     '2'           GesD02
010100060217     c                   endcs
010200060217     c                   enddo
010300060217      *
010400060217      * Fine
010500060217     c                   if        $Tibs02 = *on
010600060217     c                   clear                   TIBS02ds
010700060217     c                   movel     'C'           T02tla
010800060217     c                   call      'TIBS02R'
010900060217     c                   parm                    KPJBA
011000060217     c                   parm                    TIBS02ds
011100060217     c                   endif
011200060217     c                   movel     *on           *inLR
011300060217      *
011400060217      *---------------------------------------------------------------*
011500060217      * Operazioni Iniziali                                           *
011600060217      *---------------------------------------------------------------*
011700060217     c     RoutInz       BEGSR
011800060217      *
011900060217      * Definizioni chiavi di accesso
012000060217      * - TNTBE01L
012100060217     c     K05TBE01      klist
012200060217     c                   kfld                    TBEcod
012300060217     c                   kfld                    TBEke1
012400060217     c                   kfld                    TBEke2
012500060217     c                   kfld                    TBElin
012600060217     c                   kfld                    TBEsif
012700060217     c     K02TBE01      klist
012800060217     c                   kfld                    TBEcod
012900060217     c                   kfld                    TBEke1
013000060217      *
013100060217      * Reperisco dati job
013200060217     c                   exsr      DatiJob
013300060217      *
013400060217      * Aggancio dati generali della tabella in esame
013500060217     c                   clear                   TBEcod
013600060217     c                   movel     *zeros        TBEke1
013700060217     c                   move      C_tab         TBEke1
013800060217     c                   clear                   TBEke2
013900060217     c                   clear                   TBElin
014000060217     c                   movel     KNSIF         TBEsif
014100060217     c     K05TBE01      chain     TNTBE000
014200060217     c                   if        not %found(TNTBE01L)
014300060217     c                   clear                   TBEsif
014400060217     c     K05TBE01      chain     TNTBE000
014500060217     c                   endif
014600060217     c                   if        %found(TNTBE01L)
014700060217     c                   movel     TNTBEds       xTNTBEds
014800060217     c                   else
014900060217     c                   clear                   xTNTBEds
015000060217     c                   endif
015100060217      *
015200060217      * Verifico se ho ricevuto parametri
015300060217if  1c                   if        %subst(KPJBU:1:8) = *blanks
015400060217     c                   leavesr
015500060217e   1c                   endif
015600060217      *
015700060217      * Controllo i parametri ricevuti
015800060217     c                   eval      $Called = *on
015900060217     c                   clear                   TN48D01
016000060217     c                   eval      V1Cdiv  = %subst(KPJBU:1:3)
016100060217     c                   exsr      CtrD01
016200060217if  1c                   if        *in28
016300060217     c                   movel(p)  V1Dmsg        KPJBU
016400060217     c                   eval      $Fine   = *on
016500060217     c                   leavesr
016600060217x   1c                   else
016700060217     c                   eval      $Video  = '2'
016800060217e   1c                   endif
016900060217      *
017000060217     c                   ENDSR
017100060217      *
017200060217      *---------------------------------------------------------------*
017300060217      * Reperimento Dati del job (Utente/Operativi)                   *
017400060217      *---------------------------------------------------------------*
017500060217     c     DatiJob       BEGSR
017600060217      *
017700060217      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
017800060217     c     *dtaara       define    §azute        azuteds
017900060217     c     *dtaara       define    §datiute      ddatiute
018000060217      *
018100060217     c                   clear                   AzUteDs
018200060217     c                   clear                   DDatiUte
018300060217     c                   clear                   Tibs34Ds
018400060217     c                   in(E)     *dtaara
018500060217if  1c                   if        %Error  or  RSUT = *blanks
018600060217     c                   call      'TIBS34R'
018700060217     c                   parm                    Tibs34Ds
018800060217     c                   in        *dtaara
018900060217e   1c                   endif
019000060217      *-- Verifica errori e autorità profilo
019100060217sel 1c                   SELECT
019200060217      *-- controllo se ho errori nei dati utente
019300060217      *--   nel qual caso NON risulta un profilo abilitato
019400060217w   1c                   WHEN      DUTerr  = 'E'
019500060217     c                   eval      $Fine   = *on
019600060217      *
019700060217      *-- CONTROLLO AUTORITA'
019800060217      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
019900060217      *-- se il chiamante non richiede autorità specifica verificare
020000060217      *--   quella del profilo
020100060217      *-- se il chiamante richiede autorità specifica verificarla,
020200060217      *--  se è blank verificare quella del profilo
020300060217      *
020400060217      * se UTEAUT = *BLANK non siamo in sede
020500060217w   1c                   WHEN      UTEaut  = *blank
020600060217      *
020700060217x   1c                   OTHER
020800060217      *
020900060217e   1c                   ENDSL
021000060217      *
021100060217     c                   ENDSR
021200060217      *
021300060217      *---------------------------------------------------------------*
021400060217      * Gestione videata D01                                          *
021500060217      *---------------------------------------------------------------*
021600060217     c     GesD01        BEGSR
021700060217      *
021800060217      * Inizializzo la videata
021900060217if  1c                   if        $InzD01 = *on
022000060217     c                   exsr      InzD01
022100060217     c                   movel     *off          $InzD01
022200060217e   1c                   endif
022300060217      *
022400060217      * Emetto la window
022500060217     c                   exfmt     TN48D01
022600060217      *
022700060217     c                   setoff                                       28  90
022800060217     c                   movea     *zeros        *in(50)
022900060217     c                   clear                   V1Dmsg
023000060217      *
023100060217sel 1c                   SELECT
023200060217      *
023300060217      * F3=Fine
023400060217w   1c                   WHEN      *inKC
023500060217     c                   exsr      F03D01
023600060217      *
023700060217      * Controllo dati immessi a video
023800060217x   1c                   OTHER
023900060217     c                   exsr      CtrD01
024000060217if  2c                   if        *in90
024100060217     c                   leavesr
024200060217e   2c                   endif
024300060217      * - Passaggio alla videata successiva
024400060217     c                   eval      $Video  = '2'
024500060217     c                   eval      $InzD02 = *on
024600060217      *
024700060217e   1c                   ENDSL
024800060217      *
024900060217     c                   ENDSR
025000060217      *
025100060217      *---------------------------------------------------------------*
025200060217      * Inizializzazione videata D01                                  *
025300060217      *---------------------------------------------------------------*
025400060217     c     InzD01        BEGSR
025500060217      *
025600060217     c                   clear                   TN48D01
025700060217      *
025800060217      * Testata                              *...+....1....+....2....+
025900060217     c                   eval      V1Dtxt = 'Tab. "RCV":              '
026000060217     c***                                 + '           '
026100060217     c                   move      SDSpgm        V1Dtxt
026200060217      *
026300060217      * Imposto i dati di default
026400060217     c                   move      '?'           V1Cdiv
026500060217      *
026600060217     c                   ENDSR
026700060217      *
026800060217      *---------------------------------------------------------------*
026900060217      * Gestione tasto funzionale F03 da videata D01                  *
027000060217      *---------------------------------------------------------------*
027100060217     c     F03D01        BEGSR
027200060217      *
027300060217      * Chiudo il programma
027400060217     c                   eval      $Fine   = *on
027500060217     c                   clear                   KPJBU
027600060217      *
027700060217     c                   ENDSR
027800060217      *
027900060217      *---------------------------------------------------------------*
028000060217      * Controllo dati immessi in videata D01                         *
028100060217      *---------------------------------------------------------------*
028200060217     c     CtrD01        BEGSR
028300060217      *
028400060217     c                   movea     *zeros        *in(50)
028500060217      *
028600060217      * Interrogazione
028700060217     c     '?'           scan      V1Cdiv
028800060217if  1c                   if        %found
028900060217     c                   eval      *in90   = *on
029000060217     c                   clear                   TIBS02ds
029100060217     c                   movel     'R'           T02mod
029200060217     c                   movel     KNSIF         T02sif
029300060217     c                   movel     'GEI'         T02cod
029400060217     c                   call      'TIBS02R'
029500060217     c                   parm                    KPJBA
029600060217     c                   parm                    TIBS02ds
029700060217if  2c                   if        T02err <> *blanks
029800060217     c                   eval      *in28   = *on
029900060217     c                   movel     T02msg        V1Dmsg
030000060217     c                   leavesr
030100060217e   2c                   endif
030200060217     c                   movel     T02ke1        V1Cdiv
030300060217     c                   leavesr
030400060217e   1c                   endif
030500060217      *
030600060217      * Controllo Divisa in tab. "GEI"
030700060217     c                   movel     'GEI'         TBEcod
030800060217     c                   movel(p)  V1Cdiv        TBEke1
030900060217     c                   clear                   TBEke2
031000060217     c                   movel     TBXlin        TBElin
031100060217     c                   movel     TBXsif        TBEsif
031200060217     c     K05TBE01      chain     TNTBE000
031300060217      *
031400060217     c                   if        NOT %found(TNTBE01L)
031500060217     c                   movel     $Msg(1)       V1Dmsg
031600060217     c                   seton                                        28  90
031700060217     c                   leavesr
031800060217e   1c                   endif
031900060217      *
032000060217     c                   ENDSR
032100060217      *
032200060217      *---------------------------------------------------------------*
032300060217      * Gestione videata D02                                          *
032400060217      *---------------------------------------------------------------*
032500060217     c     GesD02        BEGSR
032600060217      *
032700060217      * Inizializzo la videata
032800060217if  1c                   if        $InzD02 = *on
032900060217     c                   exsr      InzD02
033000060217     c                   movel     *off          $InzD02
033100060217e   1c                   endif
033200060217      *
033300060217      * Emetto la window
033400060217     c                   exfmt     TN48D02
033500060217      *
033600060217     c                   setoff                                       28  90
033700060217     c                   movea     *zeros        *in(50)
033800060217     c                   clear                   V1Dmsg
033900060217      *
034000060217     c                   SELECT
034100060217      *
034200060217      * F3=Fine
034300060217w   1c                   WHEN      *inKC
034400060217     c                   exsr      F03D01
034500060217      *
034600060217      * F12=Ritorno
034700060217w   1c                   WHEN      *inKL
034800060217     c                   exsr      F12D02
034900060217      *
035000060217e   1c                   ENDSL
035100060217      *
035200060217     c                   ENDSR
035300060217      *
035400060217      *---------------------------------------------------------------*
035500060217      * Inizializzazione videata D02                                  *
035600060217      *---------------------------------------------------------------*
035700060217     c     InzD02        BEGSR
035800060217      *
035900060217     c                   clear                   TN48D02
036000060217      *
036100060217      * Testata                              *...+....1....+....2....+
036200060217     c                   eval      V2Dtxt = 'Tab. "RCV" - divisa '
036300060217     c                                    + V1Cdiv
036400060217     c                                    + ' - '
036500060217     c                                    + SDSpgm
036600060217      *
036700060217      * Reperimento record da TNTBE01L
036800060217     c                   clear                   dRCV
036900060217     c                   exsr      Rep_TNTBE
037000060217     c                   if        $Found  = *on
037100060217     c                   movel     TBEuni        dRCV
037200060217     c                   endif
037300060217      *
037400060217      * - dati relativi alla 1ª legge
037500060217if  1c                   if            §RCVsl1 <> *blanks
037600060217     c                             or  §RCVsi1 <> *zeros
037700060217     c                             or  §RCVss1 <> *zeros
037800060217     c                   eval      V2Csl1  = §RCVsl1
037900060217     c                   eval      V2Csi1  = §RCVsi1
038000060217     c                   reset                   WLBdat
038100060217     c                   eval      G08inv  = §RCVss1
038200060217     c                   call      'XSRDA8'
038300060217     c                   parm                    WLBdat
038400060217if  2c                   if        G08err  = *off
038500060217     c                   eval      V2Css1  = G08dat
038600060217e   2c                   endif
038700060217e   1c                   endif
038800060217      * - dati relativi alla 2ª legge
038900060217if  1c                   if            §RCVsl2 <> *blanks
039000060217     c                             or  §RCVsi2 <> *zeros
039100060217     c                             or  §RCVss2 <> *zeros
039200060217     c                   eval      V2Csl2  = §RCVsl2
039300060217     c                   eval      V2Csi2  = §RCVsi2
039400060217     c                   reset                   WLBdat
039500060217     c                   eval      G08inv  = §RCVss2
039600060217     c                   call      'XSRDA8'
039700060217     c                   parm                    WLBdat
039800060217if  2c                   if        G08err  = *off
039900060217     c                   eval      V2Css2  = G08dat
040000060217e   2c                   endif
040100060217e   1c                   endif
040200060217      * - dati relativi alla 3ª legge
040300060217if  1c                   if            §RCVsl3 <> *blanks
040400060217     c                             or  §RCVsi3 <> *zeros
040500060217     c                             or  §RCVss3 <> *zeros
040600060217     c                   eval      V2Csl3  = §RCVsl3
040700060217     c                   eval      V2Csi3  = §RCVsi3
040800060217     c                   reset                   WLBdat
040900060217     c                   eval      G08inv  = §RCVss3
041000060217     c                   call      'XSRDA8'
041100060217     c                   parm                    WLBdat
041200060217if  2c                   if        G08err  = *off
041300060217     c                   eval      V2Css3  = G08dat
041400060217e   2c                   endif
041500060217e   1c                   endif
041600060217      * - dati relativi alla 4ª legge
041700060217if  1c                   if            §RCVsl4 <> *blanks
041800060217     c                             or  §RCVsi4 <> *zeros
041900060217     c                             or  §RCVss4 <> *zeros
042000060217     c                   eval      V2Csl4  = §RCVsl4
042100060217     c                   eval      V2Csi4  = §RCVsi4
042200060217     c                   reset                   WLBdat
042300060217     c                   eval      G08inv  = §RCVss4
042400060217     c                   call      'XSRDA8'
042500060217     c                   parm                    WLBdat
042600060217if  2c                   if        G08err  = *off
042700060217     c                   eval      V2Css4  = G08dat
042800060217e   2c                   endif
042900060217e   1c                   endif
043000060217      * - dati relativi alla 5ª legge
043100060217if  1c                   if            §RCVsl5 <> *blanks
043200060217     c                             or  §RCVsi5 <> *zeros
043300060217     c                             or  §RCVss5 <> *zeros
043400060217     c                   eval      V2Csl5  = §RCVsl5
043500060217     c                   eval      V2Csi5  = §RCVsi5
043600060217     c                   reset                   WLBdat
043700060217     c                   eval      G08inv  = §RCVss5
043800060217     c                   call      'XSRDA8'
043900060217     c                   parm                    WLBdat
044000060217if  2c                   if        G08err  = *off
044100060217     c                   eval      V2Css5  = G08dat
044200060217e   2c                   endif
044300060217e   1c                   endif
044400060217      *
044500060217     c                   ENDSR
044600060217      *
044700060217      *---------------------------------------------------------------*
044800060217      * Gestione tasto funzionale F12 da videata D02                  *
044900060217      *---------------------------------------------------------------*
045000060217     c     F12D02        BEGSR
045100060217      *
045200060217     c                   if        $Called = *off
045300060217      * - Ritorno alla videata precedente
045400060217     c                   eval      $Video  = '1'
045500060217     c                   else
045600060217      * - Uscita dal pgm
045700060217     c                   exsr      F03D01
045800060217     c                   endif
045900060217      *
046000060217     c                   ENDSR
046100060217      *
046200060217      *---------------------------------------------------------------*
046300060217      * Reperimento record da TNTBE01L                                *
046400060217      *---------------------------------------------------------------*
046500060217     c     Rep_TNTBE     BEGSR
046600060217      *
046700060217     c                   reset                   $Found
046800060217      *
046900060217     c                   movel     C_Tab         TBEcod
047000060217     c                   movel(p)  V1Cdiv        TBEke1
047100060217     c                   clear                   TBEke2
047200060217     c                   movel     TBXlin        TBElin
047300060217     c                   movel     TBXsif        TBEsif
047400060217     c     K05TBE01      chain     TNTBE000
047500060217      *
047600060217      * Record non trovato per il codice KE1
047700060217     c                   if        NOT %found(TNTBE01L)
047800060217     c                   leavesr
047900060217     c                   endif
048000060217      *
048100060217     c                   eval      $Found  = *on
048200060217      *
048300060217     c                   ENDSR
048400060217
048500060217      *---------------------------------------------------------------*
048600060217      *   S C H I E R E   A   T E M P O   D I   C O M P I L A Z .     *
048700060217      *---------------------------------------------------------------*
048800060217
048900060217** - $MSG -------------------------------------------------------------------*
049000060217Divisa inesistente in tab. "GEI"                                                1
