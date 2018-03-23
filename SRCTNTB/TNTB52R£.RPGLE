000100060131      *---------------------------------------------------------------*
000200060131      * Gestione tab."VAP" = Abilitaz. ritorno fasi ORM a clienti VAP *
000300060131      *---------------------------------------------------------------*
000400060131
000500060202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060131
000700060131      *---------------------------------------------------------------*
000800060131      *   A R C H I V I                                               *
000900060131      *---------------------------------------------------------------*
001000060201
001100060201     fTIVSS02L  if   e           k disk
001200060201     fTIVIR02L  if   e           k disk
001300060201      *
001400060131     fTNTBE01L  Uf A e           k disk
001500060131      *
001600060131     fTNTB52D   cf   e             workstn
001700060131
001800060131      *---------------------------------------------------------------*
001900060131      *   C O S T A N T I                                             *
002000060131      *---------------------------------------------------------------*
002100060201
002200060201      * Costante per controllo "caratteri solo numerici"
002300060201     d DigitN          c                   const('0123456789')
002400060201      *
002500060201      * Codice tabella in esame
002600060201     d C_tab           c                   const('VAP')
002700060131
002800060131      *---------------------------------------------------------------*
002900060131      *   S C H I E R E                                               *
003000060131      *---------------------------------------------------------------*
003100060201
003200060201      * Decodifica status record
003300060201     d $Sts            s             10    dim( 4) ctdata perrcd(1)
003400060201      * Messaggi a video
003500060201     d $Msg            s             78    dim( 6) ctdata perrcd(1)
003600060131
003700060131      *---------------------------------------------------------------*
003800060131      *   S T R U T T U R E   D A T I                                 *
003900060131      *---------------------------------------------------------------*
004000060201
004100060131      * Parametri
004200060131     d KPJBA         e ds
004300060131      *
004400060131     d TIBS34ds      e ds
004500060131     d dDatiUte      e ds
004600060131     d AZUTEds       e ds                  extname(AZUTE00F)
004700060131      *
004800060131      * Passaggio Parametri al pgm TIBS02R
004900060131     d TIBS02DS      e ds                  inz
005000060131     d  T02mod       e                     inz('R')
005100060131     d  T02cod       e                     inz('VAP')
005200060131     *** *
005300060131     *** * Tabella VAP = Abilitazione ritorno fasi ORM a clienti "VAP"
005400060201     *** *   (per ora: VUOTA !)
005500060131     ***d dVAP          e ds                  inz
005600060131      *
005700060131      * Tracciato record file TNTBE00F
005800060131     d TNTBEds       e ds                  inz  extname(TNTBE00F)
005900060131     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
006000060131     d                                          prefix(TBX:3)
006100060131      *
006200060131     d TIBS69ds      e ds                  inz
006300060131     d DS_cnaco      e ds                  inz  extname(CNACO00F)
006400060131     d DS_cnind      e ds                  inz  extname(CNIND00F)
006500060131     d DS_cnclp      e ds                  inz  extname(CNCLP00F)
006600060131     d DS_fncls      e ds                  inz  extname(FNCLS00F)
006700060131      *
006800060131     d Status         sds
006900060131     d  VTCpgm           *proc
007000060201      *
007100060201     d WLBdat          ds                  inz
007200060201     d  G08dat                 1      8  0 inz
007300060201     d  G08inv                 9     16  0 inz
007400060201     d  G08err                17     17    inz('3')
007500060201     d  G08tgi                18     22  0 inz
007600060131
007700060131      *---------------------------------------------------------------*
007800060131      *   V A R I A B I L I                                           *
007900060131      *---------------------------------------------------------------*
008000060131      *
008100060201     d $Called         s              1a   inz(*off)
008200060131     d $Fine           s              1a   inz(*off)
008300060201     d $Found          s              1a   inz(*off)
008400060201     d $InzD01         s              1a   inz(*on)
008500060201     d $InzD02         s              1a   inz(*on)
008600060201     d $InzW01         s              1a   inz(*on)
008700060131     d $Tibs02         s              1a   inz(*off)
008800060201      *
008900060201     d $Video          s              1a   inz('1')
009000060201     d $Fxx            s              3a   inz
009100060201      *
009200060201     d wZ7             s              7  0 inz
009300060201     d wDate           s              8  0 inz
009400060201      *
009500060201     d Wksc            s                   like(I69kac)  inz
009600071005     d wPadre          s                   like(V1Cccf)  inz
009700091026
009800091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009900091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010000091026
010100060131
010200060131      *---------------------------------------------------------------*
010300060131      *  Riepilogo indicatori utilizzati:                             *
010400060131      *  --------------------------------                             *
010500060131      *  01 - Record inesistente (inserimento)                        *
010600060131      *  02 - Record esistente   (modifica)                           *
010700060131      *  04 - Record annullato   (ripristino)                         *
010800060131      *  22 - Errori in scrittura record (WRITE)                      *
010900060201      *  28 - Visualizzazione messaggio di errore                     *
011000060201      *  40 - Pgm richiamato da altro                                 *
011100060201      *  51 - Codice cliente fasi O.R.M. errato                       *
011200060201      *  52 - Codice cliente di invio    errato                       *
011300060201      *  90 - Errore generico                                         *
011400060131      *---------------------------------------------------------------*
011500060201
011600060201      *---------------------------------------------------------------*
011700060201      *   M A I N   L I N E                                           *
011800060201      *---------------------------------------------------------------*
011900060201
012000060201      * Ricezione parametri
012100060201     c     *entry        plist
012200060201     c                   parm                    KPJBA
012300060131      *
012400060131      * Operazioni iniziali
012500060201     c                   exsr      RoutInz
012600060131      *
012700060131      * Gestione video
012800060201     c                   dow       $Fine   = *off
012900060201     c     $Video        caseq     '1'           GesD01
013000060201     c     $Video        caseq     '2'           GesD02
013100060201     c     $Video        caseq     'A'           GesW01
013200060131     c                   endcs
013300060131     c                   enddo
013400060131      *
013500060131      * Fine
013600060131     c                   if        $Tibs02 = *on
013700060131     c                   clear                   TIBS02ds
013800060131     c                   movel     'C'           T02tla
013900060131     c                   call      'TIBS02R'
014000060131     c                   parm                    KPJBA
014100060131     c                   parm                    TIBS02ds
014200060131     c                   endif
014300060131      *
014400060131     c                   movel     *on           *inLR
014500060201      *
014600060131      *---------------------------------------------------------------*
014700060201      * Operazioni Iniziali                                           *
014800060131      *---------------------------------------------------------------*
014900060201     c     RoutInz       BEGSR
015000060131      *
015100060131      * Definizioni chiavi di accesso
015200060201      * - TNTBE01L
015300060201     c     K05TBE01      klist
015400060201     c                   kfld                    TBEcod
015500060201     c                   kfld                    TBEke1
015600060201     c                   kfld                    TBEke2
015700060201     c                   kfld                    TBElin
015800060201     c                   kfld                    TBEsif
015900060201     c     K02TBE01      klist
016000060201     c                   kfld                    TBEcod
016100060201     c                   kfld                    TBEke1
016200060201      * - TIVSS02L
016300060201     c     K02VSS02      klist
016400071005     c                   kfld                    VSSksu
016500060201     c                   kfld                    VSSisv
016600060201      * - TIVIR02L
016700060201     c     K02VIR02      klist
016800060201     c                   kfld                    V2Ccci
016900060201     c                   kfld                    VIRtip
017000060201      *
017100060201      * Impostazione campi "costanti"
017200060201     c                   eval      VSSisv  = 'TT'
017300060201      *
017400060201      * Reperisco dati job
017500060201     c                   exsr      DatiJob
017600060201      *
017700060201      * Reperimento data odierna in formato aaaa/mm/gg
017800060201     c                   clear                   WLBdat
017900060201     c                   move      *date         G08dat
018000060201     c                   call      'XSRDA8'
018100060201     c                   parm                    WLBdat
018200060201if  1c                   if        G08err  = *off
018300060201     c                   eval      wDate   = G08inv
018400060201e   1c                   endif
018500060201      *
018600060201      * Aggancio dati generali della tabella in esame
018700060201     c                   clear                   TBEcod
018800060201     c                   movel     *zeros        TBEke1
018900060201     c                   move      C_tab         TBEke1
019000060201     c                   clear                   TBEke2
019100060201     c                   clear                   TBElin
019200060201     c                   movel     KNSIF         TBEsif
019300060201     c     K05TBE01      chain     TNTBE000
019400060201     c                   if        not %found(TNTBE01L)
019500060201     c                   clear                   TBEsif
019600060201     c     K05TBE01      chain     TNTBE000
019700060201     c                   endif
019800060201     c                   if        %found(TNTBE01L)
019900060201     c                   movel     TNTBEds       xTNTBEds
020000060201     c                   else
020100060201     c                   clear                   xTNTBEds
020200060201     c                   endif
020300060201      *
020400060201      * Verifico se ho ricevuto parametri
020500060201if  1c                   if        %subst(KPJBU:1:8) = *blanks
020600060201     c                   leavesr
020700060201e   1c                   endif
020800060201      *
020900060201      * Controllo i parametri ricevuti
021000060201     c                   eval      $Called = *on
021100060201     c                   clear                   TN52D01
021200060201     c                   eval      V1Cccf  = %subst(KPJBU:1:8)
021300060201     c                   exsr      CtrD01
021400060201if  1c                   if        *in28
021500060201     c                   movel(p)  V1Dmsg        KPJBU
021600060201     c                   eval      $Fine   = *on
021700060201     c                   leavesr
021800060201x   1c                   else
021900060201     c                   eval      $Video  = '2'
022000060201e   1c                   endif
022100060201      *
022200060201     c                   ENDSR
022300060201      *
022400060201      *---------------------------------------------------------------*
022500060201      * Reperimento Dati del job (Utente/Operativi)                   *
022600060201      *---------------------------------------------------------------*
022700060201     c     DatiJob       BEGSR
022800060131      *
022900060131      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
023000060131     c     *dtaara       define    §azute        azuteds
023100060131     c     *dtaara       define    §datiute      ddatiute
023200060131      *
023300060131     c                   clear                   AzUteDs
023400060131     c                   clear                   DDatiUte
023500060131     c                   clear                   Tibs34Ds
023600060131     c                   in(E)     *dtaara
023700060131if  1c                   if        %Error  or  RSUT = *blanks
023800060131     c                   call      'TIBS34R'
023900060131     c                   parm                    Tibs34Ds
024000060131     c                   in        *dtaara
024100060131e   1c                   endif
024200060131      *-- Verifica errori e autorità profilo
024300060131sel 1c                   SELECT
024400060131      *-- controllo se ho errori nei dati utente
024500060131      *--   nel qual caso NON risulta un profilo abilitato
024600060201w   1c                   WHEN      DUTerr  = 'E'
024700060201     c                   eval      $Fine   = *on
024800060131      *
024900060131      *-- CONTROLLO AUTORITA'
025000060131      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
025100060131      *-- se il chiamante non richiede autorità specifica verificare
025200060131      *--   quella del profilo
025300060131      *-- se il chiamante richiede autorità specifica verificarla,
025400060131      *--  se è blank verificare quella del profilo
025500060131      *
025600060131      * se UTEAUT = *BLANK non siamo in sede
025700060201w   1c                   WHEN      UTEaut  = *blank
025800060131      *
025900060131x   1c                   OTHER
026000060131      *
026100060131e   1c                   ENDSL
026200060201      *
026300060201     c                   ENDSR
026400060201      *
026500060201      *---------------------------------------------------------------*
026600060201      * Gestione videata D01                                          *
026700060201      *---------------------------------------------------------------*
026800060201     c     GesD01        BEGSR
026900060201      *
027000060201      * Inizializzo la videata
027100060201if  1c                   if        $InzD01 = *on
027200060201     c                   exsr      InzD01
027300060201     c                   movel     *off          $InzD01
027400060201e   1c                   endif
027500060201      *
027600060201      * Emetto la testata
027700060202     c                   clear                   VTDope
027800060201     c                   write     TN52T01
027900060201      *
028000060201      * Emetto la videata
028100060201     c                   exfmt     TN52D01
028200060201     c                   setoff                                       28  90
028300060201     c                   movea     *zeros        *in(50)
028400060201     c                   clear                   V1Dmsg
028500060201      *
028600060201     c                   SELECT
028700060201      *
028800060201      * F3=Fine
028900060201w   1c                   WHEN      *inKC
029000060201     c                   exsr      F03D01
029100060201     c                   leavesr
029200060201      *
029300060201      * F19=Gestione dati consegna/giacenza per clienti
029400060201w   1c                   WHEN      *inKT
029500060201     c                   exsr      F19D01
029600060201     c                   leavesr
029700060201      *
029800060201      * F20=Gestione dati principali scheda clienti
029900060201w   1c                   WHEN      *inKU
030000060201     c                   exsr      F20D01
030100060201     c                   leavesr
030200060201      *
030300060201      * F21=Gestione dati bolle da clienti
030400060201w   1c                   WHEN      *inKV
030500060201     c                   exsr      F21D01
030600060201     c                   leavesr
030700060201      *
030800060201      * F22=Gestione dati c/assegno e fattura per clienti
030900060201w   1c                   WHEN      *inKW
031000060201     c                   exsr      F22D01
031100060201     c                   leavesr
031200060201      *
031300060201      * Controllo dati immessi a video
031400060201x   1c                   OTHER
031500060201     c                   exsr      CtrD01
031600060201if  2c                   if        *in90
031700060201     c                   leavesr
031800060201e   2c                   endif
031900060201      * - Passaggio alla videata successiva
032000060201     c                   eval      $InzD02 = *on
032100060201     c                   eval      $Video  = '2'
032200060201      *
032300060201e   1c                   ENDSL
032400060201      *
032500060201     c                   ENDSR
032600060201      *
032700060201      *---------------------------------------------------------------*
032800060201      * Inizializzazione videata D01                                  *
032900060201      *---------------------------------------------------------------*
033000060201     c     InzD01        BEGSR
033100060201      *
033200060201     c                   clear                   TN52D01
033300060202      *
033400060202      * - 40 = Pgm richiamato
033500060202     c                   eval      *in40 = ($Called = *on)
033600060201      *
033700060201      * Imposto i dati di default
033800060201     c                   move      '?'           V1Cccf
033900060201      *
034000060201     c                   ENDSR
034100060201      *
034200060201      *---------------------------------------------------------------*
034300060201      * Gestione tasto funzionale F03 da videata D01                  *
034400060201      *---------------------------------------------------------------*
034500060201     c     F03D01        BEGSR
034600060201      *
034700060201      * Chiudo il programma
034800060201     c                   eval      $Fine   = *on
034900060201      *
035000060201     c                   ENDSR
035100060201      *
035200060201      *---------------------------------------------------------------*
035300060201      * Gestione tasto funzionale F19 da videata D01                  *
035400060201      *---------------------------------------------------------------*
035500060201     c     F19D01        BEGSR
035600060201      *
035700060201     c     DigitN        check     V1Cccf
035800060201     c                   if        NOT %found
035900060201     c                   clear                   KPJBU
036000060201     c                   eval      %subst(KPJBU:1:7) = %subst(V1Cccf:2:7)
036100060201     c                   else
036200060201     c                   movel(p)  wZ7           KPJBU
036300060201     c                   endif
036400060201      *
036500060201      * F19=Gestione dati consegna/giacenza per clienti
036600060201     c                   call      'TRTB40R'
036700060201     c                   parm                    KPJBA
036800060201      *
036900060201     c                   ENDSR
037000060201      *
037100060201      *---------------------------------------------------------------*
037200060201      * Gestione tasto funzionale F20 da videata D01                  *
037300060201      *---------------------------------------------------------------*
037400060201     c     F20D01        BEGSR
037500060201      *
037600060201     c     DigitN        check     V1Cccf
037700060201     c                   if        NOT %found
037800060201     c                   clear                   KPJBU
037900060201     c                   eval      %subst(KPJBU:1:7) = %subst(V1Cccf:2:7)
038000060201     c                   else
038100060201     c                   movel(p)  wZ7           KPJBU
038200060201     c                   endif
038300060201      *
038400060201      * F20=Gestione dati principali scheda clienti
038500060201     c                   call      'TRTB47R'
038600060201     c                   parm                    KPJBA
038700060201      *
038800060201     c                   ENDSR
038900060201      *
039000060201      *---------------------------------------------------------------*
039100060201      * Gestione tasto funzionale F21 da videata D01                  *
039200060201      *---------------------------------------------------------------*
039300060201     c     F21D01        BEGSR
039400060201      *
039500060201     c     DigitN        check     V1Cccf
039600060201     c                   if        NOT %found
039700060201     c                   clear                   KPJBU
039800060201     c                   eval      %subst(KPJBU:1:7) = %subst(V1Cccf:2:7)
039900060201     c                   else
040000060201     c                   movel(p)  wZ7           KPJBU
040100060201     c                   endif
040200060201      *
040300060201      * F21=Gestione dati bolle da clienti
040400060201     c                   call      'TRTB28R'
040500060201     c                   parm                    KPJBA
040600060201      *
040700060201     c                   ENDSR
040800060201      *
040900060201      *---------------------------------------------------------------*
041000060201      * Gestione tasto funzionale F22 da videata D01                  *
041100060201      *---------------------------------------------------------------*
041200060201     c     F22D01        BEGSR
041300060201      *
041400060201     c     DigitN        check     V1Cccf
041500060201     c                   if        NOT %found
041600060201     c                   clear                   KPJBU
041700060201     c                   eval      %subst(KPJBU:1:7) = %subst(V1Cccf:2:7)
041800060201     c                   else
041900060201     c                   movel(p)  wZ7           KPJBU
042000060201     c                   endif
042100060201      *
042200060201      * F22=Gestione dati c/assegno e fattura per clienti
042300060201     c                   call      'TRTB44R'
042400060201     c                   parm                    KPJBA
042500060201      *
042600060201     c                   ENDSR
042700060201      *
042800060201      *---------------------------------------------------------------*
042900060201      * Controllo dati immessi in videata D01                         *
043000060201      *---------------------------------------------------------------*
043100060201     c     CtrD01        BEGSR
043200060201      *
043300060201     c                   movea     *zeros        *in(50)
043400060201      *
043500060201      * Controllo/Decodifica codice cliente fasi ORM
043600060201      * - Ricerche
043700060201     c     '?'           scan      V1Cccf
043800060201if  1c                   IF        %found
043900060201     c                   eval      $Tibs02 = *on
044000060201     c                   reset                   TIBS02ds
044100060201     c                   movel     KNSIF         T02sif
044200060201     c                   call      'TIBS02R'
044300060201     c                   parm                    KPJBA
044400060201     c                   parm                    TIBS02ds
044500060201     c                   if        T02err  = *blanks
044600060201     c                   movel     T02ke1        V1Cccf
044700060201     c                   endif
044800060201     c                   seton                                          5190
044900060201     c                   leavesr
045000060201e   1c                   ENDIF
045100060201if  1c                   IF             V1Cccf  = *blanks
045200060201     c                             or   V1Cccf  = *zeros
045300060201     c                   movel(p)  V1Dccf        PAXdmt
045400060201     c                   exsr      Search_Cli
045500060201      *   · Imposto il codice selezionato a video
045600060201      *     (se selezionato e non premuto F12)
045700060201if  2c                   if        PAXsta >= *zeros
045800060201     c***                movel     PAXksm        V1Cccf
045900060201     c                   eval      V1Cccf  = '0' + %subst(PAXksm:1:7)
046000060201     c                   movel     PAXdmt        V1Dccf
046100060201     c                   eval      *in90   = *on
046200060201     c                   leavesr
046300060201e   2c                   endif
046400060201e   1c                   endif
046500060201      * - Controllo
046600060201     c                   clear                   V1Dccf
046700071005if  1c                   if            V1Cccf  = *zeros
046800060201     c                             or  V1Cccf  = *blanks
046900060201     c                   seton                                        285190
047000060201     c                   eval      V1Dmsg  = $Msg(1)
047100071005     c                   leavesr
047200071005e   1c                   endif
047300060201     c     DigitN        check     V1Cccf
047400071005if  1c                   if        %found
047500060201     c                   seton                                        285190
047600060201     c                   eval      V1Dmsg  = $Msg(2)
047700060201     c                   leavesr
047800071005e   1c                   endif
047900060201     c                   move      V1Cccf        Wksc
048000060201     c                   exsr      Decod_Cli
048100071005if  1c                   if        O69err  = *on
048200060201     c                   seton                                        285190
048300060201     c                   eval      V1Dmsg  = $Msg(2)
048400060201     c                   leavesr
048500071005e   1c                   endif
048600060201     c                   movel     ACOrag        V1Dccf
048700071005      *
048800071005      * Reperimento codice padre
048900071005      * (i controlli verranno eseguiti nella subr "CtrD02")
049000091026     c                   clear                   TIBS10ds
049100091026     c                   eval      D10paf = 'P'                                 *cerca i figli
049200091026     c                   move      V1Cccf        D10cod                         *codice
049300071005     c                   eval      D10drf = wDate                               *data riferimento
049400091026     C*
049500091026     C* Preparazione chiamata al TIBS10R1
049600091026     C                   clear                   tibs10r1ds
049700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
049800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
049900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
050000091026     C*
050100091026     C                   call      'TIBS10R1'
050200091026     C                   parm                    tibs10r1ds
050300091026     C*
050400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050500091026     C*
050600071005if  1c                   if            D10err <> *blanks
050700071005     c                             or  D10cop =  *zeros
050800071005     c                             or  D10cop =  D10cod
050900071005      * - padre o senza unif.
051000071005     c                   move      D10cod        wPadre
051100071005x   1c                   else
051200071005      * - figlio
051300071005     c                   move      D10cop        wPadre
051400071005e   1c                   endif
051500060201      *
051600060201     c                   ENDSR
051700060201      *
051800060201      *---------------------------------------------------------------*
051900060201      * Gestione videata D02                                          *
052000060201      *---------------------------------------------------------------*
052100060201     c     GesD02        BEGSR
052200060201      *
052300060201      * Inizializzo la videata
052400060201if  1c                   if        $InzD02 = *on
052500060201     c                   exsr      InzD02
052600060201     c                   movel     *off          $InzD02
052700060201e   1c                   endif
052800060201      *
052900060201      * Emetto la testata
053000060201     c                   write     TN52T01
053100060201      *
053200060201      * Emetto la videata
053300060202     c***                if        $Found  = *off
053400060202     c                   if        NOT *in04
053500060201     c                   exfmt     TN52D02
053600060201     c                   else
053700060201     c                   write     TN52D02
053800060201     c                   exfmt     PROTECT
053900060201     c                   endif
054000060201     c                   setoff                                       28  90
054100060201     c                   movea     *zeros        *in(50)
054200060201     c                   clear                   V1Dmsg
054300060201     c                   clear                   $Fxx
054400060201      *
054500060201     c                   SELECT
054600060201      *
054700060201      * F3=Fine
054800060201w   1c                   WHEN      *inKC
054900060201     c                   exsr      F03D01
055000060201     c                   leavesr
055100060201      *
055200060201      * F12=Ritorno
055300060201w   1c                   WHEN      *inKL
055400060201     c                   exsr      F12D02
055500060201     c                   leavesr
055600060201      *
055700060201      * F16=Annullamento
055800060201w   1c                   WHEN      *inKQ
055900060201     c*** non adesso:    exsr      F16D02
056000060201     c                   eval      $Fxx    = 'F16'
056100060201     c                   eval      $Video  =  'A'
056200060201     c                   eval      $InzW01 =  *on
056300060201     c                   leavesr
056400060201      *
056500060201      * F19=Gestione dati consegna/giacenza per clienti
056600060201w   1c                   WHEN      *inKT
056700060201     c                   exsr      F19D02
056800060201     c                   leavesr
056900060201      *
057000060201      * F20=Gestione dati principali scheda clienti
057100060201w   1c                   WHEN      *inKU
057200060201     c                   exsr      F20D02
057300060201     c                   leavesr
057400060201      *
057500060201      * F21=Gestione dati bolle da clienti
057600060201w   1c                   WHEN      *inKV
057700060201     c                   exsr      F21D02
057800060201     c                   leavesr
057900060201      *
058000060201      * F22=Gestione dati c/assegno e fattura per clienti
058100060201w   1c                   WHEN      *inKW
058200060201     c                   exsr      F22D02
058300060201     c                   leavesr
058400060201      *
058500060201      * Controllo dati immessi a video
058600060201x   1c                   OTHER
058700060201     c                   exsr      CtrD02
058800060201if  2c                   if        *in90
058900060201     c                   leavesr
059000060201e   2c                   endif
059100060201sel 2c                   Select
059200060201      * F5=Ripristino
059300060201if  2c                   When      *inKE
059400060201     c*** non adesso:    exsr      F05D02
059500060201     c                   eval      $Fxx    = 'F05'
059600060201     c                   eval      $Video  =  'A'
059700060201     c                   eval      $InzW01 =  *on
059800060201      * F6=Aggiornamento
059900060201if  2c                   When      *inKF
060000060201     c*** non adesso:    exsr      F06D02
060100060201     c                   eval      $Fxx    = 'F06'
060200060201     c                   eval      $Video  =  'A'
060300060201     c                   eval      $InzW01 =  *on
060400060201e   2c                   EndSl
060500060201      *
060600060201e   1c                   ENDSL
060700060201      *
060800060201     c                   ENDSR
060900060201      *
061000060201      *---------------------------------------------------------------*
061100060201      * Inizializzazione videata D02                                  *
061200060201      *---------------------------------------------------------------*
061300060201     c     InzD02        BEGSR
061400060201      *
061500060201     c                   clear                   TN52D02
061600060201      *
061700060201      * Reperimento record da TNTBE01L
061800060201     c                   exsr      Rep_TNTBE
061900060201      *
062000060201      * - 01 = Record inesistente (inserimento)
062100060201     c                   eval      *in01 = ($Found = *off)
062200060201      * - 02 = Record esistente   (modifica)
062300060201     c                   eval      *in02 = ($Found = *on)
062400060201      * - 04 = Record annullato   (ripristino)
062500060201     c                   eval      *in04 = ($Found = *on  and
062600060201     c                                      TBEatb = 'A')
062700060202      * - 40 = Pgm richiamato
062800060202     c                   eval      *in40 = ($Called = *on)
062900060201      *
063000060201     c                   select
063100060201     c                   when      *in04
063200060201     c                   eval      VTDope  = $Sts(3)
063300060201     c                   when      *in02
063400060201     c                   eval      VTDope  = $Sts(2)
063500060201     c                   when      *in01
063600060201     c                   eval      VTDope  = $Sts(1)
063700060201     c                   endsl
063800060201      *
063900060201     c                   eval      V2Cccf  = V1Cccf
064000060201     c                   eval      V2Dccf  = V1Dccf
064100060201      *
064200060201     c                   movel     TBEke2        V2Ccci
064300060201      *
064400060201      * Decodifiche
064500060201     c                   if        $Found  = *on
064600060201     c                   exsr      CtrD02
064700060201     c                   setoff                                       28  90
064800060201     c                   movea     *zeros        *in(51)
064900060201     c                   endif
065000060201      *
065100060201     c                   ENDSR
065200060201      *
065300060201      *---------------------------------------------------------------*
065400060201      * Gestione tasto funzionale F05 da videata D02                  *
065500060201      *---------------------------------------------------------------*
065600060201     c     F05D02        BEGSR
065700060201      *
065800060201      * Ripristino record annullato
065900060201     c                   clear                   TBEatb
066000060201      *
066100060201     c                   UPDATE    TNTBE000
066200060201      *
066300060201     c                   if        $Called = *off
066400060202      * - Ritorno alla videata precedente
066500060201     c                   eval      $Video  = '1'
066600060201     c                   eval      $InzD01 = *on
066700060201     c                   eval      $InzD02 = *on
066800060201     c                   else
066900060202      * - Uscita dal pgm
067000060201     c                   exsr      F03D01
067100060201     c                   endif
067200060201      *
067300060201     c                   ENDSR
067400060201      *
067500060201      *---------------------------------------------------------------*
067600060201      * Gestione tasto funzionale F16 da videata D02                  *
067700060201      *---------------------------------------------------------------*
067800060201     c     F16D02        BEGSR
067900060201      *
068000060201      * Annullamento record
068100060201     c                   eval      TBEatb  = 'A'
068200060201      *
068300060201     c                   UPDATE    TNTBE000
068400060201      *
068500060201     c                   if        $Called = *off
068600060202      * - Ritorno alla videata precedente
068700060201     c                   eval      $Video  = '1'
068800060201     c                   eval      $InzD01 = *on
068900060201     c                   eval      $InzD02 = *on
069000060201     c                   else
069100060202      * - Uscita dal pgm
069200060201     c                   exsr      F03D01
069300060201     c                   endif
069400060201      *
069500060201     c                   ENDSR
069600060201      *
069700060201      *---------------------------------------------------------------*
069800060201      * Gestione tasto funzionale F06 da videata D02                  *
069900060201      *---------------------------------------------------------------*
070000060201     c     F06D02        BEGSR
070100060201      *
070200060201      * Inserimento nuovo record
070300060202if  1c                   IF        $Found  = *off
070400060202      *
070500060202     c*** già così:      clear                   TBEatb
070600060202     c*** già impostato: eval      TBEsif  = TBXsif
070700060202     c*** già impostato: eval      TBElin  = TBXlin
070800060202     c*** già impostato: eval      TBEapl  = TBXapl
070900060202     c*** già impostato: eval      TBEcod  = C_tab
071000071005     c*** già impostato: movel(p)  V1Cccf        TBEke1
071100060201     c                   movel(p)  V2Ccci        TBEke2
071200060202     c*** già così:      clear                   TBEuni
071300060201     c*** già impostato: eval      TBEftt  =
071400060201     c*** già impostato: eval      TBEflt  =
071500060201     c*** già impostato: eval      TBEftr  =
071600060201     c*** già impostato: eval      TBEdtr  =
071700060201      *
071800060201     c                   WRITE     TNTBE000
071900071005      *
072000071005      * - Verifica l'esistenza del rec. con KE1 = KE2 = V1CCCI
072100071005     c                   movel     C_Tab         TBEcod
072200071005     c                   movel(p)  V2Ccci        TBEke1
072300071005     c                   movel(p)  V2Ccci        TBEke2
072400071005     c*** già impostato: eval      TBEsif  = TBXsif
072500071005     c*** già impostato: eval      TBElin  = TBXlin
072600071005     c     K05TBE01      chain     TNTBE000
072700071005if  2c                   if        NOT %found(TNTBE01L)
072800071005     c*** già così:      clear                   TBEatb
072900071005     c*** già impostato: eval      TBEsif  = TBXsif
073000071005     c*** già impostato: eval      TBElin  = TBXlin
073100071005     c*** già impostato: eval      TBEapl  = TBXapl
073200071005     c*** già impostato: eval      TBEcod  = C_tab
073300071005     c*** già impostato: movel(p)  V2Ccci        TBEke1
073400071005     c*** già impostato: movel(p)  V2Ccci        TBEke2
073500071005     c*** già così:      clear                   TBEuni
073600071005     c*** già impostato: eval      TBEftt  =
073700071005     c*** già impostato: eval      TBEflt  =
073800071005     c*** già impostato: eval      TBEftr  =
073900071005     c*** già impostato: eval      TBEdtr  =
074000071005     c                   WRITE     TNTBE000
074100071005e   2c                   endif
074200060201      *
074300071005if  2c                   if        $Called = *off
074400060202      * - Ritorno alla videata precedente
074500060201     c                   eval      $Video  = '1'
074600060201     c                   eval      $InzD01 = *on
074700060201     c                   eval      $InzD02 = *on
074800071005x   2c                   else
074900060202      * - Uscita dal pgm
075000060201     c                   exsr      F03D01
075100071005e   2c                   endif
075200060202      *
075300060202      * Modifica record già esistente
075400060202x   1c                   ELSE
075500060202      *
075600060202     c                   movel(p)  V2Ccci        TBEke2
075700060202      *
075800060202     c                   UPDATE    TNTBE000
075900060202      *
076000071005if  2c                   if        $Called = *off
076100060202      * - Ritorno alla videata precedente
076200060202     c                   eval      $Video  = '1'
076300060202     c                   eval      $InzD01 = *on
076400060202     c                   eval      $InzD02 = *on
076500071005x   2c                   else
076600060202      * - Uscita dal pgm
076700060202     c                   exsr      F03D01
076800071005e   2c                   endif
076900060202      *
077000060202e   1c                   ENDIF
077100060201      *
077200060201     c                   ENDSR
077300060201      *
077400060201      *---------------------------------------------------------------*
077500060201      * Gestione tasto funzionale F12 da videata D02                  *
077600060201      *---------------------------------------------------------------*
077700060201     c     F12D02        BEGSR
077800060201      *
077900060201     c                   UNLOCK    TNTBE01L
078000060201      *
078100060201     c                   if        $Called = *off
078200060202      * - Ritorno alla videata precedente
078300060201     c                   eval      $Video  = '1'
078400060201     c                   else
078500060202      * - Uscita dal pgm
078600060201     c                   exsr      F03D01
078700060201     c                   endif
078800060201      *
078900060201     c                   ENDSR
079000060201      *
079100060201      *---------------------------------------------------------------*
079200060201      * Gestione tasto funzionale F19 da videata D02                  *
079300060201      *---------------------------------------------------------------*
079400060201     c     F19D02        BEGSR
079500060201      *
079600060201     c     DigitN        check     V2Ccci
079700060201     c                   if        NOT %found
079800060201     c                   clear                   KPJBU
079900060201     c                   eval      %subst(KPJBU:1:7) = %subst(V2Ccci:2:7)
080000060201     c                   else
080100060201     c                   movel(p)  wZ7           KPJBU
080200060201     c                   endif
080300060201      *
080400060201      * F19=Gestione dati consegna/giacenza per clienti
080500060201     c                   call      'TRTB40R'
080600060201     c                   parm                    KPJBA
080700060201      *
080800060201     c                   ENDSR
080900060201      *
081000060201      *---------------------------------------------------------------*
081100060201      * Gestione tasto funzionale F20 da videata D02                  *
081200060201      *---------------------------------------------------------------*
081300060201     c     F20D02        BEGSR
081400060201      *
081500060201     c     DigitN        check     V2Ccci
081600060201     c                   if        NOT %found
081700060201     c                   clear                   KPJBU
081800060201     c                   eval      %subst(KPJBU:1:7) = %subst(V2Ccci:2:7)
081900060201     c                   else
082000060201     c                   movel(p)  wZ7           KPJBU
082100060201     c                   endif
082200060201      *
082300060201      * F20=Gestione dati principali scheda clienti
082400060201     c                   call      'TRTB47R'
082500060201     c                   parm                    KPJBA
082600060201      *
082700060201     c                   ENDSR
082800060201      *
082900060201      *---------------------------------------------------------------*
083000060201      * Gestione tasto funzionale F21 da videata D02                  *
083100060201      *---------------------------------------------------------------*
083200060201     c     F21D02        BEGSR
083300060201      *
083400060201     c     DigitN        check     V2Ccci
083500060201     c                   if        NOT %found
083600060201     c                   clear                   KPJBU
083700060201     c                   eval      %subst(KPJBU:1:7) = %subst(V2Ccci:2:7)
083800060201     c                   else
083900060201     c                   movel(p)  wZ7           KPJBU
084000060201     c                   endif
084100060201      *
084200060201      * F21=Gestione dati bolle da clienti
084300060201     c                   call      'TRTB28R'
084400060201     c                   parm                    KPJBA
084500060201      *
084600060201     c                   ENDSR
084700060201      *
084800060201      *---------------------------------------------------------------*
084900060201      * Gestione tasto funzionale F22 da videata D02                  *
085000060201      *---------------------------------------------------------------*
085100060201     c     F22D02        BEGSR
085200060201      *
085300060201     c     DigitN        check     V2Ccci
085400060201     c                   if        NOT %found
085500060201     c                   clear                   KPJBU
085600060201     c                   eval      %subst(KPJBU:1:7) = %subst(V2Ccci:2:7)
085700060201     c                   else
085800060201     c                   movel(p)  wZ7           KPJBU
085900060201     c                   endif
086000060201      *
086100060201      * F22=Gestione dati c/assegno e fattura per clienti
086200060201     c                   call      'TRTB44R'
086300060201     c                   parm                    KPJBA
086400060201      *
086500060201     c                   ENDSR
086600060201      *
086700060201      *---------------------------------------------------------------*
086800060201      * Controllo dati immessi in videata D02                         *
086900060201      *---------------------------------------------------------------*
087000060201     c     CtrD02        BEGSR
087100060201      *
087200060201     c                   movea     *zeros        *in(50)
087300060201      *
087400060201      * Controllo/Decodifica codice cliente di invio
087500060201     c     '?'           scan      V2Ccci
087600060201      * - Ricerca
087700060201if  1c                   IF             %found
087800060201     c                             or   V2Ccci  = *blanks
087900060201     c                             or   V2Ccci  = *zeros
088000060201     c                   movel(p)  V2Dcci        PAXdmt
088100060201     c                   exsr      Search_Cli
088200060201      *   · Imposto il codice selezionato a video
088300060201      *     (se selezionato e non premuto F12)
088400060201if  2c                   if        PAXsta >= *zeros
088500060201     c***                movel     PAXksm        V2Ccci
088600060201     c                   eval      V2Ccci  = '0' + %subst(PAXksm:1:7)
088700060201     c                   movel     PAXdmt        V2Dcci
088800060201     c                   seton                                          5290
088900060201     c                   leavesr
089000060201e   2c                   endif
089100060201      * - Controllo
089200060201x   1c                   ELSE
089300060201     c                   clear                   V2Dcci
089400060201if  2c                   If            V2Ccci  = *zeros
089500060201     c                             or  V2Ccci  = *blanks
089600060201     c                   seton                                        285290
089700060201     c                   eval      V1Dmsg  = $Msg(1)
089800060201     c                   leavesr
089900060201x   2c                   Else
090000060203     c     DigitN        check     V2Ccci
090100060201     c                   if        %found
090200060201     c                   seton                                        285290
090300060201     c                   eval      V1Dmsg  = $Msg(2)
090400060201     c                   leavesr
090500060201     c                   endif
090600060201     c                   move      V2Ccci        Wksc
090700060201     c                   exsr      Decod_Cli
090800060201if  3c                   if        O69err  = *on
090900060201     c                   seton                                        285290
091000060201     c                   eval      V1Dmsg  = $Msg(2)
091100060201     c                   leavesr
091200060201x   3c                   else
091300060201     c                   movel     ACOrag        V2Dcci
091400060201e   3c                   endif
091500060201e   2c                   EndIf
091600060201e   1c                   ENDIF
091700060201      *
091800071005      * Verifica abilitazione del?cliente "invio"?allo scambio dati
091900060201      *   del tipo "VP"
092000071005     c                   eval      VIRtip  = 'VP'
092100060201     c     K02VIR02      chain     TIVIR000
092200060201if  2c                   if        NOT %found(TIVIR02L)
092300060201     c*** no ctrl data:            or  VSSdti  > wDate
092400060201     c*** no ctrl data:            or  VSSdtf  < wDate
092500060201     c                   seton                                        285290
092600060201     c                   eval      V1Dmsg  = $Msg(4)
092700060201     c                   leavesr
092800060201e   2c                   endif
092900060201      *
093000071005if  1c                   IF        V1Cccf <> V2Ccci
093100071005      * Verifica abilitazione del?cliente "fasi"?al servizio T&T
093200071005     c                   eval      VSSksu =  V1Cccf
093300071005     c     K02VSS02      chain     TIVSS000
093400060201if  2c                   if        NOT %found(TIVSS02L)
093500060201     c*** no ctrl data:            or  VSSdde  > wDate
093600060201     c*** no ctrl data:            or  VSSdsc  < wDate
093700071005     c                             and V1Cccf <> wPadre
093800071005     c                   eval      VSSksu =  wPadre
093900071005     c     K02VSS02      chain     TIVSS000
094000071005e   2c                   endif
094100071005if  2c                   if        NOT %found(TIVSS02L)
094200071005     c*** no ctrl data:            or  VSSdde  > wDate
094300071005     c*** no ctrl data:            or  VSSdsc  < wDate
094400071005      * Verifica abilitazione del?cliente "invio"?al tipo file "VO"
094500071005      *   nel file TIVIR
094600071005     c                   eval      VIRtip  = 'VO'
094700071005     c     K02VIR02      chain     TIVIR000
094800071005if  3c                   if        NOT %found(TIVIR02L)
094900060201     c                   seton                                        285290
095000060201     c                   eval      V1Dmsg  = $Msg(5)
095100060201     c                   leavesr
095200071005e   3c                   endif
095300060201e   2c                   endif
095400071005e   1c                   ENDIF
095500060201      *
095600060201     c                   ENDSR
095700060201      *
095800060201      *---------------------------------------------------------------*
095900060201      * Reperimento dati da TNTBE01L                                  *
096000060201      *---------------------------------------------------------------*
096100060201     c     Rep_TNTBE     BEGSR
096200060201      *
096300060201     c                   reset                   $Found
096400060201     c                   movel     C_Tab         TBEcod
096500060201     c                   movel(p)  V1Cccf        TBEke1
096600060201     c***                clear                   TBEke2
096700060201     c***                clear                   TBElin
096800060201     c***                movel     TBXsif        TBEsif
096900060201     c     K02TBE01      setll     TNTBE000
097000060201     c     K02TBE01      reade     TNTBE000
097100060201      * Se non ho trovato il record con il sistema informativo
097200060201      *   del record di "indice" riprovo
097300060201     c                   dow       NOT %eof(TNTBE01L)
097400060201     c                             and TBEsif <> TBXsif
097500060201     c     K02TBE01      reade     TNTBE000
097600060201     c                   enddo
097700060201      *
097800060201      * Record non trovato per il codice KE1
097900060201     c                   if        %eof(TNTBE01L)
098000060202     c                   clear                   TNTBE000
098100060202     c                   eval      TBEsif  = TBXsif
098200060202     c                   eval      TBElin  = TBXlin
098300060202     c                   eval      TBEapl  = TBXapl
098400060202     c                   eval      TBEcod  = C_tab
098500060202     c                   movel(p)  V1Cccf        TBEke1
098600060201     c                   leavesr
098700060201     c                   endif
098800060201      *
098900060201     c                   eval      $Found  = *on
099000060201      *
099100060201     c                   ENDSR
099200060201      *
099300060201      *---------------------------------------------------------------*
099400060201      * Ricerca cliente                                               *
099500060201      *---------------------------------------------------------------*
099600060201     c     Search_Cli    BEGSR
099700060201      *
099800060201     c                   call      'XALFA3BR'
099900060201     c                   parm      RSUT          PAXdut           30
100000060201     c                   parm      1             PAXkut            1 0
100100060201     c                   parm                    PAXdmt           48
100200060201     c                   parm      DUTkci        PAXccc            4 0
100300060201     c                   parm      9             PAXsta            1 0
100400060201     c                   parm                    PAXflr           90
100500060201     c                   parm      *blanks       PAXdit            3
100600060201     c                   parm      1             PAXnum            2 0
100700060201     c                   parm                    PAXksc           80
100800060201     c                   parm                    PAXksm          140
100900060201     c                   parm                    PAXkdm           60
101000060201     *** *
101100060201     *** *   · Imposto il codice selezionato a video
101200060201     *** *     (se selezionato e non premuto F12)
101300060201if  1***c                   if        PAXsta >= *zeros
101400060201     ***c                   movel     PAXksm        V1Cccf
101500060201     ***c                   movel     PAXdmt        V1Dccf
101600060201e   1***c                   endif
101700060201      *
101800060201     c                   ENDSR
101900060201      *
102000060201      *---------------------------------------------------------------*
102100060201      * Decodifica cliente                                            *
102200060201      *---------------------------------------------------------------*
102300060201     c     Decod_Cli     BEGSR
102400060201      *
102500060201     c                   clear                   TIBS69ds
102600060201     c                   clear                   ds_CNACO
102700060201     c                   clear                   ds_CNIND
102800060201     c                   clear                   ds_CNCLP
102900060201     c                   clear                   ds_FNCLS
103000060201      *
103100060201     c                   z-add     Wksc          I69kac
103200060201      *
103300060201     c                   call      'TIBS69R'
103400060201     c                   parm                    TIBS69ds
103500060201     c                   parm                    ds_CNACO
103600060201     c                   parm                    ds_CNIND
103700060201     c                   parm                    ds_CNCLP
103800060201     c                   parm                    ds_FNCLS
103900060201      *
104000060201     c                   ENDSR
104100060201      *
104200060201      *---------------------------------------------------------------*
104300060201      * Gestione videata W01 (dati relativi alla trasmissione)        *
104400060201      *---------------------------------------------------------------*
104500060201     c     GesW01        BEGSR
104600060201      *
104700060201      * Inizializzo la videata
104800060201if  1c                   if        $InzW01 = *on
104900060201     c                   exsr      InzW01
105000060201     c                   movel     *off          $InzW01
105100060201e   1c                   endif
105200060201      *
105300060201      * Emetto la window
105400060201     c                   exfmt     TN52W01
105500060201     c                   setoff                                       28  90
105600060202     c                   movea     *zeros        *in(50)
105700060201     c                   clear                   V1Dmsg
105800060201      *
105900060201     c                   SELECT
106000060201      *
106100060201      * F12=Ritorno
106200060201w   1c                   WHEN      *inKL
106300060201     c                   exsr      F12W01
106400060201     c                   leavesr
106500060201      *
106600060201      * Controllo dati immessi a video
106700060201x   1c                   OTHER
106800060201     c                   exsr      CtrW01
106900060201if  2c                   if        *in90
107000060201     c                   leavesr
107100060201e   2c                   endif
107200060201      * Aggiornamento:
107300060201sel 2c                   select
107400060201      * - Invio (No F6=Conferma)
107500060201w   2c                   when      NOT *inKF
107600060201      * - F5=Ripristino da D02
107700060201w   2c                   when      $Fxx    = 'F05'
107800060201     c                   exsr      F05D02
107900060201      * - F6=Aggiornamento da D02
108000060201w   2c                   when      $Fxx    = 'F06'
108100060201     c                   exsr      F06D02
108200060201      * - F16=Annullamento da D02
108300060201w   2c                   when      $Fxx    = 'F16'
108400060201     c                   exsr      F16D02
108500060201e   2c                   endsl
108600060201      *
108700060201e   1c                   ENDSL
108800060201      *
108900060201     c                   ENDSR
109000060201      *
109100060201      *---------------------------------------------------------------*
109200060201      * Inizializzazione videata W01                                  *
109300060201      *---------------------------------------------------------------*
109400060201     c     InzW01        BEGSR
109500060201      *
109600060201     c                   clear                   TN52W01
109700060201      *
109800060201sel 1c                   select
109900060201      *
110000060201      * F5=Ripristino
110100060201w   1c                   when      *inKE   and  *in04
110200060201     c                   eval      W1ftt   = TBEftt
110300060201      *
110400060201      * F6=Conferma
110500060201w   1c                   when      *inKF
110600060201sel 2c                   select
110700060201      * - Immissione
110800060201w   2c                   when      *in01
110900060201     c                   eval      W1ftt   = TBXftt
111000060201      * - Modifica / Ripristino
111100060201w   2c                   when      *in02   or   *in04
111200060201     c                   eval      W1ftt   = TBEftt
111300060201e   2c                   endsl
111400060201      *
111500060201      * F16=Annullamento
111600060201w   1c                   when      *inKQ   and  NOT *in04
111700060201     c                   eval      W1ftt   = TBEftt
111800060201
111900060201e   1c                   endsl
112000060201      *
112100060201      * Se NON immissione:
112200060201      *   visualizzo i dati relativi all'ultima trasmissione
112300060201if  1c                   if        NOT *in01
112400060201     c                   eval      W1flt   = tbeflt
112500060201     c                   eval      W1ftr   = tbeftr
112600060201if  2c                   if        TBEdtr <> *zeros
112700060201     c                   reset                   WLBdat
112800060201     c                   z-add     TBEdtr        G08inv
112900060201     c                   call      'XSRDA8'
113000060201     c                   parm                    WLBdat
113100060201     c                   z-add     G08dat        W1dtr
113200060201e   2c                   endif
113300060201e   1c                   endif
113400060201      *
113500060201     c                   ENDSR
113600060201
113700060201      *---------------------------------------------------------------*
113800060201      * Controllo dati immessi in videata W01                         *
113900060201      *---------------------------------------------------------------*
114000060201     c     CtrW01        BEGSR
114100060201      *
114200060201     c                   movea     *zeros        *in(50)
114300060201      *
114400060201     c                   ENDSR
114500060201      *
114600060201      *---------------------------------------------------------------*
114700060201      * Gestione tasto funzionale F12 da videata W01                  *
114800060201      *---------------------------------------------------------------*
114900060201     c     F12W01        BEGSR
115000060201      *
115100060201     c                   eval      $Video  = '2'
115200060201      *
115300060201     c                   ENDSR
115400060201
115500060201** - $STS
115600060201IMMISSIONE
115700060201VARIAZIONE
115800060201ANNULLATO
115900060201 BLOCCATO
116000060201** - $MSG -------------------------------------------------------------------*
116100010521Immettere il Codice Cliente                                                    1
116200010521Cliente annullato o bloccato                                                   2
116300060130Cliente già presente in tabella                                                3
116400060201Cliente di invio non abilitato allo scambio dati tipo file 'VP'                4
116500071005Cliente fasi O.R.M. non abilitato al servizio 'TT' né allo scambio dati 'VO'   5
116600091026Cliente fasi O.R.M. non unificante per legame di tipo internet                 6
