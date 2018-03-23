000100070321      * FIDG32R *-----------------------------------------------------*
000200070321      *?STAMPA O RISTAMPA LDV PER CODICE GIRO - FILTRO DI LANCIO     ?*
000300070321      *---------------------------------------------------------------*
000400070321
000500070321     h decedit('0,') datedit(*dmy/)
000600070321
000700070321      *---------------------------------------------------------------*
000800070321
000900070321     fAZORG01L  if   e           k disk
001000070321      *
001100070321     fFIGRG03L  if   e           k disk
001200070321      *
001300070321     fFIDG32D   cf   e             workstn
001400070321
001500070321      *---------------------------------------------------------------*
001600070321
001700070321      *
001800070321      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001900070321      *
002000070321      *
002100070321      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002200070321      *
002300070321      * - Messaggi di errore
002400070402     d $Msg            s             78    dim( 4) ctdata perrcd(1)
002500070321      *
002600070321      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002700070321      *
002800070321      * - Parametri
002900070321     d KPJBA         e ds
003000070321      *
003100070321      * - Parametri per FIDG33R: Stampa LdV per codice giro (batch)
003200070321     d FIDG33ds      e ds                  inz
003300070321      *
003400070321      * - Parametri x Controllo profilo utenti
003500070321     d TIBS34ds      e ds
003600070321      * - Ds di riferimento al file esterno AZUTE00F
003700070321     d AZUTEds       e ds                       extname(AZUTE00F)
003800070321      * - Ds per dati organigramma
003900070321     d dDatiUte      e ds
004000070321      *
004100070321     d OG146         e ds                  inz
004200070321     d OG148         e ds                  inz
004300070322     d OG146cf£6     e ds                  inz  extname(OG146)  prefix(£6:1)
004400070321     d OG148cf£6     e ds                  inz  extname(OG148)  prefix(£6:1)
004500070321      *
004600070321      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
004700070321     d FNLV55ds      e ds                  inz
004800070321     d  D55tpt       e                     inz('6')
004900070321      *
005000070321      * - Parametri per richiesta stampante per BOLLE ed ETICHETTE
005100070321     d TRUL90ds      e ds                  inz
005200070321     d***D90rse      e                     inz('S')
005300070321     d   D90rsb      e                     inz('L')
005400070321      *
005500070321      * - Parametri per FNLV50R: Controllo Filiale in Gestione
005600070321     d FNLV50ds      e ds                  inz
005700070321      *
005800070321      * - Parametri per FIDG09R: Interrogazione Giri
005900070321     d FIDG09ds      e ds                  inz
006000070321     d  D09iop0      e                     inz('P01')
006100070321      *
006200070321     d Status         sds
006300070321     d  SDSpgm           *proc
006400070321     d  SDSjbnm              244    253
006500070321     d  SDSjbus              254    263
006600070321     d  SDSjbnr              264    269s 0
006700070321      *
006800070321      * - Parametri per pgm. controllo data
006900070321     d WLBdat          ds                  inz
007000070321     d  G08dat                        8  0 inz
007100070321     d  G08inv                        8  0 inz
007200070321     d  G08err                        1    inz('3')
007300070321     d  G08tgi                        5  0 inz
007400070321      *
007500070321      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007600070321      *
007700070321      * - Flags
007800070321     d $Fine           s              1    inz(*off)
007900070321     d $InzD01         s              1    inz(*on)
008000070321      *
008100070321      * - Variabili di comodo
008200070321     d wDateA          s              8    inz
008300070321     d wDate           s              8  0 inz
008400070321     d W1Casl          s              8  0 inz
008500070321      *
008600070321      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008700070321      *
008800070321      * - FIGRG03L
008900070321     c     K02GRG03      klist
009000070321     c                   kfld                    GRGfgs
009100070321     c                   kfld                    GRGcgi
009200070321
009300070321      *---------------------------------------------------------------*
009400070402      *?RIEPILOGO INDICATORI                                         ?*
009500070321      *---------------------------------------------------------------*
009600070321      * 06   - Disabilitazione tasto funzionale F6                    *
009700070321      *        (Filiale non abilitata)                                *
009800070321      * 18   - Disabilitazione tasto funzionale F18  e                *
009900070321      *        Sprotezione campo "Filiale Gestione"                   *
010000070321      * 25   - Comodo
010100070321      * 28   - Emissione messaggio di errore                          *
010200070321      * 50   - Filiale Gestione errata                                *
010300070321      * 51   - Codice Giro errato                                     *
010400070321      * 52   - Data da ristampare errata                              *
010500070321      * 90   - Riemissione videata                                    *
010600070321      *---------------------------------------------------------------*
010700070321
010800070321     c     *Entry        plist
010900070321     c                   parm                    KPJBA
011000070321      *
011100070321      * Operazioni iniziali
011200070321     c                   exsr      sr_Start
011300070321      *
011400070321      * Ciclo di gestione della videata di lancio
011500070321do  1c                   dow       $Fine   =  *off
011600070321     c                   exsr      GesD01
011700070321e   1c                   enddo
011800070321      *
011900070321      * Fine
012000070321     c                   eval      *inLR   =  *on
012100070321      *
012200070321      *---------------------------------------------------------------*
012300070321      *?Operazioni Iniziali                                          ?*
012400070321      *---------------------------------------------------------------*
012500070321     c     sr_Start      BEGSR
012600070321      *
012700070321      * Reperimento dati utente
012800070321     c     *dtaara       define    §azute        AZUTEds
012900070321     c     *dtaara       define    §datiute      dDATIUTE
013000070321     c                   in(E)     *dtaara
013100070321     c                   if        %ERROR or RSUT = *blanks
013200070321     c                   clear                   TIBS34ds
013300070321     c                   call      'TIBS34R'
013400070321     c                   parm                    TIBS34ds
013500070321     c                   in        *dtaara
013600070321     c                   endif
013700070321      *
013800070321      * Richiesta stampanti
013900070321     c                   reset                   TRUL90ds
014000070321     c                   call      'TRUL90R'
014100070321     c                   parm                    KPJBA
014200070321     c                   parm                    TRUL90ds
014300070321      *
014400070321if  1c                   if        D90F3   =  *on
014500070321     c                   eval      $Fine   =  *on
014600070321     c                   leavesr
014700070321e   1c                   endif
014800070321      *
014900070321      * Impostazione iniziale dei dati fissi in testata
015000070321     c                   eval      V1Cpgm  =  SDSpgm
015100070321      *
015200070321      * Impostazione della *DATE in formato alfanumerico
015300070321     c                   clear                   WLBdat
015400070321     c                   eval      G08dat  =  *date
015500070321     c                   call      'XSRDA8'
015600070321     c                   parm                    WLBdat
015700070321     c                   eval      wDate   =  G08inv
015800070321     c                   move      wDate         wDateA
015900070321      *
016000070321     c                   ENDSR
016100070321      *
016200070321      *---------------------------------------------------------------*
016300070322      *?Gestione videata D01                                         ?*
016400070321      *---------------------------------------------------------------*
016500070321     c     GesD01        BEGSR
016600070321      *
016700070321if  1c                   if        $InzD01 =  *on
016800070321     c                   exsr      InzD01
016900070321     c                   eval      $InzD01 =  *off
017000070321e   1c                   endif
017100070321      *
017200070321      * Emissione testata e piede
017300070321if  1c***                if        NOT *in28
017400070321     c                   write     DG32T01
017500070321     c                   write     DG32Z01
017600070321e   1c***                endif
017700070321      *
017800070321      * Emissione videata di dettaglio
017900070321     c                   exfmt     DG32D01
018000070321     c                   clear                   V1Dmsg
018100070321     c                   setoff                                       28  90
018200070321      *
018300070321sel 1c                   SELECT
018400070321      * - F3 = Fine
018500070321w   1c                   WHEN      *inKC
018600070321     c                   eval      $Fine   =  *on
018700070321      * - F18 = Modifica Filiale in Gestione
018800070321w   1c                   WHEN      *inKS
018900070321     c                   eval      *in18   =  *on
019000070321     c                   eval      *in50   =  *on
019100070321      * - F9 = Personalizzazione Batch
019200070321w   1c***                WHEN      *inKI
019300070321     c***                eval      Kcoaz   =  'LRS9'
019400070321     c***                call      'BCH09'
019500070321     c***                parm                    KPJBA
019600070321      * - Enter/F6: Controllo video D01
019700070321x   1c                   OTHER
019800070321     c                   exsr      CtrD01
019900070321      * - F6 = Lancio batch (se abilitato e se NO errori)
020000070321if  2c                   if                *inKF
020100070321     c                             and NOT *in06
020200070321     c                             and NOT *in90
020300070321     c                   exsr      sr_SbmJob
020400070321     c                   eval      $Fine   =  *on
020500070321e   2c                   endif
020600070321e   1c                   ENDSL
020700070321      *
020800070321     c                   ENDSR
020900070321      *
021000070321      *---------------------------------------------------------------*
021100070321      *?Inizializzazione videata D01                                 ?*
021200070321      *---------------------------------------------------------------*
021300070321     c     InzD01        BEGSR
021400070321      *
021500070321     c                   clear                   DG32D01
021600070321      *
021700070321     c                   if        DUTlpo  =  '2'
021800070321     c                   eval      V1Cfgs  =  DUTpou
021900070321     c                   else
022000070321     c                   eval      V1Cfgs  =  DUTtfp
022100070321     c                   endif
022200070321     c***                clear                   V1Ccgi
022300070321     c***                clear                   V1Casl
022400070321      *
022500070321      * Decodifiche
022600070321     c                   exsr      CtrD01
022700070322if  1c                   if        NOT  *in06
022800070321     c                   movea     *zeros        *in(28)
022900070322e   1c                   endif
023000070321      *
023100070321     c                   ENDSR
023200070321      *
023300070321      *---------------------------------------------------------------*
023400070321      *?Controlli videata D01                                        ?*
023500070321      *---------------------------------------------------------------*
023600070321     c     CtrD01        BEGSR
023700070321      *
023800070321     c                   movea     *zeros        *in(50)
023900070321      *
024000070321      * Controllo filiale gestione
024100070321     c                   clear                   V1Dfgs
024200070321if  1c                   if        V1Cfgs  <> D50fgs
024300070321     c                   clear                   FNLV50ds
024400070321     c                   eval      D50pru  =  KNMUS
024500070321     c                   eval      D50fgs  =  V1Cfgs
024600070321     c                   call      'FNLV50R'
024700070321     c                   parm                    FNLV50ds
024800070321e   1c                   endif
024900070321if  1c                   if        D50err  <> *blanks
025000070321     c                   eval      D50fgs  =  *loval
025100070321     c                   seton                                        285090
025200070321     c                   movel     D50msg        V1Dmsg
025300070321     c                   leavesr
025400070321e   1c                   endif
025500070322      * Decodifica filiale gestione
025600070321if  1c                   if        V1Cfgs  <> ORGfil
025700070321     c     V1Cfgs        chain     AZORG
025800070321if  2c                   if        NOT  %found(AZORG01L)
025900070321     c                             or   ORGfva <> *blanks
026000070321     c                   clear                   ORGdes
026100070322     c                   clear                   OG146
026200070322     c                   clear                   OG148
026300070321e   2c                   endif
026400070321e   1c                   endif
026500070321     c                   movel     ORGdes        V1Dfgs
026600070322     c                   movel     ORGde6        OG146
026700070322     c                   movel     ORGde8        OG148
026800070322      *
026900070322      * - Reperimento capofila (£6)
027000070322      *   (da reperire SEMPRE E COMUNQUE perchè serve la capofila £6)
027100070322     c                   movel     OG146         OG146cf£6
027200070322     c                   movel     OG148         OG148cf£6
027300070322if  1c***                IF        §OGgeot <> 'S'
027400070322     c                   reset                   FNLV55ds
027500070322     c*** già così:      eval      D55tpt  =  '6'
027600070322     c                   eval      D55lin  =  V1Cfgs
027700070322     c                   eval      D55drf  =  wDate
027800070322     c                   call      'FNLV55R'
027900070322     c                   parm                    FNLV55ds
028000070322if  2c                   if        D55err  <> *blanks
028100070322     c                   eval      D55tfa  =  V1Cfgs
028200070322e   2c                   endif
028300070322      * - Reperimento 146ª e 148ª descrizioni dell'organigramma
028400070322if  2c                   if        D55tfa  <> ORGfil
028500070322     c     D55tfa        chain     AZORG
028600070322if  3c                   if        NOT  %found(AZORG01L)
028700070322     c                   clear                   ORGde6
028800070322     c                   clear                   ORGde8
028900070322e   3c                   endif
029000070322e   2c                   endif
029100070322     c                   movel     ORGde6        OG146cf£6
029200070322     c                   movel     ORGde8        OG148cf£6
029300070322if  2c                   if        £6OGdda <= *zeros
029400070322     c                   eval      £6OGdda =  *hival
029500070322e   2c                   endif
029600070322e   1c***                ENDIF
029700070321      *
029800070321      * Funzione non eseguibile (ad oggi)
029900070321if  1c                   if            (£6OGgeot <> 'S'
030000070321     c                             or   £6OGdda  >  wDateA)
030100070326     c***                          and  %subst(KNMUS:1:3) <> 'EDP'
030200070321     c                   eval      *in06   =  *on
030300070321     c                   seton                                        28  90
030400070321     c                   eval      V1Dmsg  =  $Msg( 1)
030500070321     c                   leavesr
030600070321e   1c                   endif
030700070321      *
030800070321      * Codice giro
030900070321     c                   clear                   V1Dcgi
031000070321      * - ricerca
031100070321if  1c                   if        %scan('?' : V1Ccgi) > *zeros
031200070321     c                   clear                   V1Ccgi
031300070321     c                   reset                   FIDG09ds
031400070321     c*** già così:      eval      D09iop0 =  'P01'
031500070321     c***                eval      D09ifgs =  V1Cfgs
031600070321     c                   eval      D09ifgs =  D55tfa
031700070321     c                   eval      D09idat =  wDate
031800070321     c                   movel(p)  FIDG09ds      KPJBU
031900070321     c                   call      'FIDG09R'
032000070321     c                   parm                    KPJBA
032100070321     c                   movel     KPJBU         FIDG09ds
032200070321     c                   movel     D09ocgi       V1Ccgi
032300070321     c                   seton                                          5190
032400070321e   1c                   endif
032500070321      * - controlli:
032600070321      * - - Codice giro obbligatorio
032700070321if  1c                   if             V1Ccgi  =  *blanks
032800070321     c                             or   V1Ccgi  =  *zeros
032900070321     c                   seton                                        285190
033000070321     c                   eval      V1Dmsg  =  $Msg( 2)
033100070321     c                   leavesr
033200070321e   1c                   endif
033300070321      * - - Codice giro errato
033400070321     c                   eval      GRGfgs  =  D55tfa
033500070321     c                   eval      GRGcgi  =  V1Ccgi
033600070321     c     K02GRG03      chain     FIGRG000
033700070321if  1c                   if        NOT  %found(FIGRG03L)
033800070321     c                             or   GRGatb <> *blanks
033900070321     c                   seton                                        285190
034000070321     c                   eval      V1Dmsg  =  $Msg( 3)
034100070321     c                   leavesr
034200070321e   1c                   endif
034300070321     c                   movel     GRGdes        V1Dcgi
034400070321      *
034500070321      * Data di stampa (per RIstampa)
034600070321     c                   clear                   W1Casl
034700070321if  1c                   IF        V1Casl  >  *zeros
034800070321     c                   clear                   WLBdat
034900070321     c                   eval      G08dat  =  V1Casl
035000070321     c                   call      'XSRDA8'
035100070321     c                   parm                    WLBdat
035200070321if  2c                   if        G08err  =  *off
035300070321     c                   eval      V1Casl  =  G08dat
035400070321     c                   eval      W1Casl  =  G08inv
035500070321x   2c                   else
035600070321     c                   seton                                        285290
035700070402     c                   eval      V1Dmsg  =  $Msg( 4)
035800070321     c                   leavesr
035900070321e   2c                   endif
036000070321e   1c                   ENDIF
036100070321      *
036200070321     c                   ENDSR
036300070321      *
036400070321      *---------------------------------------------------------------*
036500070321      *?Sottomissione lavoro batch                                   ?*
036600070321      *---------------------------------------------------------------*
036700070321     c     sr_SbmJob     BEGSR
036800070321      *
036900070321     c                   eval      D33fgs  =  V1Cfgs
037000070321     c                   eval      D33cgi  =  V1Ccgi
037100070321     c                   eval      D33asl  =  W1Casl
037200070321      * Parametri per override ai printer-file:
037300070321     c                   eval      D33psl  =  D90psl
037400070321     c                   eval      D33mdb  =  D90mdb
037500070321     c                   eval      D33mdb4 =  D90mdb4
037600070321     c                   eval      D33mdb5 =  D90mdb5
037700070321     c                   eval      D33prb  =  D90prb
037800070321     c                   eval      D33prb4 =  D90prb4
037900070321     c                   eval      D33prb5 =  D90prb5
038000070321      *
038100070321     c                   movel(p)  FIDG33ds      KPJBU
038200070321     c                   movel     'DG33'        KCOAZ
038300070321      *
038400070321     c                   if        KNMUS   =  *all'1'
038500070321     c                   call      'FIDG33C'
038600070321     c                   parm                    KPJBA
038700070321     c                   else
038800070321     c                   call      'BCH10'
038900070321     c                   parm                    KPJBA
039000070321     c                   endif
039100070321      *
039200070321     c                   ENDSR
039300070321
039400070321** - $Msg -------------------------------------------------------------------*
039500070321FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
039600070321Codice giro obbligatorio                                                        2
039700070321Codice giro errato                                                              3
039800070402Data di stampa formalmente errata                                               4
