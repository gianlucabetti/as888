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
004100070321      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
004200070321     d FNLV55ds      e ds                  inz
004300070321     d  D55tpt       e                     inz('6')
004400070321      *
004500070321      * - Parametri per richiesta stampante per BOLLE ed ETICHETTE
004600070321     d TRUL90ds      e ds                  inz
004700070321     d***D90rse      e                     inz('S')
004800070321     d   D90rsb      e                     inz('L')
004900070321      *
005000070321      * - Parametri per FNLV50R: Controllo Filiale in Gestione
005100070321     d FNLV50ds      e ds                  inz
005200070321      *
005300070321      * - Parametri per FIDG09R: Interrogazione Giri
005400070321     d FIDG09ds      e ds                  inz
005500070321     d  D09iop0      e                     inz('P01')
005600070321      *
005700070321     d Status         sds
005800070321     d  SDSpgm           *proc
005900070321     d  SDSjbnm              244    253
006000070321     d  SDSjbus              254    263
006100070321     d  SDSjbnr              264    269s 0
006200070321      *
006300070321      * - Parametri per pgm. controllo data
006400070321     d WLBdat          ds                  inz
006500070321     d  G08dat                        8  0 inz
006600070321     d  G08inv                        8  0 inz
006700070321     d  G08err                        1    inz('3')
006800070321     d  G08tgi                        5  0 inz
006900070321      *
007000070321      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007100070321      *
007200070321      * - Flags
007300070321     d $Fine           s              1    inz(*off)
007400070321     d $InzD01         s              1    inz(*on)
007500070321      *
007600070321      * - Variabili di comodo
007700070321     d wDateA          s              8    inz
007800070321     d wDate           s              8  0 inz
007900070321     d W1Casl          s              8  0 inz
008000070510     ddtaiso           s               d
008100070510     ddtaeur           s               d   datfmt(*eur)
008200070321      *
008300070321      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008400070321      *
008500070321      * - FIGRG03L
008600070321     c     K02GRG03      klist
008700070321     c                   kfld                    GRGfgs
008800070321     c                   kfld                    GRGcgi
008900070321
009000070321      *---------------------------------------------------------------*
009100070402      *?RIEPILOGO INDICATORI                                         ?*
009200070321      *---------------------------------------------------------------*
009300070321      * 18   - Disabilitazione tasto funzionale F18  e                *
009400070321      *        Sprotezione campo "Filiale Gestione"                   *
009500070321      * 25   - Comodo
009600070321      * 28   - Emissione messaggio di errore                          *
009700070321      * 50   - Filiale Gestione errata                                *
009800070321      * 51   - Codice Giro errato                                     *
009900070321      * 52   - Data da ristampare errata                              *
010000070321      * 90   - Riemissione videata                                    *
010100070321      *---------------------------------------------------------------*
010200070321
010300070321     c     *Entry        plist
010400070321     c                   parm                    KPJBA
010500070321      *
010600070321      * Operazioni iniziali
010700070321     c                   exsr      sr_Start
010800070321      *
010900070321      * Ciclo di gestione della videata di lancio
011000070321do  1c                   dow       $Fine   =  *off
011100070321     c                   exsr      GesD01
011200070321e   1c                   enddo
011300070321      *
011400070321      * Fine
011500070321     c                   eval      *inLR   =  *on
011600070321      *
011700070321      *---------------------------------------------------------------*
011800070321      *?Operazioni Iniziali                                          ?*
011900070321      *---------------------------------------------------------------*
012000070321     c     sr_Start      BEGSR
012100070510     c*
012200070510     C                   TIME                    WHHUDT           14 0
012300070510     C                   MOVE      WHHUDT        UDATE8            8 0
012400070510     c                   move      udate8        dtaeur
012500070510     c                   move      dtaeur        dtaiso
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
015400070510     c*****              eval      G08dat  =  *date
015500070510     c                   move      dtaiso        G08dat
015600070321     c                   call      'XSRDA8'
015700070321     c                   parm                    WLBdat
015800070321     c                   eval      wDate   =  G08inv
015900070321     c                   move      wDate         wDateA
016000070321      *
016100070321     c                   ENDSR
016200070321      *
016300070321      *---------------------------------------------------------------*
016400070322      *?Gestione videata D01                                         ?*
016500070321      *---------------------------------------------------------------*
016600070321     c     GesD01        BEGSR
016700070321      *
016800070321if  1c                   if        $InzD01 =  *on
016900070321     c                   exsr      InzD01
017000070321     c                   eval      $InzD01 =  *off
017100070321e   1c                   endif
017200070321      *
017300070321      * Emissione testata e piede
017400070321     c                   write     DG32T01
017500070321     c                   write     DG32Z01
017600070321      *
017700070321      * Emissione videata di dettaglio
017800070321     c                   exfmt     DG32D01
017900070321     c                   clear                   V1Dmsg
018000070321     c                   setoff                                       28  90
018100070321      *
018200070321sel 1c                   SELECT
018300070321      * - F3 = Fine
018400070321w   1c                   WHEN      *inKC
018500070321     c                   eval      $Fine   =  *on
018600070321      * - F18 = Modifica Filiale in Gestione
018700070321w   1c                   WHEN      *inKS
018800070321     c                   eval      *in18   =  *on
018900070321     c                   eval      *in50   =  *on
019000070321      * - F9 = Personalizzazione Batch
019100070321w   1c***                WHEN      *inKI
019200070321     c***                eval      Kcoaz   =  'LRS9'
019300070321     c***                call      'BCH09'
019400070321     c***                parm                    KPJBA
019500070321      * - Enter/F6: Controllo video D01
019600070321x   1c                   OTHER
019700070321     c                   exsr      CtrD01
019800070321      * - F6 = Lancio batch (se abilitato e se NO errori)
019900070321if  2c                   if                *inKF
020000070321     c                             and NOT *in90
020100070321     c                   exsr      sr_SbmJob
020200070321     c                   eval      $Fine   =  *on
020300070321e   2c                   endif
020400070321e   1c                   ENDSL
020500070321      *
020600070321     c                   ENDSR
020700070321      *
020800070321      *---------------------------------------------------------------*
020900070321      *?Inizializzazione videata D01                                 ?*
021000070321      *---------------------------------------------------------------*
021100070321     c     InzD01        BEGSR
021200070321      *
021300070321     c                   clear                   DG32D01
021400070321      *
021500070321     c                   if        DUTlpo  =  '2'
021600070321     c                   eval      V1Cfgs  =  DUTpou
021700070321     c                   else
021800070321     c                   eval      V1Cfgs  =  DUTtfp
021900070321     c                   endif
022000070321     c***                clear                   V1Ccgi
022100070321     c***                clear                   V1Casl
022200070321      *
022300070321      * Decodifiche
022400070321     c                   exsr      CtrD01
022500070321     c                   movea     *zeros        *in(28)
022600070321      *
022700070321     c                   ENDSR
022800070321      *
022900070321      *---------------------------------------------------------------*
023000070321      *?Controlli videata D01                                        ?*
023100070321      *---------------------------------------------------------------*
023200070321     c     CtrD01        BEGSR
023300070321      *
023400070321     c                   movea     *zeros        *in(50)
023500070321      *
023600070321      * Controllo filiale gestione
023700070321     c                   clear                   V1Dfgs
023800070321if  1c                   if        V1Cfgs  <> D50fgs
023900070321     c                   clear                   FNLV50ds
024000070321     c                   eval      D50pru  =  KNMUS
024100070321     c                   eval      D50fgs  =  V1Cfgs
024200070321     c                   call      'FNLV50R'
024300070321     c                   parm                    FNLV50ds
024400070321e   1c                   endif
024500070321if  1c                   if        D50err  <> *blanks
024600070321     c                   eval      D50fgs  =  *loval
024700070321     c                   seton                                        285090
024800070321     c                   movel     D50msg        V1Dmsg
024900070321     c                   leavesr
025000070321e   1c                   endif
025100070322      * Decodifica filiale gestione
025200070321if  1c                   if        V1Cfgs  <> ORGfil
025300070321     c     V1Cfgs        chain     AZORG
025400070321if  2c                   if        NOT  %found(AZORG01L)
025500070321     c                             or   ORGfva <> *blanks
025600070321     c                   clear                   ORGdes
025700070321e   2c                   endif
025800070321e   1c                   endif
025900070321     c                   movel     ORGdes        V1Dfgs
026000070322      *
026100070322      * - Reperimento capofila (£6)
026200070322      *   (da reperire SEMPRE E COMUNQUE perchè serve la capofila £6)
026300070416if  1c                   if        V1Cfgs  <> D55lin
026400070322     c                   reset                   FNLV55ds
026500070322     c*** già così:      eval      D55tpt  =  '6'
026600070322     c                   eval      D55lin  =  V1Cfgs
026700070322     c                   eval      D55drf  =  wDate
026800070322     c                   call      'FNLV55R'
026900070322     c                   parm                    FNLV55ds
027000070322if  2c                   if        D55err  <> *blanks
027100070322     c                   eval      D55tfa  =  V1Cfgs
027200070322e   2c                   endif
027300070416e   1c                   endif
027400070321      *
027500070321      * Codice giro
027600070321     c                   clear                   V1Dcgi
027700070321      * - ricerca
027800070321if  1c                   if        %scan('?' : V1Ccgi) > *zeros
027900070321     c                   clear                   V1Ccgi
028000070321     c                   reset                   FIDG09ds
028100070321     c*** già così:      eval      D09iop0 =  'P01'
028200070321     c***                eval      D09ifgs =  V1Cfgs
028300070321     c                   eval      D09ifgs =  D55tfa
028400070321     c                   eval      D09idat =  wDate
028500070321     c                   movel(p)  FIDG09ds      KPJBU
028600070321     c                   call      'FIDG09R'
028700070321     c                   parm                    KPJBA
028800070321     c                   movel     KPJBU         FIDG09ds
028900070321     c                   movel     D09ocgi       V1Ccgi
029000070321     c                   seton                                          5190
029100070321e   1c                   endif
029200070321      * - controlli:
029300070321      * - - Codice giro obbligatorio
029400070321if  1c                   if             V1Ccgi  =  *blanks
029500070321     c                             or   V1Ccgi  =  *zeros
029600070321     c                   seton                                        285190
029700070321     c                   eval      V1Dmsg  =  $Msg( 2)
029800070321     c                   leavesr
029900070321e   1c                   endif
030000070321      * - - Codice giro errato
030100070321     c                   eval      GRGfgs  =  D55tfa
030200070321     c                   eval      GRGcgi  =  V1Ccgi
030300070321     c     K02GRG03      chain     FIGRG000
030400070321if  1c                   if        NOT  %found(FIGRG03L)
030500070321     c                             or   GRGatb <> *blanks
030600070321     c                   seton                                        285190
030700070321     c                   eval      V1Dmsg  =  $Msg( 3)
030800070321     c                   leavesr
030900070321e   1c                   endif
031000070321     c                   movel     GRGdes        V1Dcgi
031100070321      *
031200070321      * Data di stampa (per RIstampa)
031300070321     c                   clear                   W1Casl
031400070321if  1c                   IF        V1Casl  >  *zeros
031500070321     c                   clear                   WLBdat
031600070321     c                   eval      G08dat  =  V1Casl
031700070321     c                   call      'XSRDA8'
031800070321     c                   parm                    WLBdat
031900070321if  2c                   if        G08err  =  *off
032000070321     c                   eval      V1Casl  =  G08dat
032100070321     c                   eval      W1Casl  =  G08inv
032200070321x   2c                   else
032300070321     c                   seton                                        285290
032400070402     c                   eval      V1Dmsg  =  $Msg( 4)
032500070321     c                   leavesr
032600070321e   2c                   endif
032700070321e   1c                   ENDIF
032800070321      *
032900070321     c                   ENDSR
033000070321      *
033100070321      *---------------------------------------------------------------*
033200070321      *?Sottomissione lavoro batch                                   ?*
033300070321      *---------------------------------------------------------------*
033400070321     c     sr_SbmJob     BEGSR
033500070321      *
033600070321     c                   eval      D33fgs  =  V1Cfgs
033700070321     c                   eval      D33cgi  =  V1Ccgi
033800070321     c                   eval      D33asl  =  W1Casl
033900070321      * Parametri per override ai printer-file:
034000070321     c                   eval      D33psl  =  D90psl
034100070321     c                   eval      D33mdb  =  D90mdb
034200070321     c                   eval      D33mdb4 =  D90mdb4
034300070321     c                   eval      D33mdb5 =  D90mdb5
034400070321     c                   eval      D33prb  =  D90prb
034500070321     c                   eval      D33prb4 =  D90prb4
034600070321     c                   eval      D33prb5 =  D90prb5
034700070321      *
034800070321     c                   movel(p)  FIDG33ds      KPJBU
034900070321     c                   movel     'DG33'        KCOAZ
035000070321      *
035100070321     c                   if        KNMUS   =  *all'1'
035200070321     c                   call      'FIDG33C'
035300070321     c                   parm                    KPJBA
035400070321     c                   else
035500070321     c                   call      'BCH10'
035600070321     c                   parm                    KPJBA
035700070321     c                   endif
035800070321      *
035900070321     c                   ENDSR
036000070321
036100070321** - $Msg -------------------------------------------------------------------*
036200070321FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
036300070321Codice giro obbligatorio                                                        2
036400070321Codice giro errato                                                              3
036500070402Data di stampa formalmente errata                                               4
