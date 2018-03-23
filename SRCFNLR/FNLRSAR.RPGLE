000100070329      * FNLRSAR *-----------------------------------------------------*
000200070329      *?STAMPA LDV DI BACKUP - FILTRO DI LANCIO                      ?*
000300070329      *---------------------------------------------------------------*
000400070329
000500070830     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070329
000700070329      *---------------------------------------------------------------*
000800070329
000900070329     fAZORG01L  if   e           k disk
001000070329      *
001100070329     fFNARB54L  if   e           k disk
001200070329      *
001300070329     fFNLRSAD   cf   e             workstn
001400070329
001500070329      *---------------------------------------------------------------*
001600070329
001700070329      *
001800070329      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001900070329      *
002000070329      *
002100070329      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002200070329      *
002300070329      * - Messaggi di errore
002400071210     d $Msg            s             78    dim( 7) ctdata perrcd(1)
002500070329      *
002600070329      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002700070329      *
002800070329      * - Parametri
002900070329     d KPJBA         e ds
003000070329      *
003100070329      * - Parametri per FNLRSBR: Stampa LdV di backup (batch)
003200070329     d FNLRSBds      e ds                  inz
003300070329      *
003400070329      * - Parametri x Controllo profilo utenti
003500070329     d TIBS34ds      e ds
003600070329      * - Ds di riferimento al file esterno AZUTE00F
003700070329     d AZUTEds       e ds                       extname(AZUTE00F)
003800070329      * - Ds per dati organigramma
003900070329     d dDatiUte      e ds
004000070329      *
004100070329     d OG146         e ds                  inz
004200070830     d OG147         e ds                  inz
004300070329     d OG148         e ds                  inz
004400070329      *
004500070329      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
004600070329     d FNLV55ds      e ds                  inz
004700070329     d  D55tpt       e                     inz('6')
004800070329      *
004900070329      * - Parametri per richiesta stampante per BOLLE ed ETICHETTE
005000070329     d TRUL90ds      e ds                  inz
005100070329     d***D90rse      e                     inz('S')
005200070329     d   D90rsb      e                     inz('L')
005300070329      *
005400070329      * - Parametri per FNLV50R: Controllo Filiale in Gestione
005500070329     d FNLV50ds      e ds                  inz
005600070329      *
005700070329     d Status         sds
005800070329     d  SDSpgm           *proc
005900070329     d  SDSjbnm              244    253
006000070329     d  SDSjbus              254    263
006100070329     d  SDSjbnr              264    269s 0
006200070329      *
006300070329      * - Parametri per pgm. controllo data
006400070329     d WLBdat          ds                  inz
006500070329     d  G08dat                        8  0 inz
006600070329     d  G08inv                        8  0 inz
006700070329     d  G08err                        1    inz('3')
006800070329     d  G08tgi                        5  0 inz
006900070329      *
007000070329      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007100070329      *
007200070329      * - Flags
007300070329     d $Fine           s              1    inz(*off)
007400070329     d $InzD01         s              1    inz(*on)
007500070329      *
007600070329      * - Variabili di comodo
007700070830     d W1Cdcm          s              8  0 inz
007800071210     d W1Cdcrp         s              8  0 inz
007900070830     d W1Cdla          s              8  0 inz
008000070329     d wDate           s              8  0 inz
008100070329     d wDateA          s              8    inz
008200070329      *
008300070329      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008400070329      *
008500070329     c     K02ARB54      klist
008600070329     c                   kfld                    ARBlna
008700070329     c                   kfld                    ARBdcm
008800070329
008900070329      *---------------------------------------------------------------*
009000070329      *? RIEPILOGO INDICATORI                                        ?*
009100070329      *---------------------------------------------------------------*
009200070329      * 01   - Disabilitazione tasto funzionale F1   e                *
009300070329      *        Sprotezione campo "Data di stampa".                    *
009400070329      * 06   - Disabilitazione tasto funzionale F6                    *
009500070329      *        (Filiale non abilitata)                                *
009600070329      * 18   - Disabilitazione tasto funzionale F18  e                *
009700070329      *        Sprotezione campo "Filiale Gestione"                   *
009800070329      * 28   - Emissione messaggio di errore                          *
009900070830      * 41   - Visualizzazione e gestione "Data e ora limite abbinam."*
010000070329      * 50   - Filiale Gestione errata                                *
010100070329      * 51   - Data Distinta errata                                   *
010200070830      * 52   - Data Limite Abbinamento errara                         *
010300070830      * 53   - Ora  Limite Abbinamento errara                         *
010400071210      * 54   - Data limite consegna richiesta "PRIMA DEL" errata      *
010500070329      * 90   - Riemissione videata                                    *
010600070329      *---------------------------------------------------------------*
010700070329
010800070329     c     *Entry        plist
010900070329     c                   parm                    KPJBA
011000070329      *
011100070329      * Operazioni iniziali
011200070329     c                   exsr      sr_Start
011300070329      *
011400070329      * Ciclo di gestione della videata di lancio
011500070329do  1c                   dow       $Fine   =  *off
011600070329     c                   exsr      GesD01
011700070329e   1c                   enddo
011800070329      *
011900070329      * Fine
012000070329     c                   eval      *inLR   =  *on
012100070329
012200070329      *---------------------------------------------------------------*
012300070329      *?Operazioni Iniziali                                          ?*
012400070329      *---------------------------------------------------------------*
012500070329     c     sr_Start      BEGSR
012600070329      *
012700070329      * Reperimento dati utente
012800070329     c     *dtaara       define    §azute        AZUTEds
012900070329     c     *dtaara       define    §datiute      dDATIUTE
013000070329     c                   in(E)     *dtaara
013100070329     c                   if        %ERROR or RSUT = *blanks
013200070329     c                   clear                   TIBS34ds
013300070329     c                   call      'TIBS34R'
013400070329     c                   parm                    TIBS34ds
013500070329     c                   in        *dtaara
013600070329     c                   endif
013700070329      *
013800070329      * Richiesta stampanti
013900070329     c                   reset                   TRUL90ds
014000070329     c                   call      'TRUL90R'
014100070329     c                   parm                    KPJBA
014200070329     c                   parm                    TRUL90ds
014300070329      *
014400070329     c                   if        D90F3   =  *on
014500070329     c                   eval      $Fine   =  *on
014600070329     c                   leavesr
014700070329     c                   endif
014800070329      *
014900070329      * Impostazione iniziale dei dati fissi in testata
015000070329     c                   eval      V1Cpgm  =  SDSpgm
015100070329      *
015200070329      * Impostazione della *DATE in formato alfanumerico
015300070329     c                   clear                   WLBdat
015400070329     c                   eval      G08dat  =  *date
015500070329     c                   call      'XSRDA8'
015600070329     c                   parm                    WLBdat
015700070830     c                   eval      wDate   =  G08inv
015800070830     c                   move      wDate         wDateA
015900070329      *
016000070329     c                   ENDSR
016100070329      *
016200070329      *---------------------------------------------------------------*
016300070329      *?Gestione videata D01                                         ?*
016400070329      *---------------------------------------------------------------*
016500070329     c     GesD01        BEGSR
016600070329      *
016700070329if  1c                   if        $InzD01 =  *on
016800070329     c                   exsr      InzD01
016900070329     c                   eval      $InzD01 =  *off
017000070329e   1c                   endif
017100070329      *
017200070329      * Emissione testata e piede
017300070329     c                   write     LRSAT01
017400070329     c                   write     LRSAZ01
017500070329      *
017600070329      * Emissione videata di dettaglio
017700070329     c                   exfmt     LRSAD01
017800070329     c                   clear                   V1Dmsg
017900070329     c                   setoff                                       28  90
018000070329      *
018100070329sel 1c                   SELECT
018200070329      * - F1 = Modifica Data Distinta
018300070830w   2c                   WHEN      *inKA
018400070830     c                   eval      *in01   =  *on
018500070830     c                   eval      *in51   =  *on
018600070329      * - F3 = Fine
018700070329w   1c                   WHEN      *inKC
018800070329     c                   eval      $Fine   =  *on
018900070329      * - F18 = Modifica Filiale in Gestione
019000070329w   1c                   WHEN      *inKS
019100070329     c                   eval      *in18   =  *on
019200070329     c                   eval      *in50   =  *on
019300070329      * - F9 = Personalizzazione Batch
019400070329w   1c***                WHEN      *inKI
019500070329     c***                eval      KCOAZ   =  'LRSB'
019600070329     c***                call      'BCH09'
019700070329     c***                parm                    KPJBA
019800070329      * - Enter/F6: Controllo video D01
019900070329x   1c                   OTHER
020000070329     c                   exsr      CtrD01
020100070329      * - F6 = Lancio batch (se abilitato e se NO errori)
020200070329if  2c                   if                *inKF
020300070329     c                             and NOT *in06
020400070329     c                             and NOT *in90
020500070329     c                   exsr      sr_SbmJob
020600070329     c                   eval      $Fine   =  *on
020700070329e   2c                   endif
020800070329e   1c                   ENDSL
020900070329      *
021000070329     c                   ENDSR
021100070329      *
021200070329      *---------------------------------------------------------------*
021300070329      *?Inizializzazione videata D01                                 ?*
021400070329      *---------------------------------------------------------------*
021500070329     c     InzD01        BEGSR
021600070329      *
021700070329     c                   clear                   LRSAD01
021800070329      *
021900070329     c                   if        DUTlpo  =  '2'
022000070329     c                   eval      V1Cfgs  =  DUTpou
022100070329     c                   else
022200070329     c                   eval      V1Cfgs  =  DUTtfp
022300070329     c                   endif
022400070329     c                   eval      V1Cdcm  =  *date
022500070329      *
022600070329      * Decodifiche
022700070329     c                   exsr      CtrD01
022800070329if  1c                   if        NOT  *in06
022900070329     c                   movea     *zeros        *in(28)
023000070329e   1c                   endif
023100070830      *
023200070830      * Impostazione indicatori
023300070830     c                   eval      *in41   =  (§OGdoada = 'S')
023400070329      *
023500070329     c                   ENDSR
023600070329      *
023700070329      *---------------------------------------------------------------*
023800070329      *?Controlli videata D01                                        ?*
023900070329      *---------------------------------------------------------------*
024000070329     c     CtrD01        BEGSR
024100070329      *
024200070329     c                   movea     *zeros        *in(50)
024300070329      *
024400070329      *  Controllo filiale gestione
024500070329     c                   clear                   V1Dfgs
024600070329if  1c                   if        V1Cfgs  <> D50fgs
024700070329     c                   clear                   FNLV50ds
024800070329     c                   eval      D50pru  =  KNMUS
024900070329     c                   eval      D50fgs  =  V1Cfgs
025000070329     c                   call      'FNLV50R'
025100070329     c                   parm                    FNLV50ds
025200070329e   1c                   endif
025300070329if  1c                   if        D50err  <> *blanks
025400070329     c                   seton                                        285090
025500070329     c                   movel     D50msg        V1Dmsg
025600070329     c                   leavesr
025700070329e   1c                   endif
025800070329      * Decodifica filiale gestione
025900070830      * e reperimento 146ª, 147ª e 148ª descrizioni dell'organigramma
026000070329if  1c                   if        V1Cfgs  <> ORGfil
026100070329     c     V1Cfgs        chain     AZORG
026200070329if  2c                   if        NOT  %found(AZORG01L)
026300070329     c                             or   ORGfva <> *blanks
026400070329     c                   clear                   ORGdes
026500071008     c                   clear                   ORGde6
026600071008     c                   clear                   ORGde7
026700071008     c                   clear                   ORGde8
026800070329e   2c                   endif
026900070329e   1c                   endif
027000070329     c                   movel     ORGdes        V1Dfgs
027100070329     c                   movel     ORGde6        OG146
027200070830     c                   movel     ORGde7        OG147
027300070329     c                   movel     ORGde8        OG148
027400070424if  2c                   if        §OGdda  <= *zeros
027500070424     c                   eval      §OGdda  =  *hival
027600070329e   2c                   endif
027700070329      *
027800070329      * Funzione non eseguibile (ad oggi)
027900070419     c                   eval      *in06   =  *off
028000070424if  1c                   if        §OGdda  >  wDateA
028100070329     c                   eval      *in06   =  *on
028200070329     c                   seton                                        28  90
028300070329     c                   eval      V1Dmsg  =  $Msg( 1)
028400070329     c                   leavesr
028500070329e   1c                   endif
028600070329      *
028700070329      * Data di stampa
028800070329if  1c                   if        V1Cdcm  =  *zeros
028900070329     c                   seton                                        285190
029000070329     c                   eval      V1Dmsg  =  $Msg( 2)
029100070329     c                   leavesr
029200070329e   1c                   endif
029300070329     c                   clear                   WLBdat
029400070329     c                   eval      G08dat  =  V1Cdcm
029500070329     c                   call      'XSRDA8'
029600070329     c                   parm                    WLBdat
029700070329if  1c                   if        G08err  =  *on
029800070329     c                   seton                                        285190
029900070329     c                   eval      V1Dmsg  =  $Msg( 3)
030000070329     c                   leavesr
030100070329e   1c                   endif
030200070329     c                   eval      V1Cdcm  =  G08dat
030300070830     c                   eval      W1Cdcm  =  G08inv
030400070329      *
030500070329     c                   eval      ARBlna  =  V1Cfgs
030600070830     c                   eval      ARBdcm  =  *zeros
030700070329     c     K02ARB54      setll     FNARB000
030800070329if  1c                   if        NOT %equal(FNARB54L)
030900070329     c                   seton                                        285290
031000070329     c                   eval      V1Dmsg  =  $Msg( 4)
031100070329     c                   leavesr
031200070329e   1c                   endif
031300071210      *
031400071210      * Data limite consegna richiesta "PRIMA DEL"
031500071210     c                   clear                   W1Cdcrp
031600071210     c                   clear                   WLBdat
031700071210if  1c                   IF        V1Cdcrp <> *zeros
031800071210     c                   clear                   WLBdat
031900071210     c                   eval      G08dat  =  V1Cdcrp
032000071210     c                   call      'XSRDA8'
032100071210     c                   parm                    WLBdat
032200071210if  2c                   if        G08err  =  *on
032300071210     c                   seton                                        285490
032400071210     c                   eval      V1Dmsg  =  $Msg( 3)
032500071210     c                   leavesr
032600071210e   2c                   endif
032700071210     c                   eval      V1Cdcrp =  G08dat
032800071210     c                   eval      W1Cdcrp =  G08inv
032900071210      * - la data limite consegna richiesta non può essere < della
033000071210      *   data di stampa
033100071210if  2c                   if        W1Cdcrp <  W1Cdcm
033200071210     c                   seton                                        285490
033300071210     c                   eval      V1Dmsg  =  $Msg( 6)
033400071210     c                   leavesr
033500071210e   2c                   endif
033600071210e   1c                   ENDIF
033700070830      *
033800070830      * Data limite abbinamento
033900071210     c                   clear                   W1Cdla
034000071210     c                   clear                   WLBdat
034100070830if  1c                   IF        V1Cdla  <> *zeros
034200070830     c                   eval      G08dat  =  V1Cdla
034300070830     c                   call      'XSRDA8'
034400070830     c                   parm                    WLBdat
034500070830if  2c                   if        G08err  =  *on
034600070830     c                   seton                                        285290
034700070830     c                   eval      V1Dmsg  =  $Msg( 3)
034800070830     c                   leavesr
034900070830e   2c                   endif
035000070830     c                   eval      V1Cdla  =  G08dat
035100070830     c                   eval      W1Cdla  =  G08inv
035200070830      * - la data limite abbinamento non può essere > della data di
035300070830      *   stampa
035400070830if  2c                   if        W1Cdla  >  W1Cdcm
035500070830     c                   seton                                        285290
035600071210     c                   eval      V1Dmsg  =  $Msg( 7)
035700070830     c                   leavesr
035800070830e   2c                   endif
035900070830      * - la data limite abbinamento non può essere > di oggi
036000071210if  2c                   if        W1Cdla  >  wDate
036100070830     c                   seton                                        285290
036200071210     c                   eval      V1Dmsg  =  $Msg( 7)
036300070830     c                   leavesr
036400070830e   2c                   endif
036500070830e   1c                   ENDIF
036600070830      *
036700070830      * Ora limite abbinamento
036800070830if  1c                   if            (V1Cdla  = *zeros
036900070830     c                             and  V1Cola <> *zeros)
037000070830     c                             OR  (V1Cdla <> *zeros
037100070830     c                             and (V1Cola <= *zeros
037200070830     c                              or  V1Cola  > 2400))
037300070830     c                   seton                                        285390
037400070830     c                   eval      V1Dmsg  =  $Msg( 5)
037500070830     c                   leavesr
037600070830e   1c                   endif
037700070329      *
037800070329     c                   ENDSR
037900070329      *
038000070329      *---------------------------------------------------------------*
038100070329      *?Sottomissione lavoro batch                                   ?*
038200070329      *---------------------------------------------------------------*
038300070329     c     sr_SbmJob     BEGSR
038400070329      *
038500070830     c                   clear                   FNLRSBds
038600070830      *
038700070329     c                   eval      DSBfgs  =  V1Cfgs
038800070831     c                   eval      DSBdcm  =  W1Cdcm
038900071210     c                   eval      DSBdcrp =  W1Cdcrp
039000070830if  1c                   if        V1Cdla  <> *zeros
039100070831     c                   eval      DSBdla  =  W1Cdla
039200070830     c                   eval      DSBola  =  V1Cola
039300070830e   1c                   endif
039400070329      * Parametri per override ai printer-file:
039500070329     c                   eval      DSBpsl  =  D90psl
039600070329     c                   eval      DSBmdb  =  D90mdb
039700070329     c                   eval      DSBmdb4 =  D90mdb4
039800070329     c                   eval      DSBmdb5 =  D90mdb5
039900070329     c                   eval      DSBprb  =  D90prb
040000070329     c                   eval      DSBprb4 =  D90prb4
040100070329     c                   eval      DSBprb5 =  D90prb5
040200070329      *
040300070329     c                   movel(p)  FNLRSBds      KPJBU
040400070329     c                   movel     'LRSB'        KCOAZ
040500070329      *
040600070329     c                   if        KNMUS   =  *all'1'
040700070329     c                   call      'FNLRSBC'
040800070329     c                   parm                    KPJBA
040900070329     c                   else
041000070329     c                   call      'BCH10'
041100070329     c                   parm                    KPJBA
041200070329     c                   endif
041300070329      *
041400070329     c                   ENDSR
041500070329
041600070329** - $Msg -------------------------------------------------------------------*
041700070329FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
041800070329Data di stampa obbligatoria                                                     2
041900071210Data formalmente errata                                                         3
042000070329Non trovate spedizioni da stampare                                              4
042100070830Ora errata                                                                      5
042200071210La data NON può essere precedente a quella di stampa                            6
042300071210La data NON può essere successiva né a quella di stampa né a quella odierna     7
