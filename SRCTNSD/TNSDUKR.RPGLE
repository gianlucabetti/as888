000100070416      *PARMS DBGVIEW(*SOURCE)
000200120507     /*PRM dbgview(*source)
000300120507     /*END
000400070416      *===============================================================*
000500070417      *?TNSDUKR - Conferma inserimento multiplo unità EDP            ?*
000600070416      *===============================================================*
000700070416
000800070416     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900070416
001000070416      *---------------------------------------------------------------*
001100070416
001200070509     fWFAIM01L  if   e           k disk
001300070418      *
001400070509     fUNANA02L  if   e           k disk    rename(UNANA000:UNANA002)
001500070423     fUNANA08L  if   e           k disk    rename(UNANA000:UNANA008)
001600070416     fAZORG01L  if   e           k disk
001700070416     fAZORG21L  if   e           k disk
001800070416      *
001900070416     fTNSDUKD   cf   e             workstn
002000070416     f                                     sfile(SDUKS01:S01nrr)
002100070416
002200070416      *---------------------------------------------------------------*
002300070416
002400070416      *
002500070416      *?Costanti  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002600070416      *
002700070423      * - Tasti di funzione
002800070423     d C_RollUp        c                   const(x'F5')
002900070423      * - Nr. di righe del sfl (SFLPAG)
003000070423     d C_SflPag        c                   const(16)
003100120507      *
003200120508     d c_MA            C                   const('ABCDEFGHIJKLMNOPQRS-
003300120507     d                                     TUVWYXZ')
003400120508     d c_MI            C                   const('abcdefghijklmnopqrs-
003500120507     d                                     tuvwyxz')
003600070416      *
003700070416      *?Schiere - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003800070416      *
003900070416      * - Messaggi di errore
004000120507     d $Msg            s             78    dim(23)   ctdata  perrcd(1)
004100070418      * - Inserimenti Multipli Unità bloccati per la conferma
004200070509     d $InMuUn         s                   dim(999)  like(WIMnum)  inz
004300070509     d $RecIMU         s                   dim(999)  like(WIMprg)  inz
004400070416      *
004500070416      *?Ds  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
004600070416      *
004700070416      * - Parametri
004800070416     d KPJBA         e ds
004900070417     d TNSDUKds      e ds                  inz
005000070416      *
005100070416      * - Parametri x Controllo profilo utenti
005200070423     d TIBS34ds      e ds                  inz
005300070416      * - Ds di riferimento al file esterno AZUTE00F
005400070423     d AZUTEds       e ds                  extname(AZUTE00F)
005500070416      * - Ds per dati organigramma
005600070503     d dDatiUte      e ds
005700070416      *
005800130916      * - DS campo WIMUNI file WFAIM00F trk "1" prg 01
005900070509     d dWIMuni1      e ds                  inz
006000130916      * - DS campo WIMUNI file WFAIM00F trk "1" prg 02
006100130916     d dWIMuni12     e ds                  inz
006200070423      *
006300070423      * - Ds per decodifica cliente
006400070423     d TIBS69ds      e ds                  inz
006500070423     d ds_CNACO      e ds                  inz       extname(CNACO00F)
006600070423     d ds_CNIND      e ds                  inz       extname(CNIND00F)
006700070423     d ds_CNCLP      e ds                  inz       extname(CNCLP00F)
006800070423     d ds_FNCLS      e ds                  inz       extname(FNCLS00F)
006900070423      *
007000070423      * - Ds per controllo/interrogazione tabelle
007100070423     d TIBS02ds      e ds                  inz
007200070423      * - Tabella "UNT" = Tipi Unità
007300070423     d dUNT          e ds                  inz
007400070423      *
007500070503      * - Parametri per pgm. TNSDU2R = Gestione unità EDP (inserimento)
007600070423     d dsUNIT        e ds                  inz
007700070423     d  DSUpro       e                     inz('A')
007800070423     d  DSUop0       e                     inz(' 2')
007900070423     d  DSUop1       e                     inz('F06')
008000070423     d UNANAds       e ds                  inz       extname(UNANA00F)
008100070423     d                                               prefix(DSA:3)
008200070502     d UNMOVds         ds                  inz
008300070509     d  DSMdoc                             inz       like(V2Cdoc)
008400070509     d  DSMddo                             inz       like(V2Cddo)
008500070417      *
008600070417      * - Parametri per pgm. TNSDUJR/R1/R2
008700070417     d TNSDUJds      e ds                  inz
008800070423      *
008900070423      * - Parametri per pgm. XSRDA8
009000070423     d WLBdat          ds                  inz
009100070423     d  G08dat                        8  0 inz
009200070423     d  G08inv                        8  0 inz
009300070423     d  G08err                        1    inz('3')
009400070423     d  G08tgi                        5  0 inz
009500070416      *
009600070416     d Status         sds           333
009700070416     d   SDSpgm          *proc
009800070416      *
009900070416      *?Variabili - - - - - - - - - - - - - - - - - - - - - - - - - -?*
010000070416      *
010100070416      * - Flags
010200070416     d $EoF            s              1    inz(*off)
010300070416     d $Fine           s              1    inz(*off)
010400070417     d $InzD01         s              1    inz(*on)
010500070416     d $InzD02         s              1    inz(*on)
010600070509     d $InzW01         s              1    inz(*on)
010700120507     d $InzW02         s              1    inz(*on)
010800070509     d $InzS01         s              1    inz(*on)
010900070416     d $Found          s              1    inz(*off)
011000070416     d $Err            s              1    inz(*off)
011100070416      *
011200120507      * - Variabili per la gestione del video
011300070424     d $Video          s              2    inz('D1')
011400070509     d wVideo          s                   inz  like($Video)
011500070423      *
011600120507      * - Indici di schiera
011700070424     d xx              s              5  0 inz
011800070420      *
011900070420      * - Variabili riferite al data base o al display file
012000070509     d S01nrr          s                   inz  like(C01rcd)
012100070416      *
012200070418      * - Variabili per il controllo delle "rotture di livello"
012300070509     d SAVnum          s                   inz  like(WIMnum)
012400120508     d*// SAVprg          s                   inz  like(WIMprg)
012500070416      *
012600070416      * - Campi di comodo
012700120508     d SAVprg1         s                   inz  like(WIMprg)
012800070503     d W2Cdcn          s                   inz  like(UNAdcn)
012900070503     d W2Cdsg          s                   inz  like(UNAdsg)
013000070503     d W2Cdsm          s                   inz  like(UNAdsm)
013100070503     d W2Cdsl          s                   inz  like(UNAdsl)
013200070503     d W2Cddo          s                   inz  like(V2Cddo)
013300070423     d Wtip            s                   inz  like(V2Ctip)
013400070423     d Wrg2            s                   inz  like(V2Crg2)
013500070423     d Wksc            s                   inz  like(V2Cksc)
013600070423     d WkscD           s                   inz  like(V2Dksc)
013700070423     d Wmag            s                   inz  like(V2Cmag)
013800070417     d WmagD           s             30    inz
013900070417     d Wdata           s              8  0 inz
014000070423      *
014100070423     d wPag            s              4  0 inz
014200070423     d wRig            s              3  0 inz
014300070423     d W_SflPag        s              3  0 inz
014400070416      *
014500070416      *?Key-List  - - - - - - - - - - - - - - - - - - - - - - - - - -?*
014600070416      *
014700070509      * - UNANA02L
014800070509     c     K02UNA02      klist
014900070509     c                   kfld                    W1Ctip
015000070509     c                   kfld                    W1Cmat
015100070416      * - UNANA08L
015200070423     c     K02UNA08      klist
015300070416     c                   kfld                    UNAmod
015400070416     c                   kfld                    UNAmat
015500070509      * - WFAIM01L
015600070509     c     K02WIM01      klist
015700070509     c                   kfld                    WIMnum
015800070509     c                   kfld                    WIMtrk
015900070416
016000070416      *---------------------------------------------------------------*
016100070416      *?RIEPILOGO INDICATORI                                         ?*
016200070416      *---------------------------------------------------------------*
016300070416      * 25    - Comodo                                                *
016400070416      * 28    - Emissione del messaggio di errore a video             *
016500070416      * 30    - Pulizia      del subfile S01 (sflclr, NO sfldsp)      *
016600070416      * 31    - NO emissione del subfile S01 (sflclr, NO sfldspctl)   *
016700070416      * 32    - Record modificato nel subfile S01 (sflnxtchg)         *
016800070416      * 33    - Fine dati nel subfile S01         (sflend)            *
016900070503      * 40    - S01: NON visualizza i campi nel record del subfile    *
017000130923      * 41    - D02: Condiziona DSPATR(PR) nel campo Num.Ordine       *
017100130923      * 42    - D02: Condiziona DSPATR(PR) nel campo Num./Data Docum. *
017200070416      * 50    - S01: Posizionamento cursore sul campo "Opzione"       *
017300070509      * 50    - D01: Numero Inserimento Multiplo Unità errato         *
017400070423      * 60    - D02: Tipo unità    errato                             *
017500070423      * 61    - D02: Codice unità  errato                             *
017600070423      * 62    - D02: Marca unità   errata                             *
017700070423      * 63    - D02: Modello unità errato                             *
017800070423      * 65    - D02: 2° liv. raggrupp. errato                         *
017900070423      * 66    - D02: Data consegna              errata                *
018000070423      * 67    - D02: Data scadenza manutenzione errata                *
018100070423      * 68    - D02: Data scadenza garanzia     errata                *
018200070423      * 69    - D02: Data scadenza leasing      errata                *
018300070423      * 70    - D02: Ditta proprietà         errata                   *
018400070423      * 71    - D02: Ditta manutenzione sede errata                   *
018500070423      * 72    - D02: Ditta manutenzione fil. errata                   *
018600070423      * 73    - D02: Ditta leasing           errata                   *
018700070423      * 74    - D02: Codice magazzino ubicazione errato               *
018800070423      * 75    - D02: Numero documento errato                          *
018900070423      * 76    - D02: Data   documento errata                          *
019000070423      * 77    - D02: Codice cliente errato                            *
019100120412      * 78    - D02: numero ordine obbligatorio                       *
019200070417      * 90    - Generico di errore                                    *
019300070416      *---------------------------------------------------------------*
019400070416
019500070416     c     *Entry        plist
019600070416     c                   parm                    KPJBA
019700070416      *
019800070416      * Operazioni Iniziali
019900070416     c                   exsr      RoutInz
020000070416      *
020100070416      * Gestione Video
020200070416do  1c                   DOW       $Fine     = *off
020300070417      * - Videata x impostazione parametri
020400070417cas 2c     $Video        caseq     'D1'          GesD01
020500070416      * - Videata x impostazione dati "standard" di immissione
020600070417cas 2c     $Video        caseq     'D2'          GesD02
020700070509      * - Videata x selezione    dati "standard" di immissione
020800070509cas 2c     $Video        caseq     'W1'          GesW01
020900070509      * - Videata x selezione    dati da immettere in WFAIM00F
021000070417cas 2c     $Video        caseq     'S1'          GesS01
021100070416e   2c                   endcs
021200070416e   1c                   ENDDO
021300070416      *
021400070416      * Fine
021500070424     c                   movel(p)  TNSDUKds      KPJBU
021600070416     c                   movel     *on           *inLR
021700070416      *
021800070416      *---------------------------------------------------------------*
021900070416      *?Operazioni Iniziali                                          ?*
022000070416      *---------------------------------------------------------------*
022100070416     c     RoutInz       BEGSR
022200070416      *
022300070416      * Reperimento dati job
022400070416     c                   exsr      DatiJob
022500070416      *
022600070416     c                   movel     SDSpgm        V1Tpgm
022700070416      *
022800070416      * Impostazione iniziale dei parametri i/o
022900070417     c                   clear                   TNSDUKds
023000070417if  1c                   if        KPJBU    <> *blanks
023100070417     c                   movel     KPJBU         TNSDUKds
023200070417e   1c                   endif
023300070417     c                   eval      DUKuni    = *blanks
023400070417     c                   eval      DUKret    = *off
023500070417     c                   eval      DUKerr    = *off
023600070417     c                   eval      DUKmsg    = *blanks
023700070416      *
023800070509      * Reperimento dati del trk "1" del file WFAIM01L
023900070418      *   (SE ricevuti parametri)
024000070509     c                   clear                   dWIMuni1
024100130916     c                   clear                   dWIMuni12
024200070417if  1c                   if        DUKnum    > *zeros
024300070417     c*** già così:      eval      $Video    = 'D1'
024400070417     c                   exsr      InzD01
024500070417     c                   eval      $InzD01   = *off
024600070417     c                   exsr      CtrD01
024700070503if  2c                   if        *in90     = *off
024800070417     c                   eval      $Video    = 'D2'
024900070417     c*** già così:      eval      $InzD02   = *on
025000070417x   2c                   else
025100070417     c                   eval      $Video    = 'D1'
025200070417     c*** già così:      eval      $InzD01   = *off
025300070417e   2c                   endif
025400070417e   1c                   endif
025500070416      *
025600070416     c                   ENDSR
025700070416      *
025800070416      *---------------------------------------------------------------*
025900070416      *?Reperimento Dati del job (Utente/Operativi)                  ?*
026000070416      *---------------------------------------------------------------*
026100070416     c     DatiJob       BEGSR
026200070416      *
026300070416     c     *dtaara       define    §azute        azuteds
026400070416     c     *dtaara       define    §datiute      ddatiute
026500070416      *
026600070416     c                   in(E)     *dtaara
026700070416     c                   IF        %ERROR or RSUT = *blanks
026800070416     c                   clear                   Tibs34Ds
026900070416     c                   call      'TIBS34R'
027000070416     c                   parm                    Tibs34Ds
027100070416     c                   in        *dtaara
027200070416     c                   ENDIF
027300070416      *
027400070416     c                   ENDSR
027500070416      *
027600070416      *---------------------------------------------------------------*
027700070416      *?Gestione videata D01                                         ?*
027800070416      *---------------------------------------------------------------*
027900070416     c     GesD01        BEGSR
028000070416      *
028100070416      * Inizializzazione della videata
028200070416if  1c                   if        $InzD01   = *on
028300070416     c                   exsr      InzD01
028400070416     c                   eval      $InzD01   = *off
028500070416e   1c                   endif
028600070416      *
028700070416      * Scrittura di testata e riga tasti funzionali abilitati
028800070416     c                   write     SDUKT01
028900070416     c                   write     SDUKZ01
029000070416      *
029100070416      * Emissione della videata
029200070416     c                   exfmt     SDUKD01
029300070416     c                   setoff                                       28  90
029400070416     c                   clear                   V1Dmsg
029500070416      *
029600070416sel 1c                   SELECT
029700070416      * F3=Fine
029800070416w   1c                   WHEN      *inKC
029900070416     c                   exsr      F03D01
030000070417      * F4=Ricerca Inserimento Multiplo Unità
030100070417w   1c                   WHEN      *inKD
030200070417     c                   exsr      F04D01
030300070418     *** * F12=Ritorno
030400070418w   1***c                   WHEN      *inKL
030500070418     ***c                   exsr      F12D01
030600070417      * Invio
030700070416x   1c                   OTHER
030800070416      * - Controllo dati immessi a video
030900070416     c                   exsr      CtrD01
031000070417if  2c                   if        NOT *in90
031100070416     c                   exsr      F06D01
031200070416e   2c                   endif
031300070416e   1c                   ENDSL
031400070416      *
031500070416     c                   ENDSR
031600070416      *
031700070416      *---------------------------------------------------------------*
031800070416      *?Inizializzazione videata D01                                 ?*
031900070416      *---------------------------------------------------------------*
032000070416     c     InzD01        BEGSR
032100070416      *
032200070416     c                   clear                   SDUKD01
032300070417      *
032400070417      * Impostazione campi
032500070417if  1c                   if        DUKnum    > *zeros
032600070417     c                   eval      V1Cnum    = DUKnum
032700070417e   1c                   endif
032800070416      *
032900070416     c                   ENDSR
033000070417      *
033100070417      *---------------------------------------------------------------*
033200070417      *?Controllo dati immessi in videata D01                        ?*
033300070417      *---------------------------------------------------------------*
033400070417     c     CtrD01        BEGSR
033500070417      *
033600070417     c                   movea     *zeros        *in(50)
033700070509     c*** già fatto:     clear                   dWIMuni1
033800130916     c*** già fatto:     clear                   dWIMuni12
033900070417      *
034000070509      * Gestione rec. trk "1" del file WFAIM00F per
034100070424      * · Controllo "disponibilità" dell'inserimento multiplo unità
034200070424      * · "Blocco" dell'inserimento multiplo unità x conferma
034300070509if  1c                   IF        V1Cnum    = *zeros
034400070509     c                   seton                                        285090
034500070509     c                   eval      V1Dmsg    = $Msg( 1)
034600070509     c                   leavesr
034700070509x   1c                   ELSE
034800070417     c                   clear                   TNSDUJds
034900070418     c                   eval      DUJopz    = '9'
035000070417     c                   eval      DUJnum    = V1Cnum
035100070417     c                   exsr      Call_TNSDUJ1
035200070417if  2c                   if             DUJerr <> *off
035300070417     c                             or   DUJret <> *off
035400070417     c                   leavesr
035500070417e   2c                   endif
035600130919     c                   movel     DUJuni1       dWIMuni1
035700130919     c                   movel     DUJuni2       dWIMuni12
035800070502      * · Serve SOLO "temporaneamente" (nel caso venga premuto F12
035900070502      * dalla videata "D2"), perchè poi i dati delle schiere $InMuUn
036000070502      * e $RecIMU vengono POI correttamente impostati durante il ca-
036100070502      * ricamento dei dati nel subfile S01.
036200070502     c                   eval      $InMuUn( 1) = V1Cnum
036300070502     c                   eval      $RecIMU( 1) = 1
036400070417e   1c                   ENDIF
036500070417      *
036600070417     c                   ENDSR
036700070416      *
036800070416      *---------------------------------------------------------------*
036900070416      *?Gestione tasto funzionale F03 da videata D01                 ?*
037000070416      *---------------------------------------------------------------*
037100070416     c     F03D01        BEGSR
037200070416      *
037300070416      * Chiusura del programma
037400070417     c                   eval      DUKret    = '1'
037500070416     c                   eval      $Fine     = *on
037600070416      *
037700070416     c                   ENDSR
037800070417      *
037900070417      *---------------------------------------------------------------*
038000070417      *?Gestione tasto funzionale F04 da videata D01                 ?*
038100070417      *---------------------------------------------------------------*
038200070417     c     F04D01        BEGSR
038300070417      *
038400070417     c                   clear                   TNSDUJds
038500070417     c                   eval      DUJopz    = '1'
038600070417     c                   movel(p)  TNSDUJds      KPJBU
038700070417     c                   call      'TNSDUJR'
038800070417     c                   parm                    KPJBA
038900070417     c                   movel     KPJBU         TNSDUJds
039000070417      *
039100070417sel 1c                   SELECT
039200070417      * Ritorno normale
039300070417w   1c                   WHEN           DUJret  = *off
039400070417     c                             and  DUJerr  = *off
039500130919     c                   movel     DUJuni1       dWIMuni1
039600130919     c                   movel     DUJuni2       dWIMuni12
039700070417     c                   eval      V1Cnum    = DUJnum
039800070417      * Ritorno con errore
039900070417w   1c                   WHEN      DUJerr    = *on
040000070417     c                   seton                                        28  90
040100070417     c                   eval      V1Dmsg    = DUJmsg
040200070417      * Ritorno con F3
040300070417w   1c                   WHEN      DUJret    = '1'
040400070417     c                   eval      $Fine     = *on
040500070417     c                   eval      *in90     = *on
040600070417      * Ritorno con F12
040700070417w   1c                   WHEN      DUJret    = '2'
040800070417     c                   eval      *in90     = *on
040900070417      *
041000070417e   1c                   ENDSL
041100070417      *
041200070417     c                   ENDSR
041300070416      *
041400070416      *---------------------------------------------------------------*
041500070416      *?Gestione tasto funzionale F06 da videata D01                 ?*
041600070416      *---------------------------------------------------------------*
041700070416     c     F06D01        BEGSR
041800070416      *
041900070503      * Passaggio alla videata D02
042000070503     c                   eval      wVideo    = $Video
042100070417     c                   eval      $Video    = 'D2'
042200070503     c                   eval      $InzD02   = *on
042300070416      *
042400070416     c                   ENDSR
042500070418     *** *
042600070418     *** *---------------------------------------------------------------*
042700070418     *** *?Gestione tasto funzionale F12 da videata D01                 ?*
042800070418     *** *---------------------------------------------------------------*
042900070418     ***c     F12D01        BEGSR
043000070418     *** *
043100070418     *** * Chiusura del programma
043200070418     ***c                   eval      DUKret    = '2'
043300070418     ***c                   eval      $Fine     = *on
043400070418     *** *
043500070418     ***c                   ENDSR
043600070417      *
043700070417      *---------------------------------------------------------------*
043800070417      *?Gestione videata D02                                         ?*
043900070417      *---------------------------------------------------------------*
044000070417     c     GesD02        BEGSR
044100070417      *
044200070417      * Inizializzazione della videata
044300070417if  1c                   if        $InzD02   = *on
044400070417     c                   exsr      InzD02
044500070417     c                   eval      $InzD02   = *off
044600070417e   1c                   endif
044700130923      *
044800130923      * Impostazione attributi di visualizzazione
044900130923     c                   exsr      sr_AttrVisD02
045000070417      *
045100070417      * Scrittura di testata e riga tasti funzionali abilitati
045200070417     c                   write     SDUKT01
045300070417     c                   write     SDUKZ02
045400070417      *
045500070417      * Emissione della videata
045600070417     c                   exfmt     SDUKD02
045700070417     c                   setoff                                       28  90
045800070417     c                   clear                   V1Dmsg
045900070417      *
046000070417sel 1c                   SELECT
046100070417      * F3=Fine
046200070417w   1c                   WHEN      *inKC
046300070424     c                   exsr      F03S01
046400070509      * F8=Selezione matricola da "copiare"
046500070509w   1c                   WHEN      *inKH
046600070509     c                   eval      $Video    = 'W1'
046700070417      * F12=Ritorno
046800070417w   1c                   WHEN      *inKL
046900070417     c                   exsr      F12D02
047000070417      * Invio / F6=Conferma
047100070417x   1c                   OTHER
047200070417      * - Controllo dati immessi a video
047300070417     c                   exsr      CtrD02
047400070417      * - F6=Conferma
047500070417if  2c                   if        *inKF  and  NOT *in90
047600070417     c                   exsr      F06D02
047700070417e   2c                   endif
047800070417e   1c                   ENDSL
047900070417      *
048000070417     c                   ENDSR
048100070417      *
048200070417      *---------------------------------------------------------------*
048300070417      *?Inizializzazione videata D02                                 ?*
048400070417      *---------------------------------------------------------------*
048500070417     c     InzD02        BEGSR
048600070417      *
048700070509     c                   clear                   SDUKD02
048800070503      *
048900070509      * Testata
049000070509     c                   eval      V2Dnum    = V1Cnum
049100130916     c                   eval      V2Dde1    = §WIM1des + §WIM12des
049200130916     c                   eval      V2Cord    = §WIM12ord
049300130916     c                   eval      V2Cdoc    = §WIM12doc
049400130916     c                   eval      V2Cddo    = §WIM12ddo
049500070509      *
049600070509      * Dettaglio
049700070509if  1c                   if             W1Ctip =  *zeros
049800070509     c                             or   W1Cmat =  *blanks
049900070509     c                   leavesr
050000070509e   1c                   endif
050100070509      *
050200070509     c     K02UNA02      chain     UNANA002
050300070509if  1c                   if        NOT  %found(UNANA02L)
050400070509     c                             or   UNAatb <> *blanks
050500070509     c                   leavesr
050600070509e   1c                   endif
050700070509      *
050800070509     c                   move      UNAtip        V2Ctip
050900070509     c*** NO in inserim! eval      V2Ccod    = UNAcod
051000070509     c                   eval      V2Cmar    = UNAmar
051100070509     c                   eval      V2Cmod    = UNAmod
051200070509     c*** NO in inserim! eval      V2Cmat    = UNAmat
051300070509     c                   eval      V2Cdes    = UNAdes
051400070509     c                   eval      V2Cnot    = UNAnot
051500070509     c                   reset                   WLBdat
051600070509     c                   eval      G08inv    = UNAdcn
051700070509     c                   call      'XSRDA8'
051800070509     c                   parm                    WLBdat
051900070509     c                   eval      V2Cdcn    = G08dat
052000070509     c                   reset                   WLBdat
052100070509     c                   eval      G08inv    = UNAdsg
052200070509     c                   call      'XSRDA8'
052300070509     c                   parm                    WLBdat
052400070509     c                   eval      V2Cdsg    = G08dat
052500070509     c                   eval      V2Csis    = UNAsis
052600070509     c                   move      UNAdms        V2Cdms
052700070509     c                   move      UNAdmf        V2Cdmf
052800070509     c                   reset                   WLBdat
052900070509     c                   eval      G08inv    = UNAdsm
053000070509     c                   call      'XSRDA8'
053100070509     c                   parm                    WLBdat
053200070509     c                   eval      V2Cdsm    = G08dat
053300070509     c                   eval      V2Clea    = UNAlea
053400070509     c                   eval      DSAdsl    = W2Cdsl
053500070509     c                   reset                   WLBdat
053600070509     c                   eval      G08inv    = UNAdsl
053700070509     c                   call      'XSRDA8'
053800070509     c                   parm                    WLBdat
053900070509     c                   eval      V2Cdsl    = G08dat
054000070509     c                   move      UNAmag        V2Cmag
054100120412     c                   eval      V2Cemu    = %subst(UNAemu:7:9)
054200070509     c                   eval      V2Cmvd    = UNAmvd
054300150511     c                   reset                   WLBdat
054400150511     c                   IF        %subst(UNAMDR:8:8) <> *blank
054500150511     c                   eval      G08inv = %dec(%subst(UNAMDR:8:8):8:0)
054600150511     C                   ENDIF
054700150511     c                   call      'XSRDA8'
054800150511     c                   parm                    WLBdat
054900150511     c                   eval      V2CMDR2   = G08dat
055000070509     c                   eval      V2Cmta    = UNAmta
055100070509     c                   eval      V2Cmdf    = UNAmdf
055200070509     c                   eval      V2Cmra    = UNAmra
055300150511     c                   eval      V2Cmdr    = %subst(UNAmdr:1:7)
055400070509     c                   eval      V2Cx25    = UNAx25
055500070509     c                   eval      V2Clsw    = UNAlsw
055600070509     c                   eval      V2Cksc    = UNAksc
055700070509     c                   move      UNArg2        V2Crg2
055800070417      *
055900070417     c                   ENDSR
056000130923      *
056100130923      *---------------------------------------------------------------*
056200130923      *?Impostazione attributi di vislualizzazione videata D02       ?*
056300130923      *---------------------------------------------------------------*
056400130923     c     sr_AttrVisD02 BEGSR
056500130923      *
056600130923      * -?Protezione "Numero Ordine"?
056700130923     c                   eval      *in41 = *off
056800130923     c*//                eval      *in41 = (§WIM12ord <> *zero  and
056900130923     c*//                                   Not *in78)
057000130923      * -?Protezione "Numero/Data Documento"?
057100130923     c                   eval      *in42 = *off
057200130923     c*//                eval      *in42 = (§WIM12doc <> *zero  and
057300130923     c*//                                   §WIM12ddo <> *zero  and
057400130923     c*//                                   Not *in76 and Not *in77)
057500130923      *
057600130923     c                   ENDSR
057700070417      *
057800070417      *---------------------------------------------------------------*
057900070417      *?Controllo dati immessi in videata D02                        ?*
058000070417      *---------------------------------------------------------------*
058100070417     c     CtrD02        BEGSR
058200070417      *
058300070417     c                   movea     *zeros        *in(50)
058400070417      *
058500070417      * Tipo unità errato
058600070417     c                   clear                   V2Dtip
058700070516if  1c*****              IF        V2Ctip   <> *blanks
058800070417     c                   eval      Wtip      = V2Ctip
058900070417     c                   exsr      Chk_Tip
059000070502sel 2c                   select
059100070502w   2c                   when      $Err      = *off
059200070418     c                   movel     T02ke1        V2Ctip
059300070417     c                   movel     d§UNTdes      V2Dtip
059400070418w   3c                   when      $Err      = *on
059500070502     c                   seton                                        2860
059600070509     c                   eval      V1Dmsg    = $Msg( 2)
059700070418w   3c                   when      $Err      = '2'
059800070502     c                   seton                                        2860
059900070509     c                   eval      V1Dmsg    = $Msg( 3)
060000070502e   2c                   endsl
060100070418if  2c                   if        *in90
060200070418     c                   leavesr
060300070418e   2c                   endif
060400070516e   1c*****              ENDIF
060500120412     c
060600130916      * Numero ordine obbligatorio
060700130916if  1c                   if        V2Cord   <= *zeros
060800120412     c                   eval      V1Dmsg    = $Msg(20)
060900120412     c                   seton                                        287890
061000120412     c                   leavesr
061100130916e   1c                   endif
061200070417      *
061300070417sel 1c***                Select
061400070417      * Marca obbligatoria
061500070417w   1c***                When      V2Cmar    = *blanks
061600070509     c***                eval      V1Dmsg    = $Msg( 4)
061700070417     c***                seton                                        286290
061800070417     c***                leavesr
061900070417      * Modello obbligatorio
062000070417w   1c***                When      V2Cmod    = *blanks
062100070509     c***                eval      V1Dmsg    = $Msg( 5)
062200070417     c***                seton                                        286390
062300070417     c***                leavesr
062400070417      * Matricola obbligatoria
062500070417w   1c*****              When      V2Cmat    = *blanks
062600070509     c*****              eval      V1Dmsg    = $Msg( 6)
062700070417     c*****              seton                                        286490
062800070417     c*****              leavesr
062900070417e   1c***                EndSl
063000070417      *
063100070417      * Codice con stesso modello e matricola già inserito
063200070502if  1c                   IF             V2Cmod <> *blanks
063300070417     c                             and  V2Cmat <> *blanks
063400070417     c                   eval      UNAmod    = V2Cmod
063500070417     c                   eval      UNAmat    = V2Cmat
063600070423     c     K02UNA08      setll     UNANA008
063700070417if  2c                   if        %equal(UNANA08L)
063800070509     c                   eval      V1Dmsg    = $Msg( 7)
063900070417     c                   seton                                        286390
064000070417     c                   leavesr
064100070417e   2c                   endif
064200070502e   1c                   ENDIF
064300070417      *
064400070417      * 2° livello di raggruppamento mancante o errato
064500070417     c                   clear                   V2Drg2
064600070417if  1c***                if             V2Crg2  = *blanks
064700070417     c***                          or   V2Crg2  = *zeros
064800070509     c***                eval      V1Dmsg    = $Msg( 8)
064900070417     c***                seton                                        286590
065000070417     c***                leavesr
065100070417e   1c***                endif
065200070417      ****
065300070502if  1c                   IF             V2Crg2 <> *blanks
065400070417     c                             and  V2Crg2 <> *zeros
065500070417     c                   eval      Wrg2      = V2Crg2
065600070417     c                   exsr      Chk_Rg2
065700070417if  2c                   if        $Err      = *off
065800070418     c                   eval      V2Crg2    = Wrg2
065900070417     c                   movel     T02uni        V2Drg2
066000070417x   2c                   else
066100070509     c                   eval      V1Dmsg    = $Msg( 8)
066200070417     c                   seton                                        286590
066300070417e   2c                   endif
066400070418if  2c                   if        *in90
066500070418     c                   leavesr
066600070418e   2c                   endif
066700070502e   1c                   ENDIF
066800070417      *
066900070417      * Data consegna mancante o errata
067000070423     c                   clear                   W2Cdcn
067100070417if  1c***                if        V2Cdcn    = *zeros
067200070509     c***                eval      V1Dmsg    = $Msg( 9)
067300070417     c***                seton                                        286690
067400070417     c***                leavesr
067500070417e   1c***                endif
067600070417      ****
067700070417if  1c                   IF        V2Cdcn   <> *zeros
067800070417     c                   eval      Wdata     = V2Cdcn
067900070417     c                   exsr      Chk_Dat
068000070417if  2c                   if        $Err      = *on
068100070509     c                   eval      V1Dmsg    = $Msg( 9)
068200070417     c                   seton                                        286690
068300070417     c                   leavesr
068400070417e   2c                   endif
068500070423     c                   eval      V2Cdcn    = Wdata
068600070423     c                   eval      W2Cdcn    = G08inv
068700070417e   1c                   ENDIF
068800070417      *
068900070417      * Data scadenza manutenzione mancante o errata
069000070423     c                   clear                   W2Cdsm
069100070417if  1c***                if        V2Cdsm    = *zeros
069200070509     c***                eval      V1Dmsg    = $Msg( 9)
069300070417     c***                seton                                        286790
069400070417     c***                leavesr
069500070417e   1c***                endif
069600070417      ****
069700070417if  1c                   IF        V2Cdsm   <> *zeros
069800070417     c                   eval      Wdata     = V2Cdsm
069900070417     c                   exsr      Chk_Dat
070000070417if  2c                   if        $Err      = *on
070100070509     c                   eval      V1Dmsg    = $Msg( 9)
070200070417     c                   seton                                        286790
070300070417     c                   leavesr
070400070417e   2c                   endif
070500070423     c                   eval      V2Cdsm    = Wdata
070600070423     c                   eval      W2Cdsm    = G08inv
070700070417e   1c                   ENDIF
070800070417      *
070900070417      * Data scadenza garanzia mancante o errata
071000070423     c                   clear                   W2Cdsg
071100070417if  1c***                if        V2Cdsg    = *zeros
071200070509     c***                eval      V1Dmsg    = $Msg( 9)
071300070417     c***                seton                                        286890
071400070417     c***                leavesr
071500070417e   1c***                endif
071600070417      ****
071700070417if  1c                   IF        V2Cdsg   <> *zeros
071800070417     c                   eval      Wdata     = V2Cdsg
071900070417     c                   exsr      Chk_Dat
072000070417if  2c                   if        $Err      = *on
072100070509     c                   eval      V1Dmsg    = $Msg( 9)
072200070417     c                   seton                                        286890
072300070417     c                   leavesr
072400070417e   2c                   endif
072500070423     c                   eval      V2Cdsg    = Wdata
072600070423     c                   eval      W2Cdsg    = G08inv
072700070417e   1c                   ENDIF
072800070417      *
072900070417      * Data scadenza leasing errata
073000070417     c                   clear                   W2Cdsl
073100070417if  1c                   IF        V2Cdsl   <> *zeros
073200070417     c                   eval      Wdata     = V2Cdsl
073300070417     c                   exsr      Chk_Dat
073400070417if  2c                   if        $Err      = *on
073500070509     c                   eval      V1Dmsg    = $Msg( 9)
073600070417     c                   seton                                        286990
073700070417     c                   leavesr
073800070417e   2c                   endif
073900070423     c                   eval      V2Cdsl   = Wdata
074000070423     c                   eval      W2Cdsl   = G08inv
074100070417e   1c                   ENDIF
074200070417      *
074300070417      * Ditta proprietà mancante o errata
074400070417     c                   clear                   V2Dsis
074500070417if  1c***                if        V2Csis    = *blanks
074600070509     c***                eval      V1Dmsg    = $Msg(10)
074700070417     c***                seton                                        287090
074800070417     c***                leavesr
074900070417e   1c***                endif
075000070417      ****
075100070417if  1c                   IF        V2Csis   <> *blanks
075200070417     c                   eval      Wmag      = V2Csis
075300070417     c                   exsr      Chk_Mag
075400070502if  2c                   if        $Err      = *off
075500070502     c                   eval      V2Csis    = Wmag
075600070502     c                   movel     WmagD         V2Dsis
075700070502x   2c                   else
075800070509     c                   eval      V1Dmsg    = $Msg(10)
075900070417     c                   seton                                        287090
076000070417e   2c                   endif
076100070418if  2c                   if        *in90
076200070418     c                   leavesr
076300070418e   2c                   endif
076400070417e   1c                   ENDIF
076500070417      *
076600070417      * Ditta leasing errata
076700070417     c                   clear                   V2Dlea
076800070417if  1c                   IF        V2Clea   <> *blanks
076900070417     c                   eval      Wmag      = V2Clea
077000070417     c                   exsr      Chk_Mag
077100070502if  2c                   if        $Err      = *off
077200070502     c                   eval      V2Clea    = Wmag
077300070502     c                   movel     WmagD         V2Dlea
077400070502x   2c                   else
077500070509     c                   eval      V1Dmsg    = $Msg(10)
077600070417     c                   seton                                        287190
077700070417e   2c                   endif
077800070418if  2c                   if        *in90
077900070418     c                   leavesr
078000070418e   2c                   endif
078100070417e   1c                   ENDIF
078200070417      *
078300070417      * Ditta manutenzione sede mancante o errata
078400070417     c                   clear                   V2Ddms
078500070417if  1c***                if        V2Cdms    = *blanks
078600070509     c***                eval      V1Dmsg    = $Msg(11)
078700070417     c***                seton                                        287290
078800070417     c***                leavesr
078900070417e   1c***                endif
079000070417      ****
079100070417if  1c                   IF        V2Cdms   <> *blanks
079200070417     c                   eval      Wmag      = V2Cdms
079300070417     c                   exsr      Chk_Mag
079400070502if  2c                   if        $Err      = *off
079500070502     c                   eval      V2Cdms    = Wmag
079600070502     c                   movel     WmagD         V2Ddms
079700070502x   2c                   else
079800070509     c                   eval      V1Dmsg    = $Msg(11)
079900070417     c                   seton                                        287290
080000070417e   2c                   endif
080100070418if  2c                   if        *in90
080200070418     c                   leavesr
080300070418e   2c                   endif
080400070417e   1c                   ENDIF
080500070417      *
080600070417      * Ditta manutenzione filiale mancante o errata
080700070417     c                   clear                   V2Ddmf
080800070417if  1c***                if        V2Cdmf    = *blanks
080900070509     c***                eval      V1Dmsg    = $Msg(11)
081000070417     c***                seton                                        287390
081100070417     c***                leavesr
081200070417e   1c***                endif
081300070417      ****
081400070417if  1c                   IF        V2Cdmf   <> *blanks
081500070417     c                   eval      Wmag      = V2Cdmf
081600070417     c                   exsr      Chk_Mag
081700070502if  2c                   if        $Err      = *off
081800070502     c                   eval      V2Cdmf    = Wmag
081900070502     c                   movel     WmagD         V2Ddmf
082000070502x   2c                   else
082100070509     c                   eval      V1Dmsg    = $Msg(11)
082200070417     c                   seton                                        287390
082300070502e   2c                   endif
082400070418if  2c                   if        *in90
082500070418     c                   leavesr
082600070418e   2c                   endif
082700070417e   1c                   ENDIF
082800070417      *
082900070417      * Magazzino ubicazione mancante o errato
083000070417     c                   clear                   V2Dmag
083100070417if  1c***                if        V2Cmag    = *blanks
083200070509     c***                eval      V1Dmsg    = $Msg(12)
083300070417     c***                seton                                        287490
083400070417     c***                leavesr
083500070417e   1c***                endif
083600070417      ****
083700070417if  1c                   IF        V2Cmag   <> *blanks
083800070417     c                   eval      Wmag      = V2Cmag
083900070417     c                   exsr      Chk_Mag
084000070502if  2c                   if        $Err      = *off
084100070502     c                   eval      V2Cmag    = Wmag
084200070502     c                   movel     WmagD         V2Dmag
084300070502x   2c                   else
084400070509     c                   eval      V1Dmsg    = $Msg(12)
084500070417     c                   seton                                        287490
084600070417e   2c                   endif
084700070418if  2c                   if        *in90
084800070418     c                   leavesr
084900070418e   2c                   endif
085000070417e   1c                   ENDIF
085100070417      *
085200070417      * Codice cliente mancante o errato
085300070417     c                   clear                   V2Dksc
085400070417      * - eventuale ricerca x ragione sociale
085500070423if  1c                   IF             V2Cksc <> *zeros
085600070417     c                             or   V2Dksc <> *blanks
085700070417     c                   eval      Wksc      = V2Cksc
085800070417     c                   eval      WkscD     = V2Dksc
085900070417     c                   exsr      Chk_Ksc
086000070502if  2c                   if        $Err      = *off
086100070502     c                   eval      V2Cksc    = Wksc
086200070502     c                   eval      V2Dksc    = WkscD
086300070502x   2c                   else
086400070509     c                   eval      V1Dmsg    = $Msg(13)
086500070417     c                   seton                                        287590
086600070417e   2c                   endif
086700070418if  2c                   if        *in90
086800070418     c                   leavesr
086900070418e   2c                   endif
087000070417e   1c                   ENDIF
087100070417      *
087200070417if  1c                   if             V2Cksc  = *zeros
087300070417     c                             and  V2Cmag  = '951'
087400070509     c                   eval      V1Dmsg    = $Msg(13)
087500070417     c                   seton                                        287590
087600070417     c                   leavesr
087700070417e   1c                   endif
087800070417      *
087900070417      * Numero documento mancante
088000130923if  1c                   if        V2Cdoc    = *zeros
088100130923     c                   eval      V1Dmsg    = $Msg(14)
088200130923     c                   seton                                        287690
088300130923     c                   leavesr
088400130923e   1c                   endif
088500070417      *
088600070417      * Data documento mancante o errata
088700070423     c                   clear                   W2Cddo
088800130923if  1c                   if        V2Cddo    = *zeros
088900130923     c                   eval      V1Dmsg    = $Msg(15)
089000130923     c                   seton                                        287790
089100130923     c                   leavesr
089200130923e   1c                   endif
089300130923      *
089400070502if  1c                   IF        V2Cddo   <> *zeros
089500070417     c                   eval      Wdata     = V2Cddo
089600070417     c                   exsr      Chk_Dat
089700070417if  2c                   if        $Err      = *on
089800100903     c                   eval      V1Dmsg    = $Msg(15)
089900070417     c                   seton                                        287790
090000070417     c                   leavesr
090100070417e   2c                   endif
090200070423     c                   eval      V2Cddo    = Wdata
090300070423     c                   eval      W2Cddo    = G08inv
090400070417e   1c                   ENDIF
090500070417      *
090600070417     c                   ENDSR
090700070417      *
090800070417      *---------------------------------------------------------------*
090900070417      *?Gestione tasto funzionale F06 da videata D02                 ?*
091000070417      *---------------------------------------------------------------*
091100070417     c     F06D02        BEGSR
091200070423      *
091300070423      * Memorizzazione dati di default in apposita ds
091400070423     c                   clear                   UNANAds
091500070423     c                   move      V2Ctip        DSAtip
091600070423     c                   eval      DSAcod    = V2Ccod
091700120508     c     c_MI:c_MA     xlate     V2Cmar        V2Cmar
091800070423     c                   eval      DSAmar    = V2Cmar
091900120508     c     c_MI:c_MA     xlate     V2Cmod        V2Cmod
092000070423     c                   eval      DSAmod    = V2Cmod
092100120508     c     c_MI:c_MA     xlate     V2Cmat        V2Cmat
092200070423     c                   eval      DSAmat    = V2Cmat
092300120508     c     c_MI:c_MA     xlate     V2CDES        V2Cdes
092400070423     c                   eval      DSAdes    = V2Cdes
092500120508     c     c_MI:c_MA     xlate     V2Cnot        V2Cnot
092600070423     c                   eval      DSAnot    = V2Cnot
092700070423     c                   eval      DSAdcn    = W2Cdcn
092800070423     c                   eval      DSAdsg    = W2Cdsg
092900070423     c                   eval      DSAsis    = V2Csis
093000070423     c                   move      V2Cdms        DSAdms
093100070423     c                   move      V2Cdmf        DSAdmf
093200070423     c                   eval      DSAdsm    = W2Cdsm
093300070423     c                   eval      DSAlea    = V2Clea
093400070423     c                   eval      DSAdsl    = W2Cdsl
093500070423     c                   move      V2Cmag        DSAmag
093600120508     c     c_MI:c_MA     xlate     V2Cemu        V2Cemu
093700120412     c                   eval      DSAemu    = %editc(v2cord:'X') + V2Cemu
093800120508     c     c_MI:c_MA     xlate     V2Cmvd        V2Cmvd
093900070423     c                   eval      DSAmvd    = V2Cmvd
094000120508     c     c_MI:c_MA     xlate     V2Cmta        V2Cmta
094100070423     c                   eval      DSAmta    = V2Cmta
094200120508     c     c_MI:c_MA     xlate     V2Cmdf        V2Cmdf
094300070423     c                   eval      DSAmdf    = V2Cmdf
094400120508     c     c_MI:c_MA     xlate     V2Cmra        V2Cmra
094500070423     c                   eval      DSAmra    = V2Cmra
094600120508     c     c_MI:c_MA     xlate     V2Cmdr        V2Cmdr
094700070423     c                   eval      DSAmdr    = V2Cmdr
094800120508     c     c_MI:c_MA     xlate     V2Cx25        V2Cx25
094900070423     c                   eval      DSAx25    = V2Cx25
095000120508     c     c_MI:c_MA     xlate     V2Clsw        V2Clsw
095100070423     c                   eval      DSAlsw    = V2Clsw
095200070423     c                   eval      DSAksc    = V2Cksc
095300070423     c                   move      V2Crg2        DSArg2
095400070423     c***                eval      DSAftr    = *blanks
095500070423     c***                eval      DSAdtr    = *zeros
095600070423     c***                eval      DSAatb    = *blanks
095700070502      *
095800070502     c                   clear                   UNMOVds
095900070502     c                   eval      DSMdoc    = V2Cdoc
096000070502     c                   eval      DSMddo    = W2Cddo
096100070417      *
096200070417      * Passaggio al subfile S01
096300070417     c                   eval      $Video    = 'S1'
096400070503     c                   if        wVideo   <> $Video
096500070417     c                   eval      $InzS01   = *on
096600070503     c                   endif
096700070503     c                   eval      wVideo    = 'D2'
096800070417      *
096900070417     c                   ENDSR
097000070417      *
097100070417      *---------------------------------------------------------------*
097200070417      *?Gestione tasto funzionale F12 da videata D02                 ?*
097300070417      *---------------------------------------------------------------*
097400070417     c     F12D02        BEGSR
097500070424      *
097600070424      * "Sblocco" di tutte le "Immissioni Multiple Unità"
097700070424      *   precedentemente bloccate e NON confermate
097800070424     c                   exsr      sr_Unlock_all
097900070424if  2c                   if        $Err      = *on
098000070424     c                   eval      $Video    = 'D1'
098100070424     c                   leavesr
098200070424e   2c                   endif
098300070424      *
098400070424if  1c                   IF        DUKnum    > *zeros
098500070424      * Chiusura del programma
098600070424     c                   eval      DUKret    = '2'
098700070424     c                   eval      $Fine     = *on
098800070424x   1c                   ELSE
098900070418      * Ritorno alla videata precedente
099000070418     c                   eval      $Video    = 'D1'
099100070424e   1c                   ENDIF
099200070417      *
099300070417     c                   ENDSR
099400070509      *
099500070509      *---------------------------------------------------------------*
099600070509      *?Gestione videata W01                                         ?*
099700070509      *---------------------------------------------------------------*
099800070509     c     GesW01        BEGSR
099900070509      *
100000070509      * Inizializzazione della videata
100100070509if  1c                   if        $InzW01   = *on
100200070509     c                   clear                   SDUKW01
100300070509     c                   eval      $InzW01   = *off
100400070509e   1c                   endif
100500070509      *
100600070509      * Emissione della window
100700070509     c                   exfmt     SDUKW01
100800070509     c                   setoff                                       28  90
100900070509     c                   clear                   V1Dmsg
101000070509      *
101100070509sel 1c                   SELECT
101200070509      * F4=Ricerca
101300070509w   1c                   WHEN      *inKD
101400070509     c                   exsr      F04W01
101500070509      * F12=Ritorno
101600070509w   1c                   WHEN      *inKL
101700070509     c                   eval      $Video    = 'D2'
101800070509      * Invio / F6=Conferma
101900070509x   1c                   OTHER
102000070509      * - Controllo dati immessi a video
102100070509     c                   exsr      CtrW01
102200070509      * - F6=Conferma
102300070509if  2c                   if        *inKF  and  NOT *in90
102400070509     c                   eval      $Video    = 'D2'
102500070509     c                   eval      $InzD02   = *on
102600070509e   2c                   endif
102700070509e   1c                   ENDSL
102800070509      *
102900070509     c                   ENDSR
103000070509      *
103100070509      *---------------------------------------------------------------*
103200070509      *?Gestione tasto funzionale F12 da videata W01                 ?*
103300070509      *---------------------------------------------------------------*
103400070509     c     F04W01        BEGSR
103500070509      *
103600070509      * Richiamo pgm. TNSDU1R per selezionare una matricola
103700070509     c                   clear                   UNANAds
103800070509     c                   clear                   dsUNIT
103900070509     c                   eval      DSUpro    = 'A'
104000070509     c                   eval      DSUop0    = ' 1'
104100070509      *
104200070509     c                   movel(p)  dsUNIT        KPJBU
104300070509     c                   call      'TNSDU1R'
104400070509     c                   parm                    KPJBA
104500070509     c                   parm                    UNANAds
104600070509     c                   movel     KPJBU         dsUNIT
104700070509      *
104800070509sel 1c                   SELECT
104900070509      * Ritorno normale
105000070509w   1c                   WHEN           DSUf03  = *off
105100070509     c                             and  DSUerr  = *off
105200070509     c                   eval      W1Ctip    = DSAtip
105300070509     c                   eval      W1Cmat    = DSAmat
105400070509     c                   eval      $Video    = 'D2'
105500070509     c                   eval      $InzD02   = *on
105600070509      * Ritorno con errore
105700070509w   1c                   WHEN      DSUmsg   <> *blanks
105800070509     c                   seton                                        285590
105900070509     c                   eval      V1Dmsg    = DSUmsg
106000070509      * Ritorno con F3
106100070509w   1c                   WHEN      DSUf03    = '1'
106200070509     c                   eval      *in55     = *on
106300070509     c                   eval      *in90     = *on
106400070509      * Ritorno con F12
106500070509w   1c                   WHEN      DSUf03    = '2'
106600070509     c                   eval      *in55     = *on
106700070509     c                   eval      *in90     = *on
106800070509e   1c                   ENDSL
106900070509      *
107000070509     c                   ENDSR
107100070509      *
107200070509      *---------------------------------------------------------------*
107300070509      *?Controllo dati immessi in videata W01                        ?*
107400070509      *---------------------------------------------------------------*
107500070509     c     CtrW01        BEGSR
107600070509      *
107700070509sel 1c                   select
107800070509      * Tipo unità obbligatorio
107900070509w   1c                   when      W1Ctip    = *zeros
108000070509     c                   seton                                        285590
108100070509     c                   eval      V1Dmsg    = $Msg(17)
108200070509      * Matricola obbligatoria
108300070509w   1c                   when      W1Cmat    = *zeros
108400070509     c                   seton                                        285690
108500070509     c                   eval      V1Dmsg    = $Msg(18)
108600070509      * Tipo e/o Matricola errati
108700070509x   1c                   other
108800070509     c     K02UNA02      chain     UNANA002
108900070509if  2c                   if        NOT  %found(UNANA02L)
109000070509     c                             or   UNAatb <> *blanks
109100070509     c                   seton                                        285590
109200070509     c                   eval      V1Dmsg    = $Msg(19)
109300070509e   2c                   endif
109400070509e   1c                   endsl
109500070509      *
109600070509     c                   ENDSR
109700120507      *
109800120507      *---------------------------------------------------------------*
109900120507      *?Gestione videata W02                                         ?*
110000120507      *---------------------------------------------------------------*
110100120507     c     GesW02        BEGSR
110200120507      *
110300120507      * Inizializzazione della videata
110400120507if  1c                   if        $InzW02   = *on
110500120507     c                   clear                   SDUKW02
110600120508     c                   eval      W2Dprg    = SAVprg1
110700120507     c                   eval      $InzW02   = *off
110800120507e   1c                   endif
110900120507      *
111000120507      * Emissione della window
111100120507     c                   exfmt     SDUKW02
111200120507     c                   setoff                                       28  90
111300120507     c                   clear                   V1Dmsg
111400120507      *
111500120507sel 1c                   SELECT
111600120507      * F12=Ritorno
111700120507w   1c                   WHEN      *inKL
111800120507     c                   eval      $Video    = 'S1'
111900120507      * Invio
112000120507x   1c                   OTHER
112100120507      * - Controllo dati immessi a video
112200120507     c                   exsr      CtrW02
112300120507      * - F6=Conferma
112400120507if  2c                   if        NOT *in90
112500120507     c                   exsr      F06W02
112600120507e   2c                   endif
112700120507e   1c                   ENDSL
112800120507      *
112900120507     c                   ENDSR
113000120507      *
113100120507      *---------------------------------------------------------------*
113200120507      *?Controllo dati immessi in videata W02                        ?*
113300120507      *---------------------------------------------------------------*
113400120507     c     CtrW02        BEGSR
113500120507      *
113600120507sel 1c                   select
113700120507      * Numero di matricole da selezionare x la conferma: obbligatorio
113800120507w   1c                   when      W2Cnms    = *zeros
113900120507     c                   seton                                        285790
114000120507     c                   eval      V1Dmsg    = $Msg(21)
114100120507      * Numero di matricole da selezionare x la conferma: negativo
114200120507w   1c                   when      W2Cnms    < *zeros
114300120507     c                   seton                                        285790
114400120507     c                   eval      V1Dmsg    = $Msg(22)
114500120507e   1c                   endsl
114600120507      *
114700120507      * Verifica opzioni eventualmente immesse PRIMA di premere F10
114800120507     c                   readc     SDUKS01
114900120507      *
115000120507do  1c                   DOW       NOT %eof(TNSDUKD)
115100120507      *
115200120507     c                   eval      *in32     = *off
115300120507     c                   movea     *zeros        *in(50)
115400120507     c                   z-add     S01nrr        C01csr
115500120507      *
115600120507sel 2c                   SELECT
115700120507      * - Nessuna opzione
115800120507w   2c                   WHEN      S1Copz    = *blanks
115900120507      * - Record già elaborato
116000120507     c                   WHEN      H1in40    = *on
116100120507      * - Opz. 1=Conferma "manuale" singola unità
116200120507      * - Opz. 4=Cancellazione singola unità (in subr. F06S01)
116300120507w   2c                   WHEN      S1Copz    = '1'  or
116400120507     c                             S1Copz    = '4'
116500120507if  3c                   if        S01nrr    > W2Cnms
116600120507     c                   seton                                        285090
116700120507     c                   eval      V1Dmsg    = $Msg(23)
116800120507e   3c                   endif
116900120507x   2c                   OTHER
117000120507     c                   seton                                        285090
117100120507     c                   eval      V1Dmsg    = $Msg(16)
117200120507e   2c                   ENDSL
117300120507      *
117400120507      * Aggiornamento sfl
117500120507     c                   eval      *in40     = H1in40
117600120507if  2c                   if        *in90
117700120507     c                   eval      C01csr    = S01nrr
117800120507     c                   eval      *in32     = *on
117900120507e   2c                   endif
118000120507      *                  _________________
118100120507     c                   UPDATE    SDUKS01
118200120507      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
118300120507if  2c                   if        *in28  OR  *in90
118400120507     c                   leave
118500120507e   2c                   endif
118600120507      *
118700120507     c                   readc     SDUKS01
118800120507      *
118900120507e   1c                   ENDDO
119000120507      *
119100120507     c                   ENDSR
119200120507      *
119300120507      *---------------------------------------------------------------*
119400120507      *?Gestione tasto funzionale F06 da videata W02                 ?*
119500120507      *---------------------------------------------------------------*
119600120507     c     F06W02        BEGSR
119700120507      *
119800120507      * Conferma PARZIALE del singolo inserimento multiplo unità
119900120507      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
120000120507     c                   eval      $Video    = 'S1'
120100120507     c                   eval      $Err      = *off
120200120507     c                   clear                   C01csr
120300120507      *
120400120507      * Ciclo di lettura subfile S01
120500120507do  1c     1             DO        W2Cnms        S01nrr
120600120507      *
120700120507     c     S01nrr        chain     SDUKS01
120800120507      * - E.o.F.
120900120507if  2c                   if        NOT %found(TNSDUKD)
121000120507     c                   leave
121100120507e   2c                   endif
121200120507     c                   eval      *in32     = *off
121300120507     c                   movea     *zeros        *in(50)
121400120507      *
121500120507      * - Conferma dei soli records NON "cancellati" (opz.4)
121600120507      *                           e NON già elaborati (*in40 = *on)
121700120507sel 2c                   select
121800120507     c                   when      H1in40    = *on
121900120507w   2c                   when      S1Copz    = *blanks   or
122000120507     c                             S1Copz    = '1'
122100120507     c                   exsr      OpzS01_1
122200120507w   2c                   when      S1Copz    = '4'
122300120507     c                   exsr      OpzS01_4
122400120507e   2c                   endsl
122500120507      *
122600120507      * - Aggiornamento sfl
122700120507     c                   eval      *in40     = H1in40
122800120507if  2c                   if        *in90
122900120507     c                   eval      C01csr    = S01nrr
123000120507     c                   eval      *in32     = *on
123100120507     c                   eval      $Err      = *on
123200120507e   2c                   endif
123300120507      *                  _________________
123400120507     c                   update    SDUKS01
123500120507      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
123600120507if  2c                   if        DSUf03    = '1'
123700120507     c*** già così:      eval      C01csr    = S01nrr
123800120507     c                   leavesr
123900120507e   2c                   endif
124000120507e   1c                   ENDDO
124100120507      *
124200120507     c                   if        $Err      = *off
124300120507      * Riemissione della videata S01
124400120507     c                   eval      $InzS01   = *on
124500120507     c                   eval      $InzW02   = *on
124600120507     c                   else
124700120507      * Riemissione di ciò che resta nella videata S01
124800120507     c                   eval      $InzS01   = *off
124900120507     c                   endif
125000120507      *
125100120507     c                   ENDSR
125200070417      *
125300070417      *---------------------------------------------------------------*
125400070417      *?Gestione videata S01                                         ?*
125500070417      *---------------------------------------------------------------*
125600070417     c     GesS01        BEGSR
125700070417      *
125800070417      * Inizializzazione della videata
125900070417if  1c                   if        $InzS01   = *on
126000120507     c                   clear                   SDUKW02
126100070417     c                   exsr      InzS01
126200070417     c                   eval      $InzS01   = *off
126300120507     c                   eval      $InzW02   = *on
126400070417e   1c                   endif
126500070417      *
126600070417      * Scrittura di testata e riga tasti funzionali abilitati
126700070417     c                   write     SDUKT01
126800070418     c                   write     SDUKZ03
126900070417      *
127000070417      * Visualizzazione (eventuale) del msg per la mancanza di dati
127100070417if  1c                   if        C01rcd   <= *zeros
127200070417     c                   write     SDUKD00
127300070417e   1c                   endif
127400070417      *
127500070417      * Emissione della videata
127600070417     c                   exfmt     SDUKC01
127700070417if  1c                   if        C01csr    > *zeros
127800070417     c                   eval      C01rcd    = C01csr
127900070417e   1c                   endif
128000070417     c                   setoff                                       28  90
128100070417     c                   clear                   V1Dmsg
128200070417      *
128300070417sel 1c                   SELECT
128400070417      * F3=Fine
128500070417w   1c                   WHEN      *inKC
128600070424     c                   exsr      F03S01
128700070417      * F5=Rivisualizzazione
128800070417w   1c                   WHEN      *inKE
128900070417     c                   exsr      F05S01
129000120507      * F10=Conferma Parziale
129100120507w   1c                   WHEN      *inKJ
129200120507     c                   eval      $InzW02   = *on
129300120507     c                   exsr      GesW02
129400120508if  2c*// inutile qui:   if        *in90
129500120508     c*// inutile qui:   leavesr
129600120508e   2c*// inutile qui:   endif
129700070418      * F12=Ritorno
129800070418w   1c                   WHEN      *inKL
129900070418     c                   exsr      F12S01
130000070417      * Roll-UP
130100070418w   1c***                WHEN      £Tasto    = C_RollUp
130200070418     c***                exsr      RollUpS01
130300070417      * NO rec.
130400070417w   1c                   WHEN      S01nrr   <= *zeros
130500070417      *
130600070418      * Invio / F6=Conferma
130700070417x   1c                   OTHER
130800070418      * - Gestione opzioni inserite
130900070417     c                   exsr      OpzS01
131000070502      * - F6=Conferma totale
131100070503if  2c                   IF        *inKF  and  NOT *in90
131200070418     c                   exsr      F06S01
131300070503e   2c                   ENDIF
131400070502if  2c                   if        *in90
131500070502     c                   leavesr
131600070502e   2c                   endif
131700070423      * - Cancellazione inserimenti multipli elaborati
131800070423     c                   eval      xx        = 1
131900070503do  2c                   DOW       xx       <= %elem($InMuUn)
132000070503if  3c                   If             $InMuUn(xx) <> *zeros
132100070502     c                             and  $RecIMU(xx) <= *zeros
132200070423     c                   clear                   TNSDUJds
132300070423     c                   eval      DUJopz    = 'D'
132400070423     c                   eval      DUJnum    = $InMuUn(xx)
132500070423     c                   exsr      Call_TNSDUJ1
132600070503if  4c                   if        *in90
132700070503     c                   eval      $InzD01   = *off
132800070503e   4c                   endif
132900070503e   3c                   EndIf
133000070423     c                   eval      xx        = xx + 1
133100070503e   2c                   ENDDO
133200070503      *
133300070417e   1c                   ENDSL
133400070417      *
133500070417     c                   ENDSR
133600070417      *
133700070417      *---------------------------------------------------------------*
133800070417      *?Inizializzazione videata S01                                 ?*
133900070417      *---------------------------------------------------------------*
134000070417     c     InzS01        BEGSR
134100070509      *
134200070509      * "Intestazione" del subfile-control
134300070509     c                   clear                   SDUKC01
134400070509     c                   exsr      Upd_Txt_C01
134500070417      *
134600070417      * Pulizia subfile
134700070417     c                   seton                                        3031
134800070417     c                   write     SDUKC01
134900070417     c                   setoff                                         3133
135000070417      *
135100070417     c                   reset                   $EoF
135200070417     c                   clear                   W_SflPag
135300070503     c*** Non serve:     clear                   C01csr
135400070417     c                   clear                   C01rcd
135500070417     c                   clear                   S01nrr
135600070417     c                   clear                   V1Dmsg
135700070417     c                   setoff                                       28  90
135800070417     c                   movea     *zeros        *in(50)
135900070420      *
136000070420     c                   clear                   xx
136100070420     c                   clear                   $InMuUn
136200070420     c                   clear                   $RecIMU
136300070424     c                   clear                   SAVnum
136400120508     c*//                clear                   SAVprg
136500120508     c                   clear                   SAVprg1
136600070509     c                   clear                   dWIMuni1
136700070417      *
136800070418      * Caricamento completo dei dati da presentare nel sfl
136900070509     c                   eval      WIMnum    = V1Cnum
137000070509     c                   eval      WIMtrk    = '1'
137100070509     c     K02WIM01      chain     WFAIM000
137200070509     c                   eval      $EoF      = (NOT %found(WFAIM01L))
137300070417if  1c                   if        $EoF      = *off
137400070417     c                   exsr      RollUpS01
137500070417e   1c                   endif
137600070503      *
137700070503      * Posizionamento iniziale del subfile sul 1° rec. nella 1ª pag.
137800070503if  1c                   if        NOT *in30
137900070503     c                   eval      C01rcd    = 1
138000070503e   1c                   endif
138100070418      *
138200070418      * Abilitazione tasti funzionali
138300070418      * - F6=Conferma (solo se ci sono dati nel sfl)
138400070418     c                   eval      *in06     = (NOT *in30)
138500070417      *
138600070417     c                   ENDSR
138700070417      *
138800070417      *---------------------------------------------------------------*
138900070417      *?Caricamento pagina successiva di dati S01                    ?*
139000070417      *---------------------------------------------------------------*
139100070417     c     RollUpS01     BEGSR
139200070417      *
139300070417     c                   eval      S01nrr    = (W_SflPag * C_SflPag)
139400070417     c                   add       1             W_SflPag
139500070417     c                   eval      *in32     = *off
139600070417      *
139700070417      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
139800070521do  1c                   DOW            $EoF    = *off
139900070418     c*** NO solo 1 pag.:          and  S01nrr  < W_SflPag * C_SflPag
140000070417     c                   exsr      CarS01
140100070509     c     V1Cnum        reade     WFAIM000
140200070509     c                   eval      $EoF      = (%eof(WFAIM01L))
140300070521      * - Si scartano i records già "confermati"
140400070521if  2c                   if             $Eof    = *off
140500070521     c                             and  WIMtrk  > '2'
140600070521     c     WIMnum        setgt     WFAIM000
140700070521     c                   eval      $EoF      = *on
140800070521e   2c                   endif
140900070521e   1c                   ENDDO
141000070417      *
141100070417      * Visualizzazione del SFL (se ci sono dati)
141200070417     c                   eval      *in30     = (S01nrr = *zeros)
141300070417      *
141400070417      * Attivazione (eventuale) del SFLEND
141500070417     c                   eval      *in33     = $EoF
141600070417      *
141700070417      * Posizionamento cursore al 1° rcd della pagina
141800070417if  1c                   if        S01nrr    > *zeros
141900070417     c     S01nrr        div       C_SflPag      wPag
142000070417     c                   mvr                     wRig
142100070417     c                   eval      C01rcd    = wPag * C_SflPag
142200070417     c                   if        wRig      > *zeros
142300070417     c                   eval      C01rcd    = C01rcd + 1
142400070417     c                   else
142500070417     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
142600070417     c                   endif
142700070417x   1c                   else
142800070417     c                   clear                   C01rcd
142900070417e   1c                   endif
143000070417      *
143100070417     c                   ENDSR
143200070417      *
143300070417      *---------------------------------------------------------------*
143400070417      *?Caricamento dati in subfile S01                              ?*
143500070417      *---------------------------------------------------------------*
143600070417     c     CarS01        BEGSR
143700070417      *
143800070418      * Rottura Numero Inserimento Multiplo Unità
143900070509if  1c                   IF        WIMnum   <> SAVnum
144000070503     c                   exsr      sr_Lock_trk1
144100070418if  2c                   if        $Err      = *on
144200070509     c     WIMnum        setgt     WFAIM000
144300070418     c                   leavesr
144400070418e   2c                   endif
144500070420     c                   add       1             xx
144600070509     c                   eval      $InMuUn(xx) = WIMnum
144700070509     c                   eval      SAVnum    = WIMnum
144800120508     c*//                eval      SAVprg    = *hival
144900070418e   1c                   ENDIF
145000070418      *
145100070418      * Selezione dei soli trk. "2"
145200070518      *N.B.?- Altri trk NON vengono visualizzati ma
145300070518      *        VERRANNO CANCELLATI all'eventuale F6 di conferma.
145400070518if  1c                   if             WIMtrk <> '2'
145500070518     c                             and  WIMtrk <> '4'
145600070418     c                   leavesr
145700070418e   1c                   endif
145800070417      *
145900070417      * Caricamento dati nel record del sfl
146000070418     c                   clear                   SDUKS01
146100070420      * - Campi HIDDEN
146200070507     c                   eval      H1in40    = *off
146300070509      *
146400070418      * - Progressivo unità
146500070509     c                   eval      S1Cprg    = WIMprg
146600070509      * - Barcode caricato
146700070509     c                   eval      S1Cuni    = WIMuni
146800070509      * - Matricola unità
146900070509     c                   eval      S1Cmat    = %subst(WIMuni
147000070509     c                                              : §WIM1im2
147100070509     c                                              : §WIM1lm2)
147200070420      *
147300070420      * Impostazione attributi di visualizzazione
147400070420      * - Visualizzazione record
147500070507     c                   eval      *in40     = H1in40
147600070417      *
147700070417      * Scrittura record subfile
147800070417     c                   add       1             S01nrr
147900070418     c                   write     SDUKS01
148000070420      *
148100070420      * Conteggio record caricati per Inserimento Multiplo Unità
148200070420     c                   add       1             $RecIMU(xx)
148300070420      *
148400070420      * Memorizzazione ultimi campi "di controllo" utilizzati
148500120508     c*//                eval      SAVprg    = WIMprg
148600120508     c                   if        SAVprg1   = *zero
148700120508     c                   eval      SAVprg1   = WIMprg
148800120508     c                   endif
148900070417      *
149000070417     c                   ENDSR
149100070417      *
149200070417      *---------------------------------------------------------------*
149300070417      *?Gestione tasto funzionale F03 da videata S01                 ?*
149400070417      *---------------------------------------------------------------*
149500070417     c     F03S01        BEGSR
149600070424      *
149700070424      * "Sblocco" di tutte le "Immissioni Multiple Unità"
149800070424      *   precedentemente bloccate e NON confermate
149900070424     c                   exsr      sr_Unlock_all
150000070424if  2c                   if        $Err      = *on
150100070424     c                   eval      $Video    = 'D1'
150200070424     c                   leavesr
150300070424e   2c                   endif
150400070417      *
150500070417      * Chiusura del programma
150600070424     c                   eval      DUKret    = '1'
150700070417     c                   eval      $Fine     = *on
150800070417      *
150900070417     c                   ENDSR
151000070423      *
151100070423      *---------------------------------------------------------------*
151200070423      *?Gestione tasto funzionale F05 da videata S01                 ?*
151300070423      *---------------------------------------------------------------*
151400070423     c     F05S01        BEGSR
151500070423      *
151600070423      * Ricarica del subfile
151700070423     c                   eval      $InzS01   = *on
151800070423      *
151900070423     c                   ENDSR
152000070417      *
152100070417      *---------------------------------------------------------------*
152200070417      *?Gestione tasto funzionale F06 da videata S01                 ?*
152300070417      *---------------------------------------------------------------*
152400070417     c     F06S01        BEGSR
152500070417      *
152600070420      * Conferma TOTALE dei vari inserimenti multipli unità
152700070509      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
152800070509     c                   eval      $Err      = *off
152900070516     c                   clear                   C01csr
153000070509      *
153100070420      * Ciclo di lettura subfile S01
153200070516do  1c                   DO        *hival        S01nrr
153300070420      *
153400070516     c     S01nrr        chain     SDUKS01
153500070420      * - E.o.F.
153600070420if  2c                   if        NOT %found(TNSDUKD)
153700070420     c                   leave
153800070420e   2c                   endif
153900070420     c                   eval      *in32     = *off
154000070420     c                   movea     *zeros        *in(50)
154100070420      *
154200070509      * - Conferma dei soli records NON "cancellati" (opz.4)
154300070516      *                           e NON già elaborati (*in40 = *on)
154400070420sel 2c                   select
154500070516     c                   when      H1in40    = *on
154600070420w   1c                   when      S1Copz    = *blanks
154700070503     c                   exsr      OpzS01_1
154800070420w   2c                   when      S1Copz    = '4'
154900070503     c                   exsr      OpzS01_4
155000070420e   2c                   endsl
155100070507      *
155200070507      * - Aggiornamento sfl
155300070507     c                   eval      *in40     = H1in40
155400070507if  2c                   if        *in90
155500070516     c                   eval      C01csr    = S01nrr
155600070507     c                   eval      *in32     = *on
155700070509     c                   eval      $Err      = *on
155800070507e   2c                   endif
155900070420      *                  _________________
156000070420     c                   update    SDUKS01
156100070420      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
156200070509if  2c                   if        DSUf03    = '1'
156300070516     c*** già così:      eval      C01csr    = S01nrr
156400070509     c                   leavesr
156500070509e   2c                   endif
156600070420e   1c                   ENDDO
156700070502      *
156800070509     c                   if        $Err      = *off
156900070503      * Ritorno alla videata D01
157000070503     c                   eval      $Video    = 'D1'
157100070503     c                   eval      $InzD01   = *on
157200070509     c                   else
157300070509      * Riemissione di ciò che resta nella videata S01
157400070509     c                   eval      $InzS01   = *off
157500070509     c                   endif
157600070417      *
157700070417     c                   ENDSR
157800070418      *
157900070418      *---------------------------------------------------------------*
158000070418      *?Gestione tasto funzionale F12 da videata S01                 ?*
158100070418      *---------------------------------------------------------------*
158200070418     c     F12S01        BEGSR
158300070418      *
158400070418      * Ritorno alla videata precedente
158500070503     c                   eval      wVideo    = $Video
158600070418     c                   eval      $Video    = 'D2'
158700070418      *
158800070418     c                   ENDSR
158900070417      *
159000070417      *---------------------------------------------------------------*
159100070417      *?Controllo opzioni subfile S01                                ?*
159200070417      *---------------------------------------------------------------*
159300070417     c     OpzS01        BEGSR
159400070417      *
159500070423     c                   readc     SDUKS01
159600070417      *
159700070423do  1c                   DOW       NOT %eof(TNSDUKD)
159800070417      *
159900070417     c                   eval      *in32     = *off
160000070417     c                   movea     *zeros        *in(50)
160100070516     c                   z-add     S01nrr        C01csr
160200070417      *
160300070417sel 2c                   SELECT
160400070417      * - Nessuna opzione
160500070417w   2c                   WHEN      S1Copz    = *blanks
160600120507      * - Record già elaborato
160700120507w   2c                   WHEN      H1in40    = *on
160800070420      * - Opz. 1=Conferma "manuale" singola unità
160900070420w   2c                   WHEN      S1Copz    = '1'
161000070503     c                   exsr      OpzS01_1
161100070420      * - Opz. 4=Cancellazione singola unità (in subr. F06S01)
161200070417w   2c                   WHEN      S1Copz    = '4'
161300070503     c*** Richiesto F6:  exsr      OpzS01_4
161400070417x   2c                   OTHER
161500070417     c                   seton                                        285090
161600070509     c                   eval      V1Dmsg    = $Msg(16)
161700070417e   2c                   ENDSL
161800070417      *
161900070417      * Aggiornamento sfl
162000070507     c                   eval      *in40     = H1in40
162100070417sel 2c                   select
162200070417w   2c                   when      *in28
162300070417     c                   eval      *in32     = *on
162400070509w   2c*** come se ok:    when      *in90     = *on
162500070509     c***  "   "   "     clear                   S1Copz
162600070502     c                   when      S1Copz    = '4'
162700070417x   2c                   other
162800070417     c                   clear                   S1Copz
162900070417e   2c                   endsl
163000070420     c                   UPDATE    SDUKS01
163100070417if  2c                   if        *in28  OR  *in90
163200070417     c                   leave
163300070417e   2c                   endif
163400070417      *
163500070420     c                   readc     SDUKS01
163600070417      *
163700070417e   1c                   ENDDO
163800070417      *
163900070417     c                   ENDSR
164000070417      *
164100070417      *---------------------------------------------------------------*
164200070420      *?Conferma del singolo rec. "inserimento multiplo unità"       ?*
164300070417      *---------------------------------------------------------------*
164400070503     c     OpzS01_1      BEGSR
164500070417      *
164600070423      * GIÀ ESEGUITO:
164700070423      * 1) "Blocco" dell'inserimento multiplo unità             (trk.1)
164800070423      * DA ESEGUIRE:
164900070423      * 2) Conferma dell'inserimento multiplo unità in UNANA    (trk.2)
165000070509      * 3) Cancellaz. dell'inserimento multiplo unità da WFAIM  (trk.2)
165100070518      *    => in realtà imposta trk. "4" nel record (appena elaborato)
165200070423      *
165300070423     c*** già impostata: reset                   UNANAds
165400070423     c                   reset                   dsUNIT
165500070509if  1c                   if        S1Copz    = *blanks
165600070509     c                   eval      DSUop1    = 'F6N'
165700070509e   1c                   endif
165800070509     c                   movel(p)  dsUNIT        KPJBU
165900120508     c     c_MI:c_MA     xlate     S1Cmat        S1Cmat
166000070509     c                   movel     S1Cmat        DSAmat
166100070423      *
166200070503     c                   call      'TNSDU2R'
166300070423     c                   parm                    KPJBA
166400070423     c                   parm                    UNANAds
166500070502     c                   parm                    UNMOVds
166600070423     c                   movel     KPJBU         dsUNIT
166700070423      *
166800070423sel 1c                   SELECT
166900070423      * Ritorno normale
167000070423w   1c                   WHEN           DSUf03  = *off
167100070423     c                             and  DSUerr  = *off
167200070503     c                   exsr      OpzS01_4
167300070503     c                   eval      $InzS01   = *on
167400070423      * Ritorno con errore
167500070423w   1c                   WHEN      DSUmsg   <> *blanks
167600070518     c***                eval      *in32     = *on
167700070509     c                   seton                                        285090
167800070423     c                   eval      V1Dmsg    = DSUmsg
167900070423      * Ritorno con F3
168000070423w   1c                   WHEN      DSUf03    = '1'
168100070518     c***                eval      *in32     = *on
168200070423     c                   eval      *in50     = *on
168300070423     c                   eval      *in90     = *on
168400070423      * Ritorno con F12
168500070423w   1c                   WHEN      DSUf03    = '2'
168600070518     c                   eval      *in32     = *on
168700070502     c                   eval      *in50     = *on
168800070518     c***                eval      *in90     = *on
168900070423e   1c                   ENDSL
169000070417      *
169100070417     c                   ENDSR
169200070420      *
169300070420      *---------------------------------------------------------------*
169400070518      *?Impostaz. trk.4 nel singolo rec. "inserimento multiplo unità"?*
169500070420      *---------------------------------------------------------------*
169600070503     c     OpzS01_4      BEGSR
169700070420      *
169800070423      * GIÀ ESEGUITO:
169900070423      * 1) "Blocco" dell'inserimento multiplo unità             (trk.1)
170000070518      * 2) Conferma dell'inserimento multiplo unità in UNANA    (trk.2)
170100070423      * DA ESEGUIRE:
170200070518      * 3) Cancellaz. dell'inserimento multiplo unità da WFAIM  (trk.2)
170300070518      *    => in realtà imposta trk. "4" nel record (appena elaborato)
170400070503     c                   clear                   TNSDUJds
170500070503     c                   eval      DUJopz    = '4'
170600070509     c                   eval      DUJnum    = V1Cnum
170700070503     c                   eval      DUJprg    = S1Cprg
170800070424     c                   exsr      Call_TNSDUJ2
170900070423if  1c                   if             DUJerr <> *off
171000070423     c                             or   DUJret <> *off
171100070423     c                   leavesr
171200070423e   1c                   endif
171300070503      *
171400070503      * Aggiornamento n° unità da inserire
171500070503     c                   eval      *in25     = *off
171600070503     c                   eval      xx        = 1
171700070509     c     V1Cnum        lookup    $InMuUn(xx)                            25
171800070503     c                   if        *in25
171900070503     c                   sub       1             $RecIMU(xx)
172000070503     c                   endif
172100070420      *
172200070503      * Aggiornamento rec. del subfile
172300070509     c                   eval      H1in40    = *on
172400070420      *
172500070420     c                   ENDSR
172600070509      *
172700070509      *---------------------------------------------------------------*
172800070509      *?Aggiornamento testata in C01 con nuovi dati restituiti da R1 ?*
172900070509      *---------------------------------------------------------------*
173000070509     c     Upd_Txt_C01   BEGSR
173100070509      *
173200070509     c                   eval      C1Dnum    = V1Cnum
173300130923     c                   eval      C1Ddes    = §WIM1des + §WIM12des
173400070509     c                   eval      C1Dnmi    = §WIM1nmi
173500070509      *
173600070509     c                   ENDSR
173700070417      *
173800070417      *---------------------------------------------------------------*
173900070509      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
174000070417      *---------------------------------------------------------------*
174100070417     c     Call_TNSDUJ1  BEGSR
174200070417      *
174300070417     c                   movel(p)  TNSDUJds      KPJBU
174400070417     c                   call      'TNSDUJR1'
174500070417     c                   parm                    KPJBA
174600070417     c                   movel     KPJBU         TNSDUJds
174700070417      *
174800070417sel 1c                   SELECT
174900070417      * Ritorno normale
175000070417w   1c                   WHEN           DUJret  = *off
175100070417     c                             and  DUJerr  = *off
175200070417      * Ritorno con errore
175300070417w   1c                   WHEN      DUJerr    = *on
175400070417     c                   seton                                        28  90
175500070417     c                   eval      V1Dmsg    = DUJmsg
175600070417      * Ritorno con F3
175700070417w   1c                   WHEN      DUJret    = '1'
175800070417     c                   eval      *in90     = *on
175900070417      * Ritorno con F12
176000070417w   1c                   WHEN      DUJret    = '2'
176100070417     c                   eval      *in90     = *on
176200070417      *
176300070417e   1c                   ENDSL
176400070417      *
176500070417     c                   ENDSR
176600070503      *
176700070503      *---------------------------------------------------------------*
176800070503      *?Richiamo del pgm. TNSDUJR2 (cancellazione singolo trk. "2")  ?*
176900070503      *---------------------------------------------------------------*
177000070503     c     Call_TNSDUJ2  BEGSR
177100070503      *
177200070503     c                   movel(p)  TNSDUJds      KPJBU
177300070503     c                   call      'TNSDUJR2'
177400070503     c                   parm                    KPJBA
177500070503     c                   movel     KPJBU         TNSDUJds
177600070503      *
177700070503sel 1c                   SELECT
177800070503      * Ritorno normale
177900070503w   1c                   WHEN           DUJret  = *off
178000070503     c                             and  DUJerr  = *off
178100070503      * Ritorno con errore
178200070503w   1c                   WHEN      DUJerr    = *on
178300070509     c                   eval      *in32     = *on
178400070509     c                   seton                                        285090
178500070503     c                   eval      V1Dmsg    = DUJmsg
178600070503      * Ritorno con F3
178700070503w   1c                   WHEN      DUJret    = '1'
178800070509     c                   eval      *in32     = *on
178900070503     c                   eval      *in90     = *on
179000070503      * Ritorno con F12
179100070503w   1c                   WHEN      DUJret    = '2'
179200070509     c                   eval      *in32     = *on
179300070503     c                   eval      *in90     = *on
179400070503      *
179500070503e   1c                   ENDSL
179600070503      *
179700070503     c                   ENDSR
179800070418      *
179900070418      *---------------------------------------------------------------*
180000070418      *?Verifica disponibilità dell'inserimento multiplo da trk "1"  ?*
180100070420      *?  e "allocazione" dello stesso.                              ?*
180200070418      *---------------------------------------------------------------*
180300070503     c     sr_Lock_trk1  BEGSR
180400070418      *
180500070424     c                   eval      $Err      = *off
180600070424      *
180700070418      * Il primo record del nuovo inserimento multiplo unità DEVE
180800070518      *   essere trk. "1" (se da confermare) o "3" (se già confermato)
180900070509     c                   clear                   dWIMuni1
181000070518if  1c                   if             WIMtrk = '1'
181100070518     c                             or   WIMtrk = '3'
181200070509     c                   movel     WIMuni        dWIMuni1
181300070418x   1c                   else
181400070509     c                   eval      §WIM1des  = *all'? '
181500070509     c                   eval      §WIM1usp  = *all'? '
181600070418e   1c                   endif
181700070418      *
181800070509      * Gestione rec. trk "1" del file WFAIM00F per
181900070418      * "Blocco" dell'inserimento multiplo unità x conferma
182000070418if  1c                   IF        V1Cnum    = *zeros
182100070418     c                   clear                   TNSDUJds
182200070418     c                   eval      DUJopz    = '9'
182300070509     c                   eval      DUJnum    = WIMnum
182400070418     c                   exsr      Call_TNSDUJ1
182500070418      * - Gli inserimenti multipli unità impegnati vengono scartati
182600070503if  2c                   if             DUJerr <> *off
182700070418     c                             or   DUJret <> *off
182800070418     c                   eval      $Err      = *on
182900070418     c                   leavesr
183000070503e   2c                   endif
183100070418e   1c                   ENDIF
183200070418      *
183300070420      * Eventuale "sistemazione dei dati del trk. "1"
183400070509if  1c                   if        §WIM1im2 <= *zeros
183500070509     c                   eval      §WIM1im2  = 1
183600070418e   1c                   endif
183700070509if  1c                   if             §WIM1lm2 <= *zeros
183800070509     c                             or   §WIM1lm2  > %len(UNAmat)
183900070509     c                   eval      §WIM1lm2  = %len(UNAmat)
184000070418e   1c                   endif
184100070418      *
184200070418     c                   ENDSR
184300070423      *
184400070423      *---------------------------------------------------------------*
184500070423      *?"Disallocazione" di tutti gli inserimenti multipli unità NON ?*
184600070423      *?  confermati e NON cancellati                                ?*
184700070423      *---------------------------------------------------------------*
184800070423     c     sr_Unlock_all BEGSR
184900070423      *
185000070424     c                   eval      $Err      = *off
185100070424      *
185200070509      * Gestione rec. trk "1" del file WFAIM00F per
185300070423      * "Blocco" dell'inserimento multiplo unità x conferma
185400070423     c                   eval      xx        = 1
185500070423do  1c                   DOW       xx       <= %elem($InMuUn)
185600070423      *
185700070502if  2c                   IF             $InMuUn(xx) <> *zeros
185800070502     c***                          and  $RecIMU(xx)  > *zeros
185900070423     c                   clear                   TNSDUJds
186000070423     c                   eval      DUJopz    = '8'
186100070423     c                   eval      DUJnum    = $InMuUn(xx)
186200070423     c                   exsr      Call_TNSDUJ1
186300070423      * - Gli inserimenti multipli unità impegnati vengono scartati
186400070423if  2c                   If             DUJerr <> *off
186500070423     c                             or   DUJret <> *off
186600070424     c                   eval      $Err      = *on
186700070502     c*** NO, finisco:   leavesr
186800070502x   2c                   Else
186900070502     c                   clear                   $InMuUn(xx)
187000070502     c                   clear                   $RecIMU(xx)
187100070423e   2c                   EndIf
187200070423e   2c                   ENDIF
187300070423      *
187400070423     c                   eval      xx        = xx + 1
187500070423e   1c                   ENDDO
187600070423      *
187700070423     c                   ENDSR
187800070416      *
187900070416      *---------------------------------------------------------------*
188000070416      *?Controllo del Tipo di Unità                                  ?*
188100070416      *---------------------------------------------------------------*
188200070416     c     Chk_Tip       BEGSR
188300070416      *
188400070416     c                   eval      $Err      = *off
188500070416     c                   clear                   dUNT
188600070416      *
188700070416      * Ricerca
188800070417if  1c                   if        %scan('?':%trimr(Wtip)) > *zeros
188900070416     c                   clear                   TIBS02ds
189000070416     c                   eval      T02mod    = 'R'
189100070416     c                   eval      T02sif    = KNSIF
189200070416     c                   eval      T02cod    = 'UNT'
189300070416     c                   call      'TIBS02R'
189400070416     c                   parm                    KPJBA
189500070416     c                   parm                    TIBS02ds
189600070417     c                   movel     T02ke1        Wtip
189700070416     c                   movel     T02uni        dUNT
189800070418if  2c                   if        T02ke1   <> *blanks
189900070418     c                   eval      *in90     = *on
190000070418     c                   leavesr
190100070418e   2c                   endif
190200070417e   1c                   endif
190300070416      *
190400070416      * Controllo
190500070416     c                   clear                   TIBS02ds
190600070416     c                   eval      T02mod    = 'C'
190700070416     c                   eval      T02sif    = KNSIF
190800070416     c                   eval      T02cod    = 'UNT'
190900070418     c                   movel(p)  Wtip          T02ke1
191000070416     c                   call      'TIBS02R'
191100070416     c                   parm                    KPJBA
191200070416     c                   parm                    TIBS02ds
191300070416      *
191400070416     c                   eval      $Err      = (T02err <> *blanks)
191500070416if  1c                   if        $Err      = *off
191600070416     c                   movel     T02uni        dUNT
191700070418x   1c                   else
191800070418     c                   eval      d§UNTdes  = *all'? '
191900070416e   1c                   endif
192000070416      *
192100070416     c                   ENDSR
192200070416      *
192300070416      *---------------------------------------------------------------*
192400070416      *?Controllo del secondo livello di raggruppamento              ?*
192500070416      *---------------------------------------------------------------*
192600070416     c     Chk_Rg2       BEGSR
192700070416      *
192800070416     c                   eval      $Err      = *off
192900070423     c*** NON c'è:       clear                   dUNR
193000070416      *
193100070416      * Ricerca
193200070423if  1c                   if        %scan('?':%trimr(Wrg2)) > *zeros
193300070416     c                   clear                   TIBS02ds
193400070416     c                   eval      T02mod    = 'R'
193500070416     c                   eval      T02sif    = KNSIF
193600070416     c                   eval      T02cod    = 'UNR'
193700070416     c                   call      'TIBS02R'
193800070416     c                   parm                    KPJBA
193900070416     c                   parm                    TIBS02ds
194000070417     c*** NON c'è:       movel     T02uni        dUNR
194100070418     c                   movel     T02ke1        Wrg2
194200070418if  2c                   if        T02ke1   <> *blanks
194300070418     c                   eval      *in90     = *on
194400070418     c                   leavesr
194500070418e   2c                   endif
194600070417e   1c                   endif
194700070416      * Controllo
194800070416     c                   clear                   TIBS02ds
194900070416     c                   eval      T02mod    = 'C'
195000070416     c                   eval      T02sif    = KNSIF
195100070416     c                   eval      T02cod    = 'UNR'
195200070418     c                   movel(p)  Wrg2          T02ke1
195300070416     c                   call      'TIBS02R'
195400070416     c                   parm                    KPJBA
195500070416     c                   parm                    TIBS02ds
195600070416      *
195700070416     c                   eval      $Err      = (T02err <> *blanks)
195800070416if  1c                   if        $Err      = *off
195900070417     c*** NON c'è:       movel     T02uni        dUNR
196000070418x   1c                   else
196100070418     c                   eval      T02uni    = *all'? '
196200070416e   1c                   endif
196300070416      *
196400070416     c                   ENDSR
196500070416      *
196600070416      *---------------------------------------------------------------*
196700070416      *?Controllo del magazzino di ubicazione                        ?*
196800070416      *---------------------------------------------------------------*
196900070416     c     Chk_Mag       BEGSR
197000070416      *
197100070416     c                   eval      $Err      = *off
197200070416     c                   clear                   WmagD
197300070416      *
197400070416      * Ricerca
197500070417if  1c                   if        %scan('?':%trimr(Wmag)) > *zeros
197600070416     c                   call      'TNSD20R'
197700070416     c                   parm      *blanks       §SD20cod          3
197800070416     c                   parm      *blanks       §SD20tip          1
197900070416     c                   parm      *blanks       §SD20des         25
198000070416     c                   move      §SD20cod      Wmag
198100070416     c                   movel(p)  §SD20des      WmagD
198200070418if  2c                   if        §SD20cod <> *blanks
198300070418     c                   eval      *in90     = *on
198400070418     c                   leavesr
198500070418e   2c                   endif
198600070417e   1c                   endif
198700070416      *
198800070416      * Controllo
198900070416     c                   move      Wmag          ORGfil
199000070416     c                   clear                   ORGdes
199100070416     c     ORGfil        chain     AZORG020
199200070417if  1c                   if        NOT  %found(AZORG21L)
199300070416     c     ORGfil        chain     AZORG
199400070417e   1c                   endif
199500070502     c                   eval      $Err      = (NOT %found(AZORG21L) and
199600070502     c                                          NOT %found(AZORG01L) )
199700070416if  1c                   if        $Err      = *off
199800070416     c                   movel     ORGdes        WmagD
199900070416x   1c                   else
200000070416     c                   eval      WmagD     = *all'? '
200100070416e   1c                   endif
200200070416      *
200300070416     c                   ENDSR
200400070416      *
200500070416      *---------------------------------------------------------------*
200600070416      *?Controllo della data                                         ?*
200700070416      *---------------------------------------------------------------*
200800070416     c     Chk_Dat       BEGSR
200900070416      *
201000070416     c                   eval      $Err      = *off
201100070416      *
201200070423     c                   clear                   WLBdat
201300070416     c                   eval      G08dat    = Wdata
201400070416     c                   call      'XSRDA8'
201500070416     c                   parm                    WLBdat
201600070416      *
201700070416     c                   eval      Wdata     = G08dat
201800070416     c                   eval      $Err      = (G08err <> *off)
201900070416      *
202000070416     c                   ENDSR
202100070416      *
202200070416      *---------------------------------------------------------------*
202300070416      *?Controllo del codice cliente                                 ?*
202400070416      *---------------------------------------------------------------*
202500070418     c     Chk_Ksc       BEGSR
202600070416      *
202700070416      * Ricerca
202800070416      * Se non inserito il codice ma inserita una parte della rag.soc.
202900070416      *   si richiama il programma della ricerca alfabetica
203000070417if  1c                   IF             Wksc    = *zeros
203100070417     c                             and  WkscD  <> *blanks
203200070416     c                   call      'XALFA3BR'
203300070416     c                   parm      RSUT          PAXdut           30
203400070416     c                   parm      1             PAXut             1 0
203500070418     c                   parm      WkscD         PAXdmt           48
203600070416     c                   parm      DUTkci        PAXccc            4 0
203700070416     c                   parm      9             PAXsta            1 0
203800070416     c                   parm      *blanks       PAXflr           90
203900070416     c                   parm      *blanks       PAXdit            3
204000070416     c                   parm      1             PAXnum            2 0
204100070416     c                   parm      *blanks       PAXkcm           80
204200070416     c                   parm      *blanks       PAXksm          140
204300070416     c                   parm      *blanks       PAXkdm           60
204400070416     c                   parm      'S'           PAXesci           1
204500070416     c                   parm      *blanks       PAXerr            2
204600070416     c***                parm      *blanks       PAXpiv           16
204700070416     c***                parm      *blanks       PAXcdf           16
204800070416      *
204900070416     c                   eval      $Err      = (PAXsta < *zeros)
205000070417if  2c                   if        $Err      = *off
205100070417     c                   movel     PAXksm        Wksc
205200070418     c                   movel     PAXdmt        WkscD
205300070417e   2c                   endif
205400070418if  2c                   if        PAXksm   <> *blanks
205500070418     c                   eval      *in90     = *on
205600070418     c                   leavesr
205700070418e   2c                   endif
205800070417e   1c                   ENDIF
205900070416      *
206000070416      * Controllo
206100070423if  1c                   IF        Wksc     <> *zeros
206200070416     c                   clear                   TIBS69ds
206300070417     c                   eval      I69kac    = Wksc
206400070416     c                   call      'TIBS69R'
206500070416     c                   parm                    TIBS69ds
206600070416     c                   parm                    ds_CNACO
206700070416     c                   parm                    ds_CNIND
206800070416     c                   parm                    ds_CNCLP
206900070416     c                   parm                    ds_FNCLS
207000070416      *
207100070416     c                   eval      $Err      = (O69err = *on)
207200070423if  2c                   if        $Err      = *off
207300070418     c                   movel     ACOrag        WkscD
207400070423e   2c                   endif
207500070423e   1c                   ENDIF
207600070418      *
207700070416     c                   ENDSR
207800070417      *
207900070417      *?Schiere a tempo di compilazione - - - - - - - - - - - - - - -?*
208000070417      *
208100070417** - $Msg -------------------------------------------------------------------*
208200070509Numero Inserimento Multiplo Unità obbligatorio                                 1
208300070509Tipo unità non accettabile.                                                    2
208400070509Hai indicato il tipo di PC ???????????????????                                 3
208500070509Marca unità non accettabile.                                                   4
208600070509Modello unità non accettabile.                                                 5
208700070509Matricola unità non accettabile.                                               6
208800070509Esiste già una unità con lo stesso modello e la stessa matricola.              7
208900070509Secondo livello di raggruppamento non accettabile.                             8
209000070509Data non accettabile.                                                          9
209100070509Ditta non accettabile.                                                        10
209200070509Ditta manutenzione non accettabile.                                           11
209300070509Magazzino ubicazione non accettabile.                                         12
209400070509Cliente non accettabile.                                                      13
209500070509Numero documento obbligatorio in inserimento.                                 14
209600070509Data documento obbligatoria in inserimento.                                   15
209700070509Opzione errata                                                                16
209800070509Tipo unità obbligatorio                                                       17
209900070509Matricola obbligatoria                                                        18
210000070509Tipo e/o Matricola errati                                                     19
210100120412Numero ordine obbligatorio                                                    20
210200120507Numero di matricole da selezionare per la conferma obbligatorio               21
210300120507Numero di matricole da selezionare per la conferma errato (negativo)          22
210400120507NON ammesse opzioni per matricole sequenzialmente oltre il massimo elaborabile23
