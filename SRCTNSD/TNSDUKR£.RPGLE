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
054300070509     c                   eval      V2Cmta    = UNAmta
054400070509     c                   eval      V2Cmdf    = UNAmdf
054500070509     c                   eval      V2Cmra    = UNAmra
054600070509     c                   eval      V2Cmdr    = UNAmdr
054700070509     c                   eval      V2Cx25    = UNAx25
054800070509     c                   eval      V2Clsw    = UNAlsw
054900070509     c                   eval      V2Cksc    = UNAksc
055000070509     c                   move      UNArg2        V2Crg2
055100070417      *
055200070417     c                   ENDSR
055300130923      *
055400130923      *---------------------------------------------------------------*
055500130923      *?Impostazione attributi di vislualizzazione videata D02       ?*
055600130923      *---------------------------------------------------------------*
055700130923     c     sr_AttrVisD02 BEGSR
055800130923      *
055900130923      * -?Protezione "Numero Ordine"?
056000130923     c                   eval      *in41 = *off
056100130923     c*//                eval      *in41 = (§WIM12ord <> *zero  and
056200130923     c*//                                   Not *in78)
056300130923      * -?Protezione "Numero/Data Documento"?
056400130923     c                   eval      *in42 = *off
056500130923     c*//                eval      *in42 = (§WIM12doc <> *zero  and
056600130923     c*//                                   §WIM12ddo <> *zero  and
056700130923     c*//                                   Not *in76 and Not *in77)
056800130923      *
056900130923     c                   ENDSR
057000070417      *
057100070417      *---------------------------------------------------------------*
057200070417      *?Controllo dati immessi in videata D02                        ?*
057300070417      *---------------------------------------------------------------*
057400070417     c     CtrD02        BEGSR
057500070417      *
057600070417     c                   movea     *zeros        *in(50)
057700070417      *
057800070417      * Tipo unità errato
057900070417     c                   clear                   V2Dtip
058000070516if  1c*****              IF        V2Ctip   <> *blanks
058100070417     c                   eval      Wtip      = V2Ctip
058200070417     c                   exsr      Chk_Tip
058300070502sel 2c                   select
058400070502w   2c                   when      $Err      = *off
058500070418     c                   movel     T02ke1        V2Ctip
058600070417     c                   movel     d§UNTdes      V2Dtip
058700070418w   3c                   when      $Err      = *on
058800070502     c                   seton                                        2860
058900070509     c                   eval      V1Dmsg    = $Msg( 2)
059000070418w   3c                   when      $Err      = '2'
059100070502     c                   seton                                        2860
059200070509     c                   eval      V1Dmsg    = $Msg( 3)
059300070502e   2c                   endsl
059400070418if  2c                   if        *in90
059500070418     c                   leavesr
059600070418e   2c                   endif
059700070516e   1c*****              ENDIF
059800120412     c
059900130916      * Numero ordine obbligatorio
060000130916if  1c                   if        V2Cord   <= *zeros
060100120412     c                   eval      V1Dmsg    = $Msg(20)
060200120412     c                   seton                                        287890
060300120412     c                   leavesr
060400130916e   1c                   endif
060500070417      *
060600070417sel 1c***                Select
060700070417      * Marca obbligatoria
060800070417w   1c***                When      V2Cmar    = *blanks
060900070509     c***                eval      V1Dmsg    = $Msg( 4)
061000070417     c***                seton                                        286290
061100070417     c***                leavesr
061200070417      * Modello obbligatorio
061300070417w   1c***                When      V2Cmod    = *blanks
061400070509     c***                eval      V1Dmsg    = $Msg( 5)
061500070417     c***                seton                                        286390
061600070417     c***                leavesr
061700070417      * Matricola obbligatoria
061800070417w   1c*****              When      V2Cmat    = *blanks
061900070509     c*****              eval      V1Dmsg    = $Msg( 6)
062000070417     c*****              seton                                        286490
062100070417     c*****              leavesr
062200070417e   1c***                EndSl
062300070417      *
062400070417      * Codice con stesso modello e matricola già inserito
062500070502if  1c                   IF             V2Cmod <> *blanks
062600070417     c                             and  V2Cmat <> *blanks
062700070417     c                   eval      UNAmod    = V2Cmod
062800070417     c                   eval      UNAmat    = V2Cmat
062900070423     c     K02UNA08      setll     UNANA008
063000070417if  2c                   if        %equal(UNANA08L)
063100070509     c                   eval      V1Dmsg    = $Msg( 7)
063200070417     c                   seton                                        286390
063300070417     c                   leavesr
063400070417e   2c                   endif
063500070502e   1c                   ENDIF
063600070417      *
063700070417      * 2° livello di raggruppamento mancante o errato
063800070417     c                   clear                   V2Drg2
063900070417if  1c***                if             V2Crg2  = *blanks
064000070417     c***                          or   V2Crg2  = *zeros
064100070509     c***                eval      V1Dmsg    = $Msg( 8)
064200070417     c***                seton                                        286590
064300070417     c***                leavesr
064400070417e   1c***                endif
064500070417      ****
064600070502if  1c                   IF             V2Crg2 <> *blanks
064700070417     c                             and  V2Crg2 <> *zeros
064800070417     c                   eval      Wrg2      = V2Crg2
064900070417     c                   exsr      Chk_Rg2
065000070417if  2c                   if        $Err      = *off
065100070418     c                   eval      V2Crg2    = Wrg2
065200070417     c                   movel     T02uni        V2Drg2
065300070417x   2c                   else
065400070509     c                   eval      V1Dmsg    = $Msg( 8)
065500070417     c                   seton                                        286590
065600070417e   2c                   endif
065700070418if  2c                   if        *in90
065800070418     c                   leavesr
065900070418e   2c                   endif
066000070502e   1c                   ENDIF
066100070417      *
066200070417      * Data consegna mancante o errata
066300070423     c                   clear                   W2Cdcn
066400070417if  1c***                if        V2Cdcn    = *zeros
066500070509     c***                eval      V1Dmsg    = $Msg( 9)
066600070417     c***                seton                                        286690
066700070417     c***                leavesr
066800070417e   1c***                endif
066900070417      ****
067000070417if  1c                   IF        V2Cdcn   <> *zeros
067100070417     c                   eval      Wdata     = V2Cdcn
067200070417     c                   exsr      Chk_Dat
067300070417if  2c                   if        $Err      = *on
067400070509     c                   eval      V1Dmsg    = $Msg( 9)
067500070417     c                   seton                                        286690
067600070417     c                   leavesr
067700070417e   2c                   endif
067800070423     c                   eval      V2Cdcn    = Wdata
067900070423     c                   eval      W2Cdcn    = G08inv
068000070417e   1c                   ENDIF
068100070417      *
068200070417      * Data scadenza manutenzione mancante o errata
068300070423     c                   clear                   W2Cdsm
068400070417if  1c***                if        V2Cdsm    = *zeros
068500070509     c***                eval      V1Dmsg    = $Msg( 9)
068600070417     c***                seton                                        286790
068700070417     c***                leavesr
068800070417e   1c***                endif
068900070417      ****
069000070417if  1c                   IF        V2Cdsm   <> *zeros
069100070417     c                   eval      Wdata     = V2Cdsm
069200070417     c                   exsr      Chk_Dat
069300070417if  2c                   if        $Err      = *on
069400070509     c                   eval      V1Dmsg    = $Msg( 9)
069500070417     c                   seton                                        286790
069600070417     c                   leavesr
069700070417e   2c                   endif
069800070423     c                   eval      V2Cdsm    = Wdata
069900070423     c                   eval      W2Cdsm    = G08inv
070000070417e   1c                   ENDIF
070100070417      *
070200070417      * Data scadenza garanzia mancante o errata
070300070423     c                   clear                   W2Cdsg
070400070417if  1c***                if        V2Cdsg    = *zeros
070500070509     c***                eval      V1Dmsg    = $Msg( 9)
070600070417     c***                seton                                        286890
070700070417     c***                leavesr
070800070417e   1c***                endif
070900070417      ****
071000070417if  1c                   IF        V2Cdsg   <> *zeros
071100070417     c                   eval      Wdata     = V2Cdsg
071200070417     c                   exsr      Chk_Dat
071300070417if  2c                   if        $Err      = *on
071400070509     c                   eval      V1Dmsg    = $Msg( 9)
071500070417     c                   seton                                        286890
071600070417     c                   leavesr
071700070417e   2c                   endif
071800070423     c                   eval      V2Cdsg    = Wdata
071900070423     c                   eval      W2Cdsg    = G08inv
072000070417e   1c                   ENDIF
072100070417      *
072200070417      * Data scadenza leasing errata
072300070417     c                   clear                   W2Cdsl
072400070417if  1c                   IF        V2Cdsl   <> *zeros
072500070417     c                   eval      Wdata     = V2Cdsl
072600070417     c                   exsr      Chk_Dat
072700070417if  2c                   if        $Err      = *on
072800070509     c                   eval      V1Dmsg    = $Msg( 9)
072900070417     c                   seton                                        286990
073000070417     c                   leavesr
073100070417e   2c                   endif
073200070423     c                   eval      V2Cdsl   = Wdata
073300070423     c                   eval      W2Cdsl   = G08inv
073400070417e   1c                   ENDIF
073500070417      *
073600070417      * Ditta proprietà mancante o errata
073700070417     c                   clear                   V2Dsis
073800070417if  1c***                if        V2Csis    = *blanks
073900070509     c***                eval      V1Dmsg    = $Msg(10)
074000070417     c***                seton                                        287090
074100070417     c***                leavesr
074200070417e   1c***                endif
074300070417      ****
074400070417if  1c                   IF        V2Csis   <> *blanks
074500070417     c                   eval      Wmag      = V2Csis
074600070417     c                   exsr      Chk_Mag
074700070502if  2c                   if        $Err      = *off
074800070502     c                   eval      V2Csis    = Wmag
074900070502     c                   movel     WmagD         V2Dsis
075000070502x   2c                   else
075100070509     c                   eval      V1Dmsg    = $Msg(10)
075200070417     c                   seton                                        287090
075300070417e   2c                   endif
075400070418if  2c                   if        *in90
075500070418     c                   leavesr
075600070418e   2c                   endif
075700070417e   1c                   ENDIF
075800070417      *
075900070417      * Ditta leasing errata
076000070417     c                   clear                   V2Dlea
076100070417if  1c                   IF        V2Clea   <> *blanks
076200070417     c                   eval      Wmag      = V2Clea
076300070417     c                   exsr      Chk_Mag
076400070502if  2c                   if        $Err      = *off
076500070502     c                   eval      V2Clea    = Wmag
076600070502     c                   movel     WmagD         V2Dlea
076700070502x   2c                   else
076800070509     c                   eval      V1Dmsg    = $Msg(10)
076900070417     c                   seton                                        287190
077000070417e   2c                   endif
077100070418if  2c                   if        *in90
077200070418     c                   leavesr
077300070418e   2c                   endif
077400070417e   1c                   ENDIF
077500070417      *
077600070417      * Ditta manutenzione sede mancante o errata
077700070417     c                   clear                   V2Ddms
077800070417if  1c***                if        V2Cdms    = *blanks
077900070509     c***                eval      V1Dmsg    = $Msg(11)
078000070417     c***                seton                                        287290
078100070417     c***                leavesr
078200070417e   1c***                endif
078300070417      ****
078400070417if  1c                   IF        V2Cdms   <> *blanks
078500070417     c                   eval      Wmag      = V2Cdms
078600070417     c                   exsr      Chk_Mag
078700070502if  2c                   if        $Err      = *off
078800070502     c                   eval      V2Cdms    = Wmag
078900070502     c                   movel     WmagD         V2Ddms
079000070502x   2c                   else
079100070509     c                   eval      V1Dmsg    = $Msg(11)
079200070417     c                   seton                                        287290
079300070417e   2c                   endif
079400070418if  2c                   if        *in90
079500070418     c                   leavesr
079600070418e   2c                   endif
079700070417e   1c                   ENDIF
079800070417      *
079900070417      * Ditta manutenzione filiale mancante o errata
080000070417     c                   clear                   V2Ddmf
080100070417if  1c***                if        V2Cdmf    = *blanks
080200070509     c***                eval      V1Dmsg    = $Msg(11)
080300070417     c***                seton                                        287390
080400070417     c***                leavesr
080500070417e   1c***                endif
080600070417      ****
080700070417if  1c                   IF        V2Cdmf   <> *blanks
080800070417     c                   eval      Wmag      = V2Cdmf
080900070417     c                   exsr      Chk_Mag
081000070502if  2c                   if        $Err      = *off
081100070502     c                   eval      V2Cdmf    = Wmag
081200070502     c                   movel     WmagD         V2Ddmf
081300070502x   2c                   else
081400070509     c                   eval      V1Dmsg    = $Msg(11)
081500070417     c                   seton                                        287390
081600070502e   2c                   endif
081700070418if  2c                   if        *in90
081800070418     c                   leavesr
081900070418e   2c                   endif
082000070417e   1c                   ENDIF
082100070417      *
082200070417      * Magazzino ubicazione mancante o errato
082300070417     c                   clear                   V2Dmag
082400070417if  1c***                if        V2Cmag    = *blanks
082500070509     c***                eval      V1Dmsg    = $Msg(12)
082600070417     c***                seton                                        287490
082700070417     c***                leavesr
082800070417e   1c***                endif
082900070417      ****
083000070417if  1c                   IF        V2Cmag   <> *blanks
083100070417     c                   eval      Wmag      = V2Cmag
083200070417     c                   exsr      Chk_Mag
083300070502if  2c                   if        $Err      = *off
083400070502     c                   eval      V2Cmag    = Wmag
083500070502     c                   movel     WmagD         V2Dmag
083600070502x   2c                   else
083700070509     c                   eval      V1Dmsg    = $Msg(12)
083800070417     c                   seton                                        287490
083900070417e   2c                   endif
084000070418if  2c                   if        *in90
084100070418     c                   leavesr
084200070418e   2c                   endif
084300070417e   1c                   ENDIF
084400070417      *
084500070417      * Codice cliente mancante o errato
084600070417     c                   clear                   V2Dksc
084700070417      * - eventuale ricerca x ragione sociale
084800070423if  1c                   IF             V2Cksc <> *zeros
084900070417     c                             or   V2Dksc <> *blanks
085000070417     c                   eval      Wksc      = V2Cksc
085100070417     c                   eval      WkscD     = V2Dksc
085200070417     c                   exsr      Chk_Ksc
085300070502if  2c                   if        $Err      = *off
085400070502     c                   eval      V2Cksc    = Wksc
085500070502     c                   eval      V2Dksc    = WkscD
085600070502x   2c                   else
085700070509     c                   eval      V1Dmsg    = $Msg(13)
085800070417     c                   seton                                        287590
085900070417e   2c                   endif
086000070418if  2c                   if        *in90
086100070418     c                   leavesr
086200070418e   2c                   endif
086300070417e   1c                   ENDIF
086400070417      *
086500070417if  1c                   if             V2Cksc  = *zeros
086600070417     c                             and  V2Cmag  = '951'
086700070509     c                   eval      V1Dmsg    = $Msg(13)
086800070417     c                   seton                                        287590
086900070417     c                   leavesr
087000070417e   1c                   endif
087100070417      *
087200070417      * Numero documento mancante
087300130923if  1c                   if        V2Cdoc    = *zeros
087400130923     c                   eval      V1Dmsg    = $Msg(14)
087500130923     c                   seton                                        287690
087600130923     c                   leavesr
087700130923e   1c                   endif
087800070417      *
087900070417      * Data documento mancante o errata
088000070423     c                   clear                   W2Cddo
088100130923if  1c                   if        V2Cddo    = *zeros
088200130923     c                   eval      V1Dmsg    = $Msg(15)
088300130923     c                   seton                                        287790
088400130923     c                   leavesr
088500130923e   1c                   endif
088600130923      *
088700070502if  1c                   IF        V2Cddo   <> *zeros
088800070417     c                   eval      Wdata     = V2Cddo
088900070417     c                   exsr      Chk_Dat
089000070417if  2c                   if        $Err      = *on
089100100903     c                   eval      V1Dmsg    = $Msg(15)
089200070417     c                   seton                                        287790
089300070417     c                   leavesr
089400070417e   2c                   endif
089500070423     c                   eval      V2Cddo    = Wdata
089600070423     c                   eval      W2Cddo    = G08inv
089700070417e   1c                   ENDIF
089800070417      *
089900070417     c                   ENDSR
090000070417      *
090100070417      *---------------------------------------------------------------*
090200070417      *?Gestione tasto funzionale F06 da videata D02                 ?*
090300070417      *---------------------------------------------------------------*
090400070417     c     F06D02        BEGSR
090500070423      *
090600070423      * Memorizzazione dati di default in apposita ds
090700070423     c                   clear                   UNANAds
090800070423     c                   move      V2Ctip        DSAtip
090900070423     c                   eval      DSAcod    = V2Ccod
091000120508     c     c_MI:c_MA     xlate     V2Cmar        V2Cmar
091100070423     c                   eval      DSAmar    = V2Cmar
091200120508     c     c_MI:c_MA     xlate     V2Cmod        V2Cmod
091300070423     c                   eval      DSAmod    = V2Cmod
091400120508     c     c_MI:c_MA     xlate     V2Cmat        V2Cmat
091500070423     c                   eval      DSAmat    = V2Cmat
091600120508     c     c_MI:c_MA     xlate     V2CDES        V2Cdes
091700070423     c                   eval      DSAdes    = V2Cdes
091800120508     c     c_MI:c_MA     xlate     V2Cnot        V2Cnot
091900070423     c                   eval      DSAnot    = V2Cnot
092000070423     c                   eval      DSAdcn    = W2Cdcn
092100070423     c                   eval      DSAdsg    = W2Cdsg
092200070423     c                   eval      DSAsis    = V2Csis
092300070423     c                   move      V2Cdms        DSAdms
092400070423     c                   move      V2Cdmf        DSAdmf
092500070423     c                   eval      DSAdsm    = W2Cdsm
092600070423     c                   eval      DSAlea    = V2Clea
092700070423     c                   eval      DSAdsl    = W2Cdsl
092800070423     c                   move      V2Cmag        DSAmag
092900120508     c     c_MI:c_MA     xlate     V2Cemu        V2Cemu
093000120412     c                   eval      DSAemu    = %editc(v2cord:'X') + V2Cemu
093100120508     c     c_MI:c_MA     xlate     V2Cmvd        V2Cmvd
093200070423     c                   eval      DSAmvd    = V2Cmvd
093300120508     c     c_MI:c_MA     xlate     V2Cmta        V2Cmta
093400070423     c                   eval      DSAmta    = V2Cmta
093500120508     c     c_MI:c_MA     xlate     V2Cmdf        V2Cmdf
093600070423     c                   eval      DSAmdf    = V2Cmdf
093700120508     c     c_MI:c_MA     xlate     V2Cmra        V2Cmra
093800070423     c                   eval      DSAmra    = V2Cmra
093900120508     c     c_MI:c_MA     xlate     V2Cmdr        V2Cmdr
094000070423     c                   eval      DSAmdr    = V2Cmdr
094100120508     c     c_MI:c_MA     xlate     V2Cx25        V2Cx25
094200070423     c                   eval      DSAx25    = V2Cx25
094300120508     c     c_MI:c_MA     xlate     V2Clsw        V2Clsw
094400070423     c                   eval      DSAlsw    = V2Clsw
094500070423     c                   eval      DSAksc    = V2Cksc
094600070423     c                   move      V2Crg2        DSArg2
094700070423     c***                eval      DSAftr    = *blanks
094800070423     c***                eval      DSAdtr    = *zeros
094900070423     c***                eval      DSAatb    = *blanks
095000070502      *
095100070502     c                   clear                   UNMOVds
095200070502     c                   eval      DSMdoc    = V2Cdoc
095300070502     c                   eval      DSMddo    = W2Cddo
095400070417      *
095500070417      * Passaggio al subfile S01
095600070417     c                   eval      $Video    = 'S1'
095700070503     c                   if        wVideo   <> $Video
095800070417     c                   eval      $InzS01   = *on
095900070503     c                   endif
096000070503     c                   eval      wVideo    = 'D2'
096100070417      *
096200070417     c                   ENDSR
096300070417      *
096400070417      *---------------------------------------------------------------*
096500070417      *?Gestione tasto funzionale F12 da videata D02                 ?*
096600070417      *---------------------------------------------------------------*
096700070417     c     F12D02        BEGSR
096800070424      *
096900070424      * "Sblocco" di tutte le "Immissioni Multiple Unità"
097000070424      *   precedentemente bloccate e NON confermate
097100070424     c                   exsr      sr_Unlock_all
097200070424if  2c                   if        $Err      = *on
097300070424     c                   eval      $Video    = 'D1'
097400070424     c                   leavesr
097500070424e   2c                   endif
097600070424      *
097700070424if  1c                   IF        DUKnum    > *zeros
097800070424      * Chiusura del programma
097900070424     c                   eval      DUKret    = '2'
098000070424     c                   eval      $Fine     = *on
098100070424x   1c                   ELSE
098200070418      * Ritorno alla videata precedente
098300070418     c                   eval      $Video    = 'D1'
098400070424e   1c                   ENDIF
098500070417      *
098600070417     c                   ENDSR
098700070509      *
098800070509      *---------------------------------------------------------------*
098900070509      *?Gestione videata W01                                         ?*
099000070509      *---------------------------------------------------------------*
099100070509     c     GesW01        BEGSR
099200070509      *
099300070509      * Inizializzazione della videata
099400070509if  1c                   if        $InzW01   = *on
099500070509     c                   clear                   SDUKW01
099600070509     c                   eval      $InzW01   = *off
099700070509e   1c                   endif
099800070509      *
099900070509      * Emissione della window
100000070509     c                   exfmt     SDUKW01
100100070509     c                   setoff                                       28  90
100200070509     c                   clear                   V1Dmsg
100300070509      *
100400070509sel 1c                   SELECT
100500070509      * F4=Ricerca
100600070509w   1c                   WHEN      *inKD
100700070509     c                   exsr      F04W01
100800070509      * F12=Ritorno
100900070509w   1c                   WHEN      *inKL
101000070509     c                   eval      $Video    = 'D2'
101100070509      * Invio / F6=Conferma
101200070509x   1c                   OTHER
101300070509      * - Controllo dati immessi a video
101400070509     c                   exsr      CtrW01
101500070509      * - F6=Conferma
101600070509if  2c                   if        *inKF  and  NOT *in90
101700070509     c                   eval      $Video    = 'D2'
101800070509     c                   eval      $InzD02   = *on
101900070509e   2c                   endif
102000070509e   1c                   ENDSL
102100070509      *
102200070509     c                   ENDSR
102300070509      *
102400070509      *---------------------------------------------------------------*
102500070509      *?Gestione tasto funzionale F12 da videata W01                 ?*
102600070509      *---------------------------------------------------------------*
102700070509     c     F04W01        BEGSR
102800070509      *
102900070509      * Richiamo pgm. TNSDU1R per selezionare una matricola
103000070509     c                   clear                   UNANAds
103100070509     c                   clear                   dsUNIT
103200070509     c                   eval      DSUpro    = 'A'
103300070509     c                   eval      DSUop0    = ' 1'
103400070509      *
103500070509     c                   movel(p)  dsUNIT        KPJBU
103600070509     c                   call      'TNSDU1R'
103700070509     c                   parm                    KPJBA
103800070509     c                   parm                    UNANAds
103900070509     c                   movel     KPJBU         dsUNIT
104000070509      *
104100070509sel 1c                   SELECT
104200070509      * Ritorno normale
104300070509w   1c                   WHEN           DSUf03  = *off
104400070509     c                             and  DSUerr  = *off
104500070509     c                   eval      W1Ctip    = DSAtip
104600070509     c                   eval      W1Cmat    = DSAmat
104700070509     c                   eval      $Video    = 'D2'
104800070509     c                   eval      $InzD02   = *on
104900070509      * Ritorno con errore
105000070509w   1c                   WHEN      DSUmsg   <> *blanks
105100070509     c                   seton                                        285590
105200070509     c                   eval      V1Dmsg    = DSUmsg
105300070509      * Ritorno con F3
105400070509w   1c                   WHEN      DSUf03    = '1'
105500070509     c                   eval      *in55     = *on
105600070509     c                   eval      *in90     = *on
105700070509      * Ritorno con F12
105800070509w   1c                   WHEN      DSUf03    = '2'
105900070509     c                   eval      *in55     = *on
106000070509     c                   eval      *in90     = *on
106100070509e   1c                   ENDSL
106200070509      *
106300070509     c                   ENDSR
106400070509      *
106500070509      *---------------------------------------------------------------*
106600070509      *?Controllo dati immessi in videata W01                        ?*
106700070509      *---------------------------------------------------------------*
106800070509     c     CtrW01        BEGSR
106900070509      *
107000070509sel 1c                   select
107100070509      * Tipo unità obbligatorio
107200070509w   1c                   when      W1Ctip    = *zeros
107300070509     c                   seton                                        285590
107400070509     c                   eval      V1Dmsg    = $Msg(17)
107500070509      * Matricola obbligatoria
107600070509w   1c                   when      W1Cmat    = *zeros
107700070509     c                   seton                                        285690
107800070509     c                   eval      V1Dmsg    = $Msg(18)
107900070509      * Tipo e/o Matricola errati
108000070509x   1c                   other
108100070509     c     K02UNA02      chain     UNANA002
108200070509if  2c                   if        NOT  %found(UNANA02L)
108300070509     c                             or   UNAatb <> *blanks
108400070509     c                   seton                                        285590
108500070509     c                   eval      V1Dmsg    = $Msg(19)
108600070509e   2c                   endif
108700070509e   1c                   endsl
108800070509      *
108900070509     c                   ENDSR
109000120507      *
109100120507      *---------------------------------------------------------------*
109200120507      *?Gestione videata W02                                         ?*
109300120507      *---------------------------------------------------------------*
109400120507     c     GesW02        BEGSR
109500120507      *
109600120507      * Inizializzazione della videata
109700120507if  1c                   if        $InzW02   = *on
109800120507     c                   clear                   SDUKW02
109900120508     c                   eval      W2Dprg    = SAVprg1
110000120507     c                   eval      $InzW02   = *off
110100120507e   1c                   endif
110200120507      *
110300120507      * Emissione della window
110400120507     c                   exfmt     SDUKW02
110500120507     c                   setoff                                       28  90
110600120507     c                   clear                   V1Dmsg
110700120507      *
110800120507sel 1c                   SELECT
110900120507      * F12=Ritorno
111000120507w   1c                   WHEN      *inKL
111100120507     c                   eval      $Video    = 'S1'
111200120507      * Invio
111300120507x   1c                   OTHER
111400120507      * - Controllo dati immessi a video
111500120507     c                   exsr      CtrW02
111600120507      * - F6=Conferma
111700120507if  2c                   if        NOT *in90
111800120507     c                   exsr      F06W02
111900120507e   2c                   endif
112000120507e   1c                   ENDSL
112100120507      *
112200120507     c                   ENDSR
112300120507      *
112400120507      *---------------------------------------------------------------*
112500120507      *?Controllo dati immessi in videata W02                        ?*
112600120507      *---------------------------------------------------------------*
112700120507     c     CtrW02        BEGSR
112800120507      *
112900120507sel 1c                   select
113000120507      * Numero di matricole da selezionare x la conferma: obbligatorio
113100120507w   1c                   when      W2Cnms    = *zeros
113200120507     c                   seton                                        285790
113300120507     c                   eval      V1Dmsg    = $Msg(21)
113400120507      * Numero di matricole da selezionare x la conferma: negativo
113500120507w   1c                   when      W2Cnms    < *zeros
113600120507     c                   seton                                        285790
113700120507     c                   eval      V1Dmsg    = $Msg(22)
113800120507e   1c                   endsl
113900120507      *
114000120507      * Verifica opzioni eventualmente immesse PRIMA di premere F10
114100120507     c                   readc     SDUKS01
114200120507      *
114300120507do  1c                   DOW       NOT %eof(TNSDUKD)
114400120507      *
114500120507     c                   eval      *in32     = *off
114600120507     c                   movea     *zeros        *in(50)
114700120507     c                   z-add     S01nrr        C01csr
114800120507      *
114900120507sel 2c                   SELECT
115000120507      * - Nessuna opzione
115100120507w   2c                   WHEN      S1Copz    = *blanks
115200120507      * - Record già elaborato
115300120507     c                   WHEN      H1in40    = *on
115400120507      * - Opz. 1=Conferma "manuale" singola unità
115500120507      * - Opz. 4=Cancellazione singola unità (in subr. F06S01)
115600120507w   2c                   WHEN      S1Copz    = '1'  or
115700120507     c                             S1Copz    = '4'
115800120507if  3c                   if        S01nrr    > W2Cnms
115900120507     c                   seton                                        285090
116000120507     c                   eval      V1Dmsg    = $Msg(23)
116100120507e   3c                   endif
116200120507x   2c                   OTHER
116300120507     c                   seton                                        285090
116400120507     c                   eval      V1Dmsg    = $Msg(16)
116500120507e   2c                   ENDSL
116600120507      *
116700120507      * Aggiornamento sfl
116800120507     c                   eval      *in40     = H1in40
116900120507if  2c                   if        *in90
117000120507     c                   eval      C01csr    = S01nrr
117100120507     c                   eval      *in32     = *on
117200120507e   2c                   endif
117300120507      *                  _________________
117400120507     c                   UPDATE    SDUKS01
117500120507      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
117600120507if  2c                   if        *in28  OR  *in90
117700120507     c                   leave
117800120507e   2c                   endif
117900120507      *
118000120507     c                   readc     SDUKS01
118100120507      *
118200120507e   1c                   ENDDO
118300120507      *
118400120507     c                   ENDSR
118500120507      *
118600120507      *---------------------------------------------------------------*
118700120507      *?Gestione tasto funzionale F06 da videata W02                 ?*
118800120507      *---------------------------------------------------------------*
118900120507     c     F06W02        BEGSR
119000120507      *
119100120507      * Conferma PARZIALE del singolo inserimento multiplo unità
119200120507      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
119300120507     c                   eval      $Video    = 'S1'
119400120507     c                   eval      $Err      = *off
119500120507     c                   clear                   C01csr
119600120507      *
119700120507      * Ciclo di lettura subfile S01
119800120507do  1c     1             DO        W2Cnms        S01nrr
119900120507      *
120000120507     c     S01nrr        chain     SDUKS01
120100120507      * - E.o.F.
120200120507if  2c                   if        NOT %found(TNSDUKD)
120300120507     c                   leave
120400120507e   2c                   endif
120500120507     c                   eval      *in32     = *off
120600120507     c                   movea     *zeros        *in(50)
120700120507      *
120800120507      * - Conferma dei soli records NON "cancellati" (opz.4)
120900120507      *                           e NON già elaborati (*in40 = *on)
121000120507sel 2c                   select
121100120507     c                   when      H1in40    = *on
121200120507w   2c                   when      S1Copz    = *blanks   or
121300120507     c                             S1Copz    = '1'
121400120507     c                   exsr      OpzS01_1
121500120507w   2c                   when      S1Copz    = '4'
121600120507     c                   exsr      OpzS01_4
121700120507e   2c                   endsl
121800120507      *
121900120507      * - Aggiornamento sfl
122000120507     c                   eval      *in40     = H1in40
122100120507if  2c                   if        *in90
122200120507     c                   eval      C01csr    = S01nrr
122300120507     c                   eval      *in32     = *on
122400120507     c                   eval      $Err      = *on
122500120507e   2c                   endif
122600120507      *                  _________________
122700120507     c                   update    SDUKS01
122800120507      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
122900120507if  2c                   if        DSUf03    = '1'
123000120507     c*** già così:      eval      C01csr    = S01nrr
123100120507     c                   leavesr
123200120507e   2c                   endif
123300120507e   1c                   ENDDO
123400120507      *
123500120507     c                   if        $Err      = *off
123600120507      * Riemissione della videata S01
123700120507     c                   eval      $InzS01   = *on
123800120507     c                   eval      $InzW02   = *on
123900120507     c                   else
124000120507      * Riemissione di ciò che resta nella videata S01
124100120507     c                   eval      $InzS01   = *off
124200120507     c                   endif
124300120507      *
124400120507     c                   ENDSR
124500070417      *
124600070417      *---------------------------------------------------------------*
124700070417      *?Gestione videata S01                                         ?*
124800070417      *---------------------------------------------------------------*
124900070417     c     GesS01        BEGSR
125000070417      *
125100070417      * Inizializzazione della videata
125200070417if  1c                   if        $InzS01   = *on
125300120507     c                   clear                   SDUKW02
125400070417     c                   exsr      InzS01
125500070417     c                   eval      $InzS01   = *off
125600120507     c                   eval      $InzW02   = *on
125700070417e   1c                   endif
125800070417      *
125900070417      * Scrittura di testata e riga tasti funzionali abilitati
126000070417     c                   write     SDUKT01
126100070418     c                   write     SDUKZ03
126200070417      *
126300070417      * Visualizzazione (eventuale) del msg per la mancanza di dati
126400070417if  1c                   if        C01rcd   <= *zeros
126500070417     c                   write     SDUKD00
126600070417e   1c                   endif
126700070417      *
126800070417      * Emissione della videata
126900070417     c                   exfmt     SDUKC01
127000070417if  1c                   if        C01csr    > *zeros
127100070417     c                   eval      C01rcd    = C01csr
127200070417e   1c                   endif
127300070417     c                   setoff                                       28  90
127400070417     c                   clear                   V1Dmsg
127500070417      *
127600070417sel 1c                   SELECT
127700070417      * F3=Fine
127800070417w   1c                   WHEN      *inKC
127900070424     c                   exsr      F03S01
128000070417      * F5=Rivisualizzazione
128100070417w   1c                   WHEN      *inKE
128200070417     c                   exsr      F05S01
128300120507      * F10=Conferma Parziale
128400120507w   1c                   WHEN      *inKJ
128500120507     c                   eval      $InzW02   = *on
128600120507     c                   exsr      GesW02
128700120508if  2c*// inutile qui:   if        *in90
128800120508     c*// inutile qui:   leavesr
128900120508e   2c*// inutile qui:   endif
129000070418      * F12=Ritorno
129100070418w   1c                   WHEN      *inKL
129200070418     c                   exsr      F12S01
129300070417      * Roll-UP
129400070418w   1c***                WHEN      £Tasto    = C_RollUp
129500070418     c***                exsr      RollUpS01
129600070417      * NO rec.
129700070417w   1c                   WHEN      S01nrr   <= *zeros
129800070417      *
129900070418      * Invio / F6=Conferma
130000070417x   1c                   OTHER
130100070418      * - Gestione opzioni inserite
130200070417     c                   exsr      OpzS01
130300070502      * - F6=Conferma totale
130400070503if  2c                   IF        *inKF  and  NOT *in90
130500070418     c                   exsr      F06S01
130600070503e   2c                   ENDIF
130700070502if  2c                   if        *in90
130800070502     c                   leavesr
130900070502e   2c                   endif
131000070423      * - Cancellazione inserimenti multipli elaborati
131100070423     c                   eval      xx        = 1
131200070503do  2c                   DOW       xx       <= %elem($InMuUn)
131300070503if  3c                   If             $InMuUn(xx) <> *zeros
131400070502     c                             and  $RecIMU(xx) <= *zeros
131500070423     c                   clear                   TNSDUJds
131600070423     c                   eval      DUJopz    = 'D'
131700070423     c                   eval      DUJnum    = $InMuUn(xx)
131800070423     c                   exsr      Call_TNSDUJ1
131900070503if  4c                   if        *in90
132000070503     c                   eval      $InzD01   = *off
132100070503e   4c                   endif
132200070503e   3c                   EndIf
132300070423     c                   eval      xx        = xx + 1
132400070503e   2c                   ENDDO
132500070503      *
132600070417e   1c                   ENDSL
132700070417      *
132800070417     c                   ENDSR
132900070417      *
133000070417      *---------------------------------------------------------------*
133100070417      *?Inizializzazione videata S01                                 ?*
133200070417      *---------------------------------------------------------------*
133300070417     c     InzS01        BEGSR
133400070509      *
133500070509      * "Intestazione" del subfile-control
133600070509     c                   clear                   SDUKC01
133700070509     c                   exsr      Upd_Txt_C01
133800070417      *
133900070417      * Pulizia subfile
134000070417     c                   seton                                        3031
134100070417     c                   write     SDUKC01
134200070417     c                   setoff                                         3133
134300070417      *
134400070417     c                   reset                   $EoF
134500070417     c                   clear                   W_SflPag
134600070503     c*** Non serve:     clear                   C01csr
134700070417     c                   clear                   C01rcd
134800070417     c                   clear                   S01nrr
134900070417     c                   clear                   V1Dmsg
135000070417     c                   setoff                                       28  90
135100070417     c                   movea     *zeros        *in(50)
135200070420      *
135300070420     c                   clear                   xx
135400070420     c                   clear                   $InMuUn
135500070420     c                   clear                   $RecIMU
135600070424     c                   clear                   SAVnum
135700120508     c*//                clear                   SAVprg
135800120508     c                   clear                   SAVprg1
135900070509     c                   clear                   dWIMuni1
136000070417      *
136100070418      * Caricamento completo dei dati da presentare nel sfl
136200070509     c                   eval      WIMnum    = V1Cnum
136300070509     c                   eval      WIMtrk    = '1'
136400070509     c     K02WIM01      chain     WFAIM000
136500070509     c                   eval      $EoF      = (NOT %found(WFAIM01L))
136600070417if  1c                   if        $EoF      = *off
136700070417     c                   exsr      RollUpS01
136800070417e   1c                   endif
136900070503      *
137000070503      * Posizionamento iniziale del subfile sul 1° rec. nella 1ª pag.
137100070503if  1c                   if        NOT *in30
137200070503     c                   eval      C01rcd    = 1
137300070503e   1c                   endif
137400070418      *
137500070418      * Abilitazione tasti funzionali
137600070418      * - F6=Conferma (solo se ci sono dati nel sfl)
137700070418     c                   eval      *in06     = (NOT *in30)
137800070417      *
137900070417     c                   ENDSR
138000070417      *
138100070417      *---------------------------------------------------------------*
138200070417      *?Caricamento pagina successiva di dati S01                    ?*
138300070417      *---------------------------------------------------------------*
138400070417     c     RollUpS01     BEGSR
138500070417      *
138600070417     c                   eval      S01nrr    = (W_SflPag * C_SflPag)
138700070417     c                   add       1             W_SflPag
138800070417     c                   eval      *in32     = *off
138900070417      *
139000070417      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
139100070521do  1c                   DOW            $EoF    = *off
139200070418     c*** NO solo 1 pag.:          and  S01nrr  < W_SflPag * C_SflPag
139300070417     c                   exsr      CarS01
139400070509     c     V1Cnum        reade     WFAIM000
139500070509     c                   eval      $EoF      = (%eof(WFAIM01L))
139600070521      * - Si scartano i records già "confermati"
139700070521if  2c                   if             $Eof    = *off
139800070521     c                             and  WIMtrk  > '2'
139900070521     c     WIMnum        setgt     WFAIM000
140000070521     c                   eval      $EoF      = *on
140100070521e   2c                   endif
140200070521e   1c                   ENDDO
140300070417      *
140400070417      * Visualizzazione del SFL (se ci sono dati)
140500070417     c                   eval      *in30     = (S01nrr = *zeros)
140600070417      *
140700070417      * Attivazione (eventuale) del SFLEND
140800070417     c                   eval      *in33     = $EoF
140900070417      *
141000070417      * Posizionamento cursore al 1° rcd della pagina
141100070417if  1c                   if        S01nrr    > *zeros
141200070417     c     S01nrr        div       C_SflPag      wPag
141300070417     c                   mvr                     wRig
141400070417     c                   eval      C01rcd    = wPag * C_SflPag
141500070417     c                   if        wRig      > *zeros
141600070417     c                   eval      C01rcd    = C01rcd + 1
141700070417     c                   else
141800070417     c                   eval      C01rcd    = C01rcd - C_SflPag + 1
141900070417     c                   endif
142000070417x   1c                   else
142100070417     c                   clear                   C01rcd
142200070417e   1c                   endif
142300070417      *
142400070417     c                   ENDSR
142500070417      *
142600070417      *---------------------------------------------------------------*
142700070417      *?Caricamento dati in subfile S01                              ?*
142800070417      *---------------------------------------------------------------*
142900070417     c     CarS01        BEGSR
143000070417      *
143100070418      * Rottura Numero Inserimento Multiplo Unità
143200070509if  1c                   IF        WIMnum   <> SAVnum
143300070503     c                   exsr      sr_Lock_trk1
143400070418if  2c                   if        $Err      = *on
143500070509     c     WIMnum        setgt     WFAIM000
143600070418     c                   leavesr
143700070418e   2c                   endif
143800070420     c                   add       1             xx
143900070509     c                   eval      $InMuUn(xx) = WIMnum
144000070509     c                   eval      SAVnum    = WIMnum
144100120508     c*//                eval      SAVprg    = *hival
144200070418e   1c                   ENDIF
144300070418      *
144400070418      * Selezione dei soli trk. "2"
144500070518      *N.B.?- Altri trk NON vengono visualizzati ma
144600070518      *        VERRANNO CANCELLATI all'eventuale F6 di conferma.
144700070518if  1c                   if             WIMtrk <> '2'
144800070518     c                             and  WIMtrk <> '4'
144900070418     c                   leavesr
145000070418e   1c                   endif
145100070417      *
145200070417      * Caricamento dati nel record del sfl
145300070418     c                   clear                   SDUKS01
145400070420      * - Campi HIDDEN
145500070507     c                   eval      H1in40    = *off
145600070509      *
145700070418      * - Progressivo unità
145800070509     c                   eval      S1Cprg    = WIMprg
145900070509      * - Barcode caricato
146000070509     c                   eval      S1Cuni    = WIMuni
146100070509      * - Matricola unità
146200070509     c                   eval      S1Cmat    = %subst(WIMuni
146300070509     c                                              : §WIM1im2
146400070509     c                                              : §WIM1lm2)
146500070420      *
146600070420      * Impostazione attributi di visualizzazione
146700070420      * - Visualizzazione record
146800070507     c                   eval      *in40     = H1in40
146900070417      *
147000070417      * Scrittura record subfile
147100070417     c                   add       1             S01nrr
147200070418     c                   write     SDUKS01
147300070420      *
147400070420      * Conteggio record caricati per Inserimento Multiplo Unità
147500070420     c                   add       1             $RecIMU(xx)
147600070420      *
147700070420      * Memorizzazione ultimi campi "di controllo" utilizzati
147800120508     c*//                eval      SAVprg    = WIMprg
147900120508     c                   if        SAVprg1   = *zero
148000120508     c                   eval      SAVprg1   = WIMprg
148100120508     c                   endif
148200070417      *
148300070417     c                   ENDSR
148400070417      *
148500070417      *---------------------------------------------------------------*
148600070417      *?Gestione tasto funzionale F03 da videata S01                 ?*
148700070417      *---------------------------------------------------------------*
148800070417     c     F03S01        BEGSR
148900070424      *
149000070424      * "Sblocco" di tutte le "Immissioni Multiple Unità"
149100070424      *   precedentemente bloccate e NON confermate
149200070424     c                   exsr      sr_Unlock_all
149300070424if  2c                   if        $Err      = *on
149400070424     c                   eval      $Video    = 'D1'
149500070424     c                   leavesr
149600070424e   2c                   endif
149700070417      *
149800070417      * Chiusura del programma
149900070424     c                   eval      DUKret    = '1'
150000070417     c                   eval      $Fine     = *on
150100070417      *
150200070417     c                   ENDSR
150300070423      *
150400070423      *---------------------------------------------------------------*
150500070423      *?Gestione tasto funzionale F05 da videata S01                 ?*
150600070423      *---------------------------------------------------------------*
150700070423     c     F05S01        BEGSR
150800070423      *
150900070423      * Ricarica del subfile
151000070423     c                   eval      $InzS01   = *on
151100070423      *
151200070423     c                   ENDSR
151300070417      *
151400070417      *---------------------------------------------------------------*
151500070417      *?Gestione tasto funzionale F06 da videata S01                 ?*
151600070417      *---------------------------------------------------------------*
151700070417     c     F06S01        BEGSR
151800070417      *
151900070420      * Conferma TOTALE dei vari inserimenti multipli unità
152000070509      * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
152100070509     c                   eval      $Err      = *off
152200070516     c                   clear                   C01csr
152300070509      *
152400070420      * Ciclo di lettura subfile S01
152500070516do  1c                   DO        *hival        S01nrr
152600070420      *
152700070516     c     S01nrr        chain     SDUKS01
152800070420      * - E.o.F.
152900070420if  2c                   if        NOT %found(TNSDUKD)
153000070420     c                   leave
153100070420e   2c                   endif
153200070420     c                   eval      *in32     = *off
153300070420     c                   movea     *zeros        *in(50)
153400070420      *
153500070509      * - Conferma dei soli records NON "cancellati" (opz.4)
153600070516      *                           e NON già elaborati (*in40 = *on)
153700070420sel 2c                   select
153800070516     c                   when      H1in40    = *on
153900070420w   1c                   when      S1Copz    = *blanks
154000070503     c                   exsr      OpzS01_1
154100070420w   2c                   when      S1Copz    = '4'
154200070503     c                   exsr      OpzS01_4
154300070420e   2c                   endsl
154400070507      *
154500070507      * - Aggiornamento sfl
154600070507     c                   eval      *in40     = H1in40
154700070507if  2c                   if        *in90
154800070516     c                   eval      C01csr    = S01nrr
154900070507     c                   eval      *in32     = *on
155000070509     c                   eval      $Err      = *on
155100070507e   2c                   endif
155200070420      *                  _________________
155300070420     c                   update    SDUKS01
155400070420      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
155500070509if  2c                   if        DSUf03    = '1'
155600070516     c*** già così:      eval      C01csr    = S01nrr
155700070509     c                   leavesr
155800070509e   2c                   endif
155900070420e   1c                   ENDDO
156000070502      *
156100070509     c                   if        $Err      = *off
156200070503      * Ritorno alla videata D01
156300070503     c                   eval      $Video    = 'D1'
156400070503     c                   eval      $InzD01   = *on
156500070509     c                   else
156600070509      * Riemissione di ciò che resta nella videata S01
156700070509     c                   eval      $InzS01   = *off
156800070509     c                   endif
156900070417      *
157000070417     c                   ENDSR
157100070418      *
157200070418      *---------------------------------------------------------------*
157300070418      *?Gestione tasto funzionale F12 da videata S01                 ?*
157400070418      *---------------------------------------------------------------*
157500070418     c     F12S01        BEGSR
157600070418      *
157700070418      * Ritorno alla videata precedente
157800070503     c                   eval      wVideo    = $Video
157900070418     c                   eval      $Video    = 'D2'
158000070418      *
158100070418     c                   ENDSR
158200070417      *
158300070417      *---------------------------------------------------------------*
158400070417      *?Controllo opzioni subfile S01                                ?*
158500070417      *---------------------------------------------------------------*
158600070417     c     OpzS01        BEGSR
158700070417      *
158800070423     c                   readc     SDUKS01
158900070417      *
159000070423do  1c                   DOW       NOT %eof(TNSDUKD)
159100070417      *
159200070417     c                   eval      *in32     = *off
159300070417     c                   movea     *zeros        *in(50)
159400070516     c                   z-add     S01nrr        C01csr
159500070417      *
159600070417sel 2c                   SELECT
159700070417      * - Nessuna opzione
159800070417w   2c                   WHEN      S1Copz    = *blanks
159900120507      * - Record già elaborato
160000120507w   2c                   WHEN      H1in40    = *on
160100070420      * - Opz. 1=Conferma "manuale" singola unità
160200070420w   2c                   WHEN      S1Copz    = '1'
160300070503     c                   exsr      OpzS01_1
160400070420      * - Opz. 4=Cancellazione singola unità (in subr. F06S01)
160500070417w   2c                   WHEN      S1Copz    = '4'
160600070503     c*** Richiesto F6:  exsr      OpzS01_4
160700070417x   2c                   OTHER
160800070417     c                   seton                                        285090
160900070509     c                   eval      V1Dmsg    = $Msg(16)
161000070417e   2c                   ENDSL
161100070417      *
161200070417      * Aggiornamento sfl
161300070507     c                   eval      *in40     = H1in40
161400070417sel 2c                   select
161500070417w   2c                   when      *in28
161600070417     c                   eval      *in32     = *on
161700070509w   2c*** come se ok:    when      *in90     = *on
161800070509     c***  "   "   "     clear                   S1Copz
161900070502     c                   when      S1Copz    = '4'
162000070417x   2c                   other
162100070417     c                   clear                   S1Copz
162200070417e   2c                   endsl
162300070420     c                   UPDATE    SDUKS01
162400070417if  2c                   if        *in28  OR  *in90
162500070417     c                   leave
162600070417e   2c                   endif
162700070417      *
162800070420     c                   readc     SDUKS01
162900070417      *
163000070417e   1c                   ENDDO
163100070417      *
163200070417     c                   ENDSR
163300070417      *
163400070417      *---------------------------------------------------------------*
163500070420      *?Conferma del singolo rec. "inserimento multiplo unità"       ?*
163600070417      *---------------------------------------------------------------*
163700070503     c     OpzS01_1      BEGSR
163800070417      *
163900070423      * GIÀ ESEGUITO:
164000070423      * 1) "Blocco" dell'inserimento multiplo unità             (trk.1)
164100070423      * DA ESEGUIRE:
164200070423      * 2) Conferma dell'inserimento multiplo unità in UNANA    (trk.2)
164300070509      * 3) Cancellaz. dell'inserimento multiplo unità da WFAIM  (trk.2)
164400070518      *    => in realtà imposta trk. "4" nel record (appena elaborato)
164500070423      *
164600070423     c*** già impostata: reset                   UNANAds
164700070423     c                   reset                   dsUNIT
164800070509if  1c                   if        S1Copz    = *blanks
164900070509     c                   eval      DSUop1    = 'F6N'
165000070509e   1c                   endif
165100070509     c                   movel(p)  dsUNIT        KPJBU
165200120508     c     c_MI:c_MA     xlate     S1Cmat        S1Cmat
165300070509     c                   movel     S1Cmat        DSAmat
165400070423      *
165500070503     c                   call      'TNSDU2R'
165600070423     c                   parm                    KPJBA
165700070423     c                   parm                    UNANAds
165800070502     c                   parm                    UNMOVds
165900070423     c                   movel     KPJBU         dsUNIT
166000070423      *
166100070423sel 1c                   SELECT
166200070423      * Ritorno normale
166300070423w   1c                   WHEN           DSUf03  = *off
166400070423     c                             and  DSUerr  = *off
166500070503     c                   exsr      OpzS01_4
166600070503     c                   eval      $InzS01   = *on
166700070423      * Ritorno con errore
166800070423w   1c                   WHEN      DSUmsg   <> *blanks
166900070518     c***                eval      *in32     = *on
167000070509     c                   seton                                        285090
167100070423     c                   eval      V1Dmsg    = DSUmsg
167200070423      * Ritorno con F3
167300070423w   1c                   WHEN      DSUf03    = '1'
167400070518     c***                eval      *in32     = *on
167500070423     c                   eval      *in50     = *on
167600070423     c                   eval      *in90     = *on
167700070423      * Ritorno con F12
167800070423w   1c                   WHEN      DSUf03    = '2'
167900070518     c                   eval      *in32     = *on
168000070502     c                   eval      *in50     = *on
168100070518     c***                eval      *in90     = *on
168200070423e   1c                   ENDSL
168300070417      *
168400070417     c                   ENDSR
168500070420      *
168600070420      *---------------------------------------------------------------*
168700070518      *?Impostaz. trk.4 nel singolo rec. "inserimento multiplo unità"?*
168800070420      *---------------------------------------------------------------*
168900070503     c     OpzS01_4      BEGSR
169000070420      *
169100070423      * GIÀ ESEGUITO:
169200070423      * 1) "Blocco" dell'inserimento multiplo unità             (trk.1)
169300070518      * 2) Conferma dell'inserimento multiplo unità in UNANA    (trk.2)
169400070423      * DA ESEGUIRE:
169500070518      * 3) Cancellaz. dell'inserimento multiplo unità da WFAIM  (trk.2)
169600070518      *    => in realtà imposta trk. "4" nel record (appena elaborato)
169700070503     c                   clear                   TNSDUJds
169800070503     c                   eval      DUJopz    = '4'
169900070509     c                   eval      DUJnum    = V1Cnum
170000070503     c                   eval      DUJprg    = S1Cprg
170100070424     c                   exsr      Call_TNSDUJ2
170200070423if  1c                   if             DUJerr <> *off
170300070423     c                             or   DUJret <> *off
170400070423     c                   leavesr
170500070423e   1c                   endif
170600070503      *
170700070503      * Aggiornamento n° unità da inserire
170800070503     c                   eval      *in25     = *off
170900070503     c                   eval      xx        = 1
171000070509     c     V1Cnum        lookup    $InMuUn(xx)                            25
171100070503     c                   if        *in25
171200070503     c                   sub       1             $RecIMU(xx)
171300070503     c                   endif
171400070420      *
171500070503      * Aggiornamento rec. del subfile
171600070509     c                   eval      H1in40    = *on
171700070420      *
171800070420     c                   ENDSR
171900070509      *
172000070509      *---------------------------------------------------------------*
172100070509      *?Aggiornamento testata in C01 con nuovi dati restituiti da R1 ?*
172200070509      *---------------------------------------------------------------*
172300070509     c     Upd_Txt_C01   BEGSR
172400070509      *
172500070509     c                   eval      C1Dnum    = V1Cnum
172600130923     c                   eval      C1Ddes    = §WIM1des + §WIM12des
172700070509     c                   eval      C1Dnmi    = §WIM1nmi
172800070509      *
172900070509     c                   ENDSR
173000070417      *
173100070417      *---------------------------------------------------------------*
173200070509      *?Richiamo del pgm. TNSDUJR1 (gestione trk. "1" file WFAIM00F) ?*
173300070417      *---------------------------------------------------------------*
173400070417     c     Call_TNSDUJ1  BEGSR
173500070417      *
173600070417     c                   movel(p)  TNSDUJds      KPJBU
173700070417     c                   call      'TNSDUJR1'
173800070417     c                   parm                    KPJBA
173900070417     c                   movel     KPJBU         TNSDUJds
174000070417      *
174100070417sel 1c                   SELECT
174200070417      * Ritorno normale
174300070417w   1c                   WHEN           DUJret  = *off
174400070417     c                             and  DUJerr  = *off
174500070417      * Ritorno con errore
174600070417w   1c                   WHEN      DUJerr    = *on
174700070417     c                   seton                                        28  90
174800070417     c                   eval      V1Dmsg    = DUJmsg
174900070417      * Ritorno con F3
175000070417w   1c                   WHEN      DUJret    = '1'
175100070417     c                   eval      *in90     = *on
175200070417      * Ritorno con F12
175300070417w   1c                   WHEN      DUJret    = '2'
175400070417     c                   eval      *in90     = *on
175500070417      *
175600070417e   1c                   ENDSL
175700070417      *
175800070417     c                   ENDSR
175900070503      *
176000070503      *---------------------------------------------------------------*
176100070503      *?Richiamo del pgm. TNSDUJR2 (cancellazione singolo trk. "2")  ?*
176200070503      *---------------------------------------------------------------*
176300070503     c     Call_TNSDUJ2  BEGSR
176400070503      *
176500070503     c                   movel(p)  TNSDUJds      KPJBU
176600070503     c                   call      'TNSDUJR2'
176700070503     c                   parm                    KPJBA
176800070503     c                   movel     KPJBU         TNSDUJds
176900070503      *
177000070503sel 1c                   SELECT
177100070503      * Ritorno normale
177200070503w   1c                   WHEN           DUJret  = *off
177300070503     c                             and  DUJerr  = *off
177400070503      * Ritorno con errore
177500070503w   1c                   WHEN      DUJerr    = *on
177600070509     c                   eval      *in32     = *on
177700070509     c                   seton                                        285090
177800070503     c                   eval      V1Dmsg    = DUJmsg
177900070503      * Ritorno con F3
178000070503w   1c                   WHEN      DUJret    = '1'
178100070509     c                   eval      *in32     = *on
178200070503     c                   eval      *in90     = *on
178300070503      * Ritorno con F12
178400070503w   1c                   WHEN      DUJret    = '2'
178500070509     c                   eval      *in32     = *on
178600070503     c                   eval      *in90     = *on
178700070503      *
178800070503e   1c                   ENDSL
178900070503      *
179000070503     c                   ENDSR
179100070418      *
179200070418      *---------------------------------------------------------------*
179300070418      *?Verifica disponibilità dell'inserimento multiplo da trk "1"  ?*
179400070420      *?  e "allocazione" dello stesso.                              ?*
179500070418      *---------------------------------------------------------------*
179600070503     c     sr_Lock_trk1  BEGSR
179700070418      *
179800070424     c                   eval      $Err      = *off
179900070424      *
180000070418      * Il primo record del nuovo inserimento multiplo unità DEVE
180100070518      *   essere trk. "1" (se da confermare) o "3" (se già confermato)
180200070509     c                   clear                   dWIMuni1
180300070518if  1c                   if             WIMtrk = '1'
180400070518     c                             or   WIMtrk = '3'
180500070509     c                   movel     WIMuni        dWIMuni1
180600070418x   1c                   else
180700070509     c                   eval      §WIM1des  = *all'? '
180800070509     c                   eval      §WIM1usp  = *all'? '
180900070418e   1c                   endif
181000070418      *
181100070509      * Gestione rec. trk "1" del file WFAIM00F per
181200070418      * "Blocco" dell'inserimento multiplo unità x conferma
181300070418if  1c                   IF        V1Cnum    = *zeros
181400070418     c                   clear                   TNSDUJds
181500070418     c                   eval      DUJopz    = '9'
181600070509     c                   eval      DUJnum    = WIMnum
181700070418     c                   exsr      Call_TNSDUJ1
181800070418      * - Gli inserimenti multipli unità impegnati vengono scartati
181900070503if  2c                   if             DUJerr <> *off
182000070418     c                             or   DUJret <> *off
182100070418     c                   eval      $Err      = *on
182200070418     c                   leavesr
182300070503e   2c                   endif
182400070418e   1c                   ENDIF
182500070418      *
182600070420      * Eventuale "sistemazione dei dati del trk. "1"
182700070509if  1c                   if        §WIM1im2 <= *zeros
182800070509     c                   eval      §WIM1im2  = 1
182900070418e   1c                   endif
183000070509if  1c                   if             §WIM1lm2 <= *zeros
183100070509     c                             or   §WIM1lm2  > %len(UNAmat)
183200070509     c                   eval      §WIM1lm2  = %len(UNAmat)
183300070418e   1c                   endif
183400070418      *
183500070418     c                   ENDSR
183600070423      *
183700070423      *---------------------------------------------------------------*
183800070423      *?"Disallocazione" di tutti gli inserimenti multipli unità NON ?*
183900070423      *?  confermati e NON cancellati                                ?*
184000070423      *---------------------------------------------------------------*
184100070423     c     sr_Unlock_all BEGSR
184200070423      *
184300070424     c                   eval      $Err      = *off
184400070424      *
184500070509      * Gestione rec. trk "1" del file WFAIM00F per
184600070423      * "Blocco" dell'inserimento multiplo unità x conferma
184700070423     c                   eval      xx        = 1
184800070423do  1c                   DOW       xx       <= %elem($InMuUn)
184900070423      *
185000070502if  2c                   IF             $InMuUn(xx) <> *zeros
185100070502     c***                          and  $RecIMU(xx)  > *zeros
185200070423     c                   clear                   TNSDUJds
185300070423     c                   eval      DUJopz    = '8'
185400070423     c                   eval      DUJnum    = $InMuUn(xx)
185500070423     c                   exsr      Call_TNSDUJ1
185600070423      * - Gli inserimenti multipli unità impegnati vengono scartati
185700070423if  2c                   If             DUJerr <> *off
185800070423     c                             or   DUJret <> *off
185900070424     c                   eval      $Err      = *on
186000070502     c*** NO, finisco:   leavesr
186100070502x   2c                   Else
186200070502     c                   clear                   $InMuUn(xx)
186300070502     c                   clear                   $RecIMU(xx)
186400070423e   2c                   EndIf
186500070423e   2c                   ENDIF
186600070423      *
186700070423     c                   eval      xx        = xx + 1
186800070423e   1c                   ENDDO
186900070423      *
187000070423     c                   ENDSR
187100070416      *
187200070416      *---------------------------------------------------------------*
187300070416      *?Controllo del Tipo di Unità                                  ?*
187400070416      *---------------------------------------------------------------*
187500070416     c     Chk_Tip       BEGSR
187600070416      *
187700070416     c                   eval      $Err      = *off
187800070416     c                   clear                   dUNT
187900070416      *
188000070416      * Ricerca
188100070417if  1c                   if        %scan('?':%trimr(Wtip)) > *zeros
188200070416     c                   clear                   TIBS02ds
188300070416     c                   eval      T02mod    = 'R'
188400070416     c                   eval      T02sif    = KNSIF
188500070416     c                   eval      T02cod    = 'UNT'
188600070416     c                   call      'TIBS02R'
188700070416     c                   parm                    KPJBA
188800070416     c                   parm                    TIBS02ds
188900070417     c                   movel     T02ke1        Wtip
189000070416     c                   movel     T02uni        dUNT
189100070418if  2c                   if        T02ke1   <> *blanks
189200070418     c                   eval      *in90     = *on
189300070418     c                   leavesr
189400070418e   2c                   endif
189500070417e   1c                   endif
189600070416      *
189700070416      * Controllo
189800070416     c                   clear                   TIBS02ds
189900070416     c                   eval      T02mod    = 'C'
190000070416     c                   eval      T02sif    = KNSIF
190100070416     c                   eval      T02cod    = 'UNT'
190200070418     c                   movel(p)  Wtip          T02ke1
190300070416     c                   call      'TIBS02R'
190400070416     c                   parm                    KPJBA
190500070416     c                   parm                    TIBS02ds
190600070416      *
190700070416     c                   eval      $Err      = (T02err <> *blanks)
190800070416if  1c                   if        $Err      = *off
190900070416     c                   movel     T02uni        dUNT
191000070418x   1c                   else
191100070418     c                   eval      d§UNTdes  = *all'? '
191200070416e   1c                   endif
191300070416      *
191400070416     c                   ENDSR
191500070416      *
191600070416      *---------------------------------------------------------------*
191700070416      *?Controllo del secondo livello di raggruppamento              ?*
191800070416      *---------------------------------------------------------------*
191900070416     c     Chk_Rg2       BEGSR
192000070416      *
192100070416     c                   eval      $Err      = *off
192200070423     c*** NON c'è:       clear                   dUNR
192300070416      *
192400070416      * Ricerca
192500070423if  1c                   if        %scan('?':%trimr(Wrg2)) > *zeros
192600070416     c                   clear                   TIBS02ds
192700070416     c                   eval      T02mod    = 'R'
192800070416     c                   eval      T02sif    = KNSIF
192900070416     c                   eval      T02cod    = 'UNR'
193000070416     c                   call      'TIBS02R'
193100070416     c                   parm                    KPJBA
193200070416     c                   parm                    TIBS02ds
193300070417     c*** NON c'è:       movel     T02uni        dUNR
193400070418     c                   movel     T02ke1        Wrg2
193500070418if  2c                   if        T02ke1   <> *blanks
193600070418     c                   eval      *in90     = *on
193700070418     c                   leavesr
193800070418e   2c                   endif
193900070417e   1c                   endif
194000070416      * Controllo
194100070416     c                   clear                   TIBS02ds
194200070416     c                   eval      T02mod    = 'C'
194300070416     c                   eval      T02sif    = KNSIF
194400070416     c                   eval      T02cod    = 'UNR'
194500070418     c                   movel(p)  Wrg2          T02ke1
194600070416     c                   call      'TIBS02R'
194700070416     c                   parm                    KPJBA
194800070416     c                   parm                    TIBS02ds
194900070416      *
195000070416     c                   eval      $Err      = (T02err <> *blanks)
195100070416if  1c                   if        $Err      = *off
195200070417     c*** NON c'è:       movel     T02uni        dUNR
195300070418x   1c                   else
195400070418     c                   eval      T02uni    = *all'? '
195500070416e   1c                   endif
195600070416      *
195700070416     c                   ENDSR
195800070416      *
195900070416      *---------------------------------------------------------------*
196000070416      *?Controllo del magazzino di ubicazione                        ?*
196100070416      *---------------------------------------------------------------*
196200070416     c     Chk_Mag       BEGSR
196300070416      *
196400070416     c                   eval      $Err      = *off
196500070416     c                   clear                   WmagD
196600070416      *
196700070416      * Ricerca
196800070417if  1c                   if        %scan('?':%trimr(Wmag)) > *zeros
196900070416     c                   call      'TNSD20R'
197000070416     c                   parm      *blanks       §SD20cod          3
197100070416     c                   parm      *blanks       §SD20tip          1
197200070416     c                   parm      *blanks       §SD20des         25
197300070416     c                   move      §SD20cod      Wmag
197400070416     c                   movel(p)  §SD20des      WmagD
197500070418if  2c                   if        §SD20cod <> *blanks
197600070418     c                   eval      *in90     = *on
197700070418     c                   leavesr
197800070418e   2c                   endif
197900070417e   1c                   endif
198000070416      *
198100070416      * Controllo
198200070416     c                   move      Wmag          ORGfil
198300070416     c                   clear                   ORGdes
198400070416     c     ORGfil        chain     AZORG020
198500070417if  1c                   if        NOT  %found(AZORG21L)
198600070416     c     ORGfil        chain     AZORG
198700070417e   1c                   endif
198800070502     c                   eval      $Err      = (NOT %found(AZORG21L) and
198900070502     c                                          NOT %found(AZORG01L) )
199000070416if  1c                   if        $Err      = *off
199100070416     c                   movel     ORGdes        WmagD
199200070416x   1c                   else
199300070416     c                   eval      WmagD     = *all'? '
199400070416e   1c                   endif
199500070416      *
199600070416     c                   ENDSR
199700070416      *
199800070416      *---------------------------------------------------------------*
199900070416      *?Controllo della data                                         ?*
200000070416      *---------------------------------------------------------------*
200100070416     c     Chk_Dat       BEGSR
200200070416      *
200300070416     c                   eval      $Err      = *off
200400070416      *
200500070423     c                   clear                   WLBdat
200600070416     c                   eval      G08dat    = Wdata
200700070416     c                   call      'XSRDA8'
200800070416     c                   parm                    WLBdat
200900070416      *
201000070416     c                   eval      Wdata     = G08dat
201100070416     c                   eval      $Err      = (G08err <> *off)
201200070416      *
201300070416     c                   ENDSR
201400070416      *
201500070416      *---------------------------------------------------------------*
201600070416      *?Controllo del codice cliente                                 ?*
201700070416      *---------------------------------------------------------------*
201800070418     c     Chk_Ksc       BEGSR
201900070416      *
202000070416      * Ricerca
202100070416      * Se non inserito il codice ma inserita una parte della rag.soc.
202200070416      *   si richiama il programma della ricerca alfabetica
202300070417if  1c                   IF             Wksc    = *zeros
202400070417     c                             and  WkscD  <> *blanks
202500070416     c                   call      'XALFA3BR'
202600070416     c                   parm      RSUT          PAXdut           30
202700070416     c                   parm      1             PAXut             1 0
202800070418     c                   parm      WkscD         PAXdmt           48
202900070416     c                   parm      DUTkci        PAXccc            4 0
203000070416     c                   parm      9             PAXsta            1 0
203100070416     c                   parm      *blanks       PAXflr           90
203200070416     c                   parm      *blanks       PAXdit            3
203300070416     c                   parm      1             PAXnum            2 0
203400070416     c                   parm      *blanks       PAXkcm           80
203500070416     c                   parm      *blanks       PAXksm          140
203600070416     c                   parm      *blanks       PAXkdm           60
203700070416     c                   parm      'S'           PAXesci           1
203800070416     c                   parm      *blanks       PAXerr            2
203900070416     c***                parm      *blanks       PAXpiv           16
204000070416     c***                parm      *blanks       PAXcdf           16
204100070416      *
204200070416     c                   eval      $Err      = (PAXsta < *zeros)
204300070417if  2c                   if        $Err      = *off
204400070417     c                   movel     PAXksm        Wksc
204500070418     c                   movel     PAXdmt        WkscD
204600070417e   2c                   endif
204700070418if  2c                   if        PAXksm   <> *blanks
204800070418     c                   eval      *in90     = *on
204900070418     c                   leavesr
205000070418e   2c                   endif
205100070417e   1c                   ENDIF
205200070416      *
205300070416      * Controllo
205400070423if  1c                   IF        Wksc     <> *zeros
205500070416     c                   clear                   TIBS69ds
205600070417     c                   eval      I69kac    = Wksc
205700070416     c                   call      'TIBS69R'
205800070416     c                   parm                    TIBS69ds
205900070416     c                   parm                    ds_CNACO
206000070416     c                   parm                    ds_CNIND
206100070416     c                   parm                    ds_CNCLP
206200070416     c                   parm                    ds_FNCLS
206300070416      *
206400070416     c                   eval      $Err      = (O69err = *on)
206500070423if  2c                   if        $Err      = *off
206600070418     c                   movel     ACOrag        WkscD
206700070423e   2c                   endif
206800070423e   1c                   ENDIF
206900070418      *
207000070416     c                   ENDSR
207100070417      *
207200070417      *?Schiere a tempo di compilazione - - - - - - - - - - - - - - -?*
207300070417      *
207400070417** - $Msg -------------------------------------------------------------------*
207500070509Numero Inserimento Multiplo Unità obbligatorio                                 1
207600070509Tipo unità non accettabile.                                                    2
207700070509Hai indicato il tipo di PC ???????????????????                                 3
207800070509Marca unità non accettabile.                                                   4
207900070509Modello unità non accettabile.                                                 5
208000070509Matricola unità non accettabile.                                               6
208100070509Esiste già una unità con lo stesso modello e la stessa matricola.              7
208200070509Secondo livello di raggruppamento non accettabile.                             8
208300070509Data non accettabile.                                                          9
208400070509Ditta non accettabile.                                                        10
208500070509Ditta manutenzione non accettabile.                                           11
208600070509Magazzino ubicazione non accettabile.                                         12
208700070509Cliente non accettabile.                                                      13
208800070509Numero documento obbligatorio in inserimento.                                 14
208900070509Data documento obbligatoria in inserimento.                                   15
209000070509Opzione errata                                                                16
209100070509Tipo unità obbligatorio                                                       17
209200070509Matricola obbligatoria                                                        18
209300070509Tipo e/o Matricola errati                                                     19
209400120412Numero ordine obbligatorio                                                    20
209500120507Numero di matricole da selezionare per la conferma obbligatorio               21
209600120507Numero di matricole da selezionare per la conferma errato (negativo)          22
209700120507NON ammesse opzioni per matricole sequenzialmente oltre il massimo elaborabile23
