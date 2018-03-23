000100160506     /*END
000200160506
000300031013     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400030729
000500080701      * FIMS76R *----------------------------------------------------*
000600080701      *         Scrive da FNARB a FITR600F   per Picking su Nastro   *
000700030729      *--------------------------------------------------------------*
000800030729
000900030930     fFNARB25L  if   e           k disk
001000030729     fFIAPD01L  if   e           k disk
001100030930     fFLTR901L  if   e           k disk
001200040318     fFNFVV03L  if   e           k disk
001300050331     fTigcp21L  if   e           k disk
001400030729      *
001500060217     fFiAR401L  if   e           k disk    usropn
001600030729     fFNART01L  if   e           k disk    usropn
001700030729      *
001800040318     fFNLBL01L  if   e           k disk
001900040319     fFNLBL02L  if   e           k disk    rename(FNLBL000:FNLBL002)
002000040319     fFNARB01L  if   e           k disk    rename(FNARB000:FNARBLEG)
002100040318      *
002200030729     fTABEL00F  if   e           k disk
002300030729     fTNTBE01L  if   e           k disk
002400030729     fAZORG01L  if   e           k disk
002500030729      *
002600080701     fFITR600F  o    e             disk
002700030729
002800030729      *---------------------------------------------------------------*
002900030729
003000030729      *
003100030729      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
003200030729      *
003300060214      * P.O. da gestire come NO VdL
003400060214     d C_PO_a          c                   const(006)
003500040310      * Numero di Baie/Postazioni elaborabili da FLTR900F
003600040518     d C_MaxRecSf      c                   const(300)
003700040310      * Numero di Fogli Viaggio   elaborabili da FLTR900F
003800040310      *   (deve essere doppio di C_MaxRecSf)
003900040518     d C_MaxRecPf      c                   const(600)
004000030729      *
004100030729      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004200030729      *
004300030729      * - P.O. gestiti: elenco codici
004400030730     d L6              s                   like(ARBlna)  dim(30)  inz
004500030729      * - P.O. gestiti: network corrispondente
004600030730     d L6ntw           s                   like(§OGntw)  dim(30)  inz
004700031001      *
004800031013      * - Num. F.V. previsti per il piano di smistamento picking
004900040310     d $TR9nfv         s                   like(TR9nfv)  dim(C_MaxRecPf)
005000031001     d                                     inz(*hival)
005100040310     d $TR9rec         s                   like(FLTR9ds) dim(C_MaxRecPf)
005200040310     d                                     inz
005300030729      *
005400030729      * - P.O. elaborati: elenco codici
005500031013     d $PO             s                   like(ARBlna)  dim(250) inz
005600030729      * - P.O. elaborati: dati corrispondenti
005700031013     d $POd            s                   like(AZORGds) dim(250) inz
005800030729      *
005900030729      * - Autotrasportatori elaborati: elenco codici
006000040310     d $PDC            s                   like(APDpdr)  dim(C_MaxRecSf)
006100030729     d                                     inz(*hival)
006200030729      * - Autotrasportatori elaborati: dati corrispondenti
006300040310     d $PDCd           s                   like(APDrsc)  dim(C_MaxRecSf)
006400040310     d                                     inz
006500040413      *
006600040414      * - Baie x Postazioni "IM*" (IMA/IM0/IM1/...)
006700040413     d $PosIMx         s                   like(TR6bai)  dim(20)  inz
006800040413     d $BauIMx         s                   like(TR6bau)  dim(99)  inz
006900040414     d $PosBau         s            198                  dim(20)  inz           99x20
007000040413     d $LastBau        s              3  0               dim(99)  inz
007100030729      *
007200030729      * - Tab.3A = Codici bolla: elenco codici elaborati
007300030729     d $3A             s                   like(ARBcbo)  dim(50)
007400030729     d                                     inz(*hival)
007500030729      * - Tab.3A = Codici bolla: dati corrispondenti
007600030729     d $3Ads           s                   like(ds3A)    dim(50)  inz
007700040406      *
007800040406      * - Tab.TB = Tipi bolla: elenco tipi con raggruppamento bolle "C"
007900040406     d $TBrC           s                   like(§3Atb2)  dim(50)
008000040406     d                                     inz(*hival)
008100030729      *
008200030729      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
008300030729      *
008400030729     d Status         sds
008500030729     d   SDSpgm          *proc                                                  Procedure name
008600030729     d   SDSpar          *parms                                                 Num passed parms
008700030729     d   SDSjob              244    253                                         Job name
008800030729     d   SDSusr              254    263                                         User name
008900030729     d   SDSjnr              264    269s 0                                      Job number
009000030729      *
009100030729     d KPJBA         e ds                  inz
009200030729      * - Parametri per pgm. di caricamento schiera p.o.
009300030729     d Trul06ds      e ds                  inz
009400030730     d   D06cod      e                     inz('£6')
009500030729     d   D06tla      e                     inz('L')
009600040310     d   lia                   1     90  0 inz      dim(30)
009700030729      * - Parametri per pgm. di calcolo check-digit per bar-code
009800030729     d Trul28ds      e ds                  inz
009900030729     d   I28mod      e                     inz('BAR')
010000040310      * - Parametri x Interrogazione tabelle
010100040310     d TIBS02DS      e ds                  inz
010200040310     d   T02mod      e                     inz('C')
010300040310     d   T02cod      e                     inz('MFP')
010400161109     d
010500161109     d xgiolavds     e ds                  inz
010600040310      * - Tab. "MFP" = Applicazioni installate c/o P.O.
010700040310     d dMFP          e ds                  inz
010800040310     d   §MFPap                1     10    dim(10)
010900160506      * - Tab. "03" = Zone Consegna
011000160506     d ds03          e ds                  inz
011100040406      * - Tab. "TB" = Tipo bolla
011200040406     d dsTB          e ds                  inz
011300030729      * - Tab. "3I" = Variabili programmi partenze
011400030729     d ds3I          e ds                  inz
011500030729      * - Tab. "3A" = Codici bolla
011600030729     d ds3A          e ds                  inz
011700050517      * - DS campo spuntatore in fnfvv
011800050517     d dFVVSPN       e ds                  inz
011900030729      * - Descrizione 143 dell'organigramma
012000030729     d Og143         e ds                  inz
012100160506      * - Descrizione 150 dell'organigramma
012200160506     d Og150         e ds                  inz qualified
012300030930      * - ds per trk FLTR901L
012400030930     d FLTR9ds       e ds                  inz extname(FLTR900F)
012500031001     d FLTR9ima      e ds                  inz extname(FLTR900F) prefix(IMA)
012600030729      * - ds per trk FNART01L
012700030729     d FNARTds       e ds                  inz extname(FNART00F)
012800060217      * - ds per trk FiAR401L
012900060217     d FiAR4ds       e ds                  inz extname(FiAR400F)
013000030930      * - ds per campo AR4not del file FIAR4 - tipo rec. "A"
013100030930     d dsBL4A        e ds                  inz
013200161007
013300161007     d tigcp00ds     e ds                  inz extname(tigcp00F)
013400161007     d fnlgi1ds      e ds                  inz
013500030729      *
013600030729      * - DS codice a barre per calcolo check-digit
013700030930     d BARcodeDS1      ds            18    inz
013800030729     d   BARcdeFLS                    3s 0 inz
013900030729     d   BARcdeLNA                    3s 0 inz
014000030729     d   BARcdeNRS                    2s 0 inz
014100030729     d   BARcdeNSC                    7s 0 inz
014200030729     d   BARcdeZNC                    2s 0 inz
014300030930      * - DS codice a barre (SPEDIZIONE) per calcolo check-digit
014400030930     d BARcodeDS2      ds            17    inz
014500030930     d   BARcdeAAS                    2s 0 inz
014600030930     d   BARcdeLNP                    3s 0 inz
014700030930     d   BARcdeNRS2                   2s 0 inz
014800030930     d   BARcdeNSP                    7s 0 inz
014900030930      * - DS x pgm per controllo/inversione data
015000030930     d WLBdat          ds                  inz
015100030930     d  G08dat                        8  0 inz
015200030930     d  G08inv                        8  0 inz
015300030930     d  G08err                        1    inz
015400030930     d  G08tgi                        5  0 inz
015500030729      * - DS per dati utilizzati del trk AZORG01L
015600030729     d AZORGds         ds                  inz
015700030729     d   ORGdes                            inz
015800030729     d   ORGde3                            inz
015900030729     d   ORGde5                            inz
016000030729     d AZORGfls        ds                  inz
016100030729     d   flsORGdes                         like(ORGdes) inz
016200030729     d   flsORGde3                         like(ORGde3) inz
016300030729     d   flsORGde5                         like(ORGde5) inz
016400030729     d AZORGlnp        ds                  inz
016500030729     d   lnpORGdes                         like(ORGdes) inz
016600030729     d   lnpORGde3                         like(ORGde3) inz
016700030729     d   lnpORGde5                         like(ORGde5) inz
016800030729     d AZORGlna        ds                  inz
016900030729     d   lnaORGdes                         like(ORGdes) inz
017000030729     d   lnaORGde3                         like(ORGde3) inz
017100030729     d   lnaORGde5                         like(ORGde5) inz
017200030729      *
017300030729      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
017400030729      * - campi di comodo
017500040310     d bnlTR9pep       s                   like(TR6bau) inz
017600040413     d wBAU            s                   like(TR6bau) inz
017700030930     d wDCM            s                   like(ARBdcm) inz
017800161109     d kDCM_c          s                   like(ARBdcm) inz
017900030930     d wDFVgma         s              8  0 inz
018000080702     d £po             s                   like(tr9fgs) inz
018100080703     d £TipFV          s                   like(tr9npga) inz
018200080703     d £NumFV          s                   like(tr9nfva) inz
018300030729      * - indici di schiera / memo progressivi in ciclo DO
018400030729     d xx              s              3  0 inz
018500030729     d xy              s              3  0 inz(1)
018600030729     d yy              s              3  0 inz(1)
018700031001      * - flags booleani
018800040310     d $VdL            s              1    inz(*off)
018900031001     d $FLTR9          s              1    inz(*off)
019000040310     d $Giacenza       s              1    inz(*off)
019100040406     d $C_Serviz       s              1    inz(*off)
019200030729      * - campi per test "rottura"
019300030729     d wPO             s                   like(ARBlna) inz(*hival)
019400030729     d SAVEpdc         s                   like(ARBpdc) inz(*hival)
019500030729     d SAVEcbo         s                   like(ARBcbo) inz(*hival)
019600161109     d
019700161109     d wanomale        s              1
019800161109     d Dataiso         s               d   datfmt(*iso) inz(*sys)
019900030729
020000030729      *===============================================================*
020100030729      *  RIEPILOGO INDICATORI                                         *
020200030729      *---------------------------------------------------------------*
020300030729      * 10    - Comodo                                                *
020400030729      *===============================================================*
020500030729
020600030729     c     *Entry        plist
020700080702     c                   parm                    codicepc          1
020800080702     c                   parm                    filiale           3
020900080703     c                   parm                    catfoglio         1
021000080703     c                   parm                    numfoglio         7
021100030729      *
021200030729      * Key-List
021300031013      * - file FLTR901L
021400031013     c     K03tr901      klist
021500031013     c                   kfld                    £PO
021600031013     c                   kfld                    £TipFV
021700031013     c                   kfld                    £NumFV
021800030930      * - file FNARB25L
021900030930     c     K02arb25      klist
022000030730     c                   kfld                    L6(xx)
022100030930     c                   kfld                    wDCM
022200161109      * - file FNARB25L  le consegnate degli ultimi 3 giorni
022300161109     c     K02arb_c      klist
022400161109     c                   kfld                    L6(xx)
022500161109     c                   kfld                    kDCM_c
022600030729      * - file FIAPD01L
022700030729     c     K02apd01      klist
022800030729     c                   kfld                    APDtip
022900030729     c                   kfld                    ARBpdc
023000030930      * - file FNFVV03L
023100030930     c     K03fvv03      klist
023200030930     c                   kfld                    FVVfgs
023300030930     c                   kfld                    FVVnpg
023400030930     c                   kfld                    FVVnfv
023500060217      * - file FiAR401L
023600030729     c     K05ar401      klist
023700030729     c                   kfld                    ARBaas
023800030729     c                   kfld                    ARBlnp
023900030729     c                   kfld                    ARBnrs
024000030729     c                   kfld                    ARBnsp
024100030729     c                   kfld                    AR4trc
024200040318      * - file FNART01L - FNLBL01L
024300040318     c     K04sped       klist
024400030729     c                   kfld                    ARBaas
024500030729     c                   kfld                    ARBlnp
024600030729     c                   kfld                    ARBnrs
024700030729     c                   kfld                    ARBnsp
024800040318      * - file FNARB01L
024900040318     c     K04arb01      klist
025000040318     c                   kfld                    LBLaap
025100040318     c                   kfld                    LBLlpp
025200040318     c                   kfld                    LBLnrp
025300040318     c                   kfld                    LBLnsp
025400050331      * - file TIGCP21L
025500040310     c     K05gca01      klist
025600040310     c                   kfld                    ARBaas
025700040310     c                   kfld                    ARBlnp
025800040310     c                   kfld                    ARBnrs
025900040310     c                   kfld                    ARBnsp
026000040310     c                   kfld                    GCPfrg
026100030729      * - file TABEL00F
026200030729     c     K03tab00      klist
026300030729     c                   kfld                    TBLkut
026400030729     c                   kfld                    TBLcod
026500030729     c                   kfld                    TBLkey
026600040406     c     K02tab00      klist
026700040406     c                   kfld                    TBLkut
026800040406     c                   kfld                    TBLcod
026900030729      *
027000030729      * Operazioni Iniziali
027100030729     c                   exsr      OPE_INI
027200031001      * Intabellamento dati del file FLTR9
027300031001     c                   exsr      IntabFLTR9
027400030729      *
027500030729      * Ciclo di gestione per p.o. (£1 del P.O. elaboratore)
027600030729do  1c                   DO        30            xx
027700030729      *
027800030730if  2c                   if        L6(xx)    =  *zeros
027900030729     c                   leave
028000030729e   2c                   endif
028100161109     c
028200161109     c                   eval      wanomale=' '
028300161110     c                   eval      arbcca=' '
028400030729      *
028500031001      * Posizionamento iniziale sul file FNARB
028600030930     c     K02arb25      setll     FNARB000
028700030930     c     K02arb25      reade     FNARB000
028800030729      *
028900031001      * Ciclo di elaborazione del file FNARB
029000031001do  2c                   Dow       NOT %eof(FNARB25L)
029100040318      *
029200030729     c                   exsr      sr_ELAB
029300030930     c     K02arb25      reade     FNARB000
029400040414      *
029500031001e   2c                   EndDo
029600161109
029700161109     c                   eval      wanomale='S'
029800161109      * elaboro anche le spedizioni consegnate degli ultimi 3 giorni
029900161109     c     K02arb_c      setll     FNARB000
030000161109     c     l6(xx)        reade     FNARB000
030100161109      *
030200161109      * Ciclo di elaborazione del file FNARB
030300161109do  2c                   Dow       NOT %eof(FNARB25L)
030400161109      *
030500161109     c                   exsr      sr_ELAB
030600161109     c     l6(xx)        reade     FNARB000
030700161109      *
030800161109e   2c                   EndDo
030900030729      *
031000030729e   1c                   ENDDO
031100030729      *
031200030729      * Fine
031300030729     c                   clear                   TRUL28ds
031400030729     c                   eval      I28tla = 'C'
031500030729     c                   call      'TRUL28R1'
031600030729     c                   parm                    TRUL28DS
031700161007
031800161007     c                   clear                   fnlgi1ds
031900161007     c                   eval      ILGI1TLA= 'C'
032000161007     c                   movel     fnlgi1ds      kpjbu
032100161007     c                   call      'FNLGI1R'
032200161007     c                   parm                    kpjba
032300030729     c                   eval      *inLR  = *on
032400030729
032500030729      *---------------------------------------------------------------*
032600030729      * OPERAZIONI INIZIALI                                           *
032700030729      *---------------------------------------------------------------*
032800030729     c     OPE_INI       BegSr
032900030729      *
033000080702     c                   move      filiale       £po
033100080703     c                   move      catfoglio     £tipfv
033200080703     c                   move      numfoglio     £numfv
033300030729      *
033400040406      * Imposto l'utente fisso in file TABEL00F
033500040406     c                   eval      TBLkut = 1
033600030930      * Imposto la categoria dei fogli da elaborare
033700040310     c                   eval      FVVnpg =  4
033800030729      * Imposto il tipo anagrafica per il file FIAPD01L
033900030729      *   "Anagrafica Autotrasportatori/Coop"
034000030729     c                   eval      APDtip = 'A'
034100040310      * Imposto il n° progressivo di riapertura giacenza
034200040310     c                   clear                   GCPfrg
034300030729      * Imposto la KPJBA
034400030729     c                   reset                   KPJBA
034500030729     c                   eval      KNMUS = SDSusr
034600030729     c                   eval      KNMTD = SDSjob
034700030729     c                   eval      KNRJO = SDSjnr
034800030729     c                   eval      KNMEB = SDSjob
034900040310      *
035000040310     c                   reset                   $VdL
035100040310      *
035200040310      * Controllo se P.O. in gestione con VDL
035300040310     c                   clear                   dMFP
035400040310     c                   reset                   TIBS02ds
035500040310     c                   movel     £po           T02ke1
035600040310     c                   call      'TIBS02R'
035700040310     c                   parm                    KPJBA
035800040310     c                   parm                    TIBS02ds
035900040310if  1c                   if        T02err = *blanks
036000040310     c                   movel     T02uni        dMFP
036100040310e   1c                   endif
036200060214      * - NON per i P.O. "elencati" in C_PO_...
036300060214if  1c                   if        £po   <> C_PO_a
036400040310     c     'V'           lookup    §MFPap                                 10
036500040310     c                   movel     *in10         $VdL
036600060214e   1c                   endif
036700160506      *
036800160506     c     £po           chain     AZORG
036900160506if  1c                   if            %found(AZORG01L)
037000160506     c                             and ORGfva = *blanks
037100160506     c                   movel     ORGdf0        Og150
037200160506e   1c                   endif
037300030729      *
037400030729      * Reperisco tab. "3I" = variabili pgm partenze
037500030729     c                   clear                   ds3I
037600030729     c                   eval      TBLcod = '3I'
037700030729     c                   movel(p)  '1'           TBLkey
037800030729     c     K03tab00      chain     TABEL
037900030729if  1c                   if            %found(TABEL00F)
038000030729     c                             and TBLflg = *blanks
038100030729     c                   movel     TBLuni        ds3I
038200030729e   1c                   endif
038300030729      * Carico tabella P.O. gestiti
038400030729     c                   reset                   Trul06ds
038500030729     c                   movel     £po           D06key
038600030729     c                   movel(p)  Trul06ds      KPJBU
038700030729     c                   call      'TRUL06R'
038800030729     c                   parm                    KPJBA
038900030729     c                   movel     KPJBU         Trul06ds
039000030730     c                   movea     lia           L6
039100030729      *
039200030729      * Reperisco i network corrispondenti
039300030730     c                   clear                   L6ntw
039400030729do  1c                   DO        30            xx
039500030730if  2c                   if        L6(xx) = *zeros
039600030729     c                   leave
039700030729e   2c                   endif
039800030730     c     L6(xx)        chain     AZORG
039900030729if  2c                   if            %found(AZORG01L)
040000030729     c                             and ORGfva = *blanks
040100030729     c                   movel     ORGde3        OG143
040200030730     c                   movel     §OGntw        L6ntw(xx)
040300030729e   2c                   endif
040400030729e   1c                   ENDDO
040500040406      *
040600040407      * Intabello i tipi bolla c/servizio
040700040406     c                   reset                   $TBrC
040800040406     c                   clear                   xx
040900040406     c                   clear                   dsTB
041000040406     c                   eval      TBLcod = 'TB'
041100040406     c     K02tab00      setll     TABEL
041200040406     c     K02tab00      reade     TABEL
041300040406do  1c                   dow       NOT %eof(TABEL00F)
041400040406     c                             and xx < %elem($TBrC)
041500040406if  2c                   if        TBLflg = *blanks
041600040406     c                   movel     TBLuni        dsTB
041700040406if  3c                   if        §TBrbl = 'C'
041800040406     c                   add       1             xx
041900040406     c                   movel     TBLkey        $TBrC(xx)
042000040406e   3c                   endif
042100040406e   2c                   endif
042200040406     c     K02tab00      reade     TABEL
042300040406e   1c                   enddo
042400161109      *
042500161109     c* Imposto la data di oggi  - 3 gg lavorativi
042600161109     c                   clear                   xgiolavds
042700161109     c                   movel     dataiso       IXGLDATA
042800161109     c                   movel     £po           IXGLFIL
042900161109     c                   eval      IXGLSUB='S'
043000161109     c                   eval      IXGLGG=3
043100161109     c                   eval      IXGLLAV='S'
043200161109     c                   call      'XGIOLAV'
043300161109     c                   parm                    xgiolavds
043400161109     c
043500161109     c                   eval      kdcm_C=OXGLDATA
043600030729      *
043700030729     c                   EndSr
043800031001
043900031001      *---------------------------------------------------------------*
044000031001      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL PIANO DI SMISTAMENTO  *
044100031001      *---------------------------------------------------------------*
044200031001     c     IntabFLTR9    BegSr
044300031001      *
044400031001     c                   clear                   xy
044500040413     c                   clear                   xx
044600040413     c                   clear                   yy
044700040413     c                   clear                   $PosIMx
044800040413     c                   clear                   $PosBau
044900040413     c                   clear                   $BauIMx
045000031001      *
045100031001      * Ciclo di lettura del file FLTR9 per memorizzare
045200031001      *   tipo e numero foglio viaggio x P.O. gestione
045300031013     c     K03TR901      setll     FLTR9000
045400031013     c     K03TR901      reade     FLTR9000
045500031001      *
045600040310do  1c                   dow           NOT  %eof(FLTR901L)
045700040310     c                             and xy < %elem($TR9nfv)
045800031001      *
045900040310sel 2c                   select
046000040413      * - baia non letti (VdL)
046100040310w   2c                   when      TR9bnl  = 'S'
046200040310     c                   movel     TR9pep        bnlTR9pep
046300040414      * - baia "IM*"
046400040413w   2c                   when      TR9npg  = 3
046500040413if  3c                   if        imaTR9nfv = *zeros
046600031001     c                   movel     FLTR9ds       FLTR9ima
046700040413e   3c                   endif
046800040414      * - - cerco la postazione IM* per memorizzarne le baie
046900040413     c                   eval      xx      = 1
047000040413     c                   eval      *in10   = *off
047100040413     c     TR9bai        lookup    $PosIMx(xx)                            10
047200040413if  3c                   if        NOT *in10
047300040413     c                   eval      xx      = 1
047400040413     c     *blanks       lookup    $PosIMx(xx)                            10
047500040413if  4c                   if        *in10   = *on
047600040413     c                   movel     TR9bai        $PosIMx(xx)
047700040413e   4c                   endif
047800040413e   3c                   endif
047900040414      * - - per VdL memorzzo la baia della postazione IM* in esame
048000040414if  3c                   if            $VdL    = *on
048100040414     c                             and *in10   = *on
048200040413     c                   movea     $PosBau(xx)   $BauIMx
048300040413     c                   eval      yy      = 1
048400040413     c                   eval      *in10   = *off
048500040413     c                   movel     TR9pep        wBAU
048600040413     c     wBAU          lookup    $BauIMx(yy)                            10
048700040413if  4c                   if        NOT *in10
048800040413     c                   eval      yy      = 1
048900040413     c     *blanks       lookup    $BauIMx(yy)                            10
049000040413if  5c                   if        *in10   = *on
049100040413     c                   movel     TR9pep        $BauIMx(yy)
049200040414     c                   movea     $BauIMx       $PosBau(xx)
049300040413e   5c                   endif
049400040413e   4c                   endif
049500040413e   3c                   endif
049600040414      * - baia non "IM*"
049700040310w   2c                   when      TR9npg  = 4
049800031001     c                   add       1             xy
049900031013     c                   z-add     TR9nfv        $TR9nfv(xy)
050000031013     c                   movel     FLTR9ds       $TR9rec(xy)
050100040310      *
050200040310e   2c                   endsl
050300031001      *
050400031013     c     K03TR901      reade     FLTR9000
050500031001e   1c                   enddo
050600040413      *
050700040413      * La postazione "IMA" (generica) la considero sempre valida
050800090225if  1c**                 if        $PosIMx(1)  = *blanks
050900090225     c**                 eval      $PosIMx(1)  = 'IMA'
051000090225     c**                 eval      $PosBau(1)  = 'IMA'
051100090225x   1c**                 else
051200090225     c**                 eval      xx      = 1
051300090225     c**                 eval      *in10   = *off
051400090225     c**   'IMA'         lookup    $PosIMx(xx)                            10
051500090225if  2c**                 if        NOT *in10
051600090225     c**                 eval      xx      = 1
051700090225     c**   *blanks       lookup    $PosIMx(xx)                            10
051800090225if  3c**                 if        *in10   = *on
051900090225     c**                 eval      $PosIMx(xx) = 'IMA'
052000090225     c**                 eval      $PosBau(xx) = 'IMA'
052100090225e   3c**                 endif
052200090225e   2c**                 endif
052300090225e   1c**                 endif
052400040414      *
052500040414     c                   reset                   xx
052600040414     c                   reset                   yy
052700031001      *
052800031001     c                   EndSr
052900030729
053000030729      *---------------------------------------------------------------*
053100030729      * ELABORAZIONE SINGOLO RECORD DEL FILE FNARB25L                 *
053200030729      *---------------------------------------------------------------*
053300030729     c     sr_ELAB       BegSr
053400040310      *
053500040310     c                   reset                   $FLTR9
053600040310     c                   reset                   $Giacenza
053700040406     c                   reset                   $C_Serviz
053800040318      *
053900040319      * Scarto le spedizioni mamma dato che è la figlia che viene messa
054000040319      *  in distinta
054100161109     c                   if        wanomale=' '
054200040319     c     K04sped       chain     FNLBL002
054300040319     c                   IF        %FOUND(FNLBL02L)
054400040318     c                   goto      NO_elab
054500040318     c                   ENDIF
054600161109     c
054700161109     c                   else
054800161109     c                   if        arbcca<>'1' and arbcca<>'2' and arbcca<>'6'
054900161109     c                   goto      NO_elab
055000161109     c                   ENDIF
055100161109     c                   endif
055200161109      *
055300030729      *
055400030729      * Pulisco dati reperiti solo in casi specifici
055500030729     c                   clear                   FNARTds
055600060217     c                   clear                   FiAR4ds
055700030930     c                   clear                   dsBL4A
055800030729      *
055900030729      * Reperisco dati del p.o. segnacollo
056000030729     c                   eval      wPO    =  ARBfls
056100030729     c                   exsr      RepMemoPO
056200030729     c                   movel     $POd(yy)      AZORGfls
056300030729      *
056400030729      * Reperisco dati del p.o. di partenza
056500030729     c                   eval      wPO    =  ARBlnp
056600030729     c                   exsr      RepMemoPO
056700030729     c                   movel     $POd(yy)      AZORGlnp
056800030729      *
056900030729      * Reperisco dati del p.o. di arrivo
057000030729     c                   eval      wPO    =  ARBlna
057100030729     c                   exsr      RepMemoPO
057200030729     c                   movel     $POd(yy)      AZORGlna
057300030729      *
057400030729      * Reperisco decodifica Autotrasportatore consegna
057500030729if  1c                   if        ARBpdc    <> SAVEpdc
057600030729     c                   eval      SAVEpdc   =  ARBpdc
057700030729     c                   exsr      RepMemoPDC
057800030729e   1c                   endif
057900030729      *
058000030729      * Reperisco tabella Codici bolla
058100030729if  1c                   if        ARBcbo    <> SAVEcbo
058200030729     c                   eval      SAVEcbo   =  ARBcbo
058300030729     c                   exsr      RepMemoT3A
058400030729e   1c                   endif
058500031001      *
058600031001      * Reperisco dati del piano di smitamento (da file FLTR9)
058700161110if  1c                   if        ARBndc    >  *zeros  and arbdcm=0
058800031001     c                   exsr      FVinFLTR9
058900031013e   1c                   endif
059000040310      *
059100040414      * Se TR6bai = "IM*" (se ARBNDC = *zero            oppure
059200040310      *                    se distinta NON trovata su FLTR900F)
059300040310if  1c                   if           ARBndc = *zeros
059400161110     c                             or ($FLTR9 = *off  and arbdcm=0)
059500040406      *   verifico se esiste giacenza aperta con spedizione
059600040310     c                   exsr      ChkGiacenza
059700040407      *   controllo se c/servizio
059800040406     c                   exsr      ChkC_Serv
059900040310e   1c                   endif
060000030729      *
060100080703      * Preparo e scrivo il record in FITR600F
060200030729     c                   exsr      WriteFLTR6
060300030729      *
060400031028     c     NO_elab       EndSr
060500030729
060600030729      *---------------------------------------------------------------*
060700030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL P.O.                  *
060800030729      *---------------------------------------------------------------*
060900030729     c     RepMemoPO     BegSr
061000030729      *
061100030729      * Controllo se diverso con l'ultimo reperito
061200031001if  1c                   if        wPO    <> $PO(yy)
061300030729      *
061400030729      * Controllo se già elaborato
061500030729     c                   eval      *in10   = *off
061600030729     c                   eval      yy      = 1
061700030729     c     wPO           lookup    $PO(yy)                                10
061800030729      *
061900030729      * Reperimento dati se non ancora elaborato
062000030729if  2c                   if        not *in10
062100030729      *
062200030729     c     wPO           chain     AZORG
062300030729if  3c                   if        not %found(AZORG01L)
062400030729     c                             or  ORGfva <> *blanks
062500030729     c                   clear                   AZORGds
062600030729e   3c                   endif
062700030729      * - Memorizzazione dati:
062800030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
062900030729     c     *zeros        lookup    $PO(yy)                                10
063000030729if  3c                   if        not *in10
063100030729     c                   eval      yy      = %elem($PO)
063200030729e   3c                   endif
063300030729     c                   eval      $PO(yy) = wPO
063400030729     c                   movel     AZORGds       $POd(yy)
063500030729      *
063600030729e   2c                   endif
063700030729      *
063800030729e   1c                   endif
063900030729      *
064000030729     c                   EndSr
064100030729
064200030729      *---------------------------------------------------------------*
064300030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI PADRONCINI                *
064400030729      *---------------------------------------------------------------*
064500030729     c     RepMemoPDC    BegSr
064600030729      *
064700030729      * Controllo se già elaborato
064800030729     c                   eval      *in10   =  *off
064900030729     c                   eval      xy      =  1
065000030729     c     SAVEpdc       lookup    $PDC(xy)                               10
065100030729      *
065200030729      * Reperimento dati se non ancora elaborato
065300030729if  1c                   if        not *in10
065400030729      *
065500030729     c     K02apd01      chain     FIAPD000
065600030729if  2c                   if        not %found(FIAPD01L)
065700030729     c                             or  APDatb <> *blanks
065800030729     c                   clear                   APDrsc
065900030729e   2c                   endif
066000030729      * - Memorizzazione dati:
066100030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
066200030729     c     *hival        lookup    $PDC(xy)                               10
066300030729if  2c                   if        not *in10
066400030729     c                   eval      xy       = %elem($PDC)
066500030729e   2c                   endif
066600030729     c                   eval      $PDC(xy) = SAVEpdc
066700030729     c                   movel     APDrsc        $PDCd(xy)
066800030729      *
066900030729      * RE-impostazione dati se già elaborato
067000030729x   1c                   else
067100030729      *
067200030729     c                   movel     $PDCd(xy)     APDrsc
067300030729      *
067400030729e   1c                   endif
067500030729      *
067600030729     c                   EndSr
067700030729
067800030729      *---------------------------------------------------------------*
067900030729      * REPERIMENTO (E MEMORIZZAZIONE) TAB.3A = CODICI BOLLA          *
068000030729      *---------------------------------------------------------------*
068100030729     c     RepMemoT3A    BegSr
068200030729      *
068300030729      * Controllo se già elaborato
068400030729     c                   eval      *in10   =  *off
068500030729     c                   eval      xy      =  1
068600030729     c     SAVEcbo       lookup    $3A(xy)                                10
068700030729      *
068800030729      * Reperimento dati se non ancora elaborato
068900030729if  1c                   if        not *in10
069000030729      *
069100030729     c                   clear                   ds3A
069200030729     c                   eval      TBLcod = '3A'
069300030729     c                   movel(p)  SAVEcbo       TBLkey
069400030729     c     K03tab00      chain     TABEL
069500030729if  2c                   if        %found(TABEL00F)
069600030729     c                             and TBLflg = *blanks
069700030729     c                   movel     TBLuni        ds3A
069800030729e   2c                   endif
069900030729      * - Memorizzazione dati:
070000030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
070100030729     c     *hival        lookup    $3A(xy)                                10
070200030729if  2c                   if        not *in10
070300030729     c                   eval      xy      =  %elem($3A)
070400030729e   2c                   endif
070500030729     c                   eval      $3A(xy) =  SAVEcbo
070600030729     c                   movel     ds3A          $3Ads(xy)
070700030729      *
070800030729      * RE-impostazione dati se già elaborato
070900030729x   1c                   else
071000030729      *
071100030729     c                   movel     $3Ads(xy)     ds3A
071200030729      *
071300030729e   1c                   endif
071400030729      *
071500030729     c                   EndSr
071600031001
071700031001      *---------------------------------------------------------------*
071800031013      * RICERCA DEL F.V. IN FLTR9                                     *
071900031001      *---------------------------------------------------------------*
072000031001     c     FVinFLTR9     BegSr
072100050517     c                   clear                   dfvvspn
072200031001      *
072300031001      * Reperisco dati da schiera
072400031001     c                   eval      *in10     =  *off
072500031001     c                   eval      xy        =  1
072600031013     c     ARBndc        lookup    $TR9nfv(xy)                            10
072700031001     c                   eval      $FLTR9    =  *in10
072800031001      *
072900031001if  1c                   if        $FLTR9    =  *off
073000031013      * Impostazione dati per Tipo/Numero F.V. non reperito
073100031001     c                   exsr      RepFNFVV
073200031001x   1c                   else
073300031013      * RE-impostazione dati per Tipo/Numero F.V. reperito
073400031013     c                   movel     $TR9rec(xy)   FLTR9ds
073500050517     c
073600160506     c* chain comunque in fnfvv per avere tipo distinta parcel/messagg
073700050517     c                   z-add     ARBifp        FVVfgs
073800050517     c                   z-add     ARBndc        FVVnfv
073900050517     c     K03fvv03      chain     FNFVV000
074000050517if  1c                   if            %found(FNFVV03L)
074100050517     c                   movel     fvvspn        dfvvspn
074200050517     c                   endif
074300050517     c
074400031001e   1c                   endif
074500031001      *
074600031001     c                   EndSr
074700030930
074800030930      *---------------------------------------------------------------*
074900031001      * REPERIMENTO DATI DAI FOGLI VARI                               *
075000030930      *---------------------------------------------------------------*
075100031001     c     RepFNFVV      BegSr
075200030930      *
075300030930     c                   clear                   wDFVgma
075400030930     c                   clear                   FVVdfv
075500030930      *
075600030930     c                   z-add     ARBifp        FVVfgs
075700030930     c*** (già fatto)    z-add     4             FVVnpg
075800031013     c                   z-add     ARBndc        FVVnfv
075900030930     c     K03fvv03      chain     FNFVV000
076000031013if  1c                   if            %found(FNFVV03L)
076100031013     c                             and FVVatb =  *blanks
076200030930     c                   clear                   WLBdat
076300030930     c                   eval      G08inv    =  FVVdfv
076400030930     c                   eval      G08err    =  '3'
076500030930     c                   call      'XSRDA8'
076600030930     c                   parm                    WLBdat
076700030930     c                   eval      wDFVgma   =  G08dat
076800050517     c                   movel     fvvspn        dfvvspn
076900031001e   1c                   endif
077000030930      *
077100030930     c                   EndSr
077200040310
077300040310      *---------------------------------------------------------------*
077400040310      * VERIFICA SE ESISTE GIACENZA APERTA PER SPEDIZIONE             *
077500040310      *---------------------------------------------------------------*
077600040310     c     ChkGiacenza   BegSr
077700040310      *
077800050331     c     K05gca01      chain     Tigcp21l
077900050331if  1c                   if            %found(Tigcp21l)
078000040310     c                             and GCPatb = *blanks
078100050331     c                             and (GCPfas < 035 or
078200050331     c                                  GCPfas = 036)
078300161007     c* solo se merce da portare a IMG con driver
078400161007     c                   clear                   fnlgi1ds
078500161007     c                   eval      ILGI1AAS=arbaas
078600161007     c                   eval      ILGI1lnp=arblnp
078700161007     c                   eval      ILGI1nrs=arbnrs
078800161007     c                   eval      ILGI1nsp=arbnsp
078900161007     c                   movel     fnlgi1ds      kpjbu
079000161007     c                   call      'FNLGI1R'
079100161007     c                   parm                    kpjba
079200161007     c                   parm                    tigcp00ds
079300161007     c                   movel     kpjbu         fnlgi1ds
079400161007
079500161007    2c                   if        olgi1AMG='S' or olgi1AMG='Y'
079600161007     c                             or olgi1inmg='S'
079700040310     c                   eval      $Giacenza  = *on
079800161007e   2c                   endif
079900160823e   1c                   endif
080000040310      *
080100040310     c                   EndSr
080200040406
080300040406      *---------------------------------------------------------------*
080400040407      * VERIFICA SE BOLLA C/SERVIZIO                                  *
080500040406      *---------------------------------------------------------------*
080600040406     c     ChkC_Serv     BegSr
080700040406      *
080800040406     c                   eval      *in10     =  *off
080900040406if  1c                   if        §3Atb2    =  *blanks
081000040406     c     §3Atb1        lookup    $TBrC                                  10
081100040406x   1c                   else
081200040406     c     §3Atb2        lookup    $TBrC                                  10
081300040406e   1c                   endif
081400040406     c                   eval      $C_Serviz =  *in10
081500040406      *
081600040406     c                   EndSr
081700030729
081800030729      *---------------------------------------------------------------*
081900080703      * PREPARAZIONE E SCRITTURA DEL RECORD IN FITR600F               *
082000030729      *---------------------------------------------------------------*
082100030729     c     WriteFLTR6    BegSr
082200030729      *
082300080703     c                   clear                   FITR6
082400160506      *
082500080703     c                   eval      tr6fil=filiale
082600080703     c                   eval      tr6cpc=codicepc
082700031024      *
082800060217      * aggancio FiAR4
082900031024     c                   clear                   dsBL4A
083000060217if  1c                   if        not %open(FiAR401L)
083100060217     c                   open      FiAR401L
083200031024e   1c                   endif
083300031024     c                   eval      AR4trc    =  'A'
083400060217     c     K05ar401      chain     FiAR4000
083500060217if  1c                   if        %found(FiAR401L)
083600031024     c                   movel     AR4not        dsBL4A
083700031024e   1c                   endif
083800030729      *
083900030729      * segnacollo "chi sono"
084000030729      * (imposto i dati validi per tutti i segnacolli della spedizione;
084100030729      *  il pgm. TRUL28R verrà richiamato per ogni collo)
084200030930     c                   clear                   BarCodeDS1
084300030729     c                   eval      BARcdeFLS = ARBfls
084400030729     c                   eval      BARcdeLNA = ARBlna
084500030729     c                   eval      BARcdeNRS = ARBnrs
084600030729     c                   eval      BARcdeZNC = ARBznc
084700030729      * secondi di attesa dopo stampa
084800030729     c                   eval      TR6sea    =  §3Isea
084900030729      * flag segnacollo da stampare ('S'/' ')
085000030729     c                   eval      TR6st1    =  'S'
085100030729      * flags
085200030729     c                   eval      TR6fl1    =  *off
085300030729      * codice cliente
085400030729if  1c                   if        ARBccm    >  *zeros
085500030729     c                   eval      TR6ksc    =  ARBccm
085600030729x   1c                   else
085700030729     c                   eval      TR6ksc    =  ARBksc
085800030729e   1c                   endif
085900030729      * p.o. partenza segnacollo
086000030729     c                   eval      TR6fls    =  ARBfls
086100030729      * descrizione del p.o. partenza segnacollo
086200030729     c                   eval      TR6dlp    =  flsORGdes
086300030729      * codice prodotto - default
086400030729     c                   eval      TR6cdp    =  ARBrma
086500030729      * numero serie segnacollo
086600030729     c                   eval      TR6nrs    =  ARBnrs
086700030729      * p.o. di arrivo
086800030729     c                   eval      TR6lna    =  ARBlna
086900030729      * descrizione p.o. arrivo spedizione
087000030729if  1c                   if        lnaORGde5 <> *blanks
087100030729     c                   eval      TR6dla    =  lnaORGde5
087200030729x   1c                   else
087300030729     c                   eval      TR6dla    =  lnaORGdes
087400030729e   1c                   endif
087500030729      * terminal di arrivo
087600030729     c                   eval      TR6tna    =  ARBtfa
087700030729      * tipo servizio bolle
087800030729     c                   eval      TR6tsp    =  ARBtsp
087900030729      * zona segnacollo
088000030729     c                   eval      TR6zsc    =  ARBznc
088100030729      * totale segnacolli della spedizione
088200040728     C                   IF        ARBNCL < 999
088300030729     c                   eval      TR6nc2    =  ARBncl
088400040728     c                   ELSE
088500040728     c                   eval      TR6nc2    =  999
088600040728     c                   ENDIF
088700030729      * ragione sociale mittente
088800030729     c                   eval      TR6rsm    =  ARBrsm
088900030729      * provincia mittente
089000030729     c                   eval      TR6prm    =  ARBprm
089100030729      * ragione sociale destinatario
089200030729     c                   eval      TR6rsd    =  ARBrsd
089300030729      * c.a.p. destinatario
089400030729     c                   eval      TR6cad    =  ARBcad
089500030729      * indirizzo destinatario
089600030729     c                   eval      TR6ind    =  ARBind
089700030729      * località destinatario
089800030729     c                   eval      TR6lod    =  ARBlod
089900030729      * provincia destinatario
090000030729     c                   eval      TR6prd    =  ARBprd
090100030729      * volume
090200030729     c                   eval      TR6vol    =  ARBvlf
090300030729      * peso in kg
090400030729     c                   eval      TR6pkg    =  ARBpkf
090500030729      * flag volume bollettato
090600030729     c                   eval      TR6fvr    =  ARBfvf
090700030729      * anno spedizione
090800030729     c                   eval      TR6aas    =  ARBaas
090900030729      * meso/giorno spedizione
091000030729     c                   eval      TR6mgs    =  ARBmgs
091100030729      * tipo stampa: 'ITA' / 'EST' / 'ALL' / 'DIS'
091200030729     c                   eval      TR6tst    =  'DIS'
091300030930      *
091400030930      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
091500030930      *
091600030930      * Se ARBNDC = *zero            oppure
091700030930      * Se distinta NON trovata su FLTR900F
091800161110     c* Se bolla sto elaborando bolla consegnata con cons.anomala --> IMD
091900030930if  1c                   if           ARBndc = *zeros
092000031001     c                             or $FLTR9 = *off
092100161110     c                             or (arbcca<>*blanks  and arbdcm>0)
092200030930      *
092300030930      * - baia uscita
092400040414     c*** non qui:       eval      TR6bai    =  'IMA'
092500030930      * - p.o. distinta consegna
092600031001     c                   eval      TR6poc    =  imaTR9fgs
092700030930      * - categoria foglio viaggio
092800030930     c                   eval      TR6npg    =  3
092900031001      * - numero foglio viaggio
093000031001     c                   eval      TR6nfv    =  imaTR9nfv
093100031001      * - data foglio viaggio
093200031001     c                   eval      TR6dfv    =  imaTR9dfv
093300031022      * - flag prestazione padroncino (M-mattino, P-pomeriggio)
093400040310     c***                eval      TR6fpp    =  *blanks
093500030930      * - codice autotrasportatore consegna
093600030930     c***                eval      TR6pdc    =  *zeros
093700030930      * - descrizione autotrasportatore consegna
093800030930     c***                eval      TR6dpd    =  *blanks
093900031013      * - descrizioni per stampa
094000161110if  2c                   IF        ARBndc    =  *zeros  or
094100161110     c                             (arbdcm>0 and arbcca<>' ')
094200040406     c                   exsr      sr_DS1
094300160506      * -?distinta messaggerie  o  eventuale Turno?
094400161110      *  ?(per ARBNDC = *zero) oppure consegnata con cons anomala 1/2/6
094500160506if  3c                   If            Og150.§OGpkm   = 'S'
094600160506     c                             or  Og150.§OGturno = 'S'
094700160506     c                   clear                   ds03
094800160506     c                   eval      TBLcod = '03'
094900160506     c                   eval      TBLkey = %editc( ARBlna : 'X' ) +
095000160506     c                                      %editc( ARBznc : 'X' )
095100160506     c     K03tab00      chain     TABEL
095200160506if  4c                   if        %found(TABEL00F)
095300160506     c                             and TBLflg = *blanks
095400160506     c                   movel     TBLuni        ds03
095500160506e   4c                   endif
095600160506     c                   exsr      sr_MessagTurno
095700160506e   3c                   EndIf
095800160506x   2c                   ELSE
095900040414     c                   eval      TR6bai    =  'IM5'
096000031013     c                   eval      TR6ds1    =  'DISTINTA NON PRESENTE'
096100031013     c                                       +  ' NEL PIANO'
096200040130     c                   eval      TR6ds2    =  'Distinta '
096300031013     c                                       +  %editc(ARBndc  : 'Z')
096400031013     c                                       +  ' del '
096500031013     c                                       +  %editc(wDFVgma : 'Y')
096600160506      * -?distinta messaggerie  o  eventuale Turno?
096700160506      *  ?(per $FLTR9 = *off)?
096800160506     c                   exsr      sr_MessagTurno
096900160506e   2c                   ENDIF
097000040414      * - baia uscita per etichetta (postazione)
097100040414if  2c                   if        TR6bai    =  *blanks
097200040414     c                   eval      TR6bai    =  'IMA'
097300040414e   2c                   endif
097400040414     c                   exsr      sr_PosIMA
097500040310      * - baia uscita per picking VdL
097600040413if  2c                   if        $VdL      =  *on
097700040413     c                   exsr      sr_BauIMA
097800040414if  3c                   if        TR6bau    =  *blanks
097900040414     c                   eval      TR6bau    =  imaTR9pep
098000040414e   3c                   endif
098100040310e   2c                   endif
098200030930      *
098300030930      * Se ARBNDC > *zero            e
098400030930      * Se distinta trovata su FLTR900F
098500030930x   1c                   else
098600030930      *
098700030930      * - baia uscita
098800030930     c                   eval      TR6bai    =  TR9bai
098900030930      * - p.o. distinta consegna
099000030930     c                   eval      TR6poc    =  TR9fgs
099100030930      * - categoria foglio viaggio
099200030930     c                   eval      TR6npg    =  TR9npg
099300031001      * - numero foglio viaggio
099400031001     c                   eval      TR6nfv    =  TR9nfv
099500031001      * - data foglio viaggio
099600031001     c                   eval      TR6dfv    =  TR9dfv
099700031022      * - flag prestazione padroncino (M-mattino P-pomeriggio)
099800031022if  2c                   if        TR9fl1    =  'P'
099900031022     c                   eval      TR6fpp    =  TR9fl1
100000031022e   2c                   endif
100100030930      * - codice autotrasportatore consegna
100200030930     c                   eval      TR6pdc    =  TR9pdc
100300031024     c                   eval      TR6dpd    =  %subst(APDrsc:1:19)
100400160506      * -?distinta messaggerie  o  eventuale Turno?
100500160506      *  ?(per ARBNDC > *zero e $FLTR9 = *on)?
100600160506     c                   exsr      sr_MessagTurno
100700030930      * - 1ª descrizione per stampa
100800030930     c***                eval      TR6ds1    =  *blanks
100900030930      * - 2ª descrizione per stampa
101000030930     c***                eval      TR6ds2    =  *blanks
101100040310      * - baia uscita per picking VdL
101200040310if  2c                   if        $VdL      = *on
101300040310     c                   eval      TR6bau    =  TR9pep
101400040310e   2c                   endif
101500030930      *
101600030930e   1c                   endif
101700030729      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
101800040318      * Se la spedizione non ha segnacolli ed ha una mamma aggancio la
101900040318      *  mamma per reperire i segnacolli solo se ha stessa LNA
102000040318if  1c                   IF        §3Abcm = 'S'     AND
102100040318     c                             ARBncd = *zeros  AND
102200040318     c                             ARBfns <> 'S'
102300040318     c     K04sped       chain     FNLBL000
102400040318if  2c                   IF        %found(FNLBL01L)  AND
102500040318     c                             LBLlan = LBLlap
102600040318     c     K04ARB01      chain     FNARBLEG
102700040319      * reimposto campi segnacollo e file
102800040319      *   non ricavo la descrizione del p.o. segnacollo per il picking
102900040319      *   non serve
103000040319if  3c                   IF        %found(FNARB01L)
103100040319     c                   clear                   BarCodeDS1
103200040319     c                   eval      TR6FLS    = ARBfls
103300040319     c                   eval      TR6NRS    = ARBnrs
103400040319     c                   eval      BARcdeFLS = ARBfls
103500040319     c                   eval      BARcdeLNA = ARBlna
103600040319     c                   eval      BARcdeNRS = ARBnrs
103700040319     c                   eval      BARcdeZNC = ARBznc
103800040319e   3c                   ENDIF
103900040319e   2c                   ENDIF
104000040318e   1c                   ENDIF
104100030729      *
104200030729      * cicli di WRITE dei record (1 x segnacollo)
104300040318      *  suddivisi x sequenziali e non sequenziali
104400030729sel 1c                   select
104500030729w   1c                   when      ARBncd >  *zeros
104600030729     c                   exsr      sr_SCS
104700030729w   1c                   when      ARBfns =  'S'
104800030729     c                   exsr      sr_SCNS
104900030729e   1c                   endsl
105000031006      *
105100031006      * eventuale WRITE dell'unico record x spedizione "DDT SÌ"
105200040310      *   (ultima, poiché re-imposta alcuni campi)
105300161111      *   non lo faccio per bolle con cons.anomala
105400161111    0c                   if        arbdcm=0
105500031006if  1c                   if           §B4abm =  'C'
105600031006     c                             or §B4abm =  'S'
105700031024     c                             or §B4abm =  'P'
105800031024     c                             or §B4abm =  'Q'
105900031013      * - - ripulisco i campi "inutili"
106000031013     c                   clear                   TR6ncd
106100031013     c                   clear                   TR6nc1
106200031006      * - - calcolo il check-digit per il barcode
106300031006     c                   exsr      sr_DDTSI
106400040310      * - - forzo gli "appositi" campi:
106500040310      * - - - baia uscita per picking VdL
106600040406if  2c                   if        $VdL      =  *on
106700040310     c                   eval      TR6bau    =  bnlTR9pep
106800040310e   2c                   endif
106900031006      *
107000080703     c                   WRITE(E)  FITR6
107100031006      *
107200031006e   1c                   endif
107300161111e   0c                   endif
107400030729      *
107500030729     c                   EndSr
107600040406
107700040406      *---------------------------------------------------------------*
107800040406      * Impostazione descrizione per stampa "DS1"                     *
107900040406      *---------------------------------------------------------------*
108000040406     c     sr_DS1        BegSr
108100040413      *
108200040407      * Imposto il campo TR6DS1 in base aiprimi 2?casi individuati
108300040406      *  (nel controllare 'ste 2 segnalazioni emergerebbero eventuali
108400040406      *  altri casi...)
108500040406      *
108600040414      *** - max:         eval      TR6ds1    =  '1234567890123456789'
108700040413      ***                                    +  '1234567890123456'
108800161110     c* Bolla resa o dirottata
108900161110     c                   if        arbcca<>' '  and arbdcm>0
109000161110if  2c                   if        TR6ds1    =  *blanks
109100161110     c                   eval      TR6bai    =  'IMD'
109200161110     c                   eval      TR6ds1    =  'RESO/DIR'
109300161110x   2c                   else
109400161110     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
109500161110     c                                       +  'RESO/DIR'
109600161110     c                   goto      End_DS1
109700161110e   2c                   endif
109800161110e   1c                   endif
109900040406      * Giacenza
110000040406if  1c                   if        $Giacenza =  *on
110100040406if  2c                   if        TR6ds1    =  *blanks
110200040414     c                   eval      TR6bai    =  'IM1'
110300040406     c                   eval      TR6ds1    =  'GIACENZA'
110400040406x   2c                   else
110500040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
110600040406     c                                       +  'GIACENZA'
110700040407     c                   goto      End_DS1
110800040406e   2c                   endif
110900040406e   1c                   endif
111000040406      * Fermo deposito
111100040406if  1c                   if        ARBffd    =  'S'
111200040406if  2c                   if        TR6ds1    =  *blanks
111300040414     c                   eval      TR6bai    =  'IM2'
111400040406     c                   eval      TR6ds1    =  'FERMO DEPOSITO'
111500040406x   2c                   else
111600040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
111700040406     c                                       +  'FERMO DEPOSITO'
111800040407     c                   goto      End_DS1
111900040406e   2c                   endif
112000040406e   1c                   endif
112100040406      * Supermercato
112200040406if  1c                   if        ARBtc1    =  'S'  or
112300040406     c                             ARBtc2    =  'S'
112400040406if  2c                   if        TR6ds1    =  *blanks
112500040414     c                   eval      TR6bai    =  'IM3'
112600040406     c                   eval      TR6ds1    =  'SUPERMERCATO'
112700040406x   2c                   else
112800040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
112900040406     c                                       +  'SUPERMERCATO'
113000040407     c                   goto      End_DS1
113100040406e   2c                   endif
113200040406e   1c                   endif
113300040406      * C/Servizio
113400040406if  1c                   if        $C_Serviz =  *on
113500040406if  2c                   if        TR6ds1    =  *blanks
113600040414     c                   eval      TR6bai    =  'IM4'
113700040413     c                   eval      TR6ds1    =  'C/SERVIZIO'
113800040406x   2c                   else
113900040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
114000040407     c                                       +  'C/SERVIZIO'
114100040407     c                   goto      End_DS1
114200040406e   2c                   endif
114300040406e   1c                   endif
114400040413      *
114500040413     c     End_DS1       EndSr
114600160506
114700160506      /free
114800160506
114900160506       //--------------------------------------------------------------
115000160510       //?Impostazione campo TR6FL3:                                   ?
115100160510       //?- "M" = Spedizione con zona consegna Messaggerie             ?
115200160510       //?- "1" = Spedizione nel 1° turno di smistamento colli         ?
115300160510       //?- "2" = Spedizione nel 2° turno di smistamento colli         ?
115400160510       //?- " " = Spedizione nel turno unico di smistamento colli      ?
115500160506       //--------------------------------------------------------------
115600160506       BEGSR  sr_MessagTurno;
115700160506
115800160506         // -?Se ARBNDC = *zero?
115900160506         //  ?(spedizioni in IMA - tranne "distinta non nel piano" [IM5])?
116000161110         //  ?                   - anche le cons anomale 1/2/6     [IM7])?
116100161110         If  ARBndc = *zeros or (arbdcm>0 and arbcca<>' ');
116200160506
116300160506           // ·?Distinta Messaggerie?
116400160506           if  Og150.§OGpkm   = 'S'  and  §03tpm = 'M';
116500160506             TR6fl3 = §03tpm;
116600160506           endif;
116700160506
116800160506           // ·?Turno?
116900160506           if  Og150.§OGturno = 'S'  and  TR6fl3 = *blanks;
117000160511             TR6fl3 = §03turno;
117100160506           endif;
117200160506
117300160506         // -?Altrimenti?
117400160506         //  ?(distinta  trovata  o  NON trovata [IM5]  su FLTR900F)?
117500160506         Else;
117600160506
117700160506           // ·?Distinta Messaggerie?
117800160506           if  Og150.§OGpkm   = 'S'  and  §FVVtpm = 'M';
117900160506             TR6fl3 = §FVVtpm;
118000160506           endif;
118100160506
118200160506           // ·?Turno?
118300160506           if  Og150.§OGturno = 'S'  and  TR6fl3  = *blanks;
118400160511             TR6fl3 = §FVVturno;
118500160506           endif;
118600160506
118700160506         EndIf;
118800160506
118900160506       ENDSR;
119000160506
119100160506      /end-free
119200040413
119300040413      *---------------------------------------------------------------*
119400040414      * Assegnazione postazione "IM*"                                 *
119500040413      *---------------------------------------------------------------*
119600040414     c     sr_PosIMA     BegSr
119700040413      *
119800040413     c                   eval      xy     = 1
119900040413     c                   eval      *in10  = *off
120000040414      *
120100040414      * Prima cerco la postazione specifica
120200040414      * SE non la trovo: cerco la postazione "IM0"
120300040414      * SE non trovo neppure quella: cerco la postazione "IMA"
120400040414      *   (che c'è sicuramente!!!)
120500040414do  1c                   dow       NOT *in10
120600040414      *
120700040414     c     TR6bai        lookup    $PosIMx(xy)                            10
120800040414      *
120900040414sel 2c                   select
121000040414w   2c                   when      NOT *in10 and TR6bai <> 'IM0'
121100040414     c                                       and TR6bai <> 'IMA'
121200040414     c                   eval      TR6bai = 'IM0'
121300040414w   2c                   when      NOT *in10
121400040414     c                   eval      TR6bai = 'IMA'
121500040414e   2c                   endsl
121600040414      *
121700040414e   1c                   enddo
121800040414      *
121900040414     c                   EndSr
122000040414
122100040414      *---------------------------------------------------------------*
122200040414      * Assegnazione baia di uscita corrispondente alla postaz. "IM*" *
122300040414      *---------------------------------------------------------------*
122400040414     c     sr_BauIMA     BegSr
122500040414      *
122600040414     c                   eval      xy     = 1
122700040414     c                   eval      *in10  = *off
122800040414     c     TR6bai        lookup    $PosIMx(xy)                            10
122900040414      * (lo trova sicuramente)
123000040414      *
123100040414if  1c                   if        *in10  = *on
123200040413     c                   movea     $PosBau(xy)   $BauIMx
123300040413     c                   eval      $LastBau(xy)    = $LastBau(xy) + 1
123400040414      *
123500040414if  2c                   if           $LastBau(xy) > %elem($BauIMx)
123600040413     c                             or $LastBau(xy) = *zeros
123700040413     c                             or $BauIMx($LastBau(xy)) = *blanks
123800040413     c                   eval      $LastBau(xy)    = 1
123900040414e   2c                   endif
124000040414e   1c                   endif
124100040414      *
124200040413     c                   eval      TR6bau = $BauIMx($LastBau(xy))
124300040413      *
124400040413     c                   EndSr
124500030729
124600030729      *---------------------------------------------------------------*
124700030729      * Registrazione elenco segnacolli sequenziali                   *
124800030729      *---------------------------------------------------------------*
124900030729     c     sr_SCS        BegSr
125000030729      *
125100030729     c                   clear                   TR6nc1
125200030729     c                   eval      TR6ncd    =  ARBncd - 1
125300030729if  1c                   if        ARBnca    =  *zeros
125400030729     c                   eval      ARBnca    =  ARBncd
125500030729e   1c                   endif
125600030729      *
125700030729      * Ciclo di calcolo segnacolli sequenziali
125800030729do  1c                   DOW       TR6ncd    <  ARBnca
125900030729      *
126000030729      * - numero segnacollo
126100030729     c                   eval      TR6ncd    =  TR6ncd + 1
126200030930      *
126300030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
126400030930     c                   exsr      WrtTR6rec
126500030729      *
126600030729e   1c                   ENDDO
126700030729      *
126800030729     c                   EndSr
126900030729
127000030729      *---------------------------------------------------------------*
127100030729      * Registrazione elenco segnacolli NON sequenziali               *
127200030729      *---------------------------------------------------------------*
127300030729     c     sr_SCNS       BegSr
127400030729      *
127500030729     c                   clear                   TR6nc1
127600030729      *
127700030729      * Ciclo di repermento segnacolli NON sequenziali
127800030729if  1c                   if        not %open(FNART01L)
127900030729     c                   open      FNART01L
128000030729e   1c                   endif
128100040318     c     K04sped       setll     FNART000
128200040318     c     K04sped       reade     FNART000
128300030729      *
128400030729do  1c                   DOW       NOT %eof(FNART01L)
128500030729      *
128600030729      * - numero segnacollo
128700030729     c                   eval      TR6ncd    =  ARTnsc
128800030930      *
128900030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
129000030930     c                   exsr      WrtTR6rec
129100030729      *
129200040318     c     K04sped       reade     FNART000
129300030729      *
129400030729e   1c                   ENDDO
129500030729      *
129600030729     c                   EndSr
129700030930
129800030930      *---------------------------------------------------------------*
129900080703      * Scrittura (WRITE) del record (1 o 2) nel file FITR6           *
130000030930      *---------------------------------------------------------------*
130100030930     c     WrtTR6rec     BegSr
130200030930      *
130300030930      * - progressivo del segnacollo in esame
130400050621     c* Se oltre 999 --> lascio 999
130500050621     c                   if        tr6nc1<999
130600030930     c                   eval      TR6nc1    =  TR6nc1 + 1
130700050621     c                   endif
130800030930      * - calcolo il "chi sono" (comprensivo di check-digit)
130900030930     c                   exsr      sr_CHISONO
131000030930      *
131100080703     c                   WRITE(E)  FITR6
131200030930      *
131300030930     c                   EndSr
131400030729      *---------------------------------------------------------------*
131500030729      * Impostazione del segnacollo "CHI SONO" (con check digit)      *
131600030729      *---------------------------------------------------------------*
131700030729     c     sr_CHISONO    BegSr
131800030729      *
131900030729      * Costruisco il codice a barre: FLS+LNA+NRS+NSC+ZNC+CKD
132000030729      * Mancano solo NSC e CKD:
132100030729      * - NSC l'ho appena calcolato in TR6ncd
132200030729      * - chiamo TRUL28R per ricavarne il check-digit
132300030729     c                   eval      BARcdeNSC =  TR6ncd
132400030729     c                   reset                   TRUL28ds
132500030930     c                   movel     BarCodeDS1    I28cod
132600030729     c                   call      'TRUL28R1'
132700030729     c                   parm                    TRUL28DS
132800030729if  1c                   if        O28err    =  *blanks
132900030729     c                   movel     O28cod        TR6who
133000030729e   1c                   endif
133100030729      *
133200030729     c                   EndSr
133300030930
133400030930      *---------------------------------------------------------------*
133500030930      * Impostazione del barcode spedizione per DDT SÌ                *
133600030930      *---------------------------------------------------------------*
133700030930     c     sr_DDTSI      BegSr
133800030930      *
133900030930      * Costruisco il codice a barre: AAS+LNP+NRS+NSP+CKD
134000030930      * (chiamo il pgm TRUL28R per ricavarne il check digit)
134100030930     c                   clear                   BarCodeDS2
134200030930     c                   z-add     ARBaas        BARcdeAAS
134300030930     c                   eval      BARcdeLNP  = ARBlnp
134400030930     c                   eval      BARcdeNRS2 = ARBnrs
134500030930     c                   eval      BARcdeNSP  = ARBnsp
134600030930      *
134700030930     c                   reset                   TRUL28ds
134800030930     c                   movel     BarCodeDS2    I28cod
134900030930     c                   call      'TRUL28R1'
135000030930     c                   parm                    TRUL28DS
135100030930      *
135200030930if  1c                   if        O28err     = *blanks
135300030930     c                   movel     O28cod        TR6who
135400030930x   1c                   else
135500030930     c                   clear                   TR6who
135600030930e   1c                   endif
135700030930      *
135800030930     c                   EndSr
