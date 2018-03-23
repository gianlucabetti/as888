000100160506     /*END
000200160506
000300031013     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000400030729
000500171113      * FIMS77R *----------------------------------------------------*
000600080701      *         Scrive da FNARB a FITR600F   per Picking su Nastro   *
000700030729      *--------------------------------------------------------------*
000800030729
000900030930     fFNARB25L  if   e           k disk
001000030729     fFIAPD01L  if   e           k disk
001100171110     fFLTR902L  if   e           k disk
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
003300171110      *//* P.O. da gestire come NO VdL
003400171110     d*// C_PO_a          c                   const(006)
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
018300171110     d £TipImp         s              1    inz
018400030729      * - indici di schiera / memo progressivi in ciclo DO
018500030729     d xx              s              3  0 inz
018600030729     d xy              s              3  0 inz(1)
018700030729     d yy              s              3  0 inz(1)
018800031001      * - flags booleani
018900040310     d $VdL            s              1    inz(*off)
019000031001     d $FLTR9          s              1    inz(*off)
019100040310     d $Giacenza       s              1    inz(*off)
019200040406     d $C_Serviz       s              1    inz(*off)
019300030729      * - campi per test "rottura"
019400030729     d wPO             s                   like(ARBlna) inz(*hival)
019500030729     d SAVEpdc         s                   like(ARBpdc) inz(*hival)
019600030729     d SAVEcbo         s                   like(ARBcbo) inz(*hival)
019700161109     d
019800161109     d wanomale        s              1
019900161109     d Dataiso         s               d   datfmt(*iso) inz(*sys)
020000030729
020100030729      *===============================================================*
020200030729      *  RIEPILOGO INDICATORI                                         *
020300030729      *---------------------------------------------------------------*
020400030729      * 10    - Comodo                                                *
020500030729      *===============================================================*
020600030729
020700030729     c     *Entry        plist
020800080702     c                   parm                    codicepc          1
020900080702     c                   parm                    filiale           3
021000080703     c                   parm                    catfoglio         1
021100080703     c                   parm                    numfoglio         7
021200171110     c                   parm                    TipoLavoraz       2
021300030729      *
021400030729      * Key-List
021500031013      * - file FLTR901L
021600171110     c     K04tr902      klist
021700031013     c                   kfld                    £PO
021800031013     c                   kfld                    £TipFV
021900031013     c                   kfld                    £NumFV
022000171110     c                   kfld                    £TipImp
022100030930      * - file FNARB25L
022200030930     c     K02arb25      klist
022300030730     c                   kfld                    L6(xx)
022400030930     c                   kfld                    wDCM
022500161109      * - file FNARB25L  le consegnate degli ultimi 3 giorni
022600161109     c     K02arb_c      klist
022700161109     c                   kfld                    L6(xx)
022800161109     c                   kfld                    kDCM_c
022900030729      * - file FIAPD01L
023000030729     c     K02apd01      klist
023100030729     c                   kfld                    APDtip
023200030729     c                   kfld                    ARBpdc
023300030930      * - file FNFVV03L
023400030930     c     K03fvv03      klist
023500030930     c                   kfld                    FVVfgs
023600030930     c                   kfld                    FVVnpg
023700030930     c                   kfld                    FVVnfv
023800060217      * - file FiAR401L
023900030729     c     K05ar401      klist
024000030729     c                   kfld                    ARBaas
024100030729     c                   kfld                    ARBlnp
024200030729     c                   kfld                    ARBnrs
024300030729     c                   kfld                    ARBnsp
024400030729     c                   kfld                    AR4trc
024500040318      * - file FNART01L - FNLBL01L
024600040318     c     K04sped       klist
024700030729     c                   kfld                    ARBaas
024800030729     c                   kfld                    ARBlnp
024900030729     c                   kfld                    ARBnrs
025000030729     c                   kfld                    ARBnsp
025100040318      * - file FNARB01L
025200040318     c     K04arb01      klist
025300040318     c                   kfld                    LBLaap
025400040318     c                   kfld                    LBLlpp
025500040318     c                   kfld                    LBLnrp
025600040318     c                   kfld                    LBLnsp
025700050331      * - file TIGCP21L
025800040310     c     K05gca01      klist
025900040310     c                   kfld                    ARBaas
026000040310     c                   kfld                    ARBlnp
026100040310     c                   kfld                    ARBnrs
026200040310     c                   kfld                    ARBnsp
026300040310     c                   kfld                    GCPfrg
026400030729      * - file TABEL00F
026500030729     c     K03tab00      klist
026600030729     c                   kfld                    TBLkut
026700030729     c                   kfld                    TBLcod
026800030729     c                   kfld                    TBLkey
026900040406     c     K02tab00      klist
027000040406     c                   kfld                    TBLkut
027100040406     c                   kfld                    TBLcod
027200030729      *
027300030729      * Operazioni Iniziali
027400030729     c                   exsr      OPE_INI
027500031001      * Intabellamento dati del file FLTR9
027600031001     c                   exsr      IntabFLTR9
027700030729      *
027800030729      * Ciclo di gestione per p.o. (£1 del P.O. elaboratore)
027900030729do  1c                   DO        30            xx
028000030729      *
028100030730if  2c                   if        L6(xx)    =  *zeros
028200030729     c                   leave
028300030729e   2c                   endif
028400161109     c
028500161109     c                   eval      wanomale=' '
028600161110     c                   eval      arbcca=' '
028700030729      *
028800031001      * Posizionamento iniziale sul file FNARB
028900030930     c     K02arb25      setll     FNARB000
029000030930     c     K02arb25      reade     FNARB000
029100030729      *
029200031001      * Ciclo di elaborazione del file FNARB
029300031001do  2c                   Dow       NOT %eof(FNARB25L)
029400040318      *
029500030729     c                   exsr      sr_ELAB
029600030930     c     K02arb25      reade     FNARB000
029700040414      *
029800031001e   2c                   EndDo
029900161109
030000161109     c                   eval      wanomale='S'
030100161109      * elaboro anche le spedizioni consegnate degli ultimi 3 giorni
030200161109     c     K02arb_c      setll     FNARB000
030300161109     c     l6(xx)        reade     FNARB000
030400161109      *
030500161109      * Ciclo di elaborazione del file FNARB
030600161109do  2c                   Dow       NOT %eof(FNARB25L)
030700161109      *
030800161109     c                   exsr      sr_ELAB
030900161109     c     l6(xx)        reade     FNARB000
031000161109      *
031100161109e   2c                   EndDo
031200030729      *
031300030729e   1c                   ENDDO
031400030729      *
031500030729      * Fine
031600030729     c                   clear                   TRUL28ds
031700030729     c                   eval      I28tla = 'C'
031800030729     c                   call      'TRUL28R1'
031900030729     c                   parm                    TRUL28DS
032000161007
032100161007     c                   clear                   fnlgi1ds
032200161007     c                   eval      ILGI1TLA= 'C'
032300161007     c                   movel     fnlgi1ds      kpjbu
032400161007     c                   call      'FNLGI1R'
032500161007     c                   parm                    kpjba
032600030729     c                   eval      *inLR  = *on
032700030729
032800030729      *---------------------------------------------------------------*
032900030729      * OPERAZIONI INIZIALI                                           *
033000030729      *---------------------------------------------------------------*
033100030729     c     OPE_INI       BegSr
033200030729      *
033300080702     c                   move      filiale       £po
033400080703     c                   move      catfoglio     £tipfv
033500080703     c                   move      numfoglio     £numfv
033600171110     c                   if            TipoLavoraz = '20'
033700171110     c                             or  TipoLavoraz = '23'
033800171110     c                             or  TipoLavoraz = '26'
033900171110     c                             or  TipoLavoraz = '27'
034000171110     c                             or  TipoLavoraz = '36'
034100171110     c                   eval      £TipImp = 'V'
034200171110     c                   else
034300171110     c                   eval      £TipImp = 'R'
034400171110     c                   endif
034500030729      *
034600040406      * Imposto l'utente fisso in file TABEL00F
034700040406     c                   eval      TBLkut = 1
034800030930      * Imposto la categoria dei fogli da elaborare
034900040310     c                   eval      FVVnpg =  4
035000030729      * Imposto il tipo anagrafica per il file FIAPD01L
035100030729      *   "Anagrafica Autotrasportatori/Coop"
035200030729     c                   eval      APDtip = 'A'
035300040310      * Imposto il n° progressivo di riapertura giacenza
035400040310     c                   clear                   GCPfrg
035500030729      * Imposto la KPJBA
035600030729     c                   reset                   KPJBA
035700030729     c                   eval      KNMUS = SDSusr
035800030729     c                   eval      KNMTD = SDSjob
035900030729     c                   eval      KNRJO = SDSjnr
036000030729     c                   eval      KNMEB = SDSjob
036100040310      *
036200040310     c                   reset                   $VdL
036300040310      *
036400040310      * Controllo se P.O. in gestione con VDL
036500040310     c                   clear                   dMFP
036600040310     c                   reset                   TIBS02ds
036700040310     c                   movel     £po           T02ke1
036800040310     c                   call      'TIBS02R'
036900040310     c                   parm                    KPJBA
037000040310     c                   parm                    TIBS02ds
037100040310if  1c                   if        T02err = *blanks
037200040310     c                   movel     T02uni        dMFP
037300040310e   1c                   endif
037400060214      * - NON per i P.O. "elencati" in C_PO_...
037500171110if  1c*//                if        £po   <> C_PO_a
037600040310     c     'V'           lookup    §MFPap                                 10
037700040310     c                   movel     *in10         $VdL
037800171110e   1c*//                endif
037900160506      *
038000160506     c     £po           chain     AZORG
038100160506if  1c                   if            %found(AZORG01L)
038200160506     c                             and ORGfva = *blanks
038300160506     c                   movel     ORGdf0        Og150
038400160506e   1c                   endif
038500030729      *
038600030729      * Reperisco tab. "3I" = variabili pgm partenze
038700030729     c                   clear                   ds3I
038800030729     c                   eval      TBLcod = '3I'
038900030729     c                   movel(p)  '1'           TBLkey
039000030729     c     K03tab00      chain     TABEL
039100030729if  1c                   if            %found(TABEL00F)
039200030729     c                             and TBLflg = *blanks
039300030729     c                   movel     TBLuni        ds3I
039400030729e   1c                   endif
039500030729      * Carico tabella P.O. gestiti
039600030729     c                   reset                   Trul06ds
039700030729     c                   movel     £po           D06key
039800030729     c                   movel(p)  Trul06ds      KPJBU
039900030729     c                   call      'TRUL06R'
040000030729     c                   parm                    KPJBA
040100030729     c                   movel     KPJBU         Trul06ds
040200030730     c                   movea     lia           L6
040300030729      *
040400030729      * Reperisco i network corrispondenti
040500030730     c                   clear                   L6ntw
040600030729do  1c                   DO        30            xx
040700030730if  2c                   if        L6(xx) = *zeros
040800030729     c                   leave
040900030729e   2c                   endif
041000030730     c     L6(xx)        chain     AZORG
041100030729if  2c                   if            %found(AZORG01L)
041200030729     c                             and ORGfva = *blanks
041300030729     c                   movel     ORGde3        OG143
041400030730     c                   movel     §OGntw        L6ntw(xx)
041500030729e   2c                   endif
041600030729e   1c                   ENDDO
041700040406      *
041800040407      * Intabello i tipi bolla c/servizio
041900040406     c                   reset                   $TBrC
042000040406     c                   clear                   xx
042100040406     c                   clear                   dsTB
042200040406     c                   eval      TBLcod = 'TB'
042300040406     c     K02tab00      setll     TABEL
042400040406     c     K02tab00      reade     TABEL
042500040406do  1c                   dow       NOT %eof(TABEL00F)
042600040406     c                             and xx < %elem($TBrC)
042700040406if  2c                   if        TBLflg = *blanks
042800040406     c                   movel     TBLuni        dsTB
042900040406if  3c                   if        §TBrbl = 'C'
043000040406     c                   add       1             xx
043100040406     c                   movel     TBLkey        $TBrC(xx)
043200040406e   3c                   endif
043300040406e   2c                   endif
043400040406     c     K02tab00      reade     TABEL
043500040406e   1c                   enddo
043600161109      *
043700161109     c* Imposto la data di oggi  - 3 gg lavorativi
043800161109     c                   clear                   xgiolavds
043900161109     c                   movel     dataiso       IXGLDATA
044000161109     c                   movel     £po           IXGLFIL
044100161109     c                   eval      IXGLSUB='S'
044200161109     c                   eval      IXGLGG=3
044300161109     c                   eval      IXGLLAV='S'
044400161109     c                   call      'XGIOLAV'
044500161109     c                   parm                    xgiolavds
044600161109     c
044700161109     c                   eval      kdcm_C=OXGLDATA
044800030729      *
044900030729     c                   EndSr
045000031001
045100031001      *---------------------------------------------------------------*
045200031001      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL PIANO DI SMISTAMENTO  *
045300031001      *---------------------------------------------------------------*
045400031001     c     IntabFLTR9    BegSr
045500031001      *
045600031001     c                   clear                   xy
045700040413     c                   clear                   xx
045800040413     c                   clear                   yy
045900040413     c                   clear                   $PosIMx
046000040413     c                   clear                   $PosBau
046100040413     c                   clear                   $BauIMx
046200031001      *
046300031001      * Ciclo di lettura del file FLTR9 per memorizzare
046400031001      *   tipo e numero foglio viaggio x P.O. gestione
046500171110     c     K04TR902      setll     FLTR9000
046600171110     c     K04TR902      reade     FLTR9000
046700031001      *
046800171110do  1c                   dow           NOT  %eof(FLTR902L)
046900040310     c                             and xy < %elem($TR9nfv)
047000031001      *
047100040310sel 2c                   select
047200040413      * - baia non letti (VdL)
047300040310w   2c                   when      TR9bnl  = 'S'
047400040310     c                   movel     TR9pep        bnlTR9pep
047500040414      * - baia "IM*"
047600040413w   2c                   when      TR9npg  = 3
047700040413if  3c                   if        imaTR9nfv = *zeros
047800031001     c                   movel     FLTR9ds       FLTR9ima
047900040413e   3c                   endif
048000040414      * - - cerco la postazione IM* per memorizzarne le baie
048100040413     c                   eval      xx      = 1
048200040413     c                   eval      *in10   = *off
048300040413     c     TR9bai        lookup    $PosIMx(xx)                            10
048400040413if  3c                   if        NOT *in10
048500040413     c                   eval      xx      = 1
048600040413     c     *blanks       lookup    $PosIMx(xx)                            10
048700040413if  4c                   if        *in10   = *on
048800040413     c                   movel     TR9bai        $PosIMx(xx)
048900040413e   4c                   endif
049000040413e   3c                   endif
049100040414      * - - per VdL memorzzo la baia della postazione IM* in esame
049200040414if  3c                   if            $VdL    = *on
049300171110     c                             and £TipImp = 'V'
049400171110     c                             and %subst( TR9des : 28 : 1 ) = 'V'
049500040414     c                             and *in10   = *on
049600040413     c                   movea     $PosBau(xx)   $BauIMx
049700040413     c                   eval      yy      = 1
049800040413     c                   eval      *in10   = *off
049900040413     c                   movel     TR9pep        wBAU
050000040413     c     wBAU          lookup    $BauIMx(yy)                            10
050100040413if  4c                   if        NOT *in10
050200040413     c                   eval      yy      = 1
050300040413     c     *blanks       lookup    $BauIMx(yy)                            10
050400040413if  5c                   if        *in10   = *on
050500040413     c                   movel     TR9pep        $BauIMx(yy)
050600040414     c                   movea     $BauIMx       $PosBau(xx)
050700040413e   5c                   endif
050800040413e   4c                   endif
050900040413e   3c                   endif
051000040414      * - baia non "IM*"
051100040310w   2c                   when      TR9npg  = 4
051200031001     c                   add       1             xy
051300031013     c                   z-add     TR9nfv        $TR9nfv(xy)
051400031013     c                   movel     FLTR9ds       $TR9rec(xy)
051500040310      *
051600040310e   2c                   endsl
051700031001      *
051800171113     c     K04TR902      reade     FLTR9000
051900031001e   1c                   enddo
052000040414      *
052100040414     c                   reset                   xx
052200040414     c                   reset                   yy
052300031001      *
052400031001     c                   EndSr
052500030729
052600030729      *---------------------------------------------------------------*
052700030729      * ELABORAZIONE SINGOLO RECORD DEL FILE FNARB25L                 *
052800030729      *---------------------------------------------------------------*
052900030729     c     sr_ELAB       BegSr
053000040310      *
053100040310     c                   reset                   $FLTR9
053200040310     c                   reset                   $Giacenza
053300040406     c                   reset                   $C_Serviz
053400040318      *
053500040319      * Scarto le spedizioni mamma dato che è la figlia che viene messa
053600040319      *  in distinta
053700161109     c                   if        wanomale=' '
053800040319     c     K04sped       chain     FNLBL002
053900040319     c                   IF        %FOUND(FNLBL02L)
054000040318     c                   goto      NO_elab
054100040318     c                   ENDIF
054200161109     c
054300161109     c                   else
054400161109     c                   if        arbcca<>'1' and arbcca<>'2' and arbcca<>'6'
054500161109     c                   goto      NO_elab
054600161109     c                   ENDIF
054700161109     c                   endif
054800161109      *
054900030729      *
055000030729      * Pulisco dati reperiti solo in casi specifici
055100030729     c                   clear                   FNARTds
055200060217     c                   clear                   FiAR4ds
055300030930     c                   clear                   dsBL4A
055400030729      *
055500030729      * Reperisco dati del p.o. segnacollo
055600030729     c                   eval      wPO    =  ARBfls
055700030729     c                   exsr      RepMemoPO
055800030729     c                   movel     $POd(yy)      AZORGfls
055900030729      *
056000030729      * Reperisco dati del p.o. di partenza
056100030729     c                   eval      wPO    =  ARBlnp
056200030729     c                   exsr      RepMemoPO
056300030729     c                   movel     $POd(yy)      AZORGlnp
056400030729      *
056500030729      * Reperisco dati del p.o. di arrivo
056600030729     c                   eval      wPO    =  ARBlna
056700030729     c                   exsr      RepMemoPO
056800030729     c                   movel     $POd(yy)      AZORGlna
056900030729      *
057000030729      * Reperisco decodifica Autotrasportatore consegna
057100030729if  1c                   if        ARBpdc    <> SAVEpdc
057200030729     c                   eval      SAVEpdc   =  ARBpdc
057300030729     c                   exsr      RepMemoPDC
057400030729e   1c                   endif
057500030729      *
057600030729      * Reperisco tabella Codici bolla
057700030729if  1c                   if        ARBcbo    <> SAVEcbo
057800030729     c                   eval      SAVEcbo   =  ARBcbo
057900030729     c                   exsr      RepMemoT3A
058000030729e   1c                   endif
058100031001      *
058200031001      * Reperisco dati del piano di smitamento (da file FLTR9)
058300161110if  1c                   if        ARBndc    >  *zeros  and arbdcm=0
058400031001     c                   exsr      FVinFLTR9
058500031013e   1c                   endif
058600040310      *
058700040414      * Se TR6bai = "IM*" (se ARBNDC = *zero            oppure
058800040310      *                    se distinta NON trovata su FLTR900F)
058900040310if  1c                   if           ARBndc = *zeros
059000161110     c                             or ($FLTR9 = *off  and arbdcm=0)
059100040406      *   verifico se esiste giacenza aperta con spedizione
059200040310     c                   exsr      ChkGiacenza
059300040407      *   controllo se c/servizio
059400040406     c                   exsr      ChkC_Serv
059500040310e   1c                   endif
059600030729      *
059700080703      * Preparo e scrivo il record in FITR600F
059800030729     c                   exsr      WriteFLTR6
059900030729      *
060000031028     c     NO_elab       EndSr
060100030729
060200030729      *---------------------------------------------------------------*
060300030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL P.O.                  *
060400030729      *---------------------------------------------------------------*
060500030729     c     RepMemoPO     BegSr
060600030729      *
060700030729      * Controllo se diverso con l'ultimo reperito
060800031001if  1c                   if        wPO    <> $PO(yy)
060900030729      *
061000030729      * Controllo se già elaborato
061100030729     c                   eval      *in10   = *off
061200030729     c                   eval      yy      = 1
061300030729     c     wPO           lookup    $PO(yy)                                10
061400030729      *
061500030729      * Reperimento dati se non ancora elaborato
061600030729if  2c                   if        not *in10
061700030729      *
061800030729     c     wPO           chain     AZORG
061900030729if  3c                   if        not %found(AZORG01L)
062000030729     c                             or  ORGfva <> *blanks
062100030729     c                   clear                   AZORGds
062200030729e   3c                   endif
062300030729      * - Memorizzazione dati:
062400030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
062500030729     c     *zeros        lookup    $PO(yy)                                10
062600030729if  3c                   if        not *in10
062700030729     c                   eval      yy      = %elem($PO)
062800030729e   3c                   endif
062900030729     c                   eval      $PO(yy) = wPO
063000030729     c                   movel     AZORGds       $POd(yy)
063100030729      *
063200030729e   2c                   endif
063300030729      *
063400030729e   1c                   endif
063500030729      *
063600030729     c                   EndSr
063700030729
063800030729      *---------------------------------------------------------------*
063900030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI PADRONCINI                *
064000030729      *---------------------------------------------------------------*
064100030729     c     RepMemoPDC    BegSr
064200030729      *
064300030729      * Controllo se già elaborato
064400030729     c                   eval      *in10   =  *off
064500030729     c                   eval      xy      =  1
064600030729     c     SAVEpdc       lookup    $PDC(xy)                               10
064700030729      *
064800030729      * Reperimento dati se non ancora elaborato
064900030729if  1c                   if        not *in10
065000030729      *
065100030729     c     K02apd01      chain     FIAPD000
065200030729if  2c                   if        not %found(FIAPD01L)
065300030729     c                             or  APDatb <> *blanks
065400030729     c                   clear                   APDrsc
065500030729e   2c                   endif
065600030729      * - Memorizzazione dati:
065700030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
065800030729     c     *hival        lookup    $PDC(xy)                               10
065900030729if  2c                   if        not *in10
066000030729     c                   eval      xy       = %elem($PDC)
066100030729e   2c                   endif
066200030729     c                   eval      $PDC(xy) = SAVEpdc
066300030729     c                   movel     APDrsc        $PDCd(xy)
066400030729      *
066500030729      * RE-impostazione dati se già elaborato
066600030729x   1c                   else
066700030729      *
066800030729     c                   movel     $PDCd(xy)     APDrsc
066900030729      *
067000030729e   1c                   endif
067100030729      *
067200030729     c                   EndSr
067300030729
067400030729      *---------------------------------------------------------------*
067500030729      * REPERIMENTO (E MEMORIZZAZIONE) TAB.3A = CODICI BOLLA          *
067600030729      *---------------------------------------------------------------*
067700030729     c     RepMemoT3A    BegSr
067800030729      *
067900030729      * Controllo se già elaborato
068000030729     c                   eval      *in10   =  *off
068100030729     c                   eval      xy      =  1
068200030729     c     SAVEcbo       lookup    $3A(xy)                                10
068300030729      *
068400030729      * Reperimento dati se non ancora elaborato
068500030729if  1c                   if        not *in10
068600030729      *
068700030729     c                   clear                   ds3A
068800030729     c                   eval      TBLcod = '3A'
068900030729     c                   movel(p)  SAVEcbo       TBLkey
069000030729     c     K03tab00      chain     TABEL
069100030729if  2c                   if        %found(TABEL00F)
069200030729     c                             and TBLflg = *blanks
069300030729     c                   movel     TBLuni        ds3A
069400030729e   2c                   endif
069500030729      * - Memorizzazione dati:
069600030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
069700030729     c     *hival        lookup    $3A(xy)                                10
069800030729if  2c                   if        not *in10
069900030729     c                   eval      xy      =  %elem($3A)
070000030729e   2c                   endif
070100030729     c                   eval      $3A(xy) =  SAVEcbo
070200030729     c                   movel     ds3A          $3Ads(xy)
070300030729      *
070400030729      * RE-impostazione dati se già elaborato
070500030729x   1c                   else
070600030729      *
070700030729     c                   movel     $3Ads(xy)     ds3A
070800030729      *
070900030729e   1c                   endif
071000030729      *
071100030729     c                   EndSr
071200031001
071300031001      *---------------------------------------------------------------*
071400031013      * RICERCA DEL F.V. IN FLTR9                                     *
071500031001      *---------------------------------------------------------------*
071600031001     c     FVinFLTR9     BegSr
071700050517     c                   clear                   dfvvspn
071800031001      *
071900031001      * Reperisco dati da schiera
072000031001     c                   eval      *in10     =  *off
072100031001     c                   eval      xy        =  1
072200031013     c     ARBndc        lookup    $TR9nfv(xy)                            10
072300031001     c                   eval      $FLTR9    =  *in10
072400031001      *
072500031001if  1c                   if        $FLTR9    =  *off
072600031013      * Impostazione dati per Tipo/Numero F.V. non reperito
072700031001     c                   exsr      RepFNFVV
072800031001x   1c                   else
072900031013      * RE-impostazione dati per Tipo/Numero F.V. reperito
073000031013     c                   movel     $TR9rec(xy)   FLTR9ds
073100050517     c
073200160506     c* chain comunque in fnfvv per avere tipo distinta parcel/messagg
073300050517     c                   z-add     ARBifp        FVVfgs
073400050517     c                   z-add     ARBndc        FVVnfv
073500050517     c     K03fvv03      chain     FNFVV000
073600050517if  1c                   if            %found(FNFVV03L)
073700050517     c                   movel     fvvspn        dfvvspn
073800050517     c                   endif
073900050517     c
074000031001e   1c                   endif
074100031001      *
074200031001     c                   EndSr
074300030930
074400030930      *---------------------------------------------------------------*
074500031001      * REPERIMENTO DATI DAI FOGLI VARI                               *
074600030930      *---------------------------------------------------------------*
074700031001     c     RepFNFVV      BegSr
074800030930      *
074900030930     c                   clear                   wDFVgma
075000030930     c                   clear                   FVVdfv
075100030930      *
075200030930     c                   z-add     ARBifp        FVVfgs
075300030930     c*** (già fatto)    z-add     4             FVVnpg
075400031013     c                   z-add     ARBndc        FVVnfv
075500030930     c     K03fvv03      chain     FNFVV000
075600031013if  1c                   if            %found(FNFVV03L)
075700031013     c                             and FVVatb =  *blanks
075800030930     c                   clear                   WLBdat
075900030930     c                   eval      G08inv    =  FVVdfv
076000030930     c                   eval      G08err    =  '3'
076100030930     c                   call      'XSRDA8'
076200030930     c                   parm                    WLBdat
076300030930     c                   eval      wDFVgma   =  G08dat
076400050517     c                   movel     fvvspn        dfvvspn
076500031001e   1c                   endif
076600030930      *
076700030930     c                   EndSr
076800040310
076900040310      *---------------------------------------------------------------*
077000040310      * VERIFICA SE ESISTE GIACENZA APERTA PER SPEDIZIONE             *
077100040310      *---------------------------------------------------------------*
077200040310     c     ChkGiacenza   BegSr
077300040310      *
077400050331     c     K05gca01      chain     Tigcp21l
077500050331if  1c                   if            %found(Tigcp21l)
077600040310     c                             and GCPatb = *blanks
077700050331     c                             and (GCPfas < 035 or
077800050331     c                                  GCPfas = 036)
077900161007     c* solo se merce da portare a IMG con driver
078000161007     c                   clear                   fnlgi1ds
078100161007     c                   eval      ILGI1AAS=arbaas
078200161007     c                   eval      ILGI1lnp=arblnp
078300161007     c                   eval      ILGI1nrs=arbnrs
078400161007     c                   eval      ILGI1nsp=arbnsp
078500170330     c* Passo la filiale gestione e  flag picking
078600170330     c                   movel     £po           ILGI1FGS
078700170330     c                   eval      ILGI1PICK=Og150.§OGpck
078800170330
078900170330     c                   if        ILGI1PICK=' '
079000170330     c                   eval      ILGI1PICK='N'
079100170330     c                   endif
079200170330
079300161007     c                   movel     fnlgi1ds      kpjbu
079400161007     c                   call      'FNLGI1R'
079500161007     c                   parm                    kpjba
079600161007     c                   parm                    tigcp00ds
079700161007     c                   movel     kpjbu         fnlgi1ds
079800161007
079900161007    2c                   if        olgi1AMG='S' or olgi1AMG='Y'
080000161007     c                             or olgi1inmg='S'
080100040310     c                   eval      $Giacenza  = *on
080200161007e   2c                   endif
080300160823e   1c                   endif
080400040310      *
080500040310     c                   EndSr
080600040406
080700040406      *---------------------------------------------------------------*
080800040407      * VERIFICA SE BOLLA C/SERVIZIO                                  *
080900040406      *---------------------------------------------------------------*
081000040406     c     ChkC_Serv     BegSr
081100040406      *
081200040406     c                   eval      *in10     =  *off
081300040406if  1c                   if        §3Atb2    =  *blanks
081400040406     c     §3Atb1        lookup    $TBrC                                  10
081500040406x   1c                   else
081600040406     c     §3Atb2        lookup    $TBrC                                  10
081700040406e   1c                   endif
081800040406     c                   eval      $C_Serviz =  *in10
081900040406      *
082000040406     c                   EndSr
082100030729
082200030729      *---------------------------------------------------------------*
082300080703      * PREPARAZIONE E SCRITTURA DEL RECORD IN FITR600F               *
082400030729      *---------------------------------------------------------------*
082500030729     c     WriteFLTR6    BegSr
082600030729      *
082700080703     c                   clear                   FITR6
082800160506      *
082900080703     c                   eval      tr6fil=filiale
083000080703     c                   eval      tr6cpc=codicepc
083100031024      *
083200060217      * aggancio FiAR4
083300031024     c                   clear                   dsBL4A
083400060217if  1c                   if        not %open(FiAR401L)
083500060217     c                   open      FiAR401L
083600031024e   1c                   endif
083700031024     c                   eval      AR4trc    =  'A'
083800060217     c     K05ar401      chain     FiAR4000
083900060217if  1c                   if        %found(FiAR401L)
084000031024     c                   movel     AR4not        dsBL4A
084100031024e   1c                   endif
084200030729      *
084300030729      * segnacollo "chi sono"
084400030729      * (imposto i dati validi per tutti i segnacolli della spedizione;
084500030729      *  il pgm. TRUL28R verrà richiamato per ogni collo)
084600030930     c                   clear                   BarCodeDS1
084700030729     c                   eval      BARcdeFLS = ARBfls
084800030729     c                   eval      BARcdeLNA = ARBlna
084900030729     c                   eval      BARcdeNRS = ARBnrs
085000030729     c                   eval      BARcdeZNC = ARBznc
085100030729      * secondi di attesa dopo stampa
085200030729     c                   eval      TR6sea    =  §3Isea
085300030729      * flag segnacollo da stampare ('S'/' ')
085400030729     c                   eval      TR6st1    =  'S'
085500030729      * flags
085600030729     c                   eval      TR6fl1    =  *off
085700030729      * codice cliente
085800030729if  1c                   if        ARBccm    >  *zeros
085900030729     c                   eval      TR6ksc    =  ARBccm
086000030729x   1c                   else
086100030729     c                   eval      TR6ksc    =  ARBksc
086200030729e   1c                   endif
086300030729      * p.o. partenza segnacollo
086400030729     c                   eval      TR6fls    =  ARBfls
086500030729      * descrizione del p.o. partenza segnacollo
086600030729     c                   eval      TR6dlp    =  flsORGdes
086700030729      * codice prodotto - default
086800030729     c                   eval      TR6cdp    =  ARBrma
086900030729      * numero serie segnacollo
087000030729     c                   eval      TR6nrs    =  ARBnrs
087100030729      * p.o. di arrivo
087200030729     c                   eval      TR6lna    =  ARBlna
087300030729      * descrizione p.o. arrivo spedizione
087400030729if  1c                   if        lnaORGde5 <> *blanks
087500030729     c                   eval      TR6dla    =  lnaORGde5
087600030729x   1c                   else
087700030729     c                   eval      TR6dla    =  lnaORGdes
087800030729e   1c                   endif
087900030729      * terminal di arrivo
088000030729     c                   eval      TR6tna    =  ARBtfa
088100030729      * tipo servizio bolle
088200030729     c                   eval      TR6tsp    =  ARBtsp
088300030729      * zona segnacollo
088400030729     c                   eval      TR6zsc    =  ARBznc
088500030729      * totale segnacolli della spedizione
088600040728     C                   IF        ARBNCL < 999
088700030729     c                   eval      TR6nc2    =  ARBncl
088800040728     c                   ELSE
088900040728     c                   eval      TR6nc2    =  999
089000040728     c                   ENDIF
089100030729      * ragione sociale mittente
089200030729     c                   eval      TR6rsm    =  ARBrsm
089300030729      * provincia mittente
089400030729     c                   eval      TR6prm    =  ARBprm
089500030729      * ragione sociale destinatario
089600030729     c                   eval      TR6rsd    =  ARBrsd
089700030729      * c.a.p. destinatario
089800030729     c                   eval      TR6cad    =  ARBcad
089900030729      * indirizzo destinatario
090000030729     c                   eval      TR6ind    =  ARBind
090100030729      * località destinatario
090200030729     c                   eval      TR6lod    =  ARBlod
090300030729      * provincia destinatario
090400030729     c                   eval      TR6prd    =  ARBprd
090500030729      * volume
090600030729     c                   eval      TR6vol    =  ARBvlf
090700030729      * peso in kg
090800030729     c                   eval      TR6pkg    =  ARBpkf
090900030729      * flag volume bollettato
091000030729     c                   eval      TR6fvr    =  ARBfvf
091100030729      * anno spedizione
091200030729     c                   eval      TR6aas    =  ARBaas
091300030729      * meso/giorno spedizione
091400030729     c                   eval      TR6mgs    =  ARBmgs
091500030729      * tipo stampa: 'ITA' / 'EST' / 'ALL' / 'DIS'
091600030729     c                   eval      TR6tst    =  'DIS'
091700030930      *
091800030930      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
091900030930      *
092000030930      * Se ARBNDC = *zero            oppure
092100030930      * Se distinta NON trovata su FLTR900F
092200161110     c* Se bolla sto elaborando bolla consegnata con cons.anomala --> IMD
092300030930if  1c                   if           ARBndc = *zeros
092400031001     c                             or $FLTR9 = *off
092500161110     c                             or (arbcca<>*blanks  and arbdcm>0)
092600030930      *
092700030930      * - baia uscita
092800040414     c*** non qui:       eval      TR6bai    =  'IMA'
092900030930      * - p.o. distinta consegna
093000031001     c                   eval      TR6poc    =  imaTR9fgs
093100030930      * - categoria foglio viaggio
093200030930     c                   eval      TR6npg    =  3
093300031001      * - numero foglio viaggio
093400031001     c                   eval      TR6nfv    =  imaTR9nfv
093500031001      * - data foglio viaggio
093600031001     c                   eval      TR6dfv    =  imaTR9dfv
093700031022      * - flag prestazione padroncino (M-mattino, P-pomeriggio)
093800040310     c***                eval      TR6fpp    =  *blanks
093900030930      * - codice autotrasportatore consegna
094000030930     c***                eval      TR6pdc    =  *zeros
094100030930      * - descrizione autotrasportatore consegna
094200030930     c***                eval      TR6dpd    =  *blanks
094300031013      * - descrizioni per stampa
094400161110if  2c                   IF        ARBndc    =  *zeros  or
094500161110     c                             (arbdcm>0 and arbcca<>' ')
094600040406     c                   exsr      sr_DS1
094700160506      * -?distinta messaggerie  o  eventuale Turno?
094800161110      *  ?(per ARBNDC = *zero) oppure consegnata con cons anomala 1/2/6
094900160506if  3c                   If            Og150.§OGpkm   = 'S'
095000160506     c                             or  Og150.§OGturno = 'S'
095100160506     c                   clear                   ds03
095200160506     c                   eval      TBLcod = '03'
095300160506     c                   eval      TBLkey = %editc( ARBlna : 'X' ) +
095400160506     c                                      %editc( ARBznc : 'X' )
095500160506     c     K03tab00      chain     TABEL
095600160506if  4c                   if        %found(TABEL00F)
095700160506     c                             and TBLflg = *blanks
095800160506     c                   movel     TBLuni        ds03
095900160506e   4c                   endif
096000160506     c                   exsr      sr_MessagTurno
096100160506e   3c                   EndIf
096200160506x   2c                   ELSE
096300040414     c                   eval      TR6bai    =  'IM5'
096400031013     c                   eval      TR6ds1    =  'DISTINTA NON PRESENTE'
096500031013     c                                       +  ' NEL PIANO'
096600040130     c                   eval      TR6ds2    =  'Distinta '
096700031013     c                                       +  %editc(ARBndc  : 'Z')
096800031013     c                                       +  ' del '
096900031013     c                                       +  %editc(wDFVgma : 'Y')
097000160506      * -?distinta messaggerie  o  eventuale Turno?
097100160506      *  ?(per $FLTR9 = *off)?
097200160506     c                   exsr      sr_MessagTurno
097300160506e   2c                   ENDIF
097400040414      * - baia uscita per etichetta (postazione)
097500040414if  2c                   if        TR6bai    =  *blanks
097600040414     c                   eval      TR6bai    =  'IMA'
097700040414e   2c                   endif
097800040414     c                   exsr      sr_PosIMA
097900040310      * - baia uscita per picking VdL
098000171110if  2c                   if            $VdL    =  *on
098100171110     c                             and £TipImp = 'V'
098200040413     c                   exsr      sr_BauIMA
098300040414if  3c                   if        TR6bau    =  *blanks
098400040414     c                   eval      TR6bau    =  imaTR9pep
098500040414e   3c                   endif
098600040310e   2c                   endif
098700030930      *
098800030930      * Se ARBNDC > *zero            e
098900030930      * Se distinta trovata su FLTR900F
099000030930x   1c                   else
099100030930      *
099200030930      * - baia uscita
099300030930     c                   eval      TR6bai    =  TR9bai
099400030930      * - p.o. distinta consegna
099500030930     c                   eval      TR6poc    =  TR9fgs
099600030930      * - categoria foglio viaggio
099700030930     c                   eval      TR6npg    =  TR9npg
099800031001      * - numero foglio viaggio
099900031001     c                   eval      TR6nfv    =  TR9nfv
100000031001      * - data foglio viaggio
100100031001     c                   eval      TR6dfv    =  TR9dfv
100200031022      * - flag prestazione padroncino (M-mattino P-pomeriggio)
100300031022if  2c                   if        TR9fl1    =  'P'
100400031022     c                   eval      TR6fpp    =  TR9fl1
100500031022e   2c                   endif
100600030930      * - codice autotrasportatore consegna
100700030930     c                   eval      TR6pdc    =  TR9pdc
100800031024     c                   eval      TR6dpd    =  %subst(APDrsc:1:19)
100900160506      * -?distinta messaggerie  o  eventuale Turno?
101000160506      *  ?(per ARBNDC > *zero e $FLTR9 = *on)?
101100160506     c                   exsr      sr_MessagTurno
101200030930      * - 1ª descrizione per stampa
101300030930     c***                eval      TR6ds1    =  *blanks
101400030930      * - 2ª descrizione per stampa
101500030930     c***                eval      TR6ds2    =  *blanks
101600040310      * - baia uscita per picking VdL
101700171110if  2c                   if            $VdL    = *on
101800171110     c                             and £TipImp = 'V'
101900171113     c                             and %subst( TR9des : 28 : 1 ) = 'V'
102000040310     c                   eval      TR6bau    =  TR9pep
102100040310e   2c                   endif
102200030930      *
102300030930e   1c                   endif
102400030729      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
102500040318      * Se la spedizione non ha segnacolli ed ha una mamma aggancio la
102600040318      *  mamma per reperire i segnacolli solo se ha stessa LNA
102700040318if  1c                   IF        §3Abcm = 'S'     AND
102800040318     c                             ARBncd = *zeros  AND
102900040318     c                             ARBfns <> 'S'
103000040318     c     K04sped       chain     FNLBL000
103100040318if  2c                   IF        %found(FNLBL01L)  AND
103200040318     c                             LBLlan = LBLlap
103300040318     c     K04ARB01      chain     FNARBLEG
103400040319      * reimposto campi segnacollo e file
103500040319      *   non ricavo la descrizione del p.o. segnacollo per il picking
103600040319      *   non serve
103700040319if  3c                   IF        %found(FNARB01L)
103800040319     c                   clear                   BarCodeDS1
103900040319     c                   eval      TR6FLS    = ARBfls
104000040319     c                   eval      TR6NRS    = ARBnrs
104100040319     c                   eval      BARcdeFLS = ARBfls
104200040319     c                   eval      BARcdeLNA = ARBlna
104300040319     c                   eval      BARcdeNRS = ARBnrs
104400040319     c                   eval      BARcdeZNC = ARBznc
104500040319e   3c                   ENDIF
104600040319e   2c                   ENDIF
104700040318e   1c                   ENDIF
104800030729      *
104900030729      * cicli di WRITE dei record (1 x segnacollo)
105000040318      *  suddivisi x sequenziali e non sequenziali
105100030729sel 1c                   select
105200030729w   1c                   when      ARBncd >  *zeros
105300030729     c                   exsr      sr_SCS
105400030729w   1c                   when      ARBfns =  'S'
105500030729     c                   exsr      sr_SCNS
105600030729e   1c                   endsl
105700031006      *
105800031006      * eventuale WRITE dell'unico record x spedizione "DDT SÌ"
105900040310      *   (ultima, poiché re-imposta alcuni campi)
106000161111      *   non lo faccio per bolle con cons.anomala
106100161111    0c                   if        arbdcm=0
106200031006if  1c                   if           §B4abm =  'C'
106300031006     c                             or §B4abm =  'S'
106400031024     c                             or §B4abm =  'P'
106500031024     c                             or §B4abm =  'Q'
106600031013      * - - ripulisco i campi "inutili"
106700031013     c                   clear                   TR6ncd
106800031013     c                   clear                   TR6nc1
106900031006      * - - calcolo il check-digit per il barcode
107000031006     c                   exsr      sr_DDTSI
107100040310      * - - forzo gli "appositi" campi:
107200040310      * - - - baia uscita per picking VdL
107300171110if  2c                   if            $VdL    =  *on
107400171110     c                             and £TipImp = 'V'
107500040310     c                   eval      TR6bau    =  bnlTR9pep
107600040310e   2c                   endif
107700031006      *
107800080703     c                   WRITE(E)  FITR6
107900031006      *
108000031006e   1c                   endif
108100161111e   0c                   endif
108200030729      *
108300030729     c                   EndSr
108400040406
108500040406      *---------------------------------------------------------------*
108600040406      * Impostazione descrizione per stampa "DS1"                     *
108700040406      *---------------------------------------------------------------*
108800040406     c     sr_DS1        BegSr
108900040413      *
109000040407      * Imposto il campo TR6DS1 in base aiprimi 2?casi individuati
109100040406      *  (nel controllare 'ste 2 segnalazioni emergerebbero eventuali
109200040406      *  altri casi...)
109300040406      *
109400040414      *** - max:         eval      TR6ds1    =  '1234567890123456789'
109500040413      ***                                    +  '1234567890123456'
109600161110     c* Bolla resa o dirottata
109700161110     c                   if        arbcca<>' '  and arbdcm>0
109800161110if  2c                   if        TR6ds1    =  *blanks
109900161110     c                   eval      TR6bai    =  'IMD'
110000161110     c                   eval      TR6ds1    =  'RESO/DIR'
110100161110x   2c                   else
110200161110     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
110300161110     c                                       +  'RESO/DIR'
110400161110     c                   goto      End_DS1
110500161110e   2c                   endif
110600161110e   1c                   endif
110700040406      * Giacenza
110800040406if  1c                   if        $Giacenza =  *on
110900040406if  2c                   if        TR6ds1    =  *blanks
111000040414     c                   eval      TR6bai    =  'IM1'
111100040406     c                   eval      TR6ds1    =  'GIACENZA'
111200040406x   2c                   else
111300040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
111400040406     c                                       +  'GIACENZA'
111500040407     c                   goto      End_DS1
111600040406e   2c                   endif
111700040406e   1c                   endif
111800040406      * Fermo deposito
111900040406if  1c                   if        ARBffd    =  'S'
112000040406if  2c                   if        TR6ds1    =  *blanks
112100040414     c                   eval      TR6bai    =  'IM2'
112200040406     c                   eval      TR6ds1    =  'FERMO DEPOSITO'
112300040406x   2c                   else
112400040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
112500040406     c                                       +  'FERMO DEPOSITO'
112600040407     c                   goto      End_DS1
112700040406e   2c                   endif
112800040406e   1c                   endif
112900040406      * Supermercato
113000040406if  1c                   if        ARBtc1    =  'S'  or
113100040406     c                             ARBtc2    =  'S'
113200040406if  2c                   if        TR6ds1    =  *blanks
113300040414     c                   eval      TR6bai    =  'IM3'
113400040406     c                   eval      TR6ds1    =  'SUPERMERCATO'
113500040406x   2c                   else
113600040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
113700040406     c                                       +  'SUPERMERCATO'
113800040407     c                   goto      End_DS1
113900040406e   2c                   endif
114000040406e   1c                   endif
114100040406      * C/Servizio
114200040406if  1c                   if        $C_Serviz =  *on
114300040406if  2c                   if        TR6ds1    =  *blanks
114400040414     c                   eval      TR6bai    =  'IM4'
114500040413     c                   eval      TR6ds1    =  'C/SERVIZIO'
114600040406x   2c                   else
114700040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
114800040407     c                                       +  'C/SERVIZIO'
114900040407     c                   goto      End_DS1
115000040406e   2c                   endif
115100040406e   1c                   endif
115200040413      *
115300040413     c     End_DS1       EndSr
115400160506
115500160506      /free
115600160506
115700160506       //--------------------------------------------------------------
115800160510       //?Impostazione campo TR6FL3:                                   ?
115900160510       //?- "M" = Spedizione con zona consegna Messaggerie             ?
116000160510       //?- "1" = Spedizione nel 1° turno di smistamento colli         ?
116100160510       //?- "2" = Spedizione nel 2° turno di smistamento colli         ?
116200160510       //?- " " = Spedizione nel turno unico di smistamento colli      ?
116300160506       //--------------------------------------------------------------
116400160506       BEGSR  sr_MessagTurno;
116500160506
116600160506         // -?Se ARBNDC = *zero?
116700160506         //  ?(spedizioni in IMA - tranne "distinta non nel piano" [IM5])?
116800161110         //  ?                   - anche le cons anomale 1/2/6     [IM7])?
116900161110         If  ARBndc = *zeros or (arbdcm>0 and arbcca<>' ');
117000160506
117100160506           // ·?Distinta Messaggerie?
117200160506           if  Og150.§OGpkm   = 'S'  and  §03tpm = 'M';
117300160506             TR6fl3 = §03tpm;
117400160506           endif;
117500160506
117600160506           // ·?Turno?
117700160506           if  Og150.§OGturno = 'S'  and  TR6fl3 = *blanks;
117800160511             TR6fl3 = §03turno;
117900160506           endif;
118000160506
118100160506         // -?Altrimenti?
118200160506         //  ?(distinta  trovata  o  NON trovata [IM5]  su FLTR900F)?
118300160506         Else;
118400160506
118500160506           // ·?Distinta Messaggerie?
118600160506           if  Og150.§OGpkm   = 'S'  and  §FVVtpm = 'M';
118700160506             TR6fl3 = §FVVtpm;
118800160506           endif;
118900160506
119000160506           // ·?Turno?
119100160506           if  Og150.§OGturno = 'S'  and  TR6fl3  = *blanks;
119200160511             TR6fl3 = §FVVturno;
119300160506           endif;
119400160506
119500160506         EndIf;
119600160506
119700160506       ENDSR;
119800160506
119900160506      /end-free
120000040413
120100040413      *---------------------------------------------------------------*
120200040414      * Assegnazione postazione "IM*"                                 *
120300040413      *---------------------------------------------------------------*
120400040414     c     sr_PosIMA     BegSr
120500040413      *
120600040413     c                   eval      xy     = 1
120700040413     c                   eval      *in10  = *off
120800040414      *
120900040414      * Prima cerco la postazione specifica
121000040414      * SE non la trovo: cerco la postazione "IM0"
121100040414      * SE non trovo neppure quella: cerco la postazione "IMA"
121200040414      *   (che c'è sicuramente!!!)
121300040414do  1c                   dow       NOT *in10
121400040414      *
121500040414     c     TR6bai        lookup    $PosIMx(xy)                            10
121600040414      *
121700040414sel 2c                   select
121800040414w   2c                   when      NOT *in10 and TR6bai <> 'IM0'
121900040414     c                                       and TR6bai <> 'IMA'
122000040414     c                   eval      TR6bai = 'IM0'
122100171124w   2c                   when      NOT *in10 and TR6bai = 'IMA'
122200171124     c                   leave
122300040414w   2c                   when      NOT *in10
122400040414     c                   eval      TR6bai = 'IMA'
122500040414e   2c                   endsl
122600040414      *
122700040414e   1c                   enddo
122800040414      *
122900040414     c                   EndSr
123000040414
123100040414      *---------------------------------------------------------------*
123200040414      * Assegnazione baia di uscita corrispondente alla postaz. "IM*" *
123300040414      *---------------------------------------------------------------*
123400040414     c     sr_BauIMA     BegSr
123500040414      *
123600040414     c                   eval      xy     = 1
123700040414     c                   eval      *in10  = *off
123800040414     c     TR6bai        lookup    $PosIMx(xy)                            10
123900040414      * (lo trova sicuramente)
124000040414      *
124100040414if  1c                   if        *in10  = *on
124200040413     c                   movea     $PosBau(xy)   $BauIMx
124300040413     c                   eval      $LastBau(xy)    = $LastBau(xy) + 1
124400040414      *
124500040414if  2c                   if           $LastBau(xy) > %elem($BauIMx)
124600040413     c                             or $LastBau(xy) = *zeros
124700040413     c                             or $BauIMx($LastBau(xy)) = *blanks
124800040413     c                   eval      $LastBau(xy)    = 1
124900040414e   2c                   endif
125000040414e   1c                   endif
125100040414      *
125200040413     c                   eval      TR6bau = $BauIMx($LastBau(xy))
125300040413      *
125400040413     c                   EndSr
125500030729
125600030729      *---------------------------------------------------------------*
125700030729      * Registrazione elenco segnacolli sequenziali                   *
125800030729      *---------------------------------------------------------------*
125900030729     c     sr_SCS        BegSr
126000030729      *
126100030729     c                   clear                   TR6nc1
126200030729     c                   eval      TR6ncd    =  ARBncd - 1
126300030729if  1c                   if        ARBnca    =  *zeros
126400030729     c                   eval      ARBnca    =  ARBncd
126500030729e   1c                   endif
126600030729      *
126700030729      * Ciclo di calcolo segnacolli sequenziali
126800030729do  1c                   DOW       TR6ncd    <  ARBnca
126900030729      *
127000030729      * - numero segnacollo
127100030729     c                   eval      TR6ncd    =  TR6ncd + 1
127200030930      *
127300030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
127400030930     c                   exsr      WrtTR6rec
127500030729      *
127600030729e   1c                   ENDDO
127700030729      *
127800030729     c                   EndSr
127900030729
128000030729      *---------------------------------------------------------------*
128100030729      * Registrazione elenco segnacolli NON sequenziali               *
128200030729      *---------------------------------------------------------------*
128300030729     c     sr_SCNS       BegSr
128400030729      *
128500030729     c                   clear                   TR6nc1
128600030729      *
128700030729      * Ciclo di repermento segnacolli NON sequenziali
128800030729if  1c                   if        not %open(FNART01L)
128900030729     c                   open      FNART01L
129000030729e   1c                   endif
129100040318     c     K04sped       setll     FNART000
129200040318     c     K04sped       reade     FNART000
129300030729      *
129400030729do  1c                   DOW       NOT %eof(FNART01L)
129500030729      *
129600030729      * - numero segnacollo
129700030729     c                   eval      TR6ncd    =  ARTnsc
129800030930      *
129900030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
130000030930     c                   exsr      WrtTR6rec
130100030729      *
130200040318     c     K04sped       reade     FNART000
130300030729      *
130400030729e   1c                   ENDDO
130500030729      *
130600030729     c                   EndSr
130700030930
130800030930      *---------------------------------------------------------------*
130900080703      * Scrittura (WRITE) del record (1 o 2) nel file FITR6           *
131000030930      *---------------------------------------------------------------*
131100030930     c     WrtTR6rec     BegSr
131200030930      *
131300030930      * - progressivo del segnacollo in esame
131400050621     c* Se oltre 999 --> lascio 999
131500050621     c                   if        tr6nc1<999
131600030930     c                   eval      TR6nc1    =  TR6nc1 + 1
131700050621     c                   endif
131800030930      * - calcolo il "chi sono" (comprensivo di check-digit)
131900030930     c                   exsr      sr_CHISONO
132000030930      *
132100080703     c                   WRITE(E)  FITR6
132200030930      *
132300030930     c                   EndSr
132400030729      *---------------------------------------------------------------*
132500030729      * Impostazione del segnacollo "CHI SONO" (con check digit)      *
132600030729      *---------------------------------------------------------------*
132700030729     c     sr_CHISONO    BegSr
132800030729      *
132900030729      * Costruisco il codice a barre: FLS+LNA+NRS+NSC+ZNC+CKD
133000030729      * Mancano solo NSC e CKD:
133100030729      * - NSC l'ho appena calcolato in TR6ncd
133200030729      * - chiamo TRUL28R per ricavarne il check-digit
133300030729     c                   eval      BARcdeNSC =  TR6ncd
133400030729     c                   reset                   TRUL28ds
133500030930     c                   movel     BarCodeDS1    I28cod
133600030729     c                   call      'TRUL28R1'
133700030729     c                   parm                    TRUL28DS
133800030729if  1c                   if        O28err    =  *blanks
133900030729     c                   movel     O28cod        TR6who
134000030729e   1c                   endif
134100030729      *
134200030729     c                   EndSr
134300030930
134400030930      *---------------------------------------------------------------*
134500030930      * Impostazione del barcode spedizione per DDT SÌ                *
134600030930      *---------------------------------------------------------------*
134700030930     c     sr_DDTSI      BegSr
134800030930      *
134900030930      * Costruisco il codice a barre: AAS+LNP+NRS+NSP+CKD
135000030930      * (chiamo il pgm TRUL28R per ricavarne il check digit)
135100030930     c                   clear                   BarCodeDS2
135200030930     c                   z-add     ARBaas        BARcdeAAS
135300030930     c                   eval      BARcdeLNP  = ARBlnp
135400030930     c                   eval      BARcdeNRS2 = ARBnrs
135500030930     c                   eval      BARcdeNSP  = ARBnsp
135600030930      *
135700030930     c                   reset                   TRUL28ds
135800030930     c                   movel     BarCodeDS2    I28cod
135900030930     c                   call      'TRUL28R1'
136000030930     c                   parm                    TRUL28DS
136100030930      *
136200030930if  1c                   if        O28err     = *blanks
136300030930     c                   movel     O28cod        TR6who
136400030930x   1c                   else
136500030930     c                   clear                   TR6who
136600030930e   1c                   endif
136700030930      *
136800030930     c                   EndSr
