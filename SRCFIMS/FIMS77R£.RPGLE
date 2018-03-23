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
002300180115     f*//TNTBE01L  if   e           k disk
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
003500180115     d c_FIL_a         c                   const('006')
003600040310      * Numero di Baie/Postazioni elaborabili da FLTR900F
003700040518     d C_MaxRecSf      c                   const(300)
003800040310      * Numero di Fogli Viaggio   elaborabili da FLTR900F
003900040310      *   (deve essere doppio di C_MaxRecSf)
004000040518     d C_MaxRecPf      c                   const(600)
004100030729      *
004200030729      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
004300030729      *
004400030729      * - P.O. gestiti: elenco codici
004500030730     d L6              s                   like(ARBlna)  dim(30)  inz
004600030729      * - P.O. gestiti: network corrispondente
004700030730     d L6ntw           s                   like(§OGntw)  dim(30)  inz
004800031001      *
004900031013      * - Num. F.V. previsti per il piano di smistamento picking
005000040310     d $TR9nfv         s                   like(TR9nfv)  dim(C_MaxRecPf)
005100031001     d                                     inz(*hival)
005200040310     d $TR9rec         s                   like(FLTR9ds) dim(C_MaxRecPf)
005300040310     d                                     inz
005400030729      *
005500030729      * - P.O. elaborati: elenco codici
005600031013     d $PO             s                   like(ARBlna)  dim(250) inz
005700030729      * - P.O. elaborati: dati corrispondenti
005800031013     d $POd            s                   like(AZORGds) dim(250) inz
005900030729      *
006000030729      * - Autotrasportatori elaborati: elenco codici
006100040310     d $PDC            s                   like(APDpdr)  dim(C_MaxRecSf)
006200030729     d                                     inz(*hival)
006300030729      * - Autotrasportatori elaborati: dati corrispondenti
006400040310     d $PDCd           s                   like(APDrsc)  dim(C_MaxRecSf)
006500040310     d                                     inz
006600040413      *
006700040414      * - Baie x Postazioni "IM*" (IMA/IM0/IM1/...)
006800040413     d $PosIMx         s                   like(TR6bai)  dim(20)  inz
006900040413     d $BauIMx         s                   like(TR6bau)  dim(99)  inz
007000040414     d $PosBau         s            198                  dim(20)  inz           99x20
007100040413     d $LastBau        s              3  0               dim(99)  inz
007200030729      *
007300030729      * - Tab.3A = Codici bolla: elenco codici elaborati
007400030729     d $3A             s                   like(ARBcbo)  dim(50)
007500030729     d                                     inz(*hival)
007600030729      * - Tab.3A = Codici bolla: dati corrispondenti
007700030729     d $3Ads           s                   like(ds3A)    dim(50)  inz
007800040406      *
007900040406      * - Tab.TB = Tipi bolla: elenco tipi con raggruppamento bolle "C"
008000040406     d $TBrC           s                   like(§3Atb2)  dim(50)
008100040406     d                                     inz(*hival)
008200030729      *
008300030729      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
008400030729      *
008500030729     d Status         sds
008600030729     d   SDSpgm          *proc                                                  Procedure name
008700030729     d   SDSpar          *parms                                                 Num passed parms
008800030729     d   SDSjob              244    253                                         Job name
008900030729     d   SDSusr              254    263                                         User name
009000030729     d   SDSjnr              264    269s 0                                      Job number
009100030729      *
009200030729     d KPJBA         e ds                  inz
009300030729      * - Parametri per pgm. di caricamento schiera p.o.
009400030729     d Trul06ds      e ds                  inz
009500030730     d   D06cod      e                     inz('£6')
009600030729     d   D06tla      e                     inz('L')
009700040310     d   lia                   1     90  0 inz      dim(30)
009800030729      * - Parametri per pgm. di calcolo check-digit per bar-code
009900030729     d Trul28ds      e ds                  inz
010000030729     d   I28mod      e                     inz('BAR')
010100040310      * - Parametri x Interrogazione tabelle
010200040310     d TIBS02DS      e ds                  inz
010300040310     d   T02mod      e                     inz('C')
010400040310     d   T02cod      e                     inz('MFP')
010500161109     d
010600161109     d xgiolavds     e ds                  inz
010700040310      * - Tab. "MFP" = Applicazioni installate c/o P.O.
010800040310     d dMFP          e ds                  inz
010900040310     d   §MFPap                1     10    dim(10)
011000180115      * - ?Tab. "VPO" / "DECO"?
011100180115      *   ?(vedi progetto 898 - Doppio picking in contemporanea)?
011200180115     d dVPOdeco      e ds                  qualified    inz
011300180115     d   §VPO2PkPd   e                     inz(*hival)
011400160506      * - Tab. "03" = Zone Consegna
011500160506     d ds03          e ds                  inz
011600040406      * - Tab. "TB" = Tipo bolla
011700040406     d dsTB          e ds                  inz
011800030729      * - Tab. "3I" = Variabili programmi partenze
011900030729     d ds3I          e ds                  inz
012000030729      * - Tab. "3A" = Codici bolla
012100030729     d ds3A          e ds                  inz
012200050517      * - DS campo spuntatore in fnfvv
012300050517     d dFVVSPN       e ds                  inz
012400030729      * - Descrizione 143 dell'organigramma
012500030729     d Og143         e ds                  inz
012600160506      * - Descrizione 150 dell'organigramma
012700160506     d Og150         e ds                  inz qualified
012800030930      * - ds per trk FLTR901L
012900030930     d FLTR9ds       e ds                  inz extname(FLTR900F)
013000031001     d FLTR9ima      e ds                  inz extname(FLTR900F) prefix(IMA)
013100030729      * - ds per trk FNART01L
013200030729     d FNARTds       e ds                  inz extname(FNART00F)
013300060217      * - ds per trk FiAR401L
013400060217     d FiAR4ds       e ds                  inz extname(FiAR400F)
013500030930      * - ds per campo AR4not del file FIAR4 - tipo rec. "A"
013600030930     d dsBL4A        e ds                  inz
013700161007
013800161007     d tigcp00ds     e ds                  inz extname(tigcp00F)
013900161007     d fnlgi1ds      e ds                  inz
014000030729      *
014100030729      * - DS codice a barre per calcolo check-digit
014200030930     d BARcodeDS1      ds            18    inz
014300030729     d   BARcdeFLS                    3s 0 inz
014400030729     d   BARcdeLNA                    3s 0 inz
014500030729     d   BARcdeNRS                    2s 0 inz
014600030729     d   BARcdeNSC                    7s 0 inz
014700030729     d   BARcdeZNC                    2s 0 inz
014800030930      * - DS codice a barre (SPEDIZIONE) per calcolo check-digit
014900030930     d BARcodeDS2      ds            17    inz
015000030930     d   BARcdeAAS                    2s 0 inz
015100030930     d   BARcdeLNP                    3s 0 inz
015200030930     d   BARcdeNRS2                   2s 0 inz
015300030930     d   BARcdeNSP                    7s 0 inz
015400030930      * - DS x pgm per controllo/inversione data
015500030930     d WLBdat          ds                  inz
015600030930     d  G08dat                        8  0 inz
015700030930     d  G08inv                        8  0 inz
015800030930     d  G08err                        1    inz
015900030930     d  G08tgi                        5  0 inz
016000030729      * - DS per dati utilizzati del trk AZORG01L
016100030729     d AZORGds         ds                  inz
016200030729     d   ORGdes                            inz
016300030729     d   ORGde3                            inz
016400030729     d   ORGde5                            inz
016500030729     d AZORGfls        ds                  inz
016600030729     d   flsORGdes                         like(ORGdes) inz
016700030729     d   flsORGde3                         like(ORGde3) inz
016800030729     d   flsORGde5                         like(ORGde5) inz
016900030729     d AZORGlnp        ds                  inz
017000030729     d   lnpORGdes                         like(ORGdes) inz
017100030729     d   lnpORGde3                         like(ORGde3) inz
017200030729     d   lnpORGde5                         like(ORGde5) inz
017300030729     d AZORGlna        ds                  inz
017400030729     d   lnaORGdes                         like(ORGdes) inz
017500030729     d   lnaORGde3                         like(ORGde3) inz
017600030729     d   lnaORGde5                         like(ORGde5) inz
017700030729      *
017800030729      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
017900030729      * - campi di comodo
018000040310     d bnlTR9pep       s                   like(TR6bau) inz
018100040413     d wBAU            s                   like(TR6bau) inz
018200030930     d wDCM            s                   like(ARBdcm) inz
018300161109     d kDCM_c          s                   like(ARBdcm) inz
018400030930     d wDFVgma         s              8  0 inz
018500080702     d £po             s                   like(tr9fgs) inz
018600080703     d £TipFV          s                   like(tr9npga) inz
018700080703     d £NumFV          s                   like(tr9nfva) inz
018800171110     d £TipImp         s              1    inz
018900030729      * - indici di schiera / memo progressivi in ciclo DO
019000030729     d xx              s              3  0 inz
019100030729     d xy              s              3  0 inz(1)
019200030729     d yy              s              3  0 inz(1)
019300031001      * - flags booleani
019400040310     d $VdL            s              1    inz(*off)
019500031001     d $FLTR9          s              1    inz(*off)
019600040310     d $Giacenza       s              1    inz(*off)
019700040406     d $C_Serviz       s              1    inz(*off)
019800030729      * - campi per test "rottura"
019900030729     d wPO             s                   like(ARBlna) inz(*hival)
020000030729     d SAVEpdc         s                   like(ARBpdc) inz(*hival)
020100030729     d SAVEcbo         s                   like(ARBcbo) inz(*hival)
020200161109     d
020300161109     d wanomale        s              1
020400161109     d Dataiso         s               d   datfmt(*iso) inz(*sys)
020500030729
020600030729      *===============================================================*
020700030729      *  RIEPILOGO INDICATORI                                         *
020800030729      *---------------------------------------------------------------*
020900030729      * 10    - Comodo                                                *
021000030729      *===============================================================*
021100030729
021200030729     c     *Entry        plist
021300080702     c                   parm                    codicepc          1
021400080702     c                   parm                    filiale           3
021500080703     c                   parm                    catfoglio         1
021600080703     c                   parm                    numfoglio         7
021700171110     c                   parm                    TipoLavoraz       2
021800030729      *
021900030729      * Key-List
022000031013      * - file FLTR901L
022100171110     c     K04tr902      klist
022200031013     c                   kfld                    £PO
022300031013     c                   kfld                    £TipFV
022400031013     c                   kfld                    £NumFV
022500171110     c                   kfld                    £TipImp
022600030930      * - file FNARB25L
022700030930     c     K02arb25      klist
022800030730     c                   kfld                    L6(xx)
022900030930     c                   kfld                    wDCM
023000161109      * - file FNARB25L  le consegnate degli ultimi 3 giorni
023100161109     c     K02arb_c      klist
023200161109     c                   kfld                    L6(xx)
023300161109     c                   kfld                    kDCM_c
023400030729      * - file FIAPD01L
023500030729     c     K02apd01      klist
023600030729     c                   kfld                    APDtip
023700030729     c                   kfld                    ARBpdc
023800030930      * - file FNFVV03L
023900030930     c     K03fvv03      klist
024000030930     c                   kfld                    FVVfgs
024100030930     c                   kfld                    FVVnpg
024200030930     c                   kfld                    FVVnfv
024300060217      * - file FiAR401L
024400030729     c     K05ar401      klist
024500030729     c                   kfld                    ARBaas
024600030729     c                   kfld                    ARBlnp
024700030729     c                   kfld                    ARBnrs
024800030729     c                   kfld                    ARBnsp
024900030729     c                   kfld                    AR4trc
025000040318      * - file FNART01L - FNLBL01L
025100040318     c     K04sped       klist
025200030729     c                   kfld                    ARBaas
025300030729     c                   kfld                    ARBlnp
025400030729     c                   kfld                    ARBnrs
025500030729     c                   kfld                    ARBnsp
025600040318      * - file FNARB01L
025700040318     c     K04arb01      klist
025800040318     c                   kfld                    LBLaap
025900040318     c                   kfld                    LBLlpp
026000040318     c                   kfld                    LBLnrp
026100040318     c                   kfld                    LBLnsp
026200050331      * - file TIGCP21L
026300040310     c     K05gca01      klist
026400040310     c                   kfld                    ARBaas
026500040310     c                   kfld                    ARBlnp
026600040310     c                   kfld                    ARBnrs
026700040310     c                   kfld                    ARBnsp
026800040310     c                   kfld                    GCPfrg
026900030729      * - file TABEL00F
027000030729     c     K03tab00      klist
027100030729     c                   kfld                    TBLkut
027200030729     c                   kfld                    TBLcod
027300030729     c                   kfld                    TBLkey
027400040406     c     K02tab00      klist
027500040406     c                   kfld                    TBLkut
027600040406     c                   kfld                    TBLcod
027700030729      *
027800030729      * Operazioni Iniziali
027900030729     c                   exsr      OPE_INI
028000031001      * Intabellamento dati del file FLTR9
028100031001     c                   exsr      IntabFLTR9
028200030729      *
028300030729      * Ciclo di gestione per p.o. (£1 del P.O. elaboratore)
028400030729do  1c                   DO        30            xx
028500030729      *
028600030730if  2c                   if        L6(xx)    =  *zeros
028700030729     c                   leave
028800030729e   2c                   endif
028900161109     c
029000161109     c                   eval      wanomale=' '
029100161110     c                   eval      arbcca=' '
029200030729      *
029300031001      * Posizionamento iniziale sul file FNARB
029400030930     c     K02arb25      setll     FNARB000
029500030930     c     K02arb25      reade     FNARB000
029600030729      *
029700031001      * Ciclo di elaborazione del file FNARB
029800031001do  2c                   Dow       NOT %eof(FNARB25L)
029900040318      *
030000030729     c                   exsr      sr_ELAB
030100030930     c     K02arb25      reade     FNARB000
030200040414      *
030300031001e   2c                   EndDo
030400161109
030500161109     c                   eval      wanomale='S'
030600161109      * elaboro anche le spedizioni consegnate degli ultimi 3 giorni
030700161109     c     K02arb_c      setll     FNARB000
030800161109     c     l6(xx)        reade     FNARB000
030900161109      *
031000161109      * Ciclo di elaborazione del file FNARB
031100161109do  2c                   Dow       NOT %eof(FNARB25L)
031200161109      *
031300161109     c                   exsr      sr_ELAB
031400161109     c     l6(xx)        reade     FNARB000
031500161109      *
031600161109e   2c                   EndDo
031700030729      *
031800030729e   1c                   ENDDO
031900030729      *
032000030729      * Fine
032100030729     c                   clear                   TRUL28ds
032200030729     c                   eval      I28tla = 'C'
032300030729     c                   call      'TRUL28R1'
032400030729     c                   parm                    TRUL28DS
032500161007
032600161007     c                   clear                   fnlgi1ds
032700161007     c                   eval      ILGI1TLA= 'C'
032800161007     c                   movel     fnlgi1ds      kpjbu
032900161007     c                   call      'FNLGI1R'
033000161007     c                   parm                    kpjba
033100030729     c                   eval      *inLR  = *on
033200030729
033300030729      *---------------------------------------------------------------*
033400030729      * OPERAZIONI INIZIALI                                           *
033500030729      *---------------------------------------------------------------*
033600030729     c     OPE_INI       BegSr
033700180115
033800180115         // - ?Reperimento data attivazione del "Doppio Picking" per fil. 006-PD?
033900180115         reset  dVPOdeco;
034000180115         If  Filiale = c_FIL_a;
034100180115     c                   clear                   TIBS02ds
034200180115     c                   eval      T02mod = 'C'
034300180115     c                   movel     'VPO'         T02cod
034400180115     c                   movel     'DECO'        T02ke1
034500180115     c                   call      'TIBS02R'
034600180115     c                   parm                    KPJBA
034700180115     c                   parm                    TIBS02ds
034800180115if  1c                   if        T02err = *blanks
034900180115     c                   movel     T02uni        dVPOdeco
035000180115e   1c                   endif
035100180115e   1  EndIf;
035200030729      *
035300080702     c                   move      filiale       £po
035400080703     c                   move      catfoglio     £tipfv
035500080703     c                   move      numfoglio     £numfv
035600180115
035700180115       IF  Filiale         = c_FIL_a  and
035800180115           %dec( %date() ) < dVPOdeco.§VPO2PkPd;
035900180115
036000180115         £TipImp = 'R';
036100180115
036200180115       ELSE;
036300180115
036400171110     c                   if            TipoLavoraz = '20'
036500171110     c                             or  TipoLavoraz = '23'
036600171110     c                             or  TipoLavoraz = '26'
036700171110     c                             or  TipoLavoraz = '27'
036800171110     c                             or  TipoLavoraz = '36'
036900171110     c                   eval      £TipImp = 'V'
037000171110     c                   else
037100171110     c                   eval      £TipImp = 'R'
037200171110     c                   endif
037300180115
037400180115       ENDIF;
037500030729      *
037600040406      * Imposto l'utente fisso in file TABEL00F
037700040406     c                   eval      TBLkut = 1
037800030930      * Imposto la categoria dei fogli da elaborare
037900040310     c                   eval      FVVnpg =  4
038000030729      * Imposto il tipo anagrafica per il file FIAPD01L
038100030729      *   "Anagrafica Autotrasportatori/Coop"
038200030729     c                   eval      APDtip = 'A'
038300040310      * Imposto il n° progressivo di riapertura giacenza
038400040310     c                   clear                   GCPfrg
038500030729      * Imposto la KPJBA
038600030729     c                   reset                   KPJBA
038700030729     c                   eval      KNMUS = SDSusr
038800030729     c                   eval      KNMTD = SDSjob
038900030729     c                   eval      KNRJO = SDSjnr
039000030729     c                   eval      KNMEB = SDSjob
039100040310      *
039200040310     c                   reset                   $VdL
039300040310      *
039400040310      * Controllo se P.O. in gestione con VDL
039500040310     c                   clear                   dMFP
039600040310     c                   reset                   TIBS02ds
039700040310     c                   movel     £po           T02ke1
039800040310     c                   call      'TIBS02R'
039900040310     c                   parm                    KPJBA
040000040310     c                   parm                    TIBS02ds
040100040310if  1c                   if        T02err = *blanks
040200040310     c                   movel     T02uni        dMFP
040300040310e   1c                   endif
040400060214      * - NON per i P.O. "elencati" in C_PO_...
040500171110if  1c*//                if        £po   <> C_PO_a
040600040310     c     'V'           lookup    §MFPap                                 10
040700040310     c                   movel     *in10         $VdL
040800171110e   1c*//                endif
040900160506      *
041000160506     c     £po           chain     AZORG
041100160506if  1c                   if            %found(AZORG01L)
041200160506     c                             and ORGfva = *blanks
041300160506     c                   movel     ORGdf0        Og150
041400160506e   1c                   endif
041500030729      *
041600030729      * Reperisco tab. "3I" = variabili pgm partenze
041700030729     c                   clear                   ds3I
041800030729     c                   eval      TBLcod = '3I'
041900030729     c                   movel(p)  '1'           TBLkey
042000030729     c     K03tab00      chain     TABEL
042100030729if  1c                   if            %found(TABEL00F)
042200030729     c                             and TBLflg = *blanks
042300030729     c                   movel     TBLuni        ds3I
042400030729e   1c                   endif
042500030729      * Carico tabella P.O. gestiti
042600030729     c                   reset                   Trul06ds
042700030729     c                   movel     £po           D06key
042800030729     c                   movel(p)  Trul06ds      KPJBU
042900030729     c                   call      'TRUL06R'
043000030729     c                   parm                    KPJBA
043100030729     c                   movel     KPJBU         Trul06ds
043200030730     c                   movea     lia           L6
043300030729      *
043400030729      * Reperisco i network corrispondenti
043500030730     c                   clear                   L6ntw
043600030729do  1c                   DO        30            xx
043700030730if  2c                   if        L6(xx) = *zeros
043800030729     c                   leave
043900030729e   2c                   endif
044000030730     c     L6(xx)        chain     AZORG
044100030729if  2c                   if            %found(AZORG01L)
044200030729     c                             and ORGfva = *blanks
044300030729     c                   movel     ORGde3        OG143
044400030730     c                   movel     §OGntw        L6ntw(xx)
044500030729e   2c                   endif
044600030729e   1c                   ENDDO
044700040406      *
044800040407      * Intabello i tipi bolla c/servizio
044900040406     c                   reset                   $TBrC
045000040406     c                   clear                   xx
045100040406     c                   clear                   dsTB
045200040406     c                   eval      TBLcod = 'TB'
045300040406     c     K02tab00      setll     TABEL
045400040406     c     K02tab00      reade     TABEL
045500040406do  1c                   dow       NOT %eof(TABEL00F)
045600040406     c                             and xx < %elem($TBrC)
045700040406if  2c                   if        TBLflg = *blanks
045800040406     c                   movel     TBLuni        dsTB
045900040406if  3c                   if        §TBrbl = 'C'
046000040406     c                   add       1             xx
046100040406     c                   movel     TBLkey        $TBrC(xx)
046200040406e   3c                   endif
046300040406e   2c                   endif
046400040406     c     K02tab00      reade     TABEL
046500040406e   1c                   enddo
046600161109      *
046700161109     c* Imposto la data di oggi  - 3 gg lavorativi
046800161109     c                   clear                   xgiolavds
046900161109     c                   movel     dataiso       IXGLDATA
047000161109     c                   movel     £po           IXGLFIL
047100161109     c                   eval      IXGLSUB='S'
047200161109     c                   eval      IXGLGG=3
047300161109     c                   eval      IXGLLAV='S'
047400161109     c                   call      'XGIOLAV'
047500161109     c                   parm                    xgiolavds
047600161109     c
047700161109     c                   eval      kdcm_C=OXGLDATA
047800030729      *
047900030729     c                   EndSr
048000031001
048100031001      *---------------------------------------------------------------*
048200031001      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL PIANO DI SMISTAMENTO  *
048300031001      *---------------------------------------------------------------*
048400031001     c     IntabFLTR9    BegSr
048500031001      *
048600031001     c                   clear                   xy
048700040413     c                   clear                   xx
048800040413     c                   clear                   yy
048900040413     c                   clear                   $PosIMx
049000040413     c                   clear                   $PosBau
049100040413     c                   clear                   $BauIMx
049200031001      *
049300031001      * Ciclo di lettura del file FLTR9 per memorizzare
049400031001      *   tipo e numero foglio viaggio x P.O. gestione
049500171110     c     K04TR902      setll     FLTR9000
049600171110     c     K04TR902      reade     FLTR9000
049700031001      *
049800171110do  1c                   dow           NOT  %eof(FLTR902L)
049900040310     c                             and xy < %elem($TR9nfv)
050000031001      *
050100040310sel 2c                   select
050200040413      * - baia non letti (VdL)
050300040310w   2c                   when      TR9bnl  = 'S'
050400040310     c                   movel     TR9pep        bnlTR9pep
050500040414      * - baia "IM*"
050600040413w   2c                   when      TR9npg  = 3
050700040413if  3c                   if        imaTR9nfv = *zeros
050800031001     c                   movel     FLTR9ds       FLTR9ima
050900040413e   3c                   endif
051000040414      * - - cerco la postazione IM* per memorizzarne le baie
051100040413     c                   eval      xx      = 1
051200040413     c                   eval      *in10   = *off
051300040413     c     TR9bai        lookup    $PosIMx(xx)                            10
051400040413if  3c                   if        NOT *in10
051500040413     c                   eval      xx      = 1
051600040413     c     *blanks       lookup    $PosIMx(xx)                            10
051700040413if  4c                   if        *in10   = *on
051800040413     c                   movel     TR9bai        $PosIMx(xx)
051900040413e   4c                   endif
052000040413e   3c                   endif
052100040414      * - - per VdL memorzzo la baia della postazione IM* in esame
052200040414if  3c                   if            $VdL    = *on
052300171110     c                             and £TipImp = 'V'
052400171110     c                             and %subst( TR9des : 28 : 1 ) = 'V'
052500040414     c                             and *in10   = *on
052600040413     c                   movea     $PosBau(xx)   $BauIMx
052700040413     c                   eval      yy      = 1
052800040413     c                   eval      *in10   = *off
052900040413     c                   movel     TR9pep        wBAU
053000040413     c     wBAU          lookup    $BauIMx(yy)                            10
053100040413if  4c                   if        NOT *in10
053200040413     c                   eval      yy      = 1
053300040413     c     *blanks       lookup    $BauIMx(yy)                            10
053400040413if  5c                   if        *in10   = *on
053500040413     c                   movel     TR9pep        $BauIMx(yy)
053600040414     c                   movea     $BauIMx       $PosBau(xx)
053700040413e   5c                   endif
053800040413e   4c                   endif
053900040413e   3c                   endif
054000040414      * - baia non "IM*"
054100040310w   2c                   when      TR9npg  = 4
054200031001     c                   add       1             xy
054300031013     c                   z-add     TR9nfv        $TR9nfv(xy)
054400031013     c                   movel     FLTR9ds       $TR9rec(xy)
054500040310      *
054600040310e   2c                   endsl
054700031001      *
054800171113     c     K04TR902      reade     FLTR9000
054900031001e   1c                   enddo
055000040414      *
055100040414     c                   reset                   xx
055200040414     c                   reset                   yy
055300031001      *
055400031001     c                   EndSr
055500030729
055600030729      *---------------------------------------------------------------*
055700030729      * ELABORAZIONE SINGOLO RECORD DEL FILE FNARB25L                 *
055800030729      *---------------------------------------------------------------*
055900030729     c     sr_ELAB       BegSr
056000040310      *
056100040310     c                   reset                   $FLTR9
056200040310     c                   reset                   $Giacenza
056300040406     c                   reset                   $C_Serviz
056400040318      *
056500040319      * Scarto le spedizioni mamma dato che è la figlia che viene messa
056600040319      *  in distinta
056700161109     c                   if        wanomale=' '
056800040319     c     K04sped       chain     FNLBL002
056900040319     c                   IF        %FOUND(FNLBL02L)
057000040318     c                   goto      NO_elab
057100040318     c                   ENDIF
057200161109     c
057300161109     c                   else
057400161109     c                   if        arbcca<>'1' and arbcca<>'2' and arbcca<>'6'
057500161109     c                   goto      NO_elab
057600161109     c                   ENDIF
057700161109     c                   endif
057800161109      *
057900030729      *
058000030729      * Pulisco dati reperiti solo in casi specifici
058100030729     c                   clear                   FNARTds
058200060217     c                   clear                   FiAR4ds
058300030930     c                   clear                   dsBL4A
058400030729      *
058500030729      * Reperisco dati del p.o. segnacollo
058600030729     c                   eval      wPO    =  ARBfls
058700030729     c                   exsr      RepMemoPO
058800030729     c                   movel     $POd(yy)      AZORGfls
058900030729      *
059000030729      * Reperisco dati del p.o. di partenza
059100030729     c                   eval      wPO    =  ARBlnp
059200030729     c                   exsr      RepMemoPO
059300030729     c                   movel     $POd(yy)      AZORGlnp
059400030729      *
059500030729      * Reperisco dati del p.o. di arrivo
059600030729     c                   eval      wPO    =  ARBlna
059700030729     c                   exsr      RepMemoPO
059800030729     c                   movel     $POd(yy)      AZORGlna
059900030729      *
060000030729      * Reperisco decodifica Autotrasportatore consegna
060100030729if  1c                   if        ARBpdc    <> SAVEpdc
060200030729     c                   eval      SAVEpdc   =  ARBpdc
060300030729     c                   exsr      RepMemoPDC
060400030729e   1c                   endif
060500030729      *
060600030729      * Reperisco tabella Codici bolla
060700030729if  1c                   if        ARBcbo    <> SAVEcbo
060800030729     c                   eval      SAVEcbo   =  ARBcbo
060900030729     c                   exsr      RepMemoT3A
061000030729e   1c                   endif
061100031001      *
061200031001      * Reperisco dati del piano di smitamento (da file FLTR9)
061300161110if  1c                   if        ARBndc    >  *zeros  and arbdcm=0
061400031001     c                   exsr      FVinFLTR9
061500031013e   1c                   endif
061600040310      *
061700040414      * Se TR6bai = "IM*" (se ARBNDC = *zero            oppure
061800040310      *                    se distinta NON trovata su FLTR900F)
061900040310if  1c                   if           ARBndc = *zeros
062000161110     c                             or ($FLTR9 = *off  and arbdcm=0)
062100040406      *   verifico se esiste giacenza aperta con spedizione
062200040310     c                   exsr      ChkGiacenza
062300040407      *   controllo se c/servizio
062400040406     c                   exsr      ChkC_Serv
062500040310e   1c                   endif
062600030729      *
062700080703      * Preparo e scrivo il record in FITR600F
062800030729     c                   exsr      WriteFLTR6
062900030729      *
063000031028     c     NO_elab       EndSr
063100030729
063200030729      *---------------------------------------------------------------*
063300030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI DEL P.O.                  *
063400030729      *---------------------------------------------------------------*
063500030729     c     RepMemoPO     BegSr
063600030729      *
063700030729      * Controllo se diverso con l'ultimo reperito
063800031001if  1c                   if        wPO    <> $PO(yy)
063900030729      *
064000030729      * Controllo se già elaborato
064100030729     c                   eval      *in10   = *off
064200030729     c                   eval      yy      = 1
064300030729     c     wPO           lookup    $PO(yy)                                10
064400030729      *
064500030729      * Reperimento dati se non ancora elaborato
064600030729if  2c                   if        not *in10
064700030729      *
064800030729     c     wPO           chain     AZORG
064900030729if  3c                   if        not %found(AZORG01L)
065000030729     c                             or  ORGfva <> *blanks
065100030729     c                   clear                   AZORGds
065200030729e   3c                   endif
065300030729      * - Memorizzazione dati:
065400030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
065500030729     c     *zeros        lookup    $PO(yy)                                10
065600030729if  3c                   if        not *in10
065700030729     c                   eval      yy      = %elem($PO)
065800030729e   3c                   endif
065900030729     c                   eval      $PO(yy) = wPO
066000030729     c                   movel     AZORGds       $POd(yy)
066100030729      *
066200030729e   2c                   endif
066300030729      *
066400030729e   1c                   endif
066500030729      *
066600030729     c                   EndSr
066700030729
066800030729      *---------------------------------------------------------------*
066900030729      * REPERIMENTO (E MEMORIZZAZIONE) DATI PADRONCINI                *
067000030729      *---------------------------------------------------------------*
067100030729     c     RepMemoPDC    BegSr
067200030729      *
067300030729      * Controllo se già elaborato
067400030729     c                   eval      *in10   =  *off
067500030729     c                   eval      xy      =  1
067600030729     c     SAVEpdc       lookup    $PDC(xy)                               10
067700030729      *
067800030729      * Reperimento dati se non ancora elaborato
067900030729if  1c                   if        not *in10
068000030729      *
068100030729     c     K02apd01      chain     FIAPD000
068200030729if  2c                   if        not %found(FIAPD01L)
068300030729     c                             or  APDatb <> *blanks
068400030729     c                   clear                   APDrsc
068500030729e   2c                   endif
068600030729      * - Memorizzazione dati:
068700030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
068800030729     c     *hival        lookup    $PDC(xy)                               10
068900030729if  2c                   if        not *in10
069000030729     c                   eval      xy       = %elem($PDC)
069100030729e   2c                   endif
069200030729     c                   eval      $PDC(xy) = SAVEpdc
069300030729     c                   movel     APDrsc        $PDCd(xy)
069400030729      *
069500030729      * RE-impostazione dati se già elaborato
069600030729x   1c                   else
069700030729      *
069800030729     c                   movel     $PDCd(xy)     APDrsc
069900030729      *
070000030729e   1c                   endif
070100030729      *
070200030729     c                   EndSr
070300030729
070400030729      *---------------------------------------------------------------*
070500030729      * REPERIMENTO (E MEMORIZZAZIONE) TAB.3A = CODICI BOLLA          *
070600030729      *---------------------------------------------------------------*
070700030729     c     RepMemoT3A    BegSr
070800030729      *
070900030729      * Controllo se già elaborato
071000030729     c                   eval      *in10   =  *off
071100030729     c                   eval      xy      =  1
071200030729     c     SAVEcbo       lookup    $3A(xy)                                10
071300030729      *
071400030729      * Reperimento dati se non ancora elaborato
071500030729if  1c                   if        not *in10
071600030729      *
071700030729     c                   clear                   ds3A
071800030729     c                   eval      TBLcod = '3A'
071900030729     c                   movel(p)  SAVEcbo       TBLkey
072000030729     c     K03tab00      chain     TABEL
072100030729if  2c                   if        %found(TABEL00F)
072200030729     c                             and TBLflg = *blanks
072300030729     c                   movel     TBLuni        ds3A
072400030729e   2c                   endif
072500030729      * - Memorizzazione dati:
072600030729      *   forzatura in ultimo elemento se schiera senza elementi vuoti
072700030729     c     *hival        lookup    $3A(xy)                                10
072800030729if  2c                   if        not *in10
072900030729     c                   eval      xy      =  %elem($3A)
073000030729e   2c                   endif
073100030729     c                   eval      $3A(xy) =  SAVEcbo
073200030729     c                   movel     ds3A          $3Ads(xy)
073300030729      *
073400030729      * RE-impostazione dati se già elaborato
073500030729x   1c                   else
073600030729      *
073700030729     c                   movel     $3Ads(xy)     ds3A
073800030729      *
073900030729e   1c                   endif
074000030729      *
074100030729     c                   EndSr
074200031001
074300031001      *---------------------------------------------------------------*
074400031013      * RICERCA DEL F.V. IN FLTR9                                     *
074500031001      *---------------------------------------------------------------*
074600031001     c     FVinFLTR9     BegSr
074700050517     c                   clear                   dfvvspn
074800031001      *
074900031001      * Reperisco dati da schiera
075000031001     c                   eval      *in10     =  *off
075100031001     c                   eval      xy        =  1
075200031013     c     ARBndc        lookup    $TR9nfv(xy)                            10
075300031001     c                   eval      $FLTR9    =  *in10
075400031001      *
075500031001if  1c                   if        $FLTR9    =  *off
075600031013      * Impostazione dati per Tipo/Numero F.V. non reperito
075700031001     c                   exsr      RepFNFVV
075800031001x   1c                   else
075900031013      * RE-impostazione dati per Tipo/Numero F.V. reperito
076000031013     c                   movel     $TR9rec(xy)   FLTR9ds
076100050517     c
076200160506     c* chain comunque in fnfvv per avere tipo distinta parcel/messagg
076300050517     c                   z-add     ARBifp        FVVfgs
076400050517     c                   z-add     ARBndc        FVVnfv
076500050517     c     K03fvv03      chain     FNFVV000
076600050517if  1c                   if            %found(FNFVV03L)
076700050517     c                   movel     fvvspn        dfvvspn
076800050517     c                   endif
076900050517     c
077000031001e   1c                   endif
077100031001      *
077200031001     c                   EndSr
077300030930
077400030930      *---------------------------------------------------------------*
077500031001      * REPERIMENTO DATI DAI FOGLI VARI                               *
077600030930      *---------------------------------------------------------------*
077700031001     c     RepFNFVV      BegSr
077800030930      *
077900030930     c                   clear                   wDFVgma
078000030930     c                   clear                   FVVdfv
078100030930      *
078200030930     c                   z-add     ARBifp        FVVfgs
078300030930     c*** (già fatto)    z-add     4             FVVnpg
078400031013     c                   z-add     ARBndc        FVVnfv
078500030930     c     K03fvv03      chain     FNFVV000
078600031013if  1c                   if            %found(FNFVV03L)
078700031013     c                             and FVVatb =  *blanks
078800030930     c                   clear                   WLBdat
078900030930     c                   eval      G08inv    =  FVVdfv
079000030930     c                   eval      G08err    =  '3'
079100030930     c                   call      'XSRDA8'
079200030930     c                   parm                    WLBdat
079300030930     c                   eval      wDFVgma   =  G08dat
079400050517     c                   movel     fvvspn        dfvvspn
079500031001e   1c                   endif
079600030930      *
079700030930     c                   EndSr
079800040310
079900040310      *---------------------------------------------------------------*
080000040310      * VERIFICA SE ESISTE GIACENZA APERTA PER SPEDIZIONE             *
080100040310      *---------------------------------------------------------------*
080200040310     c     ChkGiacenza   BegSr
080300040310      *
080400050331     c     K05gca01      chain     Tigcp21l
080500050331if  1c                   if            %found(Tigcp21l)
080600040310     c                             and GCPatb = *blanks
080700050331     c                             and (GCPfas < 035 or
080800050331     c                                  GCPfas = 036)
080900161007     c* solo se merce da portare a IMG con driver
081000161007     c                   clear                   fnlgi1ds
081100161007     c                   eval      ILGI1AAS=arbaas
081200161007     c                   eval      ILGI1lnp=arblnp
081300161007     c                   eval      ILGI1nrs=arbnrs
081400161007     c                   eval      ILGI1nsp=arbnsp
081500170330     c* Passo la filiale gestione e  flag picking
081600170330     c                   movel     £po           ILGI1FGS
081700170330     c                   eval      ILGI1PICK=Og150.§OGpck
081800170330
081900170330     c                   if        ILGI1PICK=' '
082000170330     c                   eval      ILGI1PICK='N'
082100170330     c                   endif
082200170330
082300161007     c                   movel     fnlgi1ds      kpjbu
082400161007     c                   call      'FNLGI1R'
082500161007     c                   parm                    kpjba
082600161007     c                   parm                    tigcp00ds
082700161007     c                   movel     kpjbu         fnlgi1ds
082800161007
082900161007    2c                   if        olgi1AMG='S' or olgi1AMG='Y'
083000161007     c                             or olgi1inmg='S'
083100040310     c                   eval      $Giacenza  = *on
083200161007e   2c                   endif
083300160823e   1c                   endif
083400040310      *
083500040310     c                   EndSr
083600040406
083700040406      *---------------------------------------------------------------*
083800040407      * VERIFICA SE BOLLA C/SERVIZIO                                  *
083900040406      *---------------------------------------------------------------*
084000040406     c     ChkC_Serv     BegSr
084100040406      *
084200040406     c                   eval      *in10     =  *off
084300040406if  1c                   if        §3Atb2    =  *blanks
084400040406     c     §3Atb1        lookup    $TBrC                                  10
084500040406x   1c                   else
084600040406     c     §3Atb2        lookup    $TBrC                                  10
084700040406e   1c                   endif
084800040406     c                   eval      $C_Serviz =  *in10
084900040406      *
085000040406     c                   EndSr
085100030729
085200030729      *---------------------------------------------------------------*
085300080703      * PREPARAZIONE E SCRITTURA DEL RECORD IN FITR600F               *
085400030729      *---------------------------------------------------------------*
085500030729     c     WriteFLTR6    BegSr
085600030729      *
085700080703     c                   clear                   FITR6
085800160506      *
085900080703     c                   eval      tr6fil=filiale
086000080703     c                   eval      tr6cpc=codicepc
086100031024      *
086200060217      * aggancio FiAR4
086300031024     c                   clear                   dsBL4A
086400060217if  1c                   if        not %open(FiAR401L)
086500060217     c                   open      FiAR401L
086600031024e   1c                   endif
086700031024     c                   eval      AR4trc    =  'A'
086800060217     c     K05ar401      chain     FiAR4000
086900060217if  1c                   if        %found(FiAR401L)
087000031024     c                   movel     AR4not        dsBL4A
087100031024e   1c                   endif
087200030729      *
087300030729      * segnacollo "chi sono"
087400030729      * (imposto i dati validi per tutti i segnacolli della spedizione;
087500030729      *  il pgm. TRUL28R verrà richiamato per ogni collo)
087600030930     c                   clear                   BarCodeDS1
087700030729     c                   eval      BARcdeFLS = ARBfls
087800030729     c                   eval      BARcdeLNA = ARBlna
087900030729     c                   eval      BARcdeNRS = ARBnrs
088000030729     c                   eval      BARcdeZNC = ARBznc
088100030729      * secondi di attesa dopo stampa
088200030729     c                   eval      TR6sea    =  §3Isea
088300030729      * flag segnacollo da stampare ('S'/' ')
088400030729     c                   eval      TR6st1    =  'S'
088500030729      * flags
088600030729     c                   eval      TR6fl1    =  *off
088700030729      * codice cliente
088800030729if  1c                   if        ARBccm    >  *zeros
088900030729     c                   eval      TR6ksc    =  ARBccm
089000030729x   1c                   else
089100030729     c                   eval      TR6ksc    =  ARBksc
089200030729e   1c                   endif
089300030729      * p.o. partenza segnacollo
089400030729     c                   eval      TR6fls    =  ARBfls
089500030729      * descrizione del p.o. partenza segnacollo
089600030729     c                   eval      TR6dlp    =  flsORGdes
089700030729      * codice prodotto - default
089800030729     c                   eval      TR6cdp    =  ARBrma
089900030729      * numero serie segnacollo
090000030729     c                   eval      TR6nrs    =  ARBnrs
090100030729      * p.o. di arrivo
090200030729     c                   eval      TR6lna    =  ARBlna
090300030729      * descrizione p.o. arrivo spedizione
090400030729if  1c                   if        lnaORGde5 <> *blanks
090500030729     c                   eval      TR6dla    =  lnaORGde5
090600030729x   1c                   else
090700030729     c                   eval      TR6dla    =  lnaORGdes
090800030729e   1c                   endif
090900030729      * terminal di arrivo
091000030729     c                   eval      TR6tna    =  ARBtfa
091100030729      * tipo servizio bolle
091200030729     c                   eval      TR6tsp    =  ARBtsp
091300030729      * zona segnacollo
091400030729     c                   eval      TR6zsc    =  ARBznc
091500030729      * totale segnacolli della spedizione
091600040728     C                   IF        ARBNCL < 999
091700030729     c                   eval      TR6nc2    =  ARBncl
091800040728     c                   ELSE
091900040728     c                   eval      TR6nc2    =  999
092000040728     c                   ENDIF
092100030729      * ragione sociale mittente
092200030729     c                   eval      TR6rsm    =  ARBrsm
092300030729      * provincia mittente
092400030729     c                   eval      TR6prm    =  ARBprm
092500030729      * ragione sociale destinatario
092600030729     c                   eval      TR6rsd    =  ARBrsd
092700030729      * c.a.p. destinatario
092800030729     c                   eval      TR6cad    =  ARBcad
092900030729      * indirizzo destinatario
093000030729     c                   eval      TR6ind    =  ARBind
093100030729      * località destinatario
093200030729     c                   eval      TR6lod    =  ARBlod
093300030729      * provincia destinatario
093400030729     c                   eval      TR6prd    =  ARBprd
093500030729      * volume
093600030729     c                   eval      TR6vol    =  ARBvlf
093700030729      * peso in kg
093800030729     c                   eval      TR6pkg    =  ARBpkf
093900030729      * flag volume bollettato
094000030729     c                   eval      TR6fvr    =  ARBfvf
094100030729      * anno spedizione
094200030729     c                   eval      TR6aas    =  ARBaas
094300030729      * meso/giorno spedizione
094400030729     c                   eval      TR6mgs    =  ARBmgs
094500030729      * tipo stampa: 'ITA' / 'EST' / 'ALL' / 'DIS'
094600030729     c                   eval      TR6tst    =  'DIS'
094700030930      *
094800030930      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
094900030930      *
095000030930      * Se ARBNDC = *zero            oppure
095100030930      * Se distinta NON trovata su FLTR900F
095200161110     c* Se bolla sto elaborando bolla consegnata con cons.anomala --> IMD
095300030930if  1c                   if           ARBndc = *zeros
095400031001     c                             or $FLTR9 = *off
095500161110     c                             or (arbcca<>*blanks  and arbdcm>0)
095600030930      *
095700030930      * - baia uscita
095800040414     c*** non qui:       eval      TR6bai    =  'IMA'
095900030930      * - p.o. distinta consegna
096000031001     c                   eval      TR6poc    =  imaTR9fgs
096100030930      * - categoria foglio viaggio
096200030930     c                   eval      TR6npg    =  3
096300031001      * - numero foglio viaggio
096400031001     c                   eval      TR6nfv    =  imaTR9nfv
096500031001      * - data foglio viaggio
096600031001     c                   eval      TR6dfv    =  imaTR9dfv
096700031022      * - flag prestazione padroncino (M-mattino, P-pomeriggio)
096800040310     c***                eval      TR6fpp    =  *blanks
096900030930      * - codice autotrasportatore consegna
097000030930     c***                eval      TR6pdc    =  *zeros
097100030930      * - descrizione autotrasportatore consegna
097200030930     c***                eval      TR6dpd    =  *blanks
097300031013      * - descrizioni per stampa
097400161110if  2c                   IF        ARBndc    =  *zeros  or
097500161110     c                             (arbdcm>0 and arbcca<>' ')
097600040406     c                   exsr      sr_DS1
097700160506      * -?distinta messaggerie  o  eventuale Turno?
097800161110      *  ?(per ARBNDC = *zero) oppure consegnata con cons anomala 1/2/6
097900160506if  3c                   If            Og150.§OGpkm   = 'S'
098000160506     c                             or  Og150.§OGturno = 'S'
098100160506     c                   clear                   ds03
098200160506     c                   eval      TBLcod = '03'
098300160506     c                   eval      TBLkey = %editc( ARBlna : 'X' ) +
098400160506     c                                      %editc( ARBznc : 'X' )
098500160506     c     K03tab00      chain     TABEL
098600160506if  4c                   if        %found(TABEL00F)
098700160506     c                             and TBLflg = *blanks
098800160506     c                   movel     TBLuni        ds03
098900160506e   4c                   endif
099000160506     c                   exsr      sr_MessagTurno
099100160506e   3c                   EndIf
099200160506x   2c                   ELSE
099300040414     c                   eval      TR6bai    =  'IM5'
099400031013     c                   eval      TR6ds1    =  'DISTINTA NON PRESENTE'
099500031013     c                                       +  ' NEL PIANO'
099600040130     c                   eval      TR6ds2    =  'Distinta '
099700031013     c                                       +  %editc(ARBndc  : 'Z')
099800031013     c                                       +  ' del '
099900031013     c                                       +  %editc(wDFVgma : 'Y')
100000160506      * -?distinta messaggerie  o  eventuale Turno?
100100160506      *  ?(per $FLTR9 = *off)?
100200160506     c                   exsr      sr_MessagTurno
100300160506e   2c                   ENDIF
100400040414      * - baia uscita per etichetta (postazione)
100500040414if  2c                   if        TR6bai    =  *blanks
100600040414     c                   eval      TR6bai    =  'IMA'
100700040414e   2c                   endif
100800040414     c                   exsr      sr_PosIMA
100900040310      * - baia uscita per picking VdL
101000171110if  2c                   if            $VdL    =  *on
101100171110     c                             and £TipImp = 'V'
101200040413     c                   exsr      sr_BauIMA
101300040414if  3c                   if        TR6bau    =  *blanks
101400040414     c                   eval      TR6bau    =  imaTR9pep
101500040414e   3c                   endif
101600040310e   2c                   endif
101700030930      *
101800030930      * Se ARBNDC > *zero            e
101900030930      * Se distinta trovata su FLTR900F
102000030930x   1c                   else
102100030930      *
102200030930      * - baia uscita
102300030930     c                   eval      TR6bai    =  TR9bai
102400030930      * - p.o. distinta consegna
102500030930     c                   eval      TR6poc    =  TR9fgs
102600030930      * - categoria foglio viaggio
102700030930     c                   eval      TR6npg    =  TR9npg
102800031001      * - numero foglio viaggio
102900031001     c                   eval      TR6nfv    =  TR9nfv
103000031001      * - data foglio viaggio
103100031001     c                   eval      TR6dfv    =  TR9dfv
103200031022      * - flag prestazione padroncino (M-mattino P-pomeriggio)
103300031022if  2c                   if        TR9fl1    =  'P'
103400031022     c                   eval      TR6fpp    =  TR9fl1
103500031022e   2c                   endif
103600030930      * - codice autotrasportatore consegna
103700030930     c                   eval      TR6pdc    =  TR9pdc
103800031024     c                   eval      TR6dpd    =  %subst(APDrsc:1:19)
103900160506      * -?distinta messaggerie  o  eventuale Turno?
104000160506      *  ?(per ARBNDC > *zero e $FLTR9 = *on)?
104100160506     c                   exsr      sr_MessagTurno
104200030930      * - 1ª descrizione per stampa
104300030930     c***                eval      TR6ds1    =  *blanks
104400030930      * - 2ª descrizione per stampa
104500030930     c***                eval      TR6ds2    =  *blanks
104600040310      * - baia uscita per picking VdL
104700171110if  2c                   if            $VdL    = *on
104800171110     c                             and £TipImp = 'V'
104900171113     c                             and %subst( TR9des : 28 : 1 ) = 'V'
105000040310     c                   eval      TR6bau    =  TR9pep
105100040310e   2c                   endif
105200030930      *
105300030930e   1c                   endif
105400030729      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
105500040318      * Se la spedizione non ha segnacolli ed ha una mamma aggancio la
105600040318      *  mamma per reperire i segnacolli solo se ha stessa LNA
105700040318if  1c                   IF        §3Abcm = 'S'     AND
105800040318     c                             ARBncd = *zeros  AND
105900040318     c                             ARBfns <> 'S'
106000040318     c     K04sped       chain     FNLBL000
106100040318if  2c                   IF        %found(FNLBL01L)  AND
106200040318     c                             LBLlan = LBLlap
106300040318     c     K04ARB01      chain     FNARBLEG
106400040319      * reimposto campi segnacollo e file
106500040319      *   non ricavo la descrizione del p.o. segnacollo per il picking
106600040319      *   non serve
106700040319if  3c                   IF        %found(FNARB01L)
106800040319     c                   clear                   BarCodeDS1
106900040319     c                   eval      TR6FLS    = ARBfls
107000040319     c                   eval      TR6NRS    = ARBnrs
107100040319     c                   eval      BARcdeFLS = ARBfls
107200040319     c                   eval      BARcdeLNA = ARBlna
107300040319     c                   eval      BARcdeNRS = ARBnrs
107400040319     c                   eval      BARcdeZNC = ARBznc
107500040319e   3c                   ENDIF
107600040319e   2c                   ENDIF
107700040318e   1c                   ENDIF
107800030729      *
107900030729      * cicli di WRITE dei record (1 x segnacollo)
108000040318      *  suddivisi x sequenziali e non sequenziali
108100030729sel 1c                   select
108200030729w   1c                   when      ARBncd >  *zeros
108300030729     c                   exsr      sr_SCS
108400030729w   1c                   when      ARBfns =  'S'
108500030729     c                   exsr      sr_SCNS
108600030729e   1c                   endsl
108700031006      *
108800031006      * eventuale WRITE dell'unico record x spedizione "DDT SÌ"
108900040310      *   (ultima, poiché re-imposta alcuni campi)
109000161111      *   non lo faccio per bolle con cons.anomala
109100161111    0c                   if        arbdcm=0
109200031006if  1c                   if           §B4abm =  'C'
109300031006     c                             or §B4abm =  'S'
109400031024     c                             or §B4abm =  'P'
109500031024     c                             or §B4abm =  'Q'
109600031013      * - - ripulisco i campi "inutili"
109700031013     c                   clear                   TR6ncd
109800031013     c                   clear                   TR6nc1
109900031006      * - - calcolo il check-digit per il barcode
110000031006     c                   exsr      sr_DDTSI
110100040310      * - - forzo gli "appositi" campi:
110200040310      * - - - baia uscita per picking VdL
110300171110if  2c                   if            $VdL    =  *on
110400171110     c                             and £TipImp = 'V'
110500040310     c                   eval      TR6bau    =  bnlTR9pep
110600040310e   2c                   endif
110700031006      *
110800080703     c                   WRITE(E)  FITR6
110900031006      *
111000031006e   1c                   endif
111100161111e   0c                   endif
111200030729      *
111300030729     c                   EndSr
111400040406
111500040406      *---------------------------------------------------------------*
111600040406      * Impostazione descrizione per stampa "DS1"                     *
111700040406      *---------------------------------------------------------------*
111800040406     c     sr_DS1        BegSr
111900040413      *
112000040407      * Imposto il campo TR6DS1 in base aiprimi 2?casi individuati
112100040406      *  (nel controllare 'ste 2 segnalazioni emergerebbero eventuali
112200040406      *  altri casi...)
112300040406      *
112400040414      *** - max:         eval      TR6ds1    =  '1234567890123456789'
112500040413      ***                                    +  '1234567890123456'
112600161110     c* Bolla resa o dirottata
112700161110     c                   if        arbcca<>' '  and arbdcm>0
112800161110if  2c                   if        TR6ds1    =  *blanks
112900161110     c                   eval      TR6bai    =  'IMD'
113000161110     c                   eval      TR6ds1    =  'RESO/DIR'
113100161110x   2c                   else
113200161110     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
113300161110     c                                       +  'RESO/DIR'
113400161110     c                   goto      End_DS1
113500161110e   2c                   endif
113600161110e   1c                   endif
113700040406      * Giacenza
113800040406if  1c                   if        $Giacenza =  *on
113900040406if  2c                   if        TR6ds1    =  *blanks
114000040414     c                   eval      TR6bai    =  'IM1'
114100040406     c                   eval      TR6ds1    =  'GIACENZA'
114200040406x   2c                   else
114300040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
114400040406     c                                       +  'GIACENZA'
114500040407     c                   goto      End_DS1
114600040406e   2c                   endif
114700040406e   1c                   endif
114800040406      * Fermo deposito
114900040406if  1c                   if        ARBffd    =  'S'
115000040406if  2c                   if        TR6ds1    =  *blanks
115100040414     c                   eval      TR6bai    =  'IM2'
115200040406     c                   eval      TR6ds1    =  'FERMO DEPOSITO'
115300040406x   2c                   else
115400040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
115500040406     c                                       +  'FERMO DEPOSITO'
115600040407     c                   goto      End_DS1
115700040406e   2c                   endif
115800040406e   1c                   endif
115900040406      * Supermercato
116000040406if  1c                   if        ARBtc1    =  'S'  or
116100040406     c                             ARBtc2    =  'S'
116200040406if  2c                   if        TR6ds1    =  *blanks
116300040414     c                   eval      TR6bai    =  'IM3'
116400040406     c                   eval      TR6ds1    =  'SUPERMERCATO'
116500040406x   2c                   else
116600040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
116700040406     c                                       +  'SUPERMERCATO'
116800040407     c                   goto      End_DS1
116900040406e   2c                   endif
117000040406e   1c                   endif
117100040406      * C/Servizio
117200040406if  1c                   if        $C_Serviz =  *on
117300040406if  2c                   if        TR6ds1    =  *blanks
117400040414     c                   eval      TR6bai    =  'IM4'
117500040413     c                   eval      TR6ds1    =  'C/SERVIZIO'
117600040406x   2c                   else
117700040413     c                   eval      TR6ds1    =  %subst(TR6ds1:1:19)
117800040407     c                                       +  'C/SERVIZIO'
117900040407     c                   goto      End_DS1
118000040406e   2c                   endif
118100040406e   1c                   endif
118200040413      *
118300040413     c     End_DS1       EndSr
118400160506
118500160506      /free
118600160506
118700160506       //--------------------------------------------------------------
118800160510       //?Impostazione campo TR6FL3:                                   ?
118900160510       //?- "M" = Spedizione con zona consegna Messaggerie             ?
119000160510       //?- "1" = Spedizione nel 1° turno di smistamento colli         ?
119100160510       //?- "2" = Spedizione nel 2° turno di smistamento colli         ?
119200160510       //?- " " = Spedizione nel turno unico di smistamento colli      ?
119300160506       //--------------------------------------------------------------
119400160506       BEGSR  sr_MessagTurno;
119500160506
119600160506         // -?Se ARBNDC = *zero?
119700160506         //  ?(spedizioni in IMA - tranne "distinta non nel piano" [IM5])?
119800161110         //  ?                   - anche le cons anomale 1/2/6     [IM7])?
119900161110         If  ARBndc = *zeros or (arbdcm>0 and arbcca<>' ');
120000160506
120100160506           // ·?Distinta Messaggerie?
120200160506           if  Og150.§OGpkm   = 'S'  and  §03tpm = 'M';
120300160506             TR6fl3 = §03tpm;
120400160506           endif;
120500160506
120600160506           // ·?Turno?
120700160506           if  Og150.§OGturno = 'S'  and  TR6fl3 = *blanks;
120800160511             TR6fl3 = §03turno;
120900160506           endif;
121000160506
121100160506         // -?Altrimenti?
121200160506         //  ?(distinta  trovata  o  NON trovata [IM5]  su FLTR900F)?
121300160506         Else;
121400160506
121500160506           // ·?Distinta Messaggerie?
121600160506           if  Og150.§OGpkm   = 'S'  and  §FVVtpm = 'M';
121700160506             TR6fl3 = §FVVtpm;
121800160506           endif;
121900160506
122000160506           // ·?Turno?
122100160506           if  Og150.§OGturno = 'S'  and  TR6fl3  = *blanks;
122200160511             TR6fl3 = §FVVturno;
122300160506           endif;
122400160506
122500160506         EndIf;
122600160506
122700160506       ENDSR;
122800160506
122900160506      /end-free
123000040413
123100040413      *---------------------------------------------------------------*
123200040414      * Assegnazione postazione "IM*"                                 *
123300040413      *---------------------------------------------------------------*
123400040414     c     sr_PosIMA     BegSr
123500040413      *
123600040413     c                   eval      xy     = 1
123700040413     c                   eval      *in10  = *off
123800040414      *
123900040414      * Prima cerco la postazione specifica
124000040414      * SE non la trovo: cerco la postazione "IM0"
124100040414      * SE non trovo neppure quella: cerco la postazione "IMA"
124200040414      *   (che c'è sicuramente!!!)
124300040414do  1c                   dow       NOT *in10
124400040414      *
124500040414     c     TR6bai        lookup    $PosIMx(xy)                            10
124600040414      *
124700040414sel 2c                   select
124800040414w   2c                   when      NOT *in10 and TR6bai <> 'IM0'
124900040414     c                                       and TR6bai <> 'IMA'
125000040414     c                   eval      TR6bai = 'IM0'
125100171124w   2c                   when      NOT *in10 and TR6bai = 'IMA'
125200171124     c                   leave
125300040414w   2c                   when      NOT *in10
125400040414     c                   eval      TR6bai = 'IMA'
125500040414e   2c                   endsl
125600040414      *
125700040414e   1c                   enddo
125800040414      *
125900040414     c                   EndSr
126000040414
126100040414      *---------------------------------------------------------------*
126200040414      * Assegnazione baia di uscita corrispondente alla postaz. "IM*" *
126300040414      *---------------------------------------------------------------*
126400040414     c     sr_BauIMA     BegSr
126500040414      *
126600040414     c                   eval      xy     = 1
126700040414     c                   eval      *in10  = *off
126800040414     c     TR6bai        lookup    $PosIMx(xy)                            10
126900040414      * (lo trova sicuramente)
127000040414      *
127100040414if  1c                   if        *in10  = *on
127200040413     c                   movea     $PosBau(xy)   $BauIMx
127300040413     c                   eval      $LastBau(xy)    = $LastBau(xy) + 1
127400040414      *
127500040414if  2c                   if           $LastBau(xy) > %elem($BauIMx)
127600040413     c                             or $LastBau(xy) = *zeros
127700040413     c                             or $BauIMx($LastBau(xy)) = *blanks
127800040413     c                   eval      $LastBau(xy)    = 1
127900040414e   2c                   endif
128000040414e   1c                   endif
128100040414      *
128200040413     c                   eval      TR6bau = $BauIMx($LastBau(xy))
128300040413      *
128400040413     c                   EndSr
128500030729
128600030729      *---------------------------------------------------------------*
128700030729      * Registrazione elenco segnacolli sequenziali                   *
128800030729      *---------------------------------------------------------------*
128900030729     c     sr_SCS        BegSr
129000030729      *
129100030729     c                   clear                   TR6nc1
129200030729     c                   eval      TR6ncd    =  ARBncd - 1
129300030729if  1c                   if        ARBnca    =  *zeros
129400030729     c                   eval      ARBnca    =  ARBncd
129500030729e   1c                   endif
129600030729      *
129700030729      * Ciclo di calcolo segnacolli sequenziali
129800030729do  1c                   DOW       TR6ncd    <  ARBnca
129900030729      *
130000030729      * - numero segnacollo
130100030729     c                   eval      TR6ncd    =  TR6ncd + 1
130200030930      *
130300030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
130400030930     c                   exsr      WrtTR6rec
130500030729      *
130600030729e   1c                   ENDDO
130700030729      *
130800030729     c                   EndSr
130900030729
131000030729      *---------------------------------------------------------------*
131100030729      * Registrazione elenco segnacolli NON sequenziali               *
131200030729      *---------------------------------------------------------------*
131300030729     c     sr_SCNS       BegSr
131400030729      *
131500030729     c                   clear                   TR6nc1
131600030729      *
131700030729      * Ciclo di repermento segnacolli NON sequenziali
131800030729if  1c                   if        not %open(FNART01L)
131900030729     c                   open      FNART01L
132000030729e   1c                   endif
132100040318     c     K04sped       setll     FNART000
132200040318     c     K04sped       reade     FNART000
132300030729      *
132400030729do  1c                   DOW       NOT %eof(FNART01L)
132500030729      *
132600030729      * - numero segnacollo
132700030729     c                   eval      TR6ncd    =  ARTnsc
132800030930      *
132900030930      * - scrittura del record (1 se DDT no, 2 se DDT sì)
133000030930     c                   exsr      WrtTR6rec
133100030729      *
133200040318     c     K04sped       reade     FNART000
133300030729      *
133400030729e   1c                   ENDDO
133500030729      *
133600030729     c                   EndSr
133700030930
133800030930      *---------------------------------------------------------------*
133900080703      * Scrittura (WRITE) del record (1 o 2) nel file FITR6           *
134000030930      *---------------------------------------------------------------*
134100030930     c     WrtTR6rec     BegSr
134200030930      *
134300030930      * - progressivo del segnacollo in esame
134400050621     c* Se oltre 999 --> lascio 999
134500050621     c                   if        tr6nc1<999
134600030930     c                   eval      TR6nc1    =  TR6nc1 + 1
134700050621     c                   endif
134800030930      * - calcolo il "chi sono" (comprensivo di check-digit)
134900030930     c                   exsr      sr_CHISONO
135000030930      *
135100080703     c                   WRITE(E)  FITR6
135200030930      *
135300030930     c                   EndSr
135400030729      *---------------------------------------------------------------*
135500030729      * Impostazione del segnacollo "CHI SONO" (con check digit)      *
135600030729      *---------------------------------------------------------------*
135700030729     c     sr_CHISONO    BegSr
135800030729      *
135900030729      * Costruisco il codice a barre: FLS+LNA+NRS+NSC+ZNC+CKD
136000030729      * Mancano solo NSC e CKD:
136100030729      * - NSC l'ho appena calcolato in TR6ncd
136200030729      * - chiamo TRUL28R per ricavarne il check-digit
136300030729     c                   eval      BARcdeNSC =  TR6ncd
136400030729     c                   reset                   TRUL28ds
136500030930     c                   movel     BarCodeDS1    I28cod
136600030729     c                   call      'TRUL28R1'
136700030729     c                   parm                    TRUL28DS
136800030729if  1c                   if        O28err    =  *blanks
136900030729     c                   movel     O28cod        TR6who
137000030729e   1c                   endif
137100030729      *
137200030729     c                   EndSr
137300030930
137400030930      *---------------------------------------------------------------*
137500030930      * Impostazione del barcode spedizione per DDT SÌ                *
137600030930      *---------------------------------------------------------------*
137700030930     c     sr_DDTSI      BegSr
137800030930      *
137900030930      * Costruisco il codice a barre: AAS+LNP+NRS+NSP+CKD
138000030930      * (chiamo il pgm TRUL28R per ricavarne il check digit)
138100030930     c                   clear                   BarCodeDS2
138200030930     c                   z-add     ARBaas        BARcdeAAS
138300030930     c                   eval      BARcdeLNP  = ARBlnp
138400030930     c                   eval      BARcdeNRS2 = ARBnrs
138500030930     c                   eval      BARcdeNSP  = ARBnsp
138600030930      *
138700030930     c                   reset                   TRUL28ds
138800030930     c                   movel     BarCodeDS2    I28cod
138900030930     c                   call      'TRUL28R1'
139000030930     c                   parm                    TRUL28DS
139100030930      *
139200030930if  1c                   if        O28err     = *blanks
139300030930     c                   movel     O28cod        TR6who
139400030930x   1c                   else
139500030930     c                   clear                   TR6who
139600030930e   1c                   endif
139700030930      *
139800030930     c                   EndSr
