000100030829      **-------------------------------------------------------------**
000200030922      ** FNLR12R1 ** Quadratura bolle da consegnare - Stampa wrkf    **
000300030829      **-------------------------------------------------------------**
000400030829
000500030829     h decedit('0,') datedit(*dmy/)
000600030829
000700030916     fWFCBB02L  ip   e           k disk
000800030829      *
000900030829     fFNARB01L  if   e           k disk
001000030829     fFNAR901L  if   e           k disk
001100030829     fFIAR601L  if   e           k disk
001200030829     fFNLBL01L  if   e           k disk
001300030829     fFNAR401L  if   e           k disk
001400030829     fTABEL00F  if   e           k disk
001500030829     fAZORG01L  if   e           k disk
001600030829      *
001700030829     fFNLR12P1  o    e             printer oflind(*in20)
001800030829
001900030829      *---------------------------------------------------------------*
002000030829
002100030829      *
002200030829      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002300030829      *
002400030829      *
002500030829      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002600030829      *
002700030924      * Sottotitoli
002800030924     d $Flg            s              1    dim(5) ctdata perrcd(1)
002900030924     d $Txt            s             99    dim(5) alt($Flg)
003000030829      * Messaggi di errore in stampa
003100030829     d $Msg            s             78    dim(5) ctdata perrcd(1)
003200030829      *
003300030829      *   S T R U T T U R E   D A T I   - - - - - - - - - - - - - - - *
003400030829      *
003500030909      * Parametri ricevuti
003600030829     d KPJBA         e ds
003700030909     d FNLR12ds      e ds                  inz
003800030829      *
003900030829      * DS parametri utente
004000030829     d dDatiUte      e ds
004100030829     d AZUTEds       e ds                  extname(AZUTE00F)
004200030829     d TIBS34ds      e ds
004300031027
004400031027      * DS per ORGDF0
004500031027     d Og150         e ds
004600030829      *
004700030829      * DS tab.3A = Codici bolla
004800030829     d ds3A          e ds                  inz
004900030829      *
005000030829      * DS per AR4not/BL4not tipo rec."A"
005100030829     d dsBL4a        e ds                  inz
005200030917      *
005300030917     d WLBDA8          ds
005400030917     d  G02dat                 1      8  0
005500030917     d  G02inv                 9     16  0
005600030917     d  G02err                17     17
005700030917     d  G02tgi                18     22  0
005800030829      *
005900030829     d                sds
006000030829     d  PT1pgm           *proc
006100030829      *
006200030829      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
006300030829      *
006400030829      * - Flags booleani
006500030829     d $SoloInca       s              1    inz(*off)
006600030829     d $Recupero       s              1    inz(*off)
006700030829     d $Stampata       s              1    inz(*off)
006800030924      *
006900030924      * - Indici di schiera
007000030924     d XX              s              3  0 inz(001)
007100030829      *
007200030829      * - Campi di comodo
007300030829     d wTime           s             14  0
007400030924     d SAVerr          s                   like(CBBerr) inz(*hival)
007500030829
007600030829      *===============================================================*
007700030829      *  RIEPILOGO INDICATORI                                         *
007800030829      *---------------------------------------------------------------*
007900030829      * 10    - Comodo                                                *
008000030829      * 20    - OverFlow                                              *
008100030829      *===============================================================*
008200030829      *
008300030829sel 1c                   select
008400030829      *
008500030829w   1c                   when      CBBerr = *blanks
008600030829     c                   exsr      CheckLDV
008700030829     c                   exsr      RiempiD1
008800030829     c                   exsr      StampaD1
008900030916     c                   add       1             Ptsn
009000030916      *
009100030916w   1c                   when      CBBerr = 'E'
009200030916     c                   exsr      RiempiD1
009300030916     c                   exsr      StampaD1
009400030916     c                   add       1             Ptse
009500030916      *
009600030916w   1c                   when      CBBerr = 'T'
009700030916     c                   add       1             Ptss
009800030829      *
009900030829e   1c                   endsl
010000030829      *
010100030829      * Fine
010200030924     cLR 20              write     LR12T1
010300030829     cLR                 write     LR12F1
010400030829      *
010500030829      *---------------------------------------------------------------*
010600030829      * *InzSr     > Operazioni Iniziali                              *
010700030829      *---------------------------------------------------------------*
010800030829     c     *InzSr        BegSr
010900030909      *
011000030909     c     *Entry        plist
011100030909     c                   parm                    KPJBA
011200030909     c                   movel     KPJBU         FNLR12ds
011300030829      *
011400030829     c     *dtaara       define    §azute        azuteds
011500030829     c     *dtaara       define    §datiute      ddatiute
011600030829     c                   in(E)     *dtaara
011700030829if  1c                   IF        %Error  or  RSUT = *blanks
011800031007     c                   clear                   Tibs34Ds
011900030829     c                   call      'TIBS34R'
012000030829     c                   parm                    Tibs34Ds
012100030829     c                   in        *dtaara
012200030829e   1c                   ENDIF
012300030829      *
012400030829      * Imposto valori fissi
012500030829     c                   eval      AR4trc = 'A'
012600030916     c                   eval      TBLkut = 1
012700030829      *
012800030909      * Imposto dati per la stampa della testata iniziale
012900030829     c                   time                    wTime
013000030829     c                   movel     wTime         PT1ora
013100030829     c                   move      wTime         PT1dta
013200030909     c                   z-add     D12pou        PT1poc
013300030909      *
013400030909     c                   exsr      DecodLNA
013500030924     c                   eval      *in20 = *on
013600030916      *
013700030916     c                   clear                   Ptss
013800030916     c                   clear                   Ptsn
013900030916     c                   clear                   Ptse
014000030829      *
014100030829     c                   EndSr
014200030909
014300030909      *---------------------------------------------------------------*
014400030909      * DecodLNA   > Decodifica P.O. di arrivo                        *
014500030909      *---------------------------------------------------------------*
014600030909     c     DecodLNA      BegSr
014700030909      *
014800030909     c     D12pou        chain     AZORG
014900030909if  1c                   if        not %found(AZORG01L)
015000030909     c                             or  ORGfva = '*'
015100030909     c                   movel     *all'? '      ORGdes
015200031027     c                   Clear                   Og150
015300030909e   1c                   endif
015400030909      *
015500030909     c                   movel     ORGdes        PT1pod
015600031027     c                   Movel     OrgDf0        Og150
015700030909      *
015800030909     c                   EndSr
015900030829
016000030829      *---------------------------------------------------------------*
016100030829      * CheckLDV    > Verifica se bolla di solo incasso               *
016200030829      *---------------------------------------------------------------*
016300030829     c     CheckLDV      BegSr
016400030829      *
016500030829      *
016600030829      * Reperisco dati da FNARB
016700030829     c     k04cbb01      chain     FNARB000
016800030901      *
016900030901      * Spedizione non trovata in FNARB (impossibile !!!)
017000030901if  1c                   if        NOT %found(FNARB01L)
017100030924     c***                movel     'e'           CBBerr
017200030901     c                   movel     $Msg(5)       CBBmsg
017300030901     c                   goto      EndCheckLDV
017400030901e   1c                   endif
017500030829      *
017600030829      * Verifica se bolla "solo incasso"
017700030829if  1c                   if        ARBdcm >  *zeros
017800030829     c                   exsr      CtrSoloInca
017900030829if  2c                   if        $SoloInca = *on
018000030924     c***                movel     'e'           CBBerr
018100030829     c                   movel     $Msg(1)       CBBmsg
018200030829     c                   goto      EndCheckLDV
018300030829e   2c                   endif
018400030829e   1c                   endif
018500030829      *
018600030829      * Verifica se bolla di recupero
018700030829     c                   exsr      CtrRecupero
018800030829if  1c                   if        $Recupero = *on
018900030924     c***                movel     'e'           CBBerr
019000030829     c                   movel     $Msg(2)       CBBmsg
019100030829     c                   goto      EndCheckLDV
019200030829e   1c                   endif
019300030829      *
019400030829      * Verifica se bolla "non stampata"
019500030829     c                   exsr      CtrStampata
019600030829if  1c                   if        $Stampata = *off
019700030924     c***                movel     'e'           CBBerr
019800030829     c                   movel     $Msg(3)       CBBmsg
019900030829     c                   goto      EndCheckLDV
020000030829e   1c                   endif
020100030829      *
020200030829      * Nessuno dei casi precedenti (ma LDV non spuntata)
020300030924     c***                movel     'e'           CBBerr
020400030829     c                   movel     $Msg(4)       CBBmsg
020500030829      *
020600030829     c     EndCheckLDV   EndSr
020700030829
020800030829      *---------------------------------------------------------------*
020900030829      * CtrSoloInca > Verifica se bolla di solo incasso               *
021000030829      *---------------------------------------------------------------*
021100030829     c     CtrSoloInca   BegSr
021200030829      *
021300030829     c                   reset                   $SoloInca
021400030829     c                   clear                   ds3A
021500030829      *
021600030829      *?SOLO INCASSO - C/ASSEGNO?
021700030829      *
021800030829      * Controllo se la bolla in esame ha legame con seconda bolla
021900030829     c     k04cbb01      chain     FNLBL000
022000030829      *
022100030829if  1c                   if        %found(FNLBL01L)
022200030829     c                             and LBLlan  = LBLlap
022300030829      *
022400030829      * > Se ho una bolla "mamma" ed è locale
022500030829     c     k04arb_1      chain     FNARB000
022600030829if  2c                   if        %found(FNARB01L)
022700030829      *
022800030829     c     K04arb_1      chain     FNAR9000
022900030829if  3c                   if             %found(FNAR901L)
023000030829     c                             and (ARBicc = *blanks
023100030829     c                              or  ARBicc = 'R')
023200030829     c                   eval      $SoloInca   = *on
023300030829e   3c                   endif
023400030829      *
023500030829e   2c                   endif
023600030829      *
023700030829      *   Mi sono "sporcato" FNARB, quindi riaggancio la figlia
023800030829     c     k04cbb01      chain     FNARB000
023900030829      *
024000030829x   1c                   else
024100030829      *
024200030829      * > Se non c'è legame controllo il c/a della bolla che sto
024300030829      *   elaborando
024400030829     c     k04cbb01      chain     FNAR9000
024500030829if  2c                   if             %found(FNAR901L)
024600030829     c                             and (ARBicc = *blanks
024700030829     c                              or  ARBicc = 'R')
024800030829     c                   eval      $SoloInca   = *on
024900030829e   2c                   endif
025000030829      *
025100030829e   1c                   endif
025200030829      *
025300030829     c     $SoloInca     cabeq     *on           EndSoloInca
025400030829      *
025500030829      *?SOLO INCASSO - FATTURA PORTO ASSEGNATO (PREPAGATO)?
025600030829      *
025700030829      * Decodifico il tipo bolla
025800030829     c                   movel     '3A'          TBLcod
025900030829     c                   movel(p)  ARBcbo        TBLkey
026000030829     c     kTABEL        chain     TABEL00F
026100030829if  1c                   if            %found(TABEL00F)
026200030829     c                             and TBLflg  = *blanks
026300030829     c                   movel     TBLuni        ds3A
026400030829e   1c                   endif
026500030829      *
026600030829      * Aggancio i dati di tassazione bolla
026700030829     c                   clear                   AR6ift
026800030829     c                   clear                   AR6div
026900030829if  1c                   if            §3Atb2  = *blanks
027000030829     c                             and %subst(§3Atb1:1:1) = 'A'
027100030829     c                   eval      AR6trc = '1'
027200030829     c     k05ar601      chain     FIAR6000
027300030829x   1c                   else
027400030829      * - 2ª bolla
027500030829if  2c                   if            §3Atb2 <> *blanks
027600030829     c                   eval      AR6trc = '2'
027700030829     c     k05ar601      chain     FIAR6000
027800030829e   2c                   endif
027900030829e   1c                   endif
028000030829      *
028100030829if  1c                   if             %found(FIAR601L)
028200030829     c                             and  AR6ift > *zeros
028300030829     c                             and (ARBfip = 'I'
028400030829     c                              or  ARBica = *blanks
028500030829     c                              or  ARBica = 'R')
028600030829     c                   eval      $SoloInca   = *on
028700030829e   1c                   endif
028800030829      *
028900030829     c     $SoloInca     cabeq     *on           EndSoloInca
029000030829      *
029100030829     c     EndSoloInca   EndSr
029200030829
029300030829      *---------------------------------------------------------------*
029400030829      * CtrRecupero > Verifica se bolla di recupero                   *
029500030829      *---------------------------------------------------------------*
029600030829     c     CtrRecupero   BegSr
029700030829      *
029800030829     c                   reset                   $Recupero
029900030829     c                   clear                   ds3A
030000030829      *
030100030829      *?BOLLA DI RECUPERO?
030200030829      *
030300030829if  1c                   IF            ARBdcm = *zeros
030400030829     c                             and ARBndc = *zeros
030500030829      *
030600030829      * Decodifico il tipo bolla
030700030829     c                   movel     '3A'          TBLcod
030800030829     c                   movel(p)  ARBcbo        TBLkey
030900030829     c     kTABEL        chain     TABEL
031000030829if  2c                   if            %found(TABEL00F)
031100030829     c                             and TBLflg = *blanks
031200030829     c                   movel     TBLuni        ds3A
031300030829e   2c                   endif
031400030829      *
031500030829if  2c                   if        §3Arbl  = 'R'
031600030829      *
031700030829     c     K04cbb01      setll     FNLBL000
031800030829      * Se non trovati legami memorizzo che è una bolla di recupero
031900030829if  3c                   if        NOT %equal(FNLBL01L)
032000030829     c                   eval      $Recupero  = *on
032100030829e   3c                   endif
032200030829      *
032300030829e   2c                   endif
032400030829      *
032500030829e   1c                   endif
032600030829      *
032700030829     c                   EndSr
032800030829
032900030829      *---------------------------------------------------------------*
033000030829      * CtrRecupero > Verifica se bolla di recupero                   *
033100030829      *---------------------------------------------------------------*
033200030829     c     CtrStampata   BegSr
033300030829      *
033400030829     c                   reset                   $Stampata
033500030829     c                   clear                   dsBL4a
033600030829      *
033700030829     c     k05ar401      chain     FNAR4000
033800030829if  1c                   if            %found(FNAR401L)
033900030829     c                             and AR4atb = *blanks
034000030829     c                   movel     AR4not        dsBL4a
034100031027     c                   Select
034200031027if  2c**!!!              if        §B4abm <> 'N'
034300031027      * p.o. arrivo senza picking
034400031027if  2c                   When      §OgPck = 'S' and §B4Abm <> 'N'
034500030829     c                   eval      $Stampata  = *on
034600031027      * p.o. arrivo con picking
034700031027if  2c                   When      §OgPck = 'S' and §B4Abm <> 'N' and
034800031027if  2c                             §B4Abm <> 'P' and §B4Abm <> 'Q'
034900031027     c                   eval      $Stampata  = *on
035000031027e   2c**!!!              endif
035100031027     c                   EndSl
035200030829e   1c                   endif
035300030829      *
035400030829     c                   EndSr
035500030829
035600030829      *---------------------------------------------------------------*
035700030829      * RiempiD1   > Impostazione campi di stampa (riga di dettaglio) *
035800030829      *---------------------------------------------------------------*
035900030829     c     RiempiD1      BegSr
036000030829      *
036100030829     c                   clear                   LR12D1
036200030829      *
036300030829      * Imposto i campi di stampa
036400030917     c                   eval      Plnp   =  CBBlnp
036500030917     c                   eval      Pnrs   =  CBBnrs
036600030917     c                   eval      Pnsp   =  CBBnsp
036700030917      *
036800030917     c                   clear                   WLBda8
036900030917     c                   eval      G02inv =  CBBaas * 10000 + CBBmgs
037000030917     c                   eval      G02err =  '3'
037100030917     c                   call      'XSRDA8'
037200030917     c                   parm                    WLBda8
037300030917if  1c                   if        G02err <> *on
037400030917     c                   eval      Pdts   =  G02dat
037500030917x   1c                   else
037600030917     c                   eval      Pdts   =  CBBaas
037700030917e   1c                   endif
037800030917      *
037900030917     c                   eval      Pncl   =  CBBncl
038000030917     c                   eval      Ppkf   =  CBBpkf
038100030917     c                   eval      Pvlf   =  CBBvlf
038200030917     c                   eval      Prsd   =  CBBrsd
038300031001      *
038400031001sel 1c                   select
038500031001w   1c                   when      CBBtcr =  *blanks    and
038600031001     c                             CBBdcr >  *zeros
038700031001     c                   eval      Ptcr   =  'Il        '
038800031001w   1c                   when      CBBtcr =  'P'
038900031001     c                   eval      Ptcr   =  'Prima del '
039000031001w   1c                   when      CBBtcr =  'D'
039100031001     c                   eval      Ptcr   =  'Dopo il   '
039200031001e   1c                   endsl
039300031001     c                   clear                   WLBda8
039400031001     c                   eval      G02inv =  CBBdcr
039500031001     c                   eval      G02err =  '3'
039600031001     c                   call      'XSRDA8'
039700031001     c                   parm                    WLBda8
039800031001if  1c                   if        G02err <> *on
039900031001     c                   eval      Pdcr   =  G02dat
040000031001e   1c                   endif
040100030917      *
040200030917     c                   eval      Pmsg   =  CBBmsg
040300030829      *
040400030829     c                   EndSr
040500030829
040600030829      *---------------------------------------------------------------*
040700030829      * StampaD1   > Stampa singola riga (di dettaglio)               *
040800030829      *---------------------------------------------------------------*
040900030829     c     StampaD1      BegSr
041000030829      *
041100030924if  1c                   if        CBBerr <> SAVerr
041200030924     c                   exsr      Sub_Title
041300030924     c                   eval      SAVerr =  CBBerr
041400030924     c                   eval      *in20  =  *on
041500030924e   1c                   endif
041600030924      *
041700030924if  1c                   if        *in20  =  *on
041800030924if  2***c                   if        Page   >  *zeros
041900030924     ***c                   write     LR12S1
042000030924e   2***c                   endif
042100030829     c                   write     LR12T1
042200030924     c                   eval      *in20  =  *off
042300030829e   1c                   endif
042400030829      *
042500030829     c                   write     LR12D1
042600030829      *
042700030829     c                   EndSr
042800030924
042900030924      *---------------------------------------------------------------*
043000030924      * Sub_Title  > Impostazione sottotitolo                         *
043100030924      *---------------------------------------------------------------*
043200030924     c     Sub_Title     BegSr
043300030924      *
043400030924     c                   eval      *in10  = *off
043500030924     c                   eval      xx     = 1
043600030924     c     CBBerr        lookup    $Flg(xx)                               10
043700030924      *
043800030924if  1c                   if        *in10  = *on
043900030924     c                   eval      PT1st1 = $Txt(xx)
044000030924x   1c                   else
044100030924     c                   eval      PT1st1 = CBBerr + ' - ?????????????'
044200030924e   1c                   endif
044300030924      *
044400030924     c                   EndSr
044500030829
044600030829      *---------------------------------------------------------------*
044700030829      * DefKList   > Definizione KEY-LIST                             *
044800030829      *---------------------------------------------------------------*
044900030829     c     DefKList      Begsr
045000030829      *
045100030829     c     k04cbb01      klist
045200030829     c                   kfld                    CBBaas
045300030829     c                   kfld                    CBBlnp
045400030829     c                   kfld                    CBBnrs
045500030829     c                   kfld                    CBBnsp
045600030829      *
045700030829     c     k04arb_1      klist
045800030829     c                   kfld                    LBLaap
045900030829     c                   kfld                    LBLlpp
046000030829     c                   kfld                    LBLnrp
046100030829     c                   kfld                    LBLnsp
046200030829      *
046300030829     c     k05ar601      klist
046400030829     c                   kfld                    CBBaas
046500030829     c                   kfld                    CBBlnp
046600030829     c                   kfld                    CBBnrs
046700030829     c                   kfld                    CBBnsp
046800030829     c                   kfld                    AR6trc
046900030829      *
047000030829     c     k05ar401      klist
047100030829     c                   kfld                    CBBaas
047200030829     c                   kfld                    CBBlnp
047300030829     c                   kfld                    CBBnrs
047400030829     c                   kfld                    CBBnsp
047500030829     c                   kfld                    AR4trc
047600030829      *
047700030829     c     kTABEL        klist
047800030829     c                   kfld                    TBLkut
047900030829     c                   kfld                    TBLcod
048000030829     c                   kfld                    TBLkey
048100030829      *
048200030829     c                   EndSr
048300030917
048400030924** - $FLG / $TXT
048500030924 SPEDIZIONI NON SPUNTATE
048600030924ESPEDIZIONI ECCEDENTI
048700030924** - $MSG                                                    *
048800030924LDV NON SPUNTATA - SPEDIZIONE SOLO INCASSO                                      1
048900030924LDV NON SPUNTATA - BOLLA DI RECUPERO                                            2
049000030924LDV NON SPUNTATA - NON STAMPATA                                                 3
049100030829LDV NON SPUNTATA                                                                4
049200030924LDV NON SPUNTATA - NON REPERITA TESTATA BOLLA ARRIVO                            5
