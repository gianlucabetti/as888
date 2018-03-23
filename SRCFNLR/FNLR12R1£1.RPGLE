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
004300030829      *
004400030829      * DS tab.3A = Codici bolla
004500030829     d ds3A          e ds                  inz
004600030829      *
004700030829      * DS per AR4not/BL4not tipo rec."A"
004800030829     d dsBL4a        e ds                  inz
004900030917      *
005000030917     d WLBDA8          ds
005100030917     d  G02dat                 1      8  0
005200030917     d  G02inv                 9     16  0
005300030917     d  G02err                17     17
005400030917     d  G02tgi                18     22  0
005500030829      *
005600030829     d                sds
005700030829     d  PT1pgm           *proc
005800030829      *
005900030829      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
006000030829      *
006100030829      * - Flags booleani
006200030829     d $SoloInca       s              1    inz(*off)
006300030829     d $Recupero       s              1    inz(*off)
006400030829     d $Stampata       s              1    inz(*off)
006500030924      *
006600030924      * - Indici di schiera
006700030924     d XX              s              3  0 inz(001)
006800030829      *
006900030829      * - Campi di comodo
007000030829     d wTime           s             14  0
007100030924     d SAVerr          s                   like(CBBerr) inz(*hival)
007200030829
007300030829      *===============================================================*
007400030829      *  RIEPILOGO INDICATORI                                         *
007500030829      *---------------------------------------------------------------*
007600030829      * 10    - Comodo                                                *
007700030829      * 20    - OverFlow                                              *
007800030829      *===============================================================*
007900030829      *
008000030829sel 1c                   select
008100030829      *
008200030829w   1c                   when      CBBerr = *blanks
008300030829     c                   exsr      CheckLDV
008400030829     c                   exsr      RiempiD1
008500030829     c                   exsr      StampaD1
008600030916     c                   add       1             Ptsn
008700030916      *
008800030916w   1c                   when      CBBerr = 'E'
008900030916     c                   exsr      RiempiD1
009000030916     c                   exsr      StampaD1
009100030916     c                   add       1             Ptse
009200030916      *
009300030916w   1c                   when      CBBerr = 'T'
009400030916     c                   add       1             Ptss
009500030829      *
009600030829e   1c                   endsl
009700030829      *
009800030829      * Fine
009900030924     cLR 20              write     LR12T1
010000030829     cLR                 write     LR12F1
010100030829      *
010200030829      *---------------------------------------------------------------*
010300030829      * *InzSr     > Operazioni Iniziali                              *
010400030829      *---------------------------------------------------------------*
010500030829     c     *InzSr        BegSr
010600030909      *
010700030909     c     *Entry        plist
010800030909     c                   parm                    KPJBA
010900030909     c                   movel     KPJBU         FNLR12ds
011000030829      *
011100030829     c     *dtaara       define    §azute        azuteds
011200030829     c     *dtaara       define    §datiute      ddatiute
011300030829     c                   in(E)     *dtaara
011400030829if  1c                   IF        %Error  or  RSUT = *blanks
011500031007     c                   clear                   Tibs34Ds
011600030829     c                   call      'TIBS34R'
011700030829     c                   parm                    Tibs34Ds
011800030829     c                   in        *dtaara
011900030829e   1c                   ENDIF
012000030829      *
012100030829      * Imposto valori fissi
012200030829     c                   eval      AR4trc = 'A'
012300030916     c                   eval      TBLkut = 1
012400030829      *
012500030909      * Imposto dati per la stampa della testata iniziale
012600030829     c                   time                    wTime
012700030829     c                   movel     wTime         PT1ora
012800030829     c                   move      wTime         PT1dta
012900030909     c                   z-add     D12pou        PT1poc
013000030909      *
013100030909     c                   exsr      DecodLNA
013200030924     c                   eval      *in20 = *on
013300030916      *
013400030916     c                   clear                   Ptss
013500030916     c                   clear                   Ptsn
013600030916     c                   clear                   Ptse
013700030829      *
013800030829     c                   EndSr
013900030909
014000030909      *---------------------------------------------------------------*
014100030909      * DecodLNA   > Decodifica P.O. di arrivo                        *
014200030909      *---------------------------------------------------------------*
014300030909     c     DecodLNA      BegSr
014400030909      *
014500030909     c     D12pou        chain     AZORG
014600030909if  1c                   if        not %found(AZORG01L)
014700030909     c                             or  ORGfva = '*'
014800030909     c                   movel     *all'? '      ORGdes
014900030909e   1c                   endif
015000030909      *
015100030909     c                   movel     ORGdes        PT1pod
015200030909      *
015300030909     c                   EndSr
015400030829
015500030829      *---------------------------------------------------------------*
015600030829      * CheckLDV    > Verifica se bolla di solo incasso               *
015700030829      *---------------------------------------------------------------*
015800030829     c     CheckLDV      BegSr
015900030829      *
016000030829      *
016100030829      * Reperisco dati da FNARB
016200030829     c     k04cbb01      chain     FNARB000
016300030901      *
016400030901      * Spedizione non trovata in FNARB (impossibile !!!)
016500030901if  1c                   if        NOT %found(FNARB01L)
016600030924     c***                movel     'e'           CBBerr
016700030901     c                   movel     $Msg(5)       CBBmsg
016800030901     c                   goto      EndCheckLDV
016900030901e   1c                   endif
017000030829      *
017100030829      * Verifica se bolla "solo incasso"
017200030829if  1c                   if        ARBdcm >  *zeros
017300030829     c                   exsr      CtrSoloInca
017400030829if  2c                   if        $SoloInca = *on
017500030924     c***                movel     'e'           CBBerr
017600030829     c                   movel     $Msg(1)       CBBmsg
017700030829     c                   goto      EndCheckLDV
017800030829e   2c                   endif
017900030829e   1c                   endif
018000030829      *
018100030829      * Verifica se bolla di recupero
018200030829     c                   exsr      CtrRecupero
018300030829if  1c                   if        $Recupero = *on
018400030924     c***                movel     'e'           CBBerr
018500030829     c                   movel     $Msg(2)       CBBmsg
018600030829     c                   goto      EndCheckLDV
018700030829e   1c                   endif
018800030829      *
018900030829      * Verifica se bolla "non stampata"
019000030829     c                   exsr      CtrStampata
019100030829if  1c                   if        $Stampata = *off
019200030924     c***                movel     'e'           CBBerr
019300030829     c                   movel     $Msg(3)       CBBmsg
019400030829     c                   goto      EndCheckLDV
019500030829e   1c                   endif
019600030829      *
019700030829      * Nessuno dei casi precedenti (ma LDV non spuntata)
019800030924     c***                movel     'e'           CBBerr
019900030829     c                   movel     $Msg(4)       CBBmsg
020000030829      *
020100030829     c     EndCheckLDV   EndSr
020200030829
020300030829      *---------------------------------------------------------------*
020400030829      * CtrSoloInca > Verifica se bolla di solo incasso               *
020500030829      *---------------------------------------------------------------*
020600030829     c     CtrSoloInca   BegSr
020700030829      *
020800030829     c                   reset                   $SoloInca
020900030829     c                   clear                   ds3A
021000030829      *
021100030829      *?SOLO INCASSO - C/ASSEGNO?
021200030829      *
021300030829      * Controllo se la bolla in esame ha legame con seconda bolla
021400030829     c     k04cbb01      chain     FNLBL000
021500030829      *
021600030829if  1c                   if        %found(FNLBL01L)
021700030829     c                             and LBLlan  = LBLlap
021800030829      *
021900030829      * > Se ho una bolla "mamma" ed è locale
022000030829     c     k04arb_1      chain     FNARB000
022100030829if  2c                   if        %found(FNARB01L)
022200030829      *
022300030829     c     K04arb_1      chain     FNAR9000
022400030829if  3c                   if             %found(FNAR901L)
022500030829     c                             and (ARBicc = *blanks
022600030829     c                              or  ARBicc = 'R')
022700030829     c                   eval      $SoloInca   = *on
022800030829e   3c                   endif
022900030829      *
023000030829e   2c                   endif
023100030829      *
023200030829      *   Mi sono "sporcato" FNARB, quindi riaggancio la figlia
023300030829     c     k04cbb01      chain     FNARB000
023400030829      *
023500030829x   1c                   else
023600030829      *
023700030829      * > Se non c'è legame controllo il c/a della bolla che sto
023800030829      *   elaborando
023900030829     c     k04cbb01      chain     FNAR9000
024000030829if  2c                   if             %found(FNAR901L)
024100030829     c                             and (ARBicc = *blanks
024200030829     c                              or  ARBicc = 'R')
024300030829     c                   eval      $SoloInca   = *on
024400030829e   2c                   endif
024500030829      *
024600030829e   1c                   endif
024700030829      *
024800030829     c     $SoloInca     cabeq     *on           EndSoloInca
024900030829      *
025000030829      *?SOLO INCASSO - FATTURA PORTO ASSEGNATO (PREPAGATO)?
025100030829      *
025200030829      * Decodifico il tipo bolla
025300030829     c                   movel     '3A'          TBLcod
025400030829     c                   movel(p)  ARBcbo        TBLkey
025500030829     c     kTABEL        chain     TABEL00F
025600030829if  1c                   if            %found(TABEL00F)
025700030829     c                             and TBLflg  = *blanks
025800030829     c                   movel     TBLuni        ds3A
025900030829e   1c                   endif
026000030829      *
026100030829      * Aggancio i dati di tassazione bolla
026200030829     c                   clear                   AR6ift
026300030829     c                   clear                   AR6div
026400030829if  1c                   if            §3Atb2  = *blanks
026500030829     c                             and %subst(§3Atb1:1:1) = 'A'
026600030829     c                   eval      AR6trc = '1'
026700030829     c     k05ar601      chain     FIAR6000
026800030829x   1c                   else
026900030829      * - 2ª bolla
027000030829if  2c                   if            §3Atb2 <> *blanks
027100030829     c                   eval      AR6trc = '2'
027200030829     c     k05ar601      chain     FIAR6000
027300030829e   2c                   endif
027400030829e   1c                   endif
027500030829      *
027600030829if  1c                   if             %found(FIAR601L)
027700030829     c                             and  AR6ift > *zeros
027800030829     c                             and (ARBfip = 'I'
027900030829     c                              or  ARBica = *blanks
028000030829     c                              or  ARBica = 'R')
028100030829     c                   eval      $SoloInca   = *on
028200030829e   1c                   endif
028300030829      *
028400030829     c     $SoloInca     cabeq     *on           EndSoloInca
028500030829      *
028600030829     c     EndSoloInca   EndSr
028700030829
028800030829      *---------------------------------------------------------------*
028900030829      * CtrRecupero > Verifica se bolla di recupero                   *
029000030829      *---------------------------------------------------------------*
029100030829     c     CtrRecupero   BegSr
029200030829      *
029300030829     c                   reset                   $Recupero
029400030829     c                   clear                   ds3A
029500030829      *
029600030829      *?BOLLA DI RECUPERO?
029700030829      *
029800030829if  1c                   IF            ARBdcm = *zeros
029900030829     c                             and ARBndc = *zeros
030000030829      *
030100030829      * Decodifico il tipo bolla
030200030829     c                   movel     '3A'          TBLcod
030300030829     c                   movel(p)  ARBcbo        TBLkey
030400030829     c     kTABEL        chain     TABEL
030500030829if  2c                   if            %found(TABEL00F)
030600030829     c                             and TBLflg = *blanks
030700030829     c                   movel     TBLuni        ds3A
030800030829e   2c                   endif
030900030829      *
031000030829if  2c                   if        §3Arbl  = 'R'
031100030829      *
031200030829     c     K04cbb01      setll     FNLBL000
031300030829      * Se non trovati legami memorizzo che è una bolla di recupero
031400030829if  3c                   if        NOT %equal(FNLBL01L)
031500030829     c                   eval      $Recupero  = *on
031600030829e   3c                   endif
031700030829      *
031800030829e   2c                   endif
031900030829      *
032000030829e   1c                   endif
032100030829      *
032200030829     c                   EndSr
032300030829
032400030829      *---------------------------------------------------------------*
032500030829      * CtrRecupero > Verifica se bolla di recupero                   *
032600030829      *---------------------------------------------------------------*
032700030829     c     CtrStampata   BegSr
032800030829      *
032900030829     c                   reset                   $Stampata
033000030829     c                   clear                   dsBL4a
033100030829      *
033200030829     c     k05ar401      chain     FNAR4000
033300030829if  1c                   if            %found(FNAR401L)
033400030829     c                             and AR4atb = *blanks
033500030829     c                   movel     AR4not        dsBL4a
033600030902if  2c                   if        §B4abm <> 'N'
033700030829     c                   eval      $Stampata  = *on
033800030829e   2c                   endif
033900030829e   1c                   endif
034000030829      *
034100030829     c                   EndSr
034200030829
034300030829      *---------------------------------------------------------------*
034400030829      * RiempiD1   > Impostazione campi di stampa (riga di dettaglio) *
034500030829      *---------------------------------------------------------------*
034600030829     c     RiempiD1      BegSr
034700030829      *
034800030829     c                   clear                   LR12D1
034900030829      *
035000030829      * Imposto i campi di stampa
035100030917     c                   eval      Plnp   =  CBBlnp
035200030917     c                   eval      Pnrs   =  CBBnrs
035300030917     c                   eval      Pnsp   =  CBBnsp
035400030917      *
035500030917     c                   clear                   WLBda8
035600030917     c                   eval      G02inv =  CBBaas * 10000 + CBBmgs
035700030917     c                   eval      G02err =  '3'
035800030917     c                   call      'XSRDA8'
035900030917     c                   parm                    WLBda8
036000030917if  1c                   if        G02err <> *on
036100030917     c                   eval      Pdts   =  G02dat
036200030917x   1c                   else
036300030917     c                   eval      Pdts   =  CBBaas
036400030917e   1c                   endif
036500030917      *
036600030917     c                   eval      Pncl   =  CBBncl
036700030917     c                   eval      Ppkf   =  CBBpkf
036800030917     c                   eval      Pvlf   =  CBBvlf
036900030917     c                   eval      Prsd   =  CBBrsd
037000031001      *
037100031001sel 1c                   select
037200031001w   1c                   when      CBBtcr =  *blanks    and
037300031001     c                             CBBdcr >  *zeros
037400031001     c                   eval      Ptcr   =  'Il        '
037500031001w   1c                   when      CBBtcr =  'P'
037600031001     c                   eval      Ptcr   =  'Prima del '
037700031001w   1c                   when      CBBtcr =  'D'
037800031001     c                   eval      Ptcr   =  'Dopo il   '
037900031001e   1c                   endsl
038000031001     c                   clear                   WLBda8
038100031001     c                   eval      G02inv =  CBBdcr
038200031001     c                   eval      G02err =  '3'
038300031001     c                   call      'XSRDA8'
038400031001     c                   parm                    WLBda8
038500031001if  1c                   if        G02err <> *on
038600031001     c                   eval      Pdcr   =  G02dat
038700031001e   1c                   endif
038800030917      *
038900030917     c                   eval      Pmsg   =  CBBmsg
039000030829      *
039100030829     c                   EndSr
039200030829
039300030829      *---------------------------------------------------------------*
039400030829      * StampaD1   > Stampa singola riga (di dettaglio)               *
039500030829      *---------------------------------------------------------------*
039600030829     c     StampaD1      BegSr
039700030829      *
039800030924if  1c                   if        CBBerr <> SAVerr
039900030924     c                   exsr      Sub_Title
040000030924     c                   eval      SAVerr =  CBBerr
040100030924     c                   eval      *in20  =  *on
040200030924e   1c                   endif
040300030924      *
040400030924if  1c                   if        *in20  =  *on
040500030924if  2***c                   if        Page   >  *zeros
040600030924     ***c                   write     LR12S1
040700030924e   2***c                   endif
040800030829     c                   write     LR12T1
040900030924     c                   eval      *in20  =  *off
041000030829e   1c                   endif
041100030829      *
041200030829     c                   write     LR12D1
041300030829      *
041400030829     c                   EndSr
041500030924
041600030924      *---------------------------------------------------------------*
041700030924      * Sub_Title  > Impostazione sottotitolo                         *
041800030924      *---------------------------------------------------------------*
041900030924     c     Sub_Title     BegSr
042000030924      *
042100030924     c                   eval      *in10  = *off
042200030924     c                   eval      xx     = 1
042300030924     c     CBBerr        lookup    $Flg(xx)                               10
042400030924      *
042500030924if  1c                   if        *in10  = *on
042600030924     c                   eval      PT1st1 = $Txt(xx)
042700030924x   1c                   else
042800030924     c                   eval      PT1st1 = CBBerr + ' - ?????????????'
042900030924e   1c                   endif
043000030924      *
043100030924     c                   EndSr
043200030829
043300030829      *---------------------------------------------------------------*
043400030829      * DefKList   > Definizione KEY-LIST                             *
043500030829      *---------------------------------------------------------------*
043600030829     c     DefKList      Begsr
043700030829      *
043800030829     c     k04cbb01      klist
043900030829     c                   kfld                    CBBaas
044000030829     c                   kfld                    CBBlnp
044100030829     c                   kfld                    CBBnrs
044200030829     c                   kfld                    CBBnsp
044300030829      *
044400030829     c     k04arb_1      klist
044500030829     c                   kfld                    LBLaap
044600030829     c                   kfld                    LBLlpp
044700030829     c                   kfld                    LBLnrp
044800030829     c                   kfld                    LBLnsp
044900030829      *
045000030829     c     k05ar601      klist
045100030829     c                   kfld                    CBBaas
045200030829     c                   kfld                    CBBlnp
045300030829     c                   kfld                    CBBnrs
045400030829     c                   kfld                    CBBnsp
045500030829     c                   kfld                    AR6trc
045600030829      *
045700030829     c     k05ar401      klist
045800030829     c                   kfld                    CBBaas
045900030829     c                   kfld                    CBBlnp
046000030829     c                   kfld                    CBBnrs
046100030829     c                   kfld                    CBBnsp
046200030829     c                   kfld                    AR4trc
046300030829      *
046400030829     c     kTABEL        klist
046500030829     c                   kfld                    TBLkut
046600030829     c                   kfld                    TBLcod
046700030829     c                   kfld                    TBLkey
046800030829      *
046900030829     c                   EndSr
047000030917
047100030924** - $FLG / $TXT
047200030924 SPEDIZIONI NON SPUNTATE
047300030924ESPEDIZIONI ECCEDENTI
047400030924** - $MSG                                                    *
047500030924LDV NON SPUNTATA - SPEDIZIONE SOLO INCASSO                                      1
047600030924LDV NON SPUNTATA - BOLLA DI RECUPERO                                            2
047700030924LDV NON SPUNTATA - NON STAMPATA                                                 3
047800030829LDV NON SPUNTATA                                                                4
047900030924LDV NON SPUNTATA - NON REPERITA TESTATA BOLLA ARRIVO                            5
