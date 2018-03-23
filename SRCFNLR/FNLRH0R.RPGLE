000100070724      * FNLRH0R *-----------------------------------------------------*
000200070724      *?Elenco spedizioni EXPORT in arrivo                   -FILTRO-?*
000300070720      *---------------------------------------------------------------*
000400070720
000500070720     h decedit('0,') datedit(*dmy/)
000600070720
000700070720      *---------------------------------------------------------------*
000800070720
000900070720     fAZORG01L  if   e           k disk
001000070723     fAZCAE05L  if   e           k disk
001100070723     fTABEL00F  if   e           k disk
001200070720     fFNFVV01L  if   e           k disk
001300070720      *
001400070724     fFNLRH0D   cf   e             workstn
001500070720
001600070720      *---------------------------------------------------------------*
001700070720
001800070720      *
001900070720      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002000070720      *
002100070720     d DigitN          c                   const('0123456789')
002200070720      *
002300070720      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002400070720      *
002500070720      * - Messaggi di errore
002600070724     d $Msg            s             78    dim( 5) ctdata  perrcd(1)
002700070720      * - £1
002800070723     d $£1             s              3  0 dim(30) inz                          FIL.GESTITE
002900070720      * - £6
003000070724     d $£6             s              3  0 dim(30) inz                          fil. gest. da fgs
003100070720      * - Filiali NON gestite
003200070720     d $NoG            s              3  0 dim(30) inz                          FIL.GESTITE
003300070720      *
003400070720      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003500070720      *
003600070720      * - Parametri
003700070720     d KPJBA         e ds
003800070720      *
003900070724      * - Parametri per FNLRH1R (batch)
004000070724     d FNLRH1ds      e ds                  inz
004100070720      *
004200070720      * - Parametri x Controllo profilo utenti
004300070720     d TIBS34ds      e ds                  inz
004400070720      * - Ds di riferimento al file esterno AZUTE00F
004500070720     d AZUTEds       e ds                  inz  extname(AZUTE00F)
004600070720      * - Ds per dati organigramma
004700070720     d dDatiUte      e ds                  inz
004800070720      *
004900070720      * - Ricerca fogli arrivo
005000070720     d dsLR01        e ds                  inz
005100070720      *
005200070720      * - Caricamento schiere filiali
005300070720     d TRUL06ds      e ds                  inz
005400070720     d  Lin                    1     90  0 inz  dim(30)                         FIL. COMODO
005500070720      *
005600070723      * - Reperimento terminal arrivo/partenza/£6
005700070720     d FNLV54ds      e ds                  inz
005800070724      *
005900070724      * - Reperimento terminal di arrivo/partenza
006000070724     d FNLV55ds      e ds                  inz
006100070723      *
006200070723      * - Tab. "TV" = Tipi Traino
006300070723     d dsTV          e ds                  inz
006400070720      *
006500070720      * Dati da AZORG
006600070720     d Og143         e ds                  inz
006700070724     d Og146         e ds                  inz
006800070724     d Og148         e ds                  inz
006900070720      *
007000070720     d Status         sds
007100070720     d  SDSpgm           *proc
007200070720      *
007300070720      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007400070720      *
007500070720      * - Flags
007600070720     d $Fine           s              1    inz(*off)
007700070720     d $InzD01         s              1    inz(*on)
007800070723     d $TestFGS        s              1    inz(*off)
007900070720     d $TerArr         s              1    inz(*off)
008000070723      *
008100070723      * - Indici di schiera
008200070723     d xx              s              3  0 inz
008300070720      *
008400070720      * - Variabili di comodo
008500070720     d SAVnpg          s                   inz  like(V1Cnpg)
008600070723     d SAVffa          s                   inz  like(V1Cffa)
008700070720     d w0050           s              5  0 inz
008800070720     d wDate           s              8  0 inz
008900070720     d wDate_Iso       s               d   inz  datfmt(*iso)
009000070720      *
009100070720      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
009200070720      *
009300070720      * - AZCAE05L
009400070720     c     k02cae05      klist
009500070720     c                   kfld                    CAEepa
009600070723     c                   kfld                    CAEtfe
009700070720     c     k03cae05      klist
009800070720     c                   kfld                    CAEepa
009900070723     c                   kfld                    CAEtfe
010000070723     c                   kfld                    V1Cfgs
010100070720      * - FNFVV01L
010200070720     c     k03fvv01      klist
010300070720     c                   kfld                    V1Cnpg
010400070720     c                   kfld                    w0050
010500070724     c                   kfld                    V1Cffa
010600070723      * - TABEL00F
010700070723     c     k02tabel      klist
010800070723     c                   kfld                    TBLkut
010900070723     c                   kfld                    TBLcod
011000070723     c     k03tabel      klist
011100070723     c                   kfld                    TBLkut
011200070723     c                   kfld                    TBLcod
011300070723     c                   kfld                    TBLkey
011400070720
011500070720      *---------------------------------------------------------------*
011600070720      *? RIEPILOGO INDICATORI                                        ?*
011700070720      *---------------------------------------------------------------*
011800070720      * 28   - Emissione messaggio di errore                          *
011900070724      * 30   - Comodo                                                 *
012000070720      * 40   - Utente di filiale di 1° livello                        *
012100070720      * 51   - Categoria foglio errata                                *
012200070720      * 52   - Linea di arrivo errata                                 *
012300070720      * 53   - Numero foglio arrivi errato                            *
012400070720      * 54   - Zona consegna DA errata                                *
012500070720      * 55   - Zona consegna A  errata                                *
012600070720      * 90   - Riemissione videata                                    *
012700070720      *---------------------------------------------------------------*
012800070720
012900070720     c     *Entry        plist
013000070720     c                   parm                    KPJBA
013100070720      *
013200070720      * Operazioni iniziali
013300070720     c                   exsr      sr_Start
013400070720      *
013500070720      * Ciclo di gestione della videata di lancio
013600070720do  1c                   dow       $Fine    = *off
013700070720     c                   exsr      GesD01
013800070720e   1c                   enddo
013900070720      *
014000070720      * Fine
014100070720     c                   eval      *inLR    = *on
014200070720
014300070720      *---------------------------------------------------------------*
014400070720      *?Operazioni Iniziali                                          ?*
014500070720      *---------------------------------------------------------------*
014600070720     c     sr_Start      BEGSR
014700070720      *
014800070720      * Reperimento dati utente
014900070720     c     *dtaara       define    §azute        AZUTEds
015000070720     c     *dtaara       define    §datiute      dDATIUTE
015100070720     c                   in(E)     *dtaara
015200070720     c                   if        %ERROR or RSUT = *blanks
015300070720     c                   clear                   TIBS34ds
015400070720     c                   call      'TIBS34R'
015500070720     c                   parm                    TIBS34ds
015600070720     c                   in        *dtaara
015700070720     c                   endif
015800070720      *
015900070720      * Impostazione campi fissi
016000070723     c                   eval      TBLkut   = 1
016100070720     c                   eval      CAEepa   = 'A'
016200070720     c                   move      *date         wDate_Iso
016300070720     c                   move      wDate_Iso     wDate
016400070720      *
016500070720      * Impostazione iniziale dei dati fissi in testata
016600070720     c                   eval      V1Cpgm   = SDSpgm
016700070720      *
016800070723      * Caricamento della £1
016900070720     c                   clear                   TRUL06ds
017000070720     c                   eval      D06Cod   = '£1'
017100070720     c                   movel     DUTtfp        D06key
017200070720     c                   movel(p)  TRUL06ds      KPJBU
017300070720     c                   call      'TRUL06R'
017400070720     c                   parm                    KPJBA
017500070720     c                   movel     KPJBU         TRUL06ds
017600070723     c                   movea     Lin           $£1
017700070720      *
017800070723      * Caricamento schiera filiali gestite da altre (su tutto l'AS400)
017900070720     c                   clear                   TRUL06ds
018000070720     c                   eval      D06Cod   = '£6'
018100070720     c                   eval      D06esc   = 'G'
018200070720     c                   movel     DUTtfp        D06key
018300070720     c                   movel(p)  TRUL06ds      KPJBU
018400070720     c                   call      'TRUL06R'
018500070720     c                   parm                    KPJBA
018600070720     c                   movel     KPJBU         TRUL06ds
018700070720     c                   movea     Lin           $NoG
018800070720      *
018900070720     c                   ENDSR
019000070720      *
019100070720      *---------------------------------------------------------------*
019200070720      *?Gestione videata D01                                         ?*
019300070720      *---------------------------------------------------------------*
019400070720     c     GesD01        BEGSR
019500070720      *
019600070720if  1c                   if        $InzD01  = *on
019700070720     c                   exsr      InzD01
019800070720     c                   eval      $InzD01  = *off
019900070720e   1c                   endif
020000070720      *
020100070720      * Emissione testata e piede
020200070724     c                   write     LRH0T01
020300070724     c                   write     LRH0Z01
020400070720      *
020500070720      * Emissione videata
020600070724     c                   exfmt     LRH0D01
020700070720     c                   clear                   V1Dmsg
020800070720     c                   setoff                                       28  90
020900070720      *
021000070720sel 1c                   SELECT
021100070720      * - F3 = Fine
021200070720w   1c                   WHEN      *inKC
021300070720     c                   eval      $Fine    = *on
021400070720      * - F9 = Personalizzazione Batch
021500070720w   1c***                WHEN      *inKI
021600070720     c***                eval      KCOAZ    = 'LRSB'
021700070720     c***                call      'BCH09'
021800070720     c***                parm                    KPJBA
021900070720      * - Enter/F6: Controllo video D01
022000070720x   1c                   OTHER
022100070720     c                   exsr      CtrD01
022200070720      * - F6 = Lancio batch (se abilitato e se NO errori)
022300070720if  2c                   if                 *inKF
022400070720     c                             and  NOT *in90
022500070724     c                   exsr      sr_SbmJob
022600070720     c                   eval      $Fine    = *on
022700070720e   2c                   endif
022800070720e   1c                   ENDSL
022900070720      *
023000070720     c                   ENDSR
023100070720      *
023200070720      *---------------------------------------------------------------*
023300070720      *?Inizializzazione videata D01                                 ?*
023400070720      *---------------------------------------------------------------*
023500070720     c     InzD01        BEGSR
023600070720      *
023700070724     c                   clear                   LRH0D01
023800070720      *
023900070720      * IMPOSTAZIONE FOGLIO ARRIVI
024000070720      *
024100070720      * Categoria (fissa "2")
024200070720     c                   eval      V1Cnpg   = 2
024300070720     c                   eval      SAVnpg   = V1Cnpg
024400070723      *
024500070724      * Linea di arrivo (ex filiale gestione)
024600070723     c                   z-add     DUTpou        V1Cfgs
024700070723     c                   exsr      sr_CtrFGS
024800070720      *
024900070724      * Foglio arrivi
025000070720     c                   exsr      sr_ImpFArr
025100070720      *
025200070720      * Zone consegna
025300070720     c                   clear                   V1Cznd
025400070720     c                   move      *hival        V1Czna
025500070720      *
025600070720     c                   movea     *zeros        *in(50)
025700070720      *
025800070720     c                   ENDSR
025900070720      *
026000070720      *---------------------------------------------------------------*
026100070723      *?Controllo linea di arrivo (ex filiale gestione)              ?*
026200070720      *---------------------------------------------------------------*
026300070723     c     sr_CtrFGS     BEGSR
026400070720      *
026500070724      * La LNA (ex filiale gestione) non può essere una fil. gestita
026600070723     c     V1Cfgs        lookup    $NoG                                   30
026700070723if  1c                   if        *in30
026800070720     c                   seton                                        284290
026900070724     c                   eval      V1Dmsg   = $Msg(1)
027000070720     c                   leavesr
027100070720e   1c                   endif
027200070720      *
027300070724      * La LNA (ex filiale gestione) deve far parte della £1
027400070723     c     V1Cfgs        lookup    $£1                                    30
027500070723if  1c                   if        NOT *in30
027600070720     c                   seton                                        284290
027700070724     c                   eval      V1Dmsg   = $Msg(1)
027800070720     c                   leavesr
027900070720e   1c                   endif
028000070720      *
028100070724      * Carica la £6 della LNA (ex filiale gestione)
028200070720     c                   clear                   TRUL06ds
028300070720     c                   eval      D06Cod   = '£6'
028400070723     c                   movel     V1Cfgs        D06Key
028500070720     c                   movel(p)  TRUL06ds      KPJBU
028600070720     c                   call      'TRUL06R'
028700070720     c                   parm                    KPJBA
028800070720     c                   movel     KPJBU         TRUL06ds
028900070723     c                   movea     Lin           $£6
029000070720      *
029100070720     c                   ENDSR
029200070720      *
029300070720      *---------------------------------------------------------------*
029400070724      *?Impostazione foglio arrivi (categoria/filiale/numero)        ?*
029500070720      *---------------------------------------------------------------*
029600070720     c     Sr_ImpFArr    BEGSR
029700070720      *
029800070720      * Categoria (già fissa "2")
029900070720     c***                eval      V1Cnpg   = 2
030000070720      *
030100070720      * Filiale foglio: propongo la LNA se è anche ter-arr
030200070720      *      altrimenti propongo il suo ter-arr se su stesso §OGas
030300070724      *      altrimenti propongo sè stessa
030400070723     c                   z-add     V1Cfgs        CAEtfe
030500070723     c                   eval      $TestFGS = *off
030600070720     c                   exsr      Sr_TerArr
030700070723if  1c                   IF        $TerArr  = *on
030800070723     c                   z-add     V1Cfgs        V1Cffa
030900070723x   1c                   ELSE
031000070723     c                   clear                   fnlv54ds
031100070723     c                   z-add     v1cfgs        i54lin
031200070723     c                   movel     'A'           i54tpt
031300070724     c                   z-add     wDate         i54drf
031400070723     c                   call      'FNLV54R'
031500070723     c                   parm                    fnlv54ds
031600070723if  2c     o54aslin      ifeq      o54astfa
031700070723     c                   z-add     o54tfa        v1cffa
031800070720x   2c                   else
031900070723     c                   z-add     v1cfgs        v1cffa
032000070720e   2c                   endif
032100070720e   1c                   ENDIF
032200070720      *
032300070720      * Numero
032400070723if  1c     v1cnpg        ifne      savnpg
032500070724     c     v1cffa        orne      savffa
032600070723     c                   move      *blanks       v1cnfa
032700070723e   1c                   endif
032800070723     c                   move      v1cnpg        savnpg
032900070723     c                   z-add     v1cffa        savffa
033000070720      *
033100070720     c                   ENDSR
033200070720      *
033300070720      *---------------------------------------------------------------*
033400070720      *?Verifica se LNA è ter-arr                                    ?*
033500070720      *---------------------------------------------------------------*
033600070720     c     sr_TerArr     BEGSR
033700070720      *
033800070720     c*** già così:      eval      CAEepa   = 'A'
033900070720     c                   eval      $TerArr  = *off
034000070723     c                   if        $TestFGS = *off
034100070720     c     K02cae05      setll     AZCAE000
034200070720     c     k02cae05      reade     AZCAE000
034300070723     c                   else
034400070723     c     K03cae05      setll     AZCAE000
034500070723     c     k03cae05      reade     AZCAE000
034600070723     c                   endif
034700070720     c                   DOW       NOT  %eof(AZCAE05L)
034800070723     c                             and  $TerArr = *off
034900070720      *
035000070720     c                   if             CAEatb  = *blanks
035100070724     c                             and  wDate  >= CAEdde
035200070724     c                             and  wDate  <= CAEdsc
035300070720     c                   eval      $TerArr  = *on
035400070720     c                   leavesr
035500070720     c                   endif
035600070720      *
035700070723     c                   if        $TestFGS = *off
035800070720     c     k02cae05      reade     AZCAE05L
035900070723     c                   else
036000070723     c     k03cae05      reade     AZCAE000
036100070723     c                   endif
036200070720     c                   ENDDO
036300070720      *
036400070720     c                   ENDSR
036500070720      *
036600070720      *---------------------------------------------------------------*
036700070720      *?Controlli videata D01                                        ?*
036800070720      *---------------------------------------------------------------*
036900070720     c     CtrD01        BEGSR
037000070720      *
037100070724     c                   movea     *zeros        *in(50)
037200070720      *
037300070720      * Categoria fogli (campo PROTETTO)
037400070720if  1c***                if        V1Cnpg  <> 2
037500070723     c***                seton                                        502890
037600070724     c***                eval      V1Dmsg   = $Msg(?)
037700070720     c***                leavesr
037800070720e   1c***                endif
037900070720      *
038000070723      * Filiale foglio arrivi
038100070723      * la fil. foglio può essere o il ter-arr di fgs o fgs stesso
038200070723if  1c     v1cffa        ifeq      *zeros
038300070720     c                   seton                                        522890
038400070724     c                   eval      V1Dmsg   = $Msg(3)
038500070720     c                   leavesr
038600070720e   1c                   endif
038700070724if  1c     v1cffa        ifne      v1cfgs
038800070724      * - verifica se al fil. foglio immessa è ter-arr di fgs
038900070724     c                   z-add     v1cffa        CAEtfe
039000070723     c                   eval      $TestFGS = *on
039100070723     c                   exsr      sr_TerArr
039200070723if  2c                   if        $TerArr  = *off
039300070723     c                   seton                                        522890
039400070724     c                   eval      V1Dmsg   = $Msg(3)
039500070723     c                   leavesr
039600070723e   2c                   endif
039700070723e   1c                   endif
039800070724     c                   eval      DH1fgs   = V1Cffa
039900070720      *
040000070720      * Numero foglio arrivi
040100070724     c                   if        V1Cnfa   = *zeros
040200070724     c                   clear                   V1Cnfa
040300070724     c                   endif
040400070724     c                   eval      *in30    = *off
040500070723     c     '?'           scan      V1Cnfa                                 30
040600070720sel 1c                   SELECT
040700070720      * - Ricerca
040800070723w   1c                   WHEN      *in30
040900070720     c                   clear                   dsLR01
041000070723     c                   movel     'S'           dlrric
041100070724     c                   movel     V1Cnpg        dlrnpg
041200070723     c                   movel     '2'           dlrtfv
041300070723     c                   eval      DLRpgm   = SDSpgm
041400070723     c                   z-add     31129999      dlrgal
041500070723     c                   z-add     99991231      dlraal
041600070723     c                   movel     v1cffa        dlrfgs
041700070720     c                   movel(p)  dsLR01        KPJBU
041800070720     c                   call      'FNLR02R'
041900070720     c                   parm                    KPJBA
042000070720     c                   movel     KPJBU         dsLR01
042100070720     c                   move      DLRnfv        V1Cnfa
042200070723     c                   seton                                        53  90
042300070720     c                   leavesr
042400070720      * - Controllo effettiva immissione
042500070720w   1c                   WHEN           V1Cnfa  = *blanks
042600070720     c                             or   V1Cnfa  = *zeros
042700070723     c                   seton                                        532890
042800070724     c                   eval      V1Dmsg   = $Msg(3)
042900070720     c                   leavesr
043000070720      * - Controllo validità
043100070720x   1c                   OTHER
043200070720      * - - Numericità
043300070723     c     DigitN        check     V1Cnfa                                 30
043400070723if  2c                   if        *in30    = *on
043500070723     c                   seton                                        532890
043600070724     c                   eval      V1Dmsg   = $Msg(3)
043700070720     c                   leavesr
043800070720e   2c                   endif
043900070720      * - - Esistenza in FNFVV00F
044000070720     c                   move      V1Cnfa        w0050
044100070720     c     k03fvv01      chain     FNFVV000
044200070720if  2c                   if        NOT  %found(FNFVV01L)
044300070720     c                             or   FVVatb <> *blanks
044400070723     c                   seton                                        532890
044500070724     c                   eval      V1Dmsg   = $Msg(3)
044600070720     c                   leavesr
044700070720e   2c                   endif
044800070724      * - - - Flag chiusura foglio (deve essere aperto)
044900070724if  2c                   if        FVVfcf   = 'S'
045000070724     c                   seton                                        532890
045100070724     c                   eval      V1Dmsg   = $Msg(4)
045200070724     c                   leavesr
045300070724e   2c                   endif
045400070720e   1c                   ENDSL
045500070724      *
045600070724      * Linea di arrivo
045700070724      * (deve essere estera - DPD/FedEx/EuroExpress)
045800070724     c                   clear                   V1Dlna
045900070724     c     V1Cfgs        chain     AZORG
046000070724if  1c                   if        NOT  %found(AZORG01L)
046100070724     c                             or   ORGfva <> *blanks
046200070724     c                   seton                                        512890
046300070724     c                   eval      V1Dmsg   = $Msg(1)
046400070724     c                   leavesr
046500070724e   1c                   endif
046600070724     c                   movel     ORGdes        V1Dlna
046700070724     c                   movel     ORGde3        Og143
046800070724if  1c                   if             §OGntw <> 'DPD'
046900070724     c                             and  §OGntw <> 'EEX'
047000070724     c                             and  §OGntw <> 'FED'
047100070724     c                   seton                                        512890
047200070724     c                   eval      V1Dmsg   = $Msg(2)
047300070724     c                   leavesr
047400070724e   1c                   endif
047500070720      *
047600070720      * Zona consegna dal deve essere inferiore di zona consegna al
047700070720if  1c                   if        V1Cznd   > V1Czna
047800070720     c                   seton                                        542890
047900070724     c                   eval      V1Dmsg   = $Msg(5)
048000070720     c                   leavesr
048100070720e   1c                   endif
048200070720      *
048300070720     c                   ENDSR
048400070723      *
048500070723      *---------------------------------------------------------------*
048600070723      *?Verifica se la filiale utilizza Tellus                       ?*
048700070723      *---------------------------------------------------------------*
048800070723     c     Ctr_Tellus    BEGSR
048900070723      *
049000070723      * Reperisce la capofila della Lna
049100070723     c                   clear                   FNLV55ds
049200070724     c                   eval      D55lin   = V1Cfgs
049300070723     c                   eval      D55tpt   = '6'
049400070723     c                   eval      D55drf   = FVVdfv
049500070723     c                   call      'FNLV55R'
049600070723     c                   parm                    FNLV55ds
049700070723      *
049800070723      * Aggancia AZORG con la capofila di Lna
049900070723     c                   clear                   og146
050000070723     c                   clear                   og148
050100070723     c     D55tfa        chain     AZORG
050200070723     c                   if        %found(AZORG01L)
050300070723     c                   movel     ORGde6        Og146
050400070723     c                   movel     ORGde8        Og148
050500070723     c                   endif
050600070723      *
050700070723     c                   ENDSR
050800070723      *
050900070723      *---------------------------------------------------------------*
051000070723      *?Sottomissione lavoro batch                                   ?*
051100070723      *---------------------------------------------------------------*
051200070723     c     sr_SbmJob     BEGSR
051300070723      *
051400070723      * Sottomissione lavoro
051500070724     c                   eval      DH1lna   = V1Cfgs
051600070724     c*** già impostato: eval      DH1fgs   = V1Cffa
051700070724     c                   eval      DH1npg   = V1Cnpg
051800070724     c                   move      V1Cnfa        DH1nfa
051900070724     c                   eval      DH1dfa   = FVVdfv
052000070724     c                   eval      DH1znd   = V1Cznd
052100070724     c                   eval      DH1zna   = V1Czna
052200070720      *
052300070724     c                   movel(p)  FNLRH1ds      KPJBU
052400070724     c                   movel     'LRH1'        KCOAZ
052500070720      *
052600070720     c                   if        KNMUS    = *all'1'
052700070724     c                   call      'FNLRH1R'
052800070720     c                   parm                    KPJBA
052900070720     c                   else
053000070720     c                   call      'BCH10'
053100070720     c                   parm                    KPJBA
053200070720     c                   endif
053300070720      *
053400070720     c                   ENDSR
053500070720
053600070720** - $Msg -------------------------------------------------------------------*
053700070720Linea di arrivo errata o mancante                                               1
053800070720Consentite SOLO lna "DPD", "FedEx" e "EuroExpress"                              2
053900070720Numero foglio arrivi errato o mancante                                          3
054000070724Foglio arrivi già chiuso                                                        4
054100070724La zona consegna DAL deve essere minore della zona consegna AL                  5
