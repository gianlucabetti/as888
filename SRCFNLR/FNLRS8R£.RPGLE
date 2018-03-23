000100070312      * FNLRS8R *-----------------------------------------------------*
000200070312      *?STAMPA LDV DA DISTINTE AUTOTRASPORTATORI - FILTRO DI LANCIO  ?*
000300070312      *---------------------------------------------------------------*
000400070312
000500101008     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070312
000700070312      *---------------------------------------------------------------*
000800070312
000900070315     fAZORG01L  if   e           k disk
001000070315      *
001100071119     fFIDST01L  if   e           k disk
001200071119     fFIDST02L  if   e           k disk    rename(FIDST000:FIDST002)
001300071119     fFIAPD01L  if   e           k disk
001400070312      *
001500070312     fFNLRS8D   cf   e             workstn
001600070312
001700070312      *---------------------------------------------------------------*
001800070312
001900070312      *
002000070312      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002100070312      *
002200070312      * - Costante per controllo "caratteri solo numerici"
002300070312     d DigitN          c                   const('1234567890')
002400070312      *
002500070312      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002600070312      *
002700070312      * - Messaggi di errore
002800160503     d $Msg            s             78    dim(19) ctdata perrcd(1)
002900070312      *
003000070312      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003100070312      *
003200070312      * - Parametri
003300070312     d KPJBA         e ds
003400070312      *
003500070312      * - Parametri per FNLRS9R: Stampa LdV da distinte AUT (batch)
003600070312     d FNLRS9ds      e ds                  inz
003700070312      *
003800070312      * - Parametri x Controllo profilo utenti
003900070312     d TIBS34ds      e ds
004000070312      * - Ds di riferimento al file esterno AZUTE00F
004100070312     d AZUTEds       e ds                       extname(AZUTE00F)
004200070312      * - Ds per dati organigramma
004300070312     d dDatiUte      e ds
004400070315      *
004500071119     d Og146         e ds                  inz
004600101008     d Og148         e ds                  inz
004700071119     d Og150         e ds                  inz
004800070312      *
004900070312      * - Parametri per richiesta stampante per BOLLE ed ETICHETTE
005000070312     d TRUL90ds      e ds                  inz
005100070312     d***D90rse      e                     inz('S')
005200070312     d   D90rsb      e                     inz('L')
005300070312      *
005400070321      * - Parametri per FNLV50R: Controllo Filiale in Gestione
005500070312     d FNLV50ds      e ds                  inz
005600070312      *
005700070312      * - Parametri per FNLR02R: Interrogazione Fogli Vari
005800070312     d dsLR01        e ds                  inz
005900070312     d   DLRnpg      e                     inz(4)
006000070312     d***DLRpgm      e                     inz('FNLRS8R   ')
006100070312     d   DLRric      e                     inz('S')
006200070312     d   DLRtfv      e                     inz('2')
006300070312      *
006400070312     d Status         sds
006500070312     d  SDSpgm           *proc
006600070312      *
006700070312      * - Parametri per pgm. controllo data
006800070312     d WLBdat          ds                  inz
006900070312     d  G08dat                        8  0 inz
007000070312     d  G08inv                        8  0 inz
007100070312     d  G08err                        1    inz('3')
007200070312     d  G08tgi                        5  0 inz
007300070312      *
007400070312      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007500070312      *
007600070312      * - Flags
007700070312     d $Fine           s              1    inz(*off)
007800070322     d $InzD01         s              1    inz(*on)
007900070312      *
008000070312      * - Variabili di comodo
008100070315     d wDate           s              8  0 inz
008200070315     d wDateA          s              8    inz
008300071119     d W1Cndd          s                   inz  like(DSTnfv)
008400070312      *
008500070312      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008600070312      *
008700071119     c     K03DST01      klist
008800071119     c                   kfld                    DSTnpg
008900071119     c                   kfld                    DSTnfv
009000071119     c                   kfld                    DSTfgs
009100070312      *
009200071119     c     K03DST02      klist
009300071119     c                   kfld                    DSTnpg
009400071119     c                   kfld                    DSTdfv
009500071119     c                   kfld                    DSTfgs
009600071119      *
009700071119     c     K02APD01      klist
009800071119     c                   kfld                    DSTtipa
009900071119     c                   kfld                    DSTpdr
010000070312
010100070312      *---------------------------------------------------------------*
010200070312      *? RIEPILOGO INDICATORI                                        ?*
010300070312      *---------------------------------------------------------------*
010400070319      * 01   - Disabilitazione tasto funzionale F1   e                *
010500070319      *        Sprotezione campo "Data Distinta"                      *
010600070319      * 06   - Disabilitazione tasto funzionale F6                    *
010700070319      *        (Filiale non abilitata)                                *
010800070319      * 18   - Disabilitazione tasto funzionale F18  e                *
010900070319      *        Sprotezione campo "Filiale Gestione"                   *
011000070312      * 28   - Emissione messaggio di errore                          *
011100070625      * 40   - Visualizzazione richiesta flag "MESSAGGERIE"           *
011200070319      * 50   - Filiale Gestione errata                                *
011300070319      * 51   - Data Distinta errata                                   *
011400070319      * 52   - Numero Distinta iniziale errato                        *
011500070319      * 53   - Numero Distinta finale   errato                        *
011600070312      * 90   - Riemissione videata                                    *
011700070312      *---------------------------------------------------------------*
011800070312
011900070312     c     *Entry        plist
012000070312     c                   parm                    KPJBA
012100070312      *
012200070312      * Operazioni iniziali
012300070312     c                   exsr      sr_Start
012400070312      *
012500070312      * Ciclo di gestione della videata di lancio
012600070322do  1c                   dow       $Fine   =  *off
012700070322     c                   exsr      GesD01
012800070322e   1c                   enddo
012900070312      *
013000070312      * Fine
013100070312     c                   eval      *inLR   =  *on
013200070312
013300070312      *---------------------------------------------------------------*
013400070312      *?Operazioni Iniziali                                          ?*
013500070312      *---------------------------------------------------------------*
013600070312     c     sr_Start      BEGSR
013700070312      *
013800070312      * Reperimento dati utente
013900070312     c     *dtaara       define    §azute        AZUTEds
014000070312     c     *dtaara       define    §datiute      dDATIUTE
014100070312     c                   in(E)     *dtaara
014200070312     c                   if        %ERROR or RSUT = *blanks
014300070312     c                   clear                   TIBS34ds
014400070312     c                   call      'TIBS34R'
014500070312     c                   parm                    TIBS34ds
014600070312     c                   in        *dtaara
014700070312     c                   endif
014800070312      *
014900070312      * Richiesta stampanti
015000070312     c                   reset                   TRUL90ds
015100070312     c                   call      'TRUL90R'
015200070312     c                   parm                    KPJBA
015300070312     c                   parm                    TRUL90ds
015400070312      *
015500070312     c                   if        D90F3   =  *on
015600070312     c                   eval      $Fine   =  *on
015700070312     c                   leavesr
015800070312     c                   endif
015900070312      *
016000070322      * Impostazione iniziale dei dati fissi in testata
016100070312     c                   eval      V1Cpgm  =  SDSpgm
016200070322      *
016300070322      * Impostazione della *DATE in formato alfanumerico
016400070322     c                   clear                   WLBdat
016500070322     c                   eval      G08dat  =  *date
016600070322     c                   call      'XSRDA8'
016700070322     c                   parm                    WLBdat
016800070322if  1c                   eval      wDate   =  G08inv
016900070322if  1c                   move      wDate         wDateA
017000070312      *
017100070312     c                   ENDSR
017200070322      *
017300070322      *---------------------------------------------------------------*
017400070322      *?Gestione videata D01                                         ?*
017500070322      *---------------------------------------------------------------*
017600070322     c     GesD01        BEGSR
017700070322      *
017800070322if  1c                   if        $InzD01 =  *on
017900070322     c                   exsr      InzD01
018000070322     c                   eval      $InzD01 =  *off
018100070322e   1c                   endif
018200070322      *
018300070322      * Emissione testata e piede
018400070322     c                   write     LRS8T01
018500070322     c                   write     LRS8Z01
018600070322      *
018700070322      * Emissione videata di dettaglio
018800070322     c                   exfmt     LRS8D01
018900070322     c                   clear                   V1Dmsg
019000070322     c                   setoff                                       28  90
019100070322      *
019200070322sel 1c                   SELECT
019300070322      * - F1 = Modifica Data Distinta
019400070322w   2c                   WHEN      *inKA
019500070322     c                   eval      *in01   =  *on
019600070322     c                   eval      *in51   =  *on
019700070322      * - F3 = Fine
019800070322w   1c                   WHEN      *inKC
019900070322     c                   eval      $Fine   =  *on
020000070322      * - F18 = Modifica Filiale in Gestione
020100070322w   1c                   WHEN      *inKS
020200070322     c                   eval      *in18   =  *on
020300070322     c                   eval      *in50   =  *on
020400070322      * - F9 = Personalizzazione Batch
020500070322w   1c***                WHEN      *inKI
020600070322     c***                eval      KCOAZ   =  'LRS9'
020700070322     c***                call      'BCH09'
020800070322     c***                parm                    KPJBA
020900070322      * - Enter/F6: Controllo video D01
021000070322x   1c                   OTHER
021100070322     c                   exsr      CtrD01
021200070322      * - F6 = Lancio batch (se abilitato e se NO errori)
021300070322if  2c                   if                *inKF
021400070322     c                             and NOT *in06
021500070322     c                             and NOT *in90
021600070322     c                   exsr      sr_SbmJob
021700070322     c                   eval      $Fine   =  *on
021800070322e   2c                   endif
021900070322e   1c                   ENDSL
022000070322      *
022100070322     c                   ENDSR
022200070322      *
022300070322      *---------------------------------------------------------------*
022400070322      *?Inizializzazione videata D01                                 ?*
022500070322      *---------------------------------------------------------------*
022600070322     c     InzD01        BEGSR
022700070322      *
022800070322     c                   clear                   LRS8D01
022900070322      *
023000070322     c                   if        DUTlpo  =  '2'
023100070322     c                   eval      V1Cfgs  =  DUTpou
023200070322     c                   else
023300070322     c                   eval      V1Cfgs  =  DUTtfp
023400070322     c                   endif
023500070322     c                   eval      V1Cddc  =  *date
023600101008     c                   eval      V1Cpda  =  'N'
023700071015     c                   eval      V1Cord  =  '1'
023800071119     c                   eval      V1Csms  =  'S'
023900070322      *
024000070322      * Decodifiche
024100070322     c                   exsr      CtrD01
024200070322if  1c                   if        NOT  *in06
024300070322     c                   movea     *zeros        *in(28)
024400070322e   1c                   endif
024500070625      *
024600070625      * Impostazione indicatori
024700070625     c                   eval      *in40    = (§OGpkm = 'S')
024800101021     c                   eval      *in41    = (§OGpdacon = 'S')                 Procedura PDA avviat
024900160503     c                   eval      *in42    = (§OGturno = 'S')                  Turno Picking Conseg
025000160503      *
025100160503if  1c                   if        *in42
025200160503     c                   eval      V1Cturn1 = '1'
025300160503     c                   eval      V1Cturn2 = '2'
025400160503e   1c                   endif
025500070322      *
025600070322     c                   ENDSR
025700070312      *
025800070312      *---------------------------------------------------------------*
025900070312      *?Controlli videata D01                                        ?*
026000070312      *---------------------------------------------------------------*
026100070312     c     CtrD01        BEGSR
026200070312      *
026300070312     c                   movea     *zeros        *in(50)
026400070312      *
026500070312      *  Controllo filiale gestione
026600070322     c                   clear                   V1Dfgs
026700070312if  1c                   if        V1Cfgs  <> D50fgs
026800070312     c                   clear                   FNLV50ds
026900070312     c                   eval      D50pru  =  KNMUS
027000070312     c                   eval      D50fgs  =  V1Cfgs
027100070312     c                   call      'FNLV50R'
027200070312     c                   parm                    FNLV50ds
027300070312e   1c                   endif
027400070312if  1c                   if        D50err  <> *blanks
027500070312     c                   seton                                        285090
027600070312     c                   movel     D50msg        V1Dmsg
027700070312     c                   leavesr
027800070312e   1c                   endif
027900070322      * Decodifica filiale gestione
028000070322if  1c                   if        V1Cfgs  <> ORGfil
028100070322     c     V1Cfgs        chain     AZORG
028200070322if  2c                   if        NOT  %found(AZORG01L)
028300070322     c                             or   ORGfva <> *blanks
028400070322     c                   clear                   ORGdes
028500070625     c                   clear                   ORGde6
028600101008     c                   clear                   ORGde8
028700070625     c                   clear                   ORGdf0
028800070322e   2c                   endif
028900070322e   1c                   endif
029000070322     c                   movel     ORGdes        V1Dfgs
029100070322     c                   movel     ORGde6        OG146
029200101008     c                   movel     ORGde8        OG148
029300070625     c                   movel     ORGdf0        OG150
029400071005if  2c                   if        §OGdda  <= *zeros
029500071005     c                   eval      §OGdda  =  *hival
029600071005e   2c                   endif
029700070625      *
029800070625      * Impostazione indicatori
029900070625     c                   eval      *in06    = (§OGdda > wDateA)
030000070625     c                   eval      *in40    = (§OGpkm = 'S')
030100160503     c                   eval      *in42    = (§OGturno = 'S')
030200070322      *
030300070322      * Funzione non eseguibile (ad oggi)
030400070625if  1c                   if        *in06
030500070322     c                   seton                                        28  90
030600070322     c                   eval      V1Dmsg  =  $Msg( 1)
030700070322     c                   leavesr
030800070322e   1c                   endif
030900070312      *
031000070312     c                   eval      DS9fgs  =  V1Cfgs
031100070312      *
031200070312      * Data distinta
031300070312if  1c                   if        V1Cddc  =  *zeros
031400070312     c                   seton                                        285190
031500070322     c                   eval      V1Dmsg  =  $Msg( 2)
031600070312     c                   leavesr
031700070312e   1c                   endif
031800070312     c                   clear                   WLBdat
031900070312     c                   eval      G08dat  =  V1Cddc
032000070312     c                   call      'XSRDA8'
032100070312     c                   parm                    WLBdat
032200070312if  1c                   if        G08err  =  *on
032300070312     c                   seton                                        285190
032400070322     c                   eval      V1Dmsg  =  $Msg( 3)
032500070312     c                   leavesr
032600070312e   1c                   endif
032700070312      *
032800070312     c                   eval      V1Cddc  =  G08dat
032900070312     c                   eval      DS9ddc  =  G08inv
033000070312      *
033100071119     c                   eval      DSTnpg  =  4
033200071119     c                   eval      DSTdfv  =  DS9ddc
033300071119     c                   eval      DSTfgs  =  V1Cfgs
033400071119     c     K03DST02      chain     FIDST002
033500071119if  1c                   if        NOT %found(FIDST02L)
033600070312     c                   seton                                        285290
033700070322     c                   eval      V1Dmsg  =  $Msg( 4)
033800070312     c                   leavesr
033900070312e   1c                   endif
034000070312      *
034100070312      * Numero distinta da
034200070312     c                   clear                   W1Cndd
034300070316     c                   clear                   V1Dndd
034400070312if  1c                   if        V1Cndd  =  *zeros
034500070312     c                   clear                   V1Cndd
034600070312e   1c                   endif
034700070312if  1c                   IF             V1Cndd <> *blanks
034800070312     c                             and  V1Cndd <> *zeros
034900070312      * - Ricerca
035000070312if  2c                   if        %scan('?' : V1Cndd) > *zeros
035100070312     c                   reset                   dsLR01
035200070312     c                   eval      DLRpgm  =  SDSpgm
035300070312     c                   eval      DLRfgs  =  V1Cfgs
035400070312     c                   eval      DLRgda  =  V1Cddc
035500070312     c                   eval      DLRgal  =  V1Cddc
035600070312     c                   movel(p)  dsLR01        KPJBU
035700070312     c                   call      'FNLR02R'
035800070312     c                   parm                    KPJBA
035900070312     c                   movel     KPJBU         dsLR01
036000070312     c                   movel     DLRnfv        V1Cndd
036100070312e   2c                   endif
036200070312      * - Controllo validità
036300070312if  2c                   if        %check(DigitN:V1Cndd) = *zeros
036400070312     c                   move      V1Cndd        W1Cndd
036500071119     c                   eval      DSTnpg  =  4
036600071119     c                   eval      DSTnfv  =  W1Cndd
036700071119     c                   eval      DSTfgs  =  V1Cfgs
036800071119     c     K03DST01      chain     FIDST000
036900070312e   2c                   endif
037000070312sel 2c                   Select
037100070312w   2c                   When      %check(DigitN:V1Cndd) > *zeros
037200070312     c                   seton                                        285290
037300070322     c                   eval      V1Dmsg  =  $Msg( 5)
037400070312     c                   leavesr
037500071119w   2c                   When      NOT  %found(FIDST01L)
037600071119     c                             or   DSTatb <> *blanks
037700070312     c                   seton                                        285290
037800070322     c                   eval      V1Dmsg  =  $Msg( 6)
037900070312     c                   leavesr
038000071119w   2c                   When      DSTfcf  =  'S'
038100070312     c                   seton                                        285290
038200070322     c                   eval      V1Dmsg  =  $Msg( 7)
038300070312     c                   leavesr
038400071119w   2c                   When      DSTdfv  <> DS9ddc
038500070312     c                   seton                                        285290
038600070322     c                   eval      V1Dmsg  =  $Msg( 8)
038700070312     c                   leavesr
038800070312x   2c                   Other
038900071119     c     K02APD01      chain     FIAPD000
039000071119     c                   if        %found(FIAPD01L)
039100071119     c                   eval      V1Dndd  =  APDrsc
039200071119     c                   else
039300071119     c                   clear                   V1Dndd
039400071119     c                   endif
039500070312if  3c                   if        V1Cnda  =  *zeros
039600070312     c                   move      V1Cndd        V1Cnda
039700070312e   3c                   endif
039800070312e   2c                   EndSl
039900070312e   1c                   ENDIF
040000070312      *
040100070312     c                   eval      DS9ndi  =  W1Cndd
040200070312      *
040300070312      * Numero distinta a
040400070312if  1c                   if        V1Cnda  <= *zeros
040500070314     c                   seton                                        285390
040600070625     c                   eval      V1Dmsg  =  $Msg( 9)
040700070314     c                   leavesr
040800070314e   1c                   endif
040900070312      *
041000070312     c                   eval      DS9ndf  =  V1Cnda
041100070312      *
041200070322      * Range incompatibile
041300070312if  1c                   if        DS9ndi  >  DS9ndf
041400070312     c                   seton                                        285390
041500070625     c                   eval      V1Dmsg  =  $Msg(10)
041600070625     c                   leavesr
041700070312e   1c                   endif
041800071015      *
041900071015      * Ordinamento
042000071015if  1c                   if             V1Cord  = '2'
042100071015     c                             and  §OGpck <> 'S'
042200071015     c                   seton                                        285490
042300071015     c                   eval      V1Dmsg  =  $Msg(13)
042400071015     c                   leavesr
042500071015e   1c                   endif
042600070625      *
042700070625      * Selezione "Tipo distinta"
042800070625if  1c                   if             V1Ctpm <> *blanks
042900070625     c                             and  DS9ndi  = DS9ndf
043000070625     c                   seton                                        285390
043100070625     c                   eval      V1Dmsg  =  $Msg(11)
043200070625     c                   leavesr
043300070625e   1c                   endif
043400071203if  1c                   if             V1Ctpm  = *blanks
043500071203     c                             and  *in40   = *on
043600071203     c                             and  DS9ndi <> DS9ndf
043700071203     c                   seton                                        285590
043800071203     c                   eval      V1Dmsg  =  $Msg(14)
043900071203     c                   leavesr
044000071203e   1c                   endif
044100160503      *
044200160503      * Selezione "Turno"
044300160503sel 1c                   select
044400160503      * - Filiale gestione NON abilitata
044500160503w   1c                   when      Not *in42
044600160503     c                   clear                   V1Cturn1
044700160503     c                   clear                   V1Cturn2
044800160503      * - Turno impostabile SOLO SE selezionate più di una distinta
044900160503w   1c                   when           DS9ndi   =  DS9ndf
045000160503     c                             and  V1Ctpm   <> 'M'
045100160503     c                             and (V1Cturn1 <> *blanks
045200160503     c                             and  V1Cturn2 =  *blanks
045300160503     c                              or  V1Cturn1 =  *blanks
045400160503     c                             and  V1Cturn2 <> *blanks)
045500160503     c                   if        V1Cturn1 <> *blanks
045600160503     c                   seton                                        285690
045700160503     c                   else
045800160503     c                   seton                                        285790
045900160503     c                   endif
046000160503     c                   eval      V1Dmsg  =  $Msg(18)
046100160503     c                   leavesr
046200160503      * - "Turno 1" o "Turno 2"
046300160503w   1c                   when      DS9ndi   <> DS9ndf  and
046400160503     c                             V1Ctpm   <> 'M'     and
046500160503     c                             V1Cturn1 =  *blank  and
046600160503     c                             V1Cturn2 =  *blank
046700160503     c                   seton                                        285690
046800160503     c                   eval      V1Dmsg   =  $Msg(15)
046900160503     c                   leavesr
047000160503      * - "Turno 1"
047100160503w   1c                   when      V1Ctpm   =  'M'  and
047200160503     c                             V1Cturn1 <> *blank
047300160503     c                   seton                                        285690
047400160503     c                   eval      V1Dmsg   =  $Msg(16)
047500160503     c                   leavesr
047600160503w   1c                   when      V1Ctpm   <> 'M'     and
047700160503     c                             V1Cturn1 <> *blank  and
047800160503     c                             V1Cturn1 <> '1'     and
047900160503     c                             V1Cturn1 <> '2'
048000160503     c                   seton                                        285690
048100160503     c                   eval      V1Dmsg   =  $Msg(17)
048200160503     c                   leavesr
048300160503      * - "Turno 2"
048400160503w   1c                   when      V1Ctpm   =  'M'  and
048500160503     c                             V1Cturn2 <> *blank
048600160503     c                   seton                                        285790
048700160503     c                   eval      V1Dmsg   =  $Msg(16)
048800160503     c                   leavesr
048900160503w   1c                   when      V1Ctpm   <> 'M'     and
049000160503     c                             V1Cturn2 <> *blank  and
049100160503     c                             V1Cturn2 <> '1'     and
049200160503     c                             V1Cturn2 <> '2'
049300160503     c                   seton                                        285790
049400160503     c                   eval      V1Dmsg   =  $Msg(17)
049500160503     c                   leavesr
049600160503      * - Turno già impostato
049700160503w   1c                   when      V1Ctpm   <> 'M'     and
049800160503     c                             V1Cturn1 <> *blank  and
049900160503     c                             V1Cturn1 =  V1Cturn2
050000160503     c                   seton                                        285690
050100160503     c                   eval      V1Dmsg   =  $Msg(19)
050200160503     c                   leavesr
050300160503      *
050400160503e   1c                   endsl
050500070625      *
050600070625      * Selezione "Prestazione padroncino"
050700070625if  1c                   if             V1Cfpp <> *blanks
050800070625     c                             and  DS9ndi  = DS9ndf
050900070625     c                   seton                                        285390
051000070625     c                   eval      V1Dmsg  =  $Msg(12)
051100070625     c                   leavesr
051200070625e   1c                   endif
051300070312      *
051400070312     c                   ENDSR
051500070312      *
051600070312      *---------------------------------------------------------------*
051700070312      *?Sottomissione lavoro batch                                   ?*
051800070312      *---------------------------------------------------------------*
051900070312     c     sr_SbmJob     BEGSR
052000070625      *
052100070625     c*** già così:      clear                   FNLRS9ds
052200070625      *
052300070625      * Parametri già impostati nella subr. "CTRD01":
052400070625     c***                eval      DS9fgs  =  V1Cfgs
052500070625     c***                eval      DS9ddc  =  G08inv
052600070625     c***                eval      DS9ndi  =  W1Cndd
052700070625     c***                eval      DS9ndf  =  V1Cnda
052800070312      *
052900070625      * Parametri NON ancora impostati:
053000101008if  1c                   if        *in41
053100101008     c                   eval      DS9pda  =  V1Cpda
053200101008x   1c                   else
053300101008     c                   clear                   DS9pda
053400101008e   1c                   endif
053500070625if  1c                   if        *in40
053600070621     c                   eval      DS9tpm  =  V1Ctpm
053700070625x   1c                   else
053800070625     c                   clear                   DS9tpm
053900070625e   1c                   endif
054000160503if  1c                   if        *in42
054100160503     c                   eval      DS9turn1=  V1Cturn1
054200160503     c                   eval      DS9turn2=  V1Cturn2
054300160503x   1c                   else
054400160503     c                   clear                   DS9turn1
054500160503     c                   clear                   DS9turn2
054600160503e   1c                   endif
054700070312     c                   eval      DS9fpp  =  V1Cfpp
054800070312     c                   eval      DS9ord  =  V1Cord
054900071119     c                   eval      DS9sms  =  V1Csms
055000070313      * Parametri per override ai printer-file:
055100070316     c                   eval      DS9psl  =  D90psl
055200070312     c                   eval      DS9mdb  =  D90mdb
055300070312     c                   eval      DS9mdb4 =  D90mdb4
055400070312     c                   eval      DS9mdb5 =  D90mdb5
055500070312     c                   eval      DS9prb  =  D90prb
055600070312     c                   eval      DS9prb4 =  D90prb4
055700070312     c                   eval      DS9prb5 =  D90prb5
055800070312      *
055900070312     c                   movel(p)  FNLRS9ds      KPJBU
056000070322     c                   movel     'LRS9'        KCOAZ
056100070312      *
056200070322     c                   if        KNMUS   =  *all'1'
056300070312     c                   call      'FNLRS9C'
056400070312     c                   parm                    KPJBA
056500070312     c                   else
056600070312     c                   call      'BCH10'
056700070312     c                   parm                    KPJBA
056800070312     c                   endif
056900070312      *
057000070312     c                   ENDSR
057100070312
057200070312** - $Msg -------------------------------------------------------------------*
057300070322FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
057400070322Data distinta obbligatoria                                                      2
057500070322Data formalmente errata                                                         3
057600070322Non trovate distinte entro i limiti indicati                                    4
057700070322Numero distinta errato: contiene caratteri NON numerici                         5
057800070322Numero distinta inesistente o annullato                                         6
057900070322Distinta già chiusa                                                             7
058000070322Non trovate distinte entro i limiti indicati                                    8
058100070625Numero distinta obbligatorio                                                    9
058200071015Limiti incompatibili: numero distinta al minore del numero distinta dal        10
058300070625"Tipo distinta" impostabile SOLO SE selezionate più di una distinta            11
058400070625"Prestazione padroncino" impostabile SOLO SE selezionate più di una distinta   12
058500071015Procedura picking NON attivata                                                 13
058600071203"Tipo Distinta" obbligatorio SE selezionate più di una distinta                14
058700160503Almeno un "Turno" obbligatorio                                                 15
058800160503"Turno" non impostabile per distinte di tipo "Messaggerie"                     16
058900160503"Turno" errato                                                                 17
059000160503Singolo "turno" impostabile SOLO SE selezionate più di una distinta            18
059100160503"Turno" già inserito                                                           19
