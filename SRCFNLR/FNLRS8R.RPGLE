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
005700160720      * -?Parametri per FIDG31R: Interrogaz./Ricerca Distinte di Consegna?
005800160720     d FIDG31ds      e ds                  inz
005900160720     d   FDGric      e                     inz('I')
006000160720     d   FDGnpg      e                     inz(4)
006100160720     d   FDGtfv      e                     inz('2')
006200160720     d   FDGtpv      e                     inz('T')
006300160720     d sk_FV           s              6  0 dim(600)
006400160720     d                                     inz(*hival)
006500070312      *
006600070312     d Status         sds
006700070312     d  SDSpgm           *proc
006800070312      *
006900070312      * - Parametri per pgm. controllo data
007000070312     d WLBdat          ds                  inz
007100070312     d  G08dat                        8  0 inz
007200070312     d  G08inv                        8  0 inz
007300070312     d  G08err                        1    inz('3')
007400070312     d  G08tgi                        5  0 inz
007500070312      *
007600070312      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007700070312      *
007800070312      * - Flags
007900070312     d $Fine           s              1    inz(*off)
008000070322     d $InzD01         s              1    inz(*on)
008100070312      *
008200070312      * - Variabili di comodo
008300070315     d wDate           s              8  0 inz
008400070315     d wDateA          s              8    inz
008500071119     d W1Cndd          s                   inz  like(DSTnfv)
008600070312      *
008700070312      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008800070312      *
008900071119     c     K03DST01      klist
009000071119     c                   kfld                    DSTnpg
009100071119     c                   kfld                    DSTnfv
009200071119     c                   kfld                    DSTfgs
009300070312      *
009400071119     c     K03DST02      klist
009500071119     c                   kfld                    DSTnpg
009600071119     c                   kfld                    DSTdfv
009700071119     c                   kfld                    DSTfgs
009800071119      *
009900071119     c     K02APD01      klist
010000071119     c                   kfld                    DSTtipa
010100071119     c                   kfld                    DSTpdr
010200070312
010300070312      *---------------------------------------------------------------*
010400070312      *? RIEPILOGO INDICATORI                                        ?*
010500070312      *---------------------------------------------------------------*
010600070319      * 01   - Disabilitazione tasto funzionale F1   e                *
010700070319      *        Sprotezione campo "Data Distinta"                      *
010800070319      * 06   - Disabilitazione tasto funzionale F6                    *
010900070319      *        (Filiale non abilitata)                                *
011000070319      * 18   - Disabilitazione tasto funzionale F18  e                *
011100070319      *        Sprotezione campo "Filiale Gestione"                   *
011200070312      * 28   - Emissione messaggio di errore                          *
011300070625      * 40   - Visualizzazione richiesta flag "MESSAGGERIE"           *
011400070319      * 50   - Filiale Gestione errata                                *
011500070319      * 51   - Data Distinta errata                                   *
011600070319      * 52   - Numero Distinta iniziale errato                        *
011700070319      * 53   - Numero Distinta finale   errato                        *
011800070312      * 90   - Riemissione videata                                    *
011900070312      *---------------------------------------------------------------*
012000070312
012100070312     c     *Entry        plist
012200070312     c                   parm                    KPJBA
012300070312      *
012400070312      * Operazioni iniziali
012500070312     c                   exsr      sr_Start
012600070312      *
012700070312      * Ciclo di gestione della videata di lancio
012800070322do  1c                   dow       $Fine   =  *off
012900070322     c                   exsr      GesD01
013000070322e   1c                   enddo
013100070312      *
013200070312      * Fine
013300070312     c                   eval      *inLR   =  *on
013400070312
013500070312      *---------------------------------------------------------------*
013600070312      *?Operazioni Iniziali                                          ?*
013700070312      *---------------------------------------------------------------*
013800070312     c     sr_Start      BEGSR
013900070312      *
014000070312      * Reperimento dati utente
014100070312     c     *dtaara       define    §azute        AZUTEds
014200070312     c     *dtaara       define    §datiute      dDATIUTE
014300070312     c                   in(E)     *dtaara
014400070312     c                   if        %ERROR or RSUT = *blanks
014500070312     c                   clear                   TIBS34ds
014600070312     c                   call      'TIBS34R'
014700070312     c                   parm                    TIBS34ds
014800070312     c                   in        *dtaara
014900070312     c                   endif
015000070312      *
015100070312      * Richiesta stampanti
015200070312     c                   reset                   TRUL90ds
015300070312     c                   call      'TRUL90R'
015400070312     c                   parm                    KPJBA
015500070312     c                   parm                    TRUL90ds
015600070312      *
015700070312     c                   if        D90F3   =  *on
015800070312     c                   eval      $Fine   =  *on
015900070312     c                   leavesr
016000070312     c                   endif
016100070312      *
016200070322      * Impostazione iniziale dei dati fissi in testata
016300070312     c                   eval      V1Cpgm  =  SDSpgm
016400070322      *
016500070322      * Impostazione della *DATE in formato alfanumerico
016600070322     c                   clear                   WLBdat
016700070322     c                   eval      G08dat  =  *date
016800070322     c                   call      'XSRDA8'
016900070322     c                   parm                    WLBdat
017000070322if  1c                   eval      wDate   =  G08inv
017100070322if  1c                   move      wDate         wDateA
017200070312      *
017300070312     c                   ENDSR
017400070322      *
017500070322      *---------------------------------------------------------------*
017600070322      *?Gestione videata D01                                         ?*
017700070322      *---------------------------------------------------------------*
017800070322     c     GesD01        BEGSR
017900070322      *
018000070322if  1c                   if        $InzD01 =  *on
018100070322     c                   exsr      InzD01
018200070322     c                   eval      $InzD01 =  *off
018300070322e   1c                   endif
018400070322      *
018500070322      * Emissione testata e piede
018600070322     c                   write     LRS8T01
018700070322     c                   write     LRS8Z01
018800070322      *
018900070322      * Emissione videata di dettaglio
019000070322     c                   exfmt     LRS8D01
019100070322     c                   clear                   V1Dmsg
019200070322     c                   setoff                                       28  90
019300070322      *
019400070322sel 1c                   SELECT
019500070322      * - F1 = Modifica Data Distinta
019600070322w   2c                   WHEN      *inKA
019700070322     c                   eval      *in01   =  *on
019800070322     c                   eval      *in51   =  *on
019900070322      * - F3 = Fine
020000070322w   1c                   WHEN      *inKC
020100070322     c                   eval      $Fine   =  *on
020200070322      * - F18 = Modifica Filiale in Gestione
020300070322w   1c                   WHEN      *inKS
020400070322     c                   eval      *in18   =  *on
020500070322     c                   eval      *in50   =  *on
020600070322      * - F9 = Personalizzazione Batch
020700070322w   1c***                WHEN      *inKI
020800070322     c***                eval      KCOAZ   =  'LRS9'
020900070322     c***                call      'BCH09'
021000070322     c***                parm                    KPJBA
021100070322      * - Enter/F6: Controllo video D01
021200070322x   1c                   OTHER
021300070322     c                   exsr      CtrD01
021400070322      * - F6 = Lancio batch (se abilitato e se NO errori)
021500070322if  2c                   if                *inKF
021600070322     c                             and NOT *in06
021700070322     c                             and NOT *in90
021800070322     c                   exsr      sr_SbmJob
021900070322     c                   eval      $Fine   =  *on
022000070322e   2c                   endif
022100070322e   1c                   ENDSL
022200070322      *
022300070322     c                   ENDSR
022400070322      *
022500070322      *---------------------------------------------------------------*
022600070322      *?Inizializzazione videata D01                                 ?*
022700070322      *---------------------------------------------------------------*
022800070322     c     InzD01        BEGSR
022900070322      *
023000070322     c                   clear                   LRS8D01
023100070322      *
023200070322     c                   if        DUTlpo  =  '2'
023300070322     c                   eval      V1Cfgs  =  DUTpou
023400070322     c                   else
023500070322     c                   eval      V1Cfgs  =  DUTtfp
023600070322     c                   endif
023700070322     c                   eval      V1Cddc  =  *date
023800101008     c                   eval      V1Cpda  =  'N'
023900071015     c                   eval      V1Cord  =  '1'
024000071119     c                   eval      V1Csms  =  'S'
024100070322      *
024200070322      * Decodifiche
024300070322     c                   exsr      CtrD01
024400070322if  1c                   if        NOT  *in06
024500070322     c                   movea     *zeros        *in(28)
024600070322e   1c                   endif
024700070625      *
024800070625      * Impostazione indicatori
024900070625     c                   eval      *in40    = (§OGpkm = 'S')
025000101021     c                   eval      *in41    = (§OGpdacon = 'S')                 Procedura PDA avviat
025100160503     c                   eval      *in42    = (§OGturno = 'S')                  Turno Picking Conseg
025200160503      *
025300160503if  1c                   if        *in42
025400160503     c                   eval      V1Cturn1 = '1'
025500160503     c                   eval      V1Cturn2 = '2'
025600160503e   1c                   endif
025700070322      *
025800070322     c                   ENDSR
025900070312      *
026000070312      *---------------------------------------------------------------*
026100070312      *?Controlli videata D01                                        ?*
026200070312      *---------------------------------------------------------------*
026300070312     c     CtrD01        BEGSR
026400070312      *
026500070312     c                   movea     *zeros        *in(50)
026600070312      *
026700070312      *  Controllo filiale gestione
026800070322     c                   clear                   V1Dfgs
026900070312if  1c                   if        V1Cfgs  <> D50fgs
027000070312     c                   clear                   FNLV50ds
027100070312     c                   eval      D50pru  =  KNMUS
027200070312     c                   eval      D50fgs  =  V1Cfgs
027300070312     c                   call      'FNLV50R'
027400070312     c                   parm                    FNLV50ds
027500070312e   1c                   endif
027600070312if  1c                   if        D50err  <> *blanks
027700070312     c                   seton                                        285090
027800070312     c                   movel     D50msg        V1Dmsg
027900070312     c                   leavesr
028000070312e   1c                   endif
028100070322      * Decodifica filiale gestione
028200070322if  1c                   if        V1Cfgs  <> ORGfil
028300070322     c     V1Cfgs        chain     AZORG
028400070322if  2c                   if        NOT  %found(AZORG01L)
028500070322     c                             or   ORGfva <> *blanks
028600070322     c                   clear                   ORGdes
028700070625     c                   clear                   ORGde6
028800101008     c                   clear                   ORGde8
028900070625     c                   clear                   ORGdf0
029000070322e   2c                   endif
029100070322e   1c                   endif
029200070322     c                   movel     ORGdes        V1Dfgs
029300070322     c                   movel     ORGde6        OG146
029400101008     c                   movel     ORGde8        OG148
029500070625     c                   movel     ORGdf0        OG150
029600071005if  2c                   if        §OGdda  <= *zeros
029700071005     c                   eval      §OGdda  =  *hival
029800071005e   2c                   endif
029900070625      *
030000070625      * Impostazione indicatori
030100070625     c                   eval      *in06    = (§OGdda > wDateA)
030200070625     c                   eval      *in40    = (§OGpkm = 'S')
030300160503     c                   eval      *in42    = (§OGturno = 'S')
030400070322      *
030500070322      * Funzione non eseguibile (ad oggi)
030600070625if  1c                   if        *in06
030700070322     c                   seton                                        28  90
030800070322     c                   eval      V1Dmsg  =  $Msg( 1)
030900070322     c                   leavesr
031000070322e   1c                   endif
031100070312      *
031200070312     c                   eval      DS9fgs  =  V1Cfgs
031300070312      *
031400070312      * Data distinta
031500070312if  1c                   if        V1Cddc  =  *zeros
031600070312     c                   seton                                        285190
031700070322     c                   eval      V1Dmsg  =  $Msg( 2)
031800070312     c                   leavesr
031900070312e   1c                   endif
032000070312     c                   clear                   WLBdat
032100070312     c                   eval      G08dat  =  V1Cddc
032200070312     c                   call      'XSRDA8'
032300070312     c                   parm                    WLBdat
032400070312if  1c                   if        G08err  =  *on
032500070312     c                   seton                                        285190
032600070322     c                   eval      V1Dmsg  =  $Msg( 3)
032700070312     c                   leavesr
032800070312e   1c                   endif
032900070312      *
033000070312     c                   eval      V1Cddc  =  G08dat
033100070312     c                   eval      DS9ddc  =  G08inv
033200070312      *
033300071119     c                   eval      DSTnpg  =  4
033400071119     c                   eval      DSTdfv  =  DS9ddc
033500071119     c                   eval      DSTfgs  =  V1Cfgs
033600071119     c     K03DST02      chain     FIDST002
033700071119if  1c                   if        NOT %found(FIDST02L)
033800070312     c                   seton                                        285290
033900070322     c                   eval      V1Dmsg  =  $Msg( 4)
034000070312     c                   leavesr
034100070312e   1c                   endif
034200070312      *
034300070312      * Numero distinta da
034400070312     c                   clear                   W1Cndd
034500070316     c                   clear                   V1Dndd
034600070312if  1c                   if        V1Cndd  =  *zeros
034700070312     c                   clear                   V1Cndd
034800070312e   1c                   endif
034900070312if  1c                   IF             V1Cndd <> *blanks
035000070312     c                             and  V1Cndd <> *zeros
035100070312      * - Ricerca
035200070312if  2c                   if        %scan('?' : V1Cndd) > *zeros
035300160720     c                   reset                   FIDG31ds
035400160720     c                   eval      FDGgda  =  DS9ddc
035500160720     c                   eval      FDGgal  =  DS9ddc
035600160720     c                   eval      FDGfgs  =  V1Cfgs
035700160720     c                   eval      FDGpgm  =  SDSpgm
035800160720     c                   eval      FDGtdis =  V1Ctpm
035900160720     c                   eval      FDGtusc =  V1Cfpp
036000160720     c*//                if        V1Csms  =  'S'
036100160720     c*//                eval      FDGstp  =  'N'
036200160720     c*//                endif
036300160720     c                   eval      FDGtur1 =  V1Cturn1
036400160720     c                   eval      FDGtur2 =  V1Cturn2
036500160720     c                   movel(p)  FIDG31ds      KPJBU
036600160720     c                   call      'FIDG31R'
036700070312     c                   parm                    KPJBA
036800160720     c                   parm                    sk_FV
036900160720     c                   movel     KPJBU         FIDG31ds
037000160720     c                   move      FDGnfv        V1Cndd
037100070312e   2c                   endif
037200070312      * - Controllo validità
037300070312if  2c                   if        %check(DigitN:V1Cndd) = *zeros
037400070312     c                   move      V1Cndd        W1Cndd
037500071119     c                   eval      DSTnpg  =  4
037600071119     c                   eval      DSTnfv  =  W1Cndd
037700071119     c                   eval      DSTfgs  =  V1Cfgs
037800071119     c     K03DST01      chain     FIDST000
037900070312e   2c                   endif
038000070312sel 2c                   Select
038100070312w   2c                   When      %check(DigitN:V1Cndd) > *zeros
038200070312     c                   seton                                        285290
038300070322     c                   eval      V1Dmsg  =  $Msg( 5)
038400070312     c                   leavesr
038500071119w   2c                   When      NOT  %found(FIDST01L)
038600071119     c                             or   DSTatb <> *blanks
038700070312     c                   seton                                        285290
038800070322     c                   eval      V1Dmsg  =  $Msg( 6)
038900070312     c                   leavesr
039000071119w   2c                   When      DSTfcf  =  'S'
039100070312     c                   seton                                        285290
039200070322     c                   eval      V1Dmsg  =  $Msg( 7)
039300070312     c                   leavesr
039400071119w   2c                   When      DSTdfv  <> DS9ddc
039500070312     c                   seton                                        285290
039600070322     c                   eval      V1Dmsg  =  $Msg( 8)
039700070312     c                   leavesr
039800070312x   2c                   Other
039900071119     c     K02APD01      chain     FIAPD000
040000071119     c                   if        %found(FIAPD01L)
040100071119     c                   eval      V1Dndd  =  APDrsc
040200071119     c                   else
040300071119     c                   clear                   V1Dndd
040400071119     c                   endif
040500070312if  3c                   if        V1Cnda  =  *zeros
040600070312     c                   move      V1Cndd        V1Cnda
040700070312e   3c                   endif
040800070312e   2c                   EndSl
040900070312e   1c                   ENDIF
041000070312      *
041100070312     c                   eval      DS9ndi  =  W1Cndd
041200070312      *
041300070312      * Numero distinta a
041400070312if  1c                   if        V1Cnda  <= *zeros
041500070314     c                   seton                                        285390
041600070625     c                   eval      V1Dmsg  =  $Msg( 9)
041700070314     c                   leavesr
041800070314e   1c                   endif
041900070312      *
042000070312     c                   eval      DS9ndf  =  V1Cnda
042100070312      *
042200070322      * Range incompatibile
042300070312if  1c                   if        DS9ndi  >  DS9ndf
042400070312     c                   seton                                        285390
042500070625     c                   eval      V1Dmsg  =  $Msg(10)
042600070625     c                   leavesr
042700070312e   1c                   endif
042800071015      *
042900071015      * Ordinamento
043000071015if  1c                   if             V1Cord  = '2'
043100071015     c                             and  §OGpck <> 'S'
043200071015     c                   seton                                        285490
043300071015     c                   eval      V1Dmsg  =  $Msg(13)
043400071015     c                   leavesr
043500071015e   1c                   endif
043600070625      *
043700070625      * Selezione "Tipo distinta"
043800070625if  1c                   if             V1Ctpm <> *blanks
043900070625     c                             and  DS9ndi  = DS9ndf
044000070625     c                   seton                                        285390
044100070625     c                   eval      V1Dmsg  =  $Msg(11)
044200070625     c                   leavesr
044300070625e   1c                   endif
044400071203if  1c                   if             V1Ctpm  = *blanks
044500071203     c                             and  *in40   = *on
044600071203     c                             and  DS9ndi <> DS9ndf
044700071203     c                   seton                                        285590
044800071203     c                   eval      V1Dmsg  =  $Msg(14)
044900071203     c                   leavesr
045000071203e   1c                   endif
045100160503      *
045200160503      * Selezione "Turno"
045300160503sel 1c                   select
045400160503      * - Filiale gestione NON abilitata
045500160503w   1c                   when      Not *in42
045600160503     c                   clear                   V1Cturn1
045700160503     c                   clear                   V1Cturn2
045800160503      * - Turno impostabile SOLO SE selezionate più di una distinta
045900160503w   1c                   when           DS9ndi   =  DS9ndf
046000160503     c                             and  V1Ctpm   <> 'M'
046100160503     c                             and (V1Cturn1 <> *blanks
046200160503     c                             and  V1Cturn2 =  *blanks
046300160503     c                              or  V1Cturn1 =  *blanks
046400160503     c                             and  V1Cturn2 <> *blanks)
046500160503     c                   if        V1Cturn1 <> *blanks
046600160503     c                   seton                                        285690
046700160503     c                   else
046800160503     c                   seton                                        285790
046900160503     c                   endif
047000160503     c                   eval      V1Dmsg  =  $Msg(18)
047100160503     c                   leavesr
047200160503      * - "Turno 1" o "Turno 2"
047300160503w   1c                   when      DS9ndi   <> DS9ndf  and
047400160503     c                             V1Ctpm   <> 'M'     and
047500160503     c                             V1Cturn1 =  *blank  and
047600160503     c                             V1Cturn2 =  *blank
047700160503     c                   seton                                        285690
047800160503     c                   eval      V1Dmsg   =  $Msg(15)
047900160503     c                   leavesr
048000160503      * - "Turno 1"
048100160503w   1c                   when      V1Ctpm   =  'M'  and
048200160503     c                             V1Cturn1 <> *blank
048300160503     c                   seton                                        285690
048400160503     c                   eval      V1Dmsg   =  $Msg(16)
048500160503     c                   leavesr
048600160503w   1c                   when      V1Ctpm   <> 'M'     and
048700160503     c                             V1Cturn1 <> *blank  and
048800160503     c                             V1Cturn1 <> '1'     and
048900160503     c                             V1Cturn1 <> '2'
049000160503     c                   seton                                        285690
049100160503     c                   eval      V1Dmsg   =  $Msg(17)
049200160503     c                   leavesr
049300160503      * - "Turno 2"
049400160503w   1c                   when      V1Ctpm   =  'M'  and
049500160503     c                             V1Cturn2 <> *blank
049600160503     c                   seton                                        285790
049700160503     c                   eval      V1Dmsg   =  $Msg(16)
049800160503     c                   leavesr
049900160503w   1c                   when      V1Ctpm   <> 'M'     and
050000160503     c                             V1Cturn2 <> *blank  and
050100160503     c                             V1Cturn2 <> '1'     and
050200160503     c                             V1Cturn2 <> '2'
050300160503     c                   seton                                        285790
050400160503     c                   eval      V1Dmsg   =  $Msg(17)
050500160503     c                   leavesr
050600160503      * - Turno già impostato
050700160503w   1c                   when      V1Ctpm   <> 'M'     and
050800160503     c                             V1Cturn1 <> *blank  and
050900160503     c                             V1Cturn1 =  V1Cturn2
051000160503     c                   seton                                        285690
051100160503     c                   eval      V1Dmsg   =  $Msg(19)
051200160503     c                   leavesr
051300160503      *
051400160503e   1c                   endsl
051500070625      *
051600070625      * Selezione "Prestazione padroncino"
051700070625if  1c                   if             V1Cfpp <> *blanks
051800070625     c                             and  DS9ndi  = DS9ndf
051900070625     c                   seton                                        285390
052000070625     c                   eval      V1Dmsg  =  $Msg(12)
052100070625     c                   leavesr
052200070625e   1c                   endif
052300070312      *
052400070312     c                   ENDSR
052500070312      *
052600070312      *---------------------------------------------------------------*
052700070312      *?Sottomissione lavoro batch                                   ?*
052800070312      *---------------------------------------------------------------*
052900070312     c     sr_SbmJob     BEGSR
053000070625      *
053100070625     c*** già così:      clear                   FNLRS9ds
053200070625      *
053300070625      * Parametri già impostati nella subr. "CTRD01":
053400070625     c***                eval      DS9fgs  =  V1Cfgs
053500070625     c***                eval      DS9ddc  =  G08inv
053600070625     c***                eval      DS9ndi  =  W1Cndd
053700070625     c***                eval      DS9ndf  =  V1Cnda
053800070312      *
053900070625      * Parametri NON ancora impostati:
054000101008if  1c                   if        *in41
054100101008     c                   eval      DS9pda  =  V1Cpda
054200101008x   1c                   else
054300101008     c                   clear                   DS9pda
054400101008e   1c                   endif
054500070625if  1c                   if        *in40
054600070621     c                   eval      DS9tpm  =  V1Ctpm
054700070625x   1c                   else
054800070625     c                   clear                   DS9tpm
054900070625e   1c                   endif
055000160503if  1c                   if        *in42
055100160503     c                   eval      DS9turn1=  V1Cturn1
055200160503     c                   eval      DS9turn2=  V1Cturn2
055300160503x   1c                   else
055400160503     c                   clear                   DS9turn1
055500160503     c                   clear                   DS9turn2
055600160503e   1c                   endif
055700070312     c                   eval      DS9fpp  =  V1Cfpp
055800070312     c                   eval      DS9ord  =  V1Cord
055900071119     c                   eval      DS9sms  =  V1Csms
056000070313      * Parametri per override ai printer-file:
056100070316     c                   eval      DS9psl  =  D90psl
056200070312     c                   eval      DS9mdb  =  D90mdb
056300070312     c                   eval      DS9mdb4 =  D90mdb4
056400070312     c                   eval      DS9mdb5 =  D90mdb5
056500070312     c                   eval      DS9prb  =  D90prb
056600070312     c                   eval      DS9prb4 =  D90prb4
056700070312     c                   eval      DS9prb5 =  D90prb5
056800070312      *
056900070312     c                   movel(p)  FNLRS9ds      KPJBU
057000070322     c                   movel     'LRS9'        KCOAZ
057100070312      *
057200070322     c                   if        KNMUS   =  *all'1'
057300070312     c                   call      'FNLRS9C'
057400070312     c                   parm                    KPJBA
057500070312     c                   else
057600070312     c                   call      'BCH10'
057700070312     c                   parm                    KPJBA
057800070312     c                   endif
057900070312      *
058000070312     c                   ENDSR
058100070312
058200070312** - $Msg -------------------------------------------------------------------*
058300070322FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
058400070322Data distinta obbligatoria                                                      2
058500070322Data formalmente errata                                                         3
058600070322Non trovate distinte entro i limiti indicati                                    4
058700070322Numero distinta errato: contiene caratteri NON numerici                         5
058800070322Numero distinta inesistente o annullato                                         6
058900070322Distinta già chiusa                                                             7
059000070322Non trovate distinte entro i limiti indicati                                    8
059100070625Numero distinta obbligatorio                                                    9
059200071015Limiti incompatibili: numero distinta al minore del numero distinta dal        10
059300070625"Tipo distinta" impostabile SOLO SE selezionate più di una distinta            11
059400070625"Prestazione padroncino" impostabile SOLO SE selezionate più di una distinta   12
059500071015Procedura picking NON attivata                                                 13
059600071203"Tipo Distinta" obbligatorio SE selezionate più di una distinta                14
059700160503Almeno un "Turno" obbligatorio                                                 15
059800160503"Turno" non impostabile per distinte di tipo "Messaggerie"                     16
059900160503"Turno" errato                                                                 17
060000160503Singolo "turno" impostabile SOLO SE selezionate più di una distinta            18
060100160503"Turno" già inserito                                                           19
