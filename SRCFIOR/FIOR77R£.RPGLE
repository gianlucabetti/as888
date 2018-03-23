000100070925      * Fior77R *-----------------------------------------------------*
000200070925      *?STAMPA ORM DA DISTINTE AUTOTRASPORTATORI - FILTRO DI LANCIO  ?*
000300070312      *---------------------------------------------------------------*
000400070312
000500070312     h decedit('0,') datedit(*dmy/)
000600070312
000700070312      *---------------------------------------------------------------*
000800070312
000900070315     fAZORG01L  if   e           k disk
001000070315      *
001100070927     fFidst09L  if   e           k disk
001200070927     fFidst08L  if   e           k disk    rename(Fidst000:Fidst002)
001300070312      *
001400070925     fFior77D   cf   e             workstn
001500070312
001600070312      *---------------------------------------------------------------*
001700070312
001800070312      *
001900070312      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002000070312      *
002100070312      * - Costante per controllo "caratteri solo numerici"
002200070312     d DigitN          c                   const('1234567890')
002300070312      *
002400070312      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002500070312      *
002600070312      * - Messaggi di errore
002700071015     d $Msg            s             78    dim(13) ctdata perrcd(1)
002800070312      *
002900070312      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003000070312      *
003100070312      * - Parametri
003200070312     d KPJBA         e ds
003300070312      *
003400070925      * - Parametri per Fior78R: Stampa ORM da distinte AUT (batch)
003500070925     d FIOR78ds      e ds                  inz
003600070312      *
003700070312      * - Parametri x Controllo profilo utenti
003800070312     d TIBS34ds      e ds
003900070312      * - Ds di riferimento al file esterno AZUTE00F
004000070312     d AZUTEds       e ds                       extname(AZUTE00F)
004100070312      * - Ds per dati organigramma
004200070312     d dDatiUte      e ds
004300070315      *
004400070925     d OG147         e ds                  inz
004401080320     d OG148         e ds                  inz
004500070927     d OG150         e ds                  inz
004600070312      *
004700070925      * - Parametri per richiesta stampante per ORM
004800070312     d TRUL90ds      e ds                  inz
004900070925     d   D90rso      e                     inz('S')
005000070312      *
005100070321      * - Parametri per FNLV50R: Controllo Filiale in Gestione
005200070312     d FNLV50ds      e ds                  inz
005300070312      *
005400070312      * - Parametri per FNLR02R: Interrogazione Fogli Vari
005500070312     d dsLR01        e ds                  inz
005600070312     d   DLRnpg      e                     inz(4)
005700070312     d   DLRric      e                     inz('S')
005800070312     d   DLRtfv      e                     inz('2')
005900070312      *
006000070312     d Status         sds
006100070312     d  SDSpgm           *proc
006200070312      *
006300070312      * - Parametri per pgm. controllo data
006400070312     d WLBdat          ds                  inz
006500070312     d  G08dat                        8  0 inz
006600070312     d  G08inv                        8  0 inz
006700070312     d  G08err                        1    inz('3')
006800070312     d  G08tgi                        5  0 inz
006900070312      *
007000070312      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
007100070312      *
007200070312      * - Flags
007300070312     d $Fine           s              1    inz(*off)
007400070322     d $InzD01         s              1    inz(*on)
007500070312      *
007600070312      * - Variabili di comodo
007700070315     d wDate           s              8  0 inz
007800070315     d wDateA          s              8    inz
007900070927     d W1Cndd          s                   inz  like(DSTNFV)
008000070312      *
008100070312      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
008200070312      *
008300070927     c     K03dst01      klist
008400070927     c                   kfld                    dstfgs
008500070927     c                   kfld                    dstnpg
008600070927     c                   kfld                    dstnfv
008700070312      *
008800070927     c     K03dst02      klist
008900070927     c                   kfld                    dstfgs
009000070927     c                   kfld                    dstnpg
009100070927     c                   kfld                    dstdfv
009200070312
009300070312      *---------------------------------------------------------------*
009400070312      *? RIEPILOGO INDICATORI                                        ?*
009500070312      *---------------------------------------------------------------*
009600070319      * 01   - Disabilitazione tasto funzionale F1   e                *
009700070319      *        Sprotezione campo "Data Distinta"                      *
009800070928      * 02   - abilito f18                                            *
009900070319      * 06   - Disabilitazione tasto funzionale F6                    *
010000070319      *        (Filiale non abilitata)                                *
010100070319      * 18   - Disabilitazione tasto funzionale F18  e                *
010200070319      *        Sprotezione campo "Filiale Gestione"                   *
010300070312      * 28   - Emissione messaggio di errore                          *
010400070625      * 40   - Visualizzazione richiesta flag "MESSAGGERIE"           *
010500070319      * 50   - Filiale Gestione errata                                *
010600070319      * 51   - Data Distinta errata                                   *
010700070319      * 52   - Numero Distinta iniziale errato                        *
010800070319      * 53   - Numero Distinta finale   errato                        *
010900070312      * 90   - Riemissione videata                                    *
011000070312      *---------------------------------------------------------------*
011100070312
011200070312     c     *Entry        plist
011300070312     c                   parm                    KPJBA
011400070312      *
011500070312      * Operazioni iniziali
011600070312     c                   exsr      sr_Start
011700070312      *
011800070312      * Ciclo di gestione della videata di lancio
011900070322do  1c                   dow       $Fine   =  *off
012000070322     c                   exsr      GesD01
012100070322e   1c                   enddo
012200070312      *
012300070312      * Fine
012400070312     c                   eval      *inLR   =  *on
012500070312
012600070312      *---------------------------------------------------------------*
012700070312      *?Operazioni Iniziali                                          ?*
012800070312      *---------------------------------------------------------------*
012900070312     c     sr_Start      BEGSR
013000070312      *
013100070312      * Reperimento dati utente
013200070312     c     *dtaara       define    §azute        AZUTEds
013300070312     c     *dtaara       define    §datiute      dDATIUTE
013400070312     c                   in(E)     *dtaara
013500070312     c                   if        %ERROR or RSUT = *blanks
013600070312     c                   clear                   TIBS34ds
013700070312     c                   call      'TIBS34R'
013800070312     c                   parm                    TIBS34ds
013900070312     c                   in        *dtaara
014000070312     c                   endif
014100070925
014200070312      *
014300070312      * Richiesta stampanti
014400070312     c                   reset                   TRUL90ds
014500070312     c                   call      'TRUL90R'
014600070312     c                   parm                    KPJBA
014700070312     c                   parm                    TRUL90ds
014800070312      *
014900070312     c                   if        D90F3   =  *on
015000070312     c                   eval      $Fine   =  *on
015100070312     c                   leavesr
015200070312     c                   endif
015300070312      *
015400070322      * Impostazione iniziale dei dati fissi in testata
015500070312     c                   eval      V1Cpgm  =  SDSpgm
015600070322      *
015700070322      * Impostazione della *DATE in formato alfanumerico
015800070322     c                   clear                   WLBdat
015900070322     c                   eval      G08dat  =  *date
016000070322     c                   call      'XSRDA8'
016100070322     c                   parm                    WLBdat
016200070925     c                   eval      wDate   =  G08inv
016300070925     c                   move      wDate         wDateA
016400070928      *
016500070312      *
016600070312     c                   ENDSR
016700070322      *
016800070322      *---------------------------------------------------------------*
016900070322      *?Gestione videata D01                                         ?*
017000070322      *---------------------------------------------------------------*
017100070322     c     GesD01        BEGSR
017200070322      *
017300070322if  1c                   if        $InzD01 =  *on
017400070322     c                   exsr      InzD01
017500070322     c                   eval      $InzD01 =  *off
017600070322e   1c                   endif
017700070322      *
017800070322      * Emissione testata e piede
017900070925     c                   write     or77T01
018000070925     c                   write     or77Z01
018100070322      *
018200070322      * Emissione videata di dettaglio
018300070925     c                   exfmt     or77D01
018400070322     c                   clear                   V1Dmsg
018500070322     c                   setoff                                       28  90
018600070322      *
018700070322sel 1c                   SELECT
018800070322      * - F1 = Modifica Data Distinta
018900070322w   2c                   WHEN      *inKA
019000070322     c                   eval      *in01   =  *on
019100070322     c                   eval      *in51   =  *on
019200070322      * - F3 = Fine
019300070322w   1c                   WHEN      *inKC
019400070322     c                   eval      $Fine   =  *on
019500070322      * - F18 = Modifica Filiale in Gestione
019600070322w   1c                   WHEN      *inKS
019700070322     c                   eval      *in18   =  *on
019800070322     c                   eval      *in50   =  *on
019900070322      * - Enter/F6: Controllo video D01
020000070322x   1c                   OTHER
020100070322     c                   exsr      CtrD01
020200070322      * - F6 = Lancio batch (se abilitato e se NO errori)
020300070322if  2c                   if                *inKF
020400070322     c                             and NOT *in06
020500070322     c                             and NOT *in90
020600070322     c                   exsr      sr_SbmJob
020700070322     c                   eval      $Fine   =  *on
020800070322e   2c                   endif
020900070322e   1c                   ENDSL
021000070322      *
021100070322     c                   ENDSR
021200070322      *
021300070322      *---------------------------------------------------------------*
021400070322      *?Inizializzazione videata D01                                 ?*
021500070322      *---------------------------------------------------------------*
021600070322     c     InzD01        BEGSR
021700070322      *
021800070925     c                   clear                   or77D01
021900070322      *
022000070322     c                   if        DUTlpo  =  '2'
022100070322     c                   eval      V1Cfgs  =  DUTpou
022200070322     c                   else
022300070928     c                   seton                                        02
022400070928     c                   eval      V1Cfgs  =  Simfel
022500070322     c                   endif
022600070322     c                   eval      V1Cddc  =  *date
022700071105     c                   eval      V1Cord  =  '1'
022800070322      *
022900070322      * Decodifiche
023000070322     c                   exsr      CtrD01
023100070322if  1c                   if        NOT  *in06
023200070322     c                   movea     *zeros        *in(28)
023300070322e   1c                   endif
023400070625      *
023500070625      * Impostazione indicatori
023600070625     c                   eval      *in40    = (§OGpkm = 'S')
023601080326     c                   eval      *in41    = (§OGpdaorm = 'S')                 Procedura PDA avviat
023602080320      *
023603080320      * imposto stampa orm scaricati su PDA = 'S'
023604080320     c                   eval      v1cpda = 'S'
023700070322      *
023800070322     c                   ENDSR
023900070312      *
024000070312      *---------------------------------------------------------------*
024100070312      *?Controlli videata D01                                        ?*
024200070312      *---------------------------------------------------------------*
024300070312     c     CtrD01        BEGSR
024400070312      *
024500070312     c                   movea     *zeros        *in(50)
024600070312      *
024700070312      *  Controllo filiale gestione
024800070322     c                   clear                   V1Dfgs
024900070312if  1c                   if        V1Cfgs  <> D50fgs
025000070312     c                   clear                   FNLV50ds
025100070312     c                   eval      D50pru  =  KNMUS
025200070312     c                   eval      D50fgs  =  V1Cfgs
025300070312     c                   call      'FNLV50R'
025400070312     c                   parm                    FNLV50ds
025500070312e   1c                   endif
025600070312if  1c                   if        D50err  <> *blanks
025700070312     c                   seton                                        285090
025800070312     c                   movel     D50msg        V1Dmsg
025900070312     c                   leavesr
026000070312e   1c                   endif
026100070322      * Decodifica filiale gestione
026200070322if  1c                   if        V1Cfgs  <> ORGfil
026300070322     c     V1Cfgs        chain     AZORG
026400070322if  2c                   if        NOT  %found(AZORG01L)
026500070322     c                             or   ORGfva <> *blanks
026600070322     c                   clear                   ORGdes
026700070925     c                   clear                   ORGde7
026701080320     c                   clear                   ORGde8
026800070322e   2c                   endif
026900070322e   1c                   endif
027000070322     c                   movel     ORGdes        V1Dfgs
027100070925     c                   movel     ORGde7        OG147
027101080320     c                   movel     ORGde8        OG148
027200070927     c                   movel     ORGdf0        OG150
027300070928
027400070928if  2c                   if        §OGddao <= *zeros
027500070928     c                   eval      §OGddao =  *hival
027600070928e   2c                   endif
027700070625      *
027800070625      * Impostazione indicatori
027900070925     c                   eval      *in06    = (§OGddao > wDateA)
028000070625     c                   eval      *in40    = (§OGpkm = 'S')
028001080320     c                   eval      *in41    = (§OGpdaorm = 'S')                 Procedura PDA avviat
028100070322      *
028200070322      * Funzione non eseguibile (ad oggi)
028300070625if  1c                   if        *in06
028400070322     c                   seton                                        28  90
028500070322     c                   eval      V1Dmsg  =  $Msg( 1)
028600070322     c                   leavesr
028700070322e   1c                   endif
028800070312      *
028900070927     c                   eval      r78fgs  =  V1Cfgs
029000070312      *
029100070312      * Data distinta
029200070312if  1c                   if        V1Cddc  =  *zeros
029300070312     c                   seton                                        285190
029400070322     c                   eval      V1Dmsg  =  $Msg( 2)
029500070312     c                   leavesr
029600070312e   1c                   endif
029700070312     c                   clear                   WLBdat
029800070312     c                   eval      G08dat  =  V1Cddc
029900070312     c                   call      'XSRDA8'
030000070312     c                   parm                    WLBdat
030100070312if  1c                   if        G08err  =  *on
030200070312     c                   seton                                        285190
030300070322     c                   eval      V1Dmsg  =  $Msg( 3)
030400070312     c                   leavesr
030500070312e   1c                   endif
030600070312      *
030700070312     c                   eval      V1Cddc  =  G08dat
030800071015      *
030900071102if  1c                   if             G08inv            <  wDate
031000071015     c                             and  %subst(KNMUS:1:3) <> 'EDP'
031100071015     c                   seton                                        285190
031200071015     c                   eval      V1Dmsg  =  $Msg(13)
031300071015     c                   leavesr
031400071015e   1c                   endif
031500071015      *
031600070927     c                   eval      r78ddc  =  G08inv
031700070312      *
031800070927     c                   eval      dstnpg  =  4
031900070927     c                   eval      dstdfv  =  r78ddc
032000070927     c                   eval      dstfgs  =  V1Cfgs
032100070927     c     K03dst02      chain     Fidst08l
032200070927if  1c                   if        NOT %found(Fidst08L)
032300070312     c                   seton                                        285290
032400070322     c                   eval      V1Dmsg  =  $Msg( 4)
032500070312     c                   leavesr
032600070312e   1c                   endif
032700070312      *
032800070312      * Numero distinta da
032900070312     c                   clear                   W1Cndd
033000070312if  1c                   if        V1Cndd  =  *zeros
033100070312     c                   clear                   V1Cndd
033200070312e   1c                   endif
033300070312if  1c                   IF             V1Cndd <> *blanks
033400070312     c                             and  V1Cndd <> *zeros
033500070312      * - Ricerca
033600070312if  2c                   if        %scan('?' : V1Cndd) > *zeros
033700070312     c                   reset                   dsLR01
033800070312     c                   eval      DLRpgm  =  SDSpgm
033900070312     c                   eval      DLRfgs  =  V1Cfgs
034000070312     c                   eval      DLRgda  =  V1Cddc
034100070312     c                   eval      DLRgal  =  V1Cddc
034200070312     c                   movel(p)  dsLR01        KPJBU
034300070312     c                   call      'FNLR02R'
034400070312     c                   parm                    KPJBA
034500070312     c                   movel     KPJBU         dsLR01
034600070312     c                   movel     DLRnfv        V1Cndd
034700070312e   2c                   endif
034800070312      * - Controllo validità
034900070312if  2c                   if        %check(DigitN:V1Cndd) = *zeros
035000070312     c                   move      V1Cndd        W1Cndd
035100070927     c                   eval      dstnpg  =  4
035200070927     c                   eval      dstnfv  =  W1Cndd
035300070927     c                   eval      dstfgs  =  V1Cfgs
035400070927     c     K03dst01      chain     Fidst09l
035500070312e   2c                   endif
035600070312sel 2c                   Select
035700070312w   2c                   When      %check(DigitN:V1Cndd) > *zeros
035800070312     c                   seton                                        285290
035900070322     c                   eval      V1Dmsg  =  $Msg( 5)
036000070312     c                   leavesr
036100070927w   2c                   When      NOT  %found(Fidst09l)
036200070927     c                             or   dstatb  <> ' '
036300070312     c                   seton                                        285290
036400070322     c                   eval      V1Dmsg  =  $Msg( 6)
036500070312     c                   leavesr
036600070927w   2c                   When      dstfcf  =  'S'
036700070312     c                   seton                                        285290
036800070322     c                   eval      V1Dmsg  =  $Msg( 7)
036900070312     c                   leavesr
037000070927w   2c                   When      dstdfv  <> r78ddc
037100070312     c                   seton                                        285290
037200070322     c                   eval      V1Dmsg  =  $Msg( 8)
037300070312     c                   leavesr
037400070312x   2c                   Other
037500070312if  3c                   if        V1Cnda  =  *zeros
037600070312     c                   move      V1Cndd        V1Cnda
037700070312e   3c                   endif
037800070312e   2c                   EndSl
037900070312e   1c                   ENDIF
038000070312      *
038100070927     c                   eval      r78ndi  =  W1Cndd
038200070312      *
038300070312      * Numero distinta a
038400070312if  1c                   if        V1Cnda  <= *zeros
038500070314     c                   seton                                        285390
038600070625     c                   eval      V1Dmsg  =  $Msg( 9)
038700070314     c                   leavesr
038800070314e   1c                   endif
038900070312      *
039000070927     c                   eval      r78ndf  =  V1Cnda
039100070312      *
039200070322      * Range incompatibile
039300070927if  1c                   if        r78ndi  >  r78ndf
039400070312     c                   seton                                        285390
039500070625     c                   eval      V1Dmsg  =  $Msg(10)
039600070625     c                   leavesr
039700070312e   1c                   endif
039800070625      *
039900070625      * Selezione "Tipo distinta"
040000070625if  1c                   if             V1Ctpm <> *blanks
040100070927     c                             and  r78ndi  = r78ndf
040200070625     c                   seton                                        285390
040300070625     c                   eval      V1Dmsg  =  $Msg(11)
040400070625     c                   leavesr
040500070625e   1c                   endif
040600070625      *
040700070625      * Selezione "Prestazione padroncino"
040800070625if  1c                   if             V1Cfpp <> *blanks
040900070927     c                             and  r78ndi  = r78ndf
041000070625     c                   seton                                        285390
041100070625     c                   eval      V1Dmsg  =  $Msg(12)
041200070625     c                   leavesr
041300070625e   1c                   endif
041400070312      *
041500070312     c                   ENDSR
041600070312      *
041700070312      *---------------------------------------------------------------*
041800070312      *?Sottomissione lavoro batch                                   ?*
041900070312      *---------------------------------------------------------------*
042000070312     c     sr_SbmJob     BEGSR
042100070625      *
042200070625if  1c                   if        *in40
042300070927     c                   eval      r78tpm  =  V1Ctpm
042400070625x   1c                   else
042500070927     c                   clear                   r78tpm
042600070625e   1c                   endif
042601080320      * se orm su pda imposto il flag  di orm scaricati su PDA
042602080320if  1c                   if        *in41
042603080320     c                   eval      r78pda  =  V1Cpda
042604080320x   1c                   else
042605080320     c                   clear                   r78pda
042606080320e   1c                   endif
042607080320
042700070927     c                   eval      r78fpp  =  V1Cfpp
042800070927     c                   eval      r78gia  =  V1Cris
042900071105     c                   eval      r78ord  =  V1Cord
043000070313      * Parametri per override ai printer-file:
043100070927     c                   eval      r78oqo  =  D90pro
043200070927     c                   eval      r78mdo  =  D90mdo
043300070312      *
043400070927     c                   movel(p)  Fior78ds      KPJBU
043500070927     c                   movel     'OR78'        KCOAZ
043600070312      *
043700071106     c                   if        KNMUS   <> *all'1'
043800070312     c                   call      'BCH10'
043900070312     c                   parm                    KPJBA
044000071106     c                   else
044100071106     c                   call      'FIOR78R'
044200071106     c                   parm                    KPJBA
044300071106     c                   endif
044400070312      *
044500070312     c                   ENDSR
044600070312
044700070312** - $Msg -------------------------------------------------------------------*
044800070322FUNZIONE NON ESEGUIBILE. Premere F3 per uscire.                                 1
044900070322Data distinta obbligatoria                                                      2
045000070322Data formalmente errata                                                         3
045100070322Non trovate distinte entro i limiti indicati                                    4
045200070322Numero distinta errato: contiene caratteri NON numerici                         5
045300070322Numero distinta inesistente o annullato                                         6
045400070322Distinta già chiusa                                                             7
045500070322Non trovate distinte entro i limiti indicati                                    8
045600070625Numero distinta obbligatorio                                                    9
045700071015Limiti incompatibili: numero distinta al minore del numero distinta dal        10
045800070625"Tipo distinta" impostabile SOLO SE selezionate più di una distinta            11
045900070625"Prestazione padroncino" impostabile SOLO SE selezionate più di una distinta   12
046000071015Non ammessa data distinta precedente quella odierna                            13
