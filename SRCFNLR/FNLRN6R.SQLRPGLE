000100070313      *---------------------------------------------------------------*
000200070309      * GESTIONE NOTE LDV                                             *
000300070313      *---------------------------------------------------------------*
000400070313
000500070313     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070313
000700070313      *---------------------------------------------------------------*
000800070313
000900070319     fFIARN11L  Uf A e           k disk
001000070319     fFIARN12L  If   e           k disk    rename(fiarn000:fiarn2)
001100070313     fFNARB01L  If   e           k disk
001200161122     fFNBLP01L  if   e           k disk
001300070312     fFIAR401L  If   e           k disk
001400070308     fAZORG01L  If   e           k disk
001500070313     fTABEL00F  If   e           k disk
001600131122     fFIDST08L  if   e           k disk
001700140304     fFIapd01l  if   e           k disk
001800140204     fFIARP01L  if   e           k disk
001900070313      *
002000131121     fFNLRN6D   cf   e             workstn sfile(LRN6S02:S01nrr)
002100131202     f                                     maxdev(*file)
002200070313
002300070313      *---------------------------------------------------------------*
002400070313
002500070313      *
002600070313      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002700070308      *
002800070313      *
002900070313      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003000070313      *
003100070404     d MSG             S             78    DIM(009) CTDATA PERRCD(1)            MSG VIDEO
003200070313      *
003300070313     d L6              s              3  0 dim(30)                              po £6
003400131120     D wpog            s              3    dim(250)
003500070313      *
003600070313      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003700131219      *
003800070313     d KPJBA         e ds
003900130304      *
004000131121     D ficauds       E DS                  extname(FICAU00F)
004100130304     D fnarbds       E DS                  extname(fnarb00f)
004200070313      * - Parametri
004300131121     d FNLRN6DS      e ds
004400070313      *
004500070313      * - Parametri x Controllo profilo utenti
004600070313     d TIBS34DS      e ds
004700131120      * - Tabelle
004800131120     d TIBS02ds      e ds                  inz
004900070313      * - Ds di riferimento al file esterno AZUTE00F
005000070313     d AZUTEDS       e ds                  extname(AZUTE00F)
005100070313      * - Ds per dati organigramma
005200070313     d DDatiUte      e ds
005300070315      * abilitazioni
005400070315     D OG148         E DS
005500070313      * DS per TRUL06R - Caricamento £X
005600070313     d TRUL06DS      E DS
005700070313     d  LIN                    1     90  0
005800070313     d                                     DIM(30)
005900131120
006000131120     D TRUL31DS      E DS
006100131120     d dLat          e ds
006200131120     d dute01        e ds
006300070313      *
006400130304      * DS per Fidg42R - passaggio dati a PDA
006500130304     d fidg42ds      e ds
006600131122
006700131122     d ddstflr       e ds
006800131128
006900131128       // Ds passaggio dati a pgm FNLRX3R
007000131128     d fnlrx3ds      e ds
007100131128
007200140121       // Ds passaggio dati a pgm TNTBCMDR
007300140121     d tntbcmdds1    e ds
007400140121
007500131128       // Tabella CMD
007600131128     d dCMD          e ds
007700131122
007800130304      *
007900070313     d Status         sds
008000070313     d  SDSpgm           *proc
008100070313     d  PARMS            *parms
008200070313      *
008300070313     d WLBdat          ds                  inz
008400070313     d  G02dat                 1      8  0 inz
008500070313     d  G02inv                 9     16  0 inz
008600070313     d  G02err                17     17    inz('3')
008700070313     d  G02tgi                18     22  0 inz
008800070315      *
008900070313      *
009000070313      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009100070313      *
009200070313      * - Campi di appoggio per il subfile
009300070313     d S01nrr          s                   like(V1Cnrr)   inz
009400070313      *
009500070313      * - Campi di salvataggio temporaneo dati
009600070313      *
009700070313      * - Flags booleani
009800070313     d $Fine           s              1    inz(*off)
009900070313     d $Sped           s              1    inz(*off)
010000070313     d $InzS01         s              1    inz(*on)
010100070314     d $almenouna      s              1    inz(*off)
010200070315     d $abilitato      s              1    inz(*off)
010300070319     d $immissione     s              1    inz(*off)
010400131219     d wSItel          s               n   inz(*off)
010500131122     d wSIpda          s               n   inz(*off)
010600131122     d wNOpda          s               n   inz(*off)
010700131122     d wTESTpda        s               n   inz(*off)
010800131127     d wEoF            s               n   inz(*off)
010900131127     d wF03            s               n   inz(*off)
011000131129     d wOkArn          s               n   inz(*off)
011100161122     d OkBollaArb      s               n   inz(*off)
011200161122     d OkBollaBlp      s               n   inz(*off)
011300070313      *
011400070313      * - Indici di schiera / Contatori
011500070313     d xx              s              3  0 inz
011600070313      *
011700070313      * - Campi di comodo
011800070313     d w0140           s             14  0
011900070314     d wData           s              8  0
012000070314     d DateU           s              8  0
012100070314     d wora            s              6  0
012200131120     d w003a           s              3
012300070308     d Keyaas          s                   like(arbaas)
012400070308     d Keylnp          s                   like(arblnp)
012500070308     d Keynrs          s                   like(arbnrs)
012600070308     d Keynsp          s                   like(arbnsp)
012700070309     d codut           s                   like(tblkut) inz(1)
012800070309     d Kcod            s                   like(tblcod)
012900070309     d Kkey            s                   like(tblkey)
013000070312     d Ktrc            s                   like(ar4trc)
013100070319     d W_gst           s                   like(arngst)
013200070319     d W_cdn           s                   like(arncdn) inz('LDV')
013300070314     d W_dim           s                   like(arndim)
013400070314     d W_arnpru        s                   like(arnpru)
013500070314     d W_arndim        s                   like(arndim)
013600070314     d W_arnoim        s                   like(arnoim)
013700070314     d W_arnnob        s                   like(arnnob)
013800070314     d W_ultimopno     s                   like(arnpno)
013900131120     d wabi            s                   like(UteAut)
014000131121     d woggetto        s                   like(CAUogg)
014100131122     d wfgs_ifp        s                   like(arblnp)
014200131122     d wnpg            s                   like(DSTnpg) inz(4)
014300140204     d wtcr            s                   like(ARPtcr)
014400140204     d wdcr            s                   like(ARBdcr)
014500140303     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
014600131122
014700131122       //--------------------------------------------------------------
014800131122       //?Definizione prototipi e procedure.
014900131122       //--------------------------------------------------------------
015000131122
015100131122      /copy gaitrasrc/srcprotopr,tibs02r
015200140121      /copy gaitrasrc/srcprotopr,tntbcmdr1
015300131122
015400131122     d fidg42r         pr                  extpgm('FIDG42R')
015500131122     d  kpjba                              likeds(kpjba)
015600131128
015700131128     d fnlrx3r         pr                  extpgm('FNLRX3R')
015800131128     d  kpjba                              likeds(kpjba)
015900131128     d  fnlrx3ds                           likeds(fnlrx3ds)
016000161122
016100140304       //--------------------------------------------------------------
016200140304
016300140304     d keyFIAPD01    e ds                  extname( FIAPD01L : *key )
016400140304     d                                     prefix(k_)   inz
016500070313      *
016600070313      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
016700070313      *
016800070319      * - file FIARN11L/12L
016900070319     c     K06ARN11      klist
017000070313     c                   kfld                    Vtcaas
017100070313     c                   kfld                    Vtclnp
017200070313     c                   kfld                    Vtcnrs
017300070313     c                   kfld                    Vtcnsp
017400070319     c                   kfld                    w_cdn
017500070319     c                   kfld                    w_gst
017600070319      * - file FNARB01L
017700070319     c     K04ARB01      klist
017800070309     c                   kfld                    vtcaas
017900070309     c                   kfld                    vtclnp
018000070309     c                   kfld                    vtcnrs
018100070309     c                   kfld                    vtcnsp
018200070312      * - file FIAR401L
018300070312     c     K05AR401      klist
018400070312     c                   kfld                    vtcaas
018500070312     c                   kfld                    vtclnp
018600070312     c                   kfld                    vtcnrs
018700070312     c                   kfld                    vtcnsp
018800070312     c                   kfld                    ktrc
018900070309      * - file TABEL00F
019000070309     c     K03TABEL      klist
019100070309     c                   kfld                    Codut
019200070309     c                   kfld                    Kcod
019300070309     c                   kfld                    Kkey
019400070313
019500070313      *===============================================================*
019600070313      *  RIEPILOGO INDICATORI                                         *
019700070313      *---------------------------------------------------------------*
019800070313      *  20    - SFLDSP    in C01                                     *
019900070313      * N21    - SFLCLR    in C01                                     *
020000070313      *  21    - SFLDSPCTL in C01                                     *
020100070313      *  23    - SFLEND    in C01                                     *
020200070313      *  28    - Errore generico dspf                                 *
020300070314      *  39    - sfl reverse immage                                   *
020400131121      *  40/43 - Condiziona visualizzazione campi                     *
020500070313      *  50/   - Errore posizionamento a video                        *
020600140204      *  51/   - Errore posizionamento a video                        *
020700070316      *  71    - utente che appartiene ad un P.O. abilitato a geo     *
020800070316      *  72    - Programma richiamato in interrogazione               *
020900070316      *  73    - Abilito F11 storico                                  *
021000070322      *  74    - Disabilito F10 Inserimento                           *
021100070313      *  90    - Errore                                               *
021200070313      *===============================================================*
021300070313
021400070315      *
021500070315     c                   exsr      sr_imposta
021600131122      /free
021700131122         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021800131122      /end-free
021900070319
022000070319      *-----------------------------------------------------------------------------
022100131121      *            FLUSSO PRINCIPALE
022200070319      *-----------------------------------------------------------------------------
022300070319
022400070313      * controllo la bolla
022500070313     c                   exsr      sr_ctrarb
022600161122      /free
022700161122       //?Prima di dare errore per spedizione non trovata provo a vedere
022800161122       //?se la bolla non è ancora partita
022900161122        IF  not OkBollaARB;
023000161122          exsr ContrBlp;
023100161122        ENDIF;
023200161122      /end-free
023300070314
023400070313      * spedizione non trovata errore e fine
023500070314
023600161122     c                   If        $sped = *off and not OkBollaBlp
023700131121     c                   movel     MSG(6)        On6msg
023800131121     c                   eval      on6err = 'E'
023900131127     c                   goto      fine
024000070319     c                   endif
024100070319
024200070313      * recupero dati dal file
024300161122     c                   IF        not OkBollaBlp
024400070313     c                   exsr      SR_FNARB
024500161122     c                   ENDIF
024600131122      /free
024700161122       //?recupero i dati dal BLP
024800161122       IF  OkBollaBlp;
024900161122         exsr CarDaBlp;
025000161122       ENDIF;
025100161122       IF  not OkBollaBlp;
025200131122       //?recupero comando dal file
025300131122       exsr sr_FICAU;
025400140204       //?Il flag di comunicazione AUT lo propongo sempre = 'SI'
025500140204       V1Caut = 'SI';
025600140204       //?se non c'è il comando imposto nella descrizione 'variazione'
025700140204       IF  V1Ccmd = *blanks;
025800140204         V1Dcmd = 'VARIAZIONE';
025900140204       ENDIF;
026000140204       //?Cerco se la spedizione in consegna ha la data consegna richiesta
026100140204       IF  IN6cons = 'S';
026200140204         clear wtcr;
026300140204         clear wdcr;
026400140204         exsr sr_DCR;
026500140204         //?Se c'è la data consegna richiesta e non è per oggi
026600140204         //?e non c'è la comunicazione all'AUT
026700140204         IF  V1Ccmd = *blanks and
026800140204            (wtcr = 'D' and wdcr >= dateu or
026900140204             wtcr = *blanks and wdcr > dateu);
027000140204         //?Non devo comunicare niente
027100140204           V1Caut = 'NO';
027200140204         ENDIF;
027300140204       ENDIF;
027400161122       ENDIF;
027500131122      /end-free
027600131121
027700070308      * gestione delle routine in caso di pgm richiamato per la sola immissione e nel caso
027800070308      * di visualizzazione e manutenzione
027900070308     c                   select
028000070308
028100131121     c                   when      in6mod = 'V'
028200070319      * carico il subfile
028300070319     c                   exsr      sr_CARSBF
028400070319
028500070316     c                   exsr      sr_VISUAL
028600131121     c                   goto      fine
028700070308
028800070308     c                   other
028900070316
029000070316     c                   exsr      sr_IMMISS
029100070319
029200070315      * se viene dato f3 di fine non proseguo
029300070319
029400070319     c                   If        $fine = *on
029500131127     c                   goto      fine
029600070319     c                   endif
029700070308
029800070308     c                   endsl
029900070319
030000070313     c     Fine          tag
030100131127
030200070313      *
030300070313     c                   eval      *inLR   = *on
030400070313
030500070313      *---------------------------------------------------------------*
030600070313      * Routine Iniziale                                              *
030700070313      *---------------------------------------------------------------*
030800070313     c     *InzSR        BegSR
030900070313      *
031000070313      * Reperisco dati job
031100070313     c                   exsr      DatiJob
031200070313     c                   movel     SDSpgm        VTCpgm
031300131120
031400131120      * Imposto le abilitazioni dell'utente
031500131120     c                   eval      dute01 = UTEfaf
031600131120
031700070314      * Reperisco data corrente
031800070314     c                   time                    w0140
031900070314     c                   move      w0140         wData
032000070314     c                   clear                   WLBdat
032100070314     c                   eval      G02dat = wData
032200070314     c                   call      'XSRDA8'
032300070314     c                   parm                    WLBdat
032400070314     c                   z-add     G02inv        DateU
032500070313      *
032600070313     c                   EndSr
032700070315      *---------------------------------------------------------------*
032800070315      * Impostazioni Iniziali                                         *
032900070315      *---------------------------------------------------------------*
033000070315     c     SR_imposta    BegSR
033100070315      *
033200070315     c     *Entry        plist
033300131129     c                   parm                    KPJBA
033400131121     c                   parm                    FNLRN6DS
033500070315
033600070315      * carico la £6
033700070315     c                   exsr      sr_carl6
033800070315
033900070315      * visualizzazione
034000131121     c                   if        in6mod = 'V'
034100070316     c                   eval      *in72 = *on
034200070315     c                   endif
034300131121
034400070319      * imposto i campi della chiave della spedizione
034500131121     c                   eval      vtcaas = in6aas
034600131121     c                   eval      vtclnp = in6lnp
034700131121     c                   eval      vtcnrs = in6nrs
034800131121     c                   eval      vtcnsp = in6nsp
034900131122
035000140206       //?Verifico la distinta a PDA
035100140207     c                   IF        IN6cons = 'S'
035200140206     C                   exsr      CTR_dist
035300140207     c                   ENDIF
035400131219      * visualizzo la videata dei comandi se bolla in consegna , filiale distinta partita con le
035500131219      * telefnate ad aut e se non sono stato richiamato dalla chiusura distinta
035600131219     c                   eval      *in41 = IN6cons = 'S' and IN6chdist = ' ' and
035700140226     c                             wSITel = *on
035800070315
035900070315     c                   EndSr
036000070315
036100070313      *---------------------------------------------------------------*
036200070313      * Reperimento Dati del job (Utente/Operativi)                   *
036300070313      *---------------------------------------------------------------*
036400070313     c     DatiJob       BegSr
036500070313      *
036600070313     c     *dtaara       define    §azute        azuteds
036700070313     c     *dtaara       define    §datiute      ddatiute
036800070313      *
036900070313     c                   in(E)     *dtaara
037000070313     c                   IF        %ERROR or RSUT = *blanks
037100070313     c                   clear                   Tibs34Ds
037200070313     c                   call      'TIBS34R'
037300070313     c                   parm                    Tibs34Ds
037400070313     c                   in        *dtaara
037500070313     c                   ENDIF
037600070313      *
037700070313     c                   EndSr
037800070313
037900070313      *---------------------------------------------------------------*
038000070313      * Recupero i P.O.  della £6                                     *
038100070313      *---------------------------------------------------------------*
038200070313     c     Sr_CARL6      begsr
038300070313
038400070313      * Recupero i PO della £6
038500070313      *
038600070313     c                   clear                   trul06ds
038700070313     c                   move      '£6'          d06cod
038800070313     c                   movel     Dutpou        d06key
038900070313     c                   movel(p)  trul06ds      KPJBU
039000070313
039100070313     c                   call      'TRUL06R'
039200070313     c                   parm                    kpjba
039300070313     c                   movel     kpjbu         trul06ds
039400070313     c                   movea     lin           L6
039500070313
039600070315      * verico se PO abilitato alla gestione telefonate GEO
039700070315
039800070315     c                   clear                   og148
039900070315     c     l6(1)         chain     azorg01l
040000070315     c                   if        %found(azorg01l)
040100070315     c                   movel     orgde8        og148
040200070315     c                   endif
040300070315
040400070315     c                   If        §oggeot = 'S'
040500070315     c                   eval      $abilitato = *on
040600070316     c                   eval      *in71      = *on
040700070315     c                   endif
040800131120
040900131120      /free
041000131120       //?Imposto abilitazione utente
041100131120         clear wpog;
041200131120         SELECT;
041300131120       //?Imposto abilitazione manutenzione bolle di filiale
041400131120           WHEN  §UTEmbf <> *blanks;
041500131120             wabi = §UTEmbf;
041600131120           WHEN  UTEaut <> *blanks;
041700131120             wabi = UTEaut;
041800131120           WHEN  DUTlpo = '1';
041900131120             wabi = 'TP';
042000131120           WHEN  DUTlpo = '2';
042100131120             wabi = 'PO';
042200131120         ENDSL;
042300131120      /end-free
042400131120
042500131120      * controllo se ok l'abilitazione dell'utente
042600131120     c                   clear                   TIBS02DS
042700131120     c                   clear                   dLAT
042800131120     c                   eval      T02mod = 'C'
042900131120     c                   eval      T02sif = knsif
043000131120     c                   eval      T02cod = 'LAT'
043100131120     c                   movel(p)  wabi          T02ke1
043200131120     c                   call      'TIBS02R'
043300131120     c                   parm                    kpjba
043400131120     c                   parm                    TIBS02DS
043500131120if  1c                   IF        T02err = *blanks
043600131120     c                   eval      dLAT = T02uni
043700131120e   1c                   ENDIF
043800131120
043900131120      * Se abilitazione trovata e OK
044000131120if  1c                   IF        T02err = *blanks and §LATabi = *blanks
044100131120      * Reperimento dei P.O. gestibili dall'utente
044200131120     c                   clear                   TRUL31DS
044300131120     c                   eval      I31abi = wabi
044400131120     c                   eval      I31cdi = DUTdis
044500131120     c                   eval      I31car = DUTare
044600131120     c                   eval      I31cpo = DUTpou
044700131120     c                   call      'TRUL31R'
044800131120     c                   parm                    kpjba
044900131120     c                   parm                    TRUL31DS
045000131120if  2c                   IF        O31pog > *zeros
045100131120     c                   movea     O31pog        wpog
045200131120x   2c                   ENDIF
045300131120    1c                   ENDIF
045400131120
045500131121      * se sk delle filiali abilitate all'utente è vuota, imposto il primo elemento della £6
045600131120     c                   IF        wpog(1) = *blanks
045700131120     c                   eval      wpog(1) = %editc(l6(1):'X')
045800131120     c                   ENDIF
045900131120
046000070313     c                   endsr
046100070313      *---------------------------------------------------------------*
046200070313      * sr_ctrarb  * controllo se spedizione corretta                 *
046300070313      *---------------------------------------------------------------*
046400070313     c     sr_ctrarb     BEGSR
046500070313
046600070313     c                   eval      $sped = *off
046700161122     c                   eval      OkBollaArb = *off
046800070313
046900070313     c     k04arb01      chain     Fnarb01l
047000070313
047100070412    1c                   If        %found(fnarb01l)
047200161122     c                   eval      OkBollaArb = *on
047300131122      * verifico se la linea di arrivo è presente nelle linee abilitate alla manut. bolle filiale
047400131120     c                   move      arblna        w003a
047500131122     c                   IF        %lookup (w003a:wPOG) > 0
047600070313     c                   eval      $sped = *on
047700070412    2c                   endif
047800161124      * se non abilitata come LNA provo con LNP
047900161124     c                   move      arblnp        w003a
048000161124     c                   IF        %lookup (w003a:wPOG) > 0
048100161124     c                   eval      $sped = *on
048200161124    2c                   endif
048300131122      * se non abilitata
048400161124     c                   IF        $sped = *off
048500070412      * disabilito il comando di scrittura note
048600131121    4c                   if        in6mod = 'V'
048700070322     c                   eval      $sped = *on
048800131122      * e disattivo f10 di inserimento
048900070322     c                   eval      *in74 = *on
049000070412    4c                   endif
049100070412    2c                   endif
049200070412
049300070412    1c                   endif
049400070313
049500070313     c                   endsr
049600161122      /free
049700161122       // ----------------------------------------------------------------------
049800161122       //?Controllo se la bolla è in BLP e se non è partita
049900161122       // ----------------------------------------------------------------------
050000161122       BEGSR ContrBlp;
050100161122
050200161122         OkBollaBlp = *off;
050300161122
050400161122         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FNBLP01L;
050500161122         IF  %found(FNBLP01L);
050600161122           w003a = %editc(BLPlnp:'X');
050700161122           IF  %lookup(w003a:wPOG) > 0;
050800161122             OkBollaBlp = *on;
050900161122           ENDIF;
051000161124         //?Se utente non abilitato come LNP provo con LNA
051100161124           w003a = %editc(BLPlna:'X');
051200161124           IF  %lookup(w003a:wPOG) > 0;
051300161124             OkBollaBlp = *on;
051400161124           ENDIF;
051500161123         //?Se utente non abilitato alla bolle vado solo in visualizzazione
051600161123         //?e disattivo il tasto F10 di immissione note
051700161124           IF  not OkBollaBlp and in6mod = 'V';
051800161123             OkBollaBlp = *on;
051900161123             *in74 = *on;
052000161123           ENDIF;
052100161122         ENDIF;
052200161122
052300161122       ENDSR;
052400161122      /end-free
052500070313
052600070313      *---------------------------------------------------------------*
052700070313      * sr_clr_W_not   * Pulizia videata richiesta note               *
052800070313      *---------------------------------------------------------------*
052900070313     c     sr_clr_W_not  BEGSR
053000070313
053100131121     c                   clear                   vtcns1
053200131121     c                   clear                   vtcns2
053300131121     c                   clear                   vtccmd
053400131121     c                   clear                   vtcdtacmd
053500131121     c                   clear                   vtcoracmd
053600131121     c                   clear                   vtcutecmd
053700131121     c                   clear                   v1ccmd
053800131121     c                   clear                   v1dcmd
053900070313     c                   clear                   v1cno1
054000070313     c                   clear                   v1cno2
054100070313     c                   clear                   v1cno3
054200070313     c                   clear                   v1cno4
054300070316     c                   clear                   v1hno1
054400070316     c                   clear                   v1hno2
054500070316     c                   clear                   v1hno3
054600070316     c                   clear                   v1hno4
054700070316     c                   clear                   vtcdim
054800070313     c                   clear                   vtcoim
054900070313     c                   clear                   vtcpru
055000131121     c                   clear                   v1caut
055100070313     c                   eval      *in40 = *off
055200070313     c                   eval      *in42 = *off
055300131121     c                   eval      *in43 = *off
055400070313
055500070313     c                   endsr
055600070313
055700070309      *---------------------------------------------------------------*
055800070309      * sr_FNARB   * Recupero informazioni da bolle arrivi            *
055900070309      *---------------------------------------------------------------*
056000070309     c     sr_FNARB      BEGSR
056100070309
056200070313      * routine di pulizia del video
056300070313
056400070313     c                   exsr      SR_clr_W_not
056500070313
056600070313      * data spedizione
056700070313     c                   clear                   wlbdat
056800070313     c                   movel(p)  arbaas        g02inv
056900070313     c                   move      arbmgs        g02inv
057000070314     c                   eval      G02err = '3'
057100070313     c                   call      'XSRDA8'
057200070313     c                   parm                    WLBdat
057300070313     c                   z-add     G02dat        vtcdta
057400070312      * note spedizione
057500070312
057600070313     c                   eval      ktrc = '8'
057700070313     c     K05ar401      chain     fiar401l
057800070312     c                   if        %found(fiar401l)
057900070312     c                   eval      vtcns1 = Ar4not
058000070312     c                   endif
058100070312
058200070313     c                   eval      ktrc = '9'
058300070313     c     K05ar401      chain     fiar401l
058400070312     c                   if        %found(fiar401l)
058500070312     c                   eval      vtcns2 = Ar4not
058600070312     c                   endif
058700070313      *
058800070313      * Verifico se sono in manutenzione e recupero le ultime note stampabili
058900070309      *
059000070319     c                   eval      W_gst = 'S'
059100070319     c     K06arn11      setll     Fiarn11l
059200070313
059300070313     c                   do        *hival
059400070313
059500070319     c     k06arn11      reade(n)  Fiarn11l
059600070314
059700070319     c                   if        %eof(fiarn11l)
059800070314     c                   leave
059900070314     c                   endif
060000070313
060100070313      * imposto i dati dell'ultimo utente che ha inserito la/e nota/e stampabile/i
060200070313
060300070313     c                   if        not *in42
060400070316      * valorizzo indicatore per gestione f11 storico
060500070316     c                   eval      *in73 = *on
060600070313
060700070313     c                   eval      *in42 = *on
060800070313     c                   eval      vtcpru = arnpru
060900070313     c                   eval      vtcoim = arnoim
061000070313
061100070313     c                   clear                   wlbdat
061200070313     c                   eval      G02inv = arndim
061300070314     c                   eval      G02err = '3'
061400070313     c                   call      'XSRDA8'
061500070313     c                   parm                    WLBdat
061600070313     c                   z-add     G02dat        vtcdim
061700070313
061800070313     c                   endif
061900070313
062000070313     c                   select
062100070313
062200070313     c                   when      v1cno1 = *blanks
062300070313     c                   eval      v1cno1 = arnnob
062400070313
062500070313     c                   when      v1cno2 = *blanks
062600070313     c                   eval      v1cno2 = arnnob
062700070313
062800070313     c                   when      v1cno3 = *blanks
062900070313     c                   eval      v1cno3 = arnnob
063000070313
063100070313     c                   other
063200070313     c                   eval      v1cno4 = arnnob
063300070313
063400070313     c                   endsl
063500070313
063600070313     c                   enddo
063700070313      * verifico se mi hanno passato  degli appunti
063800070313
063900131121     c                   if        in6not1 <> *blanks
064000131121     c                   eval      vtcno1 = in6not1
064100070313     c                   eval      *in43 = *on
064200070313     c                   endif
064300070313
064400131121     c                   eval      vtcno2 = in6not2
064500131121     c                   eval      vtcno3 = in6not3
064600131121     c                   eval      vtcno4 = in6not4
064700070313
064800070313     c                   endsr
064900131121
065000161122       // ----------------------------------------------------------------------
065100161122       //?Controllo se la bolla è in BLP e se non è partita
065200161122       // ----------------------------------------------------------------------
065300161122     c     CarDaBlp      BEGSR
065400161122
065500161122      * routine di pulizia del video
065600161122
065700161122     c                   exsr      SR_clr_W_not
065800161122
065900161122      * data spedizione
066000161122     c                   clear                   wlbdat
066100161122     c                   movel(p)  BLPaas        g02inv
066200161122     c                   move      BLPmgs        g02inv
066300161122     c                   eval      G02err = '3'
066400161122     c                   call      'XSRDA8'
066500161122     c                   parm                    WLBdat
066600161122     c                   z-add     G02dat        vtcdta
066700161122
066800161122      * note spedizione
066900161122     c                   eval      ktrc = '8'
067000161122     c     K05ar401      chain     fiar401l
067100161122     c                   if        %found(fiar401l)
067200161122     c                   eval      vtcns1 = Ar4not
067300161122     c                   endif
067400161122
067500161122     c                   eval      ktrc = '9'
067600161122     c     K05ar401      chain     fiar401l
067700161122     c                   if        %found(fiar401l)
067800161122     c                   eval      vtcns2 = Ar4not
067900161122     c                   endif
068000161122
068100161122      * Verifico se sono in manutenzione e recupero le ultime note stampabili
068200161122     c                   eval      W_gst = 'S'
068300161122     c     K06arn11      setll     Fiarn11l
068400161122     c                   do        *hival
068500161122     c     k06arn11      reade(n)  Fiarn11l
068600161122     c                   if        %eof(fiarn11l)
068700161122     c                   leave
068800161122     c                   endif
068900161122      * imposto i dati dell'ultimo utente che ha inserito la/e nota/e stampabile/i
069000161122     c                   if        not *in42
069100161122      * valorizzo indicatore per gestione f11 storico
069200161122     c                   eval      *in73 = *on
069300161122     c                   eval      *in42 = *on
069400161122     c                   eval      vtcpru = arnpru
069500161122     c                   eval      vtcoim = arnoim
069600161122     c                   clear                   wlbdat
069700161122     c                   eval      G02inv = arndim
069800161122     c                   eval      G02err = '3'
069900161122     c                   call      'XSRDA8'
070000161122     c                   parm                    WLBdat
070100161122     c                   z-add     G02dat        vtcdim
070200161122     c                   endif
070300161122     c                   select
070400161122     c                   when      v1cno1 = *blanks
070500161122     c                   eval      v1cno1 = arnnob
070600161122     c                   when      v1cno2 = *blanks
070700161122     c                   eval      v1cno2 = arnnob
070800161122     c                   when      v1cno3 = *blanks
070900161122     c                   eval      v1cno3 = arnnob
071000161122     c                   other
071100161122     c                   eval      v1cno4 = arnnob
071200161122     c                   endsl
071300161122     c                   enddo
071400161122
071500161122      * verifico se mi hanno passato  degli appunti
071600161122     c                   if        in6not1 <> *blanks
071700161122     c                   eval      vtcno1 = in6not1
071800161122     c                   eval      *in43 = *on
071900161122     c                   endif
072000161122     c                   eval      vtcno2 = in6not2
072100161122     c                   eval      vtcno3 = in6not3
072200161122     c                   eval      vtcno4 = in6not4
072300161122
072400161122     c                   endsr
072500161122
072600161124      /free
072700131122       // ----------------------------------------------------------------------
072800131122       // Recupero il comando dal file delle telefonate AUT
072900131122       // ----------------------------------------------------------------------
073000131122       BEGSR  sr_FICAU;
073100131122
073200131122         woggetto = %editc(ARBlnp:'X') +
073300131122                    %editc(ARBnrs:'X') +
073400131122                    %editc(ARBnsp:'X') +
073500131122                    %editc(ARBaas:'X');
073600131122
073700131122       //?Preparo il cursore
073800131122         exec sql
073900131122         DECLARE CAU cursor FOR
074000131122         SELECT * from FICAU00F
074100131122         WHERE CAUtor = 'C' and CAUogg = :woggetto
074200131122           and CAUfgs = :IN6fgs and CAUdfv = :IN6dfv
074300131129           and CAUnfv = :IN6nfv and CAUpdr = :IN6pdr
074400131122           and CAUatb = ' '
074500131122         ORDER by  CAUdtoins;
074600131121
074700131122       //?Apro il cursore
074800131122         exec sql
074900131122         OPEN  CAU;
075000131129
075100131129         IF sqlcode < 0;
075200131129           wEoF = *on;
075300131129         ENDIF;
075400131122
075500131122       //?leggo il file
075600131127         DOW  not wEoF;
075700131122          exec sql
075800131122          FETCH next from CAU into :FICAUDS;
075900131122
076000131122       //?fine file o errore sql esco
076100131122           IF   sqlcod = 100 or sqlcod < 0;
076200131127            wEoF = *on;
076300131122            leave;
076400131122           ENDIF;
076500131122
076600131122       //?comando
076700131122           IF  CAUdescmd <> *blanks;
076800131210             V1Ccmd = CAUcmd;
076900131211             V1Dcmd = CAUdescmd;
077000131122             VTCcmd = CAUdescmd;
077100131122             *in40 = *on;
077200140303             wdate_eur = %date(%dec(%subst(CAUdtoins:1:8):8:0));
077300140303             vtcdtacmd = %dec( Wdate_eur );
077400131122             VTCoracmd = %dec(%subst(CAUdtoins:9:6):6:0);
077500131122             VTCutecmd = CAUpruins;
077600131122           ENDIF;
077700131127           wEoF = *on;
077800131122           leave;
077900131122         ENDDO;
078000131121
078100131122       //?Chiudo il cursoe
078200131122         exec sql
078300131122         close CAU;
078400131122
078500131122       ENDSR;
078600140204
078700140204       // ----------------------------------------------------------------------
078800140204       // Recupero la data consegna richiesta.
078900140204       // ----------------------------------------------------------------------
079000140204       BEGSR  sr_DCR;
079100140204
079200140204       //?Cerco se la data è su FIARP
079300140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FIARP01L;
079400140204         IF  %found(FIARP01L);
079500140204           wtcr = ARPtcr;
079600140204           wdcr = ARPdcr;
079700140204           leavesr;
079800140204         ENDIF;
079900140204
080000140204       //?Non la trovo su ARP la cerco su FNARB
080100140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FNARB01L;
080200140204         IF  %found(FNARB01L);
080300140204           wtcr = ARbtcr;
080400140204           wdcr = ARbdcr;
080500140204           leavesr;
080600140204         ENDIF;
080700140204
080800140204       ENDSR;
080900140204
081000131122      /end-free
081100070313
081200070313      *---------------------------------------------------------------*
081300070313      * sr_IMMISS  * Gestione della videata di immissione             *
081400070313      *---------------------------------------------------------------*
081500070313     c     sr_IMMISS     BEGSR
081600070313
081700070319     c                   eval      $immissione = *on
081800070418      * mi salvo le righe di note per non scrivere delle note doppie
081900070418     c                   eval      v1hno1 = v1cno1
082000070418     c                   eval      v1hno2 = v1cno2
082100070418     c                   eval      v1hno3 = v1cno3
082200070418     c                   eval      v1hno4 = v1cno4
082300070313      *
082400070313     c                   do        *hival
082500070319     c                   eval      $fine = *off
082600131127      /free
082700131127       wF03 = *off;
082800131127      /end-free
082900070315
083000131121     c                   write     lrn6t00
083100131121     c                   write     lrn6z01
083200070313
083300131202     c                   write     lrn6d01
083400131202     c                   read(e)   fnlrn6d
083500070315     c                   eval      *in28 = *off
083600131202     c                   IF        %error
083700131202     c                   eval      *inkc = *on
083800131202     c                   ENDIF
083900131122
084000070313
084100070313      * f3 - Fine lavoro
084200070313     c                   if        *inkc
084300070315     c                   eval      $fine = *on
084400131127      /free
084500131127       wF03 = *on;
084600131127      /end-free
084700070313     c                   leave
084800070313     c                   endif
084900070315
085000070404      * f2 - Copia note R.A.
085100070404     c                   if        *inkb
085200070404     c                   exsr      SR_copia
085300070404     c                   iter
085400070404     c                   endif
085500070404
085600070316      * F11 gestione dello storico
085700070403     c                   if        *inkk
085800070403     c* Se provengo da videata di visulizzazione, esco
085900070319      * carico il subfile
086000131121     c                   if        in6mod<>'V'
086100070319     c                   exsr      sr_CARSBF
086200070319
086300070316     c                   exsr      SR_VISUAL
086400070319     c                   iter
086500070403     c                   else
086600070403     c                   leave
086700070403     c                   endif
086800070316     c                   endif
086900070316
087000070315     c                   exsr      SR_contr
087100070315
087200070315      * ci sono degli errori
087300140124     c                   if        *in28 or *in90
087400070315     c                   iter
087500070315     c                   endif
087600131122
087700070314      * F06 conferma
087800070314     c                   if        *inkf
087900070314     c                   exsr      SR_confe
088000131121     c                   if        *in28
088100070315     c                   iter
088200070315     c                   else
088300131121      * se immissione o manutenzione vado a fine pgm
088400131121     c                   if        in6mod = 'I' or in6mod = 'M'
088500070319     c                   eval      $fine = *on
088600070319     c                   endif
088700070314     c                   leave
088800070315     c                   endif
088900070314     c                   endif
089000070315
089100070313     c                   enddo
089200070319     c                   eval      $immissione = *off
089300070313     c                   endsr
089400070315      *---------------------------------------------------------------*
089500070315      * sr_Contr * Controllo i dati inseriti nella videata immissione *
089600070315      *---------------------------------------------------------------*
089700070315     c     sr_contr      BEGSR
089800070315
089900140124     c                   eval      *in90 = *off
090000140204     c                   eval      *in50 = *off
090100140204     c                   eval      *in51 = *off
090200140204
090300131122      /free
090400140204         clear  V1Dcmd;
090500131122       //?Se immesso un comando a video
090600131122         IF  V1Ccmd <> *blanks;
090700131210       //?verifico il ? di ricerca
090800131210           If    %scan('?' : V1Ccmd) > 0  ;
090900140124             *in90 = *on;
091000140121             *in50 = *on;
091100131210             clear dCMD;
091200140121             clear TNTBCMDDS1;
091300140121             ICMDtip = 'C';
091400140121             clear kpjbu;
091500140121             kpjbu = tntbcmdds1;
091600140121             TNTBCMDR1  (kpjba);
091700140121             tntbcmdds1 =  kpjbu ;
091800140121             IF Ocmderr = *off;
091900140121                 V1Ccmd = OCMDcmd ;
092000140121                 V1Dcmd  = OCMDdes;
092100140124                 clear V1Cmsg ;
092200131210             else ;
092300131210                  V1Cmsg = msg(01);
092400131210                  *in28 = *on;
092500131210                  *in50 = *on;
092600131210                  leavesr;
092700131210             endif;
092800131210             leavesr;
092900131210           endif;
093000131210
093100131122       //?deve esistere in tabella CMD
093200131128           clear dCMD;
093300131122           clear TIBS02DS;
093400131122           T02mod = 'C';
093500131122           T02sif = knsif;
093600131122           T02cod = 'CMD';
093700131122           T02ke1 = V1Ccmd;
093800131122           TNTBE_RicercaControllo (kpjba : tibs02ds);
093900131128           IF  T02err <> *blanks;
094000131122             *in28 = *on;
094100131122             *in50 = *on;
094200131122             V1Cmsg = msg(01);
094300131122             leavesr;
094400131122           ENDIF;
094500131128
094600131128           dCMD = T02uni;
094700140124           V1Dcmd = §CMDdes;
094800140124       //?se il comando è utilizzabile solo per i ritiri ERRORE
094900140124          If §cmdtip = 'R' ;
095000140124             *in28 = *on;
095100140124             *in50 = *on;
095200140124             V1Cmsg = msg(02);
095300140124             leavesr;
095400140124           ENDIF;
095500131210       //?se il comando è diverso da quello precedente imposto SI nel comunicare i dati all'AUT
095600131210          IF V1Ccmd <> CAUcmd ;
095700131210             V1Caut = 'SI' ;
095800131210          ENDIF;
095900140204       //?se c'è la comunicazione devo avvisare sempre l'AUT
096000140204          IF  V1Caut <> 'SI';
096100140204             *in28 = *on;
096200140204             *in51 = *on;
096300140204             V1Cmsg = msg(04);
096400140204             leavesr;
096500140204          ENDIF;
096600140204
096700140204         ELSE;
096800140204
096900140204       //?se c'era il comando non posso toglierlo
097000140204          IF  CAUcmd <> *blanks;
097100140204             *in28 = *on;
097200140204             *in50 = *on;
097300140204             V1Cmsg = msg(03);
097400140204             leavesr;
097500140204          ENDIF;
097600140204       //?se non c'è il comando imposto nella descrizione 'variazione'
097700140204           V1Dcmd = 'VARIAZIONE';
097800131210
097900131122         ENDIF;
098000131122      /end-free
098100070315
098200131122     c                   endsr
098300070315
098400070313      *---------------------------------------------------------------*
098500070313      * sr_VISUAL  * Gestione della videata di visualizzazione        *
098600070313      *---------------------------------------------------------------*
098700070313     c     sr_visual     BEGSR
098800070313
098900070319    1c                   do        *hival
099000070319
099100131121     c                   write     lrn6t00
099200131121     c                   write     lrn6z02
099300131121     c                   exfmt     lrn6c02
099400070319
099500070319      * F12 ritorno
099600070319    2c                   if        *inkl = *on
099700070319     c                   leave
099800070319    2c                   endif
099900070319
100000070319      * f10 Immissione
100100070319    2c                   if        *inkj = *on
100200070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
100300070319      * comando 12 videata precedente
100400070319    3c                   if        $immissione = *off
100500070319     c                   exsr      SR_IMMISS
100600070319      * provengo dalla routine di immissione
100700070319   x3c                   else
100800070319     c                   leave
100900070319    3c                   endif
101000070319      * se viene dato f3 di fine lavoro nella videtata d immissione vado fuori
101100070319    3c                   if        $fine = *on
101200070319     c                   leave
101300070319   x3c                   else
101400070319      * ricarico il subfile e visualizzo di nuovo i dati
101500070319     c                   exsr      sr_carsbf
101600070319    3c                   endif
101700070319
101800070319    2c                   endif
101900070319     c
102000070319    1c                   enddo
102100070314
102200070313     c                   endsr
102300070314      *---------------------------------------------------------------*
102400070314      * sr_CARSBF  * Caricamento sub file storico                     *
102500070314      *---------------------------------------------------------------*
102600070314     c     sr_CARSBF     BEGSR
102700070314
102800070314      * Pulizia subfile
102900070314     c                   setoff                                       20  21
103000131121     c                   write     LRN6C02
103100070314     c                   seton                                        20  21
103200070314      * pulizia campi di comodo
103300070314     c                   clear                   W_arnpru
103400070314     c                   clear                   W_arndim
103500070314     c                   clear                   W_arnoim
103600070314
103700070314     c                   eval      *in39 = *off
103800070315
103900070315     c                   clear                   s01nrr
104000070315
104100070315      * verifico se ho dello storico da visualizzare (note con 'S')
104200070315      * se ne ho visualizzo il subfile altrimenti vado direttamente
104300070315      * nella videata dell'inserimento note ldv
104400070315
104500070319     c                   eval      W_gst = 'S'
104600070319     c     K06arn11      setll     Fiarn12l
104700070315
104800070315     c                   do        *hival
104900070315
105000070319     c     k06arn11      reade     Fiarn12l
105100070315
105200070319     c                   if        %eof(fiarn12l)
105300070315     c                   leave
105400070315     c                   endif
105500070315
105600070315     c                   exsr      Sr_Wrtsbf
105700070315
105800070315     c                   enddo
105900070314
106000070315     c                   clear                   W_arnpru
106100070315     c                   clear                   W_arndim
106200070315     c                   clear                   W_arnoim
106300070315
106400070314      * verifico se ho dello storico da visualizzare (note senza 'S')
106500070314      * se ne ho visualizzo il subfile altrimenti vado direttamente
106600070314      * nella videata dell'inserimento note ldv
106700070314
106800070319     c                   eval      W_gst = ' '
106900070319     c     K06arn11      setll     Fiarn12l
107000070314
107100070314     c                   do        *hival
107200070314
107300070319     c     k06arn11      reade     Fiarn12l
107400070314
107500070319     c                   if        %eof(fiarn12l)
107600070314     c                   leave
107700070314     c                   endif
107800070314
107900070315     c                   exsr      Sr_Wrtsbf
108000070315
108100070314     c                   enddo
108200070319
108300070319      * verifico se ho scritto almeno una riga nel subfile eseguo gestione del subfile
108400070319     c                   If        s01nrr > 1
108500070319     c                   eval      *in20 = *on
108600070319     c                   z-add     1             v1cnrr
108700070319     c                   else
108800070319     c                   eval      *in20 = *off
108900070319     c                   endif
109000070319
109100070314
109200070314     c                   ENDSR
109300070315
109400070315      *---------------------------------------------------------------*
109500070315      * sr_WRTSBF  * Scrittura subfile                                *
109600070315      *---------------------------------------------------------------*
109700070315     c     sr_WRTSBF     BEGSR
109800070315
109900070315      * verifico se si tratta dello stesso utente data ora immissione
110000070315     c                   If        W_arnpru <> arnpru or
110100070315     c                             W_arndim <> arndim or
110200070315     c                             W_arnoim <> arnoim
110300070315
110400070315      * scrivo una riga subfile di interruzione con i dati dell'utente che ha inserito
110500070315      * le note successive
110600070315     c                   clear                   wlbdat
110700070315     c                   eval      G02inv = arndim
110800070315     c                   eval      G02err = '3'
110900070315     c                   call      'XSRDA8'
111000070315     c                   parm                    WLBdat
111100070315     c                   z-add     G02dat        W_dim
111200070315
111300070319     c                   If        arngst =  ' '
111400070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
111500070315     c                                       + %editw(W_dim:'  /  /    ') +
111600070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
111700070315     c                   else
111800070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
111900070315     c                                       + %editw(W_dim:'  /  /    ') +
112000070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
112100070315     c                                       + ' Stampabili '
112200070315     c                   endif
112300070315      * reverse immage
112400070315     c                   eval      *in39 = *on
112500070315
112600070315     c                   add       1             s01nrr
112700131121     c                   write     lrn6s02
112800070315     c                   eval      *in39 = *off
112900070315      * salvo i campi
113000070315     c                   eval      W_arnpru =  arnpru
113100070315     c                   eval      W_arndim =  arndim
113200070315     c                   eval      W_arnoim =  arnoim
113300070315
113400070315     c                   endif
113500070315
113600070315     c                   add       1             s01nrr
113700070315     c                   movel     arnnob        v2snbo
113800131121     c                   write     lrn6s02
113900070315
114000070315     c                   endsr
114100070314      *---------------------------------------------------------------*
114200070314      * sr_GESSBF  * Caricamento sub file storico                     *
114300070314      *---------------------------------------------------------------*
114400070314     c     sr_GESSBF     BEGSR
114500070314
114600070314     c                   z-add     1             v1cnrr
114700070314     c                   do        *hival
114800070319
114900070319     c                   eval      $fine = *off
115000070319
115100131121     c                   write     lrn6t00
115200131121     c                   write     lrn6z02
115300131121     c                   exfmt     lrn6c02
115400070314
115500070314      * F12 ritorno
115600070314     c                   if        *inkl = *on
115700070314     c                   leave
115800070314     c                   endif
115900070314
116000070314      * f10 Immissione
116100070314     c                   if        *inkj = *on
116200070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
116300070319      * comando 12 videata precedente
116400070319     c                   if        $immissione = *off
116500070314     c                   exsr      SR_IMMISS
116600070316      * se non devo visualizzare il sbf vado fuori
116700070316     c                   if        $fine = *on
116800070316     c                   leave
116900070316     c                   endif
117000070319      * provengo dalla routine di immissione
117100070319     c                   else
117200070319     c                   leave
117300070319     c                   endif
117400070316
117500070314     c                   endif
117600070314     c
117700070314     c                   enddo
117800070314     c                   ENDSR
117900131122
118000131122      /free
118100131122       // ----------------------------------------------------------------------
118200131122       // Controllo distinta
118300131122       // ----------------------------------------------------------------------
118400131122       BEGSR  ctr_Dist;
118500131122
118600140304         wSItel = *off;
118700131122         wSIpda = *off;
118800131122         wNOpda = *off;
118900131122         wTESTpda = *off;
119000131122
119100131122       //?Aggancio ARB
119200131122         chain (IN6aas:IN6lnp:IN6nrs:IN6nsp) FNARB01L;
119300131122         IF  not %found(FNARB01L);
119400131122           leavesr;
119500131122         ENDIF;
119600131219       //?Bolla non in distinta
119700160223         //IF ARBndc = 0 ;
119800160223         IF ARBndc = 0  or  ARBndc = *all'8'  or  ARBndc = *all'9';
119900131219           leavesr;
120000131219         ENDIF;
120100131122
120200131122       //?Aggancio il file delle distinte
120300131122         clear dDSTflr;
120400131122         wfgs_ifp = ARBifp;
120500131122         chain (wfgs_ifp:4:ARBddc:ARBndc) FIDST08L;
120600131122         IF  %found(FIDST08L);
120700131122           dDSTflr = DSTflr;
120800131122         ENDIF;
120900140304         // -?Impostazione campi chiave?
121000140304         clear  keyFIAPD01;
121100140304         k_APDPDR = DSTpdr;
121200140304         k_APDtip = 'A';
121300140304
121400140304         // -?Verifica se Autista escluso da fatturazione es "c/servizi" non crea FICAU
121500140304         chain  %kds( keyFIAPD01 )  FIAPD000;
121600140304         if  Not %found(FIAPD01L);
121700140304           leavesr;
121800140304         endif;
121900140304         if APDpdd = 'S'or APDatb <> ' ';
122000140304           leavesr;
122100140304         endif;
122200140304
122300140304         wSItel = *on ;
122400140304
122500131122       //?Verifico se a PDA se a PDA in TEST o NO PDA
122600131122         SELECT;
122700131122           WHEN  (DSTpda = 'C' or DSTpda = 'E') and
122800140122                 (§DSTtstpda = ' ' or §DSTtstpda = 'O');
122900131122             wSIpda = *on;
123000131122
123100131122           WHEN  (DSTpda = 'E' or DSTpda = 'C') and
123200131122                 (§dsttstpda = 'E' or §dsttstpda = 'C');
123300131122             wTESTpda = *on;
123400131122           WHEN  (DSTpda = 'O' or DSTpda = 'N') and
123500140122                 (§dsttstpda = 'O' or §dsttstpda = ' ');
123600131122             wNOpda = *on;
123700131122         ENDSL;
123800131122
123900131122       ENDSR;
124000131122      /end-free
124100131122
124200070314      *---------------------------------------------------------------*
124300070314      * sr_CONFE   * Conferma scrittura note                          *
124400070314      *---------------------------------------------------------------*
124500070314     c     sr_confe      BEGSR
124600070314
124700070316      * se è stato inserito almeno una riga nota diversa da prima scrivo sul file altrimenti passo
124800070316      * oltre
124900131129      /free
125000131129         wOkArn = *off;
125100131129      /end-free
125200070316
125300070316     c                   If        v1cno1 <> v1hno1 or  v1cno2 <> v1hno2  or
125400070319     c                             v1cno3 <> v1hno3 or  v1cno4 <> v1hno4
125500070316     c
125600070314     c                   eval      $almenouna = *off
125700070314     c                   clear                   W_ultimopno
125800070314      * verifico se esistevano già 4 righe stampabili
125900070319     c                   eval      W_gst = 'S'
126000070319     c     K06arn11      setll     Fiarn11l
126100070314
126200070314     c                   do        *hival
126300070314
126400070319     c     k06arn11      reade     Fiarn11l
126500070314
126600070319     c                   if        %eof(fiarn11l)
126700070314     c                   leave
126800070314     c                   endif
126900070314
127000070316      * imposto il flag da stampabile a storico
127100070319     c                   clear                   arngst
127200070314     c                   update    Fiarn000
127300070314     c                   eval      $almenouna = *on
127400070314     c                   eval      W_ultimopno = arnpno
127500070314
127600070314     c                   enddo
127700070314      * scrivo le nuove righe
127800070314      * attenzione se non è stato scritto nulla devo almeno scrivere una riga vuota
127900070314      * se non è la prima volta che scrivo note
128000070314
128100070314      * recupero  ora
128200070314     c                   time                    w0140
128300070314     c                   movel     w0140         wora
128400070314      * verifico se esiste almeno una riga scritta
128500070314     c                   If        v1cno1 = *blanks and v1cno2 = *blanks and
128600070316     c                             v1cno3 = *blanks and v1cno4 = *blanks
128700070314      * verifico se è la prima volta che scrivo delle note non faccio nulla altrimenti
128800070316      * scrivo una riga vuota solo se non è uguale alla precedente
128900070316     c                   if        $almenouna = *on
129000070314     c                   eval      W_arnnob = v1cno1
129100070314     c                   exsr      SR_FIARN
129200070315     c                   else
129300070315      * se non c'è neppura una riga inserita do errore
129400070315     c                   eval      *in28 = *on
129500070315     c                   eval      v1cmsg = msg(7)
129600070314     c                   endif
129700070314
129800070314     c                   else
129900070314      * scrittura nota 1 se valorizzata
130000070314     c                   if        v1cno1 <> *blanks
130100070314     c                   eval      W_arnnob = v1cno1
130200070314     c                   exsr      SR_FIARN
130300070314     c                   endif
130400070314      * scrittura nota 2 se valorizzata
130500070314     c                   if        v1cno2 <> *blanks
130600070314     c                   eval      W_arnnob = v1cno2
130700070314     c                   exsr      SR_FIARN
130800070314     c                   endif
130900070314      * scrittura nota 3 se valorizzata
131000070314     c                   if        v1cno3 <> *blanks
131100070314     c                   eval      W_arnnob = v1cno3
131200070314     c                   exsr      SR_FIARN
131300070314     c                   endif
131400070314      * scrittura nota 4 se valorizzata
131500070314     c                   if        v1cno4 <> *blanks
131600070314     c                   eval      W_arnnob = v1cno4
131700070314     c                   exsr      SR_FIARN
131800070314     c                   endif
131900070314
132000070319     c                   feod      fiarn11l
132100070314     c                   endif
132200070314
132300070316     c                   endif
132400131122
132500131219      /free
132600140205       //?Se non ho variato note e/o comunicazione e non ho ricevuto una variazione vado via
132700140205         IF  not wokarn and V1Ccmd = CAUcmd and IN6var = *blanks;
132800140204           leavesr;
132900140204         ENDIF;
133000140204
133100131219       //?Verifico la distinta a PDA
133200131219       exsr CTR_dist;
133300131127       //?Scrivo file delle telefonate all'AUT se:
133400131219       //?bolla in consegna e non sono stata richiamata da chiusura distinta e filiale partita
133500131219       //?con telefonate AUT
133600131219         IF  IN6cons = 'S' and  IN6Chdist = ' ' and wSITel = *on and
133700131219         //?e richiesto invio dati all'AUT
133800131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
133900131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
134000131219         //?                           SCRIVO
134100131219               V1Caut = 'SI';
134200131128             clear FNLRX3DS;
134300131128             IX3tpdoc = 'C';
134400131128             IX3iddoc = %editc(IN6lnp:'X') + %editc(IN6nrs:'X') +
134500131128                        %editc(IN6nsp:'X') + %editc(IN6aas:'X');
134600131128             IX3fgs = ARBifp;
134700131128             IX3nfv = ARBndc;
134800131128             IX3dfv = ARBddc;
134900131128             IX3pdr = ARBpdc;
135000131128             IX3var = IN6var;
135100131128             IX3cmd = V1Ccmd;
135200131219         //?immesse note
135300131219             IF  wOkArn ;
135400131206               IX3var  = %trim(IX3var)+ 'N';
135500131129             ENDIF;
135600131128             fnlrx3r (kpjba:fnlrx3ds);
135700131122         ENDIF;
135800131122
135900131127       //?Scrivo file delle dati a PDA se:
136000131127       //?bolla in consegna
136100131127         IF  IN6cons = 'S' and
136200131128         //?distinta a PDA (test e non test)
136300131128             (wSIpda or wTESTpda) and
136400131219         //? e richiesto invio dati all'AUT
136500131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
136600131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
136700131219         //?                           CHIAMO FIDG42R
136800140206              V1Caut <> 'NO' ;
136900131127           clear FIDG42DS;
137000131127           CO42aas = IN6aas;
137100131127           CO42lnp = IN6lnp;
137200131127           CO42nrs = IN6nrs;
137300131127           CO42nsp = IN6nsp;
137400131127           CO42fgs = ARBifp;
137500131127           CO42ndc = ARBndc;
137600131127           CO42ddc = ARBddc;
137700131127           kpjbu = FIDG42DS;
137800131127           fidg42r (kpjba);
137801171127           clear FIDG42DS;
137802171127           CO42tla = 'C';
137803171127           kpjbu = FIDG42DS;
137804171127           fidg42r (kpjba);
137900131122         ENDIF;
138000131122      /end-free
138100070316
138200070314     c                   ENDSR
138300070314      *---------------------------------------------------------------*
138400070314      * sr_FIARN   * Imposto i campi per la scrittura di FIARN        *
138500070314      *---------------------------------------------------------------*
138600070314     c     sr_FIARN      BEGSR
138700131129
138800131129      /free
138900131129         wOkArn = *on;
139000131129      /end-free
139100070314
139200070314     c                   clear                   fiarn000
139300070314     c                   eval      arnaas = vtcaas
139400070314     c                   eval      arnlnp = vtclnp
139500070314     c                   eval      arnnrs = vtcnrs
139600070314     c                   eval      arnnsp = vtcnsp
139700070319     c                   eval      arngst = 'S'
139800070314     c                   eval      W_ultimopno = W_ultimopno + 1
139900070314     c                   eval      arnpno = W_ultimopno
140000070314     c                   eval      arnnob = W_arnnob
140100070314     c                   eval      arnpru = dutute
140200070314     c                   eval      arndim = dateu
140300070314     c                   eval      arnoim = Wora
140400070314      * programma di immissione
140500070314      *                         se è richiamato prendo quello della ds
140600070314      *                         altrimenti noi stessi
140700131121     c                   eval      arnpgm = in6pgm
140800070319
140900070319     c                   eval      arncdn = 'LDV'
141000070314
141100070314     c                   write     fiarn000
141200070314
141300070314     c                   ENDSR
141400070404      *---------------------------------------------------------------*
141500070404      * sr_COPIA   * Copia note R.A.  su note LDV libere              *
141600070404      *---------------------------------------------------------------*
141700070404     c     sr_copia      BEGSR
141800070404      * se richiesta la copia delle note R.a. su note ldv prima verifico
141900070404      * se c'è posto in aggiunta alle note già esistenti
142000070404     c                   select
142100070404      * tutte occupate
142200070404     c                   when      v1cno4 <> *blanks
142300070404     c                   eval      *in28 = *on
142400070404     c                   eval      v1cmsg = msg(9)
142500070404      * se la prima nota è vuota copio tutte e 4
142600070404     c                   when      v1cno1 = *blanks
142700070404     c                   eval      v1cno1 = vtcno1
142800070404     c                   eval      v1cno2 = vtcno2
142900070404     c                   eval      v1cno3 = vtcno3
143000070404     c                   eval      v1cno4 = vtcno4
143100070404      * se la seconda nota è vuota copio solo sulle ultime 3
143200070404     c                   when      v1cno2 = *blanks
143300070404     c                   eval      v1cno2 = vtcno1
143400070404     c                   eval      v1cno3 = vtcno2
143500070404     c                   eval      v1cno4 = vtcno3
143600070404      * se la terza   nota è vuota copio solo sulle ultime 2
143700070404     c                   when      v1cno3 = *blanks
143800070404     c                   eval      v1cno3 = vtcno1
143900070404     c                   eval      v1cno4 = vtcno2
144000070404      * se la quarta  nota è vuota copio solo sull'ultima
144100070404     c                   when      v1cno4 = *blanks
144200070404     c                   eval      v1cno4 = vtcno1
144300070404
144400070404     c                   endsl
144500070404     c                   endsr
144600070314
144700070308** MSG errore
144800140204Comunicazione errata                                                            1
144900140204Comunicazione non utlizzabile per le spedizioni                                 2
145000140204Non è possibile togliere una comunicazione                                      3
145100140204Obbligo di avvisare AUT se c'è comunicazione                                    4
145200131128....libero                                                                      5
145300070315Spedizione inesistente o in gestione ad altro P.O.                              6
145400070315E' stato dato F6 di conferma senza scrivere nulla                               7
145500070316Gestione non abilitata                                                          8
145600070404Non è possibile copiare/aggiungere Note R.A. alle Note LDV già esistenti        9
