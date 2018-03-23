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
001200070312     fFIAR401L  If   e           k disk
001300070308     fAZORG01L  If   e           k disk
001400070313     fTABEL00F  If   e           k disk
001500131122     fFIDST08L  if   e           k disk
001501140304     fFIapd01l  if   e           k disk
001600140204     fFIARP01L  if   e           k disk
001700070313      *
001800131121     fFNLRN6D   cf   e             workstn sfile(LRN6S02:S01nrr)
001900131202     f                                     maxdev(*file)
002000070313
002100070313      *---------------------------------------------------------------*
002200070313
002300070313      *
002400070313      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002500070308      *
002600070313      *
002700070313      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002800070313      *
002900070404     d MSG             S             78    DIM(009) CTDATA PERRCD(1)            MSG VIDEO
003000070313      *
003100070313     d L6              s              3  0 dim(30)                              po £6
003200131120     D wpog            s              3    dim(250)
003300070313      *
003400070313      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003800131219      *
003900070313     d KPJBA         e ds
004000130304      *
004100131121     D ficauds       E DS                  extname(FICAU00F)
004200130304     D fnarbds       E DS                  extname(fnarb00f)
004300070313      * - Parametri
004400131121     d FNLRN6DS      e ds
004500070313      *
004600070313      * - Parametri x Controllo profilo utenti
004700070313     d TIBS34DS      e ds
004800131120      * - Tabelle
004900131120     d TIBS02ds      e ds                  inz
005000070313      * - Ds di riferimento al file esterno AZUTE00F
005100070313     d AZUTEDS       e ds                  extname(AZUTE00F)
005200070313      * - Ds per dati organigramma
005300070313     d DDatiUte      e ds
005400070315      * abilitazioni
005500070315     D OG148         E DS
005600070313      * DS per TRUL06R - Caricamento £X
005700070313     d TRUL06DS      E DS
005800070313     d  LIN                    1     90  0
005900070313     d                                     DIM(30)
006000131120
006100131120     D TRUL31DS      E DS
006200131120     d dLat          e ds
006300131120     d dute01        e ds
006400070313      *
006500130304      * DS per Fidg42R - passaggio dati a PDA
006600130304     d fidg42ds      e ds
006700131122
006800131122     d ddstflr       e ds
006900131128
007000131128       // Ds passaggio dati a pgm FNLRX3R
007100131128     d fnlrx3ds      e ds
007200131128
007300140121       // Ds passaggio dati a pgm TNTBCMDR
007400140121     d tntbcmdds1    e ds
007500140121
007600131128       // Tabella CMD
007700131128     d dCMD          e ds
007800131122
007900130304      *
008000070313     d Status         sds
008100070313     d  SDSpgm           *proc
008200070313     d  PARMS            *parms
008300070313      *
008400070313     d WLBdat          ds                  inz
008500070313     d  G02dat                 1      8  0 inz
008600070313     d  G02inv                 9     16  0 inz
008700070313     d  G02err                17     17    inz('3')
008800070313     d  G02tgi                18     22  0 inz
008900070315      *
009000070313      *
009100070313      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009200070313      *
009300070313      * - Campi di appoggio per il subfile
009400070313     d S01nrr          s                   like(V1Cnrr)   inz
009500070313      *
009600070313      * - Campi di salvataggio temporaneo dati
009700070313      *
009800070313      * - Flags booleani
009900070313     d $Fine           s              1    inz(*off)
010000070313     d $Sped           s              1    inz(*off)
010100070313     d $InzS01         s              1    inz(*on)
010200070314     d $almenouna      s              1    inz(*off)
010300070315     d $abilitato      s              1    inz(*off)
010400070319     d $immissione     s              1    inz(*off)
010500131219     d wSItel          s               n   inz(*off)
010600131122     d wSIpda          s               n   inz(*off)
010700131122     d wNOpda          s               n   inz(*off)
010800131122     d wTESTpda        s               n   inz(*off)
010900131127     d wEoF            s               n   inz(*off)
011000131127     d wF03            s               n   inz(*off)
011100131129     d wOkArn          s               n   inz(*off)
011200070313      *
011300070313      * - Indici di schiera / Contatori
011400070313     d xx              s              3  0 inz
011500070313      *
011600070313      * - Campi di comodo
011700070313     d w0140           s             14  0
011800070314     d wData           s              8  0
011900070314     d DateU           s              8  0
012000070314     d wora            s              6  0
012100131120     d w003a           s              3
012200070308     d Keyaas          s                   like(arbaas)
012300070308     d Keylnp          s                   like(arblnp)
012400070308     d Keynrs          s                   like(arbnrs)
012500070308     d Keynsp          s                   like(arbnsp)
012600070309     d codut           s                   like(tblkut) inz(1)
012700070309     d Kcod            s                   like(tblcod)
012800070309     d Kkey            s                   like(tblkey)
012900070312     d Ktrc            s                   like(ar4trc)
013000070319     d W_gst           s                   like(arngst)
013100070319     d W_cdn           s                   like(arncdn) inz('LDV')
013200070314     d W_dim           s                   like(arndim)
013300070314     d W_arnpru        s                   like(arnpru)
013400070314     d W_arndim        s                   like(arndim)
013500070314     d W_arnoim        s                   like(arnoim)
013600070314     d W_arnnob        s                   like(arnnob)
013700070314     d W_ultimopno     s                   like(arnpno)
013800131120     d wabi            s                   like(UteAut)
013900131121     d woggetto        s                   like(CAUogg)
014000131122     d wfgs_ifp        s                   like(arblnp)
014100131122     d wnpg            s                   like(DSTnpg) inz(4)
014200140204     d wtcr            s                   like(ARPtcr)
014300140204     d wdcr            s                   like(ARBdcr)
014301140303     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
014400140207     d wnfv            s              6  0
014500131122
014600131122       //--------------------------------------------------------------
014700131122       //?Definizione prototipi e procedure.
014800131122       //--------------------------------------------------------------
014900131122
015000131122      /copy gaitrasrc/srcprotopr,tibs02r
015100140121      /copy gaitrasrc/srcprotopr,tntbcmdr1
015200131122
015300131122     d fidg42r         pr                  extpgm('FIDG42R')
015400131122     d  kpjba                              likeds(kpjba)
015500131128
015600131128     d fnlrx3r         pr                  extpgm('FNLRX3R')
015700131128     d  kpjba                              likeds(kpjba)
015800131128     d  fnlrx3ds                           likeds(fnlrx3ds)
015900131122
016000131122       //--------------------------------------------------------------
016001140304       //--------------------------------------------------------------
016002140304       //?Definizione key-list.                                        ?
016003140304       //--------------------------------------------------------------
016004140304
016008140304     d keyFIAPD01    e ds                  extname( FIAPD01L : *key )
016009140304     d                                     prefix(k_)   inz
016200070313      *
016300070313      *   K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - - *
016400070313      *
016500070319      * - file FIARN11L/12L
016600070319     c     K06ARN11      klist
016700070313     c                   kfld                    Vtcaas
016800070313     c                   kfld                    Vtclnp
016900070313     c                   kfld                    Vtcnrs
017000070313     c                   kfld                    Vtcnsp
017100070319     c                   kfld                    w_cdn
017200070319     c                   kfld                    w_gst
017300070319      * - file FNARB01L
017400070319     c     K04ARB01      klist
017500070309     c                   kfld                    vtcaas
017600070309     c                   kfld                    vtclnp
017700070309     c                   kfld                    vtcnrs
017800070309     c                   kfld                    vtcnsp
017900070312      * - file FIAR401L
018000070312     c     K05AR401      klist
018100070312     c                   kfld                    vtcaas
018200070312     c                   kfld                    vtclnp
018300070312     c                   kfld                    vtcnrs
018400070312     c                   kfld                    vtcnsp
018500070312     c                   kfld                    ktrc
018600070309      * - file TABEL00F
018700070309     c     K03TABEL      klist
018800070309     c                   kfld                    Codut
018900070309     c                   kfld                    Kcod
019000070309     c                   kfld                    Kkey
019100070313
019200070313      *===============================================================*
019300070313      *  RIEPILOGO INDICATORI                                         *
019400070313      *---------------------------------------------------------------*
019500070313      *  20    - SFLDSP    in C01                                     *
019600070313      * N21    - SFLCLR    in C01                                     *
019700070313      *  21    - SFLDSPCTL in C01                                     *
019800070313      *  23    - SFLEND    in C01                                     *
019900070313      *  28    - Errore generico dspf                                 *
020000070314      *  39    - sfl reverse immage                                   *
020100131121      *  40/43 - Condiziona visualizzazione campi                     *
020200070313      *  50/   - Errore posizionamento a video                        *
020300140204      *  51/   - Errore posizionamento a video                        *
020400070316      *  71    - utente che appartiene ad un P.O. abilitato a geo     *
020500070316      *  72    - Programma richiamato in interrogazione               *
020600070316      *  73    - Abilito F11 storico                                  *
020700070322      *  74    - Disabilito F10 Inserimento                           *
020800070313      *  90    - Errore                                               *
020900070313      *===============================================================*
021000070313
021100070315      *
021200070315     c                   exsr      sr_imposta
021300131122      /free
021400131122         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021500131122      /end-free
021600070319
021700070319      *-----------------------------------------------------------------------------
021800131121      *            FLUSSO PRINCIPALE
021900070319      *-----------------------------------------------------------------------------
022000070319
022100070313      * controllo la bolla
022200070313     c                   exsr      sr_ctrarb
022300070314
022400070313      * spedizione non trovata errore e fine
022500070314
022600131127     c                   If        $sped = *off
022700131121     c                   movel     MSG(6)        On6msg
022800131121     c                   eval      on6err = 'E'
022900131127     c                   goto      fine
023000070319     c                   endif
023100070319
023200070313      * recupero dati dal file
023300070313     c                   exsr      SR_FNARB
023400131122      /free
023500131122       //?recupero comando dal file
023600131122       exsr sr_FICAU;
023700140204       //?Il flag di comunicazione AUT lo propongo sempre = 'SI'
023800140204       V1Caut = 'SI';
023900140204       //?se non c'è il comando imposto nella descrizione 'variazione'
024000140204       IF  V1Ccmd = *blanks;
024100140204         V1Dcmd = 'VARIAZIONE';
024200140204       ENDIF;
024300140204       //?Cerco se la spedizione in consegna ha la data consegna richiesta
024400140204       IF  IN6cons = 'S';
024500140204         clear wtcr;
024600140204         clear wdcr;
024700140204         exsr sr_DCR;
024800140204         //?Se c'è la data consegna richiesta e non è per oggi
024900140204         //?e non c'è la comunicazione all'AUT
025000140204         IF  V1Ccmd = *blanks and
025100140204            (wtcr = 'D' and wdcr >= dateu or
025200140204             wtcr = *blanks and wdcr > dateu);
025300140204         //?Non devo comunicare niente
025400140204           V1Caut = 'NO';
025500140204         ENDIF;
025600140204       ENDIF;
025700131122      /end-free
025800131121
025900070308      * gestione delle routine in caso di pgm richiamato per la sola immissione e nel caso
026000070308      * di visualizzazione e manutenzione
026100070308     c                   select
026200070308
026300131121     c                   when      in6mod = 'V'
026400070319      * carico il subfile
026500070319     c                   exsr      sr_CARSBF
026600070319
026700070316     c                   exsr      sr_VISUAL
026800131121     c                   goto      fine
026900070308
027000070308     c                   other
027100070316
027200070316     c                   exsr      sr_IMMISS
027300070319
027400070315      * se viene dato f3 di fine non proseguo
027500070319
027600070319     c                   If        $fine = *on
027700131127     c                   goto      fine
027800070319     c                   endif
027900070308
028000070308     c                   endsl
028100070319
028200070313     c     Fine          tag
028300131127
028400070313      *
028500070313     c                   eval      *inLR   = *on
028600070313
028700070313      *---------------------------------------------------------------*
028800070313      * Routine Iniziale                                              *
028900070313      *---------------------------------------------------------------*
029000070313     c     *InzSR        BegSR
029100070313      *
029200070313      * Reperisco dati job
029300070313     c                   exsr      DatiJob
029400070313     c                   movel     SDSpgm        VTCpgm
029500131120
029600131120      * Imposto le abilitazioni dell'utente
029700131120     c                   eval      dute01 = UTEfaf
029800131120
029900070314      * Reperisco data corrente
030000070314     c                   time                    w0140
030100070314     c                   move      w0140         wData
030200070314     c                   clear                   WLBdat
030300070314     c                   eval      G02dat = wData
030400070314     c                   call      'XSRDA8'
030500070314     c                   parm                    WLBdat
030600070314     c                   z-add     G02inv        DateU
030700070313      *
030800070313     c                   EndSr
030900070315      *---------------------------------------------------------------*
031000070315      * Impostazioni Iniziali                                         *
031100070315      *---------------------------------------------------------------*
031200070315     c     SR_imposta    BegSR
031300070315      *
031400070315     c     *Entry        plist
031500131129     c                   parm                    KPJBA
031600131121     c                   parm                    FNLRN6DS
031700070315
031800070315      * carico la £6
031900070315     c                   exsr      sr_carl6
032000070315
033500070315      * visualizzazione
033600131121     c                   if        in6mod = 'V'
033700070316     c                   eval      *in72 = *on
033800070315     c                   endif
033900131121
034000070319      * imposto i campi della chiave della spedizione
034100131121     c                   eval      vtcaas = in6aas
034200131121     c                   eval      vtclnp = in6lnp
034300131121     c                   eval      vtcnrs = in6nrs
034400131121     c                   eval      vtcnsp = in6nsp
034500131122
034600140206       //?Verifico la distinta a PDA
034700140207     c                   IF        IN6cons = 'S'
034800140206     C                   exsr      CTR_dist
034900140207     c                   ENDIF
035000131219      * visualizzo la videata dei comandi se bolla in consegna , filiale distinta partita con le
035100131219      * telefnate ad aut e se non sono stato richiamato dalla chiusura distinta
035200131219     c                   eval      *in41 = IN6cons = 'S' and IN6chdist = ' ' and
035300140226     c                             wSITel = *on
035400070315
035500070315     c                   EndSr
035600070315
035700070313      *---------------------------------------------------------------*
035800070313      * Reperimento Dati del job (Utente/Operativi)                   *
035900070313      *---------------------------------------------------------------*
036000070313     c     DatiJob       BegSr
036100070313      *
036200070313     c     *dtaara       define    §azute        azuteds
036300070313     c     *dtaara       define    §datiute      ddatiute
036400070313      *
036500070313     c                   in(E)     *dtaara
036600070313     c                   IF        %ERROR or RSUT = *blanks
036700070313     c                   clear                   Tibs34Ds
036800070313     c                   call      'TIBS34R'
036900070313     c                   parm                    Tibs34Ds
037000070313     c                   in        *dtaara
037100070313     c                   ENDIF
037200070313      *
037300070313     c                   EndSr
037400070313
037500070313      *---------------------------------------------------------------*
037600070313      * Recupero i P.O.  della £6                                     *
037700070313      *---------------------------------------------------------------*
037800070313     c     Sr_CARL6      begsr
037900070313
038000070313      * Recupero i PO della £6
038100070313      *
038200070313     c                   clear                   trul06ds
038300070313     c                   move      '£6'          d06cod
038400070313     c                   movel     Dutpou        d06key
038500070313     c                   movel(p)  trul06ds      KPJBU
038600070313
038700070313     c                   call      'TRUL06R'
038800070313     c                   parm                    kpjba
038900070313     c                   movel     kpjbu         trul06ds
039000070313     c                   movea     lin           L6
039100070313
039200070315      * verico se PO abilitato alla gestione telefonate GEO
039300070315
039400070315     c                   clear                   og148
039500070315     c     l6(1)         chain     azorg01l
039600070315     c                   if        %found(azorg01l)
039700070315     c                   movel     orgde8        og148
039800070315     c                   endif
039900070315
040000070315     c                   If        §oggeot = 'S'
040100070315     c                   eval      $abilitato = *on
040200070316     c                   eval      *in71      = *on
040300070315     c                   endif
040400131120
040500131120      /free
040600131120       //?Imposto abilitazione utente
040700131120         clear wpog;
040800131120         SELECT;
040900131120       //?Imposto abilitazione manutenzione bolle di filiale
041000131120           WHEN  §UTEmbf <> *blanks;
041100131120             wabi = §UTEmbf;
041200131120           WHEN  UTEaut <> *blanks;
041300131120             wabi = UTEaut;
041400131120           WHEN  DUTlpo = '1';
041500131120             wabi = 'TP';
041600131120           WHEN  DUTlpo = '2';
041700131120             wabi = 'PO';
041800131120         ENDSL;
041900131120      /end-free
042000131120
042100131120      * controllo se ok l'abilitazione dell'utente
042200131120     c                   clear                   TIBS02DS
042300131120     c                   clear                   dLAT
042400131120     c                   eval      T02mod = 'C'
042500131120     c                   eval      T02sif = knsif
042600131120     c                   eval      T02cod = 'LAT'
042700131120     c                   movel(p)  wabi          T02ke1
042800131120     c                   call      'TIBS02R'
042900131120     c                   parm                    kpjba
043000131120     c                   parm                    TIBS02DS
043100131120if  1c                   IF        T02err = *blanks
043200131120     c                   eval      dLAT = T02uni
043300131120e   1c                   ENDIF
043400131120
043500131120      * Se abilitazione trovata e OK
043600131120if  1c                   IF        T02err = *blanks and §LATabi = *blanks
043700131120      * Reperimento dei P.O. gestibili dall'utente
043800131120     c                   clear                   TRUL31DS
043900131120     c                   eval      I31abi = wabi
044000131120     c                   eval      I31cdi = DUTdis
044100131120     c                   eval      I31car = DUTare
044200131120     c                   eval      I31cpo = DUTpou
044300131120     c                   call      'TRUL31R'
044400131120     c                   parm                    kpjba
044500131120     c                   parm                    TRUL31DS
044600131120if  2c                   IF        O31pog > *zeros
044700131120     c                   movea     O31pog        wpog
044800131120x   2c                   ENDIF
044900131120    1c                   ENDIF
045000131120
045100131121      * se sk delle filiali abilitate all'utente è vuota, imposto il primo elemento della £6
045200131120     c                   IF        wpog(1) = *blanks
045300131120     c                   eval      wpog(1) = %editc(l6(1):'X')
045400131120     c                   ENDIF
045500131120
045600070313     c                   endsr
045700070313      *---------------------------------------------------------------*
045800070313      * sr_ctrarb  * controllo se spedizione corretta                 *
045900070313      *---------------------------------------------------------------*
046000070313     c     sr_ctrarb     BEGSR
046100070313
046200070313     c                   eval      $sped = *off
046300070313
046400070313     c     k04arb01      chain     Fnarb01l
046500070313
046600070412    1c                   If        %found(fnarb01l)
046700131122      * verifico se la linea di arrivo è presente nelle linee abilitate alla manut. bolle filiale
046800131120     c                   move      arblna        w003a
046900131122     c                   IF        %lookup (w003a:wPOG) > 0
047000070313     c                   eval      $sped = *on
047100070412    2c                   endif
047200131122      * se non abilitata
047300131122    2c                   if        %lookup (w003a:wpog) = 0
047400070412      * disabilito il comando di scrittura note
047500131121    4c                   if        in6mod = 'V'
047600070322     c                   eval      $sped = *on
047700131122      * e disattivo f10 di inserimento
047800070322     c                   eval      *in74 = *on
047900070412    4c                   endif
048000070412    2c                   endif
048100070412
048200070412    1c                   endif
048300070313
048400070313     c                   endsr
048500070313
048600070313      *---------------------------------------------------------------*
048700070313      * sr_clr_W_not   * Pulizia videata richiesta note               *
048800070313      *---------------------------------------------------------------*
048900070313     c     sr_clr_W_not  BEGSR
049000070313
049100131121     c                   clear                   vtcns1
049200131121     c                   clear                   vtcns2
049300131121     c                   clear                   vtccmd
049400131121     c                   clear                   vtcdtacmd
049500131121     c                   clear                   vtcoracmd
049600131121     c                   clear                   vtcutecmd
049700131121     c                   clear                   v1ccmd
049800131121     c                   clear                   v1dcmd
049900070313     c                   clear                   v1cno1
050000070313     c                   clear                   v1cno2
050100070313     c                   clear                   v1cno3
050200070313     c                   clear                   v1cno4
050300070316     c                   clear                   v1hno1
050400070316     c                   clear                   v1hno2
050500070316     c                   clear                   v1hno3
050600070316     c                   clear                   v1hno4
050700070316     c                   clear                   vtcdim
050800070313     c                   clear                   vtcoim
050900070313     c                   clear                   vtcpru
051000131121     c                   clear                   v1caut
051100070313     c                   eval      *in40 = *off
051200070313     c                   eval      *in42 = *off
051300131121     c                   eval      *in43 = *off
051400070313
051500070313     c                   endsr
051600070313
051700070309      *---------------------------------------------------------------*
051800070309      * sr_FNARB   * Recupero informazioni da bolle arrivi            *
051900070309      *---------------------------------------------------------------*
052000070309     c     sr_FNARB      BEGSR
052100070309
052200070313      * routine di pulizia del video
052300070313
052400070313     c                   exsr      SR_clr_W_not
052500070313
052600070313      * data spedizione
052700070313     c                   clear                   wlbdat
052800070313     c                   movel(p)  arbaas        g02inv
052900070313     c                   move      arbmgs        g02inv
053000070314     c                   eval      G02err = '3'
053100070313     c                   call      'XSRDA8'
053200070313     c                   parm                    WLBdat
053300070313     c                   z-add     G02dat        vtcdta
053400070312      * note spedizione
053500070312
053600070313     c                   eval      ktrc = '8'
053700070313     c     K05ar401      chain     fiar401l
053800070312     c                   if        %found(fiar401l)
053900070312     c                   eval      vtcns1 = Ar4not
054000070312     c                   endif
054100070312
054200070313     c                   eval      ktrc = '9'
054300070313     c     K05ar401      chain     fiar401l
054400070312     c                   if        %found(fiar401l)
054500070312     c                   eval      vtcns2 = Ar4not
054600070312     c                   endif
054700070313      *
054800070313      * Verifico se sono in manutenzione e recupero le ultime note stampabili
054900070309      *
055000070319     c                   eval      W_gst = 'S'
055100070319     c     K06arn11      setll     Fiarn11l
055200070313
055300070313     c                   do        *hival
055400070313
055500070319     c     k06arn11      reade(n)  Fiarn11l
055600070314
055700070319     c                   if        %eof(fiarn11l)
055800070314     c                   leave
055900070314     c                   endif
056000070313
056100070313      * imposto i dati dell'ultimo utente che ha inserito la/e nota/e stampabile/i
056200070313
056300070313     c                   if        not *in42
056400070316      * valorizzo indicatore per gestione f11 storico
056500070316     c                   eval      *in73 = *on
056600070313
056700070313     c                   eval      *in42 = *on
056800070313     c                   eval      vtcpru = arnpru
056900070313     c                   eval      vtcoim = arnoim
057000070313
057100070313     c                   clear                   wlbdat
057200070313     c                   eval      G02inv = arndim
057300070314     c                   eval      G02err = '3'
057400070313     c                   call      'XSRDA8'
057500070313     c                   parm                    WLBdat
057600070313     c                   z-add     G02dat        vtcdim
057700070313
057800070313     c                   endif
057900070313
058000070313     c                   select
058100070313
058200070313     c                   when      v1cno1 = *blanks
058300070313     c                   eval      v1cno1 = arnnob
058400070313
058500070313     c                   when      v1cno2 = *blanks
058600070313     c                   eval      v1cno2 = arnnob
058700070313
058800070313     c                   when      v1cno3 = *blanks
058900070313     c                   eval      v1cno3 = arnnob
059000070313
059100070313     c                   other
059200070313     c                   eval      v1cno4 = arnnob
059300070313
059400070313     c                   endsl
059500070313
059600070313     c                   enddo
059700070313      * verifico se mi hanno passato  degli appunti
059800070313
059900131121     c                   if        in6not1 <> *blanks
060000131121     c                   eval      vtcno1 = in6not1
060100070313     c                   eval      *in43 = *on
060200070313     c                   endif
060300070313
060400131121     c                   eval      vtcno2 = in6not2
060500131121     c                   eval      vtcno3 = in6not3
060600131121     c                   eval      vtcno4 = in6not4
060700070313
060800070313     c                   endsr
060900131121
061000131122      /free
061100131122       // ----------------------------------------------------------------------
061200131122       // Recupero il comando dal file delle telefonate AUT
061300131122       // ----------------------------------------------------------------------
061400131122       BEGSR  sr_FICAU;
061500131122
061600131122         woggetto = %editc(ARBlnp:'X') +
061700131122                    %editc(ARBnrs:'X') +
061800131122                    %editc(ARBnsp:'X') +
061900131122                    %editc(ARBaas:'X');
062000131122
062100131122       //?Preparo il cursore
062200131122         exec sql
062300131122         DECLARE CAU cursor FOR
062400131122         SELECT * from FICAU00F
062500131122         WHERE CAUtor = 'C' and CAUogg = :woggetto
062600131122           and CAUfgs = :IN6fgs and CAUdfv = :IN6dfv
062700131129           and CAUnfv = :IN6nfv and CAUpdr = :IN6pdr
062800131122           and CAUatb = ' '
062900131122         ORDER by  CAUdtoins;
063000131121
063100131122       //?Apro il cursore
063200131122         exec sql
063300131122         OPEN  CAU;
063400131129
063500131129         IF sqlcode < 0;
063600131129           wEoF = *on;
063700131129         ENDIF;
063800131122
063900131122       //?leggo il file
064000131127         DOW  not wEoF;
064100131122          exec sql
064200131122          FETCH next from CAU into :FICAUDS;
064300131122
064400131122       //?fine file o errore sql esco
064500131122           IF   sqlcod = 100 or sqlcod < 0;
064600131127            wEoF = *on;
064700131122            leave;
064800131122           ENDIF;
064900131122
065000131122       //?comando
065100131122           IF  CAUdescmd <> *blanks;
065200131210             V1Ccmd = CAUcmd;
065300131211             V1Dcmd = CAUdescmd;
065400131122             VTCcmd = CAUdescmd;
065500131122             *in40 = *on;
065600140303             wdate_eur = %date(%dec(%subst(CAUdtoins:1:8):8:0));
065601140303             vtcdtacmd = %dec( Wdate_eur );
065700131122             VTCoracmd = %dec(%subst(CAUdtoins:9:6):6:0);
065800131122             VTCutecmd = CAUpruins;
065900131122           ENDIF;
066000131127           wEoF = *on;
066100131122           leave;
066200131122         ENDDO;
066300131121
066400131122       //?Chiudo il cursoe
066500131122         exec sql
066600131122         close CAU;
066700131122
066800131122       ENDSR;
066900140204
067000140204       // ----------------------------------------------------------------------
067100140204       // Recupero la data consegna richiesta.
067200140204       // ----------------------------------------------------------------------
067300140204       BEGSR  sr_DCR;
067400140204
067500140204       //?Cerco se la data è su FIARP
067600140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FIARP01L;
067700140204         IF  %found(FIARP01L);
067800140204           wtcr = ARPtcr;
067900140204           wdcr = ARPdcr;
068000140204           leavesr;
068100140204         ENDIF;
068200140204
068300140204       //?Non la trovo su ARP la cerco su FNARB
068400140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FNARB01L;
068500140204         IF  %found(FNARB01L);
068600140204           wtcr = ARbtcr;
068700140204           wdcr = ARbdcr;
068800140204           leavesr;
068900140204         ENDIF;
069000140204
069100140204       ENDSR;
069200140204
069300131122      /end-free
069400070313
069500070313      *---------------------------------------------------------------*
069600070313      * sr_IMMISS  * Gestione della videata di immissione             *
069700070313      *---------------------------------------------------------------*
069800070313     c     sr_IMMISS     BEGSR
069900070313
070000070319     c                   eval      $immissione = *on
070100070418      * mi salvo le righe di note per non scrivere delle note doppie
070200070418     c                   eval      v1hno1 = v1cno1
070300070418     c                   eval      v1hno2 = v1cno2
070400070418     c                   eval      v1hno3 = v1cno3
070500070418     c                   eval      v1hno4 = v1cno4
070600070313      *
070700070313     c                   do        *hival
070800070319     c                   eval      $fine = *off
070900131127      /free
071000131127       wF03 = *off;
071100131127      /end-free
071200070315
071300131121     c                   write     lrn6t00
071400131121     c                   write     lrn6z01
071500070313
071600131202     c                   write     lrn6d01
071700131202     c                   read(e)   fnlrn6d
071800070315     c                   eval      *in28 = *off
071900131202     c                   IF        %error
072000131202     c                   eval      *inkc = *on
072100131202     c                   ENDIF
072200131122
072300070313
072400070313      * f3 - Fine lavoro
072500070313     c                   if        *inkc
072600070315     c                   eval      $fine = *on
072700131127      /free
072800131127       wF03 = *on;
072900131127      /end-free
073000070313     c                   leave
073100070313     c                   endif
073200070315
073300070404      * f2 - Copia note R.A.
073400070404     c                   if        *inkb
073500070404     c                   exsr      SR_copia
073600070404     c                   iter
073700070404     c                   endif
073800070404
073900070316      * F11 gestione dello storico
074000070403     c                   if        *inkk
074100070403     c* Se provengo da videata di visulizzazione, esco
074200070319      * carico il subfile
074300131121     c                   if        in6mod<>'V'
074400070319     c                   exsr      sr_CARSBF
074500070319
074600070316     c                   exsr      SR_VISUAL
074700070319     c                   iter
074800070403     c                   else
074900070403     c                   leave
075000070403     c                   endif
075100070316     c                   endif
075200070316
075300070315     c                   exsr      SR_contr
075400070315
075500070315      * ci sono degli errori
075600140124     c                   if        *in28 or *in90
075700070315     c                   iter
075800070315     c                   endif
075900131122
076000070314      * F06 conferma
076100070314     c                   if        *inkf
076200070314     c                   exsr      SR_confe
076300131121     c                   if        *in28
076400070315     c                   iter
076500070315     c                   else
076600131121      * se immissione o manutenzione vado a fine pgm
076700131121     c                   if        in6mod = 'I' or in6mod = 'M'
076800070319     c                   eval      $fine = *on
076900070319     c                   endif
077000070314     c                   leave
077100070315     c                   endif
077200070314     c                   endif
077300070315
077400070313     c                   enddo
077500070319     c                   eval      $immissione = *off
077600070313     c                   endsr
077700070315      *---------------------------------------------------------------*
077800070315      * sr_Contr * Controllo i dati inseriti nella videata immissione *
077900070315      *---------------------------------------------------------------*
078000070315     c     sr_contr      BEGSR
078100070315
078200140124     c                   eval      *in90 = *off
078300140204     c                   eval      *in50 = *off
078400140204     c                   eval      *in51 = *off
078500140204
078600131122      /free
078700140204         clear  V1Dcmd;
078800131122       //?Se immesso un comando a video
078900131122         IF  V1Ccmd <> *blanks;
079000131210       //?verifico il ? di ricerca
079100131210           If    %scan('?' : V1Ccmd) > 0  ;
079200140124             *in90 = *on;
079300140121             *in50 = *on;
079400131210             clear dCMD;
079500140121             clear TNTBCMDDS1;
079600140121             ICMDtip = 'C';
079700140121             clear kpjbu;
079800140121             kpjbu = tntbcmdds1;
079900140121             TNTBCMDR1  (kpjba);
080000140121             tntbcmdds1 =  kpjbu ;
080100140121             IF Ocmderr = *off;
080200140121                 V1Ccmd = OCMDcmd ;
080300140121                 V1Dcmd  = OCMDdes;
080400140124                 clear V1Cmsg ;
080500131210             else ;
080600131210                  V1Cmsg = msg(01);
080700131210                  *in28 = *on;
080800131210                  *in50 = *on;
080900131210                  leavesr;
081000131210             endif;
081100131210             leavesr;
081200131210           endif;
081300131210
081400131122       //?deve esistere in tabella CMD
081500131128           clear dCMD;
081600131122           clear TIBS02DS;
081700131122           T02mod = 'C';
081800131122           T02sif = knsif;
081900131122           T02cod = 'CMD';
082000131122           T02ke1 = V1Ccmd;
082100131122           TNTBE_RicercaControllo (kpjba : tibs02ds);
082200131128           IF  T02err <> *blanks;
082300131122             *in28 = *on;
082400131122             *in50 = *on;
082500131122             V1Cmsg = msg(01);
082600131122             leavesr;
082700131122           ENDIF;
082800131128
082900131128           dCMD = T02uni;
083000140124           V1Dcmd = §CMDdes;
083100140124       //?se il comando è utilizzabile solo per i ritiri ERRORE
083200140124          If §cmdtip = 'R' ;
083300140124             *in28 = *on;
083400140124             *in50 = *on;
083500140124             V1Cmsg = msg(02);
083600140124             leavesr;
083700140124           ENDIF;
083800131210       //?se il comando è diverso da quello precedente imposto SI nel comunicare i dati all'AUT
083900131210          IF V1Ccmd <> CAUcmd ;
084000131210             V1Caut = 'SI' ;
084100131210          ENDIF;
084200140204       //?se c'è la comunicazione devo avvisare sempre l'AUT
084300140204          IF  V1Caut <> 'SI';
084400140204             *in28 = *on;
084500140204             *in51 = *on;
084600140204             V1Cmsg = msg(04);
084700140204             leavesr;
084800140204          ENDIF;
084900140204
085000140204         ELSE;
085100140204
085200140204       //?se c'era il comando non posso toglierlo
085300140204          IF  CAUcmd <> *blanks;
085400140204             *in28 = *on;
085500140204             *in50 = *on;
085600140204             V1Cmsg = msg(03);
085700140204             leavesr;
085800140204          ENDIF;
085900140204       //?se non c'è il comando imposto nella descrizione 'variazione'
086000140204           V1Dcmd = 'VARIAZIONE';
086100131210
086200131122         ENDIF;
086300131122      /end-free
086400070315
086500131122     c                   endsr
086600070315
086700070313      *---------------------------------------------------------------*
086800070313      * sr_VISUAL  * Gestione della videata di visualizzazione        *
086900070313      *---------------------------------------------------------------*
087000070313     c     sr_visual     BEGSR
087100070313
087200070319    1c                   do        *hival
087300070319
087400131121     c                   write     lrn6t00
087500131121     c                   write     lrn6z02
087600131121     c                   exfmt     lrn6c02
087700070319
087800070319      * F12 ritorno
087900070319    2c                   if        *inkl = *on
088000070319     c                   leave
088100070319    2c                   endif
088200070319
088300070319      * f10 Immissione
088400070319    2c                   if        *inkj = *on
088500070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
088600070319      * comando 12 videata precedente
088700070319    3c                   if        $immissione = *off
088800070319     c                   exsr      SR_IMMISS
088900070319      * provengo dalla routine di immissione
089000070319   x3c                   else
089100070319     c                   leave
089200070319    3c                   endif
089300070319      * se viene dato f3 di fine lavoro nella videtata d immissione vado fuori
089400070319    3c                   if        $fine = *on
089500070319     c                   leave
089600070319   x3c                   else
089700070319      * ricarico il subfile e visualizzo di nuovo i dati
089800070319     c                   exsr      sr_carsbf
089900070319    3c                   endif
090000070319
090100070319    2c                   endif
090200070319     c
090300070319    1c                   enddo
090400070314
090500070313     c                   endsr
090600070314      *---------------------------------------------------------------*
090700070314      * sr_CARSBF  * Caricamento sub file storico                     *
090800070314      *---------------------------------------------------------------*
090900070314     c     sr_CARSBF     BEGSR
091000070314
091100070314      * Pulizia subfile
091200070314     c                   setoff                                       20  21
091300131121     c                   write     LRN6C02
091400070314     c                   seton                                        20  21
091500070314      * pulizia campi di comodo
091600070314     c                   clear                   W_arnpru
091700070314     c                   clear                   W_arndim
091800070314     c                   clear                   W_arnoim
091900070314
092000070314     c                   eval      *in39 = *off
092100070315
092200070315     c                   clear                   s01nrr
092300070315
092400070315      * verifico se ho dello storico da visualizzare (note con 'S')
092500070315      * se ne ho visualizzo il subfile altrimenti vado direttamente
092600070315      * nella videata dell'inserimento note ldv
092700070315
092800070319     c                   eval      W_gst = 'S'
092900070319     c     K06arn11      setll     Fiarn12l
093000070315
093100070315     c                   do        *hival
093200070315
093300070319     c     k06arn11      reade     Fiarn12l
093400070315
093500070319     c                   if        %eof(fiarn12l)
093600070315     c                   leave
093700070315     c                   endif
093800070315
093900070315     c                   exsr      Sr_Wrtsbf
094000070315
094100070315     c                   enddo
094200070314
094300070315     c                   clear                   W_arnpru
094400070315     c                   clear                   W_arndim
094500070315     c                   clear                   W_arnoim
094600070315
094700070314      * verifico se ho dello storico da visualizzare (note senza 'S')
094800070314      * se ne ho visualizzo il subfile altrimenti vado direttamente
094900070314      * nella videata dell'inserimento note ldv
095000070314
095100070319     c                   eval      W_gst = ' '
095200070319     c     K06arn11      setll     Fiarn12l
095300070314
095400070314     c                   do        *hival
095500070314
095600070319     c     k06arn11      reade     Fiarn12l
095700070314
095800070319     c                   if        %eof(fiarn12l)
095900070314     c                   leave
096000070314     c                   endif
096100070314
096200070315     c                   exsr      Sr_Wrtsbf
096300070315
096400070314     c                   enddo
096500070319
096600070319      * verifico se ho scritto almeno una riga nel subfile eseguo gestione del subfile
096700070319     c                   If        s01nrr > 1
096800070319     c                   eval      *in20 = *on
096900070319     c                   z-add     1             v1cnrr
097000070319     c                   else
097100070319     c                   eval      *in20 = *off
097200070319     c                   endif
097300070319
097400070314
097500070314     c                   ENDSR
097600070315
097700070315      *---------------------------------------------------------------*
097800070315      * sr_WRTSBF  * Scrittura subfile                                *
097900070315      *---------------------------------------------------------------*
098000070315     c     sr_WRTSBF     BEGSR
098100070315
098200070315      * verifico se si tratta dello stesso utente data ora immissione
098300070315     c                   If        W_arnpru <> arnpru or
098400070315     c                             W_arndim <> arndim or
098500070315     c                             W_arnoim <> arnoim
098600070315
098700070315      * scrivo una riga subfile di interruzione con i dati dell'utente che ha inserito
098800070315      * le note successive
098900070315     c                   clear                   wlbdat
099000070315     c                   eval      G02inv = arndim
099100070315     c                   eval      G02err = '3'
099200070315     c                   call      'XSRDA8'
099300070315     c                   parm                    WLBdat
099400070315     c                   z-add     G02dat        W_dim
099500070315
099600070319     c                   If        arngst =  ' '
099700070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
099800070315     c                                       + %editw(W_dim:'  /  /    ') +
099900070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
100000070315     c                   else
100100070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
100200070315     c                                       + %editw(W_dim:'  /  /    ') +
100300070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
100400070315     c                                       + ' Stampabili '
100500070315     c                   endif
100600070315      * reverse immage
100700070315     c                   eval      *in39 = *on
100800070315
100900070315     c                   add       1             s01nrr
101000131121     c                   write     lrn6s02
101100070315     c                   eval      *in39 = *off
101200070315      * salvo i campi
101300070315     c                   eval      W_arnpru =  arnpru
101400070315     c                   eval      W_arndim =  arndim
101500070315     c                   eval      W_arnoim =  arnoim
101600070315
101700070315     c                   endif
101800070315
101900070315     c                   add       1             s01nrr
102000070315     c                   movel     arnnob        v2snbo
102100131121     c                   write     lrn6s02
102200070315
102300070315     c                   endsr
102400070314      *---------------------------------------------------------------*
102500070314      * sr_GESSBF  * Caricamento sub file storico                     *
102600070314      *---------------------------------------------------------------*
102700070314     c     sr_GESSBF     BEGSR
102800070314
102900070314     c                   z-add     1             v1cnrr
103000070314     c                   do        *hival
103100070319
103200070319     c                   eval      $fine = *off
103300070319
103400131121     c                   write     lrn6t00
103500131121     c                   write     lrn6z02
103600131121     c                   exfmt     lrn6c02
103700070314
103800070314      * F12 ritorno
103900070314     c                   if        *inkl = *on
104000070314     c                   leave
104100070314     c                   endif
104200070314
104300070314      * f10 Immissione
104400070314     c                   if        *inkj = *on
104500070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
104600070319      * comando 12 videata precedente
104700070319     c                   if        $immissione = *off
104800070314     c                   exsr      SR_IMMISS
104900070316      * se non devo visualizzare il sbf vado fuori
105000070316     c                   if        $fine = *on
105100070316     c                   leave
105200070316     c                   endif
105300070319      * provengo dalla routine di immissione
105400070319     c                   else
105500070319     c                   leave
105600070319     c                   endif
105700070316
105800070314     c                   endif
105900070314     c
106000070314     c                   enddo
106100070314     c                   ENDSR
106200131122
106300131122      /free
106400131122       // ----------------------------------------------------------------------
106500131122       // Controllo distinta
106600131122       // ----------------------------------------------------------------------
106700131122       BEGSR  ctr_Dist;
106800131122
106900140304         wSItel = *off;
107000131122         wSIpda = *off;
107100131122         wNOpda = *off;
107200131122         wTESTpda = *off;
107300131122
107400131122       //?Aggancio ARB
107500131122         chain (IN6aas:IN6lnp:IN6nrs:IN6nsp) FNARB01L;
107600131122         IF  not %found(FNARB01L);
107700131122           leavesr;
107800131122         ENDIF;
107900131219       //?Bolla non in distinta
108000131219         IF ARBndc = 0 ;
108100131219           leavesr;
108200131219         ENDIF;
108300131122
108400131122       //?Aggancio il file delle distinte
108500131122         clear dDSTflr;
108600131122         wfgs_ifp = ARBifp;
108700140207         wnfv = ARBndc;
108800131122         chain (wfgs_ifp:4:ARBddc:ARBndc) FIDST08L;
108900131122         IF  %found(FIDST08L);
109000131122           dDSTflr = DSTflr;
109100131122         ENDIF;
109101140304         // -?Impostazione campi chiave?
109102140304         clear  keyFIAPD01;
109103140304         k_APDPDR = DSTpdr;
109104140304         k_APDtip = 'A';
109105140304
109106140304         // -?Verifica se Autista escluso da fatturazione es "c/servizi" non crea FICAU
109107140304         chain  %kds( keyFIAPD01 )  FIAPD000;
109108140304         if  Not %found(FIAPD01L);
109109140304           leavesr;
109110140304         endif;
109111140304         if APDpdd = 'S'or APDatb <> ' ';
109112140304           leavesr;
109113140304         endif;
109114140304
109115140304         wSItel = *on ;
109116140304
109700131122       //?Verifico se a PDA se a PDA in TEST o NO PDA
109800131122         SELECT;
109900131122           WHEN  (DSTpda = 'C' or DSTpda = 'E') and
110000140122                 (§DSTtstpda = ' ' or §DSTtstpda = 'O');
110100131122             wSIpda = *on;
110200131122
110300131122           WHEN  (DSTpda = 'E' or DSTpda = 'C') and
110400131122                 (§dsttstpda = 'E' or §dsttstpda = 'C');
110500131122             wTESTpda = *on;
110600131122           WHEN  (DSTpda = 'O' or DSTpda = 'N') and
110700140122                 (§dsttstpda = 'O' or §dsttstpda = ' ');
110800131122             wNOpda = *on;
110900131122         ENDSL;
111000131122
111100131122       ENDSR;
111200131122      /end-free
111300131122
111400070314      *---------------------------------------------------------------*
111500070314      * sr_CONFE   * Conferma scrittura note                          *
111600070314      *---------------------------------------------------------------*
111700070314     c     sr_confe      BEGSR
111800070314
111900070316      * se è stato inserito almeno una riga nota diversa da prima scrivo sul file altrimenti passo
112000070316      * oltre
112100131129      /free
112200131129         wOkArn = *off;
112300131129      /end-free
112400070316
112500070316     c                   If        v1cno1 <> v1hno1 or  v1cno2 <> v1hno2  or
112600070319     c                             v1cno3 <> v1hno3 or  v1cno4 <> v1hno4
112700070316     c
112800070314     c                   eval      $almenouna = *off
112900070314     c                   clear                   W_ultimopno
113000070314      * verifico se esistevano già 4 righe stampabili
113100070319     c                   eval      W_gst = 'S'
113200070319     c     K06arn11      setll     Fiarn11l
113300070314
113400070314     c                   do        *hival
113500070314
113600070319     c     k06arn11      reade     Fiarn11l
113700070314
113800070319     c                   if        %eof(fiarn11l)
113900070314     c                   leave
114000070314     c                   endif
114100070314
114200070316      * imposto il flag da stampabile a storico
114300070319     c                   clear                   arngst
114400070314     c                   update    Fiarn000
114500070314     c                   eval      $almenouna = *on
114600070314     c                   eval      W_ultimopno = arnpno
114700070314
114800070314     c                   enddo
114900070314      * scrivo le nuove righe
115000070314      * attenzione se non è stato scritto nulla devo almeno scrivere una riga vuota
115100070314      * se non è la prima volta che scrivo note
115200070314
115300070314      * recupero  ora
115400070314     c                   time                    w0140
115500070314     c                   movel     w0140         wora
115600070314      * verifico se esiste almeno una riga scritta
115700070314     c                   If        v1cno1 = *blanks and v1cno2 = *blanks and
115800070316     c                             v1cno3 = *blanks and v1cno4 = *blanks
115900070314      * verifico se è la prima volta che scrivo delle note non faccio nulla altrimenti
116000070316      * scrivo una riga vuota solo se non è uguale alla precedente
116100070316     c                   if        $almenouna = *on
116200070314     c                   eval      W_arnnob = v1cno1
116300070314     c                   exsr      SR_FIARN
116400070315     c                   else
116500070315      * se non c'è neppura una riga inserita do errore
116600070315     c                   eval      *in28 = *on
116700070315     c                   eval      v1cmsg = msg(7)
116800070314     c                   endif
116900070314
117000070314     c                   else
117100070314      * scrittura nota 1 se valorizzata
117200070314     c                   if        v1cno1 <> *blanks
117300070314     c                   eval      W_arnnob = v1cno1
117400070314     c                   exsr      SR_FIARN
117500070314     c                   endif
117600070314      * scrittura nota 2 se valorizzata
117700070314     c                   if        v1cno2 <> *blanks
117800070314     c                   eval      W_arnnob = v1cno2
117900070314     c                   exsr      SR_FIARN
118000070314     c                   endif
118100070314      * scrittura nota 3 se valorizzata
118200070314     c                   if        v1cno3 <> *blanks
118300070314     c                   eval      W_arnnob = v1cno3
118400070314     c                   exsr      SR_FIARN
118500070314     c                   endif
118600070314      * scrittura nota 4 se valorizzata
118700070314     c                   if        v1cno4 <> *blanks
118800070314     c                   eval      W_arnnob = v1cno4
118900070314     c                   exsr      SR_FIARN
119000070314     c                   endif
119100070314
119200070319     c                   feod      fiarn11l
119300070314     c                   endif
119400070314
119500070316     c                   endif
119600131122
119700131219      /free
119800140205       //?Se non ho variato note e/o comunicazione e non ho ricevuto una variazione vado via
119900140205         IF  not wokarn and V1Ccmd = CAUcmd and IN6var = *blanks;
120000140204           leavesr;
120100140204         ENDIF;
120200140204
120300131219       //?Verifico la distinta a PDA
120400131219       exsr CTR_dist;
120500131127       //?Scrivo file delle telefonate all'AUT se:
120600131219       //?bolla in consegna e non sono stata richiamata da chiusura distinta e filiale partita
120700131219       //?con telefonate AUT
120800131219         IF  IN6cons = 'S' and  IN6Chdist = ' ' and wSITel = *on and
120900131219         //?e richiesto invio dati all'AUT
121000131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
121100131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
121200131219         //?                           SCRIVO
121300131219               V1Caut = 'SI';
121400131128             clear FNLRX3DS;
121500131128             IX3tpdoc = 'C';
121600131128             IX3iddoc = %editc(IN6lnp:'X') + %editc(IN6nrs:'X') +
121700131128                        %editc(IN6nsp:'X') + %editc(IN6aas:'X');
121800131128             IX3fgs = ARBifp;
121900131128             IX3nfv = ARBndc;
122000131128             IX3dfv = ARBddc;
122100131128             IX3pdr = ARBpdc;
122200131128             IX3var = IN6var;
122300131128             IX3cmd = V1Ccmd;
122400131219         //?immesse note
122500131219             IF  wOkArn ;
122600131206               IX3var  = %trim(IX3var)+ 'N';
122700131129             ENDIF;
122800131128             fnlrx3r (kpjba:fnlrx3ds);
122900131122         ENDIF;
123000131122
123100131127       //?Scrivo file delle dati a PDA se:
123200131127       //?bolla in consegna
123300131127         IF  IN6cons = 'S' and
123400131128         //?distinta a PDA (test e non test)
123500131128             (wSIpda or wTESTpda) and
123600131219         //? e richiesto invio dati all'AUT
123700131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
123800131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
123900131219         //?                           CHIAMO FIDG42R
124000140206              V1Caut <> 'NO' ;
124100131127           clear FIDG42DS;
124200131127           CO42aas = IN6aas;
124300131127           CO42lnp = IN6lnp;
124400131127           CO42nrs = IN6nrs;
124500131127           CO42nsp = IN6nsp;
124600131127           CO42fgs = ARBifp;
124700131127           CO42ndc = ARBndc;
124800131127           CO42ddc = ARBddc;
124900131127           kpjbu = FIDG42DS;
125000131127           fidg42r (kpjba);
125100131122         ENDIF;
125200131122      /end-free
125300070316
125400070314     c                   ENDSR
125500070314      *---------------------------------------------------------------*
125600070314      * sr_FIARN   * Imposto i campi per la scrittura di FIARN        *
125700070314      *---------------------------------------------------------------*
125800070314     c     sr_FIARN      BEGSR
125900131129
126000131129      /free
126100131129         wOkArn = *on;
126200131129      /end-free
126300070314
126400070314     c                   clear                   fiarn000
126500070314     c                   eval      arnaas = vtcaas
126600070314     c                   eval      arnlnp = vtclnp
126700070314     c                   eval      arnnrs = vtcnrs
126800070314     c                   eval      arnnsp = vtcnsp
126900070319     c                   eval      arngst = 'S'
127000070314     c                   eval      W_ultimopno = W_ultimopno + 1
127100070314     c                   eval      arnpno = W_ultimopno
127200070314     c                   eval      arnnob = W_arnnob
127300070314     c                   eval      arnpru = dutute
127400070314     c                   eval      arndim = dateu
127500070314     c                   eval      arnoim = Wora
127600070314      * programma di immissione
127700070314      *                         se è richiamato prendo quello della ds
127800070314      *                         altrimenti noi stessi
127900131121     c                   eval      arnpgm = in6pgm
128000070319
128100070319     c                   eval      arncdn = 'LDV'
128200070314
128300070314     c                   write     fiarn000
128400070314
128500070314     c                   ENDSR
128600070404      *---------------------------------------------------------------*
128700070404      * sr_COPIA   * Copia note R.A.  su note LDV libere              *
128800070404      *---------------------------------------------------------------*
128900070404     c     sr_copia      BEGSR
129000070404      * se richiesta la copia delle note R.a. su note ldv prima verifico
129100070404      * se c'è posto in aggiunta alle note già esistenti
129200070404     c                   select
129300070404      * tutte occupate
129400070404     c                   when      v1cno4 <> *blanks
129500070404     c                   eval      *in28 = *on
129600070404     c                   eval      v1cmsg = msg(9)
129700070404      * se la prima nota è vuota copio tutte e 4
129800070404     c                   when      v1cno1 = *blanks
129900070404     c                   eval      v1cno1 = vtcno1
130000070404     c                   eval      v1cno2 = vtcno2
130100070404     c                   eval      v1cno3 = vtcno3
130200070404     c                   eval      v1cno4 = vtcno4
130300070404      * se la seconda nota è vuota copio solo sulle ultime 3
130400070404     c                   when      v1cno2 = *blanks
130500070404     c                   eval      v1cno2 = vtcno1
130600070404     c                   eval      v1cno3 = vtcno2
130700070404     c                   eval      v1cno4 = vtcno3
130800070404      * se la terza   nota è vuota copio solo sulle ultime 2
130900070404     c                   when      v1cno3 = *blanks
131000070404     c                   eval      v1cno3 = vtcno1
131100070404     c                   eval      v1cno4 = vtcno2
131200070404      * se la quarta  nota è vuota copio solo sull'ultima
131300070404     c                   when      v1cno4 = *blanks
131400070404     c                   eval      v1cno4 = vtcno1
131500070404
131600070404     c                   endsl
131700070404     c                   endsr
131800070314
131900070308** MSG errore
132000140204Comunicazione errata                                                            1
132100140204Comunicazione non utlizzabile per le spedizioni                                 2
132200140204Non è possibile togliere una comunicazione                                      3
132300140204Obbligo di avvisare AUT se c'è comunicazione                                    4
132400131128....libero                                                                      5
132500070315Spedizione inesistente o in gestione ad altro P.O.                              6
132600070315E' stato dato F6 di conferma senza scrivere nulla                               7
132700070316Gestione non abilitata                                                          8
132800070404Non è possibile copiare/aggiungere Note R.A. alle Note LDV già esistenti        9
