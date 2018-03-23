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
001600140304     fFIapd01l  if   e           k disk
001700140204     fFIARP01L  if   e           k disk
001800070313      *
001900131121     fFNLRN6D   cf   e             workstn sfile(LRN6S02:S01nrr)
002000131202     f                                     maxdev(*file)
002100070313
002200070313      *---------------------------------------------------------------*
002300070313
002400070313      *
002500070313      *   C O S T A N T I   - - - - - - - - - - - - - - - - - - - - - *
002600070308      *
002700070313      *
002800070313      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
002900070313      *
003000070404     d MSG             S             78    DIM(009) CTDATA PERRCD(1)            MSG VIDEO
003100070313      *
003200070313     d L6              s              3  0 dim(30)                              po £6
003300131120     D wpog            s              3    dim(250)
003400070313      *
003500070313      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
003600131219      *
003700070313     d KPJBA         e ds
003800130304      *
003900131121     D ficauds       E DS                  extname(FICAU00F)
004000130304     D fnarbds       E DS                  extname(fnarb00f)
004100070313      * - Parametri
004200131121     d FNLRN6DS      e ds
004300070313      *
004400070313      * - Parametri x Controllo profilo utenti
004500070313     d TIBS34DS      e ds
004600131120      * - Tabelle
004700131120     d TIBS02ds      e ds                  inz
004800070313      * - Ds di riferimento al file esterno AZUTE00F
004900070313     d AZUTEDS       e ds                  extname(AZUTE00F)
005000070313      * - Ds per dati organigramma
005100070313     d DDatiUte      e ds
005200070315      * abilitazioni
005300070315     D OG148         E DS
005400070313      * DS per TRUL06R - Caricamento £X
005500070313     d TRUL06DS      E DS
005600070313     d  LIN                    1     90  0
005700070313     d                                     DIM(30)
005800131120
005900131120     D TRUL31DS      E DS
006000131120     d dLat          e ds
006100131120     d dute01        e ds
006200070313      *
006300130304      * DS per Fidg42R - passaggio dati a PDA
006400130304     d fidg42ds      e ds
006500131122
006600131122     d ddstflr       e ds
006700131128
006800131128       // Ds passaggio dati a pgm FNLRX3R
006900131128     d fnlrx3ds      e ds
007000131128
007100140121       // Ds passaggio dati a pgm TNTBCMDR
007200140121     d tntbcmdds1    e ds
007300140121
007400131128       // Tabella CMD
007500131128     d dCMD          e ds
007600131122
007700130304      *
007800070313     d Status         sds
007900070313     d  SDSpgm           *proc
008000070313     d  PARMS            *parms
008100070313      *
008200070313     d WLBdat          ds                  inz
008300070313     d  G02dat                 1      8  0 inz
008400070313     d  G02inv                 9     16  0 inz
008500070313     d  G02err                17     17    inz('3')
008600070313     d  G02tgi                18     22  0 inz
008700070315      *
008800070313      *
008900070313      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009000070313      *
009100070313      * - Campi di appoggio per il subfile
009200070313     d S01nrr          s                   like(V1Cnrr)   inz
009300070313      *
009400070313      * - Campi di salvataggio temporaneo dati
009500070313      *
009600070313      * - Flags booleani
009700070313     d $Fine           s              1    inz(*off)
009800070313     d $Sped           s              1    inz(*off)
009900070313     d $InzS01         s              1    inz(*on)
010000070314     d $almenouna      s              1    inz(*off)
010100070315     d $abilitato      s              1    inz(*off)
010200070319     d $immissione     s              1    inz(*off)
010300131219     d wSItel          s               n   inz(*off)
010400131122     d wSIpda          s               n   inz(*off)
010500131122     d wNOpda          s               n   inz(*off)
010600131122     d wTESTpda        s               n   inz(*off)
010700131127     d wEoF            s               n   inz(*off)
010800131127     d wF03            s               n   inz(*off)
010900131129     d wOkArn          s               n   inz(*off)
011000070313      *
011100070313      * - Indici di schiera / Contatori
011200070313     d xx              s              3  0 inz
011300070313      *
011400070313      * - Campi di comodo
011500070313     d w0140           s             14  0
011600070314     d wData           s              8  0
011700070314     d DateU           s              8  0
011800070314     d wora            s              6  0
011900131120     d w003a           s              3
012000070308     d Keyaas          s                   like(arbaas)
012100070308     d Keylnp          s                   like(arblnp)
012200070308     d Keynrs          s                   like(arbnrs)
012300070308     d Keynsp          s                   like(arbnsp)
012400070309     d codut           s                   like(tblkut) inz(1)
012500070309     d Kcod            s                   like(tblcod)
012600070309     d Kkey            s                   like(tblkey)
012700070312     d Ktrc            s                   like(ar4trc)
012800070319     d W_gst           s                   like(arngst)
012900070319     d W_cdn           s                   like(arncdn) inz('LDV')
013000070314     d W_dim           s                   like(arndim)
013100070314     d W_arnpru        s                   like(arnpru)
013200070314     d W_arndim        s                   like(arndim)
013300070314     d W_arnoim        s                   like(arnoim)
013400070314     d W_arnnob        s                   like(arnnob)
013500070314     d W_ultimopno     s                   like(arnpno)
013600131120     d wabi            s                   like(UteAut)
013700131121     d woggetto        s                   like(CAUogg)
013800131122     d wfgs_ifp        s                   like(arblnp)
013900131122     d wnpg            s                   like(DSTnpg) inz(4)
014000140204     d wtcr            s                   like(ARPtcr)
014100140204     d wdcr            s                   like(ARBdcr)
014200140303     d wDate_EUR       s               d   datfmt(*eur)  inz(*loval)
014300131122
014400131122       //--------------------------------------------------------------
014500131122       //?Definizione prototipi e procedure.
014600131122       //--------------------------------------------------------------
014700131122
014800131122      /copy gaitrasrc/srcprotopr,tibs02r
014900140121      /copy gaitrasrc/srcprotopr,tntbcmdr1
015000131122
015100131122     d fidg42r         pr                  extpgm('FIDG42R')
015200131122     d  kpjba                              likeds(kpjba)
015300131128
015400131128     d fnlrx3r         pr                  extpgm('FNLRX3R')
015500131128     d  kpjba                              likeds(kpjba)
015600131128     d  fnlrx3ds                           likeds(fnlrx3ds)
015700131122
015800131122       //--------------------------------------------------------------
015900140304       //--------------------------------------------------------------
016000140304       //?Definizione key-list.                                        ?
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
022600070314
022700070313      * spedizione non trovata errore e fine
022800070314
022900131127     c                   If        $sped = *off
023000131121     c                   movel     MSG(6)        On6msg
023100131121     c                   eval      on6err = 'E'
023200131127     c                   goto      fine
023300070319     c                   endif
023400070319
023500070313      * recupero dati dal file
023600070313     c                   exsr      SR_FNARB
023700131122      /free
023800131122       //?recupero comando dal file
023900131122       exsr sr_FICAU;
024000140204       //?Il flag di comunicazione AUT lo propongo sempre = 'SI'
024100140204       V1Caut = 'SI';
024200140204       //?se non c'è il comando imposto nella descrizione 'variazione'
024300140204       IF  V1Ccmd = *blanks;
024400140204         V1Dcmd = 'VARIAZIONE';
024500140204       ENDIF;
024600140204       //?Cerco se la spedizione in consegna ha la data consegna richiesta
024700140204       IF  IN6cons = 'S';
024800140204         clear wtcr;
024900140204         clear wdcr;
025000140204         exsr sr_DCR;
025100140204         //?Se c'è la data consegna richiesta e non è per oggi
025200140204         //?e non c'è la comunicazione all'AUT
025300140204         IF  V1Ccmd = *blanks and
025400140204            (wtcr = 'D' and wdcr >= dateu or
025500140204             wtcr = *blanks and wdcr > dateu);
025600140204         //?Non devo comunicare niente
025700140204           V1Caut = 'NO';
025800140204         ENDIF;
025900140204       ENDIF;
026000131122      /end-free
026100131121
026200070308      * gestione delle routine in caso di pgm richiamato per la sola immissione e nel caso
026300070308      * di visualizzazione e manutenzione
026400070308     c                   select
026500070308
026600131121     c                   when      in6mod = 'V'
026700070319      * carico il subfile
026800070319     c                   exsr      sr_CARSBF
026900070319
027000070316     c                   exsr      sr_VISUAL
027100131121     c                   goto      fine
027200070308
027300070308     c                   other
027400070316
027500070316     c                   exsr      sr_IMMISS
027600070319
027700070315      * se viene dato f3 di fine non proseguo
027800070319
027900070319     c                   If        $fine = *on
028000131127     c                   goto      fine
028100070319     c                   endif
028200070308
028300070308     c                   endsl
028400070319
028500070313     c     Fine          tag
028600131127
028700070313      *
028800070313     c                   eval      *inLR   = *on
028900070313
029000070313      *---------------------------------------------------------------*
029100070313      * Routine Iniziale                                              *
029200070313      *---------------------------------------------------------------*
029300070313     c     *InzSR        BegSR
029400070313      *
029500070313      * Reperisco dati job
029600070313     c                   exsr      DatiJob
029700070313     c                   movel     SDSpgm        VTCpgm
029800131120
029900131120      * Imposto le abilitazioni dell'utente
030000131120     c                   eval      dute01 = UTEfaf
030100131120
030200070314      * Reperisco data corrente
030300070314     c                   time                    w0140
030400070314     c                   move      w0140         wData
030500070314     c                   clear                   WLBdat
030600070314     c                   eval      G02dat = wData
030700070314     c                   call      'XSRDA8'
030800070314     c                   parm                    WLBdat
030900070314     c                   z-add     G02inv        DateU
031000070313      *
031100070313     c                   EndSr
031200070315      *---------------------------------------------------------------*
031300070315      * Impostazioni Iniziali                                         *
031400070315      *---------------------------------------------------------------*
031500070315     c     SR_imposta    BegSR
031600070315      *
031700070315     c     *Entry        plist
031800131129     c                   parm                    KPJBA
031900131121     c                   parm                    FNLRN6DS
032000070315
032100070315      * carico la £6
032200070315     c                   exsr      sr_carl6
032300070315
032400070315      * visualizzazione
032500131121     c                   if        in6mod = 'V'
032600070316     c                   eval      *in72 = *on
032700070315     c                   endif
032800131121
032900070319      * imposto i campi della chiave della spedizione
033000131121     c                   eval      vtcaas = in6aas
033100131121     c                   eval      vtclnp = in6lnp
033200131121     c                   eval      vtcnrs = in6nrs
033300131121     c                   eval      vtcnsp = in6nsp
033400131122
033500140206       //?Verifico la distinta a PDA
033600140207     c                   IF        IN6cons = 'S'
033700140206     C                   exsr      CTR_dist
033800140207     c                   ENDIF
033900131219      * visualizzo la videata dei comandi se bolla in consegna , filiale distinta partita con le
034000131219      * telefnate ad aut e se non sono stato richiamato dalla chiusura distinta
034100131219     c                   eval      *in41 = IN6cons = 'S' and IN6chdist = ' ' and
034200140226     c                             wSITel = *on
034300070315
034400070315     c                   EndSr
034500070315
034600070313      *---------------------------------------------------------------*
034700070313      * Reperimento Dati del job (Utente/Operativi)                   *
034800070313      *---------------------------------------------------------------*
034900070313     c     DatiJob       BegSr
035000070313      *
035100070313     c     *dtaara       define    §azute        azuteds
035200070313     c     *dtaara       define    §datiute      ddatiute
035300070313      *
035400070313     c                   in(E)     *dtaara
035500070313     c                   IF        %ERROR or RSUT = *blanks
035600070313     c                   clear                   Tibs34Ds
035700070313     c                   call      'TIBS34R'
035800070313     c                   parm                    Tibs34Ds
035900070313     c                   in        *dtaara
036000070313     c                   ENDIF
036100070313      *
036200070313     c                   EndSr
036300070313
036400070313      *---------------------------------------------------------------*
036500070313      * Recupero i P.O.  della £6                                     *
036600070313      *---------------------------------------------------------------*
036700070313     c     Sr_CARL6      begsr
036800070313
036900070313      * Recupero i PO della £6
037000070313      *
037100070313     c                   clear                   trul06ds
037200070313     c                   move      '£6'          d06cod
037300070313     c                   movel     Dutpou        d06key
037400070313     c                   movel(p)  trul06ds      KPJBU
037500070313
037600070313     c                   call      'TRUL06R'
037700070313     c                   parm                    kpjba
037800070313     c                   movel     kpjbu         trul06ds
037900070313     c                   movea     lin           L6
038000070313
038100070315      * verico se PO abilitato alla gestione telefonate GEO
038200070315
038300070315     c                   clear                   og148
038400070315     c     l6(1)         chain     azorg01l
038500070315     c                   if        %found(azorg01l)
038600070315     c                   movel     orgde8        og148
038700070315     c                   endif
038800070315
038900070315     c                   If        §oggeot = 'S'
039000070315     c                   eval      $abilitato = *on
039100070316     c                   eval      *in71      = *on
039200070315     c                   endif
039300131120
039400131120      /free
039500131120       //?Imposto abilitazione utente
039600131120         clear wpog;
039700131120         SELECT;
039800131120       //?Imposto abilitazione manutenzione bolle di filiale
039900131120           WHEN  §UTEmbf <> *blanks;
040000131120             wabi = §UTEmbf;
040100131120           WHEN  UTEaut <> *blanks;
040200131120             wabi = UTEaut;
040300131120           WHEN  DUTlpo = '1';
040400131120             wabi = 'TP';
040500131120           WHEN  DUTlpo = '2';
040600131120             wabi = 'PO';
040700131120         ENDSL;
040800131120      /end-free
040900131120
041000131120      * controllo se ok l'abilitazione dell'utente
041100131120     c                   clear                   TIBS02DS
041200131120     c                   clear                   dLAT
041300131120     c                   eval      T02mod = 'C'
041400131120     c                   eval      T02sif = knsif
041500131120     c                   eval      T02cod = 'LAT'
041600131120     c                   movel(p)  wabi          T02ke1
041700131120     c                   call      'TIBS02R'
041800131120     c                   parm                    kpjba
041900131120     c                   parm                    TIBS02DS
042000131120if  1c                   IF        T02err = *blanks
042100131120     c                   eval      dLAT = T02uni
042200131120e   1c                   ENDIF
042300131120
042400131120      * Se abilitazione trovata e OK
042500131120if  1c                   IF        T02err = *blanks and §LATabi = *blanks
042600131120      * Reperimento dei P.O. gestibili dall'utente
042700131120     c                   clear                   TRUL31DS
042800131120     c                   eval      I31abi = wabi
042900131120     c                   eval      I31cdi = DUTdis
043000131120     c                   eval      I31car = DUTare
043100131120     c                   eval      I31cpo = DUTpou
043200131120     c                   call      'TRUL31R'
043300131120     c                   parm                    kpjba
043400131120     c                   parm                    TRUL31DS
043500131120if  2c                   IF        O31pog > *zeros
043600131120     c                   movea     O31pog        wpog
043700131120x   2c                   ENDIF
043800131120    1c                   ENDIF
043900131120
044000131121      * se sk delle filiali abilitate all'utente è vuota, imposto il primo elemento della £6
044100131120     c                   IF        wpog(1) = *blanks
044200131120     c                   eval      wpog(1) = %editc(l6(1):'X')
044300131120     c                   ENDIF
044400131120
044500070313     c                   endsr
044600070313      *---------------------------------------------------------------*
044700070313      * sr_ctrarb  * controllo se spedizione corretta                 *
044800070313      *---------------------------------------------------------------*
044900070313     c     sr_ctrarb     BEGSR
045000070313
045100070313     c                   eval      $sped = *off
045200070313
045300070313     c     k04arb01      chain     Fnarb01l
045400070313
045500070412    1c                   If        %found(fnarb01l)
045600131122      * verifico se la linea di arrivo è presente nelle linee abilitate alla manut. bolle filiale
045700131120     c                   move      arblna        w003a
045800131122     c                   IF        %lookup (w003a:wPOG) > 0
045900070313     c                   eval      $sped = *on
046000070412    2c                   endif
046100131122      * se non abilitata
046200131122    2c                   if        %lookup (w003a:wpog) = 0
046300070412      * disabilito il comando di scrittura note
046400131121    4c                   if        in6mod = 'V'
046500070322     c                   eval      $sped = *on
046600131122      * e disattivo f10 di inserimento
046700070322     c                   eval      *in74 = *on
046800070412    4c                   endif
046900070412    2c                   endif
047000070412
047100070412    1c                   endif
047200070313
047300070313     c                   endsr
047400070313
047500070313      *---------------------------------------------------------------*
047600070313      * sr_clr_W_not   * Pulizia videata richiesta note               *
047700070313      *---------------------------------------------------------------*
047800070313     c     sr_clr_W_not  BEGSR
047900070313
048000131121     c                   clear                   vtcns1
048100131121     c                   clear                   vtcns2
048200131121     c                   clear                   vtccmd
048300131121     c                   clear                   vtcdtacmd
048400131121     c                   clear                   vtcoracmd
048500131121     c                   clear                   vtcutecmd
048600131121     c                   clear                   v1ccmd
048700131121     c                   clear                   v1dcmd
048800070313     c                   clear                   v1cno1
048900070313     c                   clear                   v1cno2
049000070313     c                   clear                   v1cno3
049100070313     c                   clear                   v1cno4
049200070316     c                   clear                   v1hno1
049300070316     c                   clear                   v1hno2
049400070316     c                   clear                   v1hno3
049500070316     c                   clear                   v1hno4
049600070316     c                   clear                   vtcdim
049700070313     c                   clear                   vtcoim
049800070313     c                   clear                   vtcpru
049900131121     c                   clear                   v1caut
050000070313     c                   eval      *in40 = *off
050100070313     c                   eval      *in42 = *off
050200131121     c                   eval      *in43 = *off
050300070313
050400070313     c                   endsr
050500070313
050600070309      *---------------------------------------------------------------*
050700070309      * sr_FNARB   * Recupero informazioni da bolle arrivi            *
050800070309      *---------------------------------------------------------------*
050900070309     c     sr_FNARB      BEGSR
051000070309
051100070313      * routine di pulizia del video
051200070313
051300070313     c                   exsr      SR_clr_W_not
051400070313
051500070313      * data spedizione
051600070313     c                   clear                   wlbdat
051700070313     c                   movel(p)  arbaas        g02inv
051800070313     c                   move      arbmgs        g02inv
051900070314     c                   eval      G02err = '3'
052000070313     c                   call      'XSRDA8'
052100070313     c                   parm                    WLBdat
052200070313     c                   z-add     G02dat        vtcdta
052300070312      * note spedizione
052400070312
052500070313     c                   eval      ktrc = '8'
052600070313     c     K05ar401      chain     fiar401l
052700070312     c                   if        %found(fiar401l)
052800070312     c                   eval      vtcns1 = Ar4not
052900070312     c                   endif
053000070312
053100070313     c                   eval      ktrc = '9'
053200070313     c     K05ar401      chain     fiar401l
053300070312     c                   if        %found(fiar401l)
053400070312     c                   eval      vtcns2 = Ar4not
053500070312     c                   endif
053600070313      *
053700070313      * Verifico se sono in manutenzione e recupero le ultime note stampabili
053800070309      *
053900070319     c                   eval      W_gst = 'S'
054000070319     c     K06arn11      setll     Fiarn11l
054100070313
054200070313     c                   do        *hival
054300070313
054400070319     c     k06arn11      reade(n)  Fiarn11l
054500070314
054600070319     c                   if        %eof(fiarn11l)
054700070314     c                   leave
054800070314     c                   endif
054900070313
055000070313      * imposto i dati dell'ultimo utente che ha inserito la/e nota/e stampabile/i
055100070313
055200070313     c                   if        not *in42
055300070316      * valorizzo indicatore per gestione f11 storico
055400070316     c                   eval      *in73 = *on
055500070313
055600070313     c                   eval      *in42 = *on
055700070313     c                   eval      vtcpru = arnpru
055800070313     c                   eval      vtcoim = arnoim
055900070313
056000070313     c                   clear                   wlbdat
056100070313     c                   eval      G02inv = arndim
056200070314     c                   eval      G02err = '3'
056300070313     c                   call      'XSRDA8'
056400070313     c                   parm                    WLBdat
056500070313     c                   z-add     G02dat        vtcdim
056600070313
056700070313     c                   endif
056800070313
056900070313     c                   select
057000070313
057100070313     c                   when      v1cno1 = *blanks
057200070313     c                   eval      v1cno1 = arnnob
057300070313
057400070313     c                   when      v1cno2 = *blanks
057500070313     c                   eval      v1cno2 = arnnob
057600070313
057700070313     c                   when      v1cno3 = *blanks
057800070313     c                   eval      v1cno3 = arnnob
057900070313
058000070313     c                   other
058100070313     c                   eval      v1cno4 = arnnob
058200070313
058300070313     c                   endsl
058400070313
058500070313     c                   enddo
058600070313      * verifico se mi hanno passato  degli appunti
058700070313
058800131121     c                   if        in6not1 <> *blanks
058900131121     c                   eval      vtcno1 = in6not1
059000070313     c                   eval      *in43 = *on
059100070313     c                   endif
059200070313
059300131121     c                   eval      vtcno2 = in6not2
059400131121     c                   eval      vtcno3 = in6not3
059500131121     c                   eval      vtcno4 = in6not4
059600070313
059700070313     c                   endsr
059800131121
059900131122      /free
060000131122       // ----------------------------------------------------------------------
060100131122       // Recupero il comando dal file delle telefonate AUT
060200131122       // ----------------------------------------------------------------------
060300131122       BEGSR  sr_FICAU;
060400131122
060500131122         woggetto = %editc(ARBlnp:'X') +
060600131122                    %editc(ARBnrs:'X') +
060700131122                    %editc(ARBnsp:'X') +
060800131122                    %editc(ARBaas:'X');
060900131122
061000131122       //?Preparo il cursore
061100131122         exec sql
061200131122         DECLARE CAU cursor FOR
061300131122         SELECT * from FICAU00F
061400131122         WHERE CAUtor = 'C' and CAUogg = :woggetto
061500131122           and CAUfgs = :IN6fgs and CAUdfv = :IN6dfv
061600131129           and CAUnfv = :IN6nfv and CAUpdr = :IN6pdr
061700131122           and CAUatb = ' '
061800131122         ORDER by  CAUdtoins;
061900131121
062000131122       //?Apro il cursore
062100131122         exec sql
062200131122         OPEN  CAU;
062300131129
062400131129         IF sqlcode < 0;
062500131129           wEoF = *on;
062600131129         ENDIF;
062700131122
062800131122       //?leggo il file
062900131127         DOW  not wEoF;
063000131122          exec sql
063100131122          FETCH next from CAU into :FICAUDS;
063200131122
063300131122       //?fine file o errore sql esco
063400131122           IF   sqlcod = 100 or sqlcod < 0;
063500131127            wEoF = *on;
063600131122            leave;
063700131122           ENDIF;
063800131122
063900131122       //?comando
064000131122           IF  CAUdescmd <> *blanks;
064100131210             V1Ccmd = CAUcmd;
064200131211             V1Dcmd = CAUdescmd;
064300131122             VTCcmd = CAUdescmd;
064400131122             *in40 = *on;
064500140303             wdate_eur = %date(%dec(%subst(CAUdtoins:1:8):8:0));
064600140303             vtcdtacmd = %dec( Wdate_eur );
064700131122             VTCoracmd = %dec(%subst(CAUdtoins:9:6):6:0);
064800131122             VTCutecmd = CAUpruins;
064900131122           ENDIF;
065000131127           wEoF = *on;
065100131122           leave;
065200131122         ENDDO;
065300131121
065400131122       //?Chiudo il cursoe
065500131122         exec sql
065600131122         close CAU;
065700131122
065800131122       ENDSR;
065900140204
066000140204       // ----------------------------------------------------------------------
066100140204       // Recupero la data consegna richiesta.
066200140204       // ----------------------------------------------------------------------
066300140204       BEGSR  sr_DCR;
066400140204
066500140204       //?Cerco se la data è su FIARP
066600140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FIARP01L;
066700140204         IF  %found(FIARP01L);
066800140204           wtcr = ARPtcr;
066900140204           wdcr = ARPdcr;
067000140204           leavesr;
067100140204         ENDIF;
067200140204
067300140204       //?Non la trovo su ARP la cerco su FNARB
067400140204         chain (VTCaas:VTClnp:VTCnrs:VTCnsp) FNARB01L;
067500140204         IF  %found(FNARB01L);
067600140204           wtcr = ARbtcr;
067700140204           wdcr = ARbdcr;
067800140204           leavesr;
067900140204         ENDIF;
068000140204
068100140204       ENDSR;
068200140204
068300131122      /end-free
068400070313
068500070313      *---------------------------------------------------------------*
068600070313      * sr_IMMISS  * Gestione della videata di immissione             *
068700070313      *---------------------------------------------------------------*
068800070313     c     sr_IMMISS     BEGSR
068900070313
069000070319     c                   eval      $immissione = *on
069100070418      * mi salvo le righe di note per non scrivere delle note doppie
069200070418     c                   eval      v1hno1 = v1cno1
069300070418     c                   eval      v1hno2 = v1cno2
069400070418     c                   eval      v1hno3 = v1cno3
069500070418     c                   eval      v1hno4 = v1cno4
069600070313      *
069700070313     c                   do        *hival
069800070319     c                   eval      $fine = *off
069900131127      /free
070000131127       wF03 = *off;
070100131127      /end-free
070200070315
070300131121     c                   write     lrn6t00
070400131121     c                   write     lrn6z01
070500070313
070600131202     c                   write     lrn6d01
070700131202     c                   read(e)   fnlrn6d
070800070315     c                   eval      *in28 = *off
070900131202     c                   IF        %error
071000131202     c                   eval      *inkc = *on
071100131202     c                   ENDIF
071200131122
071300070313
071400070313      * f3 - Fine lavoro
071500070313     c                   if        *inkc
071600070315     c                   eval      $fine = *on
071700131127      /free
071800131127       wF03 = *on;
071900131127      /end-free
072000070313     c                   leave
072100070313     c                   endif
072200070315
072300070404      * f2 - Copia note R.A.
072400070404     c                   if        *inkb
072500070404     c                   exsr      SR_copia
072600070404     c                   iter
072700070404     c                   endif
072800070404
072900070316      * F11 gestione dello storico
073000070403     c                   if        *inkk
073100070403     c* Se provengo da videata di visulizzazione, esco
073200070319      * carico il subfile
073300131121     c                   if        in6mod<>'V'
073400070319     c                   exsr      sr_CARSBF
073500070319
073600070316     c                   exsr      SR_VISUAL
073700070319     c                   iter
073800070403     c                   else
073900070403     c                   leave
074000070403     c                   endif
074100070316     c                   endif
074200070316
074300070315     c                   exsr      SR_contr
074400070315
074500070315      * ci sono degli errori
074600140124     c                   if        *in28 or *in90
074700070315     c                   iter
074800070315     c                   endif
074900131122
075000070314      * F06 conferma
075100070314     c                   if        *inkf
075200070314     c                   exsr      SR_confe
075300131121     c                   if        *in28
075400070315     c                   iter
075500070315     c                   else
075600131121      * se immissione o manutenzione vado a fine pgm
075700131121     c                   if        in6mod = 'I' or in6mod = 'M'
075800070319     c                   eval      $fine = *on
075900070319     c                   endif
076000070314     c                   leave
076100070315     c                   endif
076200070314     c                   endif
076300070315
076400070313     c                   enddo
076500070319     c                   eval      $immissione = *off
076600070313     c                   endsr
076700070315      *---------------------------------------------------------------*
076800070315      * sr_Contr * Controllo i dati inseriti nella videata immissione *
076900070315      *---------------------------------------------------------------*
077000070315     c     sr_contr      BEGSR
077100070315
077200140124     c                   eval      *in90 = *off
077300140204     c                   eval      *in50 = *off
077400140204     c                   eval      *in51 = *off
077500140204
077600131122      /free
077700140204         clear  V1Dcmd;
077800131122       //?Se immesso un comando a video
077900131122         IF  V1Ccmd <> *blanks;
078000131210       //?verifico il ? di ricerca
078100131210           If    %scan('?' : V1Ccmd) > 0  ;
078200140124             *in90 = *on;
078300140121             *in50 = *on;
078400131210             clear dCMD;
078500140121             clear TNTBCMDDS1;
078600140121             ICMDtip = 'C';
078700140121             clear kpjbu;
078800140121             kpjbu = tntbcmdds1;
078900140121             TNTBCMDR1  (kpjba);
079000140121             tntbcmdds1 =  kpjbu ;
079100140121             IF Ocmderr = *off;
079200140121                 V1Ccmd = OCMDcmd ;
079300140121                 V1Dcmd  = OCMDdes;
079400140124                 clear V1Cmsg ;
079500131210             else ;
079600131210                  V1Cmsg = msg(01);
079700131210                  *in28 = *on;
079800131210                  *in50 = *on;
079900131210                  leavesr;
080000131210             endif;
080100131210             leavesr;
080200131210           endif;
080300131210
080400131122       //?deve esistere in tabella CMD
080500131128           clear dCMD;
080600131122           clear TIBS02DS;
080700131122           T02mod = 'C';
080800131122           T02sif = knsif;
080900131122           T02cod = 'CMD';
081000131122           T02ke1 = V1Ccmd;
081100131122           TNTBE_RicercaControllo (kpjba : tibs02ds);
081200131128           IF  T02err <> *blanks;
081300131122             *in28 = *on;
081400131122             *in50 = *on;
081500131122             V1Cmsg = msg(01);
081600131122             leavesr;
081700131122           ENDIF;
081800131128
081900131128           dCMD = T02uni;
082000140124           V1Dcmd = §CMDdes;
082100140124       //?se il comando è utilizzabile solo per i ritiri ERRORE
082200140124          If §cmdtip = 'R' ;
082300140124             *in28 = *on;
082400140124             *in50 = *on;
082500140124             V1Cmsg = msg(02);
082600140124             leavesr;
082700140124           ENDIF;
082800131210       //?se il comando è diverso da quello precedente imposto SI nel comunicare i dati all'AUT
082900131210          IF V1Ccmd <> CAUcmd ;
083000131210             V1Caut = 'SI' ;
083100131210          ENDIF;
083200140204       //?se c'è la comunicazione devo avvisare sempre l'AUT
083300140204          IF  V1Caut <> 'SI';
083400140204             *in28 = *on;
083500140204             *in51 = *on;
083600140204             V1Cmsg = msg(04);
083700140204             leavesr;
083800140204          ENDIF;
083900140204
084000140204         ELSE;
084100140204
084200140204       //?se c'era il comando non posso toglierlo
084300140204          IF  CAUcmd <> *blanks;
084400140204             *in28 = *on;
084500140204             *in50 = *on;
084600140204             V1Cmsg = msg(03);
084700140204             leavesr;
084800140204          ENDIF;
084900140204       //?se non c'è il comando imposto nella descrizione 'variazione'
085000140204           V1Dcmd = 'VARIAZIONE';
085100131210
085200131122         ENDIF;
085300131122      /end-free
085400070315
085500131122     c                   endsr
085600070315
085700070313      *---------------------------------------------------------------*
085800070313      * sr_VISUAL  * Gestione della videata di visualizzazione        *
085900070313      *---------------------------------------------------------------*
086000070313     c     sr_visual     BEGSR
086100070313
086200070319    1c                   do        *hival
086300070319
086400131121     c                   write     lrn6t00
086500131121     c                   write     lrn6z02
086600131121     c                   exfmt     lrn6c02
086700070319
086800070319      * F12 ritorno
086900070319    2c                   if        *inkl = *on
087000070319     c                   leave
087100070319    2c                   endif
087200070319
087300070319      * f10 Immissione
087400070319    2c                   if        *inkj = *on
087500070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
087600070319      * comando 12 videata precedente
087700070319    3c                   if        $immissione = *off
087800070319     c                   exsr      SR_IMMISS
087900070319      * provengo dalla routine di immissione
088000070319   x3c                   else
088100070319     c                   leave
088200070319    3c                   endif
088300070319      * se viene dato f3 di fine lavoro nella videtata d immissione vado fuori
088400070319    3c                   if        $fine = *on
088500070319     c                   leave
088600070319   x3c                   else
088700070319      * ricarico il subfile e visualizzo di nuovo i dati
088800070319     c                   exsr      sr_carsbf
088900070319    3c                   endif
089000070319
089100070319    2c                   endif
089200070319     c
089300070319    1c                   enddo
089400070314
089500070313     c                   endsr
089600070314      *---------------------------------------------------------------*
089700070314      * sr_CARSBF  * Caricamento sub file storico                     *
089800070314      *---------------------------------------------------------------*
089900070314     c     sr_CARSBF     BEGSR
090000070314
090100070314      * Pulizia subfile
090200070314     c                   setoff                                       20  21
090300131121     c                   write     LRN6C02
090400070314     c                   seton                                        20  21
090500070314      * pulizia campi di comodo
090600070314     c                   clear                   W_arnpru
090700070314     c                   clear                   W_arndim
090800070314     c                   clear                   W_arnoim
090900070314
091000070314     c                   eval      *in39 = *off
091100070315
091200070315     c                   clear                   s01nrr
091300070315
091400070315      * verifico se ho dello storico da visualizzare (note con 'S')
091500070315      * se ne ho visualizzo il subfile altrimenti vado direttamente
091600070315      * nella videata dell'inserimento note ldv
091700070315
091800070319     c                   eval      W_gst = 'S'
091900070319     c     K06arn11      setll     Fiarn12l
092000070315
092100070315     c                   do        *hival
092200070315
092300070319     c     k06arn11      reade     Fiarn12l
092400070315
092500070319     c                   if        %eof(fiarn12l)
092600070315     c                   leave
092700070315     c                   endif
092800070315
092900070315     c                   exsr      Sr_Wrtsbf
093000070315
093100070315     c                   enddo
093200070314
093300070315     c                   clear                   W_arnpru
093400070315     c                   clear                   W_arndim
093500070315     c                   clear                   W_arnoim
093600070315
093700070314      * verifico se ho dello storico da visualizzare (note senza 'S')
093800070314      * se ne ho visualizzo il subfile altrimenti vado direttamente
093900070314      * nella videata dell'inserimento note ldv
094000070314
094100070319     c                   eval      W_gst = ' '
094200070319     c     K06arn11      setll     Fiarn12l
094300070314
094400070314     c                   do        *hival
094500070314
094600070319     c     k06arn11      reade     Fiarn12l
094700070314
094800070319     c                   if        %eof(fiarn12l)
094900070314     c                   leave
095000070314     c                   endif
095100070314
095200070315     c                   exsr      Sr_Wrtsbf
095300070315
095400070314     c                   enddo
095500070319
095600070319      * verifico se ho scritto almeno una riga nel subfile eseguo gestione del subfile
095700070319     c                   If        s01nrr > 1
095800070319     c                   eval      *in20 = *on
095900070319     c                   z-add     1             v1cnrr
096000070319     c                   else
096100070319     c                   eval      *in20 = *off
096200070319     c                   endif
096300070319
096400070314
096500070314     c                   ENDSR
096600070315
096700070315      *---------------------------------------------------------------*
096800070315      * sr_WRTSBF  * Scrittura subfile                                *
096900070315      *---------------------------------------------------------------*
097000070315     c     sr_WRTSBF     BEGSR
097100070315
097200070315      * verifico se si tratta dello stesso utente data ora immissione
097300070315     c                   If        W_arnpru <> arnpru or
097400070315     c                             W_arndim <> arndim or
097500070315     c                             W_arnoim <> arnoim
097600070315
097700070315      * scrivo una riga subfile di interruzione con i dati dell'utente che ha inserito
097800070315      * le note successive
097900070315     c                   clear                   wlbdat
098000070315     c                   eval      G02inv = arndim
098100070315     c                   eval      G02err = '3'
098200070315     c                   call      'XSRDA8'
098300070315     c                   parm                    WLBdat
098400070315     c                   z-add     G02dat        W_dim
098500070315
098600070319     c                   If        arngst =  ' '
098700070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
098800070315     c                                       + %editw(W_dim:'  /  /    ') +
098900070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
099000070315     c                   else
099100070315     c                   eval      v2snbo = 'Inserite da ' + arnpru + ' il '
099200070315     c                                       + %editw(W_dim:'  /  /    ') +
099300070315     c                                      ' alle ' + %editw(arnoim:'  :  :  ')
099400070315     c                                       + ' Stampabili '
099500070315     c                   endif
099600070315      * reverse immage
099700070315     c                   eval      *in39 = *on
099800070315
099900070315     c                   add       1             s01nrr
100000131121     c                   write     lrn6s02
100100070315     c                   eval      *in39 = *off
100200070315      * salvo i campi
100300070315     c                   eval      W_arnpru =  arnpru
100400070315     c                   eval      W_arndim =  arndim
100500070315     c                   eval      W_arnoim =  arnoim
100600070315
100700070315     c                   endif
100800070315
100900070315     c                   add       1             s01nrr
101000070315     c                   movel     arnnob        v2snbo
101100131121     c                   write     lrn6s02
101200070315
101300070315     c                   endsr
101400070314      *---------------------------------------------------------------*
101500070314      * sr_GESSBF  * Caricamento sub file storico                     *
101600070314      *---------------------------------------------------------------*
101700070314     c     sr_GESSBF     BEGSR
101800070314
101900070314     c                   z-add     1             v1cnrr
102000070314     c                   do        *hival
102100070319
102200070319     c                   eval      $fine = *off
102300070319
102400131121     c                   write     lrn6t00
102500131121     c                   write     lrn6z02
102600131121     c                   exfmt     lrn6c02
102700070314
102800070314      * F12 ritorno
102900070314     c                   if        *inkl = *on
103000070314     c                   leave
103100070314     c                   endif
103200070314
103300070314      * f10 Immissione
103400070314     c                   if        *inkj = *on
103500070319      * se non provengo dalla routine di immissione  vado nella routine altrimenti mi comporto come
103600070319      * comando 12 videata precedente
103700070319     c                   if        $immissione = *off
103800070314     c                   exsr      SR_IMMISS
103900070316      * se non devo visualizzare il sbf vado fuori
104000070316     c                   if        $fine = *on
104100070316     c                   leave
104200070316     c                   endif
104300070319      * provengo dalla routine di immissione
104400070319     c                   else
104500070319     c                   leave
104600070319     c                   endif
104700070316
104800070314     c                   endif
104900070314     c
105000070314     c                   enddo
105100070314     c                   ENDSR
105200131122
105300131122      /free
105400131122       // ----------------------------------------------------------------------
105500131122       // Controllo distinta
105600131122       // ----------------------------------------------------------------------
105700131122       BEGSR  ctr_Dist;
105800131122
105900140304         wSItel = *off;
106000131122         wSIpda = *off;
106100131122         wNOpda = *off;
106200131122         wTESTpda = *off;
106300131122
106400131122       //?Aggancio ARB
106500131122         chain (IN6aas:IN6lnp:IN6nrs:IN6nsp) FNARB01L;
106600131122         IF  not %found(FNARB01L);
106700131122           leavesr;
106800131122         ENDIF;
106900131219       //?Bolla non in distinta
107000160223         //IF ARBndc = 0 ;
107100160223         IF ARBndc = 0  or  ARBndc = *all'8'  or  ARBndc = *all'9';
107200131219           leavesr;
107300131219         ENDIF;
107400131122
107500131122       //?Aggancio il file delle distinte
107600131122         clear dDSTflr;
107700131122         wfgs_ifp = ARBifp;
107800131122         chain (wfgs_ifp:4:ARBddc:ARBndc) FIDST08L;
107900131122         IF  %found(FIDST08L);
108000131122           dDSTflr = DSTflr;
108100131122         ENDIF;
108200140304         // -?Impostazione campi chiave?
108300140304         clear  keyFIAPD01;
108400140304         k_APDPDR = DSTpdr;
108500140304         k_APDtip = 'A';
108600140304
108700140304         // -?Verifica se Autista escluso da fatturazione es "c/servizi" non crea FICAU
108800140304         chain  %kds( keyFIAPD01 )  FIAPD000;
108900140304         if  Not %found(FIAPD01L);
109000140304           leavesr;
109100140304         endif;
109200140304         if APDpdd = 'S'or APDatb <> ' ';
109300140304           leavesr;
109400140304         endif;
109500140304
109600140304         wSItel = *on ;
109700140304
109800131122       //?Verifico se a PDA se a PDA in TEST o NO PDA
109900131122         SELECT;
110000131122           WHEN  (DSTpda = 'C' or DSTpda = 'E') and
110100140122                 (§DSTtstpda = ' ' or §DSTtstpda = 'O');
110200131122             wSIpda = *on;
110300131122
110400131122           WHEN  (DSTpda = 'E' or DSTpda = 'C') and
110500131122                 (§dsttstpda = 'E' or §dsttstpda = 'C');
110600131122             wTESTpda = *on;
110700131122           WHEN  (DSTpda = 'O' or DSTpda = 'N') and
110800140122                 (§dsttstpda = 'O' or §dsttstpda = ' ');
110900131122             wNOpda = *on;
111000131122         ENDSL;
111100131122
111200131122       ENDSR;
111300131122      /end-free
111400131122
111500070314      *---------------------------------------------------------------*
111600070314      * sr_CONFE   * Conferma scrittura note                          *
111700070314      *---------------------------------------------------------------*
111800070314     c     sr_confe      BEGSR
111900070314
112000070316      * se è stato inserito almeno una riga nota diversa da prima scrivo sul file altrimenti passo
112100070316      * oltre
112200131129      /free
112300131129         wOkArn = *off;
112400131129      /end-free
112500070316
112600070316     c                   If        v1cno1 <> v1hno1 or  v1cno2 <> v1hno2  or
112700070319     c                             v1cno3 <> v1hno3 or  v1cno4 <> v1hno4
112800070316     c
112900070314     c                   eval      $almenouna = *off
113000070314     c                   clear                   W_ultimopno
113100070314      * verifico se esistevano già 4 righe stampabili
113200070319     c                   eval      W_gst = 'S'
113300070319     c     K06arn11      setll     Fiarn11l
113400070314
113500070314     c                   do        *hival
113600070314
113700070319     c     k06arn11      reade     Fiarn11l
113800070314
113900070319     c                   if        %eof(fiarn11l)
114000070314     c                   leave
114100070314     c                   endif
114200070314
114300070316      * imposto il flag da stampabile a storico
114400070319     c                   clear                   arngst
114500070314     c                   update    Fiarn000
114600070314     c                   eval      $almenouna = *on
114700070314     c                   eval      W_ultimopno = arnpno
114800070314
114900070314     c                   enddo
115000070314      * scrivo le nuove righe
115100070314      * attenzione se non è stato scritto nulla devo almeno scrivere una riga vuota
115200070314      * se non è la prima volta che scrivo note
115300070314
115400070314      * recupero  ora
115500070314     c                   time                    w0140
115600070314     c                   movel     w0140         wora
115700070314      * verifico se esiste almeno una riga scritta
115800070314     c                   If        v1cno1 = *blanks and v1cno2 = *blanks and
115900070316     c                             v1cno3 = *blanks and v1cno4 = *blanks
116000070314      * verifico se è la prima volta che scrivo delle note non faccio nulla altrimenti
116100070316      * scrivo una riga vuota solo se non è uguale alla precedente
116200070316     c                   if        $almenouna = *on
116300070314     c                   eval      W_arnnob = v1cno1
116400070314     c                   exsr      SR_FIARN
116500070315     c                   else
116600070315      * se non c'è neppura una riga inserita do errore
116700070315     c                   eval      *in28 = *on
116800070315     c                   eval      v1cmsg = msg(7)
116900070314     c                   endif
117000070314
117100070314     c                   else
117200070314      * scrittura nota 1 se valorizzata
117300070314     c                   if        v1cno1 <> *blanks
117400070314     c                   eval      W_arnnob = v1cno1
117500070314     c                   exsr      SR_FIARN
117600070314     c                   endif
117700070314      * scrittura nota 2 se valorizzata
117800070314     c                   if        v1cno2 <> *blanks
117900070314     c                   eval      W_arnnob = v1cno2
118000070314     c                   exsr      SR_FIARN
118100070314     c                   endif
118200070314      * scrittura nota 3 se valorizzata
118300070314     c                   if        v1cno3 <> *blanks
118400070314     c                   eval      W_arnnob = v1cno3
118500070314     c                   exsr      SR_FIARN
118600070314     c                   endif
118700070314      * scrittura nota 4 se valorizzata
118800070314     c                   if        v1cno4 <> *blanks
118900070314     c                   eval      W_arnnob = v1cno4
119000070314     c                   exsr      SR_FIARN
119100070314     c                   endif
119200070314
119300070319     c                   feod      fiarn11l
119400070314     c                   endif
119500070314
119600070316     c                   endif
119700131122
119800131219      /free
119900140205       //?Se non ho variato note e/o comunicazione e non ho ricevuto una variazione vado via
120000140205         IF  not wokarn and V1Ccmd = CAUcmd and IN6var = *blanks;
120100140204           leavesr;
120200140204         ENDIF;
120300140204
120400131219       //?Verifico la distinta a PDA
120500131219       exsr CTR_dist;
120600131127       //?Scrivo file delle telefonate all'AUT se:
120700131219       //?bolla in consegna e non sono stata richiamata da chiusura distinta e filiale partita
120800131219       //?con telefonate AUT
120900131219         IF  IN6cons = 'S' and  IN6Chdist = ' ' and wSITel = *on and
121000131219         //?e richiesto invio dati all'AUT
121100131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
121200131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
121300131219         //?                           SCRIVO
121400131219               V1Caut = 'SI';
121500131128             clear FNLRX3DS;
121600131128             IX3tpdoc = 'C';
121700131128             IX3iddoc = %editc(IN6lnp:'X') + %editc(IN6nrs:'X') +
121800131128                        %editc(IN6nsp:'X') + %editc(IN6aas:'X');
121900131128             IX3fgs = ARBifp;
122000131128             IX3nfv = ARBndc;
122100131128             IX3dfv = ARBddc;
122200131128             IX3pdr = ARBpdc;
122300131128             IX3var = IN6var;
122400131128             IX3cmd = V1Ccmd;
122500131219         //?immesse note
122600131219             IF  wOkArn ;
122700131206               IX3var  = %trim(IX3var)+ 'N';
122800131129             ENDIF;
122900131128             fnlrx3r (kpjba:fnlrx3ds);
123000131122         ENDIF;
123100131122
123200131127       //?Scrivo file delle dati a PDA se:
123300131127       //?bolla in consegna
123400131127         IF  IN6cons = 'S' and
123500131128         //?distinta a PDA (test e non test)
123600131128             (wSIpda or wTESTpda) and
123700131219         //? e richiesto invio dati all'AUT
123800131219         //?                           NON MI INTERESSA SE SONO STATE INSERITE DELLE NOTE O DEI
123900131219         //?                           COMANDI O SONO STATO  RICHIAMATO DA DELLE VARIAZIONI
124000131219         //?                           CHIAMO FIDG42R
124100140206              V1Caut <> 'NO' ;
124200131127           clear FIDG42DS;
124300131127           CO42aas = IN6aas;
124400131127           CO42lnp = IN6lnp;
124500131127           CO42nrs = IN6nrs;
124600131127           CO42nsp = IN6nsp;
124700131127           CO42fgs = ARBifp;
124800131127           CO42ndc = ARBndc;
124900131127           CO42ddc = ARBddc;
125000131127           kpjbu = FIDG42DS;
125100131127           fidg42r (kpjba);
125200131122         ENDIF;
125300131122      /end-free
125400070316
125500070314     c                   ENDSR
125600070314      *---------------------------------------------------------------*
125700070314      * sr_FIARN   * Imposto i campi per la scrittura di FIARN        *
125800070314      *---------------------------------------------------------------*
125900070314     c     sr_FIARN      BEGSR
126000131129
126100131129      /free
126200131129         wOkArn = *on;
126300131129      /end-free
126400070314
126500070314     c                   clear                   fiarn000
126600070314     c                   eval      arnaas = vtcaas
126700070314     c                   eval      arnlnp = vtclnp
126800070314     c                   eval      arnnrs = vtcnrs
126900070314     c                   eval      arnnsp = vtcnsp
127000070319     c                   eval      arngst = 'S'
127100070314     c                   eval      W_ultimopno = W_ultimopno + 1
127200070314     c                   eval      arnpno = W_ultimopno
127300070314     c                   eval      arnnob = W_arnnob
127400070314     c                   eval      arnpru = dutute
127500070314     c                   eval      arndim = dateu
127600070314     c                   eval      arnoim = Wora
127700070314      * programma di immissione
127800070314      *                         se è richiamato prendo quello della ds
127900070314      *                         altrimenti noi stessi
128000131121     c                   eval      arnpgm = in6pgm
128100070319
128200070319     c                   eval      arncdn = 'LDV'
128300070314
128400070314     c                   write     fiarn000
128500070314
128600070314     c                   ENDSR
128700070404      *---------------------------------------------------------------*
128800070404      * sr_COPIA   * Copia note R.A.  su note LDV libere              *
128900070404      *---------------------------------------------------------------*
129000070404     c     sr_copia      BEGSR
129100070404      * se richiesta la copia delle note R.a. su note ldv prima verifico
129200070404      * se c'è posto in aggiunta alle note già esistenti
129300070404     c                   select
129400070404      * tutte occupate
129500070404     c                   when      v1cno4 <> *blanks
129600070404     c                   eval      *in28 = *on
129700070404     c                   eval      v1cmsg = msg(9)
129800070404      * se la prima nota è vuota copio tutte e 4
129900070404     c                   when      v1cno1 = *blanks
130000070404     c                   eval      v1cno1 = vtcno1
130100070404     c                   eval      v1cno2 = vtcno2
130200070404     c                   eval      v1cno3 = vtcno3
130300070404     c                   eval      v1cno4 = vtcno4
130400070404      * se la seconda nota è vuota copio solo sulle ultime 3
130500070404     c                   when      v1cno2 = *blanks
130600070404     c                   eval      v1cno2 = vtcno1
130700070404     c                   eval      v1cno3 = vtcno2
130800070404     c                   eval      v1cno4 = vtcno3
130900070404      * se la terza   nota è vuota copio solo sulle ultime 2
131000070404     c                   when      v1cno3 = *blanks
131100070404     c                   eval      v1cno3 = vtcno1
131200070404     c                   eval      v1cno4 = vtcno2
131300070404      * se la quarta  nota è vuota copio solo sull'ultima
131400070404     c                   when      v1cno4 = *blanks
131500070404     c                   eval      v1cno4 = vtcno1
131600070404
131700070404     c                   endsl
131800070404     c                   endsr
131900070314
132000070308** MSG errore
132100140204Comunicazione errata                                                            1
132200140204Comunicazione non utlizzabile per le spedizioni                                 2
132300140204Non è possibile togliere una comunicazione                                      3
132400140204Obbligo di avvisare AUT se c'è comunicazione                                    4
132500131128....libero                                                                      5
132600070315Spedizione inesistente o in gestione ad altro P.O.                              6
132700070315E' stato dato F6 di conferma senza scrivere nulla                               7
132800070316Gestione non abilitata                                                          8
132900070404Non è possibile copiare/aggiungere Note R.A. alle Note LDV già esistenti        9
