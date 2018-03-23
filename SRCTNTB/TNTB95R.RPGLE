000100041206      *---------------------------------------------------------------*
000200120424      * Gestione tabella "3BC" = Bypass ctrl x cliente                *
000300041206      *---------------------------------------------------------------*
000400041206
000500091211     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600091211     h dftactgrp(*no)
000700091211     h bnddir('UBRTVNETA')
000800041206
000900041206      *---------------------------------------------------------------*
001000041206      *   A R C H I V I                                               *
001100041206      *---------------------------------------------------------------*
001200041206      *
001300041206     fTNTBE01L  uf a e           k disk
001400091211     f                                     extfile(wLibFile)  usropn
001500041206      *
001600120424     fTNTB95D   cf   e             workstn
001700120424     f                                     infds(InfDspF)
001800041206
001900041206      *---------------------------------------------------------------*
002000041206      *   C O S T A N T I                                             *
002100041206      *---------------------------------------------------------------*
002200041206      *
002300091211       // -?Costanti per la definizione delle schiere con i nomi?
002400091211       //  ?degli iSeries da elaborare e delle relative librerie?
002500091211     d c_NrSyst        c                   const(2)
002600091211     d c_NrLibr        c                   const(2)
002700120424
002800120424       // - Tabella in gestione
002900120424     d c_Tab           c                   const('3BC')
003000120424
003100120424       // - Costante per controllo "caratteri solo numerici"
003200120424     d c_Digits        c                   const('0123456789')
003300041206
003400041206      *---------------------------------------------------------------*
003500041206      *   S C H I E R E                                               *
003600041206      *---------------------------------------------------------------*
003700041206      *
003800091211       // -?iSeries  &  Librerie con entrambi i file tabelle?
003900091211     d $iSystem        s                   like(currSysNetA)
004000091211     d                                     dim(c_NrSyst)
004100091211     d                                     ctdata   perrcd( 1)
004200091211     d $Libraries      s                   like(ds_Libr)
004300091211     d                                     dim(c_NrSyst)
004400091211     d                                     alt($iSystem)
004500091211      *
004600041206     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
004700050601     d $Msg            s             78    dim(06) ctdata perrcd(1)             Messaggi video
004800041206
004900041206      *---------------------------------------------------------------*
005000041206      *   S T R U T T U R E   D A T I                                 *
005100041206      *---------------------------------------------------------------*
005200041206      *
005300041206      * Parametri
005400041206     d KPJBA         e ds
005500041206      *
005600041206      * Passaggio Parametri al pgm TIBS02R
005700041206     d TIBS02DS      e ds                  inz
005800041206      *
005900120424      * Tabella 3BC = Bypass ctrl x cliente
006000120424     d d3BC          e ds                  inz
006100060627      * Tabella TSD = Tipo Stampa DDT
006200060627     d dtsd          e ds                  inz
006300041206      *
006400041206      * Tracciato record file TNTBE00F
006500041206     d TNTBEds       e ds                  extname(TNTBE00F) inz
006600041206     d xTNTBEds      e ds                  extname(TNTBE00F) inz
006700041206     d                                     prefix(TBX:3)
006800041206      *
006900041206     d TIBS34DS      e ds                  inz
007000120424
007100120424       // - Controllo/Decodifica cliente
007200120424     d TIBS69ds      e ds                  qualified  inz
007300120424     d ds_CNACO      e ds                  extname(CNACO00F)
007400120424     d                                     qualified  inz
007500120424     d ds_CNIND      e ds                  extname(CNIND00F)
007600120424     d                                     qualified  inz
007700120424     d ds_CNCLP      e ds                  extname(CNCLP00F)
007800120424     d                                     qualified  inz
007900120424     d ds_FNCLS      e ds                  extname(FNCLS00F)
008000120424     d                                     qualified  inz
008100120424
008200041206     d DDATIUTE      e ds                  inz
008300041206     d AZUTEDS       e ds                  extname(AZUTE00F) inz
008400120424
008500120424       // - Struttura per passaggio dati ad interrogazione tabella
008600120424     d Param01         ds                  inz
008700120424     d  P01cod                        7  0 inz
008800120424     d  P01ord                        1    inz
008900120424     d  P01ksu                        7  0 inz
009000120424     d  P01ke1                        7    inz
009100120424     d  P01ke2                       15    inz
009200120424     d  P01rit                        1    inz
009300041206      *
009400041206     d WLBDAT          ds                  inz
009500041206     d  G02dat                 1      8  0 inz
009600041206     d  G02inv                 9     16  0 inz
009700041206     d  G02err                17     17    inz
009800041206     d  G02tgi                18     22  0 inz
009900091211       //
010000091211       // -?Ridefinizione elenco librerie in elaborare le tabelle?
010100091211     d ds_Libr         ds                  inz
010200091211     d  $Libr                        10    dim(c_NrLibr) inz
010300041206      *
010400041206     d                sds
010500041206     d  VTCpgm           *proc
010600120424
010700120424       // - InfDS
010800120424     d InfDspF         ds
010900120424     d  dsp_aid              369    369a                                        AID byte
011000041206
011100041206      *---------------------------------------------------------------*
011200041206      *   V A R I A B I L I                                           *
011300041206      *---------------------------------------------------------------*
011400041206      *
011500091211      * - Flags booleani
011600041206     d $Fine           s              1a   inz(*off)
011700041206     d $CarV1          s              1a   inz(*on)
011800041206     d $CarV2          s              1a   inz(*off)
011900091211     d $Tibs02         s              1a   inz(*off)
012000091211      * - Variabili per la gestione del video
012100041206     d $TipVid         s              1a   inz('1')
012200091211     d wTasto          s              2a   inz(*zeros)
012300091211     d wPos            s              2  0
012400091211      * - Parametri per QCMDEXC
012500041206     d Qcmd            s             80    inz
012600041206     d Qlen            s             15  5 inz(80)
012700091211       //
012800091211       // -?Nome del sistema?
012900091211     d currSysNeta     s              8a   inz
013000091211       // -?Nome esteso Libreria/File del file tabella?
013100091211     d wLibFile        s             21a   inz
013200091211       // -?Campi di comodo?
013300091211     d w_iSystem       s              1  0 inz
013400091211     d w_SisInf        s              3  0 inz
013500120424
013600120424       //--------------------------------------------------------------
013700120424       //?Definizione key-list.                                        ?
013800120424       //--------------------------------------------------------------
013900120424
014000120424       // - File TNTBE01L
014100120424     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
014200120424     d                                     prefix(k_)  inz
014300120424
014400091211
014500091211       //--------------------------------------------------------------
014600091211       //?Definizione prototipi procedure.                             ?
014700091211       //--------------------------------------------------------------
014800091211
014900091211       // -?Reperimento NETA sistema AS/400 corrente?
015000091211      /copy gaitrasrc/srcProtoPr,UBRTVNETA
015100120424
015200120424       // - Controllo/Decodifica cliente
015300120424      /copy gaitrasrc/srcProtoPR,TIBS69R
015400120424
015500120424       // - Ricerca/Controllo tabelle
015600120424      /copy gaitrasrc/srcProtoPR,TIBS02R
015700041206
015800041206      *---------------------------------------------------------------*
015900041206      *   M A I N   L I N E                                           *
016000041206      *---------------------------------------------------------------*
016100041206      *  Riepilogo indicatori utilizzati:                             *
016200041206      *  --------------------------------                             *
016300041206      *  01 - Record inesistente (inserimento)                        *
016400041206      *  02 - Record esistente   (modifica)                           *
016500041206      *  04 - Record annullato   (ripristino)                         *
016600041206      *  20 - Comodo                                                  *
016700041206      *  22 - Errori in scrittura record (WRITE)                      *
016800120424      *  50 - 98 Errori
016900041206      *  99 - Visualizzazione messaggio di errore                     *
017000041206      *---------------------------------------------------------------*
017100041206      *
017200041206      * Operazioni iniziali
017300041206     c                   exsr      RutInz
017400041206      *
017500041206      * Gestione video
017600041206     c                   dow       $Fine = *off
017700041206     c     $TipVid       caseq     '1'           GesV1
017800041206     c     $TipVid       caseq     '2'           GesV2
017900041206     c                   endcs
018000041206     c                   enddo
018100041206      *
018200041206      * Fine
018300041206     c                   if        $Tibs02 = *on
018400041206     c                   clear                   TIBS02ds
018500041206     c                   movel     'C'           T02tla
018600041206     c                   call      'TIBS02R'
018700041206     c                   parm                    KPJBA
018800041206     c                   parm                    TIBS02ds
018900041206     c                   endif
019000041206      *
019100091211     c***                return
019200041206
019300041206      *---------------------------------------------------------------*
019400041206      * RutInz - Operazioni Iniziali                                  *
019500041206      *---------------------------------------------------------------*
019600041206     c     RutInz        BEGSR
019700041206      *
019800041206      * Ricezione parametri
019900041206     c     *entry        plist
020000041206     c                   parm                    KPJBA
020100041206      *
020200041206      * Definizioni chiavi di accesso
020300041206     c     K05TBE01      klist                                                  *tntbe01l
020400041206     c                   kfld                    TBEcod                         -tabella
020500041206     c                   kfld                    TBEke1                         -chiave uno
020600041206     c                   kfld                    TBEke2                         -chiave due
020700041206     c                   kfld                    TBElin                         -lingua
020800041206     c                   kfld                    TBEsif                         -s.informativo
020900091211      /free
021000091211
021100091211         *inLR = *on;
021200091211
021300091211         // -?Verifica del sistema AS/400 corrente?
021400091211         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
021500091211           $Fine = *on;
021600091211           leavesr;
021700091211         endif;
021800091211
021900091211         // -?Impostazione elenco librerie in cui gestire le tabelle?
022000091211         //  ?(a seconda del sistema in cui si stà lavorando)?
022100091211         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
022200091211         if  w_iSystem = *zero;
022300091211           $Fine = *on;
022400091211           leavesr;
022500091211         endif;
022600091211
022700091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
022800091211         w_SisInf = 1;
022900091211         exsr  sr_OpenFileTab;
023000091211
023100091211      /end-free
023200041206      *
023300041206      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
023400041206     c     *dtaara       define    §azute        azuteds
023500041206     c     *dtaara       define    §datiute      ddatiute
023600041206      *
023700041206     c                   clear                   AzUteDs
023800041206     c                   clear                   DDatiUte
023900041206     c                   clear                   Tibs34Ds
024000041206     c                   in(E)     *dtaara
024100041206if  1c                   if        %Error  or  RSUT = *blanks
024200041206     c                   call      'TIBS34R'
024300041206     c                   parm                    Tibs34Ds
024400041206     c                   in        *dtaara
024500041206e   1c                   endif
024600041206      *-- Verifica errori e autorità profilo
024700041206sel 1c                   SELECT
024800041206      *-- controllo se ho errori nei dati utente
024900041206      *--   nel qual caso NON risulta un profilo abilitato
025000041206w   1c                   WHEN      DUTerr = 'E'
025100041206     c                   eval      $Fine  = *on
025200041206      *
025300041206      *-- CONTROLLO AUTORITA'
025400041206      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
025500041206      *-- se il chiamante non richiede autorità specifica verificare
025600041206      *--   quella del profilo
025700041206      *-- se il chiamante richiede autorità specifica verificarla,
025800041206      *--  se è blank verificare quella del profilo
025900041206      *
026000041206      * se UTEAUT = *BLANK non siamo in sede
026100041206w   1c                   WHEN      UTEaut = *blank
026200041206      *
026300041206x   1c                   OTHER
026400041206      *
026500041206e   1c                   ENDSL
026600041206      *
026700041206      * Aggancio dati generali della tabella in esame
026800041206     c                   clear                   TBEcod
026900041206     c                   move      *zeros        TBEke1
027000120424     c                   move      '3BC'         TBEke1
027100041206     c                   clear                   TBEke2
027200041206     c                   clear                   TBElin
027300041206     c                   movel     KNSIF         TBEsif
027400091211     c     K05TBE01      chain(n)  TNTBE01L
027500041206     c                   if        not %found(TNTBE01L)
027600041206     c                   clear                   TBEsif
027700091211     c     K05TBE01      chain(n)  TNTBE01L
027800041206     c                   endif
027900041206     c                   if        %found(TNTBE01L)
028000041206     c                   movel     TNTBEds       xTNTBEds
028100041206     c                   else
028200041206     c                   clear                   xTNTBEds
028300041206     c                   endif
028400041206      *
028500120424     c                   clear                   TB95V1
028600041206      *
028700041206     c                   ENDSR
028800091211      /free
028900091211
029000091211       //--------------------------------------------------------------
029100091211       //?Apertura dei files tabelle nel sistema informativo impostato.?
029200091211       //--------------------------------------------------------------
029300091211       BEGSR  sr_OpenFileTab;
029400091211
029500091211         // -?Chiusura (eventuale) archivio?
029600091211         if  %open(TNTBE01L);
029700091211           close  TNTBE01L;
029800091211         endif;
029900091211
030000091211         // -?Apertura archivio?
030100091211         ds_Libr  = $Libraries(w_iSystem);
030200091211         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
030300091211         open  TNTBE01L;
030400091211
030500091211       ENDSR;
030600091211
030700091211      /end-free
030800041206
030900041206      *---------------------------------------------------------------*
031000041206      * GESV1  - Gestione videata selezione codice tabella            *
031100041206      *---------------------------------------------------------------*
031200041206     c     GesV1         BEGSR
031300041206      *
031400041206      * Inizializzazione videata
031500041206if  1c                   if        $CarV1 = *on
031600041206     c                   exsr      CarV1
031700041206     c                   movel     *off          $CarV1
031800041206e   1c                   endif
031900041206      *
032000041206      * Scrivo la testata
032100041206     c                   clear                   T1opz
032200120424     c                   write     TB95T1
032300041206      *
032400041206      * Se esistono errori sulla videata
032500041206      * emetto la write del formato a indicatori spenti per vedere
032600041206      * le eventuali decodifiche
032700041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
032800041206      *
032900120424     c                   exfmt     TB95V1
033000041206     c                   eval      *in99 = *off
033100041206     c                   clear                   V1Dmsg
033200041206      * F3=Fine
033300041206if  1c                   if        *inKC
033400041206     c                   exsr      F03V1
033500041206     c                   goto      EndGesV1
033600041206e   1c                   endif
033700041206      *
033800041206      * Controllo dati immessi a video
033900041206     c                   exsr      CtrV1
034000041206     c     *in99         cabeq     *on           EndGesV1
034100041206      *
034200041206      * Passaggio alla videata di dettaglio
034300041206if  1c                   if        not *in99
034400041206     c                   eval      $CarV2 = *on
034500041206     c                   eval      $TipVid = '2'
034600041206e   1c                   endif
034700041206      *
034800041206     c     EndGesV1      ENDSR
034900041206
035000041206      *---------------------------------------------------------------*
035100041206      * CARV1  - Caricamento dati prima videata                       *
035200041206      *---------------------------------------------------------------*
035300041206     c     CarV1         BEGSR
035400041206      *
035500041206     c                   movea     *zeros        *in(50)
035600041206     c                   movea     '00000'       *in(01)
035700041206      *
035800120424      * Imposto il campo "Cliente" per l'interrogazione
035900120424     c                   movel(p)  '?'           V1CCCM
036000041206      *
036100041206     c                   ENDSR
036200041206
036300120424       //--------------------------------------------------------------
036400120424       //?Controllo videata D01                                        ?
036500120424       //--------------------------------------------------------------
036600120424      /free
036700120424
036800120424       BEGSR  CtrV1;
036900120424
037000120424         clear V1DMsg;
037100120424
037200120424         SELECT;
037300120424
037400120424           // - Ricerca codice cliente
037500120424           WHEN  %scan('?' : V1Cccm) > *zero;
037600120424             $Tibs02 = *on;
037700120424             clear V1CCCM;
037800120424             *in50   = *on;
037900120424             reset TIBS02ds;
038000120424             T02sif = KNSIF;
038100120424             t02mod = 'R';
038200120424             t02cod = '3BC';
038300120424             TNTBE_RicercaControllo(kpjba : tibs02ds);
038400120424if  2        if (T02err  = *blanks);
038500120424               D3BC = T02uni;
038600120424               V1CCCM = T02ke1;
038700120424x   2        else;
038800120424               *in99   = *on;
038900120424               V1Dmsg  = $Msg(03);
039000120424               leavesr;
039100120424             endif;
039200120424
039300120424           // - Controllo immissione codice cliente
039400120424           WHEN  V1Cccm = *blank  or  V1Cccm  = *zero;
039500120424             *in99   = *on;
039600120424             V1Dmsg = $Msg(01);
039700120424             leavesr;
039800120424
039900120424           // - Controllo immissione solo caratteri numerici
040000120424           WHEN  %check(c_Digits : V1Cccm) > *zero;
040100120424             *in99   = *on;
040200120424             V1Dmsg = $Msg(02);
040300120424             leavesr;
040400120424
040500120424         ENDSL;
040600120424
040700120424         // - Controllo esistenza codice cliente in anagrafica clienti
040800120424           clear  TIBS69ds;
040900120424           tibs69ds.I69kcc = DUTkci;
041000120424           tibs69ds.I69kac = %int(V1Cccm);
041100120424           tibs69ds.I69sif = knsif;
041200120424           tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
041300120424           if  tibs69ds.O69err = *on;
041400120424             V1Dmsg = $Msg(03);
041500120424             leavesr;
041600120424           endif;
041700120424
041800120424         // - Verifica esistenza codice cliente in tabella
041900120424         reset  TNTBE000;
042000120424         k_TBEcod = c_Tab;
042100120424         k_TBEke1 = V1Cccm;
042200120424         clear  k_TBEke2;
042300120424         k_TBElin = TBXlin;
042400120424         k_TBEsif = TBXsif;
042500120424
042600120424           chain  %kds(K05tntbe01) TNTBE000;
042700120424           // - - NON ammesso nuovo record per cliente annullato
042800120424           if  NOT  %found(TNTBE01L)  and  ds_CNACO.ACOflg <> *blank;
042900120424             V1Dmsg = $Msg(04);
043000120424             leavesr;
043100120424           endif;
043200120424
043300120424         // - Decodifica cliente
043400120424         if  %found(TNTBE01L);
043500120424           d3BC = TBEuni;
043600120424             V1Drag = §3BCrag;
043700120424         else;
043800120424           clear  d3BC;
043900120424           V1Drag = %subst(ds_CNACO.ACOrag : 1 : %len(V1Drag));
044000120424         endif;
044100120424
044200120424       ENDSR;
044300041206
044400120424      /end-free
044500041206      *---------------------------------------------------------------*
044600041206      * F03V1  - Tasto funzionale F03 -> Fine programma               *
044700041206      *---------------------------------------------------------------*
044800041206     c     F03V1         BEGSR
044900041206      *
045000041206     c                   movel     *on           $Fine                          fine pgm
045100041206      *
045200041206     c                   ENDSR
045300041206
045400041206      *---------------------------------------------------------------*
045500041206      * GESV2  - Gestione videata dettaglio dati                      *
045600041206      *---------------------------------------------------------------*
045700041206     c     GesV2         BEGSR
045800041206      *
045900041206      * Inizializzazione videata
046000041206     c                   if        $CarV2 = *on
046100041206     c                   exsr      CarV2
046200041206     c                   move      *off          $CarV2
046300041206     c                   endif
046400041206      * Scrivo la testata
046500120424     c                   write     TB95T1
046600041206      *
046700041206      * Se esistono errori sulla videata
046800041206      * emetto la write del formato a indicatori spenti per vedere
046900041206      * le eventuali decodifiche
047000041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
047100060627      *?ora ce n'è uno?
047200041206      *
047300041206if  1c                   if        *in05
047400120424     c                   write     TB95V2
047500041206     c                   exfmt     PROTECT
047600041206x   1c                   else
047700120424     c                   exfmt     TB95V2
047800041206e   1c                   endif
047900041206     c                   eval      *in99 = *off
048000041206     c                   clear                   V1Dmsg
048100041206     c                   clear                   wTasto
048200041206      *
048300041206sel 1c                   select
048400041206      * F03=Fine
048500041206w   1c                   when      *inKC
048600041206     c                   exsr      F03V1
048700041206     c                   goto      EndGesV2
048800041206      * F12=Ritorno
048900041206w   1c                   when      *inKL
049000041206     c                   exsr      F12V2
049100041206     c                   goto      EndGesV2
049200041206e   1c                   endsl
049300041206      *
049400041206      * Controllo dati immessi a video
049500041206      * (non si fanno se richisto l'annullamento)
049600041206if  1c                   if        not *inKQ
049700041206     c                   exsr      CtrV2
049800041206e   1c                   endif
049900041206      *
050000041206      * Aggiornamento se non ci sono errori
050100041206if  1c                   if        not *in99
050200041206     c                             and (*inKF or *inKE or *inKQ)
050300041206sel 2c                   select
050400041206w   2c                   when      *inKE
050500041206     c                   eval      wTasto = '05'
050600041206w   2c                   when      *inKF
050700041206     c                   eval      wTasto = '06'
050800041206w   2c                   when      *inKQ
050900041206     c                   eval      wTasto = '16'
051000041206e   2c                   endsl
051100091211     c                   exsr      AggTBE
051200041206e   1c                   endif
051300041206      *
051400041206     c     EndGesV2      ENDSR
051500041206
051600041206      *---------------------------------------------------------------*
051700041206      * CARV2  - Caricamento dati seconda videata                     *
051800041206      *---------------------------------------------------------------*
051900041206     c     CarV2         BEGSR
052000041206      *
052100041206     c                   clear                   T1opz
052200041206     c                   movea     '00000'       *in(01)
052300041206      *
052400120424      * reimposto il campo chiave (qui di solo output) e la sua descrizione
052500120424     c                   eval      V2CCCM = %dec(V1CCCM : 7 : 0)
052600120424     c                   eval      V2Drag = V1Drag
052700041206      *
052800041206      * ripulisco i campi in manutenzione in questa videata
052900041206      * (e relative decodifiche)
053000120424     c                   clear                   V2CDNT
053100120424     c                   clear                   V2CCCC
053200091211      *
053300091211      /free
053400091211
053500091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
053600091211         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
053700091211           w_SisInf = 1;
053800091211           exsr  sr_OpenFileTab;
053900091211         endif;
054000091211
054100091211      /end-free
054200041206      *
054300041206      * Aggancio la tabella, se trovo il codice sono in modifica
054400041206      * o ripristino (se record annullato), altrimenti in immissione
054500041206     c                   exsr      ChnTBE
054600041206      *
054700041206if  1c                   if        %found(TNTBE01L)
054800041206      *
054900041206      * MODIFICA/RIPRISTINO
055000120424     c                   movel     TBEuni        d3BC
055100041206if  2c                   if        TBEatb = *blanks
055200041206     c                   eval      *in02  = *on
055300041206     c                   eval      T1opz  = $Opz(02)
055400041206x   2c                   else
055500041206     c                   eval      *in04  = *on
055600041206     c                   eval      T1opz  = $Opz(06)
055700041206e   2c                   endif
055800041206      *
055900041206x   1c                   else
056000041206      *
056100041206      * IMMISSIONE
056200120424     c                   clear                   d3BC
056300041206     c                   eval      *in01  = *on
056400041206     c                   eval      T1opz  = $Opz(01)
056500041206      *
056600041206e   1c                   endif
056700041206      *
056800120424     c                   eval      V2CCCM = %dec(V1CCCM : 7 : 0)
056900120424     c                   eval      V2Drag = V1Drag
057000041206      *
057100120424     c                   eval      V2CDNT = §3BCDNT
057200120424     c                   eval      V2CCCC = §3BCCCC
057300041206      *
057400041206     c                   ENDSR
057500041206
057600041206      *---------------------------------------------------------------*
057700041206      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
057800041206      *---------------------------------------------------------------*
057900041206     c     F12V2         BEGSR
058000041206      *
058100091211      /free
058200091211         unlock  TNTBE01L;
058300091211      /end-free
058400041206     c                   eval      $TipVid = '1'
058500041206      *
058600041206     c                   ENDSR
058700041206
058800041206      *---------------------------------------------------------------*
058900041206      * CTRV2  - Controllo e decodifica seconda videata               *
059000041206      *---------------------------------------------------------------*
059100041206     c     CtrV2         BEGSR
059200041206      *
059300041206     c                   movea     *zeros        *in(50)
059400041206     c                   clear                   V1Dmsg
059500041206      *
059600041206     c     EndCtrV2      ENDSR
059700041206
059800041206      *---------------------------------------------------------------*
059900041206      * CHNTBE * Aggancio tabella                                     *
060000041206      *---------------------------------------------------------------*
060100041206     c     ChnTBE        BEGSR
060200041206      *
060300120424     c                   movel     '3BC'         TBEcod
060400120424     c                   movel(p)  V2CCCM        TBEke1
060500041206     c                   clear                   TBEke2
060600041206     c                   clear                   TBElin
060700041206     c                   movel     KNSIF         TBEsif
060800041206     c     K05TBE01      chain     TNTBE01L
060900041206      * Se non ho trovato il record con il sistema informativo
061000041206      * che ho in linea, lo abblenco
061100041206if  1c                   if        not %found(TNTBE01L)
061200041206     c                   clear                   TBEsif
061300041206     c     K05TBE01      chain     TNTBE01L
061400041206e   1c                   endif
061500041206      *
061600041206     c                   ENDSR
061700041206
061800041206      *---------------------------------------------------------------*
061900041206      * AGGTBE * Aggiornamento tabella                                *
062000041206      *---------------------------------------------------------------*
062100041206     c     AggTBE        BEGSR
062200091211      *
062300091211      /free
062400091211
062500091211         // -?Ciclo di elaborazione per ogni sistema informativo?
062600091211         For  w_SisInf = 1  To  %elem($Libr);
062700091211
062800091211           // -?Apertura (eventuale) del file ed aggancio record?
062900091211           if  w_SisInf > 1;
063000091211             exsr  sr_OpenFileTab;
063100091211             exsr  ChnTBE;
063200091211           endif;
063300091211
063400091211         //? N.B.                                                      ?
063500091211         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
063600091211         //   trasmissione (vedi flag TBEFTR).                        ?
063700091211         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
063800091211         //   subito nello stesso file di entrambi i S.I. - in due    ?
063900091211         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
064000091211
064100091211      /end-free
064200041206      *
064300041206sel 1c                   SELECT
064400041206      *
064500041206      * F5=Ripristino
064600041206w   1c                   WHEN      wTasto='05'  and  *in04
064700091211      /free
064800091211               if  %found(TNTBE01L);
064900091211      /end-free
065000041206     c                   clear                   TBEatb
065100041206     c                   clear                   TBEftr
065200041206     c                   UPDATE    TNTBE000
065300091211      /free
065400091211               else;
065500091211                 // -?Record appena cancellato fisicamente?
065600091211                 exsr  RieTBE;
065700091211                 WRITE  TNTBE000;
065800091211               endif;
065900091211      /end-free
066000041206      *
066100041206      * F6=Conferma
066200041206w   1c                   WHEN      wTasto='06'
066300041206     c                   exsr      RieTBE
066400041206sel 2c                   select
066500041206      *   Immissione
066600041206w   2c                   when      *in01
066700091211     c*//                clear                   TBEflt
066800091211     c*//                clear                   TBEdtr
066900041206     c                   WRITE     TNTBE000                             22
067000041206      *   Modifica / Ripristino
067100041206w   2c                   when      *in02   or    *in04
067200041206     c                   UPDATE    TNTBE000
067300041206e   2c                   endsl
067400041206      *
067500041206      * F16=Annullamento
067600041206w   1c                   WHEN      wTasto='16'  and  not *in04
067700041206     c                   movel     'A'           TBEatb
067800041206     c                   clear                   TBEftr
067900041206     c                   UPDATE    TNTBE000
068000041206      *
068100041206e   1c                   ENDSL
068200091211      /free
068300091211
068400091211         EndFor;
068500091211
068600091211      /end-free
068700041206      *
068800041206      * Torno alla prima videata che carico come da inizio pgm
068900041206     c                   movel     '1'           $TipVid
069000041206     c                   movel     *on           $CarV1
069100041206     c                   movel     *on           $CarV2
069200041206     c
069300041206     c                   ENDSR
069400041206
069500041206      *---------------------------------------------------------------*
069600041206      * RIETBE * Riempimento dati tabella                             *
069700041206      *---------------------------------------------------------------*
069800041206     c     RieTBE        BEGSR
069900041206      *
070000041206     c                   clear                   TBEatb
070100041206     c                   if        TBXsif <> *blanks
070200041206     c                   movel     KNSIF         TBEsif
070300041206     c                   else
070400041206     c                   clear                   TBEsif
070500041206     c                   endif
070600041206     c                   movel     TBXapl        TBEapl
070700120424     c                   movel     '3BC'         TBEcod
070800120424     c                   movel     V2CCCM        TBEke1
070900041206     c                   clear                   TBEke2
071000041206      *
071100120424     c                   clear                   d3BC
071200120424     c                   eval      §3BCrag = V2Drag
071300120424     c                   eval      §3BCDNT = V2CDNT
071400120424     c                   eval      §3BCCCC = V2CCCC
071500120424     c                   movel(p)  d3BC          TBEuni
071600041206      *
071700091211     c                   movel     'S'           TBEftt
071800041206     c                   z-add     TBXflt        TBEflt
071900091211     c*//                clear                   TBEftr
072000091211      /free
072100091211
072200091211         select;
072300091211           // -?Annullamento o Ripristino?
072400091211           when  (wTasto='05'  and  *in04)   or
072500091211                 (wTasto='16'  and  not *in04);
072600091211             clear  TBEftr;
072700091211             clear  TBEdtr;
072800091211           // -?Aggiornamento o Scrittura in sede?
072900091211           when  w_SisInf = 1;
073000091211             TBEftr = 'T';
073100091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
073200091211                                     : 1 : 8 ) );
073300091211           // -?Aggiornamento o Scrittura in filiale?
073400091211           other;
073500091211             TBEftr = 'R';
073600091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
073700091211                                     : 1 : 8 ) );
073800091211         endsl;
073900091211
074000091211      /end-free
074100041206      *
074200041206     c                   ENDSR
074300041206
074400091211       //--------------------------------------------------------------
074500091211       //?Schiere a tempo di compilazione.                             ?
074600091211       //--------------------------------------------------------------
074700091211
074800091211** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
074900091211SETRAS  GAITRAGRU FILTRAGRU
075000091211AS888   GAITRAGRPSFILTRAGRPF
075100041206** - $Opz     *
075200041206  Inserimento
075300041206    Modifica
075400041206     Copia
075500041206  Annullamento
075600041206Visualizzazione
075700041206   ANNULLATO
075800041206** - $Msg                                                                    *
075900120424Immettere il codice cliente                                                     1
076000120424Immettere SOLO caratteri numerici                                               2
076100120424Codice errato                                                                   3
076200120424Cliente annullato                                                               4
076300120424Cliente già inserito in tab. "3BC"                                              5
076400120424                                                                                6
