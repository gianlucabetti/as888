000100041206      *---------------------------------------------------------------*
000200041206      * Gestione tabella "AR8" = Estensioni FIAR8 x TMD               *
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
001600041206     fTNTB35D   cf   e             workstn
001700041206
001800041206      *---------------------------------------------------------------*
001900041206      *   C O S T A N T I                                             *
002000041206      *---------------------------------------------------------------*
002100041206      *
002200091211       // -?Costanti per la definizione delle schiere con i nomi?
002300091211       //  ?degli iSeries da elaborare e delle relative librerie?
002400091211     d c_NrSyst        c                   const(2)
002500091211     d c_NrLibr        c                   const(2)
002600041206
002700041206      *---------------------------------------------------------------*
002800041206      *   S C H I E R E                                               *
002900041206      *---------------------------------------------------------------*
003000041206      *
003100091211       // -?iSeries  &  Librerie con entrambi i file tabelle?
003200091211     d $iSystem        s                   like(currSysNetA)
003300091211     d                                     dim(c_NrSyst)
003400091211     d                                     ctdata   perrcd( 1)
003500091211     d $Libraries      s                   like(ds_Libr)
003600091211     d                                     dim(c_NrSyst)
003700091211     d                                     alt($iSystem)
003800091211      *
003900041206     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
004000050601     d $Msg            s             78    dim(06) ctdata perrcd(1)             Messaggi video
004100041206     d $Cmd            s             80    dim(5)  ctdata perrcd(1)             Comandi
004200041206
004300041206      *---------------------------------------------------------------*
004400041206      *   S T R U T T U R E   D A T I                                 *
004500041206      *---------------------------------------------------------------*
004600041206      *
004700041206      * Parametri
004800041206     d KPJBA         e ds
004900041206      *
005000041206      * Passaggio Parametri al pgm TIBS02R
005100041206     d TIBS02DS      e ds                  inz
005200041206      *
005300041206      * Tabella AR8 = Estensioni FIAR8 x TMD
005400041206     d dAR8          e ds                  inz
005500060627      * Tabella TSD = Tipo Stampa DDT
005600060627     d dtsd          e ds                  inz
005700041206      *
005800041206      * Tracciato record file TNTBE00F
005900041206     d TNTBEds       e ds                  extname(TNTBE00F) inz
006000041206     d xTNTBEds      e ds                  extname(TNTBE00F) inz
006100041206     d                                     prefix(TBX:3)
006200041206      *
006300041206     d TIBS34DS      e ds                  inz
006400041206     d DDATIUTE      e ds                  inz
006500041206     d AZUTEDS       e ds                  extname(AZUTE00F) inz
006600041206      *
006700041206     d WLBDAT          ds                  inz
006800041206     d  G02dat                 1      8  0 inz
006900041206     d  G02inv                 9     16  0 inz
007000041206     d  G02err                17     17    inz
007100041206     d  G02tgi                18     22  0 inz
007200091211       //
007300091211       // -?Ridefinizione elenco librerie in elaborare le tabelle?
007400091211     d ds_Libr         ds                  inz
007500091211     d  $Libr                        10    dim(c_NrLibr) inz
007600041206      *
007700041206     d                sds
007800041206     d  VTCpgm           *proc
007900041206
008000041206      *---------------------------------------------------------------*
008100041206      *   V A R I A B I L I                                           *
008200041206      *---------------------------------------------------------------*
008300041206      *
008400091211      * - Flags booleani
008500041206     d $Fine           s              1a   inz(*off)
008600041206     d $CarV1          s              1a   inz(*on)
008700041206     d $CarV2          s              1a   inz(*off)
008800041206     d $CarW1          s              1a   inz(*off)
008900091211     d $Tibs02         s              1a   inz(*off)
009000091211      * - Variabili per la gestione del video
009100041206     d $TipVid         s              1a   inz('1')
009200091211     d wTasto          s              2a   inz(*zeros)
009300091211     d wPos            s              2  0
009400091211      * - Nomi (completi) dei loghi
009500041206     d §Logo           s             10a   inz
009600091211     d §LogoF          s             10a   inz
009700091211      * - Parametri per QCMDEXC
009800041206     d Qcmd            s             80    inz
009900041206     d Qlen            s             15  5 inz(80)
010000091211       //
010100091211       // -?Nome del sistema?
010200091211     d currSysNeta     s              8a   inz
010300091211       // -?Nome esteso Libreria/File del file tabella?
010400091211     d wLibFile        s             21a   inz
010500091211       // -?Campi di comodo?
010600091211     d w_iSystem       s              1  0 inz
010700091211     d w_SisInf        s              3  0 inz
010800091211
010900091211       //--------------------------------------------------------------
011000091211       //?Definizione prototipi procedure.                             ?
011100091211       //--------------------------------------------------------------
011200091211
011300091211       // -?Reperimento NETA sistema AS/400 corrente?
011400091211      /copy gaitrasrc/srcProtoPr,UBRTVNETA
011500041206
011600041206      *---------------------------------------------------------------*
011700041206      *   M A I N   L I N E                                           *
011800041206      *---------------------------------------------------------------*
011900041206      *  Riepilogo indicatori utilizzati:                             *
012000041206      *  --------------------------------                             *
012100041206      *  01 - Record inesistente (inserimento)                        *
012200041206      *  02 - Record esistente   (modifica)                           *
012300041206      *  04 - Record annullato   (ripristino)                         *
012400041206      *  20 - Comodo                                                  *
012500041206      *  22 - Errori in scrittura record (WRITE)                      *
012600041206      *  50 - Posizionamento cursore su "Tipo modulo DDT cliente"     *
012700041206      *  51 - Posizionamento cursore su "Descrizione tipo modulo DDT" *
012800041206      *  52 - Posizionamento cursore su "Tipo stampa DDT"             *
012900050601      *  53 - Posizionamento cursore su logo per stampa fattura       *
013000041206      *  99 - Visualizzazione messaggio di errore                     *
013100041206      *---------------------------------------------------------------*
013200041206      *
013300041206      * Operazioni iniziali
013400041206     c                   exsr      RutInz
013500041206      *
013600041206      * Gestione video
013700041206     c                   dow       $Fine = *off
013800041206     c     $TipVid       caseq     '1'           GesV1
013900041206     c     $TipVid       caseq     '2'           GesV2
014000041206     c     $TipVid       caseq     'A'           GesW1
014100041206     c                   endcs
014200041206     c                   enddo
014300041206      *
014400041206      * Fine
014500041206     c                   if        $Tibs02 = *on
014600041206     c                   clear                   TIBS02ds
014700041206     c                   movel     'C'           T02tla
014800041206     c                   call      'TIBS02R'
014900041206     c                   parm                    KPJBA
015000041206     c                   parm                    TIBS02ds
015100041206     c                   endif
015200041206      *
015300091211     c***                return
015400041206
015500041206      *---------------------------------------------------------------*
015600041206      * RutInz - Operazioni Iniziali                                  *
015700041206      *---------------------------------------------------------------*
015800041206     c     RutInz        BEGSR
015900041206      *
016000041206      * Ricezione parametri
016100041206     c     *entry        plist
016200041206     c                   parm                    KPJBA
016300041206      *
016400041206      * Definizioni chiavi di accesso
016500041206     c     K05TBE01      klist                                                  *tntbe01l
016600041206     c                   kfld                    TBEcod                         -tabella
016700041206     c                   kfld                    TBEke1                         -chiave uno
016800041206     c                   kfld                    TBEke2                         -chiave due
016900041206     c                   kfld                    TBElin                         -lingua
017000041206     c                   kfld                    TBEsif                         -s.informativo
017100091211      /free
017200091211
017300091211         *inLR = *on;
017400091211
017500091211         // -?Verifica del sistema AS/400 corrente?
017600091211         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
017700091211           $Fine = *on;
017800091211           leavesr;
017900091211         endif;
018000091211
018100091211         // -?Impostazione elenco librerie in cui gestire le tabelle?
018200091211         //  ?(a seconda del sistema in cui si stà lavorando)?
018300091211         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
018400091211         if  w_iSystem = *zero;
018500091211           $Fine = *on;
018600091211           leavesr;
018700091211         endif;
018800091211
018900091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
019000091211         w_SisInf = 1;
019100091211         exsr  sr_OpenFileTab;
019200091211
019300091211      /end-free
019400041206      *
019500041206      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
019600041206     c     *dtaara       define    §azute        azuteds
019700041206     c     *dtaara       define    §datiute      ddatiute
019800041206      *
019900041206     c                   clear                   AzUteDs
020000041206     c                   clear                   DDatiUte
020100041206     c                   clear                   Tibs34Ds
020200041206     c                   in(E)     *dtaara
020300041206if  1c                   if        %Error  or  RSUT = *blanks
020400041206     c                   call      'TIBS34R'
020500041206     c                   parm                    Tibs34Ds
020600041206     c                   in        *dtaara
020700041206e   1c                   endif
020800041206      *-- Verifica errori e autorità profilo
020900041206sel 1c                   SELECT
021000041206      *-- controllo se ho errori nei dati utente
021100041206      *--   nel qual caso NON risulta un profilo abilitato
021200041206w   1c                   WHEN      DUTerr = 'E'
021300041206     c                   eval      $Fine  = *on
021400041206      *
021500041206      *-- CONTROLLO AUTORITA'
021600041206      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
021700041206      *-- se il chiamante non richiede autorità specifica verificare
021800041206      *--   quella del profilo
021900041206      *-- se il chiamante richiede autorità specifica verificarla,
022000041206      *--  se è blank verificare quella del profilo
022100041206      *
022200041206      * se UTEAUT = *BLANK non siamo in sede
022300041206w   1c                   WHEN      UTEaut = *blank
022400041206      *
022500041206x   1c                   OTHER
022600041206      *
022700041206e   1c                   ENDSL
022800041206      *
022900041206      * Aggancio dati generali della tabella in esame
023000041206     c                   clear                   TBEcod
023100041206     c                   move      *zeros        TBEke1
023200091211     c                   move      'AR8'         TBEke1
023300041206     c                   clear                   TBEke2
023400041206     c                   clear                   TBElin
023500041206     c                   movel     KNSIF         TBEsif
023600091211     c     K05TBE01      chain(n)  TNTBE01L
023700041206     c                   if        not %found(TNTBE01L)
023800041206     c                   clear                   TBEsif
023900091211     c     K05TBE01      chain(n)  TNTBE01L
024000041206     c                   endif
024100041206     c                   if        %found(TNTBE01L)
024200041206     c                   movel     TNTBEds       xTNTBEds
024300041206     c                   else
024400041206     c                   clear                   xTNTBEds
024500041206     c                   endif
024600041206      *
024700041206     c                   clear                   TB35V1
024800041206      *
024900041206     c                   ENDSR
025000091211      /free
025100091211
025200091211       //--------------------------------------------------------------
025300091211       //?Apertura dei files tabelle nel sistema informativo impostato.?
025400091211       //--------------------------------------------------------------
025500091211       BEGSR  sr_OpenFileTab;
025600091211
025700091211         // -?Chiusura (eventuale) archivio?
025800091211         if  %open(TNTBE01L);
025900091211           close  TNTBE01L;
026000091211         endif;
026100091211
026200091211         // -?Apertura archivio?
026300091211         ds_Libr  = $Libraries(w_iSystem);
026400091211         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
026500091211         open  TNTBE01L;
026600091211
026700091211       ENDSR;
026800091211
026900091211      /end-free
027000041206
027100041206      *---------------------------------------------------------------*
027200041206      * GESV1  - Gestione videata selezione codice tabella            *
027300041206      *---------------------------------------------------------------*
027400041206     c     GesV1         BEGSR
027500041206      *
027600041206      * Inizializzazione videata
027700041206if  1c                   if        $CarV1 = *on
027800041206     c                   exsr      CarV1
027900041206     c                   movel     *off          $CarV1
028000041206e   1c                   endif
028100041206      *
028200041206      * Scrivo la testata
028300041206     c                   clear                   T1opz
028400041206     c                   write     TB35T1
028500041206      *
028600041206      * Se esistono errori sulla videata
028700041206      * emetto la write del formato a indicatori spenti per vedere
028800041206      * le eventuali decodifiche
028900041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
029000041206      *
029100041206     c                   exfmt     TB35V1
029200041206     c                   eval      *in99 = *off
029300041206     c                   clear                   V1Dmsg
029400041206      * F3=Fine
029500041206if  1c                   if        *inKC
029600041206     c                   exsr      F03V1
029700041206     c                   goto      EndGesV1
029800041206e   1c                   endif
029900041206      *
030000041206      * Controllo dati immessi a video
030100041206     c                   exsr      CtrV1
030200041206     c     *in99         cabeq     *on           EndGesV1
030300041206      *
030400041206      * Passaggio alla videata di dettaglio
030500041206if  1c                   if        not *in99
030600041206     c                   eval      $CarV2 = *on
030700041206     c                   eval      $TipVid = '2'
030800041206e   1c                   endif
030900041206      *
031000041206     c     EndGesV1      ENDSR
031100041206
031200041206      *---------------------------------------------------------------*
031300041206      * CARV1  - Caricamento dati prima videata                       *
031400041206      *---------------------------------------------------------------*
031500041206     c     CarV1         BEGSR
031600041206      *
031700041206     c                   movea     *zeros        *in(50)
031800041206     c                   movea     '00000'       *in(01)
031900041206      *
032000041206      * Imposto il campo "tipo modulo DDT cliente" per l'interrogazione
032100041206     c                   movel(p)  '?'           V1Ctmd
032200041206      *
032300041206     c                   ENDSR
032400041206
032500041206      *---------------------------------------------------------------*
032600041206      * CTRV1  - Controllo e decodifica prima videata                 *
032700041206      *---------------------------------------------------------------*
032800041206     c     CtrV1         BEGSR
032900041206      *
033000041206     c                   movea     *zeros        *in(50)
033100041206     c                   clear                   V1Dmsg
033200041206      *
033300041206      * Tipo modulo DDT
033400041206      * - Ricerca:
033500041206     c     '?'           scan      V1Ctmd                                 20
033600041206if  1c                   if        *in20
033700041206     c                   eval      $Tibs02 = *on
033800041206     c                   clear                   V1Ctmd
033900041206     c                   eval      *in50   = *on
034000041206     c                   reset                   TIBS02ds
034100041206     c                   movel     KNSIF         T02sif
034200060627     c                   eval      t02mod = 'R'
034300060627     c                   eval      t02cod = 'AR8'
034400041206     c                   call      'TIBS02R'
034500041206     c                   parm                    KPJBA
034600041206     c                   parm                    TIBS02ds
034700041206if  2c                   if        T02err  = *blanks
034800041206     c                   movel     T02uni        dAR8
034900041206     c                   movel     T02ke1        V1Ctmd
035000041206x   2c                   else
035100041206     c                   eval      *in99   = *on
035200041206     c                   movel     T02msg        V1Dmsg
035300041206     c                   endif
035400041206e   1c                   endif
035500041206      * - Controllo:
035600041206if  1c                   if        V1Ctmd  = *blanks
035700041206     c                   seton                                        50  99
035800041206     c                   eval      V1Dmsg  = $Msg(01)
035900041206     c                   goto      EndCtrV1
036000041206e   1c                   endif
036100041206      *
036200041206     c     EndCtrV1      ENDSR
036300041206
036400041206      *---------------------------------------------------------------*
036500041206      * F03V1  - Tasto funzionale F03 -> Fine programma               *
036600041206      *---------------------------------------------------------------*
036700041206     c     F03V1         BEGSR
036800041206      *
036900041206     c                   movel     *on           $Fine                          fine pgm
037000041206      *
037100041206     c                   ENDSR
037200041206
037300041206      *---------------------------------------------------------------*
037400041206      * GESV2  - Gestione videata dettaglio dati                      *
037500041206      *---------------------------------------------------------------*
037600041206     c     GesV2         BEGSR
037700041206      *
037800041206      * Inizializzazione videata
037900041206     c                   if        $CarV2 = *on
038000041206     c                   exsr      CarV2
038100041206     c                   move      *off          $CarV2
038200041206     c                   endif
038300041206      * Scrivo la testata
038400041206     c                   write     TB35T1
038500041206      *
038600041206      * Se esistono errori sulla videata
038700041206      * emetto la write del formato a indicatori spenti per vedere
038800041206      * le eventuali decodifiche
038900041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
039000060627      *?ora ce n'è uno?
039100041206      *
039200041206if  1c                   if        *in05
039300041206     c                   write     TB35V2
039400041206     c                   exfmt     PROTECT
039500041206x   1c                   else
039600041206     c                   exfmt     TB35V2
039700041206e   1c                   endif
039800041206     c                   eval      *in99 = *off
039900041206     c                   clear                   V1Dmsg
040000041206     c                   clear                   wTasto
040100041206      *
040200041206sel 1c                   select
040300041206      * F03=Fine
040400041206w   1c                   when      *inKC
040500041206     c                   exsr      F03V1
040600041206     c                   goto      EndGesV2
040700041206      * F12=Ritorno
040800041206w   1c                   when      *inKL
040900041206     c                   exsr      F12V2
041000041206     c                   goto      EndGesV2
041100041206e   1c                   endsl
041200041206      *
041300041206      * Controllo dati immessi a video
041400041206      * (non si fanno se richisto l'annullamento)
041500041206if  1c                   if        not *inKQ
041600041206     c                   exsr      CtrV2
041700041206e   1c                   endif
041800041206      *
041900041206      * Aggiornamento se non ci sono errori
042000041206if  1c                   if        not *in99
042100041206     c                             and (*inKF or *inKE or *inKQ)
042200041206sel 2c                   select
042300041206w   2c                   when      *inKE
042400041206     c                   eval      wTasto = '05'
042500041206w   2c                   when      *inKF
042600041206     c                   eval      wTasto = '06'
042700041206w   2c                   when      *inKQ
042800041206     c                   eval      wTasto = '16'
042900041206e   2c                   endsl
043000091211       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
043100091211       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
043200091211       //  ?NON ha più senso gestire la richiesta di trasmissione!?
043300091211     ***c                   eval      $CarW1 = *on
043400091211     ***c                   eval      $TipVid = 'A'
043500091211     c                   exsr      AggTBE
043600041206e   1c                   endif
043700041206      *
043800041206     c     EndGesV2      ENDSR
043900041206
044000041206      *---------------------------------------------------------------*
044100041206      * CARV2  - Caricamento dati seconda videata                     *
044200041206      *---------------------------------------------------------------*
044300041206     c     CarV2         BEGSR
044400041206      *
044500041206     c                   clear                   T1opz
044600041206     c                   movea     '00000'       *in(01)
044700041206     c                   clear                   §Logo
044800091211     c                   clear                   §LogoF
044900041206      *
045000041206      * reimposto il campo chiave (qui di solo output)
045100041206     c                   eval      V2Ctmd = V1Ctmd
045200041206      *
045300041206      * ripulisco i campi in manutenzione in questa videata
045400041206      * (e relative decodifiche)
045500041206     c                   clear                   V2Dtmd
045600041206     c                   clear                   V2Ctsd
045700041206     c                   clear                   V2Dtsd
045800091211     c                   clear                   V2Clogof
045900091211      *
046000091211      /free
046100091211
046200091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
046300091211         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
046400091211           w_SisInf = 1;
046500091211           exsr  sr_OpenFileTab;
046600091211         endif;
046700091211
046800091211      /end-free
046900041206      *
047000041206      * Aggancio la tabella, se trovo il codice sono in modifica
047100041206      * o ripristino (se record annullato), altrimenti in immissione
047200041206     c                   exsr      ChnTBE
047300041206      *
047400041206if  1c                   if        %found(TNTBE01L)
047500041206      *
047600041206      * MODIFICA/RIPRISTINO
047700041206     c                   movel     TBEuni        dAR8
047800041206if  2c                   if        TBEatb = *blanks
047900041206     c                   eval      *in02  = *on
048000041206     c                   eval      T1opz  = $Opz(02)
048100041206x   2c                   else
048200041206     c                   eval      *in04  = *on
048300041206     c                   eval      T1opz  = $Opz(06)
048400041206e   2c                   endif
048500041206      *
048600041206x   1c                   else
048700041206      *
048800041206      * IMMISSIONE
048900041206     c                   clear                   dAR8
049000041206     c                   eval      *in01  = *on
049100041206     c                   eval      T1opz  = $Opz(01)
049200041206      *
049300041206e   1c                   endif
049400041206      *
049500041206     c                   eval      V2Ctmd = V1Ctmd
049600041206      *
049700041206     c                   eval      V2Dtmd = §AR8des
049800041206     c                   eval      V2Ctsd = §AR8tsd
049900050331     c                   eval      V2Ccxd = §AR8cxd
050000050601     c                   Eval      v2clogof = §ar8logof
050100060627
050200060627      * decodifico tipo stampa ddt
050300060627     c                   if        v2ctsd <> *blanks
050400060627     c                   clear                   v2dtsd
050500060627     c                   eval      $Tibs02 = *on
050600060627     c                   clear                   tibs02ds
050700060627     c                   eval      t02mod = 'C'
050800060627     c                   eval      t02cod = 'TSD'
050900060627     c                   eval      t02ke1 = v2ctsd
051000060627     c                   eval      t02sif = knsif
051100060627     c                   call      'TIBS02R'
051200060627     c                   parm                    kpjba
051300060627     c                   parm                    tibs02ds
051400060627     c                   if        t02err = *blanks
051500060627     c                   eval      dtsd = t02uni
051600060627     c                   eval      v2dtsd = §tsddes
051700060627     c                   endif
051800060627     c                   endif
051900041206      *
052000041206     c                   ENDSR
052100041206
052200041206      *---------------------------------------------------------------*
052300041206      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
052400041206      *---------------------------------------------------------------*
052500041206     c     F12V2         BEGSR
052600041206      *
052700091211      /free
052800091211         unlock  TNTBE01L;
052900091211      /end-free
053000041206     c                   eval      $TipVid = '1'
053100041206      *
053200041206     c                   ENDSR
053300041206
053400041206      *---------------------------------------------------------------*
053500041206      * CTRV2  - Controllo e decodifica seconda videata               *
053600041206      *---------------------------------------------------------------*
053700041206     c     CtrV2         BEGSR
053800041206      *
053900041206     c                   movea     *zeros        *in(50)
054000041206     c                   clear                   V1Dmsg
054100041206      *
054200041206      * Descrizione tipo modulo DDT obbligatorio
054300041206if  1c                   if        V2Dtmd = *blanks
054400041206     c                   seton                                        51  99
054500041206     c                   movel     $Msg(02)      V1Dmsg
054600041206     c                   goto      EndCtrV2
054700041206e   1c                   endif
054800041206      *
054900060627      * Tipo stampa DDT obbligatorio
055000060627     c                   if        v2ctsd = *blanks
055100060627     c                   eval      *in52 = *on
055200060627     c                   eval      *in99 = *on
055300060627     c                   movel     $msg(04)      v1dmsg
055400060627     c                   goto      endctrv2
055500060627     c                   endif
055600060627      * ricerca tipo stampa DDT
055700060627     c                   eval      wpos = %scan('?':v2ctsd)
055800060627     c                   if        wpos > 0
055900060627     c                   clear                   tibs02ds
056000060627     c                   clear                   v2ctsd
056100060627     c                   eval      $Tibs02 = *on
056200060627     c                   eval      *in52 = *on
056300060627     c                   eval      t02mod = 'R'
056400060627     c                   eval      t02cod = 'TSD'
056500060627     c                   eval      t02sif = knsif
056600060627     c                   call      'TIBS02R'
056700060627     c                   parm                    kpjba
056800060627     c                   parm                    tibs02ds
056900060627     c                   if        t02err = *blanks
057000060627     c                   eval      dtsd = t02uni
057100060627     c                   movel     t02ke1        v2ctsd
057200060627     c                   else
057300060627     c                   eval      *in99 = *on
057400060627     c                   eval      v1dmsg = t02msg
057500060627     c                   endif
057600060627     c                   goto      endctrv2
057700060627     c                   endif
057800060627      * controllo tipo stampa DDT
057900060627     c                   eval      $Tibs02 = *on
058000060627     c                   clear                   tibs02ds
058100060627     c                   eval      t02mod = 'C'
058200060627     c                   eval      t02cod = 'TSD'
058300060627     c                   eval      t02ke1 = v2ctsd
058400060627     c                   eval      t02sif = knsif
058500060627     c                   call      'TIBS02R'
058600060627     c                   parm                    kpjba
058700060627     c                   parm                    tibs02ds
058800060627     c                   if        t02err = *blanks
058900060627     c                   eval      dtsd = t02uni
059000060627     c                   eval      v2dtsd = §tsddes
059100060627     c                   else
059200060627     c                   clear                   v2dtsd
059300041206     c                   seton                                        52  99
059400041206     c                   movel     $Msg(04)      V1Dmsg
059500041206     c                   goto      EndCtrV2
059600060627     c                   endif
059700041206      *
059800041206      * Logo DDT inesistente
059900041206if  1c                   if            V2Ctsd <> 'S1'
060000041206     c                             and §Logo  <> ('LOGO' + V2Ctmd)
060100041206     c                   eval      §Logo  = 'LOGO' + V2Ctmd
060200041206     c                   eval      Qcmd  = %replace(§Logo:$Cmd(1):
060300041206     c                                     %scan('&LOGO     ':
060400041206     c                                           $Cmd(1)))
060500041206     c                   call      'QCMDEXC'                            20
060600041206     c                   parm                    Qcmd
060700041206     c                   parm                    Qlen
060800041206      * (messaggio di solo avviso)
060900041206if  2c                   if            *in20 = *on
061000041206     c                   seton                                        52  99
061100041206     c                   movel     $Msg(05)      V1Dmsg
061200041206     c                   goto      EndCtrV2
061300041206e   2c                   endif
061400041206e   1c                   endif
061500050601
061600050601      * Controllo se esiste il logo per la stampa fattura
061700050601if  1c                   If        v2clogof <> *Blanks
061800050601     c                             and §logof <> ('LOGO' + v2clogof)
061900050601     c                   eval      §logof = 'LOGO' + v2clogof
062000050601     c                   eval      Qcmd  = %replace(§Logof:$Cmd(1):
062100050601     c                                     %scan('&LOGO     ':
062200050601     c                                           $Cmd(1)))
062300050601     c                   call      'QCMDEXC'                            20
062400050601     c                   parm                    Qcmd
062500050601     c                   parm                    Qlen
062600050601      * (messaggio di solo avviso)
062700050601if  2c                   if            *in20 = *on
062800050601     c                   seton                                        53  99
062900050601     c                   movel     $Msg(06)      V1Dmsg
063000050601     c                   goto      EndCtrV2
063100050601e   2c                   endif
063200050601e   1c                   endif
063300041206      *
063400041206     c     EndCtrV2      ENDSR
063500041206
063600041206      *---------------------------------------------------------------*
063700041206      * GESW1  - Gestione videata dati relativi alla trasmissione     *
063800041206      *---------------------------------------------------------------*
063900041206     c     GesW1         BEGSR
064000041206      *
064100041206      * Inizializzazione videata
064200041206if  1c                   if        $CarW1 = *on
064300041206     c                   exsr      CarW1
064400041206     c                   movel     *off          $CarW1
064500041206e   1c                   endif
064600041206      *
064700041206if  1c                   if        *in05
064800041206     c                   write     TB35W1
064900041206     c                   exfmt     PROTECT
065000041206x   1c                   else
065100041206     c                   exfmt     TB35W1
065200041206e   1c                   endif
065300041206     c                   eval      *in99 = *off
065400041206     c                   clear                   W1MSG
065500041206      *
065600041206sel 1c                   select
065700041206      * F12=Ritorno
065800041206w   1c                   when      *inKL
065900041206     c                   exsr      F12W1
066000041206     c                   goto      EndGesW1
066100041206e   1c                   endsl
066200041206      *
066300041206      * Controllo dati immessi a video
066400041206     c                   exsr      CtrW1
066500041206      *
066600041206      * Aggiornamento se non ci sono errori
066700041206if  1c                   if        not *in99 and *inKF
066800041206     c                   exsr      AggTBE
066900041206e   1c                   endif
067000041206      *
067100041206     c     EndGesW1      ENDSR
067200041206
067300041206      *---------------------------------------------------------------*
067400041206      * CARW1  - Caricamento dati window                              *
067500041206      *---------------------------------------------------------------*
067600041206     c     CarW1         BEGSR
067700041206      *
067800041206     c                   movea     *zeros        *in(50)
067900041206      *
068000041206sel 1c                   select
068100041206      *
068200041206      * F5=Ripristino
068300041206w   1c                   when      *inKE   and  *in04
068400041206     c                   eval      W1FTT = TBEftt
068500041206      *
068600041206      * F6=Conferma
068700041206w   1c                   when      *inKF
068800041206sel 2c                   select
068900041206      *   Immissione
069000041206w   2c                   when      *in01
069100041206     c                   eval      W1FTT = TBXftt
069200041206      *   Modifica / Ripristino
069300041206w   2c                   when      *in02   or    *in04
069400041206     c                   eval      W1FTT = TBEftt
069500041206e   2c                   endsl
069600041206      *
069700041206      * F16=Annullamento
069800041206w   1c                   when      *inKQ   and  not *in04
069900041206     c                   eval      W1FTT = TBEftt
070000041206      *
070100041206e   1c                   endsl
070200041206      *
070300041206      * Se NON immissione: visualizzo i dati relativi all'ultima
070400041206      *   trasmissione
070500041206if  1c                   if        not *in01
070600041206     c                   eval      W1FLT = TBEflt
070700041206     c                   eval      W1FTR = TBEftr
070800041206if  2c                   if        TBEdtr <> 0
070900041206     c                   clear                   WLBDAT
071000041206     c                   z-add     TBEdtr        G02inv
071100041206     c                   movel     '3'           G02err
071200041206     c                   call      'XSRDA8'
071300041206     c                   parm                    WLBDAT
071400041206     c                   z-add     G02dat        W1DTR
071500041206e   2c                   endif
071600041206e   1c                   endif
071700041206      *
071800041206     c                   ENDSR
071900041206
072000041206      *---------------------------------------------------------------*
072100041206      * CTRW1  - Controllo e decodifica window                        *
072200041206      *---------------------------------------------------------------*
072300041206     c     CtrW1         BEGSR
072400041206      *
072500041206     c                   movea     *zeros        *in(50)
072600041206      *
072700041206     c     EndCtrW1      ENDSR
072800041206
072900041206      *---------------------------------------------------------------*
073000041206      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
073100041206      *---------------------------------------------------------------*
073200041206     c     F12W1         BEGSR
073300041206      *
073400041206     c                   eval      $TipVid = '2'
073500041206      *
073600041206     c                   ENDSR
073700041206
073800041206      *---------------------------------------------------------------*
073900041206      * CHNTBE * Aggancio tabella                                     *
074000041206      *---------------------------------------------------------------*
074100041206     c     ChnTBE        BEGSR
074200041206      *
074300060627     c                   movel     'AR8'         TBEcod
074400041206     c                   movel(p)  V2Ctmd        TBEke1
074500041206     c                   clear                   TBEke2
074600041206     c                   clear                   TBElin
074700041206     c                   movel     KNSIF         TBEsif
074800041206     c     K05TBE01      chain     TNTBE01L
074900041206      * Se non ho trovato il record con il sistema informativo
075000041206      * che ho in linea, lo abblenco
075100041206if  1c                   if        not %found(TNTBE01L)
075200041206     c                   clear                   TBEsif
075300041206     c     K05TBE01      chain     TNTBE01L
075400041206e   1c                   endif
075500041206      *
075600041206     c                   ENDSR
075700041206
075800041206      *---------------------------------------------------------------*
075900041206      * AGGTBE * Aggiornamento tabella                                *
076000041206      *---------------------------------------------------------------*
076100041206     c     AggTBE        BEGSR
076200091211      *
076300091211      /free
076400091211
076500091211         // -?Ciclo di elaborazione per ogni sistema informativo?
076600091211         For  w_SisInf = 1  To  %elem($Libr);
076700091211
076800091211           // -?Apertura (eventuale) del file ed aggancio record?
076900091211           if  w_SisInf > 1;
077000091211             exsr  sr_OpenFileTab;
077100091211             exsr  ChnTBE;
077200091211           endif;
077300091211
077400091211         //? N.B.                                                      ?
077500091211         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
077600091211         //   trasmissione (vedi flag TBEFTR).                        ?
077700091211         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
077800091211         //   subito nello stesso file di entrambi i S.I. - in due    ?
077900091211         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
078000091211
078100091211      /end-free
078200041206      *
078300041206sel 1c                   SELECT
078400041206      *
078500041206      * F5=Ripristino
078600041206w   1c                   WHEN      wTasto='05'  and  *in04
078700091211      /free
078800091211               if  %found(TNTBE01L);
078900091211      /end-free
079000041206     c                   clear                   TBEatb
079100041206     c                   clear                   TBEftr
079200041206     c                   UPDATE    TNTBE000
079300091211      /free
079400091211               else;
079500091211                 // -?Record appena cancellato fisicamente?
079600091211                 exsr  RieTBE;
079700091211                 WRITE  TNTBE000;
079800091211               endif;
079900091211      /end-free
080000041206      *
080100041206      * F6=Conferma
080200041206w   1c                   WHEN      wTasto='06'
080300041206     c                   exsr      RieTBE
080400041206sel 2c                   select
080500041206      *   Immissione
080600041206w   2c                   when      *in01
080700091211     c*//                clear                   TBEflt
080800091211     c*//                clear                   TBEdtr
080900041206     c                   WRITE     TNTBE000                             22
081000041206      *   Modifica / Ripristino
081100041206w   2c                   when      *in02   or    *in04
081200041206     c                   UPDATE    TNTBE000
081300041206e   2c                   endsl
081400041206      *
081500041206      * F16=Annullamento
081600041206w   1c                   WHEN      wTasto='16'  and  not *in04
081700041206     c                   movel     'A'           TBEatb
081800041206     c                   clear                   TBEftr
081900041206     c                   UPDATE    TNTBE000
082000041206      *
082100041206e   1c                   ENDSL
082200091211      /free
082300091211
082400091211         EndFor;
082500091211
082600091211      /end-free
082700041206      *
082800041206      * Torno alla prima videata che carico come da inizio pgm
082900041206     c                   movel     '1'           $TipVid
083000041206     c                   movel     *on           $CarV1
083100041206     c                   movel     *on           $CarV2
083200041206     c
083300041206     c                   ENDSR
083400041206
083500041206      *---------------------------------------------------------------*
083600041206      * RIETBE * Riempimento dati tabella                             *
083700041206      *---------------------------------------------------------------*
083800041206     c     RieTBE        BEGSR
083900041206      *
084000041206     c                   clear                   TBEatb
084100041206     c                   if        TBXsif <> *blanks
084200041206     c                   movel     KNSIF         TBEsif
084300041206     c                   else
084400041206     c                   clear                   TBEsif
084500041206     c                   endif
084600041206     c                   movel     TBXapl        TBEapl
084700060627     c                   movel     'AR8'         TBEcod
084800041206     c                   movel     V2Ctmd        TBEke1
084900041206     c                   clear                   TBEke2
085000041206      *
085100041206     c                   clear                   dAR8
085200041206     c                   eval      §AR8des = V2Dtmd
085300041206     c                   eval      §AR8tsd = V2Ctsd
085400050331     c                   eval      §AR8cxd = V2Ccxd
085500050601     c                   Eval      §ar8logof = v2clogof
085600041206     c                   movel(p)  dAR8          TBEuni
085700041206      *
085800091211     c*//                movel     W1FTT         TBEftt
085900091211     c                   movel     'S'           TBEftt
086000041206     c                   z-add     TBXflt        TBEflt
086100091211     c*//                clear                   TBEftr
086200091211      /free
086300091211
086400091211         select;
086500091211           // -?Annullamento o Ripristino?
086600091211           when  (wTasto='05'  and  *in04)   or
086700091211                 (wTasto='16'  and  not *in04);
086800091211             clear  TBEftr;
086900091211             clear  TBEdtr;
087000091211           // -?Aggiornamento o Scrittura in sede?
087100091211           when  w_SisInf = 1;
087200091211             TBEftr = 'T';
087300091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
087400091211                                     : 1 : 8 ) );
087500091211           // -?Aggiornamento o Scrittura in filiale?
087600091211           other;
087700091211             TBEftr = 'R';
087800091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
087900091211                                     : 1 : 8 ) );
088000091211         endsl;
088100091211
088200091211      /end-free
088300041206      *
088400041206     c                   ENDSR
088500041206
088600091211       //--------------------------------------------------------------
088700091211       //?Schiere a tempo di compilazione.                             ?
088800091211       //--------------------------------------------------------------
088900091211
089000091211** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
089100091211SETRAS  GAITRAGRU FILTRAGRU
089200091211AS888   GAITRAGRPSFILTRAGRPF
089300041206** - $Opz     *
089400041206  Inserimento
089500041206    Modifica
089600041206     Copia
089700041206  Annullamento
089800041206Visualizzazione
089900041206   ANNULLATO
090000041206** - $Msg                                                                    *
090100041206Immettere il tipo modulo DDT cliente                                           01
090200041206Descrizione obbligatoria                                                       02
090300041206Immettere il tipo stampa DDT                                                   03
090400041206Tipo stampa DDT errato                                                         04
090500041206AVVERTIMENTO: logo DDT non trovato                                             05
090600050601AVVERTIMENTO: logo per stampa fattura non trovato                              06
090700041206** - $Cmd                                                                      *
090800091211CHKOBJ    OBJ(GAITRAOBJ/&LOGO     )  OBJTYPE(*PAGSEG)                            01
