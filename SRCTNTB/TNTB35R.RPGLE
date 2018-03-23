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
002600130312
002700130312       // -?Tipo stampa DDT per immagini/documenti PDF allegati?
002800130312     d c_Tsd§§         c                   const('§§')
002900041206
003000041206      *---------------------------------------------------------------*
003100041206      *   S C H I E R E                                               *
003200041206      *---------------------------------------------------------------*
003300041206      *
003400091211       // -?iSeries  &  Librerie con entrambi i file tabelle?
003500091211     d $iSystem        s                   like(currSysNetA)
003600091211     d                                     dim(c_NrSyst)
003700091211     d                                     ctdata   perrcd( 1)
003800091211     d $Libraries      s                   like(ds_Libr)
003900091211     d                                     dim(c_NrSyst)
004000091211     d                                     alt($iSystem)
004100091211      *
004200041206     d $Opz            s             15    dim(06) ctdata perrcd(1)             Decodifica OPZ
004300130325     d $Msg            s             78    dim(10) ctdata perrcd(1)             Messaggi video
004400041206     d $Cmd            s             80    dim(5)  ctdata perrcd(1)             Comandi
004500041206
004600041206      *---------------------------------------------------------------*
004700041206      *   S T R U T T U R E   D A T I                                 *
004800041206      *---------------------------------------------------------------*
004900041206      *
005000041206      * Parametri
005100041206     d KPJBA         e ds
005200041206      *
005300041206      * Passaggio Parametri al pgm TIBS02R
005400041206     d TIBS02DS      e ds                  inz
005500041206      *
005600041206      * Tabella AR8 = Estensioni FIAR8 x TMD
005700041206     d dAR8          e ds                  inz
005800060627      * Tabella TSD = Tipo Stampa DDT
005900060627     d dtsd          e ds                  inz
006000041206      *
006100041206      * Tracciato record file TNTBE00F
006200041206     d TNTBEds       e ds                  extname(TNTBE00F) inz
006300041206     d xTNTBEds      e ds                  extname(TNTBE00F) inz
006400041206     d                                     prefix(TBX:3)
006500041206      *
006600041206     d TIBS34DS      e ds                  inz
006700041206     d DDATIUTE      e ds                  inz
006800041206     d AZUTEDS       e ds                  extname(AZUTE00F) inz
006900041206      *
007000041206     d WLBDAT          ds                  inz
007100041206     d  G02dat                 1      8  0 inz
007200041206     d  G02inv                 9     16  0 inz
007300041206     d  G02err                17     17    inz
007400041206     d  G02tgi                18     22  0 inz
007500091211       //
007600091211       // -?Ridefinizione elenco librerie in elaborare le tabelle?
007700091211     d ds_Libr         ds                  inz
007800091211     d  $Libr                        10    dim(c_NrLibr) inz
007900041206      *
008000041206     d                sds
008100041206     d  VTCpgm           *proc
008200041206
008300041206      *---------------------------------------------------------------*
008400041206      *   V A R I A B I L I                                           *
008500041206      *---------------------------------------------------------------*
008600041206      *
008700091211      * - Flags booleani
008800041206     d $Fine           s              1a   inz(*off)
008900041206     d $CarV1          s              1a   inz(*on)
009000041206     d $CarV2          s              1a   inz(*off)
009100041206     d $CarW1          s              1a   inz(*off)
009200091211     d $Tibs02         s              1a   inz(*off)
009300091211      * - Variabili per la gestione del video
009400041206     d $TipVid         s              1a   inz('1')
009500091211     d wTasto          s              2a   inz(*zeros)
009600091211     d wPos            s              2  0
009700091211      * - Nomi (completi) dei loghi
009800041206     d §Logo           s             10a   inz
009900091211     d §LogoF          s             10a   inz
010000091211      * - Parametri per QCMDEXC
010100041206     d Qcmd            s             80    inz
010200041206     d Qlen            s             15  5 inz(80)
010300091211       //
010400091211       // -?Nome del sistema?
010500091211     d currSysNeta     s              8a   inz
010600091211       // -?Nome esteso Libreria/File del file tabella?
010700091211     d wLibFile        s             21a   inz
010800091211       // -?Campi di comodo?
010900091211     d w_iSystem       s              1  0 inz
011000091211     d w_SisInf        s              3  0 inz
011100091211
011200091211       //--------------------------------------------------------------
011300091211       //?Definizione prototipi procedure.                             ?
011400091211       //--------------------------------------------------------------
011500091211
011600091211       // -?Reperimento NETA sistema AS/400 corrente?
011700091211      /copy gaitrasrc/srcProtoPr,UBRTVNETA
011800041206
011900041206      *---------------------------------------------------------------*
012000041206      *   M A I N   L I N E                                           *
012100041206      *---------------------------------------------------------------*
012200041206      *  Riepilogo indicatori utilizzati:                             *
012300041206      *  --------------------------------                             *
012400041206      *  01 - Record inesistente (inserimento)                        *
012500041206      *  02 - Record esistente   (modifica)                           *
012600041206      *  04 - Record annullato   (ripristino)                         *
012700041206      *  20 - Comodo                                                  *
012800041206      *  22 - Errori in scrittura record (WRITE)                      *
012900130311      *  41 - Protegge e NON Visualizza i campi della 2ª videata      *
013000130311      *       che NON servono ai documenti in formato PDF (§...)      *
013100130312      *  42 - Protegge "Tipo stampa DDT" se correttamente inserito §§ *
013200130312      *       o se inserimento per codice §...                        *
013300041206      *  50 - Posizionamento cursore su "Tipo modulo DDT cliente"     *
013400041206      *  51 - Posizionamento cursore su "Descrizione tipo modulo DDT" *
013500041206      *  52 - Posizionamento cursore su "Tipo stampa DDT"             *
013600050601      *  53 - Posizionamento cursore su logo per stampa fattura       *
013700041206      *  99 - Visualizzazione messaggio di errore                     *
013800041206      *---------------------------------------------------------------*
013900041206      *
014000041206      * Operazioni iniziali
014100041206     c                   exsr      RutInz
014200041206      *
014300041206      * Gestione video
014400041206     c                   dow       $Fine = *off
014500041206     c     $TipVid       caseq     '1'           GesV1
014600041206     c     $TipVid       caseq     '2'           GesV2
014700041206     c     $TipVid       caseq     'A'           GesW1
014800041206     c                   endcs
014900041206     c                   enddo
015000041206      *
015100041206      * Fine
015200041206     c                   if        $Tibs02 = *on
015300041206     c                   clear                   TIBS02ds
015400041206     c                   movel     'C'           T02tla
015500041206     c                   call      'TIBS02R'
015600041206     c                   parm                    KPJBA
015700041206     c                   parm                    TIBS02ds
015800041206     c                   endif
015900041206      *
016000091211     c***                return
016100041206
016200041206      *---------------------------------------------------------------*
016300041206      * RutInz - Operazioni Iniziali                                  *
016400041206      *---------------------------------------------------------------*
016500041206     c     RutInz        BEGSR
016600041206      *
016700041206      * Ricezione parametri
016800041206     c     *entry        plist
016900041206     c                   parm                    KPJBA
017000041206      *
017100041206      * Definizioni chiavi di accesso
017200041206     c     K05TBE01      klist                                                  *tntbe01l
017300041206     c                   kfld                    TBEcod                         -tabella
017400041206     c                   kfld                    TBEke1                         -chiave uno
017500041206     c                   kfld                    TBEke2                         -chiave due
017600041206     c                   kfld                    TBElin                         -lingua
017700041206     c                   kfld                    TBEsif                         -s.informativo
017800091211      /free
017900091211
018000091211         *inLR = *on;
018100091211
018200091211         // -?Verifica del sistema AS/400 corrente?
018300091211         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
018400091211           $Fine = *on;
018500091211           leavesr;
018600091211         endif;
018700091211
018800091211         // -?Impostazione elenco librerie in cui gestire le tabelle?
018900091211         //  ?(a seconda del sistema in cui si stà lavorando)?
019000091211         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
019100091211         if  w_iSystem = *zero;
019200091211           $Fine = *on;
019300091211           leavesr;
019400091211         endif;
019500091211
019600091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
019700091211         w_SisInf = 1;
019800091211         exsr  sr_OpenFileTab;
019900091211
020000091211      /end-free
020100041206      *
020200041206      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
020300041206     c     *dtaara       define    §azute        azuteds
020400041206     c     *dtaara       define    §datiute      ddatiute
020500041206      *
020600041206     c                   clear                   AzUteDs
020700041206     c                   clear                   DDatiUte
020800041206     c                   clear                   Tibs34Ds
020900041206     c                   in(E)     *dtaara
021000041206if  1c                   if        %Error  or  RSUT = *blanks
021100041206     c                   call      'TIBS34R'
021200041206     c                   parm                    Tibs34Ds
021300041206     c                   in        *dtaara
021400041206e   1c                   endif
021500041206      *-- Verifica errori e autorità profilo
021600041206sel 1c                   SELECT
021700041206      *-- controllo se ho errori nei dati utente
021800041206      *--   nel qual caso NON risulta un profilo abilitato
021900041206w   1c                   WHEN      DUTerr = 'E'
022000041206     c                   eval      $Fine  = *on
022100041206      *
022200041206      *-- CONTROLLO AUTORITA'
022300041206      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
022400041206      *-- se il chiamante non richiede autorità specifica verificare
022500041206      *--   quella del profilo
022600041206      *-- se il chiamante richiede autorità specifica verificarla,
022700041206      *--  se è blank verificare quella del profilo
022800041206      *
022900041206      * se UTEAUT = *BLANK non siamo in sede
023000041206w   1c                   WHEN      UTEaut = *blank
023100041206      *
023200041206x   1c                   OTHER
023300041206      *
023400041206e   1c                   ENDSL
023500041206      *
023600041206      * Aggancio dati generali della tabella in esame
023700041206     c                   clear                   TBEcod
023800041206     c                   move      *zeros        TBEke1
023900091211     c                   move      'AR8'         TBEke1
024000041206     c                   clear                   TBEke2
024100041206     c                   clear                   TBElin
024200041206     c                   movel     KNSIF         TBEsif
024300091211     c     K05TBE01      chain(n)  TNTBE01L
024400041206     c                   if        not %found(TNTBE01L)
024500041206     c                   clear                   TBEsif
024600091211     c     K05TBE01      chain(n)  TNTBE01L
024700041206     c                   endif
024800041206     c                   if        %found(TNTBE01L)
024900041206     c                   movel     TNTBEds       xTNTBEds
025000041206     c                   else
025100041206     c                   clear                   xTNTBEds
025200041206     c                   endif
025300041206      *
025400041206     c                   clear                   TB35V1
025500041206      *
025600041206     c                   ENDSR
025700091211      /free
025800091211
025900091211       //--------------------------------------------------------------
026000091211       //?Apertura dei files tabelle nel sistema informativo impostato.?
026100091211       //--------------------------------------------------------------
026200091211       BEGSR  sr_OpenFileTab;
026300091211
026400091211         // -?Chiusura (eventuale) archivio?
026500091211         if  %open(TNTBE01L);
026600091211           close  TNTBE01L;
026700091211         endif;
026800091211
026900091211         // -?Apertura archivio?
027000091211         ds_Libr  = $Libraries(w_iSystem);
027100091211         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
027200091211         open  TNTBE01L;
027300091211
027400091211       ENDSR;
027500091211
027600091211      /end-free
027700041206
027800041206      *---------------------------------------------------------------*
027900041206      * GESV1  - Gestione videata selezione codice tabella            *
028000041206      *---------------------------------------------------------------*
028100041206     c     GesV1         BEGSR
028200041206      *
028300041206      * Inizializzazione videata
028400041206if  1c                   if        $CarV1 = *on
028500041206     c                   exsr      CarV1
028600041206     c                   movel     *off          $CarV1
028700041206e   1c                   endif
028800041206      *
028900041206      * Scrivo la testata
029000041206     c                   clear                   T1opz
029100041206     c                   write     TB35T1
029200041206      *
029300041206      * Se esistono errori sulla videata
029400041206      * emetto la write del formato a indicatori spenti per vedere
029500041206      * le eventuali decodifiche
029600041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
029700041206      *
029800041206     c                   exfmt     TB35V1
029900041206     c                   eval      *in99 = *off
030000041206     c                   clear                   V1Dmsg
030100041206      * F3=Fine
030200041206if  1c                   if        *inKC
030300041206     c                   exsr      F03V1
030400041206     c                   goto      EndGesV1
030500041206e   1c                   endif
030600041206      *
030700041206      * Controllo dati immessi a video
030800041206     c                   exsr      CtrV1
030900041206     c     *in99         cabeq     *on           EndGesV1
031000041206      *
031100041206      * Passaggio alla videata di dettaglio
031200041206if  1c                   if        not *in99
031300041206     c                   eval      $CarV2 = *on
031400041206     c                   eval      $TipVid = '2'
031500041206e   1c                   endif
031600041206      *
031700041206     c     EndGesV1      ENDSR
031800041206
031900041206      *---------------------------------------------------------------*
032000041206      * CARV1  - Caricamento dati prima videata                       *
032100041206      *---------------------------------------------------------------*
032200041206     c     CarV1         BEGSR
032300041206      *
032400041206     c                   movea     *zeros        *in(50)
032500041206     c                   movea     '00000'       *in(01)
032600041206      *
032700041206      * Imposto il campo "tipo modulo DDT cliente" per l'interrogazione
032800041206     c                   movel(p)  '?'           V1Ctmd
032900041206      *
033000041206     c                   ENDSR
033100041206
033200041206      *---------------------------------------------------------------*
033300041206      * CTRV1  - Controllo e decodifica prima videata                 *
033400041206      *---------------------------------------------------------------*
033500041206     c     CtrV1         BEGSR
033600041206      *
033700041206     c                   movea     *zeros        *in(50)
033800041206     c                   clear                   V1Dmsg
033900041206      *
034000041206      * Tipo modulo DDT
034100041206      * - Ricerca:
034200041206     c     '?'           scan      V1Ctmd                                 20
034300041206if  1c                   if        *in20
034400041206     c                   eval      $Tibs02 = *on
034500041206     c                   clear                   V1Ctmd
034600041206     c                   eval      *in50   = *on
034700041206     c                   reset                   TIBS02ds
034800041206     c                   movel     KNSIF         T02sif
034900060627     c                   eval      t02mod = 'R'
035000060627     c                   eval      t02cod = 'AR8'
035100041206     c                   call      'TIBS02R'
035200041206     c                   parm                    KPJBA
035300041206     c                   parm                    TIBS02ds
035400041206if  2c                   if        T02err  = *blanks
035500041206     c                   movel     T02uni        dAR8
035600041206     c                   movel     T02ke1        V1Ctmd
035700041206x   2c                   else
035800041206     c                   eval      *in99   = *on
035900041206     c                   movel     T02msg        V1Dmsg
036000041206     c                   endif
036100041206e   1c                   endif
036200041206      * - Controllo:
036300041206if  1c                   if        V1Ctmd  = *blanks
036400041206     c                   seton                                        50  99
036500041206     c                   eval      V1Dmsg  = $Msg(01)
036600130325     c                   leavesr
036700041206e   1c                   endif
036800130325      *
036900130325     c                   if        %subst(V1Ctmd : 1 : 1) <  'A'  and
037000130325     c                             %subst(V1Ctmd : 1 : 1) <> '§'
037100130325     c                   seton                                        50  99
037200130325     c                   eval      V1Dmsg  = $Msg(10)
037300130325     c                   leavesr
037400130325     c                   endif
037500041206      *
037600130325     c                   ENDSR
037700041206
037800041206      *---------------------------------------------------------------*
037900041206      * F03V1  - Tasto funzionale F03 -> Fine programma               *
038000041206      *---------------------------------------------------------------*
038100041206     c     F03V1         BEGSR
038200041206      *
038300041206     c                   movel     *on           $Fine                          fine pgm
038400041206      *
038500041206     c                   ENDSR
038600041206
038700041206      *---------------------------------------------------------------*
038800041206      * GESV2  - Gestione videata dettaglio dati                      *
038900041206      *---------------------------------------------------------------*
039000041206     c     GesV2         BEGSR
039100041206      *
039200041206      * Inizializzazione videata
039300041206     c                   if        $CarV2 = *on
039400041206     c                   exsr      CarV2
039500041206     c                   move      *off          $CarV2
039600041206     c                   endif
039700041206      * Scrivo la testata
039800041206     c                   write     TB35T1
039900041206      *
040000041206      * Se esistono errori sulla videata
040100041206      * emetto la write del formato a indicatori spenti per vedere
040200041206      * le eventuali decodifiche
040300041206      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
040400060627      *?ora ce n'è uno?
040500041206      *
040600041206if  1c                   if        *in05
040700041206     c                   write     TB35V2
040800041206     c                   exfmt     PROTECT
040900041206x   1c                   else
041000041206     c                   exfmt     TB35V2
041100041206e   1c                   endif
041200041206     c                   eval      *in99 = *off
041300041206     c                   clear                   V1Dmsg
041400041206     c                   clear                   wTasto
041500041206      *
041600041206sel 1c                   select
041700041206      * F03=Fine
041800041206w   1c                   when      *inKC
041900041206     c                   exsr      F03V1
042000041206     c                   goto      EndGesV2
042100041206      * F12=Ritorno
042200041206w   1c                   when      *inKL
042300041206     c                   exsr      F12V2
042400041206     c                   goto      EndGesV2
042500041206e   1c                   endsl
042600041206      *
042700041206      * Controllo dati immessi a video
042800041206      * (non si fanno se richisto l'annullamento)
042900041206if  1c                   if        not *inKQ
043000041206     c                   exsr      CtrV2
043100041206e   1c                   endif
043200041206      *
043300041206      * Aggiornamento se non ci sono errori
043400041206if  1c                   if        not *in99
043500041206     c                             and (*inKF or *inKE or *inKQ)
043600041206sel 2c                   select
043700041206w   2c                   when      *inKE
043800041206     c                   eval      wTasto = '05'
043900041206w   2c                   when      *inKF
044000041206     c                   eval      wTasto = '06'
044100041206w   2c                   when      *inKQ
044200041206     c                   eval      wTasto = '16'
044300041206e   2c                   endsl
044400091211       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
044500091211       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
044600091211       //  ?NON ha più senso gestire la richiesta di trasmissione!?
044700091211     ***c                   eval      $CarW1 = *on
044800091211     ***c                   eval      $TipVid = 'A'
044900091211     c                   exsr      AggTBE
045000041206e   1c                   endif
045100041206      *
045200041206     c     EndGesV2      ENDSR
045300041206
045400041206      *---------------------------------------------------------------*
045500041206      * CARV2  - Caricamento dati seconda videata                     *
045600041206      *---------------------------------------------------------------*
045700041206     c     CarV2         BEGSR
045800041206      *
045900041206     c                   clear                   T1opz
046000041206     c                   movea     '00000'       *in(01)
046100041206     c                   clear                   §Logo
046200091211     c                   clear                   §LogoF
046300041206      *
046400041206      * reimposto il campo chiave (qui di solo output)
046500041206     c                   eval      V2Ctmd = V1Ctmd
046600041206      *
046700041206      * ripulisco i campi in manutenzione in questa videata
046800041206      * (e relative decodifiche)
046900041206     c                   clear                   V2Dtmd
047000041206     c                   clear                   V2Ctsd
047100041206     c                   clear                   V2Dtsd
047200091211     c                   clear                   V2Clogof
047300091211      *
047400091211      /free
047500091211
047600091211         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
047700091211         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
047800091211           w_SisInf = 1;
047900091211           exsr  sr_OpenFileTab;
048000091211         endif;
048100130311
048200130311         // -?Verifica se codice logo per PDA ("§...")?
048300130311         //  ?per NON visualizzare a video le richieste di:?
048400130311         //  ?· stampa indicazione di "COPIA PER IL DESTINATARIO" (S/N)?
048500130311         //  ?· cod. logo per stampa fattura?
048600130311         *in41 = (%subst(V1Ctmd : 1 : 1) = '§');
048700091211
048800091211      /end-free
048900041206      *
049000041206      * Aggancio la tabella, se trovo il codice sono in modifica
049100041206      * o ripristino (se record annullato), altrimenti in immissione
049200041206     c                   exsr      ChnTBE
049300041206      *
049400041206if  1c                   if        %found(TNTBE01L)
049500041206      *
049600041206      * MODIFICA/RIPRISTINO
049700041206     c                   movel     TBEuni        dAR8
049800041206if  2c                   if        TBEatb = *blanks
049900041206     c                   eval      *in02  = *on
050000041206     c                   eval      T1opz  = $Opz(02)
050100041206x   2c                   else
050200041206     c                   eval      *in04  = *on
050300041206     c                   eval      T1opz  = $Opz(06)
050400041206e   2c                   endif
050500041206      *
050600041206x   1c                   else
050700041206      *
050800041206      * IMMISSIONE
050900041206     c                   clear                   dAR8
051000041206     c                   eval      *in01  = *on
051100041206     c                   eval      T1opz  = $Opz(01)
051200041206      *
051300041206e   1c                   endif
051400041206      *
051500041206     c                   eval      V2Ctmd = V1Ctmd
051600041206      *
051700041206     c                   eval      V2Dtmd = §AR8des
051800041206     c                   eval      V2Ctsd = §AR8tsd
051900050331     c                   eval      V2Ccxd = §AR8cxd
052000050601     c                   Eval      v2clogof = §ar8logof
052100130312      *
052200130312     c                   eval      *in42  = *off
052300130312sel 1c                   select
052400130312w   1c                   when           Not %found(TNTBE01L)
052500130312     c                             and  %subst(V2Ctmd : 1 : 1) = '§'
052600130312     c                   eval      V2Ctsd = c_Tsd§§
052700130312     c                   eval      *in42  = *on
052800130312w   1c                   when           %found(TNTBE01L)
052900130312     c                             and  %subst(V2Ctmd : 1 : 1) = '§'
053000130312     c                             and  §AR8tsd                = c_Tsd§§
053100130312     c                   eval      *in42  = *on
053200130312e   1c                   endsl
053300130312      *
053400060627      * decodifico tipo stampa ddt
053500060627     c                   if        v2ctsd <> *blanks
053600060627     c                   clear                   v2dtsd
053700060627     c                   eval      $Tibs02 = *on
053800060627     c                   clear                   tibs02ds
053900060627     c                   eval      t02mod = 'C'
054000060627     c                   eval      t02cod = 'TSD'
054100060627     c                   eval      t02ke1 = v2ctsd
054200060627     c                   eval      t02sif = knsif
054300060627     c                   call      'TIBS02R'
054400060627     c                   parm                    kpjba
054500060627     c                   parm                    tibs02ds
054600060627     c                   if        t02err = *blanks
054700060627     c                   eval      dtsd = t02uni
054800060627     c                   eval      v2dtsd = §tsddes
054900060627     c                   endif
055000060627     c                   endif
055100041206      *
055200041206     c                   ENDSR
055300041206
055400041206      *---------------------------------------------------------------*
055500041206      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
055600041206      *---------------------------------------------------------------*
055700041206     c     F12V2         BEGSR
055800041206      *
055900091211      /free
056000091211         unlock  TNTBE01L;
056100091211      /end-free
056200041206     c                   eval      $TipVid = '1'
056300041206      *
056400041206     c                   ENDSR
056500041206
056600041206      *---------------------------------------------------------------*
056700041206      * CTRV2  - Controllo e decodifica seconda videata               *
056800041206      *---------------------------------------------------------------*
056900041206     c     CtrV2         BEGSR
057000041206      *
057100041206     c                   movea     *zeros        *in(50)
057200041206     c                   clear                   V1Dmsg
057300041206      *
057400041206      * Descrizione tipo modulo DDT obbligatorio
057500041206if  1c                   if        V2Dtmd = *blanks
057600041206     c                   seton                                        51  99
057700041206     c                   movel     $Msg(02)      V1Dmsg
057800041206     c                   goto      EndCtrV2
057900041206e   1c                   endif
058000041206      *
058100060627      * Tipo stampa DDT obbligatorio
058200130312if  1c                   if        v2ctsd = *blanks
058300060627     c                   eval      *in52 = *on
058400060627     c                   eval      *in99 = *on
058500060627     c                   movel     $msg(04)      v1dmsg
058600060627     c                   goto      endctrv2
058700130312e   1c                   endif
058800060627      * ricerca tipo stampa DDT
058900060627     c                   eval      wpos = %scan('?':v2ctsd)
059000130312if  1c                   if        wpos > 0
059100060627     c                   clear                   tibs02ds
059200060627     c                   clear                   v2ctsd
059300060627     c                   eval      $Tibs02 = *on
059400060627     c                   eval      *in52 = *on
059500060627     c                   eval      t02mod = 'R'
059600060627     c                   eval      t02cod = 'TSD'
059700060627     c                   eval      t02sif = knsif
059800060627     c                   call      'TIBS02R'
059900060627     c                   parm                    kpjba
060000060627     c                   parm                    tibs02ds
060100130312if  2c                   if        t02err = *blanks
060200060627     c                   eval      dtsd = t02uni
060300060627     c                   movel     t02ke1        v2ctsd
060400130312x   2c                   else
060500060627     c                   eval      *in99 = *on
060600060627     c                   eval      v1dmsg = t02msg
060700130312e   2c                   endif
060800060627     c                   goto      endctrv2
060900130312e   1c                   endif
061000060627      * controllo tipo stampa DDT
061100060627     c                   eval      $Tibs02 = *on
061200060627     c                   clear                   tibs02ds
061300060627     c                   eval      t02mod = 'C'
061400060627     c                   eval      t02cod = 'TSD'
061500060627     c                   eval      t02ke1 = v2ctsd
061600060627     c                   eval      t02sif = knsif
061700060627     c                   call      'TIBS02R'
061800060627     c                   parm                    kpjba
061900060627     c                   parm                    tibs02ds
062000130312if  1c                   if        t02err = *blanks
062100060627     c                   eval      dtsd = t02uni
062200060627     c                   eval      v2dtsd = §tsddes
062300130312x   1c                   else
062400060627     c                   clear                   v2dtsd
062500041206     c                   seton                                        52  99
062600041206     c                   movel     $Msg(04)      V1Dmsg
062700041206     c                   goto      EndCtrV2
062800130312e   1c                   endif
062900130312      *
063000130312sel 1c                   select
063100130312w   1c                   when      %subst(V2Ctmd : 1 : 1) <> '§'
063200130312     c                             and    V2Ctsd          =  c_Tsd§§
063300130312     c                   seton                                        52  99
063400130312     c                   movel     $Msg(08)      V1Dmsg
063500130312     c                   leavesr
063600130312w   1c                   when      %subst(V2Ctmd : 1 : 1) = '§'
063700130312     c                             and    V2Ctsd         <> c_Tsd§§
063800130312     c                   seton                                        52  99
063900130312     c                   movel     $Msg(09)      V1Dmsg
064000130312     c                   leavesr
064100130312e   1c                   endsl
064200041206      *
064300041206      * Logo DDT inesistente
064400041206if  1c                   if            V2Ctsd <> 'S1'
064500041206     c                             and §Logo  <> ('LOGO' + V2Ctmd)
064600041206     c                   eval      §Logo  = 'LOGO' + V2Ctmd
064700041206     c                   eval      Qcmd  = %replace(§Logo:$Cmd(1):
064800041206     c                                     %scan('&LOGO     ':
064900041206     c                                           $Cmd(1)))
065000041206     c                   call      'QCMDEXC'                            20
065100041206     c                   parm                    Qcmd
065200041206     c                   parm                    Qlen
065300041206      * (messaggio di solo avviso)
065400130312sel 2c                   select
065500130312w   2c                   when          *in20  =  *on
065600130312     c                             and V2Ctsd <> c_Tsd§§
065700041206     c                   seton                                        52  99
065800041206     c                   movel     $Msg(05)      V1Dmsg
065900041206     c                   goto      EndCtrV2
066000130312w   2c                   when          *in20  = *off
066100130312     c                             and V2Ctsd = c_Tsd§§
066200130312     c                   seton                                        52  99
066300130312     c                   movel     $Msg(07)      V1Dmsg
066400130312     c                   leavesr
066500130312e   2c                   endsl
066600041206e   1c                   endif
066700050601
066800050601      * Controllo se esiste il logo per la stampa fattura
066900050601if  1c                   If        v2clogof <> *Blanks
067000050601     c                             and §logof <> ('LOGO' + v2clogof)
067100050601     c                   eval      §logof = 'LOGO' + v2clogof
067200050601     c                   eval      Qcmd  = %replace(§Logof:$Cmd(1):
067300050601     c                                     %scan('&LOGO     ':
067400050601     c                                           $Cmd(1)))
067500050601     c                   call      'QCMDEXC'                            20
067600050601     c                   parm                    Qcmd
067700050601     c                   parm                    Qlen
067800050601      * (messaggio di solo avviso)
067900050601if  2c                   if            *in20 = *on
068000050601     c                   seton                                        53  99
068100050601     c                   movel     $Msg(06)      V1Dmsg
068200050601     c                   goto      EndCtrV2
068300050601e   2c                   endif
068400050601e   1c                   endif
068500041206      *
068600041206     c     EndCtrV2      ENDSR
068700041206
068800041206      *---------------------------------------------------------------*
068900041206      * GESW1  - Gestione videata dati relativi alla trasmissione     *
069000041206      *---------------------------------------------------------------*
069100041206     c     GesW1         BEGSR
069200041206      *
069300041206      * Inizializzazione videata
069400041206if  1c                   if        $CarW1 = *on
069500041206     c                   exsr      CarW1
069600041206     c                   movel     *off          $CarW1
069700041206e   1c                   endif
069800041206      *
069900041206if  1c                   if        *in05
070000041206     c                   write     TB35W1
070100041206     c                   exfmt     PROTECT
070200041206x   1c                   else
070300041206     c                   exfmt     TB35W1
070400041206e   1c                   endif
070500041206     c                   eval      *in99 = *off
070600041206     c                   clear                   W1MSG
070700041206      *
070800041206sel 1c                   select
070900041206      * F12=Ritorno
071000041206w   1c                   when      *inKL
071100041206     c                   exsr      F12W1
071200041206     c                   goto      EndGesW1
071300041206e   1c                   endsl
071400041206      *
071500041206      * Controllo dati immessi a video
071600041206     c                   exsr      CtrW1
071700041206      *
071800041206      * Aggiornamento se non ci sono errori
071900041206if  1c                   if        not *in99 and *inKF
072000041206     c                   exsr      AggTBE
072100041206e   1c                   endif
072200041206      *
072300041206     c     EndGesW1      ENDSR
072400041206
072500041206      *---------------------------------------------------------------*
072600041206      * CARW1  - Caricamento dati window                              *
072700041206      *---------------------------------------------------------------*
072800041206     c     CarW1         BEGSR
072900041206      *
073000041206     c                   movea     *zeros        *in(50)
073100041206      *
073200041206sel 1c                   select
073300041206      *
073400041206      * F5=Ripristino
073500041206w   1c                   when      *inKE   and  *in04
073600041206     c                   eval      W1FTT = TBEftt
073700041206      *
073800041206      * F6=Conferma
073900041206w   1c                   when      *inKF
074000041206sel 2c                   select
074100041206      *   Immissione
074200041206w   2c                   when      *in01
074300041206     c                   eval      W1FTT = TBXftt
074400041206      *   Modifica / Ripristino
074500041206w   2c                   when      *in02   or    *in04
074600041206     c                   eval      W1FTT = TBEftt
074700041206e   2c                   endsl
074800041206      *
074900041206      * F16=Annullamento
075000041206w   1c                   when      *inKQ   and  not *in04
075100041206     c                   eval      W1FTT = TBEftt
075200041206      *
075300041206e   1c                   endsl
075400041206      *
075500041206      * Se NON immissione: visualizzo i dati relativi all'ultima
075600041206      *   trasmissione
075700041206if  1c                   if        not *in01
075800041206     c                   eval      W1FLT = TBEflt
075900041206     c                   eval      W1FTR = TBEftr
076000041206if  2c                   if        TBEdtr <> 0
076100041206     c                   clear                   WLBDAT
076200041206     c                   z-add     TBEdtr        G02inv
076300041206     c                   movel     '3'           G02err
076400041206     c                   call      'XSRDA8'
076500041206     c                   parm                    WLBDAT
076600041206     c                   z-add     G02dat        W1DTR
076700041206e   2c                   endif
076800041206e   1c                   endif
076900041206      *
077000041206     c                   ENDSR
077100041206
077200041206      *---------------------------------------------------------------*
077300041206      * CTRW1  - Controllo e decodifica window                        *
077400041206      *---------------------------------------------------------------*
077500041206     c     CtrW1         BEGSR
077600041206      *
077700041206     c                   movea     *zeros        *in(50)
077800041206      *
077900041206     c     EndCtrW1      ENDSR
078000041206
078100041206      *---------------------------------------------------------------*
078200041206      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
078300041206      *---------------------------------------------------------------*
078400041206     c     F12W1         BEGSR
078500041206      *
078600041206     c                   eval      $TipVid = '2'
078700041206      *
078800041206     c                   ENDSR
078900041206
079000041206      *---------------------------------------------------------------*
079100041206      * CHNTBE * Aggancio tabella                                     *
079200041206      *---------------------------------------------------------------*
079300041206     c     ChnTBE        BEGSR
079400041206      *
079500060627     c                   movel     'AR8'         TBEcod
079600041206     c                   movel(p)  V2Ctmd        TBEke1
079700041206     c                   clear                   TBEke2
079800041206     c                   clear                   TBElin
079900041206     c                   movel     KNSIF         TBEsif
080000041206     c     K05TBE01      chain     TNTBE01L
080100041206      * Se non ho trovato il record con il sistema informativo
080200041206      * che ho in linea, lo abblenco
080300041206if  1c                   if        not %found(TNTBE01L)
080400041206     c                   clear                   TBEsif
080500041206     c     K05TBE01      chain     TNTBE01L
080600041206e   1c                   endif
080700041206      *
080800041206     c                   ENDSR
080900041206
081000041206      *---------------------------------------------------------------*
081100041206      * AGGTBE * Aggiornamento tabella                                *
081200041206      *---------------------------------------------------------------*
081300041206     c     AggTBE        BEGSR
081400091211      *
081500091211      /free
081600091211
081700091211         // -?Ciclo di elaborazione per ogni sistema informativo?
081800091211         For  w_SisInf = 1  To  %elem($Libr);
081900091211
082000091211           // -?Apertura (eventuale) del file ed aggancio record?
082100091211           if  w_SisInf > 1;
082200091211             exsr  sr_OpenFileTab;
082300091211             exsr  ChnTBE;
082400091211           endif;
082500091211
082600091211         //? N.B.                                                      ?
082700091211         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
082800091211         //   trasmissione (vedi flag TBEFTR).                        ?
082900091211         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
083000091211         //   subito nello stesso file di entrambi i S.I. - in due    ?
083100091211         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
083200091211
083300091211      /end-free
083400041206      *
083500041206sel 1c                   SELECT
083600041206      *
083700041206      * F5=Ripristino
083800041206w   1c                   WHEN      wTasto='05'  and  *in04
083900091211      /free
084000091211               if  %found(TNTBE01L);
084100091211      /end-free
084200041206     c                   clear                   TBEatb
084300041206     c                   clear                   TBEftr
084400041206     c                   UPDATE    TNTBE000
084500091211      /free
084600091211               else;
084700091211                 // -?Record appena cancellato fisicamente?
084800091211                 exsr  RieTBE;
084900091211                 WRITE  TNTBE000;
085000091211               endif;
085100091211      /end-free
085200041206      *
085300041206      * F6=Conferma
085400041206w   1c                   WHEN      wTasto='06'
085500041206     c                   exsr      RieTBE
085600041206sel 2c                   select
085700041206      *   Immissione
085800041206w   2c                   when      *in01
085900091211     c*//                clear                   TBEflt
086000091211     c*//                clear                   TBEdtr
086100041206     c                   WRITE     TNTBE000                             22
086200041206      *   Modifica / Ripristino
086300041206w   2c                   when      *in02   or    *in04
086400041206     c                   UPDATE    TNTBE000
086500041206e   2c                   endsl
086600041206      *
086700041206      * F16=Annullamento
086800041206w   1c                   WHEN      wTasto='16'  and  not *in04
086900041206     c                   movel     'A'           TBEatb
087000041206     c                   clear                   TBEftr
087100041206     c                   UPDATE    TNTBE000
087200041206      *
087300041206e   1c                   ENDSL
087400091211      /free
087500091211
087600091211         EndFor;
087700091211
087800091211      /end-free
087900041206      *
088000041206      * Torno alla prima videata che carico come da inizio pgm
088100041206     c                   movel     '1'           $TipVid
088200041206     c                   movel     *on           $CarV1
088300041206     c                   movel     *on           $CarV2
088400041206     c
088500041206     c                   ENDSR
088600041206
088700041206      *---------------------------------------------------------------*
088800041206      * RIETBE * Riempimento dati tabella                             *
088900041206      *---------------------------------------------------------------*
089000041206     c     RieTBE        BEGSR
089100041206      *
089200041206     c                   clear                   TBEatb
089300041206     c                   if        TBXsif <> *blanks
089400041206     c                   movel     KNSIF         TBEsif
089500041206     c                   else
089600041206     c                   clear                   TBEsif
089700041206     c                   endif
089800041206     c                   movel     TBXapl        TBEapl
089900060627     c                   movel     'AR8'         TBEcod
090000041206     c                   movel     V2Ctmd        TBEke1
090100041206     c                   clear                   TBEke2
090200041206      *
090300041206     c                   clear                   dAR8
090400041206     c                   eval      §AR8des = V2Dtmd
090500041206     c                   eval      §AR8tsd = V2Ctsd
090600050331     c                   eval      §AR8cxd = V2Ccxd
090700050601     c                   Eval      §ar8logof = v2clogof
090800041206     c                   movel(p)  dAR8          TBEuni
090900041206      *
091000091211     c*//                movel     W1FTT         TBEftt
091100091211     c                   movel     'S'           TBEftt
091200041206     c                   z-add     TBXflt        TBEflt
091300091211     c*//                clear                   TBEftr
091400091211      /free
091500091211
091600091211         select;
091700091211           // -?Annullamento o Ripristino?
091800091211           when  (wTasto='05'  and  *in04)   or
091900091211                 (wTasto='16'  and  not *in04);
092000091211             clear  TBEftr;
092100091211             clear  TBEdtr;
092200091211           // -?Aggiornamento o Scrittura in sede?
092300091211           when  w_SisInf = 1;
092400091211             TBEftr = 'T';
092500091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
092600091211                                     : 1 : 8 ) );
092700091211           // -?Aggiornamento o Scrittura in filiale?
092800091211           other;
092900091211             TBEftr = 'R';
093000091211             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
093100091211                                     : 1 : 8 ) );
093200091211         endsl;
093300091211
093400091211      /end-free
093500041206      *
093600041206     c                   ENDSR
093700041206
093800091211       //--------------------------------------------------------------
093900091211       //?Schiere a tempo di compilazione.                             ?
094000091211       //--------------------------------------------------------------
094100091211
094200091211** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
094300091211SETRAS  GAITRAGRU FILTRAGRU
094400091211AS888   GAITRAGRPSFILTRAGRPF
094500041206** - $Opz     *
094600041206  Inserimento
094700041206    Modifica
094800041206     Copia
094900041206  Annullamento
095000041206Visualizzazione
095100041206   ANNULLATO
095200041206** - $Msg                                                                    *
095300041206Immettere il tipo modulo DDT cliente                                           01
095400041206Descrizione obbligatoria                                                       02
095500041206Immettere il tipo stampa DDT                                                   03
095600041206Tipo stampa DDT errato                                                         04
095700041206AVVERTIMENTO: logo DDT non trovato                                             05
095800050601AVVERTIMENTO: logo per stampa fattura non trovato                              06
095900130312AVVERTIMENTO: reperito logo con questo nome (nonostante codice per PDF)        07
096000130312Il tipo stampa DDT "§§" richiede un tipo modulo DDT cliente che inizia con "§" 08
096100130312Il tipo modulo DDT cliente che inizia con "§" richiede il tipo stampa DDT "§§" 09
096200130325Carattere iniziale del "Tipo Modulo DDT"  NON  ammesso - vedi note             10
096300041206** - $Cmd                                                                      *
096400091211CHKOBJ    OBJ(GAITRAOBJ/&LOGO     )  OBJTYPE(*PAGSEG)                            01
