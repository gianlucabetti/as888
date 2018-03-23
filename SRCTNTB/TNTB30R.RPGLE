000100120112     /*END
000200030805      *---------------------------------------------------------------*
000300030805      * Gestione tabella "OSR" = Serie O.R.M.                         *
000400030805      *---------------------------------------------------------------*
000500030805
000600091203     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700091203     h dftactgrp(*no)
000800091203     h bnddir('UBRTVNETA')
000900030805
001000030805      *---------------------------------------------------------------*
001100030805      *   A R C H I V I                                               *
001200030805      *---------------------------------------------------------------*
001300030805      *
001400030805     fAZORG01L  if   e           k disk
001500090525     ffnacr01l  if   e           k disk
001600030805     fTABEL00F  if   e           k disk
001700030805      *
001800091203     fTNTBE01L  Uf A e           k disk
001900091203     f                                     extfile(wLibFile)  usropn
002000030805      *
002100030805     fTNTB30D   cf   e             workstn
002200030805
002300030805      *---------------------------------------------------------------*
002400030805      *   C O S T A N T I                                             *
002500030805      *---------------------------------------------------------------*
002600030805      *
002700091203      * - Costante per controllo "caratteri solo numerici"
002800120112     d c_Digits        c                   const('0123456789')
002900091203       //
003000091203       // -?Costanti per la definizione delle schiere con i nomi?
003100091203       //  ?degli iSeries da elaborare e delle relative librerie?
003200091203     d c_NrSyst        c                   const(2)
003300091203     d c_NrLibr        c                   const(2)
003400030805
003500030805      *---------------------------------------------------------------*
003600030805      *   S C H I E R E                                               *
003700030805      *---------------------------------------------------------------*
003800030805      *
003900091203       // -?iSeries  &  Librerie con entrambi i file tabelle?
004000091203     d $iSystem        s                   like(currSysNetA)
004100091203     d                                     dim(c_NrSyst)
004200091203     d                                     ctdata   perrcd( 1)
004300091203     d $Libraries      s                   like(ds_Libr)
004400091203     d                                     dim(c_NrSyst)
004500091203     d                                     alt($iSystem)
004600091203      *
004700091203      * - Decodifica opzioni
004800091203     d $Opz            s             15    dim(06) ctdata perrcd(1)
004900091203      *
005000091203      * - Messaggi di errore
005100091203     d $Msg            s             78    dim(15) ctdata perrcd(1)
005200091203      *
005300091203      * - Campi per ricerca
005400091203     d $Fld            s             10    dim(10)
005500170804      *
005600170804      * -?Comodo per definire la schiera con i codici cliente a video?
005700170804     d sk_V2Ccli       s                   like(V2Ccli)
005800170804     d                                     dim(16) inz(*zero)
005900030805
006000030805      *---------------------------------------------------------------*
006100030805      *   S T R U T T U R E   D A T I                                 *
006200030805      *---------------------------------------------------------------*
006300030805      *
006400030805      * Parametri
006500030805     d KPJBA         e ds
006600030805      *
006700030805      * Passaggio Parametri al pgm TIBS02R
006800030805     d TIBS02DS      e ds                  inz
006900030805     d  T02mod       e                     inz('R')
007000030805     d  T02cod       e                     inz('OSR')
007100030805      *
007200030805      * Tabella OSR = Serie O.R.M.
007300030805     d dOSR          e ds                  inz
007400091022     d aD§OSRcl5                      7A   overlay(D§OSRcl5)
007500080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
007600080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
007700080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
007800080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
007900080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
008000080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
008100080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
008200080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
008300080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
008400080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
008500080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
008600030805      *
008700030807      * Tabella 3A = Codici bolla
008800030807     d ds3A          e ds                  inz
008900030805      *
009000030805      * Parametri per interrogazione serie O.R.M.
009100030805     d TNTB301ds     e ds                  inz
009200030805     d  B30err       e                     inz(*off)
009300030805      *
009400030805      * Tracciato record file TNTBE00F
009500030805     d TNTBEds       e ds                  extname(TNTBE00F) inz
009600030805     d xTNTBEds      e ds                  extname(TNTBE00F) inz
009700030805     d                                     prefix(TBX:3)
009800130121
009900130121      * Tabella OSR di lavoro
010000130121     d wOSR          e ds                  extname(dOSR) inz
010100130121     d                                     prefix(w:1)
010200030805      *
010300030805     d TIBS34DS      e ds                  inz
010400030805     d DDATIUTE      e ds                  inz
010500030805     d AZUTEDS       e ds                  extname(AZUTE00F) inz
010600030805      *
010700030805     d TIBS69DS      e ds                  inz
010800030805     d DS_ACO        e ds                  extname(CNACO00F) inz
010900030805     d DS_IND        e ds                  extname(CNIND00F) inz
011000030805     d DS_CLP        e ds                  extname(CNCLP00F) inz
011100030805     d DS_CLS        e ds                  extname(FNCLS00F) inz
011200170803       //
011300170803       // -?Memorizzazione dati della 2ª videata?
011400170803     d TB30V2_ds     e ds                  extname(TNTB30D : TB30V2 : *input)
011500170803     d                                     inz
011600170803     d TB30V2_ds_old   ds                  likeds(TB30V2_ds) inz
011700170803     d TB30V2_ds_new   ds                  likeds(TB30V2_ds) inz
011800170804      *
011900170804      * -?Parametri aggiuntivi per TRTB71R (Gest. tab. "7U")?
012000170804     d TNTB30ds        ds                  inz   qualified
012100170807     d   §OSRopz                      1    inz
012200170804     d   §OSRcli                      7s 0 inz
012300170804     d   §OSRtpa                      1    inz('R')
012400170804     d   §OSRcpa                      2    inz('RC')
012500170804     d   §OSRerr                      1    inz(*off)
012600170804     d   §OSRmsg                     78    inz
012700030805      *
012800030805     d WLBDAT          ds                  inz
012900030805     d  G02DAT                 1      8  0 inz
013000030805     d  G02INV                 9     16  0 inz
013100030805     d  G02ERR                17     17    inz
013200030805     d  G02TGI                18     22  0 inz
013300030805      *
013400030805     d DSkeyOSR        ds             5    inz
013500030805     d  V1Cpoe
013600030805     d  V1Cnrs
013700091203       //
013800091203       // -?Ridefinizione elenco librerie in elaborare le tabelle?
013900091203     d ds_Libr         ds                  inz
014000091203     d  $Libr                        10    dim(c_NrLibr) inz
014100030805      *
014200091203      * - Status ds
014300091203     d Status         sds
014400030805     d  VTCpgm           *proc
014500030805
014600030805      *---------------------------------------------------------------*
014700030805      *   V A R I A B I L I                                           *
014800030805      *---------------------------------------------------------------*
014900030805      *
015000091203      * - Indici di schiera
015100030805     d XX              s              2  0 inz
015200091203      * - Flags booleani
015300120112     d $Called         s               n   inz(*off)
015400030805     d $Fine           s              1a   inz(*off)
015500030805     d $CarV1          s              1a   inz(*on)
015600030805     d $CarV2          s              1a   inz(*off)
015700090522     d $CarV3          s              1a   inz(*off)
015800030805     d $CarW1          s              1a   inz(*off)
015900091203      * - Variabili per la gestione dei video
016000030805     d $TipVid         s              1a   inz('1')
016100030805     d $Tibs02         s              1a   inz(*off)
016200030805     d Win             s             99a   inz(*zeros)
016300091203      * - Campi chiave per l'accesso ai file
016400091203     d kacr            s                   like(acrcro)
016500091203     d CodUt           s              1  0 inz(1)
016600091203      * - Campi di comodo
016700090522     d wksc            s                   like(acoksc)
016800090522     d wrag            s                   like(acorag)
016900030805     d wTasto          s              2a   inz(*zeros)
017000091203       // -?Nome del sistema?
017100091203     d currSysNeta     s              8a   inz
017200091203       // -?Nome esteso Libreria/File del file tabella?
017300091203     d wLibFile        s             21a   inz
017400091203       // -?Campi di comodo?
017500091203     d w_iSystem       s              1  0 inz
017600091203     d w_SisInf        s              3  0 inz
017700091203
017800091203       //--------------------------------------------------------------
017900091203       //?Definizione prototipi procedure.                             ?
018000091203       //--------------------------------------------------------------
018100091203
018200091203       // -?Reperimento NETA sistema AS/400 corrente?
018300091203      /copy gaitrasrc/srcProtoPr,UBRTVNETA
018400170804
018500170804       // -?Gest. Tab. Abil.clienti gestione particolarità: "7U"?
018600170804     d TRTB71C         pr                  extpgm('TRTB71C')
018700170804     d   kpjba                             likeds(kpjba)
018800170804     d   TNTB30ds                          likeds(TNTB30ds)
018900030805
019000030805      *---------------------------------------------------------------*
019100030805      *   M A I N   L I N E                                           *
019200030805      *---------------------------------------------------------------*
019300030805      *  Riepilogo indicatori utilizzati:                             *
019400030805      *  --------------------------------                             *
019500030805      *  01 - Record inesistente (inserimento)                        *
019600030805      *  02 - Record esistente   (modifica)                           *
019700030805      *  04 - Record annullato   (ripristino)                         *
019800030805      *  20 - Comodo                                                  *
019900030805      *  22 - Errori in scrittura record (WRITE)                      *
020000030805      *---------------------------------------------------------------*
020100030805      *
020200030805      * Operazioni iniziali
020300030805     c                   exsr      RutInz
020400030805      *
020500030805      * Gestione video
020600030805     c                   dow       $Fine = *off
020700030805     c     $TipVid       caseq     '1'           GesV1
020800030805     c     $TipVid       caseq     '2'           GesV2
020900090522     c     $TipVid       caseq     '3'           GesV3
021000030805     c     $TipVid       caseq     'A'           GesW1
021100030805     c                   endcs
021200030805     c                   enddo
021300030805      *
021400030805      * Fine
021500030805     c                   if        $Tibs02 = *on
021600030805     c                   clear                   TIBS02ds
021700030805     c                   movel     'C'           T02tla
021800030805     c                   call      'TIBS02R'
021900030805     c                   parm                    KPJBA
022000030805     c                   parm                    TIBS02ds
022100030805     c                   endif
022200030805      *
022300091203     c                   return
022400030805
022500030805      *---------------------------------------------------------------*
022600030805      * RutInz - Operazioni Iniziali                                  *
022700030805      *---------------------------------------------------------------*
022800030805     c     RutInz        BEGSR
022900030805      *
023000030805      * Ricezione parametri
023100030805     c     *entry        plist
023200030805     c                   parm                    KPJBA
023300030805      *
023400030805      * Definizioni chiavi di accesso
023500030805     c     K05TBE01      klist                                                  *tntbe01l
023600030805     c                   kfld                    TBEcod                         -tabella
023700030805     c                   kfld                    TBEke1                         -chiave uno
023800030805     c                   kfld                    TBEke2                         -chiave due
023900030805     c                   kfld                    TBElin                         -lingua
024000030805     c                   kfld                    TBEsif                         -s.informativo
024100030805      *
024200030805     c     K03TAB00      klist
024300030805     c                   kfld                    CodUt
024400030805     c                   kfld                    TBLcod
024500030805     c                   kfld                    TBLkey
024600091203      *
024700091203     c                   movel     *on           *inLR
024800091203      /free
024900091203
025000091203         // -?Verifica del sistema AS/400 corrente?
025100091203         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
025200091203           $Fine = *on;
025300091203           leavesr;
025400091203         endif;
025500091203
025600091203         // -?Impostazione elenco librerie in cui gestire le tabelle?
025700091203         //  ?(a seconda del sistema in cui si stà lavorando)?
025800091203         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
025900091203         if  w_iSystem = *zero;
026000091203           $Fine = *on;
026100091203           leavesr;
026200091203         endif;
026300091203
026400091203      /end-free
026500030805      *
026600030805      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
026700030805     c     *dtaara       define    §azute        azuteds
026800030805     c     *dtaara       define    §datiute      ddatiute
026900030805      *
027000030805     c                   clear                   AzUteDs
027100030805     c                   clear                   DDatiUte
027200030805     c                   clear                   Tibs34Ds
027300030805     c                   in(E)     *dtaara
027400030805if  1c                   if        %Error  or  RSUT = *blanks
027500030805     c                   call      'TIBS34R'
027600030805     c                   parm                    Tibs34Ds
027700030805     c                   in        *dtaara
027800030805e   1c                   endif
027900030805      *-- Verifica errori e autorità profilo
028000030805sel 1c                   SELECT
028100030805      *-- controllo se ho errori nei dati utente
028200030805      *--   nel qual caso NON risulta un profilo abilitato
028300030805w   1c                   WHEN      DUTerr = 'E'
028400030805     c                   eval      $Fine  = *on
028500030805      *
028600030805      *-- CONTROLLO AUTORITA'
028700030805      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
028800030805      *-- se il chiamante non richiede autorità specifica verificare
028900030805      *--   quella del profilo
029000030805      *-- se il chiamante richiede autorità specifica verificarla,
029100030805      *--  se è blank verificare quella del profilo
029200030805      *
029300030805      * se UTEAUT = *BLANK non siamo in sede
029400030805w   1c                   WHEN      UTEaut = *blank
029500030805      *
029600030805x   1c                   OTHER
029700030805      *
029800030805e   1c                   ENDSL
029900091203      /free
030000091203
030100091203         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
030200091203         w_SisInf = 1;
030300091203         exsr  sr_OpenFileTab;
030400091203
030500091203      /end-free
030600030805      *
030700030805      * Aggancio dati generali della tabella in esame
030800030805     c                   clear                   TBEcod
030900030805     c                   move      *zeros        TBEke1
031000030805     c                   move      T02cod        TBEke1
031100030805     c                   clear                   TBEke2
031200030805     c                   clear                   TBElin
031300030805     c                   movel     KNSIF         TBEsif
031400091211     c     K05TBE01      chain(n)  TNTBE000
031500030805     c                   if        not %found(TNTBE01L)
031600030805     c                   clear                   TBEsif
031700091211     c     K05TBE01      chain(n)  TNTBE000
031800030805     c                   endif
031900030805     c                   if        %found(TNTBE01L)
032000030805     c                   movel     TNTBEds       xTNTBEds
032100030805     c                   else
032200030805     c                   clear                   xTNTBEds
032300030805     c                   endif
032400030808      *
032500030808     c                   clear                   TB30V1
032600030808      *
032700030808      * Imposto il P.O. emissione ORM di default
032800030808     c                   clear                   V1Cpoe
032900120112      *
033000120112      * Verifico se ho ricevuto parametri
033100120112if  1c                   if        %subst(KPJBU:1:8) = *blanks
033200120112     c                   leavesr
033300120112e   1c                   endif
033400120112      *
033500120112      * Controllo i parametri ricevuti
033600120112     c                   eval      $Called = *on
033700120112     c                   eval      V1Cpoe  = %int( %subst(KPJBU : 1 : 3) )
033800120112     c                   eval      V1Cnrs  = %subst(KPJBU : 4 : 2)
033900120112     c                   exsr      CtrV1
034000120112if  1c                   if        *in28
034100120112     c                   movel(p)  V1Dmsg        KPJBU
034200120112     c                   eval      $Fine   = *on
034300120112     c                   leavesr
034400120112x   1c                   else
034500120112     c                   eval      $TipVid = '2'
034600120112     c                   eval      $CarV2  = *on
034700120112e   1c                   endif
034800030805      *
034900030805     c                   ENDSR
035000091203      /free
035100091203
035200091203       //--------------------------------------------------------------
035300091203       //?Apertura dei files tabelle nel sistema informativo impostato.?
035400091203       //--------------------------------------------------------------
035500091203       BEGSR  sr_OpenFileTab;
035600091203
035700091203         // -?Chiusura (eventuale) archivio?
035800091203         if  %open(TNTBE01L);
035900091203           close  TNTBE01L;
036000091203         endif;
036100091203
036200091203         // -?Apertura archivio?
036300091203         ds_Libr  = $Libraries(w_iSystem);
036400091203         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
036500091203         open  TNTBE01L;
036600091203
036700091203       ENDSR;
036800091203
036900091203      /end-free
037000030805
037100030805      *---------------------------------------------------------------*
037200030805      * GESV1  - Gestione videata selezione codice tabella            *
037300030805      *---------------------------------------------------------------*
037400030805     c     GesV1         BEGSR
037500030805      *
037600030805      * Inizializzazione videata
037700030805if  1c                   if        $CarV1 = *on
037800030805     c                   exsr      CarV1
037900030805     c                   movel     *off          $CarV1
038000030805e   1c                   endif
038100030805      *
038200030805      * Scrivo la testata
038300030805     c                   clear                   T1opz
038400030805     c                   write     TB30T1
038500030805      *
038600030805      * Se esistono errori sulla videata
038700030805      * emetto la write del formato a indicatori spenti per vedere
038800030805      * le eventuali decodifiche
038900030805if  1c                   if        *in99
039000030805     c                   movea     *in           Win
039100030805     c                   movea     *zeros        *in(50)
039200030805     c                   write     TB30V1
039300030805     c                   movea     Win           *in
039400030805e   1c                   endif
039500030805      *
039600030805     c                   exfmt     TB30V1
039700030805     c                   eval      *in99 = *off
039800030805     c                   clear                   V1Dmsg
039900030805      * F3=Fine
040000030805if  1c                   if        *inKC
040100030805     c                   exsr      F03V1
040200030805     c                   goto      EndGesV1
040300030805e   1c                   endif
040400030805      *
040500030805      * Controllo dati immessi a video
040600030805     c                   exsr      CtrV1
040700030805     c     *in99         cabeq     *on           EndGesV1
040800090525     c     *in90         cabeq     *on           EndGesV1
040900030805      *
041000030805      * Passaggio alla videata di dettaglio
041100030805if  1c                   if        not *in99
041200030805     c                   eval      $CarV2 = *on
041300030805     c                   eval      $TipVid = '2'
041400030805e   1c                   endif
041500030805      *
041600030805     c     EndGesV1      ENDSR
041700030805
041800030805      *---------------------------------------------------------------*
041900030805      * CARV1  - Caricamento dati prima videata                       *
042000030805      *---------------------------------------------------------------*
042100030805     c     CarV1         BEGSR
042200030805      *
042300030805     c                   movea     *zeros        *in(50)
042400030805     c                   movea     '00000'       *in(01)
042500030805      *
042600030808      * Imposto il campo "numero serie" per l'interrogazione
042700030808      *   (ma lascio il cursore posizionato sul P.O.)
042800030808     c                   move (p)  '?'           V1Cnrs
042900130418
043000130418     c                   clear                   V01ksc
043100130418     c                   clear                   V01rag
043200030805      *
043300030805     c                   ENDSR
043400030805
043500030805      *---------------------------------------------------------------*
043600030805      * CTRV1  - Controllo e decodifica prima videata                 *
043700030805      *---------------------------------------------------------------*
043800030805     c     CtrV1         BEGSR
043900030805      *
044000030805     c                   movea     *zeros        *in(50)
044100030805     c                   clear                   V1Dmsg
044200030808     c                   clear                   xx
044300030808     c                   clear                   $Fld
044400030808      *
044500030808      * Controlli / Ricerche
044600130418
044700130418      * Almeno 1 tra filiale/serie e cliente deve essere inserito
044800130418     c                   IF        V1Cpoe = *zeros and
044900130418     c                             (V1Cnrs = *blanks or V1Cnrs = *zeros)
045000130418     c                             and V01ksc = *zeros
045100130418     c                   eval      *in50 = *on
045200130418     c                   eval      *in99 = *on
045300130418     c                   eval      V1Dmsg = $Msg(14)
045400130418     c                   leavesr
045500130418     c                   ENDIF
045600130418      * se inserito cliente no ricerca su Numero Serie e no Filiale Emissione ORM
045700130418     c                   IF        V01ksc > 0 and V1Cnrs <> *blanks and
045800130418     c                             V1Cpoe > 0
045900130418     c                   eval      *in53 = *on
046000130418     c                   eval      *in99 = *on
046100130418     c                   eval      V1Dmsg = $Msg(08)
046200130418     c                   leavesr
046300130418     c                   ENDIF
046400130418
046500030806      *
046600030808      * Ricerca:
046700030806      * - Numero Serie
046800030806     c     '?'           scan      V1Cnrs                                 20
046900030808if  1c                   if        *in20   or   *inKJ
047000030806     c                   clear                   V1Cnrs
047100030806     c                   add       1             XX
047200030808     c                   eval      $Fld(xx)  = 'V1CNRS    '
047300030806e   1c                   endif
047400030806if  1c                   if        xx > *zeros
047500030806     c                   exsr      Search
047600130418     c                   IF        B30fxx = 'F12' or B30Fxx = 'F03'
047700130418     c                   eval      $carv1 = *on
047800130418     c                   eval      $tipvid = '1'
047900130418     c                   leavesr
048000130418     c                   ENDIF
048100030806e   1c                   endif
048200130418
048300130418     c                   IF        V01ksc = 0
048400091204      *
048500091204      * Controllo:
048600091204      * - P.O. emissione O.R.M. obbligatorio e obbligatoriamente valido
048700130418if  1c                   if        NOT  *inKJ
048800091204     c                   exsr      CtrPO
048900091204     c   99              goto      EndCtrV1
049000091204e   1c                   endif
049100030808      * Controllo:
049200030806      * - Numero serie
049300030808if  1c                   if           V1Cnrs = *blanks
049400030808     c                             or V1Cnrs = *zeros
049500030805     c                   seton                                        51  99
049600030808     c                   eval      V1Dmsg    = $Msg(03)
049700030805     c                   goto      EndCtrV1
049800030806x   1c                   else
049900120112     c     c_Digits      check     V1Cnrs                                 20
050000030808if  2c                   if           *in20
050100030808     c                             or V1Cnrs = *all'9'
050200130418     c                   seton                                        51  99
050300030806     c                   movel     $Msg(04)      V1Dmsg
050400030806     c                   goto      EndCtrV1
050500030806e   2c                   endif
050600030806e   1c                   endif
050700130418     c                   ENDIF
050800130418
050900130418      * Controllo cliente
051000130418     c                   IF        V01ksc > 0
051100130418     c                   eval      wksc = V01ksc
051200130418     c                   exsr      sr_tibs69
051300130418     c                   eval      V01rag = wrag
051400130418     c                   IF        *in99
051500130418     c                   eval      *in53 = *on
051600130418     c                   leavesr
051700130418     c                   ENDIF
051800130418     c                   add       1             XX
051900130418     c                   eval      $Fld(xx)  = 'V01KSC    '
052000130418     c                   exsr      Search
052100130418     c                   IF        B30fxx = 'F12' or B30Fxx = 'F03'
052200130418     c                   eval      $carv1 = *on
052300130418     c                   eval      $tipvid = '1'
052400130418     c                   leavesr
052500130418     c                   ENDIF
052600130418     c                   ENDIF
052700030805      *
052800030805     c     EndCtrV1      ENDSR
052900030805
053000030805      *---------------------------------------------------------------*
053100030805      * F03V1  - Tasto funzionale F03 -> Fine programma               *
053200030805      *---------------------------------------------------------------*
053300030805     c     F03V1         BEGSR
053400030805      *
053500030805     c                   movel     *on           $Fine                          fine pgm
053600030805      *
053700030805     c                   ENDSR
053800030805
053900030805      *---------------------------------------------------------------*
054000030805      * CTRPO  - Controllo Punto Operativo                            *
054100030805      *---------------------------------------------------------------*
054200030805     c     CtrPO         BEGSR
054300030805      *
054400030805     c                   clear                   V1Dpoe
054500030805      *
054600040303     c                   If        V1cPoe > *Zeros
054700030805     c     V1Cpoe        chain     AZORG01L
054800030805if  1c                   if        not %found(AZORG01L)
054900030805     c                             or  ORGfva =  '*'
055000030805     c                   seton                                        50  99
055100091204     c                   eval      V1Dmsg = $Msg(02)
055200030805     c                   goto      EndCtrPO
055300030805x   1c                   else
055400030805     c                   movel     ORGdes        V1Dpoe
055500030805e   1c                   endif
055600091204     c                   Else
055700091204     c                   seton                                        50  99
055800091204     c                   eval      V1Dmsg = $Msg(01)
055900091204     c                   goto      EndCtrPO
056000040303     c                   EndIf
056100030805      *
056200030805     c     EndCtrPO      ENDSR
056300030805
056400030805      *---------------------------------------------------------------*
056500030805      * SEARCH - Ricerche                                             *
056600030805      *---------------------------------------------------------------*
056700030805     c     Search        BEGSR
056800030805      *
056900090525     c                   eval      *in90    =  *on
057000030805      *
057100030805do  1c                   DO        10            xx
057200030805      *
057300030805if  2c                   if        $Fld(xx) =  *blanks
057400030805     c                   leave
057500030805e   2c                   endif
057600030805      *
057700030805sel 2c                   SELECT
057800030805      * Numero Serie
057900030805w   2c                   WHEN      $Fld(xx) =  'V1CNRS    '
058000030805     c                   clear                   TNTB301ds
058100030806if  3c                   if        *inKJ  = *off
058200030805     c                   eval      B30opz = '1'
058300030806x   3c                   else
058400030806     c                   eval      B30opz = '2'
058500030806e   3c                   endif
058600030805     c                   eval      B30ke1 = DSkeyOSR
058700030805     c                   eval      B30sif = KNSIF
058800030805     c                   movel(p)  TNTB301ds     KPJBU
058900030805     c                   call      'TNTB301R'
059000030805     c                   parm                    KPJBA
059100030805     c                   movel     KPJBU         TNTB301ds
059200030806if  3c                   if            B30fxx <> 'F03'
059300030805     c                             and B30fxx <> 'F12'
059400030806     c                             and B30err =  *blanks
059500090330if  4c                   if        B30ke1 <> *blanks
059600030805     c                   movel     B30ke1        DSkeyOSR
059700090330x   4c                   else
059800090330     c                   eval      V1Cnrs = '01'
059900090330e   4c                   endif
060000030806e   3c                   endif
060100130418
060200130418      * Codice cliente in prima videata
060300130418w   2c                   WHEN      $Fld(xx) =  'V01KSC    '
060400130418     c                   clear                   TNTB301ds
060500130418     c                   eval      B30opz = '3'
060600130418     c                   eval      B30ke1 = %editc(V01ksc:'X')
060700130418     c                   movel(p)  TNTB301ds     KPJBU
060800130418     c                   call      'TNTB301R'
060900130418     c                   parm                    KPJBA
061000130418     c                   movel     KPJBU         TNTB301ds
061100130418if  3c                   if            B30fxx <> 'F03'
061200130418     c                             and B30fxx <> 'F12'
061300130418     c                             and B30err =  *blanks
061400130418if  4c                             and B30ke1 <> *blanks
061500130418     c                   movel     B30ke1        DSkeyOSR
061600130418     c                   clear                   V01ksc
061700130418     c                   clear                   V01rag
061800130418e   4c                   endif
061900030805      *
062000030805      * Codice cliente
062100030805w   2c                   WHEN      $Fld(xx) =  'V2CCLI    '
062200030805     c                   movel(p)  V2Drag        PAXdmt
062300030805     c                   call      'XALFA3BR'
062400030805     c                   parm      RSut          PAXdut           30
062500030805     c                   parm                    CODut
062600030805     c                   parm                    PAXdmt           48
062700030805     c                   parm      DUTkci        PAXccc            4 0
062800030805     c                   parm      9             PAXsta            1 0
062900030805     c                   parm      *blanks       PAXflr           90
063000030805     c                   parm      *blanks       PAXdit            3
063100030805     c                   parm      01            PAXnum            2 0
063200030805     c                   parm                    PAXkcm           80
063300030805     c                   parm                    PAXksm          140
063400030805     c                   parm                    PAXldm           60
063500030805if  3c                   if        PAXsta >= *zeros
063600030805     c                   movel     PAXksm        V2Ccli
063700030805e   3c                   endif
063800040303      *
063900040303      * Codice cliente 2
064000040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI2   '
064100040303     c                   movel(p)  V2Drag2       PAXdmt
064200040303     c                   call      'XALFA3BR'
064300040303     c                   parm      RSut          PAXdut           30
064400040303     c                   parm                    CODut
064500040303     c                   parm                    PAXdmt           48
064600040303     c                   parm      DUTkci        PAXccc            4 0
064700040303     c                   parm      9             PAXsta            1 0
064800040303     c                   parm      *blanks       PAXflr           90
064900040303     c                   parm      *blanks       PAXdit            3
065000040303     c                   parm      01            PAXnum            2 0
065100040303     c                   parm                    PAXkcm           80
065200040303     c                   parm                    PAXksm          140
065300040303     c                   parm                    PAXldm           60
065400040303if  3c                   if        PAXsta >= *zeros
065500040303     c                   movel     PAXksm        V2Ccli2
065600040303e   3c                   endif
065700040303      *
065800040303      * Codice cliente 3
065900040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI3   '
066000040303     c                   movel(p)  V2Drag3       PAXdmt
066100040303     c                   call      'XALFA3BR'
066200040303     c                   parm      RSut          PAXdut           30
066300040303     c                   parm                    CODut
066400040303     c                   parm                    PAXdmt           48
066500040303     c                   parm      DUTkci        PAXccc            4 0
066600040303     c                   parm      9             PAXsta            1 0
066700040303     c                   parm      *blanks       PAXflr           90
066800040303     c                   parm      *blanks       PAXdit            3
066900040303     c                   parm      01            PAXnum            2 0
067000040303     c                   parm                    PAXkcm           80
067100040303     c                   parm                    PAXksm          140
067200040303     c                   parm                    PAXldm           60
067300040303if  3c                   if        PAXsta >= *zeros
067400040303     c                   movel     PAXksm        V2Ccli3
067500040303e   3c                   endif
067600040303      *
067700040303      * Codice cliente 4
067800040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI4   '
067900040303     c                   movel(p)  V2Drag4       PAXdmt
068000040303     c                   call      'XALFA3BR'
068100040303     c                   parm      RSut          PAXdut           30
068200040303     c                   parm                    CODut
068300040303     c                   parm                    PAXdmt           48
068400040303     c                   parm      DUTkci        PAXccc            4 0
068500040303     c                   parm      9             PAXsta            1 0
068600040303     c                   parm      *blanks       PAXflr           90
068700040303     c                   parm      *blanks       PAXdit            3
068800040303     c                   parm      01            PAXnum            2 0
068900040303     c                   parm                    PAXkcm           80
069000040303     c                   parm                    PAXksm          140
069100040303     c                   parm                    PAXldm           60
069200040303if  3c                   if        PAXsta >= *zeros
069300040303     c                   movel     PAXksm        V2Ccli4
069400040303e   3c                   endif
069500040303      *
069600040303      * Codice cliente 5
069700040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI5   '
069800040303     c                   movel(p)  V2Drag5       PAXdmt
069900040303     c                   call      'XALFA3BR'
070000040303     c                   parm      RSut          PAXdut           30
070100040303     c                   parm                    CODut
070200040303     c                   parm                    PAXdmt           48
070300040303     c                   parm      DUTkci        PAXccc            4 0
070400040303     c                   parm      9             PAXsta            1 0
070500040303     c                   parm      *blanks       PAXflr           90
070600040303     c                   parm      *blanks       PAXdit            3
070700040303     c                   parm      01            PAXnum            2 0
070800040303     c                   parm                    PAXkcm           80
070900040303     c                   parm                    PAXksm          140
071000040303     c                   parm                    PAXldm           60
071100040303if  3c                   if        PAXsta >= *zeros
071200040303     c                   movel     PAXksm        V2Ccli5
071300040303e   3c                   endif
071400080502      * Codice cliente 6
071500080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI6   '
071600080502     c                   movel(p)  V2Drag6       PAXdmt
071700080502     c                   call      'XALFA3BR'
071800080502     c                   parm      RSut          PAXdut           30
071900080502     c                   parm                    CODut
072000080502     c                   parm                    PAXdmt           48
072100080502     c                   parm      DUTkci        PAXccc            4 0
072200080502     c                   parm      9             PAXsta            1 0
072300080502     c                   parm      *blanks       PAXflr           90
072400080502     c                   parm      *blanks       PAXdit            3
072500080502     c                   parm      01            PAXnum            2 0
072600080502     c                   parm                    PAXkcm           80
072700080502     c                   parm                    PAXksm          140
072800080502     c                   parm                    PAXldm           60
072900080502if  3c                   if        PAXsta >= *zeros
073000080502     c                   movel     PAXksm        V2Ccli6
073100080502e   3c                   endif
073200080502      * Codice cliente 7
073300080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI7   '
073400080502     c                   movel(p)  V2Drag7       PAXdmt
073500080502     c                   call      'XALFA3BR'
073600080502     c                   parm      RSut          PAXdut           30
073700080502     c                   parm                    CODut
073800080502     c                   parm                    PAXdmt           48
073900080502     c                   parm      DUTkci        PAXccc            4 0
074000080502     c                   parm      9             PAXsta            1 0
074100080502     c                   parm      *blanks       PAXflr           90
074200080502     c                   parm      *blanks       PAXdit            3
074300080502     c                   parm      01            PAXnum            2 0
074400080502     c                   parm                    PAXkcm           80
074500080502     c                   parm                    PAXksm          140
074600080502     c                   parm                    PAXldm           60
074700080502if  3c                   if        PAXsta >= *zeros
074800080502     c                   movel     PAXksm        V2Ccli7
074900080502e   3c                   endif
075000080502      * Codice cliente 8
075100080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI8   '
075200080502     c                   movel(p)  V2Drag8       PAXdmt
075300080502     c                   call      'XALFA3BR'
075400080502     c                   parm      RSut          PAXdut           30
075500080502     c                   parm                    CODut
075600080502     c                   parm                    PAXdmt           48
075700080502     c                   parm      DUTkci        PAXccc            4 0
075800080502     c                   parm      9             PAXsta            1 0
075900080502     c                   parm      *blanks       PAXflr           90
076000080502     c                   parm      *blanks       PAXdit            3
076100080502     c                   parm      01            PAXnum            2 0
076200080502     c                   parm                    PAXkcm           80
076300080502     c                   parm                    PAXksm          140
076400080502     c                   parm                    PAXldm           60
076500080502if  3c                   if        PAXsta >= *zeros
076600080502     c                   movel     PAXksm        V2Ccli8
076700080502e   3c                   endif
076800080502      * Codice cliente 9
076900080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI9   '
077000080502     c                   movel(p)  V2Drag9       PAXdmt
077100080502     c                   call      'XALFA3BR'
077200080502     c                   parm      RSut          PAXdut           30
077300080502     c                   parm                    CODut
077400080502     c                   parm                    PAXdmt           48
077500080502     c                   parm      DUTkci        PAXccc            4 0
077600080502     c                   parm      9             PAXsta            1 0
077700080502     c                   parm      *blanks       PAXflr           90
077800080502     c                   parm      *blanks       PAXdit            3
077900080502     c                   parm      01            PAXnum            2 0
078000080502     c                   parm                    PAXkcm           80
078100080502     c                   parm                    PAXksm          140
078200080502     c                   parm                    PAXldm           60
078300080502if  3c                   if        PAXsta >= *zeros
078400080502     c                   movel     PAXksm        V2Ccli9
078500080502e   3c                   endif
078600080502      * Codice cliente 10
078700080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI10  '
078800080502     c                   movel(p)  V2Drag10      PAXdmt
078900080502     c                   call      'XALFA3BR'
079000080502     c                   parm      RSut          PAXdut           30
079100080502     c                   parm                    CODut
079200080502     c                   parm                    PAXdmt           48
079300080502     c                   parm      DUTkci        PAXccc            4 0
079400080502     c                   parm      9             PAXsta            1 0
079500080502     c                   parm      *blanks       PAXflr           90
079600080502     c                   parm      *blanks       PAXdit            3
079700080502     c                   parm      01            PAXnum            2 0
079800080502     c                   parm                    PAXkcm           80
079900080502     c                   parm                    PAXksm          140
080000080502     c                   parm                    PAXldm           60
080100080502if  3c                   if        PAXsta >= *zeros
080200080502     c                   movel     PAXksm        V2Ccli10
080300080502e   3c                   endif
080400080502      * Codice cliente 11
080500080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI11  '
080600080502     c                   movel(p)  V2Drag11      PAXdmt
080700080502     c                   call      'XALFA3BR'
080800080502     c                   parm      RSut          PAXdut           30
080900080502     c                   parm                    CODut
081000080502     c                   parm                    PAXdmt           48
081100080502     c                   parm      DUTkci        PAXccc            4 0
081200080502     c                   parm      9             PAXsta            1 0
081300080502     c                   parm      *blanks       PAXflr           90
081400080502     c                   parm      *blanks       PAXdit            3
081500080502     c                   parm      01            PAXnum            2 0
081600080502     c                   parm                    PAXkcm           80
081700080502     c                   parm                    PAXksm          140
081800080502     c                   parm                    PAXldm           60
081900080502if  3c                   if        PAXsta >= *zeros
082000080502     c                   movel     PAXksm        V2Ccli11
082100080502e   3c                   endif
082200080502      * Codice cliente 12
082300080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI12  '
082400080502     c                   movel(p)  V2Drag12      PAXdmt
082500080502     c                   call      'XALFA3BR'
082600080502     c                   parm      RSut          PAXdut           30
082700080502     c                   parm                    CODut
082800080502     c                   parm                    PAXdmt           48
082900080502     c                   parm      DUTkci        PAXccc            4 0
083000080502     c                   parm      9             PAXsta            1 0
083100080502     c                   parm      *blanks       PAXflr           90
083200080502     c                   parm      *blanks       PAXdit            3
083300080502     c                   parm      01            PAXnum            2 0
083400080502     c                   parm                    PAXkcm           80
083500080502     c                   parm                    PAXksm          140
083600080502     c                   parm                    PAXldm           60
083700080502if  3c                   if        PAXsta >= *zeros
083800080502     c                   movel     PAXksm        V2Ccli12
083900080502e   3c                   endif
084000080502      * Codice cliente 13
084100080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI13  '
084200080502     c                   movel(p)  V2Drag13      PAXdmt
084300080502     c                   call      'XALFA3BR'
084400080502     c                   parm      RSut          PAXdut           30
084500080502     c                   parm                    CODut
084600080502     c                   parm                    PAXdmt           48
084700080502     c                   parm      DUTkci        PAXccc            4 0
084800080502     c                   parm      9             PAXsta            1 0
084900080502     c                   parm      *blanks       PAXflr           90
085000080502     c                   parm      *blanks       PAXdit            3
085100080502     c                   parm      01            PAXnum            2 0
085200080502     c                   parm                    PAXkcm           80
085300080502     c                   parm                    PAXksm          140
085400080502     c                   parm                    PAXldm           60
085500080502if  3c                   if        PAXsta >= *zeros
085600080502     c                   movel     PAXksm        V2Ccli13
085700080502e   3c                   endif
085800080502      * Codice cliente 14
085900080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI14  '
086000080502     c                   movel(p)  V2Drag14      PAXdmt
086100080502     c                   call      'XALFA3BR'
086200080502     c                   parm      RSut          PAXdut           30
086300080502     c                   parm                    CODut
086400080502     c                   parm                    PAXdmt           48
086500080502     c                   parm      DUTkci        PAXccc            4 0
086600080502     c                   parm      9             PAXsta            1 0
086700080502     c                   parm      *blanks       PAXflr           90
086800080502     c                   parm      *blanks       PAXdit            3
086900080502     c                   parm      01            PAXnum            2 0
087000080502     c                   parm                    PAXkcm           80
087100080502     c                   parm                    PAXksm          140
087200080502     c                   parm                    PAXldm           60
087300080502if  3c                   if        PAXsta >= *zeros
087400080502     c                   movel     PAXksm        V2Ccli14
087500080502e   3c                   endif
087600080502      * Codice cliente 15
087700080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI15  '
087800080502     c                   movel(p)  V2Drag15      PAXdmt
087900080502     c                   call      'XALFA3BR'
088000080502     c                   parm      RSut          PAXdut           30
088100080502     c                   parm                    CODut
088200080502     c                   parm                    PAXdmt           48
088300080502     c                   parm      DUTkci        PAXccc            4 0
088400080502     c                   parm      9             PAXsta            1 0
088500080502     c                   parm      *blanks       PAXflr           90
088600080502     c                   parm      *blanks       PAXdit            3
088700080502     c                   parm      01            PAXnum            2 0
088800080502     c                   parm                    PAXkcm           80
088900080502     c                   parm                    PAXksm          140
089000080502     c                   parm                    PAXldm           60
089100080502if  3c                   if        PAXsta >= *zeros
089200080502     c                   movel     PAXksm        V2Ccli15
089300080502e   3c                   endif
089400080502      * Codice cliente 16
089500080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI16  '
089600080502     c                   movel(p)  V2Drag16      PAXdmt
089700080502     c                   call      'XALFA3BR'
089800080502     c                   parm      RSut          PAXdut           30
089900080502     c                   parm                    CODut
090000080502     c                   parm                    PAXdmt           48
090100080502     c                   parm      DUTkci        PAXccc            4 0
090200080502     c                   parm      9             PAXsta            1 0
090300080502     c                   parm      *blanks       PAXflr           90
090400080502     c                   parm      *blanks       PAXdit            3
090500080502     c                   parm      01            PAXnum            2 0
090600080502     c                   parm                    PAXkcm           80
090700080502     c                   parm                    PAXksm          140
090800080502     c                   parm                    PAXldm           60
090900080502if  3c                   if        PAXsta >= *zeros
091000080502     c                   movel     PAXksm        V2Ccli16
091100080502e   3c                   endif
091200030805      *
091300030805      * Codice bolla da forzare in bollettazione
091400030805w   2c                   WHEN      $Fld(xx) =  'V2CCBO    '
091500030805     c                   call      'X§TABER'
091600030805     c                   parm                    CODut
091700030807     c                   parm      '3A'          X§TABcod          2
091800030805     c                   parm      *blanks       X§TABkey          8
091900030805     c                   parm      *blanks       X§TABdes         30
092000030805     c                   movel     X§TABkey      V2Ccbo
092100030805     c                   movel     X§TABdes      V2Dcbo
092200030805      *
092300030805e   2c                   ENDSL
092400030805      *
092500030805e   1c                   ENDDO
092600030805      *
092700030805     c                   ENDSR
092800030805
092900030805      *---------------------------------------------------------------*
093000030805      * GESV2  - Gestione videata dettaglio dati                      *
093100030805      *---------------------------------------------------------------*
093200030805     c     GesV2         BEGSR
093300030805      *
093400030805      * Inizializzazione videata
093500030805     c                   if        $CarV2 = *on
093600030805     c                   exsr      CarV2
093700030805     c                   move      *off          $CarV2
093800030805     c                   endif
093900030805      * Scrivo la testata
094000170803     c                   eval      TB30V2_ds_old = TB30V2_ds
094100090525     c                   write     TB30T1
094200091203     c                   write     TB30T2
094300091203     c                   write     TB30P2
094400030805     c                   exfmt     TB30V2
094500030805     c                   eval      *in99 = *off
094600030805     c                   clear                   V1Dmsg
094700030805     c                   clear                   wTasto
094800090525     c                   movea     *zeros        *in(50)
094900090525
095000030805      *
095100030805sel 1c                   select
095200030805      * F03=Fine
095300030805w   1c                   when      *inKC
095400030805     c                   exsr      F03V1
095500030805     c                   goto      EndGesV2
095600030805      * F12=Ritorno
095700030805w   1c                   when      *inKL
095800030805     c                   exsr      F12V2
095900030805     c                   goto      EndGesV2
096000030805e   1c                   endsl
096100030805      *
096200030805      * Controllo dati immessi a video
096300170804     *** * (non si fanno se rcd annullato)
096400170804     *** *?...E se stessi ripristinando ??
096500170804if  1***c                   if        not *in04
096600030805     c                   exsr      CtrV2
096700170804e   1***c                   endif
096800090522
096900170803     c                   eval      TB30V2_ds_new = TB30V2_ds
097000090522      * passo alla terza videata
097100090525if  1c                   if        not *in99 and not *in90
097200170803     c                             and TB30V2_ds_new = TB30V2_ds_old
097300090525     c                   eval      $CarV3 = *on
097400090525     c                   eval      $TipVid = '3'
097500090525e   1c                   endif
097600030805      *
097700030805     c     EndGesV2      ENDSR
097800090525
097900090525      *---------------------------------------------------------------*
098000090525      * CARV2  - Caricamento dati seconda videata                     *
098100090525      *---------------------------------------------------------------*
098200090525     c     CarV2         BEGSR
098300090525      *
098400090525     c                   clear                   T1opz
098500090525     c                   movea     '00000'       *in(01)
098600091203      *
098700091203      /free
098800091203
098900091203         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
099000091203         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
099100091203           w_SisInf = 1;
099200091203           exsr  sr_OpenFileTab;
099300091203         endif;
099400091203
099500091203      /end-free
099600090525      *
099700090525      * Aggancio la tabella, se trovo il codice sono in modifica
099800090525      * o ripristino (se record annullato), altrimenti in immissione
099900090525     c                   exsr      ChnTBE
100000090525      *
100100090525if  1c                   if        %found(TNTBE01L)
100200090525      *
100300090525      * MODIFICA/RIPRISTINO
100400090525     c                   movel     TBEuni        dOSR
100500090525if  2c                   if        TBEatb = *blanks
100600090525     c                   eval      *in02  = *on
100700090525     c                   eval      T1opz  = $Opz(02)
100800090525x   2c                   else
100900090525     c                   eval      *in04  = *on
101000090525     c                   eval      T1opz  = $Opz(06)
101100090525e   2c                   endif
101200090525      *
101300090525x   1c                   else
101400090525      *
101500090525      * IMMISSIONE
101600090525     c                   clear                   dOSR
101700090525     c                   eval      *in01  = *on
101800090525     c                   eval      T1opz  = $Opz(01)
101900090525      *
102000090525e   1c                   endif
102100090525      *
102200090525     c                   eval      V2Cpoe = V1Cpoe
102300090525     c                   eval      V2Dpoe = V1Dpoe
102400090525     c                   move      V1Cnrs        V2Cnrs
102500090525     c                   move      D§OSRcli      V2Ccli
102600090525     c                   eval      V2Drag = D§OSRrag
102700090525     c                   move      D§OSRcl2      V2Ccli2
102800090525     c                   eval      V2Drag2 = D§OSRra2
102900090525     c                   move      D§OSRcl3      V2Ccli3
103000090525     c                   eval      V2Drag3 = D§OSRra3
103100090525     c                   move      D§OSRcl4      V2Ccli4
103200090610     c                   eval      V2Drag4 = D§OSRra4
103300091022     c* Verifica e forzatura x nuovi campi aggiunti
103400091022     c                   if        aD§OSRcl5 = *blanks
103500091022     c                   eval      D§OSRcl5 = *zeros
103600091022     c                   endif
103700091022     c                   if        aD§OSRcl6 = *blanks
103800091022     c                   eval      D§OSRcl6 = *zeros
103900091022     c                   endif
104000091022     c                   if        aD§OSRcl7 = *blanks
104100091022     c                   eval      D§OSRcl7 = *zeros
104200091022     c                   endif
104300091022     c                   if        aD§OSRcl8 = *blanks
104400091022     c                   eval      D§OSRcl8 = *zeros
104500091022     c                   endif
104600091022     c                   if        aD§OSRcl9 = *blanks
104700091022     c                   eval      D§OSRcl9 = *zeros
104800091022     c                   endif
104900091022     c                   if        aD§OSRcl10 = *blanks
105000091022     c                   eval      D§OSRcl10 = *zeros
105100091022     c                   endif
105200091022     c                   if        aD§OSRcl11 = *blanks
105300091022     c                   eval      D§OSRcl11 = *zeros
105400091022     c                   endif
105500091022     c                   if        aD§OSRcl12 = *blanks
105600091022     c                   eval      D§OSRcl12 = *zeros
105700091022     c                   endif
105800091022     c                   if        aD§OSRcl13 = *blanks
105900091022     c                   eval      D§OSRcl13 = *zeros
106000091022     c                   endif
106100091022     c                   if        aD§OSRcl14 = *blanks
106200091022     c                   eval      D§OSRcl14 = *zeros
106300091022     c                   endif
106400091022     c                   if        aD§OSRcl15 = *blanks
106500091022     c                   eval      D§OSRcl15 = *zeros
106600091022     c                   endif
106700091022     c                   if        aD§OSRcl16 = *blanks
106800091022     c                   eval      D§OSRcl16 = *zeros
106900091022     c                   endif
107000090525     c                   move      D§OSRcl5      V2Ccli5
107100090525     c                   move      D§OSRcl6      V2Ccli6
107200090525     c                   move      D§OSRcl7      V2Ccli7
107300090525     c                   move      D§OSRcl8      V2Ccli8
107400090525     c                   move      D§OSRcl9      V2Ccli9
107500090525     c                   move      D§OSRcl10     V2Ccli10
107600090525     c                   move      D§OSRcl11     V2Ccli11
107700090525     c                   move      D§OSRcl12     V2Ccli12
107800090525     c                   move      D§OSRcl13     V2Ccli13
107900090525     c                   move      D§OSRcl14     V2Ccli14
108000090525     c                   move      D§OSRcl15     V2Ccli15
108100090525     c                   move      D§OSRcl16     V2Ccli16
108200090525
108300090525      * decodifiche ragioni sociali
108400090525     c                   if        v2ccli5 > *zeros
108500090525     c                   eval      wksc = %dec(v2ccli5:7:0)
108600090525     c                   exsr      sr_tibs69
108700090525     c                   eval      v2drag5 = wrag
108800090525     c                   endif
108900090525     c                   if        v2ccli6 > *zeros
109000090525     c                   eval      wksc = %dec(v2ccli6:7:0)
109100090525     c                   exsr      sr_tibs69
109200090525     c                   eval      v2drag6 = wrag
109300090525     c                   endif
109400090525     c                   if        v2ccli7 > *zeros
109500090525     c                   eval      wksc = %dec(v2ccli7:7:0)
109600090525     c                   exsr      sr_tibs69
109700090525     c                   eval      v2drag7 = wrag
109800090525     c                   endif
109900090525     c                   if        v2ccli8 > *zeros
110000090525     c                   eval      wksc = %dec(v2ccli8:7:0)
110100090525     c                   exsr      sr_tibs69
110200090525     c                   eval      v2drag8 = wrag
110300090525     c                   endif
110400090525     c                   if        v2ccli9 > *zeros
110500090525     c                   eval      wksc = %dec(v2ccli9:7:0)
110600090525     c                   exsr      sr_tibs69
110700090525     c                   eval      v2drag9 = wrag
110800090525     c                   endif
110900090525     c                   if        v2ccli10 > *zeros
111000090525     c                   eval      wksc = %dec(v2ccli10:7:0)
111100090525     c                   exsr      sr_tibs69
111200090525     c                   eval      v2drag10 = wrag
111300090525     c                   endif
111400090525     c                   if        v2ccli11 > *zeros
111500090525     c                   eval      wksc = %dec(v2ccli11:7:0)
111600090525     c                   exsr      sr_tibs69
111700090525     c                   eval      v2drag11 = wrag
111800090525     c                   endif
111900090525     c                   if        v2ccli12 > *zeros
112000090525     c                   eval      wksc = %dec(v2ccli12:7:0)
112100090525     c                   exsr      sr_tibs69
112200090525     c                   eval      v2drag12 = wrag
112300090525     c                   endif
112400090525     c                   if        v2ccli13 > *zeros
112500090525     c                   eval      wksc = %dec(v2ccli13:7:0)
112600090525     c                   exsr      sr_tibs69
112700090525     c                   eval      v2drag13 = wrag
112800090525     c                   endif
112900090525     c                   if        v2ccli14 > *zeros
113000090525     c                   eval      wksc = %dec(v2ccli14:7:0)
113100090525     c                   exsr      sr_tibs69
113200090525     c                   eval      v2drag14 = wrag
113300090525     c                   endif
113400090525     c                   if        v2ccli15 > *zeros
113500090525     c                   eval      wksc = %dec(v2ccli15:7:0)
113600090525     c                   exsr      sr_tibs69
113700090525     c                   eval      v2drag15 = wrag
113800090525     c                   endif
113900090525     c                   if        v2ccli16 > *zeros
114000090525     c                   eval      wksc = %dec(v2ccli16:7:0)
114100090525     c                   exsr      sr_tibs69
114200090525     c                   eval      v2drag16 = wrag
114300090525     c                   endif
114400090525
114500090525      * --> nuovi campi per creazione ORM RC
114600090525      * traduttore personalizzato
114700091124     c                   if        d§osrvas = *blanks
114800091124     c                   eval      v2fvas = 'N'
114900091124     c                   else
115000090525     c                   eval      v2fvas = d§osrvas
115100091124     c                   endif
115200120307     c   01              eval      v2fvas = 'N'
115300090525      * codice ordinante
115400090525     c                   eval      v2ccor = d§osrcor
115500090525      * decodifico
115600090525     c                   if        v2ccor <> 0
115700090525     c                   eval      wksc = v2ccor
115800090525     c                   exsr      sr_tibs69
115900090525     c                   eval      v2dcor = wrag
116000090525     c                   endif
116100090525      * codice destinatario
116200090525     c                   eval      v2ccrc = d§osrcrc
116300090525      * decodifico
116400090525     c                   if        v2ccrc <> 0
116500090525     c                   eval      wksc = v2ccrc
116600090525     c                   exsr      sr_tibs69
116700090525     c                   eval      v2dcrc = wrag
116800090525     c                   endif
116900090525      * chi paga
117000090525     c                   eval      v2cpag = d§osrpag
117100120302      * se immissione imposto il dft = 'O'
117200090525     c                   if        *in01
117300120302     c                   eval      v2cpag = 'O'
117400090525     c                   endif
117500090525      * colli/peso
117600090623     c                   eval      v2cnclpkg = d§osrfcp
117700090610      * se immissione imposto il flag di dft
117800090525     c                   if        *in01
117900090623     c                   eval      v2cnclpkg = '1'
118000090525     c                   endif
118100090525      * filiale consegna
118200090525     c                   eval      v2cpoc = d§osrpoc
118300090525      * decodifico
118400090525     c                   clear                   v2dpoc
118500090525     c     v2cpoc        chain     azorg01l
118600090525     c                   if        %found(azorg01l)
118700090525     c                   eval      v2dpoc = orgdes
118800090525     c                   endif
118900090525      *
119000090525     c                   ENDSR
119100030805
119200030805      *---------------------------------------------------------------*
119300030805      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
119400030805      *---------------------------------------------------------------*
119500030805     c     F12V2         BEGSR
119600030805      *
119700091203      /free
119800091203         unlock  TNTBE01L;
119900091203      /end-free
120000030805     c                   eval      $TipVid = '1'
120100030805      *
120200030805     c                   ENDSR
120300030805
120400030805      *---------------------------------------------------------------*
120500030805      * CTRV2  - Controllo e decodifica seconda videata               *
120600030805      *---------------------------------------------------------------*
120700030805     c     CtrV2         BEGSR
120800030805      *
120900030805     c                   movea     *zeros        *in(50)
121000030805     c                   clear                   V1Dmsg
121100030805      *
121200030805      * Ricerche
121300030805     c                   clear                   xx
121400030805     c                   clear                   $Fld
121500030805      * - Codice cliente
121600030805     c     '?'           scan      V2Ccli                                 20
121700030805if  1c                   if        *in20
121800030805     c                   add       1             XX
121900030805     c                   eval      $Fld(xx) = 'V2CCLI    '
122000030805e   1c                   endif
122100040303      * - Codice cliente 2
122200040303     c     '?'           scan      V2Ccli2                                20
122300040303if  1c                   if        *in20
122400040303     c                   add       1             XX
122500040303     c                   eval      $Fld(xx) = 'V2CCLI2   '
122600040303e   1c                   endif
122700040303      * - Codice cliente 3
122800040303     c     '?'           scan      V2Ccli3                                20
122900040303if  1c                   if        *in20
123000040303     c                   add       1             XX
123100040303     c                   eval      $Fld(xx) = 'V2CCLI3   '
123200040303e   1c                   endif
123300040303      * - Codice cliente 4
123400040303     c     '?'           scan      V2Ccli4                                20
123500040303if  1c                   if        *in20
123600040303     c                   add       1             XX
123700040303     c                   eval      $Fld(xx) = 'V2CCLI4   '
123800040303e   1c                   endif
123900040303      * - Codice cliente 5
124000040303     c     '?'           scan      V2Ccli5                                20
124100040303if  1c                   if        *in20
124200040303     c                   add       1             XX
124300040303     c                   eval      $Fld(xx) = 'V2CCLI5   '
124400040303e   1c                   endif
124500080502      * - Codice cliente 6
124600080502     c     '?'           scan      V2Ccli6                                20
124700080502if  1c                   if        *in20
124800080502     c                   add       1             XX
124900080502     c                   eval      $Fld(xx) = 'V2CCLI6   '
125000080502e   1c                   endif
125100080502      * - Codice cliente 7
125200080502     c     '?'           scan      V2Ccli7                                20
125300080502if  1c                   if        *in20
125400080502     c                   add       1             XX
125500080502     c                   eval      $Fld(xx) = 'V2CCLI7   '
125600080502e   1c                   endif
125700080502      * - Codice cliente 8
125800080502     c     '?'           scan      V2Ccli8                                20
125900080502if  1c                   if        *in20
126000080502     c                   add       1             XX
126100080502     c                   eval      $Fld(xx) = 'V2CCLI8   '
126200080502e   1c                   endif
126300080502      * - Codice cliente 9
126400080502     c     '?'           scan      V2Ccli9                                20
126500080502if  1c                   if        *in20
126600080502     c                   add       1             XX
126700080502     c                   eval      $Fld(xx) = 'V2CCLI9   '
126800080502e   1c                   endif
126900080502      * - Codice cliente 10
127000080502     c     '?'           scan      V2Ccli10                               20
127100080502if  1c                   if        *in20
127200080502     c                   add       1             XX
127300080502     c                   eval      $Fld(xx) = 'V2CCLI10  '
127400080502e   1c                   endif
127500080502      * - Codice cliente 11
127600080502     c     '?'           scan      V2Ccli11                               20
127700080502if  1c                   if        *in20
127800080502     c                   add       1             XX
127900080502     c                   eval      $Fld(xx) = 'V2CCLI11  '
128000080502e   1c                   endif
128100080502      * - Codice cliente 12
128200080502     c     '?'           scan      V2Ccli12                               20
128300080502if  1c                   if        *in20
128400080502     c                   add       1             XX
128500080502     c                   eval      $Fld(xx) = 'V2CCLI12  '
128600080502e   1c                   endif
128700080502      * - Codice cliente 13
128800080502     c     '?'           scan      V2Ccli13                               20
128900080502if  1c                   if        *in20
129000080502     c                   add       1             XX
129100080502     c                   eval      $Fld(xx) = 'V2CCLI13  '
129200080502e   1c                   endif
129300080502      * - Codice cliente 14
129400080502     c     '?'           scan      V2Ccli14                               20
129500080502if  1c                   if        *in20
129600080502     c                   add       1             XX
129700080502     c                   eval      $Fld(xx) = 'V2CCLI14  '
129800080502e   1c                   endif
129900080502      * - Codice cliente 15
130000080502     c     '?'           scan      V2Ccli15                               20
130100080502if  1c                   if        *in20
130200080502     c                   add       1             XX
130300080502     c                   eval      $Fld(xx) = 'V2CCLI15  '
130400080502e   1c                   endif
130500080502      * - Codice cliente 16
130600080502     c     '?'           scan      V2Ccli16                               20
130700080502if  1c                   if        *in20
130800080502     c                   add       1             XX
130900080502     c                   eval      $Fld(xx) = 'V2CCLI16  '
131000080502e   1c                   endif
131100030805      * - Codice bolla da forzare
131200030805     c     '?'           scan      V2Ccbo                                 20
131300030805if  1c                   if        *in20
131400030805     c                   add       1             XX
131500030805     c                   eval      $Fld(xx) = 'V2CCBO    '
131600030805e   1c                   endif
131700030805      *
131800030805if  1c                   if        xx > *zeros
131900030805     c                   exsr      Search
132000030805e   1c                   endif
132100030805      *
132200030805      * Controlli
132300030805      *
132400030805      * - Codice cliente obbligatorio e obbligatoriamente valido
132500030805     c                   clear                   V2Drag
132600030805if  1c                   IF           V2Ccli = *blanks
132700030805     c                             or V2Ccli = *zeros
132800030805     c                   seton                                        52  99
132900030806     c                   movel     $Msg(05)      V1Dmsg
133000030805     c                   goto      EndCtrV2
133100030805x   1c                   ELSE
133200120112     c     c_Digits      check     V2Ccli                                 20
133300030806if  2c                   if        *in20
133400030806     c                   seton                                        52  99
133500030806     c                   movel     $Msg(06)      V1Dmsg
133600030806     c                   goto      EndCtrV2
133700030806e   2c                   endif
133800090525     c                   eval      wksc = %dec(V2Ccli:7:0)
133900090522     c                   exsr      sr_tibs69
134000090522if  2c                   if        *in99
134100090522     c                   seton                                        52
134200030805     c                   goto      EndCtrV2
134300030805e   2c                   endif
134400040303      * Decodifica cliente
134500090522     c                   movel     wrag          V2Drag
134600130121      * Controllo se il cliente è già presente in tabella.
134700130121     c                   exsr      ctrOSR
134800130121     c                   IF        *in99
134900130121     c                   eval      *in52 = *on
135000130121     c                   leavesr
135100130121     c                   ENDIF
135200120307      * se non ha traduttore
135300120302      * controllo se il cliente esite su FNACR come 333 e 444
135400120307     c                   IF        v2fvas = 'N'
135500120302     c                   eval      kacr = %dec(v2ccli:10:0) * 1000
135600120302     c                   exsr      CtrACR
135700120302if  2c                   IF        *in99
135800120302     c                   eval      *in52 = *on
135900120302     c                   leavesr
136000120302     c                   ENDIF
136100120307     c                   ENDIF
136200030805e   1c                   ENDIF
136300040303
136400040303      * - Codice cliente 2 valido
136500040303     c                   clear                   V2Drag2
136600040303if  1c                   IF            V2Ccli2 <> *blanks
136700040303     c                             and V2Ccli2 <> *zeros
136800120112     c     c_Digits      check     V2Ccli2                                20
136900040303if  2c                   if        *in20
137000040303     c                   seton                                        59  99
137100040303     c                   movel     $Msg(06)      V1Dmsg
137200040303     c                   goto      EndCtrV2
137300040303e   2c                   endif
137400090525     c                   eval      wksc = %dec(V2Ccli2:7:0)
137500090522     c                   exsr      sr_tibs69
137600090522if  2c                   if        *in99
137700090522     c                   seton                                        59
137800040303     c                   goto      EndCtrV2
137900040303e   2c                   endif
138000090522     c                   movel     wrag          V2Drag2
138100130121      * Controllo se il cliente è già presente in tabella.
138200130121     c                   exsr      ctrOSR
138300130121     c                   IF        *in99
138400130121     c                   eval      *in59 = *on
138500130121     c                   leavesr
138600130121     c                   ENDIF
138700120302      * controllo se il cliente esite su FNACR come 333 e 444
138800120307     c                   IF        v2fvas = 'N'
138900120302     c                   eval      kacr = %dec(v2ccli2:10:0) * 1000
139000120302     c                   exsr      CtrACR
139100120302if  2c                   IF        *in99
139200120302     c                   eval      *in59 = *on
139300120302     c                   leavesr
139400120307     c                   ENDIF
139500120302     c                   ENDIF
139600040303e   1c                   ENDIF
139700040303
139800040303      * - Codice cliente 3 valido
139900040303     c                   clear                   V2Drag3
140000040303if  1c                   IF            V2Ccli3 <> *blanks
140100040303     c                             and V2Ccli3 <> *zeros
140200120112     c     c_Digits      check     V2Ccli3                                20
140300040303if  2c                   if        *in20
140400040303     c                   seton                                        60  99
140500040303     c                   movel     $Msg(06)      V1Dmsg
140600040303     c                   goto      EndCtrV2
140700040303e   2c                   endif
140800090525     c                   eval      wksc = %dec(V2Ccli3:7:0)
140900090522     c                   exsr      sr_tibs69
141000090522if  2c                   if        *in99
141100090522     c                   seton                                        60
141200040303     c                   goto      EndCtrV2
141300040303e   2c                   endif
141400090522     c                   movel     wrag          V2Drag3
141500130121      * Controllo se il cliente è già presente in tabella.
141600130121     c                   exsr      ctrOSR
141700130121     c                   IF        *in99
141800130121     c                   eval      *in60 = *on
141900130121     c                   leavesr
142000130121     c                   ENDIF
142100120302      * controllo se il cliente esite su FNACR come 333 e 444
142200120307     c                   IF        v2fvas = 'N'
142300120302     c                   eval      kacr = %dec(v2ccli3:10:0) * 1000
142400120302     c                   exsr      CtrACR
142500120302if  2c                   IF        *in99
142600120302     c                   eval      *in60 = *on
142700120302     c                   leavesr
142800120302     c                   ENDIF
142900120307     c                   ENDIF
143000040303e   1c                   ENDIF
143100040303
143200040303      * - Codice cliente 4 valido
143300040303     c                   clear                   V2Drag4
143400040303if  1c                   IF            V2Ccli4 <> *blanks
143500040303     c                             and V2Ccli4 <> *zeros
143600120112     c     c_Digits      check     V2Ccli4                                20
143700040303if  2c                   if        *in20
143800040303     c                   seton                                        61  99
143900040303     c                   movel     $Msg(06)      V1Dmsg
144000040303     c                   goto      EndCtrV2
144100040303e   2c                   endif
144200090525     c                   eval      wksc = %dec(V2Ccli4:7:0)
144300090525     c                   exsr      sr_tibs69
144400090522if  2c                   if        *in99
144500090522     c                   seton                                        61
144600040303     c                   goto      EndCtrV2
144700040303e   2c                   endif
144800090522     c                   movel     wrag          V2Drag4
144900130121      * Controllo se il cliente è già presente in tabella.
145000130121     c                   exsr      ctrOSR
145100130121     c                   IF        *in99
145200130121     c                   eval      *in61 = *on
145300130121     c                   leavesr
145400130121     c                   ENDIF
145500120302      * controllo se il cliente esite su FNACR come 333 e 444
145600120307     c                   IF        v2fvas = 'N'
145700120302     c                   eval      kacr = %dec(v2ccli4:10:0) * 1000
145800120302     c                   exsr      CtrACR
145900120302if  2c                   IF        *in99
146000120302     c                   eval      *in61 = *on
146100120302     c                   leavesr
146200120302     c                   ENDIF
146300120307     c                   ENDIF
146400040303e   1c                   ENDIF
146500040303
146600040303      * - Codice cliente 5 valido
146700040303     c                   clear                   V2Drag5
146800040303if  1c                   IF            V2Ccli5 <> *blanks
146900040303     c                             and V2Ccli5 <> *zeros
147000120112     c     c_Digits      check     V2Ccli5                                20
147100040303if  2c                   if        *in20
147200040303     c                   seton                                        62  99
147300040303     c                   movel     $Msg(06)      V1Dmsg
147400040303     c                   goto      EndCtrV2
147500040303e   2c                   endif
147600090525     c                   eval      wksc = %dec(V2Ccli5:7:0)
147700090522     c                   exsr      sr_tibs69
147800090522if  2c                   if        *in99
147900090522     c                   seton                                        62
148000040303     c                   goto      EndCtrV2
148100040303e   2c                   endif
148200090522     c                   movel     wrag          V2Drag5
148300130121      * Controllo se il cliente è già presente in tabella.
148400130121     c                   exsr      ctrOSR
148500130121     c                   IF        *in99
148600130121     c                   eval      *in62 = *on
148700130121     c                   leavesr
148800130121     c                   ENDIF
148900120302      * controllo se il cliente esite su FNACR come 333 e 444
149000120307     c                   IF        v2fvas = 'N'
149100120302     c                   eval      kacr = %dec(v2ccli5:10:0) * 1000
149200120302     c                   exsr      CtrACR
149300120302if  2c                   IF        *in99
149400120302     c                   eval      *in62 = *on
149500120302     c                   leavesr
149600120302     c                   ENDIF
149700120307     c                   ENDIF
149800040303e   1c                   ENDIF
149900080502
150000080502      * - Codice cliente 6 valido
150100080502     c                   clear                   V2Drag6
150200080502if  1c                   IF            V2Ccli6 <> *blanks
150300080502     c                             and V2Ccli6 <> *zeros
150400120112     c     c_Digits      check     V2Ccli6                                20
150500080502if  2c                   if        *in20
150600090522     c                   seton                                        63  99
150700080502     c                   movel     $Msg(06)      V1Dmsg
150800080502     c                   goto      EndCtrV2
150900080502e   2c                   endif
151000090525     c                   eval      wksc = %dec(V2Ccli6:7:0)
151100090522     c                   exsr      sr_tibs69
151200090522if  2c                   if        *in99
151300090522     c                   seton                                        63
151400080502     c                   goto      EndCtrV2
151500080502e   2c                   endif
151600090522     c                   movel     wrag          V2Drag6
151700130121      * Controllo se il cliente è già presente in tabella.
151800130121     c                   exsr      ctrOSR
151900130121     c                   IF        *in99
152000130121     c                   eval      *in63 = *on
152100130121     c                   leavesr
152200130121     c                   ENDIF
152300120302      * controllo se il cliente esite su FNACR come 333 e 444
152400120307     c                   IF        v2fvas = 'N'
152500120302     c                   eval      kacr = %dec(v2ccli6:10:0) * 1000
152600120302     c                   exsr      CtrACR
152700120302if  2c                   IF        *in99
152800120302     c                   eval      *in63 = *on
152900120302     c                   leavesr
153000120302     c                   ENDIF
153100120307     c                   ENDIF
153200080502e   1c                   ENDIF
153300080502
153400080502      * - Codice cliente 7 valido
153500080502     c                   clear                   V2Drag7
153600080502if  1c                   IF            V2Ccli7 <> *blanks
153700080502     c                             and V2Ccli7 <> *zeros
153800120112     c     c_Digits      check     V2Ccli7                                20
153900080502if  2c                   if        *in20
154000090522     c                   seton                                        64  99
154100080502     c                   movel     $Msg(06)      V1Dmsg
154200080502     c                   goto      EndCtrV2
154300080502e   2c                   endif
154400090525     c                   eval      wksc = %dec(V2Ccli7:7:0)
154500090522     c                   exsr      sr_tibs69
154600090522if  2c                   if        *in99
154700090522     c                   seton                                        64
154800080502     c                   goto      EndCtrV2
154900080502e   2c                   endif
155000090522     c                   movel     wrag          V2Drag7
155100130121      * Controllo se il cliente è già presente in tabella.
155200130121     c                   exsr      ctrOSR
155300130121     c                   IF        *in99
155400130121     c                   eval      *in64 = *on
155500130121     c                   leavesr
155600130121     c                   ENDIF
155700120302      * controllo se il cliente esite su FNACR come 333 e 444
155800120307     c                   IF        v2fvas = 'N'
155900120302     c                   eval      kacr = %dec(v2ccli7:10:0) * 1000
156000120302     c                   exsr      CtrACR
156100120302if  2c                   IF        *in99
156200120302     c                   eval      *in64 = *on
156300120302     c                   leavesr
156400120302     c                   ENDIF
156500120307     c                   ENDIF
156600080502e   1c                   ENDIF
156700080502
156800080502      * - Codice cliente 8 valido
156900080502     c                   clear                   V2Drag8
157000080502if  1c                   IF            V2Ccli8 <> *blanks
157100080502     c                             and V2Ccli8 <> *zeros
157200120112     c     c_Digits      check     V2Ccli8                                20
157300080502if  2c                   if        *in20
157400090522     c                   seton                                        65  99
157500080502     c                   movel     $Msg(06)      V1Dmsg
157600080502     c                   goto      EndCtrV2
157700080502e   2c                   endif
157800090525     c                   eval      wksc = %dec(V2Ccli8:7:0)
157900090522     c                   exsr      sr_tibs69
158000090522if  2c                   if        *in99
158100090522     c                   seton                                        65
158200080502     c                   goto      EndCtrV2
158300080502e   2c                   endif
158400090522     c                   movel     wrag          V2Drag8
158500130121      * Controllo se il cliente è già presente in tabella.
158600130121     c                   exsr      ctrOSR
158700130121     c                   IF        *in99
158800130121     c                   eval      *in65 = *on
158900130121     c                   leavesr
159000130121     c                   ENDIF
159100120302      * controllo se il cliente esite su FNACR come 333 e 444
159200120307     c                   IF        v2fvas = 'N'
159300120302     c                   eval      kacr = %dec(v2ccli8:10:0) * 1000
159400120302     c                   exsr      CtrACR
159500120302if  2c                   IF        *in99
159600120302     c                   eval      *in65 = *on
159700120302     c                   leavesr
159800120302     c                   ENDIF
159900120307     c                   ENDIF
160000080502e   1c                   ENDIF
160100080502
160200080502      * - Codice cliente 9 valido
160300080502     c                   clear                   V2Drag9
160400080502if  1c                   IF            V2Ccli9 <> *blanks
160500080502     c                             and V2Ccli9 <> *zeros
160600120112     c     c_Digits      check     V2Ccli9                                20
160700080502if  2c                   if        *in20
160800090522     c                   seton                                        66  99
160900080502     c                   movel     $Msg(06)      V1Dmsg
161000080502     c                   goto      EndCtrV2
161100080502e   2c                   endif
161200090525     c                   eval      wksc = %dec(V2Ccli9:7:0)
161300090522     c                   exsr      sr_tibs69
161400090522if  2c                   if        *in99
161500090525     c                   seton                                        66
161600080502     c                   goto      EndCtrV2
161700080502e   2c                   endif
161800090522     c                   movel     wrag          V2Drag9
161900130121      * Controllo se il cliente è già presente in tabella.
162000130121     c                   exsr      ctrOSR
162100130121     c                   IF        *in99
162200130121     c                   eval      *in66 = *on
162300130121     c                   leavesr
162400130121     c                   ENDIF
162500120302      * controllo se il cliente esite su FNACR come 333 e 444
162600120307     c                   IF        v2fvas = 'N'
162700120302     c                   eval      kacr = %dec(v2ccli9:10:0) * 1000
162800120302     c                   exsr      CtrACR
162900120302if  2c                   IF        *in99
163000120302     c                   eval      *in66 = *on
163100120302     c                   leavesr
163200120302     c                   ENDIF
163300120307     c                   ENDIF
163400080502e   1c                   ENDIF
163500080502
163600080502      * - Codice cliente 10 valido
163700080502     c                   clear                   V2Drag10
163800080502if  1c                   IF            V2Ccli10 <> *blanks
163900080502     c                             and V2Ccli10 <> *zeros
164000120112     c     c_Digits      check     V2Ccli10                               20
164100080502if  2c                   if        *in20
164200090522     c                   seton                                        67  99
164300080502     c                   movel     $Msg(06)      V1Dmsg
164400080502     c                   goto      EndCtrV2
164500080502e   2c                   endif
164600090525     c                   eval      wksc = %dec(V2Ccli10:7:0)
164700090522     c                   exsr      sr_tibs69
164800090522if  2c                   if        *in99
164900090522     c                   seton                                        67
165000080502     c                   goto      EndCtrV2
165100080502e   2c                   endif
165200090522     c                   movel     wrag          V2Drag10
165300130121      * Controllo se il cliente è già presente in tabella.
165400130121     c                   exsr      ctrOSR
165500130121     c                   IF        *in99
165600130121     c                   eval      *in67 = *on
165700130121     c                   leavesr
165800130121     c                   ENDIF
165900120302      * controllo se il cliente esite su FNACR come 333 e 444
166000120307     c                   IF        v2fvas = 'N'
166100120302     c                   eval      kacr = %dec(v2ccli10:10:0) * 1000
166200120302     c                   exsr      CtrACR
166300120302if  2c                   IF        *in99
166400120302     c                   eval      *in67 = *on
166500120302     c                   leavesr
166600120302     c                   ENDIF
166700120307     c                   ENDIF
166800080502e   1c                   ENDIF
166900080502
167000080502      * - Codice cliente 11 valido
167100080502     c                   clear                   V2Drag11
167200080502if  1c                   IF            V2Ccli11 <> *blanks
167300080502     c                             and V2Ccli11 <> *zeros
167400120112     c     c_Digits      check     V2Ccli11                               20
167500080502if  2c                   if        *in20
167600090522     c                   seton                                        68  99
167700080502     c                   movel     $Msg(06)      V1Dmsg
167800080502     c                   goto      EndCtrV2
167900080502e   2c                   endif
168000090525     c                   eval      wksc = %dec(V2Ccli11:7:0)
168100090522     c                   exsr      sr_tibs69
168200090522if  2c                   if        *in99
168300090522     c                   seton                                        68
168400080502     c                   goto      EndCtrV2
168500080502e   2c                   endif
168600090522     c                   movel     wrag          V2Drag11
168700130121      * Controllo se il cliente è già presente in tabella.
168800130121     c                   exsr      ctrOSR
168900130121     c                   IF        *in99
169000130121     c                   eval      *in68 = *on
169100130121     c                   leavesr
169200130121     c                   ENDIF
169300120302      * controllo se il cliente esite su FNACR come 333 e 444
169400120307     c                   IF        v2fvas = 'N'
169500120302     c                   eval      kacr = %dec(v2ccli11:10:0) * 1000
169600120302     c                   exsr      CtrACR
169700120302if  2c                   IF        *in99
169800120302     c                   eval      *in68 = *on
169900120302     c                   leavesr
170000120302     c                   ENDIF
170100120307     c                   ENDIF
170200080502e   1c                   ENDIF
170300080502
170400080502      * - Codice cliente 12 valido
170500080502     c                   clear                   V2Drag12
170600080502if  1c                   IF            V2Ccli12 <> *blanks
170700080502     c                             and V2Ccli12 <> *zeros
170800120112     c     c_Digits      check     V2Ccli12                               20
170900080502if  2c                   if        *in20
171000090522     c                   seton                                        69  99
171100080502     c                   movel     $Msg(06)      V1Dmsg
171200080502     c                   goto      EndCtrV2
171300080502e   2c                   endif
171400090525     c                   eval      wksc = %dec(V2Ccli12:7:0)
171500090522     c                   exsr      sr_tibs69
171600090522if  2c                   if        *in99
171700090522     c                   seton                                        69
171800080502     c                   goto      EndCtrV2
171900080502e   2c                   endif
172000090522     c                   movel     wrag          V2Drag12
172100130121      * Controllo se il cliente è già presente in tabella.
172200130121     c                   exsr      ctrOSR
172300130121     c                   IF        *in99
172400130121     c                   eval      *in69 = *on
172500130121     c                   leavesr
172600130121     c                   ENDIF
172700120302      * controllo se il cliente esite su FNACR come 333 e 444
172800120307     c                   IF        v2fvas = 'N'
172900120302     c                   eval      kacr = %dec(v2ccli12:10:0) * 1000
173000120302     c                   exsr      CtrACR
173100120302if  2c                   IF        *in99
173200120302     c                   eval      *in69 = *on
173300120302     c                   leavesr
173400120302     c                   ENDIF
173500120307     c                   ENDIF
173600080502e   1c                   ENDIF
173700080502
173800080502      * - Codice cliente 13 valido
173900080502     c                   clear                   V2Drag13
174000080502if  1c                   IF            V2Ccli13 <> *blanks
174100080502     c                             and V2Ccli13 <> *zeros
174200120112     c     c_Digits      check     V2Ccli13                               20
174300080502if  2c                   if        *in20
174400090525     c                   seton                                        70  99
174500080502     c                   movel     $Msg(06)      V1Dmsg
174600080502     c                   goto      EndCtrV2
174700080502e   2c                   endif
174800090525     c                   eval      wksc = %dec(V2Ccli13:7:0)
174900090522     c                   exsr      sr_tibs69
175000090522if  2c                   if        *in99
175100090525     c                   seton                                        70
175200080502     c                   goto      EndCtrV2
175300080502e   2c                   endif
175400090522     c                   movel     wrag          V2Drag13
175500130121      * Controllo se il cliente è già presente in tabella.
175600130121     c                   exsr      ctrOSR
175700130121     c                   IF        *in99
175800130121     c                   eval      *in70 = *on
175900130121     c                   leavesr
176000130121     c                   ENDIF
176100120302      * controllo se il cliente esite su FNACR come 333 e 444
176200120307     c                   IF        v2fvas = 'N'
176300120302     c                   eval      kacr = %dec(v2ccli13:10:0) * 1000
176400120302     c                   exsr      CtrACR
176500120302if  2c                   IF        *in99
176600120302     c                   eval      *in70 = *on
176700120302     c                   leavesr
176800120302     c                   ENDIF
176900120307     c                   ENDIF
177000080502e   1c                   ENDIF
177100080502
177200080502      * - Codice cliente 14 valido
177300080502     c                   clear                   V2Drag14
177400080502if  1c                   IF            V2Ccli14 <> *blanks
177500080502     c                             and V2Ccli14 <> *zeros
177600120112     c     c_Digits      check     V2Ccli14                               20
177700080502if  2c                   if        *in20
177800090522     c                   seton                                        71  99
177900080502     c                   movel     $Msg(06)      V1Dmsg
178000080502     c                   goto      EndCtrV2
178100080502e   2c                   endif
178200090525     c                   eval      wksc = %dec(V2Ccli14:7:0)
178300090522     c                   exsr      sr_tibs69
178400090522if  2c                   if        *in99
178500090522     c                   seton                                        71
178600080502     c                   goto      EndCtrV2
178700080502e   2c                   endif
178800090522     c                   movel     wrag          V2Drag14
178900130121      * Controllo se il cliente è già presente in tabella.
179000130121     c                   exsr      ctrOSR
179100130121     c                   IF        *in99
179200130121     c                   eval      *in71 = *on
179300130121     c                   leavesr
179400130121     c                   ENDIF
179500120302      * controllo se il cliente esite su FNACR come 333 e 444
179600120307     c                   IF        v2fvas = 'N'
179700120302     c                   eval      kacr = %dec(v2ccli14:10:0) * 1000
179800120302     c                   exsr      CtrACR
179900120302if  2c                   IF        *in99
180000120302     c                   eval      *in71 = *on
180100120302     c                   leavesr
180200120302     c                   ENDIF
180300120307     c                   ENDIF
180400080502e   1c                   ENDIF
180500080502
180600080502      * - Codice cliente 15 valido
180700080502     c                   clear                   V2Drag15
180800080502if  1c                   IF            V2Ccli15 <> *blanks
180900080502     c                             and V2Ccli15 <> *zeros
181000120112     c     c_Digits      check     V2Ccli15                               20
181100080502if  2c                   if        *in20
181200090522     c                   seton                                        72  99
181300080502     c                   movel     $Msg(06)      V1Dmsg
181400080502     c                   goto      EndCtrV2
181500080502e   2c                   endif
181600090525     c                   eval      wksc = %dec(V2Ccli15:7:0)
181700090522     c                   exsr      sr_tibs69
181800090522if  2c                   if        *in99
181900090522     c                   seton                                        72  99
182000080502     c                   goto      EndCtrV2
182100080502e   2c                   endif
182200090522     c                   movel     wrag          V2Drag15
182300130121      * Controllo se il cliente è già presente in tabella.
182400130121     c                   exsr      ctrOSR
182500130121     c                   IF        *in99
182600130121     c                   eval      *in72 = *on
182700130121     c                   leavesr
182800130121     c                   ENDIF
182900120302      * controllo se il cliente esite su FNACR come 333 e 444
183000120307     c                   IF        v2fvas = 'N'
183100120302     c                   eval      kacr = %dec(v2ccli15:10:0) * 1000
183200120302     c                   exsr      CtrACR
183300120302if  2c                   IF        *in99
183400120302     c                   eval      *in72 = *on
183500120302     c                   leavesr
183600120302     c                   ENDIF
183700120307     c                   ENDIF
183800080502e   1c                   ENDIF
183900080502
184000080502      * - Codice cliente 16 valido
184100080502     c                   clear                   V2Drag16
184200080502if  1c                   IF            V2Ccli16 <> *blanks
184300080502     c                             and V2Ccli16 <> *zeros
184400120112     c     c_Digits      check     V2Ccli16                               20
184500080502if  2c                   if        *in20
184600090522     c                   seton                                        73  99
184700080502     c                   movel     $Msg(06)      V1Dmsg
184800080502     c                   goto      EndCtrV2
184900080502e   2c                   endif
185000090525     c                   eval      wksc = %dec(V2Ccli16:7:0)
185100090522     c                   exsr      sr_tibs69
185200090522if  2c                   if        *in99
185300090522     c                   seton                                        73
185400080502     c                   goto      EndCtrV2
185500080502e   2c                   endif
185600090522     c                   movel     wrag          V2Drag16
185700130121      * Controllo se il cliente è già presente in tabella.
185800130121     c                   exsr      ctrOSR
185900130121     c                   IF        *in99
186000130121     c                   eval      *in73 = *on
186100130121     c                   leavesr
186200130121     c                   ENDIF
186300120302      * controllo se il cliente esite su FNACR come 333 e 444
186400120307     c                   IF        v2fvas = 'N'
186500120302     c                   eval      kacr = %dec(v2ccli16:10:0) * 1000
186600120302     c                   exsr      CtrACR
186700120302if  2c                   IF        *in99
186800120302     c                   eval      *in73 = *on
186900120302     c                   leavesr
187000120302     c                   ENDIF
187100080502e   1c                   ENDIF
187200120307e   1c                   ENDIF
187300130125
187400130125      * Riaggancio il tntbe del rcd in modifica
187500130125     c                   movel(p)  DSkeyOSR      TBEke1
187600130125     c     K05TBE01      chain     TNTBE000
187700130123
187800091124      * traduttore personalizzato
187900091124     c                   if        v2fvas = *blanks
188000091124     c                   eval      *in99 = *on
188100091124     c                   eval      *in79 = *on
188200091124     c                   eval      v1dmsg = $msg(13)
188300091124     c                   leavesr
188400091124     c                   endif
188500090525
188600090525      * codice ordinante valido
188700090525     c                   if        v2ccor > *zeros
188800090525     c                   eval      wksc = v2ccor
188900090525     c                   exsr      sr_tibs69
189000090525     c                   if        *in99
189100090525     c                   eval      *in74 = *on
189200090525     c                   leavesr
189300090525     c                   endif
189400090525      * deve esistere anche su anagrafica clienti ritiro
189500090525     c                   eval      kacr = %dec(v2ccor:10:0) * 1000 + 333
189600090525     c     kacr          chain     fnacr01l
189700090525     c                   if        not %found(fnacr01l) or acratb <> *blanks
189800090525     c                   eval      *in99 = *on
189900090525     c                   eval      *in74 = *on
190000090525     c                   eval      v1dmsg = $msg(09)
190100090525     c                   leavesr
190200090525     c                   endif
190300090525     c                   eval      v2dcor = acrrsr
190400090525     c                   endif
190500090525
190600090525      * codice consegna valido
190700090525     c                   if        v2ccrc > *zeros
190800090525     c                   eval      wksc = v2ccrc
190900090525     c                   exsr      sr_tibs69
191000090525     c                   if        *in99
191100090525     c                   eval      *in75 = *on
191200090525     c                   leavesr
191300090525     c                   endif
191400090525      * deve esistere anche su anagrafica clienti ritiro
191500090525     c                   eval      kacr = %dec(v2ccrc:10:0) * 1000 + 444
191600090525     c     kacr          chain     fnacr01l
191700090525     c                   if        not %found(fnacr01l) or acratb <> *blanks
191800090525     c                   eval      *in99 = *on
191900090525     c                   eval      *in75 = *on
192000090525     c                   eval      v1dmsg = $msg(09)
192100090525     c                   leavesr
192200090525     c                   endif
192300090525     c                   eval      v2dcrc = acrrsr
192400090525     c                   endif
192500090525
192600090525      * chi paga non può essere vuoto
192700090624      * se non è traduttore personalizzato
192800091124     c                   if        v2cpag = *blanks and v2fvas = 'N'
192900090525     c                   eval      *in99 = *on
193000090525     c                   eval      *in77 = *on
193100090525     c                   eval      v1dmsg = $msg(11)
193200090525     c                   leavesr
193300090525     c                   endif
193400090610
193500090623      * flag per colli e peso non può essere vuoto
193600090624      * se non è traduttore personalizzato
193700091124     c                   if        v2cnclpkg = *blanks and v2fvas = 'N'
193800090610     c                   eval      *in99 = *on
193900090610     c                   eval      *in78 = *on
194000090610     c                   eval      v1dmsg = $msg(12)
194100090610     c                   leavesr
194200090610     c                   endif
194300090525
194400090525      * filiale consegna valida
194500090525     c                   if        v2cpoc > *zeros
194600090525     c     v2cpoc        chain     azorg01l
194700090525     c                   if        not %found(azorg01l) or
194800090525     c                             orgfva <> *blanks
194900090525     c                   eval      *in99 = *on
195000090525     c                   eval      *in76 = *on
195100090525     c                   eval      v1dmsg = $msg(10)
195200090525     c                   leavesr
195300090525     c                   endif
195400090525     c                   eval      v2dpoc = orgdes
195500090525     c                   endif
195600030805      *
195700030805     c     EndCtrV2      ENDSR
195800090522
195900090522      *---------------------------------------------------------------*
196000090522      * SR_TIBS69 - Decodifico/Controllo cliente                      *
196100090522      *---------------------------------------------------------------*
196200090522     c     sr_tibs69     BEGSR
196300090522
196400090522     c                   clear                   wrag
196500090522     c                   clear                   TIBS69ds
196600090522     c                   clear                   DS_ACO
196700090522     c                   clear                   DS_IND
196800090522     c                   clear                   DS_CLP
196900090522     c                   clear                   DS_CLS
197000090522     c                   eval      I69kcc = DUTkci
197100090522     c                   move      wksc          I69kac
197200090522     c                   eval      I69sif = KNSIF
197300090522     c                   call      'TIBS69R'
197400090522     c                   parm                    TIBS69ds
197500090522     c                   parm                    DS_ACO
197600090522     c                   parm                    DS_IND
197700090522     c                   parm                    DS_CLP
197800090522     c                   parm                    DS_CLS
197900090522     c                   if        O69err <> *blanks
198000090522     c                   eval      *in99 = *on
198100090522     c                   movel     $Msg(06)      V1Dmsg
198200090522     c                   endif
198300090522      * Decodifica cliente
198400090522     c                   movel     ACOrag        wrag
198500090522
198600090522     c                   ENDSR
198700130121
198800130121      /free
198900130121       //--------------------------------------------------------------
199000130121       //?Controllo se cliente già presente in tabella
199100130121       //--------------------------------------------------------------
199200130121       BEGSR ctrOSR;
199300130121
199400130121         *in99 = *off;
199500130121         clear wOSR;
199600130121       //?leggo tutta la tabella OSR
199700130121         TBEcod = 'OSR';
199800130121         setll TBEcod tntbe01l;
199900130123         reade(n) TBEcod tntbe01l;
200000130121
200100130121         DOW not %eof(tntbe01l);
200200130121           IF  TBEatb = *blanks;
200300130121           wosr = tbeuni;
200400130121           IF w§osrcli  = wKSC or w§OSRcl2  = wKSC or
200500130121              w§osrcl3  = wKSC or w§OSRcl4  = wKSC or
200600130121              w§osrcl5  = wKSC or w§OSRcl6  = wKSC or
200700130121              w§osrcl7  = wKSC or w§OSRcl8  = wKSC or
200800130121              w§osrcl9  = wKSC or w§OSRcl10 = wKSC or
200900130121              w§osrcl11 = wKSC or w§OSRcl12 = wKSC or
201000130121              w§osrcl13 = wKSC or w§OSRcl14 = wKSC or
201100130121              w§osrcl15 = wKSC or w§OSRcl16 = wKSC;
201200130121             IF  TBEke1 <> dskeyOSR;
201300130121               V1Dmsg = 'Cliente già presente in tabella con LNP ' +
201400130121                         %subst(TBEke1:1:3) + ' e SERIE ' +
201500130121                         %subst(TBEke1:4:2);
201600130121               *in99 = *on;
201700130121               leavesr;
201800130121             ENDIF;
201900130121           ENDIF;
202000130121           ENDIF;
202100130123           reade(n) TBEcod tntbe01l;
202200130121         ENDDO;
202300130121
202400130121       ENDSR;
202500130121      /end-free
202600120302
202700120302      *---------------------------------------------------------------*
202800120302      * CtrACR - Controllo se il codice cliente esiste su ACR         *
202900120302      *---------------------------------------------------------------*
203000120302     c     CtrACR        BEGSR
203100120302
203200120302      * Controllo 333
203300120302     c                   eval      kacr += 333
203400120302     c     kacr          chain     fnacr01l
203500120302     c                   if        not %found(fnacr01l) or acratb <> *blanks
203600120302     c                   eval      *in99 = *on
203700120302     c                   eval      v1dmsg = $msg(09)
203800120302     c                   eval      v1dmsg = %trim(v1dmsg) + ' con luogo 333'
203900120302     c                   leavesr
204000120302     c                   endif
204100120302      * Controllo 444
204200120302     c                   eval      kacr -= 333
204300120302     c                   eval      kacr += 444
204400120302     c     kacr          chain     fnacr01l
204500120302     c                   if        not %found(fnacr01l) or acratb <> *blanks
204600120302     c                   eval      *in99 = *on
204700120302     c                   eval      v1dmsg = $msg(09)
204800120302     c                   eval      v1dmsg = %trim(v1dmsg) + ' con luogo 444'
204900120302     c                   leavesr
205000120302     c                   endif
205100120302
205200120302     c                   ENDSR
205300090522
205400090522      *---------------------------------------------------------------*
205500090525      * GESV3  - Gestione videata dettaglio dati                      *
205600090522      *---------------------------------------------------------------*
205700090525     c     GesV3         BEGSR
205800090522      *
205900090522      * Inizializzazione videata
206000090525     c                   if        $CarV3 = *on
206100090525     c                   exsr      CarV3
206200090525     c                   move      *off          $CarV3
206300090522     c                   endif
206400091203      * Scrivo la testata e il piede
206500090522     c                   write     TB30T1
206600091203     c                   write     TB30T2
206700091203     c                   write     TB30P3
206800090522      *
206900090522      * Se esistono errori sulla videata
207000090522      * emetto la write del formato a indicatori spenti per vedere
207100090522      * le eventuali decodifiche
207200090522     c                   if        *in99
207300090522     c                   movea     *in           Win
207400090522     c                   movea     *zeros        *in(50)
207500090525     c                   write     TB30V3
207600090522     c                   movea     Win           *in
207700090522     c                   endif
207800090522      *
207900090522if  1c                   if        *in05
208000090525     c                   write     TB30V3
208100090522     c                   exfmt     PROTECT
208200090522x   1c                   else
208300090525     c                   exfmt     TB30V3
208400090522e   1c                   endif
208500090522     c                   eval      *in99 = *off
208600090522     c                   clear                   V1Dmsg
208700090522     c                   clear                   wTasto
208800090522      *
208900090522sel 1c                   select
209000090522      * F03=Fine
209100090522w   1c                   when      *inKC
209200090522     c                   exsr      F03V1
209300090525     c                   leavesr
209400090522      * F12=Ritorno
209500090522w   1c                   when      *inKL
209600090525     c                   exsr      F12V3
209700090525     c                   leavesr
209800090522e   1c                   endsl
209900090525      *
210000090525      * Controllo dati immessi a video
210100090525      * (non si fanno se richisto l'annullamento)
210200090525if  1c                   if        not *inKQ
210300090525     c                   exsr      CtrV3
210400090525e   1c                   endif
210500090525      *
210600090525      * Aggiornamento se non ci sono errori
210700090525if  1c                   if        not *in99 and not *in90
210800090525     c                             and (*inKF or *inKE or *inKQ)
210900090525sel 2c                   select
211000090525w   2c                   when      *inKE
211100090525     c                   eval      wTasto = '05'
211200090525w   2c                   when      *inKF
211300090525     c                   eval      wTasto = '06'
211400090525w   2c                   when      *inKQ
211500090525     c                   eval      wTasto = '16'
211600090525e   2c                   endsl
211700091204       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
211800091204       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
211900091204       //  ?NON ha più senso gestire la richiesta di trasmissione!?
212000091204     ***c                   eval      $CarW1 = *on
212100091204     ***c                   eval      $TipVid = 'A'
212200091204     c                   exsr      AggTBE
212300090525e   1c                   endif
212400090522      *
212500090525     c                   ENDSR
212600090525
212700090525      *---------------------------------------------------------------*
212800090525      * CARV3  - Caricamento dati terza videata                       *
212900090525      *---------------------------------------------------------------*
213000090525     c     CarV3         BEGSR
213100090525      *
213200090525     c                   eval      V2Ccbo = D§OSRcbo
213300090525     c                   eval      V2Dcbo = *blanks
213400090525     c                   eval      V2Cman = D§OSRman
213500090525     c                   eval      V2Ccpy = D§OSRcpy
213600090525     c                   eval      V2Cstp = D§OSRstp
213700090525     c                   eval      V2Cass = D§OSRass
213800090525     c                   eval      V2Cdir = D§OSRdir
213900090525     c                   eval      V2Csta = D§OSRsta
214000090525     c                   eval      V2Crmn = D§OSRrmn
214100090525     c                   eval      V2Crma = D§OSRrma
214200090525      * decodifico
214300090525     c                   clear                   V2Dcbo
214400090525if  1c                   if        V2Ccbo <> *blanks
214500090525     c                   clear                   ds3A
214600090525     c                   eval      TBLcod = '3A'
214700090525     c                   movel(p)  V2Ccbo        TBLkey
214800090525     c     K03TAB00      chain     TABEL
214900090525if  2c                   if        %found(TABEL00F)
215000090525     c                             and TBLflg = *blanks
215100090525     c                   movel     TBLuni        ds3A
215200090525     c                   movel     §3Ades        V2Dcbo
215300090525e   1c                   endif
215400090525e   1c                   endif
215500091124
215600091124      * se immissione imposto campi di dft
215700091124     c                   if        *in01
215800091124     c                   eval      v2ccbo = '2R'
215900091124     c                   eval      v2cman = 'S'
216000091124     c                   eval      v2cstp = 'S'
216100091124     c                   eval      v2ccpy = 'S'
216200091124     c                   eval      v2cass = 'S'
216300091124     c                   eval      v2cdir = 'S'
216400091124     c                   eval      v2csta = 'S'
216500091124     c                   endif
216600090525
216700090525     c                   ENDSR
216800090522
216900090522      *---------------------------------------------------------------*
217000090525      * F12V3  - Tasto funzionale F12 -> Ritorno                      *
217100090522      *---------------------------------------------------------------*
217200090525     c     F12V3         BEGSR
217300090522      *
217400090525     c                   eval      $TipVid = '2'
217500090522      *
217600090522     c                   ENDSR
217700090522
217800090522      *---------------------------------------------------------------*
217900090525      * CTRV2  - Controllo e decodifica terza videata                 *
218000090522      *---------------------------------------------------------------*
218100090525     c     CtrV3         BEGSR
218200090522      *
218300090522     c                   movea     *zeros        *in(50)
218400090522     c                   clear                   V1Dmsg
218500090522      *
218600090522      * Ricerche
218700090522     c                   clear                   xx
218800090522     c                   clear                   $Fld
218900090525      * - Codice bolla da forzare
219000090525     c     '?'           scan      V2Ccbo                                 20
219100090525if  1c                   if        *in20
219200090525     c                   add       1             XX
219300090525     c                   eval      $Fld(xx) = 'V2CCBO    '
219400090525e   1c                   endif
219500090522      *
219600090522if  1c                   if        xx > *zeros
219700090522     c                   exsr      Search
219800090522e   1c                   endif
219900090522      *
220000090522      * Controlli
220100090522      *
220200090522      * Codice bolla da forzare in bollettazione
220300090522     c                   clear                   V2Dcbo
220400090522if  1c                   if        V2Ccbo <> *blanks
220500090522     c                   clear                   ds3A
220600090522     c                   eval      TBLcod = '3A'
220700090522     c                   movel(p)  V2Ccbo        TBLkey
220800090522     c     K03TAB00      chain     TABEL
220900090522if  2c                   if        %found(TABEL00F)
221000090522     c                             and TBLflg = *blanks
221100090522     c                   movel     TBLuni        ds3A
221200090522     c                   movel     §3Ades        V2Dcbo
221300090522x   2c                   else
221400090522     c                   seton                                        54  99
221500090522     c                   movel     $Msg(07)      V1Dmsg
221600090525     c                   leavesr
221700090522e   2c                   endif
221800090522e   1c                   endif
221900090522      *
222000090525     c                   ENDSR
222100030805
222200030805      *---------------------------------------------------------------*
222300030805      * GESW1  - Gestione videata dati relativi alla trasmissione     *
222400030805      *---------------------------------------------------------------*
222500030805     c     GesW1         BEGSR
222600030805      *
222700030805      * Inizializzazione videata
222800030805if  1c                   if        $CarW1 = *on
222900030805     c                   exsr      CarW1
223000030805     c                   movel     *off          $CarW1
223100030805e   1c                   endif
223200030805      *
223300030805if  1c                   if        *in05
223400030805     c                   write     TB30W1
223500030805     c                   exfmt     PROTECT
223600030805x   1c                   else
223700030805     c                   exfmt     TB30W1
223800030805e   1c                   endif
223900030805     c                   eval      *in99 = *off
224000030805     c                   clear                   W1MSG
224100030805      *
224200030805sel 1c                   select
224300030805      * F12=Ritorno
224400030805w   1c                   when      *inKL
224500030805     c                   exsr      F12W1
224600030805     c                   goto      EndGesW1
224700030805e   1c                   endsl
224800030805      *
224900030805      * Controllo dati immessi a video
225000030805     c                   exsr      CtrW1
225100030805      *
225200030805      * Aggiornamento se non ci sono errori
225300030805if  1c                   if        not *in99 and *inKF
225400030805     c                   exsr      AggTBE
225500030805e   1c                   endif
225600030805      *
225700030805     c     EndGesW1      ENDSR
225800030805
225900030805      *---------------------------------------------------------------*
226000030805      * CARW1  - Caricamento dati window                              *
226100030805      *---------------------------------------------------------------*
226200030805     c     CarW1         BEGSR
226300030805      *
226400030805     c                   movea     *zeros        *in(50)
226500030805      *
226600030805sel 1c                   select
226700030805      *
226800030805      * F5=Ripristino
226900030805w   1c                   when      *inKE   and  *in04
227000030805     c                   eval      W1FTT = TBEftt
227100030805      *
227200030805      * F6=Conferma
227300030805w   1c                   when      *inKF
227400030805sel 2c                   select
227500030805      *   Immissione
227600030805w   2c                   when      *in01
227700030805     c                   eval      W1FTT = TBXftt
227800030805      *   Modifica / Ripristino
227900030805w   2c                   when      *in02   or    *in04
228000030805     c                   eval      W1FTT = TBEftt
228100030805e   2c                   endsl
228200030805      *
228300030805      * F16=Annullamento
228400030805w   1c                   when      *inKQ   and  not *in04
228500030805     c                   eval      W1FTT = TBEftt
228600030805      *
228700030805e   1c                   endsl
228800030805      *
228900030805      * Se NON immissione: visualizzo i dati relativi all'ultima
229000030805      *   trasmissione
229100030805if  1c                   if        not *in01
229200030805     c                   eval      W1FLT = TBEflt
229300030805     c                   eval      W1FTR = TBEftr
229400030805if  2c                   if        TBEdtr <> 0
229500030805     c                   clear                   WLBDAT
229600030805     c                   z-add     TBEdtr        G02inv
229700030805     c                   movel     '3'           G02err
229800030805     c                   call      'XSRDA8'
229900030805     c                   parm                    WLBDAT
230000030805     c                   z-add     G02dat        W1DTR
230100030805e   2c                   endif
230200030805e   1c                   endif
230300030805      *
230400030805     c                   ENDSR
230500030805
230600030805      *---------------------------------------------------------------*
230700030805      * CTRW1  - Controllo e decodifica window                        *
230800030805      *---------------------------------------------------------------*
230900030805     c     CtrW1         BEGSR
231000030805      *
231100030805     c                   movea     *zeros        *in(50)
231200030805      *
231300091203     c                   ENDSR
231400030805
231500030805      *---------------------------------------------------------------*
231600030805      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
231700030805      *---------------------------------------------------------------*
231800030805     c     F12W1         BEGSR
231900030805      *
232000030805     c                   eval      $TipVid = '2'
232100030805      *
232200030805     c                   ENDSR
232300030805
232400030805      *---------------------------------------------------------------*
232500030805      * CHNTBE * Aggancio tabella                                     *
232600030805      *---------------------------------------------------------------*
232700030805     c     ChnTBE        BEGSR
232800030805      *
232900030805     c                   movel     T02cod        TBEcod
233000030805     c                   movel(p)  DSkeyOSR      TBEke1
233100030805     c                   clear                   TBEke2
233200030805     c                   clear                   TBElin
233300030805     c                   movel     KNSIF         TBEsif
233400091203     c     K05TBE01      chain     TNTBE000
233500030805      * Se non ho trovato il record con il sistema informativo
233600030805      * che ho in linea, lo abblenco
233700030805if  1c                   if        not %found(TNTBE01L)
233800030805     c                   clear                   TBEsif
233900091203     c     K05TBE01      chain     TNTBE000
234000030805e   1c                   endif
234100030805      *
234200030805     c                   ENDSR
234300030805
234400030805      *---------------------------------------------------------------*
234500030805      * AGGTBE * Aggiornamento tabella                                *
234600030805      *---------------------------------------------------------------*
234700030805     c     AggTBE        BEGSR
234800030805      *
234900091203      /free
235000091203
235100091203         // -?Ciclo di elaborazione per ogni sistema informativo?
235200091203         For  w_SisInf = 1  To  %elem($Libr);
235300091203
235400091203           // -?Apertura (eventuale) del file ed aggancio record?
235500091203           if  w_SisInf > 1;
235600091203             exsr  sr_OpenFileTab;
235700091203             exsr  ChnTBE;
235800091203           endif;
235900091203
236000091203         //? N.B.                                                      ?
236100091203         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
236200091203         //   trasmissione (vedi flag TBEFTR).                        ?
236300091203         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
236400091203         //   subito nello stesso file di entrambi i S.I. - in due    ?
236500091203         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
236600091203
236700091203      /end-free
236800030805sel 1c                   SELECT
236900030805      *
237000030805      * F5=Ripristino
237100030805w   1c                   WHEN      wTasto='05'  and  *in04
237200091203      /free
237300091203               if  %found(TNTBE01L);
237400091203      /end-free
237500030805     c                   clear                   TBEatb
237600030805     c                   clear                   TBEftr
237700030805     c                   UPDATE    TNTBE000
237800091203      /free
237900091203               else;
238000091203                 // - Record appena cancellato fisicamente
238100091203                 exsr  RieTBE;
238200091203                 WRITE  TNTBE000;
238300091203               endif;
238400091203      /end-free
238500030805      *
238600030805      * F6=Conferma
238700030805w   1c                   WHEN      wTasto='06'
238800030805     c                   exsr      RieTBE
238900030805sel 2c                   select
239000030805      *   Immissione
239100030805w   2c                   when      *in01
239200030805     c                   WRITE     TNTBE000                             22
239300030805      *   Modifica / Ripristino
239400030805w   2c                   when      *in02   or    *in04
239500030805     c                   UPDATE    TNTBE000
239600030805e   2c                   endsl
239700030805      *
239800030805      * F16=Annullamento
239900030805w   1c                   WHEN      wTasto='16'  and  not *in04
240000030805     c                   movel     'A'           TBEatb
240100030805     c                   clear                   TBEftr
240200030805     c                   UPDATE    TNTBE000
240300030805      *
240400030805e   1c                   ENDSL
240500091203      /free
240600091203
240700091203         EndFor;
240800170804
240900170804         // -?Si richiama il pgm di gestione Particolarità?
241000170905         If  wTasto <> '16';
241100170905           exsr  sr_GesTab7U_RC;
241200170905         EndIf;
241300091203
241400091203      /end-free
241500030805      *
241600120112     c                   if        $Called = *off
241700030805      * Torno alla prima videata che carico come da inizio pgm
241800030805     c                   movel     '1'           $TipVid
241900030805     c                   movel     *on           $CarV1
242000030805     c                   movel     *on           $CarV2
242100090522     c                   movel     *on           $CarV3
242200120302
242300170804     *** * prima di tornare alla prima videata devo richiamare il pgm di gestione
242400170804     *** * particolarità
242500170804     ***c                   clear                   kpjbu
242600170804     ***c                   call      'TRTB71C'
242700170804     ***c                   parm                    kpjba
242800170804      *
242900120302
243000120112     c                   else
243100120305
243200170804     *** * prima di uscire devo richiamare il pgm di gestione
243300170804     *** * particolarità
243400170804     ***c                   clear                   kpjbu
243500170804     ***c                   call      'TRTB71C'
243600170804     ***c                   parm                    kpjba
243700120112      * - Uscita dal pgm
243800120112     c                   exsr      F03V1
243900120112     c                   endif
244000120112      *
244100030805     c                   ENDSR
244200170905
244300170905      /free
244400170905
244500170905       //--------------------------------------------------------------
244600170905       //?Richiamo *pgm di gestione Particolarità Consegna "RC"        ?
244700170905       //?  in tab. "7U".                                              ?
244800170905       //--------------------------------------------------------------
244900170905       BEGSR  sr_GesTab7U_RC;
245000170905
245100170905         sk_V2Ccli( 1) = V2Ccli;
245200170905         sk_V2Ccli( 2) = V2Ccli2;
245300170905         sk_V2Ccli( 3) = V2Ccli3;
245400170905         sk_V2Ccli( 4) = V2Ccli4;
245500170905         sk_V2Ccli( 5) = V2Ccli5;
245600170905         sk_V2Ccli( 6) = V2Ccli6;
245700170905         sk_V2Ccli( 7) = V2Ccli7;
245800170905         sk_V2Ccli( 8) = V2Ccli8;
245900170905         sk_V2Ccli( 9) = V2Ccli9;
246000170905         sk_V2Ccli(10) = V2Ccli10;
246100170905         sk_V2Ccli(11) = V2Ccli11;
246200170905         sk_V2Ccli(12) = V2Ccli12;
246300170905         sk_V2Ccli(13) = V2Ccli13;
246400170905         sk_V2Ccli(14) = V2Ccli14;
246500170905         sk_V2Ccli(15) = V2Ccli15;
246600170905         sk_V2Ccli(16) = V2Ccli16;
246700170905
246800170905         For  xx = 1  To  %elem( sk_V2Ccli );
246900170905
247000170905           If  sk_V2Ccli(xx) > *zero;
247100170905
247200170905             clear  kpjbu;
247300170905             // -?KPJBU = "*_ " serve al cl TRTB71C per fargli capire?
247400170905             //  ?che riceve anche un 2° parametro?
247500170905             kpjbu = '*_ ';
247600170905             reset  TNTB30ds;
247700170905             select;
247800170905               // -?Ripristino?
247900170905               when  wTasto = '05'  and  *in04;
248000170905                 TNTB30ds.§OSRopz = '7';
248100170905               // -?Annullamento?
248200170905               when  wTasto = '16'  and  not *in04;
248300170905                 TNTB30ds.§OSRopz = '4';
248400170905               //*·// -?Conferma?
248500170905               //*·when  wTasto = '06';
248600170905               //*·  clear  TNTB30ds.§OSRopz;
248700170905             endsl;
248800170905             TNTB30ds.§OSRcli = %int( sk_V2Ccli(xx) );
248900170905
249000170905             TRTB71C ( KPJBA : TNTB30ds );
249100170905
249200170905             // -?Nel caso venga restituito un errore: si visualizza?
249300170905             //  ?su una window (l'aggiornamento è già stato eseguito)?
249400170905             if  TNTB30ds.§OSRerr <> *blank;
249500170905               W2msg = TNTB30ds.§OSRmsg;
249600170905               DoW  Not *inKL;
249700170905                 exfmt  TB30W2;
249800170905               EndDo;
249900170905             endif;
250000170905
250100170905           EndIf;
250200170905
250300170905         EndFor;
250400170905
250500170905       ENDSR;
250600170905
250700170905      /end-free
250800030805
250900030805      *---------------------------------------------------------------*
251000030805      * RIETBE * Riempimento dati tabella                             *
251100030805      *---------------------------------------------------------------*
251200030805     c     RieTBE        BEGSR
251300030805      *
251400030805     c                   clear                   TBEatb
251500030805     c                   if        TBXsif <> *blanks
251600030805     c                   movel     KNSIF         TBEsif
251700030805     c                   else
251800030805     c                   clear                   TBEsif
251900030805     c                   endif
252000030805     c                   movel     TBXapl        TBEapl
252100030805     c                   movel     T02cod        TBEcod
252200030805     c                   movel     DSkeyOSR      TBEke1
252300030805     c                   clear                   TBEke2
252400030805      *
252500030805     c                   clear                   dOSR
252600030805     c                   move      V2Ccli        D§OSRcli
252700030805     c                   eval      D§OSRrag = V2Drag
252800040303     c                   move      V2Ccli2       D§OSRcl2
252900040303     c                   eval      D§OSRra2 = V2Drag2
253000040303     c                   move      V2Ccli3       D§OSRcl3
253100040303     c                   eval      D§OSRra3 = V2Drag3
253200040303     c                   move      V2Ccli4       D§OSRcl4
253300090610     c                   eval      D§OSRra4 = V2Drag4
253400080430     c                   move      V2Ccli5       D§OSRcl5
253500091204      *
253600091124     c                   if        v2fvas = 'N'
253700091124     c                   clear                   d§osrvas
253800091124     c                   else
253900090522     c                   eval      d§osrvas = v2fvas
254000091124     c                   endif
254100090522     c                   eval      d§osrcor = v2ccor
254200090522     c                   eval      d§osrcrc = v2ccrc
254300090522     c                   eval      d§osrpag = v2cpag
254400090623     c                   eval      d§osrfcp = v2cnclpkg
254500090522     c                   eval      d§osrpoc = v2cpoc
254600091204      *
254700030805     c                   eval      D§OSRcbo = V2Ccbo
254800030805     c                   eval      D§OSRman = V2Cman
254900030805     c                   eval      D§OSRcpy = V2Ccpy
255000030805     c                   eval      D§OSRstp = V2Cstp
255100030805     c                   eval      D§OSRass = V2Cass
255200040414     c                   eval      D§OSRdir = V2Cdir
255300031002     c                   eval      D§OSRsta = V2Csta
255400040218     c                   eval      D§OSRrmn = V2Crmn
255500040218     c                   eval      D§OSRrma = V2Crma
255600080502     c                   move      V2Ccli6       D§OSRcl6
255700080502     c                   move      V2Ccli7       D§OSRcl7
255800080502     c                   move      V2Ccli8       D§OSRcl8
255900080502     c                   move      V2Ccli9       D§OSRcl9
256000080502     c                   move      V2Ccli10      D§OSRcl10
256100080502     c                   move      V2Ccli11      D§OSRcl11
256200080502     c                   move      V2Ccli12      D§OSRcl12
256300080502     c                   move      V2Ccli13      D§OSRcl13
256400080502     c                   move      V2Ccli14      D§OSRcl14
256500080502     c                   move      V2Ccli15      D§OSRcl15
256600080502     c                   move      V2Ccli16      D§OSRcl16
256700030805     c                   movel(p)  dOSR          TBEuni
256800030805      *
256900091204     c                   movel     'S'           TBEftt
257000030807     c                   z-add     TBXflt        TBEflt
257100091203      /free
257200091203
257300091203         select;
257400091203           // - Annullamento o Ripristino
257500091203           when  (wTasto='05'  and  *in04)   or
257600091203                 (wTasto='16'  and  not *in04);
257700091203             clear  TBEftr;
257800091203             clear  TBEdtr;
257900091203           // - Aggiornamento o Scrittura in sede
258000091203           when  w_SisInf = 1;
258100091203             TBEftr = 'T';
258200091203             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
258300091203                                     : 1 : 8 ) );
258400091203           // - Aggiornamento o Scrittura in filiale
258500091203           other;
258600091203             TBEftr = 'R';
258700091203             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
258800091203                                     : 1 : 8 ) );
258900091203         endsl;
259000091203
259100091203      /end-free
259200030805      *
259300030805     c                   ENDSR
259400030805
259500091203       //--------------------------------------------------------------
259600091203       //?Schiere a tempo di compilazione.                             ?
259700091203       //--------------------------------------------------------------
259800091203
259900091203** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
260000091203SETRAS  GAITRAGRU FILTRAGRU
260100091203AS888   GAITRAGRPSFILTRAGRPF
260200030806**   -$OPZ-   *
260300030805  Inserimento
260400030805    Modifica
260500030805     Copia
260600030805  Annullamento
260700030805Visualizzazione
260800030805   ANNULLATO
260900030806**   -$MSG-                                                                  *
261000090522Immettere la Fil. di emissione O.R.M.                                          01
261100091204Fil. di emissione O.R.M. inesistente o annullata                               02
261200030808Immettere un numero di serie O.R.M.                                            03
261300030806Numero di serie O.R.M. non valido                                              04
261400030806Immettere il codice cliente                                                    05
261500030806Codice cliente errato                                                          06
261600030807Codice bolla inesistente in tab. "3A"                                          07
261700130418Se ricerca per Cliente no ricerca per Filiale/Serie                            08
261800090525Cliente non trovato in anagrafica clienti ritiro                               09
261900090525Filiale consegna errata                                                        10
262000090525Inserire il flag "chi paga"                                                    11
262100090623Inserire come si devono impostare i colli e peso sul ritiro                    12
262200091124Inserire il flag traduttore personalizzato                                     13
262300130418Immettere o Filiale/Serie o Cliente                                            14
