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
005500030805
005600030805      *---------------------------------------------------------------*
005700030805      *   S T R U T T U R E   D A T I                                 *
005800030805      *---------------------------------------------------------------*
005900030805      *
006000030805      * Parametri
006100030805     d KPJBA         e ds
006200030805      *
006300030805      * Passaggio Parametri al pgm TIBS02R
006400030805     d TIBS02DS      e ds                  inz
006500030805     d  T02mod       e                     inz('R')
006600030805     d  T02cod       e                     inz('OSR')
006700030805      *
006800030805      * Tabella OSR = Serie O.R.M.
006900030805     d dOSR          e ds                  inz
007000091022     d aD§OSRcl5                      7A   overlay(D§OSRcl5)
007100080502     d aD§OSRcl6                      7A   overlay(D§OSRcl6)
007200080502     d aD§OSRcl7                      7A   overlay(D§OSRcl7)
007300080502     d aD§OSRcl8                      7A   overlay(D§OSRcl8)
007400080502     d aD§OSRcl9                      7A   overlay(D§OSRcl9)
007500080502     d aD§OSRcl10                     7A   overlay(D§OSRcl10)
007600080502     d aD§OSRcl11                     7A   overlay(D§OSRcl11)
007700080502     d aD§OSRcl12                     7A   overlay(D§OSRcl12)
007800080502     d aD§OSRcl13                     7A   overlay(D§OSRcl13)
007900080502     d aD§OSRcl14                     7A   overlay(D§OSRcl14)
008000080502     d aD§OSRcl15                     7A   overlay(D§OSRcl15)
008100080502     d aD§OSRcl16                     7A   overlay(D§OSRcl16)
008200030805      *
008300030807      * Tabella 3A = Codici bolla
008400030807     d ds3A          e ds                  inz
008500030805      *
008600030805      * Parametri per interrogazione serie O.R.M.
008700030805     d TNTB301ds     e ds                  inz
008800030805     d  B30err       e                     inz(*off)
008900030805      *
009000030805      * Tracciato record file TNTBE00F
009100030805     d TNTBEds       e ds                  extname(TNTBE00F) inz
009200030805     d xTNTBEds      e ds                  extname(TNTBE00F) inz
009300030805     d                                     prefix(TBX:3)
009400130121
009500130121      * Tabella OSR di lavoro
009600130121     d wOSR          e ds                  extname(dOSR) inz
009700130121     d                                     prefix(w:1)
009800030805      *
009900030805     d TIBS34DS      e ds                  inz
010000030805     d DDATIUTE      e ds                  inz
010100030805     d AZUTEDS       e ds                  extname(AZUTE00F) inz
010200030805      *
010300030805     d TIBS69DS      e ds                  inz
010400030805     d DS_ACO        e ds                  extname(CNACO00F) inz
010500030805     d DS_IND        e ds                  extname(CNIND00F) inz
010600030805     d DS_CLP        e ds                  extname(CNCLP00F) inz
010700030805     d DS_CLS        e ds                  extname(FNCLS00F) inz
010800030805      *
010900030805     d WLBDAT          ds                  inz
011000030805     d  G02DAT                 1      8  0 inz
011100030805     d  G02INV                 9     16  0 inz
011200030805     d  G02ERR                17     17    inz
011300030805     d  G02TGI                18     22  0 inz
011400030805      *
011500030805     d DSkeyOSR        ds             5    inz
011600030805     d  V1Cpoe
011700030805     d  V1Cnrs
011800091203       //
011900091203       // -?Ridefinizione elenco librerie in elaborare le tabelle?
012000091203     d ds_Libr         ds                  inz
012100091203     d  $Libr                        10    dim(c_NrLibr) inz
012200030805      *
012300091203      * - Status ds
012400091203     d Status         sds
012500030805     d  VTCpgm           *proc
012600030805
012700030805      *---------------------------------------------------------------*
012800030805      *   V A R I A B I L I                                           *
012900030805      *---------------------------------------------------------------*
013000030805      *
013100091203      * - Indici di schiera
013200030805     d XX              s              2  0 inz
013300091203      * - Flags booleani
013400120112     d $Called         s               n   inz(*off)
013500030805     d $Fine           s              1a   inz(*off)
013600030805     d $CarV1          s              1a   inz(*on)
013700030805     d $CarV2          s              1a   inz(*off)
013800090522     d $CarV3          s              1a   inz(*off)
013900030805     d $CarW1          s              1a   inz(*off)
014000091203      * - Variabili per la gestione dei video
014100030805     d $TipVid         s              1a   inz('1')
014200030805     d $Tibs02         s              1a   inz(*off)
014300030805     d Win             s             99a   inz(*zeros)
014400091203      * - Campi chiave per l'accesso ai file
014500091203     d kacr            s                   like(acrcro)
014600091203     d CodUt           s              1  0 inz(1)
014700091203      * - Campi di comodo
014800090522     d wksc            s                   like(acoksc)
014900090522     d wrag            s                   like(acorag)
015000030805     d wTasto          s              2a   inz(*zeros)
015100091203       // -?Nome del sistema?
015200091203     d currSysNeta     s              8a   inz
015300091203       // -?Nome esteso Libreria/File del file tabella?
015400091203     d wLibFile        s             21a   inz
015500091203       // -?Campi di comodo?
015600091203     d w_iSystem       s              1  0 inz
015700091203     d w_SisInf        s              3  0 inz
015800091203
015900091203       //--------------------------------------------------------------
016000091203       //?Definizione prototipi procedure.                             ?
016100091203       //--------------------------------------------------------------
016200091203
016300091203       // -?Reperimento NETA sistema AS/400 corrente?
016400091203      /copy gaitrasrc/srcProtoPr,UBRTVNETA
016500030805
016600030805      *---------------------------------------------------------------*
016700030805      *   M A I N   L I N E                                           *
016800030805      *---------------------------------------------------------------*
016900030805      *  Riepilogo indicatori utilizzati:                             *
017000030805      *  --------------------------------                             *
017100030805      *  01 - Record inesistente (inserimento)                        *
017200030805      *  02 - Record esistente   (modifica)                           *
017300030805      *  04 - Record annullato   (ripristino)                         *
017400030805      *  20 - Comodo                                                  *
017500030805      *  22 - Errori in scrittura record (WRITE)                      *
017600030805      *---------------------------------------------------------------*
017700030805      *
017800030805      * Operazioni iniziali
017900030805     c                   exsr      RutInz
018000030805      *
018100030805      * Gestione video
018200030805     c                   dow       $Fine = *off
018300030805     c     $TipVid       caseq     '1'           GesV1
018400030805     c     $TipVid       caseq     '2'           GesV2
018500090522     c     $TipVid       caseq     '3'           GesV3
018600030805     c     $TipVid       caseq     'A'           GesW1
018700030805     c                   endcs
018800030805     c                   enddo
018900030805      *
019000030805      * Fine
019100030805     c                   if        $Tibs02 = *on
019200030805     c                   clear                   TIBS02ds
019300030805     c                   movel     'C'           T02tla
019400030805     c                   call      'TIBS02R'
019500030805     c                   parm                    KPJBA
019600030805     c                   parm                    TIBS02ds
019700030805     c                   endif
019800030805      *
019900091203     c                   return
020000030805
020100030805      *---------------------------------------------------------------*
020200030805      * RutInz - Operazioni Iniziali                                  *
020300030805      *---------------------------------------------------------------*
020400030805     c     RutInz        BEGSR
020500030805      *
020600030805      * Ricezione parametri
020700030805     c     *entry        plist
020800030805     c                   parm                    KPJBA
020900030805      *
021000030805      * Definizioni chiavi di accesso
021100030805     c     K05TBE01      klist                                                  *tntbe01l
021200030805     c                   kfld                    TBEcod                         -tabella
021300030805     c                   kfld                    TBEke1                         -chiave uno
021400030805     c                   kfld                    TBEke2                         -chiave due
021500030805     c                   kfld                    TBElin                         -lingua
021600030805     c                   kfld                    TBEsif                         -s.informativo
021700030805      *
021800030805     c     K03TAB00      klist
021900030805     c                   kfld                    CodUt
022000030805     c                   kfld                    TBLcod
022100030805     c                   kfld                    TBLkey
022200091203      *
022300091203     c                   movel     *on           *inLR
022400091203      /free
022500091203
022600091203         // -?Verifica del sistema AS/400 corrente?
022700091203         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
022800091203           $Fine = *on;
022900091203           leavesr;
023000091203         endif;
023100091203
023200091203         // -?Impostazione elenco librerie in cui gestire le tabelle?
023300091203         //  ?(a seconda del sistema in cui si stà lavorando)?
023400091203         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
023500091203         if  w_iSystem = *zero;
023600091203           $Fine = *on;
023700091203           leavesr;
023800091203         endif;
023900091203
024000091203      /end-free
024100030805      *
024200030805      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
024300030805     c     *dtaara       define    §azute        azuteds
024400030805     c     *dtaara       define    §datiute      ddatiute
024500030805      *
024600030805     c                   clear                   AzUteDs
024700030805     c                   clear                   DDatiUte
024800030805     c                   clear                   Tibs34Ds
024900030805     c                   in(E)     *dtaara
025000030805if  1c                   if        %Error  or  RSUT = *blanks
025100030805     c                   call      'TIBS34R'
025200030805     c                   parm                    Tibs34Ds
025300030805     c                   in        *dtaara
025400030805e   1c                   endif
025500030805      *-- Verifica errori e autorità profilo
025600030805sel 1c                   SELECT
025700030805      *-- controllo se ho errori nei dati utente
025800030805      *--   nel qual caso NON risulta un profilo abilitato
025900030805w   1c                   WHEN      DUTerr = 'E'
026000030805     c                   eval      $Fine  = *on
026100030805      *
026200030805      *-- CONTROLLO AUTORITA'
026300030805      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
026400030805      *-- se il chiamante non richiede autorità specifica verificare
026500030805      *--   quella del profilo
026600030805      *-- se il chiamante richiede autorità specifica verificarla,
026700030805      *--  se è blank verificare quella del profilo
026800030805      *
026900030805      * se UTEAUT = *BLANK non siamo in sede
027000030805w   1c                   WHEN      UTEaut = *blank
027100030805      *
027200030805x   1c                   OTHER
027300030805      *
027400030805e   1c                   ENDSL
027500091203      /free
027600091203
027700091203         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
027800091203         w_SisInf = 1;
027900091203         exsr  sr_OpenFileTab;
028000091203
028100091203      /end-free
028200030805      *
028300030805      * Aggancio dati generali della tabella in esame
028400030805     c                   clear                   TBEcod
028500030805     c                   move      *zeros        TBEke1
028600030805     c                   move      T02cod        TBEke1
028700030805     c                   clear                   TBEke2
028800030805     c                   clear                   TBElin
028900030805     c                   movel     KNSIF         TBEsif
029000091211     c     K05TBE01      chain(n)  TNTBE000
029100030805     c                   if        not %found(TNTBE01L)
029200030805     c                   clear                   TBEsif
029300091211     c     K05TBE01      chain(n)  TNTBE000
029400030805     c                   endif
029500030805     c                   if        %found(TNTBE01L)
029600030805     c                   movel     TNTBEds       xTNTBEds
029700030805     c                   else
029800030805     c                   clear                   xTNTBEds
029900030805     c                   endif
030000030808      *
030100030808     c                   clear                   TB30V1
030200030808      *
030300030808      * Imposto il P.O. emissione ORM di default
030400030808     c                   clear                   V1Cpoe
030500120112      *
030600120112      * Verifico se ho ricevuto parametri
030700120112if  1c                   if        %subst(KPJBU:1:8) = *blanks
030800120112     c                   leavesr
030900120112e   1c                   endif
031000120112      *
031100120112      * Controllo i parametri ricevuti
031200120112     c                   eval      $Called = *on
031300120112     c                   eval      V1Cpoe  = %int( %subst(KPJBU : 1 : 3) )
031400120112     c                   eval      V1Cnrs  = %subst(KPJBU : 4 : 2)
031500120112     c                   exsr      CtrV1
031600120112if  1c                   if        *in28
031700120112     c                   movel(p)  V1Dmsg        KPJBU
031800120112     c                   eval      $Fine   = *on
031900120112     c                   leavesr
032000120112x   1c                   else
032100120112     c                   eval      $TipVid = '2'
032200120112     c                   eval      $CarV2  = *on
032300120112e   1c                   endif
032400030805      *
032500030805     c                   ENDSR
032600091203      /free
032700091203
032800091203       //--------------------------------------------------------------
032900091203       //?Apertura dei files tabelle nel sistema informativo impostato.?
033000091203       //--------------------------------------------------------------
033100091203       BEGSR  sr_OpenFileTab;
033200091203
033300091203         // -?Chiusura (eventuale) archivio?
033400091203         if  %open(TNTBE01L);
033500091203           close  TNTBE01L;
033600091203         endif;
033700091203
033800091203         // -?Apertura archivio?
033900091203         ds_Libr  = $Libraries(w_iSystem);
034000091203         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
034100091203         open  TNTBE01L;
034200091203
034300091203       ENDSR;
034400091203
034500091203      /end-free
034600030805
034700030805      *---------------------------------------------------------------*
034800030805      * GESV1  - Gestione videata selezione codice tabella            *
034900030805      *---------------------------------------------------------------*
035000030805     c     GesV1         BEGSR
035100030805      *
035200030805      * Inizializzazione videata
035300030805if  1c                   if        $CarV1 = *on
035400030805     c                   exsr      CarV1
035500030805     c                   movel     *off          $CarV1
035600030805e   1c                   endif
035700030805      *
035800030805      * Scrivo la testata
035900030805     c                   clear                   T1opz
036000030805     c                   write     TB30T1
036100030805      *
036200030805      * Se esistono errori sulla videata
036300030805      * emetto la write del formato a indicatori spenti per vedere
036400030805      * le eventuali decodifiche
036500030805if  1c                   if        *in99
036600030805     c                   movea     *in           Win
036700030805     c                   movea     *zeros        *in(50)
036800030805     c                   write     TB30V1
036900030805     c                   movea     Win           *in
037000030805e   1c                   endif
037100030805      *
037200030805     c                   exfmt     TB30V1
037300030805     c                   eval      *in99 = *off
037400030805     c                   clear                   V1Dmsg
037500030805      * F3=Fine
037600030805if  1c                   if        *inKC
037700030805     c                   exsr      F03V1
037800030805     c                   goto      EndGesV1
037900030805e   1c                   endif
038000030805      *
038100030805      * Controllo dati immessi a video
038200030805     c                   exsr      CtrV1
038300030805     c     *in99         cabeq     *on           EndGesV1
038400090525     c     *in90         cabeq     *on           EndGesV1
038500030805      *
038600030805      * Passaggio alla videata di dettaglio
038700030805if  1c                   if        not *in99
038800030805     c                   eval      $CarV2 = *on
038900030805     c                   eval      $TipVid = '2'
039000030805e   1c                   endif
039100030805      *
039200030805     c     EndGesV1      ENDSR
039300030805
039400030805      *---------------------------------------------------------------*
039500030805      * CARV1  - Caricamento dati prima videata                       *
039600030805      *---------------------------------------------------------------*
039700030805     c     CarV1         BEGSR
039800030805      *
039900030805     c                   movea     *zeros        *in(50)
040000030805     c                   movea     '00000'       *in(01)
040100030805      *
040200030808      * Imposto il campo "numero serie" per l'interrogazione
040300030808      *   (ma lascio il cursore posizionato sul P.O.)
040400030808     c                   move (p)  '?'           V1Cnrs
040500130418
040600130418     c                   clear                   V01ksc
040700130418     c                   clear                   V01rag
040800030805      *
040900030805     c                   ENDSR
041000030805
041100030805      *---------------------------------------------------------------*
041200030805      * CTRV1  - Controllo e decodifica prima videata                 *
041300030805      *---------------------------------------------------------------*
041400030805     c     CtrV1         BEGSR
041500030805      *
041600030805     c                   movea     *zeros        *in(50)
041700030805     c                   clear                   V1Dmsg
041800030808     c                   clear                   xx
041900030808     c                   clear                   $Fld
042000030808      *
042100030808      * Controlli / Ricerche
042200130418
042300130418      * Almeno 1 tra filiale/serie e cliente deve essere inserito
042400130418     c                   IF        V1Cpoe = *zeros and
042500130418     c                             (V1Cnrs = *blanks or V1Cnrs = *zeros)
042600130418     c                             and V01ksc = *zeros
042700130418     c                   eval      *in50 = *on
042800130418     c                   eval      *in99 = *on
042900130418     c                   eval      V1Dmsg = $Msg(14)
043000130418     c                   leavesr
043100130418     c                   ENDIF
043200130418      * se inserito cliente no ricerca su Numero Serie e no Filiale Emissione ORM
043300130418     c                   IF        V01ksc > 0 and V1Cnrs <> *blanks and
043400130418     c                             V1Cpoe > 0
043500130418     c                   eval      *in53 = *on
043600130418     c                   eval      *in99 = *on
043700130418     c                   eval      V1Dmsg = $Msg(08)
043800130418     c                   leavesr
043900130418     c                   ENDIF
044000130418
044100030806      *
044200030808      * Ricerca:
044300030806      * - Numero Serie
044400030806     c     '?'           scan      V1Cnrs                                 20
044500030808if  1c                   if        *in20   or   *inKJ
044600030806     c                   clear                   V1Cnrs
044700030806     c                   add       1             XX
044800030808     c                   eval      $Fld(xx)  = 'V1CNRS    '
044900030806e   1c                   endif
045000030806if  1c                   if        xx > *zeros
045100030806     c                   exsr      Search
045200130418     c                   IF        B30fxx = 'F12' or B30Fxx = 'F03'
045300130418     c                   eval      $carv1 = *on
045400130418     c                   eval      $tipvid = '1'
045500130418     c                   leavesr
045600130418     c                   ENDIF
045700030806e   1c                   endif
045800130418
045900130418     c                   IF        V01ksc = 0
046000091204      *
046100091204      * Controllo:
046200091204      * - P.O. emissione O.R.M. obbligatorio e obbligatoriamente valido
046300130418if  1c                   if        NOT  *inKJ
046400091204     c                   exsr      CtrPO
046500091204     c   99              goto      EndCtrV1
046600091204e   1c                   endif
046700030808      * Controllo:
046800030806      * - Numero serie
046900030808if  1c                   if           V1Cnrs = *blanks
047000030808     c                             or V1Cnrs = *zeros
047100030805     c                   seton                                        51  99
047200030808     c                   eval      V1Dmsg    = $Msg(03)
047300030805     c                   goto      EndCtrV1
047400030806x   1c                   else
047500120112     c     c_Digits      check     V1Cnrs                                 20
047600030808if  2c                   if           *in20
047700030808     c                             or V1Cnrs = *all'9'
047800130418     c                   seton                                        51  99
047900030806     c                   movel     $Msg(04)      V1Dmsg
048000030806     c                   goto      EndCtrV1
048100030806e   2c                   endif
048200030806e   1c                   endif
048300130418     c                   ENDIF
048400130418
048500130418      * Controllo cliente
048600130418     c                   IF        V01ksc > 0
048700130418     c                   eval      wksc = V01ksc
048800130418     c                   exsr      sr_tibs69
048900130418     c                   eval      V01rag = wrag
049000130418     c                   IF        *in99
049100130418     c                   eval      *in53 = *on
049200130418     c                   leavesr
049300130418     c                   ENDIF
049400130418     c                   add       1             XX
049500130418     c                   eval      $Fld(xx)  = 'V01KSC    '
049600130418     c                   exsr      Search
049700130418     c                   IF        B30fxx = 'F12' or B30Fxx = 'F03'
049800130418     c                   eval      $carv1 = *on
049900130418     c                   eval      $tipvid = '1'
050000130418     c                   leavesr
050100130418     c                   ENDIF
050200130418     c                   ENDIF
050300030805      *
050400030805     c     EndCtrV1      ENDSR
050500030805
050600030805      *---------------------------------------------------------------*
050700030805      * F03V1  - Tasto funzionale F03 -> Fine programma               *
050800030805      *---------------------------------------------------------------*
050900030805     c     F03V1         BEGSR
051000030805      *
051100030805     c                   movel     *on           $Fine                          fine pgm
051200030805      *
051300030805     c                   ENDSR
051400030805
051500030805      *---------------------------------------------------------------*
051600030805      * CTRPO  - Controllo Punto Operativo                            *
051700030805      *---------------------------------------------------------------*
051800030805     c     CtrPO         BEGSR
051900030805      *
052000030805     c                   clear                   V1Dpoe
052100030805      *
052200040303     c                   If        V1cPoe > *Zeros
052300030805     c     V1Cpoe        chain     AZORG01L
052400030805if  1c                   if        not %found(AZORG01L)
052500030805     c                             or  ORGfva =  '*'
052600030805     c                   seton                                        50  99
052700091204     c                   eval      V1Dmsg = $Msg(02)
052800030805     c                   goto      EndCtrPO
052900030805x   1c                   else
053000030805     c                   movel     ORGdes        V1Dpoe
053100030805e   1c                   endif
053200091204     c                   Else
053300091204     c                   seton                                        50  99
053400091204     c                   eval      V1Dmsg = $Msg(01)
053500091204     c                   goto      EndCtrPO
053600040303     c                   EndIf
053700030805      *
053800030805     c     EndCtrPO      ENDSR
053900030805
054000030805      *---------------------------------------------------------------*
054100030805      * SEARCH - Ricerche                                             *
054200030805      *---------------------------------------------------------------*
054300030805     c     Search        BEGSR
054400030805      *
054500090525     c                   eval      *in90    =  *on
054600030805      *
054700030805do  1c                   DO        10            xx
054800030805      *
054900030805if  2c                   if        $Fld(xx) =  *blanks
055000030805     c                   leave
055100030805e   2c                   endif
055200030805      *
055300030805sel 2c                   SELECT
055400030805      * Numero Serie
055500030805w   2c                   WHEN      $Fld(xx) =  'V1CNRS    '
055600030805     c                   clear                   TNTB301ds
055700030806if  3c                   if        *inKJ  = *off
055800030805     c                   eval      B30opz = '1'
055900030806x   3c                   else
056000030806     c                   eval      B30opz = '2'
056100030806e   3c                   endif
056200030805     c                   eval      B30ke1 = DSkeyOSR
056300030805     c                   eval      B30sif = KNSIF
056400030805     c                   movel(p)  TNTB301ds     KPJBU
056500030805     c                   call      'TNTB301R'
056600030805     c                   parm                    KPJBA
056700030805     c                   movel     KPJBU         TNTB301ds
056800030806if  3c                   if            B30fxx <> 'F03'
056900030805     c                             and B30fxx <> 'F12'
057000030806     c                             and B30err =  *blanks
057100090330if  4c                   if        B30ke1 <> *blanks
057200030805     c                   movel     B30ke1        DSkeyOSR
057300090330x   4c                   else
057400090330     c                   eval      V1Cnrs = '01'
057500090330e   4c                   endif
057600030806e   3c                   endif
057700130418
057800130418      * Codice cliente in prima videata
057900130418w   2c                   WHEN      $Fld(xx) =  'V01KSC    '
058000130418     c                   clear                   TNTB301ds
058100130418     c                   eval      B30opz = '3'
058200130418     c                   eval      B30ke1 = %editc(V01ksc:'X')
058300130418     c                   movel(p)  TNTB301ds     KPJBU
058400130418     c                   call      'TNTB301R'
058500130418     c                   parm                    KPJBA
058600130418     c                   movel     KPJBU         TNTB301ds
058700130418if  3c                   if            B30fxx <> 'F03'
058800130418     c                             and B30fxx <> 'F12'
058900130418     c                             and B30err =  *blanks
059000130418if  4c                             and B30ke1 <> *blanks
059100130418     c                   movel     B30ke1        DSkeyOSR
059200130418     c                   clear                   V01ksc
059300130418     c                   clear                   V01rag
059400130418e   4c                   endif
059500030805      *
059600030805      * Codice cliente
059700030805w   2c                   WHEN      $Fld(xx) =  'V2CCLI    '
059800030805     c                   movel(p)  V2Drag        PAXdmt
059900030805     c                   call      'XALFA3BR'
060000030805     c                   parm      RSut          PAXdut           30
060100030805     c                   parm                    CODut
060200030805     c                   parm                    PAXdmt           48
060300030805     c                   parm      DUTkci        PAXccc            4 0
060400030805     c                   parm      9             PAXsta            1 0
060500030805     c                   parm      *blanks       PAXflr           90
060600030805     c                   parm      *blanks       PAXdit            3
060700030805     c                   parm      01            PAXnum            2 0
060800030805     c                   parm                    PAXkcm           80
060900030805     c                   parm                    PAXksm          140
061000030805     c                   parm                    PAXldm           60
061100030805if  3c                   if        PAXsta >= *zeros
061200030805     c                   movel     PAXksm        V2Ccli
061300030805e   3c                   endif
061400040303      *
061500040303      * Codice cliente 2
061600040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI2   '
061700040303     c                   movel(p)  V2Drag2       PAXdmt
061800040303     c                   call      'XALFA3BR'
061900040303     c                   parm      RSut          PAXdut           30
062000040303     c                   parm                    CODut
062100040303     c                   parm                    PAXdmt           48
062200040303     c                   parm      DUTkci        PAXccc            4 0
062300040303     c                   parm      9             PAXsta            1 0
062400040303     c                   parm      *blanks       PAXflr           90
062500040303     c                   parm      *blanks       PAXdit            3
062600040303     c                   parm      01            PAXnum            2 0
062700040303     c                   parm                    PAXkcm           80
062800040303     c                   parm                    PAXksm          140
062900040303     c                   parm                    PAXldm           60
063000040303if  3c                   if        PAXsta >= *zeros
063100040303     c                   movel     PAXksm        V2Ccli2
063200040303e   3c                   endif
063300040303      *
063400040303      * Codice cliente 3
063500040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI3   '
063600040303     c                   movel(p)  V2Drag3       PAXdmt
063700040303     c                   call      'XALFA3BR'
063800040303     c                   parm      RSut          PAXdut           30
063900040303     c                   parm                    CODut
064000040303     c                   parm                    PAXdmt           48
064100040303     c                   parm      DUTkci        PAXccc            4 0
064200040303     c                   parm      9             PAXsta            1 0
064300040303     c                   parm      *blanks       PAXflr           90
064400040303     c                   parm      *blanks       PAXdit            3
064500040303     c                   parm      01            PAXnum            2 0
064600040303     c                   parm                    PAXkcm           80
064700040303     c                   parm                    PAXksm          140
064800040303     c                   parm                    PAXldm           60
064900040303if  3c                   if        PAXsta >= *zeros
065000040303     c                   movel     PAXksm        V2Ccli3
065100040303e   3c                   endif
065200040303      *
065300040303      * Codice cliente 4
065400040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI4   '
065500040303     c                   movel(p)  V2Drag4       PAXdmt
065600040303     c                   call      'XALFA3BR'
065700040303     c                   parm      RSut          PAXdut           30
065800040303     c                   parm                    CODut
065900040303     c                   parm                    PAXdmt           48
066000040303     c                   parm      DUTkci        PAXccc            4 0
066100040303     c                   parm      9             PAXsta            1 0
066200040303     c                   parm      *blanks       PAXflr           90
066300040303     c                   parm      *blanks       PAXdit            3
066400040303     c                   parm      01            PAXnum            2 0
066500040303     c                   parm                    PAXkcm           80
066600040303     c                   parm                    PAXksm          140
066700040303     c                   parm                    PAXldm           60
066800040303if  3c                   if        PAXsta >= *zeros
066900040303     c                   movel     PAXksm        V2Ccli4
067000040303e   3c                   endif
067100040303      *
067200040303      * Codice cliente 5
067300040303w   2c                   WHEN      $Fld(xx) =  'V2CCLI5   '
067400040303     c                   movel(p)  V2Drag5       PAXdmt
067500040303     c                   call      'XALFA3BR'
067600040303     c                   parm      RSut          PAXdut           30
067700040303     c                   parm                    CODut
067800040303     c                   parm                    PAXdmt           48
067900040303     c                   parm      DUTkci        PAXccc            4 0
068000040303     c                   parm      9             PAXsta            1 0
068100040303     c                   parm      *blanks       PAXflr           90
068200040303     c                   parm      *blanks       PAXdit            3
068300040303     c                   parm      01            PAXnum            2 0
068400040303     c                   parm                    PAXkcm           80
068500040303     c                   parm                    PAXksm          140
068600040303     c                   parm                    PAXldm           60
068700040303if  3c                   if        PAXsta >= *zeros
068800040303     c                   movel     PAXksm        V2Ccli5
068900040303e   3c                   endif
069000080502      * Codice cliente 6
069100080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI6   '
069200080502     c                   movel(p)  V2Drag6       PAXdmt
069300080502     c                   call      'XALFA3BR'
069400080502     c                   parm      RSut          PAXdut           30
069500080502     c                   parm                    CODut
069600080502     c                   parm                    PAXdmt           48
069700080502     c                   parm      DUTkci        PAXccc            4 0
069800080502     c                   parm      9             PAXsta            1 0
069900080502     c                   parm      *blanks       PAXflr           90
070000080502     c                   parm      *blanks       PAXdit            3
070100080502     c                   parm      01            PAXnum            2 0
070200080502     c                   parm                    PAXkcm           80
070300080502     c                   parm                    PAXksm          140
070400080502     c                   parm                    PAXldm           60
070500080502if  3c                   if        PAXsta >= *zeros
070600080502     c                   movel     PAXksm        V2Ccli6
070700080502e   3c                   endif
070800080502      * Codice cliente 7
070900080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI7   '
071000080502     c                   movel(p)  V2Drag7       PAXdmt
071100080502     c                   call      'XALFA3BR'
071200080502     c                   parm      RSut          PAXdut           30
071300080502     c                   parm                    CODut
071400080502     c                   parm                    PAXdmt           48
071500080502     c                   parm      DUTkci        PAXccc            4 0
071600080502     c                   parm      9             PAXsta            1 0
071700080502     c                   parm      *blanks       PAXflr           90
071800080502     c                   parm      *blanks       PAXdit            3
071900080502     c                   parm      01            PAXnum            2 0
072000080502     c                   parm                    PAXkcm           80
072100080502     c                   parm                    PAXksm          140
072200080502     c                   parm                    PAXldm           60
072300080502if  3c                   if        PAXsta >= *zeros
072400080502     c                   movel     PAXksm        V2Ccli7
072500080502e   3c                   endif
072600080502      * Codice cliente 8
072700080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI8   '
072800080502     c                   movel(p)  V2Drag8       PAXdmt
072900080502     c                   call      'XALFA3BR'
073000080502     c                   parm      RSut          PAXdut           30
073100080502     c                   parm                    CODut
073200080502     c                   parm                    PAXdmt           48
073300080502     c                   parm      DUTkci        PAXccc            4 0
073400080502     c                   parm      9             PAXsta            1 0
073500080502     c                   parm      *blanks       PAXflr           90
073600080502     c                   parm      *blanks       PAXdit            3
073700080502     c                   parm      01            PAXnum            2 0
073800080502     c                   parm                    PAXkcm           80
073900080502     c                   parm                    PAXksm          140
074000080502     c                   parm                    PAXldm           60
074100080502if  3c                   if        PAXsta >= *zeros
074200080502     c                   movel     PAXksm        V2Ccli8
074300080502e   3c                   endif
074400080502      * Codice cliente 9
074500080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI9   '
074600080502     c                   movel(p)  V2Drag9       PAXdmt
074700080502     c                   call      'XALFA3BR'
074800080502     c                   parm      RSut          PAXdut           30
074900080502     c                   parm                    CODut
075000080502     c                   parm                    PAXdmt           48
075100080502     c                   parm      DUTkci        PAXccc            4 0
075200080502     c                   parm      9             PAXsta            1 0
075300080502     c                   parm      *blanks       PAXflr           90
075400080502     c                   parm      *blanks       PAXdit            3
075500080502     c                   parm      01            PAXnum            2 0
075600080502     c                   parm                    PAXkcm           80
075700080502     c                   parm                    PAXksm          140
075800080502     c                   parm                    PAXldm           60
075900080502if  3c                   if        PAXsta >= *zeros
076000080502     c                   movel     PAXksm        V2Ccli9
076100080502e   3c                   endif
076200080502      * Codice cliente 10
076300080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI10  '
076400080502     c                   movel(p)  V2Drag10      PAXdmt
076500080502     c                   call      'XALFA3BR'
076600080502     c                   parm      RSut          PAXdut           30
076700080502     c                   parm                    CODut
076800080502     c                   parm                    PAXdmt           48
076900080502     c                   parm      DUTkci        PAXccc            4 0
077000080502     c                   parm      9             PAXsta            1 0
077100080502     c                   parm      *blanks       PAXflr           90
077200080502     c                   parm      *blanks       PAXdit            3
077300080502     c                   parm      01            PAXnum            2 0
077400080502     c                   parm                    PAXkcm           80
077500080502     c                   parm                    PAXksm          140
077600080502     c                   parm                    PAXldm           60
077700080502if  3c                   if        PAXsta >= *zeros
077800080502     c                   movel     PAXksm        V2Ccli10
077900080502e   3c                   endif
078000080502      * Codice cliente 11
078100080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI11  '
078200080502     c                   movel(p)  V2Drag11      PAXdmt
078300080502     c                   call      'XALFA3BR'
078400080502     c                   parm      RSut          PAXdut           30
078500080502     c                   parm                    CODut
078600080502     c                   parm                    PAXdmt           48
078700080502     c                   parm      DUTkci        PAXccc            4 0
078800080502     c                   parm      9             PAXsta            1 0
078900080502     c                   parm      *blanks       PAXflr           90
079000080502     c                   parm      *blanks       PAXdit            3
079100080502     c                   parm      01            PAXnum            2 0
079200080502     c                   parm                    PAXkcm           80
079300080502     c                   parm                    PAXksm          140
079400080502     c                   parm                    PAXldm           60
079500080502if  3c                   if        PAXsta >= *zeros
079600080502     c                   movel     PAXksm        V2Ccli11
079700080502e   3c                   endif
079800080502      * Codice cliente 12
079900080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI12  '
080000080502     c                   movel(p)  V2Drag12      PAXdmt
080100080502     c                   call      'XALFA3BR'
080200080502     c                   parm      RSut          PAXdut           30
080300080502     c                   parm                    CODut
080400080502     c                   parm                    PAXdmt           48
080500080502     c                   parm      DUTkci        PAXccc            4 0
080600080502     c                   parm      9             PAXsta            1 0
080700080502     c                   parm      *blanks       PAXflr           90
080800080502     c                   parm      *blanks       PAXdit            3
080900080502     c                   parm      01            PAXnum            2 0
081000080502     c                   parm                    PAXkcm           80
081100080502     c                   parm                    PAXksm          140
081200080502     c                   parm                    PAXldm           60
081300080502if  3c                   if        PAXsta >= *zeros
081400080502     c                   movel     PAXksm        V2Ccli12
081500080502e   3c                   endif
081600080502      * Codice cliente 13
081700080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI13  '
081800080502     c                   movel(p)  V2Drag13      PAXdmt
081900080502     c                   call      'XALFA3BR'
082000080502     c                   parm      RSut          PAXdut           30
082100080502     c                   parm                    CODut
082200080502     c                   parm                    PAXdmt           48
082300080502     c                   parm      DUTkci        PAXccc            4 0
082400080502     c                   parm      9             PAXsta            1 0
082500080502     c                   parm      *blanks       PAXflr           90
082600080502     c                   parm      *blanks       PAXdit            3
082700080502     c                   parm      01            PAXnum            2 0
082800080502     c                   parm                    PAXkcm           80
082900080502     c                   parm                    PAXksm          140
083000080502     c                   parm                    PAXldm           60
083100080502if  3c                   if        PAXsta >= *zeros
083200080502     c                   movel     PAXksm        V2Ccli13
083300080502e   3c                   endif
083400080502      * Codice cliente 14
083500080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI14  '
083600080502     c                   movel(p)  V2Drag14      PAXdmt
083700080502     c                   call      'XALFA3BR'
083800080502     c                   parm      RSut          PAXdut           30
083900080502     c                   parm                    CODut
084000080502     c                   parm                    PAXdmt           48
084100080502     c                   parm      DUTkci        PAXccc            4 0
084200080502     c                   parm      9             PAXsta            1 0
084300080502     c                   parm      *blanks       PAXflr           90
084400080502     c                   parm      *blanks       PAXdit            3
084500080502     c                   parm      01            PAXnum            2 0
084600080502     c                   parm                    PAXkcm           80
084700080502     c                   parm                    PAXksm          140
084800080502     c                   parm                    PAXldm           60
084900080502if  3c                   if        PAXsta >= *zeros
085000080502     c                   movel     PAXksm        V2Ccli14
085100080502e   3c                   endif
085200080502      * Codice cliente 15
085300080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI15  '
085400080502     c                   movel(p)  V2Drag15      PAXdmt
085500080502     c                   call      'XALFA3BR'
085600080502     c                   parm      RSut          PAXdut           30
085700080502     c                   parm                    CODut
085800080502     c                   parm                    PAXdmt           48
085900080502     c                   parm      DUTkci        PAXccc            4 0
086000080502     c                   parm      9             PAXsta            1 0
086100080502     c                   parm      *blanks       PAXflr           90
086200080502     c                   parm      *blanks       PAXdit            3
086300080502     c                   parm      01            PAXnum            2 0
086400080502     c                   parm                    PAXkcm           80
086500080502     c                   parm                    PAXksm          140
086600080502     c                   parm                    PAXldm           60
086700080502if  3c                   if        PAXsta >= *zeros
086800080502     c                   movel     PAXksm        V2Ccli15
086900080502e   3c                   endif
087000080502      * Codice cliente 16
087100080502w   2c                   WHEN      $Fld(xx) =  'V2CCLI16  '
087200080502     c                   movel(p)  V2Drag16      PAXdmt
087300080502     c                   call      'XALFA3BR'
087400080502     c                   parm      RSut          PAXdut           30
087500080502     c                   parm                    CODut
087600080502     c                   parm                    PAXdmt           48
087700080502     c                   parm      DUTkci        PAXccc            4 0
087800080502     c                   parm      9             PAXsta            1 0
087900080502     c                   parm      *blanks       PAXflr           90
088000080502     c                   parm      *blanks       PAXdit            3
088100080502     c                   parm      01            PAXnum            2 0
088200080502     c                   parm                    PAXkcm           80
088300080502     c                   parm                    PAXksm          140
088400080502     c                   parm                    PAXldm           60
088500080502if  3c                   if        PAXsta >= *zeros
088600080502     c                   movel     PAXksm        V2Ccli16
088700080502e   3c                   endif
088800030805      *
088900030805      * Codice bolla da forzare in bollettazione
089000030805w   2c                   WHEN      $Fld(xx) =  'V2CCBO    '
089100030805     c                   call      'X§TABER'
089200030805     c                   parm                    CODut
089300030807     c                   parm      '3A'          X§TABcod          2
089400030805     c                   parm      *blanks       X§TABkey          8
089500030805     c                   parm      *blanks       X§TABdes         30
089600030805     c                   movel     X§TABkey      V2Ccbo
089700030805     c                   movel     X§TABdes      V2Dcbo
089800030805      *
089900030805e   2c                   ENDSL
090000030805      *
090100030805e   1c                   ENDDO
090200030805      *
090300030805     c                   ENDSR
090400030805
090500030805      *---------------------------------------------------------------*
090600030805      * GESV2  - Gestione videata dettaglio dati                      *
090700030805      *---------------------------------------------------------------*
090800030805     c     GesV2         BEGSR
090900030805      *
091000030805      * Inizializzazione videata
091100030805     c                   if        $CarV2 = *on
091200030805     c                   exsr      CarV2
091300030805     c                   move      *off          $CarV2
091400030805     c                   endif
091500030805      * Scrivo la testata
091600090525     c                   write     TB30T1
091700091203     c                   write     TB30T2
091800091203     c                   write     TB30P2
091900030805     c                   exfmt     TB30V2
092000030805     c                   eval      *in99 = *off
092100030805     c                   clear                   V1Dmsg
092200030805     c                   clear                   wTasto
092300090525     c                   movea     *zeros        *in(50)
092400090525
092500030805      *
092600030805sel 1c                   select
092700030805      * F03=Fine
092800030805w   1c                   when      *inKC
092900030805     c                   exsr      F03V1
093000030805     c                   goto      EndGesV2
093100030805      * F12=Ritorno
093200030805w   1c                   when      *inKL
093300030805     c                   exsr      F12V2
093400030805     c                   goto      EndGesV2
093500030805e   1c                   endsl
093600030805      *
093700030805      * Controllo dati immessi a video
093800090525      * (non si fanno se rcd annullato)
093900090525if  1c                   if        not *in04
094000030805     c                   exsr      CtrV2
094100090525e   1c                   endif
094200090522
094300090522      * passo alla terza videata
094400090525if  1c                   if        not *in99 and not *in90
094500090525     c                   eval      $CarV3 = *on
094600090525     c                   eval      $TipVid = '3'
094700090525e   1c                   endif
094800030805      *
094900030805     c     EndGesV2      ENDSR
095000090525
095100090525      *---------------------------------------------------------------*
095200090525      * CARV2  - Caricamento dati seconda videata                     *
095300090525      *---------------------------------------------------------------*
095400090525     c     CarV2         BEGSR
095500090525      *
095600090525     c                   clear                   T1opz
095700090525     c                   movea     '00000'       *in(01)
095800091203      *
095900091203      /free
096000091203
096100091203         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
096200091203         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
096300091203           w_SisInf = 1;
096400091203           exsr  sr_OpenFileTab;
096500091203         endif;
096600091203
096700091203      /end-free
096800090525      *
096900090525      * Aggancio la tabella, se trovo il codice sono in modifica
097000090525      * o ripristino (se record annullato), altrimenti in immissione
097100090525     c                   exsr      ChnTBE
097200090525      *
097300090525if  1c                   if        %found(TNTBE01L)
097400090525      *
097500090525      * MODIFICA/RIPRISTINO
097600090525     c                   movel     TBEuni        dOSR
097700090525if  2c                   if        TBEatb = *blanks
097800090525     c                   eval      *in02  = *on
097900090525     c                   eval      T1opz  = $Opz(02)
098000090525x   2c                   else
098100090525     c                   eval      *in04  = *on
098200090525     c                   eval      T1opz  = $Opz(06)
098300090525e   2c                   endif
098400090525      *
098500090525x   1c                   else
098600090525      *
098700090525      * IMMISSIONE
098800090525     c                   clear                   dOSR
098900090525     c                   eval      *in01  = *on
099000090525     c                   eval      T1opz  = $Opz(01)
099100090525      *
099200090525e   1c                   endif
099300090525      *
099400090525     c                   eval      V2Cpoe = V1Cpoe
099500090525     c                   eval      V2Dpoe = V1Dpoe
099600090525     c                   move      V1Cnrs        V2Cnrs
099700090525     c                   move      D§OSRcli      V2Ccli
099800090525     c                   eval      V2Drag = D§OSRrag
099900090525     c                   move      D§OSRcl2      V2Ccli2
100000090525     c                   eval      V2Drag2 = D§OSRra2
100100090525     c                   move      D§OSRcl3      V2Ccli3
100200090525     c                   eval      V2Drag3 = D§OSRra3
100300090525     c                   move      D§OSRcl4      V2Ccli4
100400090610     c                   eval      V2Drag4 = D§OSRra4
100500091022     c* Verifica e forzatura x nuovi campi aggiunti
100600091022     c                   if        aD§OSRcl5 = *blanks
100700091022     c                   eval      D§OSRcl5 = *zeros
100800091022     c                   endif
100900091022     c                   if        aD§OSRcl6 = *blanks
101000091022     c                   eval      D§OSRcl6 = *zeros
101100091022     c                   endif
101200091022     c                   if        aD§OSRcl7 = *blanks
101300091022     c                   eval      D§OSRcl7 = *zeros
101400091022     c                   endif
101500091022     c                   if        aD§OSRcl8 = *blanks
101600091022     c                   eval      D§OSRcl8 = *zeros
101700091022     c                   endif
101800091022     c                   if        aD§OSRcl9 = *blanks
101900091022     c                   eval      D§OSRcl9 = *zeros
102000091022     c                   endif
102100091022     c                   if        aD§OSRcl10 = *blanks
102200091022     c                   eval      D§OSRcl10 = *zeros
102300091022     c                   endif
102400091022     c                   if        aD§OSRcl11 = *blanks
102500091022     c                   eval      D§OSRcl11 = *zeros
102600091022     c                   endif
102700091022     c                   if        aD§OSRcl12 = *blanks
102800091022     c                   eval      D§OSRcl12 = *zeros
102900091022     c                   endif
103000091022     c                   if        aD§OSRcl13 = *blanks
103100091022     c                   eval      D§OSRcl13 = *zeros
103200091022     c                   endif
103300091022     c                   if        aD§OSRcl14 = *blanks
103400091022     c                   eval      D§OSRcl14 = *zeros
103500091022     c                   endif
103600091022     c                   if        aD§OSRcl15 = *blanks
103700091022     c                   eval      D§OSRcl15 = *zeros
103800091022     c                   endif
103900091022     c                   if        aD§OSRcl16 = *blanks
104000091022     c                   eval      D§OSRcl16 = *zeros
104100091022     c                   endif
104200090525     c                   move      D§OSRcl5      V2Ccli5
104300090525     c                   move      D§OSRcl6      V2Ccli6
104400090525     c                   move      D§OSRcl7      V2Ccli7
104500090525     c                   move      D§OSRcl8      V2Ccli8
104600090525     c                   move      D§OSRcl9      V2Ccli9
104700090525     c                   move      D§OSRcl10     V2Ccli10
104800090525     c                   move      D§OSRcl11     V2Ccli11
104900090525     c                   move      D§OSRcl12     V2Ccli12
105000090525     c                   move      D§OSRcl13     V2Ccli13
105100090525     c                   move      D§OSRcl14     V2Ccli14
105200090525     c                   move      D§OSRcl15     V2Ccli15
105300090525     c                   move      D§OSRcl16     V2Ccli16
105400090525
105500090525      * decodifiche ragioni sociali
105600090525     c                   if        v2ccli5 > *zeros
105700090525     c                   eval      wksc = %dec(v2ccli5:7:0)
105800090525     c                   exsr      sr_tibs69
105900090525     c                   eval      v2drag5 = wrag
106000090525     c                   endif
106100090525     c                   if        v2ccli6 > *zeros
106200090525     c                   eval      wksc = %dec(v2ccli6:7:0)
106300090525     c                   exsr      sr_tibs69
106400090525     c                   eval      v2drag6 = wrag
106500090525     c                   endif
106600090525     c                   if        v2ccli7 > *zeros
106700090525     c                   eval      wksc = %dec(v2ccli7:7:0)
106800090525     c                   exsr      sr_tibs69
106900090525     c                   eval      v2drag7 = wrag
107000090525     c                   endif
107100090525     c                   if        v2ccli8 > *zeros
107200090525     c                   eval      wksc = %dec(v2ccli8:7:0)
107300090525     c                   exsr      sr_tibs69
107400090525     c                   eval      v2drag8 = wrag
107500090525     c                   endif
107600090525     c                   if        v2ccli9 > *zeros
107700090525     c                   eval      wksc = %dec(v2ccli9:7:0)
107800090525     c                   exsr      sr_tibs69
107900090525     c                   eval      v2drag9 = wrag
108000090525     c                   endif
108100090525     c                   if        v2ccli10 > *zeros
108200090525     c                   eval      wksc = %dec(v2ccli10:7:0)
108300090525     c                   exsr      sr_tibs69
108400090525     c                   eval      v2drag10 = wrag
108500090525     c                   endif
108600090525     c                   if        v2ccli11 > *zeros
108700090525     c                   eval      wksc = %dec(v2ccli11:7:0)
108800090525     c                   exsr      sr_tibs69
108900090525     c                   eval      v2drag11 = wrag
109000090525     c                   endif
109100090525     c                   if        v2ccli12 > *zeros
109200090525     c                   eval      wksc = %dec(v2ccli12:7:0)
109300090525     c                   exsr      sr_tibs69
109400090525     c                   eval      v2drag12 = wrag
109500090525     c                   endif
109600090525     c                   if        v2ccli13 > *zeros
109700090525     c                   eval      wksc = %dec(v2ccli13:7:0)
109800090525     c                   exsr      sr_tibs69
109900090525     c                   eval      v2drag13 = wrag
110000090525     c                   endif
110100090525     c                   if        v2ccli14 > *zeros
110200090525     c                   eval      wksc = %dec(v2ccli14:7:0)
110300090525     c                   exsr      sr_tibs69
110400090525     c                   eval      v2drag14 = wrag
110500090525     c                   endif
110600090525     c                   if        v2ccli15 > *zeros
110700090525     c                   eval      wksc = %dec(v2ccli15:7:0)
110800090525     c                   exsr      sr_tibs69
110900090525     c                   eval      v2drag15 = wrag
111000090525     c                   endif
111100090525     c                   if        v2ccli16 > *zeros
111200090525     c                   eval      wksc = %dec(v2ccli16:7:0)
111300090525     c                   exsr      sr_tibs69
111400090525     c                   eval      v2drag16 = wrag
111500090525     c                   endif
111600090525
111700090525      * --> nuovi campi per creazione ORM RC
111800090525      * traduttore personalizzato
111900091124     c                   if        d§osrvas = *blanks
112000091124     c                   eval      v2fvas = 'N'
112100091124     c                   else
112200090525     c                   eval      v2fvas = d§osrvas
112300091124     c                   endif
112400120307     c   01              eval      v2fvas = 'N'
112500090525      * codice ordinante
112600090525     c                   eval      v2ccor = d§osrcor
112700090525      * decodifico
112800090525     c                   if        v2ccor <> 0
112900090525     c                   eval      wksc = v2ccor
113000090525     c                   exsr      sr_tibs69
113100090525     c                   eval      v2dcor = wrag
113200090525     c                   endif
113300090525      * codice destinatario
113400090525     c                   eval      v2ccrc = d§osrcrc
113500090525      * decodifico
113600090525     c                   if        v2ccrc <> 0
113700090525     c                   eval      wksc = v2ccrc
113800090525     c                   exsr      sr_tibs69
113900090525     c                   eval      v2dcrc = wrag
114000090525     c                   endif
114100090525      * chi paga
114200090525     c                   eval      v2cpag = d§osrpag
114300120302      * se immissione imposto il dft = 'O'
114400090525     c                   if        *in01
114500120302     c                   eval      v2cpag = 'O'
114600090525     c                   endif
114700090525      * colli/peso
114800090623     c                   eval      v2cnclpkg = d§osrfcp
114900090610      * se immissione imposto il flag di dft
115000090525     c                   if        *in01
115100090623     c                   eval      v2cnclpkg = '1'
115200090525     c                   endif
115300090525      * filiale consegna
115400090525     c                   eval      v2cpoc = d§osrpoc
115500090525      * decodifico
115600090525     c                   clear                   v2dpoc
115700090525     c     v2cpoc        chain     azorg01l
115800090525     c                   if        %found(azorg01l)
115900090525     c                   eval      v2dpoc = orgdes
116000090525     c                   endif
116100090525      *
116200090525     c                   ENDSR
116300030805
116400030805      *---------------------------------------------------------------*
116500030805      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
116600030805      *---------------------------------------------------------------*
116700030805     c     F12V2         BEGSR
116800030805      *
116900091203      /free
117000091203         unlock  TNTBE01L;
117100091203      /end-free
117200030805     c                   eval      $TipVid = '1'
117300030805      *
117400030805     c                   ENDSR
117500030805
117600030805      *---------------------------------------------------------------*
117700030805      * CTRV2  - Controllo e decodifica seconda videata               *
117800030805      *---------------------------------------------------------------*
117900030805     c     CtrV2         BEGSR
118000030805      *
118100030805     c                   movea     *zeros        *in(50)
118200030805     c                   clear                   V1Dmsg
118300030805      *
118400030805      * Ricerche
118500030805     c                   clear                   xx
118600030805     c                   clear                   $Fld
118700030805      * - Codice cliente
118800030805     c     '?'           scan      V2Ccli                                 20
118900030805if  1c                   if        *in20
119000030805     c                   add       1             XX
119100030805     c                   eval      $Fld(xx) = 'V2CCLI    '
119200030805e   1c                   endif
119300040303      * - Codice cliente 2
119400040303     c     '?'           scan      V2Ccli2                                20
119500040303if  1c                   if        *in20
119600040303     c                   add       1             XX
119700040303     c                   eval      $Fld(xx) = 'V2CCLI2   '
119800040303e   1c                   endif
119900040303      * - Codice cliente 3
120000040303     c     '?'           scan      V2Ccli3                                20
120100040303if  1c                   if        *in20
120200040303     c                   add       1             XX
120300040303     c                   eval      $Fld(xx) = 'V2CCLI3   '
120400040303e   1c                   endif
120500040303      * - Codice cliente 4
120600040303     c     '?'           scan      V2Ccli4                                20
120700040303if  1c                   if        *in20
120800040303     c                   add       1             XX
120900040303     c                   eval      $Fld(xx) = 'V2CCLI4   '
121000040303e   1c                   endif
121100040303      * - Codice cliente 5
121200040303     c     '?'           scan      V2Ccli5                                20
121300040303if  1c                   if        *in20
121400040303     c                   add       1             XX
121500040303     c                   eval      $Fld(xx) = 'V2CCLI5   '
121600040303e   1c                   endif
121700080502      * - Codice cliente 6
121800080502     c     '?'           scan      V2Ccli6                                20
121900080502if  1c                   if        *in20
122000080502     c                   add       1             XX
122100080502     c                   eval      $Fld(xx) = 'V2CCLI6   '
122200080502e   1c                   endif
122300080502      * - Codice cliente 7
122400080502     c     '?'           scan      V2Ccli7                                20
122500080502if  1c                   if        *in20
122600080502     c                   add       1             XX
122700080502     c                   eval      $Fld(xx) = 'V2CCLI7   '
122800080502e   1c                   endif
122900080502      * - Codice cliente 8
123000080502     c     '?'           scan      V2Ccli8                                20
123100080502if  1c                   if        *in20
123200080502     c                   add       1             XX
123300080502     c                   eval      $Fld(xx) = 'V2CCLI8   '
123400080502e   1c                   endif
123500080502      * - Codice cliente 9
123600080502     c     '?'           scan      V2Ccli9                                20
123700080502if  1c                   if        *in20
123800080502     c                   add       1             XX
123900080502     c                   eval      $Fld(xx) = 'V2CCLI9   '
124000080502e   1c                   endif
124100080502      * - Codice cliente 10
124200080502     c     '?'           scan      V2Ccli10                               20
124300080502if  1c                   if        *in20
124400080502     c                   add       1             XX
124500080502     c                   eval      $Fld(xx) = 'V2CCLI10  '
124600080502e   1c                   endif
124700080502      * - Codice cliente 11
124800080502     c     '?'           scan      V2Ccli11                               20
124900080502if  1c                   if        *in20
125000080502     c                   add       1             XX
125100080502     c                   eval      $Fld(xx) = 'V2CCLI11  '
125200080502e   1c                   endif
125300080502      * - Codice cliente 12
125400080502     c     '?'           scan      V2Ccli12                               20
125500080502if  1c                   if        *in20
125600080502     c                   add       1             XX
125700080502     c                   eval      $Fld(xx) = 'V2CCLI12  '
125800080502e   1c                   endif
125900080502      * - Codice cliente 13
126000080502     c     '?'           scan      V2Ccli13                               20
126100080502if  1c                   if        *in20
126200080502     c                   add       1             XX
126300080502     c                   eval      $Fld(xx) = 'V2CCLI13  '
126400080502e   1c                   endif
126500080502      * - Codice cliente 14
126600080502     c     '?'           scan      V2Ccli14                               20
126700080502if  1c                   if        *in20
126800080502     c                   add       1             XX
126900080502     c                   eval      $Fld(xx) = 'V2CCLI14  '
127000080502e   1c                   endif
127100080502      * - Codice cliente 15
127200080502     c     '?'           scan      V2Ccli15                               20
127300080502if  1c                   if        *in20
127400080502     c                   add       1             XX
127500080502     c                   eval      $Fld(xx) = 'V2CCLI15  '
127600080502e   1c                   endif
127700080502      * - Codice cliente 16
127800080502     c     '?'           scan      V2Ccli16                               20
127900080502if  1c                   if        *in20
128000080502     c                   add       1             XX
128100080502     c                   eval      $Fld(xx) = 'V2CCLI16  '
128200080502e   1c                   endif
128300030805      * - Codice bolla da forzare
128400030805     c     '?'           scan      V2Ccbo                                 20
128500030805if  1c                   if        *in20
128600030805     c                   add       1             XX
128700030805     c                   eval      $Fld(xx) = 'V2CCBO    '
128800030805e   1c                   endif
128900030805      *
129000030805if  1c                   if        xx > *zeros
129100030805     c                   exsr      Search
129200030805e   1c                   endif
129300030805      *
129400030805      * Controlli
129500030805      *
129600030805      * - Codice cliente obbligatorio e obbligatoriamente valido
129700030805     c                   clear                   V2Drag
129800030805if  1c                   IF           V2Ccli = *blanks
129900030805     c                             or V2Ccli = *zeros
130000030805     c                   seton                                        52  99
130100030806     c                   movel     $Msg(05)      V1Dmsg
130200030805     c                   goto      EndCtrV2
130300030805x   1c                   ELSE
130400120112     c     c_Digits      check     V2Ccli                                 20
130500030806if  2c                   if        *in20
130600030806     c                   seton                                        52  99
130700030806     c                   movel     $Msg(06)      V1Dmsg
130800030806     c                   goto      EndCtrV2
130900030806e   2c                   endif
131000090525     c                   eval      wksc = %dec(V2Ccli:7:0)
131100090522     c                   exsr      sr_tibs69
131200090522if  2c                   if        *in99
131300090522     c                   seton                                        52
131400030805     c                   goto      EndCtrV2
131500030805e   2c                   endif
131600040303      * Decodifica cliente
131700090522     c                   movel     wrag          V2Drag
131800130121      * Controllo se il cliente è già presente in tabella.
131900130121     c                   exsr      ctrOSR
132000130121     c                   IF        *in99
132100130121     c                   eval      *in52 = *on
132200130121     c                   leavesr
132300130121     c                   ENDIF
132400120307      * se non ha traduttore
132500120302      * controllo se il cliente esite su FNACR come 333 e 444
132600120307     c                   IF        v2fvas = 'N'
132700120302     c                   eval      kacr = %dec(v2ccli:10:0) * 1000
132800120302     c                   exsr      CtrACR
132900120302if  2c                   IF        *in99
133000120302     c                   eval      *in52 = *on
133100120302     c                   leavesr
133200120302     c                   ENDIF
133300120307     c                   ENDIF
133400030805e   1c                   ENDIF
133500040303
133600040303      * - Codice cliente 2 valido
133700040303     c                   clear                   V2Drag2
133800040303if  1c                   IF            V2Ccli2 <> *blanks
133900040303     c                             and V2Ccli2 <> *zeros
134000120112     c     c_Digits      check     V2Ccli2                                20
134100040303if  2c                   if        *in20
134200040303     c                   seton                                        59  99
134300040303     c                   movel     $Msg(06)      V1Dmsg
134400040303     c                   goto      EndCtrV2
134500040303e   2c                   endif
134600090525     c                   eval      wksc = %dec(V2Ccli2:7:0)
134700090522     c                   exsr      sr_tibs69
134800090522if  2c                   if        *in99
134900090522     c                   seton                                        59
135000040303     c                   goto      EndCtrV2
135100040303e   2c                   endif
135200090522     c                   movel     wrag          V2Drag2
135300130121      * Controllo se il cliente è già presente in tabella.
135400130121     c                   exsr      ctrOSR
135500130121     c                   IF        *in99
135600130121     c                   eval      *in59 = *on
135700130121     c                   leavesr
135800130121     c                   ENDIF
135900120302      * controllo se il cliente esite su FNACR come 333 e 444
136000120307     c                   IF        v2fvas = 'N'
136100120302     c                   eval      kacr = %dec(v2ccli2:10:0) * 1000
136200120302     c                   exsr      CtrACR
136300120302if  2c                   IF        *in99
136400120302     c                   eval      *in59 = *on
136500120302     c                   leavesr
136600120307     c                   ENDIF
136700120302     c                   ENDIF
136800040303e   1c                   ENDIF
136900040303
137000040303      * - Codice cliente 3 valido
137100040303     c                   clear                   V2Drag3
137200040303if  1c                   IF            V2Ccli3 <> *blanks
137300040303     c                             and V2Ccli3 <> *zeros
137400120112     c     c_Digits      check     V2Ccli3                                20
137500040303if  2c                   if        *in20
137600040303     c                   seton                                        60  99
137700040303     c                   movel     $Msg(06)      V1Dmsg
137800040303     c                   goto      EndCtrV2
137900040303e   2c                   endif
138000090525     c                   eval      wksc = %dec(V2Ccli3:7:0)
138100090522     c                   exsr      sr_tibs69
138200090522if  2c                   if        *in99
138300090522     c                   seton                                        60
138400040303     c                   goto      EndCtrV2
138500040303e   2c                   endif
138600090522     c                   movel     wrag          V2Drag3
138700130121      * Controllo se il cliente è già presente in tabella.
138800130121     c                   exsr      ctrOSR
138900130121     c                   IF        *in99
139000130121     c                   eval      *in60 = *on
139100130121     c                   leavesr
139200130121     c                   ENDIF
139300120302      * controllo se il cliente esite su FNACR come 333 e 444
139400120307     c                   IF        v2fvas = 'N'
139500120302     c                   eval      kacr = %dec(v2ccli3:10:0) * 1000
139600120302     c                   exsr      CtrACR
139700120302if  2c                   IF        *in99
139800120302     c                   eval      *in60 = *on
139900120302     c                   leavesr
140000120302     c                   ENDIF
140100120307     c                   ENDIF
140200040303e   1c                   ENDIF
140300040303
140400040303      * - Codice cliente 4 valido
140500040303     c                   clear                   V2Drag4
140600040303if  1c                   IF            V2Ccli4 <> *blanks
140700040303     c                             and V2Ccli4 <> *zeros
140800120112     c     c_Digits      check     V2Ccli4                                20
140900040303if  2c                   if        *in20
141000040303     c                   seton                                        61  99
141100040303     c                   movel     $Msg(06)      V1Dmsg
141200040303     c                   goto      EndCtrV2
141300040303e   2c                   endif
141400090525     c                   eval      wksc = %dec(V2Ccli4:7:0)
141500090525     c                   exsr      sr_tibs69
141600090522if  2c                   if        *in99
141700090522     c                   seton                                        61
141800040303     c                   goto      EndCtrV2
141900040303e   2c                   endif
142000090522     c                   movel     wrag          V2Drag4
142100130121      * Controllo se il cliente è già presente in tabella.
142200130121     c                   exsr      ctrOSR
142300130121     c                   IF        *in99
142400130121     c                   eval      *in61 = *on
142500130121     c                   leavesr
142600130121     c                   ENDIF
142700120302      * controllo se il cliente esite su FNACR come 333 e 444
142800120307     c                   IF        v2fvas = 'N'
142900120302     c                   eval      kacr = %dec(v2ccli4:10:0) * 1000
143000120302     c                   exsr      CtrACR
143100120302if  2c                   IF        *in99
143200120302     c                   eval      *in61 = *on
143300120302     c                   leavesr
143400120302     c                   ENDIF
143500120307     c                   ENDIF
143600040303e   1c                   ENDIF
143700040303
143800040303      * - Codice cliente 5 valido
143900040303     c                   clear                   V2Drag5
144000040303if  1c                   IF            V2Ccli5 <> *blanks
144100040303     c                             and V2Ccli5 <> *zeros
144200120112     c     c_Digits      check     V2Ccli5                                20
144300040303if  2c                   if        *in20
144400040303     c                   seton                                        62  99
144500040303     c                   movel     $Msg(06)      V1Dmsg
144600040303     c                   goto      EndCtrV2
144700040303e   2c                   endif
144800090525     c                   eval      wksc = %dec(V2Ccli5:7:0)
144900090522     c                   exsr      sr_tibs69
145000090522if  2c                   if        *in99
145100090522     c                   seton                                        62
145200040303     c                   goto      EndCtrV2
145300040303e   2c                   endif
145400090522     c                   movel     wrag          V2Drag5
145500130121      * Controllo se il cliente è già presente in tabella.
145600130121     c                   exsr      ctrOSR
145700130121     c                   IF        *in99
145800130121     c                   eval      *in62 = *on
145900130121     c                   leavesr
146000130121     c                   ENDIF
146100120302      * controllo se il cliente esite su FNACR come 333 e 444
146200120307     c                   IF        v2fvas = 'N'
146300120302     c                   eval      kacr = %dec(v2ccli5:10:0) * 1000
146400120302     c                   exsr      CtrACR
146500120302if  2c                   IF        *in99
146600120302     c                   eval      *in62 = *on
146700120302     c                   leavesr
146800120302     c                   ENDIF
146900120307     c                   ENDIF
147000040303e   1c                   ENDIF
147100080502
147200080502      * - Codice cliente 6 valido
147300080502     c                   clear                   V2Drag6
147400080502if  1c                   IF            V2Ccli6 <> *blanks
147500080502     c                             and V2Ccli6 <> *zeros
147600120112     c     c_Digits      check     V2Ccli6                                20
147700080502if  2c                   if        *in20
147800090522     c                   seton                                        63  99
147900080502     c                   movel     $Msg(06)      V1Dmsg
148000080502     c                   goto      EndCtrV2
148100080502e   2c                   endif
148200090525     c                   eval      wksc = %dec(V2Ccli6:7:0)
148300090522     c                   exsr      sr_tibs69
148400090522if  2c                   if        *in99
148500090522     c                   seton                                        63
148600080502     c                   goto      EndCtrV2
148700080502e   2c                   endif
148800090522     c                   movel     wrag          V2Drag6
148900130121      * Controllo se il cliente è già presente in tabella.
149000130121     c                   exsr      ctrOSR
149100130121     c                   IF        *in99
149200130121     c                   eval      *in63 = *on
149300130121     c                   leavesr
149400130121     c                   ENDIF
149500120302      * controllo se il cliente esite su FNACR come 333 e 444
149600120307     c                   IF        v2fvas = 'N'
149700120302     c                   eval      kacr = %dec(v2ccli6:10:0) * 1000
149800120302     c                   exsr      CtrACR
149900120302if  2c                   IF        *in99
150000120302     c                   eval      *in63 = *on
150100120302     c                   leavesr
150200120302     c                   ENDIF
150300120307     c                   ENDIF
150400080502e   1c                   ENDIF
150500080502
150600080502      * - Codice cliente 7 valido
150700080502     c                   clear                   V2Drag7
150800080502if  1c                   IF            V2Ccli7 <> *blanks
150900080502     c                             and V2Ccli7 <> *zeros
151000120112     c     c_Digits      check     V2Ccli7                                20
151100080502if  2c                   if        *in20
151200090522     c                   seton                                        64  99
151300080502     c                   movel     $Msg(06)      V1Dmsg
151400080502     c                   goto      EndCtrV2
151500080502e   2c                   endif
151600090525     c                   eval      wksc = %dec(V2Ccli7:7:0)
151700090522     c                   exsr      sr_tibs69
151800090522if  2c                   if        *in99
151900090522     c                   seton                                        64
152000080502     c                   goto      EndCtrV2
152100080502e   2c                   endif
152200090522     c                   movel     wrag          V2Drag7
152300130121      * Controllo se il cliente è già presente in tabella.
152400130121     c                   exsr      ctrOSR
152500130121     c                   IF        *in99
152600130121     c                   eval      *in64 = *on
152700130121     c                   leavesr
152800130121     c                   ENDIF
152900120302      * controllo se il cliente esite su FNACR come 333 e 444
153000120307     c                   IF        v2fvas = 'N'
153100120302     c                   eval      kacr = %dec(v2ccli7:10:0) * 1000
153200120302     c                   exsr      CtrACR
153300120302if  2c                   IF        *in99
153400120302     c                   eval      *in64 = *on
153500120302     c                   leavesr
153600120302     c                   ENDIF
153700120307     c                   ENDIF
153800080502e   1c                   ENDIF
153900080502
154000080502      * - Codice cliente 8 valido
154100080502     c                   clear                   V2Drag8
154200080502if  1c                   IF            V2Ccli8 <> *blanks
154300080502     c                             and V2Ccli8 <> *zeros
154400120112     c     c_Digits      check     V2Ccli8                                20
154500080502if  2c                   if        *in20
154600090522     c                   seton                                        65  99
154700080502     c                   movel     $Msg(06)      V1Dmsg
154800080502     c                   goto      EndCtrV2
154900080502e   2c                   endif
155000090525     c                   eval      wksc = %dec(V2Ccli8:7:0)
155100090522     c                   exsr      sr_tibs69
155200090522if  2c                   if        *in99
155300090522     c                   seton                                        65
155400080502     c                   goto      EndCtrV2
155500080502e   2c                   endif
155600090522     c                   movel     wrag          V2Drag8
155700130121      * Controllo se il cliente è già presente in tabella.
155800130121     c                   exsr      ctrOSR
155900130121     c                   IF        *in99
156000130121     c                   eval      *in65 = *on
156100130121     c                   leavesr
156200130121     c                   ENDIF
156300120302      * controllo se il cliente esite su FNACR come 333 e 444
156400120307     c                   IF        v2fvas = 'N'
156500120302     c                   eval      kacr = %dec(v2ccli8:10:0) * 1000
156600120302     c                   exsr      CtrACR
156700120302if  2c                   IF        *in99
156800120302     c                   eval      *in65 = *on
156900120302     c                   leavesr
157000120302     c                   ENDIF
157100120307     c                   ENDIF
157200080502e   1c                   ENDIF
157300080502
157400080502      * - Codice cliente 9 valido
157500080502     c                   clear                   V2Drag9
157600080502if  1c                   IF            V2Ccli9 <> *blanks
157700080502     c                             and V2Ccli9 <> *zeros
157800120112     c     c_Digits      check     V2Ccli9                                20
157900080502if  2c                   if        *in20
158000090522     c                   seton                                        66  99
158100080502     c                   movel     $Msg(06)      V1Dmsg
158200080502     c                   goto      EndCtrV2
158300080502e   2c                   endif
158400090525     c                   eval      wksc = %dec(V2Ccli9:7:0)
158500090522     c                   exsr      sr_tibs69
158600090522if  2c                   if        *in99
158700090525     c                   seton                                        66
158800080502     c                   goto      EndCtrV2
158900080502e   2c                   endif
159000090522     c                   movel     wrag          V2Drag9
159100130121      * Controllo se il cliente è già presente in tabella.
159200130121     c                   exsr      ctrOSR
159300130121     c                   IF        *in99
159400130121     c                   eval      *in66 = *on
159500130121     c                   leavesr
159600130121     c                   ENDIF
159700120302      * controllo se il cliente esite su FNACR come 333 e 444
159800120307     c                   IF        v2fvas = 'N'
159900120302     c                   eval      kacr = %dec(v2ccli9:10:0) * 1000
160000120302     c                   exsr      CtrACR
160100120302if  2c                   IF        *in99
160200120302     c                   eval      *in66 = *on
160300120302     c                   leavesr
160400120302     c                   ENDIF
160500120307     c                   ENDIF
160600080502e   1c                   ENDIF
160700080502
160800080502      * - Codice cliente 10 valido
160900080502     c                   clear                   V2Drag10
161000080502if  1c                   IF            V2Ccli10 <> *blanks
161100080502     c                             and V2Ccli10 <> *zeros
161200120112     c     c_Digits      check     V2Ccli10                               20
161300080502if  2c                   if        *in20
161400090522     c                   seton                                        67  99
161500080502     c                   movel     $Msg(06)      V1Dmsg
161600080502     c                   goto      EndCtrV2
161700080502e   2c                   endif
161800090525     c                   eval      wksc = %dec(V2Ccli10:7:0)
161900090522     c                   exsr      sr_tibs69
162000090522if  2c                   if        *in99
162100090522     c                   seton                                        67
162200080502     c                   goto      EndCtrV2
162300080502e   2c                   endif
162400090522     c                   movel     wrag          V2Drag10
162500130121      * Controllo se il cliente è già presente in tabella.
162600130121     c                   exsr      ctrOSR
162700130121     c                   IF        *in99
162800130121     c                   eval      *in67 = *on
162900130121     c                   leavesr
163000130121     c                   ENDIF
163100120302      * controllo se il cliente esite su FNACR come 333 e 444
163200120307     c                   IF        v2fvas = 'N'
163300120302     c                   eval      kacr = %dec(v2ccli10:10:0) * 1000
163400120302     c                   exsr      CtrACR
163500120302if  2c                   IF        *in99
163600120302     c                   eval      *in67 = *on
163700120302     c                   leavesr
163800120302     c                   ENDIF
163900120307     c                   ENDIF
164000080502e   1c                   ENDIF
164100080502
164200080502      * - Codice cliente 11 valido
164300080502     c                   clear                   V2Drag11
164400080502if  1c                   IF            V2Ccli11 <> *blanks
164500080502     c                             and V2Ccli11 <> *zeros
164600120112     c     c_Digits      check     V2Ccli11                               20
164700080502if  2c                   if        *in20
164800090522     c                   seton                                        68  99
164900080502     c                   movel     $Msg(06)      V1Dmsg
165000080502     c                   goto      EndCtrV2
165100080502e   2c                   endif
165200090525     c                   eval      wksc = %dec(V2Ccli11:7:0)
165300090522     c                   exsr      sr_tibs69
165400090522if  2c                   if        *in99
165500090522     c                   seton                                        68
165600080502     c                   goto      EndCtrV2
165700080502e   2c                   endif
165800090522     c                   movel     wrag          V2Drag11
165900130121      * Controllo se il cliente è già presente in tabella.
166000130121     c                   exsr      ctrOSR
166100130121     c                   IF        *in99
166200130121     c                   eval      *in68 = *on
166300130121     c                   leavesr
166400130121     c                   ENDIF
166500120302      * controllo se il cliente esite su FNACR come 333 e 444
166600120307     c                   IF        v2fvas = 'N'
166700120302     c                   eval      kacr = %dec(v2ccli11:10:0) * 1000
166800120302     c                   exsr      CtrACR
166900120302if  2c                   IF        *in99
167000120302     c                   eval      *in68 = *on
167100120302     c                   leavesr
167200120302     c                   ENDIF
167300120307     c                   ENDIF
167400080502e   1c                   ENDIF
167500080502
167600080502      * - Codice cliente 12 valido
167700080502     c                   clear                   V2Drag12
167800080502if  1c                   IF            V2Ccli12 <> *blanks
167900080502     c                             and V2Ccli12 <> *zeros
168000120112     c     c_Digits      check     V2Ccli12                               20
168100080502if  2c                   if        *in20
168200090522     c                   seton                                        69  99
168300080502     c                   movel     $Msg(06)      V1Dmsg
168400080502     c                   goto      EndCtrV2
168500080502e   2c                   endif
168600090525     c                   eval      wksc = %dec(V2Ccli12:7:0)
168700090522     c                   exsr      sr_tibs69
168800090522if  2c                   if        *in99
168900090522     c                   seton                                        69
169000080502     c                   goto      EndCtrV2
169100080502e   2c                   endif
169200090522     c                   movel     wrag          V2Drag12
169300130121      * Controllo se il cliente è già presente in tabella.
169400130121     c                   exsr      ctrOSR
169500130121     c                   IF        *in99
169600130121     c                   eval      *in69 = *on
169700130121     c                   leavesr
169800130121     c                   ENDIF
169900120302      * controllo se il cliente esite su FNACR come 333 e 444
170000120307     c                   IF        v2fvas = 'N'
170100120302     c                   eval      kacr = %dec(v2ccli12:10:0) * 1000
170200120302     c                   exsr      CtrACR
170300120302if  2c                   IF        *in99
170400120302     c                   eval      *in69 = *on
170500120302     c                   leavesr
170600120302     c                   ENDIF
170700120307     c                   ENDIF
170800080502e   1c                   ENDIF
170900080502
171000080502      * - Codice cliente 13 valido
171100080502     c                   clear                   V2Drag13
171200080502if  1c                   IF            V2Ccli13 <> *blanks
171300080502     c                             and V2Ccli13 <> *zeros
171400120112     c     c_Digits      check     V2Ccli13                               20
171500080502if  2c                   if        *in20
171600090525     c                   seton                                        70  99
171700080502     c                   movel     $Msg(06)      V1Dmsg
171800080502     c                   goto      EndCtrV2
171900080502e   2c                   endif
172000090525     c                   eval      wksc = %dec(V2Ccli13:7:0)
172100090522     c                   exsr      sr_tibs69
172200090522if  2c                   if        *in99
172300090525     c                   seton                                        70
172400080502     c                   goto      EndCtrV2
172500080502e   2c                   endif
172600090522     c                   movel     wrag          V2Drag13
172700130121      * Controllo se il cliente è già presente in tabella.
172800130121     c                   exsr      ctrOSR
172900130121     c                   IF        *in99
173000130121     c                   eval      *in70 = *on
173100130121     c                   leavesr
173200130121     c                   ENDIF
173300120302      * controllo se il cliente esite su FNACR come 333 e 444
173400120307     c                   IF        v2fvas = 'N'
173500120302     c                   eval      kacr = %dec(v2ccli13:10:0) * 1000
173600120302     c                   exsr      CtrACR
173700120302if  2c                   IF        *in99
173800120302     c                   eval      *in70 = *on
173900120302     c                   leavesr
174000120302     c                   ENDIF
174100120307     c                   ENDIF
174200080502e   1c                   ENDIF
174300080502
174400080502      * - Codice cliente 14 valido
174500080502     c                   clear                   V2Drag14
174600080502if  1c                   IF            V2Ccli14 <> *blanks
174700080502     c                             and V2Ccli14 <> *zeros
174800120112     c     c_Digits      check     V2Ccli14                               20
174900080502if  2c                   if        *in20
175000090522     c                   seton                                        71  99
175100080502     c                   movel     $Msg(06)      V1Dmsg
175200080502     c                   goto      EndCtrV2
175300080502e   2c                   endif
175400090525     c                   eval      wksc = %dec(V2Ccli14:7:0)
175500090522     c                   exsr      sr_tibs69
175600090522if  2c                   if        *in99
175700090522     c                   seton                                        71
175800080502     c                   goto      EndCtrV2
175900080502e   2c                   endif
176000090522     c                   movel     wrag          V2Drag14
176100130121      * Controllo se il cliente è già presente in tabella.
176200130121     c                   exsr      ctrOSR
176300130121     c                   IF        *in99
176400130121     c                   eval      *in71 = *on
176500130121     c                   leavesr
176600130121     c                   ENDIF
176700120302      * controllo se il cliente esite su FNACR come 333 e 444
176800120307     c                   IF        v2fvas = 'N'
176900120302     c                   eval      kacr = %dec(v2ccli14:10:0) * 1000
177000120302     c                   exsr      CtrACR
177100120302if  2c                   IF        *in99
177200120302     c                   eval      *in71 = *on
177300120302     c                   leavesr
177400120302     c                   ENDIF
177500120307     c                   ENDIF
177600080502e   1c                   ENDIF
177700080502
177800080502      * - Codice cliente 15 valido
177900080502     c                   clear                   V2Drag15
178000080502if  1c                   IF            V2Ccli15 <> *blanks
178100080502     c                             and V2Ccli15 <> *zeros
178200120112     c     c_Digits      check     V2Ccli15                               20
178300080502if  2c                   if        *in20
178400090522     c                   seton                                        72  99
178500080502     c                   movel     $Msg(06)      V1Dmsg
178600080502     c                   goto      EndCtrV2
178700080502e   2c                   endif
178800090525     c                   eval      wksc = %dec(V2Ccli15:7:0)
178900090522     c                   exsr      sr_tibs69
179000090522if  2c                   if        *in99
179100090522     c                   seton                                        72  99
179200080502     c                   goto      EndCtrV2
179300080502e   2c                   endif
179400090522     c                   movel     wrag          V2Drag15
179500130121      * Controllo se il cliente è già presente in tabella.
179600130121     c                   exsr      ctrOSR
179700130121     c                   IF        *in99
179800130121     c                   eval      *in72 = *on
179900130121     c                   leavesr
180000130121     c                   ENDIF
180100120302      * controllo se il cliente esite su FNACR come 333 e 444
180200120307     c                   IF        v2fvas = 'N'
180300120302     c                   eval      kacr = %dec(v2ccli15:10:0) * 1000
180400120302     c                   exsr      CtrACR
180500120302if  2c                   IF        *in99
180600120302     c                   eval      *in72 = *on
180700120302     c                   leavesr
180800120302     c                   ENDIF
180900120307     c                   ENDIF
181000080502e   1c                   ENDIF
181100080502
181200080502      * - Codice cliente 16 valido
181300080502     c                   clear                   V2Drag16
181400080502if  1c                   IF            V2Ccli16 <> *blanks
181500080502     c                             and V2Ccli16 <> *zeros
181600120112     c     c_Digits      check     V2Ccli16                               20
181700080502if  2c                   if        *in20
181800090522     c                   seton                                        73  99
181900080502     c                   movel     $Msg(06)      V1Dmsg
182000080502     c                   goto      EndCtrV2
182100080502e   2c                   endif
182200090525     c                   eval      wksc = %dec(V2Ccli16:7:0)
182300090522     c                   exsr      sr_tibs69
182400090522if  2c                   if        *in99
182500090522     c                   seton                                        73
182600080502     c                   goto      EndCtrV2
182700080502e   2c                   endif
182800090522     c                   movel     wrag          V2Drag16
182900130121      * Controllo se il cliente è già presente in tabella.
183000130121     c                   exsr      ctrOSR
183100130121     c                   IF        *in99
183200130121     c                   eval      *in73 = *on
183300130121     c                   leavesr
183400130121     c                   ENDIF
183500120302      * controllo se il cliente esite su FNACR come 333 e 444
183600120307     c                   IF        v2fvas = 'N'
183700120302     c                   eval      kacr = %dec(v2ccli16:10:0) * 1000
183800120302     c                   exsr      CtrACR
183900120302if  2c                   IF        *in99
184000120302     c                   eval      *in73 = *on
184100120302     c                   leavesr
184200120302     c                   ENDIF
184300080502e   1c                   ENDIF
184400120307e   1c                   ENDIF
184500130125
184600130125      * Riaggancio il tntbe del rcd in modifica
184700130125     c                   movel(p)  DSkeyOSR      TBEke1
184800130125     c     K05TBE01      chain     TNTBE000
184900130123
185000091124      * traduttore personalizzato
185100091124     c                   if        v2fvas = *blanks
185200091124     c                   eval      *in99 = *on
185300091124     c                   eval      *in79 = *on
185400091124     c                   eval      v1dmsg = $msg(13)
185500091124     c                   leavesr
185600091124     c                   endif
185700090525
185800090525      * codice ordinante valido
185900090525     c                   if        v2ccor > *zeros
186000090525     c                   eval      wksc = v2ccor
186100090525     c                   exsr      sr_tibs69
186200090525     c                   if        *in99
186300090525     c                   eval      *in74 = *on
186400090525     c                   leavesr
186500090525     c                   endif
186600090525      * deve esistere anche su anagrafica clienti ritiro
186700090525     c                   eval      kacr = %dec(v2ccor:10:0) * 1000 + 333
186800090525     c     kacr          chain     fnacr01l
186900090525     c                   if        not %found(fnacr01l) or acratb <> *blanks
187000090525     c                   eval      *in99 = *on
187100090525     c                   eval      *in74 = *on
187200090525     c                   eval      v1dmsg = $msg(09)
187300090525     c                   leavesr
187400090525     c                   endif
187500090525     c                   eval      v2dcor = acrrsr
187600090525     c                   endif
187700090525
187800090525      * codice consegna valido
187900090525     c                   if        v2ccrc > *zeros
188000090525     c                   eval      wksc = v2ccrc
188100090525     c                   exsr      sr_tibs69
188200090525     c                   if        *in99
188300090525     c                   eval      *in75 = *on
188400090525     c                   leavesr
188500090525     c                   endif
188600090525      * deve esistere anche su anagrafica clienti ritiro
188700090525     c                   eval      kacr = %dec(v2ccrc:10:0) * 1000 + 444
188800090525     c     kacr          chain     fnacr01l
188900090525     c                   if        not %found(fnacr01l) or acratb <> *blanks
189000090525     c                   eval      *in99 = *on
189100090525     c                   eval      *in75 = *on
189200090525     c                   eval      v1dmsg = $msg(09)
189300090525     c                   leavesr
189400090525     c                   endif
189500090525     c                   eval      v2dcrc = acrrsr
189600090525     c                   endif
189700090525
189800090525      * chi paga non può essere vuoto
189900090624      * se non è traduttore personalizzato
190000091124     c                   if        v2cpag = *blanks and v2fvas = 'N'
190100090525     c                   eval      *in99 = *on
190200090525     c                   eval      *in77 = *on
190300090525     c                   eval      v1dmsg = $msg(11)
190400090525     c                   leavesr
190500090525     c                   endif
190600090610
190700090623      * flag per colli e peso non può essere vuoto
190800090624      * se non è traduttore personalizzato
190900091124     c                   if        v2cnclpkg = *blanks and v2fvas = 'N'
191000090610     c                   eval      *in99 = *on
191100090610     c                   eval      *in78 = *on
191200090610     c                   eval      v1dmsg = $msg(12)
191300090610     c                   leavesr
191400090610     c                   endif
191500090525
191600090525      * filiale consegna valida
191700090525     c                   if        v2cpoc > *zeros
191800090525     c     v2cpoc        chain     azorg01l
191900090525     c                   if        not %found(azorg01l) or
192000090525     c                             orgfva <> *blanks
192100090525     c                   eval      *in99 = *on
192200090525     c                   eval      *in76 = *on
192300090525     c                   eval      v1dmsg = $msg(10)
192400090525     c                   leavesr
192500090525     c                   endif
192600090525     c                   eval      v2dpoc = orgdes
192700090525     c                   endif
192800030805      *
192900030805     c     EndCtrV2      ENDSR
193000090522
193100090522      *---------------------------------------------------------------*
193200090522      * SR_TIBS69 - Decodifico/Controllo cliente                      *
193300090522      *---------------------------------------------------------------*
193400090522     c     sr_tibs69     BEGSR
193500090522
193600090522     c                   clear                   wrag
193700090522     c                   clear                   TIBS69ds
193800090522     c                   clear                   DS_ACO
193900090522     c                   clear                   DS_IND
194000090522     c                   clear                   DS_CLP
194100090522     c                   clear                   DS_CLS
194200090522     c                   eval      I69kcc = DUTkci
194300090522     c                   move      wksc          I69kac
194400090522     c                   eval      I69sif = KNSIF
194500090522     c                   call      'TIBS69R'
194600090522     c                   parm                    TIBS69ds
194700090522     c                   parm                    DS_ACO
194800090522     c                   parm                    DS_IND
194900090522     c                   parm                    DS_CLP
195000090522     c                   parm                    DS_CLS
195100090522     c                   if        O69err <> *blanks
195200090522     c                   eval      *in99 = *on
195300090522     c                   movel     $Msg(06)      V1Dmsg
195400090522     c                   endif
195500090522      * Decodifica cliente
195600090522     c                   movel     ACOrag        wrag
195700090522
195800090522     c                   ENDSR
195900130121
196000130121      /free
196100130121       //--------------------------------------------------------------
196200130121       //?Controllo se cliente già presente in tabella
196300130121       //--------------------------------------------------------------
196400130121       BEGSR ctrOSR;
196500130121
196600130121         *in99 = *off;
196700130121         clear wOSR;
196800130121       //?leggo tutta la tabella OSR
196900130121         TBEcod = 'OSR';
197000130121         setll TBEcod tntbe01l;
197100130123         reade(n) TBEcod tntbe01l;
197200130121
197300130121         DOW not %eof(tntbe01l);
197400130121           IF  TBEatb = *blanks;
197500130121           wosr = tbeuni;
197600130121           IF w§osrcli  = wKSC or w§OSRcl2  = wKSC or
197700130121              w§osrcl3  = wKSC or w§OSRcl4  = wKSC or
197800130121              w§osrcl5  = wKSC or w§OSRcl6  = wKSC or
197900130121              w§osrcl7  = wKSC or w§OSRcl8  = wKSC or
198000130121              w§osrcl9  = wKSC or w§OSRcl10 = wKSC or
198100130121              w§osrcl11 = wKSC or w§OSRcl12 = wKSC or
198200130121              w§osrcl13 = wKSC or w§OSRcl14 = wKSC or
198300130121              w§osrcl15 = wKSC or w§OSRcl16 = wKSC;
198400130121             IF  TBEke1 <> dskeyOSR;
198500130121               V1Dmsg = 'Cliente già presente in tabella con LNP ' +
198600130121                         %subst(TBEke1:1:3) + ' e SERIE ' +
198700130121                         %subst(TBEke1:4:2);
198800130121               *in99 = *on;
198900130121               leavesr;
199000130121             ENDIF;
199100130121           ENDIF;
199200130121           ENDIF;
199300130123           reade(n) TBEcod tntbe01l;
199400130121         ENDDO;
199500130121
199600130121       ENDSR;
199700130121      /end-free
199800120302
199900120302      *---------------------------------------------------------------*
200000120302      * CtrACR - Controllo se il codice cliente esiste su ACR         *
200100120302      *---------------------------------------------------------------*
200200120302     c     CtrACR        BEGSR
200300120302
200400120302      * Controllo 333
200500120302     c                   eval      kacr += 333
200600120302     c     kacr          chain     fnacr01l
200700120302     c                   if        not %found(fnacr01l) or acratb <> *blanks
200800120302     c                   eval      *in99 = *on
200900120302     c                   eval      v1dmsg = $msg(09)
201000120302     c                   eval      v1dmsg = %trim(v1dmsg) + ' con luogo 333'
201100120302     c                   leavesr
201200120302     c                   endif
201300120302      * Controllo 444
201400120302     c                   eval      kacr -= 333
201500120302     c                   eval      kacr += 444
201600120302     c     kacr          chain     fnacr01l
201700120302     c                   if        not %found(fnacr01l) or acratb <> *blanks
201800120302     c                   eval      *in99 = *on
201900120302     c                   eval      v1dmsg = $msg(09)
202000120302     c                   eval      v1dmsg = %trim(v1dmsg) + ' con luogo 444'
202100120302     c                   leavesr
202200120302     c                   endif
202300120302
202400120302     c                   ENDSR
202500090522
202600090522      *---------------------------------------------------------------*
202700090525      * GESV3  - Gestione videata dettaglio dati                      *
202800090522      *---------------------------------------------------------------*
202900090525     c     GesV3         BEGSR
203000090522      *
203100090522      * Inizializzazione videata
203200090525     c                   if        $CarV3 = *on
203300090525     c                   exsr      CarV3
203400090525     c                   move      *off          $CarV3
203500090522     c                   endif
203600091203      * Scrivo la testata e il piede
203700090522     c                   write     TB30T1
203800091203     c                   write     TB30T2
203900091203     c                   write     TB30P3
204000090522      *
204100090522      * Se esistono errori sulla videata
204200090522      * emetto la write del formato a indicatori spenti per vedere
204300090522      * le eventuali decodifiche
204400090522     c                   if        *in99
204500090522     c                   movea     *in           Win
204600090522     c                   movea     *zeros        *in(50)
204700090525     c                   write     TB30V3
204800090522     c                   movea     Win           *in
204900090522     c                   endif
205000090522      *
205100090522if  1c                   if        *in05
205200090525     c                   write     TB30V3
205300090522     c                   exfmt     PROTECT
205400090522x   1c                   else
205500090525     c                   exfmt     TB30V3
205600090522e   1c                   endif
205700090522     c                   eval      *in99 = *off
205800090522     c                   clear                   V1Dmsg
205900090522     c                   clear                   wTasto
206000090522      *
206100090522sel 1c                   select
206200090522      * F03=Fine
206300090522w   1c                   when      *inKC
206400090522     c                   exsr      F03V1
206500090525     c                   leavesr
206600090522      * F12=Ritorno
206700090522w   1c                   when      *inKL
206800090525     c                   exsr      F12V3
206900090525     c                   leavesr
207000090522e   1c                   endsl
207100090525      *
207200090525      * Controllo dati immessi a video
207300090525      * (non si fanno se richisto l'annullamento)
207400090525if  1c                   if        not *inKQ
207500090525     c                   exsr      CtrV3
207600090525e   1c                   endif
207700090525      *
207800090525      * Aggiornamento se non ci sono errori
207900090525if  1c                   if        not *in99 and not *in90
208000090525     c                             and (*inKF or *inKE or *inKQ)
208100090525sel 2c                   select
208200090525w   2c                   when      *inKE
208300090525     c                   eval      wTasto = '05'
208400090525w   2c                   when      *inKF
208500090525     c                   eval      wTasto = '06'
208600090525w   2c                   when      *inKQ
208700090525     c                   eval      wTasto = '16'
208800090525e   2c                   endsl
208900091204       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
209000091204       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
209100091204       //  ?NON ha più senso gestire la richiesta di trasmissione!?
209200091204     ***c                   eval      $CarW1 = *on
209300091204     ***c                   eval      $TipVid = 'A'
209400091204     c                   exsr      AggTBE
209500090525e   1c                   endif
209600090522      *
209700090525     c                   ENDSR
209800090525
209900090525      *---------------------------------------------------------------*
210000090525      * CARV3  - Caricamento dati terza videata                       *
210100090525      *---------------------------------------------------------------*
210200090525     c     CarV3         BEGSR
210300090525      *
210400090525     c                   eval      V2Ccbo = D§OSRcbo
210500090525     c                   eval      V2Dcbo = *blanks
210600090525     c                   eval      V2Cman = D§OSRman
210700090525     c                   eval      V2Ccpy = D§OSRcpy
210800090525     c                   eval      V2Cstp = D§OSRstp
210900090525     c                   eval      V2Cass = D§OSRass
211000090525     c                   eval      V2Cdir = D§OSRdir
211100090525     c                   eval      V2Csta = D§OSRsta
211200090525     c                   eval      V2Crmn = D§OSRrmn
211300090525     c                   eval      V2Crma = D§OSRrma
211400090525      * decodifico
211500090525     c                   clear                   V2Dcbo
211600090525if  1c                   if        V2Ccbo <> *blanks
211700090525     c                   clear                   ds3A
211800090525     c                   eval      TBLcod = '3A'
211900090525     c                   movel(p)  V2Ccbo        TBLkey
212000090525     c     K03TAB00      chain     TABEL
212100090525if  2c                   if        %found(TABEL00F)
212200090525     c                             and TBLflg = *blanks
212300090525     c                   movel     TBLuni        ds3A
212400090525     c                   movel     §3Ades        V2Dcbo
212500090525e   1c                   endif
212600090525e   1c                   endif
212700091124
212800091124      * se immissione imposto campi di dft
212900091124     c                   if        *in01
213000091124     c                   eval      v2ccbo = '2R'
213100091124     c                   eval      v2cman = 'S'
213200091124     c                   eval      v2cstp = 'S'
213300091124     c                   eval      v2ccpy = 'S'
213400091124     c                   eval      v2cass = 'S'
213500091124     c                   eval      v2cdir = 'S'
213600091124     c                   eval      v2csta = 'S'
213700091124     c                   endif
213800090525
213900090525     c                   ENDSR
214000090522
214100090522      *---------------------------------------------------------------*
214200090525      * F12V3  - Tasto funzionale F12 -> Ritorno                      *
214300090522      *---------------------------------------------------------------*
214400090525     c     F12V3         BEGSR
214500090522      *
214600090525     c                   eval      $TipVid = '2'
214700090522      *
214800090522     c                   ENDSR
214900090522
215000090522      *---------------------------------------------------------------*
215100090525      * CTRV2  - Controllo e decodifica terza videata                 *
215200090522      *---------------------------------------------------------------*
215300090525     c     CtrV3         BEGSR
215400090522      *
215500090522     c                   movea     *zeros        *in(50)
215600090522     c                   clear                   V1Dmsg
215700090522      *
215800090522      * Ricerche
215900090522     c                   clear                   xx
216000090522     c                   clear                   $Fld
216100090525      * - Codice bolla da forzare
216200090525     c     '?'           scan      V2Ccbo                                 20
216300090525if  1c                   if        *in20
216400090525     c                   add       1             XX
216500090525     c                   eval      $Fld(xx) = 'V2CCBO    '
216600090525e   1c                   endif
216700090522      *
216800090522if  1c                   if        xx > *zeros
216900090522     c                   exsr      Search
217000090522e   1c                   endif
217100090522      *
217200090522      * Controlli
217300090522      *
217400090522      * Codice bolla da forzare in bollettazione
217500090522     c                   clear                   V2Dcbo
217600090522if  1c                   if        V2Ccbo <> *blanks
217700090522     c                   clear                   ds3A
217800090522     c                   eval      TBLcod = '3A'
217900090522     c                   movel(p)  V2Ccbo        TBLkey
218000090522     c     K03TAB00      chain     TABEL
218100090522if  2c                   if        %found(TABEL00F)
218200090522     c                             and TBLflg = *blanks
218300090522     c                   movel     TBLuni        ds3A
218400090522     c                   movel     §3Ades        V2Dcbo
218500090522x   2c                   else
218600090522     c                   seton                                        54  99
218700090522     c                   movel     $Msg(07)      V1Dmsg
218800090525     c                   leavesr
218900090522e   2c                   endif
219000090522e   1c                   endif
219100090522      *
219200090525     c                   ENDSR
219300030805
219400030805      *---------------------------------------------------------------*
219500030805      * GESW1  - Gestione videata dati relativi alla trasmissione     *
219600030805      *---------------------------------------------------------------*
219700030805     c     GesW1         BEGSR
219800030805      *
219900030805      * Inizializzazione videata
220000030805if  1c                   if        $CarW1 = *on
220100030805     c                   exsr      CarW1
220200030805     c                   movel     *off          $CarW1
220300030805e   1c                   endif
220400030805      *
220500030805if  1c                   if        *in05
220600030805     c                   write     TB30W1
220700030805     c                   exfmt     PROTECT
220800030805x   1c                   else
220900030805     c                   exfmt     TB30W1
221000030805e   1c                   endif
221100030805     c                   eval      *in99 = *off
221200030805     c                   clear                   W1MSG
221300030805      *
221400030805sel 1c                   select
221500030805      * F12=Ritorno
221600030805w   1c                   when      *inKL
221700030805     c                   exsr      F12W1
221800030805     c                   goto      EndGesW1
221900030805e   1c                   endsl
222000030805      *
222100030805      * Controllo dati immessi a video
222200030805     c                   exsr      CtrW1
222300030805      *
222400030805      * Aggiornamento se non ci sono errori
222500030805if  1c                   if        not *in99 and *inKF
222600030805     c                   exsr      AggTBE
222700030805e   1c                   endif
222800030805      *
222900030805     c     EndGesW1      ENDSR
223000030805
223100030805      *---------------------------------------------------------------*
223200030805      * CARW1  - Caricamento dati window                              *
223300030805      *---------------------------------------------------------------*
223400030805     c     CarW1         BEGSR
223500030805      *
223600030805     c                   movea     *zeros        *in(50)
223700030805      *
223800030805sel 1c                   select
223900030805      *
224000030805      * F5=Ripristino
224100030805w   1c                   when      *inKE   and  *in04
224200030805     c                   eval      W1FTT = TBEftt
224300030805      *
224400030805      * F6=Conferma
224500030805w   1c                   when      *inKF
224600030805sel 2c                   select
224700030805      *   Immissione
224800030805w   2c                   when      *in01
224900030805     c                   eval      W1FTT = TBXftt
225000030805      *   Modifica / Ripristino
225100030805w   2c                   when      *in02   or    *in04
225200030805     c                   eval      W1FTT = TBEftt
225300030805e   2c                   endsl
225400030805      *
225500030805      * F16=Annullamento
225600030805w   1c                   when      *inKQ   and  not *in04
225700030805     c                   eval      W1FTT = TBEftt
225800030805      *
225900030805e   1c                   endsl
226000030805      *
226100030805      * Se NON immissione: visualizzo i dati relativi all'ultima
226200030805      *   trasmissione
226300030805if  1c                   if        not *in01
226400030805     c                   eval      W1FLT = TBEflt
226500030805     c                   eval      W1FTR = TBEftr
226600030805if  2c                   if        TBEdtr <> 0
226700030805     c                   clear                   WLBDAT
226800030805     c                   z-add     TBEdtr        G02inv
226900030805     c                   movel     '3'           G02err
227000030805     c                   call      'XSRDA8'
227100030805     c                   parm                    WLBDAT
227200030805     c                   z-add     G02dat        W1DTR
227300030805e   2c                   endif
227400030805e   1c                   endif
227500030805      *
227600030805     c                   ENDSR
227700030805
227800030805      *---------------------------------------------------------------*
227900030805      * CTRW1  - Controllo e decodifica window                        *
228000030805      *---------------------------------------------------------------*
228100030805     c     CtrW1         BEGSR
228200030805      *
228300030805     c                   movea     *zeros        *in(50)
228400030805      *
228500091203     c                   ENDSR
228600030805
228700030805      *---------------------------------------------------------------*
228800030805      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
228900030805      *---------------------------------------------------------------*
229000030805     c     F12W1         BEGSR
229100030805      *
229200030805     c                   eval      $TipVid = '2'
229300030805      *
229400030805     c                   ENDSR
229500030805
229600030805      *---------------------------------------------------------------*
229700030805      * CHNTBE * Aggancio tabella                                     *
229800030805      *---------------------------------------------------------------*
229900030805     c     ChnTBE        BEGSR
230000030805      *
230100030805     c                   movel     T02cod        TBEcod
230200030805     c                   movel(p)  DSkeyOSR      TBEke1
230300030805     c                   clear                   TBEke2
230400030805     c                   clear                   TBElin
230500030805     c                   movel     KNSIF         TBEsif
230600091203     c     K05TBE01      chain     TNTBE000
230700030805      * Se non ho trovato il record con il sistema informativo
230800030805      * che ho in linea, lo abblenco
230900030805if  1c                   if        not %found(TNTBE01L)
231000030805     c                   clear                   TBEsif
231100091203     c     K05TBE01      chain     TNTBE000
231200030805e   1c                   endif
231300030805      *
231400030805     c                   ENDSR
231500030805
231600030805      *---------------------------------------------------------------*
231700030805      * AGGTBE * Aggiornamento tabella                                *
231800030805      *---------------------------------------------------------------*
231900030805     c     AggTBE        BEGSR
232000030805      *
232100091203      /free
232200091203
232300091203         // -?Ciclo di elaborazione per ogni sistema informativo?
232400091203         For  w_SisInf = 1  To  %elem($Libr);
232500091203
232600091203           // -?Apertura (eventuale) del file ed aggancio record?
232700091203           if  w_SisInf > 1;
232800091203             exsr  sr_OpenFileTab;
232900091203             exsr  ChnTBE;
233000091203           endif;
233100091203
233200091203         //? N.B.                                                      ?
233300091203         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
233400091203         //   trasmissione (vedi flag TBEFTR).                        ?
233500091203         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
233600091203         //   subito nello stesso file di entrambi i S.I. - in due    ?
233700091203         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
233800091203
233900091203      /end-free
234000030805sel 1c                   SELECT
234100030805      *
234200030805      * F5=Ripristino
234300030805w   1c                   WHEN      wTasto='05'  and  *in04
234400091203      /free
234500091203               if  %found(TNTBE01L);
234600091203      /end-free
234700030805     c                   clear                   TBEatb
234800030805     c                   clear                   TBEftr
234900030805     c                   UPDATE    TNTBE000
235000091203      /free
235100091203               else;
235200091203                 // - Record appena cancellato fisicamente
235300091203                 exsr  RieTBE;
235400091203                 WRITE  TNTBE000;
235500091203               endif;
235600091203      /end-free
235700030805      *
235800030805      * F6=Conferma
235900030805w   1c                   WHEN      wTasto='06'
236000030805     c                   exsr      RieTBE
236100030805sel 2c                   select
236200030805      *   Immissione
236300030805w   2c                   when      *in01
236400030805     c                   WRITE     TNTBE000                             22
236500030805      *   Modifica / Ripristino
236600030805w   2c                   when      *in02   or    *in04
236700030805     c                   UPDATE    TNTBE000
236800030805e   2c                   endsl
236900030805      *
237000030805      * F16=Annullamento
237100030805w   1c                   WHEN      wTasto='16'  and  not *in04
237200030805     c                   movel     'A'           TBEatb
237300030805     c                   clear                   TBEftr
237400030805     c                   UPDATE    TNTBE000
237500030805      *
237600030805e   1c                   ENDSL
237700091203      /free
237800091203
237900091203         EndFor;
238000091203
238100091203      /end-free
238200030805      *
238300120112     c                   if        $Called = *off
238400030805      * Torno alla prima videata che carico come da inizio pgm
238500030805     c                   movel     '1'           $TipVid
238600030805     c                   movel     *on           $CarV1
238700030805     c                   movel     *on           $CarV2
238800090522     c                   movel     *on           $CarV3
238900120302
239000120302      * prima di tornare alla prima videata devo richiamare il pgm di gestione
239100120302      * particolarità
239200120302     c                   clear                   kpjbu
239300120302     c                   call      'TRTB71R'
239400120302     c                   parm                    kpjba
239500120302
239600120112     c                   else
239700120305
239800120305      * prima di uscire devo richiamare il pgm di gestione
239900120305      * particolarità
240000120305     c                   clear                   kpjbu
240100120305     c                   call      'TRTB71R'
240200120305     c                   parm                    kpjba
240300120112      * - Uscita dal pgm
240400120112     c                   exsr      F03V1
240500120112     c                   endif
240600120112      *
240700030805     c                   ENDSR
240800030805
240900030805      *---------------------------------------------------------------*
241000030805      * RIETBE * Riempimento dati tabella                             *
241100030805      *---------------------------------------------------------------*
241200030805     c     RieTBE        BEGSR
241300030805      *
241400030805     c                   clear                   TBEatb
241500030805     c                   if        TBXsif <> *blanks
241600030805     c                   movel     KNSIF         TBEsif
241700030805     c                   else
241800030805     c                   clear                   TBEsif
241900030805     c                   endif
242000030805     c                   movel     TBXapl        TBEapl
242100030805     c                   movel     T02cod        TBEcod
242200030805     c                   movel     DSkeyOSR      TBEke1
242300030805     c                   clear                   TBEke2
242400030805      *
242500030805     c                   clear                   dOSR
242600030805     c                   move      V2Ccli        D§OSRcli
242700030805     c                   eval      D§OSRrag = V2Drag
242800040303     c                   move      V2Ccli2       D§OSRcl2
242900040303     c                   eval      D§OSRra2 = V2Drag2
243000040303     c                   move      V2Ccli3       D§OSRcl3
243100040303     c                   eval      D§OSRra3 = V2Drag3
243200040303     c                   move      V2Ccli4       D§OSRcl4
243300090610     c                   eval      D§OSRra4 = V2Drag4
243400080430     c                   move      V2Ccli5       D§OSRcl5
243500091204      *
243600091124     c                   if        v2fvas = 'N'
243700091124     c                   clear                   d§osrvas
243800091124     c                   else
243900090522     c                   eval      d§osrvas = v2fvas
244000091124     c                   endif
244100090522     c                   eval      d§osrcor = v2ccor
244200090522     c                   eval      d§osrcrc = v2ccrc
244300090522     c                   eval      d§osrpag = v2cpag
244400090623     c                   eval      d§osrfcp = v2cnclpkg
244500090522     c                   eval      d§osrpoc = v2cpoc
244600091204      *
244700030805     c                   eval      D§OSRcbo = V2Ccbo
244800030805     c                   eval      D§OSRman = V2Cman
244900030805     c                   eval      D§OSRcpy = V2Ccpy
245000030805     c                   eval      D§OSRstp = V2Cstp
245100030805     c                   eval      D§OSRass = V2Cass
245200040414     c                   eval      D§OSRdir = V2Cdir
245300031002     c                   eval      D§OSRsta = V2Csta
245400040218     c                   eval      D§OSRrmn = V2Crmn
245500040218     c                   eval      D§OSRrma = V2Crma
245600080502     c                   move      V2Ccli6       D§OSRcl6
245700080502     c                   move      V2Ccli7       D§OSRcl7
245800080502     c                   move      V2Ccli8       D§OSRcl8
245900080502     c                   move      V2Ccli9       D§OSRcl9
246000080502     c                   move      V2Ccli10      D§OSRcl10
246100080502     c                   move      V2Ccli11      D§OSRcl11
246200080502     c                   move      V2Ccli12      D§OSRcl12
246300080502     c                   move      V2Ccli13      D§OSRcl13
246400080502     c                   move      V2Ccli14      D§OSRcl14
246500080502     c                   move      V2Ccli15      D§OSRcl15
246600080502     c                   move      V2Ccli16      D§OSRcl16
246700030805     c                   movel(p)  dOSR          TBEuni
246800030805      *
246900091204     c                   movel     'S'           TBEftt
247000030807     c                   z-add     TBXflt        TBEflt
247100091203      /free
247200091203
247300091203         select;
247400091203           // - Annullamento o Ripristino
247500091203           when  (wTasto='05'  and  *in04)   or
247600091203                 (wTasto='16'  and  not *in04);
247700091203             clear  TBEftr;
247800091203             clear  TBEdtr;
247900091203           // - Aggiornamento o Scrittura in sede
248000091203           when  w_SisInf = 1;
248100091203             TBEftr = 'T';
248200091203             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
248300091203                                     : 1 : 8 ) );
248400091203           // - Aggiornamento o Scrittura in filiale
248500091203           other;
248600091203             TBEftr = 'R';
248700091203             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
248800091203                                     : 1 : 8 ) );
248900091203         endsl;
249000091203
249100091203      /end-free
249200030805      *
249300030805     c                   ENDSR
249400030805
249500091203       //--------------------------------------------------------------
249600091203       //?Schiere a tempo di compilazione.                             ?
249700091203       //--------------------------------------------------------------
249800091203
249900091203** - $iSystem / $Libraries:?Sistemi AS/400 & Librerie con il file TNTBE?
250000091203SETRAS  GAITRAGRU FILTRAGRU
250100091203AS888   GAITRAGRPSFILTRAGRPF
250200030806**   -$OPZ-   *
250300030805  Inserimento
250400030805    Modifica
250500030805     Copia
250600030805  Annullamento
250700030805Visualizzazione
250800030805   ANNULLATO
250900030806**   -$MSG-                                                                  *
251000090522Immettere la Fil. di emissione O.R.M.                                          01
251100091204Fil. di emissione O.R.M. inesistente o annullata                               02
251200030808Immettere un numero di serie O.R.M.                                            03
251300030806Numero di serie O.R.M. non valido                                              04
251400030806Immettere il codice cliente                                                    05
251500030806Codice cliente errato                                                          06
251600030807Codice bolla inesistente in tab. "3A"                                          07
251700130418Se ricerca per Cliente no ricerca per Filiale/Serie                            08
251800090525Cliente non trovato in anagrafica clienti ritiro                               09
251900090525Filiale consegna errata                                                        10
252000090525Inserire il flag "chi paga"                                                    11
252100090623Inserire come si devono impostare i colli e peso sul ritiro                    12
252200091124Inserire il flag traduttore personalizzato                                     13
252300130418Immettere o Filiale/Serie o Cliente                                            14
