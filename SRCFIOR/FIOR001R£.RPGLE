000100140414       //==============================================================
000200140414       //
000300170530       //?FIOR001R - ORM IMMISSIONE: Controller
000400140414       //
000500140618       // nel paradigma MVC è il Controller
000600140618       //
000700140414       //==============================================================
000800140414     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000900141008     h dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR':'TRUL':'TIS')
001000140414
001100140414       //--------------------------------------------------------------
001200140414       //?Dichiarazione file.                                          ?
001300140414       //--------------------------------------------------------------
001400140618     fAZORG01L  if   e           k disk
001500160615     fFNACR13L  if   e           k disk
001600141010     fFNORM01L  if   e           k disk
001700141010     fFNORN01L  if   e           k disk
001800141010     fFNORT01L  if   e           k disk
001900160607     fTABEL00F  if   e           k disk
002000160615     fTNTBE01L  if   e           k disk
002100140414
002200140414       //--------------------------------------------------------------
002300140414       //?Definizione costanti.                                        ?
002400140414       //--------------------------------------------------------------
002500140613       // -?Costanti dei programmi
002600140613      /copy gaitrasrc/srcconst,FIOR000R
002700140618      /copy gaitrasrc/srcconst,FIOR001R
002800140613      /copy gaitrasrc/srcconst,FIORA00R
002900140414
003000140414       //---------------------------------------------------------------
003100140414       //?Definizione aree dati.
003200140414       //---------------------------------------------------------------
003300140414
003400140414       //--------------------------------------------------------------
003500140414       //?Definizione strutture dati.                                  ?
003600140414       //--------------------------------------------------------------
003700140414       // -?Dati INPUT
003800141008     d FIOR000I      e ds                  QUALIFIED INZ(*EXTDFT)
003900141008     d FIOR001I      e ds                  QUALIFIED INZ(*EXTDFT)
004000140613     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
004100140613     d  idRichiamo   e                     INZ(FIORA00_ID_RICHIAMO_AS400)
004200140613     d  idLingua     e                     INZ(FIORA00_ID_LINGUA_IT)
004300170420     d FIORA01I      e ds                  QUALIFIED INZ(*EXTDFT)
004400140414
004500140414       // -?Dati OUTPUT
004600141008     d FIOR000O      e ds                  QUALIFIED INZ(*EXTDFT)
004700140613     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
004800170420     d FIORA01O      e ds                  QUALIFIED INZ(*EXTDFT)
004900140613
005000140613       // -?Messaggi
005100140613     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
005200140613     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
005300140613     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
005400140613     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
005500140613     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
005600140613
005700140613       // -?Forzature
005800140613     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
005900140613     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006000140613     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006100140613     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
006200141010
006300141010       // -?Controllo Anagrafiche clienti
006400141010     d FIORB0OO      e ds                  EXTNAME(FIORB0O1)
006500141010     d                                     QUALIFIED INZ(*EXTDFT)
006600141010     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
006700141010     d                                     QUALIFIED INZ(*EXTDFT)
006800141010     d FIORB0OC      e ds                  EXTNAME(FIORB0O1)
006900141010     d                                     QUALIFIED INZ(*EXTDFT)
007000140618
007100140618       // -?Attributi campo e Tasti Funzione
007200140618     d FIOR000A      e ds                  QUALIFIED INZ(*EXTDFT)
007300170330     d  skTastiFun                    1a   dim(26) overlay(TastiFun)
007400140618     d  skIdAttr                      5a   dim(99) overlay(IdAttr)
007500140618     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007600160608
007700160608       // -?Controllo/Decodifica/Ricerca Giro
007800160608     d FIDG09DS      e ds                  QUALIFIED INZ
007900160620
008000160620       // -?Recupero ora presunta ritiro da PDA
008100160620     d FIOR01DS      e ds                  QUALIFIED INZ
008200160614
008300160614       // -?Gestione note
008400160620     d FIOR06DS      e ds                  QUALIFIED INZ
008500141021
008600141021       // -?Controllo indirizzo completo
008700141021     d FNLV13DS      e ds                  QUALIFIED INZ
008800141009
008900141009       // -?Interrogazione Cappario
009000141009     d FNLV14DS      e ds                  QUALIFIED INZ
009100141007
009200141007       // -?Interrogazione Anagrafica clienti ritiro
009300141007     d FIOR81DS      e ds                  QUALIFIED INZ
009400170505
009500170505       // -?Interrogazione Cappario DPD
009600170505     d TISIE8DS      e ds                  QUALIFIED INZ
009700160621
009800160621       // -?Simulazione Delivery
009900170505     d TISI92DS      e ds                  QUALIFIED INZ
010000141010
010100141010       // -?Controllo indirizzo
010200141010     d TISI95DS      e ds                  QUALIFIED INZ
010300141021     d TISI97DS      e ds                  QUALIFIED INZ
010400160321
010500160321       // -?Reperimento filiali gestite dall'utente
010600160321     d TRUL31DS      e ds                  QUALIFIED INZ
010700160321     d  POG                   10    759    dim(250)
010800170403
010900170403       // -?Aggiunge/Toglie gg/mm dalla data
011000170403     d XGIOLAVDS     e ds                  inz
011100140414
011200140414       // -?Campo VAOFLO
011300141008     d dORM01        e ds                  QUALIFIED INZ
011400140414
011500140414       // -?Kpjba
011600141008     d KPJBA         e ds                  INZ
011700140415
011800140415       // -?Tabella DFT
011900141008     d dDFT          e ds                  QUALIFIED INZ
012000141017
012100141017       // -?Tabella PVO - Personalizzazione FLO
012200141017     d dPVO          e ds                  QUALIFIED INZ
012300160607
012400160607       // -?Tabella 15 - Nazioni
012500160607     d ds15          e ds                  QUALIFIED INZ
012600160608
012700160608       // -?Parametri per pgm. controllo/inversione data?
012800160608     d wlbdat          ds                  inz
012900160608     d  G08dat                 1      8  0
013000160608     d  G08inv                 9     16  0
013100160608     d  G08err                17     17
013200160608     d  G08tgi                18     22  0
013300140414
013400140414       // -?Status
013500140414     d Psds           sds
013600140414     d  SDSpgm           *proc
013700140414
013800140414       //--------------------------------------------------------------
013900140414       //?Definizione schiere.
014000140414       //--------------------------------------------------------------
014100140414
014200140414       //--------------------------------------------------------------
014300140414       //?Definizione campi.
014400140414       //--------------------------------------------------------------
014500140414       // -?Flags boolenai
014600160616     d EmessoAlertComm...
014700160616     d                 s               n   inz(*off)
014800160616     d EmessoAlertConf...
014900160616     d                 s               n   inz(*off)
015000160616     d Fine            s               n   inz(*off)
015100160616     d FineVideo       s               n   inz(*off)
015200141014     d RiemetteVideo   s               n   inz(*off)
015300141017     d wDatiFLO        s               n   inz(*off)
015400170414     d wOkF06F04       s               n   inz(*off)
015500170414     d wOkF06F13       s               n   inz(*off)
015600141022     d wOkF15          s               n   inz(*off)
015700140415
015800140415       // -?Campi di comodo data
015900140415     d dataISO         s               d   datfmt(*ISO)
016000140415     d dataEUR         s               d   datfmt(*EUR)
016100140414
016200140414       // -?Campi di comodo
016300160615     d conta           s              3s 0 inz
016400160615     d contartf        s              1s 0 inz
016500160615     d dataoggi        s              8s 0 inz
016600160615     d flgbac          s              1a   inz
016700160615     d keytabella      s             15a   inz
016800160615     d k_TBEcod        s                   inz like(TBEcod)
016900160615     d k_TBEke1        s                   inz like(TBEke1)
017000160615     d NrErrore        s              2s 0 inz
017100160615     d nrighe          s              1s 0 inz
017200160615     d nrmsgatt        s              3s 0 inz
017300160615     d nrmsgblc        s              3s 0 inz
017400160615     d nrmsgcgi        s              3s 0 inz
017500160615     d nrmsgmtc        s              3s 0 inz
017600160615     d pos             s              3s 0 inz
017700160615     d posda           s              3s 0 inz
017800160615     d posrtf1         s              3s 0 inz
017900160615     d posrtf2         s              3s 0 inz
018000160615     d posrtf3         s              3s 0 inz
018100160615     d rigatf1         s             61a   inz
018200160615     d rigatf2         s             61a   inz
018300160615     d rigatf3         s             61a   inz
018400160615     d savacrcgi       s                   inz like(fiora0I0.cgi)
018500160610     d savatt          s                   inz like(fiora0I0.vaoatt)
018600160610     d savblc          s                   inz like(fiora0I0.vaoblc)
018700160610     d savcgi          s                   inz like(fiora0I0.cgi)
018800160610     d savcomm         s                   inz like(fior000i.comm)
018900160610     d savffd          s                   inz like(fiora0I0.vaoffd)
019000160610     d savmail         s                   inz like(fiora0I0.mail)
019100160613     d savmailconf     s                   inz like(fiora0I0.mailconf)
019200160610     d savmtc          s                   inz like(fiora0I0.vaomtc)
019300160610     d savsms          s                   inz like(fiora0I0.sms)
019400160613     d savsmsconf      s                   inz like(fiora0I0.smsconf)
019500160610     d savsto          s                   inz like(fiora0I0.vaosto)
019600160615     d tastof          s             25a   inz
019700160615     d wAmbito         s              1a   inz
019800141009     d wCap            s                   inz like(fiora0I0.vaocar)
019900160615     d wCodice         s             10s 0 inz
020000160525     d wErrc           s                   inz like(fnlv13ds.O13err)
020100160525     d wErro           s                   inz like(fnlv13ds.O13err)
020200160525     d wErrr           s                   inz like(fnlv13ds.O13err)
020300160615     d wFiliale        s              3s 0 inz
020400160525     d wfzvc           s                   inz like(fnlv13ds.E13fzv)
020500160525     d wfz1c           s                   inz like(fnlv13ds.E13fz1)
020600160525     d wfz2c           s                   inz like(fnlv13ds.E13fz2)
020700160525     d wfz3c           s                   inz like(fnlv13ds.E13fz3)
020800160525     d wfzvo           s                   inz like(fnlv13ds.E13fzv)
020900160525     d wfz1o           s                   inz like(fnlv13ds.E13fz1)
021000160525     d wfz2o           s                   inz like(fnlv13ds.E13fz2)
021100160525     d wfz3o           s                   inz like(fnlv13ds.E13fz3)
021200160525     d wfzvr           s                   inz like(fnlv13ds.E13fzv)
021300160525     d wfz1r           s                   inz like(fnlv13ds.E13fz1)
021400160525     d wfz2r           s                   inz like(fnlv13ds.E13fz2)
021500160525     d wfz3r           s                   inz like(fnlv13ds.E13fz3)
021600160615     d wGiro           s             10a   inz
021700160615     d wIdAttr         s              5a   inz
021800160615     d wIdCampo        s             10a   inz
021900160615     d wIdMsg          s              7a   inz
022000141009     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
022100141009     d wLocalita       s                   inz like(fiora0I0.vaolor)
022200160525     d wMsgc           s                   inz like(fnlv13ds.O13msg)
022300160525     d wMsgo           s                   inz like(fnlv13ds.O13msg)
022400160525     d wMsgr           s                   inz like(fnlv13ds.O13msg)
022500141009     d wNazione        s                   inz like(fiora0I0.vaonar)
022600141020     d wNrForza        s              3s 0 inz
022700141020     d wNrMsg          s              3s 0 inz
022800141007     d wParm           s            512a   inz
022900141020     d wPor            s              3s 0 inz
023000141009     d wProvincia      s                   inz like(fiora0I0.vaoprr)
023100141021     d wRagSoc         s                   inz like(fiora0I0.vaorsr)
023200140613
023300140618       // -?Parametri per richiamo pgm
023400140613     d RpyIdMsg        s             10i 0
023500140613     d RpyOpCode       s             10i 0
023600140613     d RqsOpCode       s             10i 0
023700140414
023800140414       //--------------------------------------------------------------
023900140414       //?Definizione procedure.
024000140414       //--------------------------------------------------------------
024100160608       // -?Pgm descrizione giro
024200160620     d FIDG09R         pr                  extpgm('FIDG09R')
024300160608     d  kpjba                              likeds(kpjba)
024400160620
024500160620       // -?Recupero ora presunta ritiro
024600160620     d FIOR01R         pr                  extpgm('FIOR01R')
024700160620     d  fior01ds                           likeds(FIOR01DS)
024800160614
024900160614       // -?Pgm gestione note
025000160614     d FIOR06R         pr                  extpgm('FIOR06R')
025100160614     d  kpjba                              likeds(KPJBA)
025200160614     d  fior06ds                           likeds(FIOR06DS)
025300160608
025400141008       // -?Pgm interrogazione anagrafica clienti ritiro
025500141008     d FIOR81R         pr                  extpgm('FIOR81R')
025600141008     d  kpjba                              likeds(KPJBA)
025700141008     d  fior81ds                           likeds(FIOR81DS)
025800141021
025900141021       // -?Pgm controllo indirizzo completo
026000141021     d FNLV13R         pr                  extpgm('FNLV13R')
026100141021     d  kpjba                              likeds(KPJBA)
026200141021     d  fnlv13ds                           likeds(FNLV13DS)
026300141021     d  tisi95ds                           likeds(TISI95DS)
026400141021     d  flgbac                        1a   options(*nopass)
026500141021     d  tisi97ds                           likeds(TISI97DS)
026600141021     d                                     options(*nopass)
026700141009
026800141009       // -?Pgm interrogazione cappario
026900141009     d FNLV14R         pr                  extpgm('FNLV14R')
027000141009     d  kpjba                              likeds(KPJBA)
027100141009     d  fnlv14ds                           likeds(FNLV14DS)
027200170505
027300170505       // -?Interrogazione Cappario DPD
027400170505     d TISIE8R         pr                  extpgm('TISIE8R')
027500170505     d  kpjba                              likeds(KPJBA)
027600170505     d  tisie8ds                           likeds(TISIE8DS)
027700160621
027800160621       // -?Simulazione Delivery
027900160621     d TISI92R         pr                  extpgm('TISI92R')
028000160621     d  kpjba                              likeds(KPJBA)
028100160321
028200160321       // -?Caricamento Filiali in gestione
028300160321     d TRUL31R         pr                  extpgm('TRUL31R')
028400160321     d  kpjba                              likeds(KPJBA)
028500160321     d  trul31ds                           likeds(TRUL31DS)
028600170403
028700170403       // -?Aggiunge/Toglie gg/mm dalla data
028800170403     d XGIOLAV         pr                  extpgm('XGIOLAV')
028900170403     d  xgiolavds                          likeds(xgiolavds)
029000140414
029100140414       //--------------------------------------------------------------
029200140414       //?Definizione prototipi.
029300140414       //--------------------------------------------------------------
029400140618      /copy gaitrasrc/srcprotopr,FIOR000R
029500140618      /copy gaitrasrc/srcprotopr,FIOR001R
029600140613      /copy gaitrasrc/srcprotopr,FIORA00R
029700160622      /copy gaitrasrc/srcprotopr,FIORA01R
029800141007      /copy gaitrasrc/srcprotopr,RTVMSGLANG
029900140416      /copy gaitrasrc/srcprotopi,TRULTAB
030000140416      /copy gaitrasrc/srcprotopr,TRULTAB
030100160608      /copy gaitrasrc/srcprotopr,XSRDA8
030200140414
030300140414       //--------------------------------------------------------------
030400140414       //?Definizione parametri programma.
030500140414       //--------------------------------------------------------------
030600140618     d Fior001_Controller...
030700140414     d                 PI
030800140618     d prmRqsOpCode...
030900140618     d                               10I 0 CONST
031000140618     d prmRpyOpCode...
031100140618     d                               10I 0
031200140618     d prmRpyIdMsg...
031300140618     d                               10I 0
031400140618     d prmRqsFormato...
031500140618     d                               10A   CONST
031600140618     d prmRqsData...
031700140618     d                            32767A   OPTIONS(*VARSIZE)
031800140618     d prmRqsSize...
031900140618     d                               10I 0 CONST
032000140618     d prmRpyFormato...
032100140618     d                               10A   CONST
032200140618     d prmRpyData...
032300140618     d                            32767A   OPTIONS(*VARSIZE)
032400140618     d prmRpySize...
032500140618     d                               10I 0 CONST
032600140618     d prmRpyFormMsg...
032700140618     d                               10A   CONST OPTIONS(*NOPASS)
032800140618     d prmRpyMessage...
032900140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033000140618     d prmRpySizeMsg...
033100140618     d                               10I 0 CONST OPTIONS(*NOPASS)
033200140618     d prmRpyFormForz...
033300140618     d                               10A   CONST OPTIONS(*NOPASS)
033400140618     d prmRpyForzatu...
033500140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033600140618     d prmRpySizeForz...
033700140618     d                               10I 0 CONST OPTIONS(*NOPASS)
033800140618     d prmRpyFormTastiFun...
033900140618     d                               10A   CONST OPTIONS(*NOPASS)
034000140618     d prmRpyTastiFun...
034100140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
034200140618     d prmRpySizeTastiFun...
034300140618     d                               10I 0 CONST OPTIONS(*NOPASS)
034400140618     d prmKPJBA...
034500140618     d                              502A   CONST OPTIONS(*NOPASS)
034600160622
034700160607      //---------------------------------------------------------------
034800160607      //?Definizione key-list.
034900160607      //---------------------------------------------------------------
035000160607       // - File TABEL00F
035100160607     d k03tabel      e ds                  extname(TABEL00F:*key)
035200160607     d                                     prefix(k_)
035300160607
035400140414       //--------------------------------------------------------------
035500140414       //?MAIN.
035600140414       //--------------------------------------------------------------
035700140414
035800140414      /free
035900160616
036000160616       //?Operazioni iniziali
036100160616       exsr RoutInz;
036200160616
036300160616       //?Imposto i dati nella ds di input
036400160616       exsr MoveDs00;
036500160616
036600160616       //?Gestione video
036700160616       DOW  not Fine;
036800160616
036900160616         SELECT;
037000160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
037100160616           exsr VideoPrincipale;
037200160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
037300160616           exsr VideoAltriDati;
037400160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
037500160616           exsr VideoAlertComm;
037600160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
037700160616           exsr VideoAlertConf;
037800160616         WHEN  fior000i.video = FIOR000_VIDEO_WHOISORD;
037900160616           exsr VideoWho;
038000160616         OTHER;
038100160616           Fine = *on;
038200160616         ENDSL;
038300160616
038400160616       ENDDO;
038500140618
038600140414       //?Operazioni finali
038700140414       exsr RoutEnd;
038800160616
038900160616       //--------------------------------------------------------------
039000160616       //?Operazioni iniziali.
039100160616       //--------------------------------------------------------------
039200160616       BEGSR  Routinz;
039300160616
039400160616         *inLR = *on;
039500160616
039600160616       //?Controllo formale dei dati passati
039700160616         exsr ControllaParm;
039800160616
039900160616       //?Imposto i dati utente
040000160616         exsr DatiUtente;
040100160616
040200160616       //?Cerco i dati di Default
040300160616         exsr CaricaDefault;
040400160616
040500160616       ENDSR;
040600160616
040700160616       //--------------------------------------------------------------
040800160616       //?Imposto i campi nella DS di input. (pgm FIOR000R)
040900160616       //--------------------------------------------------------------
041000160616       BEGSR  MoveDs00;
041100160616
041200160616         reset FIOR000I;
041300160616
041400160616         eval-corr fior000i = fior001i;
041500160616         reset fior000i.formato;
041600160616         reset fior000i.versione;
041700160616
041800160616         fior000i.titolo = FIOR001_TITOLO;
041900160616         fior000i.dpoei = GetDesFiliale(fior001i.poei);
042000160616
042100160616         SELECT;
042200160616       //?Imposto i campi per immissione manuale
042300160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
042400160616               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
042500160616           fior000i.mod = FIOR001_MOD_IMMISSIONE;
042600160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
042700160616           dataISO = %date(dataoggi:*iso);
042800160616           dataEUR = dataISO;
042900160616           fior000i.dao = %dec(dataEUR);
043000160616           fior000i.oao = %dec(%time());
043100160616
043200160616           fior000i.tor = ddft.d§DFTtor;
043300160616
043400160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL;
043500160616             fior000i.tco = FIORA00_TIPOCOMORM_TELEFONICO;
043600160616           ENDIF;
043700160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
043800160616             fior000i.tco = FIORA00_TIPOCOMORM_MAIL;
043900160616           ENDIF;
044000160616
044100160616           fior000i.pag = ddft.d§DFTpag;
044200160616           fior000i.ddt = ddft.d§DFTddt;
044300170331           fior000i.dpm = fior000i.dao;
044400170403           fior000i.orr = fior000i.oao / 100;
044500170530
044600170530       //?Imposto i campi per conferma da VAO
044700170530         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_INTERNET;
044800170530           fior000i.mod = FIOR001_MOD_IMMISSIONE;
044900170530           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
045000160616
045100170530       //?Imposto i campi per conferma ORM Fissi
045200170530         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
045300160616           fior000i.mod = FIOR001_MOD_IMMISSIONE;
045400160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
045500170530
045600170530           dataISO = %date(dataoggi:*iso);
045700170530           dataEUR = dataISO;
045800170530           fior000i.dao = %dec(dataEUR);
045900170530           fior000i.oao = %dec(%time());
046000170530           fior000i.tco = FIORA00_TIPOCOMORM_FISSO;
046100160616
046200160616       //?Imposto i campi per Manutenzione
046300160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
046400160616           fior000i.mod = FIOR001_MOD_MANUTENZIONE;
046500160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
046600160616
046700160616         //?Aggancio ORM
046800160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv)
046900160616                  FNORM01L;
047000160616           IF  %found(FNORM01L);
047100160616             fior000i.fao = ORMfao;
047200160616           ENDIF;
047300160616
047400160616         ENDSL;
047500170420
047600170420       //?Resetto tutti i flag booleani
047700170420         reset EmessoAlertComm;
047800170420         reset EmessoAlertConf;
047900170420         reset Fine;
048000170420         reset FineVideo;
048100170420         reset RiemetteVideo;
048200170420         reset wDatiFLO;
048300170420         reset wOkF06F04;
048400170420         reset wOkF06F13;
048500170420         reset wOkF15;
048600160616
048700160616       ENDSR;
048800160616
048900160616       //--------------------------------------------------------------
049000160616       //?Gestione Videata Principale.
049100160616       //--------------------------------------------------------------
049200160616       BEGSR  VideoPrincipale;
049300160616
049400160616         FineVideo = *off;
049500160616
049600160616         DOW  not FineVideo;
049700160616
049800160616           RiemetteVideo = *off;
049900160616
050000160616         //?Imposto i tasti funzione e gli attributi del video
050100160620         //?Se F24 non lo faccio
050200160620           IF  fior000a.TastoVideo <> FIOR000_TASTO_F24;
050300170331             exsr ImpostaVideo;
050400160620           ENDIF;
050500160616
050600160616           RqsOpCode = prmRqsOpCode;
050700160616
050800160616         //?Richiamo pgm gestione video
050900160616           exsr CallVideo;
051000160616
051100160616         //?Se dato ENTER prima di verificare il tasto dato dall'utente
051200160616         //?faccio dei controlli preventivi sui dati immessi a video
051300170418           IF  fior000a.TastoVideo = FIOR000_TASTO_ENTER or
051400170418               fior000a.TastoVideo = FIOR000_TASTO_F06;
051500160616             exsr PrimiControlli;
051600160616           //?Se devo riemettere il video torno su
051700160616             IF  RiemetteVideo;
051800160616               eval-corr fior000i = fior000o;
051900160616               reset fior000i.formato;
052000160616               reset fior000i.versione;
052100160616               iter;
052200160616             ENDIF;
052300160616           ENDIF;
052400160616
052500160616         //?Salvo il flag di ORM commissionato del video
052600160616           savcomm = fior000i.comm;
052700160616
052800160616         //?Eseguo i comandi dati dall'utente
052900160616           exsr Comandi;
053000160616
053100160616         ENDDO;
053200160616
053300160616       ENDSR;
053400160616
053500160616       //--------------------------------------------------------------
053600160616       //?Gestione Window del F05 Altri Dati.
053700160616       //--------------------------------------------------------------
053800160616       BEGSR  VideoAltriDati;
053900160616
054000160616         FineVideo = *off;
054100160616
054200160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
054300160616       //?emettere subito il messaggio di errore
054400160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
054500160616           exsr ControllaDatiVideo;
054600160616         ELSE;
054700160616           reset fiora0M0;
054800160616         ENDIF;
054900160616
055000160616       //?Mi salvo i dati della videata 02
055100160616         savblc = fior000i.blc;
055200160616         savatt = fior000i.att;
055300160616         savmtc = fior000i.mtc;
055400160616         savsto = fior000i.sto;
055500160616         savcgi = fior000i.cgi;
055600160616         savffd = fior000i.ffd;
055700160616
055800160616         DOW  not FineVideo;
055900160616
056000160616         //?Imposto i tasti funzione e gli attributi del video
056100160616           exsr ImpostaVideo;
056200160616
056300160616         //?Emetto la videata
056400160616           exsr CallVideo;
056500160616
056600160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
056700160616         //?quindi devo eseguire i comandi dati dall'utente
056800160616           exsr Comandi;
056900160616
057000160616         ENDDO;
057100160616
057200160616       ENDSR;
057300160616
057400160616       //--------------------------------------------------------------
057500160616       //?Gestione Window del F04 Alert Commissionato.
057600160616       //--------------------------------------------------------------
057700160616       BEGSR  VideoAlertComm;
057800160616
057900160616         FineVideo = *off;
058000160616         EmessoAlertComm = *on;
058100160616
058200160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
058300160616       //?emettere subito il messaggio di errore
058400160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
058500160616           exsr ControllaDatiVideo;
058600160616         ELSE;
058700160616           reset fiora0M0;
058800160616         ENDIF;
058900160616
059000160616       //?Mi salvo i dati della videata 03
059100160616         savmail = fior000i.mail;
059200160616         savsms  = fior000i.sms;
059300160616
059400160616         DOW  not FineVideo;
059500160616
059600160616         //?Imposto i tasti funzione e gli attributi del video
059700160616           exsr ImpostaVideo;
059800160616
059900160616         //?Emetto la videata
060000160616           exsr CallVideo;
060100160616
060200160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
060300160616         //?quindi devo eseguire i comandi dati dall'utente
060400160616           exsr Comandi;
060500160616
060600160616         ENDDO;
060700160616
060800160616       ENDSR;
060900160616
061000160616       //--------------------------------------------------------------
061100160616       //?Gestione Window del F13 Alert Conferma Prenotazione.
061200160616       //--------------------------------------------------------------
061300160616       BEGSR  VideoAlertConf;
061400160616
061500160616         FineVideo = *off;
061600160616         EmessoAlertConf = *on;
061700160616
061800160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
061900160616       //?emettere subito il messaggio di errore
062000160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
062100160616           exsr ControllaDatiVideo;
062200160616         ELSE;
062300160616           reset fiora0M0;
062400160616         ENDIF;
062500160616
062600160616       //?Mi salvo i dati della videata 04
062700160616         savmailconf = fior000i.mailconf;
062800160616         savsmsconf  = fior000i.smsconf;
062900160616
063000160616         DOW  not FineVideo;
063100160616
063200160616         //?Imposto i tasti funzione e gli attributi del video
063300160616           exsr ImpostaVideo;
063400160616
063500160616         //?Emetto la videata
063600160616           exsr CallVideo;
063700160616
063800160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
063900160616         //?quindi devo eseguire i comandi dati dall'utente
064000160616           exsr Comandi;
064100160616
064200160616         ENDDO;
064300160616
064400160616       ENDSR;
064500160616
064600160616       //--------------------------------------------------------------
064700160616       //?Gestione Window Tipo Ordinante ORM Commissionato.
064800160616       //--------------------------------------------------------------
064900160616       BEGSR  VideoWho;
065000160616
065100160616         FineVideo = *off;
065200160616
065300160616         DOW  not FineVideo;
065400160616
065500160616         //?Emetto la videata
065600160616           exsr CallVideo;
065700160616
065800160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
065900160616         //?quindi devo eseguire i comandi dati dall'utente
066000160616           exsr Comandi;
066100160616
066200160616         ENDDO;
066300160616
066400160616       ENDSR;
066500160616
066600160616       //--------------------------------------------------------------
066700160616       //?Operazioni finali.
066800160616       //--------------------------------------------------------------
066900160616       BEGSR  RoutEnd;
067000160616
067100160616         %subst(prmRpyData:1:prmRpySize) = fior000o;
067200160616
067300160616         return;
067400160616
067500160616       ENDSR;
067600140414
067700140414       //--------------------------------------------------------------
067800140414       //?Controllo formale dei dati passati.
067900140414       //--------------------------------------------------------------
068000140414       BEGSR  ControllaParm;
068100140414
068200140618       //?OpCode
068300160321         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_TEL and
068400160321             prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_MAIL and
068500170530             prmRqsOpCode <> FIOR001_RQSOPCODE_CONFERMA_FISSI and
068600141010             prmRqsOpCode <> FIOR001_RQSOPCODE_CONFERMA_INTERNET and
068700141010             prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE;
068800140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
068900140618           prmRpyIdMsg  = FIOR001_ESITO_RQSOPCODE_SCONOSCIUTO;
069000140618           exsr RoutEnd;
069100140618         ENDIF;
069200140618
069300140618       //?Formato e size RQS
069400140618         IF  prmRqsSize > 0;
069500140618         ELSE;
069600140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
069700140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
069800140618           exsr RoutEnd;
069900140618         ENDIF;
070000140618         IF  prmRqsFormato = fior001i.formato;
070100140618           fior001i = %SUBST(prmRqsData:1:prmRqsSize);
070200140618         ELSE;
070300140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
070400140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
070500140618           exsr RoutEnd;
070600140618         ENDIF;
070700140618
070800140618       //?Formato e size RPY
070900140618         IF  prmRpySize > 0;
071000140618         ELSE;
071100140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
071200140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
071300140618           exsr RoutEnd;
071400140618         ENDIF;
071500140618         IF  prmRpyFormato = fior000o.formato;
071600140618         ELSE;
071700140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
071800140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
071900140618           exsr RoutEnd;
072000140618         ENDIF;
072100140618
072200140618       //?Formato e size MSG
072300140618         IF  prmRpySizeMsg > 0;
072400140618         ELSE;
072500140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
072600140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
072700140618           exsr RoutEnd;
072800140618         ENDIF;
072900140618         IF  prmRpyFormMsg = fiora0m0.formato;
073000160531           fiora0m0 = %SUBST(prmRpyMessage:1:prmRpySizeMsg);
073100140618         ELSE;
073200140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
073300140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
073400140618           exsr RoutEnd;
073500140618         ENDIF;
073600140618
073700140618       //?Formato e size Forzature
073800140618         IF  prmRpySizeForz > 0;
073900140618         ELSE;
074000140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
074100140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
074200140618           exsr RoutEnd;
074300140618         ENDIF;
074400140618         IF  prmRpyFormForz = fiora0f0.formato;
074500160531           fiora0f0 = %SUBST(prmRpyForzatu:1:prmRpySizeForz);
074600140618         ELSE;
074700140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
074800140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
074900140618           exsr RoutEnd;
075000140618         ENDIF;
075100140618
075200140618       //?Formato e size Tasti Funzione
075300140618         IF  prmRpySizeTastiFun > 0;
075400140618         ELSE;
075500140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
075600140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
075700140618           exsr RoutEnd;
075800140618         ENDIF;
075900140618         IF  prmRpyFormTastiFun = fior000a.formato;
076000140618         ELSE;
076100140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
076200140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
076300140618           exsr RoutEnd;
076400140618         ENDIF;
076500140414
076600140414       ENDSR;
076700160321
076800160321       //--------------------------------------------------------------
076900160321       //?Imposto i dati Utente.
077000160321       //--------------------------------------------------------------
077100160321       BEGSR  DatiUtente;
077200160321
077300160321         clear TRUL31DS;
077400160321         clear kpjbu;
077500160321         trul31ds.I31abi = fior001i.abiute;
077600160321         trul31ds.I31cpo = fior001i.filute;
077700160321         TRUL31R (kpjba:trul31ds);
077800160321         IF  trul31ds.O31pog <= *zeros;
077900160321           leavesr;
078000160321         ENDIF;
078100160321
078200160321       ENDSR;
078300140618
078400140618       //--------------------------------------------------------------
078500140619       //?Carico i dati di Default.
078600140618       //--------------------------------------------------------------
078700140619       BEGSR  CaricaDefault;
078800140619
078900140619         prmRpyOpCode = FIOR001_RPYOPCODE_DONE;
079000140619         prmRpyIdMsg  = FIOR001_ESITO_OK;
079100140619
079200140619       //?Data del giorno
079300140619         dataoggi = %dec(%date());
079400140618
079500140618       //?Reperimento tabella DFT
079600160321         keytabella = %editc(fior001i.poei:'X');
079700140618         FOR conta = 1 to 2;
079800140618         //?prima provo con la filiale emissione
079900140618           clear dDFT;
080000140618           IF  getTabella (c_tntbe:'DFT':keytabella:
080100140618                           *blanks:*blanks:*blanks:
080200140618                           *omit:*omit:*omit:*omit:*omit:
080300140618                           *omit:*omit:*omit:*omit:*omit:
080400140618                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
080500140618               = *zeros and ds_TNTBE.TBEatb = *blanks;
080600140618             dDFT = ds_TNTBE.TBEuni;
080700140618             leave;
080800140618           ENDIF;
080900140618         //?se non trovo con filiale emissione
081000140618         //?provo con '999'
081100140618           clear dDFT;
081200140618           keytabella = '999';
081300140618         ENDFOR;
081400160607
081500160607       //?Impostazione campi "fissi"
081600160607         k_TBLkut = 1;
081700140618
081800140618       ENDSR;
081900160616
082000160616       //--------------------------------------------------------------
082100160616       //?Imposto i tasti funzione e gli attributi dei campi.
082200160616       //--------------------------------------------------------------
082300160616       BEGSR  ImpostaVideo;
082400160616
082500160616         reset FIOR000A;
082600160616
082700160616       //?Metto la filiale ritiro in un campo numerico
082800160616         clear wPor;
082900160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
083000160616           wPor = %int(fior000i.por);
083100160616         ENDIF;
083200160616
083300160616         exsr TastiFunzione;
083400160616
083500160616         SELECT;
083600160616
083700160616       //?Attributi validi solo per immissione manuale
083800160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
083900160616               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
084000160616         //?Filiale Emissione
084100160616           wIdCampo = 'ATTRDPOEI';
084200160616           wIdAttr = FIOR001_ATTR_BLUE;
084300160616           IF  fior001i.poei = fior001i.poe;
084400160616             wIdAttr = FIOR001_ATTR_ND;
084500160616           ENDIF;
084600160616           exsr Attributi;
084700160616           wIdCampo = 'ATTRPOEI';
084800160616           wIdAttr = FIOR001_ATTR_PINK;
084900160616           IF  fior001i.poei = fior001i.poe;
085000160616             wIdAttr = FIOR001_ATTR_ND;
085100160616           ENDIF;
085200160616           exsr Attributi;
085300160616         //?Tipo Comunicazione ORM
085400160616           wIdCampo = 'ATTRTCO';
085500160616           wIdAttr = FIOR001_ATTR_HIUL;
085600160616           exsr Attributi;
085700160616         //?Tipo ORM
085800160616           wIdCampo = 'ATTRTOR';
085900160616           wIdAttr = FIOR001_ATTR_HIUL;
086000160616           exsr Attributi;
086100160616         //?Mittente
086200160616           wIdCampo = 'ATTRCRA';
086300160616           wIdAttr = FIOR001_ATTR_HIUL;
086400160616           exsr Attributi;
086500160616           //?Se codice mittente inserito devo bloccare i dati del mittente
086600160616           IF  fior000i.cra2 > 0;
086700160616             wIdCampo = 'ATTRCRA1';
086800160616             wIdAttr = FIOR001_ATTR_PR;
086900160616             exsr Attributi;
087000160616           ENDIF;
087100160616           //?Se codice mittente non inserito devo sbloccare i dati del mittente
087200160616           IF  fior000i.cra2 = 0;
087300160616             wIdCampo = 'ATTRCRA1';
087400160616             wIdAttr = FIOR001_ATTR_HIUL;
087500160616             exsr Attributi;
087600160616           ENDIF;
087700160616         //?Altri dati Mittente
087800160616           wIdCampo = 'ATTRCRA2';
087900160616           wIdAttr = FIOR001_ATTR_HIUL;
088000160616           exsr Attributi;
088100160616         //?Quantità
088200160616           wIdCampo = 'ATTRQTA';
088300160616           wIdAttr = FIOR001_ATTR_HIUL;
088400160616           exsr Attributi;
088500160616         //?Natura merce
088600160616           wIdCampo = 'ATTRNAM';
088700160616           wIdAttr = FIOR001_ATTR_HIUL;
088800160616           exsr Attributi;
088900160616         //?Riferimento Alfa
089000160616           wIdCampo = 'ATTRRFA';
089100160616           wIdAttr = FIOR001_ATTR_HIUL;
089200160616           exsr Attributi;
089300160616         //?Ordinante
089400160616           wIdCampo = 'ATTRCOR';
089500160616           wIdAttr = FIOR001_ATTR_HIUL;
089600160616           exsr Attributi;
089700160616           //?Se codice ordinante inserito devo bloccare i dati dell'ordinante
089800160616           IF  fior000i.cor2 > 0;
089900160616             wIdCampo = 'ATTRCOR1';
090000160616             wIdAttr = FIOR001_ATTR_PR;
090100160616             exsr Attributi;
090200160616           ENDIF;
090300160616           //?Se codice ordinante non inserito devo sbloccare i dati dell'ordinante
090400160616           IF  fior000i.cor2 = 0;
090500160616             wIdCampo = 'ATTRCOR1';
090600160616             wIdAttr = FIOR001_ATTR_HIUL;
090700160616             exsr Attributi;
090800160616           ENDIF;
090900160616         //?Destinatario
091000160616           wIdCampo = 'ATTRCRC';
091100160616           wIdAttr = FIOR001_ATTR_HIUL;
091200160616           exsr Attributi;
091300160616           wIdCampo = 'ATTRCRC1';
091400160616           wIdAttr = FIOR001_ATTR_HIUL;
091500160616           exsr Attributi;
091600160616           SELECT;
091700160616           //?Blocco del destinatario blocco TUTTO il destinatario
091800160616           WHEN  dorm01.§ORMfd = 'S' and (fior000i.crc2 > 0 or
091900160616                 fior000i.rsc > *blanks);
092000160616             wIdCampo = 'ATTRCRC';
092100160616             wIdAttr = FIOR001_ATTR_PR;
092200160616             exsr Attributi;
092300160616             wIdCampo = 'ATTRCRC1';
092400160616             wIdAttr = FIOR001_ATTR_PR;
092500160616             exsr Attributi;
092600160616           //?Se codice destinatario inserito devo bloccare i dati del destinatario
092700160616           WHEN  fior000i.crc2 > 0;
092800160616             wIdCampo = 'ATTRCRC1';
092900160616             wIdAttr = FIOR001_ATTR_PR;
093000160616             exsr Attributi;
093100160616           //?Se codice destinatario non inserito devo sbloccare i dati del destinatario
093200160616           WHEN  fior000i.crc2 = 0;
093300160616             wIdCampo = 'ATTRCRC1';
093400160616             wIdAttr = FIOR001_ATTR_HIUL;
093500160616             exsr Attributi;
093600160616           ENDSL;
093700160616         //?Filiale Ritiro
093800160616           wIdCampo = 'ATTRPOR';
093900160616           wIdAttr = FIOR001_ATTR_HIUL;
094000160616           exsr Attributi;
094100160616         //?Filiale Consegna
094200160616           wIdCampo = 'ATTRPOC';
094300160616           wIdAttr = FIOR001_ATTR_HIUL;
094400160616           exsr Attributi;
094500160616         //?Nota1 e Nota2
094600160616           wIdCampo = 'ATTRNOTA';
094700160616           wIdAttr = FIOR001_ATTR_HIUL;
094800160616           exsr Attributi;
094900160616         //?Chi Paga
095000160616           wIdCampo = 'ATTRPAG';
095100160616           wIdAttr = FIOR001_ATTR_HIUL;
095200160616           exsr Attributi;
095300160616         //?Commissionato
095400160616           wIdCampo = 'ATTRCOMM';
095500160616           SELECT;
095600160616         //?Proteggo se ORM con ricevuta di ritiro
095700160616           WHEN  dorm01.§ORsrm = 'S';
095800160616             wIdAttr = FIOR001_ATTR_PRHI;
095900160616         //?Proteggo se ORM con preavviso mail
096000160616           WHEN  dorm01.§ORMpre = 'S';
096100160616             wIdAttr = FIOR001_ATTR_PRHI;
096200160616           OTHER;
096300160616             wIdAttr = FIOR001_ATTR_HIUL;
096400160616           ENDSL;
096500160616           exsr Attributi;
096600160616         //?Numero ORM
096700160616           wIdCampo = 'ATTRNOR';
096800160616           wIdAttr = FIOR001_ATTR_ND;
096900160616           exsr Attributi;
097000160616           wIdCampo = 'ATTRFASE';
097100160616           wIdAttr = FIOR001_ATTR_ND;
097200160616           exsr Attributi;
097300160616           wIdCampo = 'ATTRCAU';
097400160616           wIdAttr = FIOR001_ATTR_ND;
097500160616           exsr Attributi;
097600170330         //?Non visualizzo la data ritiro se non presente
097700170330           IF  fior000i.dar = *zeros;
097800170330             wIdCampo = 'ATTRDAR';
097900170330             wIdAttr = FIOR001_ATTR_ND;
098000170330             exsr Attributi;
098100170330           ENDIF;
098200170403         //?Se non ho ancora calcolato la data ritiro non faccio
098300170403         //?vedere gli orari servizio
098400170403           IF  fior000i.dar = *zeros;
098500170403             wIdCampo = 'ATTRORSR';
098600170403             wIdAttr = FIOR001_ATTR_ND;
098700170403             exsr Attributi;
098800170403           ENDIF;
098900170420         //?Ultimo ORM immesso
099000170420           IF  fior000i.poeu = *zeros and fior000i.noru = *zeros;
099100170420             wIdCampo = 'ATTRNORU';
099200170420             wIdAttr = FIOR001_ATTR_ND;
099300170420             exsr Attributi;
099400170420           ENDIF;
099500160616         //?Campi videata TastoF05
099600160616           wIdCampo = 'ATTRBLC';
099700160616           wIdAttr = FIOR001_ATTR_HIUL;
099800160616           exsr Attributi;
099900160616           wIdCampo = 'ATTRATT';
100000160616           wIdAttr = FIOR001_ATTR_HIUL;
100100160616           exsr Attributi;
100200160616           wIdCampo = 'ATTRMTC';
100300160616           wIdAttr = FIOR001_ATTR_HIUL;
100400160616           exsr Attributi;
100500160616           wIdCampo = 'ATTRSTO';
100600160616           wIdAttr = FIOR001_ATTR_HIUL;
100700160616           exsr Attributi;
100800160616           wIdCampo = 'ATTRCGI';
100900160616           wIdAttr = FIOR001_ATTR_HIUL;
101000160616           exsr Attributi;
101100160616           wIdCampo = 'ATTRFFD';
101200160616           wIdAttr = FIOR001_ATTR_HIUL;
101300160616           exsr Attributi;
101400160616         //?Campi videata TastoF04
101500160616           wIdCampo = 'ATTRSMS';
101600160616           wIdAttr = FIOR001_ATTR_HIUL;
101700160616           exsr Attributi;
101800160616           wIdCampo = 'ATTRMAIL';
101900160616           wIdAttr = FIOR001_ATTR_HIUL;
102000160616           exsr Attributi;
102100160616         //?Campi videata TastoF13
102200160616           wIdCampo = 'ATTRSMSC';
102300160616           wIdAttr = FIOR001_ATTR_HIUL;
102400160616           exsr Attributi;
102500160616           wIdCampo = 'ATTRMAILC';
102600160616           wIdAttr = FIOR001_ATTR_HIUL;
102700160616           exsr Attributi;
102800160616
102900160616         //?Se tipo ORM multiplo
103000160616         //?e ho i dati del destinatario
103100160616         //?imposto in automatico che ORM singolo
103200160616         //?ma NON se c'è errore sul destinatario o sul tipo ORM
103300160616         IF  fior000i.tor = FIORA00_TIPOORM_MULTIPLO and
103400160616             ((fior000i.crc1 > 0 and fior000i.crc2 > 0) or
103500160616              (fior000i.rsc <> *blanks)) and
103600160616               %lookup('VAOCRC':fiora0M0.skIdCampo) = 0 and
103700160616               %lookup('VAOTOR':fiora0M0.skIdCampo) = 0;
103800160616           fior000i.tor = FIORA00_TIPOORM_SINGOLO;
103900160616         ENDIF;
104000160616
104100160616       //?Attributi validi solo conferma da Internet
104200160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_INTERNET;
104300160616
104400160616         //?Non visualizzo il numero ORM
104500160616           wIdCampo = 'ATTRNOR';
104600160616           wIdAttr = FIOR001_ATTR_ND;
104700160616           exsr Attributi;
104800160616
104900160616         //?Non visualizzo la fase
105000160616           wIdCampo = 'ATTRFASE';
105100160616           wIdAttr = FIOR001_ATTR_ND;
105200160616           exsr Attributi;
105300160616
105400160616         //?Non visualizzo la causale
105500160616           wIdCampo = 'ATTRCAU';
105600160616           wIdAttr = FIOR001_ATTR_ND;
105700160616           exsr Attributi;
105800160616
105900160616         //?Se non ho ancora calcolato la data ritiro non faccio
106000160616         //?vedere gli orari servizio
106100160616           IF  fior000i.dar = *zeros;
106200160616             wIdCampo = 'ATTRORSR';
106300160616             wIdAttr = FIOR001_ATTR_ND;
106400160616             exsr Attributi;
106500160616           ENDIF;
106600160616
106700160616         //?Se ho il numero dell'ultimo ORM immesso lo visualizzo
106800160616           IF  fior000i.noru > 0;
106900160616             wIdCampo = 'ATTRNORU';
107000160616             wIdAttr = FIOR001_ATTR_ND;
107100160616             exsr Attributi;
107200160616           ENDIF;
107300160616
107400160616         ENDSL;
107500160616
107600160616       //?Attributi validi sui tasti funzione
107700160616       //?se ho dei dati relativi alla videata del F5
107800160616         IF  fior000i.blc > 0 or fior000i.att > 0 or
107900160616             fior000i.mtc > 0 or fior000i.sto > 0 or
108000160616             fior000i.cgi <> *blanks or
108100160616            (fior000i.ffd <> *blanks and fior000i.ffd <> 'N');
108200160616           wIdCampo = 'ATTRF05';
108300160616           wIdAttr = FIOR001_ATTR_HI;
108400160616           exsr Attributi;
108500160616         ENDIF;
108600160616       //?se ho dei dati relativi alla videata del F04
108700160616         IF  fior000i.mail <> *blanks or
108800160616             fior000i.sms <> *blanks;
108900160616           wIdCampo = 'ATTRF04';
109000160616           wIdAttr = FIOR001_ATTR_HI;
109100160616           exsr Attributi;
109200160616         ENDIF;
109300160616       //?Se ho dei dati relativi alle note AUT F17
109400160616         IF  fior000i.nor > 0;
109500160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv) FNORT01L;
109600160616           IF  %found(FNORT01L);
109700160616             wIdCampo = 'ATTRF17';
109800160616             wIdAttr = FIOR001_ATTR_HI;
109900160616             exsr Attributi;
110000160616           ENDIF;
110100160616         ENDIF;
110200160616       //?Se ho dei dati relativi alle note ORM F18
110300160616         IF  fior000i.nor > 0;
110400160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv) FNORN01L;
110500160616           IF  %found(FNORN01L);
110600160616             wIdCampo = 'ATTRF18';
110700160616             wIdAttr = FIOR001_ATTR_HI;
110800160616             exsr Attributi;
110900160616           ENDIF;
111000160616         ENDIF;
111100160616
111200160616       //?Decodifico i dati del video
111300160616         exsr Decodifica;
111400160616
111500160616       ENDSR;
111600160616
111700160616       //--------------------------------------------------------------
111800160616       //?Richiamo pgm gestione video.
111900160616       //--------------------------------------------------------------
112000160616       BEGSR  CallVideo;
112100160616
112200160616       //?Richiamo pgm gestione video
112300160616         MONITOR;
112400160616           Fior000_Video( RqsOpCode
112500160616                        : RpyOpCode
112600160616                        : RpyIdMsg
112700160616                        : fior000i.formato
112800160616                        : fior000i
112900160616                        : %SIZE(fior000i)
113000160616                        : fior000o.formato
113100160616                        : fior000o
113200160616                        : %SIZE(fior000o)
113300160616                        : FIORA0M0.formato
113400160616                        : FIORA0M0
113500160616                        : %size(FIORA0M0)
113600160616                        : FIORA0F0.formato
113700160616                        : FIORA0F0
113800160616                        : %size(FIORA0F0)
113900160616                        : FIOR000A.formato
114000160616                        : FIOR000A
114100160616                        : %size(FIOR000A)
114200160616                        );
114300160616           ON-ERROR;
114400160616           RpyOpCode = FIOR000_RPYOPCODE_ERROR;
114500160616         ENDMON;
114600160616
114700160616       //?Se non è DONE esco
114800160616         IF RpyOpCode < FIOR000_RPYOPCODE_DONE;
114900160616           exsr RoutEnd;
115000160616         ENDIF;
115100160616
115200160616       ENDSR;
115300160616
115400160616       //--------------------------------------------------------------
115500160616       //?Comandi dati sul video.
115600160616       //--------------------------------------------------------------
115700160616       BEGSR  Comandi;
115800160616
115900160616       //?imposto i dati dalla DS di Output del FIOR000R
116000160616       //?alla ds di input del FIOR000R
116100160616         eval-corr fior000i = fior000o;
116200160616         reset fior000i.formato;
116300160616         reset fior000i.versione;
116400160616
116500160616         SELECT;
116600160616
116700160616       //?ENTER = Controlli
116800160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER;
116900160616           exsr TastoEnter;
117000160616
117100160616       //?F04 = Alert Ritiro Merce ORM Commissionato
117200160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F04;
117300160616           exsr TastoF04;
117400160616
117500160616       //?F05 = Altri dati
117600160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F05;
117700160616           exsr TastoF05;
117800160616
117900160616       //?F06 = Conferma
118000160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F06;
118100160616           exsr TastoF06;
118200160616
118300160616       //?F07 = Mittente
118400160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F07;
118500160616           exsr TastoF07;
118600160616
118700160616       //?F08 = Ordinante
118800160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F08;
118900160616           exsr TastoF08;
119000160616
119100160616       //?F09 = Destinatario
119200160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F09;
119300160616           exsr TastoF09;
119400160616
119500160616       //?F10 = Copia Numero Telefono
119600160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F10;
119700160616           exsr TastoF10;
119800160616
119900160616       //?F11 = Volume
120000160616       //?viene gestito nel pgm di gestione video fior000r
120100160616       //?perchè il pgm richiamato da F11 emette una WIN
120200160616       //?che se emessa da questo pgm non mantiene attiva
120300160616       //?la videata dell'ORM
120400160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F11;
120500160616
120600160616       //?F12 = Ritorno al chiamante
120700160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F12;
120800160616           exsr TastoF12;
120900160616
121000160616       //?F13 = Alert Conferma Prenotazione Ritiro
121100160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F13;
121200160616           exsr TastoF13;
121300160620
121400160620       //?F14 = Visualizza Orari Servizio
121500160621       //?viene gestito nel pgm di gestione video fior000r
121600160621       //?perchè il pgm richiamato da F11 emette una WIN
121700160621       //?che se emessa da questo pgm non mantiene attiva
121800160621       //?la videata dell'ORM
121900160621       //?qua gestisco solo gli errori
122000160620         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F14;
122100160620           exsr TastoF14;
122200170505
122300170505       //?F15 = Interrogazione Cappario DPD
122400170505       //?attivato da un messaggio di errore
122500170505         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F15;
122600170505           exsr TastoF15;
122700160616
122800160616       //?F18 = Note
122900160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F18;
123000160616           exsr TastoF18;
123100160621
123200160621       //?F20 = Simulazione Delivery
123300160621         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F20;
123400160621           exsr TastoF20;
123500160616
123600160616       //?F24 = Altri tasti
123700160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F24;
123800160616           exsr TastoF24;
123900170330
124000170330       //?RollUp = Tolgo 1 GG alla data Ritiro
124100170330         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ROLL_UP;
124200170330           exsr TastoRollUp;
124300170330
124400170330       //?RollDown = Aggiungo 1 GG alla data Ritiro
124500170330         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ROLL_DOWN;
124600170330           exsr TastoRollDown;
124700160616
124800160616         ENDSL;
124900160616
125000160616       ENDSR;
125100140618
125200140618       //--------------------------------------------------------------
125300140618       //?Imposto i tasti funzione.
125400140618       //--------------------------------------------------------------
125500141008       BEGSR  TastiFunzione;
125600140618
125700160616         fior000a.tastifun = *all'0';
125800160616
125900160615         SELECT;
126000160615
126100160615       //?Tasti funzione su videata principale
126200160616         WHEN fior000i.video = FIOR000_VIDEO_PRINCIPALE;
126300160615           clear rigaTF1;
126400160615           clear rigaTF2;
126500160615           clear rigaTF3;
126600160615           clear posrtf1;
126700160615           clear posrtf2;
126800160615           clear posrtf3;
126900160615           clear pos;
127000160615           clear posda;
127100160616         //?Tasti presenti nella riga Fissa
127200160615         //?F4 Alert
127300160616         //?solo in immissione
127400160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
127500160616               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
127600160616             %subst(fior000a.tastifun:4:1) = '1';
127700160616           ENDIF;
127800160615         //?F05 Altri dati
127900160615           %subst(fior000a.tastifun:5:1) = '1';
128000160615         //?F07 Int.Mittente
128100160615           %subst(fior000a.tastifun:7:1) = '1';
128200160615         //?F08 Int.Ordinante
128300160615           %subst(fior000a.tastifun:8:1) = '1';
128400160615         //?F09 Int.Destinatario
128500160615           %subst(fior000a.tastifun:9:1) = '1';
128600160615         //?F12 Ritorno
128700160615           %subst(fior000a.tastifun:12:1) = '1';
128800160616         //?F17 Note Aut
128900160616         //?solo in manutenzione
129000160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
129100160616             %subst(fior000a.tastifun:17:1) = '1';
129200160616           ENDIF;
129300160615         //?F18 Note
129400160615           %subst(fior000a.tastifun:18:1) = '1';
129500160616         //?Tasti presenti nella riga Mobile
129600160616         //?F02 Dati DPD
129700160616         //?solo in manutenzione
129800160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
129900160616             %subst(fior000a.tastifun:2:1) = '1';
130000160616             tastof = FIOR001_F02;
130100160616             exsr RieTastiFun;
130200160616           ENDIF;
130300160615         //?F11 Volume
130400160615           %subst(fior000a.tastifun:11:1) = '1';
130500160615           tastof = FIOR001_F11;
130600160615           exsr RieTastiFun;
130700160615         //?F06 Conferma
130800160615           %subst(fior000a.tastifun:6:1) = '1';
130900160615           tastof = FIOR001_F06;
131000160615           exsr RieTastiFun;
131100160616         //?F13 Conferma Prenotazione Ritiro
131200160616         //?solo in immissione
131300160615           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
131400160615               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
131500160615             %subst(fior000a.tastifun:13:1) = '1';
131600160615             tastof = FIOR001_F13;
131700160615             exsr RieTastiFun;
131800160615           ENDIF;
131900160615         //?F14 Orari Servizio
132000160615           %subst(fior000a.tastifun:14:1) = '1';
132100160615           tastof = FIOR001_F14;
132200160615           exsr RieTastiFun;
132300160616         //?F20 Simulazione
132400160616         //?solo in immissione
132500160615           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
132600160615               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
132700160615             %subst(fior000a.tastifun:20:1) = '1';
132800160615             tastof = FIOR001_F20;
132900160615             exsr RieTastiFun;
133000160615           ENDIF;
133100160616         //?F16 Chiusura
133200160616         //?solo in manutenzione
133300160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
133400160616             %subst(fior000a.tastifun:16:1) = '1';
133500160616             tastof = FIOR001_F16;
133600160616             exsr RieTastiFun;
133700160616           ENDIF;
133800160616         //?F19 Proposte di variazione
133900160616         //?solo in manutenzione
134000160616         // ------>>>>
134100160616         //?F21 Dirottamento
134200160616         //?solo in manutenzione
134300160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
134400160616             IF  fior000i.fao < FIOR001_ORM_ASSEGNATO and
134500160616                 fior000i.tor <> 'P' and
134600160616                 %lookup(%editc(wPor:'X'):trul31ds.pog) > 0;
134700160616               %subst(fior000a.tastifun:21:1) = '1';
134800160616               tastof = FIOR001_F21;
134900160616               exsr RieTastiFun;
135000160616             ENDIF;
135100160616           ENDIF;
135200160615         //?F15 Cappario DPD
135300160616         //?viene attivato solo se necessario
135400160616         //?non è scritto nelle righe dei tasti funzione
135500160616           IF  wOkF15;
135600160616             %subst(fior000a.tastifun:15:1) = '1';
135700160616           ENDIF;
135800140618
135900160615         //?Imposto il campo relativo al tasto F24
136000160615           SELECT;
136100160615             WHEN  posrtf2 <> 0 and posrtf3 <> 0;
136200160615               nrighe = 3;
136300160615               fior000a.rigatf24 = FIOR001_F24_1_3;
136400160615             WHEN  posrtf2 <> 0 and posrtf3 = 0;
136500160615               nrighe = 2;
136600160615               fior000a.rigatf24 = FIOR001_F24_1_2 ;
136700160615             OTHER;
136800160615               nrighe = 1;
136900160615               clear fior000a.rigatf24;
137000160615           ENDSL;
137100140618
137200160616         //?Imposto il campo relativo ai tasti mobili
137300160616           fior000a.rigatfmob = rigatf1;
137400160616           contartf = 1;
137500170330
137600170330         //?Attivo RollUp e RollDown in immissione e manutenzione
137700170330           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
137800170330               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
137900170330               prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
138000170330             %subst(fior000a.tastifun:25:1) = '1';
138100170330             %subst(fior000a.tastifun:26:1) = '1';
138200170330           ENDIF;
138300160615
138400160615       //?Tasti funzione su videata F5
138500160615       //?sempre attivi
138600160616         WHEN fior000i.video = FIOR000_VIDEO_ALTRIDATI;
138700160615         //?F06 Conferma
138800160615           %subst(fior000a.tastifun:6:1) = '1';
138900160615         //?F12 Ritorno
139000160615           %subst(fior000a.tastifun:12:1) = '1';
139100160615
139200160615       //?Tasti funzione su videata F4
139300160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
139400160616         //?F06 Conferma
139500160616           %subst(fior000a.tastifun:6:1) = '1';
139600160616         //?F10 Copia Telefono
139700160616           %subst(fior000a.tastifun:10:1) = '1';
139800160616         //?F12 Ritorno
139900160616           %subst(fior000a.tastifun:12:1) = '1';
140000160615
140100160615       //?Tasti funzione su videata F13
140200160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
140300160616         //?F06 Conferma
140400160616           %subst(fior000a.tastifun:6:1) = '1';
140500160616         //?F10 Copia Telefono
140600160616           %subst(fior000a.tastifun:10:1) = '1';
140700160616         //?F12 Ritorno
140800160616           %subst(fior000a.tastifun:12:1) = '1';
140900160615
141000160615         ENDSL;
141100140618
141200140618       ENDSR;
141300140618
141400140618       //--------------------------------------------------------------
141500140618       //?Riempo le righe dei tasti Funzionali.
141600140618       //--------------------------------------------------------------
141700141008       BEGSR  RieTastiFun;
141800140618
141900140618         pos = %scan('#':tastof);
142000140618
142100140618       //?Riga 1
142200140618         posrtf1 += pos;
142300140618         IF  posrtf1 <= 61;
142400140618           %subst(rigatf1:(posrtf1-pos+1):(pos)) =
142500140618           %subst(tastof:1:(pos-1));
142600140618           leavesr;
142700140618         ENDIF;
142800140618
142900140618       //?Riga 2
143000140618         posrtf2 += pos;
143100140618         IF  posrtf2 <= 61;
143200140618           %subst(rigatf2:(posrtf2-pos+1):(pos)) =
143300140618           %subst(tastof:1:(pos-1));
143400140618           leavesr;
143500140618         ENDIF;
143600140618
143700140618       //?Riga 3
143800140618         posrtf3 += pos;
143900140618         IF  posrtf3 <= 61;
144000140618           %subst(rigatf3:(posrtf3-pos+1):(pos)) =
144100140618           %subst(tastof:1:(pos-1));
144200140618           leavesr;
144300140618         ENDIF;
144400140618
144500140618       ENDSR;
144600140618
144700140618       //--------------------------------------------------------------
144800140618       //?Imposto gli attributi del video.
144900140618       //--------------------------------------------------------------
145000141008       BEGSR  Attributi;
145100140618
145200140618       //?Se ho già caricato 99 Attributi esco
145300140618         IF  fior000a.nrAttr = 99;
145400140618           exsr RoutEnd;
145500140618           clear fior000a.nrAttr;
145600140618         ENDIF;
145700140618
145800140618         fior000a.nrAttr += 1;
145900140618         fior000a.skIdAttr(fior000a.nrAttr) = wIdAttr;
146000140618         fior000a.skIdCampo(fior000a.nrAttr) = wIdCampo;
146100140618
146200140618       ENDSR;
146300160616
146400160616       //--------------------------------------------------------------
146500160616       //?Decodifico i campi del video.
146600160616       //--------------------------------------------------------------
146700160616       BEGSR  Decodifica;
146800160616
146900160616       //?Imposto la descrizione del tipo ORM
147000160616         keytabella = fior000i.tor;
147100160616         exsr DecoTipoOrm;
147200160616
147300160616       //?Imposto la descrizione del tipo comunicazione ORM
147400160616         keytabella = fior000i.tco;
147500160616         exsr DecoTipoComOrm;
147600160616
147700160616       //?Imposto la descrizione della filiale ritiro
147800160616         clear fior000i.dpor;
147900160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
148000160616           wFiliale = %int(fior000i.por);
148100160616           fior000i.dpor = GetDesFiliale(wFiliale);
148200160616         ENDIF;
148300160616
148400160616       //?Imposto la descrizione della filiale consegna
148500160616         clear fior000i.dpoc;
148600160616         IF  fior000i.poc <> *blanks and fior000i.poc > *zeros;
148700160616           wFiliale = %int(fior000i.poc);
148800160616           fior000i.dpoc = GetDesFiliale(wFiliale);
148900160616         ENDIF;
149000160616
149100160616       //?Imposto la descrizione del giro
149200160616         clear fior000i.dcgi;
149300160616         IF  fior000i.cgi <> *blanks and
149400160616             fior000i.por <> *blanks and fior000i.por > *zeros;
149500160616           wFiliale = %int(fior000i.por);
149600160616           fior000i.dcgi = GetDesGiro(fior000i.cgi:wFiliale);
149700160616         ENDIF;
149800160620
149900160620       //?Imposto la descrizione dello stato
150000160620         keytabella = %editc(fior000i.sto:'X');
150100160620         exsr DecoStatoOrm;
150200160616
150300160616       ENDSR;
150400160616
150500160616       //--------------------------------------------------------------
150600160616       //?Controlli da fare prima della gestione dei tasti video.
150700160616       //--------------------------------------------------------------
150800160616       BEGSR  PrimiControlli;
150900160616
151000160616       //?Controlli sul mittente
151100160616         exsr Mittente;
151200160616         IF  RiemetteVideo;
151300160616           leavesr;
151400160616         ENDIF;
151500160616
151600170330       //?Controlli sulla data pronta merce
151700170330         exsr DataProntaMerce;
151800160616         IF  RiemetteVideo;
151900160616           leavesr;
152000160616         ENDIF;
152100160616
152200160616       //?Controlli sul chi paga
152300160616         exsr ChiPaga;
152400160616
152500160616       //?Controlli sulla filiale ritiro
152600160616         exsr FilRitiro;
152700160616         IF  RiemetteVideo;
152800160616           leavesr;
152900160616         ENDIF;
153000160616
153100160616       //?Controlli sul destinatario
153200160616         exsr Destinatario;
153300160616         IF  RiemetteVideo;
153400160616           leavesr;
153500160616         ENDIF;
153600160616
153700160616       //?Controlli sull'ordinante
153800160616         exsr Ordinante;
153900160616         IF  RiemetteVideo;
154000160616           leavesr;
154100160616         ENDIF;
154200160616
154300160616       //?Controlli sulla filiale consegna
154400160616         exsr FilConsegna;
154500160616         IF  RiemetteVideo;
154600160616           leavesr;
154700160616         ENDIF;
154800170412
154900170412       //?Se variato Codice Mittente
155000170412       //?           Codice Ordinante
155100170412       //?           Data Pronta Merce
155200170412       //?           Quantità
155300170412       //?           Commissionato
155400170412       //?Devo ricalcolare la data di ritiro
155500170413       //?devo azzerare la data ritiro così viene ricalcolata
155600170413         IF  fior000o.cra1 <> fior000i.cra1 or
155700170413             fior000o.cra2 <> fior000i.cra2 or
155800170413             fior000o.cra3 <> fior000i.cra3 or
155900170413             fior000o.cor1 <> fior000i.cor1 or
156000170413             fior000o.cor2 <> fior000i.cor2 or
156100170413             fior000o.cor3 <> fior000i.cor3 or
156200170413             fior000o.dpm  <> fior000i.dpm  or
156300170413             fior000o.pkg  <> fior000i.pkg  or
156400170413             fior000o.vlm  <> fior000i.vlm  or
156500170413             fior000o.bnc  <> fior000i.bnc  or
156600170413             fior000o.blc  <> fior000i.blc  or
156700170413             fior000o.att  <> fior000i.att  or
156800170413             fior000o.mtc  <> fior000i.mtc  or
156900170413             fior000o.comm <> fior000i.comm;
157000170413           clear fior000o.dar;
157100170413         ENDIF;
157200160616
157300160616       ENDSR;
157400160321
157500160321       //--------------------------------------------------------------
157600160321       //?Decodifico il tipo Comunicazione ORM.
157700160321       //--------------------------------------------------------------
157800160321       BEGSR  DecoTipoComOrm;
157900160321
158000160321         clear fior000i.dtco;
158100160321
158200160321         IF  getTabella (c_tntbe:'TCO':keytabella:
158300160321                         *blanks:*blanks:*blanks:
158400160321                         *omit:*omit:*omit:*omit:*omit:
158500160321                         *omit:*omit:*omit:*omit:*omit:
158600160321                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
158700160321             = *zeros and ds_TNTBE.TBEatb = *blanks;
158800160321           fior000i.dtco = ds_TNTBE.TBEuni;
158900160321         ENDIF;
159000160321
159100160321       ENDSR;
159200141020
159300141020       //--------------------------------------------------------------
159400141020       //?Decodifico il tipo ORM.
159500141020       //--------------------------------------------------------------
159600141020       BEGSR  DecoTipoOrm;
159700141020
159800141020         clear fior000i.dtor;
159900141020
160000141020         IF  getTabella (c_tntbe:'TOR':keytabella:
160100141020                         *blanks:*blanks:*blanks:
160200141020                         *omit:*omit:*omit:*omit:*omit:
160300141020                         *omit:*omit:*omit:*omit:*omit:
160400141020                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
160500141020             = *zeros and ds_TNTBE.TBEatb = *blanks;
160600141020           fior000i.dtor = ds_TNTBE.TBEuni;
160700141020         ENDIF;
160800141020
160900141020       ENDSR;
161000160620
161100160620       //--------------------------------------------------------------
161200160620       //?Decodifico lo Stato ORM.
161300160620       //--------------------------------------------------------------
161400160620       BEGSR  DecoStatoOrm;
161500160620
161600160620         clear fior000i.dsto;
161700160620
161800160620         IF  getTabella (c_tntbe:'STO':keytabella:
161900160620                         *blanks:*blanks:*blanks:
162000160620                         *omit:*omit:*omit:*omit:*omit:
162100160620                         *omit:*omit:*omit:*omit:*omit:
162200160620                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
162300160620             = *zeros and ds_TNTBE.TBEatb = *blanks;
162400160620           fior000i.dsto = ds_TNTBE.TBEuni;
162500160620         ENDIF;
162600160620
162700160620       ENDSR;
162800141010
162900141010       //--------------------------------------------------------------
163000141010       //?Controlli sul mittente.
163100141010       //--------------------------------------------------------------
163200141010       BEGSR  Mittente;
163300141010
163400160607       //?Se cambio codice mittente
163500160607         IF  fior000i.cra1 <> fior000o.cra1 or
163600160607             fior000i.cra2 <> fior000o.cra2 or
163700160607             fior000i.cra3 <> fior000o.cra3;
163800160607         //?Carico i dati del mittente
163900160607           wCodice  = (fior000o.cra1 * 10000000);
164000160607           wCodice += (fior000o.cra2 * 1000);
164100160607           wCodice += (fior000o.cra3);
164200160607         //?se da codificato passo a non codificato
164300160607           IF  wCodice = 0;
164400160607         //?se paga mittente pulisco KSC e CTR
164500160607             IF  fior000o.pag = FIORA00_PAGA_MITTENTE;
164600160607               clear fior000o.ksc;
164700160607               clear fior000o.ctr;
164800160607             ENDIF;
164900160607           //?Se NON è Manutenzione
165000160613             IF  prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE;
165100160613             //?pulisco il campo della data ritiro
165200160613             //?così il FIORA la ricalcola
165300160607               clear fior000o.dar;
165400160607             ENDIF;
165500160607           ENDIF;
165600160615           IF  wCodice > 0;
165700160615             exsr GetDatiMittente;
165800160615           ENDIF;
165900160607         //?pulisco tutte le forzature già memorizzate
166000160607           reset FIORA0F0;
166100170419         //?pulisco il giro perchè cambiando codice devo cambiare anche il giro
166200170419           clear fior000o.tgi;
166300170419           clear fior000o.cgi;
166400160606         ENDIF;
166500141021
166600141021       //?Se non ci sono dati non controllo
166700160525       //?o se non è stato variato il codice
166800160525         IF  (fior000o.inr = *blanks and
166900160525              fior000o.lor = *blanks and fior000o.car = *blanks and
167000160525              fior000o.prr = *blanks and fior000o.nar = *blanks) or
167100160615             (fior000i.cra1 = fior000o.cra1 and
167200160615              fior000i.cra2 = fior000o.cra2 and
167300160615              fior000i.cra3 = fior000o.cra3 and
167400160615              fior000o.cra1 > 0 and fior000o.cra2 > 0);
167500141021           leavesr;
167600141021         ENDIF;
167700141021
167800141021       //?Devo controllare l'indirizzo completo del mittente
167900141021         clear FNLV13DS;
168000141021         clear TISI95DS;
168100141021         fnlv13ds.I13af0 = 'S';
168200141021         fnlv13ds.I13af1 = 'S';
168300141021         fnlv13ds.I13sz2 = 'S';
168400141021         fnlv13ds.I13la3 = 'S';
168500141021         fnlv13ds.I13sz3 = 'S';
168600141021         fnlv13ds.I13cnv = 'S';
168700141021         tisi95ds.I95tcn = '7';
168800141021         tisi95ds.I95ind = fior000o.inr;
168900141021         tisi95ds.I95loc = fior000o.lor;
169000141021         tisi95ds.i95cap = fior000o.car;
169100141021         tisi95ds.i95prv = fior000o.prr;
169200141021         tisi95ds.i95nar = fior000o.nar;
169300141021       //?Se richiesto ntw DPD lo passo
169400141021         IF  fior000o.ntwdpd <> *blanks;
169500141021           tisi95ds.I95fi1 = fior000o.ntwdpd;
169600141021         ENDIF;
169700141021
169800141021       //?Gestione forzature in caso di cambio indirizzo
169900160525         IF  fior000i.car <> fior000o.car;
170000160525           fior000i.car = fior000o.car;
170100160525           clear wfzvr;
170200160525           clear wfz1r;
170300160525           clear wfz2r;
170400160525           clear wfz3r;
170500141021         ENDIF;
170600160525         IF  fior000i.lor <> fior000o.lor;
170700160525           fior000i.lor = fior000o.lor;
170800160525           clear wfz1r;
170900160525           clear wfz2r;
171000160525           clear wfz3r;
171100141021         ENDIF;
171200160525         IF  fior000i.prr <> fior000o.prr;
171300160525           fior000i.prr = fior000o.prr;
171400160525           clear wfz3r;
171500141021         ENDIF;
171600160525         fnlv13ds.E13fzv = wfzvr;
171700160525         fnlv13ds.E13fz1 = wfz1r;
171800160525         fnlv13ds.E13fz2 = wfz2r;
171900160525         fnlv13ds.E13fz3 = wfz3r;
172000141021
172100141021         fnlv13r (kpjba:fnlv13ds:tisi95ds);
172200160525
172300160525         werrr = fnlv13ds.O13err;
172400160525         wmsgr = fnlv13ds.O13msg;
172500160525         wfzvr = fnlv13ds.E13fzv;
172600160525         wfz1r = fnlv13ds.E13fz1;
172700160525         wfz2r = fnlv13ds.E13fz2;
172800160525         wfz3r = fnlv13ds.E13fz3;
172900141021
173000141021       //?Se non presente il codice mittente
173100141021         IF  fior000o.cra1 = 0 and fior000o.cra2 = 0;
173200141021       //?Se il livello di affidabilità non è inferiore a 2 e
173300141021       //?il livello di reperimento non è inferiore a 2
173400141021       //?reimposto i dati da TISI95
173500141021           IF  fnlv13ds.O13ron = 'S';
173600141021             fior000o.nar = tisi95ds.O95nar;
173700141021           ENDIF;
173800141021           IF  fnlv13ds.O13roc = 'S';
173900141021             fior000o.car = tisi95ds.O95cap;
174000141021           ENDIF;
174100141021           IF  fnlv13ds.O13rop = 'S';
174200141021             fior000o.prr = tisi95ds.O95prv;
174300141021           ENDIF;
174400141021           IF  fnlv13ds.O13rol = 'S';
174500141021             fior000o.lor = tisi95ds.O95loc;
174600141021           ENDIF;
174700141021       //?per errore o se variato indirizzo messaggio da FNLV13
174800160525           IF  fnlv13ds.O13err <> *blanks and
174900160525               fnlv13ds.O13msg <> *blanks;
175000160608             reset fiora0M0;
175100141021             SELECT;
175200141021             WHEN  fnlv13ds.O13err = '3';
175300141021               wIdCampo = 'VAOLOR';
175400141021             WHEN  fnlv13ds.O13err = '5';
175500141021               wIdCampo = 'VAOCAR';
175600141021             WHEN  fnlv13ds.O13err = '4';
175700141021               wIdCampo = 'VAOPRR';
175800141021             WHEN  fnlv13ds.O13err = '6';
175900141021               wIdCampo = 'VAONAR';
176000141021             ENDSL;
176100160520             wIdMsg   = 'TIS0718';
176200160520             wParm = fnlv13ds.O13msg;
176300160520             exsr Messaggi;
176400160520             RiemetteVideo = *on;
176500160520             leavesr;
176600160520           ENDIF;
176700141021         ENDIF;
176800141021
176900141021       //?Se presente il codice mittente
177000160412         IF  fior000o.cra1 > 0 and fior000o.cra2 > 0 and
177100141021             fnlv13ds.O13err <> *blanks and
177200141021             fnlv13ds.O13msg <> *blanks;
177300160608           reset fiora0M0;
177400141021           wIdMsg   = 'TIS0798';
177500160525           wIdCampo = 'VAOCRA';
177600141021           clear wParm;
177700141021           exsr Messaggi;
177800160519           RiemetteVideo = *on;
177900141021           leavesr;
178000141021         ENDIF;
178100141021
178200141021       //?Controlla se reimpostati i dati dell'indirizzo
178300141021         IF  fnlv13ds.O13rol = 'S';
178400160608           reset fiora0M0;
178500141021           wIdMsg   = 'TIS0718';
178600141021           wIdCampo = 'VAOLOR';
178700141021           wParm = 'ATTENZIONE!! Modificata Località.';
178800141021           exsr Forzatura;
178900160519           RiemetteVideo = *on;
179000141021           leavesr;
179100141021         ENDIF;
179200141021         IF  fnlv13ds.O13rop = 'S';
179300160608           reset fiora0M0;
179400141021           wIdMsg   = 'TIS0718';
179500141021           wIdCampo = 'VAOPRR';
179600141021           wParm = 'ATTENZIONE!! Modificata Provincia.';
179700141021           exsr Forzatura;
179800160519           RiemetteVideo = *on;
179900141021           leavesr;
180000141021         ENDIF;
180100141021         IF  fnlv13ds.O13roc = 'S';
180200160608           reset fiora0M0;
180300141021           wIdMsg   = 'TIS0718';
180400141021           wIdCampo = 'VAOCAR';
180500141021           wParm = 'ATTENZIONE!! Modificata CAP.';
180600141021           exsr Forzatura;
180700160519           RiemetteVideo = *on;
180800141021           leavesr;
180900141021         ENDIF;
181000141021         IF  fnlv13ds.O13ron = 'S';
181100160608           reset fiora0M0;
181200141021           wIdMsg   = 'TIS0718';
181300141021           wIdCampo = 'VAONAR';
181400141021           wParm = 'ATTENZIONE!! Modificata Nazione.';
181500141021           exsr Forzatura;
181600160519           RiemetteVideo = *on;
181700141021           leavesr;
181800141021         ENDIF;
181900141009
182000141009       ENDSR;
182100160412
182200160412       //--------------------------------------------------------------
182300160412       //?Recupero i dati del mittente.
182400160412       //--------------------------------------------------------------
182500160412       BEGSR  GetDatiMittente;
182600160412
182700160412         reset FIORB0OR;
182800160412
182900160519       //?Se non valido o bloccato vado via
183000160519       //?(ci pensa il FIORA00R a tornare l'errore)
183100160412         IF  not IsCodiceValido(FIORB0OR);
183200160412           leavesr;
183300160412         ENDIF;
183400160412
183500160412       //?Imposto i dati del mittente
183600160412         fior000o.rsr = fiorb0OR.nome;
183700160412         fior000o.inr = fiorb0OR.indirizzo;
183800160412         fior000o.car = fiorb0OR.cap;
183900160412         fior000o.lor = fiorb0OR.localita;
184000160412         fior000o.prr = fiorb0OR.provincia;
184100160412         fior000o.nar = fiorb0OR.nazione;
184200170420         IF  fiorb0OR.oraritiro > *zeros;
184300170420           fior000o.orr = fiorb0OR.oraritiro;
184400170420         ENDIF;
184500160412         fior000o.ter = fiorb0OR.telefono;
184600160412         fior000o.rer = fiorb0OR.referente;
184700160412         fior000o.nam = fiorb0OR.natmerce;
184800160412         fior000o.dalleam = fiorb0OR.dalleam;
184900160412         fior000o.alleam = fiorb0OR.alleam;
185000160412         fior000o.dallepm = fiorb0OR.dallepm;
185100160412         fior000o.allepm = fiorb0OR.allepm;
185200160412         fior000o.spi = fiorb0OR.sponda;
185300160531         IF  fiorb0OR.ordinante > 0;
185400160531           fior000o.cor1 =
185500160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):1:3):3:0);
185600160531           fior000o.cor2 =
185700160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):4:4):4:0);
185800160531           fior000o.cor3 =
185900160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):8:3):3:0);
186000160531         ENDIF;
186100160412
186200160412       //?Se paga mittente imposto i dati
186300160412         IF  fior000o.pag = FIORA00_PAGA_MITTENTE;
186400160412           fior000o.ksc = fiorb0OR.codksc;
186500160412           IF  fiorb0OR.codtariffa = 999;
186600160412             clear fior000o.ctr;
186700160412           ELSE;
186800160412             fior000o.ctr =
186900160412             %subst(%editc(fiorb0OR.codtariffa:'X'):2:3);
187000160412           ENDIF;
187100160412         ENDIF;
187200160531
187300160531       //?Alert Conferma Prenotazione ritiro
187400170426       //?se non presente l'ordinante
187500170426         IF  fiorb0OR.alertconf = 'S' and
187600170426             fior000o.cor1 = 0 and
187700170426             fior000o.cor2 = 0;
187800160531           fior000o.mailconf = fiorb0OR.mailconf;
187900160531           fior000o.smsconf = fiorb0OR.smsconf;
188000170505           IF  fior000o.mailconf <> *blanks;
188100170505             fior000i.alertcmail = 'S';
188200170505           ENDIF;
188300170505           IF  fior000o.smsconf <> *blanks;
188400170505             fior000i.alertcsms = 'S';
188500170505           ENDIF;
188600160531         ENDIF;
188700160412
188800160412       //?msg INFO se sto sostituendo le note già messe dall'utente
188900160531         IF  (fior000o.nota1 <> *blanks and
189000160531              fior000o.nota1 <> fiorb0OR.nota1 and
189100160531              fiorb0OR.nota1 <> *blanks) or
189200160531             (fior000o.nota2 <> *blanks and
189300160531              fior000o.nota2 <> fiorb0OR.nota2 and
189400160531              fiorb0OR.nota2 <> *blanks);
189500160608           reset fiora0M0;
189600160531           wIdMsg   = 'TIS0718';
189700160531           wIdCampo = 'VAONO1';
189800160531           wParm = 'ATTENZIONE!! Le note immesse sono state sovrascritte.';
189900160531           exsr Forzatura;
190000160531           RiemetteVideo = *on;
190100160531         ENDIF;
190200160531         IF  fiorb0OR.nota1 <> *blanks;
190300160531           fior000o.nota1 = fiorb0OR.nota1;
190400160531         ENDIF;
190500160531         IF  fiorb0OR.nota2 <> *blanks;
190600160531           fior000o.nota2 = fiorb0OR.nota2;
190700160531         ENDIF;
190800160531         IF  RiemetteVideo;
190900160531           leavesr;
191000160531         ENDIF;
191100160412
191200160412       ENDSR;
191300160616
191400160616       //--------------------------------------------------------------
191500160616       //?Controlli sull'ordinante.
191600160616       //--------------------------------------------------------------
191700160616       BEGSR  Ordinante;
191800160616
191900160616       //?Se cambio codice ordinante
192000160616         IF  fior000i.cor1 <> fior000o.cor1 or
192100160616             fior000i.cor2 <> fior000o.cor2 or
192200160616             fior000i.cor3 <> fior000o.cor3;
192300160616         //?Carico i dati dell' ordinante
192400160616           wCodice  = (fior000o.cor1 * 10000000);
192500160616           wCodice += (fior000o.cor2 * 1000);
192600160616           wCodice += (fior000o.cor3);
192700160616         //?se da codificato passo a non codificato
192800160616           IF  wCodice = 0;
192900160616         //?se paga ordinante pulisco KSC e CTR
193000160616             IF  fior000o.pag = FIORA00_PAGA_ORDINANTE;
193100160616               clear fior000o.ksc;
193200160616               clear fior000o.ctr;
193300160616             ENDIF;
193400160616           ENDIF;
193500160616           IF  wCodice > 0;
193600160616             exsr GetDatiOrdinante;
193700160616           ENDIF;
193800160616         ENDIF;
193900160616
194000160616       //?Se non ci sono dati non controllo
194100160616       //?o se non è stato variato il codice
194200160616         IF  (fior000o.ino = *blanks and
194300160616              fior000o.loo = *blanks and fior000o.cao = *blanks and
194400160616              fior000o.pro = *blanks and fior000o.nao = *blanks) or
194500160616             (fior000i.cor1 = fior000o.cor1 and
194600160616              fior000i.cor2 = fior000o.cor2 and
194700160616              fior000i.cor3 = fior000o.cor3 and
194800160616              fior000o.cor1 > 0 and fior000o.cor2 > 0);
194900160616           leavesr;
195000160616         ENDIF;
195100160616
195200160616       //?Devo controllare l'indirizzo completo dell'ordinante
195300160616         clear FNLV13DS;
195400160616         clear TISI95DS;
195500160616         fnlv13ds.I13af0 = 'S';
195600160616         fnlv13ds.I13af1 = 'S';
195700160616         fnlv13ds.I13sz2 = 'S';
195800160616         fnlv13ds.I13la3 = 'S';
195900160616         fnlv13ds.I13sz3 = 'S';
196000160616         fnlv13ds.I13cnv = 'S';
196100160616         tisi95ds.I95tcn = '7';
196200160616         tisi95ds.I95ind = fior000o.ino;
196300160616         tisi95ds.I95loc = fior000o.loo;
196400160616         tisi95ds.i95cap = fior000o.cao;
196500160616         tisi95ds.i95prv = fior000o.pro;
196600160616         tisi95ds.i95nar = fior000o.nao;
196700160616
196800160616       //?Gestione forzature in caso di cambio indirizzo
196900160616         IF  fior000i.cao <> fior000o.cao;
197000160616           fior000i.cao = fior000o.cao;
197100160616           clear wfzvr;
197200160616           clear wfz1r;
197300160616           clear wfz2r;
197400160616           clear wfz3r;
197500160616         ENDIF;
197600160616         IF  fior000i.loo <> fior000o.loo;
197700160616           fior000i.loo = fior000o.loo;
197800160616           clear wfz1r;
197900160616           clear wfz2r;
198000160616           clear wfz3r;
198100160616         ENDIF;
198200160616         IF  fior000i.pro <> fior000o.pro;
198300160616           fior000i.pro = fior000o.pro;
198400160616           clear wfz3r;
198500160616         ENDIF;
198600160616         fnlv13ds.E13fzv = wfzvr;
198700160616         fnlv13ds.E13fz1 = wfz1r;
198800160616         fnlv13ds.E13fz2 = wfz2r;
198900160616         fnlv13ds.E13fz3 = wfz3r;
199000160616
199100160616         fnlv13r (kpjba:fnlv13ds:tisi95ds);
199200160616
199300160616         werrr = fnlv13ds.O13err;
199400160616         wmsgr = fnlv13ds.O13msg;
199500160616         wfzvr = fnlv13ds.E13fzv;
199600160616         wfz1r = fnlv13ds.E13fz1;
199700160616         wfz2r = fnlv13ds.E13fz2;
199800160616         wfz3r = fnlv13ds.E13fz3;
199900160616
200000160616       //?Se non presente il codice ordinante
200100160616         IF  fior000o.cor1 = 0 and fior000o.cor2 = 0;
200200160616       //?Se il livello di affidabilità non è inferiore a 2 e
200300160616       //?il livello di reperimento non è inferiore a 2
200400160616       //?reimposto i dati da TISI95
200500160616           IF  fnlv13ds.O13ron = 'S';
200600160616             fior000o.nao = tisi95ds.O95nar;
200700160616           ENDIF;
200800160616           IF  fnlv13ds.O13roc = 'S';
200900160616             fior000o.cao = tisi95ds.O95cap;
201000160616           ENDIF;
201100160616           IF  fnlv13ds.O13rop = 'S';
201200160616             fior000o.pro = tisi95ds.O95prv;
201300160616           ENDIF;
201400160616           IF  fnlv13ds.O13rol = 'S';
201500160616             fior000o.loo = tisi95ds.O95loc;
201600160616           ENDIF;
201700160616       //?per errore o se variato indirizzo messaggio da FNLV13
201800160616           IF  fnlv13ds.O13err <> *blanks and
201900160616               fnlv13ds.O13msg <> *blanks;
202000160616             reset fiora0M0;
202100160616             SELECT;
202200160616             WHEN  fnlv13ds.O13err = '3';
202300160616               wIdCampo = 'VAOLOO';
202400160616             WHEN  fnlv13ds.O13err = '5';
202500160616               wIdCampo = 'VAOCAO';
202600160616             WHEN  fnlv13ds.O13err = '4';
202700160616               wIdCampo = 'VAOPRO';
202800160616             WHEN  fnlv13ds.O13err = '6';
202900160616               wIdCampo = 'VAONAO';
203000160616             ENDSL;
203100160616             wIdMsg   = 'TIS0718';
203200160616             wParm = fnlv13ds.O13msg;
203300160616             exsr Messaggi;
203400160616             RiemetteVideo = *on;
203500160616             leavesr;
203600160616           ENDIF;
203700160616         ENDIF;
203800160616
203900160616       //?Se presente il codice ordinante
204000160616         IF  fior000o.cor1 > 0 and fior000o.cor2 > 0 and
204100160616             fnlv13ds.O13err <> *blanks and
204200160616             fnlv13ds.O13msg <> *blanks;
204300160616           reset fiora0M0;
204400160616           wIdMsg   = 'TIS0798';
204500160616           wIdCampo = 'VAOCRO';
204600160616           clear wParm;
204700160616           exsr Messaggi;
204800160616           RiemetteVideo = *on;
204900160616           leavesr;
205000160616         ENDIF;
205100160616
205200160616       //?Controlla se reimpostati i dati dell'indirizzo
205300160616         IF  fnlv13ds.O13rol = 'S';
205400160616           reset fiora0M0;
205500160616           wIdMsg   = 'TIS0718';
205600160616           wIdCampo = 'VAOLOO';
205700160616           wParm = 'ATTENZIONE!! Modificata Località.';
205800160616           exsr Forzatura;
205900160616           RiemetteVideo = *on;
206000160616           leavesr;
206100160616         ENDIF;
206200160616         IF  fnlv13ds.O13rop = 'S';
206300160616           reset fiora0M0;
206400160616           wIdMsg   = 'TIS0718';
206500160616           wIdCampo = 'VAOPRO';
206600160616           wParm = 'ATTENZIONE!! Modificata Provincia.';
206700160616           exsr Forzatura;
206800160616           RiemetteVideo = *on;
206900160616           leavesr;
207000160616         ENDIF;
207100160616         IF  fnlv13ds.O13roc = 'S';
207200160616           reset fiora0M0;
207300160616           wIdMsg   = 'TIS0718';
207400160616           wIdCampo = 'VAOCAO';
207500160616           wParm = 'ATTENZIONE!! Modificata CAP.';
207600160616           exsr Forzatura;
207700160616           RiemetteVideo = *on;
207800160616           leavesr;
207900160616         ENDIF;
208000160616         IF  fnlv13ds.O13ron = 'S';
208100160616           reset fiora0M0;
208200160616           wIdMsg   = 'TIS0718';
208300160616           wIdCampo = 'VAONAO';
208400160616           wParm = 'ATTENZIONE!! Modificata Nazione.';
208500160616           exsr Forzatura;
208600160616           RiemetteVideo = *on;
208700160616           leavesr;
208800160616         ENDIF;
208900160616
209000160616       ENDSR;
209100160616
209200160616       //--------------------------------------------------------------
209300160616       //?Recupero i dati dell'ordinante.
209400160616       //--------------------------------------------------------------
209500160616       BEGSR  GetDatiOrdinante;
209600160616
209700160616         reset FIORB0OO;
209800160616
209900160616       //?Se non valido o bloccato vado via
210000160616       //?(ci pensa il FIORA00R a tornare l'errore)
210100160616         IF  not IsCodiceValido(FIORB0OO);
210200160616           leavesr;
210300160616         ENDIF;
210400160616
210500160616       //?Imposto i dati dell'ordinante
210600160616         fior000o.rso = fiorb0OO.nome;
210700160616         fior000o.ino = fiorb0OO.indirizzo;
210800160616         fior000o.cao = fiorb0OO.cap;
210900160616         fior000o.loo = fiorb0OO.localita;
211000160616         fior000o.pro = fiorb0OO.provincia;
211100160616         fior000o.nao = fiorb0OO.nazione;
211200160616
211300160616       //?Se paga ordinante imposto i dati
211400160616         IF  fior000o.pag = FIORA00_PAGA_ORDINANTE;
211500160616           fior000o.ksc = fiorb0OO.codksc;
211600160616           IF  fiorb0OO.codtariffa = 999;
211700160616             clear fior000o.ctr;
211800160616           ELSE;
211900160616             fior000o.ctr =
212000160616             %subst(%editc(fiorb0OR.codtariffa:'X'):2:3);
212100160616           ENDIF;
212200160616         ENDIF;
212300170426
212400170426       //?Imposto anche il flag del commissionato
212500170426         fior000o.comm = fiorb0OO.contatto;
212600170426
212700170426       //?Alert Conferma Prenotazione ritiro
212800170426         IF  fiorb0OO.alertconf = 'S';
212900170426           fior000o.mailconf = fiorb0OO.mailconf;
213000170426           fior000o.smsconf = fiorb0OO.smsconf;
213100170505           IF  fior000o.mailconf <> *blanks;
213200170505             fior000i.alertcmail = 'S';
213300170505           ENDIF;
213400170505           IF  fior000o.smsconf <> *blanks;
213500170505             fior000i.alertcsms = 'S';
213600170505           ENDIF;
213700170426         ENDIF;
213800170522
213900170522       //?Se mittente non codificato imposto le note dell'ordinante
214000170522         IF  fior000i.cra1 = *zeros and fior000i.cra2 = *zeros;
214100170522           IF  fior000o.nota1 = *blanks;
214200170522             fior000o.nota1 = fiorb0OO.nota1;
214300170522           ENDIF;
214400170522           IF  fior000o.nota2 = *blanks;
214500170522             fior000o.nota2 = fiorb0OO.nota2;
214600170522           ENDIF;
214700170522        ENDIF;
214800160616
214900160616       ENDSR;
215000160616
215100160616       //--------------------------------------------------------------
215200160616       //?Controlli sul destinatario.
215300160616       //--------------------------------------------------------------
215400160616       BEGSR  Destinatario;
215500160616
215600160616       //?Se NON è Manutenzione
215700160616       //?ed è stato inserito il destinatario
215800160616       //?e il tipo ORM è Multiplo
215900160616       //?imposto in automatico tipo ORM Singolo
216000160616         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE and
216100160616             fior000o.tor = FIORA00_TIPOORM_MULTIPLO and
216200160616           ((fior000o.crc1 > 0 and fior000o.crc2 > 0 and
216300160616             fior000i.crc1 <> fior000o.crc1 and
216400160616             fior000i.crc2 <> fior000o.crc2 and
216500160616             fior000i.crc1 = 0 and fior000i.crc2 = 0) or
216600160616            (fior000o.rsc <> *blanks and
216700160616             fior000i.rsc <> fior000o.rsc and
216800160616             fior000i.rsc = *blanks));
216900160616           fior000o.tor = FIORA00_TIPOORM_SINGOLO;
217000160616         ENDIF;
217100160616
217200160616       //?Se cambio codice destinatario
217300160616         IF  fior000i.crc1 <> fior000o.crc1 or
217400160616             fior000i.crc2 <> fior000o.crc2 or
217500160616             fior000i.crc3 <> fior000o.crc3;
217600160616         //?Carico i dati del destinatario
217700160616           wCodice  = (fior000o.crc1 * 10000000);
217800160616           wCodice += (fior000o.crc2 * 1000);
217900160616           wCodice += (fior000o.crc3);
218000160616         //?se da codificato passo a non codificato
218100160616           IF  wCodice = 0;
218200160616         //?se paga destinatario pulisco KSC e CTR
218300160616             IF  fior000o.pag = FIORA00_PAGA_DESTINATARIO;
218400160616               clear fior000o.ksc;
218500160616               clear fior000o.ctr;
218600160616             ENDIF;
218700160616           ENDIF;
218800160616           IF  wCodice > 0;
218900160616             exsr GetDatiDestinatario;
219000160616           ENDIF;
219100160616         ENDIF;
219200160616
219300160616       //?Se non ci sono dati non controllo
219400160616       //?o se non è stato variato il codice
219500160616         IF  (fior000o.inc = *blanks and
219600160616              fior000o.loc = *blanks and fior000o.cac = *blanks and
219700160616              fior000o.prc = *blanks and fior000o.nac = *blanks) or
219800160616             (fior000i.crc1 = fior000o.crc1 and
219900160616              fior000i.crc2 = fior000o.crc2 and
220000160616              fior000i.crc3 = fior000o.crc3 and
220100160616              fior000o.crc1 > 0 and fior000o.crc2 > 0);
220200160616           leavesr;
220300160616         ENDIF;
220400160616
220500160616       //?Devo controllare l'indirizzo completo del destinatario
220600160616         clear FNLV13DS;
220700160616         clear TISI95DS;
220800160616         fnlv13ds.I13af0 = 'S';
220900160616         fnlv13ds.I13af1 = 'S';
221000160616         fnlv13ds.I13sz2 = 'S';
221100160616         fnlv13ds.I13la3 = 'S';
221200160616         fnlv13ds.I13sz3 = 'S';
221300160616         fnlv13ds.I13cnv = 'S';
221400160616         tisi95ds.I95tcn = '7';
221500160616         tisi95ds.I95ind = fior000o.inc;
221600160616         tisi95ds.I95loc = fior000o.loc;
221700160616         tisi95ds.i95cap = fior000o.cac;
221800160616         tisi95ds.i95prv = fior000o.prc;
221900160616         tisi95ds.i95nar = fior000o.nac;
222000160616       //?Se non paga mittente imposto anche il flag per Assegnato
222100160616         IF  fior000o.pag <> FIORA00_PAGA_MITTENTE;
222200160616           tisi95ds.I95tpo = FIORA00_BOLLA_ASSEGNATO;
222300160616         ENDIF;
222400160616
222500160616       //?Gestione forzature in caso di cambio indirizzo
222600160616         IF  fior000i.cac <> fior000o.cac;
222700160616           fior000i.cac = fior000o.cac;
222800160616           clear wfzvr;
222900160616           clear wfz1r;
223000160616           clear wfz2r;
223100160616           clear wfz3r;
223200160616         ENDIF;
223300160616         IF  fior000i.lor <> fior000o.loc;
223400160616           fior000i.loc = fior000o.loc;
223500160616           clear wfz1r;
223600160616           clear wfz2r;
223700160616           clear wfz3r;
223800160616         ENDIF;
223900160616         IF  fior000i.prc <> fior000o.prc;
224000160616           fior000i.prc = fior000o.prc;
224100160616           clear wfz3r;
224200160616         ENDIF;
224300160616         fnlv13ds.E13fzv = wfzvr;
224400160616         fnlv13ds.E13fz1 = wfz1r;
224500160616         fnlv13ds.E13fz2 = wfz2r;
224600160616         fnlv13ds.E13fz3 = wfz3r;
224700160616
224800160616         fnlv13r (kpjba:fnlv13ds:tisi95ds);
224900160616
225000160616         werrr = fnlv13ds.O13err;
225100160616         wmsgr = fnlv13ds.O13msg;
225200160616         wfzvr = fnlv13ds.E13fzv;
225300160616         wfz1r = fnlv13ds.E13fz1;
225400160616         wfz2r = fnlv13ds.E13fz2;
225500160616         wfz3r = fnlv13ds.E13fz3;
225600160616
225700160616       //?Se non presente il codice destinatario
225800160616         IF  fior000o.crc1 = 0 and fior000o.crc2 = 0;
225900160616       //?Se il livello di affidabilità non è inferiore a 2 e
226000160616       //?il livello di reperimento non è inferiore a 2
226100160616       //?reimposto i dati da TISI95
226200160616           IF  fnlv13ds.O13ron = 'S';
226300160616             fior000o.nac = tisi95ds.O95nar;
226400160616           ENDIF;
226500160616           IF  fnlv13ds.O13roc = 'S';
226600160616             fior000o.cac = tisi95ds.O95cap;
226700160616           ENDIF;
226800160616           IF  fnlv13ds.O13rop = 'S';
226900160616             fior000o.prc = tisi95ds.O95prv;
227000160616           ENDIF;
227100160616           IF  fnlv13ds.O13rol = 'S';
227200160616             fior000o.loc = tisi95ds.O95loc;
227300160616           ENDIF;
227400160616       //?per errore o se variato indirizzo messaggio da FNLV13
227500160616           IF  fnlv13ds.O13err <> *blanks and
227600160616               fnlv13ds.O13msg <> *blanks;
227700160616             reset fiora0M0;
227800160616             SELECT;
227900160616             WHEN  fnlv13ds.O13err = '3';
228000160616               wIdCampo = 'VAOLOC';
228100160616             WHEN  fnlv13ds.O13err = '5';
228200160616               wIdCampo = 'VAOCAC';
228300160616             WHEN  fnlv13ds.O13err = '4';
228400160616               wIdCampo = 'VAOPRC';
228500160616             WHEN  fnlv13ds.O13err = '6';
228600160616               wIdCampo = 'VAONAC';
228700160616             ENDSL;
228800160616             wIdMsg   = 'TIS0718';
228900160616             wParm = fnlv13ds.O13msg;
229000160616             exsr Messaggi;
229100160616             RiemetteVideo = *on;
229200160616             leavesr;
229300160616           ENDIF;
229400160616         ENDIF;
229500160616
229600160616       //?Se presente il codice destinatario
229700160616         IF  fior000o.crc1 > 0 and fior000o.crc2 > 0 and
229800160616             fnlv13ds.O13err <> *blanks and
229900160616             fnlv13ds.O13msg <> *blanks;
230000160616           reset fiora0M0;
230100160616           wIdMsg   = 'TIS0798';
230200160616           wIdCampo = 'VAOCRC';
230300160616           clear wParm;
230400160616           exsr Messaggi;
230500160616           RiemetteVideo = *on;
230600160616           leavesr;
230700160616         ENDIF;
230800160616
230900160616       //?Controlla se reimpostati i dati dell'indirizzo
231000160616         IF  fnlv13ds.O13rol = 'S';
231100160616           reset fiora0M0;
231200160616           wIdMsg   = 'TIS0718';
231300160616           wIdCampo = 'VAOLOC';
231400160616           wParm = 'ATTENZIONE!! Modificata Località.';
231500160616           exsr Forzatura;
231600160616           RiemetteVideo = *on;
231700160616           leavesr;
231800160616         ENDIF;
231900160616         IF  fnlv13ds.O13rop = 'S';
232000160616           reset fiora0M0;
232100160616           wIdMsg   = 'TIS0718';
232200160616           wIdCampo = 'VAOPRC';
232300160616           wParm = 'ATTENZIONE!! Modificata Provincia.';
232400160616           exsr Forzatura;
232500160616           RiemetteVideo = *on;
232600160616           leavesr;
232700160616         ENDIF;
232800160616         IF  fnlv13ds.O13roc = 'S';
232900160616           reset fiora0M0;
233000160616           wIdMsg   = 'TIS0718';
233100160616           wIdCampo = 'VAOCAC';
233200160616           wParm = 'ATTENZIONE!! Modificata CAP.';
233300160616           exsr Forzatura;
233400160616           RiemetteVideo = *on;
233500160616           leavesr;
233600160616         ENDIF;
233700160616         IF  fnlv13ds.O13ron = 'S';
233800160616           reset fiora0M0;
233900160616           wIdMsg   = 'TIS0718';
234000160616           wIdCampo = 'VAONAC';
234100160616           wParm = 'ATTENZIONE!! Modificata Nazione.';
234200160616           exsr Forzatura;
234300160616           RiemetteVideo = *on;
234400160616           leavesr;
234500160616         ENDIF;
234600160616
234700160616       //?Se presente la Nazione ed è lunga 2 byte e questa è presente anche
234800160616       //?come Provincia devo avvisare l'utente per sapere se è giusta la nazione
234900160616       //?o se voleva inserire la provincia
235000160616       //?Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
235100160616       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia,
235200160616       //?ha sbagliato campo
235300160616         IF  fior000o.nac <> *blanks and
235400160616            ((%subst(fior000o.nac:1:1) = *blanks and
235500160616              %subst(fior000o.nac:2:2) <> *blanks) or
235600160616             (%subst(fior000o.nac:1:2) <> *blanks and
235700160616              %subst(fior000o.nac:3:1) = *blanks));
235800160616           k_TBLcod = 'PR';
235900160616           k_TBLkey = fior000o.nac;
236000160616           chain %kds(K03tabel) TABEL00F;
236100160616           IF  %found(TABEL00F) and TBLflg = *blanks;
236200160616             k_TBLcod = '15';
236300160616             k_TBLkey = fior000o.nac;
236400160616             chain %kds(K03tabel) TABEL00F;
236500160616             IF  %found(TABEL00F);
236600160616               ds15 = TBLuni;
236700160616             ENDIF;
236800160616             reset fiora0M0;
236900160616             wIdMsg   = 'TIS0718';
237000160616             wIdCampo = 'VAONAC';
237100160616             wParm = 'Verificare la nazione del destinatario ' +
237200160616                      %trim(fior000o.nac) + '-' + %trim(ds15.§15des);
237300160616             exsr Forzatura;
237400160616             RiemetteVideo = *on;
237500160616             leavesr;
237600160616           ENDIF;
237700160616         ENDIF;
237800160616
237900160616       ENDSR;
238000160616
238100160616       //--------------------------------------------------------------
238200160616       //?Recupero i dati del destinatario.
238300160616       //--------------------------------------------------------------
238400160616       BEGSR  GetDatiDestinatario;
238500160616
238600160616         reset FIORB0OC;
238700160616
238800160616       //?Se non valido o bloccato vado via
238900160616       //?(ci pensa il FIORA00R a tornare l'errore)
239000160616         IF  not IsCodiceValido(FIORB0OC);
239100160616           leavesr;
239200160616         ENDIF;
239300160616
239400160616       //?Imposto i dati del mittente
239500160616         fior000o.rsc = fiorb0OC.nome;
239600160616         fior000o.inc = fiorb0OC.indirizzo;
239700160616         fior000o.cac = fiorb0OC.cap;
239800160616         fior000o.loc = fiorb0OC.localita;
239900160616         fior000o.prc = fiorb0OC.provincia;
240000160616         fior000o.nac = fiorb0OC.nazione;
240100160616
240200160616       //?Se paga mittente imposto i dati
240300160616         IF  fior000o.pag = FIORA00_PAGA_DESTINATARIO;
240400160616           fior000o.ksc = fiorb0OC.codksc;
240500160616           IF  fiorb0OC.codtariffa = 999;
240600160616             clear fior000o.ctr;
240700160616           ELSE;
240800160616             fior000o.ctr =
240900160616             %subst(%editc(fiorb0OC.codtariffa:'X'):2:3);
241000160616           ENDIF;
241100160616         ENDIF;
241200160616
241300160616       ENDSR;
241400160616
241500160616       //--------------------------------------------------------------
241600160616       //?Controlli sul chi paga.
241700160616       //--------------------------------------------------------------
241800160616       BEGSR  ChiPaga;
241900160616
242000160616       //?Se cambio il Chi Paga cancello il KSC e CTR
242100160616         IF  fior000i.pag <> fior000o.pag;
242200160616           clear fior000o.ksc;
242300160616           clear fior000o.ctr;
242400160616         ENDIF;
242500160616
242600160616       //?Se codice Ordinante = codice Mittente imposto in automatico
242700160616       //?che paga Mittente
242800160616         IF  fior000o.cra1 > 0 and fior000o.cra2 > 0 and
242900160616             fior000o.cor1 > 0 and fior000o.cor2 > 0 and
243000160616             fior000o.cra1 = fior000o.cor1 and
243100160616             fior000o.cra2 = fior000o.cor2 and
243200160616             fior000o.cra3 = fior000o.cor3 and
243300160616             fior000o.pag <> FIORA00_PAGA_MITTENTE;
243400160616           fior000o.pag = FIORA00_PAGA_MITTENTE;
243500160616         ENDIF;
243600160616
243700160616       ENDSR;
243800160608
243900160608       //--------------------------------------------------------------
244000170330       //?Controlli sulla data pronta merce.
244100160608       //--------------------------------------------------------------
244200170330       BEGSR  DataProntaMerce;
244300170404
244400170404       //?Se la data pronta merce a video è vuota la imposto = oggi
244500170404         IF  fior000o.dpm = 0;
244600170404           dataISO = %date(dataoggi:*iso);
244700170404           dataEUR = dataISO;
244800170404           fior000o.dpm = %dec(dataEUR);
244900170404         ENDIF;
245000160608
245100160608       //?Può capitare che a video la data venga inserita lunga 6
245200160608       //?prima di richiamare il FIORA00R la sistemo
245300170330         IF  fior000o.dpm > 0;
245400160608           clear wlbdat;
245500170330           G08dat = fior000o.dpm;
245600160608           xsrda8(wlbdat);
245700160608           IF  G08err = '1';
245800160608             reset fiora0M0;
245900170404             wIdMsg   = 'TIS0718';
246000170330             wIdCampo = 'VAODPM';
246100170404             wParm = 'Data Pronta Merce formalmente errata';
246200160608             exsr Messaggi;
246300160608             RiemetteVideo = *on;
246400160608             leavesr;
246500160608           ENDIF;
246600170330           fior000o.dpm = G08dat;
246700160608         ENDIF;
246800160608
246900160608       ENDSR;
247000160608
247100160608       //--------------------------------------------------------------
247200160608       //?Controlli sulla filiale ritiro.
247300160608       //--------------------------------------------------------------
247400160608       BEGSR  FilRitiro;
247500160608
247600160608         IF  fior000o.por <> *blanks;
247700160608           IF  %subst(fior000o.por:1:1) = *blanks;
247800160608             %subst(fior000o.por:1:1) = '0';
247900160608           ENDIF;
248000160608           IF  %subst(fior000o.por:2:1) = *blanks;
248100160608             %subst(fior000o.por:2:1) = '0';
248200160608           ENDIF;
248300160608           IF  %check(FIOR001_DIGITS:fior000o.por) > *zeros;
248400160608             reset fiora0M0;
248500160608             wIdMsg   = 'TIS0809';
248600160608             wIdCampo = 'VAOPOR';
248700160608             clear wParm;
248800160608             exsr Messaggi;
248900160608             RiemetteVideo = *on;
249000160608             fior000i.por = fior000o.por;
249100160608             clear fior000i.dpor;
249200160608           ENDIF;
249300160608         ENDIF;
249400160608
249500160608       ENDSR;
249600141020
249700141020       //--------------------------------------------------------------
249800141020       //?Controlli sulla filiale consegna.
249900141020       //--------------------------------------------------------------
250000141020       BEGSR  FilConsegna;
250100141020
250200141020         IF  fior000o.poc <> *blanks;
250300160608           IF  %subst(fior000o.poc:1:1) = *blanks;
250400160608             %subst(fior000o.poc:1:1) = '0';
250500160608           ENDIF;
250600160608           IF  %subst(fior000o.poc:2:1) = *blanks;
250700160608             %subst(fior000o.poc:2:1) = '0';
250800160608           ENDIF;
250900141020           IF  %check(FIOR001_DIGITS:fior000o.poc) > *zeros;
251000160608             reset fiora0M0;
251100141020             wIdMsg   = 'TIS0803';
251200141020             wIdCampo = 'VAOPOC';
251300141020             clear wParm;
251400141020             exsr Messaggi;
251500141020             RiemetteVideo = *on;
251600141020             fior000i.poc = fior000o.poc;
251700141020             clear fior000i.dpoc;
251800141020           ENDIF;
251900141020         ENDIF;
252000141020
252100141020       ENDSR;
252200160616
252300160616       //--------------------------------------------------------------
252400160616       //?Tasto ENTER.
252500160616       //--------------------------------------------------------------
252600160616       BEGSR  TastoEnter;
252700160616
252800160616         SELECT;
252900160616       //?Videata WHO
253000160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER and
253100160616               fior000i.video = FIOR000_VIDEO_WHOISORD;
253200160616           exsr ControllaWho;
253300160616       //?Tutte le altre videate
253400160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER and
253500160616               fior000i.video <> FIOR000_VIDEO_WHOISORD;
253600160616           exsr ControllaDatiVideo;
253700160616
253800160616         ENDSL;
253900160616
254000160616       ENDSR;
254100160616
254200160616       //--------------------------------------------------------------
254300160616       //?F02 = Dati DPD.
254400160616       //--------------------------------------------------------------
254500160616       BEGSR  TastoF02;
254600160616
254700160616
254800160616       ENDSR;
254900160616
255000160616       //--------------------------------------------------------------
255100160616       //?F04 = Alert Ritiro Merce.
255200160616       //--------------------------------------------------------------
255300160616       BEGSR  TastoF04;
255400160616
255500160616         fior000i.video = FIOR000_VIDEO_ALERTCOMM;
255600160616         FineVideo = *on;
255700160616
255800160616       ENDSR;
255900160615
256000160615       //--------------------------------------------------------------
256100160616       //?F05 = Altri Dati.
256200160615       //--------------------------------------------------------------
256300160615       BEGSR  TastoF05;
256400160616
256500160616         fior000i.video = FIOR000_VIDEO_ALTRIDATI;
256600160616         FineVideo = *on;
256700160615
256800160615       ENDSR;
256900160616
257000160616       //--------------------------------------------------------------
257100160616       //?F06 = Conferma.
257200160616       //--------------------------------------------------------------
257300160616       BEGSR  TastoF06;
257400160616
257500160616       //?Prima devo controllare i dati se Ok allora li convalido
257600160616         exsr ControllaDatiVideo;
257700170505         IF  FineVideo;
257800170505           leavesr;
257900170505         ENDIF;
258000170420
258100170420       //?F06 Dato in videata F05
258200170420       //?se non ci sono errori riferiti a questa videata ritorno alla
258300170420       //?videata principale
258400170420         IF  fior000i.video = FIOR000_VIDEO_ALTRIDATI and
258500170420             %lookup('VAOBLC':fiora0M0.skIdCampo) = 0 and
258600170420             %lookup('VAOMTC':fiora0M0.skIdCampo) = 0 and
258700170420             %lookup('VAOATT':fiora0M0.skIdCampo) = 0 and
258800170420             %lookup('CGI':fiora0M0.skIdCampo) = 0;
258900170420           FineVideo = *on;
259000170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
259100170420           leavesr;
259200170420         ENDIF;
259300170420       //?F06 Dato in videata F04
259400170420       //?se non ci sono errori riferiti a questa videata ritorno alla
259500170420       //?videata principale
259600170420         IF  fior000i.video = FIOR000_VIDEO_ALERTCOMM and
259700170420             %lookup('SMS':fiora0M0.skIdCampo) = 0 and
259800170420             %lookup('MAIL':fiora0M0.skIdCampo) = 0;
259900170420           FineVideo = *on;
260000170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
260100170420           wOKF06F04 = *on;
260200170420           leavesr;
260300170420         ENDIF;
260400170420       //?F06 Dato in videata F13
260500170420       //?se non ci sono errori riferiti a questa videata ritorno alla
260600170420       //?videata principale
260700170420         IF  fior000i.video = FIOR000_VIDEO_ALERTCONF and
260800170420             %lookup('SMSCONF':fiora0M0.skIdCampo) = 0 and
260900170420             %lookup('MAILCONF':fiora0M0.skIdCampo) = 0;
261000170420           FineVideo = *on;
261100170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
261200170420           wOKF06F13 = *on;
261300170420           leavesr;
261400170420         ENDIF;
261500160616
261600160616       //?Se ci sono errori esco
261700160616         IF  fiora0m0.NrMsg > 0;
261800160616           leavesr;
261900160616         ENDIF;
262000160616
262100160616       //?F06 Dato in videata principale
262200170420         IF  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
262300170414         //?Come prima cosa devo verificare gli ALERT
262400170414           exsr AlertORM;
262500170414           IF  FineVideo;
262600170414             leavesr;
262700170414           ENDIF;
262800160622           exsr ScriviFileORM;
262900170414         //?Se non ci sono errori in scittura riemetto la videata vuota
263000170414           IF  fiora0m0.NrMsg = 0;
263100170414           //?Imposto i dati nella ds di input
263200170414             exsr MoveDs00;
263300170420           //?Imposto l'ultimo ORM immesso
263400170420             fior000i.poeu = fiora01o.poe;
263500170420             fior000i.nsru = fiora01o.nsr;
263600170420             fior000i.noru = fiora01o.nor;
263700170420             fior000i.nrvu = fiora01o.nrv;
263800160622             FineVideo = *on;
263900160622           ENDIF;
264000170420         ENDIF;
264100160616
264200160616       ENDSR;
264300141007
264400141007       //--------------------------------------------------------------
264500160616       //?F07 = Ricerca del mittente.
264600141007       //--------------------------------------------------------------
264700160615       BEGSR  TastoF07;
264800141007
264900141007         clear FIOR81DS;
265000160608         IF  fior000o.cra1 > 0;
265100160608           fior81ds.I81fil = fior000o.cra1;
265200141007         ENDIF;
265300141007         IF  fior000o.rsr <> *blanks;
265400160608           fior81ds.I81rag = fior000o.rsr;
265500141007         ENDIF;
265600141007         fior81r (kpjba:FIOR81DS);
265700141007       //?Se torna errore emetto messaggio a video
265800141007         IF  fior81ds.O81err <> *blanks;
265900160608           reset fiora0M0;
266000141009           wIdMsg   = 'TIS0718';
266100141009           wIdCampo = 'VAOCRA';
266200141009           wParm = fior81ds.O81msg;
266300141009           exsr Messaggi;
266400141008           leavesr;
266500141007         ENDIF;
266600141007       //?Se torna F12 non faccio niente
266700141007         IF  fior81ds.O81ret = '1';
266800141007           leavesr;
266900141007         ENDIF;
267000141008
267100141007       //?Se arrivo qua vuol dire che è stata fatta una scelta
267200141007       //?quindi imposto già i dati del codice scelto
267300160608         fior000o.cra1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
267400160608         fior000o.cra2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
267500160608         fior000o.cra3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
267600141008
267700160608       //?Decodifico
267800160608         exsr Mittente;
267900160608
268000160608       //?imposto i dati dalla DS di Output del FIOR000R
268100160608       //?alla ds di input del FIOR000R
268200160608         eval-corr fior000i = fior000o;
268300160608         reset fior000i.formato;
268400160608         reset fior000i.versione;
268500160608
268600160608       //?Controllo
268700160608         exsr ControllaDatiVideo;
268800141015
268900141015       //?se torna errore ma non è relativo al mittente riemetto il video
269000141015       //?senza errori
269100141015         NrErrore = 1;
269200141015         FOR  NrErrore by 1 to fiora0m0.NrMsg;
269300141015           IF  NrErrore > 0 and
269400141015               fiora0m0.skIdCampo(NrErrore) = 'VAONCL';
269500141015             reset fiora0m0;
269600141015             leavesr;
269700141015           ENDIF;
269800141015         ENDFOR;
269900141007
270000141007       ENDSR;
270100141014
270200141015       //--------------------------------------------------------------
270300160616       //?F08 = Ricerca dell'ordinante.
270400141015       //--------------------------------------------------------------
270500160615       BEGSR  TastoF08;
270600141015
270700141015         clear FIOR81DS;
270800160608         IF  fior000o.cor1 > 0;
270900160608           fior81ds.I81fil = fior000o.cor1;
271000141015         ENDIF;
271100141015         IF  fior000o.rso <> *blanks;
271200160608           fior81ds.I81rag = fior000o.rso;
271300141015         ENDIF;
271400141015         fior81r (kpjba:FIOR81DS);
271500141015       //?Se torna errore emetto messaggio a video
271600141015         IF  fior81ds.O81err <> *blanks;
271700160608           reset fiora0M0;
271800141015           wIdMsg   = 'TIS0718';
271900141015           wIdCampo = 'VAOCOR';
272000141015           wParm = fior81ds.O81msg;
272100141015           exsr Messaggi;
272200141015           leavesr;
272300141015         ENDIF;
272400141015       //?Se torna F12 non faccio niente
272500141015         IF  fior81ds.O81ret = '1';
272600141015           leavesr;
272700141015         ENDIF;
272800141015
272900141015       //?Se arrivo qua vuol dire che è stata fatta una scelta
273000141015       //?quindi imposto già i dati del codice scelto
273100160608         fior000o.cor1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
273200160608         fior000o.cor2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
273300160608         fior000o.cor3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
273400141015
273500160608       //?decodifico
273600160608         exsr Ordinante;
273700160608
273800160608       //?imposto i dati dalla DS di Output del FIOR000R
273900160608       //?alla ds di input del FIOR000R
274000160608         eval-corr fior000i = fior000o;
274100160608         reset fior000i.formato;
274200160608         reset fior000i.versione;
274300160608
274400160608       //?controllo
274500160608         exsr ControllaDatiVideo;
274600141015
274700141015       ENDSR;
274800141015
274900141015       //--------------------------------------------------------------
275000160616       //?F09 = Ricerca del destinatario.
275100141015       //--------------------------------------------------------------
275200160615       BEGSR  TastoF09;
275300141015
275400141015         clear FIOR81DS;
275500160608         IF  fior000o.crc1 > 0;
275600160608           fior81ds.I81fil = fior000o.crc1;
275700141015         ENDIF;
275800141015         IF  fior000o.rsc <> *blanks;
275900160608           fior81ds.I81rag = fior000o.rsc;
276000141015         ENDIF;
276100141015         fior81r (kpjba:FIOR81DS);
276200141015       //?Se torna errore emetto messaggio a video
276300141015         IF  fior81ds.O81err <> *blanks;
276400160608           reset fiora0M0;
276500141015           wIdMsg   = 'TIS0718';
276600141015           wIdCampo = 'VAOCRC';
276700141015           wParm = fior81ds.O81msg;
276800141015           exsr Messaggi;
276900141015           leavesr;
277000141015         ENDIF;
277100141015       //?Se torna F12 non faccio niente
277200141015         IF  fior81ds.O81ret = '1';
277300141015           leavesr;
277400141015         ENDIF;
277500141015
277600141015       //?Se arrivo qua vuol dire che è stata fatta una scelta
277700141015       //?quindi imposto già i dati del codice scelto
277800160608         fior000o.crc1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
277900160608         fior000o.crc2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
278000160608         fior000o.crc3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
278100141015
278200160608       //?decodifico
278300160608         exsr Destinatario;
278400160608
278500160608       //?imposto i dati dalla DS di Output del FIOR000R
278600160608       //?alla ds di input del FIOR000R
278700160608         eval-corr fior000i = fior000o;
278800160608         reset fior000i.formato;
278900160608         reset fior000i.versione;
279000160608
279100160608       //?controllo
279200160608         exsr ControllaDatiVideo;
279300141015
279400141015       ENDSR;
279500160616
279600160616       //--------------------------------------------------------------
279700160616       //?F10 = Copia Numero Telefono.
279800160616       //--------------------------------------------------------------
279900160616       BEGSR  TastoF10;
280000160616
280100160616         SELECT;
280200160616       //?F10 dato in videata F04
280300160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
280400160616           fior000i.sms = fior000i.ter;
280500160616       //?F10 dato in videata F13
280600160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
280700160616           fior000i.smsconf = fior000i.ter;
280800160616         ENDSL;
280900160616
281000160616       ENDSR;
281100160615
281200160615       //--------------------------------------------------------------
281300160616       //?F12 = Ritorno.
281400160615       //--------------------------------------------------------------
281500160615       BEGSR  TastoF12;
281600160616
281700160616         Finevideo = *on;
281800160615
281900160615         SELECT;
282000160615       //?F12 dato in videata principale
282100160616         WHEN fior000i.video = FIOR000_VIDEO_PRINCIPALE;
282200160616           Fine = *on;
282300160615       //?F12 dato in videata F05
282400160616         WHEN fior000i.video = FIOR000_VIDEO_ALTRIDATI;
282500160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
282600160615       //?reimposto i dati salvati
282700160615           fior000i.blc = savblc;
282800160615           fior000i.att = savatt;
282900160615           fior000i.mtc = savmtc;
283000160615           fior000i.sto = savsto;
283100160615           fior000i.cgi = savcgi;
283200160615           fior000i.ffd = savffd;
283300160615           reset fiora0M0;
283400160616       //?F12 dato in videata F04
283500160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
283600160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
283700160616       //?reimposto i dati salvati
283800160616           fior000i.sms  = savsms;
283900160616           fior000i.mail = savmail;
284000160616           reset fiora0M0;
284100160616       //?F12 dato in videata F13
284200160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
284300160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
284400160616       //?reimposto i dati salvati
284500160616           fior000i.smsconf  = savsmsconf;
284600160616           fior000i.mailconf = savmailconf;
284700160616           reset fiora0M0;
284800160616       //?F12 dato in videata WHO
284900160616         WHEN fior000i.video = FIOR000_VIDEO_WHOISORD;
285000160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
285100160616
285200160616         ENDSL;
285300160615
285400160615       ENDSR;
285500160616
285600160616       //--------------------------------------------------------------
285700160616       //?F13 = Alert Conferma Prenotazione.
285800160616       //--------------------------------------------------------------
285900160616       BEGSR  TastoF13;
286000160616
286100160616         fior000i.video = FIOR000_VIDEO_ALERTCONF;
286200160616         FineVideo = *on;
286300160616
286400160616       ENDSR;
286500160620
286600160620       //--------------------------------------------------------------
286700160620       //?F14 = Visualizza Orari Servizio.
286800160620       //--------------------------------------------------------------
286900160620       BEGSR  TastoF14;
287000160620
287100160620       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
287200160620       //?un mittente codificato e siamo in immissione/copia
287300160620       //?non posso interrogare gli orari
287400160620         IF  fior000i.por = *zeros and fior000i.pkg = 0 and
287500160620             fior000i.vlm = 0 and fior000i.bnc = 0 and
287600160620             fior000i.cra1 = 0 and fior000i.cra2 = 0;
287700160620           wIdMsg   = 'TIS0718';
287800160620           wIdCampo = 'VAONCL';
287900160620           wParm = 'Per F14 serve la fil.ritiro e +
288000160620                    almeno 1 tra peso/vol./bancali';
288100160620           exsr Messaggi;
288200160620           leavesr;
288300160620         ENDIF;
288400160620
288500160620       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
288600160620       //?la filiale di ritiro
288700160620         IF  fior000i.por = *zeros;
288800160620           wIdMsg   = 'TIS0718';
288900160620           wIdCampo = 'VAOPOR';
289000160620           wParm = 'Per interrogare gli orari serve la filiale ritiro';
289100160620           exsr Messaggi;
289200160620           leavesr;
289300160620         ENDIF;
289400160620
289500160620       //?l'indirizzo completo del mittente
289600160620         IF  fior000i.car = *blanks or fior000i.lor = *blanks;
289700160620           wIdMsg   = 'TIS0718';
289800160620           wIdCampo = 'VAOCAR';
289900160620           clear wIdCampo;
290000160620           wParm = 'Per interrogare gli orari serve l''indirizzo del +
290100160620                    mittente';
290200160620           exsr Messaggi;
290300160620           leavesr;
290400160620         ENDIF;
290500160620
290600160620       //?Cerco eventuale ora presunta ritiro
290700160620       //?solo se non è immissione ORM
290800160620       //?e solo se ORM in distinta e NON quadrato
290900160620         clear fior01ds;
291000160620         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_TEL and
291100160620             prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_MAIL and
291200160620             fior000i.ndc > 0 and fior000i.fao < 600;
291300160620           fior01ds.IOR01fgs = %dec(fior000i.por:3:0);
291400160620           fior01ds.IOR01ndc = fior000i.ndc;
291500160620           fior01ds.IOR01poe = fior000i.poe;
291600160620           fior01ds.IOR01nsr = fior000i.nsr;
291700160620           fior01ds.IOR01nor = fior000i.nor;
291800160620           fior01ds.IOR01nrv = fior000i.nrv;
291900160620           fior01r (fior01ds);
292000160620         ENDIF;
292100160621
292200160621         fior000i.orapresrit = fior01ds.OOR01ora;
292300160621         fior000i.pickupcalc = fiora0O0.pickupcalc;
292400160620
292500160620       ENDSR;
292600170505
292700170505       //--------------------------------------------------------------
292800170505       //?F15 = Interrogazione Cappario DPD.
292900170505       //--------------------------------------------------------------
293000170505       BEGSR  TastoF15;
293100170505
293200170505       //?Recupero alcuni dati dalla tabella delle nazioni
293300170505         k_TBLcod = '15';
293400170505         k_TBLkey = fior000i.nar;
293500170505         chain %kds(K03tabel) TABEL00F;
293600170505         IF  %found(TABEL00F) and TBLflg = *blanks;
293700170505           ds15 = TBLuni;
293800170505         ENDIF;
293900170505         clear TISIE8DS;
294000170505         %subst(tisie8ds.ISIE8cap:(ds15.§15pcf):
294100170505               (%len(fior000i.car) - ds15.§15pcf)) =
294200170505         %subst(fior000i.car:(ds15.§15pcf):
294300170505               (%len(fior000i.car) - ds15.§15pcf));
294400170505         %subst(tisie8ds.ISIE8cap:(ds15.§15pcf + ds15.§15ecf):
294500170505               (%len(fior000i.car) - ds15.§15ecf)) = *blanks;
294600170505         tisie8ds.ISIE8dri = dataoggi;
294700170505         tisie8ds.ISIE8op0 = 'D01';
294800170505         tisie8ds.ISIE8nar = fior000i.nar;
294900170505         tisie8r (kpjba:tisie8ds);
295000170505
295100170505       //?Se torna errore emetto messaggio a video
295200170505         IF  tisie8ds.OSIE8err <> *blanks;
295300170505           reset fiora0M0;
295400170505           wIdMsg   = 'TIS0718';
295500170505           wIdCampo = 'VAOCAR';
295600170505           wParm = tisie8ds.OSIE8msg;
295700170505           exsr Messaggi;
295800170505           leavesr;
295900170505         ENDIF;
296000170505
296100170505       //?Ritorno eventuale cap scelto
296200170505         IF  tisie8ds.OSIE8capb <> *blanks;
296300170505           fior000i.car = tisie8ds.OSIE8capb;
296400170505           reset fiora0M0;
296500170505           reset wOkF15;
296600170505         ENDIF;
296700170505
296800170505       ENDSR;
296900160614
297000160614       //--------------------------------------------------------------
297100160616       //?F18 = Gestione Note.
297200160614       //--------------------------------------------------------------
297300160615       BEGSR  TastoF18;
297400160614
297500160614         clear FIOR06DS;
297600160614         fior06ds.I06dta = fior000o.dao;
297700160614         fior06ds.I06poe = fior000i.poe;
297800160614         fior06ds.I06nor = fior000i.nor;
297900160614         fior06ds.I06nsr = fior000i.nsr;
298000160614         fior06ds.I06nrv = fior000i.nrv;
298100160614         fior06ds.I06far = fior000i.fao;
298200160614         fior06ds.I06dai = dataoggi;
298300160614         fior06ds.I06ori = %dec(%time());
298400160614
298500160614         SELECT;
298600160614         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
298700160614               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
298800160614           fior06ds.I06flm = 'M';
298900170522       //?Devo passare la filiale emissione di input
299000170522       //?(può variare dalla filiale utente quando si cambia la fialie emissione nel chiamante)
299100170522           fior06ds.I06poe = fior000i.poei;
299200160614         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
299300160614           fior06ds.I06flm = 'V';
299400160614         ENDSL;
299500160614
299600160614         kpjba = prmkpjba;
299700160614         fior06r (kpjba:FIOR06DS);
299800160614
299900160614       ENDSR;
300000160621
300100160621       //--------------------------------------------------------------
300200160621       //?F20 = Simulazione Delivery.
300300160621       //--------------------------------------------------------------
300400160621       BEGSR  TastoF20;
300500160621
300600160621         clear tisi92ds;
300700160621         tisi92ds.d92op0 = 'ORM';
300800160621         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
300900160621           tisi92ds.d92lnp = %dec(fior000i.por:3:0);
301000160621         ENDIF;
301100160621         tisi92ds.d92nar = fior000i.nac;
301200160621         tisi92ds.d92cad = fior000i.cac;
301300160621         tisi92ds.d92lod = fior000i.loc;
301400160621         tisi92ds.d92ffd = fior000i.ffd;
301500160621         tisi92ds.d92ksc = fior000i.ksc;
301600160621         IF  fior000i.ctr <> *blanks and fior000i.ctr > *zeros;
301700160621           tisi92ds.d92ctr = %dec(fior000i.ctr:3:0);
301800160621         ENDIF;
301900160621         tisi92ds.d92ncl = fior000i.ncl;
302000160621         tisi92ds.d92pkb = fior000i.pkg;
302100160621         tisi92ds.d92vlf = fior000i.vlm;
302200160621         tisi92ds.d92ddt = fior000i.ddt;
302300160621         kpjbu = tisi92ds;
302400160621         tisi92r (kpjba);
302500160621
302600160621       ENDSR;
302700160616
302800160616       //--------------------------------------------------------------
302900160616       //?F24 = Altri Tasti Funzione.
303000160616       //--------------------------------------------------------------
303100160616       BEGSR  TastoF24;
303200160616
303300160620         IF  nrighe = 1;
303400160620           leavesr;
303500160620         ENDIF;
303600160620
303700160620         SELECT;
303800160620         WHEN  (nrighe = 2 and contartf = 2) or
303900160620               (nrighe = 3 and contartf = 3);
304000160620           fior000a.rigatfmob = rigatf1;
304100160620           contartf = 1;
304200160620           IF  nrighe = 2;
304300160620             fior000a.rigatf24 = FIOR001_F24_1_2;
304400160620           ELSE;
304500160620             fior000a.rigatf24 = FIOR001_F24_1_3;
304600160620           ENDIF;
304700160620
304800160620         WHEN  (nrighe = 2 and contartf = 1) or
304900160620               (nrighe = 3 and contartf = 1);
305000160620           fior000a.rigatfmob = rigatf2;
305100160620           contartf = 2;
305200160620           IF  nrighe = 2;
305300160620             fior000a.rigatf24 = FIOR001_F24_2_2;
305400160620           ELSE;
305500160620             fior000a.rigatf24 = FIOR001_F24_2_3;
305600160620           ENDIF;
305700160620
305800160620         WHEN  nrighe = 3 and contartf = 2;
305900160620           fior000a.rigatfmob = rigatf3;
306000160620           contartf = 3;
306100160620           fior000a.rigatf24 = FIOR001_F24_3_3;
306200160620
306300160620         ENDSL;
306400160616
306500160616       ENDSR;
306600170330
306700170330       //--------------------------------------------------------------
306800170330       //?RollUp = Tolgo 1 GG alla data Ritiro.
306900170330       //--------------------------------------------------------------
307000170331       BEGSR  TastoRollUp;
307100170403
307200170403       //?Se la data ritiro non è ancora stata calcolata non posso fare UP/DOWN
307300170403         IF  fior000i.dar = 0;
307400170403           reset fiora0M0;
307500170403           wIdMsg   = 'TIS0718';
307600170403           clear wIdCampo;
307700170403           wParm = 'ATTENZIONE!! +
307800170403                    PageUp/PageDown non possibile senza data ritiro';
307900170403           exsr Messaggi;
308000170403           RiemetteVideo = *on;
308100170403           leavesr;
308200170403         ENDIF;
308300170403
308400170403         clear XGIOLAVDS;
308500170403         clear wlbdat;
308600170403         dataEUR = %date(fior000i.dar:*eur);
308700170403         dataISO = dataEUR;
308800170403         IXGLdata = %dec(dataISO);
308900170403         IXGLadd  = 'S';
309000170403         IXGLgg   = 1;
309100170403         IXGLlav  = 'S';
309200170403         IXGLpa   = 'P';
309300170403         IXGLfil  = %int(fior000i.por);
309400170403         XGIOLAV (xgiolavds);
309500170403         IF  OXGLerr <> *blanks;
309600170403           leavesr;
309700170403         ENDIF;
309800170404
309900170404       //?Solo se immissione
310000170404         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
310100170404             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
310200170404         //?Se data ritiro inferiore alla data ritiro calcolata errore
310300170404           IF  OXGldata < fiora0O0.dataritiro and
310400170404               OXGLdata < fiora0O0.dataritmin;
310500170404             wIdMsg   = 'TIS0718';
310600170404             wIdCampo = 'VAODAR';
310700170404             wParm = 'Data ritiro inferiore a quella calcolata.';
310800170404             exsr Messaggi;
310900170404             leavesr;
311000170404           ENDIF;
311100170404         ENDIF;
311200170404
311300170404       //?Supera la data max consentita
311400170404         IF  OXGLdata > fiora0O0.dataritmax;
311500170404           wIdMsg   = 'TIS0718';
311600170404           wIdCampo = 'VAODAR';
311700170404           wParm = 'Data ritiro superiore alla data max.';
311800170404           exsr Messaggi;
311900170404           leavesr;
312000170404         ENDIF;
312100170404
312200170404       //?Non può essere inferiore alla data immissione ORM
312300170404         IF  OXGLdata < fior001i.dao;
312400170404           wIdMsg   = 'TIS0718';
312500170404           wIdCampo = 'VAODAR';
312600170404           wParm = 'Data ritiro inferioe alla data immissione.';
312700170404           exsr Messaggi;
312800170404           leavesr;
312900170404         ENDIF;
313000170404
313100170403         dataISO = %date(OXGLdata:*iso);
313200170403         dataEUR = dataISO;
313300170403         fior000i.dar =  %dec(dataEUR);
313400170403
313500170403         fior000a.TastoVideo = FIOR000_TASTO_ENTER;
313600170403         exsr TastoEnter;
313700170330
313800170330       ENDSR;
313900170330
314000170330       //--------------------------------------------------------------
314100170330       //?RollDown = Aggiungo 1 GG alla data Ritiro.
314200170330       //--------------------------------------------------------------
314300170331       BEGSR  TastoRollDown;
314400170403
314500170403       //?Se la data ritiro non è ancora stata calcolata non posso fare UP/DOWN
314600170403         IF  fior000i.dar = 0;
314700170403           reset fiora0M0;
314800170403           wIdMsg   = 'TIS0718';
314900170403           clear wIdCampo;
315000170403           wParm = 'ATTENZIONE!! +
315100170403                    PageUp/PageDown non possibile senza data ritiro';
315200170403           exsr Messaggi;
315300170403           RiemetteVideo = *on;
315400170403           leavesr;
315500170403         ENDIF;
315600170403
315700170403         clear XGIOLAVDS;
315800170403         clear wlbdat;
315900170403         dataEUR = %date(fior000i.dar:*eur);
316000170403         dataISO = dataEUR;
316100170403         IXGLdata = %dec(dataISO);
316200170403         IXGLsub  = 'S';
316300170403         IXGLgg   = 1;
316400170403         IXGLlav  = 'S';
316500170403         IXGLpa   = 'P';
316600170403         IXGLfil  = %int(fior000i.por);
316700170403         XGIOLAV (xgiolavds);
316800170403         IF  OXGLerr <> *blanks;
316900170403           leavesr;
317000170403         ENDIF;
317100170404
317200170404       //?Solo se immissione
317300170404         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
317400170404             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
317500170404         //?Se data ritiro inferiore alla data ritiro calcolata errore
317600170404           IF  OXGldata < fiora0O0.dataritiro and
317700170404               OXGLdata < fiora0O0.dataritmin;
317800170404             wIdMsg   = 'TIS0718';
317900170404             wIdCampo = 'VAODAR';
318000170404             wParm = 'Data ritiro inferiore a quella calcolata.';
318100170404             exsr Messaggi;
318200170404             leavesr;
318300170404           ENDIF;
318400170404         ENDIF;
318500170404
318600170404       //?Supera la data max consentita
318700170404         IF  OXGLdata > fiora0O0.dataritmax;
318800170404           wIdMsg   = 'TIS0718';
318900170404           wIdCampo = 'VAODAR';
319000170404           wParm = 'Data ritiro superiore alla data max.';
319100170404           exsr Messaggi;
319200170404           leavesr;
319300170404         ENDIF;
319400170404
319500170404       //?Non può essere inferiore alla data immissione ORM
319600170404         IF  OXGLdata < fior001i.dao;
319700170404           wIdMsg   = 'TIS0718';
319800170404           wIdCampo = 'VAODAR';
319900170404           wParm = 'Data ritiro inferiore alla data immissione.';
320000170404           exsr Messaggi;
320100170404           leavesr;
320200170404         ENDIF;
320300170404
320400170403         dataISO = %date(OXGLdata:*iso);
320500170403         dataEUR = dataISO;
320600170403         fior000i.dar =  %dec(dataEUR);
320700170403
320800170403         fior000a.TastoVideo = FIOR000_TASTO_ENTER;
320900170403         exsr TastoEnter;
321000170330
321100170330       ENDSR;
321200160616
321300160616       //--------------------------------------------------------------
321400160616       //?ENTER = Controlli.
321500160616       //--------------------------------------------------------------
321600160616       BEGSR  ControllaDatiVideo;
321700170426
321800170426       //?Immissione ORM, videata principale, tasto enter o F6
321900170426       //?reimposto sempre la data e ora immissione ORM
322000170426         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
322100170426             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL and
322200170426             fior000i.video = FIOR000_VIDEO_PRINCIPALE and
322300170426             (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
322400170426              fior000a.TastoVideo = FIOR000_TASTO_F06);
322500170426           dataISO = %date(dataoggi:*iso);
322600170426           dataEUR = dataISO;
322700170426           fior000i.dao = %dec(dataEUR);
322800170426           fior000i.oao = %dec(%time());
322900170426         ENDIF;
323000160616
323100160616       //?Imposto i dati dalla ds di input del video
323200160616       //?alla ds di input del pgm di controllo
323300160616         exsr MoveDsA0I;
323400160616
323500160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
323600160616           RqsOpCode = FIORA00_RQSOPCODE_CONTROL;
323700160616         ENDIF;
323800160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
323900160616             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
324000160616           RqsOpCode = FIORA00_RQSOPCODE_WRITE;
324100160616         ENDIF;
324200160616
324300160616       //?Richiamo pgm di controllo dati
324400160616         MONITOR;
324500160616           FiorA00_Immetti( RqsOpCode
324600160616                          : RpyOpCode
324700160616                          : RpyIdMsg
324800160616                          : fiora0i0.formato
324900160616                          : fiora0i0
325000160616                          : %SIZE(fiora0i0)
325100160616                          : fiora0o0.formato
325200160616                          : fiora0o0
325300160616                          : %SIZE(fiora0o0)
325400160616                          : FIORA0M0.formato
325500160616                          : FIORA0M0
325600160616                          : %size(FIORA0M0)
325700160616                          : FIORA0F0.formato
325800160616                          : FIORA0F0
325900160616                          : %size(FIORA0F0)
326000160616                          );
326100160616           ON-ERROR;
326200160616           RpyOpCode = FIORA00_RPYOPCODE_ERROR;
326300160616         ENDMON;
326400160616
326500160616       //?Se torna errore emetto messaggio generico a video
326600160616         IF RpyOpCode < FIORA00_RPYOPCODE_DONE;
326700160616           IF  fiora0m0.nrmsg < 99;
326800160616             fiora0m0.nrmsg += 1;
326900160616             fiora0M0.skIdMsg(fiora0M0.nrmsg) = 'ORM*';
327000160616             fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'ORM*';
327100160616             fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
327200160616             fiora0M0.skTextMsg (fiora0M0.nrmsg)=
327300160616             'Grave errore nei controlli ORM';
327400160616             leavesr;
327500160616           ENDIF;
327600160616         //?Esco dal programma se ho più di 99 errori
327700160616           exsr RoutEnd;
327800160616         ENDIF;
327900160616
328000160616         //?Se arrivo qua vuol dire che i controlli sono andati bene
328100170419         //?quindi passo i dati della ds di Output del pgm di controllo
328200160616         //?nella ds di input del video
328300160616         exsr MoveDs000I;
328400160616
328500170419         //?A questo punto faccio ulteriori controlli in base alla videata
328600170419         //?e al tasto funzione
328700170419         SELECT;
328800160616         //?ENTER dato sulla videata principale
328900170419         //?o F6 dato sulla videata principale
329000170419         //?e sono in Immissione
329100170420         //?Se dai controlli torna un errore relativo ad una delle altre videate
329200170420         //?resetto tutti gli errori
329300170420         //?ed emetto errore di messaggi in sospeso su altre videate
329400160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE and
329500170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
329600170419                fior000a.TastoVideo = FIOR000_TASTO_F06) and
329700170419               (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
329800170420                prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
329900170420               (%lookup('VAOBLC':fiora0M0.skIdCampo) > 0 or
330000170419                %lookup('VAOMTC':fiora0M0.skIdCampo) > 0 or
330100170419                %lookup('VAOATT':fiora0M0.skIdCampo) > 0 or
330200170419                %lookup('CGI':fiora0M0.skIdCampo) > 0);
330300170420           nrmsgblc = %lookup('VAOBLC':fiora0M0.skIdCampo);
330400170420           nrmsgmtc = %lookup('VAOMTC':fiora0M0.skIdCampo);
330500170420           nrmsgatt = %lookup('VAOATT':fiora0M0.skIdCampo);
330600170420           nrmsgcgi = %lookup('CGI':fiora0M0.skIdCampo);
330700170420           IF  nrmsgblc = 1 or nrmsgmtc = 1 or nrmsgatt = 1 or
330800170420               nrmsgcgi = 1;
330900170420             reset fiora0M0;
331000170420             clear wIdCampo;
331100170420             wIdMsg  = 'TIS0847';
331200170420             clear wParm;
331300170420             exsr Messaggi;
331400170420             RiemetteVideo = *on;
331500170420           ENDIF;
331600170419         //?Se ho dei messaggi da emettere non faccio altri controlli
331700170419         WHEN  fiora0M0.nrmsg > 0;
331800170419         //?ENTER dato sulla videata principale
331900170419         //?o F6 dato sulla videata principale
332000170419         //?in Immissione
332100170419         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE and
332200170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
332300170419                fior000a.TastoVideo = FIOR000_TASTO_F06) and
332400170419               (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
332500170419                prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL);
332600170419           //?Se è un ORM commissionato
332700170419           IF  fior000i.comm = 'S';
332800170419             //?se non c'è l'ordinante devo richiederlo
332900170419             IF  fior000i.cor1 = 0 and fior000i.cor2 = 0 and
333000170419               fior000i.rso = *blanks;
333100170419               fior000i.video = FIOR000_VIDEO_WHOISORD;
333200170420               FineVideo = *on;
333300170419               leavesr;
333400170419             ENDIF;
333500170419             //?se non sono presenti i dati per Alert ORM commissionato
333600170419             //?e posso fare Alert richiedo i dati
333700170420             //?e non faccio vedere la data ritiro e gli orari servizio
333800170420             //?quindi azzero la data ritiro
333900170419             IF  fior000i.sms = *blanks and fior000i.mail = *blanks and
334000170419                 not EmessoAlertComm;
334100170419               fior000i.video = FIOR000_VIDEO_ALERTCOMM;
334200170420               FineVideo = *on;
334300170420               clear fior000i.dar;
334400170420               clear fior000i.anticipa;
334500170419               leavesr;
334600170419             ENDIF;
334700170419           ENDIF;
334800170419           //?Se è un ORM senza mittente e/o ordinante codificati
334900170419           //?se non sono presenti i dati per Alert Conferma Prenotazione
335000170419           //?richiedo i dati
335100170419           //?se non li ho mai emessi
335200170419           IF  fior000i.cra1 = 0 and fior000i.cra2 = 0 and
335300170419               fior000i.cor1 = 0 and fior000i.cor2 = 0 and
335400170419               fior000i.smsconf = *blanks and
335500170419               fior000i.mailconf = *blanks and
335600170419               not EmessoAlertConf;
335700170419             fior000i.video = FIOR000_VIDEO_ALERTCONF;
335800170420             FineVideo = *on;
335900170419             leavesr;
336000170419           ENDIF;
336100160616
336200160616         //?ENTER dato sulla videata F05
336300170419         //?o F6 dato sulla videata F05
336400160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI and
336500170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
336600170419                fior000a.TastoVideo = FIOR000_TASTO_F06);
336700160616           //?se torna errore relativo ai campi a video lo emetto
336800160616           //?se no pulisco errori e resto nella videata
336900160616           IF  %lookup('VAOBLC':fiora0M0.skIdCampo) = 0 and
337000160616               %lookup('VAOATT':fiora0M0.skIdCampo) = 0 and
337100160616               %lookup('VAOMTC':fiora0M0.skIdCampo) = 0 and
337200160616               %lookup('CGI':fiora0M0.skIdCampo) = 0;
337300160616             reset fiora0M0;
337400160616           ENDIF;
337500160616
337600160616         ENDSL;
337700160616
337800160616       ENDSR;
337900160616
338000160616       //--------------------------------------------------------------
338100160616       //?Controllo i dati del Video Who.
338200160616       //--------------------------------------------------------------
338300160616       BEGSR  ControllaWho;
338400160616
338500160616         SELECT;
338600160616       //?Non è stato immesso nessun tipo ordinante
338700160616         WHEN  fior000i.tipoord = *blanks;
338800160616
338900160616       //?Immesso mittente come tipo ordinante
339000160616       //?imposto i dati del mittente nell'ordinante
339100160616         WHEN  fior000i.tipoord = 'M';
339200160616           fior000i.cor1 = fior000i.cra1;
339300160616           fior000i.cor2 = fior000i.cra2;
339400160616           fior000i.cor3 = fior000i.cra3;
339500160616           fior000i.rso  = fior000i.rsr;
339600160616           fior000i.ino  = fior000i.inr;
339700160616           fior000i.loo  = fior000i.lor;
339800160616           fior000i.cao  = fior000i.car;
339900160616           fior000i.pro  = fior000i.prr;
340000160616           fior000i.nao  = fior000i.nar;
340100160616           fior000i.comm = 'N';
340200160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
340300160616           FineVideo = *on;
340400160616
340500160616       //?Immesso destinatario come tipo ordinante
340600160616       //?imposto i dati del destinatario nell'ordinante
340700160616         WHEN  fior000i.tipoord = 'D';
340800160616           fior000i.cor1 = fior000i.crc1;
340900160616           fior000i.cor2 = fior000i.crc2;
341000160616           fior000i.cor3 = fior000i.crc3;
341100160616           fior000i.rso  = fior000i.rsc;
341200160616           fior000i.ino  = fior000i.inc;
341300160616           fior000i.loo  = fior000i.loc;
341400160616           fior000i.cao  = fior000i.cac;
341500160616           fior000i.pro  = fior000i.prc;
341600160616           fior000i.nao  = fior000i.nac;
341700160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
341800160616           FineVideo = *on;
341900160616
342000160616       //?Immesso altro come tipo ordinante
342100160616       //?imposto a mano i dati dell'ordinante
342200160616         WHEN  fior000i.tipoord = 'A';
342300160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
342400160616           FineVideo = *on;
342500160616
342600160616         ENDSL;
342700160616
342800160616       ENDSR;
342900170414
343000170414       //--------------------------------------------------------------
343100170414       //?Controllo se devo riemettere le videata degli alert ORM.
343200170414       //--------------------------------------------------------------
343300170414       BEGSR  AlertORM;
343400170414
343500170414       //?Se ORM commissionato ed ho emesso la videata
343600170414       //?relativa all'alert ORM COMMISSIONATO (F04)
343700170414       //?ma non ho inserito nessun dato e non ho mai dato F6 sulla videata
343800170418       //?oppure se ORM commissionato e non ho mai emesso la videata e non
343900170418       //?ci sono i dati
344000170414       //?ripropongo la win prima della scrittura dei file
344100170418         IF  fior000i.comm = 'S' and
344200170420             EmessoAlertComm and not wOKF06F04 and
344300170418             fior000i.sms = *blanks and fior000i.mail = *blanks;
344400170414           fior000i.video = FIOR000_VIDEO_ALERTCOMM;
344500170414           FineVideo = *on;
344600170414           leavesr;
344700170414         ENDIF;
344800170420       //?Se ORM commissionato ed ho emesso la videata
344900170420       //?relativa all'alert ORM COMMISSIONATO (F04)
345000170420       //?ho inserito i dati e ho dato F6 sulla videata
345100170420       //?imposto il flag per scrittura file Alert
345200170420         IF  fior000i.comm = 'S' and
345300170420             EmessoAlertComm and
345400170420             (fior000i.sms <> *blanks or fior000i.mail <> *blanks);
345500170420           IF  fior000i.sms <> *blanks;
345600170420             fior000i.alertsms = 'S';
345700170420           ENDIF;
345800170420           IF  fior000i.mail <> *blanks;
345900170420             fior000i.alertmail = 'S';
346000170420           ENDIF;
346100170420         ENDIF;
346200170414
346300170414       //?Se ho emesso la videata relativa alla CONFERMA PRENOTAZIONE RITIRO (F13)
346400170414       //?ma non ho inserito nessun dato e non ho mai dato F6 sulla videata
346500170414       //?ripropongo la win prima della scrittura dei file
346600170420         IF  EmessoAlertConf and not wOKF06F13 and
346700170418             fior000i.smsconf = *blanks and fior000i.mailconf = *blanks;
346800170414           fior000i.video = FIOR000_VIDEO_ALERTCONF;
346900170414           FineVideo = *on;
347000170414           leavesr;
347100170414         ENDIF;
347200170420       //?Se ho emesso la videata relativa alla CONFERMA PRENOTAZIONE RITIRO (F13)
347300170420       //?ed ho inserito i dati e ho fatto F6 sulla videata
347400170420       //?imposto il flag per scrittura file Alert
347500170420         IF  EmessoAlertConf and
347600170420             (fior000i.smsconf <> *blanks or fior000i.mailconf <> *blanks);
347700170427           IF  fior000i.smsconf <> *blanks;
347800170427             fior000i.alertcsms = 'S';
347900170427           ENDIF;
348000170427           IF  fior000i.mailconf <> *blanks;
348100170427             fior000i.alertcmail = 'S';
348200170427           ENDIF;
348300170420         ENDIF;
348400170414
348500170414       ENDSR;
348600160622
348700160622       //--------------------------------------------------------------
348800160622       //?Scrivo l'ORM.
348900160622       //--------------------------------------------------------------
349000160622       BEGSR  ScriviFileORM;
349100160622
349200160622       //?Imposto i dati dalla ds di input del video
349300160622       //?alle ds del pgm di scrittura
349400160622         exsr MoveDsA0I;
349500160622         exsr MoveDsA01;
349600160622
349700160622         RqsOpCode = FIORA00_RQSOPCODE_WRITE;
349800160622
349900160622       //?Richiamo pgm di scrittura file
350000160622         MONITOR;
350100160622           FiorA01_Scrivi( RqsOpCode
350200160622                         : RpyOpCode
350300160622                         : RpyIdMsg
350400160622                         : fiora0i0.formato
350500160622                         : fiora0i0
350600160622                         : %SIZE(fiora0i0)
350700170420                         : fiora01i.formato
350800170420                         : fiora01i
350900170420                         : %SIZE(fiora01i)
351000170420                         : fiora01o.formato
351100170420                         : fiora01o
351200170420                         : %SIZE(fiora01o)
351300170420                         : prmkpjba
351400170420                         );
351500160622           ON-ERROR;
351600160622           RpyOpCode = FIORA00_RPYOPCODE_ERROR;
351700160622         ENDMON;
351800160622
351900160622       //?Se torna errore emetto messaggio generico a video
352000160622         IF RpyOpCode < FIORA00_RPYOPCODE_DONE;
352100160622           IF  fiora0m0.nrmsg < 99;
352200160622             fiora0m0.nrmsg += 1;
352300160622             fiora0M0.skIdMsg(fiora0M0.nrmsg) = 'ORM*';
352400160622             fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'ORM*';
352500160622             fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
352600160622             fiora0M0.skTextMsg (fiora0M0.nrmsg)=
352700160622             'Grave errore in scrittura ORM';
352800160622             leavesr;
352900160622           ENDIF;
353000160622         //?Esco dal programma se ho più di 99 errori
353100160622           exsr RoutEnd;
353200160622         ENDIF;
353300160622
353400160622       ENDSR;
353500160616
353600160616       //--------------------------------------------------------------
353700160616       //?Imposto i campi nella DS di input. (pgm FIORA00R)
353800160616       //--------------------------------------------------------------
353900160616       BEGSR  MoveDsA0I;
354000160616
354100160616         reset FIORA0I0;
354200160616         fiora0i0.VAOpoe = fior000i.poe;
354300170522       //?Se immissione manuale passo la filiale emissione di input
354400170522         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
354500170522             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
354600170522           fiora0i0.VAOpoe = fior000i.poei;
354700170522         ENDIF;
354800160616         dataEUR = %date(fior000i.dao:*eur);
354900160616         dataISO = dataEUR;
355000160616         fiora0i0.VAOdao = %dec(dataISO);
355100160616         fiora0i0.VAOoao = fior000i.oao;
355200160616         fiora0i0.VAOcor =  (fior000i.cor1 * 10000000);
355300160616         fiora0i0.VAOcor += (fior000i.cor2 * 1000);
355400160616         fiora0i0.VAOcor += (fior000i.cor3);
355500160616         fiora0i0.VAOrso = fior000i.rso;
355600160616         fiora0i0.VAOino = fior000i.ino;
355700160616         fiora0i0.VAOcao = fior000i.cao;
355800160616         fiora0i0.VAOloo = fior000i.loo;
355900160616         fiora0i0.VAOpro = fior000i.pro;
356000160616         fiora0i0.VAOnao = fior000i.nao;
356100160616         fiora0i0.VAOcra =  (fior000i.cra1 * 10000000);
356200160616         fiora0i0.VAOcra += (fior000i.cra2 * 1000);
356300160616         fiora0i0.VAOcra += (fior000i.cra3);
356400160616         fiora0i0.VAOrsr = fior000i.rsr;
356500160616         fiora0i0.VAOinr = fior000i.inr;
356600160616         fiora0i0.VAOcar = fior000i.car;
356700160616         fiora0i0.VAOlor = fior000i.lor;
356800160616         fiora0i0.VAOprr = fior000i.prr;
356900160616         fiora0i0.VAOnar = fior000i.nar;
357000160616         fiora0i0.VAOcrc =  (fior000i.crc1 * 10000000);
357100160616         fiora0i0.VAOcrc += (fior000i.crc2 * 1000);
357200160616         fiora0i0.VAOcrc += (fior000i.crc3);
357300160616         fiora0i0.VAOrsc = fior000i.rsc;
357400160616         fiora0i0.VAOinc = fior000i.inc;
357500160616         fiora0i0.VAOcac = fior000i.cac;
357600160616         fiora0i0.VAOloc = fior000i.loc;
357700160616         fiora0i0.VAOprc = fior000i.prc;
357800160616         fiora0i0.VAOnac = fior000i.nac;
357900160616         fiora0i0.VAOpag = fior000i.pag;
358000160616         fiora0i0.VAOrer = fior000i.rer;
358100160616         fiora0i0.VAOter = fior000i.ter;
358200160616         fiora0i0.VAOffd = fior000i.ffd;
358300170330         IF  fior000i.dpm > 0;
358400170330           dataEUR = %date(fior000i.dpm:*eur);
358500160616           dataISO = dataEUR;
358600170404           fiora0i0.datapm = %dec(dataISO);
358700160616         ENDIF;
358800170404         IF  fior000i.dar > 0;
358900170404           dataEUR = %date(fior000i.dar:*eur);
359000170404           dataISO = dataEUR;
359100170404           fiora0i0.VAOdar = %dec(dataISO);
359200170404         ENDIF;
359300160616         fiora0i0.VAOorr = fior000i.orr;
359400160616         fiora0i0.VAOncl = fior000i.ncl;
359500160616         fiora0i0.VAOpkg = fior000i.pkg;
359600160616         fiora0i0.VAOvlm = fior000i.vlm;
359700160616         fiora0i0.VAObnc = fior000i.bnc;
359800160616         fiora0i0.VAOblc = fior000i.blc;
359900160616         fiora0i0.VAOatt = fior000i.att;
360000160616         fiora0i0.VAOmtc = fior000i.mtc;
360100160616         fiora0i0.VAOnam = fior000i.nam;
360200160616         fiora0i0.VAOrfa = fior000i.rfa;
360300160616         fiora0i0.VAOno1 = fior000i.nota1;
360400160616         fiora0i0.VAOno2 = fior000i.nota2;
360500160616         fiora0i0.VAOksc = fior000i.ksc;
360600160616         fiora0i0.VAOctr = fior000i.ctr;
360700160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
360800160616           fiora0i0.VAOpor = %int(fior000i.por);
360900160616         ELSE;
361000160616           clear fiora0i0.VAOpor;
361100160616         ENDIF;
361200160616         IF  fior000i.poc <> *blanks and fior000i.poc > *zeros;
361300160616           fiora0i0.VAOpoc = %int(fior000i.poc);
361400160616         ELSE;
361500160616           clear fiora0i0.VAOpoc;
361600160616         ENDIF;
361700160616         fiora0i0.VAOsto = fior000i.sto;
361800160616         fiora0i0.VAOspi = fior000i.spi;
361900160616         fiora0i0.SceltaNTW = fior000i.NTWDPD;
362000160616         fiora0i0.DalleAm = fior000i.DalleAm;
362100160616         fiora0i0.AlleAm  = fior000i.AlleAm;
362200160616         fiora0i0.DallePm = fior000i.DallePm;
362300160616         fiora0i0.AllePm  = fior000i.AllePm;
362400160616         fiora0i0.Contattot = fior000i.comm;
362500170420         fiora0i0.AlertMail = fior000i.AlertMail;
362600170420         fiora0i0.AlertSms = fior000i.AlertSms;
362700160616         fiora0i0.Mail = fior000i.Mail;
362800160616         fiora0i0.Sms  = fior000i.Sms;
362900160616         fiora0i0.fgs = fior000i.poei;
363000160616         fiora0i0.tor = fior000i.tor;
363100170419         fiora0i0.tgi = fior000i.tgi;
363200160616         fiora0i0.cgi = fior000i.cgi;
363300160616         fiora0i0.tco = fior000i.tco;
363400170427         IF  fior000i.AlertcMail = 'S';
363500170427           fiora0i0.AlertConf = fior000i.AlertcMail;
363600170427         ENDIF;
363700170427         IF  fior000i.AlertcSms = 'S';
363800170427           fiora0i0.AlertConf = fior000i.AlertcSms;
363900170427         ENDIF;
364000170420         fiora0i0.smsconf = fior000i.smsconf;
364100160620         fiora0i0.mailconf = fior000i.mailconf;
364200170420         fiora0i0.peso = fior000i.peso;
364300170420         fiora0i0.volume = fior000i.volume;
364400160616
364500160616       //?Se immissione
364600160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
364700160616             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
364800160616
364900160616       //?Imposto i dati del FLO
365000160616           exsr DatiFLO;
365100160616
365200160616         ENDIF;
365300160616
365400160616       ENDSR;
365500160616
365600160616       //--------------------------------------------------------------
365700160616       //?Imposto i campi nella DS di input. (FIOR000R)
365800160616       //--------------------------------------------------------------
365900160616       BEGSR  MoveDs000I;
366000160616
366100160616         fior000i.rsr = fiora0o0.VAOrsr;
366200160616         fior000i.inr = fiora0o0.VAOinr;
366300160616         fior000i.car = fiora0o0.VAOcar;
366400160616         fior000i.lor = fiora0o0.VAOlor;
366500160616         fior000i.prr = fiora0o0.VAOprr;
366600160616         fior000i.nar = fiora0o0.VAOnar;
366700160616         fior000i.cor1 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):1:3):3:0);
366800160616         fior000i.cor2 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):4:4):4:0);
366900160616         fior000i.cor3 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):8:3):3:0);
367000160616         fior000i.rso = fiora0o0.VAOrso;
367100160616         fior000i.ino = fiora0o0.VAOino;
367200160616         fior000i.cao = fiora0o0.VAOcao;
367300160616         fior000i.loo = fiora0o0.VAOloo;
367400160616         fior000i.pro = fiora0o0.VAOpro;
367500160616         fior000i.nao = fiora0o0.VAOnao;
367600160616         fior000i.rsc = fiora0o0.VAOrsc;
367700160616         fior000i.inc = fiora0o0.VAOinc;
367800160616         fior000i.cac = fiora0o0.VAOcac;
367900160616         fior000i.loc = fiora0o0.VAOloc;
368000160616         fior000i.prc = fiora0o0.VAOprc;
368100160616         fior000i.nac = fiora0o0.VAOnac;
368200160616         fior000i.rer = fiora0o0.VAOrer;
368300160616         fior000i.ter = fiora0o0.VAOter;
368400160616         fior000i.anticipa = fiora0o0.anticipa;
368500170404         dataISO = %date(fiora0o0.datapm:*iso);
368600170404         dataEUR = dataISO;
368700170404         fior000i.dpm = %dec(dataEUR);
368800170404         IF  fiora0o0.VAOdar > 0;
368900160616           dataISO = %date(fiora0o0.VAOdar:*iso);
369000160616           dataEUR = dataISO;
369100160616           fior000i.dar = %dec(dataEUR);
369200160616         ELSE;
369300160616           clear fior000i.dar;
369400160616           clear fior000i.anticipa;
369500160616         ENDIF;
369600160616         fior000i.orr = fiora0o0.VAOorr;
369700160616         fior000i.nam = fiora0o0.VAOnam;
369800160616         fior000i.rfa = fiora0o0.VAOrfa;
369900160616         fior000i.nota1 = fiora0o0.VAOno1;
370000160616         fior000i.nota2 = fiora0o0.VAOno2;
370100160616         fior000i.ksc = fiora0o0.VAOksc;
370200160616         fior000i.ctr = fiora0o0.VAOctr;
370300160616         fior000i.ntwdpd = fiora0o0.sceltantw;
370400160616         fior000i.por = %editc(fiora0o0.VAOpor:'X');
370500170420         fior000i.AlertMail = fiora0o0.AlertMail;
370600170420         fior000i.AlertSms = fiora0o0.AlertSms;
370700160616         fior000i.Mail = fiora0o0.Mail;
370800160616         fior000i.Sms  = fiora0o0.Sms;
370900160616         fior000i.orainizio = fiora0o0.orainizio;
371000160616         fior000i.orafine = fiora0o0.orafine;
371100170426         fior000i.oraestesa = fiora0o0.oraestesa;
371200160616         fior000i.comm = fiora0O0.Contattot;
371300160616         fior000i.spi = fiora0o0.VAOspi;
371400170419         fior000i.tgi = fiora0o0.tgi;
371500160616         fior000i.cgi = fiora0o0.cgi;
371600170427         fior000i.AlertcMail = fiora0o0.AlertConf;
371700170427         fior000i.AlertcSms = fiora0o0.AlertConfs;
371800170331         fior000i.Mailconf = fiora0o0.Mailconf;
371900170331         fior000i.Smsconf  = fiora0o0.Smsconf;
372000170420         fior000i.Peso = fiora0o0.Peso;
372100170420         fior000i.Volume = fiora0o0.Volume;
372200160616
372300160616         dORM01 = fiora0o0.VAOflo;
372400160616
372500160616       //?Se immissione
372600160616       //?e Ordinante = Mittente e paga Ordinante in automatico
372700160616       //?imposto che paga il mittente
372800160616         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
372900160616              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
373000160616             fior000i.pag = FIORA00_PAGA_ORDINANTE and
373100160616             fior000i.cra1 > 0 and fior000i.cra2 > 0 and
373200160616             fior000i.cor1 = fior000i.cra1 and
373300160616             fior000i.cor2 = fior000i.cra2 and
373400160616             fior000i.cor3 = fior000i.cra3;
373500160616           fior000i.pag = FIORA00_PAGA_MITTENTE;
373600160616         ENDIF;
373700160616
373800160616       //?Se immissione
373900160616       //?e dal pgm dei controlli torna errore sulle quantità
374000160616       //?non devo far vedere la data ritiro
374100160616         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
374200160616              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
374300160616             (%lookup('VAONCL':fiora0M0.skIdCampo) > 0 or
374400160616              %lookup('VAOPKG':fiora0M0.skIdCampo) > 0 or
374500160616              %lookup('VAOVLM':fiora0M0.skIdCampo) > 0 or
374600160616              %lookup('VAOBNC':fiora0M0.skIdCampo) > 0);
374700160616           clear fior000i.dar;
374800160616           clear fior000i.anticipa;
374900160616         ENDIF;
375000170505
375100170505       //?Se immissione manuale
375200170505       //?e i flag per invio Alert Conferma Prenotazione Ritiro
375300170505       //?sono tutti e 2 a 'N' li pulisco in modo da non generare
375400170505       //?inutilmente il rcd 'G' in scrittura
375500170505         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
375600170505              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
375700170505              fior000i.Mailconf = *blanks and fior000i.Smsconf = *blanks;
375800170505           clear fior000i.AlertcMail;
375900170505           clear fior000i.AlertcSms;
376000170505         ENDIF;
376100160616
376200160616       //?Se dal pgm di controllo torna errore sul cap DPD attivo F15
376300160616         IF  %lookup('TIS0811':fiora0M0.skIdMsg) > 0;
376400160616           wOkF15 = *on;
376500160616         ELSE;
376600160616           wOkF15 = *off;
376700160616         ENDIF;
376800160616
376900160616       ENDSR;
377000160622
377100160622       //--------------------------------------------------------------
377200160622       //?Imposto i campi nella DS di input. (pgm FIORA01R)
377300160622       //--------------------------------------------------------------
377400160622       BEGSR  MoveDsA01;
377500160622
377600170420         reset fiora01I;
377700170420         fiora01I.utente = fior001i.utente;
377800170420         fiora01I.filute = fior001i.filute;
377900170420         fiora01I.orainizio = fiora0O0.orainizio;
378000170426         fiora01I.orafine = fiora0O0.orafine;
378100170505       //?Se immissione manuale
378200170505         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
378300170505             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
378400170505         //?Se l'ora pronta merce è maggiore dell'ora inizio servizio
378500170505         //?e minore dell'ora fine e la data ritiro = data pronta merce
378600170505         //?imposto ora inizio servizio = ora pronta merce
378700170505           IF  fiora0O0.vaoorr > fiora0O0.orainizio and
378800170505               fiora0O0.vaoorr < fiora0O0.orafine and
378900170505               fiora0O0.vaodar = fiora0i0.datapm;
379000170505             fiora01i.orainizio = fiora0O0.vaoorr;
379100170505           ENDIF;
379200170505         //?Se l'ora pronta merce è maggiore dell'ora fine servizio
379300170505         //?e la data ritiro = data pronta merce
379400170505         //?imposto ora fine servizio = ora pronta merce
379500170505           IF  fiora0O0.vaoorr > fiora0O0.orafine and
379600170505               fiora0O0.vaodar = fiora0i0.datapm;
379700170505             fiora01i.orafine = fiora0O0.vaoorr;
379800170505           ENDIF;
379900170505         ENDIF;
380000170426         fiora01I.oraestesa = fiora0O0.oraestesa;
380100170420         fiora01I.oratappo = ddft.d§DFTorrtp;
380200170420         fiora01I.oracutoff = fiora0O0.oranocomm;
380300170420         fiora01I.ddt = fior000i.ddt;
380400170414         IF  wOKF06F04;
380500170420           fiora01I.okf06f04 = 'S';
380600170414         ENDIF;
380700170414         IF  wOKF06F13;
380800170420           fiora01I.okf06f13 = 'S';
380900170414         ENDIF;
381000170505         fiora01i.alertcmail = fior000i.alertcmail;
381100170505         fiora01i.alertcsms = fior000i.alertcsms;
381200170505
381300170505       //?Se immissione manuale e i dati per Alert Conferma Prenotazione Ritiro
381400170505       //?non sono presenti NON imposto niente nei relativi flag a questo modo
381500170505       //?non viene scritto il rcd 'G', sarebbe inutile
381600170505         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
381700170505             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
381800170505           IF  fiora0i0.mailconf = *blanks and fiora0i0.smsconf = *blanks;
381900170505             clear fiora01i.alertcmail;
382000170505             clear fiora01i.alertcsms;
382100170505           ENDIF;
382200170505         ENDIF;
382300170526
382400170526       //?Dati necessari per memorizzare data ritiro calcolata, se posticipo,
382500170526       //?se anticipo, se anticipato e di quanto anticipato
382600170526         fiora01I.dataritiro = fiora0O0.dataritiro;
382700170526         fiora01I.dataritmin = fiora0O0.dataritmin;
382800170526         fiora01I.posticipo  = fiora0O0.posticipo;
382900160622
383000160622       ENDSR;
383100141017
383200141017       //--------------------------------------------------------------
383300141017       //?Recupero i dati per il campo FLO.
383400141017       //--------------------------------------------------------------
383500141017       BEGSR  DatiFLO;
383600141017
383700141017         wDatiFLO = *off;
383800141017         clear dPVO;
383900141017
384000141017       //?Imposto il codice Ordinante
384100160608         wCodice  = (fior000o.cor1 * 10000000);
384200160608         wCodice += (fior000o.cor2 * 1000);
384300160608         wCodice += (fior000o.cor3);
384400141017
384500141017       //?Prima provo a cercare con il codice ordinante lungo 10
384600141017         IF  wCodice > 0;
384700141017           keytabella = %editc(wCodice:'X');
384800141017           IF  getTabella (c_tntbe:'PVO':keytabella:'O':
384900141017                           *blanks:*blanks:
385000141017                           *omit:*omit:*omit:*omit:*omit:
385100141017                           *omit:*omit:*omit:*omit:*omit:
385200141017                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
385300141017               = *zeros and ds_TNTBE.TBEatb = *blanks;
385400141017             dPVO = ds_TNTBE.TBEuni;
385500141017             wDatiFLO = *on;
385600141017           ENDIF;
385700141017
385800141017       //?Se non trovo con Ordinante lungo 10
385900141017       //?provo a cercare con il codice Ordinante lungo 7
386000141017           IF  not wDatiFLO;
386100141017             keytabella = %editc(wCodice:'X');
386200141017             %subst(keytabella:8:3) = *blanks;
386300141017             IF  getTabella (c_tntbe:'PVO':keytabella:'O':
386400141017                           *blanks:*blanks:
386500141017                             *omit:*omit:*omit:*omit:*omit:
386600141017                             *omit:*omit:*omit:*omit:*omit:
386700141017                             ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
386800141017                 = *zeros and ds_TNTBE.TBEatb = *blanks;
386900141017               dPVO = ds_TNTBE.TBEuni;
387000141017               wDatiFLO = *on;
387100141017             ENDIF;
387200141017           ENDIF;
387300141017         ENDIF;
387400141017
387500141017       //?Se non trovo con Ordinante lungo 10 e lungo 7
387600141017       //?provo a cercare con il codice KSC
387700141017         IF  not wDatiflo;
387800141017           keytabella = %editc(fiora0i0.vaoksc:'X');
387900141017           IF  getTabella (c_tntbe:'PVO':keytabella:'K':
388000141017                           *blanks:*blanks:
388100141017                           *omit:*omit:*omit:*omit:*omit:
388200141017                           *omit:*omit:*omit:*omit:*omit:
388300141017                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
388400141017               = *zeros and ds_TNTBE.TBEatb = *blanks;
388500141017             dPVO = ds_TNTBE.TBEuni;
388600141017             wDatiFLO = *on;
388700141017           ENDIF;
388800141017         ENDIF;
388900141017
389000141017       //?Se non trovo con nessun codice vado via
389100141017         IF  not wDatiFLO;
389200141017           leavesr;
389300141017         ENDIF;
389400141017
389500141017       //?Imposto i dati del FLO
389600141017         IF  dpvo.d§PVOcb = 'S';
389700141017           dorm01.§ORMcb = dpvo.d§PVOcb;
389800141017         ENDIF;
389900141017         IF  dpvo.d§PVOfr = 'S';
390000141017           dorm01.§ORMfr = dpvo.d§PVOfr;
390100141017         ENDIF;
390200141017         IF  dpvo.d§PVOks = 'S';
390300141017           dorm01.§ORMks = dpvo.d§PVOks;
390400141017         ENDIF;
390500141017         IF  dpvo.d§PVOfd = 'S';
390600141017           dorm01.§ORMfd = dpvo.d§PVOfd;
390700141017         ENDIF;
390800141017         IF  dpvo.d§PVOic <> *blanks;
390900141017           dorm01.§ORMic = dpvo.d§PVOic;
391000141017         ENDIF;
391100141017
391200141017         //?Se ORM con ricevuta è sempre ORM commissionato
391300141017         //?per questo devo anche ricalcolare la data di ritiro
391400141017         IF  dpvo.d§PVOsrm = 'S';
391500141017           dorm01.§ORsrm = dpvo.d§PVOsrm;
391600141017           dorm01.§ORcom = 'S';
391700141017           clear fiora0i0.VAOdar;
391800141022           fiora0i0.contattot = dorm01.§ORcom;
391900141017         ENDIF;
392000141017
392100141017         //?Se ORM con preavviso via MAIL (CEVA) e sempre ORM NON commissionato
392200141017         IF  dpvo.d§PVOpre = 'M';
392300141017           dorm01.§ORMpre = dpvo.d§PVOpre;
392400141017           dorm01.§ORcom = 'N';
392500141022           fiora0i0.contattot = dorm01.§ORcom;
392600141017         ENDIF;
392700141022
392800141022         //?Commissionato da personalizzazione del cliente
392900141022         IF  fiora0i0.contattot = *blanks;
393000141022           fiora0I0.contattot = dpvo.d§PVOcomc;
393100141022         ENDIF;
393200141017
393300141017         fiora0i0.VAOflo = dORM01;
393400141017
393500141017       ENDSR;
393600141009
393700141009       //--------------------------------------------------------------
393800141009       //?Richiamo Interrogazione cappario.
393900141009       //--------------------------------------------------------------
394000141009       BEGSR  call_FNLV14R;
394100141009
394200141009         clear FNLV14DS;
394300141009         fnlv14ds.I14dri = dataoggi;
394400141021         fnlv14ds.I14rsc = wRagSoc;
394500141009         fnlv14ds.I14ind = wIndirizzo;
394600141009         fnlv14ds.E14cap = wCap;
394700141009         fnlv14ds.E14loc = wLocalita;
394800141009         fnlv14ds.E14prv = wProvincia;
394900141009         fnlv14ds.E14nar = wNazione;
395000141009
395100141009         fnlv14r (kpjba:fnlv14ds);
395200141009
395300141009       ENDSR;
395400141021
395500141021       //--------------------------------------------------------------
395600141021       //?Routine per schierare le forzature nella ds FIORA0F0.
395700141021       //--------------------------------------------------------------
395800141021       BEGSR  Forzatura;
395900141021
396000160531       //?Se già caricato in sk non lo ricarico
396100160531         wNrForza = 1;
396200160531         FOR  wNrForza by 1 to fiora0F0.NrForza;
396300160531           IF  wNrForza > 0 and
396400160531               fiora0F0.skIdMsg(wNrForza) = wIdMsg and
396500160531               fiora0F0.skIdCampo(wNrForza) = wIdCampo and
396600160531               fiora0F0.skGiaForza(wNrForza) =
396700160531               FIORA00_GIAFORZA_SI;
396800160531             leavesr;
396900160531           ENDIF;
397000160531         ENDFOR;
397100160531
397200160531       //?Se ho già caricato 99 mesaggi esco
397300160531         IF  fiora0M0.nrmsg = 99;
397400160531           exsr RoutEnd;
397500160531           clear fiora0M0.nrmsg;
397600160531         ENDIF;
397700160531
397800160531       //?Imposto messaggio da forzare
397900160531         fiora0M0.nrmsg += 1;
398000160531         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
398100160531         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
398200160531         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
398300160531         IF  wParm <> *blanks;
398400160531           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
398500160531           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
398600160531         ELSE;
398700160531           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
398800160531           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
398900160531         ENDIF;
399000141021
399100141021       ENDSR;
399200141009
399300141009       //--------------------------------------------------------------
399400141009       //?Routine per schierare i messaggi nella ds FIORA0M0.
399500141009       //--------------------------------------------------------------
399600141009       BEGSR  Messaggi;
399700141009
399800141009       //?Se ho già caricato 99 messaggi esco
399900141009         IF  fiora0M0.nrmsg = 99;
400000141009           exsr RoutEnd;
400100141009           clear fiora0M0.nrmsg;
400200141009         ENDIF;
400300141009
400400141009         fiora0M0.nrmsg += 1;
400500141009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
400600160608         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
400700160531         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
400800141009         IF  wParm <> *blanks;
400900141009           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
401000141009           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
401100141009         ELSE;
401200141009           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
401300141009           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
401400141009         ENDIF;
401500141009
401600141009       ENDSR;
401700140414
401800140414       //--------------------------------------------------------------
401900140415      /end-free
402000140416
402100140415      *--------------------------------------------------
402200140618      * Procedure name: GetDesFiliale
402300140618      * Purpose:        Recupera la descrizione della filiale
402400140618      * Returns:        Descrizione
402500140618      * Parameter:      Filiale
402600140415      *--------------------------------------------------
402700140618     p GetDesFiliale   B
402800140618     d GetDesFiliale   PI            20a
402900140618     D  filiale                       3s 0 CONST
403000140415
403100140618      /free
403200140618       IF  filiale = *zeros;
403300140618         RETURN *blanks;
403400140618       ENDIF;
403500140618
403600140618       chain (filiale) AZORG01L;
403700140618       IF  not %found(AZORG01L);
403800140618         RETURN *blanks;
403900140618       ENDIF;
404000140618       IF  ORGfva <> *blanks;
404100140618         RETURN *blanks;
404200140618       ENDIF;
404300140618       IF  ORGfag <> 'F' and ORGfag <> 'A';
404400140618         RETURN *blanks;
404500140618       ENDIF;
404600140618
404700140618       RETURN ORGdes;
404800140415
404900140415      /end-free
405000140618     P GetDesFiliale   E
405100140618
405200140618      *--------------------------------------------------
405300140618      * Procedure name: IsAbilitata
405400140618      * Purpose:        Verifica se filiale abilitata
405500140618      * Returns:        ON - OFF
405600140618      * Parameter:      key tabelle VPO
405700140618      *                 Filiale 1 obbligo
405800140618      *                 Filiale 2 facoltativa
405900140618      *--------------------------------------------------
406000140618     p IsAbilitata     B
406100140618     d IsAbilitata     PI              n
406200140618     d keyVPO                        15a   const
406300140618     d filiale1                       3s 0 const
406400140618     d filiale2                       3s 0 const
406500140618     d                                     options(*nopass)
406600140618
406700140618      // ds per filiali abilitate
406800140618     d TRULVPODS     e ds
406900140618     d  skFilOk               16    765    dim(250)
407000140618
407100140618      // Pgm per reperire filiali abilitate
407200140618     d trulvpor        pr                  extpgm('TRULVPOR')
407300140618     d  trulvpods                          likeds(TRULVPODS)
407400140618
407500140618      /free
407600140618
407700140618       // se non c'è la key errore
407800140618       IF  keyVPO = *blanks;
407900140618         return *off;
408000140618       ENDIF;
408100140618
408200140618       // se non c'è prima filiale obbligatoria errore
408300140618       IF  filiale1 = *zeros;
408400140618         return *off;
408500140618       ENDIF;
408600140618
408700140618       // richiamo il Pgm per reperire filiali abilitate
408800140618       clear TRULVPODS;
408900140618       IVPOke1 = keyVPO;
409000140618       TRULVPOR (trulvpods);
409100140618
409200140618       // se al primo elemento c'è 999 tutti abilitati
409300140618       IF  skFilOk(1) = '999';
409400140618         return *on;
409500140618       ENDIF;
409600140618
409700140618       // se NON ho la seconda filiale verifico solo con la prima
409800140618       IF  %lookup(%editc(filiale1:'X'):skFilOk) > 0 and
409900140618           %parms = 2;
410000140618         return *on;
410100140618       ENDIF;
410200140618
410300140618       // ho tutte e 2 le filiali le verifico
410400140618       IF  %parms > 2 and filiale2 > 0 and
410500140618           %lookup(%editc(filiale1:'X'):skFilOk) > 0 and
410600140618           %lookup(%editc(filiale2:'X'):skFilOk) > 0;
410700140618         return *on;
410800140618       ENDIF;
410900140618
411000140618       return *off;
411100140618
411200140618      /end-free
411300140618     P IsAbilitata     E
411400160412
411500160412      *--------------------------------------------------
411600160412      * Procedure name: IsCodiceValido
411700160412      * Purpose:        Controlla codice
411800160412      * Returns:        ON - OFF
411900160412      *--------------------------------------------------
412000160412     p IsCodiceValido  B
412100160412     d IsCodiceValido  PI              n
412200160412     d  FIORB0O1                           likeds(FIORB0OO)
412300160412
412400160412      /copy gaitrasrc/srcconst,FIORB00R
412500160412     d isErrore        s               n   inz
412600160412     d RpyIdMsg        s             10i 0
412700160412     d RpyOpCode       s             10i 0
412800160412     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
412900160412     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
413000160412      /copy gaitrasrc/srcprotopr,FIORB00R
413100160412
413200160412      /free
413300160412
413400160412       reset isErrore;
413500160412       reset RpyIdMsg;
413600160412       reset RpyOpCode;
413700160412       reset fiorb0I1;
413800160412
413900160412       fiorb0I1.codice = wCodice;
414000160412       MONITOR;
414100160412         Fiorb00_Anagrafica( RqsOpCode
414200160412                           : RpyOpCode
414300160412                           : RpyIdMsg
414400160412                           : fiorb0I1.formato
414500160412                           : fiorb0I1
414600160412                           : %SIZE(fiorb0I1)
414700160412                           : fiorb0O1.formato
414800160412                           : fiorb0O1
414900160412                           : %SIZE(fiorb0O1)
415000160412                           );
415100160412         ON-ERROR;
415200160412           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
415300160412         ENDMON;
415400160412
415500160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE or
415600160519           RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND or
415700160519           RpyOpCode = FIORB00_RPYOPCODE_BLOCCATO;
415800160412         isErrore = *on;
415900160412       ENDIF;
416000160412
416100160412       IF  isErrore;
416200160412         return *off;
416300160412       ENDIF;
416400160412
416500160412       RETURN *on;
416600160412
416700160412      /END-FREE
416800160412     p IsCodiceValido  E
416900160616
417000160616      *--------------------------------------------------
417100160616      * Procedure name: GetDesGiro
417200160616      * Purpose:        Recupera la descrizione del giro
417300160616      * Returns:        Descrizione
417400160616      * Parameter:      Giro - Filiale Ritiro
417500160616      *--------------------------------------------------
417600160616     p GetDesGiro      B
417700160616     d GetDesGiro      PI            20a
417800160616     D  giro                         10a   CONST
417900160616     D  filiale                       3s 0 CONST
418000160616
418100160616      /free
418200160616       IF  giro = *blanks;
418300160616         RETURN *blanks;
418400160616       ENDIF;
418500160616       IF  filiale = *zeros;
418600160616         RETURN *blanks;
418700160616       ENDIF;
418800160616
418900160616       clear FIDG09DS;
419000160616       fidg09ds.D09iop0 = '001';
419100160616       fidg09ds.D09ifgs = filiale;
419200160616       fidg09ds.D09idat = dataoggi;
419300160616       fidg09ds.D09icgi = giro;
419400160616       fidg09ds.D09itug = 'R';
419500160616       kpjbu = FIDG09DS;
419600160616       fidg09r (kpjba);
419700160616       fidg09ds = kpjbu;
419800160616       IF  fidg09ds.D09oerr <> *blanks;
419900160616         RETURN *blanks;
420000160616       ENDIF;
420100160616
420200160616       RETURN fidg09ds.D09Odes;
420300160616
420400160616      /end-free
420500160616     P GetDesGiro      E
