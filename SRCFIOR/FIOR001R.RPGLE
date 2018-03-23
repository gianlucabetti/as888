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
004700170530     d FIOR001O      e ds                  QUALIFIED INZ(*EXTDFT)
004800140613     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
004900170420     d FIORA01O      e ds                  QUALIFIED INZ(*EXTDFT)
005000140613
005100140613       // -?Messaggi
005200140613     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
005300140613     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
005400140613     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
005500140613     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
005600140613     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
005700140613
005800140613       // -?Forzature
005900140613     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
006000140613     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006100140613     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006200140613     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
006300141010
006400141010       // -?Controllo Anagrafiche clienti
006500141010     d FIORB0OO      e ds                  EXTNAME(FIORB0O1)
006600141010     d                                     QUALIFIED INZ(*EXTDFT)
006700141010     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
006800141010     d                                     QUALIFIED INZ(*EXTDFT)
006900141010     d FIORB0OC      e ds                  EXTNAME(FIORB0O1)
007000141010     d                                     QUALIFIED INZ(*EXTDFT)
007100140618
007200140618       // -?Attributi campo e Tasti Funzione
007300140618     d FIOR000A      e ds                  QUALIFIED INZ(*EXTDFT)
007400170330     d  skTastiFun                    1a   dim(26) overlay(TastiFun)
007500140618     d  skIdAttr                      5a   dim(99) overlay(IdAttr)
007600140618     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007700160608
007800160608       // -?Controllo/Decodifica/Ricerca Giro
007900160608     d FIDG09DS      e ds                  QUALIFIED INZ
008000160620
008100160620       // -?Recupero ora presunta ritiro da PDA
008200160620     d FIOR01DS      e ds                  QUALIFIED INZ
008300160614
008400160614       // -?Gestione note
008500160620     d FIOR06DS      e ds                  QUALIFIED INZ
008600141021
008700141021       // -?Controllo indirizzo completo
008800141021     d FNLV13DS      e ds                  QUALIFIED INZ
008900141009
009000141009       // -?Interrogazione Cappario
009100141009     d FNLV14DS      e ds                  QUALIFIED INZ
009200141007
009300141007       // -?Interrogazione Anagrafica clienti ritiro
009400141007     d FIOR81DS      e ds                  QUALIFIED INZ
009500170505
009600170505       // -?Interrogazione Cappario DPD
009700170505     d TISIE8DS      e ds                  QUALIFIED INZ
009800160621
009900160621       // -?Simulazione Delivery
010000170505     d TISI92DS      e ds                  QUALIFIED INZ
010100141010
010200141010       // -?Controllo indirizzo
010300141010     d TISI95DS      e ds                  QUALIFIED INZ
010400141021     d TISI97DS      e ds                  QUALIFIED INZ
010500160321
010600160321       // -?Reperimento filiali gestite dall'utente
010700160321     d TRUL31DS      e ds                  QUALIFIED INZ
010800160321     d  POG                   10    759    dim(250)
010900170403
011000170403       // -?Aggiunge/Toglie gg/mm dalla data
011100170403     d XGIOLAVDS     e ds                  inz
011200140414
011300140414       // -?Campo VAOFLO
011400141008     d dORM01        e ds                  QUALIFIED INZ
011500140414
011600140414       // -?Kpjba
011700141008     d KPJBA         e ds                  INZ
011800140415
011900140415       // -?Tabella DFT
012000141008     d dDFT          e ds                  QUALIFIED INZ
012100141017
012200141017       // -?Tabella PVO - Personalizzazione FLO
012300141017     d dPVO          e ds                  QUALIFIED INZ
012400160607
012500160607       // -?Tabella 15 - Nazioni
012600160607     d ds15          e ds                  QUALIFIED INZ
012700160608
012800160608       // -?Parametri per pgm. controllo/inversione data?
012900160608     d wlbdat          ds                  inz
013000160608     d  G08dat                 1      8  0
013100160608     d  G08inv                 9     16  0
013200160608     d  G08err                17     17
013300160608     d  G08tgi                18     22  0
013400140414
013500140414       // -?Status
013600140414     d Psds           sds
013700140414     d  SDSpgm           *proc
013800140414
013900140414       //--------------------------------------------------------------
014000140414       //?Definizione schiere.
014100140414       //--------------------------------------------------------------
014200140414
014300140414       //--------------------------------------------------------------
014400140414       //?Definizione campi.
014500140414       //--------------------------------------------------------------
014600140414       // -?Flags boolenai
014700160616     d EmessoAlertComm...
014800160616     d                 s               n   inz(*off)
014900160616     d EmessoAlertConf...
015000160616     d                 s               n   inz(*off)
015100160616     d Fine            s               n   inz(*off)
015200160616     d FineVideo       s               n   inz(*off)
015300141014     d RiemetteVideo   s               n   inz(*off)
015400141017     d wDatiFLO        s               n   inz(*off)
015500170414     d wOkF06F04       s               n   inz(*off)
015600170414     d wOkF06F13       s               n   inz(*off)
015700141022     d wOkF15          s               n   inz(*off)
015800140415
015900140415       // -?Campi di comodo data
016000140415     d dataISO         s               d   datfmt(*ISO)
016100140415     d dataEUR         s               d   datfmt(*EUR)
016200140414
016300140414       // -?Campi di comodo
016400160615     d conta           s              3s 0 inz
016500160615     d contartf        s              1s 0 inz
016600160615     d dataoggi        s              8s 0 inz
016700160615     d flgbac          s              1a   inz
016800160615     d keytabella      s             15a   inz
016900160615     d k_TBEcod        s                   inz like(TBEcod)
017000160615     d k_TBEke1        s                   inz like(TBEke1)
017100160615     d NrErrore        s              2s 0 inz
017200160615     d nrighe          s              1s 0 inz
017300160615     d nrmsgatt        s              3s 0 inz
017400160615     d nrmsgblc        s              3s 0 inz
017500160615     d nrmsgcgi        s              3s 0 inz
017600160615     d nrmsgmtc        s              3s 0 inz
017700160615     d pos             s              3s 0 inz
017800160615     d posda           s              3s 0 inz
017900160615     d posrtf1         s              3s 0 inz
018000160615     d posrtf2         s              3s 0 inz
018100160615     d posrtf3         s              3s 0 inz
018200160615     d rigatf1         s             61a   inz
018300160615     d rigatf2         s             61a   inz
018400160615     d rigatf3         s             61a   inz
018500160615     d savacrcgi       s                   inz like(fiora0I0.cgi)
018600160610     d savatt          s                   inz like(fiora0I0.vaoatt)
018700160610     d savblc          s                   inz like(fiora0I0.vaoblc)
018800160610     d savcgi          s                   inz like(fiora0I0.cgi)
018900160610     d savcomm         s                   inz like(fior000i.comm)
019000160610     d savffd          s                   inz like(fiora0I0.vaoffd)
019100160610     d savmail         s                   inz like(fiora0I0.mail)
019200160613     d savmailconf     s                   inz like(fiora0I0.mailconf)
019300160610     d savmtc          s                   inz like(fiora0I0.vaomtc)
019400160610     d savsms          s                   inz like(fiora0I0.sms)
019500160613     d savsmsconf      s                   inz like(fiora0I0.smsconf)
019600160610     d savsto          s                   inz like(fiora0I0.vaosto)
019700160615     d tastof          s             25a   inz
019800160615     d wAmbito         s              1a   inz
019900141009     d wCap            s                   inz like(fiora0I0.vaocar)
020000160615     d wCodice         s             10s 0 inz
020100160525     d wErrc           s                   inz like(fnlv13ds.O13err)
020200160525     d wErro           s                   inz like(fnlv13ds.O13err)
020300160525     d wErrr           s                   inz like(fnlv13ds.O13err)
020400160615     d wFiliale        s              3s 0 inz
020500160525     d wfzvc           s                   inz like(fnlv13ds.E13fzv)
020600160525     d wfz1c           s                   inz like(fnlv13ds.E13fz1)
020700160525     d wfz2c           s                   inz like(fnlv13ds.E13fz2)
020800160525     d wfz3c           s                   inz like(fnlv13ds.E13fz3)
020900160525     d wfzvo           s                   inz like(fnlv13ds.E13fzv)
021000160525     d wfz1o           s                   inz like(fnlv13ds.E13fz1)
021100160525     d wfz2o           s                   inz like(fnlv13ds.E13fz2)
021200160525     d wfz3o           s                   inz like(fnlv13ds.E13fz3)
021300160525     d wfzvr           s                   inz like(fnlv13ds.E13fzv)
021400160525     d wfz1r           s                   inz like(fnlv13ds.E13fz1)
021500160525     d wfz2r           s                   inz like(fnlv13ds.E13fz2)
021600160525     d wfz3r           s                   inz like(fnlv13ds.E13fz3)
021700160615     d wGiro           s             10a   inz
021800160615     d wIdAttr         s              5a   inz
021900160615     d wIdCampo        s             10a   inz
022000160615     d wIdMsg          s              7a   inz
022100141009     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
022200141009     d wLocalita       s                   inz like(fiora0I0.vaolor)
022300160525     d wMsgc           s                   inz like(fnlv13ds.O13msg)
022400160525     d wMsgo           s                   inz like(fnlv13ds.O13msg)
022500160525     d wMsgr           s                   inz like(fnlv13ds.O13msg)
022600141009     d wNazione        s                   inz like(fiora0I0.vaonar)
022700141020     d wNrForza        s              3s 0 inz
022800141020     d wNrMsg          s              3s 0 inz
022900141007     d wParm           s            512a   inz
023000141020     d wPor            s              3s 0 inz
023100141009     d wProvincia      s                   inz like(fiora0I0.vaoprr)
023200141021     d wRagSoc         s                   inz like(fiora0I0.vaorsr)
023300140613
023400140618       // -?Parametri per richiamo pgm
023500140613     d RpyIdMsg        s             10i 0
023600140613     d RpyOpCode       s             10i 0
023700140613     d RqsOpCode       s             10i 0
023800140414
023900140414       //--------------------------------------------------------------
024000140414       //?Definizione procedure.
024100140414       //--------------------------------------------------------------
024200160608       // -?Pgm descrizione giro
024300160620     d FIDG09R         pr                  extpgm('FIDG09R')
024400160608     d  kpjba                              likeds(kpjba)
024500160620
024600160620       // -?Recupero ora presunta ritiro
024700160620     d FIOR01R         pr                  extpgm('FIOR01R')
024800160620     d  fior01ds                           likeds(FIOR01DS)
024900160614
025000160614       // -?Pgm gestione note
025100160614     d FIOR06R         pr                  extpgm('FIOR06R')
025200160614     d  kpjba                              likeds(KPJBA)
025300160614     d  fior06ds                           likeds(FIOR06DS)
025400160608
025500141008       // -?Pgm interrogazione anagrafica clienti ritiro
025600141008     d FIOR81R         pr                  extpgm('FIOR81R')
025700141008     d  kpjba                              likeds(KPJBA)
025800141008     d  fior81ds                           likeds(FIOR81DS)
025900141021
026000141021       // -?Pgm controllo indirizzo completo
026100141021     d FNLV13R         pr                  extpgm('FNLV13R')
026200141021     d  kpjba                              likeds(KPJBA)
026300141021     d  fnlv13ds                           likeds(FNLV13DS)
026400141021     d  tisi95ds                           likeds(TISI95DS)
026500141021     d  flgbac                        1a   options(*nopass)
026600141021     d  tisi97ds                           likeds(TISI97DS)
026700141021     d                                     options(*nopass)
026800141009
026900141009       // -?Pgm interrogazione cappario
027000141009     d FNLV14R         pr                  extpgm('FNLV14R')
027100141009     d  kpjba                              likeds(KPJBA)
027200141009     d  fnlv14ds                           likeds(FNLV14DS)
027300170505
027400170505       // -?Interrogazione Cappario DPD
027500170505     d TISIE8R         pr                  extpgm('TISIE8R')
027600170505     d  kpjba                              likeds(KPJBA)
027700170505     d  tisie8ds                           likeds(TISIE8DS)
027800160621
027900160621       // -?Simulazione Delivery
028000160621     d TISI92R         pr                  extpgm('TISI92R')
028100160621     d  kpjba                              likeds(KPJBA)
028200160321
028300160321       // -?Caricamento Filiali in gestione
028400160321     d TRUL31R         pr                  extpgm('TRUL31R')
028500160321     d  kpjba                              likeds(KPJBA)
028600160321     d  trul31ds                           likeds(TRUL31DS)
028700170403
028800170403       // -?Aggiunge/Toglie gg/mm dalla data
028900170403     d XGIOLAV         pr                  extpgm('XGIOLAV')
029000170403     d  xgiolavds                          likeds(xgiolavds)
029100140414
029200140414       //--------------------------------------------------------------
029300140414       //?Definizione prototipi.
029400140414       //--------------------------------------------------------------
029500140618      /copy gaitrasrc/srcprotopr,FIOR000R
029600140618      /copy gaitrasrc/srcprotopr,FIOR001R
029700140613      /copy gaitrasrc/srcprotopr,FIORA00R
029800160622      /copy gaitrasrc/srcprotopr,FIORA01R
029900141007      /copy gaitrasrc/srcprotopr,RTVMSGLANG
030000140416      /copy gaitrasrc/srcprotopi,TRULTAB
030100140416      /copy gaitrasrc/srcprotopr,TRULTAB
030200160608      /copy gaitrasrc/srcprotopr,XSRDA8
030300140414
030400140414       //--------------------------------------------------------------
030500140414       //?Definizione parametri programma.
030600140414       //--------------------------------------------------------------
030700140618     d Fior001_Controller...
030800140414     d                 PI
030900140618     d prmRqsOpCode...
031000140618     d                               10I 0 CONST
031100140618     d prmRpyOpCode...
031200140618     d                               10I 0
031300140618     d prmRpyIdMsg...
031400140618     d                               10I 0
031500140618     d prmRqsFormato...
031600140618     d                               10A   CONST
031700140618     d prmRqsData...
031800140618     d                            32767A   OPTIONS(*VARSIZE)
031900140618     d prmRqsSize...
032000140618     d                               10I 0 CONST
032100140618     d prmRpyFormato...
032200140618     d                               10A   CONST
032300140618     d prmRpyData...
032400140618     d                            32767A   OPTIONS(*VARSIZE)
032500140618     d prmRpySize...
032600140618     d                               10I 0 CONST
032700140618     d prmRpyFormMsg...
032800140618     d                               10A   CONST OPTIONS(*NOPASS)
032900140618     d prmRpyMessage...
033000140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033100140618     d prmRpySizeMsg...
033200140618     d                               10I 0 CONST OPTIONS(*NOPASS)
033300140618     d prmRpyFormForz...
033400140618     d                               10A   CONST OPTIONS(*NOPASS)
033500140618     d prmRpyForzatu...
033600140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033700140618     d prmRpySizeForz...
033800140618     d                               10I 0 CONST OPTIONS(*NOPASS)
033900140618     d prmRpyFormTastiFun...
034000140618     d                               10A   CONST OPTIONS(*NOPASS)
034100140618     d prmRpyTastiFun...
034200140618     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
034300140618     d prmRpySizeTastiFun...
034400140618     d                               10I 0 CONST OPTIONS(*NOPASS)
034500140618     d prmKPJBA...
034600140618     d                              502A   CONST OPTIONS(*NOPASS)
034700160622
034800160607      //---------------------------------------------------------------
034900160607      //?Definizione key-list.
035000160607      //---------------------------------------------------------------
035100160607       // - File TABEL00F
035200160607     d k03tabel      e ds                  extname(TABEL00F:*key)
035300160607     d                                     prefix(k_)
035400160607
035500140414       //--------------------------------------------------------------
035600140414       //?MAIN.
035700140414       //--------------------------------------------------------------
035800140414
035900140414      /free
036000160616
036100160616       //?Operazioni iniziali
036200160616       exsr RoutInz;
036300160616
036400160616       //?Imposto i dati nella ds di input
036500160616       exsr MoveDs00;
036600160616
036700160616       //?Gestione video
036800160616       DOW  not Fine;
036900160616
037000160616         SELECT;
037100160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
037200160616           exsr VideoPrincipale;
037300160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI;
037400160616           exsr VideoAltriDati;
037500160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCOMM;
037600160616           exsr VideoAlertComm;
037700160616         WHEN  fior000i.video = FIOR000_VIDEO_ALERTCONF;
037800160616           exsr VideoAlertConf;
037900160616         WHEN  fior000i.video = FIOR000_VIDEO_WHOISORD;
038000160616           exsr VideoWho;
038100160616         OTHER;
038200160616           Fine = *on;
038300160616         ENDSL;
038400160616
038500160616       ENDDO;
038600140618
038700140414       //?Operazioni finali
038800140414       exsr RoutEnd;
038900160616
039000160616       //--------------------------------------------------------------
039100160616       //?Operazioni iniziali.
039200160616       //--------------------------------------------------------------
039300160616       BEGSR  Routinz;
039400160616
039500160616         *inLR = *on;
039600160616
039700160616       //?Controllo formale dei dati passati
039800160616         exsr ControllaParm;
039900160616
040000160616       //?Imposto i dati utente
040100160616         exsr DatiUtente;
040200160616
040300160616       //?Cerco i dati di Default
040400160616         exsr CaricaDefault;
040500160616
040600160616       ENDSR;
040700160616
040800160616       //--------------------------------------------------------------
040900160616       //?Imposto i campi nella DS di input. (pgm FIOR000R)
041000160616       //--------------------------------------------------------------
041100160616       BEGSR  MoveDs00;
041200160616
041300160616         reset FIOR000I;
041400160616
041500160616         eval-corr fior000i = fior001i;
041600160616         reset fior000i.formato;
041700160616         reset fior000i.versione;
041800160616
041900160616         fior000i.titolo = FIOR001_TITOLO;
042000160616         fior000i.dpoei = GetDesFiliale(fior001i.poei);
042100160616
042200160616         SELECT;
042300160616       //?Imposto i campi per immissione manuale
042400160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
042500160616               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
042600160616           fior000i.mod = FIOR001_MOD_IMMISSIONE;
042700160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
042800160616           dataISO = %date(dataoggi:*iso);
042900160616           dataEUR = dataISO;
043000160616           fior000i.dao = %dec(dataEUR);
043100160616           fior000i.oao = %dec(%time());
043200160616
043300160616           fior000i.tor = ddft.d§DFTtor;
043400160616
043500160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL;
043600160616             fior000i.tco = FIORA00_TIPOCOMORM_TELEFONICO;
043700160616           ENDIF;
043800160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
043900160616             fior000i.tco = FIORA00_TIPOCOMORM_MAIL;
044000160616           ENDIF;
044100160616
044200160616           fior000i.pag = ddft.d§DFTpag;
044300160616           fior000i.ddt = ddft.d§DFTddt;
044400170331           fior000i.dpm = fior000i.dao;
044500170403           fior000i.orr = fior000i.oao / 100;
044600170530
044700170530       //?Imposto i campi per conferma da VAO
044800170530         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_INTERNET;
044900170530           fior000i.mod = FIOR001_MOD_IMMISSIONE;
045000170530           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
045100160616
045200170530       //?Imposto i campi per conferma ORM Fissi
045300170530         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
045400160616           fior000i.mod = FIOR001_MOD_IMMISSIONE;
045500160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
045600170530
045700170530           dataISO = %date(dataoggi:*iso);
045800170530           dataEUR = dataISO;
045900170530           fior000i.dao = %dec(dataEUR);
046000170530           fior000i.oao = %dec(%time());
046100170530           fior000i.tco = FIORA00_TIPOCOMORM_FISSO;
046200170530           fior000i.ddt = ddft.d§DFTddt;
046300170530           IF  fior000i.pag = *blanks;
046400170530             fior000i.pag = ddft.d§DFTpag;
046500170530           ENDIF;
046600160616
046700160616       //?Imposto i campi per Manutenzione
046800160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
046900160616           fior000i.mod = FIOR001_MOD_MANUTENZIONE;
047000160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
047100160616
047200160616         //?Aggancio ORM
047300160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv)
047400160616                  FNORM01L;
047500160616           IF  %found(FNORM01L);
047600160616             fior000i.fao = ORMfao;
047700160616           ENDIF;
047800160616
047900160616         ENDSL;
048000170420
048100170420       //?Resetto tutti i flag booleani
048200170420         reset EmessoAlertComm;
048300170420         reset EmessoAlertConf;
048400170420         reset Fine;
048500170420         reset FineVideo;
048600170420         reset RiemetteVideo;
048700170420         reset wDatiFLO;
048800170420         reset wOkF06F04;
048900170420         reset wOkF06F13;
049000170420         reset wOkF15;
049100160616
049200160616       ENDSR;
049300160616
049400160616       //--------------------------------------------------------------
049500160616       //?Gestione Videata Principale.
049600160616       //--------------------------------------------------------------
049700160616       BEGSR  VideoPrincipale;
049800160616
049900160616         FineVideo = *off;
050000160616
050100160616         DOW  not FineVideo;
050200160616
050300160616           RiemetteVideo = *off;
050400160616
050500160616         //?Imposto i tasti funzione e gli attributi del video
050600160620         //?Se F24 non lo faccio
050700160620           IF  fior000a.TastoVideo <> FIOR000_TASTO_F24;
050800170331             exsr ImpostaVideo;
050900160620           ENDIF;
051000160616
051100160616           RqsOpCode = prmRqsOpCode;
051200160616
051300160616         //?Richiamo pgm gestione video
051400160616           exsr CallVideo;
051500160616
051600160616         //?Se dato ENTER prima di verificare il tasto dato dall'utente
051700160616         //?faccio dei controlli preventivi sui dati immessi a video
051800170418           IF  fior000a.TastoVideo = FIOR000_TASTO_ENTER or
051900170418               fior000a.TastoVideo = FIOR000_TASTO_F06;
052000160616             exsr PrimiControlli;
052100160616           //?Se devo riemettere il video torno su
052200160616             IF  RiemetteVideo;
052300160616               eval-corr fior000i = fior000o;
052400160616               reset fior000i.formato;
052500160616               reset fior000i.versione;
052600160616               iter;
052700160616             ENDIF;
052800160616           ENDIF;
052900160616
053000160616         //?Salvo il flag di ORM commissionato del video
053100160616           savcomm = fior000i.comm;
053200160616
053300160616         //?Eseguo i comandi dati dall'utente
053400160616           exsr Comandi;
053500160616
053600160616         ENDDO;
053700160616
053800160616       ENDSR;
053900160616
054000160616       //--------------------------------------------------------------
054100160616       //?Gestione Window del F05 Altri Dati.
054200160616       //--------------------------------------------------------------
054300160616       BEGSR  VideoAltriDati;
054400160616
054500160616         FineVideo = *off;
054600160616
054700160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
054800160616       //?emettere subito il messaggio di errore
054900160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
055000160616           exsr ControllaDatiVideo;
055100160616         ELSE;
055200160616           reset fiora0M0;
055300160616         ENDIF;
055400160616
055500160616       //?Mi salvo i dati della videata 02
055600160616         savblc = fior000i.blc;
055700160616         savatt = fior000i.att;
055800160616         savmtc = fior000i.mtc;
055900160616         savsto = fior000i.sto;
056000160616         savcgi = fior000i.cgi;
056100160616         savffd = fior000i.ffd;
056200160616
056300160616         DOW  not FineVideo;
056400160616
056500160616         //?Imposto i tasti funzione e gli attributi del video
056600160616           exsr ImpostaVideo;
056700160616
056800160616         //?Emetto la videata
056900160616           exsr CallVideo;
057000160616
057100160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
057200160616         //?quindi devo eseguire i comandi dati dall'utente
057300160616           exsr Comandi;
057400160616
057500160616         ENDDO;
057600160616
057700160616       ENDSR;
057800160616
057900160616       //--------------------------------------------------------------
058000160616       //?Gestione Window del F04 Alert Commissionato.
058100160616       //--------------------------------------------------------------
058200160616       BEGSR  VideoAlertComm;
058300160616
058400160616         FineVideo = *off;
058500160616         EmessoAlertComm = *on;
058600160616
058700160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
058800160616       //?emettere subito il messaggio di errore
058900160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
059000160616           exsr ControllaDatiVideo;
059100160616         ELSE;
059200160616           reset fiora0M0;
059300160616         ENDIF;
059400160616
059500160616       //?Mi salvo i dati della videata 03
059600160616         savmail = fior000i.mail;
059700160616         savsms  = fior000i.sms;
059800160616
059900160616         DOW  not FineVideo;
060000160616
060100160616         //?Imposto i tasti funzione e gli attributi del video
060200160616           exsr ImpostaVideo;
060300160616
060400160616         //?Emetto la videata
060500160616           exsr CallVideo;
060600160616
060700160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
060800160616         //?quindi devo eseguire i comandi dati dall'utente
060900160616           exsr Comandi;
061000160616
061100160616         ENDDO;
061200160616
061300160616       ENDSR;
061400160616
061500160616       //--------------------------------------------------------------
061600160616       //?Gestione Window del F13 Alert Conferma Prenotazione.
061700160616       //--------------------------------------------------------------
061800160616       BEGSR  VideoAlertConf;
061900160616
062000160616         FineVideo = *off;
062100160616         EmessoAlertConf = *on;
062200160616
062300160616       //?Se ho errore di messaggio in sospeso prima richiamo i controlli per
062400160616       //?emettere subito il messaggio di errore
062500160616         IF  %lookup('TIS0847':fiora0M0.skIdMsg) > 0;
062600160616           exsr ControllaDatiVideo;
062700160616         ELSE;
062800160616           reset fiora0M0;
062900160616         ENDIF;
063000160616
063100160616       //?Mi salvo i dati della videata 04
063200160616         savmailconf = fior000i.mailconf;
063300160616         savsmsconf  = fior000i.smsconf;
063400160616
063500160616         DOW  not FineVideo;
063600160616
063700160616         //?Imposto i tasti funzione e gli attributi del video
063800160616           exsr ImpostaVideo;
063900160616
064000160616         //?Emetto la videata
064100160616           exsr CallVideo;
064200160616
064300160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
064400160616         //?quindi devo eseguire i comandi dati dall'utente
064500160616           exsr Comandi;
064600160616
064700160616         ENDDO;
064800160616
064900160616       ENDSR;
065000160616
065100160616       //--------------------------------------------------------------
065200160616       //?Gestione Window Tipo Ordinante ORM Commissionato.
065300160616       //--------------------------------------------------------------
065400160616       BEGSR  VideoWho;
065500160616
065600160616         FineVideo = *off;
065700160616
065800160616         DOW  not FineVideo;
065900160616
066000160616         //?Emetto la videata
066100160616           exsr CallVideo;
066200160616
066300160616         //?Se arrivo qua vuol dire che la chiamata al pgm del video è andata bene
066400160616         //?quindi devo eseguire i comandi dati dall'utente
066500160616           exsr Comandi;
066600160616
066700160616         ENDDO;
066800160616
066900160616       ENDSR;
067000160616
067100160616       //--------------------------------------------------------------
067200160616       //?Operazioni finali.
067300160616       //--------------------------------------------------------------
067400160616       BEGSR  RoutEnd;
067500160616
067600170530         %subst(prmRpyData:1:prmRpySize) = fior001o;
067700160616
067800160616         return;
067900160616
068000160616       ENDSR;
068100140414
068200140414       //--------------------------------------------------------------
068300140414       //?Controllo formale dei dati passati.
068400140414       //--------------------------------------------------------------
068500140414       BEGSR  ControllaParm;
068600140414
068700140618       //?OpCode
068800160321         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_TEL and
068900160321             prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_MAIL and
069000170530             prmRqsOpCode <> FIOR001_RQSOPCODE_CONFERMA_FISSI and
069100141010             prmRqsOpCode <> FIOR001_RQSOPCODE_CONFERMA_INTERNET and
069200141010             prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE;
069300140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
069400140618           prmRpyIdMsg  = FIOR001_ESITO_RQSOPCODE_SCONOSCIUTO;
069500140618           exsr RoutEnd;
069600140618         ENDIF;
069700140618
069800140618       //?Formato e size RQS
069900140618         IF  prmRqsSize > 0;
070000140618         ELSE;
070100140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
070200140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
070300140618           exsr RoutEnd;
070400140618         ENDIF;
070500140618         IF  prmRqsFormato = fior001i.formato;
070600140618           fior001i = %SUBST(prmRqsData:1:prmRqsSize);
070700140618         ELSE;
070800140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
070900140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
071000140618           exsr RoutEnd;
071100140618         ENDIF;
071200140618
071300140618       //?Formato e size RPY
071400140618         IF  prmRpySize > 0;
071500140618         ELSE;
071600140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
071700140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
071800140618           exsr RoutEnd;
071900140618         ENDIF;
072000170601         IF  prmRpyFormato = fior001o.formato;
072100140618         ELSE;
072200140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
072300140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
072400140618           exsr RoutEnd;
072500140618         ENDIF;
072600140618
072700140618       //?Formato e size MSG
072800140618         IF  prmRpySizeMsg > 0;
072900140618         ELSE;
073000140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
073100140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
073200140618           exsr RoutEnd;
073300140618         ENDIF;
073400140618         IF  prmRpyFormMsg = fiora0m0.formato;
073500160531           fiora0m0 = %SUBST(prmRpyMessage:1:prmRpySizeMsg);
073600140618         ELSE;
073700140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
073800140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
073900140618           exsr RoutEnd;
074000140618         ENDIF;
074100140618
074200140618       //?Formato e size Forzature
074300140618         IF  prmRpySizeForz > 0;
074400140618         ELSE;
074500140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
074600140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
074700140618           exsr RoutEnd;
074800140618         ENDIF;
074900140618         IF  prmRpyFormForz = fiora0f0.formato;
075000160531           fiora0f0 = %SUBST(prmRpyForzatu:1:prmRpySizeForz);
075100140618         ELSE;
075200140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
075300140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
075400140618           exsr RoutEnd;
075500140618         ENDIF;
075600140618
075700140618       //?Formato e size Tasti Funzione
075800140618         IF  prmRpySizeTastiFun > 0;
075900140618         ELSE;
076000140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
076100140618           prmRpyIdMsg = FIOR001_ESITO_SIZE_DATA_ERRATO;
076200140618           exsr RoutEnd;
076300140618         ENDIF;
076400140618         IF  prmRpyFormTastiFun = fior000a.formato;
076500140618         ELSE;
076600140618           prmRpyOpCode = FIOR001_RPYOPCODE_ERROR;
076700140618           prmRpyIdMsg = FIOR001_ESITO_NOME_FORMATO_SCONOSCIUTO;
076800140618           exsr RoutEnd;
076900140618         ENDIF;
077000140414
077100140414       ENDSR;
077200160321
077300160321       //--------------------------------------------------------------
077400160321       //?Imposto i dati Utente.
077500160321       //--------------------------------------------------------------
077600160321       BEGSR  DatiUtente;
077700160321
077800160321         clear TRUL31DS;
077900160321         clear kpjbu;
078000160321         trul31ds.I31abi = fior001i.abiute;
078100160321         trul31ds.I31cpo = fior001i.filute;
078200160321         TRUL31R (kpjba:trul31ds);
078300160321         IF  trul31ds.O31pog <= *zeros;
078400160321           leavesr;
078500160321         ENDIF;
078600160321
078700160321       ENDSR;
078800140618
078900140618       //--------------------------------------------------------------
079000140619       //?Carico i dati di Default.
079100140618       //--------------------------------------------------------------
079200140619       BEGSR  CaricaDefault;
079300140619
079400140619         prmRpyOpCode = FIOR001_RPYOPCODE_DONE;
079500140619         prmRpyIdMsg  = FIOR001_ESITO_OK;
079600140619
079700140619       //?Data del giorno
079800140619         dataoggi = %dec(%date());
079900140618
080000140618       //?Reperimento tabella DFT
080100160321         keytabella = %editc(fior001i.poei:'X');
080200140618         FOR conta = 1 to 2;
080300140618         //?prima provo con la filiale emissione
080400140618           clear dDFT;
080500140618           IF  getTabella (c_tntbe:'DFT':keytabella:
080600140618                           *blanks:*blanks:*blanks:
080700140618                           *omit:*omit:*omit:*omit:*omit:
080800140618                           *omit:*omit:*omit:*omit:*omit:
080900140618                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
081000140618               = *zeros and ds_TNTBE.TBEatb = *blanks;
081100140618             dDFT = ds_TNTBE.TBEuni;
081200140618             leave;
081300140618           ENDIF;
081400140618         //?se non trovo con filiale emissione
081500140618         //?provo con '999'
081600140618           clear dDFT;
081700140618           keytabella = '999';
081800140618         ENDFOR;
081900160607
082000160607       //?Impostazione campi "fissi"
082100160607         k_TBLkut = 1;
082200140618
082300140618       ENDSR;
082400160616
082500160616       //--------------------------------------------------------------
082600160616       //?Imposto i tasti funzione e gli attributi dei campi.
082700160616       //--------------------------------------------------------------
082800160616       BEGSR  ImpostaVideo;
082900160616
083000160616         reset FIOR000A;
083100160616
083200160616       //?Metto la filiale ritiro in un campo numerico
083300160616         clear wPor;
083400160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
083500160616           wPor = %int(fior000i.por);
083600160616         ENDIF;
083700160616
083800160616         exsr TastiFunzione;
083900160616
084000160616         SELECT;
084100160616
084200160616       //?Attributi validi solo per immissione manuale
084300160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
084400170530               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
084500170530               prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
084600160616         //?Filiale Emissione
084700160616           wIdCampo = 'ATTRDPOEI';
084800160616           wIdAttr = FIOR001_ATTR_BLUE;
084900160616           IF  fior001i.poei = fior001i.poe;
085000160616             wIdAttr = FIOR001_ATTR_ND;
085100160616           ENDIF;
085200160616           exsr Attributi;
085300160616           wIdCampo = 'ATTRPOEI';
085400160616           wIdAttr = FIOR001_ATTR_PINK;
085500160616           IF  fior001i.poei = fior001i.poe;
085600160616             wIdAttr = FIOR001_ATTR_ND;
085700160616           ENDIF;
085800160616           exsr Attributi;
085900160616         //?Tipo Comunicazione ORM
086000160616           wIdCampo = 'ATTRTCO';
086100160616           wIdAttr = FIOR001_ATTR_HIUL;
086200160616           exsr Attributi;
086300160616         //?Tipo ORM
086400160616           wIdCampo = 'ATTRTOR';
086500160616           wIdAttr = FIOR001_ATTR_HIUL;
086600160616           exsr Attributi;
086700160616         //?Mittente
086800160616           wIdCampo = 'ATTRCRA';
086900160616           wIdAttr = FIOR001_ATTR_HIUL;
087000160616           exsr Attributi;
087100160616           //?Se codice mittente inserito devo bloccare i dati del mittente
087200160616           IF  fior000i.cra2 > 0;
087300160616             wIdCampo = 'ATTRCRA1';
087400160616             wIdAttr = FIOR001_ATTR_PR;
087500160616             exsr Attributi;
087600160616           ENDIF;
087700160616           //?Se codice mittente non inserito devo sbloccare i dati del mittente
087800160616           IF  fior000i.cra2 = 0;
087900160616             wIdCampo = 'ATTRCRA1';
088000160616             wIdAttr = FIOR001_ATTR_HIUL;
088100160616             exsr Attributi;
088200160616           ENDIF;
088300160616         //?Altri dati Mittente
088400160616           wIdCampo = 'ATTRCRA2';
088500160616           wIdAttr = FIOR001_ATTR_HIUL;
088600160616           exsr Attributi;
088700160616         //?Quantità
088800160616           wIdCampo = 'ATTRQTA';
088900160616           wIdAttr = FIOR001_ATTR_HIUL;
089000160616           exsr Attributi;
089100160616         //?Natura merce
089200160616           wIdCampo = 'ATTRNAM';
089300160616           wIdAttr = FIOR001_ATTR_HIUL;
089400160616           exsr Attributi;
089500160616         //?Riferimento Alfa
089600160616           wIdCampo = 'ATTRRFA';
089700160616           wIdAttr = FIOR001_ATTR_HIUL;
089800160616           exsr Attributi;
089900160616         //?Ordinante
090000160616           wIdCampo = 'ATTRCOR';
090100160616           wIdAttr = FIOR001_ATTR_HIUL;
090200160616           exsr Attributi;
090300160616           //?Se codice ordinante inserito devo bloccare i dati dell'ordinante
090400160616           IF  fior000i.cor2 > 0;
090500160616             wIdCampo = 'ATTRCOR1';
090600160616             wIdAttr = FIOR001_ATTR_PR;
090700160616             exsr Attributi;
090800160616           ENDIF;
090900160616           //?Se codice ordinante non inserito devo sbloccare i dati dell'ordinante
091000160616           IF  fior000i.cor2 = 0;
091100160616             wIdCampo = 'ATTRCOR1';
091200160616             wIdAttr = FIOR001_ATTR_HIUL;
091300160616             exsr Attributi;
091400160616           ENDIF;
091500160616         //?Destinatario
091600160616           wIdCampo = 'ATTRCRC';
091700160616           wIdAttr = FIOR001_ATTR_HIUL;
091800160616           exsr Attributi;
091900160616           wIdCampo = 'ATTRCRC1';
092000160616           wIdAttr = FIOR001_ATTR_HIUL;
092100160616           exsr Attributi;
092200160616           SELECT;
092300160616           //?Blocco del destinatario blocco TUTTO il destinatario
092400160616           WHEN  dorm01.§ORMfd = 'S' and (fior000i.crc2 > 0 or
092500160616                 fior000i.rsc > *blanks);
092600160616             wIdCampo = 'ATTRCRC';
092700160616             wIdAttr = FIOR001_ATTR_PR;
092800160616             exsr Attributi;
092900160616             wIdCampo = 'ATTRCRC1';
093000160616             wIdAttr = FIOR001_ATTR_PR;
093100160616             exsr Attributi;
093200160616           //?Se codice destinatario inserito devo bloccare i dati del destinatario
093300160616           WHEN  fior000i.crc2 > 0;
093400160616             wIdCampo = 'ATTRCRC1';
093500160616             wIdAttr = FIOR001_ATTR_PR;
093600160616             exsr Attributi;
093700160616           //?Se codice destinatario non inserito devo sbloccare i dati del destinatario
093800160616           WHEN  fior000i.crc2 = 0;
093900160616             wIdCampo = 'ATTRCRC1';
094000160616             wIdAttr = FIOR001_ATTR_HIUL;
094100160616             exsr Attributi;
094200160616           ENDSL;
094300160616         //?Filiale Ritiro
094400160616           wIdCampo = 'ATTRPOR';
094500160616           wIdAttr = FIOR001_ATTR_HIUL;
094600160616           exsr Attributi;
094700160616         //?Filiale Consegna
094800160616           wIdCampo = 'ATTRPOC';
094900160616           wIdAttr = FIOR001_ATTR_HIUL;
095000160616           exsr Attributi;
095100160616         //?Nota1 e Nota2
095200160616           wIdCampo = 'ATTRNOTA';
095300160616           wIdAttr = FIOR001_ATTR_HIUL;
095400160616           exsr Attributi;
095500160616         //?Chi Paga
095600160616           wIdCampo = 'ATTRPAG';
095700160616           wIdAttr = FIOR001_ATTR_HIUL;
095800160616           exsr Attributi;
095900160616         //?Commissionato
096000160616           wIdCampo = 'ATTRCOMM';
096100160616           SELECT;
096200160616         //?Proteggo se ORM con ricevuta di ritiro
096300160616           WHEN  dorm01.§ORsrm = 'S';
096400160616             wIdAttr = FIOR001_ATTR_PRHI;
096500160616         //?Proteggo se ORM con preavviso mail
096600160616           WHEN  dorm01.§ORMpre = 'S';
096700160616             wIdAttr = FIOR001_ATTR_PRHI;
096800160616           OTHER;
096900160616             wIdAttr = FIOR001_ATTR_HIUL;
097000160616           ENDSL;
097100160616           exsr Attributi;
097200160616         //?Numero ORM
097300160616           wIdCampo = 'ATTRNOR';
097400160616           wIdAttr = FIOR001_ATTR_ND;
097500160616           exsr Attributi;
097600160616           wIdCampo = 'ATTRFASE';
097700160616           wIdAttr = FIOR001_ATTR_ND;
097800160616           exsr Attributi;
097900160616           wIdCampo = 'ATTRCAU';
098000160616           wIdAttr = FIOR001_ATTR_ND;
098100160616           exsr Attributi;
098200170330         //?Non visualizzo la data ritiro se non presente
098300170330           IF  fior000i.dar = *zeros;
098400170330             wIdCampo = 'ATTRDAR';
098500170330             wIdAttr = FIOR001_ATTR_ND;
098600170330             exsr Attributi;
098700170330           ENDIF;
098800170403         //?Se non ho ancora calcolato la data ritiro non faccio
098900170403         //?vedere gli orari servizio
099000170403           IF  fior000i.dar = *zeros;
099100170403             wIdCampo = 'ATTRORSR';
099200170403             wIdAttr = FIOR001_ATTR_ND;
099300170403             exsr Attributi;
099400170403           ENDIF;
099500170420         //?Ultimo ORM immesso
099600170420           IF  fior000i.poeu = *zeros and fior000i.noru = *zeros;
099700170420             wIdCampo = 'ATTRNORU';
099800170420             wIdAttr = FIOR001_ATTR_ND;
099900170420             exsr Attributi;
100000170420           ENDIF;
100100160616         //?Campi videata TastoF05
100200160616           wIdCampo = 'ATTRBLC';
100300160616           wIdAttr = FIOR001_ATTR_HIUL;
100400160616           exsr Attributi;
100500160616           wIdCampo = 'ATTRATT';
100600160616           wIdAttr = FIOR001_ATTR_HIUL;
100700160616           exsr Attributi;
100800160616           wIdCampo = 'ATTRMTC';
100900160616           wIdAttr = FIOR001_ATTR_HIUL;
101000160616           exsr Attributi;
101100160616           wIdCampo = 'ATTRSTO';
101200160616           wIdAttr = FIOR001_ATTR_HIUL;
101300160616           exsr Attributi;
101400160616           wIdCampo = 'ATTRCGI';
101500160616           wIdAttr = FIOR001_ATTR_HIUL;
101600160616           exsr Attributi;
101700160616           wIdCampo = 'ATTRFFD';
101800160616           wIdAttr = FIOR001_ATTR_HIUL;
101900160616           exsr Attributi;
102000160616         //?Campi videata TastoF04
102100160616           wIdCampo = 'ATTRSMS';
102200160616           wIdAttr = FIOR001_ATTR_HIUL;
102300160616           exsr Attributi;
102400160616           wIdCampo = 'ATTRMAIL';
102500160616           wIdAttr = FIOR001_ATTR_HIUL;
102600160616           exsr Attributi;
102700160616         //?Campi videata TastoF13
102800160616           wIdCampo = 'ATTRSMSC';
102900160616           wIdAttr = FIOR001_ATTR_HIUL;
103000160616           exsr Attributi;
103100160616           wIdCampo = 'ATTRMAILC';
103200160616           wIdAttr = FIOR001_ATTR_HIUL;
103300160616           exsr Attributi;
103400160616
103500160616         //?Se tipo ORM multiplo
103600160616         //?e ho i dati del destinatario
103700160616         //?imposto in automatico che ORM singolo
103800160616         //?ma NON se c'è errore sul destinatario o sul tipo ORM
103900160616         IF  fior000i.tor = FIORA00_TIPOORM_MULTIPLO and
104000160616             ((fior000i.crc1 > 0 and fior000i.crc2 > 0) or
104100160616              (fior000i.rsc <> *blanks)) and
104200160616               %lookup('VAOCRC':fiora0M0.skIdCampo) = 0 and
104300160616               %lookup('VAOTOR':fiora0M0.skIdCampo) = 0;
104400160616           fior000i.tor = FIORA00_TIPOORM_SINGOLO;
104500160616         ENDIF;
104600170530
104700170530         //?Attributi validi solo per conferma fissi
104800170530           IF  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
104900170530             wIdCampo = 'ATTRTCO';
105000170530             wIdAttr = FIOR001_ATTR_PR;
105100170530             exsr Attributi;
105200170530           ENDIF;
105300160616
105400160616       //?Attributi validi solo conferma da Internet
105500160616         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_INTERNET;
105600160616
105700160616         //?Non visualizzo il numero ORM
105800160616           wIdCampo = 'ATTRNOR';
105900160616           wIdAttr = FIOR001_ATTR_ND;
106000160616           exsr Attributi;
106100160616
106200160616         //?Non visualizzo la fase
106300160616           wIdCampo = 'ATTRFASE';
106400160616           wIdAttr = FIOR001_ATTR_ND;
106500160616           exsr Attributi;
106600160616
106700160616         //?Non visualizzo la causale
106800160616           wIdCampo = 'ATTRCAU';
106900160616           wIdAttr = FIOR001_ATTR_ND;
107000160616           exsr Attributi;
107100160616
107200160616         //?Se non ho ancora calcolato la data ritiro non faccio
107300160616         //?vedere gli orari servizio
107400160616           IF  fior000i.dar = *zeros;
107500160616             wIdCampo = 'ATTRORSR';
107600160616             wIdAttr = FIOR001_ATTR_ND;
107700160616             exsr Attributi;
107800160616           ENDIF;
107900160616
108000160616         //?Se ho il numero dell'ultimo ORM immesso lo visualizzo
108100160616           IF  fior000i.noru > 0;
108200160616             wIdCampo = 'ATTRNORU';
108300160616             wIdAttr = FIOR001_ATTR_ND;
108400160616             exsr Attributi;
108500160616           ENDIF;
108600160616
108700160616         ENDSL;
108800160616
108900160616       //?Attributi validi sui tasti funzione
109000160616       //?se ho dei dati relativi alla videata del F5
109100160616         IF  fior000i.blc > 0 or fior000i.att > 0 or
109200160616             fior000i.mtc > 0 or fior000i.sto > 0 or
109300160616             fior000i.cgi <> *blanks or
109400160616            (fior000i.ffd <> *blanks and fior000i.ffd <> 'N');
109500160616           wIdCampo = 'ATTRF05';
109600160616           wIdAttr = FIOR001_ATTR_HI;
109700160616           exsr Attributi;
109800160616         ENDIF;
109900160616       //?se ho dei dati relativi alla videata del F04
110000160616         IF  fior000i.mail <> *blanks or
110100160616             fior000i.sms <> *blanks;
110200160616           wIdCampo = 'ATTRF04';
110300160616           wIdAttr = FIOR001_ATTR_HI;
110400160616           exsr Attributi;
110500160616         ENDIF;
110600160616       //?Se ho dei dati relativi alle note AUT F17
110700160616         IF  fior000i.nor > 0;
110800160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv) FNORT01L;
110900160616           IF  %found(FNORT01L);
111000160616             wIdCampo = 'ATTRF17';
111100160616             wIdAttr = FIOR001_ATTR_HI;
111200160616             exsr Attributi;
111300160616           ENDIF;
111400160616         ENDIF;
111500160616       //?Se ho dei dati relativi alle note ORM F18
111600160616         IF  fior000i.nor > 0;
111700160616           chain (fior000i.poe:fior000i.nsr:fior000i.nor:fior000i.nrv) FNORN01L;
111800160616           IF  %found(FNORN01L);
111900160616             wIdCampo = 'ATTRF18';
112000160616             wIdAttr = FIOR001_ATTR_HI;
112100160616             exsr Attributi;
112200160616           ENDIF;
112300160616         ENDIF;
112400160616
112500160616       //?Decodifico i dati del video
112600160616         exsr Decodifica;
112700160616
112800160616       ENDSR;
112900160616
113000160616       //--------------------------------------------------------------
113100160616       //?Richiamo pgm gestione video.
113200160616       //--------------------------------------------------------------
113300160616       BEGSR  CallVideo;
113400160616
113500160616       //?Richiamo pgm gestione video
113600160616         MONITOR;
113700160616           Fior000_Video( RqsOpCode
113800160616                        : RpyOpCode
113900160616                        : RpyIdMsg
114000160616                        : fior000i.formato
114100160616                        : fior000i
114200160616                        : %SIZE(fior000i)
114300160616                        : fior000o.formato
114400160616                        : fior000o
114500160616                        : %SIZE(fior000o)
114600160616                        : FIORA0M0.formato
114700160616                        : FIORA0M0
114800160616                        : %size(FIORA0M0)
114900160616                        : FIORA0F0.formato
115000160616                        : FIORA0F0
115100160616                        : %size(FIORA0F0)
115200160616                        : FIOR000A.formato
115300160616                        : FIOR000A
115400160616                        : %size(FIOR000A)
115500160616                        );
115600160616           ON-ERROR;
115700160616           RpyOpCode = FIOR000_RPYOPCODE_ERROR;
115800160616         ENDMON;
115900160616
116000160616       //?Se non è DONE esco
116100160616         IF RpyOpCode < FIOR000_RPYOPCODE_DONE;
116200160616           exsr RoutEnd;
116300160616         ENDIF;
116400160616
116500160616       ENDSR;
116600160616
116700160616       //--------------------------------------------------------------
116800160616       //?Comandi dati sul video.
116900160616       //--------------------------------------------------------------
117000160616       BEGSR  Comandi;
117100160616
117200160616       //?imposto i dati dalla DS di Output del FIOR000R
117300160616       //?alla ds di input del FIOR000R
117400160616         eval-corr fior000i = fior000o;
117500160616         reset fior000i.formato;
117600160616         reset fior000i.versione;
117700160616
117800160616         SELECT;
117900160616
118000160616       //?ENTER = Controlli
118100160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER;
118200160616           exsr TastoEnter;
118300160616
118400160616       //?F04 = Alert Ritiro Merce ORM Commissionato
118500160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F04;
118600160616           exsr TastoF04;
118700160616
118800160616       //?F05 = Altri dati
118900160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F05;
119000160616           exsr TastoF05;
119100160616
119200160616       //?F06 = Conferma
119300160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F06;
119400160616           exsr TastoF06;
119500160616
119600160616       //?F07 = Mittente
119700160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F07;
119800160616           exsr TastoF07;
119900160616
120000160616       //?F08 = Ordinante
120100160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F08;
120200160616           exsr TastoF08;
120300160616
120400160616       //?F09 = Destinatario
120500160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F09;
120600160616           exsr TastoF09;
120700160616
120800160616       //?F10 = Copia Numero Telefono
120900160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F10;
121000160616           exsr TastoF10;
121100160616
121200160616       //?F11 = Volume
121300160616       //?viene gestito nel pgm di gestione video fior000r
121400160616       //?perchè il pgm richiamato da F11 emette una WIN
121500160616       //?che se emessa da questo pgm non mantiene attiva
121600160616       //?la videata dell'ORM
121700160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F11;
121800160616
121900160616       //?F12 = Ritorno al chiamante
122000160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F12;
122100160616           exsr TastoF12;
122200160616
122300160616       //?F13 = Alert Conferma Prenotazione Ritiro
122400160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F13;
122500160616           exsr TastoF13;
122600160620
122700160620       //?F14 = Visualizza Orari Servizio
122800160621       //?viene gestito nel pgm di gestione video fior000r
122900160621       //?perchè il pgm richiamato da F11 emette una WIN
123000160621       //?che se emessa da questo pgm non mantiene attiva
123100160621       //?la videata dell'ORM
123200160621       //?qua gestisco solo gli errori
123300160620         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F14;
123400160620           exsr TastoF14;
123500170505
123600170505       //?F15 = Interrogazione Cappario DPD
123700170505       //?attivato da un messaggio di errore
123800170505         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F15;
123900170505           exsr TastoF15;
124000160616
124100160616       //?F18 = Note
124200160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F18;
124300160616           exsr TastoF18;
124400160621
124500160621       //?F20 = Simulazione Delivery
124600160621         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F20;
124700160621           exsr TastoF20;
124800160616
124900160616       //?F24 = Altri tasti
125000160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_F24;
125100160616           exsr TastoF24;
125200170330
125300170330       //?RollUp = Tolgo 1 GG alla data Ritiro
125400170330         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ROLL_UP;
125500170330           exsr TastoRollUp;
125600170330
125700170330       //?RollDown = Aggiungo 1 GG alla data Ritiro
125800170330         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ROLL_DOWN;
125900170330           exsr TastoRollDown;
126000160616
126100160616         ENDSL;
126200160616
126300160616       ENDSR;
126400140618
126500140618       //--------------------------------------------------------------
126600140618       //?Imposto i tasti funzione.
126700140618       //--------------------------------------------------------------
126800141008       BEGSR  TastiFunzione;
126900140618
127000160616         fior000a.tastifun = *all'0';
127100160616
127200160615         SELECT;
127300160615
127400160615       //?Tasti funzione su videata principale
127500160616         WHEN fior000i.video = FIOR000_VIDEO_PRINCIPALE;
127600160615           clear rigaTF1;
127700160615           clear rigaTF2;
127800160615           clear rigaTF3;
127900160615           clear posrtf1;
128000160615           clear posrtf2;
128100160615           clear posrtf3;
128200160615           clear pos;
128300160615           clear posda;
128400160616         //?Tasti presenti nella riga Fissa
128500160615         //?F4 Alert
128600160616         //?solo in immissione
128700160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
128800170530               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
128900170530               prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
129000160616             %subst(fior000a.tastifun:4:1) = '1';
129100160616           ENDIF;
129200160615         //?F05 Altri dati
129300160615           %subst(fior000a.tastifun:5:1) = '1';
129400160615         //?F07 Int.Mittente
129500160615           %subst(fior000a.tastifun:7:1) = '1';
129600160615         //?F08 Int.Ordinante
129700160615           %subst(fior000a.tastifun:8:1) = '1';
129800160615         //?F09 Int.Destinatario
129900160615           %subst(fior000a.tastifun:9:1) = '1';
130000160615         //?F12 Ritorno
130100160615           %subst(fior000a.tastifun:12:1) = '1';
130200160616         //?F17 Note Aut
130300160616         //?solo in manutenzione
130400160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
130500160616             %subst(fior000a.tastifun:17:1) = '1';
130600160616           ENDIF;
130700160615         //?F18 Note
130800160615           %subst(fior000a.tastifun:18:1) = '1';
130900160616         //?Tasti presenti nella riga Mobile
131000160616         //?F02 Dati DPD
131100160616         //?solo in manutenzione
131200160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
131300160616             %subst(fior000a.tastifun:2:1) = '1';
131400160616             tastof = FIOR001_F02;
131500160616             exsr RieTastiFun;
131600160616           ENDIF;
131700160615         //?F11 Volume
131800160615           %subst(fior000a.tastifun:11:1) = '1';
131900160615           tastof = FIOR001_F11;
132000160615           exsr RieTastiFun;
132100160615         //?F06 Conferma
132200160615           %subst(fior000a.tastifun:6:1) = '1';
132300160615           tastof = FIOR001_F06;
132400160615           exsr RieTastiFun;
132500160616         //?F13 Conferma Prenotazione Ritiro
132600160616         //?solo in immissione
132700160615           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
132800160615               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
132900160615             %subst(fior000a.tastifun:13:1) = '1';
133000160615             tastof = FIOR001_F13;
133100160615             exsr RieTastiFun;
133200160615           ENDIF;
133300160615         //?F14 Orari Servizio
133400160615           %subst(fior000a.tastifun:14:1) = '1';
133500160615           tastof = FIOR001_F14;
133600160615           exsr RieTastiFun;
133700160616         //?F20 Simulazione
133800160616         //?solo in immissione
133900160615           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
134000170530               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
134100170530               prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
134200160615             %subst(fior000a.tastifun:20:1) = '1';
134300160615             tastof = FIOR001_F20;
134400160615             exsr RieTastiFun;
134500160615           ENDIF;
134600160616         //?F16 Chiusura
134700160616         //?solo in manutenzione
134800160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
134900160616             %subst(fior000a.tastifun:16:1) = '1';
135000160616             tastof = FIOR001_F16;
135100160616             exsr RieTastiFun;
135200160616           ENDIF;
135300160616         //?F19 Proposte di variazione
135400160616         //?solo in manutenzione
135500170530         // ------>>>>
135600170530           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
135700170530             %subst(fior000a.tastifun:19:1) = '1';
135800170530             tastof = FIOR001_F19;
135900170530             exsr RieTastiFun;
136000170530           ENDIF;
136100160616         //?F21 Dirottamento
136200160616         //?solo in manutenzione
136300160616           IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
136400160616             IF  fior000i.fao < FIOR001_ORM_ASSEGNATO and
136500160616                 fior000i.tor <> 'P' and
136600160616                 %lookup(%editc(wPor:'X'):trul31ds.pog) > 0;
136700160616               %subst(fior000a.tastifun:21:1) = '1';
136800160616               tastof = FIOR001_F21;
136900160616               exsr RieTastiFun;
137000160616             ENDIF;
137100160616           ENDIF;
137200160615         //?F15 Cappario DPD
137300160616         //?viene attivato solo se necessario
137400160616         //?non è scritto nelle righe dei tasti funzione
137500160616           IF  wOkF15;
137600160616             %subst(fior000a.tastifun:15:1) = '1';
137700160616           ENDIF;
137800140618
137900160615         //?Imposto il campo relativo al tasto F24
138000160615           SELECT;
138100160615             WHEN  posrtf2 <> 0 and posrtf3 <> 0;
138200160615               nrighe = 3;
138300160615               fior000a.rigatf24 = FIOR001_F24_1_3;
138400160615             WHEN  posrtf2 <> 0 and posrtf3 = 0;
138500160615               nrighe = 2;
138600160615               fior000a.rigatf24 = FIOR001_F24_1_2 ;
138700160615             OTHER;
138800160615               nrighe = 1;
138900160615               clear fior000a.rigatf24;
139000160615           ENDSL;
139100140618
139200160616         //?Imposto il campo relativo ai tasti mobili
139300160616           fior000a.rigatfmob = rigatf1;
139400160616           contartf = 1;
139500170330
139600170330         //?Attivo RollUp e RollDown in immissione e manutenzione
139700170330           IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
139800170330               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
139900170530               prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI or
140000170330               prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
140100170330             %subst(fior000a.tastifun:25:1) = '1';
140200170330             %subst(fior000a.tastifun:26:1) = '1';
140300170330           ENDIF;
140400160615
140500160615       //?Tasti funzione su videata F5
140600160615       //?sempre attivi
140700160616         WHEN fior000i.video = FIOR000_VIDEO_ALTRIDATI;
140800160615         //?F06 Conferma
140900160615           %subst(fior000a.tastifun:6:1) = '1';
141000160615         //?F12 Ritorno
141100160615           %subst(fior000a.tastifun:12:1) = '1';
141200160615
141300160615       //?Tasti funzione su videata F4
141400160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
141500160616         //?F06 Conferma
141600160616           %subst(fior000a.tastifun:6:1) = '1';
141700160616         //?F10 Copia Telefono
141800160616           %subst(fior000a.tastifun:10:1) = '1';
141900160616         //?F12 Ritorno
142000160616           %subst(fior000a.tastifun:12:1) = '1';
142100160615
142200160615       //?Tasti funzione su videata F13
142300160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
142400160616         //?F06 Conferma
142500160616           %subst(fior000a.tastifun:6:1) = '1';
142600160616         //?F10 Copia Telefono
142700160616           %subst(fior000a.tastifun:10:1) = '1';
142800160616         //?F12 Ritorno
142900160616           %subst(fior000a.tastifun:12:1) = '1';
143000160615
143100160615         ENDSL;
143200140618
143300140618       ENDSR;
143400140618
143500140618       //--------------------------------------------------------------
143600140618       //?Riempo le righe dei tasti Funzionali.
143700140618       //--------------------------------------------------------------
143800141008       BEGSR  RieTastiFun;
143900140618
144000140618         pos = %scan('#':tastof);
144100140618
144200140618       //?Riga 1
144300140618         posrtf1 += pos;
144400140618         IF  posrtf1 <= 61;
144500140618           %subst(rigatf1:(posrtf1-pos+1):(pos)) =
144600140618           %subst(tastof:1:(pos-1));
144700140618           leavesr;
144800140618         ENDIF;
144900140618
145000140618       //?Riga 2
145100140618         posrtf2 += pos;
145200140618         IF  posrtf2 <= 61;
145300140618           %subst(rigatf2:(posrtf2-pos+1):(pos)) =
145400140618           %subst(tastof:1:(pos-1));
145500140618           leavesr;
145600140618         ENDIF;
145700140618
145800140618       //?Riga 3
145900140618         posrtf3 += pos;
146000140618         IF  posrtf3 <= 61;
146100140618           %subst(rigatf3:(posrtf3-pos+1):(pos)) =
146200140618           %subst(tastof:1:(pos-1));
146300140618           leavesr;
146400140618         ENDIF;
146500140618
146600140618       ENDSR;
146700140618
146800140618       //--------------------------------------------------------------
146900140618       //?Imposto gli attributi del video.
147000140618       //--------------------------------------------------------------
147100141008       BEGSR  Attributi;
147200140618
147300140618       //?Se ho già caricato 99 Attributi esco
147400140618         IF  fior000a.nrAttr = 99;
147500140618           exsr RoutEnd;
147600140618           clear fior000a.nrAttr;
147700140618         ENDIF;
147800140618
147900140618         fior000a.nrAttr += 1;
148000140618         fior000a.skIdAttr(fior000a.nrAttr) = wIdAttr;
148100140618         fior000a.skIdCampo(fior000a.nrAttr) = wIdCampo;
148200140618
148300140618       ENDSR;
148400160616
148500160616       //--------------------------------------------------------------
148600160616       //?Decodifico i campi del video.
148700160616       //--------------------------------------------------------------
148800160616       BEGSR  Decodifica;
148900160616
149000160616       //?Imposto la descrizione del tipo ORM
149100160616         keytabella = fior000i.tor;
149200160616         exsr DecoTipoOrm;
149300160616
149400160616       //?Imposto la descrizione del tipo comunicazione ORM
149500160616         keytabella = fior000i.tco;
149600160616         exsr DecoTipoComOrm;
149700160616
149800160616       //?Imposto la descrizione della filiale ritiro
149900160616         clear fior000i.dpor;
150000160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
150100160616           wFiliale = %int(fior000i.por);
150200160616           fior000i.dpor = GetDesFiliale(wFiliale);
150300160616         ENDIF;
150400160616
150500160616       //?Imposto la descrizione della filiale consegna
150600160616         clear fior000i.dpoc;
150700160616         IF  fior000i.poc <> *blanks and fior000i.poc > *zeros;
150800160616           wFiliale = %int(fior000i.poc);
150900160616           fior000i.dpoc = GetDesFiliale(wFiliale);
151000160616         ENDIF;
151100160616
151200160616       //?Imposto la descrizione del giro
151300160616         clear fior000i.dcgi;
151400160616         IF  fior000i.cgi <> *blanks and
151500160616             fior000i.por <> *blanks and fior000i.por > *zeros;
151600160616           wFiliale = %int(fior000i.por);
151700160616           fior000i.dcgi = GetDesGiro(fior000i.cgi:wFiliale);
151800160616         ENDIF;
151900160620
152000160620       //?Imposto la descrizione dello stato
152100160620         keytabella = %editc(fior000i.sto:'X');
152200160620         exsr DecoStatoOrm;
152300160616
152400160616       ENDSR;
152500160616
152600160616       //--------------------------------------------------------------
152700160616       //?Controlli da fare prima della gestione dei tasti video.
152800160616       //--------------------------------------------------------------
152900160616       BEGSR  PrimiControlli;
153000160616
153100160616       //?Controlli sul mittente
153200160616         exsr Mittente;
153300160616         IF  RiemetteVideo;
153400160616           leavesr;
153500160616         ENDIF;
153600160616
153700170330       //?Controlli sulla data pronta merce
153800170330         exsr DataProntaMerce;
153900160616         IF  RiemetteVideo;
154000160616           leavesr;
154100160616         ENDIF;
154200160616
154300160616       //?Controlli sul chi paga
154400160616         exsr ChiPaga;
154500160616
154600160616       //?Controlli sulla filiale ritiro
154700160616         exsr FilRitiro;
154800160616         IF  RiemetteVideo;
154900160616           leavesr;
155000160616         ENDIF;
155100160616
155200160616       //?Controlli sul destinatario
155300160616         exsr Destinatario;
155400160616         IF  RiemetteVideo;
155500160616           leavesr;
155600160616         ENDIF;
155700160616
155800160616       //?Controlli sull'ordinante
155900160616         exsr Ordinante;
156000160616         IF  RiemetteVideo;
156100160616           leavesr;
156200160616         ENDIF;
156300160616
156400160616       //?Controlli sulla filiale consegna
156500160616         exsr FilConsegna;
156600160616         IF  RiemetteVideo;
156700160616           leavesr;
156800160616         ENDIF;
156900170412
157000170412       //?Se variato Codice Mittente
157100170412       //?           Codice Ordinante
157200170412       //?           Data Pronta Merce
157300170412       //?           Quantità
157400170412       //?           Commissionato
157500170412       //?Devo ricalcolare la data di ritiro
157600170413       //?devo azzerare la data ritiro così viene ricalcolata
157700170413         IF  fior000o.cra1 <> fior000i.cra1 or
157800170413             fior000o.cra2 <> fior000i.cra2 or
157900170413             fior000o.cra3 <> fior000i.cra3 or
158000170413             fior000o.cor1 <> fior000i.cor1 or
158100170413             fior000o.cor2 <> fior000i.cor2 or
158200170413             fior000o.cor3 <> fior000i.cor3 or
158300170413             fior000o.dpm  <> fior000i.dpm  or
158400170413             fior000o.pkg  <> fior000i.pkg  or
158500170413             fior000o.vlm  <> fior000i.vlm  or
158600170413             fior000o.bnc  <> fior000i.bnc  or
158700170413             fior000o.blc  <> fior000i.blc  or
158800170413             fior000o.att  <> fior000i.att  or
158900170413             fior000o.mtc  <> fior000i.mtc  or
159000170413             fior000o.comm <> fior000i.comm;
159100170413           clear fior000o.dar;
159200170413         ENDIF;
159300160616
159400160616       ENDSR;
159500160321
159600160321       //--------------------------------------------------------------
159700160321       //?Decodifico il tipo Comunicazione ORM.
159800160321       //--------------------------------------------------------------
159900160321       BEGSR  DecoTipoComOrm;
160000160321
160100160321         clear fior000i.dtco;
160200160321
160300160321         IF  getTabella (c_tntbe:'TCO':keytabella:
160400160321                         *blanks:*blanks:*blanks:
160500160321                         *omit:*omit:*omit:*omit:*omit:
160600160321                         *omit:*omit:*omit:*omit:*omit:
160700160321                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
160800160321             = *zeros and ds_TNTBE.TBEatb = *blanks;
160900160321           fior000i.dtco = ds_TNTBE.TBEuni;
161000160321         ENDIF;
161100170530
161200170530       //?Se tipo comunicazione FISSO aggiungo anche il n. ORM fisso
161300170530         IF  fior000i.tco = FIORA00_TIPOCOMORM_FISSO;
161400170530           fior000i.dtco = %trim(fior000i.dtco) + ' ' +
161500170530           %editc(fior000i.pos:'X') + '-' +
161600170530           %trim(%editc(fior000i.ors:'4'));
161700170530         ENDIF;
161800160321
161900160321       ENDSR;
162000141020
162100141020       //--------------------------------------------------------------
162200141020       //?Decodifico il tipo ORM.
162300141020       //--------------------------------------------------------------
162400141020       BEGSR  DecoTipoOrm;
162500141020
162600141020         clear fior000i.dtor;
162700141020
162800141020         IF  getTabella (c_tntbe:'TOR':keytabella:
162900141020                         *blanks:*blanks:*blanks:
163000141020                         *omit:*omit:*omit:*omit:*omit:
163100141020                         *omit:*omit:*omit:*omit:*omit:
163200141020                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
163300141020             = *zeros and ds_TNTBE.TBEatb = *blanks;
163400141020           fior000i.dtor = ds_TNTBE.TBEuni;
163500141020         ENDIF;
163600141020
163700141020       ENDSR;
163800160620
163900160620       //--------------------------------------------------------------
164000160620       //?Decodifico lo Stato ORM.
164100160620       //--------------------------------------------------------------
164200160620       BEGSR  DecoStatoOrm;
164300160620
164400160620         clear fior000i.dsto;
164500160620
164600160620         IF  getTabella (c_tntbe:'STO':keytabella:
164700160620                         *blanks:*blanks:*blanks:
164800160620                         *omit:*omit:*omit:*omit:*omit:
164900160620                         *omit:*omit:*omit:*omit:*omit:
165000160620                         ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
165100160620             = *zeros and ds_TNTBE.TBEatb = *blanks;
165200160620           fior000i.dsto = ds_TNTBE.TBEuni;
165300160620         ENDIF;
165400160620
165500160620       ENDSR;
165600141010
165700141010       //--------------------------------------------------------------
165800141010       //?Controlli sul mittente.
165900141010       //--------------------------------------------------------------
166000141010       BEGSR  Mittente;
166100141010
166200160607       //?Se cambio codice mittente
166300160607         IF  fior000i.cra1 <> fior000o.cra1 or
166400160607             fior000i.cra2 <> fior000o.cra2 or
166500160607             fior000i.cra3 <> fior000o.cra3;
166600160607         //?Carico i dati del mittente
166700160607           wCodice  = (fior000o.cra1 * 10000000);
166800160607           wCodice += (fior000o.cra2 * 1000);
166900160607           wCodice += (fior000o.cra3);
167000160607         //?se da codificato passo a non codificato
167100160607           IF  wCodice = 0;
167200160607         //?se paga mittente pulisco KSC e CTR
167300160607             IF  fior000o.pag = FIORA00_PAGA_MITTENTE;
167400160607               clear fior000o.ksc;
167500160607               clear fior000o.ctr;
167600160607             ENDIF;
167700160607           //?Se NON è Manutenzione
167800160613             IF  prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE;
167900160613             //?pulisco il campo della data ritiro
168000160613             //?così il FIORA la ricalcola
168100160607               clear fior000o.dar;
168200160607             ENDIF;
168300160607           ENDIF;
168400160615           IF  wCodice > 0;
168500160615             exsr GetDatiMittente;
168600160615           ENDIF;
168700160607         //?pulisco tutte le forzature già memorizzate
168800160607           reset FIORA0F0;
168900170419         //?pulisco il giro perchè cambiando codice devo cambiare anche il giro
169000170419           clear fior000o.tgi;
169100170419           clear fior000o.cgi;
169200160606         ENDIF;
169300141021
169400141021       //?Se non ci sono dati non controllo
169500160525       //?o se non è stato variato il codice
169600160525         IF  (fior000o.inr = *blanks and
169700160525              fior000o.lor = *blanks and fior000o.car = *blanks and
169800160525              fior000o.prr = *blanks and fior000o.nar = *blanks) or
169900160615             (fior000i.cra1 = fior000o.cra1 and
170000160615              fior000i.cra2 = fior000o.cra2 and
170100160615              fior000i.cra3 = fior000o.cra3 and
170200160615              fior000o.cra1 > 0 and fior000o.cra2 > 0);
170300141021           leavesr;
170400141021         ENDIF;
170500141021
170600141021       //?Devo controllare l'indirizzo completo del mittente
170700141021         clear FNLV13DS;
170800141021         clear TISI95DS;
170900141021         fnlv13ds.I13af0 = 'S';
171000141021         fnlv13ds.I13af1 = 'S';
171100141021         fnlv13ds.I13sz2 = 'S';
171200141021         fnlv13ds.I13la3 = 'S';
171300141021         fnlv13ds.I13sz3 = 'S';
171400141021         fnlv13ds.I13cnv = 'S';
171500141021         tisi95ds.I95tcn = '7';
171600141021         tisi95ds.I95ind = fior000o.inr;
171700141021         tisi95ds.I95loc = fior000o.lor;
171800141021         tisi95ds.i95cap = fior000o.car;
171900141021         tisi95ds.i95prv = fior000o.prr;
172000141021         tisi95ds.i95nar = fior000o.nar;
172100141021       //?Se richiesto ntw DPD lo passo
172200141021         IF  fior000o.ntwdpd <> *blanks;
172300141021           tisi95ds.I95fi1 = fior000o.ntwdpd;
172400141021         ENDIF;
172500141021
172600141021       //?Gestione forzature in caso di cambio indirizzo
172700160525         IF  fior000i.car <> fior000o.car;
172800160525           fior000i.car = fior000o.car;
172900160525           clear wfzvr;
173000160525           clear wfz1r;
173100160525           clear wfz2r;
173200160525           clear wfz3r;
173300141021         ENDIF;
173400160525         IF  fior000i.lor <> fior000o.lor;
173500160525           fior000i.lor = fior000o.lor;
173600160525           clear wfz1r;
173700160525           clear wfz2r;
173800160525           clear wfz3r;
173900141021         ENDIF;
174000160525         IF  fior000i.prr <> fior000o.prr;
174100160525           fior000i.prr = fior000o.prr;
174200160525           clear wfz3r;
174300141021         ENDIF;
174400160525         fnlv13ds.E13fzv = wfzvr;
174500160525         fnlv13ds.E13fz1 = wfz1r;
174600160525         fnlv13ds.E13fz2 = wfz2r;
174700160525         fnlv13ds.E13fz3 = wfz3r;
174800141021
174900141021         fnlv13r (kpjba:fnlv13ds:tisi95ds);
175000160525
175100160525         werrr = fnlv13ds.O13err;
175200160525         wmsgr = fnlv13ds.O13msg;
175300160525         wfzvr = fnlv13ds.E13fzv;
175400160525         wfz1r = fnlv13ds.E13fz1;
175500160525         wfz2r = fnlv13ds.E13fz2;
175600160525         wfz3r = fnlv13ds.E13fz3;
175700141021
175800141021       //?Se non presente il codice mittente
175900141021         IF  fior000o.cra1 = 0 and fior000o.cra2 = 0;
176000141021       //?Se il livello di affidabilità non è inferiore a 2 e
176100141021       //?il livello di reperimento non è inferiore a 2
176200141021       //?reimposto i dati da TISI95
176300141021           IF  fnlv13ds.O13ron = 'S';
176400141021             fior000o.nar = tisi95ds.O95nar;
176500141021           ENDIF;
176600141021           IF  fnlv13ds.O13roc = 'S';
176700141021             fior000o.car = tisi95ds.O95cap;
176800141021           ENDIF;
176900141021           IF  fnlv13ds.O13rop = 'S';
177000141021             fior000o.prr = tisi95ds.O95prv;
177100141021           ENDIF;
177200141021           IF  fnlv13ds.O13rol = 'S';
177300141021             fior000o.lor = tisi95ds.O95loc;
177400141021           ENDIF;
177500141021       //?per errore o se variato indirizzo messaggio da FNLV13
177600160525           IF  fnlv13ds.O13err <> *blanks and
177700160525               fnlv13ds.O13msg <> *blanks;
177800160608             reset fiora0M0;
177900141021             SELECT;
178000141021             WHEN  fnlv13ds.O13err = '3';
178100141021               wIdCampo = 'VAOLOR';
178200141021             WHEN  fnlv13ds.O13err = '5';
178300141021               wIdCampo = 'VAOCAR';
178400141021             WHEN  fnlv13ds.O13err = '4';
178500141021               wIdCampo = 'VAOPRR';
178600141021             WHEN  fnlv13ds.O13err = '6';
178700141021               wIdCampo = 'VAONAR';
178800141021             ENDSL;
178900160520             wIdMsg   = 'TIS0718';
179000160520             wParm = fnlv13ds.O13msg;
179100160520             exsr Messaggi;
179200160520             RiemetteVideo = *on;
179300160520             leavesr;
179400160520           ENDIF;
179500141021         ENDIF;
179600141021
179700141021       //?Se presente il codice mittente
179800160412         IF  fior000o.cra1 > 0 and fior000o.cra2 > 0 and
179900141021             fnlv13ds.O13err <> *blanks and
180000141021             fnlv13ds.O13msg <> *blanks;
180100160608           reset fiora0M0;
180200141021           wIdMsg   = 'TIS0798';
180300160525           wIdCampo = 'VAOCRA';
180400141021           clear wParm;
180500141021           exsr Messaggi;
180600160519           RiemetteVideo = *on;
180700141021           leavesr;
180800141021         ENDIF;
180900141021
181000141021       //?Controlla se reimpostati i dati dell'indirizzo
181100141021         IF  fnlv13ds.O13rol = 'S';
181200160608           reset fiora0M0;
181300141021           wIdMsg   = 'TIS0718';
181400141021           wIdCampo = 'VAOLOR';
181500141021           wParm = 'ATTENZIONE!! Modificata Località.';
181600141021           exsr Forzatura;
181700160519           RiemetteVideo = *on;
181800141021           leavesr;
181900141021         ENDIF;
182000141021         IF  fnlv13ds.O13rop = 'S';
182100160608           reset fiora0M0;
182200141021           wIdMsg   = 'TIS0718';
182300141021           wIdCampo = 'VAOPRR';
182400141021           wParm = 'ATTENZIONE!! Modificata Provincia.';
182500141021           exsr Forzatura;
182600160519           RiemetteVideo = *on;
182700141021           leavesr;
182800141021         ENDIF;
182900141021         IF  fnlv13ds.O13roc = 'S';
183000160608           reset fiora0M0;
183100141021           wIdMsg   = 'TIS0718';
183200141021           wIdCampo = 'VAOCAR';
183300141021           wParm = 'ATTENZIONE!! Modificata CAP.';
183400141021           exsr Forzatura;
183500160519           RiemetteVideo = *on;
183600141021           leavesr;
183700141021         ENDIF;
183800141021         IF  fnlv13ds.O13ron = 'S';
183900160608           reset fiora0M0;
184000141021           wIdMsg   = 'TIS0718';
184100141021           wIdCampo = 'VAONAR';
184200141021           wParm = 'ATTENZIONE!! Modificata Nazione.';
184300141021           exsr Forzatura;
184400160519           RiemetteVideo = *on;
184500141021           leavesr;
184600141021         ENDIF;
184700141009
184800141009       ENDSR;
184900160412
185000160412       //--------------------------------------------------------------
185100160412       //?Recupero i dati del mittente.
185200160412       //--------------------------------------------------------------
185300160412       BEGSR  GetDatiMittente;
185400160412
185500160412         reset FIORB0OR;
185600160412
185700160519       //?Se non valido o bloccato vado via
185800160519       //?(ci pensa il FIORA00R a tornare l'errore)
185900160412         IF  not IsCodiceValido(FIORB0OR);
186000160412           leavesr;
186100160412         ENDIF;
186200160412
186300160412       //?Imposto i dati del mittente
186400160412         fior000o.rsr = fiorb0OR.nome;
186500160412         fior000o.inr = fiorb0OR.indirizzo;
186600160412         fior000o.car = fiorb0OR.cap;
186700160412         fior000o.lor = fiorb0OR.localita;
186800160412         fior000o.prr = fiorb0OR.provincia;
186900160412         fior000o.nar = fiorb0OR.nazione;
187000170420         IF  fiorb0OR.oraritiro > *zeros;
187100170420           fior000o.orr = fiorb0OR.oraritiro;
187200170420         ENDIF;
187300160412         fior000o.ter = fiorb0OR.telefono;
187400160412         fior000o.rer = fiorb0OR.referente;
187500160412         fior000o.nam = fiorb0OR.natmerce;
187600160412         fior000o.dalleam = fiorb0OR.dalleam;
187700160412         fior000o.alleam = fiorb0OR.alleam;
187800160412         fior000o.dallepm = fiorb0OR.dallepm;
187900160412         fior000o.allepm = fiorb0OR.allepm;
188000160412         fior000o.spi = fiorb0OR.sponda;
188100160531         IF  fiorb0OR.ordinante > 0;
188200160531           fior000o.cor1 =
188300160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):1:3):3:0);
188400160531           fior000o.cor2 =
188500160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):4:4):4:0);
188600160531           fior000o.cor3 =
188700160531           %dec(%subst(%editc(fiorb0OR.ordinante:'X'):8:3):3:0);
188800160531         ENDIF;
188900160412
189000160412       //?Se paga mittente imposto i dati
189100160412         IF  fior000o.pag = FIORA00_PAGA_MITTENTE;
189200160412           fior000o.ksc = fiorb0OR.codksc;
189300160412           IF  fiorb0OR.codtariffa = 999;
189400160412             clear fior000o.ctr;
189500160412           ELSE;
189600160412             fior000o.ctr =
189700160412             %subst(%editc(fiorb0OR.codtariffa:'X'):2:3);
189800160412           ENDIF;
189900160412         ENDIF;
190000160531
190100160531       //?Alert Conferma Prenotazione ritiro
190200170426       //?se non presente l'ordinante
190300170426         IF  fiorb0OR.alertconf = 'S' and
190400170426             fior000o.cor1 = 0 and
190500170426             fior000o.cor2 = 0;
190600160531           fior000o.mailconf = fiorb0OR.mailconf;
190700160531           fior000o.smsconf = fiorb0OR.smsconf;
190800170505           IF  fior000o.mailconf <> *blanks;
190900170505             fior000i.alertcmail = 'S';
191000170505           ENDIF;
191100170505           IF  fior000o.smsconf <> *blanks;
191200170505             fior000i.alertcsms = 'S';
191300170505           ENDIF;
191400160531         ENDIF;
191500160412
191600160412       //?msg INFO se sto sostituendo le note già messe dall'utente
191700160531         IF  (fior000o.nota1 <> *blanks and
191800160531              fior000o.nota1 <> fiorb0OR.nota1 and
191900160531              fiorb0OR.nota1 <> *blanks) or
192000160531             (fior000o.nota2 <> *blanks and
192100160531              fior000o.nota2 <> fiorb0OR.nota2 and
192200160531              fiorb0OR.nota2 <> *blanks);
192300160608           reset fiora0M0;
192400160531           wIdMsg   = 'TIS0718';
192500160531           wIdCampo = 'VAONO1';
192600160531           wParm = 'ATTENZIONE!! Le note immesse sono state sovrascritte.';
192700160531           exsr Forzatura;
192800160531           RiemetteVideo = *on;
192900160531         ENDIF;
193000160531         IF  fiorb0OR.nota1 <> *blanks;
193100160531           fior000o.nota1 = fiorb0OR.nota1;
193200160531         ENDIF;
193300160531         IF  fiorb0OR.nota2 <> *blanks;
193400160531           fior000o.nota2 = fiorb0OR.nota2;
193500160531         ENDIF;
193600160531         IF  RiemetteVideo;
193700160531           leavesr;
193800160531         ENDIF;
193900160412
194000160412       ENDSR;
194100160616
194200160616       //--------------------------------------------------------------
194300160616       //?Controlli sull'ordinante.
194400160616       //--------------------------------------------------------------
194500160616       BEGSR  Ordinante;
194600160616
194700160616       //?Se cambio codice ordinante
194800160616         IF  fior000i.cor1 <> fior000o.cor1 or
194900160616             fior000i.cor2 <> fior000o.cor2 or
195000160616             fior000i.cor3 <> fior000o.cor3;
195100160616         //?Carico i dati dell' ordinante
195200160616           wCodice  = (fior000o.cor1 * 10000000);
195300160616           wCodice += (fior000o.cor2 * 1000);
195400160616           wCodice += (fior000o.cor3);
195500160616         //?se da codificato passo a non codificato
195600160616           IF  wCodice = 0;
195700160616         //?se paga ordinante pulisco KSC e CTR
195800160616             IF  fior000o.pag = FIORA00_PAGA_ORDINANTE;
195900160616               clear fior000o.ksc;
196000160616               clear fior000o.ctr;
196100160616             ENDIF;
196200160616           ENDIF;
196300160616           IF  wCodice > 0;
196400160616             exsr GetDatiOrdinante;
196500160616           ENDIF;
196600160616         ENDIF;
196700160616
196800160616       //?Se non ci sono dati non controllo
196900160616       //?o se non è stato variato il codice
197000160616         IF  (fior000o.ino = *blanks and
197100160616              fior000o.loo = *blanks and fior000o.cao = *blanks and
197200160616              fior000o.pro = *blanks and fior000o.nao = *blanks) or
197300160616             (fior000i.cor1 = fior000o.cor1 and
197400160616              fior000i.cor2 = fior000o.cor2 and
197500160616              fior000i.cor3 = fior000o.cor3 and
197600160616              fior000o.cor1 > 0 and fior000o.cor2 > 0);
197700160616           leavesr;
197800160616         ENDIF;
197900160616
198000160616       //?Devo controllare l'indirizzo completo dell'ordinante
198100160616         clear FNLV13DS;
198200160616         clear TISI95DS;
198300160616         fnlv13ds.I13af0 = 'S';
198400160616         fnlv13ds.I13af1 = 'S';
198500160616         fnlv13ds.I13sz2 = 'S';
198600160616         fnlv13ds.I13la3 = 'S';
198700160616         fnlv13ds.I13sz3 = 'S';
198800160616         fnlv13ds.I13cnv = 'S';
198900160616         tisi95ds.I95tcn = '7';
199000160616         tisi95ds.I95ind = fior000o.ino;
199100160616         tisi95ds.I95loc = fior000o.loo;
199200160616         tisi95ds.i95cap = fior000o.cao;
199300160616         tisi95ds.i95prv = fior000o.pro;
199400160616         tisi95ds.i95nar = fior000o.nao;
199500160616
199600160616       //?Gestione forzature in caso di cambio indirizzo
199700160616         IF  fior000i.cao <> fior000o.cao;
199800160616           fior000i.cao = fior000o.cao;
199900160616           clear wfzvr;
200000160616           clear wfz1r;
200100160616           clear wfz2r;
200200160616           clear wfz3r;
200300160616         ENDIF;
200400160616         IF  fior000i.loo <> fior000o.loo;
200500160616           fior000i.loo = fior000o.loo;
200600160616           clear wfz1r;
200700160616           clear wfz2r;
200800160616           clear wfz3r;
200900160616         ENDIF;
201000160616         IF  fior000i.pro <> fior000o.pro;
201100160616           fior000i.pro = fior000o.pro;
201200160616           clear wfz3r;
201300160616         ENDIF;
201400160616         fnlv13ds.E13fzv = wfzvr;
201500160616         fnlv13ds.E13fz1 = wfz1r;
201600160616         fnlv13ds.E13fz2 = wfz2r;
201700160616         fnlv13ds.E13fz3 = wfz3r;
201800160616
201900160616         fnlv13r (kpjba:fnlv13ds:tisi95ds);
202000160616
202100160616         werrr = fnlv13ds.O13err;
202200160616         wmsgr = fnlv13ds.O13msg;
202300160616         wfzvr = fnlv13ds.E13fzv;
202400160616         wfz1r = fnlv13ds.E13fz1;
202500160616         wfz2r = fnlv13ds.E13fz2;
202600160616         wfz3r = fnlv13ds.E13fz3;
202700160616
202800160616       //?Se non presente il codice ordinante
202900160616         IF  fior000o.cor1 = 0 and fior000o.cor2 = 0;
203000160616       //?Se il livello di affidabilità non è inferiore a 2 e
203100160616       //?il livello di reperimento non è inferiore a 2
203200160616       //?reimposto i dati da TISI95
203300160616           IF  fnlv13ds.O13ron = 'S';
203400160616             fior000o.nao = tisi95ds.O95nar;
203500160616           ENDIF;
203600160616           IF  fnlv13ds.O13roc = 'S';
203700160616             fior000o.cao = tisi95ds.O95cap;
203800160616           ENDIF;
203900160616           IF  fnlv13ds.O13rop = 'S';
204000160616             fior000o.pro = tisi95ds.O95prv;
204100160616           ENDIF;
204200160616           IF  fnlv13ds.O13rol = 'S';
204300160616             fior000o.loo = tisi95ds.O95loc;
204400160616           ENDIF;
204500160616       //?per errore o se variato indirizzo messaggio da FNLV13
204600160616           IF  fnlv13ds.O13err <> *blanks and
204700160616               fnlv13ds.O13msg <> *blanks;
204800160616             reset fiora0M0;
204900160616             SELECT;
205000160616             WHEN  fnlv13ds.O13err = '3';
205100160616               wIdCampo = 'VAOLOO';
205200160616             WHEN  fnlv13ds.O13err = '5';
205300160616               wIdCampo = 'VAOCAO';
205400160616             WHEN  fnlv13ds.O13err = '4';
205500160616               wIdCampo = 'VAOPRO';
205600160616             WHEN  fnlv13ds.O13err = '6';
205700160616               wIdCampo = 'VAONAO';
205800160616             ENDSL;
205900160616             wIdMsg   = 'TIS0718';
206000160616             wParm = fnlv13ds.O13msg;
206100160616             exsr Messaggi;
206200160616             RiemetteVideo = *on;
206300160616             leavesr;
206400160616           ENDIF;
206500160616         ENDIF;
206600160616
206700160616       //?Se presente il codice ordinante
206800160616         IF  fior000o.cor1 > 0 and fior000o.cor2 > 0 and
206900160616             fnlv13ds.O13err <> *blanks and
207000160616             fnlv13ds.O13msg <> *blanks;
207100160616           reset fiora0M0;
207200160616           wIdMsg   = 'TIS0798';
207300160616           wIdCampo = 'VAOCRO';
207400160616           clear wParm;
207500160616           exsr Messaggi;
207600160616           RiemetteVideo = *on;
207700160616           leavesr;
207800160616         ENDIF;
207900160616
208000160616       //?Controlla se reimpostati i dati dell'indirizzo
208100160616         IF  fnlv13ds.O13rol = 'S';
208200160616           reset fiora0M0;
208300160616           wIdMsg   = 'TIS0718';
208400160616           wIdCampo = 'VAOLOO';
208500160616           wParm = 'ATTENZIONE!! Modificata Località.';
208600160616           exsr Forzatura;
208700160616           RiemetteVideo = *on;
208800160616           leavesr;
208900160616         ENDIF;
209000160616         IF  fnlv13ds.O13rop = 'S';
209100160616           reset fiora0M0;
209200160616           wIdMsg   = 'TIS0718';
209300160616           wIdCampo = 'VAOPRO';
209400160616           wParm = 'ATTENZIONE!! Modificata Provincia.';
209500160616           exsr Forzatura;
209600160616           RiemetteVideo = *on;
209700160616           leavesr;
209800160616         ENDIF;
209900160616         IF  fnlv13ds.O13roc = 'S';
210000160616           reset fiora0M0;
210100160616           wIdMsg   = 'TIS0718';
210200160616           wIdCampo = 'VAOCAO';
210300160616           wParm = 'ATTENZIONE!! Modificata CAP.';
210400160616           exsr Forzatura;
210500160616           RiemetteVideo = *on;
210600160616           leavesr;
210700160616         ENDIF;
210800160616         IF  fnlv13ds.O13ron = 'S';
210900160616           reset fiora0M0;
211000160616           wIdMsg   = 'TIS0718';
211100160616           wIdCampo = 'VAONAO';
211200160616           wParm = 'ATTENZIONE!! Modificata Nazione.';
211300160616           exsr Forzatura;
211400160616           RiemetteVideo = *on;
211500160616           leavesr;
211600160616         ENDIF;
211700160616
211800160616       ENDSR;
211900160616
212000160616       //--------------------------------------------------------------
212100160616       //?Recupero i dati dell'ordinante.
212200160616       //--------------------------------------------------------------
212300160616       BEGSR  GetDatiOrdinante;
212400160616
212500160616         reset FIORB0OO;
212600160616
212700160616       //?Se non valido o bloccato vado via
212800160616       //?(ci pensa il FIORA00R a tornare l'errore)
212900160616         IF  not IsCodiceValido(FIORB0OO);
213000160616           leavesr;
213100160616         ENDIF;
213200160616
213300160616       //?Imposto i dati dell'ordinante
213400160616         fior000o.rso = fiorb0OO.nome;
213500160616         fior000o.ino = fiorb0OO.indirizzo;
213600160616         fior000o.cao = fiorb0OO.cap;
213700160616         fior000o.loo = fiorb0OO.localita;
213800160616         fior000o.pro = fiorb0OO.provincia;
213900160616         fior000o.nao = fiorb0OO.nazione;
214000160616
214100160616       //?Se paga ordinante imposto i dati
214200160616         IF  fior000o.pag = FIORA00_PAGA_ORDINANTE;
214300160616           fior000o.ksc = fiorb0OO.codksc;
214400160616           IF  fiorb0OO.codtariffa = 999;
214500160616             clear fior000o.ctr;
214600160616           ELSE;
214700160616             fior000o.ctr =
214800160616             %subst(%editc(fiorb0OR.codtariffa:'X'):2:3);
214900160616           ENDIF;
215000160616         ENDIF;
215100170426
215200170426       //?Imposto anche il flag del commissionato
215300170426         fior000o.comm = fiorb0OO.contatto;
215400170426
215500170426       //?Alert Conferma Prenotazione ritiro
215600170426         IF  fiorb0OO.alertconf = 'S';
215700170426           fior000o.mailconf = fiorb0OO.mailconf;
215800170426           fior000o.smsconf = fiorb0OO.smsconf;
215900170505           IF  fior000o.mailconf <> *blanks;
216000170505             fior000i.alertcmail = 'S';
216100170505           ENDIF;
216200170505           IF  fior000o.smsconf <> *blanks;
216300170505             fior000i.alertcsms = 'S';
216400170505           ENDIF;
216500170426         ENDIF;
216600170522
216700170522       //?Se mittente non codificato imposto le note dell'ordinante
216800170522         IF  fior000i.cra1 = *zeros and fior000i.cra2 = *zeros;
216900170522           IF  fior000o.nota1 = *blanks;
217000170522             fior000o.nota1 = fiorb0OO.nota1;
217100170522           ENDIF;
217200170522           IF  fior000o.nota2 = *blanks;
217300170522             fior000o.nota2 = fiorb0OO.nota2;
217400170522           ENDIF;
217500170522        ENDIF;
217600160616
217700160616       ENDSR;
217800160616
217900160616       //--------------------------------------------------------------
218000160616       //?Controlli sul destinatario.
218100160616       //--------------------------------------------------------------
218200160616       BEGSR  Destinatario;
218300160616
218400160616       //?Se NON è Manutenzione
218500160616       //?ed è stato inserito il destinatario
218600160616       //?e il tipo ORM è Multiplo
218700160616       //?imposto in automatico tipo ORM Singolo
218800160616         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_MANUTENZIONE and
218900160616             fior000o.tor = FIORA00_TIPOORM_MULTIPLO and
219000160616           ((fior000o.crc1 > 0 and fior000o.crc2 > 0 and
219100160616             fior000i.crc1 <> fior000o.crc1 and
219200160616             fior000i.crc2 <> fior000o.crc2 and
219300160616             fior000i.crc1 = 0 and fior000i.crc2 = 0) or
219400160616            (fior000o.rsc <> *blanks and
219500160616             fior000i.rsc <> fior000o.rsc and
219600160616             fior000i.rsc = *blanks));
219700160616           fior000o.tor = FIORA00_TIPOORM_SINGOLO;
219800160616         ENDIF;
219900160616
220000160616       //?Se cambio codice destinatario
220100160616         IF  fior000i.crc1 <> fior000o.crc1 or
220200160616             fior000i.crc2 <> fior000o.crc2 or
220300160616             fior000i.crc3 <> fior000o.crc3;
220400160616         //?Carico i dati del destinatario
220500160616           wCodice  = (fior000o.crc1 * 10000000);
220600160616           wCodice += (fior000o.crc2 * 1000);
220700160616           wCodice += (fior000o.crc3);
220800160616         //?se da codificato passo a non codificato
220900160616           IF  wCodice = 0;
221000160616         //?se paga destinatario pulisco KSC e CTR
221100160616             IF  fior000o.pag = FIORA00_PAGA_DESTINATARIO;
221200160616               clear fior000o.ksc;
221300160616               clear fior000o.ctr;
221400160616             ENDIF;
221500160616           ENDIF;
221600160616           IF  wCodice > 0;
221700160616             exsr GetDatiDestinatario;
221800160616           ENDIF;
221900160616         ENDIF;
222000160616
222100160616       //?Se non ci sono dati non controllo
222200160616       //?o se non è stato variato il codice
222300160616         IF  (fior000o.inc = *blanks and
222400160616              fior000o.loc = *blanks and fior000o.cac = *blanks and
222500160616              fior000o.prc = *blanks and fior000o.nac = *blanks) or
222600160616             (fior000i.crc1 = fior000o.crc1 and
222700160616              fior000i.crc2 = fior000o.crc2 and
222800160616              fior000i.crc3 = fior000o.crc3 and
222900160616              fior000o.crc1 > 0 and fior000o.crc2 > 0);
223000160616           leavesr;
223100160616         ENDIF;
223200160616
223300160616       //?Devo controllare l'indirizzo completo del destinatario
223400160616         clear FNLV13DS;
223500160616         clear TISI95DS;
223600160616         fnlv13ds.I13af0 = 'S';
223700160616         fnlv13ds.I13af1 = 'S';
223800160616         fnlv13ds.I13sz2 = 'S';
223900160616         fnlv13ds.I13la3 = 'S';
224000160616         fnlv13ds.I13sz3 = 'S';
224100160616         fnlv13ds.I13cnv = 'S';
224200160616         tisi95ds.I95tcn = '7';
224300160616         tisi95ds.I95ind = fior000o.inc;
224400160616         tisi95ds.I95loc = fior000o.loc;
224500160616         tisi95ds.i95cap = fior000o.cac;
224600160616         tisi95ds.i95prv = fior000o.prc;
224700160616         tisi95ds.i95nar = fior000o.nac;
224800160616       //?Se non paga mittente imposto anche il flag per Assegnato
224900160616         IF  fior000o.pag <> FIORA00_PAGA_MITTENTE;
225000160616           tisi95ds.I95tpo = FIORA00_BOLLA_ASSEGNATO;
225100160616         ENDIF;
225200160616
225300160616       //?Gestione forzature in caso di cambio indirizzo
225400160616         IF  fior000i.cac <> fior000o.cac;
225500160616           fior000i.cac = fior000o.cac;
225600160616           clear wfzvr;
225700160616           clear wfz1r;
225800160616           clear wfz2r;
225900160616           clear wfz3r;
226000160616         ENDIF;
226100160616         IF  fior000i.lor <> fior000o.loc;
226200160616           fior000i.loc = fior000o.loc;
226300160616           clear wfz1r;
226400160616           clear wfz2r;
226500160616           clear wfz3r;
226600160616         ENDIF;
226700160616         IF  fior000i.prc <> fior000o.prc;
226800160616           fior000i.prc = fior000o.prc;
226900160616           clear wfz3r;
227000160616         ENDIF;
227100160616         fnlv13ds.E13fzv = wfzvr;
227200160616         fnlv13ds.E13fz1 = wfz1r;
227300160616         fnlv13ds.E13fz2 = wfz2r;
227400160616         fnlv13ds.E13fz3 = wfz3r;
227500160616
227600160616         fnlv13r (kpjba:fnlv13ds:tisi95ds);
227700160616
227800160616         werrr = fnlv13ds.O13err;
227900160616         wmsgr = fnlv13ds.O13msg;
228000160616         wfzvr = fnlv13ds.E13fzv;
228100160616         wfz1r = fnlv13ds.E13fz1;
228200160616         wfz2r = fnlv13ds.E13fz2;
228300160616         wfz3r = fnlv13ds.E13fz3;
228400160616
228500160616       //?Se non presente il codice destinatario
228600160616         IF  fior000o.crc1 = 0 and fior000o.crc2 = 0;
228700160616       //?Se il livello di affidabilità non è inferiore a 2 e
228800160616       //?il livello di reperimento non è inferiore a 2
228900160616       //?reimposto i dati da TISI95
229000160616           IF  fnlv13ds.O13ron = 'S';
229100160616             fior000o.nac = tisi95ds.O95nar;
229200160616           ENDIF;
229300160616           IF  fnlv13ds.O13roc = 'S';
229400160616             fior000o.cac = tisi95ds.O95cap;
229500160616           ENDIF;
229600160616           IF  fnlv13ds.O13rop = 'S';
229700160616             fior000o.prc = tisi95ds.O95prv;
229800160616           ENDIF;
229900160616           IF  fnlv13ds.O13rol = 'S';
230000160616             fior000o.loc = tisi95ds.O95loc;
230100160616           ENDIF;
230200160616       //?per errore o se variato indirizzo messaggio da FNLV13
230300160616           IF  fnlv13ds.O13err <> *blanks and
230400160616               fnlv13ds.O13msg <> *blanks;
230500160616             reset fiora0M0;
230600160616             SELECT;
230700160616             WHEN  fnlv13ds.O13err = '3';
230800160616               wIdCampo = 'VAOLOC';
230900160616             WHEN  fnlv13ds.O13err = '5';
231000160616               wIdCampo = 'VAOCAC';
231100160616             WHEN  fnlv13ds.O13err = '4';
231200160616               wIdCampo = 'VAOPRC';
231300160616             WHEN  fnlv13ds.O13err = '6';
231400160616               wIdCampo = 'VAONAC';
231500160616             ENDSL;
231600160616             wIdMsg   = 'TIS0718';
231700160616             wParm = fnlv13ds.O13msg;
231800160616             exsr Messaggi;
231900160616             RiemetteVideo = *on;
232000160616             leavesr;
232100160616           ENDIF;
232200160616         ENDIF;
232300160616
232400160616       //?Se presente il codice destinatario
232500160616         IF  fior000o.crc1 > 0 and fior000o.crc2 > 0 and
232600160616             fnlv13ds.O13err <> *blanks and
232700160616             fnlv13ds.O13msg <> *blanks;
232800160616           reset fiora0M0;
232900160616           wIdMsg   = 'TIS0798';
233000160616           wIdCampo = 'VAOCRC';
233100160616           clear wParm;
233200160616           exsr Messaggi;
233300160616           RiemetteVideo = *on;
233400160616           leavesr;
233500160616         ENDIF;
233600160616
233700160616       //?Controlla se reimpostati i dati dell'indirizzo
233800160616         IF  fnlv13ds.O13rol = 'S';
233900160616           reset fiora0M0;
234000160616           wIdMsg   = 'TIS0718';
234100160616           wIdCampo = 'VAOLOC';
234200160616           wParm = 'ATTENZIONE!! Modificata Località.';
234300160616           exsr Forzatura;
234400160616           RiemetteVideo = *on;
234500160616           leavesr;
234600160616         ENDIF;
234700160616         IF  fnlv13ds.O13rop = 'S';
234800160616           reset fiora0M0;
234900160616           wIdMsg   = 'TIS0718';
235000160616           wIdCampo = 'VAOPRC';
235100160616           wParm = 'ATTENZIONE!! Modificata Provincia.';
235200160616           exsr Forzatura;
235300160616           RiemetteVideo = *on;
235400160616           leavesr;
235500160616         ENDIF;
235600160616         IF  fnlv13ds.O13roc = 'S';
235700160616           reset fiora0M0;
235800160616           wIdMsg   = 'TIS0718';
235900160616           wIdCampo = 'VAOCAC';
236000160616           wParm = 'ATTENZIONE!! Modificata CAP.';
236100160616           exsr Forzatura;
236200160616           RiemetteVideo = *on;
236300160616           leavesr;
236400160616         ENDIF;
236500160616         IF  fnlv13ds.O13ron = 'S';
236600160616           reset fiora0M0;
236700160616           wIdMsg   = 'TIS0718';
236800160616           wIdCampo = 'VAONAC';
236900160616           wParm = 'ATTENZIONE!! Modificata Nazione.';
237000160616           exsr Forzatura;
237100160616           RiemetteVideo = *on;
237200160616           leavesr;
237300160616         ENDIF;
237400160616
237500160616       //?Se presente la Nazione ed è lunga 2 byte e questa è presente anche
237600160616       //?come Provincia devo avvisare l'utente per sapere se è giusta la nazione
237700160616       //?o se voleva inserire la provincia
237800160616       //?Caso di nazione 'VE' accettata in immissione ORM perchè esiste la
237900160616       //?nazione VE Venezuale ma l'utente voleva inserire VE Venezia,
238000160616       //?ha sbagliato campo
238100160616         IF  fior000o.nac <> *blanks and
238200160616            ((%subst(fior000o.nac:1:1) = *blanks and
238300160616              %subst(fior000o.nac:2:2) <> *blanks) or
238400160616             (%subst(fior000o.nac:1:2) <> *blanks and
238500160616              %subst(fior000o.nac:3:1) = *blanks));
238600160616           k_TBLcod = 'PR';
238700160616           k_TBLkey = fior000o.nac;
238800160616           chain %kds(K03tabel) TABEL00F;
238900160616           IF  %found(TABEL00F) and TBLflg = *blanks;
239000160616             k_TBLcod = '15';
239100160616             k_TBLkey = fior000o.nac;
239200160616             chain %kds(K03tabel) TABEL00F;
239300160616             IF  %found(TABEL00F);
239400160616               ds15 = TBLuni;
239500160616             ENDIF;
239600160616             reset fiora0M0;
239700160616             wIdMsg   = 'TIS0718';
239800160616             wIdCampo = 'VAONAC';
239900160616             wParm = 'Verificare la nazione del destinatario ' +
240000160616                      %trim(fior000o.nac) + '-' + %trim(ds15.§15des);
240100160616             exsr Forzatura;
240200160616             RiemetteVideo = *on;
240300160616             leavesr;
240400160616           ENDIF;
240500160616         ENDIF;
240600160616
240700160616       ENDSR;
240800160616
240900160616       //--------------------------------------------------------------
241000160616       //?Recupero i dati del destinatario.
241100160616       //--------------------------------------------------------------
241200160616       BEGSR  GetDatiDestinatario;
241300160616
241400160616         reset FIORB0OC;
241500160616
241600160616       //?Se non valido o bloccato vado via
241700160616       //?(ci pensa il FIORA00R a tornare l'errore)
241800160616         IF  not IsCodiceValido(FIORB0OC);
241900160616           leavesr;
242000160616         ENDIF;
242100160616
242200160616       //?Imposto i dati del mittente
242300160616         fior000o.rsc = fiorb0OC.nome;
242400160616         fior000o.inc = fiorb0OC.indirizzo;
242500160616         fior000o.cac = fiorb0OC.cap;
242600160616         fior000o.loc = fiorb0OC.localita;
242700160616         fior000o.prc = fiorb0OC.provincia;
242800160616         fior000o.nac = fiorb0OC.nazione;
242900160616
243000160616       //?Se paga mittente imposto i dati
243100160616         IF  fior000o.pag = FIORA00_PAGA_DESTINATARIO;
243200160616           fior000o.ksc = fiorb0OC.codksc;
243300160616           IF  fiorb0OC.codtariffa = 999;
243400160616             clear fior000o.ctr;
243500160616           ELSE;
243600160616             fior000o.ctr =
243700160616             %subst(%editc(fiorb0OC.codtariffa:'X'):2:3);
243800160616           ENDIF;
243900160616         ENDIF;
244000160616
244100160616       ENDSR;
244200160616
244300160616       //--------------------------------------------------------------
244400160616       //?Controlli sul chi paga.
244500160616       //--------------------------------------------------------------
244600160616       BEGSR  ChiPaga;
244700160616
244800160616       //?Se cambio il Chi Paga cancello il KSC e CTR
244900160616         IF  fior000i.pag <> fior000o.pag;
245000160616           clear fior000o.ksc;
245100160616           clear fior000o.ctr;
245200160616         ENDIF;
245300160616
245400160616       //?Se codice Ordinante = codice Mittente imposto in automatico
245500160616       //?che paga Mittente
245600160616         IF  fior000o.cra1 > 0 and fior000o.cra2 > 0 and
245700160616             fior000o.cor1 > 0 and fior000o.cor2 > 0 and
245800160616             fior000o.cra1 = fior000o.cor1 and
245900160616             fior000o.cra2 = fior000o.cor2 and
246000160616             fior000o.cra3 = fior000o.cor3 and
246100160616             fior000o.pag <> FIORA00_PAGA_MITTENTE;
246200160616           fior000o.pag = FIORA00_PAGA_MITTENTE;
246300160616         ENDIF;
246400160616
246500160616       ENDSR;
246600160608
246700160608       //--------------------------------------------------------------
246800170330       //?Controlli sulla data pronta merce.
246900160608       //--------------------------------------------------------------
247000170330       BEGSR  DataProntaMerce;
247100170404
247200170404       //?Se la data pronta merce a video è vuota la imposto = oggi
247300170404         IF  fior000o.dpm = 0;
247400170404           dataISO = %date(dataoggi:*iso);
247500170404           dataEUR = dataISO;
247600170404           fior000o.dpm = %dec(dataEUR);
247700170404         ENDIF;
247800160608
247900160608       //?Può capitare che a video la data venga inserita lunga 6
248000160608       //?prima di richiamare il FIORA00R la sistemo
248100170330         IF  fior000o.dpm > 0;
248200160608           clear wlbdat;
248300170330           G08dat = fior000o.dpm;
248400160608           xsrda8(wlbdat);
248500160608           IF  G08err = '1';
248600160608             reset fiora0M0;
248700170404             wIdMsg   = 'TIS0718';
248800170330             wIdCampo = 'VAODPM';
248900170404             wParm = 'Data Pronta Merce formalmente errata';
249000160608             exsr Messaggi;
249100160608             RiemetteVideo = *on;
249200160608             leavesr;
249300160608           ENDIF;
249400170330           fior000o.dpm = G08dat;
249500160608         ENDIF;
249600160608
249700160608       ENDSR;
249800160608
249900160608       //--------------------------------------------------------------
250000160608       //?Controlli sulla filiale ritiro.
250100160608       //--------------------------------------------------------------
250200160608       BEGSR  FilRitiro;
250300160608
250400160608         IF  fior000o.por <> *blanks;
250500160608           IF  %subst(fior000o.por:1:1) = *blanks;
250600160608             %subst(fior000o.por:1:1) = '0';
250700160608           ENDIF;
250800160608           IF  %subst(fior000o.por:2:1) = *blanks;
250900160608             %subst(fior000o.por:2:1) = '0';
251000160608           ENDIF;
251100160608           IF  %check(FIOR001_DIGITS:fior000o.por) > *zeros;
251200160608             reset fiora0M0;
251300160608             wIdMsg   = 'TIS0809';
251400160608             wIdCampo = 'VAOPOR';
251500160608             clear wParm;
251600160608             exsr Messaggi;
251700160608             RiemetteVideo = *on;
251800160608             fior000i.por = fior000o.por;
251900160608             clear fior000i.dpor;
252000160608           ENDIF;
252100160608         ENDIF;
252200160608
252300160608       ENDSR;
252400141020
252500141020       //--------------------------------------------------------------
252600141020       //?Controlli sulla filiale consegna.
252700141020       //--------------------------------------------------------------
252800141020       BEGSR  FilConsegna;
252900141020
253000141020         IF  fior000o.poc <> *blanks;
253100160608           IF  %subst(fior000o.poc:1:1) = *blanks;
253200160608             %subst(fior000o.poc:1:1) = '0';
253300160608           ENDIF;
253400160608           IF  %subst(fior000o.poc:2:1) = *blanks;
253500160608             %subst(fior000o.poc:2:1) = '0';
253600160608           ENDIF;
253700141020           IF  %check(FIOR001_DIGITS:fior000o.poc) > *zeros;
253800160608             reset fiora0M0;
253900141020             wIdMsg   = 'TIS0803';
254000141020             wIdCampo = 'VAOPOC';
254100141020             clear wParm;
254200141020             exsr Messaggi;
254300141020             RiemetteVideo = *on;
254400141020             fior000i.poc = fior000o.poc;
254500141020             clear fior000i.dpoc;
254600141020           ENDIF;
254700141020         ENDIF;
254800141020
254900141020       ENDSR;
255000160616
255100160616       //--------------------------------------------------------------
255200160616       //?Tasto ENTER.
255300160616       //--------------------------------------------------------------
255400160616       BEGSR  TastoEnter;
255500160616
255600160616         SELECT;
255700160616       //?Videata WHO
255800160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER and
255900160616               fior000i.video = FIOR000_VIDEO_WHOISORD;
256000160616           exsr ControllaWho;
256100160616       //?Tutte le altre videate
256200160616         WHEN  fior000a.TastoVideo = FIOR000_TASTO_ENTER and
256300160616               fior000i.video <> FIOR000_VIDEO_WHOISORD;
256400160616           exsr ControllaDatiVideo;
256500160616
256600160616         ENDSL;
256700160616
256800160616       ENDSR;
256900160616
257000160616       //--------------------------------------------------------------
257100160616       //?F02 = Dati DPD.
257200160616       //--------------------------------------------------------------
257300160616       BEGSR  TastoF02;
257400160616
257500160616
257600160616       ENDSR;
257700160616
257800160616       //--------------------------------------------------------------
257900160616       //?F04 = Alert Ritiro Merce.
258000160616       //--------------------------------------------------------------
258100160616       BEGSR  TastoF04;
258200160616
258300160616         fior000i.video = FIOR000_VIDEO_ALERTCOMM;
258400160616         FineVideo = *on;
258500160616
258600160616       ENDSR;
258700160615
258800160615       //--------------------------------------------------------------
258900160616       //?F05 = Altri Dati.
259000160615       //--------------------------------------------------------------
259100160615       BEGSR  TastoF05;
259200160616
259300160616         fior000i.video = FIOR000_VIDEO_ALTRIDATI;
259400160616         FineVideo = *on;
259500160615
259600160615       ENDSR;
259700160616
259800160616       //--------------------------------------------------------------
259900160616       //?F06 = Conferma.
260000160616       //--------------------------------------------------------------
260100160616       BEGSR  TastoF06;
260200160616
260300160616       //?Prima devo controllare i dati se Ok allora li convalido
260400160616         exsr ControllaDatiVideo;
260500170505         IF  FineVideo;
260600170505           leavesr;
260700170505         ENDIF;
260800170420
260900170420       //?F06 Dato in videata F05
261000170420       //?se non ci sono errori riferiti a questa videata ritorno alla
261100170420       //?videata principale
261200170420         IF  fior000i.video = FIOR000_VIDEO_ALTRIDATI and
261300170420             %lookup('VAOBLC':fiora0M0.skIdCampo) = 0 and
261400170420             %lookup('VAOMTC':fiora0M0.skIdCampo) = 0 and
261500170420             %lookup('VAOATT':fiora0M0.skIdCampo) = 0 and
261600170420             %lookup('CGI':fiora0M0.skIdCampo) = 0;
261700170420           FineVideo = *on;
261800170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
261900170420           leavesr;
262000170420         ENDIF;
262100170420       //?F06 Dato in videata F04
262200170420       //?se non ci sono errori riferiti a questa videata ritorno alla
262300170420       //?videata principale
262400170420         IF  fior000i.video = FIOR000_VIDEO_ALERTCOMM and
262500170420             %lookup('SMS':fiora0M0.skIdCampo) = 0 and
262600170420             %lookup('MAIL':fiora0M0.skIdCampo) = 0;
262700170420           FineVideo = *on;
262800170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
262900170420           wOKF06F04 = *on;
263000170420           leavesr;
263100170420         ENDIF;
263200170420       //?F06 Dato in videata F13
263300170420       //?se non ci sono errori riferiti a questa videata ritorno alla
263400170420       //?videata principale
263500170420         IF  fior000i.video = FIOR000_VIDEO_ALERTCONF and
263600170420             %lookup('SMSCONF':fiora0M0.skIdCampo) = 0 and
263700170420             %lookup('MAILCONF':fiora0M0.skIdCampo) = 0;
263800170420           FineVideo = *on;
263900170420           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
264000170420           wOKF06F13 = *on;
264100170420           leavesr;
264200170420         ENDIF;
264300160616
264400160616       //?Se ci sono errori esco
264500160616         IF  fiora0m0.NrMsg > 0;
264600160616           leavesr;
264700160616         ENDIF;
264800160616
264900160616       //?F06 Dato in videata principale
265000170420         IF  fior000i.video = FIOR000_VIDEO_PRINCIPALE;
265100170414         //?Come prima cosa devo verificare gli ALERT
265200170414           exsr AlertORM;
265300170414           IF  FineVideo;
265400170414             leavesr;
265500170414           ENDIF;
265600160622           exsr ScriviFileORM;
265700170414         //?Se non ci sono errori in scittura riemetto la videata vuota
265800170414           IF  fiora0m0.NrMsg = 0;
265900170414           //?Imposto i dati nella ds di input
266000170414             exsr MoveDs00;
266100170420           //?Imposto l'ultimo ORM immesso
266200170420             fior000i.poeu = fiora01o.poe;
266300170420             fior000i.nsru = fiora01o.nsr;
266400170420             fior000i.noru = fiora01o.nor;
266500170420             fior000i.nrvu = fiora01o.nrv;
266600160622             FineVideo = *on;
266700170530           //?Se Conferma ORM Fissi devo uscire del tutto
266800170530             IF  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
266900170530               Fine = *on;
267000170530             ENDIF;
267100160622           ENDIF;
267200170420         ENDIF;
267300160616
267400160616       ENDSR;
267500141007
267600141007       //--------------------------------------------------------------
267700160616       //?F07 = Ricerca del mittente.
267800141007       //--------------------------------------------------------------
267900160615       BEGSR  TastoF07;
268000141007
268100141007         clear FIOR81DS;
268200160608         IF  fior000o.cra1 > 0;
268300160608           fior81ds.I81fil = fior000o.cra1;
268400141007         ENDIF;
268500141007         IF  fior000o.rsr <> *blanks;
268600160608           fior81ds.I81rag = fior000o.rsr;
268700141007         ENDIF;
268800141007         fior81r (kpjba:FIOR81DS);
268900141007       //?Se torna errore emetto messaggio a video
269000141007         IF  fior81ds.O81err <> *blanks;
269100160608           reset fiora0M0;
269200141009           wIdMsg   = 'TIS0718';
269300141009           wIdCampo = 'VAOCRA';
269400141009           wParm = fior81ds.O81msg;
269500141009           exsr Messaggi;
269600141008           leavesr;
269700141007         ENDIF;
269800141007       //?Se torna F12 non faccio niente
269900141007         IF  fior81ds.O81ret = '1';
270000141007           leavesr;
270100141007         ENDIF;
270200141008
270300141007       //?Se arrivo qua vuol dire che è stata fatta una scelta
270400141007       //?quindi imposto già i dati del codice scelto
270500160608         fior000o.cra1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
270600160608         fior000o.cra2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
270700160608         fior000o.cra3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
270800141008
270900160608       //?Decodifico
271000160608         exsr Mittente;
271100160608
271200160608       //?imposto i dati dalla DS di Output del FIOR000R
271300160608       //?alla ds di input del FIOR000R
271400160608         eval-corr fior000i = fior000o;
271500160608         reset fior000i.formato;
271600160608         reset fior000i.versione;
271700160608
271800160608       //?Controllo
271900160608         exsr ControllaDatiVideo;
272000141015
272100141015       //?se torna errore ma non è relativo al mittente riemetto il video
272200141015       //?senza errori
272300141015         NrErrore = 1;
272400141015         FOR  NrErrore by 1 to fiora0m0.NrMsg;
272500141015           IF  NrErrore > 0 and
272600141015               fiora0m0.skIdCampo(NrErrore) = 'VAONCL';
272700141015             reset fiora0m0;
272800141015             leavesr;
272900141015           ENDIF;
273000141015         ENDFOR;
273100141007
273200141007       ENDSR;
273300141014
273400141015       //--------------------------------------------------------------
273500160616       //?F08 = Ricerca dell'ordinante.
273600141015       //--------------------------------------------------------------
273700160615       BEGSR  TastoF08;
273800141015
273900141015         clear FIOR81DS;
274000160608         IF  fior000o.cor1 > 0;
274100160608           fior81ds.I81fil = fior000o.cor1;
274200141015         ENDIF;
274300141015         IF  fior000o.rso <> *blanks;
274400160608           fior81ds.I81rag = fior000o.rso;
274500141015         ENDIF;
274600141015         fior81r (kpjba:FIOR81DS);
274700141015       //?Se torna errore emetto messaggio a video
274800141015         IF  fior81ds.O81err <> *blanks;
274900160608           reset fiora0M0;
275000141015           wIdMsg   = 'TIS0718';
275100141015           wIdCampo = 'VAOCOR';
275200141015           wParm = fior81ds.O81msg;
275300141015           exsr Messaggi;
275400141015           leavesr;
275500141015         ENDIF;
275600141015       //?Se torna F12 non faccio niente
275700141015         IF  fior81ds.O81ret = '1';
275800141015           leavesr;
275900141015         ENDIF;
276000141015
276100141015       //?Se arrivo qua vuol dire che è stata fatta una scelta
276200141015       //?quindi imposto già i dati del codice scelto
276300160608         fior000o.cor1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
276400160608         fior000o.cor2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
276500160608         fior000o.cor3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
276600141015
276700160608       //?decodifico
276800160608         exsr Ordinante;
276900160608
277000160608       //?imposto i dati dalla DS di Output del FIOR000R
277100160608       //?alla ds di input del FIOR000R
277200160608         eval-corr fior000i = fior000o;
277300160608         reset fior000i.formato;
277400160608         reset fior000i.versione;
277500160608
277600160608       //?controllo
277700160608         exsr ControllaDatiVideo;
277800141015
277900141015       ENDSR;
278000141015
278100141015       //--------------------------------------------------------------
278200160616       //?F09 = Ricerca del destinatario.
278300141015       //--------------------------------------------------------------
278400160615       BEGSR  TastoF09;
278500141015
278600141015         clear FIOR81DS;
278700160608         IF  fior000o.crc1 > 0;
278800160608           fior81ds.I81fil = fior000o.crc1;
278900141015         ENDIF;
279000141015         IF  fior000o.rsc <> *blanks;
279100160608           fior81ds.I81rag = fior000o.rsc;
279200141015         ENDIF;
279300141015         fior81r (kpjba:FIOR81DS);
279400141015       //?Se torna errore emetto messaggio a video
279500141015         IF  fior81ds.O81err <> *blanks;
279600160608           reset fiora0M0;
279700141015           wIdMsg   = 'TIS0718';
279800141015           wIdCampo = 'VAOCRC';
279900141015           wParm = fior81ds.O81msg;
280000141015           exsr Messaggi;
280100141015           leavesr;
280200141015         ENDIF;
280300141015       //?Se torna F12 non faccio niente
280400141015         IF  fior81ds.O81ret = '1';
280500141015           leavesr;
280600141015         ENDIF;
280700141015
280800141015       //?Se arrivo qua vuol dire che è stata fatta una scelta
280900141015       //?quindi imposto già i dati del codice scelto
281000160608         fior000o.crc1 = %dec(%subst(%editc(fior81ds.O81cro:'X'):1:3):3:0);
281100160608         fior000o.crc2 = %dec(%subst(%editc(fior81ds.O81cro:'X'):4:4):4:0);
281200160608         fior000o.crc3 = %dec(%subst(%editc(fior81ds.O81cro:'X'):8:3):3:0);
281300141015
281400160608       //?decodifico
281500160608         exsr Destinatario;
281600160608
281700160608       //?imposto i dati dalla DS di Output del FIOR000R
281800160608       //?alla ds di input del FIOR000R
281900160608         eval-corr fior000i = fior000o;
282000160608         reset fior000i.formato;
282100160608         reset fior000i.versione;
282200160608
282300160608       //?controllo
282400160608         exsr ControllaDatiVideo;
282500141015
282600141015       ENDSR;
282700160616
282800160616       //--------------------------------------------------------------
282900160616       //?F10 = Copia Numero Telefono.
283000160616       //--------------------------------------------------------------
283100160616       BEGSR  TastoF10;
283200160616
283300160616         SELECT;
283400160616       //?F10 dato in videata F04
283500160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
283600160616           fior000i.sms = fior000i.ter;
283700160616       //?F10 dato in videata F13
283800160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
283900160616           fior000i.smsconf = fior000i.ter;
284000160616         ENDSL;
284100160616
284200160616       ENDSR;
284300160615
284400160615       //--------------------------------------------------------------
284500160616       //?F12 = Ritorno.
284600160615       //--------------------------------------------------------------
284700160615       BEGSR  TastoF12;
284800160616
284900160616         Finevideo = *on;
285000160615
285100160615         SELECT;
285200160615       //?F12 dato in videata principale
285300160616         WHEN fior000i.video = FIOR000_VIDEO_PRINCIPALE;
285400160616           Fine = *on;
285500170530           fior001o.tastoF12 = 'S';
285600160615       //?F12 dato in videata F05
285700160616         WHEN fior000i.video = FIOR000_VIDEO_ALTRIDATI;
285800160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
285900160615       //?reimposto i dati salvati
286000160615           fior000i.blc = savblc;
286100160615           fior000i.att = savatt;
286200160615           fior000i.mtc = savmtc;
286300160615           fior000i.sto = savsto;
286400160615           fior000i.cgi = savcgi;
286500160615           fior000i.ffd = savffd;
286600160615           reset fiora0M0;
286700160616       //?F12 dato in videata F04
286800160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCOMM;
286900160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
287000160616       //?reimposto i dati salvati
287100160616           fior000i.sms  = savsms;
287200160616           fior000i.mail = savmail;
287300160616           reset fiora0M0;
287400160616       //?F12 dato in videata F13
287500160616         WHEN fior000i.video = FIOR000_VIDEO_ALERTCONF;
287600160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
287700160616       //?reimposto i dati salvati
287800160616           fior000i.smsconf  = savsmsconf;
287900160616           fior000i.mailconf = savmailconf;
288000160616           reset fiora0M0;
288100160616       //?F12 dato in videata WHO
288200160616         WHEN fior000i.video = FIOR000_VIDEO_WHOISORD;
288300160616           fior000i.video = FIOR000_VIDEO_PRINCIPALE;
288400160616
288500160616         ENDSL;
288600160615
288700160615       ENDSR;
288800160616
288900160616       //--------------------------------------------------------------
289000160616       //?F13 = Alert Conferma Prenotazione.
289100160616       //--------------------------------------------------------------
289200160616       BEGSR  TastoF13;
289300160616
289400160616         fior000i.video = FIOR000_VIDEO_ALERTCONF;
289500160616         FineVideo = *on;
289600160616
289700160616       ENDSR;
289800160620
289900160620       //--------------------------------------------------------------
290000160620       //?F14 = Visualizza Orari Servizio.
290100160620       //--------------------------------------------------------------
290200160620       BEGSR  TastoF14;
290300160620
290400160620       //?Se non c'è la filiale ritiro e non ho peso/volume/bancali e non è
290500160620       //?un mittente codificato e siamo in immissione/copia
290600160620       //?non posso interrogare gli orari
290700160620         IF  fior000i.por = *zeros and fior000i.pkg = 0 and
290800160620             fior000i.vlm = 0 and fior000i.bnc = 0 and
290900160620             fior000i.cra1 = 0 and fior000i.cra2 = 0;
291000160620           wIdMsg   = 'TIS0718';
291100160620           wIdCampo = 'VAONCL';
291200160620           wParm = 'Per F14 serve la fil.ritiro e +
291300160620                    almeno 1 tra peso/vol./bancali';
291400160620           exsr Messaggi;
291500160620           leavesr;
291600160620         ENDIF;
291700160620
291800160620       //?Posso richiamare il pgm di int.orari di servizio solo se ho già
291900160620       //?la filiale di ritiro
292000160620         IF  fior000i.por = *zeros;
292100160620           wIdMsg   = 'TIS0718';
292200160620           wIdCampo = 'VAOPOR';
292300160620           wParm = 'Per interrogare gli orari serve la filiale ritiro';
292400160620           exsr Messaggi;
292500160620           leavesr;
292600160620         ENDIF;
292700160620
292800160620       //?l'indirizzo completo del mittente
292900160620         IF  fior000i.car = *blanks or fior000i.lor = *blanks;
293000160620           wIdMsg   = 'TIS0718';
293100160620           wIdCampo = 'VAOCAR';
293200160620           clear wIdCampo;
293300160620           wParm = 'Per interrogare gli orari serve l''indirizzo del +
293400160620                    mittente';
293500160620           exsr Messaggi;
293600160620           leavesr;
293700160620         ENDIF;
293800160620
293900160620       //?Cerco eventuale ora presunta ritiro
294000160620       //?solo se non è immissione ORM
294100160620       //?e solo se ORM in distinta e NON quadrato
294200160620         clear fior01ds;
294300160620         IF  prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_TEL and
294400160620             prmRqsOpCode <> FIOR001_RQSOPCODE_IMMISSIONE_MAIL and
294500170530             prmRqsOpCode <> FIOR001_RQSOPCODE_CONFERMA_FISSI and
294600160620             fior000i.ndc > 0 and fior000i.fao < 600;
294700160620           fior01ds.IOR01fgs = %dec(fior000i.por:3:0);
294800160620           fior01ds.IOR01ndc = fior000i.ndc;
294900160620           fior01ds.IOR01poe = fior000i.poe;
295000160620           fior01ds.IOR01nsr = fior000i.nsr;
295100160620           fior01ds.IOR01nor = fior000i.nor;
295200160620           fior01ds.IOR01nrv = fior000i.nrv;
295300160620           fior01r (fior01ds);
295400160620         ENDIF;
295500160621
295600160621         fior000i.orapresrit = fior01ds.OOR01ora;
295700160621         fior000i.pickupcalc = fiora0O0.pickupcalc;
295800160620
295900160620       ENDSR;
296000170505
296100170505       //--------------------------------------------------------------
296200170505       //?F15 = Interrogazione Cappario DPD.
296300170505       //--------------------------------------------------------------
296400170505       BEGSR  TastoF15;
296500170505
296600170505       //?Recupero alcuni dati dalla tabella delle nazioni
296700170505         k_TBLcod = '15';
296800170505         k_TBLkey = fior000i.nar;
296900170505         chain %kds(K03tabel) TABEL00F;
297000170505         IF  %found(TABEL00F) and TBLflg = *blanks;
297100170505           ds15 = TBLuni;
297200170505         ENDIF;
297300170505         clear TISIE8DS;
297400170505         %subst(tisie8ds.ISIE8cap:(ds15.§15pcf):
297500170505               (%len(fior000i.car) - ds15.§15pcf)) =
297600170505         %subst(fior000i.car:(ds15.§15pcf):
297700170505               (%len(fior000i.car) - ds15.§15pcf));
297800170505         %subst(tisie8ds.ISIE8cap:(ds15.§15pcf + ds15.§15ecf):
297900170505               (%len(fior000i.car) - ds15.§15ecf)) = *blanks;
298000170505         tisie8ds.ISIE8dri = dataoggi;
298100170505         tisie8ds.ISIE8op0 = 'D01';
298200170505         tisie8ds.ISIE8nar = fior000i.nar;
298300170505         tisie8r (kpjba:tisie8ds);
298400170505
298500170505       //?Se torna errore emetto messaggio a video
298600170505         IF  tisie8ds.OSIE8err <> *blanks;
298700170505           reset fiora0M0;
298800170505           wIdMsg   = 'TIS0718';
298900170505           wIdCampo = 'VAOCAR';
299000170505           wParm = tisie8ds.OSIE8msg;
299100170505           exsr Messaggi;
299200170505           leavesr;
299300170505         ENDIF;
299400170505
299500170505       //?Ritorno eventuale cap scelto
299600170505         IF  tisie8ds.OSIE8capb <> *blanks;
299700170505           fior000i.car = tisie8ds.OSIE8capb;
299800170505           reset fiora0M0;
299900170505           reset wOkF15;
300000170505         ENDIF;
300100170505
300200170505       ENDSR;
300300160614
300400160614       //--------------------------------------------------------------
300500160616       //?F18 = Gestione Note.
300600160614       //--------------------------------------------------------------
300700160615       BEGSR  TastoF18;
300800160614
300900160614         clear FIOR06DS;
301000160614         fior06ds.I06dta = fior000o.dao;
301100160614         fior06ds.I06poe = fior000i.poe;
301200160614         fior06ds.I06nor = fior000i.nor;
301300160614         fior06ds.I06nsr = fior000i.nsr;
301400160614         fior06ds.I06nrv = fior000i.nrv;
301500160614         fior06ds.I06far = fior000i.fao;
301600160614         fior06ds.I06dai = dataoggi;
301700160614         fior06ds.I06ori = %dec(%time());
301800160614
301900160614         SELECT;
302000160614         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
302100170530               prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
302200170530               prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
302300160614           fior06ds.I06flm = 'M';
302400170522       //?Devo passare la filiale emissione di input
302500170522       //?(può variare dalla filiale utente quando si cambia la fialie emissione nel chiamante)
302600170522           fior06ds.I06poe = fior000i.poei;
302700160614         WHEN  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
302800160614           fior06ds.I06flm = 'V';
302900160614         ENDSL;
303000160614
303100160614         kpjba = prmkpjba;
303200160614         fior06r (kpjba:FIOR06DS);
303300160614
303400160614       ENDSR;
303500160621
303600160621       //--------------------------------------------------------------
303700160621       //?F20 = Simulazione Delivery.
303800160621       //--------------------------------------------------------------
303900160621       BEGSR  TastoF20;
304000160621
304100160621         clear tisi92ds;
304200160621         tisi92ds.d92op0 = 'ORM';
304300160621         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
304400160621           tisi92ds.d92lnp = %dec(fior000i.por:3:0);
304500160621         ENDIF;
304600160621         tisi92ds.d92nar = fior000i.nac;
304700160621         tisi92ds.d92cad = fior000i.cac;
304800160621         tisi92ds.d92lod = fior000i.loc;
304900160621         tisi92ds.d92ffd = fior000i.ffd;
305000160621         tisi92ds.d92ksc = fior000i.ksc;
305100160621         IF  fior000i.ctr <> *blanks and fior000i.ctr > *zeros;
305200160621           tisi92ds.d92ctr = %dec(fior000i.ctr:3:0);
305300160621         ENDIF;
305400160621         tisi92ds.d92ncl = fior000i.ncl;
305500160621         tisi92ds.d92pkb = fior000i.pkg;
305600160621         tisi92ds.d92vlf = fior000i.vlm;
305700160621         tisi92ds.d92ddt = fior000i.ddt;
305800160621         kpjbu = tisi92ds;
305900160621         tisi92r (kpjba);
306000160621
306100160621       ENDSR;
306200160616
306300160616       //--------------------------------------------------------------
306400160616       //?F24 = Altri Tasti Funzione.
306500160616       //--------------------------------------------------------------
306600160616       BEGSR  TastoF24;
306700160616
306800160620         IF  nrighe = 1;
306900160620           leavesr;
307000160620         ENDIF;
307100160620
307200160620         SELECT;
307300160620         WHEN  (nrighe = 2 and contartf = 2) or
307400160620               (nrighe = 3 and contartf = 3);
307500160620           fior000a.rigatfmob = rigatf1;
307600160620           contartf = 1;
307700160620           IF  nrighe = 2;
307800160620             fior000a.rigatf24 = FIOR001_F24_1_2;
307900160620           ELSE;
308000160620             fior000a.rigatf24 = FIOR001_F24_1_3;
308100160620           ENDIF;
308200160620
308300160620         WHEN  (nrighe = 2 and contartf = 1) or
308400160620               (nrighe = 3 and contartf = 1);
308500160620           fior000a.rigatfmob = rigatf2;
308600160620           contartf = 2;
308700160620           IF  nrighe = 2;
308800160620             fior000a.rigatf24 = FIOR001_F24_2_2;
308900160620           ELSE;
309000160620             fior000a.rigatf24 = FIOR001_F24_2_3;
309100160620           ENDIF;
309200160620
309300160620         WHEN  nrighe = 3 and contartf = 2;
309400160620           fior000a.rigatfmob = rigatf3;
309500160620           contartf = 3;
309600160620           fior000a.rigatf24 = FIOR001_F24_3_3;
309700160620
309800160620         ENDSL;
309900160616
310000160616       ENDSR;
310100170330
310200170330       //--------------------------------------------------------------
310300170330       //?RollUp = Tolgo 1 GG alla data Ritiro.
310400170330       //--------------------------------------------------------------
310500170331       BEGSR  TastoRollUp;
310600170403
310700170403       //?Se la data ritiro non è ancora stata calcolata non posso fare UP/DOWN
310800170403         IF  fior000i.dar = 0;
310900170403           reset fiora0M0;
311000170403           wIdMsg   = 'TIS0718';
311100170403           clear wIdCampo;
311200170403           wParm = 'ATTENZIONE!! +
311300170403                    PageUp/PageDown non possibile senza data ritiro';
311400170403           exsr Messaggi;
311500170403           RiemetteVideo = *on;
311600170403           leavesr;
311700170403         ENDIF;
311800170403
311900170403         clear XGIOLAVDS;
312000170403         clear wlbdat;
312100170403         dataEUR = %date(fior000i.dar:*eur);
312200170403         dataISO = dataEUR;
312300170403         IXGLdata = %dec(dataISO);
312400170403         IXGLadd  = 'S';
312500170403         IXGLgg   = 1;
312600170403         IXGLlav  = 'S';
312700170403         IXGLpa   = 'P';
312800170403         IXGLfil  = %int(fior000i.por);
312900170403         XGIOLAV (xgiolavds);
313000170403         IF  OXGLerr <> *blanks;
313100170403           leavesr;
313200170403         ENDIF;
313300170404
313400170404       //?Solo se immissione
313500170404         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
313600170530             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
313700170530             prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
313800170404         //?Se data ritiro inferiore alla data ritiro calcolata errore
313900170404           IF  OXGldata < fiora0O0.dataritiro and
314000170404               OXGLdata < fiora0O0.dataritmin;
314100170404             wIdMsg   = 'TIS0718';
314200170404             wIdCampo = 'VAODAR';
314300170404             wParm = 'Data ritiro inferiore a quella calcolata.';
314400170404             exsr Messaggi;
314500170404             leavesr;
314600170404           ENDIF;
314700170404         ENDIF;
314800170404
314900170404       //?Supera la data max consentita
315000170404         IF  OXGLdata > fiora0O0.dataritmax;
315100170404           wIdMsg   = 'TIS0718';
315200170404           wIdCampo = 'VAODAR';
315300170404           wParm = 'Data ritiro superiore alla data max.';
315400170404           exsr Messaggi;
315500170404           leavesr;
315600170404         ENDIF;
315700170404
315800170404       //?Non può essere inferiore alla data immissione ORM
315900170404         IF  OXGLdata < fior001i.dao;
316000170404           wIdMsg   = 'TIS0718';
316100170404           wIdCampo = 'VAODAR';
316200170404           wParm = 'Data ritiro inferioe alla data immissione.';
316300170404           exsr Messaggi;
316400170404           leavesr;
316500170404         ENDIF;
316600170404
316700170403         dataISO = %date(OXGLdata:*iso);
316800170403         dataEUR = dataISO;
316900170403         fior000i.dar =  %dec(dataEUR);
317000170403
317100170403         fior000a.TastoVideo = FIOR000_TASTO_ENTER;
317200170403         exsr TastoEnter;
317300170330
317400170330       ENDSR;
317500170330
317600170330       //--------------------------------------------------------------
317700170330       //?RollDown = Aggiungo 1 GG alla data Ritiro.
317800170330       //--------------------------------------------------------------
317900170331       BEGSR  TastoRollDown;
318000170403
318100170403       //?Se la data ritiro non è ancora stata calcolata non posso fare UP/DOWN
318200170403         IF  fior000i.dar = 0;
318300170403           reset fiora0M0;
318400170403           wIdMsg   = 'TIS0718';
318500170403           clear wIdCampo;
318600170403           wParm = 'ATTENZIONE!! +
318700170403                    PageUp/PageDown non possibile senza data ritiro';
318800170403           exsr Messaggi;
318900170403           RiemetteVideo = *on;
319000170403           leavesr;
319100170403         ENDIF;
319200170403
319300170403         clear XGIOLAVDS;
319400170403         clear wlbdat;
319500170403         dataEUR = %date(fior000i.dar:*eur);
319600170403         dataISO = dataEUR;
319700170403         IXGLdata = %dec(dataISO);
319800170403         IXGLsub  = 'S';
319900170403         IXGLgg   = 1;
320000170403         IXGLlav  = 'S';
320100170403         IXGLpa   = 'P';
320200170403         IXGLfil  = %int(fior000i.por);
320300170403         XGIOLAV (xgiolavds);
320400170403         IF  OXGLerr <> *blanks;
320500170403           leavesr;
320600170403         ENDIF;
320700170404
320800170404       //?Solo se immissione
320900170404         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
321000170530             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
321100170530             prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
321200170404         //?Se data ritiro inferiore alla data ritiro calcolata errore
321300170404           IF  OXGldata < fiora0O0.dataritiro and
321400170404               OXGLdata < fiora0O0.dataritmin;
321500170404             wIdMsg   = 'TIS0718';
321600170404             wIdCampo = 'VAODAR';
321700170404             wParm = 'Data ritiro inferiore a quella calcolata.';
321800170404             exsr Messaggi;
321900170404             leavesr;
322000170404           ENDIF;
322100170404         ENDIF;
322200170404
322300170404       //?Supera la data max consentita
322400170404         IF  OXGLdata > fiora0O0.dataritmax;
322500170404           wIdMsg   = 'TIS0718';
322600170404           wIdCampo = 'VAODAR';
322700170404           wParm = 'Data ritiro superiore alla data max.';
322800170404           exsr Messaggi;
322900170404           leavesr;
323000170404         ENDIF;
323100170404
323200170404       //?Non può essere inferiore alla data immissione ORM
323300170404         IF  OXGLdata < fior001i.dao;
323400170404           wIdMsg   = 'TIS0718';
323500170404           wIdCampo = 'VAODAR';
323600170404           wParm = 'Data ritiro inferiore alla data immissione.';
323700170404           exsr Messaggi;
323800170404           leavesr;
323900170404         ENDIF;
324000170404
324100170403         dataISO = %date(OXGLdata:*iso);
324200170403         dataEUR = dataISO;
324300170403         fior000i.dar =  %dec(dataEUR);
324400170403
324500170403         fior000a.TastoVideo = FIOR000_TASTO_ENTER;
324600170403         exsr TastoEnter;
324700170330
324800170330       ENDSR;
324900160616
325000160616       //--------------------------------------------------------------
325100160616       //?ENTER = Controlli.
325200160616       //--------------------------------------------------------------
325300160616       BEGSR  ControllaDatiVideo;
325400170426
325500170426       //?Immissione ORM, videata principale, tasto enter o F6
325600170426       //?reimposto sempre la data e ora immissione ORM
325700170530         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
325800170530              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
325900170530              prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI) and
326000170426             fior000i.video = FIOR000_VIDEO_PRINCIPALE and
326100170426             (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
326200170426              fior000a.TastoVideo = FIOR000_TASTO_F06);
326300170426           dataISO = %date(dataoggi:*iso);
326400170426           dataEUR = dataISO;
326500170426           fior000i.dao = %dec(dataEUR);
326600170426           fior000i.oao = %dec(%time());
326700170426         ENDIF;
326800160616
326900160616       //?Imposto i dati dalla ds di input del video
327000160616       //?alla ds di input del pgm di controllo
327100160616         exsr MoveDsA0I;
327200160616
327300160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_MANUTENZIONE;
327400160616           RqsOpCode = FIORA00_RQSOPCODE_CONTROL;
327500160616         ENDIF;
327600160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
327700170530             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
327800160616           RqsOpCode = FIORA00_RQSOPCODE_WRITE;
327900160616         ENDIF;
328000170530         IF  prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
328100170530           RqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
328200170530         ENDIF;
328300160616
328400160616       //?Richiamo pgm di controllo dati
328500160616         MONITOR;
328600160616           FiorA00_Immetti( RqsOpCode
328700160616                          : RpyOpCode
328800160616                          : RpyIdMsg
328900160616                          : fiora0i0.formato
329000160616                          : fiora0i0
329100160616                          : %SIZE(fiora0i0)
329200160616                          : fiora0o0.formato
329300160616                          : fiora0o0
329400160616                          : %SIZE(fiora0o0)
329500160616                          : FIORA0M0.formato
329600160616                          : FIORA0M0
329700160616                          : %size(FIORA0M0)
329800160616                          : FIORA0F0.formato
329900160616                          : FIORA0F0
330000160616                          : %size(FIORA0F0)
330100160616                          );
330200160616           ON-ERROR;
330300160616           RpyOpCode = FIORA00_RPYOPCODE_ERROR;
330400160616         ENDMON;
330500160616
330600160616       //?Se torna errore emetto messaggio generico a video
330700160616         IF RpyOpCode < FIORA00_RPYOPCODE_DONE;
330800160616           IF  fiora0m0.nrmsg < 99;
330900160616             fiora0m0.nrmsg += 1;
331000160616             fiora0M0.skIdMsg(fiora0M0.nrmsg) = 'ORM*';
331100160616             fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'ORM*';
331200160616             fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
331300160616             fiora0M0.skTextMsg (fiora0M0.nrmsg)=
331400160616             'Grave errore nei controlli ORM';
331500170530             fior001o.errore = 'S';
331600160616             leavesr;
331700160616           ENDIF;
331800160616         //?Esco dal programma se ho più di 99 errori
331900160616           exsr RoutEnd;
332000160616         ENDIF;
332100160616
332200160616         //?Se arrivo qua vuol dire che i controlli sono andati bene
332300170419         //?quindi passo i dati della ds di Output del pgm di controllo
332400160616         //?nella ds di input del video
332500160616         exsr MoveDs000I;
332600160616
332700170419         //?A questo punto faccio ulteriori controlli in base alla videata
332800170419         //?e al tasto funzione
332900170419         SELECT;
333000160616         //?ENTER dato sulla videata principale
333100170419         //?o F6 dato sulla videata principale
333200170419         //?e sono in Immissione
333300170420         //?Se dai controlli torna un errore relativo ad una delle altre videate
333400170420         //?resetto tutti gli errori
333500170420         //?ed emetto errore di messaggi in sospeso su altre videate
333600160616         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE and
333700170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
333800170419                fior000a.TastoVideo = FIOR000_TASTO_F06) and
333900170419               (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
334000170530                prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
334100170530                prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI) and
334200170420               (%lookup('VAOBLC':fiora0M0.skIdCampo) > 0 or
334300170419                %lookup('VAOMTC':fiora0M0.skIdCampo) > 0 or
334400170419                %lookup('VAOATT':fiora0M0.skIdCampo) > 0 or
334500170419                %lookup('CGI':fiora0M0.skIdCampo) > 0);
334600170420           nrmsgblc = %lookup('VAOBLC':fiora0M0.skIdCampo);
334700170420           nrmsgmtc = %lookup('VAOMTC':fiora0M0.skIdCampo);
334800170420           nrmsgatt = %lookup('VAOATT':fiora0M0.skIdCampo);
334900170420           nrmsgcgi = %lookup('CGI':fiora0M0.skIdCampo);
335000170420           IF  nrmsgblc = 1 or nrmsgmtc = 1 or nrmsgatt = 1 or
335100170420               nrmsgcgi = 1;
335200170420             reset fiora0M0;
335300170420             clear wIdCampo;
335400170420             wIdMsg  = 'TIS0847';
335500170420             clear wParm;
335600170420             exsr Messaggi;
335700170420             RiemetteVideo = *on;
335800170420           ENDIF;
335900170419         //?Se ho dei messaggi da emettere non faccio altri controlli
336000170419         WHEN  fiora0M0.nrmsg > 0;
336100170419         //?ENTER dato sulla videata principale
336200170419         //?o F6 dato sulla videata principale
336300170419         //?in Immissione
336400170419         WHEN  fior000i.video = FIOR000_VIDEO_PRINCIPALE and
336500170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
336600170419                fior000a.TastoVideo = FIOR000_TASTO_F06) and
336700170419               (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
336800170530                prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
336900170530                prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI);
337000170419           //?Se è un ORM commissionato
337100170419           IF  fior000i.comm = 'S';
337200170419             //?se non c'è l'ordinante devo richiederlo
337300170419             IF  fior000i.cor1 = 0 and fior000i.cor2 = 0 and
337400170419               fior000i.rso = *blanks;
337500170419               fior000i.video = FIOR000_VIDEO_WHOISORD;
337600170420               FineVideo = *on;
337700170419               leavesr;
337800170419             ENDIF;
337900170419             //?se non sono presenti i dati per Alert ORM commissionato
338000170419             //?e posso fare Alert richiedo i dati
338100170420             //?e non faccio vedere la data ritiro e gli orari servizio
338200170420             //?quindi azzero la data ritiro
338300170419             IF  fior000i.sms = *blanks and fior000i.mail = *blanks and
338400170419                 not EmessoAlertComm;
338500170419               fior000i.video = FIOR000_VIDEO_ALERTCOMM;
338600170420               FineVideo = *on;
338700170420               clear fior000i.dar;
338800170420               clear fior000i.anticipa;
338900170419               leavesr;
339000170419             ENDIF;
339100170419           ENDIF;
339200170419           //?Se è un ORM senza mittente e/o ordinante codificati
339300170419           //?se non sono presenti i dati per Alert Conferma Prenotazione
339400170419           //?richiedo i dati
339500170419           //?se non li ho mai emessi
339600170419           IF  fior000i.cra1 = 0 and fior000i.cra2 = 0 and
339700170419               fior000i.cor1 = 0 and fior000i.cor2 = 0 and
339800170419               fior000i.smsconf = *blanks and
339900170419               fior000i.mailconf = *blanks and
340000170419               not EmessoAlertConf;
340100170419             fior000i.video = FIOR000_VIDEO_ALERTCONF;
340200170420             FineVideo = *on;
340300170419             leavesr;
340400170419           ENDIF;
340500160616
340600160616         //?ENTER dato sulla videata F05
340700170419         //?o F6 dato sulla videata F05
340800160616         WHEN  fior000i.video = FIOR000_VIDEO_ALTRIDATI and
340900170419               (fior000a.TastoVideo = FIOR000_TASTO_ENTER or
341000170419                fior000a.TastoVideo = FIOR000_TASTO_F06);
341100160616           //?se torna errore relativo ai campi a video lo emetto
341200160616           //?se no pulisco errori e resto nella videata
341300160616           IF  %lookup('VAOBLC':fiora0M0.skIdCampo) = 0 and
341400160616               %lookup('VAOATT':fiora0M0.skIdCampo) = 0 and
341500160616               %lookup('VAOMTC':fiora0M0.skIdCampo) = 0 and
341600160616               %lookup('CGI':fiora0M0.skIdCampo) = 0;
341700160616             reset fiora0M0;
341800160616           ENDIF;
341900160616
342000160616         ENDSL;
342100160616
342200160616       ENDSR;
342300160616
342400160616       //--------------------------------------------------------------
342500160616       //?Controllo i dati del Video Who.
342600160616       //--------------------------------------------------------------
342700160616       BEGSR  ControllaWho;
342800160616
342900160616         SELECT;
343000160616       //?Non è stato immesso nessun tipo ordinante
343100160616         WHEN  fior000i.tipoord = *blanks;
343200160616
343300160616       //?Immesso mittente come tipo ordinante
343400160616       //?imposto i dati del mittente nell'ordinante
343500160616         WHEN  fior000i.tipoord = 'M';
343600160616           fior000i.cor1 = fior000i.cra1;
343700160616           fior000i.cor2 = fior000i.cra2;
343800160616           fior000i.cor3 = fior000i.cra3;
343900160616           fior000i.rso  = fior000i.rsr;
344000160616           fior000i.ino  = fior000i.inr;
344100160616           fior000i.loo  = fior000i.lor;
344200160616           fior000i.cao  = fior000i.car;
344300160616           fior000i.pro  = fior000i.prr;
344400160616           fior000i.nao  = fior000i.nar;
344500160616           fior000i.comm = 'N';
344600160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
344700160616           FineVideo = *on;
344800160616
344900160616       //?Immesso destinatario come tipo ordinante
345000160616       //?imposto i dati del destinatario nell'ordinante
345100160616         WHEN  fior000i.tipoord = 'D';
345200160616           fior000i.cor1 = fior000i.crc1;
345300160616           fior000i.cor2 = fior000i.crc2;
345400160616           fior000i.cor3 = fior000i.crc3;
345500160616           fior000i.rso  = fior000i.rsc;
345600160616           fior000i.ino  = fior000i.inc;
345700160616           fior000i.loo  = fior000i.loc;
345800160616           fior000i.cao  = fior000i.cac;
345900160616           fior000i.pro  = fior000i.prc;
346000160616           fior000i.nao  = fior000i.nac;
346100160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
346200160616           FineVideo = *on;
346300160616
346400160616       //?Immesso altro come tipo ordinante
346500160616       //?imposto a mano i dati dell'ordinante
346600160616         WHEN  fior000i.tipoord = 'A';
346700160616           fior000i.Video = FIOR000_VIDEO_PRINCIPALE;
346800160616           FineVideo = *on;
346900160616
347000160616         ENDSL;
347100160616
347200160616       ENDSR;
347300170414
347400170414       //--------------------------------------------------------------
347500170414       //?Controllo se devo riemettere le videata degli alert ORM.
347600170414       //--------------------------------------------------------------
347700170414       BEGSR  AlertORM;
347800170414
347900170414       //?Se ORM commissionato ed ho emesso la videata
348000170414       //?relativa all'alert ORM COMMISSIONATO (F04)
348100170414       //?ma non ho inserito nessun dato e non ho mai dato F6 sulla videata
348200170418       //?oppure se ORM commissionato e non ho mai emesso la videata e non
348300170418       //?ci sono i dati
348400170414       //?ripropongo la win prima della scrittura dei file
348500170418         IF  fior000i.comm = 'S' and
348600170420             EmessoAlertComm and not wOKF06F04 and
348700170418             fior000i.sms = *blanks and fior000i.mail = *blanks;
348800170414           fior000i.video = FIOR000_VIDEO_ALERTCOMM;
348900170414           FineVideo = *on;
349000170414           leavesr;
349100170414         ENDIF;
349200170420       //?Se ORM commissionato ed ho emesso la videata
349300170420       //?relativa all'alert ORM COMMISSIONATO (F04)
349400170420       //?ho inserito i dati e ho dato F6 sulla videata
349500170420       //?imposto il flag per scrittura file Alert
349600170420         IF  fior000i.comm = 'S' and
349700170420             EmessoAlertComm and
349800170420             (fior000i.sms <> *blanks or fior000i.mail <> *blanks);
349900170420           IF  fior000i.sms <> *blanks;
350000170420             fior000i.alertsms = 'S';
350100170420           ENDIF;
350200170420           IF  fior000i.mail <> *blanks;
350300170420             fior000i.alertmail = 'S';
350400170420           ENDIF;
350500170420         ENDIF;
350600170414
350700170414       //?Se ho emesso la videata relativa alla CONFERMA PRENOTAZIONE RITIRO (F13)
350800170414       //?ma non ho inserito nessun dato e non ho mai dato F6 sulla videata
350900170414       //?ripropongo la win prima della scrittura dei file
351000170420         IF  EmessoAlertConf and not wOKF06F13 and
351100170418             fior000i.smsconf = *blanks and fior000i.mailconf = *blanks;
351200170414           fior000i.video = FIOR000_VIDEO_ALERTCONF;
351300170414           FineVideo = *on;
351400170414           leavesr;
351500170414         ENDIF;
351600170420       //?Se ho emesso la videata relativa alla CONFERMA PRENOTAZIONE RITIRO (F13)
351700170420       //?ed ho inserito i dati e ho fatto F6 sulla videata
351800170420       //?imposto il flag per scrittura file Alert
351900170420         IF  EmessoAlertConf and
352000170420             (fior000i.smsconf <> *blanks or fior000i.mailconf <> *blanks);
352100170427           IF  fior000i.smsconf <> *blanks;
352200170427             fior000i.alertcsms = 'S';
352300170427           ENDIF;
352400170427           IF  fior000i.mailconf <> *blanks;
352500170427             fior000i.alertcmail = 'S';
352600170427           ENDIF;
352700170420         ENDIF;
352800170414
352900170414       ENDSR;
353000160622
353100160622       //--------------------------------------------------------------
353200160622       //?Scrivo l'ORM.
353300160622       //--------------------------------------------------------------
353400160622       BEGSR  ScriviFileORM;
353500160622
353600160622       //?Imposto i dati dalla ds di input del video
353700160622       //?alle ds del pgm di scrittura
353800160622         exsr MoveDsA0I;
353900160622         exsr MoveDsA01;
354000160622
354100160622         RqsOpCode = FIORA00_RQSOPCODE_WRITE;
354200160622
354300160622       //?Richiamo pgm di scrittura file
354400160622         MONITOR;
354500160622           FiorA01_Scrivi( RqsOpCode
354600160622                         : RpyOpCode
354700160622                         : RpyIdMsg
354800160622                         : fiora0i0.formato
354900160622                         : fiora0i0
355000160622                         : %SIZE(fiora0i0)
355100170420                         : fiora01i.formato
355200170420                         : fiora01i
355300170420                         : %SIZE(fiora01i)
355400170420                         : fiora01o.formato
355500170420                         : fiora01o
355600170420                         : %SIZE(fiora01o)
355700170420                         : prmkpjba
355800170420                         );
355900160622           ON-ERROR;
356000160622           RpyOpCode = FIORA00_RPYOPCODE_ERROR;
356100160622         ENDMON;
356200160622
356300160622       //?Se torna errore emetto messaggio generico a video
356400160622         IF RpyOpCode < FIORA00_RPYOPCODE_DONE;
356500160622           IF  fiora0m0.nrmsg < 99;
356600160622             fiora0m0.nrmsg += 1;
356700160622             fiora0M0.skIdMsg(fiora0M0.nrmsg) = 'ORM*';
356800160622             fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'ORM*';
356900160622             fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
357000160622             fiora0M0.skTextMsg (fiora0M0.nrmsg)=
357100160622             'Grave errore in scrittura ORM';
357200170530             fior001o.errore = 'S';
357300160622             leavesr;
357400160622           ENDIF;
357500160622         //?Esco dal programma se ho più di 99 errori
357600160622           exsr RoutEnd;
357700160622         ENDIF;
357800160622
357900160622       ENDSR;
358000160616
358100160616       //--------------------------------------------------------------
358200160616       //?Imposto i campi nella DS di input. (pgm FIORA00R)
358300160616       //--------------------------------------------------------------
358400160616       BEGSR  MoveDsA0I;
358500160616
358600160616         reset FIORA0I0;
358700160616         fiora0i0.VAOpoe = fior000i.poe;
358800170522       //?Se immissione manuale passo la filiale emissione di input
358900170522         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
359000170522             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
359100170522           fiora0i0.VAOpoe = fior000i.poei;
359200170522         ENDIF;
359300160616         dataEUR = %date(fior000i.dao:*eur);
359400160616         dataISO = dataEUR;
359500160616         fiora0i0.VAOdao = %dec(dataISO);
359600160616         fiora0i0.VAOoao = fior000i.oao;
359700160616         fiora0i0.VAOcor =  (fior000i.cor1 * 10000000);
359800160616         fiora0i0.VAOcor += (fior000i.cor2 * 1000);
359900160616         fiora0i0.VAOcor += (fior000i.cor3);
360000160616         fiora0i0.VAOrso = fior000i.rso;
360100160616         fiora0i0.VAOino = fior000i.ino;
360200160616         fiora0i0.VAOcao = fior000i.cao;
360300160616         fiora0i0.VAOloo = fior000i.loo;
360400160616         fiora0i0.VAOpro = fior000i.pro;
360500160616         fiora0i0.VAOnao = fior000i.nao;
360600160616         fiora0i0.VAOcra =  (fior000i.cra1 * 10000000);
360700160616         fiora0i0.VAOcra += (fior000i.cra2 * 1000);
360800160616         fiora0i0.VAOcra += (fior000i.cra3);
360900160616         fiora0i0.VAOrsr = fior000i.rsr;
361000160616         fiora0i0.VAOinr = fior000i.inr;
361100160616         fiora0i0.VAOcar = fior000i.car;
361200160616         fiora0i0.VAOlor = fior000i.lor;
361300160616         fiora0i0.VAOprr = fior000i.prr;
361400160616         fiora0i0.VAOnar = fior000i.nar;
361500160616         fiora0i0.VAOcrc =  (fior000i.crc1 * 10000000);
361600160616         fiora0i0.VAOcrc += (fior000i.crc2 * 1000);
361700160616         fiora0i0.VAOcrc += (fior000i.crc3);
361800160616         fiora0i0.VAOrsc = fior000i.rsc;
361900160616         fiora0i0.VAOinc = fior000i.inc;
362000160616         fiora0i0.VAOcac = fior000i.cac;
362100160616         fiora0i0.VAOloc = fior000i.loc;
362200160616         fiora0i0.VAOprc = fior000i.prc;
362300160616         fiora0i0.VAOnac = fior000i.nac;
362400160616         fiora0i0.VAOpag = fior000i.pag;
362500160616         fiora0i0.VAOrer = fior000i.rer;
362600160616         fiora0i0.VAOter = fior000i.ter;
362700160616         fiora0i0.VAOffd = fior000i.ffd;
362800170330         IF  fior000i.dpm > 0;
362900170330           dataEUR = %date(fior000i.dpm:*eur);
363000160616           dataISO = dataEUR;
363100170404           fiora0i0.datapm = %dec(dataISO);
363200160616         ENDIF;
363300170404         IF  fior000i.dar > 0;
363400170404           dataEUR = %date(fior000i.dar:*eur);
363500170404           dataISO = dataEUR;
363600170404           fiora0i0.VAOdar = %dec(dataISO);
363700170404         ENDIF;
363800160616         fiora0i0.VAOorr = fior000i.orr;
363900160616         fiora0i0.VAOncl = fior000i.ncl;
364000160616         fiora0i0.VAOpkg = fior000i.pkg;
364100160616         fiora0i0.VAOvlm = fior000i.vlm;
364200160616         fiora0i0.VAObnc = fior000i.bnc;
364300160616         fiora0i0.VAOblc = fior000i.blc;
364400160616         fiora0i0.VAOatt = fior000i.att;
364500160616         fiora0i0.VAOmtc = fior000i.mtc;
364600160616         fiora0i0.VAOnam = fior000i.nam;
364700160616         fiora0i0.VAOrfa = fior000i.rfa;
364800160616         fiora0i0.VAOno1 = fior000i.nota1;
364900160616         fiora0i0.VAOno2 = fior000i.nota2;
365000160616         fiora0i0.VAOksc = fior000i.ksc;
365100160616         fiora0i0.VAOctr = fior000i.ctr;
365200160616         IF  fior000i.por <> *blanks and fior000i.por > *zeros;
365300160616           fiora0i0.VAOpor = %int(fior000i.por);
365400160616         ELSE;
365500160616           clear fiora0i0.VAOpor;
365600160616         ENDIF;
365700160616         IF  fior000i.poc <> *blanks and fior000i.poc > *zeros;
365800160616           fiora0i0.VAOpoc = %int(fior000i.poc);
365900160616         ELSE;
366000160616           clear fiora0i0.VAOpoc;
366100160616         ENDIF;
366200160616         fiora0i0.VAOsto = fior000i.sto;
366300160616         fiora0i0.VAOspi = fior000i.spi;
366400160616         fiora0i0.SceltaNTW = fior000i.NTWDPD;
366500160616         fiora0i0.DalleAm = fior000i.DalleAm;
366600160616         fiora0i0.AlleAm  = fior000i.AlleAm;
366700160616         fiora0i0.DallePm = fior000i.DallePm;
366800160616         fiora0i0.AllePm  = fior000i.AllePm;
366900160616         fiora0i0.Contattot = fior000i.comm;
367000170420         fiora0i0.AlertMail = fior000i.AlertMail;
367100170420         fiora0i0.AlertSms = fior000i.AlertSms;
367200160616         fiora0i0.Mail = fior000i.Mail;
367300160616         fiora0i0.Sms  = fior000i.Sms;
367400160616         fiora0i0.fgs = fior000i.poei;
367500160616         fiora0i0.tor = fior000i.tor;
367600170419         fiora0i0.tgi = fior000i.tgi;
367700160616         fiora0i0.cgi = fior000i.cgi;
367800160616         fiora0i0.tco = fior000i.tco;
367900170427         IF  fior000i.AlertcMail = 'S';
368000170427           fiora0i0.AlertConf = fior000i.AlertcMail;
368100170427         ENDIF;
368200170427         IF  fior000i.AlertcSms = 'S';
368300170427           fiora0i0.AlertConf = fior000i.AlertcSms;
368400170427         ENDIF;
368500170420         fiora0i0.smsconf = fior000i.smsconf;
368600160620         fiora0i0.mailconf = fior000i.mailconf;
368700170420         fiora0i0.peso = fior000i.peso;
368800170420         fiora0i0.volume = fior000i.volume;
368900160616
369000160616       //?Se immissione
369100160616         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
369200170530             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
369300170530             prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI;
369400160616
369500160616       //?Imposto i dati del FLO
369600160616           exsr DatiFLO;
369700160616
369800160616         ENDIF;
369900160616
370000160616       ENDSR;
370100160616
370200160616       //--------------------------------------------------------------
370300160616       //?Imposto i campi nella DS di input. (FIOR000R)
370400160616       //--------------------------------------------------------------
370500160616       BEGSR  MoveDs000I;
370600160616
370700160616         fior000i.rsr = fiora0o0.VAOrsr;
370800160616         fior000i.inr = fiora0o0.VAOinr;
370900160616         fior000i.car = fiora0o0.VAOcar;
371000160616         fior000i.lor = fiora0o0.VAOlor;
371100160616         fior000i.prr = fiora0o0.VAOprr;
371200160616         fior000i.nar = fiora0o0.VAOnar;
371300160616         fior000i.cor1 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):1:3):3:0);
371400160616         fior000i.cor2 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):4:4):4:0);
371500160616         fior000i.cor3 = %dec(%subst(%editc(fiora0o0.VAOcor:'X'):8:3):3:0);
371600160616         fior000i.rso = fiora0o0.VAOrso;
371700160616         fior000i.ino = fiora0o0.VAOino;
371800160616         fior000i.cao = fiora0o0.VAOcao;
371900160616         fior000i.loo = fiora0o0.VAOloo;
372000160616         fior000i.pro = fiora0o0.VAOpro;
372100160616         fior000i.nao = fiora0o0.VAOnao;
372200160616         fior000i.rsc = fiora0o0.VAOrsc;
372300160616         fior000i.inc = fiora0o0.VAOinc;
372400160616         fior000i.cac = fiora0o0.VAOcac;
372500160616         fior000i.loc = fiora0o0.VAOloc;
372600160616         fior000i.prc = fiora0o0.VAOprc;
372700160616         fior000i.nac = fiora0o0.VAOnac;
372800160616         fior000i.rer = fiora0o0.VAOrer;
372900160616         fior000i.ter = fiora0o0.VAOter;
373000160616         fior000i.anticipa = fiora0o0.anticipa;
373100170404         dataISO = %date(fiora0o0.datapm:*iso);
373200170404         dataEUR = dataISO;
373300170404         fior000i.dpm = %dec(dataEUR);
373400170404         IF  fiora0o0.VAOdar > 0;
373500160616           dataISO = %date(fiora0o0.VAOdar:*iso);
373600160616           dataEUR = dataISO;
373700160616           fior000i.dar = %dec(dataEUR);
373800160616         ELSE;
373900160616           clear fior000i.dar;
374000160616           clear fior000i.anticipa;
374100160616         ENDIF;
374200160616         fior000i.orr = fiora0o0.VAOorr;
374300160616         fior000i.nam = fiora0o0.VAOnam;
374400160616         fior000i.rfa = fiora0o0.VAOrfa;
374500160616         fior000i.nota1 = fiora0o0.VAOno1;
374600160616         fior000i.nota2 = fiora0o0.VAOno2;
374700160616         fior000i.ksc = fiora0o0.VAOksc;
374800160616         fior000i.ctr = fiora0o0.VAOctr;
374900160616         fior000i.ntwdpd = fiora0o0.sceltantw;
375000160616         fior000i.por = %editc(fiora0o0.VAOpor:'X');
375100170420         fior000i.AlertMail = fiora0o0.AlertMail;
375200170420         fior000i.AlertSms = fiora0o0.AlertSms;
375300160616         fior000i.Mail = fiora0o0.Mail;
375400160616         fior000i.Sms  = fiora0o0.Sms;
375500160616         fior000i.orainizio = fiora0o0.orainizio;
375600160616         fior000i.orafine = fiora0o0.orafine;
375700170426         fior000i.oraestesa = fiora0o0.oraestesa;
375800160616         fior000i.comm = fiora0O0.Contattot;
375900160616         fior000i.spi = fiora0o0.VAOspi;
376000170419         fior000i.tgi = fiora0o0.tgi;
376100160616         fior000i.cgi = fiora0o0.cgi;
376200170427         fior000i.AlertcMail = fiora0o0.AlertConf;
376300170427         fior000i.AlertcSms = fiora0o0.AlertConfs;
376400170331         fior000i.Mailconf = fiora0o0.Mailconf;
376500170331         fior000i.Smsconf  = fiora0o0.Smsconf;
376600170420         fior000i.Peso = fiora0o0.Peso;
376700170420         fior000i.Volume = fiora0o0.Volume;
376800160616
376900160616         dORM01 = fiora0o0.VAOflo;
377000160616
377100160616       //?Se immissione
377200160616       //?e Ordinante = Mittente e paga Ordinante in automatico
377300160616       //?imposto che paga il mittente
377400160616         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
377500170530              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL or
377600170530              prmRqsOpCode = FIOR001_RQSOPCODE_CONFERMA_FISSI) and
377700160616             fior000i.pag = FIORA00_PAGA_ORDINANTE and
377800160616             fior000i.cra1 > 0 and fior000i.cra2 > 0 and
377900160616             fior000i.cor1 = fior000i.cra1 and
378000160616             fior000i.cor2 = fior000i.cra2 and
378100160616             fior000i.cor3 = fior000i.cra3;
378200160616           fior000i.pag = FIORA00_PAGA_MITTENTE;
378300160616         ENDIF;
378400160616
378500160616       //?Se immissione
378600160616       //?e dal pgm dei controlli torna errore sulle quantità
378700160616       //?non devo far vedere la data ritiro
378800160616         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
378900160616              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
379000160616             (%lookup('VAONCL':fiora0M0.skIdCampo) > 0 or
379100160616              %lookup('VAOPKG':fiora0M0.skIdCampo) > 0 or
379200160616              %lookup('VAOVLM':fiora0M0.skIdCampo) > 0 or
379300160616              %lookup('VAOBNC':fiora0M0.skIdCampo) > 0);
379400160616           clear fior000i.dar;
379500160616           clear fior000i.anticipa;
379600160616         ENDIF;
379700170505
379800170505       //?Se immissione manuale
379900170505       //?e i flag per invio Alert Conferma Prenotazione Ritiro
380000170505       //?sono tutti e 2 a 'N' li pulisco in modo da non generare
380100170505       //?inutilmente il rcd 'G' in scrittura
380200170505         IF  (prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
380300170505              prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL) and
380400170505              fior000i.Mailconf = *blanks and fior000i.Smsconf = *blanks;
380500170505           clear fior000i.AlertcMail;
380600170505           clear fior000i.AlertcSms;
380700170505         ENDIF;
380800160616
380900160616       //?Se dal pgm di controllo torna errore sul cap DPD attivo F15
381000160616         IF  %lookup('TIS0811':fiora0M0.skIdMsg) > 0;
381100160616           wOkF15 = *on;
381200160616         ELSE;
381300160616           wOkF15 = *off;
381400160616         ENDIF;
381500160616
381600160616       ENDSR;
381700160622
381800160622       //--------------------------------------------------------------
381900160622       //?Imposto i campi nella DS di input. (pgm FIORA01R)
382000160622       //--------------------------------------------------------------
382100160622       BEGSR  MoveDsA01;
382200160622
382300170420         reset fiora01I;
382400170420         fiora01I.utente = fior001i.utente;
382500170420         fiora01I.filute = fior001i.filute;
382600170420         fiora01I.orainizio = fiora0O0.orainizio;
382700170426         fiora01I.orafine = fiora0O0.orafine;
382800170505       //?Se immissione manuale
382900170505         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
383000170505             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
383100170505         //?Se l'ora pronta merce è maggiore dell'ora inizio servizio
383200170505         //?e minore dell'ora fine e la data ritiro = data pronta merce
383300170505         //?imposto ora inizio servizio = ora pronta merce
383400170505           IF  fiora0O0.vaoorr > fiora0O0.orainizio and
383500170505               fiora0O0.vaoorr < fiora0O0.orafine and
383600170505               fiora0O0.vaodar = fiora0i0.datapm;
383700170505             fiora01i.orainizio = fiora0O0.vaoorr;
383800170505           ENDIF;
383900170505         //?Se l'ora pronta merce è maggiore dell'ora fine servizio
384000170505         //?e la data ritiro = data pronta merce
384100170505         //?imposto ora fine servizio = ora pronta merce
384200170505           IF  fiora0O0.vaoorr > fiora0O0.orafine and
384300170505               fiora0O0.vaodar = fiora0i0.datapm;
384400170505             fiora01i.orafine = fiora0O0.vaoorr;
384500170505           ENDIF;
384600170505         ENDIF;
384700170426         fiora01I.oraestesa = fiora0O0.oraestesa;
384800170420         fiora01I.oratappo = ddft.d§DFTorrtp;
384900170420         fiora01I.oracutoff = fiora0O0.oranocomm;
385000170420         fiora01I.ddt = fior000i.ddt;
385100170414         IF  wOKF06F04;
385200170420           fiora01I.okf06f04 = 'S';
385300170414         ENDIF;
385400170414         IF  wOKF06F13;
385500170420           fiora01I.okf06f13 = 'S';
385600170414         ENDIF;
385700170505         fiora01i.alertcmail = fior000i.alertcmail;
385800170505         fiora01i.alertcsms = fior000i.alertcsms;
385900170505
386000170505       //?Se immissione manuale e i dati per Alert Conferma Prenotazione Ritiro
386100170505       //?non sono presenti NON imposto niente nei relativi flag a questo modo
386200170505       //?non viene scritto il rcd 'G', sarebbe inutile
386300170505         IF  prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_TEL or
386400170505             prmRqsOpCode = FIOR001_RQSOPCODE_IMMISSIONE_MAIL;
386500170505           IF  fiora0i0.mailconf = *blanks and fiora0i0.smsconf = *blanks;
386600170505             clear fiora01i.alertcmail;
386700170505             clear fiora01i.alertcsms;
386800170505           ENDIF;
386900170505         ENDIF;
387000170526
387100170526       //?Dati necessari per memorizzare data ritiro calcolata, se posticipo,
387200170526       //?se anticipo, se anticipato e di quanto anticipato
387300170526         fiora01I.dataritiro = fiora0O0.dataritiro;
387400170526         fiora01I.dataritmin = fiora0O0.dataritmin;
387500170526         fiora01I.posticipo  = fiora0O0.posticipo;
387600160622
387700160622       ENDSR;
387800141017
387900141017       //--------------------------------------------------------------
388000141017       //?Recupero i dati per il campo FLO.
388100141017       //--------------------------------------------------------------
388200141017       BEGSR  DatiFLO;
388300141017
388400141017         wDatiFLO = *off;
388500141017         clear dPVO;
388600141017
388700141017       //?Imposto il codice Ordinante
388800160608         wCodice  = (fior000o.cor1 * 10000000);
388900160608         wCodice += (fior000o.cor2 * 1000);
389000160608         wCodice += (fior000o.cor3);
389100141017
389200141017       //?Prima provo a cercare con il codice ordinante lungo 10
389300141017         IF  wCodice > 0;
389400141017           keytabella = %editc(wCodice:'X');
389500141017           IF  getTabella (c_tntbe:'PVO':keytabella:'O':
389600141017                           *blanks:*blanks:
389700141017                           *omit:*omit:*omit:*omit:*omit:
389800141017                           *omit:*omit:*omit:*omit:*omit:
389900141017                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
390000141017               = *zeros and ds_TNTBE.TBEatb = *blanks;
390100141017             dPVO = ds_TNTBE.TBEuni;
390200141017             wDatiFLO = *on;
390300141017           ENDIF;
390400141017
390500141017       //?Se non trovo con Ordinante lungo 10
390600141017       //?provo a cercare con il codice Ordinante lungo 7
390700141017           IF  not wDatiFLO;
390800141017             keytabella = %editc(wCodice:'X');
390900141017             %subst(keytabella:8:3) = *blanks;
391000141017             IF  getTabella (c_tntbe:'PVO':keytabella:'O':
391100141017                           *blanks:*blanks:
391200141017                             *omit:*omit:*omit:*omit:*omit:
391300141017                             *omit:*omit:*omit:*omit:*omit:
391400141017                             ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
391500141017                 = *zeros and ds_TNTBE.TBEatb = *blanks;
391600141017               dPVO = ds_TNTBE.TBEuni;
391700141017               wDatiFLO = *on;
391800141017             ENDIF;
391900141017           ENDIF;
392000141017         ENDIF;
392100141017
392200141017       //?Se non trovo con Ordinante lungo 10 e lungo 7
392300141017       //?provo a cercare con il codice KSC
392400141017         IF  not wDatiflo;
392500141017           keytabella = %editc(fiora0i0.vaoksc:'X');
392600141017           IF  getTabella (c_tntbe:'PVO':keytabella:'K':
392700141017                           *blanks:*blanks:
392800141017                           *omit:*omit:*omit:*omit:*omit:
392900141017                           *omit:*omit:*omit:*omit:*omit:
393000141017                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
393100141017               = *zeros and ds_TNTBE.TBEatb = *blanks;
393200141017             dPVO = ds_TNTBE.TBEuni;
393300141017             wDatiFLO = *on;
393400141017           ENDIF;
393500141017         ENDIF;
393600141017
393700141017       //?Se non trovo con nessun codice vado via
393800141017         IF  not wDatiFLO;
393900141017           leavesr;
394000141017         ENDIF;
394100141017
394200141017       //?Imposto i dati del FLO
394300141017         IF  dpvo.d§PVOcb = 'S';
394400141017           dorm01.§ORMcb = dpvo.d§PVOcb;
394500141017         ENDIF;
394600141017         IF  dpvo.d§PVOfr = 'S';
394700141017           dorm01.§ORMfr = dpvo.d§PVOfr;
394800141017         ENDIF;
394900141017         IF  dpvo.d§PVOks = 'S';
395000141017           dorm01.§ORMks = dpvo.d§PVOks;
395100141017         ENDIF;
395200141017         IF  dpvo.d§PVOfd = 'S';
395300141017           dorm01.§ORMfd = dpvo.d§PVOfd;
395400141017         ENDIF;
395500141017         IF  dpvo.d§PVOic <> *blanks;
395600141017           dorm01.§ORMic = dpvo.d§PVOic;
395700141017         ENDIF;
395800141017
395900141017         //?Se ORM con ricevuta è sempre ORM commissionato
396000141017         //?per questo devo anche ricalcolare la data di ritiro
396100141017         IF  dpvo.d§PVOsrm = 'S';
396200141017           dorm01.§ORsrm = dpvo.d§PVOsrm;
396300141017           dorm01.§ORcom = 'S';
396400141017           clear fiora0i0.VAOdar;
396500141022           fiora0i0.contattot = dorm01.§ORcom;
396600141017         ENDIF;
396700141017
396800141017         //?Se ORM con preavviso via MAIL (CEVA) e sempre ORM NON commissionato
396900141017         IF  dpvo.d§PVOpre = 'M';
397000141017           dorm01.§ORMpre = dpvo.d§PVOpre;
397100141017           dorm01.§ORcom = 'N';
397200141022           fiora0i0.contattot = dorm01.§ORcom;
397300141017         ENDIF;
397400141022
397500141022         //?Commissionato da personalizzazione del cliente
397600141022         IF  fiora0i0.contattot = *blanks;
397700141022           fiora0I0.contattot = dpvo.d§PVOcomc;
397800141022         ENDIF;
397900141017
398000141017         fiora0i0.VAOflo = dORM01;
398100141017
398200141017       ENDSR;
398300141009
398400141009       //--------------------------------------------------------------
398500141009       //?Richiamo Interrogazione cappario.
398600141009       //--------------------------------------------------------------
398700141009       BEGSR  call_FNLV14R;
398800141009
398900141009         clear FNLV14DS;
399000141009         fnlv14ds.I14dri = dataoggi;
399100141021         fnlv14ds.I14rsc = wRagSoc;
399200141009         fnlv14ds.I14ind = wIndirizzo;
399300141009         fnlv14ds.E14cap = wCap;
399400141009         fnlv14ds.E14loc = wLocalita;
399500141009         fnlv14ds.E14prv = wProvincia;
399600141009         fnlv14ds.E14nar = wNazione;
399700141009
399800141009         fnlv14r (kpjba:fnlv14ds);
399900141009
400000141009       ENDSR;
400100141021
400200141021       //--------------------------------------------------------------
400300141021       //?Routine per schierare le forzature nella ds FIORA0F0.
400400141021       //--------------------------------------------------------------
400500141021       BEGSR  Forzatura;
400600141021
400700160531       //?Se già caricato in sk non lo ricarico
400800160531         wNrForza = 1;
400900160531         FOR  wNrForza by 1 to fiora0F0.NrForza;
401000160531           IF  wNrForza > 0 and
401100160531               fiora0F0.skIdMsg(wNrForza) = wIdMsg and
401200160531               fiora0F0.skIdCampo(wNrForza) = wIdCampo and
401300160531               fiora0F0.skGiaForza(wNrForza) =
401400160531               FIORA00_GIAFORZA_SI;
401500160531             leavesr;
401600160531           ENDIF;
401700160531         ENDFOR;
401800160531
401900160531       //?Se ho già caricato 99 mesaggi esco
402000160531         IF  fiora0M0.nrmsg = 99;
402100160531           exsr RoutEnd;
402200160531           clear fiora0M0.nrmsg;
402300160531         ENDIF;
402400160531
402500160531       //?Imposto messaggio da forzare
402600160531         fiora0M0.nrmsg += 1;
402700160531         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
402800160531         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
402900160531         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
403000160531         IF  wParm <> *blanks;
403100160531           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
403200160531           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
403300160531         ELSE;
403400160531           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
403500160531           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
403600160531         ENDIF;
403700141021
403800141021       ENDSR;
403900141009
404000141009       //--------------------------------------------------------------
404100141009       //?Routine per schierare i messaggi nella ds FIORA0M0.
404200141009       //--------------------------------------------------------------
404300141009       BEGSR  Messaggi;
404400141009
404500141009       //?Se ho già caricato 99 messaggi esco
404600141009         IF  fiora0M0.nrmsg = 99;
404700141009           exsr RoutEnd;
404800141009           clear fiora0M0.nrmsg;
404900141009         ENDIF;
405000141009
405100141009         fiora0M0.nrmsg += 1;
405200141009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
405300160608         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
405400160531         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
405500141009         IF  wParm <> *blanks;
405600141009           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
405700141009           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
405800141009         ELSE;
405900141009           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
406000141009           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
406100141009         ENDIF;
406200141009
406300141009       ENDSR;
406400140414
406500140414       //--------------------------------------------------------------
406600140415      /end-free
406700140416
406800140415      *--------------------------------------------------
406900140618      * Procedure name: GetDesFiliale
407000140618      * Purpose:        Recupera la descrizione della filiale
407100140618      * Returns:        Descrizione
407200140618      * Parameter:      Filiale
407300140415      *--------------------------------------------------
407400140618     p GetDesFiliale   B
407500140618     d GetDesFiliale   PI            20a
407600140618     D  filiale                       3s 0 CONST
407700140415
407800140618      /free
407900140618       IF  filiale = *zeros;
408000140618         RETURN *blanks;
408100140618       ENDIF;
408200140618
408300140618       chain (filiale) AZORG01L;
408400140618       IF  not %found(AZORG01L);
408500140618         RETURN *blanks;
408600140618       ENDIF;
408700140618       IF  ORGfva <> *blanks;
408800140618         RETURN *blanks;
408900140618       ENDIF;
409000140618       IF  ORGfag <> 'F' and ORGfag <> 'A';
409100140618         RETURN *blanks;
409200140618       ENDIF;
409300140618
409400140618       RETURN ORGdes;
409500140415
409600140415      /end-free
409700140618     P GetDesFiliale   E
409800140618
409900140618      *--------------------------------------------------
410000140618      * Procedure name: IsAbilitata
410100140618      * Purpose:        Verifica se filiale abilitata
410200140618      * Returns:        ON - OFF
410300140618      * Parameter:      key tabelle VPO
410400140618      *                 Filiale 1 obbligo
410500140618      *                 Filiale 2 facoltativa
410600140618      *--------------------------------------------------
410700140618     p IsAbilitata     B
410800140618     d IsAbilitata     PI              n
410900140618     d keyVPO                        15a   const
411000140618     d filiale1                       3s 0 const
411100140618     d filiale2                       3s 0 const
411200140618     d                                     options(*nopass)
411300140618
411400140618      // ds per filiali abilitate
411500140618     d TRULVPODS     e ds
411600140618     d  skFilOk               16    765    dim(250)
411700140618
411800140618      // Pgm per reperire filiali abilitate
411900140618     d trulvpor        pr                  extpgm('TRULVPOR')
412000140618     d  trulvpods                          likeds(TRULVPODS)
412100140618
412200140618      /free
412300140618
412400140618       // se non c'è la key errore
412500140618       IF  keyVPO = *blanks;
412600140618         return *off;
412700140618       ENDIF;
412800140618
412900140618       // se non c'è prima filiale obbligatoria errore
413000140618       IF  filiale1 = *zeros;
413100140618         return *off;
413200140618       ENDIF;
413300140618
413400140618       // richiamo il Pgm per reperire filiali abilitate
413500140618       clear TRULVPODS;
413600140618       IVPOke1 = keyVPO;
413700140618       TRULVPOR (trulvpods);
413800140618
413900140618       // se al primo elemento c'è 999 tutti abilitati
414000140618       IF  skFilOk(1) = '999';
414100140618         return *on;
414200140618       ENDIF;
414300140618
414400140618       // se NON ho la seconda filiale verifico solo con la prima
414500140618       IF  %lookup(%editc(filiale1:'X'):skFilOk) > 0 and
414600140618           %parms = 2;
414700140618         return *on;
414800140618       ENDIF;
414900140618
415000140618       // ho tutte e 2 le filiali le verifico
415100140618       IF  %parms > 2 and filiale2 > 0 and
415200140618           %lookup(%editc(filiale1:'X'):skFilOk) > 0 and
415300140618           %lookup(%editc(filiale2:'X'):skFilOk) > 0;
415400140618         return *on;
415500140618       ENDIF;
415600140618
415700140618       return *off;
415800140618
415900140618      /end-free
416000140618     P IsAbilitata     E
416100160412
416200160412      *--------------------------------------------------
416300160412      * Procedure name: IsCodiceValido
416400160412      * Purpose:        Controlla codice
416500160412      * Returns:        ON - OFF
416600160412      *--------------------------------------------------
416700160412     p IsCodiceValido  B
416800160412     d IsCodiceValido  PI              n
416900160412     d  FIORB0O1                           likeds(FIORB0OO)
417000160412
417100160412      /copy gaitrasrc/srcconst,FIORB00R
417200160412     d isErrore        s               n   inz
417300160412     d RpyIdMsg        s             10i 0
417400160412     d RpyOpCode       s             10i 0
417500160412     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
417600160412     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
417700160412      /copy gaitrasrc/srcprotopr,FIORB00R
417800160412
417900160412      /free
418000160412
418100160412       reset isErrore;
418200160412       reset RpyIdMsg;
418300160412       reset RpyOpCode;
418400160412       reset fiorb0I1;
418500160412
418600160412       fiorb0I1.codice = wCodice;
418700160412       MONITOR;
418800160412         Fiorb00_Anagrafica( RqsOpCode
418900160412                           : RpyOpCode
419000160412                           : RpyIdMsg
419100160412                           : fiorb0I1.formato
419200160412                           : fiorb0I1
419300160412                           : %SIZE(fiorb0I1)
419400160412                           : fiorb0O1.formato
419500160412                           : fiorb0O1
419600160412                           : %SIZE(fiorb0O1)
419700160412                           );
419800160412         ON-ERROR;
419900160412           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
420000160412         ENDMON;
420100160412
420200160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE or
420300160519           RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND or
420400160519           RpyOpCode = FIORB00_RPYOPCODE_BLOCCATO;
420500160412         isErrore = *on;
420600160412       ENDIF;
420700160412
420800160412       IF  isErrore;
420900160412         return *off;
421000160412       ENDIF;
421100160412
421200160412       RETURN *on;
421300160412
421400160412      /END-FREE
421500160412     p IsCodiceValido  E
421600160616
421700160616      *--------------------------------------------------
421800160616      * Procedure name: GetDesGiro
421900160616      * Purpose:        Recupera la descrizione del giro
422000160616      * Returns:        Descrizione
422100160616      * Parameter:      Giro - Filiale Ritiro
422200160616      *--------------------------------------------------
422300160616     p GetDesGiro      B
422400160616     d GetDesGiro      PI            20a
422500160616     D  giro                         10a   CONST
422600160616     D  filiale                       3s 0 CONST
422700160616
422800160616      /free
422900160616       IF  giro = *blanks;
423000160616         RETURN *blanks;
423100160616       ENDIF;
423200160616       IF  filiale = *zeros;
423300160616         RETURN *blanks;
423400160616       ENDIF;
423500160616
423600160616       clear FIDG09DS;
423700160616       fidg09ds.D09iop0 = '001';
423800160616       fidg09ds.D09ifgs = filiale;
423900160616       fidg09ds.D09idat = dataoggi;
424000160616       fidg09ds.D09icgi = giro;
424100160616       fidg09ds.D09itug = 'R';
424200160616       kpjbu = FIDG09DS;
424300160616       fidg09r (kpjba);
424400160616       fidg09ds = kpjbu;
424500160616       IF  fidg09ds.D09oerr <> *blanks;
424600160616         RETURN *blanks;
424700160616       ENDIF;
424800160616
424900160616       RETURN fidg09ds.D09Odes;
425000160616
425100160616      /end-free
425200160616     P GetDesGiro      E
