000100131007       //==============================================================
000200131007       //
000300140414       //?FIORA00R - Controlli ORM
000400140613       //
000500140402       // Questo programma di servizio controlla i dati dell'ORM
000600140613       // nel paradigma MVC è il MODEL
000700140613       //
000800131007       //==============================================================
000900131009
001000131009       //--------------------------------------------------------------
001100131009       //?Specifiche di controllo.                                     ?
001200131009       //--------------------------------------------------------------
001300160216     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS':'UBBNDDIR':'TIO7')
001400131009
001500131009       //--------------------------------------------------------------
001600131009       //?Dichiarazione file.                                          ?
001700131009       //--------------------------------------------------------------
001800131010
001900131010       // -?File organigramma
002000131010     fAZORG01L  if   e           k disk
002100160315
002200160315       // -?File anagrafico clienti PdC
002300160315     fCNACO00F  if   e           k disk
002400170420
002500170420       // -?File ORM
002600170420     fFNORM18L  if   e           k disk
002700131009
002800131009       // -?File Tabelle
002900131009     fTABEL00F  if   e           k disk
003000131010     fTNTBE01L  if   e           k disk
003100160315
003200160315       // -?File Tariffe
003300160315     fTNTAM01L  if   e           k disk
003400131008
003500131007       //--------------------------------------------------------------
003600131007       //?Definizione costanti.                                        ?
003700131007       //--------------------------------------------------------------
003800131007      /copy gaitrasrc/srcconst,FIORA00R
003900170418      /copy gaitrasrc/srcconst,FIORB00R
004000140114
004100140114     d c_Digits        c                   const('0123456789')
004200160531     d SaltaFraseExport...
004300160531     d                 c                   const('S')
004400131008
004500131007       //--------------------------------------------------------------
004600131007       //?Definizione strutture dati.                                  ?
004700131007       //--------------------------------------------------------------
004800131007       // -?Dati INPUT
004900131009     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
005000140408     d FIORA0I1      e ds                  QUALIFIED INZ(*EXTDFT)
005100170418     d FIORB0I2      e ds                  QUALIFIED INZ(*EXTDFT)
005200131007
005300131007       // -?Dati OUTPUT
005400131009     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
005500140408     d FIORA0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005600170418     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
005700140401     d FIORB0OO      e ds                  EXTNAME(FIORB0O1)
005800140401     d                                     QUALIFIED INZ(*EXTDFT)
005900140401     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
006000140401     d                                     QUALIFIED INZ(*EXTDFT)
006100140401     d FIORB0OC      e ds                  EXTNAME(FIORB0O1)
006200140401     d                                     QUALIFIED INZ(*EXTDFT)
006300170418     d FIORB0O2      e ds                  QUALIFIED INZ(*EXTDFT)
006400131007
006500131007       // -?Messaggi
006600131009     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
006700131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006800131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006900131219     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
007000131219     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
007100131007
007200131007       // -?Forzature
007300131009     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
007400131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007500131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007600131219     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
007700160315
007800160315       // -?Controllo giro ritiro
007900160315     d FIDG09DS      e ds                  QUALIFIED INZ
008000160526
008100160526       // -?Ritorna orari servizio ritiri estero
008200160526     d FIORE00DS     e ds                  QUALIFIED INZ
008300140312
008400131205       // -?Controllo destinatario per ORM export DPD
008500131205     d FIOR95DS      e ds                  QUALIFIED INZ
008600131010
008700131010       // -?Forzatura filiale ritiro
008800131204     d FIOR96DS      e ds                  QUALIFIED INZ
008900131010
009000131010       // -?Calcolo data ritiro
009100131204     d FIOR97DS      e ds                  QUALIFIED INZ
009200170404     d FIOR971DS     e ds                  QUALIFIED INZ
009300131205
009400131205       // -?Ritirabilità all'estero
009500131205     d FNLV12DS      e ds                  QUALIFIED INZ
009600151009
009700151009       // -?Controllo cap/località/provincia
009800151009     d FNLV13DS      e ds                  QUALIFIED INZ
009900160315
010000160315       // -?Controllo Indirizzo
010100160315     d FNLV14DS      e ds                  QUALIFIED INZ
010200160315
010300160315       // -?Ricerca terminal
010400160315     d FNLV55DS      e ds                  QUALIFIED INZ
010500160922
010600160922       // -?Controllo limiti quantità
010700160922     d TRUL73DS      e ds                  QUALIFIED INZ
010800160922     d TRUL731DS     e ds                  QUALIFIED INZ
010900131223
011000131223       // -?Calcola orari di servizio
011100140327     d TRULORSDS     e ds                  QUALIFIED INZ
011200140327     d TRULOR2ds     e ds                  QUALIFIED INZ
011300131009
011400131204       // -?Controllo instradamento
011500131204     d TISI95DS      e ds                  QUALIFIED INZ
011600131205     d TISI97DS      e ds                  QUALIFIED INZ
011700131204
011800131204       // -?Controllo orari apertura
011900131204     d TRUL03DS      e ds                  QUALIFIED INZ
012000160408
012100160408       // -?Ritorna dati Servizio Clienti
012200160408     d TRULDSCDS     e ds                  QUALIFIED INZ
012300131008
012400131008       // -?Controllo partita IVA
012500151020     d XCFIVA1DS     e ds                  QUALIFIED INZ
012600140311
012700140311       // -?Controllo indirizzo mail
012800140311     d dsemail       e ds                  QUALIFIED INZ
012900160208
013000160208       // -?Tabella 15 - Nazioni
013100160208     d ds15          e ds                  QUALIFIED INZ
013200131204
013300131204       // -?Tabella 3I - DPD
013400131204     d ds3Idp        e ds                  QUALIFIED INZ
013500170418
013600170418       // -?Tabella AGR - Ambiti giri per filiale
013700170418     d dAGR          e ds                  QUALIFIED INZ
013800151008
013900151008       // -?Tabella DFT - Default ORM
014000151008     d dDFT          e ds                  QUALIFIED INZ
014100160315
014200160315       // -?Tabella FFC - Forza Filiale Consenga
014300160315     d dFFC          e ds                  QUALIFIED INZ
014400131010
014500131010       // -?Tabella NTW - Network
014600131204     d dNTW          e ds                  QUALIFIED INZ
014700140114
014800140114       // -?Campo ACRMAI
014900140114     d dACR01        e ds                  QUALIFIED INZ
015000170404
015100170404       // -?Campo IOREflo - TRULORSDS
015200170404     d dIORE01       e ds                  QUALIFIED INZ
015300131010
015400131010       // -?Campo VAOFLO
015500131204     d dORM01        e ds                  QUALIFIED INZ
015600131008
015700131008       // -?Partita IVA
015800131009     d wIVA            ds
015900131008     d  wISO                   1      2
016000131008     d  wPIVA                  3     16
016100131009
016200131009       //--------------------------------------------------------------
016300131009       //?Definizione schiere.
016400131009       //--------------------------------------------------------------
016500131009     d skprv           s              2a   dim(200)
016600131218     d sknaz           s              3a   dim(500)
016700160208     d sknazd          s             25a   dim(500)
016800131007
016900131007       //--------------------------------------------------------------
017000131007       //?Definizione campi.
017100131007       //--------------------------------------------------------------
017200131009     d dataoggi        s              8s 0
017300170404     d dataoggi7       s              8s 0
017400131220     d inv_dataritiro  s              8s 0
017500131010     d kpjba           s            502a
017600131205     d k_ORGfil        s                   like(ORGfil)
017700131205     d k_TBEcod        s                   like(TBEcod)
017800131205     d k_TBEke1        s                   like(TBEke1)
017900160315     d k_TBEke2        s                   like(TBEke2)
018000131205     d k_TBLcod        s                   like(TBLcod)
018100131205     d k_TBLkey        s                   like(TBLkey)
018200131010     d k_TBLkut        s                   like(TBLkut)
018300160315     d k_TAMksc        s                   like(TAMksc)
018400160315     d k_TAMctr        s                   like(TAMctr)
018500160315     d keyfilritiro    s                   like(wFiliale)
018600160315     d keytabella      s             15a
018700160922     d old_vaoncl      s                   like(fiora0I0.vaoncl)
018800160922     d old_vaopkg      s                   like(fiora0I0.vaopkg)
018900160922     d old_vaovlm      s                   like(fiora0I0.vaovlm)
019000160922     d old_vaobnc      s                   like(fiora0I0.vaobnc)
019100140411     d oraalfa         s              4a   inz
019200160216     d parametroNPR    s             10a   inz
019300170413     d PosticipaDataRitiro...
019400170413     d                 s               n   inz
019500140402     d wCap            s                   inz like(fiora0I0.vaocar)
019600140401     d wCodice         s             10s 0
019700131010     d wCom            s               n   inz
019800140408     d wData           s              8s 0 inz
019900131009     d wErr            s               n   inz
020000131219     d wErrMitt        s               n   inz
020100160613     d wErrQta         s               n   inz
020200160315     d wFil            s              3s 0 inz
020300131204     d wFiliale        s                   like(tisi95ds.O95lna)
020400160315     d wForza          s               n   inz
020500140401     d wHH             s              2s 0
020600131009     d wIdMsg          s              7a
020700131008     d wIdCampo        s             10a
020800140402     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
020900140402     d wLocalita       s                   inz like(fiora0I0.vaolor)
021000140401     d wMM             s              2s 0
021100140402     d wNazione        s                   inz like(fiora0I0.vaonar)
021200160315     d wNetworkCon     s              3a
021300160315     d wNetworkEmi     s              3a
021400160315     d wNetworkOrd     s              3a
021500131205     d wNetworkRit     s              3a
021600140211     d wNrForza        s              3s 0
021700140211     d wNrfForza       s              3s 0
021800170418     d wOKagr          s               n   inz
021900160315     d wOKffc          s               n   inz
022000131204     d wOKfilritiro    s               n   inz
022100160315     d wOKtariffa      s               n   inz
022200140401     d wOra            s              6s 0
022300131204     d wParm           s            512a   inz
022400131204     d wPeso           s                   like(tisi95ds.I95lkg)
022500160606     d wPesoaut        s             10s 1
022600160606     d wPesoblc        s             10s 1
022700160606     d wPesomax        s             10s 1
022800160606     d wPesomtc        s             10s 1
022900160315     d wPickup         s                   like(tisi95ds.O95gf2)
023000140408     d wPor            s                   inz like(fiora0I0.vaopor)
023100140402     d wProvincia      s                   inz like(fiora0I0.vaoprr)
023200151008     d wVolume         s                   like(tisi95ds.I95lmc)
023300160606     d wVolumeaut      s             10s 3
023400160606     d wVolumeblc      s             10s 3
023500151008     d wVolumemax      s             10s 3
023600160606     d wVolumemtc      s             10s 3
023700131009     d wVuoto          s              1a
023800131009     d xx              s              3s 0
023900170404
024000170404       // -?Campi di comodo data
024100170404     d dataISO         s               d   datfmt(*ISO)
024200170404     d dataEUR         s               d   datfmt(*EUR)
024300170418
024400170418       // -?Parametri per richiamo pgm
024500170418     d RpyIdMsg        s             10i 0
024600170418     d RpyOpCode       s             10i 0
024700170418     d RqsOpCode       s             10i 0
024800131008
024900131008       //--------------------------------------------------------------
025000131008       //?Definizione procedure.
025100131008       //--------------------------------------------------------------
025200160315       // -?Controllo giro di ritiro
025300160315     d fidg09r         pr                  extpgm('FIDG09R')
025400160315     d  kpjba                       502a
025500160526
025600160526       // -?Ritorna Orari Servizio Ritiro Estero
025700160526     d fiore00r        pr                  extpgm('FIORE00R')
025800160526     d  fiore00ds                          likeds(FIORE00DS)
025900131205
026000131205       // -?Controllo destinatario per ORM export DPD
026100131205     d fior95r         pr                  extpgm('FIOR95R')
026200131205     d  kpjba                       502a
026300131205     d  fior95ds                           likeds(FIOR95ds)
026400131010
026500131010       // -?Forzatura filiale ritiro
026600131010     d fior96r         pr                  extpgm('FIOR96R')
026700131010     d  kpjba                       502a
026800131010     d  fior96ds                           likeds(FIOR96ds)
026900131204
027000131204       // -?Calcolo data ritiro
027100131204     d fior97r         pr                  extpgm('FIOR97R')
027200131204     d  kpjba                       502a
027300170404     d  fior97ds                           likeds(FIOR97DS)
027400170404     d  fior971ds                          likeds(FIOR971DS)
027500170404     d                                     options(*nopass)
027600131205
027700131205       // -?Controllo ritirabilità all'estero
027800131205     d fnlv12r         pr                  extpgm('FNLV12R')
027900131205     d  fnlv12ds                           likeds(FNLV12DS)
028000131205     d  tisi95ds                           likeds(TISI95DS)
028100131205     d  tisi97ds                           likeds(TISI97DS)
028200151009
028300151009       // -?Controllo cap/località/provincia
028400151009     d fnlv13r         pr                  extpgm('FNLV13R')
028500151009     d  kpjba                       502a
028600151009     d  fnlv13ds                           likeds(FNLV13DS)
028700151009     d  tisi95ds                           likeds(TISI95DS)
028800160315
028900160315       // -?Pgm controllo indirizzo
029000160315     d FNLV14R         pr                  extpgm('FNLV14R')
029100160315     d  kpjba                       502a
029200160315     d  fnlv14ds                           likeds(FNLV14DS)
029300160315
029400160315       // -?Controllo terminal
029500160315     d fnlv55r         pr                  extpgm('FNLV55R')
029600160315     d  fnlv55ds                           likeds(FNLV55DS)
029700160216
029800160216       // -?Recupero nuovo Numero Prenotazione Ritiro
029900160216     d GetNPR          pr            10
030000160216     d  ParametroNPR                 10
030100160922
030200160922       // -?Controlla limiti quantità
030300160922     d trul73r         pr                  extpgm('TRUL73R')
030400160922     d  trul73ds                           likeds(TRUL73DS)
030500160922     d trul731r        pr                  extpgm('TRUL731R')
030600160922     d  trul731ds                          likeds(TRUL731DS)
030700131223
030800131223       // -?Calcolo orari servizio
030900140327     d trulorsr        pr                  extpgm('TRULORSR')
031000131223     d  kpjba                       502a
031100140327     d  trulorsds                          likeds(TRULORSDS)
031200140327     d  trulor2ds                          likeds(TRULOR2DS)
031300140327     d                                     options (*nopass)
031400131204
031500131204       // -?Controllo orari apertura
031600131204     d trul03r         pr                  extpgm('TRUL03R')
031700131204     d  trul03ds                           likeds(TRUL03ds)
031800160408
031900160408       // -?Ritorna dati Servizio Clienti
032000160408     d truldscr        pr                  extpgm('TRULDSCR')
032100160408     d  truldscds                          likeds(TRULDSCDS)
032200131008
032300131008       //--------------------------------------------------------------
032400131008       //?Definizione prototipi.
032500131008       //--------------------------------------------------------------
032600131008      /copy gaitrasrc/srcprotopr,FIORA00R
032700170418      /copy gaitrasrc/srcprotopr,FIORB00R
032800131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
032900131230      /copy gaitrasrc/srcprotopr,TIBS0800R
033000160315      /copy gaitrasrc/srcprotopr,TISI95R
033100140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
033200140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
033300151020      /copy gaitrasrc/srcprotopr,XCFIVAR1
033400140311      /copy gaitrasrc/srcprotopr,XEMAIL
033500131219
033600131219       //--------------------------------------------------------------
033700131219       //?Definizione parametri programma.
033800131219       //--------------------------------------------------------------
033900131219     d Fiora00_Immetti...
034000131219     d                 PI
034100131219     d prmRqsOpCode...
034200140210     d                               10I 0 CONST
034300131219     d prmRpyOpCode...
034400131219     d                               10I 0
034500131219     d prmRpyIdMsg...
034600131219     d                               10I 0
034700131219     d prmRqsFormato...
034800140210     d                               10A   CONST
034900131219     d prmRqsData...
035000131219     d                            32767A   OPTIONS(*VARSIZE)
035100131219     d prmRqsDataSize...
035200140210     d                               10I 0 CONST
035300131219     d prmRpyFormato...
035400140210     d                               10A   CONST
035500131219     d prmRpyData...
035600131219     d                            32767A   OPTIONS(*VARSIZE)
035700131219     d prmRpyDataSize...
035800140210     d                               10I 0 CONST
035900131219     d prmRpyFormMsg...
036000140408     d                               10A   CONST OPTIONS(*NOPASS)
036100131219     d prmRpyMessage...
036200140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
036300131219     d prmRpyMsgSize...
036400140408     d                               10I 0 CONST OPTIONS(*NOPASS)
036500131219     d prmRpyFormForz...
036600140408     d                               10A   CONST OPTIONS(*NOPASS)
036700131219     d prmRpyForzatu...
036800140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
036900131219     d prmRpyForSize...
037000140408     d                               10I 0 CONST OPTIONS(*NOPASS)
037100131007
037200131007       //--------------------------------------------------------------
037300131007       //?MAIN.
037400131007       //--------------------------------------------------------------
037500131008
037600131007      /free
037700131008
037800131008       //?Operazioni iniziali
037900131008       exsr RoutInz;
038000131008
038100131008       //?Controllo formale dei dati passati
038200131008       exsr Controlla;
038300140408
038400140408       //?Solo per formato FIORA0I0 = a richiamo <> da GET_FRASE1
038500140408       IF  fiora0I0.formato = prmRqsFormato;
038600140408
038700140408       //?Carico Tabelle
038800140408         exsr CaricaTab;
038900131007
039000131008       //?Imposto alcuni campi di DFT se non passati
039100131010         exsr DftInput;
039200131007
039300131007       //?Elaborazione principale per ORM da Internet
039400131010         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
039500131010           exsr Internet;
039600131010         ENDIF;
039700140620
039800160216       //?Elaborazione principale per ORM da AS400
039900140620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
040000140620           exsr AS400;
040100140620         ENDIF;
040200131009
040300131010       //?Imposto i dati di ritorno nella ds
040400131010       //?quelli che non sono già stati impostati dalle varie routine
040500131010         exsr DsOutput;
040600160216
040700160318       //?Dalla 'C' della ds di output FIORA0O0
040800160216       //?Se arrivo senza errori e sono stata richiamata con
040900160216       //?id richiamo OpCode WRITENPR
041000160216       //?devo staccare un nuovo numero NPR e tornarlo al chiamante
041100160408       //?più tutti i dati relativi alla filiale ritiro
041200160318         IF  fiora0O0.versione > 'B' and
041300160216             fiora0M0.nrmsg = 0 and
041400160216             prmRqsOpCode = FIORA00_RQSOPCODE_WRITENPR;
041500160216           fiora0O0.npr = GetNPR(parametroNPR);
041600160216         //?Se il rif. ORM è vuoto impoto il NPR
041700160216           IF  fiora0O0.vaorfa = *blanks;
041800160317             fiora0O0.vaorfa = fiora0O0.npr;
041900160216           ENDIF;
042000160408         //?Richiamo pgm che torna i dati del servizio clienti
042100160408         //?per la filiale passata
042200160503         //?per precauzione imposto già gli orari a 0
042300160503         //?è capitato che si spaccasse il pgm di Cussini per data/ora non validi
042400160503           fiora0O0.oraamdapor = *all'0';
042500160503           fiora0O0.oraamapor = *all'0';
042600160503           fiora0O0.orapmdapor = *all'0';
042700160503           fiora0O0.orapmapor = *all'0';
042800160408           clear TRULDSCDS;
042900160408           truldscds.IULDSCfil = fiora0O0.vaopor;
043000160408           truldscds.IULDSClin = fiora0O0.idlingua;
043100160408           truldscr (truldscds);
043200160408           IF  truldscds.OULDSCerr = *blanks;
043300160408             fiora0O0.descrpor = truldscds.OULDSCdesc;
043400160408             fiora0O0.telpor = truldscds.OULDSCtel;
043500160408             fiora0O0.mailpor = truldscds.OULDSCmail;
043600160408             fiora0O0.urlpor = truldscds.OULDSCurl;
043700160408             fiora0O0.oraamdapor = truldscds.OULDSCamda;
043800160408             fiora0O0.oraamapor = truldscds.OULDSCama;
043900160408             fiora0O0.orapmdapor = truldscds.OULDSCpmda;
044000160420             fiora0O0.orapmapor = truldscds.OULDSCpma;
044100160408           ENDIF;
044200160216         ENDIF;
044300140408
044400140408       ENDIF;
044500140408
044600140408       //?Solo per formato FIORA0I1 = a richiamo GET_FRASE1
044700140408       IF  fiora0I1.formato = prmRqsFormato;
044800140408         exsr GetFrase1;
044900140408       ENDIF;
045000131010
045100131010       //?Operazioni finali
045200131010       exsr RoutEnd;
045300131008
045400131008       //--------------------------------------------------------------
045500131008       //?Operazioni iniziali.                                         ?
045600131008       //--------------------------------------------------------------
045700131008       BEGSR  RoutInz;
045800140211
045900140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
046000131008
046100140210         prmRpyOpCode = FIORA00_RPYOPCODE_DONE;
046200140210         prmRpyIdMsg  = FIORA00_ESITO_OK;
046300131009
046400131009       //?Data del giorno
046500131009         dataoggi = %dec(%date());
046600170404
046700170404       //?Data del giorno - 7 giorni
046800170404         dataISO = %date(dataoggi:*iso);
046900170404         dataISO = dataISO - %days(7);
047000170404         dataoggi7 = %dec(dataISO);
047100131009
047200131009       //?Pulisco ds di output
047300131219         reset FIORA0O0;
047400140408         reset FIORA0O1;
047500131219         reset FIORA0M0;
047600131008
047700131008         *inLR = *on;
047800131008
047900131008       ENDSR;
048000131008
048100131008       //--------------------------------------------------------------
048200131008       //?Controllo formale dei dati passati.
048300131008       //--------------------------------------------------------------
048400131008       BEGSR  Controlla;
048500131008
048600131008       //?OpCode
048700131009         IF  prmRqsOpCode <> FIORA00_RQSOPCODE_CONTROL  and
048800140408             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITE    and
048900170530             prmRqsOpCode <> FIORA00_RQSOPCODE_CONFERMA_FISSI and
049000160216             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITENPR and
049100140408             prmRqsOpCode <> FIORA00_RQSOPCODE_GET_FRASE1;
049200131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
049300131009           prmRpyIdMsg  = FIORA00_ESITO_RQSOPCODE_SCONOSCIUTO;
049400140210           exsr RoutEnd;
049500131008         ENDIF;
049600140408
049700140408       //?per GET_FRASE1
049800140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
049900140408       //?Formato e size RQS
050000140408           IF  prmRqsFormato = fiora0I1.formato;
050100140408             fiora0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
050200140408           ELSE;
050300140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
050400140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
050500140408             exsr RoutEnd;
050600140408           ENDIF;
050700140408           IF  prmRqsDataSize > 0;
050800140408           ELSE;
050900140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051000140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
051100140408             exsr RoutEnd;
051200140408           ENDIF;
051300140408         //?Formato e size RPY
051400140408           IF  prmRpyFormato = fiora0O1.formato;
051500140408           ELSE;
051600140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051700140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
051800140408             exsr RoutEnd;
051900140408           ENDIF;
052000140408           IF  prmRpyDataSize > 0;
052100140408           ELSE;
052200140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
052300140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
052400140408             exsr RoutEnd;
052500140408           ENDIF;
052600140408           leavesr;
052700140408         ENDIF;
052800131008
052900140210       //?Formato e size RQS
053000131008         IF  prmRqsFormato = fiora0I0.formato;
053100131219           fiora0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
053200131008         ELSE;
053300131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
053400131008           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
053500140210           exsr RoutEnd;
053600131008         ENDIF;
053700140210         IF  prmRqsDataSize > 0;
053800140210         ELSE;
053900140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
054000140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
054100140210           exsr RoutEnd;
054200140210         ENDIF;
054300140210
054400140210       //?Formato e size RPY
054500140210         IF  prmRpyFormato = fiora0O0.formato;
054600140210         ELSE;
054700140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
054800140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
054900140210           exsr RoutEnd;
055000140210         ENDIF;
055100140210         IF  prmRpyDataSize > 0;
055200140210         ELSE;
055300140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
055400140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
055500140210           exsr RoutEnd;
055600140210         ENDIF;
055700140210
055800140210       //?Formato e size MSG
055900140210         IF  prmRpyFormMsg = fiora0M0.formato;
056000140210         ELSE;
056100140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
056200140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
056300140210           exsr RoutEnd;
056400140210         ENDIF;
056500140210         IF  prmRpyMsgSize > 0;
056600140210         ELSE;
056700140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
056800140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
056900140210           exsr RoutEnd;
057000140210         ENDIF;
057100140210
057200140210       //?Formato e size Forzature
057300140210         IF  prmRpyFormForz = fiora0F0.formato;
057400160527           fiora0F0 = %SUBST(prmRpyForzatu:1:prmRpyForSize);
057500140210         ELSE;
057600140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
057700140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
057800140210           exsr RoutEnd;
057900140210         ENDIF;
058000140210         IF  prmRpyForSize > 0;
058100140210         ELSE;
058200140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
058300140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
058400140210           exsr RoutEnd;
058500140210         ENDIF;
058600131008
058700131008       ENDSR;
058800140408
058900140408       //--------------------------------------------------------------
059000140408       //?Carico tabelle.
059100140408       //--------------------------------------------------------------
059200140408       BEGSR  CaricaTab;
059300140408
059400140408       //?Carico sk delle provincie
059500140408         clear xx;
059600140408         clear skprv;
059700140408         k_TBLkut = 1;
059800140408         k_TBLcod = 'PR';
059900140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
060000140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
060100140408         DOW not %eof(TABEL00F);
060200140408           IF  TBLflg = *blanks;
060300140408             xx += 1;
060400140408             skprv(xx) = TBLkey;
060500140408           ENDIF;
060600140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
060700140408         ENDDO;
060800140408
060900140408       //?Carico sk delle nazioni
061000140408         clear xx;
061100140408         clear sknaz;
061200140408         k_TBLkut = 1;
061300140408         k_TBLcod = '15';
061400140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
061500140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
061600140408         DOW not %eof(TABEL00F);
061700140408           IF  TBLflg = *blanks;
061800160208             ds15 = TBLuni;
061900140408             xx += 1;
062000140408             sknaz(xx) = TBLkey;
062100160208             sknazd(xx) = ds15.§15des;
062200140408           ENDIF;
062300140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
062400140408         ENDDO;
062500140408
062600140408       //?Carico tabella 3I - DPD
062700140408         clear ds3Idp;
062800140408         k_TBLkut = 1;
062900140408         k_TBLcod = '3I';
063000140408         k_TBLkey = 'DPD';
063100140408         chain (k_TBLkut:k_TBLcod:k_TBLkey) TABEL00F;
063200140408         IF  %found(TABEL00F) and TBLflg = *blanks;
063300140408           ds3Idp = TBLuni;
063400140408         ENDIF;
063500151008
063600151008       //?Carico tabella DFT ORM con key generica 999
063700151008         clear dDFT;
063800151008         k_TBEcod = 'DFT';
063900151008         k_TBEke1 = '999';
064000151008         chain (k_TBEcod:k_TBEke1) TNTBE01L;
064100151008         IF  %found(TNTBE01L);
064200151008           dDFT = TBEuni;
064300151008         ENDIF;
064400140408
064500140408       ENDSR;
064600131007
064700131007       //--------------------------------------------------------------
064800131008       //?Imposta i DFT se non passati alcuni campi.
064900131007       //--------------------------------------------------------------
065000131008       BEGSR  DftInput;
065100131007
065200140210       //?Se ID richiamo non valido o non passato
065300140210       //?assumo internet
065400140210         IF  fiora0I0.idrichiamo = *blanks or
065500140210            (fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET and
065600140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_FILE and
065700140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_AS400);
065800131009            fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
065900140210            fiora0M0.nrmsg += 1;
066000140210            fiora0M0.skIdMsg(fiora0M0.nrmsg) = '*parm';
066100140210            fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'IDRICHIAMO';
066200140210            fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
066300140210            fiora0M0.skTextMsg(fiora0M0.nrmsg) =
066400140210            'Id richiamo non previsto, assunto Internet';
066500131007         ENDIF;
066600131007
066700140210       //?Se ID lingua non valido o non passato
066800140210       //?assumo italiano
066900140210         IF  fiora0I0.idlingua = *blanks or
067000140210            (fiora0I0.idlingua <> FIORA00_ID_LINGUA_IT and
067100140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_FR and
067200140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_EN and
067300140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_DE);
067400140210            fiora0I0.idlingua = FIORA00_ID_LINGUA_IT;
067500131007         ENDIF;
067600131010
067700131010       //?Imposto la ds DORM01
067800131010         dORM01 = fiora0I0.vaoflo;
067900131007
068000131007       ENDSR;
068100131007
068200131007       //--------------------------------------------------------------
068300131007       //?Routine principale per view INTERNET.
068400131007       //--------------------------------------------------------------
068500131008       BEGSR  Internet;
068600131008
068700151110       //?Dalla versione 'B' il controllo del Cod.Fiscale e/o Partita IVA
068800151110       //?è da fare solo se cliente senza psw e se paga mittente
068900151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor = 0 and
069000151110             fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
069100151110           exsr CodFiscPiva;
069200151110         ENDIF;
069300151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor > 0;
069400151110           exsr Ordinante;
069500151110         ENDIF;
069600151110
069700151104         IF  fiora0I0.versione = 'A';
069800131010       //?Se ordinante non codificato (è utente senza PSW)
069900131008       //?controllo Codice Fiscale/Partita IVA
070000131009         IF  fiora0I0.vaocor = 0;
070100131010           exsr CodFiscPiva;
070200131223         ELSE;
070300131223           exsr Ordinante;
070400131008         ENDIF;
070500151104         ENDIF;
070600131009
070700131009       //?Controllo cliente mittente
070800131009         IF  fiora0I0.vaocra > 0;
070900131010           exsr CodMittente;
071000131219         ELSE;
071100131219       //?Controllo mittente
071200131219           exsr Mittente;
071300131219         ENDIF;
071400131219
071500131219       //?Se manca il mittente ho finito i giochi, subito errore
071600131219         IF  wErrMitt;
071700131219           leavesr;
071800131219         ENDIF;
071900140311
072000140311       //?Controllo chi paga
072100140311         exsr ChiPaga;
072200140404
072300140404       //?Controllo commissionato
072400140404         exsr Commissionato;
072500131205
072600131205       //?Controllo filiale emissione
072700131205         exsr FilEmissione;
072800151008
072900151008       //?Controllo le quantità
073000151008         exsr Quantita;
073100151008
073200151008       //?Calcolo il maggiore tra volume/quantità per instradamento
073300151008         exsr PesoVolume;
073400131205
073500131205       //?Controllo filiale ritiro
073600131205         exsr FilRitiro;
073700140408
073800140408       //?Cerco gli orari servizio
073900140408         exsr OrariServizio;
074000131205
074100131205       //?Controllo data ritiro
074200131205         exsr DataRitiro;
074300131011
074400131011       //?Controllo ora pronta merce
074500131011         exsr OraRitiro;
074600160404
074700160404       //?Controllo ora pronta merce con orari servizio
074800160404         exsr OraRitiroServizio;
074900131028
075000131028       //?Controllo orari apertura
075100140404         exsr OrariApertura;
075200131204
075300131204       //?Controllo la natura della merce
075400131204         exsr NaturaMerce;
075500131204
075600131204       //?Controllo uno o più destinatario
075700131204         exsr UnoPiuDest;
075800131205
075900131205       //?Controllo Referente
076000131205         exsr Referente;
076100131223
076200131223       //?Controllo Telefono
076300131223         exsr Telefono;
076400140311
076500140311       //?Controllo Mail
076600140311         exsr Mail;
076700140311
076800140311       //?Controllo SMS
076900140311         exsr Sms;
077000131205
077100140313       //?Controllo cliente destinatario
077200140313         IF  fiora0I0.vaocrc > 0;
077300140313           exsr CodDestinatario;
077400140313         ELSE;
077500131205       //?Controllo se deve esserci o no il Destinatario
077600140313           exsr Destinatario;
077700140313         ENDIF;
077800131205
077900131205       //?Controllo Fermo Deposito
078000131205         exsr FermoDeposito;
078100140506
078200140506       //?Controllo Alert
078300140506         exsr Alert;
078400151008
078500151008       //?Dalla versione 'B'
078600151008         IF  fiora0I0.versione > 'A';
078700151008         //?Controllo Mail x accettazione ORM
078800151008           exsr MailConferma;
078900151008         //?Calcolo orario indicativo ritiro
079000151008           exsr OrarioIndRitiro;
079100151008         ENDIF;
079200160318
079300160318       //?Dalla versione 'C'
079400160322         IF  fiora0I0.versione > 'B';
079500160318         //?Controllo SMS x accettazione ORM
079600160318           exsr SmsConferma;
079700160318         ENDIF;
079800160714
079900160714       //?Dalla versione 'D'
080000160714         IF  fiora0I0.versione > 'C';
080100160714         //?Controllo flag per memorizzazione dati su anagrafica clienti ritiro
080200160714           exsr FlagMemDati;
080300160714         ENDIF;
080400131007
080500131007       ENDSR;
080600140620
080700140620       //--------------------------------------------------------------
080800140620       //?Routine principale per view AS400.
080900140620       //--------------------------------------------------------------
081000140620       BEGSR  AS400;
081100160530
081200160530       //?Come prima cosa passo tutti i dati dalla ds di Input alla ds di Output
081300160530       //?in questo modo quando esco dal pgm per le forzature non perdo i dati
081400160530       //?immessi dall'utente
081500160530         eval-corr fiora0O0 = fiora0I0;
081600160530         reset fiora0O0.formato;
081700160530         reset fiora0O0.versione;
081800170404         wErr = *off;
081900170403
082000170403       //?Se ho già il dato impostato di ORM commissionato imposto anche il relativo flag
082100170403         IF  fiora0I0.contattot = FIORA00_SI;
082200170403           wCom = *on;
082300170403         ENDIF;
082400160322
082500160322       //?Controllo tipo Comunicazione ORM
082600160322         exsr TipoComOrm;
082700140620
082800140620       //?Controllo cliente mittente
082900140620         IF  fiora0I0.vaocra > 0;
083000140620           exsr CodMittente;
083100160401       //?Controllo dati mittente
083200160401         ELSE;
083300160401           exsr Mittente;
083400160315         ENDIF;
083500140717
083600140717       //?Se manca il mittente ho finito i giochi, subito errore
083700140717         IF  wErrMitt;
083800140717           leavesr;
083900140717         ENDIF;
084000170522
084100170522       //?Controllo tipo ORM
084200170522         exsr TipoOrm;
084300170522
084400170522       //?Imposto uno o più destinatario
084500170522         exsr UnoPiuDest;
084600140620
084700160315       //?Calcola filiale emissione
084800140620         exsr FilEmissione;
084900151008
085000151008       //?Controllo le quantità
085100151008         exsr Quantita;
085200160613         IF  wErrQta;
085300160613           leavesr;
085400160613         ENDIF;
085500170404
085600170404       //?Calcolo il maggiore tra volume/quantità per instradamento
085700170404         exsr PesoVolume;
085800170404
085900170404       //?Controllo la data pronta merce
086000170404         exsr DataProntaMerce;
086100170404
086200170404       //?In caso di Immissione/Copia ORM
086300170530       //?o di conferma ORM FISSI
086400170404       //?calcolo ora la filiale ritiro
086500170530         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
086600170530             prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
086700170404         //?Calcola filiale ritiro
086800170404           exsr FilRitiro;
086900170404         ENDIF;
087000170404
087100170404       //?Se filiale ritiro già impostata la metto subito nei dati di output
087200170404         IF  fiora0I0.vaopor > 0;
087300170404           fiora0O0.vaopor = fiora0I0.vaopor;
087400170418         //?Se mittente codificato recupero il giro dall'anagrafica clienti ritiro
087500170419           IF  fiora0I0.vaocra > 0 and fiora0I0.tgi <> 'M';
087600170418             exsr GiroAnagrafica;
087700170418           ENDIF;
087800170404         ENDIF;
087900170404
088000170404       //?Controllo Referente
088100170404         exsr Referente;
088200170404
088300170404       //?Controllo Telefono
088400170404         exsr Telefono;
088500170404
088600170404       //?Se ho qualche errore trovato in precedenza emetto subito errore
088700170404         IF  wErr;
088800170404           leavesr;
088900170404         ENDIF;
089000170404
089100170404       //?A questo punto in caso di Immissione/Copia ORM
089200170530       //?o di conferma ORM FISSI
089300170404       //?devo calcolare la data ritiro e gli orari di servizio
089400170530         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
089500170530             prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
089600170404           exsr DataRitiro;
089700170404           exsr OrariServizio;
089800170404         ENDIF;
089900170420
090000170420       //?Se Immissione/Copia ORM su mittente codificato
090100170530       //?o di conferma ORM FISSI
090200170420       //?se già immesso altro ORM
090300170420       //?per lo stesso mittente con stessa data ritiro
090400170530       //?anche in caso di conferma ORM FISSI
090500170530         IF  (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
090600170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
090700170420             fiora0I0.vaocra > *zeros;
090800170420           chain (fiora0I0.vaocra:fiora0I0.vaodar:fiora0I0.vaopoe) FNORM18L;
090900170420           IF  %found(FNORM18L);
091000170420             wIdMsg   = 'TIS0718';
091100170420             wIdCampo = 'VAOCRA';
091200170420             wParm = 'ORM ' + %editc(ORMpoe:'X') + '-' +
091300170420                     %editc(ORMnsr:'X') + '-' +
091400170420                     %editc(ORMnor:'X') + '-' +
091500170420                     %editc(ORMnrv:'X') + ' da ritirare lo ' +
091600170420                     'stesso giorno per lo stesso mittente';
091700170420             exsr Forzatura;
091800170426             IF  wForza;
091900170426               leavesr;
092000170426             ENDIF;
092100170420           ENDIF;
092200170420         ENDIF;
092300160606
092400160606       //?Controllo orari apertura
092500160606         exsr OrariApertura;
092600160607
092700160607       //?Controllo chi paga
092800160607         exsr ChiPaga;
092900170522
093000170522       //?Controllo per ORM prepagato
093100170522         exsr OrmPrepagato;
093200160607
093300160607       //?Controllo cliente destinatario
093400160607         IF  fiora0I0.vaocrc > 0;
093500160607           exsr CodDestinatario;
093600160607         ELSE;
093700160607       //?Controllo se deve esserci o no il Destinatario
093800160607           exsr Destinatario;
093900160607         ENDIF;
094000160610
094100160610       //?Controllo ora pronta merce
094200160610         exsr OraRitiro;
094300160518
094400160518       //?Controllo ora pronta merce con orari servizio
094500160518         exsr OraRitiroServizio;
094600140620
094700140620       //?Controllo la natura della merce
094800140620         exsr NaturaMerce;
094900160613
095000160613       //?Controllo le quantità della videata F05
095100160613         exsr Quantita2;
095200140620
095300140620       //?Controllo Mail
095400140620         exsr Mail;
095500140620
095600140620       //?Controllo SMS
095700140620         exsr Sms;
095800140620
095900140620       //?Controllo Fermo Deposito
096000140620         exsr FermoDeposito;
096100160315
096200160315       //?Dalla versione 'B'
096300160315         IF  fiora0I0.versione > 'A';
096400160315         //?Controllo Mail x accettazione ORM
096500160315           exsr MailConferma;
096600160318         //?Controllo SMS x accettazione ORM
096700160318           exsr SmsConferma;
096800160315         //?Calcolo orario indicativo ritiro
096900160315           exsr OrarioIndRitiro;
097000160315         ENDIF;
097100160315
097200160315       //?Controllo la filiale di ritiro
097300160601         exsr ControllaFilRitiro;
097400160315
097500160315       //?Controllo cliente ordinante
097600160315         IF  fiora0I0.vaocor > 0;
097700160315           exsr Ordinante;
097800160315         ENDIF;
097900160315       //?Controllo dati Ordinante
098000160315         exsr DatiOrdinante;
098100160315
098200160315       //?Controllo Filiale Consegna
098300160315       //?Campo della DS previsto solo da versione 'B' in poi
098400160315         IF  fiora0I0.versione > 'A';
098500160315           exsr FilConsegna;
098600160315         ENDIF;
098700160315
098800160315       //?Controllo Cliente tassazione
098900160315         exsr ClienteKSC;
099000160315
099100160315       //?Campo della DS previsto solo da versione 'B' in poi
099200160315         IF  fiora0I0.versione > 'A';
099300170419       //?Controllo il giro se impostato
099400170419           IF  fiora0I0.cgi <> *blanks;
099500170419             exsr GiroRitiro;
099600160610           ENDIF;
099700160315         ENDIF;
099800160315
099900160315       //?Calcola commissionato
100000160315         exsr Commissionato;
100100160610
100200160610       //?Se non ci sono errori
100300170530       //?se sono in immissione ORM o
100400170530       //?conferma ORM FISSI
100500160610       //?se ORM commissionato
100600160610       //?se dati alert ORM commissionato presenti
100700160610       //?verifico se la data di ritiro è da ricalcolare
100800160610         IF  fiora0M0.nrmsg = 0 and
100900170530             (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
101000170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
101100160610             wcom  and
101200160610            (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks) and
101300160610             fiora0I0.vaodar = dataoggi;
101400160610         //?Calcolo la data ritiro
101500160610           exsr DataRitiro;
101600160610         ENDIF;
101700140620
101800140620       ENDSR;
101900131008
102000131008       //--------------------------------------------------------------
102100131008       //?Controllo codice fiscale/partita iva.
102200131008       //--------------------------------------------------------------
102300131010       BEGSR  CodFiscPiva;
102400131009
102500131009         wErr = *off;
102600140211
102700140211         exec sql
102800140211         set :fiora0O0.codfiscale = ucase (:fiora0I0.codfiscale);
102900140211
103000140211         exec sql
103100140211         set :fiora0O0.partitaiva = ucase (:fiora0I0.partitaiva);
103200131008
103300131008       //?Accettiamo solo 1 dei 2, o CF o PI
103400131223         IF  fiora0O0.codfiscale <> *blanks and
103500131223             fiora0O0.partitaiva <> *blanks;
103600131008           wIdMsg   = 'TIS0240';
103700131008           wIdCampo = 'CODFISCALE';
103800131204           clear wParm;
103900131008           exsr Messaggi;
104000131009           wErr = *on;
104100131008         ENDIF;
104200131008
104300131008       //?Almeno 1 dei 2 deve essere presente, o CF o PI
104400131223         IF  fiora0O0.codfiscale = *blanks and
104500131223             fiora0O0.partitaiva = *blanks;
104600131008           wIdMsg   = 'TIS0240';
104700131008           wIdCampo = 'CODFISCALE';
104800131204           clear wParm;
104900131008           exsr Messaggi;
105000131009           wErr = *on;
105100131008         ENDIF;
105200131008
105300131008       //?Controllo CF
105400131223         IF  fiora0O0.codfiscale <> *blanks;
105500151020           clear xcfiva1ds;
105600151020           xcfiva1ds.xcfivapar = fiora0O0.codfiscale;
105700151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
105800151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
105900151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
106000151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
106100151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
106200151020           xcfivar1 (xcfiva1ds);
106300151020           IF  xcfiva1ds.xcfivaflg < *zeros;
106400131008             wIdMsg   = 'TIS0069';
106500131008             wIdCampo = 'CODFISCALE';
106600131204             clear wParm;
106700131008             exsr Messaggi;
106800131009             wErr = *on;
106900131008           ENDIF;
107000131008         ENDIF;
107100131008
107200131008       //?Partita IVA valida
107300131223         IF  fiora0O0.partitaiva <> *blanks;
107400140211           IF  %subst(fiora0O0.partitaiva:1:2) >= '00';
107500140211             wPIVA = fiora0O0.partitaiva;
107600140211           ELSE;
107700140211             wIVA = fiora0O0.partitaiva;
107800140211           ENDIF;
107900151020           clear xcfiva1ds;
108000151020           xcfiva1ds.xcfivamod = 'P';
108100151020           xcfiva1ds.xcfivapar = wIVA;
108200151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
108300151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
108400151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
108500151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
108600151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
108700151020           xcfivar1 (xcfiva1ds);
108800131009
108900131008         //?Errore forzabile
109000151020           IF  xcfiva1ds.xcfivaflg = -9;
109100131008             wIdMsg   = 'TIS0444';
109200131008             wIdCampo = 'PARTITAIVA';
109300140210             clear wParm;
109400131008             exsr Forzatura;
109500131009             wErr = *on;
109600131008           ENDIF;
109700131008
109800131008         //?Errore bloccante
109900151020           IF  xcfiva1ds.xcfivaflg < *zeros and xcfiva1ds.xcfivaflg <> -9;
110000131008             wIdMsg   = 'TIS0444';
110100131008             wIdCampo = 'PARTITAIVA';
110200131204             clear wParm;
110300131009             exsr Messaggi;
110400131009             wErr = *on;
110500131008           ENDIF;
110600131009
110700131009       //?Se errore non imposto i campi di output
110800131009          IF  wErr;
110900131009            leavesr;
111000131009          ENDIF;
111100131008
111200131008       //?Imposto il codice ISO che torna dal pgm di controllo
111300151020           IF  wISO <> %subst(xcfiva1ds.xcfivapar:3:2);
111400151020             wISO = %subst(xcfiva1ds.xcfivapar:3:2);
111500131008           ENDIF;
111600131008         ENDIF;
111700131007
111800131008       ENDSR;
111900131223
112000131223       //--------------------------------------------------------------
112100131223       //?Imposto i dati dell'ordinante.
112200131223       //--------------------------------------------------------------
112300131223       BEGSR  Ordinante;
112400131223
112500160520       //?Solo per richiamo da AS400
112600160520       //?torno il codice al chiamante
112700160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
112800160520           fiora0O0.vaocor = fiora0I0.vaocor;
112900160520         ENDIF;
113000160520
113100140401         wCodice = fiora0I0.vaocor;
113200140402         reset FIORB0OO;
113300140401
113400140401       //?codice valido
113500140402         IF  not IsCodiceValido(FIORB0OO);
113600140305           wIdMsg   = 'TIS0719';
113700140304           wIdCampo = 'VAOCOR';
113800140305           clear wParm;
113900131223           exsr Messaggi;
114000131223           leavesr;
114100131223         ENDIF;
114200160315
114300160315       //?Solo per richiamo da AS400
114400160315       //?e solo dalla versione 'B' in poi
114500160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
114600160315             fiorb0OO.versione > 'A';
114700160315       //?codice bloccato
114800160714           IF  fiorb0OO.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
114900160315             wIdMsg   = 'TIS0799';
115000160315             wIdCampo = 'VAOCOR';
115100160315             clear wParm;
115200160315             exsr Messaggi;
115300160315             leavesr;
115400160315           ENDIF;
115500160315         ENDIF;
115600131227
115700131227       //?cerco network della filiale del codice ordinante
115800160315         clear wNetworkOrd;
115900160315         wFil = %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
116000160315         wNetworkOrd = GetNtwFiliale(wFil);
116100131227         clear dNTW;
116200131227         k_TBEcod = 'NTW';
116300160315         k_TBEke1 = wNetworkOrd;
116400131227         chain (k_TBEcod:k_TBEke1) TNTBE01L;
116500131227         IF  %found(TNTBE01L);
116600131227           dNTW = TBEuni;
116700131227         ENDIF;
116800131227       //?se la filiale del codice ordinante è una filiale con network DPD
116900160315         IF  wNetworkOrd = FIORA00_NTW_DPD;
117000131227         //?imposto il servizio DPD
117100131227           fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
117200131227         ENDIF;
117300131227       //?se la filiale del codice ordinante è una filiale con network estero
117400160315         IF  dntw.§NTWfie = FIORA00_NTW_ESTERO;
117500131227         //?imposto il servizio Estero
117600131227           fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
117700131227         ENDIF;
117800160315
117900160315       //?Solo per richiamo da AS400
118000160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
118100160315       //?imposto i dati di ritorno dell'ordinante
118200160315           fiora0I0.vaorso = fiorb0OO.nome;
118300160315           fiora0I0.vaoino = fiorb0OO.indirizzo;
118400160315           fiora0I0.vaocao = fiorb0OO.cap;
118500160315           fiora0I0.vaoloo = fiorb0OO.localita;
118600160315           fiora0I0.vaopro = fiorb0OO.provincia;
118700160315           fiora0I0.vaonao = fiorb0OO.nazione;
118800160315         //?Se paga ordinante imposto i dati
118900160315           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
119000160315               fiora0I0.vaoksc = 0 and fiorb0OO.codksc > 0;
119100160315             fiora0I0.vaoksc = fiorb0OO.codksc;
119200160315             IF  fiorb0OO.codtariffa = 999;
119300160315               clear fiora0I0.vaoctr;
119400160315             ELSE;
119500160315               fiora0I0.vaoctr =
119600160315               %subst(%editc(fiorb0OO.codtariffa:'X'):2:3);
119700160315             ENDIF;
119800160315           ENDIF;
119900160315         //?Imposto se contatto telefonico (commissionato)
120000160315         //?solo dalla versione 'B' in poi
120100160315           IF  fiorb0OO.versione > 'A';
120200160315             fiora0O0.contattot = fiorb0OO.contatto;
120300160315           ENDIF;
120400160315         ENDIF;
120500131223
120600131223       ENDSR;
120700160315
120800160315       //--------------------------------------------------------------
120900160315       //?Controllo dati Ordinante.
121000160315       //--------------------------------------------------------------
121100160315       BEGSR  DatiOrdinante;
121200160315
121300160315       //?Se non ho dati vado via
121400160315         IF  fiora0I0.vaorso = *blanks and fiora0I0.vaoino = *blanks and
121500160315             fiora0i0.vaoloo = *blanks and fiora0i0.vaocao = *blanks;
121600160315           leavesr;
121700160315         ENDIF;
121800160315
121900160315         exec sql
122000160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
122100160315
122200160315       //?Controllo nominativo ordinante
122300160315         exsr NomeOrdinante;
122400160315
122500160315       //?Controllo indirizzo ordinante
122600160315         exsr ViaOrdinante;
122700160315
122800160315       //?Controllo località ordinante
122900160315         exsr LocOrdinante;
123000160315
123100160315       //?Controllo cap ordinante
123200160315         exsr CapOrdinante;
123300160315
123400160315       //?Controllo provincia ordinante
123500160315         exsr ProvOrdinante;
123600160315
123700160315       //?Controllo nazione ordinante
123800160315         exsr NazOrdinante;
123900160315
124000160315       //?Controllo indirizzo completo dell'ordinante
124100160315         exsr IndTotOrdinante;
124200160315
124300160315       ENDSR;
124400160315
124500160315       //--------------------------------------------------------------
124600160315       //?Controllo nominativo ordinante.
124700160315       //--------------------------------------------------------------
124800160315       BEGSR  NomeOrdinante;
124900160315
125000160315         exec sql
125100160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
125200160315
125300160315       ENDSR;
125400160315
125500160315       //--------------------------------------------------------------
125600160315       //?Controllo indirizzo ordinante.
125700160315       //--------------------------------------------------------------
125800160315       BEGSR  ViaOrdinante;
125900160315
126000160315         exec sql
126100160315         set :fiora0O0.vaoino = ucase (:fiora0I0.vaoino);
126200160315
126300160315       ENDSR;
126400160315
126500160315       //--------------------------------------------------------------
126600160315       //?Controllo cap ordinante.
126700160315       //--------------------------------------------------------------
126800160315       BEGSR  CapOrdinante;
126900160315
127000160315         exec sql
127100160315         set :fiora0O0.vaocao = ucase (:fiora0I0.vaocao);
127200160315
127300160315       ENDSR;
127400160315
127500160315       //--------------------------------------------------------------
127600160315       //?Controllo località ordinante.
127700160315       //--------------------------------------------------------------
127800160315       BEGSR  LocOrdinante;
127900160315
128000160315         exec sql
128100160315         set :fiora0O0.vaoloo = ucase (:fiora0I0.vaoloo);
128200160315
128300160315       ENDSR;
128400160315
128500160315       //--------------------------------------------------------------
128600160315       //?Controllo provincia ordinante.
128700160315       //--------------------------------------------------------------
128800160315       BEGSR  ProvOrdinante;
128900160315
129000160315         exec sql
129100160315         set :fiora0O0.vaopro = ucase (:fiora0I0.vaopro);
129200160315
129300160315       //?Controllo con la sk delle provincie
129400160315         IF  fiora0O0.vaopro <> *blanks and
129500160315            %lookup(fiora0O0.vaopro:skprv) = 0;
129600160315           wIdMsg   = 'TIS0464';
129700160315           wIdCampo = 'VAOPRO';
129800160315           clear wParm;
129900160315           exsr Messaggi;
130000160315         ENDIF;
130100160315
130200160315       ENDSR;
130300160315
130400160315       //--------------------------------------------------------------
130500160315       //?Controllo nazione ordinante.
130600160315       //--------------------------------------------------------------
130700160315       BEGSR  NazOrdinante;
130800160315
130900160315         exec sql
131000160315         set :fiora0O0.vaonao = ucase (:fiora0I0.vaonao);
131100160315
131200160315         //?Controllo con la sk delle nazioni
131300160315         IF  fiora0O0.vaonao <> *blanks and
131400160315             %lookup(fiora0O0.vaonao:sknaz) = 0;
131500160315           wIdMsg   = 'TIS0385';
131600160315           wIdCampo = 'VAONAO';
131700160315           clear wParm;
131800160315           exsr Messaggi;
131900160315         ENDIF;
132000160315
132100160315       ENDSR;
132200160315
132300160315       //--------------------------------------------------------------
132400160315       //?Controllo indirizzo completo dell'ordinante.
132500160315       //--------------------------------------------------------------
132600160315       BEGSR  IndTotOrdinante;
132700160315
132800160315         clear TISI95DS;
132900160315         tisi95ds.I95tcn = '7';
133000160315         tisi95ds.I95nar = fiora0O0.vaonao;
133100160315         tisi95ds.I95cap = fiora0O0.vaocao;
133200160315         tisi95ds.I95loc = fiora0O0.vaoloo;
133300160315         tisi95ds.I95prv = fiora0O0.vaopro;
133400160315         tisi95ds.I95ind = fiora0O0.vaoino;
133500160315         tisi95ds.I95dat = %dec(%date());
133600160315
133700160315       //?richiamo il tisi95r
133800160315         ChkCapLocalita (tisi95ds);
133900160315
134000160315       //?errore o dato non affidabile
134100160315         IF  fiora0O0.vaonao = *blanks and tisi95ds.O95lia <> '3'  or
134200160315             fiora0O0.vaonao <> *blanks and tisi95ds.O95lia <> '2';
134300160315           wIdMsg   = 'TIS0054';
134400160315           wIdCampo = 'VAOLOO';
134500160315           clear wParm;
134600160315           exsr Messaggi;
134700160315           leavesr;
134800160315         ENDIF;
134900160315
135000160315       ENDSR;
135100160322
135200160322       //--------------------------------------------------------------
135300160322       //?Controllo Tipo Comunicazione ORM.
135400160322       //--------------------------------------------------------------
135500160322       BEGSR  TipoComOrm;
135600160322
135700160404         k_TBEcod = 'TCO';
135800160404         k_TBEke1 = fiora0I0.tco;
135900160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
136000160322         IF  not %found(TNTBE01L);
136100160404           wIdMsg   = 'TIS0832';
136200160404           wIdCampo = 'TCO';
136300160322           clear wParm;
136400160322           exsr Messaggi;
136500160322           leavesr;
136600160322         ENDIF;
136700160520
136800160520       //?Se richiamato da AS400
136900160520       //?se sono in WRITE ORM
137000160520       //?controllo il tipo comunicazione ORM con il tipo richiamo
137100160520       //?se richiamato da immissine manuale posso immettere solo
137200160520       //?'TELEFONICO' 'MAIL/FAX'
137300160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
137400160520             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
137500160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_TELEFONICO and
137600160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_MAIL;
137700160520           wIdMsg   = 'TIS0832';
137800160520           wIdCampo = 'VAOTCO';
137900160520           clear wParm;
138000160520           exsr Messaggi;
138100160520         ENDIF;
138200160520       //?se richiamato da immissione VAS posso avere solo
138300160520       //?'FILE' 'INTERNET'
138400160520       //?se richiamato da immissione FISSI posso avere solo
138500160520       //?'FISSO'
138600170530         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
138700170530             prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI AND
138800170530             fiora0I0.tco <> FIORA00_TIPOCOMORM_FISSO;
138900170530           wIdMsg   = 'TIS0832';
139000170530           wIdCampo = 'VAOTCO';
139100170530           clear wParm;
139200170530           exsr Messaggi;
139300170530         ENDIF;
139400160520       //?se richiamato da immissione PREPAGATI posso avere solo
139500160520       //?'PREPAGATO'
139600170404       //?===========>>>>>>
139700160322
139800160322       ENDSR;
139900160315
140000160315       //--------------------------------------------------------------
140100160315       //?Controllo Tipo ORM.
140200160315       //--------------------------------------------------------------
140300160315       BEGSR  TipoOrm;
140400160315
140500160322         k_TBEcod = 'TOR';
140600160322         k_TBEke1 = fiora0I0.tor;
140700160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
140800160322         IF  not %found(TNTBE01L);
140900160322           wIdMsg   = 'TIS0797';
141000160322           wIdCampo = 'TOR';
141100160322           clear wParm;
141200160322           exsr Messaggi;
141300160322           leavesr;
141400160322         ENDIF;
141500160315
141600160315       //?Se tipo ORM = P imposto il flag di prepagato
141700160315       //?e che è un solo destinatario
141800160322         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
141900160322           fiora0O0.prepagato = FIORA00_PREPAGATO;
142000160404           fiora0I0.unopiudest = '1';
142100160322         ENDIF;
142200160714       //?Se tipo ORM = M imposto 2 più destinatari
142300160322         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO;
142400160714           fiora0I0.unopiudest = FIORA00_PIUDESTINATARI;
142500160322         ENDIF;
142600160322       //?Se tipo ORM = S imposto 1 destinatario
142700160322         IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO;
142800160714           fiora0I0.unopiudest = FIORA00_UNDESTINATARIO;
142900160322         ENDIF;
143000160315
143100160315       ENDSR;
143200131009
143300131009       //--------------------------------------------------------------
143400131009       //?Controllo codice mittente.
143500131009       //--------------------------------------------------------------
143600131010       BEGSR  CodMittente;
143700160315
143800160315         wErrMitt = *off;
143900140401
144000140401         wCodice = fiora0I0.vaocra;
144100140402         reset FIORB0OR;
144200131009
144300131009       //?codice valido
144400140402         IF not IsCodiceValido(FIORB0OR);
144500140312           wIdMsg   = 'TIS0068';
144600131009           wIdCampo = 'VAOCRA';
144700131204           clear wParm;
144800131009           exsr Messaggi;
144900160315         //?Solo per richiamo da AS400
145000160315           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
145100160315             wErrMitt = *on;
145200160315           ENDIF;
145300131009           leavesr;
145400131009         ENDIF;
145500160315
145600160315       //?Solo per richiamo da AS400
145700160315       //?e solo dalla versione 'B' in poi
145800160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
145900160315             fiorb0OR.versione > 'A';
146000160315         //?codice bloccato
146100160714           IF  fiorb0OR.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
146200160315             wIdMsg   = 'TIS0800';
146300160315             wIdCampo = 'VAOCRA';
146400160315             clear wParm;
146500160315             exsr Messaggi;
146600160315             wErrMitt = *on;
146700160315             leavesr;
146800160315           ENDIF;
146900160315         ENDIF;
147000140312
147100160315       //?Solo per richiamo da Internet
147200160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
147300140312       //?controllo se fa parte della stessa famiglia del codice di LOG
147400140313         IF not IsCodiceLegato();
147500140313           wIdMsg   = 'TIS0068';
147600140313           wIdCampo = 'VAOCRA';
147700140313           clear wParm;
147800140313           exsr Messaggi;
147900140313           leavesr;
148000140313         ENDIF;
148100160315         ENDIF;
148200140402
148300140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
148400140402         wIndirizzo = fiorb0OR.indirizzo;
148500140402         wCap = fiorb0OR.cap;
148600140402         wLocalita = fiorb0OR.localita;
148700140402         wProvincia = fiorb0OR.provincia;
148800140402         wNazione = fiorb0OR.nazione;
148900140411
149000140411       //?Solo per richiamo da Internet
149100140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
149200140411       //?normalizzo l'ora pronta merce dell'anagrafica clienti ritiro
149300140411             fiorb0OR.oraritiro > 0;
149400140411           oraalfa = GetOraAggiustata(fiorb0OR.oraritiro);
149500140411           fiorb0OR.oraritiro = %dec(oraalfa:4:0);
149600140411
149700151012           IF  fiorb0OR.oraritiro > *zeros and
149800151012              (fiora0I0.orrmin > 0 or fiora0I0.orrmax > 0);
149900140411             SELECT;
150000140411               WHEN fiorb0Or.oraritiro < fiora0I0.orrmin;
150100140411                 fiorb0OR.oraritiro = fiora0I0.orrmin;
150200140411               WHEN fiorb0Or.oraritiro > fiora0I0.orrmax;
150300140411                 fiorb0OR.oraritiro = fiora0I0.orrmax;
150400140411             ENDSL;
150500140411           ENDIF;
150600140411         ENDIF;
150700140717
150800140717       //?Solo per richiamo da AS400
150900160520       //?Imposto i dati del mittente
151000160519         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
151100160530           fiora0O0.vaorsr = fiora0I0.vaorsr;
151200160530           fiora0O0.vaoinr = fiora0I0.vaoinr;
151300160530           fiora0O0.vaocar = fiora0I0.vaocar;
151400160530           fiora0O0.vaolor = fiora0I0.vaolor;
151500160530           fiora0O0.vaoprr = fiora0I0.vaoprr;
151600160530           fiora0O0.vaonar = fiora0I0.vaonar;
151700140717         ENDIF;
151800131009
151900131009       ENDSR;
152000131219
152100131219       //--------------------------------------------------------------
152200131219       //?Controllo il mittente.
152300131219       //--------------------------------------------------------------
152400131219       BEGSR  Mittente;
152500131219
152600131219         wErrMitt = *off;
152700131219
152800131223       //?Se non ho nessun dato del cliente non controllo più niente
152900131219         IF  fiora0I0.vaorsr = *blanks and
153000131219             fiora0I0.vaoinr = *blanks and
153100131219             fiora0I0.vaocar = *blanks and
153200131219             fiora0I0.vaolor = *blanks and
153300131219             (fiora0I0.vaoprr = *blanks or
153400131219              fiora0I0.vaonar = *blanks);
153500140304           wIdMsg   = 'TIS0361';
153600131219           wIdCampo = 'VAORSR';
153700131219           clear wParm;
153800131219           exsr Messaggi;
153900131219           wErrMitt = *on;
154000131219           leavesr;
154100131219         ENDIF;
154200131219
154300131219       //?Nominativo mittente
154400131219         exsr NomeMittente;
154500131219
154600131219       //?Indirizzo mittente
154700131219         exsr ViaMittente;
154800160315
154900160315       //?Solo per richiamo da AS400
155000160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
155100160315         //?Controllo prima la località, poi provincia e cap
155200160315         //?Località mittente
155300160315           exsr LocMittente;
155400160315         //?Provincia mittente
155500160315           exsr ProvMittente;
155600160315         //?Cap mittente
155700160315           exsr CapMittente;
155800160520         ENDIF;
155900160520
156000160525       //?Per richiamo da INTERNET
156100160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
156200131219       //?Cap mittente
156300131219         exsr CapMittente;
156400131219
156500131219       //?Località mittente
156600131219         exsr LocMittente;
156700131219
156800131219       //?Provincia mittente
156900131219         exsr ProvMittente;
157000160315         ENDIF;
157100131219
157200131219       //?Nazione mittente
157300131219         exsr NazMittente;
157400131219
157500131219       //?Controllo indirizzo completo del mittente
157600131219         clear wPeso;
157700151008         clear wVolume;
157800131219         exsr IndTotMittente;
157900140402
158000140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
158100140402         wIndirizzo = fiora0I0.vaoinr;
158200140402         wCap = fiora0I0.vaocar;
158300140402         wLocalita = fiora0I0.vaolor;
158400140402         wProvincia = fiora0I0.vaoprr;
158500140402         wNazione = fiora0I0.vaonar;
158600131219
158700131219       ENDSR;
158800131009
158900131009       //--------------------------------------------------------------
159000131009       //?Controllo nominativo mittente.
159100131009       //--------------------------------------------------------------
159200131010       BEGSR  NomeMittente;
159300140211
159400140211         exec sql
159500140211         set :fiora0O0.vaorsr = ucase (:fiora0I0.vaorsr);
159600131009
159700131219       //?Errore se non presente
159800131223         IF  fiora0O0.vaorsr = *blanks;
159900131205           wIdMsg   = 'TIS0482';
160000131009           wIdCampo = 'VAORSR';
160100131204           clear wParm;
160200131009           exsr Messaggi;
160300131219           wErrMitt = *on;
160400131009         ENDIF;
160500131009
160600131009       ENDSR;
160700131009
160800131009       //--------------------------------------------------------------
160900131009       //?Controllo indirizzo mittente.
161000131009       //--------------------------------------------------------------
161100131010       BEGSR  ViaMittente;
161200140211
161300140211         exec sql
161400140211         set :fiora0O0.vaoinr = ucase (:fiora0I0.vaoinr);
161500131009
161600131219       //?Errore se non presente
161700131223         IF  fiora0O0.vaoinr = *blanks;
161800131205           wIdMsg   = 'TIS0283';
161900131009           wIdCampo = 'VAOINR';
162000131204           clear wParm;
162100131009           exsr Messaggi;
162200131219           wErrMitt = *on;
162300131009         ENDIF;
162400131009
162500131009       ENDSR;
162600131009
162700131009       //--------------------------------------------------------------
162800131009       //?Controllo cap mittente.
162900131009       //--------------------------------------------------------------
163000131010       BEGSR  CapMittente;
163100140211
163200140211         exec sql
163300140211         set :fiora0O0.vaocar = ucase (:fiora0I0.vaocar);
163400131009
163500131219       //?Errore se non presente
163600131223         IF  fiora0O0.vaocar = *blanks;
163700131205           wIdMsg   = 'TIS0053';
163800131009           wIdCampo = 'VAOCAR';
163900131204           clear wParm;
164000131009           exsr Messaggi;
164100170404           wErrMitt = *on;
164200131009         ENDIF;
164300131009
164400131009       ENDSR;
164500131009
164600131009       //--------------------------------------------------------------
164700131009       //?Controllo località mittente.
164800131009       //--------------------------------------------------------------
164900131010       BEGSR  LocMittente;
165000140211
165100140211         exec sql
165200140211         set :fiora0O0.vaolor = ucase (:fiora0I0.vaolor);
165300131219
165400131009       //?Errore se non presente codice e località
165500131223         IF  fiora0O0.vaolor = *blanks;
165600131205           wIdMsg   = 'TIS0347';
165700131009           wIdCampo = 'VAOLOR';
165800131204           clear wParm;
165900131009           exsr Messaggi;
166000170404           wErrMitt = *on;
166100131009         ENDIF;
166200131009
166300131009       ENDSR;
166400131009
166500131009       //--------------------------------------------------------------
166600131009       //?Controllo provincia mittente.
166700131009       //--------------------------------------------------------------
166800131010       BEGSR  ProvMittente;
166900140211
167000140211         exec sql
167100140211         set :fiora0O0.vaoprr = ucase (:fiora0I0.vaoprr);
167200131009
167300131009       //?Controllo con la sk delle provincie
167400131009         IF  fiora0O0.vaoprr <> *blanks and
167500131009             %lookup(fiora0O0.vaoprr:skprv) = 0;
167600131205           wIdMsg   = 'TIS0466';
167700131009           wIdCampo = 'VAOPRR';
167800131204           clear wParm;
167900131009           exsr Messaggi;
168000131219           wErrMitt = *on;
168100131009         ENDIF;
168200131009
168300131009       ENDSR;
168400131008
168500131009       //--------------------------------------------------------------
168600131009       //?Controllo nazione mittente.
168700131009       //--------------------------------------------------------------
168800131010       BEGSR  NazMittente;
168900140211
169000140211         exec sql
169100140211         set :fiora0O0.vaonar = ucase (:fiora0I0.vaonar);
169200131009
169300131009       //?Controllo con la sk delle provincie
169400131009         IF  fiora0O0.vaonar <> *blanks and
169500131009             %lookup(fiora0O0.vaonar:sknaz) = 0;
169600140326           wIdMsg   = 'TIS0386';
169700131009           wIdCampo = 'VAONAR';
169800131204           clear wParm;
169900131009           exsr Messaggi;
170000131219           wErrMitt = *on;
170100131009         ENDIF;
170200131009
170300131009       ENDSR;
170400131009
170500131009       //--------------------------------------------------------------
170600131009       //?Controllo indirizzo completo del mittente.
170700131009       //--------------------------------------------------------------
170800131010       BEGSR  IndTotMittente;
170900131009
171000131009         clear TISI95DS;
171100151009         clear FNLV13DS;
171200131204         tisi95ds.I95tcn = '7';
171300131204         tisi95ds.I95nar = fiora0O0.vaonar;
171400131204         tisi95ds.I95cap = fiora0O0.vaocar;
171500131204         tisi95ds.I95loc = fiora0O0.vaolor;
171600131204         tisi95ds.I95prv = fiora0O0.vaoprr;
171700131204         tisi95ds.I95ind = fiora0O0.vaoinr;
171800131204         tisi95ds.I95dat = %dec(%date());
171900131204         tisi95ds.I95lkg = wPeso;
172000151008         tisi95ds.I95lmc = wVolume;
172100151009         fnlv13ds.I13af0 = 'S';
172200151009         fnlv13ds.I13cnv = 'S';
172300151009         fnlv13ds.I13la3 = 'S';
172400131230
172500131230       //?Se richiesto ntw DPD lo passo
172600160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
172700131230           tisi95ds.I95fi1 = fiora0I0.sceltantw;
172800131230         ENDIF;
172900131009
173000131009       //?Solo per richiamo da Internet
173100131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
173200131009         //?se richiesta nazione irlanda forzo cap eire
173300131204           IF  tisi95ds.I95nar = FIORA00_IRLANDA;
173400131204             tisi95ds.I95cap =  FIORA00_EIRE;
173500131009           ENDIF;
173600131205         ENDIF;
173700131205
173800151009       //?controllo cap/località/provincia
173900151009         fnlv13r (kpjba:fnlv13ds:tisi95ds);
174000160525       //?Solo per richiamo da Internet
174100160525       //?controllo gli errori, da AS ci pensa già il programma chiamante
174200160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
174300151016       //?se torna errore per cap generico diversifico il messaggio
174400151016         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
174500151016             %scan('generico':fnlv13ds.O13msg) > 0;
174600151016           wIdMsg   = 'TIS0826';
174700151016           wIdCampo = 'VAOCAR';
174800151016           clear wParm;
174900151016           exsr Messaggi;
175000151016           wErrMitt = *on;
175100151016           leavesr;
175200151016         ENDIF;
175300151009       //?se torna errore imposto messaggio generico
175400151009         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
175500151016           wIdMsg   = 'TIS0056';
175600131205           wIdCampo = 'VAOLOR';
175700131205           clear wParm;
175800131205           exsr Messaggi;
175900131219           wErrMitt = *on;
176000131205           leavesr;
176100131205         ENDIF;
176200160525         ENDIF;
176300131009
176400131009       //?Imposto la linea di instradamento
176500131204         wFiliale = tisi95ds.O95lna;
176600160315
176700160315       //?Imposto la pickup area
176800160315         wPickup = tisi95ds.O95gf2;
176900131009
177000131009       ENDSR;
177100140311
177200140311       //--------------------------------------------------------------
177300140311       //?Controllo chi paga.
177400140311       //--------------------------------------------------------------
177500140311       BEGSR  ChiPaga;
177600140311
177700140311       //?Controllo formale del dato
177800140401         IF  fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE and
177900140401             fiora0I0.vaopag <> FIORA00_PAGA_ORDINANTE and
178000140401             fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
178100140311           wIdMsg   = 'TIS0140';
178200140311           wIdCampo = 'VAOPAG';
178300140311           clear wParm;
178400140311           exsr Messaggi;
178500140311           leavesr;
178600140311         ENDIF;
178700140311
178800140311       //?Solo per richiamo da Internet
178900140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
179000140311         //?Se utente loggato e ritiro presso altro luogo
179100140311         //?il pagamento non può essere a carico del mittente
179200140401           IF  fiora0I0.IdUtente > 0 and fiora0I0.vaocra = 0 and
179300140401               fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
179400140311             wIdMsg   = 'TIS0140';
179500140311             wIdCampo = 'VAOPAG';
179600140311             clear wParm;
179700140311             exsr Messaggi;
179800140311           ENDIF;
179900140311         ENDIF;
180000140311
180100140311       //?Ritiro all'estero non può pagare il mittente
180200140401         IF  fiora0I0.vaopag = FIORA00_PAGA_MITTENTE and
180300140402             wNazione <> *blanks and
180400140402             wNazione <> FIORA00_SANMARINO;
180500140311           wIdMsg   = 'TIS0695';
180600140311           wIdCampo = 'VAOPAG';
180700140311           clear wParm;
180800140311           exsr Messaggi;
180900140402         ENDIF;
181000140311
181100140311       //?Solo per richiamo da Internet
181200140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
181300140311       //?Imposto se prepagato
181400140401           IF  fiora0I0.vaocor = *zeros and
181500140401               fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
181600140311             fiora0O0.prepagato = FIORA00_PREPAGATO;
181700140311           ENDIF;
181800140311         ENDIF;
181900160607
182000160607       //?Solo per richiamo da AS400
182100160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
182200160607         //?Se ORM prepagato e non è ritiro all'estero
182300160607         //?deve pagare il Mittente
182400160607           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
182500160607               wNazione = *blanks and
182600160607               fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE;
182700160607             wIdMsg   = 'TIS0804';
182800160607             wIdCampo = 'VAOPAG';
182900160607             clear wParm;
183000160607             exsr Messaggi;
183100160607           ENDIF;
183200160607         //?Se ORM singolo e paga Destinatario
183300160607           IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO and
183400160607               fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
183500160607           //?il destinatario deve essere presente
183600160607             IF  fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
183700160607               wIdMsg   = 'TIS0805';
183800160607               wIdCampo = 'VAOCRC';
183900160607               clear wParm;
184000160607               exsr Messaggi;
184100160607             ENDIF;
184200160607           //?il destinatario se codificato non può essere 8888 o 9999
184300160607             IF  fiora0I0.vaocrc > 0 and
184400160607                (%subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '8888' or
184500160607                 %subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '9999');
184600160607               wIdMsg   = 'TIS0806';
184700160607               wIdCampo = 'VAOCRC';
184800160607               clear wParm;
184900160607               exsr Messaggi;
185000160607             ENDIF;
185100160607           ENDIF;
185200160607         //?Se Paga Ordinante
185300160607         //?l'ordinante deve essere un codificato
185400160607         //?e non può essere un 8888 o 9999
185500160607           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
185600170505              (fiora0I0.vaocor = 0 or
185700170505               %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
185800160607               %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999');
185900160607             wIdMsg   = 'TIS0807';
186000160607             wIdCampo = 'VAOCOR';
186100160607             clear wParm;
186200160607             exsr Messaggi;
186300160607           ENDIF;
186400160607         ENDIF;
186500140311
186600140311       ENDSR;
186700140404
186800140404       //--------------------------------------------------------------
186900140404       //?Controllo commissionato.
187000140404       //--------------------------------------------------------------
187100140404       BEGSR  Commissionato;
187200140404
187300140407         wCom = *off;
187400140404
187500140404       //?Solo per richiamo da Internet
187600140404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
187700140404
187800140404         //?Contatto telefonico
187900140404           exec sql
188000140404           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
188100140404
188200140404         //?valido solo se Blank, N o S
188300140404           IF  fiora0O0.contattot <> *blanks and
188400160714               fiora0O0.contattot <> FIORA00_SI and
188500160714               fiora0O0.contattot <> FIORA00_NO;
188600140404             wIdMsg   = 'TIS0720';
188700140404             wIdCampo = 'CONTATTOT';
188800140404             clear wParm;
188900140404             exsr Messaggi;
189000140404           ENDIF;
189100140404
189200140404         //?Ordinante non codificato (è utente senza PSW)
189300160315         //?se il flag non è già impostato
189400140404         //?è sempre commissionato
189500140404           IF  fiora0I0.vaocor = 0;
189600140404             IF  fiora0O0.contattot = *blanks;
189700160714               fiora0O0.contattot = FIORA00_SI;
189800140404             ENDIF;
189900140404           ENDIF;
190000140404
190100160714           IF  fiora0O0.contattot = FIORA00_NO;
190200140407             wCom = *off;
190300140404           ENDIF;
190400160714           IF  fiora0O0.contattot = FIORA00_SI;
190500140407             wCom = *on;
190600140404           ENDIF;
190700140404         ENDIF;
190800160608
190900170601       //?Solo per immissione manuale ORM
191000170601       //?o per conferma ORM FISSI
191100170601         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
191200170601             prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
191300160610         //?se non già presente calcolo il flag di commissionato
191400170530           IF   fiora0I0.Contattot = *blanks;
191500160610           //?commissionato se:
191600160610           //?--> codice mittente <> codice ordinante (primi 7 byte)
191700170601           //?    no se ORM FISSO
191800160610           //?--> Ordinante impostato ma non codificato o 8888 o 9999
191900160610           //?--> Filiale emissione <> filiale ritiro e mittente non codificato
192000160610             IF  (%subst(%editc(fiora0I0.vaocra:'X'):1:7) <>
192100160610                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) and
192200170601                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) > *zeros and
192300170601                  prmRqsOpCode <> FIORA00_RQSOPCODE_CONFERMA_FISSI) or
192400160610                 (fiora0I0.vaorso <> *blanks and
192500160610                 (fiora0I0.vaocor = 0 or
192600160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
192700160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999'));
192800160714               fiora0I0.Contattot = FIORA00_SI;
192900160610             ENDIF;
193000160610             IF  fiora0I0.Contattot = *blanks and
193100160610    1            fiora0I0.vaopor <> fiora0I0.vaopoe and
193200160610                 fiora0I0.vaocra = 0;
193300160714               fiora0I0.Contattot = FIORA00_SI;
193400160610             ENDIF;
193500160610           ENDIF;
193600170522
193700170522           //?Se ORM prepagato NON locale l'ORM deve essere un commissionato
193800170522           IF  fiora0I0.contattot <> 'S' and
193900170522               fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
194000170522               fiora0O0.vaopor <> fiora0I0.fgs;
194100170522             wIdMsg   = 'TIS0718';
194200170522             wIdCampo = 'CONTATTOT';
194300170522             wParm = 'ATTENZIONE!! ORM PREPAGATO non locale';
194400170522             wParm = %trim(wParm) + '. Deve essere un ORM commissionato';
194500170522             exsr Messaggi;
194600170522           ENDIF;
194700160608
194800160610           exec sql
194900160610           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
195000160610
195100160714           IF  fiora0O0.contattot = FIORA00_SI;
195200160610             wCom = *on;
195300160610           ENDIF;
195400160608         ENDIF;
195500140404
195600140404       ENDSR;
195700140404
195800140404       //--------------------------------------------------------------
195900140404       //?Cerco orari servizio.
196000140404       //--------------------------------------------------------------
196100140404       BEGSR  OrariServizio;
196200140408
196300140408         wPor  = fiora0O0.vaopor;
196400140408         wData = inv_dataritiro;
196500140408
196600140408       //?Richiamo il pgm TRULORSR
196700140408         exsr CallTrulORS;
196800140404
196900140404       //?Orari Standard Servizio
197000140404         fiora0O0.orainizio = trulor2ds.OOR2stis;
197100140404         fiora0O0.orafine   = trulor2ds.OOR2stfs;
197200140404
197300140404       //?Orari Limiti
197400140404         fiora0O0.oracomm = trulor2ds.OOR2lrsc;
197500140404         fiora0O0.oranocomm = trulor2ds.OOR2lrnc;
197600170404
197700170404       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
197800170404       //?non è da fare per i ritiri esteri
197900170404       //?non è da fare se non ho gli orari minimo e massimo
198000170404         IF  wPeso > 0 and wPeso < dDFT.d§DFTpkgdt and wNazione = *blanks and
198100170413             trulor2ds.OOR2miis > *zeros and trulor2ds.OOR2mxfs > *zeros;
198200170404           fiora0O0.orainizio = trulor2ds.OOR2miis;
198300170404           fiora0O0.orafine   = trulor2ds.OOR2mxfs;
198400170426           fiora0O0.oraestesa = 'S';
198500170404         ENDIF;
198600160526
198700160526       //?Se ritiro estero chiamo nuovo pgm per ricerca orari servizio
198800160526         IF  fiora0O0.vaonar <> *blanks;
198900160526           exsr CallFIORE;
199000160526         //?Orari Standard Servizio
199100160526           fiora0O0.orainizio = fiore00ds.OORE00min;
199200160526           fiora0O0.orafine   = fiore00ds.OORE00max;
199300160526         //?Orari Limiti
199400160526           fiora0O0.oracomm   = fiore00ds.OORE00lrg;
199500160526           fiora0O0.oranocomm = fiore00ds.OORE00lrg;
199600160526         ENDIF;
199700140404
199800140404       ENDSR;
199900131205
200000131205       //--------------------------------------------------------------
200100131205       //?Controllo filiale emissione.
200200131205       //--------------------------------------------------------------
200300131205       BEGSR  FilEmissione;
200400131205
200500131205       //?Se richiamo da Internet
200600131205         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
200700131205         //?Se ordinante codificato (è utente con PSW)
200800131205         //?e mittente non codificato
200900131205         //?filiale emissione del codice ordinante
201000140401           IF  fiora0I0.vaocor > 0 and fiora0I0.vaocra = 0;
201100131205             fiora0O0.vaopoe =
201200140401             %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
201300131205           ENDIF;
201400131205         //?Se ordinante non codificato (è utente senza PSW)
201500131205         //?filiale emissione da instradamento
201600140401           IF  fiora0I0.vaocor = 0;
201700131205             fiora0O0.vaopoe = wFiliale;
201800131205           ENDIF;
201900131205         ENDIF;
202000140402
202100140402         //?Se mittene codificato filiale emissione da FNACR
202200160519         //?solo se richiamo da internet
202300160519         IF  fiora0I0.vaocra > 0 and fiora0O0.vaopoe = 0 and
202400160519             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
202500140711           fiora0O0.vaopoe = fiorb0OR.filemi;
202600140402         ENDIF;
202700160315
202800160315       //?Se richiamo da AS400
202900160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
203000160315           fiora0O0.vaopoe = fiora0I0.vaopoe;
203100160524         //?Carico tabella DFT ORM della filiale emissione
203200160524         //?se c'è
203300160524           k_TBEcod = 'DFT';
203400160524           k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
203500160524           chain (k_TBEcod:k_TBEke1) TNTBE01L;
203600160524           IF  %found(TNTBE01L) and TBEatb = *blanks;
203700160524             dDFT = TBEuni;
203800160524           ENDIF;
203900160315         ENDIF;
204000131205
204100131205       //?Cerco il network della filiale emissione
204200131219         IF  fiora0O0.vaopoe > *zeros;
204300160315           wNetworkEmi = GetNtwFiliale(fiora0O0.vaopoe);
204400131219         ENDIF;
204500131205
204600131205       ENDSR;
204700131205
204800131205       //--------------------------------------------------------------
204900131205       //?Controllo filiale ritiro.
205000131205       //--------------------------------------------------------------
205100131205       BEGSR  FilRitiro;
205200131205
205300131205         wOKfilritiro = *off;
205400131205
205500131205       //?Cliente mittente codificato
205600131205       //?la filile ritiro è già calcolata
205700140401         IF  fiora0I0.vaocra > 0;
205800140402           wFiliale = fiorb0OR.filrit;
205900131205           wOKfilritiro = *on;
206000131205         ENDIF;
206100131205
206200131205       //?Calcolo la filiale ritiro
206300151008       //?Se cliente mittente NON codificato
206400151008       //?calcolo la filiale ritiro in base al cappario
206500131205         IF  not wOKfilritiro;
206600151008           exsr IndTotMittente;
206700131205         ENDIF;
206800131205
206900131205       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
207000131205         clear FIOR96ds;
207100160527         fior96ds.IOR96poe = fiora0O0.vaopoe;
207200131205         fior96ds.IOR96por = wFiliale;
207300131205         %subst(kpjba:92:10) = 'GAITRA201';
207400131205         fior96r (kpjba : fior96ds);
207500131205         IF  fior96ds.OOR96err = *blanks and fior96ds.OOR96por <> *zeros;
207600131205           wFiliale = fior96ds.OOR96por;
207700160601         //?In caso di richiamo da AS400
207800160601         //?pulisco il campo della filiale ritiro passato
207900160601         //?in questo modo forzo sempre anche se impostata a mano.
208000160601           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
208100160601             clear fiora0I0.vaopor;
208200160601           ENDIF;
208300131205         ENDIF;
208400160525
208500160525       //?In caso di richiamo in WRITE (Immissione/Copia ORM)
208600170530       //?o di conferma ORM FISSI
208700170530         IF  (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
208800170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
208900160525             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
209000160525       //?Se la filale ritiro è una filiale gestita devo cercare le filiale Mamma
209100160525       //?per impostarla come filiale ritiro
209200160525           clear FNLV55DS;
209300160525           fnlv55ds.D55tpt = '6';
209400160525           fnlv55ds.D55lin = wFiliale;
209500160525           fnlv55ds.D55drf = %dec(%date());
209600160525           fnlv55r (FNLV55DS);
209700160525           IF  fnlv55ds.D55err = *blanks;
209800160525             wFiliale = fnlv55ds.D55tfa;
209900160525           ENDIF;
210000160525         ENDIF;
210100131205
210200160525       //?Se richiamo da Internet
210300160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
210400131205         fiora0O0.vaopor = wFiliale;
210500160525         ENDIF;
210600160404
210700160404       //?Se richiamo da AS400 e la filiale ritiro non è stata passata
210800160404       //?la imposto ora
210900160404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
211000160404             fiora0I0.vaopor = 0;
211100160404           fiora0I0.vaopor = wFiliale;
211200160525           fiora0O0.vaopor = wFiliale;
211300160404         ENDIF;
211400131205
211500131205       //?Imposto se ritiro all'estero
211600160315         wNetworkRit = GetNtwFiliale(fiora0O0.vaopor);
211700131205         fiora0O0.ritiroest = FIORA00_RITIROESTERO_NO;
211800131205         clear dNTW;
211900131205         k_TBEcod = 'NTW';
212000160315         k_TBEke1 = wNetworkRit;
212100131205         chain (k_TBEcod:k_TBEke1) TNTBE01L;
212200131205         IF  %found(TNTBE01L);
212300131205           dNTW = TBEuni;
212400131205         ENDIF;
212500160315         IF  wNetworkRit = FIORA00_NTW_DPD or
212600131205             dntw.§NTWfie = FIORA00_NTW_ESTERO;
212700131205           fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
212800131205         ENDIF;
212900131205
213000131205       //?Verifico la ritirabilità all'estero
213100131205         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
213200140402             wNazione <> *blanks;
213300131205           clear FNLV12DS;
213400131205           clear TISI95DS;
213500131205           clear TISI97DS;
213600131230           fnlv12ds.I12lang = fiora0I0.idlingua;
213700140402           tisi95ds.I95nar = wNazione;
213800140402           tisi95ds.I95cap = wCap;
213900131205           tisi95ds.I95dat = dataoggi;
214000131205           tisi97ds.I97ntw = wNetworkRit;
214100160315           IF  wNetworkRit = FIORA00_NTW_DPD;
214200160315             tisi95ds.I95fi1 = FIORA00_SERVIZIO_DPD;
214300131205           ENDIF;
214400131205           fnlv12r (fnlv12ds:tisi95ds:tisi97ds);
214500131205           IF  fnlv12ds.O12err <> *blanks;
214600140304             wIdMsg   = 'TIS0718';
214700160315           //?Solo per richiamo da Internet
214800160315             IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
214900131205             wIdCampo = 'SCELTANTW';
215000160315             ELSE;
215100160315               wIdCampo = 'VAOPOR';
215200160315             ENDIF;
215300131205             wParm = fnlv12ds.O12msg;
215400131205             exsr Messaggi;
215500170404             wErr = *on;
215600131205           ENDIF;
215700131227
215800131227         //?Imposto il ntw da forzare per ritiro all'estero
215900131227           IF  fiora0I0.sceltantw <> *blanks;
216000131227             fiora0O0.sceltantw = fiora0I0.sceltantw;
216100131227             fiora0O0.forzantw = fiora0I0.sceltantw;
216200131227           ELSE;
216300160315             IF  wNetworkRit = FIORA00_NTW_DPD;
216400131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
216500131227             ENDIF;
216600160315             IF  wNetworkRit = FIORA00_NTW_EEX;
216700131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
216800131227             ENDIF;
216900131227           ENDIF;
217000131205
217100131227         ENDIF;
217200160525
217300160525       //?Se richiamato da AS400
217400160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
217500160525         //?Imposto il ntw da forzare per ritiro all'estero
217600160525           IF  fiora0I0.sceltantw <> *blanks;
217700160525             fiora0O0.sceltantw = fiora0I0.sceltantw;
217800160525             fiora0O0.forzantw = fiora0I0.sceltantw;
217900160525           ELSE;
218000160525             IF  wNetworkRit = FIORA00_NTW_DPD;
218100160525               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
218200160525             ENDIF;
218300160525             IF  wNetworkRit = FIORA00_NTW_EEX;
218400160525               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
218500160525             ENDIF;
218600160525           ENDIF;
218700160526         //?In caso di richiamo in WRITE (Immissione/Copia ORM)
218800170530         //?o di conferma ORM FISSI
218900170530           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
219000170530               prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
219100160526         //?Messaggio di avvertimento se filiale ritiro calcolata <> da quella del video
219200160526             IF  wFiliale > 0 and fiora0I0.vaopor > 0 and
219300160526                 wFiliale <> fiora0I0.vaopor;
219400160526               wIdMsg   = 'TIS0833';
219500160526               wIdCampo = 'VAOPOR';
219600160526               clear wParm;
219700160526               exsr Forzatura;
219800160531               IF  wForza;
219900160531                 exsr RoutEnd;
220000160531               ENDIF;
220100160526             ENDIF;
220200160526           ENDIF;
220300160525         ENDIF;
220400151008
220500151120         //?Se ordinante NON codificato quindi utente senza PSW e
220600151008         //?Se mittene NON codificato reimposto la filiale emissione
220700151008         //?la filiale ritiro potrebbe essere variata a causa delle quantità
220800151008         //?inserite
220900160601         //?SOLO se richiamato da Internet
221000160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
221100151120         IF  not wOKfilritiro and fiora0I0.vaocor = 0;
221200151008           fiora0O0.vaopoe = wFiliale;
221300151008         ENDIF;
221400160601         ENDIF;
221500160606
221600160606       //?Dalla versione 'D' della ds di output FIORA0O0
221700160606       //?torno la filiale ritiro calcolata
221800160620       //?e il pickup
221900160728       //?solo per AS400
222000160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
222100160606         IF  fiora0O0.versione > 'C';
222200160606           fiora0O0.filritcalc = wfiliale;
222300160620           fiora0O0.pickupcalc = wpickup;
222400160620         //?se cliente codificato cerco il pickup
222500160620           IF  wOkfilritiro;
222600160620             exsr IndTotMittente;
222700160620             fiora0O0.pickupcalc = wpickup;
222800160620           ENDIF;
222900160606         ENDIF;
223000160728         ENDIF;
223100170404
223200170404       //?solo per AS400
223300170418         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
223400170418         //?per errore sul calcolo dell'instradamento
223500170418           IF  wErrMitt;
223600170418           //?imposto flag di errore
223700170418             wErr = *on;
223800170418           ENDIF;
223900170418         //?Cerco i valori per impostazione ambiti giri ritiro
224000170418           wOKagr = *off;
224100170418           clear dAGR;
224200170418           k_TBEcod = 'AGR';
224300170418           k_TBEke1 = %editc(fiora0I0.vaopor:'X');
224400170418           chain (k_TBEcod:k_TBEke1) TNTBE01L;
224500170418           IF  %found(TNTBE01L);
224600170418             dAGR = TBEuni;
224700170418             wOKagr = *on;
224800170418           ENDIF;
224900170404         ENDIF;
225000131227
225100131205       ENDSR;
225200160315
225300160315       //--------------------------------------------------------------
225400160315       //?Controllo filiale ritiro.
225500160315       //--------------------------------------------------------------
225600160601       BEGSR  ControllaFilRitiro;
225700160315
225800160315       //?Filiale ritiro obbligatoria
225900160315         IF  fiora0I0.vaopor = 0;
226000160315           wIdMsg   = 'TIS0809';
226100160315           wIdCampo = 'VAOPOR';
226200160315           clear wParm;
226300160315           exsr Messaggi;
226400160315           leavesr;
226500160315         ENDIF;
226600160315
226700160601       //?Controllo se la filiale ritiro è valida
226800160601         IF  not IsFilValida(fiora0O0.vaopor);
226900160601           wIdMsg   = 'TIS0809';
227000160601           wIdCampo = 'VAOPOR';
227100160601           clear wParm;
227200160601           exsr Messaggi;
227300160601           leavesr;
227400160315         ENDIF;
227500160315       //?Errore se la filiale ritiro NON ha la procedura ORM attiva
227600160315         IF  not IsFilAttivaORM(fiora0O0.vaopor);
227700160607           wIdMsg   = 'TIS0836';
227800160315           wIdCampo = 'VAOPOR';
227900160607           clear wParm;
228000160315           exsr Messaggi;
228100160315         ENDIF;
228200160315
228300160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
228400170530       //?o di conferma ORM FISSI
228500160315       //?controllo congruenza tra network DPD e filiale ritiro
228600170530         IF  (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
228700170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
228800160315             wNetworkRit <> FIORA00_NTW_DPD and
228900160315             fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
229000160607           wIdMsg   = 'TIS0837';
229100160315           wIdCampo = 'VAOPOR';
229200160607           clear wParm;
229300160315           exsr Messaggi;
229400160315         ENDIF;
229500160315
229600160315       //?Controllo con cappario DPD se ritiro per DPD
229700160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD or
229800160315             wNetworkRit = FIORA00_NTW_DPD;
229900160315           IF  not IsValidoCapDPD(fiora0O0.vaonar:
230000160315                                  fiora0O0.vaocar:
230100160315                                  fiora0O0.vaodar:
230200160315                                  fiora0I0.vaopkg:
230300160315                                  ds3idp.§3Ilmp:
230400160315                                  fiora0O0.vaopoe);
230500160315             wIdMsg   = 'TIS0811';
230600160315             wIdCampo = 'VAOCAR';
230700160315             clear wParm;
230800160315             exsr Messaggi;
230900160315           ENDIF;
231000160315         ENDIF;
231100160315
231200160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
231300170530       //?o di conferma ORM FISSI
231400160315       //?e filiale ritiro = a chi sta inserendo ORM
231500160315       //?non posso fare un prepagato
231600170530         IF  (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
231700170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
231800160315             fiora0O0.vaopor = fiora0I0.fgs and
231900160315             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
232000160607           wIdMsg   = 'TIS0838';
232100160315           wIdCampo = 'VAOTOR';
232200160607           clear wParm;
232300160315           exsr Messaggi;
232400160315         ENDIF;
232500160315
232600160315       //?Non possibile ORM prepagato se ORM Export
232700160315         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
232800160315            (wNetworkRit = FIORA00_SERVIZIO_DPD or
232900160315             wNetworkRit = FIORA00_SERVIZIO_EEX);
233000160607           wIdMsg   = 'TIS0839';
233100160315           wIdCampo = 'VAOTOR';
233200160607           clear wParm;
233300160315           exsr Messaggi;
233400160315         ENDIF;
233500160527
233600160607       //?Se richiesto il ritiro tramite DPD deve essere ORM Singolo
233700160527         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
233800160607             fiora0I0.tor <> FIORA00_TIPOORM_SINGOLO;
233900160527           wIdMsg   = 'TIS0307';
234000160607           wIdCampo = 'VAOTOR';
234100160527           clear wParm;
234200160527           exsr Messaggi;
234300160527         ENDIF;
234400160315
234500160315       ENDSR;
234600170404
234700170404       //--------------------------------------------------------------
234800170404       //?Controllo data pronta merce.
234900170404       //--------------------------------------------------------------
235000170404       BEGSR  DataProntaMerce;
235100170404
235200170404       //?Non accetto a 0
235300170404         IF  fiora0I0.datapm = 0;
235400170404           wIdMsg   = 'TIS0718';
235500170404           wIdCampo = 'DATAPM';
235600170404           wParm = 'Data Pronta Merce errata';
235700170404           exsr Messaggi;
235800170404           wErr = *on;
235900170404           leavesr;
236000170404         ENDIF;
236100170404
236200170404       //?Non deve essere inferiore a 7 gg da oggi
236300170404         IF  fiora0I0.datapm < dataoggi7;
236400170404           wIdMsg   = 'TIS0718';
236500170404           wIdCampo = 'DATAPM';
236600170404           wParm = 'Data Pronta Merce errata. Inferiore a 7 gg da oggi';
236700170404           exsr Messaggi;
236800170404           wErr = *on;
236900170404           leavesr;
237000170404         ENDIF;
237100170404
237200170404       ENDSR;
237300131205
237400131205       //--------------------------------------------------------------
237500131205       //?Controllo data ritiro.
237600131205       //--------------------------------------------------------------
237700131205       BEGSR  DataRitiro;
237800170413
237900170413         PosticipaDataRitiro = *off;
238000131205
238100131223       //?Calcolo la data ritiro
238200170404         clear FIOR97DS;
238300170404         clear FIOR971DS;
238400131205         fior97ds.IOR97poe = fiora0O0.vaopoe;
238500131205         fior97ds.IOR97por = fiora0O0.vaopor;
238600140402         fior97ds.IOR97cap = wCap;
238700140402         fior97ds.IOR97loc = wLocalita;
238800160526         fior97ds.IOR97naz = wNazione;
238900131205         IF  fiora0I0.vaodao = 0;
239000131205           fior97ds.IOR97dao = dataoggi;
239100131205         ELSE;
239200131205           fior97ds.IOR97dao = fiora0I0.vaodao;
239300131205         ENDIF;
239400131205         IF  fiora0I0.vaooao = 0;
239500131205           fior97ds.IOR97oao = %dec(%time());
239600131205         ELSE;
239700131205           fior97ds.IOR97oao = fiora0I0.vaooao;
239800131205         ENDIF;
239900131205         fior97ds.IOR97gf2 = tisi95ds.O95gf2;
240000170404         //?=========>>>>
240100170404         //?Da AS400 può essere con Ricevuta di Ritiro
240200170404         //?=========>>>>
240300131205         IF  wCom;
240400160714           fior97ds.IOR97com = FIORA00_SI;
240500131205         ENDIF;
240600140331         SELECT;
240700140331           WHEN fiora0O0.prepagato = FIORA00_PREPAGATO;
240800160714             fior97ds.IOR97tor = FIORA00_ORM_BOLLAPREPAGATO;
240900140331           WHEN fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
241000160714             fior97ds.IOR97tor = FIORA00_ORM_INTERNET;
241100140331           OTHER;
241200160714             fior97ds.IOR97tor = FIORA00_ORM_MANUALE;
241300140331         ENDSL;
241400131205         IF  fiora0I0.vaocra > 0;
241500131205           fior97ds.IOR97mcod = 'S';
241600131205         ENDIF;
241700160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
241800131205         fior97ds.IOR97web = 'S';
241900160315         ENDIF;
242000160601
242100160601         //?se ORM manuale e ci sono i dati per Alert Commissionato
242200160601         //?e l'ORM è un commissionato passo il flag
242300160601         IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
242400160601              fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and wCom and
242500160601             (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks);
242600160714           fior97ds.IOR97alert = FIORA00_SI;
242700160601         ENDIF;
242800170404
242900170404         //?Dalla Versione "E"
243000170404         IF  fiora0I0.versione > 'D';
243100170404           //?Passo il peso 'esploso'
243200170404           fior971ds.IOR97pkg = wPeso;
243300170404           //?Passo se Ordinante codificato
243400170623           //?no se codice Yoox
243500170623           IF  fiora0I0.VAOcor > 0 and fiora0I0.VAOcor <> 0017732000;
243600170404             fior971ds.IOR97ocod = 'S';
243700170404           ENDIF;
243800170404           //?Passo la Data Pronta Merce
243900170404           fior971ds.IOR97dpm = fiora0I0.datapm;
244000170404         ENDIF;
244100160601
244200170404       //?Dalla Versione "E"
244300170404         IF  fiora0I0.versione > 'D';
244400170404         //?passo anche la ds nuova
244500170404           fior97r(kpjba : fior97ds : fior971ds);
244600170404         ELSE;
244700131205         fior97r(kpjba : fior97ds);
244800170404         ENDIF;
244900160601
245000131205         IF  fior97ds.OOR97err = *blanks and fior97ds.OOR97dar <> *zeros;
245100131220           inv_dataritiro = fior97ds.OOR97dar;
245200131205         ENDIF;
245300170404
245400170404       //?Dalla Versione "E"
245500170404         IF  fiora0O0.versione > 'D';
245600170404         //?Ritorno al chiamante le date:
245700170404         //?Ritiro calcolata - Minimo e Massimo consentite
245800170404           fiora0O0.dataritiro = fior97ds.OOR97dar;
245900170404           fiora0O0.dataritmin = fior97ds.OOR97dmin;
246000170404           fiora0O0.dataritmax = fior97ds.OOR97dmaxb;
246100170413         //?Mi memorizzo se devo posticipare
246200170413           IF  fior971ds.OOR97postd = 'S';
246300170413             PosticipaDataRitiro = *on;
246400170620             fiora0O0.posticipo = 'S';
246500170413           ENDIF;
246600170404         ENDIF;
246700131205
246800160610         //?Se richiamato da INTERNET
246900160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
247000131220         //?se non è stata immessa imposto quella calcolata
247100131220         IF  fiora0I0.vaodar = 0;
247200131220           fiora0O0.vaodar = inv_dataritiro;
247300131220         ELSE;
247400131220           fiora0O0.vaodar = fiora0I0.vaodar;
247500131220         ENDIF;
247600131223
247700131223         //?Controlli sulla data
247800131223         //?Non accetto a 0
247900131223         IF  fiora0O0.vaodar = 0;
248000140407           wIdMsg   = 'TIS0259';
248100131223           wIdCampo = 'VAODAR';
248200131223           clear wParm;
248300131223           exsr Messaggi;
248400131223           leavesr;
248500131223         ENDIF;
248600131223         //?Non deve essere inferiore alla data minima
248700131223         IF  fiora0O0.vaodar < fior97ds.OOR97dmin;
248800140407           wIdMsg   = 'TIS0730';
248900131223           wIdCampo = 'VAODAR';
249000140407           IF  wCom;
249100140407             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
249200140407           ELSE;
249300140407             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
249400140407           ENDIF;
249500160526           IF  fiora0I0.vaonar <> *blanks;
249600160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
249700160526           ENDIF;
249800131223           exsr Messaggi;
249900131223         ENDIF;
250000131223         //?Non deve essere superiore alla data massima
250100131223         IF  fiora0O0.vaodar > fior97ds.OOR97dmaxb;
250200131223           wIdMsg   = 'TIS0116';
250300131223           wIdCampo = 'VAODAR';
250400131223           clear wParm;
250500131223           exsr Messaggi;
250600131223         ENDIF;
250700170620         //?Mi memorizzo se posso anticipare
250800170620         //?da WEB passo 'S' --> quello che torna dalla DS FIOR971DS
250900170620           fiora0O0.anticipa = fior971ds.OOR97antd;
251000160610         ENDIF;
251100160610
251200160610       //?Se richiamato da AS400
251300160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
251400160610         //?Se sono in immissione ORM
251500170530         //?o in conferma ORM FISSI
251600170530           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
251700170530               prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
251800160610           //?se posso anticipare la data passo il flag
251900160610             IF  fior97ds.OOR97dmin < fior97ds.OOR97dar;
252000160714               fiora0O0.anticipa = FIORA00_ANTICIPA;
252100160610             ENDIF;
252200160610             //?Se non passata imposto la data calcolata
252300160610             IF  fiora0I0.vaodar = 0;
252400170404               fiora0I0.vaodar = fior97ds.OOR97dar;
252500160610             ENDIF;
252600160610           ENDIF;
252700170404           fiora0O0.vaodar = fiora0I0.vaodar;
252800160610           //?In caso di richiamo in WRITE (Immissione/Copia ORM)
252900170530           //?o di conferma ORM FISSI
253000170530           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
253100170530               prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI;
253200160610             //?Se posso anticipare ma la data ritiro immessa è = alla
253300160610             //?data minima pulisco il flag di anticipa
253400160714             IF  fiora0O0.anticipa = FIORA00_ANTICIPA and
253500160610                 fior97ds.OOR97dmin = fiora0I0.vaodar;
253600160610               clear fiora0O0.anticipa;
253700160610             ENDIF;
253800160610           ENDIF;
253900160610         ENDIF;
254000131205
254100131205       ENDSR;
254200131011
254300131011       //--------------------------------------------------------------
254400131011       //?Controllo ora pronta merce.
254500131011       //--------------------------------------------------------------
254600131011       BEGSR  OraRitiro;
254700131219
254800131223         IF  fiora0I0.vaoorr > 0;
254900131223           fiora0O0.vaoorr = fiora0I0.vaoorr;
255000131223         ENDIF;
255100131028
255200140407         IF  fiora0O0.vaoorr = *zeros and not wCom;
255300131028           wIdMsg   = 'TIS0423';
255400131028           wIdCampo = 'VAOORR';
255500131204           clear wParm;
255600131028           exsr Messaggi;
255700131219           leavesr;
255800131028         ENDIF;
255900131028
256000140407         IF  wCom and fiora0O0.vaoorr = 0;
256100131220           leavesr;
256200131220         ENDIF;
256300131220
256400131223         IF  fiora0O0.vaoorr < 1 or fiora0O0.vaoorr > 2359;
256500131028           wIdMsg   = 'TIS0425';
256600131028           wIdCampo = 'VAOORR';
256700131204           clear wParm;
256800131028           exsr Messaggi;
256900131028         ENDIF;
257000160404
257100160404       ENDSR;
257200160404
257300160404       //--------------------------------------------------------------
257400160404       //?Controllo ora pronta merce con orari servizio.
257500160404       //--------------------------------------------------------------
257600160404       BEGSR  OraRitiroServizio;
257700140306
257800140306       //?Il controllo dell'ora pronta merce non lo faccio se ritiro estero
257900140306         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
258000140306           leavesr;
258100140306         ENDIF;
258200131220
258300131220       //?Controllo ora pronta merce se non è presa da anagrafica clienti
258400131220         SELECT;
258500140402           WHEN  fiorb0OR.oraritiro > 0 and
258600140402                 fiora0O0.vaoorr = fiorb0OR.oraritiro;
258700131220           OTHER;
258800131220       //?Controllo ora pronta merce con ora fine standard
258900140331           IF  fiora0O0.vaoorr > trulor2ds.OOR2stfs;
259000140306             wIdMsg   = 'TIS0724';
259100131223             wIdCampo = 'VAOORR';
259200140331             wParm = %editw(trulor2ds.OOR2stis:'  :  ') +
259300140331                     %editw(trulor2ds.OOR2stfs:'  :  ');
259400131223             exsr Messaggi;
259500131223           ENDIF;
259600131220         ENDSL;
259700131220
259800131220       //?Controllo ora immissione ORM con ora massima ritiro
259900140306         IF  %dec(%subst(%editc(fiora0I0.vaooao:'X'):1:4):4:0)
260000140410             > trulor2ds.OOR2stfs and
260100140306             fiora0I0.vaodao = fiora0O0.vaodar;
260200140306           wIdMsg   = 'TIS0721';
260300140306           wIdCampo = 'VAOOAO';
260400140327           wParm = %editw(trulor2ds.OOR2stfs:'  :  ');
260500131223           exsr Messaggi;
260600131220         ENDIF;
260700131011
260800131011       ENDSR;
260900131028
261000131028       //--------------------------------------------------------------
261100131028       //?Controllo orari apertura.
261200131028       //--------------------------------------------------------------
261300140404       BEGSR  OrariApertura;
261400131028
261500140401         IF  fiora0I0.dalleam > 0 or fiora0I0.alleam > 0 or
261600140401             fiora0I0.dallepm > 0 or fiora0I0.allepm > 0;
261700131204           clear TRUL03ds;
261800140401           trul03ds.I03hm1 = fiora0I0.dalleam;
261900140401           trul03ds.I03hm2 = fiora0I0.alleam;
262000140401           trul03ds.I03hm3 = fiora0I0.dallepm;
262100140401           trul03ds.I03hm4 = fiora0I0.allepm;
262200131204           trul03r (TRUL03DS);
262300131204           IF  trul03ds.O03err <> *off;
262400131204             wIdMsg   = 'TIS0171';
262500131204             SELECT;
262600131204               WHEN  trul03ds.O03errpos = 1;
262700131204                 wIdCampo = 'DALLEAM';
262800131204                 clear wParm;
262900131204                 exsr Messaggi;
263000131204               WHEN  trul03ds.O03errpos = 2;
263100131204                 wIdCampo = 'ALLEAM';
263200131204                 clear wParm;
263300131204                 exsr Messaggi;
263400131204               WHEN  trul03ds.O03errpos = 3;
263500131204                 wIdCampo = 'DALLEPM';
263600131204                 clear wParm;
263700131204                 exsr Messaggi;
263800131204               WHEN  trul03ds.O03errpos = 4;
263900131204                 wIdCampo = 'ALLEPM';
264000131204                 clear wParm;
264100131204                 exsr Messaggi;
264200131204             ENDSL;
264300131204           ENDIF;
264400131204         ENDIF;
264500131028
264600131028       ENDSR;
264700131028
264800131028       //--------------------------------------------------------------
264900131028       //?Controllo quantità.
265000131028       //--------------------------------------------------------------
265100131028       BEGSR  Quantita;
265200160613
265300160613         wErrQta = *off;
265400140410
265500140410       //?Se Prepagato obbligo di colli e peso
265600140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
265700140410             fiora0I0.vaoncl = 0;
265800140410           wIdMsg   = 'TIS0726';
265900140410           wIdCampo = 'VAONCL';
266000140410           clear wParm;
266100140410           exsr Messaggi;
266200160613           wErrQta = *on;
266300140410         ENDIF;
266400140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
266500140410             fiora0I0.vaopkg = 0;
266600140410           wIdMsg   = 'TIS0727';
266700140410           wIdCampo = 'VAOPKG';
266800140410           clear wParm;
266900140410           exsr Messaggi;
267000160613           wErrQta = *on;
267100140410         ENDIF;
267200140410
267300140410       //?Se richiesto il ritiro tramite DPD
267400140410       //?il peso è obbligatorio e deve essere inferiore al limite da tab. 3I
267500160714         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
267600140410           IF  fiora0I0.vaopkg = 0;
267700140410             wIdMsg   = 'TIS0727';
267800140410             wIdCampo = 'VAOPKG';
267900140410             clear wParm;
268000140410             exsr Messaggi;
268100160613             wErrQta = *on;
268200140410           ENDIF;
268300140410           IF  fiora0I0.vaopkg > 0 and ds3idp.§3Ipkd > 0 and
268400140410               fiora0I0.vaopkg > ds3idp.§3Ipkd;
268500140410             wIdMsg   = 'TIS0694';
268600140410             wIdCampo = 'VAOPKG';
268700140410             wParm = %editc(ds3idp.§3Ipkd:'3');
268800140410             exsr Messaggi;
268900160613             wErrQta = *on;
269000140410           ENDIF;
269100140410       //?i colli sono obbligatori e deve essere impostato a 1
269200140410           IF  fiora0I0.vaoncl <> 1;
269300140410             wIdMsg   = 'TIS0693';
269400140410             wIdCampo = 'VAONCL';
269500140410             clear wParm;
269600140410             exsr Messaggi;
269700160613             wErrQta = *on;
269800140410           ENDIF;
269900140410         ENDIF;
270000160922
270100160922       //?Controllo subito i limiti delle quantità
270200160922         exsr LimitiQta;
270300160922       //?Se da AS400 e c'è errore esco
270400160922         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
270500160922             wErrQta;
270600160922           leavesr;
270700160922         ENDIF;
270800160922
270900140410       //?Se Prepagato finisco qua i controlli
271000140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO;
271100140410           leavesr;
271200140410         ENDIF;
271300160530
271400160601       //?Faccio controlli diversificati tra Internet ed AS400
271500140331
271600160601       //?Controlli per richiamo da internet
271700160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
271800160601
271900140331       //?Almeno 1 tra colli - bancali deve essere presente
272000140401         IF  fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
272100140331           wIdMsg   = 'TIS0729';
272200140331           wIdCampo = 'VAONCL';
272300140331           clear wParm;
272400140331           exsr Messaggi;
272500140331         ENDIF;
272600140331
272700140331       //?Peso obbligatorio
272800140429         IF  fiora0I0.vaopkg = 0;
272900140429           wIdMsg   = 'TIS0727';
273000140429           wIdCampo = 'VAOPKG';
273100140429           clear wParm;
273200140429           exsr Messaggi;
273300140429         ENDIF;
273400131204
273500131204       //?Controllo con i flag su Anagrafica clienti ritiro
273600140331       //?Per internet facciamo solo i controlli di 'O' obbligatorio
273700160714           IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO and
273800160714               fiora0I0.vaoncl = 0;
273900140331             wIdMsg   = 'TIS0726';
274000140331             wIdCampo = 'VAONCL';
274100140331             clear wParm;
274200140331             exsr Messaggi;
274300140331           ENDIF;
274400140331
274500160714           IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO and
274600160714               fiora0I0.vaopkg = 0;
274700140331             wIdMsg   = 'TIS0727';
274800140331             wIdCampo = 'VAOPKG';
274900140331             clear wParm;
275000140331             exsr Messaggi;
275100140331           ENDIF;
275200140331
275300160714           IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO and
275400160714               fiora0I0.vaovlm = 0;
275500140331             wIdMsg   = 'TIS0728';
275600140331             wIdCampo = 'VAOVLM';
275700140331             clear wParm;
275800140331             exsr Messaggi;
275900140331           ENDIF;
276000140331
276100160714           IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO and
276200160714               fiora0I0.vaobnc = 0;
276300140331             wIdMsg   = 'TIS0722';
276400140331             wIdCampo = 'VAOBNC';
276500140331             clear wParm;
276600140331             exsr Messaggi;
276700140331           ENDIF;
276800160601
276900140331         ENDIF;
277000160601
277100160601       //?Controlli Se richiamato da AS400
277200160601       //?faccio prima i controlli con i flag dell'anagrafica mittente
277300160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
277400160601         //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
277500160601         //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
277600160601           IF  fiorb0OR.flagfncl <> *blanks and fiora0I0.vaoncl = 0;
277700160601             wIdMsg   = 'TIS0726';
277800160601             wIdCampo = 'VAONCL';
277900160601             clear wParm;
278000160714             IF  fiorb0OR.flagfncl = FIORA00_FORZABILE;
278100160601               exsr Forzatura;
278200160601               IF  wForza;
278300160601                 exsr RoutEnd;
278400160601               ENDIF;
278500160601             ENDIF;
278600160714             IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO;
278700160601               exsr Messaggi;
278800160613               wErrQta = *on;
278900160613               leavesr;
279000160601             ENDIF;
279100160601           ENDIF;
279200160601           IF  fiorb0OR.flagfpkg <> *blanks and fiora0I0.vaopkg = 0;
279300160601             wIdMsg   = 'TIS0727';
279400160601             wIdCampo = 'VAOPKG';
279500160601             clear wParm;
279600160714             IF  fiorb0OR.flagfpkg = FIORA00_FORZABILE;
279700160601               exsr Forzatura;
279800160601               IF  wForza;
279900160601                 exsr RoutEnd;
280000160601               ENDIF;
280100160601             ENDIF;
280200160714             IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO;
280300160601               exsr Messaggi;
280400160613               wErrQta = *on;
280500160613               leavesr;
280600160601             ENDIF;
280700160601           ENDIF;
280800160601           IF  fiorb0OR.flagfvlm <> *blanks and fiora0I0.vaovlm = 0;
280900160601             wIdMsg   = 'TIS0728';
281000160601             wIdCampo = 'VAOVLM';
281100160601             clear wParm;
281200160714             IF  fiorb0OR.flagfvlm = FIORA00_FORZABILE;
281300160601               exsr Forzatura;
281400160601               IF  wForza;
281500160601                 exsr RoutEnd;
281600160601               ENDIF;
281700160601             ENDIF;
281800160714             IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO;
281900160601               exsr Messaggi;
282000160613               wErrQta = *on;
282100160613               leavesr;
282200160601             ENDIF;
282300160601           ENDIF;
282400160601           IF  fiorb0OR.flagfbnc <> *blanks and fiora0I0.vaobnc = 0;
282500160601             wIdMsg   = 'TIS0722';
282600160601             wIdCampo = 'VAOBNC';
282700160601             clear wParm;
282800160714             IF  fiorb0OR.flagfbnc = FIORA00_FORZABILE;
282900160601               exsr Forzatura;
283000160601               IF  wForza;
283100160601                 exsr RoutEnd;
283200160601               ENDIF;
283300160601             ENDIF;
283400160714             IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO;
283500160601               exsr Messaggi;
283600160613               wErrQta = *on;
283700160613               leavesr;
283800160601             ENDIF;
283900160601           ENDIF;
284000160601         //?In caso di Immissione/Copia ORM
284100160601         //?immissione Manuale
284200160601           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
284300160714              (fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL or
284400160714               fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO) and
284500160601           //?Almeno 1 tra colli - bancali deve essere presente
284600160601               fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
284700160601             wIdMsg   = 'TIS0729';
284800160601             wIdCampo = 'VAONCL';
284900160601             clear wParm;
285000160601             exsr Messaggi;
285100160613             wErrQta = *on;
285200160613             leavesr;
285300160601           ENDIF;
285400160601         //?Almeno 1 tra peso volume o bancali deve essere presente
285500160610           IF  fiora0I0.vaobnc = 0 and fiora0I0.vaovlm = 0 and
285600160601               fiora0I0.vaopkg = 0;
285700160601             wIdMsg   = 'TIS0235';
285800160601             wIdCampo = 'VAOPKG';
285900160601             clear wParm;
286000160601             exsr Messaggi;
286100160613             wErrQta = *on;
286200160613             leavesr;
286300160601           ENDIF;
286400160601         ENDIF;
286500131028
286600131028       ENDSR;
286700160922
286800160922       //--------------------------------------------------------------
286900160922       //?Controllo limiti quantità.
287000160922       //--------------------------------------------------------------
287100160922       BEGSR  LimitiQta;
287200160922
287300160922         clear TRUL73DS;
287400160922         trul73ds.I73tsp = 'C';
287500160922         trul73ds.I73ncl = fiora0I0.vaoncl;
287600160922         trul73ds.I73psk = fiora0I0.vaopkg;
287700160922         trul73ds.I73vmc = fiora0I0.vaovlm;
287800160922         trul73r (trul73ds);
287900160922
288000160922       //?Limite colli bloccante
288100160922         SELECT;
288200160922         WHEN  trul73ds.O73fclm = '1';
288300160922           wIdMsg   = 'TIS0850';
288400160922           wIdCampo = 'VAONCL';
288500160922           wParm = %editc(trul73ds.O73lclm:'4');
288600160922           exsr Messaggi;
288700160922           wErrQta = *on;
288800160922       //?Limite peso bloccante
288900160922         WHEN  trul73ds.O73fkgm = '1';
289000160922           wIdMsg   = 'TIS0851';
289100160922           wIdCampo = 'VAOPKG';
289200160922           wParm = %editc(trul73ds.O73lkgm:'4');
289300160922           exsr Messaggi;
289400160922           wErrQta = *on;
289500160922       //?Limite volume bloccante
289600160922         WHEN  trul73ds.O73fmcm = '1';
289700160922           wIdMsg   = 'TIS0852';
289800160922           wIdCampo = 'VAOVLM';
289900160922           wParm = %editc(trul73ds.O73lmcm:'4');
290000160922           exsr Messaggi;
290100160922           wErrQta = *on;
290200160922         ENDSL;
290300160922
290400160922         clear TRUL731DS;
290500160922         trul731ds.I731bnc = fiora0I0.vaobnc;
290600160922         trul731r (trul731ds);
290700160922
290800160922       //?Limite bancali bloccante
290900160922         IF  trul731ds.O731fbncm = '1';
291000160922           wIdMsg   = 'TIS0853';
291100160922           wIdCampo = 'VAOBNC';
291200160922           wParm = %editc(trul731ds.O731lbncm:'4');
291300160922           exsr Messaggi;
291400160922           wErrQta = *on;
291500160922         ENDIF;
291600160922
291700160922       //?Se da AS400 faccio i controlli anche per i limiti forzabili
291800160922         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
291900160922         //?Limite colli forzabile
292000160922           SELECT;
292100160922           WHEN  trul73ds.O73fclf = '1';
292200160922             wIdMsg   = 'TIS0850';
292300160922             wIdCampo = 'VAONCL';
292400160922             wParm = %editc(trul73ds.O73lclf:'4');
292500160922             exsr Forzatura;
292600170427             IF  wForza;
292700170427               wErrQta = *on;
292800170427             ENDIF;
292900160922         //?Limite peso forzabile
293000160922           WHEN  trul73ds.O73fkgf = '1';
293100160922             wIdMsg   = 'TIS0851';
293200160922             wIdCampo = 'VAOPKG';
293300160922             wParm = %editc(trul73ds.O73lkgf:'4');
293400160922             exsr Forzatura;
293500170427             IF  wForza;
293600170427               wErrQta = *on;
293700170427             ENDIF;
293800160922         //?Limite volume forzabile
293900160922           WHEN  trul73ds.O73fmcf = '1';
294000160922             wIdMsg   = 'TIS0852';
294100160922             wIdCampo = 'VAOVLM';
294200160922             wParm = %editc(trul73ds.O73lmcf:'4');
294300160922             exsr Forzatura;
294400170427             IF  wForza;
294500170427               wErrQta = *on;
294600170427             ENDIF;
294700160922         //?Limite bancali forzabile
294800160922           WHEN  trul731ds.O731fbncf = '1';
294900160922             wIdMsg   = 'TIS0853';
295000160922             wIdCampo = 'VAOBNC';
295100160922             wParm = %editc(trul731ds.O731lbncf:'4');
295200160922             exsr Forzatura;
295300170427             IF  wForza;
295400170427               wErrQta = *on;
295500170427             ENDIF;
295600160922           ENDSL;
295700160922         ENDIF;
295800160922
295900160922       ENDSR;
296000151008
296100151008       //--------------------------------------------------------------
296200151008       //?Calcolo peso/volume più alto per instradamento.
296300151008       //--------------------------------------------------------------
296400151008       BEGSR  PesoVolume;
296500160606
296600160606       //?Sviluppo il peso e/o volume per calcolo instradamento
296700160606         clear wPesomax;
296800160606         clear wVolumemax;
296900160606         clear wPesoaut;
297000160606         clear wVolumeaut;
297100160606         clear wPesoblc;
297200160606         clear wVolumeblc;
297300160606         clear wPesomtc;
297400160606         clear wVolumemtc;
297500151008
297600151008         wPeso = fiora0I0.vaopkg;
297700151008         wVolume = fiora0I0.vaovlm;
297800160606
297900160606       //?se non inserito il peso lo sviluppo
298000160606         IF  wPeso = 0;
298100160606           SELECT;
298200160606           WHEN  fiora0I0.vaobnc > 0;
298300160606             eval(h) wPesomax =
298400160606                 fiora0I0.vaobnc / dDFT.d§DFTbnc * dDFT.d§DFTpkg;
298500160606           WHEN  fiora0I0.vaovlm > 0;
298600160606             eval(h) wPesomax = fiora0I0.vaovlm * dDFT.d§DFTpkg;
298700160606           ENDSL;
298800160606       //?se peso troppo alto imposto il massimo consentito
298900160606           IF  wPesomax > 999999,9;
299000160606             wPeso = 999999,9;
299100160606           ELSE;
299200160606             wPeso = wPesomax;
299300160606           ENDIF;
299400160606         ENDIF;
299500151008
299600160606       //?se non inserito il volume lo sviluppo
299700151008         IF  wVolume = 0;
299800151008           SELECT;
299900151008           WHEN  fiora0I0.vaobnc > 0;
300000151008             eval(h) wVolumemax = fiora0I0.vaobnc / dDFT.d§DFTbnc;
300100151008           WHEN  fiora0I0.vaopkg > 0;
300200151008             eval(h) wVolumemax = fiora0I0.vaopkg / dDFT.d§DFTpkg;
300300151008           ENDSL;
300400151008       //?se volume troppo alto imposto il massimo consentito
300500151008           IF  wVolumemax > 99,999;
300600151008             wVolume = 99,990;
300700151008           ELSE;
300800151008             wVolume = wVolumemax;
300900151008           ENDIF;
301000151008         ENDIF;
301100160606
301200160606       //?Se richiamato da AS400 ho altre quantità che posso inserire
301300160606       //?quindi se inserite le sviluppo
301400160606       //?sono quantità da sviluppare in m³
301500160606         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
301600160606           IF  fiora0I0.vaoatt > 0;
301700160606             eval(h) wVolumeaut = fiora0I0.vaoatt * dDFT.d§DFTaut;
301800160606             eval(h) wPesoaut = wVolumeaut * dDFT.d§DFTpkg;
301900160606           ENDIF;
302000160606           IF  fiora0I0.vaomtc > 0;
302100160606             eval(h) wVolumemtc = fiora0I0.vaomtc * dDFT.d§DFTmtc;
302200160606             eval(h) wPesomtc = wVolumemtc * dDFT.d§DFTpkg;
302300160606           ENDIF;
302400160606           IF  fiora0I0.vaoblc > 0;
302500160606             eval(h) wVolumeblc = fiora0I0.vaoblc * dDFT.d§DFTblc;
302600160606             eval(h) wPesoblc = wVolumeblc * dDFT.d§DFTpkg;
302700160606           ENDIF;
302800160606           IF  wVolumeaut > 99,999;
302900160606             wVolumeaut = 99,999;
303000160606           ENDIF;
303100160606           IF  wPesoaut > 999999,9;
303200160606             wPesoaut = 999999,9;
303300160606           ENDIF;
303400160606           IF  wVolumemtc > 99,999;
303500160606             wVolumemtc = 99,999;
303600160606           ENDIF;
303700160606           IF  wPesomtc > 999999,9;
303800160606             wPesomtc = 999999,9;
303900160606           ENDIF;
304000160606           IF  wVolumeblc > 99,999;
304100160606             wPesoblc = 999999,9;
304200160606           ENDIF;
304300160606           IF  wPesoblc > 999999,9;
304400160606             wVolumeblc = 99,999;
304500160606           ENDIF;
304600160606         ENDIF;
304700160606
304800160606       //?Dalla versione 'D' della ds di output FIORA0O0
304900160606       //?torno il peso e volume da memorizzare su FNORG
305000160728       //?solo per AS400
305100160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
305200160606         IF  fiora0O0.versione > 'C';
305300160615           fiora0O0.volume = wVolume;
305400160615           fiora0O0.peso = wPeso;
305500160606           IF  wVolumeaut > wVolume;
305600160615             fiora0O0.volume = wVolumeaut;
305700160606           ENDIF;
305800160606           IF  wPesoaut > wPeso;
305900160615             fiora0O0.peso = wPesoaut;
306000160606           ENDIF;
306100160606           IF  wVolumemtc > wVolume;
306200160615             fiora0O0.volume = wVolumemtc;
306300160606           ENDIF;
306400160606           IF  wPesomtc > wPeso;
306500160615             fiora0O0.peso = wPesomtc;
306600160606           ENDIF;
306700160606           IF  wVolumeblc > wVolume;
306800160615             fiora0O0.volume = wVolumeblc;
306900160606           ENDIF;
307000160606           IF  wPesoblc > wPeso;
307100160615             fiora0O0.peso = wPesoblc;
307200160606           ENDIF;
307300160615           wVolume = fiora0O0.volume;
307400160615           wPeso = fiora0O0.peso;
307500160606         ENDIF;
307600160728         ENDIF;
307700151008
307800151008       ENDSR;
307900131204
308000131204       //--------------------------------------------------------------
308100131204       //?Controllo la natura della merce.
308200131204       //--------------------------------------------------------------
308300131204       BEGSR  NaturaMerce;
308400131204
308500140404       //?Se richiamato da internet
308600140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
308700140411       //?Obbligo della natura merce se mittente non codificato
308800140416       //?                         e se destinatario non codificato
308900140416             fiora0I0.vaonam = *blanks and fiora0I0.vaocra = *zeros and
309000140416             fiora0I0.vaocrc = *zeros;
309100140411           wIdMsg   = 'TIS0143';
309200140411           wIdCampo = 'VAONAM';
309300140411           clear wParm;
309400140411           exsr Messaggi;
309500140411           leavesr;
309600140407         ENDIF;
309700140211
309800140211         IF  fiora0I0.vaonam <> *blanks;
309900140211           exec sql
310000140211           set :fiora0O0.vaonam = ucase (:fiora0I0.vaonam);
310100131223         ENDIF;
310200131204
310300131204       ENDSR;
310400131204
310500131204       //--------------------------------------------------------------
310600131204       //?Controllo uno o più destinatari.
310700131204       //--------------------------------------------------------------
310800131204       BEGSR  UnoPiuDest;
310900131220
311000131220         fiora0O0.unopiudest = fiora0I0.unopiudest;
311100160607
311200160607       //?I controlli sucessivi li devo fare solo se richiamato da Internet
311300160607         IF  fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET;
311400160607           leavesr;
311500160607         ENDIF;
311600131204
311700131204       //?Accetto 1 o 2
311800160714         IF  fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO and
311900160714             fiora0O0.unopiudest <> FIORA00_PIUDESTINATARI;
312000131204           wIdMsg   = 'TIS0295';
312100131204           wIdCampo = 'UNOPIUDEST';
312200131204           clear wParm;
312300131204           exsr Messaggi;
312400131219           leavesr;
312500131204         ENDIF;
312600131204
312700131204       //?Se ORM prepagato SOLO 1 destinatario
312800131204         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
312900160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
313000131204           wIdMsg   = 'TIS0307';
313100131204           wIdCampo = 'UNOPIUDEST';
313200131204           clear wParm;
313300131204           exsr Messaggi;
313400131204         ENDIF;
313500131204
313600131204       //?Se richiesto il ritiro tramite DPD SOLO 1 destinatario
313700131204         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
313800160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
313900131204           wIdMsg   = 'TIS0307';
314000131204           wIdCampo = 'UNOPIUDEST';
314100131204           clear wParm;
314200131204           exsr Messaggi;
314300131204         ENDIF;
314400131204
314500131204       ENDSR;
314600131205
314700131205       //--------------------------------------------------------------
314800131205       //?Controllo Referente.
314900131205       //--------------------------------------------------------------
315000131205       BEGSR  Referente;
315100131205
315200170522       //?Se richiamato da internet
315300170522         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
315400131205       //?Se mittente non codificato oppure ORM commissionato
315500131205       //?il referente è obbligatorio
315600140401         IF  (fiora0I0.vaocra = 0 or wCom) and
315700131223              fiora0I0.vaorer = *blanks;
315800131205           wIdMsg   = 'TIS0290';
315900131205           wIdCampo = 'VAORER';
316000131205           clear wParm;
316100131205           exsr Messaggi;
316200170404           wErr = *on;
316300131205         ENDIF;
316400170522         ENDIF;
316500160601
316600160613       //?se Richiamato da AS400
316700160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
316800160613         //?Referente obbligatorio
316900160613         //?se Prepagato
317000160613           IF   fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
317100160613                fiora0I0.vaorer = *blanks;
317200160613             wIdMsg   = 'TIS0290';
317300160613             wIdCampo = 'VAORER';
317400160613             clear wParm;
317500170404             wErr = *on;
317600160613             exsr Messaggi;
317700160613           ENDIF;
317800170522         //?Referente obbligatorio
317900170522         //?se Commissionato
318000170522           IF  wCom and fiora0I0.vaorer = *blanks and not wErr;
318100170522               wIdMsg   = 'TIS0290';
318200170522               wIdCampo = 'VAORER';
318300170522               clear wParm;
318400170522               wErr = *on;
318500170522               exsr Messaggi;
318600170522           ENDIF;
318700170522         //?Referente obbligatorio
318800170522         //?se mittente non codificato e immissione manuale
318900170522           IF  fiora0I0.vaocra = *zeros and fiora0I0.vaorer = *blanks and
319000170522              (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
319100170522               fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and not wErr;
319200170522               wIdMsg   = 'TIS0290';
319300170522               wIdCampo = 'VAORER';
319400170522               clear wParm;
319500170522               wErr = *on;
319600170522               exsr Messaggi;
319700170522           ENDIF;
319800160613         ENDIF;
319900160613
320000131223         IF  fiora0I0.vaorer <> *blanks;
320100140211           exec sql
320200140211           set :fiora0O0.vaorer = ucase (:fiora0I0.vaorer);
320300131223         ENDIF;
320400131205
320500131205       ENDSR;
320600131223
320700131223       //--------------------------------------------------------------
320800131223       //?Controllo Telefono.
320900131223       //--------------------------------------------------------------
321000131223       BEGSR  Telefono;
321100131223
321200170522       //?Se richiamato da internet
321300170522         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
321400131223       //?Se mittente non codificato oppure ORM commissionato
321500131223       //?il telefono è obbligatorio
321600140401         IF  (fiora0I0.vaocra = 0 or wCom) and
321700131223              fiora0I0.vaoter = *blanks;
321800131223           wIdMsg   = 'TIS0246';
321900131223           wIdCampo = 'VAOTER';
322000131223           clear wParm;
322100131223           exsr Messaggi;
322200170404           wErr = *on;
322300131223         ENDIF;
322400170522         ENDIF;
322500160601
322600160601       //?se Richiamato da AS400
322700160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
322800160613         //?Telefono obbligatorio
322900160613         //?se Prepagato
323000160613           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
323100160613               fiora0I0.vaoter = *blanks;
323200160613             wIdMsg   = 'TIS0246';
323300160613             wIdCampo = 'VAOTER';
323400160613             clear wParm;
323500160613             exsr Messaggi;
323600170404             wErr = *on;
323700160613           ENDIF;
323800170522         //?Telefono obbligatorio
323900170522         //?se Commissionato
324000170522           IF  wCom and fiora0I0.vaoter = *blanks and not wErr;
324100170522             wIdMsg   = 'TIS0246';
324200170522             wIdCampo = 'VAOTER';
324300170522             clear wParm;
324400170522             exsr Messaggi;
324500170522             wErr = *on;
324600170522           ENDIF;
324700170522         //?Telefono obbligatorio
324800170522         //?se mittente non codificato e immissione manuale
324900170522           IF  fiora0I0.vaocra = *zeros and fiora0I0.vaoter = *blanks and
325000170522              (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
325100170522               fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and not wErr;
325200170522               wIdMsg   = 'TIS0246';
325300170522               wIdCampo = 'VAOTER';
325400170522               clear wParm;
325500170522               wErr = *on;
325600170522               exsr Messaggi;
325700170522           ENDIF;
325800160601         ENDIF;
325900131223
326000131223         IF  fiora0I0.vaoter <> *blanks;
326100140211           exec sql
326200140211           set :fiora0O0.vaoter = ucase (:fiora0I0.vaoter);
326300131223         ENDIF;
326400131223
326500131223       ENDSR;
326600140311
326700140311       //--------------------------------------------------------------
326800140311       //?Controllo indirizzo mail.
326900140311       //--------------------------------------------------------------
327000140311       BEGSR  Mail;
327100140311
327200140311         IF  fiora0I0.mail <> *blanks;
327300140311           clear dsemail;
327400140311           dsemail.§emlindi = fiora0I0.mail;
327500140311           xemail (dsemail);
327600140311           IF  dsemail.§emlerro <> '0';
327700140424             wIdMsg   = 'TIS0732';
327800140311             wIdCampo = 'MAIL';
327900140424             clear wParm;
328000140311             exsr Messaggi;
328100140311             leavesr;
328200140311           ENDIF;
328300140311
328400140424           fiora0O0.mail = fiora0I0.mail;
328500140722           IF  dsemail.§emlindo <> *blanks;
328600140722             fiora0O0.mail = dsemail.§emlindo;
328700140722           ENDIF;
328800140311         ENDIF;
328900140311
329000140311       ENDSR;
329100140311
329200140311       //--------------------------------------------------------------
329300140311       //?Controllo n.cellulare.
329400140311       //--------------------------------------------------------------
329500140311       BEGSR  Sms;
329600140311
329700140311         IF  fiora0I0.sms <> *blanks;
329800140311           pInCell = %trim(fiora0I0.sms);
329900140311           clear pOutErr;
330000140722           clear pOutCell;
330100140311           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
330200140311             wIdMsg   = 'TIS0725';
330300140311             wIdCampo = 'SMS';
330400140311             clear wParm;
330500140311             exsr Messaggi;
330600160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
330700160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
330800160601             fiora0I0.sms;
330900140311             leavesr;
331000140311           ENDIF;
331100140311
331200140416           fiora0O0.sms = fiora0I0.sms;
331300140722           IF  pOutCell <> *blanks;
331400140722             fiora0O0.sms = pOutCell;
331500140722           ENDIF;
331600140311         ENDIF;
331700140311
331800140311       ENDSR;
331900170418
332000170418       //--------------------------------------------------------------
332100170418       //?Recupero il giro da anagrafica clienti ritiro.
332200170418       //--------------------------------------------------------------
332300170418       BEGSR  GiroAnagrafica;
332400170418
332500170418         reset FIORB0I2;
332600170418         reset FIORB0O2;
332700170418         fiorb0I2.codice = fiora0I0.vaocra;
332800170418         fiorb0I2.filiale = fiora0I0.vaopor;
332900170418         RqsOpCode = FIORB00_RQSOPCODE_GIRI;
333000170418
333100170418         MONITOR;
333200170418           Fiorb00_Anagrafica( RqsOpCode
333300170418                             : RpyOpCode
333400170418                             : RpyIdMsg
333500170418                             : fiorb0I2.formato
333600170418                             : fiorb0I2
333700170418                             : %SIZE(fiorb0I2)
333800170418                             : fiorb0O2.formato
333900170418                             : fiorb0O2
334000170418                             : %SIZE(fiorb0O2)
334100170418                             );
334200170418           ON-ERROR;
334300170418             RpyOpCode = FIORB00_RPYOPCODE_ERROR;
334400170418           ENDMON;
334500170418
334600170418       //?Se torna errore emetto messaggio generico a video
334700170418         IF RpyOpCode < FIORB00_RPYOPCODE_DONE;
334800170418           IF  fiora0m0.nrmsg < 99;
334900170418             fiora0m0.nrmsg += 1;
335000170418             fiora0M0.skIdMsg(fiora0M0.nrmsg) = 'ORM*';
335100170418             fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'ORM*';
335200170418             fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
335300170418             fiora0M0.skTextMsg (fiora0M0.nrmsg)=
335400170418             'Grave errore in ricerca GIRO da Anagrafica';
335500170418             leavesr;
335600170418           ENDIF;
335700170418         //?Esco dal programma se ho più di 99 errori
335800170418           exsr RoutEnd;
335900170418         ENDIF;
336000170418
336100170418       //?Se non ho nessun giro vado via
336200170418         IF  fiorb0O2.girostd = *blanks and fiorb0O2.girooltre = *blanks and
336300170418             fiorb0O2.girosotto = *blanks;
336400170418           leavesr;
336500170418         ENDIF;
336600170418
336700170418       //?Imposto il giro in base all'ambito e tabella AGR
336800170419         fiora0I0.cgi = fiorb0O2.girostd;
336900170418         IF  wOKagr;
337000170419           IF  (wPeso > dagr.§AGRpkgo or wVolume > dagr.§AGRvlmo or
337100170419               fiora0I0.vaobnc > dagr.§AGRbnco) and
337200170418               fiorb0O2.girooltre <> *blanks;
337300170419             fiora0I0.cgi = fiorb0O2.girooltre;
337400170418           ENDIF;
337500170419           IF  ((wPeso > *zeros and wPeso < dagr.§AGRpkgs) or
337600170419                (wVolume > *zeros and wVolume < dagr.§AGRvlms) or
337700170419                (fiora0I0.vaobnc > *zeros and fiora0I0.vaobnc < dagr.§AGRbncs))
337800170419               and fiorb0O2.girosotto <> *blanks;
337900170419             fiora0I0.cgi = fiorb0O2.girosotto;
338000170418           ENDIF;
338100170418         ENDIF;
338200170419         fiora0I0.tgi = 'A';
338300170418
338400170418       ENDSR;
338500140313
338600140313       //--------------------------------------------------------------
338700140313       //?Controllo Codice Destinatario.
338800140313       //--------------------------------------------------------------
338900140313       BEGSR  CodDestinatario;
339000140401
339100140401         wCodice = fiora0I0.vaocrc;
339200140402         reset FIORB0OC;
339300140313
339400140313       //?Se presente codice destinatario verifico se valido
339500160607       //?e se legato al codice di LOG solo se da Internet
339600140402         IF  not IsCodiceValido(FIORB0OC);
339700140313           wIdMsg   = 'TIS0066';
339800140313           wIdCampo = 'VAOCRC';
339900140313           clear wParm;
340000140313           exsr Messaggi;
340100140313           leavesr;
340200140313         ENDIF;
340300160607
340400160607       //?Solo per richiamo da AS400
340500160607       //?e solo dalla versione 'B' in poi
340600160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
340700160607             fiorb0OC.versione > 'A';
340800160607         //?codice bloccato
340900160714           IF  fiorb0OC.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
341000160607             wIdMsg   = 'TIS0800';
341100160607             wIdCampo = 'VAOCRC';
341200160607             clear wParm;
341300160607             exsr Messaggi;
341400160607           ENDIF;
341500160607         ENDIF;
341600140313
341700160607       //?Se richiamato da Internet
341800160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
341900140313       //?controllo se fa parte della stessa famiglia del codice di LOG
342000140313         IF not IsCodiceLegato();
342100140313           wIdMsg   = 'TIS0066';
342200140313           wIdCampo = 'VAOCRC';
342300140313           clear wParm;
342400140313           exsr Messaggi;
342500140313           leavesr;
342600140313         ENDIF;
342700160607         ENDIF;
342800160607
342900140313
343000140313       ENDSR;
343100131205
343200131205       //--------------------------------------------------------------
343300131205       //?Controllo Destinatario.
343400131205       //--------------------------------------------------------------
343500131205       BEGSR  Destinatario;
343600131220
343700140211         exec sql
343800140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
343900131205
344000131205       //?Se richiesto UN solo destinatario il dato è obbligatorio
344100160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
344200131205            (fiora0I0.vaorsc = *blanks or fiora0I0.vaoinc = *blanks or
344300131205             fiora0i0.vaoloc = *blanks or fiora0i0.vaocac = *blanks);
344400131205           wIdMsg   = 'TIS0288';
344500131205           wIdCampo = 'VAORSC';
344600131205           clear wParm;
344700131205           exsr Messaggi;
344800131220           leavesr;
344900131205         ENDIF;
345000131205
345100131205       //?Se richiesto più destinatari il dato NON è da inserire
345200160714         IF  fiora0I0.unopiudest = FIORA00_PIUDESTINATARI and
345300131205            (fiora0I0.vaorsc <> *blanks or fiora0I0.vaoinc <> *blanks or
345400131205             fiora0i0.vaoloc <> *blanks or fiora0i0.vaocac <> *blanks);
345500131205           wIdMsg   = 'TIS0380';
345600131205           wIdCampo = 'VAORSC';
345700131205           clear wParm;
345800131205           exsr Messaggi;
345900131220           leavesr;
346000131205         ENDIF;
346100131220
346200131220       //?Controllo nominativo destinatario
346300131220         exsr NomeDestinatario;
346400131220
346500131220       //?Controllo indirizzo destinatario
346600131220         exsr ViaDestinatario;
346700131220
346800131220       //?Controllo località destinatario
346900131220         exsr LocDestinatario;
347000131220
347100131220       //?Controllo cap destinatario
347200131220         exsr CapDestinatario;
347300131220
347400131220       //?Controllo provincia destinatario
347500131220         exsr ProvDestinatario;
347600131220
347700131220       //?Controllo nazione destinatario
347800131220         exsr NazDestinatario;
347900131220
348000131220       //?Controllo indirizzo completo del destinatario
348100131220         exsr IndTotDestinatario;
348200131205
348300131205       ENDSR;
348400131205
348500131205       //--------------------------------------------------------------
348600131205       //?Controllo nominativo destinatario.
348700131205       //--------------------------------------------------------------
348800131205       BEGSR  NomeDestinatario;
348900131205
349000131205       //?Se richiesto UN solo destinatario deve essere indicato
349100160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
349200131205             fiora0I0.vaorsc = *blanks;
349300131205           wIdMsg   = 'TIS0480';
349400131205           wIdCampo = 'VAORSC';
349500131205           clear wParm;
349600131205           exsr Messaggi;
349700131205         ENDIF;
349800131205
349900140211         exec sql
350000140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
350100131205
350200131205       ENDSR;
350300131205
350400131205       //--------------------------------------------------------------
350500131205       //?Controllo indirizzo destinatario.
350600131205       //--------------------------------------------------------------
350700131205       BEGSR  ViaDestinatario;
350800131205
350900131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
351000131220       //?deve essere indicata anche la via
351100160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
351200160714              fiora0I0.vaorsc <> *blanks) and
351300131220              fiora0I0.vaoinc = *blanks;
351400131205           wIdMsg   = 'TIS0281';
351500131205           wIdCampo = 'VAOINC';
351600131205           clear wParm;
351700131205           exsr Messaggi;
351800131205         ENDIF;
351900131205
352000140211         exec sql
352100140211         set :fiora0O0.vaoinc = ucase (:fiora0I0.vaoinc);
352200131205
352300131205       ENDSR;
352400131205
352500131205       //--------------------------------------------------------------
352600131205       //?Controllo cap destinatario.
352700131205       //--------------------------------------------------------------
352800131205       BEGSR  CapDestinatario;
352900131205
353000131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
353100131220       //?deve essere indicato
353200160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
353300160714              fiora0I0.vaorsc <> *blanks) and
353400131220              fiora0I0.vaocac = *blanks;
353500131205           wIdMsg   = 'TIS0051';
353600131205           wIdCampo = 'VAOCAC';
353700131205           clear wParm;
353800131205           exsr Messaggi;
353900131205         ENDIF;
354000131205
354100140211         exec sql
354200140211         set :fiora0O0.vaocac = ucase (:fiora0I0.vaocac);
354300131205
354400131205       ENDSR;
354500131205
354600131205       //--------------------------------------------------------------
354700131205       //?Controllo località destinatario.
354800131205       //--------------------------------------------------------------
354900131205       BEGSR  LocDestinatario;
355000131205
355100131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
355200131220       //?deve essere indicato
355300160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
355400160714              fiora0I0.vaorsc <> *blanks) and
355500131220              fiora0I0.vaoloc = *blanks;
355600131205           wIdMsg   = 'TIS0345';
355700131205           wIdCampo = 'VAOLOC';
355800131205           clear wParm;
355900131205           exsr Messaggi;
356000131205         ENDIF;
356100131205
356200140211         exec sql
356300140211         set :fiora0O0.vaoloc = ucase (:fiora0I0.vaoloc);
356400131205
356500131205       ENDSR;
356600131009
356700131205       //--------------------------------------------------------------
356800131205       //?Controllo provincia destinatario.
356900131205       //--------------------------------------------------------------
357000131205       BEGSR  ProvDestinatario;
357100131205
357200131220       //?Controllo se richiesto UN solo destinatario
357300131220       //?o se immessa ragione sociale
357400160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
357500160714             fiora0I0.vaorsc <> *blanks;
357600131205
357700140211           exec sql
357800140211           set :fiora0O0.vaoprc = ucase (:fiora0I0.vaoprc);
357900131205
358000131205         //?Controllo con la sk delle provincie
358100131205           IF  fiora0O0.vaoprc <> *blanks and
358200131205              %lookup(fiora0O0.vaoprc:skprv) = 0;
358300131205             wIdMsg   = 'TIS0461';
358400131205             wIdCampo = 'VAOPRC';
358500131205             clear wParm;
358600131205             exsr Messaggi;
358700131205           ENDIF;
358800131205
358900131205         ENDIF;
359000131205
359100131205       ENDSR;
359200131205
359300131205       //--------------------------------------------------------------
359400131205       //?Controllo nazione destinatario.
359500131205       //--------------------------------------------------------------
359600131205       BEGSR  NazDestinatario;
359700131205
359800131205       //?Controllo se richiesto UN solo destinatario
359900131220       //?o immessa ragione sociale
360000160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
360100160714             fiora0I0.vaorsc <> *blanks;
360200131205
360300140211           exec sql
360400140211           set :fiora0O0.vaonac = ucase (:fiora0I0.vaonac);
360500131205
360600131205         //?Controllo con la sk delle provincie
360700131205           IF  fiora0O0.vaonac <> *blanks and
360800131205               %lookup(fiora0O0.vaonac:sknaz) = 0;
360900140326             wIdMsg   = 'TIS0384';
361000131205             wIdCampo = 'VAONAC';
361100131205             clear wParm;
361200131205             exsr Messaggi;
361300131205           ENDIF;
361400131205
361500131205         ENDIF;
361600131205
361700131205       ENDSR;
361800131205
361900131205       //--------------------------------------------------------------
362000131205       //?Controllo indirizzo completo del destinatario.
362100131205       //--------------------------------------------------------------
362200131205       BEGSR  IndTotDestinatario;
362300131205
362400131205       //?Controllo se richiesto UN solo destinatario
362500131220       //?o immessa ragione sociale
362600160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
362700160714             fiora0I0.vaorsc <> *blanks;
362800131205
362900131205           clear TISI95DS;
363000151009           clear FNLV13DS;
363100131205           tisi95ds.I95tcn = '7';
363200131205           tisi95ds.I95nar = fiora0O0.vaonac;
363300131205           tisi95ds.I95cap = fiora0O0.vaocac;
363400131205           tisi95ds.I95loc = fiora0O0.vaoloc;
363500131205           tisi95ds.I95prv = fiora0O0.vaoprc;
363600131205           tisi95ds.I95ind = fiora0O0.vaoinc;
363700131205           tisi95ds.I95dat = %dec(%date());
363800151009           fnlv13ds.I13af0 = 'S';
363900151009           fnlv13ds.I13cnv = 'S';
364000151009           fnlv13ds.I13la3 = 'S';
364100131205
364200131205         //?Solo per richiamo da Internet
364300131205           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
364400131205           //?se richiesta nazione irlanda forzo cap eire
364500131205             IF  tisi95ds.I95nar = FIORA00_IRLANDA;
364600131205               tisi95ds.I95cap =  FIORA00_EIRE;
364700131205             ENDIF;
364800131205           ENDIF;
364900131205
365000151009         //?controllo cap/località/provincia
365100151009           fnlv13r (kpjba:fnlv13ds:tisi95ds);
365200160607         //?Solo per richiamo da Internet
365300160607         //?controllo gli errori, da AS ci pensa già il programma chiamante
365400160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
365500151016         //?se torna errore per cap generico diversifico il messaggio
365600151016           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
365700151016               %scan('generico':fnlv13ds.O13msg) > 0;
365800151016             wIdMsg   = 'TIS0826';
365900151016             wIdCampo = 'VAOCAC';
366000151016             clear wParm;
366100151016             exsr Messaggi;
366200151016             wErrMitt = *on;
366300151016             leavesr;
366400151016           ENDIF;
366500151009         //?se torna errore imposto messaggio generico
366600151009           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
366700131205             wIdMsg   = 'TIS0054';
366800131227             wIdCampo = 'VAOLOC';
366900131205             clear wParm;
367000131205             exsr Messaggi;
367100131205             leavesr;
367200131205           ENDIF;
367300160607           ENDIF;
367400160607
367500160607         //?Se richiamato da AS400
367600160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
367700160607         //?se oltre al destinatario c'è anche la filiale consegna impostata devo controllarla
367800160607         //?questo serve ai fini della bollettazione e non ai fini dell'ORM
367900160607             IF  fiora0I0.vaopoc <> *zeros and fiora0I0.vaorsc <> *blanks;
368000160607               exsr FilConsegna;
368100160607             ENDIF;
368200160607         //?se ORM import DPD devo controllare che la nazione di destino sia servita da DPD
368300160607             IF  wNetworkEmi = FIORA00_NTW_DPD and
368400160607                 fiora0I0.vaonac <> *blanks and
368500160607                 fnlv14ds.O14lad <= 0;
368600160607               wIdMsg   = 'TIS0841';
368700160607               wIdCampo = 'VAONAC';
368800160607               clear wParm;
368900160607               exsr Messaggi;
369000160607             ENDIF;
369100160607           ENDIF;
369200131227
369300131227         //?Se ORM export EEX non accetto destinatario esteri
369400131227           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
369500131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX and
369600131227               fiora0O0.vaonac <> *blanks;
369700140305             wIdMsg   = 'TIS0723';
369800131227             wIdCampo = 'VAONAC';
369900140305             clear wParm;
370000131227             exsr Messaggi;
370100131227           ENDIF;
370200131205
370300131205         //?Se ORM export DPD controllo la fattibilità della consegna
370400131205           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
370500131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
370600131205             clear fior95ds;
370700131205             fior95ds.IOR95cap = fiora0O0.vaocac;
370800131205             fior95ds.IOR95loc = fiora0O0.vaoloc;
370900131205             fior95ds.IOR95prv = fiora0O0.vaoprc;
371000131205             fior95ds.IOR95naz = fiora0O0.vaonac;
371100131230             fior95ds.IOR95lin = cvtLinguaISO2ToTntbe(fiora0I0.idlingua);
371200131205             fior95r (kpjba : fior95ds);
371300131205             IF  fior95ds.OOR95err <> *blanks or fior95ds.OOR95ok = 'N';
371400140304               wIdMsg   = 'TIS0718';
371500131205               wIdCampo = 'VAONAC';
371600131205               wParm = fior95ds.OOR95msg;
371700131205               exsr Messaggi;
371800131205             ENDIF;
371900131205           ENDIF;
372000160208
372100160208         //?Se ORM prepagato e consegna con LNA 340-341-345 blocco
372200160208         //?come da DV 1540
372300160208           IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
372400160209               fiora0I0.vaonac <> *blanks and fnlv13ds.O13err = *blanks and
372500160208               (tisi95ds.O95lna = 340 or tisi95ds.O95lna = 341 or
372600160208                tisi95ds.O95lna = 345);
372700160208             clear xx;
372800160208             xx = %lookup(fiora0I0.vaonac:sknaz);
372900160208             wIdMsg   = 'TIS0828';
373000160208             wIdCampo = 'VAOPAG';
373100160208             IF  xx > 0;
373200160208               wParm = sknazd(xx);
373300160208             ELSE;
373400160208               wParm = fiora0I0.vaoloc;
373500160208             ENDIF;
373600160208             exsr Messaggi;
373700160208           ENDIF;
373800160607         //?Se richiamato da AS400 controllo anche con la filiale consegna
373900160607         //?se inserita
374000160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
374100160607               fiora0O0.prepagato = FIORA00_PREPAGATO and
374200160607               fiora0I0.vaonac <> *blanks and
374300160607               (fiora0I0.vaopoc = 340 or fiora0I0.vaopoc = 341 or
374400160607                fiora0I0.vaopoc = 345);
374500160607             clear xx;
374600160607             xx = %lookup(fiora0I0.vaonac:sknaz);
374700160607             wIdMsg   = 'TIS0828';
374800160607             wIdCampo = 'VAOTOR';
374900160607             IF  xx > 0;
375000160607               wParm = sknazd(xx);
375100160607             ELSE;
375200160607               wParm = fiora0I0.vaoloc;
375300160607             ENDIF;
375400160607             exsr Messaggi;
375500160607           ENDIF;
375600160208
375700160208         //?Solo per richiamo da Internet
375800160208           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
375900160208           //?Errore
376000160208           //?Se ordinante non codificato (è utente senza PSW)
376100160208           //?Se consegna all'estero
376200160208           //?Se paga destinatario
376300160208             IF  fiora0I0.vaocor = 0 and
376400160208                 fiora0I0.vaonac <> *blanks and
376500160714                 fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
376600160208               wIdMsg   = 'TIS0448';
376700160208               wIdCampo = 'VAOPAG';
376800160208               clear wParm;
376900160208               exsr Messaggi;
377000160208             ENDIF;
377100160208           ENDIF;
377200131205
377300131205         ENDIF;
377400131205
377500131205       ENDSR;
377600160613
377700160613       //--------------------------------------------------------------
377800160613       //?Controllo quantità videata F05.
377900160613       //--------------------------------------------------------------
378000160613       BEGSR  Quantita2;
378100160613
378200160613       //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
378300160613       //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
378400160613         IF  fiorb0OR.flagfblc <> *blanks and fiora0I0.vaoblc = 0;
378500160613           wIdMsg   = 'TIS0844';
378600160613           wIdCampo = 'VAOBLC';
378700160613           clear wParm;
378800160714           IF  fiorb0OR.flagfblc = FIORA00_FORZABILE;
378900160613             exsr Forzatura;
379000160613             IF  wForza;
379100160613               exsr RoutEnd;
379200160613             ENDIF;
379300160613           ENDIF;
379400160714           IF  fiorb0OR.flagfblc = FIORA00_OBBLIGATORIO;
379500160613             exsr Messaggi;
379600160613           ENDIF;
379700160613         ENDIF;
379800160613         IF  fiorb0OR.flagfmtc <> *blanks and fiora0I0.vaomtc = 0;
379900160613           wIdMsg   = 'TIS0845';
380000160613           wIdCampo = 'VAOMTC';
380100160613           clear wParm;
380200160714           IF  fiorb0OR.flagfmtc = FIORA00_FORZABILE;
380300160613             exsr Forzatura;
380400160613             IF  wForza;
380500160613               exsr RoutEnd;
380600160613             ENDIF;
380700160613           ENDIF;
380800160714           IF  fiorb0OR.flagfmtc = FIORA00_OBBLIGATORIO;
380900160613             exsr Messaggi;
381000160613           ENDIF;
381100160613         ENDIF;
381200160613         IF  fiorb0OR.flagfatt <> *blanks and fiora0I0.vaoatt = 0;
381300160613           wIdMsg   = 'TIS0846';
381400160613           wIdCampo = 'VAOATT';
381500160613           clear wParm;
381600160714           IF  fiorb0OR.flagfatt = FIORA00_FORZABILE;
381700160613             exsr Forzatura;
381800160613             IF  wForza;
381900160613               exsr RoutEnd;
382000160613             ENDIF;
382100160613           ENDIF;
382200160714           IF  fiorb0OR.flagfatt = FIORA00_OBBLIGATORIO;
382300160613             exsr Messaggi;
382400160613           ENDIF;
382500160613         ENDIF;
382600160922
382700160922       //?Controllo limiti quantità
382800160922         clear TRUL731DS;
382900160922         trul731ds.I731blc = fiora0I0.vaoblc;
383000160922         trul731ds.I731att = fiora0I0.vaoatt;
383100160922         trul731ds.I731mtc = fiora0I0.vaomtc;
383200160922         trul731r (trul731ds);
383300160922
383400160922         SELECT;
383500160922       //?Limite bilico bloccante
383600160922         WHEN  trul731ds.O731fblcm = '1';
383700160922           wIdMsg   = 'TIS0854';
383800160922           wIdCampo = 'VAOBLC';
383900160922           wParm = %editc(trul731ds.O731lblcm:'4');
384000160922           exsr Messaggi;
384100160922       //?Limite bilico forzabile
384200160922         WHEN  trul731ds.O731fblcf = '1';
384300160922           wIdMsg   = 'TIS0854';
384400160922           wIdCampo = 'VAOBLC';
384500160922           wParm = %editc(trul731ds.O731lblcf:'4');
384600160922           exsr Forzatura;
384700160922       //?Limite autotreno bloccante
384800160922         WHEN  trul731ds.O731fattm = '1';
384900160922           wIdMsg   = 'TIS0855';
385000160922           wIdCampo = 'VAOATT';
385100160922           wParm = %editc(trul731ds.O731lattm:'4');
385200160922           exsr Messaggi;
385300160922       //?Limite autotreno forzabile
385400160922         WHEN  trul731ds.O731fattf = '1';
385500160922           wIdMsg   = 'TIS0855';
385600160922           wIdCampo = 'VAOATT';
385700160922           wParm = %editc(trul731ds.O731lattf:'4');
385800160922           exsr Forzatura;
385900160922       //?Limite motrice bloccante
386000160922         WHEN  trul731ds.O731fmtcm = '1';
386100160922           wIdMsg   = 'TIS0856';
386200160922           wIdCampo = 'VAOMTC';
386300160922           wParm = %editc(trul731ds.O731lmtcm:'4');
386400160922           exsr Messaggi;
386500160922       //?Limite motrice forzabile
386600160922         WHEN  trul731ds.O731fmtcf = '1';
386700160922           wIdMsg   = 'TIS0856';
386800160922           wIdCampo = 'VAOMTC';
386900160922           wParm = %editc(trul731ds.O731lmtcf:'4');
387000160922           exsr Forzatura;
387100160922         ENDSL;
387200160613
387300160613       ENDSR;
387400170522
387500170522       //--------------------------------------------------------------
387600170522       //?Controlli per ORM Prepagato.
387700170522       //--------------------------------------------------------------
387800170522       BEGSR  OrmPrepagato;
387900170522
388000170522       //?Tipo ORM multiplo e presenti i dati del destinatario errore
388100170522         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO and
388200170522            (fiora0I0.vaocrc > 0 or fiora0I0.vaorsc <> *blanks);
388300170522           wIdMsg   = 'TIS0802';
388400170522           wIdCampo = 'VAOTOR';
388500170522           clear wParm;
388600170522           exsr Messaggi;
388700170522         ENDIF;
388800170522       //?Tipo ORM prepagato e manca il destinatario errore
388900170522         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
389000170522             fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
389100170522           wIdMsg   = 'TIS0834';
389200170522           wIdCampo = 'VAOCRC';
389300170522           clear wParm;
389400170522           exsr Messaggi;
389500170522         ENDIF;
389600170522       //?Tipo ORM prepagato e c'è già la filiale ritiro calcolata
389700170522       //?verifico se possibile il prepagato o se devo immettere prima
389800170522       //?la bolla prepagata
389900170530         IF  (prmRqsOpCode = FIORA00_RQSOPCODE_WRITE or
390000170530              prmRqsOpCode = FIORA00_RQSOPCODE_CONFERMA_FISSI) and
390100170522             fiora0O0.vaopor > *zeros and
390200170522             fiora0O0.vaopor = fiora0I0.fgs and
390300170522             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
390400170522           wIdMsg   = 'TIS0838';
390500170522           wIdCampo = 'VAOTOR';
390600170522           clear wParm;
390700170522           exsr Messaggi;
390800170522         ENDIF;
390900170522
391000170522       ENDSR;
391100131205
391200131205       //--------------------------------------------------------------
391300131205       //?Controllo fermo deposito.
391400131205       //--------------------------------------------------------------
391500131205       BEGSR  FermoDeposito;
391600131205
391700160714         IF  fiora0I0.vaoffd <> FIORA00_SI and
391800160714             fiora0I0.vaoffd <> FIORA00_NO;
391900131205           wIdMsg   = 'TIS0537';
392000131205           wIdCampo = 'VAOFFD';
392100131205           clear wParm;
392200131205           exsr Messaggi;
392300131205         ENDIF;
392400131205
392500131205       ENDSR;
392600160315
392700160315       //--------------------------------------------------------------
392800160315       //?Controllo Filiale Consegna.
392900160315       //--------------------------------------------------------------
393000160315       BEGSR  FilConsegna;
393100160315
393200160315         wOKffc = *off;
393300160315         fiora0O0.vaopoc = fiora0I0.vaopoc;
393400160315
393500160315       //?Se il destinatario è estero
393600160315       //?forzo la filiale di consegna in base alla tabella FFC
393700160315         IF  fiora0O0.vaonac <> *blanks;
393800160315           clear dFFC;
393900160315       //?prima provo con la filiale dell'ordinante
394000160315           IF  fiora0O0.vaocor > 0;
394100160315             k_TBEcod = 'FFC';
394200160315             k_TBEke1 = %subst(%editc(fiora0O0.vaocor:'X'):1:3);
394300160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
394400160315             IF  %found(TNTBE01L);
394500160315               dFFC = TBEuni;
394600160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
394700160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
394800160315               ENDIF;
394900160315               wOKffc = *on;
395000160315             ENDIF;
395100160315           ENDIF;
395200160315       //?se non trovo con filiale ordinante proco con filiale emissione se diversa
395300160315           IF  not wOKffc and
395400160315               %subst(%editc(fiora0O0.vaocor:'X'):1:3) <>
395500160315               %editc(fiora0O0.vaopoe:'X');
395600160315             clear dFFC;
395700160315             k_TBEcod = 'FFC';
395800160315             k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
395900160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
396000160315             IF  %found(TNTBE01L);
396100160315               dFFC = TBEuni;
396200160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
396300160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
396400160315               ENDIF;
396500160315             ENDIF;
396600160315           ENDIF;
396700160315         ENDIF;
396800160315
396900160315       //?Se non impostata non controllo
397000160315         IF  fiora0O0.vaopoc = *zeros;
397100160315           leavesr;
397200160315         ENDIF;
397300160315
397400160315       //?Controllo la filiale consegna
397500160315         IF  not IsFilValida(fiora0I0.vaopoc);
397600160315           wIdMsg   = 'TIS0803';
397700160315           wIdCampo = 'VAOPOC';
397800160315           clear wParm;
397900160315           exsr Messaggi;
398000160315           leavesr;
398100160315         ENDIF;
398200160315
398300160315       //?Network e Decodifico la filiale consegna
398400160315         wNetworkCon = GetNtwFiliale(fiora0O0.vaopoc);
398500160315
398600160315       ENDSR;
398700160315
398800160315       //--------------------------------------------------------------
398900160315       //?Controllo Cliente tassazione.
399000160315       //--------------------------------------------------------------
399100160315       BEGSR  ClienteKsc;
399200160315
399300160315         wOKtariffa = *off;
399400170414
399500170414       //?Se non ho il codice KSC
399600170414         IF  fiora0I0.vaoksc = 0;
399700170414         //?Se paga Ordinante imposto come KSC i primi 7 del codice ordinante
399800170414           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE;
399900170414           fiora0I0.vaoksc = %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:7):7:0);
400000170414           ENDIF;
400100170414         //?Se paga Destinatario imposto come KSC i primi 7 del codice destinatario
400200170414           IF  fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO and
400300170414               fiora0I0.vaocrc > 0;
400400170414           fiora0I0.vaoksc = %dec(%subst(%editc(fiora0I0.vaocrc:'X'):1:7):7:0);
400500170414           ENDIF;
400600170414         ENDIF;
400700160315
400800160315       //?Codice tassazione valido
400900160315         IF  fiora0I0.vaoksc > *zeros;
401000160315           IF  not IsKscValido(fiora0I0.vaoksc);
401100160315             wIdMsg   = 'TIS0810';
401200160315             wIdCampo = 'VAOKSC';
401300160315             clear wParm;
401400160315             exsr Messaggi;
401500160315             leavesr;
401600160315           ENDIF;
401700160315         ENDIF;
401800160315
401900160315       //?Codice tariffa valido
402000160315         IF  fiora0I0.vaoctr <> *blanks;
402100160315           IF  %check(FIORA00_DIGITS:fiora0I0.vaoctr) > *zeros;
402200160315             wIdMsg   = 'TIS0810';
402300160315             wIdCampo = 'VAOCTR';
402400160315             clear wParm;
402500160315             exsr Messaggi;
402600160315             leavesr;
402700160315           ENDIF;
402800160315         ENDIF;
402900160315
403000160315       //?Se codice tariffa deve esserci anche il codice cliente
403100160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc = *zeros;
403200160607           wIdMsg   = 'TIS0840';
403300160315           wIdCampo = 'VAOCTR';
403400160607           clear wParm;
403500160315           exsr Messaggi;
403600160315           leavesr;
403700160315         ENDIF;
403800160315
403900160315       //?La tariffa deve esistere
404000160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc > *zeros;
404100160315
404200160315           k_TAMksc = fiora0I0.vaoksc;
404300160315           k_TAMctr = %int(fiora0I0.vaoctr);
404400160315           setll (k_TAMksc:k_TAMctr) TNTAM01L;
404500160315           IF  not %equal(TNTAM01L);
404600160315             wIdMsg   = 'TIS0810';
404700160315             wIdCampo = 'VAOKSC';
404800160315             clear wParm;
404900160315             exsr Messaggi;
405000160315             leavesr;
405100160315           ENDIF;
405200160315
405300160315           DOW  not wOKtariffa;
405400160315             reade (k_TAMksc:k_TAMctr) TNTAM01L;
405500160315             IF  %eof(TNTAM01L);
405600160315               leave;
405700160315             ENDIF;
405800160315             IF  TAMatb = *blanks;
405900160315               wOKtariffa = *on;
406000160315             ENDIF;
406100160315           ENDDO;
406200160315           IF  not wOKtariffa;
406300160315             wIdMsg   = 'TIS0810';
406400160315             wIdCampo = 'VAOKSC';
406500160315             clear wParm;
406600160315             exsr Messaggi;
406700160315             leavesr;
406800160315           ENDIF;
406900160315
407000160315         ENDIF;
407100160315
407200160315         fiora0O0.vaoksc = fiora0I0.vaoksc;
407300160315         fiora0O0.vaoctr = fiora0O0.vaoctr;
407400160315
407500160315       ENDSR;
407600140506
407700140506       //--------------------------------------------------------------
407800140506       //?Controllo Alert.
407900140506       //--------------------------------------------------------------
408000140506       BEGSR  Alert;
408100140506
408200140506         IF  fiora0I0.alertmail <> *blanks and
408300160714             fiora0I0.alertmail <> FIORA00_NO and
408400160714             fiora0I0.alertmail <> FIORA00_X;
408500140506           clear fiora0O0.alertmail;
408600140506         ENDIF;
408700140506
408800140506         IF  fiora0I0.alertsms <> *blanks and
408900160714             fiora0I0.alertsms <> FIORA00_NO and
409000160714             fiora0I0.alertsms <> FIORA00_X;
409100140506           clear fiora0O0.alertsms;
409200140506         ENDIF;
409300140506
409400140506       ENDSR;
409500151008
409600151008       //--------------------------------------------------------------
409700151008       //?Controllo indirizzo mail x accettazione ORM.
409800151008       //--------------------------------------------------------------
409900151008       BEGSR  MailConferma;
410000160503
410100160714         fiora0O0.alertconf = FIORA00_NO;
410200151008
410300160318       //?Versione inferiore alla 'C'
410400160714         IF  fiora0I0.mailconf = *blanks and
410500160714             fiora0I0.alertconf = FIORA00_SI and
410600160318             fiora0O0.versione < 'C';
410700151008           wIdMsg   = 'TIS0732';
410800151008           wIdCampo = 'MAILCONF';
410900151008           clear wParm;
411000151008           exsr Messaggi;
411100151008           leavesr;
411200151008         ENDIF;
411300160318       //?dalla Versione 'C'
411400170427       //?solo per richiamo da Internet
411500170427         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
411600160714         IF  fiora0I0.mailconf = *blanks and
411700160714             fiora0I0.alertconf = FIORA00_SI and
411800160318             fiora0O0.versione > 'B' and fiora0I0.smsconf = *blanks;
411900160318           wIdMsg   = 'TIS0831';
412000160318           wIdCampo = 'MAILCONF';
412100160318           clear wParm;
412200160318           exsr Messaggi;
412300160318           leavesr;
412400160318         ENDIF;
412500170427         ENDIF;
412600151008
412700170427         IF  fiora0I0.mailconf <> *blanks and
412800170427             fiora0I0.alertconf = FIORA00_SI;
412900151008           clear dsemail;
413000151008           dsemail.§emlindi = fiora0I0.mailconf;
413100151008           xemail (dsemail);
413200151008           IF  dsemail.§emlerro <> '0';
413300151008             wIdMsg   = 'TIS0732';
413400151008             wIdCampo = 'MAILCONF';
413500151008             clear wParm;
413600151008             exsr Messaggi;
413700151008             leavesr;
413800151008           ENDIF;
413900151008
414000151008           fiora0O0.mailconf = fiora0I0.mailconf;
414100151008           IF  dsemail.§emlindo <> *blanks;
414200151008             fiora0O0.mailconf = dsemail.§emlindo;
414300151008           ENDIF;
414400151008         ENDIF;
414500151008
414600151008       //?Passo il dato solo se impostato a 'SI'
414700160714         IF  fiora0I0.alertconf = FIORA00_SI;
414800151008           fiora0O0.alertconf = fiora0I0.alertconf;
414900160322         //?Dalla versione 'C' se non c'è la mail non lo imposto
415000160322           IF  fiora0O0.versione > 'B' and fiora0O0.mailconf = *blanks;
415100160714             fiora0O0.alertconf = FIORA00_NO;
415200160322           ENDIF;
415300151008         ENDIF;
415400151008
415500151008       ENDSR;
415600151008
415700151008       //--------------------------------------------------------------
415800151008       //?Calcolo orario indicativo ritiro.
415900151008       //--------------------------------------------------------------
416000151008       BEGSR  OrarioIndRitiro;
416100151008
416200151008       //?imposto orario ritiro standard
416300151012         fiora0O0.orariti = fiora0O0.orainizio;
416400151012         fiora0O0.oraritf = fiora0O0.orafine;
416500151126
416600170621       //?Se data immissione = data ritiro
416700170621         IF  fiora0O0.vaodar = fiora0I0.vaodao;
416800151126       //?Se l'ora pronta merce è maggiore
416900151126       //?dell'ora inizio servizio
417000151126       //?e minore dell'ora fine
417100151126       //?la imposto come orario inzio
417200151126         IF  fiora0O0.vaoorr > fiora0O0.orainizio and
417300151126             fiora0O0.vaoorr < fiora0O0.orafine;
417400151126           fiora0O0.orariti = fiora0O0.vaoorr;
417500151126         ENDIF;
417600151126       //?Se l'ora pronta merce è maggiore
417700151126       //?dell'ora fine servizio
417800151126       //?la imposto come orario fine
417900151126         IF  fiora0O0.vaoorr > fiora0O0.orafine;
418000151126           fiora0O0.oraritf = fiora0O0.vaoorr;
418100151126         ENDIF;
418200170621         ENDIF;
418300151008
418400151008       ENDSR;
418500160318
418600160318       //--------------------------------------------------------------
418700160318       //?Controllo SMS x accettazione ORM.
418800160318       //--------------------------------------------------------------
418900160318       BEGSR  SmsConferma;
419000160318
419100160714         fiora0O0.alertconfs = FIORA00_NO;
419200160318
419300160620       //?Se richiamato da Internet
419400160620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
419500160714         IF  fiora0I0.smsconf = *blanks and
419600160714             fiora0I0.alertconf = FIORA00_SI and
419700160318             fiora0I0.mailconf = *blanks;
419800160318           wIdMsg   = 'TIS0831';
419900160318           wIdCampo = 'SMSCONF';
420000160318           clear wParm;
420100160318           exsr Messaggi;
420200160318           leavesr;
420300160318         ENDIF;
420400170427         ENDIF;
420500160318
420600160714         IF  fiora0I0.smsconf <> *blanks and
420700160714             fiora0I0.alertconf = FIORA00_SI;
420800160318           pInCell = %trim(fiora0I0.smsconf);
420900160318           clear pOutErr;
421000160318           clear pOutCell;
421100160318           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
421200160318             wIdMsg   = 'TIS0725';
421300160318             wIdCampo = 'SMSCONF';
421400160318             clear wParm;
421500160318             exsr Messaggi;
421600160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
421700160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
421800160601             fiora0I0.smsconf;
421900160318             leavesr;
422000160318           ENDIF;
422100170427           fiora0O0.smsconf = fiora0I0.smsconf;
422200170427           IF  pOutCell <> *blanks;
422300170427             fiora0O0.smsconf = pOutCell;
422400170427           ENDIF;
422500170427         ENDIF;
422600160318
422700160318       //?Passo il dato solo se impostato a 'SI'
422800170427         IF  fiora0I0.alertconf = FIORA00_SI;
422900170427           fiora0O0.alertconfs = fiora0I0.alertconf;
423000170427           IF  fiora0O0.smsconf = *blanks;
423100170427             fiora0O0.alertconfs = FIORA00_NO;
423200160318           ENDIF;
423300160502         ENDIF;
423400160318
423500160318       ENDSR;
423600160714
423700160714       //--------------------------------------------------------------
423800160714       //?Controllo Flag x memorizzazione dati su Anagrafica Ritiro.
423900160714       //--------------------------------------------------------------
424000160714       BEGSR  FlagMemDati;
424100160714
424200160714       //?Per richiamo da Internet
424300160714       //?e utente anonimo non controllo
424400160714         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
424500160714             fiora0I0.IdUtente = 0;
424600160714           leavesr;
424700160714         ENDIF;
424800160714
424900160714       //?Accetto solo S o N
425000160714         IF  fiora0I0.memdatiacr <> FIORA00_SI and
425100160728             fiora0I0.memdatiacr <> FIORA00_NO and
425200160728             fiora0I0.memdatiacr <> *blanks;
425300160714           wIdMsg   = 'TIS0848';
425400160714           wIdCampo = 'MEMDATIACR';
425500160714           clear wParm;
425600160714           exsr Messaggi;
425700160714         ENDIF;
425800160714
425900160714       ENDSR;
426000160315
426100160315       //--------------------------------------------------------------
426200160315       //?Cerco il giro di ritiro.
426300160315       //--------------------------------------------------------------
426400160315       BEGSR  GiroRitiro;
426500160315
426600160315         fiora0O0.cgi = fiora0I0.cgi;
426700170419         fiora0O0.tgi = fiora0I0.tgi;
426800160315
426900160412       //?Se giro impostato lo controllo
427000160315         IF  fiora0O0.cgi <> *blanks;
427100160315           clear FIDG09DS;
427200160315           fidg09ds.D09iop0 = '001';
427300160518           fidg09ds.D09ifgs = fiora0I0.VAOpor;
427400160315           fidg09ds.D09idat = dataoggi;
427500160315           fidg09ds.D09icgi = fiora0O0.cgi;
427600160315           fidg09ds.D09itug = 'R';
427700160315           %subst(kpjba:247:256) = FIDG09DS;
427800160315           fidg09r (kpjba);
427900160315           fidg09ds = %subst(kpjba:247:256);
428000160315           IF  fidg09ds.D09oerr <> *blanks;
428100160315             wIdMsg   = 'TIS0718';
428200160315             wIdCampo = 'CGI';
428300160315             wParm = fidg09ds.D09omsg;
428400160315             exsr Messaggi;
428500160315             leavesr;
428600160315           ENDIF;
428700160315         ENDIF;
428800170419
428900170419       //?Se giro impostato e tipo giro = 'A' e non c'è il mittente
429000170419       //?devo impostare il giro come 'M' manuale
429100170419         IF  fiora0O0.cgi <> *blanks and fiora0I0.tgi = 'A' and
429200170419             fiora0I0.vaocra = 0;
429300170419           fiora0O0.tgi = 'M';
429400170419         ENDIF;
429500160315
429600160315       ENDSR;
429700140312
429800131008       //--------------------------------------------------------------
429900131008       //?Routine per schierare i messaggi nella ds FIORA0M0.
430000131008       //--------------------------------------------------------------
430100131008       BEGSR  Messaggi;
430200131007
430300131008       //?Se ho già caricato 99 messaggi esco
430400131008         IF  fiora0M0.nrmsg = 99;
430500131010           exsr RoutEnd;
430600131008           clear fiora0M0.nrmsg;
430700131008         ENDIF;
430800131008
430900131008         fiora0M0.nrmsg += 1;
431000131009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
431100131009         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
431200140211         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
431300131204         IF  wParm <> *blanks;
431400131219           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
431500131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
431600131204         ELSE;
431700131219           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
431800131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
431900131204         ENDIF;
432000131008
432100131008       ENDSR;
432200131008
432300131008       //--------------------------------------------------------------
432400131008       //?Routine per schierare le forzature nella ds FIORA0F0.
432500131008       //--------------------------------------------------------------
432600131008       BEGSR  Forzatura;
432700160531
432800160531         wForza = *off;
432900160530
433000160530       //?Se già forzato non riemetto il messaggio
433100160531         wNrForza = %lookup(wIdMsg:fiora0F0.skIdMsg);
433200160531         IF  wNrForza > 0 and fiora0F0.skGiaForza(wNrForza) =
433300160530             FIORA00_GIAFORZA_SI;
433400160530           leavesr;
433500160530         ENDIF;
433600160531
433700160531       //?Se ho già caricato 99 mesaggi esco
433800160531         IF  fiora0M0.nrmsg = 99;
433900160531           exsr RoutEnd;
434000160531           clear fiora0M0.nrmsg;
434100160531         ENDIF;
434200160530
434300160531       //?Imposto messaggio da forzare
434400160530         fiora0M0.nrmsg += 1;
434500160530         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
434600160530         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
434700160530         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
434800160530         IF  wParm <> *blanks;
434900160530           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
435000160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
435100160530         ELSE;
435200160530           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
435300160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
435400160530         ENDIF;
435500160531
435600160531         wForza = *on;
435700131008
435800131008       ENDSR;
435900131009
436000131009       //--------------------------------------------------------------
436100131009       //?Imposta i dati nella ds di Output.
436200131009       //--------------------------------------------------------------
436300131009       BEGSR  DsOutput;
436400131008
436500131009       //?Se richiamo da Internet
436600131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
436700131223
436800131223         //?Imposto se commissionato
436900140407           IF  wCom;
437000160714             dorm01.§orcomc = FIORA00_SI;
437100131223           ELSE;
437200160714             dorm01.§orcomc = FIORA00_NO;
437300131223           ENDIF;
437400140506
437500140507         //?Se ORM commissionato e data ritiro passata al pgm = 0
437600140507         //?vuol dire che non è ancora stata calcolata quindi siamo
437700140507         //?nel passaggio tra la prima e la seconda pagina
437800140507           IF  wCom and fiora0I0.vaodar = 0;
437900140507           //?se mail e/o sms impostati torno 'S' nei flag degli alert
438000140507             IF  fiora0O0.mail <> *blanks;
438100160714               fiora0O0.alertmail = FIORA00_SI;
438200140507             ENDIF;
438300140507             IF  fiora0O0.sms <> *blanks;
438400160714               fiora0O0.alertsms = FIORA00_SI;
438500140507             ENDIF;
438600140507           ENDIF;
438700140507
438800140507         //?Se ORM commissionato e data ritiro (calcolata o impostata dall'utente) = oggi
438900140506         //?imposto i flag alert a 'N'
439000140508         //?ma se data ritiro passata è > di 0, in questo caso vuol dire che siamo già
439100140508         //?in seconda pagina e l'utente ha messo oggi al posto di domani
439200140508           IF  wCom and fiora0O0.vaodar = dataoggi and fiora0I0.vaodar > 0;
439300140507             IF  fiora0O0.mail <> *blanks;
439400160714               fiora0O0.alertmail = FIORA00_NO;
439500140506             ENDIF;
439600140507             IF  fiora0O0.sms <> *blanks;
439700160714               fiora0O0.alertsms = FIORA00_NO;
439800140506             ENDIF;
439900140506           ENDIF;
440000131009
440100131009         ENDIF;
440200131223
440300131227         fiora0O0.idrichiamo = fiora0I0.idrichiamo;
440400131223         fiora0O0.idlingua = fiora0I0.idlingua;
440500140402
440600140402         IF  fiora0I0.vaorfa <> *blanks;
440700140402           exec sql
440800140402           set :fiora0O0.vaorfa = ucase (:fiora0I0.vaorfa);
440900140402         ENDIF;
441000131223
441100131223         IF  fiora0I0.vaono1 <> *blanks;
441200140211           exec sql
441300140211           set :fiora0O0.vaono1 = ucase (:fiora0I0.vaono1);
441400131223         ENDIF;
441500131223         IF  fiora0I0.vaono2 <> *blanks;
441600140211           exec sql
441700140211           set :fiora0O0.vaono2 = ucase (:fiora0I0.vaono2);
441800131223         ENDIF;
441900131223
442000131205         fiora0O0.vaoflo = dorm01;
442100131009
442200131009       ENDSR;
442300140408
442400140408       //--------------------------------------------------------------
442500140408       //?Richiamo pgm per calcolo orari servizio.
442600140408       //--------------------------------------------------------------
442700140408       BEGSR  CallTrulORS;
442800140408
442900140408         clear TRULORSds;
443000140408         trulorsds.IOREfil  = wPor;
443100140408         trulorsds.IORecap  = wCap;
443200140408         trulorsds.IOREloc  = wLocalita;
443300140408         trulorsds.IOREdta  = wData;
443400140408         trulorsds.IOREtser = 'R';
443500170404
443600170404       //?Se peso inferiore a 5 Kg. devo far vedere gli orari servizio estesi
443700170404       //?non è da fare per i ritiri esteri
443800170404         clear dIORE01;
443900170529         IF  wPeso > 0 and wPeso < dDFT.d§DFTpkgdt and wNazione = *blanks;
444000170404           diore01.§IOREr_mnx = 'S';
444100170404         ENDIF;
444200170404         trulorsds.IOREflo = dIORE01;
444300170404
444400140408         trulorsr (kpjba:trulorsds:trulor2ds);
444500140408
444600140408       ENDSR;
444700160526
444800160526       //--------------------------------------------------------------
444900160526       //?Richiamo pgm per calcolo orari servizio estero.
445000160526       //--------------------------------------------------------------
445100160526       BEGSR  CallFIORE;
445200160526
445300160526         clear FIORE00ds;
445400160526         fiore00ds.IORE00por = wPor;
445500160526         fiore00ds.IORE00nar = wNazione;
445600160526         fiore00r (fiore00ds);
445700160526
445800160526       ENDSR;
445900140408
446000140408       //--------------------------------------------------------------
446100140408       //?Imposto la frase dal tornare al chiamante.                   ?
446200140408       //--------------------------------------------------------------
446300140408       BEGSR  GetFrase1;
446400140408
446500140408       //?Se ID lingua non valido o non passato
446600140408       //?assumo italiano
446700140408         IF  fiora0I1.idlingua = *blanks or
446800140408            (fiora0I1.idlingua <> FIORA00_ID_LINGUA_IT and
446900140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_FR and
447000140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_EN and
447100140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_DE);
447200140408            fiora0I1.idlingua = FIORA00_ID_LINGUA_IT;
447300140408         ENDIF;
447400140408
447500140408       //?Cerco gli orari di servizio
447600140408         wPor      = fiora0I1.filritiro;
447700140408         wData     = dataoggi;
447800140408         wCap      = fiora0I1.cap;
447900140408         wLocalita = fiora0I1.localita;
448000140408         exsr CallTrulORS;
448100160526
448200160526       //?Dalla versione "C" cerco la frase anche per i ritiri esteri
448300160526         SELECT;
448400160531         WHEN  fiora0I1.versione > 'B';
448500160531           wNetworkRit = GetNtwFiliale(fiora0I1.filritiro);
448600160531           clear dNTW;
448700160531           k_TBEcod = 'NTW';
448800160531           k_TBEke1 = wNetworkRit;
448900160531           chain (k_TBEcod:k_TBEke1) TNTBE01L;
449000160531           IF  %found(TNTBE01L);
449100160531             dNTW = TBEuni;
449200160531           ENDIF;
449300160531           IF  wNetworkRit = FIORA00_NTW_DPD or
449400160531               dntw.§NTWfie = FIORA00_NTW_ESTERO;
449500160526             wPor     = fiora0I1.filritiro;
449600160526             wNazione = fiora0I1.nazione;
449700160526             exsr CallFIORE;
449800160526           ENDIF;
449900160526         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
450000160526         //?non devo emettere la frase dato che la merce parte sempre il
450100160526         //?giorno dopo e la data ritiro è sempre il giorno dopo
450200160526           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
450300160526               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
450400160526             clear fiora0O1.frase;
450500160526             fiora0O1.idlingua = fiora0I1.idlingua;
450600160526             leavesr;
450700160526           ENDIF;
450800160526         //?Se non è un prepagato imposto la frase
450900160526           wIdMsg   = 'TIS0827';
451000160526           SELECT;
451100160526           //?Ritiri esteri
451200160526           WHEN  fiora0I1.nazione <> *blanks;
451300160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
451400160526           //?Ritiri commissionati
451500160714           WHEN  fiora0I1.contattot = FIORA00_SI;
451600160526             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
451700160526           OTHER;
451800160526           //?Ritiri normali
451900160526             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
452000160526           ENDSL;
452100170914
452200160526         OTHER;
452300140408
452400140408       //?Imposto la frase
452500151104       //?Frasi diverse in base alla versione
452600151104         IF  fiora0I1.versione > 'A';
452700151104         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
452800151104         //?non devo emettere la frase dato che la merce parte sempre il
452900151104         //?giorno dopo e la data ritiro è sempre il giorno dopo
453000151104           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
453100151104               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
453200151104             clear fiora0O1.frase;
453300151104             fiora0O1.idlingua = fiora0I1.idlingua;
453400151104             leavesr;
453500151104           ENDIF;
453600151104         //?Se cliente codificato imposto la nuova frase
453700151104           wIdMsg   = 'TIS0827';
453800160714           IF  fiora0I1.contattot = FIORA00_SI;
453900151104             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
454000151104           ELSE;
454100151104             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
454200151104           ENDIF;
454300151104         ELSE;
454400140408         wIdMsg   = 'TIS0731';
454500160714         IF  fiora0I1.contattot = FIORA00_SI;
454600140408           wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
454700140408         ELSE;
454800140408           wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
454900140408         ENDIF;
455000151104         ENDIF;
455100160526
455200160526         ENDSL;
455300140408
455400160531       //?Per ora ND ha deciso di non impostare la frase per i ritiri esteri
455500160531         IF  fiora0I1.nazione <> *blanks and SaltaFraseExport <> *blanks;
455600160531           clear fiora0O1.frase;
455700160531           leavesr;
455800160531         ENDIF;
455900170914
456000140408         fiora0O1.frase =
456100140408         rtvMsgLang(wIdMsg:fiora0I1.idlingua:wParm);
456200140408
456300140408         fiora0O1.idlingua = fiora0I1.idlingua;
456400170914
456500170914       //?Dalla versione "E", del richiamo Immetti, non emetto la frase se cliente anonimo
456600170914         IF  fiora0I0.versione > 'D' and fiora0I1.anonimo = FIORA00_ANONIMO;
456700170914           clear fiora0O1.frase;
456800170914         ENDIF;
456900140408
457000140408       ENDSR;
457100131010
457200131010       //--------------------------------------------------------------
457300131010       //?Operazioni finali.
457400131010       //--------------------------------------------------------------
457500131010       BEGSR  RoutEnd;
457600140408
457700140408       //?Se richiamato per GET_FRASE1
457800140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
457900140408           %subst(prmRpyData:1:prmRpyDataSize) = fiora0O1;
458000140408         ELSE;
458100131010
458200131219         %subst(prmRpyData:1:prmRpyDataSize) = fiora0O0;
458300131219         %subst(prmRpyMessage:1:prmRpyMsgSize) = fiora0M0;
458400131219         %subst(prmRpyForzatu:1:prmRpyForSize) = fiora0F0;
458500140408
458600140408         ENDIF;
458700131010
458800131010         return;
458900131010
459000131010       ENDSR;
459100131009
459200131007      /end-free
459300140312
459400140313      *--------------------------------------------------
459500140313      * Procedure name: IsCodiceLegato
459600140313      * Purpose:        Controlla codice
459700140402      * Returns:        ON - OFF
459800140313      *--------------------------------------------------
459900140313     p IsCodiceLegato  B
460000140313     d IsCodiceLegato  PI              n
460100140312
460200140313      /copy gaitrasrc/srcconst,FIORB00R
460300140313     d isErrore        s               n   inz
460400140313     d RpyIdMsg        s             10i 0
460500140313     d RpyOpCode       s             10i 0
460600140313     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_LEGGI)
460700140313     d wNrRec          s              7s 0 inz
460800140313     d fiorb0I0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
460900140313     d fiorb0O0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
461000140313     d  skCodice                     10a   dim(10) overlay(Codice)
461100140313      /copy gaitrasrc/srcprotopr,FIORB00R
461200140312
461300140313      /free
461400140312
461500140313       reset isErrore;
461600140313       reset RpyIdMsg;
461700140313       reset RpyOpCode;
461800140313       reset fiorb0I0;
461900140313       reset fiorb0O0;
462000140312
462100140313       fiorb0I0.codiceksu = *all'0';
462200140313       %subst(fiorb0I0.codiceKsu:2:7) =
462300140312       %subst(%editc(wCodice:'X'):1:7);
462400140312       //?Solo per richiamo da Internet
462500140313       IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
462600140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
462700140313       ELSE;
462800140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
462900140313       ENDIF;
463000140312
463100160519       DOU  wNrRec = fiorb0O0.clientitot
463200140313         OR fiorb0O0.nrrec = *ZERO;
463300140313
463400140313         MONITOR;
463500140313           Fiorb00_Anagrafica( RqsOpCode
463600140313                             : RpyOpCode
463700140313                             : RpyIdMsg
463800140313                             : fiorb0I0.formato
463900140313                             : fiorb0I0
464000140313                             : %SIZE(fiorb0I0)
464100140313                             : fiorb0O0.formato
464200140313                             : fiorb0O0
464300140313                             : %SIZE(fiorb0O0)
464400140313                             );
464500140313         ON-ERROR;
464600140313           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
464700140313         ENDMON;
464800140312
464900160519         IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
465000140313           isErrore = *on;
465100140313           leave;
465200140313         ENDIF;
465300140313
465400140313         //?Controllo se il codice fa parte della stessa famiglia
465500140313         //?se fa parte esco dal LOOP
465600140313         IF  %lookup(%editc(wCodice:'X'):fiorb0O0.skCodice) > 0;
465700140313           leave;
465800140313         ENDIF;
465900140313
466000140313         wNrRec += 1;
466100140313         RqsOpCode = FIORB00_RQSOPCODE_CARICA;
466200140312
466300140313       ENDDO;
466400140312
466500140313       Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
466600140313                           : RpyOpCode
466700140313                           : RpyIdMsg
466800140312                           );
466900140313
467000140313       IF  isErrore;
467100140313         return *off;
467200140313       ENDIF;
467300140312
467400140313       RETURN *on;
467500140312
467600140312      /END-FREE
467700140313     p IsCodiceLegato  E
467800140312
467900140401      *--------------------------------------------------
468000140401      * Procedure name: IsCodiceValido
468100140401      * Purpose:        Controlla codice
468200140402      * Returns:        ON - OFF
468300140401      *--------------------------------------------------
468400140401     p IsCodiceValido  B
468500140401     d IsCodiceValido  PI              n
468600140402     d  FIORB0O1                           likeds(FIORB0OO)
468700140401
468800140401      /copy gaitrasrc/srcconst,FIORB00R
468900140401     d isErrore        s               n   inz
469000140401     d RpyIdMsg        s             10i 0
469100140401     d RpyOpCode       s             10i 0
469200140401     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
469300140401     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
469400140401      /copy gaitrasrc/srcprotopr,FIORB00R
469500140401
469600140401      /free
469700140401
469800140401       reset isErrore;
469900140401       reset RpyIdMsg;
470000140401       reset RpyOpCode;
470100140401       reset fiorb0I1;
470200140401
470300140401       fiorb0I1.codice = wCodice;
470400140401       MONITOR;
470500140401         Fiorb00_Anagrafica( RqsOpCode
470600140401                           : RpyOpCode
470700140401                           : RpyIdMsg
470800140401                           : fiorb0I1.formato
470900140401                           : fiorb0I1
471000140401                           : %SIZE(fiorb0I1)
471100140401                           : fiorb0O1.formato
471200140401                           : fiorb0O1
471300140401                           : %SIZE(fiorb0O1)
471400140401                           );
471500140401         ON-ERROR;
471600140401           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
471700140401         ENDMON;
471800140401
471900160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
472000140401         isErrore = *on;
472100140401       ENDIF;
472200160519
472300160519       IF  RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND and
472400160519           fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
472500160519         isErrore = *on;
472600160519       ENDIF;
472700140401
472800140401       IF  isErrore;
472900140401         return *off;
473000140402       ENDIF;
473100140402
473200140402       RETURN *on;
473300140401
473400140401      /END-FREE
473500140401     p IsCodiceValido  E
473600140411
473700140411     P*--------------------------------------------------
473800140411     P* Procedure name: GetOraAggiustata
473900140411     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
474000140411     P* Returns:        Ora e minuto aggiustati.
474100140411     P* Parameter:      hhmm => Ora e minuto
474200140411     P*--------------------------------------------------
474300140411     P GetOraAggiustata...
474400140411     P                 B
474500140411     D GetOraAggiustata...
474600140411     D                 PI             4A
474700140411     D  hhmm                          4S 0 CONST
474800140411     D retField        DS             4    QUALIFIED STATIC
474900140411     D  hhmm                   1      4S 0
475000140411     D  hh                     1      2S 0
475100140411     D  mm                     3      4S 0
475200140411
475300140411      /FREE
475400140411
475500140411       IF hhmm = *ZERO;
475600140411         RETURN *BLANK;
475700140411       ENDIF;
475800140411
475900140411       retField.hhmm = hhmm;
476000140411
476100140411       SELECT;
476200140411         WHEN retField.mm = 00 OR retField.mm = 15
476300140411           OR retField.mm = 30 OR retField.mm = 45;
476400140411           // Non è da aggiustare.
476500140411         WHEN retField.mm < 15;
476600140411           retField.mm = 15;
476700140411         WHEN retField.mm < 30;
476800140411           retField.mm = 30;
476900140411         WHEN retField.mm < 45;
477000140411           retField.mm = 45;
477100140411         OTHER;
477200140411           retField.hh += 1;
477300140411           retField.mm = 00;
477400140411       ENDSL;
477500140411
477600140411       RETURN retField;
477700140411
477800140411      /end-free
477900140411     P GetOraAggiustata...
478000140411     P                 E
478100160315
478200160315      *--------------------------------------------------
478300160315      * Procedure name: IsFilValida
478400160315      * Purpose:        Ritorna se filiale è valida
478500160315      * Returns:        On - OFF
478600160315      * Parameter:      Filiale
478700160315      *--------------------------------------------------
478800160315     p IsFilValida     B
478900160315     d IsFilValida     PI              n
479000160315     D  filiale                       3s 0 CONST
479100160315
479200160315      /free
479300160315
479400160315       IF  filiale = *zeros;
479500160315         RETURN *off;
479600160315       ENDIF;
479700160315
479800160315       chain (filiale) AZORG01L;
479900160315       IF  not %found(AZORG01L);
480000160315         RETURN *off;
480100160315       ENDIF;
480200160315       IF  ORGfva <> *blanks;
480300160315         RETURN *off;
480400160315       ENDIF;
480500160315       IF  ORGfag <> 'F' and ORGfag <> 'A';
480600160315         RETURN *off;
480700160315       ENDIF;
480800160315
480900160315       RETURN *on;
481000160315
481100160315      /end-free
481200160315     P IsFilValida     E
481300160315
481400160315      *--------------------------------------------------
481500160315      * Procedure name: GetNtwFiliale
481600160315      * Purpose:        Recupera il Network della filiale
481700160315      * Returns:        Network
481800160315      * Parameter:      Filiale
481900160315      *--------------------------------------------------
482000160315     p GetNtwFiliale   B
482100160315     d GetNtwFiliale   PI            20a
482200160315     D  filiale                       3s 0 CONST
482300160315
482400160315      // ds per campo ORGDE3 di AZORG
482500160315     d OG143         e ds
482600160315
482700160315      /free
482800160315       IF  filiale = *zeros;
482900160315         RETURN *blanks;
483000160315       ENDIF;
483100160315
483200160315       clear OG143;
483300160315       chain (filiale) AZORG01L;
483400160315
483500160315       OG143 = ORGde3;
483600160315
483700160315       RETURN §OGntw;
483800160315
483900160315      /end-free
484000160315     P GetNtwFiliale   E
484100160315
484200160315      *--------------------------------------------------
484300160315      * Procedure name: IsFilAttivaORM
484400160315      * Purpose:        Ritorna se filiale attiva alla procedura ORM
484500160315      * Returns:        On - OFF
484600160315      * Parameter:      Filiale
484700160315      *--------------------------------------------------
484800160315     p IsFilAttivaORM  B
484900160315     d IsFilAttivaORM  PI              n
485000160315     D  filiale                       3s 0 CONST
485100160315
485200160315      // ds per campo ORGDE8 di AZORG
485300160315     d OG148         e ds
485400160315
485500160315      /free
485600160315
485700160315       IF  filiale = *zeros;
485800160315         RETURN *off;
485900160315       ENDIF;
486000160315
486100160315       clear OG148;
486200160315
486300160315       chain (filiale) AZORG01L;
486400160315
486500160315       OG148 = ORGde8;
486600160315
486700160315       IF  §OGorm <> 'S';
486800160315         return *off;
486900160315       ENDIF;
487000160315
487100160315       RETURN *on;
487200160315
487300160315      /end-free
487400160315     P IsFilAttivaORM  E
487500160315
487600160315      *--------------------------------------------------
487700160315      * Procedure name: IsKscValido
487800160315      * Purpose:        Ritorna se Ksc è valido
487900160315      * Returns:        On - OFF
488000160315      * Parameter:      cliente
488100160315      *--------------------------------------------------
488200160315     p IsKscValido     B
488300160315     d IsKscValido     PI              n
488400160315     D  cliente                       7s 0 CONST
488500160315
488600160315      // campi di comodo
488700160315     d filiale         s              3s 0
488800160401     d k_kcc           s                   like(ACOkcc) inz(151)
488900160401     d k_kut           s                   like(ACOkut) inz(1)
489000160315     d network         s              3a
489100160315
489200160315      /free
489300160315
489400160315       IF  cliente = *zeros;
489500160315         RETURN *off;
489600160315       ENDIF;
489700160315
489800160401       chain (k_kut:k_kcc:cliente) CNACO00F;
489900160315       IF  not %found(CNACO00F);
490000160315         RETURN *off;
490100160315       ENDIF;
490200160315       IF  ACOflg <> *blanks;
490300160315         RETURN *off;
490400160315       ENDIF;
490500160315
490600160315      // cerco il ntw del cliente non posso accettare clienti
490700160315      // logistica o di sede
490800160401       filiale = %dec(%subst(%editc(cliente:'X'):1:3):3:0);
490900160315       network = GetNtwFiliale(filiale);
491000160315       IF  network = 'LOG' or network = 'XXX';
491100160315         RETURN *off;
491200160315       ENDIF;
491300160315
491400160315       RETURN *on;
491500160315
491600160315      /end-free
491700160315     P IsKscValido     E
491800160315
491900160315      *--------------------------------------------------
492000160315      * Procedure name: IsValidoCapDPD
492100160315      * Purpose:        Ritorna se Cap DPD valido
492200160315      * Returns:        On - OFF
492300160315      * Parameter:      nazione - cap del mittente
492400160315      *--------------------------------------------------
492500160315     p IsValidoCapDPD  B
492600160315     d IsValidoCapDPD  PI              n
492700160315     D  nazione                       3a   CONST
492800160315     D  cap                           9a   CONST
492900160315     D  data                          8s 0 CONST
493000160315     D  peso                          7s 1 CONST
493100160315     D  limite                        7s 1 CONST
493200160315     D  filiale                       3s 0 CONST
493300160315
493400160315      // ds per cappario DPD
493500160315     d TISIE3DS      e ds                  QUALIFIED INZ
493600160315      // pgm per cappario DPD
493700160315     d TISIE3R         pr                  extpgm('TISIE3R')
493800160315     d  tisie3ds                           likeds(TISIE3DS)
493900160315
494000160315      /free
494100160315
494200160315       IF  nazione = *blanks or cap = *blanks or filiale = 0;
494300160315         RETURN *off;
494400160315       ENDIF;
494500160315
494600160315       clear TISIE3DS;
494700170505       tisie3ds.ISIE3dri = %dec(%date());
494800160315       IF  data = 0;
494900170505         tisie3ds.ISIE3dsp = %dec(%date());
495000160315       ELSE;
495100170505         tisie3ds.ISIE3dsp = data;
495200160315       ENDIF;
495300160315
495400160315       tisie3ds.ISIE3hsp = %dec(%time());
495500160315       tisie3ds.ISIE3nzd = nazione;
495600160315       tisie3ds.ISIE3cad = cap;
495700160315       IF  peso > limite;
495800160315         tisie3ds.ISIE3srv = 101;
495900160315       ELSE;
496000160315         tisie3ds.ISIE3srv = 136;
496100160315       ENDIF;
496200160315       tisie3ds.ISIE3lnp = filiale;
496300160315       tisie3r (tisie3ds);
496400160315       IF  tisie3ds.OSIE3err <> *blanks;
496500160315         RETURN *off;
496600160315       ENDIF;
496700160315
496800160315       RETURN *on;
496900160315
497000160315      /end-free
497100160315     P IsValidoCapDPD  E
