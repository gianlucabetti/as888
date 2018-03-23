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
002400131009
002500131009       // -?File Tabelle
002600131009     fTABEL00F  if   e           k disk
002700131010     fTNTBE01L  if   e           k disk
002800160315
002900160315       // -?File Tariffe
003000160315     fTNTAM01L  if   e           k disk
003100131008
003200131007       //--------------------------------------------------------------
003300131007       //?Definizione costanti.                                        ?
003400131007       //--------------------------------------------------------------
003500131007      /copy gaitrasrc/srcconst,FIORA00R
003600140114
003700140114     d c_Digits        c                   const('0123456789')
003800160531     d SaltaFraseExport...
003900160531     d                 c                   const('S')
004000131008
004100131007       //--------------------------------------------------------------
004200131007       //?Definizione strutture dati.                                  ?
004300131007       //--------------------------------------------------------------
004400131007       // -?Dati INPUT
004500131009     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
004600140408     d FIORA0I1      e ds                  QUALIFIED INZ(*EXTDFT)
004700131007
004800131007       // -?Dati OUTPUT
004900131009     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
005000140408     d FIORA0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005100140401     d FIORB0OO      e ds                  EXTNAME(FIORB0O1)
005200140401     d                                     QUALIFIED INZ(*EXTDFT)
005300140401     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
005400140401     d                                     QUALIFIED INZ(*EXTDFT)
005500140401     d FIORB0OC      e ds                  EXTNAME(FIORB0O1)
005600140401     d                                     QUALIFIED INZ(*EXTDFT)
005700131007
005800131007       // -?Messaggi
005900131009     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
006000131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006100131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006200131219     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
006300131219     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
006400131007
006500131007       // -?Forzature
006600131009     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
006700131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006800131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006900131219     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
007000160315
007100160315       // -?Controllo giro ritiro
007200160315     d FIDG09DS      e ds                  QUALIFIED INZ
007300160526
007400160526       // -?Ritorna orari servizio ritiri estero
007500160526     d FIORE00DS     e ds                  QUALIFIED INZ
007600140312
007700131205       // -?Controllo destinatario per ORM export DPD
007800131205     d FIOR95DS      e ds                  QUALIFIED INZ
007900131010
008000131010       // -?Forzatura filiale ritiro
008100131204     d FIOR96DS      e ds                  QUALIFIED INZ
008200131010
008300131010       // -?Calcolo data ritiro
008400131204     d FIOR97DS      e ds                  QUALIFIED INZ
008500131205
008600131205       // -?Ritirabilità all'estero
008700131205     d FNLV12DS      e ds                  QUALIFIED INZ
008800151009
008900151009       // -?Controllo cap/località/provincia
009000151009     d FNLV13DS      e ds                  QUALIFIED INZ
009100160315
009200160315       // -?Controllo Indirizzo
009300160315     d FNLV14DS      e ds                  QUALIFIED INZ
009400160315
009500160315       // -?Ricerca terminal
009600160315     d FNLV55DS      e ds                  QUALIFIED INZ
009700160922
009800160922       // -?Controllo limiti quantità
009900160922     d TRUL73DS      e ds                  QUALIFIED INZ
010000160922     d TRUL731DS     e ds                  QUALIFIED INZ
010100131223
010200131223       // -?Calcola orari di servizio
010300140327     d TRULORSDS     e ds                  QUALIFIED INZ
010400140327     d TRULOR2ds     e ds                  QUALIFIED INZ
010500131009
010600131204       // -?Controllo instradamento
010700131204     d TISI95DS      e ds                  QUALIFIED INZ
010800131205     d TISI97DS      e ds                  QUALIFIED INZ
010900131204
011000131204       // -?Controllo orari apertura
011100131204     d TRUL03DS      e ds                  QUALIFIED INZ
011200160408
011300160408       // -?Ritorna dati Servizio Clienti
011400160408     d TRULDSCDS     e ds                  QUALIFIED INZ
011500131008
011600131008       // -?Controllo partita IVA
011700151020     d XCFIVA1DS     e ds                  QUALIFIED INZ
011800140311
011900140311       // -?Controllo indirizzo mail
012000140311     d dsemail       e ds                  QUALIFIED INZ
012100160208
012200160208       // -?Tabella 15 - Nazioni
012300160208     d ds15          e ds                  QUALIFIED INZ
012400131204
012500131204       // -?Tabella 3I - DPD
012600131204     d ds3Idp        e ds                  QUALIFIED INZ
012700151008
012800151008       // -?Tabella DFT - Default ORM
012900151008     d dDFT          e ds                  QUALIFIED INZ
013000160315
013100160315       // -?Tabella FFC - Forza Filiale Consenga
013200160315     d dFFC          e ds                  QUALIFIED INZ
013300131010
013400131010       // -?Tabella NTW - Network
013500131204     d dNTW          e ds                  QUALIFIED INZ
013600140114
013700140114       // -?Campo ACRMAI
013800140114     d dACR01        e ds                  QUALIFIED INZ
013900131010
014000131010       // -?Campo VAOFLO
014100131204     d dORM01        e ds                  QUALIFIED INZ
014200131008
014300131008       // -?Partita IVA
014400131009     d wIVA            ds
014500131008     d  wISO                   1      2
014600131008     d  wPIVA                  3     16
014700131009
014800131009       //--------------------------------------------------------------
014900131009       //?Definizione schiere.
015000131009       //--------------------------------------------------------------
015100131009     d skprv           s              2a   dim(200)
015200131218     d sknaz           s              3a   dim(500)
015300160208     d sknazd          s             25a   dim(500)
015400131007
015500131007       //--------------------------------------------------------------
015600131007       //?Definizione campi.
015700131007       //--------------------------------------------------------------
015800131009     d dataoggi        s              8s 0
015900131220     d inv_dataritiro  s              8s 0
016000131010     d kpjba           s            502a
016100131205     d k_ORGfil        s                   like(ORGfil)
016200131205     d k_TBEcod        s                   like(TBEcod)
016300131205     d k_TBEke1        s                   like(TBEke1)
016400160315     d k_TBEke2        s                   like(TBEke2)
016500131205     d k_TBLcod        s                   like(TBLcod)
016600131205     d k_TBLkey        s                   like(TBLkey)
016700131010     d k_TBLkut        s                   like(TBLkut)
016800160315     d k_TAMksc        s                   like(TAMksc)
016900160315     d k_TAMctr        s                   like(TAMctr)
017000160315     d keyfilritiro    s                   like(wFiliale)
017100160315     d keytabella      s             15a
017200160922     d old_vaoncl      s                   like(fiora0I0.vaoncl)
017300160922     d old_vaopkg      s                   like(fiora0I0.vaopkg)
017400160922     d old_vaovlm      s                   like(fiora0I0.vaovlm)
017500160922     d old_vaobnc      s                   like(fiora0I0.vaobnc)
017600140411     d oraalfa         s              4a   inz
017700160216     d parametroNPR    s             10a   inz
017800140402     d wCap            s                   inz like(fiora0I0.vaocar)
017900140401     d wCodice         s             10s 0
018000131010     d wCom            s               n   inz
018100140408     d wData           s              8s 0 inz
018200131009     d wErr            s               n   inz
018300131219     d wErrMitt        s               n   inz
018400160613     d wErrQta         s               n   inz
018500160315     d wFil            s              3s 0 inz
018600131204     d wFiliale        s                   like(tisi95ds.O95lna)
018700160315     d wForza          s               n   inz
018800140401     d wHH             s              2s 0
018900131009     d wIdMsg          s              7a
019000131008     d wIdCampo        s             10a
019100140402     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
019200140402     d wLocalita       s                   inz like(fiora0I0.vaolor)
019300140401     d wMM             s              2s 0
019400140402     d wNazione        s                   inz like(fiora0I0.vaonar)
019500160315     d wNetworkCon     s              3a
019600160315     d wNetworkEmi     s              3a
019700160315     d wNetworkOrd     s              3a
019800131205     d wNetworkRit     s              3a
019900140211     d wNrForza        s              3s 0
020000140211     d wNrfForza       s              3s 0
020100160315     d wOKffc          s               n   inz
020200131204     d wOKfilritiro    s               n   inz
020300160315     d wOKtariffa      s               n   inz
020400140401     d wOra            s              6s 0
020500131204     d wParm           s            512a   inz
020600131204     d wPeso           s                   like(tisi95ds.I95lkg)
020700160606     d wPesoaut        s             10s 1
020800160606     d wPesoblc        s             10s 1
020900160606     d wPesomax        s             10s 1
021000160606     d wPesomtc        s             10s 1
021100160315     d wPickup         s                   like(tisi95ds.O95gf2)
021200140408     d wPor            s                   inz like(fiora0I0.vaopor)
021300140402     d wProvincia      s                   inz like(fiora0I0.vaoprr)
021400151008     d wVolume         s                   like(tisi95ds.I95lmc)
021500160606     d wVolumeaut      s             10s 3
021600160606     d wVolumeblc      s             10s 3
021700151008     d wVolumemax      s             10s 3
021800160606     d wVolumemtc      s             10s 3
021900131009     d wVuoto          s              1a
022000131009     d xx              s              3s 0
022100131008
022200131008       //--------------------------------------------------------------
022300131008       //?Definizione procedure.
022400131008       //--------------------------------------------------------------
022500160315       // -?Controllo giro di ritiro
022600160315     d fidg09r         pr                  extpgm('FIDG09R')
022700160315     d  kpjba                       502a
022800160526
022900160526       // -?Ritorna Orari Servizio Ritiro Estero
023000160526     d fiore00r        pr                  extpgm('FIORE00R')
023100160526     d  fiore00ds                          likeds(FIORE00DS)
023200131205
023300131205       // -?Controllo destinatario per ORM export DPD
023400131205     d fior95r         pr                  extpgm('FIOR95R')
023500131205     d  kpjba                       502a
023600131205     d  fior95ds                           likeds(FIOR95ds)
023700131010
023800131010       // -?Forzatura filiale ritiro
023900131010     d fior96r         pr                  extpgm('FIOR96R')
024000131010     d  kpjba                       502a
024100131010     d  fior96ds                           likeds(FIOR96ds)
024200131204
024300131204       // -?Calcolo data ritiro
024400131204     d fior97r         pr                  extpgm('FIOR97R')
024500131204     d  kpjba                       502a
024600131204     d  fior97ds                           likeds(FIOR97ds)
024700131205
024800131205       // -?Controllo ritirabilità all'estero
024900131205     d fnlv12r         pr                  extpgm('FNLV12R')
025000131205     d  fnlv12ds                           likeds(FNLV12DS)
025100131205     d  tisi95ds                           likeds(TISI95DS)
025200131205     d  tisi97ds                           likeds(TISI97DS)
025300151009
025400151009       // -?Controllo cap/località/provincia
025500151009     d fnlv13r         pr                  extpgm('FNLV13R')
025600151009     d  kpjba                       502a
025700151009     d  fnlv13ds                           likeds(FNLV13DS)
025800151009     d  tisi95ds                           likeds(TISI95DS)
025900160315
026000160315       // -?Pgm controllo indirizzo
026100160315     d FNLV14R         pr                  extpgm('FNLV14R')
026200160315     d  kpjba                       502a
026300160315     d  fnlv14ds                           likeds(FNLV14DS)
026400160315
026500160315       // -?Controllo terminal
026600160315     d fnlv55r         pr                  extpgm('FNLV55R')
026700160315     d  fnlv55ds                           likeds(FNLV55DS)
026800160216
026900160216       // -?Recupero nuovo Numero Prenotazione Ritiro
027000160216     d GetNPR          pr            10
027100160216     d  ParametroNPR                 10
027200160922
027300160922       // -?Controlla limiti quantità
027400160922     d trul73r         pr                  extpgm('TRUL73R')
027500160922     d  trul73ds                           likeds(TRUL73DS)
027600160922     d trul731r        pr                  extpgm('TRUL731R')
027700160922     d  trul731ds                          likeds(TRUL731DS)
027800131223
027900131223       // -?Calcolo orari servizio
028000140327     d trulorsr        pr                  extpgm('TRULORSR')
028100131223     d  kpjba                       502a
028200140327     d  trulorsds                          likeds(TRULORSDS)
028300140327     d  trulor2ds                          likeds(TRULOR2DS)
028400140327     d                                     options (*nopass)
028500131204
028600131204       // -?Controllo orari apertura
028700131204     d trul03r         pr                  extpgm('TRUL03R')
028800131204     d  trul03ds                           likeds(TRUL03ds)
028900160408
029000160408       // -?Ritorna dati Servizio Clienti
029100160408     d truldscr        pr                  extpgm('TRULDSCR')
029200160408     d  truldscds                          likeds(TRULDSCDS)
029300131008
029400131008       //--------------------------------------------------------------
029500131008       //?Definizione prototipi.
029600131008       //--------------------------------------------------------------
029700131008      /copy gaitrasrc/srcprotopr,FIORA00R
029800131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
029900131230      /copy gaitrasrc/srcprotopr,TIBS0800R
030000160315      /copy gaitrasrc/srcprotopr,TISI95R
030100140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
030200140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
030300151020      /copy gaitrasrc/srcprotopr,XCFIVAR1
030400140311      /copy gaitrasrc/srcprotopr,XEMAIL
030500131219
030600131219       //--------------------------------------------------------------
030700131219       //?Definizione parametri programma.
030800131219       //--------------------------------------------------------------
030900131219     d Fiora00_Immetti...
031000131219     d                 PI
031100131219     d prmRqsOpCode...
031200140210     d                               10I 0 CONST
031300131219     d prmRpyOpCode...
031400131219     d                               10I 0
031500131219     d prmRpyIdMsg...
031600131219     d                               10I 0
031700131219     d prmRqsFormato...
031800140210     d                               10A   CONST
031900131219     d prmRqsData...
032000131219     d                            32767A   OPTIONS(*VARSIZE)
032100131219     d prmRqsDataSize...
032200140210     d                               10I 0 CONST
032300131219     d prmRpyFormato...
032400140210     d                               10A   CONST
032500131219     d prmRpyData...
032600131219     d                            32767A   OPTIONS(*VARSIZE)
032700131219     d prmRpyDataSize...
032800140210     d                               10I 0 CONST
032900131219     d prmRpyFormMsg...
033000140408     d                               10A   CONST OPTIONS(*NOPASS)
033100131219     d prmRpyMessage...
033200140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033300131219     d prmRpyMsgSize...
033400140408     d                               10I 0 CONST OPTIONS(*NOPASS)
033500131219     d prmRpyFormForz...
033600140408     d                               10A   CONST OPTIONS(*NOPASS)
033700131219     d prmRpyForzatu...
033800140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
033900131219     d prmRpyForSize...
034000140408     d                               10I 0 CONST OPTIONS(*NOPASS)
034100131007
034200131007       //--------------------------------------------------------------
034300131007       //?MAIN.
034400131007       //--------------------------------------------------------------
034500131008
034600131007      /free
034700131008
034800131008       //?Operazioni iniziali
034900131008       exsr RoutInz;
035000131008
035100131008       //?Controllo formale dei dati passati
035200131008       exsr Controlla;
035300140408
035400140408       //?Solo per formato FIORA0I0 = a richiamo <> da GET_FRASE1
035500140408       IF  fiora0I0.formato = prmRqsFormato;
035600140408
035700140408       //?Carico Tabelle
035800140408         exsr CaricaTab;
035900131007
036000131008       //?Imposto alcuni campi di DFT se non passati
036100131010         exsr DftInput;
036200131007
036300131007       //?Elaborazione principale per ORM da Internet
036400131010         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
036500131010           exsr Internet;
036600131010         ENDIF;
036700140620
036800160216       //?Elaborazione principale per ORM da AS400
036900140620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
037000140620           exsr AS400;
037100140620         ENDIF;
037200131009
037300131010       //?Imposto i dati di ritorno nella ds
037400131010       //?quelli che non sono già stati impostati dalle varie routine
037500131010         exsr DsOutput;
037600160216
037700160318       //?Dalla 'C' della ds di output FIORA0O0
037800160216       //?Se arrivo senza errori e sono stata richiamata con
037900160216       //?id richiamo OpCode WRITENPR
038000160216       //?devo staccare un nuovo numero NPR e tornarlo al chiamante
038100160408       //?più tutti i dati relativi alla filiale ritiro
038200160318         IF  fiora0O0.versione > 'B' and
038300160216             fiora0M0.nrmsg = 0 and
038400160216             prmRqsOpCode = FIORA00_RQSOPCODE_WRITENPR;
038500160216           fiora0O0.npr = GetNPR(parametroNPR);
038600160216         //?Se il rif. ORM è vuoto impoto il NPR
038700160216           IF  fiora0O0.vaorfa = *blanks;
038800160317             fiora0O0.vaorfa = fiora0O0.npr;
038900160216           ENDIF;
039000160408         //?Richiamo pgm che torna i dati del servizio clienti
039100160408         //?per la filiale passata
039200160503         //?per precauzione imposto già gli orari a 0
039300160503         //?è capitato che si spaccasse il pgm di Cussini per data/ora non validi
039400160503           fiora0O0.oraamdapor = *all'0';
039500160503           fiora0O0.oraamapor = *all'0';
039600160503           fiora0O0.orapmdapor = *all'0';
039700160503           fiora0O0.orapmapor = *all'0';
039800160408           clear TRULDSCDS;
039900160408           truldscds.IULDSCfil = fiora0O0.vaopor;
040000160408           truldscds.IULDSClin = fiora0O0.idlingua;
040100160408           truldscr (truldscds);
040200160408           IF  truldscds.OULDSCerr = *blanks;
040300160408             fiora0O0.descrpor = truldscds.OULDSCdesc;
040400160408             fiora0O0.telpor = truldscds.OULDSCtel;
040500160408             fiora0O0.mailpor = truldscds.OULDSCmail;
040600160408             fiora0O0.urlpor = truldscds.OULDSCurl;
040700160408             fiora0O0.oraamdapor = truldscds.OULDSCamda;
040800160408             fiora0O0.oraamapor = truldscds.OULDSCama;
040900160408             fiora0O0.orapmdapor = truldscds.OULDSCpmda;
041000160420             fiora0O0.orapmapor = truldscds.OULDSCpma;
041100160408           ENDIF;
041200160216         ENDIF;
041300140408
041400140408       ENDIF;
041500140408
041600140408       //?Solo per formato FIORA0I1 = a richiamo GET_FRASE1
041700140408       IF  fiora0I1.formato = prmRqsFormato;
041800140408         exsr GetFrase1;
041900140408       ENDIF;
042000131010
042100131010       //?Operazioni finali
042200131010       exsr RoutEnd;
042300131008
042400131008       //--------------------------------------------------------------
042500131008       //?Operazioni iniziali.                                         ?
042600131008       //--------------------------------------------------------------
042700131008       BEGSR  RoutInz;
042800140211
042900140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
043000131008
043100140210         prmRpyOpCode = FIORA00_RPYOPCODE_DONE;
043200140210         prmRpyIdMsg  = FIORA00_ESITO_OK;
043300131009
043400131009       //?Data del giorno
043500131009         dataoggi = %dec(%date());
043600131009
043700131009       //?Pulisco ds di output
043800131219         reset FIORA0O0;
043900140408         reset FIORA0O1;
044000131219         reset FIORA0M0;
044100131008
044200131008         *inLR = *on;
044300131008
044400131008       ENDSR;
044500131008
044600131008       //--------------------------------------------------------------
044700131008       //?Controllo formale dei dati passati.
044800131008       //--------------------------------------------------------------
044900131008       BEGSR  Controlla;
045000131008
045100131008       //?OpCode
045200131009         IF  prmRqsOpCode <> FIORA00_RQSOPCODE_CONTROL  and
045300140408             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITE    and
045400160216             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITENPR and
045500140408             prmRqsOpCode <> FIORA00_RQSOPCODE_GET_FRASE1;
045600131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
045700131009           prmRpyIdMsg  = FIORA00_ESITO_RQSOPCODE_SCONOSCIUTO;
045800140210           exsr RoutEnd;
045900131008         ENDIF;
046000140408
046100140408       //?per GET_FRASE1
046200140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
046300140408       //?Formato e size RQS
046400140408           IF  prmRqsFormato = fiora0I1.formato;
046500140408             fiora0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
046600140408           ELSE;
046700140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
046800140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
046900140408             exsr RoutEnd;
047000140408           ENDIF;
047100140408           IF  prmRqsDataSize > 0;
047200140408           ELSE;
047300140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
047400140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
047500140408             exsr RoutEnd;
047600140408           ENDIF;
047700140408         //?Formato e size RPY
047800140408           IF  prmRpyFormato = fiora0O1.formato;
047900140408           ELSE;
048000140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
048100140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
048200140408             exsr RoutEnd;
048300140408           ENDIF;
048400140408           IF  prmRpyDataSize > 0;
048500140408           ELSE;
048600140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
048700140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
048800140408             exsr RoutEnd;
048900140408           ENDIF;
049000140408           leavesr;
049100140408         ENDIF;
049200131008
049300140210       //?Formato e size RQS
049400131008         IF  prmRqsFormato = fiora0I0.formato;
049500131219           fiora0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
049600131008         ELSE;
049700131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
049800131008           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
049900140210           exsr RoutEnd;
050000131008         ENDIF;
050100140210         IF  prmRqsDataSize > 0;
050200140210         ELSE;
050300140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
050400140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
050500140210           exsr RoutEnd;
050600140210         ENDIF;
050700140210
050800140210       //?Formato e size RPY
050900140210         IF  prmRpyFormato = fiora0O0.formato;
051000140210         ELSE;
051100140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051200140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
051300140210           exsr RoutEnd;
051400140210         ENDIF;
051500140210         IF  prmRpyDataSize > 0;
051600140210         ELSE;
051700140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051800140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
051900140210           exsr RoutEnd;
052000140210         ENDIF;
052100140210
052200140210       //?Formato e size MSG
052300140210         IF  prmRpyFormMsg = fiora0M0.formato;
052400140210         ELSE;
052500140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
052600140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
052700140210           exsr RoutEnd;
052800140210         ENDIF;
052900140210         IF  prmRpyMsgSize > 0;
053000140210         ELSE;
053100140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
053200140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
053300140210           exsr RoutEnd;
053400140210         ENDIF;
053500140210
053600140210       //?Formato e size Forzature
053700140210         IF  prmRpyFormForz = fiora0F0.formato;
053800160527           fiora0F0 = %SUBST(prmRpyForzatu:1:prmRpyForSize);
053900140210         ELSE;
054000140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
054100140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
054200140210           exsr RoutEnd;
054300140210         ENDIF;
054400140210         IF  prmRpyForSize > 0;
054500140210         ELSE;
054600140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
054700140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
054800140210           exsr RoutEnd;
054900140210         ENDIF;
055000131008
055100131008       ENDSR;
055200140408
055300140408       //--------------------------------------------------------------
055400140408       //?Carico tabelle.
055500140408       //--------------------------------------------------------------
055600140408       BEGSR  CaricaTab;
055700140408
055800140408       //?Carico sk delle provincie
055900140408         clear xx;
056000140408         clear skprv;
056100140408         k_TBLkut = 1;
056200140408         k_TBLcod = 'PR';
056300140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
056400140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
056500140408         DOW not %eof(TABEL00F);
056600140408           IF  TBLflg = *blanks;
056700140408             xx += 1;
056800140408             skprv(xx) = TBLkey;
056900140408           ENDIF;
057000140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
057100140408         ENDDO;
057200140408
057300140408       //?Carico sk delle nazioni
057400140408         clear xx;
057500140408         clear sknaz;
057600140408         k_TBLkut = 1;
057700140408         k_TBLcod = '15';
057800140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
057900140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
058000140408         DOW not %eof(TABEL00F);
058100140408           IF  TBLflg = *blanks;
058200160208             ds15 = TBLuni;
058300140408             xx += 1;
058400140408             sknaz(xx) = TBLkey;
058500160208             sknazd(xx) = ds15.§15des;
058600140408           ENDIF;
058700140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
058800140408         ENDDO;
058900140408
059000140408       //?Carico tabella 3I - DPD
059100140408         clear ds3Idp;
059200140408         k_TBLkut = 1;
059300140408         k_TBLcod = '3I';
059400140408         k_TBLkey = 'DPD';
059500140408         chain (k_TBLkut:k_TBLcod:k_TBLkey) TABEL00F;
059600140408         IF  %found(TABEL00F) and TBLflg = *blanks;
059700140408           ds3Idp = TBLuni;
059800140408         ENDIF;
059900151008
060000151008       //?Carico tabella DFT ORM con key generica 999
060100151008         clear dDFT;
060200151008         k_TBEcod = 'DFT';
060300151008         k_TBEke1 = '999';
060400151008         chain (k_TBEcod:k_TBEke1) TNTBE01L;
060500151008         IF  %found(TNTBE01L);
060600151008           dDFT = TBEuni;
060700151008         ENDIF;
060800140408
060900140408       ENDSR;
061000131007
061100131007       //--------------------------------------------------------------
061200131008       //?Imposta i DFT se non passati alcuni campi.
061300131007       //--------------------------------------------------------------
061400131008       BEGSR  DftInput;
061500131007
061600140210       //?Se ID richiamo non valido o non passato
061700140210       //?assumo internet
061800140210         IF  fiora0I0.idrichiamo = *blanks or
061900140210            (fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET and
062000140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_FILE and
062100140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_AS400);
062200131009            fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
062300140210            fiora0M0.nrmsg += 1;
062400140210            fiora0M0.skIdMsg(fiora0M0.nrmsg) = '*parm';
062500140210            fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'IDRICHIAMO';
062600140210            fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
062700140210            fiora0M0.skTextMsg(fiora0M0.nrmsg) =
062800140210            'Id richiamo non previsto, assunto Internet';
062900131007         ENDIF;
063000131007
063100140210       //?Se ID lingua non valido o non passato
063200140210       //?assumo italiano
063300140210         IF  fiora0I0.idlingua = *blanks or
063400140210            (fiora0I0.idlingua <> FIORA00_ID_LINGUA_IT and
063500140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_FR and
063600140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_EN and
063700140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_DE);
063800140210            fiora0I0.idlingua = FIORA00_ID_LINGUA_IT;
063900160509            //fiora0M0.nrmsg += 1;
064000160509            //fiora0M0.skIdMsg(fiora0M0.nrmsg) = '*parm';
064100160509            //fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'IDLINGUA';
064200160509            //fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
064300160509            //fiora0M0.skTextMsg(fiora0M0.nrmsg) =
064400160509            //'Id lingua non previsto, assunto Italiano';
064500131007         ENDIF;
064600131010
064700131010       //?Imposto la ds DORM01
064800131010         dORM01 = fiora0I0.vaoflo;
064900131007
065000131007       ENDSR;
065100131007
065200131007       //--------------------------------------------------------------
065300131007       //?Routine principale per view INTERNET.
065400131007       //--------------------------------------------------------------
065500131008       BEGSR  Internet;
065600131008
065700151110       //?Dalla versione 'B' il controllo del Cod.Fiscale e/o Partita IVA
065800151110       //?è da fare solo se cliente senza psw e se paga mittente
065900151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor = 0 and
066000151110             fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
066100151110           exsr CodFiscPiva;
066200151110         ENDIF;
066300151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor > 0;
066400151110           exsr Ordinante;
066500151110         ENDIF;
066600151110
066700151104         IF  fiora0I0.versione = 'A';
066800131010       //?Se ordinante non codificato (è utente senza PSW)
066900131008       //?controllo Codice Fiscale/Partita IVA
067000131009         IF  fiora0I0.vaocor = 0;
067100131010           exsr CodFiscPiva;
067200131223         ELSE;
067300131223           exsr Ordinante;
067400131008         ENDIF;
067500151104         ENDIF;
067600131009
067700131009       //?Controllo cliente mittente
067800131009         IF  fiora0I0.vaocra > 0;
067900131010           exsr CodMittente;
068000131219         ELSE;
068100131219       //?Controllo mittente
068200131219           exsr Mittente;
068300131219         ENDIF;
068400131219
068500131219       //?Se manca il mittente ho finito i giochi, subito errore
068600131219         IF  wErrMitt;
068700131219           leavesr;
068800131219         ENDIF;
068900140311
069000140311       //?Controllo chi paga
069100140311         exsr ChiPaga;
069200140404
069300140404       //?Controllo commissionato
069400140404         exsr Commissionato;
069500131205
069600131205       //?Controllo filiale emissione
069700131205         exsr FilEmissione;
069800151008
069900151008       //?Controllo le quantità
070000151008         exsr Quantita;
070100151008
070200151008       //?Calcolo il maggiore tra volume/quantità per instradamento
070300151008         exsr PesoVolume;
070400131205
070500131205       //?Controllo filiale ritiro
070600131205         exsr FilRitiro;
070700140408
070800140408       //?Cerco gli orari servizio
070900140408         exsr OrariServizio;
071000131205
071100131205       //?Controllo data ritiro
071200131205         exsr DataRitiro;
071300131011
071400131011       //?Controllo ora pronta merce
071500131011         exsr OraRitiro;
071600160404
071700160404       //?Controllo ora pronta merce con orari servizio
071800160404         exsr OraRitiroServizio;
071900131028
072000131028       //?Controllo orari apertura
072100140404         exsr OrariApertura;
072200131204
072300131204       //?Controllo la natura della merce
072400131204         exsr NaturaMerce;
072500131204
072600131204       //?Controllo uno o più destinatario
072700131204         exsr UnoPiuDest;
072800131205
072900131205       //?Controllo Referente
073000131205         exsr Referente;
073100131223
073200131223       //?Controllo Telefono
073300131223         exsr Telefono;
073400140311
073500140311       //?Controllo Mail
073600140311         exsr Mail;
073700140311
073800140311       //?Controllo SMS
073900140311         exsr Sms;
074000131205
074100140313       //?Controllo cliente destinatario
074200140313         IF  fiora0I0.vaocrc > 0;
074300140313           exsr CodDestinatario;
074400140313         ELSE;
074500131205       //?Controllo se deve esserci o no il Destinatario
074600140313           exsr Destinatario;
074700140313         ENDIF;
074800131205
074900131205       //?Controllo Fermo Deposito
075000131205         exsr FermoDeposito;
075100140506
075200140506       //?Controllo Alert
075300140506         exsr Alert;
075400151008
075500151008       //?Dalla versione 'B'
075600151008         IF  fiora0I0.versione > 'A';
075700151008         //?Controllo Mail x accettazione ORM
075800151008           exsr MailConferma;
075900151008         //?Calcolo orario indicativo ritiro
076000151008           exsr OrarioIndRitiro;
076100151008         ENDIF;
076200160318
076300160318       //?Dalla versione 'C'
076400160322         IF  fiora0I0.versione > 'B';
076500160318         //?Controllo SMS x accettazione ORM
076600160318           exsr SmsConferma;
076700160318         ENDIF;
076800160714
076900160714       //?Dalla versione 'D'
077000160714         IF  fiora0I0.versione > 'C';
077100160714         //?Controllo flag per memorizzazione dati su anagrafica clienti ritiro
077200160714           exsr FlagMemDati;
077300160714         ENDIF;
077400131007
077500131007       ENDSR;
077600140620
077700140620       //--------------------------------------------------------------
077800140620       //?Routine principale per view AS400.
077900140620       //--------------------------------------------------------------
078000140620       BEGSR  AS400;
078100160530
078200160530       //?Come prima cosa passo tutti i dati dalla ds di Input alla ds di Output
078300160530       //?in questo modo quando esco dal pgm per le forzature non perdo i dati
078400160530       //?immessi dall'utente
078500160530         eval-corr fiora0O0 = fiora0I0;
078600160530         reset fiora0O0.formato;
078700160530         reset fiora0O0.versione;
078800160322
078900160322       //?Controllo tipo Comunicazione ORM
079000160322         exsr TipoComOrm;
079100160315
079200160315       //?Controllo tipo ORM
079300160315         exsr TipoOrm;
079400160607
079500160607       //?Imposto uno o più destinatario
079600160607         exsr UnoPiuDest;
079700140620
079800140620       //?Controllo cliente mittente
079900140620         IF  fiora0I0.vaocra > 0;
080000140620           exsr CodMittente;
080100160401       //?Controllo dati mittente
080200160401         ELSE;
080300160401           exsr Mittente;
080400160315         ENDIF;
080500140717
080600140717       //?Se manca il mittente ho finito i giochi, subito errore
080700140717         IF  wErrMitt;
080800140717           leavesr;
080900140717         ENDIF;
081000140620
081100160315       //?Calcola filiale emissione
081200140620         exsr FilEmissione;
081300160404
081400160404       //?Se filiale ritiro già impostata la metto subito nei dati di output
081500160404         IF  fiora0I0.vaopor > 0;
081600160404           fiora0O0.vaopor = fiora0I0.vaopor;
081700160404         ENDIF;
081800160404
081900160404       //?In caso di Immissione/Copia ORM
082000160601         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
082100160606             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
082200160606       //?e mittente codificato
082300160606             fiora0I0.vaocra > 0;
082400160606         //?se non impostata calcolo la filiale ritiro
082500160606           IF  fiora0I0.vaopor = 0;
082600160601             exsr FilRitiro;
082700160606           ENDIF;
082800160613         //?calcolo la data ritiro
082900160613           exsr DataRitiro;
083000160606         //?Calcolo anche gli orari servizio
083100160606           exsr OrariServizio;
083200160404         ENDIF;
083300151008
083400151008       //?Controllo le quantità
083500151008         exsr Quantita;
083600160613         IF  wErrQta;
083700160613           leavesr;
083800160613         ENDIF;
083900151008
084000151008       //?Calcolo il maggiore tra volume/quantità per instradamento
084100151008         exsr PesoVolume;
084200160315
084300160404       //?In caso di Immissione/Copia ORM
084400160404       //?Se mittente NON codificato calcolo ora la filiale ritiro
084500160404         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
084600160404             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
084700160525             fiora0I0.vaocra = 0;
084800160404         //?Calcola filiale ritiro
084900160404           exsr FilRitiro;
085000160404         ENDIF;
085100160606
085200160606       //?Controllo Referente
085300160606         exsr Referente;
085400160606
085500160606       //?Controllo Telefono
085600160606         exsr Telefono;
085700160606
085800160606       //?Controllo orari apertura
085900160606         exsr OrariApertura;
086000160607
086100160607       //?Controllo chi paga
086200160607         exsr ChiPaga;
086300160607
086400160607       //?Controllo cliente destinatario
086500160607         IF  fiora0I0.vaocrc > 0;
086600160607           exsr CodDestinatario;
086700160607         ELSE;
086800160607       //?Controllo se deve esserci o no il Destinatario
086900160607           exsr Destinatario;
087000160607         ENDIF;
087100140620
087200160610       //?In caso di Immissione/Copia ORM
087300160610       //?Se mittente NON codificato calcolo ora la data ritiro
087400160610       //?Se prima non ci sono stati errori
087500160610         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
087600160610             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
087700160610             fiora0I0.vaocra = 0 and
087800160610             fiora0M0.nrmsg = 0;
087900160610         //?Controllo data ritiro
088000160610           exsr DataRitiro;
088100160610         ENDIF;
088200160404
088300160404       //?Cerco gli orari servizio
088400160613         exsr OrariServizio;
088500160610
088600160610       //?Controllo ora pronta merce
088700160610         exsr OraRitiro;
088800160518
088900160518       //?Controllo ora pronta merce con orari servizio
089000160518         exsr OraRitiroServizio;
089100140620
089200140620       //?Controllo la natura della merce
089300140620         exsr NaturaMerce;
089400160613
089500160613       //?Controllo le quantità della videata F05
089600160613         exsr Quantita2;
089700140620
089800140620       //?Controllo Mail
089900140620         exsr Mail;
090000140620
090100140620       //?Controllo SMS
090200140620         exsr Sms;
090300140620
090400140620       //?Controllo Fermo Deposito
090500140620         exsr FermoDeposito;
090600160315
090700160315       //?Dalla versione 'B'
090800160315         IF  fiora0I0.versione > 'A';
090900160315         //?Controllo Mail x accettazione ORM
091000160315           exsr MailConferma;
091100160318         //?Controllo SMS x accettazione ORM
091200160318           exsr SmsConferma;
091300160315         //?Calcolo orario indicativo ritiro
091400160315           exsr OrarioIndRitiro;
091500160315         ENDIF;
091600160315
091700160315       //?Controllo la filiale di ritiro
091800160601         exsr ControllaFilRitiro;
091900160315
092000160315       //?Controllo cliente ordinante
092100160315         IF  fiora0I0.vaocor > 0;
092200160315           exsr Ordinante;
092300160315         ENDIF;
092400160315       //?Controllo dati Ordinante
092500160315         exsr DatiOrdinante;
092600160315
092700160315       //?Controllo Filiale Consegna
092800160315       //?Campo della DS previsto solo da versione 'B' in poi
092900160315         IF  fiora0I0.versione > 'A';
093000160315           exsr FilConsegna;
093100160315         ENDIF;
093200160315
093300160315       //?Controllo Cliente tassazione
093400160315         exsr ClienteKSC;
093500160315
093600160315       //?Campo della DS previsto solo da versione 'B' in poi
093700160315         IF  fiora0I0.versione > 'A';
093800160315       //?Cerco il giro di ritiro per il mittente codificato
093900160315           IF  fiora0I0.vaocra > 0;
094000160315             exsr GiroRitiro;
094100160315           ENDIF;
094200160610       //?Controllo il giro
094300160610           IF  fiora0I0.cgi <> *blanks;
094400160610             clear FIDG09DS;
094500160610             fidg09ds.D09iop0 = '001';
094600160610             fidg09ds.D09ifgs = fiora0I0.VAOpor;
094700160610             fidg09ds.D09idat = dataoggi;
094800160610             fidg09ds.D09icgi = fiora0O0.cgi;
094900160610             fidg09ds.D09itug = 'R';
095000160610             %subst(kpjba:247:256) = FIDG09DS;
095100160610             fidg09r (kpjba);
095200160610             fidg09ds = %subst(kpjba:247:256);
095300160610             IF  fidg09ds.D09oerr <> *blanks;
095400160610               wIdMsg   = 'TIS0718';
095500160610               wIdCampo = 'CGI';
095600160610               wParm = fidg09ds.D09omsg;
095700160610               exsr Messaggi;
095800160610               leavesr;
095900160610             ENDIF;
096000160610           ENDIF;
096100160315         ENDIF;
096200160315
096300160315       //?Calcola commissionato
096400160315         exsr Commissionato;
096500160610
096600160610       //?Se non ci sono errori
096700160610       //?se sono in immissione ORM
096800160610       //?se ORM commissionato
096900160610       //?se dati alert ORM commissionato presenti
097000160610       //?verifico se la data di ritiro è da ricalcolare
097100160610         IF  fiora0M0.nrmsg = 0 and
097200160610             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
097300160610             wcom  and
097400160610            (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks) and
097500160610             fiora0I0.vaodar = dataoggi;
097600160610         //?Calcolo la data ritiro
097700160610           exsr DataRitiro;
097800160610         ENDIF;
097900160610
098000140620
098100140620       ENDSR;
098200131008
098300131008       //--------------------------------------------------------------
098400131008       //?Controllo codice fiscale/partita iva.
098500131008       //--------------------------------------------------------------
098600131010       BEGSR  CodFiscPiva;
098700131009
098800131009         wErr = *off;
098900140211
099000140211         exec sql
099100140211         set :fiora0O0.codfiscale = ucase (:fiora0I0.codfiscale);
099200140211
099300140211         exec sql
099400140211         set :fiora0O0.partitaiva = ucase (:fiora0I0.partitaiva);
099500131008
099600131008       //?Accettiamo solo 1 dei 2, o CF o PI
099700131223         IF  fiora0O0.codfiscale <> *blanks and
099800131223             fiora0O0.partitaiva <> *blanks;
099900131008           wIdMsg   = 'TIS0240';
100000131008           wIdCampo = 'CODFISCALE';
100100131204           clear wParm;
100200131008           exsr Messaggi;
100300131009           wErr = *on;
100400131008         ENDIF;
100500131008
100600131008       //?Almeno 1 dei 2 deve essere presente, o CF o PI
100700131223         IF  fiora0O0.codfiscale = *blanks and
100800131223             fiora0O0.partitaiva = *blanks;
100900131008           wIdMsg   = 'TIS0240';
101000131008           wIdCampo = 'CODFISCALE';
101100131204           clear wParm;
101200131008           exsr Messaggi;
101300131009           wErr = *on;
101400131008         ENDIF;
101500131008
101600131008       //?Controllo CF
101700131223         IF  fiora0O0.codfiscale <> *blanks;
101800151020           clear xcfiva1ds;
101900151020           xcfiva1ds.xcfivapar = fiora0O0.codfiscale;
102000151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
102100151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
102200151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
102300151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
102400151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
102500151020           xcfivar1 (xcfiva1ds);
102600151020           IF  xcfiva1ds.xcfivaflg < *zeros;
102700131008             wIdMsg   = 'TIS0069';
102800131008             wIdCampo = 'CODFISCALE';
102900131204             clear wParm;
103000131008             exsr Messaggi;
103100131009             wErr = *on;
103200131008           ENDIF;
103300131008         ENDIF;
103400131008
103500131008       //?Partita IVA valida
103600131223         IF  fiora0O0.partitaiva <> *blanks;
103700140211           IF  %subst(fiora0O0.partitaiva:1:2) >= '00';
103800140211             wPIVA = fiora0O0.partitaiva;
103900140211           ELSE;
104000140211             wIVA = fiora0O0.partitaiva;
104100140211           ENDIF;
104200151020           clear xcfiva1ds;
104300151020           xcfiva1ds.xcfivamod = 'P';
104400151020           xcfiva1ds.xcfivapar = wIVA;
104500151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
104600151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
104700151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
104800151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
104900151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
105000151020           xcfivar1 (xcfiva1ds);
105100131009
105200131008         //?Errore forzabile
105300151020           IF  xcfiva1ds.xcfivaflg = -9;
105400131008             wIdMsg   = 'TIS0444';
105500131008             wIdCampo = 'PARTITAIVA';
105600140210             clear wParm;
105700131008             exsr Forzatura;
105800131009             wErr = *on;
105900131008           ENDIF;
106000131008
106100131008         //?Errore bloccante
106200151020           IF  xcfiva1ds.xcfivaflg < *zeros and xcfiva1ds.xcfivaflg <> -9;
106300131008             wIdMsg   = 'TIS0444';
106400131008             wIdCampo = 'PARTITAIVA';
106500131204             clear wParm;
106600131009             exsr Messaggi;
106700131009             wErr = *on;
106800131008           ENDIF;
106900131009
107000131009       //?Se errore non imposto i campi di output
107100131009          IF  wErr;
107200131009            leavesr;
107300131009          ENDIF;
107400131008
107500131008       //?Imposto il codice ISO che torna dal pgm di controllo
107600151020           IF  wISO <> %subst(xcfiva1ds.xcfivapar:3:2);
107700151020             wISO = %subst(xcfiva1ds.xcfivapar:3:2);
107800131008           ENDIF;
107900131008         ENDIF;
108000131007
108100131008       ENDSR;
108200131223
108300131223       //--------------------------------------------------------------
108400131223       //?Imposto i dati dell'ordinante.
108500131223       //--------------------------------------------------------------
108600131223       BEGSR  Ordinante;
108700131223
108800160520       //?Solo per richiamo da AS400
108900160520       //?torno il codice al chiamante
109000160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
109100160520           fiora0O0.vaocor = fiora0I0.vaocor;
109200160520         ENDIF;
109300160520
109400140401         wCodice = fiora0I0.vaocor;
109500140402         reset FIORB0OO;
109600140401
109700140401       //?codice valido
109800140402         IF  not IsCodiceValido(FIORB0OO);
109900140305           wIdMsg   = 'TIS0719';
110000140304           wIdCampo = 'VAOCOR';
110100140305           clear wParm;
110200131223           exsr Messaggi;
110300131223           leavesr;
110400131223         ENDIF;
110500160315
110600160315       //?Solo per richiamo da AS400
110700160315       //?e solo dalla versione 'B' in poi
110800160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
110900160315             fiorb0OO.versione > 'A';
111000160315       //?codice bloccato
111100160714           IF  fiorb0OO.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
111200160315             wIdMsg   = 'TIS0799';
111300160315             wIdCampo = 'VAOCOR';
111400160315             clear wParm;
111500160315             exsr Messaggi;
111600160315             leavesr;
111700160315           ENDIF;
111800160315         ENDIF;
111900131227
112000131227       //?cerco network della filiale del codice ordinante
112100160315         clear wNetworkOrd;
112200160315         wFil = %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
112300160315         wNetworkOrd = GetNtwFiliale(wFil);
112400131227         clear dNTW;
112500131227         k_TBEcod = 'NTW';
112600160315         k_TBEke1 = wNetworkOrd;
112700131227         chain (k_TBEcod:k_TBEke1) TNTBE01L;
112800131227         IF  %found(TNTBE01L);
112900131227           dNTW = TBEuni;
113000131227         ENDIF;
113100131227       //?se la filiale del codice ordinante è una filiale con network DPD
113200160315         IF  wNetworkOrd = FIORA00_NTW_DPD;
113300131227         //?imposto il servizio DPD
113400131227           fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
113500131227         ENDIF;
113600131227       //?se la filiale del codice ordinante è una filiale con network estero
113700160315         IF  dntw.§NTWfie = FIORA00_NTW_ESTERO;
113800131227         //?imposto il servizio Estero
113900131227           fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
114000131227         ENDIF;
114100160315
114200160315       //?Solo per richiamo da AS400
114300160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
114400160315       //?imposto i dati di ritorno dell'ordinante
114500160315           fiora0I0.vaorso = fiorb0OO.nome;
114600160315           fiora0I0.vaoino = fiorb0OO.indirizzo;
114700160315           fiora0I0.vaocao = fiorb0OO.cap;
114800160315           fiora0I0.vaoloo = fiorb0OO.localita;
114900160315           fiora0I0.vaopro = fiorb0OO.provincia;
115000160315           fiora0I0.vaonao = fiorb0OO.nazione;
115100160315         //?Se paga ordinante imposto i dati
115200160315           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
115300160315               fiora0I0.vaoksc = 0 and fiorb0OO.codksc > 0;
115400160315             fiora0I0.vaoksc = fiorb0OO.codksc;
115500160315             IF  fiorb0OO.codtariffa = 999;
115600160315               clear fiora0I0.vaoctr;
115700160315             ELSE;
115800160315               fiora0I0.vaoctr =
115900160315               %subst(%editc(fiorb0OO.codtariffa:'X'):2:3);
116000160315             ENDIF;
116100160315           ENDIF;
116200160315         //?Imposto se contatto telefonico (commissionato)
116300160315         //?solo dalla versione 'B' in poi
116400160315           IF  fiorb0OO.versione > 'A';
116500160315             fiora0O0.contattot = fiorb0OO.contatto;
116600160315           ENDIF;
116700160315         ENDIF;
116800131223
116900131223       ENDSR;
117000160315
117100160315       //--------------------------------------------------------------
117200160315       //?Controllo dati Ordinante.
117300160315       //--------------------------------------------------------------
117400160315       BEGSR  DatiOrdinante;
117500160315
117600160315       //?Se non ho dati vado via
117700160315         IF  fiora0I0.vaorso = *blanks and fiora0I0.vaoino = *blanks and
117800160315             fiora0i0.vaoloo = *blanks and fiora0i0.vaocao = *blanks;
117900160315           leavesr;
118000160315         ENDIF;
118100160315
118200160315         exec sql
118300160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
118400160315
118500160315       //?Controllo nominativo ordinante
118600160315         exsr NomeOrdinante;
118700160315
118800160315       //?Controllo indirizzo ordinante
118900160315         exsr ViaOrdinante;
119000160315
119100160315       //?Controllo località ordinante
119200160315         exsr LocOrdinante;
119300160315
119400160315       //?Controllo cap ordinante
119500160315         exsr CapOrdinante;
119600160315
119700160315       //?Controllo provincia ordinante
119800160315         exsr ProvOrdinante;
119900160315
120000160315       //?Controllo nazione ordinante
120100160315         exsr NazOrdinante;
120200160315
120300160315       //?Controllo indirizzo completo dell'ordinante
120400160315         exsr IndTotOrdinante;
120500160315
120600160315       ENDSR;
120700160315
120800160315       //--------------------------------------------------------------
120900160315       //?Controllo nominativo ordinante.
121000160315       //--------------------------------------------------------------
121100160315       BEGSR  NomeOrdinante;
121200160315
121300160315         exec sql
121400160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
121500160315
121600160315       ENDSR;
121700160315
121800160315       //--------------------------------------------------------------
121900160315       //?Controllo indirizzo ordinante.
122000160315       //--------------------------------------------------------------
122100160315       BEGSR  ViaOrdinante;
122200160315
122300160315         exec sql
122400160315         set :fiora0O0.vaoino = ucase (:fiora0I0.vaoino);
122500160315
122600160315       ENDSR;
122700160315
122800160315       //--------------------------------------------------------------
122900160315       //?Controllo cap ordinante.
123000160315       //--------------------------------------------------------------
123100160315       BEGSR  CapOrdinante;
123200160315
123300160315         exec sql
123400160315         set :fiora0O0.vaocao = ucase (:fiora0I0.vaocao);
123500160315
123600160315       ENDSR;
123700160315
123800160315       //--------------------------------------------------------------
123900160315       //?Controllo località ordinante.
124000160315       //--------------------------------------------------------------
124100160315       BEGSR  LocOrdinante;
124200160315
124300160315         exec sql
124400160315         set :fiora0O0.vaoloo = ucase (:fiora0I0.vaoloo);
124500160315
124600160315       ENDSR;
124700160315
124800160315       //--------------------------------------------------------------
124900160315       //?Controllo provincia ordinante.
125000160315       //--------------------------------------------------------------
125100160315       BEGSR  ProvOrdinante;
125200160315
125300160315         exec sql
125400160315         set :fiora0O0.vaopro = ucase (:fiora0I0.vaopro);
125500160315
125600160315       //?Controllo con la sk delle provincie
125700160315         IF  fiora0O0.vaopro <> *blanks and
125800160315            %lookup(fiora0O0.vaopro:skprv) = 0;
125900160315           wIdMsg   = 'TIS0464';
126000160315           wIdCampo = 'VAOPRO';
126100160315           clear wParm;
126200160315           exsr Messaggi;
126300160315         ENDIF;
126400160315
126500160315       ENDSR;
126600160315
126700160315       //--------------------------------------------------------------
126800160315       //?Controllo nazione ordinante.
126900160315       //--------------------------------------------------------------
127000160315       BEGSR  NazOrdinante;
127100160315
127200160315         exec sql
127300160315         set :fiora0O0.vaonao = ucase (:fiora0I0.vaonao);
127400160315
127500160315         //?Controllo con la sk delle nazioni
127600160315         IF  fiora0O0.vaonao <> *blanks and
127700160315             %lookup(fiora0O0.vaonao:sknaz) = 0;
127800160315           wIdMsg   = 'TIS0385';
127900160315           wIdCampo = 'VAONAO';
128000160315           clear wParm;
128100160315           exsr Messaggi;
128200160315         ENDIF;
128300160315
128400160315       ENDSR;
128500160315
128600160315       //--------------------------------------------------------------
128700160315       //?Controllo indirizzo completo dell'ordinante.
128800160315       //--------------------------------------------------------------
128900160315       BEGSR  IndTotOrdinante;
129000160315
129100160315         clear TISI95DS;
129200160315         tisi95ds.I95tcn = '7';
129300160315         tisi95ds.I95nar = fiora0O0.vaonao;
129400160315         tisi95ds.I95cap = fiora0O0.vaocao;
129500160315         tisi95ds.I95loc = fiora0O0.vaoloo;
129600160315         tisi95ds.I95prv = fiora0O0.vaopro;
129700160315         tisi95ds.I95ind = fiora0O0.vaoino;
129800160315         tisi95ds.I95dat = %dec(%date());
129900160315
130000160315       //?richiamo il tisi95r
130100160315         ChkCapLocalita (tisi95ds);
130200160315
130300160315       //?errore o dato non affidabile
130400160315         IF  fiora0O0.vaonao = *blanks and tisi95ds.O95lia <> '3'  or
130500160315             fiora0O0.vaonao <> *blanks and tisi95ds.O95lia <> '2';
130600160315           wIdMsg   = 'TIS0054';
130700160315           wIdCampo = 'VAOLOO';
130800160315           clear wParm;
130900160315           exsr Messaggi;
131000160315           leavesr;
131100160315         ENDIF;
131200160315
131300160315       ENDSR;
131400160322
131500160322       //--------------------------------------------------------------
131600160322       //?Controllo Tipo Comunicazione ORM.
131700160322       //--------------------------------------------------------------
131800160322       BEGSR  TipoComOrm;
131900160322
132000160404         k_TBEcod = 'TCO';
132100160404         k_TBEke1 = fiora0I0.tco;
132200160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
132300160322         IF  not %found(TNTBE01L);
132400160404           wIdMsg   = 'TIS0832';
132500160404           wIdCampo = 'TCO';
132600160322           clear wParm;
132700160322           exsr Messaggi;
132800160322           leavesr;
132900160322         ENDIF;
133000160520
133100160520       //?Se richiamato da AS400
133200160520       //?se sono in WRITE ORM
133300160520       //?controllo il tipo comunicazione ORM con il tipo richiamo
133400160520       //?se richiamato da immissine manuale posso immettere solo
133500160520       //?'TELEFONICO' 'MAIL/FAX'
133600160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
133700160520             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
133800160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_TELEFONICO and
133900160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_MAIL;
134000160520           wIdMsg   = 'TIS0832';
134100160520           wIdCampo = 'VAOTCO';
134200160520           clear wParm;
134300160520           exsr Messaggi;
134400160520         ENDIF;
134500160520       //?se richiamato da immissione VAS posso avere solo
134600160520       //?'FILE' 'INTERNET'
134700160520       //?se richiamato da immissione FISSI posso avere solo
134800160520       //?'FISSO'
134900160520       //?se richiamato da immissione PREPAGATI posso avere solo
135000160520       //?'PREPAGATO'
135100160322
135200160322       ENDSR;
135300160315
135400160315       //--------------------------------------------------------------
135500160315       //?Controllo Tipo ORM.
135600160315       //--------------------------------------------------------------
135700160315       BEGSR  TipoOrm;
135800160315
135900160322         k_TBEcod = 'TOR';
136000160322         k_TBEke1 = fiora0I0.tor;
136100160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
136200160322         IF  not %found(TNTBE01L);
136300160322           wIdMsg   = 'TIS0797';
136400160322           wIdCampo = 'TOR';
136500160322           clear wParm;
136600160322           exsr Messaggi;
136700160322           leavesr;
136800160322         ENDIF;
136900160315
137000160315       //?Se tipo ORM = P imposto il flag di prepagato
137100160315       //?e che è un solo destinatario
137200160322         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
137300160322           fiora0O0.prepagato = FIORA00_PREPAGATO;
137400160404           fiora0I0.unopiudest = '1';
137500160322         ENDIF;
137600160714       //?Se tipo ORM = M imposto 2 più destinatari
137700160322         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO;
137800160714           fiora0I0.unopiudest = FIORA00_PIUDESTINATARI;
137900160322         ENDIF;
138000160322       //?Se tipo ORM = S imposto 1 destinatario
138100160322         IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO;
138200160714           fiora0I0.unopiudest = FIORA00_UNDESTINATARIO;
138300160322         ENDIF;
138400160520
138500160520       //?Se richiamato da AS400
138600160608         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
138700160520       //?Tipo ORM multiplo e presenti i dati del destinatario errore
138800160608         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO and
138900160520            (fiora0I0.vaocrc > 0 or fiora0I0.vaorsc <> *blanks);
139000160520           wIdMsg   = 'TIS0802';
139100160520           wIdCampo = 'VAOTOR';
139200160520           clear wParm;
139300160520           exsr Messaggi;
139400160520         ENDIF;
139500160607       //?Tipo ORM prepagato e manca il destinatario errore
139600160608         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
139700160608             fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
139800160607           wIdMsg   = 'TIS0834';
139900160607           wIdCampo = 'VAOCRC';
140000160607           clear wParm;
140100160607           exsr Messaggi;
140200160607         ENDIF;
140300160608       //?Tipo ORM prepagato e c'è già la filiale ritiro calcolata
140400160608       //?verifico se possibile il prepagato o se devo immettere prima
140500160608       //?la bolla prepagata
140600160608         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
140700160608             fiora0O0.vaopor > *zeros and
140800160608             fiora0O0.vaopor = fiora0I0.fgs and
140900160608             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
141000160608           wIdMsg   = 'TIS0838';
141100160608           wIdCampo = 'VAOTOR';
141200160608           clear wParm;
141300160608           exsr Messaggi;
141400160608         ENDIF;
141500160608         ENDIF;
141600160315
141700160315       ENDSR;
141800131009
141900131009       //--------------------------------------------------------------
142000131009       //?Controllo codice mittente.
142100131009       //--------------------------------------------------------------
142200131010       BEGSR  CodMittente;
142300160315
142400160315         wErrMitt = *off;
142500140401
142600140401         wCodice = fiora0I0.vaocra;
142700140402         reset FIORB0OR;
142800131009
142900131009       //?codice valido
143000140402         IF not IsCodiceValido(FIORB0OR);
143100140312           wIdMsg   = 'TIS0068';
143200131009           wIdCampo = 'VAOCRA';
143300131204           clear wParm;
143400131009           exsr Messaggi;
143500160315         //?Solo per richiamo da AS400
143600160315           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
143700160315             wErrMitt = *on;
143800160315           ENDIF;
143900131009           leavesr;
144000131009         ENDIF;
144100160315
144200160315       //?Solo per richiamo da AS400
144300160315       //?e solo dalla versione 'B' in poi
144400160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
144500160315             fiorb0OR.versione > 'A';
144600160315         //?codice bloccato
144700160714           IF  fiorb0OR.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
144800160315             wIdMsg   = 'TIS0800';
144900160315             wIdCampo = 'VAOCRA';
145000160315             clear wParm;
145100160315             exsr Messaggi;
145200160315             wErrMitt = *on;
145300160315             leavesr;
145400160315           ENDIF;
145500160315         ENDIF;
145600140312
145700160315       //?Solo per richiamo da Internet
145800160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
145900140312       //?controllo se fa parte della stessa famiglia del codice di LOG
146000140313         IF not IsCodiceLegato();
146100140313           wIdMsg   = 'TIS0068';
146200140313           wIdCampo = 'VAOCRA';
146300140313           clear wParm;
146400140313           exsr Messaggi;
146500140313           leavesr;
146600140313         ENDIF;
146700160315         ENDIF;
146800140402
146900140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
147000140402         wIndirizzo = fiorb0OR.indirizzo;
147100140402         wCap = fiorb0OR.cap;
147200140402         wLocalita = fiorb0OR.localita;
147300140402         wProvincia = fiorb0OR.provincia;
147400140402         wNazione = fiorb0OR.nazione;
147500140411
147600140411       //?Solo per richiamo da Internet
147700140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
147800140411       //?normalizzo l'ora pronta merce dell'anagrafica clienti ritiro
147900140411             fiorb0OR.oraritiro > 0;
148000140411           oraalfa = GetOraAggiustata(fiorb0OR.oraritiro);
148100140411           fiorb0OR.oraritiro = %dec(oraalfa:4:0);
148200140411
148300151012           IF  fiorb0OR.oraritiro > *zeros and
148400151012              (fiora0I0.orrmin > 0 or fiora0I0.orrmax > 0);
148500140411             SELECT;
148600140411               WHEN fiorb0Or.oraritiro < fiora0I0.orrmin;
148700140411                 fiorb0OR.oraritiro = fiora0I0.orrmin;
148800140411               WHEN fiorb0Or.oraritiro > fiora0I0.orrmax;
148900140411                 fiorb0OR.oraritiro = fiora0I0.orrmax;
149000140411             ENDSL;
149100140411           ENDIF;
149200140411         ENDIF;
149300140717
149400140717       //?Solo per richiamo da AS400
149500160520       //?Imposto i dati del mittente
149600160519         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
149700160530           fiora0O0.vaorsr = fiora0I0.vaorsr;
149800160530           fiora0O0.vaoinr = fiora0I0.vaoinr;
149900160530           fiora0O0.vaocar = fiora0I0.vaocar;
150000160530           fiora0O0.vaolor = fiora0I0.vaolor;
150100160530           fiora0O0.vaoprr = fiora0I0.vaoprr;
150200160530           fiora0O0.vaonar = fiora0I0.vaonar;
150300140717         ENDIF;
150400131009
150500131009       ENDSR;
150600131219
150700131219       //--------------------------------------------------------------
150800131219       //?Controllo il mittente.
150900131219       //--------------------------------------------------------------
151000131219       BEGSR  Mittente;
151100131219
151200131219         wErrMitt = *off;
151300131219
151400131223       //?Se non ho nessun dato del cliente non controllo più niente
151500131219         IF  fiora0I0.vaorsr = *blanks and
151600131219             fiora0I0.vaoinr = *blanks and
151700131219             fiora0I0.vaocar = *blanks and
151800131219             fiora0I0.vaolor = *blanks and
151900131219             (fiora0I0.vaoprr = *blanks or
152000131219              fiora0I0.vaonar = *blanks);
152100140304           wIdMsg   = 'TIS0361';
152200131219           wIdCampo = 'VAORSR';
152300131219           clear wParm;
152400131219           exsr Messaggi;
152500131219           wErrMitt = *on;
152600131219           leavesr;
152700131219         ENDIF;
152800131219
152900131219       //?Nominativo mittente
153000131219         exsr NomeMittente;
153100131219
153200131219       //?Indirizzo mittente
153300131219         exsr ViaMittente;
153400160315
153500160315       //?Solo per richiamo da AS400
153600160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
153700160315         //?Controllo prima la località, poi provincia e cap
153800160315         //?Località mittente
153900160315           exsr LocMittente;
154000160315         //?Provincia mittente
154100160315           exsr ProvMittente;
154200160315         //?Cap mittente
154300160315           exsr CapMittente;
154400160520         ENDIF;
154500160520
154600160525       //?Per richiamo da INTERNET
154700160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
154800131219       //?Cap mittente
154900131219         exsr CapMittente;
155000131219
155100131219       //?Località mittente
155200131219         exsr LocMittente;
155300131219
155400131219       //?Provincia mittente
155500131219         exsr ProvMittente;
155600160315         ENDIF;
155700131219
155800131219       //?Nazione mittente
155900131219         exsr NazMittente;
156000131219
156100131219       //?Controllo indirizzo completo del mittente
156200131219         clear wPeso;
156300151008         clear wVolume;
156400131219         exsr IndTotMittente;
156500140402
156600140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
156700140402         wIndirizzo = fiora0I0.vaoinr;
156800140402         wCap = fiora0I0.vaocar;
156900140402         wLocalita = fiora0I0.vaolor;
157000140402         wProvincia = fiora0I0.vaoprr;
157100140402         wNazione = fiora0I0.vaonar;
157200131219
157300131219       ENDSR;
157400131009
157500131009       //--------------------------------------------------------------
157600131009       //?Controllo nominativo mittente.
157700131009       //--------------------------------------------------------------
157800131010       BEGSR  NomeMittente;
157900140211
158000140211         exec sql
158100140211         set :fiora0O0.vaorsr = ucase (:fiora0I0.vaorsr);
158200131009
158300131219       //?Errore se non presente
158400131223         IF  fiora0O0.vaorsr = *blanks;
158500131205           wIdMsg   = 'TIS0482';
158600131009           wIdCampo = 'VAORSR';
158700131204           clear wParm;
158800131009           exsr Messaggi;
158900131219           wErrMitt = *on;
159000131009         ENDIF;
159100131009
159200131009       ENDSR;
159300131009
159400131009       //--------------------------------------------------------------
159500131009       //?Controllo indirizzo mittente.
159600131009       //--------------------------------------------------------------
159700131010       BEGSR  ViaMittente;
159800140211
159900140211         exec sql
160000140211         set :fiora0O0.vaoinr = ucase (:fiora0I0.vaoinr);
160100131009
160200131219       //?Errore se non presente
160300131223         IF  fiora0O0.vaoinr = *blanks;
160400131205           wIdMsg   = 'TIS0283';
160500131009           wIdCampo = 'VAOINR';
160600131204           clear wParm;
160700131009           exsr Messaggi;
160800131219           wErrMitt = *on;
160900131009         ENDIF;
161000131009
161100131009       ENDSR;
161200131009
161300131009       //--------------------------------------------------------------
161400131009       //?Controllo cap mittente.
161500131009       //--------------------------------------------------------------
161600131010       BEGSR  CapMittente;
161700140211
161800140211         exec sql
161900140211         set :fiora0O0.vaocar = ucase (:fiora0I0.vaocar);
162000131009
162100131219       //?Errore se non presente
162200131223         IF  fiora0O0.vaocar = *blanks;
162300131205           wIdMsg   = 'TIS0053';
162400131009           wIdCampo = 'VAOCAR';
162500131204           clear wParm;
162600131009           exsr Messaggi;
162700131219           wErr = *on;
162800131009         ENDIF;
162900131009
163000131009       ENDSR;
163100131009
163200131009       //--------------------------------------------------------------
163300131009       //?Controllo località mittente.
163400131009       //--------------------------------------------------------------
163500131010       BEGSR  LocMittente;
163600140211
163700140211         exec sql
163800140211         set :fiora0O0.vaolor = ucase (:fiora0I0.vaolor);
163900131219
164000131009       //?Errore se non presente codice e località
164100131223         IF  fiora0O0.vaolor = *blanks;
164200131205           wIdMsg   = 'TIS0347';
164300131009           wIdCampo = 'VAOLOR';
164400131204           clear wParm;
164500131009           exsr Messaggi;
164600131219           wErr = *on;
164700131009         ENDIF;
164800131009
164900131009       ENDSR;
165000131009
165100131009       //--------------------------------------------------------------
165200131009       //?Controllo provincia mittente.
165300131009       //--------------------------------------------------------------
165400131010       BEGSR  ProvMittente;
165500140211
165600140211         exec sql
165700140211         set :fiora0O0.vaoprr = ucase (:fiora0I0.vaoprr);
165800131009
165900131009       //?Controllo con la sk delle provincie
166000131009         IF  fiora0O0.vaoprr <> *blanks and
166100131009             %lookup(fiora0O0.vaoprr:skprv) = 0;
166200131205           wIdMsg   = 'TIS0466';
166300131009           wIdCampo = 'VAOPRR';
166400131204           clear wParm;
166500131009           exsr Messaggi;
166600131219           wErrMitt = *on;
166700131009         ENDIF;
166800131009
166900131009       ENDSR;
167000131008
167100131009       //--------------------------------------------------------------
167200131009       //?Controllo nazione mittente.
167300131009       //--------------------------------------------------------------
167400131010       BEGSR  NazMittente;
167500140211
167600140211         exec sql
167700140211         set :fiora0O0.vaonar = ucase (:fiora0I0.vaonar);
167800131009
167900131009       //?Controllo con la sk delle provincie
168000131009         IF  fiora0O0.vaonar <> *blanks and
168100131009             %lookup(fiora0O0.vaonar:sknaz) = 0;
168200140326           wIdMsg   = 'TIS0386';
168300131009           wIdCampo = 'VAONAR';
168400131204           clear wParm;
168500131009           exsr Messaggi;
168600131219           wErrMitt = *on;
168700131009         ENDIF;
168800131009
168900131009       ENDSR;
169000131009
169100131009       //--------------------------------------------------------------
169200131009       //?Controllo indirizzo completo del mittente.
169300131009       //--------------------------------------------------------------
169400131010       BEGSR  IndTotMittente;
169500131009
169600131009         clear TISI95DS;
169700151009         clear FNLV13DS;
169800131204         tisi95ds.I95tcn = '7';
169900131204         tisi95ds.I95nar = fiora0O0.vaonar;
170000131204         tisi95ds.I95cap = fiora0O0.vaocar;
170100131204         tisi95ds.I95loc = fiora0O0.vaolor;
170200131204         tisi95ds.I95prv = fiora0O0.vaoprr;
170300131204         tisi95ds.I95ind = fiora0O0.vaoinr;
170400131204         tisi95ds.I95dat = %dec(%date());
170500131204         tisi95ds.I95lkg = wPeso;
170600151008         tisi95ds.I95lmc = wVolume;
170700151009         fnlv13ds.I13af0 = 'S';
170800151009         fnlv13ds.I13cnv = 'S';
170900151009         fnlv13ds.I13la3 = 'S';
171000131230
171100131230       //?Se richiesto ntw DPD lo passo
171200160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
171300131230           tisi95ds.I95fi1 = fiora0I0.sceltantw;
171400131230         ENDIF;
171500131009
171600131009       //?Solo per richiamo da Internet
171700131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
171800131009         //?se richiesta nazione irlanda forzo cap eire
171900131204           IF  tisi95ds.I95nar = FIORA00_IRLANDA;
172000131204             tisi95ds.I95cap =  FIORA00_EIRE;
172100131009           ENDIF;
172200131205         ENDIF;
172300131205
172400151009       //?controllo cap/località/provincia
172500151009         fnlv13r (kpjba:fnlv13ds:tisi95ds);
172600160525       //?Solo per richiamo da Internet
172700160525       //?controllo gli errori, da AS ci pensa già il programma chiamante
172800160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
172900151016       //?se torna errore per cap generico diversifico il messaggio
173000151016         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
173100151016             %scan('generico':fnlv13ds.O13msg) > 0;
173200151016           wIdMsg   = 'TIS0826';
173300151016           wIdCampo = 'VAOCAR';
173400151016           clear wParm;
173500151016           exsr Messaggi;
173600151016           wErrMitt = *on;
173700151016           leavesr;
173800151016         ENDIF;
173900151009       //?se torna errore imposto messaggio generico
174000151009         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
174100151016           wIdMsg   = 'TIS0056';
174200131205           wIdCampo = 'VAOLOR';
174300131205           clear wParm;
174400131205           exsr Messaggi;
174500131219           wErrMitt = *on;
174600131205           leavesr;
174700131205         ENDIF;
174800160525         ENDIF;
174900131009
175000131009       //?Imposto la linea di instradamento
175100131204         wFiliale = tisi95ds.O95lna;
175200160315
175300160315       //?Imposto la pickup area
175400160315         wPickup = tisi95ds.O95gf2;
175500131009
175600131009       ENDSR;
175700140311
175800140311       //--------------------------------------------------------------
175900140311       //?Controllo chi paga.
176000140311       //--------------------------------------------------------------
176100140311       BEGSR  ChiPaga;
176200140311
176300140311       //?Controllo formale del dato
176400140401         IF  fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE and
176500140401             fiora0I0.vaopag <> FIORA00_PAGA_ORDINANTE and
176600140401             fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
176700140311           wIdMsg   = 'TIS0140';
176800140311           wIdCampo = 'VAOPAG';
176900140311           clear wParm;
177000140311           exsr Messaggi;
177100140311           leavesr;
177200140311         ENDIF;
177300140311
177400140311       //?Solo per richiamo da Internet
177500140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
177600140311         //?Se utente loggato e ritiro presso altro luogo
177700140311         //?il pagamento non può essere a carico del mittente
177800140401           IF  fiora0I0.IdUtente > 0 and fiora0I0.vaocra = 0 and
177900140401               fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
178000140311             wIdMsg   = 'TIS0140';
178100140311             wIdCampo = 'VAOPAG';
178200140311             clear wParm;
178300140311             exsr Messaggi;
178400140311           ENDIF;
178500140311         ENDIF;
178600140311
178700140311       //?Ritiro all'estero non può pagare il mittente
178800140401         IF  fiora0I0.vaopag = FIORA00_PAGA_MITTENTE and
178900140402             wNazione <> *blanks and
179000140402             wNazione <> FIORA00_SANMARINO;
179100140311           wIdMsg   = 'TIS0695';
179200140311           wIdCampo = 'VAOPAG';
179300140311           clear wParm;
179400140311           exsr Messaggi;
179500140402         ENDIF;
179600140311
179700140311       //?Solo per richiamo da Internet
179800140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
179900140311       //?Imposto se prepagato
180000140401           IF  fiora0I0.vaocor = *zeros and
180100140401               fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
180200140311             fiora0O0.prepagato = FIORA00_PREPAGATO;
180300140311           ENDIF;
180400140311         ENDIF;
180500160607
180600160607       //?Solo per richiamo da AS400
180700160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
180800160607         //?Se ORM prepagato e non è ritiro all'estero
180900160607         //?deve pagare il Mittente
181000160607           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
181100160607               wNazione = *blanks and
181200160607               fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE;
181300160607             wIdMsg   = 'TIS0804';
181400160607             wIdCampo = 'VAOPAG';
181500160607             clear wParm;
181600160607             exsr Messaggi;
181700160607           ENDIF;
181800160607         //?Se ORM singolo e paga Destinatario
181900160607           IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO and
182000160607               fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
182100160607           //?il destinatario deve essere presente
182200160607             IF  fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
182300160607               wIdMsg   = 'TIS0805';
182400160607               wIdCampo = 'VAOCRC';
182500160607               clear wParm;
182600160607               exsr Messaggi;
182700160607             ENDIF;
182800160607           //?il destinatario se codificato non può essere 8888 o 9999
182900160607             IF  fiora0I0.vaocrc > 0 and
183000160607                (%subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '8888' or
183100160607                 %subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '9999');
183200160607               wIdMsg   = 'TIS0806';
183300160607               wIdCampo = 'VAOCRC';
183400160607               clear wParm;
183500160607               exsr Messaggi;
183600160607             ENDIF;
183700160607           ENDIF;
183800160607         //?Se Paga Ordinante
183900160607         //?l'ordinante deve essere un codificato
184000160607         //?e non può essere un 8888 o 9999
184100160607           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
184200160607               fiora0I0.vaocor = 0 or
184300160607              (%subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
184400160607               %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999');
184500160607             wIdMsg   = 'TIS0807';
184600160607             wIdCampo = 'VAOCOR';
184700160607             clear wParm;
184800160607             exsr Messaggi;
184900160607           ENDIF;
185000160607         ENDIF;
185100140311
185200140311       ENDSR;
185300140404
185400140404       //--------------------------------------------------------------
185500140404       //?Controllo commissionato.
185600140404       //--------------------------------------------------------------
185700140404       BEGSR  Commissionato;
185800140404
185900140407         wCom = *off;
186000140404
186100140404       //?Solo per richiamo da Internet
186200140404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
186300140404
186400140404         //?Contatto telefonico
186500140404           exec sql
186600140404           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
186700140404
186800140404         //?valido solo se Blank, N o S
186900140404           IF  fiora0O0.contattot <> *blanks and
187000160714               fiora0O0.contattot <> FIORA00_SI and
187100160714               fiora0O0.contattot <> FIORA00_NO;
187200140404             wIdMsg   = 'TIS0720';
187300140404             wIdCampo = 'CONTATTOT';
187400140404             clear wParm;
187500140404             exsr Messaggi;
187600140404           ENDIF;
187700140404
187800140404         //?Ordinante non codificato (è utente senza PSW)
187900160315         //?se il flag non è già impostato
188000140404         //?è sempre commissionato
188100140404           IF  fiora0I0.vaocor = 0;
188200140404             IF  fiora0O0.contattot = *blanks;
188300160714               fiora0O0.contattot = FIORA00_SI;
188400140404             ENDIF;
188500140404           ENDIF;
188600140404
188700160714           IF  fiora0O0.contattot = FIORA00_NO;
188800140407             wCom = *off;
188900140404           ENDIF;
189000160714           IF  fiora0O0.contattot = FIORA00_SI;
189100140407             wCom = *on;
189200140404           ENDIF;
189300140404         ENDIF;
189400160608
189500160608       //?Solo per immissione manuale ORM
189600160610         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
189700160610         //?se non già presente calcolo il flag di commissionato
189800160714           IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL or
189900160714                fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO) and
190000160610                fiora0I0.Contattot = *blanks;
190100160610           //?commissionato se:
190200160610           //?--> codice mittente <> codice ordinante (primi 7 byte)
190300160610           //?--> Ordinante impostato ma non codificato o 8888 o 9999
190400160610           //?--> Filiale emissione <> filiale ritiro e mittente non codificato
190500160610             IF  (%subst(%editc(fiora0I0.vaocra:'X'):1:7) <>
190600160610                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) and
190700160610                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) > *zeros) or
190800160610                 (fiora0I0.vaorso <> *blanks and
190900160610                 (fiora0I0.vaocor = 0 or
191000160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
191100160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999'));
191200160714               fiora0I0.Contattot = FIORA00_SI;
191300160610             ENDIF;
191400160610             IF  fiora0I0.Contattot = *blanks and
191500160610    1            fiora0I0.vaopor <> fiora0I0.vaopoe and
191600160610                 fiora0I0.vaocra = 0;
191700160714               fiora0I0.Contattot = FIORA00_SI;
191800160610             ENDIF;
191900160610           ENDIF;
192000160608
192100160610           exec sql
192200160610           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
192300160610
192400160714           IF  fiora0O0.contattot = FIORA00_SI;
192500160610             wCom = *on;
192600160610           ENDIF;
192700160608         ENDIF;
192800140404
192900140404       ENDSR;
193000140404
193100140404       //--------------------------------------------------------------
193200140404       //?Cerco orari servizio.
193300140404       //--------------------------------------------------------------
193400140404       BEGSR  OrariServizio;
193500140408
193600140408         wPor  = fiora0O0.vaopor;
193700140408         wData = inv_dataritiro;
193800140408
193900140408       //?Richiamo il pgm TRULORSR
194000140408         exsr CallTrulORS;
194100140404
194200140404       //?Orari Standard Servizio
194300140404         fiora0O0.orainizio = trulor2ds.OOR2stis;
194400140404         fiora0O0.orafine   = trulor2ds.OOR2stfs;
194500140404
194600140404       //?Orari Limiti
194700140404         fiora0O0.oracomm = trulor2ds.OOR2lrsc;
194800140404         fiora0O0.oranocomm = trulor2ds.OOR2lrnc;
194900160526
195000160526       //?Se ritiro estero chiamo nuovo pgm per ricerca orari servizio
195100160526         IF  fiora0O0.vaonar <> *blanks;
195200160526           exsr CallFIORE;
195300160526         //?Orari Standard Servizio
195400160526           fiora0O0.orainizio = fiore00ds.OORE00min;
195500160526           fiora0O0.orafine   = fiore00ds.OORE00max;
195600160526         //?Orari Limiti
195700160526           fiora0O0.oracomm   = fiore00ds.OORE00lrg;
195800160526           fiora0O0.oranocomm = fiore00ds.OORE00lrg;
195900160526         ENDIF;
196000140404
196100140404       ENDSR;
196200131205
196300131205       //--------------------------------------------------------------
196400131205       //?Controllo filiale emissione.
196500131205       //--------------------------------------------------------------
196600131205       BEGSR  FilEmissione;
196700131205
196800131205       //?Se richiamo da Internet
196900131205         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
197000131205         //?Se ordinante codificato (è utente con PSW)
197100131205         //?e mittente non codificato
197200131205         //?filiale emissione del codice ordinante
197300140401           IF  fiora0I0.vaocor > 0 and fiora0I0.vaocra = 0;
197400131205             fiora0O0.vaopoe =
197500140401             %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
197600131205           ENDIF;
197700131205         //?Se ordinante non codificato (è utente senza PSW)
197800131205         //?filiale emissione da instradamento
197900140401           IF  fiora0I0.vaocor = 0;
198000131205             fiora0O0.vaopoe = wFiliale;
198100131205           ENDIF;
198200131205         ENDIF;
198300140402
198400140402         //?Se mittene codificato filiale emissione da FNACR
198500160519         //?solo se richiamo da internet
198600160519         IF  fiora0I0.vaocra > 0 and fiora0O0.vaopoe = 0 and
198700160519             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
198800140711           fiora0O0.vaopoe = fiorb0OR.filemi;
198900140402         ENDIF;
199000160315
199100160315       //?Se richiamo da AS400
199200160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
199300160315           fiora0O0.vaopoe = fiora0I0.vaopoe;
199400160524         //?Carico tabella DFT ORM della filiale emissione
199500160524         //?se c'è
199600160524           k_TBEcod = 'DFT';
199700160524           k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
199800160524           chain (k_TBEcod:k_TBEke1) TNTBE01L;
199900160524           IF  %found(TNTBE01L) and TBEatb = *blanks;
200000160524             dDFT = TBEuni;
200100160524           ENDIF;
200200160315         ENDIF;
200300131205
200400131205       //?Cerco il network della filiale emissione
200500131219         IF  fiora0O0.vaopoe > *zeros;
200600160315           wNetworkEmi = GetNtwFiliale(fiora0O0.vaopoe);
200700131219         ENDIF;
200800131205
200900131205       ENDSR;
201000131205
201100131205       //--------------------------------------------------------------
201200131205       //?Controllo filiale ritiro.
201300131205       //--------------------------------------------------------------
201400131205       BEGSR  FilRitiro;
201500131205
201600131205         wOKfilritiro = *off;
201700131205
201800131205       //?Cliente mittente codificato
201900131205       //?la filile ritiro è già calcolata
202000140401         IF  fiora0I0.vaocra > 0;
202100140402           wFiliale = fiorb0OR.filrit;
202200131205           wOKfilritiro = *on;
202300131205         ENDIF;
202400131205
202500131205       //?Calcolo la filiale ritiro
202600151008       //?Se cliente mittente NON codificato
202700151008       //?calcolo la filiale ritiro in base al cappario
202800131205         IF  not wOKfilritiro;
202900151008           exsr IndTotMittente;
203000131205         ENDIF;
203100131205
203200131205       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
203300131205         clear FIOR96ds;
203400160527         fior96ds.IOR96poe = fiora0O0.vaopoe;
203500131205         fior96ds.IOR96por = wFiliale;
203600131205         %subst(kpjba:92:10) = 'GAITRA201';
203700131205         fior96r (kpjba : fior96ds);
203800131205         IF  fior96ds.OOR96err = *blanks and fior96ds.OOR96por <> *zeros;
203900131205           wFiliale = fior96ds.OOR96por;
204000160601         //?In caso di richiamo da AS400
204100160601         //?pulisco il campo della filiale ritiro passato
204200160601         //?in questo modo forzo sempre anche se impostata a mano.
204300160601           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
204400160601             clear fiora0I0.vaopor;
204500160601           ENDIF;
204600131205         ENDIF;
204700160525
204800160525       //?In caso di richiamo in WRITE (Immissione/Copia ORM)
204900160525         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
205000160525             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
205100160525       //?Se la filale ritiro è una filiale gestita devo cercare le filiale Mamma
205200160525       //?per impostarla come filiale ritiro
205300160525           clear FNLV55DS;
205400160525           fnlv55ds.D55tpt = '6';
205500160525           fnlv55ds.D55lin = wFiliale;
205600160525           fnlv55ds.D55drf = %dec(%date());
205700160525           fnlv55r (FNLV55DS);
205800160525           IF  fnlv55ds.D55err = *blanks;
205900160525             wFiliale = fnlv55ds.D55tfa;
206000160525           ENDIF;
206100160525         ENDIF;
206200131205
206300160525       //?Se richiamo da Internet
206400160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
206500131205         fiora0O0.vaopor = wFiliale;
206600160525         ENDIF;
206700160404
206800160404       //?Se richiamo da AS400 e la filiale ritiro non è stata passata
206900160404       //?la imposto ora
207000160404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
207100160404             fiora0I0.vaopor = 0;
207200160404           fiora0I0.vaopor = wFiliale;
207300160525           fiora0O0.vaopor = wFiliale;
207400160404         ENDIF;
207500131205
207600131205       //?Imposto se ritiro all'estero
207700160315         wNetworkRit = GetNtwFiliale(fiora0O0.vaopor);
207800131205         fiora0O0.ritiroest = FIORA00_RITIROESTERO_NO;
207900131205         clear dNTW;
208000131205         k_TBEcod = 'NTW';
208100160315         k_TBEke1 = wNetworkRit;
208200131205         chain (k_TBEcod:k_TBEke1) TNTBE01L;
208300131205         IF  %found(TNTBE01L);
208400131205           dNTW = TBEuni;
208500131205         ENDIF;
208600160315         IF  wNetworkRit = FIORA00_NTW_DPD or
208700131205             dntw.§NTWfie = FIORA00_NTW_ESTERO;
208800131205           fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
208900131205         ENDIF;
209000131205
209100131205       //?Verifico la ritirabilità all'estero
209200131205         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
209300140402             wNazione <> *blanks;
209400131205           clear FNLV12DS;
209500131205           clear TISI95DS;
209600131205           clear TISI97DS;
209700131230           fnlv12ds.I12lang = fiora0I0.idlingua;
209800140402           tisi95ds.I95nar = wNazione;
209900140402           tisi95ds.I95cap = wCap;
210000131205           tisi95ds.I95dat = dataoggi;
210100131205           tisi97ds.I97ntw = wNetworkRit;
210200160315           IF  wNetworkRit = FIORA00_NTW_DPD;
210300160315             tisi95ds.I95fi1 = FIORA00_SERVIZIO_DPD;
210400131205           ENDIF;
210500131205           fnlv12r (fnlv12ds:tisi95ds:tisi97ds);
210600131205           IF  fnlv12ds.O12err <> *blanks;
210700140304             wIdMsg   = 'TIS0718';
210800160315           //?Solo per richiamo da Internet
210900160315             IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
211000131205             wIdCampo = 'SCELTANTW';
211100160315             ELSE;
211200160315               wIdCampo = 'VAOPOR';
211300160315             ENDIF;
211400131205             wParm = fnlv12ds.O12msg;
211500131205             exsr Messaggi;
211600131205           ENDIF;
211700131227
211800131227         //?Imposto il ntw da forzare per ritiro all'estero
211900131227           IF  fiora0I0.sceltantw <> *blanks;
212000131227             fiora0O0.sceltantw = fiora0I0.sceltantw;
212100131227             fiora0O0.forzantw = fiora0I0.sceltantw;
212200131227           ELSE;
212300160315             IF  wNetworkRit = FIORA00_NTW_DPD;
212400131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
212500131227             ENDIF;
212600160315             IF  wNetworkRit = FIORA00_NTW_EEX;
212700131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
212800131227             ENDIF;
212900131227           ENDIF;
213000131205
213100131227         ENDIF;
213200160525
213300160525       //?Se richiamato da AS400
213400160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
213500160525         //?Imposto il ntw da forzare per ritiro all'estero
213600160525           IF  fiora0I0.sceltantw <> *blanks;
213700160525             fiora0O0.sceltantw = fiora0I0.sceltantw;
213800160525             fiora0O0.forzantw = fiora0I0.sceltantw;
213900160525           ELSE;
214000160525             IF  wNetworkRit = FIORA00_NTW_DPD;
214100160525               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
214200160525             ENDIF;
214300160525             IF  wNetworkRit = FIORA00_NTW_EEX;
214400160525               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
214500160525             ENDIF;
214600160525           ENDIF;
214700160526         //?In caso di richiamo in WRITE (Immissione/Copia ORM)
214800160526           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
214900160526         //?Messaggio di avvertimento se filiale ritiro calcolata <> da quella del video
215000160526             IF  wFiliale > 0 and fiora0I0.vaopor > 0 and
215100160526                 wFiliale <> fiora0I0.vaopor;
215200160526               wIdMsg   = 'TIS0833';
215300160526               wIdCampo = 'VAOPOR';
215400160526               clear wParm;
215500160526               exsr Forzatura;
215600160531               IF  wForza;
215700160531                 exsr RoutEnd;
215800160531               ENDIF;
215900160526             ENDIF;
216000160526           ENDIF;
216100160525         ENDIF;
216200151008
216300151120         //?Se ordinante NON codificato quindi utente senza PSW e
216400151008         //?Se mittene NON codificato reimposto la filiale emissione
216500151008         //?la filiale ritiro potrebbe essere variata a causa delle quantità
216600151008         //?inserite
216700160601         //?SOLO se richiamato da Internet
216800160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
216900151120         IF  not wOKfilritiro and fiora0I0.vaocor = 0;
217000151008           fiora0O0.vaopoe = wFiliale;
217100151008         ENDIF;
217200160601         ENDIF;
217300160606
217400160606       //?Dalla versione 'D' della ds di output FIORA0O0
217500160606       //?torno la filiale ritiro calcolata
217600160620       //?e il pickup
217700160728       //?solo per AS400
217800160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
217900160606         IF  fiora0O0.versione > 'C';
218000160606           fiora0O0.filritcalc = wfiliale;
218100160620           fiora0O0.pickupcalc = wpickup;
218200160620         //?se cliente codificato cerco il pickup
218300160620           IF  wOkfilritiro;
218400160620             exsr IndTotMittente;
218500160620             fiora0O0.pickupcalc = wpickup;
218600160620           ENDIF;
218700160606         ENDIF;
218800160728         ENDIF;
218900131227
219000131205       ENDSR;
219100160315
219200160315       //--------------------------------------------------------------
219300160315       //?Controllo filiale ritiro.
219400160315       //--------------------------------------------------------------
219500160601       BEGSR  ControllaFilRitiro;
219600160315
219700160315       //?Filiale ritiro obbligatoria
219800160315         IF  fiora0I0.vaopor = 0;
219900160315           wIdMsg   = 'TIS0809';
220000160315           wIdCampo = 'VAOPOR';
220100160315           clear wParm;
220200160315           exsr Messaggi;
220300160315           leavesr;
220400160315         ENDIF;
220500160315
220600160601       //?Controllo se la filiale ritiro è valida
220700160601         IF  not IsFilValida(fiora0O0.vaopor);
220800160601           wIdMsg   = 'TIS0809';
220900160601           wIdCampo = 'VAOPOR';
221000160601           clear wParm;
221100160601           exsr Messaggi;
221200160601           leavesr;
221300160315         ENDIF;
221400160315       //?Errore se la filiale ritiro NON ha la procedura ORM attiva
221500160315         IF  not IsFilAttivaORM(fiora0O0.vaopor);
221600160607           wIdMsg   = 'TIS0836';
221700160315           wIdCampo = 'VAOPOR';
221800160607           clear wParm;
221900160315           exsr Messaggi;
222000160315         ENDIF;
222100160315
222200160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
222300160315       //?controllo congruenza tra network DPD e filiale ritiro
222400160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
222500160315             wNetworkRit <> FIORA00_NTW_DPD and
222600160315             fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
222700160607           wIdMsg   = 'TIS0837';
222800160315           wIdCampo = 'VAOPOR';
222900160607           clear wParm;
223000160315           exsr Messaggi;
223100160315         ENDIF;
223200160315
223300160315       //?Controllo con cappario DPD se ritiro per DPD
223400160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD or
223500160315             wNetworkRit = FIORA00_NTW_DPD;
223600160315           IF  not IsValidoCapDPD(fiora0O0.vaonar:
223700160315                                  fiora0O0.vaocar:
223800160315                                  fiora0O0.vaodar:
223900160315                                  fiora0I0.vaopkg:
224000160315                                  ds3idp.§3Ilmp:
224100160315                                  fiora0O0.vaopoe);
224200160315             wIdMsg   = 'TIS0811';
224300160315             wIdCampo = 'VAOCAR';
224400160315             clear wParm;
224500160315             exsr Messaggi;
224600160315           ENDIF;
224700160315         ENDIF;
224800160315
224900160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
225000160315       //?e filiale ritiro = a chi sta inserendo ORM
225100160315       //?non posso fare un prepagato
225200160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
225300160315             fiora0O0.vaopor = fiora0I0.fgs and
225400160315             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
225500160607           wIdMsg   = 'TIS0838';
225600160315           wIdCampo = 'VAOTOR';
225700160607           clear wParm;
225800160315           exsr Messaggi;
225900160315         ENDIF;
226000160315
226100160315       //?Non possibile ORM prepagato se ORM Export
226200160315         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
226300160315            (wNetworkRit = FIORA00_SERVIZIO_DPD or
226400160315             wNetworkRit = FIORA00_SERVIZIO_EEX);
226500160607           wIdMsg   = 'TIS0839';
226600160315           wIdCampo = 'VAOTOR';
226700160607           clear wParm;
226800160315           exsr Messaggi;
226900160315         ENDIF;
227000160527
227100160607       //?Se richiesto il ritiro tramite DPD deve essere ORM Singolo
227200160527         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
227300160607             fiora0I0.tor <> FIORA00_TIPOORM_SINGOLO;
227400160527           wIdMsg   = 'TIS0307';
227500160607           wIdCampo = 'VAOTOR';
227600160527           clear wParm;
227700160527           exsr Messaggi;
227800160527         ENDIF;
227900160315
228000160315       ENDSR;
228100131205
228200131205       //--------------------------------------------------------------
228300131205       //?Controllo data ritiro.
228400131205       //--------------------------------------------------------------
228500131205       BEGSR  DataRitiro;
228600131205
228700131223       //?Calcolo la data ritiro
228800131205         clear FIOR97ds;
228900131205         fior97ds.IOR97poe = fiora0O0.vaopoe;
229000131205         fior97ds.IOR97por = fiora0O0.vaopor;
229100140402         fior97ds.IOR97cap = wCap;
229200140402         fior97ds.IOR97loc = wLocalita;
229300160526         fior97ds.IOR97naz = wNazione;
229400131205         IF  fiora0I0.vaodao = 0;
229500131205           fior97ds.IOR97dao = dataoggi;
229600131205         ELSE;
229700131205           fior97ds.IOR97dao = fiora0I0.vaodao;
229800131205         ENDIF;
229900131205         IF  fiora0I0.vaooao = 0;
230000131205           fior97ds.IOR97oao = %dec(%time());
230100131205         ELSE;
230200131205           fior97ds.IOR97oao = fiora0I0.vaooao;
230300131205         ENDIF;
230400131205         fior97ds.IOR97gf2 = tisi95ds.O95gf2;
230500131205         IF  wCom;
230600160714           fior97ds.IOR97com = FIORA00_SI;
230700131205         ENDIF;
230800140331         SELECT;
230900140331           WHEN fiora0O0.prepagato = FIORA00_PREPAGATO;
231000160714             fior97ds.IOR97tor = FIORA00_ORM_BOLLAPREPAGATO;
231100140331           WHEN fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
231200160714             fior97ds.IOR97tor = FIORA00_ORM_INTERNET;
231300140331           OTHER;
231400160714             fior97ds.IOR97tor = FIORA00_ORM_MANUALE;
231500140331         ENDSL;
231600131205         IF  fiora0I0.vaocra > 0;
231700131205           fior97ds.IOR97mcod = 'S';
231800131205         ENDIF;
231900160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
232000131205         fior97ds.IOR97web = 'S';
232100160315         ENDIF;
232200160601
232300160601         //?se ORM manuale e ci sono i dati per Alert Commissionato
232400160601         //?e l'ORM è un commissionato passo il flag
232500160601         IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
232600160601              fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and wCom and
232700160601             (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks);
232800160714           fior97ds.IOR97alert = FIORA00_SI;
232900160601         ENDIF;
233000160601
233100131205         fior97r(kpjba : fior97ds);
233200160601
233300131205         IF  fior97ds.OOR97err = *blanks and fior97ds.OOR97dar <> *zeros;
233400131220           inv_dataritiro = fior97ds.OOR97dar;
233500131205         ENDIF;
233600131205
233700160610         //?Se richiamato da INTERNET
233800160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
233900131220         //?se non è stata immessa imposto quella calcolata
234000131220         IF  fiora0I0.vaodar = 0;
234100131220           fiora0O0.vaodar = inv_dataritiro;
234200131220         ELSE;
234300131220           fiora0O0.vaodar = fiora0I0.vaodar;
234400131220         ENDIF;
234500131223
234600131223         //?Controlli sulla data
234700131223         //?Non accetto a 0
234800131223         IF  fiora0O0.vaodar = 0;
234900140407           wIdMsg   = 'TIS0259';
235000131223           wIdCampo = 'VAODAR';
235100131223           clear wParm;
235200131223           exsr Messaggi;
235300131223           leavesr;
235400131223         ENDIF;
235500131223         //?Non deve essere inferiore alla data minima
235600131223         IF  fiora0O0.vaodar < fior97ds.OOR97dmin;
235700160608           //?Se richiamato da AS400 messaggio di errore diverso
235800160608           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
235900160608             wIdMsg   = 'TIS0842';
236000160608           ELSE;
236100140407           wIdMsg   = 'TIS0730';
236200160608           ENDIF;
236300131223           wIdCampo = 'VAODAR';
236400140407           IF  wCom;
236500140407             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
236600140407           ELSE;
236700140407             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
236800140407           ENDIF;
236900160526           IF  fiora0I0.vaonar <> *blanks;
237000160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
237100160526           ENDIF;
237200131223           exsr Messaggi;
237300131223         ENDIF;
237400131223         //?Non deve essere superiore alla data massima
237500131223         IF  fiora0O0.vaodar > fior97ds.OOR97dmaxb;
237600131223           wIdMsg   = 'TIS0116';
237700131223           wIdCampo = 'VAODAR';
237800131223           clear wParm;
237900131223           exsr Messaggi;
238000131223         ENDIF;
238100160610         ENDIF;
238200160610
238300160610       //?Se richiamato da AS400
238400160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
238500160610         //?Se sono in immissione ORM
238600160610           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
238700160610           //?se posso anticipare la data passo il flag
238800160610             IF  fior97ds.OOR97dmin < fior97ds.OOR97dar;
238900160714               fiora0O0.anticipa = FIORA00_ANTICIPA;
239000160610             ENDIF;
239100160610             //?Se non passata imposto la data calcolata
239200160610             IF  fiora0I0.vaodar = 0;
239300160610               fiora0I0.vaodar = inv_dataritiro;
239400160610             ENDIF;
239500160610           ENDIF;
239600160610           //?Non deve essere a 0
239700160610           IF  fiora0I0.vaodar = 0;
239800160610             wIdMsg   = 'TIS0259';
239900160610             wIdCampo = 'VAODAR';
240000160610             clear wParm;
240100160610             exsr Messaggi;
240200160610             leavesr;
240300160610           ENDIF;
240400160610           //?Non deve essere inferiore alla data minima
240500160610           IF  fiora0I0.vaodar < fior97ds.OOR97dmin;
240600160610             wIdMsg   = 'TIS0842';
240700160610             wIdCampo = 'VAODAR';
240800160610             IF  wCom;
240900160610               wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
241000160610             ELSE;
241100160610               wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
241200160610             ENDIF;
241300160610             IF  fiora0I0.vaonar <> *blanks;
241400160610               wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
241500160610             ENDIF;
241600160610             exsr Messaggi;
241700160610           ENDIF;
241800160610           //?Non deve essere superiore alla data massima
241900160610           IF  fiora0I0.vaodar > fior97ds.OOR97dmaxb;
242000160610             wIdMsg   = 'TIS0116';
242100160610             wIdCampo = 'VAODAR';
242200160610             clear wParm;
242300160610             exsr Messaggi;
242400160610           ENDIF;
242500160610           //?In caso di richiamo in WRITE (Immissione/Copia ORM)
242600160610           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
242700160610             //?Se posso anticipare ma la data ritiro immessa è = alla
242800160610             //?data minima pulisco il flag di anticipa
242900160714             IF  fiora0O0.anticipa = FIORA00_ANTICIPA and
243000160610                 fior97ds.OOR97dmin = fiora0I0.vaodar;
243100160610               clear fiora0O0.anticipa;
243200160610             ENDIF;
243300160610             fiora0O0.vaodar = fiora0I0.vaodar;
243400160610           //?Messaggio di avvertimento data ritiro calcolata <> da quella del video
243500160610             IF  inv_dataritiro <> fiora0O0.vaodar;
243600160610               wIdMsg   = 'TIS0843';
243700160610               wIdCampo = 'VAODAR';
243800160610               clear wParm;
243900160610               exsr Forzatura;
244000160610               IF  wForza;
244100160610                 exsr RoutEnd;
244200160610               ENDIF;
244300160610             ENDIF;
244400160610           ENDIF;
244500160610           fiora0O0.vaodar = fiora0I0.vaodar;
244600160610         ENDIF;
244700131205
244800131205       ENDSR;
244900131011
245000131011       //--------------------------------------------------------------
245100131011       //?Controllo ora pronta merce.
245200131011       //--------------------------------------------------------------
245300131011       BEGSR  OraRitiro;
245400131219
245500131223         IF  fiora0I0.vaoorr > 0;
245600131223           fiora0O0.vaoorr = fiora0I0.vaoorr;
245700131223         ENDIF;
245800131028
245900140407         IF  fiora0O0.vaoorr = *zeros and not wCom;
246000131028           wIdMsg   = 'TIS0423';
246100131028           wIdCampo = 'VAOORR';
246200131204           clear wParm;
246300131028           exsr Messaggi;
246400131219           leavesr;
246500131028         ENDIF;
246600131028
246700140407         IF  wCom and fiora0O0.vaoorr = 0;
246800131220           leavesr;
246900131220         ENDIF;
247000131220
247100131223         IF  fiora0O0.vaoorr < 1 or fiora0O0.vaoorr > 2359;
247200131028           wIdMsg   = 'TIS0425';
247300131028           wIdCampo = 'VAOORR';
247400131204           clear wParm;
247500131028           exsr Messaggi;
247600131028         ENDIF;
247700160404
247800160404       ENDSR;
247900160404
248000160404       //--------------------------------------------------------------
248100160404       //?Controllo ora pronta merce con orari servizio.
248200160404       //--------------------------------------------------------------
248300160404       BEGSR  OraRitiroServizio;
248400140306
248500140306       //?Il controllo dell'ora pronta merce non lo faccio se ritiro estero
248600140306         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
248700140306           leavesr;
248800140306         ENDIF;
248900131220
249000131220       //?Controllo ora pronta merce se non è presa da anagrafica clienti
249100131220         SELECT;
249200140402           WHEN  fiorb0OR.oraritiro > 0 and
249300140402                 fiora0O0.vaoorr = fiorb0OR.oraritiro;
249400131220           OTHER;
249500131220       //?Controllo ora pronta merce con ora fine standard
249600140331           IF  fiora0O0.vaoorr > trulor2ds.OOR2stfs;
249700140306             wIdMsg   = 'TIS0724';
249800131223             wIdCampo = 'VAOORR';
249900140331             wParm = %editw(trulor2ds.OOR2stis:'  :  ') +
250000140331                     %editw(trulor2ds.OOR2stfs:'  :  ');
250100131223             exsr Messaggi;
250200131223           ENDIF;
250300131220         ENDSL;
250400131220
250500131220       //?Controllo ora immissione ORM con ora massima ritiro
250600140306         IF  %dec(%subst(%editc(fiora0I0.vaooao:'X'):1:4):4:0)
250700140410             > trulor2ds.OOR2stfs and
250800140306             fiora0I0.vaodao = fiora0O0.vaodar;
250900140306           wIdMsg   = 'TIS0721';
251000140306           wIdCampo = 'VAOOAO';
251100140327           wParm = %editw(trulor2ds.OOR2stfs:'  :  ');
251200131223           exsr Messaggi;
251300131220         ENDIF;
251400131011
251500131011       ENDSR;
251600131028
251700131028       //--------------------------------------------------------------
251800131028       //?Controllo orari apertura.
251900131028       //--------------------------------------------------------------
252000140404       BEGSR  OrariApertura;
252100131028
252200140401         IF  fiora0I0.dalleam > 0 or fiora0I0.alleam > 0 or
252300140401             fiora0I0.dallepm > 0 or fiora0I0.allepm > 0;
252400131204           clear TRUL03ds;
252500140401           trul03ds.I03hm1 = fiora0I0.dalleam;
252600140401           trul03ds.I03hm2 = fiora0I0.alleam;
252700140401           trul03ds.I03hm3 = fiora0I0.dallepm;
252800140401           trul03ds.I03hm4 = fiora0I0.allepm;
252900131204           trul03r (TRUL03DS);
253000131204           IF  trul03ds.O03err <> *off;
253100131204             wIdMsg   = 'TIS0171';
253200131204             SELECT;
253300131204               WHEN  trul03ds.O03errpos = 1;
253400131204                 wIdCampo = 'DALLEAM';
253500131204                 clear wParm;
253600131204                 exsr Messaggi;
253700131204               WHEN  trul03ds.O03errpos = 2;
253800131204                 wIdCampo = 'ALLEAM';
253900131204                 clear wParm;
254000131204                 exsr Messaggi;
254100131204               WHEN  trul03ds.O03errpos = 3;
254200131204                 wIdCampo = 'DALLEPM';
254300131204                 clear wParm;
254400131204                 exsr Messaggi;
254500131204               WHEN  trul03ds.O03errpos = 4;
254600131204                 wIdCampo = 'ALLEPM';
254700131204                 clear wParm;
254800131204                 exsr Messaggi;
254900131204             ENDSL;
255000131204           ENDIF;
255100131204         ENDIF;
255200131028
255300131028       ENDSR;
255400131028
255500131028       //--------------------------------------------------------------
255600131028       //?Controllo quantità.
255700131028       //--------------------------------------------------------------
255800131028       BEGSR  Quantita;
255900160613
256000160613         wErrQta = *off;
256100140410
256200140410       //?Se Prepagato obbligo di colli e peso
256300140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
256400140410             fiora0I0.vaoncl = 0;
256500140410           wIdMsg   = 'TIS0726';
256600140410           wIdCampo = 'VAONCL';
256700140410           clear wParm;
256800140410           exsr Messaggi;
256900160613           wErrQta = *on;
257000140410         ENDIF;
257100140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
257200140410             fiora0I0.vaopkg = 0;
257300140410           wIdMsg   = 'TIS0727';
257400140410           wIdCampo = 'VAOPKG';
257500140410           clear wParm;
257600140410           exsr Messaggi;
257700160613           wErrQta = *on;
257800140410         ENDIF;
257900140410
258000140410       //?Se richiesto il ritiro tramite DPD
258100140410       //?il peso è obbligatorio e deve essere inferiore al limite da tab. 3I
258200160714         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
258300140410           IF  fiora0I0.vaopkg = 0;
258400140410             wIdMsg   = 'TIS0727';
258500140410             wIdCampo = 'VAOPKG';
258600140410             clear wParm;
258700140410             exsr Messaggi;
258800160613             wErrQta = *on;
258900140410           ENDIF;
259000140410           IF  fiora0I0.vaopkg > 0 and ds3idp.§3Ipkd > 0 and
259100140410               fiora0I0.vaopkg > ds3idp.§3Ipkd;
259200140410             wIdMsg   = 'TIS0694';
259300140410             wIdCampo = 'VAOPKG';
259400140410             wParm = %editc(ds3idp.§3Ipkd:'3');
259500140410             exsr Messaggi;
259600160613             wErrQta = *on;
259700140410           ENDIF;
259800140410       //?i colli sono obbligatori e deve essere impostato a 1
259900140410           IF  fiora0I0.vaoncl <> 1;
260000140410             wIdMsg   = 'TIS0693';
260100140410             wIdCampo = 'VAONCL';
260200140410             clear wParm;
260300140410             exsr Messaggi;
260400160613             wErrQta = *on;
260500140410           ENDIF;
260600140410         ENDIF;
260700160922
260800160922       //?Controllo subito i limiti delle quantità
260900160922         exsr LimitiQta;
261000160922       //?Se da AS400 e c'è errore esco
261100160922         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
261200160922             wErrQta;
261300160922           leavesr;
261400160922         ENDIF;
261500160922
261600140410       //?Se Prepagato finisco qua i controlli
261700140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO;
261800140410           leavesr;
261900140410         ENDIF;
262000160530
262100160601       //?Faccio controlli diversificati tra Internet ed AS400
262200140331
262300160601       //?Controlli per richiamo da internet
262400160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
262500160601
262600140331       //?Almeno 1 tra colli - bancali deve essere presente
262700140401         IF  fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
262800140331           wIdMsg   = 'TIS0729';
262900140331           wIdCampo = 'VAONCL';
263000140331           clear wParm;
263100140331           exsr Messaggi;
263200140331         ENDIF;
263300140331
263400140331       //?Peso obbligatorio
263500140429         IF  fiora0I0.vaopkg = 0;
263600140429           wIdMsg   = 'TIS0727';
263700140429           wIdCampo = 'VAOPKG';
263800140429           clear wParm;
263900140429           exsr Messaggi;
264000140429         ENDIF;
264100131204
264200131204       //?Controllo con i flag su Anagrafica clienti ritiro
264300140331       //?Per internet facciamo solo i controlli di 'O' obbligatorio
264400160714           IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO and
264500160714               fiora0I0.vaoncl = 0;
264600140331             wIdMsg   = 'TIS0726';
264700140331             wIdCampo = 'VAONCL';
264800140331             clear wParm;
264900140331             exsr Messaggi;
265000140331           ENDIF;
265100140331
265200160714           IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO and
265300160714               fiora0I0.vaopkg = 0;
265400140331             wIdMsg   = 'TIS0727';
265500140331             wIdCampo = 'VAOPKG';
265600140331             clear wParm;
265700140331             exsr Messaggi;
265800140331           ENDIF;
265900140331
266000160714           IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO and
266100160714               fiora0I0.vaovlm = 0;
266200140331             wIdMsg   = 'TIS0728';
266300140331             wIdCampo = 'VAOVLM';
266400140331             clear wParm;
266500140331             exsr Messaggi;
266600140331           ENDIF;
266700140331
266800160714           IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO and
266900160714               fiora0I0.vaobnc = 0;
267000140331             wIdMsg   = 'TIS0722';
267100140331             wIdCampo = 'VAOBNC';
267200140331             clear wParm;
267300140331             exsr Messaggi;
267400140331           ENDIF;
267500160601
267600140331         ENDIF;
267700160601
267800160601       //?Controlli Se richiamato da AS400
267900160601       //?faccio prima i controlli con i flag dell'anagrafica mittente
268000160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
268100160601         //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
268200160601         //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
268300160601           IF  fiorb0OR.flagfncl <> *blanks and fiora0I0.vaoncl = 0;
268400160601             wIdMsg   = 'TIS0726';
268500160601             wIdCampo = 'VAONCL';
268600160601             clear wParm;
268700160714             IF  fiorb0OR.flagfncl = FIORA00_FORZABILE;
268800160601               exsr Forzatura;
268900160601               IF  wForza;
269000160601                 exsr RoutEnd;
269100160601               ENDIF;
269200160601             ENDIF;
269300160714             IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO;
269400160601               exsr Messaggi;
269500160613               wErrQta = *on;
269600160613               leavesr;
269700160601             ENDIF;
269800160601           ENDIF;
269900160601           IF  fiorb0OR.flagfpkg <> *blanks and fiora0I0.vaopkg = 0;
270000160601             wIdMsg   = 'TIS0727';
270100160601             wIdCampo = 'VAOPKG';
270200160601             clear wParm;
270300160714             IF  fiorb0OR.flagfpkg = FIORA00_FORZABILE;
270400160601               exsr Forzatura;
270500160601               IF  wForza;
270600160601                 exsr RoutEnd;
270700160601               ENDIF;
270800160601             ENDIF;
270900160714             IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO;
271000160601               exsr Messaggi;
271100160613               wErrQta = *on;
271200160613               leavesr;
271300160601             ENDIF;
271400160601           ENDIF;
271500160601           IF  fiorb0OR.flagfvlm <> *blanks and fiora0I0.vaovlm = 0;
271600160601             wIdMsg   = 'TIS0728';
271700160601             wIdCampo = 'VAOVLM';
271800160601             clear wParm;
271900160714             IF  fiorb0OR.flagfvlm = FIORA00_FORZABILE;
272000160601               exsr Forzatura;
272100160601               IF  wForza;
272200160601                 exsr RoutEnd;
272300160601               ENDIF;
272400160601             ENDIF;
272500160714             IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO;
272600160601               exsr Messaggi;
272700160613               wErrQta = *on;
272800160613               leavesr;
272900160601             ENDIF;
273000160601           ENDIF;
273100160601           IF  fiorb0OR.flagfbnc <> *blanks and fiora0I0.vaobnc = 0;
273200160601             wIdMsg   = 'TIS0722';
273300160601             wIdCampo = 'VAOBNC';
273400160601             clear wParm;
273500160714             IF  fiorb0OR.flagfbnc = FIORA00_FORZABILE;
273600160601               exsr Forzatura;
273700160601               IF  wForza;
273800160601                 exsr RoutEnd;
273900160601               ENDIF;
274000160601             ENDIF;
274100160714             IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO;
274200160601               exsr Messaggi;
274300160613               wErrQta = *on;
274400160613               leavesr;
274500160601             ENDIF;
274600160601           ENDIF;
274700160601         //?In caso di Immissione/Copia ORM
274800160601         //?immissione Manuale
274900160601           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
275000160714              (fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL or
275100160714               fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO) and
275200160601           //?Almeno 1 tra colli - bancali deve essere presente
275300160601               fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
275400160601             wIdMsg   = 'TIS0729';
275500160601             wIdCampo = 'VAONCL';
275600160601             clear wParm;
275700160601             exsr Messaggi;
275800160613             wErrQta = *on;
275900160613             leavesr;
276000160601           ENDIF;
276100160601         //?Almeno 1 tra peso volume o bancali deve essere presente
276200160610           IF  fiora0I0.vaobnc = 0 and fiora0I0.vaovlm = 0 and
276300160601               fiora0I0.vaopkg = 0;
276400160601             wIdMsg   = 'TIS0235';
276500160601             wIdCampo = 'VAOPKG';
276600160601             clear wParm;
276700160601             exsr Messaggi;
276800160613             wErrQta = *on;
276900160613             leavesr;
277000160601           ENDIF;
277100160601         ENDIF;
277200131028
277300131028       ENDSR;
277400160922
277500160922       //--------------------------------------------------------------
277600160922       //?Controllo limiti quantità.
277700160922       //--------------------------------------------------------------
277800160922       BEGSR  LimitiQta;
277900160922
278000160922         clear TRUL73DS;
278100160922         trul73ds.I73tsp = 'C';
278200160922         trul73ds.I73ncl = fiora0I0.vaoncl;
278300160922         trul73ds.I73psk = fiora0I0.vaopkg;
278400160922         trul73ds.I73vmc = fiora0I0.vaovlm;
278500160922         trul73r (trul73ds);
278600160922
278700160922       //?Limite colli bloccante
278800160922         SELECT;
278900160922         WHEN  trul73ds.O73fclm = '1';
279000160922           wIdMsg   = 'TIS0850';
279100160922           wIdCampo = 'VAONCL';
279200160922           wParm = %editc(trul73ds.O73lclm:'4');
279300160922           exsr Messaggi;
279400160922           wErrQta = *on;
279500160922       //?Limite peso bloccante
279600160922         WHEN  trul73ds.O73fkgm = '1';
279700160922           wIdMsg   = 'TIS0851';
279800160922           wIdCampo = 'VAOPKG';
279900160922           wParm = %editc(trul73ds.O73lkgm:'4');
280000160922           exsr Messaggi;
280100160922           wErrQta = *on;
280200160922       //?Limite volume bloccante
280300160922         WHEN  trul73ds.O73fmcm = '1';
280400160922           wIdMsg   = 'TIS0852';
280500160922           wIdCampo = 'VAOVLM';
280600160922           wParm = %editc(trul73ds.O73lmcm:'4');
280700160922           exsr Messaggi;
280800160922           wErrQta = *on;
280900160922         ENDSL;
281000160922
281100160922         clear TRUL731DS;
281200160922         trul731ds.I731bnc = fiora0I0.vaobnc;
281300160922         trul731r (trul731ds);
281400160922
281500160922       //?Limite bancali bloccante
281600160922         IF  trul731ds.O731fbncm = '1';
281700160922           wIdMsg   = 'TIS0853';
281800160922           wIdCampo = 'VAOBNC';
281900160922           wParm = %editc(trul731ds.O731lbncm:'4');
282000160922           exsr Messaggi;
282100160922           wErrQta = *on;
282200160922         ENDIF;
282300160922
282400160922       //?Se da AS400 faccio i controlli anche per i limiti forzabili
282500160922         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
282600160922         //?Limite colli forzabile
282700160922           SELECT;
282800160922           WHEN  trul73ds.O73fclf = '1';
282900160922             wIdMsg   = 'TIS0850';
283000160922             wIdCampo = 'VAONCL';
283100160922             wParm = %editc(trul73ds.O73lclf:'4');
283200160922             exsr Forzatura;
283300160922             wErrQta = *on;
283400160922         //?Limite peso forzabile
283500160922           WHEN  trul73ds.O73fkgf = '1';
283600160922             wIdMsg   = 'TIS0851';
283700160922             wIdCampo = 'VAOPKG';
283800160922             wParm = %editc(trul73ds.O73lkgf:'4');
283900160922             exsr Forzatura;
284000160922             wErrQta = *on;
284100160922         //?Limite volume forzabile
284200160922           WHEN  trul73ds.O73fmcf = '1';
284300160922             wIdMsg   = 'TIS0852';
284400160922             wIdCampo = 'VAOVLM';
284500160922             wParm = %editc(trul73ds.O73lmcf:'4');
284600160922             exsr Forzatura;
284700160922             wErrQta = *on;
284800160922         //?Limite bancali forzabile
284900160922           WHEN  trul731ds.O731fbncf = '1';
285000160922             wIdMsg   = 'TIS0853';
285100160922             wIdCampo = 'VAOBNC';
285200160922             wParm = %editc(trul731ds.O731lbncf:'4');
285300160922             exsr Forzatura;
285400160922             wErrQta = *on;
285500160922           ENDSL;
285600160922         ENDIF;
285700160922
285800160922       ENDSR;
285900151008
286000151008       //--------------------------------------------------------------
286100151008       //?Calcolo peso/volume più alto per instradamento.
286200151008       //--------------------------------------------------------------
286300151008       BEGSR  PesoVolume;
286400160606
286500160606       //?Sviluppo il peso e/o volume per calcolo instradamento
286600160606         clear wPesomax;
286700160606         clear wVolumemax;
286800160606         clear wPesoaut;
286900160606         clear wVolumeaut;
287000160606         clear wPesoblc;
287100160606         clear wVolumeblc;
287200160606         clear wPesomtc;
287300160606         clear wVolumemtc;
287400151008
287500151008         wPeso = fiora0I0.vaopkg;
287600151008         wVolume = fiora0I0.vaovlm;
287700160606
287800160606       //?se non inserito il peso lo sviluppo
287900160606         IF  wPeso = 0;
288000160606           SELECT;
288100160606           WHEN  fiora0I0.vaobnc > 0;
288200160606             eval(h) wPesomax =
288300160606                 fiora0I0.vaobnc / dDFT.d§DFTbnc * dDFT.d§DFTpkg;
288400160606           WHEN  fiora0I0.vaovlm > 0;
288500160606             eval(h) wPesomax = fiora0I0.vaovlm * dDFT.d§DFTpkg;
288600160606           ENDSL;
288700160606       //?se peso troppo alto imposto il massimo consentito
288800160606           IF  wPesomax > 999999,9;
288900160606             wPeso = 999999,9;
289000160606           ELSE;
289100160606             wPeso = wPesomax;
289200160606           ENDIF;
289300160606         ENDIF;
289400151008
289500160606       //?se non inserito il volume lo sviluppo
289600151008         IF  wVolume = 0;
289700151008           SELECT;
289800151008           WHEN  fiora0I0.vaobnc > 0;
289900151008             eval(h) wVolumemax = fiora0I0.vaobnc / dDFT.d§DFTbnc;
290000151008           WHEN  fiora0I0.vaopkg > 0;
290100151008             eval(h) wVolumemax = fiora0I0.vaopkg / dDFT.d§DFTpkg;
290200151008           ENDSL;
290300151008       //?se volume troppo alto imposto il massimo consentito
290400151008           IF  wVolumemax > 99,999;
290500151008             wVolume = 99,990;
290600151008           ELSE;
290700151008             wVolume = wVolumemax;
290800151008           ENDIF;
290900151008         ENDIF;
291000160606
291100160606       //?Se richiamato da AS400 ho altre quantità che posso inserire
291200160606       //?quindi se inserite le sviluppo
291300160606       //?sono quantità da sviluppare in m³
291400160606         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
291500160606           IF  fiora0I0.vaoatt > 0;
291600160606             eval(h) wVolumeaut = fiora0I0.vaoatt * dDFT.d§DFTaut;
291700160606             eval(h) wPesoaut = wVolumeaut * dDFT.d§DFTpkg;
291800160606           ENDIF;
291900160606           IF  fiora0I0.vaomtc > 0;
292000160606             eval(h) wVolumemtc = fiora0I0.vaomtc * dDFT.d§DFTmtc;
292100160606             eval(h) wPesomtc = wVolumemtc * dDFT.d§DFTpkg;
292200160606           ENDIF;
292300160606           IF  fiora0I0.vaoblc > 0;
292400160606             eval(h) wVolumeblc = fiora0I0.vaoblc * dDFT.d§DFTblc;
292500160606             eval(h) wPesoblc = wVolumeblc * dDFT.d§DFTpkg;
292600160606           ENDIF;
292700160606           IF  wVolumeaut > 99,999;
292800160606             wVolumeaut = 99,999;
292900160606           ENDIF;
293000160606           IF  wPesoaut > 999999,9;
293100160606             wPesoaut = 999999,9;
293200160606           ENDIF;
293300160606           IF  wVolumemtc > 99,999;
293400160606             wVolumemtc = 99,999;
293500160606           ENDIF;
293600160606           IF  wPesomtc > 999999,9;
293700160606             wPesomtc = 999999,9;
293800160606           ENDIF;
293900160606           IF  wVolumeblc > 99,999;
294000160606             wPesoblc = 999999,9;
294100160606           ENDIF;
294200160606           IF  wPesoblc > 999999,9;
294300160606             wVolumeblc = 99,999;
294400160606           ENDIF;
294500160606         ENDIF;
294600160606
294700160606       //?Dalla versione 'D' della ds di output FIORA0O0
294800160606       //?torno il peso e volume da memorizzare su FNORG
294900160728       //?solo per AS400
295000160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
295100160606         IF  fiora0O0.versione > 'C';
295200160615           fiora0O0.volume = wVolume;
295300160615           fiora0O0.peso = wPeso;
295400160606           IF  wVolumeaut > wVolume;
295500160615             fiora0O0.volume = wVolumeaut;
295600160606           ENDIF;
295700160606           IF  wPesoaut > wPeso;
295800160615             fiora0O0.peso = wPesoaut;
295900160606           ENDIF;
296000160606           IF  wVolumemtc > wVolume;
296100160615             fiora0O0.volume = wVolumemtc;
296200160606           ENDIF;
296300160606           IF  wPesomtc > wPeso;
296400160615             fiora0O0.peso = wPesomtc;
296500160606           ENDIF;
296600160606           IF  wVolumeblc > wVolume;
296700160615             fiora0O0.volume = wVolumeblc;
296800160606           ENDIF;
296900160606           IF  wPesoblc > wPeso;
297000160615             fiora0O0.peso = wPesoblc;
297100160606           ENDIF;
297200160615           wVolume = fiora0O0.volume;
297300160615           wPeso = fiora0O0.peso;
297400160606         ENDIF;
297500160728         ENDIF;
297600151008
297700151008       ENDSR;
297800131204
297900131204       //--------------------------------------------------------------
298000131204       //?Controllo la natura della merce.
298100131204       //--------------------------------------------------------------
298200131204       BEGSR  NaturaMerce;
298300131204
298400140404       //?Se richiamato da internet
298500140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
298600140411       //?Obbligo della natura merce se mittente non codificato
298700140416       //?                         e se destinatario non codificato
298800140416             fiora0I0.vaonam = *blanks and fiora0I0.vaocra = *zeros and
298900140416             fiora0I0.vaocrc = *zeros;
299000140411           wIdMsg   = 'TIS0143';
299100140411           wIdCampo = 'VAONAM';
299200140411           clear wParm;
299300140411           exsr Messaggi;
299400140411           leavesr;
299500140407         ENDIF;
299600140211
299700140211         IF  fiora0I0.vaonam <> *blanks;
299800140211           exec sql
299900140211           set :fiora0O0.vaonam = ucase (:fiora0I0.vaonam);
300000131223         ENDIF;
300100131204
300200131204       ENDSR;
300300131204
300400131204       //--------------------------------------------------------------
300500131204       //?Controllo uno o più destinatari.
300600131204       //--------------------------------------------------------------
300700131204       BEGSR  UnoPiuDest;
300800131220
300900131220         fiora0O0.unopiudest = fiora0I0.unopiudest;
301000160607
301100160607       //?I controlli sucessivi li devo fare solo se richiamato da Internet
301200160607         IF  fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET;
301300160607           leavesr;
301400160607         ENDIF;
301500131204
301600131204       //?Accetto 1 o 2
301700160714         IF  fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO and
301800160714             fiora0O0.unopiudest <> FIORA00_PIUDESTINATARI;
301900131204           wIdMsg   = 'TIS0295';
302000131204           wIdCampo = 'UNOPIUDEST';
302100131204           clear wParm;
302200131204           exsr Messaggi;
302300131219           leavesr;
302400131204         ENDIF;
302500131204
302600131204       //?Se ORM prepagato SOLO 1 destinatario
302700131204         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
302800160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
302900131204           wIdMsg   = 'TIS0307';
303000131204           wIdCampo = 'UNOPIUDEST';
303100131204           clear wParm;
303200131204           exsr Messaggi;
303300131204         ENDIF;
303400131204
303500131204       //?Se richiesto il ritiro tramite DPD SOLO 1 destinatario
303600131204         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
303700160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
303800131204           wIdMsg   = 'TIS0307';
303900131204           wIdCampo = 'UNOPIUDEST';
304000131204           clear wParm;
304100131204           exsr Messaggi;
304200131204         ENDIF;
304300131204
304400131204       ENDSR;
304500131205
304600131205       //--------------------------------------------------------------
304700131205       //?Controllo Referente.
304800131205       //--------------------------------------------------------------
304900131205       BEGSR  Referente;
305000131205
305100131205       //?Se mittente non codificato oppure ORM commissionato
305200131205       //?il referente è obbligatorio
305300140401         IF  (fiora0I0.vaocra = 0 or wCom) and
305400131223              fiora0I0.vaorer = *blanks;
305500131205           wIdMsg   = 'TIS0290';
305600131205           wIdCampo = 'VAORER';
305700131205           clear wParm;
305800131205           exsr Messaggi;
305900131205         ENDIF;
306000160601
306100160613       //?se Richiamato da AS400
306200160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
306300160613         //?Referente obbligatorio
306400160613         //?se Prepagato
306500160613           IF   fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
306600160613                fiora0I0.vaorer = *blanks;
306700160613             wIdMsg   = 'TIS0290';
306800160613             wIdCampo = 'VAORER';
306900160613             clear wParm;
307000160613             exsr Messaggi;
307100160613           ENDIF;
307200160613         ENDIF;
307300160613
307400131223         IF  fiora0I0.vaorer <> *blanks;
307500140211           exec sql
307600140211           set :fiora0O0.vaorer = ucase (:fiora0I0.vaorer);
307700131223         ENDIF;
307800131205
307900131205       ENDSR;
308000131223
308100131223       //--------------------------------------------------------------
308200131223       //?Controllo Telefono.
308300131223       //--------------------------------------------------------------
308400131223       BEGSR  Telefono;
308500131223
308600131223       //?Se mittente non codificato oppure ORM commissionato
308700131223       //?il telefono è obbligatorio
308800140401         IF  (fiora0I0.vaocra = 0 or wCom) and
308900131223              fiora0I0.vaoter = *blanks;
309000131223           wIdMsg   = 'TIS0246';
309100131223           wIdCampo = 'VAOTER';
309200131223           clear wParm;
309300131223           exsr Messaggi;
309400131223         ENDIF;
309500160601
309600160601       //?se Richiamato da AS400
309700160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
309800160613         //?Telefono obbligatorio
309900160613         //?se Prepagato
310000160613           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
310100160613               fiora0I0.vaoter = *blanks;
310200160613             wIdMsg   = 'TIS0246';
310300160613             wIdCampo = 'VAOTER';
310400160613             clear wParm;
310500160613             exsr Messaggi;
310600160613           ENDIF;
310700160601         ENDIF;
310800131223
310900131223         IF  fiora0I0.vaoter <> *blanks;
311000140211           exec sql
311100140211           set :fiora0O0.vaoter = ucase (:fiora0I0.vaoter);
311200131223         ENDIF;
311300131223
311400131223       ENDSR;
311500140311
311600140311       //--------------------------------------------------------------
311700140311       //?Controllo indirizzo mail.
311800140311       //--------------------------------------------------------------
311900140311       BEGSR  Mail;
312000140311
312100140311         IF  fiora0I0.mail <> *blanks;
312200140311           clear dsemail;
312300140311           dsemail.§emlindi = fiora0I0.mail;
312400140311           xemail (dsemail);
312500140311           IF  dsemail.§emlerro <> '0';
312600140424             wIdMsg   = 'TIS0732';
312700140311             wIdCampo = 'MAIL';
312800140424             clear wParm;
312900140311             exsr Messaggi;
313000140311             leavesr;
313100140311           ENDIF;
313200140311
313300140424           fiora0O0.mail = fiora0I0.mail;
313400140722           IF  dsemail.§emlindo <> *blanks;
313500140722             fiora0O0.mail = dsemail.§emlindo;
313600140722           ENDIF;
313700140311         ENDIF;
313800140311
313900140311       ENDSR;
314000140311
314100140311       //--------------------------------------------------------------
314200140311       //?Controllo n.cellulare.
314300140311       //--------------------------------------------------------------
314400140311       BEGSR  Sms;
314500140311
314600140311         IF  fiora0I0.sms <> *blanks;
314700140311           pInCell = %trim(fiora0I0.sms);
314800140311           clear pOutErr;
314900140722           clear pOutCell;
315000140311           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
315100140311             wIdMsg   = 'TIS0725';
315200140311             wIdCampo = 'SMS';
315300140311             clear wParm;
315400140311             exsr Messaggi;
315500160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
315600160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
315700160601             fiora0I0.sms;
315800140311             leavesr;
315900140311           ENDIF;
316000140311
316100140416           fiora0O0.sms = fiora0I0.sms;
316200140722           IF  pOutCell <> *blanks;
316300140722             fiora0O0.sms = pOutCell;
316400140722           ENDIF;
316500140311         ENDIF;
316600140311
316700140311       ENDSR;
316800140313
316900140313       //--------------------------------------------------------------
317000140313       //?Controllo Codice Destinatario.
317100140313       //--------------------------------------------------------------
317200140313       BEGSR  CodDestinatario;
317300140401
317400140401         wCodice = fiora0I0.vaocrc;
317500140402         reset FIORB0OC;
317600140313
317700140313       //?Se presente codice destinatario verifico se valido
317800160607       //?e se legato al codice di LOG solo se da Internet
317900140402         IF  not IsCodiceValido(FIORB0OC);
318000140313           wIdMsg   = 'TIS0066';
318100140313           wIdCampo = 'VAOCRC';
318200140313           clear wParm;
318300140313           exsr Messaggi;
318400140313           leavesr;
318500140313         ENDIF;
318600160607
318700160607       //?Solo per richiamo da AS400
318800160607       //?e solo dalla versione 'B' in poi
318900160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
319000160607             fiorb0OC.versione > 'A';
319100160607         //?codice bloccato
319200160714           IF  fiorb0OC.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
319300160607             wIdMsg   = 'TIS0800';
319400160607             wIdCampo = 'VAOCRC';
319500160607             clear wParm;
319600160607             exsr Messaggi;
319700160607           ENDIF;
319800160607         ENDIF;
319900140313
320000160607       //?Se richiamato da Internet
320100160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
320200140313       //?controllo se fa parte della stessa famiglia del codice di LOG
320300140313         IF not IsCodiceLegato();
320400140313           wIdMsg   = 'TIS0066';
320500140313           wIdCampo = 'VAOCRC';
320600140313           clear wParm;
320700140313           exsr Messaggi;
320800140313           leavesr;
320900140313         ENDIF;
321000160607         ENDIF;
321100160607
321200140313
321300140313       ENDSR;
321400131205
321500131205       //--------------------------------------------------------------
321600131205       //?Controllo Destinatario.
321700131205       //--------------------------------------------------------------
321800131205       BEGSR  Destinatario;
321900131220
322000140211         exec sql
322100140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
322200131205
322300131205       //?Se richiesto UN solo destinatario il dato è obbligatorio
322400160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
322500131205            (fiora0I0.vaorsc = *blanks or fiora0I0.vaoinc = *blanks or
322600131205             fiora0i0.vaoloc = *blanks or fiora0i0.vaocac = *blanks);
322700131205           wIdMsg   = 'TIS0288';
322800131205           wIdCampo = 'VAORSC';
322900131205           clear wParm;
323000131205           exsr Messaggi;
323100131220           leavesr;
323200131205         ENDIF;
323300131205
323400131205       //?Se richiesto più destinatari il dato NON è da inserire
323500160714         IF  fiora0I0.unopiudest = FIORA00_PIUDESTINATARI and
323600131205            (fiora0I0.vaorsc <> *blanks or fiora0I0.vaoinc <> *blanks or
323700131205             fiora0i0.vaoloc <> *blanks or fiora0i0.vaocac <> *blanks);
323800131205           wIdMsg   = 'TIS0380';
323900131205           wIdCampo = 'VAORSC';
324000131205           clear wParm;
324100131205           exsr Messaggi;
324200131220           leavesr;
324300131205         ENDIF;
324400131220
324500131220       //?Controllo nominativo destinatario
324600131220         exsr NomeDestinatario;
324700131220
324800131220       //?Controllo indirizzo destinatario
324900131220         exsr ViaDestinatario;
325000131220
325100131220       //?Controllo località destinatario
325200131220         exsr LocDestinatario;
325300131220
325400131220       //?Controllo cap destinatario
325500131220         exsr CapDestinatario;
325600131220
325700131220       //?Controllo provincia destinatario
325800131220         exsr ProvDestinatario;
325900131220
326000131220       //?Controllo nazione destinatario
326100131220         exsr NazDestinatario;
326200131220
326300131220       //?Controllo indirizzo completo del destinatario
326400131220         exsr IndTotDestinatario;
326500131205
326600131205       ENDSR;
326700131205
326800131205       //--------------------------------------------------------------
326900131205       //?Controllo nominativo destinatario.
327000131205       //--------------------------------------------------------------
327100131205       BEGSR  NomeDestinatario;
327200131205
327300131205       //?Se richiesto UN solo destinatario deve essere indicato
327400160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
327500131205             fiora0I0.vaorsc = *blanks;
327600131205           wIdMsg   = 'TIS0480';
327700131205           wIdCampo = 'VAORSC';
327800131205           clear wParm;
327900131205           exsr Messaggi;
328000131205         ENDIF;
328100131205
328200140211         exec sql
328300140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
328400131205
328500131205       ENDSR;
328600131205
328700131205       //--------------------------------------------------------------
328800131205       //?Controllo indirizzo destinatario.
328900131205       //--------------------------------------------------------------
329000131205       BEGSR  ViaDestinatario;
329100131205
329200131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
329300131220       //?deve essere indicata anche la via
329400160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
329500160714              fiora0I0.vaorsc <> *blanks) and
329600131220              fiora0I0.vaoinc = *blanks;
329700131205           wIdMsg   = 'TIS0281';
329800131205           wIdCampo = 'VAOINC';
329900131205           clear wParm;
330000131205           exsr Messaggi;
330100131205         ENDIF;
330200131205
330300140211         exec sql
330400140211         set :fiora0O0.vaoinc = ucase (:fiora0I0.vaoinc);
330500131205
330600131205       ENDSR;
330700131205
330800131205       //--------------------------------------------------------------
330900131205       //?Controllo cap destinatario.
331000131205       //--------------------------------------------------------------
331100131205       BEGSR  CapDestinatario;
331200131205
331300131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
331400131220       //?deve essere indicato
331500160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
331600160714              fiora0I0.vaorsc <> *blanks) and
331700131220              fiora0I0.vaocac = *blanks;
331800131205           wIdMsg   = 'TIS0051';
331900131205           wIdCampo = 'VAOCAC';
332000131205           clear wParm;
332100131205           exsr Messaggi;
332200131205         ENDIF;
332300131205
332400140211         exec sql
332500140211         set :fiora0O0.vaocac = ucase (:fiora0I0.vaocac);
332600131205
332700131205       ENDSR;
332800131205
332900131205       //--------------------------------------------------------------
333000131205       //?Controllo località destinatario.
333100131205       //--------------------------------------------------------------
333200131205       BEGSR  LocDestinatario;
333300131205
333400131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
333500131220       //?deve essere indicato
333600160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
333700160714              fiora0I0.vaorsc <> *blanks) and
333800131220              fiora0I0.vaoloc = *blanks;
333900131205           wIdMsg   = 'TIS0345';
334000131205           wIdCampo = 'VAOLOC';
334100131205           clear wParm;
334200131205           exsr Messaggi;
334300131205         ENDIF;
334400131205
334500140211         exec sql
334600140211         set :fiora0O0.vaoloc = ucase (:fiora0I0.vaoloc);
334700131205
334800131205       ENDSR;
334900131009
335000131205       //--------------------------------------------------------------
335100131205       //?Controllo provincia destinatario.
335200131205       //--------------------------------------------------------------
335300131205       BEGSR  ProvDestinatario;
335400131205
335500131220       //?Controllo se richiesto UN solo destinatario
335600131220       //?o se immessa ragione sociale
335700160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
335800160714             fiora0I0.vaorsc <> *blanks;
335900131205
336000140211           exec sql
336100140211           set :fiora0O0.vaoprc = ucase (:fiora0I0.vaoprc);
336200131205
336300131205         //?Controllo con la sk delle provincie
336400131205           IF  fiora0O0.vaoprc <> *blanks and
336500131205              %lookup(fiora0O0.vaoprc:skprv) = 0;
336600131205             wIdMsg   = 'TIS0461';
336700131205             wIdCampo = 'VAOPRC';
336800131205             clear wParm;
336900131205             exsr Messaggi;
337000131205           ENDIF;
337100131205
337200131205         ENDIF;
337300131205
337400131205       ENDSR;
337500131205
337600131205       //--------------------------------------------------------------
337700131205       //?Controllo nazione destinatario.
337800131205       //--------------------------------------------------------------
337900131205       BEGSR  NazDestinatario;
338000131205
338100131205       //?Controllo se richiesto UN solo destinatario
338200131220       //?o immessa ragione sociale
338300160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
338400160714             fiora0I0.vaorsc <> *blanks;
338500131205
338600140211           exec sql
338700140211           set :fiora0O0.vaonac = ucase (:fiora0I0.vaonac);
338800131205
338900131205         //?Controllo con la sk delle provincie
339000131205           IF  fiora0O0.vaonac <> *blanks and
339100131205               %lookup(fiora0O0.vaonac:sknaz) = 0;
339200140326             wIdMsg   = 'TIS0384';
339300131205             wIdCampo = 'VAONAC';
339400131205             clear wParm;
339500131205             exsr Messaggi;
339600131205           ENDIF;
339700131205
339800131205         ENDIF;
339900131205
340000131205       ENDSR;
340100131205
340200131205       //--------------------------------------------------------------
340300131205       //?Controllo indirizzo completo del destinatario.
340400131205       //--------------------------------------------------------------
340500131205       BEGSR  IndTotDestinatario;
340600131205
340700131205       //?Controllo se richiesto UN solo destinatario
340800131220       //?o immessa ragione sociale
340900160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
341000160714             fiora0I0.vaorsc <> *blanks;
341100131205
341200131205           clear TISI95DS;
341300151009           clear FNLV13DS;
341400131205           tisi95ds.I95tcn = '7';
341500131205           tisi95ds.I95nar = fiora0O0.vaonac;
341600131205           tisi95ds.I95cap = fiora0O0.vaocac;
341700131205           tisi95ds.I95loc = fiora0O0.vaoloc;
341800131205           tisi95ds.I95prv = fiora0O0.vaoprc;
341900131205           tisi95ds.I95ind = fiora0O0.vaoinc;
342000131205           tisi95ds.I95dat = %dec(%date());
342100151009           fnlv13ds.I13af0 = 'S';
342200151009           fnlv13ds.I13cnv = 'S';
342300151009           fnlv13ds.I13la3 = 'S';
342400131205
342500131205         //?Solo per richiamo da Internet
342600131205           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
342700131205           //?se richiesta nazione irlanda forzo cap eire
342800131205             IF  tisi95ds.I95nar = FIORA00_IRLANDA;
342900131205               tisi95ds.I95cap =  FIORA00_EIRE;
343000131205             ENDIF;
343100131205           ENDIF;
343200131205
343300151009         //?controllo cap/località/provincia
343400151009           fnlv13r (kpjba:fnlv13ds:tisi95ds);
343500160607         //?Solo per richiamo da Internet
343600160607         //?controllo gli errori, da AS ci pensa già il programma chiamante
343700160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
343800151016         //?se torna errore per cap generico diversifico il messaggio
343900151016           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
344000151016               %scan('generico':fnlv13ds.O13msg) > 0;
344100151016             wIdMsg   = 'TIS0826';
344200151016             wIdCampo = 'VAOCAC';
344300151016             clear wParm;
344400151016             exsr Messaggi;
344500151016             wErrMitt = *on;
344600151016             leavesr;
344700151016           ENDIF;
344800151009         //?se torna errore imposto messaggio generico
344900151009           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
345000131205             wIdMsg   = 'TIS0054';
345100131227             wIdCampo = 'VAOLOC';
345200131205             clear wParm;
345300131205             exsr Messaggi;
345400131205             leavesr;
345500131205           ENDIF;
345600160607           ENDIF;
345700160607
345800160607         //?Se richiamato da AS400
345900160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
346000160607         //?se oltre al destinatario c'è anche la filiale consegna impostata devo controllarla
346100160607         //?questo serve ai fini della bollettazione e non ai fini dell'ORM
346200160607             IF  fiora0I0.vaopoc <> *zeros and fiora0I0.vaorsc <> *blanks;
346300160607               exsr FilConsegna;
346400160607             ENDIF;
346500160607         //?se ORM import DPD devo controllare che la nazione di destino sia servita da DPD
346600160607             IF  wNetworkEmi = FIORA00_NTW_DPD and
346700160607                 fiora0I0.vaonac <> *blanks and
346800160607                 fnlv14ds.O14lad <= 0;
346900160607               wIdMsg   = 'TIS0841';
347000160607               wIdCampo = 'VAONAC';
347100160607               clear wParm;
347200160607               exsr Messaggi;
347300160607             ENDIF;
347400160607           ENDIF;
347500131227
347600131227         //?Se ORM export EEX non accetto destinatario esteri
347700131227           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
347800131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX and
347900131227               fiora0O0.vaonac <> *blanks;
348000140305             wIdMsg   = 'TIS0723';
348100131227             wIdCampo = 'VAONAC';
348200140305             clear wParm;
348300131227             exsr Messaggi;
348400131227           ENDIF;
348500131205
348600131205         //?Se ORM export DPD controllo la fattibilità della consegna
348700131205           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
348800131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
348900131205             clear fior95ds;
349000131205             fior95ds.IOR95cap = fiora0O0.vaocac;
349100131205             fior95ds.IOR95loc = fiora0O0.vaoloc;
349200131205             fior95ds.IOR95prv = fiora0O0.vaoprc;
349300131205             fior95ds.IOR95naz = fiora0O0.vaonac;
349400131230             fior95ds.IOR95lin = cvtLinguaISO2ToTntbe(fiora0I0.idlingua);
349500131205             fior95r (kpjba : fior95ds);
349600131205             IF  fior95ds.OOR95err <> *blanks or fior95ds.OOR95ok = 'N';
349700140304               wIdMsg   = 'TIS0718';
349800131205               wIdCampo = 'VAONAC';
349900131205               wParm = fior95ds.OOR95msg;
350000131205               exsr Messaggi;
350100131205             ENDIF;
350200131205           ENDIF;
350300160208
350400160208         //?Se ORM prepagato e consegna con LNA 340-341-345 blocco
350500160208         //?come da DV 1540
350600160208           IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
350700160209               fiora0I0.vaonac <> *blanks and fnlv13ds.O13err = *blanks and
350800160208               (tisi95ds.O95lna = 340 or tisi95ds.O95lna = 341 or
350900160208                tisi95ds.O95lna = 345);
351000160208             clear xx;
351100160208             xx = %lookup(fiora0I0.vaonac:sknaz);
351200160208             wIdMsg   = 'TIS0828';
351300160208             wIdCampo = 'VAOPAG';
351400160208             IF  xx > 0;
351500160208               wParm = sknazd(xx);
351600160208             ELSE;
351700160208               wParm = fiora0I0.vaoloc;
351800160208             ENDIF;
351900160208             exsr Messaggi;
352000160208           ENDIF;
352100160607         //?Se richiamato da AS400 controllo anche con la filiale consegna
352200160607         //?se inserita
352300160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
352400160607               fiora0O0.prepagato = FIORA00_PREPAGATO and
352500160607               fiora0I0.vaonac <> *blanks and
352600160607               (fiora0I0.vaopoc = 340 or fiora0I0.vaopoc = 341 or
352700160607                fiora0I0.vaopoc = 345);
352800160607             clear xx;
352900160607             xx = %lookup(fiora0I0.vaonac:sknaz);
353000160607             wIdMsg   = 'TIS0828';
353100160607             wIdCampo = 'VAOTOR';
353200160607             IF  xx > 0;
353300160607               wParm = sknazd(xx);
353400160607             ELSE;
353500160607               wParm = fiora0I0.vaoloc;
353600160607             ENDIF;
353700160607             exsr Messaggi;
353800160607           ENDIF;
353900160208
354000160208         //?Solo per richiamo da Internet
354100160208           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
354200160208           //?Errore
354300160208           //?Se ordinante non codificato (è utente senza PSW)
354400160208           //?Se consegna all'estero
354500160208           //?Se paga destinatario
354600160208             IF  fiora0I0.vaocor = 0 and
354700160208                 fiora0I0.vaonac <> *blanks and
354800160714                 fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
354900160208               wIdMsg   = 'TIS0448';
355000160208               wIdCampo = 'VAOPAG';
355100160208               clear wParm;
355200160208               exsr Messaggi;
355300160208             ENDIF;
355400160208           ENDIF;
355500131205
355600131205         ENDIF;
355700131205
355800131205       ENDSR;
355900160613
356000160613       //--------------------------------------------------------------
356100160613       //?Controllo quantità videata F05.
356200160613       //--------------------------------------------------------------
356300160613       BEGSR  Quantita2;
356400160613
356500160613       //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
356600160613       //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
356700160613         IF  fiorb0OR.flagfblc <> *blanks and fiora0I0.vaoblc = 0;
356800160613           wIdMsg   = 'TIS0844';
356900160613           wIdCampo = 'VAOBLC';
357000160613           clear wParm;
357100160714           IF  fiorb0OR.flagfblc = FIORA00_FORZABILE;
357200160613             exsr Forzatura;
357300160613             IF  wForza;
357400160613               exsr RoutEnd;
357500160613             ENDIF;
357600160613           ENDIF;
357700160714           IF  fiorb0OR.flagfblc = FIORA00_OBBLIGATORIO;
357800160613             exsr Messaggi;
357900160613           ENDIF;
358000160613         ENDIF;
358100160613         IF  fiorb0OR.flagfmtc <> *blanks and fiora0I0.vaomtc = 0;
358200160613           wIdMsg   = 'TIS0845';
358300160613           wIdCampo = 'VAOMTC';
358400160613           clear wParm;
358500160714           IF  fiorb0OR.flagfmtc = FIORA00_FORZABILE;
358600160613             exsr Forzatura;
358700160613             IF  wForza;
358800160613               exsr RoutEnd;
358900160613             ENDIF;
359000160613           ENDIF;
359100160714           IF  fiorb0OR.flagfmtc = FIORA00_OBBLIGATORIO;
359200160613             exsr Messaggi;
359300160613           ENDIF;
359400160613         ENDIF;
359500160613         IF  fiorb0OR.flagfatt <> *blanks and fiora0I0.vaoatt = 0;
359600160613           wIdMsg   = 'TIS0846';
359700160613           wIdCampo = 'VAOATT';
359800160613           clear wParm;
359900160714           IF  fiorb0OR.flagfatt = FIORA00_FORZABILE;
360000160613             exsr Forzatura;
360100160613             IF  wForza;
360200160613               exsr RoutEnd;
360300160613             ENDIF;
360400160613           ENDIF;
360500160714           IF  fiorb0OR.flagfatt = FIORA00_OBBLIGATORIO;
360600160613             exsr Messaggi;
360700160613           ENDIF;
360800160613         ENDIF;
360900160922
361000160922       //?Controllo limiti quantità
361100160922         clear TRUL731DS;
361200160922         trul731ds.I731blc = fiora0I0.vaoblc;
361300160922         trul731ds.I731att = fiora0I0.vaoatt;
361400160922         trul731ds.I731mtc = fiora0I0.vaomtc;
361500160922         trul731r (trul731ds);
361600160922
361700160922         SELECT;
361800160922       //?Limite bilico bloccante
361900160922         WHEN  trul731ds.O731fblcm = '1';
362000160922           wIdMsg   = 'TIS0854';
362100160922           wIdCampo = 'VAOBLC';
362200160922           wParm = %editc(trul731ds.O731lblcm:'4');
362300160922           exsr Messaggi;
362400160922       //?Limite bilico forzabile
362500160922         WHEN  trul731ds.O731fblcf = '1';
362600160922           wIdMsg   = 'TIS0854';
362700160922           wIdCampo = 'VAOBLC';
362800160922           wParm = %editc(trul731ds.O731lblcf:'4');
362900160922           exsr Forzatura;
363000160922       //?Limite autotreno bloccante
363100160922         WHEN  trul731ds.O731fattm = '1';
363200160922           wIdMsg   = 'TIS0855';
363300160922           wIdCampo = 'VAOATT';
363400160922           wParm = %editc(trul731ds.O731lattm:'4');
363500160922           exsr Messaggi;
363600160922       //?Limite autotreno forzabile
363700160922         WHEN  trul731ds.O731fattf = '1';
363800160922           wIdMsg   = 'TIS0855';
363900160922           wIdCampo = 'VAOATT';
364000160922           wParm = %editc(trul731ds.O731lattf:'4');
364100160922           exsr Forzatura;
364200160922       //?Limite motrice bloccante
364300160922         WHEN  trul731ds.O731fmtcm = '1';
364400160922           wIdMsg   = 'TIS0856';
364500160922           wIdCampo = 'VAOMTC';
364600160922           wParm = %editc(trul731ds.O731lmtcm:'4');
364700160922           exsr Messaggi;
364800160922       //?Limite motrice forzabile
364900160922         WHEN  trul731ds.O731fmtcf = '1';
365000160922           wIdMsg   = 'TIS0856';
365100160922           wIdCampo = 'VAOMTC';
365200160922           wParm = %editc(trul731ds.O731lmtcf:'4');
365300160922           exsr Forzatura;
365400160922         ENDSL;
365500160613
365600160613       ENDSR;
365700131205
365800131205       //--------------------------------------------------------------
365900131205       //?Controllo fermo deposito.
366000131205       //--------------------------------------------------------------
366100131205       BEGSR  FermoDeposito;
366200131205
366300160714         IF  fiora0I0.vaoffd <> FIORA00_SI and
366400160714             fiora0I0.vaoffd <> FIORA00_NO;
366500131205           wIdMsg   = 'TIS0537';
366600131205           wIdCampo = 'VAOFFD';
366700131205           clear wParm;
366800131205           exsr Messaggi;
366900131205         ENDIF;
367000131205
367100131205       ENDSR;
367200160315
367300160315       //--------------------------------------------------------------
367400160315       //?Controllo Filiale Consegna.
367500160315       //--------------------------------------------------------------
367600160315       BEGSR  FilConsegna;
367700160315
367800160315         wOKffc = *off;
367900160315         fiora0O0.vaopoc = fiora0I0.vaopoc;
368000160315
368100160315       //?Se il destinatario è estero
368200160315       //?forzo la filiale di consegna in base alla tabella FFC
368300160315         IF  fiora0O0.vaonac <> *blanks;
368400160315           clear dFFC;
368500160315       //?prima provo con la filiale dell'ordinante
368600160315           IF  fiora0O0.vaocor > 0;
368700160315             k_TBEcod = 'FFC';
368800160315             k_TBEke1 = %subst(%editc(fiora0O0.vaocor:'X'):1:3);
368900160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
369000160315             IF  %found(TNTBE01L);
369100160315               dFFC = TBEuni;
369200160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
369300160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
369400160315               ENDIF;
369500160315               wOKffc = *on;
369600160315             ENDIF;
369700160315           ENDIF;
369800160315       //?se non trovo con filiale ordinante proco con filiale emissione se diversa
369900160315           IF  not wOKffc and
370000160315               %subst(%editc(fiora0O0.vaocor:'X'):1:3) <>
370100160315               %editc(fiora0O0.vaopoe:'X');
370200160315             clear dFFC;
370300160315             k_TBEcod = 'FFC';
370400160315             k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
370500160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
370600160315             IF  %found(TNTBE01L);
370700160315               dFFC = TBEuni;
370800160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
370900160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
371000160315               ENDIF;
371100160315             ENDIF;
371200160315           ENDIF;
371300160315         ENDIF;
371400160315
371500160315       //?Se non impostata non controllo
371600160315         IF  fiora0O0.vaopoc = *zeros;
371700160315           leavesr;
371800160315         ENDIF;
371900160315
372000160315       //?Controllo la filiale consegna
372100160315         IF  not IsFilValida(fiora0I0.vaopoc);
372200160315           wIdMsg   = 'TIS0803';
372300160315           wIdCampo = 'VAOPOC';
372400160315           clear wParm;
372500160315           exsr Messaggi;
372600160315           leavesr;
372700160315         ENDIF;
372800160315
372900160315       //?Network e Decodifico la filiale consegna
373000160315         wNetworkCon = GetNtwFiliale(fiora0O0.vaopoc);
373100160315
373200160315       ENDSR;
373300160315
373400160315       //--------------------------------------------------------------
373500160315       //?Controllo Cliente tassazione.
373600160315       //--------------------------------------------------------------
373700160315       BEGSR  ClienteKsc;
373800160315
373900160315         wOKtariffa = *off;
374000160315
374100160315       //?Codice tassazione valido
374200160315         IF  fiora0I0.vaoksc > *zeros;
374300160315           IF  not IsKscValido(fiora0I0.vaoksc);
374400160315             wIdMsg   = 'TIS0810';
374500160315             wIdCampo = 'VAOKSC';
374600160315             clear wParm;
374700160315             exsr Messaggi;
374800160315             leavesr;
374900160315           ENDIF;
375000160315         ENDIF;
375100160315
375200160315       //?Codice tariffa valido
375300160315         IF  fiora0I0.vaoctr <> *blanks;
375400160315           IF  %check(FIORA00_DIGITS:fiora0I0.vaoctr) > *zeros;
375500160315             wIdMsg   = 'TIS0810';
375600160315             wIdCampo = 'VAOCTR';
375700160315             clear wParm;
375800160315             exsr Messaggi;
375900160315             leavesr;
376000160315           ENDIF;
376100160315         ENDIF;
376200160315
376300160315       //?Se codice tariffa deve esserci anche il codice cliente
376400160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc = *zeros;
376500160607           wIdMsg   = 'TIS0840';
376600160315           wIdCampo = 'VAOCTR';
376700160607           clear wParm;
376800160315           exsr Messaggi;
376900160315           leavesr;
377000160315         ENDIF;
377100160315
377200160315       //?La tariffa deve esistere
377300160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc > *zeros;
377400160315
377500160315           k_TAMksc = fiora0I0.vaoksc;
377600160315           k_TAMctr = %int(fiora0I0.vaoctr);
377700160315           setll (k_TAMksc:k_TAMctr) TNTAM01L;
377800160315           IF  not %equal(TNTAM01L);
377900160315             wIdMsg   = 'TIS0810';
378000160315             wIdCampo = 'VAOKSC';
378100160315             clear wParm;
378200160315             exsr Messaggi;
378300160315             leavesr;
378400160315           ENDIF;
378500160315
378600160315           DOW  not wOKtariffa;
378700160315             reade (k_TAMksc:k_TAMctr) TNTAM01L;
378800160315             IF  %eof(TNTAM01L);
378900160315               leave;
379000160315             ENDIF;
379100160315             IF  TAMatb = *blanks;
379200160315               wOKtariffa = *on;
379300160315             ENDIF;
379400160315           ENDDO;
379500160315           IF  not wOKtariffa;
379600160315             wIdMsg   = 'TIS0810';
379700160315             wIdCampo = 'VAOKSC';
379800160315             clear wParm;
379900160315             exsr Messaggi;
380000160315             leavesr;
380100160315           ENDIF;
380200160315
380300160315         ENDIF;
380400160315
380500160315         fiora0O0.vaoksc = fiora0I0.vaoksc;
380600160315         fiora0O0.vaoctr = fiora0O0.vaoctr;
380700160315
380800160315       ENDSR;
380900140506
381000140506       //--------------------------------------------------------------
381100140506       //?Controllo Alert.
381200140506       //--------------------------------------------------------------
381300140506       BEGSR  Alert;
381400140506
381500140506         IF  fiora0I0.alertmail <> *blanks and
381600160714             fiora0I0.alertmail <> FIORA00_NO and
381700160714             fiora0I0.alertmail <> FIORA00_X;
381800140506           clear fiora0O0.alertmail;
381900140506         ENDIF;
382000140506
382100140506         IF  fiora0I0.alertsms <> *blanks and
382200160714             fiora0I0.alertsms <> FIORA00_NO and
382300160714             fiora0I0.alertsms <> FIORA00_X;
382400140506           clear fiora0O0.alertsms;
382500140506         ENDIF;
382600140506
382700140506       ENDSR;
382800151008
382900151008       //--------------------------------------------------------------
383000151008       //?Controllo indirizzo mail x accettazione ORM.
383100151008       //--------------------------------------------------------------
383200151008       BEGSR  MailConferma;
383300160503
383400160714         fiora0O0.alertconf = FIORA00_NO;
383500151008
383600160318       //?Versione inferiore alla 'C'
383700160714         IF  fiora0I0.mailconf = *blanks and
383800160714             fiora0I0.alertconf = FIORA00_SI and
383900160318             fiora0O0.versione < 'C';
384000151008           wIdMsg   = 'TIS0732';
384100151008           wIdCampo = 'MAILCONF';
384200151008           clear wParm;
384300151008           exsr Messaggi;
384400151008           leavesr;
384500151008         ENDIF;
384600160318       //?dalla Versione 'C'
384700160714         IF  fiora0I0.mailconf = *blanks and
384800160714             fiora0I0.alertconf = FIORA00_SI and
384900160318             fiora0O0.versione > 'B' and fiora0I0.smsconf = *blanks;
385000160318           wIdMsg   = 'TIS0831';
385100160318           wIdCampo = 'MAILCONF';
385200160318           clear wParm;
385300160318           exsr Messaggi;
385400160318           leavesr;
385500160318         ENDIF;
385600151008
385700151008         IF  fiora0I0.mailconf <> *blanks;
385800151008           clear dsemail;
385900151008           dsemail.§emlindi = fiora0I0.mailconf;
386000151008           xemail (dsemail);
386100151008           IF  dsemail.§emlerro <> '0';
386200151008             wIdMsg   = 'TIS0732';
386300151008             wIdCampo = 'MAILCONF';
386400151008             clear wParm;
386500151008             exsr Messaggi;
386600151008             leavesr;
386700151008           ENDIF;
386800151008
386900151008           fiora0O0.mailconf = fiora0I0.mailconf;
387000151008           IF  dsemail.§emlindo <> *blanks;
387100151008             fiora0O0.mailconf = dsemail.§emlindo;
387200151008           ENDIF;
387300151008         ENDIF;
387400151008
387500151008       //?Passo il dato solo se impostato a 'SI'
387600160714         IF  fiora0I0.alertconf = FIORA00_SI;
387700151008           fiora0O0.alertconf = fiora0I0.alertconf;
387800160322         //?Dalla versione 'C' se non c'è la mail non lo imposto
387900160322           IF  fiora0O0.versione > 'B' and fiora0O0.mailconf = *blanks;
388000160714             fiora0O0.alertconf = FIORA00_NO;
388100160322           ENDIF;
388200151008         ENDIF;
388300151008
388400151008       ENDSR;
388500151008
388600151008       //--------------------------------------------------------------
388700151008       //?Calcolo orario indicativo ritiro.
388800151008       //--------------------------------------------------------------
388900151008       BEGSR  OrarioIndRitiro;
389000151008
389100151008       //?imposto orario ritiro standard
389200151012         fiora0O0.orariti = fiora0O0.orainizio;
389300151012         fiora0O0.oraritf = fiora0O0.orafine;
389400151126
389500151126       //?Se l'ora pronta merce è maggiore
389600151126       //?dell'ora inizio servizio
389700151126       //?e minore dell'ora fine
389800151126       //?la imposto come orario inzio
389900151126         IF  fiora0O0.vaoorr > fiora0O0.orainizio and
390000151126             fiora0O0.vaoorr < fiora0O0.orafine;
390100151126           fiora0O0.orariti = fiora0O0.vaoorr;
390200151126         ENDIF;
390300151126
390400151126       //?Se l'ora pronta merce è maggiore
390500151126       //?dell'ora fine servizio
390600151126       //?la imposto come orario fine
390700151126         IF  fiora0O0.vaoorr > fiora0O0.orafine;
390800151126           fiora0O0.oraritf = fiora0O0.vaoorr;
390900151126         ENDIF;
391000151008
391100151008       ENDSR;
391200160318
391300160318       //--------------------------------------------------------------
391400160318       //?Controllo SMS x accettazione ORM.
391500160318       //--------------------------------------------------------------
391600160318       BEGSR  SmsConferma;
391700160318
391800160714         fiora0O0.alertconfs = FIORA00_NO;
391900160318
392000160620       //?Se richiamato da Internet
392100160620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
392200160714         IF  fiora0I0.smsconf = *blanks and
392300160714             fiora0I0.alertconf = FIORA00_SI and
392400160318             fiora0I0.mailconf = *blanks;
392500160318           wIdMsg   = 'TIS0831';
392600160318           wIdCampo = 'SMSCONF';
392700160318           clear wParm;
392800160318           exsr Messaggi;
392900160318           leavesr;
393000160318         ENDIF;
393100160318
393200160714         IF  fiora0I0.smsconf <> *blanks and
393300160714             fiora0I0.alertconf = FIORA00_SI;
393400160318           pInCell = %trim(fiora0I0.smsconf);
393500160318           clear pOutErr;
393600160318           clear pOutCell;
393700160318           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
393800160318             wIdMsg   = 'TIS0725';
393900160318             wIdCampo = 'SMSCONF';
394000160318             clear wParm;
394100160318             exsr Messaggi;
394200160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
394300160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
394400160601             fiora0I0.smsconf;
394500160318             leavesr;
394600160318           ENDIF;
394700160318
394800160318       //?Passo il dato solo se impostato a 'SI'
394900160714         //IF  fiora0I0.alertconf = FIORA00_SI;
395000160318           fiora0O0.smsconf = fiora0I0.smsconf;
395100160318           IF  pOutCell <> *blanks;
395200160318             fiora0O0.smsconf = pOutCell;
395300160318           ENDIF;
395400160318         //?Passo anche il nuovo flag
395500160318           IF  fiora0O0.smsconf <> *blanks;
395600160714             fiora0O0.alertconfs = FIORA00_SI;
395700160318           ENDIF;
395800160503         //ENDIF;
395900160502         ENDIF;
396000160620         ENDIF;
396100160620
396200160620       //?Se richiamato da AS400
396300160620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
396400160620           IF  fiora0I0.smsconf <> *blanks;
396500160620             pInCell = %trim(fiora0I0.smsconf);
396600160620             clear pOutErr;
396700160620             clear pOutCell;
396800160620             IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
396900160620               wIdMsg   = 'TIS0725';
397000160620               wIdCampo = 'SMSCONF';
397100160620               clear wParm;
397200160620               exsr Messaggi;
397300160620               fiora0M0.skTextMsg (fiora0M0.nrmsg) =
397400160620               %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
397500160620               fiora0I0.smsconf;
397600160620               leavesr;
397700160620             ENDIF;
397800160620             fiora0O0.smsconf = fiora0I0.smsconf;
397900160620             IF  pOutCell <> *blanks;
398000160620               fiora0O0.smsconf = pOutCell;
398100160620             ENDIF;
398200160620           ENDIF;
398300160620         ENDIF;
398400160318
398500160318       ENDSR;
398600160714
398700160714       //--------------------------------------------------------------
398800160714       //?Controllo Flag x memorizzazione dati su Anagrafica Ritiro.
398900160714       //--------------------------------------------------------------
399000160714       BEGSR  FlagMemDati;
399100160714
399200160714       //?Per richiamo da Internet
399300160714       //?e utente anonimo non controllo
399400160714         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
399500160714             fiora0I0.IdUtente = 0;
399600160714           leavesr;
399700160714         ENDIF;
399800160714
399900160714       //?Accetto solo S o N
400000160714         IF  fiora0I0.memdatiacr <> FIORA00_SI and
400100160728             fiora0I0.memdatiacr <> FIORA00_NO and
400200160728             fiora0I0.memdatiacr <> *blanks;
400300160714           wIdMsg   = 'TIS0848';
400400160714           wIdCampo = 'MEMDATIACR';
400500160714           clear wParm;
400600160714           exsr Messaggi;
400700160714         ENDIF;
400800160714
400900160714       ENDSR;
401000160315
401100160315       //--------------------------------------------------------------
401200160315       //?Cerco il giro di ritiro.
401300160315       //--------------------------------------------------------------
401400160315       BEGSR  GiroRitiro;
401500160315
401600160315         fiora0O0.cgi = fiora0I0.cgi;
401700160315
401800160412       //?Se giro impostato lo controllo
401900160315         IF  fiora0O0.cgi <> *blanks;
402000160315           clear FIDG09DS;
402100160315           fidg09ds.D09iop0 = '001';
402200160518           fidg09ds.D09ifgs = fiora0I0.VAOpor;
402300160315           fidg09ds.D09idat = dataoggi;
402400160315           fidg09ds.D09icgi = fiora0O0.cgi;
402500160315           fidg09ds.D09itug = 'R';
402600160315           %subst(kpjba:247:256) = FIDG09DS;
402700160315           fidg09r (kpjba);
402800160315           fidg09ds = %subst(kpjba:247:256);
402900160315           IF  fidg09ds.D09oerr <> *blanks;
403000160315             wIdMsg   = 'TIS0718';
403100160315             wIdCampo = 'CGI';
403200160315             wParm = fidg09ds.D09omsg;
403300160315             exsr Messaggi;
403400160315             leavesr;
403500160315           ENDIF;
403600160315         ENDIF;
403700160315
403800160315       ENDSR;
403900140312
404000131008       //--------------------------------------------------------------
404100131008       //?Routine per schierare i messaggi nella ds FIORA0M0.
404200131008       //--------------------------------------------------------------
404300131008       BEGSR  Messaggi;
404400131007
404500131008       //?Se ho già caricato 99 messaggi esco
404600131008         IF  fiora0M0.nrmsg = 99;
404700131010           exsr RoutEnd;
404800131008           clear fiora0M0.nrmsg;
404900131008         ENDIF;
405000131008
405100131008         fiora0M0.nrmsg += 1;
405200131009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
405300131009         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
405400140211         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
405500131204         IF  wParm <> *blanks;
405600131219           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
405700131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
405800131204         ELSE;
405900131219           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
406000131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
406100131204         ENDIF;
406200131008
406300131008       ENDSR;
406400131008
406500131008       //--------------------------------------------------------------
406600131008       //?Routine per schierare le forzature nella ds FIORA0F0.
406700131008       //--------------------------------------------------------------
406800131008       BEGSR  Forzatura;
406900160531
407000160531         wForza = *off;
407100160530
407200160530       //?Se già forzato non riemetto il messaggio
407300160531         wNrForza = %lookup(wIdMsg:fiora0F0.skIdMsg);
407400160531         IF  wNrForza > 0 and fiora0F0.skGiaForza(wNrForza) =
407500160530             FIORA00_GIAFORZA_SI;
407600160530           leavesr;
407700160530         ENDIF;
407800160531
407900160531       //?Se ho già caricato 99 mesaggi esco
408000160531         IF  fiora0M0.nrmsg = 99;
408100160531           exsr RoutEnd;
408200160531           clear fiora0M0.nrmsg;
408300160531         ENDIF;
408400160530
408500160531       //?Imposto messaggio da forzare
408600160530         fiora0M0.nrmsg += 1;
408700160530         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
408800160530         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
408900160530         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
409000160530         IF  wParm <> *blanks;
409100160530           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
409200160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
409300160530         ELSE;
409400160530           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
409500160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
409600160530         ENDIF;
409700160531
409800160531         wForza = *on;
409900131008
410000131008       ENDSR;
410100131009
410200131009       //--------------------------------------------------------------
410300131009       //?Imposta i dati nella ds di Output.
410400131009       //--------------------------------------------------------------
410500131009       BEGSR  DsOutput;
410600131008
410700131009       //?Se richiamo da Internet
410800131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
410900131223
411000131223         //?Imposto se commissionato
411100140407           IF  wCom;
411200160714             dorm01.§orcomc = FIORA00_SI;
411300131223           ELSE;
411400160714             dorm01.§orcomc = FIORA00_NO;
411500131223           ENDIF;
411600140506
411700140507         //?Se ORM commissionato e data ritiro passata al pgm = 0
411800140507         //?vuol dire che non è ancora stata calcolata quindi siamo
411900140507         //?nel passaggio tra la prima e la seconda pagina
412000140507           IF  wCom and fiora0I0.vaodar = 0;
412100140507           //?se mail e/o sms impostati torno 'S' nei flag degli alert
412200140507             IF  fiora0O0.mail <> *blanks;
412300160714               fiora0O0.alertmail = FIORA00_SI;
412400140507             ENDIF;
412500140507             IF  fiora0O0.sms <> *blanks;
412600160714               fiora0O0.alertsms = FIORA00_SI;
412700140507             ENDIF;
412800140507           ENDIF;
412900140507
413000140507         //?Se ORM commissionato e data ritiro (calcolata o impostata dall'utente) = oggi
413100140506         //?imposto i flag alert a 'N'
413200140508         //?ma se data ritiro passata è > di 0, in questo caso vuol dire che siamo già
413300140508         //?in seconda pagina e l'utente ha messo oggi al posto di domani
413400140508           IF  wCom and fiora0O0.vaodar = dataoggi and fiora0I0.vaodar > 0;
413500140507             IF  fiora0O0.mail <> *blanks;
413600160714               fiora0O0.alertmail = FIORA00_NO;
413700140506             ENDIF;
413800140507             IF  fiora0O0.sms <> *blanks;
413900160714               fiora0O0.alertsms = FIORA00_NO;
414000140506             ENDIF;
414100140506           ENDIF;
414200131009
414300131009         ENDIF;
414400131223
414500131227         fiora0O0.idrichiamo = fiora0I0.idrichiamo;
414600131223         fiora0O0.idlingua = fiora0I0.idlingua;
414700140402
414800140402         IF  fiora0I0.vaorfa <> *blanks;
414900140402           exec sql
415000140402           set :fiora0O0.vaorfa = ucase (:fiora0I0.vaorfa);
415100140402         ENDIF;
415200131223
415300131223         IF  fiora0I0.vaono1 <> *blanks;
415400140211           exec sql
415500140211           set :fiora0O0.vaono1 = ucase (:fiora0I0.vaono1);
415600131223         ENDIF;
415700131223         IF  fiora0I0.vaono2 <> *blanks;
415800140211           exec sql
415900140211           set :fiora0O0.vaono2 = ucase (:fiora0I0.vaono2);
416000131223         ENDIF;
416100131223
416200131205         fiora0O0.vaoflo = dorm01;
416300131009
416400131009       ENDSR;
416500140408
416600140408       //--------------------------------------------------------------
416700140408       //?Richiamo pgm per calcolo orari servizio.
416800140408       //--------------------------------------------------------------
416900140408       BEGSR  CallTrulORS;
417000140408
417100140408         clear TRULORSds;
417200140408         trulorsds.IOREfil  = wPor;
417300140408         trulorsds.IORecap  = wCap;
417400140408         trulorsds.IOREloc  = wLocalita;
417500140408         trulorsds.IOREdta  = wData;
417600140408         trulorsds.IOREtser = 'R';
417700140408         trulorsr (kpjba:trulorsds:trulor2ds);
417800140408
417900140408       ENDSR;
418000160526
418100160526       //--------------------------------------------------------------
418200160526       //?Richiamo pgm per calcolo orari servizio estero.
418300160526       //--------------------------------------------------------------
418400160526       BEGSR  CallFIORE;
418500160526
418600160526         clear FIORE00ds;
418700160526         fiore00ds.IORE00por = wPor;
418800160526         fiore00ds.IORE00nar = wNazione;
418900160526         fiore00r (fiore00ds);
419000160526
419100160526       ENDSR;
419200140408
419300140408       //--------------------------------------------------------------
419400140408       //?Imposto la frase dal tornare al chiamante.                   ?
419500140408       //--------------------------------------------------------------
419600140408       BEGSR  GetFrase1;
419700140408
419800140408       //?Se ID lingua non valido o non passato
419900140408       //?assumo italiano
420000140408         IF  fiora0I1.idlingua = *blanks or
420100140408            (fiora0I1.idlingua <> FIORA00_ID_LINGUA_IT and
420200140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_FR and
420300140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_EN and
420400140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_DE);
420500140408            fiora0I1.idlingua = FIORA00_ID_LINGUA_IT;
420600140408         ENDIF;
420700140408
420800140408       //?Cerco gli orari di servizio
420900140408         wPor      = fiora0I1.filritiro;
421000140408         wData     = dataoggi;
421100140408         wCap      = fiora0I1.cap;
421200140408         wLocalita = fiora0I1.localita;
421300140408         exsr CallTrulORS;
421400160526
421500160526       //?Dalla versione "C" cerco la frase anche per i ritiri esteri
421600160526         SELECT;
421700160531         WHEN  fiora0I1.versione > 'B';
421800160531           wNetworkRit = GetNtwFiliale(fiora0I1.filritiro);
421900160531           clear dNTW;
422000160531           k_TBEcod = 'NTW';
422100160531           k_TBEke1 = wNetworkRit;
422200160531           chain (k_TBEcod:k_TBEke1) TNTBE01L;
422300160531           IF  %found(TNTBE01L);
422400160531             dNTW = TBEuni;
422500160531           ENDIF;
422600160531           IF  wNetworkRit = FIORA00_NTW_DPD or
422700160531               dntw.§NTWfie = FIORA00_NTW_ESTERO;
422800160526             wPor     = fiora0I1.filritiro;
422900160526             wNazione = fiora0I1.nazione;
423000160526             exsr CallFIORE;
423100160526           ENDIF;
423200160526         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
423300160526         //?non devo emettere la frase dato che la merce parte sempre il
423400160526         //?giorno dopo e la data ritiro è sempre il giorno dopo
423500160526           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
423600160526               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
423700160526             clear fiora0O1.frase;
423800160526             fiora0O1.idlingua = fiora0I1.idlingua;
423900160526             leavesr;
424000160526           ENDIF;
424100160526         //?Se non è un prepagato imposto la frase
424200160526           wIdMsg   = 'TIS0827';
424300160526           SELECT;
424400160526           //?Ritiri esteri
424500160526           WHEN  fiora0I1.nazione <> *blanks;
424600160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
424700160526           //?Ritiri commissionati
424800160714           WHEN  fiora0I1.contattot = FIORA00_SI;
424900160526             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
425000160526           OTHER;
425100160526           //?Ritiri normali
425200160526             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
425300160526           ENDSL;
425400160526         OTHER;
425500140408
425600140408       //?Imposto la frase
425700151104       //?Frasi diverse in base alla versione
425800151104         IF  fiora0I1.versione > 'A';
425900151104         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
426000151104         //?non devo emettere la frase dato che la merce parte sempre il
426100151104         //?giorno dopo e la data ritiro è sempre il giorno dopo
426200151104           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
426300151104               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
426400151104             clear fiora0O1.frase;
426500151104             fiora0O1.idlingua = fiora0I1.idlingua;
426600151104             leavesr;
426700151104           ENDIF;
426800151104         //?Se cliente codificato imposto la nuova frase
426900151104           wIdMsg   = 'TIS0827';
427000160714           IF  fiora0I1.contattot = FIORA00_SI;
427100151104             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
427200151104           ELSE;
427300151104             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
427400151104           ENDIF;
427500151104         ELSE;
427600140408         wIdMsg   = 'TIS0731';
427700160714         IF  fiora0I1.contattot = FIORA00_SI;
427800140408           wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
427900140408         ELSE;
428000140408           wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
428100140408         ENDIF;
428200151104         ENDIF;
428300160526
428400160526         ENDSL;
428500140408
428600160531       //?Per ora ND ha deciso di non impostare la frase per i ritiri esteri
428700160531         IF  fiora0I1.nazione <> *blanks and SaltaFraseExport <> *blanks;
428800160531           clear fiora0O1.frase;
428900160531           leavesr;
429000160531         ENDIF;
429100160531
429200140408         fiora0O1.frase =
429300140408         rtvMsgLang(wIdMsg:fiora0I1.idlingua:wParm);
429400140408
429500140408         fiora0O1.idlingua = fiora0I1.idlingua;
429600140408
429700140408       ENDSR;
429800131010
429900131010       //--------------------------------------------------------------
430000131010       //?Operazioni finali.
430100131010       //--------------------------------------------------------------
430200131010       BEGSR  RoutEnd;
430300140408
430400140408       //?Se richiamato per GET_FRASE1
430500140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
430600140408           %subst(prmRpyData:1:prmRpyDataSize) = fiora0O1;
430700140408         ELSE;
430800131010
430900131219         %subst(prmRpyData:1:prmRpyDataSize) = fiora0O0;
431000131219         %subst(prmRpyMessage:1:prmRpyMsgSize) = fiora0M0;
431100131219         %subst(prmRpyForzatu:1:prmRpyForSize) = fiora0F0;
431200140408
431300140408         ENDIF;
431400131010
431500131010         return;
431600131010
431700131010       ENDSR;
431800131009
431900131007      /end-free
432000140312
432100140313      *--------------------------------------------------
432200140313      * Procedure name: IsCodiceLegato
432300140313      * Purpose:        Controlla codice
432400140402      * Returns:        ON - OFF
432500140313      *--------------------------------------------------
432600140313     p IsCodiceLegato  B
432700140313     d IsCodiceLegato  PI              n
432800140312
432900140313      /copy gaitrasrc/srcconst,FIORB00R
433000140313     d isErrore        s               n   inz
433100140313     d RpyIdMsg        s             10i 0
433200140313     d RpyOpCode       s             10i 0
433300140313     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_LEGGI)
433400140313     d wNrRec          s              7s 0 inz
433500140313     d fiorb0I0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
433600140313     d fiorb0O0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
433700140313     d  skCodice                     10a   dim(10) overlay(Codice)
433800140313      /copy gaitrasrc/srcprotopr,FIORB00R
433900140312
434000140313      /free
434100140312
434200140313       reset isErrore;
434300140313       reset RpyIdMsg;
434400140313       reset RpyOpCode;
434500140313       reset fiorb0I0;
434600140313       reset fiorb0O0;
434700140312
434800140313       fiorb0I0.codiceksu = *all'0';
434900140313       %subst(fiorb0I0.codiceKsu:2:7) =
435000140312       %subst(%editc(wCodice:'X'):1:7);
435100140312       //?Solo per richiamo da Internet
435200140313       IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
435300140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
435400140313       ELSE;
435500140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
435600140313       ENDIF;
435700140312
435800160519       DOU  wNrRec = fiorb0O0.clientitot
435900140313         OR fiorb0O0.nrrec = *ZERO;
436000140313
436100140313         MONITOR;
436200140313           Fiorb00_Anagrafica( RqsOpCode
436300140313                             : RpyOpCode
436400140313                             : RpyIdMsg
436500140313                             : fiorb0I0.formato
436600140313                             : fiorb0I0
436700140313                             : %SIZE(fiorb0I0)
436800140313                             : fiorb0O0.formato
436900140313                             : fiorb0O0
437000140313                             : %SIZE(fiorb0O0)
437100140313                             );
437200140313         ON-ERROR;
437300140313           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
437400140313         ENDMON;
437500140312
437600160519         IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
437700140313           isErrore = *on;
437800140313           leave;
437900140313         ENDIF;
438000140313
438100140313         //?Controllo se il codice fa parte della stessa famiglia
438200140313         //?se fa parte esco dal LOOP
438300140313         IF  %lookup(%editc(wCodice:'X'):fiorb0O0.skCodice) > 0;
438400140313           leave;
438500140313         ENDIF;
438600140313
438700140313         wNrRec += 1;
438800140313         RqsOpCode = FIORB00_RQSOPCODE_CARICA;
438900140312
439000140313       ENDDO;
439100140312
439200140313       Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
439300140313                           : RpyOpCode
439400140313                           : RpyIdMsg
439500140312                           );
439600140313
439700140313       IF  isErrore;
439800140313         return *off;
439900140313       ENDIF;
440000140312
440100140313       RETURN *on;
440200140312
440300140312      /END-FREE
440400140313     p IsCodiceLegato  E
440500140312
440600140401      *--------------------------------------------------
440700140401      * Procedure name: IsCodiceValido
440800140401      * Purpose:        Controlla codice
440900140402      * Returns:        ON - OFF
441000140401      *--------------------------------------------------
441100140401     p IsCodiceValido  B
441200140401     d IsCodiceValido  PI              n
441300140402     d  FIORB0O1                           likeds(FIORB0OO)
441400140401
441500140401      /copy gaitrasrc/srcconst,FIORB00R
441600140401     d isErrore        s               n   inz
441700140401     d RpyIdMsg        s             10i 0
441800140401     d RpyOpCode       s             10i 0
441900140401     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
442000140401     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
442100140401      /copy gaitrasrc/srcprotopr,FIORB00R
442200140401
442300140401      /free
442400140401
442500140401       reset isErrore;
442600140401       reset RpyIdMsg;
442700140401       reset RpyOpCode;
442800140401       reset fiorb0I1;
442900140401
443000140401       fiorb0I1.codice = wCodice;
443100140401       MONITOR;
443200140401         Fiorb00_Anagrafica( RqsOpCode
443300140401                           : RpyOpCode
443400140401                           : RpyIdMsg
443500140401                           : fiorb0I1.formato
443600140401                           : fiorb0I1
443700140401                           : %SIZE(fiorb0I1)
443800140401                           : fiorb0O1.formato
443900140401                           : fiorb0O1
444000140401                           : %SIZE(fiorb0O1)
444100140401                           );
444200140401         ON-ERROR;
444300140401           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
444400140401         ENDMON;
444500140401
444600160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
444700140401         isErrore = *on;
444800140401       ENDIF;
444900160519
445000160519       IF  RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND and
445100160519           fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
445200160519         isErrore = *on;
445300160519       ENDIF;
445400140401
445500140401       IF  isErrore;
445600140401         return *off;
445700140402       ENDIF;
445800140402
445900140402       RETURN *on;
446000140401
446100140401      /END-FREE
446200140401     p IsCodiceValido  E
446300140411
446400140411     P*--------------------------------------------------
446500140411     P* Procedure name: GetOraAggiustata
446600140411     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
446700140411     P* Returns:        Ora e minuto aggiustati.
446800140411     P* Parameter:      hhmm => Ora e minuto
446900140411     P*--------------------------------------------------
447000140411     P GetOraAggiustata...
447100140411     P                 B
447200140411     D GetOraAggiustata...
447300140411     D                 PI             4A
447400140411     D  hhmm                          4S 0 CONST
447500140411     D retField        DS             4    QUALIFIED STATIC
447600140411     D  hhmm                   1      4S 0
447700140411     D  hh                     1      2S 0
447800140411     D  mm                     3      4S 0
447900140411
448000140411      /FREE
448100140411
448200140411       IF hhmm = *ZERO;
448300140411         RETURN *BLANK;
448400140411       ENDIF;
448500140411
448600140411       retField.hhmm = hhmm;
448700140411
448800140411       SELECT;
448900140411         WHEN retField.mm = 00 OR retField.mm = 15
449000140411           OR retField.mm = 30 OR retField.mm = 45;
449100140411           // Non è da aggiustare.
449200140411         WHEN retField.mm < 15;
449300140411           retField.mm = 15;
449400140411         WHEN retField.mm < 30;
449500140411           retField.mm = 30;
449600140411         WHEN retField.mm < 45;
449700140411           retField.mm = 45;
449800140411         OTHER;
449900140411           retField.hh += 1;
450000140411           retField.mm = 00;
450100140411       ENDSL;
450200140411
450300140411       RETURN retField;
450400140411
450500140411      /end-free
450600140411     P GetOraAggiustata...
450700140411     P                 E
450800160315
450900160315      *--------------------------------------------------
451000160315      * Procedure name: IsFilValida
451100160315      * Purpose:        Ritorna se filiale è valida
451200160315      * Returns:        On - OFF
451300160315      * Parameter:      Filiale
451400160315      *--------------------------------------------------
451500160315     p IsFilValida     B
451600160315     d IsFilValida     PI              n
451700160315     D  filiale                       3s 0 CONST
451800160315
451900160315      /free
452000160315
452100160315       IF  filiale = *zeros;
452200160315         RETURN *off;
452300160315       ENDIF;
452400160315
452500160315       chain (filiale) AZORG01L;
452600160315       IF  not %found(AZORG01L);
452700160315         RETURN *off;
452800160315       ENDIF;
452900160315       IF  ORGfva <> *blanks;
453000160315         RETURN *off;
453100160315       ENDIF;
453200160315       IF  ORGfag <> 'F' and ORGfag <> 'A';
453300160315         RETURN *off;
453400160315       ENDIF;
453500160315
453600160315       RETURN *on;
453700160315
453800160315      /end-free
453900160315     P IsFilValida     E
454000160315
454100160315      *--------------------------------------------------
454200160315      * Procedure name: GetNtwFiliale
454300160315      * Purpose:        Recupera il Network della filiale
454400160315      * Returns:        Network
454500160315      * Parameter:      Filiale
454600160315      *--------------------------------------------------
454700160315     p GetNtwFiliale   B
454800160315     d GetNtwFiliale   PI            20a
454900160315     D  filiale                       3s 0 CONST
455000160315
455100160315      // ds per campo ORGDE3 di AZORG
455200160315     d OG143         e ds
455300160315
455400160315      /free
455500160315       IF  filiale = *zeros;
455600160315         RETURN *blanks;
455700160315       ENDIF;
455800160315
455900160315       clear OG143;
456000160315       chain (filiale) AZORG01L;
456100160315
456200160315       OG143 = ORGde3;
456300160315
456400160315       RETURN §OGntw;
456500160315
456600160315      /end-free
456700160315     P GetNtwFiliale   E
456800160315
456900160315      *--------------------------------------------------
457000160315      * Procedure name: IsFilAttivaORM
457100160315      * Purpose:        Ritorna se filiale attiva alla procedura ORM
457200160315      * Returns:        On - OFF
457300160315      * Parameter:      Filiale
457400160315      *--------------------------------------------------
457500160315     p IsFilAttivaORM  B
457600160315     d IsFilAttivaORM  PI              n
457700160315     D  filiale                       3s 0 CONST
457800160315
457900160315      // ds per campo ORGDE8 di AZORG
458000160315     d OG148         e ds
458100160315
458200160315      /free
458300160315
458400160315       IF  filiale = *zeros;
458500160315         RETURN *off;
458600160315       ENDIF;
458700160315
458800160315       clear OG148;
458900160315
459000160315       chain (filiale) AZORG01L;
459100160315
459200160315       OG148 = ORGde8;
459300160315
459400160315       IF  §OGorm <> 'S';
459500160315         return *off;
459600160315       ENDIF;
459700160315
459800160315       RETURN *on;
459900160315
460000160315      /end-free
460100160315     P IsFilAttivaORM  E
460200160315
460300160315      *--------------------------------------------------
460400160315      * Procedure name: IsKscValido
460500160315      * Purpose:        Ritorna se Ksc è valido
460600160315      * Returns:        On - OFF
460700160315      * Parameter:      cliente
460800160315      *--------------------------------------------------
460900160315     p IsKscValido     B
461000160315     d IsKscValido     PI              n
461100160315     D  cliente                       7s 0 CONST
461200160315
461300160315      // campi di comodo
461400160315     d filiale         s              3s 0
461500160401     d k_kcc           s                   like(ACOkcc) inz(151)
461600160401     d k_kut           s                   like(ACOkut) inz(1)
461700160315     d network         s              3a
461800160315
461900160315      /free
462000160315
462100160315       IF  cliente = *zeros;
462200160315         RETURN *off;
462300160315       ENDIF;
462400160315
462500160401       chain (k_kut:k_kcc:cliente) CNACO00F;
462600160315       IF  not %found(CNACO00F);
462700160315         RETURN *off;
462800160315       ENDIF;
462900160315       IF  ACOflg <> *blanks;
463000160315         RETURN *off;
463100160315       ENDIF;
463200160315
463300160315      // cerco il ntw del cliente non posso accettare clienti
463400160315      // logistica o di sede
463500160401       filiale = %dec(%subst(%editc(cliente:'X'):1:3):3:0);
463600160315       network = GetNtwFiliale(filiale);
463700160315       IF  network = 'LOG' or network = 'XXX';
463800160315         RETURN *off;
463900160315       ENDIF;
464000160315
464100160315       RETURN *on;
464200160315
464300160315      /end-free
464400160315     P IsKscValido     E
464500160315
464600160315      *--------------------------------------------------
464700160315      * Procedure name: IsValidoCapDPD
464800160315      * Purpose:        Ritorna se Cap DPD valido
464900160315      * Returns:        On - OFF
465000160315      * Parameter:      nazione - cap del mittente
465100160315      *--------------------------------------------------
465200160315     p IsValidoCapDPD  B
465300160315     d IsValidoCapDPD  PI              n
465400160315     D  nazione                       3a   CONST
465500160315     D  cap                           9a   CONST
465600160315     D  data                          8s 0 CONST
465700160315     D  peso                          7s 1 CONST
465800160315     D  limite                        7s 1 CONST
465900160315     D  filiale                       3s 0 CONST
466000160315
466100160315      // ds per cappario DPD
466200160315     d TISIE3DS      e ds                  QUALIFIED INZ
466300160315      // pgm per cappario DPD
466400160315     d TISIE3R         pr                  extpgm('TISIE3R')
466500160315     d  tisie3ds                           likeds(TISIE3DS)
466600160315
466700160315      /free
466800160315
466900160315       IF  nazione = *blanks or cap = *blanks or filiale = 0;
467000160315         RETURN *off;
467100160315       ENDIF;
467200160315
467300160315       clear TISIE3DS;
467400160315       IF  data = 0;
467500160315         tisie3ds.ISIE3dri = %dec(%date());
467600160315       ELSE;
467700160315         tisie3ds.ISIE3dri = data;
467800160315       ENDIF;
467900160315
468000160315       tisie3ds.ISIE3hsp = %dec(%time());
468100160315       tisie3ds.ISIE3nzd = nazione;
468200160315       tisie3ds.ISIE3cad = cap;
468300160315       IF  peso > limite;
468400160315         tisie3ds.ISIE3srv = 101;
468500160315       ELSE;
468600160315         tisie3ds.ISIE3srv = 136;
468700160315       ENDIF;
468800160315       tisie3ds.ISIE3lnp = filiale;
468900160315       tisie3r (tisie3ds);
469000160315       IF  tisie3ds.OSIE3err <> *blanks;
469100160315         RETURN *off;
469200160315       ENDIF;
469300160315
469400160315       RETURN *on;
469500160315
469600160315      /end-free
469700160315     P IsValidoCapDPD  E
