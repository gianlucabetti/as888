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
009700131223
009800131223       // -?Calcola orari di servizio
009900140327     d TRULORSDS     e ds                  QUALIFIED INZ
010000140327     d TRULOR2ds     e ds                  QUALIFIED INZ
010100131009
010200131204       // -?Controllo instradamento
010300131204     d TISI95DS      e ds                  QUALIFIED INZ
010400131205     d TISI97DS      e ds                  QUALIFIED INZ
010500131204
010600131204       // -?Controllo orari apertura
010700131204     d TRUL03DS      e ds                  QUALIFIED INZ
010800160408
010900160408       // -?Ritorna dati Servizio Clienti
011000160408     d TRULDSCDS     e ds                  QUALIFIED INZ
011100131008
011200131008       // -?Controllo partita IVA
011300151020     d XCFIVA1DS     e ds                  QUALIFIED INZ
011400140311
011500140311       // -?Controllo indirizzo mail
011600140311     d dsemail       e ds                  QUALIFIED INZ
011700160208
011800160208       // -?Tabella 15 - Nazioni
011900160208     d ds15          e ds                  QUALIFIED INZ
012000131204
012100131204       // -?Tabella 3I - DPD
012200131204     d ds3Idp        e ds                  QUALIFIED INZ
012300151008
012400151008       // -?Tabella DFT - Default ORM
012500151008     d dDFT          e ds                  QUALIFIED INZ
012600160315
012700160315       // -?Tabella FFC - Forza Filiale Consenga
012800160315     d dFFC          e ds                  QUALIFIED INZ
012900131010
013000131010       // -?Tabella NTW - Network
013100131204     d dNTW          e ds                  QUALIFIED INZ
013200140114
013300140114       // -?Campo ACRMAI
013400140114     d dACR01        e ds                  QUALIFIED INZ
013500131010
013600131010       // -?Campo VAOFLO
013700131204     d dORM01        e ds                  QUALIFIED INZ
013800131008
013900131008       // -?Partita IVA
014000131009     d wIVA            ds
014100131008     d  wISO                   1      2
014200131008     d  wPIVA                  3     16
014300131009
014400131009       //--------------------------------------------------------------
014500131009       //?Definizione schiere.
014600131009       //--------------------------------------------------------------
014700131009     d skprv           s              2a   dim(200)
014800131218     d sknaz           s              3a   dim(500)
014900160208     d sknazd          s             25a   dim(500)
015000131007
015100131007       //--------------------------------------------------------------
015200131007       //?Definizione campi.
015300131007       //--------------------------------------------------------------
015400131009     d dataoggi        s              8s 0
015500131220     d inv_dataritiro  s              8s 0
015600131010     d kpjba           s            502a
015700131205     d k_ORGfil        s                   like(ORGfil)
015800131205     d k_TBEcod        s                   like(TBEcod)
015900131205     d k_TBEke1        s                   like(TBEke1)
016000160315     d k_TBEke2        s                   like(TBEke2)
016100131205     d k_TBLcod        s                   like(TBLcod)
016200131205     d k_TBLkey        s                   like(TBLkey)
016300131010     d k_TBLkut        s                   like(TBLkut)
016400160315     d k_TAMksc        s                   like(TAMksc)
016500160315     d k_TAMctr        s                   like(TAMctr)
016600160315     d keyfilritiro    s                   like(wFiliale)
016700160315     d keytabella      s             15a
016800140411     d oraalfa         s              4a   inz
016900160216     d parametroNPR    s             10a   inz
017000140402     d wCap            s                   inz like(fiora0I0.vaocar)
017100140401     d wCodice         s             10s 0
017200131010     d wCom            s               n   inz
017300140408     d wData           s              8s 0 inz
017400131009     d wErr            s               n   inz
017500131219     d wErrMitt        s               n   inz
017600160613     d wErrQta         s               n   inz
017700160315     d wFil            s              3s 0 inz
017800131204     d wFiliale        s                   like(tisi95ds.O95lna)
017900160315     d wForza          s               n   inz
018000140401     d wHH             s              2s 0
018100131009     d wIdMsg          s              7a
018200131008     d wIdCampo        s             10a
018300140402     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
018400140402     d wLocalita       s                   inz like(fiora0I0.vaolor)
018500140401     d wMM             s              2s 0
018600140402     d wNazione        s                   inz like(fiora0I0.vaonar)
018700160315     d wNetworkCon     s              3a
018800160315     d wNetworkEmi     s              3a
018900160315     d wNetworkOrd     s              3a
019000131205     d wNetworkRit     s              3a
019100140211     d wNrForza        s              3s 0
019200140211     d wNrfForza       s              3s 0
019300160315     d wOKffc          s               n   inz
019400131204     d wOKfilritiro    s               n   inz
019500160315     d wOKtariffa      s               n   inz
019600140401     d wOra            s              6s 0
019700131204     d wParm           s            512a   inz
019800131204     d wPeso           s                   like(tisi95ds.I95lkg)
019900160606     d wPesoaut        s             10s 1
020000160606     d wPesoblc        s             10s 1
020100160606     d wPesomax        s             10s 1
020200160606     d wPesomtc        s             10s 1
020300160315     d wPickup         s                   like(tisi95ds.O95gf2)
020400140408     d wPor            s                   inz like(fiora0I0.vaopor)
020500140402     d wProvincia      s                   inz like(fiora0I0.vaoprr)
020600151008     d wVolume         s                   like(tisi95ds.I95lmc)
020700160606     d wVolumeaut      s             10s 3
020800160606     d wVolumeblc      s             10s 3
020900151008     d wVolumemax      s             10s 3
021000160606     d wVolumemtc      s             10s 3
021100131009     d wVuoto          s              1a
021200131009     d xx              s              3s 0
021300131008
021400131008       //--------------------------------------------------------------
021500131008       //?Definizione procedure.
021600131008       //--------------------------------------------------------------
021700160315       // -?Controllo giro di ritiro
021800160315     d fidg09r         pr                  extpgm('FIDG09R')
021900160315     d  kpjba                       502a
022000160526
022100160526       // -?Ritorna Orari Servizio Ritiro Estero
022200160526     d fiore00r        pr                  extpgm('FIORE00R')
022300160526     d  fiore00ds                          likeds(FIORE00DS)
022400131205
022500131205       // -?Controllo destinatario per ORM export DPD
022600131205     d fior95r         pr                  extpgm('FIOR95R')
022700131205     d  kpjba                       502a
022800131205     d  fior95ds                           likeds(FIOR95ds)
022900131010
023000131010       // -?Forzatura filiale ritiro
023100131010     d fior96r         pr                  extpgm('FIOR96R')
023200131010     d  kpjba                       502a
023300131010     d  fior96ds                           likeds(FIOR96ds)
023400131204
023500131204       // -?Calcolo data ritiro
023600131204     d fior97r         pr                  extpgm('FIOR97R')
023700131204     d  kpjba                       502a
023800131204     d  fior97ds                           likeds(FIOR97ds)
023900131205
024000131205       // -?Controllo ritirabilità all'estero
024100131205     d fnlv12r         pr                  extpgm('FNLV12R')
024200131205     d  fnlv12ds                           likeds(FNLV12DS)
024300131205     d  tisi95ds                           likeds(TISI95DS)
024400131205     d  tisi97ds                           likeds(TISI97DS)
024500151009
024600151009       // -?Controllo cap/località/provincia
024700151009     d fnlv13r         pr                  extpgm('FNLV13R')
024800151009     d  kpjba                       502a
024900151009     d  fnlv13ds                           likeds(FNLV13DS)
025000151009     d  tisi95ds                           likeds(TISI95DS)
025100160315
025200160315       // -?Pgm controllo indirizzo
025300160315     d FNLV14R         pr                  extpgm('FNLV14R')
025400160315     d  kpjba                       502a
025500160315     d  fnlv14ds                           likeds(FNLV14DS)
025600160315
025700160315       // -?Controllo terminal
025800160315     d fnlv55r         pr                  extpgm('FNLV55R')
025900160315     d  fnlv55ds                           likeds(FNLV55DS)
026000160216
026100160216       // -?Recupero nuovo Numero Prenotazione Ritiro
026200160216     d GetNPR          pr            10
026300160216     d  ParametroNPR                 10
026400131223
026500131223       // -?Calcolo orari servizio
026600140327     d trulorsr        pr                  extpgm('TRULORSR')
026700131223     d  kpjba                       502a
026800140327     d  trulorsds                          likeds(TRULORSDS)
026900140327     d  trulor2ds                          likeds(TRULOR2DS)
027000140327     d                                     options (*nopass)
027100131204
027200131204       // -?Controllo orari apertura
027300131204     d trul03r         pr                  extpgm('TRUL03R')
027400131204     d  trul03ds                           likeds(TRUL03ds)
027500160408
027600160408       // -?Ritorna dati Servizio Clienti
027700160408     d truldscr        pr                  extpgm('TRULDSCR')
027800160408     d  truldscds                          likeds(TRULDSCDS)
027900131008
028000131008       //--------------------------------------------------------------
028100131008       //?Definizione prototipi.
028200131008       //--------------------------------------------------------------
028300131008      /copy gaitrasrc/srcprotopr,FIORA00R
028400131009      /copy gaitrasrc/srcprotopr,RTVMSGLANG
028500131230      /copy gaitrasrc/srcprotopr,TIBS0800R
028600160315      /copy gaitrasrc/srcprotopr,TISI95R
028700140311      /copy gaitrasrc/srcprotopr,UBCHKCEL
028800140311      /copy gaitrasrc/srcprotopi,UBCHKCEL
028900151020      /copy gaitrasrc/srcprotopr,XCFIVAR1
029000140311      /copy gaitrasrc/srcprotopr,XEMAIL
029100131219
029200131219       //--------------------------------------------------------------
029300131219       //?Definizione parametri programma.
029400131219       //--------------------------------------------------------------
029500131219     d Fiora00_Immetti...
029600131219     d                 PI
029700131219     d prmRqsOpCode...
029800140210     d                               10I 0 CONST
029900131219     d prmRpyOpCode...
030000131219     d                               10I 0
030100131219     d prmRpyIdMsg...
030200131219     d                               10I 0
030300131219     d prmRqsFormato...
030400140210     d                               10A   CONST
030500131219     d prmRqsData...
030600131219     d                            32767A   OPTIONS(*VARSIZE)
030700131219     d prmRqsDataSize...
030800140210     d                               10I 0 CONST
030900131219     d prmRpyFormato...
031000140210     d                               10A   CONST
031100131219     d prmRpyData...
031200131219     d                            32767A   OPTIONS(*VARSIZE)
031300131219     d prmRpyDataSize...
031400140210     d                               10I 0 CONST
031500131219     d prmRpyFormMsg...
031600140408     d                               10A   CONST OPTIONS(*NOPASS)
031700131219     d prmRpyMessage...
031800140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
031900131219     d prmRpyMsgSize...
032000140408     d                               10I 0 CONST OPTIONS(*NOPASS)
032100131219     d prmRpyFormForz...
032200140408     d                               10A   CONST OPTIONS(*NOPASS)
032300131219     d prmRpyForzatu...
032400140408     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
032500131219     d prmRpyForSize...
032600140408     d                               10I 0 CONST OPTIONS(*NOPASS)
032700131007
032800131007       //--------------------------------------------------------------
032900131007       //?MAIN.
033000131007       //--------------------------------------------------------------
033100131008
033200131007      /free
033300131008
033400131008       //?Operazioni iniziali
033500131008       exsr RoutInz;
033600131008
033700131008       //?Controllo formale dei dati passati
033800131008       exsr Controlla;
033900140408
034000140408       //?Solo per formato FIORA0I0 = a richiamo <> da GET_FRASE1
034100140408       IF  fiora0I0.formato = prmRqsFormato;
034200140408
034300140408       //?Carico Tabelle
034400140408         exsr CaricaTab;
034500131007
034600131008       //?Imposto alcuni campi di DFT se non passati
034700131010         exsr DftInput;
034800131007
034900131007       //?Elaborazione principale per ORM da Internet
035000131010         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
035100131010           exsr Internet;
035200131010         ENDIF;
035300140620
035400160216       //?Elaborazione principale per ORM da AS400
035500140620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
035600140620           exsr AS400;
035700140620         ENDIF;
035800131009
035900131010       //?Imposto i dati di ritorno nella ds
036000131010       //?quelli che non sono già stati impostati dalle varie routine
036100131010         exsr DsOutput;
036200160216
036300160318       //?Dalla 'C' della ds di output FIORA0O0
036400160216       //?Se arrivo senza errori e sono stata richiamata con
036500160216       //?id richiamo OpCode WRITENPR
036600160216       //?devo staccare un nuovo numero NPR e tornarlo al chiamante
036700160408       //?più tutti i dati relativi alla filiale ritiro
036800160318         IF  fiora0O0.versione > 'B' and
036900160216             fiora0M0.nrmsg = 0 and
037000160216             prmRqsOpCode = FIORA00_RQSOPCODE_WRITENPR;
037100160216           fiora0O0.npr = GetNPR(parametroNPR);
037200160216         //?Se il rif. ORM è vuoto impoto il NPR
037300160216           IF  fiora0O0.vaorfa = *blanks;
037400160317             fiora0O0.vaorfa = fiora0O0.npr;
037500160216           ENDIF;
037600160408         //?Richiamo pgm che torna i dati del servizio clienti
037700160408         //?per la filiale passata
037800160503         //?per precauzione imposto già gli orari a 0
037900160503         //?è capitato che si spaccasse il pgm di Cussini per data/ora non validi
038000160503           fiora0O0.oraamdapor = *all'0';
038100160503           fiora0O0.oraamapor = *all'0';
038200160503           fiora0O0.orapmdapor = *all'0';
038300160503           fiora0O0.orapmapor = *all'0';
038400160408           clear TRULDSCDS;
038500160408           truldscds.IULDSCfil = fiora0O0.vaopor;
038600160408           truldscds.IULDSClin = fiora0O0.idlingua;
038700160408           truldscr (truldscds);
038800160408           IF  truldscds.OULDSCerr = *blanks;
038900160408             fiora0O0.descrpor = truldscds.OULDSCdesc;
039000160408             fiora0O0.telpor = truldscds.OULDSCtel;
039100160408             fiora0O0.mailpor = truldscds.OULDSCmail;
039200160408             fiora0O0.urlpor = truldscds.OULDSCurl;
039300160408             fiora0O0.oraamdapor = truldscds.OULDSCamda;
039400160408             fiora0O0.oraamapor = truldscds.OULDSCama;
039500160408             fiora0O0.orapmdapor = truldscds.OULDSCpmda;
039600160420             fiora0O0.orapmapor = truldscds.OULDSCpma;
039700160408           ENDIF;
039800160216         ENDIF;
039900140408
040000140408       ENDIF;
040100140408
040200140408       //?Solo per formato FIORA0I1 = a richiamo GET_FRASE1
040300140408       IF  fiora0I1.formato = prmRqsFormato;
040400140408         exsr GetFrase1;
040500140408       ENDIF;
040600131010
040700131010       //?Operazioni finali
040800131010       exsr RoutEnd;
040900131008
041000131008       //--------------------------------------------------------------
041100131008       //?Operazioni iniziali.                                         ?
041200131008       //--------------------------------------------------------------
041300131008       BEGSR  RoutInz;
041400140211
041500140211         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
041600131008
041700140210         prmRpyOpCode = FIORA00_RPYOPCODE_DONE;
041800140210         prmRpyIdMsg  = FIORA00_ESITO_OK;
041900131009
042000131009       //?Data del giorno
042100131009         dataoggi = %dec(%date());
042200131009
042300131009       //?Pulisco ds di output
042400131219         reset FIORA0O0;
042500140408         reset FIORA0O1;
042600131219         reset FIORA0M0;
042700131008
042800131008         *inLR = *on;
042900131008
043000131008       ENDSR;
043100131008
043200131008       //--------------------------------------------------------------
043300131008       //?Controllo formale dei dati passati.
043400131008       //--------------------------------------------------------------
043500131008       BEGSR  Controlla;
043600131008
043700131008       //?OpCode
043800131009         IF  prmRqsOpCode <> FIORA00_RQSOPCODE_CONTROL  and
043900140408             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITE    and
044000160216             prmRqsOpCode <> FIORA00_RQSOPCODE_WRITENPR and
044100140408             prmRqsOpCode <> FIORA00_RQSOPCODE_GET_FRASE1;
044200131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
044300131009           prmRpyIdMsg  = FIORA00_ESITO_RQSOPCODE_SCONOSCIUTO;
044400140210           exsr RoutEnd;
044500131008         ENDIF;
044600140408
044700140408       //?per GET_FRASE1
044800140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
044900140408       //?Formato e size RQS
045000140408           IF  prmRqsFormato = fiora0I1.formato;
045100140408             fiora0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
045200140408           ELSE;
045300140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
045400140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
045500140408             exsr RoutEnd;
045600140408           ENDIF;
045700140408           IF  prmRqsDataSize > 0;
045800140408           ELSE;
045900140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
046000140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
046100140408             exsr RoutEnd;
046200140408           ENDIF;
046300140408         //?Formato e size RPY
046400140408           IF  prmRpyFormato = fiora0O1.formato;
046500140408           ELSE;
046600140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
046700140408             prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
046800140408             exsr RoutEnd;
046900140408           ENDIF;
047000140408           IF  prmRpyDataSize > 0;
047100140408           ELSE;
047200140408             prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
047300140408             prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
047400140408             exsr RoutEnd;
047500140408           ENDIF;
047600140408           leavesr;
047700140408         ENDIF;
047800131008
047900140210       //?Formato e size RQS
048000131008         IF  prmRqsFormato = fiora0I0.formato;
048100131219           fiora0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
048200131008         ELSE;
048300131008           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
048400131008           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
048500140210           exsr RoutEnd;
048600131008         ENDIF;
048700140210         IF  prmRqsDataSize > 0;
048800140210         ELSE;
048900140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
049000140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
049100140210           exsr RoutEnd;
049200140210         ENDIF;
049300140210
049400140210       //?Formato e size RPY
049500140210         IF  prmRpyFormato = fiora0O0.formato;
049600140210         ELSE;
049700140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
049800140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
049900140210           exsr RoutEnd;
050000140210         ENDIF;
050100140210         IF  prmRpyDataSize > 0;
050200140210         ELSE;
050300140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
050400140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
050500140210           exsr RoutEnd;
050600140210         ENDIF;
050700140210
050800140210       //?Formato e size MSG
050900140210         IF  prmRpyFormMsg = fiora0M0.formato;
051000140210         ELSE;
051100140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051200140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
051300140210           exsr RoutEnd;
051400140210         ENDIF;
051500140210         IF  prmRpyMsgSize > 0;
051600140210         ELSE;
051700140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
051800140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
051900140210           exsr RoutEnd;
052000140210         ENDIF;
052100140210
052200140210       //?Formato e size Forzature
052300140210         IF  prmRpyFormForz = fiora0F0.formato;
052400160527           fiora0F0 = %SUBST(prmRpyForzatu:1:prmRpyForSize);
052500140210         ELSE;
052600140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
052700140210           prmRpyIdMsg = FIORA00_ESITO_NOME_FORMATO_SCONOSCIUTO;
052800140210           exsr RoutEnd;
052900140210         ENDIF;
053000140210         IF  prmRpyForSize > 0;
053100140210         ELSE;
053200140210           prmRpyOpCode = FIORA00_RPYOPCODE_ERROR;
053300140210           prmRpyIdMsg = FIORA00_ESITO_SIZE_DATA_ERRATO;
053400140210           exsr RoutEnd;
053500140210         ENDIF;
053600131008
053700131008       ENDSR;
053800140408
053900140408       //--------------------------------------------------------------
054000140408       //?Carico tabelle.
054100140408       //--------------------------------------------------------------
054200140408       BEGSR  CaricaTab;
054300140408
054400140408       //?Carico sk delle provincie
054500140408         clear xx;
054600140408         clear skprv;
054700140408         k_TBLkut = 1;
054800140408         k_TBLcod = 'PR';
054900140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
055000140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
055100140408         DOW not %eof(TABEL00F);
055200140408           IF  TBLflg = *blanks;
055300140408             xx += 1;
055400140408             skprv(xx) = TBLkey;
055500140408           ENDIF;
055600140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
055700140408         ENDDO;
055800140408
055900140408       //?Carico sk delle nazioni
056000140408         clear xx;
056100140408         clear sknaz;
056200140408         k_TBLkut = 1;
056300140408         k_TBLcod = '15';
056400140408         setll (k_TBLkut:k_TBLcod) TABEL00F;
056500140408         reade (k_TBLkut:k_TBLcod) TABEL00F;
056600140408         DOW not %eof(TABEL00F);
056700140408           IF  TBLflg = *blanks;
056800160208             ds15 = TBLuni;
056900140408             xx += 1;
057000140408             sknaz(xx) = TBLkey;
057100160208             sknazd(xx) = ds15.§15des;
057200140408           ENDIF;
057300140408           reade (k_TBLkut:k_TBLcod) TABEL00F;
057400140408         ENDDO;
057500140408
057600140408       //?Carico tabella 3I - DPD
057700140408         clear ds3Idp;
057800140408         k_TBLkut = 1;
057900140408         k_TBLcod = '3I';
058000140408         k_TBLkey = 'DPD';
058100140408         chain (k_TBLkut:k_TBLcod:k_TBLkey) TABEL00F;
058200140408         IF  %found(TABEL00F) and TBLflg = *blanks;
058300140408           ds3Idp = TBLuni;
058400140408         ENDIF;
058500151008
058600151008       //?Carico tabella DFT ORM con key generica 999
058700151008         clear dDFT;
058800151008         k_TBEcod = 'DFT';
058900151008         k_TBEke1 = '999';
059000151008         chain (k_TBEcod:k_TBEke1) TNTBE01L;
059100151008         IF  %found(TNTBE01L);
059200151008           dDFT = TBEuni;
059300151008         ENDIF;
059400140408
059500140408       ENDSR;
059600131007
059700131007       //--------------------------------------------------------------
059800131008       //?Imposta i DFT se non passati alcuni campi.
059900131007       //--------------------------------------------------------------
060000131008       BEGSR  DftInput;
060100131007
060200140210       //?Se ID richiamo non valido o non passato
060300140210       //?assumo internet
060400140210         IF  fiora0I0.idrichiamo = *blanks or
060500140210            (fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET and
060600140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_FILE and
060700140210             fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_AS400);
060800131009            fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
060900140210            fiora0M0.nrmsg += 1;
061000140210            fiora0M0.skIdMsg(fiora0M0.nrmsg) = '*parm';
061100140210            fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'IDRICHIAMO';
061200140210            fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
061300140210            fiora0M0.skTextMsg(fiora0M0.nrmsg) =
061400140210            'Id richiamo non previsto, assunto Internet';
061500131007         ENDIF;
061600131007
061700140210       //?Se ID lingua non valido o non passato
061800140210       //?assumo italiano
061900140210         IF  fiora0I0.idlingua = *blanks or
062000140210            (fiora0I0.idlingua <> FIORA00_ID_LINGUA_IT and
062100140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_FR and
062200140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_EN and
062300140210             fiora0I0.idlingua <> FIORA00_ID_LINGUA_DE);
062400140210            fiora0I0.idlingua = FIORA00_ID_LINGUA_IT;
062500160509            //fiora0M0.nrmsg += 1;
062600160509            //fiora0M0.skIdMsg(fiora0M0.nrmsg) = '*parm';
062700160509            //fiora0M0.skIdCampo(fiora0M0.nrmsg) = 'IDLINGUA';
062800160509            //fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
062900160509            //fiora0M0.skTextMsg(fiora0M0.nrmsg) =
063000160509            //'Id lingua non previsto, assunto Italiano';
063100131007         ENDIF;
063200131010
063300131010       //?Imposto la ds DORM01
063400131010         dORM01 = fiora0I0.vaoflo;
063500131007
063600131007       ENDSR;
063700131007
063800131007       //--------------------------------------------------------------
063900131007       //?Routine principale per view INTERNET.
064000131007       //--------------------------------------------------------------
064100131008       BEGSR  Internet;
064200131008
064300151110       //?Dalla versione 'B' il controllo del Cod.Fiscale e/o Partita IVA
064400151110       //?è da fare solo se cliente senza psw e se paga mittente
064500151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor = 0 and
064600151110             fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
064700151110           exsr CodFiscPiva;
064800151110         ENDIF;
064900151110         IF  fiora0I0.versione > 'A' and fiora0I0.vaocor > 0;
065000151110           exsr Ordinante;
065100151110         ENDIF;
065200151110
065300151104         IF  fiora0I0.versione = 'A';
065400131010       //?Se ordinante non codificato (è utente senza PSW)
065500131008       //?controllo Codice Fiscale/Partita IVA
065600131009         IF  fiora0I0.vaocor = 0;
065700131010           exsr CodFiscPiva;
065800131223         ELSE;
065900131223           exsr Ordinante;
066000131008         ENDIF;
066100151104         ENDIF;
066200131009
066300131009       //?Controllo cliente mittente
066400131009         IF  fiora0I0.vaocra > 0;
066500131010           exsr CodMittente;
066600131219         ELSE;
066700131219       //?Controllo mittente
066800131219           exsr Mittente;
066900131219         ENDIF;
067000131219
067100131219       //?Se manca il mittente ho finito i giochi, subito errore
067200131219         IF  wErrMitt;
067300131219           leavesr;
067400131219         ENDIF;
067500140311
067600140311       //?Controllo chi paga
067700140311         exsr ChiPaga;
067800140404
067900140404       //?Controllo commissionato
068000140404         exsr Commissionato;
068100131205
068200131205       //?Controllo filiale emissione
068300131205         exsr FilEmissione;
068400151008
068500151008       //?Controllo le quantità
068600151008         exsr Quantita;
068700151008
068800151008       //?Calcolo il maggiore tra volume/quantità per instradamento
068900151008         exsr PesoVolume;
069000131205
069100131205       //?Controllo filiale ritiro
069200131205         exsr FilRitiro;
069300140408
069400140408       //?Cerco gli orari servizio
069500140408         exsr OrariServizio;
069600131205
069700131205       //?Controllo data ritiro
069800131205         exsr DataRitiro;
069900131011
070000131011       //?Controllo ora pronta merce
070100131011         exsr OraRitiro;
070200160404
070300160404       //?Controllo ora pronta merce con orari servizio
070400160404         exsr OraRitiroServizio;
070500131028
070600131028       //?Controllo orari apertura
070700140404         exsr OrariApertura;
070800131204
070900131204       //?Controllo la natura della merce
071000131204         exsr NaturaMerce;
071100131204
071200131204       //?Controllo uno o più destinatario
071300131204         exsr UnoPiuDest;
071400131205
071500131205       //?Controllo Referente
071600131205         exsr Referente;
071700131223
071800131223       //?Controllo Telefono
071900131223         exsr Telefono;
072000140311
072100140311       //?Controllo Mail
072200140311         exsr Mail;
072300140311
072400140311       //?Controllo SMS
072500140311         exsr Sms;
072600131205
072700140313       //?Controllo cliente destinatario
072800140313         IF  fiora0I0.vaocrc > 0;
072900140313           exsr CodDestinatario;
073000140313         ELSE;
073100131205       //?Controllo se deve esserci o no il Destinatario
073200140313           exsr Destinatario;
073300140313         ENDIF;
073400131205
073500131205       //?Controllo Fermo Deposito
073600131205         exsr FermoDeposito;
073700140506
073800140506       //?Controllo Alert
073900140506         exsr Alert;
074000151008
074100151008       //?Dalla versione 'B'
074200151008         IF  fiora0I0.versione > 'A';
074300151008         //?Controllo Mail x accettazione ORM
074400151008           exsr MailConferma;
074500151008         //?Calcolo orario indicativo ritiro
074600151008           exsr OrarioIndRitiro;
074700151008         ENDIF;
074800160318
074900160318       //?Dalla versione 'C'
075000160322         IF  fiora0I0.versione > 'B';
075100160318         //?Controllo SMS x accettazione ORM
075200160318           exsr SmsConferma;
075300160318         ENDIF;
075400160714
075500160714       //?Dalla versione 'D'
075600160714         IF  fiora0I0.versione > 'C';
075700160714         //?Controllo flag per memorizzazione dati su anagrafica clienti ritiro
075800160714           exsr FlagMemDati;
075900160714         ENDIF;
076000131007
076100131007       ENDSR;
076200140620
076300140620       //--------------------------------------------------------------
076400140620       //?Routine principale per view AS400.
076500140620       //--------------------------------------------------------------
076600140620       BEGSR  AS400;
076700160530
076800160530       //?Come prima cosa passo tutti i dati dalla ds di Input alla ds di Output
076900160530       //?in questo modo quando esco dal pgm per le forzature non perdo i dati
077000160530       //?immessi dall'utente
077100160530         eval-corr fiora0O0 = fiora0I0;
077200160530         reset fiora0O0.formato;
077300160530         reset fiora0O0.versione;
077400160322
077500160322       //?Controllo tipo Comunicazione ORM
077600160322         exsr TipoComOrm;
077700160315
077800160315       //?Controllo tipo ORM
077900160315         exsr TipoOrm;
078000160607
078100160607       //?Imposto uno o più destinatario
078200160607         exsr UnoPiuDest;
078300140620
078400140620       //?Controllo cliente mittente
078500140620         IF  fiora0I0.vaocra > 0;
078600140620           exsr CodMittente;
078700160401       //?Controllo dati mittente
078800160401         ELSE;
078900160401           exsr Mittente;
079000160315         ENDIF;
079100140717
079200140717       //?Se manca il mittente ho finito i giochi, subito errore
079300140717         IF  wErrMitt;
079400140717           leavesr;
079500140717         ENDIF;
079600140620
079700160315       //?Calcola filiale emissione
079800140620         exsr FilEmissione;
079900160404
080000160404       //?Se filiale ritiro già impostata la metto subito nei dati di output
080100160404         IF  fiora0I0.vaopor > 0;
080200160404           fiora0O0.vaopor = fiora0I0.vaopor;
080300160404         ENDIF;
080400160404
080500160404       //?In caso di Immissione/Copia ORM
080600160601         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
080700160606             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
080800160606       //?e mittente codificato
080900160606             fiora0I0.vaocra > 0;
081000160606         //?se non impostata calcolo la filiale ritiro
081100160606           IF  fiora0I0.vaopor = 0;
081200160601             exsr FilRitiro;
081300160606           ENDIF;
081400160613         //?calcolo la data ritiro
081500160613           exsr DataRitiro;
081600160606         //?Calcolo anche gli orari servizio
081700160606           exsr OrariServizio;
081800160404         ENDIF;
081900151008
082000151008       //?Controllo le quantità
082100151008         exsr Quantita;
082200160613         IF  wErrQta;
082300160613           leavesr;
082400160613         ENDIF;
082500151008
082600151008       //?Calcolo il maggiore tra volume/quantità per instradamento
082700151008         exsr PesoVolume;
082800160315
082900160404       //?In caso di Immissione/Copia ORM
083000160404       //?Se mittente NON codificato calcolo ora la filiale ritiro
083100160404         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
083200160404             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
083300160525             fiora0I0.vaocra = 0;
083400160404         //?Calcola filiale ritiro
083500160404           exsr FilRitiro;
083600160404         ENDIF;
083700160606
083800160606       //?Controllo Referente
083900160606         exsr Referente;
084000160606
084100160606       //?Controllo Telefono
084200160606         exsr Telefono;
084300160606
084400160606       //?Controllo orari apertura
084500160606         exsr OrariApertura;
084600160607
084700160607       //?Controllo chi paga
084800160607         exsr ChiPaga;
084900160607
085000160607       //?Controllo cliente destinatario
085100160607         IF  fiora0I0.vaocrc > 0;
085200160607           exsr CodDestinatario;
085300160607         ELSE;
085400160607       //?Controllo se deve esserci o no il Destinatario
085500160607           exsr Destinatario;
085600160607         ENDIF;
085700140620
085800160610       //?In caso di Immissione/Copia ORM
085900160610       //?Se mittente NON codificato calcolo ora la data ritiro
086000160610       //?Se prima non ci sono stati errori
086100160610         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
086200160610             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
086300160610             fiora0I0.vaocra = 0 and
086400160610             fiora0M0.nrmsg = 0;
086500160610         //?Controllo data ritiro
086600160610           exsr DataRitiro;
086700160610         ENDIF;
086800160404
086900160404       //?Cerco gli orari servizio
087000160613         exsr OrariServizio;
087100160610
087200160610       //?Controllo ora pronta merce
087300160610         exsr OraRitiro;
087400160518
087500160518       //?Controllo ora pronta merce con orari servizio
087600160518         exsr OraRitiroServizio;
087700140620
087800140620       //?Controllo la natura della merce
087900140620         exsr NaturaMerce;
088000160613
088100160613       //?Controllo le quantità della videata F05
088200160613         exsr Quantita2;
088300140620
088400140620       //?Controllo Mail
088500140620         exsr Mail;
088600140620
088700140620       //?Controllo SMS
088800140620         exsr Sms;
088900140620
089000140620       //?Controllo Fermo Deposito
089100140620         exsr FermoDeposito;
089200160315
089300160315       //?Dalla versione 'B'
089400160315         IF  fiora0I0.versione > 'A';
089500160315         //?Controllo Mail x accettazione ORM
089600160315           exsr MailConferma;
089700160318         //?Controllo SMS x accettazione ORM
089800160318           exsr SmsConferma;
089900160315         //?Calcolo orario indicativo ritiro
090000160315           exsr OrarioIndRitiro;
090100160315         ENDIF;
090200160315
090300160315       //?Controllo la filiale di ritiro
090400160601         exsr ControllaFilRitiro;
090500160315
090600160315       //?Controllo cliente ordinante
090700160315         IF  fiora0I0.vaocor > 0;
090800160315           exsr Ordinante;
090900160315         ENDIF;
091000160315       //?Controllo dati Ordinante
091100160315         exsr DatiOrdinante;
091200160315
091300160315       //?Controllo Filiale Consegna
091400160315       //?Campo della DS previsto solo da versione 'B' in poi
091500160315         IF  fiora0I0.versione > 'A';
091600160315           exsr FilConsegna;
091700160315         ENDIF;
091800160315
091900160315       //?Controllo Cliente tassazione
092000160315         exsr ClienteKSC;
092100160315
092200160315       //?Campo della DS previsto solo da versione 'B' in poi
092300160315         IF  fiora0I0.versione > 'A';
092400160315       //?Cerco il giro di ritiro per il mittente codificato
092500160315           IF  fiora0I0.vaocra > 0;
092600160315             exsr GiroRitiro;
092700160315           ENDIF;
092800160610       //?Controllo il giro
092900160610           IF  fiora0I0.cgi <> *blanks;
093000160610             clear FIDG09DS;
093100160610             fidg09ds.D09iop0 = '001';
093200160610             fidg09ds.D09ifgs = fiora0I0.VAOpor;
093300160610             fidg09ds.D09idat = dataoggi;
093400160610             fidg09ds.D09icgi = fiora0O0.cgi;
093500160610             fidg09ds.D09itug = 'R';
093600160610             %subst(kpjba:247:256) = FIDG09DS;
093700160610             fidg09r (kpjba);
093800160610             fidg09ds = %subst(kpjba:247:256);
093900160610             IF  fidg09ds.D09oerr <> *blanks;
094000160610               wIdMsg   = 'TIS0718';
094100160610               wIdCampo = 'CGI';
094200160610               wParm = fidg09ds.D09omsg;
094300160610               exsr Messaggi;
094400160610               leavesr;
094500160610             ENDIF;
094600160610           ENDIF;
094700160315         ENDIF;
094800160315
094900160315       //?Calcola commissionato
095000160315         exsr Commissionato;
095100160610
095200160610       //?Se non ci sono errori
095300160610       //?se sono in immissione ORM
095400160610       //?se ORM commissionato
095500160610       //?se dati alert ORM commissionato presenti
095600160610       //?verifico se la data di ritiro è da ricalcolare
095700160610         IF  fiora0M0.nrmsg = 0 and
095800160610             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
095900160610             wcom  and
096000160610            (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks) and
096100160610             fiora0I0.vaodar = dataoggi;
096200160610         //?Calcolo la data ritiro
096300160610           exsr DataRitiro;
096400160610         ENDIF;
096500160610
096600140620
096700140620       ENDSR;
096800131008
096900131008       //--------------------------------------------------------------
097000131008       //?Controllo codice fiscale/partita iva.
097100131008       //--------------------------------------------------------------
097200131010       BEGSR  CodFiscPiva;
097300131009
097400131009         wErr = *off;
097500140211
097600140211         exec sql
097700140211         set :fiora0O0.codfiscale = ucase (:fiora0I0.codfiscale);
097800140211
097900140211         exec sql
098000140211         set :fiora0O0.partitaiva = ucase (:fiora0I0.partitaiva);
098100131008
098200131008       //?Accettiamo solo 1 dei 2, o CF o PI
098300131223         IF  fiora0O0.codfiscale <> *blanks and
098400131223             fiora0O0.partitaiva <> *blanks;
098500131008           wIdMsg   = 'TIS0240';
098600131008           wIdCampo = 'CODFISCALE';
098700131204           clear wParm;
098800131008           exsr Messaggi;
098900131009           wErr = *on;
099000131008         ENDIF;
099100131008
099200131008       //?Almeno 1 dei 2 deve essere presente, o CF o PI
099300131223         IF  fiora0O0.codfiscale = *blanks and
099400131223             fiora0O0.partitaiva = *blanks;
099500131008           wIdMsg   = 'TIS0240';
099600131008           wIdCampo = 'CODFISCALE';
099700131204           clear wParm;
099800131008           exsr Messaggi;
099900131009           wErr = *on;
100000131008         ENDIF;
100100131008
100200131008       //?Controllo CF
100300131223         IF  fiora0O0.codfiscale <> *blanks;
100400151020           clear xcfiva1ds;
100500151020           xcfiva1ds.xcfivapar = fiora0O0.codfiscale;
100600151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
100700151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
100800151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
100900151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
101000151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
101100151020           xcfivar1 (xcfiva1ds);
101200151020           IF  xcfiva1ds.xcfivaflg < *zeros;
101300131008             wIdMsg   = 'TIS0069';
101400131008             wIdCampo = 'CODFISCALE';
101500131204             clear wParm;
101600131008             exsr Messaggi;
101700131009             wErr = *on;
101800131008           ENDIF;
101900131008         ENDIF;
102000131008
102100131008       //?Partita IVA valida
102200131223         IF  fiora0O0.partitaiva <> *blanks;
102300140211           IF  %subst(fiora0O0.partitaiva:1:2) >= '00';
102400140211             wPIVA = fiora0O0.partitaiva;
102500140211           ELSE;
102600140211             wIVA = fiora0O0.partitaiva;
102700140211           ENDIF;
102800151020           clear xcfiva1ds;
102900151020           xcfiva1ds.xcfivamod = 'P';
103000151020           xcfiva1ds.xcfivapar = wIVA;
103100151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
103200151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
103300151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
103400151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
103500151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
103600151020           xcfivar1 (xcfiva1ds);
103700131009
103800131008         //?Errore forzabile
103900151020           IF  xcfiva1ds.xcfivaflg = -9;
104000131008             wIdMsg   = 'TIS0444';
104100131008             wIdCampo = 'PARTITAIVA';
104200140210             clear wParm;
104300131008             exsr Forzatura;
104400131009             wErr = *on;
104500131008           ENDIF;
104600131008
104700131008         //?Errore bloccante
104800151020           IF  xcfiva1ds.xcfivaflg < *zeros and xcfiva1ds.xcfivaflg <> -9;
104900131008             wIdMsg   = 'TIS0444';
105000131008             wIdCampo = 'PARTITAIVA';
105100131204             clear wParm;
105200131009             exsr Messaggi;
105300131009             wErr = *on;
105400131008           ENDIF;
105500131009
105600131009       //?Se errore non imposto i campi di output
105700131009          IF  wErr;
105800131009            leavesr;
105900131009          ENDIF;
106000131008
106100131008       //?Imposto il codice ISO che torna dal pgm di controllo
106200151020           IF  wISO <> %subst(xcfiva1ds.xcfivapar:3:2);
106300151020             wISO = %subst(xcfiva1ds.xcfivapar:3:2);
106400131008           ENDIF;
106500131008         ENDIF;
106600131007
106700131008       ENDSR;
106800131223
106900131223       //--------------------------------------------------------------
107000131223       //?Imposto i dati dell'ordinante.
107100131223       //--------------------------------------------------------------
107200131223       BEGSR  Ordinante;
107300131223
107400160520       //?Solo per richiamo da AS400
107500160520       //?torno il codice al chiamante
107600160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
107700160520           fiora0O0.vaocor = fiora0I0.vaocor;
107800160520         ENDIF;
107900160520
108000140401         wCodice = fiora0I0.vaocor;
108100140402         reset FIORB0OO;
108200140401
108300140401       //?codice valido
108400140402         IF  not IsCodiceValido(FIORB0OO);
108500140305           wIdMsg   = 'TIS0719';
108600140304           wIdCampo = 'VAOCOR';
108700140305           clear wParm;
108800131223           exsr Messaggi;
108900131223           leavesr;
109000131223         ENDIF;
109100160315
109200160315       //?Solo per richiamo da AS400
109300160315       //?e solo dalla versione 'B' in poi
109400160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
109500160315             fiorb0OO.versione > 'A';
109600160315       //?codice bloccato
109700160714           IF  fiorb0OO.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
109800160315             wIdMsg   = 'TIS0799';
109900160315             wIdCampo = 'VAOCOR';
110000160315             clear wParm;
110100160315             exsr Messaggi;
110200160315             leavesr;
110300160315           ENDIF;
110400160315         ENDIF;
110500131227
110600131227       //?cerco network della filiale del codice ordinante
110700160315         clear wNetworkOrd;
110800160315         wFil = %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
110900160315         wNetworkOrd = GetNtwFiliale(wFil);
111000131227         clear dNTW;
111100131227         k_TBEcod = 'NTW';
111200160315         k_TBEke1 = wNetworkOrd;
111300131227         chain (k_TBEcod:k_TBEke1) TNTBE01L;
111400131227         IF  %found(TNTBE01L);
111500131227           dNTW = TBEuni;
111600131227         ENDIF;
111700131227       //?se la filiale del codice ordinante è una filiale con network DPD
111800160315         IF  wNetworkOrd = FIORA00_NTW_DPD;
111900131227         //?imposto il servizio DPD
112000131227           fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
112100131227         ENDIF;
112200131227       //?se la filiale del codice ordinante è una filiale con network estero
112300160315         IF  dntw.§NTWfie = FIORA00_NTW_ESTERO;
112400131227         //?imposto il servizio Estero
112500131227           fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
112600131227         ENDIF;
112700160315
112800160315       //?Solo per richiamo da AS400
112900160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
113000160315       //?imposto i dati di ritorno dell'ordinante
113100160315           fiora0I0.vaorso = fiorb0OO.nome;
113200160315           fiora0I0.vaoino = fiorb0OO.indirizzo;
113300160315           fiora0I0.vaocao = fiorb0OO.cap;
113400160315           fiora0I0.vaoloo = fiorb0OO.localita;
113500160315           fiora0I0.vaopro = fiorb0OO.provincia;
113600160315           fiora0I0.vaonao = fiorb0OO.nazione;
113700160315         //?Se paga ordinante imposto i dati
113800160315           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
113900160315               fiora0I0.vaoksc = 0 and fiorb0OO.codksc > 0;
114000160315             fiora0I0.vaoksc = fiorb0OO.codksc;
114100160315             IF  fiorb0OO.codtariffa = 999;
114200160315               clear fiora0I0.vaoctr;
114300160315             ELSE;
114400160315               fiora0I0.vaoctr =
114500160315               %subst(%editc(fiorb0OO.codtariffa:'X'):2:3);
114600160315             ENDIF;
114700160315           ENDIF;
114800160315         //?Imposto se contatto telefonico (commissionato)
114900160315         //?solo dalla versione 'B' in poi
115000160315           IF  fiorb0OO.versione > 'A';
115100160315             fiora0O0.contattot = fiorb0OO.contatto;
115200160315           ENDIF;
115300160315         ENDIF;
115400131223
115500131223       ENDSR;
115600160315
115700160315       //--------------------------------------------------------------
115800160315       //?Controllo dati Ordinante.
115900160315       //--------------------------------------------------------------
116000160315       BEGSR  DatiOrdinante;
116100160315
116200160315       //?Se non ho dati vado via
116300160315         IF  fiora0I0.vaorso = *blanks and fiora0I0.vaoino = *blanks and
116400160315             fiora0i0.vaoloo = *blanks and fiora0i0.vaocao = *blanks;
116500160315           leavesr;
116600160315         ENDIF;
116700160315
116800160315         exec sql
116900160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
117000160315
117100160315       //?Controllo nominativo ordinante
117200160315         exsr NomeOrdinante;
117300160315
117400160315       //?Controllo indirizzo ordinante
117500160315         exsr ViaOrdinante;
117600160315
117700160315       //?Controllo località ordinante
117800160315         exsr LocOrdinante;
117900160315
118000160315       //?Controllo cap ordinante
118100160315         exsr CapOrdinante;
118200160315
118300160315       //?Controllo provincia ordinante
118400160315         exsr ProvOrdinante;
118500160315
118600160315       //?Controllo nazione ordinante
118700160315         exsr NazOrdinante;
118800160315
118900160315       //?Controllo indirizzo completo dell'ordinante
119000160315         exsr IndTotOrdinante;
119100160315
119200160315       ENDSR;
119300160315
119400160315       //--------------------------------------------------------------
119500160315       //?Controllo nominativo ordinante.
119600160315       //--------------------------------------------------------------
119700160315       BEGSR  NomeOrdinante;
119800160315
119900160315         exec sql
120000160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
120100160315
120200160315       ENDSR;
120300160315
120400160315       //--------------------------------------------------------------
120500160315       //?Controllo indirizzo ordinante.
120600160315       //--------------------------------------------------------------
120700160315       BEGSR  ViaOrdinante;
120800160315
120900160315         exec sql
121000160315         set :fiora0O0.vaoino = ucase (:fiora0I0.vaoino);
121100160315
121200160315       ENDSR;
121300160315
121400160315       //--------------------------------------------------------------
121500160315       //?Controllo cap ordinante.
121600160315       //--------------------------------------------------------------
121700160315       BEGSR  CapOrdinante;
121800160315
121900160315         exec sql
122000160315         set :fiora0O0.vaocao = ucase (:fiora0I0.vaocao);
122100160315
122200160315       ENDSR;
122300160315
122400160315       //--------------------------------------------------------------
122500160315       //?Controllo località ordinante.
122600160315       //--------------------------------------------------------------
122700160315       BEGSR  LocOrdinante;
122800160315
122900160315         exec sql
123000160315         set :fiora0O0.vaoloo = ucase (:fiora0I0.vaoloo);
123100160315
123200160315       ENDSR;
123300160315
123400160315       //--------------------------------------------------------------
123500160315       //?Controllo provincia ordinante.
123600160315       //--------------------------------------------------------------
123700160315       BEGSR  ProvOrdinante;
123800160315
123900160315         exec sql
124000160315         set :fiora0O0.vaopro = ucase (:fiora0I0.vaopro);
124100160315
124200160315       //?Controllo con la sk delle provincie
124300160315         IF  fiora0O0.vaopro <> *blanks and
124400160315            %lookup(fiora0O0.vaopro:skprv) = 0;
124500160315           wIdMsg   = 'TIS0464';
124600160315           wIdCampo = 'VAOPRO';
124700160315           clear wParm;
124800160315           exsr Messaggi;
124900160315         ENDIF;
125000160315
125100160315       ENDSR;
125200160315
125300160315       //--------------------------------------------------------------
125400160315       //?Controllo nazione ordinante.
125500160315       //--------------------------------------------------------------
125600160315       BEGSR  NazOrdinante;
125700160315
125800160315         exec sql
125900160315         set :fiora0O0.vaonao = ucase (:fiora0I0.vaonao);
126000160315
126100160315         //?Controllo con la sk delle nazioni
126200160315         IF  fiora0O0.vaonao <> *blanks and
126300160315             %lookup(fiora0O0.vaonao:sknaz) = 0;
126400160315           wIdMsg   = 'TIS0385';
126500160315           wIdCampo = 'VAONAO';
126600160315           clear wParm;
126700160315           exsr Messaggi;
126800160315         ENDIF;
126900160315
127000160315       ENDSR;
127100160315
127200160315       //--------------------------------------------------------------
127300160315       //?Controllo indirizzo completo dell'ordinante.
127400160315       //--------------------------------------------------------------
127500160315       BEGSR  IndTotOrdinante;
127600160315
127700160315         clear TISI95DS;
127800160315         tisi95ds.I95tcn = '7';
127900160315         tisi95ds.I95nar = fiora0O0.vaonao;
128000160315         tisi95ds.I95cap = fiora0O0.vaocao;
128100160315         tisi95ds.I95loc = fiora0O0.vaoloo;
128200160315         tisi95ds.I95prv = fiora0O0.vaopro;
128300160315         tisi95ds.I95ind = fiora0O0.vaoino;
128400160315         tisi95ds.I95dat = %dec(%date());
128500160315
128600160315       //?richiamo il tisi95r
128700160315         ChkCapLocalita (tisi95ds);
128800160315
128900160315       //?errore o dato non affidabile
129000160315         IF  fiora0O0.vaonao = *blanks and tisi95ds.O95lia <> '3'  or
129100160315             fiora0O0.vaonao <> *blanks and tisi95ds.O95lia <> '2';
129200160315           wIdMsg   = 'TIS0054';
129300160315           wIdCampo = 'VAOLOO';
129400160315           clear wParm;
129500160315           exsr Messaggi;
129600160315           leavesr;
129700160315         ENDIF;
129800160315
129900160315       ENDSR;
130000160322
130100160322       //--------------------------------------------------------------
130200160322       //?Controllo Tipo Comunicazione ORM.
130300160322       //--------------------------------------------------------------
130400160322       BEGSR  TipoComOrm;
130500160322
130600160404         k_TBEcod = 'TCO';
130700160404         k_TBEke1 = fiora0I0.tco;
130800160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
130900160322         IF  not %found(TNTBE01L);
131000160404           wIdMsg   = 'TIS0832';
131100160404           wIdCampo = 'TCO';
131200160322           clear wParm;
131300160322           exsr Messaggi;
131400160322           leavesr;
131500160322         ENDIF;
131600160520
131700160520       //?Se richiamato da AS400
131800160520       //?se sono in WRITE ORM
131900160520       //?controllo il tipo comunicazione ORM con il tipo richiamo
132000160520       //?se richiamato da immissine manuale posso immettere solo
132100160520       //?'TELEFONICO' 'MAIL/FAX'
132200160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
132300160520             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
132400160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_TELEFONICO and
132500160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_MAIL;
132600160520           wIdMsg   = 'TIS0832';
132700160520           wIdCampo = 'VAOTCO';
132800160520           clear wParm;
132900160520           exsr Messaggi;
133000160520         ENDIF;
133100160520       //?se richiamato da immissione VAS posso avere solo
133200160520       //?'FILE' 'INTERNET'
133300160520       //?se richiamato da immissione FISSI posso avere solo
133400160520       //?'FISSO'
133500160520       //?se richiamato da immissione PREPAGATI posso avere solo
133600160520       //?'PREPAGATO'
133700160322
133800160322       ENDSR;
133900160315
134000160315       //--------------------------------------------------------------
134100160315       //?Controllo Tipo ORM.
134200160315       //--------------------------------------------------------------
134300160315       BEGSR  TipoOrm;
134400160315
134500160322         k_TBEcod = 'TOR';
134600160322         k_TBEke1 = fiora0I0.tor;
134700160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
134800160322         IF  not %found(TNTBE01L);
134900160322           wIdMsg   = 'TIS0797';
135000160322           wIdCampo = 'TOR';
135100160322           clear wParm;
135200160322           exsr Messaggi;
135300160322           leavesr;
135400160322         ENDIF;
135500160315
135600160315       //?Se tipo ORM = P imposto il flag di prepagato
135700160315       //?e che è un solo destinatario
135800160322         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
135900160322           fiora0O0.prepagato = FIORA00_PREPAGATO;
136000160404           fiora0I0.unopiudest = '1';
136100160322         ENDIF;
136200160714       //?Se tipo ORM = M imposto 2 più destinatari
136300160322         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO;
136400160714           fiora0I0.unopiudest = FIORA00_PIUDESTINATARI;
136500160322         ENDIF;
136600160322       //?Se tipo ORM = S imposto 1 destinatario
136700160322         IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO;
136800160714           fiora0I0.unopiudest = FIORA00_UNDESTINATARIO;
136900160322         ENDIF;
137000160520
137100160520       //?Se richiamato da AS400
137200160608         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
137300160520       //?Tipo ORM multiplo e presenti i dati del destinatario errore
137400160608         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO and
137500160520            (fiora0I0.vaocrc > 0 or fiora0I0.vaorsc <> *blanks);
137600160520           wIdMsg   = 'TIS0802';
137700160520           wIdCampo = 'VAOTOR';
137800160520           clear wParm;
137900160520           exsr Messaggi;
138000160520         ENDIF;
138100160607       //?Tipo ORM prepagato e manca il destinatario errore
138200160608         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
138300160608             fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
138400160607           wIdMsg   = 'TIS0834';
138500160607           wIdCampo = 'VAOCRC';
138600160607           clear wParm;
138700160607           exsr Messaggi;
138800160607         ENDIF;
138900160608       //?Tipo ORM prepagato e c'è già la filiale ritiro calcolata
139000160608       //?verifico se possibile il prepagato o se devo immettere prima
139100160608       //?la bolla prepagata
139200160608         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
139300160608             fiora0O0.vaopor > *zeros and
139400160608             fiora0O0.vaopor = fiora0I0.fgs and
139500160608             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
139600160608           wIdMsg   = 'TIS0838';
139700160608           wIdCampo = 'VAOTOR';
139800160608           clear wParm;
139900160608           exsr Messaggi;
140000160608         ENDIF;
140100160608         ENDIF;
140200160315
140300160315       ENDSR;
140400131009
140500131009       //--------------------------------------------------------------
140600131009       //?Controllo codice mittente.
140700131009       //--------------------------------------------------------------
140800131010       BEGSR  CodMittente;
140900160315
141000160315         wErrMitt = *off;
141100140401
141200140401         wCodice = fiora0I0.vaocra;
141300140402         reset FIORB0OR;
141400131009
141500131009       //?codice valido
141600140402         IF not IsCodiceValido(FIORB0OR);
141700140312           wIdMsg   = 'TIS0068';
141800131009           wIdCampo = 'VAOCRA';
141900131204           clear wParm;
142000131009           exsr Messaggi;
142100160315         //?Solo per richiamo da AS400
142200160315           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
142300160315             wErrMitt = *on;
142400160315           ENDIF;
142500131009           leavesr;
142600131009         ENDIF;
142700160315
142800160315       //?Solo per richiamo da AS400
142900160315       //?e solo dalla versione 'B' in poi
143000160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
143100160315             fiorb0OR.versione > 'A';
143200160315         //?codice bloccato
143300160714           IF  fiorb0OR.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
143400160315             wIdMsg   = 'TIS0800';
143500160315             wIdCampo = 'VAOCRA';
143600160315             clear wParm;
143700160315             exsr Messaggi;
143800160315             wErrMitt = *on;
143900160315             leavesr;
144000160315           ENDIF;
144100160315         ENDIF;
144200140312
144300160315       //?Solo per richiamo da Internet
144400160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
144500140312       //?controllo se fa parte della stessa famiglia del codice di LOG
144600140313         IF not IsCodiceLegato();
144700140313           wIdMsg   = 'TIS0068';
144800140313           wIdCampo = 'VAOCRA';
144900140313           clear wParm;
145000140313           exsr Messaggi;
145100140313           leavesr;
145200140313         ENDIF;
145300160315         ENDIF;
145400140402
145500140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
145600140402         wIndirizzo = fiorb0OR.indirizzo;
145700140402         wCap = fiorb0OR.cap;
145800140402         wLocalita = fiorb0OR.localita;
145900140402         wProvincia = fiorb0OR.provincia;
146000140402         wNazione = fiorb0OR.nazione;
146100140411
146200140411       //?Solo per richiamo da Internet
146300140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
146400140411       //?normalizzo l'ora pronta merce dell'anagrafica clienti ritiro
146500140411             fiorb0OR.oraritiro > 0;
146600140411           oraalfa = GetOraAggiustata(fiorb0OR.oraritiro);
146700140411           fiorb0OR.oraritiro = %dec(oraalfa:4:0);
146800140411
146900151012           IF  fiorb0OR.oraritiro > *zeros and
147000151012              (fiora0I0.orrmin > 0 or fiora0I0.orrmax > 0);
147100140411             SELECT;
147200140411               WHEN fiorb0Or.oraritiro < fiora0I0.orrmin;
147300140411                 fiorb0OR.oraritiro = fiora0I0.orrmin;
147400140411               WHEN fiorb0Or.oraritiro > fiora0I0.orrmax;
147500140411                 fiorb0OR.oraritiro = fiora0I0.orrmax;
147600140411             ENDSL;
147700140411           ENDIF;
147800140411         ENDIF;
147900140717
148000140717       //?Solo per richiamo da AS400
148100160520       //?Imposto i dati del mittente
148200160519         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
148300160530           fiora0O0.vaorsr = fiora0I0.vaorsr;
148400160530           fiora0O0.vaoinr = fiora0I0.vaoinr;
148500160530           fiora0O0.vaocar = fiora0I0.vaocar;
148600160530           fiora0O0.vaolor = fiora0I0.vaolor;
148700160530           fiora0O0.vaoprr = fiora0I0.vaoprr;
148800160530           fiora0O0.vaonar = fiora0I0.vaonar;
148900140717         ENDIF;
149000131009
149100131009       ENDSR;
149200131219
149300131219       //--------------------------------------------------------------
149400131219       //?Controllo il mittente.
149500131219       //--------------------------------------------------------------
149600131219       BEGSR  Mittente;
149700131219
149800131219         wErrMitt = *off;
149900131219
150000131223       //?Se non ho nessun dato del cliente non controllo più niente
150100131219         IF  fiora0I0.vaorsr = *blanks and
150200131219             fiora0I0.vaoinr = *blanks and
150300131219             fiora0I0.vaocar = *blanks and
150400131219             fiora0I0.vaolor = *blanks and
150500131219             (fiora0I0.vaoprr = *blanks or
150600131219              fiora0I0.vaonar = *blanks);
150700140304           wIdMsg   = 'TIS0361';
150800131219           wIdCampo = 'VAORSR';
150900131219           clear wParm;
151000131219           exsr Messaggi;
151100131219           wErrMitt = *on;
151200131219           leavesr;
151300131219         ENDIF;
151400131219
151500131219       //?Nominativo mittente
151600131219         exsr NomeMittente;
151700131219
151800131219       //?Indirizzo mittente
151900131219         exsr ViaMittente;
152000160315
152100160315       //?Solo per richiamo da AS400
152200160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
152300160315         //?Controllo prima la località, poi provincia e cap
152400160315         //?Località mittente
152500160315           exsr LocMittente;
152600160315         //?Provincia mittente
152700160315           exsr ProvMittente;
152800160315         //?Cap mittente
152900160315           exsr CapMittente;
153000160520         ENDIF;
153100160520
153200160525       //?Per richiamo da INTERNET
153300160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
153400131219       //?Cap mittente
153500131219         exsr CapMittente;
153600131219
153700131219       //?Località mittente
153800131219         exsr LocMittente;
153900131219
154000131219       //?Provincia mittente
154100131219         exsr ProvMittente;
154200160315         ENDIF;
154300131219
154400131219       //?Nazione mittente
154500131219         exsr NazMittente;
154600131219
154700131219       //?Controllo indirizzo completo del mittente
154800131219         clear wPeso;
154900151008         clear wVolume;
155000131219         exsr IndTotMittente;
155100140402
155200140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
155300140402         wIndirizzo = fiora0I0.vaoinr;
155400140402         wCap = fiora0I0.vaocar;
155500140402         wLocalita = fiora0I0.vaolor;
155600140402         wProvincia = fiora0I0.vaoprr;
155700140402         wNazione = fiora0I0.vaonar;
155800131219
155900131219       ENDSR;
156000131009
156100131009       //--------------------------------------------------------------
156200131009       //?Controllo nominativo mittente.
156300131009       //--------------------------------------------------------------
156400131010       BEGSR  NomeMittente;
156500140211
156600140211         exec sql
156700140211         set :fiora0O0.vaorsr = ucase (:fiora0I0.vaorsr);
156800131009
156900131219       //?Errore se non presente
157000131223         IF  fiora0O0.vaorsr = *blanks;
157100131205           wIdMsg   = 'TIS0482';
157200131009           wIdCampo = 'VAORSR';
157300131204           clear wParm;
157400131009           exsr Messaggi;
157500131219           wErrMitt = *on;
157600131009         ENDIF;
157700131009
157800131009       ENDSR;
157900131009
158000131009       //--------------------------------------------------------------
158100131009       //?Controllo indirizzo mittente.
158200131009       //--------------------------------------------------------------
158300131010       BEGSR  ViaMittente;
158400140211
158500140211         exec sql
158600140211         set :fiora0O0.vaoinr = ucase (:fiora0I0.vaoinr);
158700131009
158800131219       //?Errore se non presente
158900131223         IF  fiora0O0.vaoinr = *blanks;
159000131205           wIdMsg   = 'TIS0283';
159100131009           wIdCampo = 'VAOINR';
159200131204           clear wParm;
159300131009           exsr Messaggi;
159400131219           wErrMitt = *on;
159500131009         ENDIF;
159600131009
159700131009       ENDSR;
159800131009
159900131009       //--------------------------------------------------------------
160000131009       //?Controllo cap mittente.
160100131009       //--------------------------------------------------------------
160200131010       BEGSR  CapMittente;
160300140211
160400140211         exec sql
160500140211         set :fiora0O0.vaocar = ucase (:fiora0I0.vaocar);
160600131009
160700131219       //?Errore se non presente
160800131223         IF  fiora0O0.vaocar = *blanks;
160900131205           wIdMsg   = 'TIS0053';
161000131009           wIdCampo = 'VAOCAR';
161100131204           clear wParm;
161200131009           exsr Messaggi;
161300131219           wErr = *on;
161400131009         ENDIF;
161500131009
161600131009       ENDSR;
161700131009
161800131009       //--------------------------------------------------------------
161900131009       //?Controllo località mittente.
162000131009       //--------------------------------------------------------------
162100131010       BEGSR  LocMittente;
162200140211
162300140211         exec sql
162400140211         set :fiora0O0.vaolor = ucase (:fiora0I0.vaolor);
162500131219
162600131009       //?Errore se non presente codice e località
162700131223         IF  fiora0O0.vaolor = *blanks;
162800131205           wIdMsg   = 'TIS0347';
162900131009           wIdCampo = 'VAOLOR';
163000131204           clear wParm;
163100131009           exsr Messaggi;
163200131219           wErr = *on;
163300131009         ENDIF;
163400131009
163500131009       ENDSR;
163600131009
163700131009       //--------------------------------------------------------------
163800131009       //?Controllo provincia mittente.
163900131009       //--------------------------------------------------------------
164000131010       BEGSR  ProvMittente;
164100140211
164200140211         exec sql
164300140211         set :fiora0O0.vaoprr = ucase (:fiora0I0.vaoprr);
164400131009
164500131009       //?Controllo con la sk delle provincie
164600131009         IF  fiora0O0.vaoprr <> *blanks and
164700131009             %lookup(fiora0O0.vaoprr:skprv) = 0;
164800131205           wIdMsg   = 'TIS0466';
164900131009           wIdCampo = 'VAOPRR';
165000131204           clear wParm;
165100131009           exsr Messaggi;
165200131219           wErrMitt = *on;
165300131009         ENDIF;
165400131009
165500131009       ENDSR;
165600131008
165700131009       //--------------------------------------------------------------
165800131009       //?Controllo nazione mittente.
165900131009       //--------------------------------------------------------------
166000131010       BEGSR  NazMittente;
166100140211
166200140211         exec sql
166300140211         set :fiora0O0.vaonar = ucase (:fiora0I0.vaonar);
166400131009
166500131009       //?Controllo con la sk delle provincie
166600131009         IF  fiora0O0.vaonar <> *blanks and
166700131009             %lookup(fiora0O0.vaonar:sknaz) = 0;
166800140326           wIdMsg   = 'TIS0386';
166900131009           wIdCampo = 'VAONAR';
167000131204           clear wParm;
167100131009           exsr Messaggi;
167200131219           wErrMitt = *on;
167300131009         ENDIF;
167400131009
167500131009       ENDSR;
167600131009
167700131009       //--------------------------------------------------------------
167800131009       //?Controllo indirizzo completo del mittente.
167900131009       //--------------------------------------------------------------
168000131010       BEGSR  IndTotMittente;
168100131009
168200131009         clear TISI95DS;
168300151009         clear FNLV13DS;
168400131204         tisi95ds.I95tcn = '7';
168500131204         tisi95ds.I95nar = fiora0O0.vaonar;
168600131204         tisi95ds.I95cap = fiora0O0.vaocar;
168700131204         tisi95ds.I95loc = fiora0O0.vaolor;
168800131204         tisi95ds.I95prv = fiora0O0.vaoprr;
168900131204         tisi95ds.I95ind = fiora0O0.vaoinr;
169000131204         tisi95ds.I95dat = %dec(%date());
169100131204         tisi95ds.I95lkg = wPeso;
169200151008         tisi95ds.I95lmc = wVolume;
169300151009         fnlv13ds.I13af0 = 'S';
169400151009         fnlv13ds.I13cnv = 'S';
169500151009         fnlv13ds.I13la3 = 'S';
169600131230
169700131230       //?Se richiesto ntw DPD lo passo
169800160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
169900131230           tisi95ds.I95fi1 = fiora0I0.sceltantw;
170000131230         ENDIF;
170100131009
170200131009       //?Solo per richiamo da Internet
170300131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
170400131009         //?se richiesta nazione irlanda forzo cap eire
170500131204           IF  tisi95ds.I95nar = FIORA00_IRLANDA;
170600131204             tisi95ds.I95cap =  FIORA00_EIRE;
170700131009           ENDIF;
170800131205         ENDIF;
170900131205
171000151009       //?controllo cap/località/provincia
171100151009         fnlv13r (kpjba:fnlv13ds:tisi95ds);
171200160525       //?Solo per richiamo da Internet
171300160525       //?controllo gli errori, da AS ci pensa già il programma chiamante
171400160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
171500151016       //?se torna errore per cap generico diversifico il messaggio
171600151016         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
171700151016             %scan('generico':fnlv13ds.O13msg) > 0;
171800151016           wIdMsg   = 'TIS0826';
171900151016           wIdCampo = 'VAOCAR';
172000151016           clear wParm;
172100151016           exsr Messaggi;
172200151016           wErrMitt = *on;
172300151016           leavesr;
172400151016         ENDIF;
172500151009       //?se torna errore imposto messaggio generico
172600151009         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
172700151016           wIdMsg   = 'TIS0056';
172800131205           wIdCampo = 'VAOLOR';
172900131205           clear wParm;
173000131205           exsr Messaggi;
173100131219           wErrMitt = *on;
173200131205           leavesr;
173300131205         ENDIF;
173400160525         ENDIF;
173500131009
173600131009       //?Imposto la linea di instradamento
173700131204         wFiliale = tisi95ds.O95lna;
173800160315
173900160315       //?Imposto la pickup area
174000160315         wPickup = tisi95ds.O95gf2;
174100131009
174200131009       ENDSR;
174300140311
174400140311       //--------------------------------------------------------------
174500140311       //?Controllo chi paga.
174600140311       //--------------------------------------------------------------
174700140311       BEGSR  ChiPaga;
174800140311
174900140311       //?Controllo formale del dato
175000140401         IF  fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE and
175100140401             fiora0I0.vaopag <> FIORA00_PAGA_ORDINANTE and
175200140401             fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
175300140311           wIdMsg   = 'TIS0140';
175400140311           wIdCampo = 'VAOPAG';
175500140311           clear wParm;
175600140311           exsr Messaggi;
175700140311           leavesr;
175800140311         ENDIF;
175900140311
176000140311       //?Solo per richiamo da Internet
176100140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
176200140311         //?Se utente loggato e ritiro presso altro luogo
176300140311         //?il pagamento non può essere a carico del mittente
176400140401           IF  fiora0I0.IdUtente > 0 and fiora0I0.vaocra = 0 and
176500140401               fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
176600140311             wIdMsg   = 'TIS0140';
176700140311             wIdCampo = 'VAOPAG';
176800140311             clear wParm;
176900140311             exsr Messaggi;
177000140311           ENDIF;
177100140311         ENDIF;
177200140311
177300140311       //?Ritiro all'estero non può pagare il mittente
177400140401         IF  fiora0I0.vaopag = FIORA00_PAGA_MITTENTE and
177500140402             wNazione <> *blanks and
177600140402             wNazione <> FIORA00_SANMARINO;
177700140311           wIdMsg   = 'TIS0695';
177800140311           wIdCampo = 'VAOPAG';
177900140311           clear wParm;
178000140311           exsr Messaggi;
178100140402         ENDIF;
178200140311
178300140311       //?Solo per richiamo da Internet
178400140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
178500140311       //?Imposto se prepagato
178600140401           IF  fiora0I0.vaocor = *zeros and
178700140401               fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
178800140311             fiora0O0.prepagato = FIORA00_PREPAGATO;
178900140311           ENDIF;
179000140311         ENDIF;
179100160607
179200160607       //?Solo per richiamo da AS400
179300160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
179400160607         //?Se ORM prepagato e non è ritiro all'estero
179500160607         //?deve pagare il Mittente
179600160607           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
179700160607               wNazione = *blanks and
179800160607               fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE;
179900160607             wIdMsg   = 'TIS0804';
180000160607             wIdCampo = 'VAOPAG';
180100160607             clear wParm;
180200160607             exsr Messaggi;
180300160607           ENDIF;
180400160607         //?Se ORM singolo e paga Destinatario
180500160607           IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO and
180600160607               fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
180700160607           //?il destinatario deve essere presente
180800160607             IF  fiora0I0.vaocrc = 0 and fiora0I0.vaorsc = *blanks;
180900160607               wIdMsg   = 'TIS0805';
181000160607               wIdCampo = 'VAOCRC';
181100160607               clear wParm;
181200160607               exsr Messaggi;
181300160607             ENDIF;
181400160607           //?il destinatario se codificato non può essere 8888 o 9999
181500160607             IF  fiora0I0.vaocrc > 0 and
181600160607                (%subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '8888' or
181700160607                 %subst(%editc(fiora0I0.vaocrc:'X'):4:4) = '9999');
181800160607               wIdMsg   = 'TIS0806';
181900160607               wIdCampo = 'VAOCRC';
182000160607               clear wParm;
182100160607               exsr Messaggi;
182200160607             ENDIF;
182300160607           ENDIF;
182400160607         //?Se Paga Ordinante
182500160607         //?l'ordinante deve essere un codificato
182600160607         //?e non può essere un 8888 o 9999
182700160607           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
182800160607               fiora0I0.vaocor = 0 or
182900160607              (%subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
183000160607               %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999');
183100160607             wIdMsg   = 'TIS0807';
183200160607             wIdCampo = 'VAOCOR';
183300160607             clear wParm;
183400160607             exsr Messaggi;
183500160607           ENDIF;
183600160607         ENDIF;
183700140311
183800140311       ENDSR;
183900140404
184000140404       //--------------------------------------------------------------
184100140404       //?Controllo commissionato.
184200140404       //--------------------------------------------------------------
184300140404       BEGSR  Commissionato;
184400140404
184500140407         wCom = *off;
184600140404
184700140404       //?Solo per richiamo da Internet
184800140404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
184900140404
185000140404         //?Contatto telefonico
185100140404           exec sql
185200140404           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
185300140404
185400140404         //?valido solo se Blank, N o S
185500140404           IF  fiora0O0.contattot <> *blanks and
185600160714               fiora0O0.contattot <> FIORA00_SI and
185700160714               fiora0O0.contattot <> FIORA00_NO;
185800140404             wIdMsg   = 'TIS0720';
185900140404             wIdCampo = 'CONTATTOT';
186000140404             clear wParm;
186100140404             exsr Messaggi;
186200140404           ENDIF;
186300140404
186400140404         //?Ordinante non codificato (è utente senza PSW)
186500160315         //?se il flag non è già impostato
186600140404         //?è sempre commissionato
186700140404           IF  fiora0I0.vaocor = 0;
186800140404             IF  fiora0O0.contattot = *blanks;
186900160714               fiora0O0.contattot = FIORA00_SI;
187000140404             ENDIF;
187100140404           ENDIF;
187200140404
187300160714           IF  fiora0O0.contattot = FIORA00_NO;
187400140407             wCom = *off;
187500140404           ENDIF;
187600160714           IF  fiora0O0.contattot = FIORA00_SI;
187700140407             wCom = *on;
187800140404           ENDIF;
187900140404         ENDIF;
188000160608
188100160608       //?Solo per immissione manuale ORM
188200160610         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
188300160610         //?se non già presente calcolo il flag di commissionato
188400160714           IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL or
188500160714                fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO) and
188600160610                fiora0I0.Contattot = *blanks;
188700160610           //?commissionato se:
188800160610           //?--> codice mittente <> codice ordinante (primi 7 byte)
188900160610           //?--> Ordinante impostato ma non codificato o 8888 o 9999
189000160610           //?--> Filiale emissione <> filiale ritiro e mittente non codificato
189100160610             IF  (%subst(%editc(fiora0I0.vaocra:'X'):1:7) <>
189200160610                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) and
189300160610                  %subst(%editc(fiora0I0.vaocor:'X'):1:7) > *zeros) or
189400160610                 (fiora0I0.vaorso <> *blanks and
189500160610                 (fiora0I0.vaocor = 0 or
189600160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '8888' or
189700160610                  %subst(%editc(fiora0I0.vaocor:'X'):4:4) = '9999'));
189800160714               fiora0I0.Contattot = FIORA00_SI;
189900160610             ENDIF;
190000160610             IF  fiora0I0.Contattot = *blanks and
190100160610    1            fiora0I0.vaopor <> fiora0I0.vaopoe and
190200160610                 fiora0I0.vaocra = 0;
190300160714               fiora0I0.Contattot = FIORA00_SI;
190400160610             ENDIF;
190500160610           ENDIF;
190600160608
190700160610           exec sql
190800160610           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
190900160610
191000160714           IF  fiora0O0.contattot = FIORA00_SI;
191100160610             wCom = *on;
191200160610           ENDIF;
191300160608         ENDIF;
191400140404
191500140404       ENDSR;
191600140404
191700140404       //--------------------------------------------------------------
191800140404       //?Cerco orari servizio.
191900140404       //--------------------------------------------------------------
192000140404       BEGSR  OrariServizio;
192100140408
192200140408         wPor  = fiora0O0.vaopor;
192300140408         wData = inv_dataritiro;
192400140408
192500140408       //?Richiamo il pgm TRULORSR
192600140408         exsr CallTrulORS;
192700140404
192800140404       //?Orari Standard Servizio
192900140404         fiora0O0.orainizio = trulor2ds.OOR2stis;
193000140404         fiora0O0.orafine   = trulor2ds.OOR2stfs;
193100140404
193200140404       //?Orari Limiti
193300140404         fiora0O0.oracomm = trulor2ds.OOR2lrsc;
193400140404         fiora0O0.oranocomm = trulor2ds.OOR2lrnc;
193500160526
193600160526       //?Se ritiro estero chiamo nuovo pgm per ricerca orari servizio
193700160526         IF  fiora0O0.vaonar <> *blanks;
193800160526           exsr CallFIORE;
193900160526         //?Orari Standard Servizio
194000160526           fiora0O0.orainizio = fiore00ds.OORE00min;
194100160526           fiora0O0.orafine   = fiore00ds.OORE00max;
194200160526         //?Orari Limiti
194300160526           fiora0O0.oracomm   = fiore00ds.OORE00lrg;
194400160526           fiora0O0.oranocomm = fiore00ds.OORE00lrg;
194500160526         ENDIF;
194600140404
194700140404       ENDSR;
194800131205
194900131205       //--------------------------------------------------------------
195000131205       //?Controllo filiale emissione.
195100131205       //--------------------------------------------------------------
195200131205       BEGSR  FilEmissione;
195300131205
195400131205       //?Se richiamo da Internet
195500131205         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
195600131205         //?Se ordinante codificato (è utente con PSW)
195700131205         //?e mittente non codificato
195800131205         //?filiale emissione del codice ordinante
195900140401           IF  fiora0I0.vaocor > 0 and fiora0I0.vaocra = 0;
196000131205             fiora0O0.vaopoe =
196100140401             %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
196200131205           ENDIF;
196300131205         //?Se ordinante non codificato (è utente senza PSW)
196400131205         //?filiale emissione da instradamento
196500140401           IF  fiora0I0.vaocor = 0;
196600131205             fiora0O0.vaopoe = wFiliale;
196700131205           ENDIF;
196800131205         ENDIF;
196900140402
197000140402         //?Se mittene codificato filiale emissione da FNACR
197100160519         //?solo se richiamo da internet
197200160519         IF  fiora0I0.vaocra > 0 and fiora0O0.vaopoe = 0 and
197300160519             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
197400140711           fiora0O0.vaopoe = fiorb0OR.filemi;
197500140402         ENDIF;
197600160315
197700160315       //?Se richiamo da AS400
197800160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
197900160315           fiora0O0.vaopoe = fiora0I0.vaopoe;
198000160524         //?Carico tabella DFT ORM della filiale emissione
198100160524         //?se c'è
198200160524           k_TBEcod = 'DFT';
198300160524           k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
198400160524           chain (k_TBEcod:k_TBEke1) TNTBE01L;
198500160524           IF  %found(TNTBE01L) and TBEatb = *blanks;
198600160524             dDFT = TBEuni;
198700160524           ENDIF;
198800160315         ENDIF;
198900131205
199000131205       //?Cerco il network della filiale emissione
199100131219         IF  fiora0O0.vaopoe > *zeros;
199200160315           wNetworkEmi = GetNtwFiliale(fiora0O0.vaopoe);
199300131219         ENDIF;
199400131205
199500131205       ENDSR;
199600131205
199700131205       //--------------------------------------------------------------
199800131205       //?Controllo filiale ritiro.
199900131205       //--------------------------------------------------------------
200000131205       BEGSR  FilRitiro;
200100131205
200200131205         wOKfilritiro = *off;
200300131205
200400131205       //?Cliente mittente codificato
200500131205       //?la filile ritiro è già calcolata
200600140401         IF  fiora0I0.vaocra > 0;
200700140402           wFiliale = fiorb0OR.filrit;
200800131205           wOKfilritiro = *on;
200900131205         ENDIF;
201000131205
201100131205       //?Calcolo la filiale ritiro
201200151008       //?Se cliente mittente NON codificato
201300151008       //?calcolo la filiale ritiro in base al cappario
201400131205         IF  not wOKfilritiro;
201500151008           exsr IndTotMittente;
201600131205         ENDIF;
201700131205
201800131205       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
201900131205         clear FIOR96ds;
202000160527         fior96ds.IOR96poe = fiora0O0.vaopoe;
202100131205         fior96ds.IOR96por = wFiliale;
202200131205         %subst(kpjba:92:10) = 'GAITRA201';
202300131205         fior96r (kpjba : fior96ds);
202400131205         IF  fior96ds.OOR96err = *blanks and fior96ds.OOR96por <> *zeros;
202500131205           wFiliale = fior96ds.OOR96por;
202600160601         //?In caso di richiamo da AS400
202700160601         //?pulisco il campo della filiale ritiro passato
202800160601         //?in questo modo forzo sempre anche se impostata a mano.
202900160601           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
203000160601             clear fiora0I0.vaopor;
203100160601           ENDIF;
203200131205         ENDIF;
203300160525
203400160525       //?In caso di richiamo in WRITE (Immissione/Copia ORM)
203500160525         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
203600160525             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
203700160525       //?Se la filale ritiro è una filiale gestita devo cercare le filiale Mamma
203800160525       //?per impostarla come filiale ritiro
203900160525           clear FNLV55DS;
204000160525           fnlv55ds.D55tpt = '6';
204100160525           fnlv55ds.D55lin = wFiliale;
204200160525           fnlv55ds.D55drf = %dec(%date());
204300160525           fnlv55r (FNLV55DS);
204400160525           IF  fnlv55ds.D55err = *blanks;
204500160525             wFiliale = fnlv55ds.D55tfa;
204600160525           ENDIF;
204700160525         ENDIF;
204800131205
204900160525       //?Se richiamo da Internet
205000160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
205100131205         fiora0O0.vaopor = wFiliale;
205200160525         ENDIF;
205300160404
205400160404       //?Se richiamo da AS400 e la filiale ritiro non è stata passata
205500160404       //?la imposto ora
205600160404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
205700160404             fiora0I0.vaopor = 0;
205800160404           fiora0I0.vaopor = wFiliale;
205900160525           fiora0O0.vaopor = wFiliale;
206000160404         ENDIF;
206100131205
206200131205       //?Imposto se ritiro all'estero
206300160315         wNetworkRit = GetNtwFiliale(fiora0O0.vaopor);
206400131205         fiora0O0.ritiroest = FIORA00_RITIROESTERO_NO;
206500131205         clear dNTW;
206600131205         k_TBEcod = 'NTW';
206700160315         k_TBEke1 = wNetworkRit;
206800131205         chain (k_TBEcod:k_TBEke1) TNTBE01L;
206900131205         IF  %found(TNTBE01L);
207000131205           dNTW = TBEuni;
207100131205         ENDIF;
207200160315         IF  wNetworkRit = FIORA00_NTW_DPD or
207300131205             dntw.§NTWfie = FIORA00_NTW_ESTERO;
207400131205           fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
207500131205         ENDIF;
207600131205
207700131205       //?Verifico la ritirabilità all'estero
207800131205         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
207900140402             wNazione <> *blanks;
208000131205           clear FNLV12DS;
208100131205           clear TISI95DS;
208200131205           clear TISI97DS;
208300131230           fnlv12ds.I12lang = fiora0I0.idlingua;
208400140402           tisi95ds.I95nar = wNazione;
208500140402           tisi95ds.I95cap = wCap;
208600131205           tisi95ds.I95dat = dataoggi;
208700131205           tisi97ds.I97ntw = wNetworkRit;
208800160315           IF  wNetworkRit = FIORA00_NTW_DPD;
208900160315             tisi95ds.I95fi1 = FIORA00_SERVIZIO_DPD;
209000131205           ENDIF;
209100131205           fnlv12r (fnlv12ds:tisi95ds:tisi97ds);
209200131205           IF  fnlv12ds.O12err <> *blanks;
209300140304             wIdMsg   = 'TIS0718';
209400160315           //?Solo per richiamo da Internet
209500160315             IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
209600131205             wIdCampo = 'SCELTANTW';
209700160315             ELSE;
209800160315               wIdCampo = 'VAOPOR';
209900160315             ENDIF;
210000131205             wParm = fnlv12ds.O12msg;
210100131205             exsr Messaggi;
210200131205           ENDIF;
210300131227
210400131227         //?Imposto il ntw da forzare per ritiro all'estero
210500131227           IF  fiora0I0.sceltantw <> *blanks;
210600131227             fiora0O0.sceltantw = fiora0I0.sceltantw;
210700131227             fiora0O0.forzantw = fiora0I0.sceltantw;
210800131227           ELSE;
210900160315             IF  wNetworkRit = FIORA00_NTW_DPD;
211000131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
211100131227             ENDIF;
211200160315             IF  wNetworkRit = FIORA00_NTW_EEX;
211300131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
211400131227             ENDIF;
211500131227           ENDIF;
211600131205
211700131227         ENDIF;
211800160525
211900160525       //?Se richiamato da AS400
212000160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
212100160525         //?Imposto il ntw da forzare per ritiro all'estero
212200160525           IF  fiora0I0.sceltantw <> *blanks;
212300160525             fiora0O0.sceltantw = fiora0I0.sceltantw;
212400160525             fiora0O0.forzantw = fiora0I0.sceltantw;
212500160525           ELSE;
212600160525             IF  wNetworkRit = FIORA00_NTW_DPD;
212700160525               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
212800160525             ENDIF;
212900160525             IF  wNetworkRit = FIORA00_NTW_EEX;
213000160525               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
213100160525             ENDIF;
213200160525           ENDIF;
213300160526         //?In caso di richiamo in WRITE (Immissione/Copia ORM)
213400160526           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
213500160526         //?Messaggio di avvertimento se filiale ritiro calcolata <> da quella del video
213600160526             IF  wFiliale > 0 and fiora0I0.vaopor > 0 and
213700160526                 wFiliale <> fiora0I0.vaopor;
213800160526               wIdMsg   = 'TIS0833';
213900160526               wIdCampo = 'VAOPOR';
214000160526               clear wParm;
214100160526               exsr Forzatura;
214200160531               IF  wForza;
214300160531                 exsr RoutEnd;
214400160531               ENDIF;
214500160526             ENDIF;
214600160526           ENDIF;
214700160525         ENDIF;
214800151008
214900151120         //?Se ordinante NON codificato quindi utente senza PSW e
215000151008         //?Se mittene NON codificato reimposto la filiale emissione
215100151008         //?la filiale ritiro potrebbe essere variata a causa delle quantità
215200151008         //?inserite
215300160601         //?SOLO se richiamato da Internet
215400160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
215500151120         IF  not wOKfilritiro and fiora0I0.vaocor = 0;
215600151008           fiora0O0.vaopoe = wFiliale;
215700151008         ENDIF;
215800160601         ENDIF;
215900160606
216000160606       //?Dalla versione 'D' della ds di output FIORA0O0
216100160606       //?torno la filiale ritiro calcolata
216200160620       //?e il pickup
216300160728       //?solo per AS400
216400160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
216500160606         IF  fiora0O0.versione > 'C';
216600160606           fiora0O0.filritcalc = wfiliale;
216700160620           fiora0O0.pickupcalc = wpickup;
216800160620         //?se cliente codificato cerco il pickup
216900160620           IF  wOkfilritiro;
217000160620             exsr IndTotMittente;
217100160620             fiora0O0.pickupcalc = wpickup;
217200160620           ENDIF;
217300160606         ENDIF;
217400160728         ENDIF;
217500131227
217600131205       ENDSR;
217700160315
217800160315       //--------------------------------------------------------------
217900160315       //?Controllo filiale ritiro.
218000160315       //--------------------------------------------------------------
218100160601       BEGSR  ControllaFilRitiro;
218200160315
218300160315       //?Filiale ritiro obbligatoria
218400160315         IF  fiora0I0.vaopor = 0;
218500160315           wIdMsg   = 'TIS0809';
218600160315           wIdCampo = 'VAOPOR';
218700160315           clear wParm;
218800160315           exsr Messaggi;
218900160315           leavesr;
219000160315         ENDIF;
219100160315
219200160601       //?Controllo se la filiale ritiro è valida
219300160601         IF  not IsFilValida(fiora0O0.vaopor);
219400160601           wIdMsg   = 'TIS0809';
219500160601           wIdCampo = 'VAOPOR';
219600160601           clear wParm;
219700160601           exsr Messaggi;
219800160601           leavesr;
219900160315         ENDIF;
220000160315       //?Errore se la filiale ritiro NON ha la procedura ORM attiva
220100160315         IF  not IsFilAttivaORM(fiora0O0.vaopor);
220200160607           wIdMsg   = 'TIS0836';
220300160315           wIdCampo = 'VAOPOR';
220400160607           clear wParm;
220500160315           exsr Messaggi;
220600160315         ENDIF;
220700160315
220800160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
220900160315       //?controllo congruenza tra network DPD e filiale ritiro
221000160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
221100160315             wNetworkRit <> FIORA00_NTW_DPD and
221200160315             fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
221300160607           wIdMsg   = 'TIS0837';
221400160315           wIdCampo = 'VAOPOR';
221500160607           clear wParm;
221600160315           exsr Messaggi;
221700160315         ENDIF;
221800160315
221900160315       //?Controllo con cappario DPD se ritiro per DPD
222000160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD or
222100160315             wNetworkRit = FIORA00_NTW_DPD;
222200160315           IF  not IsValidoCapDPD(fiora0O0.vaonar:
222300160315                                  fiora0O0.vaocar:
222400160315                                  fiora0O0.vaodar:
222500160315                                  fiora0I0.vaopkg:
222600160315                                  ds3idp.§3Ilmp:
222700160315                                  fiora0O0.vaopoe);
222800160315             wIdMsg   = 'TIS0811';
222900160315             wIdCampo = 'VAOCAR';
223000160315             clear wParm;
223100160315             exsr Messaggi;
223200160315           ENDIF;
223300160315         ENDIF;
223400160315
223500160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
223600160315       //?e filiale ritiro = a chi sta inserendo ORM
223700160315       //?non posso fare un prepagato
223800160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
223900160315             fiora0O0.vaopor = fiora0I0.fgs and
224000160315             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
224100160607           wIdMsg   = 'TIS0838';
224200160315           wIdCampo = 'VAOTOR';
224300160607           clear wParm;
224400160315           exsr Messaggi;
224500160315         ENDIF;
224600160315
224700160315       //?Non possibile ORM prepagato se ORM Export
224800160315         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
224900160315            (wNetworkRit = FIORA00_SERVIZIO_DPD or
225000160315             wNetworkRit = FIORA00_SERVIZIO_EEX);
225100160607           wIdMsg   = 'TIS0839';
225200160315           wIdCampo = 'VAOTOR';
225300160607           clear wParm;
225400160315           exsr Messaggi;
225500160315         ENDIF;
225600160527
225700160607       //?Se richiesto il ritiro tramite DPD deve essere ORM Singolo
225800160527         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
225900160607             fiora0I0.tor <> FIORA00_TIPOORM_SINGOLO;
226000160527           wIdMsg   = 'TIS0307';
226100160607           wIdCampo = 'VAOTOR';
226200160527           clear wParm;
226300160527           exsr Messaggi;
226400160527         ENDIF;
226500160315
226600160315       ENDSR;
226700131205
226800131205       //--------------------------------------------------------------
226900131205       //?Controllo data ritiro.
227000131205       //--------------------------------------------------------------
227100131205       BEGSR  DataRitiro;
227200131205
227300131223       //?Calcolo la data ritiro
227400131205         clear FIOR97ds;
227500131205         fior97ds.IOR97poe = fiora0O0.vaopoe;
227600131205         fior97ds.IOR97por = fiora0O0.vaopor;
227700140402         fior97ds.IOR97cap = wCap;
227800140402         fior97ds.IOR97loc = wLocalita;
227900160526         fior97ds.IOR97naz = wNazione;
228000131205         IF  fiora0I0.vaodao = 0;
228100131205           fior97ds.IOR97dao = dataoggi;
228200131205         ELSE;
228300131205           fior97ds.IOR97dao = fiora0I0.vaodao;
228400131205         ENDIF;
228500131205         IF  fiora0I0.vaooao = 0;
228600131205           fior97ds.IOR97oao = %dec(%time());
228700131205         ELSE;
228800131205           fior97ds.IOR97oao = fiora0I0.vaooao;
228900131205         ENDIF;
229000131205         fior97ds.IOR97gf2 = tisi95ds.O95gf2;
229100131205         IF  wCom;
229200160714           fior97ds.IOR97com = FIORA00_SI;
229300131205         ENDIF;
229400140331         SELECT;
229500140331           WHEN fiora0O0.prepagato = FIORA00_PREPAGATO;
229600160714             fior97ds.IOR97tor = FIORA00_ORM_BOLLAPREPAGATO;
229700140331           WHEN fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
229800160714             fior97ds.IOR97tor = FIORA00_ORM_INTERNET;
229900140331           OTHER;
230000160714             fior97ds.IOR97tor = FIORA00_ORM_MANUALE;
230100140331         ENDSL;
230200131205         IF  fiora0I0.vaocra > 0;
230300131205           fior97ds.IOR97mcod = 'S';
230400131205         ENDIF;
230500160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
230600131205         fior97ds.IOR97web = 'S';
230700160315         ENDIF;
230800160601
230900160601         //?se ORM manuale e ci sono i dati per Alert Commissionato
231000160601         //?e l'ORM è un commissionato passo il flag
231100160601         IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
231200160601              fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and wCom and
231300160601             (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks);
231400160714           fior97ds.IOR97alert = FIORA00_SI;
231500160601         ENDIF;
231600160601
231700131205         fior97r(kpjba : fior97ds);
231800160601
231900131205         IF  fior97ds.OOR97err = *blanks and fior97ds.OOR97dar <> *zeros;
232000131220           inv_dataritiro = fior97ds.OOR97dar;
232100131205         ENDIF;
232200131205
232300160610         //?Se richiamato da INTERNET
232400160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
232500131220         //?se non è stata immessa imposto quella calcolata
232600131220         IF  fiora0I0.vaodar = 0;
232700131220           fiora0O0.vaodar = inv_dataritiro;
232800131220         ELSE;
232900131220           fiora0O0.vaodar = fiora0I0.vaodar;
233000131220         ENDIF;
233100131223
233200131223         //?Controlli sulla data
233300131223         //?Non accetto a 0
233400131223         IF  fiora0O0.vaodar = 0;
233500140407           wIdMsg   = 'TIS0259';
233600131223           wIdCampo = 'VAODAR';
233700131223           clear wParm;
233800131223           exsr Messaggi;
233900131223           leavesr;
234000131223         ENDIF;
234100131223         //?Non deve essere inferiore alla data minima
234200131223         IF  fiora0O0.vaodar < fior97ds.OOR97dmin;
234300160608           //?Se richiamato da AS400 messaggio di errore diverso
234400160608           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
234500160608             wIdMsg   = 'TIS0842';
234600160608           ELSE;
234700140407           wIdMsg   = 'TIS0730';
234800160608           ENDIF;
234900131223           wIdCampo = 'VAODAR';
235000140407           IF  wCom;
235100140407             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
235200140407           ELSE;
235300140407             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
235400140407           ENDIF;
235500160526           IF  fiora0I0.vaonar <> *blanks;
235600160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
235700160526           ENDIF;
235800131223           exsr Messaggi;
235900131223         ENDIF;
236000131223         //?Non deve essere superiore alla data massima
236100131223         IF  fiora0O0.vaodar > fior97ds.OOR97dmaxb;
236200131223           wIdMsg   = 'TIS0116';
236300131223           wIdCampo = 'VAODAR';
236400131223           clear wParm;
236500131223           exsr Messaggi;
236600131223         ENDIF;
236700160610         ENDIF;
236800160610
236900160610       //?Se richiamato da AS400
237000160610         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
237100160610         //?Se sono in immissione ORM
237200160610           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
237300160610           //?se posso anticipare la data passo il flag
237400160610             IF  fior97ds.OOR97dmin < fior97ds.OOR97dar;
237500160714               fiora0O0.anticipa = FIORA00_ANTICIPA;
237600160610             ENDIF;
237700160610             //?Se non passata imposto la data calcolata
237800160610             IF  fiora0I0.vaodar = 0;
237900160610               fiora0I0.vaodar = inv_dataritiro;
238000160610             ENDIF;
238100160610           ENDIF;
238200160610           //?Non deve essere a 0
238300160610           IF  fiora0I0.vaodar = 0;
238400160610             wIdMsg   = 'TIS0259';
238500160610             wIdCampo = 'VAODAR';
238600160610             clear wParm;
238700160610             exsr Messaggi;
238800160610             leavesr;
238900160610           ENDIF;
239000160610           //?Non deve essere inferiore alla data minima
239100160610           IF  fiora0I0.vaodar < fior97ds.OOR97dmin;
239200160610             wIdMsg   = 'TIS0842';
239300160610             wIdCampo = 'VAODAR';
239400160610             IF  wCom;
239500160610               wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
239600160610             ELSE;
239700160610               wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
239800160610             ENDIF;
239900160610             IF  fiora0I0.vaonar <> *blanks;
240000160610               wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
240100160610             ENDIF;
240200160610             exsr Messaggi;
240300160610           ENDIF;
240400160610           //?Non deve essere superiore alla data massima
240500160610           IF  fiora0I0.vaodar > fior97ds.OOR97dmaxb;
240600160610             wIdMsg   = 'TIS0116';
240700160610             wIdCampo = 'VAODAR';
240800160610             clear wParm;
240900160610             exsr Messaggi;
241000160610           ENDIF;
241100160610           //?In caso di richiamo in WRITE (Immissione/Copia ORM)
241200160610           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
241300160610             //?Se posso anticipare ma la data ritiro immessa è = alla
241400160610             //?data minima pulisco il flag di anticipa
241500160714             IF  fiora0O0.anticipa = FIORA00_ANTICIPA and
241600160610                 fior97ds.OOR97dmin = fiora0I0.vaodar;
241700160610               clear fiora0O0.anticipa;
241800160610             ENDIF;
241900160610             fiora0O0.vaodar = fiora0I0.vaodar;
242000160610           //?Messaggio di avvertimento data ritiro calcolata <> da quella del video
242100160610             IF  inv_dataritiro <> fiora0O0.vaodar;
242200160610               wIdMsg   = 'TIS0843';
242300160610               wIdCampo = 'VAODAR';
242400160610               clear wParm;
242500160610               exsr Forzatura;
242600160610               IF  wForza;
242700160610                 exsr RoutEnd;
242800160610               ENDIF;
242900160610             ENDIF;
243000160610           ENDIF;
243100160610           fiora0O0.vaodar = fiora0I0.vaodar;
243200160610         ENDIF;
243300131205
243400131205       ENDSR;
243500131011
243600131011       //--------------------------------------------------------------
243700131011       //?Controllo ora pronta merce.
243800131011       //--------------------------------------------------------------
243900131011       BEGSR  OraRitiro;
244000131219
244100131223         IF  fiora0I0.vaoorr > 0;
244200131223           fiora0O0.vaoorr = fiora0I0.vaoorr;
244300131223         ENDIF;
244400131028
244500140407         IF  fiora0O0.vaoorr = *zeros and not wCom;
244600131028           wIdMsg   = 'TIS0423';
244700131028           wIdCampo = 'VAOORR';
244800131204           clear wParm;
244900131028           exsr Messaggi;
245000131219           leavesr;
245100131028         ENDIF;
245200131028
245300140407         IF  wCom and fiora0O0.vaoorr = 0;
245400131220           leavesr;
245500131220         ENDIF;
245600131220
245700131223         IF  fiora0O0.vaoorr < 1 or fiora0O0.vaoorr > 2359;
245800131028           wIdMsg   = 'TIS0425';
245900131028           wIdCampo = 'VAOORR';
246000131204           clear wParm;
246100131028           exsr Messaggi;
246200131028         ENDIF;
246300160404
246400160404       ENDSR;
246500160404
246600160404       //--------------------------------------------------------------
246700160404       //?Controllo ora pronta merce con orari servizio.
246800160404       //--------------------------------------------------------------
246900160404       BEGSR  OraRitiroServizio;
247000140306
247100140306       //?Il controllo dell'ora pronta merce non lo faccio se ritiro estero
247200140306         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
247300140306           leavesr;
247400140306         ENDIF;
247500131220
247600131220       //?Controllo ora pronta merce se non è presa da anagrafica clienti
247700131220         SELECT;
247800140402           WHEN  fiorb0OR.oraritiro > 0 and
247900140402                 fiora0O0.vaoorr = fiorb0OR.oraritiro;
248000131220           OTHER;
248100131220       //?Controllo ora pronta merce con ora fine standard
248200140331           IF  fiora0O0.vaoorr > trulor2ds.OOR2stfs;
248300140306             wIdMsg   = 'TIS0724';
248400131223             wIdCampo = 'VAOORR';
248500140331             wParm = %editw(trulor2ds.OOR2stis:'  :  ') +
248600140331                     %editw(trulor2ds.OOR2stfs:'  :  ');
248700131223             exsr Messaggi;
248800131223           ENDIF;
248900131220         ENDSL;
249000131220
249100131220       //?Controllo ora immissione ORM con ora massima ritiro
249200140306         IF  %dec(%subst(%editc(fiora0I0.vaooao:'X'):1:4):4:0)
249300140410             > trulor2ds.OOR2stfs and
249400140306             fiora0I0.vaodao = fiora0O0.vaodar;
249500140306           wIdMsg   = 'TIS0721';
249600140306           wIdCampo = 'VAOOAO';
249700140327           wParm = %editw(trulor2ds.OOR2stfs:'  :  ');
249800131223           exsr Messaggi;
249900131220         ENDIF;
250000131011
250100131011       ENDSR;
250200131028
250300131028       //--------------------------------------------------------------
250400131028       //?Controllo orari apertura.
250500131028       //--------------------------------------------------------------
250600140404       BEGSR  OrariApertura;
250700131028
250800140401         IF  fiora0I0.dalleam > 0 or fiora0I0.alleam > 0 or
250900140401             fiora0I0.dallepm > 0 or fiora0I0.allepm > 0;
251000131204           clear TRUL03ds;
251100140401           trul03ds.I03hm1 = fiora0I0.dalleam;
251200140401           trul03ds.I03hm2 = fiora0I0.alleam;
251300140401           trul03ds.I03hm3 = fiora0I0.dallepm;
251400140401           trul03ds.I03hm4 = fiora0I0.allepm;
251500131204           trul03r (TRUL03DS);
251600131204           IF  trul03ds.O03err <> *off;
251700131204             wIdMsg   = 'TIS0171';
251800131204             SELECT;
251900131204               WHEN  trul03ds.O03errpos = 1;
252000131204                 wIdCampo = 'DALLEAM';
252100131204                 clear wParm;
252200131204                 exsr Messaggi;
252300131204               WHEN  trul03ds.O03errpos = 2;
252400131204                 wIdCampo = 'ALLEAM';
252500131204                 clear wParm;
252600131204                 exsr Messaggi;
252700131204               WHEN  trul03ds.O03errpos = 3;
252800131204                 wIdCampo = 'DALLEPM';
252900131204                 clear wParm;
253000131204                 exsr Messaggi;
253100131204               WHEN  trul03ds.O03errpos = 4;
253200131204                 wIdCampo = 'ALLEPM';
253300131204                 clear wParm;
253400131204                 exsr Messaggi;
253500131204             ENDSL;
253600131204           ENDIF;
253700131204         ENDIF;
253800131028
253900131028       ENDSR;
254000131028
254100131028       //--------------------------------------------------------------
254200131028       //?Controllo quantità.
254300131028       //--------------------------------------------------------------
254400131028       BEGSR  Quantita;
254500160613
254600160613         wErrQta = *off;
254700140410
254800140410       //?Se Prepagato obbligo di colli e peso
254900140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
255000140410             fiora0I0.vaoncl = 0;
255100140410           wIdMsg   = 'TIS0726';
255200140410           wIdCampo = 'VAONCL';
255300140410           clear wParm;
255400140410           exsr Messaggi;
255500160613           wErrQta = *on;
255600140410         ENDIF;
255700140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
255800140410             fiora0I0.vaopkg = 0;
255900140410           wIdMsg   = 'TIS0727';
256000140410           wIdCampo = 'VAOPKG';
256100140410           clear wParm;
256200140410           exsr Messaggi;
256300160613           wErrQta = *on;
256400140410         ENDIF;
256500140410
256600140410       //?Se richiesto il ritiro tramite DPD
256700140410       //?il peso è obbligatorio e deve essere inferiore al limite da tab. 3I
256800160714         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
256900140410           IF  fiora0I0.vaopkg = 0;
257000140410             wIdMsg   = 'TIS0727';
257100140410             wIdCampo = 'VAOPKG';
257200140410             clear wParm;
257300140410             exsr Messaggi;
257400160613             wErrQta = *on;
257500140410           ENDIF;
257600140410           IF  fiora0I0.vaopkg > 0 and ds3idp.§3Ipkd > 0 and
257700140410               fiora0I0.vaopkg > ds3idp.§3Ipkd;
257800140410             wIdMsg   = 'TIS0694';
257900140410             wIdCampo = 'VAOPKG';
258000140410             wParm = %editc(ds3idp.§3Ipkd:'3');
258100140410             exsr Messaggi;
258200160613             wErrQta = *on;
258300140410           ENDIF;
258400140410       //?i colli sono obbligatori e deve essere impostato a 1
258500140410           IF  fiora0I0.vaoncl <> 1;
258600140410             wIdMsg   = 'TIS0693';
258700140410             wIdCampo = 'VAONCL';
258800140410             clear wParm;
258900140410             exsr Messaggi;
259000160613             wErrQta = *on;
259100140410           ENDIF;
259200140410         ENDIF;
259300140410
259400140410       //?Se Prepagato finisco qua i controlli
259500140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO;
259600140410           leavesr;
259700140410         ENDIF;
259800160530
259900160601       //?Faccio controlli diversificati tra Internet ed AS400
260000140331
260100160601       //?Controlli per richiamo da internet
260200160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
260300160601
260400140331       //?Almeno 1 tra colli - bancali deve essere presente
260500140401         IF  fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
260600140331           wIdMsg   = 'TIS0729';
260700140331           wIdCampo = 'VAONCL';
260800140331           clear wParm;
260900140331           exsr Messaggi;
261000140331         ENDIF;
261100140331
261200140331       //?Peso obbligatorio
261300140429         IF  fiora0I0.vaopkg = 0;
261400140429           wIdMsg   = 'TIS0727';
261500140429           wIdCampo = 'VAOPKG';
261600140429           clear wParm;
261700140429           exsr Messaggi;
261800140429         ENDIF;
261900131204
262000131204       //?Controllo con i flag su Anagrafica clienti ritiro
262100140331       //?Per internet facciamo solo i controlli di 'O' obbligatorio
262200160714           IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO and
262300160714               fiora0I0.vaoncl = 0;
262400140331             wIdMsg   = 'TIS0726';
262500140331             wIdCampo = 'VAONCL';
262600140331             clear wParm;
262700140331             exsr Messaggi;
262800140331           ENDIF;
262900140331
263000160714           IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO and
263100160714               fiora0I0.vaopkg = 0;
263200140331             wIdMsg   = 'TIS0727';
263300140331             wIdCampo = 'VAOPKG';
263400140331             clear wParm;
263500140331             exsr Messaggi;
263600140331           ENDIF;
263700140331
263800160714           IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO and
263900160714               fiora0I0.vaovlm = 0;
264000140331             wIdMsg   = 'TIS0728';
264100140331             wIdCampo = 'VAOVLM';
264200140331             clear wParm;
264300140331             exsr Messaggi;
264400140331           ENDIF;
264500140331
264600160714           IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO and
264700160714               fiora0I0.vaobnc = 0;
264800140331             wIdMsg   = 'TIS0722';
264900140331             wIdCampo = 'VAOBNC';
265000140331             clear wParm;
265100140331             exsr Messaggi;
265200140331           ENDIF;
265300160601
265400140331         ENDIF;
265500160601
265600160601       //?Controlli Se richiamato da AS400
265700160601       //?faccio prima i controlli con i flag dell'anagrafica mittente
265800160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
265900160601         //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
266000160601         //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
266100160601           IF  fiorb0OR.flagfncl <> *blanks and fiora0I0.vaoncl = 0;
266200160601             wIdMsg   = 'TIS0726';
266300160601             wIdCampo = 'VAONCL';
266400160601             clear wParm;
266500160714             IF  fiorb0OR.flagfncl = FIORA00_FORZABILE;
266600160601               exsr Forzatura;
266700160601               IF  wForza;
266800160601                 exsr RoutEnd;
266900160601               ENDIF;
267000160601             ENDIF;
267100160714             IF  fiorb0OR.flagfncl = FIORA00_OBBLIGATORIO;
267200160601               exsr Messaggi;
267300160613               wErrQta = *on;
267400160613               leavesr;
267500160601             ENDIF;
267600160601           ENDIF;
267700160601           IF  fiorb0OR.flagfpkg <> *blanks and fiora0I0.vaopkg = 0;
267800160601             wIdMsg   = 'TIS0727';
267900160601             wIdCampo = 'VAOPKG';
268000160601             clear wParm;
268100160714             IF  fiorb0OR.flagfpkg = FIORA00_FORZABILE;
268200160601               exsr Forzatura;
268300160601               IF  wForza;
268400160601                 exsr RoutEnd;
268500160601               ENDIF;
268600160601             ENDIF;
268700160714             IF  fiorb0OR.flagfpkg = FIORA00_OBBLIGATORIO;
268800160601               exsr Messaggi;
268900160613               wErrQta = *on;
269000160613               leavesr;
269100160601             ENDIF;
269200160601           ENDIF;
269300160601           IF  fiorb0OR.flagfvlm <> *blanks and fiora0I0.vaovlm = 0;
269400160601             wIdMsg   = 'TIS0728';
269500160601             wIdCampo = 'VAOVLM';
269600160601             clear wParm;
269700160714             IF  fiorb0OR.flagfvlm = FIORA00_FORZABILE;
269800160601               exsr Forzatura;
269900160601               IF  wForza;
270000160601                 exsr RoutEnd;
270100160601               ENDIF;
270200160601             ENDIF;
270300160714             IF  fiorb0OR.flagfvlm = FIORA00_OBBLIGATORIO;
270400160601               exsr Messaggi;
270500160613               wErrQta = *on;
270600160613               leavesr;
270700160601             ENDIF;
270800160601           ENDIF;
270900160601           IF  fiorb0OR.flagfbnc <> *blanks and fiora0I0.vaobnc = 0;
271000160601             wIdMsg   = 'TIS0722';
271100160601             wIdCampo = 'VAOBNC';
271200160601             clear wParm;
271300160714             IF  fiorb0OR.flagfbnc = FIORA00_FORZABILE;
271400160601               exsr Forzatura;
271500160601               IF  wForza;
271600160601                 exsr RoutEnd;
271700160601               ENDIF;
271800160601             ENDIF;
271900160714             IF  fiorb0OR.flagfbnc = FIORA00_OBBLIGATORIO;
272000160601               exsr Messaggi;
272100160613               wErrQta = *on;
272200160613               leavesr;
272300160601             ENDIF;
272400160601           ENDIF;
272500160601         //?In caso di Immissione/Copia ORM
272600160601         //?immissione Manuale
272700160601           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
272800160714              (fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL or
272900160714               fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO) and
273000160601           //?Almeno 1 tra colli - bancali deve essere presente
273100160601               fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
273200160601             wIdMsg   = 'TIS0729';
273300160601             wIdCampo = 'VAONCL';
273400160601             clear wParm;
273500160601             exsr Messaggi;
273600160613             wErrQta = *on;
273700160613             leavesr;
273800160601           ENDIF;
273900160601         //?Almeno 1 tra peso volume o bancali deve essere presente
274000160610           IF  fiora0I0.vaobnc = 0 and fiora0I0.vaovlm = 0 and
274100160601               fiora0I0.vaopkg = 0;
274200160601             wIdMsg   = 'TIS0235';
274300160601             wIdCampo = 'VAOPKG';
274400160601             clear wParm;
274500160601             exsr Messaggi;
274600160613             wErrQta = *on;
274700160613             leavesr;
274800160601           ENDIF;
274900160601         ENDIF;
275000131028
275100131028       ENDSR;
275200151008
275300151008       //--------------------------------------------------------------
275400151008       //?Calcolo peso/volume più alto per instradamento.
275500151008       //--------------------------------------------------------------
275600151008       BEGSR  PesoVolume;
275700160606
275800160606       //?Sviluppo il peso e/o volume per calcolo instradamento
275900160606         clear wPesomax;
276000160606         clear wVolumemax;
276100160606         clear wPesoaut;
276200160606         clear wVolumeaut;
276300160606         clear wPesoblc;
276400160606         clear wVolumeblc;
276500160606         clear wPesomtc;
276600160606         clear wVolumemtc;
276700151008
276800151008         wPeso = fiora0I0.vaopkg;
276900151008         wVolume = fiora0I0.vaovlm;
277000160606
277100160606       //?se non inserito il peso lo sviluppo
277200160606         IF  wPeso = 0;
277300160606           SELECT;
277400160606           WHEN  fiora0I0.vaobnc > 0;
277500160606             eval(h) wPesomax =
277600160606                 fiora0I0.vaobnc / dDFT.d§DFTbnc * dDFT.d§DFTpkg;
277700160606           WHEN  fiora0I0.vaovlm > 0;
277800160606             eval(h) wPesomax = fiora0I0.vaovlm * dDFT.d§DFTpkg;
277900160606           ENDSL;
278000160606       //?se peso troppo alto imposto il massimo consentito
278100160606           IF  wPesomax > 999999,9;
278200160606             wPeso = 999999,9;
278300160606           ELSE;
278400160606             wPeso = wPesomax;
278500160606           ENDIF;
278600160606         ENDIF;
278700151008
278800160606       //?se non inserito il volume lo sviluppo
278900151008         IF  wVolume = 0;
279000151008           SELECT;
279100151008           WHEN  fiora0I0.vaobnc > 0;
279200151008             eval(h) wVolumemax = fiora0I0.vaobnc / dDFT.d§DFTbnc;
279300151008           WHEN  fiora0I0.vaopkg > 0;
279400151008             eval(h) wVolumemax = fiora0I0.vaopkg / dDFT.d§DFTpkg;
279500151008           ENDSL;
279600151008       //?se volume troppo alto imposto il massimo consentito
279700151008           IF  wVolumemax > 99,999;
279800151008             wVolume = 99,990;
279900151008           ELSE;
280000151008             wVolume = wVolumemax;
280100151008           ENDIF;
280200151008         ENDIF;
280300160606
280400160606       //?Se richiamato da AS400 ho altre quantità che posso inserire
280500160606       //?quindi se inserite le sviluppo
280600160606       //?sono quantità da sviluppare in m³
280700160606         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
280800160606           IF  fiora0I0.vaoatt > 0;
280900160606             eval(h) wVolumeaut = fiora0I0.vaoatt * dDFT.d§DFTaut;
281000160606             eval(h) wPesoaut = wVolumeaut * dDFT.d§DFTpkg;
281100160606           ENDIF;
281200160606           IF  fiora0I0.vaomtc > 0;
281300160606             eval(h) wVolumemtc = fiora0I0.vaomtc * dDFT.d§DFTmtc;
281400160606             eval(h) wPesomtc = wVolumemtc * dDFT.d§DFTpkg;
281500160606           ENDIF;
281600160606           IF  fiora0I0.vaoblc > 0;
281700160606             eval(h) wVolumeblc = fiora0I0.vaoblc * dDFT.d§DFTblc;
281800160606             eval(h) wPesoblc = wVolumeblc * dDFT.d§DFTpkg;
281900160606           ENDIF;
282000160606           IF  wVolumeaut > 99,999;
282100160606             wVolumeaut = 99,999;
282200160606           ENDIF;
282300160606           IF  wPesoaut > 999999,9;
282400160606             wPesoaut = 999999,9;
282500160606           ENDIF;
282600160606           IF  wVolumemtc > 99,999;
282700160606             wVolumemtc = 99,999;
282800160606           ENDIF;
282900160606           IF  wPesomtc > 999999,9;
283000160606             wPesomtc = 999999,9;
283100160606           ENDIF;
283200160606           IF  wVolumeblc > 99,999;
283300160606             wPesoblc = 999999,9;
283400160606           ENDIF;
283500160606           IF  wPesoblc > 999999,9;
283600160606             wVolumeblc = 99,999;
283700160606           ENDIF;
283800160606         ENDIF;
283900160606
284000160606       //?Dalla versione 'D' della ds di output FIORA0O0
284100160606       //?torno il peso e volume da memorizzare su FNORG
284200160728       //?solo per AS400
284300160728         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
284400160606         IF  fiora0O0.versione > 'C';
284500160615           fiora0O0.volume = wVolume;
284600160615           fiora0O0.peso = wPeso;
284700160606           IF  wVolumeaut > wVolume;
284800160615             fiora0O0.volume = wVolumeaut;
284900160606           ENDIF;
285000160606           IF  wPesoaut > wPeso;
285100160615             fiora0O0.peso = wPesoaut;
285200160606           ENDIF;
285300160606           IF  wVolumemtc > wVolume;
285400160615             fiora0O0.volume = wVolumemtc;
285500160606           ENDIF;
285600160606           IF  wPesomtc > wPeso;
285700160615             fiora0O0.peso = wPesomtc;
285800160606           ENDIF;
285900160606           IF  wVolumeblc > wVolume;
286000160615             fiora0O0.volume = wVolumeblc;
286100160606           ENDIF;
286200160606           IF  wPesoblc > wPeso;
286300160615             fiora0O0.peso = wPesoblc;
286400160606           ENDIF;
286500160615           wVolume = fiora0O0.volume;
286600160615           wPeso = fiora0O0.peso;
286700160606         ENDIF;
286800160728         ENDIF;
286900151008
287000151008       ENDSR;
287100131204
287200131204       //--------------------------------------------------------------
287300131204       //?Controllo la natura della merce.
287400131204       //--------------------------------------------------------------
287500131204       BEGSR  NaturaMerce;
287600131204
287700140404       //?Se richiamato da internet
287800140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
287900140411       //?Obbligo della natura merce se mittente non codificato
288000140416       //?                         e se destinatario non codificato
288100140416             fiora0I0.vaonam = *blanks and fiora0I0.vaocra = *zeros and
288200140416             fiora0I0.vaocrc = *zeros;
288300140411           wIdMsg   = 'TIS0143';
288400140411           wIdCampo = 'VAONAM';
288500140411           clear wParm;
288600140411           exsr Messaggi;
288700140411           leavesr;
288800140407         ENDIF;
288900140211
289000140211         IF  fiora0I0.vaonam <> *blanks;
289100140211           exec sql
289200140211           set :fiora0O0.vaonam = ucase (:fiora0I0.vaonam);
289300131223         ENDIF;
289400131204
289500131204       ENDSR;
289600131204
289700131204       //--------------------------------------------------------------
289800131204       //?Controllo uno o più destinatari.
289900131204       //--------------------------------------------------------------
290000131204       BEGSR  UnoPiuDest;
290100131220
290200131220         fiora0O0.unopiudest = fiora0I0.unopiudest;
290300160607
290400160607       //?I controlli sucessivi li devo fare solo se richiamato da Internet
290500160607         IF  fiora0I0.idrichiamo <> FIORA00_ID_RICHIAMO_INTERNET;
290600160607           leavesr;
290700160607         ENDIF;
290800131204
290900131204       //?Accetto 1 o 2
291000160714         IF  fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO and
291100160714             fiora0O0.unopiudest <> FIORA00_PIUDESTINATARI;
291200131204           wIdMsg   = 'TIS0295';
291300131204           wIdCampo = 'UNOPIUDEST';
291400131204           clear wParm;
291500131204           exsr Messaggi;
291600131219           leavesr;
291700131204         ENDIF;
291800131204
291900131204       //?Se ORM prepagato SOLO 1 destinatario
292000131204         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
292100160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
292200131204           wIdMsg   = 'TIS0307';
292300131204           wIdCampo = 'UNOPIUDEST';
292400131204           clear wParm;
292500131204           exsr Messaggi;
292600131204         ENDIF;
292700131204
292800131204       //?Se richiesto il ritiro tramite DPD SOLO 1 destinatario
292900131204         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
293000160714             fiora0O0.unopiudest <> FIORA00_UNDESTINATARIO;
293100131204           wIdMsg   = 'TIS0307';
293200131204           wIdCampo = 'UNOPIUDEST';
293300131204           clear wParm;
293400131204           exsr Messaggi;
293500131204         ENDIF;
293600131204
293700131204       ENDSR;
293800131205
293900131205       //--------------------------------------------------------------
294000131205       //?Controllo Referente.
294100131205       //--------------------------------------------------------------
294200131205       BEGSR  Referente;
294300131205
294400131205       //?Se mittente non codificato oppure ORM commissionato
294500131205       //?il referente è obbligatorio
294600140401         IF  (fiora0I0.vaocra = 0 or wCom) and
294700131223              fiora0I0.vaorer = *blanks;
294800131205           wIdMsg   = 'TIS0290';
294900131205           wIdCampo = 'VAORER';
295000131205           clear wParm;
295100131205           exsr Messaggi;
295200131205         ENDIF;
295300160601
295400160613       //?se Richiamato da AS400
295500160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
295600160613         //?Referente obbligatorio
295700160613         //?se Prepagato
295800160613           IF   fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
295900160613                fiora0I0.vaorer = *blanks;
296000160613             wIdMsg   = 'TIS0290';
296100160613             wIdCampo = 'VAORER';
296200160613             clear wParm;
296300160613             exsr Messaggi;
296400160613           ENDIF;
296500160613         ENDIF;
296600160613
296700131223         IF  fiora0I0.vaorer <> *blanks;
296800140211           exec sql
296900140211           set :fiora0O0.vaorer = ucase (:fiora0I0.vaorer);
297000131223         ENDIF;
297100131205
297200131205       ENDSR;
297300131223
297400131223       //--------------------------------------------------------------
297500131223       //?Controllo Telefono.
297600131223       //--------------------------------------------------------------
297700131223       BEGSR  Telefono;
297800131223
297900131223       //?Se mittente non codificato oppure ORM commissionato
298000131223       //?il telefono è obbligatorio
298100140401         IF  (fiora0I0.vaocra = 0 or wCom) and
298200131223              fiora0I0.vaoter = *blanks;
298300131223           wIdMsg   = 'TIS0246';
298400131223           wIdCampo = 'VAOTER';
298500131223           clear wParm;
298600131223           exsr Messaggi;
298700131223         ENDIF;
298800160601
298900160601       //?se Richiamato da AS400
299000160613         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
299100160613         //?Telefono obbligatorio
299200160613         //?se Prepagato
299300160613           IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
299400160613               fiora0I0.vaoter = *blanks;
299500160613             wIdMsg   = 'TIS0246';
299600160613             wIdCampo = 'VAOTER';
299700160613             clear wParm;
299800160613             exsr Messaggi;
299900160613           ENDIF;
300000160601         ENDIF;
300100131223
300200131223         IF  fiora0I0.vaoter <> *blanks;
300300140211           exec sql
300400140211           set :fiora0O0.vaoter = ucase (:fiora0I0.vaoter);
300500131223         ENDIF;
300600131223
300700131223       ENDSR;
300800140311
300900140311       //--------------------------------------------------------------
301000140311       //?Controllo indirizzo mail.
301100140311       //--------------------------------------------------------------
301200140311       BEGSR  Mail;
301300140311
301400140311         IF  fiora0I0.mail <> *blanks;
301500140311           clear dsemail;
301600140311           dsemail.§emlindi = fiora0I0.mail;
301700140311           xemail (dsemail);
301800140311           IF  dsemail.§emlerro <> '0';
301900140424             wIdMsg   = 'TIS0732';
302000140311             wIdCampo = 'MAIL';
302100140424             clear wParm;
302200140311             exsr Messaggi;
302300140311             leavesr;
302400140311           ENDIF;
302500140311
302600140424           fiora0O0.mail = fiora0I0.mail;
302700140722           IF  dsemail.§emlindo <> *blanks;
302800140722             fiora0O0.mail = dsemail.§emlindo;
302900140722           ENDIF;
303000140311         ENDIF;
303100140311
303200140311       ENDSR;
303300140311
303400140311       //--------------------------------------------------------------
303500140311       //?Controllo n.cellulare.
303600140311       //--------------------------------------------------------------
303700140311       BEGSR  Sms;
303800140311
303900140311         IF  fiora0I0.sms <> *blanks;
304000140311           pInCell = %trim(fiora0I0.sms);
304100140311           clear pOutErr;
304200140722           clear pOutCell;
304300140311           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
304400140311             wIdMsg   = 'TIS0725';
304500140311             wIdCampo = 'SMS';
304600140311             clear wParm;
304700140311             exsr Messaggi;
304800160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
304900160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
305000160601             fiora0I0.sms;
305100140311             leavesr;
305200140311           ENDIF;
305300140311
305400140416           fiora0O0.sms = fiora0I0.sms;
305500140722           IF  pOutCell <> *blanks;
305600140722             fiora0O0.sms = pOutCell;
305700140722           ENDIF;
305800140311         ENDIF;
305900140311
306000140311       ENDSR;
306100140313
306200140313       //--------------------------------------------------------------
306300140313       //?Controllo Codice Destinatario.
306400140313       //--------------------------------------------------------------
306500140313       BEGSR  CodDestinatario;
306600140401
306700140401         wCodice = fiora0I0.vaocrc;
306800140402         reset FIORB0OC;
306900140313
307000140313       //?Se presente codice destinatario verifico se valido
307100160607       //?e se legato al codice di LOG solo se da Internet
307200140402         IF  not IsCodiceValido(FIORB0OC);
307300140313           wIdMsg   = 'TIS0066';
307400140313           wIdCampo = 'VAOCRC';
307500140313           clear wParm;
307600140313           exsr Messaggi;
307700140313           leavesr;
307800140313         ENDIF;
307900160607
308000160607       //?Solo per richiamo da AS400
308100160607       //?e solo dalla versione 'B' in poi
308200160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
308300160607             fiorb0OC.versione > 'A';
308400160607         //?codice bloccato
308500160714           IF  fiorb0OC.frequenza = FIORA00_FREQUENZA_RITIRO_MAI;
308600160607             wIdMsg   = 'TIS0800';
308700160607             wIdCampo = 'VAOCRC';
308800160607             clear wParm;
308900160607             exsr Messaggi;
309000160607           ENDIF;
309100160607         ENDIF;
309200140313
309300160607       //?Se richiamato da Internet
309400160607         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
309500140313       //?controllo se fa parte della stessa famiglia del codice di LOG
309600140313         IF not IsCodiceLegato();
309700140313           wIdMsg   = 'TIS0066';
309800140313           wIdCampo = 'VAOCRC';
309900140313           clear wParm;
310000140313           exsr Messaggi;
310100140313           leavesr;
310200140313         ENDIF;
310300160607         ENDIF;
310400160607
310500140313
310600140313       ENDSR;
310700131205
310800131205       //--------------------------------------------------------------
310900131205       //?Controllo Destinatario.
311000131205       //--------------------------------------------------------------
311100131205       BEGSR  Destinatario;
311200131220
311300140211         exec sql
311400140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
311500131205
311600131205       //?Se richiesto UN solo destinatario il dato è obbligatorio
311700160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
311800131205            (fiora0I0.vaorsc = *blanks or fiora0I0.vaoinc = *blanks or
311900131205             fiora0i0.vaoloc = *blanks or fiora0i0.vaocac = *blanks);
312000131205           wIdMsg   = 'TIS0288';
312100131205           wIdCampo = 'VAORSC';
312200131205           clear wParm;
312300131205           exsr Messaggi;
312400131220           leavesr;
312500131205         ENDIF;
312600131205
312700131205       //?Se richiesto più destinatari il dato NON è da inserire
312800160714         IF  fiora0I0.unopiudest = FIORA00_PIUDESTINATARI and
312900131205            (fiora0I0.vaorsc <> *blanks or fiora0I0.vaoinc <> *blanks or
313000131205             fiora0i0.vaoloc <> *blanks or fiora0i0.vaocac <> *blanks);
313100131205           wIdMsg   = 'TIS0380';
313200131205           wIdCampo = 'VAORSC';
313300131205           clear wParm;
313400131205           exsr Messaggi;
313500131220           leavesr;
313600131205         ENDIF;
313700131220
313800131220       //?Controllo nominativo destinatario
313900131220         exsr NomeDestinatario;
314000131220
314100131220       //?Controllo indirizzo destinatario
314200131220         exsr ViaDestinatario;
314300131220
314400131220       //?Controllo località destinatario
314500131220         exsr LocDestinatario;
314600131220
314700131220       //?Controllo cap destinatario
314800131220         exsr CapDestinatario;
314900131220
315000131220       //?Controllo provincia destinatario
315100131220         exsr ProvDestinatario;
315200131220
315300131220       //?Controllo nazione destinatario
315400131220         exsr NazDestinatario;
315500131220
315600131220       //?Controllo indirizzo completo del destinatario
315700131220         exsr IndTotDestinatario;
315800131205
315900131205       ENDSR;
316000131205
316100131205       //--------------------------------------------------------------
316200131205       //?Controllo nominativo destinatario.
316300131205       //--------------------------------------------------------------
316400131205       BEGSR  NomeDestinatario;
316500131205
316600131205       //?Se richiesto UN solo destinatario deve essere indicato
316700160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
316800131205             fiora0I0.vaorsc = *blanks;
316900131205           wIdMsg   = 'TIS0480';
317000131205           wIdCampo = 'VAORSC';
317100131205           clear wParm;
317200131205           exsr Messaggi;
317300131205         ENDIF;
317400131205
317500140211         exec sql
317600140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
317700131205
317800131205       ENDSR;
317900131205
318000131205       //--------------------------------------------------------------
318100131205       //?Controllo indirizzo destinatario.
318200131205       //--------------------------------------------------------------
318300131205       BEGSR  ViaDestinatario;
318400131205
318500131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
318600131220       //?deve essere indicata anche la via
318700160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
318800160714              fiora0I0.vaorsc <> *blanks) and
318900131220              fiora0I0.vaoinc = *blanks;
319000131205           wIdMsg   = 'TIS0281';
319100131205           wIdCampo = 'VAOINC';
319200131205           clear wParm;
319300131205           exsr Messaggi;
319400131205         ENDIF;
319500131205
319600140211         exec sql
319700140211         set :fiora0O0.vaoinc = ucase (:fiora0I0.vaoinc);
319800131205
319900131205       ENDSR;
320000131205
320100131205       //--------------------------------------------------------------
320200131205       //?Controllo cap destinatario.
320300131205       //--------------------------------------------------------------
320400131205       BEGSR  CapDestinatario;
320500131205
320600131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
320700131220       //?deve essere indicato
320800160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
320900160714              fiora0I0.vaorsc <> *blanks) and
321000131220              fiora0I0.vaocac = *blanks;
321100131205           wIdMsg   = 'TIS0051';
321200131205           wIdCampo = 'VAOCAC';
321300131205           clear wParm;
321400131205           exsr Messaggi;
321500131205         ENDIF;
321600131205
321700140211         exec sql
321800140211         set :fiora0O0.vaocac = ucase (:fiora0I0.vaocac);
321900131205
322000131205       ENDSR;
322100131205
322200131205       //--------------------------------------------------------------
322300131205       //?Controllo località destinatario.
322400131205       //--------------------------------------------------------------
322500131205       BEGSR  LocDestinatario;
322600131205
322700131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
322800131220       //?deve essere indicato
322900160714         IF  (fiora0O0.unopiudest = FIORA00_UNDESTINATARIO and
323000160714              fiora0I0.vaorsc <> *blanks) and
323100131220              fiora0I0.vaoloc = *blanks;
323200131205           wIdMsg   = 'TIS0345';
323300131205           wIdCampo = 'VAOLOC';
323400131205           clear wParm;
323500131205           exsr Messaggi;
323600131205         ENDIF;
323700131205
323800140211         exec sql
323900140211         set :fiora0O0.vaoloc = ucase (:fiora0I0.vaoloc);
324000131205
324100131205       ENDSR;
324200131009
324300131205       //--------------------------------------------------------------
324400131205       //?Controllo provincia destinatario.
324500131205       //--------------------------------------------------------------
324600131205       BEGSR  ProvDestinatario;
324700131205
324800131220       //?Controllo se richiesto UN solo destinatario
324900131220       //?o se immessa ragione sociale
325000160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
325100160714             fiora0I0.vaorsc <> *blanks;
325200131205
325300140211           exec sql
325400140211           set :fiora0O0.vaoprc = ucase (:fiora0I0.vaoprc);
325500131205
325600131205         //?Controllo con la sk delle provincie
325700131205           IF  fiora0O0.vaoprc <> *blanks and
325800131205              %lookup(fiora0O0.vaoprc:skprv) = 0;
325900131205             wIdMsg   = 'TIS0461';
326000131205             wIdCampo = 'VAOPRC';
326100131205             clear wParm;
326200131205             exsr Messaggi;
326300131205           ENDIF;
326400131205
326500131205         ENDIF;
326600131205
326700131205       ENDSR;
326800131205
326900131205       //--------------------------------------------------------------
327000131205       //?Controllo nazione destinatario.
327100131205       //--------------------------------------------------------------
327200131205       BEGSR  NazDestinatario;
327300131205
327400131205       //?Controllo se richiesto UN solo destinatario
327500131220       //?o immessa ragione sociale
327600160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
327700160714             fiora0I0.vaorsc <> *blanks;
327800131205
327900140211           exec sql
328000140211           set :fiora0O0.vaonac = ucase (:fiora0I0.vaonac);
328100131205
328200131205         //?Controllo con la sk delle provincie
328300131205           IF  fiora0O0.vaonac <> *blanks and
328400131205               %lookup(fiora0O0.vaonac:sknaz) = 0;
328500140326             wIdMsg   = 'TIS0384';
328600131205             wIdCampo = 'VAONAC';
328700131205             clear wParm;
328800131205             exsr Messaggi;
328900131205           ENDIF;
329000131205
329100131205         ENDIF;
329200131205
329300131205       ENDSR;
329400131205
329500131205       //--------------------------------------------------------------
329600131205       //?Controllo indirizzo completo del destinatario.
329700131205       //--------------------------------------------------------------
329800131205       BEGSR  IndTotDestinatario;
329900131205
330000131205       //?Controllo se richiesto UN solo destinatario
330100131220       //?o immessa ragione sociale
330200160714         IF  fiora0O0.unopiudest = FIORA00_UNDESTINATARIO or
330300160714             fiora0I0.vaorsc <> *blanks;
330400131205
330500131205           clear TISI95DS;
330600151009           clear FNLV13DS;
330700131205           tisi95ds.I95tcn = '7';
330800131205           tisi95ds.I95nar = fiora0O0.vaonac;
330900131205           tisi95ds.I95cap = fiora0O0.vaocac;
331000131205           tisi95ds.I95loc = fiora0O0.vaoloc;
331100131205           tisi95ds.I95prv = fiora0O0.vaoprc;
331200131205           tisi95ds.I95ind = fiora0O0.vaoinc;
331300131205           tisi95ds.I95dat = %dec(%date());
331400151009           fnlv13ds.I13af0 = 'S';
331500151009           fnlv13ds.I13cnv = 'S';
331600151009           fnlv13ds.I13la3 = 'S';
331700131205
331800131205         //?Solo per richiamo da Internet
331900131205           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
332000131205           //?se richiesta nazione irlanda forzo cap eire
332100131205             IF  tisi95ds.I95nar = FIORA00_IRLANDA;
332200131205               tisi95ds.I95cap =  FIORA00_EIRE;
332300131205             ENDIF;
332400131205           ENDIF;
332500131205
332600151009         //?controllo cap/località/provincia
332700151009           fnlv13r (kpjba:fnlv13ds:tisi95ds);
332800160607         //?Solo per richiamo da Internet
332900160607         //?controllo gli errori, da AS ci pensa già il programma chiamante
333000160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
333100151016         //?se torna errore per cap generico diversifico il messaggio
333200151016           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
333300151016               %scan('generico':fnlv13ds.O13msg) > 0;
333400151016             wIdMsg   = 'TIS0826';
333500151016             wIdCampo = 'VAOCAC';
333600151016             clear wParm;
333700151016             exsr Messaggi;
333800151016             wErrMitt = *on;
333900151016             leavesr;
334000151016           ENDIF;
334100151009         //?se torna errore imposto messaggio generico
334200151009           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
334300131205             wIdMsg   = 'TIS0054';
334400131227             wIdCampo = 'VAOLOC';
334500131205             clear wParm;
334600131205             exsr Messaggi;
334700131205             leavesr;
334800131205           ENDIF;
334900160607           ENDIF;
335000160607
335100160607         //?Se richiamato da AS400
335200160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
335300160607         //?se oltre al destinatario c'è anche la filiale consegna impostata devo controllarla
335400160607         //?questo serve ai fini della bollettazione e non ai fini dell'ORM
335500160607             IF  fiora0I0.vaopoc <> *zeros and fiora0I0.vaorsc <> *blanks;
335600160607               exsr FilConsegna;
335700160607             ENDIF;
335800160607         //?se ORM import DPD devo controllare che la nazione di destino sia servita da DPD
335900160607             IF  wNetworkEmi = FIORA00_NTW_DPD and
336000160607                 fiora0I0.vaonac <> *blanks and
336100160607                 fnlv14ds.O14lad <= 0;
336200160607               wIdMsg   = 'TIS0841';
336300160607               wIdCampo = 'VAONAC';
336400160607               clear wParm;
336500160607               exsr Messaggi;
336600160607             ENDIF;
336700160607           ENDIF;
336800131227
336900131227         //?Se ORM export EEX non accetto destinatario esteri
337000131227           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
337100131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX and
337200131227               fiora0O0.vaonac <> *blanks;
337300140305             wIdMsg   = 'TIS0723';
337400131227             wIdCampo = 'VAONAC';
337500140305             clear wParm;
337600131227             exsr Messaggi;
337700131227           ENDIF;
337800131205
337900131205         //?Se ORM export DPD controllo la fattibilità della consegna
338000131205           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
338100131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
338200131205             clear fior95ds;
338300131205             fior95ds.IOR95cap = fiora0O0.vaocac;
338400131205             fior95ds.IOR95loc = fiora0O0.vaoloc;
338500131205             fior95ds.IOR95prv = fiora0O0.vaoprc;
338600131205             fior95ds.IOR95naz = fiora0O0.vaonac;
338700131230             fior95ds.IOR95lin = cvtLinguaISO2ToTntbe(fiora0I0.idlingua);
338800131205             fior95r (kpjba : fior95ds);
338900131205             IF  fior95ds.OOR95err <> *blanks or fior95ds.OOR95ok = 'N';
339000140304               wIdMsg   = 'TIS0718';
339100131205               wIdCampo = 'VAONAC';
339200131205               wParm = fior95ds.OOR95msg;
339300131205               exsr Messaggi;
339400131205             ENDIF;
339500131205           ENDIF;
339600160208
339700160208         //?Se ORM prepagato e consegna con LNA 340-341-345 blocco
339800160208         //?come da DV 1540
339900160208           IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
340000160209               fiora0I0.vaonac <> *blanks and fnlv13ds.O13err = *blanks and
340100160208               (tisi95ds.O95lna = 340 or tisi95ds.O95lna = 341 or
340200160208                tisi95ds.O95lna = 345);
340300160208             clear xx;
340400160208             xx = %lookup(fiora0I0.vaonac:sknaz);
340500160208             wIdMsg   = 'TIS0828';
340600160208             wIdCampo = 'VAOPAG';
340700160208             IF  xx > 0;
340800160208               wParm = sknazd(xx);
340900160208             ELSE;
341000160208               wParm = fiora0I0.vaoloc;
341100160208             ENDIF;
341200160208             exsr Messaggi;
341300160208           ENDIF;
341400160607         //?Se richiamato da AS400 controllo anche con la filiale consegna
341500160607         //?se inserita
341600160607           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
341700160607               fiora0O0.prepagato = FIORA00_PREPAGATO and
341800160607               fiora0I0.vaonac <> *blanks and
341900160607               (fiora0I0.vaopoc = 340 or fiora0I0.vaopoc = 341 or
342000160607                fiora0I0.vaopoc = 345);
342100160607             clear xx;
342200160607             xx = %lookup(fiora0I0.vaonac:sknaz);
342300160607             wIdMsg   = 'TIS0828';
342400160607             wIdCampo = 'VAOTOR';
342500160607             IF  xx > 0;
342600160607               wParm = sknazd(xx);
342700160607             ELSE;
342800160607               wParm = fiora0I0.vaoloc;
342900160607             ENDIF;
343000160607             exsr Messaggi;
343100160607           ENDIF;
343200160208
343300160208         //?Solo per richiamo da Internet
343400160208           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
343500160208           //?Errore
343600160208           //?Se ordinante non codificato (è utente senza PSW)
343700160208           //?Se consegna all'estero
343800160208           //?Se paga destinatario
343900160208             IF  fiora0I0.vaocor = 0 and
344000160208                 fiora0I0.vaonac <> *blanks and
344100160714                 fiora0I0.vaopag = FIORA00_PAGA_DESTINATARIO;
344200160208               wIdMsg   = 'TIS0448';
344300160208               wIdCampo = 'VAOPAG';
344400160208               clear wParm;
344500160208               exsr Messaggi;
344600160208             ENDIF;
344700160208           ENDIF;
344800131205
344900131205         ENDIF;
345000131205
345100131205       ENDSR;
345200160613
345300160613       //--------------------------------------------------------------
345400160613       //?Controllo quantità videata F05.
345500160613       //--------------------------------------------------------------
345600160613       BEGSR  Quantita2;
345700160613
345800160613       //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
345900160613       //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
346000160613         IF  fiorb0OR.flagfblc <> *blanks and fiora0I0.vaoblc = 0;
346100160613           wIdMsg   = 'TIS0844';
346200160613           wIdCampo = 'VAOBLC';
346300160613           clear wParm;
346400160714           IF  fiorb0OR.flagfblc = FIORA00_FORZABILE;
346500160613             exsr Forzatura;
346600160613             IF  wForza;
346700160613               exsr RoutEnd;
346800160613             ENDIF;
346900160613           ENDIF;
347000160714           IF  fiorb0OR.flagfblc = FIORA00_OBBLIGATORIO;
347100160613             exsr Messaggi;
347200160613           ENDIF;
347300160613         ENDIF;
347400160613         IF  fiorb0OR.flagfmtc <> *blanks and fiora0I0.vaomtc = 0;
347500160613           wIdMsg   = 'TIS0845';
347600160613           wIdCampo = 'VAOMTC';
347700160613           clear wParm;
347800160714           IF  fiorb0OR.flagfmtc = FIORA00_FORZABILE;
347900160613             exsr Forzatura;
348000160613             IF  wForza;
348100160613               exsr RoutEnd;
348200160613             ENDIF;
348300160613           ENDIF;
348400160714           IF  fiorb0OR.flagfmtc = FIORA00_OBBLIGATORIO;
348500160613             exsr Messaggi;
348600160613           ENDIF;
348700160613         ENDIF;
348800160613         IF  fiorb0OR.flagfatt <> *blanks and fiora0I0.vaoatt = 0;
348900160613           wIdMsg   = 'TIS0846';
349000160613           wIdCampo = 'VAOATT';
349100160613           clear wParm;
349200160714           IF  fiorb0OR.flagfatt = FIORA00_FORZABILE;
349300160613             exsr Forzatura;
349400160613             IF  wForza;
349500160613               exsr RoutEnd;
349600160613             ENDIF;
349700160613           ENDIF;
349800160714           IF  fiorb0OR.flagfatt = FIORA00_OBBLIGATORIO;
349900160613             exsr Messaggi;
350000160613           ENDIF;
350100160613         ENDIF;
350200160613
350300160613       ENDSR;
350400131205
350500131205       //--------------------------------------------------------------
350600131205       //?Controllo fermo deposito.
350700131205       //--------------------------------------------------------------
350800131205       BEGSR  FermoDeposito;
350900131205
351000160714         IF  fiora0I0.vaoffd <> FIORA00_SI and
351100160714             fiora0I0.vaoffd <> FIORA00_NO;
351200131205           wIdMsg   = 'TIS0537';
351300131205           wIdCampo = 'VAOFFD';
351400131205           clear wParm;
351500131205           exsr Messaggi;
351600131205         ENDIF;
351700131205
351800131205       ENDSR;
351900160315
352000160315       //--------------------------------------------------------------
352100160315       //?Controllo Filiale Consegna.
352200160315       //--------------------------------------------------------------
352300160315       BEGSR  FilConsegna;
352400160315
352500160315         wOKffc = *off;
352600160315         fiora0O0.vaopoc = fiora0I0.vaopoc;
352700160315
352800160315       //?Se il destinatario è estero
352900160315       //?forzo la filiale di consegna in base alla tabella FFC
353000160315         IF  fiora0O0.vaonac <> *blanks;
353100160315           clear dFFC;
353200160315       //?prima provo con la filiale dell'ordinante
353300160315           IF  fiora0O0.vaocor > 0;
353400160315             k_TBEcod = 'FFC';
353500160315             k_TBEke1 = %subst(%editc(fiora0O0.vaocor:'X'):1:3);
353600160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
353700160315             IF  %found(TNTBE01L);
353800160315               dFFC = TBEuni;
353900160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
354000160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
354100160315               ENDIF;
354200160315               wOKffc = *on;
354300160315             ENDIF;
354400160315           ENDIF;
354500160315       //?se non trovo con filiale ordinante proco con filiale emissione se diversa
354600160315           IF  not wOKffc and
354700160315               %subst(%editc(fiora0O0.vaocor:'X'):1:3) <>
354800160315               %editc(fiora0O0.vaopoe:'X');
354900160315             clear dFFC;
355000160315             k_TBEcod = 'FFC';
355100160315             k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
355200160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
355300160315             IF  %found(TNTBE01L);
355400160315               dFFC = TBEuni;
355500160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
355600160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
355700160315               ENDIF;
355800160315             ENDIF;
355900160315           ENDIF;
356000160315         ENDIF;
356100160315
356200160315       //?Se non impostata non controllo
356300160315         IF  fiora0O0.vaopoc = *zeros;
356400160315           leavesr;
356500160315         ENDIF;
356600160315
356700160315       //?Controllo la filiale consegna
356800160315         IF  not IsFilValida(fiora0I0.vaopoc);
356900160315           wIdMsg   = 'TIS0803';
357000160315           wIdCampo = 'VAOPOC';
357100160315           clear wParm;
357200160315           exsr Messaggi;
357300160315           leavesr;
357400160315         ENDIF;
357500160315
357600160315       //?Network e Decodifico la filiale consegna
357700160315         wNetworkCon = GetNtwFiliale(fiora0O0.vaopoc);
357800160315
357900160315       ENDSR;
358000160315
358100160315       //--------------------------------------------------------------
358200160315       //?Controllo Cliente tassazione.
358300160315       //--------------------------------------------------------------
358400160315       BEGSR  ClienteKsc;
358500160315
358600160315         wOKtariffa = *off;
358700160315
358800160315       //?Codice tassazione valido
358900160315         IF  fiora0I0.vaoksc > *zeros;
359000160315           IF  not IsKscValido(fiora0I0.vaoksc);
359100160315             wIdMsg   = 'TIS0810';
359200160315             wIdCampo = 'VAOKSC';
359300160315             clear wParm;
359400160315             exsr Messaggi;
359500160315             leavesr;
359600160315           ENDIF;
359700160315         ENDIF;
359800160315
359900160315       //?Codice tariffa valido
360000160315         IF  fiora0I0.vaoctr <> *blanks;
360100160315           IF  %check(FIORA00_DIGITS:fiora0I0.vaoctr) > *zeros;
360200160315             wIdMsg   = 'TIS0810';
360300160315             wIdCampo = 'VAOCTR';
360400160315             clear wParm;
360500160315             exsr Messaggi;
360600160315             leavesr;
360700160315           ENDIF;
360800160315         ENDIF;
360900160315
361000160315       //?Se codice tariffa deve esserci anche il codice cliente
361100160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc = *zeros;
361200160607           wIdMsg   = 'TIS0840';
361300160315           wIdCampo = 'VAOCTR';
361400160607           clear wParm;
361500160315           exsr Messaggi;
361600160315           leavesr;
361700160315         ENDIF;
361800160315
361900160315       //?La tariffa deve esistere
362000160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc > *zeros;
362100160315
362200160315           k_TAMksc = fiora0I0.vaoksc;
362300160315           k_TAMctr = %int(fiora0I0.vaoctr);
362400160315           setll (k_TAMksc:k_TAMctr) TNTAM01L;
362500160315           IF  not %equal(TNTAM01L);
362600160315             wIdMsg   = 'TIS0810';
362700160315             wIdCampo = 'VAOKSC';
362800160315             clear wParm;
362900160315             exsr Messaggi;
363000160315             leavesr;
363100160315           ENDIF;
363200160315
363300160315           DOW  not wOKtariffa;
363400160315             reade (k_TAMksc:k_TAMctr) TNTAM01L;
363500160315             IF  %eof(TNTAM01L);
363600160315               leave;
363700160315             ENDIF;
363800160315             IF  TAMatb = *blanks;
363900160315               wOKtariffa = *on;
364000160315             ENDIF;
364100160315           ENDDO;
364200160315           IF  not wOKtariffa;
364300160315             wIdMsg   = 'TIS0810';
364400160315             wIdCampo = 'VAOKSC';
364500160315             clear wParm;
364600160315             exsr Messaggi;
364700160315             leavesr;
364800160315           ENDIF;
364900160315
365000160315         ENDIF;
365100160315
365200160315         fiora0O0.vaoksc = fiora0I0.vaoksc;
365300160315         fiora0O0.vaoctr = fiora0O0.vaoctr;
365400160315
365500160315       ENDSR;
365600140506
365700140506       //--------------------------------------------------------------
365800140506       //?Controllo Alert.
365900140506       //--------------------------------------------------------------
366000140506       BEGSR  Alert;
366100140506
366200140506         IF  fiora0I0.alertmail <> *blanks and
366300160714             fiora0I0.alertmail <> FIORA00_NO and
366400160714             fiora0I0.alertmail <> FIORA00_X;
366500140506           clear fiora0O0.alertmail;
366600140506         ENDIF;
366700140506
366800140506         IF  fiora0I0.alertsms <> *blanks and
366900160714             fiora0I0.alertsms <> FIORA00_NO and
367000160714             fiora0I0.alertsms <> FIORA00_X;
367100140506           clear fiora0O0.alertsms;
367200140506         ENDIF;
367300140506
367400140506       ENDSR;
367500151008
367600151008       //--------------------------------------------------------------
367700151008       //?Controllo indirizzo mail x accettazione ORM.
367800151008       //--------------------------------------------------------------
367900151008       BEGSR  MailConferma;
368000160503
368100160714         fiora0O0.alertconf = FIORA00_NO;
368200151008
368300160318       //?Versione inferiore alla 'C'
368400160714         IF  fiora0I0.mailconf = *blanks and
368500160714             fiora0I0.alertconf = FIORA00_SI and
368600160318             fiora0O0.versione < 'C';
368700151008           wIdMsg   = 'TIS0732';
368800151008           wIdCampo = 'MAILCONF';
368900151008           clear wParm;
369000151008           exsr Messaggi;
369100151008           leavesr;
369200151008         ENDIF;
369300160318       //?dalla Versione 'C'
369400160714         IF  fiora0I0.mailconf = *blanks and
369500160714             fiora0I0.alertconf = FIORA00_SI and
369600160318             fiora0O0.versione > 'B' and fiora0I0.smsconf = *blanks;
369700160318           wIdMsg   = 'TIS0831';
369800160318           wIdCampo = 'MAILCONF';
369900160318           clear wParm;
370000160318           exsr Messaggi;
370100160318           leavesr;
370200160318         ENDIF;
370300151008
370400151008         IF  fiora0I0.mailconf <> *blanks;
370500151008           clear dsemail;
370600151008           dsemail.§emlindi = fiora0I0.mailconf;
370700151008           xemail (dsemail);
370800151008           IF  dsemail.§emlerro <> '0';
370900151008             wIdMsg   = 'TIS0732';
371000151008             wIdCampo = 'MAILCONF';
371100151008             clear wParm;
371200151008             exsr Messaggi;
371300151008             leavesr;
371400151008           ENDIF;
371500151008
371600151008           fiora0O0.mailconf = fiora0I0.mailconf;
371700151008           IF  dsemail.§emlindo <> *blanks;
371800151008             fiora0O0.mailconf = dsemail.§emlindo;
371900151008           ENDIF;
372000151008         ENDIF;
372100151008
372200151008       //?Passo il dato solo se impostato a 'SI'
372300160714         IF  fiora0I0.alertconf = FIORA00_SI;
372400151008           fiora0O0.alertconf = fiora0I0.alertconf;
372500160322         //?Dalla versione 'C' se non c'è la mail non lo imposto
372600160322           IF  fiora0O0.versione > 'B' and fiora0O0.mailconf = *blanks;
372700160714             fiora0O0.alertconf = FIORA00_NO;
372800160322           ENDIF;
372900151008         ENDIF;
373000151008
373100151008       ENDSR;
373200151008
373300151008       //--------------------------------------------------------------
373400151008       //?Calcolo orario indicativo ritiro.
373500151008       //--------------------------------------------------------------
373600151008       BEGSR  OrarioIndRitiro;
373700151008
373800151008       //?imposto orario ritiro standard
373900151012         fiora0O0.orariti = fiora0O0.orainizio;
374000151012         fiora0O0.oraritf = fiora0O0.orafine;
374100151126
374200151126       //?Se l'ora pronta merce è maggiore
374300151126       //?dell'ora inizio servizio
374400151126       //?e minore dell'ora fine
374500151126       //?la imposto come orario inzio
374600151126         IF  fiora0O0.vaoorr > fiora0O0.orainizio and
374700151126             fiora0O0.vaoorr < fiora0O0.orafine;
374800151126           fiora0O0.orariti = fiora0O0.vaoorr;
374900151126         ENDIF;
375000151126
375100151126       //?Se l'ora pronta merce è maggiore
375200151126       //?dell'ora fine servizio
375300151126       //?la imposto come orario fine
375400151126         IF  fiora0O0.vaoorr > fiora0O0.orafine;
375500151126           fiora0O0.oraritf = fiora0O0.vaoorr;
375600151126         ENDIF;
375700151008
375800151008       ENDSR;
375900160318
376000160318       //--------------------------------------------------------------
376100160318       //?Controllo SMS x accettazione ORM.
376200160318       //--------------------------------------------------------------
376300160318       BEGSR  SmsConferma;
376400160318
376500160714         fiora0O0.alertconfs = FIORA00_NO;
376600160318
376700160620       //?Se richiamato da Internet
376800160620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
376900160714         IF  fiora0I0.smsconf = *blanks and
377000160714             fiora0I0.alertconf = FIORA00_SI and
377100160318             fiora0I0.mailconf = *blanks;
377200160318           wIdMsg   = 'TIS0831';
377300160318           wIdCampo = 'SMSCONF';
377400160318           clear wParm;
377500160318           exsr Messaggi;
377600160318           leavesr;
377700160318         ENDIF;
377800160318
377900160714         IF  fiora0I0.smsconf <> *blanks and
378000160714             fiora0I0.alertconf = FIORA00_SI;
378100160318           pInCell = %trim(fiora0I0.smsconf);
378200160318           clear pOutErr;
378300160318           clear pOutCell;
378400160318           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
378500160318             wIdMsg   = 'TIS0725';
378600160318             wIdCampo = 'SMSCONF';
378700160318             clear wParm;
378800160318             exsr Messaggi;
378900160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
379000160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
379100160601             fiora0I0.smsconf;
379200160318             leavesr;
379300160318           ENDIF;
379400160318
379500160318       //?Passo il dato solo se impostato a 'SI'
379600160714         //IF  fiora0I0.alertconf = FIORA00_SI;
379700160318           fiora0O0.smsconf = fiora0I0.smsconf;
379800160318           IF  pOutCell <> *blanks;
379900160318             fiora0O0.smsconf = pOutCell;
380000160318           ENDIF;
380100160318         //?Passo anche il nuovo flag
380200160318           IF  fiora0O0.smsconf <> *blanks;
380300160714             fiora0O0.alertconfs = FIORA00_SI;
380400160318           ENDIF;
380500160503         //ENDIF;
380600160502         ENDIF;
380700160620         ENDIF;
380800160620
380900160620       //?Se richiamato da AS400
381000160620         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
381100160620           IF  fiora0I0.smsconf <> *blanks;
381200160620             pInCell = %trim(fiora0I0.smsconf);
381300160620             clear pOutErr;
381400160620             clear pOutCell;
381500160620             IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
381600160620               wIdMsg   = 'TIS0725';
381700160620               wIdCampo = 'SMSCONF';
381800160620               clear wParm;
381900160620               exsr Messaggi;
382000160620               fiora0M0.skTextMsg (fiora0M0.nrmsg) =
382100160620               %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
382200160620               fiora0I0.smsconf;
382300160620               leavesr;
382400160620             ENDIF;
382500160620             fiora0O0.smsconf = fiora0I0.smsconf;
382600160620             IF  pOutCell <> *blanks;
382700160620               fiora0O0.smsconf = pOutCell;
382800160620             ENDIF;
382900160620           ENDIF;
383000160620         ENDIF;
383100160318
383200160318       ENDSR;
383300160714
383400160714       //--------------------------------------------------------------
383500160714       //?Controllo Flag x memorizzazione dati su Anagrafica Ritiro.
383600160714       //--------------------------------------------------------------
383700160714       BEGSR  FlagMemDati;
383800160714
383900160714       //?Per richiamo da Internet
384000160714       //?e utente anonimo non controllo
384100160714         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
384200160714             fiora0I0.IdUtente = 0;
384300160714           leavesr;
384400160714         ENDIF;
384500160714
384600160714       //?Accetto solo S o N
384700160714         IF  fiora0I0.memdatiacr <> FIORA00_SI and
384800160728             fiora0I0.memdatiacr <> FIORA00_NO and
384900160728             fiora0I0.memdatiacr <> *blanks;
385000160714           wIdMsg   = 'TIS0848';
385100160714           wIdCampo = 'MEMDATIACR';
385200160714           clear wParm;
385300160714           exsr Messaggi;
385400160714         ENDIF;
385500160714
385600160714       ENDSR;
385700160315
385800160315       //--------------------------------------------------------------
385900160315       //?Cerco il giro di ritiro.
386000160315       //--------------------------------------------------------------
386100160315       BEGSR  GiroRitiro;
386200160315
386300160315         fiora0O0.cgi = fiora0I0.cgi;
386400160315
386500160412       //?Se giro impostato lo controllo
386600160315         IF  fiora0O0.cgi <> *blanks;
386700160315           clear FIDG09DS;
386800160315           fidg09ds.D09iop0 = '001';
386900160518           fidg09ds.D09ifgs = fiora0I0.VAOpor;
387000160315           fidg09ds.D09idat = dataoggi;
387100160315           fidg09ds.D09icgi = fiora0O0.cgi;
387200160315           fidg09ds.D09itug = 'R';
387300160315           %subst(kpjba:247:256) = FIDG09DS;
387400160315           fidg09r (kpjba);
387500160315           fidg09ds = %subst(kpjba:247:256);
387600160315           IF  fidg09ds.D09oerr <> *blanks;
387700160315             wIdMsg   = 'TIS0718';
387800160315             wIdCampo = 'CGI';
387900160315             wParm = fidg09ds.D09omsg;
388000160315             exsr Messaggi;
388100160315             leavesr;
388200160315           ENDIF;
388300160315         ENDIF;
388400160315
388500160315       ENDSR;
388600140312
388700131008       //--------------------------------------------------------------
388800131008       //?Routine per schierare i messaggi nella ds FIORA0M0.
388900131008       //--------------------------------------------------------------
389000131008       BEGSR  Messaggi;
389100131007
389200131008       //?Se ho già caricato 99 messaggi esco
389300131008         IF  fiora0M0.nrmsg = 99;
389400131010           exsr RoutEnd;
389500131008           clear fiora0M0.nrmsg;
389600131008         ENDIF;
389700131008
389800131008         fiora0M0.nrmsg += 1;
389900131009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
390000131009         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
390100140211         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
390200131204         IF  wParm <> *blanks;
390300131219           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
390400131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
390500131204         ELSE;
390600131219           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
390700131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
390800131204         ENDIF;
390900131008
391000131008       ENDSR;
391100131008
391200131008       //--------------------------------------------------------------
391300131008       //?Routine per schierare le forzature nella ds FIORA0F0.
391400131008       //--------------------------------------------------------------
391500131008       BEGSR  Forzatura;
391600160531
391700160531         wForza = *off;
391800160530
391900160530       //?Se già forzato non riemetto il messaggio
392000160531         wNrForza = %lookup(wIdMsg:fiora0F0.skIdMsg);
392100160531         IF  wNrForza > 0 and fiora0F0.skGiaForza(wNrForza) =
392200160530             FIORA00_GIAFORZA_SI;
392300160530           leavesr;
392400160530         ENDIF;
392500160531
392600160531       //?Se ho già caricato 99 mesaggi esco
392700160531         IF  fiora0M0.nrmsg = 99;
392800160531           exsr RoutEnd;
392900160531           clear fiora0M0.nrmsg;
393000160531         ENDIF;
393100160530
393200160531       //?Imposto messaggio da forzare
393300160530         fiora0M0.nrmsg += 1;
393400160530         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
393500160530         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
393600160530         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
393700160530         IF  wParm <> *blanks;
393800160530           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
393900160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
394000160530         ELSE;
394100160530           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
394200160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
394300160530         ENDIF;
394400160531
394500160531         wForza = *on;
394600131008
394700131008       ENDSR;
394800131009
394900131009       //--------------------------------------------------------------
395000131009       //?Imposta i dati nella ds di Output.
395100131009       //--------------------------------------------------------------
395200131009       BEGSR  DsOutput;
395300131008
395400131009       //?Se richiamo da Internet
395500131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
395600131223
395700131223         //?Imposto se commissionato
395800140407           IF  wCom;
395900160714             dorm01.§orcomc = FIORA00_SI;
396000131223           ELSE;
396100160714             dorm01.§orcomc = FIORA00_NO;
396200131223           ENDIF;
396300140506
396400140507         //?Se ORM commissionato e data ritiro passata al pgm = 0
396500140507         //?vuol dire che non è ancora stata calcolata quindi siamo
396600140507         //?nel passaggio tra la prima e la seconda pagina
396700140507           IF  wCom and fiora0I0.vaodar = 0;
396800140507           //?se mail e/o sms impostati torno 'S' nei flag degli alert
396900140507             IF  fiora0O0.mail <> *blanks;
397000160714               fiora0O0.alertmail = FIORA00_SI;
397100140507             ENDIF;
397200140507             IF  fiora0O0.sms <> *blanks;
397300160714               fiora0O0.alertsms = FIORA00_SI;
397400140507             ENDIF;
397500140507           ENDIF;
397600140507
397700140507         //?Se ORM commissionato e data ritiro (calcolata o impostata dall'utente) = oggi
397800140506         //?imposto i flag alert a 'N'
397900140508         //?ma se data ritiro passata è > di 0, in questo caso vuol dire che siamo già
398000140508         //?in seconda pagina e l'utente ha messo oggi al posto di domani
398100140508           IF  wCom and fiora0O0.vaodar = dataoggi and fiora0I0.vaodar > 0;
398200140507             IF  fiora0O0.mail <> *blanks;
398300160714               fiora0O0.alertmail = FIORA00_NO;
398400140506             ENDIF;
398500140507             IF  fiora0O0.sms <> *blanks;
398600160714               fiora0O0.alertsms = FIORA00_NO;
398700140506             ENDIF;
398800140506           ENDIF;
398900131009
399000131009         ENDIF;
399100131223
399200131227         fiora0O0.idrichiamo = fiora0I0.idrichiamo;
399300131223         fiora0O0.idlingua = fiora0I0.idlingua;
399400140402
399500140402         IF  fiora0I0.vaorfa <> *blanks;
399600140402           exec sql
399700140402           set :fiora0O0.vaorfa = ucase (:fiora0I0.vaorfa);
399800140402         ENDIF;
399900131223
400000131223         IF  fiora0I0.vaono1 <> *blanks;
400100140211           exec sql
400200140211           set :fiora0O0.vaono1 = ucase (:fiora0I0.vaono1);
400300131223         ENDIF;
400400131223         IF  fiora0I0.vaono2 <> *blanks;
400500140211           exec sql
400600140211           set :fiora0O0.vaono2 = ucase (:fiora0I0.vaono2);
400700131223         ENDIF;
400800131223
400900131205         fiora0O0.vaoflo = dorm01;
401000131009
401100131009       ENDSR;
401200140408
401300140408       //--------------------------------------------------------------
401400140408       //?Richiamo pgm per calcolo orari servizio.
401500140408       //--------------------------------------------------------------
401600140408       BEGSR  CallTrulORS;
401700140408
401800140408         clear TRULORSds;
401900140408         trulorsds.IOREfil  = wPor;
402000140408         trulorsds.IORecap  = wCap;
402100140408         trulorsds.IOREloc  = wLocalita;
402200140408         trulorsds.IOREdta  = wData;
402300140408         trulorsds.IOREtser = 'R';
402400140408         trulorsr (kpjba:trulorsds:trulor2ds);
402500140408
402600140408       ENDSR;
402700160526
402800160526       //--------------------------------------------------------------
402900160526       //?Richiamo pgm per calcolo orari servizio estero.
403000160526       //--------------------------------------------------------------
403100160526       BEGSR  CallFIORE;
403200160526
403300160526         clear FIORE00ds;
403400160526         fiore00ds.IORE00por = wPor;
403500160526         fiore00ds.IORE00nar = wNazione;
403600160526         fiore00r (fiore00ds);
403700160526
403800160526       ENDSR;
403900140408
404000140408       //--------------------------------------------------------------
404100140408       //?Imposto la frase dal tornare al chiamante.                   ?
404200140408       //--------------------------------------------------------------
404300140408       BEGSR  GetFrase1;
404400140408
404500140408       //?Se ID lingua non valido o non passato
404600140408       //?assumo italiano
404700140408         IF  fiora0I1.idlingua = *blanks or
404800140408            (fiora0I1.idlingua <> FIORA00_ID_LINGUA_IT and
404900140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_FR and
405000140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_EN and
405100140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_DE);
405200140408            fiora0I1.idlingua = FIORA00_ID_LINGUA_IT;
405300140408         ENDIF;
405400140408
405500140408       //?Cerco gli orari di servizio
405600140408         wPor      = fiora0I1.filritiro;
405700140408         wData     = dataoggi;
405800140408         wCap      = fiora0I1.cap;
405900140408         wLocalita = fiora0I1.localita;
406000140408         exsr CallTrulORS;
406100160526
406200160526       //?Dalla versione "C" cerco la frase anche per i ritiri esteri
406300160526         SELECT;
406400160531         WHEN  fiora0I1.versione > 'B';
406500160531           wNetworkRit = GetNtwFiliale(fiora0I1.filritiro);
406600160531           clear dNTW;
406700160531           k_TBEcod = 'NTW';
406800160531           k_TBEke1 = wNetworkRit;
406900160531           chain (k_TBEcod:k_TBEke1) TNTBE01L;
407000160531           IF  %found(TNTBE01L);
407100160531             dNTW = TBEuni;
407200160531           ENDIF;
407300160531           IF  wNetworkRit = FIORA00_NTW_DPD or
407400160531               dntw.§NTWfie = FIORA00_NTW_ESTERO;
407500160526             wPor     = fiora0I1.filritiro;
407600160526             wNazione = fiora0I1.nazione;
407700160526             exsr CallFIORE;
407800160526           ENDIF;
407900160526         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
408000160526         //?non devo emettere la frase dato che la merce parte sempre il
408100160526         //?giorno dopo e la data ritiro è sempre il giorno dopo
408200160526           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
408300160526               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
408400160526             clear fiora0O1.frase;
408500160526             fiora0O1.idlingua = fiora0I1.idlingua;
408600160526             leavesr;
408700160526           ENDIF;
408800160526         //?Se non è un prepagato imposto la frase
408900160526           wIdMsg   = 'TIS0827';
409000160526           SELECT;
409100160526           //?Ritiri esteri
409200160526           WHEN  fiora0I1.nazione <> *blanks;
409300160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
409400160526           //?Ritiri commissionati
409500160714           WHEN  fiora0I1.contattot = FIORA00_SI;
409600160526             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
409700160526           OTHER;
409800160526           //?Ritiri normali
409900160526             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
410000160526           ENDSL;
410100160526         OTHER;
410200140408
410300140408       //?Imposto la frase
410400151104       //?Frasi diverse in base alla versione
410500151104         IF  fiora0I1.versione > 'A';
410600151104         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
410700151104         //?non devo emettere la frase dato che la merce parte sempre il
410800151104         //?giorno dopo e la data ritiro è sempre il giorno dopo
410900151104           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
411000151104               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
411100151104             clear fiora0O1.frase;
411200151104             fiora0O1.idlingua = fiora0I1.idlingua;
411300151104             leavesr;
411400151104           ENDIF;
411500151104         //?Se cliente codificato imposto la nuova frase
411600151104           wIdMsg   = 'TIS0827';
411700160714           IF  fiora0I1.contattot = FIORA00_SI;
411800151104             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
411900151104           ELSE;
412000151104             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
412100151104           ENDIF;
412200151104         ELSE;
412300140408         wIdMsg   = 'TIS0731';
412400160714         IF  fiora0I1.contattot = FIORA00_SI;
412500140408           wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
412600140408         ELSE;
412700140408           wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
412800140408         ENDIF;
412900151104         ENDIF;
413000160526
413100160526         ENDSL;
413200140408
413300160531       //?Per ora ND ha deciso di non impostare la frase per i ritiri esteri
413400160531         IF  fiora0I1.nazione <> *blanks and SaltaFraseExport <> *blanks;
413500160531           clear fiora0O1.frase;
413600160531           leavesr;
413700160531         ENDIF;
413800160531
413900140408         fiora0O1.frase =
414000140408         rtvMsgLang(wIdMsg:fiora0I1.idlingua:wParm);
414100140408
414200140408         fiora0O1.idlingua = fiora0I1.idlingua;
414300140408
414400140408       ENDSR;
414500131010
414600131010       //--------------------------------------------------------------
414700131010       //?Operazioni finali.
414800131010       //--------------------------------------------------------------
414900131010       BEGSR  RoutEnd;
415000140408
415100140408       //?Se richiamato per GET_FRASE1
415200140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
415300140408           %subst(prmRpyData:1:prmRpyDataSize) = fiora0O1;
415400140408         ELSE;
415500131010
415600131219         %subst(prmRpyData:1:prmRpyDataSize) = fiora0O0;
415700131219         %subst(prmRpyMessage:1:prmRpyMsgSize) = fiora0M0;
415800131219         %subst(prmRpyForzatu:1:prmRpyForSize) = fiora0F0;
415900140408
416000140408         ENDIF;
416100131010
416200131010         return;
416300131010
416400131010       ENDSR;
416500131009
416600131007      /end-free
416700140312
416800140313      *--------------------------------------------------
416900140313      * Procedure name: IsCodiceLegato
417000140313      * Purpose:        Controlla codice
417100140402      * Returns:        ON - OFF
417200140313      *--------------------------------------------------
417300140313     p IsCodiceLegato  B
417400140313     d IsCodiceLegato  PI              n
417500140312
417600140313      /copy gaitrasrc/srcconst,FIORB00R
417700140313     d isErrore        s               n   inz
417800140313     d RpyIdMsg        s             10i 0
417900140313     d RpyOpCode       s             10i 0
418000140313     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_LEGGI)
418100140313     d wNrRec          s              7s 0 inz
418200140313     d fiorb0I0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
418300140313     d fiorb0O0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
418400140313     d  skCodice                     10a   dim(10) overlay(Codice)
418500140313      /copy gaitrasrc/srcprotopr,FIORB00R
418600140312
418700140313      /free
418800140312
418900140313       reset isErrore;
419000140313       reset RpyIdMsg;
419100140313       reset RpyOpCode;
419200140313       reset fiorb0I0;
419300140313       reset fiorb0O0;
419400140312
419500140313       fiorb0I0.codiceksu = *all'0';
419600140313       %subst(fiorb0I0.codiceKsu:2:7) =
419700140312       %subst(%editc(wCodice:'X'):1:7);
419800140312       //?Solo per richiamo da Internet
419900140313       IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
420000140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
420100140313       ELSE;
420200140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
420300140313       ENDIF;
420400140312
420500160519       DOU  wNrRec = fiorb0O0.clientitot
420600140313         OR fiorb0O0.nrrec = *ZERO;
420700140313
420800140313         MONITOR;
420900140313           Fiorb00_Anagrafica( RqsOpCode
421000140313                             : RpyOpCode
421100140313                             : RpyIdMsg
421200140313                             : fiorb0I0.formato
421300140313                             : fiorb0I0
421400140313                             : %SIZE(fiorb0I0)
421500140313                             : fiorb0O0.formato
421600140313                             : fiorb0O0
421700140313                             : %SIZE(fiorb0O0)
421800140313                             );
421900140313         ON-ERROR;
422000140313           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
422100140313         ENDMON;
422200140312
422300160519         IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
422400140313           isErrore = *on;
422500140313           leave;
422600140313         ENDIF;
422700140313
422800140313         //?Controllo se il codice fa parte della stessa famiglia
422900140313         //?se fa parte esco dal LOOP
423000140313         IF  %lookup(%editc(wCodice:'X'):fiorb0O0.skCodice) > 0;
423100140313           leave;
423200140313         ENDIF;
423300140313
423400140313         wNrRec += 1;
423500140313         RqsOpCode = FIORB00_RQSOPCODE_CARICA;
423600140312
423700140313       ENDDO;
423800140312
423900140313       Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
424000140313                           : RpyOpCode
424100140313                           : RpyIdMsg
424200140312                           );
424300140313
424400140313       IF  isErrore;
424500140313         return *off;
424600140313       ENDIF;
424700140312
424800140313       RETURN *on;
424900140312
425000140312      /END-FREE
425100140313     p IsCodiceLegato  E
425200140312
425300140401      *--------------------------------------------------
425400140401      * Procedure name: IsCodiceValido
425500140401      * Purpose:        Controlla codice
425600140402      * Returns:        ON - OFF
425700140401      *--------------------------------------------------
425800140401     p IsCodiceValido  B
425900140401     d IsCodiceValido  PI              n
426000140402     d  FIORB0O1                           likeds(FIORB0OO)
426100140401
426200140401      /copy gaitrasrc/srcconst,FIORB00R
426300140401     d isErrore        s               n   inz
426400140401     d RpyIdMsg        s             10i 0
426500140401     d RpyOpCode       s             10i 0
426600140401     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
426700140401     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
426800140401      /copy gaitrasrc/srcprotopr,FIORB00R
426900140401
427000140401      /free
427100140401
427200140401       reset isErrore;
427300140401       reset RpyIdMsg;
427400140401       reset RpyOpCode;
427500140401       reset fiorb0I1;
427600140401
427700140401       fiorb0I1.codice = wCodice;
427800140401       MONITOR;
427900140401         Fiorb00_Anagrafica( RqsOpCode
428000140401                           : RpyOpCode
428100140401                           : RpyIdMsg
428200140401                           : fiorb0I1.formato
428300140401                           : fiorb0I1
428400140401                           : %SIZE(fiorb0I1)
428500140401                           : fiorb0O1.formato
428600140401                           : fiorb0O1
428700140401                           : %SIZE(fiorb0O1)
428800140401                           );
428900140401         ON-ERROR;
429000140401           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
429100140401         ENDMON;
429200140401
429300160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
429400140401         isErrore = *on;
429500140401       ENDIF;
429600160519
429700160519       IF  RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND and
429800160519           fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
429900160519         isErrore = *on;
430000160519       ENDIF;
430100140401
430200140401       IF  isErrore;
430300140401         return *off;
430400140402       ENDIF;
430500140402
430600140402       RETURN *on;
430700140401
430800140401      /END-FREE
430900140401     p IsCodiceValido  E
431000140411
431100140411     P*--------------------------------------------------
431200140411     P* Procedure name: GetOraAggiustata
431300140411     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
431400140411     P* Returns:        Ora e minuto aggiustati.
431500140411     P* Parameter:      hhmm => Ora e minuto
431600140411     P*--------------------------------------------------
431700140411     P GetOraAggiustata...
431800140411     P                 B
431900140411     D GetOraAggiustata...
432000140411     D                 PI             4A
432100140411     D  hhmm                          4S 0 CONST
432200140411     D retField        DS             4    QUALIFIED STATIC
432300140411     D  hhmm                   1      4S 0
432400140411     D  hh                     1      2S 0
432500140411     D  mm                     3      4S 0
432600140411
432700140411      /FREE
432800140411
432900140411       IF hhmm = *ZERO;
433000140411         RETURN *BLANK;
433100140411       ENDIF;
433200140411
433300140411       retField.hhmm = hhmm;
433400140411
433500140411       SELECT;
433600140411         WHEN retField.mm = 00 OR retField.mm = 15
433700140411           OR retField.mm = 30 OR retField.mm = 45;
433800140411           // Non è da aggiustare.
433900140411         WHEN retField.mm < 15;
434000140411           retField.mm = 15;
434100140411         WHEN retField.mm < 30;
434200140411           retField.mm = 30;
434300140411         WHEN retField.mm < 45;
434400140411           retField.mm = 45;
434500140411         OTHER;
434600140411           retField.hh += 1;
434700140411           retField.mm = 00;
434800140411       ENDSL;
434900140411
435000140411       RETURN retField;
435100140411
435200140411      /end-free
435300140411     P GetOraAggiustata...
435400140411     P                 E
435500160315
435600160315      *--------------------------------------------------
435700160315      * Procedure name: IsFilValida
435800160315      * Purpose:        Ritorna se filiale è valida
435900160315      * Returns:        On - OFF
436000160315      * Parameter:      Filiale
436100160315      *--------------------------------------------------
436200160315     p IsFilValida     B
436300160315     d IsFilValida     PI              n
436400160315     D  filiale                       3s 0 CONST
436500160315
436600160315      /free
436700160315
436800160315       IF  filiale = *zeros;
436900160315         RETURN *off;
437000160315       ENDIF;
437100160315
437200160315       chain (filiale) AZORG01L;
437300160315       IF  not %found(AZORG01L);
437400160315         RETURN *off;
437500160315       ENDIF;
437600160315       IF  ORGfva <> *blanks;
437700160315         RETURN *off;
437800160315       ENDIF;
437900160315       IF  ORGfag <> 'F' and ORGfag <> 'A';
438000160315         RETURN *off;
438100160315       ENDIF;
438200160315
438300160315       RETURN *on;
438400160315
438500160315      /end-free
438600160315     P IsFilValida     E
438700160315
438800160315      *--------------------------------------------------
438900160315      * Procedure name: GetNtwFiliale
439000160315      * Purpose:        Recupera il Network della filiale
439100160315      * Returns:        Network
439200160315      * Parameter:      Filiale
439300160315      *--------------------------------------------------
439400160315     p GetNtwFiliale   B
439500160315     d GetNtwFiliale   PI            20a
439600160315     D  filiale                       3s 0 CONST
439700160315
439800160315      // ds per campo ORGDE3 di AZORG
439900160315     d OG143         e ds
440000160315
440100160315      /free
440200160315       IF  filiale = *zeros;
440300160315         RETURN *blanks;
440400160315       ENDIF;
440500160315
440600160315       clear OG143;
440700160315       chain (filiale) AZORG01L;
440800160315
440900160315       OG143 = ORGde3;
441000160315
441100160315       RETURN §OGntw;
441200160315
441300160315      /end-free
441400160315     P GetNtwFiliale   E
441500160315
441600160315      *--------------------------------------------------
441700160315      * Procedure name: IsFilAttivaORM
441800160315      * Purpose:        Ritorna se filiale attiva alla procedura ORM
441900160315      * Returns:        On - OFF
442000160315      * Parameter:      Filiale
442100160315      *--------------------------------------------------
442200160315     p IsFilAttivaORM  B
442300160315     d IsFilAttivaORM  PI              n
442400160315     D  filiale                       3s 0 CONST
442500160315
442600160315      // ds per campo ORGDE8 di AZORG
442700160315     d OG148         e ds
442800160315
442900160315      /free
443000160315
443100160315       IF  filiale = *zeros;
443200160315         RETURN *off;
443300160315       ENDIF;
443400160315
443500160315       clear OG148;
443600160315
443700160315       chain (filiale) AZORG01L;
443800160315
443900160315       OG148 = ORGde8;
444000160315
444100160315       IF  §OGorm <> 'S';
444200160315         return *off;
444300160315       ENDIF;
444400160315
444500160315       RETURN *on;
444600160315
444700160315      /end-free
444800160315     P IsFilAttivaORM  E
444900160315
445000160315      *--------------------------------------------------
445100160315      * Procedure name: IsKscValido
445200160315      * Purpose:        Ritorna se Ksc è valido
445300160315      * Returns:        On - OFF
445400160315      * Parameter:      cliente
445500160315      *--------------------------------------------------
445600160315     p IsKscValido     B
445700160315     d IsKscValido     PI              n
445800160315     D  cliente                       7s 0 CONST
445900160315
446000160315      // campi di comodo
446100160315     d filiale         s              3s 0
446200160401     d k_kcc           s                   like(ACOkcc) inz(151)
446300160401     d k_kut           s                   like(ACOkut) inz(1)
446400160315     d network         s              3a
446500160315
446600160315      /free
446700160315
446800160315       IF  cliente = *zeros;
446900160315         RETURN *off;
447000160315       ENDIF;
447100160315
447200160401       chain (k_kut:k_kcc:cliente) CNACO00F;
447300160315       IF  not %found(CNACO00F);
447400160315         RETURN *off;
447500160315       ENDIF;
447600160315       IF  ACOflg <> *blanks;
447700160315         RETURN *off;
447800160315       ENDIF;
447900160315
448000160315      // cerco il ntw del cliente non posso accettare clienti
448100160315      // logistica o di sede
448200160401       filiale = %dec(%subst(%editc(cliente:'X'):1:3):3:0);
448300160315       network = GetNtwFiliale(filiale);
448400160315       IF  network = 'LOG' or network = 'XXX';
448500160315         RETURN *off;
448600160315       ENDIF;
448700160315
448800160315       RETURN *on;
448900160315
449000160315      /end-free
449100160315     P IsKscValido     E
449200160315
449300160315      *--------------------------------------------------
449400160315      * Procedure name: IsValidoCapDPD
449500160315      * Purpose:        Ritorna se Cap DPD valido
449600160315      * Returns:        On - OFF
449700160315      * Parameter:      nazione - cap del mittente
449800160315      *--------------------------------------------------
449900160315     p IsValidoCapDPD  B
450000160315     d IsValidoCapDPD  PI              n
450100160315     D  nazione                       3a   CONST
450200160315     D  cap                           9a   CONST
450300160315     D  data                          8s 0 CONST
450400160315     D  peso                          7s 1 CONST
450500160315     D  limite                        7s 1 CONST
450600160315     D  filiale                       3s 0 CONST
450700160315
450800160315      // ds per cappario DPD
450900160315     d TISIE3DS      e ds                  QUALIFIED INZ
451000160315      // pgm per cappario DPD
451100160315     d TISIE3R         pr                  extpgm('TISIE3R')
451200160315     d  tisie3ds                           likeds(TISIE3DS)
451300160315
451400160315      /free
451500160315
451600160315       IF  nazione = *blanks or cap = *blanks or filiale = 0;
451700160315         RETURN *off;
451800160315       ENDIF;
451900160315
452000160315       clear TISIE3DS;
452100160315       IF  data = 0;
452200160315         tisie3ds.ISIE3dri = %dec(%date());
452300160315       ELSE;
452400160315         tisie3ds.ISIE3dri = data;
452500160315       ENDIF;
452600160315
452700160315       tisie3ds.ISIE3hsp = %dec(%time());
452800160315       tisie3ds.ISIE3nzd = nazione;
452900160315       tisie3ds.ISIE3cad = cap;
453000160315       IF  peso > limite;
453100160315         tisie3ds.ISIE3srv = 101;
453200160315       ELSE;
453300160315         tisie3ds.ISIE3srv = 136;
453400160315       ENDIF;
453500160315       tisie3ds.ISIE3lnp = filiale;
453600160315       tisie3r (tisie3ds);
453700160315       IF  tisie3ds.OSIE3err <> *blanks;
453800160315         RETURN *off;
453900160315       ENDIF;
454000160315
454100160315       RETURN *on;
454200160315
454300160315      /end-free
454400160315     P IsValidoCapDPD  E
