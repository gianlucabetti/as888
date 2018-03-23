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
002400160315
002500160315       // -?File Anagrafic clienti ritiro - estensione
002600160315     fFNACR13L  if   e           k disk
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
003900140114
004000140114     d c_Digits        c                   const('0123456789')
004100160531     d SaltaFraseExport...
004200160531     d                 c                   const('S')
004300131008
004400131007       //--------------------------------------------------------------
004500131007       //?Definizione strutture dati.                                  ?
004600131007       //--------------------------------------------------------------
004700131007       // -?Dati INPUT
004800131009     d FIORA0I0      e ds                  QUALIFIED INZ(*EXTDFT)
004900140408     d FIORA0I1      e ds                  QUALIFIED INZ(*EXTDFT)
005000131007
005100131007       // -?Dati OUTPUT
005200131009     d FIORA0O0      e ds                  QUALIFIED INZ(*EXTDFT)
005300140408     d FIORA0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005400140401     d FIORB0OO      e ds                  EXTNAME(FIORB0O1)
005500140401     d                                     QUALIFIED INZ(*EXTDFT)
005600140401     d FIORB0OR      e ds                  EXTNAME(FIORB0O1)
005700140401     d                                     QUALIFIED INZ(*EXTDFT)
005800140401     d FIORB0OC      e ds                  EXTNAME(FIORB0O1)
005900140401     d                                     QUALIFIED INZ(*EXTDFT)
006000131007
006100131007       // -?Messaggi
006200131009     d FIORA0M0      e ds                  QUALIFIED INZ(*EXTDFT)
006300131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
006400131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
006500131219     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
006600131219     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
006700131007
006800131007       // -?Forzature
006900131009     d FIORA0F0      e ds                  QUALIFIED INZ(*EXTDFT)
007000131219     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
007100131219     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
007200131219     d  skGiaForza                    1a   dim(99) overlay(GiaForzato)
007300160315
007400160315       // -?Controllo giro ritiro
007500160315     d FIDG09DS      e ds                  QUALIFIED INZ
007600160526
007700160526       // -?Ritorna orari servizio ritiri estero
007800160526     d FIORE00DS     e ds                  QUALIFIED INZ
007900140312
008000131205       // -?Controllo destinatario per ORM export DPD
008100131205     d FIOR95DS      e ds                  QUALIFIED INZ
008200131010
008300131010       // -?Forzatura filiale ritiro
008400131204     d FIOR96DS      e ds                  QUALIFIED INZ
008500131010
008600131010       // -?Calcolo data ritiro
008700131204     d FIOR97DS      e ds                  QUALIFIED INZ
008800131205
008900131205       // -?Ritirabilità all'estero
009000131205     d FNLV12DS      e ds                  QUALIFIED INZ
009100151009
009200151009       // -?Controllo cap/località/provincia
009300151009     d FNLV13DS      e ds                  QUALIFIED INZ
009400160315
009500160315       // -?Controllo Indirizzo
009600160315     d FNLV14DS      e ds                  QUALIFIED INZ
009700160315
009800160315       // -?Ricerca terminal
009900160315     d FNLV55DS      e ds                  QUALIFIED INZ
010000131223
010100131223       // -?Calcola orari di servizio
010200140327     d TRULORSDS     e ds                  QUALIFIED INZ
010300140327     d TRULOR2ds     e ds                  QUALIFIED INZ
010400131009
010500131204       // -?Controllo instradamento
010600131204     d TISI95DS      e ds                  QUALIFIED INZ
010700131205     d TISI97DS      e ds                  QUALIFIED INZ
010800131204
010900131204       // -?Controllo orari apertura
011000131204     d TRUL03DS      e ds                  QUALIFIED INZ
011100160408
011200160408       // -?Ritorna dati Servizio Clienti
011300160408     d TRULDSCDS     e ds                  QUALIFIED INZ
011400131008
011500131008       // -?Controllo partita IVA
011600151020     d XCFIVA1DS     e ds                  QUALIFIED INZ
011700140311
011800140311       // -?Controllo indirizzo mail
011900140311     d dsemail       e ds                  QUALIFIED INZ
012000160208
012100160208       // -?Tabella 15 - Nazioni
012200160208     d ds15          e ds                  QUALIFIED INZ
012300131204
012400131204       // -?Tabella 3I - DPD
012500131204     d ds3Idp        e ds                  QUALIFIED INZ
012600160315
012700160315       // -?Tabella AGR - Ambiti dei giri di ritiro
012800160315     d dAGR          e ds                  QUALIFIED INZ
012900151008
013000151008       // -?Tabella DFT - Default ORM
013100151008     d dDFT          e ds                  QUALIFIED INZ
013200160315
013300160315       // -?Tabella FFC - Forza Filiale Consenga
013400160315     d dFFC          e ds                  QUALIFIED INZ
013500131010
013600131010       // -?Tabella NTW - Network
013700131204     d dNTW          e ds                  QUALIFIED INZ
013800140114
013900140114       // -?Campo ACRMAI
014000140114     d dACR01        e ds                  QUALIFIED INZ
014100131010
014200131010       // -?Campo VAOFLO
014300131204     d dORM01        e ds                  QUALIFIED INZ
014400131008
014500131008       // -?Partita IVA
014600131009     d wIVA            ds
014700131008     d  wISO                   1      2
014800131008     d  wPIVA                  3     16
014900131009
015000131009       //--------------------------------------------------------------
015100131009       //?Definizione schiere.
015200131009       //--------------------------------------------------------------
015300131009     d skprv           s              2a   dim(200)
015400131218     d sknaz           s              3a   dim(500)
015500160208     d sknazd          s             25a   dim(500)
015600131007
015700131007       //--------------------------------------------------------------
015800131007       //?Definizione campi.
015900131007       //--------------------------------------------------------------
016000131009     d dataoggi        s              8s 0
016100131220     d inv_dataritiro  s              8s 0
016200131010     d kpjba           s            502a
016300131205     d k_ORGfil        s                   like(ORGfil)
016400131205     d k_TBEcod        s                   like(TBEcod)
016500131205     d k_TBEke1        s                   like(TBEke1)
016600160315     d k_TBEke2        s                   like(TBEke2)
016700131205     d k_TBLcod        s                   like(TBLcod)
016800131205     d k_TBLkey        s                   like(TBLkey)
016900131010     d k_TBLkut        s                   like(TBLkut)
017000160315     d k_TAMksc        s                   like(TAMksc)
017100160315     d k_TAMctr        s                   like(TAMctr)
017200160315     d keyfilritiro    s                   like(wFiliale)
017300160315     d keytabella      s             15a
017400140411     d oraalfa         s              4a   inz
017500160216     d parametroNPR    s             10a   inz
017600160315     d wAGR            s               n   inz
017700160315     d wAmbito         s              1a   inz
017800140402     d wCap            s                   inz like(fiora0I0.vaocar)
017900140401     d wCodice         s             10s 0
018000131010     d wCom            s               n   inz
018100140408     d wData           s              8s 0 inz
018200131009     d wErr            s               n   inz
018300131219     d wErrMitt        s               n   inz
018400160315     d wFil            s              3s 0 inz
018500131204     d wFiliale        s                   like(tisi95ds.O95lna)
018600160315     d wForza          s               n   inz
018700140401     d wHH             s              2s 0
018800131009     d wIdMsg          s              7a
018900131008     d wIdCampo        s             10a
019000140402     d wIndirizzo      s                   inz like(fiora0I0.vaoinr)
019100140402     d wLocalita       s                   inz like(fiora0I0.vaolor)
019200140401     d wMM             s              2s 0
019300140402     d wNazione        s                   inz like(fiora0I0.vaonar)
019400160315     d wNetworkCon     s              3a
019500160315     d wNetworkEmi     s              3a
019600160315     d wNetworkOrd     s              3a
019700131205     d wNetworkRit     s              3a
019800140211     d wNrForza        s              3s 0
019900140211     d wNrfForza       s              3s 0
020000160315     d wOKffc          s               n   inz
020100131204     d wOKfilritiro    s               n   inz
020200160315     d wOKtariffa      s               n   inz
020300140401     d wOra            s              6s 0
020400131204     d wParm           s            512a   inz
020500131204     d wPeso           s                   like(tisi95ds.I95lkg)
020600160315     d wPickup         s                   like(tisi95ds.O95gf2)
020700140408     d wPor            s                   inz like(fiora0I0.vaopor)
020800140402     d wProvincia      s                   inz like(fiora0I0.vaoprr)
020900151008     d wVolume         s                   like(tisi95ds.I95lmc)
021000151008     d wVolumemax      s             10s 3
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
075400131007
075500131007       ENDSR;
075600140620
075700140620       //--------------------------------------------------------------
075800140620       //?Routine principale per view AS400.
075900140620       //--------------------------------------------------------------
076000140620       BEGSR  AS400;
076100160530
076200160530       //?Come prima cosa passo tutti i dati dalla ds di Input alla ds di Output
076300160530       //?in questo modo quando esco dal pgm per le forzature non perdo i dati
076400160530       //?immessi dall'utente
076500160530         eval-corr fiora0O0 = fiora0I0;
076600160530         reset fiora0O0.formato;
076700160530         reset fiora0O0.versione;
076800160322
076900160322       //?Controllo tipo Comunicazione ORM
077000160322         exsr TipoComOrm;
077100160315
077200160315       //?Controllo tipo ORM
077300160315         exsr TipoOrm;
077400140620
077500140620       //?Controllo cliente mittente
077600140620         IF  fiora0I0.vaocra > 0;
077700140620           exsr CodMittente;
077800160401       //?Controllo dati mittente
077900160401         ELSE;
078000160401           exsr Mittente;
078100160315         ENDIF;
078200140717
078300140717       //?Se manca il mittente ho finito i giochi, subito errore
078400140717         IF  wErrMitt;
078500140717           leavesr;
078600140717         ENDIF;
078700140620
078800160315       //?Calcola filiale emissione
078900140620         exsr FilEmissione;
079000160404
079100160404       //?Se filiale ritiro già impostata la metto subito nei dati di output
079200160404         IF  fiora0I0.vaopor > 0;
079300160404           fiora0O0.vaopor = fiora0I0.vaopor;
079400160404         ENDIF;
079500160404
079600160404       //?In caso di Immissione/Copia ORM
079700160601         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
079800160601             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
079900160601       //?Se mittente codificato devo calcolare subito la filiale ritiro
080000160601
080100160601       //?Se mittente codificato e filiale ritiro non impostata
080200160601       //?la calcolo subito
080300160601           IF  fiora0I0.vaocra > 0 and fiora0I0.vaopor = 0;
080400160404         //?Calcola filiale ritiro
080500160601             exsr FilRitiro;
080600160601           ENDIF;
080700160404         ENDIF;
080800151008
080900151008       //?Controllo le quantità
081000151008         exsr Quantita;
081100151008
081200151008       //?Calcolo il maggiore tra volume/quantità per instradamento
081300151008         exsr PesoVolume;
081400160315
081500160404       //?In caso di Immissione/Copia ORM
081600160404       //?Se mittente NON codificato calcolo ora la filiale ritiro
081700160404         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
081800160404             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
081900160525             fiora0I0.vaocra = 0;
082000160404         //?Calcola filiale ritiro
082100160404           exsr FilRitiro;
082200160404         ENDIF;
082300140620
082400140620       //?Controllo data ritiro
082500140620         exsr DataRitiro;
082600140620
082700140620       //?Controllo ora pronta merce
082800140620         exsr OraRitiro;
082900160404
083000160404       //?Se prima non ci sono stati errori
083100160404       //?Cerco gli orari servizio
083200160404         IF  fiora0M0.nrmsg = 0;
083300160404           exsr OrariServizio;
083400160404         //?Se data ritiro > oggi come orari servizio devo impostare
083500160404         //?gli orari min/max e non gli standard
083600160404           IF  inv_dataritiro > dataoggi;
083700160518           //fiora0O0.orainizio = trulor2ds.OOR2miis;
083800160518           //fiora0O0.orafine   = trulor2ds.OOR2mxfs;
083900160404           ENDIF;
084000160404         ENDIF;
084100160518
084200160518       //?Controllo ora pronta merce con orari servizio
084300160518         exsr OraRitiroServizio;
084400140620
084500140620       //?Controllo orari apertura
084600140620         exsr OrariApertura;
084700140620
084800140620       //?Controllo la natura della merce
084900140620         exsr NaturaMerce;
085000140620
085100140620       //?Controllo uno o più destinatario
085200140620         exsr UnoPiuDest;
085300140620
085400140620       //?Controllo Referente
085500140620         exsr Referente;
085600140620
085700140620       //?Controllo Telefono
085800140620         exsr Telefono;
085900160315
086000140717       //?Controllo chi paga
086100140717         exsr ChiPaga;
086200140620
086300140620       //?Controllo Mail
086400140620         exsr Mail;
086500140620
086600140620       //?Controllo SMS
086700140620         exsr Sms;
086800140620
086900140620       //?Controllo cliente destinatario
087000140620         IF  fiora0I0.vaocrc > 0;
087100140620           exsr CodDestinatario;
087200140620         ELSE;
087300140620       //?Controllo se deve esserci o no il Destinatario
087400140620           exsr Destinatario;
087500140620         ENDIF;
087600140620
087700140620       //?Controllo Fermo Deposito
087800140620         exsr FermoDeposito;
087900140620
088000140620       //?Controllo Alert
088100140620         exsr Alert;
088200160315
088300160315       //?Dalla versione 'B'
088400160315         IF  fiora0I0.versione > 'A';
088500160315         //?Controllo Mail x accettazione ORM
088600160315           exsr MailConferma;
088700160318         //?Controllo SMS x accettazione ORM
088800160318           exsr SmsConferma;
088900160315         //?Calcolo orario indicativo ritiro
089000160315           exsr OrarioIndRitiro;
089100160315         ENDIF;
089200160315
089300160315       //?Controllo la filiale di ritiro
089400160601         exsr ControllaFilRitiro;
089500160315
089600160315       //?Controllo cliente destinatario
089700160315         IF  fiora0I0.vaocrc > 0;
089800160315           exsr CodDestinatario;
089900160315         ENDIF;
090000160315       //?Controllo dati Destinatario
090100160315         exsr Destinatario;
090200160315
090300160315       //?Controllo cliente ordinante
090400160315         IF  fiora0I0.vaocor > 0;
090500160315           exsr Ordinante;
090600160315         ENDIF;
090700160315       //?Controllo dati Ordinante
090800160315         exsr DatiOrdinante;
090900160315
091000160315       //?Controllo Filiale Consegna
091100160315       //?Campo della DS previsto solo da versione 'B' in poi
091200160315         IF  fiora0I0.versione > 'A';
091300160315           exsr FilConsegna;
091400160315         ENDIF;
091500160315
091600160315       //?Controllo Cliente tassazione
091700160315         exsr ClienteKSC;
091800160315
091900160315       //?Campo della DS previsto solo da versione 'B' in poi
092000160315         IF  fiora0I0.versione > 'A';
092100160315       //?Cerco il giro di ritiro per il mittente codificato
092200160315           IF  fiora0I0.vaocra > 0;
092300160315             exsr GiroRitiro;
092400160315           ENDIF;
092500160315         ENDIF;
092600160315
092700160315       //?Calcola commissionato
092800160315         exsr Commissionato;
092900160315
093000160315       //?Controllo i dati per ORM commissionato
093100160315         exsr ControllaCommissionato;
093200140620
093300140620       ENDSR;
093400131008
093500131008       //--------------------------------------------------------------
093600131008       //?Controllo codice fiscale/partita iva.
093700131008       //--------------------------------------------------------------
093800131010       BEGSR  CodFiscPiva;
093900131009
094000131009         wErr = *off;
094100140211
094200140211         exec sql
094300140211         set :fiora0O0.codfiscale = ucase (:fiora0I0.codfiscale);
094400140211
094500140211         exec sql
094600140211         set :fiora0O0.partitaiva = ucase (:fiora0I0.partitaiva);
094700131008
094800131008       //?Accettiamo solo 1 dei 2, o CF o PI
094900131223         IF  fiora0O0.codfiscale <> *blanks and
095000131223             fiora0O0.partitaiva <> *blanks;
095100131008           wIdMsg   = 'TIS0240';
095200131008           wIdCampo = 'CODFISCALE';
095300131204           clear wParm;
095400131008           exsr Messaggi;
095500131009           wErr = *on;
095600131008         ENDIF;
095700131008
095800131008       //?Almeno 1 dei 2 deve essere presente, o CF o PI
095900131223         IF  fiora0O0.codfiscale = *blanks and
096000131223             fiora0O0.partitaiva = *blanks;
096100131008           wIdMsg   = 'TIS0240';
096200131008           wIdCampo = 'CODFISCALE';
096300131204           clear wParm;
096400131008           exsr Messaggi;
096500131009           wErr = *on;
096600131008         ENDIF;
096700131008
096800131008       //?Controllo CF
096900131223         IF  fiora0O0.codfiscale <> *blanks;
097000151020           clear xcfiva1ds;
097100151020           xcfiva1ds.xcfivapar = fiora0O0.codfiscale;
097200151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
097300151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
097400151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
097500151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
097600151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
097700151020           xcfivar1 (xcfiva1ds);
097800151020           IF  xcfiva1ds.xcfivaflg < *zeros;
097900131008             wIdMsg   = 'TIS0069';
098000131008             wIdCampo = 'CODFISCALE';
098100131204             clear wParm;
098200131008             exsr Messaggi;
098300131009             wErr = *on;
098400131008           ENDIF;
098500131008         ENDIF;
098600131008
098700131008       //?Partita IVA valida
098800131223         IF  fiora0O0.partitaiva <> *blanks;
098900140211           IF  %subst(fiora0O0.partitaiva:1:2) >= '00';
099000140211             wPIVA = fiora0O0.partitaiva;
099100140211           ELSE;
099200140211             wIVA = fiora0O0.partitaiva;
099300140211           ENDIF;
099400151020           clear xcfiva1ds;
099500151020           xcfiva1ds.xcfivamod = 'P';
099600151020           xcfiva1ds.xcfivapar = wIVA;
099700151020           xcfiva1ds.xcfivanar = fiora0I0.vaonar;
099800151020           xcfiva1ds.xcfivaprv = fiora0I0.vaoprr;
099900151020           xcfiva1ds.xcfivacap = fiora0I0.vaocar;
100000151020           xcfiva1ds.xcfivaloc = fiora0I0.vaolor;
100100151020           xcfiva1ds.xcfivapiva = fiora0O0.partitaiva;
100200151020           xcfivar1 (xcfiva1ds);
100300131009
100400131008         //?Errore forzabile
100500151020           IF  xcfiva1ds.xcfivaflg = -9;
100600131008             wIdMsg   = 'TIS0444';
100700131008             wIdCampo = 'PARTITAIVA';
100800140210             clear wParm;
100900131008             exsr Forzatura;
101000131009             wErr = *on;
101100131008           ENDIF;
101200131008
101300131008         //?Errore bloccante
101400151020           IF  xcfiva1ds.xcfivaflg < *zeros and xcfiva1ds.xcfivaflg <> -9;
101500131008             wIdMsg   = 'TIS0444';
101600131008             wIdCampo = 'PARTITAIVA';
101700131204             clear wParm;
101800131009             exsr Messaggi;
101900131009             wErr = *on;
102000131008           ENDIF;
102100131009
102200131009       //?Se errore non imposto i campi di output
102300131009          IF  wErr;
102400131009            leavesr;
102500131009          ENDIF;
102600131008
102700131008       //?Imposto il codice ISO che torna dal pgm di controllo
102800151020           IF  wISO <> %subst(xcfiva1ds.xcfivapar:3:2);
102900151020             wISO = %subst(xcfiva1ds.xcfivapar:3:2);
103000131008           ENDIF;
103100131008         ENDIF;
103200131007
103300131008       ENDSR;
103400131223
103500131223       //--------------------------------------------------------------
103600131223       //?Imposto i dati dell'ordinante.
103700131223       //--------------------------------------------------------------
103800131223       BEGSR  Ordinante;
103900131223
104000160520       //?Solo per richiamo da AS400
104100160520       //?torno il codice al chiamante
104200160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
104300160520           fiora0O0.vaocor = fiora0I0.vaocor;
104400160520         ENDIF;
104500160520
104600140401         wCodice = fiora0I0.vaocor;
104700140402         reset FIORB0OO;
104800140401
104900140401       //?codice valido
105000140402         IF  not IsCodiceValido(FIORB0OO);
105100140305           wIdMsg   = 'TIS0719';
105200140304           wIdCampo = 'VAOCOR';
105300140305           clear wParm;
105400131223           exsr Messaggi;
105500131223           leavesr;
105600131223         ENDIF;
105700160315
105800160315       //?Solo per richiamo da AS400
105900160315       //?e solo dalla versione 'B' in poi
106000160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
106100160315             fiorb0OO.versione > 'A';
106200160315       //?codice bloccato
106300160315           IF  fiorb0OO.frequenza = 'M';
106400160315             wIdMsg   = 'TIS0799';
106500160315             wIdCampo = 'VAOCOR';
106600160315             clear wParm;
106700160315             exsr Messaggi;
106800160315             leavesr;
106900160315           ENDIF;
107000160315         ENDIF;
107100131227
107200131227       //?cerco network della filiale del codice ordinante
107300160315         clear wNetworkOrd;
107400160315         wFil = %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
107500160315         wNetworkOrd = GetNtwFiliale(wFil);
107600131227         clear dNTW;
107700131227         k_TBEcod = 'NTW';
107800160315         k_TBEke1 = wNetworkOrd;
107900131227         chain (k_TBEcod:k_TBEke1) TNTBE01L;
108000131227         IF  %found(TNTBE01L);
108100131227           dNTW = TBEuni;
108200131227         ENDIF;
108300131227       //?se la filiale del codice ordinante è una filiale con network DPD
108400160315         IF  wNetworkOrd = FIORA00_NTW_DPD;
108500131227         //?imposto il servizio DPD
108600131227           fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
108700131227         ENDIF;
108800131227       //?se la filiale del codice ordinante è una filiale con network estero
108900160315         IF  dntw.§NTWfie = FIORA00_NTW_ESTERO;
109000131227         //?imposto il servizio Estero
109100131227           fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
109200131227         ENDIF;
109300160315
109400160315       //?Solo per richiamo da AS400
109500160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
109600160315       //?imposto i dati di ritorno dell'ordinante
109700160315           fiora0I0.vaorso = fiorb0OO.nome;
109800160315           fiora0I0.vaoino = fiorb0OO.indirizzo;
109900160315           fiora0I0.vaocao = fiorb0OO.cap;
110000160315           fiora0I0.vaoloo = fiorb0OO.localita;
110100160315           fiora0I0.vaopro = fiorb0OO.provincia;
110200160315           fiora0I0.vaonao = fiorb0OO.nazione;
110300160315         //?Se paga ordinante imposto i dati
110400160315           IF  fiora0I0.vaopag = FIORA00_PAGA_ORDINANTE and
110500160315               fiora0I0.vaoksc = 0 and fiorb0OO.codksc > 0;
110600160315             fiora0I0.vaoksc = fiorb0OO.codksc;
110700160315             IF  fiorb0OO.codtariffa = 999;
110800160315               clear fiora0I0.vaoctr;
110900160315             ELSE;
111000160315               fiora0I0.vaoctr =
111100160315               %subst(%editc(fiorb0OO.codtariffa:'X'):2:3);
111200160315             ENDIF;
111300160315           ENDIF;
111400160315         //?Imposto se contatto telefonico (commissionato)
111500160315         //?solo dalla versione 'B' in poi
111600160315           IF  fiorb0OO.versione > 'A';
111700160315             fiora0O0.contattot = fiorb0OO.contatto;
111800160315           ENDIF;
111900160315         ENDIF;
112000131223
112100131223       ENDSR;
112200160315
112300160315       //--------------------------------------------------------------
112400160315       //?Controllo dati Ordinante.
112500160315       //--------------------------------------------------------------
112600160315       BEGSR  DatiOrdinante;
112700160315
112800160315       //?Se non ho dati vado via
112900160315         IF  fiora0I0.vaorso = *blanks and fiora0I0.vaoino = *blanks and
113000160315             fiora0i0.vaoloo = *blanks and fiora0i0.vaocao = *blanks;
113100160315           leavesr;
113200160315         ENDIF;
113300160315
113400160315         exec sql
113500160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
113600160315
113700160315       //?Controllo nominativo ordinante
113800160315         exsr NomeOrdinante;
113900160315
114000160315       //?Controllo indirizzo ordinante
114100160315         exsr ViaOrdinante;
114200160315
114300160315       //?Controllo località ordinante
114400160315         exsr LocOrdinante;
114500160315
114600160315       //?Controllo cap ordinante
114700160315         exsr CapOrdinante;
114800160315
114900160315       //?Controllo provincia ordinante
115000160315         exsr ProvOrdinante;
115100160315
115200160315       //?Controllo nazione ordinante
115300160315         exsr NazOrdinante;
115400160315
115500160315       //?Controllo indirizzo completo dell'ordinante
115600160315         exsr IndTotOrdinante;
115700160315
115800160315       ENDSR;
115900160315
116000160315       //--------------------------------------------------------------
116100160315       //?Controllo nominativo ordinante.
116200160315       //--------------------------------------------------------------
116300160315       BEGSR  NomeOrdinante;
116400160315
116500160315         exec sql
116600160315         set :fiora0O0.vaorso = ucase (:fiora0I0.vaorso);
116700160315
116800160315       ENDSR;
116900160315
117000160315       //--------------------------------------------------------------
117100160315       //?Controllo indirizzo ordinante.
117200160315       //--------------------------------------------------------------
117300160315       BEGSR  ViaOrdinante;
117400160315
117500160315         exec sql
117600160315         set :fiora0O0.vaoino = ucase (:fiora0I0.vaoino);
117700160315
117800160315       ENDSR;
117900160315
118000160315       //--------------------------------------------------------------
118100160315       //?Controllo cap ordinante.
118200160315       //--------------------------------------------------------------
118300160315       BEGSR  CapOrdinante;
118400160315
118500160315         exec sql
118600160315         set :fiora0O0.vaocao = ucase (:fiora0I0.vaocao);
118700160315
118800160315       ENDSR;
118900160315
119000160315       //--------------------------------------------------------------
119100160315       //?Controllo località ordinante.
119200160315       //--------------------------------------------------------------
119300160315       BEGSR  LocOrdinante;
119400160315
119500160315         exec sql
119600160315         set :fiora0O0.vaoloo = ucase (:fiora0I0.vaoloo);
119700160315
119800160315       ENDSR;
119900160315
120000160315       //--------------------------------------------------------------
120100160315       //?Controllo provincia ordinante.
120200160315       //--------------------------------------------------------------
120300160315       BEGSR  ProvOrdinante;
120400160315
120500160315         exec sql
120600160315         set :fiora0O0.vaopro = ucase (:fiora0I0.vaopro);
120700160315
120800160315       //?Controllo con la sk delle provincie
120900160315         IF  fiora0O0.vaopro <> *blanks and
121000160315            %lookup(fiora0O0.vaopro:skprv) = 0;
121100160315           wIdMsg   = 'TIS0464';
121200160315           wIdCampo = 'VAOPRO';
121300160315           clear wParm;
121400160315           exsr Messaggi;
121500160315         ENDIF;
121600160315
121700160315       ENDSR;
121800160315
121900160315       //--------------------------------------------------------------
122000160315       //?Controllo nazione ordinante.
122100160315       //--------------------------------------------------------------
122200160315       BEGSR  NazOrdinante;
122300160315
122400160315         exec sql
122500160315         set :fiora0O0.vaonao = ucase (:fiora0I0.vaonao);
122600160315
122700160315         //?Controllo con la sk delle nazioni
122800160315         IF  fiora0O0.vaonao <> *blanks and
122900160315             %lookup(fiora0O0.vaonao:sknaz) = 0;
123000160315           wIdMsg   = 'TIS0385';
123100160315           wIdCampo = 'VAONAO';
123200160315           clear wParm;
123300160315           exsr Messaggi;
123400160315         ENDIF;
123500160315
123600160315       ENDSR;
123700160315
123800160315       //--------------------------------------------------------------
123900160315       //?Controllo indirizzo completo dell'ordinante.
124000160315       //--------------------------------------------------------------
124100160315       BEGSR  IndTotOrdinante;
124200160315
124300160315         clear TISI95DS;
124400160315         tisi95ds.I95tcn = '7';
124500160315         tisi95ds.I95nar = fiora0O0.vaonao;
124600160315         tisi95ds.I95cap = fiora0O0.vaocao;
124700160315         tisi95ds.I95loc = fiora0O0.vaoloo;
124800160315         tisi95ds.I95prv = fiora0O0.vaopro;
124900160315         tisi95ds.I95ind = fiora0O0.vaoino;
125000160315         tisi95ds.I95dat = %dec(%date());
125100160315
125200160315       //?richiamo il tisi95r
125300160315         ChkCapLocalita (tisi95ds);
125400160315
125500160315       //?errore o dato non affidabile
125600160315         IF  fiora0O0.vaonao = *blanks and tisi95ds.O95lia <> '3'  or
125700160315             fiora0O0.vaonao <> *blanks and tisi95ds.O95lia <> '2';
125800160315           wIdMsg   = 'TIS0054';
125900160315           wIdCampo = 'VAOLOO';
126000160315           clear wParm;
126100160315           exsr Messaggi;
126200160315           leavesr;
126300160315         ENDIF;
126400160315
126500160315       ENDSR;
126600160322
126700160322       //--------------------------------------------------------------
126800160322       //?Controllo Tipo Comunicazione ORM.
126900160322       //--------------------------------------------------------------
127000160322       BEGSR  TipoComOrm;
127100160322
127200160404         k_TBEcod = 'TCO';
127300160404         k_TBEke1 = fiora0I0.tco;
127400160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
127500160322         IF  not %found(TNTBE01L);
127600160404           wIdMsg   = 'TIS0832';
127700160404           wIdCampo = 'TCO';
127800160322           clear wParm;
127900160322           exsr Messaggi;
128000160322           leavesr;
128100160322         ENDIF;
128200160520
128300160520       //?Se richiamato da AS400
128400160520       //?se sono in WRITE ORM
128500160520       //?controllo il tipo comunicazione ORM con il tipo richiamo
128600160520       //?se richiamato da immissine manuale posso immettere solo
128700160520       //?'TELEFONICO' 'MAIL/FAX'
128800160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
128900160520             prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
129000160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_TELEFONICO and
129100160520             fiora0I0.tco <> FIORA00_TIPOCOMORM_MAIL;
129200160520           wIdMsg   = 'TIS0832';
129300160520           wIdCampo = 'VAOTCO';
129400160520           clear wParm;
129500160520           exsr Messaggi;
129600160520         ENDIF;
129700160520       //?se richiamato da immissione VAS posso avere solo
129800160520       //?'FILE' 'INTERNET'
129900160520       //?se richiamato da immissione FISSI posso avere solo
130000160520       //?'FISSO'
130100160520       //?se richiamato da immissione PREPAGATI posso avere solo
130200160520       //?'PREPAGATO'
130300160322
130400160322       ENDSR;
130500160315
130600160315       //--------------------------------------------------------------
130700160315       //?Controllo Tipo ORM.
130800160315       //--------------------------------------------------------------
130900160315       BEGSR  TipoOrm;
131000160315
131100160322         k_TBEcod = 'TOR';
131200160322         k_TBEke1 = fiora0I0.tor;
131300160322         chain (k_TBEcod:k_TBEke1) TNTBE01L;
131400160322         IF  not %found(TNTBE01L);
131500160322           wIdMsg   = 'TIS0797';
131600160322           wIdCampo = 'TOR';
131700160322           clear wParm;
131800160322           exsr Messaggi;
131900160322           leavesr;
132000160322         ENDIF;
132100160315
132200160315       //?Se tipo ORM = P imposto il flag di prepagato
132300160315       //?e che è un solo destinatario
132400160322         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
132500160322           fiora0O0.prepagato = FIORA00_PREPAGATO;
132600160404           fiora0I0.unopiudest = '1';
132700160322         ENDIF;
132800160322       //?Se tipo ORM = M imposto più destinatario
132900160322         IF  fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO;
133000160404           fiora0I0.unopiudest = '2';
133100160322         ENDIF;
133200160322       //?Se tipo ORM = S imposto 1 destinatario
133300160322         IF  fiora0I0.tor = FIORA00_TIPOORM_SINGOLO;
133400160404           fiora0I0.unopiudest = '1';
133500160322         ENDIF;
133600160520
133700160520       //?Se richiamato da AS400
133800160520       //?Tipo ORM multiplo e presenti i dati del destinatario errore
133900160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
134000160520             fiora0I0.tor = FIORA00_TIPOORM_MULTIPLO and
134100160520            (fiora0I0.vaocrc > 0 or fiora0I0.vaorsc <> *blanks);
134200160520           wIdMsg   = 'TIS0802';
134300160520           wIdCampo = 'VAOTOR';
134400160520           clear wParm;
134500160520           exsr Messaggi;
134600160520         ENDIF;
134700160315
134800160315       ENDSR;
134900131009
135000131009       //--------------------------------------------------------------
135100131009       //?Controllo codice mittente.
135200131009       //--------------------------------------------------------------
135300131010       BEGSR  CodMittente;
135400160315
135500160315         wErrMitt = *off;
135600140401
135700140401         wCodice = fiora0I0.vaocra;
135800140402         reset FIORB0OR;
135900131009
136000131009       //?codice valido
136100140402         IF not IsCodiceValido(FIORB0OR);
136200140312           wIdMsg   = 'TIS0068';
136300131009           wIdCampo = 'VAOCRA';
136400131204           clear wParm;
136500131009           exsr Messaggi;
136600160315         //?Solo per richiamo da AS400
136700160315           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
136800160315             wErrMitt = *on;
136900160315           ENDIF;
137000131009           leavesr;
137100131009         ENDIF;
137200160315
137300160315       //?Solo per richiamo da AS400
137400160315       //?e solo dalla versione 'B' in poi
137500160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
137600160315             fiorb0OR.versione > 'A';
137700160315         //?codice bloccato
137800160315           IF  fiorb0OR.frequenza = 'M';
137900160315             wIdMsg   = 'TIS0800';
138000160315             wIdCampo = 'VAOCRA';
138100160315             clear wParm;
138200160315             exsr Messaggi;
138300160315             wErrMitt = *on;
138400160315             leavesr;
138500160315           ENDIF;
138600160315         ENDIF;
138700140312
138800160315       //?Solo per richiamo da Internet
138900160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
139000140312       //?controllo se fa parte della stessa famiglia del codice di LOG
139100140313         IF not IsCodiceLegato();
139200140313           wIdMsg   = 'TIS0068';
139300140313           wIdCampo = 'VAOCRA';
139400140313           clear wParm;
139500140313           exsr Messaggi;
139600140313           leavesr;
139700140313         ENDIF;
139800160315         ENDIF;
139900140402
140000140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
140100140402         wIndirizzo = fiorb0OR.indirizzo;
140200140402         wCap = fiorb0OR.cap;
140300140402         wLocalita = fiorb0OR.localita;
140400140402         wProvincia = fiorb0OR.provincia;
140500140402         wNazione = fiorb0OR.nazione;
140600140411
140700140411       //?Solo per richiamo da Internet
140800140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
140900140411       //?normalizzo l'ora pronta merce dell'anagrafica clienti ritiro
141000140411             fiorb0OR.oraritiro > 0;
141100140411           oraalfa = GetOraAggiustata(fiorb0OR.oraritiro);
141200140411           fiorb0OR.oraritiro = %dec(oraalfa:4:0);
141300140411
141400151012           IF  fiorb0OR.oraritiro > *zeros and
141500151012              (fiora0I0.orrmin > 0 or fiora0I0.orrmax > 0);
141600140411             SELECT;
141700140411               WHEN fiorb0Or.oraritiro < fiora0I0.orrmin;
141800140411                 fiorb0OR.oraritiro = fiora0I0.orrmin;
141900140411               WHEN fiorb0Or.oraritiro > fiora0I0.orrmax;
142000140411                 fiorb0OR.oraritiro = fiora0I0.orrmax;
142100140411             ENDSL;
142200140411           ENDIF;
142300140411         ENDIF;
142400140717
142500140717       //?Solo per richiamo da AS400
142600160520       //?Imposto i dati del mittente
142700160519         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
142800160530           fiora0O0.vaorsr = fiora0I0.vaorsr;
142900160530           fiora0O0.vaoinr = fiora0I0.vaoinr;
143000160530           fiora0O0.vaocar = fiora0I0.vaocar;
143100160530           fiora0O0.vaolor = fiora0I0.vaolor;
143200160530           fiora0O0.vaoprr = fiora0I0.vaoprr;
143300160530           fiora0O0.vaonar = fiora0I0.vaonar;
143400160531       //-------------------
143500160531           IF  fiorb0OR.versione > 'Z';
143600160315         //?Se paga mittente imposto i dati
143700160315           IF  fiora0I0.vaopag = FIORA00_PAGA_MITTENTE and
143800160315               fiora0I0.vaoksc = 0 and fiorb0OR.codksc > 0;
143900160530             fiora0O0.vaoksc = fiorb0OR.codksc;
144000160315             IF  fiorb0OR.codtariffa = 999;
144100160530               clear fiora0O0.vaoctr;
144200160315             ELSE;
144300160530               fiora0O0.vaoctr =
144400160315               %subst(%editc(fiorb0OR.codtariffa:'X'):2:3);
144500160315             ENDIF;
144600160315           ENDIF;
144700160315
144800160315         //?msg INFO se sto sostituendo le note già messe dall'utente
144900160531           IF  (fiora0I0.vaono1 <> *blanks and
145000160531                fiora0I0.vaono1 <> fiorb0OR.nota1 and
145100160531                fiorb0OR.nota1 <> *blanks) or
145200160531               (fiora0I0.vaono2 <> *blanks and
145300160531                fiora0I0.vaono2 <> fiorb0OR.nota2 and
145400160531                fiorb0OR.nota2 <> *blanks);
145500160531             wIdMsg   = 'TIS0718';
145600160531             wIdCampo = 'VAONO1';
145700160531             wParm = 'ATTENZIONE!! Le note immesse sono state sovrascritte.';
145800160531             exsr Forzatura;
145900160531           ENDIF;
146000160531           IF  fiorb0OR.nota1 <> *blanks;
146100160531             fiora0O0.vaono1 = fiorb0OR.nota1;
146200160531           ENDIF;
146300160531           IF  fiorb0OR.nota2 <> *blanks;
146400160531             fiora0O0.vaono2 = fiorb0OR.nota2;
146500160531           ENDIF;
146600160531           IF  wForza;
146700160531             exsr RoutEnd;
146800160531           ENDIF;
146900160531           ENDIF;
147000140717         ENDIF;
147100131009
147200131009       ENDSR;
147300131219
147400131219       //--------------------------------------------------------------
147500131219       //?Controllo il mittente.
147600131219       //--------------------------------------------------------------
147700131219       BEGSR  Mittente;
147800131219
147900131219         wErrMitt = *off;
148000131219
148100131223       //?Se non ho nessun dato del cliente non controllo più niente
148200131219         IF  fiora0I0.vaorsr = *blanks and
148300131219             fiora0I0.vaoinr = *blanks and
148400131219             fiora0I0.vaocar = *blanks and
148500131219             fiora0I0.vaolor = *blanks and
148600131219             (fiora0I0.vaoprr = *blanks or
148700131219              fiora0I0.vaonar = *blanks);
148800140304           wIdMsg   = 'TIS0361';
148900131219           wIdCampo = 'VAORSR';
149000131219           clear wParm;
149100131219           exsr Messaggi;
149200131219           wErrMitt = *on;
149300131219           leavesr;
149400131219         ENDIF;
149500131219
149600131219       //?Nominativo mittente
149700131219         exsr NomeMittente;
149800131219
149900131219       //?Indirizzo mittente
150000131219         exsr ViaMittente;
150100160315
150200160315       //?Solo per richiamo da AS400
150300160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
150400160315         //?Controllo prima la località, poi provincia e cap
150500160315         //?Località mittente
150600160315           exsr LocMittente;
150700160315         //?Provincia mittente
150800160315           exsr ProvMittente;
150900160315         //?Cap mittente
151000160315           exsr CapMittente;
151100160520         ENDIF;
151200160520
151300160525       //?Per richiamo da INTERNET
151400160520         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
151500131219       //?Cap mittente
151600131219         exsr CapMittente;
151700131219
151800131219       //?Località mittente
151900131219         exsr LocMittente;
152000131219
152100131219       //?Provincia mittente
152200131219         exsr ProvMittente;
152300160315         ENDIF;
152400131219
152500131219       //?Nazione mittente
152600131219         exsr NazMittente;
152700131219
152800131219       //?Controllo indirizzo completo del mittente
152900131219         clear wPeso;
153000151008         clear wVolume;
153100131219         exsr IndTotMittente;
153200140402
153300140402       //?imposto l'indirizzo del mittente codificato in campi di comodo
153400140402         wIndirizzo = fiora0I0.vaoinr;
153500140402         wCap = fiora0I0.vaocar;
153600140402         wLocalita = fiora0I0.vaolor;
153700140402         wProvincia = fiora0I0.vaoprr;
153800140402         wNazione = fiora0I0.vaonar;
153900131219
154000131219       ENDSR;
154100131009
154200131009       //--------------------------------------------------------------
154300131009       //?Controllo nominativo mittente.
154400131009       //--------------------------------------------------------------
154500131010       BEGSR  NomeMittente;
154600140211
154700140211         exec sql
154800140211         set :fiora0O0.vaorsr = ucase (:fiora0I0.vaorsr);
154900131009
155000131219       //?Errore se non presente
155100131223         IF  fiora0O0.vaorsr = *blanks;
155200131205           wIdMsg   = 'TIS0482';
155300131009           wIdCampo = 'VAORSR';
155400131204           clear wParm;
155500131009           exsr Messaggi;
155600131219           wErrMitt = *on;
155700131009         ENDIF;
155800131009
155900131009       ENDSR;
156000131009
156100131009       //--------------------------------------------------------------
156200131009       //?Controllo indirizzo mittente.
156300131009       //--------------------------------------------------------------
156400131010       BEGSR  ViaMittente;
156500140211
156600140211         exec sql
156700140211         set :fiora0O0.vaoinr = ucase (:fiora0I0.vaoinr);
156800131009
156900131219       //?Errore se non presente
157000131223         IF  fiora0O0.vaoinr = *blanks;
157100131205           wIdMsg   = 'TIS0283';
157200131009           wIdCampo = 'VAOINR';
157300131204           clear wParm;
157400131009           exsr Messaggi;
157500131219           wErrMitt = *on;
157600131009         ENDIF;
157700131009
157800131009       ENDSR;
157900131009
158000131009       //--------------------------------------------------------------
158100131009       //?Controllo cap mittente.
158200131009       //--------------------------------------------------------------
158300131010       BEGSR  CapMittente;
158400140211
158500140211         exec sql
158600140211         set :fiora0O0.vaocar = ucase (:fiora0I0.vaocar);
158700131009
158800131219       //?Errore se non presente
158900131223         IF  fiora0O0.vaocar = *blanks;
159000131205           wIdMsg   = 'TIS0053';
159100131009           wIdCampo = 'VAOCAR';
159200131204           clear wParm;
159300131009           exsr Messaggi;
159400131219           wErr = *on;
159500131009         ENDIF;
159600131009
159700131009       ENDSR;
159800131009
159900131009       //--------------------------------------------------------------
160000131009       //?Controllo località mittente.
160100131009       //--------------------------------------------------------------
160200131010       BEGSR  LocMittente;
160300140211
160400140211         exec sql
160500140211         set :fiora0O0.vaolor = ucase (:fiora0I0.vaolor);
160600131219
160700131009       //?Errore se non presente codice e località
160800131223         IF  fiora0O0.vaolor = *blanks;
160900131205           wIdMsg   = 'TIS0347';
161000131009           wIdCampo = 'VAOLOR';
161100131204           clear wParm;
161200131009           exsr Messaggi;
161300131219           wErr = *on;
161400131009         ENDIF;
161500131009
161600131009       ENDSR;
161700131009
161800131009       //--------------------------------------------------------------
161900131009       //?Controllo provincia mittente.
162000131009       //--------------------------------------------------------------
162100131010       BEGSR  ProvMittente;
162200140211
162300140211         exec sql
162400140211         set :fiora0O0.vaoprr = ucase (:fiora0I0.vaoprr);
162500131009
162600131009       //?Controllo con la sk delle provincie
162700131009         IF  fiora0O0.vaoprr <> *blanks and
162800131009             %lookup(fiora0O0.vaoprr:skprv) = 0;
162900131205           wIdMsg   = 'TIS0466';
163000131009           wIdCampo = 'VAOPRR';
163100131204           clear wParm;
163200131009           exsr Messaggi;
163300131219           wErrMitt = *on;
163400131009         ENDIF;
163500131009
163600131009       ENDSR;
163700131008
163800131009       //--------------------------------------------------------------
163900131009       //?Controllo nazione mittente.
164000131009       //--------------------------------------------------------------
164100131010       BEGSR  NazMittente;
164200140211
164300140211         exec sql
164400140211         set :fiora0O0.vaonar = ucase (:fiora0I0.vaonar);
164500131009
164600131009       //?Controllo con la sk delle provincie
164700131009         IF  fiora0O0.vaonar <> *blanks and
164800131009             %lookup(fiora0O0.vaonar:sknaz) = 0;
164900140326           wIdMsg   = 'TIS0386';
165000131009           wIdCampo = 'VAONAR';
165100131204           clear wParm;
165200131009           exsr Messaggi;
165300131219           wErrMitt = *on;
165400131009         ENDIF;
165500131009
165600131009       ENDSR;
165700131009
165800131009       //--------------------------------------------------------------
165900131009       //?Controllo indirizzo completo del mittente.
166000131009       //--------------------------------------------------------------
166100131010       BEGSR  IndTotMittente;
166200131009
166300131009         clear TISI95DS;
166400151009         clear FNLV13DS;
166500131204         tisi95ds.I95tcn = '7';
166600131204         tisi95ds.I95nar = fiora0O0.vaonar;
166700131204         tisi95ds.I95cap = fiora0O0.vaocar;
166800131204         tisi95ds.I95loc = fiora0O0.vaolor;
166900131204         tisi95ds.I95prv = fiora0O0.vaoprr;
167000131204         tisi95ds.I95ind = fiora0O0.vaoinr;
167100131204         tisi95ds.I95dat = %dec(%date());
167200131204         tisi95ds.I95lkg = wPeso;
167300151008         tisi95ds.I95lmc = wVolume;
167400151009         fnlv13ds.I13af0 = 'S';
167500151009         fnlv13ds.I13cnv = 'S';
167600151009         fnlv13ds.I13la3 = 'S';
167700131230
167800131230       //?Se richiesto ntw DPD lo passo
167900160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
168000131230           tisi95ds.I95fi1 = fiora0I0.sceltantw;
168100131230         ENDIF;
168200131009
168300131009       //?Solo per richiamo da Internet
168400131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
168500131009         //?se richiesta nazione irlanda forzo cap eire
168600131204           IF  tisi95ds.I95nar = FIORA00_IRLANDA;
168700131204             tisi95ds.I95cap =  FIORA00_EIRE;
168800131009           ENDIF;
168900131205         ENDIF;
169000131205
169100151009       //?controllo cap/località/provincia
169200151009         fnlv13r (kpjba:fnlv13ds:tisi95ds);
169300160525       //?Solo per richiamo da Internet
169400160525       //?controllo gli errori, da AS ci pensa già il programma chiamante
169500160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
169600151016       //?se torna errore per cap generico diversifico il messaggio
169700151016         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
169800151016             %scan('generico':fnlv13ds.O13msg) > 0;
169900151016           wIdMsg   = 'TIS0826';
170000151016           wIdCampo = 'VAOCAR';
170100151016           clear wParm;
170200151016           exsr Messaggi;
170300151016           wErrMitt = *on;
170400151016           leavesr;
170500151016         ENDIF;
170600151009       //?se torna errore imposto messaggio generico
170700151009         IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
170800151016           wIdMsg   = 'TIS0056';
170900131205           wIdCampo = 'VAOLOR';
171000131205           clear wParm;
171100131205           exsr Messaggi;
171200131219           wErrMitt = *on;
171300131205           leavesr;
171400131205         ENDIF;
171500160525         ENDIF;
171600131009
171700131009       //?Imposto la linea di instradamento
171800131204         wFiliale = tisi95ds.O95lna;
171900160315
172000160315       //?Imposto la pickup area
172100160315         wPickup = tisi95ds.O95gf2;
172200131009
172300131009       ENDSR;
172400140311
172500140311       //--------------------------------------------------------------
172600140311       //?Controllo chi paga.
172700140311       //--------------------------------------------------------------
172800140311       BEGSR  ChiPaga;
172900140311
173000140311       //?Controllo formale del dato
173100140401         IF  fiora0I0.vaopag <> FIORA00_PAGA_MITTENTE and
173200140401             fiora0I0.vaopag <> FIORA00_PAGA_ORDINANTE and
173300140401             fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
173400140311           wIdMsg   = 'TIS0140';
173500140311           wIdCampo = 'VAOPAG';
173600140311           clear wParm;
173700140311           exsr Messaggi;
173800140311           leavesr;
173900140311         ENDIF;
174000140311
174100140311       //?Solo per richiamo da Internet
174200140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
174300140311         //?Se utente loggato e ritiro presso altro luogo
174400140311         //?il pagamento non può essere a carico del mittente
174500140401           IF  fiora0I0.IdUtente > 0 and fiora0I0.vaocra = 0 and
174600140401               fiora0I0.vaopag = FIORA00_PAGA_MITTENTE;
174700140311             wIdMsg   = 'TIS0140';
174800140311             wIdCampo = 'VAOPAG';
174900140311             clear wParm;
175000140311             exsr Messaggi;
175100140311           ENDIF;
175200140311         ENDIF;
175300140311
175400140311       //?Ritiro all'estero non può pagare il mittente
175500140401         IF  fiora0I0.vaopag = FIORA00_PAGA_MITTENTE and
175600140402             wNazione <> *blanks and
175700140402             wNazione <> FIORA00_SANMARINO;
175800140311           wIdMsg   = 'TIS0695';
175900140311           wIdCampo = 'VAOPAG';
176000140311           clear wParm;
176100140311           exsr Messaggi;
176200140402         ENDIF;
176300140311
176400140311       //?Solo per richiamo da Internet
176500140311         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
176600140311       //?Imposto se prepagato
176700140401           IF  fiora0I0.vaocor = *zeros and
176800140401               fiora0I0.vaopag <> FIORA00_PAGA_DESTINATARIO;
176900140311             fiora0O0.prepagato = FIORA00_PREPAGATO;
177000140311           ENDIF;
177100140311         ENDIF;
177200140311
177300140311       ENDSR;
177400140404
177500140404       //--------------------------------------------------------------
177600140404       //?Controllo commissionato.
177700140404       //--------------------------------------------------------------
177800140404       BEGSR  Commissionato;
177900140404
178000140407         wCom = *off;
178100140404
178200140404       //?Solo per richiamo da Internet
178300140404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
178400140404
178500140404         //?Contatto telefonico
178600140404           exec sql
178700140404           set :fiora0O0.contattot = ucase (:fiora0I0.contattot);
178800140404
178900140404         //?valido solo se Blank, N o S
179000140404           IF  fiora0O0.contattot <> *blanks and
179100140404               fiora0O0.contattot <> 'S' and
179200140404               fiora0O0.contattot <> 'N';
179300140404             wIdMsg   = 'TIS0720';
179400140404             wIdCampo = 'CONTATTOT';
179500140404             clear wParm;
179600140404             exsr Messaggi;
179700140404           ENDIF;
179800140404
179900140404         //?Ordinante non codificato (è utente senza PSW)
180000160315         //?se il flag non è già impostato
180100140404         //?è sempre commissionato
180200140404           IF  fiora0I0.vaocor = 0;
180300140404             IF  fiora0O0.contattot = *blanks;
180400140404               fiora0O0.contattot = 'S';
180500140404             ENDIF;
180600140404           ENDIF;
180700140404
180800140404           IF  fiora0O0.contattot = 'N';
180900140407             wCom = *off;
181000140404           ENDIF;
181100140404           IF  fiora0O0.contattot = 'S';
181200140407             wCom = *on;
181300140404           ENDIF;
181400140404         ENDIF;
181500140404
181600140404       ENDSR;
181700140404
181800140404       //--------------------------------------------------------------
181900140404       //?Cerco orari servizio.
182000140404       //--------------------------------------------------------------
182100140404       BEGSR  OrariServizio;
182200140408
182300140408         wPor  = fiora0O0.vaopor;
182400140408         wData = inv_dataritiro;
182500140408
182600140408       //?Richiamo il pgm TRULORSR
182700140408         exsr CallTrulORS;
182800140404
182900140404       //?Orari Standard Servizio
183000140404         fiora0O0.orainizio = trulor2ds.OOR2stis;
183100140404         fiora0O0.orafine   = trulor2ds.OOR2stfs;
183200140404
183300140404       //?Orari Limiti
183400140404         fiora0O0.oracomm = trulor2ds.OOR2lrsc;
183500140404         fiora0O0.oranocomm = trulor2ds.OOR2lrnc;
183600160526
183700160526       //?Se ritiro estero chiamo nuovo pgm per ricerca orari servizio
183800160526         IF  fiora0O0.vaonar <> *blanks;
183900160526           exsr CallFIORE;
184000160526         //?Orari Standard Servizio
184100160526           fiora0O0.orainizio = fiore00ds.OORE00min;
184200160526           fiora0O0.orafine   = fiore00ds.OORE00max;
184300160526         //?Orari Limiti
184400160526           fiora0O0.oracomm   = fiore00ds.OORE00lrg;
184500160526           fiora0O0.oranocomm = fiore00ds.OORE00lrg;
184600160526         ENDIF;
184700140404
184800140404       ENDSR;
184900131205
185000131205       //--------------------------------------------------------------
185100131205       //?Controllo filiale emissione.
185200131205       //--------------------------------------------------------------
185300131205       BEGSR  FilEmissione;
185400131205
185500131205       //?Se richiamo da Internet
185600131205         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
185700131205         //?Se ordinante codificato (è utente con PSW)
185800131205         //?e mittente non codificato
185900131205         //?filiale emissione del codice ordinante
186000140401           IF  fiora0I0.vaocor > 0 and fiora0I0.vaocra = 0;
186100131205             fiora0O0.vaopoe =
186200140401             %dec(%subst(%editc(fiora0I0.vaocor:'X'):1:3):3:0);
186300131205           ENDIF;
186400131205         //?Se ordinante non codificato (è utente senza PSW)
186500131205         //?filiale emissione da instradamento
186600140401           IF  fiora0I0.vaocor = 0;
186700131205             fiora0O0.vaopoe = wFiliale;
186800131205           ENDIF;
186900131205         ENDIF;
187000140402
187100140402         //?Se mittene codificato filiale emissione da FNACR
187200160519         //?solo se richiamo da internet
187300160519         IF  fiora0I0.vaocra > 0 and fiora0O0.vaopoe = 0 and
187400160519             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
187500140711           fiora0O0.vaopoe = fiorb0OR.filemi;
187600140402         ENDIF;
187700160315
187800160315       //?Se richiamo da AS400
187900160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
188000160315           fiora0O0.vaopoe = fiora0I0.vaopoe;
188100160524         //?Carico tabella DFT ORM della filiale emissione
188200160524         //?se c'è
188300160524           k_TBEcod = 'DFT';
188400160524           k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
188500160524           chain (k_TBEcod:k_TBEke1) TNTBE01L;
188600160524           IF  %found(TNTBE01L) and TBEatb = *blanks;
188700160524             dDFT = TBEuni;
188800160524           ENDIF;
188900160315         ENDIF;
189000131205
189100131205       //?Cerco il network della filiale emissione
189200131219         IF  fiora0O0.vaopoe > *zeros;
189300160315           wNetworkEmi = GetNtwFiliale(fiora0O0.vaopoe);
189400131219         ENDIF;
189500131205
189600131205       ENDSR;
189700131205
189800131205       //--------------------------------------------------------------
189900131205       //?Controllo filiale ritiro.
190000131205       //--------------------------------------------------------------
190100131205       BEGSR  FilRitiro;
190200131205
190300131205         wOKfilritiro = *off;
190400131205
190500131205       //?Cliente mittente codificato
190600131205       //?la filile ritiro è già calcolata
190700140401         IF  fiora0I0.vaocra > 0;
190800140402           wFiliale = fiorb0OR.filrit;
190900131205           wOKfilritiro = *on;
191000131205         ENDIF;
191100131205
191200131205       //?Calcolo la filiale ritiro
191300151008       //?Se cliente mittente NON codificato
191400151008       //?calcolo la filiale ritiro in base al cappario
191500131205         IF  not wOKfilritiro;
191600151008           exsr IndTotMittente;
191700131205         ENDIF;
191800131205
191900131205       //?Richiamo pgm per Forzature da fare sulla Filiale di Ritiro
192000131205         clear FIOR96ds;
192100160527         fior96ds.IOR96poe = fiora0O0.vaopoe;
192200131205         fior96ds.IOR96por = wFiliale;
192300131205         %subst(kpjba:92:10) = 'GAITRA201';
192400131205         fior96r (kpjba : fior96ds);
192500131205         IF  fior96ds.OOR96err = *blanks and fior96ds.OOR96por <> *zeros;
192600131205           wFiliale = fior96ds.OOR96por;
192700160601         //?In caso di richiamo da AS400
192800160601         //?pulisco il campo della filiale ritiro passato
192900160601         //?in questo modo forzo sempre anche se impostata a mano.
193000160601           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
193100160601             clear fiora0I0.vaopor;
193200160601           ENDIF;
193300131205         ENDIF;
193400160525
193500160525       //?In caso di richiamo in WRITE (Immissione/Copia ORM)
193600160525         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
193700160525             fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
193800160525       //?Se la filale ritiro è una filiale gestita devo cercare le filiale Mamma
193900160525       //?per impostarla come filiale ritiro
194000160525           clear FNLV55DS;
194100160525           fnlv55ds.D55tpt = '6';
194200160525           fnlv55ds.D55lin = wFiliale;
194300160525           fnlv55ds.D55drf = %dec(%date());
194400160525           fnlv55r (FNLV55DS);
194500160525           IF  fnlv55ds.D55err = *blanks;
194600160525             wFiliale = fnlv55ds.D55tfa;
194700160525           ENDIF;
194800160525         ENDIF;
194900131205
195000160525       //?Se richiamo da Internet
195100160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
195200131205         fiora0O0.vaopor = wFiliale;
195300160525         ENDIF;
195400160404
195500160404       //?Se richiamo da AS400 e la filiale ritiro non è stata passata
195600160404       //?la imposto ora
195700160404         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
195800160404             fiora0I0.vaopor = 0;
195900160404           fiora0I0.vaopor = wFiliale;
196000160525           fiora0O0.vaopor = wFiliale;
196100160404         ENDIF;
196200131205
196300131205       //?Imposto se ritiro all'estero
196400160315         wNetworkRit = GetNtwFiliale(fiora0O0.vaopor);
196500131205         fiora0O0.ritiroest = FIORA00_RITIROESTERO_NO;
196600131205         clear dNTW;
196700131205         k_TBEcod = 'NTW';
196800160315         k_TBEke1 = wNetworkRit;
196900131205         chain (k_TBEcod:k_TBEke1) TNTBE01L;
197000131205         IF  %found(TNTBE01L);
197100131205           dNTW = TBEuni;
197200131205         ENDIF;
197300160315         IF  wNetworkRit = FIORA00_NTW_DPD or
197400131205             dntw.§NTWfie = FIORA00_NTW_ESTERO;
197500131205           fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
197600131205         ENDIF;
197700131205
197800131205       //?Verifico la ritirabilità all'estero
197900131205         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
198000140402             wNazione <> *blanks;
198100131205           clear FNLV12DS;
198200131205           clear TISI95DS;
198300131205           clear TISI97DS;
198400131230           fnlv12ds.I12lang = fiora0I0.idlingua;
198500140402           tisi95ds.I95nar = wNazione;
198600140402           tisi95ds.I95cap = wCap;
198700131205           tisi95ds.I95dat = dataoggi;
198800131205           tisi97ds.I97ntw = wNetworkRit;
198900160315           IF  wNetworkRit = FIORA00_NTW_DPD;
199000160315             tisi95ds.I95fi1 = FIORA00_SERVIZIO_DPD;
199100131205           ENDIF;
199200131205           fnlv12r (fnlv12ds:tisi95ds:tisi97ds);
199300131205           IF  fnlv12ds.O12err <> *blanks;
199400140304             wIdMsg   = 'TIS0718';
199500160315           //?Solo per richiamo da Internet
199600160315             IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
199700131205             wIdCampo = 'SCELTANTW';
199800160315             ELSE;
199900160315               wIdCampo = 'VAOPOR';
200000160315             ENDIF;
200100131205             wParm = fnlv12ds.O12msg;
200200131205             exsr Messaggi;
200300131205           ENDIF;
200400131227
200500131227         //?Imposto il ntw da forzare per ritiro all'estero
200600131227           IF  fiora0I0.sceltantw <> *blanks;
200700131227             fiora0O0.sceltantw = fiora0I0.sceltantw;
200800131227             fiora0O0.forzantw = fiora0I0.sceltantw;
200900131227           ELSE;
201000160315             IF  wNetworkRit = FIORA00_NTW_DPD;
201100131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
201200131227             ENDIF;
201300160315             IF  wNetworkRit = FIORA00_NTW_EEX;
201400131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
201500131227             ENDIF;
201600131227           ENDIF;
201700131205
201800131227         ENDIF;
201900160525
202000160525       //?Se richiamato da AS400
202100160525         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
202200160525         //?Imposto il ntw da forzare per ritiro all'estero
202300160525           IF  fiora0I0.sceltantw <> *blanks;
202400160525             fiora0O0.sceltantw = fiora0I0.sceltantw;
202500160525             fiora0O0.forzantw = fiora0I0.sceltantw;
202600160525           ELSE;
202700160525             IF  wNetworkRit = FIORA00_NTW_DPD;
202800160525               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
202900160525             ENDIF;
203000160525             IF  wNetworkRit = FIORA00_NTW_EEX;
203100160525               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX;
203200160525             ENDIF;
203300160525           ENDIF;
203400160526         //?In caso di richiamo in WRITE (Immissione/Copia ORM)
203500160526           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE;
203600160526         //?Messaggio di avvertimento se filiale ritiro calcolata <> da quella del video
203700160526             IF  wFiliale > 0 and fiora0I0.vaopor > 0 and
203800160526                 wFiliale <> fiora0I0.vaopor;
203900160526               wIdMsg   = 'TIS0833';
204000160526               wIdCampo = 'VAOPOR';
204100160526               clear wParm;
204200160526               exsr Forzatura;
204300160531               IF  wForza;
204400160531                 exsr RoutEnd;
204500160531               ENDIF;
204600160526             ENDIF;
204700160526           ENDIF;
204800160525         ENDIF;
204900151008
205000151120         //?Se ordinante NON codificato quindi utente senza PSW e
205100151008         //?Se mittene NON codificato reimposto la filiale emissione
205200151008         //?la filiale ritiro potrebbe essere variata a causa delle quantità
205300151008         //?inserite
205400160601         //?SOLO se richiamato da Internet
205500160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
205600151120         IF  not wOKfilritiro and fiora0I0.vaocor = 0;
205700151008           fiora0O0.vaopoe = wFiliale;
205800151008         ENDIF;
205900160601         ENDIF;
206000131227
206100131205       ENDSR;
206200160315
206300160315       //--------------------------------------------------------------
206400160315       //?Controllo filiale ritiro.
206500160315       //--------------------------------------------------------------
206600160601       BEGSR  ControllaFilRitiro;
206700160315
206800160315       //?Filiale ritiro obbligatoria
206900160315         IF  fiora0I0.vaopor = 0;
207000160315           wIdMsg   = 'TIS0809';
207100160315           wIdCampo = 'VAOPOR';
207200160315           clear wParm;
207300160315           exsr Messaggi;
207400160315           leavesr;
207500160315         ENDIF;
207600160315
207700160601       //?Controllo se la filiale ritiro è valida
207800160601         IF  not IsFilValida(fiora0O0.vaopor);
207900160601           wIdMsg   = 'TIS0809';
208000160601           wIdCampo = 'VAOPOR';
208100160601           clear wParm;
208200160601           exsr Messaggi;
208300160601           leavesr;
208400160315         ENDIF;
208500160315       //?Errore se la filiale ritiro NON ha la procedura ORM attiva
208600160315         IF  not IsFilAttivaORM(fiora0O0.vaopor);
208700160315           wIdMsg   = 'TIS0718';
208800160315           wIdCampo = 'VAOPOR';
208900160315           wParm = 'La Fil.Ritiro non ha la proc.ORM attiva. +
209000160315           Commissionare per posta elettronica';
209100160315           exsr Messaggi;
209200160315         ENDIF;
209300160315
209400160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
209500160315       //?controllo congruenza tra network DPD e filiale ritiro
209600160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
209700160315             wNetworkRit <> FIORA00_NTW_DPD and
209800160315             fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD;
209900160315           wIdMsg   = 'TIS0718';
210000160315           wIdCampo = 'VAOPOR';
210100160315           wParm = 'Indicato Network DPD. Incongruenza con la +
210200160315           Filiale ritiro.';
210300160315           exsr Messaggi;
210400160315         ENDIF;
210500160315
210600160315       //?Controllo con cappario DPD se ritiro per DPD
210700160315         IF  fiora0I0.sceltantw = FIORA00_SERVIZIO_DPD or
210800160315             wNetworkRit = FIORA00_NTW_DPD;
210900160315           IF  not IsValidoCapDPD(fiora0O0.vaonar:
211000160315                                  fiora0O0.vaocar:
211100160315                                  fiora0O0.vaodar:
211200160315                                  fiora0I0.vaopkg:
211300160315                                  ds3idp.§3Ilmp:
211400160315                                  fiora0O0.vaopoe);
211500160315             wIdMsg   = 'TIS0811';
211600160315             wIdCampo = 'VAOCAR';
211700160315             clear wParm;
211800160315             exsr Messaggi;
211900160315           ENDIF;
212000160315         ENDIF;
212100160315
212200160315       //?Solo per richiamo IN WRITE (Immissione/Copia ORM)
212300160315       //?e filiale ritiro = a chi sta inserendo ORM
212400160315       //?non posso fare un prepagato
212500160315         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
212600160315             fiora0O0.vaopor = fiora0I0.fgs and
212700160315             fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO;
212800160315           wIdMsg   = 'TIS0718';
212900160315           wIdCampo = 'VAOTOR';
213000160315           wParm = 'ORM prepagato non possibile. Inserire prima +
213100160315                    il prepagato!!';
213200160315           exsr Messaggi;
213300160315         ENDIF;
213400160315
213500160315       //?Non possibile ORM prepagato se ORM Export
213600160315         IF  fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO and
213700160315            (wNetworkRit = FIORA00_SERVIZIO_DPD or
213800160315             wNetworkRit = FIORA00_SERVIZIO_EEX);
213900160315           wIdMsg   = 'TIS0718';
214000160315           wIdCampo = 'VAOTOR';
214100160315           wParm = 'Per ORM Export non si può fare un ORM +
214200160315                    prepagato!!';
214300160315           exsr Messaggi;
214400160315         ENDIF;
214500160527
214600160527       //?Se richiesto il ritiro tramite DPD SOLO 1 destinatario
214700160527         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
214800160527             fiora0O0.unopiudest <> '1';
214900160527           wIdMsg   = 'TIS0307';
215000160527           wIdCampo = 'UNOPIUDEST';
215100160527           clear wParm;
215200160527           exsr Messaggi;
215300160527         ENDIF;
215400160315
215500160315       ENDSR;
215600131205
215700131205       //--------------------------------------------------------------
215800131205       //?Controllo data ritiro.
215900131205       //--------------------------------------------------------------
216000131205       BEGSR  DataRitiro;
216100131205
216200131223       //?Calcolo la data ritiro
216300131205         clear FIOR97ds;
216400131205         fior97ds.IOR97poe = fiora0O0.vaopoe;
216500131205         fior97ds.IOR97por = fiora0O0.vaopor;
216600140402         fior97ds.IOR97cap = wCap;
216700140402         fior97ds.IOR97loc = wLocalita;
216800160526         fior97ds.IOR97naz = wNazione;
216900131205         IF  fiora0I0.vaodao = 0;
217000131205           fior97ds.IOR97dao = dataoggi;
217100131205         ELSE;
217200131205           fior97ds.IOR97dao = fiora0I0.vaodao;
217300131205         ENDIF;
217400131205         IF  fiora0I0.vaooao = 0;
217500131205           fior97ds.IOR97oao = %dec(%time());
217600131205         ELSE;
217700131205           fior97ds.IOR97oao = fiora0I0.vaooao;
217800131205         ENDIF;
217900131205         fior97ds.IOR97gf2 = tisi95ds.O95gf2;
218000131205         IF  wCom;
218100131205           fior97ds.IOR97com = 'S';
218200131205         ENDIF;
218300140331         SELECT;
218400140331           WHEN fiora0O0.prepagato = FIORA00_PREPAGATO;
218500140331             fior97ds.IOR97tor = 'P';
218600140331           WHEN fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
218700140331             fior97ds.IOR97tor = 'I';
218800140331           OTHER;
218900140331             fior97ds.IOR97tor = 'M';
219000140331         ENDSL;
219100131205         IF  fiora0I0.vaocra > 0;
219200131205           fior97ds.IOR97mcod = 'S';
219300131205         ENDIF;
219400160315         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
219500131205         fior97ds.IOR97web = 'S';
219600160315         ENDIF;
219700160601
219800160601         //?se ORM manuale e ci sono i dati per Alert Commissionato
219900160601         //?e l'ORM è un commissionato passo il flag
220000160601         IF  (fiora0I0.tco = FIORA00_TIPOCOMORM_TELEFONICO or
220100160601              fiora0I0.tco = FIORA00_TIPOCOMORM_MAIL) and wCom and
220200160601             (fiora0I0.mail <> *blanks or fiora0I0.sms <> *blanks);
220300160601           fior97ds.IOR97alert = 'S';
220400160601         ENDIF;
220500160601
220600131205         fior97r(kpjba : fior97ds);
220700160601
220800131205         IF  fior97ds.OOR97err = *blanks and fior97ds.OOR97dar <> *zeros;
220900131220           inv_dataritiro = fior97ds.OOR97dar;
221000131205         ENDIF;
221100131205
221200131220         //?se non è stata immessa imposto quella calcolata
221300131220         IF  fiora0I0.vaodar = 0;
221400131220           fiora0O0.vaodar = inv_dataritiro;
221500131220         ELSE;
221600131220           fiora0O0.vaodar = fiora0I0.vaodar;
221700131220         ENDIF;
221800160601
221900160601       //?Se richiamato per IMMISSIONE ORM
222000160601       //?e posso anticipare la data passo il flag
222100160601         IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
222200160601             fior97ds.OOR97dmin < fior97ds.OOR97dar;
222300160601           fiora0O0.anticipa = 'A';
222400160601         ENDIF;
222500131223
222600131223         //?Controlli sulla data
222700131223
222800131223         //?Non accetto a 0
222900131223         IF  fiora0O0.vaodar = 0;
223000140407           wIdMsg   = 'TIS0259';
223100131223           wIdCampo = 'VAODAR';
223200131223           clear wParm;
223300131223           exsr Messaggi;
223400131223           leavesr;
223500131223         ENDIF;
223600131223         //?Non deve essere inferiore alla data minima
223700131223         IF  fiora0O0.vaodar < fior97ds.OOR97dmin;
223800140407           wIdMsg   = 'TIS0730';
223900131223           wIdCampo = 'VAODAR';
224000140407           IF  wCom;
224100140407             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
224200140407           ELSE;
224300140407             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
224400140407           ENDIF;
224500160526           IF  fiora0I0.vaonar <> *blanks;
224600160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
224700160526           ENDIF;
224800131223           exsr Messaggi;
224900131223         ENDIF;
225000131223         //?Non deve essere superiore alla data massima
225100131223         IF  fiora0O0.vaodar > fior97ds.OOR97dmaxb;
225200131223           wIdMsg   = 'TIS0116';
225300131223           wIdCampo = 'VAODAR';
225400131223           clear wParm;
225500131223           exsr Messaggi;
225600131223         ENDIF;
225700131205
225800131205       ENDSR;
225900131011
226000131011       //--------------------------------------------------------------
226100131011       //?Controllo ora pronta merce.
226200131011       //--------------------------------------------------------------
226300131011       BEGSR  OraRitiro;
226400131219
226500131223         IF  fiora0I0.vaoorr > 0;
226600131223           fiora0O0.vaoorr = fiora0I0.vaoorr;
226700131223         ENDIF;
226800131028
226900140407         IF  fiora0O0.vaoorr = *zeros and not wCom;
227000131028           wIdMsg   = 'TIS0423';
227100131028           wIdCampo = 'VAOORR';
227200131204           clear wParm;
227300131028           exsr Messaggi;
227400131219           leavesr;
227500131028         ENDIF;
227600131028
227700140407         IF  wCom and fiora0O0.vaoorr = 0;
227800131220           leavesr;
227900131220         ENDIF;
228000131220
228100131223         IF  fiora0O0.vaoorr < 1 or fiora0O0.vaoorr > 2359;
228200131028           wIdMsg   = 'TIS0425';
228300131028           wIdCampo = 'VAOORR';
228400131204           clear wParm;
228500131028           exsr Messaggi;
228600131028         ENDIF;
228700160404
228800160404       ENDSR;
228900160404
229000160404       //--------------------------------------------------------------
229100160404       //?Controllo ora pronta merce con orari servizio.
229200160404       //--------------------------------------------------------------
229300160404       BEGSR  OraRitiroServizio;
229400140306
229500140306       //?Il controllo dell'ora pronta merce non lo faccio se ritiro estero
229600140306         IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI;
229700140306           leavesr;
229800140306         ENDIF;
229900131220
230000131220       //?Controllo ora pronta merce se non è presa da anagrafica clienti
230100131220         SELECT;
230200140402           WHEN  fiorb0OR.oraritiro > 0 and
230300140402                 fiora0O0.vaoorr = fiorb0OR.oraritiro;
230400131220           OTHER;
230500131220       //?Controllo ora pronta merce con ora fine standard
230600140331           IF  fiora0O0.vaoorr > trulor2ds.OOR2stfs;
230700140306             wIdMsg   = 'TIS0724';
230800131223             wIdCampo = 'VAOORR';
230900140331             wParm = %editw(trulor2ds.OOR2stis:'  :  ') +
231000140331                     %editw(trulor2ds.OOR2stfs:'  :  ');
231100131223             exsr Messaggi;
231200131223           ENDIF;
231300131220         ENDSL;
231400131220
231500131220       //?Controllo ora immissione ORM con ora massima ritiro
231600140306         IF  %dec(%subst(%editc(fiora0I0.vaooao:'X'):1:4):4:0)
231700140410             > trulor2ds.OOR2stfs and
231800140306             fiora0I0.vaodao = fiora0O0.vaodar;
231900140306           wIdMsg   = 'TIS0721';
232000140306           wIdCampo = 'VAOOAO';
232100140327           wParm = %editw(trulor2ds.OOR2stfs:'  :  ');
232200131223           exsr Messaggi;
232300131220         ENDIF;
232400131011
232500131011       ENDSR;
232600131028
232700131028       //--------------------------------------------------------------
232800131028       //?Controllo orari apertura.
232900131028       //--------------------------------------------------------------
233000140404       BEGSR  OrariApertura;
233100131028
233200140401         IF  fiora0I0.dalleam > 0 or fiora0I0.alleam > 0 or
233300140401             fiora0I0.dallepm > 0 or fiora0I0.allepm > 0;
233400131204           clear TRUL03ds;
233500140401           trul03ds.I03hm1 = fiora0I0.dalleam;
233600140401           trul03ds.I03hm2 = fiora0I0.alleam;
233700140401           trul03ds.I03hm3 = fiora0I0.dallepm;
233800140401           trul03ds.I03hm4 = fiora0I0.allepm;
233900131204           trul03r (TRUL03DS);
234000131204           IF  trul03ds.O03err <> *off;
234100131204             wIdMsg   = 'TIS0171';
234200131204             SELECT;
234300131204               WHEN  trul03ds.O03errpos = 1;
234400131204                 wIdCampo = 'DALLEAM';
234500131204                 clear wParm;
234600131204                 exsr Messaggi;
234700131204               WHEN  trul03ds.O03errpos = 2;
234800131204                 wIdCampo = 'ALLEAM';
234900131204                 clear wParm;
235000131204                 exsr Messaggi;
235100131204               WHEN  trul03ds.O03errpos = 3;
235200131204                 wIdCampo = 'DALLEPM';
235300131204                 clear wParm;
235400131204                 exsr Messaggi;
235500131204               WHEN  trul03ds.O03errpos = 4;
235600131204                 wIdCampo = 'ALLEPM';
235700131204                 clear wParm;
235800131204                 exsr Messaggi;
235900131204             ENDSL;
236000131204           ENDIF;
236100131204         ENDIF;
236200131028
236300131028       ENDSR;
236400131028
236500131028       //--------------------------------------------------------------
236600131028       //?Controllo quantità.
236700131028       //--------------------------------------------------------------
236800131028       BEGSR  Quantita;
236900140410
237000140410       //?Se Prepagato obbligo di colli e peso
237100140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
237200140410             fiora0I0.vaoncl = 0;
237300140410           wIdMsg   = 'TIS0726';
237400140410           wIdCampo = 'VAONCL';
237500140410           clear wParm;
237600140410           exsr Messaggi;
237700140410         ENDIF;
237800140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
237900140410             fiora0I0.vaopkg = 0;
238000140410           wIdMsg   = 'TIS0727';
238100140410           wIdCampo = 'VAOPKG';
238200140410           clear wParm;
238300140410           exsr Messaggi;
238400140410         ENDIF;
238500140410
238600140410       //?Se richiesto il ritiro tramite DPD
238700140410       //?il peso è obbligatorio e deve essere inferiore al limite da tab. 3I
238800151217         IF  fiora0I0.sceltantw = 'D';
238900140410           IF  fiora0I0.vaopkg = 0;
239000140410             wIdMsg   = 'TIS0727';
239100140410             wIdCampo = 'VAOPKG';
239200140410             clear wParm;
239300140410             exsr Messaggi;
239400140410           ENDIF;
239500140410           IF  fiora0I0.vaopkg > 0 and ds3idp.§3Ipkd > 0 and
239600140410               fiora0I0.vaopkg > ds3idp.§3Ipkd;
239700140410             wIdMsg   = 'TIS0694';
239800140410             wIdCampo = 'VAOPKG';
239900140410             wParm = %editc(ds3idp.§3Ipkd:'3');
240000140410             exsr Messaggi;
240100140410           ENDIF;
240200140410       //?i colli sono obbligatori e deve essere impostato a 1
240300140410           IF  fiora0I0.vaoncl <> 1;
240400140410             wIdMsg   = 'TIS0693';
240500140410             wIdCampo = 'VAONCL';
240600140410             clear wParm;
240700140410             exsr Messaggi;
240800140410           ENDIF;
240900140410         ENDIF;
241000140410
241100140410       //?Se Prepagato finisco qua i controlli
241200140410         IF  fiora0O0.prepagato = FIORA00_PREPAGATO;
241300140410           leavesr;
241400140410         ENDIF;
241500160530
241600160601       //?Faccio controlli diversificati tra Internet ed AS400
241700140331
241800160601       //?Controlli per richiamo da internet
241900160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
242000160601
242100140331       //?Almeno 1 tra colli - bancali deve essere presente
242200140401         IF  fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
242300140331           wIdMsg   = 'TIS0729';
242400140331           wIdCampo = 'VAONCL';
242500140331           clear wParm;
242600140331           exsr Messaggi;
242700140331         ENDIF;
242800140331
242900140331       //?Peso obbligatorio
243000140429         IF  fiora0I0.vaopkg = 0;
243100140429           wIdMsg   = 'TIS0727';
243200140429           wIdCampo = 'VAOPKG';
243300140429           clear wParm;
243400140429           exsr Messaggi;
243500140429         ENDIF;
243600131204
243700131204       //?Controllo con i flag su Anagrafica clienti ritiro
243800140331       //?Per internet facciamo solo i controlli di 'O' obbligatorio
243900160601         //IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
244000140402           IF  fiorb0OR.flagfncl = 'O' and fiora0I0.vaoncl = 0;
244100140331             wIdMsg   = 'TIS0726';
244200140331             wIdCampo = 'VAONCL';
244300140331             clear wParm;
244400140331             exsr Messaggi;
244500140331           ENDIF;
244600140331
244700140402           IF  fiorb0OR.flagfpkg = 'O' and fiora0I0.vaopkg = 0;
244800140331             wIdMsg   = 'TIS0727';
244900140331             wIdCampo = 'VAOPKG';
245000140331             clear wParm;
245100140331             exsr Messaggi;
245200140331           ENDIF;
245300140331
245400140402           IF  fiorb0OR.flagfvlm = 'O' and fiora0I0.vaovlm = 0;
245500140331             wIdMsg   = 'TIS0728';
245600140331             wIdCampo = 'VAOVLM';
245700140331             clear wParm;
245800140331             exsr Messaggi;
245900140331           ENDIF;
246000140331
246100140402           IF  fiorb0OR.flagfbnc = 'O' and fiora0I0.vaobnc = 0;
246200140331             wIdMsg   = 'TIS0722';
246300140331             wIdCampo = 'VAOBNC';
246400140331             clear wParm;
246500140331             exsr Messaggi;
246600140331           ENDIF;
246700160601
246800140331         ENDIF;
246900160601
247000160601       //?Controlli Se richiamato da AS400
247100160601       //?faccio prima i controlli con i flag dell'anagrafica mittente
247200160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
247300160601         //?Flag F vuol dire quantità forzabile quindi posso anche non immetterla
247400160601         //?Flag O vuol dire quantità obbligatoria quindi non accetto se a 0
247500160601           IF  fiorb0OR.flagfncl <> *blanks and fiora0I0.vaoncl = 0;
247600160601             wIdMsg   = 'TIS0726';
247700160601             wIdCampo = 'VAONCL';
247800160601             clear wParm;
247900160601             IF  fiorb0OR.flagfncl = 'F';
248000160601               exsr Forzatura;
248100160601               IF  wForza;
248200160601                 exsr RoutEnd;
248300160601               ENDIF;
248400160601             ENDIF;
248500160601             IF  fiorb0OR.flagfncl = 'O';
248600160601               exsr Messaggi;
248700160601             ENDIF;
248800160601           ENDIF;
248900160601           IF  fiorb0OR.flagfpkg <> *blanks and fiora0I0.vaopkg = 0;
249000160601             wIdMsg   = 'TIS0727';
249100160601             wIdCampo = 'VAOPKG';
249200160601             clear wParm;
249300160601             IF  fiorb0OR.flagfpkg = 'F';
249400160601               exsr Forzatura;
249500160601               IF  wForza;
249600160601                 exsr RoutEnd;
249700160601               ENDIF;
249800160601             ENDIF;
249900160601             IF  fiorb0OR.flagfpkg = 'O';
250000160601               exsr Messaggi;
250100160601             ENDIF;
250200160601           ENDIF;
250300160601           IF  fiorb0OR.flagfvlm <> *blanks and fiora0I0.vaovlm = 0;
250400160601             wIdMsg   = 'TIS0728';
250500160601             wIdCampo = 'VAOVLM';
250600160601             clear wParm;
250700160601             IF  fiorb0OR.flagfvlm = 'F';
250800160601               exsr Forzatura;
250900160601               IF  wForza;
251000160601                 exsr RoutEnd;
251100160601               ENDIF;
251200160601             ENDIF;
251300160601             IF  fiorb0OR.flagfvlm = 'O';
251400160601               exsr Messaggi;
251500160601             ENDIF;
251600160601           ENDIF;
251700160601           IF  fiorb0OR.flagfbnc <> *blanks and fiora0I0.vaobnc = 0;
251800160601             wIdMsg   = 'TIS0722';
251900160601             wIdCampo = 'VAOBNC';
252000160601             clear wParm;
252100160601             IF  fiorb0OR.flagfbnc = 'F';
252200160601               exsr Forzatura;
252300160601               IF  wForza;
252400160601                 exsr RoutEnd;
252500160601               ENDIF;
252600160601             ENDIF;
252700160601             IF  fiorb0OR.flagfbnc = 'O';
252800160601               exsr Messaggi;
252900160601             ENDIF;
253000160601           ENDIF;
253100160601         //?In caso di Immissione/Copia ORM
253200160601         //?immissione Manuale
253300160601           IF  prmRqsOpCode = FIORA00_RQSOPCODE_WRITE and
253400160601              (fiora0I0.tco = 'E' or fiora0I0.tco = 'M') and
253500160601           //?Almeno 1 tra colli - bancali deve essere presente
253600160601               fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0;
253700160601             wIdMsg   = 'TIS0729';
253800160601             wIdCampo = 'VAONCL';
253900160601             clear wParm;
254000160601             exsr Messaggi;
254100160601           ENDIF;
254200160601         //?Almeno 1 tra peso volume o bancali deve essere presente
254300160601           IF  fiora0I0.vaoncl = 0 and fiora0I0.vaobnc = 0 and
254400160601               fiora0I0.vaopkg = 0;
254500160601             wIdMsg   = 'TIS0235';
254600160601             wIdCampo = 'VAOPKG';
254700160601             clear wParm;
254800160601             exsr Messaggi;
254900160601           ENDIF;
255000160601         ENDIF;
255100131028
255200131028       ENDSR;
255300151008
255400151008       //--------------------------------------------------------------
255500151008       //?Calcolo peso/volume più alto per instradamento.
255600151008       //--------------------------------------------------------------
255700151008       BEGSR  PesoVolume;
255800151008
255900151008         wPeso = fiora0I0.vaopkg;
256000151008         wVolume = fiora0I0.vaovlm;
256100151008
256200151008       //?il peso c'è sempre dato che è un campo obbligatorio
256300151008       //?quindi sviluppo il volume se non immesso
256400151008         IF  wVolume = 0;
256500151008           SELECT;
256600151008           WHEN  fiora0I0.vaobnc > 0;
256700151008             eval(h) wVolumemax = fiora0I0.vaobnc / dDFT.d§DFTbnc;
256800151008           WHEN  fiora0I0.vaopkg > 0;
256900151008             eval(h) wVolumemax = fiora0I0.vaopkg / dDFT.d§DFTpkg;
257000151008           ENDSL;
257100151008       //?se volume troppo alto imposto il massimo consentito
257200151008           IF  wVolumemax > 99,999;
257300151008             wVolume = 99,990;
257400151008           ELSE;
257500151008             wVolume = wVolumemax;
257600151008           ENDIF;
257700151008
257800151008         ENDIF;
257900151008
258000151008       ENDSR;
258100131204
258200131204       //--------------------------------------------------------------
258300131204       //?Controllo la natura della merce.
258400131204       //--------------------------------------------------------------
258500131204       BEGSR  NaturaMerce;
258600131204
258700140404       //?Se richiamato da internet
258800140411         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET and
258900140411       //?Obbligo della natura merce se mittente non codificato
259000140416       //?                         e se destinatario non codificato
259100140416             fiora0I0.vaonam = *blanks and fiora0I0.vaocra = *zeros and
259200140416             fiora0I0.vaocrc = *zeros;
259300140411           wIdMsg   = 'TIS0143';
259400140411           wIdCampo = 'VAONAM';
259500140411           clear wParm;
259600140411           exsr Messaggi;
259700140411           leavesr;
259800140407         ENDIF;
259900140211
260000140211         IF  fiora0I0.vaonam <> *blanks;
260100140211           exec sql
260200140211           set :fiora0O0.vaonam = ucase (:fiora0I0.vaonam);
260300131223         ENDIF;
260400131204
260500131204       ENDSR;
260600131204
260700131204       //--------------------------------------------------------------
260800131204       //?Controllo uno o più destinatari.
260900131204       //--------------------------------------------------------------
261000131204       BEGSR  UnoPiuDest;
261100131220
261200131220         fiora0O0.unopiudest = fiora0I0.unopiudest;
261300131204
261400131204       //?Accetto 1 o 2
261500131223         IF  fiora0O0.unopiudest <> '1' and fiora0O0.unopiudest <> '2';
261600131204           wIdMsg   = 'TIS0295';
261700131204           wIdCampo = 'UNOPIUDEST';
261800131204           clear wParm;
261900131204           exsr Messaggi;
262000131219           leavesr;
262100131204         ENDIF;
262200131204
262300131204       //?Se ORM prepagato SOLO 1 destinatario
262400131204         IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
262500131223             fiora0O0.unopiudest <> '1';
262600131204           wIdMsg   = 'TIS0307';
262700131204           wIdCampo = 'UNOPIUDEST';
262800131204           clear wParm;
262900131204           exsr Messaggi;
263000131204         ENDIF;
263100131204
263200131204       //?Se richiesto il ritiro tramite DPD SOLO 1 destinatario
263300160527       //?Il controllo qua lo faccio solo se richiamato da Internet
263400160527         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
263500131204         IF  fiora0O0.forzantw = FIORA00_SERVIZIO_DPD and
263600131223             fiora0O0.unopiudest <> '1';
263700131204           wIdMsg   = 'TIS0307';
263800131204           wIdCampo = 'UNOPIUDEST';
263900131204           clear wParm;
264000131204           exsr Messaggi;
264100131204         ENDIF;
264200160527         ENDIF;
264300131204
264400131204       ENDSR;
264500131205
264600131205       //--------------------------------------------------------------
264700131205       //?Controllo Referente.
264800131205       //--------------------------------------------------------------
264900131205       BEGSR  Referente;
265000131205
265100160601       //?Se Richiamato da INTERNET
265200160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
265300131205       //?Se mittente non codificato oppure ORM commissionato
265400131205       //?il referente è obbligatorio
265500140401         IF  (fiora0I0.vaocra = 0 or wCom) and
265600131223              fiora0I0.vaorer = *blanks;
265700131205           wIdMsg   = 'TIS0290';
265800131205           wIdCampo = 'VAORER';
265900131205           clear wParm;
266000131205           exsr Messaggi;
266100131205         ENDIF;
266200160601         ENDIF;
266300160601
266400160601       //?Referente obbligatorio
266500160601       //?se Richiamato da AS400
266600160601       //?e Prepagato o ORM commissionato
266700160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
266800160601            (fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO or wCom) and
266900160601             fiora0I0.vaorer = *blanks;
267000160601           wIdMsg   = 'TIS0290';
267100160601           wIdCampo = 'VAORER';
267200160601           clear wParm;
267300160601           exsr Messaggi;
267400160601         ENDIF;
267500131205
267600131223         IF  fiora0I0.vaorer <> *blanks;
267700140211           exec sql
267800140211           set :fiora0O0.vaorer = ucase (:fiora0I0.vaorer);
267900131223         ENDIF;
268000131205
268100131205       ENDSR;
268200131223
268300131223       //--------------------------------------------------------------
268400131223       //?Controllo Telefono.
268500131223       //--------------------------------------------------------------
268600131223       BEGSR  Telefono;
268700131223
268800160601       //?Se Richiamato da INTERNET
268900160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
269000131223       //?Se mittente non codificato oppure ORM commissionato
269100131223       //?il telefono è obbligatorio
269200140401         IF  (fiora0I0.vaocra = 0 or wCom) and
269300131223              fiora0I0.vaoter = *blanks;
269400131223           wIdMsg   = 'TIS0246';
269500131223           wIdCampo = 'VAOTER';
269600131223           clear wParm;
269700131223           exsr Messaggi;
269800131223         ENDIF;
269900160601         ENDIF;
270000160601
270100160601       //?Telefono obbligatorio
270200160601       //?se Richiamato da AS400
270300160601       //?e Prepagato o ORM commissionato
270400160601         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400 and
270500160601            (fiora0I0.tor = FIORA00_TIPOORM_PREPAGATO or wCom) and
270600160601             fiora0I0.vaoter = *blanks;
270700160601           wIdMsg   = 'TIS0246';
270800160601           wIdCampo = 'VAOTER';
270900160601           clear wParm;
271000160601           exsr Messaggi;
271100160601         ENDIF;
271200131223
271300131223         IF  fiora0I0.vaoter <> *blanks;
271400140211           exec sql
271500140211           set :fiora0O0.vaoter = ucase (:fiora0I0.vaoter);
271600131223         ENDIF;
271700131223
271800131223       ENDSR;
271900140311
272000140311       //--------------------------------------------------------------
272100140311       //?Controllo indirizzo mail.
272200140311       //--------------------------------------------------------------
272300140311       BEGSR  Mail;
272400140311
272500140311         IF  fiora0I0.mail <> *blanks;
272600140311           clear dsemail;
272700140311           dsemail.§emlindi = fiora0I0.mail;
272800140311           xemail (dsemail);
272900140311           IF  dsemail.§emlerro <> '0';
273000140424             wIdMsg   = 'TIS0732';
273100140311             wIdCampo = 'MAIL';
273200140424             clear wParm;
273300140311             exsr Messaggi;
273400140311             leavesr;
273500140311           ENDIF;
273600140311
273700140424           fiora0O0.mail = fiora0I0.mail;
273800140722           IF  dsemail.§emlindo <> *blanks;
273900140722             fiora0O0.mail = dsemail.§emlindo;
274000140722           ENDIF;
274100140311         ENDIF;
274200140311
274300140311       ENDSR;
274400140311
274500140311       //--------------------------------------------------------------
274600140311       //?Controllo n.cellulare.
274700140311       //--------------------------------------------------------------
274800140311       BEGSR  Sms;
274900140311
275000140311         IF  fiora0I0.sms <> *blanks;
275100140311           pInCell = %trim(fiora0I0.sms);
275200140311           clear pOutErr;
275300140722           clear pOutCell;
275400140311           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
275500140311             wIdMsg   = 'TIS0725';
275600140311             wIdCampo = 'SMS';
275700140311             clear wParm;
275800140311             exsr Messaggi;
275900160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
276000160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
276100160601             fiora0I0.sms;
276200140311             leavesr;
276300140311           ENDIF;
276400140311
276500140416           fiora0O0.sms = fiora0I0.sms;
276600140722           IF  pOutCell <> *blanks;
276700140722             fiora0O0.sms = pOutCell;
276800140722           ENDIF;
276900140311         ENDIF;
277000140311
277100140311       ENDSR;
277200140313
277300140313       //--------------------------------------------------------------
277400140313       //?Controllo Codice Destinatario.
277500140313       //--------------------------------------------------------------
277600140313       BEGSR  CodDestinatario;
277700140401
277800140401         wCodice = fiora0I0.vaocrc;
277900140402         reset FIORB0OC;
278000140313
278100140313       //?Se presente codice destinatario verifico se valido
278200140313       //?e se legato al codice di LOG
278300140402         IF  not IsCodiceValido(FIORB0OC);
278400140313           wIdMsg   = 'TIS0066';
278500140313           wIdCampo = 'VAOCRC';
278600140313           clear wParm;
278700140313           exsr Messaggi;
278800140313           leavesr;
278900140313         ENDIF;
279000140313
279100140313       //?controllo se fa parte della stessa famiglia del codice di LOG
279200140313         IF not IsCodiceLegato();
279300140313           wIdMsg   = 'TIS0066';
279400140313           wIdCampo = 'VAOCRC';
279500140313           clear wParm;
279600140313           exsr Messaggi;
279700140313           leavesr;
279800140313         ENDIF;
279900140313
280000140313       ENDSR;
280100131205
280200131205       //--------------------------------------------------------------
280300131205       //?Controllo Destinatario.
280400131205       //--------------------------------------------------------------
280500131205       BEGSR  Destinatario;
280600131220
280700140211         exec sql
280800140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
280900131205
281000131205       //?Se richiesto UN solo destinatario il dato è obbligatorio
281100131223         IF  fiora0O0.unopiudest = '1' and
281200131205            (fiora0I0.vaorsc = *blanks or fiora0I0.vaoinc = *blanks or
281300131205             fiora0i0.vaoloc = *blanks or fiora0i0.vaocac = *blanks);
281400131205           wIdMsg   = 'TIS0288';
281500131205           wIdCampo = 'VAORSC';
281600131205           clear wParm;
281700131205           exsr Messaggi;
281800131220           leavesr;
281900131205         ENDIF;
282000131205
282100131205       //?Se richiesto più destinatari il dato NON è da inserire
282200131205         IF  fiora0I0.unopiudest = '2' and
282300131205            (fiora0I0.vaorsc <> *blanks or fiora0I0.vaoinc <> *blanks or
282400131205             fiora0i0.vaoloc <> *blanks or fiora0i0.vaocac <> *blanks);
282500131205           wIdMsg   = 'TIS0380';
282600131205           wIdCampo = 'VAORSC';
282700131205           clear wParm;
282800131205           exsr Messaggi;
282900131220           leavesr;
283000131205         ENDIF;
283100131220
283200131220       //?Controllo nominativo destinatario
283300131220         exsr NomeDestinatario;
283400131220
283500131220       //?Controllo indirizzo destinatario
283600131220         exsr ViaDestinatario;
283700131220
283800131220       //?Controllo località destinatario
283900131220         exsr LocDestinatario;
284000131220
284100131220       //?Controllo cap destinatario
284200131220         exsr CapDestinatario;
284300131220
284400131220       //?Controllo provincia destinatario
284500131220         exsr ProvDestinatario;
284600131220
284700131220       //?Controllo nazione destinatario
284800131220         exsr NazDestinatario;
284900131220
285000131220       //?Controllo indirizzo completo del destinatario
285100131220         exsr IndTotDestinatario;
285200131205
285300131205       ENDSR;
285400131205
285500131205       //--------------------------------------------------------------
285600131205       //?Controllo nominativo destinatario.
285700131205       //--------------------------------------------------------------
285800131205       BEGSR  NomeDestinatario;
285900131205
286000131205       //?Se richiesto UN solo destinatario deve essere indicato
286100131223         IF  fiora0O0.unopiudest = '1' and
286200131205             fiora0I0.vaorsc = *blanks;
286300131205           wIdMsg   = 'TIS0480';
286400131205           wIdCampo = 'VAORSC';
286500131205           clear wParm;
286600131205           exsr Messaggi;
286700131205         ENDIF;
286800131205
286900140211         exec sql
287000140211         set :fiora0O0.vaorsc = ucase (:fiora0I0.vaorsc);
287100131205
287200131205       ENDSR;
287300131205
287400131205       //--------------------------------------------------------------
287500131205       //?Controllo indirizzo destinatario.
287600131205       //--------------------------------------------------------------
287700131205       BEGSR  ViaDestinatario;
287800131205
287900131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
288000131220       //?deve essere indicata anche la via
288100131223         IF  (fiora0O0.unopiudest = '1' or fiora0I0.vaorsc <> *blanks) and
288200131220              fiora0I0.vaoinc = *blanks;
288300131205           wIdMsg   = 'TIS0281';
288400131205           wIdCampo = 'VAOINC';
288500131205           clear wParm;
288600131205           exsr Messaggi;
288700131205         ENDIF;
288800131205
288900140211         exec sql
289000140211         set :fiora0O0.vaoinc = ucase (:fiora0I0.vaoinc);
289100131205
289200131205       ENDSR;
289300131205
289400131205       //--------------------------------------------------------------
289500131205       //?Controllo cap destinatario.
289600131205       //--------------------------------------------------------------
289700131205       BEGSR  CapDestinatario;
289800131205
289900131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
290000131220       //?deve essere indicato
290100131223         IF  (fiora0O0.unopiudest = '1' or fiora0I0.vaorsc <> *blanks) and
290200131220              fiora0I0.vaocac = *blanks;
290300131205           wIdMsg   = 'TIS0051';
290400131205           wIdCampo = 'VAOCAC';
290500131205           clear wParm;
290600131205           exsr Messaggi;
290700131205         ENDIF;
290800131205
290900140211         exec sql
291000140211         set :fiora0O0.vaocac = ucase (:fiora0I0.vaocac);
291100131205
291200131205       ENDSR;
291300131205
291400131205       //--------------------------------------------------------------
291500131205       //?Controllo località destinatario.
291600131205       //--------------------------------------------------------------
291700131205       BEGSR  LocDestinatario;
291800131205
291900131220       //?Se richiesto UN solo destinatario o immessa la ragione sociale
292000131220       //?deve essere indicato
292100131223         IF  (fiora0O0.unopiudest = '1' and fiora0I0.vaorsc <> *blanks) and
292200131220              fiora0I0.vaoloc = *blanks;
292300131205           wIdMsg   = 'TIS0345';
292400131205           wIdCampo = 'VAOLOC';
292500131205           clear wParm;
292600131205           exsr Messaggi;
292700131205         ENDIF;
292800131205
292900140211         exec sql
293000140211         set :fiora0O0.vaoloc = ucase (:fiora0I0.vaoloc);
293100131205
293200131205       ENDSR;
293300131009
293400131205       //--------------------------------------------------------------
293500131205       //?Controllo provincia destinatario.
293600131205       //--------------------------------------------------------------
293700131205       BEGSR  ProvDestinatario;
293800131205
293900131220       //?Controllo se richiesto UN solo destinatario
294000131220       //?o se immessa ragione sociale
294100131223         IF  fiora0O0.unopiudest = '1' or fiora0I0.vaorsc <> *blanks;
294200131205
294300140211           exec sql
294400140211           set :fiora0O0.vaoprc = ucase (:fiora0I0.vaoprc);
294500131205
294600131205         //?Controllo con la sk delle provincie
294700131205           IF  fiora0O0.vaoprc <> *blanks and
294800131205              %lookup(fiora0O0.vaoprc:skprv) = 0;
294900131205             wIdMsg   = 'TIS0461';
295000131205             wIdCampo = 'VAOPRC';
295100131205             clear wParm;
295200131205             exsr Messaggi;
295300131205           ENDIF;
295400131205
295500131205         ENDIF;
295600131205
295700131205       ENDSR;
295800131205
295900131205       //--------------------------------------------------------------
296000131205       //?Controllo nazione destinatario.
296100131205       //--------------------------------------------------------------
296200131205       BEGSR  NazDestinatario;
296300131205
296400131205       //?Controllo se richiesto UN solo destinatario
296500131220       //?o immessa ragione sociale
296600131223         IF  fiora0O0.unopiudest = '1' or fiora0I0.vaorsc <> *blanks;
296700131205
296800140211           exec sql
296900140211           set :fiora0O0.vaonac = ucase (:fiora0I0.vaonac);
297000131205
297100131205         //?Controllo con la sk delle provincie
297200131205           IF  fiora0O0.vaonac <> *blanks and
297300131205               %lookup(fiora0O0.vaonac:sknaz) = 0;
297400140326             wIdMsg   = 'TIS0384';
297500131205             wIdCampo = 'VAONAC';
297600131205             clear wParm;
297700131205             exsr Messaggi;
297800131205           ENDIF;
297900131205
298000131205         ENDIF;
298100131205
298200131205       ENDSR;
298300131205
298400131205       //--------------------------------------------------------------
298500131205       //?Controllo indirizzo completo del destinatario.
298600131205       //--------------------------------------------------------------
298700131205       BEGSR  IndTotDestinatario;
298800131205
298900131205       //?Controllo se richiesto UN solo destinatario
299000131220       //?o immessa ragione sociale
299100131223         IF  fiora0O0.unopiudest = '1' or fiora0I0.vaorsc <> *blanks;
299200131205
299300131205           clear TISI95DS;
299400151009           clear FNLV13DS;
299500131205           tisi95ds.I95tcn = '7';
299600131205           tisi95ds.I95nar = fiora0O0.vaonac;
299700131205           tisi95ds.I95cap = fiora0O0.vaocac;
299800131205           tisi95ds.I95loc = fiora0O0.vaoloc;
299900131205           tisi95ds.I95prv = fiora0O0.vaoprc;
300000131205           tisi95ds.I95ind = fiora0O0.vaoinc;
300100131205           tisi95ds.I95dat = %dec(%date());
300200151009           fnlv13ds.I13af0 = 'S';
300300151009           fnlv13ds.I13cnv = 'S';
300400151009           fnlv13ds.I13la3 = 'S';
300500131205
300600131205         //?Solo per richiamo da Internet
300700131205           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
300800131205           //?se richiesta nazione irlanda forzo cap eire
300900131205             IF  tisi95ds.I95nar = FIORA00_IRLANDA;
301000131205               tisi95ds.I95cap =  FIORA00_EIRE;
301100131205             ENDIF;
301200131205           ENDIF;
301300131205
301400151009         //?controllo cap/località/provincia
301500151009           fnlv13r (kpjba:fnlv13ds:tisi95ds);
301600151016         //?se torna errore per cap generico diversifico il messaggio
301700151016           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks and
301800151016               %scan('generico':fnlv13ds.O13msg) > 0;
301900151016             wIdMsg   = 'TIS0826';
302000151016             wIdCampo = 'VAOCAC';
302100151016             clear wParm;
302200151016             exsr Messaggi;
302300151016             wErrMitt = *on;
302400151016             leavesr;
302500151016           ENDIF;
302600151009         //?se torna errore imposto messaggio generico
302700151009           IF  fnlv13ds.O13msg <> *blanks and fnlv13ds.O13err <> *blanks;
302800131205             wIdMsg   = 'TIS0054';
302900131227             wIdCampo = 'VAOLOC';
303000131205             clear wParm;
303100131205             exsr Messaggi;
303200131205             leavesr;
303300131205           ENDIF;
303400131227
303500131227         //?Se ORM export EEX non accetto destinatario esteri
303600131227           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
303700131227               fiora0O0.forzantw = FIORA00_SERVIZIO_EEX and
303800131227               fiora0O0.vaonac <> *blanks;
303900140305             wIdMsg   = 'TIS0723';
304000131227             wIdCampo = 'VAONAC';
304100140305             clear wParm;
304200131227             exsr Messaggi;
304300131227           ENDIF;
304400131205
304500131205         //?Se ORM export DPD controllo la fattibilità della consegna
304600131205           IF  fiora0O0.ritiroest = FIORA00_RITIROESTERO_SI and
304700131227               fiora0O0.forzantw = FIORA00_SERVIZIO_DPD;
304800131205             clear fior95ds;
304900131205             fior95ds.IOR95cap = fiora0O0.vaocac;
305000131205             fior95ds.IOR95loc = fiora0O0.vaoloc;
305100131205             fior95ds.IOR95prv = fiora0O0.vaoprc;
305200131205             fior95ds.IOR95naz = fiora0O0.vaonac;
305300131230             fior95ds.IOR95lin = cvtLinguaISO2ToTntbe(fiora0I0.idlingua);
305400131205             fior95r (kpjba : fior95ds);
305500131205             IF  fior95ds.OOR95err <> *blanks or fior95ds.OOR95ok = 'N';
305600140304               wIdMsg   = 'TIS0718';
305700131205               wIdCampo = 'VAONAC';
305800131205               wParm = fior95ds.OOR95msg;
305900131205               exsr Messaggi;
306000131205             ENDIF;
306100131205           ENDIF;
306200160208
306300160208         //?Se ORM prepagato e consegna con LNA 340-341-345 blocco
306400160208         //?come da DV 1540
306500160208           IF  fiora0O0.prepagato = FIORA00_PREPAGATO and
306600160209               fiora0I0.vaonac <> *blanks and fnlv13ds.O13err = *blanks and
306700160208               (tisi95ds.O95lna = 340 or tisi95ds.O95lna = 341 or
306800160208                tisi95ds.O95lna = 345);
306900160208             clear xx;
307000160208             xx = %lookup(fiora0I0.vaonac:sknaz);
307100160208             wIdMsg   = 'TIS0828';
307200160208             wIdCampo = 'VAOPAG';
307300160208             IF  xx > 0;
307400160208               wParm = sknazd(xx);
307500160208             ELSE;
307600160208               wParm = fiora0I0.vaoloc;
307700160208             ENDIF;
307800160208             exsr Messaggi;
307900160208           ENDIF;
308000160208
308100160208         //?Solo per richiamo da Internet
308200160208           IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
308300160208           //?Errore
308400160208           //?Se ordinante non codificato (è utente senza PSW)
308500160208           //?Se consegna all'estero
308600160208           //?Se paga destinatario
308700160208             IF  fiora0I0.vaocor = 0 and
308800160208                 fiora0I0.vaonac <> *blanks and
308900160208                 fiora0I0.vaopag = 'D';
309000160208               wIdMsg   = 'TIS0448';
309100160208               wIdCampo = 'VAOPAG';
309200160208               clear wParm;
309300160208               exsr Messaggi;
309400160208             ENDIF;
309500160208           ENDIF;
309600131205
309700131205         ENDIF;
309800131205
309900131205       ENDSR;
310000131205
310100131205       //--------------------------------------------------------------
310200131205       //?Controllo fermo deposito.
310300131205       //--------------------------------------------------------------
310400131205       BEGSR  FermoDeposito;
310500131205
310600131205         IF  fiora0I0.vaoffd <> 'S' and fiora0I0.vaoffd <> 'N';
310700131205           wIdMsg   = 'TIS0537';
310800131205           wIdCampo = 'VAOFFD';
310900131205           clear wParm;
311000131205           exsr Messaggi;
311100131205         ENDIF;
311200131205
311300131205       ENDSR;
311400160315
311500160315       //--------------------------------------------------------------
311600160315       //?Controllo Filiale Consegna.
311700160315       //--------------------------------------------------------------
311800160315       BEGSR  FilConsegna;
311900160315
312000160315         wOKffc = *off;
312100160315         fiora0O0.vaopoc = fiora0I0.vaopoc;
312200160315
312300160315       //?Se il destinatario è estero
312400160315       //?forzo la filiale di consegna in base alla tabella FFC
312500160315         IF  fiora0O0.vaonac <> *blanks;
312600160315           clear dFFC;
312700160315       //?prima provo con la filiale dell'ordinante
312800160315           IF  fiora0O0.vaocor > 0;
312900160315             k_TBEcod = 'FFC';
313000160315             k_TBEke1 = %subst(%editc(fiora0O0.vaocor:'X'):1:3);
313100160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
313200160315             IF  %found(TNTBE01L);
313300160315               dFFC = TBEuni;
313400160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
313500160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
313600160315               ENDIF;
313700160315               wOKffc = *on;
313800160315             ENDIF;
313900160315           ENDIF;
314000160315       //?se non trovo con filiale ordinante proco con filiale emissione se diversa
314100160315           IF  not wOKffc and
314200160315               %subst(%editc(fiora0O0.vaocor:'X'):1:3) <>
314300160315               %editc(fiora0O0.vaopoe:'X');
314400160315             clear dFFC;
314500160315             k_TBEcod = 'FFC';
314600160315             k_TBEke1 = %editc(fiora0O0.vaopoe:'X');
314700160315             chain (k_TBEcod:k_TBEke1) TNTBE01L;
314800160315             IF  %found(TNTBE01L);
314900160315               dFFC = TBEuni;
315000160315               IF  dataoggi >= dffc.§FFCdti and dataoggi <= dffc.§FFCdtf;
315100160315                 fiora0O0.vaopoc = dffc.§FFCpoc;
315200160315               ENDIF;
315300160315             ENDIF;
315400160315           ENDIF;
315500160315         ENDIF;
315600160315
315700160315       //?Se non impostata non controllo
315800160315         IF  fiora0O0.vaopoc = *zeros;
315900160315           leavesr;
316000160315         ENDIF;
316100160315
316200160315       //?Controllo la filiale consegna
316300160315         IF  not IsFilValida(fiora0I0.vaopoc);
316400160315           wIdMsg   = 'TIS0803';
316500160315           wIdCampo = 'VAOPOC';
316600160315           clear wParm;
316700160315           exsr Messaggi;
316800160315           leavesr;
316900160315         ENDIF;
317000160315
317100160315       //?Network e Decodifico la filiale consegna
317200160315         wNetworkCon = GetNtwFiliale(fiora0O0.vaopoc);
317300160315
317400160315       ENDSR;
317500160315
317600160315       //--------------------------------------------------------------
317700160315       //?Controllo Cliente tassazione.
317800160315       //--------------------------------------------------------------
317900160315       BEGSR  ClienteKsc;
318000160315
318100160315         wOKtariffa = *off;
318200160315
318300160315       //?Codice tassazione valido
318400160315         IF  fiora0I0.vaoksc > *zeros;
318500160315           IF  not IsKscValido(fiora0I0.vaoksc);
318600160315             wIdMsg   = 'TIS0810';
318700160315             wIdCampo = 'VAOKSC';
318800160315             clear wParm;
318900160315             exsr Messaggi;
319000160315             leavesr;
319100160315           ENDIF;
319200160315         ENDIF;
319300160315
319400160315       //?Codice tariffa valido
319500160315         IF  fiora0I0.vaoctr <> *blanks;
319600160315           IF  %check(FIORA00_DIGITS:fiora0I0.vaoctr) > *zeros;
319700160315             wIdMsg   = 'TIS0810';
319800160315             wIdCampo = 'VAOCTR';
319900160315             clear wParm;
320000160315             exsr Messaggi;
320100160315             leavesr;
320200160315           ENDIF;
320300160315         ENDIF;
320400160315
320500160315       //?Se codice tariffa deve esserci anche il codice cliente
320600160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc = *zeros;
320700160315           wIdMsg   = 'TIS0718';
320800160315           wIdCampo = 'VAOCTR';
320900160315           wParm = 'Non ammesso codice tariffa se non presente anche +
321000160315                    il codice cliente';
321100160315           exsr Messaggi;
321200160315           leavesr;
321300160315         ENDIF;
321400160315
321500160315       //?La tariffa deve esistere
321600160315         IF  fiora0I0.vaoctr <> *blanks and fiora0I0.vaoksc > *zeros;
321700160315
321800160315           k_TAMksc = fiora0I0.vaoksc;
321900160315           k_TAMctr = %int(fiora0I0.vaoctr);
322000160315           setll (k_TAMksc:k_TAMctr) TNTAM01L;
322100160315           IF  not %equal(TNTAM01L);
322200160315             wIdMsg   = 'TIS0810';
322300160315             wIdCampo = 'VAOKSC';
322400160315             clear wParm;
322500160315             exsr Messaggi;
322600160315             leavesr;
322700160315           ENDIF;
322800160315
322900160315           DOW  not wOKtariffa;
323000160315             reade (k_TAMksc:k_TAMctr) TNTAM01L;
323100160315             IF  %eof(TNTAM01L);
323200160315               leave;
323300160315             ENDIF;
323400160315             IF  TAMatb = *blanks;
323500160315               wOKtariffa = *on;
323600160315             ENDIF;
323700160315           ENDDO;
323800160315           IF  not wOKtariffa;
323900160315             wIdMsg   = 'TIS0810';
324000160315             wIdCampo = 'VAOKSC';
324100160315             clear wParm;
324200160315             exsr Messaggi;
324300160315             leavesr;
324400160315           ENDIF;
324500160315
324600160315         ENDIF;
324700160315
324800160315         fiora0O0.vaoksc = fiora0I0.vaoksc;
324900160315         fiora0O0.vaoctr = fiora0O0.vaoctr;
325000160315
325100160315       ENDSR;
325200140506
325300140506       //--------------------------------------------------------------
325400140506       //?Controllo Alert.
325500140506       //--------------------------------------------------------------
325600140506       BEGSR  Alert;
325700140506
325800140506         IF  fiora0I0.alertmail <> *blanks and
325900140506             fiora0I0.alertmail <> 'N'     and
326000140506             fiora0I0.alertmail <> 'X';
326100140506           clear fiora0O0.alertmail;
326200140506         ENDIF;
326300140506
326400140506         IF  fiora0I0.alertsms <> *blanks and
326500140506             fiora0I0.alertsms <> 'N'     and
326600140506             fiora0I0.alertsms <> 'X';
326700140506           clear fiora0O0.alertsms;
326800140506         ENDIF;
326900140506
327000140506       ENDSR;
327100151008
327200151008       //--------------------------------------------------------------
327300151008       //?Controllo indirizzo mail x accettazione ORM.
327400151008       //--------------------------------------------------------------
327500151008       BEGSR  MailConferma;
327600160503
327700160503         fiora0O0.alertconf = 'N';
327800151008
327900160318       //?Versione inferiore alla 'C'
328000160318         IF  fiora0I0.mailconf = *blanks and fiora0I0.alertconf = 'S' and
328100160318             fiora0O0.versione < 'C';
328200151008           wIdMsg   = 'TIS0732';
328300151008           wIdCampo = 'MAILCONF';
328400151008           clear wParm;
328500151008           exsr Messaggi;
328600151008           leavesr;
328700151008         ENDIF;
328800160318       //?dalla Versione 'C'
328900160318         IF  fiora0I0.mailconf = *blanks and fiora0I0.alertconf = 'S' and
329000160318             fiora0O0.versione > 'B' and fiora0I0.smsconf = *blanks;
329100160318           wIdMsg   = 'TIS0831';
329200160318           wIdCampo = 'MAILCONF';
329300160318           clear wParm;
329400160318           exsr Messaggi;
329500160318           leavesr;
329600160318         ENDIF;
329700151008
329800151008         IF  fiora0I0.mailconf <> *blanks;
329900151008           clear dsemail;
330000151008           dsemail.§emlindi = fiora0I0.mailconf;
330100151008           xemail (dsemail);
330200151008           IF  dsemail.§emlerro <> '0';
330300151008             wIdMsg   = 'TIS0732';
330400151008             wIdCampo = 'MAILCONF';
330500151008             clear wParm;
330600151008             exsr Messaggi;
330700151008             leavesr;
330800151008           ENDIF;
330900151008
331000151008           fiora0O0.mailconf = fiora0I0.mailconf;
331100151008           IF  dsemail.§emlindo <> *blanks;
331200151008             fiora0O0.mailconf = dsemail.§emlindo;
331300151008           ENDIF;
331400151008         ENDIF;
331500151008
331600151008       //?Passo il dato solo se impostato a 'SI'
331700151008         IF  fiora0I0.alertconf = 'S';
331800151008           fiora0O0.alertconf = fiora0I0.alertconf;
331900160322         //?Dalla versione 'C' se non c'è la mail non lo imposto
332000160322           IF  fiora0O0.versione > 'B' and fiora0O0.mailconf = *blanks;
332100160503             fiora0O0.alertconf = 'N';
332200160322           ENDIF;
332300151008         ENDIF;
332400151008
332500151008       ENDSR;
332600151008
332700151008       //--------------------------------------------------------------
332800151008       //?Calcolo orario indicativo ritiro.
332900151008       //--------------------------------------------------------------
333000151008       BEGSR  OrarioIndRitiro;
333100151008
333200151008       //?imposto orario ritiro standard
333300151012         fiora0O0.orariti = fiora0O0.orainizio;
333400151012         fiora0O0.oraritf = fiora0O0.orafine;
333500151126
333600151126       //?Se l'ora pronta merce è maggiore
333700151126       //?dell'ora inizio servizio
333800151126       //?e minore dell'ora fine
333900151126       //?la imposto come orario inzio
334000151126         IF  fiora0O0.vaoorr > fiora0O0.orainizio and
334100151126             fiora0O0.vaoorr < fiora0O0.orafine;
334200151126           fiora0O0.orariti = fiora0O0.vaoorr;
334300151126         ENDIF;
334400151126
334500151126       //?Se l'ora pronta merce è maggiore
334600151126       //?dell'ora fine servizio
334700151126       //?la imposto come orario fine
334800151126         IF  fiora0O0.vaoorr > fiora0O0.orafine;
334900151126           fiora0O0.oraritf = fiora0O0.vaoorr;
335000151126         ENDIF;
335100151008
335200151008       ENDSR;
335300160318
335400160318       //--------------------------------------------------------------
335500160318       //?Controllo SMS x accettazione ORM.
335600160318       //--------------------------------------------------------------
335700160318       BEGSR  SmsConferma;
335800160318
335900160318         fiora0O0.alertconfs = 'N';
336000160318
336100160318         IF  fiora0I0.smsconf = *blanks and fiora0I0.alertconf = 'S' and
336200160318             fiora0I0.mailconf = *blanks;
336300160318           wIdMsg   = 'TIS0831';
336400160318           wIdCampo = 'SMSCONF';
336500160318           clear wParm;
336600160318           exsr Messaggi;
336700160318           leavesr;
336800160318         ENDIF;
336900160318
337000160503         IF  fiora0I0.smsconf <> *blanks and fiora0I0.alertconf = 'S';
337100160318           pInCell = %trim(fiora0I0.smsconf);
337200160318           clear pOutErr;
337300160318           clear pOutCell;
337400160318           IF  UBCHKCEL_Check(pInCell:pOutCell:pOutErr) < 0 OR pOutErr = 'E';
337500160318             wIdMsg   = 'TIS0725';
337600160318             wIdCampo = 'SMSCONF';
337700160318             clear wParm;
337800160318             exsr Messaggi;
337900160601             fiora0M0.skTextMsg (fiora0M0.nrmsg) =
338000160601             %trim(fiora0M0.skTextMsg (fiora0M0.nrmsg)) + ' Numero ' +
338100160601             fiora0I0.smsconf;
338200160318             leavesr;
338300160318           ENDIF;
338400160318
338500160318       //?Passo il dato solo se impostato a 'SI'
338600160503         //IF  fiora0I0.alertconf = 'S';
338700160318           fiora0O0.smsconf = fiora0I0.smsconf;
338800160318           IF  pOutCell <> *blanks;
338900160318             fiora0O0.smsconf = pOutCell;
339000160318           ENDIF;
339100160318         //?Passo anche il nuovo flag
339200160318           IF  fiora0O0.smsconf <> *blanks;
339300160318             fiora0O0.alertconfs = 'S';
339400160318           ENDIF;
339500160503         //ENDIF;
339600160502         ENDIF;
339700160318
339800160318       ENDSR;
339900160315
340000160315       //--------------------------------------------------------------
340100160315       //?Cerco il giro di ritiro.
340200160315       //--------------------------------------------------------------
340300160315       BEGSR  GiroRitiro;
340400160315
340500160315         fiora0O0.cgi = fiora0I0.cgi;
340600160315
340700160315       //?Se non l'ho ancora fatto carico la tabella AGR
340800160315         IF  not wAGR;
340900160315         //?Reperimento tabella AGR
341000160315           clear dAGR;
341100160315           k_TBEcod = 'AGR';
341200160315           k_TBEke1 = %editc(fiora0I0.vaopor:'X');
341300160315           chain (k_TBEcod:k_TBEke1) TNTBE01L;
341400160315           IF  %found(TNTBE01L);
341500160315             dAGR = TBEuni;
341600160315           ENDIF;
341700160315           wAGR = *on;
341800160315         ENDIF;
341900160315
342000160315       //?Imposto l'ambito STANDARD '='
342100160315         wAmbito = '=';
342200160315       //?In base alle qtà verifico se ambito OLTRE o SOTTO
342300160315         IF  fiora0I0.Peso > dagr.§AGRpkgo or
342400160315             fiora0I0.Volume > dagr.§AGRvlmo or
342500160315             fiora0I0.VAObnc > dagr.§AGRbnco;
342600160315           wAmbito = '>';
342700160315         ENDIF;
342800160518         IF  (fiora0I0.Peso > 0 and
342900160518              fiora0I0.Peso < dagr.§AGRpkgs) or
343000160518             (fiora0I0.Volume > 0 and
343100160518              fiora0I0.Volume < dagr.§AGRvlms) or
343200160518             (fiora0I0.VAObnc > 0 and
343300160518              fiora0I0.VAObnc < dagr.§AGRbncs);
343400160315           wAmbito = '<';
343500160315         ENDIF;
343600160315
343700160315       //?Recupero il giro dall'anagrafica clienti ritiro
343800160315         IF  fiora0I0.VAOcra > 0 and fiora0O0.cgi = *blanks;
343900160315           fiora0O0.cgi =
344000160315           GetGiroRitiro(fiora0I0.VAOcra:fiora0I0.VAOpor:wAmbito);
344100160315         ENDIF;
344200160315
344300160412       //?Se giro impostato lo controllo
344400160315         IF  fiora0O0.cgi <> *blanks;
344500160315           clear FIDG09DS;
344600160315           fidg09ds.D09iop0 = '001';
344700160518           fidg09ds.D09ifgs = fiora0I0.VAOpor;
344800160315           fidg09ds.D09idat = dataoggi;
344900160315           fidg09ds.D09icgi = fiora0O0.cgi;
345000160315           fidg09ds.D09itug = 'R';
345100160315           %subst(kpjba:247:256) = FIDG09DS;
345200160315           fidg09r (kpjba);
345300160315           fidg09ds = %subst(kpjba:247:256);
345400160315           IF  fidg09ds.D09oerr <> *blanks;
345500160315             wIdMsg   = 'TIS0718';
345600160315             wIdCampo = 'CGI';
345700160315             wParm = fidg09ds.D09omsg;
345800160315             exsr Messaggi;
345900160315             leavesr;
346000160315           ENDIF;
346100160315         ENDIF;
346200160315
346300160315
346400160315       //?Recupero il giro dal ritiro fisso
346500160315       //?===================================
346600160315
346700160315       ENDSR;
346800160315
346900160315       //--------------------------------------------------------------
347000160315       //?Controlla i dati per ORM commissionato.
347100160315       //--------------------------------------------------------------
347200160315       BEGSR  ControllaCommissionato;
347300160315
347400160315
347500160315
347600160315       ENDSR;
347700140312
347800131008       //--------------------------------------------------------------
347900131008       //?Routine per schierare i messaggi nella ds FIORA0M0.
348000131008       //--------------------------------------------------------------
348100131008       BEGSR  Messaggi;
348200131007
348300131008       //?Se ho già caricato 99 messaggi esco
348400131008         IF  fiora0M0.nrmsg = 99;
348500131010           exsr RoutEnd;
348600131008           clear fiora0M0.nrmsg;
348700131008         ENDIF;
348800131008
348900131008         fiora0M0.nrmsg += 1;
349000131009         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
349100131009         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
349200140211         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_ERRORE;
349300131204         IF  wParm <> *blanks;
349400131219           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
349500131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
349600131204         ELSE;
349700131219           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
349800131204           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
349900131204         ENDIF;
350000131008
350100131008       ENDSR;
350200131008
350300131008       //--------------------------------------------------------------
350400131008       //?Routine per schierare le forzature nella ds FIORA0F0.
350500131008       //--------------------------------------------------------------
350600131008       BEGSR  Forzatura;
350700160531
350800160531         wForza = *off;
350900160530
351000160530       //?Se già forzato non riemetto il messaggio
351100160531         wNrForza = %lookup(wIdMsg:fiora0F0.skIdMsg);
351200160531         IF  wNrForza > 0 and fiora0F0.skGiaForza(wNrForza) =
351300160530             FIORA00_GIAFORZA_SI;
351400160530           leavesr;
351500160530         ENDIF;
351600160531
351700160531       //?Se ho già caricato 99 mesaggi esco
351800160531         IF  fiora0M0.nrmsg = 99;
351900160531           exsr RoutEnd;
352000160531           clear fiora0M0.nrmsg;
352100160531         ENDIF;
352200160530
352300160531       //?Imposto messaggio da forzare
352400160530         fiora0M0.nrmsg += 1;
352500160530         fiora0M0.skIdMsg(fiora0M0.nrmsg) = wIdMsg;
352600160530         fiora0M0.skIdCampo(fiora0M0.nrmsg) = wIdCampo;
352700160530         fiora0M0.skErrWarn(fiora0M0.nrmsg) = FIORA00_MSG_WARNING;
352800160530         IF  wParm <> *blanks;
352900160530           fiora0M0.skTextMsg (fiora0M0.nrmsg) =
353000160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua:wParm);
353100160530         ELSE;
353200160530           fiora0M0.skTextMsg (fiora0M0.nrmsg)=
353300160530           rtvMsgLang(wIdMsg:fiora0I0.idlingua);
353400160530         ENDIF;
353500160531
353600160531         wForza = *on;
353700131008
353800131008       ENDSR;
353900131009
354000131009       //--------------------------------------------------------------
354100131009       //?Imposta i dati nella ds di Output.
354200131009       //--------------------------------------------------------------
354300131009       BEGSR  DsOutput;
354400131008
354500131009       //?Se richiamo da Internet
354600131009         IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
354700131223
354800131223         //?Imposto se commissionato
354900140407           IF  wCom;
355000131223             dorm01.§orcomc = 'S';
355100131223           ELSE;
355200131223             dorm01.§orcomc = 'N';
355300131223           ENDIF;
355400140506
355500140507         //?Se ORM commissionato e data ritiro passata al pgm = 0
355600140507         //?vuol dire che non è ancora stata calcolata quindi siamo
355700140507         //?nel passaggio tra la prima e la seconda pagina
355800140507           IF  wCom and fiora0I0.vaodar = 0;
355900140507           //?se mail e/o sms impostati torno 'S' nei flag degli alert
356000140507             IF  fiora0O0.mail <> *blanks;
356100140507               fiora0O0.alertmail = 'S';
356200140507             ENDIF;
356300140507             IF  fiora0O0.sms <> *blanks;
356400140507               fiora0O0.alertsms = 'S';
356500140507             ENDIF;
356600140507           ENDIF;
356700140507
356800140507         //?Se ORM commissionato e data ritiro (calcolata o impostata dall'utente) = oggi
356900140506         //?imposto i flag alert a 'N'
357000140508         //?ma se data ritiro passata è > di 0, in questo caso vuol dire che siamo già
357100140508         //?in seconda pagina e l'utente ha messo oggi al posto di domani
357200140508           IF  wCom and fiora0O0.vaodar = dataoggi and fiora0I0.vaodar > 0;
357300140507             IF  fiora0O0.mail <> *blanks;
357400140506               fiora0O0.alertmail = 'N';
357500140506             ENDIF;
357600140507             IF  fiora0O0.sms <> *blanks;
357700140506               fiora0O0.alertsms = 'N';
357800140506             ENDIF;
357900140506           ENDIF;
358000131009
358100131009         ENDIF;
358200131223
358300131227         fiora0O0.idrichiamo = fiora0I0.idrichiamo;
358400131223         fiora0O0.idlingua = fiora0I0.idlingua;
358500140402
358600140402         IF  fiora0I0.vaorfa <> *blanks;
358700140402           exec sql
358800140402           set :fiora0O0.vaorfa = ucase (:fiora0I0.vaorfa);
358900140402         ENDIF;
359000131223
359100131223         IF  fiora0I0.vaono1 <> *blanks;
359200140211           exec sql
359300140211           set :fiora0O0.vaono1 = ucase (:fiora0I0.vaono1);
359400131223         ENDIF;
359500131223         IF  fiora0I0.vaono2 <> *blanks;
359600140211           exec sql
359700140211           set :fiora0O0.vaono2 = ucase (:fiora0I0.vaono2);
359800131223         ENDIF;
359900131223
360000131205         fiora0O0.vaoflo = dorm01;
360100131009
360200131009       ENDSR;
360300140408
360400140408       //--------------------------------------------------------------
360500140408       //?Richiamo pgm per calcolo orari servizio.
360600140408       //--------------------------------------------------------------
360700140408       BEGSR  CallTrulORS;
360800140408
360900140408         clear TRULORSds;
361000140408         trulorsds.IOREfil  = wPor;
361100140408         trulorsds.IORecap  = wCap;
361200140408         trulorsds.IOREloc  = wLocalita;
361300140408         trulorsds.IOREdta  = wData;
361400140408         trulorsds.IOREtser = 'R';
361500140408         trulorsr (kpjba:trulorsds:trulor2ds);
361600140408
361700140408       ENDSR;
361800160526
361900160526       //--------------------------------------------------------------
362000160526       //?Richiamo pgm per calcolo orari servizio estero.
362100160526       //--------------------------------------------------------------
362200160526       BEGSR  CallFIORE;
362300160526
362400160526         clear FIORE00ds;
362500160526         fiore00ds.IORE00por = wPor;
362600160526         fiore00ds.IORE00nar = wNazione;
362700160526         fiore00r (fiore00ds);
362800160526
362900160526       ENDSR;
363000140408
363100140408       //--------------------------------------------------------------
363200140408       //?Imposto la frase dal tornare al chiamante.                   ?
363300140408       //--------------------------------------------------------------
363400140408       BEGSR  GetFrase1;
363500140408
363600140408       //?Se ID lingua non valido o non passato
363700140408       //?assumo italiano
363800140408         IF  fiora0I1.idlingua = *blanks or
363900140408            (fiora0I1.idlingua <> FIORA00_ID_LINGUA_IT and
364000140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_FR and
364100140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_EN and
364200140408             fiora0I1.idlingua <> FIORA00_ID_LINGUA_DE);
364300140408            fiora0I1.idlingua = FIORA00_ID_LINGUA_IT;
364400140408         ENDIF;
364500140408
364600140408       //?Cerco gli orari di servizio
364700140408         wPor      = fiora0I1.filritiro;
364800140408         wData     = dataoggi;
364900140408         wCap      = fiora0I1.cap;
365000140408         wLocalita = fiora0I1.localita;
365100140408         exsr CallTrulORS;
365200160526
365300160526       //?Dalla versione "C" cerco la frase anche per i ritiri esteri
365400160526         SELECT;
365500160531         WHEN  fiora0I1.versione > 'B';
365600160531           wNetworkRit = GetNtwFiliale(fiora0I1.filritiro);
365700160531           clear dNTW;
365800160531           k_TBEcod = 'NTW';
365900160531           k_TBEke1 = wNetworkRit;
366000160531           chain (k_TBEcod:k_TBEke1) TNTBE01L;
366100160531           IF  %found(TNTBE01L);
366200160531             dNTW = TBEuni;
366300160531           ENDIF;
366400160531           IF  wNetworkRit = FIORA00_NTW_DPD or
366500160531               dntw.§NTWfie = FIORA00_NTW_ESTERO;
366600160526             wPor     = fiora0I1.filritiro;
366700160526             wNazione = fiora0I1.nazione;
366800160526             exsr CallFIORE;
366900160526           ENDIF;
367000160526         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
367100160526         //?non devo emettere la frase dato che la merce parte sempre il
367200160526         //?giorno dopo e la data ritiro è sempre il giorno dopo
367300160526           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
367400160526               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
367500160526             clear fiora0O1.frase;
367600160526             fiora0O1.idlingua = fiora0I1.idlingua;
367700160526             leavesr;
367800160526           ENDIF;
367900160526         //?Se non è un prepagato imposto la frase
368000160526           wIdMsg   = 'TIS0827';
368100160526           SELECT;
368200160526           //?Ritiri esteri
368300160526           WHEN  fiora0I1.nazione <> *blanks;
368400160526             wParm = %editw(fiore00ds.OORE00lrg:'  :  ');
368500160526           //?Ritiri commissionati
368600160526           WHEN  fiora0I1.contattot = 'S';
368700160526             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
368800160526           OTHER;
368900160526           //?Ritiri normali
369000160526             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
369100160526           ENDSL;
369200160526         OTHER;
369300140408
369400140408       //?Imposto la frase
369500151104       //?Frasi diverse in base alla versione
369600151104         IF  fiora0I1.versione > 'A';
369700151104         //?Se cliente NON LOGGATO (anonimo) e paga mittente (Prepagato)
369800151104         //?non devo emettere la frase dato che la merce parte sempre il
369900151104         //?giorno dopo e la data ritiro è sempre il giorno dopo
370000151104           IF  fiora0I1.anonimo = FIORA00_ANONIMO and
370100151104               fiora0I1.chipaga = FIORA00_PAGA_MITTENTE;
370200151104             clear fiora0O1.frase;
370300151104             fiora0O1.idlingua = fiora0I1.idlingua;
370400151104             leavesr;
370500151104           ENDIF;
370600151104         //?Se cliente codificato imposto la nuova frase
370700151104           wIdMsg   = 'TIS0827';
370800151104           IF  fiora0I1.contattot = 'S';
370900151104             wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
371000151104           ELSE;
371100151104             wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
371200151104           ENDIF;
371300151104         ELSE;
371400140408         wIdMsg   = 'TIS0731';
371500140408         IF  fiora0I1.contattot = 'S';
371600140408           wParm = %editw(trulor2ds.OOR2lrsc:'  :  ');
371700140408         ELSE;
371800140408           wParm = %editw(trulor2ds.OOR2lrnc:'  :  ');
371900140408         ENDIF;
372000151104         ENDIF;
372100160526
372200160526         ENDSL;
372300140408
372400160531       //?Per ora ND ha deciso di non impostare la frase per i ritiri esteri
372500160531         IF  fiora0I1.nazione <> *blanks and SaltaFraseExport <> *blanks;
372600160531           clear fiora0O1.frase;
372700160531           leavesr;
372800160531         ENDIF;
372900160531
373000140408         fiora0O1.frase =
373100140408         rtvMsgLang(wIdMsg:fiora0I1.idlingua:wParm);
373200140408
373300140408         fiora0O1.idlingua = fiora0I1.idlingua;
373400140408
373500140408       ENDSR;
373600131010
373700131010       //--------------------------------------------------------------
373800131010       //?Operazioni finali.
373900131010       //--------------------------------------------------------------
374000131010       BEGSR  RoutEnd;
374100140408
374200140408       //?Se richiamato per GET_FRASE1
374300140408         IF  prmRqsOpCode = FIORA00_RQSOPCODE_GET_FRASE1;
374400140408           %subst(prmRpyData:1:prmRpyDataSize) = fiora0O1;
374500140408         ELSE;
374600131010
374700131219         %subst(prmRpyData:1:prmRpyDataSize) = fiora0O0;
374800131219         %subst(prmRpyMessage:1:prmRpyMsgSize) = fiora0M0;
374900131219         %subst(prmRpyForzatu:1:prmRpyForSize) = fiora0F0;
375000140408
375100140408         ENDIF;
375200131010
375300131010         return;
375400131010
375500131010       ENDSR;
375600131009
375700131007      /end-free
375800140312
375900140313      *--------------------------------------------------
376000140313      * Procedure name: IsCodiceLegato
376100140313      * Purpose:        Controlla codice
376200140402      * Returns:        ON - OFF
376300140313      *--------------------------------------------------
376400140313     p IsCodiceLegato  B
376500140313     d IsCodiceLegato  PI              n
376600140312
376700140313      /copy gaitrasrc/srcconst,FIORB00R
376800140313     d isErrore        s               n   inz
376900140313     d RpyIdMsg        s             10i 0
377000140313     d RpyOpCode       s             10i 0
377100140313     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_LEGGI)
377200140313     d wNrRec          s              7s 0 inz
377300140313     d fiorb0I0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
377400140313     d fiorb0O0      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
377500140313     d  skCodice                     10a   dim(10) overlay(Codice)
377600140313      /copy gaitrasrc/srcprotopr,FIORB00R
377700140312
377800140313      /free
377900140312
378000140313       reset isErrore;
378100140313       reset RpyIdMsg;
378200140313       reset RpyOpCode;
378300140313       reset fiorb0I0;
378400140313       reset fiorb0O0;
378500140312
378600140313       fiorb0I0.codiceksu = *all'0';
378700140313       %subst(fiorb0I0.codiceKsu:2:7) =
378800140312       %subst(%editc(wCodice:'X'):1:7);
378900140312       //?Solo per richiamo da Internet
379000140313       IF  fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_INTERNET;
379100140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
379200140313       ELSE;
379300140313         fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
379400140313       ENDIF;
379500140312
379600160519       DOU  wNrRec = fiorb0O0.clientitot
379700140313         OR fiorb0O0.nrrec = *ZERO;
379800140313
379900140313         MONITOR;
380000140313           Fiorb00_Anagrafica( RqsOpCode
380100140313                             : RpyOpCode
380200140313                             : RpyIdMsg
380300140313                             : fiorb0I0.formato
380400140313                             : fiorb0I0
380500140313                             : %SIZE(fiorb0I0)
380600140313                             : fiorb0O0.formato
380700140313                             : fiorb0O0
380800140313                             : %SIZE(fiorb0O0)
380900140313                             );
381000140313         ON-ERROR;
381100140313           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
381200140313         ENDMON;
381300140312
381400160519         IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
381500140313           isErrore = *on;
381600140313           leave;
381700140313         ENDIF;
381800140313
381900140313         //?Controllo se il codice fa parte della stessa famiglia
382000140313         //?se fa parte esco dal LOOP
382100140313         IF  %lookup(%editc(wCodice:'X'):fiorb0O0.skCodice) > 0;
382200140313           leave;
382300140313         ENDIF;
382400140313
382500140313         wNrRec += 1;
382600140313         RqsOpCode = FIORB00_RQSOPCODE_CARICA;
382700140312
382800140313       ENDDO;
382900140312
383000140313       Fiorb00_Anagrafica( FIORB00_RQSOPCODE_CLOSE
383100140313                           : RpyOpCode
383200140313                           : RpyIdMsg
383300140312                           );
383400140313
383500140313       IF  isErrore;
383600140313         return *off;
383700140313       ENDIF;
383800140312
383900140313       RETURN *on;
384000140312
384100140312      /END-FREE
384200140313     p IsCodiceLegato  E
384300140312
384400140401      *--------------------------------------------------
384500140401      * Procedure name: IsCodiceValido
384600140401      * Purpose:        Controlla codice
384700140402      * Returns:        ON - OFF
384800140401      *--------------------------------------------------
384900140401     p IsCodiceValido  B
385000140401     d IsCodiceValido  PI              n
385100140402     d  FIORB0O1                           likeds(FIORB0OO)
385200140401
385300140401      /copy gaitrasrc/srcconst,FIORB00R
385400140401     d isErrore        s               n   inz
385500140401     d RpyIdMsg        s             10i 0
385600140401     d RpyOpCode       s             10i 0
385700140401     d RqsOpCode       s             10i 0 inz(FIORB00_RQSOPCODE_GETDATI)
385800140401     d fiorb0I1      e ds                  QUALIFIED STATIC INZ(*EXTDFT)
385900140401      /copy gaitrasrc/srcprotopr,FIORB00R
386000140401
386100140401      /free
386200140401
386300140401       reset isErrore;
386400140401       reset RpyIdMsg;
386500140401       reset RpyOpCode;
386600140401       reset fiorb0I1;
386700140401
386800140401       fiorb0I1.codice = wCodice;
386900140401       MONITOR;
387000140401         Fiorb00_Anagrafica( RqsOpCode
387100140401                           : RpyOpCode
387200140401                           : RpyIdMsg
387300140401                           : fiorb0I1.formato
387400140401                           : fiorb0I1
387500140401                           : %SIZE(fiorb0I1)
387600140401                           : fiorb0O1.formato
387700140401                           : fiorb0O1
387800140401                           : %SIZE(fiorb0O1)
387900140401                           );
388000140401         ON-ERROR;
388100140401           RpyOpCode = FIORB00_RPYOPCODE_ERROR;
388200140401         ENDMON;
388300140401
388400160519       IF  RpyOpCode < FIORB00_RPYOPCODE_DONE;
388500140401         isErrore = *on;
388600140401       ENDIF;
388700160519
388800160519       IF  RpyOpCode = FIORB00_RPYOPCODE_NOTFOUND and
388900160519           fiora0I0.idrichiamo = FIORA00_ID_RICHIAMO_AS400;
389000160519         isErrore = *on;
389100160519       ENDIF;
389200140401
389300140401       IF  isErrore;
389400140401         return *off;
389500140402       ENDIF;
389600140402
389700140402       RETURN *on;
389800140401
389900140401      /END-FREE
390000140401     p IsCodiceValido  E
390100140411
390200140411     P*--------------------------------------------------
390300140411     P* Procedure name: GetOraAggiustata
390400140411     P* Purpose:        Restituisce l'ora aggiustata ai 15 minuti.
390500140411     P* Returns:        Ora e minuto aggiustati.
390600140411     P* Parameter:      hhmm => Ora e minuto
390700140411     P*--------------------------------------------------
390800140411     P GetOraAggiustata...
390900140411     P                 B
391000140411     D GetOraAggiustata...
391100140411     D                 PI             4A
391200140411     D  hhmm                          4S 0 CONST
391300140411     D retField        DS             4    QUALIFIED STATIC
391400140411     D  hhmm                   1      4S 0
391500140411     D  hh                     1      2S 0
391600140411     D  mm                     3      4S 0
391700140411
391800140411      /FREE
391900140411
392000140411       IF hhmm = *ZERO;
392100140411         RETURN *BLANK;
392200140411       ENDIF;
392300140411
392400140411       retField.hhmm = hhmm;
392500140411
392600140411       SELECT;
392700140411         WHEN retField.mm = 00 OR retField.mm = 15
392800140411           OR retField.mm = 30 OR retField.mm = 45;
392900140411           // Non è da aggiustare.
393000140411         WHEN retField.mm < 15;
393100140411           retField.mm = 15;
393200140411         WHEN retField.mm < 30;
393300140411           retField.mm = 30;
393400140411         WHEN retField.mm < 45;
393500140411           retField.mm = 45;
393600140411         OTHER;
393700140411           retField.hh += 1;
393800140411           retField.mm = 00;
393900140411       ENDSL;
394000140411
394100140411       RETURN retField;
394200140411
394300140411      /end-free
394400140411     P GetOraAggiustata...
394500140411     P                 E
394600160315
394700160315     P*--------------------------------------------------
394800160315     P* Procedure name: GetGiroRitiro
394900160315     P* Purpose:        Restituisce il giro per il ritiro.
395000160315     P* Returns:        Giro
395100160315     P* Parameter:      Cliente mittente - filiale ritiro - ambito
395200160315     P*--------------------------------------------------
395300160315     P GetGiroRitiro...
395400160315     P                 B
395500160315     D GetGiroRitiro...
395600160315     D                 PI            14A
395700160315     D  Mittente                     10S 0 CONST
395800160315     D  FilRitiro                     3S 0 CONST
395900160315     D  Ambito                        1a
396000160315     D retField        DS            10    QUALIFIED STATIC
396100160315     D  giro                   1     10a
396200160315
396300160315      /FREE
396400160315
396500160315       IF  mittente = *ZERO;
396600160315         RETURN *BLANK;
396700160315       ENDIF;
396800160315
396900160315       IF  filritiro = *ZERO;
397000160315         RETURN *BLANK;
397100160315       ENDIF;
397200160315
397300160315       IF  ambito = *blanks;
397400160315         RETURN *BLANK;
397500160315       ENDIF;
397600160315
397700160315       chain  (mittente:filritiro:ambito) FNACR13L;
397800160315       IF  %found(FNACR13L) and ACR1cgi <> *blanks;
397900160315         retfield.giro = ACR1cgi;
398000160315       ELSE;
398100160601         IF  ambito <> '=';
398200160315           ambito = '=';
398300160315           chain  (mittente:filritiro:ambito) FNACR13L;
398400160315           IF  %found(FNACR13L) and ACR1cgi <> *blanks;
398500160315             retfield.giro = ACR1cgi;
398600160315           ENDIF;
398700160315         ENDIF;
398800160315       ENDIF;
398900160315
399000160315       RETURN retField;
399100160315
399200160315      /end-free
399300160315     P GetGiroRitiro...
399400160315     P                 E
399500160315
399600160315      *--------------------------------------------------
399700160315      * Procedure name: IsFilValida
399800160315      * Purpose:        Ritorna se filiale è valida
399900160315      * Returns:        On - OFF
400000160315      * Parameter:      Filiale
400100160315      *--------------------------------------------------
400200160315     p IsFilValida     B
400300160315     d IsFilValida     PI              n
400400160315     D  filiale                       3s 0 CONST
400500160315
400600160315      /free
400700160315
400800160315       IF  filiale = *zeros;
400900160315         RETURN *off;
401000160315       ENDIF;
401100160315
401200160315       chain (filiale) AZORG01L;
401300160315       IF  not %found(AZORG01L);
401400160315         RETURN *off;
401500160315       ENDIF;
401600160315       IF  ORGfva <> *blanks;
401700160315         RETURN *off;
401800160315       ENDIF;
401900160315       IF  ORGfag <> 'F' and ORGfag <> 'A';
402000160315         RETURN *off;
402100160315       ENDIF;
402200160315
402300160315       RETURN *on;
402400160315
402500160315      /end-free
402600160315     P IsFilValida     E
402700160315
402800160315      *--------------------------------------------------
402900160315      * Procedure name: GetNtwFiliale
403000160315      * Purpose:        Recupera il Network della filiale
403100160315      * Returns:        Network
403200160315      * Parameter:      Filiale
403300160315      *--------------------------------------------------
403400160315     p GetNtwFiliale   B
403500160315     d GetNtwFiliale   PI            20a
403600160315     D  filiale                       3s 0 CONST
403700160315
403800160315      // ds per campo ORGDE3 di AZORG
403900160315     d OG143         e ds
404000160315
404100160315      /free
404200160315       IF  filiale = *zeros;
404300160315         RETURN *blanks;
404400160315       ENDIF;
404500160315
404600160315       clear OG143;
404700160315       chain (filiale) AZORG01L;
404800160315
404900160315       OG143 = ORGde3;
405000160315
405100160315       RETURN §OGntw;
405200160315
405300160315      /end-free
405400160315     P GetNtwFiliale   E
405500160315
405600160315      *--------------------------------------------------
405700160315      * Procedure name: IsFilAttivaORM
405800160315      * Purpose:        Ritorna se filiale attiva alla procedura ORM
405900160315      * Returns:        On - OFF
406000160315      * Parameter:      Filiale
406100160315      *--------------------------------------------------
406200160315     p IsFilAttivaORM  B
406300160315     d IsFilAttivaORM  PI              n
406400160315     D  filiale                       3s 0 CONST
406500160315
406600160315      // ds per campo ORGDE8 di AZORG
406700160315     d OG148         e ds
406800160315
406900160315      /free
407000160315
407100160315       IF  filiale = *zeros;
407200160315         RETURN *off;
407300160315       ENDIF;
407400160315
407500160315       clear OG148;
407600160315
407700160315       chain (filiale) AZORG01L;
407800160315
407900160315       OG148 = ORGde8;
408000160315
408100160315       IF  §OGorm <> 'S';
408200160315         return *off;
408300160315       ENDIF;
408400160315
408500160315       RETURN *on;
408600160315
408700160315      /end-free
408800160315     P IsFilAttivaORM  E
408900160315
409000160315      *--------------------------------------------------
409100160315      * Procedure name: IsKscValido
409200160315      * Purpose:        Ritorna se Ksc è valido
409300160315      * Returns:        On - OFF
409400160315      * Parameter:      cliente
409500160315      *--------------------------------------------------
409600160315     p IsKscValido     B
409700160315     d IsKscValido     PI              n
409800160315     D  cliente                       7s 0 CONST
409900160315
410000160315      // campi di comodo
410100160315     d filiale         s              3s 0
410200160401     d k_kcc           s                   like(ACOkcc) inz(151)
410300160401     d k_kut           s                   like(ACOkut) inz(1)
410400160315     d network         s              3a
410500160315
410600160315      /free
410700160315
410800160315       IF  cliente = *zeros;
410900160315         RETURN *off;
411000160315       ENDIF;
411100160315
411200160401       chain (k_kut:k_kcc:cliente) CNACO00F;
411300160315       IF  not %found(CNACO00F);
411400160315         RETURN *off;
411500160315       ENDIF;
411600160315       IF  ACOflg <> *blanks;
411700160315         RETURN *off;
411800160315       ENDIF;
411900160315
412000160315      // cerco il ntw del cliente non posso accettare clienti
412100160315      // logistica o di sede
412200160401       filiale = %dec(%subst(%editc(cliente:'X'):1:3):3:0);
412300160315       network = GetNtwFiliale(filiale);
412400160315       IF  network = 'LOG' or network = 'XXX';
412500160315         RETURN *off;
412600160315       ENDIF;
412700160315
412800160315       RETURN *on;
412900160315
413000160315      /end-free
413100160315     P IsKscValido     E
413200160315
413300160315      *--------------------------------------------------
413400160315      * Procedure name: IsValidoCapDPD
413500160315      * Purpose:        Ritorna se Cap DPD valido
413600160315      * Returns:        On - OFF
413700160315      * Parameter:      nazione - cap del mittente
413800160315      *--------------------------------------------------
413900160315     p IsValidoCapDPD  B
414000160315     d IsValidoCapDPD  PI              n
414100160315     D  nazione                       3a   CONST
414200160315     D  cap                           9a   CONST
414300160315     D  data                          8s 0 CONST
414400160315     D  peso                          7s 1 CONST
414500160315     D  limite                        7s 1 CONST
414600160315     D  filiale                       3s 0 CONST
414700160315
414800160315      // ds per cappario DPD
414900160315     d TISIE3DS      e ds                  QUALIFIED INZ
415000160315      // pgm per cappario DPD
415100160315     d TISIE3R         pr                  extpgm('TISIE3R')
415200160315     d  tisie3ds                           likeds(TISIE3DS)
415300160315
415400160315      /free
415500160315
415600160315       IF  nazione = *blanks or cap = *blanks or filiale = 0;
415700160315         RETURN *off;
415800160315       ENDIF;
415900160315
416000160315       clear TISIE3DS;
416100160315       IF  data = 0;
416200160315         tisie3ds.ISIE3dri = %dec(%date());
416300160315       ELSE;
416400160315         tisie3ds.ISIE3dri = data;
416500160315       ENDIF;
416600160315
416700160315       tisie3ds.ISIE3hsp = %dec(%time());
416800160315       tisie3ds.ISIE3nzd = nazione;
416900160315       tisie3ds.ISIE3cad = cap;
417000160315       IF  peso > limite;
417100160315         tisie3ds.ISIE3srv = 101;
417200160315       ELSE;
417300160315         tisie3ds.ISIE3srv = 136;
417400160315       ENDIF;
417500160315       tisie3ds.ISIE3lnp = filiale;
417600160315       tisie3r (tisie3ds);
417700160315       IF  tisie3ds.OSIE3err <> *blanks;
417800160315         RETURN *off;
417900160315       ENDIF;
418000160315
418100160315       RETURN *on;
418200160315
418300160315      /end-free
418400160315     P IsValidoCapDPD  E
