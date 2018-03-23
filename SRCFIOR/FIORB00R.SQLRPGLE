000100140306       //==============================================================
000200140306       //
000300140402       //?FIORB00R - Anagrafica Ritiri
000400140402       // Recupera Anagrafiche di tutti i clienti legati allo stesso KSU
000500140402       // Recupera i dati Anagrafici di UN solo codice
000600170418       // Recupera i giri std - oltre - sotto
000700170418       //                 di un solo codice per filiale ritiro
000800140306       //
000900140306       //==============================================================
001000140306
001100140306       //--------------------------------------------------------------
001200140306       //?Specifiche di controllo.                                     ?
001300140306       //--------------------------------------------------------------
001400140306     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS')
001500140306
001600140306       //--------------------------------------------------------------
001700140306       //?Dichiarazione file.                                          ?
001800140306       //--------------------------------------------------------------
001900140307       // -?Clienti abilitati a VAS
002000140307     fTIVSS02L  if   e           k disk
002100140317       // -?Anagrafica clienti
002200140317     fFNACR01L  if   e           k disk
002300151007       // -?Anagrafica clienti ritiro - estensione
002400151007     fFNACRE1L  if   e           k disk
002500170418       // -?Anagrafica clienti ritiro - giri
002600170418     fFNACR11L  if   e           k disk
002700140306
002800140306       //--------------------------------------------------------------
002900140306       //?Definizione costanti.                                        ?
003000140306       //--------------------------------------------------------------
003100140307      /copy gaitrasrc/srcconst,FIORB00R
003200140401
003300140401     d c_Digits        c                   const('0123456789')
003400151007
003500151007     d MailAccettazione...
003600151007     d                 c                   const('MC')
003700160318
003800160318     d SmsAccettazione...
003900160318     d                 c                   const('SC')
004000140306
004100140306       //--------------------------------------------------------------
004200140306       //?Definizione strutture dati.                                  ?
004300140306       //--------------------------------------------------------------
004400140306       // -?Dati INPUT
004500140307     d FIORB0I0      e ds                  QUALIFIED INZ(*EXTDFT)
004600140401     d FIORB0I1      e ds                  QUALIFIED INZ(*EXTDFT)
004700170418     d FIORB0I2      e ds                  QUALIFIED INZ(*EXTDFT)
004800140306
004900140306       // -?Dati OUTPUT
005000140307     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
005100140307     d  skCodice                     10a   dim(10) overlay(Codice)
005200140307     d  skNome                       48a   dim(10) overlay(Nome)
005300140307     d  skIndirizzo                  35a   dim(10) overlay(Indirizzo)
005400140307     d  skCap                         9a   dim(10) overlay(Cap)
005500140307     d  skLocalita                   35a   dim(10) overlay(Localita)
005600140307     d  skProvincia                   2a   dim(10) overlay(Provincia)
005700140307     d  skNazione                     3a   dim(10) overlay(Nazione)
005800140401     d FIORB0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005900170418     d FIORB0O2      e ds                  QUALIFIED INZ(*EXTDFT)
006000140307
006100140307       // -?Legami clienti
006200140307     d TIBS10DS      e ds                  QUALIFIED INZ(*EXTDFT)
006300140311     d  skKsc11               21   5520  0 dim(500)
006400140401
006500140401       // -?Campo ACRMAI
006600140401     d dACR01        e ds                  QUALIFIED INZ
006700140307
006800140307       // -?Ds multipla per ritorno dati a chiamante
006900140307     d anagrafica      ds                  QUALIFIED OCCURS(10)
007000140310     d  cro                                like(ACRcro)
007100140310     d  rsr                                like(ACRrsr)
007200140310     d  inr                                like(ACRinr)
007300140310     d  car                                like(ACRcar)
007400140310     d  lor                                like(ACRlor)
007500140310     d  prr                                like(ACRprr)
007600140310     d  nar                                like(ACRnar)
007700140306
007800140306       //--------------------------------------------------------------
007900140306       //?Definizione schiere.
008000140306       //--------------------------------------------------------------
008100140307     d skKsc           s              7s 0 dim(500)
008200140306
008300140306       //--------------------------------------------------------------
008400140306       //?Definizione campi.
008500140306       //--------------------------------------------------------------
008600140307     d dataoggi        s              8s 0
008700140310     d db2NumberRows   s             31p 0 inz
008800140310     d indicex         s              5i 0 inz
008900140310     d indicey         s              5i 0 inz
009000140317     d keyacr          s             10s 0 inz
009100140804     d keyacrend       s             10s 0 inz
009200140310     d ksunumerico     s              8s 0 inz
009300140403     d kscnumerico     s              7s 0 inz
009400160318     d mail            s             60a   inz
009500160318     d sms             s             16a   inz
009600140307     d wEnd            s              1n   inz(*off)
009700170418     d wEndGiri        s              1n   inz(*off)
009800140311     d wSQL            s           4096    Varying        inz
009900140306
010000140306       //--------------------------------------------------------------
010100140306       //?Definizione procedure.
010200140306       //--------------------------------------------------------------
010300140306
010400140306       //--------------------------------------------------------------
010500140306       //?Definizione prototipi.
010600140306       //--------------------------------------------------------------
010700140307      /copy gaitrasrc/srcprotopr,FIORB00R
010800140307      /copy gaitrasrc/srcprotopr,TIBS10R
010900140306
011000140306       //--------------------------------------------------------------
011100140306       //?Definizione parametri programma.
011200140306       //--------------------------------------------------------------
011300140307     d Fiorb00_Anagrafica...
011400140306     d                 PI
011500140306     d prmRqsOpCode...
011600140306     d                               10I 0 CONST
011700140306     d prmRpyOpCode...
011800140306     d                               10I 0
011900140306     d prmRpyIdMsg...
012000140306     d                               10I 0
012100140306     d prmRqsFormato...
012200140312     d                               10A   CONST OPTIONS(*NOPASS)
012300140306     d prmRqsData...
012400140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
012500140306     d prmRqsDataSize...
012600140312     d                               10I 0 CONST OPTIONS(*NOPASS)
012700140306     d prmRpyFormato...
012800140312     d                               10A   CONST OPTIONS(*NOPASS)
012900140306     d prmRpyData...
013000140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
013100140306     d prmRpyDataSize...
013200140312     d                               10I 0 CONST OPTIONS(*NOPASS)
013300140306
013400140306       //--------------------------------------------------------------
013500140306       //?MAIN.
013600140306       //--------------------------------------------------------------
013700140306
013800140306      /free
013900140306
014000140306       //?Operazioni iniziali
014100140306       exsr RoutInz;
014200140307
014300140307       //?Controllo formale dei dati passati
014400140307       exsr Controlla;
014500140307
014600140307       //?Elabora
014700140312       exsr Elabora;
014800060612
014900140306       //?Operazioni finali
015000140306       exsr RoutEnd;
015100140306
015200140306       //--------------------------------------------------------------
015300140306       //?Operazioni iniziali.                                         ?
015400140306       //--------------------------------------------------------------
015500140306       BEGSR  RoutInz;
015600140306
015700140307         prmRpyOpCode = FIORB00_RPYOPCODE_DONE;
015800140307         prmRpyIdMsg  = FIORB00_ESITO_OK;
015900140307
016000140311         wEnd = *off;
016100140307
016200140307       //?Data del giorno
016300140307         dataoggi = %dec(%date());
016400140306
016500140306       ENDSR;
016600140306
016700140306       //--------------------------------------------------------------
016800140306       //?Controllo formale dei dati passati.
016900140306       //--------------------------------------------------------------
017000140306       BEGSR  Controlla;
017100140307
017200140307       //?OpCode
017300140307         IF  prmRqsOpCode <> FIORB00_RQSOPCODE_LEGGI  and
017400140312             prmRqsOpCode <> FIORB00_RQSOPCODE_CARICA and
017500140401             prmRqsOpCode <> FIORB00_RQSOPCODE_GETDATI and
017600170418             prmRqsOpCode <> FIORB00_RQSOPCODE_CLOSE and
017700170418             prmRqsOpCode <> FIORB00_RQSOPCODE_GIRI;
017800140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
017900140307           prmRpyIdMsg  = FIORB00_ESITO_RQSOPCODE_SCONOSCIUTO;
018000140307           exsr RoutEnd;
018100140307         ENDIF;
018200140312
018300140312       //?Se OpCode CLOSE salto i prossimi controlli
018400140312         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
018500140312           leavesr;
018600140312         ENDIF;
018700140401
018800140401       //?Se OpCode GETDATI controllo le DS interessate
018900140401         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
019000140401         //?Formato e size RQS
019100140401           IF  prmRqsFormato = fiorb0I1.formato;
019200140401             fiorb0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
019300140401           ELSE;
019400140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
019500140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
019600140401             exsr RoutEnd;
019700140401           ENDIF;
019800140401           IF  prmRqsDataSize > 0;
019900140401           ELSE;
020000140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
020100140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
020200140401             exsr RoutEnd;
020300140401           ENDIF;
020400140401         //?Formato e size RPY
020500140401           IF  prmRpyFormato = fiorb0O1.formato;
020600140401           ELSE;
020700140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
020800140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
020900140401             exsr RoutEnd;
021000140401           ENDIF;
021100140401           IF  prmRpyDataSize > 0;
021200140401           ELSE;
021300140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
021400140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
021500140401             exsr RoutEnd;
021600140401           ENDIF;
021700140401           leavesr;
021800140401         ENDIF;
021900170418
022000170418       //?Se OpCode GIRI controllo le DS interessate
022100170418         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GIRI;
022200170418         //?Formato e size RQS
022300170418           IF  prmRqsFormato = fiorb0I2.formato;
022400170418             fiorb0I2 = %SUBST(prmRqsData:1:prmRqsDataSize);
022500170418           ELSE;
022600170418             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
022700170418             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
022800170418             exsr RoutEnd;
022900170418           ENDIF;
023000170418           IF  prmRqsDataSize > 0;
023100170418           ELSE;
023200170418             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
023300170418             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
023400170418             exsr RoutEnd;
023500170418           ENDIF;
023600170418         //?Formato e size RPY
023700170418           IF  prmRpyFormato = fiorb0O2.formato;
023800170418           ELSE;
023900170418             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
024000170418             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
024100170418             exsr RoutEnd;
024200170418           ENDIF;
024300170418           IF  prmRpyDataSize > 0;
024400170418           ELSE;
024500170418             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
024600170418             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
024700170418             exsr RoutEnd;
024800170418           ENDIF;
024900170418           leavesr;
025000170418         ENDIF;
025100140307
025200140401       //?Altri OpCode diversi dai precedenti
025300140307       //?Formato e size RQS
025400140307         IF  prmRqsFormato = fiorb0I0.formato;
025500140307           fiorb0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
025600140307         ELSE;
025700140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
025800140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
025900140307           exsr RoutEnd;
026000140307         ENDIF;
026100140307         IF  prmRqsDataSize > 0;
026200140307         ELSE;
026300140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
026400140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
026500140307           exsr RoutEnd;
026600140307         ENDIF;
026700140307       //?Formato e size RPY
026800140307         IF  prmRpyFormato = fiorb0O0.formato;
026900140307         ELSE;
027000140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
027100140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
027200140307           exsr RoutEnd;
027300140307         ENDIF;
027400140307         IF  prmRpyDataSize > 0;
027500140307         ELSE;
027600140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
027700140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
027800140307           exsr RoutEnd;
027900140307         ENDIF;
028000140306
028100140306       ENDSR;
028200140307
028300140307       //--------------------------------------------------------------
028400140307       //?Elabora.
028500140307       //--------------------------------------------------------------
028600140307       BEGSR  Elabora;
028700140312
028800140312         SELECT;
028900140312
029000140312       //?Richiesta elaborazione CLOSE
029100140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
029200140312         //?Forzo la chiusura del cursore
029300140312             exec sql CLOSE ACR;
029400140312             wEnd = *on;
029500140312             exsr routend;
029600140307
029700140312       //?Richiesta elaborazione LEGGI
029800140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_LEGGI;
029900140312             IF  fiorb0I0.formato <> prmRqsFormato;
030000140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
030100140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
030200140312               exsr RoutEnd;
030300140312             ENDIF;
030400140312           //?Pulisco ds di output
030500140312             reset FIORB0O0;
030600140312           //?preparo i dati e torno il n. totale dei clienti da caricare
030700140312             exsr CaricaKun;
030800140312             exsr LeggeAcr;
030900140312           //?torno anche i primi 10 clienti
031000140312             exsr CaricaAcr;
031100140307
031200140312       //?Richiesta elaborazione CARICA
031300140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CARICA;
031400140312             IF  fiorb0I0.formato <> prmRqsFormato;
031500140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
031600140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
031700140312               exsr RoutEnd;
031800140312             ENDIF;
031900140312           //?Torno i dati delle anagrafiche
032000140312             exsr CaricaAcr;
032100140401
032200140401       //?Richiesta elaborazione GETDATI
032300140401           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
032400140401             IF  fiorb0I1.formato <> prmRqsFormato;
032500140401               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
032600140401               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
032700140401               exsr RoutEnd;
032800140401             ENDIF;
032900140401           //?Pulisco ds di output
033000140401             reset FIORB0O1;
033100140401           //?Torno i dati dell' anagrafica
033200140401             exsr RecuperaAcr;
033300170418
033400170418       //?Richiesta elaborazione GIRI
033500170418           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_GIRI;
033600170418             IF  fiorb0I2.formato <> prmRqsFormato;
033700170418               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
033800170418               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
033900170418               exsr RoutEnd;
034000170418             ENDIF;
034100170418           //?Pulisco ds di output
034200170418             reset FIORB0O2;
034300170418           //?Torno i dati dei giri presenti in anagrafica per la
034400170418           //?filiale ritiro richiesta
034500170418             exsr RecuperaGiri;
034600140401
034700140312         ENDSL;
034800140307
034900140307       ENDSR;
035000140307
035100140307       //--------------------------------------------------------------
035200140307       //?Carica codici legati.
035300140307       //--------------------------------------------------------------
035400140307       BEGSR  CaricaKun;
035500140307
035600140404         ksunumerico = %dec(fiorb0I0.codiceksu:8:0);
035700140307         VSSisv = 'TT';
035800140307       //?Reperisce il legame clienti figli (se c'è) sul file delle abilitazioni VAS
035900140307         setll (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
036000140307         reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
036100140307         DOW not %eof(TIVSS02L);
036200140307         //?Servizio attivo
036300140307           IF  VSSvat = *blanks;
036400140307           //?NON scaduto
036500140307             IF  dataoggi >= VSSdde and dataoggi <= VSSdsc;
036600140307             //?controllo se per il cliente KSU ci sono figli da elaborare
036700140307               IF  VSStle <> *blanks;
036800140307                 clear TIBS10DS;
036900140310                 tibs10ds.D10drf = dataoggi;
037000140310                 tibs10ds.D10tle = VSStle;
037100140310                 tibs10ds.D10paf = 'F';     // cerco i figli
037200140307                 tibs10ds.D10cod = ksunumerico;
037300140307                 tibs10r (TIBS10DS);
037400140307               //?Se errore non ci sono figli memorizzo in sk il codice passato
037500140307                 IF  tibs10ds.D10err <> *blanks;
037600140307                   skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
037700140307                 ELSE;
037800140307               //?Se no errore memorizzo la sk dei figli
037900140307                   indicex = 1;
038000140307                   FOR  indicex by 1 to %elem(tibs10ds.skksc11);
038100140310                     skksc(indicex) =
038200140311                     tibs10ds.skksc11(indicex);
038300140307                   ENDFOR;
038400140307                 ENDIF;
038500140307               ELSE;
038600140307             //?se per il cliente KSU NON ci sono legami
038700140307             //?memorizzo in sk il codice passato
038800140307                 skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
038900140307               ENDIF;
039000140307           //?trovato cliente valido non scaduto esco dalla lettura
039100140307               leave;
039200140307             ENDIF;
039300140307           ENDIF;
039400140307           reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
039500140307         ENDDO;
039600140317
039700140317       //?Recupero il flag di contatto preventivo
039800140709         clear ACRfcc;
039900140403         kscnumerico = %dec(%subst(%editc(ksunumerico:'X'):2:7):7:0);
040000140403         keyacr = kscnumerico * 1000;
040100140804         keyacrend = keyacr + 999;
040200140804         exec sql
040300140804           select ACRfcc into :ACRfcc
040400140804           from FNACR00F
040500140804           where ACRcro between :keyacr and :keyacrend
040600140804           order by ACRcro
040700140804           fetch first 1 row only;
040800140804           IF  sqlcode = 100;
040900140804             clear ACRfcc;
041000140804           ENDIF;
041100151007
041200151007       //?Dalla versione 'B'
041300151007       //?aggiungo anche il flag di invio mail x accettazione ORM
041400151007       //?e il codice ordinante per recuperare la mail sul file estensione
041500151007         IF  fiorb0O0.versione > 'A';
041600151007           clear ACRcro;
041700151007           clear ACRmai;
041800151007           exec sql
041900151007           select ACRcro, ACRmai
042000151007           into :ACRcro, :ACRmai
042100151007           from FNACR00F
042200151007           where ACRcro between :keyacr and :keyacrend
042300151007           order by ACRcro
042400151007           fetch first 1 row only;
042500151007           IF  sqlcode = 100;
042600151007             clear ACRcro;
042700151007             clear ACRmai;
042800151007           ENDIF;
042900151007           dACR01 = ACRmai;
043000151007           chain (ACRcro:MailAccettazione) FNACRE1L;
043100151007           IF  not %found(FNACRE1L);
043200151007             clear ACREdati;
043300151007           ENDIF;
043400160318         //?Dalla versione 'C'
043500160322           IF  fiorb0O0.versione > 'B';
043600160318             mail = ACREdati;
043700160318           //?aggiungo il recupero del SMS x accettazione ORM
043800160318             chain (ACRcro:SmsAccettazione) FNACRE1L;
043900160318             IF  not %found(FNACRE1L);
044000160318               clear ACREdati;
044100160318             ENDIF;
044200160318             sms = ACREdati;
044300160318           ENDIF;
044400151007         ENDIF;
044500141222
044600141222       //?Se la sk dei codici da caricare da FNACR è vuota esco dal pgm
044700141222       //?e torno errore
044800141222         IF  skksc(1) = 0;
044900141222           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
045000141222           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
045100141222           wEnd = *on;
045200141222           exsr RoutEnd;
045300141222         ENDIF;
045400140403
045500140403         IF ACRfcc = *blanks;
045600140403           fiorb0O0.contatto = 'S';
045700140403         ELSE;
045800140403           fiorb0O0.contatto = ACRfcc;
045900140403         ENDIF;
046000151007
046100160322       //?Fino alla versione 'B'
046200151007       //?ritrono anche  il flag di invio mail x accettazione ORM
046300151007       //?e la relativa mail
046400160322         IF  fiorb0O0.versione < 'C';
046500151007           fiorb0O0.alertconf = dACR01.§ACRfmar;
046600151007           fiorb0O0.mailconf = ACREdati;
046700151007         ENDIF;
046800160318
046900160318       //?Dalla versione 'C'
047000160318       //?ritrono anche sms x accettazione ORM
047100160318         IF  fiorb0O0.versione > 'B';
047200160322           fiorb0O0.alertconf = dACR01.§ACRfmar;
047300160318           fiorb0O0.mailconf = mail;
047400160318           fiorb0O0.smsconf  = sms;
047500160318         ENDIF;
047600140307
047700140307       ENDSR;
047800140307
047900140307       //--------------------------------------------------------------
048000140307       //?Legge le anagrafiche.
048100140307       //--------------------------------------------------------------
048200140310       BEGSR  LeggeAcr;
048300140307
048400140307       //?Preparo la stringa
048500140307         exsr Stringa;
048600140307
048700140307       //?Dichiarazione cursore
048800140307         exec sql
048900140307           prepare S1   from :wSQL;
049000140307         exec sql
049100140730           declare ACR insensitive no scroll cursor for S1;
049200140307
049300140307       //?Apertura del cursore
049400140307         exec sql
049500140307           open ACR;
049600140307
049700140307       //?Errore in SQL esco
049800140307         IF sqlcode < 0;
049900140307           exec sql CLOSE ACR;
050000141222           DUMP(A);
050100140311           wEnd = *on;
050200140307           exsr routend;
050300140307           leavesr;
050400140307         ENDIF;
050500140307
050600140307       //?Recupero il numero totali di codici che dovrò leggere
050700140307         exec sql
050800140310          GET DIAGNOSTICS :fiorb0O0.clientitot = DB2_NUMBER_ROWS;
050900140307
051000140310         IF fiorb0O0.clientitot = *zeros;
051100140307           exec sql CLOSE ACR;
051200140311           wEnd = *on;
051300140307           exsr routend;
051400140307         ENDIF;
051500140307
051600140307       ENDSR;
051700140307
051800140307       //--------------------------------------------------------------
051900140307       //?Carica le anagrafiche.
052000140307       //--------------------------------------------------------------
052100140307       BEGSR  CaricaAcr;
052200140307
052300140307       //?Leggo il file delle anagrafiche
052400140307           exec sql
052500140307             fetch next from ACR for 10 ROWS into :anagrafica;
052600140307
052700140307           IF sqlcod = 100 or sqlcod < 0;
052800140311             exec sql CLOSE ACR;
052900140311             wEnd = *on;
053000140311             exsr routend;
053100140311             leavesr;
053200140307           ENDIF;
053300140307
053400140307           fiorb0O0.nrrec = sqlErrD(3);    // Number of rows retrieved.
053500140307
053600140307           FOR indicex = 1 TO fiorb0O0.nrrec;
053700140307             %occur(anagrafica) = indicex;
053800140310             fiorb0O0.skCodice(indicex) = %editc(anagrafica.cro:'X');
053900140310             fiorb0O0.skNome(indicex) = anagrafica.rsr;
054000140310             fiorb0O0.skIndirizzo(indicex) = anagrafica.inr;
054100140310             fiorb0O0.skCap(indicex) = anagrafica.car;
054200140310             fiorb0O0.skLocalita(indicex) = anagrafica.lor;
054300140310             fiorb0O0.skProvincia(indicex) = anagrafica.prr;
054400140310             fiorb0O0.skNazione(indicex) = anagrafica.nar;
054500140307           ENDFOR;
054600140307
054700140307           IF sqlErrD(5) = 100; // Contains +100 if the last row was fetched.
054800140313             exec sql CLOSE ACR;
054900140313             wEnd = *on;
055000140307           ENDIF;
055100140307
055200140307       ENDSR;
055300140307
055400140307       //--------------------------------------------------------------
055500140307       //?Preparo la stringa SQL.
055600140307       //--------------------------------------------------------------
055700140307       BEGSR  Stringa;
055800140307
055900140307       //?Carico stringa SQL per elaborare le anagrafiche
056000140307         wSQL = 'select ACRcro, ACRrsr, ACRinr, +
056100140404                 ACRcar, ACRlor, ACRprr, ACRnar  +
056200140307                 from FNACR00F +
056300140407                 where substr(digits(ACRcro), 1, 7) in(';
056400140307         indicey = 0;
056500140307         indicex = 1;
056600140307         FOR indicex by 1 to %elem(skksc);
056700140307           IF skksc(indicex) <> *zeros;
056800140307             IF indicey > 0;
056900140307               wSQL += ', ';
057000140307             ELSE;
057100140307               indicey = 1;
057200140307             ENDIF;
057300140404             wSQL += %editc(skksc(indicex):'X');
057400140307           ENDIF;
057500140307         ENDFOR;
057600140307
057700140307         wSQL += ')';
057800140307
057900140307       //?Richiesta esclusione sulla frequenza ritiro
058000140307         IF  fiorb0I0.escluditcr <> *blanks;
058100140307           SELECT;
058200140307         //?MAI
058300140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
058400140314               wSQL += ' and ACRtcr <> ' + '''' +
058500140314                         FIORB00_ESCLUDI_MAI + '''';
058600140307         //?Internet
058700140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_INTERNET;
058800140314               wSQL += ' and ACRtcr <> ' + '''' +
058900140314                         FIORB00_ESCLUDI_INTERNET + '''';
059000140307         //?Fisso
059100140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_FISSO;
059200140314               wSQL += ' and ACRtcr <> ' + '''' +
059300140314                         FIORB00_ESCLUDI_FISSO + '''';
059400140307         //?Ricorrente
059500140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_RICORRENTE;
059600140314               wSQL += ' and ACRtcr <> ' + '''' +
059700140314                         FIORB00_ESCLUDI_RICORRENTE + '''';
059800140307         //?Sia Mai che Internet
059900140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
060000140314               wSQL += ' and ACRtcr <> ' + '''' +
060100140314                         FIORB00_ESCLUDI_MAI + '''' +
060200140314                       ' and ACRtcr <> ' + '''' +
060300140314                         FIORB00_ESCLUDI_INTERNET + '''';
060400140307           ENDSL;
060500140307         ENDIF;
060600140307
060700140307       //?Richiesto ordinamento
060800140307         IF  fiorb0I0.ordina <> *blanks;
060900140307           SELECT;
061000140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_LOC;
061100140307               wSQL += ' ORDER BY ACRlor, ACRinr, ACRrsr, ACRcro';
061200140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_COD;
061300140307               wSQL += ' ORDER BY ACRcro, ACRrsr';
061400140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_RAG;
061500140307               wSQL += ' ORDER BY ACRrsr, ACRcro';
061600140307           ENDSL;
061700140307         ENDIF;
061800140307
061900140307       ENDSR;
062000140401
062100140401       //--------------------------------------------------------------
062200140401       //?Recupero i dati dell'anagrafica.
062300140401       //--------------------------------------------------------------
062400140401       BEGSR  RecuperaAcr;
062500140401
062600140401       //?Recupero i dati dall'anagrafica clienti ritiro
062700140401         chain fiorb0I1.codice FNACR01L;
062800140401         IF  not %found(FNACR01L);
062900140519           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
063000140401           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
063100140401           wEnd = *on;
063200140401           exsr RoutEnd;
063300140401         ENDIF;
063400140401
063500140401         dACR01 = ACRmai;
063600140401
063700140401       //?imposto i dati del mittente nei campi di output
063800140401         fiorb0O1.nome = ACRrsr;
063900140401         fiorb0O1.indirizzo = ACRinr;
064000140401         fiorb0O1.cap = ACRcar;
064100140401         fiorb0O1.localita = ACRlor;
064200140401         fiorb0O1.provincia = ACRprr;
064300140401         fiorb0O1.nazione = ACRnar;
064400140401         fiorb0O1.referente = ACRper;
064500140401         fiorb0O1.telefono = ACRtel;
064600140401         fiorb0O1.natmerce = ACRntm;
064700140401         fiorb0O1.codksc = ACRksc;
064800140401         fiorb0O1.codtariffa = ACRccc;
064900140411         fiorb0O1.oraritiro = ACRorr;
065000140401         fiorb0O1.nota1  = ACRno1;
065100140401         fiorb0O1.nota2  = ACRno2;
065200140401         IF  %check(c_Digits:%subst(ACRmai:1:
065300140401             %len(dACR01.§ACRoa1))) = *zeros;
065400140401           fiorb0O1.dalleam = %int(dACR01.§ACRoa1);
065500140401         ENDIF;
065600140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+1:
065700140401             %len(dACR01.§ACRoa2))) = *zeros;
065800140401           fiorb0O1.alleam = %int(dACR01.§ACRoa2);
065900140401         ENDIF;
066000140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
066100140401             %len(dACR01.§ACRoa2)+1:
066200140401             %len(dACR01.§ACRoa3))) = *zeros;
066300140401           fiorb0O1.dallepm = %int(dACR01.§ACRoa3);
066400140401         ENDIF;
066500140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
066600140401             %len(dACR01.§ACRoa2)+
066700140401             %len(dACR01.§ACRoa3)+1:
066800140401             %len(dACR01.§ACRoa4))) = *zeros;
066900140401           fiorb0O1.allepm = %int(dACR01.§ACRoa4);
067000140401         ENDIF;
067100140401         fiorb0O1.filemi = ACRpoa;
067200140401         fiorb0O1.filrit = ACRpoa;
067300140401         fiorb0O1.flagfncl = ACRfcl;
067400140401         fiorb0O1.flagfpkg = ACRfpk;
067500140401         fiorb0O1.flagfvlm = ACRfmc;
067600140401         fiorb0O1.flagfbnc = ACRfbn;
067700140401         fiorb0O1.flagfblc = ACRfbl;
067800140401         fiorb0O1.flagfmtc = ACRfmt;
067900140401         fiorb0O1.flagfatt = ACRfat;
068000140401         fiorb0O1.sponda = ACRspi;
068100160315
068200160315       //?Campo della DS previsto solo da versione 'B' in poi
068300160315         IF  fiorb0O1.versione > 'A';
068400160315           fiorb0O1.frequenza = ACRtcr;
068500160315           fiorb0O1.ordinante = ACRcco;
068600160315           IF ACRfcc = *blanks;
068700160315             fiorb0O1.contatto = 'S';
068800160315           ELSE;
068900160315             fiorb0O1.contatto = ACRfcc;
069000160315           ENDIF;
069100160315         ENDIF;
069200160531
069300160531       //?Campi della DS previsti solo da versione 'C' in poi
069400160531         IF  fiorb0O1.versione > 'B';
069500170426           fiorb0O1.alertconf = dACR01.§ACRfmar;
069600160531           chain (ACRcro:MailAccettazione) FNACRE1L;
069700160531           IF  %found(FNACRE1L);
069800170426             fiorb0O1.mailconf = ACREdati;
069900160531           ENDIF;
070000160531           chain (ACRcro:SmsAccettazione) FNACRE1L;
070100160531           IF  %found(FNACRE1L);
070200170426             fiorb0O1.smsconf  = ACREdati;
070300160531           ENDIF;
070400160531         ENDIF;
070500140401
070600140401       ENDSR;
070700170418
070800170418       //--------------------------------------------------------------
070900170418       //?Recupero i giri presenti sull'anagrafica per fil.ritiro.
071000170418       //--------------------------------------------------------------
071100170418       BEGSR  RecuperaGiri;
071200170418
071300170418         wEndGiri = *off;
071400170418
071500170418       //?Recupero i giri
071600170418         chain (fiorb0I2.codice:fiorb0I2.filiale) FNACR11L;
071700170418         IF  not %found(FNACR11L);
071800170418           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
071900170418           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
072000170418           wEnd = *on;
072100170418           exsr RoutEnd;
072200170418         ENDIF;
072300170418
072400170418         DOW  not wEndGiri;
072500170418           IF  %eof(FNACR11L);
072600170418             wEndGiri = *on;
072700170418             leave;
072800170418           ENDIF;
072900170418           IF  ACR1atb = *blanks;
073000170418             SELECT;
073100170418             WHEN  ACR1ain = FIORB00_AMBITO_STANDARD;
073200170418               fiorb0O2.girostd = ACR1cgi;
073300170418             WHEN  ACR1ain = FIORB00_AMBITO_SOTTO;
073400170418             fiorb0O2.girosotto = ACR1cgi;
073500170418             WHEN  ACR1ain = FIORB00_AMBITO_OLTRE;
073600170418               fiorb0O2.girooltre = ACR1cgi;
073700170418             ENDSL;
073800170418           ENDIF;
073900170418
074000170418           reade (fiorb0I2.codice:fiorb0I2.filiale) FNACR11L;
074100170418         ENDDO;
074200170418
074300170418       ENDSR;
074400140306
074500140306       //--------------------------------------------------------------
074600140306       //?Operazioni finali.
074700140306       //--------------------------------------------------------------
074800140306       BEGSR  RoutEnd;
074900140311
075000140311         IF  wEnd;
075100140311           *inLR = *on;
075200140311         ELSE;
075300140311           *inRT = *on;
075400140311         ENDIF;
075500140306
075600140313       //?Se OpCode CLOSE salto i prossimi controlli
075700140313         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
075800140313           leavesr;
075900140313         ENDIF;
076000140313
076100140402       //?Se OpCode GETDATI torno la FIORB0O1
076200140402         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
076300140402           %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O1;
076400141016           return;
076500140402         ENDIF;
076600170419
076700170419       //?Se OpCode GIRI torno la FIORB0O2
076800170419         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GIRI;
076900170419           %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O2;
077000170419           return;
077100170419         ENDIF;
077200140402
077300140307         %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O0;
077400140306
077500140306         return;
077600140306
077700140306       ENDSR;
077800140410      /end-free
