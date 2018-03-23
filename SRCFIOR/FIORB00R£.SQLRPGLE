000100140306       //==============================================================
000200140306       //
000300140402       //?FIORB00R - Anagrafica Ritiri
000400140402       // Recupera Anagrafiche di tutti i clienti legati allo stesso KSU
000500140402       // Recupera i dati Anagrafici di UN solo codice
000600140306       //
000700140306       //==============================================================
000800140306
000900140306       //--------------------------------------------------------------
001000140306       //?Specifiche di controllo.                                     ?
001100140306       //--------------------------------------------------------------
001200140306     h DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS')
001300140306
001400140306       //--------------------------------------------------------------
001500140306       //?Dichiarazione file.                                          ?
001600140306       //--------------------------------------------------------------
001700140307       // -?Clienti abilitati a VAS
001800140307     fTIVSS02L  if   e           k disk
001900140317       // -?Anagrafica clienti
002000140317     fFNACR01L  if   e           k disk
002100151007       // -?Anagrafica clienti ritiro - estensione
002200151007     fFNACRE1L  if   e           k disk
002300140306
002400140306       //--------------------------------------------------------------
002500140306       //?Definizione costanti.                                        ?
002600140306       //--------------------------------------------------------------
002700140307      /copy gaitrasrc/srcconst,FIORB00R
002800140401
002900140401     d c_Digits        c                   const('0123456789')
003000151007
003100151007     d MailAccettazione...
003200151007     d                 c                   const('MC')
003300160318
003400160318     d SmsAccettazione...
003500160318     d                 c                   const('SC')
003600140306
003700140306       //--------------------------------------------------------------
003800140306       //?Definizione strutture dati.                                  ?
003900140306       //--------------------------------------------------------------
004000140306       // -?Dati INPUT
004100140307     d FIORB0I0      e ds                  QUALIFIED INZ(*EXTDFT)
004200140401     d FIORB0I1      e ds                  QUALIFIED INZ(*EXTDFT)
004300140306
004400140306       // -?Dati OUTPUT
004500140307     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
004600140307     d  skCodice                     10a   dim(10) overlay(Codice)
004700140307     d  skNome                       48a   dim(10) overlay(Nome)
004800140307     d  skIndirizzo                  35a   dim(10) overlay(Indirizzo)
004900140307     d  skCap                         9a   dim(10) overlay(Cap)
005000140307     d  skLocalita                   35a   dim(10) overlay(Localita)
005100140307     d  skProvincia                   2a   dim(10) overlay(Provincia)
005200140307     d  skNazione                     3a   dim(10) overlay(Nazione)
005300140401     d FIORB0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005400140307
005500140307       // -?Legami clienti
005600140307     d TIBS10DS      e ds                  QUALIFIED INZ(*EXTDFT)
005700140311     d  skKsc11               21   5520  0 dim(500)
005800140401
005900140401       // -?Campo ACRMAI
006000140401     d dACR01        e ds                  QUALIFIED INZ
006100140307
006200140307       // -?Ds multipla per ritorno dati a chiamante
006300140307     d anagrafica      ds                  QUALIFIED OCCURS(10)
006400140310     d  cro                                like(ACRcro)
006500140310     d  rsr                                like(ACRrsr)
006600140310     d  inr                                like(ACRinr)
006700140310     d  car                                like(ACRcar)
006800140310     d  lor                                like(ACRlor)
006900140310     d  prr                                like(ACRprr)
007000140310     d  nar                                like(ACRnar)
007100140306
007200140306       //--------------------------------------------------------------
007300140306       //?Definizione schiere.
007400140306       //--------------------------------------------------------------
007500140307     d skKsc           s              7s 0 dim(500)
007600140306
007700140306       //--------------------------------------------------------------
007800140306       //?Definizione campi.
007900140306       //--------------------------------------------------------------
008000140307     d dataoggi        s              8s 0
008100140310     d db2NumberRows   s             31p 0 inz
008200140310     d indicex         s              5i 0 inz
008300140310     d indicey         s              5i 0 inz
008400140317     d keyacr          s             10s 0 inz
008500140804     d keyacrend       s             10s 0 inz
008600140310     d ksunumerico     s              8s 0 inz
008700140403     d kscnumerico     s              7s 0 inz
008800160318     d mail            s             60a   inz
008900160318     d sms             s             16a   inz
009000140307     d wEnd            s              1n   inz(*off)
009100140311     d wSQL            s           4096    Varying        inz
009200140306
009300140306       //--------------------------------------------------------------
009400140306       //?Definizione procedure.
009500140306       //--------------------------------------------------------------
009600140306
009700140306       //--------------------------------------------------------------
009800140306       //?Definizione prototipi.
009900140306       //--------------------------------------------------------------
010000140307      /copy gaitrasrc/srcprotopr,FIORB00R
010100140307      /copy gaitrasrc/srcprotopr,TIBS10R
010200140306
010300140306       //--------------------------------------------------------------
010400140306       //?Definizione parametri programma.
010500140306       //--------------------------------------------------------------
010600140307     d Fiorb00_Anagrafica...
010700140306     d                 PI
010800140306     d prmRqsOpCode...
010900140306     d                               10I 0 CONST
011000140306     d prmRpyOpCode...
011100140306     d                               10I 0
011200140306     d prmRpyIdMsg...
011300140306     d                               10I 0
011400140306     d prmRqsFormato...
011500140312     d                               10A   CONST OPTIONS(*NOPASS)
011600140306     d prmRqsData...
011700140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
011800140306     d prmRqsDataSize...
011900140312     d                               10I 0 CONST OPTIONS(*NOPASS)
012000140306     d prmRpyFormato...
012100140312     d                               10A   CONST OPTIONS(*NOPASS)
012200140306     d prmRpyData...
012300140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
012400140306     d prmRpyDataSize...
012500140312     d                               10I 0 CONST OPTIONS(*NOPASS)
012600140306
012700140306       //--------------------------------------------------------------
012800140306       //?MAIN.
012900140306       //--------------------------------------------------------------
013000140306
013100140306      /free
013200140306
013300140306       //?Operazioni iniziali
013400140306       exsr RoutInz;
013500140307
013600140307       //?Controllo formale dei dati passati
013700140307       exsr Controlla;
013800140307
013900140307       //?Elabora
014000140312       exsr Elabora;
014100060612
014200140306       //?Operazioni finali
014300140306       exsr RoutEnd;
014400140306
014500140306       //--------------------------------------------------------------
014600140306       //?Operazioni iniziali.                                         ?
014700140306       //--------------------------------------------------------------
014800140306       BEGSR  RoutInz;
014900140306
015000140311         //exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
015100140306
015200140307         prmRpyOpCode = FIORB00_RPYOPCODE_DONE;
015300140307         prmRpyIdMsg  = FIORB00_ESITO_OK;
015400140307
015500140311         wEnd = *off;
015600140307
015700140307       //?Data del giorno
015800140307         dataoggi = %dec(%date());
015900140306
016000140306       ENDSR;
016100140306
016200140306       //--------------------------------------------------------------
016300140306       //?Controllo formale dei dati passati.
016400140306       //--------------------------------------------------------------
016500140306       BEGSR  Controlla;
016600140307
016700140307       //?OpCode
016800140307         IF  prmRqsOpCode <> FIORB00_RQSOPCODE_LEGGI  and
016900140312             prmRqsOpCode <> FIORB00_RQSOPCODE_CARICA and
017000140401             prmRqsOpCode <> FIORB00_RQSOPCODE_GETDATI and
017100140312             prmRqsOpCode <> FIORB00_RQSOPCODE_CLOSE;
017200140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
017300140307           prmRpyIdMsg  = FIORB00_ESITO_RQSOPCODE_SCONOSCIUTO;
017400140307           exsr RoutEnd;
017500140307         ENDIF;
017600140312
017700140312       //?Se OpCode CLOSE salto i prossimi controlli
017800140312         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
017900140312           leavesr;
018000140312         ENDIF;
018100140401
018200140401       //?Se OpCode GETDATI controllo le DS interessate
018300140401         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
018400140401         //?Formato e size RQS
018500140401           IF  prmRqsFormato = fiorb0I1.formato;
018600140401             fiorb0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
018700140401           ELSE;
018800140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
018900140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
019000140401             exsr RoutEnd;
019100140401           ENDIF;
019200140401           IF  prmRqsDataSize > 0;
019300140401           ELSE;
019400140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
019500140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
019600140401             exsr RoutEnd;
019700140401           ENDIF;
019800140401         //?Formato e size RPY
019900140401           IF  prmRpyFormato = fiorb0O1.formato;
020000140401           ELSE;
020100140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
020200140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
020300140401             exsr RoutEnd;
020400140401           ENDIF;
020500140401           IF  prmRpyDataSize > 0;
020600140401           ELSE;
020700140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
020800140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
020900140401             exsr RoutEnd;
021000140401           ENDIF;
021100140401           leavesr;
021200140401         ENDIF;
021300140307
021400140401       //?Altri OpCode diversi dai precedenti
021500140307       //?Formato e size RQS
021600140307         IF  prmRqsFormato = fiorb0I0.formato;
021700140307           fiorb0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
021800140307         ELSE;
021900140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
022000140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
022100140307           exsr RoutEnd;
022200140307         ENDIF;
022300140307         IF  prmRqsDataSize > 0;
022400140307         ELSE;
022500140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
022600140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
022700140307           exsr RoutEnd;
022800140307         ENDIF;
022900140307       //?Formato e size RPY
023000140307         IF  prmRpyFormato = fiorb0O0.formato;
023100140307         ELSE;
023200140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
023300140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
023400140307           exsr RoutEnd;
023500140307         ENDIF;
023600140307         IF  prmRpyDataSize > 0;
023700140307         ELSE;
023800140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
023900140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
024000140307           exsr RoutEnd;
024100140307         ENDIF;
024200140306
024300140306       ENDSR;
024400140307
024500140307       //--------------------------------------------------------------
024600140307       //?Elabora.
024700140307       //--------------------------------------------------------------
024800140307       BEGSR  Elabora;
024900140312
025000140312         SELECT;
025100140312
025200140312       //?Richiesta elaborazione CLOSE
025300140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
025400140312         //?Forzo la chiusura del cursore
025500140312             exec sql CLOSE ACR;
025600140312             wEnd = *on;
025700140312             exsr routend;
025800140307
025900140312       //?Richiesta elaborazione LEGGI
026000140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_LEGGI;
026100140312             IF  fiorb0I0.formato <> prmRqsFormato;
026200140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
026300140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
026400140312               exsr RoutEnd;
026500140312             ENDIF;
026600140312           //?Pulisco ds di output
026700140312             reset FIORB0O0;
026800140312           //?preparo i dati e torno il n. totale dei clienti da caricare
026900140312             exsr CaricaKun;
027000140312             exsr LeggeAcr;
027100140312           //?torno anche i primi 10 clienti
027200140312             exsr CaricaAcr;
027300140307
027400140312       //?Richiesta elaborazione CARICA
027500140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CARICA;
027600140312             IF  fiorb0I0.formato <> prmRqsFormato;
027700140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
027800140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
027900140312               exsr RoutEnd;
028000140312             ENDIF;
028100140312           //?Torno i dati delle anagrafiche
028200140312             exsr CaricaAcr;
028300140401
028400140401       //?Richiesta elaborazione GETDATI
028500140401           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
028600140401             IF  fiorb0I1.formato <> prmRqsFormato;
028700140401               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
028800140401               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
028900140401               exsr RoutEnd;
029000140401             ENDIF;
029100140401           //?Pulisco ds di output
029200140401             reset FIORB0O1;
029300140401           //?Torno i dati dell' anagrafica
029400140401             exsr RecuperaAcr;
029500140401
029600140312         ENDSL;
029700140307
029800140307       ENDSR;
029900140307
030000140307       //--------------------------------------------------------------
030100140307       //?Carica codici legati.
030200140307       //--------------------------------------------------------------
030300140307       BEGSR  CaricaKun;
030400140307
030500140404         ksunumerico = %dec(fiorb0I0.codiceksu:8:0);
030600140307         VSSisv = 'TT';
030700140307       //?Reperisce il legame clienti figli (se c'è) sul file delle abilitazioni VAS
030800140307         setll (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
030900140307         reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
031000140307         DOW not %eof(TIVSS02L);
031100140307         //?Servizio attivo
031200140307           IF  VSSvat = *blanks;
031300140307           //?NON scaduto
031400140307             IF  dataoggi >= VSSdde and dataoggi <= VSSdsc;
031500140307             //?controllo se per il cliente KSU ci sono figli da elaborare
031600140307               IF  VSStle <> *blanks;
031700140307                 clear TIBS10DS;
031800140310                 tibs10ds.D10drf = dataoggi;
031900140310                 tibs10ds.D10tle = VSStle;
032000140310                 tibs10ds.D10paf = 'F';     // cerco i figli
032100140307                 tibs10ds.D10cod = ksunumerico;
032200140307                 tibs10r (TIBS10DS);
032300140307               //?Se errore non ci sono figli memorizzo in sk il codice passato
032400140307                 IF  tibs10ds.D10err <> *blanks;
032500140307                   skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
032600140307                 ELSE;
032700140307               //?Se no errore memorizzo la sk dei figli
032800140307                   indicex = 1;
032900140307                   FOR  indicex by 1 to %elem(tibs10ds.skksc11);
033000140310                     skksc(indicex) =
033100140311                     tibs10ds.skksc11(indicex);
033200140307                   ENDFOR;
033300140307                 ENDIF;
033400140307               ELSE;
033500140307             //?se per il cliente KSU NON ci sono legami
033600140307             //?memorizzo in sk il codice passato
033700140307                 skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
033800140307               ENDIF;
033900140307           //?trovato cliente valido non scaduto esco dalla lettura
034000140307               leave;
034100140307             ENDIF;
034200140307           ENDIF;
034300140307           reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
034400140307         ENDDO;
034500140317
034600140317       //?Recupero il flag di contatto preventivo
034700140709         clear ACRfcc;
034800140403         kscnumerico = %dec(%subst(%editc(ksunumerico:'X'):2:7):7:0);
034900140403         keyacr = kscnumerico * 1000;
035000140804         keyacrend = keyacr + 999;
035100140804         exec sql
035200140804           select ACRfcc into :ACRfcc
035300140804           from FNACR00F
035400140804           where ACRcro between :keyacr and :keyacrend
035500140804           order by ACRcro
035600140804           fetch first 1 row only;
035700140804           IF  sqlcode = 100;
035800140804             clear ACRfcc;
035900140804           ENDIF;
036000151007
036100151007       //?Dalla versione 'B'
036200151007       //?aggiungo anche il flag di invio mail x accettazione ORM
036300151007       //?e il codice ordinante per recuperare la mail sul file estensione
036400151007         IF  fiorb0O0.versione > 'A';
036500151007           clear ACRcro;
036600151007           clear ACRmai;
036700151007           exec sql
036800151007           select ACRcro, ACRmai
036900151007           into :ACRcro, :ACRmai
037000151007           from FNACR00F
037100151007           where ACRcro between :keyacr and :keyacrend
037200151007           order by ACRcro
037300151007           fetch first 1 row only;
037400151007           IF  sqlcode = 100;
037500151007             clear ACRcro;
037600151007             clear ACRmai;
037700151007           ENDIF;
037800151007           dACR01 = ACRmai;
037900151007           chain (ACRcro:MailAccettazione) FNACRE1L;
038000151007           IF  not %found(FNACRE1L);
038100151007             clear ACREdati;
038200151007           ENDIF;
038300160318         //?Dalla versione 'C'
038400160322           IF  fiorb0O0.versione > 'B';
038500160318             mail = ACREdati;
038600160318           //?aggiungo il recupero del SMS x accettazione ORM
038700160318             chain (ACRcro:SmsAccettazione) FNACRE1L;
038800160318             IF  not %found(FNACRE1L);
038900160318               clear ACREdati;
039000160318             ENDIF;
039100160318             sms = ACREdati;
039200160318           ENDIF;
039300151007         ENDIF;
039400141222
039500141222       //?Se la sk dei codici da caricare da FNACR è vuota esco dal pgm
039600141222       //?e torno errore
039700141222         IF  skksc(1) = 0;
039800141222           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
039900141222           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
040000141222           wEnd = *on;
040100141222           exsr RoutEnd;
040200141222         ENDIF;
040300140403
040400140403         IF ACRfcc = *blanks;
040500140403           fiorb0O0.contatto = 'S';
040600140403         ELSE;
040700140403           fiorb0O0.contatto = ACRfcc;
040800140403         ENDIF;
040900151007
041000160322       //?Fino alla versione 'B'
041100151007       //?ritrono anche  il flag di invio mail x accettazione ORM
041200151007       //?e la relativa mail
041300160322         IF  fiorb0O0.versione < 'C';
041400151007           fiorb0O0.alertconf = dACR01.§ACRfmar;
041500151007           fiorb0O0.mailconf = ACREdati;
041600151007         ENDIF;
041700160318
041800160318       //?Dalla versione 'C'
041900160318       //?ritrono anche sms x accettazione ORM
042000160318         IF  fiorb0O0.versione > 'B';
042100160322           fiorb0O0.alertconf = dACR01.§ACRfmar;
042200160318           fiorb0O0.mailconf = mail;
042300160318           fiorb0O0.smsconf  = sms;
042400160318         ENDIF;
042500140307
042600140307       ENDSR;
042700140307
042800140307       //--------------------------------------------------------------
042900140307       //?Legge le anagrafiche.
043000140307       //--------------------------------------------------------------
043100140310       BEGSR  LeggeAcr;
043200140307
043300140307       //?Preparo la stringa
043400140307         exsr Stringa;
043500140307
043600140307       //?Dichiarazione cursore
043700140307         exec sql
043800140307           prepare S1   from :wSQL;
043900140307         exec sql
044000140730           declare ACR insensitive no scroll cursor for S1;
044100140307
044200140307       //?Apertura del cursore
044300140307         exec sql
044400140307           open ACR;
044500140307
044600140307       //?Errore in SQL esco
044700140307         IF sqlcode < 0;
044800140307           exec sql CLOSE ACR;
044900141222           DUMP(A);
045000140311           wEnd = *on;
045100140307           exsr routend;
045200140307           leavesr;
045300140307         ENDIF;
045400140307
045500140307       //?Recupero il numero totali di codici che dovrò leggere
045600140307         exec sql
045700140310          GET DIAGNOSTICS :fiorb0O0.clientitot = DB2_NUMBER_ROWS;
045800140307
045900140310         IF fiorb0O0.clientitot = *zeros;
046000140307           exec sql CLOSE ACR;
046100140311           wEnd = *on;
046200140307           exsr routend;
046300140307         ENDIF;
046400140307
046500140307       ENDSR;
046600140307
046700140307       //--------------------------------------------------------------
046800140307       //?Carica le anagrafiche.
046900140307       //--------------------------------------------------------------
047000140307       BEGSR  CaricaAcr;
047100140307
047200140307       //?Leggo il file delle anagrafiche
047300140307           exec sql
047400140307             fetch next from ACR for 10 ROWS into :anagrafica;
047500140307
047600140307           IF sqlcod = 100 or sqlcod < 0;
047700140311             exec sql CLOSE ACR;
047800140311             wEnd = *on;
047900140311             exsr routend;
048000140311             leavesr;
048100140307           ENDIF;
048200140307
048300140307           fiorb0O0.nrrec = sqlErrD(3);    // Number of rows retrieved.
048400140307
048500140307           FOR indicex = 1 TO fiorb0O0.nrrec;
048600140307             %occur(anagrafica) = indicex;
048700140310             fiorb0O0.skCodice(indicex) = %editc(anagrafica.cro:'X');
048800140310             fiorb0O0.skNome(indicex) = anagrafica.rsr;
048900140310             fiorb0O0.skIndirizzo(indicex) = anagrafica.inr;
049000140310             fiorb0O0.skCap(indicex) = anagrafica.car;
049100140310             fiorb0O0.skLocalita(indicex) = anagrafica.lor;
049200140310             fiorb0O0.skProvincia(indicex) = anagrafica.prr;
049300140310             fiorb0O0.skNazione(indicex) = anagrafica.nar;
049400140307           ENDFOR;
049500140307
049600140307           IF sqlErrD(5) = 100; // Contains +100 if the last row was fetched.
049700140313             exec sql CLOSE ACR;
049800140313             wEnd = *on;
049900140307           ENDIF;
050000140307
050100140307       ENDSR;
050200140307
050300140307       //--------------------------------------------------------------
050400140307       //?Preparo la stringa SQL.
050500140307       //--------------------------------------------------------------
050600140307       BEGSR  Stringa;
050700140307
050800140307       //?Carico stringa SQL per elaborare le anagrafiche
050900140307         wSQL = 'select ACRcro, ACRrsr, ACRinr, +
051000140404                 ACRcar, ACRlor, ACRprr, ACRnar  +
051100140307                 from FNACR00F +
051200140407                 where substr(digits(ACRcro), 1, 7) in(';
051300140307         indicey = 0;
051400140307         indicex = 1;
051500140307         FOR indicex by 1 to %elem(skksc);
051600140307           IF skksc(indicex) <> *zeros;
051700140307             IF indicey > 0;
051800140307               wSQL += ', ';
051900140307             ELSE;
052000140307               indicey = 1;
052100140307             ENDIF;
052200140404             wSQL += %editc(skksc(indicex):'X');
052300140307           ENDIF;
052400140307         ENDFOR;
052500140307
052600140307         wSQL += ')';
052700140307
052800140307       //?Richiesta esclusione sulla frequenza ritiro
052900140307         IF  fiorb0I0.escluditcr <> *blanks;
053000140307           SELECT;
053100140307         //?MAI
053200140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
053300140314               wSQL += ' and ACRtcr <> ' + '''' +
053400140314                         FIORB00_ESCLUDI_MAI + '''';
053500140307         //?Internet
053600140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_INTERNET;
053700140314               wSQL += ' and ACRtcr <> ' + '''' +
053800140314                         FIORB00_ESCLUDI_INTERNET + '''';
053900140307         //?Fisso
054000140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_FISSO;
054100140314               wSQL += ' and ACRtcr <> ' + '''' +
054200140314                         FIORB00_ESCLUDI_FISSO + '''';
054300140307         //?Ricorrente
054400140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_RICORRENTE;
054500140314               wSQL += ' and ACRtcr <> ' + '''' +
054600140314                         FIORB00_ESCLUDI_RICORRENTE + '''';
054700140307         //?Sia Mai che Internet
054800140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
054900140314               wSQL += ' and ACRtcr <> ' + '''' +
055000140314                         FIORB00_ESCLUDI_MAI + '''' +
055100140314                       ' and ACRtcr <> ' + '''' +
055200140314                         FIORB00_ESCLUDI_INTERNET + '''';
055300140307           ENDSL;
055400140307         ENDIF;
055500140307
055600140307       //?Richiesto ordinamento
055700140307         IF  fiorb0I0.ordina <> *blanks;
055800140307           SELECT;
055900140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_LOC;
056000140307               wSQL += ' ORDER BY ACRlor, ACRinr, ACRrsr, ACRcro';
056100140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_COD;
056200140307               wSQL += ' ORDER BY ACRcro, ACRrsr';
056300140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_RAG;
056400140307               wSQL += ' ORDER BY ACRrsr, ACRcro';
056500140307           ENDSL;
056600140307         ENDIF;
056700140307
056800140307       ENDSR;
056900140401
057000140401       //--------------------------------------------------------------
057100140401       //?Recupero i dati dell'anagrafica.
057200140401       //--------------------------------------------------------------
057300140401       BEGSR  RecuperaAcr;
057400140401
057500140401       //?Recupero i dati dall'anagrafica clienti ritiro
057600140401         chain fiorb0I1.codice FNACR01L;
057700140401         IF  not %found(FNACR01L);
057800140519           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
057900140401           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
058000140401           wEnd = *on;
058100140401           exsr RoutEnd;
058200140401         ENDIF;
058300140401
058400140401         dACR01 = ACRmai;
058500140401
058600140401       //?imposto i dati del mittente nei campi di output
058700140401         fiorb0O1.nome = ACRrsr;
058800140401         fiorb0O1.indirizzo = ACRinr;
058900140401         fiorb0O1.cap = ACRcar;
059000140401         fiorb0O1.localita = ACRlor;
059100140401         fiorb0O1.provincia = ACRprr;
059200140401         fiorb0O1.nazione = ACRnar;
059300140401         fiorb0O1.referente = ACRper;
059400140401         fiorb0O1.telefono = ACRtel;
059500140401         fiorb0O1.natmerce = ACRntm;
059600140401         fiorb0O1.codksc = ACRksc;
059700140401         fiorb0O1.codtariffa = ACRccc;
059800140411         fiorb0O1.oraritiro = ACRorr;
059900140401         fiorb0O1.nota1  = ACRno1;
060000140401         fiorb0O1.nota2  = ACRno2;
060100140401         IF  %check(c_Digits:%subst(ACRmai:1:
060200140401             %len(dACR01.§ACRoa1))) = *zeros;
060300140401           fiorb0O1.dalleam = %int(dACR01.§ACRoa1);
060400140401         ENDIF;
060500140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+1:
060600140401             %len(dACR01.§ACRoa2))) = *zeros;
060700140401           fiorb0O1.alleam = %int(dACR01.§ACRoa2);
060800140401         ENDIF;
060900140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
061000140401             %len(dACR01.§ACRoa2)+1:
061100140401             %len(dACR01.§ACRoa3))) = *zeros;
061200140401           fiorb0O1.dallepm = %int(dACR01.§ACRoa3);
061300140401         ENDIF;
061400140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
061500140401             %len(dACR01.§ACRoa2)+
061600140401             %len(dACR01.§ACRoa3)+1:
061700140401             %len(dACR01.§ACRoa4))) = *zeros;
061800140401           fiorb0O1.allepm = %int(dACR01.§ACRoa4);
061900140401         ENDIF;
062000140401         fiorb0O1.filemi = ACRpoa;
062100140401         fiorb0O1.filrit = ACRpoa;
062200140401         fiorb0O1.flagfncl = ACRfcl;
062300140401         fiorb0O1.flagfpkg = ACRfpk;
062400140401         fiorb0O1.flagfvlm = ACRfmc;
062500140401         fiorb0O1.flagfbnc = ACRfbn;
062600140401         fiorb0O1.flagfblc = ACRfbl;
062700140401         fiorb0O1.flagfmtc = ACRfmt;
062800140401         fiorb0O1.flagfatt = ACRfat;
062900140401         fiorb0O1.sponda = ACRspi;
063000160315
063100160315       //?Campo della DS previsto solo da versione 'B' in poi
063200160315         IF  fiorb0O1.versione > 'A';
063300160315           fiorb0O1.frequenza = ACRtcr;
063400160315           fiorb0O1.ordinante = ACRcco;
063500160315           IF ACRfcc = *blanks;
063600160315             fiorb0O1.contatto = 'S';
063700160315           ELSE;
063800160315             fiorb0O1.contatto = ACRfcc;
063900160315           ENDIF;
064000160315         ENDIF;
064100160531
064200160531       //?Campi della DS previsti solo da versione 'C' in poi
064300160531         IF  fiorb0O1.versione > 'B';
064400160531           fiorb0O0.alertconf = dACR01.§ACRfmar;
064500160531           chain (ACRcro:MailAccettazione) FNACRE1L;
064600160531           IF  %found(FNACRE1L);
064700160531             fiorb0O0.mailconf = ACREdati;
064800160531           ENDIF;
064900160531           chain (ACRcro:SmsAccettazione) FNACRE1L;
065000160531           IF  %found(FNACRE1L);
065100160531             fiorb0O0.smsconf  = ACREdati;
065200160531           ENDIF;
065300160531         ENDIF;
065400140401
065500140401       ENDSR;
065600140306
065700140306       //--------------------------------------------------------------
065800140306       //?Operazioni finali.
065900140306       //--------------------------------------------------------------
066000140306       BEGSR  RoutEnd;
066100140311
066200140311         IF  wEnd;
066300140311           *inLR = *on;
066400140311         ELSE;
066500140311           *inRT = *on;
066600140311         ENDIF;
066700140306
066800140313       //?Se OpCode CLOSE salto i prossimi controlli
066900140313         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
067000140313           leavesr;
067100140313         ENDIF;
067200140313
067300140402       //?Se OpCode GETDATI torno la FIORB0O1
067400140402         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
067500140402           %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O1;
067600141016           return;
067700140402         ENDIF;
067800140402
067900140307         %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O0;
068000140306
068100140306         return;
068200140306
068300140306       ENDSR;
068400140410      /end-free
