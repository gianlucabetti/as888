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
003300140306
003400140306       //--------------------------------------------------------------
003500140306       //?Definizione strutture dati.                                  ?
003600140306       //--------------------------------------------------------------
003700140306       // -?Dati INPUT
003800140307     d FIORB0I0      e ds                  QUALIFIED INZ(*EXTDFT)
003900140401     d FIORB0I1      e ds                  QUALIFIED INZ(*EXTDFT)
004000140306
004100140306       // -?Dati OUTPUT
004200140307     d FIORB0O0      e ds                  QUALIFIED INZ(*EXTDFT)
004300140307     d  skCodice                     10a   dim(10) overlay(Codice)
004400140307     d  skNome                       48a   dim(10) overlay(Nome)
004500140307     d  skIndirizzo                  35a   dim(10) overlay(Indirizzo)
004600140307     d  skCap                         9a   dim(10) overlay(Cap)
004700140307     d  skLocalita                   35a   dim(10) overlay(Localita)
004800140307     d  skProvincia                   2a   dim(10) overlay(Provincia)
004900140307     d  skNazione                     3a   dim(10) overlay(Nazione)
005000140401     d FIORB0O1      e ds                  QUALIFIED INZ(*EXTDFT)
005100140307
005200140307       // -?Legami clienti
005300140307     d TIBS10DS      e ds                  QUALIFIED INZ(*EXTDFT)
005400140311     d  skKsc11               21   5520  0 dim(500)
005500140401
005600140401       // -?Campo ACRMAI
005700140401     d dACR01        e ds                  QUALIFIED INZ
005800140307
005900140307       // -?Ds multipla per ritorno dati a chiamante
006000140307     d anagrafica      ds                  QUALIFIED OCCURS(10)
006100140310     d  cro                                like(ACRcro)
006200140310     d  rsr                                like(ACRrsr)
006300140310     d  inr                                like(ACRinr)
006400140310     d  car                                like(ACRcar)
006500140310     d  lor                                like(ACRlor)
006600140310     d  prr                                like(ACRprr)
006700140310     d  nar                                like(ACRnar)
006800140306
006900140306       //--------------------------------------------------------------
007000140306       //?Definizione schiere.
007100140306       //--------------------------------------------------------------
007200140307     d skKsc           s              7s 0 dim(500)
007300140306
007400140306       //--------------------------------------------------------------
007500140306       //?Definizione campi.
007600140306       //--------------------------------------------------------------
007700140307     d dataoggi        s              8s 0
007800140310     d db2NumberRows   s             31p 0 inz
007900140310     d indicex         s              5i 0 inz
008000140310     d indicey         s              5i 0 inz
008100140317     d keyacr          s             10s 0 inz
008200140804     d keyacrend       s             10s 0 inz
008300140310     d ksunumerico     s              8s 0 inz
008400140403     d kscnumerico     s              7s 0 inz
008500140307     d wEnd            s              1n   inz(*off)
008600140311     d wSQL            s           4096    Varying        inz
008700140306
008800140306       //--------------------------------------------------------------
008900140306       //?Definizione procedure.
009000140306       //--------------------------------------------------------------
009100140306
009200140306       //--------------------------------------------------------------
009300140306       //?Definizione prototipi.
009400140306       //--------------------------------------------------------------
009500140307      /copy gaitrasrc/srcprotopr,FIORB00R
009600140307      /copy gaitrasrc/srcprotopr,TIBS10R
009700140306
009800140306       //--------------------------------------------------------------
009900140306       //?Definizione parametri programma.
010000140306       //--------------------------------------------------------------
010100140307     d Fiorb00_Anagrafica...
010200140306     d                 PI
010300140306     d prmRqsOpCode...
010400140306     d                               10I 0 CONST
010500140306     d prmRpyOpCode...
010600140306     d                               10I 0
010700140306     d prmRpyIdMsg...
010800140306     d                               10I 0
010900140306     d prmRqsFormato...
011000140312     d                               10A   CONST OPTIONS(*NOPASS)
011100140306     d prmRqsData...
011200140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
011300140306     d prmRqsDataSize...
011400140312     d                               10I 0 CONST OPTIONS(*NOPASS)
011500140306     d prmRpyFormato...
011600140312     d                               10A   CONST OPTIONS(*NOPASS)
011700140306     d prmRpyData...
011800140312     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
011900140306     d prmRpyDataSize...
012000140312     d                               10I 0 CONST OPTIONS(*NOPASS)
012100140306
012200140306       //--------------------------------------------------------------
012300140306       //?MAIN.
012400140306       //--------------------------------------------------------------
012500140306
012600140306      /free
012700140306
012800140306       //?Operazioni iniziali
012900140306       exsr RoutInz;
013000140307
013100140307       //?Controllo formale dei dati passati
013200140307       exsr Controlla;
013300140307
013400140307       //?Elabora
013500140312       exsr Elabora;
013600060612
013700140306       //?Operazioni finali
013800140306       exsr RoutEnd;
013900140306
014000140306       //--------------------------------------------------------------
014100140306       //?Operazioni iniziali.                                         ?
014200140306       //--------------------------------------------------------------
014300140306       BEGSR  RoutInz;
014400140306
014500140311         //exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014600140306
014700140307         prmRpyOpCode = FIORB00_RPYOPCODE_DONE;
014800140307         prmRpyIdMsg  = FIORB00_ESITO_OK;
014900140307
015000140311         wEnd = *off;
015100140307
015200140307       //?Data del giorno
015300140307         dataoggi = %dec(%date());
015400140306
015500140306       ENDSR;
015600140306
015700140306       //--------------------------------------------------------------
015800140306       //?Controllo formale dei dati passati.
015900140306       //--------------------------------------------------------------
016000140306       BEGSR  Controlla;
016100140307
016200140307       //?OpCode
016300140307         IF  prmRqsOpCode <> FIORB00_RQSOPCODE_LEGGI  and
016400140312             prmRqsOpCode <> FIORB00_RQSOPCODE_CARICA and
016500140401             prmRqsOpCode <> FIORB00_RQSOPCODE_GETDATI and
016600140312             prmRqsOpCode <> FIORB00_RQSOPCODE_CLOSE;
016700140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
016800140307           prmRpyIdMsg  = FIORB00_ESITO_RQSOPCODE_SCONOSCIUTO;
016900140307           exsr RoutEnd;
017000140307         ENDIF;
017100140312
017200140312       //?Se OpCode CLOSE salto i prossimi controlli
017300140312         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
017400140312           leavesr;
017500140312         ENDIF;
017600140401
017700140401       //?Se OpCode GETDATI controllo le DS interessate
017800140401         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
017900140401         //?Formato e size RQS
018000140401           IF  prmRqsFormato = fiorb0I1.formato;
018100140401             fiorb0I1 = %SUBST(prmRqsData:1:prmRqsDataSize);
018200140401           ELSE;
018300140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
018400140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
018500140401             exsr RoutEnd;
018600140401           ENDIF;
018700140401           IF  prmRqsDataSize > 0;
018800140401           ELSE;
018900140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
019000140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
019100140401             exsr RoutEnd;
019200140401           ENDIF;
019300140401         //?Formato e size RPY
019400140401           IF  prmRpyFormato = fiorb0O1.formato;
019500140401           ELSE;
019600140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
019700140401             prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
019800140401             exsr RoutEnd;
019900140401           ENDIF;
020000140401           IF  prmRpyDataSize > 0;
020100140401           ELSE;
020200140401             prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
020300140401             prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
020400140401             exsr RoutEnd;
020500140401           ENDIF;
020600140401           leavesr;
020700140401         ENDIF;
020800140307
020900140401       //?Altri OpCode diversi dai precedenti
021000140307       //?Formato e size RQS
021100140307         IF  prmRqsFormato = fiorb0I0.formato;
021200140307           fiorb0I0 = %SUBST(prmRqsData:1:prmRqsDataSize);
021300140307         ELSE;
021400140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
021500140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
021600140307           exsr RoutEnd;
021700140307         ENDIF;
021800140307         IF  prmRqsDataSize > 0;
021900140307         ELSE;
022000140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
022100140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
022200140307           exsr RoutEnd;
022300140307         ENDIF;
022400140307       //?Formato e size RPY
022500140307         IF  prmRpyFormato = fiorb0O0.formato;
022600140307         ELSE;
022700140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
022800140307           prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
022900140307           exsr RoutEnd;
023000140307         ENDIF;
023100140307         IF  prmRpyDataSize > 0;
023200140307         ELSE;
023300140307           prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
023400140307           prmRpyIdMsg = FIORB00_ESITO_SIZE_DATA_ERRATO;
023500140307           exsr RoutEnd;
023600140307         ENDIF;
023700140306
023800140306       ENDSR;
023900140307
024000140307       //--------------------------------------------------------------
024100140307       //?Elabora.
024200140307       //--------------------------------------------------------------
024300140307       BEGSR  Elabora;
024400140312
024500140312         SELECT;
024600140312
024700140312       //?Richiesta elaborazione CLOSE
024800140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
024900140312         //?Forzo la chiusura del cursore
025000140312             exec sql CLOSE ACR;
025100140312             wEnd = *on;
025200140312             exsr routend;
025300140307
025400140312       //?Richiesta elaborazione LEGGI
025500140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_LEGGI;
025600140312             IF  fiorb0I0.formato <> prmRqsFormato;
025700140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
025800140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
025900140312               exsr RoutEnd;
026000140312             ENDIF;
026100140312           //?Pulisco ds di output
026200140312             reset FIORB0O0;
026300140312           //?preparo i dati e torno il n. totale dei clienti da caricare
026400140312             exsr CaricaKun;
026500140312             exsr LeggeAcr;
026600140312           //?torno anche i primi 10 clienti
026700140312             exsr CaricaAcr;
026800140307
026900140312       //?Richiesta elaborazione CARICA
027000140312           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_CARICA;
027100140312             IF  fiorb0I0.formato <> prmRqsFormato;
027200140312               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
027300140312               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
027400140312               exsr RoutEnd;
027500140312             ENDIF;
027600140312           //?Torno i dati delle anagrafiche
027700140312             exsr CaricaAcr;
027800140401
027900140401       //?Richiesta elaborazione GETDATI
028000140401           WHEN  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
028100140401             IF  fiorb0I1.formato <> prmRqsFormato;
028200140401               prmRpyOpCode = FIORB00_RPYOPCODE_ERROR;
028300140401               prmRpyIdMsg = FIORB00_ESITO_NOME_FORMATO_SCONOSCIUTO;
028400140401               exsr RoutEnd;
028500140401             ENDIF;
028600140401           //?Pulisco ds di output
028700140401             reset FIORB0O1;
028800140401           //?Torno i dati dell' anagrafica
028900140401             exsr RecuperaAcr;
029000140401
029100140312         ENDSL;
029200140307
029300140307       ENDSR;
029400140307
029500140307       //--------------------------------------------------------------
029600140307       //?Carica codici legati.
029700140307       //--------------------------------------------------------------
029800140307       BEGSR  CaricaKun;
029900140307
030000140404         ksunumerico = %dec(fiorb0I0.codiceksu:8:0);
030100140307         VSSisv = 'TT';
030200140307       //?Reperisce il legame clienti figli (se c'è) sul file delle abilitazioni VAS
030300140307         setll (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
030400140307         reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
030500140307         DOW not %eof(TIVSS02L);
030600140307         //?Servizio attivo
030700140307           IF  VSSvat = *blanks;
030800140307           //?NON scaduto
030900140307             IF  dataoggi >= VSSdde and dataoggi <= VSSdsc;
031000140307             //?controllo se per il cliente KSU ci sono figli da elaborare
031100140307               IF  VSStle <> *blanks;
031200140307                 clear TIBS10DS;
031300140310                 tibs10ds.D10drf = dataoggi;
031400140310                 tibs10ds.D10tle = VSStle;
031500140310                 tibs10ds.D10paf = 'F';     // cerco i figli
031600140307                 tibs10ds.D10cod = ksunumerico;
031700140307                 tibs10r (TIBS10DS);
031800140307               //?Se errore non ci sono figli memorizzo in sk il codice passato
031900140307                 IF  tibs10ds.D10err <> *blanks;
032000140307                   skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
032100140307                 ELSE;
032200140307               //?Se no errore memorizzo la sk dei figli
032300140307                   indicex = 1;
032400140307                   FOR  indicex by 1 to %elem(tibs10ds.skksc11);
032500140310                     skksc(indicex) =
032600140311                     tibs10ds.skksc11(indicex);
032700140307                   ENDFOR;
032800140307                 ENDIF;
032900140307               ELSE;
033000140307             //?se per il cliente KSU NON ci sono legami
033100140307             //?memorizzo in sk il codice passato
033200140307                 skksc(1) = %dec(%subst(fiorb0I0.codiceksu:2:7):7:0);
033300140307               ENDIF;
033400140307           //?trovato cliente valido non scaduto esco dalla lettura
033500140307               leave;
033600140307             ENDIF;
033700140307           ENDIF;
033800140307           reade (fiorb0I0.codiceksu:VSSisv) TIVSS02L;
033900140307         ENDDO;
034000140317
034100140317       //?Recupero il flag di contatto preventivo
034200140709         clear ACRfcc;
034300140403         kscnumerico = %dec(%subst(%editc(ksunumerico:'X'):2:7):7:0);
034400140403         keyacr = kscnumerico * 1000;
034500140804         keyacrend = keyacr + 999;
034600140804         exec sql
034700140804           select ACRfcc into :ACRfcc
034800140804           from FNACR00F
034900140804           where ACRcro between :keyacr and :keyacrend
035000140804           order by ACRcro
035100140804           fetch first 1 row only;
035200140804           IF  sqlcode = 100;
035300140804             clear ACRfcc;
035400140804           ENDIF;
035500151007
035600151007       //?Dalla versione 'B'
035700151007       //?aggiungo anche il flag di invio mail x accettazione ORM
035800151007       //?e il codice ordinante per recuperare la mail sul file estensione
035900151007         IF  fiorb0O0.versione > 'A';
036000151007           clear ACRcro;
036100151007           clear ACRmai;
036200151007           exec sql
036300151007           select ACRcro, ACRmai
036400151007           into :ACRcro, :ACRmai
036500151007           from FNACR00F
036600151007           where ACRcro between :keyacr and :keyacrend
036700151007           order by ACRcro
036800151007           fetch first 1 row only;
036900151007           IF  sqlcode = 100;
037000151007             clear ACRcro;
037100151007             clear ACRmai;
037200151007           ENDIF;
037300151007           dACR01 = ACRmai;
037400151007           chain (ACRcro:MailAccettazione) FNACRE1L;
037500151007           IF  not %found(FNACRE1L);
037600151007             clear ACREdati;
037700151007           ENDIF;
037800151007         ENDIF;
037900141222
038000141222       //?Se la sk dei codici da caricare da FNACR è vuota esco dal pgm
038100141222       //?e torno errore
038200141222         IF  skksc(1) = 0;
038300141222           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
038400141222           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
038500141222           wEnd = *on;
038600141222           exsr RoutEnd;
038700141222         ENDIF;
038800140403
038900140403         IF ACRfcc = *blanks;
039000140403           fiorb0O0.contatto = 'S';
039100140403         ELSE;
039200140403           fiorb0O0.contatto = ACRfcc;
039300140403         ENDIF;
039400151007
039500151007       //?Dalla versione 'B'
039600151007       //?ritrono anche  il flag di invio mail x accettazione ORM
039700151007       //?e la relativa mail
039800151007         IF  fiorb0O0.versione > 'A';
039900151007           fiorb0O0.alertconf = dACR01.§ACRfmar;
040000151007           fiorb0O0.mailconf = ACREdati;
040100151007         ENDIF;
040200140307
040300140307       ENDSR;
040400140307
040500140307       //--------------------------------------------------------------
040600140307       //?Legge le anagrafiche.
040700140307       //--------------------------------------------------------------
040800140310       BEGSR  LeggeAcr;
040900140307
041000140307       //?Preparo la stringa
041100140307         exsr Stringa;
041200140307
041300140307       //?Dichiarazione cursore
041400140307         exec sql
041500140307           prepare S1   from :wSQL;
041600140307         exec sql
041700140730           declare ACR insensitive no scroll cursor for S1;
041800140307
041900140307       //?Apertura del cursore
042000140307         exec sql
042100140307           open ACR;
042200140307
042300140307       //?Errore in SQL esco
042400140307         IF sqlcode < 0;
042500140307           exec sql CLOSE ACR;
042600141222           DUMP(A);
042700140311           wEnd = *on;
042800140307           exsr routend;
042900140307           leavesr;
043000140307         ENDIF;
043100140307
043200140307       //?Recupero il numero totali di codici che dovrò leggere
043300140307         exec sql
043400140310          GET DIAGNOSTICS :fiorb0O0.clientitot = DB2_NUMBER_ROWS;
043500140307
043600140310         IF fiorb0O0.clientitot = *zeros;
043700140307           exec sql CLOSE ACR;
043800140311           wEnd = *on;
043900140307           exsr routend;
044000140307         ENDIF;
044100140307
044200140307       ENDSR;
044300140307
044400140307       //--------------------------------------------------------------
044500140307       //?Carica le anagrafiche.
044600140307       //--------------------------------------------------------------
044700140307       BEGSR  CaricaAcr;
044800140307
044900140307       //?Leggo il file delle anagrafiche
045000140307           exec sql
045100140307             fetch next from ACR for 10 ROWS into :anagrafica;
045200140307
045300140307           IF sqlcod = 100 or sqlcod < 0;
045400140311             exec sql CLOSE ACR;
045500140311             wEnd = *on;
045600140311             exsr routend;
045700140311             leavesr;
045800140307           ENDIF;
045900140307
046000140307           fiorb0O0.nrrec = sqlErrD(3);    // Number of rows retrieved.
046100140307
046200140307           FOR indicex = 1 TO fiorb0O0.nrrec;
046300140307             %occur(anagrafica) = indicex;
046400140310             fiorb0O0.skCodice(indicex) = %editc(anagrafica.cro:'X');
046500140310             fiorb0O0.skNome(indicex) = anagrafica.rsr;
046600140310             fiorb0O0.skIndirizzo(indicex) = anagrafica.inr;
046700140310             fiorb0O0.skCap(indicex) = anagrafica.car;
046800140310             fiorb0O0.skLocalita(indicex) = anagrafica.lor;
046900140310             fiorb0O0.skProvincia(indicex) = anagrafica.prr;
047000140310             fiorb0O0.skNazione(indicex) = anagrafica.nar;
047100140307           ENDFOR;
047200140307
047300140307           IF sqlErrD(5) = 100; // Contains +100 if the last row was fetched.
047400140313             exec sql CLOSE ACR;
047500140313             wEnd = *on;
047600140307           ENDIF;
047700140307
047800140307       ENDSR;
047900140307
048000140307       //--------------------------------------------------------------
048100140307       //?Preparo la stringa SQL.
048200140307       //--------------------------------------------------------------
048300140307       BEGSR  Stringa;
048400140307
048500140307       //?Carico stringa SQL per elaborare le anagrafiche
048600140307         wSQL = 'select ACRcro, ACRrsr, ACRinr, +
048700140404                 ACRcar, ACRlor, ACRprr, ACRnar  +
048800140307                 from FNACR00F +
048900140407                 where substr(digits(ACRcro), 1, 7) in(';
049000140307         indicey = 0;
049100140307         indicex = 1;
049200140307         FOR indicex by 1 to %elem(skksc);
049300140307           IF skksc(indicex) <> *zeros;
049400140307             IF indicey > 0;
049500140307               wSQL += ', ';
049600140307             ELSE;
049700140307               indicey = 1;
049800140307             ENDIF;
049900140404             wSQL += %editc(skksc(indicex):'X');
050000140307           ENDIF;
050100140307         ENDFOR;
050200140307
050300140307         wSQL += ')';
050400140307
050500140307       //?Richiesta esclusione sulla frequenza ritiro
050600140307         IF  fiorb0I0.escluditcr <> *blanks;
050700140307           SELECT;
050800140307         //?MAI
050900140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAI;
051000140314               wSQL += ' and ACRtcr <> ' + '''' +
051100140314                         FIORB00_ESCLUDI_MAI + '''';
051200140307         //?Internet
051300140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_INTERNET;
051400140314               wSQL += ' and ACRtcr <> ' + '''' +
051500140314                         FIORB00_ESCLUDI_INTERNET + '''';
051600140307         //?Fisso
051700140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_FISSO;
051800140314               wSQL += ' and ACRtcr <> ' + '''' +
051900140314                         FIORB00_ESCLUDI_FISSO + '''';
052000140307         //?Ricorrente
052100140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_RICORRENTE;
052200140314               wSQL += ' and ACRtcr <> ' + '''' +
052300140314                         FIORB00_ESCLUDI_RICORRENTE + '''';
052400140307         //?Sia Mai che Internet
052500140311             WHEN  fiorb0I0.escluditcr = FIORB00_ESCLUDI_MAIINTERNET;
052600140314               wSQL += ' and ACRtcr <> ' + '''' +
052700140314                         FIORB00_ESCLUDI_MAI + '''' +
052800140314                       ' and ACRtcr <> ' + '''' +
052900140314                         FIORB00_ESCLUDI_INTERNET + '''';
053000140307           ENDSL;
053100140307         ENDIF;
053200140307
053300140307       //?Richiesto ordinamento
053400140307         IF  fiorb0I0.ordina <> *blanks;
053500140307           SELECT;
053600140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_LOC;
053700140307               wSQL += ' ORDER BY ACRlor, ACRinr, ACRrsr, ACRcro';
053800140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_COD;
053900140307               wSQL += ' ORDER BY ACRcro, ACRrsr';
054000140311             WHEN  fiorb0I0.ordina = FIORB00_ORDINA_RAG;
054100140307               wSQL += ' ORDER BY ACRrsr, ACRcro';
054200140307           ENDSL;
054300140307         ENDIF;
054400140307
054500140307       ENDSR;
054600140401
054700140401       //--------------------------------------------------------------
054800140401       //?Recupero i dati dell'anagrafica.
054900140401       //--------------------------------------------------------------
055000140401       BEGSR  RecuperaAcr;
055100140401
055200140401       //?Recupero i dati dall'anagrafica clienti ritiro
055300140401         chain fiorb0I1.codice FNACR01L;
055400140401         IF  not %found(FNACR01L);
055500140519           prmRpyOpCode = FIORB00_RPYOPCODE_NOTFOUND;
055600140401           prmRpyIdMsg = FIORB00_ESITO_GET_CODICE_ERRATO;
055700140401           wEnd = *on;
055800140401           exsr RoutEnd;
055900140401         ENDIF;
056000140401
056100140401         dACR01 = ACRmai;
056200140401
056300140401       //?imposto i dati del mittente nei campi di output
056400140401         fiorb0O1.nome = ACRrsr;
056500140401         fiorb0O1.indirizzo = ACRinr;
056600140401         fiorb0O1.cap = ACRcar;
056700140401         fiorb0O1.localita = ACRlor;
056800140401         fiorb0O1.provincia = ACRprr;
056900140401         fiorb0O1.nazione = ACRnar;
057000140401         fiorb0O1.referente = ACRper;
057100140401         fiorb0O1.telefono = ACRtel;
057200140401         fiorb0O1.natmerce = ACRntm;
057300140401         fiorb0O1.codksc = ACRksc;
057400140401         fiorb0O1.codtariffa = ACRccc;
057500140411         fiorb0O1.oraritiro = ACRorr;
057600140401         fiorb0O1.nota1  = ACRno1;
057700140401         fiorb0O1.nota2  = ACRno2;
057800140401         IF  %check(c_Digits:%subst(ACRmai:1:
057900140401             %len(dACR01.§ACRoa1))) = *zeros;
058000140401           fiorb0O1.dalleam = %int(dACR01.§ACRoa1);
058100140401         ENDIF;
058200140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+1:
058300140401             %len(dACR01.§ACRoa2))) = *zeros;
058400140401           fiorb0O1.alleam = %int(dACR01.§ACRoa2);
058500140401         ENDIF;
058600140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
058700140401             %len(dACR01.§ACRoa2)+1:
058800140401             %len(dACR01.§ACRoa3))) = *zeros;
058900140401           fiorb0O1.dallepm = %int(dACR01.§ACRoa3);
059000140401         ENDIF;
059100140401         IF  %check(c_Digits:%subst(ACRmai:%len(dACR01.§ACRoa1)+
059200140401             %len(dACR01.§ACRoa2)+
059300140401             %len(dACR01.§ACRoa3)+1:
059400140401             %len(dACR01.§ACRoa4))) = *zeros;
059500140401           fiorb0O1.allepm = %int(dACR01.§ACRoa4);
059600140401         ENDIF;
059700140401         fiorb0O1.filemi = ACRpoa;
059800140401         fiorb0O1.filrit = ACRpoa;
059900140401         fiorb0O1.flagfncl = ACRfcl;
060000140401         fiorb0O1.flagfpkg = ACRfpk;
060100140401         fiorb0O1.flagfvlm = ACRfmc;
060200140401         fiorb0O1.flagfbnc = ACRfbn;
060300140401         fiorb0O1.flagfblc = ACRfbl;
060400140401         fiorb0O1.flagfmtc = ACRfmt;
060500140401         fiorb0O1.flagfatt = ACRfat;
060600140401         fiorb0O1.sponda = ACRspi;
060700140401
060800140401       ENDSR;
060900140306
061000140306       //--------------------------------------------------------------
061100140306       //?Operazioni finali.
061200140306       //--------------------------------------------------------------
061300140306       BEGSR  RoutEnd;
061400140311
061500140311         IF  wEnd;
061600140311           *inLR = *on;
061700140311         ELSE;
061800140311           *inRT = *on;
061900140311         ENDIF;
062000140306
062100140313       //?Se OpCode CLOSE salto i prossimi controlli
062200140313         IF  prmRqsOpCode = FIORB00_RQSOPCODE_CLOSE;
062300140313           leavesr;
062400140313         ENDIF;
062500140313
062600140402       //?Se OpCode GETDATI torno la FIORB0O1
062700140402         IF  prmRqsOpCode = FIORB00_RQSOPCODE_GETDATI;
062800140402           %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O1;
062900141016           return;
063000140402         ENDIF;
063100140402
063200140307         %subst(prmRpyData:1:prmRpyDataSize) = fiorb0O0;
063300140306
063400140306         return;
063500140306
063600140306       ENDSR;
063700140410      /end-free
