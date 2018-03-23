000100160421      //--------------------------------------------------------------
000200160421      //?FIORG01R - SCAMBIO DATI DA BARTOLINI A TELLUS
000300160421      //--------------------------------------------------------------
000400160421     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160421     h dftactgrp(*no) actgrp('BARTVAS')
000600160421
000700160421      //---------------------------------------------------------------
000800160421      //?Dichiarazione file.
000900160421      //---------------------------------------------------------------
001000160428      // - File Log Tellus
001100160506     fFILTO01L  uf a e           k disk
001200160428
001300160421      // - File Giri Ritiri
001400160427     fFNORG01L  uf   e           k disk
001500160421
001600160421      // - File Ritiri
001700160421     fFNORM13L  if   e           k disk
001800160427
001900160427      // - File scambio dati con Tellus
002000160428     fTIVGD00F  if a e             disk
002100160421
002200160421      //---------------------------------------------------------------
002300160421      //?Definizione costanti.
002400160421      //---------------------------------------------------------------
002500160421
002600160421      //---------------------------------------------------------------
002700160421      //?Definizione schiere.
002800160421      //---------------------------------------------------------------
002900160421
003000160421      //---------------------------------------------------------------
003100160421      //?Definizione aree dati.
003200160421      //---------------------------------------------------------------
003300160421      // - Dati utente
003400160421     d §AzUte        e ds                  extname(AZUTE00F)
003500160421     d                                     dtaara
003600160421     d §DatiUte      e ds                  extname(dDatiUte)
003700160421     d                                     dtaara
003800160421
003900160421      //---------------------------------------------------------------
004000160421      //?Definizione strutture dati.
004100160421      //---------------------------------------------------------------
004200160421      // - Parametri ricevuti
004300160421     d KPJBA         e ds
004400160421     d FIORG01DS     e ds
004500160421
004600160421      // - Ricerca/Controllo tabelle
004700160421     d TIBS02DS      e ds                  inz
004800160421
004900160421      // - Reperimento dati utente
005000160421     d TIBS34DS      e ds
005100160427
005200160427      // - Instradamento
005300160427     d TISI95DS      e ds
005400160427
005500160427      // - Scambio dati VAS - DOWNLOAD
005600160427     d TIS781DS      e ds
005700160421
005800160421       // - Caricamento Filiali in £6
005900160421     d TRUL06DS      e ds
006000160421     d  L6                     1     90    dim(30)
006100160427
006200160427       // - Stacca numero
006300160427     d TRUL33DS      e ds
006400160428
006500160506      // - Campo LTOflo
006600160506     d DLTOFLO       e ds
006700160421
006800160615      // - Tabella TPR - Personalizzazione Pesi
006900160615     d dTPR          e ds
007000160427
007100160427      // - Dati Output Tellus
007200160427     d DTLLOO        e ds
007300160427
007400160427      // - Campo VGDflo
007500160427     d DVGDFLO       e ds
007600160421
007700160421      // - Tabella VPO - Tellus
007800160421     d dVPOTELLUS    e ds
007900160427
008000160427      // - Campo §781flo
008100160427     d TIS781DFLO    e ds
008200160421
008300160421      //---------------------------------------------------------------
008400160421      //?Definizione variabili globali.
008500160421      //---------------------------------------------------------------
008600160421      // - Flags booleani
008700160421     d EscludiORM      s               n   inz(*off)
008800160421     d Fine            s               n   inz(*off)
008900160421     d wEnd            s               n   inz(*off)
009000160428     d wStaccaId       s               n   inz(*off)
009100160421
009200160421      // - Indici di schiera
009300160421     d xx              s              4s 0 inz
009400160421
009500160421      // - Campi x richiamo FIDG11R1
009600160421     d OUTkeyver       s              8a   inz
009700160421     d OUTverdes       s             30a   inz
009800160421     d OUTesito        s              1a   inz
009900160428
010000160428      // - Campi x richiamo TIS781C1
010100160428     d P781esito       s              1a   inz
010200160428     d P781prg         s             10a   inz
010300160421
010400160421      // - Campi di comodo
010500160427     d ContaORM        s              5s 0 inz
010600160428     d NrId            s              7s 0 inz
010700160421     d Oggi            s              8s 0 inz
010800160428     d wPor            s                   like(ORMpor) inz
010900160421
011000160421      //---------------------------------------------------------------
011100160421      //?Definizione Procedure usate.
011200160421      //---------------------------------------------------------------
011300160421       // - Controllo versione giri
011400160421     d FIDG11R1        pr                  extpgm('FIDG11R1')
011500160428     d  IORG01por                     3s 0
011600160428     d  IORG01data                    8s 0
011700160421     d  OUTkeyver                     8a
011800160421     d  OUTverdes                    30a
011900160421     d  OUTesito                      1a
012000160427
012100160427       // - Instradamentp
012200160427     d TISI95R         pr                  extpgm('TISI95R')
012300160427     d  tisi95ds                           likeds(tisi95ds)
012400160428
012500160428       // - Invio dati
012600160428     d TIS781C1        pr                  extpgm('TIS781C1')
012700160428     d  P781esito                     1a
012800160428     d  tis781ds                           likeds(tis781ds)
012900160428     d  P781prg                      10a
013000160421
013100160421       // - Caricamento Filiali in £6
013200160421     d TRUL06R         pr                  extpgm('TRUL06R')
013300160421     d  kpjba                              likeds(kpjba)
013400160427
013500160427       // - Stacca Numero
013600160427     d TRUL33R         pr                  extpgm('TRUL33R')
013700160427     d  kpjba                              likeds(kpjba)
013800160421
013900160421      //---------------------------------------------------------------
014000160421      //?Definizione Prototipi.
014100160421      //---------------------------------------------------------------
014200160421      /copy gaitrasrc/srcprotopr,TIBS02R
014300160421      /copy gaitrasrc/srcprotopr,TIBS34R
014400160421
014500160421      //---------------------------------------------------------------
014600160421      //?Definizione key-list.
014700160421      //---------------------------------------------------------------
014800160421
014900160421      //---------------------------------------------------------------
015000160421      //?M A I N - L I N E
015100160421      //---------------------------------------------------------------
015200160421
015300160421     c     *Entry        plist
015400160421     c                   parm                    kpjba
015500160421
015600160421      /free
015700160421
015800160421       //?Operazioni iniziali
015900160421       exsr RoutInz;
016000160421
016100160421       //?Elabora per £6
016200160421         xx = 1;
016300160421         FOR xx by 1 to %elem(L6);
016400160421           IF  L6(xx) = *zeros or L6(xx) = *blanks;
016500160421             leave;
016600160421           ENDIF;
016700160421
016800160421           exsr Elabora;
016900160421
017000160421         ENDFOR;
017100160427
017200160427       //?Alla fine di tutte le letture
017300160427       //?se giro di controllo
017400160427       //?verifico se ho elaborato più o meno bolle
017500160427       //?rispetto al minimo richiesto
017600160427       IF  IORG01ctrl <> *blanks and OORG01nrit < IORG01nrit;
017700160427         OORG01min = 'S';
017800160427       ENDIF;
017900160427
018000160427       //?se non è giro di controllo
018100160427       //?mando i dati a tellus
018200160427       IF  IORG01ctrl = *blanks;
018300160427         exsr InviaTellus;
018400160427       ENDIF;
018500160421
018600160421       //?Operazioni finali
018700160421       exsr RoutEnd;
018800160421
018900160421       //--------------------------------------------------------------
019000160421       //?Operazioni iniziali.
019100160421       //--------------------------------------------------------------
019200160421       BEGSR RoutInz;
019300160421
019400160421       //?Reperimento dati job
019500160421         exsr DatiJob;
019600160421
019700160421       //?Imposto oggi
019800160421         Oggi = %dec(%date());
019900160421
020000160421       //?Imposto DS FIORG01DS
020100160421         FIORG01DS = kpjbu;
020200160421         clear OORG01min;
020300160421         clear OORG01nrit;
020400160524
020500160524       //?Se dal chiamante non ricevo la data imposto la data del giorno
020600160524         IF  IORG01data = 0;
020700160524           IORG01data = Oggi;
020800160524         ENDIF;
020900160421
021000160428       //?Aggancio tabella VPO 'TELLUSORM'
021100160421         clear dVPOTELLUS;
021200160421         clear TIBS02DS;
021300160421         T02mod = 'C';
021400160421         T02sif = knsif;
021500160421         T02cod = 'VPO';
021600160428         T02ke1 = 'TELLUSORM';
021700160421         TNTBE_RicercaControllo (kpjba:tibs02ds);
021800160421         IF  T02err = *blanks;
021900160421           dVPOTELLUS = T02uni;
022000160421         ENDIF;
022100160421
022200160615       //?Aggancio tabella TPR - Personalizzazione Pesi
022300160615         clear dTPR;
022400160421         clear TIBS02DS;
022500160421         T02mod = 'C';
022600160421         T02sif = knsif;
022700160615         T02cod = 'TPR';
022800160421         T02ke1 = %editc(IORG01por:'X');
022900160421         TNTBE_RicercaControllo (kpjba:tibs02ds);
023000160421         IF  T02err = *blanks;
023100160615           dTPR = T02uni;
023200160421         ENDIF;
023300160421
023400160421       //?Carico la £6 della filiale ritiro
023500160421         clear TRUL06DS;
023600160421         D06cod = '£6';
023700160428         D06key = %editc(IORG01por:'X');
023800160421         kpjbu = TRUL06DS;
023900160421         trul06r (kpjba);
024000160421         TRUL06DS = kpjbu;
024100160421
024200160421       //?Cerco la versione dei giri per la filiale da elaborare
024300160428         fidg11r1 (IORG01por:IORG01data:OUTkeyver:OUTverdes:OUTesito);
024400160421
024500160421       ENDSR;
024600160421
024700160421       //--------------------------------------------------------------
024800160421       //?Reperimento Dati del job (Utente/Operativi).
024900160421       //--------------------------------------------------------------
025000160421       BEGSR DatiJob;
025100160421
025200160421         in(E) §AzUte;
025300160421         IF  NOT %error;
025400160421           in(E) §DatiUte;
025500160421         ENDIF;
025600160421         IF  %error or RSut = *blanks;
025700160421           clear TIBS34ds;
025800160421           tibs34r(tibs34ds);
025900160421           in §AzUte;
026000160421           in §DatiUte;
026100160421         ENDIF;
026200160421
026300160421       ENDSR;
026400160421
026500160421       //--------------------------------------------------------------
026600160421       //?Elabora.
026700160421       //--------------------------------------------------------------
026800160421       BEGSR Elabora;
026900160428
027000160428         wPor = %dec(L6(xx):3:0);
027100160421
027200160421       //?Leggo i ritiri
027300160428         setll (wPor) FNORM13L;
027400160428         reade (wPor) FNORM13L;
027500160421         DOW  not %eof(FNORM13L);
027600160421
027700160421         //?Escludo i ritiri che non devono essere elaborati
027800160421           exsr Escludi;
027900160427
028000160427         //?Se OK l'ORM
028100160427           IF  not EscludiORM;
028200160427           //?Se giro di controllo devo solo contare
028300160427             IF  IORG01ctrl <> *blanks;
028400160427               OORG01nrit += 1;
028500160427             ELSE;
028600160427           //?Ho già scritto il n.max di ritiri invio i dati
028700160427               IF  contaORM > §VPObomax;
028800160427                 exsr InviaTellus;
028900160427               ENDIF;
029000160427           //?Scrivo file da inviare a Tellus
029100160427               exsr ScriviFile;
029200160427             ENDIF;
029300160427           ENDIF;
029400160427
029500160428           reade (wPor) FNORM13L;
029600160421         ENDDO;
029700160421
029800160421       ENDSR;
029900160421
030000160421       //--------------------------------------------------------------
030100160421       //?Escludo i ritiri che non devono essere elaborati.
030200160421       //--------------------------------------------------------------
030300160421       BEGSR Escludi;
030400160512
030500160512         EscludiORM = *off;
030600160428
030700160428       //?Escludo gli ORM RC
030800160428         IF  ORMnsr > 0;
030900160428           EscludiORM = *on;
031000160428           leavesr;
031100160428         ENDIF;
031200160421
031300160428       //?Escludo i già eseguiti
031400160428         IF  ORMfao > 500;
031500160421           EscludiORM = *on;
031600160421           leavesr;
031700160421         ENDIF;
031800160421
031900160421       //?Aggancio FNORG01L
032000160427         chain(n) (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
032100160421         IF  not %found(FNORG01L);
032200160421           EscludiORM = *on;
032300160421           leavesr;
032400160421         ENDIF;
032500160428
032600160428       //?Escludo gli ORM in distinta
032700160428         IF  ORGndc > 0;
032800160428           EscludiORM = *on;
032900160428           leavesr;
033000160428         ENDIF;
033100160428
033200160421       //?Con giro MANUALE
033300160421         IF  ORGtgi = 'M';
033400160421           EscludiORM = *on;
033500160421           leavesr;
033600160421         ENDIF;
033700160524
033800160524       //?Con giro ANAGRAFICA
033900160524         IF  ORGtgi = 'A';
034000160524           EscludiORM = *on;
034100160524           leavesr;
034200160524         ENDIF;
034300160421
034400160421       //?Se richiesto escludo i ritiri che hanno già il giro
034500160428         IF  IORG01giro = 'S' and ORGcgi <> *blanks;
034600160421           EscludiORM = *on;
034700160421           leavesr;
034800160421         ENDIF;
034900160427
035000160427       //?Forzo eventuale giro
035100160427         IF  ORMprr = 'RN' and ORMcar >= '47890' and
035200160427                               ORMcar <= '47899';
035300160427         //?Se non è elaborazione di test aggiorno FNORG
035400160427           IF  IORG01test = *blanks;
035500160427             chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
035600160427             IF  %found(FNORG01L);
035700160427               ORGtgi = 'M';
035800160427               ORGpocgi = ORGpor;
035900160427               ORGcgi = 'RSM' + %trim(ORMcar);
036000160427               update FNORG000;
036100160427             ENDIF;
036200160427         //?Escludo l'ORM
036300160427             EscludiORM = *on;
036400160427             leavesr;
036500160427           ENDIF;
036600160427         ENDIF;
036700160427
036800160427       //?ORM già in distinta se richiesto
036900160427         IF  ORGndc > 0 and ORGddc > 0 and IORG01dist = *blanks;
037000160427           EscludiORM = *on;
037100160427           leavesr;
037200160427         ENDIF;
037300160421
037400160421       ENDSR;
037500160427
037600160427       //--------------------------------------------------------------
037700160427       //?Scrivo file da inviare a Tellus.
037800160427       //--------------------------------------------------------------
037900160427       BEGSR ScriviFile;
038000160427
038100160428       //?Stacco Id elaborazione solo una volta
038200160428         IF  not wStaccaId;
038300160427           clear TRUL33DS;
038400160427           I33tla = 'L';
038500160427           I33cnu = 60;
038600160427           I33num = 1;
038700160427           kpjbu = TRUL33DS;
038800160427           trul33r (kpjba);
038900160427           TRUL33ds = kpjbu;
039000160427           IF  O33err <> *zeros;
039100160427             O33nri = 1;
039200160427           ENDIF;
039300160428           NrId = O33nri;
039400160428           wStaccaId = *on;
039500160427         ENDIF;
039600160427
039700160427       //?Imposto i dati per TIVGD
039800160427         clear TIVGD000;
039900160427         VGDtip = 'OO';
040000160427         IF  IORG01test <> *blanks;
040100160427           VGDksu = '00TELTST';
040200160427         ELSE;
040300160427           VGDksu = '00TELLUS';
040400160427         ENDIF;
040500160427         VGDtsc = 'WW';
040600160427         VGDdat = Oggi;
040700160428         VGDpgm = 'TLL' + %editc(NrId:'X');
040800160427         clear dVGDflo;
040900160427         §VGDfela = 'P';
041000160427         VGDflo = DVGDFLO;
041100160427
041200160427       //?Imposto i dati per ORM
041300160427         clear DTLLOO;
041400160511         §TLLOOaas = %dec(%subst(%editc(Oggi:'X'):3:2):2:0);
041500160509         §TLLOOpoe = ORMpoe;
041600160427         §TLLOOnsr = ORMnsr;
041700160427         §TLLOOnor = ORMnor;
041800160427         §TLLOOnrv = ORMnrv;
041900160428         §TLLOOidb = NrId;
042000160427         §TLLOOpor = ORMpor;
042100160427         §TLLOOind = ORMinr;
042200160427         §TLLOOcap = ORMcar;
042300160427         §TLLOOloc = ORMlor;
042400160427         §TLLOOprv = ORMprr;
042500160427         §TLLOOnaz = 'ITA';
042600160427
042700160427       //?richiamo TISI95 per avere la località normalizzata
042800160427         exsr CallTISI95;
042900160427       //?se tutto ok
043000160427         IF  O95err = *blanks;
043100160506         //?imposto provincia + provincia alternativa
043200160427           §TLLOOprv = O95prv;
043300160427           §TLLOOpra = O95pra;
043400160427         //?imposto località normalizzata solo se diversa
043500160427           IF  O95loc <> ORMlor;
043600160427             §TLLOOlocn = O95loc;
043700160427           ENDIF;
043800160427         ENDIF;
043900160427
044000160428         §TLLOOkey = OutKeyVer;
044100160427         %subst(§TLLOOflo:17:1) = '-';
044200160427
044300160427         VGDdta = %subst(DTLLOO:1:%size(DTLLOO));
044400160427         write TIVGD000;
044500160427
044600160427         IF  ContaORM < 99999;
044700160427           ContaORM += 1;
044800160427         ENDIF;
044900160427
045000160427       ENDSR;
045100160427
045200160427       //--------------------------------------------------------------
045300160427       //?Richiamo TISI95R.
045400160427       //--------------------------------------------------------------
045500160427       BEGSR CallTISI95;
045600160427
045700160427         clear TISI95DS;
045800160427         I95tcn = '7';
045900160427         I95dat = Oggi;
046000160427         I95nar = ORMnar;
046100160427         I95prv = ORMprr;
046200160427         I95cap = ORMcar;
046300160427         I95loc = ORMlor;
046400160427         I95lkg = ORGpkg;
046500160427         I95lmc = ORGvlm;
046600160427         tisi95r (TISI95DS);
046700160427
046800160427         SELECT;
046900161121       //?Se c'è sponda idraulica imposto ambito sempre OLTRE
047000161121         WHEN  ORMspi = 'S';
047100161121           §TLLOOain = '>';
047200160427       //?Imposto ambito in base al cappario
047300160615       //?se non c'è tabella TPR
047400160615       //?se     c'è tabella TPR e non devo fare i calcoli in base al peso
047500160615         WHEN  §TPRlkg = 0;
047600160427         //?Imposto ambito di instradamento se non c'è stato errore
047700160427           IF  O95err <> *blanks;
047800160427             §TLLOOerr = 'P';
047900160427           ELSE;
048000160427         //?per determinare l'ambito della filiale/zona di ritiro
048100160427         //?dell'instradamento reperito
048200160429         //?controllo in ordine: LOC - CAP
048300160429             //?==> LOC
048400160429             IF  O95liv = 'L';
048500161121             //?STANDARD
048600160429               IF  O95lna = O95lla and O95znc = O95lzc;
048700160429                 §TLLOOain = '=';
048800160429               ENDIF;
048900161121             //?SOTTO
049000160429               IF  O95lna = O95lls and O95znc = O95lzs;
049100160429                 §TLLOOain = '<';
049200160429               ENDIF;
049300161121             //?OLTRE
049400160429               IF  O95lna = O95llo and O95znc = O95lzo;
049500160429                 §TLLOOain = '>';
049600160429               ENDIF;
049700160429             //?==> CAP
049800160429             ELSE;
049900161121             //?STANDARD
050000160429               IF  O95lna = O95cla and O95znc = O95czc;
050100160429                 §TLLOOain = '=';
050200160429               ENDIF;
050300161121             //?SOTTO
050400160429               IF  O95lna = O95cls and O95znc = O95czs;
050500160429                 §TLLOOain = '<';
050600160429               ENDIF;
050700161121             //?OLTRE
050800160429               IF  O95lna = O95clo and O95znc = O95czo;
050900160429                 §TLLOOain = '>';
051000160429               ENDIF;
051100160429             ENDIF;
051200160427           ENDIF;
051300161121         //?se non impostato niente metto di dft STANDARD
051400160427           IF  §TLLOOain = *blanks;
051500160427             §TLLOOain = '=';
051600160427           ENDIF;
051700160427       //?Imposto ambito in base alla tabella
051800160427         OTHER;
051900161121         //?Limite SOTTO personalizzato
052000160615           IF  ORGpkg < §TPRlks or ORGvlm < §TPRlms;
052100160427             §TLLOOain = '<';
052200160427           ENDIF;
052300161121         //?Limite OLTRE personalizzato
052400161123           IF  ORGpkg > §TPRlkg or ORGvlm > §TPRlmc;
052500160427             §TLLOOain = '>';
052600160427           ENDIF;
052700161121         //?Se non impostato niente metto di dft STANDARD
052800160427           IF  §TLLOOain = *blanks;
052900160427             §TLLOOain = '=';
053000160427           ENDIF;
053100160427         ENDSL;
053200160427
053300160427       ENDSR;
053400160427
053500160427       //--------------------------------------------------------------
053600160427       //?Invio i dati a Tellus.
053700160427       //--------------------------------------------------------------
053800160427       BEGSR InviaTellus;
053900160427
054000160427         IF  ContaORM > 0;
054100160427         //?Scrivo file di LOG
054200160427           exsr ScriviLog;
054300160427
054400160427           clear TIS781DS;
054500160427           clear TIS781DFLO;
054600160506           §781tip = 'OO';
054700160427           IF  IORG01test <> *blanks;
054800160427             §781ksu = '00TELTST';
054900160427           ELSE;
055000160427             §781ksu = '00TELLUS';
055100160427           ENDIF;
055200160427           §781tsc = 'WW';
055300160427           §781dat = Oggi;
055400160428           §781pgm = 'TLL' + %editc(NrId:'X');
055500160505           §781out = 'SO' + %subst(%editc(NrId:'X'):3:5) +
055600160427                     %editc(IORG01por:'X');
055700160427           §781flocsi = 'S';
055800160427           §781floela = 'P';
055900160427           §781flo = TIS781DFLO;
056000160428
056100160428           feod TIVGD00F;
056200160428
056300160428           tis781c1 (P781esito:tis781ds:P781prg);
056400160428
056500160506           chain(e) (NrId) FILTO01L;
056600160428           IF  not %error;
056700160428             IF  P781prg = *blanks;
056800160506               LTOhdl = 'ANNULLATO';
056900160506               LTOprg = '*NULL';
057000160506               LTOdatu = Oggi;
057100160506               LTOorau = %dec(%time());
057200160428             ELSE;
057300160506               IF  LTOhdl <> 'ANNULLATO';
057400160506                 clear LTOhdl;
057500160428               ENDIF;
057600160506               LTOprg = P781prg;
057700160428             ENDIF;
057800160506             update FILTO000;
057900160428           ENDIF;
058000160428
058100160427         ENDIF;
058200160427
058300160428         wStaccaId = *off;
058400160427         clear ContaORM;
058500160427
058600160427       ENDSR;
058700160428
058800160428       //--------------------------------------------------------------
058900160428       //?Scrivo file di LOG.
059000160428       //--------------------------------------------------------------
059100160428       BEGSR ScriviLog;
059200160428
059300160506         clear FILTO000;
059400160506         clear DLTOFLO;
059500160506         LTOidb = NrId;
059600160509         LTOfil = IORG01por;
059700160506         LTOprg = P781prg;
059800160506         LTOhdl = 'IN CORSO';
059900160506         LTOdat = Oggi;
060000160506         LTOora = %dec(%time());
060100160506         LTOnris = ContaORM;
060200160428         §FLOtst = IORG01test;
060300160506         LTOflo = DLTOFLO;
060400160506         write FILTO000;
060500160428
060600160428       ENDSR;
060700160421
060800160421       //--------------------------------------------------------------
060900160421       //?Operazioni finali.
061000160421       //--------------------------------------------------------------
061100160421       BEGSR RoutEnd;
061200160421
061300160421         *inLR = *on;
061400160421         return;
061500160421
061600160421       ENDSR;
061700160421
061800160421      /end-free
