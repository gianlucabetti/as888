000100160421      //--------------------------------------------------------------
000200160617      //?FIORG11R - CONTROLLO GIRI ANAGRAFICA CLIENTI RITIRO
000300160617      //?           DA BRT A TELLUS
000400160617      //?           GENERA FILE DI WRK - WFACR10F
000500160421      //--------------------------------------------------------------
000600160421     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700160421     h dftactgrp(*no) actgrp('BARTVAS')
000800160421
000900160421      //---------------------------------------------------------------
001000160421      //?Dichiarazione file.
001100160421      //---------------------------------------------------------------
001200160617      // - File Anagrafica Clienti Ritiro
001300160617     fFNACR01L  if   e           k disk
001400160427
001500160427      // - File scambio dati con Tellus
001600160428     fTIVGD00F  if a e             disk
001700160617
001800160617      // - File WRK Giri elaborati
001900160617     fWFACR10F  o    e             disk
002000160421
002100160421      //---------------------------------------------------------------
002200160421      //?Definizione costanti.
002300160421      //---------------------------------------------------------------
002400160421
002500160421      //---------------------------------------------------------------
002600160421      //?Definizione schiere.
002700160421      //---------------------------------------------------------------
002800160421
002900160421      //---------------------------------------------------------------
003000160421      //?Definizione aree dati.
003100160421      //---------------------------------------------------------------
003200160421      // - Dati utente
003300160421     d §AzUte        e ds                  extname(AZUTE00F)
003400160421     d                                     dtaara
003500160421     d §DatiUte      e ds                  extname(dDatiUte)
003600160421     d                                     dtaara
003700160421
003800160421      //---------------------------------------------------------------
003900160421      //?Definizione strutture dati.
004000160421      //---------------------------------------------------------------
004100160421      // - Parametri ricevuti
004200160421     d KPJBA         e ds
004300160617
004400160617      // - Controllo/Decodifica/Ricerca Giro
004500160617     d FIDG09DS      e ds                  inz
004600160421
004700160421      // - Ricerca/Controllo tabelle
004800160421     d TIBS02DS      e ds                  inz
004900160421
005000160421      // - Reperimento dati utente
005100160421     d TIBS34DS      e ds
005200160427
005300160427      // - Instradamento
005400160427     d TISI95DS      e ds
005500160427
005600160427      // - Scambio dati VAS - DOWNLOAD
005700160427     d TIS781DS      e ds
005800160427
005900160427       // - Stacca numero
006000160427     d TRUL33DS      e ds
006100160427
006200160427      // - Dati Output Tellus
006300160617     d DTLLXO        e ds
006400160427
006500160427      // - Campo VGDflo
006600160427     d DVGDFLO       e ds
006700160427
006800160427      // - Campo §781flo
006900160427     d TIS781DFLO    e ds
007000160617
007100160617      // - Campo GRGflo
007200160617     d dGRGFLO       e ds
007300160617
007400160617      // - File FNACR10F
007500160617     d FNACR10F      e ds                  extname(FNACR10F)
007600160421
007700160421      //---------------------------------------------------------------
007800160421      //?Definizione variabili globali.
007900160421      //---------------------------------------------------------------
008000160421      // - Flags booleani
008100160421     d Fine            s               n   inz(*off)
008200160617     d OkAlmenoUno     s               n   inz(*off)
008300160617     d wStaccaId       s               n   inz(*off)
008400160421
008500160421      // - Indici di schiera
008600160421     d xx              s              4s 0 inz
008700160421
008800160421      // - Campi x richiamo FIDG11R1
008900160421     d OUTkeyver       s              8a   inz
009000160421     d OUTverdes       s             30a   inz
009100160421     d OUTesito        s              1a   inz
009200160428
009300160428      // - Campi x richiamo TIS781C1
009400160428     d P781esito       s              1a   inz
009500160428     d P781prg         s             10a   inz
009600160421
009700160421      // - Campi di comodo
009800160617     d Filiale         s              3s 0 inz
009900160617     d NrId            s              7s 0 inz
010000160421     d Oggi            s              8s 0 inz
010100160421
010200160421      //---------------------------------------------------------------
010300160421      //?Definizione Procedure usate.
010400160421      //---------------------------------------------------------------
010500160617      // - Pgm descrizione giro
010600160617     d FIDG09R         pr                  extpgm('FIDG09R')
010700160617     d  kpjba                              likeds(kpjba)
010800160617
010900160421       // - Controllo versione giri
011000160421     d FIDG11R1        pr                  extpgm('FIDG11R1')
011100160428     d  IORG01por                     3s 0
011200160428     d  IORG01data                    8s 0
011300160421     d  OUTkeyver                     8a
011400160421     d  OUTverdes                    30a
011500160421     d  OUTesito                      1a
011600160427
011700160427       // - Instradamentp
011800160427     d TISI95R         pr                  extpgm('TISI95R')
011900160427     d  tisi95ds                           likeds(tisi95ds)
012000160428
012100160428       // - Invio dati
012200160428     d TIS781C1        pr                  extpgm('TIS781C1')
012300160428     d  P781esito                     1a
012400160428     d  tis781ds                           likeds(tis781ds)
012500160428     d  P781prg                      10a
012600160427
012700160427       // - Stacca Numero
012800160427     d TRUL33R         pr                  extpgm('TRUL33R')
012900160427     d  kpjba                              likeds(kpjba)
013000160421
013100160421      //---------------------------------------------------------------
013200160421      //?Definizione Prototipi.
013300160421      //---------------------------------------------------------------
013400160421      /copy gaitrasrc/srcprotopr,TIBS02R
013500160421      /copy gaitrasrc/srcprotopr,TIBS34R
013600160421
013700160421      //---------------------------------------------------------------
013800160421      //?Definizione key-list.
013900160421      //---------------------------------------------------------------
014000160421
014100160421      //---------------------------------------------------------------
014200160421      //?M A I N - L I N E
014300160421      //---------------------------------------------------------------
014400160421
014500160421     c     *Entry        plist
014600160421     c                   parm                    kpjba
014700160421
014800160421      /free
014900160421
015000160421       //?Operazioni iniziali
015100160421       exsr RoutInz;
015200160421
015300160617       //?Elabora
015400160617       exsr Elabora;
015500160427
015600160617       //?Mando i dati a tellus
015700160617       exsr InviaTellus;
015800160421
015900160421       //?Operazioni finali
016000160421       exsr RoutEnd;
016100160421
016200160421       //--------------------------------------------------------------
016300160421       //?Operazioni iniziali.
016400160421       //--------------------------------------------------------------
016500160421       BEGSR RoutInz;
016600160617
016700160617         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016800160617
016900160617       //?Recupero la filiale da elaborare
017000160617         Filiale = %dec(%subst(kpjbu:1:3):3:0);
017100160421
017200160421       //?Reperimento dati job
017300160421         exsr DatiJob;
017400160421
017500160421       //?Imposto oggi
017600160421         Oggi = %dec(%date());
017700160421
017800160421       //?Cerco la versione dei giri per la filiale da elaborare
017900160617         fidg11r1 (filiale:oggi:OUTkeyver:OUTverdes:OUTesito);
018000160421
018100160421       ENDSR;
018200160421
018300160421       //--------------------------------------------------------------
018400160421       //?Reperimento Dati del job (Utente/Operativi).
018500160421       //--------------------------------------------------------------
018600160421       BEGSR DatiJob;
018700160421
018800160421         in(E) §AzUte;
018900160421         IF  NOT %error;
019000160421           in(E) §DatiUte;
019100160421         ENDIF;
019200160421         IF  %error or RSut = *blanks;
019300160421           clear TIBS34ds;
019400160421           tibs34r(tibs34ds);
019500160421           in §AzUte;
019600160421           in §DatiUte;
019700160421         ENDIF;
019800160421
019900160421       ENDSR;
020000160421
020100160421       //--------------------------------------------------------------
020200160421       //?Elabora.
020300160421       //--------------------------------------------------------------
020400160421       BEGSR Elabora;
020500160617
020600160617         OkAlmenoUno = *off;
020700160421
020800160617       //?Leggo i giri delle anagrafiche
020900160617         exec sql
021000160617         DECLARE acr CURSOR for
021100160617         SELECT * from FNACR10F
021200160617         WHERE ACR1atb = ' ' and ACR1pocgi = :Filiale
021300160617         ORDER BY ACR1pocgi, ACR1ain, ACR1cgi, ACR1cro;
021400160617
021500160617         exec sql
021600160617         OPEN acr;
021700160617
021800160617         IF sqlcode < 0;
021900160617           Fine = *on;
022000160617           exec sql
022100160617           CLOSE acr;
022200160617           leavesr;
022300160617         ENDIF;
022400160617
022500160617         DOW  not Fine;
022600160617           exec sql
022700160617           FETCH NEXT from acr into :FNACR10F;
022800160617           IF  sqlcod = 100 or sqlcod < 0;
022900160617             Fine = *on;
023000160617             leave;
023100160617           ENDIF;
023200160617
023300160617         //?Scrivo file di WORK e file per Tellus
023400160617           exsr ScriviFile;
023500160617
023600160617         ENDDO;
023700160617
023800160617       //?Chiusura del cursore
023900160617         exec sql
024000160617         CLOSE acr;
024100160421
024200160421       ENDSR;
024300160427
024400160427       //--------------------------------------------------------------
024500160617       //?Scrivo file di WORK e file da inviare a Tellus.
024600160427       //--------------------------------------------------------------
024700160427       BEGSR ScriviFile;
024800160617
024900160617         clear WFACR100;
025000160617
025100160617       //?Aggancio Anagrafica Clienti Ritiro
025200160617         chain (ACR1cro) FNACR01L;
025300160617       //?Non trovo anagrafica clienti non elaboro
025400160617         IF  not %found(FNACR01L);
025500160617           leavesr;
025600160617         ENDIF;
025700160617       //?Cliente annullato non elaboro
025800160617         IF  ACRatb <> *blanks;
025900160617           leavesr;
026000160617         ENDIF;
026100160617
026200160617         WFutente  = knmus;
026300160617         WFdataela = oggi;
026400160617         WFfiliale = filiale;
026500160617
026600160617       //?Stacco Key di work
026700160617         clear TRUL33DS;
026800160617         I33tla = 'L';
026900160617         I33cnu = 399;
027000160617         I33num = 1;
027100160617         kpjbu = TRUL33DS;
027200160617         trul33r (kpjba);
027300160617         TRUL33ds = kpjbu;
027400160617         IF  O33err <> *zeros;
027500160617           O33nri = 1;
027600160617         ENDIF;
027700160617         WFwrkkey = O33nri;
027800160617
027900160617       //?Imposto i dati presi da FNACR10F
028000160617         WFAcra = ACR1cro;
028100160617         WFAcgi = ACR1cgi;
028200160617         WFAain = ACR1ain;
028300160617         WFAfil = ACR1pocgi;
028400160617
028500160617       //?Imposto i dati presi da FNACR00F
028600160617         WFArag = ACRrsr;
028700160617         WFAind = ACRinr;
028800160617         WFAloc = ACRlor;
028900160617         WFAcap = ACRcar;
029000160617         WFAprv = ACRprr;
029100160617         WFAnaz = ACRnar;
029200160617
029300160617       //?Recupero il tipo giro dall'anagrafica Giri
029400160617         clear FIDG09DS;
029500160617         D09iop0 = '001';
029600160617         D09ifgs = filiale;
029700160617         D09idat = oggi;
029800160617         D09icgi = WFAcgi;
029900160617         kpjbu = FIDG09DS;
030000160617         fidg09r (kpjba);
030100160617         fidg09ds = kpjbu;
030200160617         IF  D09oerr = *blanks;
030300160617           dGRGflo = D09oflo;
030400160617         ENDIF;
030500160617         WFAtug = §GRGtug;
030600160617
030700160617       //?Scrivo file WRK
030800160617         write WFACR100;
030900160617         feod WFACR10F;
031000160617
031100160617       //?Stacco Id elaborazione solo una volta
031200160617         IF  not wStaccaId;
031300160617           clear TRUL33DS;
031400160617           I33tla = 'L';
031500160617           I33cnu = 60;
031600160617           I33num = 1;
031700160617           kpjbu = TRUL33DS;
031800160617           trul33r (kpjba);
031900160617           TRUL33ds = kpjbu;
032000160617           IF  O33err <> *zeros;
032100160617             O33nri = 1;
032200160617           ENDIF;
032300160617           NrId = O33nri;
032400160617           wStaccaId = *on;
032500160617         ENDIF;
032600160427
032700160427       //?Imposto i dati per TIVGD
032800160427         clear TIVGD000;
032900160617         VGDtip = 'XO';
033000160617         VGDksu = '00TELLUS';
033100160427         VGDtsc = 'WW';
033200160427         VGDdat = Oggi;
033300160617         VGDpgm = 'TLL' + %editc(NrId:'X');
033400160427         clear dVGDflo;
033500160427         §VGDfela = 'P';
033600160427         VGDflo = DVGDFLO;
033700160427
033800160617       //?Imposto i dati ds da inviare a tellus
033900160617         clear DTLLXO;
034000160617         §TLLXOetyp = 'V01';
034100160617         §TLLXOekey = %editc(WFwrkkey:'X');
034200160617         §TLLXOfil  = WFfiliale;
034300160617         §TLLXOind  = WFAind;
034400160617         §TLLXOcap  = WFAcap;
034500160617         §TLLXOloc  = WFAloc;
034600160617         §TLLXOprv  = WFAprv;
034700160617         IF  WFAnaz = *blanks;
034800160617           §TLLXOnaz = 'ITA';
034900160617         ELSE;
035000160617           §TLLXOnaz  = WFAnaz;
035100160617         ENDIF;
035200160617         §TLLXOain = WFAain;
035300160617         §TLLXOcgi = WFAcgi;
035400160427
035500160427       //?richiamo TISI95 per avere la località normalizzata
035600160427         exsr CallTISI95;
035700160427       //?se tutto ok
035800160427         IF  O95err = *blanks;
035900160506         //?imposto provincia + provincia alternativa
036000160617           §TLLXOprv = O95prv;
036100160617           §TLLXOpra = O95pra;
036200160427         //?imposto località normalizzata solo se diversa
036300160617           IF  O95loc <> WFAloc;
036400160617             §TLLXOlocn = O95loc;
036500160427           ENDIF;
036600160427         ENDIF;
036700160427
036800160617         §TLLXOkey = OutKeyVer;
036900160617         %subst(§TLLXOflo:17:1) = '-';
037000160427
037100160617         VGDdta = %subst(DTLLXO:1:%size(DTLLXO));
037200160427         write TIVGD000;
037300160617
037400160617         OkAlmenoUno = *on;
037500160427
037600160427       ENDSR;
037700160427
037800160427       //--------------------------------------------------------------
037900160427       //?Richiamo TISI95R.
038000160427       //--------------------------------------------------------------
038100160427       BEGSR CallTISI95;
038200160427
038300160427         clear TISI95DS;
038400160427         I95tcn = '7';
038500160427         I95dat = Oggi;
038600160617         I95nar = WFAnaz;
038700160617         I95prv = WFAprv;
038800160617         I95cap = WFAcap;
038900160617         I95loc = WFAloc;
039000160427         tisi95r (TISI95DS);
039100160427
039200160427       ENDSR;
039300160427
039400160427       //--------------------------------------------------------------
039500160427       //?Invio i dati a Tellus.
039600160427       //--------------------------------------------------------------
039700160427       BEGSR InviaTellus;
039800160427
039900160617         IF  OkAlmenoUno;
040000160427
040100160427           clear TIS781DS;
040200160427           clear TIS781DFLO;
040300160617           §781tip = 'XO';
040400160617           §781ksu = '00TELLUS';
040500160427           §781tsc = 'WW';
040600160427           §781dat = Oggi;
040700160617           §781pgm = 'TLL' + %editc(NrId:'X');
040800160617           §781out = 'SX' + %subst(%editc(NrId:'X'):3:5) +
040900160617                     %editc(Filiale:'X');
041000160427           §781flocsi = 'S';
041100160427           §781floela = 'P';
041200160427           §781flo = TIS781DFLO;
041300160428
041400160428           feod TIVGD00F;
041500160428
041600160428           tis781c1 (P781esito:tis781ds:P781prg);
041700160428
041800160427         ENDIF;
041900160427
042000160428         wStaccaId = *off;
042100160617         OkAlmenoUno = *off;
042200160427
042300160427       ENDSR;
042400160421
042500160421       //--------------------------------------------------------------
042600160421       //?Operazioni finali.
042700160421       //--------------------------------------------------------------
042800160421       BEGSR RoutEnd;
042900160421
043000160421         *inLR = *on;
043100160421         return;
043200160421
043300160421       ENDSR;
043400160421
043500160421      /end-free
