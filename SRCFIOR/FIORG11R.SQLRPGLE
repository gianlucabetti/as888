000100160617      //--------------------------------------------------------------
000200160617      //?FIORG11R - CONTROLLO GIRI ANAGRAFICA CLIENTI RITIRO
000300160617      //?           DA BRT A TELLUS
000400160617      //?           GENERA FILE DI WRK - WFACR10F
000500160617      //--------------------------------------------------------------
000600160617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700160617     h dftactgrp(*no) actgrp('BARTVAS')
000800160617
000900160617      //---------------------------------------------------------------
001000160617      //?Dichiarazione file.
001100160617      //---------------------------------------------------------------
001200160617      // - File Anagrafica Clienti Ritiro
001300160617     fFNACR01L  if   e           k disk
001400160617
001500160617      // - File scambio dati con Tellus
001600160617     fTIVGD00F  if a e             disk
001700160617
001800160617      // - File WRK Giri elaborati
001900160617     fWFACR10F  o    e             disk
002000160617
002100160617      //---------------------------------------------------------------
002200160617      //?Definizione costanti.
002300160617      //---------------------------------------------------------------
002400160617
002500160617      //---------------------------------------------------------------
002600160617      //?Definizione schiere.
002700160617      //---------------------------------------------------------------
002800160617
002900160617      //---------------------------------------------------------------
003000160617      //?Definizione aree dati.
003100160617      //---------------------------------------------------------------
003200160617      // - Dati utente
003300160617     d §AzUte        e ds                  extname(AZUTE00F)
003400160617     d                                     dtaara
003500160617     d §DatiUte      e ds                  extname(dDatiUte)
003600160617     d                                     dtaara
003700160617
003800160617      //---------------------------------------------------------------
003900160617      //?Definizione strutture dati.
004000160617      //---------------------------------------------------------------
004100160617      // - Parametri ricevuti
004200160617     d KPJBA         e ds
004300160617
004400160617      // - Controllo/Decodifica/Ricerca Giro
004500160617     d FIDG09DS      e ds                  inz
004600160617
004700160617      // - Reperimento dati utente
004800160617     d TIBS34DS      e ds
004900160617
005000160617      // - Instradamento
005100160617     d TISI95DS      e ds
005200160617
005300160617      // - Scambio dati VAS - DOWNLOAD
005400160617     d TIS781DS      e ds
005500160617
005600160617       // - Stacca numero
005700160617     d TRUL33DS      e ds
005800160617
005900160617      // - Dati Output Tellus
006000160617     d DTLLXO        e ds
006100160617
006200160617      // - Campo VGDflo
006300160617     d DVGDFLO       e ds
006400160617
006500160617      // - Campo §781flo
006600160617     d TIS781DFLO    e ds
006700160617
006800160617      // - Campo GRGflo
006900160617     d dGRGFLO       e ds
007000160617
007100160617      // - File FNACR10F
007200160617     d FNACR10F      e ds                  extname(FNACR10F)
007300160617
007400160617      //---------------------------------------------------------------
007500160617      //?Definizione variabili globali.
007600160617      //---------------------------------------------------------------
007700160617      // - Flags booleani
007800160617     d Fine            s               n   inz(*off)
007900160617     d OkAlmenoUno     s               n   inz(*off)
008000160617     d wStaccaId       s               n   inz(*off)
008100160617
008200160617      // - Campi x richiamo FIDG11R1
008300160617     d OUTkeyver       s              8a   inz
008400160617     d OUTverdes       s             30a   inz
008500160617     d OUTesito        s              1a   inz
008600160617
008700160617      // - Campi x richiamo TIS781C1
008800160617     d P781esito       s              1a   inz
008900160617     d P781prg         s             10a   inz
009000160617
009100160617      // - Campi di comodo
009200160617     d Filiale         s              3s 0 inz
009300160617     d NrId            s              7s 0 inz
009400160617     d Oggi            s              8s 0 inz
009500160617
009600160617      //---------------------------------------------------------------
009700160617      //?Definizione Procedure usate.
009800160617      //---------------------------------------------------------------
009900160617      // - Pgm descrizione giro
010000160617     d FIDG09R         pr                  extpgm('FIDG09R')
010100160617     d  kpjba                              likeds(kpjba)
010200160617
010300160617       // - Controllo versione giri
010400160617     d FIDG11R1        pr                  extpgm('FIDG11R1')
010500160617     d  IORG01por                     3s 0
010600160617     d  IORG01data                    8s 0
010700160617     d  OUTkeyver                     8a
010800160617     d  OUTverdes                    30a
010900160617     d  OUTesito                      1a
011000160617
011100160617       // - Instradamentp
011200160617     d TISI95R         pr                  extpgm('TISI95R')
011300160617     d  tisi95ds                           likeds(tisi95ds)
011400160617
011500160617       // - Invio dati
011600160617     d TIS781C1        pr                  extpgm('TIS781C1')
011700160617     d  P781esito                     1a
011800160617     d  tis781ds                           likeds(tis781ds)
011900160617     d  P781prg                      10a
012000160617
012100160617       // - Stacca Numero
012200160617     d TRUL33R         pr                  extpgm('TRUL33R')
012300160617     d  kpjba                              likeds(kpjba)
012400160617
012500160617      //---------------------------------------------------------------
012600160617      //?Definizione Prototipi.
012700160617      //---------------------------------------------------------------
012800160617      /copy gaitrasrc/srcprotopr,TIBS34R
012900160617
013000160617      //---------------------------------------------------------------
013100160617      //?Definizione key-list.
013200160617      //---------------------------------------------------------------
013300160617
013400160617      //---------------------------------------------------------------
013500160617      //?M A I N - L I N E
013600160617      //---------------------------------------------------------------
013700160617     c     *Entry        plist
013800160617     c                   parm                    kpjba
013900160617
014000160617      /free
014100160617
014200160617       //?Operazioni iniziali
014300160617       exsr RoutInz;
014400160617
014500160617       //?Elabora
014600160617       exsr Elabora;
014700160617
014800160617       //?Mando i dati a tellus
014900160617       exsr InviaTellus;
015000160617
015100160617       //?Operazioni finali
015200160617       exsr RoutEnd;
015300160617
015400160617       //--------------------------------------------------------------
015500160617       //?Operazioni iniziali.
015600160617       //--------------------------------------------------------------
015700160617       BEGSR RoutInz;
015800160617
015900160617         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016000160617
016100160617       //?Recupero la filiale da elaborare
016200160617         Filiale = %dec(%subst(kpjbu:1:3):3:0);
016300160617
016400160617       //?Reperimento dati job
016500160617         exsr DatiJob;
016600160617
016700160617       //?Imposto oggi
016800160617         Oggi = %dec(%date());
016900160617
017000160617       //?Cerco la versione dei giri per la filiale da elaborare
017100160617         fidg11r1 (filiale:oggi:OUTkeyver:OUTverdes:OUTesito);
017200160617
017300160617       ENDSR;
017400160617
017500160617       //--------------------------------------------------------------
017600160617       //?Reperimento Dati del job (Utente/Operativi).
017700160617       //--------------------------------------------------------------
017800160617       BEGSR DatiJob;
017900160617
018000160617         in(E) §AzUte;
018100160617         IF  NOT %error;
018200160617           in(E) §DatiUte;
018300160617         ENDIF;
018400160617         IF  %error or RSut = *blanks;
018500160617           clear TIBS34ds;
018600160617           tibs34r(tibs34ds);
018700160617           in §AzUte;
018800160617           in §DatiUte;
018900160617         ENDIF;
019000160617
019100160617       ENDSR;
019200160617
019300160617       //--------------------------------------------------------------
019400160617       //?Elabora.
019500160617       //--------------------------------------------------------------
019600160617       BEGSR Elabora;
019700160617
019800160617         OkAlmenoUno = *off;
019900160617
020000160617       //?Leggo i giri delle anagrafiche
020100160617         exec sql
020200160617         DECLARE acr CURSOR for
020300160617         SELECT * from FNACR10F
020400160617         WHERE ACR1atb = ' ' and ACR1pocgi = :Filiale
020500160617         ORDER BY ACR1pocgi, ACR1ain, ACR1cgi, ACR1cro;
020600160617
020700160617         exec sql
020800160617         OPEN acr;
020900160617
021000160617         IF  sqlcode < 0;
021100160617           Fine = *on;
021200160617           exec sql
021300160617           CLOSE acr;
021400160617           leavesr;
021500160617         ENDIF;
021600160617
021700160617         DOW  not Fine;
021800160617           exec sql
021900160617           FETCH NEXT from acr into :FNACR10F;
022000160617           IF  sqlcod = 100 or sqlcod < 0;
022100160617             Fine = *on;
022200160617             leave;
022300160617           ENDIF;
022400160617
022500160617         //?Scrivo file di WORK e file per Tellus
022600160617           exsr ScriviFile;
022700160617
022800160617         ENDDO;
022900160617
023000160617       //?Chiusura del cursore
023100160617         exec sql
023200160617         CLOSE acr;
023300160617
023400160617       ENDSR;
023500160617
023600160617       //--------------------------------------------------------------
023700160617       //?Scrivo file di WORK e file da inviare a Tellus.
023800160617       //--------------------------------------------------------------
023900160617       BEGSR ScriviFile;
024000160617
024100160617         clear WFACR100;
024200160617
024300160617       //?Aggancio Anagrafica Clienti Ritiro
024400160617         chain (ACR1cro) FNACR01L;
024500160617       //?Non trovo anagrafica clienti non elaboro
024600160617         IF  not %found(FNACR01L);
024700160617           leavesr;
024800160617         ENDIF;
024900160617       //?Cliente annullato non elaboro
025000160617         IF  ACRatb <> *blanks;
025100160617           leavesr;
025200160617         ENDIF;
025300160617
025400160617         WFutente  = knmus;
025500160617         WFdataela = oggi;
025600160617         WFfiliale = filiale;
025700160617
025800160617       //?Stacco Key di work
025900160617         clear TRUL33DS;
026000160617         I33tla = 'L';
026100160617         I33cnu = 399;
026200160617         I33num = 1;
026300160617         kpjbu = TRUL33DS;
026400160617         trul33r (kpjba);
026500160617         TRUL33ds = kpjbu;
026600160617         IF  O33err <> *zeros;
026700160617           O33nri = 1;
026800160617         ENDIF;
026900160617         WFwrkkey = O33nri;
027000160617
027100160617       //?Imposto i dati presi da FNACR10F
027200160617         WFAcra = ACR1cro;
027300160617         WFAcgi = ACR1cgi;
027400160617         WFAain = ACR1ain;
027500160617         WFAfil = ACR1pocgi;
027600160617
027700160617       //?Imposto i dati presi da FNACR00F
027800160617         WFArag = ACRrsr;
027900160617         WFAind = ACRinr;
028000160617         WFAloc = ACRlor;
028100160617         WFAcap = ACRcar;
028200160617         WFAprv = ACRprr;
028300160617         WFAnaz = ACRnar;
028400160617
028500160617       //?Recupero il tipo giro dall'anagrafica Giri
028600160617         clear FIDG09DS;
028700160620         clear dGRGflo;
028800160617         D09iop0 = '001';
028900160617         D09ifgs = filiale;
029000160617         D09idat = oggi;
029100160617         D09icgi = WFAcgi;
029200160617         kpjbu = FIDG09DS;
029300160617         fidg09r (kpjba);
029400160617         fidg09ds = kpjbu;
029500160617         IF  D09oerr = *blanks;
029600160617           dGRGflo = D09oflo;
029700160617         ENDIF;
029800160617         WFAtug = §GRGtug;
029900160617
030000160617       //?Scrivo file WRK
030100160617         write WFACR100;
030200160617         feod WFACR10F;
030300160617
030400160617       //?Stacco Id elaborazione solo una volta
030500160617         IF  not wStaccaId;
030600160617           clear TRUL33DS;
030700160617           I33tla = 'L';
030800160617           I33cnu = 60;
030900160617           I33num = 1;
031000160617           kpjbu = TRUL33DS;
031100160617           trul33r (kpjba);
031200160617           TRUL33ds = kpjbu;
031300160617           IF  O33err <> *zeros;
031400160617             O33nri = 1;
031500160617           ENDIF;
031600160617           NrId = O33nri;
031700160617           wStaccaId = *on;
031800160617         ENDIF;
031900160617
032000160617       //?Imposto i dati per TIVGD
032100160617         clear TIVGD000;
032200160617         VGDtip = 'XO';
032300160617         VGDksu = '00TELLUS';
032400160617         VGDtsc = 'WW';
032500160617         VGDdat = Oggi;
032600160617         VGDpgm = 'TLL' + %editc(NrId:'X');
032700160617         clear dVGDflo;
032800160617         §VGDfela = 'P';
032900160617         VGDflo = DVGDFLO;
033000160617
033100160617       //?Imposto i dati ds da inviare a tellus
033200160617         clear DTLLXO;
033300160617         §TLLXOetyp = 'V01';
033400160620         §TLLXOekey = *all'0';
033500160620         %subst(§TLLXOekey:1:7) = %editc(WFwrkkey:'X');
033600160617         §TLLXOfil  = WFfiliale;
033700160617         §TLLXOind  = WFAind;
033800160617         §TLLXOcap  = WFAcap;
033900160617         §TLLXOloc  = WFAloc;
034000160617         §TLLXOprv  = WFAprv;
034100160617         IF  WFAnaz = *blanks;
034200160617           §TLLXOnaz = 'ITA';
034300160617         ELSE;
034400160617           §TLLXOnaz  = WFAnaz;
034500160617         ENDIF;
034600160617         §TLLXOain = WFAain;
034700160620         //§TLLXOcgi = WFAcgi;
034800160617
034900160617       //?richiamo TISI95 per avere la località normalizzata
035000160617         exsr CallTISI95;
035100160617       //?se tutto ok
035200160617         IF  O95err = *blanks;
035300160617         //?imposto provincia + provincia alternativa
035400160617           §TLLXOprv = O95prv;
035500160617           §TLLXOpra = O95pra;
035600160617         //?imposto località normalizzata solo se diversa
035700160617           IF  O95loc <> WFAloc;
035800160617             §TLLXOlocn = O95loc;
035900160617           ENDIF;
036000160617         ENDIF;
036100160617
036200160617         §TLLXOkey = OutKeyVer;
036300160617         %subst(§TLLXOflo:17:1) = '-';
036400160617
036500160617         VGDdta = %subst(DTLLXO:1:%size(DTLLXO));
036600160617         write TIVGD000;
036700160617
036800160617         OkAlmenoUno = *on;
036900160617
037000160617       ENDSR;
037100160617
037200160617       //--------------------------------------------------------------
037300160617       //?Richiamo TISI95R.
037400160617       //--------------------------------------------------------------
037500160617       BEGSR CallTISI95;
037600160617
037700160617         clear TISI95DS;
037800160617         I95tcn = '7';
037900160617         I95dat = Oggi;
038000160617         I95nar = WFAnaz;
038100160617         I95prv = WFAprv;
038200160617         I95cap = WFAcap;
038300160617         I95loc = WFAloc;
038400160617         tisi95r (TISI95DS);
038500160617
038600160617       ENDSR;
038700160617
038800160617       //--------------------------------------------------------------
038900160617       //?Invio i dati a Tellus.
039000160617       //--------------------------------------------------------------
039100160617       BEGSR InviaTellus;
039200160617
039300160617         IF  OkAlmenoUno;
039400160617
039500160617           clear TIS781DS;
039600160617           clear TIS781DFLO;
039700160617           §781tip = 'XO';
039800160617           §781ksu = '00TELLUS';
039900160617           §781tsc = 'WW';
040000160617           §781dat = Oggi;
040100160617           §781pgm = 'TLL' + %editc(NrId:'X');
040200160617           §781out = 'SX' + %subst(%editc(NrId:'X'):3:5) +
040300160617                     %editc(Filiale:'X');
040400160617           §781flocsi = 'S';
040500160617           §781floela = 'P';
040600160617           §781flo = TIS781DFLO;
040700160617
040800160617           feod TIVGD00F;
040900160617
041000160617           tis781c1 (P781esito:tis781ds:P781prg);
041100160617
041200160617         ENDIF;
041300160617
041400160617         wStaccaId = *off;
041500160617         OkAlmenoUno = *off;
041600160617
041700160617       ENDSR;
041800160617
041900160617       //--------------------------------------------------------------
042000160617       //?Operazioni finali.
042100160617       //--------------------------------------------------------------
042200160617       BEGSR RoutEnd;
042300160617
042400160617         *inLR = *on;
042500160617         return;
042600160617
042700160617       ENDSR;
042800160617
042900160617      /end-free
