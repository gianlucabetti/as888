000100100413      //---------------------------------------------------------------
000200100421      //?TNVRCRM4 - Da nuovo TICPS genero attività
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000100415     fCNACO16L  if   e           k disk
001100100415     fTABEL00F  if   e           k disk
001200100415     fTIATC00F  o    e             disk
001300100422     fTICPS01L  if   e           k disk    prefix(s_)
001400100422     fTIVIS05L  if   e           k disk
001500100421     fTNCPO01L  if   e           k disk
001600100414     fTNVRCRMP  o    e             printer oflind(*in99)
001700100413
001800100413      //---------------------------------------------------------------
001900100413      //?Definizione costanti.
002000100413      //---------------------------------------------------------------
002100100413
002200100413      //---------------------------------------------------------------
002300100413      //?Definizione schiere.
002400100413      //---------------------------------------------------------------
002500090715
002600100413      //---------------------------------------------------------------
002700100413      //?Definizione aree dati.
002800100413      //---------------------------------------------------------------
002900100413
003000100413      //---------------------------------------------------------------
003100100413      //?Definizione strutture dati.
003200100413      //---------------------------------------------------------------
003300100415
003400100415     d KPJBA         e ds
003500100421
003600100421      // File stati potenziali
003700100421     d TICPSds       e ds                  extname(TICPS00F)
003800100415
003900100415      // - Reperimento filiali
004000100415     d TRUL31DS      e ds
004100100415     d pog                    10    759  0 dim(250)
004200100415
004300100415      // - Ricerca ultimo numero attività
004400100415     d TRUL33ds      e ds                  inz
004500100415
004600100415      // - Tabella 01 = Codici Commerciali
004700100415     d ds01          e ds                  inz
004800100422
004900100422      // - DS per MMGG  ggi
005000100422     d wOggiMMGG       ds
005100100422     d wOggiMM                        2  0
005200100422     d wOggiGG                        2  0
005300100422
005400100422      // - DS per MMGG data stato
005500100422     d wDstMMGG        ds
005600100422     d wDstMM                         2  0
005700100422     d wDstGG                         2  0
005800100415
005900100413      //---------------------------------------------------------------
006000100413      //?Definizione variabili globali.
006100100413      //---------------------------------------------------------------
006200100413
006300100413      // - Flags booleani
006400100427     d $Del            s               n   inz(*off)
006500100413     d $End            s               n   inz(*off)
006600100413     d $Fine           s               n   inz(*off)
006700100422     d $NoVis          s               n   inz(*off)
006800100414     d $Stampa         s               n   inz(*off)
006900100421
007000100421      // - Parametro ricevuto
007100100421     d filiale         s              3
007200100413
007300100413      // - Campi di comodo
007400100422     d old_CPScpo      s                   like(CPScpo)
007500100415     d wCad            s                   like(ATCcad)
007600100422     d wCmm            s                   like(ATCcmm)
007700100415     d wDad            s                   like(ATCdad)
007800100416     d wData           s              8  0
007900100415     d wDataOggi       s              8  0
008000100427     d wDataOggi2      s              8  0
008100100414     d wData_ISO       s               d   datfmt(*iso)
008200100422     d wFlo            s              1
008300100427     d wgg             s              2  0 inz(20)
008400100415     d wRiga           s              2  0
008500100415     d wTat            s                   like(ATCtat)
008600100413
008700100413      //---------------------------------------------------------------
008800100413      //?Definizione procedure esterne.
008900100413      //---------------------------------------------------------------
009000100413
009100100413      //---------------------------------------------------------------
009200100413      //?prototipi
009300100413      //---------------------------------------------------------------
009400100414
009500100422      /copy gaitrasrc/srcprotopr,trul31r
009600100415      /copy gaitrasrc/srcprotopr,trul33r
009700100413
009800100413      //---------------------------------------------------------------
009900100413      //?Definizione key-list.
010000100413      //---------------------------------------------------------------
010100100415
010200100415       // - File TABEL00F
010300100415     d k03tabel      e ds                  extname(TABEL00F:*key)
010400100415     d                                     prefix(k_)
010500100413
010600100413      //---------------------------------------------------------------
010700100413      //?Riepilogo indicatori.
010800100413      //---------------------------------------------------------------
010900100414
011000100414      // 99    : Salto pagina
011100100413
011200100413      //---------------------------------------------------------------
011300100413      //?M A I N - L I N E
011400100413      //---------------------------------------------------------------
011500100413
011600100413     c     *Entry        plist
011700100413     c                   parm                    filiale
011800100413
011900100413      /free
012000100413
012100100413       //?Operazioni iniziali
012200100413       exsr sr_RoutInz;
012300100413
012400100413       //?Scrivo i nuovi file per le trattative
012500100413       IF  not $Fine;
012600100413         exsr sr_Elabora;
012700100413       ENDIF;
012800100413
012900100413       //?Operazioni finali
013000100413       exsr sr_RoutEnd;
013100100413
013200100413       //--------------------------------------------------------------
013300100413       //?Operazioni iniziali.
013400100413       //--------------------------------------------------------------
013500100413       BEGSR sr_RoutInz;
013600100422
013700100422       //?Recupero filiali del 'RA' della filiale ricevuta
013800100422         IF  filiale <> *blanks;
013900100422           clear TRUL31DS;
014000100422           I31abi = 'RA';
014100100422           I31cpo = %dec(filiale:3:0);
014200100422           callp trul31r (kpjba : trul31ds);
014300100422           IF  O31pog <= *zeros;
014400100422             $Fine = *on;
014500100422           ENDIF;
014600100422         //?imposto flag x riconoscere gli stati che ho trascodificato nel pgm precedente
014700100422           wFlo = 'B';
014800100422         ENDIF;
014900100422
015000100422         //?imposto flag x riconoscere gli stati che ho trascodificato nel pgm precedente
015100100422         IF  filiale = *blanks;
015200100422           wFlo = 'T';
015300100422         ENDIF;
015400100414
015500100414         wData_ISO = %date();
015600100415         wDataOggi = %dec(wData_ISO);
015700100422         wOggiMM   = %subdt(wData_ISO:*months);
015800100422         wOggiGG   = %subdt(wData_ISO:*days);
015900100427         wDataOggi2= %dec(wData_ISO + %days(wgg));
016000100413
016100100413         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
016200100416
016300100416       //?Impostazione campi "fissi"
016400100416         k_TBLkut = 1;
016500100413
016600100413       ENDSR;
016700100413
016800100413       //--------------------------------------------------------------
016900100422       //?Elaboro.
017000100413       //--------------------------------------------------------------
017100100413       BEGSR sr_Elabora;
017200100413
017300100413         $End = *off;
017400100422         clear old_CPScpo;
017500100413
017600100413         exec sql
017700100421         DECLARE CPS cursor for
017800100422         SELECT  *
017900100422         FROM TICPS00F
018000100422         WHERE substr(CPSflo, 1, 1) = :wFlo
018100100422         ORDER by CPScpo, CPSdst desc;
018200100413
018300100421         exec sql OPEN CPS;
018400100413
018500100413         DOW  not $End;
018600100421           exec sql FETCH next from CPS into :TICPSds;
018700100413           IF  sqlcod = 100 or sqlcod < 0;
018800100413             $End = *on;
018900100413             leave;
019000100413           ENDIF;
019100100422
019200100422         //?Faccio tutto a rottura di potenziale
019300100422           IF  CPScpo <> old_CPScpo;
019400100426
019500100426             old_CPScpo = CPScpo;
019600100422
019700100422           //?Se potenziale non esiste stampo errore e non faccio niente
019800100422             chain CPScpo TNCPO01L;
019900100422             IF  not %found(TNCPO01L);
020000100422               STPcpo = CPScpo;
020100100422               wRiga = 04;
020200100422               exsr sr_Stampa;
020300100422              iter;
020400100422             ENDIF;
020500100413
020600100422           //?Se potenziale mai codificato
020700100422           //?devo pianificare nuova attività in base all'ultimo stato
020800100422             chain CPScpo CNACO16L;
020900100422             IF  not %found(CNACO16L);
021000100422             //?L'attività la scrivo solo se non esiste già uno stato '2' sul nuovo file stati
021100100422             //?se non c'è già un nuovo stato '2' sul potenziale
021200100422               chain (CPScpo : '2 ') TICPS01L;
021300100422               IF  not %Found(TICPS01L);
021400100422               //?scrivo nuovo file attività
021500100422                 exsr Scrivi;
021600100422               ENDIF;
021700100422             ENDIF;
021800100422
021900100422           ENDIF;
022000100422
022100100413         ENDDO;
022200100413
022300100421         exec sql CLOSE CPS;
022400100413
022500100413       ENDSR;
022600100413
022700100413       //--------------------------------------------------------------
022800100413       //?Scrivo i nuovi file.
022900100413       //--------------------------------------------------------------
023000100413       BEGSR Scrivi;
023100100422
023200100422         $NoVis = *off;
023300100422         clear wCad;
023400100422         clear wCmm;
023500100422         clear wDad;
023600100422         clear wTat;
023700100422
023800100422       //?Controllo se la trattativa che c'è sullo stato esiste già sul nuovo file
023900100422       //?oppure è chiusa o fittizia
024000100422         chain CPSnrv TIVIS05L;
024100100422         IF  not %Found(TIVIS05L) or VISdch > 0 or VISffz = 'S';
024200100422           $NoVis = *on;
024300100422         ENDIF;
024400100415
024500100422       //?se primo giro di trascodifica
024600100422         IF  filiale <> *blanks;
024700100422         //?controllo se il commerciale dello stato fa parte dell'area
024800100422           IF  %lookup(%dec(%subst(%editc(CPScmm:'X'):1:3):3:0):pog) > 0;
024900100422           //?fa parte dell'area creo attività
025000100422             wCmm = CPScmm;
025100100422           //?Stacco nuovo numero attività
025200100422             exsr Stacca_Numero;
025300100422           //?Scrivo la nuova attività
025400100422             exsr Scrivi_TIATC;
025500100422           ELSE;
025600100422           //?non fa parte dell'area
025700100422           //?stampo
025800100422             wRiga = 05;
025900100422             exsr sr_Stampa;
026000100422           //?controllo il commerciale del potenziale
026100100422             IF  %lookup(%dec(%subst(%editc(CPOcmm:'X'):1:3):3:0):pog) > 0;
026200100422             //?fa parte dell'area creo attività
026300100422               wCmm = CPOcmm;
026400100422             //?Stacco nuovo numero attività
026500100422               exsr Stacca_Numero;
026600100422             //?Scrivo la nuova attività
026700100422               exsr Scrivi_TIATC;
026800100422             ELSE;
026900100422             //?non fa parte dell'area stampo e non creo attività
027000100422               wRiga = 06;
027100100422               exsr sr_Stampa;
027200100422               leavesr;
027300100422             ENDIF;
027400100422           ENDIF;
027500100422         ENDIF;
027600100422
027700100422       //?non è il primo giro di trascodifica
027800100422       //?non controllo il commerciale
027900100422         IF  filiale = *blanks;
028000100422         //?Stacco nuovo numero attività
028100100422           exsr Stacca_Numero;
028200100422         //?Scrivo la nuova attività
028300100422           exsr Scrivi_TIATC;
028400100422         ENDIF;
028500100413
028600100413       ENDSR;
028700100415
028800100415       //--------------------------------------------------------------
028900100415       //?Stacco nuovo numero attività.
029000100415       //--------------------------------------------------------------
029100100415       BEGSR Stacca_Numero;
029200100415
029300100415         clear TRUL33ds;
029400100415         I33tla = 'L';
029500100415         I33ope = 0;
029600100415         I33cnu = 070;
029700100415         I33num = 1;
029800100415         I33aaa = *year;
029900100415         kpjbu  = TRUL33ds;
030000100415         trul33r (kpjba);
030100100415         TRUL33ds = kpjbu;
030200100415
030300100415       ENDSR;
030400100415
030500100415       //--------------------------------------------------------------
030600100415       //?Scrivo file TIATC.
030700100415       //--------------------------------------------------------------
030800100415       BEGSR Scrivi_TIATC;
030900100422
031000100422       //?Imposto il tipo e la causale attività in base allo stato
031100100427         SELECT;
031200100427           WHEN  CPSfst = 'A0' or CPSfst = 'A1';
031300100427             wTat = 'T';
031400100427             wCad = '60';
031500100427           WHEN  $NoVis and (CPSfst = 'A2' or CPSfst = 'A3');
031600100427             wTat = 'T';
031700100427             wCad = '60';
031800100423         //?Stampo e non scrivo nessuna attività
031900100427           WHEN  CPSfst = 'A4';
032000100427             wRiga = 07;
032100100427             exsr sr_Stampa;
032200100427             wTat = 'T';
032300100427             wCad = '60';
032400100427           WHEN  CPSfst = 'A7';
032500100427             wRiga = 07;
032600100427             exsr sr_Stampa;
032700100427             wTat = 'T';
032800100427             wCad = '60';
032900100427           WHEN  CPSfst = 'A8';
033000100427             wTat = 'T';
033100100427             wCad = '60';
033200100427           WHEN  CPSfst = 'A5' or CPSfst = 'A6' or CPSfst = 'A9';
033300100427             wTat = 'T';
033400100427             wCad = '24';
033500100427          ENDSL;
033600100422
033700100422       //?Imposto la data
033800100427         SELECT;
033900100427           WHEN  CPSdst >= wDataOggi;
034000100427             wDad = CPSdst;
034100100427           WHEN  CPSdst > %dec(%date(wDataOggi) - %months(2));
034200100427             wDad = %dec(%date(CPSdst) + %months(3));
034300100427           OTHER;
034400100427             wData_ISO = %date(CPSdst);
034500100427             wDstMM    = %subdt(wData_ISO:*months);
034600100427             wDstGG    = %subdt(wData_ISO:*days);
034700100427             IF  wDstMMGG < wOggiMMGG;
034800100427               IF  filiale <> *blanks;
034900100427                 wData_ISO = wData_ISO + %months(6);
035000100427                 wDstMM    = %subdt(wData_ISO:*months);
035100100427                 wDstGG    = %subdt(wData_ISO:*days);
035200100427                 // wDstMM += 6;
035300100427               ELSE;
035400100427                 wData_ISO = wData_ISO + %months(7);
035500100427                 wDstMM    = %subdt(wData_ISO:*months);
035600100427                 wDstGG    = %subdt(wData_ISO:*days);
035700100427                 // wDstMM += 7;
035800100427               ENDIF;
035900100427             ENDIF;
036000100427               wDad = 20100000 + (wDstMM *100) + wDSTGG;
036100100427         ENDSL;
036200100415
036300100415         clear TIATC000;
036400100415         ATCtat  = wTat;
036500100415         ATCatn  = O33Nrf;
036600100415         ATCatnp = 1;
036700100422         ATCcpo  = CPScpo;
036800100415         ATCcad  = wCad;
036900100415         ATCdad  = wDad;
037000100422         ATChda  = 103000;
037100100422         ATCcmm  = wCmm;
037200100422         ATCco3  = wCmm;
037300100422         ATCcnw = 'P';
037400100415         ATCdim  = %dec(%date());
037500100415         ATChim  = %dec(%time());
037600100415         ATCpri  = 'BATCH';
037700100415         write TIATC000;
037800100415
037900100415       ENDSR;
038000100414
038100100414       //--------------------------------------------------------------
038200100414       //?Stampa errori.
038300100414       //--------------------------------------------------------------
038400100414       BEGSR sr_Stampa;
038500100414
038600100414         IF  not $Stampa;
038700100414           write CRMT01;
038800100414           $Stampa = *on;
038900100426           DESpgm = 'Scrive attività da nuovi stati potenziali';
039000100426           write CRMT02;
039100100414         ENDIF;
039200100414
039300100422         IF  wRiga = 04;
039400100426           STPdes = 'Stato senza file TNCPO non creo attività.';
039500100422           write CRMD04;
039600100415         ENDIF;
039700100422
039800100422         IF  wRiga = 05;
039900100422           write CRMD05;
040000100422         ENDIF;
040100100422
040200100422         IF  wRiga = 06;
040300100422           write CRMD06;
040400100422         ENDIF;
040500100422
040600100422         IF  wRiga = 07;
040700100422           write CRMD07;
040800100422         ENDIF;
040900100414
041000100414       ENDSR;
041100100413
041200100413       //--------------------------------------------------------------
041300100413       //?Operazioni finali.
041400100413       //--------------------------------------------------------------
041500100413       BEGSR sr_RoutEnd;
041600100413
041700100413         *inLR = *on;
041800100413         return;
041900100413
042000100413       ENDSR;
042100100413
042200100413      /end-free
