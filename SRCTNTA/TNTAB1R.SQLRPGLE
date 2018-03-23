000100160916      //--------------------------------------------------------------
000200160916      //?TNTAB1R - Duplica tariffe DPD             - batch -
000300160916      //--------------------------------------------------------------
000400160916     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160916
000600160916      //---------------------------------------------------------------
000700160916      //?Dichiarazione file.
000800160916      //---------------------------------------------------------------
000900160916      // - Organigramma
001000160916     fAZORG01L  if   e           k disk
001100160916
001200160916      // - Tariffe
001300160916     fTNTAM01L  uf   e           k disk    rename(TNTAM000:TNTAM01)
001400160916     f                                     prefix(w)
001500160916     fTNTAM04L  if   e           k disk
001600160916
001700160916      // - File di Stampa
001800160916     fTNTAB1P   o    e             printer oflind(*in90) usropn
001900160916
002000160916      //---------------------------------------------------------------
002100160916      //?Definizione costanti.
002200160916      //---------------------------------------------------------------
002300160916
002400160916      //---------------------------------------------------------------
002500160916      //?Definizione schiere.
002600160916      //---------------------------------------------------------------
002700160916      // - Filiali DPD presenti in organigramma
002800160916     d skFilDPD        s              3  0 dim(250)
002900160916
003000160916      // - Clienti da non duplicare
003100160916     d skNoKSC         s              7  0 dim(11)
003200160916
003300160916      //---------------------------------------------------------------
003400160916      //?Definizione aree dati.
003500160916      //---------------------------------------------------------------
003600160916      // - Dati utente
003700160916     d 你zUte        e ds                  extname(AZUTE00F)
003800160916     d                                     dtaara
003900160916     d 伶atiUte      e ds                  extname(dDatiUte)
004000160916     d                                     dtaara
004100160916
004200160916      //---------------------------------------------------------------
004300160916      //?Definizione strutture dati.
004400160916      //---------------------------------------------------------------
004500160916      // - Parametri ricevuti
004600160916     d KPJBA         e ds
004700160916     d TNTAB0DS      e ds
004800160916
004900160916      // - Reperimento dati utente
005000160916     d TIBS34ds      e ds
005100160916
005200160916      // - Duplica Tariffe
005300160916     d TNTA25DS      e ds
005400160916     d TNTA25TAD     e ds                  inz
005500160916     d TNTA25TGC     e ds                  inz
005600160916     d TNTA25TPD     e ds                  inz
005700160916     d TNTA25TPT     e ds                  inz
005800160916
005900160916      // - Campo ORGDE3 - AZORG00F
006000160916     d OG143         e ds
006100160916
006200160916      //---------------------------------------------------------------
006300160916      //?Definizione variabili globali.
006400160916      //---------------------------------------------------------------
006500160916      // - Flags booleani
006600160916     d Buona           s               n   inz(*off)
006700160916     d Dettaglio       s               n   inz(*off)
006800160916     d Errore          s               n   inz(*off)
006900160916     d Fine            s               n   inz(*off)
007000160916     d FineTAM         s               n   inz(*off)
007100160919     d NoTarNew        s               n   inz(*off)
007200160916
007300160916      // - Indici di schiera
007400160916     d xx              s              4s 0 inz
007500160916     d yy              s              4s 0 inz
007600160916
007700160916       // - Stringa SQL da eseguire
007800160916     d wSQL            s           2048    Varying        inz
007900160916
008000160916      // - Campi di comodo data
008100160916     d Oggi            s              8  0 inz
008200160916
008300160916      // - Campi di comodo
008400160916     d wfil            s                   like(ORGfil)
008500160916     d wprg            s                   like(TAMprg)
008600160916
008700160916      //---------------------------------------------------------------
008800160916      //?Definizione procedure usate.
008900160916      //---------------------------------------------------------------
009000160916      // - Duplica Tariffa
009100160916     d tnta25r         pr                  extpgm('TNTA25R')
009200160916     d  kpjba                              likeds(kpjba)
009300160916     d  tnta25ds                           likeds(TNTA25DS)
009400160916     d  tnta25tad                          likeds(TNTA25TAD)
009500160916     d  tnta25tgc                          likeds(TNTA25TGC)
009600160916     d  tnta25tpd                          likeds(TNTA25TPD)
009700160916     d  tnta25tpt                          likeds(TNTA25TPT)
009800160916
009900160916      //---------------------------------------------------------------
010000160916      //?Definizione prototipi usati.
010100160916      //---------------------------------------------------------------
010200160916      /copy gaitrasrc/srcprotopr,tibs34r
010300160916
010400160916      //---------------------------------------------------------------
010500160916      //?Definizione key-list.
010600160916      //---------------------------------------------------------------
010700160916      // - File TABEL00F
010800160916     d k03tabel      e ds                  extname(TABEL00F:*key)
010900160916     d                                     prefix(k_)
011000160916
011100160916      //---------------------------------------------------------------
011200160916      //?Riepilogo indicatori.
011300160916      //---------------------------------------------------------------
011400160916
011500160916      //---------------------------------------------------------------
011600160916      //?M A I N - L I N E
011700160916      //---------------------------------------------------------------
011800160916     c     *Entry        plist
011900160916     c                   parm                    KPJBA
012000160916
012100160916      /free
012200160916
012300160916       //?Operazioni iniziali
012400160916       exsr RoutInz;
012500160916
012600160916       //?Carico in SK le filiali DPD
012700160916       exsr CarSkDPD;
012800160916
012900160916       //?Elaboro leggendo i clienti DPD da CNACO
013000160916       exsr Elabora;
013100160916
013200160916       //?Operazioni finali
013300160916       exsr RoutEnd;
013400160916
013500160916       //--------------------------------------------------------------
013600160916       //?Operazioni iniziali.
013700160916       //--------------------------------------------------------------
013800160916       BEGSR RoutInz;
013900160916
014000160916         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
014100160916
014200160916       //?Reperimento dati job
014300160916         exsr DatiJob;
014400160916
014500160916       //?Importo i dati nella ds
014600160916         TNTAB0DS = kpjbu;
014700160916         skNoKSC(01) = TAB0ksc;
014800160916         skNoKSC(02) = TAB0ksc1;
014900160916         skNoKSC(03) = TAB0ksc2;
015000160916         skNoKSC(04) = TAB0ksc3;
015100160916         skNoKSC(05) = TAB0ksc4;
015200160916         skNoKSC(06) = TAB0ksc5;
015300160916         skNoKSC(07) = TAB0ksc6;
015400160916         skNoKSC(08) = TAB0ksc7;
015500160916         skNoKSC(09) = TAB0ksc8;
015600160916         skNoKSC(10) = TAB0ksc9;
015700160916         skNoKSC(11) = TAB0ksc10;
015800160916
015900160916       //?Calcolo oggi
016000160916         Oggi = %dec(%date());
016100160916
016200160916       ENDSR;
016300160916
016400160916       //--------------------------------------------------------------
016500160916       //?Reperimento Dati del job (Utente/Operativi).
016600160916       //--------------------------------------------------------------
016700160916       BEGSR DatiJob;
016800160916
016900160916         in(E) 你zUte;
017000160916         IF  NOT %error;
017100160916           in(E) 伶atiUte;
017200160916         ENDIF;
017300160916         IF  %error or Rsut = *blanks;
017400160916           clear TIBS34ds;
017500160916           tibs34r(tibs34ds);
017600160916           in 你zUte;
017700160916           in 伶atiUte;
017800160916         ENDIF;
017900160916
018000160916       ENDSR;
018100160916
018200160916       //--------------------------------------------------------------
018300160916       //?Carico le filiali DPD presenti in Organigramma.
018400160916       //--------------------------------------------------------------
018500160916       BEGSR CarSkDPD;
018600160916
018700160916       //?Carico le filiali DPD presenti sull'organigramma
018800160916         clear xx;
018900160916         clear wfil;
019000160916         setll wfil AZORG01L;
019100160916         read AZORG01L;
019200160916         DOW  not %eof(AZORG01L);
019300160916           IF  ORGfva = *blanks and
019400160916              (ORGfag = 'F' or ORGfag = 'A');
019500160916           ENDIF;
019600160916           OG143 = ORGde3;
019700160916           IF  別Gntw = 'DPD';
019800160916             xx += 1;
019900160916             skFilDPD(xx) = ORGfil;
020000160916           ENDIF;
020100160916           read AZORG01L;
020200160916         ENDDO;
020300160916
020400160916       ENDSR;
020500160916
020600160916       //--------------------------------------------------------------
020700160916       //?Elaboro leggendo i clienti DPD da CNACO.
020800160916       //--------------------------------------------------------------
020900160916       BEGSR Elabora;
021000160916
021100160916       //?Preparo Stringa Sql
021200160916           exsr PreparaSQL;
021300160916
021400160916       //?Dichiarazione cursore
021500160916           exec sql
021600160916           prepare S1 from :wSQL;
021700160916           exec sql
021800160916           DECLARE FileACO CURSOR for S1;
021900160916
022000160916         //?Apertura del cursore
022100160916           exec sql
022200160916           OPEN FileAco;
022300160916
022400160916           IF  sqlcode < 0;
022500160916             Fine = *on;
022600160916             exec sql
022700160916             CLOSE FileACO;
022800160916             leavesr;
022900160916           ENDIF;
023000160916
023100160916           DOW  not Fine;
023200160916             exec sql
023300160916             FETCH NEXT from FileACO into :ACOksc;
023400160916             IF  sqlcod = 100 or sqlcod < 0;
023500160916               Fine = *on;
023600160916               leave;
023700160916             ENDIF;
023800160916
023900160916             SELECT;
024000160916           //?Non elaboro se cliente 8888 o 9999
024100160916             WHEN  %subst(%editc(ACOksc:'X'):4:4) = '8888' or
024200160916                   %subst(%editc(ACOksc:'X'):4:4) = '9999';
024300160916           //?Non elaboro se cliente escluso
024400160916             WHEN  %lookup(ACOksc:skNoKSC) > 0;
024500160916           //?Elaboro
024600160916             OTHER;
024700160916               exsr Duplica;
024800160916             ENDSL;
024900160916
025000160916           ENDDO;
025100160916
025200160916         //?Chiusura del cursore
025300160916           exec sql
025400160916           CLOSE FileACO;
025500160916
025600160916       ENDSR;
025700160916
025800160916       //--------------------------------------------------------------
025900160916       //?Preparo la stringa SQL.
026000160916       //--------------------------------------------------------------
026100160916       BEGSR PreparaSQL;
026200160916
026300160916       //?Leggo CNACO
026400160916         wSQL = 'SELECT ACOksc from CNACO00F -
026500160916                 WHERE substr(digits(ACOksc), 1, 3) in (';
026600160916
026700160916         yy = 0;
026800160916         xx = 1;
026900160916         FOR  xx by 1 to %elem(skFilDPD);
027000160916           IF  skFilDPD(xx) <> *zeros;
027100160916             IF yy > 0;
027200160916               wSQL += ', ';
027300160916             ELSE;
027400160916               yy = 1;
027500160916             ENDIF;
027600160916             wSQL += %editc(skFilDPD(xx):'X');
027700160916           ENDIF;
027800160916         ENDFOR;
027900160916
028000160916         wSQL += ') for read only';
028100160916
028200160916       ENDSR;
028300160916
028400160916       //--------------------------------------------------------------
028500160916       //?Duplico la tariffa.
028600160916       //--------------------------------------------------------------
028700160916       BEGSR Duplica;
028800160916
028900160916         FineTAM = *off;
029000160916
029100160916       //?Cerco ultimo Progressivo esistente sul cliente di arrivo
029200160916         setll (ACOksc:TAB0ctr) TNTAM04L;
029300160916         reade (ACOksc:TAB0ctr) TNTAM04L;
029400160916         DOW  not FineTAM;
029500160916         //?Se fine file duplico (vuol dire che non ho altri progressivi)
029600160916           IF  %eof(TNTAM04L);
029700160916             exsr CallTA25;
029800160916             leave;
029900160916           ENDIF;
030000160916         //?Verifico la data scadenza dell'ultima tariffa
030100160916         //?se minore alla data scadenza della tariffa da cui copio
030200160916         //?aggiorno la data scadenza per non avere dei buchi
030300160916         //?e duplico
030400160916           IF  TAMdst < TAB0tamdst;
030500160916             exsr AggiornaTAM;
030600160916             exsr CallTA25;
030700160916             leave;
030800160916           ENDIF;
030900160916         //?Verifico la data scadenza dell'ultima tariffa
031000160916         //?se uguale alla data scadenza della tariffa da cui copio
031100160916         //?duplico
031200160916           IF  TAMdst = TAB0tamdst;
031300160916             exsr CallTA25;
031400160916             leave;
031500160916           ENDIF;
031600160916         //?Verifico la data scadenza dell'ultima tariffa
031700160916         //?se maggiore alla data scadenza della tariffa da cui copio
031800160916         //?stampo
031900160916           IF  TAMdst > TAB0tamdst;
032000160916             Dettaglio = *on;
032100160916             Errore = *off;
032200160916             Buona = *off;
032300160919             NoTarNew = *off;
032400160916             exsr Stampa;
032500160916             leave;
032600160916           ENDIF;
032700160916           reade (ACOksc:TAB0ctr) TNTAM04L;
032800160916         ENDDO;
032900160916
033000160916       ENDSR;
033100160916
033200160916       //--------------------------------------------------------------
033300160916       //?Chiamo il TNTA25R per duplicare.
033400160916       //--------------------------------------------------------------
033500160916       BEGSR CallTA25;
033600160916
033700160916         clear TNTA25DS;
033800160916         TA25ta01 = '1';
033900160916         TA25inglo = 'N';
034000160916         TA25tipo = 'T';
034100160916         TA25ksco = TAB0ksc;
034200160916         TA25ctro = TAB0ctr;
034300160916         TA25prgo = TAB0prg;
034400160916         TA25tipn = 'T';
034500160916         TA25kscn = ACOksc;
034600160916         TA25ctrn = TAB0ctr;
034700160916         TA25tam = 'S';
034800160916         TA25tad = 'T';
034900160916         TA25tpt = 'T';
035000160916         TA25tpd = 'N';
035100160916         TA25tgc = 'T';
035200160916         TA25cat = 'N';
035300160916         TA25fie = 'I';
035400160916         tnta25r
035500160916         (KPJBA:TNTA25DS:TNTA25TAD:TNTA25TGC:TNTA25TPD:TNTA25TPT);
035600160916
035700160916       //?Se errore da copia tariffa stampo
035800160916         IF  TA25err <> *blanks;
035900160916           Dettaglio = *off;
036000160916           Errore = *on;
036100160916           Buona = *off;
036200160919           NoTarNew = *off;
036300160916           exsr Stampa;
036400160916           leavesr;
036500160916         ENDIF;
036600160916
036700160916       //?Se OK da copia tariffa stampo
036800160916         IF  TA25prg <> *blanks;
036900160919         //?Devo azzerare l'ISTAT
037000160919           wprg = %dec(TA25prg:3:0);
037100160919           chain (ACOksc:TAB0ctr:wprg) TNTAM01L;
037200160919           IF  %found(TNTAM01L);
037300160919             clear wTAMpri;
037400160919             clear wTAMrct;
037500160919             update TNTAM01;
037600160919             Dettaglio = *off;
037700160919             Errore = *off;
037800160919             Buona = *on;
037900160919             NoTarNew = *off;
038000160919             exsr Stampa;
038100160919           ELSE;
038200160919             Dettaglio = *off;
038300160919             Errore = *off;
038400160919             Buona = *off;
038500160919             NoTarNew = *on;
038600160919             exsr Stampa;
038700160919           ENDIF;
038800160919         ELSE;
038900160919           Dettaglio = *off;
039000160919           Errore = *off;
039100160919           Buona = *off;
039200160919           NoTarNew = *on;
039300160919           exsr Stampa;
039400160916         ENDIF;
039500160916
039600160916       ENDSR;
039700160916
039800160916       //--------------------------------------------------------------
039900160916       //?Aggiorno TNTAM.
040000160916       //--------------------------------------------------------------
040100160916       BEGSR AggiornaTAM;
040200160916
040300160916         chain (TAMksc:TAMctr:TAMprg) TNTAM01L;
040400160916         IF  %found(TNTAM01L);
040500160916           wTAMdst = TAB0tamdst;
040600160916           wTAMduv = Oggi;
040700160916           wTAMdtr = Oggi;
040800160916           clear wTAMftr;
040900160916           update TNTAM01;
041000160916         ENDIF;
041100160916
041200160916       ENDSR;
041300160916
041400160916       //--------------------------------------------------------------
041500160916       //?Stampo.
041600160916       //--------------------------------------------------------------
041700160916       BEGSR Stampa;
041800160916
041900160916         IF  not %open(TNTAB1P);
042000160916           open TNTAB1P;
042100160916           write TAB1testa;
042200160916         ENDIF;
042300160916
042400160916         IF  *in90;
042500160916           write TAB1testa;
042600160916           *in90 = *off;
042700160916         ENDIF;
042800160916
042900160916         IF  Dettaglio;
043000160916           write TAB1dett;
043100160916         ENDIF;
043200160916
043300160916         IF  Errore;
043400160916           write TAB1err;
043500160916         ENDIF;
043600160916
043700160916         IF  Buona;
043800160916           write TAB1buona;
043900160916         ENDIF;
044000160919
044100160919         IF  NoTarNew;
044200160919           write TAB1notnew;
044300160919         ENDIF;
044400160916
044500160916       ENDSR;
044600160916
044700160916       //--------------------------------------------------------------
044800160916       //?Operazioni finali.
044900160916       //--------------------------------------------------------------
045000160916       BEGSR RoutEnd;
045100160916
045200160916         IF  %open(TNTAB1P);
045300160916           write TAB1fine;
045400160916         ENDIF;
045500160916
045600160916         *inLR = *on;
045700160916         return;
045800160916
045900160916       ENDSR;
046000160916
046100160916      /end-free
