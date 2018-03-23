000100140514      //--------------------------------------------------------------
000200140923      //?ACPO11R - Potenziali eliminabili
000300140514      //--------------------------------------------------------------
000400140514
000500140514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140514
000700140514      //---------------------------------------------------------------
000800140514      //?Dichiarazione file.
000900140514      //---------------------------------------------------------------
001000140811      // - File Attività
001100140811     fTIATC05L  uf a e           k disk
001200140811
001300140811      // - File Potenziali
001400140811     fTNCPO01L  uf   e           k disk
001500140923     fTNCPO11L  if   e           k disk
001600140514
001700140808      // - File di stampa
001800140923     fACPO11P   o    e             printer oflind(*in90)
001900140514      //---------------------------------------------------------------
002000140514      //?Definizione costanti.
002100140514      //---------------------------------------------------------------
002200140514
002300140514      //---------------------------------------------------------------
002400140514      //?Definizione schiere.
002500140514      //---------------------------------------------------------------
002600140514
002700140514      //---------------------------------------------------------------
002800140514      //?Definizione aree dati.
002900140514      //---------------------------------------------------------------
003000140514      // - Dati utente
003100140514     d §AzUte        e ds                  extname(AZUTE00F)
003200140514     d                                     dtaara
003300140514     d §DatiUte      e ds                  extname(dDatiUte)
003400140514     d                                     dtaara
003500140514
003600140514      //---------------------------------------------------------------
003700140514      //?Definizione strutture dati.
003800140514      //---------------------------------------------------------------
003900140514
004000140514      // - Parametri ricevuti
004100140514     d KPJBA         e ds
004200140514
004300140514      // - Reperimento dati utente
004400140514     d TIBS34DS      e ds                  inz qualified
004500140811
004600140811      // -  Gestione Note clienti/potenziali
004700140811     d TRMK20ds      e ds                  inz
004800140811
004900140811      // - Ricerca ultimo numero attività
005000140811     d TRUL33ds      e ds                  inz
005100140923
005200140923      // - File Potenziali
005300140923     d TNCPOds       e ds                  extname(TNCPO00F)
005400140923     d                                     prefix(s_)
005500140811
005600140811      // - Campo ATCflo
005700140811     d dATC01        e ds                  inz
005800140811
005900140811      // - Campo CPOrst
006000140811     d dCPO01        e ds                  inz qualified
006100140514
006200140514      //---------------------------------------------------------------
006300140514      //?Definizione variabili globali.
006400140514      //---------------------------------------------------------------
006500140514      // - Flags booleani
006600140811     d NoTestata       s               n   inz(*off)
006700140923     d OkAtc21         s               n   inz(*off)
006800140923     d wEndAtc         s               n   inz(*off)
006900140808     d wEoF            s               n   inz(*off)
007000140514
007100140514      // - Campi di comodo data
007200140808     d wData_EUR       s               d   datfmt(*eur)
007300140808     d wData_ISO       s               d   datfmt(*iso)
007400140514     d wOggi           s              8  0 inz
007500140923     d wOggiwork       s              8  0 inz
007600140923     d wDataAcq        s              8  0 inz
007700140514
007800140514      // - Campi di comodo
007900140808     d wTpEla          s              1a   inz
008000140514
008100140514      //---------------------------------------------------------------
008200140514      //?Definizione procedure usate.
008300140514      //---------------------------------------------------------------
008400140514
008500140514      //---------------------------------------------------------------
008600140807      //?Definizione prototipi.
008700140514      //---------------------------------------------------------------
008800140514      /copy gaitrasrc/srcprotopr,TIBS34R
008900140811      /copy gaitrasrc/srcprotopr,TRMK20R
009000140811      /copy gaitrasrc/srcprotopr,TRUL33R
009100140514
009200140514      //---------------------------------------------------------------
009300140514      //?Definizione key-list.
009400140514      //---------------------------------------------------------------
009500140514
009600140514      //---------------------------------------------------------------
009700140514      //?M A I N - L I N E
009800140514      //---------------------------------------------------------------
009900140514
010000140514     c     *entry        plist
010100140514     c                   parm                    KPJBA
010200140514
010300140514      /free
010400140514
010500140514       //?Operazioni iniziali
010600140514       exsr RoutInz;
010700140514
010800140808       //?Elaborazioni
010900140808       exsr Conta;
011000140514
011100140514       //?Operazioni finali
011200140514       exsr RoutEnd;
011300140514
011400140514       //--------------------------------------------------------------
011500140514       //?Operazioni iniziali.
011600140514       //--------------------------------------------------------------
011700140514       BEGSR RoutInz;
011800140808
011900140808         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012000140808
012100140808       //?Imposto il tipo di elaborazione da fare
012200140808       //?Se blank metto di dft il conteggio
012300140923       //?C=Conteggio dei rcd da aggiornare
012400140808       //?A=Mette i potenziali a stato ELIMINABILI
012500140808         IF  %subst(kpjbu:1:1) = *blanks;
012600140808           wTpEla = 'C';
012700140808         ELSE;
012800140808           wTpEla = %subst(kpjbu:1:1);
012900140808         ENDIF;
013000140923         IF  wTpEla <> 'C' and wTpEla <> 'A';
013100140808           clear wTpEla;
013200140808         ENDIF;
013300140808
013400140808         clear StpTpEla;
013500140808         IF  wTpEla = 'C';
013600140808           StpTpEla = 'Conteggio dei potenziali';
013700140808         ENDIF;
013800140808         IF  wTpEla = 'A';
013900140808           StpTpEla = 'Aggiorna pot. Eliminabili';
014000140808         ENDIF;
014100080206
014200100326       //?Reperimento dati job
014300080206         exsr DatiJob;
014400140808
014500140808       //?Data e ora sistema
014600140808         wOggi = %dec(%date());
014700140808         wData_ISO = %date(wOggi:*ISO);
014800140808         wData_EUR = wData_ISO;
014900140808         DataEla   = %dec(wData_EUR);
015000140808         OraEla    = %dec(%time());
015100140923
015200140924         wOggiwork = wOggi;
015300141027         wDataAcq  = 20140814;
015400140808
015500140808       //?Stampo testata
015600140923         write ACPO11T0;
015700140514
015800140514       ENDSR;
015900140514
016000140514       //--------------------------------------------------------------
016100140514       //?Reperimento Dati del job.
016200140514       //--------------------------------------------------------------
016300140514       BEGSR DatiJob;
016400140514
016500140514         in(E) §AzUte;
016600140514         IF  not %error;
016700140514           in(E) §DatiUte;
016800140514         ENDIF;
016900140514         IF  %error or rsut = *blanks;
017000140514           clear TIBS34ds;
017100140514           tibs34r(tibs34ds);
017200140514           in §AzUte;
017300140514           in §DatiUte;
017400140514         ENDIF;
017500140514
017600140514       ENDSR;
017700140808
017800140808       //--------------------------------------------------------------
017900140808       //?Conto i potenziali.
018000140808       //--------------------------------------------------------------
018100140808       BEGSR Conta;
018200140923
018300140923         wEoF = *off;
018400140808
018500140923       //?Conto quanti potenziali Mai contattati e Mai codificati
018600140923       //?sono stati aggiornati e hanno il numero di telefono
018700140808         exec sql
018800140923         DECLARE A1 cursor for
018900140923         SELECT  TNCPO00F.*
019000140923                 FROM TNCPO00F join TNCPO10F
019100140923                 on CPOcpo = CPO1cpo
019200140923         WHERE   CPOfls = 'M' and
019300140923                (substr(CPOrst, 13, 8) = '        ' or
019400140923                 substr(CPOrst, 13, 8) = '00000000') and
019500140923                 CPO1tel <> '                ' and
019600140923                 CPOdaq <> :wDataAcq and
019700140923                 CPOdtr  = :wOggiwork
019800140923         ORDER BY CPOcpo;
019900140811
020000140811         exec sql
020100140811         OPEN A1;
020200140811         IF  sqlcode < 0;
020300140811           wEoF = *on;
020400140811         ENDIF;
020500140811
020600140811         DOW  not wEoF;
020700140811           exec sql
020800140923           FETCH NEXT from A1 into :TNCPODS;
020900140811           IF  sqlcod = 100 or sqlcod < 0;
021000140811             wEoF = *on;
021100140811             leave;
021200140811           ENDIF;
021300140923
021400140923         //?Totale potenziali da aggiornare
021500140923           totpot += 1;
021600140811
021700140923         //?di cui con attività ancora da eseguire = 21
021800140923           chain (s_CPOcpo:0:0) TIATC05L;
021900140811           IF  %found(TIATC05L) and
022000140923               ATCcad = '21';
022100140923             totatt += 1;
022200140811           ENDIF;
022300140923
022400140923         //?Se richiamato per aggiornamento eseguo
022500140923           IF  wTpEla = 'A';
022600140923             exsr Aggiorna;
022700140923           ENDIF;
022800140811
022900140811         ENDDO;
023000140811
023100140811         exec sql
023200140811         CLOSE A1;
023300140923
023400140923         wEoF = *off;
023500140811
023600140923       //?Conto quanti potenziali Mai contattati e Mai codificati
023700140923       //?non hanno il numero di telefono
023800140923         exec sql
023900140923         DECLARE A2 cursor for
024000140923         SELECT  *
024100140923                 FROM TNCPO00F
024200140923         WHERE   CPOfls = 'M' and
024300140923                (substr(CPOrst, 13, 8) = '        ' or
024400140923                 substr(CPOrst, 13, 8) = '00000000')
024500140923         ORDER BY CPOcpo;
024600140923
024700140923         exec sql
024800140923         OPEN A2;
024900140923         IF  sqlcode < 0;
025000140923           wEoF = *on;
025100140923         ENDIF;
025200140923
025300140923         DOW  not wEoF;
025400140923           exec sql
025500140923           FETCH NEXT from A2 into :TNCPODS;
025600140923           IF  sqlcod = 100 or sqlcod < 0;
025700140923             wEoF = *on;
025800140923             leave;
025900140923           ENDIF;
026000140923
026100140923         //?Controllo se hanno il telefono
026200140923           chain s_CPOcpo TNCPO11L;
026300140923           IF  (%found(TNCPO11L) and CPO1tel = *blanks) or
026400140923                not %found(TNCPO11L);
026500140923
026600140923           //?Totale potenziali da rendere eliminabile
026700140923             totpote += 1;
026800140923
026900140923           //?Se richiamato per aggiornamento eseguo
027000140923             IF  wTpEla = 'A';
027100140923               exsr Elimina;
027200140923             ENDIF;
027300140923
027400140923           ENDIF;
027500140923
027600140923         ENDDO;
027700140923
027800140923         exec sql
027900140923         CLOSE A2;
028000140811
028100140808       //?Stampo il conteggio
028200140923         write ACPO11D1;
028300140808
028400140808       ENDSR;
028500140808
028600140923       //--------------------------------------------------------------
028700140923       //?Aggiorno i potenziali.
028800140923       //--------------------------------------------------------------
028900140923       BEGSR Aggiorna;
029000140923
029100140923       //?Cancello le eventuali attività 21 ancora da eseguire
029200140923         wEndAtc = *off;
029300140923         setll (s_CPOcpo:0:0) TIATC05L;
029400140923         reade (s_CPOcpo:0:0) TIATC05L;
029500140923         DOW  not wEndAtc;
029600140923           IF  %eof(TIATC05L);
029700140923             leave;
029800140923           ENDIF;
029900140923           IF  ATCcad = '21';
030000140923             delete TIATC000;
030100140923           ENDIF;
030200140923
030300140923           reade (s_CPOcpo:0:0) TIATC05L;
030400140923         ENDDO;
030500140923
030600140923       //?Aggiorno la data acquisizione
030700140923         chain s_CPOcpo TNCPO01L;
030800140923         IF  %found(TNCPO01L);
030900140923           CPOdaq = 20140814;
031000140923           update TNCPO000;
031100140923         ENDIF;
031200140923
031300140923       ENDSR;
031400140811
031500140811       //--------------------------------------------------------------
031600140811       //?Elimino i potenziali.
031700140811       //--------------------------------------------------------------
031800140811       BEGSR Elimina;
031900140811
032000140923       //?Aggancio il potenziale
032100140923         chain s_CPOcpo TNCPO01L;
032200140923         IF  not %found(TNCPO01L);
032300140923           leavesr;
032400140923         ENDIF;
032500140923
032600140923         dCPO01 = CPOrst;
032700140923
032800140923       //?controllo se ha attività 21 ancora da eseguire
032900140923         okATC21 = *off;
033000140923         setll (CPOcpo:0:0) TIATC05L;
033100140923         reade (CPOcpo:0:0) TIATC05L;
033200140923         DOW  not %eof(TIATC05L);
033300140923
033400140923           IF  ATCcad = '21';
033500140923             okATC21 = *on;
033600140923
033700140923             dATC01 = ATCflo;
033800140923
033900140923           //?Chiudo l'attività ancora aperta per pot.eliminabile
034000140923             ATCdco  = %dec(%date());
034100140923             ATChco  = %dec(%time());
034200140923             ATCcco  = 0460000;
034300140923             ATCpru  = 'BATCH';
034400140923             ATCcac  = '15';
034500140923
034600140923           //?Memorizzo la categoria del potenziale alla chiusura dell'attività
034700140923             §ATCcapo2 = CPOfls;
034800140923             ATCflo = dATC01;
034900140923             update TIATC000;
035000140923
035100140923           //?Scrivo la nota richiesta obbligatoria della causale
035200140923             exsr Nota;
035300140923
035400140923           ENDIF;
035500140923
035600140923           reade (CPOcpo:0:0) TIATC05L;
035700140923         ENDDO;
035800140911
035900140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
036000140923         IF  not OkAtc21;
036100140923           exsr NewAttivita;
036200140923         //?Memorizzo che potenziale mai contattato
036300140923           ATCflo = dATC01;
036400140923           write TIATC000;
036500140811
036600140923         //?Scrivo la nota richiesta obbligatoria della causale
036700140923           exsr Nota;
036800140923         ENDIF;
036900140811
037000140811       //?Metto il potenziale in categoria "E" --> Eliminabile
037100140811         CPOfls = 'E';
037200140923       //?Imposto la data primo contatto
037300140923         dcpo01.§CPOdtpcon = %editc(woggi:'X');
037400140923         CPOrst = dCPO01;
037500140911       //?Imposto la data primo contatto
037600140811         update TNCPO000;
037700140811
037800140811       ENDSR;
037900140923
038000140911       //--------------------------------------------------------------
038100140911       //?Scrivo nuova attività.
038200140911       //--------------------------------------------------------------
038300140911       BEGSR NewAttivita;
038400140911
038500140911       //?Stacco nuovo numero attività
038600140911         exsr StaccaNumero;
038700140911       //?se errore vado subito a fine
038800140911         IF  O33Err <> 0;
038900140911           leavesr;
039000140911         ENDIF;
039100140911
039200140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
039300140911         clear TIATC000;
039400140911
039500140911         ATCtat  = 'T';
039600140911         ATCatn  = O33Nrf;
039700140911         ATCatnp = 1;
039800140911         ATCcpo  = CPOcpo;
039900140911
040000140911         ATCcad  = '60';
040100140911         ATCdad  = %dec(%date());
040200140911         ATChda  = %dec(%time());
040300140911
040400140911         ATCcmm = 0460000;
040500140911
040600140911       //?Scrivo l'attività già chiusa per pot.eliminabile
040700140911         ATCdco  = %dec(%date());
040800140911         ATChco  = %dec(%time());
040900140911         ATCcco  = ATCcmm;
041000140911         ATCpru  = 'BATCH';
041100140911         ATCcac  = '15';
041200140911
041300140911         ATCdim  = %dec(%date());
041400140911         ATChim  = %dec(%time());
041500140911         ATCpri  = 'BATCH';
041600140911
041700140911       //?Attività CREATA potenziale
041800140911         ATCcnw = 'P';
041900140911
042000140911       //?Memorizzo la categoria del potenziale
042100140911         §ATCcapo1 = CPOfls;
042200140911         §ATCcapo2 = CPOfls;
042300140911
042400140911       ENDSR;
042500140811
042600140811       //--------------------------------------------------------------
042700140811       //?Stacco nuovo numero attività.
042800140811       //--------------------------------------------------------------
042900140911       BEGSR StaccaNumero;
043000140811
043100140811         clear TRUL33ds;
043200140811         I33tla = 'L';
043300140811         I33ope = 0;
043400140811         I33cnu = 070;
043500140811         I33num = 1;
043600140811         I33aaa = *YEAR;
043700140811         kpjbu  = TRUL33ds;
043800140811         trul33r (kpjba);
043900140811         TRUL33ds = kpjbu;
044000140811
044100140811       ENDSR;
044200140911
044300140911       //--------------------------------------------------------------
044400140911       //?Scrivo la nota legata all'attività.
044500140911       //--------------------------------------------------------------
044600140911       BEGSR Nota;
044700140911
044800140911         clear TRMK20ds;
044900140911         IMK20tla  = 'L';
045000140911         IMK20flm  = 'C';
045100140911         IMK20fno  = 'S';
045200140911         IMK20cpo  = ATCcpo;
045300140911         IMK20ksc  = ATCksc;
045400140911         IMK20tat  = ATCtat;
045500140911         IMK20atn  = ATCatn;
045600140911         IMK20atnp = ATCatnp;
045700140923         EMK20no1 = 'Mancano dati per contatto';
045800140911         trmk20r (kpjba : TRMK20ds);
045900140911       ENDSR;
046000140514
046100140514       //--------------------------------------------------------------
046200140514       //?Operazioni finali.
046300140514       //--------------------------------------------------------------
046400140514       BEGSR RoutEnd;
046500140808
046600140808       //?Stampo la fine elaborazione
046700140923         write ACPO11E;
046800140514
046900140514         *inLR = *on;
047000140514         return;
047100140514
047200140514       ENDSR;
047300140514
047400140514      /end-free
