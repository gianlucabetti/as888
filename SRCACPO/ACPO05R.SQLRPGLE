000100140514      //--------------------------------------------------------------
000200140808      //?ACPO05R - Potenziali eliminabili
000300140808      //?          WFACPE0F
000400140514      //--------------------------------------------------------------
000500140514
000600140514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700140514
000800140514      //---------------------------------------------------------------
000900140514      //?Dichiarazione file.
001000140514      //---------------------------------------------------------------
001100140811      // - File Attività
001200140811     fTIATC05L  uf a e           k disk
001300140811
001400140811      // - File Trattative
001500140811     fTIVIS01L  if   e           k disk
001600140811
001700140811      // - File Potenziali
001800140811     fTNCPO01L  uf   e           k disk
001900140811
002000140808      // - File Potenziali Eliminabili
002100140811     fWFACPE0F  o    e             disk
002200140514
002300140808      // - File di stampa
002400140808     fACPO05P   o    e             printer oflind(*in90)
002500140514      //---------------------------------------------------------------
002600140514      //?Definizione costanti.
002700140514      //---------------------------------------------------------------
002800140514
002900140514      //---------------------------------------------------------------
003000140514      //?Definizione schiere.
003100140514      //---------------------------------------------------------------
003200140514
003300140514      //---------------------------------------------------------------
003400140514      //?Definizione aree dati.
003500140514      //---------------------------------------------------------------
003600140514      // - Dati utente
003700140514     d §AzUte        e ds                  extname(AZUTE00F)
003800140514     d                                     dtaara
003900140514     d §DatiUte      e ds                  extname(dDatiUte)
004000140514     d                                     dtaara
004100140514
004200140514      //---------------------------------------------------------------
004300140514      //?Definizione strutture dati.
004400140514      //---------------------------------------------------------------
004500140514
004600140514      // - Parametri ricevuti
004700140514     d KPJBA         e ds
004800140514
004900140514      // - Reperimento dati utente
005000140514     d TIBS34DS      e ds                  inz qualified
005100140811
005200140811      // -  Gestione Note clienti/potenziali
005300140811     d TRMK20ds      e ds                  inz
005400140811
005500140811      // - Ricerca ultimo numero attività
005600140811     d TRUL33ds      e ds                  inz
005700140808
005800140808      // - File Potenziali aggiornati da CRIBIS
005900140808     d WFACPV0F      e ds                  extname(WFACPV0F)
006000140811
006100140811      // - File Potenziali eliminabili
006200140811     d WFACPEDS      e ds                  extname(WFACPE0F)
006300140811     d                                     prefix(w)
006400140811
006500140811      // - Campo ATCflo
006600140811     d dATC01        e ds                  inz
006700140811
006800140811      // - Campo CPOrst
006900140811     d dCPO01        e ds                  inz qualified
007000140514
007100140514      //---------------------------------------------------------------
007200140514      //?Definizione variabili globali.
007300140514      //---------------------------------------------------------------
007400140514      // - Flags booleani
007500140811     d NoTestata       s               n   inz(*off)
007600140911     d OkAtc           s               n   inz(*off)
007700140808     d wEoF            s               n   inz(*off)
007800140514
007900140514      // - Campi di comodo data
008000140808     d wData_EUR       s               d   datfmt(*eur)
008100140808     d wData_ISO       s               d   datfmt(*iso)
008200140514     d wOggi           s              8  0 inz
008300140514
008400140514      // - Campi di comodo
008500140808     d wTpEla          s              1a   inz
008600140514
008700140514      //---------------------------------------------------------------
008800140514      //?Definizione procedure usate.
008900140514      //---------------------------------------------------------------
009000140514
009100140514      //---------------------------------------------------------------
009200140807      //?Definizione prototipi.
009300140514      //---------------------------------------------------------------
009400140514      /copy gaitrasrc/srcprotopr,TIBS34R
009500140811      /copy gaitrasrc/srcprotopr,TRMK20R
009600140811      /copy gaitrasrc/srcprotopr,TRUL33R
009700140514
009800140514      //---------------------------------------------------------------
009900140514      //?Definizione key-list.
010000140514      //---------------------------------------------------------------
010100140514
010200140514      //---------------------------------------------------------------
010300140514      //?M A I N - L I N E
010400140514      //---------------------------------------------------------------
010500140514
010600140514     c     *entry        plist
010700140514     c                   parm                    KPJBA
010800140514
010900140514      /free
011000140514
011100140514       //?Operazioni iniziali
011200140514       exsr RoutInz;
011300140514
011400140808       //?Elaborazioni
011500140808       exsr Conta;
011600140808       IF  wTpEla <> 'C';
011700140808         exsr Scrivi;
011800140808         exsr Elimina;
011900140808       ENDIF;
012000140514
012100140514       //?Operazioni finali
012200140514       exsr RoutEnd;
012300140514
012400140514       //--------------------------------------------------------------
012500140514       //?Operazioni iniziali.
012600140514       //--------------------------------------------------------------
012700140514       BEGSR RoutInz;
012800140808
012900140808         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013000140808
013100140808       //?Imposto il tipo di elaborazione da fare
013200140808       //?Se blank metto di dft il conteggio
013300140808       //?C=Conteggio dei rcd da aggiornare e non per Situazione 1 e 2
013400140808       //?W=Crea i file dei potenziali eliminabili
013500140808       //?A=Mette i potenziali a stato ELIMINABILI
013600140808         IF  %subst(kpjbu:1:1) = *blanks;
013700140808           wTpEla = 'C';
013800140808         ELSE;
013900140808           wTpEla = %subst(kpjbu:1:1);
014000140808         ENDIF;
014100140808         IF  wTpEla <> 'C' and wTpEla <> 'W' and wTpEla <> 'A';
014200140808           clear wTpEla;
014300140808         ENDIF;
014400140808
014500140808         clear StpTpEla;
014600140808         IF  wTpEla = 'C';
014700140808           StpTpEla = 'Conteggio dei potenziali';
014800140808         ENDIF;
014900140808         IF  wTpEla = 'W';
015000140808           StpTpEla = 'Scrive File + Conteggio';
015100140808         ENDIF;
015200140808         IF  wTpEla = 'A';
015300140808           StpTpEla = 'Aggiorna pot. Eliminabili';
015400140808         ENDIF;
015500140808
015600140811       //?Se elaborazione NO = 'C' devo prima cancellare i dati dal file di work
015700140808         IF  wTpEla <> 'C';
015800140808           exec sql
015900140808           DELETE
016000140808           FROM  WFACPE0F;
016100140808         ENDIF;
016200080206
016300100326       //?Reperimento dati job
016400080206         exsr DatiJob;
016500140808
016600140808       //?Data e ora sistema
016700140808         wOggi = %dec(%date());
016800140808         wData_ISO = %date(wOggi:*ISO);
016900140808         wData_EUR = wData_ISO;
017000140808         DataEla   = %dec(wData_EUR);
017100140808         OraEla    = %dec(%time());
017200140808
017300140808       //?Stampo testata
017400140808         write ACPO05T0;
017500140514
017600140514       ENDSR;
017700140514
017800140514       //--------------------------------------------------------------
017900140514       //?Reperimento Dati del job.
018000140514       //--------------------------------------------------------------
018100140514       BEGSR DatiJob;
018200140514
018300140514         in(E) §AzUte;
018400140514         IF  not %error;
018500140514           in(E) §DatiUte;
018600140514         ENDIF;
018700140514         IF  %error or rsut = *blanks;
018800140514           clear TIBS34ds;
018900140514           tibs34r(tibs34ds);
019000140514           in §AzUte;
019100140514           in §DatiUte;
019200140514         ENDIF;
019300140514
019400140514       ENDSR;
019500140808
019600140808       //--------------------------------------------------------------
019700140808       //?Conto i potenziali.
019800140808       //--------------------------------------------------------------
019900140808       BEGSR Conta;
020000140808
020100140808       //?Conto quanti potenziali posso essere modificati in eliminabili
020200140808         exec sql
020300140808         SELECT count(*) into :totpte
020400140808         FROM   WFACPV0F join TNCPO01L on  WAPcpo = CPOcpo
020500140808         WHERE  WAPoob = 'Y' and WAPcmpvnd = ' ' and
020600140811                CPOfls in('M', 'P');
020700140808
020800140811         wEoF = *off;
020900140811
021000140811       //?Leggo WFACPV0F
021100140811         exec sql
021200140811         DECLARE A1 cursor for
021300140811         SELECT wfacpv0f.*
021400140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
021500140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
021600140811               CPOfls in('M', 'P');
021700140811
021800140811         exec sql
021900140811         OPEN A1;
022000140811         IF  sqlcode < 0;
022100140811           wEoF = *on;
022200140811         ENDIF;
022300140811
022400140811         DOW  not wEoF;
022500140811           exec sql
022600140811           FETCH NEXT from A1 into :WFACPV0F;
022700140811           IF  sqlcod = 100 or sqlcod < 0;
022800140811             wEoF = *on;
022900140811             leave;
023000140811           ENDIF;
023100140811
023200140811         //?Conto i di cui
023300140811         //?Non eliminabili perchè trattativa aperta
023400140811           chain (WAPcpo:0) TIVIS01L;
023500140811           IF  %found(TIVIS01L);
023600140811             totnoettr += 1;
023700140812           ELSE;
023800140811
023900140811         //?Non eliminabili perchè attività ancora da eseguire <> 21-22-24-26-60
024000140811           chain (WAPcpo:0:0) TIATC05L;
024100140811           IF  %found(TIATC05L) and
024200140811               ATCcad <> '21' and ATCcad <> '22' and
024300140811               ATCcad <> '24' and ATCcad <> '26' and
024400140811               ATCcad <> '60';
024500140811             totnoeatt += 1;
024600140811           ENDIF;
024700140812           ENDIF;
024800140811
024900140811       //?Eliminabili perchè attività ancora da eseguire = 21-22-24-26-60
025000140811           chain (WAPcpo:0:0) TIATC05L;
025100140811           IF  %found(TIATC05L) and
025200140811               (ATCcad = '21' or ATCcad = '22' or
025300140811                ATCcad = '24' or ATCcad = '26' or
025400140811                ATCcad = '60');
025500140811             toteatt += 1;
025600140811           ENDIF;
025700140811
025800140811         ENDDO;
025900140811
026000140811         exec sql
026100140811         CLOSE A1;
026200140811
026300140811       //?Eliminabili perchè mai contattati
026400140811         exec sql
026500140811         SELECT count(*) into :totemai
026600140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
026700140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
026800140811               CPOfls in('M', 'P') and substr(CPOrst, 13, 8) = '';
026900140811
027000140808       //?Stampo il conteggio
027100140808         write ACPO05D1;
027200140808
027300140808       ENDSR;
027400140808
027500140808       //--------------------------------------------------------------
027600140808       //?Scrive file di work.
027700140808       //--------------------------------------------------------------
027800140808       BEGSR Scrivi;
027900140808
028000140811         wEoF = *off;
028100140811       //?Leggo WFACPV0F
028200140808         exec sql
028300140811         DECLARE WFACP cursor for
028400140811         SELECT wfacpv0f.*
028500140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
028600140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
028700140811               CPOfls in('M', 'P');
028800140808
028900140808         exec sql
029000140811         OPEN WFACP;
029100140808         IF  sqlcode < 0;
029200140808           wEoF = *on;
029300140808         ENDIF;
029400140808
029500140808         DOW  not wEoF;
029600140808           exec sql
029700140811           FETCH NEXT from WFACP into :WFACPV0F;
029800140808           IF  sqlcod = 100 or sqlcod < 0;
029900140808             wEoF = *on;
030000140808             leave;
030100140808           ENDIF;
030200140808
030300140811           //?Scrivo il file dei potenziali da eliminare
030400140811           write WFACPE00;
030500140808
030600140808         ENDDO;
030700140808
030800140808         exec sql
030900140811         CLOSE WFACP;
031000140808
031100140811       //?Conto i rcd scritti nel file
031200140808         exec sql
031300140811         SELECT count(*) into :stpnrracpe
031400140811         FROM   WFACPE0F;
031500140808
031600140808       //?Stampo
031700140811         write ACPO05D2;
031800140808
031900140808       ENDSR;
032000140811
032100140811       //--------------------------------------------------------------
032200140811       //?Elimino i potenziali.
032300140811       //--------------------------------------------------------------
032400140811       BEGSR Elimina;
032500140811
032600140811         clear totemaie;
032700140811         clear totettre;
032800140811         clear stppoteli;
032900140811         wEoF = *off;
033000140811
033100140811       //?Leggo WFACPE0F
033200140811         exec sql
033300140811         DECLARE WFACP1 cursor for
033400140811         SELECT *
033500140811         FROM WFACPE0F;
033600140811
033700140811         exec sql
033800140811         OPEN WFACP1;
033900140811         IF  sqlcode < 0;
034000140811           wEoF = *on;
034100140811         ENDIF;
034200140811
034300140811         DOW  not wEoF;
034400140811           exec sql
034500140811           FETCH NEXT from WFACP1 into :WFACPEDS;
034600140811           IF  sqlcod = 100 or sqlcod < 0;
034700140811             wEoF = *on;
034800140811             leave;
034900140811           ENDIF;
035000140811
035100140811         //?Aggancio il potenziale
035200140811           chain wWAPcpo TNCPO01L;
035300140811           IF  not %found(TNCPO01L);
035400140811             leavesr;
035500140811           ENDIF;
035600140811
035700140811           dCPO01 = CPOrst;
035800140811
035900140811         //?Conto se mai contattato - Elimino se richiesto dal lancio
036000140911           IF  dcpo01.§CPOdtpcon = *blanks or dcpo01.§CPOdtpcon = *zeros;
036100140811             totemaie += 1;
036200140924             exsr PotMaiCont;
036300140811           ELSE;
036400140811         //?Conto se NO trattative in corso - Elimino se richiesto dal lancio
036500140811             chain (wWAPcpo:0) TIVIS01L;
036600140811             IF  not %found(TIVIS01L);
036700140811               totettre += 1;
036800140811               exsr PotNoTtr;
036900140811             ENDIF;
037000140811           ENDIF;
037100140811
037200140811         ENDDO;
037300140811
037400140811         exec sql
037500140811         CLOSE WFACP1;
037600140811
037700140811       //?Stampo
037800140811         write ACPO05D3;
037900140811
038000140811       ENDSR;
038100140811
038200140811       //--------------------------------------------------------------
038300140811       //?Metto ELIMINABILE potenziale mai contattato.
038400140811       //--------------------------------------------------------------
038500140811       BEGSR PotMaiCont;
038600140923
038700140923         OkAtc = *off;
038800140811
038900140811       //?Conto quanti potenziali elimino
039000140811         StpPotEli += 1;
039100140811
039200140911       //?Se AGGIORNA scrivo nuova attività 60
039300140923       //?            o chiudo le attività ancora aperte
039400140811         IF  wTpEla <> 'A';
039500140811           unlock TNCPO01L;
039600140811           leavesr;
039700140811         ENDIF;
039800140923
039900140923       //?Cerco tutte le attività del potenziale ancora da eseguire
040000140923         setll (wWAPcpo:0:0) TIATC05L;
040100140923         reade (wWAPcpo:0:0) TIATC05L;
040200140923         DOW  not %eof(TIATC05L);
040300140923
040400140923           dATC01 = ATCflo;
040500140923           OkAtc = *on;
040600140923
040700140923         //?Chiudo l'attività ancora aperta per pot.eliminabile
040800140923           ATCdco  = %dec(%date());
040900140923           ATChco  = %dec(%time());
041000140923           ATCcco  = 0460000;
041100140923           ATCpru  = 'BATCH';
041200140923           ATCcac  = '15';
041300140923
041400140923         //?Memorizzo la categoria del potenziale alla chiusura dell'attività
041500140923           §ATCcapo2 = CPOfls;
041600140923           ATCflo = dATC01;
041700140923           update TIATC000;
041800140923
041900140923         //?Scrivo la nota richiesta obbligatoria della causale
042000140923           exsr Nota;
042100140923
042200140923           reade (wWAPcpo:0:0) TIATC05L;
042300140923         ENDDO;
042400140923
042500140923       //?Se non ho trovato attività da eseguire scrivo una attività
042600140923         IF  not OkAtc;
042700140923           exsr NewAttivita;
042800140923           §ATCmaitr = 'S';
042900140923           ATCflo = dATC01;
043000140923           write TIATC000;
043100140923           exsr Nota;
043200140923         ENDIF;
043300140811
043400140811       //?Metto il potenziale in categoria "E" --> Eliminabile
043500140811         CPOfls = 'E';
043600140911       //?Imposto la data primo contatto
043700140911         dcpo01.§CPOdtpcon = %editc(woggi:'X');
043800140911         CPOrst = dCPO01;
043900140811         update TNCPO000;
044000140811
044100140811       ENDSR;
044200140811
044300140811       //--------------------------------------------------------------
044400140811       //?Metto ELIMINABILE potenziale no trattative.
044500140811       //--------------------------------------------------------------
044600140811       BEGSR PotNoTtr;
044700140911
044800140911         OkAtc = *off;
044900140811
045000140811       //?Se NON AGGIORNA conto e basta
045100140811         IF  wTpEla <> 'A';
045200140811           chain(n) wWAPcpo TNCPO01L;
045300140811           IF  %found(TNCPO01L);
045400140811             StpPotEli += 1;
045500140811             leavesr;
045600140811           ENDIF;
045700140811         ENDIF;
045800140811
045900140811       //?Cerco tutte le attività del potenziale ancora da eseguire
046000140811         setll (wWAPcpo:0:0) TIATC05L;
046100140811         reade (wWAPcpo:0:0) TIATC05L;
046200140811         DOW  not %eof(TIATC05L);
046300140911
046400140911           dATC01 = ATCflo;
046500140911           OkAtc = *on;
046600140811
046700140811         //?Chiudo l'attività ancora aperta per pot.eliminabile
046800140911           ATCdco  = %dec(%date());
046900140911           ATChco  = %dec(%time());
047000140911           ATCcco  = 0460000;
047100140911           ATCpru  = 'BATCH';
047200140812           ATCcac  = '15';
047300140911
047400140911         //?Memorizzo la categoria del potenziale alla chiusura dell'attività
047500140911           §ATCcapo2 = CPOfls;
047600140911           ATCflo = dATC01;
047700140811           update TIATC000;
047800140811
047900140811         //?Scrivo la nota richiesta obbligatoria della causale
048000140911           exsr Nota;
048100140811
048200140811           reade (wWAPcpo:0:0) TIATC05L;
048300140811         ENDDO;
048400140911
048500140911       //?Se non ho trovato attività da eseguire devo scrivere una attività
048600140911       //?come faccio per i mai contattati
048700140911         IF  not OkAtc;
048800140911           exsr NewAttivita;
048900140923           ATCflo = dATC01;
049000140923           write TIATC000;
049100140911           exsr Nota;
049200140911         ENDIF;
049300140811
049400140811       //?Metto il potenziale in categoria "E" --> Eliminabile
049500140811         chain wWAPcpo TNCPO01L;
049600140811         IF  %found(TNCPO01L);
049700140811           CPOfls = 'E';
049800140811           update TNCPO000;
049900140811           StpPotEli += 1;
050000140811         ENDIF;
050100140811
050200140811       ENDSR;
050300140911
050400140911       //--------------------------------------------------------------
050500140911       //?Scrivo nuova attività.
050600140911       //--------------------------------------------------------------
050700140911       BEGSR NewAttivita;
050800140911
050900140911       //?Stacco nuovo numero attività
051000140911         exsr StaccaNumero;
051100140911       //?se errore vado subito a fine
051200140911         IF  O33Err <> 0;
051300140911           leavesr;
051400140911         ENDIF;
051500140911
051600140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
051700140911         clear TIATC000;
051800140911
051900140911         ATCtat  = 'T';
052000140911         ATCatn  = O33Nrf;
052100140911         ATCatnp = 1;
052200140911         ATCcpo  = CPOcpo;
052300140911
052400140911         ATCcad  = '60';
052500140911         ATCdad  = %dec(%date());
052600140911         ATChda  = %dec(%time());
052700140911
052800140911         ATCcmm = 0460000;
052900140911
053000140911       //?Scrivo l'attività già chiusa per pot.eliminabile
053100140911         ATCdco  = %dec(%date());
053200140911         ATChco  = %dec(%time());
053300140911         ATCcco  = ATCcmm;
053400140911         ATCpru  = 'BATCH';
053500140911         ATCcac  = '15';
053600140911
053700140911         ATCdim  = %dec(%date());
053800140911         ATChim  = %dec(%time());
053900140911         ATCpri  = 'BATCH';
054000140911
054100140911       //?Attività CREATA potenziale
054200140911         ATCcnw = 'P';
054300140911
054400140911       //?Memorizzo la categoria del potenziale
054500140911         §ATCcapo1 = CPOfls;
054600140911         §ATCcapo2 = CPOfls;
054700140911
054800140911       ENDSR;
054900140811
055000140811       //--------------------------------------------------------------
055100140811       //?Stacco nuovo numero attività.
055200140811       //--------------------------------------------------------------
055300140911       BEGSR StaccaNumero;
055400140811
055500140811         clear TRUL33ds;
055600140811         I33tla = 'L';
055700140811         I33ope = 0;
055800140811         I33cnu = 070;
055900140811         I33num = 1;
056000140811         I33aaa = *YEAR;
056100140811         kpjbu  = TRUL33ds;
056200140811         trul33r (kpjba);
056300140811         TRUL33ds = kpjbu;
056400140811
056500140811       ENDSR;
056600140911
056700140911       //--------------------------------------------------------------
056800140911       //?Scrivo la nota legata all'attività.
056900140911       //--------------------------------------------------------------
057000140911       BEGSR Nota;
057100140911
057200140911         clear TRMK20ds;
057300140911         IMK20tla  = 'L';
057400140911         IMK20flm  = 'C';
057500140911         IMK20fno  = 'S';
057600140911         IMK20cpo  = ATCcpo;
057700140911         IMK20ksc  = ATCksc;
057800140911         IMK20tat  = ATCtat;
057900140911         IMK20atn  = ATCatn;
058000140911         IMK20atnp = ATCatnp;
058100140911         EMK20no1 = 'CRIBIS';
058200140911         trmk20r (kpjba : TRMK20ds);
058300140911       ENDSR;
058400140514
058500140514       //--------------------------------------------------------------
058600140514       //?Operazioni finali.
058700140514       //--------------------------------------------------------------
058800140514       BEGSR RoutEnd;
058900140808
059000140808       //?Stampo la fine elaborazione
059100140811         write ACPO05E4;
059200140514
059300140514         *inLR = *on;
059400140514         return;
059500140514
059600140514       ENDSR;
059700140514
059800140514      /end-free
