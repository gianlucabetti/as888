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
007600140808     d wEoF            s               n   inz(*off)
007700140514     d wFine           s               n   inz(*off)
007800140514
007900140514      // - Campi di comodo data
008000140808     d wData_EUR       s               d   datfmt(*eur)
008100140808     d wData_ISO       s               d   datfmt(*iso)
008200140514     d wOggi           s              8  0 inz
008300140514
008400140514      // - Campi di comodo
008500140811     d wCMMda          s              7s 0 inz
008600140811     d wCMMa           s              7s 0 inz
008700140808     d wTpEla          s              1a   inz
008800140514
008900140514      //---------------------------------------------------------------
009000140514      //?Definizione procedure usate.
009100140514      //---------------------------------------------------------------
009200140514
009300140514      //---------------------------------------------------------------
009400140807      //?Definizione prototipi.
009500140514      //---------------------------------------------------------------
009600140514      /copy gaitrasrc/srcprotopr,TIBS34R
009700140811      /copy gaitrasrc/srcprotopr,TRMK20R
009800140811      /copy gaitrasrc/srcprotopr,TRUL33R
009900140514
010000140514      //---------------------------------------------------------------
010100140514      //?Definizione key-list.
010200140514      //---------------------------------------------------------------
010300140514
010400140514      //---------------------------------------------------------------
010500140514      //?M A I N - L I N E
010600140514      //---------------------------------------------------------------
010700140514
010800140514     c     *entry        plist
010900140514     c                   parm                    KPJBA
011000140514
011100140514      /free
011200140514
011300140514       //?Operazioni iniziali
011400140514       exsr RoutInz;
011500140514
011600140808       //?Elaborazioni
011700140808       exsr Conta;
011800140808       IF  wTpEla <> 'C';
011900140808         exsr Scrivi;
012000140808         exsr Elimina;
012100140808       ENDIF;
012200140514
012300140514       //?Operazioni finali
012400140514       exsr RoutEnd;
012500140514
012600140514       //--------------------------------------------------------------
012700140514       //?Operazioni iniziali.
012800140514       //--------------------------------------------------------------
012900140514       BEGSR RoutInz;
013000140808
013100140808         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013200140808
013300140808       //?Imposto il tipo di elaborazione da fare
013400140808       //?Se blank metto di dft il conteggio
013500140808       //?C=Conteggio dei rcd da aggiornare e non per Situazione 1 e 2
013600140808       //?W=Crea i file dei potenziali eliminabili
013700140808       //?A=Mette i potenziali a stato ELIMINABILI
013800140808         IF  %subst(kpjbu:1:1) = *blanks;
013900140808           wTpEla = 'C';
014000140808         ELSE;
014100140808           wTpEla = %subst(kpjbu:1:1);
014200140808         ENDIF;
014300140808         IF  wTpEla <> 'C' and wTpEla <> 'W' and wTpEla <> 'A';
014400140808           clear wTpEla;
014500140808         ENDIF;
014600140808
014700140808         clear StpTpEla;
014800140808         IF  wTpEla = 'C';
014900140808           StpTpEla = 'Conteggio dei potenziali';
015000140808         ENDIF;
015100140808         IF  wTpEla = 'W';
015200140808           StpTpEla = 'Scrive File + Conteggio';
015300140808         ENDIF;
015400140808         IF  wTpEla = 'A';
015500140808           StpTpEla = 'Aggiorna pot. Eliminabili';
015600140808         ENDIF;
015700140808
015800140811       //?Se elaborazione NO = 'C' devo prima cancellare i dati dal file di work
015900140808         IF  wTpEla <> 'C';
016000140808           exec sql
016100140808           DELETE
016200140808           FROM  WFACPE0F;
016300140808         ENDIF;
016400080206
016500100326       //?Reperimento dati job
016600080206         exsr DatiJob;
016700140808
016800140808       //?Data e ora sistema
016900140808         wOggi = %dec(%date());
017000140808         wData_ISO = %date(wOggi:*ISO);
017100140808         wData_EUR = wData_ISO;
017200140808         DataEla   = %dec(wData_EUR);
017300140808         OraEla    = %dec(%time());
017400140808
017500140808       //?Stampo testata
017600140808         write ACPO05T0;
017700140514
017800140514       ENDSR;
017900140514
018000140514       //--------------------------------------------------------------
018100140514       //?Reperimento Dati del job.
018200140514       //--------------------------------------------------------------
018300140514       BEGSR DatiJob;
018400140514
018500140514         in(E) §AzUte;
018600140514         IF  not %error;
018700140514           in(E) §DatiUte;
018800140514         ENDIF;
018900140514         IF  %error or rsut = *blanks;
019000140514           clear TIBS34ds;
019100140514           tibs34r(tibs34ds);
019200140514           in §AzUte;
019300140514           in §DatiUte;
019400140514         ENDIF;
019500140514
019600140514       ENDSR;
019700140808
019800140808       //--------------------------------------------------------------
019900140808       //?Conto i potenziali.
020000140808       //--------------------------------------------------------------
020100140808       BEGSR Conta;
020200140808
020300140808       //?Conto quanti potenziali posso essere modificati in eliminabili
020400140808         exec sql
020500140808         SELECT count(*) into :totpte
020600140808         FROM   WFACPV0F join TNCPO01L on  WAPcpo = CPOcpo
020700140808         WHERE  WAPoob = 'Y' and WAPcmpvnd = ' ' and
020800140811                CPOfls in('M', 'P');
020900140808
021000140811         wEoF = *off;
021100140811
021200140811       //?Leggo WFACPV0F
021300140811         exec sql
021400140811         DECLARE A1 cursor for
021500140811         SELECT wfacpv0f.*
021600140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
021700140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
021800140811               CPOfls in('M', 'P');
021900140811
022000140811         exec sql
022100140811         OPEN A1;
022200140811         IF  sqlcode < 0;
022300140811           wEoF = *on;
022400140811         ENDIF;
022500140811
022600140811         DOW  not wEoF;
022700140811           exec sql
022800140811           FETCH NEXT from A1 into :WFACPV0F;
022900140811           IF  sqlcod = 100 or sqlcod < 0;
023000140811             wEoF = *on;
023100140811             leave;
023200140811           ENDIF;
023300140811
023400140811         //?Conto i di cui
023500140811         //?Non eliminabili perchè trattativa aperta
023600140811           chain (WAPcpo:0) TIVIS01L;
023700140811           IF  %found(TIVIS01L);
023800140811             totnoettr += 1;
023900140812           ELSE;
024000140811
024100140811         //?Non eliminabili perchè attività ancora da eseguire <> 21-22-24-26-60
024200140811           chain (WAPcpo:0:0) TIATC05L;
024300140811           IF  %found(TIATC05L) and
024400140811               ATCcad <> '21' and ATCcad <> '22' and
024500140811               ATCcad <> '24' and ATCcad <> '26' and
024600140811               ATCcad <> '60';
024700140811             totnoeatt += 1;
024800140811           ENDIF;
024900140812           ENDIF;
025000140811
025100140811       //?Eliminabili perchè attività ancora da eseguire = 21-22-24-26-60
025200140811           chain (WAPcpo:0:0) TIATC05L;
025300140811           IF  %found(TIATC05L) and
025400140811               (ATCcad = '21' or ATCcad = '22' or
025500140811                ATCcad = '24' or ATCcad = '26' or
025600140811                ATCcad = '60');
025700140811             toteatt += 1;
025800140811           ENDIF;
025900140811
026000140811         ENDDO;
026100140811
026200140811         exec sql
026300140811         CLOSE A1;
026400140811
026500140811       //?Eliminabili perchè mai contattati
026600140811         exec sql
026700140811         SELECT count(*) into :totemai
026800140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
026900140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
027000140811               CPOfls in('M', 'P') and substr(CPOrst, 13, 8) = '';
027100140811
027200140808       //?Stampo il conteggio
027300140808         write ACPO05D1;
027400140808
027500140808       ENDSR;
027600140808
027700140808       //--------------------------------------------------------------
027800140808       //?Scrive file di work.
027900140808       //--------------------------------------------------------------
028000140808       BEGSR Scrivi;
028100140808
028200140811         wEoF = *off;
028300140811       //?Leggo WFACPV0F
028400140808         exec sql
028500140811         DECLARE WFACP cursor for
028600140811         SELECT wfacpv0f.*
028700140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
028800140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
028900140811               CPOfls in('M', 'P');
029000140808
029100140808         exec sql
029200140811         OPEN WFACP;
029300140808         IF  sqlcode < 0;
029400140808           wEoF = *on;
029500140808         ENDIF;
029600140808
029700140808         DOW  not wEoF;
029800140808           exec sql
029900140811           FETCH NEXT from WFACP into :WFACPV0F;
030000140808           IF  sqlcod = 100 or sqlcod < 0;
030100140808             wEoF = *on;
030200140808             leave;
030300140808           ENDIF;
030400140808
030500140811           //?Scrivo il file dei potenziali da eliminare
030600140811           write WFACPE00;
030700140808
030800140808         ENDDO;
030900140808
031000140808         exec sql
031100140811         CLOSE WFACP;
031200140808
031300140811       //?Conto i rcd scritti nel file
031400140808         exec sql
031500140811         SELECT count(*) into :stpnrracpe
031600140811         FROM   WFACPE0F;
031700140808
031800140808       //?Stampo
031900140811         write ACPO05D2;
032000140808
032100140808       ENDSR;
032200140811
032300140811       //--------------------------------------------------------------
032400140811       //?Elimino i potenziali.
032500140811       //--------------------------------------------------------------
032600140811       BEGSR Elimina;
032700140811
032800140811         clear totemaie;
032900140811         clear totettre;
033000140811         clear stppoteli;
033100140811         wEoF = *off;
033200140811
033300140811       //?Leggo WFACPE0F
033400140811         exec sql
033500140811         DECLARE WFACP1 cursor for
033600140811         SELECT *
033700140811         FROM WFACPE0F;
033800140811
033900140811         exec sql
034000140811         OPEN WFACP1;
034100140811         IF  sqlcode < 0;
034200140811           wEoF = *on;
034300140811         ENDIF;
034400140811
034500140811         DOW  not wEoF;
034600140811           exec sql
034700140811           FETCH NEXT from WFACP1 into :WFACPEDS;
034800140811           IF  sqlcod = 100 or sqlcod < 0;
034900140811             wEoF = *on;
035000140811             leave;
035100140811           ENDIF;
035200140811
035300140811         //?Aggancio il potenziale
035400140811           chain wWAPcpo TNCPO01L;
035500140811           IF  not %found(TNCPO01L);
035600140811             leavesr;
035700140811           ENDIF;
035800140811
035900140811           dCPO01 = CPOrst;
036000140811
036100140811         //?Conto se mai contattato - Elimino se richiesto dal lancio
036200140811           IF  dcpo01.§CPOdtpcon = *blanks;
036300140811             totemaie += 1;
036400140811             exsr PotMaiCont;
036500140811           ELSE;
036600140811         //?Conto se NO trattative in corso - Elimino se richiesto dal lancio
036700140811             chain (wWAPcpo:0) TIVIS01L;
036800140811             IF  not %found(TIVIS01L);
036900140811               totettre += 1;
037000140811               exsr PotNoTtr;
037100140811             ENDIF;
037200140811           ENDIF;
037300140811
037400140811         ENDDO;
037500140811
037600140811         exec sql
037700140811         CLOSE WFACP1;
037800140811
037900140811       //?Stampo
038000140811         write ACPO05D3;
038100140811
038200140811       ENDSR;
038300140811
038400140811       //--------------------------------------------------------------
038500140811       //?Metto ELIMINABILE potenziale mai contattato.
038600140811       //--------------------------------------------------------------
038700140811       BEGSR PotMaiCont;
038800140811
038900140811       //?Conto quanti potenziali elimino
039000140811         StpPotEli += 1;
039100140811
039200140811       //?Se non c'è il commericale sul potenziale
039300140811       //?prendo il primo comm.int della filiale del potenziale
039400140811         IF  CPOcmm = 0;
039500140811           exsr CercaComInt;
039600140811           ATCcmm = CMMcod;
039700140811         ENDIF;
039800140811
039900140911       //?Se AGGIORNA scrivo nuova attività 60
040000140811         IF  wTpEla <> 'A';
040100140811           unlock TNCPO01L;
040200140811           leavesr;
040300140811         ENDIF;
040400140811
040500140811       //?Stacco nuovo numero attività
040600140811         exsr NewAttivita;
040700140811       //?se errore vado subito a fine
040800140811         IF  O33Err <> 0;
040900140811           leavesr;
041000140811         ENDIF;
041100140811
041200140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
041300140811         clear dATC01;
041400140811         clear TIATC000;
041500140811
041600140811         ATCtat  = 'T';
041700140811         ATCatn  = O33Nrf;
041800140811         ATCatnp = 1;
041900140811         ATCcpo  = CPOcpo;
042000140811
042100140911         ATCcad  = '60';
042200140911         ATCdad  = %dec(%date());
042300140911         ATChda  = %dec(%time());
042400140811
042500140811         IF  CPOcmm > 0;
042600140811           ATCcmm  = CPOcmm;
042700140811         ELSE;
042800140811           ATCcmm = CMMcod;
042900140811         ENDIF;
043000140911
043100140911         ATCcmm = 0460000;
043200140811
043300140811       //?Scrivo l'attività già chiusa per pot.eliminabile
043400140911         ATCdco  = %dec(%date());
043500140911         ATChco  = %dec(%time());
043600140811         ATCcco  = ATCcmm;
043700140911         ATCpru  = 'BATCH';
043800140812         ATCcac  = '15';
043900140811
044000140911         //ATCco3  = ATCcmm;
044100140811         ATCdim  = %dec(%date());
044200140811         ATChim  = %dec(%time());
044300140811         ATCpri  = 'BATCH';
044400140811
044500140811       //?Attività CREATA potenziale
044600140811         ATCcnw = 'P';
044700140911
044800140911       //?Memorizzo se mai contattato o no
044900140911         IF  dCPO01.§CPOdtpcon = *blanks or dCPO01.§CPOdtpcon = *zeros;
045000140911           §ATCmaitr = 'S';
045100140911         ENDIF;
045200140811
045300140911       //?Memorizzo la categoria del potenziale
045400140811         §ATCcapo1 = CPOfls;
045500140911         §ATCcapo2 = CPOfls;
045600140811         ATCflo = dATC01;
045700140811
045800140811         write TIATC000;
045900140811
046000140811       //?Scrivo la nota richiesta obbligatoria della causale
046100140811         clear TRMK20ds;
046200140811         IMK20tla  = 'L';
046300140811         IMK20flm  = 'C';
046400140811         IMK20fno  = 'S';
046500140811         IMK20cpo  = ATCcpo;
046600140812         IMK20ksc  = ATCksc;
046700140811         IMK20tat  = ATCtat;
046800140811         IMK20atn  = ATCatn;
046900140911         IMK20atnp = ATCatnp;
047000140811         EMK20no1 = 'CRIBIS';
047100140811         trmk20r (kpjba : TRMK20ds);
047200140811
047300140811       //?Metto il potenziale in categoria "E" --> Eliminabile
047400140811         CPOfls = 'E';
047500140911       //?Se mai contattato imposto la data primo contatto
047600140911         IF  dCPO01.§CPOdtpcon = *blanks or dCPO01.§CPOdtpcon = *zeros;
047700140911           dCPO01.§CPOdtpcon = %editc(woggi:'X');
047800140911           CPOrst = dCPO01;
047900140911         ENDIF;
048000140811         update TNCPO000;
048100140811
048200140811       ENDSR;
048300140811
048400140811       //--------------------------------------------------------------
048500140811       //?Metto ELIMINABILE potenziale no trattative.
048600140811       //--------------------------------------------------------------
048700140811       BEGSR PotNoTtr;
048800140811
048900140811       //?Se NON AGGIORNA conto e basta
049000140811         IF  wTpEla <> 'A';
049100140811           chain(n) wWAPcpo TNCPO01L;
049200140811           IF  %found(TNCPO01L);
049300140811             StpPotEli += 1;
049400140811             leavesr;
049500140811           ENDIF;
049600140811         ENDIF;
049700140811
049800140811       //?Cerco tutte le attività del potenziale ancora da eseguire
049900140811         setll (wWAPcpo:0:0) TIATC05L;
050000140811         reade (wWAPcpo:0:0) TIATC05L;
050100140811         DOW  not %eof(TIATC05L);
050200140811
050300140811         //?Chiudo l'attività ancora aperta per pot.eliminabile
050400140911           ATCdco  = %dec(%date());
050500140911           ATChco  = %dec(%time());
050600140811           ATCcco  = ATCcmm;
050700140911           ATCcco  = 0460000;
050800140911           ATCpru  = 'BATCH';
050900140812           ATCcac  = '15';
051000140911
051100140911         //?Memorizzo se mai contattato o no
051200140911           IF  dCPO01.§CPOdtpcon = *blanks or dCPO01.§CPOdtpcon = *zeros;
051300140911             §ATCmaitr = 'S';
051400140911           ENDIF;
051500140911         //?Memorizzo la categoria del potenziale alla chiusura dell'attività
051600140911           §ATCcapo2 = CPOfls;
051700140911           ATCflo = dATC01;
051800140811           update TIATC000;
051900140811
052000140811         //?Scrivo la nota richiesta obbligatoria della causale
052100140811           clear TRMK20ds;
052200140811           IMK20tla  = 'L';
052300140811           IMK20flm  = 'C';
052400140811           IMK20fno  = 'S';
052500140811           IMK20cpo  = ATCcpo;
052600140811           IMK20ksc  = ATCksc;
052700140811           IMK20tat  = ATCtat;
052800140811           IMK20atn  = ATCatn;
052900140911           IMK20atnp = ATCatnp;
053000140811           EMK20no1 = 'CRIBIS';
053100140811           trmk20r (kpjba : TRMK20ds);
053200140811
053300140811           reade (wWAPcpo:0:0) TIATC05L;
053400140811         ENDDO;
053500140811
053600140811       //?Metto il potenziale in categoria "E" --> Eliminabile
053700140811         chain wWAPcpo TNCPO01L;
053800140811         IF  %found(TNCPO01L);
053900140811           CPOfls = 'E';
054000140911         //?Se mai contattato imposto la data primo contatto
054100140911           IF  dCPO01.§CPOdtpcon = *blanks or dCPO01.§CPOdtpcon = *zeros;
054200140911             dCPO01.§CPOdtpcon = %editc(woggi:'X');
054300140911             CPOrst = dCPO01;
054400140911           ENDIF;
054500140811           update TNCPO000;
054600140811           StpPotEli += 1;
054700140811         ENDIF;
054800140811
054900140811       ENDSR;
055000140811
055100140811       //--------------------------------------------------------------
055200140811       //?Stacco nuovo numero attività.
055300140811       //--------------------------------------------------------------
055400140811       BEGSR NewAttivita;
055500140811
055600140811         clear TRUL33ds;
055700140811         I33tla = 'L';
055800140811         I33ope = 0;
055900140811         I33cnu = 070;
056000140811         I33num = 1;
056100140811         I33aaa = *YEAR;
056200140811         kpjbu  = TRUL33ds;
056300140811         trul33r (kpjba);
056400140811         TRUL33ds = kpjbu;
056500140811
056600140811       ENDSR;
056700140811
056800140811       //--------------------------------------------------------------
056900140811       //?Cerco il primo comm.interno filiale Potenziale.
057000140811       //--------------------------------------------------------------
057100140811       BEGSR CercaComInt;
057200140811
057300140811       //?Se mai fatto stampo la testata
057400140811         IF  not noTestata;
057500140811           write ACPO05C1;
057600140811           write ACPO05C2;
057700140811           noTestata = *on;
057800140811         ENDIF;
057900140811         IF  *in90;
058000140811           write ACPO05C1;
058100140811           write ACPO05C2;
058200140811           *in90 = *off;
058300140811         ENDIF;
058400140811
058500140811         wCMMda = CPOflt * 1000;
058600140811         wCMMa  = wCMMda + 9999;
058700140811
058800140811         exec sql
058900140811         SELECT CMMcod into :CMMcod
059000140811         FROM   AZCMM01L
059100140811         WHERE  CMMcod between :wCMMda and :wCMMa and
059200140811                CMMfun in('COMIN', 'SA')
059300140811         ORDER BY CMMcod
059400140811         FETCH FIRST 1 ROW ONLY;
059500140811         IF  sqlcode = 100;
059600140811           clear CMMcod;
059700140811         ENDIF;
059800140811
059900140811         write ACPO05C3;
060000140811
060100140811       ENDSR;
060200140514
060300140514       //--------------------------------------------------------------
060400140514       //?Operazioni finali.
060500140514       //--------------------------------------------------------------
060600140514       BEGSR RoutEnd;
060700140808
060800140808       //?Stampo la fine elaborazione
060900140811         write ACPO05E4;
061000140514
061100140514         *inLR = *on;
061200140514         return;
061300140514
061400140514       ENDSR;
061500140514
061600140514      /end-free
