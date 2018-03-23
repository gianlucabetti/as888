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
007800140514     d wFine           s               n   inz(*off)
007900140514
008000140514      // - Campi di comodo data
008100140808     d wData_EUR       s               d   datfmt(*eur)
008200140808     d wData_ISO       s               d   datfmt(*iso)
008300140514     d wOggi           s              8  0 inz
008400140514
008500140514      // - Campi di comodo
008600140811     d wCMMda          s              7s 0 inz
008700140811     d wCMMa           s              7s 0 inz
008800140808     d wTpEla          s              1a   inz
008900140514
009000140514      //---------------------------------------------------------------
009100140514      //?Definizione procedure usate.
009200140514      //---------------------------------------------------------------
009300140514
009400140514      //---------------------------------------------------------------
009500140807      //?Definizione prototipi.
009600140514      //---------------------------------------------------------------
009700140514      /copy gaitrasrc/srcprotopr,TIBS34R
009800140811      /copy gaitrasrc/srcprotopr,TRMK20R
009900140811      /copy gaitrasrc/srcprotopr,TRUL33R
010000140514
010100140514      //---------------------------------------------------------------
010200140514      //?Definizione key-list.
010300140514      //---------------------------------------------------------------
010400140514
010500140514      //---------------------------------------------------------------
010600140514      //?M A I N - L I N E
010700140514      //---------------------------------------------------------------
010800140514
010900140514     c     *entry        plist
011000140514     c                   parm                    KPJBA
011100140514
011200140514      /free
011300140514
011400140514       //?Operazioni iniziali
011500140514       exsr RoutInz;
011600140514
011700140808       //?Elaborazioni
011800140808       exsr Conta;
011900140808       IF  wTpEla <> 'C';
012000140808         exsr Scrivi;
012100140808         exsr Elimina;
012200140808       ENDIF;
012300140514
012400140514       //?Operazioni finali
012500140514       exsr RoutEnd;
012600140514
012700140514       //--------------------------------------------------------------
012800140514       //?Operazioni iniziali.
012900140514       //--------------------------------------------------------------
013000140514       BEGSR RoutInz;
013100140808
013200140808         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013300140808
013400140808       //?Imposto il tipo di elaborazione da fare
013500140808       //?Se blank metto di dft il conteggio
013600140808       //?C=Conteggio dei rcd da aggiornare e non per Situazione 1 e 2
013700140808       //?W=Crea i file dei potenziali eliminabili
013800140808       //?A=Mette i potenziali a stato ELIMINABILI
013900140808         IF  %subst(kpjbu:1:1) = *blanks;
014000140808           wTpEla = 'C';
014100140808         ELSE;
014200140808           wTpEla = %subst(kpjbu:1:1);
014300140808         ENDIF;
014400140808         IF  wTpEla <> 'C' and wTpEla <> 'W' and wTpEla <> 'A';
014500140808           clear wTpEla;
014600140808         ENDIF;
014700140808
014800140808         clear StpTpEla;
014900140808         IF  wTpEla = 'C';
015000140808           StpTpEla = 'Conteggio dei potenziali';
015100140808         ENDIF;
015200140808         IF  wTpEla = 'W';
015300140808           StpTpEla = 'Scrive File + Conteggio';
015400140808         ENDIF;
015500140808         IF  wTpEla = 'A';
015600140808           StpTpEla = 'Aggiorna pot. Eliminabili';
015700140808         ENDIF;
015800140808
015900140811       //?Se elaborazione NO = 'C' devo prima cancellare i dati dal file di work
016000140808         IF  wTpEla <> 'C';
016100140808           exec sql
016200140808           DELETE
016300140808           FROM  WFACPE0F;
016400140808         ENDIF;
016500080206
016600100326       //?Reperimento dati job
016700080206         exsr DatiJob;
016800140808
016900140808       //?Data e ora sistema
017000140808         wOggi = %dec(%date());
017100140808         wData_ISO = %date(wOggi:*ISO);
017200140808         wData_EUR = wData_ISO;
017300140808         DataEla   = %dec(wData_EUR);
017400140808         OraEla    = %dec(%time());
017500140808
017600140808       //?Stampo testata
017700140808         write ACPO05T0;
017800140514
017900140514       ENDSR;
018000140514
018100140514       //--------------------------------------------------------------
018200140514       //?Reperimento Dati del job.
018300140514       //--------------------------------------------------------------
018400140514       BEGSR DatiJob;
018500140514
018600140514         in(E) §AzUte;
018700140514         IF  not %error;
018800140514           in(E) §DatiUte;
018900140514         ENDIF;
019000140514         IF  %error or rsut = *blanks;
019100140514           clear TIBS34ds;
019200140514           tibs34r(tibs34ds);
019300140514           in §AzUte;
019400140514           in §DatiUte;
019500140514         ENDIF;
019600140514
019700140514       ENDSR;
019800140808
019900140808       //--------------------------------------------------------------
020000140808       //?Conto i potenziali.
020100140808       //--------------------------------------------------------------
020200140808       BEGSR Conta;
020300140808
020400140808       //?Conto quanti potenziali posso essere modificati in eliminabili
020500140808         exec sql
020600140808         SELECT count(*) into :totpte
020700140808         FROM   WFACPV0F join TNCPO01L on  WAPcpo = CPOcpo
020800140808         WHERE  WAPoob = 'Y' and WAPcmpvnd = ' ' and
020900140811                CPOfls in('M', 'P');
021000140808
021100140811         wEoF = *off;
021200140811
021300140811       //?Leggo WFACPV0F
021400140811         exec sql
021500140811         DECLARE A1 cursor for
021600140811         SELECT wfacpv0f.*
021700140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
021800140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
021900140811               CPOfls in('M', 'P');
022000140811
022100140811         exec sql
022200140811         OPEN A1;
022300140811         IF  sqlcode < 0;
022400140811           wEoF = *on;
022500140811         ENDIF;
022600140811
022700140811         DOW  not wEoF;
022800140811           exec sql
022900140811           FETCH NEXT from A1 into :WFACPV0F;
023000140811           IF  sqlcod = 100 or sqlcod < 0;
023100140811             wEoF = *on;
023200140811             leave;
023300140811           ENDIF;
023400140811
023500140811         //?Conto i di cui
023600140811         //?Non eliminabili perchè trattativa aperta
023700140811           chain (WAPcpo:0) TIVIS01L;
023800140811           IF  %found(TIVIS01L);
023900140811             totnoettr += 1;
024000140812           ELSE;
024100140811
024200140811         //?Non eliminabili perchè attività ancora da eseguire <> 21-22-24-26-60
024300140811           chain (WAPcpo:0:0) TIATC05L;
024400140811           IF  %found(TIATC05L) and
024500140811               ATCcad <> '21' and ATCcad <> '22' and
024600140811               ATCcad <> '24' and ATCcad <> '26' and
024700140811               ATCcad <> '60';
024800140811             totnoeatt += 1;
024900140811           ENDIF;
025000140812           ENDIF;
025100140811
025200140811       //?Eliminabili perchè attività ancora da eseguire = 21-22-24-26-60
025300140811           chain (WAPcpo:0:0) TIATC05L;
025400140811           IF  %found(TIATC05L) and
025500140811               (ATCcad = '21' or ATCcad = '22' or
025600140811                ATCcad = '24' or ATCcad = '26' or
025700140811                ATCcad = '60');
025800140811             toteatt += 1;
025900140811           ENDIF;
026000140811
026100140811         ENDDO;
026200140811
026300140811         exec sql
026400140811         CLOSE A1;
026500140811
026600140811       //?Eliminabili perchè mai contattati
026700140811         exec sql
026800140811         SELECT count(*) into :totemai
026900140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
027000140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
027100140811               CPOfls in('M', 'P') and substr(CPOrst, 13, 8) = '';
027200140811
027300140808       //?Stampo il conteggio
027400140808         write ACPO05D1;
027500140808
027600140808       ENDSR;
027700140808
027800140808       //--------------------------------------------------------------
027900140808       //?Scrive file di work.
028000140808       //--------------------------------------------------------------
028100140808       BEGSR Scrivi;
028200140808
028300140811         wEoF = *off;
028400140811       //?Leggo WFACPV0F
028500140808         exec sql
028600140811         DECLARE WFACP cursor for
028700140811         SELECT wfacpv0f.*
028800140811         FROM WFACPV0F join TNCPO01L on WAPcpo = CPOcpo
028900140811         WHERE WAPoob = 'Y' and WAPcmpvnd = ' ' and
029000140811               CPOfls in('M', 'P');
029100140808
029200140808         exec sql
029300140811         OPEN WFACP;
029400140808         IF  sqlcode < 0;
029500140808           wEoF = *on;
029600140808         ENDIF;
029700140808
029800140808         DOW  not wEoF;
029900140808           exec sql
030000140811           FETCH NEXT from WFACP into :WFACPV0F;
030100140808           IF  sqlcod = 100 or sqlcod < 0;
030200140808             wEoF = *on;
030300140808             leave;
030400140808           ENDIF;
030500140808
030600140811           //?Scrivo il file dei potenziali da eliminare
030700140811           write WFACPE00;
030800140808
030900140808         ENDDO;
031000140808
031100140808         exec sql
031200140811         CLOSE WFACP;
031300140808
031400140811       //?Conto i rcd scritti nel file
031500140808         exec sql
031600140811         SELECT count(*) into :stpnrracpe
031700140811         FROM   WFACPE0F;
031800140808
031900140808       //?Stampo
032000140811         write ACPO05D2;
032100140808
032200140808       ENDSR;
032300140811
032400140811       //--------------------------------------------------------------
032500140811       //?Elimino i potenziali.
032600140811       //--------------------------------------------------------------
032700140811       BEGSR Elimina;
032800140811
032900140811         clear totemaie;
033000140811         clear totettre;
033100140811         clear stppoteli;
033200140811         wEoF = *off;
033300140811
033400140811       //?Leggo WFACPE0F
033500140811         exec sql
033600140811         DECLARE WFACP1 cursor for
033700140811         SELECT *
033800140811         FROM WFACPE0F;
033900140811
034000140811         exec sql
034100140811         OPEN WFACP1;
034200140811         IF  sqlcode < 0;
034300140811           wEoF = *on;
034400140811         ENDIF;
034500140811
034600140811         DOW  not wEoF;
034700140811           exec sql
034800140811           FETCH NEXT from WFACP1 into :WFACPEDS;
034900140811           IF  sqlcod = 100 or sqlcod < 0;
035000140811             wEoF = *on;
035100140811             leave;
035200140811           ENDIF;
035300140811
035400140811         //?Aggancio il potenziale
035500140811           chain wWAPcpo TNCPO01L;
035600140811           IF  not %found(TNCPO01L);
035700140811             leavesr;
035800140811           ENDIF;
035900140811
036000140811           dCPO01 = CPOrst;
036100140811
036200140811         //?Conto se mai contattato - Elimino se richiesto dal lancio
036300140911           IF  dcpo01.§CPOdtpcon = *blanks or dcpo01.§CPOdtpcon = *zeros;
036400140811             totemaie += 1;
036500140811             exsr PotMaiCont;
036600140811           ELSE;
036700140811         //?Conto se NO trattative in corso - Elimino se richiesto dal lancio
036800140811             chain (wWAPcpo:0) TIVIS01L;
036900140811             IF  not %found(TIVIS01L);
037000140811               totettre += 1;
037100140811               exsr PotNoTtr;
037200140811             ENDIF;
037300140811           ENDIF;
037400140811
037500140811         ENDDO;
037600140811
037700140811         exec sql
037800140811         CLOSE WFACP1;
037900140811
038000140811       //?Stampo
038100140811         write ACPO05D3;
038200140811
038300140811       ENDSR;
038400140811
038500140811       //--------------------------------------------------------------
038600140811       //?Metto ELIMINABILE potenziale mai contattato.
038700140811       //--------------------------------------------------------------
038800140811       BEGSR PotMaiCont;
038900140811
039000140811       //?Conto quanti potenziali elimino
039100140811         StpPotEli += 1;
039200140811
039300140911       //?Se AGGIORNA scrivo nuova attività 60
039400140811         IF  wTpEla <> 'A';
039500140811           unlock TNCPO01L;
039600140811           leavesr;
039700140811         ENDIF;
039800140911
039900140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
040000140911         exsr NewAttivita;
040100140911       //?Memorizzo che potenziale mai contattato
040200140911         §ATCmaitr = 'S';
040300140911         ATCflo = dATC01;
040400140911         write TIATC000;
040500140811
040600140811       //?Scrivo la nota richiesta obbligatoria della causale
040700140911         exsr Nota;
040800140811
040900140811       //?Metto il potenziale in categoria "E" --> Eliminabile
041000140811         CPOfls = 'E';
041100140911       //?Imposto la data primo contatto
041200140911         dcpo01.§CPOdtpcon = %editc(woggi:'X');
041300140911         CPOrst = dCPO01;
041400140811         update TNCPO000;
041500140811
041600140811       ENDSR;
041700140811
041800140811       //--------------------------------------------------------------
041900140811       //?Metto ELIMINABILE potenziale no trattative.
042000140811       //--------------------------------------------------------------
042100140811       BEGSR PotNoTtr;
042200140911
042300140911         OkAtc = *off;
042400140811
042500140811       //?Se NON AGGIORNA conto e basta
042600140811         IF  wTpEla <> 'A';
042700140811           chain(n) wWAPcpo TNCPO01L;
042800140811           IF  %found(TNCPO01L);
042900140811             StpPotEli += 1;
043000140811             leavesr;
043100140811           ENDIF;
043200140811         ENDIF;
043300140811
043400140811       //?Cerco tutte le attività del potenziale ancora da eseguire
043500140811         setll (wWAPcpo:0:0) TIATC05L;
043600140811         reade (wWAPcpo:0:0) TIATC05L;
043700140811         DOW  not %eof(TIATC05L);
043800140911
043900140911           dATC01 = ATCflo;
044000140911           OkAtc = *on;
044100140811
044200140811         //?Chiudo l'attività ancora aperta per pot.eliminabile
044300140911           ATCdco  = %dec(%date());
044400140911           ATChco  = %dec(%time());
044500140911           ATCcco  = 0460000;
044600140911           ATCpru  = 'BATCH';
044700140812           ATCcac  = '15';
044800140911
044900140911         //?Memorizzo la categoria del potenziale alla chiusura dell'attività
045000140911           §ATCcapo2 = CPOfls;
045100140911           ATCflo = dATC01;
045200140811           update TIATC000;
045300140811
045400140811         //?Scrivo la nota richiesta obbligatoria della causale
045500140911           exsr Nota;
045600140811
045700140811           reade (wWAPcpo:0:0) TIATC05L;
045800140811         ENDDO;
045900140911
046000140911       //?Se non ho trovato attività da eseguire devo scrivere una attività
046100140911       //?come faccio per i mai contattati
046200140911         IF  not OkAtc;
046300140911           exsr NewAttivita;
046400140911         ATCflo = dATC01;
046500140911         write TIATC000;
046600140911           exsr Nota;
046700140911         ENDIF;
046800140811
046900140811       //?Metto il potenziale in categoria "E" --> Eliminabile
047000140811         chain wWAPcpo TNCPO01L;
047100140811         IF  %found(TNCPO01L);
047200140811           CPOfls = 'E';
047300140811           update TNCPO000;
047400140811           StpPotEli += 1;
047500140811         ENDIF;
047600140811
047700140811       ENDSR;
047800140911
047900140911       //--------------------------------------------------------------
048000140911       //?Scrivo nuova attività.
048100140911       //--------------------------------------------------------------
048200140911       BEGSR NewAttivita;
048300140911
048400140911       //?Stacco nuovo numero attività
048500140911         exsr StaccaNumero;
048600140911       //?se errore vado subito a fine
048700140911         IF  O33Err <> 0;
048800140911           leavesr;
048900140911         ENDIF;
049000140911
049100140911       //?Scrivo attività 60 già chiusa per pot.eliminabile
049200140911         clear TIATC000;
049300140911
049400140911         ATCtat  = 'T';
049500140911         ATCatn  = O33Nrf;
049600140911         ATCatnp = 1;
049700140911         ATCcpo  = CPOcpo;
049800140911
049900140911         ATCcad  = '60';
050000140911         ATCdad  = %dec(%date());
050100140911         ATChda  = %dec(%time());
050200140911
050300140911         ATCcmm = 0460000;
050400140911
050500140911       //?Scrivo l'attività già chiusa per pot.eliminabile
050600140911         ATCdco  = %dec(%date());
050700140911         ATChco  = %dec(%time());
050800140911         ATCcco  = ATCcmm;
050900140911         ATCpru  = 'BATCH';
051000140911         ATCcac  = '15';
051100140911
051200140911         ATCdim  = %dec(%date());
051300140911         ATChim  = %dec(%time());
051400140911         ATCpri  = 'BATCH';
051500140911
051600140911       //?Attività CREATA potenziale
051700140911         ATCcnw = 'P';
051800140911
051900140911       //?Memorizzo la categoria del potenziale
052000140911         §ATCcapo1 = CPOfls;
052100140911         §ATCcapo2 = CPOfls;
052200140911
052300140911       ENDSR;
052400140811
052500140811       //--------------------------------------------------------------
052600140811       //?Stacco nuovo numero attività.
052700140811       //--------------------------------------------------------------
052800140911       BEGSR StaccaNumero;
052900140811
053000140811         clear TRUL33ds;
053100140811         I33tla = 'L';
053200140811         I33ope = 0;
053300140811         I33cnu = 070;
053400140811         I33num = 1;
053500140811         I33aaa = *YEAR;
053600140811         kpjbu  = TRUL33ds;
053700140811         trul33r (kpjba);
053800140811         TRUL33ds = kpjbu;
053900140811
054000140811       ENDSR;
054100140911
054200140911       //--------------------------------------------------------------
054300140911       //?Scrivo la nota legata all'attività.
054400140911       //--------------------------------------------------------------
054500140911       BEGSR Nota;
054600140911
054700140911         clear TRMK20ds;
054800140911         IMK20tla  = 'L';
054900140911         IMK20flm  = 'C';
055000140911         IMK20fno  = 'S';
055100140911         IMK20cpo  = ATCcpo;
055200140911         IMK20ksc  = ATCksc;
055300140911         IMK20tat  = ATCtat;
055400140911         IMK20atn  = ATCatn;
055500140911         IMK20atnp = ATCatnp;
055600140911         EMK20no1 = 'CRIBIS';
055700140911         trmk20r (kpjba : TRMK20ds);
055800140911       ENDSR;
055900140514
056000140514       //--------------------------------------------------------------
056100140514       //?Operazioni finali.
056200140514       //--------------------------------------------------------------
056300140514       BEGSR RoutEnd;
056400140808
056500140808       //?Stampo la fine elaborazione
056600140811         write ACPO05E4;
056700140514
056800140514         *inLR = *on;
056900140514         return;
057000140514
057100140514       ENDSR;
057200140514
057300140514      /end-free
