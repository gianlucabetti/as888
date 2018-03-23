000100140514      //--------------------------------------------------------------
000200140808      //?ACPO04R - Crea WFACPN0F e WFACPV0F
000300140808      //?          da WFACP00F
000400140514      //--------------------------------------------------------------
000500140514
000600140514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700140514
000800140514      //---------------------------------------------------------------
000900140514      //?Dichiarazione file.
001000140514      //---------------------------------------------------------------
001100140808      // - File Potenziali da NON Aggiornare
001200140808     fWFACPN0F  o    e             disk
001300140807
001400140808      // - File Potenziali da Aggiornare
001500140807     fWFACPV0F  o    e             disk
001600140514
001700140808      // - File di stampa
001800140808     fACPO04P   o    e             printer oflind(*in99)
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
004500140808
004600140808      // - File Potenziali aggiornati da CRIBIS
004700140808     d WFACP00F      e ds                  extname(WFACP00F)
004800140514
004900140514      //---------------------------------------------------------------
005000140514      //?Definizione variabili globali.
005100140514      //---------------------------------------------------------------
005200140514      // - Flags booleani
005300140808     d wEoF            s               n   inz(*off)
005400140514     d wFine           s               n   inz(*off)
005500140514
005600140514      // - Campi di comodo data
005700140808     d wData_EUR       s               d   datfmt(*eur)
005800140808     d wData_ISO       s               d   datfmt(*iso)
005900140514     d wOggi           s              8  0 inz
006000140514
006100140514      // - Campi di comodo
006200140808     d wTpEla          s              1a   inz
006300140514
006400140514      //---------------------------------------------------------------
006500140514      //?Definizione procedure usate.
006600140514      //---------------------------------------------------------------
006700140514
006800140514      //---------------------------------------------------------------
006900140807      //?Definizione prototipi.
007000140514      //---------------------------------------------------------------
007100140514      /copy gaitrasrc/srcprotopr,TIBS34R
007200140514
007300140514      //---------------------------------------------------------------
007400140514      //?Definizione key-list.
007500140514      //---------------------------------------------------------------
007600140514
007700140514      //---------------------------------------------------------------
007800140514      //?M A I N - L I N E
007900140514      //---------------------------------------------------------------
008000140514
008100140514     c     *entry        plist
008200140514     c                   parm                    KPJBA
008300140514
008400140514      /free
008500140514
008600140514       //?Operazioni iniziali
008700140514       exsr RoutInz;
008800140514
008900140808       //?Elaborazioni
009000140808       IF  wTpEla <> *blanks;
009100140808         exsr Conta;
009200140808       ENDIF;
009300140808       IF  wTpEla = 'W';
009400140808         exsr Scrivi;
009500140808       ENDIF;
009600140514
009700140514       //?Operazioni finali
009800140514       exsr RoutEnd;
009900140514
010000140514       //--------------------------------------------------------------
010100140514       //?Operazioni iniziali.
010200140514       //--------------------------------------------------------------
010300140514       BEGSR RoutInz;
010400140808
010500140808         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010600140808
010700140808       //?Imposto il tipo di elaborazione da fare
010800140808       //?Se blank metto di dft il conteggio
010900140808       //?C=Conteggio dei rcd da aggiornare e non per Situazione 1 e 2
011000140808       //?W=Crea i file per Situazione 1 e 2  e conteggio
011100140808         IF  %subst(kpjbu:1:1) = *blanks;
011200140808           wTpEla = 'C';
011300140808         ELSE;
011400140808           wTpEla = %subst(kpjbu:1:1);
011500140808         ENDIF;
011600140808         IF  wTpEla <> 'C' and wTpEla <> 'W';
011700140808           clear wTpEla;
011800140808         ENDIF;
011900140808
012000140808         clear StpTpEla;
012100140808         IF  wTpEla = 'C';
012200140808           StpTpEla = 'Conteggio dei potenziali';
012300140808         ENDIF;
012400140808         IF  wTpEla = 'W';
012500140808           StpTpEla = 'Scrive File + Conteggio';
012600140808         ENDIF;
012700140808
012800140808       //?Se elaborazione 'W' devo prima cancellare i dati dai file di work
012900140808         IF  wTpEla = 'W';
013000140808           exec sql
013100140808           DELETE
013200140808           FROM  WFACPN0F;
013300140808
013400140808           exec sql
013500140808           DELETE
013600140808           FROM  WFACPV0F;
013700140808         ENDIF;
013800080206
013900100326       //?Reperimento dati job
014000080206         exsr DatiJob;
014100140808
014200140808       //?Data e ora sistema
014300140808         wOggi = %dec(%date());
014400140808         wData_ISO = %date(wOggi:*ISO);
014500140808         wData_EUR = wData_ISO;
014600140808         DataEla   = %dec(wData_EUR);
014700140808         OraEla    = %dec(%time());
014800140808
014900140808       //?Stampo testata
015000140808         write ACPO04T0;
015100140514
015200140514       ENDSR;
015300140514
015400140514       //--------------------------------------------------------------
015500140514       //?Reperimento Dati del job.
015600140514       //--------------------------------------------------------------
015700140514       BEGSR DatiJob;
015800140514
015900140514         in(E) §AzUte;
016000140514         IF  not %error;
016100140514           in(E) §DatiUte;
016200140514         ENDIF;
016300140514         IF  %error or rsut = *blanks;
016400140514           clear TIBS34ds;
016500140514           tibs34r(tibs34ds);
016600140514           in §AzUte;
016700140514           in §DatiUte;
016800140514         ENDIF;
016900140514
017000140514       ENDSR;
017100140808
017200140808       //--------------------------------------------------------------
017300140808       //?Conto i potenziali.
017400140808       //--------------------------------------------------------------
017500140808       BEGSR Conta;
017600140808
017700140808       //?Conto quanti potenziali situazione 1
017800140808         exec sql
017900140808         SELECT count(*) into :totpts1
018000140808         FROM   WFACP00F
018100140808         WHERE  WAPconf <> '  ';
018200140808
018300140808       //?Conto quanti potenziali situazione 1 saranno da aggiornare
018400140808         exec sql
018500140808         SELECT count(*) into :totpts1agg
018600140808         FROM   WFACP00F
018700140808         WHERE  WAPconf >= '07' and WAPsimn >= 30 and WAPsima >= 30;
018800140808
018900140808       //?Conto quanti potenziali situazione 1 NON saranno da aggiornare
019000140808         totpts1nag = totpts1 - totpts1agg;
019100140808
019200140808       //?Conto quanti potenziali situazione 2
019300140808         exec sql
019400140808         SELECT count(*) into :totpts2
019500140808         FROM   WFACP00F
019600140808         WHERE  WAPconf = '  ';
019700140808
019800140808       //?Conto quanti potenziali situazione 2 saranno da aggiornare
019900140808         exec sql
020000140808         SELECT count(*) into :totpts2agg
020100140808         FROM   WFACP00F
020200140808         WHERE  WAPconf = '  ' and WAPmgrn >= 30 and WAPmgra >= 30;
020300140808
020400140808       //?Conto quanti potenziali situazione 2 NON saranno da aggiornare
020500140808         totpts2nag = totpts2 - totpts2agg;
020600140808
020700140808       //?Stampo il conteggio
020800140808         write ACPO04D1;
020900140808
021000140808       ENDSR;
021100140808
021200140808       //--------------------------------------------------------------
021300140808       //?Scrive file di work.
021400140808       //--------------------------------------------------------------
021500140808       BEGSR Scrivi;
021600140808
021700140808       //?Leggo WFACP00F
021800140808         exec sql
021900140808         DECLARE WFACP cursor for select *
022000140808         FROM    WFACP00F;
022100140808
022200140808         exec sql
022300140808         OPEN WFACP;
022400140808         IF  sqlcode < 0;
022500140808           wEoF = *on;
022600140808         ENDIF;
022700140808
022800140808         DOW  not wEoF;
022900140808           exec sql
023000140808           FETCH NEXT from WFACP into :WFACP00F;
023100140808           IF  sqlcod = 100 or sqlcod < 0;
023200140808             wEoF = *on;
023300140808             leave;
023400140808           ENDIF;
023500140808
023600140808         //?Situazione 1
023700140808           IF  WAPconf <> *blanks;
023800140808           //?Scrivo il file dei potenziali da aggiornare
023900140808             IF  WAPconf >= '07' and WAPsimn >= 30 and WAPsima >= 30;
024000140808               write WFACPV00;
024100140808           //?Scrivo il file dei potenziali da NON aggiornare
024200140808             ELSE;
024300140808               write WFACPN00;
024400140808             ENDIF;
024500140808           ENDIF;
024600140808
024700140808         //?Situazione 2
024800140808           IF  WAPconf = *blanks;
024900140808           //?Scrivo il file dei potenziali da aggiornare
025000140808             IF  WAPmgrn >= 30 and WAPmgra >= 30;
025100140808               write WFACPV00;
025200140808           //?Scrivo il file dei potenziali da NON aggiornare
025300140808             ELSE;
025400140808               write WFACPN00;
025500140808             ENDIF;
025600140808           ENDIF;
025700140808
025800140808         ENDDO;
025900140808
026000140808         exec sql
026100140808         CLOSE WFACP;
026200140808
026300140808       //?Conto i rcd scritti nei 2 file
026400140808         exec sql
026500140808         SELECT count(*) into :stpnrracpv
026600140808         FROM   WFACPV0f;
026700140808
026800140808         exec sql
026900140808         SELECT count(*) into :stpnrracpn
027000140808         FROM   WFACPN0f;
027100140808
027200140808       //?Stampo
027300140808         write ACPO04D2;
027400140808
027500140808       ENDSR;
027600140514
027700140514       //--------------------------------------------------------------
027800140514       //?Operazioni finali.
027900140514       //--------------------------------------------------------------
028000140514       BEGSR RoutEnd;
028100140808
028200140808       //?Stampo la fine elaborazione
028300140808         write ACPO04E3;
028400140514
028500140514         *inLR = *on;
028600140514         return;
028700140514
028800140514       ENDSR;
028900140514
029000140514      /end-free
