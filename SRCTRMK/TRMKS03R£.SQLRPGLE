000100120925      //--------------------------------------------------------------
000200120925      //?PULCRM - Pulizia CRM
000300120925      //--------------------------------------------------------------
000400120925
000500120925     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120925
000700120925      //---------------------------------------------------------------
000800120925      //?Dichiarazione file.
000900120925      //---------------------------------------------------------------
001000120928
001100121001       // -?File Trattative
001200121002     fTIATC05L  uf   e           k disk
001300120928
001400120928       // -?File Note
001500121002     fTICPN03L  uf   e           k disk
001600120925
001700120925      //---------------------------------------------------------------
001800120925      //?Definizione costanti.
001900120925      //---------------------------------------------------------------
002000120925
002100120925      //---------------------------------------------------------------
002200120925      //?Definizione schiere.
002300120925      //---------------------------------------------------------------
002400120925
002500120925      //---------------------------------------------------------------
002600120925      //?Definizione aree dati.
002700120925      //---------------------------------------------------------------
002800120925
002900120925      //---------------------------------------------------------------
003000120925      //?Definizione strutture dati.
003100120925      //---------------------------------------------------------------
003200121002
003300121002       // -?File TIVIS00F
003400121002     d TIVISds       e ds                  extname(TIVIS00F)
003500121002
003600121002       // -?File TNCPO00F
003700121002     d TNCPOds       e ds                  extname(TNCPO00F)
003800120925
003900120925      //---------------------------------------------------------------
004000120925      //?Definizione variabili globali.
004100120925      //---------------------------------------------------------------
004200120925
004300120925       // -?Flags booleani
004400120925     d wEoF            s               n   inz(*off)
004500121002     d wEoF_1          s               n   inz(*off)
004600120928
004700120928       // -?Campi Rnn
004800121002     d ATC_Rrn         s              9  0
004900120925
005000120925       // -?Campi di comodo data
005100121008     d DataPul         s              8s 0
005200120927     d DataEla         s              8s 0 inz(20130101)
005300120928     d DataISO         s               d   datfmt(*iso)
005400121002
005500121002       // -?Campi di comodo contatori
005600121002     d contaATC        s              5s 0
005700121002     d diffATC         s              5s 0
005800121002     d magATC          s              5s 0
005900121002     d maxATC          s              5s 0 inz(5)
006000121002     d minATC          s              5s 0
006100121002     d nrATC           s              5s 0
006200121002     d nrVIS           s              5s 0
006300121002     d pulATC          s              5s 0
006400120925
006500120925      //---------------------------------------------------------------
006600120925      //?Definizione procedure esterne.
006700120925      //---------------------------------------------------------------
006800120925
006900120925      //---------------------------------------------------------------
007000120925      //?Definizione prototipi.
007100120925      //---------------------------------------------------------------
007200120925
007300120925      //---------------------------------------------------------------
007400120925      //?Definizione key-list.
007500120925      //---------------------------------------------------------------
007600120925
007700120925      //---------------------------------------------------------------
007800120925      //?Riepilogo indicatori.
007900120925      //---------------------------------------------------------------
008000120925
008100120925      //---------------------------------------------------------------
008200120925
008300120925      //---------------------------------------------------------------
008400120925      //?M A I N - L I N E
008500120925      //---------------------------------------------------------------
008600120925
008700120925      /free
008800120925
008900120925       //?Operazioni iniziali
009000120925       exsr RoutInz;
009100120925
009200121002       //?Leggo i potenziali già contattati
009300121002       exsr Leggi_CPO;
009400120925
009500120925       //?Operazioni finali
009600120925       exsr RoutEnd;
009700120925
009800120925       //--------------------------------------------------------------
009900120925       //?Operazioni iniziali.
010000120925       //--------------------------------------------------------------
010100120925       BEGSR RoutInz;
010200120925
010300120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010400120927
010500120928       //?Calcolo la data pulizia
010600120928         DataISO = %date(DataEla:*ISO);
010700120928         DataISO = DataISO - %years(2);
010800120928         DataPul = %dec(DataISO);
010900120925
011000120925       ENDSR;
011100120928
011200120928       //--------------------------------------------------------------
011300121002       //?Lettura dei potenziali già contattati.
011400120928       //--------------------------------------------------------------
011500121002       BEGSR Leggi_CPO;
011600121001
011700121001         wEoF = *off;
011800121001
011900121002       //?Leggo i potenziali SOLO quelli contattati
012000121001         exec sql
012100121002          DECLARE CPO1 cursor for
012200121002          SELECT CPOcpo from TNCPO00F
012300121002          WHERE substr(CPOrst, 13, 8) <> '        ' and
012400121002                substr(CPOrst, 13, 8) <> '00000000'
012500121002          ORDER BY CPOcpo;
012600121001
012700121001         exec sql
012800121002           open CPO1;
012900121001
013000121001         IF sqlcode < 0;
013100121001           wEoF = *on;
013200121001           leavesr;
013300121001         ENDIF;
013400121001
013500121001         DOW  not wEoF;
013600121001
013700121002           exec sql fetch next from CPO1 into :CPOcpo;
013800121001           IF  sqlcod = 100 or sqlcod < 0;
013900121001             wEoF = *on;
014000121001             leave;
014100121001           ENDIF;
014200121002
014300121002         //?Controllo se per il potenziale esistono trattative chiuse
014400121002           exec sql
014500121002           SELECT count(*) into :nrVIS from TIVIS00F
014600121002           WHERE VIScpo = :CPOcpo and VISdch > 0;
014700121002
014800121002           SELECT;
014900121002
015000121002           //?Se NON ci sono trattative chiuse devo verificare quante attività
015100121002           //?eseguite ci sono, ne devono restare almeno 5
015200121002             WHEN nrVIS = 0;
015300121002               exsr  Pul_ATC;
015400121002
015500121002           //?Se ho trovato anche 1 sola trattativa chiusa posso cancellare
015600121002           //?tutte le attività fuori trattativa con data esecuzione < data pulizia
015700121002             WHEN nrVIS > 0;
015800121002               clear pulATC;
015900121002               exsr  Pul_ATC_F;
016000121002
016100121002           ENDSL;
016200121001
016300121001         ENDDO;
016400121001
016500121002         exec sql close CPO1;
016600121001         wEoF = *off;
016700121001
016800121001       ENDSR;
016900121002
017000121002       //--------------------------------------------------------------
017100121002       //?Pulisco le attività, ne devono restare 5.
017200121002       //--------------------------------------------------------------
017300121002       BEGSR Pul_ATC;
017400121002
017500121002       //?Conto quante attività eseguite ci sono per il potenziale
017600121002       //?ne devo tenere al massimo 5
017700121003       //?non considero le eventuali 'AA' fatte negli ultimi 2 anni
017800121002           exec sql
017900121002           SELECT count(*) into :nrATC from TIATC00F
018000121003           WHERE ATCcpo = :CPOcpo and ATCdco > 0 and ATCcac <> 'AA';
018100121002
018200121002         //?Se ho trovato più di 5 attività
018300121002           IF  nrATC > maxATC;
018400121002
018500121002           //?Conto quante attività eseguite ci sono per il potenziale
018600121002           //?con data esecuzione >= data pulizia
018700121003           //?escludo le 'AA'
018800121002             exec sql
018900121002             SELECT count(*) into :magATC from TIATC00F
019000121002             WHERE ATCcpo = :CPOcpo and ATCdco > 0 and
019100121003                   ATCdco >= :DataPul and ATCcac <> 'AA';
019200121002           //?Se sono >= 5
019300121002           //?pulisco tutte le attività con data esecuzione < data pulizia
019400121002             IF  magATC >= maxATC;
019500121002               clear pulATC;
019600121002               exsr  Pul_ATC_F;
019700121002               leavesr;
019800121002             ENDIF;
019900121002
020000121002           //?Conto quante attività eseguite ci sono per il potenziale
020100121002           //?con data esecuzione < data pulizia
020200121008           //?non considero le 'AA' in quanto sono già state pulite prima
020300121002             exec sql
020400121002             SELECT count(*) into :minATC from TIATC00F
020500121002             WHERE ATCcpo = :CPOcpo and ATCdco > 0 and
020600121002                   ATCdco < :DataPul;
020700121002           //?cerco quante attività devo pulire
020800121002           //?dal nr. di attività minori data pulizia tolgo la differenza
020900121002           //?tra 5 (nr. max attività da tenere) e il nr. attività maggiori data pulizia
021000121002           //?poi pulisco le attività che sono in più
021100121002             pulATC = minATC - (maxATC - magATC);
021200121002             IF  pulAtc > 0;
021300121002               exsr  Pul_ATC_F;
021400121002               leavesr;
021500121002             ENDIF;
021600121002
021700121002           leavesr;
021800121002         ENDIF;
021900121002
022000121002       ENDSR;
022100121002
022200121002       //--------------------------------------------------------------
022300121002       //?Pulisco le attività fuori trattativa.
022400121002       //--------------------------------------------------------------
022500121002       BEGSR Pul_ATC_F;
022600121002
022700121002         wEoF_1 = *off;
022800121002         clear contaATC;
022900121002
023000121002       //?Leggo le attività eseguite fuori trattativa
023100121002         setll (CPOcpo) TIATC05L;
023200121002
023300121002         DOW not wEoF_1;
023400121002           reade (CPOcpo) TIATC05L;
023500121002
023600121002           IF  %eof(TIATC05L);
023700121002             leavesr;
023800121002           ENDIF;
023900121002
024000121002         //?Data esecuzione < data pulizia
024100121002           IF  ATCdco = 0;
024200121002             iter;
024300121002           ENDIF;
024400121002           IF  ATCdco >= DataPul;
024500121002             leavesr;
024600121002           ENDIF;
024700121002
024800121002         //?Attività fuori trattativa
024900121002           IF  ATCnrv > 0;
025000121002             iter;
025100121002           ENDIF;
025200121002
025300121002         //?Ho raggiunto il nr. massimo di attività da pulire
025400121003           IF  pulATC > 0 and contaATC >= pulATC;
025500121002             leavesr;
025600121002           ENDIF;
025700121002
025800121002           contaATC += 1;
025900121002
026000121002         //?Se ci sono note legate all'attività da pulire
026100121002         //?le modifico pulendo i campi key attività
026200121002           setll (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
026300121002           reade (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
026400121002             DOW not %eof(TICPN03L);
026500121002               clear CPNtat;
026600121002               clear CPNatn;
026700121002               clear CPNatnp;
026800121002               UPDATE  TICPN000;
026900121002               reade (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
027000121002             ENDDO;
027100121002
027200121002         //?Deleto l'attività
027300121002           DELETE  TIATC000;
027400121002
027500121002         ENDDO;
027600121002
027700121002       ENDSR;
027800120925
027900120925       //--------------------------------------------------------------
028000120925       //?Operazioni finali.
028100120925       //--------------------------------------------------------------
028200120925       BEGSR RoutEnd;
028300120925
028400120925         *inLR = *on;
028500120925         return;
028600120925
028700120925       ENDSR;
028800120925
028900120925      /end-free
