000100121025      //--------------------------------------------------------------
000200121025      //?TRMKS03R - Pulizia Attività fuori trattativa.
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
001500121113     fTICPN05L  uf   e           k disk
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
003200121025
003300121025       // -?KPJBA
003400121025     d kpjba         e ds
003500121107     d  DataParm             247    254s 0
003600121002
003700121002       // -?File TIVIS00F
003800121002     d TIVISds       e ds                  extname(TIVIS00F)
003900121002
004000121002       // -?File TNCPO00F
004100121002     d TNCPOds       e ds                  extname(TNCPO00F)
004200120925
004300120925      //---------------------------------------------------------------
004400120925      //?Definizione variabili globali.
004500120925      //---------------------------------------------------------------
004600120925
004700120925       // -?Flags booleani
004800120925     d wEoF            s               n   inz(*off)
004900121002     d wEoF_1          s               n   inz(*off)
005000120925
005100120925       // -?Campi di comodo data
005200120928     d DataISO         s               d   datfmt(*iso)
005300121107     d DataPul         s              8  0 inz
005400121107     d DataSav         s              8  0 inz
005500121002
005600121002       // -?Campi di comodo contatori
005700121002     d contaATC        s              5s 0
005800121002     d diffATC         s              5s 0
005900121002     d magATC          s              5s 0
006000121002     d maxATC          s              5s 0 inz(5)
006100121002     d minATC          s              5s 0
006200121002     d nrATC           s              5s 0
006300121002     d nrVIS           s              5s 0
006400121002     d pulATC          s              5s 0
006500120925
006600120925      //---------------------------------------------------------------
006700120925      //?Definizione procedure esterne.
006800120925      //---------------------------------------------------------------
006900120925
007000120925      //---------------------------------------------------------------
007100120925      //?Definizione prototipi.
007200120925      //---------------------------------------------------------------
007300120925
007400120925      //---------------------------------------------------------------
007500120925      //?Definizione key-list.
007600120925      //---------------------------------------------------------------
007700120925
007800120925      //---------------------------------------------------------------
007900120925      //?Riepilogo indicatori.
008000120925      //---------------------------------------------------------------
008100120925
008200120925      //---------------------------------------------------------------
008300120925
008400120925      //---------------------------------------------------------------
008500120925      //?M A I N - L I N E
008600120925      //---------------------------------------------------------------
008700121025
008800121025     c     *entry        plist
008900121025     c                   parm                    KPJBA
009000120925
009100120925      /free
009200120925
009300120925       //?Operazioni iniziali
009400120925       exsr RoutInz;
009500120925
009600121002       //?Leggo i potenziali già contattati
009700121002       exsr Leggi_CPO;
009800120925
009900120925       //?Operazioni finali
010000120925       exsr RoutEnd;
010100120925
010200120925       //--------------------------------------------------------------
010300120925       //?Operazioni iniziali.
010400120925       //--------------------------------------------------------------
010500120925       BEGSR RoutInz;
010600120925
010700120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010800121107
010900121107       //?Salvo la data passata
011000121107         DataSav = DataParm;
011100120927
011200121025       //?Alla data pulizia aggiungo 1 gg
011300121107         DataISO = %date(DataParm:*ISO);
011400121025         DataISO = DataISO + %days(1);
011500121025         DataPul = %dec(DataISO);
011600120925
011700120925       ENDSR;
011800120928
011900120928       //--------------------------------------------------------------
012000121002       //?Lettura dei potenziali già contattati.
012100120928       //--------------------------------------------------------------
012200121002       BEGSR Leggi_CPO;
012300121001
012400121001         wEoF = *off;
012500121001
012600121002       //?Leggo i potenziali SOLO quelli contattati
012700121210       //?e NON eliminabili
012800121001         exec sql
012900121002          DECLARE CPO1 cursor for
013000121002          SELECT CPOcpo from TNCPO00F
013100121002          WHERE substr(CPOrst, 13, 8) <> '        ' and
013200121210                substr(CPOrst, 13, 8) <> '00000000' and
013300121210                CPOfls <> 'E'
013400121002          ORDER BY CPOcpo;
013500121001
013600121001         exec sql
013700121002           open CPO1;
013800121001
013900121001         IF sqlcode < 0;
014000121001           wEoF = *on;
014100121001           leavesr;
014200121001         ENDIF;
014300121001
014400121001         DOW  not wEoF;
014500121001
014600121002           exec sql fetch next from CPO1 into :CPOcpo;
014700121001           IF  sqlcod = 100 or sqlcod < 0;
014800121001             wEoF = *on;
014900121001             leave;
015000121001           ENDIF;
015100121002
015200121025         //?Controllo se per il potenziale esistono trattative
015300121002           exec sql
015400121002           SELECT count(*) into :nrVIS from TIVIS00F
015500121025           WHERE VIScpo = :CPOcpo;
015600121002
015700121002           SELECT;
015800121002
015900121025           //?Se NON ci sono trattative devo verificare quante attività
016000121002           //?eseguite ci sono, ne devono restare almeno 5
016100121002             WHEN nrVIS = 0;
016200121002               exsr  Pul_ATC;
016300121002
016400121025           //?Se ho trovato anche 1 sola trattativa posso cancellare
016500121002           //?tutte le attività fuori trattativa con data esecuzione < data pulizia
016600121002             WHEN nrVIS > 0;
016700121002               clear pulATC;
016800121002               exsr  Pul_ATC_F;
016900121002
017000121002           ENDSL;
017100121001
017200121001         ENDDO;
017300121001
017400121002         exec sql close CPO1;
017500121001         wEoF = *off;
017600121001
017700121001       ENDSR;
017800121002
017900121002       //--------------------------------------------------------------
018000121002       //?Pulisco le attività, ne devono restare 5.
018100121002       //--------------------------------------------------------------
018200121002       BEGSR Pul_ATC;
018300121002
018400121002       //?Conto quante attività eseguite ci sono per il potenziale
018500121002       //?ne devo tenere al massimo 5
018600121003       //?non considero le eventuali 'AA' fatte negli ultimi 2 anni
018700121002           exec sql
018800121002           SELECT count(*) into :nrATC from TIATC00F
018900121003           WHERE ATCcpo = :CPOcpo and ATCdco > 0 and ATCcac <> 'AA';
019000121002
019100121002         //?Se ho trovato più di 5 attività
019200121002           IF  nrATC > maxATC;
019300121002
019400121002           //?Conto quante attività eseguite ci sono per il potenziale
019500121002           //?con data esecuzione >= data pulizia
019600121003           //?escludo le 'AA'
019700121002             exec sql
019800121002             SELECT count(*) into :magATC from TIATC00F
019900121002             WHERE ATCcpo = :CPOcpo and ATCdco > 0 and
020000121003                   ATCdco >= :DataPul and ATCcac <> 'AA';
020100121002           //?Se sono >= 5
020200121002           //?pulisco tutte le attività con data esecuzione < data pulizia
020300121002             IF  magATC >= maxATC;
020400121002               clear pulATC;
020500121002               exsr  Pul_ATC_F;
020600121002               leavesr;
020700121002             ENDIF;
020800121002
020900121002           //?Conto quante attività eseguite ci sono per il potenziale
021000121002           //?con data esecuzione < data pulizia
021100121008           //?non considero le 'AA' in quanto sono già state pulite prima
021200121002             exec sql
021300121002             SELECT count(*) into :minATC from TIATC00F
021400121002             WHERE ATCcpo = :CPOcpo and ATCdco > 0 and
021500121002                   ATCdco < :DataPul;
021600121002           //?cerco quante attività devo pulire
021700121002           //?dal nr. di attività minori data pulizia tolgo la differenza
021800121002           //?tra 5 (nr. max attività da tenere) e il nr. attività maggiori data pulizia
021900121002           //?poi pulisco le attività che sono in più
022000121002             pulATC = minATC - (maxATC - magATC);
022100121002             IF  pulAtc > 0;
022200121002               exsr  Pul_ATC_F;
022300121002               leavesr;
022400121002             ENDIF;
022500121002
022600121002           leavesr;
022700121002         ENDIF;
022800121002
022900121002       ENDSR;
023000121002
023100121002       //--------------------------------------------------------------
023200121002       //?Pulisco le attività fuori trattativa.
023300121002       //--------------------------------------------------------------
023400121002       BEGSR Pul_ATC_F;
023500121002
023600121002         wEoF_1 = *off;
023700121002         clear contaATC;
023800121002
023900121002       //?Leggo le attività eseguite fuori trattativa
024000121002         setll (CPOcpo) TIATC05L;
024100121002
024200121002         DOW not wEoF_1;
024300121002           reade (CPOcpo) TIATC05L;
024400121002
024500121002           IF  %eof(TIATC05L);
024600121002             leavesr;
024700121002           ENDIF;
024800121002
024900121002         //?Data esecuzione < data pulizia
025000121002           IF  ATCdco = 0;
025100121002             iter;
025200121002           ENDIF;
025300121002           IF  ATCdco >= DataPul;
025400121002             leavesr;
025500121002           ENDIF;
025600121002
025700121002         //?Attività fuori trattativa
025800121002           IF  ATCnrv > 0;
025900121002             iter;
026000121002           ENDIF;
026100121002
026200121002         //?Ho raggiunto il nr. massimo di attività da pulire
026300121003           IF  pulATC > 0 and contaATC >= pulATC;
026400121002             leavesr;
026500121002           ENDIF;
026600121002
026700121002           contaATC += 1;
026800121002
026900121002         //?Se ci sono note legate all'attività da pulire
027000121002         //?le modifico pulendo i campi key attività
027100121113           setll (ATCcpo:ATCatn:ATCatnp) TICPN05L;
027200121113           reade (ATCcpo:ATCatn:ATCatnp) TICPN05L;
027300121113             DOW not %eof(TICPN05L);
027400121002               clear CPNtat;
027500121002               clear CPNatn;
027600121002               clear CPNatnp;
027700121002               UPDATE  TICPN000;
027800121113               reade (ATCcpo:ATCatn:ATCatnp) TICPN05L;
027900121002             ENDDO;
028000121025
028100121025         //?Se l'attività da pulire è con progressivo 1
028200121025         //?pulire i campi tipo attività, nr.attività e progressivo
028300121025         //?anche sulle note con progressivo 0
028400121025           IF  ATCatnp = 1;
028500121113             setll (ATCcpo:ATCatn:0) TICPN05L;
028600121113             reade (ATCcpo:ATCatn:0) TICPN05L;
028700121113             DOW not %eof(TICPN05L);
028800121025               clear CPNtat;
028900121025               clear CPNatn;
029000121025               clear CPNatnp;
029100121025               UPDATE  TICPN000;
029200121113               reade (ATCcpo:ATCatn:0) TICPN05L;
029300121025             ENDDO;
029400121025           ENDIF;
029500121002
029600121002         //?Deleto l'attività
029700121002           DELETE  TIATC000;
029800121002
029900121002         ENDDO;
030000121002
030100121002       ENDSR;
030200120925
030300120925       //--------------------------------------------------------------
030400120925       //?Operazioni finali.
030500120925       //--------------------------------------------------------------
030600120925       BEGSR RoutEnd;
030700121107
030800121107       //?Reimposto la data passata
030900121107         DataParm = DataSav;
031000120925
031100120925         *inLR = *on;
031200120925         return;
031300120925
031400120925       ENDSR;
031500120925
031600120925      /end-free
