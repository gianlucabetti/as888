000100120925      //--------------------------------------------------------------
000200121015      //?TRMKS01R - Pulizia CRM - Att. Ferie Ufficio AA + note
000300120925      //--------------------------------------------------------------
000400120925
000500120925     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120925
000700120925      //---------------------------------------------------------------
000800120925      //?Dichiarazione file.
000900120925      //---------------------------------------------------------------
001000120928
001100120928       // -?File Attivita
001200120928     fTIATC00F  uf   e             disk    prefix(w)
001300120928
001400120928       // -?File Note
001500120928     fTICPN03L  uf   e           k disk
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
003200121015
003300121015       // -?KPJBA
003400121015     d kpjba         e ds
003500121107     d  DataParm             247    254s 0
003600120928
003700120928       // -?File TIATC00F
003800120928     d TIATCds       e ds                  extname(TIATC00F)
003900120925
004000120925      //---------------------------------------------------------------
004100120925      //?Definizione variabili globali.
004200120925      //---------------------------------------------------------------
004300120925
004400120925       // -?Flags booleani
004500120925     d wEoF            s               n   inz(*off)
004600120928
004700120928       // -?Campi Rnn
004800120928     d Atc_Rrn         s              9  0
004900120925
005000120925       // -?Campi di comodo data
005100120928     d DataISO         s               d   datfmt(*iso)
005200121107     d DataPul         s              8  0 inz
005300121107     d DataSav         s              8  0 inz
005400120925
005500120925      //---------------------------------------------------------------
005600120925      //?Definizione procedure esterne.
005700120925      //---------------------------------------------------------------
005800120925
005900120925      //---------------------------------------------------------------
006000120925      //?Definizione prototipi.
006100120925      //---------------------------------------------------------------
006200120925
006300120925      //---------------------------------------------------------------
006400120925      //?Definizione key-list.
006500120925      //---------------------------------------------------------------
006600120925
006700120925      //---------------------------------------------------------------
006800120925      //?Riepilogo indicatori.
006900120925      //---------------------------------------------------------------
007000120925
007100120925      //---------------------------------------------------------------
007200120925
007300120925      //---------------------------------------------------------------
007400120925      //?M A I N - L I N E
007500120925      //---------------------------------------------------------------
007600121015
007700121015     c     *entry        plist
007800121015     c                   parm                    KPJBA
007900120925
008000120925      /free
008100120925
008200120925       //?Operazioni iniziali
008300120925       exsr RoutInz;
008400120925
008500120925       //?Elabora
008600120928       exsr Pul_Att_U_F;
008700121001       exsr Pul_Att_AA;
008800120925
008900120925       //?Operazioni finali
009000120925       exsr RoutEnd;
009100120925
009200120925       //--------------------------------------------------------------
009300120925       //?Operazioni iniziali.
009400120925       //--------------------------------------------------------------
009500120925       BEGSR RoutInz;
009600120925
009700120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009800120927
009900121107       //?Salvo la data passata
010000121107         DataSav = DataParm;
010100121107
010200121025       //?Alla data pulizia aggiungo 1 gg
010300121107         DataISO = %date(DataParm:*ISO);
010400121025         DataISO = DataISO + %days(1);
010500120928         DataPul = %dec(DataISO);
010600120925
010700120925       ENDSR;
010800120928
010900120928       //--------------------------------------------------------------
011000120928       //?Pulisco tutte le attività Uffico e Ferie.
011100120928       //--------------------------------------------------------------
011200120928       BEGSR Pul_Att_U_F;
011300120928
011400120928         wEoF = *off;
011500120927
011600120928       //?Leggo tutte le attività Ferie e Ufficio
011700120928       //?con data attività minore data pulizia
011800120928         exec sql
011900120928          DECLARE UFF cursor for
012000120928          SELECT rrn(TIATC00F), TIATC00F.* from TIATC00F
012100120928          WHERE ATCtat in('F', 'U') and ATCdad > 0 and
012200120928                ATCdad < :DataPul
012300120928          ORDER BY ATCtat, ATCatn, ATCatnp;
012400120928
012500120928         exec sql
012600120928           open UFF;
012700120928
012800120928         IF sqlcode < 0;
012900120928           wEoF = *on;
013000120928           leavesr;
013100120928         ENDIF;
013200120928
013300120928         DOW  not wEoF;
013400120928
013500120928           exec sql fetch next from UFF into :Atc_Rrn, :TIATCds;
013600120928           IF  sqlcod = 100 or sqlcod < 0;
013700120928             wEoF = *on;
013800120928             leave;
013900120928           ENDIF;
014000120928
014100120928         //?Se ci sono note legate all'attività da pulire le cancello
014200120928           setll (0:ATCtat:ATCatn) TICPN03L;
014300120928           reade (0:ATCtat:ATCatn) TICPN03L;
014400120928             DOW not %eof(TICPN03L);
014500120928               DELETE  TICPN000;
014600120928               reade (0:ATCtat:ATCatn) TICPN03L;
014700120928             ENDDO;
014800120928
014900120928         //?Deleto l'attività
015000120928           chain Atc_Rrn TIATC00F;
015100120928           IF  %found(TIATC00F);
015200120928             DELETE  TIATC000;
015300120928           ENDIF;
015400120928
015500120928         ENDDO;
015600120928
015700120928         exec sql close UFF;
015800120928         wEoF = *off;
015900120928
016000120928       ENDSR;
016100121001
016200121001       //--------------------------------------------------------------
016300121001       //?Pulisco tutte le attività AA.
016400121001       //--------------------------------------------------------------
016500121001       BEGSR Pul_Att_AA;
016600121001
016700121001         wEoF = *off;
016800121001
016900121001       //?Leggo tutte le attività AA
017000121001       //?con data esecuzione minore data pulizia
017100121001         exec sql
017200121001          DECLARE ATTAA cursor for
017300121001          SELECT rrn(TIATC00F), TIATC00F.* from TIATC00F
017400121001          WHERE ATCcac = 'AA' and ATCdco > 0 and
017500121001                ATCdco < :DataPul
017600121001          ORDER BY ATCtat, ATCatn, ATCatnp;
017700121001
017800121001         exec sql
017900121001           open ATTAA;
018000121001
018100121001         IF sqlcode < 0;
018200121001           wEoF = *on;
018300121001           leavesr;
018400121001         ENDIF;
018500121001
018600121001         DOW  not wEoF;
018700121001
018800121001           exec sql fetch next from ATTAA into :Atc_Rrn, :TIATCds;
018900121001           IF  sqlcod = 100 or sqlcod < 0;
019000121001             wEoF = *on;
019100121001             leave;
019200121001           ENDIF;
019300121001
019400121001         //?Se ci sono note legate all'attività da pulire
019500121001         //?le modifico pulendo i campi Tipo attività, nr. attività e progressivo
019600121001           setll (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
019700121001           reade (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
019800121001             DOW not %eof(TICPN03L);
019900121001               clear CPNtat;
020000121001               clear CPNatn;
020100121001               clear CPNatnp;
020200121001               UPDATE  TICPN000;
020300121001               reade (ATCcpo:ATCtat:ATCatn:ATCatnp) TICPN03L;
020400121001             ENDDO;
020500121025
020600121025         //?Se l'attività da pulire è con progressivo 1
020700121025         //?pulire i campi tipo attività, nr.attività e progressivo
020800121025         //?anche sulle note con progressivo 0
020900121025           IF  ATCatnp = 1;
021000121025             setll (ATCcpo:ATCtat:ATCatn:0) TICPN03L;
021100121025             reade (ATCcpo:ATCtat:ATCatn:0) TICPN03L;
021200121025             DOW not %eof(TICPN03L);
021300121025               clear CPNtat;
021400121025               clear CPNatn;
021500121025               clear CPNatnp;
021600121025               UPDATE  TICPN000;
021700121025               reade (ATCcpo:ATCtat:ATCatn:0) TICPN03L;
021800121025             ENDDO;
021900121025           ENDIF;
022000121001
022100121001         //?Deleto l'attività
022200121001           chain Atc_Rrn TIATC00F;
022300121001           IF  %found(TIATC00F);
022400121001             DELETE  TIATC000;
022500121001           ENDIF;
022600121001
022700121001         ENDDO;
022800121001
022900121001         exec sql close ATTAA;
023000121001         wEoF = *off;
023100121001
023200121001       ENDSR;
023300120925
023400120925       //--------------------------------------------------------------
023500120925       //?Operazioni finali.
023600120925       //--------------------------------------------------------------
023700120925       BEGSR RoutEnd;
023800121107
023900121107       //?Reimposto la data passata
024000121107         DataParm = DataSav;
024100120925
024200120925         *inLR = *on;
024300120925         return;
024400120925
024500120925       ENDSR;
024600120925
024700120925      /end-free
