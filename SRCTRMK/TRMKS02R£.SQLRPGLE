000100120925      //--------------------------------------------------------------
000200121015      //?TRMKS02R - Pulizia CRM - Trattative Fittizie + file legati
000300120925      //--------------------------------------------------------------
000400120925
000500120925     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120925
000700120925      //---------------------------------------------------------------
000800120925      //?Dichiarazione file.
000900120925      //---------------------------------------------------------------
001000121015
001100121015       // -?File Tabelle
001200121015     fTABEL00F  if   e           k disk
001300120928
001400120928       // -?File Note
001500121001     fTICPN04L  uf   e           k disk
001600121015
001700121015       // -?File INFO trattativa
001800121015     fTIVII01L  if   e           k disk
001900121015
002000121015       // -?File Trattative
002100121015     fTIVIS00F  uf   e             disk    prefix(w)
002200121015
002300121015       // -?File Offerte
002400121015     fTIVOF11L  if   e           k disk
002500121015
002600121015       // -?File Offerte
002700121015     fTNOFM01L  uf   e           k disk
002800121015
002900121015       // -?File Tabelle
003000121015     fTNTBE01L  if   e           k disk
003100120925
003200120925      //---------------------------------------------------------------
003300120925      //?Definizione costanti.
003400120925      //---------------------------------------------------------------
003500120925
003600120925      //---------------------------------------------------------------
003700120925      //?Definizione schiere.
003800120925      //---------------------------------------------------------------
003900121015
004000121015     d ITO             s              3    dim(50)
004100121015     d ITOds           s             70    dim(50)
004200121015     d ITOor           s              8    dim(50)
004300121015     d TTR_info        s              1    dim(50)
004400121015     d TTR_off         s              1    dim(50)
004500120925
004600120925      //---------------------------------------------------------------
004700120925      //?Definizione aree dati.
004800120925      //---------------------------------------------------------------
004900120925
005000120925      //---------------------------------------------------------------
005100120925      //?Definizione strutture dati.
005200120925      //---------------------------------------------------------------
005300121015
005400121015       // -?Tabella ITO = Info Trattativa
005500121015     d dITO          e ds                  inz
005600121015
005700121015       // -?Tabella ITS = Info Trattativa
005800121015     d dITS          e ds                  inz
005900121015
006000121015       // -?Tabella TTR = Tipo Trattativa
006100121015     d dTTR          e ds                  inz
006200121015
006300121015       // -?KPJBA
006400121015     d kpjba         e ds
006500121015     d  DataEla              247    254s 0
006600121015
006700121015       // -?Ricerca/Controllo tabelle
006800121015     d TIBS02ds      e ds                  inz
006900120928
007000121001       // -?File TIVIS00F
007100121001     d TIVISds       e ds                  extname(TIVIS00F)
007200120925
007300120925      //---------------------------------------------------------------
007400120925      //?Definizione variabili globali.
007500120925      //---------------------------------------------------------------
007600120925
007700120925       // -?Flags booleani
007800120925     d wEoF            s               n   inz(*off)
007900120928
008000120928       // -?Campi Rnn
008100121001     d VIS_Rrn         s              9  0
008200120925
008300120925       // -?Campi di comodo data
008400121008     d DataPul         s              8s 0
008500121015       // DataEla         s              8s 0 inz(20130101)
008600120928     d DataISO         s               d   datfmt(*iso)
008700120925
008800120925      //---------------------------------------------------------------
008900120925      //?Definizione procedure esterne.
009000120925      //---------------------------------------------------------------
009100120925
009200120925      //---------------------------------------------------------------
009300120925      //?Definizione prototipi.
009400120925      //---------------------------------------------------------------
009500120925
009600120925      //---------------------------------------------------------------
009700120925      //?Definizione key-list.
009800120925      //---------------------------------------------------------------
009900120925
010000120925      //---------------------------------------------------------------
010100120925      //?Riepilogo indicatori.
010200120925      //---------------------------------------------------------------
010300120925
010400120925      //---------------------------------------------------------------
010500120925
010600120925      //---------------------------------------------------------------
010700120925      //?M A I N - L I N E
010800120925      //---------------------------------------------------------------
010900121015
011000121015     c     *entry        plist
011100121015     c                   parm                    KPJBA
011200120925
011300120925      /free
011400120925
011500120925       //?Operazioni iniziali
011600120925       exsr RoutInz;
011700120925
011800120925       //?Elabora
011900121001       exsr Pul_Ntc_VisF;
012000121001       exsr Pul_Vis_F;
012100120925
012200120925       //?Operazioni finali
012300120925       exsr RoutEnd;
012400120925
012500120925       //--------------------------------------------------------------
012600120925       //?Operazioni iniziali.
012700120925       //--------------------------------------------------------------
012800120925       BEGSR RoutInz;
012900120925
013000120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
013100120927
013200120928       //?Calcolo la data pulizia
013300120928         DataISO = %date(DataEla:*ISO);
013400120928         DataISO = DataISO - %years(2);
013500120928         DataPul = %dec(DataISO);
013600121015
013700121015       //?Carico le INFO in sk
013800121015         clear xx;
013900121015         clear ITO;
014000121015         clear ITOds;
014100121015         clear ITOor;
014200121015         setll ('ITO') TNTBE01L;
014300121015         reade ('ITO') TNTBE01L;
014400121015         DOW not %eof(TNTBE01L);
014500121015           IF  TBEatb = *blanks;
014600121015             dITO = TBEuni;
014700121015             xx += 1;
014800121015             ITO(xx)   = TBEke1;
014900121015             ITOds(xx) = dITO;
015000121015             ITOor(xx) = §ITOitg + %editc(§ITOogi:'X') + TBEke1;
015100121015           ENDIF;
015200121015           reade ('ITO') TNTBE01L;
015300121015         ENDDO;
015400121015
015500121015       //?Ordino la sk delle INFO
015600121015         sorta ITOor;
015700121015
015800121015       //?Carico le sk per i tipi Trattativa
015900121015         clear xx;
016000121015         clear yy;
016100121015         setll ('TTR') TNTBE01L;
016200121015         reade ('TTR') TNTBE01L;
016300121015         DOW not %eof(TNTBE01L);
016400121015           IF  TBEatb = *blanks;
016500121015             dTTR = TBEuni;
016600121015             IF  §TTRinfo = 'S';
016700121015               xx += 1;
016800121015               TTR_info(xx)   = TBEke1;
016900121015             ENDIF;
017000121015             IF  §TTRoff = 'S';
017100121015               yy += 1;
017200121015               TTR_off(yy)   = TBEke1;
017300121015             ENDIF;
017400121015           ENDIF;
017500121015           reade ('TTR') TNTBE01L;
017600121015         ENDDO;
017700121015
017800121015       //?Imposto campi fissi
017900121015         k_TBLkut = 1;
018000120925
018100120925       ENDSR;
018200120928
018300120928       //--------------------------------------------------------------
018400121001       //?Pulisco tutte le note automatiche delle trattative fittizie.
018500120928       //--------------------------------------------------------------
018600121001       BEGSR Pul_Ntc_VisF;
018700120928
018800121001       //?Cancello le note automatiche delle trattative Fittizie
018900120928         exec sql
019000121001          DELETE from TICPN00F
019100121001          WHERE CPNnrv > 0 and CPNpru = 'BATCH' and CPNnot =
019200121001                'Trattativa modificata in FITTIZIA';
019300120928
019400120928       ENDSR;
019500121001
019600121001       //--------------------------------------------------------------
019700121001       //?Pulisco tutte le trattative fittizie.
019800121001       //--------------------------------------------------------------
019900121001       BEGSR Pul_Vis_F;
020000121001
020100121001         wEoF = *off;
020200121001
020300121001       //?Leggo tutte le trattative Fittizie
020400121001       //?con data apertura minore data pulizia
020500121001         exec sql
020600121001          DECLARE VISF cursor for
020700121001          SELECT rrn(TIVIS00F), TIVIS00F.* from TIVIS00F
020800121001          WHERE VISffz = 'S' and VISdat > 0 and
020900121001                VISdat < :DataPul
021000121001          ORDER BY VISnrv;
021100121001
021200121001         exec sql
021300121001           open VISF;
021400121001
021500121001         IF sqlcode < 0;
021600121001           wEoF = *on;
021700121001           leavesr;
021800121001         ENDIF;
021900121001
022000121001         DOW  not wEoF;
022100121001
022200121001           exec sql fetch next from VISF into :VIS_Rrn, :TIVISds;
022300121001           IF  sqlcod = 100 or sqlcod < 0;
022400121001             wEoF = *on;
022500121001             leave;
022600121001           ENDIF;
022700121001
022800121001         //?Se ci sono note legate alla trattativa da pulire
022900121001         //?le modifico pulendo i campi Nr.Trattativa
023000121001           setll (VIScpo:VISksc:VISnrv) TICPN04L;
023100121001           reade (VIScpo:VISksc:VISnrv) TICPN04L;
023200121001             DOW not %eof(TICPN04L);
023300121001               clear CPNnrv;
023400121001               UPDATE  TICPN000;
023500121001               reade (VIScpo:VISksc:VISnrv) TICPN04L;
023600121001             ENDDO;
023700121015
023800121015         //?Storicizzo le INFO trattative
023900121015           IF  %lookup(VIStpv : TTR_info) > 0;
024000121015             exsr Sto_INFO;
024100121015           ENDIF;
024200121015
024300121015         //?Pulisco le offerte
024400121015           IF  %lookup(VIStpv : TTR_off) > 0;
024500121015             exsr Pul_VOF;
024600121015           ENDIF;
024700121015
024800121015         //?Pulisco le anagrafiche provvisorie
024900121015           exsr Pul_ANAG;
025000121015
025100121015         //?Pulisco le attività legate alla trattativa
025200121015           exsr Pul_ATC;
025300121015
025400121015         //?Pulisco le INFO trattative
025500121015           IF  %lookup(VIStpv : TTR_info) > 0;
025600121015             exsr Pul_VII;
025700121015           ENDIF;
025800121001
025900121001         //?Deleto la trattativa
026000121001           chain VIS_Rrn TIVIS00F;
026100121001           IF  %found(TIVIS00F);
026200121001             DELETE  TIVIS000;
026300121001           ENDIF;
026400121001
026500121001         ENDDO;
026600121001
026700121001         exec sql close VISF;
026800121001         wEoF = *off;
026900121001
027000121001       ENDSR;
027100120925
027200120925       //--------------------------------------------------------------
027300120925       //?Operazioni finali.
027400120925       //--------------------------------------------------------------
027500120925       BEGSR RoutEnd;
027600120925
027700120925         *inLR = *on;
027800120925         return;
027900120925
028000120925       ENDSR;
028100120925
028200120925      /end-free
