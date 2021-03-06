000100120925      //--------------------------------------------------------------
000200121105      //?TRMKS05R - Pulizia SIC file storici.
000300120925      //--------------------------------------------------------------
000400120925
000500120925     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120925
000700120925      //---------------------------------------------------------------
000800120925      //?Dichiarazione file.
000900120925      //---------------------------------------------------------------
001000121008
001100121105       // -?SIC storico attivit�
001200121107     fTIATCE0F  uf   e             disk
001300121105
001400121105       // -?SIC Info commerciali apertura trattativa
001500121107     fTICPIQ0F  uf   e             disk
001600121105
001700121105       // -?SIC Info trattative
001800121107     fTIVIIC0F  uf   e             disk    prefix(w)
001900120928
002000121105       // -?SIC trattative eseguite
002100121107     fTIVISC0F  uf   e             disk
002200121105
002300121105       // -?SIC offerte
002400121107     fTIVOFC0F  uf   e             disk
002500120925
002600120925      //---------------------------------------------------------------
002700120925      //?Definizione costanti.
002800120925      //---------------------------------------------------------------
002900120925
003000120925      //---------------------------------------------------------------
003100120925      //?Definizione schiere.
003200120925      //---------------------------------------------------------------
003300120925
003400120925      //---------------------------------------------------------------
003500120925      //?Definizione aree dati.
003600120925      //---------------------------------------------------------------
003700120925
003800120925      //---------------------------------------------------------------
003900120925      //?Definizione strutture dati.
004000120925      //---------------------------------------------------------------
004100121010
004200121010       // -?Kpjba
004300121010     d KPJBA         e ds
004400121002
004500121105       // -?File TIATCE0F
004600121105     d TIATCeds      e ds                  extname(TIATCE0F)
004700121105
004800121105       // -?File TICPIQ0F
004900121105     d TICPIQds      e ds                  extname(TICPIQ0F)
005000121105
005100121105       // -?File TIVIIC0F
005200121105     d TIVIICds      e ds                  extname(TIVIIC0F)
005300121105     d                                     prefix(w)
005400121002
005500121105       // -?File TIVISC0F
005600121105     d TIVISCds      e ds                  extname(TIVISC0F)
005700121105
005800121105       // -?File TIVOFC0F
005900121105     d TIVOFCds      e ds                  extname(TIVOFC0F)
006000120925
006100120925      //---------------------------------------------------------------
006200120925      //?Definizione variabili globali.
006300120925      //---------------------------------------------------------------
006400120925
006500120925       // -?Flags booleani
006600120925     d wEoF            s               n   inz(*off)
006700120928
006800120928       // -?Campi Rnn
006900121105     d ATCE_rrn        s              9  0
007000121105     d CPIQ_rrn        s              9  0
007100121105     d VIIC_rrn        s              9  0
007200121105     d VISC_rrn        s              9  0
007300121105     d VOFC_rrn        s              9  0
007400121008
007500120925      //---------------------------------------------------------------
007600120925      //?Definizione procedure esterne.
007700120925      //---------------------------------------------------------------
007800120925
007900120925      //---------------------------------------------------------------
008000120925      //?Definizione prototipi.
008100120925      //---------------------------------------------------------------
008200120925
008300120925      //---------------------------------------------------------------
008400120925      //?Definizione key-list.
008500120925      //---------------------------------------------------------------
008600120925
008700120925      //---------------------------------------------------------------
008800120925      //?Riepilogo indicatori.
008900120925      //---------------------------------------------------------------
009000120925
009100120925      //---------------------------------------------------------------
009200120925
009300120925      //---------------------------------------------------------------
009400120925      //?M A I N - L I N E
009500120925      //---------------------------------------------------------------
009600121011
009700121011     c     *entry        plist
009800121011     c                   parm                    KPJBA
009900120925
010000120925      /free
010100120925
010200120925       //?Operazioni iniziali
010300120925       exsr RoutInz;
010400120925
010500121105       //?Pulizia TIATCE
010600121105       exsr Pul_TIATCE;
010700121105
010800121105       //?Pulizia TIVISC
010900121105       exsr Pul_TIVISC;
011000121105
011100121105       //?Pulizia TIVOFC
011200121105       exsr Pul_TIVOFC;
011300121105
011400121105       //?Pulizia TIVIIC
011500121105       exsr Pul_TIVIIC;
011600121105
011700121105       //?Pulizia TICPIQ
011800121105       exsr Pul_TICPIQ;
011900120925
012000120925       //?Operazioni finali
012100120925       exsr RoutEnd;
012200120925
012300120925       //--------------------------------------------------------------
012400120925       //?Operazioni iniziali.
012500120925       //--------------------------------------------------------------
012600120925       BEGSR RoutInz;
012700120925
012800120925         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012900120925
013000120925       ENDSR;
013100120928
013200120928       //--------------------------------------------------------------
013300121105       //?Pulizia TIATCE0F.
013400120928       //--------------------------------------------------------------
013500121105       BEGSR Pul_TIATCE;
013600121001
013700121001         wEoF = *off;
013800121001
013900121105       //?Leggo TIATCE0F senza TIATC00F
014000121107         exec sql
014100121107          DECLARE ATCE cursor for
014200121107          SELECT rrn(TIATCE0F) from TIATCE0F
014300121107          exception join TIATC00F on
014400121107          ATEatn = ATCatn and ATEatnp = ATCatnp and ATEtat = ATCtat;
014500121001
014600121001         exec sql
014700121105           open ATCE;
014800121001
014900121001         IF sqlcode < 0;
015000121001           wEoF = *on;
015100121001           leavesr;
015200121001         ENDIF;
015300121001
015400121001         DOW  not wEoF;
015500121001
015600121105           exec sql fetch next from ATCE into :ATCE_rrn;
015700121001           IF  sqlcod = 100 or sqlcod < 0;
015800121001             wEoF = *on;
015900121001             leave;
016000121001           ENDIF;
016100121105
016200121105         //?Deleto lo storico attivit� del SIC
016300121105           chain ATCE_rrn TIATCE0F;
016400121105           IF  %found(TIATCE0F);
016500121105             DELETE  TIATCE00;
016600121105           ENDIF;
016700121001
016800121001         ENDDO;
016900121001
017000121105         exec sql close ATCE;
017100121001         wEoF = *off;
017200121001
017300121001       ENDSR;
017400121105
017500121105       //--------------------------------------------------------------
017600121105       //?Pulizia TIVISC0F.
017700121105       //--------------------------------------------------------------
017800121105       BEGSR Pul_TIVISC;
017900121105
018000121105         wEoF = *off;
018100121105
018200121105       //?Leggo TIVISC0F senza TIVIS00F
018300121105         exec sql
018400121105          DECLARE VISC cursor for
018500121105          SELECT rrn(TIVISC0F) from TIVISC0F
018600121105          exception join TIVIS00F on
018700121105          VICnrv = VISnrv;
018800121105
018900121105         exec sql
019000121105           open VISC;
019100121105
019200121105         IF sqlcode < 0;
019300121105           wEoF = *on;
019400121105           leavesr;
019500121105         ENDIF;
019600121105
019700121105         DOW  not wEoF;
019800121105
019900121105           exec sql fetch next from VISC into :VISC_rrn;
020000121105           IF  sqlcod = 100 or sqlcod < 0;
020100121105             wEoF = *on;
020200121105             leave;
020300121105           ENDIF;
020400121105
020500121105         //?Deleto lo storico trattative del SIC
020600121105           chain VISC_rrn TIVISC0F;
020700121105           IF  %found(TIVISC0F);
020800121105             DELETE  TIVISC00;
020900121105           ENDIF;
021000121105
021100121105         ENDDO;
021200121105
021300121105         exec sql close VISC;
021400121105         wEoF = *off;
021500121105
021600121105       ENDSR;
021700121105
021800121105       //--------------------------------------------------------------
021900121105       //?Pulizia TIVOFC0F.
022000121105       //--------------------------------------------------------------
022100121105       BEGSR Pul_TIVOFC;
022200121105
022300121105         wEoF = *off;
022400121105
022500121105       //?Leggo TIVOFC0F senza TIVOF00F
022600121105         exec sql
022700121105          DECLARE VOFC cursor for
022800121105          SELECT rrn(TIVOFC0F) from TIVOFC0F
022900121105          exception join TIVOF00F on
023000121105          VOCnrv = VOFnrv;
023100121105
023200121105         exec sql
023300121105           open VOFC;
023400121105
023500121105         IF sqlcode < 0;
023600121105           wEoF = *on;
023700121105           leavesr;
023800121105         ENDIF;
023900121105
024000121105         DOW  not wEoF;
024100121105
024200121105           exec sql fetch next from VOFC into :VOFC_rrn;
024300121105           IF  sqlcod = 100 or sqlcod < 0;
024400121105             wEoF = *on;
024500121105             leave;
024600121105           ENDIF;
024700121105
024800121105         //?Deleto lo storico offerte del SIC
024900121105           chain VOFC_rrn TIVOFC0F;
025000121105           IF  %found(TIVOFC0F);
025100121105             DELETE  TIVOFC00;
025200121105           ENDIF;
025300121105
025400121105         ENDDO;
025500121105
025600121105         exec sql close VOFC;
025700121105         wEoF = *off;
025800121105
025900121105       ENDSR;
026000121105
026100121105       //--------------------------------------------------------------
026200121105       //?Pulizia TIVIIC0F.
026300121105       //--------------------------------------------------------------
026400121105       BEGSR Pul_TIVIIC;
026500121105
026600121105         wEoF = *off;
026700121105
026800121105       //?Leggo TIVIIC0F senza TIVII00F
026900121105         exec sql
027000121105          DECLARE VIIC cursor for
027100121105          SELECT rrn(TIVIIC0F) from TIVIIC0F
027200121105          exception join TIVII00F on
027300121105          VICnrv = VIInrv;
027400121105
027500121105         exec sql
027600121105           open VIIC;
027700121105
027800121105         IF sqlcode < 0;
027900121105           wEoF = *on;
028000121105           leavesr;
028100121105         ENDIF;
028200121105
028300121105         DOW  not wEoF;
028400121105
028500121105           exec sql fetch next from VIIC into :VIIC_rrn;
028600121105           IF  sqlcod = 100 or sqlcod < 0;
028700121105             wEoF = *on;
028800121105             leave;
028900121105           ENDIF;
029000121105
029100121105         //?Deleto lo storico trattative del SIC
029200121105           chain VIIC_rrn TIVIIC0F;
029300121105           IF  %found(TIVIIC0F);
029400121105             DELETE  TIVIIC00;
029500121105           ENDIF;
029600121105
029700121105         ENDDO;
029800121105
029900121105         exec sql close VIIC;
030000121105         wEoF = *off;
030100121105
030200121105       ENDSR;
030300121105
030400121105       //--------------------------------------------------------------
030500121105       //?Pulizia TICPIQ0F.
030600121105       //--------------------------------------------------------------
030700121105       BEGSR Pul_TICPIQ;
030800121105
030900121105         wEoF = *off;
031000121105
031100121105       //?Leggo TICPIQ0F senza TIVIS00F
031200121105         exec sql
031300121105          DECLARE CPIQ cursor for
031400121105          SELECT rrn(TICPIQ0F) from TICPIQ0F
031500121105          exception join TIVIS00F on
031600121105          CPQnrv = VISnrv;
031700121105
031800121105         exec sql
031900121105           open CPIQ;
032000121105
032100121105         IF sqlcode < 0;
032200121105           wEoF = *on;
032300121105           leavesr;
032400121105         ENDIF;
032500121105
032600121105         DOW  not wEoF;
032700121105
032800121105           exec sql fetch next from CPIQ into :CPIQ_rrn;
032900121105           IF  sqlcod = 100 or sqlcod < 0;
033000121105             wEoF = *on;
033100121105             leave;
033200121105           ENDIF;
033300121105
033400121105         //?Deleto lo storico info comm. apertura trattative del SIC
033500121105           chain CPIQ_rrn TICPIQ0F;
033600121105           IF  %found(TICPIQ0F);
033700121105             DELETE  TICPIQ00;
033800121105           ENDIF;
033900121105
034000121105         ENDDO;
034100121105
034200121105         exec sql close CPIQ;
034300121105         wEoF = *off;
034400121105
034500121105       ENDSR;
034600120925
034700120925       //--------------------------------------------------------------
034800120925       //?Operazioni finali.
034900120925       //--------------------------------------------------------------
035000120925       BEGSR RoutEnd;
035100120925
035200120925         *inLR = *on;
035300120925         return;
035400120925
035500120925       ENDSR;
035600120925
035700120925      /end-free
