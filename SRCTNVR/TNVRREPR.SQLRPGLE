000100100922      //--------------------------------------------------------------
000200100922      //--------------------------------------------------------------
000300100922
000400100922     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500100922     h dftactgrp(*no) actgrp(*caller)
000600100922
000700100922      //---------------------------------------------------------------
000800100922      //?Dichiarazione file.
000900100922      //---------------------------------------------------------------
001000110314     fwfrep10f  if   e             disk
001100110314     ftabel00f  if   e           k disk
001200100922
001300100922      //---------------------------------------------------------------
001400100922      //?Definizione costanti.
001500100922      //---------------------------------------------------------------
001600100922      //---------------------------------------------------------------
001700100922      //?Definizione schiere.
001800100922      //---------------------------------------------------------------
001900100922      //?Definizione aree dati.
002000100922      //---------------------------------------------------------------
002100100922
002200100922      // - Dati utente
002300100922     d �AzUte        e ds                  extname(AZUTE00F)
002400100922     d                                     dtaara
002500100922     d �DatiUte      e ds                  extname(dDatiUte)
002600100922     d                                     dtaara
002700100922
002800110314     d wfrepds       e ds                  extname(wfrep10f)
002900100922
003000100922      //---------------------------------------------------------------
003100100922      //?Definizione strutture dati.
003200100922      //---------------------------------------------------------------
003300100922
003400100922      // - Parametri ricevuti
003500100922     d KPJBA         e ds
003600100922
003700100922      // - Reperimento dati utente
003800100922     d TIBS34DS      e ds
003900100922
004000110314     d dS01          e ds
004100110314     D  alfdip                39     43
004200100922
004300100922
004400100922      //---------------------------------------------------------------
004500100922      //?Definizione variabili globali.
004600100922      //---------------------------------------------------------------
004700100922
004800100922      // - Flags booleani
004900100922     d $End            s               n   inz(*off)
005000100922     d $Endtab         s               n   inz(*off)
005100100922     d $RcdOk          s               n   inz(*off)
005200100922
005300100922      // - Indici di schiera
005400100922     d xx              s              4  0 inz
005500100922     d yy              s              4  0 inz
005600100922
005700100922       // - Stringa SQL da eseguire
005800100922     d wSQL            s           2048    Varying        inz
005900110314     d key             s              8
006000110314     d totpcf          s              3  0
006100110314     d totpce          s              3  0
006200100922
006300100922      // - Campi di comodo data
006400110125     d Dataeur         s               d   datfmt(*eur)
006500110125     d Dataiso         s               d   datfmt(*iso)
006600110125     d Datasys         s               d   datfmt(*iso) inz(*sys)
006700100922
006800100922
006900100922
007000100922      //---------------------------------------------------------------
007100100922      //?Definizione procedure esterne.
007200100922      //---------------------------------------------------------------
007300100922
007400100922
007500100922      //---------------------------------------------------------------
007600100922      //?prototipi
007700100922      //---------------------------------------------------------------
007800100922
007900100922      /copy gaitrasrc/srcprotopr,tibs34r
008000100922
008100100922      //---------------------------------------------------------------
008200100922      //?Definizione key-list.
008300100922      //---------------------------------------------------------------
008400100922
008500100922      //---------------------------------------------------------------
008600100922      //?Riepilogo indicatori.
008700100922      //---------------------------------------------------------------
008800100922
008900100922
009000100922      //---------------------------------------------------------------
009100100922
009200100922      //---------------------------------------------------------------
009300100922      //?M A I N - L I N E
009400100922      //---------------------------------------------------------------
009500100922
009600100922     c     *Entry        plist
009700100922     c                   parm                    KPJBA
009800100922
009900100922      /free
010000100922
010100100922       //?Operazioni iniziali
010200100922       exsr RoutInz;
010300100922
010400100922       //?Elaboro le bolle tassate
010500100922       exsr sr_spedizioni ;
010600100922
010700100922
010800100922       //?Operazioni finali
010900100922       exsr RoutEnd;
011000100922
011100100922       //--------------------------------------------------------------
011200100922       //?Operazioni iniziali.
011300100922       //--------------------------------------------------------------
011400100922       BEGSR RoutInz;
011500100922
011600100922         //?Reperimento dati job
011700100922         exsr DatiJob;
011800100922
011900100922       ENDSR;
012000100922
012100100922       //--------------------------------------------------------------
012200100922       BEGSR sr_Spedizioni ;
012300100922
012400100922         $End = *off;
012500100922
012600100922         //?Imposto la stringa per SQL
012700100922         exsr  sr_PrepSQL;
012800100922
012900100922         //?Dichiarazione cursore
013000100922         exec sql
013100100922           prepare S1   from :wSQL;
013200100922         exec sql
013300100922           declare c1   cursor for S1;
013400100922
013500100922         //?Apertura del cursore
013600100922         exec sql
013700100922           open c1 ;
013800100922
013900100922         IF sqlcode < 0;
014000100922           $End = *on;
014100100922         ENDIF;
014200100922
014300100922         DOW not $End;
014400100922           exec sql
014500110314             fetch next from C1  into :wfrepds                 ;
014600100922           IF sqlcod = 100 or sqlcod < 0;
014700100922             $End = *on;
014800100922             leave;
014900100922           ENDIF;
015000100922
015100100922
015200110314           clear key  ;
015300110314           key =%editc(reprgf:'X')  ;
015400110314           chain (1:'01':key) tabel00f   ;
015500110314           if %found(tabel00f)  ;
015600110314           ds01=tbluni   ;
015700110314
015800110314         exec sql   select sum(dec(substr(tbluni, 33, 3), 3 , 0)),
015900110314         sum(dec(substr(tbluni, 36, 3), 3 , 0))  into :totpcf, :totpce
016000110314         from tabel00f where tblcod='01' and
016100110314         dec(substr(tbluni, 26, 7), 7 , 0)  =:reprgf ;
016200110314
016300110314         if alfdip>*zeros ;
016400110314
016500110314         exec sql   update wfrep10f set reipcf=:totpcf, reepcf=:totpce,
016600110314         repdip=:�01dip, repsoc=:�01soc  where reprgf=:reprgf and repann=2011
016700110314y        and repmes=02  and repfil=:repfil ;
016800110314         else  ;
016900110314         exec sql   update wfrep10f set reipcf=:totpcf, reepcf=:totpce
017000110314           where reprgf=:reprgf and repann=2011
017100110314y        and repmes=02  and repfil=:repfil ;
017200110314
017300110314           endif  ;
017400110314           endif  ;
017500110314
017600100922         ENDDO;
017700100922
017800100922         exec sql
017900100922           close c1 ;
018000100922
018100100922       ENDSR;
018200100922
018300100922       //--------------------------------------------------------------
018400100922       //?Preparazione stringa SQL.
018500100922       //--------------------------------------------------------------
018600100922       BEGSR sr_PrepSQL;
018700100922
018800110314         wSQL = 'select *   from WFREP10F +
018900110314                where  REPANN=2011 and REPmes=02'  ;
019000100922
019100100922       ENDSR;
019200100922
019300100922       //--------------------------------------------------------------
019400100922       //?Operazioni finali.
019500100922       //--------------------------------------------------------------
019600100922       BEGSR RoutEnd;
019700100922
019800100922         *inLR = *on;
019900100922         return;
020000100922
020100100922       ENDSR;
020200110314       //--------------------------------------------------------------
020300110314       BEGSR DatiJob;
020400110314
020500110314         in(E) �AzUte;
020600110314         if NOT %error;
020700110314           in(E) �DatiUte;
020800110314         endif;
020900110314         if %error or RSut = *blanks;
021000110314           clear TIBS34ds;
021100110314           tibs34r(tibs34ds);
021200110314           in �AzUte;
021300110314           in �DatiUte;
021400110314         endif;
021500110314
021600110314       ENDSR;
021700110314
021800100922
021900100922      /end-free
