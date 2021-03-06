000100080206      //--------------------------------------------------------------
000200120530      //?TRUL02R - Crea dtaara DDATIUTE e AZUTE
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000080206
001100080206      //---------------------------------------------------------------
001200090406      //?Definizione costanti.
001300080206      //---------------------------------------------------------------
001400080206
001500080206      //---------------------------------------------------------------
001600080206      //?Definizione schiere.
001700080206      //---------------------------------------------------------------
001800080206
001900080206      //---------------------------------------------------------------
002000080206      //?Definizione aree dati.
002100080206      //---------------------------------------------------------------
002200080206
002300080206      // - Dati utente
002400080206     d �AzUte        e ds                  extname(AZUTE00F)
002500080206     d                                     dtaara
002600080206     d �DatiUte      e ds                  extname(dDatiUte)
002700080206     d                                     dtaara
002800080206
002900080206      //---------------------------------------------------------------
003000080206      //?Definizione strutture dati.
003100080206      //---------------------------------------------------------------
003200080206
003300080206      // - Parametri ricevuti
003400080206     d KPJBA         e ds
003500120530
003600100326      // - Reperimento dati utente
003700100326     d TIBS34ds      e ds
003800090717
003900080206      //---------------------------------------------------------------
004000080206      //?Definizione variabili globali.
004100080206      //---------------------------------------------------------------
004200080208
004300090508      //---------------------------------------------------------------
004400090508      //?Definizione procedure usate.
004500090508      //---------------------------------------------------------------
004600080206
004700080626      //---------------------------------------------------------------
004800120530      //?Prototipi.
004900080626      //---------------------------------------------------------------
005000100326
005100080626      /copy gaitrasrc/srcprotopr,tibs34r
005200100326
005300080206      //---------------------------------------------------------------
005400080206      //?Definizione key-list.
005500080206      //---------------------------------------------------------------
005600080206
005700080206      //---------------------------------------------------------------
005800080206      //?Riepilogo indicatori.
005900080206      //---------------------------------------------------------------
006000120530
006100080206      //---------------------------------------------------------------
006200080206
006300080206      //---------------------------------------------------------------
006400080206      //?M A I N - L I N E
006500080206      //---------------------------------------------------------------
006600080206
006700080206     c     *Entry        plist
006800080206     c                   parm                    KPJBA
006900080206
007000080206      /free
007100080206
007200100326       //?Operazioni iniziali
007300080206       exsr RoutInz;
007400080206
007500100326       //?Operazioni finali
007600080206       exsr RoutEnd;
007700080206
007800080206       //--------------------------------------------------------------
007900080206       //?Operazioni iniziali.
008000080206       //--------------------------------------------------------------
008100080206       BEGSR RoutInz;
008200080206
008300100326       //?Reperimento dati job
008400080206         exsr DatiJob;
008500090714
008600080206       ENDSR;
008700080206
008800080206       //--------------------------------------------------------------
008900080206       //?Reperimento Dati del job (Utente/Operativi).
009000080206       //--------------------------------------------------------------
009100080206       BEGSR DatiJob;
009200080206
009300080206         in(E) �AzUte;
009400080206         if NOT %error;
009500080206           in(E) �DatiUte;
009600080206         endif;
009700080206         if %error or RSut = *blanks;
009800080206           clear TIBS34ds;
009900080206           tibs34r(tibs34ds);
010000080206           in �AzUte;
010100080206           in �DatiUte;
010200080206         endif;
010300080206
010400080206       ENDSR;
010500080206
010600080206       //--------------------------------------------------------------
010700080206       //?Operazioni finali.
010800080206       //--------------------------------------------------------------
010900080206       BEGSR RoutEnd;
011000080206
011100080206         *inLR = *on;
011200080206         return;
011300080206
011400080206       ENDSR;
011500080206
011600080206      /end-free
