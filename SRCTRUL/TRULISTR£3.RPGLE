000100101013      //---------------------------------------------------------------
000200101013      //?TRULISTR - Calcola la % ISTAT da aggiungere all'imponibile
000300101013      //---------------------------------------------------------------
000400101013     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500101013
000600101013      //---------------------------------------------------------------
000700101013      //?Dichiarazione file.
000800101013      //---------------------------------------------------------------
000900101013     ftisis01l  if   e           k disk
001000101013
001100101013      //---------------------------------------------------------------
001200101013      //?Definizione costanti.
001300101013      //---------------------------------------------------------------
001400101013
001500101013      //---------------------------------------------------------------
001600101013      //?Definizione schiere.
001700101013      //---------------------------------------------------------------
001800101013
001900110405     d $Data           S              8  0 DIM(998)
002000110405     d $Punti          S              5  1 DIM(998)
002100101013
002200101013      //---------------------------------------------------------------
002300101013      //?Definizione aree dati.
002400101013      //---------------------------------------------------------------
002500101013
002600101013      // - Dati utente
002700101013     d �AzUte        e ds                  extname(AZUTE00F)
002800101013     d                                     dtaara
002900101013     d �DatiUte      e ds                  extname(dDatiUte)
003000101013     d                                     dtaara
003100101013
003200101013      //---------------------------------------------------------------
003300101013      //?Definizione strutture dati.
003400101013      //---------------------------------------------------------------
003500101013
003600101013      // - Parametri ricevuti
003700101013     d TRULISTDS     e ds
003800101013
003900101013      // - Reperimento dati utente
004000101013     d TIBS34ds      e ds
004100101013
004200101013      //---------------------------------------------------------------
004300101013      //?Definizione variabili globali.
004400101013      //---------------------------------------------------------------
004500101013
004600101013      // - Flags booleani
004700101013     d $EoF            s               n   inz(*off)
004800101013     d $Fine           s               n   inz(*off)
004900101013
005000101013      // - Campi di comodo
005100101013     d $Scatto         s                   like(SISsca)
005200101013     d wDate_EUR       s               d   datfmt(*eur)
005300101013     d wDate_ISO       s               d   datfmt(*iso)
005400101013
005500101013      //---------------------------------------------------------------
005600101013      //?Definizione procedure esterne.
005700101013      //---------------------------------------------------------------
005800101013
005900101013      // - Reperimento dati utente
006000101013     d tibs34r         pr                  extpgm('TIBS34R')
006100101013     d  tibs34ds                           likeds(TIBS34ds)
006200101013
006300101013      //---------------------------------------------------------------
006400101013      //?Definizione prototipi.
006500101013      //---------------------------------------------------------------
006600101013
006700101013      //---------------------------------------------------------------
006800101013      //?Definizione key-list.
006900101013      //---------------------------------------------------------------
007000101013
007100101013      //---------------------------------------------------------------
007200101013      //?Riepilogo indicatori.
007300101013      //---------------------------------------------------------------
007400101013
007500101013      //---------------------------------------------------------------
007600101013      //?M A I N - L I N E
007700101013      //---------------------------------------------------------------
007800101013
007900101013     c     *Entry        plist
008000101013     c                   parm                    TRULISTDS
008100101013
008200101013      /free
008300101013
008400101013       //?Operazioni iniziali
008500101013       exsr RoutInz;
008600101013
008700101013       //?Elaboro se tutti i dati sono stati passati
008800101013       IF  not $Fine;
008900101013         //?Carico gli scatti ISTAT presenti nel file
009000101013         exsr Carica;
009100101013         //?Calcolo la % ISTAT
009200101013         exsr Elabora;
009300101013       ENDIF;
009400101013
009500101013       //?Operazioni finali
009600101013       exsr RoutEnd;
009700101013
009800101013       //--------------------------------------------------------------
009900101013       //?Operazioni iniziali.
010000101013       //--------------------------------------------------------------
010100101013       BEGSR RoutInz;
010200101013
010300101013         //?Reperimento dati job
010400101013         exsr DatiJob;
010500101013
010600101013         //?Controllo dati ricevuti
010700101013         //?Se non c'� lo scatto torno errore e non elaboro
010800101013         IF  IISTsca = 0;
010900101013           $Fine = *on;
011000101013           OISTerr = '1';
011100101013         ENDIF;
011200101013
011300101013         //?Se non c'� la data torno errore e non elaboro
011400101013         IF  IISTdsp = 0;
011500101013           $Fine = *on;
011600101013           OISTerr = '1';
011700101013         ENDIF;
011800101013
011900101013       ENDSR;
012000101013
012100101013       //--------------------------------------------------------------
012200101013       //?Reperimento Dati del job (Utente/Operativi).
012300101013       //--------------------------------------------------------------
012400101013       BEGSR DatiJob;
012500101013
012600101013         in(E) �AzUte;
012700101013         IF  NOT %error;
012800101013           in(E) �DatiUte;
012900101013         ENDIF;
013000101013         IF  %error or RSut = *blanks;
013100101013           clear TIBS34ds;
013200101013           tibs34r(tibs34ds);
013300101013           in �AzUte;
013400101013           in �DatiUte;
013500101013         ENDIF;
013600101013
013700101013       ENDSR;
013800101013
013900101013       //--------------------------------------------------------------
014000101013       //?Carico TUTTI gli scatti ISTAT presenti nel file.
014100101013       //--------------------------------------------------------------
014200101013       BEGSR Carica;
014300101013
014400101013         clear $Data;
014500101013         clear $Punti;
014600101013
014700101013         SISsca = 0;
014800101013         setll (SISsca) TISIS01L;
014900101013
015000101013         DOW  not $EoF;
015100101013
015200101013           read TISIS01L;
015300101013
015400101013           IF  %Eof(TISIS01L);
015500101013             $EoF = *on;
015600101013             leave;
015700101013           ENDIF;
015800101013
015900101013           $Data(SISsca) = SISdata;
016000101013           $Punti(SISsca) = SISpunti;
016100101013
016200101013         ENDDO;
016300101013
016400101013       ENDSR;
016500101013
016600101013       //--------------------------------------------------------------
016700101013       //?Elaboro.
016800101013       //--------------------------------------------------------------
016900101013       BEGSR Elabora;
017000101018
017100101018       // CALCOLO LA PERCENTUALE DI BASE DI CALCOLO
017200101018       // SE ESISTE GI� UNA PERCENTUALE CALCOLO LA PERENTUALE DELLA PERCENTYUALE E POI SOMMO TUTTO
017300101018       // ESEMPIO-....
017400101018       // 1� PUNTO PERCENTUALE ISTAT  2%
017500101018       // 2� PUNTO PERCENTUALE ISTAT  5%
017600101018       // PERCENTUALE ISTAT = 2 + (IL 5% DI 2) + 5 = 7,1
017700101013
017800101013         clear OISTiac;
017900101013
018000101013         FOR IISTsca by 1 to %elem($Data);
018100101013
018200101013           IF  IISTdsp >= $Data(IISTsca);
018300101013             IF  OISTiac > 0;
018400101013               OISTiac = OISTiac + ((OISTiac * $Punti(IISTsca)) / 100);
018500101013             ENDIF;
018600101013             OISTiac += $Punti(IISTsca);
018700101013           ELSE;
018800101013             leave;
018900101013           ENDIF;
019000101013
019100101013         ENDFOR;
019200101013
019300101013       ENDSR;
019400101013
019500101013       //--------------------------------------------------------------
019600101013       //?Operazioni finali.
019700101013       //--------------------------------------------------------------
019800101013       BEGSR RoutEnd;
019900101013
020000101013         *inLR = *on;
020100101013         return;
020200101013
020300101013       ENDSR;
020400101013
020500101013      /end-free
