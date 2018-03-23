000100100413      //---------------------------------------------------------------
000200140922      //?TNVRMB23R - Prg. 823 Mod. Info Commerciali  NO BRT
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000140922     fTICPI01L  if a e           k disk
001100100413
001200100413      //---------------------------------------------------------------
001300100413      //?Definizione costanti.
001400100413      //---------------------------------------------------------------
001500100413
001600100413      //---------------------------------------------------------------
001700100413      //?Definizione schiere.
001800100413      //---------------------------------------------------------------
001900090715
002000100413      //---------------------------------------------------------------
002100100413      //?Definizione aree dati.
002200100413      //---------------------------------------------------------------
002300100413
002400100413      //---------------------------------------------------------------
002500100413      //?Definizione strutture dati.
002600100413      //---------------------------------------------------------------
002700140724
002800140724      // Anagrafica potenziali
002900140922     d TNCPOds       e ds                  extname(TNCPO00F)
003000100415
003100100413      //---------------------------------------------------------------
003200100413      //?Definizione variabili globali.
003300100413      //---------------------------------------------------------------
003400100413
003500100413      // - Flags booleani
003600140714     d wEnd            s               n   inz(*off)
003700100413
003800100413      // - Campi di comodo
003900140922     d savAERval       s                   like(CPIval) inz
004000140922     d savATRval       s                   like(CPIval) inz
004100140922     d savCAEval       s                   like(CPIval) inz
004200140922     d savPRFval       s                   like(CPIval) inz
004300140922     d savPROval       s                   like(CPIval) inz
004400140922     d savSPTpft       s                   like(CPIpft) inz
004500100413
004600100413      //---------------------------------------------------------------
004700100413      //?Definizione procedure esterne.
004800100413      //---------------------------------------------------------------
004900100413
005000100413      //---------------------------------------------------------------
005100140714      //?Definizione prototipi.
005200100413      //---------------------------------------------------------------
005300100413
005400100413      //---------------------------------------------------------------
005500100413      //?Definizione key-list.
005600100413      //---------------------------------------------------------------
005700100413
005800100413      //---------------------------------------------------------------
005900100413      //?Riepilogo indicatori.
006000100413      //---------------------------------------------------------------
006100100413
006200100413      //---------------------------------------------------------------
006300100413      //?M A I N - L I N E
006400100413      //---------------------------------------------------------------
006500100413
006600100413      /free
006700110304
006800140922       //?Leggo i potenziali
006900140922         exec sql
007000140922         DECLARE CLIPOT cursor for
007100140922         SELECT *
007200140922         FROM TNCPO00F
007300140922         ORDER by CPOcpo;
007400140922
007500140922         //?Apertura del cursore
007600140922         exec sql open CLIPOT;
007700140922
007800140922         IF sqlcode < 0;
007900140922           wEnd = *on;
008000140922         ENDIF;
008100140922
008200140922         DOW not wEnd;
008300140922
008400140922           exec sql FETCH next from CLIPOT into :TNCPOds;
008500140922
008600140922           IF  sqlcod = 100 or sqlcod < 0;
008700140922             wEnd = *on;
008800140922             leave;
008900140922           ENDIF;
009000140922
009100140922           clear savSPTpft;
009200140922           clear savPRFval;
009300140922           clear savPROval;
009400140922           clear savAERval;
009500140922           clear savCAEval;
009600140922           clear savATRval;
009700140922
009800140922         //?Elaboro solo i potenziali con: il totale della spesa impostato
009900140922         //?totale della spesa
010000140922           chain (CPOcpo:'SPT') TICPI01L;
010100140922           IF  %found(TICPI01L) and CPIpft > 0;
010200140922             savSPTpft = CPIpft;
010300140922
010400140922         //?e no % affidato a BRT
010500140922             chain (CPOcpo:'10':'_BAR') TICPI01L;
010600140922             IF  not %found(TICPI01L);
010700140922               exsr Elabora_INFO;
010800140922             ENDIF;
010900140922           ENDIF;
011000140922
011100140922         ENDDO;
011200140922
011300140922         //?chiudo il cursore
011400140922         exec sql CLOSE CLIPOT;
011500100413
011600100413       //?Operazioni finali
011700110304       exsr RoutEnd;
011800100420
011900100413       //--------------------------------------------------------------
012000140922       //?Elabora INFO NON affidato a BRT.
012100100413       //--------------------------------------------------------------
012200140922       BEGSR Elabora_INFO;
012300140923
012400140923       //?Se anche solo 1 ripartizione del NON affidato a BRT è presente
012500140923       //?non devo fare niente
012600140923         chain (CPOcpo:'NRF') TICPI01L;
012700140923         IF  %found(TICPI01L);
012800140923           leavesr;
012900140923         ENDIF;
013000140923         chain (CPOcpo:'NRO') TICPI01L;
013100140923         IF  %found(TICPI01L);
013200140923           leavesr;
013300140923         ENDIF;
013400140923         chain (CPOcpo:'NER') TICPI01L;
013500140923         IF  %found(TICPI01L);
013600140923           leavesr;
013700140923         ENDIF;
013800140923         chain (CPOcpo:'NAE') TICPI01L;
013900140923         IF  %found(TICPI01L);
014000140923           leavesr;
014100140923         ENDIF;
014200140923         chain (CPOcpo:'NTR') TICPI01L;
014300140923         IF  %found(TICPI01L);
014400140923           leavesr;
014500140923         ENDIF;
014600110304
014700140922       //?Cerco le ripartizioni spesa
014800140922         chain (CPOcpo:'PRF') TICPI01L;
014900140922         IF  %found(TICPI01L);
015000140922           savPRFval = CPIval;
015100140922         ENDIF;
015200140922
015300140922         chain (CPOcpo:'PRO') TICPI01L;
015400140922         IF  %found(TICPI01L);
015500140922           savPROval = CPIval;
015600140922         ENDIF;
015700140922
015800140922         chain (CPOcpo:'AER') TICPI01L;
015900140922         IF  %found(TICPI01L);
016000140922           savAERval = CPIval;
016100140922         ENDIF;
016200140922
016300140922         chain (CPOcpo:'CAE') TICPI01L;
016400140922         IF  %found(TICPI01L);
016500140922           savCAEval = CPIval;
016600140922         ENDIF;
016700140922
016800140922         chain (CPOcpo:'ATR') TICPI01L;
016900140922         IF  %found(TICPI01L);
017000140922           savATRval = CPIval;
017100140922         ENDIF;
017200140922
017300140923       //?Se la ripartizione della spesa è 100%
017400140923         IF  (savPRFval + savPROval + savAERval +
017500140923              savCAEval + savATRval) = 100;
017600140922
017700140922         //?Scrivo le ripartizioni spesa del NON affidato a BRT
017800140922           exsr NoAffidatoBRT;
017900140922
018000140922         ENDIF;
018100140922
018200140922       ENDSR;
018300140922
018400140922       //--------------------------------------------------------------
018500140922       //?Creo le info di NON Affidato a BRT.
018600140922       //--------------------------------------------------------------
018700140922        BEGSR NoAffidatoBRT;
018800140922
018900140922          clear TICPI000;
019000140922          CPIcpo = CPOcpo;
019100140922          CPIvft = 'EUR';
019200140922          CPIdim = *date;
019300140922          CPIfil = CPOflt;
019400140922          CPIpru = 'BATCH';
019500140922
019600140922          IF  savPRFval > 0;
019700140922            CPItpf = 'NRF';
019800140922            CPIpft = (savSPTpft * savPRFval / 100);
019900140922            write TICPI000;
020000140922          ENDIF;
020100140922
020200140922          IF  savPROval > 0;
020300140922            CPItpf = 'NRO';
020400140922            CPIpft = (savSPTpft * savPROval / 100);
020500140922            write TICPI000;
020600140922          ENDIF;
020700140922
020800140922          IF  savAERval > 0;
020900140922            CPItpf = 'NER';
021000140922            CPIpft = (savSPTpft * savAERval / 100);
021100140922            write TICPI000;
021200140922          ENDIF;
021300140922
021400140922          IF  savCAEval > 0;
021500140922            CPItpf = 'NAE';
021600140922            CPIpft = (savSPTpft * savCAEval / 100);
021700140922            write TICPI000;
021800140922          ENDIF;
021900140922
022000140922          IF  savATRval > 0;
022100140922            CPItpf = 'NTR';
022200140922            CPIpft = (savSPTpft * savATRval / 100);
022300140922            write TICPI000;
022400140922          ENDIF;
022500140922
022600140922        ENDSR;
022700100426
022800100413       //--------------------------------------------------------------
022900100413       //?Operazioni finali.
023000100413       //--------------------------------------------------------------
023100110304       BEGSR RoutEnd;
023200100413
023300100413         *inLR = *on;
023400100413         return;
023500100413
023600100413       ENDSR;
023700100413
023800100413      /end-free
