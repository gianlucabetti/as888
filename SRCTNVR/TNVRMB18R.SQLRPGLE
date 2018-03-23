000100121218      //---------------------------------------------------------------
000200121218      //
000300140613      //?        Controllo cappario GB tra BRT e DPD
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100140613     fAZCPC01L  if   e           k disk
001200140613     fDPCRO01L  if   e           k disk
001300140616     fCAPDPD01  if a e           k disk
001400140613     fTNVRMB18P o    e             printer oflind(*in90)
001500040119
001600040119      *---------------------------------------------------------------*
001700121218
001800121218       // - Campi di comodo
001900140714     d k_verbrt        s                   like(CPCver)  inz(44)
002000140714     d k_verdpd        s                   like(CROver)  inz(52)
002100140613     d k_nazbrt        s                   like(CPCnar)  inz('GB ')
002200140613     d k_nazdpd        s                   like(CROiso2) inz('GB')
002300140613
002400140613     d lenprebrt       s              2s 0 inz
002500140613     d lenpredpd       s              2s 0 inz
002600140613     d prebrt          s              4a   inz
002700140613     d predpd          s              4a   inz
002800140613     d sav_predpd      s              4a   inz
002900140613
003000140613      // - Campi di comodo data
003100140613     d wData_EUR       s               d   datfmt(*eur)
003200140613     d wData_ISO       s               d   datfmt(*iso)
003300140613     d wOggi           s              8  0 inz
003400121218
003500121218       // - flag booleani
003600140613     d almeno1         s               n
003700121218     d wEoF            s               n
003800140613     d wEoFdpd         s               n
003900140613
004000140613       //--------------------------------------------------------------
004100140613       //?Definizione strutture dati.                                  ?
004200140613       //--------------------------------------------------------------
004300140613     d KPJBA         e ds
004400140616
004500140616     d capdpdds      e ds                  extname(capdpd)
004600040119
004700140613       //--------------------------------------------------------------
004800140613       //?M A I N - L I N E
004900140613       //---------------------------------------------------------------
005000140613
005100140613     c     *entry        plist
005200140613     c                   parm                    KPJBA
005300040119
005400121218      /free
005500140613
005600140613       // Imposto nome programma
005700140613         NomePgm = 'TNVRMB18R';
005800140613
005900140613       // Imposto data e ora sistema
006000140613         wOggi     = %dec(%date());
006100140613         wData_ISO = %date(wOggi:*ISO);
006200140613         wData_EUR = wData_ISO;
006300140613         DataEla   = %dec(wData_EUR);
006400140613         OraEla    = %dec(%time());
006500140613
006600140613       // Stampo subito la testata
006700140613         write MB18T1;
006800140613         write MB18T2;
006900140616
007000140616       // Scrivo file di work con cap DPD solo prefissi
007100140616         exsr newcapdpd;
007200140616         clear predpd;
007300140616
007400140616       // Confronto i cap BRT con DPD
007500140616         exsr leggicapbrt;
007600140616
007700140616       // Confronto i cap DPD con BRT
007800140616         exsr leggicapdpd;
007900140613
008000140613       // Fine stampa
008100140613         write MB18E1;
008200121218
008300140613         *inLR = *ON;
008400140616
008500140616       //--------------------------------------------------------------
008600140616       // Creo new cap DPD solo prefisso.                              ?
008700140616       //--------------------------------------------------------------
008800140616       BEGSR  newcapDPD;
008900140616
009000140616         wEoF = *off;
009100140616         clear sav_predpd;
009200140616
009300140616       // Leggo cappario DPD e scrivo file work
009400140616         setll (k_verdpd:k_nazdpd) DPCRO01L;
009500140616         reade (k_verdpd:k_nazdpd) DPCRO01L;
009600140616         DOW  not wEoF;
009700140616           IF  %eof(DPCRO01L);
009800140616             sav_predpd = predpd;
009900140616             leave;
010000140616           ENDIF;
010100140616
010200140616           reset lenpredpd;
010300140616           reset predpd;
010400140616
010500140616         // calcolo quanto è pieno il campo
010600140616           lenpredpd = %checkr(' ' : CROptcb);
010700140616         // calcolo il prefisso DPD
010800140616           SELECT;
010900140616         // prefisso CAP lungo 4
011000140616           WHEN  lenpredpd = 7;
011100140616             predpd = %subst(CROptcb:1:4);
011200140616             lenpredpd = 4;
011300140616         // prefisso CAP lungo 3
011400140616           WHEN  lenpredpd = 6;
011500140616             predpd = %subst(CROptcb:1:3);
011600140616             lenpredpd = 3;
011700140616         // prefisso CAP lungo 2
011800140616           WHEN  lenpredpd = 5;
011900140616             predpd = %subst(CROptcb:1:2);
012000140616             lenpredpd = 2;
012100140616           ENDSL;
012200140616
012300140616           IF  sav_predpd <> *blanks and
012400140616               sav_predpd <> predpd;
012500140616             chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
012600140616             IF  not %found(CAPDPD01);
012700140616               clear capdpd00;
012800140616               cap = sav_predpd;
012900140616               ver = k_verdpd;
013000140616               naz = k_nazdpd;
013100140616               write CAPDPD00;
013200140616             ENDIF;
013300140616             sav_predpd = predpd;
013400140616           ENDIF;
013500140616
013600140616           IF  sav_predpd = *blanks;
013700140616             sav_predpd = predpd;
013800140616           ENDIF;
013900140616
014000140616           reade (k_verdpd:k_nazdpd) DPCRO01L;
014100140616         ENDDO;
014200140616
014300140616         chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
014400140616         IF  not %found(CAPDPD01);
014500140616           clear capdpd00;
014600140616           cap = sav_predpd;
014700140616           ver = k_verdpd;
014800140616           naz = k_nazdpd;
014900140616           write CAPDPD00;
015000140616         ENDIF;
015100140616
015200140616       ENDSR;
015300140616
015400140616       //--------------------------------------------------------------
015500140616       // Controllo cappario BRT con cappario DPD.                     ?
015600140616       //--------------------------------------------------------------
015700140616       BEGSR  leggicapBRT;
015800140616
015900140616         wEoF = *off;
016000140616
016100140616       // Leggo cappario BRT ultima versione e solo Gran Bretagna
016200140616         setll (k_verbrt:k_nazbrt) AZCPC01L;
016300140616         reade (k_verbrt:k_nazbrt) AZCPC01L;
016400140616         DOW  not wEoF;
016500140616           IF  %eof(AZCPC01L);
016600140616             leave;
016700140616           ENDIF;
016800140616
016900140616           reset lenprebrt;
017000140616           reset prebrt;
017100140616
017200140616         // calcolo quanto è pieno il campo
017300140616           lenprebrt = %checkr(' ' : CPCcap);
017400140616         // calcolo il prefisso BRT
017500140616           SELECT;
017600140616         // prefisso CAP lungo 4
017700140616           WHEN  lenprebrt = 4;
017800140616             prebrt = %subst(CPCcap:1:4);
017900140616             lenprebrt = 4;
018000140616         // prefisso CAP lungo 3
018100140616           WHEN  lenprebrt = 3;
018200140616             prebrt = %subst(CPCcap:1:3);
018300140616             lenprebrt = 3;
018400140616         // prefisso CAP lungo 2
018500140616           WHEN  lenprebrt = 2;
018600140616             prebrt = %subst(CPCcap:1:2);
018700140616             lenprebrt = 2;
018800140616           ENDSL;
018900140616
019000140616         // controllo con work cappario DPD
019100140616           exsr controllaDPD;
019200140616
019300140616           reade (k_verbrt:k_nazbrt) AZCPC01L;
019400140616         ENDDO;
019500140616
019600140616       ENDSR;
019700140613
019800140613       //--------------------------------------------------------------
019900140616       // Controllo se esiste su cappario DPD.                                      ?
020000140613       //--------------------------------------------------------------
020100140616       BEGSR  controllaDPD;
020200140613
020300140616         exec sql
020400140616          select CAP into :cap from capdpd
020500140616          where ver = :k_verdpd and naz = :k_nazdpd and
020600140616          cap = :prebrt;
020700140616
020800140616       // non trovo il rcd corrispondente stampo
020900140616         IF  sqlcod <> 0;
021000140616          exsr stampaB;
021100140616         ENDIF;
021200140616
021300140616       ENDSR;
021400140613
021500140616       //--------------------------------------------------------------
021600140616       // Controllo cappario DPD con cappario BRT.                    ?
021700140616       //--------------------------------------------------------------
021800140616       BEGSR  leggicapDPD;
021900140616
022000140616         wEoF = *off;
022100140616         *in90 = *on;
022200140616
022300140616         exec sql
022400140616          declare DPD cursor for
022500140616          select * from capdpd
022600140616          where ver = :k_verdpd and naz = :k_nazdpd
022700140616          order by cap;
022800140616
022900140616         exec sql open DPD;
023000140616
023100140616         DOW  not wEoF;
023200140616
023300140616           exec sql fetch next from DPD into :capdpdds;
023400140616
023500140616         // fine file o errore sql esco
023600140616           IF  sqlcod = 100 or sqlcod < 0;
023700140616             wEoF = *on;
023800140616             leave;
023900140616           ENDIF;
024000140616
024100140616         // controllo con cappario BRT
024200140616           exsr controllaBRT;
024300140613
024400140613         ENDDO;
024500140616
024600140616         exec sql close DPD;
024700140613
024800140613       ENDSR;
024900140616
025000140616       //--------------------------------------------------------------
025100140616       // Controllo se esiste su cappario BRT.                                      ?
025200140616       //--------------------------------------------------------------
025300140616       BEGSR  controllaBRT;
025400140616
025500140616         chain (k_verbrt:k_nazbrt:cap) AZCPC01L;
025600140616       // non trovo il rcd corrispondente stampo
025700140616         IF  not %found(AZCPC01L);
025800140616          predpd = cap;
025900140616          exsr stampaD;
026000140616         ENDIF;
026100140616
026200140616       ENDSR;
026300140613
026400140613       //--------------------------------------------------------------
026500140616       // Stampa BRT.
026600140613       //--------------------------------------------------------------
026700140616       BEGSR  stampaB;
026800140613
026900140613       // Testata
027000140613         IF  *in90;
027100140613           write MB18T1;
027200140613           write MB18T2;
027300140616           *in90 = *off;
027400140613         ENDIF;
027500140613
027600140613       // Dettaglio
027700140616         write MB18D1;
027800140613
027900140613       ENDSR;
028000140616
028100140616       //--------------------------------------------------------------
028200140616       // Stampa DPD.
028300140616       //--------------------------------------------------------------
028400140616       BEGSR  stampaD;
028500140616
028600140616       // Testata
028700140616         IF  *in90;
028800140616           write MB18T1;
028900140616           write MB18T3;
029000140616           *in90 = *off;
029100140616         ENDIF;
029200140616
029300140616       // Dettaglio
029400140616         write MB18D2;
029500140616
029600140616       ENDSR;
029700050330
029800121218      /end-free
029900140331
