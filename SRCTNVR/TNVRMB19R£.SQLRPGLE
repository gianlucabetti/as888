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
001900140613     d k_CROsrvc       s                   like(CROsrvc) inz
002000140613     d k_CROroup       s                   like(CROroup) inz('0000')
002100140613     d k_CROptcb       s                   like(CROptcb) inz
002200140613     d k_verbrt        s                   like(CPCver)  inz(43)
002300140613     d k_verdpd        s                   like(CROver)  inz(51)
002400140613     d k_nazbrt        s                   like(CPCnar)  inz('GB ')
002500140613     d k_nazdpd        s                   like(CROiso2) inz('GB')
002600140613
002700140613     d lenprebrt       s              2s 0 inz
002800140613     d lenpredpd       s              2s 0 inz
002900140613     d prebrt          s              4a   inz
003000140613     d predpd          s              4a   inz
003100140613     d sav_predpd      s              4a   inz
003200140616     d sav_lenpredpd   s              2s 0 inz
003300140613
003400140613      // - Campi di comodo data
003500140613     d wData_EUR       s               d   datfmt(*eur)
003600140613     d wData_ISO       s               d   datfmt(*iso)
003700140613     d wOggi           s              8  0 inz
003800121218
003900121218       // - flag booleani
004000140613     d almeno1         s               n
004100121218     d wEoF            s               n
004200140613     d wEoFdpd         s               n
004300140613
004400140613       //--------------------------------------------------------------
004500140613       //?Definizione strutture dati.                                  ?
004600140613       //--------------------------------------------------------------
004700140613     d KPJBA         e ds
004800140616
004900140616     d capdpdds      e ds                  extname(capdpd)
005000040119
005100140613       //--------------------------------------------------------------
005200140613       //?M A I N - L I N E
005300140613       //---------------------------------------------------------------
005400140613
005500140613     c     *entry        plist
005600140613     c                   parm                    KPJBA
005700040119
005800121218      /free
005900140613
006000140613       // Imposto nome programma
006100140613         NomePgm = 'TNVRMB18R';
006200140613
006300140613       // Imposto data e ora sistema
006400140613         wOggi     = %dec(%date());
006500140613         wData_ISO = %date(wOggi:*ISO);
006600140613         wData_EUR = wData_ISO;
006700140613         DataEla   = %dec(wData_EUR);
006800140613         OraEla    = %dec(%time());
006900140613
007000140613       // Stampo subito la testata
007100140613         write MB18T1;
007200140613         write MB18T2;
007300140616
007400140616       // Scrivo file di work con cap DPD solo prefissi
007500140616         exsr newcapdpd;
007600140616         clear predpd;
007700140616
007800140616       // Confronto i cap BRT con DPD
007900140616         exsr leggicapbrt;
008000140616
008100140616       // Confronto i cap DPD con BRT
008200140616         exsr leggicapdpd;
008300140613
008400140613       // Fine stampa
008500140613         write MB18E1;
008600121218
008700140613         *inLR = *ON;
008800140616
008900140616       //--------------------------------------------------------------
009000140616       // Creo new cap DPD solo prefisso.                              ?
009100140616       //--------------------------------------------------------------
009200140616       BEGSR  newcapDPD;
009300140616
009400140616         wEoF = *off;
009500140616         clear sav_predpd;
009600140616         clear sav_lenpredpd;
009700140616
009800140616       // Leggo cappario DPD e scrivo file work
009900140616         setll (k_verdpd:k_nazdpd) DPCRO01L;
010000140616         reade (k_verdpd:k_nazdpd) DPCRO01L;
010100140616         DOW  not wEoF;
010200140616           IF  %eof(DPCRO01L);
010300140616             sav_predpd = predpd;
010400140616             leave;
010500140616           ENDIF;
010600140616
010700140616           reset lenpredpd;
010800140616           reset predpd;
010900140616
011000140616         // calcolo quanto è pieno il campo
011100140616           lenpredpd = %checkr(' ' : CROptcb);
011200140616         // calcolo il prefisso DPD
011300140616           SELECT;
011400140616         // prefisso CAP lungo 4
011500140616           WHEN  lenpredpd = 7;
011600140616             predpd = %subst(CROptcb:1:4);
011700140616             lenpredpd = 4;
011800140616         // prefisso CAP lungo 3
011900140616           WHEN  lenpredpd = 6;
012000140616             predpd = %subst(CROptcb:1:3);
012100140616             lenpredpd = 3;
012200140616         // prefisso CAP lungo 2
012300140616           WHEN  lenpredpd = 5;
012400140616             predpd = %subst(CROptcb:1:2);
012500140616             lenpredpd = 2;
012600140616           ENDSL;
012700140616
012800140616           IF  sav_predpd <> *blanks and
012900140616               sav_predpd <> predpd;
013000140616             chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
013100140616             IF  not %found(CAPDPD01);
013200140616               clear capdpd00;
013300140616               cap = sav_predpd;
013400140616               ver = k_verdpd;
013500140616               naz = k_nazdpd;
013600140616               write CAPDPD00;
013700140616             ENDIF;
013800140616             sav_predpd = predpd;
013900140616           ENDIF;
014000140616
014100140616           IF  sav_predpd = *blanks;
014200140616             sav_predpd = predpd;
014300140616           ENDIF;
014400140616
014500140616           reade (k_verdpd:k_nazdpd) DPCRO01L;
014600140616         ENDDO;
014700140616
014800140616         chain (k_verdpd:k_nazdpd:sav_predpd) CAPDPD01;
014900140616         IF  not %found(CAPDPD01);
015000140616           clear capdpd00;
015100140616           cap = sav_predpd;
015200140616           ver = k_verdpd;
015300140616           naz = k_nazdpd;
015400140616           write CAPDPD00;
015500140616         ENDIF;
015600140616
015700140616       ENDSR;
015800140616
015900140616       //--------------------------------------------------------------
016000140616       // Controllo cappario BRT con cappario DPD.                     ?
016100140616       //--------------------------------------------------------------
016200140616       BEGSR  leggicapBRT;
016300140616
016400140616         wEoF = *off;
016500140616
016600140616       // Leggo cappario BRT ultima versione e solo Gran Bretagna
016700140616         setll (k_verbrt:k_nazbrt) AZCPC01L;
016800140616         reade (k_verbrt:k_nazbrt) AZCPC01L;
016900140616         DOW  not wEoF;
017000140616           IF  %eof(AZCPC01L);
017100140616             leave;
017200140616           ENDIF;
017300140616
017400140616           reset lenprebrt;
017500140616           reset prebrt;
017600140616
017700140616         // calcolo quanto è pieno il campo
017800140616           lenprebrt = %checkr(' ' : CPCcap);
017900140616         // calcolo il prefisso BRT
018000140616           SELECT;
018100140616         // prefisso CAP lungo 4
018200140616           WHEN  lenprebrt = 4;
018300140616             prebrt = %subst(CPCcap:1:4);
018400140616             lenprebrt = 4;
018500140616         // prefisso CAP lungo 3
018600140616           WHEN  lenprebrt = 3;
018700140616             prebrt = %subst(CPCcap:1:3);
018800140616             lenprebrt = 3;
018900140616         // prefisso CAP lungo 2
019000140616           WHEN  lenprebrt = 2;
019100140616             prebrt = %subst(CPCcap:1:2);
019200140616             lenprebrt = 2;
019300140616           ENDSL;
019400140616
019500140616         // controllo con work cappario DPD
019600140616           exsr controllaDPD;
019700140616
019800140616           reade (k_verbrt:k_nazbrt) AZCPC01L;
019900140616         ENDDO;
020000140616
020100140616       ENDSR;
020200140613
020300140613       //--------------------------------------------------------------
020400140616       // Controllo se esiste su cappario DPD.                                      ?
020500140613       //--------------------------------------------------------------
020600140616       BEGSR  controllaDPD;
020700140613
020800140616         exec sql
020900140616          select CAP into :cap from capdpd
021000140616          where ver = :k_verdpd and naz = :k_nazdpd and
021100140616          cap = :prebrt;
021200140616
021300140616       // non trovo il rcd corrispondente stampo
021400140616         IF  sqlcod <> 0;
021500140616          exsr stampaB;
021600140616         ENDIF;
021700140616
021800140616       ENDSR;
021900140613
022000140616       //--------------------------------------------------------------
022100140616       // Controllo cappario DPD con cappario BRT.                    ?
022200140616       //--------------------------------------------------------------
022300140616       BEGSR  leggicapDPD;
022400140616
022500140616         wEoF = *off;
022600140616         *in90 = *on;
022700140616
022800140616         exec sql
022900140616          declare DPD cursor for
023000140616          select * from capdpd
023100140616          where ver = :k_verdpd and naz = :k_nazdpd
023200140616          order by cap;
023300140616
023400140616         exec sql open DPD;
023500140616
023600140616         DOW  not wEoF;
023700140616
023800140616           exec sql fetch next from DPD into :capdpdds;
023900140616
024000140616         // fine file o errore sql esco
024100140616           IF  sqlcod = 100 or sqlcod < 0;
024200140616             wEoF = *on;
024300140616             leave;
024400140616           ENDIF;
024500140616
024600140616         // controllo con cappario BRT
024700140616           exsr controllaBRT;
024800140613
024900140613         ENDDO;
025000140616
025100140616         exec sql close DPD;
025200140613
025300140613       ENDSR;
025400140616
025500140616       //--------------------------------------------------------------
025600140616       // Controllo se esiste su cappario BRT.                                      ?
025700140616       //--------------------------------------------------------------
025800140616       BEGSR  controllaBRT;
025900140616
026000140616         chain (k_verbrt:k_nazbrt:cap) AZCPC01L;
026100140616       // non trovo il rcd corrispondente stampo
026200140616         IF  not %found(AZCPC01L);
026300140616          predpd = cap;
026400140616          exsr stampaD;
026500140616         ENDIF;
026600140616
026700140616       ENDSR;
026800140613
026900140613       //--------------------------------------------------------------
027000140616       // Stampa BRT.
027100140613       //--------------------------------------------------------------
027200140616       BEGSR  stampaB;
027300140613
027400140613       // Testata
027500140613         IF  *in90;
027600140613           write MB18T1;
027700140613           write MB18T2;
027800140616           *in90 = *off;
027900140613         ENDIF;
028000140613
028100140613       // Dettaglio
028200140616         write MB18D1;
028300140613
028400140613       ENDSR;
028500140616
028600140616       //--------------------------------------------------------------
028700140616       // Stampa DPD.
028800140616       //--------------------------------------------------------------
028900140616       BEGSR  stampaD;
029000140616
029100140616       // Testata
029200140616         IF  *in90;
029300140616           write MB18T1;
029400140616           write MB18T3;
029500140616           *in90 = *off;
029600140616         ENDIF;
029700140616
029800140616       // Dettaglio
029900140616         write MB18D2;
030000140616
030100140616       ENDSR;
030200050330
030300121218      /end-free
030400140331
