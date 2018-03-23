000100091228       //==============================================================
000200130919       // Pgm di utility per reperimento dettaglio segnacolli
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100091228
001200091228       //--------------------------------------------------------------
001300121106       // Specifiche di controllo.
001400091228       //--------------------------------------------------------------
001500091223
001600091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700100325     h dftactgrp(*no)
001800091223     h alwnull(*inputonly)
001900100325     h bnddir('UBBNDDIR')
002000091223
002100091223       //--------------------------------------------------------------
002200121106       // Dichiarazione file.
002300091223       //--------------------------------------------------------------
002400100112
002500130919     FTITAT30C  IF   E           K DISK    usropn
002600130920     FTITAS30C  IF   E           K DISK
002700091223
002800091223       //--------------------------------------------------------------
002900121106       // Definizione costanti.
003000091223       //--------------------------------------------------------------
003100100302
003200091223
003300091223       //--------------------------------------------------------------
003400121106       // Definizione schiere.
003500091223       //--------------------------------------------------------------
003600091223
003700091223
003800091223       //--------------------------------------------------------------
003900121106       // Definizione aree dati.
004000091223       //--------------------------------------------------------------
004100091223
004200091223
004300091223       //--------------------------------------------------------------
004400121106       // Definizione strutture dati.
004500091223       //--------------------------------------------------------------
004600110516
004700121106       // - Status
004800110516     d Psds           sds
004900110516     d   SDSpgm          *proc
005000121106     d   JobName             244    253                                         Job name
005100121106     d   JobUser             254    263                                         User name
005200121106     d   JobNumber           264    269s 0                                      Job number
005300121106
005400130920       // reperimento check digits
005500130920     D TRUL28DS      e ds                  inz
005600091223
005700091223       //--------------------------------------------------------------
005800121106       // Definizione variabili.
005900091223       //--------------------------------------------------------------
006000091223
006100121106       // - Parametri ricevuti:
006200130919     d pIn_AAS         s              4s 0
006300130919     d pIn_LNP         s              3s 0
006400130919     d pIn_NRS         s              2s 0
006500130919     d pIn_NSP         s              7s 0
006600130919     d pIn_NCL         s              5s 0
006700130919
006800130919     d pOut_SGN        s             18    dim(1000)
006900130919     d pOut_NrSGN      s              3s 0
007000130919     d pOut_Esito      s              1
007100121106
007200121106       // Campi di comodo
007300130920     d wDate           s              8  0 inz
007400130920     d i               s              7s 0
007500130920
007600121106     d wTable          s             10
007700121106     d wInit           s              3
007800121107     d wStart          s             10
007900121107     d wEnd            s             10
008000121107     d wErroreC5       s              1
008100121107     d count           s              7s 0 inz
008200121108     d wLib            s             11
008300091223
008400091223       //--------------------------------------------------------------
008500121106       // Definizione prototipi procedure.
008600091223       //--------------------------------------------------------------
008700091223
008800130920     d TRUL28R1        pr                  extpgm('TRUL28R1')
008900130920     D  pTRUL28DS                          like(TRUL28DS)
009000130920
009100091223       //--------------------------------------------------------------
009200121106       // Definizione key-list.
009300091223       //--------------------------------------------------------------
009400091223
009500091223
009600091223       //--------------------------------------------------------------
009700121107       // Definizione parametri procedura.
009800091223       //--------------------------------------------------------------
009900091223
010000091223     c     *Entry        plist
010100130919     c                   parm                    pIn_AAS
010200130919     c                   parm                    pIn_LNP
010300130919     c                   parm                    pIn_NRS
010400130919     c                   parm                    pIn_NSP
010500130919     c                   parm                    pIn_NCL
010600130919     c                   parm                    pOut_SGN
010700130919     c                   parm                    pOut_NrSGN
010800130919     c                   parm                    pOut_Esito
010900091223
011000091223      /free
011100091223
011200091223       //--------------------------------------------------------------
011300121106       // M A I N - L I N E
011400091223       //--------------------------------------------------------------
011500091223
011600121106       // Operazioni iniziali?
011700091223       exsr sr_RoutInz;
011800091223
011900130919       // Verifica parametri input
012000130919       exsr  VerPIn;
012100091223
012200130919       // verifica segnacolli sequenziali o no
012300130919       exsr VerSGNSeq;
012400130919
012500130920       // valutazione dei risultati
012600130920       exsr ValRisultati;
012700130920
012800130919       // Operazioni finali?
012900130919       exsr sr_RoutEnd;
013000091223
013100091223       //--------------------------------------------------------------
013200121107       // Operazioni iniziali.
013300091223       //--------------------------------------------------------------
013400091223       BEGSR  sr_RoutInz;
013500091223
013600091223         *inLR = *on;
013700100112
013800121108         // inizializzo i dati di output
013900130920         clear pOut_SGN;
014000130920         clear pOut_NrSGN;
014100130920         clear pOut_Esito;
014200121108
014300121108         // Reperimento data odierna (fmt aaaa/mm/gg)
014400121108         wDate = %dec( %date() );
014500091223
014600091223       ENDSR;
014700110523
014800110523       //--------------------------------------------------------------
014900130920       // Verifica parametri input
015000110523       //--------------------------------------------------------------
015100130920       BEGSR  VerPIn;
015200110523
015300130920         // controllo che i dati obbligatori siano pieni;
015400130920         if pIn_AAS = 0
015500130920         or pIn_LNP = 0
015600130920         or pIn_NSP = 0;
015700130920           pOut_Esito = '1';
015800130920           exsr sr_RoutEnd;
015900130920         endif;
016000121106
016100121106       ENDSR;
016200130920
016300130920       //--------------------------------------------------------------
016400130920       // Verifica segnacolli sequenziali o no
016500130920       //--------------------------------------------------------------
016600130920       BEGSR  VerSGNSeq;
016700130920
016800130920         // reperisco la testata e verifico se i segnacolli sono sequenziali
016900130920         chain (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP) titas30c;
017000130920         // se trovato il rcd, sviluppo tanti segnacolli quanto sono i colli totali
017100130920         // e poi esco dalla routine
017200130920         if %found(titas30c);
017300130920           // se non è valorizzato il nr. segnacollo DA allora siamo nel caso di segnacolli non
017400130920           // sequenziali
017500130920           if TASNCD <> 0;
017600130920             for i = TASNCD to (TASNCD + TASNCL);
017700130920               pOut_SGN(i - TASNCD + 1)=(
017800130920                 %editc(TASLNP:'X')+%editc(TASLNA:'X')+%editc(TASNRS:'X')+
017900130920                 %editc(i:'X')+%editc(TASZNC:'X'));
018000130920               // calcolo il check digit
018100130920               clear TRUL28DS;
018200130920               I28Mod  = 'BAR';
018300130920               I28COD = %trim(pOut_SGN(i - TASNCD + 1));
018400130920               TRUL28R1(TRUL28DS);
018500130920               // Se tutto ok aggiungo il check digit
018600130920               if O28err = *blanks;
018700130920                 pOut_SGN(i - TASNCD + 1) = O28Cod;
018800130920               endif;
018900130920             endfor;
019000130920             pOut_NrSGN = TASNCL;
019100130920             pOut_Esito = 'S';
019200130920             // esco dalla routine
019300130920             leavesr;
019400130920           endif;
019500130920
019600130920           // se sono qui la testata è stata reperita ma i segnacolli sono NON sequenziali
019700130920           if not %open(titat30c);
019800130920              open titat30c;
019900130920           endif;
020000130920           i = 0;
020100130920           setll (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP) titat30c;
020200130920           if %equal(titat30c);
020300130920             reade  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP) titat30c;
020400130920             dow not %eof(titat30c);
020500130920               // creo il segnacollo
020600130920               i = i + 1;
020700130920               pOut_SGN(i) =
020800130920                 %editc(TATLNP:'X')+%editc(TATLNA:'X')+%editc(TATNRS:'X')+
020900130920                 %editc(TATNSC:'X')+%editc(TASZNC:'X');
021000130920               // calcolo il check digit
021100130920               clear TRUL28DS;
021200130920               I28Mod  = 'BAR';
021300130920               I28COD = %trim(pOut_SGN(i));
021400130920               TRUL28R1(TRUL28DS);
021500130920               // Se tutto ok aggiungo il check digit
021600130920               if O28err = *blanks;
021700130920                 pOut_SGN(i) = O28Cod;
021800130920               endif;
021900130920
022000130920               // leggo il successivo
022100130920               reade  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP) titat30c;
022200130920             enddo;
022300130920             pOut_NrSGN = i;
022400130920             pOut_Esito = 'N';
022500130920             // esco dalla routine
022600130920             leavesr;
022700130920           endif;
022800130920
022900130920         endif;
023000130920
023100130920         // se sono arrivato sin qui, vuol dire che non ho trovato segnacolli o la spedizione
023200130920         pOut_Esito = '1';
023300130920         exsr sr_RoutEnd;
023400130920
023500130920       ENDSR;
023600121106
023700091223       //--------------------------------------------------------------
023800130920       // Valutazione dei risultati
023900091223       //--------------------------------------------------------------
024000130920       BEGSR  ValRisultati;
024100130109
024200130920         // se il numero colli in ingresso è <> 0 e <> dal nr. segnacolli reperiti
024300130920         if pIn_NCL <> 0 and pIn_NCL <> pOut_NrSGN;
024400130920           // lo segnalo
024500130920           pOut_Esito = 'X';
024600130920         endif;
024700091223
024800091223       ENDSR;
024900130920
025000130920       //--------------------------------------------------------------
025100130920       // Operazioni finali.
025200130920       //--------------------------------------------------------------
025300130920       BEGSR  sr_RoutEnd;
025400130920
025500130920         // Chiusura pgm
025600130920         return;
025700130920
025800130920       ENDSR;
025900091223
026000091223      /end-free
