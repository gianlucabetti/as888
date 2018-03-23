000100061025      //********************************************************************************************
000200061025      //
000300061025      // Questo programma riceve un codice cliente e ne restituisce l' anagrafica gestionale
000400061025      // completa. Se il cliente è non codificato, reperisce l' anagrafica dalla particolarità
000500061025      // di partita.
000600061025      //
000700061025      //********************************************************************************************
000800061025     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('PJXBND')
000900061025
001000061025      //********************************************************************************************
001100061025      //
001200061025      // Definizione files.
001300061025      //
001400061025      //********************************************************************************************
001500061025
001600061025      //********************************************************************************************
001700061025      //
001800061025      // Definizione costanti.
001900061025      //
002000061025      //********************************************************************************************
002100061025     D FranchiVari     C                   '8888'
002200061025     D Assegnati       C                   '9999'
002300061025
002400061025      //********************************************************************************************
002500061025      //
002600061025      // Definizione procedure usate.
002700061025      //
002800061025      //********************************************************************************************
002900061025     D xSoc            PR                  EXTPROC('XSOC')
003000061025     D  tipXsc                        6    CONST
003100061025     D  socXsc                             LIKE(xscSoc)
003200061025     D  cdsXsc                        9P 0 CONST
003300061025     D  modXsc                        3    CONST
003400061025     D  rtnXsc                        1
003500061025     D  xSoc001ds                          LIKEDS(xSoc001ds)
003600061025     D  kpjba                              LIKEDS(kpjba)
003700061025     D tibs69r         PR                  EXTPGM('TIBS69R')
003800061025     D  tibs69ds                           LIKEDS(tibs69ds)
003900061025     D  cnaco00f                           LIKEDS(cnaco00f)
004000061025     D  cnind00f                           LIKEDS(cnind00f)
004100061025     D  cnclp00f                           LIKEDS(cnclp00f)
004200061025     D  fncls00f                           LIKEDS(fncls00f)
004300061025
004400061025      //********************************************************************************************
004500061025      //
004600061025      // Definizione strutture dati.
004700061025      //
004800061025      //********************************************************************************************
004900061025     D tibs69ds      E DS                  INZ
005000061025     D cnaco00f      E DS
005100061025     D cnind00f      E DS
005200061025     D cnclp00f      E DS
005300061025     D fncls00f      E DS
005400061025     D andiz00f      E DS                  BASED(nullPtr) PREFIX(pjz)
005500061025     D kpjba         E DS                  INZ
005600061025     D xSoc001ds     E DS                  INZ
005700061025
005800061025      //********************************************************************************************
005900061025      //
006000061025      // Definizione variabili.
006100061025      //
006200061025      //********************************************************************************************
006300061025     D nullPtr         S               *
006400061025     D rqsSocieta      S                   LIKE(pjzSocieta)
006500061025     D rqsCtb          S                   LIKE(pjzCtb)
006600061025     D rqsKcc          S                   LIKE(pjzKcc)
006700061025     D rqsKsc          S                   LIKE(pjzKsc)
006800061025     D rqsDtPar        S                   LIKE(pjzData)
006900061025     D rqsNrPar        S                   LIKE(pjzNr)
007000061025     D rqsSerPar       S                   LIKE(pjzSerieNm)
007100061025     D rqsCnaco00f     S              1N
007200061025     D rqsCnind00f     S              1N
007300061025     D rqsCnclp00f     S              1N
007400061025     D rqsFncls00f     S              1N
007500061025     D rpyEsito        S             10I 0
007600061025     D rpyOpCode       S             10A
007700061025     D rpyStatus       S             10I 0
007800061025     D rtnXsc          S              1A
007900061025
008000061025      //********************************************************************************************
008100061025      //
008200061025      // Definizione parametri.
008300061025      //
008400061025      //********************************************************************************************
008500061025     C     *ENTRY        PLIST
008600061025     C                   PARM                    rqsSocieta
008700061025     C                   PARM                    rqsCtb
008800061025     C                   PARM                    rqsKcc
008900061025     C                   PARM                    rqsKsc
009000061025     C                   PARM                    rqsDtPar
009100061025     C                   PARM                    rqsNrPar
009200061025     C                   PARM                    rqsSerPar
009300061025     C                   PARM                    rqsCnaco00f
009400061025     C                   PARM                    rqsCnind00f
009500061025     C                   PARM                    rqsCnclp00f
009600061025     C                   PARM                    rqsFncls00f
009700061025     C                   PARM                    rpyEsito
009800061025     C                   PARM                    rpyOpCode
009900061025     C                   PARM                    rpyStatus
010000061025     C                   PARM                    cnaco00f
010100061025     C                   PARM                    cnind00f
010200061025     C                   PARM                    cnclp00f
010300061025     C                   PARM                    fncls00f
010400061025
010500061025      /FREE
010600061025
010700061025       //*******************************************************************************************
010800061025       //
010900061025       // Main.
011000061025       //
011100061025       //*******************************************************************************************
011200061025       EXSR chkParms;
011300061025
011400061025       EXSR getClienteCodificato;
011500061025
011600061025       IF %SUBST(rqsKsc:5:4) = FranchiVari OR %SUBST(rqsKsc:5:4) = Assegnati;
011700061025         EXSR getClienteNonCodificato;
011800061025       ENDIF;
011900061025
011901180313       IF %SUBST(indCaE:1:4) = '4789' AND (indSta = 'IT' OR indSta = *BLANK);
011903180313         indSta = 'SM';
011904180313         CLEAR indPrv;
011905180313       ENDIF;
011906180313
011907180313       IF indIva = '$$';
011908180313         CLEAR indIva;
011909180313       ENDIF;
011910180313
011911180313       IF indCdf = '$$';
011912180313         CLEAR indCdf;
011913180313       ENDIF;
011914180313
012000061025       EXSR uscita;
012100061025
012200061025       //*******************************************************************************************
012300061025       //
012400061025       // Ricezione e controllo parametri ricevuti.
012500061025       //
012600061025       //*******************************************************************************************
012700061025       BEGSR *INZSR;
012800061025
012900061025       ENDSR;
013000061025
013100061025       //*******************************************************************************************
013200061025       //
013300061025       // Ricezione e controllo parametri ricevuti.
013400061025       //
013500061025       //*******************************************************************************************
013600061025       BEGSR chkParms;
013700061025
013800061025         CLEAR rpyEsito;
013900061025         CLEAR rpyOpCode;
014000061025         CLEAR rpyStatus;
014100061025         CLEAR cnaco00f;
014200061025         CLEAR cnind00f;
014300061025         CLEAR cnclp00f;
014400061025         CLEAR fncls00f;
014500061025
014600061025         IF rqsKsc = *BLANK; // Codice cliente obbligatorio.
014700061025           rpyEsito = -1;
014800061025           rpyOpCode = 'ERRORE';
014900061025           rpyStatus = 1;
015000061025           EXSR uscita;
015100061025         ENDIF;
015200061025
015300061025         IF rqsSocieta = *BLANK; // Codice società obbligatorio.
015400061025           CLEAR kpjbu;
015500061025           xSoc('SOC002' : rqsSocieta : *ZERO : *BLANK : rtnXsc : xSoc001ds
015600061025           : kpjba);
015700061025           IF rtnXsc = *ON;
015800061025             rpyEsito = -1;
015900061025             rpyOpCode = 'ERRORE';
016000061025             rpyStatus = 2;
016100061025             EXSR uscita;
016200061025           ENDIF;
016300061025         ENDIF;
016400061025
016500061025         IF rqsKcc = *BLANK; // Capo conto cliente obbligatorio.
016600061025      /END-FREE
016700061025     C/EXEC SQL
016800061025     C+ SELECT CLNKCC INTO :rqsKcc
016900061025     C+ FROM ANCLN00F
017000061025     C+ WHERE CLNSOCIETA = :rqsSocieta AND clnKsc = :rqsKsc AND
017100061025     C+       CLNFILIALE = 0 AND CLNLINEAV = ' '
017200061025     C+ ORDER BY CLNSOCIETA, CLNKSC, CLNFILIALE, CLNLINEAV
017300061025     C/END-EXEC
017400061025      /FREE
017500061025           IF sqlCode < 0 OR sqlCode = 100;
017600061025             rpyEsito = -1;
017700061025             rpyOpCode = 'SQLCODE';
017800061025             rpyStatus = sqlCode;
017900061025             EXSR uscita;
018000061025           ENDIF;
018100061025         ENDIF;
018200061025
018300061025         IF NOT rqsCnaco00f AND NOT rqsCnind00f AND
018400061025            NOT rqsCnclp00f AND NOT rqsFncls00f;
018500061025           rpyEsito = -1;
018600061025           rpyOpCode = 'ERRORE';
018700061025           rpyStatus = 3;
018800061025           EXSR uscita;
018900061025         ENDIF;
019000061025
019100061025         IF rqsCtb = *BLANK;
019200061025           rqsCtb = 'CG';
019300061025         ENDIF;
019400061025
019500061025       ENDSR;
019600061025
019700061025       //*******************************************************************************************
019800061025       //
019900061025       // Reperisco anagrafica cliente codificato.
020000061025       //
020100061025       //*******************************************************************************************
020200061025       BEGSR getClienteCodificato;
020300061025
020400061025         RESET tibs69ds;
020500061025
020600061025         IF rqsCnaco00f;
020700061025           i69Kac = %DEC(%SUBST(rqsKsc:2:7):7:0);
020800061025         ENDIF;
020900061025         IF rqsCnind00f;
021000061025           i69Kin = %DEC(%SUBST(rqsKsc:2:7):7:0);
021100061025         ENDIF;
021200061025         IF rqsCnclp00f;
021300061025           i69Kcp = %DEC(%SUBST(rqsKsc:2:7):7:0);
021400061025         ENDIF;
021500061025         IF rqsFncls00f;
021600061025           i69Kcs = %DEC(%SUBST(rqsKsc:2:7):7:0);
021700061025         ENDIF;
021800061025
021900061025         tibs69r(tibs69ds : cnaco00f : cnind00f : cnclp00f : fncls00f);
022000061025
022100061025         IF o69Err = *ON;
022200061025           rpyEsito = -1;
022300061025           rpyOpCode = 'ERRORE';
022400061025           rpyStatus = 101;
022500061025           EXSR uscita;
022600061025         ENDIF;
022700061025
022800061025       ENDSR;
022900061025
023000061025       //*******************************************************************************************
023100061025       //
023200061025       // Reperisco anagrafica cliente non codificato.
023300061025       //
023400061025       //*******************************************************************************************
023500061025       BEGSR getClienteNonCodificato;
023600061025
023700061025      /END-FREE
023800061025     C/EXEC SQL
023900061025     C+ SELECT CPADESCR, LEFT(CPAINDIRIZ, 30), CASE WHEN CPACAP BETWEEN '00000'
024000061025     C+        AND '99999' THEN DEC(CPACAP, 5, 0) ELSE 0 END,
024100061025     C+        LEFT(CPALOCALIT, 30), CPAPROV, CPASTATO, LEFT(CPACDFISC, 16),
024200061025     C+        LEFT(CPAPARTIVA, 16), CASE WHEN CPAABI BETWEEN '00000' AND
024300061025     C+        '99999' THEN DEC(CPAABI) ELSE 0 END, CASE WHEN CPACAB BETWEEN
024400061025     C+        '00000' AND '99999' THEN DEC(CPACAB) ELSE 0 END,
024500070208     C+        LEFT(CPACCB, 12) CONCAT '   ' CONCAT CPACIN, CPACAP
024600061025     C+ INTO :acoRag, :indVia, :indCap, :indCit, :indPrv, :indSta, :indCdf,
024700070208     C+      :indIva, :clpAbi, :clpCab, :clpCcb, :indCae
024800061025     C+ FROM NDPPA00F JOIN NDCPA00F
024900061025     C+ ON PPASYS = CPASYS AND PPANRASINT = CPANRASINT
025000061025     C+ WHERE PPASOCIETA = :rqsSocieta AND PPACTB = :rqsCtb AND
025100061025     C+       PPAKCC = :rqsKcc AND PPAKSC = :rqsKsc AND
025200061025     C+       PPADTPAR = :rqsDtPar AND PPANRPAR = :rqsNrPar AND
025300061025     C+       PPASERPAR = :rqsSerPar
025400061025     C+ ORDER BY PPASOCIETA, PPACTB, PPAKCC, PPAKSC, PPADTPAR, PPANRPAR,
025500061025     C+          PPASERPAR
025600061025     C/END-EXEC
025700061025      /FREE
025800061025
025900061025         SELECT;
026000061025           WHEN sqlCode < 0;
026100061025             rpyEsito = -1;
026200061025             rpyOpCode = 'SQLCODE';
026300061025             rpyStatus = sqlCode;
026400061025             EXSR uscita;
026500061025           WHEN sqlCode = 100;
026600061025             rpyEsito = 1;
026700061025             rpyOpCode = 'ERRORE';
026800061025             rpyStatus = 201;
026900061025         ENDSL;
027000061025
027100061025       ENDSR;
027200061025
027300061025       //*******************************************************************************************
027400061025       //
027500061025       // Uscita.
027600061025       //
027700061025       //*******************************************************************************************
027800061025       BEGSR uscita;
027900061025
028000061025         IF rpyOpCode = *BLANK;
028100061025           rpyOpCode = 'DONE';
028200061025         ENDIF;
028300061025
028400061025         RETURN;
028500061025
028600061025       ENDSR;
028700061025
028800061025      /END-FREE
