000100060621      //********************************************************************************************
000200060621      //
000300060621      // Istruzioni per la compilazione:
000400060621      //
000500060621      // CRTSQLRPGI OBJ([libreria]/TIBS0800R) OBJTYPE(*MODULE)
000600060621      // CRTSRVPGM  SRVPGM([libreria]/TIBS0800R) EXPORT(*ALL)
000700060621      // DLTMOD     MODULE([libreria]/TIBS0800R)
000800060621      //
000900060621      //********************************************************************************************
001000060621     H NOMAIN
001100060621
001200060621      //********************************************************************************************
001300060621      //
001400060621      // Definizione costanti globali.
001500060621      //
001600060621      //********************************************************************************************
001700060621     D ItalianoISO2    C                   'it'
001800060621
001900060621     D*--------------------------------------------------
002000060621     D* Procedure name: inzLingue
002100060621     D* Purpose:        Inizializza schiere lingue.
002200060621     D* Returns:        Numero elementi reperiti.
002300060621     D* Parameter:      rpyOpCode => Codice risposta.
002400060621     D* Parameter:      rpyEsito => Esito.
002500060621     D* Parameter:      rpyElementi => Numero elementi reperiti.
002600060621     D*--------------------------------------------------
002700060621     D inzLingue       PR            10I 0
002800060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002900060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003000060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
003100060621
003200060621     D*--------------------------------------------------
003300060621     D* Procedure name: cvtLinguaISO2ToTabel
003400060621     D* Purpose:        Conversione codice lingua da ISO2 a TABEL00F
003500060621     D* Returns:        Codice lingua TABEL00F
003600060621     D* Parameter:      rqsISO2 => Codice lingua ISO2
003700060621     D* Parameter:      rpyOpCode => Codice risposta.
003800060621     D* Parameter:      rpyEsito => Esito
003900060621     D* Parameter:      rpyTabel => Codice lingua TABEL00F
004000060621     D*--------------------------------------------------
004100060621     D cvtLinguaISO2ToTabel...
004200060621     D                 PR                  LIKE(linTabel)
004300060621     D  rqsISO2                            LIKE(linISO2)
004400060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004500060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004600060621     D  rpyTabel                           LIKE(linTabel)
004700060621     D                                     OPTIONS(*NOPASS:*OMIT)
004800060621
004900060621     D*--------------------------------------------------
005000060621     D* Procedure name: cvtLinguaISO2ToTntbe
005100060621     D* Purpose:        Conversione codice lingua da ISO2 a TNTBE00F.
005200060621     D* Returns:        Codice lingua TNTBE00F.
005300060621     D* Parameter:      rqsISO2 => Codice lingua ISO2
005400060621     D* Parameter:      rpyOpCode => Codice risposta.
005500060621     D* Parameter:      rpyEsito => Esito.
005600060621     D* Parameter:      rpyTntbe => Codice lingua TNTBE00F
005700060621     D*--------------------------------------------------
005800060621     D cvtLinguaISO2ToTntbe...
005900060621     D                 PR                  LIKE(linTntbe)
006000060621     D  rqsISO2                            LIKE(linISO2)
006100060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
006200060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
006300060621     D  rpyTntbe                           LIKE(linTntbe)
006400060621     D                                     OPTIONS(*NOPASS:*OMIT)
006500060621
006600060621      //********************************************************************************************
006700060621      //
006800060621      // Definizione strutture dati.
006900060621      //
007000060621      //********************************************************************************************
007100060621     D azlin00f      E DS
007200060621     D                                     BASED(nullPtr)
007300060621
007400060621      //********************************************************************************************
007500060621      //
007600060621      // Definizione variabili globali.
007700060621      //
007800060621      //********************************************************************************************
007900060621     D nullPtr         S               *
008000060621     D elementi        S             10I 0
008100060621     D aryTabel        S
008200060621     D                                     LIKE(linTabel)
008300060621     D                                     DIM(9)
008400060621     D aryISO2         S
008500060621     D                                     LIKE(linISO2)
008600060621     D                                     DIM(%ELEM(aryTabel))
008700060621     D aryTntbe        S
008800060621     D                                     LIKE(linTntbe)
008900060621     D                                     DIM(%ELEM(aryTabel))
009000060621
009100060621
009200060621     P*--------------------------------------------------
009300060621     P* Procedure name: inzLingue
009400060621     P* Purpose:        Inizializza schiere lingue.
009500060621     P* Returns:        Numero elementi reperiti.
009600060621     P* Parameter:      rpyOpCode => Codice risposta.
009700060621     P* Parameter:      rpyEsito => Esito.
009800060621     P* Parameter:      rpyElementi => Numero elementi reperiti.
009900060621     P*--------------------------------------------------
010000060621     P inzLingue       B                   EXPORT
010100060621     D inzLingue       PI            10I 0
010200060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
010300060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
010400060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
010500060621
010600060621     D rowISO2         S
010700060621     D                                     LIKE(linISO2)
010800060621     D rowTabel        S
010900060621     D                                     LIKE(linTabel)
011000060621     D rowTntbe        S
011100060621     D                                     LIKE(linTntbe)
011200060621
011300060621      /FREE
011400060621
011500060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
011600060621         rpyOpCode = 'DONE';
011700060621       ENDIF;
011800060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
011900060621         CLEAR rpyEsito;
012000060621       ENDIF;
012100060621       IF %PARMS > 2 AND %ADDR(rpyElementi) <> *NULL;
012200060621         CLEAR rpyElementi;
012300060621       ENDIF;
012400060621
012500060621       CLEAR aryTabel;
012600060621       CLEAR aryISO2;
012700060621       CLEAR aryTntbe;
012800060621       CLEAR elementi;
012900060621
013000060621      /END-FREE
013100060621     C/EXEC SQL
013200060621     C+ DECLARE AZLIN00F CURSOR FOR
013300060621     C+ SELECT LINISO2, LINTNTBE, LINTABEL
013400060621     C+ FROM AZLIN00F
013500060621     C+ FOR READ ONLY
013600060621     C/END-EXEC
013700060621     C
013800060621     C/EXEC SQL
013900060621     C+ OPEN AZLIN00F
014000060621     C/END-EXEC
014100060621      /FREE
014200060621
014300060621       IF sqlCod < 0;
014400060621         IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
014500060621           rpyOpCode = 'SQLCOD';
014600060621         ENDIF;
014700060621         IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
014800060621           rpyEsito = sqlCod;
014900060621         ENDIF;
015000060621         RETURN elementi;
015100060621       ENDIF;
015200060621
015300060621       DOU sqlCod = 100;
015400060621      /END-FREE
015500060621     C/EXEC SQL
015600060621     C+ FETCH NEXT FROM AZLIN00F
015700060621     C+ INTO :rowISO2, :rowTntbe, :rowTabel
015800060621     C/END-EXEC
015900060621      /FREE
016000060621         SELECT;
016100060621           WHEN sqlCod = 100;
016200060621             LEAVE;
016300060621           WHEN sqlCod < 0;
016400060621             IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
016500060621               rpyOpCode = 'SQLCOD';
016600060621             ENDIF;
016700060621             IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
016800060621               rpyEsito = sqlCod;
016900060621             ENDIF;
017000060621             LEAVE;
017100060621           OTHER;
017200060621             elementi += 1;
017300060621             aryISO2(elementi) = rowISO2;
017400060621             aryTntbe(elementi) = rowTntbe;
017500060621             aryTabel(elementi) = rowTabel;
017600060621         ENDSL;
017700060621       ENDDO;
017800060621
017900060621      /END-FREE
018000060621     C/EXEC SQL
018100060621     C+ CLOSE AZLIN00F
018200060621     C/END-EXEC
018300060621      /FREE
018400060621
018500060621       IF %PARMS > 2 AND %ADDR(rpyElementi) <> *NULL;
018600060621         rpyElementi = elementi;
018700060621       ENDIF;
018800060621
018900060621       RETURN elementi;
019000060621
019100060621      /END-FREE
019200060621     P inzLingue       E
019300060621
019400060621
019500060621     P*--------------------------------------------------
019600060621     P* Procedure name: cvtLinguaISO2ToTabel
019700060621     P* Purpose:        Conversione codice lingua da ISO2 a TABEL00F
019800060621     P* Returns:        Codice lingua TABEL00F
019900060621     P* Parameter:      rqsISO2 => Codice lingua ISO2
020000060621     P* Parameter:      rpyOpCode => Codice risposta.
020100060621     P* Parameter:      rpyEsito => Esito
020200060621     P* Parameter:      rpyTabel => Codice lingua TABEL00F
020300060621     P*--------------------------------------------------
020400060621     P cvtLinguaISO2ToTabel...
020500060621     P                 B                   EXPORT
020600060621     D cvtLinguaISO2ToTabel...
020700060621     D                 PI                  LIKE(linTabel)
020800060621     D  rqsISO2                            LIKE(linISO2)
020900060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
021000060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
021100060621     D  rpyTabel                           LIKE(linTabel)
021200060621     D                                     OPTIONS(*NOPASS:*OMIT)
021300060621
021400060621     D ItalianoTabel   C                   1
021500060621     D i               S              3I 0
021600060621
021700060621      /FREE
021800060621
021900060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
022000060621         rpyOpCode = 'DONE';
022100060621       ENDIF;
022200060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
022300060621         CLEAR rpyEsito;
022400060621       ENDIF;
022500060621       IF %PARMS > 3 AND %ADDR(rpyTabel) <> *NULL;
022600060621         rpyTabel = ItalianoTabel;
022700060621       ENDIF;
022800060621
022900060621       IF rqsISO2 = *BLANK;
023000060621         rqsISO2 = ItalianoISO2;
023100060621       ENDIF;
023200060621
023300060621       IF elementi = 0;
023400060621         inzLingue();
023500060621       ENDIF;
023600060621
023700060621       i = %LOOKUP(rqsISO2:aryISO2:1:elementi);
023800060621
023900060621       IF i = 0;
024000060621         RETURN ItalianoTabel;
024100060621       ELSE;
024200060621         IF %PARMS > 3 AND %ADDR(rpyTabel) <> *NULL;
024300060621           rpyTabel = aryTabel(i);
024400060621         ENDIF;
024500060621         RETURN aryTabel(i);
024600060621       ENDIF;
024700060621
024800060621      /END-FREE
024900060621     P cvtLinguaISO2ToTabel...
025000060621     P                 E
025100060621
025200060621
025300060621     P*--------------------------------------------------
025400060621     P* Procedure name: cvtLinguaISO2ToTntbe
025500060621     P* Purpose:        Conversione codice lingua da ISO2 a TNTBE00F.
025600060621     P* Returns:        Codice lingua TNTBE00F.
025700060621     P* Parameter:      rqsISO2 => Codice lingua ISO2
025800060621     P* Parameter:      rpyOpCode => Codice risposta.
025900060621     P* Parameter:      rpyEsito => Esito.
026000060621     P* Parameter:      rpyTntbe => Codice lingua TNTBE00F
026100060621     P*--------------------------------------------------
026200060621     P cvtLinguaISO2ToTntbe...
026300060621     P                 B                   EXPORT
026400060621     D cvtLinguaISO2ToTntbe...
026500060621     D                 PI                  LIKE(linTntbe)
026600060621     D  rqsISO2                            LIKE(linISO2)
026700060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
026800060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
026900060621     D  rpyTntbe                           LIKE(linTntbe)
027000060621     D                                     OPTIONS(*NOPASS:*OMIT)
027100060621
027200060621     D ItalianoTntbe   C                   ' '
027300060621     D i               S              3I 0
027400060621
027500060621      /FREE
027600060621
027700060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
027800060621         rpyOpCode = 'DONE';
027900060621       ENDIF;
028000060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
028100060621         CLEAR rpyEsito;
028200060621       ENDIF;
028300060621       IF %PARMS > 3 AND %ADDR(rpyTntbe) <> *NULL;
028400060621         rpyTntbe = ItalianoTntbe;
028500060621       ENDIF;
028600060621
028700060621       IF rqsISO2 = *BLANK;
028800060621         rqsISO2 = ItalianoISO2;
028900060621       ENDIF;
029000060621
029100060621       IF elementi = 0;
029200060621         inzLingue();
029300060621       ENDIF;
029400060621
029500060621       i = %LOOKUP(rqsISO2:aryISO2:1:elementi);
029600060621
029700060621       IF i = 0;
029800060621         RETURN ItalianoTntbe;
029900060621       ELSE;
030000060621         IF %PARMS > 3 AND %ADDR(rpyTntbe) <> *NULL;
030100060621           rpyTntbe = aryTntbe(i);
030200060621         ENDIF;
030300060621         RETURN aryTntbe(i);
030400060621       ENDIF;
030500060621
030600060621      /END-FREE
030700060621     P cvtLinguaISO2ToTntbe...
030800060621     P                 E
030900060621
