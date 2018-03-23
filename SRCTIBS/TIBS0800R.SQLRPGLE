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
001700091028     D ITALIANO_ISO2   C                   'it'
001800091028     D ITALIANO_ISO3   C                   'ita'
001900091028
002000091028      //********************************************************************************************
002100091028      //
002200091028      // Procedure.
002300091028      //
002400091028      //********************************************************************************************
002500091028     D/COPY GAITRASRC/SRCPROTOPR,TIBS0800R
002600060621
002700060621      //********************************************************************************************
002800060621      //
002900060621      // Definizione strutture dati.
003000060621      //
003100060621      //********************************************************************************************
003200060621     D azlin00f      E DS
003300060621     D                                     BASED(nullPtr)
003400060621
003500060621      //********************************************************************************************
003600060621      //
003700060621      // Definizione variabili globali.
003800060621      //
003900060621      //********************************************************************************************
004000060621     D nullPtr         S               *
004100060621     D elementi        S             10I 0
004200060621     D aryTabel        S
004300060621     D                                     LIKE(linTabel)
004400060621     D                                     DIM(9)
004500060621     D aryISO2         S
004600060621     D                                     LIKE(linISO2)
004700060621     D                                     DIM(%ELEM(aryTabel))
004800091028     D aryISO3         S
004900091028     D                                     LIKE(linISO3)
005000091028     D                                     DIM(%ELEM(aryTabel))
005100060621     D aryTntbe        S
005200060621     D                                     LIKE(linTntbe)
005300060621     D                                     DIM(%ELEM(aryTabel))
005400060621
005500060621
005600060621     P*--------------------------------------------------
005700060621     P* Procedure name: inzLingue
005800060621     P* Purpose:        Inizializza schiere lingue.
005900060621     P* Returns:        Numero elementi reperiti.
006000060621     P* Parameter:      rpyOpCode => Codice risposta.
006100060621     P* Parameter:      rpyEsito => Esito.
006200060621     P* Parameter:      rpyElementi => Numero elementi reperiti.
006300060621     P*--------------------------------------------------
006400060621     P inzLingue       B                   EXPORT
006500060621     D inzLingue       PI            10I 0
006600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
006700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
006800060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
006900060621
007000060621     D rowISO2         S
007100060621     D                                     LIKE(linISO2)
007200091028     D rowISO3         S
007300091028     D                                     LIKE(linISO3)
007400060621     D rowTabel        S
007500060621     D                                     LIKE(linTabel)
007600060621     D rowTntbe        S
007700060621     D                                     LIKE(linTntbe)
007800060621
007900060621      /FREE
008000060621
008100060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
008200060621         rpyOpCode = 'DONE';
008300060621       ENDIF;
008400060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
008500060621         CLEAR rpyEsito;
008600060621       ENDIF;
008700060621       IF %PARMS > 2 AND %ADDR(rpyElementi) <> *NULL;
008800060621         CLEAR rpyElementi;
008900060621       ENDIF;
009000060621
009100060621       CLEAR aryTabel;
009200060621       CLEAR aryISO2;
009300060621       CLEAR aryTntbe;
009400060621       CLEAR elementi;
009500060621
009600060621      /END-FREE
009700060621     C/EXEC SQL
009800091028     C+ DECLARE AZLIN00F NO SCROLL CURSOR FOR
009900091028     C+ SELECT LINISO2, LINTNTBE, LINTABEL, LINISO3
010000060621     C+ FROM AZLIN00F
010100060621     C+ FOR READ ONLY
010200060621     C/END-EXEC
010300060621     C
010400060621     C/EXEC SQL
010500060621     C+ OPEN AZLIN00F
010600060621     C/END-EXEC
010700060621      /FREE
010800060621
010900060621       IF sqlCod < 0;
011000060621         IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
011100060621           rpyOpCode = 'SQLCOD';
011200060621         ENDIF;
011300060621         IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
011400060621           rpyEsito = sqlCod;
011500060621         ENDIF;
011600060621         RETURN elementi;
011700060621       ENDIF;
011800060621
011900060621       DOU sqlCod = 100;
012000060621      /END-FREE
012100060621     C/EXEC SQL
012200060621     C+ FETCH NEXT FROM AZLIN00F
012300091028     C+ INTO :rowISO2, :rowTntbe, :rowTabel, :rowISO3
012400060621     C/END-EXEC
012500060621      /FREE
012600060621         SELECT;
012700060621           WHEN sqlCod = 100;
012800060621             LEAVE;
012900060621           WHEN sqlCod < 0;
013000060621             IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
013100060621               rpyOpCode = 'SQLCOD';
013200060621             ENDIF;
013300060621             IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
013400060621               rpyEsito = sqlCod;
013500060621             ENDIF;
013600060621             LEAVE;
013700060621           OTHER;
013800060621             elementi += 1;
013900060621             aryISO2(elementi) = rowISO2;
014000091028             aryISO3(elementi) = rowISO3;
014100060621             aryTntbe(elementi) = rowTntbe;
014200060621             aryTabel(elementi) = rowTabel;
014300060621         ENDSL;
014400060621       ENDDO;
014500060621
014600060621      /END-FREE
014700060621     C/EXEC SQL
014800060621     C+ CLOSE AZLIN00F
014900060621     C/END-EXEC
015000060621      /FREE
015100060621
015200060621       IF %PARMS > 2 AND %ADDR(rpyElementi) <> *NULL;
015300060621         rpyElementi = elementi;
015400060621       ENDIF;
015500060621
015600060621       RETURN elementi;
015700060621
015800060621      /END-FREE
015900060621     P inzLingue       E
016000060621
016100060621
016200060621     P*--------------------------------------------------
016300060621     P* Procedure name: cvtLinguaISO2ToTabel
016400060621     P* Purpose:        Conversione codice lingua da ISO2 a TABEL00F
016500060621     P* Returns:        Codice lingua TABEL00F
016600060621     P* Parameter:      rqsISO2 => Codice lingua ISO2
016700060621     P* Parameter:      rpyOpCode => Codice risposta.
016800060621     P* Parameter:      rpyEsito => Esito
016900060621     P* Parameter:      rpyTabel => Codice lingua TABEL00F
017000060621     P*--------------------------------------------------
017100060621     P cvtLinguaISO2ToTabel...
017200060621     P                 B                   EXPORT
017300060621     D cvtLinguaISO2ToTabel...
017400060621     D                 PI                  LIKE(linTabel)
017500060621     D  rqsISO2                            LIKE(linISO2)
017600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
017700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
017800060621     D  rpyTabel                           LIKE(linTabel)
017900060621     D                                     OPTIONS(*NOPASS:*OMIT)
018000060621
018100060621     D ItalianoTabel   C                   1
018200060621     D i               S              3I 0
018300060621
018400060621      /FREE
018500060621
018600060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
018700060621         rpyOpCode = 'DONE';
018800060621       ENDIF;
018900060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
019000060621         CLEAR rpyEsito;
019100060621       ENDIF;
019200060621       IF %PARMS > 3 AND %ADDR(rpyTabel) <> *NULL;
019300060621         rpyTabel = ItalianoTabel;
019400060621       ENDIF;
019500060621
019600060621       IF rqsISO2 = *BLANK;
019700091028         rqsISO2 = ITALIANO_ISO2;
019800060621       ENDIF;
019900060621
020000060621       IF elementi = 0;
020100060621         inzLingue();
020200060621       ENDIF;
020300060621
020400060621       i = %LOOKUP(rqsISO2:aryISO2:1:elementi);
020500060621
020600060621       IF i = 0;
020700060621         RETURN ItalianoTabel;
020800060621       ELSE;
020900060621         IF %PARMS > 3 AND %ADDR(rpyTabel) <> *NULL;
021000060621           rpyTabel = aryTabel(i);
021100060621         ENDIF;
021200060621         RETURN aryTabel(i);
021300060621       ENDIF;
021400060621
021500060621      /END-FREE
021600060621     P cvtLinguaISO2ToTabel...
021700060621     P                 E
021800060621
021900060621
022000060621     P*--------------------------------------------------
022100060621     P* Procedure name: cvtLinguaISO2ToTntbe
022200060621     P* Purpose:        Conversione codice lingua da ISO2 a TNTBE00F.
022300060621     P* Returns:        Codice lingua TNTBE00F.
022400060621     P* Parameter:      rqsISO2 => Codice lingua ISO2
022500060621     P* Parameter:      rpyOpCode => Codice risposta.
022600060621     P* Parameter:      rpyEsito => Esito.
022700060621     P* Parameter:      rpyTntbe => Codice lingua TNTBE00F
022800060621     P*--------------------------------------------------
022900060621     P cvtLinguaISO2ToTntbe...
023000060621     P                 B                   EXPORT
023100060621     D cvtLinguaISO2ToTntbe...
023200060621     D                 PI                  LIKE(linTntbe)
023300060621     D  rqsISO2                            LIKE(linISO2)
023400060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
023500060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
023600060621     D  rpyTntbe                           LIKE(linTntbe)
023700060621     D                                     OPTIONS(*NOPASS:*OMIT)
023800060621
023900060621     D ItalianoTntbe   C                   ' '
024000060621     D i               S              3I 0
024100060621
024200060621      /FREE
024300060621
024400060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
024500060621         rpyOpCode = 'DONE';
024600060621       ENDIF;
024700060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
024800060621         CLEAR rpyEsito;
024900060621       ENDIF;
025000060621       IF %PARMS > 3 AND %ADDR(rpyTntbe) <> *NULL;
025100060621         rpyTntbe = ItalianoTntbe;
025200060621       ENDIF;
025300060621
025400060621       IF rqsISO2 = *BLANK;
025500091028         rqsISO2 = ITALIANO_ISO2;
025600060621       ENDIF;
025700060621
025800060621       IF elementi = 0;
025900060621         inzLingue();
026000060621       ENDIF;
026100060621
026200060621       i = %LOOKUP(rqsISO2:aryISO2:1:elementi);
026300060621
026400060621       IF i = 0;
026500060621         RETURN ItalianoTntbe;
026600060621       ELSE;
026700060621         IF %PARMS > 3 AND %ADDR(rpyTntbe) <> *NULL;
026800060621           rpyTntbe = aryTntbe(i);
026900060621         ENDIF;
027000060621         RETURN aryTntbe(i);
027100060621       ENDIF;
027200060621
027300060621      /END-FREE
027400060621     P cvtLinguaISO2ToTntbe...
027500060621     P                 E
027600060621
027700091028
027800091028     P*--------------------------------------------------
027900091028     P* Procedure name: cvtLinguaISO3ToISO2
028000091028     P* Purpose:        Conversione codice lingua da ISO3 a ISO2.
028100091028     P* Returns:        Codice lingua ISO2.
028200091028     P* Parameter:      rqsISO3 => Codice lingua ISO3
028300091028     P* Parameter:      rpyOpCode => Codice risposta.
028400091028     P* Parameter:      rpyEsito => Esito.
028500091028     P* Parameter:      rpyISO2 => Codice lingua ISO2
028600091028     P*--------------------------------------------------
028700091028     P cvtLinguaISO3ToISO2...
028800091028     P                 B                   EXPORT
028900091028     D cvtLinguaISO3ToISO2...
029000091028     D                 PI             2A
029100091028     D  rqsISO3                       3A
029200091028     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
029300091028     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
029400091028     D  rpyISO2                       2A   OPTIONS(*NOPASS:*OMIT)
029500091028
029600091028     D i               S              3I 0
029700091028
029800091028      /FREE
029900091028
030000091028       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
030100091028         rpyOpCode = 'DONE';
030200091028       ENDIF;
030300091028       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
030400091028         CLEAR rpyEsito;
030500091028       ENDIF;
030600091028       IF %PARMS > 3 AND %ADDR(rpyIso2) <> *NULL;
030700091028         rpyISO2 = ITALIANO_ISO2;
030800091028       ENDIF;
030900091028
031000091028       IF rqsISO3 = *BLANK;
031100091028         rqsISO3 = ITALIANO_ISO3;
031200091028       ENDIF;
031300091028
031400091028       IF elementi = 0;
031500091028         inzLingue();
031600091028       ENDIF;
031700091028
031800091028       i = %LOOKUP(rqsISO3 : aryISO3 : 1 : elementi);
031900091028
032000091028       IF i = 0;
032100091028         RETURN ITALIANO_ISO2;
032200091028       ELSE;
032300091028         IF %PARMS > 3 AND %ADDR(rpyISO2) <> *NULL;
032400091028           rpyISO2 = aryISO2(i);
032500091028         ENDIF;
032600091028         RETURN aryISO2(i);
032700091028       ENDIF;
032800091028
032900091028      /END-FREE
033000091028     P cvtLinguaISO3ToISO2...
033100091028     P                 E
