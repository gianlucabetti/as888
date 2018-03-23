000100090402     ***************************************************************************
000200090402     **
000300120319     ** Società delle filiali.
000400120319     ** Selettore righe.
000500090402     **
000600090402     ***************************************************************************
000700120319     H DFTACTGRP(*NO) ACTGRP('AZSOF') BNDDIR('PJXBND')
000800090402
000900090402     ***************************************************************************
001000090402     **
001100120319     ** Costanti.
001200090402     **
001300090402     ***************************************************************************
001400120319     D/COPY GAITRASRC/SRCCONST,TNSD4100R
001500090402     D ESITO_ERROR...
001600090402     D                 C                   -1
001700090402     D ESITO_OK...
001800090402     D                 C                   0
001900090402     D ESITO_WARNING...
002000090402     D                 C                   1
002100090402
002200090402     ***************************************************************************
002300090402     **
002400120319     ** Prototipi.
002500090402     **
002600090402     ***************************************************************************
002700120319     D/COPY GAITRASRC/SRCPROTOPR,TNSD4100R
002800090402
002900090402     ***************************************************************************
003000090402     **
003100120319     ** Strutture dati.
003200090402     **
003300090402     ***************************************************************************
003400120321     D filiali_ary     DS                  QUALIFIED DIM(99)
003500120321     D  idFiliale                     5I 0
003600120321     D  idRiga                       10I 0
003700120319     D kpjba         E DS                  QUALIFIED INZ
003800120319     D  knmus        E                     INZ(*USER)
003900120319     D tnsd4130i     E DS                  QUALIFIED INZ(*EXTDFT)
004000120321     D tnsd4131i     E DS                  QUALIFIED INZ(*EXTDFT)
004100120319     D tnsd4130o     E DS                  QUALIFIED INZ(*EXTDFT)
004200120321     D tnsd4131o     E DS                  QUALIFIED INZ(*EXTDFT)
004300120321     D  idRighe_ary                  10I 0 DIM(99) OVERLAY(idRighe)
004400120321     D  idFiliali_ary                 5I 0 DIM(99) OVERLAY(idFiliali)
004500090420
004600090402     ***************************************************************************
004700090402     **
004800120319     ** Campi.
004900090402     **
005000090402     ***************************************************************************
005100120321     D esito           S             10I 0
005200120321     D esitoProj       S              1A
005300090402
005400090402     ***************************************************************************
005500090402     **
005600120319     ** Parametri procedura principale.
005700090402     **
005800090402     ***************************************************************************
005900120319     D Tnsd41_Selezione...
006000090402     D                 PI
006100120319     D  priRqsOpCode...
006200120319     D                               10I 0 CONST
006300120319     D  priRpyOpCode...
006400120319     D                               10I 0
006500120319     D  priRpyIdMsg...
006600090402     D                               10I 0
006700120319     D  priRqsFormato...
006800120319     D                               10A   CONST
006900090403     D                                     OPTIONS(*NOPASS:*OMIT)
007000120319     D  priRqsData...
007100090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS:*OMIT)
007200090402     D                                     CONST
007300120319     D  priRqsDataSize...
007400120319     D                               10I 0 CONST
007500090403     D                                     OPTIONS(*NOPASS:*OMIT)
007600120319     D  priRpyFormato...
007700090402     D                               10A
007800090402     D                                     CONST
007900090402     D                                     OPTIONS(*NOPASS)
008000120319     D  priRpyData...
008100090402     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
008200120319     D  priRpyDataSize...
008300090402     D                               10I 0
008400090402     D                                     CONST
008500090402     D                                     OPTIONS(*NOPASS)
008600090402
008700090402     D*--------------------------------------------------
008800090402     D* Procedure name: NewSelect
008900090402     D* Purpose:        Nuova selezione.
009000090402     D* Returns:        Esito.
009100090402     D*--------------------------------------------------
009200090402     D NewSelect       PR            10I 0
009300090402
009400090402     D*--------------------------------------------------
009500090402     D* Procedure name: EndSelect
009600090402     D* Purpose:        Fine selezione.
009700090402     D* Returns:        Esito.
009800090402     D*--------------------------------------------------
009900090402     D EndSelect       PR            10I 0
010000090402
010100090402     D*--------------------------------------------------
010200090402     D* Procedure name: GetNextRow
010300090402     D* Purpose:        Legge la riga successiva.
010400090402     D* Returns:        Esito.
010500090402     D*--------------------------------------------------
010600090402     D GetNextRow      PR            10I 0
010700090402
010800090402      /FREE
010900090402
011000120321       IF priRqsOpCode = TNSD41_RQSOPCODE_SELEZIONE_GETNEXTROW;
011100090402
011200120319         priRpyIdMsg = GetNextRow();
011300090402
011400120321         IF priRpyFormato = tnsd4131o.formato;
011500120321           %SUBST(priRpyData : 1 : priRpyDataSize) = tnsd4131o;
011600090402         ENDIF;
011700090402
011800120319         IF priRpyIdMsg < ESITO_OK;
011900120319           priRpyOpCode = TNSD41_RPYOPCODE_ERRORE;
012000090402           RETURN;
012100090402         ENDIF;
012200090402
012300120321       ELSEIF priRqsOpCode = TNSD41_RQSOPCODE_SELEZIONE_NEW;
012400090402
012500090402         esito = NewSelect();
012600120321
012700090402         IF esito < ESITO_OK;
012800120319           priRpyOpCode = TNSD41_RPYOPCODE_ERRORE;
012900120319           priRpyIdMsg = esito;
013000090402           RETURN;
013100090402         ENDIF;
013200090402
013300120321       ELSEIF priRqsOpCode = TNSD41_RQSOPCODE_SELEZIONE_END;
013400090402
013500090402         esito = EndSelect();
013600090402         IF esito < ESITO_OK;
013700120319           priRpyOpCode = TNSD41_RPYOPCODE_ERRORE;
013800120319           priRpyIdMsg = esito;
013900090402           RETURN;
014000090402         ENDIF;
014100090402
014200120319       ELSEIF priRqsOpCode = TNSD41_RQSOPCODE_INIT;
014300090402
014400120319         IF priRqsFormato = 'KPJBA';
014500120319           kpjba = priRqsData;
014600090402         ENDIF;
014700090420
014800120319       ELSEIF priRqsOpCode = TNSD41_RQSOPCODE_FINALIZE;
014900090402
015000090402         *INLR = *ON;
015100090402
015200090402       ENDIF;
015300090402
015400120319       priRpyOpCode = TNSD41_RPYOPCODE_DONE;
015500090402
015600090402       RETURN;
015700090402
015800090402      /END-FREE
015900090402
016000090402     P*--------------------------------------------------
016100090402     P* Procedure name: NewSelect
016200090402     P* Purpose:        Nuova selezione.
016300090402     P* Returns:        Esito.
016400090402     P*--------------------------------------------------
016500090402     P NewSelect       B
016600090402     D NewSelect       PI            10I 0
016700090402
016800090403     D ragioneSociale...
016900090403     D                 S             50A
017000090402     D stm...
017100090402     D                 S          32740A   VARYING
017200090402     D stmSelect...
017300090402     D                 S          32740A   VARYING
017400090402     D stmFrom...
017500090402     D                 S          32740A   VARYING
017600090402     D stmWhere...
017700090402     D                 S          32740A   VARYING
017800090402     D stmOrderBy...
017900090402     D                 S          32740A   VARYING
018000090402
018100090402      /FREE
018200090402
018300120319       IF priRqsFormato = tnsd4130i.formato;
018400120319         tnsd4130i = %SUBST(priRqsData : 1 : priRqsDataSize);
018500090402       ENDIF;
018600120321
018700120321       IF priRpyFormato = tnsd4130o.formato;
018800120321         RESET tnsd4130o;
018900120321       ENDIF;
019000090402
019100120321
019200120321
019300120321       stmSelect = 'SELECT AZORG00F.ORGFIL, IFNULL(AZSOF00F.ID_RIGA, 0)';
019400120321
019500120321       stmFrom = ' FROM AZORG00F LEFT OUTER JOIN AZSOF00F +
019600120322                  ON AZORG00F.ORGFIL = AZSOF00F.ID_FILIALE';
019700120322
019800120322       IF tnsd4130i.valDat > *LOVAL;
019900120322         stmFrom += ' AND ''' + %CHAR(tnsd4130i.valDat) + ''' BETWEEN +
020000120322                     AZSOF00F.VALIDITA_DATA_INIZIO AND +
020100120322                     AZSOF00F.VALIDITA_DATA_FINE';
020200120322       ENDIF;
020300120321
020400120321       stm = stmSelect + stmFrom;
020500090402
020600120321       stmOrderBy = ' ORDER BY AZORG00F.ORGFIL +
020700120319                    , AZSOF00F.VALIDITA_DATA_FINE DESC';
020800090402
020900120322
021000120322       stmWhere = ' WHERE AZORG00F.ORGFVA = '' '' +
021100120322                    AND SUBSTR(AZORG00F.ORGDE3, 11, 3) IN +
021200120322                    (''COR'',''MES'',''LOG'')';
021300120319
021400120322       IF tnsd4130i.idFiliale <> *ZERO;
021500120322         IF stmWhere <> ' WHERE';
021600120322           stmWhere += ' AND';
021700120322         ENDIF;
021800120322         stmWhere += ' AZORG00F.ORGFIL = ' + %CHAR(tnsd4130i.idFiliale);
021900120322       ENDIF;
022000120322
022100120322       IF tnsd4130i.idArea <> *ZERO;
022200120322         IF stmWhere <> ' WHERE';
022300120322           stmWhere += ' AND';
022400120322         ENDIF;
022500120322         stmWhere += ' AZORG00F.ORGCAR = ' + %CHAR(tnsd4130i.idArea);
022600120322       ENDIF;
022700120322
022800120322       IF tnsd4130i.idRegione <> *ZERO;
022900120322         IF stmWhere <> ' WHERE';
023000120322           stmWhere += ' AND';
023100120322         ENDIF;
023200120322         stmWhere += ' AZORG00F.ORGCRE = ' + %CHAR(tnsd4130i.idRegione);
023300120322       ENDIF;
023400120319
023500120322       IF tnsd4130i.tpSocieta <> *BLANK;
023600120322         IF stmWhere <> ' WHERE';
023700120322           stmWhere += ' AND';
023800120322         ENDIF;
023900120322         stmWhere += ' AZSOF00F.TIPO_SOCIETA = '''
024000120322                   + tnsd4130i.tpSocieta + '''';
024100120322       ENDIF;
024200090402
024300120322       IF tnsd4130i.idSocieta <> *BLANK;
024400120322         IF stmWhere <> ' WHERE';
024500120322           stmWhere += ' AND';
024600120322         ENDIF;
024700120322         stmWhere += ' AZSOF00F.ID_SOCIETA = ''' + tnsd4130i.idSocieta + '''';
024800120322       ENDIF;
024900090402
025000120322       stm += stmWhere;
025100090402       stm += stmOrderBy;
025200090402       stm += ' FOR READ ONLY';
025300090402
025400090402       EXEC SQL
025500120319         PREPARE FILIALI FROM :stm
025600090402       ;
025700090402
025800090402       EXEC SQL
025900120319         DECLARE FILIALI NO SCROLL CURSOR FOR FILIALI
026000090402       ;
026100090402
026200090402       EXEC SQL
026300120319         OPEN FILIALI
026400090402       ;
026500120321
026600120321       IF priRpyFormato = tnsd4130o.formato;
026700120321         %SUBST(priRpyData : 1 : priRpyDataSize) = tnsd4130o;
026800120321       ENDIF;
026900090402
027000090402       IF sqlCode < *ZERO;
027100090402         DUMP(A);
027200090402         RETURN sqlCode;
027300090402       ENDIF;
027400090402
027500090402       RETURN ESITO_OK;
027600090402
027700090402      /END-FREE
027800090402     P NewSelect       E
027900090402
028000090402
028100090402     P*--------------------------------------------------
028200090402     P* Procedure name: EndSelect
028300090402     P* Purpose:        Fine selezione.
028400090402     P* Returns:        Esito.
028500090402     P*--------------------------------------------------
028600090402     P EndSelect       B
028700090402     D EndSelect       PI            10I 0
028800090402
028900090402      /FREE
029000090402
029100090402       EXEC SQL
029200120319         CLOSE FILIALI
029300090402       ;
029400090402
029500090402       RETURN ESITO_OK;
029600090402
029700090402      /END-FREE
029800090402     P EndSelect       E
029900090402
030000090402
030100090402     P*--------------------------------------------------
030200090402     P* Procedure name: GetNextRow
030300090402     P* Purpose:        Legge la riga successiva.
030400090402     P* Returns:        Esito.
030500090402     P*--------------------------------------------------
030600090402     P GetNextRow      B
030700090402     D GetNextRow      PI            10I 0
030800090402
030900120321     D retField        S             10I 0 STATIC
031000120321     D i               S             10I 0 STATIC
031100120321
031200090402      /FREE
031300120321
031400120321       IF priRqsFormato = tnsd4131i.formato;
031500120321         tnsd4131i = %SUBST(priRqsData : 1 : priRqsDataSize);
031600120321       ENDIF;
031700120321
031800120321       IF priRpyFormato = tnsd4131o.formato;
031900120321         RESET tnsd4131o;
032000120321       ENDIF;
032100120321
032200090402       EXEC SQL
032300090402         FETCH NEXT
032400120321           FROM FILIALI
032500120321           FOR :tnsd4131i.righePag ROWS
032600120321           INTO :filiali_ary
032700090402       ;
032800090402
032900090402       IF sqlCode < *ZERO;
033000090402         DUMP(A);
033100090402         RETURN sqlCode;
033200090402       ELSEIF sqlCode = 100;
033300090402         RETURN sqlCode;
033400090402       ENDIF;
033500090402
033600120321       EXEC SQL GET DIAGNOSTICS :tnsd4131o.nrRighe = ROW_COUNT;
033700120321       EXEC SQL GET DIAGNOSTICS :retField = DB2_LAST_ROW;
033800120321
033900120321       FOR i = 1 TO tnsd4131o.nrRighe;
034000120321         tnsd4131o.idRighe_ary(i) = filiali_ary(i).idRiga;
034100120321         tnsd4131o.idFiliali_ary(i) = filiali_ary(i).idFiliale;
034200120321       ENDFOR;
034300120321
034400120321       IF priRpyFormato = tnsd4131o.formato;
034500120321         %SUBST(priRpyData : 1 : priRpyDataSize) = tnsd4131o;
034600120321       ENDIF;
034700120321
034800120321       RETURN retField;
034900090402
035000090402      /END-FREE
035100090402     P GetNextRow      E
035200090402
