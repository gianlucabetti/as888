000100090402     ***************************************************************************
000200090402     **
000300090402     ** Anagrafico società.
000400090402     ** Selezione società.
000500090402     **
000600090402     ***************************************************************************
000700090420     H DFTACTGRP(*NO) ACTGRP('AZSOC') BNDDIR('PJXBND')
000800090402
000900090402     ***************************************************************************
001000090402     **
001100090402     ** Definizione costanti.
001200090402     **
001300090402     ***************************************************************************
001400090402     D/COPY GAITRASRC/SRCCONST,TNSD4000R
001500090402     D ESITO_ERROR...
001600090402     D                 C                   -1
001700090402     D ESITO_OK...
001800090402     D                 C                   0
001900090402     D ESITO_WARNING...
002000090402     D                 C                   1
002100090402
002200090402     ***************************************************************************
002300090402     **
002400090402     ** Dichiarazione prototipi.
002500090402     **
002600090402     ***************************************************************************
002700090403     D/COPY GAITRASRC/SRCPROTOPR,QLGCNVCS
002800090403     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
002900090420     D/COPY GAITRASRC/SRCPROTOPR,XSOC
003000090402
003100090402     ***************************************************************************
003200090402     **
003300090402     ** Dichiarazione strutture dati.
003400090402     **
003500090402     ***************************************************************************
003600090403     D/COPY QSYSINC/QRPGLESRC,QLG
003700090403     D/COPY QSYSINC/QRPGLESRC,QUSEC
003800090403     D kpjba...
003900090402     D               E DS
004000090402     D                                     QUALIFIED
004100090402     D                                     INZ
004200090402     D  knmus...
004300090402     D               E                     INZ(*USER)
004400090402     D tnsd4030i...
004500090402     D               E DS
004600090402     D                                     QUALIFIED
004700090402     D                                     INZ
004800090402     D tnsd4030o...
004900090402     D               E DS
005000090402     D                                     QUALIFIED
005100090402     D                                     INZ
005200090420     D xSoc001Ds...
005300090420     D               E DS
005400090420     D                                     QUALIFIED
005500090420     D                                     INZ
005600090420
005700090402     ***************************************************************************
005800090402     **
005900090402     ** Dichiarazione variabili.
006000090402     **
006100090402     ***************************************************************************
006200090402     D esito...
006300090402     D                 S             10I 0
006400090420     D esitoProj...
006500090420     D                 S              1A
006600090402
006700090402     ***************************************************************************
006800090402     **
006900090402     ** Dichiarazione interfaccia procedura principale.
007000090402     **
007100090402     ***************************************************************************
007200090402     D Societa_Selezione...
007300090402     D                 PI
007400090402     D  piRqsOpCode...
007500090402     D                               10A
007600090402     D                                     CONST
007700090402     D  piRpyOpCode...
007800090402     D                               10A
007900090402     D  piRpyIdMsg...
008000090402     D                               10I 0
008100090402     D  piRqsFormato...
008200090402     D                               10A
008300090402     D                                     CONST
008400090403     D                                     OPTIONS(*NOPASS:*OMIT)
008500090402     D  piRqsData...
008600090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS:*OMIT)
008700090402     D                                     CONST
008800090402     D  piRqsDataSize...
008900090402     D                               10I 0
009000090402     D                                     CONST
009100090403     D                                     OPTIONS(*NOPASS:*OMIT)
009200090402     D  piRpyFormato...
009300090402     D                               10A
009400090402     D                                     CONST
009500090402     D                                     OPTIONS(*NOPASS)
009600090402     D  piRpyData...
009700090402     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
009800090402     D  piRpyDataSize...
009900090402     D                               10I 0
010000090402     D                                     CONST
010100090402     D                                     OPTIONS(*NOPASS)
010200090402
010300090402     D*--------------------------------------------------
010400090402     D* Procedure name: NewSelect
010500090402     D* Purpose:        Nuova selezione.
010600090402     D* Returns:        Esito.
010700090402     D*--------------------------------------------------
010800090402     D NewSelect       PR            10I 0
010900090402
011000090402     D*--------------------------------------------------
011100090402     D* Procedure name: EndSelect
011200090402     D* Purpose:        Fine selezione.
011300090402     D* Returns:        Esito.
011400090402     D*--------------------------------------------------
011500090402     D EndSelect       PR            10I 0
011600090402
011700090402     D*--------------------------------------------------
011800090402     D* Procedure name: GetNextRow
011900090402     D* Purpose:        Legge la riga successiva.
012000090402     D* Returns:        Esito.
012100090402     D*--------------------------------------------------
012200090402     D GetNextRow      PR            10I 0
012300090402
012400090402      /FREE
012500090402
012600090403       IF piRqsOpCode = SOCIETA_OPCODE_SELEZIONE_GETNEXTROW;
012700090402
012800090402         IF piRpyFormato = 'TNSD4030O';
012900090402           RESET tnsd4030o;
013000090402         ENDIF;
013100090402
013200090402         piRpyIdMsg = GetNextRow();
013300090402
013400090402         IF piRpyFormato = 'TNSD4030O';
013500090402           %SUBST(piRpyData : 1 : piRpyDataSize) = tnsd4030o;
013600090402         ENDIF;
013700090402
013800090402         IF piRpyIdMsg < ESITO_OK;
013900090402           piRpyOpCode = SOCIETA_OPCODE_ERRORE;
014000090402           RETURN;
014100090402         ENDIF;
014200090402
014300090403       ELSEIF piRqsOpCode = SOCIETA_OPCODE_SELEZIONE_NEW;
014400090402
014500090402         esito = NewSelect();
014600090402         IF esito < ESITO_OK;
014700090402           piRpyOpCode = SOCIETA_OPCODE_ERRORE;
014800090402           piRpyIdMsg = esito;
014900090402           RETURN;
015000090402         ENDIF;
015100090402
015200090403       ELSEIF piRqsOpCode = SOCIETA_OPCODE_SELEZIONE_END;
015300090402
015400090402         esito = EndSelect();
015500090402         IF esito < ESITO_OK;
015600090402           piRpyOpCode = SOCIETA_OPCODE_ERRORE;
015700090402           piRpyIdMsg = esito;
015800090402           RETURN;
015900090402         ENDIF;
016000090402
016100090402       ELSEIF piRqsOpCode = SOCIETA_OPCODE_INIT;
016200090402
016300090402         IF piRqsFormato = 'KPJBA';
016400090402           kpjba = piRqsData;
016500090402         ENDIF;
016600090402
016700090420         xSoc001Ds.xscSoc = kpjba.kpjbu;
016800090420
016900090420         Proj_Societa( 'SOC001'
017000090420                     : xSoc001Ds.xscSoc
017100090420                     : *ZERO
017200090420                     : *BLANK
017300090420                     : esitoProj
017400090420                     : xSoc001Ds
017500090420                     : kpjba
017600090420                     );
017700090420
017800090420         IF esitoProj = *ON;
017900090420           RETURN;
018000090420         ENDIF;
018100090420
018200090402       ELSEIF piRqsOpCode = SOCIETA_OPCODE_FINALIZE;
018300090402
018400090402         *INLR = *ON;
018500090402
018600090402       ENDIF;
018700090402
018800090402       piRpyOpCode = SOCIETA_OPCODE_DONE;
018900090402
019000090402       RETURN;
019100090402
019200090402      /END-FREE
019300090402
019400090402     P*--------------------------------------------------
019500090402     P* Procedure name: NewSelect
019600090402     P* Purpose:        Nuova selezione.
019700090402     P* Returns:        Esito.
019800090402     P*--------------------------------------------------
019900090402     P NewSelect       B
020000090402     D NewSelect       PI            10I 0
020100090402
020200090403     D ragioneSociale...
020300090403     D                 S             50A
020400090402     D stm...
020500090402     D                 S          32740A   VARYING
020600090402     D stmSelect...
020700090402     D                 S          32740A   VARYING
020800090402     D stmFrom...
020900090402     D                 S          32740A   VARYING
021000090402     D stmWhere...
021100090402     D                 S          32740A   VARYING
021200090402     D stmOrderBy...
021300090402     D                 S          32740A   VARYING
021400090402
021500090402      /FREE
021600090402
021700090402       IF piRqsFormato = 'TNSD4030I';
021800090402         tnsd4030i = %SUBST(piRqsData : 1 : piRqsDataSize);
021900090402       ENDIF;
022000090402
022100090402       stmSelect = 'SELECT AZSOC00F.ID_RIGA';
022200090402       stmFrom = ' FROM AZSOC00F';
022300090402       stm = stmSelect + stmFrom;
022400090402
022500090430       stmOrderBy = ' ORDER BY AZSOC00F.ID_SOCIETA +
022600090403                    , AZSOC00F.VALIDITA_DATA_FINE DESC';
022700090402
022800090403       IF tnsd4030i.idSocieta <> *BLANK OR tnsd4030i.codFiscale <> *BLANK
022900090402       OR tnsd4030i.valDat > *LOVAL OR tnsd4030i.attDat > *LOVAL
023000090402       OR (%LEN(tnsd4030i.ragSociale) > *ZERO AND
023100090402           tnsd4030i.ragSociale <> *BLANK);
023200090402
023300090402         stmWhere = ' WHERE';
023400090402
023500090403         IF tnsd4030i.idSocieta <> *BLANK;
023600090403           stmWhere += ' AZSOC00F.ID_SOCIETA = ''' + tnsd4030i.idSocieta + '''';
023700090402         ENDIF;
023800090403
023900090403         IF tnsd4030i.codFiscale <> *BLANK;
024000090403           IF stmWhere <> ' WHERE';
024100090403             stmWhere += ' AND';
024200090403           ENDIF;
024300090403           stmWhere += ' AZSOC00F.CODICE_FISCALE = '''
024400090403                     + tnsd4030i.codFiscale + '''';
024500090403         ENDIF;
024600090402
024700090402         IF tnsd4030i.valDat > *LOVAL;
024800090402           IF stmWhere <> ' WHERE';
024900090402             stmWhere += ' AND';
025000090402           ENDIF;
025100090402           stmWhere += ' ''' + %CHAR(tnsd4030i.valDat) + ''' BETWEEN +
025200090402                        AZSOC00F.VALIDITA_DATA_INIZIO AND +
025300090402                        AZSOC00F.VALIDITA_DATA_FINE';
025400090402         ENDIF;
025500090402
025600090402         IF tnsd4030i.attDat > *LOVAL;
025700090402           IF stmWhere <> ' WHERE';
025800090402             stmWhere += ' AND';
025900090402           ENDIF;
026000090402           stmWhere += ' ''' + %CHAR(tnsd4030i.attDat) + ''' BETWEEN +
026100090402                        AZSOC00F.ATTIVITA_DATA_INIZIO AND +
026200090402                        IFNULL(AZSOC00F.ATTIVITA_DATA_FINE , ''9999-12-31'')';
026300090402         ENDIF;
026400090402
026500090402         IF %LEN(tnsd4030i.ragSociale) > *ZERO
026600090402         AND tnsd4030i.ragSociale <> *BLANK;
026700090402
026800090402           IF stmWhere <> ' WHERE';
026900090402             stmWhere += ' AND';
027000090402           ENDIF;
027100090403           CLEAR QUSEC;
027200090403           QUSBPRV = %SIZE(QUSEC);
027300090403           CLEAR QLGIDRCB00;
027400090403           QLGTOR02 = 1;
027500090403           QLGIDOID00 = 1144;
027600090403           QLGCR00 = 0;
027700090403           QLGERVED04 = *ALLX'00';
027800090403           ConvertCase( QLGIDRCB00
027900090403                      : tnsd4030i.ragSociale
028000090403                      : ragioneSociale
028100090403                      : %LEN(tnsd4030i.ragSociale)
028200090403                      : QUSEC
028300090403                      );
028400090403           tnsd4030i.ragSociale = %TRIMR(ragioneSociale);
028500090403           stmWhere += ' UCASE(AZSOC00F.RAGIONE_SOCIALE) LIKE ''%'
028600090402                     + tnsd4030i.ragSociale + '%''';
028700090402         ENDIF;
028800090402
028900090402       stm += stmWhere;
029000090402
029100090402       ENDIF;
029200090402
029300090402       stm += stmOrderBy;
029400090402       stm += ' FOR READ ONLY';
029500090402
029600090402       EXEC SQL
029700090402         PREPARE SOCIETA FROM :stm
029800090402       ;
029900090402
030000090402       EXEC SQL
030100090402         DECLARE SOCIETA NO SCROLL CURSOR FOR SOCIETA
030200090402       ;
030300090402
030400090402       EXEC SQL
030500090402         OPEN SOCIETA
030600090402       ;
030700090402
030800090402       IF sqlCode < *ZERO;
030900090402         DUMP(A);
031000090402         RETURN sqlCode;
031100090402       ENDIF;
031200090402
031300090402       RETURN ESITO_OK;
031400090402
031500090402      /END-FREE
031600090402     P NewSelect       E
031700090402
031800090402
031900090402     P*--------------------------------------------------
032000090402     P* Procedure name: EndSelect
032100090402     P* Purpose:        Fine selezione.
032200090402     P* Returns:        Esito.
032300090402     P*--------------------------------------------------
032400090402     P EndSelect       B
032500090402     D EndSelect       PI            10I 0
032600090402
032700090402      /FREE
032800090402
032900090402       EXEC SQL
033000090402         CLOSE SOCIETA
033100090402       ;
033200090402
033300090402       RETURN ESITO_OK;
033400090402
033500090402      /END-FREE
033600090402     P EndSelect       E
033700090402
033800090402
033900090402     P*--------------------------------------------------
034000090402     P* Procedure name: GetNextRow
034100090402     P* Purpose:        Legge la riga successiva.
034200090402     P* Returns:        Esito.
034300090402     P*--------------------------------------------------
034400090402     P GetNextRow      B
034500090402     D GetNextRow      PI            10I 0
034600090402
034700090402      /FREE
034800090402
034900090402       RESET tnsd4030o;
035000090402
035100090402       EXEC SQL
035200090402         FETCH NEXT
035300090402           FROM SOCIETA
035400090402           INTO :tnsd4030o.idRiga
035500090402       ;
035600090402
035700090402       IF sqlCode < *ZERO;
035800090402         DUMP(A);
035900090402         RETURN sqlCode;
036000090402       ELSEIF sqlCode = 100;
036100090402         RETURN sqlCode;
036200090402       ENDIF;
036300090402
036400090402       RETURN ESITO_OK;
036500090402
036600090402      /END-FREE
036700090402     P GetNextRow      E
036800090402
