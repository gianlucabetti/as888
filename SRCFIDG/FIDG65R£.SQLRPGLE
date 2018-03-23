000100081008     ***************************************************************************
000200081008     **
000300150907     ** Note di debito in CE
000500081016     ** Quando vengo chiamato è già attiva una OVRDBF sul membro di TIVIN00R.
000600081016     ** Casomai in TIVLR00F.VLRHDL c'è il nome del membro.
004400081017     **
004500081008     ***************************************************************************
004600081016     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('QC2LE')
004700081015
004800081015     ***************************************************************************
004900081015     **
005000081015     ** Definizioni costanti.
005100081015     **
005200081015     ***************************************************************************
005300081015     D ESITO_ERROR...
005400081015     D                 C                   -1
005500081015     D ESITO_OK...
005600081015     D                 C                   0
005700081015     D ESITO_WARNING...
005800081015     D                 C                   1
006100081015
006200081015     ***************************************************************************
006300081015     **
006400081015     ** Dichiarazione procedure esterne.
006500081015     **
006600081015     ***************************************************************************
006700081015     D/COPY GAITRASRC/SRCPROTOPR,SYSTEM
006800081015
006900081015     ***************************************************************************
007000081015     **
007100081015     ** Dichiarazione strutture dati.
007200081015     **
007300081015     ***************************************************************************
007400081015     D tivlr00f      E DS                  QUALIFIED
007500081015     D  vlrPpt_DelimitatoreCampo...
007600081015     D                                1A   OVERLAY(vlrPpt:2)
007700081015     D  vlrPpt_DelimitatoreStringa...
007800081015     D                                1A   OVERLAY(vlrPpt:3)
007900081015     D  vlrPpt_VirgolaDecimale...
008000081015     D                                1A   OVERLAY(vlrPpt:4)
008100081015
008200081015     ***************************************************************************
008300081015     **
008400081015     ** Dichiarazione variabili.
008500081015     **
008600081015     ***************************************************************************
008700081015     D cmd             S            255A   VARYING
008800081016     D count           S             10I 0
008900081015     D exceptionMessageID...
009000081015     D                 S              7A   IMPORT('_EXCP_MSGID')
009100081015     D rpyEsito        S             10I 0
009200081015     D rpyOpCode       S             10A
009300081015     D rpyStatus       S             10I 0
009400081015     D rqsOpCode       S             10A
009500081015
009600081015     ***************************************************************************
009700081015     **
009800081015     ** Dichiarazione parametri.
009900081015     **
010000081015     ***************************************************************************
010100081015     C     *ENTRY        PLIST
010200081015     C                   PARM                    rqsOpCode
010300081015     C                   PARM                    rpyEsito
010400081015     C                   PARM                    rpyOpCode
010500081015     C                   PARM                    rpyStatus
010600081015     C                   PARM                    tivlr00f
010700081015
010800081015     ***************************************************************************
010900081015     **
011000081015     ** Main.
011100081015     **
011200081015     ***************************************************************************
011300081015      /FREE
011400081015
011500081015       *INLR = *ON;
011600081015
011700081015       CLEAR rpyEsito;
011800081015       CLEAR rpyOpCode;
011900081015       CLEAR rpyStatus;
012000081016
013800081016       // Creo una tabella temporanea con solo la colonna di TIVIN00R
013900081016       // che contiene i dati in formato CSV da tradurre.
014000081016
014100150907       EXEC SQL DROP TABLE QTEMP/ADDSTP_CSV;
014200081016
014300081222       // Il programma chiamante mi ha puntato sul membro giusto di TIVIN00R
014400081222       // con OVRDBF.
014500081222
014600150907       EXEC SQL CREATE TABLE QTEMP/ADDSTP_CSV
014700081028                AS ( SELECT VINDTA
014800081028                     FROM TIVIN00R
014900081028                     WHERE VINDTA <> ';;;;;;;'
015000081028                   )
015100081028                WITH DATA
015200081015       ;
015300081015
015400081015       IF sqlCode < 0;
015500081015         rpyEsito = ESITO_ERROR;
015600081015         rpyOpCode = 'SQLCODE';
015700081015         rpyStatus = sqlCode;
015800081015         DUMP(A);
015900081016         EXSR uscita;
016000081015       ENDIF;
016100081016
016200081016       // Creo una tabella temporanea con solo i dati in formato DB2 da tradurre.
016300081016       // Questa tabella deve avere una colonna per ogni colonna del file CSV.
016400081015
016500150907       EXEC SQL DROP TABLE QTEMP/ADDSTP_DB2;
016600081015
016700081015       EXEC SQL CREATE TABLE
016800150907                QTEMP/ADDSTP_DB2
016900150908                ( causale  CHAR (3)
017000150915                  NOT NULL
017300150908                , importo dec (19, 3)
017400150908                  NOT NULL
017700151022                , competenza char (8)
017800150908                  NOT NULL
018100150908                , capoconto char (6)
018201150908                , sottoconto char (8)
018203150908                , cdc char (8)
018205150915                , molti dec (3, 0)
018500081015                )
018600081015       ;
018700081015
018800081015       IF sqlCode < 0;
018900081015         rpyEsito = ESITO_ERROR;
019000081015         rpyOpCode = 'SQLCODE';
019100081015         rpyStatus = sqlCode;
019200081015         DUMP(A);
019300081016         EXSR uscita;
019400081015       ENDIF;
019500081016
019600081016       // Creo il file di log per il comando CPYFRMIMPF.
019700081016
019800150907       EXEC SQL DROP TABLE QTEMP/ADDSTP_ERR;
019900081016
020000150907       IF ExecuteCommand('CRTSRCPF FILE(QTEMP/ADDSTP_ERR) RCDLEN(2060) +
020100081016                          MBR(CPYFRMIMPF)'
020200081016                        ) <> 0;
020300081016         rpyEsito = ESITO_ERROR;
020400081016         rpyOpCode = exceptionMessageID;
020500081016         DUMP(A);
020600151221         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
020700081016         EXSR uscita;
020800081016       ENDIF;
020900081015
021000081016       // Preparo il comando di copia dal file CSV alla tabella DB2 intermedia.
021100081016
021200150907       cmd = 'CPYFRMIMPF FROMFILE(QTEMP/ADDSTP_CSV) TOFILE(QTEMP/ADDSTP_DB2) +
021300081017              MBROPT(*REPLACE) RCDDLM(*CRLF) RMVBLANK(*NONE) DATFMT(*DMY) +
021400150907              ERRRCDFILE(QTEMP/ADDSTP_ERR CPYFRMIMPF) ERRRCDOPT(*REPLACE)';
021500150907
021600081016       IF tivlr00f.vlrPpt_DelimitatoreStringa = *BLANK;
021700081015         cmd += ' STRDLM(*NONE)';
021800081015       ELSEIF tivlr00f.vlrPpt_DelimitatoreStringa = '"';
021900081015         cmd += ' STRDLM(*DBLQUOTE)';
022000081015       ELSE;
022100081016         cmd += ' STRDLM(''' + tivlr00f.vlrPpt_DelimitatoreStringa + ''')';
022200081015       ENDIF;
022300081015
022400081016       cmd += ' FLDDLM(''' + tivlr00f.vlrPpt_DelimitatoreCampo + ''')';
022500081015
022600081016       IF tivlr00f.vlrPpt_VirgolaDecimale = ',';
022700081016         cmd += ' DECPNT(*COMMA)';
022800081015       ENDIF;
022900081015
023000081015       IF ExecuteCommand( cmd ) <> 0;
023100081015         rpyEsito = ESITO_ERROR;
023200081015         rpyOpCode = exceptionMessageID;
023300081015         DUMP(A);
023400151221         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
023500081016         EXSR uscita;
023600081015       ENDIF;
023700081016
023800150907       // A questo punto la tabella QTEMP/ADDSTP_ERR deve essere vuota.
023900081016       // Le eventuali righe presenti sono le righe non copiate a causa di
024000150907       // errori che stamperò per lasciare una traccia
024200081016
024300081016       EXEC SQL SELECT COUNT(*)
024400081016                INTO :count
024500150907                FROM QTEMP/ADDSTP_ERR
024600081016       ;
024700081016
024800081016       IF count > 0;
024900151221         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
025000150907         ExecuteCommand('CPYF FROMFILE(QTEMP/ADDSTP_ERR) TOFILE(*PRINT)');
025100081028         rpyEsito = ESITO_ERROR;
025200081028         EXSR uscita;
025300081016       ENDIF;
029400081015
029500081016       EXSR uscita;
029600081016
029700081015       //***********************************************************************
029800081015       //
029900081015       //
030000081015       //
030100081015       //***********************************************************************
030200081015
030300081016       BEGSR uscita;
030800081016
030900081016         IF rpyOpCode = *BLANK AND rpyEsito > ESITO_ERROR;
031000081016           rpyOpCode = 'DONE';
031100081030         ELSEIF rpyOpCode = 'SQLCODE' AND rpyEsito < ESITO_OK;
031101150908         EXEC SQL DROP TABLE QTEMP/ADDSTP_CSV;
031102150908         EXEC SQL DROP TABLE QTEMP/ADDSTP_DB2;
031103150908         EXEC SQL DROP TABLE QTEMP/ADDSTP_ERR;
031200151221           ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
031300081030         ENDIF;
031400081016
031500081016         RETURN;
031600081016
031700081015       ENDSR;
031800081015
031900081015      /END-FREE
