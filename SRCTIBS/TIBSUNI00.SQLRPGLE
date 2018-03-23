000100080515     ***************************************************************************
000200080515     **
000300080515     ** Modulo di lettura veloce dell'anagrafico unità ANUNI00F.
000400080515     **
000500080515     ** Creare il programma di servizio col comando
000600080515     ** CRTSRVPGM SRVPGM(GAITRAFIL/TIBSUNI00) SRCFILE(GAITRASRC/SRCSRVPGM)
000700141212     ** TEXT('Organigramma.') ACTGRP(*CALLER)
000800080515     **
000900080515     ** Può essere creata anche la versione programma col comando
001000141212     ** CRTPGM PGM(GAITRAFIL/TIBSUNI00) ACTGRP(*CALLER)
001100080515     **
001200080515     ***************************************************************************
001300080515
001400080515     ***************************************************************************
001500080515     **
001600080515     ** Definizione prototipi procedure.
001700080515     **
001800080515     ***************************************************************************
001900080515     D/COPY GAITRASRC/SRCPROTOPR,TIBSUNI00
002000080515     D/COPY GAITRASRC/SRCPROTOPR,CEETREC
002100080515
002200080515     ***************************************************************************
002300080515     **
002400080515     ** Definizione interfaccia procedura.
002500080515     **
002600080515     ***************************************************************************
002700080515     D Unita...
002800080515     D                 PI
002900080515     D  rqsOpCode                    10A   CONST
003000080515     D  rpyEsito                     10I 0
003100080515     D  rqsData                      13A   OPTIONS(*NOPASS:*VARSIZE)
003200080515     D  rqsDataLen                   10I 0 CONST OPTIONS(*NOPASS)
003300080515     D  rpyData                      31A   OPTIONS(*NOPASS:*VARSIZE)
003400080515     D  rpyDataLen                   10I 0 CONST OPTIONS(*NOPASS)
003500080515
003600080515     ***************************************************************************
003700080515     **
003800080515     ** Definizione costanti.
003900080515     **
004000080515     ***************************************************************************
004100080515     D UNITA_ESITO_OK...
004200080515     D                 C                   0
004300080515     D UNITA_ESITO_ERRORE_GENERICO...
004400080515     D                 C                   1
004500080515     D UNITA_ESITO_ERRORE_UNITA_CODICENONVALIDO...
004600080515     D                 C                   2
004700080515     D UNITA_ESITO_ERRORE_UNITA_NONESISTE...
004800080515     D                 C                   3
004900080515     D UNITA_ESITO_ERRORE_OPCODE_SCONOSCIUTO...
005000080515     D                 C                   4
005100080515     D UNITA_OPCODE_GETANAGRAFICAUNITA...
005200080515     D                 C                   'GETANUNI00'
005300080515     D UNITA_OPCODE_GETDESCRIZIONEUNITA...
005400080515     D                 C                   'GETDESBREV'
005500080515     D UNITA_OPCODE_INIT...
005600080515     D                 C                   'INIT'
005700080515     D UNITA_OPCODE_ISINIT...
005800080515     D                 C                   'ISINIT'
005900080515     D UNITA_OPCODE_FINALIZE...
006000080515     D                 C                   'FINALIZE'
006100080515
006200080515     ***************************************************************************
006300080515     **
006400080515     ** Definizione strutture dati.
006500080515     **
006600080515     ***************************************************************************
006700080515     D anuni00f      E DS                  QUALIFIED
006800080515     D                                     DIM(32766)
006900080515     D                                     BASED(anuni00fPtr)
007000080515     D rpyAnuni      E DS                  QUALIFIED
007100080515     D                                     EXTNAME(anuni00f)
007200080515     D tibsuni01     E DS                  QUALIFIED
007300080515     D tibsuni02     E DS                  QUALIFIED
007400080515
007500080515     ***************************************************************************
007600080515     **
007700080515     ** Definizione variabili globali.
007800080515     **
007900080515     ***************************************************************************
008000080515     D guida           S             11A   DIM(32767) BASED(guidaPtr) ASCEND
008100080515     D elem            S             10I 0
008200080515     D i               S             10I 0
008300080515     D initDone        S               N   INZ(*OFF)
008400080515     D societa         S                   LIKE(anuni00f.uniSocieta)
008500080515     D dataValiditaSocieta...
008600080515     D                 S               D
008700080515
008800080515      /FREE
008900080515
009000080515       CLEAR rpyEsito;
009100080515
009200080515       IF rqsOpCode = UNITA_OPCODE_GETDESCRIZIONEUNITA;
009300080515         tibsuni01 = %SUBST(rqsData : 1 : rqsDataLen);
009400080515         rpyEsito = Unita_getDescrizioneUnita( tibsuni01.societa
009500080515                                             : tibsuni01.unita
009600080515                                             : rpyAnuni.uniDesBrev
009700080515                                             );
009800080515         %SUBST(rpyData : 1 : rpyDataLen) = rpyAnuni.uniDesBrev;
009900080515       ELSEIF rqsOpCode = UNITA_OPCODE_GETANAGRAFICAUNITA;
010000080515         tibsuni01 = %SUBST(rqsData : 1 : rqsDataLen);
010100080515         rpyEsito = Unita_getAnagraficaUnita( tibsuni01.societa
010200080515                                            : tibsuni01.unita
010300080515                                            : rpyAnuni
010400080515                                            );
010500080515         %SUBST(rpyData : 1 : rpyDataLen) = rpyAnuni;
010600080515       ELSEIF rqsOpCode = UNITA_OPCODE_ISINIT;
010700080515         %SUBST(rpyData : 1 : rpyDataLen) = Unita_isInit();
010800080515       ELSEIF rqsOpCode = UNITA_OPCODE_INIT;
010900080515         tibsuni02 = %SUBST(rqsData : 1 : rqsDataLen);
011000080515         rpyEsito = Unita_init(tibsuni02.societa : tibsuni02.dtValSoc);
011100080515       ELSEIF rqsOpCode = UNITA_OPCODE_FINALIZE;
011200080515         *INLR = *ON;
011300080515         Unita_finalize();
011400080515       ELSE;
011500080515         rpyEsito = UNITA_ESITO_ERRORE_OPCODE_SCONOSCIUTO;
011600080515       ENDIF;
011700080515
011800080515       RETURN;
011900080515
012000080515      /END-FREE
012100080515
012200080515     P*--------------------------------------------------
012300080515     P* Procedure name: Unita_init
012400080515     P* Purpose:        Inizializza il modulo.
012500080515     P* Returns:        Esito.
012600080515     P*--------------------------------------------------
012700080515     P Unita_init...
012800080515     P                 B                   EXPORT
012900080515     D Unita_init...
013000080515     D                 PI            10I 0
013100080515     D  piSocieta                          LIKE(anuni00f.uniSocieta)
013200080515     D                                     CONST
013300080515     D                                     OPTIONS(*NOPASS:*OMIT)
013400080515     D  piDataValiditaSocieta...
013500080515     D                                 D   CONST
013600080515     D                                     OPTIONS(*NOPASS:*OMIT)
013700080515
013800080515     D esito           S             10I 0
013900080515     D stm             S            255A   VARYING
014000080515     D stmSelect       S            255A   VARYING
014100080515     D stmFrom         S            255A   VARYING
014200080515     D stmWhere        S            255A   VARYING
014300080515     D stmJoin         S            255A   VARYING
014400080515     D stmOrderBy      S            255A   VARYING
014500080515
014600080515      /FREE
014700080515
014800080515       // Modulo già inizializzato.
014900080515
015000080515       IF Unita_isInit();
015100080625         RETURN UNITA_ESITO_OK;
015200080515       ENDIF;
015300080515
015400080515       // Compongo l'istruzione SQL secondo i parametri ricevuti.
015500080515
015600080515       stmSelect = 'SELECT COUNT(*)';
015700080515       stmFrom = ' FROM ANUNI00F';
015800080515
015900080515       IF %PARMS() > 0 AND %ADDR(piSocieta) <> *NULL AND piSocieta <> *BLANK;
016000080515         societa = piSocieta;
016100080515         IF %LEN(stmWhere) = 0;
016200080515           stmWhere = ' WHERE';
016300080515         ELSE;
016400080515           stmWhere += ' AND';
016500080515         ENDIF;
016600080515         stmWhere += ' ANUNI00F.UNISOCIETA = ''' + piSocieta + '''';
016700080515       ENDIF;
016800080515
016900080515       IF %PARMS() > 1 AND %ADDR(piDataValiditaSocieta) <> *NULL
017000080515       AND piDataValiditaSocieta <> *LOVAL;
017100080515         dataValiditasocieta = piDataValiditaSocieta;
017200080515         IF %LEN(stmWhere) = 0;
017300080515           stmWhere = ' WHERE';
017400080515         ELSE;
017500080515           stmWhere += ' AND';
017600080515         ENDIF;
017700080515         stmWhere += ' ANSIF00F.SIFDTFIVL = ''0001-01-01'' +
017800080515         OR ANSIF00F.SIFDTFIVL > ''' + %CHAR(piDataValiditaSocieta) + '''';
017900080515         stmJoin = ' JOIN ANSIF00F +
018000080515                    ON ANUNI00F.UNISOCIETA = ANSIF00F.SIFSOCIETA';
018100080515       ENDIF;
018200080515
018300080515       // Conto quante filiali ci sono nell'organigramma.
018400080515
018500080515       CLEAR elem;
018600080515
018700080515       stm = stmSelect + stmFrom + stmJoin + stmWhere;
018800080515
018900080515       EXEC SQL PREPARE CONTEGGIO_UNITA FROM :stm;
019000080515       EXEC SQL DECLARE CONTEGGIO_UNITA CURSOR FOR CONTEGGIO_UNITA;
019100080515
019200080515       IF sqlCode < 0;
019300080515         DUMP(A);
019400080515         RETURN sqlCode;
019500080515       ENDIF;
019600080515
019700080515       EXEC SQL OPEN CONTEGGIO_UNITA;
019800080515
019900080515       IF sqlCode < 0;
020000080515         DUMP(A);
020100080515         RETURN sqlCode;
020200080515       ENDIF;
020300080515
020400080515       EXEC SQL FETCH NEXT FROM CONTEGGIO_UNITA INTO :elem;
020500080515
020600080515       IF sqlCode < 0;
020700080515         DUMP(A);
020800080515         esito = sqlCode;
020900080515       ENDIF;
021000080515
021100080515       EXEC SQL CLOSE CONTEGGIO_UNITA;
021200080515
021300080515       IF esito < 0;
021400080515         RETURN esito;
021500080515       ENDIF;
021600080515
021700080515       // Alloco la memoria necessaria per memorizzare le unità selezionate.
021800080515
021900080515       anuni00fPtr = %ALLOC(elem * %SIZE(anuni00f));
022000080515       guidaPtr = %ALLOC(elem * %SIZE(guida));
022100080515
022200080515       // Memorizzo le unità in una schiera.
022300080515
022400080515       stmSelect = 'SELECT ANUNI00F.*';
022500080515       stmOrderBy = ' ORDER BY ANUNI00F.UNISOCIETA, ANUNI00F.UNIUNITA';
022600080515       stm = stmSelect + stmFrom + stmJoin + stmWhere + stmOrderBy;
022700080515
022800080515       EXEC SQL PREPARE UNITA FROM :stm;
022900080515       EXEC SQL DECLARE UNITA CURSOR FOR UNITA;
023000080515
023100080515       IF sqlCode < 0;
023200080515         DUMP(A);
023300080515         RETURN sqlCode;
023400080515       ENDIF;
023500080515
023600080515       EXEC SQL OPEN UNITA;
023700080515
023800080515       IF sqlCode < 0;
023900080515         DUMP(A);
024000080515         RETURN sqlCode;
024100080515       ENDIF;
024200080515
024300080515       EXEC SQL FETCH NEXT
024400080515                FROM UNITA
024500080515                FOR :elem ROWS
024600080515                INTO :anuni00f
024700080515       ;
024800080515
024900080515       IF sqlCode < 0;
025000080515         DUMP(A);
025100080515         esito = sqlCode;
025200080515       ENDIF;
025300080515
025400080515       EXEC SQL CLOSE UNITA;
025500080515
025600080515       IF esito < 0;
025700080515         RETURN esito;
025800080515       ENDIF;
025900080515
026000080515       // Carico la schiera dei codici unità che mi farà da guida per accedere alla
026100080515       // schiera dell'anagrafico unità.
026200080515
026300080515       FOR i = 1 TO elem;
026400080515         guida(i) = anuni00f(i).uniSocieta + anuni00f(i).uniUnita;
026500080515       ENDFOR;
026600080515
026700080515       CLEAR i;
026800080515       initDone = *ON;
026900080515
027000080515       RETURN esito;
027100080515
027200080515      /END-FREE
027300080515     P Unita_init...
027400080515     P                 E
027500080515
027600080515     P*--------------------------------------------------
027700080515     P* Procedure name: Unita_getAnagraficaUnita
027800080515     P* Purpose:        Restituisce l'anagrafica di una unita.
027900080515     P* Returns:        Esito
028000080515     P* Parameter:      piSocieta => Societa.
028100080515     P* Parameter:      piUnita => Unita.
028200080515     P* Parameter:      piAnuni00f => Record ANUNI00F.
028300080515     P*--------------------------------------------------
028400080515     P Unita_getAnagraficaUnita...
028500080515     P                 B                   EXPORT
028600080515     D Unita_getAnagraficaUnita...
028700080515     D                 PI            10I 0
028800080515     D  piSocieta                          LIKE(anuni00f.uniSocieta)
028900080515     D                                     CONST
029000080515     D  piUnita                            LIKE(anuni00f.uniUnita)
029100080515     D                                     CONST
029200080515     D  piAnuni00f                         LIKEDS(anuni00f)
029300080515
029400080515     D societaUnita    S                   LIKE(guida)
029500080515
029600080515      /FREE
029700080515
029800080515       // Pulisco i parametri di output.
029900080515
030000080515       CLEAR piAnuni00f;
030100080515
030200080515       // Non è stata eseguita l'inizializzazione del modulo.
030300080515
030400080515       IF NOT Unita_isInit();
030500080515         Unita_init();
030600080515       ENDIF;
030700080515
030800080515       // Il codice società e unita sono obbligatori.
030900080515
031000080515       IF piSocieta = *BLANK OR piUnita = *BLANK;
031100080515         RETURN UNITA_ESITO_ERRORE_UNITA_CODICENONVALIDO;
031200080515       ENDIF;
031300080515
031400080515       societaUnita = piSocieta + piUnita;
031500080515
031600080515       // Il codice società e unita non esistono.
031700080515
031800080515       IF societaUnita < guida(1) OR societaUnita > guida(elem);
031900080515         RETURN UNITA_ESITO_ERRORE_UNITA_NONESISTE;
032000080515       ENDIF;
032100080515
032200080515       IF i > 0 AND societaUnita = guida(i);
032300080515         piAnuni00f = anuni00f(i);
032400080515         RETURN UNITA_ESITO_OK;
032500080515       ELSEIF i > 0 AND societaUnita > guida(i);
032600080515         i = %LOOKUP(societaUnita : guida : i + 1 : elem - i);
032700080515       ELSE;
032800080515         i = %LOOKUP(societaUnita : guida : 1 : elem);
032900080515       ENDIF;
033000080515
033100080515       // La unita non esiste.
033200080515
033300080515       IF i = 0;
033400080515         RETURN UNITA_ESITO_ERRORE_UNITA_NONESISTE;
033500080515       ENDIF;
033600080515
033700080515       // Restituisco tutta l'anagrafica.
033800080515
033900080515       piAnuni00f = anuni00f(i);
034000080515
034100080515       RETURN UNITA_ESITO_OK;
034200080515
034300080515      /END-FREE
034400080515     P Unita_getAnagraficaUnita...
034500080515     P                 E
034600080515
034700080515     P*--------------------------------------------------
034800080515     P* Procedure name: Unita_getDescrizioneUnita
034900080515     P* Purpose:        Restituisce la descrizione della unita.
035000080515     P* Returns:        Esito.
035100080515     P* Parameter:      piUnita => Unita.
035200080515     P* Parameter:      piDescrizioneUnita => Descrizione unita.
035300080515     P*--------------------------------------------------
035400080515     P Unita_getDescrizioneUnita...
035500080515     P                 B                   EXPORT
035600080515     D Unita_getDescrizioneUnita...
035700080515     D                 PI            10I 0
035800080515     D  piSocieta                          LIKE(anuni00f.uniSocieta)
035900080515     D                                     CONST
036000080515     D  piUnita                            LIKE(anuni00f.uniUnita)
036100080515     D                                     CONST
036200080515     D  piDescrizioneUnita...
036300080515     D                                     LIKE(anuni00f.uniDesBrev)
036400080515
036500080515     D societaUnita    S                   LIKE(guida)
036600080515
036700080515      /FREE
036800080515
036900080515       // Pulisco i parametri di output.
037000080515
037100080515       CLEAR piDescrizioneUnita;
037200080515
037300080515       // Non è stata eseguita l'inizializzazione del modulo.
037400080515
037500080515       IF NOT Unita_isInit();
037600080515         Unita_init();
037700080515       ENDIF;
037800080515
037900080515       // Il codice società e unita sono obbligatori.
038000080515
038100080515       IF piSocieta = *BLANK OR piUnita = *BLANK;
038200080515         RETURN UNITA_ESITO_ERRORE_UNITA_CODICENONVALIDO;
038300080515       ENDIF;
038400080515
038500080515       societaUnita = piSocieta + piUnita;
038600080515
038700080515       // Il codice società e unita non esistono.
038800080515
038900080515       IF societaUnita < guida(1) OR societaUnita > guida(elem);
039000080515         RETURN UNITA_ESITO_ERRORE_UNITA_NONESISTE;
039100080515       ENDIF;
039200080515
039300080515       IF i > 0 AND societaUnita = guida(i);
039400080515         piDescrizioneUnita = anuni00f(i).uniDesBrev;
039500080515         RETURN UNITA_ESITO_OK;
039600080515       ELSEIF i > 0 AND societaUnita > guida(i);
039700080515         i = %LOOKUP(societaUnita : guida : i + 1 : elem - i);
039800080515       ELSE;
039900080515         i = %LOOKUP(societaUnita : guida : 1 : elem);
040000080515       ENDIF;
040100080515
040200080515       IF i = 0;
040300080515         RETURN UNITA_ESITO_ERRORE_UNITA_NONESISTE;
040400080515       ENDIF;
040500080515
040600080515       // Restituisco la descrizione dell'unità.
040700080515
040800080515       piDescrizioneUnita = anuni00f(i).uniDesBrev;
040900080515
041000080515       RETURN UNITA_ESITO_OK;
041100080515
041200080515      /END-FREE
041300080515     P Unita_getDescrizioneUnita...
041400080515     P                 E
041500080515
041600080515     P*--------------------------------------------------
041700080515     P* Procedure name: Unita_finalize
041800080515     P* Purpose:        Chiude il modulo
041900080515     P*--------------------------------------------------
042000080515     P Unita_finalize...
042100080515     P                 B                   EXPORT
042200080515     D Unita_finalize...
042300080515     D                 PI
042400080515
042500080515      /FREE
042600080515
042700080515       // Chiudo il gruppo di attivazione ANUNI.
042800080515
042900080515       CEE_NormalEnd();
043000080515
043100080515      /END-FREE
043200080515     P Unita_finalize...
043300080515     P                 E
043400080515
043500080515     P*--------------------------------------------------
043600080515     P* Procedure name: Unita_isInit
043700080515     P* Purpose:        Restituisce *ON se il modulo è già stato inizializzato.
043800080515     P* Returns:        *ON = modulo già inizializzato, *OFF = modulo non i...
043900080515     P*                          nizializzato.
044000080515     P*--------------------------------------------------
044100080515     P Unita_isInit...
044200080515     P                 B                   EXPORT
044300080515     D Unita_isInit...
044400080515     D                 PI              N
044500080515
044600080515      /FREE
044700080515
044800080515       RETURN initDone;
044900080515
045000080515      /END-FREE
045100080515     P Unita_isInit...
045200080515     P                 E
045300080515
