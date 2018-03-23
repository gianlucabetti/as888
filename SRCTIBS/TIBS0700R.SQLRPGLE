000100060620      //********************************************************************************************
000200060530      //
000300060620      // Questo modulo reperisce TABEL00F.
000400060531      //
000500060619      // Codice                                            Codice
000600060619      // operativo  Descrizione                            risposta   Descrizione
000700060620      // ---------- -------------------------------------- ---------- ------------------------------
000800060621      // SETLIBRARY Imposta la libreria di TABEL00F        DONE       Fatto.
000900060621      // GETLIBRARY Restituisce la libreria di TABEL00F
001000060620      // OPEN       Aprire TABEL00F.
001100060621      // CLOSE      Chiudere TABEL00F.
001200060620      // SETONLR    Accendere *INLR.
001300060619      // ---------- -------------------------------------- ---------- ------------------------------
001400060619      // CHAIN1     CHAIN con KUT.                         FOUND      Record trovato.
001500060619      // CHAIN2     CHAIN con KUT COD.                     NOTFOUND   Record non trovato.
001600060531      // CHAIN3     CHAIN con KUT COD KEY.
001700060619      // ---------- -------------------------------------- ---------- ------------------------------
001800060619      // SETLL1     SETLL con KUT.                         FOUND      Trovato record con chiave
001900060621      // SETLL2     SETLL con KUT COD.                                maggiore.
002000060621      // SETLL3     SETLL con KUT COD KEY.                 EQUAL      Trovato record con chiave
002100060621      // SETGT1     SETGT con KUT.                                    uguale.
002200060621      // SETGT2     SETGT con KUT COD.                     NOTFOUND   Trovato nessun record.
002300060621      // SETGT3     SETGT con KUT COD KEY.
002400060619      // ---------- -------------------------------------- ---------- ------------------------------
002500060619      // READE1     READE con KUT.                         EOF        Fine file.
002600060619      // READE2     READE con KUT COD.                     NOTEOF     Record trovato.
002700060531      // READE3     READE con KUT COD KEY.
002800060531      // READ       READ  senza chiave.
002900060620      // READPE1    READPE con KUT.
003000060620      // READPE2    READPE con KUT COD.
003100060620      // READPE3    READPE con KUT COD KEY.
003200060620      // READP      READP  senza chiave.
003300060619      // ---------- -------------------------------------- ---------- ------------------------------
003400060619      //            Preparazione e apertura cursore SQL    SQLCOD     RPYESITO contiene SQLCOD.
003500060626      // SQLOPEN1   ... con KUT.
003600060626      // SQLOPEN2   ... con KUT COD.
003700060626      // SQLOPEN3   ... con KUT COD e TBLKEY >= KEY.
003800060620      // FETCHNEXT  FETCH NEXT   del cursore aperto.
003900060620      // FETCHPRIOR FETCH PRIOR  del cursore aperto.
004000060620      // FETCHFIRST FETCH FIRST  del cursore aperto.
004100060620      // FETCHLAST  FETCH LAST   del cursore aperto.
004200060620      // FETCHBEFOR FETCH BEFORE del cursore aperto.
004300060620      // FETCHAFTER FETCH AFTER  del cursore aperto.
004400060626      // SQLCLOSE   Chiusura cursore aperto.
004500060620      //
004600060620      //********************************************************************************************
004700060620      //
004800060620      // Istruzioni per la compilazione:
004900060620      //
005000060620      // CRTSQLRPGI OBJ([libreria]/TIBS0700R) OBJTYPE(*MODULE)
005100060620      // CRTSRVPGM  SRVPGM([libreria]/TIBS0700R) EXPORT(*ALL)
005200060620      // CRTPGM     PGM([libreria]/TIBS0700R) ACTGRP(*CALLER)
005300060620      // DLTMOD     MODULE([libreria]/TIBS0700R)
005400060620      //
005500060530      //********************************************************************************************
005600060620     H OPTIMIZE(*FULL)
005700060303
005800060530      //********************************************************************************************
005900060530      //
006000060530      // Definizione files.
006100060530      //
006200060530      //********************************************************************************************
006300060530     Ftabel00f  IF   E           K DISK
006400060303     F                                     USROPN
006500060531     F                                     EXTFILE(fileName)
006600060531     F                                     INFDS(fileFbk)
006700060530
006800060530      //********************************************************************************************
006900060530      //
007000060530      // Definizione costanti.
007100060530      //
007200060530      //********************************************************************************************
007300060530     D Italiano        C                   1
007400060530     D Inglese         C                   2
007500060530     D Francese        C                   3
007600060530     D Tedesco         C                   4
007700091102
007800091102      //********************************************************************************************
007900091102      //
008000091102      // Procedure.
008100091102      //
008200091102      //********************************************************************************************
008300091102     D/COPY GAITRASRC/SRCPROTOPR,TIBS0700R
008400091102
008500060620     D*--------------------------------------------------
008600060620     D* Procedure name: chkKut
008700060620     D* Purpose:
008800060620     D* Returns:        Lingua
008900060620     D* Parameter:      rqsKut => Lingua.
009000060620     D*--------------------------------------------------
009100060620     D chkKut          PR             1P 0
009200060620     D  rqsKut                        1P 0
009300060621
009400060530      //********************************************************************************************
009500060530      //
009600060530      // Definizione strutture dati.
009700060530      //
009800060530      //********************************************************************************************
009900060531     D fileFbk         DS
010000060620     D  libraryFbk            93    102A
010100060626     D rpyTabel00f     DS                  LIKEREC(tabel)
010200060530
010300060530      //********************************************************************************************
010400060530      //
010500060619      // Definizione variabili globali.
010600060530      //
010700060530      //********************************************************************************************
010800060620     D rqsOpCode       S             10A
010900060621     D rpyOpCode       S             10A
011000060621     D rpyEsito        S             10I 0
011100060622     D rqsKut          S                   LIKE(tblKut)
011200060622     D rqsCod          S                   LIKE(tblCod)
011300060622     D rqsKey          S                   LIKE(tblKey)
011400060626     D rqsFormat       S             10A
011500060626     D rpyData         S                   LIKE(rpyTabel00f)
011600060621     D rqsOrderBy      S            256A
011700060620     D rqsLibrary      S             10A
011800060620     D rpyLibrary      S             10A
011900060621     D library         S             10A   VARYING
012000060621     D fileName        S             21A   VARYING INZ('*LIBL/TABEL00F')
012100060622     D keyKut          S                   LIKE(tblKut)
012200060620
012300060620      //********************************************************************************************
012400060620      //
012500060620      // Definizione parametri programma.
012600060620      //
012700060620      //********************************************************************************************
012800060620     C     *ENTRY        PLIST
012900060620     C                   PARM                    rqsOpCode                      1
013000060620     C                   PARM                    rpyOpCode                      2
013100060620     C                   PARM                    rpyEsito                       3
013200060620     C                   PARM                    rqsKut                         4
013300060620     C                   PARM                    rqsCod                         5
013400060620     C                   PARM                    rqsKey                         6
013500060626     C                   PARM                    rqsFormat                      7
013600060626     C                   PARM                    rpyData                        8
013700060626     C                   PARM                    rqsOrderBy                     9
013800060626     C                   PARM                    rqsLibrary                     10
013900060626     C                   PARM                    rpyLibrary                     11
014000060620      /FREE
014100060621
014200060621       SELECT;
014300060621         WHEN %SUBST(rqsOpCode:1:5) = 'CHAIN';
014400060626           chainTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rqsFormat
014500060626           :rpyOpCode:rpyEsito:rpyData);
014600060621         WHEN %SUBST(rqsOpCode:1:5) = 'FETCH';
014700060626           fetchTabel00f(rqsOpCode:rqsFormat:rpyOpCode:rpyEsito:rpyData);
014800060621         WHEN %SUBST(rqsOpCode:1:4) = 'READ';
014900060626           readTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rqsFormat
015000060626           :rpyOpCode:rpyEsito:rpyData);
015100060621         WHEN %SUBST(rqsOpCode:1:5) = 'SETLL'
015200060621         OR %SUBST(rqsOpCode:1:5) = 'SETGT';
015300060621           setTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rpyOpCode
015400060621           :rpyEsito);
015500060626         WHEN %SUBST(rqsOpCode:1:7) = 'SQLOPEN';
015600060621           openSqlTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey)
015700060621           :rpyOpCode:rpyEsito:rqsOrderBy:%LEN(rqsOrderBy));
015800060626         WHEN rqsOpCode = 'SQLCLOSE';
015900060621           closeSqlTabel00f(rpyOpCode:rpyEsito);
016000060621         WHEN rqsOpCode = 'OPEN';
016100060621           openTabel00f(rpyOpCode:rpyEsito);
016200060621         WHEN rqsOpCode = 'CLOSE';
016300060621           closeTabel00f(rpyOpCode:rpyEsito);
016400060621         WHEN rqsOpCode = 'SETONLR';
016500060621           *INLR = *ON;
016600060621           rpyOpCode = 'DONE';
016700060621         WHEN rqsOpCode = 'SETLIBRARY';
016800060621           setTabel00fLibrary(rqsLibrary:rpyOpCode:rpyEsito);
016900060621         WHEN rqsOpCode = 'GETLIBRARY';
017000060621           getTabel00fLibrary(rpyOpCode:rpyEsito:rpyLibrary);
017100060621         OTHER;
017200060621           rpyOpCode = 'ERROR';
017300060621           rpyEsito = -1;
017400060621       ENDSL;
017500060621
017600060620       RETURN;
017700060620
017800060620      /END-FREE
017900060619
018000060619
018100060619     P*--------------------------------------------------
018200060621     P* Procedure name: openTabel00f
018300060621     P* Purpose:        OPEN TABEL00F
018400060619     P* Returns:
018500060619     P* Parameter:      rqsLibrary => Libreria
018600060619     P* Parameter:      rpyLibrary => Libreria dove risiede TABEL00F
018700060619     P* Parameter:      rpyOpCode => Codice risposta.
018800060619     P* Parameter:      rpyEsito => Esito.
018900060619     P*--------------------------------------------------
019000060621     P openTabel00f    B                   EXPORT
019100060621     D openTabel00f    PI
019200060619     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
019300060619     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
019400060619
019500060619      /FREE
019600060619
019700060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
019800060619         rpyOpCode = 'DONE';
019900060619       ENDIF;
020000060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
020100060619         CLEAR rpyEsito;
020200060619       ENDIF;
020300060619
020400060619       IF NOT %OPEN(tabel00f);
020500060621         IF library = *BLANK;
020600060620           RESET fileName;
020700060619         ELSE;
020800060621           fileName = library + '/TABEL00F';
020900060619         ENDIF;
021000060619         OPEN(E) tabel00f;
021100060619         IF %ERROR;
021200060621           IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
021300060619             rpyOpCode = 'ERROR';
021400060619           ENDIF;
021500060621           IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
021600060619             rpyEsito = %STATUS;
021700060619           ENDIF;
021800060619         ELSE;
021900060621           library = libraryFbk;
022000060619         ENDIF;
022100060619       ENDIF;
022200060619
022300060619      /END-FREE
022400060621     P openTabel00f    E
022500060619
022600060619
022700060619     P*--------------------------------------------------
022800060621     P* Procedure name: closeTabel00F
022900060619     P* Purpose:        CLOSE TABEL00F
023000060619     P* Returns:
023100060619     P* Parameter:      rpyOpCode => Codice risposta.
023200060619     P* Parameter:      rpyEsito => Esito
023300060619     P*--------------------------------------------------
023400060621     P closeTabel00f   B                   EXPORT
023500060621     D closeTabel00f   PI
023600060619     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
023700060619     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
023800060619
023900060619      /FREE
024000060619
024100060619       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
024200060619         rpyOpCode = 'DONE';
024300060619       ENDIF;
024400060619       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
024500060619         CLEAR rpyEsito;
024600060619       ENDIF;
024700060619
024800060619       IF %OPEN(tabel00f);
024900060619         CLOSE(E) tabel00f;
025000060619         IF %ERROR;
025100060619           IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
025200060619             rpyOpCode = 'ERROR';
025300060619           ENDIF;
025400060619           IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
025500060619             rpyEsito = %STATUS;
025600060619           ENDIF;
025700060619         ENDIF;
025800060619       ENDIF;
025900060619
026000060619      /END-FREE
026100060621     P closeTabel00f   E
026200060619
026300060619
026400060619     P*--------------------------------------------------
026500060621     P* Procedure name: chainTabel00f
026600060619     P* Purpose:        CHAIN TABEL00F
026700060619     P* Returns:
026800060619     P* Parameter:      rqsKut => Lingua
026900060619     P* Parameter:      rqsCod => Codice tabella
027000060619     P* Parameter:      rqsKey => Codice elemento.
027100060619     P* Parameter:      rpyOpCode => Codice risposta.
027200060619     P* Parameter:      rpyEsito => Esito
027300060619     P* Parameter:      rpyTabel00f
027400060619     P*--------------------------------------------------
027500060621     P chainTabel00f   B                   EXPORT
027600060626     D chainTabel00f   PI                  LIKE(rpyTabel00f)
027700060621     D  rqsOpCode                    10A   CONST
027800060619     D  rqsKut                        1P 0
027900060621     D  rqsCod                        2A   CONST
028000060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
028100060620     D  rqsLengthOfKey...
028200060621     D                               10I 0 CONST
028300060626     D  rqsFormat                    10A   CONST
028400060621     D  rpyOpCode                    10A
028500060621     D  rpyEsito                     10I 0
028600060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
028700060626     D                                     LIKE(rpyTabel00f)
028800060620
028900060626     D rtnData         S                   LIKE(rpyTabel00f)
029000060619      /FREE
029100060619
029200060619       CLEAR rpyOpCode;
029300060619       CLEAR rpyEsito;
029400060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
029500060626         CLEAR rpyData;
029600060626       ENDIF;
029700060626       CLEAR rpyTabel00f;
029800060622       keyKut = chkKut(rqsKut);
029900060619
030000060619       DOU %FOUND;
030100060619         SELECT;
030200060619           WHEN rqsOpCode = 'CHAIN3';
030300060622             CHAIN(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
030400060626             rpyTabel00f;
030500060619           WHEN rqsOpCode = 'CHAIN2';
030600060626             CHAIN(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
030700060619           WHEN rqsOpCode = 'CHAIN1';
030800060626             CHAIN(E) (keyKut) tabel00f rpyTabel00f;
030900060620           OTHER;
031000060620             rpyOpCode = 'ERROR';
031100060620             rpyEsito = -1;
031200060626             SELECT;
031300060626               WHEN rqsFormat = 'TABEL';
031400060626                 rtnData = rpyTabel00f;
031500060626               WHEN rqsFormat = 'TBLUNI';
031600060626                 rtnData = rpyTabel00f.tblUni;
031700060626               OTHER;
031800060626                 CLEAR rtnData;
031900060626             ENDSL;
032000060626             RETURN rtnData;
032100060619         ENDSL;
032200060619         IF %ERROR;
032300060619           rpyOpCode = 'ERROR';
032400060619           rpyEsito = %STATUS;
032500060626           SELECT;
032600060626             WHEN rqsFormat = 'TABEL';
032700060626               rtnData = rpyTabel00f;
032800060626             WHEN rqsFormat = 'TBLUNI';
032900060626               rtnData = rpyTabel00f.tblUni;
033000060626             OTHER;
033100060626               CLEAR rtnData;
033200060626           ENDSL;
033300060626           RETURN rtnData;
033400060626         ENDIF;
033500060622         IF %FOUND OR keyKut = Italiano;
033600060619           LEAVE;
033700060619         ENDIF;
033800060622         keyKut = Italiano;
033900060619       ENDDO;
034000060619
034100060619       IF %FOUND;
034200060619         rpyOpCode = 'FOUND';
034300060619       ELSE;
034400060619         rpyOpCode = 'NOTFOUND';
034500060619       ENDIF;
034600060619
034700060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
034800060626         SELECT;
034900060626           WHEN rqsFormat = 'TABEL';
035000060626             rpyData = rpyTabel00f;
035100060626           WHEN rqsFormat = 'TBLUNI';
035200060626             rpyData = rpyTabel00f.tblUni;
035300060626           OTHER;
035400060626             CLEAR rpyData;
035500060626         ENDSL;
035600060620       ENDIF;
035700060620
035800060626       SELECT;
035900060626         WHEN rqsFormat = 'TABEL';
036000060626           rtnData = rpyTabel00f;
036100060626         WHEN rqsFormat = 'TBLUNI';
036200060626           rtnData = rpyTabel00f.tblUni;
036300060626         OTHER;
036400060626           CLEAR rtnData;
036500060626       ENDSL;
036600060626
036700060626       RETURN rtnData;
036800060626
036900060619      /END-FREE
037000060621     P chainTabel00f   E
037100060619
037200060619
037300060619     P*--------------------------------------------------
037400060621     P* Procedure name: setTabel00f
037500060620     P* Purpose:        SETXX TABEL00F
037600060619     P* Returns:
037700060619     P* Parameter:      rqsKut => Lingua
037800060619     P* Parameter:      rqsCod => Codice tabella
037900060619     P* Parameter:      rqsKey => Codice elemento.
038000060619     P* Parameter:      rpyOpCode => Codice risposta.
038100060619     P* Parameter:      rpyEsito => Esito
038200060619     P*--------------------------------------------------
038300060621     P setTabel00f     B                   EXPORT
038400060621     D setTabel00f     PI
038500060621     D  rqsOpCode                    10A   CONST
038600060619     D  rqsKut                        1P 0
038700060621     D  rqsCod                        2A   CONST
038800060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
038900060620     D  rqsLengthOfKey...
039000060621     D                               10I 0 CONST
039100060619     D  rpyOpCode                    10A
039200060619     D  rpyEsito                     10I 0
039300060619
039400060619      /FREE
039500060619
039600060619       CLEAR rpyOpCode;
039700060619       CLEAR rpyEsito;
039800060622       keyKut = chkKut(rqsKut);
039900060619
040000060619       DOU %FOUND;
040100060619         SELECT;
040200060619           WHEN rqsOpCode = 'SETLL2';
040300060622             SETLL(E) (keyKut:rqsCod) tabel00f;
040400060619           WHEN rqsOpCode = 'SETLL1';
040500060622             SETLL(E) (keyKut) tabel00f;
040600060619           WHEN rqsOpCode = 'SETLL3';
040700060622             SETLL(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f;
040800060620           WHEN rqsOpCode = 'SETGT2';
040900060622             SETGT(E) (keyKut:rqsCod) tabel00f;
041000060620           WHEN rqsOpCode = 'SETGT1';
041100060622             SETGT(E) (keyKut) tabel00f;
041200060620           WHEN rqsOpCode = 'SETGT3';
041300060622             SETGT(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f;
041400060620           OTHER;
041500060620             rpyOpCode = 'ERROR';
041600060620             rpyEsito = -1;
041700060620             RETURN;
041800060619         ENDSL;
041900060619         IF %ERROR;
042000060619           rpyOpCode = 'ERROR';
042100060619           rpyEsito = %STATUS;
042200060619           RETURN;
042300060619         ENDIF;
042400060622         IF %FOUND OR keyKut = Italiano;
042500060619           LEAVE;
042600060619         ENDIF;
042700060622         keyKut = Italiano;
042800060619       ENDDO;
042900060619
043000060619       SELECT;
043100060619         WHEN %EQUAL;
043200060619           rpyOpCode = 'EQUAL';
043300060619         WHEN %FOUND;
043400060619           rpyOpCode = 'FOUND';
043500060619         OTHER;
043600060619           rpyOpCode = 'NOTFOUND';
043700060619       ENDSL;
043800060619
043900060619      /END-FREE
044000060621     P setTabel00f     E
044100060619
044200060619
044300060619     P*--------------------------------------------------
044400060621     P* Procedure name: readTabel00f
044500060620     P* Purpose:        READXX TABEL00F
044600060619     P* Returns:
044700060619     D* Parameter:      rqsOpCode => Codice operativo
044800060619     P* Parameter:      rqsKut => Lingua
044900060619     P* Parameter:      rqsCod => Codice tabella
045000060619     P* Parameter:      rqsKey => Codice elemento.
045100060619     P* Parameter:      rpyOpCode => Codice risposta.
045200060619     P* Parameter:      rpyEsito => Esito
045300060619     P* Parameter:      rpyTabel00f
045400060619     P*--------------------------------------------------
045500060621     P readTabel00f    B                   EXPORT
045600060626     D readTabel00f    PI                  LIKE(rpyTabel00f)
045700060621     D  rqsOpCode                    10A   CONST
045800060619     D  rqsKut                        1P 0
045900060621     D  rqsCod                        2A   CONST
046000060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
046100060620     D  rqsLengthOfKey...
046200060621     D                               10I 0 CONST
046300060626     D  rqsFormat                    10A   CONST
046400060619     D  rpyOpCode                    10A
046500060619     D  rpyEsito                     10I 0
046600060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
046700060626     D                                     LIKE(rpyTabel00f)
046800060620
046900060626     D rtnData         S                   LIKE(rpyTabel00f)
047000060619      /FREE
047100060619
047200060626       CLEAR rpyTabel00f;
047300060619       CLEAR rpyOpCode;
047400060619       CLEAR rpyEsito;
047500060620
047600060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
047700060626         CLEAR rpyData;
047800060620       ENDIF;
047900060620
048000060622       keyKut = chkKut(rqsKut);
048100060619
048200060619       SELECT;
048300060619         WHEN rqsOpCode = 'READE2';
048400060626           READE(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
048500060619         WHEN rqsOpCode = 'READE1';
048600060626           READE(E) (keyKut) tabel00f rpyTabel00f;
048700060619         WHEN rqsOpCode = 'READE3';
048800060622           READE(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
048900060626           rpyTabel00f;
049000060620         WHEN rqsOpCode = 'READ';
049100060626           READ(E) tabel00f rpyTabel00f;
049200060620         WHEN rqsOpCode = 'READPE2';
049300060626           READPE(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
049400060620         WHEN rqsOpCode = 'READPE1';
049500060626           READPE(E) (keyKut) tabel00f rpyTabel00f;
049600060620         WHEN rqsOpCode = 'READPE3';
049700060622           READPE(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
049800060626           rpyTabel00f;
049900060620         WHEN rqsOpCode = 'READP';
050000060626           READP(E) tabel00f rpyTabel00f;
050100060620         OTHER;
050200060620           rpyOpCode = 'ERROR';
050300060620           rpyEsito = -1;
050400060626           SELECT;
050500060626             WHEN rqsFormat = 'TABEL';
050600060626               rtnData = rpyTabel00f;
050700060626             WHEN rqsFormat = 'TBLUNI';
050800060626               rtnData = rpyTabel00f.tblUni;
050900060626             OTHER;
051000060626               CLEAR rtnData;
051100060626           ENDSL;
051200060626           RETURN rtnData;
051300060626       ENDSL;
051400060619
051500060619       IF %EOF;
051600060619         rpyOpCode = 'EOF';
051700060619       ELSE;
051800060619         rpyOpCode = 'NOTEOF';
051900060619       ENDIF;
052000060620
052100060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
052200060626         SELECT;
052300060626           WHEN rqsFormat = 'TABEL';
052400060626             rpyData = rpyTabel00f;
052500060626           WHEN rqsFormat = 'TBLUNI';
052600060626             rpyData = rpyTabel00f.tblUni;
052700060626           OTHER;
052800060626             rpyData = *BLANK;
052900060626         ENDSL;
053000060620       ENDIF;
053100060619
053200060626       SELECT;
053300060626         WHEN rqsFormat = 'TABEL';
053400060626           rtnData = rpyTabel00f;
053500060626         WHEN rqsFormat = 'TBLUNI';
053600060626           rtnData = rpyTabel00f.tblUni;
053700060626         OTHER;
053800060626           CLEAR rtnData;
053900060626       ENDSL;
054000060626
054100060626       RETURN rtnData;
054200060626
054300060619      /END-FREE
054400060621     P readTabel00f    E
054500060619
054600060619
054700060619     P*--------------------------------------------------
054800060620     P* Procedure name: openSqlTabel
054900060619     P* Purpose:        PREPARE e OPEN cursore TABEL00F
055000060619     P* Returns:
055100060619     D* Parameter:      rqsOpCode => Codice operativo
055200060619     P* Parameter:      rqsKut => Lingua
055300060619     P* Parameter:      rqsCod => Codice tabella
055400060619     P* Parameter:      rqsKey => Codice elemento.
055500060619     P* Parameter:      rpyOpCode => Codice risposta.
055600060619     P* Parameter:      rpyEsito => Esito
055700060619     P*--------------------------------------------------
055800060621     P openSqlTabel00f...
055900060621     P                 B                   EXPORT
056000060621     D openSqlTabel00f...
056100060621     D                 PI
056200060621     D  rqsOpCode                    10A   CONST
056300060619     D  rqsKut                        1P 0
056400060621     D  rqsCod                        2A   CONST
056500060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
056600060620     D  rqsLengthOfKey...
056700060621     D                               10I 0 CONST
056800060619     D  rpyOpCode                    10A
056900060619     D  rpyEsito                     10I 0
057000060620     D  rqsOrderBy                  256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
057100060621     D  rqsLengthOfOrderBy...
057200060621     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
057300091102     D  rqsWhere                    256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
057400091102     D  rqsLengthOfWhere...
057500091102     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
057600060619
057700060620     D tableName       S             21A
057800060620     D                                     VARYING
057900060620     D                                     INZ('TABEL00F')
058000091030     D stmSql          S          32740A
058100060619     D                                     VARYING
058200060620     D orderBy         S            256A
058300060620     D                                     VARYING
058400060620     D                                     INZ('TBLCOD, TBLKEY')
058500060619
058600060620     C/EXEC SQL
058700060620     C+ SET OPTION DYNUSRPRF = *OWNER
058800060620     C/END-EXEC
058900060620
059000060619      /FREE
059100060619
059200060621       closeSqlTabel00f();
059300060622       keyKut = chkKut(rqsKut);
059400060620
059500060621       IF %PARMS > 7 AND %ADDR(rqsOrderBy) <> *NULL AND rqsOrderBy <> *BLANK;
059600091030         orderBy = %TRIMR(%SUBST(rqsOrderBy : 1 : rqsLengthOfOrderBy));
059700060620       ENDIF;
059800060619
059900060621       IF library = *BLANK;
060000060620         RESET tableName;
060100060619       ELSE;
060200060621         tableName = library + '/TABEL00F';
060300060619       ENDIF;
060400060619
060500091102       stmSql = 'WITH TABEL00F_THIS +
060600091102                 (TBLKUT, TBLCOD, TBLKEY, TBLFLG, TBLUNI, TBLFTT, TBLFLT +
060700091102                 , TBLFTR, TBLDTR) AS ( +
060800091102                 SELECT * FROM ' + tableName;
060900060622       stmSql += ' WHERE TBLKUT = ' + %CHAR(keyKut);
061000060619
061100060619       SELECT;
061200060626         WHEN rqsOpCode = 'SQLOPEN2';
061300060619           stmSql += ' AND TBLCOD = ' + '''' + rqsCod + '''';
061400060626         WHEN rqsOpCode = 'SQLOPEN3';
061500060619           stmSql += ' AND TBLCOD = ' + '''' + rqsCod + ''''
061600060620           + ' AND TBLKEY >= ' + '''' + %SUBST(rqsKey:1:rqsLengthOfKey) + '''';
061700060619       ENDSL;
061800060619
061900060620       stmSql += ' UNION SELECT A.* FROM ' + tableName + ' AS A +
062000060622       EXCEPTION JOIN ' + tableName + ' AS B ON ' + %CHAR(keyKut) +
062100060619       ' = B.TBLKUT AND A.TBLCOD = B.TBLCOD AND A.TBLKEY = B.TBLKEY +
062200060619       WHERE A.TBLKUT = 1';
062300060619
062400060619       SELECT;
062500060626         WHEN rqsOpCode = 'SQLOPEN2';
062600060619           stmSql += ' AND A.TBLCOD = ' + '''' + rqsCod + '''';
062700060626         WHEN rqsOpCode = 'SQLOPEN3';
062800060619           stmSql += ' AND A.TBLCOD = ' + '''' + rqsCod + ''''
062900060620           + ' AND A.TBLKEY >= ' + ''' + %SUBST(rqsKey:1:rqsLengthOfKey) + ''';
063000060619       ENDSL;
063100060619
063200091102       stmSql += ') SELECT * FROM TABEL00F_THIS';
063300091102
063400091102       IF %PARMS > 9 AND %ADDR(rqsWhere) <> *NULL AND rqsWhere <> *BLANK;
063500091102         stmSql += ' WHERE ' + %TRIMR(%SUBST(rqsWhere : 1 : rqsLengthOfWhere));
063600091102       ENDIF;
063700091102
063800091102       stmSql += ' ORDER BY ' + orderBy;
063900060619
064000060619      /END-FREE
064100060619     C/EXEC SQL
064200060619     C+ PREPARE TABEL00F FROM :stmSql
064300060619     C/END-EXEC
064400060619     C
064500060619     C/EXEC SQL
064600060620     C+ DECLARE TABEL00F SCROLL CURSOR FOR TABEL00F
064700060619     C/END-EXEC
064800060619     C
064900060619     C/EXEC SQL
065000060619     C+ OPEN TABEL00F
065100060619     C/END-EXEC
065200060619      /FREE
065300060619
065400091030       IF sqlCode < 0;
065500091030         DUMP(A);
065600091030       ENDIF;
065700091030
065800060619       rpyOpCode = 'SQLCOD';
065900060619       rpyEsito = sqlCod;
066000060619
066100060619      /END-FREE
066200060621     P openSqlTabel00f...
066300060621     P                 E
066400060619
066500060619
066600060619     P*--------------------------------------------------
066700060620     P* Procedure name: fetchTabel
066800060619     P* Purpose:        FETCH cursore TABEL00F
066900060619     P* Returns:
067000060619     P*--------------------------------------------------
067100060621     P fetchTabel00f   B                   EXPORT
067200060626     D fetchTabel00f   PI                  LIKE(rpyTabel00f)
067300060621     D  rqsOpCode                    10A   CONST
067400091030     D  rqsFormat                    10A   CONST
067500060619     D  rpyOpCode                    10A
067600060619     D  rpyEsito                     10I 0
067700060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
067800060626     D                                     LIKE(rpyTabel00f)
067900060620
068000060626     D rtnData         S                   LIKE(rpyTabel00f)
068100060619      /FREE
068200060619
068300060626       IF %PARMS > 4 AND %ADDR(rpyData) <> *NULL;
068400060626         CLEAR rpyData;
068500060626       ENDIF;
068600060626
068700060626       CLEAR rpyTabel00f;
068800060619
068900060619       SELECT;
069000060619         WHEN rqsOpCode = 'FETCHNEXT';
069100060619      /END-FREE
069200060619     C/EXEC SQL
069300060620     C+ FETCH NEXT FROM TABEL00F
069400060626     C+ INTO :rpyTabel00f
069500060619     C/END-EXEC
069600060619      /FREE
069700060620         WHEN rqsOpCode = 'FETCHPRIOR';
069800060620      /END-FREE
069900060620     C/EXEC SQL
070000060620     C+ FETCH PRIOR FROM TABEL00F
070100060626     C+ INTO :rpyTabel00f
070200060620     C/END-EXEC
070300060620      /FREE
070400060620         WHEN rqsOpCode = 'FETCHFIRST';
070500060620      /END-FREE
070600060620     C/EXEC SQL
070700060620     C+ FETCH FIRST FROM TABEL00F
070800060626     C+ INTO :rpyTabel00f
070900060620     C/END-EXEC
071000060620      /FREE
071100060620         WHEN rqsOpCode = 'FETCHLAST';
071200060620      /END-FREE
071300060620     C/EXEC SQL
071400060620     C+ FETCH LAST FROM TABEL00F
071500060626     C+ INTO :rpyTabel00f
071600060620     C/END-EXEC
071700060620      /FREE
071800060620         WHEN rqsOpCode = 'FETCHBEFOR';
071900060620      /END-FREE
072000060620     C/EXEC SQL
072100060620     C+ FETCH BEFORE FROM TABEL00F
072200060620     C/END-EXEC
072300060620      /FREE
072400060620         WHEN rqsOpCode = 'FETCHAFTER';
072500060620      /END-FREE
072600060620     C/EXEC SQL
072700060620     C+ FETCH AFTER FROM TABEL00F
072800060620     C/END-EXEC
072900060620      /FREE
073000060619         OTHER;
073100060619           rpyOpCode = 'ERROR';
073200060619           rpyEsito = -1;
073300060626           SELECT;
073400060626             WHEN rqsFormat = 'TABEL';
073500060626               rtnData = rpyTabel00f;
073600060626             WHEN rqsFormat = 'TBLUNI';
073700060626               rtnData = rpyTabel00f.tblUni;
073800060626             OTHER;
073900060626               CLEAR rtnData;
074000060626           ENDSL;
074100060626           RETURN rtnData;
074200060626       ENDSL;
074300060619
074400060619       rpyOpCode = 'SQLCOD';
074500060619       rpyEsito = sqlCod;
074600060620
074700060626       IF %PARMS > 4 AND %ADDR(rpyData) <> *NULL;
074800060626         rpyData = rpyTabel00f;
074900060620       ENDIF;
075000060620
075100060626       SELECT;
075200060626         WHEN rqsFormat = 'TABEL';
075300060626           rtnData = rpyTabel00f;
075400060626         WHEN rqsFormat = 'TBLUNI';
075500060626           rtnData = rpyTabel00f.tblUni;
075600060626         OTHER;
075700060626           CLEAR rtnData;
075800060626       ENDSL;
075900060626
076000060626       RETURN rtnData;
076100060626
076200060619      /END-FREE
076300060621     P fetchTabel00f   E
076400060619
076500060619
076600060619     P*--------------------------------------------------
076700060620     P* Procedure name: closeSqlTabel
076800060619     P* Purpose:        CLOSE cursore TABEL00F
076900060619     P* Returns:
077000060619     P*--------------------------------------------------
077100060621     P closeSqlTabel00f...
077200060621     P                 B                   EXPORT
077300060621     D closeSqlTabel00f...
077400060621     D                 PI
077500060620     D  rpyOpCode                    10A   OPTIONS(*OMIT:*NOPASS)
077600060620     D  rpyEsito                     10I 0 OPTIONS(*OMIT:*NOPASS)
077700060619
077800060619
077900060619     C/EXEC SQL
078000060619     C+ CLOSE TABEL00F
078100060619     C/END-EXEC
078200060619
078300060619      /FREE
078400060619
078500060620       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
078600060620         rpyOpCode = 'SQLCOD';
078700060620       ENDIF;
078800060620       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
078900060620         rpyEsito = sqlCod;
079000060620       ENDIF;
079100060619
079200060619      /END-FREE
079300060621     P closeSqlTabel00f...
079400060621     P                 E
079500060619
079600060619     P*--------------------------------------------------
079700060619     P* Procedure name: chkKut
079800060619     P* Purpose:
079900060619     P* Returns:        Lingua
080000060619     P* Parameter:      rqsKut => Lingua.
080100060619     P*--------------------------------------------------
080200060619     P chkKut          B
080300060619     D chkKut          PI             1P 0
080400060620     D  rqsKut                        1P 0
080500060619
080600060619     D rpyKut          S              1P 0
080700060619
080800060619      /FREE
080900060620
081000060619       IF rqsKut = Italiano OR rqsKut = Inglese
081100060619       OR rqsKut = Francese OR rqsKut = Tedesco;
081200060619         rpyKut = rqsKut;
081300060619       ELSE;
081400060619         rpyKut = Italiano;
081500060619       ENDIF;
081600060619
081700060619       RETURN rpyKut;
081800060619
081900060619      /END-FREE
082000060619     P chkKut          E
082100060621
082200060621
082300060621     P*--------------------------------------------------
082400060621     P* Procedure name: setTabel00fLibrary
082500060621     P* Purpose:        Imposta la libreria di TABEL00F
082600060621     P* Returns:
082700060621     P* Parameter:      rqsLibrary => Libreria
082800060621     P* Parameter:      rpyOpCode => Codice risposta
082900060621     P* Parameter:      rpyEsito => Esito
083000060621     P*--------------------------------------------------
083100060621     P setTabel00fLibrary...
083200060621     P                 B                   EXPORT
083300060621     D setTabel00fLibrary...
083400060621     D                 PI
083500060621     D  rqsLibrary                   10A   CONST
083600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
083700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
083800060621
083900060621      /FREE
084000060621
084100060621       IF rqsLibrary = *BLANK OR rqsLibrary = '*LIBL';
084200060621         RESET library;
084300060621       ELSE;
084400060621         library = rqsLibrary;
084500060621       ENDIF;
084600060621
084700060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
084800060621         rpyOpCode = 'DONE';
084900060621       ENDIF;
085000060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
085100060621         rpyEsito = 0;
085200060621       ENDIF;
085300060621
085400060621      /END-FREE
085500060621     P setTabel00fLibrary...
085600060621     P                 E
085700060621
085800060621
085900060621     P*--------------------------------------------------
086000060621     P* Procedure name: getTabel00fLibrary
086100060621     P* Purpose:        Restituisce la libreria di TABEL00F
086200060621     P* Returns:        Libreria
086300060621     P* Parameter:      rpyOpCode => Codice risposta.
086400060621     P* Parameter:      rpyEsito => Esito
086500060621     P* Parameter:      rpyLibrary => Libreria
086600060621     P*--------------------------------------------------
086700060621     P getTabel00fLibrary...
086800060621     P                 B                   EXPORT
086900060621     D getTabel00fLibrary...
087000060621     D                 PI            10A
087100060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
087200060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
087300060621     D  rpyLibrary                   10A   OPTIONS(*NOPASS:*OMIT)
087400060621
087500060621      /FREE
087600060621
087700060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
087800060621         rpyOpCode = 'DONE';
087900060621       ENDIF;
088000060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
088100060621         rpyEsito = 0;
088200060621       ENDIF;
088300060621       IF %PARMS > 2 AND %ADDR(rpyLibrary) <> *NULL;
088400060621         rpyLibrary = library;
088500060621       ENDIF;
088600060621
088700060621       RETURN library;
088800060621
088900060621      /END-FREE
089000060621     P getTabel00fLibrary...
089100060621     P                 E
089200060621
