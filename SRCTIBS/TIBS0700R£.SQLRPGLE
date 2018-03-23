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
007700060620
007800060620     D*--------------------------------------------------
007900060620     D* Procedure name: openTabel
008000060621     D* Purpose:        OPEN TABEL00F
008100060620     D* Returns:
008200060620     D* Parameter:      rpyOpCode => Codice risposta.
008300060620     D* Parameter:      rpyEsito => Esito.
008400060620     D*--------------------------------------------------
008500060621     D openTabel00f    PR
008600060620     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
008700060620     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
008800060620
008900060620     D*--------------------------------------------------
009000060620     D* Procedure name: closeTabel
009100060620     D* Purpose:        CLOSE TABEL00F
009200060620     D* Returns:
009300060620     D* Parameter:      rpyOpCode => Codice risposta.
009400060620     D* Parameter:      rpyEsito => Esito
009500060620     D*--------------------------------------------------
009600060621     D closeTabel00f   PR
009700060620     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
009800060620     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
009900060620
010000060620     D*--------------------------------------------------
010100060620     D* Procedure name: chainTabel
010200060620     D* Purpose:        CHAIN TABEL00F
010300060620     D* Returns:
010400060620     D* Parameter:      rqsOpCode => Codice operativo
010500060620     D* Parameter:      rqsKut => Lingua
010600060620     D* Parameter:      rqsCod => Codice tabella
010700060620     D* Parameter:      rqsKey => Codice elemento.
010800060620     D* Parameter:      rpyOpCode => Codice risposta.
010900060620     D* Parameter:      rpyEsito => Esito
011000060620     D* Parameter:      rpyTabel00f
011100060620     D*--------------------------------------------------
011200060626     D chainTabel00f   PR                  LIKE(rpyTabel00f)
011300060621     D  rqsOpCode                    10A   CONST
011400060620     D  rqsKut                        1P 0
011500060621     D  rqsCod                        2A   CONST
011600060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
011700060620     D  rqsLengthOfKey...
011800060621     D                               10I 0 CONST
011900060626     D  rqsFormat                    10A   CONST
012000060620     D  rpyOpCode                    10A
012100060620     D  rpyEsito                     10I 0
012200060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
012300060626     D                                     LIKE(rpyTabel00f)
012400060620
012500060620     D*--------------------------------------------------
012600060620     D* Procedure name: setTabel
012700060620     D* Purpose:        SETLL TABEL00F
012800060620     D* Returns:
012900060620     D* Parameter:      rqsOpCode => Codice operativo
013000060620     D* Parameter:      rqsKut => Lingua
013100060620     D* Parameter:      rqsCod => Codice tabella
013200060620     D* Parameter:      rqsKey => Codice elemento.
013300060620     D* Parameter:      rpyOpCode => Codice risposta.
013400060620     D* Parameter:      rpyEsito => Esito
013500060620     D*--------------------------------------------------
013600060621     D setTabel00f     PR
013700060621     D  rqsOpCode                    10A   CONST
013800060620     D  rqsKut                        1P 0
013900060621     D  rqsCod                        2A   CONST
014000060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
014100060620     D  rqsLengthOfKey...
014200060621     D                               10I 0 CONST
014300060620     D  rpyOpCode                    10A
014400060620     D  rpyEsito                     10I 0
014500060620
014600060620     D*--------------------------------------------------
014700060620     D* Procedure name: readTabel
014800060620     D* Purpose:        READ{E} TABEL00F
014900060620     D* Returns:
015000060620     D* Parameter:      rqsKut => Lingua
015100060620     D* Parameter:      rqsCod => Codice tabella
015200060620     D* Parameter:      rqsKey => Codice elemento.
015300060620     D* Parameter:      rpyOpCode => Codice risposta.
015400060620     D* Parameter:      rpyEsito => Esito
015500060620     D* Parameter:      rpyTabel00f
015600060620     D*--------------------------------------------------
015700060626     D readTabel00f    PR                  LIKE(rpyTabel00f)
015800060621     D  rqsOpCode                    10A   CONST
015900060620     D  rqsKut                        1P 0
016000060621     D  rqsCod                        2A   CONST
016100060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
016200060620     D  rqsLengthOfKey...
016300060621     D                               10I 0 CONST
016400060626     D  rqsFormat                    10A   CONST
016500060620     D  rpyOpCode                    10A
016600060620     D  rpyEsito                     10I 0
016700060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
016800060626     D                                     LIKE(rpyTabel00f)
016900060620
017000060620     D*--------------------------------------------------
017100060621     D* Procedure name: openSqlTabel00f
017200060620     D* Purpose:        PREPARE e OPEN cursore TABEL00F
017300060620     D* Returns:
017400060620     D* Parameter:      rqsOpCode => Codice operativo
017500060620     D* Parameter:      rqsKut => Lingua
017600060620     D* Parameter:      rqsLibrary => Libreria
017700060620     D* Parameter:      rqsCod => Codice tabella
017800060620     D* Parameter:      rqsKey => Codice elemento.
017900060620     D* Parameter:      rpyOpCode => Codice risposta.
018000060620     D* Parameter:      rpyEsito => Esito
018100060620     D*--------------------------------------------------
018200060621     D openSqlTabel00f...
018300060621     D                 PR
018400060621     D  rqsOpCode                    10A   CONST
018500060620     D  rqsKut                        1P 0
018600060621     D  rqsCod                        2A   CONST
018700060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
018800060620     D  rqsLengthOfKey...
018900060621     D                               10I 0 CONST
019000060620     D  rpyOpCode                    10A
019100060620     D  rpyEsito                     10I 0
019200060620     D  rqsOrderBy                  256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
019300060621     D  rqsLengthOfOrderBy...
019400060621     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
019500060620
019600060620     D*--------------------------------------------------
019700060621     D* Procedure name: fetchTabel00f
019800060621     D* Purpose:        FETCH cursore TABEL00F
019900060620     D* Returns:        TABEL00F
020000060620     D* Parameter:      rqsOpCode => Codice operativo
020100060620     D* Parameter:      rpyOpCode => Codice risposta.
020200060620     D* Parameter:      rpyEsito => Esito
020300060620     D* Parameter:      rpyTabel00f
020400060620     D*--------------------------------------------------
020500060626     D fetchTabel00f   PR                  LIKE(rpyTabel00f)
020600060621     D  rqsOpCode                    10A   CONST
020700060626     D  rqsFormat                    10A   CONST
020800060620     D  rpyOpCode                    10A
020900060620     D  rpyEsito                     10I 0
021000060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
021100060626     D                                     LIKE(rpyTabel00f)
021200060620
021300060620     D*--------------------------------------------------
021400060621     D* Procedure name: closeSqlTabel00f
021500060620     D* Purpose:        CLOSE cursore TABEL00F
021600060620     D* Returns:
021700060620     D*--------------------------------------------------
021800060621     D closeSqlTabel00f...
021900060621     D                 PR
022000060620     D  rpyOpCode                    10A   OPTIONS(*OMIT:*NOPASS)
022100060620     D  rpyEsito                     10I 0 OPTIONS(*OMIT:*NOPASS)
022200060620
022300060620     D*--------------------------------------------------
022400060620     D* Procedure name: chkKut
022500060620     D* Purpose:
022600060620     D* Returns:        Lingua
022700060620     D* Parameter:      rqsKut => Lingua.
022800060620     D*--------------------------------------------------
022900060620     D chkKut          PR             1P 0
023000060620     D  rqsKut                        1P 0
023100060303
023200060621     D*--------------------------------------------------
023300060621     D* Procedure name: getTabel00fLibrary
023400060621     D* Purpose:        Restituisce la libreria di TABEL00F
023500060621     D* Returns:        Libreria
023600060621     D* Parameter:      rpyOpCode => Codice risposta.
023700060621     D* Parameter:      rpyEsito => Esito
023800060621     D* Parameter:      rpyLibrary => Libreria
023900060621     D*--------------------------------------------------
024000060621     D getTabel00fLibrary...
024100060621     D                 PR            10A
024200060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
024300060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
024400060621     D  rpyLibrary                   10A   OPTIONS(*NOPASS:*OMIT)
024500060621
024600060621     D*--------------------------------------------------
024700060621     D* Procedure name: setTabel00fLibrary
024800060621     D* Purpose:        Imposta la libreria di TABEL00F
024900060621     D* Returns:
025000060621     D* Parameter:      rqsLibrary => Libreria
025100060621     D* Parameter:      rpyOpCode => Codice risposta
025200060621     D* Parameter:      rpyEsito => Esito
025300060621     D*--------------------------------------------------
025400060621     D setTabel00fLibrary...
025500060621     D                 PR
025600060621     D  rqsLibrary                   10A   CONST
025700060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
025800060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
025900060621
026000060530      //********************************************************************************************
026100060530      //
026200060530      // Definizione strutture dati.
026300060530      //
026400060530      //********************************************************************************************
026500060531     D fileFbk         DS
026600060620     D  libraryFbk            93    102A
026700060626     D rpyTabel00f     DS                  LIKEREC(tabel)
026800060530
026900060530      //********************************************************************************************
027000060530      //
027100060619      // Definizione variabili globali.
027200060530      //
027300060530      //********************************************************************************************
027400060620     D rqsOpCode       S             10A
027500060621     D rpyOpCode       S             10A
027600060621     D rpyEsito        S             10I 0
027700060622     D rqsKut          S                   LIKE(tblKut)
027800060622     D rqsCod          S                   LIKE(tblCod)
027900060622     D rqsKey          S                   LIKE(tblKey)
028000060626     D rqsFormat       S             10A
028100060626     D rpyData         S                   LIKE(rpyTabel00f)
028200060621     D rqsOrderBy      S            256A
028300060620     D rqsLibrary      S             10A
028400060620     D rpyLibrary      S             10A
028500060621     D library         S             10A   VARYING
028600060621     D fileName        S             21A   VARYING INZ('*LIBL/TABEL00F')
028700060622     D keyKut          S                   LIKE(tblKut)
028800060620
028900060620      //********************************************************************************************
029000060620      //
029100060620      // Definizione parametri programma.
029200060620      //
029300060620      //********************************************************************************************
029400060620     C     *ENTRY        PLIST
029500060620     C                   PARM                    rqsOpCode                      1
029600060620     C                   PARM                    rpyOpCode                      2
029700060620     C                   PARM                    rpyEsito                       3
029800060620     C                   PARM                    rqsKut                         4
029900060620     C                   PARM                    rqsCod                         5
030000060620     C                   PARM                    rqsKey                         6
030100060626     C                   PARM                    rqsFormat                      7
030200060626     C                   PARM                    rpyData                        8
030300060626     C                   PARM                    rqsOrderBy                     9
030400060626     C                   PARM                    rqsLibrary                     10
030500060626     C                   PARM                    rpyLibrary                     11
030600060620      /FREE
030700060621
030800060621       SELECT;
030900060621         WHEN %SUBST(rqsOpCode:1:5) = 'CHAIN';
031000060626           chainTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rqsFormat
031100060626           :rpyOpCode:rpyEsito:rpyData);
031200060621         WHEN %SUBST(rqsOpCode:1:5) = 'FETCH';
031300060626           fetchTabel00f(rqsOpCode:rqsFormat:rpyOpCode:rpyEsito:rpyData);
031400060621         WHEN %SUBST(rqsOpCode:1:4) = 'READ';
031500060626           readTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rqsFormat
031600060626           :rpyOpCode:rpyEsito:rpyData);
031700060621         WHEN %SUBST(rqsOpCode:1:5) = 'SETLL'
031800060621         OR %SUBST(rqsOpCode:1:5) = 'SETGT';
031900060621           setTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey):rpyOpCode
032000060621           :rpyEsito);
032100060626         WHEN %SUBST(rqsOpCode:1:7) = 'SQLOPEN';
032200060621           openSqlTabel00f(rqsOpCode:rqsKut:rqsCod:rqsKey:%LEN(rqsKey)
032300060621           :rpyOpCode:rpyEsito:rqsOrderBy:%LEN(rqsOrderBy));
032400060626         WHEN rqsOpCode = 'SQLCLOSE';
032500060621           closeSqlTabel00f(rpyOpCode:rpyEsito);
032600060621         WHEN rqsOpCode = 'OPEN';
032700060621           openTabel00f(rpyOpCode:rpyEsito);
032800060621         WHEN rqsOpCode = 'CLOSE';
032900060621           closeTabel00f(rpyOpCode:rpyEsito);
033000060621         WHEN rqsOpCode = 'SETONLR';
033100060621           *INLR = *ON;
033200060621           rpyOpCode = 'DONE';
033300060621         WHEN rqsOpCode = 'SETLIBRARY';
033400060621           setTabel00fLibrary(rqsLibrary:rpyOpCode:rpyEsito);
033500060621         WHEN rqsOpCode = 'GETLIBRARY';
033600060621           getTabel00fLibrary(rpyOpCode:rpyEsito:rpyLibrary);
033700060621         OTHER;
033800060621           rpyOpCode = 'ERROR';
033900060621           rpyEsito = -1;
034000060621       ENDSL;
034100060621
034200060620       RETURN;
034300060620
034400060620      /END-FREE
034500060619
034600060619
034700060619     P*--------------------------------------------------
034800060621     P* Procedure name: openTabel00f
034900060621     P* Purpose:        OPEN TABEL00F
035000060619     P* Returns:
035100060619     P* Parameter:      rqsLibrary => Libreria
035200060619     P* Parameter:      rpyLibrary => Libreria dove risiede TABEL00F
035300060619     P* Parameter:      rpyOpCode => Codice risposta.
035400060619     P* Parameter:      rpyEsito => Esito.
035500060619     P*--------------------------------------------------
035600060621     P openTabel00f    B                   EXPORT
035700060621     D openTabel00f    PI
035800060619     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
035900060619     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
036000060619
036100060619      /FREE
036200060619
036300060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
036400060619         rpyOpCode = 'DONE';
036500060619       ENDIF;
036600060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
036700060619         CLEAR rpyEsito;
036800060619       ENDIF;
036900060619
037000060619       IF NOT %OPEN(tabel00f);
037100060621         IF library = *BLANK;
037200060620           RESET fileName;
037300060619         ELSE;
037400060621           fileName = library + '/TABEL00F';
037500060619         ENDIF;
037600060619         OPEN(E) tabel00f;
037700060619         IF %ERROR;
037800060621           IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
037900060619             rpyOpCode = 'ERROR';
038000060619           ENDIF;
038100060621           IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
038200060619             rpyEsito = %STATUS;
038300060619           ENDIF;
038400060619         ELSE;
038500060621           library = libraryFbk;
038600060619         ENDIF;
038700060619       ENDIF;
038800060619
038900060619      /END-FREE
039000060621     P openTabel00f    E
039100060619
039200060619
039300060619     P*--------------------------------------------------
039400060621     P* Procedure name: closeTabel00F
039500060619     P* Purpose:        CLOSE TABEL00F
039600060619     P* Returns:
039700060619     P* Parameter:      rpyOpCode => Codice risposta.
039800060619     P* Parameter:      rpyEsito => Esito
039900060619     P*--------------------------------------------------
040000060621     P closeTabel00f   B                   EXPORT
040100060621     D closeTabel00f   PI
040200060619     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
040300060619     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
040400060619
040500060619      /FREE
040600060619
040700060619       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
040800060619         rpyOpCode = 'DONE';
040900060619       ENDIF;
041000060619       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
041100060619         CLEAR rpyEsito;
041200060619       ENDIF;
041300060619
041400060619       IF %OPEN(tabel00f);
041500060619         CLOSE(E) tabel00f;
041600060619         IF %ERROR;
041700060619           IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
041800060619             rpyOpCode = 'ERROR';
041900060619           ENDIF;
042000060619           IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
042100060619             rpyEsito = %STATUS;
042200060619           ENDIF;
042300060619         ENDIF;
042400060619       ENDIF;
042500060619
042600060619      /END-FREE
042700060621     P closeTabel00f   E
042800060619
042900060619
043000060619     P*--------------------------------------------------
043100060621     P* Procedure name: chainTabel00f
043200060619     P* Purpose:        CHAIN TABEL00F
043300060619     P* Returns:
043400060619     P* Parameter:      rqsKut => Lingua
043500060619     P* Parameter:      rqsCod => Codice tabella
043600060619     P* Parameter:      rqsKey => Codice elemento.
043700060619     P* Parameter:      rpyOpCode => Codice risposta.
043800060619     P* Parameter:      rpyEsito => Esito
043900060619     P* Parameter:      rpyTabel00f
044000060619     P*--------------------------------------------------
044100060621     P chainTabel00f   B                   EXPORT
044200060626     D chainTabel00f   PI                  LIKE(rpyTabel00f)
044300060621     D  rqsOpCode                    10A   CONST
044400060619     D  rqsKut                        1P 0
044500060621     D  rqsCod                        2A   CONST
044600060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
044700060620     D  rqsLengthOfKey...
044800060621     D                               10I 0 CONST
044900060626     D  rqsFormat                    10A   CONST
045000060621     D  rpyOpCode                    10A
045100060621     D  rpyEsito                     10I 0
045200060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
045300060626     D                                     LIKE(rpyTabel00f)
045400060620
045500060626     D rtnData         S                   LIKE(rpyTabel00f)
045600060619      /FREE
045700060619
045800060619       CLEAR rpyOpCode;
045900060619       CLEAR rpyEsito;
046000060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
046100060626         CLEAR rpyData;
046200060626       ENDIF;
046300060626       CLEAR rpyTabel00f;
046400060622       keyKut = chkKut(rqsKut);
046500060619
046600060619       DOU %FOUND;
046700060619         SELECT;
046800060619           WHEN rqsOpCode = 'CHAIN3';
046900060622             CHAIN(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
047000060626             rpyTabel00f;
047100060619           WHEN rqsOpCode = 'CHAIN2';
047200060626             CHAIN(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
047300060619           WHEN rqsOpCode = 'CHAIN1';
047400060626             CHAIN(E) (keyKut) tabel00f rpyTabel00f;
047500060620           OTHER;
047600060620             rpyOpCode = 'ERROR';
047700060620             rpyEsito = -1;
047800060626             SELECT;
047900060626               WHEN rqsFormat = 'TABEL';
048000060626                 rtnData = rpyTabel00f;
048100060626               WHEN rqsFormat = 'TBLUNI';
048200060626                 rtnData = rpyTabel00f.tblUni;
048300060626               OTHER;
048400060626                 CLEAR rtnData;
048500060626             ENDSL;
048600060626             RETURN rtnData;
048700060619         ENDSL;
048800060619         IF %ERROR;
048900060619           rpyOpCode = 'ERROR';
049000060619           rpyEsito = %STATUS;
049100060626           SELECT;
049200060626             WHEN rqsFormat = 'TABEL';
049300060626               rtnData = rpyTabel00f;
049400060626             WHEN rqsFormat = 'TBLUNI';
049500060626               rtnData = rpyTabel00f.tblUni;
049600060626             OTHER;
049700060626               CLEAR rtnData;
049800060626           ENDSL;
049900060626           RETURN rtnData;
050000060626         ENDIF;
050100060622         IF %FOUND OR keyKut = Italiano;
050200060619           LEAVE;
050300060619         ENDIF;
050400060622         keyKut = Italiano;
050500060619       ENDDO;
050600060619
050700060619       IF %FOUND;
050800060619         rpyOpCode = 'FOUND';
050900060619       ELSE;
051000060619         rpyOpCode = 'NOTFOUND';
051100060619       ENDIF;
051200060619
051300060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
051400060626         SELECT;
051500060626           WHEN rqsFormat = 'TABEL';
051600060626             rpyData = rpyTabel00f;
051700060626           WHEN rqsFormat = 'TBLUNI';
051800060626             rpyData = rpyTabel00f.tblUni;
051900060626           OTHER;
052000060626             CLEAR rpyData;
052100060626         ENDSL;
052200060620       ENDIF;
052300060620
052400060626       SELECT;
052500060626         WHEN rqsFormat = 'TABEL';
052600060626           rtnData = rpyTabel00f;
052700060626         WHEN rqsFormat = 'TBLUNI';
052800060626           rtnData = rpyTabel00f.tblUni;
052900060626         OTHER;
053000060626           CLEAR rtnData;
053100060626       ENDSL;
053200060626
053300060626       RETURN rtnData;
053400060626
053500060619      /END-FREE
053600060621     P chainTabel00f   E
053700060619
053800060619
053900060619     P*--------------------------------------------------
054000060621     P* Procedure name: setTabel00f
054100060620     P* Purpose:        SETXX TABEL00F
054200060619     P* Returns:
054300060619     P* Parameter:      rqsKut => Lingua
054400060619     P* Parameter:      rqsCod => Codice tabella
054500060619     P* Parameter:      rqsKey => Codice elemento.
054600060619     P* Parameter:      rpyOpCode => Codice risposta.
054700060619     P* Parameter:      rpyEsito => Esito
054800060619     P*--------------------------------------------------
054900060621     P setTabel00f     B                   EXPORT
055000060621     D setTabel00f     PI
055100060621     D  rqsOpCode                    10A   CONST
055200060619     D  rqsKut                        1P 0
055300060621     D  rqsCod                        2A   CONST
055400060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
055500060620     D  rqsLengthOfKey...
055600060621     D                               10I 0 CONST
055700060619     D  rpyOpCode                    10A
055800060619     D  rpyEsito                     10I 0
055900060619
056000060619      /FREE
056100060619
056200060619       CLEAR rpyOpCode;
056300060619       CLEAR rpyEsito;
056400060622       keyKut = chkKut(rqsKut);
056500060619
056600060619       DOU %FOUND;
056700060619         SELECT;
056800060619           WHEN rqsOpCode = 'SETLL2';
056900060622             SETLL(E) (keyKut:rqsCod) tabel00f;
057000060619           WHEN rqsOpCode = 'SETLL1';
057100060622             SETLL(E) (keyKut) tabel00f;
057200060619           WHEN rqsOpCode = 'SETLL3';
057300060622             SETLL(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f;
057400060620           WHEN rqsOpCode = 'SETGT2';
057500060622             SETGT(E) (keyKut:rqsCod) tabel00f;
057600060620           WHEN rqsOpCode = 'SETGT1';
057700060622             SETGT(E) (keyKut) tabel00f;
057800060620           WHEN rqsOpCode = 'SETGT3';
057900060622             SETGT(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f;
058000060620           OTHER;
058100060620             rpyOpCode = 'ERROR';
058200060620             rpyEsito = -1;
058300060620             RETURN;
058400060619         ENDSL;
058500060619         IF %ERROR;
058600060619           rpyOpCode = 'ERROR';
058700060619           rpyEsito = %STATUS;
058800060619           RETURN;
058900060619         ENDIF;
059000060622         IF %FOUND OR keyKut = Italiano;
059100060619           LEAVE;
059200060619         ENDIF;
059300060622         keyKut = Italiano;
059400060619       ENDDO;
059500060619
059600060619       SELECT;
059700060619         WHEN %EQUAL;
059800060619           rpyOpCode = 'EQUAL';
059900060619         WHEN %FOUND;
060000060619           rpyOpCode = 'FOUND';
060100060619         OTHER;
060200060619           rpyOpCode = 'NOTFOUND';
060300060619       ENDSL;
060400060619
060500060619      /END-FREE
060600060621     P setTabel00f     E
060700060619
060800060619
060900060619     P*--------------------------------------------------
061000060621     P* Procedure name: readTabel00f
061100060620     P* Purpose:        READXX TABEL00F
061200060619     P* Returns:
061300060619     D* Parameter:      rqsOpCode => Codice operativo
061400060619     P* Parameter:      rqsKut => Lingua
061500060619     P* Parameter:      rqsCod => Codice tabella
061600060619     P* Parameter:      rqsKey => Codice elemento.
061700060619     P* Parameter:      rpyOpCode => Codice risposta.
061800060619     P* Parameter:      rpyEsito => Esito
061900060619     P* Parameter:      rpyTabel00f
062000060619     P*--------------------------------------------------
062100060621     P readTabel00f    B                   EXPORT
062200060626     D readTabel00f    PI                  LIKE(rpyTabel00f)
062300060621     D  rqsOpCode                    10A   CONST
062400060619     D  rqsKut                        1P 0
062500060621     D  rqsCod                        2A   CONST
062600060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
062700060620     D  rqsLengthOfKey...
062800060621     D                               10I 0 CONST
062900060626     D  rqsFormat                    10A   CONST
063000060619     D  rpyOpCode                    10A
063100060619     D  rpyEsito                     10I 0
063200060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
063300060626     D                                     LIKE(rpyTabel00f)
063400060620
063500060626     D rtnData         S                   LIKE(rpyTabel00f)
063600060619      /FREE
063700060619
063800060626       CLEAR rpyTabel00f;
063900060619       CLEAR rpyOpCode;
064000060619       CLEAR rpyEsito;
064100060620
064200060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
064300060626         CLEAR rpyData;
064400060620       ENDIF;
064500060620
064600060622       keyKut = chkKut(rqsKut);
064700060619
064800060619       SELECT;
064900060619         WHEN rqsOpCode = 'READE2';
065000060626           READE(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
065100060619         WHEN rqsOpCode = 'READE1';
065200060626           READE(E) (keyKut) tabel00f rpyTabel00f;
065300060619         WHEN rqsOpCode = 'READE3';
065400060622           READE(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
065500060626           rpyTabel00f;
065600060620         WHEN rqsOpCode = 'READ';
065700060626           READ(E) tabel00f rpyTabel00f;
065800060620         WHEN rqsOpCode = 'READPE2';
065900060626           READPE(E) (keyKut:rqsCod) tabel00f rpyTabel00f;
066000060620         WHEN rqsOpCode = 'READPE1';
066100060626           READPE(E) (keyKut) tabel00f rpyTabel00f;
066200060620         WHEN rqsOpCode = 'READPE3';
066300060622           READPE(E) (keyKut:rqsCod:%SUBST(rqsKey:1:rqsLengthOfKey)) tabel00f
066400060626           rpyTabel00f;
066500060620         WHEN rqsOpCode = 'READP';
066600060626           READP(E) tabel00f rpyTabel00f;
066700060620         OTHER;
066800060620           rpyOpCode = 'ERROR';
066900060620           rpyEsito = -1;
067000060626           SELECT;
067100060626             WHEN rqsFormat = 'TABEL';
067200060626               rtnData = rpyTabel00f;
067300060626             WHEN rqsFormat = 'TBLUNI';
067400060626               rtnData = rpyTabel00f.tblUni;
067500060626             OTHER;
067600060626               CLEAR rtnData;
067700060626           ENDSL;
067800060626           RETURN rtnData;
067900060626       ENDSL;
068000060619
068100060619       IF %EOF;
068200060619         rpyOpCode = 'EOF';
068300060619       ELSE;
068400060619         rpyOpCode = 'NOTEOF';
068500060619       ENDIF;
068600060620
068700060626       IF %PARMS > 8 AND %ADDR(rpyData) <> *NULL;
068800060626         SELECT;
068900060626           WHEN rqsFormat = 'TABEL';
069000060626             rpyData = rpyTabel00f;
069100060626           WHEN rqsFormat = 'TBLUNI';
069200060626             rpyData = rpyTabel00f.tblUni;
069300060626           OTHER;
069400060626             rpyData = *BLANK;
069500060626         ENDSL;
069600060620       ENDIF;
069700060619
069800060626       SELECT;
069900060626         WHEN rqsFormat = 'TABEL';
070000060626           rtnData = rpyTabel00f;
070100060626         WHEN rqsFormat = 'TBLUNI';
070200060626           rtnData = rpyTabel00f.tblUni;
070300060626         OTHER;
070400060626           CLEAR rtnData;
070500060626       ENDSL;
070600060626
070700060626       RETURN rtnData;
070800060626
070900060619      /END-FREE
071000060621     P readTabel00f    E
071100060619
071200060619
071300060619     P*--------------------------------------------------
071400060620     P* Procedure name: openSqlTabel
071500060619     P* Purpose:        PREPARE e OPEN cursore TABEL00F
071600060619     P* Returns:
071700060619     D* Parameter:      rqsOpCode => Codice operativo
071800060619     P* Parameter:      rqsKut => Lingua
071900060619     P* Parameter:      rqsCod => Codice tabella
072000060619     P* Parameter:      rqsKey => Codice elemento.
072100060619     P* Parameter:      rpyOpCode => Codice risposta.
072200060619     P* Parameter:      rpyEsito => Esito
072300060619     P*--------------------------------------------------
072400060621     P openSqlTabel00f...
072500060621     P                 B                   EXPORT
072600060621     D openSqlTabel00f...
072700060621     D                 PI
072800060621     D  rqsOpCode                    10A   CONST
072900060619     D  rqsKut                        1P 0
073000060621     D  rqsCod                        2A   CONST
073100060621     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
073200060620     D  rqsLengthOfKey...
073300060621     D                               10I 0 CONST
073400060619     D  rpyOpCode                    10A
073500060619     D  rpyEsito                     10I 0
073600060620     D  rqsOrderBy                  256A   OPTIONS(*NOPASS:*OMIT:*VARSIZE)
073700060621     D  rqsLengthOfOrderBy...
073800060621     D                               10I 0 OPTIONS(*NOPASS:*OMIT) CONST
073900060619
074000060620     D tableName       S             21A
074100060620     D                                     VARYING
074200060620     D                                     INZ('TABEL00F')
074300060619     D stmSql          S            512A
074400060619     D                                     VARYING
074500060620     D orderBy         S            256A
074600060620     D                                     VARYING
074700060620     D                                     INZ('TBLCOD, TBLKEY')
074800060619
074900060620     C/EXEC SQL
075000060620     C+ SET OPTION DYNUSRPRF = *OWNER
075100060620     C/END-EXEC
075200060620
075300060619      /FREE
075400060619
075500060621       closeSqlTabel00f();
075600060622       keyKut = chkKut(rqsKut);
075700060620
075800060621       IF %PARMS > 7 AND %ADDR(rqsOrderBy) <> *NULL AND rqsOrderBy <> *BLANK;
075900060621         orderBy = %TRIMR(%SUBST(rqsOrderBy:rqsLengthOfOrderBy));
076000060620       ENDIF;
076100060619
076200060621       IF library = *BLANK;
076300060620         RESET tableName;
076400060619       ELSE;
076500060621         tableName = library + '/TABEL00F';
076600060619       ENDIF;
076700060619
076800060620       stmSql = 'SELECT * FROM ' + tableName;
076900060622       stmSql += ' WHERE TBLKUT = ' + %CHAR(keyKut);
077000060619
077100060619       SELECT;
077200060626         WHEN rqsOpCode = 'SQLOPEN2';
077300060619           stmSql += ' AND TBLCOD = ' + '''' + rqsCod + '''';
077400060626         WHEN rqsOpCode = 'SQLOPEN3';
077500060619           stmSql += ' AND TBLCOD = ' + '''' + rqsCod + ''''
077600060620           + ' AND TBLKEY >= ' + '''' + %SUBST(rqsKey:1:rqsLengthOfKey) + '''';
077700060619       ENDSL;
077800060619
077900060620       stmSql += ' UNION SELECT A.* FROM ' + tableName + ' AS A +
078000060622       EXCEPTION JOIN ' + tableName + ' AS B ON ' + %CHAR(keyKut) +
078100060619       ' = B.TBLKUT AND A.TBLCOD = B.TBLCOD AND A.TBLKEY = B.TBLKEY +
078200060619       WHERE A.TBLKUT = 1';
078300060619
078400060619       SELECT;
078500060626         WHEN rqsOpCode = 'SQLOPEN2';
078600060619           stmSql += ' AND A.TBLCOD = ' + '''' + rqsCod + '''';
078700060626         WHEN rqsOpCode = 'SQLOPEN3';
078800060619           stmSql += ' AND A.TBLCOD = ' + '''' + rqsCod + ''''
078900060620           + ' AND A.TBLKEY >= ' + ''' + %SUBST(rqsKey:1:rqsLengthOfKey) + ''';
079000060619       ENDSL;
079100060619
079200060620       stmSql += ' ORDER BY ' + orderBy;
079300060619
079400060619      /END-FREE
079500060619     C/EXEC SQL
079600060619     C+ PREPARE TABEL00F FROM :stmSql
079700060619     C/END-EXEC
079800060619     C
079900060619     C/EXEC SQL
080000060620     C+ DECLARE TABEL00F SCROLL CURSOR FOR TABEL00F
080100060619     C/END-EXEC
080200060619     C
080300060619     C/EXEC SQL
080400060619     C+ OPEN TABEL00F
080500060619     C/END-EXEC
080600060619      /FREE
080700060619
080800060619       rpyOpCode = 'SQLCOD';
080900060619       rpyEsito = sqlCod;
081000060619
081100060619      /END-FREE
081200060621     P openSqlTabel00f...
081300060621     P                 E
081400060619
081500060619
081600060619     P*--------------------------------------------------
081700060620     P* Procedure name: fetchTabel
081800060619     P* Purpose:        FETCH cursore TABEL00F
081900060619     P* Returns:
082000060619     P*--------------------------------------------------
082100060621     P fetchTabel00f   B                   EXPORT
082200060626     D fetchTabel00f   PI                  LIKE(rpyTabel00f)
082300060626     D  rqsFormat                    10A   CONST
082400060621     D  rqsOpCode                    10A   CONST
082500060619     D  rpyOpCode                    10A
082600060619     D  rpyEsito                     10I 0
082700060626     D  rpyData                            OPTIONS(*NOPASS:*OMIT)
082800060626     D                                     LIKE(rpyTabel00f)
082900060620
083000060626     D rtnData         S                   LIKE(rpyTabel00f)
083100060619      /FREE
083200060619
083300060626       IF %PARMS > 4 AND %ADDR(rpyData) <> *NULL;
083400060626         CLEAR rpyData;
083500060626       ENDIF;
083600060626
083700060626       CLEAR rpyTabel00f;
083800060619
083900060619       SELECT;
084000060619         WHEN rqsOpCode = 'FETCHNEXT';
084100060619      /END-FREE
084200060619     C/EXEC SQL
084300060620     C+ FETCH NEXT FROM TABEL00F
084400060626     C+ INTO :rpyTabel00f
084500060619     C/END-EXEC
084600060619      /FREE
084700060620         WHEN rqsOpCode = 'FETCHPRIOR';
084800060620      /END-FREE
084900060620     C/EXEC SQL
085000060620     C+ FETCH PRIOR FROM TABEL00F
085100060626     C+ INTO :rpyTabel00f
085200060620     C/END-EXEC
085300060620      /FREE
085400060620         WHEN rqsOpCode = 'FETCHFIRST';
085500060620      /END-FREE
085600060620     C/EXEC SQL
085700060620     C+ FETCH FIRST FROM TABEL00F
085800060626     C+ INTO :rpyTabel00f
085900060620     C/END-EXEC
086000060620      /FREE
086100060620         WHEN rqsOpCode = 'FETCHLAST';
086200060620      /END-FREE
086300060620     C/EXEC SQL
086400060620     C+ FETCH LAST FROM TABEL00F
086500060626     C+ INTO :rpyTabel00f
086600060620     C/END-EXEC
086700060620      /FREE
086800060620         WHEN rqsOpCode = 'FETCHBEFOR';
086900060620      /END-FREE
087000060620     C/EXEC SQL
087100060620     C+ FETCH BEFORE FROM TABEL00F
087200060620     C/END-EXEC
087300060620      /FREE
087400060620         WHEN rqsOpCode = 'FETCHAFTER';
087500060620      /END-FREE
087600060620     C/EXEC SQL
087700060620     C+ FETCH AFTER FROM TABEL00F
087800060620     C/END-EXEC
087900060620      /FREE
088000060619         OTHER;
088100060619           rpyOpCode = 'ERROR';
088200060619           rpyEsito = -1;
088300060626           SELECT;
088400060626             WHEN rqsFormat = 'TABEL';
088500060626               rtnData = rpyTabel00f;
088600060626             WHEN rqsFormat = 'TBLUNI';
088700060626               rtnData = rpyTabel00f.tblUni;
088800060626             OTHER;
088900060626               CLEAR rtnData;
089000060626           ENDSL;
089100060626           RETURN rtnData;
089200060626       ENDSL;
089300060619
089400060619       rpyOpCode = 'SQLCOD';
089500060619       rpyEsito = sqlCod;
089600060620
089700060626       IF %PARMS > 4 AND %ADDR(rpyData) <> *NULL;
089800060626         rpyData = rpyTabel00f;
089900060620       ENDIF;
090000060620
090100060626       SELECT;
090200060626         WHEN rqsFormat = 'TABEL';
090300060626           rtnData = rpyTabel00f;
090400060626         WHEN rqsFormat = 'TBLUNI';
090500060626           rtnData = rpyTabel00f.tblUni;
090600060626         OTHER;
090700060626           CLEAR rtnData;
090800060626       ENDSL;
090900060626
091000060626       RETURN rtnData;
091100060626
091200060619      /END-FREE
091300060621     P fetchTabel00f   E
091400060619
091500060619
091600060619     P*--------------------------------------------------
091700060620     P* Procedure name: closeSqlTabel
091800060619     P* Purpose:        CLOSE cursore TABEL00F
091900060619     P* Returns:
092000060619     P*--------------------------------------------------
092100060621     P closeSqlTabel00f...
092200060621     P                 B                   EXPORT
092300060621     D closeSqlTabel00f...
092400060621     D                 PI
092500060620     D  rpyOpCode                    10A   OPTIONS(*OMIT:*NOPASS)
092600060620     D  rpyEsito                     10I 0 OPTIONS(*OMIT:*NOPASS)
092700060619
092800060619
092900060619     C/EXEC SQL
093000060619     C+ CLOSE TABEL00F
093100060619     C/END-EXEC
093200060619
093300060619      /FREE
093400060619
093500060620       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
093600060620         rpyOpCode = 'SQLCOD';
093700060620       ENDIF;
093800060620       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
093900060620         rpyEsito = sqlCod;
094000060620       ENDIF;
094100060619
094200060619      /END-FREE
094300060621     P closeSqlTabel00f...
094400060621     P                 E
094500060619
094600060619     P*--------------------------------------------------
094700060619     P* Procedure name: chkKut
094800060619     P* Purpose:
094900060619     P* Returns:        Lingua
095000060619     P* Parameter:      rqsKut => Lingua.
095100060619     P*--------------------------------------------------
095200060619     P chkKut          B
095300060619     D chkKut          PI             1P 0
095400060620     D  rqsKut                        1P 0
095500060619
095600060619     D rpyKut          S              1P 0
095700060619
095800060619      /FREE
095900060620
096000060619       IF rqsKut = Italiano OR rqsKut = Inglese
096100060619       OR rqsKut = Francese OR rqsKut = Tedesco;
096200060619         rpyKut = rqsKut;
096300060619       ELSE;
096400060619         rpyKut = Italiano;
096500060619       ENDIF;
096600060619
096700060619       RETURN rpyKut;
096800060619
096900060619      /END-FREE
097000060619     P chkKut          E
097100060621
097200060621
097300060621     P*--------------------------------------------------
097400060621     P* Procedure name: setTabel00fLibrary
097500060621     P* Purpose:        Imposta la libreria di TABEL00F
097600060621     P* Returns:
097700060621     P* Parameter:      rqsLibrary => Libreria
097800060621     P* Parameter:      rpyOpCode => Codice risposta
097900060621     P* Parameter:      rpyEsito => Esito
098000060621     P*--------------------------------------------------
098100060621     P setTabel00fLibrary...
098200060621     P                 B                   EXPORT
098300060621     D setTabel00fLibrary...
098400060621     D                 PI
098500060621     D  rqsLibrary                   10A   CONST
098600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
098700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
098800060621
098900060621      /FREE
099000060621
099100060621       IF rqsLibrary = *BLANK OR rqsLibrary = '*LIBL';
099200060621         RESET library;
099300060621       ELSE;
099400060621         library = rqsLibrary;
099500060621       ENDIF;
099600060621
099700060621       IF %PARMS > 1 AND %ADDR(rpyOpCode) <> *NULL;
099800060621         rpyOpCode = 'DONE';
099900060621       ENDIF;
100000060621       IF %PARMS > 2 AND %ADDR(rpyEsito) <> *NULL;
100100060621         rpyEsito = 0;
100200060621       ENDIF;
100300060621
100400060621      /END-FREE
100500060621     P setTabel00fLibrary...
100600060621     P                 E
100700060621
100800060621
100900060621     P*--------------------------------------------------
101000060621     P* Procedure name: getTabel00fLibrary
101100060621     P* Purpose:        Restituisce la libreria di TABEL00F
101200060621     P* Returns:        Libreria
101300060621     P* Parameter:      rpyOpCode => Codice risposta.
101400060621     P* Parameter:      rpyEsito => Esito
101500060621     P* Parameter:      rpyLibrary => Libreria
101600060621     P*--------------------------------------------------
101700060621     P getTabel00fLibrary...
101800060621     P                 B                   EXPORT
101900060621     D getTabel00fLibrary...
102000060621     D                 PI            10A
102100060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
102200060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
102300060621     D  rpyLibrary                   10A   OPTIONS(*NOPASS:*OMIT)
102400060621
102500060621      /FREE
102600060621
102700060621       IF %PARMS > 0 AND %ADDR(rpyOpCode) <> *NULL;
102800060621         rpyOpCode = 'DONE';
102900060621       ENDIF;
103000060621       IF %PARMS > 1 AND %ADDR(rpyEsito) <> *NULL;
103100060621         rpyEsito = 0;
103200060621       ENDIF;
103300060621       IF %PARMS > 2 AND %ADDR(rpyLibrary) <> *NULL;
103400060621         rpyLibrary = library;
103500060621       ENDIF;
103600060621
103700060621       RETURN library;
103800060621
103900060621      /END-FREE
104000060621     P getTabel00fLibrary...
104100060621     P                 E
104200060621
