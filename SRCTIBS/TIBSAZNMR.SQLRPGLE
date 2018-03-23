000100080320     H NOMAIN BNDDIR('TRUL')
000200060906
000300060906      //********************************************************************************************
000400060906      //
000500060906      // Definizione database.
000600060906      //
000700060906      //********************************************************************************************
000800060907     Faznmrr01l UF A E           K DISK    USROPN PREFIX(r) COMMIT(fileCommit)
000900060907     F                                     EXTFILE(fileNameR) INFDS(infAznmrr)
001000060907     Faznmrn01l UF A E           K DISK    USROPN PREFIX(n) COMMIT(fileCommit)
001100060907     F                                     EXTFILE(fileNameN) INFDS(infAznmrn)
001200060906
001300060906      //********************************************************************************************
001400060906      //
001500060906      // Definizione costanti.
001600060906      //
001700060906      //********************************************************************************************
001800080320     D ADD             C                   'A'
001900080320     D REPLACE         C                   'R'
002000060906     D Giorno          C                   'G'
002100060906     D Settimana       C                   'S'
002200060906     D Mese            C                   'M'
002300060906     D AnnoSolare      C                   'A'
002400060906     D EsercizioCG     C                   'E'
002500060906     D Progressivo     C                   'P'
002600060906     D Assoluto        C                   'T'
002700060906     D DiGruppo        C                   'G'
002800060906     D DiSocieta       C                   'S'
002900060906     D POTutti         C                   '0'
003000060906     D POLivello1      C                   '1'
003100060906     D POLivello2      C                   '2'
003200060906     D Numerico        C                   'N'
003300060906     D Carattere       C                   'C'
003400060906     D SoloNumeri      C                   'N'
003500060906     D SoloLettere     C                   'L'
003600060906     D NumeriLettere   C                   'T'
003700060907     D CCSIDFormat     C                   1
003800060907     D CCSIDOfTheJob   C                   0
003900060907     D ToUppercase     C                   0
004000120302     D Dominio         C                   '@brt.it'
004100060913     D IgnoreDBCSData  C                   '0'
004200060913     D NeverPromptTheCommand...
004300060913     D                 C                   '0'
004400060913     D UseSystemSyntax...
004500060913     D                 C                   '0'
004600060907     D ErrGenerico     C                   -1
004700060907     D ErrCodiceObbligatorio...
004800060907     D                 C                   -2
004900060907     D ErrRegolaNonTrovata...
005000060907     D                 C                   -3
005100060908     D ErrNumeratoreNonTrovato...
005200060908     D                 C                   -4
005300060912     D ErrNuovoProgressivo...
005400060912     D                 C                   -5
005500080321     D ErrCapienzaBloccante...
005600060913     D                 C                   -6
005700060907     D ErrIdNumeratoreSocieta...
005800060907     D                 C                   -10
005900060907     D ErrIdNumeratoreUnita...
006000060907     D                 C                   -11
006100060907     D ErrIdNumeratoreChiaveGestionale...
006200060907     D                 C                   -12
006300060907     D ErrIdNumeratoreData...
006400060907     D                 C                   -13
006500060908     D ErrRecordAlreadyLocked...
006600060908     D                 C                   -1218
006700060907     D InfoIlFileNonEraAperto...
006800060907     D                 C                   1
006900060912     D InfoRaggiuntaSogliaRischio...
007000060912     D                 C                   2
007100080321     D ErrCapienzaNonBloccante...
007200080321     D                 C                   6
007300060911
007400060907      //********************************************************************************************
007500060907      //
007600080320      // Definizione procedure.
007700060907      //
007800060907      //********************************************************************************************
007900080320     D/COPY GAITRASRC/SRCPROTOPR,TRULEMAIL
008000080320     D/COPY GAITRASRC/SRCPROTOPR,TIBSAZNMR
008100060908     D ConvertCase     PR                  EXTPROC('QlgConvertCase')
008200060907     D  RequestControlBlock...
008300060907     D                                     LIKE(RequestControlBlockFormat)
008400060907     D  InputData                    10A
008500060907     D  OutputData                   10A
008600060907     D  LengthOfData                 10I 0 CONST
008700060907     D  ErrorCode                          LIKE(qusec)
008800060907     D getEsercizioProj...
008900060907     D                 PR                  EXTPGM('X10ESER')
009000060907     D  x10ese                             LIKEDS(x10eseds)
009100060906
009200060906      //********************************************************************************************
009300060906      //
009400060906      // Definizione strutture dati.
009500060906      //
009600060906      //********************************************************************************************
009700060907     D/COPY QSYSINC/QRPGLESRC,QUSEC
009800060911     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
009900060912     D psds           SDS
010000060912     D  jobName              244    253A
010100060912     D  jobUser              254    263A
010200060912     D  jobNumber            264    269A
010300080320     D aznmrr00f     E DS                  INZ QUALIFIED
010400080320     D aznmrn00f     E DS                  INZ QUALIFIED
010500060907     D x10eseds      E DS                  INZ
010600060907     D  x10Ctb       E                     INZ('CG')
010700060907     D RequestControlBlockFormat...
010800060907     D                 DS                  INZ
010900060907     D  TypeOfRequest                10I 0 INZ(CCSIDFormat)
011000060907     D  CCSIDOfInputData...
011100060907     D                               10I 0 INZ(CCSIDOfTheJob)
011200060907     D  CaseRequest                  10I 0 INZ(ToUppercase)
011300060907     D  Reserved                     10A   INZ(*ALLX'00')
011400060907     D infAznmrr       DS                  QUALIFIED
011500060907     D  library               93    102A
011600060907     D infAznmrn       DS                  QUALIFIED
011700060907     D  library               93    102A
011800060907
011900060907      //********************************************************************************************
012000060907      //
012100060907      // Definizione chiavi.
012200060907      //
012300060907      //********************************************************************************************
012400060908     D keyAznmrn01l    DS                  INZ LIKEREC(aznmrn000:*KEY)
012500060907
012600060907      //********************************************************************************************
012700060907      //
012800060907      // Definizione variabili globali.
012900060907      //
013000060907      //********************************************************************************************
013100060913     D libreria        S             10A
013200060907     D fileCommit      S              1A   INZ(*OFF)
013300060907     D fileNameR       S             21A   INZ('AZNMRR01L')
013400060907     D fileNameN       S             21A   INZ('AZNMRN01L')
013500060911     D SourceCommandString...
013600060911     D                 S          32702A
013700060911     D LengthOfChangedCommandStringAvailableToReturn...
013800060911     D                 S             10I 0
013900060912     D descrizione...
014000060912     D                 S             44A   VARYING
014100060912     D messaggioLungo...
014200060912     D                 S           5000A   VARYING
014300060912     D messageText...
014400060912     D                 S            494A   VARYING
014500060907
014600080320     D*--------------------------------------------------
014700080320     D* Procedure name: Aznmr_crtNumeratore
014800080320     D* Purpose:        Crea un nuovo numeratore secondo regola.
014900080320     D* Returns:        Esito
015000080320     D* Parameter:      rqsCodice => Codice numeratore
015100080320     D* Parameter:      rqsSocieta => Società
015200080320     D* Parameter:      rqsUnita => Unità
015300080320     D* Parameter:      rqsKeyGest => Chiave gestionale
015400080320     D* Parameter:      rqsDataPeriod => Data periodo.
015500080320     D* Parameter:      rpyEsito => Esito
015600080320     D*--------------------------------------------------
015700080320     D Aznmr_crtNumeratore...
015800080320     D                 PR            10I 0
015900080320     D  rqsCodice                          LIKE(nCodice)
016000080320     D  rqsSocieta                         LIKE(nSocieta)
016100080320     D  rqsUnita                           LIKE(nUnita)
016200080320     D  rqsKeyGest                         LIKE(nKeyGest)
016300080320     D  rqsDataPeriod                      LIKE(nDataPeriod)
016400080320     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
016500080320
016600080320     D*--------------------------------------------------
016700080320     D* Procedure name: Aznmr_inzNumeroN
016800080320     D* Purpose:        Inizializza il numero numerico secondo la regola.
016900080320     D* Returns:        Numero numerico.
017000080320     D* Parameter:      rqsValNInizio => Valore numerico iniziale.
017100080320     D* Parameter:      rqsValNFine => Valore numerico finale.
017200080320     D*--------------------------------------------------
017300080320     D Aznmr_inzNumeroN...
017400080320     D                 PR                  LIKE(nNumero)
017500080320     D  rqsValNInizio                      LIKE(nValNInizio)
017600080320     D  rqsValNFine                        LIKE(nValNFine)
017700080320
017800080320     D*--------------------------------------------------
017900080320     D* Procedure name: Aznmr_chkSogliaN
018000080320     D* Purpose:        Controlla se è stata raggiunta la soglia di rischio.
018100080320     D* Returns:        Esito.
018200080320     D* Parameter:      rpyEsito => Esito
018300080320     D*--------------------------------------------------
018400080320     D Aznmr_chkSogliaN...
018500080320     D                 PR            10I 0
018600080320     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
018700080320
018800080320     D*--------------------------------------------------
018900080320     D* Procedure name: Aznmr_nextProgressivoN
019000080320     D* Purpose:        Passa al progressivo successivo numerico.
019100080320     D* Returns:        Esito
019200080320     D* Parameter:      rqsCodice => Codice numeratore
019300080320     D* Parameter:      rqsSocieta => Società
019400080320     D* Parameter:      rqsUnita => Unità
019500080320     D* Parameter:      rqsKeyGest => Chiave gestionale
019600080320     D* Parameter:      rpyEsito => Esito
019700080320     D*--------------------------------------------------
019800080320     D Aznmr_nextProgressivoN...
019900080320     D                 PR            10I 0
020000080320     D  rqsCodice                          LIKE(nCodice)
020100080320     D  rqsSocieta                         LIKE(nSocieta)
020200080320     D  rqsUnita                           LIKE(nUnita)
020300080320     D  rqsKeyGest                         LIKE(nKeyGest)
020400080320     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
020500080320
020600080321     D*--------------------------------------------------
020700080321     D* Procedure name: Aznmr_readRegola
020800080321     D* Purpose:        Restituisce la regola di un numeratore.
020900080321     D* Returns:        Dati AZNMRR00F
021000080321     D* Parameter:      rqsCodice => Codice numeratore
021100080321     D* Parameter:      rpyEsito => Esito.
021200080321     D* Parameter:      rpyDatiAznmrr00f => Record di AZNMRR00F
021300080321     D*--------------------------------------------------
021400080321     D Aznmr_readRegola...
021500080321     D                 PR                  LIKEDS(aznmrr00f)
021600080321     D  rqsCodice                    10A   CONST
021700080321     D  rpyEsito                     10I 0
021800080321     D  rpyDatiAznmrr00f...
021900080321     D                                     LIKEDS(aznmrr00f)
022000080321     D                                     OPTIONS(*NOPASS:*OMIT)
022100080321
022200080321     D*--------------------------------------------------
022300080321     D* Procedure name: Aznmr_setCommit
022400080321     D* Purpose:        Imposta la variabile della parola chiave COMMIT.
022500080321     D* Returns:
022600080321     D* Parameter:      commit
022700080321     D*--------------------------------------------------
022800080321     DAznmr_setCommit  PR
022900080321     D  rqsCommit                     1A   CONST
023000080321
023100080321     D*--------------------------------------------------
023200080321     D* Procedure name: Aznmr_setLibreria
023300080321     D* Purpose:        Imposta la libreria del parametro EXTFILE.
023400080321     D* Returns:
023500080321     D* Parameter:      rqsLibreria => Libreria
023600080321     D*--------------------------------------------------
023700080321     D Aznmr_setLibreria...
023800080321     D                 PR
023900080321     D  rqsLibreria                  10A   CONST
024000080321
024100080321     D*--------------------------------------------------
024200080321     D* Procedure name: Aznmr_openAznmrr00f
024300080321     D* Purpose:        Apre AZNMRR00F
024400080321     D* Returns:        Esito
024500080321     D* Parameter:      rpyEsito => Esito
024600080321     D*--------------------------------------------------
024700080321     D Aznmr_openAznmrr00f...
024800080321     D                 PR            10I 0
024900080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
025000080321     D  rpyLibreria                  10A   OPTIONS(*NOPASS:*OMIT)
025100080321
025200080321     D*--------------------------------------------------
025300080321     D* Procedure name: Aznmr_closeAznmrr00f
025400080321     D* Purpose:        Chiude AZNMRR00F
025500080321     D* Returns:        Esito
025600080321     D* Parameter:      rpyEsito => Esito
025700080321     D*--------------------------------------------------
025800080321     D Aznmr_closeAznmrr00f...
025900080321     D                 PR            10I 0
026000080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
026100080321
026200080321     D*--------------------------------------------------
026300080321     D* Procedure name: Aznmr_openAznmrn00f
026400080321     D* Purpose:        Apre AZNMRN00F
026500080321     D* Returns:        Esito
026600080321     D* Parameter:      rpyEsito => Esito
026700080321     D*--------------------------------------------------
026800080321     D Aznmr_openAznmrn00f...
026900080321     D                 PR            10I 0
027000080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
027100080321     D  rpyLibreria                  10A   OPTIONS(*NOPASS:*OMIT)
027200080321
027300080321     D*--------------------------------------------------
027400080321     D* Procedure name: Aznmr_closeAznmrn00f
027500080321     D* Purpose:        Chiude AZNMRN00F
027600080321     D* Returns:        Esito
027700080321     D* Parameter:      rpyEsito => Esito
027800080321     D*--------------------------------------------------
027900080321     D Aznmr_closeAznmrn00f...
028000080321     D                 PR            10I 0
028100080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
028200080321
028300080321     D*--------------------------------------------------
028400080321     D* Procedure name: Aznmr_setIdNumeratore
028500080321     D* Purpose:        Imposta ID numeratore secondo regola.
028600080321     D* Returns:        Esito
028700080321     D* Parameter:      rqsCodice => Codice numeratore
028800080321     D* Parameter:      rqsSocieta => Società
028900080321     D* Parameter:      rqsUnita => Unità
029000080321     D* Parameter:      rqsKeyGest => Chiave gestionale
029100080321     D* Parameter:      rqsData => Data per determinare il periodo.
029200080321     D* Parameter:      rpyEsito => Esito
029300080321     D*--------------------------------------------------
029400080321     D Aznmr_setIdNumeratore...
029500080321     D                 PR            10I 0
029600080321     D  rqsCodice                    10A
029700080321     D                                     CONST
029800080321     D  rqsSocieta                    3A
029900080321     D                                     OPTIONS(*OMIT)
030000080321     D  rqsUnita                      8A
030100080321     D                                     OPTIONS(*OMIT)
030200080321     D  rqsKeyGest                   10A
030300080321     D                                     OPTIONS(*OMIT)
030400080321     D  rqsDataPeriod                  D
030500080321     D                                     OPTIONS(*OMIT)
030600080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
030700080321
030800080321     D*--------------------------------------------------
030900080321     D* Procedure name: Aznmr_sndMsg
031000080321     D* Purpose:        Invia un messaggio all' utente responsabile del numeratore.
031100080321     D* Returns:        Esito
031200080321     D* Parameter:      codice => Codice numeratore
031300080321     D* Parameter:      descrizione => Descrizione
031400080321     D* Parameter:      messaggio => Messaggio
031500080321     D* Parameter:      messaggioLungo => Messaggio lungo
031600080321     D* Parameter:      messaggeText => Testo messaggio per SNDMSG.
031700080321     D* Parameter:      esito => Esito
031800080321     D*--------------------------------------------------
031900080321     D Aznmr_sndMsg    PR            10I 0
032000080321     D  rqsCodice                    10A   CONST
032100080321     D  rqsDescrizione...
032200080321     D                               44A   VARYING
032300080321     D                                     CONST
032400080321     D                                     OPTIONS(*OMIT:*VARSIZE)
032500080321     D  rqsMessaggioLungo...
032600080321     D                             5000A   VARYING
032700080321     D                                     CONST
032800080321     D                                     OPTIONS(*OMIT:*VARSIZE)
032900080321     D  rqsMessaggio                256A   VARYING
033000080321     D                                     CONST
033100080321     D                                     OPTIONS(*NOPASS:*OMIT:*VARSIZE)
033200080321     D  rqsMessageText...
033300080321     D                              494A   VARYING
033400080321     D                                     CONST
033500080321     D                                     OPTIONS(*NOPASS:*OMIT:*VARSIZE)
033600080321     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
033700080321
033800080320     D*--------------------------------------------------
033900080321     D* Procedure name: Aznmr_init
034000060913     D* Purpose:        Inizializza il modulo.
034100060913     D* Returns:        Esito.
034200060913     D* Parameter:      rqsCommit => Aprire i file sotto controllo sincronia.
034300060913     D* Parameter:      rqsLibreria => Libreria per EXTFILE.
034400060913     D* Parameter:      rpyEsito => Esito
034500060913     D*--------------------------------------------------
034600080321     P Aznmr_init      B                   EXPORT
034700080321     D Aznmr_init      PI            10I 0
034800060913     D  rqsCommit                     1A   CONST
034900060913     D                                     OPTIONS(*NOPASS:*OMIT)
035000060913     D  rqsLibreria                  10A   CONST
035100060913     D                                     OPTIONS(*NOPASS:*OMIT)
035200060913     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
035300060913
035400060913     D rtnEsito        S             10I 0
035500060913
035600060913      /FREE
035700060913
035800060913       IF %PARMS > 3 AND %ADDR(rpyEsito) <> *NULL;
035900060913         CLEAR rpyEsito;
036000060913       ENDIF;
036100060913
036200060913       IF %PARMS > 0 AND %ADDR(rqsCommit) <> *NULL;
036300060913         Aznmr_setCommit(rqsCommit);
036400060913       ENDIF;
036500060913
036600060913       IF %PARMS > 1 AND %ADDR(rqsLibreria) <> *NULL AND rqsLibreria <> *BLANK;
036700060913         Aznmr_setLibreria(rqsLibreria);
036800060913       ENDIF;
036900060913
037000060913       Aznmr_openAznmrr00f();
037100060913       Aznmr_openAznmrn00f();
037200060913
037300060913       RETURN rtnEsito;
037400060913
037500060913      /END-FREE
037600080321     PAznmr_init       E
037700060913
037800060908
037900060907     P*--------------------------------------------------
038000060907     P* Procedure name: Aznmr_setCommit
038100060907     P* Purpose:        Imposta la variabile della parola chiave COMMIT.
038200060907     P* Returns:
038300060907     P* Parameter:      commit
038400060907     P*--------------------------------------------------
038500080321     PAznmr_setCommit  B
038600060907     DAznmr_setCommit  PI
038700060907     D  rqsCommit                     1A   CONST
038800080321
038900060907      /FREE
039000080321
039100060907       fileCommit = rqsCommit;
039200060907       RETURN;
039300080321
039400060907      /END-FREE
039500060907     PAznmr_setCommit  E
039600060907
039700060907     P*--------------------------------------------------
039800060907     P* Procedure name: Aznmr_setLibreria
039900060907     P* Purpose:        Imposta la libreria del parametro EXTFILE.
040000060907     P* Returns:
040100060907     P* Parameter:      rqsLibreria => Libreria
040200060907     P*--------------------------------------------------
040300060907     P Aznmr_setLibreria...
040400080321     P                 B
040500060907     D Aznmr_setLibreria...
040600060907     D                 PI
040700060913     D  rqsLibreria                  10A   CONST
040800080321
040900060907      /FREE
041000080321
041100060913       libreria = %TRIML(rqsLibreria);
041200060907       CLEAR qusec;
041300060907       qusbprv = %SIZE(qusec);
041400060913       ConvertCase(RequestControlBlockFormat:libreria:libreria
041500060907       :10:qusec);
041600060907       RESET fileNameR;
041700060913       fileNameR = %TRIMR(libreria) + '/' + fileNameR;
041800060907       RESET fileNameN;
041900060913       fileNameN = %TRIMR(libreria) + '/' + fileNameN;
042000060907       RETURN;
042100080321
042200060907      /END-FREE
042300060907     P Aznmr_setLibreria...
042400060907     P                 E
042500060906
042600060907     P*--------------------------------------------------
042700060907     P* Procedure name: Aznmr_openAznmrr00f
042800060907     P* Purpose:        Apre AZNMRR00F
042900060907     P* Returns:        Esito
043000060907     P* Parameter:      rpyEsito => Esito
043100060907     P*--------------------------------------------------
043200060907     P Aznmr_openAznmrr00f...
043300080321     P                 B
043400060907     D Aznmr_openAznmrr00f...
043500060907     D                 PI            10I 0
043600060907     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
043700060907     D  rpyLibreria                  10A   OPTIONS(*NOPASS:*OMIT)
043800060907
043900060907     D rtnEsito        S             10I 0
044000060907
044100060907      /FREE
044200060907
044300060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
044400060907         CLEAR rpyEsito;
044500060907       ENDIF;
044600060911       IF %PARMS > 1 AND %ADDR(rpyLibreria) <> *NULL;
044700060907         CLEAR rpyLibreria;
044800060907       ENDIF;
044900060907
045000080321       Aznmr_closeAznmrr00f();
045100080321
045200080321       OPEN(E) aznmrr01l;
045300060907
045400060907       IF %ERROR;
045500060907         rtnEsito = %STATUS * -1;
045600060911         IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
045700060907           rpyEsito = rtnEsito;
045800060907         ENDIF;
045900060907       ELSE;
046000060911         IF %PARMS > 1 AND %ADDR(rpyLibreria) <> *NULL;
046100060907           rpyLibreria = infAznmrr.library;
046200060907         ENDIF;
046300060907       ENDIF;
046400060907
046500060907       RETURN rtnEsito;
046600060907
046700060907      /END-FREE
046800060907     P Aznmr_openAznmrr00f...
046900060907     P                 E
047000060907
047100060907     P*--------------------------------------------------
047200060907     P* Procedure name: Aznmr_closeAznmrr00f
047300060907     P* Purpose:        Chiude AZNMRR00F
047400060907     P* Returns:        Esito
047500060907     P* Parameter:      rpyEsito => Esito
047600060907     P*--------------------------------------------------
047700060907     P Aznmr_closeAznmrr00f...
047800080321     P                 B
047900060907     D Aznmr_closeAznmrr00f...
048000060907     D                 PI            10I 0
048100060907     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
048200060907
048300060907     D rtnEsito        S             10I 0
048400060907
048500060907      /FREE
048600060907
048700060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
048800060907         CLEAR rpyEsito;
048900060907       ENDIF;
049000060907
049100060907       IF %OPEN(aznmrr01l);
049200060907         CLOSE aznmrr01l;
049300060907       ELSE;
049400060907         rtnEsito = InfoIlFileNonEraAperto;
049500060907       ENDIF;
049600060907
049700060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
049800060907         rpyEsito = rtnEsito;
049900060907       ENDIF;
050000060907
050100060907       RETURN rtnEsito;
050200060907
050300060907      /END-FREE
050400060907     P Aznmr_closeAznmrr00f...
050500060907     P                 E
050600060907
050700060907     P*--------------------------------------------------
050800060907     P* Procedure name: Aznmr_openAznmrn00f
050900060907     P* Purpose:        Apre AZNMRN00F
051000060907     P* Returns:        Esito
051100060907     P* Parameter:      rpyEsito => Esito
051200060907     P*--------------------------------------------------
051300060907     P Aznmr_openAznmrn00f...
051400080321     P                 B
051500060907     D Aznmr_openAznmrn00f...
051600060907     D                 PI            10I 0
051700060907     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
051800060907     D  rpyLibreria                  10A   OPTIONS(*NOPASS:*OMIT)
051900060907
052000060907     D rtnEsito        S             10I 0
052100060907
052200060907      /FREE
052300060907
052400060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
052500060907         CLEAR rpyEsito;
052600060907       ENDIF;
052700060911       IF %PARMS > 1 AND %ADDR(rpyLibreria) <> *NULL;
052800060907         CLEAR rpyLibreria;
052900060907       ENDIF;
053000060907
053100080321       Aznmr_closeAznmrn00f();
053200080321
053300060907       OPEN(E) aznmrn01l;
053400060907
053500060907       IF %ERROR;
053600060907         rtnEsito = %STATUS * -1;
053700060911         IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
053800060907           rpyEsito = rtnEsito;
053900060907         ENDIF;
054000060907       ELSE;
054100060911         IF %PARMS > 1 AND %ADDR(rpyLibreria) <> *NULL;
054200060907           rpyLibreria = infAznmrn.library;
054300060907         ENDIF;
054400060907       ENDIF;
054500060907
054600060907       RETURN rtnEsito;
054700060907
054800060907      /END-FREE
054900060907     P Aznmr_openAznmrn00f...
055000060907     P                 E
055100060907
055200060907     P*--------------------------------------------------
055300060907     P* Procedure name: Aznmr_closeAznmrn00f
055400060907     P* Purpose:        Chiude AZNMRN00F
055500060907     P* Returns:        Esito
055600060907     P* Parameter:      rpyEsito => Esito
055700060907     P*--------------------------------------------------
055800060907     P Aznmr_closeAznmrn00f...
055900080321     P                 B
056000060907     D Aznmr_closeAznmrn00f...
056100060907     D                 PI            10I 0
056200060907     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
056300060907
056400060907     D rtnEsito        S             10I 0
056500060907
056600060907      /FREE
056700060907
056800060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
056900060907         CLEAR rpyEsito;
057000060907       ENDIF;
057100060907
057200060907       IF %OPEN(aznmrn01l);
057300060907         CLOSE aznmrn01l;
057400060907       ELSE;
057500060907         rtnEsito = 1; // Il file non era aperto.
057600060907       ENDIF;
057700060907
057800060911       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
057900060907         rpyEsito = rtnEsito;
058000060907       ENDIF;
058100060907
058200060907       RETURN rtnEsito;
058300060907
058400060907      /END-FREE
058500060907     P Aznmr_closeAznmrn00f...
058600060907     P                 E
058700060906
058800060906     P*--------------------------------------------------
058900060906     P* Procedure name: Aznmr_readRegola
059000060906     P* Purpose:        Restituisce la regola di un numeratore.
059100060906     P* Returns:        Dati AZNMRR00F
059200060907     P* Parameter:      rqsCodice => Codice numeratore
059300060907     P* Parameter:      rpyEsito => Esito.
059400060907     P* Parameter:      rpyDatiAznmrr00f => Record di AZNMRR00F
059500060906     P*--------------------------------------------------
059600060906     P Aznmr_readRegola...
059700080321     P                 B
059800060906     D Aznmr_readRegola...
059900060906     D                 PI                  LIKEREC(aznmrr000)
060000060907     D  rqsCodice                          LIKE(rCodice) CONST
060100060907     D  rpyEsito                     10I 0
060200060907     D  rpyDatiAznmrr00f...
060300060907     D                                     LIKEREC(aznmrr000)
060400060906     D                                     OPTIONS(*NOPASS:*OMIT)
060500060906
060600060906      /FREE
060700060911
060800060906       CLEAR aznmrr00f;
060900060907       CLEAR rpyEsito;
061000080321
061100060911       IF %PARMS > 2 AND %ADDR(rpyDatiAznmrr00f) <> *NULL;
061200060907         CLEAR rpyDatiAznmrr00f;
061300060906       ENDIF;
061400060906
061500060907       IF rqsCodice = *BLANK;
061600060907         rpyEsito = ErrCodiceObbligatorio;
061700060906         RETURN aznmrr00f;
061800060906       ENDIF;
061900060906
062000060907       CHAIN(NE) rqsCodice aznmrr01l;
062100060907
062200060907       SELECT;
062300060907         WHEN %ERROR;
062400060907           rpyEsito = %STATUS * -1;
062500060907         WHEN NOT %FOUND;
062600060907           rpyEsito = ErrRegolaNonTrovata;
062700060907       ENDSL;
062800060906
062900060911       IF %PARMS > 2 AND %ADDR(rpyDatiAznmrr00f) <> *NULL;
063000060907         rpyDatiAznmrr00f = aznmrr00f;
063100060906       ENDIF;
063200060906
063300060906       RETURN aznmrr00f;
063400060906
063500060906      /END-FREE
063600060906     P Aznmr_readRegola...
063700060906     P                 E
063800060907
063900060907     P*--------------------------------------------------
064000060907     P* Procedure name: Aznmr_getNumeroN
064100060907     P* Purpose:        Preleva un numero numerico.
064200060907     P* Returns:        Numero.
064300060907     P* Parameter:      rqsCodice => Codice numeratore
064400060907     P* Parameter:      rqsSocieta => Società
064500060907     P* Parameter:      rqsUnita => Unità
064600060907     P* Parameter:      rqsKeyGest => Chiave gestionale
064700060907     P* Parameter:      rqsData => Data.
064800060907     P* Parameter:      rpyEsito => Esito
064900060907     P* Parameter:      rpyNumero => Numero.
065000060907     P*--------------------------------------------------
065100060907     P Aznmr_getNumeroN...
065200060907     P                 B                   EXPORT
065300060907     D Aznmr_getNumeroN...
065400060907     D                 PI                  LIKE(nNumero)
065500060907     D  rqsCodice                          LIKE(nCodice)
065600060907     D                                     CONST
065700060907     D  rqsSocieta                         LIKE(nSocieta)
065800060907     D                                     CONST
065900060907     D                                     OPTIONS(*OMIT)
066000060907     D  rqsUnita                           LIKE(nUnita)
066100060907     D                                     CONST
066200060907     D                                     OPTIONS(*OMIT)
066300060907     D  rqsKeyGest                         LIKE(nKeyGest)
066400060907     D                                     CONST
066500060907     D                                     OPTIONS(*OMIT)
066600060907     D  rqsData                            LIKE(nDataPeriod)
066700060908     D                                     CONST
066800060907     D                                     OPTIONS(*OMIT)
066900060907     D  rpyEsito                     10I 0
067000060907     D  rpyNumero                          LIKE(nNumero)
067100060908     D                                     OPTIONS(*NOPASS:*OMIT)
067200060907
067300060907      /FREE
067400060907
067500060907       CLEAR rpyEsito;
067600080321
067700060911       IF %PARMS > 6 AND %ADDR(rpyNumero) <> *NULL;
067800060908         CLEAR rpyNumero;
067900060908       ENDIF;
068000060907
068100060907       RESET keyAznmrn01l;
068200060907       keyAznmrn01l.nCodice = rqsCodice;
068300080321
068400060907       IF %ADDR(rqsSocieta) <> *NULL;
068500060907         keyAznmrn01l.nSocieta = rqsSocieta;
068600060907       ENDIF;
068700080321
068800060907       IF %ADDR(rqsUnita) <> *NULL;
068900060907         keyAznmrn01l.nUnita = rqsUnita;
069000060907       ENDIF;
069100080321
069200060907       IF %ADDR(rqsKeyGest) <> *NULL;
069300060907         keyAznmrn01l.nKeyGest = rqsKeyGest;
069400060907       ENDIF;
069500080321
069600060907       IF %ADDR(rqsData) <> *NULL;
069700060907         keyAznmrn01l.nDataPeriod = rqsData;
069800060907       ENDIF;
069900060907
070000060907       Aznmr_setIdNumeratore(keyAznmrn01l.nCodice:keyAznmrn01l.nSocieta
070100060907       :keyAznmrn01l.nUnita:keyAznmrn01l.nKeyGest:keyAznmrn01l.nDataPeriod
070200060907       :rpyEsito);
070300060907
070400060907       IF rpyEsito < 0;
070500060907         RETURN 0;
070600060907       ENDIF;
070700060907
070800060908       CHAIN(E) %KDS(keyAznmrn01l) aznmrn01l;
070900060908
071000060908       IF %ERROR;
071100060908         rpyEsito = %STATUS * -1;
071200060908         RETURN 0;
071300060908       ENDIF;
071400060908
071500060908       // Il numeratore non esiste, tento di crearne uno nuovo.
071600080321
071700060908       IF NOT %FOUND;
071800060908         Aznmr_crtNumeratore(keyAznmrn01l.nCodice:keyAznmrn01l.nSocieta
071900060908         :keyAznmrn01l.nUnita:keyAznmrn01l.nKeyGest:keyAznmrn01l.nDataPeriod
072000060908         :rpyEsito);
072100060908         IF rpyEsito < 0;
072200060908           RETURN 0;
072300060908         ENDIF;
072400060908         CHAIN(E) %KDS(keyAznmrn01l) aznmrn01l;
072500060908         IF %ERROR;
072600060908           rpyEsito = %STATUS * -1;
072700060908           RETURN 0;
072800060908         ENDIF;
072900060908         IF NOT %FOUND;
073000060914           messaggioLungo = 'Il numeratore ' + %TRIMR(rqsCodice) + ' ' +
073100060914           %TRIMR(rDescrizion);
073200060912           IF keyAznmrn01l.nSocieta <> *BLANK;
073300060912             messaggioLungo += ' società ' + keyAznmrn01l.nSocieta;
073400060912           ENDIF;
073500060912           IF keyAznmrn01l.nUnita <> *BLANK;
073600060912             messaggioLungo += ' unità ' + %TRIMR(keyAznmrn01l.nUnita);
073700060912           ENDIF;
073800060912           IF keyAznmrn01l.nKeyGest <> *BLANK;
073900060914             messaggioLungo += ' ' + %TRIMR(rDesKeyGest) + ' ' +
074000060914             %TRIMR(keyAznmrn01l.nKeyGest);
074100060912           ENDIF;
074200060912           IF keyAznmrn01l.nDataPeriod > *LOVAL;
074300060912             messaggioLungo += ' periodo ' +
074400060912             %CHAR(keyAznmrn01l.nDataPeriod);
074500060912           ENDIF;
074600060912           messaggioLungo += ' non esiste.';
074700060912           messageText = messaggioLungo;
074800060912           messaggioLungo += ':/NCreare il numeratore con +
074900060912           l''''apposita funzione di gestione dei numeratori.';
075000060912           messageText += ' Creare il numeratore con +
075100060912           l''apposita funzione di gestione dei numeratori.';
075200060914           Aznmr_sndMsg(rqsCodice:*BLANK:messaggioLungo:*OMIT
075300060912           :messageText);
075400060908           rpyEsito = ErrNumeratoreNonTrovato;
075500060908           RETURN 0;
075600060908         ENDIF;
075700060908       ENDIF;
075800060908
075900060908       // Il numeratore esiste ma è esaurito.
076000080321
076100060908       IF %FOUND AND nNumero = nValNFine;
076200060908         SELECT;
076300060908           WHEN rRtnAutIniz = *ON; // Riparto dal valore iniziale.
076400060908             nNumero = Aznmr_inzNumeroN(rValNInizio:rValNFine);
076500060908           WHEN rTemporizza = Progressivo; // Passo al progressivo successivo.
076600060908             Aznmr_nextProgressivoN(keyAznmrn01l.nCodice:keyAznmrn01l.nSocieta
076700060908             :keyAznmrn01l.nUnita:keyAznmrn01l.nKeyGest:rpyEsito);
076800060908             IF rpyEsito < 0;
076900060908               RETURN 0;
077000060908             ENDIF;
077100060908             CHAIN(E) %KDS(keyAznmrn01l) aznmrn01l;
077200060908             IF %ERROR;
077300060908               rpyEsito = %STATUS * -1;
077400060908               RETURN 0;
077500060908             ENDIF;
077600060908             IF NOT %FOUND;
077700060908               rpyEsito = ErrNumeratoreNonTrovato;
077800060908               RETURN 0;
077900060908             ENDIF;
078000060908         ENDSL;
078100060908       ENDIF;
078200060908
078300060911       IF nNumero = 0;
078400060911         nNumero = Aznmr_inzNumeroN(nValNInizio:nValNFine);
078500060911       ENDIF;
078600060911
078700060908       nNumero += rValNIncrem;
078800060908
078900060908       IF %ADDR(rqsData) <> *NULL AND rqsData > *LOVAL;
079000060908         nDataUltimo = rqsData;
079100060908       ELSE;
079200060908         nDataUltimo = %DATE();
079300060908       ENDIF;
079400060908
079500060908       UPDATE aznmrn000;
079600060908
079700060912       Aznmr_chkSogliaN();
079800060912
079900060911       IF %PARMS > 6 AND %ADDR(rpyNumero) <> *NULL;
080000060908         rpyNumero = nNumero;
080100060908       ENDIF;
080200060908
080300060908       RETURN nNumero;
080400060907
080500060907      /END-FREE
080600060907     P Aznmr_getNumeroN...
080700060907     P                 E
080800060907
080900060907     P*--------------------------------------------------
081000060907     P* Procedure name: Aznmr_setIdNumeratore
081100060907     P* Purpose:        Imposta ID numeratore secondo regola.
081200060907     P* Returns:        Esito
081300060907     P* Parameter:      rqsCodice => Codice numeratore
081400060907     P* Parameter:      rqsSocieta => Società
081500060907     P* Parameter:      rqsUnita => Unità
081600060907     P* Parameter:      rqsKeyGest => Chiave gestionale
081700060907     P* Parameter:      rqsData => Data per determinare il periodo.
081800060907     P* Parameter:      rpyEsito => Esito
081900060907     P*--------------------------------------------------
082000060907     P Aznmr_setIdNumeratore...
082100080321     P                 B
082200060907     D Aznmr_setIdNumeratore...
082300060907     D                 PI            10I 0
082400060907     D  rqsCodice                          LIKE(nCodice)
082500060907     D                                     CONST
082600060907     D  rqsSocieta                         LIKE(nSocieta)
082700060907     D                                     OPTIONS(*OMIT)
082800060907     D  rqsUnita                           LIKE(nUnita)
082900060907     D                                     OPTIONS(*OMIT)
083000060907     D  rqsKeyGest                         LIKE(nKeyGest)
083100060907     D                                     OPTIONS(*OMIT)
083200060907     D  rqsDataPeriod                      LIKE(nDataPeriod)
083300060907     D                                     OPTIONS(*OMIT)
083400060907     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
083500060907
083600060907     D rtnEsito        S             10I 0
083700060907
083800060907      /FREE
083900060907
084000060913       IF rqsCodice <> rCodice;
084100060913         Aznmr_readRegola(rqsCodice:rtnEsito);
084200060913         IF rtnEsito < 0;
084300060913           IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
084400060913             rpyEsito = rtnEsito;
084500060913           ENDIF;
084600060913           RETURN rtnEsito;
084700060913         ENDIF;
084800060913       ENDIF;
084900060907
085000060907       // Controllo impostazione codice società.
085100080321
085200060907       SELECT;
085300060907         WHEN rGruOSoc = DiSocieta AND
085400060907         (%ADDR(rqsSocieta) = *NULL OR rqsSocieta = *BLANK);
085500060907           rtnEsito = ErrIdNumeratoreSocieta;
085600060907         WHEN rGruOSoc = DiGruppo AND
085700060907         (%ADDR(rqsSocieta) <> *NULL AND rqsSocieta <> *BLANK);
085800060907           CLEAR rqsSocieta;
085900060907       ENDSL;
086000060907
086100060907       // Controllo impostazione codice unità.
086200080321
086300060907       SELECT;
086400060907         WHEN rLivelloPO <> POTutti AND
086500060907         (%ADDR(rqsUnita) = *NULL OR rqsUnita = *BLANK);
086600060907           rtnEsito = ErrIdNumeratoreUnita;
086700060907         WHEN rLivelloPO = POTutti AND
086800060907         (%ADDR(rqsUnita) <> *NULL AND rqsUnita <> *BLANK);
086900060907           CLEAR rqsUnita;
087000060907       ENDSL;
087100060907
087200060907       // Controllo impostazione chiave gestionale.
087300080321
087400060907       SELECT;
087500060907         WHEN rDesKeyGest <> *BLANK AND
087600060907         (%ADDR(rqsKeyGest) = *NULL OR rqsKeyGest = *BLANK);
087700060907           rtnEsito = ErrIdNumeratoreChiaveGestionale;
087800060907         WHEN rDesKeyGest = *BLANK AND
087900060907         (%ADDR(rqsKeyGest) <> *NULL AND rqsKeyGest <> *BLANK);
088000060907           CLEAR rqsKeyGest;
088100060907       ENDSL;
088200060907
088300060907       // Controllo impostazione data periodo.
088400080321
088500060907       SELECT;
088600060907         WHEN (rTemporizza = Giorno OR rTemporizza = Settimana OR
088700060907         rTemporizza = Mese OR rTemporizza = AnnoSolare OR
088800060907         rTemporizza = EsercizioCG)
088900060907         AND (%ADDR(rqsDataPeriod) = *NULL OR rqsDataPeriod = *LOVAL);
089000060907           rtnEsito = ErrIdNumeratoreData;
089100060907         WHEN (rTemporizza = Progressivo OR rTemporizza = Assoluto)
089200060907         AND (%ADDR(rqsDataPeriod) <> *NULL AND rqsDataPeriod > *LOVAL);
089300060907           CLEAR rqsDataPeriod;
089400060907         OTHER;
089500060907           // Determino il 1o giorno del periodo.
089600060907           SELECT;
089700060908             WHEN rTemporizza = AnnoSolare AND rQtaTempo = 1;
089800060907               rqsDataPeriod -= %MONTHS(%SUBDT(rqsDataPeriod:*MONTHS)-1);
089900060907               rqsDataPeriod -= %DAYS(%SUBDT(rqsDataPeriod:*DAYS)-1);
090000060908             WHEN rTemporizza = EsercizioCG AND rQtaTempo = 1;
090100060907               RESET x10eseds;
090200060907               x10Data = rqsDataPeriod;
090300060907               x10Societa = rqsSocieta;
090400060907               getEsercizioProj(x10eseds);
090500060907               IF x10Eserciz > 0;
090600060907                 rqsDataPeriod = x10DtInes;
090700060907               ELSE;
090800060907                 rtnEsito = ErrIdNumeratoreData;
090900060907               ENDIF;
091000060908             WHEN rTemporizza = Mese AND rQtaTempo = 1;
091100060907               rqsDataPeriod -= %DAYS(%SUBDT(rqsDataPeriod:*DAYS)-1);
091200060908             WHEN rTemporizza = Settimana AND rQtaTempo = 1;
091300060908             WHEN rTemporizza = Giorno AND rQtaTempo = 1;
091400060907           ENDSL;
091500060907       ENDSL;
091600060907
091700060911       IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
091800060907         rpyEsito = rtnEsito;
091900060907       ENDIF;
092000060907
092100060907       RETURN rtnEsito;
092200060907
092300060907      /END-FREE
092400060907     P Aznmr_setIdNumeratore...
092500060907     P                 E
092600060907
092700060908     D*--------------------------------------------------
092800060908     D* Procedure name: Aznmr_crtNumeratore
092900060908     D* Purpose:        Crea un nuovo numeratore secondo regola.
093000060908     D* Returns:        Esito
093100060908     D* Parameter:      rqsCodice => Codice numeratore
093200060908     D* Parameter:      rqsSocieta => Società
093300060908     D* Parameter:      rqsUnita => Unità
093400060908     D* Parameter:      rqsKeyGest => Chiave gestionale
093500060908     D* Parameter:      rqsDataPeriod => Data periodo.
093600060908     D* Parameter:      rpyEsito => Esito
093700060908     D*--------------------------------------------------
093800060908     P Aznmr_crtNumeratore...
093900060908     P                 B
094000060908     D Aznmr_crtNumeratore...
094100060908     D                 PI            10I 0
094200060908     D  rqsCodice                          LIKE(nCodice)
094300060908     D  rqsSocieta                         LIKE(nSocieta)
094400060908     D  rqsUnita                           LIKE(nUnita)
094500060908     D  rqsKeyGest                         LIKE(nKeyGest)
094600060908     D  rqsDataPeriod                      LIKE(nDataPeriod)
094700060908     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
094800060908
094900060908     D rtnEsito        S             10I 0
095000060908
095100060908      /FREE
095200060908
095300060908       CLEAR rtnEsito;
095400060908
095500060908       IF rGenerAutom = *ON;
095600060908         IF rTipoNumero = Numerico;
095700060908           CLEAR aznmrn000;
095800060908           nCodice = rqsCodice;
095900060908           nSocieta = rqsSocieta;
096000060908           nUnita = rqsUnita;
096100060908           nDataPeriod = rqsDataPeriod;
096200060908           nNumero = Aznmr_inzNumeroN(rValNInizio:rValNFine);
096300060908           nValNInizio = rValNInizio;
096400060908           nValNFine = rValNFine;
096500060908           nSogliaNRis = rSogliaNRis;
096600060908           WRITE aznmrn000;
096700060908         ENDIF;
096800060908       ENDIF;
096900060908
097000060911       IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
097100060908         rpyEsito = rtnEsito;
097200060908       ENDIF;
097300060908
097400060908       RETURN rtnEsito;
097500060908
097600060908      /END-FREE
097700060908     P Aznmr_crtNumeratore...
097800060908     P                 E
097900060908
098000060908     P*--------------------------------------------------
098100060908     P* Procedure name: Aznmr_inzNumeroN
098200060908     P* Purpose:        Inizializza il numero numerico secondo la regola.
098300060908     P* Returns:        Numero numerico.
098400060908     P* Parameter:      rqsValNInizio => Valore numerico iniziale.
098500060908     P* Parameter:      rqsValNFine => Valore numerico finale.
098600060908     P*--------------------------------------------------
098700060908     P Aznmr_inzNumeroN...
098800060908     P                 B
098900060908     D Aznmr_inzNumeroN...
099000060908     D                 PI                  LIKE(nNumero)
099100060908     D  rqsValNInizio                      LIKE(rValNInizio)
099200060908     D  rqsValNFine                        LIKE(rValNFine)
099300060908
099400060908     D rtnNumero       S                   LIKE(nNumero)
099500060908
099600060908      /FREE
099700060908
099800060908       SELECT;
099900080321         WHEN rqsValNFine >= rqsValNInizio;
100000060908           rtnNumero = rqsValNInizio - 1;
100100060908         WHEN rqsValNFine < rqsValNInizio;
100200060908           rtnNumero = rqsValNInizio + 1;
100300060908       ENDSL;
100400060908
100500060908       RETURN rtnNumero;
100600060908
100700060908      /END-FREE
100800060908     P Aznmr_inzNumeroN...
100900060908     P                 E
101000060912
101100060912     D*--------------------------------------------------
101200060912     D* Procedure name: Aznmr_chkSogliaN
101300060912     D* Purpose:        Controlla se è stata raggiunta la soglia di rischio.
101400060912     D* Returns:        Esito.
101500060912     D* Parameter:      rpyEsito => Esito
101600060912     D*--------------------------------------------------
101700060912     P Aznmr_chkSogliaN...
101800060912     P                 B
101900060912     D Aznmr_chkSogliaN...
102000060912     D                 PI            10I 0
102100060912     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
102200060912
102300060912     D rtnEsito        S             10I 0
102400060912
102500060912      /FREE
102600060912
102700060912       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
102800060912         CLEAR rpyEsito;
102900060912       ENDIF;
103000060912
103100060912       IF nNumero < nSogliaNRis OR nSogliaNRis = 0;
103200060912         RETURN 0;
103300060912       ENDIF;
103400060912
103500060912       IF (nNumero - nSogliaNRis) <= rValNIncrem;
103600060914         messaggioLungo = 'Il numeratore ' + %TRIMR(nCodice) + ' ' +
103700060914         %TRIMR(rDescrizion);
103800060912         IF nSocieta <> *BLANK;
103900060912           messaggioLungo += ' società ' + nSocieta;
104000060912         ENDIF;
104100060912         IF nUnita <> *BLANK;
104200060912           messaggioLungo += ' unità ' + %TRIMR(nUnita);
104300060912         ENDIF;
104400060912         IF nKeyGest <> *BLANK;
104500060914           messaggioLungo += ' ' + %TRIMR(rDesKeyGest) + ' ' +
104600060914           %TRIMR(nKeyGest);
104700060912         ENDIF;
104800060912         IF nDataPeriod > *LOVAL;
104900060912           messaggioLungo += ' periodo ' + %CHAR(nDataPeriod);
105000060912         ENDIF;
105100060912         messaggioLungo += ' ha raggiunto la soglia di rischio ' +
105200060912         %CHAR(nSogliaNRis) + '.';
105300060912         messageText = messaggioLungo;
105400060914         Aznmr_sndMsg(nCodice:*BLANK:messaggioLungo:*OMIT
105500060912         :messageText);
105600060912         rtnEsito = InfoRaggiuntaSogliaRischio;
105700060912       ENDIF;
105800060912
105900060912
106000060912       IF %PARMS > 0 AND %ADDR(rpyEsito) <> *NULL;
106100060912         rpyEsito = rtnEsito;
106200060912       ENDIF;
106300060912
106400060912       RETURN rtnEsito;
106500060912
106600060912      /END-FREE
106700060912     P Aznmr_chkSogliaN...
106800060912     P                 E
106900060908
107000060908     D*--------------------------------------------------
107100060908     D* Procedure name: Aznmr_nextProgressivoN
107200060908     D* Purpose:        Passa al progressivo successivo numerico.
107300060908     D* Returns:        Esito
107400060908     D* Parameter:      rqsCodice => Codice numeratore
107500060908     D* Parameter:      rqsSocieta => Società
107600060908     D* Parameter:      rqsUnita => Unità
107700060908     D* Parameter:      rqsKeyGest => Chiave gestionale
107800060908     D* Parameter:      rpyEsito => Esito
107900060908     D*--------------------------------------------------
108000060908     P Aznmr_nextProgressivoN...
108100060908     P                 B
108200060908     D Aznmr_nextProgressivoN...
108300060908     D                 PI            10I 0
108400060908     D  rqsCodice                          LIKE(nCodice)
108500060908     D  rqsSocieta                         LIKE(nSocieta)
108600060908     D  rqsUnita                           LIKE(nUnita)
108700060908     D  rqsKeyGest                         LIKE(nKeyGest)
108800060908     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
108900060908
109000060911     D keyAznmrn01l    DS                  INZ LIKEREC(aznmrn000:*KEY)
109100060908     D rtnEsito        S             10I 0
109200060908     D lclProgressiv   S                   LIKE(nProgressiv)
109300060908
109400060908      /FREE
109500060908
109600060908       //***********************************************************************
109700060908       // Esempio di passaggio al progressivo successivo:
109800060908       //
109900060908       // Prima Dopo
110000060929       //     0   -1
110100060908       //     1    0
110200060908       //     2    1
110300060908       //***********************************************************************
110400060908
110500060911       IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
110600060908         CLEAR rpyEsito;
110700060908       ENDIF;
110800060929
110900060908       keyAznmrn01l.nCodice = rqsCodice;
111000060908       keyAznmrn01l.nSocieta = rqsSocieta;
111100060908       keyAznmrn01l.nUnita = rqsUnita;
111200060908       keyAznmrn01l.nKeyGest = rqsKeyGest;
111300060908       CLEAR keyAznmrn01l.nDataPeriod;
111400060929       CLEAR keyAznmrn01l.nProgressiv;
111500060929       SETLL %KDS(keyAznmrn01l:5) aznmrn01l;
111600060929       DOU %EOF(aznmrn01l);
111700060929         READE %KDS(keyAznmrn01l:5) aznmrn01l;
111800060929         IF nProgressiv > 0 OR %EOF;
111900060929           LEAVE;
112000060929         ENDIF;
112100060929         IF nProgressiv = *LOVAL;
112200060929           DELETE aznmrn000;
112300060929         ELSE;
112400060929           nProgressiv -= 1;
112500060929           UPDATE aznmrn000 %FIELDS(nProgressiv);
112600060929         ENDIF;
112700060929       ENDDO;
112800060908
112900060908       // Cambio tutti i progressivi positivi in 0, 1, 2 ...
113000060908       keyAznmrn01l.nProgressiv = 1;
113100060908       SETLL %KDS(keyAznmrn01l) aznmrn01l;
113200060908       FOR lclProgressiv = 0 TO 999;
113300060908         READE %KDS(keyAznmrn01l:5) aznmrn01l;
113400060908         IF %EOF;
113500060912           IF lclProgressiv = 0;
113600060914             messaggioLungo = 'Per il numeratore ' + %TRIMR(rqsCodice) + ' ' +
113700060914             %TRIMR(rDescrizion);
113800060912             IF rqsSocieta <> *BLANK;
113900060912               messaggioLungo += ' società ' + rqsSocieta;
114000060912             ENDIF;
114100060912             IF rqsUnita <> *BLANK;
114200060912               messaggioLungo += ' unità ' + %TRIMR(rqsUnita);
114300060912             ENDIF;
114400060912             IF rqsKeyGest <> *BLANK;
114500060914               messaggioLungo += ' ' + %TRIMR(rDesKeyGest) + ' ' +
114600060914               %TRIMR(rqsKeyGest);
114700060912             ENDIF;
114800060912             messaggioLungo += ' non è stato reperito un nuovo progressivo.';
114900060912             messageText = messaggioLungo;
115000060912             messaggioLungo += ':/NCreare un nuovo progressivo con +
115100060912             l''''apposita funzione di gestione dei numeratori.';
115200060912             messageText += ' Creare un nuovo progressivo con +
115300060912             l''apposita funzione di gestione dei numeratori.';
115400060914             Aznmr_sndMsg(rqsCodice:*BLANK:messaggioLungo:*OMIT
115500060912             :messageText);
115600060912             rtnEsito = ErrNuovoProgressivo;
115700060912           ENDIF;
115800060908           LEAVE;
115900060908         ENDIF;
116000060908         nProgressiv = lclProgressiv;
116100060908         UPDATE aznmrn000;
116200060908       ENDFOR;
116300060908
116400060911       IF %PARMS > 4 AND %ADDR(rpyEsito) <> *NULL;
116500060908         rpyEsito = rtnEsito;
116600060908       ENDIF;
116700060908
116800060908       RETURN rtnEsito;
116900060908
117000060908      /END-FREE
117100060908     P Aznmr_nextProgressivoN...
117200060908     P                 E
117300060911
117400060911     P*--------------------------------------------------
117500060911     P* Procedure name: Aznmr_sndMsg
117600060911     P* Purpose:        Invia un messaggio all' utente responsabile del numeratore.
117700060911     P* Returns:        Esito
117800060911     P* Parameter:      codice => Codice numeratore
117900060911     P* Parameter:      descrizione => Descrizione
118000060911     P* Parameter:      messaggio => Messaggio
118100060911     P* Parameter:      messaggioLungo => Messaggio lungo
118200060911     P* Parameter:      esito => Esito
118300060911     P*--------------------------------------------------
118400080321     P Aznmr_sndMsg    B
118500060911     D Aznmr_sndMsg    PI            10I 0
118600060912     D  rqsCodice                          LIKE(rCodice) CONST
118700060911     D  rqsDescrizione...
118800060911     D                               44A   VARYING
118900060911     D                                     CONST
119000060912     D                                     OPTIONS(*OMIT:*VARSIZE)
119100060911     D  rqsMessaggioLungo...
119200060911     D                             5000A   VARYING
119300060911     D                                     CONST
119400060912     D                                     OPTIONS(*OMIT:*VARSIZE)
119500060911     D  rqsMessaggio                256A   VARYING
119600060911     D                                     CONST
119700060911     D                                     OPTIONS(*NOPASS:*OMIT:*VARSIZE)
119800060912     D  rqsMessageText...
119900060912     D                              494A   VARYING
120000060912     D                                     CONST
120100060912     D                                     OPTIONS(*NOPASS:*OMIT:*VARSIZE)
120200060911     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
120300060911
120400060911     D rtnEsito        S             10I 0
120500060912     D messaggioLungo...
120600060912     D                 S           5000A   VARYING
120700060911     D MessageText     S            494A
120800060911
120900060911      /FREE
121000060911
121100060912       IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
121200060911         CLEAR rpyEsito;
121300060911       ENDIF;
121400060911
121500060911       IF rqsCodice <> rCodice;
121600060913         Aznmr_readRegola(rqsCodice:rtnEsito);
121700060911         IF rtnEsito < 0;
121800060912           IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
121900060911             rpyEsito = rtnEsito;
122000060911           ENDIF;
122100060911           RETURN rtnEsito;
122200060911         ENDIF;
122300060911       ENDIF;
122400080320
122500080320       // invio una e-mail all'utente del lavoro e al responsabile del numeratore.
122600080320
122700080320       email_setAddressFromUser(jobUser : REPLACE);
122800080320       email_setAddressFromUser(rUtenteResp : ADD);
122900080320
123000080320       IF %ADDR(rqsMessaggioLungo) <> *NULL AND %LEN(rqsMessaggioLungo) > 0;
123100080320         messaggioLungo = rqsMessaggioLungo;
123200080320       ENDIF;
123300080320
123400080320       IF email_send('Numeratore ' + %TRIMR(rqsCodice) + ' ' +
123500080320       %TRIMR(rDescrizion) + '.'
123600080320       : messaggioLungo
123700080320       ) < 0;
123800080320         IF %PARMS > 4 AND %ADDR(rqsMessageText) <> *NULL
123900080320         AND %LEN(rqsMessageText) > 0;
124000080320           messageText = rqsMessageText;
124100080320           email_sndBrkMsg(messageText);
124200080320         ENDIF;
124300080320       ENDIF;
124400060911
124500060912       IF %PARMS > 5 AND %ADDR(rpyEsito) <> *NULL;
124600060911         rpyEsito = rtnEsito;
124700060911       ENDIF;
124800060911
124900060911       RETURN rtnEsito;
125000060911
125100060911      /END-FREE
125200060911     P Aznmr_sndMsg    E
125300060913
125400060913     P*--------------------------------------------------
125500060913     P* Procedure name: Aznmr_chkCapienzaN
125600060913     P* Purpose:        Controlla la capienza di un numeratore numerico.
125700060913     P* Returns:        Esito
125800060913     P* Parameter:      rqsCodice => Codice numeratore.
125900060913     P* Parameter:      rqsSocieta => Società
126000060913     P* Parameter:      rqsUnita => Unità
126100060913     P* Parameter:      rqsKeyGest => Chiave gestionale
126200060913     P* Parameter:      rqsData => Data.
126300060913     P* Parameter:      rqsQuantita => Quantità di numeri da prelevare.
126400060913     P* Parameter:      rpyCapienza => Quantità di numeri disponibili.
126500060913     P* Parameter:      rpyEsito => Esito.
126600060913     P*--------------------------------------------------
126700060913     P Aznmr_chkCapienzaN...
126800060913     P                 B                   EXPORT
126900060913     D Aznmr_chkCapienzaN...
127000060913     D                 PI            10I 0
127100060913     D  rqsCodice                          LIKE(nCodice)
127200060913     D                                     CONST
127300060913     D  rqsSocieta                         LIKE(nSocieta)
127400060913     D                                     CONST
127500060913     D                                     OPTIONS(*OMIT)
127600060913     D  rqsUnita                           LIKE(nUnita)
127700060913     D                                     CONST
127800060913     D                                     OPTIONS(*OMIT)
127900060913     D  rqsKeyGest                         LIKE(nKeyGest)
128000060913     D                                     CONST
128100060913     D                                     OPTIONS(*OMIT)
128200060913     D  rqsData                            LIKE(nDataPeriod)
128300060913     D                                     CONST
128400060913     D                                     OPTIONS(*OMIT)
128500060913     D  rqsQuantita                        LIKE(nNumero)
128600060913     D  rpyCapienza                        LIKE(nNumero)
128700060913     D                                     OPTIONS(*NOPASS:*OMIT)
128800060913     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
128900060913
129000060913     D rtnEsito        S             10I 0
129100060913     D lclCodice       S                   LIKE(nCodice)
129200060913     D lclCapienza     S                   LIKE(nNumero)
129300060913     D lclSocieta      S                   LIKE(nSocieta)
129400060913     D lclUnita        S                   LIKE(nUnita)
129500060913     D lclKeyGest      S                   LIKE(nKeyGest)
129600060913     D lclDataPeriod   S                   LIKE(nDataPeriod)
129700060913
129800060913      /FREE
129900060913
130000060913       IF %PARMS > 6 AND %ADDR(rpyCapienza) <> *NULL;
130100060913         CLEAR rpyCapienza;
130200060913       ENDIF;
130300060913
130400060913       IF %PARMS > 7 AND %ADDR(rpyEsito) <> *NULL;
130500060913         CLEAR rpyEsito;
130600060913       ENDIF;
130700060913
130800060913       IF rqsQuantita <= 0;
130900060913         RETURN 0;
131000060913       ENDIF;
131100060913
131200060913       IF rqsCodice <> rCodice;
131300060913         Aznmr_readRegola(rqsCodice:rtnEsito);
131400060913         IF rtnEsito < 0;
131500060913           IF %PARMS > 7 AND %ADDR(rpyEsito) <> *NULL;
131600060913             rpyEsito = rtnEsito;
131700060913           ENDIF;
131800060913           RETURN rtnEsito;
131900060913         ENDIF;
132000060913       ENDIF;
132100060913
132200060913       // Se per il numeratore è previsto il ritorno automatico al valore iniziale
132300060913       // è inutile fare il controllo di capienza.
132400080321
132500060913       IF rRtnAutIniz = *ON;
132600060913         RETURN 0;
132700060913       ENDIF;
132800060913
132900060913       lclCodice = rqsCodice;
133000080321
133100060913       IF %ADDR(rqsSocieta) <> *NULL;
133200060913         lclSocieta = rqsSocieta;
133300060913       ENDIF;
133400080321
133500060913       IF %ADDR(rqsUnita) <> *NULL;
133600060913         lclUnita = rqsUnita;
133700060913       ENDIF;
133800080321
133900060913       IF %ADDR(rqsKeyGest) <> *NULL;
134000060913         lclKeyGest = rqsKeyGest;
134100060913       ENDIF;
134200080321
134300060913       IF %ADDR(rqsData) <> *NULL;
134400060913         lclDataPeriod = rqsData;
134500060913       ENDIF;
134600060913
134700060913       Aznmr_setIdNumeratore(lclCodice:lclSocieta:lclUnita:lclKeyGest
134800060913       :lclDataPeriod:rtnEsito);
134900060913
135000060913       IF rtnEsito < 0;
135100060913         IF %PARMS > 7 AND %ADDR(rpyEsito) <> *NULL;
135200060913           rpyEsito = rtnEsito;
135300060913         ENDIF;
135400060913         RETURN rtnEsito;
135500060913       ENDIF;
135600060913
135700060913      /END-FREE
135800060913     C/EXEC SQL
135900060913     C+ SELECT SUM(CASE WHEN NUMERO > 0 THEN VALNFINE - NUMERO
136000060913     C+        ELSE VALNFINE - VALNINIZIO + 1 END) INTO :lclCapienza
136100060913     C+ FROM AZNMRN00F
136200060913     C+ WHERE CODICE = :lclCodice AND SOCIETA = :lclSocieta AND
136300060913     C+       UNITA = :lclUnita AND KEYGEST = :lclKeyGest AND
136400060913     C+       DATAPERIOD = :lclDataPeriod AND PROGRESSIV >= 0
136500060913     C/END-EXEC
136600060913      /FREE
136700060913
136800060913       IF sqlCode < 0;
136900060913         rtnEsito = sqlCode * -1;
137000060913         IF %PARMS > 7 AND %ADDR(rpyEsito) <> *NULL;
137100060913           rpyEsito = rtnEsito;
137200060913         ENDIF;
137300060913         RETURN rtnEsito;
137400060913       ENDIF;
137500060913
137600060913       IF %PARMS > 6 AND %ADDR(rpyCapienza) <> *NULL;
137700060913         rpyCapienza = lclCapienza;
137800060913       ENDIF;
137900060913
138000060913       IF lclCapienza < rqsQuantita;
138100080321         rtnEsito = ErrCapienzaNonBloccante;
138200080321         IF rStopCapien = *ON;
138300080321           rtnEsito = ErrCapienzaBloccante;
138400080321           messaggioLungo = 'Il numeratore ' + %TRIMR(lclCodice) + ' ' +
138500080321           %TRIMR(rDescrizion);
138600080321           IF lclSocieta <> *BLANK;
138700080321             messaggioLungo += ' società ' + lclSocieta;
138800080321           ENDIF;
138900080321           IF lclUnita <> *BLANK;
139000080321             messaggioLungo += ' unità ' + %TRIMR(lclUnita);
139100080321           ENDIF;
139200080321           IF lclKeyGest <> *BLANK;
139300080321             messaggioLungo += ' ' + %TRIMR(rDesKeyGest) + ' ' +
139400080321             %TRIMR(lclKeyGest);
139500080321           ENDIF;
139600080321           IF lclDataPeriod > *LOVAL;
139700080321             messaggioLungo += ' periodo ' +
139800080321             %CHAR(lclDataPeriod);
139900080321           ENDIF;
140000080321           messaggioLungo += ' non ha una capienza sufficiente.';
140100080321           messageText = messaggioLungo;
140200080321           Aznmr_sndMsg(rqsCodice:*BLANK:messaggioLungo:*OMIT
140300080321           :messageText);
140400080321         ENDIF;
140500080321       ENDIF;
140600060913
140700060913       IF %PARMS > 7 AND %ADDR(rpyEsito) <> *NULL;
140800060913         rpyEsito = rtnEsito;
140900060913       ENDIF;
141000060913
141100060913       RETURN rtnEsito;
141200060913
141300060913      /END-FREE
141400060913     P Aznmr_chkCapienzaN...
141500060913     P                 E
141600060913
141700080321     P*--------------------------------------------------
141800080321     P* Procedure name: Aznmr_finalize
141900080321     P* Purpose:        Chiusura oggetto numeratore.
142000080321     P* Returns:        Esito.
142100080321     P*--------------------------------------------------
142200080321     P Aznmr_finalize  B                   EXPORT
142300080321     D Aznmr_finalize  PI            10I 0
142400080321
142500080321      /FREE
142600080321
142700080321       Aznmr_closeAznmrr00f();
142800080321       Aznmr_closeAznmrn00f();
142900080321       RETURN 0;
143000080321
143100080321      /END-FREE
143200080321     P Aznmr_finalize  E
143300080321
