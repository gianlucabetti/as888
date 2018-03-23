000100121109      //************************************************************************
000200121109      //
000300121109      // PITECO CARICO MOVIMENTI STANDARD MSEQ
000400121109      //
000500121112      // Questo service program prepara una registrazione per Piteco.
000600121112      //
000700121112      // Esempio: per 1 movimento di 100,00 € in avere del conto 000201 0000027
000800121112      // vengono inseriti 2 movimenti:
000900121112      //
001000121112      // - tipo record T: 100,00 € avere del conto 000202 0000027;
001100121112      // - tipo record R: 100,00 € dare  del conto 000201 0000027;
001200121112      //
001300121112      // La registrazione sarà poi importata in Piteco, confermata ed inserita
001400121112      // in Proj, quindi al termine del giro in teoria il conto 000201 0000027
001500121112      // dovrebbe avere saldo 0.
001600121112      //
001700121109      //************************************************************************
001800130206     H NOMAIN BNDDIR('PJXBND')
001900121109
002000121109      //************************************************************************
002100121109      //
002200121109      // Strutture dati.
002300121109      //
002400121109      //************************************************************************
002401131015     D anbob00f      E DS                  QUALIFIED INZ
002500121109     D done            DS                  QUALIFIED INZ
002600121109     D  init                           N
002700121109     D  newFile                        N
002800121109     D  newRegistrazione...
002900121109     D                                 N
003000121109     D this            DS                  QUALIFIED INZ
003100121109     D  dir                         255A   VARYING
003200121109     D  stmf                        255A   VARYING
003300121109     D  stmfCounter                   3U 0
003400121109     D  fd                           10I 0
003500121112     D  nrPiteco                     10U 0
003600121109     D kpjba         E DS                  QUALIFIED
003700130206     D ndDvaBcoDs    E DS                  QUALIFIED
003800121112     D psds           SDS                  QUALIFIED
003900121109     D  jobName              244    253A
004000121109     D  jobUser              254    263A
004100121109     D  jobNumber            264    269A
004200121109     D yPtcMseqR     E DS                  QUALIFIED INZ(*EXTDFT)
004300121109     D yPtcMseqT     E DS                  QUALIFIED INZ(*EXTDFT)
004400121109
004500121109      //************************************************************************
004600121109      //
004700121109      // Costanti.
004800121109      //
004900121109      //************************************************************************
005000121109      /COPY GAITRASRC/SRCCONST,IFS
005100121109      /COPY GAITRASRC/SRCCONST,YPTCMSEQ
005200121112     D CRLF...
005300121112     D                 C                   X'0D25'
005400130206     D PROJ_CALCOLO_DATA_VALUTA_MANUALE...
005500130206     D                 C                   *BLANK
005501131015     D SOCIETA_CAPOFILA...
005502131015     D                 C                   '888'
005600121112     D SQLCODE_ROW_NOT_FOUND...
005700121109     D                 C                   100
005800121109     D STMF_DEFINITIVO...
005900121109     D                 C                   '.txt'
006000121109     D STMF_TEMPORANEO...
006100121109     D                 C                   '.tmp'
006200121109
006300121109      //************************************************************************
006400121109      //
006500121109      // Campi.
006600121109      //
006700121109      //************************************************************************
006800121109     D errno           S             10I 0 BASED(errnoPtr)
006900130206     D esitoProj       S              1A
007000121113
007100121113     D*--------------------------------------------------
007200121113     D* Procedure name: YPTCMSEQ_SetRiferimentoBanca
007300121113     D* Purpose:        Restituisce il riferimento banca.
007400121113     D* Returns:        Esito.
007500121113     D* Parameter:      priSys
007600121113     D* Parameter:      priNrAsReg
007700121113     D* Parameter:      priNrRigaM
007800121113     D* Parameter:      priRiferimentoBanca
007900121113     D*--------------------------------------------------
008000121113     D YPTCMSEQ_SetRiferimentoBanca...
008100121113     D                 PR            10I 0
008200121113     D  priSys                        3P 0
008300121113     D  priNrAsReg                    9P 0
008400121113     D  priNrRigaM                    5P 0
008500121113     D  priIdSocieta                  3A
008600121113     D  priCausale                    4A
008700121113     D  priTipoOperazione...
008800121113     D                                4A
008900121113
009000121113     D*--------------------------------------------------
009100121113     D* Procedure name: YPTCMSEQ_SetRiferimentoInterno
009200121113     D* Purpose:        Restituisce il riferimento interno.
009300121113     D* Returns:        Esito.
009400121113     D* Parameter:      priSys
009500121113     D* Parameter:      priNrAsReg
009600121113     D* Parameter:      priNrRigaM
009700121113     D*--------------------------------------------------
009800121113     D YPTCMSEQ_SetRiferimentoInterno...
009900121113     D                 PR            10I 0
010000121113     D  priSys                        3P 0
010100121113     D  priNrAsReg                    9P 0
010200121113     D  priNrRigaM                    5P 0
010300121113     D  priIdSocieta                  3A
010400121113     D  priCausale                    4A
010500121113     D  priTipoOperazione...
010600121113     D                                4A
010700121113
010800121113     D*--------------------------------------------------
010900121113     D* Procedure name: YPTCMSEQ_SetCentroDiCostoByUnita
011000121113     D* Purpose:        Imposta il centro di costo dall'unità.
011100121113     D* Returns:        Esito.
011200121113     D* Parameter:      priIdSocieta
011300121113     D* Parameter:      priUnita
011400121113     D*--------------------------------------------------
011500121113     D YPTCMSEQ_SetCentroDiCostoByUnita...
011600121113     D                 PR            10I 0
011700121113     D  priIdSocieta                  3A
011800121113     D  priUnita                      8A
011900121113
012000121113
012100121113
012200121109      //************************************************************************
012300121109      //
012400121109      // Prototipi.
012500121109      //
012600121109      //************************************************************************
012700121109      /COPY GAITRASRC/SRCPROTOPR,ERRNO
012800121109      /COPY GAITRASRC/SRCPROTOPR,IFS
012900130206      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
013000121109      /COPY GAITRASRC/SRCPROTOPR,YPTCMSEQ
013100121109
013200121109     P*--------------------------------------------------
013300121109     P* Procedure name: YPTCMSEQ_Init
013400121109     P* Purpose:        Inizializzazione modulo.
013500121109     P* Returns:        Esito.
013600121109     P* Parameter:      priKpjba
013700121109     P*--------------------------------------------------
013800121109     P YPTCMSEQ_Init   B                   EXPORT
013900121109     D YPTCMSEQ_Init   PI            10I 0
014000121109     D  priKpjba                           LIKE(kpjba)
014100121109
014200121109     D retField        S             10I 0 STATIC
014300121109
014400121109      /FREE
014500121109
014600121109       RESET retField;
014700121109
014800121109       IF done.init;
014900121109         RETURN YPTCMSEQ_ESITO_GIA_INIZIALIZZATO;
015000121109       ENDIF;
015100121109
015200121109       // Reperisco il nome della cartella dove creare i files.
015300121109
015400121109       EXEC SQL
015500121109         SELECT RTRIM(tntbe00f.tbeUni)
015600121109           INTO :this.dir
015700121109           FROM tntbe00f
015800121109           WHERE tntbe00f.tbeCod = '011' AND tntbe00f.tbeKe1 = '*DIR'
015900121109             AND tntbe00f.tbeKe2 = '*MSEQ' AND tntbe00f.tbeLin = ''
016000121109             AND tntbe00f.tbeSif = '' AND tntbe00f.tbeAtb = ''
016100121109       ;
016200121109
016300121109       SELECT;
016400121109         WHEN sqlCode < *ZERO;
016500121109           DUMP(A);
016600121109           RETURN sqlCode;
016700121109         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
016800121109           DUMP(A);
016900121109           RETURN YPTCMSEQ_ESITO_ERRORE_DIR;
017000121109       ENDSL;
017100121109
017101170607       EXEC SQL SET :this.dir = REPLACE(:this.dir, '/Bartolini',
017102170607       (SELECT valore FROM yenvvar0f WHERE nome = 'HOMEDIRROOT'));
017103170607
017200121109       IF IFS_ChangeCurrentDirectory(this.dir) = -1;
017300121109         errnoPtr = GetErrno();
017400121109         DUMP(A);
017500121109         RETURN YPTCMSEQ_ESITO_ERRORE_DIR;
017600121109       ENDIF;
017700121109
017800121109       kpjba = priKpjba;
017900121109
018000121109       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
018100121109
018200121109       done.init = *ON;
018300121109
018400121109       RETURN retField;
018500121109
018600121109      /END-FREE
018700121109     P YPTCMSEQ_Init   E
018800121109
018900121112
019000121112     P*--------------------------------------------------
019100121112     P* Procedure name: YPTCMSEQ_Finalize
019200121112     P* Purpose:        Chiusura modulo.
019300121112     P* Returns:        Esito.
019400121112     P*--------------------------------------------------
019500121112     P YPTCMSEQ_Finalize...
019600121112     P                 B                   EXPORT
019700121112     D YPTCMSEQ_Finalize...
019800121112     D                 PI            10I 0
019900121112
020000121112     D retField        S             10I 0 STATIC
020100121112
020200121112      /FREE
020300121112
020400121112       RESET retField;
020500121112       RESET this;
020600121112       RESET done;
020700121112       RETURN retField;
020800121112
020900121112      /END-FREE
021000121112     P YPTCMSEQ_Finalize...
021100121112     P                 E
021200121112
021300121109
021400121109     P*--------------------------------------------------
021500121109     P* Procedure name: YPTCMSEQ_NewFile
021600121109     P* Purpose:        Nuovo file.
021700121109     P* Returns:        Esito.
021800121109     P* Parameter:      priNomeFileFlusso => Nome file flusso. Se non passa...
021900121109     P*                          to sarà assegnato un nome automatico.
022000121109     P*--------------------------------------------------
022100121109     P YPTCMSEQ_NewFile...
022200121109     P                 B                   EXPORT
022300121109     D YPTCMSEQ_NewFile...
022400121109     D                 PI            10I 0
022500121109     D  priNomeFileFlusso...
022600121109     D                              255A   VARYING
022700121109     D                                     CONST
022800121109     D                                     OPTIONS(*NOPASS)
022900121109
023000121109     D retField        S             10I 0 STATIC
023100121109
023200121109      /FREE
023300121109
023400121109       IF NOT done.init;
023500121109         RETURN YPTCMSEQ_ESITO_NON_INIZIALIZZATO;
023600121109       ENDIF;
023700121109
023800121109       RESET retField;
023900121109
024000121109       IF %PARMS() < 1;
024100140117         this.stmf = 'mseq';
024300121109       ELSE;
024400121109         this.stmf = priNomeFileFlusso;
024500121109       ENDIF;
024600121109
024700121109       // Creo e apro il file.
024800121109
024900121109       this.fd = IFS_OpenFile( this.stmf + STMF_TEMPORANEO
025000121112                             : O_CREAT + O_WRONLY + O_CCSID + O_TEXTDATA
025100121112                             + O_TEXT_CREAT + O_EXCL
025200121112                             : S_IRUSR + S_IWUSR + S_IXUSR + S_IROTH
025300121112                             : CP_WINDOWS
025400121112                             : CP_ITALIAN );
025500121112
025600121112       IF this.fd = -1;
025700121109         errnoPtr = GetErrno();
025800121109         DUMP(A);
025900121112         RETURN YPTCMSEQ_ESITO_ERRORE_STMF_OPEN;
026000121109       ENDIF;
026100121109
026200121109       IF %PARMS() < 1;
026300121109         this.stmfCounter += 1;
026400121109       ENDIF;
026500121109
026600121113       // Resetto quello che devo resettare.
026700121113
026800121113       CLEAR this.nrPiteco;
026900121113
027000121109       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
027100121109
027200121109       done.newFile = *ON;
027300121109
027400121109       RETURN retField;
027500121109
027600121109      /END-FREE
027700121109     P YPTCMSEQ_NewFile...
027800121109     P                 E
027900121109
028000121109
028100121109     P*--------------------------------------------------
028200121109     P* Procedure name: YPTCMSEQ_EndFile
028300121109     P* Purpose:        Fine file.
028400121109     P* Returns:        Esito.
028500121109     P* Parameter:      priCommit => *ON = commit; *OFF = rollback.
028600121109     P*--------------------------------------------------
028700121109     P YPTCMSEQ_EndFile...
028800121109     P                 B                   EXPORT
028900121109     D YPTCMSEQ_EndFile...
029000121109     D                 PI            10I 0
029100121109     D  priCommit                      N   CONST
029200121109
029300121109     D retField        S             10I 0 STATIC
029400121109
029500121109      /FREE
029600121109
029700121109       IF NOT done.newFile;
029800121109         RETURN YPTCMSEQ_ESITO_OK;
029900121109       ENDIF;
030000121109
030100121109       RESET retField;
030200121109
030300121109       // Chiudo il file.
030400121109
030500121109       IF IFS_CloseFile( this.fd ) = -1;
030600121109         errnoPtr = GetErrno();
030700121109         DUMP(A);
030800121109         RETURN YPTCMSEQ_ESITO_ERRORE_STMF_CLOSE;
030900121109       ENDIF;
031000121109
031100121109       // Confermo/Cancello il file.
031200121109
031300121109       IF priCommit;
031400140117         IF IFS_RenameFileOrDirectoryKeep( this.stmf + STMF_TEMPORANEO
031500140117                                         : this.stmf + STMF_DEFINITIVO
031600140117                                         ) = -1;
031700121109           errnoPtr = GetErrno();
031800121109           DUMP(A);
031801140117           IFS_RemoveLinkToFile( this.stmf + STMF_TEMPORANEO );
031900121109           RETURN YPTCMSEQ_ESITO_ERRORE_STMF_RENAME;
032000121109         ENDIF;
032100121109       ELSE;
032200140117         IF IFS_RemoveLinkToFile( this.stmf + STMF_TEMPORANEO ) = -1;
032300121109           errnoPtr = GetErrno();
032400121109           DUMP(A);
032500121109           RETURN YPTCMSEQ_ESITO_ERRORE_STMF_REMOVE;
032600121109         ENDIF;
032700121109       ENDIF;
032800121109
032900121109       // Se arrivo qui vuol dire che è andato tutto bene ... forse ...
033000121109
033100121109       done.newFile = *OFF;
033200121109
033300121109       RETURN retField;
033400121109
033500121109      /END-FREE
033600121109     P YPTCMSEQ_EndFile...
033700121109     P                 E
033800121109
033900121109
034000121109     P*--------------------------------------------------
034100121109     P* Procedure name: YPTCMSEQ_NewRegistrazione
034200121109     P* Purpose:        Inizia una nuova registrazione.
034300121109     P* Returns:        Esito.
034400121109     P* Parameter:      priIdSocieta => ID società.
034500121109     P* Parameter:      priDataRegistrazione => Data registrazione.
034600121109     P* Parameter:      priNumeroRegistrazione => Data registrazione.
034700121109     P* Parameter:      priProfiloUtente => Profilo utente che ha inserito ...
034800121109     P*                          la registrazione in Proj.
034900121109     P* Parameter:      priKccBanca => Capo conto banca.
035000121109     P* Parameter:      priKscBanca => Sotto conto banca.
035100121109     P* Parameter:      priSegno => Dare/Avere
035200121109     P* Parameter:      priDivisa => Divisa.
035300121109     P* Parameter:      priImportoDivisa => Importo in divisa.
035400121109     P* Parameter:      priImporto => Importo in moneta corrente.
035500121109     P* Parameter:      priUnita => Unita.
035600121109     P* Parameter:      priDescrizioneBreve => Descrizione breve.
035700121109     P*--------------------------------------------------
035800121109     P YPTCMSEQ_NewRegistrazione...
035900121109     P                 B                   EXPORT
036000121109     D YPTCMSEQ_NewRegistrazione...
036100121109     D                 PI            10I 0
036200121113     D  priSys                        3P 0
036300121113     D  priNrAsReg                    9P 0
036400121113     D  priNrRigaM                    5P 0
036500121113     D  priIdSocieta                  3A
036600121109     D  priDataRegistrazione...
036700121109     D                                 D   DATFMT(*ISO)
036800121109     D  priNumeroRegistrazione...
036900121109     D                                9P 0
037000121109     D  priProfiloUtente...
037100121109     D                               10A
037200121109     D  priKccBanca                   6A
037300121109     D  priKscBanca                   8A
037400121109     D  priSegno                      1A
037500121109     D  priDivisa                     4A
037600121109     D  priImportoDivisa...
037700121112     D                               13P 2 CONST
037800121112     D  priImporto                   13P 2 CONST
037900121112     D  priCausale                    4A
038000121112     D  priDataOperazione...
038100121112     D                                 D
038200121112     D  priDataValuta...
038300121112     D                                 D
038400121112     D  priUnita                      8A
038500121116     D  priDescrizione...
038600121116     D                              250A
038700130206     D  priBanca                      4A
038800130206     D  priLinea                      4A
038900121109
039000121109     D retField        S             10I 0 STATIC
039100121113     D local           DS                  QUALIFIED INZ STATIC
039200121113     D  tipoOperazione...
039300121113     D                                4A
039400130206     D  tipoCalcoloDataValuta...
039500130206     D                                1A
039600121109
039700121109      /FREE
039800121109
039900121109       IF NOT done.newFile;
040000121109         RETURN YPTCMSEQ_ESITO_FILE_NON_INIZIATO;
040100121109       ENDIF;
040200121109
040300121109       IF done.newRegistrazione;
040400121109         RETURN YPTCMSEQ_ESITO_REGISTRAZIONE_GIA_INIZIATA;
040500121109       ENDIF;
040600121109
040700121112       IF priIdSocieta = *BLANK;
040800121112         RETURN YPTCMSEQ_ESITO_SOCIETA_OBBLIGATORIA;
040900121112       ENDIF;
041000121113
041100121113       RESET retField;
041200121113       RESET local;
041300131015       RESET anbob00f;
041301131015       RESET ndDvaBcoDs;
041400121113
041500130206       Proj_CausaleBanca( '01'
041600131015                        : SOCIETA_CAPOFILA
041700131015                        : *BLANK
041800131015                        : *BLANK
041900130206                        : priCausale
042000130206                        : priDataOperazione
042100131015                        : 'ANBOB'
042200131015                        : %SIZE(anbob00f)
042300130206                        : esitoProj
042400131015                        : anbob00f
042500130206                        );
042600130208
042700140721       IF esitoProj = *OFF AND anbob00f.bobCauTeso = '*OMIT';
043200130208         RETURN YPTCMSEQ_ESITO_CAUSALE_OMESSA;
043300130208       ENDIF;
043303131015
043304131015       Proj_CausaleBanca( '01'
043305131015                        : priIdSocieta
043306131015                        : priBanca
043307131015                        : priLinea
043308131015                        : priCausale
043309131015                        : priDataOperazione
043310131015                        : 'NDDVABCODS'
043311131015                        : %SIZE(ndDvaBcoDs)
043312131015                        : esitoProj
043313131015                        : ndDvaBcoDs
043314131015                        );
043315131015
043317131015       IF esitoProj = *ON;
043318140721       //RETURN YPTCMSEQ_ESITO_CAUSALE_OMESSA;
043319131015       ENDIF;
043400121113
043500121109       RESET yPtcMseqT;
043600121112
043700121109       yPtcMseqT.societa = priIdSocieta;
043800121109       this.nrPiteco += 1;
043900121109       yPtcMseqT.nrPiteco = this.nrPiteco;
044000121115       yPtcMseqT.dtReg = %CHAR(priDataRegistrazione : *ISO0);
044100121115       yPtcMseqT.nrReg = '0' + %EDITC(priNumeroRegistrazione : 'X');
044200121109       yPtcMseqT.user = priProfiloUtente;
044300121109       yPtcMseqT.data = %CHAR(%DATE() : *ISO0);
044400121109       yPtcMseqT.time = %CHAR(%TIME() : *ISO0) + '00';
044500121112       yPtcMseqT.codDigit = YPTCMSEQ_KCC_BANCHE_PITECO + '/' + priKscBanca;
044600121112       yPtcMseqT.dtOpe = %CHAR(priDataOperazione : *ISO0);
044700121115
044800131015       IF anbob00f.bobTipOpe = YPTCMSEQ_TIPO_OPERAZIONE_DISTINTA_PAGAMENTO
044900130206       OR ndDvaBcoDs.calDtVX17 = PROJ_CALCOLO_DATA_VALUTA_MANUALE;
045000130206         yPtcMseqT.dtVal = %CHAR(priDataValuta : *ISO0);
045100121115       ENDIF;
045200121115
045300130208       // Cambio la causale con l'eventuale causale tesoreria esterna.
045400130208
045500131015       IF anbob00f.bobCauTeso = *BLANK;
045600130208         yPtcMseqT.top = priCausale;
045700130208       ELSE;
045800131015         yPtcMseqT.top = anbob00f.bobCauTeso;
045900130208       ENDIF;
046000130208
046100121112       yPtcMseqT.segno = priSegno;
046200121112       yPtcMseqT.divOpe = priDivisa;
046300121112       yPtcMseqT.impOpe = priImportoDivisa;
046400121112       yPtcMseqT.impSoc = priImporto;
046500121112       yPtcMseqT.area = priUnita;
046600121116       yPtcMseqT.descrizion = priDescrizione;
046700121113
046800121113       YPTCMSEQ_SetRiferimentoBanca( priSys : priNrAsReg : priNrRigaM
046900121113                                   : priIdSocieta : priCausale
047000130206                                   : ndDvaBcoDs.tipOpeX17 );
047100121113
047200121113       YPTCMSEQ_SetRiferimentoInterno( priSys : priNrAsReg : priNrRigaM
047300121113                                     : priIdSocieta : priCausale
047400130206                                     : ndDvaBcoDs.tipOpeX17 );
047500121113
047600121113       YPTCMSEQ_SetCentroDiCostoByUnita( priIdSocieta : priUnita );
047700121109
047800121112       RESET yPtcMseqR;
047900121112
048000121112       yPtcMseqR.societa = yPtcMseqT.societa;
048100121112       yPtcMseqR.nrPiteco = yPtcMseqT.nrPiteco;
048200121112       yPtcMseqR.dtReg = yPtcMseqT.dtReg;
048300121115       yPtcMseqR.nrReg = yPtcMseqT.nrReg;
048400121112       yPtcMseqR.user = yPtcMseqT.user;
048500121112       yPtcMseqR.data = yPtcMseqT.data;
048600121112       yPtcMseqR.time = yPtcMseqT.time;
048700121112       yPtcMseqR.codDigit = priKccBanca + '/' + priKscBanca;
048800140721       yPtcMseqR.dtOpe = yPtcMseqT.dtOpe;
048801140721       yPtcMseqR.rifBanca = yPtcMseqT.rifBanca;
048900121113       yPtcMseqR.rifIntern = yPtcMseqT.rifIntern;
049000121115       yPtcMseqR.top = yPtcMseqT.top;
049100121112
049200121112       IF yPtcMseqT.segno = YPTCMSEQ_SEGNO_DARE;
049300121112         yPtcMseqR.segno = YPTCMSEQ_SEGNO_AVERE;
049400121112       ELSE;
049500121112         yPtcMseqR.segno = YPTCMSEQ_SEGNO_DARE;
049600121112       ENDIF;
049700121112
049800121112       yPtcMseqR.divOpe = yPtcMseqT.divOpe;
049900121112       yPtcMseqR.impOpe = yPtcMseqT.impOpe;
050000121112       yPtcMseqR.impSoc = yPtcMseqT.impSoc;
050100121112       yPtcMseqR.area = yPtcMseqT.area;
050200121113       yPtcMseqR.cdc = yPtcMseqT.cdc;
050300121112       yPtcMseqR.descrizion = yPtcMseqT.descrizion;
050400121112
050500121112       done.newRegistrazione = *ON;
050600121113
050700121109       RETURN retField;
050800121109
050900121109      /END-FREE
051000121109     P YPTCMSEQ_NewRegistrazione...
051100121109     P                 E
051200121112
051300121112
051400121112     P*--------------------------------------------------
051500121112     P* Procedure name: YPTCMSEQ_EndRegistrazione
051600121112     P* Purpose:        Fine registrazione.
051700121112     P* Returns:        Esito.
051800121112     P* Parameter:      priCommit => *ON = commit; *OFF = rollback.
051900121112     P*--------------------------------------------------
052000121112     P YPTCMSEQ_EndRegistrazione...
052100121112     P                 B                   EXPORT
052200121112     D YPTCMSEQ_EndRegistrazione...
052300121112     D                 PI            10I 0
052400121112     D  priCommit                      N   CONST
052500121112
052600121112     D retField        S             10I 0 STATIC
052700121112     D local           DS                  QUALIFIED INZ STATIC
052800121112     D  buf                        1604A
052900121112     D  bytes_write                  10I 0
053000121112
053100121112      /FREE
053200121112
053300121112       IF NOT done.newRegistrazione;
053400121112         RETURN YPTCMSEQ_ESITO_REGISTRAZIONE_NON_INIZIATA;
053500121112       ENDIF;
053600121112
053700121112       RESET retField;
053800121112
053900121112       IF priCommit;
054000121112
054100121112         local.buf = yPtcMseqT + CRLF + yPtcMseqR + CRLF;
054200121112
054300121112         local.bytes_write = IFS_WriteToDescriptor( this.fd
054400121112                                                  : %ADDR(local.buf)
054500121112                                                  : %SIZE(local.buf)
054600121112                                                  );
054700121112
054800121112         IF local.bytes_write = -1;
054900121112           errnoPtr = GetErrno();
055000121112           DUMP(A);
055100121112           RETURN YPTCMSEQ_ESITO_ERRORE_STMF_WRITE;
055200121112         ENDIF;
055300121112
055400121112       ENDIF;
055500121112
055600121112       done.newRegistrazione = *OFF;
055700121112
055800121112       RETURN retField;
055900121112
056000121112      /END-FREE
056100121112     P YPTCMSEQ_EndRegistrazione...
056200121112     P                 E
056300121112
056400121113
056500121113     P*--------------------------------------------------
056600121113     P* Procedure name: YPTCMSEQ_SetRiferimentoBanca
056700121113     P* Purpose:        Restituisce il riferimento banca.
056800121113     P* Returns:        Riferimento interno.
056900121113     P* Parameter:      priSys
057000121113     P* Parameter:      priNrAsReg
057100121113     P* Parameter:      priNrRigaM
057200121113     P* Parameter:      priRiferimentoBanca
057300121113     P*--------------------------------------------------
057400121113     P YPTCMSEQ_SetRiferimentoBanca...
057500121113     P                 B
057600121113     D YPTCMSEQ_SetRiferimentoBanca...
057700121113     D                 PI            10I 0
057800121113     D  priSys                        3P 0
057900121113     D  priNrAsReg                    9P 0
058000121113     D  priNrRigaM                    5P 0
058100121113     D  priIdSocieta                  3A
058200121113     D  priCausale                    4A
058300121113     D  priTipoOperazione...
058400121113     D                                4A
058500121113
058600121113     D retField        S             10I 0 STATIC
058700121113
058800121113      /FREE
058900121113
059000121113       RESET retField;
059100121113
059200121113       IF priTipoOperazione = *BLANK;
059300121113         RETURN retField;
059400121113       ENDIF;
059500121113
059600121113       SELECT;
059700121113
059800121113         WHEN priTipoOperazione = YPTCMSEQ_TIPO_OPERAZIONE_ASSEGNO;
059900121113
060000121113           EXEC SQL
060100121113             SELECT SUBSTR(ndmov00f.movDesBrev, 11, 10) AS NUMERO_ASSEGNO
060200121113                  , CONCAT('DISTINTA:', CHAR(ndreg00f.regNrDoc))
060300121113                    AS NUMERO_DISTINTA
060400121113               INTO :yPtcMseqT.rifBanca
060500121113                  , :yPtcMseqT.descrizion
060600121113               FROM ndmov00f
060700121113               JOIN ndreg00f
060800121113                 ON ndreg00f.regSys = :priSys
060900121113                AND ndreg00f.regNrAsReg = :priNrAsReg
061000121113               WHERE ndmov00f.movSys = :priSys
061100121113                 AND ndmov00f.movNrAsReg = :priNrAsReg
061200121113                 AND ndmov00f.movNrRigaM = :priNrRigaM
061300121113           ;
061400121113
061500121113         WHEN priTipoOperazione = YPTCMSEQ_TIPO_OPERAZIONE_DISTINTA_PAGAMENTO;
061600121113
061700121113           EXEC SQL
061800121116             SELECT CONCAT('D', DIGITS(ndreg00f.regNrDoc))
061900121116                  , CONCAT('DISTINTA:', CHAR(ndreg00f.regNrDoc))
062000121113               INTO :yPtcMseqT.rifBanca
062100121116                  , :yPtcMseqT.descrizion
062200121115               FROM ndreg00f
062300121115               WHERE ndreg00f.regSys = :priSys
062400121115                 AND ndreg00f.regNrAsReg = :priNrAsReg
062500121113           ;
062600121113
062700121113         WHEN priTipoOperazione = YPTCMSEQ_TIPO_OPERAZIONE_VERSAMENTO;
062800121113
062900121113           EXEC SQL
063000121113             SELECT CONCAT('V', DIGITS(ndreg00f.regNrReg)) AS NUMERO_VERSAMENTO
063100121113               INTO :yPtcMseqT.rifBanca
063200121113               FROM ndreg00f
063300121113               WHERE ndreg00f.regSys = :priSys
063400121113                 AND ndreg00f.regNrAsReg = :priNrAsReg
063500121113           ;
063600121113
063700121113       ENDSL;
063800121113
063900121113       SELECT;
064000121113         WHEN sqlCode < *ZERO;
064100121113           DUMP(A);
064200121113           RETURN sqlCode;
064300121113       ENDSL;
064400121113
064500121113       RETURN retField;
064600121113
064700121113      /END-FREE
064800121113     P YPTCMSEQ_SetRiferimentoBanca...
064900121113     P                 E
065000121113
065100121113
065200121113     P*--------------------------------------------------
065300121113     P* Procedure name: YPTCMSEQ_SetRiferimentoInterno
065400121113     P* Purpose:        Restituisce il riferimento interno.
065500121113     P* Returns:        Riferimento interno.
065600121113     P* Parameter:      priSys
065700121113     P* Parameter:      priNrAsReg
065800121113     P* Parameter:      priNrRigaM
065900121113     P*--------------------------------------------------
066000121113     P YPTCMSEQ_SetRiferimentoInterno...
066100121113     P                 B
066200121113     D YPTCMSEQ_SetRiferimentoInterno...
066300121113     D                 PI            10I 0
066400121113     D  priSys                        3P 0
066500121113     D  priNrAsReg                    9P 0
066600121113     D  priNrRigaM                    5P 0
066700121113     D  priIdSocieta                  3A
066800121113     D  priCausale                    4A
066900121113     D  priTipoOperazione...
067000121113     D                                4A
067100121113
067200121113     D retField        S             10I 0 STATIC
067300121113
067400121113      /FREE
067500121113
067600121113       RESET retField;
067700121113
067800121113       IF priTipoOperazione <> YPTCMSEQ_TIPO_OPERAZIONE_DISTINTA_PAGAMENTO;
067900121113         RETURN retField;
068000121113       ENDIF;
068100121113
068200121113       // Calcolo il numero di disposizioni della distinta.
068500121115
068600121113       EXEC SQL
068700121113         WITH d (ddt, ndt)
068800121113           AS ( SELECT ndreg00f.regDtDoc, ndreg00f.regNrDoc
068900121113                  FROM ndreg00f
069000121113                 WHERE ndreg00f.regSys = :priSys
069100121113                   AND ndreg00f.regNrAsReg = :priNrAsReg )
069200121115           SELECT DIGITS(DEC(COUNT(DISTINCT yhbos00f.bonNbn), 10, 0))
069300121113             INTO :yPtcMseqT.rifIntern
069400121113             FROM d
069500121113             JOIN yhbos00f
069600121113               ON yhbos00f.bonEla = ''
069700121113              AND yhbos00f.bonSocieta = :priIdSocieta
069800121113              AND yhbos00f.bonDdt = d.ddt
069900121113              AND yhbos00f.bonNdt = d.ndt
070000121113       ;
070100121113
070200121113       SELECT;
070300121113         WHEN sqlCode < *ZERO;
070400121113           DUMP(A);
070500121113           RETURN sqlCode;
070600121113       ENDSL;
070900121113
071000121113       RETURN retField;
071100121113
071200121113      /END-FREE
071300121113     P YPTCMSEQ_SetRiferimentoInterno...
071400121113     P                 E
071500121113
071600121113     P*--------------------------------------------------
071700121113     P* Procedure name: YPTCMSEQ_SetCentroDiCostoByUnita
071800121113     P* Purpose:        Imposta il centro di costo dall'unità.
071900121113     P* Returns:        Esito.
072000121113     P* Parameter:      priIdSocieta
072100121113     P* Parameter:      priUnita
072200121113     P*--------------------------------------------------
072300121113     P YPTCMSEQ_SetCentroDiCostoByUnita...
072400121113     P                 B
072500121113     D YPTCMSEQ_SetCentroDiCostoByUnita...
072600121113     D                 PI            10I 0
072700121113     D  priIdSocieta                  3A
072800121113     D  priUnita                      8A
072900121113
073000121113     D retField        S             10I 0 STATIC
073100121113
073200121113      /FREE
073300121113
073400121113       RESET retField;
073500121113
073600121113       EXEC SQL
073700121113         SELECT anent00f.entEntita
073800121113           INTO :yPtcMseqT.cdc
073900121113           FROM anent00f
074000121113           WHERE anent00f.entSocieta = :priIdSocieta
074100121113             AND anent00f.entTpRifAn = 'CDC'
074200121113             AND anent00f.entEntita = :priUnita
074300121113       ;
074400121113
074500121113       SELECT;
074600121113         WHEN sqlCode < *ZERO;
074700121113           DUMP(A);
074800121113           RETURN sqlCode;
074900121113       ENDSL;
075000121113
075100121113       RETURN retField;
075200121113
075300121113      /END-FREE
075400121113     P YPTCMSEQ_SetCentroDiCostoByUnita...
075500121113     P                 E
075600121113
