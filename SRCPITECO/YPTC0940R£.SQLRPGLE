000100130620     ***************************************************************************
000101130620     **
000102130620     ** Questo programma importa in Proj nella table NDRI100F i movimenti di
000103130620     ** accredito bonifico (causale 48xx) a cui Piteco non è riuscito ad
000104130620     ** attribuire il cliente.
000106130620     **
000107130620     ***************************************************************************
000108130620     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJXBND':'QC2LE':'YPTC':'TRUL')
000109130620
000110130620     ***************************************************************************
000111130620     **
000112130620     ** File.
000113130620     **
000114130620     ***************************************************************************
000115130620     Fy094log   O    F  132        PRINTER EXTFILE('QSYSPRT')
000116130620
000117130620     ***************************************************************************
000118130620     **
000119130620     ** Costanti.
000120130620     **
000121130620     ***************************************************************************
000122130620      /COPY GAITRASRC/SRCCONST,TRULDIRINF
000123140319      /COPY GAITRASRC/SRCCONST,TRULH2C
000124131021     D BANCA...
000125131021     D                 S              4A   INZ('0048')
000126131021     D LINEA...
000127131021     D                 S              4A   INZ('CCO')
000128131021     D OPERAZIONE...
000129131021     D                 S              4A   INZ('48')
000130131021     D PROJ_CONTABILIZZATO...
000131131021     D                 S              1A   INZ('2')
000132131021     D SOCIETA...
000133131021     D                 S              3A   INZ('201')
000134130620     D SQLCODE_ROW_NOT_FOUND...
000135130620     D                 C                   100
000136130620     D SQLCODE_ALREADY_EXISTS...
000137130620     D                 C                   -601
000138130621     D ESITO_ERROR...
000139130621     D                 C                   -1
000140130621     D ESITO_OK...
000141130621     D                 C                   0
000142140319     D CPF7302...
000143140319     D                 C                   'CPF7302'
000144130620
000145130620     ***************************************************************************
000146130620     **
000147130620     ** Strutture dati.
000148130620     **
000149130620     ***************************************************************************
000150130620     D kpjba         E DS                  QUALIFIED
000151130620     D log             DS           132
000152130620     D this            DS                  QUALIFIED INZ
000153130620     D  cmd                         512A   VARYING
000154130621     D  count                        10I 0
000155130620     D  dir                         256A   VARYING
000156130621     D  esito                        10I 0
000157130620     D  idxDirObj                          LIKEDS(DIRINF_idxDirObj)
000158130620
000159130620     ***************************************************************************
000160130620     **
000161130620     ** Campi.
000162130620     **
000163130620     ***************************************************************************
000164130620     D exceptionMessageID...
000165130620     D                 S              7A   IMPORT('_EXCP_MSGID')
000166130621
000167130621     D*--------------------------------------------------
000168130621     D* Procedure name: Main
000169130621     D* Purpose:
000170130621     D* Returns:        Esito.
000171130621     D*--------------------------------------------------
000172130621     D Main            PR            10I 0
000173140319
000174140319     D*--------------------------------------------------
000175140319     D* Procedure name: CvtHtml2Char
000176140319     D* Purpose:        Converte i caratteri HTML in EBCDIC.
000177140319     D* Returns:        Esito.
000178140319     D*--------------------------------------------------
000179140319     D CvtHtml2Char    PR            10I 0
000180140319
000181140319
000182130621
000183130620     ***************************************************************************
000184130620     **
000185130620     ** Prototipi.
000186130620     **
000187130620     ***************************************************************************
000188130620      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
000189130620      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000190130621      /COPY GAITRASRC/SRCPROTOPR,TRULDIRINF
000191140319      /COPY GAITRASRC/SRCPROTOPR,TRULH2C
000192130620
000193130620     ***************************************************************************
000194130620     **
000195130620     ** Parametri.
000196130620     **
000197130620     ***************************************************************************
000198130620     C     *ENTRY        PLIST
000199130620     C                   PARM                    kpjba
000200130620
000201130620     ***************************************************************************
000202130620     **
000203130620     ** Main.
000204130620     **
000205130620     ***************************************************************************
000206130620
000207130620      /FREE
000208130620
000209130620       *INLR = *ON;
000210130620
000211130621       log = 'Inizio importazione bonifici da Piteco.';
000212130621       WRITE y094log log;
000213130621
000214130621       IF Main() < *ZERO;
000215130621         log = 'Fine anomala.';
000216130621       ELSE;
000217130621         log = 'Fine.';
000218130621       ENDIF;
000374130620
000375130621       WRITE y094log log;
000376130621
000377130621       // Prima di uscire faccio un po' di pulizia.
000378130621
000379130621       EXEC SQL DROP TABLE QTEMP/PITECO48;
000380130621       ExecuteCommand('DLTF FILE(QTEMP/ERRRCDFILE)');
000388130621
000389130620      /END-FREE
000390130621
000391130621     P*--------------------------------------------------
000392130621     P* Procedure name: Main
000393130621     P* Purpose:
000394130621     P* Returns:        Esito.
000395130621     P*--------------------------------------------------
000396130621     P Main            B
000397130621     D Main            PI            10I 0
000398130621
000399130621     D retField        S             10I 0
000400130621     D local           DS                  QUALIFIED INZ
000404130621     D  esito                        10I 0
000406130621
000407130621      /FREE
000430130621
000431131122       // Cancello i movimenti della banca 0048 più vecchi di 6 mese e registrati.
000432131021
000433131021       EXEC SQL
000434131021         DELETE FROM ndri100f
000435131021           WHERE ri1Societa = :SOCIETA
000436131021             AND ri1Banca = :BANCA
000437131021             AND ri1Linea = :LINEA
000438131122             AND ri1Dop < CURRENT_DATE - 6 MONTH
000439131024             AND ri1Sta = :PROJ_CONTABILIZZATO
000440131021            WITH NC
000441131021       ;
000442131021
000443131021       SELECT;
000444131021         WHEN sqlCode < *ZERO;
000445131021           DUMP(A);
000446131021           log = 'ERRORE SQLSTATE ' + sqlState + '.';
000447131021           WRITE y094log log;
000448131021       ENDSL;
000449131021
000450130621       // Recupero la directory da cui prendere i files.
000451130621
000452130621       EXEC SQL
000453130621         SELECT RTRIM(tntbe00f.tbeUni)
000454130621           INTO :this.dir
000455130621           FROM tntbe00f
000456130621          WHERE tntbe00f.tbeCod = '011' AND tntbe00f.tbeKe1 = '*DIR'
000457130621            AND tntbe00f.tbeKe2 = '*BONIFICI_CLI' AND tntbe00f.tbeLin = ''
000458130621            AND tntbe00f.tbeSif = '' AND tntbe00f.tbeAtb = ''
000459130621       ;
000460130621
000461130621       SELECT;
000462130621         WHEN sqlCode < *ZERO;
000463130621           DUMP(A);
000464130621           log = 'ERRORE SQLSTATE ' + sqlState + '.';
000465130621           WRITE y094log log;
000466130621           RETURN sqlCode;
000467130621         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
000468130621           DUMP(A);
000469130621           log = 'ERRORE Tabella 011 *DIR *BONIFICI_CLI non trovata.';
000470130621           WRITE y094log log;
000471130621           RETURN ESITO_ERROR;
000472130621       ENDSL;
000473130621
000474130621       retField = DIRINF_NewRichiamo(this.dir);
000475130621
000476130621       IF retField < DIRINF_ESITO_OK;
000477130621         log = 'ERRORE ' + %CHAR(retField) + ' da DIRINF_NewRichiamo().';
000478130621         WRITE y094log log;
000479130621         RETURN retField;
000480130621       ENDIF;
000481130621
000482130621       // Mi interessano solo i file comma separated value.
000483130621
000484130621       DIRINF_SetWhereObjectName(' LIKE ''%.csv''');
000485130621
000486130621       retField = DIRINF_RunRichiamo();
000487130621
000488130621       IF retField < DIRINF_ESITO_OK;
000489130621         log = 'ERRORE ' + %CHAR(retField) + ' da DIRINF_RunRichiamo().';
000490130621         WRITE y094log log;
000491130621         RETURN retField;
000492130621       ENDIF;
000493130621
000494130621       // Mi creo una table DB2 temporanea dove parcheggiare i dati.
000495130621
000496130621       EXEC SQL
000497130621         CREATE TABLE QTEMP/PITECO48
000498131021           ( SOCIETA CHAR (3) NOT NULL WITH DEFAULT
000499130621           , DATA_OPERAZIONE DATE NOT NULL WITH DEFAULT
000500130621           , DATA_VALUTA DATE NOT NULL WITH DEFAULT
000501131021           , CAUSALE CHAR (4) NOT NULL WITH DEFAULT
000502131021           , DIVISA CHAR (3) NOT NULL WITH DEFAULT
000503131021           , IMPORTO DEC (15, 2) NOT NULL WITH DEFAULT
000504131120           , DESCRIZIONE CHAR (600) NOT NULL WITH DEFAULT )
000505130621       ;
000506130621
000507130621       SELECT;
000508130621         WHEN sqlCode = SQLCODE_ALREADY_EXISTS;
000509130621           // La table esiste già, no problem.
000510130621         WHEN sqlCode < *ZERO;
000511130621           DUMP(A);
000512130621           log = 'ERRORE SQLSTATE ' + sqlState + '.';
000513130621           WRITE y094log log;
000514130621           RETURN sqlCode;
000515130621       ENDSL;
000516130621
000517130621       // Creo un PF-SRC dove registrare eventuali errori di copia.
000518130621
000519130621       this.cmd = 'CRTPF FILE(QTEMP/ERRRCDFILE) RCDLEN(240) FILETYPE(*SRC)';
000520130621
000521130621       IF ExecuteCommand(this.cmd) <> *ZERO;
000523130621         log = 'ERRORE ' + exceptionMessageID + '.';
000524130621         WRITE y094log log;
000525130621         log = 'Elaborazione terminata in modo anomalo.';
000526130621         WRITE y094log log;
000527140319         DUMP(A);
000528130621         RETURN ESITO_ERROR;
000529130621       ENDIF;
000542130621
000543130621       // Per ogni file.csv esistente nella directory eseguo il comendo di copia.
000544130621
000545130621       DOU local.esito = DIRINF_ESITO_ROW_NOT_FOUND;
000546130621
000547130621         local.esito = DIRINF_GetNextObjectName(this.idxDirObj);
000548130621
000549130621         IF local.esito < DIRINF_ESITO_OK;
000550130621           log = 'ERRORE ' + %CHAR(local.esito)
000551130621               + ' da DIRINF_GetNextObjectName().';
000552130621           WRITE y094log log;
000553130621           retField = local.esito;
000554130621           LEAVE;
000555130621         ENDIF;
000556130621
000557130621         IF local.esito = DIRINF_ESITO_ROW_NOT_FOUND;
000558130621           LEAVE;
000559130621         ENDIF;
000560130621
000561130621         log = 'File ' + this.idxDirObj.dirNam + '/' + this.idxDirObj.objNam;
000562130621         WRITE y094log log;
000580130621
000581140319         IF CvtHtml2Char() < ESITO_OK;
000582140319           ITER;
000583140319         ENDIF;
000601130621
000602130621         EXEC SQL
000603130621           SELECT COUNT(*)
000604130621             INTO :this.count
000605130621             FROM QTEMP/ERRRCDFILE
000606130621         ;
000607130621
000608130621         IF this.count > *ZERO;
000609130621           log = 'ERRORE Presenti dati non validi. Vedere spool QPSUPRTF.';
000610130621           WRITE y094log log;
000611130621           local.esito = ESITO_ERROR;
000612130621           ExecuteCommand('QSYS/STRSEU SRCFILE(QTEMP/ERRRCDFILE) +
000613130621                           SRCMBR(ERRRCDFILE) OPTION(6)');
000614130621           ITER;
000615130621         ENDIF;
000616130621
000617130621         EXEC SQL
000618130705           INSERT INTO ndri100f
000619131021             SELECT :BANCA AS RI1BANCA
000620131021                  , :LINEA AS RI1LINEA
000621130621                  , CURRENT_DATE AS RI1DAT
000622130705                  , piteco48.data_operazione AS RI1DOP
000623130705                  , piteco48.data_valuta AS RI1DVA
000624131128                  , LEFT(piteco48.causale, 2) AS RI1COD
000625131021                  , LEFT(piteco48.descrizione, 120) AS RI1DES
000626130621                  , 'D' AS RI1DA
000627130705                  , piteco48.importo AS RI1IMP
000628130705                  , piteco48.divisa AS RI1DIV
000629130621                  , '' AS RI1STA
000630130621                  , 0 AS RI1SYSM
000631130621                  , 0 AS RI1NCB
000632130621                  , 0 AS RI1NAS
000633130621                  , 0 AS RI1SYSN
000634130621                  , 0 AS RI1NUM
000635130621                  , '' AS RI1ABI
000636130621                  , '' AS RI1SIA
000637130621                  , '' AS RI1CCB
000638130621                  , 0 AS RI1NRE
000639130621                  , '0001-01-01' AS RI1DRG
000640130621                  , 0 AS RI1NRG
000641130621                  , 0 AS RI1NRJ
000642130705                  , piteco48.societa AS RI1SOCIETA
000643130621                  , '' AS RI1STATUS
000644130621                  , CURRENT_DATE AS RI1DATACAR
000645130705                  , (SELECT MAX(ri1NrCar) FROM ndri100f) + 1 AS RI1NRCAR
000646130705                  , ROW_NUMBER() OVER ( ORDER BY piteco48.importo )
000647130705                    AS RI1PROGCAR
000648130621                  , 'C' AS RI1PROVEN
000649130705                  , piteco48.descrizione AS RI1DESORI
000650130705             FROM qtemp/piteco48
000651141030             WHERE LEFT(piteco48.causale, 2)
000652160419                   IN ('48','ZI','27','07','79','46','90','24','09')
000653130621             WITH UR
000654130621         ;
000655130621
000656130621         SELECT;
000657130621           WHEN sqlCode < *ZERO;
000658130621             DUMP(A);
000659131010             local.esito = ESITO_ERROR;
000660130621             log = 'ERRORE SQLSTATE ' + sqlState + '.';
000661130621             WRITE y094log log;
000662130621             log = 'Elaborazione terminata in modo anomalo.';
000663130621             WRITE y094log log;
000664130621             EXEC SQL ROLLBACK HOLD;
000665130621             ITER;
000666130621         ENDSL;
000667130621
000668130621         // Se l'inserimento è andato bene cancello il file.csv.
000669130621
000670130621         this.cmd = 'RMVLNK OBJLNK('''
000671130621                  + this.idxDirObj.dirNam + '/' + this.idxDirObj.objNam
000672130621                  + ''')';
000673130621
000674130621         IF ExecuteCommand(this.cmd) <> *ZERO;
000675130621           local.esito = ESITO_ERROR;
000676130621           log = 'ERRORE ' + exceptionMessageID
000677130621               + ' comando ' + this.cmd
000678130621               + '.';
000679130621           WRITE y094log log;
000680140319           DUMP(A);
000681130621         ENDIF;
000682130621
000683130621         // Se arrivo qui significa che è andato tutto bene ... forse ...
000684130621
000685130621         IF local.esito < ESITO_OK;
000686130621           EXEC SQL ROLLBACK HOLD;
000687130621           log = 'Elaborazione terminata in modo anomalo.';
000688131010           ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
000689130621         ELSE;
000690130621           EXEC SQL COMMIT HOLD;
000691130621           log = 'Elaborazione eseguita con successo.';
000692130621         ENDIF;
000693130621
000694130621         WRITE y094log log;
000695130621
000696130621       ENDDO;
000697130621
000698130621       DIRINF_EndRichiamo();
000699130621
000700130621       RETURN retField;
000701130621
000702130621      /END-FREE
000703130621     P Main            E
000704130621
000705140319
000706140319     P*--------------------------------------------------
000707140319     P* Procedure name: CvtHtml2Char
000708140319     P* Purpose:        Converte i caratteri HTML in EBCDIC.
000709140319     P* Returns:        Esito.
000710140319     P*--------------------------------------------------
000711140319     P CvtHtml2Char    B
000712140319     D CvtHtml2Char    PI            10I 0
000713140319
000714140319     D retField        S             10I 0 STATIC
000715140319     D local           DS                  QUALIFIED STATIC
000716140320     D  pitecoh2ci                 1000A
000717140320     D  pitecoh2co                         LIKE(local.pitecoh2ci)
000719140319     D  esito                        10I 0
000720140319
000721140319      /FREE
000722140319
000723140319       RESET retField;
000724140319       RESET local;
000725140319
000726140319       // Creo un PF di transito per eseguire degli aggiustamenti sui dati.
000727140319
000728140319       this.cmd = 'CRTPF FILE(GAITRATMP/PITECOH2C) RCDLEN('
000729140320                + %CHAR(%SIZE(local.pitecoh2ci))
000730140319                + ')';
000731140319
000732140319       IF ExecuteCommand(this.cmd) <> *ZERO AND exceptionMessageID <> CPF7302;
000733140319         log = 'ERRORE ' + exceptionMessageID + '.';
000734140319         WRITE y094log log;
000735140319         log = 'Elaborazione terminata in modo anomalo.';
000736140319         WRITE y094log log;
000737140319         DUMP(A);
000738140319         RETURN ESITO_ERROR;
000739140319       ENDIF;
000740140319
000741140319       // Copia dei dati.
000742140319
000743140319       this.cmd = 'CPYFRMSTMF FROMSTMF('''
000744140319                + this.idxDirObj.dirNam + '/' + this.idxDirObj.objNam
000745140319                + ''') TOMBR(''/QSYS.LIB/GAITRATMP.LIB/PITECOH2C.FILE'
000746140319                + '/PITECOH2C.MBR'') MBROPT(*REPLACE) DBFCCSID(1144)';
000747140319
000748140319       IF ExecuteCommand(this.cmd) <> *ZERO;
000750140319         log = 'ERRORE ' + exceptionMessageID
000751140319             + ' comando ' + this.cmd + '.';
000752140319         WRITE y094log log;
000753140319         DUMP(A);
000754140319         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
000755140319         RETURN ESITO_ERROR;
000756140319       ENDIF;
000757140319
000758140319       // Conversione.
000759140319
000760140319       EXEC SQL
000761140319         DECLARE pitecoh2c NO SCROLL CURSOR FOR
000762140319           SELECT pitecoh2c
000763140319           FROM gaitratmp/pitecoh2c
000764140319           FOR UPDATE OF pitecoh2c
000765140319           WITH NC
000766140319       ;
000767140319
000768140319       EXEC SQL
000769140319         OPEN pitecoh2c
000770140319       ;
000771140319
000772140319       SELECT;
000773140319         WHEN sqlCode < *ZERO;
000774140319           DUMP(A);
000775140319           RETURN sqlCode;
000776140319       ENDSL;
000777140319
000778140319       DOU sqlCode < *ZERO;
000779140319
000780140319         EXEC SQL
000781140319           FETCH NEXT FROM pitecoh2c
000782140320             INTO :local.pitecoh2ci
000783140319         ;
000784140319
000785140319         SELECT;
000786140319           WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
000787140319             LEAVE;
000788140319           WHEN sqlCode < *ZERO;
000789140319             DUMP(A);
000790140319             retField = sqlCode;
000791140319             LEAVE;
000792140319         ENDSL;
000793140319
000794140319         TrulH2c( H2C_HTML2CHAR
000795140320                : local.esito
000796140320                : local.pitecoh2ci
000797140320                : %SIZE(local.pitecoh2ci)
000798140320                : local.pitecoh2co
000799140320                : %SIZE(local.pitecoh2co)
000800140319                );
000801140319
000802140320         IF local.esito = H2C_ESITO_OK AND local.pitecoh2co <> local.pitecoh2ci;
000803140319           EXEC SQL
000804140319             UPDATE gaitratmp/pitecoh2c AS pitecoh2c
000805140320               SET pitecoh2c.pitecoh2c = :local.pitecoh2co
000806140319               WHERE CURRENT OF pitecoh2c
000807140319           ;
000808140319         ENDIF;
000809140319
000810140319       ENDDO;
000811140319
000812140319       EXEC SQL
000813140319         CLOSE pitecoh2c
000814140319       ;
000815140320
000816140320       TrulH2c( H2C_SETONLR );
000823140319
000824140319       IF local.esito = ESITO_OK;
000825140319
000826140319         this.cmd = 'CPYFRMIMPF FROMFILE(GAITRATMP/PITECOH2C) +
000827140319                    TOFILE(QTEMP/PITECO48) MBROPT(*REPLACE) RCDDLM(*ALL) +
000828140319                    RMVBLANK(*TRAILING) FLDDLM('';'') DECPNT(*COMMA) +
000829140319                    STRDLM(*NONE) ERRRCDFILE(QTEMP/ERRRCDFILE) +
000830140319                    ERRRCDOPT(*REPLACE)';
000831140319
000832140319         IF ExecuteCommand(this.cmd) <> *ZERO;
000833140319           log = 'ERRORE ' + exceptionMessageID
000834140319               + ' comando ' + this.cmd + '.';
000835140319           WRITE y094log log;
000836140319           DUMP(A);
000837140319           retField = ESITO_ERROR;
000838140319           ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
000839140319         ENDIF;
000840140319
000841140319       ENDIF;
000842140319
000843140319       // Pulizie finali.
000844140319
000845140319       this.cmd = 'DLTF FILE(GAITRATMP/PITECOH2C)';
000846140319
000847140319       IF ExecuteCommand(this.cmd) <> *ZERO;
000848140319       ENDIF;
000849140319
000850140319       RETURN retField;
000851140319
000852140319      /END-FREE
000853140319     P CvtHtml2Char    E
000854140319
