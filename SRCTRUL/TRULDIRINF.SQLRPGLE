000100110623     ***************************************************************************
000200110623     **
000300110623     ** Il comando RTVDIRINF (Richiamo informazioni indirizzario) viene
000400110623     ** utilizzato per raccogliere attributi per indirizzari e file
000500110623     ** nell'Integrated File System. Le informazioni raccolte sono
000600110623     ** memorizzate nei file di database che sono denominati utilizzando il
000700110623     ** prefisso file di informazioni specificato dal parametro INFFILEPFX.
000800110623     ** I file sono creati nella libreria specificata dal parametro INFLIB.
000900110624     ** Limitazione:
001000110624     ** - Le informazioni sull'indirizzario possono essere richiamate solo
001100110624     **   per file system caricati.
001200110624     ** - Informazioni sull'indirizzario possono essere richiamate solo
001300110624     **   per file system locali.
001400110624     ** - È necessario disporre dell'autorizzazione speciale a tutti gli
001500110624     **   oggetti (*ALLOBJ) per eseguire questo comando, quindi
001600110624     **   QUESTO SERVICE PROGRAM DEVE ESSERE CREATO DA QSECOFR e poi
001700110624     **   CHGSRVPGM SRVPGM(TRULDIRINF) USRPRF(*OWNER)
001800110624     **
001900110623     ***************************************************************************
002000110624     H NOMAIN
002100110623
002200110623     ***************************************************************************
002300110623     **
002400110623     ** Strutture dati.
002500110623     **
002600110623     ***************************************************************************
002700110623      /COPY QSYSINC/QRPGLESRC,QMHRCVPM
002800110623      /COPY QSYSINC/QRPGLESRC,QUSEC
002900110623     D done            DS                  QUALIFIED INZ
003000110623     D  newRichiamo                    N
003100110624     D  runRichiamo                    N
003200110623     D rtvDirInf       DS                  QUALIFIED INZ
003300110624     D  dir                         512A   VARYING
003400110624     D  objNamLike                  256A   VARYING
003500110623     D  subTree                       5A
003600110623     D  infFilePfx                    9A
003700130129     D  infLib                       10A   VARYING INZ(GAITRATMP)
003800110623     D  infFileD                     10A   VARYING
003900110623     D  infFileO                     10A   VARYING
004000110624     D  rtvDirInf                  1024A   VARYING
004100110623     D rcvm0100        DS                  QUALIFIED
004200110623     D  QMHM010001                         LIKE(QMHM010001)
004300110623     D  QMHMD00                      30A
004400110624     D stm             DS                  QUALIFIED INZ
004500110624     D  select                      255A   VARYING
004600110624     D  from                        255A   VARYING
004700110624     D  join                        255A   VARYING
004800110624     D  where                       255A   VARYING
004900110624     D  orderBy                     255A   VARYING
005000110624     D  stm                        1024A   VARYING
005100110623
005200110623     ***************************************************************************
005300110623     **
005400110623     ** Costanti.
005500110623     **
005600110623     ***************************************************************************
005700110623      /COPY GAITRASRC/SRCCONST,TRULDIRINF
005800130129     D GAITRATMP       C                   'GAITRATMP'
005900110623
006000110623     ***************************************************************************
006100110623     **
006200110623     ** Campi.
006300110623     **
006400110623     ***************************************************************************
006500110623
006600110623     ***************************************************************************
006700110623     **
006800110623     ** Prototipi.
006900110623     **
007000110623     ***************************************************************************
007100110623      /COPY GAITRASRC/SRCPROTOPR,QCMDEXC
007200110623      /COPY GAITRASRC/SRCPROTOPR,QMHRCVPM
007300110623      /COPY GAITRASRC/SRCPROTOPR,TRULDIRINF
007400110623
007500110623
007600110623     P*--------------------------------------------------
007700110623     P* Procedure name: DIRINF_NewRichiamo
007800110623     P* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
007900110623     P* Returns:        Esito.
008000110623     P* Parameter:      priIndirizzario
008100110623     P* Parameter:      priAlberoSecondarioIndirizzari
008200110623     P* Parameter:      priPrefissoFileInformazioni
008300110623     P* Parameter:      priLibreriaInformazioni
008400110623     P*--------------------------------------------------
008500110623     P DIRINF_NewRichiamo...
008600110623     P                 B                   EXPORT
008700110623     D DIRINF_NewRichiamo...
008800110623     D                 PI            10I 0
008900110623     D  priIndirizzario...                                                      1
009000110623     D                              512A   VARYING
009100110623     D                                     CONST
009200110624     D  priAlberoSecondarioIndirizzari...                                       2
009300110623     D                                5A   CONST
009400110623     D                                     OPTIONS(*NOPASS:*OMIT)
009500110624     D  priPrefissoFileInformazioni...                                          3
009600110623     D                                9A   CONST
009700110623     D                                     OPTIONS(*NOPASS:*OMIT)
009800110624     D  priLibreriaInformazioni...                                              4
009900110623     D                               10A   CONST
010000110623     D                                     OPTIONS(*NOPASS)
010100110623
010200110623     D retField        S             10I 0
010300110623
010400110623      /FREE
010500110623
010600110624       IF done.newRichiamo;
010700110624         RETURN DIRINF_ESITO_RICHIAMO_GIA_PREPARATO;
010800110624       ENDIF;
010900110624
011000110623       RESET rtvDirInf;
011100110623
011200110624       IF %LEN(priIndirizzario) = *ZERO OR priIndirizzario = *BLANK;
011300110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
011400110624       ENDIF;
011500110624
011600110623       rtvDirInf.dir = priIndirizzario;
011700110623
011800110624       IF %PARMS() > 1 AND %ADDR(priAlberoSecondarioIndirizzari) <> *NULL
011900110623       AND priAlberoSecondarioIndirizzari <> *BLANK;
012000110623         rtvDirInf.subTree = priAlberoSecondarioIndirizzari;
012100110623       ENDIF;
012200110623
012300110624       IF %PARMS() > 2 AND %ADDR(priPrefissoFileInformazioni) <> *NULL
012400110623       AND priPrefissoFileInformazioni <> *BLANK;
012500110623         rtvDirInf.infFilePfx = priPrefissoFileInformazioni;
012600110623       ENDIF;
012700110623
012800110624       IF %PARMS() > 3 AND priLibreriaInformazioni <> *BLANK;
012900110623         IF priLibreriaInformazioni = DIRINF_INFLIB_QTEMP;
013000110623           RETURN DIRINF_ESITO_LIBRERIA_NON_VALIDA;
013100110623         ENDIF;
013200110623         rtvDirInf.infLib = priLibreriaInformazioni;
013300110623       ENDIF;
013400110623
013500110624       RESET stm;
013600110623       done.newRichiamo = *ON;
013700110623
013800110623       RETURN retField;
013900110623
014000110623      /END-FREE
014100110623     P DIRINF_NewRichiamo...
014200110623     P                 E
014300110624
014400110624
014500110624     P*--------------------------------------------------
014600110624     P* Procedure name: DIRINF_SetWhereObjectName
014700110624     P* Purpose:        Imposta la WHERE sul nome oggetto.
014800110624     P* Returns:        Esito.
014900110624     P* Parameter:      priWhereObjectNameSearchCondition => WHERE object name
015000110624     P*                 search condition.
015100110624     P*--------------------------------------------------
015200110624     P DIRINF_SetWhereObjectName...
015300110624     P                 B                   EXPORT
015400110624     D DIRINF_SetWhereObjectName...
015500110624     D                 PI            10I 0
015600110624     D  priWhereObjectNameSearchCondition...
015700110624     D                              255A   VARYING
015800110624     D                                     CONST
015900110624
016000110624     D retField        S             10I 0
016100110624
016200110624      /FREE
016300110624
016400110624       IF NOT done.newRichiamo;
016500110624         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
016600110624       ENDIF;
016700110624
016800110624       IF %LEN(priWhereObjectNameSearchCondition) = *ZERO
016900110624       OR priWhereObjectNameSearchCondition = *BLANK;
017000110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
017100110624       ENDIF;
017200110624
017300110624       IF %LEN(stm.where) = *ZERO;
017400110624         stm.where = 'WHERE ';
017500110624       ELSE;
017600110624         stm.where = ' AND ';
017700110624       ENDIF;
017800110624
017900110624       stm.where += 'LCASE(RTRIM(CHAR(O.QEZOBJNAM))) '
018000110624                  + priWhereObjectNameSearchCondition;
018100110624
018200110624       RETURN retField;
018300110624
018400110624      /END-FREE
018500110624     P DIRINF_SetWhereObjectName...
018600110624     P                 E
018700110624
018800110624
018900110624     P*--------------------------------------------------
019000110624     P* Procedure name: DIRINF_SetWhereObjectType
019100110624     P* Purpose:        Imposta la WHERE sul tipo oggetto (p.e. *STMF, *DIR)
019200110624     P* Returns:        Esito.
019300110624     P* Parameter:      priWherObjectTypeSearchCondition
019400110624     P*--------------------------------------------------
019500110624     P DIRINF_SetWhereObjectType...
019600110624     P                 B                   EXPORT
019700110624     D DIRINF_SetWhereObjectType...
019800110624     D                 PI            10I 0
019900110624     D  priWhereObjectTypeSearchCondition...
020000110624     D                              255A   VARYING
020100110624     D                                     CONST
020200110624
020300110624     D retField        S             10I 0
020400110624
020500110624      /FREE
020600110624
020700110624       IF NOT done.newRichiamo;
020800110624         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
020900110624       ENDIF;
021000110624
021100110624       IF %LEN(priWhereObjectTypeSearchCondition) = *ZERO
021200110624       OR priWhereObjectTypeSearchCondition = *BLANK;
021300110624         RETURN DIRINF_ESITO_PARAMETRO_OBBLIGATORIO;
021400110624       ENDIF;
021500110624
021600110624       IF %LEN(stm.where) = *ZERO;
021700110624         stm.where = 'WHERE ';
021800110624       ELSE;
021900110624         stm.where = ' AND ';
022000110624       ENDIF;
022100110624
022200110722       stm.where += 'CHAR(O.QEZOBJTYPE) '
022300110722                  + priWhereObjectTypeSearchCondition;
022400110624
022500110624       RETURN retField;
022600110624
022700110624      /END-FREE
022800110624     P DIRINF_SetWhereObjectType...
022900110624     P                 E
023000110624
023100110624
023200110624     P*--------------------------------------------------
023300110624     P* Procedure name: DIRINF_RunRichiamo
023400110624     P* Purpose:        Esegue un nuovo richiamo informazioni indirizzario.
023500110624     P* Returns:        Esito.
023600110624     P*--------------------------------------------------
023700110624     P DIRINF_RunRichiamo...
023800110624     P                 B                   EXPORT
023900110624     D DIRINF_RunRichiamo...
024000110624     D                 PI            10I 0
024100110624
024200110624     D retField        S             10I 0
024300110624
024400110624      /FREE
024500110624
024600110624       IF NOT done.newRichiamo;
024700110624         RETURN DIRINF_ESITO_RICHIAMO_NON_PREPARATO;
024800110624       ENDIF;
024900110624
025000110624       rtvDirInf.rtvDirInf = 'RTVDIRINF DIR(''' + rtvDirInf.dir  + ''')';
025100110624
025200110624       IF rtvDirInf.subTree <> *BLANK;
025300110624         rtvDirInf.rtvDirInf += ' SUBTREE(' + rtvDirInf.subTree + ')';
025400110624       ENDIF;
025500110624
025600110624       IF rtvDirInf.infFilePfx <> *BLANK;
025700110624         rtvDirInf.rtvDirInf += ' INFFILEPFX(' + rtvDirInf.infFilePfx + ')';
025800110624       ENDIF;
025900110624
026000110624       IF rtvDirInf.infLib <> *BLANK;
026100110624         rtvDirInf.rtvDirInf += ' INFLIB(' + rtvDirInf.infLib + ')';
026200110624       ENDIF;
026300110624
026400110624       MONITOR;
026500110624         ExecuteCommand(rtvDirInf.rtvDirInf : %LEN(rtvDirInf.rtvDirInf));
026600110624         ON-ERROR;
026700110624           DUMP(A);
026800110624           RETURN DIRINF_ESITO_ERRORE_RTVDIRINF;
026900110624       ENDMON;
027000110624
027100110624       // Recupero l'ultimo messaggio emesso da RTVDIRINF che può essere:
027200110624       // CPC1E31 Informazioni indirizzario richiamate correttamente.
027300110624       // CPF1ED7 Errore durante il richiamo di informazioni sull'indirizzario.
027400110624
027500110624       CLEAR QUSEC;
027600110624       QUSBPRV = %SIZE(QUSEC);
027700110624
027800110624       ReceiveProgramMessage( rcvm0100
027900110624                            : %SIZE(rcvm0100)
028000110624                            : 'RCVM0100'
028100110624                            : '*'
028200110624                            : *ZERO
028300110624                            : '*LAST'
028400110624                            : *BLANK
028500110624                            : *ZERO
028600110624                            : '*OLD'
028700110624                            : QUSEC
028800110624                            );
028900110624
029000110624       IF QUSEI <> *BLANK;
029100110624         DUMP(A);
029200110624         RETURN DIRINF_ESITO_ERRORE_RTVDIRINF;
029300110624       ENDIF;
029400110624
029500110624       QMHM010001 = rcvm0100.QMHM010001;
029600110624
029700110624       // Qualcosa non ha funzionato.
029800110624
029900110624       IF QMHMI03 <> 'CPC1E31';
030000110624         RETURN DIRINF_ESITO_ERRORE_RTVDIRINF;
030100110624       ENDIF;
030200110624
030300110624       // Recupero il primo messaggio emesso da RTVDIRINF che dovrebbe essere
030400110624       // CPI1E30 File di database QAEZDnnnnO e QAEZDnnnnD creati nella libreria
030500110624       //         QUSRSYS.
030600110624
030700110624       CLEAR QUSEC;
030800110624       QUSBPRV = %SIZE(QUSEC);
030900110624
031000110624       ReceiveProgramMessage( rcvm0100
031100110624                            : %SIZE(rcvm0100)
031200110624                            : 'RCVM0100'
031300110624                            : '*'
031400110624                            : *ZERO
031500110624                            : '*PRV'
031600110624                            : QMHMK03
031700110624                            : *ZERO
031800110624                            : '*OLD'
031900110624                            : QUSEC
032000110624                            );
032100110624
032200110624       IF QUSEI <> *BLANK;
032300110624         DUMP(A);
032400110624         RETURN DIRINF_ESITO_ERRORE_RTVDIRINF;
032500110624       ENDIF;
032600110624
032700110624       QMHM010001 = rcvm0100.QMHM010001;
032800110624
032900110624       // Qualcosa non ha funzionato.
033000110624
033100110624       IF QMHMI03 <> 'CPI1E30';
033200110624         RETURN DIRINF_ESITO_ERRORE_RTVDIRINF;
033300110624       ENDIF;
033400110624
033500110624       // Recupero libreria e nomi delle tabelle create dal comando RTVDIRINF.
033600110624
033700110624       rtvDirInf.infFileO = %TRIMR(%SUBST(rcvm0100.QMHMD00 : 1));
033800110624       rtvDirInf.infFileD = %TRIMR(%SUBST(rcvm0100.QMHMD00 : 11));
033900110624       rtvDirInf.infLib = %TRIMR(%SUBST(rcvm0100.QMHMD00 : 21));
034000110624
034100110624       EXEC SQL DROP ALIAS QTEMP/TRULDIRINFO;
034200110624       EXEC SQL DROP ALIAS QTEMP/TRULDIRINFD;
034300110624
034400110624       stm.stm = 'CREATE ALIAS QTEMP/TRULDIRINFO FOR '
034500110624               + rtvDirInf.infLib + '/' + rtvDirInf.infFileO;
034600110624
034700110624       EXEC SQL EXECUTE IMMEDIATE :stm.stm;
034800110624
034900110624       IF sqlCode < *ZERO;
035000110624         DUMP(A);
035100110624         RETURN sqlCode;
035200110624       ENDIF;
035300110624
035400110624       stm.stm = 'CREATE ALIAS QTEMP/TRULDIRINFD FOR '
035500110624               + rtvDirInf.infLib + '/' + rtvDirInf.infFileD;
035600110624
035700110624       EXEC SQL EXECUTE IMMEDIATE :stm.stm;
035800110624
035900110624       IF sqlCode < *ZERO;
036000110624         DUMP(A);
036100110624         RETURN sqlCode;
036200110624       ENDIF;
036300110624
036400110624       // Compongo istruzione SQL con le clausole.
036500110624
036600110624       stm.select = 'SELECT D.QEZDIRIDX, RTRIM(CHAR(D.QEZDIRNAM1)), +
036700110624                    RTRIM(CHAR(O.QEZOBJNAM))';
036800110624       stm.from = 'FROM QTEMP/TRULDIRINFD AS D';
036900110624       stm.join = 'JOIN QTEMP/TRULDIRINFO AS O ON D.QEZDIRIDX = O.QEZDIRIDX';
037000110624       stm.orderBy = 'ORDER BY D.QEZDIRIDX';
037100110624       stm.stm = stm.select + ' ' + stm.from + ' ' + stm.join + ' '
037200110624               + stm.where + ' ' + stm.orderBy;
037300110624
037400110624
037500110624       EXEC SQL PREPARE selectIdxDirObj FROM :stm.stm;
037600110624
037700110624       EXEC SQL
037800110624         DECLARE idxDirObj NO SCROLL CURSOR WITH HOLD FOR selectIdxDirObj
037900110624       ;
038000110624
038100110624       EXEC SQL OPEN idxDirObj ;
038200110624
038300110624       IF sqlCode < *ZERO;
038400110624         DUMP(A);
038500110624         RETURN sqlCode;
038600110624       ENDIF;
038700110624
038800110624       done.runRichiamo = *ON;
038900110624
039000110624       RETURN retField;
039100110624
039200110624      /END-FREE
039300110624     P DIRINF_RunRichiamo...
039400110624     P                 E
039500110623
039600110623
039700110623     P*--------------------------------------------------
039800110623     P* Procedure name: DIRINF_EndRichiamo
039900110623     P* Purpose:        Fine richiamo.
040000110623     P* Returns:        Esito.
040100110623     P*--------------------------------------------------
040200110623     P DIRINF_EndRichiamo...
040300110623     P                 B                   EXPORT
040400110623     D DIRINF_EndRichiamo...
040500110623     D                 PI            10I 0
040600110623
040700110623     D retField        S             10I 0
040800110623
040900110623      /FREE
041000110623
041100110624       IF NOT done.newRichiamo;
041200110624         RETURN DIRINF_ESITO_RICHIAMO_NON_PREPARATO;
041300110623       ENDIF;
041400110623
041500110623       EXEC SQL CLOSE idxDirObj;
041600110623       EXEC SQL DROP ALIAS QTEMP/TRULDIRINFO;
041700110623       EXEC SQL DROP ALIAS QTEMP/TRULDIRINFD;
041800110623
041900110624       stm.stm = 'DROP TABLE '+ rtvDirInf.infLib + '/' + rtvDirInf.infFileD;
042000110624       EXEC SQL EXECUTE IMMEDIATE :stm.stm;
042100110623
042200110624       stm.stm = 'DROP TABLE '+ rtvDirInf.infLib + '/' + rtvDirInf.infFileO;
042300110624       EXEC SQL EXECUTE IMMEDIATE :stm.stm;
042400110623
042500110623       RESET done;
042600110624       RESET rtvDirInf;
042700110623
042800110623       RETURN retField;
042900110623
043000110623      /END-FREE
043100110623     P DIRINF_EndRichiamo...
043200110623     P                 E
043300110623
043400110623
043500110623     P*--------------------------------------------------
043600110623     P* Procedure name: DIRINF_GetNextObjectName
043700110623     P* Purpose:        Restituisce il prossimo nome oggetto.
043800110623     P* Returns:        Parent directory index, parent directory path e obj...
043900110623     P*                          ect name.
044000110623     P*--------------------------------------------------
044100110623     P DIRINF_GetNextObjectName...
044200110623     P                 B                   EXPORT
044300110623     D DIRINF_GetNextObjectName...
044400110623     D                 PI            10I 0
044500110623     D priIdxDirObj                        LIKEDS(DIRINF_idxDirObj)
044600110623
044700110623     D retField        S             10I 0 STATIC
044800110623
044900110623      /FREE
045000110623
045100110623       CLEAR priIdxDirObj;
045200110623       CLEAR retField;
045300110623
045400110624       IF NOT done.runRichiamo;
045500110623         RETURN DIRINF_ESITO_RICHIAMO_NON_ESEGUITO;
045600110623       ENDIF;
045700110623
045800110623       EXEC SQL FETCH NEXT FROM idxDirObj INTO :priIdxDirObj;
045900110623
046000110623       SELECT;
046100110623         WHEN sqlCode = DIRINF_ESITO_ROW_NOT_FOUND;
046200110623           retField = sqlCode;
046300110623         WHEN sqlCode < *ZERO;
046400110623           DUMP(A);
046500110623           retField = sqlCode;
046600110623       ENDSL;
046700110623
046800110623       RETURN retField;
046900110623
047000110623      /END-FREE
047100110623     P DIRINF_GetNextObjectName...
047200110623     P                 E
047300110624
