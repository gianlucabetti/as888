000100120521     ***************************************************************************
000200120521     **
000300120521     ** Questo modulo gestisce la lettura di una pratica di contenzioso cliente.
000400120521     **
000500120521     ***************************************************************************
000600120521     H NOMAIN BNDDIR('PJXBND':'TIBS':'QC2LE':'YCO') EXTBININT(*YES)
000700120605     H ALWNULL(*USRCTL)
000800120521
000900120521     ***************************************************************************
001000120521     **
001100120521     ** Strutture dati.
001200120521     **
001300120521     ***************************************************************************
001400120521     D andiz00f      E DS                  QUALIFIED TEMPLATE
001500120521     D done            DS                  QUALIFIED INZ
001600120521     D  init                           N
001700120521     D  newPratica                     N
001800120521     D  newClienti                     N
001900120521     D  newCliente                     N
002000120529     D  newStati                       N
002100120529     D  newStato                       N
002200120521     D kpjba         E DS                  QUALIFIED INZ
002300120713     D xChkAutDs     E DS                  QUALIFIED INZ
002400120713     D  xcaFnc       E                     INZ('YCTZ00')
002500120713     D  xcaVfu       E                     INZ(*ZERO)
002600120713     D  xcaTck       E                     INZ('AP')
002700120713     D xSoc001Ds     E DS                  QUALIFIED INZ
002800120713     D yCtzCli0f     E DS                  QUALIFIED INZ
002900120523     D yCtzCli0f_ind   DS                  QUALIFIED INZ
003000120523     D  usrPrfUpd                     5I 0
003100120523     D  usrPrfDlt                     5I 0
003200120523     D  usrPrfRst                     5I 0
003300120523     D  dtUpd                         5I 0
003400120523     D  dtDlt                         5I 0
003500120523     D  dtRst                         5I 0
003600120523     D  accentrSys                    5I 0
003700120523     D  accentrNas                    5I 0
003800120523     D yCtzCli0f_csr   DS                  QUALIFIED DIM(32740)
003900120523     D                                     BASED(yCtzCli0f_ptr)
004000120523     D  idRiga                             LIKE(yCtzCli0f.idRiga)
004100120606     D yCtzPra0f     E DS                  QUALIFIED INZ EXTNAME(yCtzPra1v)
004200120522     D yCtzPra0f_ind   DS                  QUALIFIED INZ
004300120522     D  usrPrfUpd                     5I 0
004400120522     D  usrPrfDlt                     5I 0
004500120522     D  usrPrfRst                     5I 0
004600120522     D  dtUpd                         5I 0
004700120522     D  dtDlt                         5I 0
004800120522     D  dtRst                         5I 0
004900120522     D  partitaIva                    5I 0
005000120522     D  codFiscale                    5I 0
005100120522     D  dtContenzF                    5I 0
005200120522     D yCtzSta0f     E DS                  QUALIFIED INZ
005300120529     D yCtzSta0f_ind   DS                  QUALIFIED INZ
005400120529     D  usrPrfUpd                     5I 0
005500120529     D  usrPrfDlt                     5I 0
005600120529     D  usrPrfRst                     5I 0
005700120529     D  dtUpd                         5I 0
005800120529     D  dtDlt                         5I 0
005900120529     D  dtRst                         5I 0
006000120529     D yCtzSta0f_csr   DS                  QUALIFIED DIM(32740)
006100120529     D                                     BASED(yCtzSta0f_ptr)
006200120529     D  idRiga                             LIKE(yCtzSta0f.idRiga)
006300120521
006400120521     ***************************************************************************
006500120521     **
006600120521     ** Campi.
006700120521     **
006800120521     ***************************************************************************
006900120521     D esito           S             10I 0
007000120713     D esitoProj       S              1A
007100120521     D yCtzCli0f_ele   S             10I 0
007200120521     D yCtzCli0f_idx   S             10I 0
007300120522     D yCtzSta0f_ele   S             10I 0
007400120522     D yCtzSta0f_idx   S             10I 0
007500120521
007600120521     ***************************************************************************
007700120521     **
007800120521     ** Costanti.
007900120521     **
008000120521     ***************************************************************************
008100120521     D/COPY GAITRASRC/SRCCONST,YCOCTZ
008200120521     D COMMIT...
008300120521     D                 C                   *ON
008400120521     D PROJ_ESITO_OK...
008500120521     D                 C                   '0'
008600120521     D PROJ_RICHIESTA_CHAIN...
008700120521     D                 C                   '1'
008800120521     D PROJ_SOTTONATURA_CLIENTE...
008900120521     D                 C                   'C'
009000120521     D ROLLBACK...
009100120521     D                 C                   *OFF
009200120521     D SERIE...
009300120521     D                 C                   'CTZ'
009400120521     D SQLCODE_ROW_IN_USE...
009500120521     D                 C                   -913
009600120521     D SQLCODE_ROW_NOT_FOUND...
009700120521     D                 C                   100
009800120521
009900120521     ***************************************************************************
010000120521     **
010100120521     ** Prototipi.
010200120521     **
010300120521     ***************************************************************************
010400120521     D/COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
010500120713     D/COPY GAITRASRC/SRCPROTOPR,XCHKAUT
010600120713     D/COPY GAITRASRC/SRCPROTOPR,XSOC
010700120521     D/COPY GAITRASRC/SRCPROTOPR,YCOCTZIN
010800120521
010900120521
011000120521     P*--------------------------------------------------
011100120521     P* Procedure name: YCOCTZIN_Init
011200120521     P* Purpose:        Inizializzazione modulo.
011300120521     P* Returns:        Esito.
011400120521     P* Parameter:      priKpjba
011500120521     P*--------------------------------------------------
011600120521     P YCOCTZIN_Init   B                   EXPORT
011700120521     D YCOCTZIN_Init   PI            10I 0
011800120521     D  priKpjba                           LIKEDS(kpjba)
011900120521
012000120521      /FREE
012100120521
012200120713       IF done.init;
012300120713         RETURN YCOCTZ_ESITO_OK;
012400120521       ENDIF;
012500120521
012600120713       kpjba = priKpjba;
012700120713
012800120713       Proj_Societa( 'SOC001'
012900120713                   : xSoc001ds.xscSoc
013000120713                   : *ZERO
013100120713                   : *BLANK
013200120713                   : esitoProj
013300120713                   : xSoc001ds
013400120713                   : kpjba
013500120713                   );
013600120713
013700120713       done.init = *ON;
013800120713
013900120521       RETURN YCOCTZ_ESITO_OK;
014000120521
014100120521      /END-FREE
014200120521     P YCOCTZIN_Init   E
014300120521
014400120521
014500120521     P*--------------------------------------------------
014600120521     P* Procedure name: YCOCTZIN_Finalize
014700120521     P* Purpose:        Chiusura modulo.
014800120521     P* Returns:
014900120521     P*--------------------------------------------------
015000120521     P YCOCTZIN_Finalize...
015100120521     P                 B                   EXPORT
015200120521     D YCOCTZIN_Finalize...
015300120521     D                 PI
015400120521
015500120521      /FREE
015600120521
015700120521       RESET done;
015800120521
015900120521      /END-FREE
016000120521     P YCOCTZIN_Finalize...
016100120521     P                 E
016200120521
016300120521
016400120521     P*--------------------------------------------------
016500120521     P* Procedure name: YCOCTZIN_NewPratica
016600120521     P* Purpose:        Inizia a leggere una pratica.
016700120521     P* Returns:        Esito.
016800120521     P* Parameter:      priIdContenzioso => ID contenzioso.
016900120521     P*--------------------------------------------------
017000120521     P YCOCTZIN_NewPratica...
017100120521     P                 B                   EXPORT
017200120521     D YCOCTZIN_NewPratica...
017300120521     D                 PI            10I 0
017400120521     D  priIdContenzioso...
017500120521     D                               10I 0 CONST
017600120521
017700120521     D retField        S             10I 0 STATIC
017800120521
017900120521      /FREE
018000120521
018100120521       RESET retField;
018200120521       RESET yCtzPra0f;
018300120522       RESET yCtzSta0f;
018400120521
018500120521       IF NOT done.init;
018600120521         RETURN YCOCTZ_ERRORE_MODULO_NON_INIZIATO;
018700120521       ENDIF;
018800120521
018900120521       IF done.newPratica;
019000120521         RETURN YCOCTZ_ERRORE_PRATICA_GIA_INIZIATA;
019100120521       ENDIF;
019200120521
019300120521       IF priIdContenzioso = *ZERO;
019400120521         RETURN YCOCTZ_ERRORE_PARAMETRI;
019500120521       ENDIF;
019600120713
019700120713       // Controllo che l'utente sia autorizzato almeno a visualizzare il contenzioso.
019800120713
019900120713       RESET xChkAutDs;
020000120713       xChkAutDs.xcaSoc = xSoc001Ds.xscSoc;
020100120713       xChkAutDs.xcaGrp = xSoc001Ds.xscGrp;
020200120713       xChkAutDs.xcaPrf = kpjba.knmus;
020300120713       Proj_Autorizzazione( xChkAutDs );
020400120713
020500120713       IF xChkAutDs.xcaErr <> *OFF;
020600120713         RETURN YCOCTZ_ERRORE_AUTORIZZAZIONE;
020700120713       ENDIF;
020800120521
020900120521       EXEC SQL
021000120522         SELECT id_riga
021100120522              , profilo_utente_insert
021200120522              , profilo_utente_update
021300120522              , profilo_utente_delete
021400120522              , profilo_utente_restore
021500120522              , data_insert
021600120522              , data_update
021700120522              , data_delete
021800120522              , data_restore
021900120522              , partita_iva
022000120522              , codice_fiscale
022100120522              , id_cliente_societa
022200120522              , id_cliente_ksc
022300120522              , id_contenzioso
022400120522              , data_inizio_contenzioso
022500120522              , data_fine_contenzioso
022600120606              , id_stato_credito
022700120606              , data_stato_credito
022800120606              , importo_contenzioso
022900120522           INTO :yCtzPra0f.idRiga
023000120522              , :yCtzPra0f.usrPrfIns
023100120522              , :yCtzPra0f.usrPrfUpd :yCtzPra0f_ind.usrPrfUpd
023200120522              , :yCtzPra0f.usrPrfDlt :yCtzPra0f_ind.usrPrfDlt
023300120522              , :yCtzPra0f.usrPrfRst :yCtzPra0f_ind.usrPrfRst
023400120522              , :yCtzPra0f.dtIns
023500120522              , :yCtzPra0f.dtUpd :yCtzPra0f_ind.dtUpd
023600120522              , :yCtzPra0f.dtDlt :yCtzPra0f_ind.dtDlt
023700120522              , :yCtzPra0f.dtRst :yCtzPra0f_ind.dtRst
023800120522              , :yCtzPra0f.partitaIva :yCtzPra0f_ind.partitaIva
023900120522              , :yCtzPra0f.codFiscale :yCtzPra0f_ind.codFiscale
024000120522              , :yCtzPra0f.idCliSoc
024100120522              , :yCtzPra0f.idCliKsc
024200120522              , :yCtzPra0f.idContenz
024300120522              , :yCtzPra0f.dtContenzI
024400120522              , :yCtzPra0f.dtContenzF :yCtzPra0f_ind.dtContenzF
024500120606              , :yCtzPra0f.idStatoCre
024600120606              , :yCtzPra0f.dtStatoCre
024700120606              , :yCtzPra0f.impContenz
024800120606           FROM yCtzPra1v
024900120521           WHERE id_contenzioso = :priIdContenzioso
025000120521           WITH RS
025100120521       ;
025200120521
025300120521       SELECT;
025400120521         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
025500120521           RETURN YCOCTZ_ERRORE_PRATICA_NON_TROVATA;
025600120521         WHEN sqlCode = SQLCODE_ROW_IN_USE;
025700120521           RETURN YCOCTZ_ERRORE_PRATICA_ALLOCATA;
025800120521         WHEN sqlCode < *ZERO;
025900120521           DUMP(A);
026000120521           RETURN sqlCode;
026100120521       ENDSL;
026200120521
026300120530       done.newPratica = *ON;
026400120530
026500120521       RETURN retField;
026600120521
026700120521      /END-FREE
026800120521     P YCOCTZIN_NewPratica...
026900120521     P                 E
027000120521
027100120521
027200120521     P*--------------------------------------------------
027300120521     P* Procedure name: YCOCTZIN_EndPratica
027400120521     P* Purpose:        Finisce di leggere una pratica.
027500120521     P* Returns:        Esito.
027600120521     P* Parameter:      priCommit => *ON = commit; *OFF = rollback
027700120521     P*                              *NOPASS = niente.
027800120521     P*--------------------------------------------------
027900120521     P YCOCTZIN_EndPratica...
028000120521     P                 B                   EXPORT
028100120521     D YCOCTZIN_EndPratica...
028200120521     D                 PI            10I 0
028300120521     D priCommit                       N   OPTIONS(*NOPASS) CONST
028400120521
028500120521      /FREE
028600120521
028700120521       IF %PARMS() > *ZERO;
028800120521         IF priCommit;
028900120521           EXEC SQL COMMIT HOLD;
029000120521         ELSE;
029100120521           EXEC SQL ROLLBACK HOLD;
029200120521         ENDIF;
029300120521       ENDIF;
029400120521
029500120521       RESET done.newPratica;
029600120521       RETURN YCOCTZ_ESITO_OK;
029700120521
029800120521      /END-FREE
029900120521     P YCOCTZIN_EndPratica...
030000120521     P                 E
030100120521
030200120521
030300120521     P*--------------------------------------------------
030400120521     P* Procedure name: YCOCTZIN_NewClienti
030500120521     P* Purpose:        Inizia a leggere i clienti di una pratica di conten...
030600120521     P*                          zioso.
030700120521     P* Returns:        Esito.
030800120521     P*--------------------------------------------------
030900120521     P YCOCTZIN_NewClienti...
031000120521     P                 B                   EXPORT
031100120521     D YCOCTZIN_NewClienti...
031200120521     D                 PI            10I 0
031300120521
031400120521     D retField        S             10I 0 STATIC
031500120521
031600120521      /FREE
031700120521
031800120521       RESET retField;
031900120521
032000120521       IF NOT done.init;
032100120521         RETURN YCOCTZ_ERRORE_MODULO_NON_INIZIATO;
032200120521       ENDIF;
032300120521
032400120521       IF NOT done.newPratica;
032500120521         RETURN YCOCTZ_ERRORE_PRATICA_GIA_INIZIATA;
032600120521       ENDIF;
032700120521
032800120521       IF done.newClienti;
032900120521         RETURN YCOCTZ_ERRORE_CLIENTI_GIA_INIZIATI;
033000120521       ENDIF;
033100120521
033200120521       EXEC SQL
033300120521         DECLARE clienti INSENSITIVE NO SCROLL CURSOR FOR
033400120523           SELECT id_riga
033500120521           FROM yCtzCli0f
033600120521           WHERE id_contenzioso = :yCtzPra0f.idContenz
033700120703             AND data_delete IS NULL
033800120521           FOR READ ONLY
033900120521           WITH RS
034000120521       ;
034100120521
034200120521       EXEC SQL
034300120521         OPEN clienti
034400120521       ;
034500120521
034600120521       SELECT;
034700120521         WHEN sqlCode < *ZERO;
034800120521           DUMP(A);
034900120521           RETURN sqlCode;
035000120521       ENDSL;
035100120521
035200120521       yCtzCli0f_ele = sqlErrD(2);
035300120521
035400120521       IF yCtzCli0f_ele = *ZERO;
035500120521
035600120521         retField = SQLCODE_ROW_NOT_FOUND;
035700120521
035800120521       ELSE;
035900120521
036000120521         yCtzCli0f_ptr = %ALLOC(%SIZE(yCtzCli0f_csr) * yCtzCli0f_ele);
036100120521
036200120521         EXEC SQL
036300120521           FETCH NEXT FROM clienti
036400120521             FOR :yCtzCli0f_ele ROWS
036500120521             INTO :yCtzCli0f_csr
036600120521         ;
036700120521
036800120521         SELECT;
036900120525           WHEN sqlCode = SQLCODE_ROW_IN_USE;
037000120525             retField = YCOCTZ_ERRORE_CLIENTE_ALLOCATO;
037100120521           WHEN sqlCode < *ZERO;
037200120521             DUMP(A);
037300120521             retField = sqlCode;
037400120521         ENDSL;
037500120521
037600120521       ENDIF;
037700120521
037800120521       EXEC SQL
037900120521         CLOSE clienti
038000120521       ;
038100120521
038200120521       RETURN retField;
038300120521
038400120521      /END-FREE
038500120521     P YCOCTZIN_NewClienti...
038600120521     P                 E
038700120521
038800120521
038900120521     P*--------------------------------------------------
039000120521     P* Procedure name: YCOCTZIN_EndClienti
039100120521     P* Purpose:        Finisce di leggere i clienti di una pratica.
039200120521     P* Returns:        Esito.
039300120521     P*--------------------------------------------------
039400120521     P YCOCTZIN_EndClienti...
039500120521     P                 B                   EXPORT
039600120521     D YCOCTZIN_EndClienti...
039700120521     D                 PI            10I 0
039800120521
039900120521      /FREE
040000120521
040100120521       RESET done.newClienti;
040200120521       RESET yCtzCli0f_idx;
040300120521
040400120521       IF yCtzCli0f_ele > *ZERO;
040500120521         DEALLOC yCtzCli0f_ptr;
040600120521         RESET yCtzCli0f_ele;
040700120521       ENDIF;
040800120521
040900120521       RETURN YCOCTZ_ESITO_OK;
041000120521
041100120521      /END-FREE
041200120521     P YCOCTZIN_EndClienti...
041300120521     P                 E
041400120521
041500120521
041600120521     P*--------------------------------------------------
041700120521     P* Procedure name: YCOCTZIN_GetNextIdCliente
041800120521     P* Purpose:        Restituisce il prossimo ID cliente.
041900120521     P* Returns:        >0 = ID cliente; <0 = errore; 0 = fine.
042000120521     P*--------------------------------------------------
042100120521     P YCOCTZIN_GetNextIdCliente...
042200120521     P                 B                   EXPORT
042300120521     D YCOCTZIN_GetNextIdCliente...
042400120521     D                 PI            10I 0
042500120521
042600120521      /FREE
042700120521
042800120521       yCtzCli0f_idx += 1;
042900120521
043000120521       IF yCtzCli0f_idx > yCtzCli0f_ele;
043100120521         RETURN *ZERO;
043200120521       ENDIF;
043300120521
043400120521       RETURN yCtzCli0f_csr(yCtzCli0f_idx).idRiga;
043500120521
043600120521      /END-FREE
043700120521     P YCOCTZIN_GetNextIdCliente...
043800120521     P                 E
043900120521
044000120521
044100120521     P*--------------------------------------------------
044200120521     P* Procedure name: YCOCTZIN_NewCliente
044300120521     P* Purpose:        Inizia la lettura di un cliente.
044400120521     P* Returns:        Esito.
044500120521     P* Parameter:      priIdRigaCliente => ID riga cliente.
044600120521     P*--------------------------------------------------
044700120521     P YCOCTZIN_NewCliente...
044800120521     P                 B                   EXPORT
044900120521     D YCOCTZIN_NewCliente...
045000120521     D                 PI            10I 0
045100120521     D  priIdRigaCliente...
045200120521     D                               10I 0 CONST
045300120521
045400120521      /FREE
045500120521
045600120521       IF done.newCliente;
045700120521         RETURN YCOCTZ_ERRORE_CLIENTE_GIA_INIZIATO;
045800120521       ENDIF;
045900120521
046000120521       EXEC SQL
046100120523         SELECT id_riga	
046200120523              , profilo_utente_insert	
046300120523              , profilo_utente_update	
046400120523              , profilo_utente_delete	
046500120523              , profilo_utente_restore	
046600120523              , data_insert	
046700120523              , data_update	
046800120523              , data_delete	
046900120523              , data_restore	
047000120523              , id_contenzioso	
047100120523              , id_cliente_societa	
047200120523              , id_cliente_ksc	
047300120523              , accentramento_sys	
047400120523              , accentramento_numero	
047500120523              , importo_contenzioso	
047600120523           INTO :yCtzCli0f.idRiga
047700120523              , :yCtzCli0f.usrPrfIns
047800120523              , :yCtzCli0f.usrPrfUpd :yCtzCli0f_ind.usrPrfUpd
047900120523              , :yCtzCli0f.usrPrfDlt :yCtzCli0f_ind.usrPrfDlt
048000120523              , :yCtzCli0f.usrPrfRst :yCtzCli0f_ind.usrPrfRst
048100120523              , :yCtzCli0f.dtIns
048200120523              , :yCtzCli0f.dtUpd :yCtzCli0f_ind.dtUpd
048300120523              , :yCtzCli0f.dtDlt :yCtzCli0f_ind.dtDlt
048400120523              , :yCtzCli0f.dtRst :yCtzCli0f_ind.dtRst
048500120523              , :yCtzCli0f.idContenz
048600120523              , :yCtzCli0f.idCliSoc
048700120523              , :yCtzCli0f.idCliKsc
048800120523              , :yCtzCli0f.accentrSys :yCtzCli0f_ind.accentrSys
048900120523              , :yCtzCli0f.accentrNas :yCtzCli0f_ind.accentrNas
049000120523              , :yCtzCli0f.impContenz
049100120521           FROM yCtzCli0f
049200120521           WHERE id_riga = :priIdRigaCliente
049300120521           WITH RS
049400120521       ;
049500120521
049600120521       SELECT;
049700120521         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
049800120521           RETURN YCOCTZ_ERRORE_CLIENTE_NON_TROVATO;
049900120521         WHEN sqlCode = SQLCODE_ROW_IN_USE;
050000120521           RETURN YCOCTZ_ERRORE_CLIENTE_ALLOCATO;
050100120521         WHEN sqlCode < *ZERO;
050200120521           DUMP(A);
050300120521           RETURN sqlCode;
050400120521       ENDSL;
050500120521
050600120521       done.newCliente = *ON;
050700120521
050800120521       RETURN YCOCTZ_ESITO_OK;
050900120521
051000120521      /END-FREE
051100120521     P YCOCTZIN_NewCliente...
051200120521     P                 E
051300120521
051400120521
051500120521     P*--------------------------------------------------
051600120521     P* Procedure name: YCOCTZIN_EndCliente
051700120521     P* Purpose:        Finisce la lettura di un cliente.
051800120521     P* Returns:        Esito.
051900120521     P*--------------------------------------------------
052000120521     P YCOCTZIN_EndCliente...
052100120521     P                 B                   EXPORT
052200120521     D YCOCTZIN_EndCliente...
052300120521     D                 PI            10I 0
052400120521
052500120521      /FREE
052600120521
052700120521       RESET yCtzCli0f;
052800120521       RESET done.newCliente;
052900120521
053000120521       RETURN YCOCTZ_ESITO_OK;
053100120521
053200120521      /END-FREE
053300120521     P YCOCTZIN_EndCliente...
053400120521     P                 E
053500120521
053600120521
053700120521     P*--------------------------------------------------
053800120521     P* Procedure name: YCOCTZIN_GetCliente
053900120521     P* Purpose:        Reperisce le informazioni del cliente in contenzioso.
054000120521     P* Returns:        Esito.
054100120521     P* Parameter:      priIdSocieta => ID società.
054200120521     P* Parameter:      priIdCliente => ID cliente.
054300120521     P* Parameter:      priAccentramentoSys => Registrazione Proj accentram...
054400120521     P*                          ento: sistema.
054500120523     P* Parameter:      priAccentramentoNas => Registrazione Proj accent...
054600120521     P*                          ramento: n. assoluto.
054700120521     P* Parameter:      priImportoContenzioso => Importo contenzioso in euro.
054800120521     P*--------------------------------------------------
054900120521     P YCOCTZIN_GetCliente...
055000120521     P                 B                   EXPORT
055100120521     D YCOCTZIN_GetCliente...
055200120521     D                 PI            10I 0
055300120521     D  priIdSocieta                  3A
055400120521     D  priIdCliente                  8A
055500120521     D  priAccentramentoSys...
055600120521     D                                5I 0
055700120523     D  priAccentramentoNas...
055800120521     D                               10I 0
055900120521     D  priImportoContenzioso...
056000120521     D                               15P 2
056100120521
056200120521      /FREE
056300120521
056400120521       IF NOT done.newCliente;
056500120521         RETURN YCOCTZ_ERRORE_CLIENTE_NON_INIZIATO;
056600120521       ENDIF;
056700120521
056800120521       priIdSocieta = yCtzCli0f.idCliSoc;
056900120521       priIdCliente = yCtzCli0f.idCliKsc;
057000120523
057100120523       IF yCtzCli0f_ind.accentrSys < *ZERO; // Nullo
057200120523         CLEAR priAccentramentoSys;
057300120523       ELSE;
057400120523         priAccentramentoSys = yCtzCli0f.accentrSys;
057500120523       ENDIF;
057600120523
057700120523       IF yCtzCli0f_ind.accentrNas < *ZERO; // Nullo
057800120523         CLEAR priAccentramentoNas;
057900120523       ELSE;
058000120523         priAccentramentoNas = yCtzCli0f.accentrNas;
058100120523       ENDIF;
058200120523
058300120521       priImportoContenzioso = yCtzCli0f.impContenz;
058400120521
058500120521       RETURN YCOCTZ_ESITO_OK;
058600120521
058700120521      /END-FREE
058800120521     P YCOCTZIN_GetCliente...
058900120521     P                 E
059000120529
059100120529
059200120529     P*--------------------------------------------------
059300120529     P* Procedure name: YCOCTZIN_NewStati
059400120529     P* Purpose:        Inizia la lettura di tutti gli stati di una pratica.
059500120529     P* Returns:        Esito.
059600120529     P*--------------------------------------------------
059700120529     P YCOCTZIN_NewStati...
059800120529     P                 B                   EXPORT
059900120529     D YCOCTZIN_NewStati...
060000120529     D                 PI            10I 0
060100120529
060200120529     D retField        S             10I 0 STATIC
060300120529
060400120529      /FREE
060500120529
060600120529       RESET retField;
060700120529
060800120529       IF NOT done.init;
060900120529         RETURN YCOCTZ_ERRORE_MODULO_NON_INIZIATO;
061000120529       ENDIF;
061100120529
061200120529       IF NOT done.newPratica;
061300120529         RETURN YCOCTZ_ERRORE_PRATICA_GIA_INIZIATA;
061400120529       ENDIF;
061500120529
061600120529       IF done.newStati;
061700120529         RETURN YCOCTZ_ERRORE_STATI_GIA_INIZIATI;
061800120529       ENDIF;
061900120529
062000120529       EXEC SQL
062100120529         DECLARE stati INSENSITIVE NO SCROLL CURSOR FOR
062200120529           SELECT id_riga
062300120529           FROM yCtzSta0f
062400120529           WHERE id_contenzioso = :yCtzPra0f.idContenz
062500120703             AND data_delete IS NULL
062600120703           ORDER BY id_contenzioso, data_stato_credito DESC
062700120529           FOR READ ONLY
062800120529           WITH RS
062900120529       ;
063000120529
063100120529       EXEC SQL
063200120529         OPEN stati
063300120529       ;
063400120529
063500120529       SELECT;
063600120529         WHEN sqlCode < *ZERO;
063700120529           DUMP(A);
063800120529           RETURN sqlCode;
063900120529       ENDSL;
064000120529
064100120529       yCtzSta0f_ele = sqlErrD(2);
064200120529
064300120529       IF yCtzSta0f_ele = *ZERO;
064400120529
064500120529         retField = SQLCODE_ROW_NOT_FOUND;
064600120529
064700120529       ELSE;
064800120529
064900120529         yCtzSta0f_ptr = %ALLOC(%SIZE(yCtzSta0f_csr) * yCtzSta0f_ele);
065000120529
065100120529         EXEC SQL
065200120529           FETCH NEXT FROM stati
065300120529             FOR :yCtzSta0f_ele ROWS
065400120529             INTO :yCtzSta0f_csr
065500120529         ;
065600120529
065700120529         SELECT;
065800120529           WHEN sqlCode = SQLCODE_ROW_IN_USE;
065900120529             retField = YCOCTZ_ERRORE_CLIENTE_ALLOCATO;
066000120529           WHEN sqlCode < *ZERO;
066100120529             DUMP(A);
066200120529             retField = sqlCode;
066300120703           OTHER;
066400120703             retField = yCtzSta0f_ele;
066500120529         ENDSL;
066600120529
066700120529       ENDIF;
066800120529
066900120529       EXEC SQL
067000120529         CLOSE stati
067100120529       ;
067200120530
067300120530       done.newStati = *ON;
067400120529
067500120530       RETURN retField;
067600120529
067700120529      /END-FREE
067800120529     P YCOCTZIN_NewStati...
067900120529     P                 E
068000120529
068100120529
068200120529     P*--------------------------------------------------
068300120529     P* Procedure name: YCOCTZIN_GetNextIdStato
068400120529     P* Purpose:        Restituisce il prossimo ID stato.
068500120529     P* Returns:        Esito.
068600120529     P*--------------------------------------------------
068700120529     P YCOCTZIN_GetNextIdStato...
068800120529     P                 B                   EXPORT
068900120529     D YCOCTZIN_GetNextIdStato...
069000120529     D                 PI            10I 0
069100120529
069200120529      /FREE
069300120529
069400120529       yCtzSta0f_idx += 1;
069500120529
069600120529       IF yCtzSta0f_idx > yCtzSta0f_ele;
069700120529         RETURN *ZERO;
069800120529       ENDIF;
069900120529
070000120529       RETURN yCtzSta0f_csr(yCtzSta0f_idx).idRiga;
070100120529
070200120529      /END-FREE
070300120529     P YCOCTZIN_GetNextIdStato...
070400120529     P                 E
070500120529
070600120529
070700120529     P*--------------------------------------------------
070800120529     P* Procedure name: YCOCTZIN_EndStati
070900120529     P* Purpose:        Finisce di leggere gli stati di una pratica.
071000120529     P* Returns:        Esito.
071100120529     P*--------------------------------------------------
071200120529     P YCOCTZIN_EndStati...
071300120529     P                 B                   EXPORT
071400120529     D YCOCTZIN_EndStati...
071500120529     D                 PI            10I 0
071600120529
071700120529      /FREE
071800120529
071900120529       RESET done.newStati;
072000120529       RESET yCtzSta0f_idx;
072100120529
072200120529       IF yCtzSta0f_ele > *ZERO;
072300120529         DEALLOC yCtzSta0f_ptr;
072400120529         RESET yCtzSta0f_ele;
072500120529       ENDIF;
072600120529
072700120529       RETURN YCOCTZ_ESITO_OK;
072800120529
072900120529      /END-FREE
073000120529     P YCOCTZIN_EndStati...
073100120529     P                 E
073200120529
073300120529
073400120529     P*--------------------------------------------------
073500120529     P* Procedure name: YCOCTZIN_NewStato
073600120529     P* Purpose:        Inizia la lettura di uno stato.
073700120529     P* Returns:        Esito.
073800120529     P* Parameter:      priIdRigaStato => ID riga stato.
073900120529     P*--------------------------------------------------
074000120529     P YCOCTZIN_NewStato...
074100120529     P                 B                   EXPORT
074200120529     D YCOCTZIN_NewStato...
074300120529     D                 PI            10I 0
074400120529     D priIdRigaStato                10I 0 CONST
074500120529
074600120529      /FREE
074700120529
074800120529       IF done.newStato;
074900120529         RETURN YCOCTZ_ERRORE_STATO_GIA_INIZIATO;
075000120529       ENDIF;
075100120529
075200120529       EXEC SQL
075300120529         SELECT id_riga
075400120529              , profilo_utente_insert	
075500120529              , profilo_utente_update	
075600120529              , profilo_utente_delete	
075700120529              , profilo_utente_restore	
075800120529              , data_insert	
075900120529              , data_update	
076000120529              , data_delete	
076100120529              , data_restore	
076200120529              , id_contenzioso
076300120529              , id_stato_credito
076400120529              , data_stato_credito
076500120529           INTO :yCtzSta0f.idRiga
076600120529              , :yCtzSta0f.usrPrfIns
076700120529              , :yCtzSta0f.usrPrfUpd :yCtzSta0f_ind.usrPrfUpd
076800120529              , :yCtzSta0f.usrPrfDlt :yCtzSta0f_ind.usrPrfDlt
076900120529              , :yCtzSta0f.usrPrfRst :yCtzSta0f_ind.usrPrfRst
077000120529              , :yCtzSta0f.dtIns
077100120529              , :yCtzSta0f.dtUpd :yCtzSta0f_ind.dtUpd
077200120529              , :yCtzSta0f.dtDlt :yCtzSta0f_ind.dtDlt
077300120529              , :yCtzSta0f.dtRst :yCtzSta0f_ind.dtRst
077400120529              , :yCtzSta0f.idContenz
077500120529              , :yCtzSta0f.idStatoCre
077600120529              , :yCtzSta0f.dtStatoCre
077700120529           FROM yCtzSta0f
077800120529           WHERE id_riga = :priIdRigaStato
077900120529           WITH RS
078000120529       ;
078100120529
078200120529       SELECT;
078300120529         WHEN sqlCode = SQLCODE_ROW_NOT_FOUND;
078400120529           RETURN YCOCTZ_ERRORE_STATO_NON_TROVATO;
078500120529         WHEN sqlCode = SQLCODE_ROW_IN_USE;
078600120529           RETURN YCOCTZ_ERRORE_STATO_ALLOCATO;
078700120529         WHEN sqlCode < *ZERO;
078800120529           DUMP(A);
078900120529           RETURN sqlCode;
079000120529       ENDSL;
079100120529
079200120529       done.newStato = *ON;
079300120529
079400120529       RETURN YCOCTZ_ESITO_OK;
079500120529
079600120529      /END-FREE
079700120529     P YCOCTZIN_NewStato...
079800120529     P                 E
079900120529
080000120529
080100120529     P*--------------------------------------------------
080200120529     P* Procedure name: YCOCTZIN_GetStato
080300120529     P* Purpose:        Reperisce le informazioni dello stato del credito.
080400120529     P* Returns:        Esito.
080500120529     P* Parameter:      priIdStatoCredito => ID stato del credito.
080600120529     P* Parameter:      priDataStatoCredito => Data dello stato del credito.
080700120529     P*--------------------------------------------------
080800120529     P YCOCTZIN_GetStato...
080900120529     P                 B                   EXPORT
081000120529     D YCOCTZIN_GetStato...
081100120529     D                 PI            10I 0
081200120529     D  priIdStatoCredito...
081300120529     D                                4A
081400120529     D  priDataStatoCredito...
081500120529     D                                 D   DATFMT(*ISO)
081600120705     D  priTimestampStatoCredito...
081700120705     D                                 Z   OPTIONS(*NOPASS)
081800120529
081900120529      /FREE
082000120529
082100120529       IF NOT done.newStato;
082200120529         RETURN YCOCTZ_ERRORE_STATO_NON_INIZIATO;
082300120529       ENDIF;
082400120529
082500120529       priIdStatoCredito = yCtzSta0f.idStatoCre;
082600120705       priDataStatoCredito = %DATE(yCtzSta0f.dtStatoCre);
082700120529
082800120705       IF %PARMS() > 2;
082900120705         priTimestampStatoCredito = yCtzSta0f.dtStatoCre;
083000120705       ENDIF;
083100120705
083200120529       RETURN YCOCTZ_ESITO_OK;
083300120529
083400120529      /END-FREE
083500120529     P YCOCTZIN_GetStato...
083600120529     P                 E
083700120529
083800120529
083900120529     P*--------------------------------------------------
084000120529     P* Procedure name: YCOCTZIN_EndStato
084100120529     P* Purpose:        Finisce la lettura di uno stato del credito.
084200120529     P* Returns:        Esito.
084300120529     P*--------------------------------------------------
084400120529     P YCOCTZIN_EndStato...
084500120529     P                 B                   EXPORT
084600120529     D YCOCTZIN_EndStato...
084700120529     D                 PI            10I 0
084800120529
084900120529      /FREE
085000120529
085100120529       RESET yCtzSta0f;
085200120529       RESET done.newStato;
085300120529
085400120529       RETURN YCOCTZ_ESITO_OK;
085500120529
085600120529      /END-FREE
085700120529     P YCOCTZIN_EndStato...
085800120529     P                 E
085900120605
086000120605
086100120605     P*--------------------------------------------------
086200120605     P* Procedure name: YCOCTZIN_GetPratica
086300120605     P* Purpose:        Restituisce i dati di una pratica di contenzioso.
086400120605     P* Returns:        Esito.
086500120605     P* Parameter:      priPartitaIva => Partita IVA.
086600120605     P* Parameter:      priCodiceFiscale => Codice fiscale.
086700120605     P* Parameter:      priIdClienteSocieta => ID società cliente.
086800120605     P* Parameter:      priIdCliente => ID cliente accentratore.
086900120605     P* Parameter:      priDataInizioContenzioso => Data inizio contenzioso.
087000120605     P* Parameter:      priDataContenzioso => Data fine contenzioso.
087100120605     P* Parameter:      priIdStatoCredito => ID stato del credito.
087200120605     P* Parameter:      priImportoContenzioso => Importo contenzioso in euro.
087300120605     P*--------------------------------------------------
087400120605     P YCOCTZIN_GetPratica...
087500120605     P                 B                   EXPORT
087600120605     D YCOCTZIN_GetPratica...
087700120605     D                 PI            10I 0
087800120605     D  priPartitaIva                20A   VARYING OPTIONS(*NULLIND)
087900120605     D  priCodiceFiscale...
088000120605     D                               16A   VARYING OPTIONS(*NULLIND)
088100120605     D  priIdClienteSocieta...
088200120605     D                                3A
088300120605     D  priIdCliente                  8A
088400120605     D  priDataInizioContenzioso...
088500120605     D                                 D   DATFMT(*ISO)
088600120605     D  priDataFineContenzioso...
088700120605     D                                 D   DATFMT(*ISO)
088800120605     D                                     OPTIONS(*NULLIND)
088900120605     D  priIdStatoCredito...
089000120605     D                                4A
089100120606     D  priDataStatoCredito...
089200120706     D                                 Z
089300120606     D  priImportoContenzioso...
089400120605     D                               15P 2
089500120605
089600120605     D retField        S             10I 0 STATIC
089700120605
089800120605      /FREE
089900120605
090000120606       RESET retField;
090100120606
090200120605       IF NOT done.newPratica;
090300120605         RETURN YCOCTZ_ERRORE_PRATICA_NON_INIZIATA;
090400120605       ENDIF;
090500120605
090600120605       IF yCtzPra0f_ind.partitaIva < *ZERO;
090700120605         %NULLIND(priPartitaIva) = *ON;
090800120605       ELSE;
090900120605         %NULLIND(priPartitaIva) = *OFF;
091000120605         priPartitaIva = yCtzPra0f.partitaIva;
091100120605       ENDIF;
091200120605
091300120605       IF yCtzPra0f_ind.codFiscale < *ZERO;
091400120605         %NULLIND(priCodiceFiscale) = *ON;
091500120605       ELSE;
091600120605         %NULLIND(priCodiceFiscale) = *OFF;
091700120605         priCodiceFiscale = yCtzPra0f.codFiscale;
091800120605       ENDIF;
091900120605
092000120605       priIdClienteSocieta = yCtzPra0f.idCliSoc;
092100120605       priIdCliente = yCtzPra0f.idCliKsc;
092200120605       priDataInizioContenzioso = yCtzPra0f.dtContenzI;
092300120605
092400120605       IF yCtzPra0f_ind.dtContenzF < *ZERO;
092500120605         %NULLIND(priDataFineContenzioso) = *ON;
092600120605       ELSE;
092700120605         %NULLIND(priDataFineContenzioso) = *OFF;
092800120605         priDataFineContenzioso = yCtzPra0f.dtContenzF;
092900120605       ENDIF;
093000120605
093100120606       priIdStatoCredito = yCtzPra0f.idStatoCre;
093200120706       priDataStatoCredito = yCtzPra0f.dtStatoCre;
093300120606       priImportoContenzioso = yCtzPra0f.impContenz;
093400120606
093500120605       RETURN retField;
093600120605
093700120605      /END-FREE
093800120605     P YCOCTZIN_GetPratica...
093900120605     P                 E
094000120606
094100120606
094200120606     P*--------------------------------------------------
094300120606     P* Procedure name: YCOCTZIN_GetStatoContenziosoByData
094400120606     P* Purpose:        Restituisce lo stato del contenzioso in base alla data.
094500120606     P* Returns:        Negativo = errore; Positivo = ID riga stato.
094600120606     P* Parameter:      priDataRiferimento => Data di riferimento (*NOPASS ...
094700120606     P*                          = data corrente).
094800120606     P*--------------------------------------------------
094900120606     P YCOCTZIN_GetStatoContenziosoByData...
095000120606     P                 B                   EXPORT
095100120606     D YCOCTZIN_GetStatoContenziosoByData...
095200120606     D                 PI            10I 0
095300120606     D  priDataStatoCredito...
095400120606     D                                 D   DATFMT(*ISO)
095500120606     D                                     VALUE
095600120606     D                                     OPTIONS(*NOPASS)
095700120606
095800120606     D retField        S             10I 0 STATIC
095900120606     D local           DS                  STATIC QUALIFIED
096000120606     D  dataStatoCredito...
096100120705     D                                 Z   INZ(*SYS)
096200120606
096300120606      /FREE
096400120606
096500120606       RESET retField;
096600120606       RESET local;
096700120606
096800120606       IF NOT done.newPratica;
096900120606         RETURN YCOCTZ_ERRORE_PRATICA_GIA_INIZIATA;
097000120606       ENDIF;
097100120606
097200120606       IF %PARMS() > *ZERO AND priDataStatoCredito > *LOVAL;
097300120705         local.dataStatoCredito = %TIMESTAMP(priDataStatoCredito);
097400120606       ENDIF;
097500120606
097600120606       EXEC SQL
097700120606         SELECT id_riga
097800120606           INTO :retField
097900120606           FROM yCtzSta0f
098000120606           WHERE id_contenzioso = :yCtzPra0f.idContenz
098100120606             AND data_delete IS NULL
098200120606             AND data_stato_credito <= :local.dataStatoCredito
098300120606           ORDER BY id_contenzioso, data_stato_credito DESC
098400120606           FETCH FIRST 1 ROW ONLY
098500120606       ;
098600120606
098700120606       SELECT;
098800120606         WHEN sqlCode < *ZERO;
098900120606           DUMP(A);
099000120606           RETURN sqlCode;
099100120606       ENDSL;
099200120606
099300120606       RETURN retField;
099400120606
099500120606      /END-FREE
099600120606     P YCOCTZIN_GetStatoContenziosoByData...
099700120606     P                 E
099800120606
