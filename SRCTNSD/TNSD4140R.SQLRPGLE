000100090409     ***************************************************************************
000200090409     **
000300121218     ** Anagrafico società operative per filiale.
000400090409     ** Manutenzione.
000500090409     **
000600090409     ***************************************************************************
000700120322     H DFTACTGRP(*NO) ACTGRP('AZSOF') BNDDIR('TIBS':'PJXBND')
000800090409
000900090409     ***************************************************************************
001000090409     **
001100120320     ** Files.
001200090409     **
001300090409     ***************************************************************************
001400120402     Ftnsd4140d CF   E             WORKSTN PREFIX(dsp_) INDDS(dspInd)
001500120402     F                                     INFDS(dspInf)
001600090409
001700090409     ***************************************************************************
001800090409     **
001900120320     ** Costanti.
002000090409     **
002100090409     ***************************************************************************
002200120322     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
002300120405     D/COPY GAITRASRC/SRCCONST,TIBSSOFR
002400120322     D/COPY GAITRASRC/SRCCONST,TNSD4100R
002500120322     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002600090415     D DSPATR_NONPROTECT_UNDERSCORE...
002700090415     D                 C                   X'24'
002800090415     D DSPATR_PROTECT_NORMAL...
002900090415     D                 C                   X'A0'
003000120320     D ESITO_WARNING_ENTER...
003100120320     D                 C                   241
003200120320     D ESITO_WARNING_F2...
003300120320     D                 C                   50
003400120320     D ESITO_WARNING_F3...
003500120320     D                 C                   51
003600120320     D ESITO_WARNING_F4...
003700120320     D                 C                   52
003800120320     D ESITO_WARNING_F5...
003900120320     D                 C                   53
004000120320     D ESITO_WARNING_F6...
004100120320     D                 C                   54
004200120320     D ESITO_WARNING_F8...
004300120320     D                 C                   56
004400120320     D ESITO_WARNING_F12...
004500120320     D                 C                   60
004600120320     D ESITO_WARNING_PERIODI_NON_CONTIGUI...
004700090415     D                 C                   2
004800090414     D NOT_NULL...
004900090414     D                 C                   0
005000090416     D ORDINAMENTO_DESCRIZIONE...
005100090416     D                 C                   '1'
005200120322     D OPCODE_RITORNO...
005300120322     D                 C                   -2
005400120322     D OPCODE_FINE...
005500120322     D                 C                   -1
005600120322     D OPCODE_DONE...
005700120322     D                 C                   0
005800120320     D OPCODE_ANAGRAFICA_CHK...
005900120320     D                 C                   1
006000120320     D OPCODE_ANAGRAFICA_GET...
006100120320     D                 C                   2
006200120320     D OPCODE_ANAGRAFICA_INZ...
006300120320     D                 C                   3
006400120320     D OPCODE_ANAGRAFICA_PUT...
006500120320     D                 C                   4
006600120320     D OPCODE_ANAGRAFICA_RUN...
006700120320     D                 C                   5
006800120320     D OPCODE_ANAGRAFICA_SET...
006900120320     D                 C                   6
007000120320     D OPCODE_RICHIESTA...
007100120320     D                 C                   7
007200120320     D OPCODE_SUCCESSIVO...
007300120320     D                 C                   8
007400090409
007500090409     ***************************************************************************
007600090409     **
007700120320     ** Prototipi.
007800090409     **
007900090409     ***************************************************************************
008000120322      /DEFINE DFTACTGRP_NO
008100120322     D/COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
008200090415     D/COPY GAITRASRC/SRCPROTOPR,QUILNGTX
008300120322     D/COPY GAITRASRC/SRCPROTOPR,TIBSORG00
008400120322     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008500120405     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
008600120322     D/COPY GAITRASRC/SRCPROTOPR,TIBS02R
008700120322     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
008800120322     D/COPY GAITRASRC/SRCPROTOPR,TNSD4100R
008900090416     D/COPY GAITRASRC/SRCPROTOPR,TRUL19R
009000090428     D/COPY GAITRASRC/SRCPROTOPR,TRULCALER
009100090416     D/COPY GAITRASRC/SRCPROTOPR,XCHKAUT
009200120322      /UNDEFINE DFTACTGRP_NO
009300090409
009400090409     ***************************************************************************
009500090409     **
009600120320     ** Strutture dati.
009700090409     **
009800090409     ***************************************************************************
009900090415     D/COPY QSYSINC/QRPGLESRC,QUSEC
010000120405     D azorg00f      E DS                  QUALIFIED
010100120320     D dspInd          DS                  QUALIFIED INZ
010200120320     D  errMsgIdFiliale...
010300120320     D                         1      1N
010400090416     D  errMsgDataValiditaSequenza...
010500090415     D                         2      2N
010600090416     D  errMsgDataValiditaInizioSovrapposta...
010700090415     D                         3      3N
010800090416     D  errMsgDataValiditaFineSovrapposta...
010900090415     D                         4      4N
011000090416     D  errMsgIdSocieta...
011100090415     D                         5      5N
011200120320     D  errMsgTpSocieta...
011300120320     D                         6      6N
011400090415     D  erroreGenerico...
011500090409     D                        99     99N
011600120320     D dspInf          DS                  QUALIFIED
011700090409     D  dsp_aid              369    369A
011800090409     D  dsp_aid_num          369    369U 0
011900090409     D  cursorRow            370    370I 0
012000090409     D  cursorCol            371    371I 0
012100090409     D  actCsrRow            382    382I 0
012200090409     D  actCsrCol            383    383I 0
012300120320     D kpjba         E DS                  QUALIFIED INZ
012400120320     D  knmus        E                     INZ(*USER)
012500120402     D tibsSocI0     E DS                  QUALIFIED INZ(*EXTDFT)
012600120402     D tibsSocO0     E DS                  QUALIFIED INZ(*EXTDFT)
012700120402     D tibs02ds      E DS                  QUALIFIED INZ
012800120322     D tnsd4020i     E DS                  QUALIFIED INZ(*EXTDFT)
012900120322     D tnsd4020o     E DS                  QUALIFIED INZ(*EXTDFT)
013000120322     D tnsd4140i     E DS                  QUALIFIED INZ(*EXTDFT)
013100120320     D tnsd4140o     E DS                  QUALIFIED INZ(*EXTDFT)
013200120320     D xChkAutDs     E DS                  QUALIFIED INZ
013300120320     D  xcaFnc       E                     INZ('YAZSOF')
013400120320     D  xcaVfu       E                     INZ(*ZERO)
013500120320     D  xcaTck       E                     INZ('CK')
013600120320     D  xcaNau       E                     INZ(*ON)
013700090409
013800090409     ***************************************************************************
013900090409     **
014000120320     ** Campi.
014100090409     **
014200090409     ***************************************************************************
014300090415     D count...
014400090415     D                 S             10I 0
014500090430     D dataEUR0...
014600090430     D                 S              8A
014700090409     D esito...
014800090409     D                 S             10I 0
014900090416     D esitoPartitaIva...
015000090416     D                 S              1P 0
015100090410     D esitoProj...
015200090410     D                 S              1A
015300090416     D idElemento...
015400090416     D                 S              8A
015500090409     D opCodeCur...
015600120320     D                 S             10I 0
015700090409     D opCodeNxt...
015800090409     D                 S                   LIKE(opCodeCur)
015900120320     D                                     INZ(OPCODE_ANAGRAFICA_INZ)
016000090409     D opCodePrv...
016100090409     D                 S                   LIKE(opCodeCur)
016200090409     D pannello...
016300090409     D                 S              3U 0 INZ(1)
016400090416     D partitaIvaProj...
016500090416     D                 S             20A
016600090416     D tastoFunzionaleUscita...
016700090416     D                 S              1A
016800090415     D textString...
016900090415     D                 S            256A
017000120405     D tibssof_esito...
017100120405     D                 S             10I 0 IMPORT
017200090409
017300090409     ***************************************************************************
017400090409     **
017500120320     ** Parametri procedura principale.
017600090409     **
017700090409     ***************************************************************************
017800120320     D Tnsd41_Manutenzione...
017900090409     D                 PI
018000120320     D  priRqsOpCode...
018100120320     D                               10I 0
018200090409     D                                     CONST
018300120320     D  priRpyOpCode...
018400120320     D                               10I 0
018500120320     D  priRpyIdMsg...
018600090409     D                               10I 0
018700120320     D  priRqsFormato...
018800090409     D                               10A
018900090409     D                                     CONST
019000090409     D                                     OPTIONS(*NOPASS:*OMIT)
019100120320     D  priRqsData...
019200090409     D                            65535A   OPTIONS(*VARSIZE:*NOPASS:*OMIT)
019300090409     D                                     CONST
019400120320     D  priRqsDataSize...
019500090409     D                               10I 0
019600090409     D                                     CONST
019700090409     D                                     OPTIONS(*NOPASS:*OMIT)
019800120320     D  priRpyFormato...
019900090409     D                               10A
020000090409     D                                     CONST
020100090409     D                                     OPTIONS(*NOPASS)
020200120320     D  priRpyData...
020300090409     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
020400120320     D  priRpyDataSize...
020500090409     D                               10I 0
020600090409     D                                     CONST
020700090409     D                                     OPTIONS(*NOPASS)
020800090409
020900090409     D*--------------------------------------------------
021000090409     D* Procedure name: InzAnagrafica
021100090409     D* Purpose:        Inizializza finestra elenco.
021200090409     D* Returns:        Esito.
021300090409     D*--------------------------------------------------
021400090409     D InzAnagrafica   PR            10I 0
021500090409
021600090409     D*--------------------------------------------------
021700090409     D* Procedure name: SetAnagrafica
021800090409     D* Purpose:        Imposta dati finestra elenco.
021900090409     D* Returns:        Esito.
022000090409     D*--------------------------------------------------
022100090409     D SetAnagrafica   PR            10I 0
022200090409
022300090409     D*--------------------------------------------------
022400090409     D* Procedure name: PutAnagrafica
022500090409     D* Purpose:        Emette finestra elenco.
022600090409     D* Returns:        Esito.
022700090409     D*--------------------------------------------------
022800090409     D PutAnagrafica   PR            10I 0
022900090409
023000090409     D*--------------------------------------------------
023100090409     D* Procedure name: GetAnagrafica
023200090409     D* Purpose:        Legge la finestra elenco.
023300090409     D* Returns:        Esito.
023400090409     D*--------------------------------------------------
023500090409     D GetAnagrafica   PR            10I 0
023600090409
023700090409     D*--------------------------------------------------
023800090409     D* Procedure name: ChkAnagrafica
023900090409     D* Purpose:        Controlla i dati inseriti nella finestra elenco.
024000090409     D* Returns:        Esito.
024100090409     D*--------------------------------------------------
024200090409     D ChkAnagrafica   PR            10I 0
024300090409
024400090409     D*--------------------------------------------------
024500090409     D* Procedure name: RunAnagrafica
024600090409     D* Purpose:        Elabora i dati inseriti nella finestra elenco.
024700090409     D* Returns:        Esito.
024800090409     D*--------------------------------------------------
024900090409     D RunAnagrafica   PR            10I 0
025000090414
025100090414     D*--------------------------------------------------
025200090414     D* Procedure name: RunRichiesta
025300090414     D* Purpose:        Gestione F4=Richiesta.
025400090414     D* Returns:        Esito.
025500090414     D*--------------------------------------------------
025600090414     D RunRichiesta    PR            10I 0
025700090414
025800090414
025900090409      /FREE
026000090416
026100120320       SELECT;
026200120320
026300120320         WHEN priRqsOpCode = TNSD41_RQSOPCODE_INIT;
026400120320
026500120320           IF priRqsFormato = 'KPJBA';
026600120320             kpjba = priRqsData;
026700120320           ENDIF;
026800120320
026900120402           Societa_Init();
027000120402
027100120320           RETURN;
027200120320
027300120320         WHEN priRqsOpCode = TNSD41_RQSOPCODE_FINALIZE;
027400120320
027500120320           *INLR = *ON;
027600120320           RETURN;
027700120320
027800120320       ENDSL;
027900090416
028000120320       // Controllo autorizzazione.
028100090416
028200120320       RESET xChkAutDs;
028300120320       xChkAutDs.xcaPrf = kpjba.knmus;
028400120320       Proj_Autorizzazione( xChkAutDs );
028500090416
028600120320       IF xChkAutDs.xcaRtn <> *ZERO;
028700120320         priRpyOpCode = TNSD41_RPYOPCODE_ERRORE;
028800120320         RETURN;
028900120320       ENDIF;
029000090416
029100120320       // Ricezione parametri.
029200090416
029300120320       IF priRqsFormato = tnsd4140i.formato;
029400120320         tnsd4140i = %SUBST(priRqsData : 1 : priRqsDataSize);
029500120320       ENDIF;
029600090409
029700120320       IF tnsd4140i.rqsOpCode <> TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA;
029800120320         EXEC SQL
029900120320           SET TRANSACTION ISOLATION LEVEL REPEATABLE READ
030000120320         ;
030100120320       ENDIF;
030200090409
030300120320       RESET opCodeNxt;
030400120320       RESET tnsd4140o;
030500090414
030600120322       DOU opCodeCur = OPCODE_FINE;
030700090409
030800120320         opCodeCur = opCodeNxt;
030900090409
031000120320         // Gestione della anagrafica.
031100090414
031200120320         IF opCodeCur = OPCODE_ANAGRAFICA_INZ;
031300090414
031400120320           InzAnagrafica();
031500120320           opCodeNxt = OPCODE_ANAGRAFICA_CHK;
031600090414
031700120320         ELSEIF opCodeCur = OPCODE_ANAGRAFICA_SET;
031800090414
031900120320           SetAnagrafica();
032000120320           opCodeNxt = OPCODE_ANAGRAFICA_PUT;
032100090414
032200120320         ELSEIF opCodeCur = OPCODE_ANAGRAFICA_PUT;
032300090414
032400120320           PutAnagrafica();
032500120320           opCodeNxt = OPCODE_ANAGRAFICA_GET;
032600090414
032700120320         ELSEIF opCodeCur = OPCODE_ANAGRAFICA_GET;
032800090414
032900120320           esito = GetAnagrafica();
033000090414
033100120320           IF esito = ESITO_WARNING_ENTER;
033200120320             opCodeNxt = OPCODE_ANAGRAFICA_CHK;
033300120322           ELSEIF esito = ESITO_WARNING_F3;
033400120322             opCodeNxt = OPCODE_FINE;
033500120322             priRpyOpCode = TNSD41_RPYOPCODE_FINE;
033600120320           ELSEIF esito = ESITO_WARNING_F4;
033700120320             opCodeNxt = OPCODE_RICHIESTA;
033800120320             opCodePrv = OPCODE_ANAGRAFICA_SET;
033900120320           ELSEIF esito = ESITO_WARNING_F5;
034000120320             opCodeNxt = OPCODE_ANAGRAFICA_INZ;
034100120320           ELSEIF esito = ESITO_WARNING_F6;
034200120320             opCodeNxt = OPCODE_ANAGRAFICA_RUN;
034300120320           ELSEIF esito = ESITO_WARNING_F8;
034400120320             opCodeNxt = OPCODE_SUCCESSIVO;
034500120320           ELSEIF esito = ESITO_WARNING_F12;
034600120322             opCodeNxt = OPCODE_RITORNO;
034700120322             priRpyOpCode = TNSD41_RPYOPCODE_RITORNO;
034800120320           ELSE;
034900120322             opCodeNxt = OPCODE_FINE;
035000120322             priRpyOpCode = TNSD41_RPYOPCODE_FINE;
035100120320           ENDIF;
035200090414
035300120320         ELSEIF opCodeCur = OPCODE_ANAGRAFICA_CHK;
035400090414
035500120320           ChkAnagrafica();
035600120320           opCodeNxt = OPCODE_ANAGRAFICA_SET;
035700090414
035800120320         ELSEIF opCodeCur = OPCODE_ANAGRAFICA_RUN;
035900090414
036000120322           IF ChkAnagrafica() = OPCODE_DONE;
036100120320             esito = RunAnagrafica();
036200120322             IF esito < OPCODE_DONE;
036300120320               EXEC SQL
036400120320                 ROLLBACK
036500120320               ;
036600120320               priRpyOpCode = TNSD41_RPYOPCODE_ERRORE;
036700120320               priRpyIdMsg = esito;
036800120320             ELSE;
036900120320               EXEC SQL
037000120320                 COMMIT
037100120320               ;
037200120320               priRpyOpCode = TNSD41_RPYOPCODE_DONE;
037300120320               IF esito = ESITO_WARNING_PERIODI_NON_CONTIGUI;
037400120320                 RESET qusec;
037500120320                 textString = 'ATTENZIONE: le anagrafiche della +
037600120320                               filiale ' + %CHAR(dsp_idFiliale) + ' id ' +
037700120320                               dsp_idSocieta + ' non sono contigue dal +
037800120320                               01.01.0001 al 31.12.9999. +
037900120320                               Aggiornamento eseguito.';
038000120320                 DisplayLongText( textString
038100120320                                : %SIZE(textString)
038200120320                                : *BLANK
038300120320                                : *BLANK
038400120320                                : qusec
038500120320                                );
038600120320               ENDIF;
038700120320             ENDIF;
038800120322             opCodeNxt = OPCODE_FINE;
038900120320             priRpyOpCode = TNSD41_RPYOPCODE_DONE;
039000120320           ELSE;
039100120320             opCodeNxt = OPCODE_ANAGRAFICA_SET;
039200120320           ENDIF;
039300090414
039400120320         // Gestione uscita dal programma.
039500090414
039600120320         ELSEIF opCodeCur = OPCODE_RICHIESTA;
039700090414
039800120320           RunRichiesta();
039900120320           opCodeNxt = opCodePrv;
040000090414
040100120320         ELSEIF opCodeCur = OPCODE_SUCCESSIVO;
040200090414
040300120320           priRpyOpCode = opCodeCur;
040400120320           LEAVE;
040500090414
040600120322         ELSEIF opCodeCur = OPCODE_RITORNO;
040700090414
040800120320           priRpyOpCode = opCodeCur;
040900120320           LEAVE;
041000090414
041100120322         ELSEIF opCodeCur = OPCODE_FINE;
041200090414
041300120320           LEAVE;
041400090414
041500120320         ELSE;
041600090414
041700120322           opCodeNxt = OPCODE_FINE;
041800120322           priRpyOpCode = TNSD41_RPYOPCODE_FINE;
041900090414
042000120320         ENDIF;
042100090409
042200120320       ENDDO;
042300090414
042400120320       WRITE frcDta;
042500090420
042600120320       IF priRpyFormato = tnsd4140o.formato;
042700120320         %SUBST(priRpyData : 1 : priRpyDataSize) = tnsd4140o;
042800120320       ENDIF;
042900090414
043000090409       RETURN;
043100090409
043200090415       //***********************************************************************
043300090415       //
043400090415       //
043500090415       //
043600090415       //***********************************************************************
043700090415
043800090415       BEGSR *INZSR;
043900090415
044000090415         CLEAR qusec;
044100090415         qusbprv = %SIZE(qusec);
044200120320         //dsp_da_idFiliale = DSPATR_NONPROTECT_UNDERSCORE;
044300090415         dsp_da_valDatI = DSPATR_NONPROTECT_UNDERSCORE;
044400090415         dsp_da_valDatF = DSPATR_NONPROTECT_UNDERSCORE;
044500120322         Organigramma_init(kpjba);
044600120322
044700120322       ENDSR;
044800090415
044900090409      /END-FREE
045000090409
045100090409
045200090409     P*--------------------------------------------------
045300090409     P* Procedure name: InzAnagrafica
045400090409     P* Purpose:        Inizializza finestra elenco.
045500090409     P* Returns:        Esito.
045600090409     P*--------------------------------------------------
045700090409     P InzAnagrafica   B
045800090409     D InzAnagrafica   PI            10I 0
045900090409
046000090409      /FREE
046100090409
046200090409       RESET dspInd;
046300090409       RESET testata;
046400120320       RESET azsof;
046500090409
046600120319       IF tnsd4140i.idRiga <> *ZERO;
046700090409
046800090409         EXEC SQL
046900120322           SELECT AZSOF00F.ID_FILIALE
047000120322                , AZSOF00F.ID_SOCIETA
047100120322                , AZSOF00F.TIPO_SOCIETA
047200120320                , AZSOF00F.VALIDITA_DATA_INIZIO
047300120320                , AZSOF00F.VALIDITA_DATA_FINE
047400120322             INTO :dsp_idFiliale
047500120322                , :dsp_idSocieta
047600120322                , :dsp_tpSocieta
047700090409                , :dsp_valDatIniz
047800090409                , :dsp_valDatFine
047900120320             FROM AZSOF00F
048000120320             WHERE AZSOF00F.ID_RIGA = :tnsd4140i.idRiga
048100090409         ;
048200090409
048300090409         IF sqlCode < *ZERO;
048400090409           RETURN sqlCode;
048500090409         ENDIF;
048600090410
048700120322       ELSE;
048800120322
048900120322         tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_FILIALE;
049000120322         dsp_idFiliale = tnsd4140i.idFiliale;
049100121218         dsp_tpSocieta = TIBSSOF_TIPO_SOCIETA_OPERATIVA;
049200120322
049300090410       ENDIF;
049400090409
049500090410       // Inizializzazioni specifiche per operazione.
049600090410
049700120320       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_MODIFICARE_ANAGRAFICA;
049800090410
049900090409         %SUBST(dsp_subTitolo : 36) = 'Modifica';
050000120320         //dsp_da_idFiliale = DSPATR_PROTECT_NORMAL;
050100090410
050200120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA;
050300090410
050400090409         %SUBST(dsp_subTitolo : 32) = 'Visualizzazione';
050500090410
050600120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_ANAGRAFICA;
050700090410
050800090409         %SUBST(dsp_subTitolo : 32) = 'Nuova anagrafica';
050900090410
051000090409         IF dsp_valDatFine < *HIVAL;
051100090409           dsp_valDatIniz = dsp_valDatFine + %DAYS(1);
051200090409         ELSE;
051300090409           dsp_valDatIniz = *LOVAL;
051400090409         ENDIF;
051500090410
051600090409         dsp_valDatFine = *HIVAL;
051700090415         dsp_da_valDatF = DSPATR_PROTECT_NORMAL;
051800120322
051900120322       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_FILIALE;
052000120322
052100120322         %SUBST(dsp_subTitolo : 32) = 'Nuova anagrafica';
052200120322         dsp_valDatIniz = *LOVAL;
052300120322         dsp_valDatFine = *HIVAL;
052400120322         dsp_da_valDatF = DSPATR_PROTECT_NORMAL;
052500090410
052600120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
052700090410
052800090409         %SUBST(dsp_subTitolo : 28) = 'Cancellazione anagrafica';
052900090410
053000090409       ENDIF;
053100121218
053200121218       IF dsp_idFiliale <> *ZERO;
053300121218         dsp_da_idFil = DSPATR_PROTECT_NORMAL;
053400121218       ELSE;
053500121218         RESET dsp_da_idFil;
053600121218       ENDIF;
053700090409
053800120320       RETURN TNSD41_RPYOPCODE_DONE;
053900090409
054000090409      /END-FREE
054100090409     P InzAnagrafica   E
054200090409
054300090409
054400090409     P*--------------------------------------------------
054500090409     P* Procedure name: SetAnagrafica
054600090409     P* Purpose:        Imposta dati finestra elenco.
054700090409     P* Returns:        Esito.
054800090409     P*--------------------------------------------------
054900090409     P SetAnagrafica   B
055000090409     D SetAnagrafica   PI            10I 0
055100090409
055200090409      /FREE
055300090409
055400090416       dsp_csrLocRow = dsp_rtnCsrRow;
055500090416       dsp_csrLocCol = dsp_rtnCsrCol;
055600090416
055700120320       RETURN TNSD41_RPYOPCODE_DONE;
055800090409
055900090409      /END-FREE
056000090409     P SetAnagrafica   E
056100090409
056200090409
056300090409     P*--------------------------------------------------
056400090409     P* Procedure name: PutAnagrafica
056500090409     P* Purpose:        Emette finestra elenco.
056600090409     P* Returns:        Esito.
056700090409     P*--------------------------------------------------
056800090409     P PutAnagrafica   B
056900090409     D PutAnagrafica   PI            10I 0
057000090409
057100090409      /FREE
057200090415
057300090415       WRITE testata;
057400090409       WRITE tasti;
057500090415
057600090415       IF dspInd.erroreGenerico;
057700090415         dspInd.erroreGenerico = *OFF;
057800120320         WRITE azsof;
057900090415         dspInd.erroreGenerico = *ON;
058000090415       ENDIF;
058100120320
058200120320       WRITE azsof;
058300090409
058400120320       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA
058500120320       OR tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
058600090409         WRITE protect;
058700090409       ENDIF;
058800090409
058900120320       RETURN TNSD41_RPYOPCODE_DONE;
059000090409
059100090409      /END-FREE
059200090409     P PutAnagrafica   E
059300090409
059400090409
059500090409     P*--------------------------------------------------
059600090409     P* Procedure name: GetAnagrafica
059700090409     P* Purpose:        Legge la finestra elenco.
059800090409     P* Returns:        Esito.
059900090409     P*--------------------------------------------------
060000090409     P GetAnagrafica   B
060100090409     D GetAnagrafica   PI            10I 0
060200090409
060300090409      /FREE
060400090409
060500120320       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA
060600120320       OR tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
060700090409         READ protect;
060800090409       ELSE;
060900120320         READ azsof;
061000090409       ENDIF;
061100090409
061200090409       RETURN dspInf.dsp_aid_num;
061300090409
061400090409      /END-FREE
061500090409     P GetAnagrafica   E
061600090409
061700090409
061800090409     P*--------------------------------------------------
061900090409     P* Procedure name: ChkAnagrafica
062000090409     P* Purpose:        Controlla i dati inseriti nella finestra elenco.
062100090409     P* Returns:        Esito.
062200090409     P*--------------------------------------------------
062300090409     P ChkAnagrafica   B
062400090409     D ChkAnagrafica   PI            10I 0
062500090415
062600090415      /FREE
062700090409
062800090409       RESET dspInd.erroreGenerico;
062900090416       RESET dspInd.errMsgDataValiditaSequenza;
063000090416       RESET dspInd.errMsgDataValiditaInizioSovrapposta;
063100090416       RESET dspInd.errMsgDataValiditaFineSovrapposta;
063200090416       RESET dspInd.errMsgIdSocieta;
063300120322       RESET dspInd.errMsgIdFiliale;
063400120322
063500120322       // Controlo ID filiale.
063600120405
063700120405       CLEAR dsp_dzFiliale;
063800120405
063900120405       azorg00f = TibsSof_GetOrganigrammaByIdFiliale(dsp_idFiliale);
064000120405
064100120405       IF tibssof_esito = TIBSSOF_ROW_NOT_FOUND
064200120405       OR tibssof_esito < TIBSSOF_ESITO_OK;
064300120405         dspInd.erroreGenerico = *ON;
064400120405         dspInd.errMsgIdFiliale = *ON;
064500120405       ELSE;
064600120405         dsp_dzFiliale = azorg00f.orgDes;
064700120405       ENDIF;
064800120405
064900090415       // Date in sequenza.
065000090415
065100120322       IF dsp_valDatFine = *LOVAL;
065200090416         dsp_valDatFine = *HIVAL;
065300090416       ENDIF;
065400090416
065500090415       IF dsp_valDatIniz > dsp_valDatFine;
065600090415         dspInd.erroreGenerico = *ON;
065700090416         dspInd.errMsgDataValiditaSequenza = *ON;
065800090415       ENDIF;
065900090415
066000090415       // Periodo sovrapposto.
066100090415
066200090415       EXEC SQL
066300090415         SELECT COUNT(*)
066400090415           INTO :count
066500120320           FROM AZSOF00F
066600120320           WHERE ID_FILIALE = :dsp_idFiliale
066700120319             AND ID_RIGA <> :tnsd4140i.idRiga
066800090415             AND :dsp_valDatIniz BETWEEN VALIDITA_DATA_INIZIO
066900090415                                     AND VALIDITA_DATA_FINE
067000120320           GROUP BY ID_FILIALE
067100090415       ;
067200090415
067300090415       IF sqlCode < *ZERO;
067400090415         DUMP(A);
067500090415       ELSEIF sqlCode <> 100 AND count > *ZERO;
067600090415         dspInd.erroreGenerico = *ON;
067700090416         dspInd.errMsgDataValiditaInizioSovrapposta = *ON;
067800090415       ENDIF;
067900090415
068000090415       EXEC SQL
068100090415         SELECT COUNT(*)
068200090415           INTO :count
068300120320           FROM AZSOF00F
068400120320           WHERE ID_FILIALE = :dsp_idFiliale
068500120319             AND ID_RIGA <> :tnsd4140i.idRiga
068600090415             AND :dsp_valDatFine BETWEEN VALIDITA_DATA_INIZIO
068700090415                                     AND VALIDITA_DATA_FINE
068800120320           GROUP BY ID_FILIALE
068900090415       ;
069000090415
069100090415       IF sqlCode < *ZERO;
069200090415         DUMP(A);
069300090415       ELSEIF sqlCode <> 100 AND count > *ZERO;
069400090415         dspInd.erroreGenerico = *ON;
069500090416         dspInd.errMsgDataValiditaFineSovrapposta = *ON;
069600090415       ENDIF;
069700090415
069800120402       // Id società.
069900090409
070000090415       RESET dsp_desSocieta;
070100120402
070200120402       IF Societa_NewSocieta(dsp_idSocieta) <> *ZERO;
070300120402         dspind.errMsgIdSocieta = *ON;
070400120402         dspind.erroreGenerico = *ON;
070500120402       ELSE;
070600120402         tibsSoco0 = Societa_GetAnagrafica('TIBSSOCO0');
070700120402         dsp_desSocieta = tibsSoco0.ragSocBrev;
070800120402       ENDIF;
070900090416
071000090416       // Restituisco la presenza di errori;
071100090416
071200120402       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA
071300120320       OR tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
071400120320         RETURN TNSD41_RPYOPCODE_DONE;
071500090416       ENDIF;
071600090416
071700090416       IF dspInd.erroreGenerico;
071800120320         RETURN TNSD41_RPYOPCODE_ERRORE;
071900090415       ENDIF;
072000090415
072100120320       RETURN TNSD41_RPYOPCODE_DONE;
072200090409
072300090409      /END-FREE
072400090409     P ChkAnagrafica   E
072500090409
072600090409
072700090409     P*--------------------------------------------------
072800090409     P* Procedure name: RunAnagrafica
072900090409     P* Purpose:        Elabora i dati inseriti nella finestra elenco.
073000090409     P* Returns:        Esito.
073100090409     P*--------------------------------------------------
073200090409     P RunAnagrafica   B
073300090409     D RunAnagrafica   PI            10I 0
073400090415
073500090415     D giorniCodiceFiscale...
073600090415     D                 S             10I 0
073700090415     D giorniSocieta...
073800090415     D                 S             10I 0
073900090409
074000090409      /FREE
074100090416
074200120320       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA;
074300120320         RETURN TNSD41_RPYOPCODE_DONE;
074400090416       ENDIF;
074500090409
074600120320       IF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_MODIFICARE_ANAGRAFICA;
074700090409
074800090409         EXEC SQL
074900120320           UPDATE AZSOF00F
075000120322             SET AZSOF00F.ID_SOCIETA = :dsp_idSocieta
075100120322               , AZSOF00F.TIPO_SOCIETA = :dsp_tpSocieta
075200120320               , AZSOF00F.VALIDITA_DATA_INIZIO = :dsp_valDatIniz
075300120320               , AZSOF00F.VALIDITA_DATA_FINE = DATE(:dsp_valDatFine)
075400120320             WHERE AZSOF00F.ID_RIGA = :tnsd4140i.idRiga
075500090409         ;
075600090409
075700090409         IF sqlCode < *ZERO;
075800090409           DUMP(A);
075900090409           RETURN sqlCode;
076000090409         ENDIF;
076100090409
076200120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_ANAGRAFICA;
076300090409
076400090409         EXEC SQL
076500120320           UPDATE AZSOF00F
076600120320             SET AZSOF00F.VALIDITA_DATA_FINE = :dsp_valDatIniz - 1 DAY
076700120320             WHERE AZSOF00F.ID_RIGA = :tnsd4140i.idRiga
076800090409         ;
076900090409
077000090409         IF sqlCode < *ZERO;
077100090409           DUMP(A);
077200090409           RETURN sqlCode;
077300090409         ENDIF;
077400090409
077500090409         EXEC SQL
077600120320           INSERT INTO AZSOF00F
077700120320             ( AZSOF00F.ID_FILIALE
077800120320             , AZSOF00F.VALIDITA_DATA_INIZIO
077900120320             , AZSOF00F.ID_SOCIETA
078000120320             , AZSOF00F.TIPO_SOCIETA
078100090409             )
078200120320             VALUES( :dsp_idFiliale
078300090409                   , :dsp_valDatIniz
078400120320                   , :dsp_idSocieta
078500120320                   , :dsp_tpSocieta
078600090409                   )
078700090409         ;
078800090409
078900090409         IF sqlCode < *ZERO;
079000090409           DUMP(A);
079100090409           RETURN sqlCode;
079200090409         ENDIF;
079300090409
079400090420         EXEC SQL
079500090420           VALUES IDENTITY_VAL_LOCAL()
079600120319             INTO :tnsd4140o.idRiga
079700090420         ;
079800090420
079900120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_FILIALE;
080000090409
080100090409         EXEC SQL
080200120320           INSERT INTO AZSOF00F
080300120320             ( AZSOF00F.ID_FILIALE
080400120320             , AZSOF00F.ID_SOCIETA
080500120320             , AZSOF00F.TIPO_SOCIETA
080600090409             )
080700120320             VALUES( :dsp_idFiliale
080800120320                   , :dsp_idSocieta
080900120320                   , :dsp_tpSocieta
081000090409                   )
081100090409         ;
081200090409
081300090409         IF sqlCode < *ZERO;
081400090409           DUMP(A);
081500090409           RETURN sqlCode;
081600090409         ENDIF;
081700090420
081800090420         EXEC SQL
081900090420           VALUES IDENTITY_VAL_LOCAL()
082000120319             INTO :tnsd4140o.idRiga
082100090420         ;
082200090409
082300120320       ELSEIF tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
082400090409
082500090409         EXEC SQL
082600120320           DELETE FROM AZSOF00F
082700120320             WHERE AZSOF00F.ID_RIGA = :tnsd4140i.idRiga
082800090409         ;
082900090409
083000090409         IF sqlCode < *ZERO;
083100090409           DUMP(A);
083200090409           RETURN sqlCode;
083300090409         ENDIF;
083400090409
083500090409       ENDIF;
083600090409
083700090415       // Controllo che i periodi di validità delle anagrafiche siano contigui.
083800090415
083900090415       EXEC SQL
084000090415         SELECT SUM(DAYS(A.VALIDITA_DATA_FINE) - DAYS(A.VALIDITA_DATA_INIZIO))
084100090415                + ( SELECT COUNT(*) - 1
084200120320                    FROM AZSOF00F AS B
084300120320                    WHERE B.ID_FILIALE = A.ID_FILIALE
084400090415                   )
084500090415           INTO :giorniCodiceFiscale
084600120320           FROM AZSOF00F AS A
084700120320           WHERE A.ID_FILIALE = :dsp_idFiliale
084800120320           GROUP BY A.ID_FILIALE
084900090415       ;
085000090415
085100090415       IF sqlCode < *ZERO;
085200090415         DUMP(A);
085300090415         RETURN sqlCode;
085400090415       ENDIF;
085500090415
085600120320       IF giorniCodiceFiscale < 3652058;
085700120320         RETURN ESITO_WARNING_PERIODI_NON_CONTIGUI;
085800090415       ENDIF;
085900090415
086000120320       RETURN TNSD41_RPYOPCODE_DONE;
086100090409
086200090409      /END-FREE
086300090409     P RunAnagrafica   E
086400090409
086500090414
086600090414     P*--------------------------------------------------
086700090414     P* Procedure name: RunRichiesta
086800090414     P* Purpose:        Gestione F4=Richiesta.
086900090414     P* Returns:        Esito.
087000090414     P*--------------------------------------------------
087100090414     P RunRichiesta    B
087200090414     D RunRichiesta    PI            10I 0
087300090428
087400090428     D dataInput...
087500090428     D                 S               D
087600090428     D dataOutput...
087700090428     D                 S               D
087800090428     D cEsito...
087900090428     D                 S              1A
088000120322     D rpyOpCode...
088100120322     D                 S             10A   STATIC
088200120322     D rpyIdMsg...
088300120322     D                 S             10I 0 STATIC
088400090428
088500090414      /FREE
088600090414
088700120320       IF dsp_rtnCsrFld = 'IDSOCIETA';
088800120322
088900120322         RESET tnsd4020i;
089000120322         RESET tnsd4020o;
089100120322         tnsd4020i.valDat = %DATE();
089200120322
089300120322         Societa_Elenco( SOCIETA_OPCODE_RICERCA
089400120322                       : rpyOpCode
089500120322                       : rpyIdMsg
089600120322                       : 'TNSD4020I'
089700120322                       : tnsd4020i
089800120322                       : %SIZE(tnsd4020i)
089900120322                       : tnsd4020o.formato
090000120322                       : tnsd4020o
090100120322                       : %SIZE(tnsd4020o)
090200120322                       );
090300120322
090400120322         IF tnsd4020o.idSocieta <> *BLANK;
090500120322           dsp_idSocieta = tnsd4020o.idSocieta;
090600120322         ENDIF;
090700120322
090800090428       ELSEIF dsp_rtnCsrFld = 'VALDATINIZ' OR dsp_rtnCsrFld = 'VALDATFINE'
090900090428       OR dsp_rtnCsrFld = 'ATTDATINIZ';
091000090428
091100090428         IF dsp_rtnCsrFld = 'VALDATINIZ';
091200090428           dataInput = dsp_valDatIniz;
091300090428         ELSEIF dsp_rtnCsrFld = 'VALDATFINE';
091400090428           dataInput = dsp_valDatFine;
091500090428         ENDIF;
091600090428
091700090428         Calendario( dataInput : dataOutput : cEsito );
091800090428
091900090428         IF cEsito = *OFF;
092000090428           IF dsp_rtnCsrFld = 'VALDATINIZ';
092100090428             dsp_valDatIniz = dataOutput;
092200090428           ELSEIF dsp_rtnCsrFld = 'VALDATFINE';
092300090428             dsp_valDatFine = dataOutput;
092400090428           ENDIF;
092500090428         ENDIF;
092600090428
092700090414       ENDIF;
092800090414
092900120320       RETURN TNSD41_RPYOPCODE_DONE;
093000090414
093100090414      /END-FREE
093200090414     P RunRichiesta    E
093300090414
