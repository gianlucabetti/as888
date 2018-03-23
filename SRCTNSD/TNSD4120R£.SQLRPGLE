000100090403     ***************************************************************************
000200090403     **
000300120319     ** Società delle filiali.
000400090403     ** Elenco.
000500090403     **
000600090403     ***************************************************************************
000700120322     H DFTACTGRP(*NO) ACTGRP('AZSOF') BNDDIR('TIBS':'PJXBND')
000800090403
000900090403     ***************************************************************************
001000090403     **
001100090403     ** Dichiarazione files.
001200090403     **
001300090403     ***************************************************************************
001400120319     Ftnsd4120d CF   E             WORKSTN
001500090403     F                                     PREFIX(dsp_)
001600090403     F                                     INDDS(dspInd)
001700090403     F                                     INFDS(dspInf)
001800120321     F                                     SFILE(filialiS:societaRrn)
001900090403
002000090403     ***************************************************************************
002100090403     **
002200120319     ** Costanti.
002300090403     **
002400090403     ***************************************************************************
002500120402     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002600120402     D/COPY GAITRASRC/SRCCONST,TNSD4100R
002601150819     D AUTORIZZATO_TUTTO...
002602150819     D                 C                   '000000000000000'
002700090403     D ESITO_ERROR...
002800090403     D                 C                   -1
002900090403     D ESITO_OK...
003000090403     D                 C                   0
003100090403     D ESITO_WARNING...
003200090403     D                 C                   1
003300090403     D ESITO_WARNING_ENTER...
003400090403     D                 C                   241
003500090403     D ESITO_WARNING_F3...
003600090403     D                 C                   51
003700090403     D ESITO_WARNING_F5...
003800090403     D                 C                   53
003900090409     D ESITO_WARNING_F10...
004000090409     D                 C                   58
004100090403     D ESITO_WARNING_F12...
004200090403     D                 C                   60
004300090420     D ESITO_WARNING_F13...
004400090420     D                 C                   177
004500090409     D OPZIONE_MODIFICARE...
004600090409     D                 C                   '2'
004700090409     D OPZIONE_NUOVAANAGRAFICA...
004800090409     D                 C                   '3'
004900090414     D OPZIONE_FINEATTIVITA...
005000090409     D                 C                   '4'
005100090409     D OPZIONE_VISUALIZZARE...
005200090409     D                 C                   '5'
005300090409     D OPZIONE_SOCIETAPROJ...
005400090409     D                 C                   '9'
005500090409     D OPZIONE_CANCELLAREANAGRAFICA...
005600090409     D                 C                   '44'
005700120321     D OPCODE_FINE...
005800120321     D                 C                   -1
005900120321     D OPCODE_RITORNO...
006000120321     D                 C                   -2
006100120321     D OPCODE_ELENCO_CHK...
006200120321     D                 C                   1
006300120321     D OPCODE_ELENCO_GET...
006400120321     D                 C                   2
006500120321     D OPCODE_ELENCO_INZ...
006600120321     D                 C                   3
006700120321     D OPCODE_ELENCO_PUT...
006800120321     D                 C                   4
006900120321     D OPCODE_ELENCO_RUN...
007000120321     D                 C                   5
007100120321     D OPCODE_ELENCO_SET...
007200120321     D                 C                   6
007300090403
007400090403     ***************************************************************************
007500090403     **
007600120319     ** Prototipi.
007700090403     **
007800090403     ***************************************************************************
007900120322      /DEFINE DFTACTGRP_NO
008000150819     D/COPY GAITRASRC/SRCPROTOPR,XCHKAUT
008001150819     D/COPY GAITRASRC/SRCPROTOPR,QUILNGTX
008002150819     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008100120402     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
008200120402     D/COPY GAITRASRC/SRCPROTOPR,TNSD4100R
008300120322      /UNDEFINE DFTACTGRP_NO
008400090403
008500090403     ***************************************************************************
008600090403     **
008700120319     ** Strutture dati.
008800090403     **
008900090403     ***************************************************************************
009000150819     D/COPY QSYSINC/QRPGLESRC,QUSEC
009001150819     D dspInd...
009100090403     D                 DS
009200090403     D                                     QUALIFIED
009300090403     D                                     INZ
009400090403     D subfileClear...
009500090403     D                         1      1N
009600090403     D subfileDisplay...
009700090403     D                         2      2N
009800090420     D subfileNextChange...
009900090420     D                         3      3N
010000090403     D erroreGenerico...
010100090403     D                        99     99N
010200090403     D dspInf...
010300090403     D                 DS
010400090403     D                                     QUALIFIED
010500090403     D  dsp_aid              369    369A
010600090403     D  dsp_aid_num          369    369U 0
010700090403     D  cursorRow            370    370I 0
010800090403     D  cursorCol            371    371I 0
010900090403     D  actCsrRow            382    382I 0
011000090403     D  actCsrCol            383    383I 0
011100120319     D kpjba         E DS                  QUALIFIED INZ
011200120319     D  knmus        E                     INZ(*USER)
011300120322     D tibsSocI0     E DS                  QUALIFIED INZ
011400120322     D tibsSocO0     E DS                  QUALIFIED INZ
011500120402     D tnsd4020i     E DS                  QUALIFIED INZ(*EXTDFT)
011600120402     D tnsd4020o     E DS                  QUALIFIED INZ(*EXTDFT)
011700120402     D tnsd4120i     E DS                  QUALIFIED INZ(*EXTDFT)
011800120402     D tnsd4120o     E DS                  QUALIFIED INZ(*EXTDFT)
011900120319     D tnsd4130i     E DS                  QUALIFIED INZ(*EXTDFT)
012000120402     D tnsd4130o     E DS                  QUALIFIED INZ(*EXTDFT)
012100120321     D tnsd4131i     E DS                  QUALIFIED INZ(*EXTDFT)
012200120322     D  righePag     E                     INZ(99)
012300120402     D tnsd4131o     E DS                  QUALIFIED INZ(*EXTDFT)
012400120402     D  idRighe_ary                  10I 0 DIM(99) OVERLAY(idRighe)
012500120402     D  idFiliali_ary                 5I 0 DIM(99) OVERLAY(idFiliali)
012600120321     D tnsd4140i     E DS                  QUALIFIED INZ(*EXTDFT)
012700120321     D tnsd4140o     E DS                  QUALIFIED INZ(*EXTDFT)
012800150819     D xChkAutDs     E DS                  QUALIFIED INZ
012801150819     D  xcaFnc       E                     INZ('YAZSOF')
012803150819     D  xcaTck       E                     INZ('CK')
012804150819     D  xcaNau       E                     INZ(*OFF)
012805150819
012900090403     ***************************************************************************
013000090403     **
013100120319     ** Campi.
013200090403     **
013300090403     ***************************************************************************
013400090403     D esito...
013500090403     D                 S             10I 0
013600090420     D esitoProj...
013700090420     D                 S              1A
013800090403     D opCodeCur...
013900120321     D                 S             10I 0
014000090403     D opCodeNxt...
014100090403     D                 S                   LIKE(opCodeCur)
014200120321     D                                     INZ(OPCODE_ELENCO_INZ)
014300090403     D opCodePrv...
014400090403     D                 S                   LIKE(opCodeCur)
014500090403     D rpyOpCode...
014600120321     D                 S             10I 0
014700120402     D rpyOpCodeChar...
014800120402     D                 S             10A
014900090403     D rpyIdMsg...
015000090403     D                 S             10I 0
015100090403     D societaRrn...
015200090403     D                 S              5U 0
015300150819     D textString...
015301150819     D                 S            256A
015302150819
015400090403     ***************************************************************************
015500090403     **
015600090403     ** Dichiarazione interfaccia procedura principale.
015700090403     **
015800090403     ***************************************************************************
015900120319     D Tnsd41_Elenco...
016000090403     D                 PI
016100120319     D  priRqsOpCode...
016200120319     D                               10I 0
016300090403     D                                     CONST
016400120319     D  priRpyOpCode...
016500120319     D                               10I 0
016600120319     D  priRpyIdMsg...
016700090403     D                               10I 0
016800120319     D  priRqsFormato...
016900090403     D                               10A
017000090403     D                                     CONST
017100090403     D                                     OPTIONS(*NOPASS)
017200120319     D  priRqsData...
017300090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
017400090403     D                                     CONST
017500120319     D  priRqsDataSize...
017600090403     D                               10I 0
017700090403     D                                     CONST
017800090403     D                                     OPTIONS(*NOPASS)
017900120319     D  priRpyFormato...
018000090403     D                               10A
018100090403     D                                     CONST
018200090403     D                                     OPTIONS(*NOPASS)
018300120319     D  priRpyData...
018400090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
018500120319     D  priRpyDataSize...
018600090403     D                               10I 0
018700090403     D                                     CONST
018800090403     D                                     OPTIONS(*NOPASS)
018900090403
019000090403     D*--------------------------------------------------
019100090403     D* Procedure name: InzElenco
019200090403     D* Purpose:        Inizializza finestra elenco.
019300090403     D* Returns:        Esito.
019400090403     D*--------------------------------------------------
019500090403     D InzElenco       PR            10I 0
019600090403
019700090403     D*--------------------------------------------------
019800090403     D* Procedure name: SetElenco
019900090403     D* Purpose:        Imposta dati finestra elenco.
020000090403     D* Returns:        Esito.
020100090403     D*--------------------------------------------------
020200090403     D SetElenco       PR            10I 0
020300090403
020400090403     D*--------------------------------------------------
020500090403     D* Procedure name: PutElenco
020600090403     D* Purpose:        Emette finestra elenco.
020700090403     D* Returns:        Esito.
020800090403     D*--------------------------------------------------
020900090403     D PutElenco       PR            10I 0
021000090403
021100090403     D*--------------------------------------------------
021200090403     D* Procedure name: GetElenco
021300090403     D* Purpose:        Legge la finestra elenco.
021400090403     D* Returns:        Esito.
021500090403     D*--------------------------------------------------
021600090403     D GetElenco       PR            10I 0
021700090403
021800090403     D*--------------------------------------------------
021900090403     D* Procedure name: ChkElenco
022000090403     D* Purpose:        Controlla i dati inseriti nella finestra elenco.
022100090403     D* Returns:        Esito.
022200090403     D*--------------------------------------------------
022300090403     D ChkElenco       PR            10I 0
022400090403
022500090403     D*--------------------------------------------------
022600090403     D* Procedure name: RunElenco
022700090403     D* Purpose:        Elabora i dati inseriti nella finestra elenco.
022800090403     D* Returns:        Esito.
022900090403     D*--------------------------------------------------
023000090403     D RunElenco       PR            10I 0
023100090403
023200090403
023300090403      /FREE
023400120321
023500120321       IF priRqsOpCode = TNSD41_RQSOPCODE_INIT;
023600120321
023700120321         IF priRqsFormato = 'KPJBA';
023800120321           kpjba = priRqsData;
023900120321         ENDIF;
024013150819
024100120321         Tnsd41_Selezione( TNSD41_RQSOPCODE_INIT
024200120321                         : rpyOpCode
024300120321                         : rpyIdMsg
024400120321                         : 'KPJBA'
024500120321                         : kpjba
024600120321                         : %SIZE(kpjba)
024700120321                         );
024800120321
024900120321         Tnsd41_Manutenzione( TNSD41_RQSOPCODE_INIT
025000120321                             : rpyOpCode
025100120321                             : rpyIdMsg
025200120321                             : 'KPJBA'
025300120321                             : kpjba
025400120321                             : %SIZE(kpjba)
025500120321                             );
025600120402
025700120402         Societa_Elenco( SOCIETA_OPCODE_INIT
025800120402                       : rpyOpCodeChar
025900120402                       : rpyIdMsg
026000120402                       : 'KPJBA'
026100120402                       : kpjba
026200120402                       : %SIZE(kpjba)
026300120402                       );
026400120402
026500120321         RETURN;
026600120321
026700120321       ELSEIF priRqsOpCode = TNSD41_RQSOPCODE_FINALIZE;
026800120321
026900120321         Tnsd41_Selezione( TNSD41_RQSOPCODE_FINALIZE
027000120321                         : rpyOpCode
027100120321                         : rpyIdMsg
027200120321                         );
027300120321
027400120321         Tnsd41_Manutenzione( TNSD41_RQSOPCODE_FINALIZE
027500120321                             : rpyOpCode
027600120321                             : rpyIdMsg
027700120321                             );
027800120321
027900120321         *INLR = *ON;
028000120321         RETURN;
028100120321
028200120321       ENDIF;
028201150819
028202150819       // Controllo autorizzazione: tutto o solo visualizzare?
028203150819
028204150819       RESET xChkAutDs;
028205150819       xChkAutDs.xcaPrf = kpjba.knmus;
028206150819       xChkAutDs.xcaVfu = AUTORIZZATO_TUTTO;
028207150819       Proj_Autorizzazione( xChkAutDs );
028208150819
028209150819       IF xChkAutDs.xcaRtn <> *ZERO;
028210150819         // Solo visualizzazione.
028211150819       ENDIF;
028300090403
028400120321       RESET opCodeNxt;
028500090408
028600120321       DOU opCodeCur = OPCODE_FINE;
028700090403
028800120321         opCodeCur = opCodeNxt;
028900090403
029000120321         IF opCodeCur = OPCODE_ELENCO_INZ;
029100120321           InzElenco();
029200120321           opCodeNxt = OPCODE_ELENCO_SET;
029300120321         ELSEIF opCodeCur = OPCODE_ELENCO_SET;
029400120321           SetElenco();
029500120321           opCodeNxt = OPCODE_ELENCO_PUT;
029600120321         ELSEIF opCodeCur = OPCODE_ELENCO_PUT;
029700120321           PutElenco();
029800120321           opCodeNxt = OPCODE_ELENCO_GET;
029900120321         ELSEIF opCodeCur = OPCODE_ELENCO_GET;
030000120321           esito = GetElenco();
030100120321           IF esito = ESITO_WARNING_ENTER;
030200120321             opCodeNxt = OPCODE_ELENCO_RUN;
030300120321           ELSEIF esito = ESITO_WARNING_F3;
030400120321             opCodeNxt = OPCODE_FINE;
030500120321             priRpyOpCode = TNSD41_RPYOPCODE_FINE;
030600120321           ELSEIF esito = ESITO_WARNING_F5;
030700120321             opCodeNxt = OPCODE_ELENCO_INZ;
030800120321           ELSEIF esito = ESITO_WARNING_F10;
030900120321             RESET tnsd4140i;
031000120321             tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_FILIALE;
031100120321             Tnsd41_Manutenzione( *ZERO
031200120321                                 : rpyOpCode
031300120321                                 : rpyIdMsg
031400120321                                 : 'TNSD4140I'
031500120321                                 : tnsd4140i
031600120321                                 : %SIZE(tnsd4140i)
031700120321                                 : 'TNSD4140O'
031800120321                                 : tnsd4140o
031900120321                                 : %SIZE(tnsd4140o)
032000120321                                 );
032100120321             opCodeNxt = OPCODE_ELENCO_INZ;
032200120321           ELSEIF esito = ESITO_WARNING_F12;
032300120321             opCodeNxt = OPCODE_RITORNO;
032400120321             priRpyOpCode = TNSD41_RPYOPCODE_RITORNO;
032500120321           ELSEIF esito = ESITO_WARNING_F13;
032600120321             opCodeNxt = OPCODE_ELENCO_SET;
032700120321           ELSE;
032800120321             opCodeNxt = OPCODE_FINE;
032900120321           ENDIF;
033000120321         ELSEIF opCodeCur = OPCODE_ELENCO_CHK;
033100120321           IF ChkElenco() = ESITO_OK;
033200120321             opCodeNxt = OPCODE_ELENCO_RUN;
033300120321           ELSE;
033400120321             opCodeNxt = OPCODE_ELENCO_SET;
033500120321           ENDIF;
033600120321         ELSEIF opCodeCur = OPCODE_ELENCO_RUN;
033700120321           IF ChkElenco() = ESITO_OK;
033800120321             esito = RunElenco();
033900120321             IF esito = ESITO_WARNING_F3;
034000120321               opCodeNxt = OPCODE_FINE;
034100120321               priRpyOpCode = TNSD41_RPYOPCODE_FINE;
034200120321             ELSEIF esito = ESITO_WARNING_F12;
034300120322               opCodeNxt = OPCODE_FINE;
034400120321             ELSE;
034500120321               opCodeNxt = OPCODE_ELENCO_SET;
034600120321             ENDIF;
034700120321           ELSE;
034800120321             opCodeNxt = OPCODE_ELENCO_SET;
034900120321           ENDIF;
035000120321         ELSEIF opCodeCur = OPCODE_RITORNO;
035100120322           priRpyOpCode = TNSD41_RPYOPCODE_RITORNO;
035200120321           LEAVE;
035300120321         ELSEIF opCodeCur = OPCODE_FINE;
035400120321           LEAVE;
035500120321         ELSE;
035600120321           opCodeNxt = OPCODE_FINE;
035700120321         ENDIF;
035800090403
035900120321       ENDDO;
036000090403
036100090403       RETURN;
036200090403
036300090420       //***********************************************************************
036400090420       //
036500090420       //
036600090420       //
036700090420       //***********************************************************************
036800090420
036900090420       BEGSR *INZSR;
037000090420
037100090420         dsp_sflRcdNbr = 1;
037200120322         Societa_Init();
037300090420
037400120322       ENDSR;
037500090420
037600090403      /END-FREE
037700090403
037800090403
037900090403     P*--------------------------------------------------
038000090403     P* Procedure name: InzElenco
038100090403     P* Purpose:        Inizializza finestra elenco.
038200090403     P* Returns:        Esito.
038300090403     P*--------------------------------------------------
038400090403     P InzElenco       B
038500090403     D InzElenco       PI            10I 0
038600090403
038700120321     D i               S             10I 0 STATIC
038800120321
038900090403      /FREE
039000090403
039100090403       RESET dspInd;
039200090403       dspInd.subfileClear = *ON;
039300090403       dspInd.subfileDisplay = *OFF;
039400120321       WRITE filialiC;
039500090403       dspInd.subfileClear = *OFF;
039600090403       RESET societaRrn;
039700090420       RESET dsp_sflRcdNbr;
039800090420
039900120319       IF priRqsFormato = 'TNSD4120I';
040000120319         tnsd4120i = %SUBST(priRqsData : 1 : priRqsDataSize);
040100090403       ENDIF;
040200090403
040300120319       EVAL-CORR tnsd4130i = tnsd4120i;
040400120321       RESET tnsd4130i.formato;
040500120321       RESET tnsd4130i.versione;
040600090403
040700120321       Tnsd41_Selezione( TNSD41_RQSOPCODE_SELEZIONE_NEW
040800120321                       : rpyOpCode
040900120321                       : rpyIdMsg
041000120321                       : tnsd4130i.formato
041100120321                       : tnsd4130i
041200120321                       : %SIZE(tnsd4130i)
041300120321                       : tnsd4130o.formato
041400120321                       : tnsd4130o
041500120321                       : %SIZE(tnsd4130o)
041600120321                       );
041700090403
041800120321       IF rpyOpCode <> TNSD41_RPYOPCODE_DONE;
041900090403         RETURN ESITO_ERROR;
042000090403       ENDIF;
042100090403
042200120321       CLEAR filialiS;
042300090409
042400090403       DOU rpyIdMsg = 100;
042500090403
042600120321         Tnsd41_Selezione( TNSD41_RQSOPCODE_SELEZIONE_GETNEXTROW
042700090403                          : rpyOpCode
042800090403                          : rpyIdMsg
042900120321                          : tnsd4131i.formato
043000120321                          : tnsd4131i
043100120321                          : %SIZE(tnsd4131i)
043200120321                          : tnsd4131o.formato
043300120321                          : tnsd4131o
043400120321                          : %SIZE(tnsd4131o)
043500090403                          );
043600090403
043700120321         IF tnsd4131o.nrRighe = *ZERO OR rpyIdMsg < ESITO_OK;
043800090403           LEAVE;
043900090403         ENDIF;
044000090403
044100120321         IF rpyOpCode = TNSD41_RPYOPCODE_DONE;
044200120321           FOR i = 1 TO tnsd4131o.nrRighe;
044300120321             RESET filialiS;
044400120321             dsp_idRiga = tnsd4131o.idRighe_ary(i);
044500120321             dsp_idFiliale  = tnsd4131o.idFiliali_ary(i);
044600120321             EXEC SQL
044700120321               SELECT AZORG00F.ORGDES
044800120321                 INTO :dsp_dzFiliale
044900120321                 FROM AZORG00F
045000120321                 WHERE ORGFIL = :dsp_idFiliale
045100120321             ;
045200120322             IF dsp_idRiga > *ZERO;
045300120321               EXEC SQL
045400120321                 SELECT AZSOF00F.ID_SOCIETA
045500120321                      , AZSOF00F.TIPO_SOCIETA
045600120321                      , CHAR(AZSOF00F.VALIDITA_DATA_INIZIO, EUR)
045700120321                      , CHAR(AZSOF00F.VALIDITA_DATA_FINE, EUR)
045800120321                   INTO :dsp_idSocieta
045900120321                      , :dsp_tpSocieta
046000120321                      , :dsp_valDatIniz
046100120321                      , :dsp_valDatFine
046200120322                   FROM AZSOF00F
046300120321                   WHERE AZSOF00F.ID_RIGA = :dsp_idRiga
046400120321               ;
046500120322               Societa_NewSocieta(dsp_idSocieta : tnsd4120i.valDat);
046600120322               tibssoco0 = Societa_GetAnagrafica('TIBSSOCO0');
046700120322               dsp_dzSocieta = tibssoco0.ragSocBrev;
046800120322             ENDIF;
046900120321             societaRrn += 1;
047000120321             WRITE filialiS;
047100120321           ENDFOR;
047200090403         ENDIF;
047300090403
047400090403       ENDDO;
047500090403
047600120321       Tnsd41_Selezione( TNSD41_RQSOPCODE_SELEZIONE_END
047700090403                        : rpyOpCode
047800090403                        : rpyIdMsg
047900090403                        );
048000090403
048100090403       dspInd.subfileDisplay = (societaRrn > *ZERO);
048200090403
048300090403       RETURN ESITO_OK;
048400090403
048500090403      /END-FREE
048600090403     P InzElenco       E
048700090403
048800090403
048900090403     P*--------------------------------------------------
049000090403     P* Procedure name: SetElenco
049100090403     P* Purpose:        Imposta dati finestra elenco.
049200090403     P* Returns:        Esito.
049300090403     P*--------------------------------------------------
049400090403     P SetElenco       B
049500090403     D SetElenco       PI            10I 0
049600090403
049700090403      /FREE
049800121218
049900121218       dsp_csrLocRow = dsp_rtnCsrRow;
050000121218       dsp_csrLocCol = dsp_rtnCsrCol;
050100121218
050200090403       RETURN ESITO_OK;
050300090403
050400090403      /END-FREE
050500090403     P SetElenco       E
050600090403
050700090403
050800090403     P*--------------------------------------------------
050900090403     P* Procedure name: PutElenco
051000090403     P* Purpose:        Emette finestra elenco.
051100090403     P* Returns:        Esito.
051200090403     P*--------------------------------------------------
051300090403     P PutElenco       B
051400090403     D PutElenco       PI            10I 0
051500090403
051600090403      /FREE
051700090403
051800090403       WRITE tasti;
051900120321       WRITE filialiC;
052000090403
052100090403       RETURN ESITO_OK;
052200090403
052300090403      /END-FREE
052400090403     P PutElenco       E
052500090403
052600090403
052700090403     P*--------------------------------------------------
052800090403     P* Procedure name: GetElenco
052900090403     P* Purpose:        Legge la finestra elenco.
053000090403     P* Returns:        Esito.
053100090403     P*--------------------------------------------------
053200090403     P GetElenco       B
053300090403     D GetElenco       PI            10I 0
053400090403
053500090420     D savOpzione...
053600090420     D                 S                   LIKE(dsp_opzione)
053700121218     D local           DS                  QUALIFIED STATIC
053800121218     D  societaRrn                   10I 0
053900090420
054000090420      /FREE
054100090403
054200121218       RESET local;
054300121218
054400120321       READ filialiC;
054500090420
054600121218       IF dsp_sflCsrRrn > 0;
054700121218         dsp_sflRcdNbr = dsp_sflCsrRrn;
054800121218       ENDIF;
054900090420
055000090420       IF dspInf.dsp_aid_num = ESITO_WARNING_F13;
055100090420         dspInd.subfileNextChange = *ON;
055200121218         FOR local.societaRrn = 1 TO 9999;
055300121218           CHAIN local.societaRrn filialiS;
055400090420           IF NOT %FOUND();
055500090420             LEAVE;
055600090420           ENDIF;
055700121218           dsp_sflRcdNbr = local.societaRrn;
055800090420           IF savOpzione <> *BLANK AND dsp_opzione = *BLANK;
055900090420             dsp_opzione = savOpzione;
056000120321             UPDATE filialiS;
056100090420             dsp_sflRcdNbr = dsp_sflCsrRrn;
056200090420           ELSE;
056300090420             dsp_opzione = %TRIML(dsp_opzione);
056400120321             UPDATE filialiS;
056500090420             savOpzione = dsp_opzione;
056600090420           ENDIF;
056700090420         ENDFOR;
056800090420         dspInd.subfileNextChange = *OFF;
056900090420       ENDIF;
057000090420
057100090403       RETURN dspInf.dsp_aid_num;
057200090403
057300090403      /END-FREE
057400090403     P GetElenco       E
057500090403
057600090403
057700090403     P*--------------------------------------------------
057800090403     P* Procedure name: ChkElenco
057900090403     P* Purpose:        Controlla i dati inseriti nella finestra elenco.
058000090403     P* Returns:        Esito.
058100090403     P*--------------------------------------------------
058200090403     P ChkElenco       B
058300090403     D ChkElenco       PI            10I 0
058400090403
058500090403      /FREE
058600090403
058700090403       RESET dspInd.erroreGenerico;
058800090403
058900090403       IF dspInd.erroreGenerico;
059000090403         RETURN ESITO_ERROR;
059100090403       ENDIF;
059200090403
059300090403       RETURN ESITO_OK;
059400090403
059500090403      /END-FREE
059600090403     P ChkElenco       E
059700090403
059800090403
059900090403     P*--------------------------------------------------
060000090403     P* Procedure name: RunElenco
060100090403     P* Purpose:        Elabora i dati inseriti nella finestra elenco.
060200090403     P* Returns:        Esito.
060300090403     P*--------------------------------------------------
060400090403     P RunElenco       B
060500090403     D RunElenco       PI            10I 0
060600090420
060700090403      /FREE
060800090403
060900090409       DOU %EOF();
061000090409
061100120321         READC filialiS;
061200090409
061300090409         IF %EOF();
061400090409           LEAVE;
061500150819         ELSEIF dsp_opzione = *BLANK;
061600090420           ITER;
061605150819         ELSEIF xChkAutDs.xcaRtn <> *ZERO;
061606150819           RESET qusec;
061607150819           textString = 'Sei autorizzato solo alla visualizzazione +
061608150819                         dell''elenco.';
061609150819           DisplayLongText( textString
061610150819                          : %SIZE(textString)
061611150819                          : *BLANK
061612150819                          : *BLANK
061613150819                          : qusec
061614150819                          );
061615150819           CLEAR dsp_opzione;
061616150819           UPDATE filialiS;
061617150819           ITER;
061700090409         ENDIF;
061800090409
061900090420         dsp_sflRcdNbr = dsp_sflCsrRrn;
062000090420
062100090409         dsp_opzione = %TRIML(dsp_opzione);
062200090409
062300120402         IF dsp_opzione = '25';
062400120402           IF dsp_idSocieta <> *BLANK;
062500120402             RESET tnsd4020i;
062600120402             tnsd4020i.idSocieta = dsp_idSocieta;
062700120402             Societa_Elenco( SOCIETA_OPCODE_ELENCO
062800120402                           : rpyOpCodeChar
062900120402                           : rpyIdMsg
063000120402                           : 'TNSD4020I'
063100120402                           : tnsd4020i
063200120402                           : %SIZE(tnsd4020i)
063300120402                           : 'TNSD4020O'
063400120402                           : tnsd4020o
063500120402                           : %SIZE(tnsd4020o)
063600120402                           );
063700120402           ENDIF;
063800120402           CLEAR dsp_opzione;
063900120402           UPDATE filialiS;
064000120402           ITER;
064100090409         ENDIF;
064200090414
064300090409         IF dsp_opzione = OPZIONE_MODIFICARE;
064400120321           tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_MODIFICARE_ANAGRAFICA;
064500120322           tnsd4140i.idFiliale = dsp_idFiliale;
064600090409         ELSEIF dsp_opzione = OPZIONE_NUOVAANAGRAFICA;
064700120321           tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_NUOVA_ANAGRAFICA;
064800090409         ELSEIF dsp_opzione = OPZIONE_CANCELLAREANAGRAFICA;
064900120321           tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_CANCELLARE_ANAGRAFICA;
065000090409         ELSEIF dsp_opzione = OPZIONE_VISUALIZZARE;
065100120321           tnsd4140i.rqsOpCode = TNSD41_RQSOPCODE_VISUALIZZARE_ANAGRAFICA;
065200120406         ELSE;
065300120406           CLEAR dsp_opzione;
065400120406           UPDATE filialiS;
065500120406           ITER;
065600090409         ENDIF;
065700090409
065800120319         tnsd4140i.idRiga = dsp_idRiga;
065900090409
066000120321         Tnsd41_Manutenzione( *ZERO
066100090409                             : rpyOpCode
066200090409                             : rpyIdMsg
066300120319                             : 'TNSD4140I'
066400120319                             : tnsd4140i
066500120319                             : %SIZE(tnsd4140i)
066600120319                             : 'TNSD4140O'
066700120319                             : tnsd4140o
066800120319                             : %SIZE(tnsd4140o)
066900090409                             );
067000090409
067100090409         CLEAR dsp_opzione;
067200120321         UPDATE filialiS;
067300090409
067400120321         IF rpyOpCode = TNSD41_RPYOPCODE_FINE;
067500090409           RETURN ESITO_WARNING_F3;
067600120321         ELSEIF rpyOpCode = TNSD41_RPYOPCODE_RITORNO;
067700090420           RETURN ESITO_WARNING_F12;
067800090409           LEAVE;
067900090409         ENDIF;
068000090409
068100090409       ENDDO;
068200090409
068300090403       RETURN ESITO_OK;
068400090403
068500090403      /END-FREE
068600090403     P RunElenco       E
068700090403
