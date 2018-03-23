000100090327     ***************************************************************************
000200090327     **
000300120220     ** Società delle filiali.
000400090330     ** Gestione finestra filtro.
000500090327     **
000600090327     ***************************************************************************
000700120405     H DFTACTGRP(*NO) ACTGRP('AZSOF') BNDDIR('PJXBND':'TIBS')
000800090330
000900090330     ***************************************************************************
001000090330     **
001100120220     ** File.
001200090330     **
001300090330     ***************************************************************************
001400120402     Ftnsd4110d CF   E             WORKSTN USROPN PREFIX(dsp_)
001500120402     F                                     INDDS(dspInd) INFDS(dspInf)
001600090330
001700090330     ***************************************************************************
001800090330     **
001900120220     ** Costanti.
002000090330     **
002100090330     ***************************************************************************
002200120223     D/COPY GAITRASRC/SRCCONST,TIBSSOFR
002300120320     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002400120320     D/COPY GAITRASRC/SRCCONST,TNSD4100R
002500120320     D ESITO_WARNING_ENTER...
002600120320     D                 C                   241
002700120320     D ESITO_WARNING_F2...
002800120320     D                 C                   50
002900120320     D ESITO_WARNING_F3...
003000120320     D                 C                   51
003100120320     D ESITO_WARNING_F4...
003200120320     D                 C                   52
003300120320     D ESITO_WARNING_F5...
003400120320     D                 C                   53
003500120320     D ESITO_WARNING_F6...
003600120320     D                 C                   54
003700120320     D ESITO_WARNING_F8...
003800120320     D                 C                   56
003900120320     D ESITO_WARNING_F12...
004000120320     D                 C                   60
004100120320     D OPCODE_FILTRO_CHK...
004200120220     D                 C                   1
004300120223     D OPCODE_FILTRO_CHK_RUN...
004400120223     D                 C                   2
004500120220     D OPCODE_FILTRO_GET...
004600120223     D                 C                   3
004700120220     D OPCODE_FILTRO_INZ...
004800120223     D                 C                   4
004900120220     D OPCODE_FILTRO_PUT...
005000120223     D                 C                   5
005100120220     D OPCODE_FILTRO_RUN...
005200120223     D                 C                   6
005300120220     D OPCODE_FILTRO_SET...
005400120223     D                 C                   7
005500120220     D OPCODE_DONE...
005600120223     D                 C                   8
005700120220     D OPCODE_FINE...
005800120223     D                 C                   9
005900120220     D OPCODE_RITORNO...
006000120223     D                 C                   10
006100120223     D PROJ_RICERCA...
006200120223     D                 C                   '01'
006300120223     D PROJ_ENTER...
006400120223     D                 C                   ' '
006500120223     D PROJ_F3...
006600120223     D                 C                   '1'
006700120223     D PROJ_F12...
006800120223     D                 C                   '2'
006900090330
007000090330     ***************************************************************************
007100090330     **
007200120220     ** Prototipi.
007300090330     **
007400090330     ***************************************************************************
007500120223      /DEFINE DFTACTGRP_NO
007600120223      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
007700120405      /COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
007800120223      /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
007900120320      /COPY GAITRASRC/SRCPROTOPR,TNSD4000R
008000120320      /COPY GAITRASRC/SRCPROTOPR,TNSD4100R
008100120223      /COPY GAITRASRC/SRCPROTOPR,TNSD24R
008200120223      /COPY GAITRASRC/SRCPROTOPR,TRULCALER
008300120322      /COPY GAITRASRC/SRCPROTOPR,X§TABER
008400120402      /COPY GAITRASRC/SRCPROTOPR,TIBS02R
008500120402      /UNDEFINE DFTACTGRP_NO
008600090330
008700090330     ***************************************************************************
008800090330     **
008900120220     ** Strutture dati.
009000090330     **
009100090330     ***************************************************************************
009200120223     D azorg00f      E DS                  QUALIFIED INZ
009300120405     D ds05          E DS                  QUALIFIED INZ
009400120405     D  tblUni                             LIKE(tblUni) OVERLAY(ds05)
009500120405     D ds14          E DS                  QUALIFIED INZ
009600120405     D  tblUni                             LIKE(tblUni) OVERLAY(ds14)
009601171218     D ds17          E DS                  QUALIFIED INZ
009602171218     D  tblUni                             LIKE(tblUni) OVERLAY(ds17)
009700120223     D dspInd          DS                  QUALIFIED INZ
009800120223     D erroreIdFiliale...
009900090331     D                         1      1N
010000120223     D erroreIdSocieta...
010100090331     D                         2      2N
010200120405     D erroreIdArea...
010300120405     D                         3      3N
010400120405     D erroreIdRegione...
010500120405     D                         4      4N
010501171218     D erroreIdDistretto...
010502171218     D                         5      5N
010600090331     D erroreGenerico...
010700090331     D                        99     99N
010800120223     D dspInf          DS                  QUALIFIED
010900090331     D  dsp_aid              369    369A
011000090331     D  dsp_aid_num          369    369U 0
011100090331     D  cursorRow            370    370I 0
011200090331     D  cursorCol            371    371I 0
011300090331     D  actCsrRow            382    382I 0
011400090331     D  actCsrCol            383    383I 0
011500120223     D kpjba         E DS                  QUALIFIED INZ
011600120223     D  knmus        E                     INZ(*USER)
011700120322     D tabel00f      E DS                  TEMPLATE
011800120405     D tibssoci0     E DS                  INZ QUALIFIED
011900120405     D tibssoco0     E DS                  INZ QUALIFIED
012000120402     D tibs02ds      E DS                  INZ QUALIFIED
012100120402     D  t02mod       E                     INZ('C')
012200120402     D tnsd4020i     E DS                  QUALIFIED INZ(*EXTDFT)
012300120320     D tnsd4020o     E DS                  QUALIFIED INZ(*EXTDFT)
012400120320     D tnsd4110i     E DS                  QUALIFIED INZ(*EXTDFT)
012500120320     D tnsd4110o     E DS                  QUALIFIED INZ(*EXTDFT)
012600090420
012700090330     ***************************************************************************
012800090330     **
012900120220     ** Campi.
013000090330     **
013100090331     ***************************************************************************
013200090331     D esito...
013300090331     D                 S             10I 0
013400120223     D esitoChar...
013500120223     D                 S              1A
013600090331     D opCodeCur...
013700120220     D                 S             10I 0
013800090330     D opCodeNxt...
013900120220     D                 S                   LIKE(opCodeCur)
014000120220     D                                     INZ(OPCODE_FILTRO_INZ)
014100090330     D opCodePrv...
014200090330     D                 S                   LIKE(opCodeCur)
014300120223     D tibssof_esito...
014400120223     D                 S             10I 0 IMPORT
014500120223
014600090330     ***************************************************************************
014700090330     **
014800120220     ** Parametri procedura principale.
014900090330     **
015000090330     ***************************************************************************
015100120220     D Tnsd41_Filtro...
015200090330     D                 PI
015300120220     D  priRqsOpCode...
015400120220     D                               10I 0
015500090330     D                                     CONST
015600120220     D  priRpyOpCode...
015700120220     D                               10I 0
015800120220     D  priRpyIdMsg...
015900090330     D                               10I 0
016000120220     D  priRqsFormato...
016100090330     D                               10A
016200090330     D                                     CONST
016300090330     D                                     OPTIONS(*NOPASS)
016400120220     D  priRqsData...
016500090330     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
016600090330     D                                     CONST
016700120220     D  priRqsDataSize...
016800090330     D                               10I 0
016900090330     D                                     CONST
017000090330     D                                     OPTIONS(*NOPASS)
017100120220     D  priRpyFormato...
017200090330     D                               10A
017300090330     D                                     CONST
017400090330     D                                     OPTIONS(*NOPASS)
017500120220     D  priRpyData...
017600090330     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
017700120220     D  priRpyDataSize...
017800090330     D                               10I 0
017900090330     D                                     CONST
018000090330     D                                     OPTIONS(*NOPASS)
018100090330
018200090330     D*--------------------------------------------------
018300090330     D* Procedure name: InzFiltro
018400090330     D* Purpose:        Inizializza finestra filtro.
018500090330     D* Returns:        Esito.
018600090330     D*--------------------------------------------------
018700090330     D InzFiltro       PR            10I 0
018800090330
018900090330     D*--------------------------------------------------
019000090330     D* Procedure name: SetFiltro
019100090330     D* Purpose:        Imposta dati finestra filtro.
019200090330     D* Returns:        Esito.
019300090330     D*--------------------------------------------------
019400090330     D SetFiltro       PR            10I 0
019500090330
019600090330     D*--------------------------------------------------
019700090330     D* Procedure name: PutFiltro
019800090330     D* Purpose:        Emette finestra filtro.
019900090330     D* Returns:        Esito.
020000090330     D*--------------------------------------------------
020100090330     D PutFiltro       PR            10I 0
020200090330
020300090330     D*--------------------------------------------------
020400090330     D* Procedure name: GetFiltro
020500090330     D* Purpose:        Legge la finestra filtro.
020600090330     D* Returns:        Esito.
020700090330     D*--------------------------------------------------
020800090330     D GetFiltro       PR            10I 0
020900090330
021000090330     D*--------------------------------------------------
021100090330     D* Procedure name: ChkFiltro
021200090330     D* Purpose:        Controlla i dati inseriti nella finestra filtro.
021300090330     D* Returns:        Esito.
021400090330     D*--------------------------------------------------
021500090330     D ChkFiltro       PR            10I 0
021600090330
021700090330     D*--------------------------------------------------
021800090330     D* Procedure name: RunFiltro
021900090330     D* Purpose:        Elabora i dati inseriti nella finestra filtro.
022000090330     D* Returns:        Esito.
022100090330     D*--------------------------------------------------
022200090330     D RunFiltro       PR            10I 0
022300120223
022400120223     D*--------------------------------------------------
022500120223     D* Procedure name: RunRichiesta
022600120223     D* Purpose:        Elabora il tasto F4=Richiesta premuto su un campo.
022700120223     D* Returns:        Valore scelto.
022800120223     D* Parameter:      priRecord => Nome record.
022900120223     D* Parameter:      priField => Nome campo.
023000120223     D*--------------------------------------------------
023100120223     D RunRichiesta    PR            10A
023200120223     D  priRecord                    10A
023300120223     D  priField                     10A
023400120223     D  priValore                    10A   OPTIONS(*NOPASS) CONST
023500120223
023600090330
023700090330      /FREE
023800090420
023900120320       SELECT;
024000120320
024100120320         WHEN priRqsOpCode = TNSD41_RQSOPCODE_INIT;
024200120320
024300120320           IF priRqsFormato = 'KPJBA';
024400120320             kpjba = priRqsData;
024500120320           ENDIF;
024600120320
024700120320           RETURN;
024800120320
024900120320         WHEN priRqsOpCode = TNSD41_RQSOPCODE_FINALIZE;
025000120320
025100120320           *INLR = *ON;
025200120320           RETURN;
025300120320
025400120320       ENDSL;
025500090330
025600120320       RESET opCodeNxt;
025700120320       OPEN tnsd4110d;
025800090408
025900120320       DOU opCodeCur = OPCODE_FINE;
026000090330
026100120320         opCodeCur = opCodeNxt;
026200090330
026300120320         IF opCodeCur = OPCODE_FILTRO_INZ;
026400120320           InzFiltro();
026500120320           opCodeNxt = OPCODE_FILTRO_SET;
026600120320         ELSEIF opCodeCur = OPCODE_FILTRO_SET;
026700120320           SetFiltro();
026800120320           opCodeNxt = OPCODE_FILTRO_PUT;
026900120320         ELSEIF opCodeCur = OPCODE_FILTRO_PUT;
027000120320           PutFiltro();
027100120320           opCodeNxt = OPCODE_FILTRO_GET;
027200120320         ELSEIF opCodeCur = OPCODE_FILTRO_GET;
027300120320           esito = GetFiltro();
027400120320           IF esito = ESITO_WARNING_ENTER;
027500120320             opCodeNxt = OPCODE_FILTRO_CHK_RUN;
027600120320           ELSEIF esito = ESITO_WARNING_F2;
027700120320             opCodeNxt = OPCODE_FILTRO_CHK;
027800120320           ELSEIF esito = ESITO_WARNING_F4;
027900120320             opCodeNxt = OPCODE_FILTRO_PUT;
028000120320           ELSEIF esito = ESITO_WARNING_F5;
028100120320             opCodeNxt = OPCODE_FILTRO_INZ;
028200120320           ELSEIF esito = ESITO_WARNING_F12;
028300120320             opCodeNxt = OPCODE_RITORNO;
028400120320           ELSE;
028500120320             opCodeNxt = OPCODE_FINE;
028600120320           ENDIF;
028700120320         ELSEIF opCodeCur = OPCODE_FILTRO_CHK_RUN;
028800120320           IF ChkFiltro() = TNSD41_RPYOPCODE_DONE;
028900120320             opCodeNxt = OPCODE_FILTRO_RUN;
029000120320           ELSE;
029100120320             opCodeNxt = OPCODE_FILTRO_SET;
029200120320           ENDIF;
029300120320         ELSEIF opCodeCur = OPCODE_FILTRO_CHK;
029400120320             ChkFiltro();
029500120320             opCodeNxt = OPCODE_FILTRO_SET;
029600120320         ELSEIF opCodeCur = OPCODE_FILTRO_RUN;
029700120320           IF ChkFiltro() = TNSD41_RPYOPCODE_DONE;
029800120320             RunFiltro();
029900120320             opCodeNxt = OPCODE_DONE;
030000120320           ELSE;
030100120320             opCodeNxt = OPCODE_FILTRO_SET;
030200120320           ENDIF;
030300120320         ELSEIF opCodeCur = OPCODE_DONE;
030400120320           priRpyOpCode = TNSD41_RPYOPCODE_DONE;
030500120320           LEAVE;
030600120320         ELSEIF opCodeCur = OPCODE_RITORNO;
030700120320           priRpyOpCode = TNSD41_RPYOPCODE_RITORNO;
030800120320           LEAVE;
030900120320         ELSEIF opCodeCur = OPCODE_FINE;
031000120320           priRpyOpCode = TNSD41_RPYOPCODE_FINE;
031100120320           LEAVE;
031200120320         ELSE;
031300120320           opCodeNxt = OPCODE_FINE;
031400120320         ENDIF;
031500090330
031600120320       ENDDO;
031700090330
031800120320       CLOSE tnsd4110d;
031900090330
032000090330       RETURN;
032100090330
032200090330      /END-FREE
032300090330
032400090330
032500090330     P*--------------------------------------------------
032600090330     P* Procedure name: InzFiltro
032700090330     P* Purpose:        Inizializza finestra filtro.
032800090330     P* Returns:        Esito.
032900090330     P*--------------------------------------------------
033000090330     P InzFiltro       B
033100090330     D InzFiltro       PI            10I 0
033200090330
033300090330      /FREE
033400120405
033500120405       Societa_Init();
033600120405       Tibssof_Init();
033700090330
033800090330       RESET filtro;
033900090331       RESET dspInd;
034000090330
034100120223       IF priRqsFormato = 'TNSD4110I';
034200120220         tnsd4110i = %SUBST(priRqsData : 1 : priRqsDataSize);
034300120220         dsp_idSocieta = tnsd4110i.idSocieta;
034400120220         dsp_idFiliale = tnsd4110i.idFiliale;
034500120322         dsp_idArea = tnsd4110i.idArea;
034600120322         dsp_idRegione = tnsd4110i.idRegione;
034601171218         dsp_idDistrett = tnsd4110i.idDistrett;
034700120220         dsp_tpSocieta = tnsd4110i.tpSocieta;
034800120322         dsp_valDat = tnsd4110i.valDat;
034900120405         ChkFiltro();
035000120405       ENDIF;
035100120223
035200120320       RETURN TNSD41_RPYOPCODE_DONE;
035300090330
035400090330      /END-FREE
035500090330     P InzFiltro       E
035600090330
035700090330
035800090330     P*--------------------------------------------------
035900090330     P* Procedure name: SetFiltro
036000090330     P* Purpose:        Imposta dati finestra filtro.
036100090330     P* Returns:        Esito.
036200090330     P*--------------------------------------------------
036300090330     P SetFiltro       B
036400090330     D SetFiltro       PI            10I 0
036500090330
036600090330      /FREE
036700090330
036800120320       RETURN TNSD41_RPYOPCODE_DONE;
036900090330
037000090330      /END-FREE
037100090330     P SetFiltro       E
037200090330
037300090330
037400090330     P*--------------------------------------------------
037500090330     P* Procedure name: PutFiltro
037600090330     P* Purpose:        Emette finestra filtro.
037700090330     P* Returns:        Esito.
037800090330     P*--------------------------------------------------
037900090330     P PutFiltro       B
038000090330     D PutFiltro       PI            10I 0
038100090330
038200090330      /FREE
038300090330
038400120223       IF dspInd.erroreGenerico;
038500120223         dspInd.erroreGenerico = *OFF;
038600120223         WRITE filtro;
038700120223         dspInd.erroreGenerico = *ON;
038800120223       ENDIF;
038900120223
039000090330       WRITE filtro;
039100090330
039200120320       RETURN TNSD41_RPYOPCODE_DONE;
039300090330
039400090330      /END-FREE
039500090330     P PutFiltro       E
039600090330
039700090330
039800090330     P*--------------------------------------------------
039900090330     P* Procedure name: GetFiltro
040000090330     P* Purpose:        Legge la finestra filtro.
040100090330     P* Returns:        Esito.
040200090330     P*--------------------------------------------------
040300090330     P GetFiltro       B
040400090330     D GetFiltro       PI            10I 0
040500090330
040600090330      /FREE
040700090330
040800090330       READ filtro;
040900090330
041000120320       IF dspInf.dsp_aid_num = ESITO_WARNING_F4;
041100120223         SELECT;
041200120223           WHEN dsp_rtnCsrLocF = 'IDFILIALE';
041300120223             dsp_idFiliale = %INT(RunRichiesta( dsp_rtnCsrLocR
041400120223                                              : dsp_rtnCsrLocF
041500120322                                              : %EDITC(dsp_idFiliale : 'X')
041600120223                                              ));
041700120322           WHEN dsp_rtnCsrLocF = 'IDAREA';
041800120322             dsp_idArea = %INT(RunRichiesta( dsp_rtnCsrLocR
041900120322                                           : dsp_rtnCsrLocF
042000120322                                           : %EDITC(dsp_idArea : 'X')
042100120322                                           ));
042200120322           WHEN dsp_rtnCsrLocF = 'IDREGIONE';
042300120322             dsp_idRegione = %INT(RunRichiesta( dsp_rtnCsrLocR
042400120322                                              : dsp_rtnCsrLocF
042500120322                                              : %EDITC(dsp_idRegione : 'X')
042600120322                                              ));
042601171218           WHEN dsp_rtnCsrLocF = 'IDDISTRETT';
042602171218             dsp_idDistrett = RunRichiesta( dsp_rtnCsrLocR
042603171218                                          : dsp_rtnCsrLocF
042604171218                                          : dsp_idDistrett
042605171218                                          );
042700120223           WHEN dsp_rtnCsrLocF = 'IDSOCIETA';
042800120223             dsp_idSocieta = RunRichiesta( dsp_rtnCsrLocR
042900120223                                         : dsp_rtnCsrLocF
043000120223                                         : dsp_idSocieta
043100120223                                         );
043200120223           WHEN dsp_rtnCsrLocF = 'TPSOCIETA';
043300120223             dsp_tpSocieta = RunRichiesta( dsp_rtnCsrLocR
043400120223                                         : dsp_rtnCsrLocF
043500120223                                         : dsp_tpSocieta
043600120223                                         );
043700120223           WHEN dsp_rtnCsrLocF = 'VALDAT';
043800120223             dsp_valDat = %DATE(RunRichiesta( dsp_rtnCsrLocR
043900120223                                            : dsp_rtnCsrLocF
044000120223                                            : %CHAR(dsp_valDat)
044100120223                                            ));
044200120223         ENDSL;
044300120223       ENDIF;
044400120223
044500120223       RETURN dspInf.dsp_aid_num;
044600090330
044700090330      /END-FREE
044800090330     P GetFiltro       E
044900090330
045000090330
045100090330     P*--------------------------------------------------
045200090330     P* Procedure name: ChkFiltro
045300090330     P* Purpose:        Controlla i dati inseriti nella finestra filtro.
045400090330     P* Returns:        Esito.
045500090330     P*--------------------------------------------------
045600090330     P ChkFiltro       B
045700090330     D ChkFiltro       PI            10I 0
045800090330
045900090330      /FREE
046000090330
046100090331       RESET dspInd.erroreGenerico;
046200120223       RESET dspInd.erroreIdFiliale;
046300120223       RESET dspInd.erroreIdSocieta;
046400090331
046500120223       // Controllo ID filiale.
046600120223
046700120321       CLEAR dsp_dzFiliale;
046800120321
046900120405       IF dsp_idFiliale = 0;
047000120405         dsp_dzFiliale = 'Tutto';
047100120405       ELSE;
047200120321         azorg00f = TibsSof_GetOrganigrammaByIdFiliale(dsp_idFiliale);
047300120321         IF tibssof_esito = TIBSSOF_ROW_NOT_FOUND
047400120321         OR tibssof_esito < TIBSSOF_ESITO_OK;
047500120321           dspInd.erroreGenerico = *ON;
047600120321           dspInd.erroreIdFiliale = *ON;
047700120321           CLEAR dsp_dzFiliale;
047800120321         ELSE;
047900120321           dsp_dzFiliale = azorg00f.orgDes;
048000120321         ENDIF;
048100120321       ENDIF;
048200120402
048300120402       // Controllo tipo società.
048400120402
048500120405       CLEAR dsp_dzTpSoc;
048600120405
048700120405       IF dsp_tpSocieta = *BLANK;
048800120405         dsp_dzTpSoc = 'Tutto';
048900120402       ELSE;
049000120402         RESET tibs02ds;
049100120402         tibs02ds.t02cod = '009';
049200120402         tibs02ds.t02ke1 = dsp_tpSocieta;
049300120402         TNTBE_RicercaControllo(kpjba : tibs02ds);
049400120402         IF tibs02ds.t02err <> *BLANK;
049500120402           dspInd.erroreGenerico = *ON;
049600120402         ELSE;
049700120402           dsp_dzTpSoc = tibs02ds.t02uni;
049800120402         ENDIF;
049900120402       ENDIF;
050000120405
050100120405       // Controllo ID società.
050200120402
050300120405       CLEAR dsp_ragSocBrev;
050400120405
050500120405       IF dsp_idSocieta = *BLANK;
050600120405         dsp_ragSocBrev = 'Tutto';
050700120405       ELSE;
050800120405         IF Societa_NewSocieta(dsp_idSocieta) <> *ZERO;
050900120405           dspInd.erroreGenerico = *ON;
051000120405           dspInd.erroreIdSocieta = *ON;
051100120405         ELSE;
051200120405           tibssoco0 = Societa_GetAnagrafica('TIBSSOCO0');
051300120405           dsp_ragSocBrev = tibssoco0.ragSocBrev;
051400120405         ENDIF;
051500120405       ENDIF;
051600120405
051700120405       // Controllo ID area.
051800120405
051900120405       CLEAR dsp_§05des;
052000120405
052100120405       IF dsp_idArea = *ZERO;
052200120405         dsp_§05des = 'Tutto';
052300120405       ELSE;
052400120405         EXEC SQL
052500120405           SELECT tblUni
052600120405             INTO :ds05.tblUni
052700120405             FROM tabel00f
052800120405             WHERE tblKut = 1 AND tblCod = '05' AND tblKey = :dsp_idArea
052900120405               AND tblFlg = ''
053000120405         ;
053100120405         IF sqlCode = 100;
053200120405           dspInd.erroreGenerico = *ON;
053300120405           dspInd.erroreIdArea = *ON;
053400120405         ELSE;
053500120405           dsp_§05des = ds05.§05des;
053600120405         ENDIF;
053700120405       ENDIF;
053800120405
053900120405       // Controllo ID regione.
054000120405
054100120405       CLEAR dsp_§14des;
054200120405
054300120405       IF dsp_idRegione = *ZERO;
054400120405         dsp_§14des = 'Tutto';
054500120405       ELSE;
054600120405         EXEC SQL
054700120405           SELECT tblUni
054800120405             INTO :ds14.tblUni
054900120405             FROM tabel00f
055000120405             WHERE tblKut = 1 AND tblCod = '14' AND tblKey = :dsp_idRegione
055100120405               AND tblFlg = ''
055200120405         ;
055300120405         IF sqlCode = 100;
055400120405           dspInd.erroreGenerico = *ON;
055500120405           dspInd.erroreIdRegione = *ON;
055600120405         ELSE;
055700120405           dsp_§14des = ds14.§14des;
055800120405         ENDIF;
055900120405       ENDIF;
055901171218
055902171218       // Controllo ID distretto.
055903171218
055904171218       CLEAR dsp_§17des;
055905171218
055906171218       IF dsp_idDistrett = *BLANK;
055907171218         dsp_§17des = 'Tutto';
055908171218       ELSE;
055909171218         EXEC SQL
055910171218           SELECT tblUni
055911171218             INTO :ds17.tblUni
055912171218             FROM tabel00f
055913171218             WHERE tblKut = 1 AND tblCod = '17' AND tblKey = :dsp_idDistrett
055914171218               AND tblFlg = ''
055915171218         ;
055916171218         IF sqlCode = 100;
055917171218           dspInd.erroreGenerico = *ON;
055918171218           dspInd.erroreIdDistretto = *ON;
055919171218         ELSE;
055920171218           dsp_§17des = ds17.§17des;
055921171218         ENDIF;
055922171218       ENDIF;
056000120405
056100120405       // Ci sono degli errori?
056200120405
056300120223       IF dspInd.erroreGenerico;
056400120320         RETURN TNSD41_RPYOPCODE_ERRORE;
056500090331       ENDIF;
056600090331
056700120320       RETURN TNSD41_RPYOPCODE_DONE;
056800090330
056900090330      /END-FREE
057000090330     P ChkFiltro       E
057100090330
057200090330
057300090330     P*--------------------------------------------------
057400090330     P* Procedure name: RunFiltro
057500090330     P* Purpose:        Elabora i dati inseriti nella finestra filtro.
057600090330     P* Returns:        Esito.
057700090330     P*--------------------------------------------------
057800090330     P RunFiltro       B
057900090330     D RunFiltro       PI            10I 0
058000090330
058100090330      /FREE
058200090330
058300120223       IF priRpyFormato = 'TNSD4110O';
058400120220         RESET tnsd4110o;
058500120220         tnsd4110o.idSocieta = dsp_idSocieta;
058600120220         tnsd4110o.idFiliale = dsp_idFiliale;
058700120322         tnsd4110o.idArea = dsp_idArea;
058800120322         tnsd4110o.idRegione = dsp_idRegione;
058801171218         tnsd4110o.idDistrett = dsp_idDistrett;
058900120220         tnsd4110o.tpSocieta = dsp_tpSocieta;
059000120223         tnsd4110o.valDat = dsp_valDat;
059100120220         %SUBST(priRpyData : 1 : priRpyDataSize) = tnsd4110o;
059200090330       ENDIF;
059300090330
059400120320       RETURN TNSD41_RPYOPCODE_DONE;
059500090330
059600090330      /END-FREE
059700090330     P RunFiltro       E
059800090330
059900120223
060000120223     P*--------------------------------------------------
060100120223     P* Procedure name: RunRichiesta
060200120223     P* Purpose:        Elabora il tasto F4=Richiesta premuto su un campo.
060300120223     P* Returns:        Valore scelto.
060400120223     P* Parameter:      priRecord => Nome record.
060500120223     P* Parameter:      priField => Nome campo.
060600120223     P*--------------------------------------------------
060700120223     P RunRichiesta    B
060800120223     D RunRichiesta    PI            10A
060900120223     D  priRecord                    10A
061000120223     D  priField                     10A
061100120223     D  priValore                    10A   OPTIONS(*NOPASS) CONST
061200120223
061300120223     D retField        S             10A   STATIC
061400120223     D retDate         S               D   STATIC
061500120320     D rpyIdMsg        S             10I 0 STATIC
061600120320     D rpyOpCode       S             10A   STATIC
061700120223     D tnsd24ds        DS                  STATIC QUALIFIED INZ
061800120223     D  filialeId                     3A
061900120223     D  filialeTipo                   1A
062000120223     D  filialeDescrizione...
062100120223     D                               25A
062200120223     D  filialeDitta                  3A
062300120322     D key             S              8A   STATIC
062400120322     D des             S             25A   STATIC
062500120223
062600120223      /FREE
062700120223
062800120223       CLEAR retField;
062900120322       CLEAR key;
063000120322       CLEAR des;
063100120223
063200120223       SELECT;
063300120223
063400120223         WHEN priField = 'IDFILIALE';
063500120223
063600120223           RESET tnsd24ds;
063700120223           tnsd24r( tnsd24ds.filialeId
063800120223                  : tnsd24ds.filialeTipo
063900120223                  : tnsd24ds.filialeDescrizione
064000120223                  : tnsd24ds.filialeDitta
064100120223                  );
064200120223
064300120223           SELECT;
064400120223             WHEN tnsd24ds.filialeId <> *BLANK;
064500120223               retField = tnsd24ds.filialeId;
064600120223             WHEN %PARMS() > 2;
064700120223               retField = priValore;
064800120223             OTHER;
064900120223               retField = *ZERO;
065000120223           ENDSL;
065100120223
065200120223         WHEN priField = 'IDSOCIETA';
065300120223
065400120320           RESET tnsd4020i;
065500120320           RESET tnsd4020o;
065600120320           tnsd4020i.valDat = dsp_valDat;
065700120320           tnsd4020i.attDat = dsp_valDat;
065800120320
065900120320           Societa_Elenco( SOCIETA_OPCODE_RICERCA
066000120320                         : rpyOpCode
066100120320                         : rpyIdMsg
066200120320                         : 'TNSD4020I'
066300120320                         : tnsd4020i
066400120320                         : %SIZE(tnsd4020i)
066500120320                         : tnsd4020o.formato
066600120320                         : tnsd4020o
066700120320                         : %SIZE(tnsd4020o)
066800120320                         );
066900120223
067000120320           IF tnsd4020o.idSocieta <> *BLANK;
067100120320             retField = tnsd4020o.idSocieta;
067200120320           ELSE;
067300120320             retField = priValore;
067400120320           ENDIF;
067500120223
067600120223         WHEN priField = 'TPSOCIETA';
067700120223
067800120402           RESET tibs02ds;
067900120402           tibs02ds.t02mod = 'R';
068000120402           tibs02ds.t02cod = '009';
068100120402
068200120402           TNTBE_RicercaControllo(kpjba : tibs02ds);
068300120402
068400120402           SELECT;
068500120402             WHEN tibs02ds.t02ke1 <> *BLANK;
068600120402               retField = tibs02ds.t02ke1;
068700120402             OTHER;
068800120402               retField = priValore;
068900120402           ENDSL;
069000120402
069100120223         WHEN priField = 'VALDAT';
069200120223
069300120223           Calendario( %DATE(priValore) : retDate : esitoChar );
069400120223
069500120223           IF esitoChar = *OFF;
069600120223             retField = %CHAR(retDate);
069700120223           ELSE;
069800120223             retField = priValore;
069900120223           ENDIF;
070000120322
070100120322         WHEN priField = 'IDAREA';
070200120322
070300120322           Ricerca_tabel00f(1 : '05' : key : des);
070400120322
070500120322           IF key <> *BLANK;
070600120322             retField = %TRIML(key);
070700120322           ELSE;
070800120322             retField = priValore;
070900120322           ENDIF;
071000120322
071100120322         WHEN priField = 'IDREGIONE';
071200120223
071300120322           Ricerca_tabel00f(1 : '14' : key : des);
071400120322
071500120322           IF key <> *BLANK;
071600120322             retField = %TRIML(key);
071700120322           ELSE;
071800120322             retField = priValore;
071900120322           ENDIF;
071901171218
071902171218         WHEN priField = 'IDDISTRETT';
071903171218
071904171218           Ricerca_tabel00f(1 : '17' : key : des);
071905171218
071906171218           IF key <> *BLANK;
071907171218             retField = %TRIML(key);
071908171218           ELSE;
071909171218             retField = priValore;
071910171218           ENDIF;
072000120322
072100120223       ENDSL;
072200120223
072300120223       RETURN retField;
072400120223
072500120223      /END-FREE
072600120223     P RunRichiesta    E
072700120223
