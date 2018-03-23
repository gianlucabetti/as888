000100090327     ***************************************************************************
000200090327     **
000300090327     ** Anagrafico società.
000400090330     ** Gestione finestra filtro.
000500090327     **
000600090327     ***************************************************************************
000700090420     H DFTACTGRP(*NO) ACTGRP('AZSOC') BNDDIR('PJXBND')
000800090330
000900090330     ***************************************************************************
001000090330     **
001100090330     ** Dichiarazione files.
001200090330     **
001300090330     ***************************************************************************
001400090330     Ftnsd4010d CF   E             WORKSTN
001500090409     F                                     USROPN
001600090330     F                                     PREFIX(dsp_)
001700090330     F                                     INDDS(dspInd)
001800090330     F                                     INFDS(dspInf)
001900090330
002000090330     ***************************************************************************
002100090330     **
002200090330     ** Definizione costanti.
002300090330     **
002400090330     ***************************************************************************
002500090330     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002600090331     D ESITO_ERROR...
002700090330     D                 C                   -1
002800090330     D ESITO_OK...
002900090330     D                 C                   0
003000090330     D ESITO_WARNING...
003100090330     D                 C                   1
003200090331     D ESITO_WARNING_ENTER...
003300090331     D                 C                   241
003400090331     D ESITO_WARNING_F3...
003500090331     D                 C                   51
003600090331     D ESITO_WARNING_F5...
003700090331     D                 C                   53
003800090331     D ESITO_WARNING_F12...
003900090331     D                 C                   60
004000090330     D SOCIETA_OPCODE_FILTRO_CHK...
004100090330     D                 C                   'CHKFILTRO'
004200090330     D SOCIETA_OPCODE_FILTRO_GET...
004300090330     D                 C                   'GETFILTRO'
004400090330     D SOCIETA_OPCODE_FILTRO_INZ...
004500090330     D                 C                   'INZFILTRO'
004600090330     D SOCIETA_OPCODE_FILTRO_PUT...
004700090330     D                 C                   'PUTFILTRO'
004800090330     D SOCIETA_OPCODE_FILTRO_RUN...
004900090330     D                 C                   'RUNFILTRO'
005000090330     D SOCIETA_OPCODE_FILTRO_SET...
005100090330     D                 C                   'SETFILTRO'
005200090330
005300090330     ***************************************************************************
005400090330     **
005500090330     ** Dichiarazione prototipi.
005600090330     **
005700090330     ***************************************************************************
005800090330     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
005900090420     D/COPY GAITRASRC/SRCPROTOPR,XSOC
006000090330
006100090330     ***************************************************************************
006200090330     **
006300090330     ** Dichiarazione strutture dati.
006400090330     **
006500090330     ***************************************************************************
006600090330     D dspInd...
006700090330     D                 DS
006800090330     D                                     QUALIFIED
006900090331     D                                     INZ
007000090331     D erroreDataValidita...
007100090331     D                         1      1N
007200090331     D erroreDataAttivita...
007300090331     D                         2      2N
007400090331     D erroreGenerico...
007500090331     D                        99     99N
007600090331     D dspInf...
007700090330     D                 DS
007800090330     D                                     QUALIFIED
007900090331     D  dsp_aid              369    369A
008000090331     D  dsp_aid_num          369    369U 0
008100090331     D  cursorRow            370    370I 0
008200090331     D  cursorCol            371    371I 0
008300090331     D  actCsrRow            382    382I 0
008400090331     D  actCsrCol            383    383I 0
008500090330     D kpjba...
008600090330     D               E DS
008700090330     D                                     QUALIFIED
008800090402     D                                     INZ
008900090330     D  knmus...
009000090330     D               E                     INZ(*USER)
009100090330     D tnsd4010i...
009200090330     D               E DS
009300090330     D                                     QUALIFIED
009400090331     D                                     INZ
009500090330     D tnsd4010o...
009600090330     D               E DS
009700090330     D                                     QUALIFIED
009800090331     D                                     INZ
009900090420     D xSoc001Ds...
010000090420     D               E DS
010100090420     D                                     QUALIFIED
010200090420     D                                     INZ
010300090420
010400090330     ***************************************************************************
010500090330     **
010600090330     ** Dichiarazione variabili.
010700090330     **
010800090331     ***************************************************************************
010900090331     D esito...
011000090331     D                 S             10I 0
011100090420     D esitoProj...
011200090420     D                 S              1A
011300090331     D opCodeCur...
011400090330     D                 S             10A
011500090330     D opCodeNxt...
011600090330     D                 S                   LIKE(opCodeCur)
011700090330     D                                     INZ(SOCIETA_OPCODE_FILTRO_INZ)
011800090330     D opCodePrv...
011900090330     D                 S                   LIKE(opCodeCur)
012000090330
012100090330     ***************************************************************************
012200090330     **
012300090330     ** Dichiarazione interfaccia procedura principale.
012400090330     **
012500090330     ***************************************************************************
012600090330     D Societa_Filtro...
012700090330     D                 PI
012800090330     D  piRqsOpCode...
012900090330     D                               10A
013000090330     D                                     CONST
013100090330     D  piRpyOpCode...
013200090330     D                               10A
013300090330     D  piRpyIdMsg...
013400090330     D                               10I 0
013500090330     D  piRqsFormato...
013600090330     D                               10A
013700090330     D                                     CONST
013800090330     D                                     OPTIONS(*NOPASS)
013900090330     D  piRqsData...
014000090330     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
014100090330     D                                     CONST
014200090330     D  piRqsDataSize...
014300090330     D                               10I 0
014400090330     D                                     CONST
014500090330     D                                     OPTIONS(*NOPASS)
014600090330     D  piRpyFormato...
014700090330     D                               10A
014800090330     D                                     CONST
014900090330     D                                     OPTIONS(*NOPASS)
015000090330     D  piRpyData...
015100090330     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
015200090330     D  piRpyDataSize...
015300090330     D                               10I 0
015400090330     D                                     CONST
015500090330     D                                     OPTIONS(*NOPASS)
015600090330
015700090330     D*--------------------------------------------------
015800090330     D* Procedure name: InzFiltro
015900090330     D* Purpose:        Inizializza finestra filtro.
016000090330     D* Returns:        Esito.
016100090330     D*--------------------------------------------------
016200090330     D InzFiltro       PR            10I 0
016300090330
016400090330     D*--------------------------------------------------
016500090330     D* Procedure name: SetFiltro
016600090330     D* Purpose:        Imposta dati finestra filtro.
016700090330     D* Returns:        Esito.
016800090330     D*--------------------------------------------------
016900090330     D SetFiltro       PR            10I 0
017000090330
017100090330     D*--------------------------------------------------
017200090330     D* Procedure name: PutFiltro
017300090330     D* Purpose:        Emette finestra filtro.
017400090330     D* Returns:        Esito.
017500090330     D*--------------------------------------------------
017600090330     D PutFiltro       PR            10I 0
017700090330
017800090330     D*--------------------------------------------------
017900090330     D* Procedure name: GetFiltro
018000090330     D* Purpose:        Legge la finestra filtro.
018100090330     D* Returns:        Esito.
018200090330     D*--------------------------------------------------
018300090330     D GetFiltro       PR            10I 0
018400090330
018500090330     D*--------------------------------------------------
018600090330     D* Procedure name: ChkFiltro
018700090330     D* Purpose:        Controlla i dati inseriti nella finestra filtro.
018800090330     D* Returns:        Esito.
018900090330     D*--------------------------------------------------
019000090330     D ChkFiltro       PR            10I 0
019100090330
019200090330     D*--------------------------------------------------
019300090330     D* Procedure name: RunFiltro
019400090330     D* Purpose:        Elabora i dati inseriti nella finestra filtro.
019500090330     D* Returns:        Esito.
019600090330     D*--------------------------------------------------
019700090330     D RunFiltro       PR            10I 0
019800090330
019900090330
020000090330      /FREE
020100090420
020200090330       IF piRqsOpCode = SOCIETA_OPCODE_FILTRO;
020300090330
020400090408         RESET opCodeNxt;
020500090409         OPEN tnsd4010d;
020600090408
020700090330         DOU opCodeCur = SOCIETA_OPCODE_FINE;
020800090330
020900090330           opCodeCur = opCodeNxt;
021000090330
021100090330           IF opCodeCur = SOCIETA_OPCODE_FILTRO_INZ;
021200090330             InzFiltro();
021300090330             opCodeNxt = SOCIETA_OPCODE_FILTRO_SET;
021400090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FILTRO_SET;
021500090330             SetFiltro();
021600090330             opCodeNxt = SOCIETA_OPCODE_FILTRO_PUT;
021700090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FILTRO_PUT;
021800090330             PutFiltro();
021900090330             opCodeNxt = SOCIETA_OPCODE_FILTRO_GET;
022000090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FILTRO_GET;
022100090331             esito = GetFiltro();
022200090331             IF esito = ESITO_WARNING_ENTER;
022300090331               opCodeNxt = SOCIETA_OPCODE_FILTRO_RUN;
022400090331             ELSEIF esito = ESITO_WARNING_F5;
022500090331               opCodeNxt = SOCIETA_OPCODE_FILTRO_INZ;
022600090331             ELSEIF esito = ESITO_WARNING_F12;
022700090331               opCodeNxt = SOCIETA_OPCODE_RITORNO;
022800090331             ELSE;
022900090331               opCodeNxt = SOCIETA_OPCODE_FINE;
023000090331             ENDIF;
023100090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FILTRO_CHK;
023200090331             IF ChkFiltro() = ESITO_OK;
023300090331               opCodeNxt = SOCIETA_OPCODE_FILTRO_RUN;
023400090331             ELSE;
023500090331               opCodeNxt = SOCIETA_OPCODE_FILTRO_SET;
023600090331             ENDIF;
023700090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FILTRO_RUN;
023800090331             IF ChkFiltro() = ESITO_OK;
023900090331               RunFiltro();
024000090408               opCodeNxt = SOCIETA_OPCODE_DONE;
024100090331             ELSE;
024200090331               opCodeNxt = SOCIETA_OPCODE_FILTRO_SET;
024300090331             ENDIF;
024400090408           ELSEIF opCodeCur = SOCIETA_OPCODE_DONE;
024500090408             piRpyOpCode = opCodeCur;
024600090408             LEAVE;
024700090408           ELSEIF opCodeCur = SOCIETA_OPCODE_RITORNO;
024800090408             piRpyOpCode = opCodeCur;
024900090408             LEAVE;
025000090330           ELSEIF opCodeCur = SOCIETA_OPCODE_FINE;
025100090408             piRpyOpCode = opCodeCur;
025200090330             LEAVE;
025300090330           ELSE;
025400090330             opCodeNxt = SOCIETA_OPCODE_FINE;
025500090330           ENDIF;
025600090330
025700090330         ENDDO;
025800090330
025900090409         CLOSE tnsd4010d;
026000090409
026100090330       ELSEIF piRqsOpCode = SOCIETA_OPCODE_INIT;
026200090330
026300090330         IF piRqsFormato = 'KPJBA';
026400090330           kpjba = piRqsData;
026500090330         ENDIF;
026600090420
026700090420         xSoc001Ds.xscSoc = kpjba.kpjbu;
026800090420
026900090420         Proj_Societa( 'SOC001'
027000090420                     : xSoc001Ds.xscSoc
027100090420                     : *ZERO
027200090420                     : *BLANK
027300090420                     : esitoProj
027400090420                     : xSoc001Ds
027500090420                     : kpjba
027600090420                     );
027700090420
027800090420         IF esitoProj = *ON;
027900090420           RETURN;
028000090420         ENDIF;
028100090330
028200090330       ELSEIF piRqsOpCode = SOCIETA_OPCODE_FINALIZE;
028300090330
028400090330         *INLR = *ON;
028500090330
028600090330       ENDIF;
028700090330
028800090330       RETURN;
028900090330
029000090330      /END-FREE
029100090330
029200090330
029300090330     P*--------------------------------------------------
029400090330     P* Procedure name: InzFiltro
029500090330     P* Purpose:        Inizializza finestra filtro.
029600090330     P* Returns:        Esito.
029700090330     P*--------------------------------------------------
029800090330     P InzFiltro       B
029900090330     D InzFiltro       PI            10I 0
030000090330
030100090330      /FREE
030200090330
030300090330       RESET filtro;
030400090331       RESET dspInd;
030500090330
030600090330       IF piRqsFormato = 'TNSD4010I';
030700090330         tnsd4010i = %SUBST(piRqsData : 1 : piRqsDataSize);
030800090403         dsp_idSocieta = tnsd4010i.idSocieta;
030900090403         dsp_codFiscale = tnsd4010i.codFiscale;
031000090330         dsp_ragSociale = tnsd4010i.ragSociale;
031100090330         IF tnsd4010i.valDat > *LOVAL;
031200090330           dsp_valDat = %DEC(tnsd4010i.valDat : *JOBRUN);
031300090330         ENDIF;
031400090330         IF tnsd4010i.attDat > *LOVAL;
031500090330           dsp_attDat = %DEC(tnsd4010i.attDat : *JOBRUN);
031600090330         ENDIF;
031700090330       ENDIF;
031800090330
031900090330       RETURN ESITO_OK;
032000090330
032100090330      /END-FREE
032200090330     P InzFiltro       E
032300090330
032400090330
032500090330     P*--------------------------------------------------
032600090330     P* Procedure name: SetFiltro
032700090330     P* Purpose:        Imposta dati finestra filtro.
032800090330     P* Returns:        Esito.
032900090330     P*--------------------------------------------------
033000090330     P SetFiltro       B
033100090330     D SetFiltro       PI            10I 0
033200090330
033300090330      /FREE
033400090330
033500090330       RETURN ESITO_OK;
033600090330
033700090330      /END-FREE
033800090330     P SetFiltro       E
033900090330
034000090330
034100090330     P*--------------------------------------------------
034200090330     P* Procedure name: PutFiltro
034300090330     P* Purpose:        Emette finestra filtro.
034400090330     P* Returns:        Esito.
034500090330     P*--------------------------------------------------
034600090330     P PutFiltro       B
034700090330     D PutFiltro       PI            10I 0
034800090330
034900090330      /FREE
035000090330
035100090330       WRITE filtro;
035200090330
035300090330       RETURN ESITO_OK;
035400090330
035500090330      /END-FREE
035600090330     P PutFiltro       E
035700090330
035800090330
035900090330     P*--------------------------------------------------
036000090330     P* Procedure name: GetFiltro
036100090330     P* Purpose:        Legge la finestra filtro.
036200090330     P* Returns:        Esito.
036300090330     P*--------------------------------------------------
036400090330     P GetFiltro       B
036500090330     D GetFiltro       PI            10I 0
036600090330
036700090330      /FREE
036800090330
036900090330       READ filtro;
037000090330
037100090331       RETURN dspInf.dsp_aid_num;
037200090330
037300090330      /END-FREE
037400090330     P GetFiltro       E
037500090330
037600090330
037700090330     P*--------------------------------------------------
037800090330     P* Procedure name: ChkFiltro
037900090330     P* Purpose:        Controlla i dati inseriti nella finestra filtro.
038000090330     P* Returns:        Esito.
038100090330     P*--------------------------------------------------
038200090330     P ChkFiltro       B
038300090330     D ChkFiltro       PI            10I 0
038400090330
038500090330      /FREE
038600090330
038700090331       RESET dspInd.erroreGenerico;
038800090331       RESET dspInd.erroreDataValidita;
038900090331       RESET dspInd.erroreDataAttivita;
039000090331
039100090331       // Controllo formale data validità anagrafica.
039200090331
039300090331       IF dsp_valDat > *ZERO;
039400090331         TEST(DE) *JOBRUN dsp_valDat;
039500090331         IF %ERROR();
039600090331           dspInd.erroreGenerico = *ON;
039700090331           dspInd.erroreDataValidita = *ON;
039800090331         ENDIF;
039900090331       ENDIF;
040000090331
040100090331       // Controllo formale data attività società.
040200090331
040300090331       IF dsp_attDat > *ZERO;
040400090331         TEST(DE) *JOBRUN dsp_attDat;
040500090331         IF %ERROR();
040600090331           dspInd.erroreGenerico = *ON;
040700090331           dspInd.erroreDataAttivita = *ON;
040800090331         ENDIF;
040900090331       ENDIF;
041000090331
041100090331       IF dspInd.erroreGenerico;
041200090331         RETURN ESITO_ERROR;
041300090331       ENDIF;
041400090331
041500090330       RETURN ESITO_OK;
041600090330
041700090330      /END-FREE
041800090330     P ChkFiltro       E
041900090330
042000090330
042100090330     P*--------------------------------------------------
042200090330     P* Procedure name: RunFiltro
042300090330     P* Purpose:        Elabora i dati inseriti nella finestra filtro.
042400090330     P* Returns:        Esito.
042500090330     P*--------------------------------------------------
042600090330     P RunFiltro       B
042700090330     D RunFiltro       PI            10I 0
042800090330
042900090330      /FREE
043000090330
043100090331       IF piRpyFormato = 'TNSD4010O';
043200090331         RESET tnsd4010o;
043300090403         tnsd4010o.idSocieta = dsp_idSocieta;
043400090403         tnsd4010o.codFiscale = dsp_codFiscale;
043500090330         tnsd4010o.ragSociale = %TRIMR(dsp_ragSociale);
043600090330         IF dsp_valDat > *ZERO;
043700090330           tnsd4010o.valDat = %DATE(dsp_valDat : *JOBRUN);
043800090330         ENDIF;
043900090330         IF dsp_attDat > *ZERO;
044000090330           tnsd4010o.attDat = %DATE(dsp_attDat : *JOBRUN);
044100090330         ENDIF;
044200090330         %SUBST(piRpyData : 1 : piRpyDataSize) = tnsd4010o;
044300090330       ENDIF;
044400090330
044500090330       RETURN ESITO_OK;
044600090330
044700090330      /END-FREE
044800090330     P RunFiltro       E
044900090330
