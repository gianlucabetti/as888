000100090403     ***************************************************************************
000200090403     **
000300090403     ** Anagrafico società.
000400090403     ** Elenco.
000500090403     **
000600090403     ***************************************************************************
000700090420     H DFTACTGRP(*NO) ACTGRP('AZSOC') BNDDIR('PJXBND')
000800090403
000900090403     ***************************************************************************
001000090403     **
001100090403     ** Dichiarazione files.
001200090403     **
001300090403     ***************************************************************************
001400090403     Ftnsd4020d CF   E             WORKSTN
001500090403     F                                     PREFIX(dsp_)
001600090403     F                                     INDDS(dspInd)
001700090403     F                                     INFDS(dspInf)
001800090403     F                                     SFILE(societaS:societaRrn)
001900090403
002000090403     ***************************************************************************
002100090403     **
002200090403     ** Definizione costanti.
002300090403     **
002400090403     ***************************************************************************
002500090403     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002600090403     D ESITO_ERROR...
002700090403     D                 C                   -1
002800090403     D ESITO_OK...
002900090403     D                 C                   0
003000090403     D ESITO_WARNING...
003100090403     D                 C                   1
003200090403     D ESITO_WARNING_ENTER...
003300090403     D                 C                   241
003400090403     D ESITO_WARNING_F3...
003500090403     D                 C                   51
003600090403     D ESITO_WARNING_F5...
003700090403     D                 C                   53
003800090409     D ESITO_WARNING_F10...
003900090409     D                 C                   58
004000090403     D ESITO_WARNING_F12...
004100090403     D                 C                   60
004200090420     D ESITO_WARNING_F13...
004300090420     D                 C                   177
004400120320     D OPZIONE_SCELTA...
004500120320     D                 C                   '1'
004600090409     D OPZIONE_MODIFICARE...
004700090409     D                 C                   '2'
004800090409     D OPZIONE_NUOVAANAGRAFICA...
004900090409     D                 C                   '3'
005000090414     D OPZIONE_FINEATTIVITA...
005100090409     D                 C                   '4'
005200090409     D OPZIONE_VISUALIZZARE...
005300090409     D                 C                   '5'
005400090409     D OPZIONE_SOCIETAPROJ...
005500090409     D                 C                   '9'
005600090409     D OPZIONE_CANCELLAREANAGRAFICA...
005700090409     D                 C                   '44'
005800090409     D PROJ_F3...
005900090409     D                 C                   '1'
006000090409     D PROJ_F8...
006100090409     D                 C                   '0'
006200090409     D PROJ_F12...
006300090409     D                 C                   '2'
006400090409     D PROJ_OPZIONE_MODIFICARE...
006500090409     D                 C                   '02'
006600090403     D SOCIETA_OPCODE_ELENCO_CHK...
006700090403     D                 C                   'CHKELENCO'
006800090403     D SOCIETA_OPCODE_ELENCO_GET...
006900090403     D                 C                   'GETELENCO'
007000090403     D SOCIETA_OPCODE_ELENCO_INZ...
007100090403     D                 C                   'INZELENCO'
007200090403     D SOCIETA_OPCODE_ELENCO_PUT...
007300090403     D                 C                   'PUTELENCO'
007400090403     D SOCIETA_OPCODE_ELENCO_RUN...
007500090403     D                 C                   'RUNELENCO'
007600090403     D SOCIETA_OPCODE_ELENCO_SET...
007700090403     D                 C                   'SETELENCO'
007800090403
007900090403     ***************************************************************************
008000090403     **
008100090403     ** Dichiarazione prototipi.
008200090403     **
008300090403     ***************************************************************************
008400090409     D/COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
008500090409     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
008600090420     D/COPY GAITRASRC/SRCPROTOPR,XSOC
008700090403
008800090403     ***************************************************************************
008900090403     **
009000090403     ** Dichiarazione strutture dati.
009100090403     **
009200090403     ***************************************************************************
009300090409     D ana008Ds...
009400090409     D               E DS
009500090409     D                                     QUALIFIED
009600090409     D                                     INZ
009700090409     D  opz008...
009800090409     D               E                     INZ(PROJ_OPZIONE_MODIFICARE)
009900090403     D dspInd...
010000090403     D                 DS
010100090403     D                                     QUALIFIED
010200090403     D                                     INZ
010300090403     D subfileClear...
010400090403     D                         1      1N
010500090403     D subfileDisplay...
010600090403     D                         2      2N
010700090420     D subfileNextChange...
010800090420     D                         3      3N
010900120320     D ricerca...
011000120320     D                         4      4N
011100090403     D erroreGenerico...
011200090403     D                        99     99N
011300090403     D dspInf...
011400090403     D                 DS
011500090403     D                                     QUALIFIED
011600090403     D  dsp_aid              369    369A
011700090403     D  dsp_aid_num          369    369U 0
011800090403     D  cursorRow            370    370I 0
011900090403     D  cursorCol            371    371I 0
012000090403     D  actCsrRow            382    382I 0
012100090403     D  actCsrCol            383    383I 0
012200090403     D kpjba...
012300090403     D               E DS
012400090403     D                                     QUALIFIED
012500090403     D                                     INZ
012600090403     D  knmus...
012700090403     D               E                     INZ(*USER)
012800090403     D tnsd4020i...
012900090403     D               E DS
013000090403     D                                     QUALIFIED
013100090403     D                                     INZ
013200120320     D tnsd4020o...
013300120320     D               E DS
013400120320     D                                     QUALIFIED
013500120320     D                                     INZ(*EXTDFT)
013600090409     D tnsd4030i...
013700090409     D               E DS
013800090409     D                                     QUALIFIED
013900090409     D                                     INZ
014000090403     D tnsd4030o...
014100090403     D               E DS
014200090403     D                                     QUALIFIED
014300090403     D                                     INZ
014400090409     D tnsd4040i...
014500090409     D               E DS
014600090409     D                                     QUALIFIED
014700090409     D                                     INZ
014800090409     D tnsd4040o...
014900090409     D               E DS
015000090409     D                                     QUALIFIED
015100090409     D                                     INZ
015200090420     D xSoc001Ds...
015300090420     D               E DS
015400090420     D                                     QUALIFIED
015500090420     D                                     INZ
015600090420
015700090403     ***************************************************************************
015800090403     **
015900090403     ** Dichiarazione variabili.
016000090403     **
016100090403     ***************************************************************************
016200090403     D esito...
016300090403     D                 S             10I 0
016400090420     D esitoProj...
016500090420     D                 S              1A
016600090403     D opCodeCur...
016700090403     D                 S             10A
016800090403     D opCodeNxt...
016900090403     D                 S                   LIKE(opCodeCur)
017000090403     D                                     INZ(SOCIETA_OPCODE_ELENCO_INZ)
017100090403     D opCodePrv...
017200090403     D                 S                   LIKE(opCodeCur)
017300090403     D rpyOpCode...
017400090403     D                 S             10A
017500090403     D rpyIdMsg...
017600090403     D                 S             10I 0
017700090403     D societaRrn...
017800090403     D                 S              5U 0
017900090403
018000090403     ***************************************************************************
018100090403     **
018200090403     ** Dichiarazione interfaccia procedura principale.
018300090403     **
018400090403     ***************************************************************************
018500090403     D Societa_Elenco...
018600090403     D                 PI
018700090403     D  piRqsOpCode...
018800090403     D                               10A
018900090403     D                                     CONST
019000090403     D  piRpyOpCode...
019100090403     D                               10A
019200090403     D  piRpyIdMsg...
019300090403     D                               10I 0
019400090403     D  piRqsFormato...
019500090403     D                               10A
019600090403     D                                     CONST
019700090403     D                                     OPTIONS(*NOPASS)
019800090403     D  piRqsData...
019900090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
020000090403     D                                     CONST
020100090403     D  piRqsDataSize...
020200090403     D                               10I 0
020300090403     D                                     CONST
020400090403     D                                     OPTIONS(*NOPASS)
020500090403     D  piRpyFormato...
020600090403     D                               10A
020700090403     D                                     CONST
020800090403     D                                     OPTIONS(*NOPASS)
020900090403     D  piRpyData...
021000090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
021100090403     D  piRpyDataSize...
021200090403     D                               10I 0
021300090403     D                                     CONST
021400090403     D                                     OPTIONS(*NOPASS)
021500090403
021600090403     D*--------------------------------------------------
021700090403     D* Procedure name: InzElenco
021800090403     D* Purpose:        Inizializza finestra elenco.
021900090403     D* Returns:        Esito.
022000090403     D*--------------------------------------------------
022100090403     D InzElenco       PR            10I 0
022200090403
022300090403     D*--------------------------------------------------
022400090403     D* Procedure name: SetElenco
022500090403     D* Purpose:        Imposta dati finestra elenco.
022600090403     D* Returns:        Esito.
022700090403     D*--------------------------------------------------
022800090403     D SetElenco       PR            10I 0
022900090403
023000090403     D*--------------------------------------------------
023100090403     D* Procedure name: PutElenco
023200090403     D* Purpose:        Emette finestra elenco.
023300090403     D* Returns:        Esito.
023400090403     D*--------------------------------------------------
023500090403     D PutElenco       PR            10I 0
023600090403
023700090403     D*--------------------------------------------------
023800090403     D* Procedure name: GetElenco
023900090403     D* Purpose:        Legge la finestra elenco.
024000090403     D* Returns:        Esito.
024100090403     D*--------------------------------------------------
024200090403     D GetElenco       PR            10I 0
024300090403
024400090403     D*--------------------------------------------------
024500090403     D* Procedure name: ChkElenco
024600090403     D* Purpose:        Controlla i dati inseriti nella finestra elenco.
024700090403     D* Returns:        Esito.
024800090403     D*--------------------------------------------------
024900090403     D ChkElenco       PR            10I 0
025000090403
025100090403     D*--------------------------------------------------
025200090403     D* Procedure name: RunElenco
025300090403     D* Purpose:        Elabora i dati inseriti nella finestra elenco.
025400090403     D* Returns:        Esito.
025500090403     D*--------------------------------------------------
025600090403     D RunElenco       PR            10I 0
025700090403
025800090403
025900090403      /FREE
026000120320
026100120320       IF piRqsOpCode = SOCIETA_OPCODE_INIT;
026200120320
026300120320         IF piRqsFormato = 'KPJBA';
026400120320           kpjba = piRqsData;
026500120320         ENDIF;
026600120320
026700120320         xSoc001Ds.xscSoc = kpjba.kpjbu;
026800120320
026900120320         Proj_Societa( 'SOC001'
027000120320                     : xSoc001Ds.xscSoc
027100120320                     : *ZERO
027200120320                     : *BLANK
027300120320                     : esitoProj
027400120320                     : xSoc001Ds
027500120320                     : kpjba
027600120320                     );
027700120320
027800120320         IF esitoProj = *ON;
027900120320           piRpyOpCode = SOCIETA_OPCODE_ERRORE;
028000120320           RETURN;
028100120320         ENDIF;
028200120320
028300120320         kpjba.kpjbu = xSoc001Ds.xscSoc;
028400120320
028500120320         Societa_Selezione( SOCIETA_OPCODE_INIT
028600120320                          : rpyOpCode
028700120320                          : rpyIdMsg
028800120320                          : 'KPJBA'
028900120320                          : kpjba
029000120320                          : %SIZE(kpjba)
029100120320                          );
029200120320
029300120320         kpjba.kpjbu = xSoc001Ds.xscSoc;
029400120320
029500120320         Societa_Manutenzione( SOCIETA_OPCODE_INIT
029600120320                             : rpyOpCode
029700120320                             : rpyIdMsg
029800120320                             : 'KPJBA'
029900120320                             : kpjba
030000120320                             : %SIZE(kpjba)
030100120320                             );
030200120320
030300120320         RETURN;
030400120320
030500120320       ELSEIF piRqsOpCode = SOCIETA_OPCODE_FINALIZE;
030600120320
030700120320         Societa_Selezione( SOCIETA_OPCODE_FINALIZE
030800120320                          : rpyOpCode
030900120320                          : rpyIdMsg
031000120320                          );
031100120320
031200120320         Societa_Manutenzione( SOCIETA_OPCODE_FINALIZE
031300120320                             : rpyOpCode
031400120320                             : rpyIdMsg
031500120320                             );
031600120320
031700120320         *INLR = *ON;
031800120320         RETURN;
031900120320
032000120320       ENDIF;
032100120320
032200120320       IF %PARMS() > 6 AND piRpyFormato = tnsd4020o.formato;
032300120320         RESET tnsd4020o;
032400120320       ENDIF;
032500090403
032600120320       RESET opCodeNxt;
032700090408
032800120320       DOU opCodeCur = SOCIETA_OPCODE_FINE;
032900090403
033000120320         opCodeCur = opCodeNxt;
033100090403
033200120320         IF opCodeCur = SOCIETA_OPCODE_ELENCO_INZ;
033300120320           InzElenco();
033400120320           opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
033500120320         ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_SET;
033600120320           SetElenco();
033700120320           opCodeNxt = SOCIETA_OPCODE_ELENCO_PUT;
033800120320         ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_PUT;
033900120320           PutElenco();
034000120320           opCodeNxt = SOCIETA_OPCODE_ELENCO_GET;
034100120320         ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_GET;
034200120320           esito = GetElenco();
034300120320           IF esito = ESITO_WARNING_ENTER;
034400120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_RUN;
034500120320           ELSEIF esito = ESITO_WARNING_F5;
034600120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_INZ;
034700120320           ELSEIF esito = ESITO_WARNING_F10;
034800120320             RESET tnsd4040i;
034900120320             tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVASOCIETA;
035000120320             Societa_Manutenzione( SOCIETA_OPCODE_ANAGRAFICA
035100120320                                 : rpyOpCode
035200120320                                 : rpyIdMsg
035300120320                                 : 'TNSD4040I'
035400120320                                 : tnsd4040i
035500120320                                 : %SIZE(tnsd4040i)
035600120320                                 : 'TNSD4040O'
035700120320                                 : tnsd4040o
035800120320                                 : %SIZE(tnsd4040o)
035900120320                                 );
036000120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_INZ;
036100120320           ELSEIF esito = ESITO_WARNING_F12;
036200120320             opCodeNxt = SOCIETA_OPCODE_RITORNO;
036300120320           ELSEIF esito = ESITO_WARNING_F13;
036400120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
036500120320           ELSE;
036600120320             opCodeNxt = SOCIETA_OPCODE_FINE;
036700120320           ENDIF;
036800120320         ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_CHK;
036900120320           IF ChkElenco() = ESITO_OK;
037000120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_RUN;
037100120320           ELSE;
037200120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
037300120320           ENDIF;
037400120320         ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_RUN;
037500120320           IF ChkElenco() = ESITO_OK;
037600120320             esito = RunElenco();
037700120320             IF esito = ESITO_WARNING_F3;
037800120320               opCodeNxt = SOCIETA_OPCODE_FINE;
037900120320             ELSEIF esito = ESITO_WARNING_F12;
038000120320               opCodeNxt = SOCIETA_OPCODE_ELENCO_PUT;
038100120320             ELSE;
038200120320               opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
038300120320             ENDIF;
038400120320           ELSE;
038500120320             opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
038600120320           ENDIF;
038700120320         ELSEIF opCodeCur = SOCIETA_OPCODE_RITORNO;
038800120320           piRpyOpCode = opCodeCur;
038900120320           LEAVE;
039000120320         ELSEIF opCodeCur = SOCIETA_OPCODE_FINE;
039100120320           piRpyOpCode = opCodeCur;
039200120320           LEAVE;
039300120320         ELSE;
039400120320           opCodeNxt = SOCIETA_OPCODE_FINE;
039500120320         ENDIF;
039600090403
039700120320       ENDDO;
039800090403
039900120320       IF %PARMS() > 6 AND piRpyFormato = tnsd4020o.formato;
040000120320         %SUBST(piRpyData : 1 : piRpyDataSize) = tnsd4020o;
040100120320       ENDIF;
040200120320
040300090403       RETURN;
040400090403
040500090420       //***********************************************************************
040600090420       //
040700090420       //
040800090420       //
040900090420       //***********************************************************************
041000090420
041100090420       BEGSR *INZSR;
041200090420
041300090420         dsp_sflRcdNbr = 1;
041400090420
041500090420       ENDSR;
041600090420
041700090403      /END-FREE
041800090403
041900090403
042000090403     P*--------------------------------------------------
042100090403     P* Procedure name: InzElenco
042200090403     P* Purpose:        Inizializza finestra elenco.
042300090403     P* Returns:        Esito.
042400090403     P*--------------------------------------------------
042500090403     P InzElenco       B
042600090403     D InzElenco       PI            10I 0
042700090403
042800090403      /FREE
042900090403
043000090403       RESET dspInd;
043100090403       dspInd.subfileClear = *ON;
043200090403       dspInd.subfileDisplay = *OFF;
043300090403       WRITE societaC;
043400090403       dspInd.subfileClear = *OFF;
043500090403       RESET societaRrn;
043600090420       RESET dsp_sflRcdNbr;
043700090420
043800090403       IF piRqsFormato = 'TNSD4020I';
043900090403         tnsd4020i = %SUBST(piRqsData : 1 : piRqsDataSize);
044000090403       ENDIF;
044100090403
044200090403       EVAL-CORR tnsd4030i = tnsd4020i;
044300090403
044400090403       Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_NEW
044500090403                        : rpyOpCode
044600090403                        : rpyIdMsg
044700090403                        : 'TNSD4030I'
044800090403                        : tnsd4030i
044900090403                        : %SIZE(tnsd4030i)
045000090403                        );
045100090403
045200090403       IF rpyOpCode <> SOCIETA_OPCODE_DONE;
045300090403         RETURN ESITO_ERROR;
045400090403       ENDIF;
045500090403
045600090409       CLEAR societaS;
045700090409
045800090403       DOU rpyIdMsg = 100;
045900090403
046000090403         Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_GETNEXTROW
046100090403                          : rpyOpCode
046200090403                          : rpyIdMsg
046300090403                          : *OMIT
046400090403                          : *OMIT
046500090403                          : *OMIT
046600090403                          : 'TNSD4030O'
046700090403                          : tnsd4030o
046800090403                          : %SIZE(tnsd4030o)
046900090403                          );
047000090403
047100090403         IF rpyIdMsg < ESITO_OK OR rpyIdMsg = 100;
047200090403           LEAVE;
047300090403         ENDIF;
047400090403
047500090403         IF rpyOpCode = SOCIETA_OPCODE_DONE;
047600090403           EXEC SQL
047700090403             SELECT AZSOC00F.ID_SOCIETA
047800090403                  , AZSOC00F.RAGIONE_SOCIALE_BREVE
047900090403                  , AZSOC00F.VALIDITA_DATA_INIZIO
048000090403                  , AZSOC00F.VALIDITA_DATA_FINE
048100090409                  , IFNULL(CHAR(AZSOC00F.ATTIVITA_DATA_FINE , EUR) , '')
048200090403                  , AZSOC00F.CODICE_FISCALE
048300090403               INTO :dsp_idSocieta
048400090403                  , :dsp_ragSocBrev
048500090403                  , :dsp_valDatIniz
048600090403                  , :dsp_valDatFine
048700090403                  , :dsp_attDatFine
048800090403                  , :dsp_codFiscale
048900090403               FROM AZSOC00F
049000090403               WHERE AZSOC00F.ID_RIGA = :tnsd4030o.idRiga
049100090403           ;
049200090409           dsp_idRiga = tnsd4030o.idRiga;
049300090409           societaRrn += 1;
049400090403           WRITE societaS;
049500090403         ENDIF;
049600090403
049700090403       ENDDO;
049800090403
049900090403       Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_END
050000090403                        : rpyOpCode
050100090403                        : rpyIdMsg
050200090403                        );
050300090403
050400090403       dspInd.subfileDisplay = (societaRrn > *ZERO);
050500090403
050600090403       RETURN ESITO_OK;
050700090403
050800090403      /END-FREE
050900090403     P InzElenco       E
051000090403
051100090403
051200090403     P*--------------------------------------------------
051300090403     P* Procedure name: SetElenco
051400090403     P* Purpose:        Imposta dati finestra elenco.
051500090403     P* Returns:        Esito.
051600090403     P*--------------------------------------------------
051700090403     P SetElenco       B
051800090403     D SetElenco       PI            10I 0
051900090403
052000090403      /FREE
052100090403
052200090403       RETURN ESITO_OK;
052300090403
052400090403      /END-FREE
052500090403     P SetElenco       E
052600090403
052700090403
052800090403     P*--------------------------------------------------
052900090403     P* Procedure name: PutElenco
053000090403     P* Purpose:        Emette finestra elenco.
053100090403     P* Returns:        Esito.
053200090403     P*--------------------------------------------------
053300090403     P PutElenco       B
053400090403     D PutElenco       PI            10I 0
053500090403
053600090403      /FREE
053700090403
053800120320       dspInd.ricerca = (piRqsOpCode = SOCIETA_OPCODE_RICERCA);
053900090403       WRITE tasti;
054000090403       WRITE societaC;
054100090403
054200090403       RETURN ESITO_OK;
054300090403
054400090403      /END-FREE
054500090403     P PutElenco       E
054600090403
054700090403
054800090403     P*--------------------------------------------------
054900090403     P* Procedure name: GetElenco
055000090403     P* Purpose:        Legge la finestra elenco.
055100090403     P* Returns:        Esito.
055200090403     P*--------------------------------------------------
055300090403     P GetElenco       B
055400090403     D GetElenco       PI            10I 0
055500090403
055600090420     D savOpzione...
055700090420     D                 S                   LIKE(dsp_opzione)
055800090420
055900090420      /FREE
056000090403
056100090403       READ societaC;
056200090420
056300090420       dsp_sflRcdNbr = dsp_sflCsrRrn;
056400090420
056500090420       IF dspInf.dsp_aid_num = ESITO_WARNING_F13;
056600090420         dspInd.subfileNextChange = *ON;
056700090420         FOR societaRrn = 1 TO 9999;
056800090420           CHAIN societaRrn societaS;
056900090420           IF NOT %FOUND();
057000090420             LEAVE;
057100090420           ENDIF;
057200090420           IF savOpzione <> *BLANK AND dsp_opzione = *BLANK;
057300090420             dsp_opzione = savOpzione;
057400090420             UPDATE societaS;
057500090420             dsp_sflRcdNbr = dsp_sflCsrRrn;
057600090420           ELSE;
057700090420             dsp_opzione = %TRIML(dsp_opzione);
057800090420             UPDATE societaS;
057900090420             savOpzione = dsp_opzione;
058000090420           ENDIF;
058100090420         ENDFOR;
058200090420         dspInd.subfileNextChange = *OFF;
058300090420       ENDIF;
058400090420
058500090403       RETURN dspInf.dsp_aid_num;
058600090403
058700090403      /END-FREE
058800090403     P GetElenco       E
058900090403
059000090403
059100090403     P*--------------------------------------------------
059200090403     P* Procedure name: ChkElenco
059300090403     P* Purpose:        Controlla i dati inseriti nella finestra elenco.
059400090403     P* Returns:        Esito.
059500090403     P*--------------------------------------------------
059600090403     P ChkElenco       B
059700090403     D ChkElenco       PI            10I 0
059800090403
059900090403      /FREE
060000090403
060100090403       RESET dspInd.erroreGenerico;
060200090403
060300090403       IF dspInd.erroreGenerico;
060400090403         RETURN ESITO_ERROR;
060500090403       ENDIF;
060600090403
060700090403       RETURN ESITO_OK;
060800090403
060900090403      /END-FREE
061000090403     P ChkElenco       E
061100090403
061200090403
061300090403     P*--------------------------------------------------
061400090403     P* Procedure name: RunElenco
061500090403     P* Purpose:        Elabora i dati inseriti nella finestra elenco.
061600090403     P* Returns:        Esito.
061700090403     P*--------------------------------------------------
061800090403     P RunElenco       B
061900090403     D RunElenco       PI            10I 0
062000090420
062100090403      /FREE
062200090403
062300090409       DOU %EOF();
062400090409
062500090409         READC societaS;
062600090409
062700090409         IF %EOF();
062800090409           LEAVE;
062900090420         ELSEIF dsp_opzione = *BLANK;
063000090420           ITER;
063100090409         ENDIF;
063200090409
063300090420         dsp_sflRcdNbr = dsp_sflCsrRrn;
063400090420
063500090409         dsp_opzione = %TRIML(dsp_opzione);
063600090409
063700090409         IF dsp_opzione = OPZIONE_SOCIETAPROJ;
063800090420           RESET dsp_opzione;
063900090420           UPDATE societaS;
064000090420           RESET ana008Ds;
064100090409           ana008Ds.soc008 = dsp_idSocieta;
064200090409           kpjba.kpjbu = ana008Ds;
064300090409           Proj_AnagraficaSocieta( kpjba );
064400090409           ana008Ds = kpjba.kpjbu;
064500090409           IF ana008Ds.ret008 = PROJ_F8;
064600090409             ITER;
064700090409           ELSE;
064800090420             RETURN ESITO_WARNING_F12;
064900090409           ENDIF;
065000090409         ENDIF;
065100120320
065200120320         IF dsp_opzione = OPZIONE_SCELTA;
065300120320           IF dspInd.ricerca;
065400120320             tnsd4020o.idSocieta = dsp_idSocieta;
065500120320             RETURN ESITO_WARNING_F3;
065600120320           ELSE;
065700120320             CLEAR dsp_opzione;
065800120320             ITER;
065900120320           ENDIF;
066000120320         ENDIF;
066100090414
066200090409         IF dsp_opzione = OPZIONE_MODIFICARE;
066300090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_MODIFICARE;
066400090409         ELSEIF dsp_opzione = OPZIONE_NUOVAANAGRAFICA;
066500090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVAANAGRAFICA;
066600090409         ELSEIF dsp_opzione = OPZIONE_CANCELLAREANAGRAFICA;
066700090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
066800090409         ELSEIF dsp_opzione = OPZIONE_VISUALIZZARE;
066900090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE;
067000090414         ELSEIF dsp_opzione = OPZIONE_FINEATTIVITA;
067100090414           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_FINEATTIVITA;
067200090409         ENDIF;
067300090409
067400090409         tnsd4040i.idRiga = dsp_idRiga;
067500090409
067600090409         Societa_Manutenzione( SOCIETA_OPCODE_ANAGRAFICA
067700090409                             : rpyOpCode
067800090409                             : rpyIdMsg
067900090409                             : 'TNSD4040I'
068000090409                             : tnsd4040i
068100090409                             : %SIZE(tnsd4040i)
068200090409                             : 'TNSD4040O'
068300090409                             : tnsd4040o
068400090409                             : %SIZE(tnsd4040o)
068500090409                             );
068600090409
068700090409         CLEAR dsp_opzione;
068800090420         UPDATE societaS;
068900090409
069000090409         IF rpyOpCode = SOCIETA_OPCODE_FINE;
069100090409           RETURN ESITO_WARNING_F3;
069200090409         ELSEIF rpyOpCode = SOCIETA_OPCODE_RITORNO;
069300090420           RETURN ESITO_WARNING_F12;
069400090409           LEAVE;
069500090409         ENDIF;
069600090409
069700090409       ENDDO;
069800090409
069900090403       RETURN ESITO_OK;
070000090403
070100090403      /END-FREE
070200090403     P RunElenco       E
070300090403
