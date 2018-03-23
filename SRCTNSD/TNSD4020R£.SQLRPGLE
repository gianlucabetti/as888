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
004400090409     D OPZIONE_MODIFICARE...
004500090409     D                 C                   '2'
004600090409     D OPZIONE_NUOVAANAGRAFICA...
004700090409     D                 C                   '3'
004800090414     D OPZIONE_FINEATTIVITA...
004900090409     D                 C                   '4'
005000090409     D OPZIONE_VISUALIZZARE...
005100090409     D                 C                   '5'
005200090409     D OPZIONE_SOCIETAPROJ...
005300090409     D                 C                   '9'
005400090409     D OPZIONE_CANCELLAREANAGRAFICA...
005500090409     D                 C                   '44'
005600090409     D PROJ_F3...
005700090409     D                 C                   '1'
005800090409     D PROJ_F8...
005900090409     D                 C                   '0'
006000090409     D PROJ_F12...
006100090409     D                 C                   '2'
006200090409     D PROJ_OPZIONE_MODIFICARE...
006300090409     D                 C                   '02'
006400090403     D SOCIETA_OPCODE_ELENCO_CHK...
006500090403     D                 C                   'CHKELENCO'
006600090403     D SOCIETA_OPCODE_ELENCO_GET...
006700090403     D                 C                   'GETELENCO'
006800090403     D SOCIETA_OPCODE_ELENCO_INZ...
006900090403     D                 C                   'INZELENCO'
007000090403     D SOCIETA_OPCODE_ELENCO_PUT...
007100090403     D                 C                   'PUTELENCO'
007200090403     D SOCIETA_OPCODE_ELENCO_RUN...
007300090403     D                 C                   'RUNELENCO'
007400090403     D SOCIETA_OPCODE_ELENCO_SET...
007500090403     D                 C                   'SETELENCO'
007600090403
007700090403     ***************************************************************************
007800090403     **
007900090403     ** Dichiarazione prototipi.
008000090403     **
008100090403     ***************************************************************************
008200090409     D/COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
008300090409     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
008400090420     D/COPY GAITRASRC/SRCPROTOPR,XSOC
008500090403
008600090403     ***************************************************************************
008700090403     **
008800090403     ** Dichiarazione strutture dati.
008900090403     **
009000090403     ***************************************************************************
009100090409     D ana008Ds...
009200090409     D               E DS
009300090409     D                                     QUALIFIED
009400090409     D                                     INZ
009500090409     D  opz008...
009600090409     D               E                     INZ(PROJ_OPZIONE_MODIFICARE)
009700090403     D dspInd...
009800090403     D                 DS
009900090403     D                                     QUALIFIED
010000090403     D                                     INZ
010100090403     D subfileClear...
010200090403     D                         1      1N
010300090403     D subfileDisplay...
010400090403     D                         2      2N
010500090420     D subfileNextChange...
010600090420     D                         3      3N
010700090403     D erroreGenerico...
010800090403     D                        99     99N
010900090403     D dspInf...
011000090403     D                 DS
011100090403     D                                     QUALIFIED
011200090403     D  dsp_aid              369    369A
011300090403     D  dsp_aid_num          369    369U 0
011400090403     D  cursorRow            370    370I 0
011500090403     D  cursorCol            371    371I 0
011600090403     D  actCsrRow            382    382I 0
011700090403     D  actCsrCol            383    383I 0
011800090403     D kpjba...
011900090403     D               E DS
012000090403     D                                     QUALIFIED
012100090403     D                                     INZ
012200090403     D  knmus...
012300090403     D               E                     INZ(*USER)
012400090403     D tnsd4020i...
012500090403     D               E DS
012600090403     D                                     QUALIFIED
012700090403     D                                     INZ
012800090403     D*tnsd4020o...
012900090403     D*              E DS
013000090403     D*                                    QUALIFIED
013100090403     D*                                    INZ
013200090409     D tnsd4030i...
013300090409     D               E DS
013400090409     D                                     QUALIFIED
013500090409     D                                     INZ
013600090403     D tnsd4030o...
013700090403     D               E DS
013800090403     D                                     QUALIFIED
013900090403     D                                     INZ
014000090409     D tnsd4040i...
014100090409     D               E DS
014200090409     D                                     QUALIFIED
014300090409     D                                     INZ
014400090409     D tnsd4040o...
014500090409     D               E DS
014600090409     D                                     QUALIFIED
014700090409     D                                     INZ
014800090420     D xSoc001Ds...
014900090420     D               E DS
015000090420     D                                     QUALIFIED
015100090420     D                                     INZ
015200090420
015300090403     ***************************************************************************
015400090403     **
015500090403     ** Dichiarazione variabili.
015600090403     **
015700090403     ***************************************************************************
015800090403     D esito...
015900090403     D                 S             10I 0
016000090420     D esitoProj...
016100090420     D                 S              1A
016200090403     D opCodeCur...
016300090403     D                 S             10A
016400090403     D opCodeNxt...
016500090403     D                 S                   LIKE(opCodeCur)
016600090403     D                                     INZ(SOCIETA_OPCODE_ELENCO_INZ)
016700090403     D opCodePrv...
016800090403     D                 S                   LIKE(opCodeCur)
016900090403     D rpyOpCode...
017000090403     D                 S             10A
017100090403     D rpyIdMsg...
017200090403     D                 S             10I 0
017300090403     D societaRrn...
017400090403     D                 S              5U 0
017500090403
017600090403     ***************************************************************************
017700090403     **
017800090403     ** Dichiarazione interfaccia procedura principale.
017900090403     **
018000090403     ***************************************************************************
018100090403     D Societa_Elenco...
018200090403     D                 PI
018300090403     D  piRqsOpCode...
018400090403     D                               10A
018500090403     D                                     CONST
018600090403     D  piRpyOpCode...
018700090403     D                               10A
018800090403     D  piRpyIdMsg...
018900090403     D                               10I 0
019000090403     D  piRqsFormato...
019100090403     D                               10A
019200090403     D                                     CONST
019300090403     D                                     OPTIONS(*NOPASS)
019400090403     D  piRqsData...
019500090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
019600090403     D                                     CONST
019700090403     D  piRqsDataSize...
019800090403     D                               10I 0
019900090403     D                                     CONST
020000090403     D                                     OPTIONS(*NOPASS)
020100090403     D  piRpyFormato...
020200090403     D                               10A
020300090403     D                                     CONST
020400090403     D                                     OPTIONS(*NOPASS)
020500090403     D  piRpyData...
020600090403     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
020700090403     D  piRpyDataSize...
020800090403     D                               10I 0
020900090403     D                                     CONST
021000090403     D                                     OPTIONS(*NOPASS)
021100090403
021200090403     D*--------------------------------------------------
021300090403     D* Procedure name: InzElenco
021400090403     D* Purpose:        Inizializza finestra elenco.
021500090403     D* Returns:        Esito.
021600090403     D*--------------------------------------------------
021700090403     D InzElenco       PR            10I 0
021800090403
021900090403     D*--------------------------------------------------
022000090403     D* Procedure name: SetElenco
022100090403     D* Purpose:        Imposta dati finestra elenco.
022200090403     D* Returns:        Esito.
022300090403     D*--------------------------------------------------
022400090403     D SetElenco       PR            10I 0
022500090403
022600090403     D*--------------------------------------------------
022700090403     D* Procedure name: PutElenco
022800090403     D* Purpose:        Emette finestra elenco.
022900090403     D* Returns:        Esito.
023000090403     D*--------------------------------------------------
023100090403     D PutElenco       PR            10I 0
023200090403
023300090403     D*--------------------------------------------------
023400090403     D* Procedure name: GetElenco
023500090403     D* Purpose:        Legge la finestra elenco.
023600090403     D* Returns:        Esito.
023700090403     D*--------------------------------------------------
023800090403     D GetElenco       PR            10I 0
023900090403
024000090403     D*--------------------------------------------------
024100090403     D* Procedure name: ChkElenco
024200090403     D* Purpose:        Controlla i dati inseriti nella finestra elenco.
024300090403     D* Returns:        Esito.
024400090403     D*--------------------------------------------------
024500090403     D ChkElenco       PR            10I 0
024600090403
024700090403     D*--------------------------------------------------
024800090403     D* Procedure name: RunElenco
024900090403     D* Purpose:        Elabora i dati inseriti nella finestra elenco.
025000090403     D* Returns:        Esito.
025100090403     D*--------------------------------------------------
025200090403     D RunElenco       PR            10I 0
025300090403
025400090403
025500090403      /FREE
025600090403
025700090403       IF piRqsOpCode = SOCIETA_OPCODE_ELENCO;
025800090403
025900090408         RESET opCodeNxt;
026000090408
026100090403         DOU opCodeCur = SOCIETA_OPCODE_FINE;
026200090403
026300090403           opCodeCur = opCodeNxt;
026400090403
026500090403           IF opCodeCur = SOCIETA_OPCODE_ELENCO_INZ;
026600090403             InzElenco();
026700090403             opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
026800090403           ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_SET;
026900090403             SetElenco();
027000090403             opCodeNxt = SOCIETA_OPCODE_ELENCO_PUT;
027100090403           ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_PUT;
027200090403             PutElenco();
027300090403             opCodeNxt = SOCIETA_OPCODE_ELENCO_GET;
027400090403           ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_GET;
027500090403             esito = GetElenco();
027600090403             IF esito = ESITO_WARNING_ENTER;
027700090403               opCodeNxt = SOCIETA_OPCODE_ELENCO_RUN;
027800090403             ELSEIF esito = ESITO_WARNING_F5;
027900090403               opCodeNxt = SOCIETA_OPCODE_ELENCO_INZ;
028000090409             ELSEIF esito = ESITO_WARNING_F10;
028100090409               RESET tnsd4040i;
028200090409               tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVASOCIETA;
028300090409               Societa_Manutenzione( SOCIETA_OPCODE_ANAGRAFICA
028400090409                                   : rpyOpCode
028500090409                                   : rpyIdMsg
028600090409                                   : 'TNSD4040I'
028700090409                                   : tnsd4040i
028800090409                                   : %SIZE(tnsd4040i)
028900090409                                   : 'TNSD4040O'
029000090409                                   : tnsd4040o
029100090409                                   : %SIZE(tnsd4040o)
029200090409                                   );
029300090409               opCodeNxt = SOCIETA_OPCODE_ELENCO_INZ;
029400090403             ELSEIF esito = ESITO_WARNING_F12;
029500090403               opCodeNxt = SOCIETA_OPCODE_RITORNO;
029600090420             ELSEIF esito = ESITO_WARNING_F13;
029700090420               opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
029800090403             ELSE;
029900090403               opCodeNxt = SOCIETA_OPCODE_FINE;
030000090403             ENDIF;
030100090403           ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_CHK;
030200090403             IF ChkElenco() = ESITO_OK;
030300090403               opCodeNxt = SOCIETA_OPCODE_ELENCO_RUN;
030400090403             ELSE;
030500090403               opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
030600090403             ENDIF;
030700090403           ELSEIF opCodeCur = SOCIETA_OPCODE_ELENCO_RUN;
030800090403             IF ChkElenco() = ESITO_OK;
030900090409               esito = RunElenco();
031000090409               IF esito = ESITO_WARNING_F3;
031100090409                 opCodeNxt = SOCIETA_OPCODE_FINE;
031200090420               ELSEIF esito = ESITO_WARNING_F12;
031300090420                 opCodeNxt = SOCIETA_OPCODE_ELENCO_PUT;
031400090409               ELSE;
031500090420                 opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
031600090409               ENDIF;
031700090403             ELSE;
031800090403               opCodeNxt = SOCIETA_OPCODE_ELENCO_SET;
031900090403             ENDIF;
032000090408           ELSEIF opCodeCur = SOCIETA_OPCODE_RITORNO;
032100090408             piRpyOpCode = opCodeCur;
032200090408             LEAVE;
032300090403           ELSEIF opCodeCur = SOCIETA_OPCODE_FINE;
032400090408             piRpyOpCode = opCodeCur;
032500090403             LEAVE;
032600090403           ELSE;
032700090403             opCodeNxt = SOCIETA_OPCODE_FINE;
032800090403           ENDIF;
032900090403
033000090403         ENDDO;
033100090403
033200090403       ELSEIF piRqsOpCode = SOCIETA_OPCODE_INIT;
033300090403
033400090403         IF piRqsFormato = 'KPJBA';
033500090403           kpjba = piRqsData;
033600090403         ENDIF;
033700090403
033800090420         xSoc001Ds.xscSoc = kpjba.kpjbu;
033900090420
034000090420         Proj_Societa( 'SOC001'
034100090420                     : xSoc001Ds.xscSoc
034200090420                     : *ZERO
034300090420                     : *BLANK
034400090420                     : esitoProj
034500090420                     : xSoc001Ds
034600090420                     : kpjba
034700090420                     );
034800090420
034900090420         IF esitoProj = *ON;
035000090420           RETURN;
035100090420         ENDIF;
035200090420
035300090420         kpjba.kpjbu = xSoc001Ds.xscSoc;
035400090420
035500090403         Societa_Selezione( SOCIETA_OPCODE_INIT
035600090403                          : rpyOpCode
035700090403                          : rpyIdMsg
035800090403                          : 'KPJBA'
035900090403                          : kpjba
036000090403                          : %SIZE(kpjba)
036100090403                          );
036200090420
036300090420         kpjba.kpjbu = xSoc001Ds.xscSoc;
036400090409
036500090409         Societa_Manutenzione( SOCIETA_OPCODE_INIT
036600090409                             : rpyOpCode
036700090409                             : rpyIdMsg
036800090409                             : 'KPJBA'
036900090409                             : kpjba
037000090409                             : %SIZE(kpjba)
037100090409                             );
037200090403
037300090403       ELSEIF piRqsOpCode = SOCIETA_OPCODE_FINALIZE;
037400090403
037500090403         Societa_Selezione( SOCIETA_OPCODE_FINALIZE
037600090403                          : rpyOpCode
037700090403                          : rpyIdMsg
037800090403                          );
037900090409
038000090409         Societa_Manutenzione( SOCIETA_OPCODE_FINALIZE
038100090409                             : rpyOpCode
038200090409                             : rpyIdMsg
038300090409                             );
038400090403
038500090403         *INLR = *ON;
038600090403
038700090403       ENDIF;
038800090403
038900090403       RETURN;
039000090403
039100090420       //***********************************************************************
039200090420       //
039300090420       //
039400090420       //
039500090420       //***********************************************************************
039600090420
039700090420       BEGSR *INZSR;
039800090420
039900090420         dsp_sflRcdNbr = 1;
040000090420
040100090420       ENDSR;
040200090420
040300090403      /END-FREE
040400090403
040500090403
040600090403     P*--------------------------------------------------
040700090403     P* Procedure name: InzElenco
040800090403     P* Purpose:        Inizializza finestra elenco.
040900090403     P* Returns:        Esito.
041000090403     P*--------------------------------------------------
041100090403     P InzElenco       B
041200090403     D InzElenco       PI            10I 0
041300090403
041400090403      /FREE
041500090403
041600090403       RESET dspInd;
041700090403       dspInd.subfileClear = *ON;
041800090403       dspInd.subfileDisplay = *OFF;
041900090403       WRITE societaC;
042000090403       dspInd.subfileClear = *OFF;
042100090403       RESET societaRrn;
042200090420       RESET dsp_sflRcdNbr;
042300090420
042400090403       IF piRqsFormato = 'TNSD4020I';
042500090403         tnsd4020i = %SUBST(piRqsData : 1 : piRqsDataSize);
042600090403       ENDIF;
042700090403
042800090403       EVAL-CORR tnsd4030i = tnsd4020i;
042900090403
043000090403       Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_NEW
043100090403                        : rpyOpCode
043200090403                        : rpyIdMsg
043300090403                        : 'TNSD4030I'
043400090403                        : tnsd4030i
043500090403                        : %SIZE(tnsd4030i)
043600090403                        );
043700090403
043800090403       IF rpyOpCode <> SOCIETA_OPCODE_DONE;
043900090403         RETURN ESITO_ERROR;
044000090403       ENDIF;
044100090403
044200090409       CLEAR societaS;
044300090409
044400090403       DOU rpyIdMsg = 100;
044500090403
044600090403         Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_GETNEXTROW
044700090403                          : rpyOpCode
044800090403                          : rpyIdMsg
044900090403                          : *OMIT
045000090403                          : *OMIT
045100090403                          : *OMIT
045200090403                          : 'TNSD4030O'
045300090403                          : tnsd4030o
045400090403                          : %SIZE(tnsd4030o)
045500090403                          );
045600090403
045700090403         IF rpyIdMsg < ESITO_OK OR rpyIdMsg = 100;
045800090403           LEAVE;
045900090403         ENDIF;
046000090403
046100090403         IF rpyOpCode = SOCIETA_OPCODE_DONE;
046200090403           EXEC SQL
046300090403             SELECT AZSOC00F.ID_SOCIETA
046400090403                  , AZSOC00F.RAGIONE_SOCIALE_BREVE
046500090403                  , AZSOC00F.VALIDITA_DATA_INIZIO
046600090403                  , AZSOC00F.VALIDITA_DATA_FINE
046700090409                  , IFNULL(CHAR(AZSOC00F.ATTIVITA_DATA_FINE , EUR) , '')
046800090403                  , AZSOC00F.CODICE_FISCALE
046900090403               INTO :dsp_idSocieta
047000090403                  , :dsp_ragSocBrev
047100090403                  , :dsp_valDatIniz
047200090403                  , :dsp_valDatFine
047300090403                  , :dsp_attDatFine
047400090403                  , :dsp_codFiscale
047500090403               FROM AZSOC00F
047600090403               WHERE AZSOC00F.ID_RIGA = :tnsd4030o.idRiga
047700090403           ;
047800090409           dsp_idRiga = tnsd4030o.idRiga;
047900090409           societaRrn += 1;
048000090403           WRITE societaS;
048100090403         ENDIF;
048200090403
048300090403       ENDDO;
048400090403
048500090403       Societa_Selezione( SOCIETA_OPCODE_SELEZIONE_END
048600090403                        : rpyOpCode
048700090403                        : rpyIdMsg
048800090403                        );
048900090403
049000090403       dspInd.subfileDisplay = (societaRrn > *ZERO);
049100090403
049200090403       RETURN ESITO_OK;
049300090403
049400090403      /END-FREE
049500090403     P InzElenco       E
049600090403
049700090403
049800090403     P*--------------------------------------------------
049900090403     P* Procedure name: SetElenco
050000090403     P* Purpose:        Imposta dati finestra elenco.
050100090403     P* Returns:        Esito.
050200090403     P*--------------------------------------------------
050300090403     P SetElenco       B
050400090403     D SetElenco       PI            10I 0
050500090403
050600090403      /FREE
050700090403
050800090403       RETURN ESITO_OK;
050900090403
051000090403      /END-FREE
051100090403     P SetElenco       E
051200090403
051300090403
051400090403     P*--------------------------------------------------
051500090403     P* Procedure name: PutElenco
051600090403     P* Purpose:        Emette finestra elenco.
051700090403     P* Returns:        Esito.
051800090403     P*--------------------------------------------------
051900090403     P PutElenco       B
052000090403     D PutElenco       PI            10I 0
052100090403
052200090403      /FREE
052300090403
052400090403       WRITE tasti;
052500090403       WRITE societaC;
052600090403
052700090403       RETURN ESITO_OK;
052800090403
052900090403      /END-FREE
053000090403     P PutElenco       E
053100090403
053200090403
053300090403     P*--------------------------------------------------
053400090403     P* Procedure name: GetElenco
053500090403     P* Purpose:        Legge la finestra elenco.
053600090403     P* Returns:        Esito.
053700090403     P*--------------------------------------------------
053800090403     P GetElenco       B
053900090403     D GetElenco       PI            10I 0
054000090403
054100090420     D savOpzione...
054200090420     D                 S                   LIKE(dsp_opzione)
054300090420
054400090420      /FREE
054500090403
054600090403       READ societaC;
054700090420
054800090420       dsp_sflRcdNbr = dsp_sflCsrRrn;
054900090420
055000090420       IF dspInf.dsp_aid_num = ESITO_WARNING_F13;
055100090420         dspInd.subfileNextChange = *ON;
055200090420         FOR societaRrn = 1 TO 9999;
055300090420           CHAIN societaRrn societaS;
055400090420           IF NOT %FOUND();
055500090420             LEAVE;
055600090420           ENDIF;
055700090420           IF savOpzione <> *BLANK AND dsp_opzione = *BLANK;
055800090420             dsp_opzione = savOpzione;
055900090420             UPDATE societaS;
056000090420             dsp_sflRcdNbr = dsp_sflCsrRrn;
056100090420           ELSE;
056200090420             dsp_opzione = %TRIML(dsp_opzione);
056300090420             UPDATE societaS;
056400090420             savOpzione = dsp_opzione;
056500090420           ENDIF;
056600090420         ENDFOR;
056700090420         dspInd.subfileNextChange = *OFF;
056800090420       ENDIF;
056900090420
057000090403       RETURN dspInf.dsp_aid_num;
057100090403
057200090403      /END-FREE
057300090403     P GetElenco       E
057400090403
057500090403
057600090403     P*--------------------------------------------------
057700090403     P* Procedure name: ChkElenco
057800090403     P* Purpose:        Controlla i dati inseriti nella finestra elenco.
057900090403     P* Returns:        Esito.
058000090403     P*--------------------------------------------------
058100090403     P ChkElenco       B
058200090403     D ChkElenco       PI            10I 0
058300090403
058400090403      /FREE
058500090403
058600090403       RESET dspInd.erroreGenerico;
058700090403
058800090403       IF dspInd.erroreGenerico;
058900090403         RETURN ESITO_ERROR;
059000090403       ENDIF;
059100090403
059200090403       RETURN ESITO_OK;
059300090403
059400090403      /END-FREE
059500090403     P ChkElenco       E
059600090403
059700090403
059800090403     P*--------------------------------------------------
059900090403     P* Procedure name: RunElenco
060000090403     P* Purpose:        Elabora i dati inseriti nella finestra elenco.
060100090403     P* Returns:        Esito.
060200090403     P*--------------------------------------------------
060300090403     P RunElenco       B
060400090403     D RunElenco       PI            10I 0
060500090420
060600090403      /FREE
060700090403
060800090409       DOU %EOF();
060900090409
061000090409         READC societaS;
061100090409
061200090409         IF %EOF();
061300090409           LEAVE;
061400090420         ELSEIF dsp_opzione = *BLANK;
061500090420           ITER;
061600090409         ENDIF;
061700090409
061800090420         dsp_sflRcdNbr = dsp_sflCsrRrn;
061900090420
062000090409         dsp_opzione = %TRIML(dsp_opzione);
062100090409
062200090409         IF dsp_opzione = OPZIONE_SOCIETAPROJ;
062300090420           RESET dsp_opzione;
062400090420           UPDATE societaS;
062500090420           RESET ana008Ds;
062600090409           ana008Ds.soc008 = dsp_idSocieta;
062700090409           kpjba.kpjbu = ana008Ds;
062800090409           Proj_AnagraficaSocieta( kpjba );
062900090409           ana008Ds = kpjba.kpjbu;
063000090409           IF ana008Ds.ret008 = PROJ_F8;
063100090409             ITER;
063200090409           ELSE;
063300090420             RETURN ESITO_WARNING_F12;
063400090409           ENDIF;
063500090409         ENDIF;
063600090414
063700090409         IF dsp_opzione = OPZIONE_MODIFICARE;
063800090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_MODIFICARE;
063900090409         ELSEIF dsp_opzione = OPZIONE_NUOVAANAGRAFICA;
064000090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVAANAGRAFICA;
064100090409         ELSEIF dsp_opzione = OPZIONE_CANCELLAREANAGRAFICA;
064200090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
064300090409         ELSEIF dsp_opzione = OPZIONE_VISUALIZZARE;
064400090409           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE;
064500090414         ELSEIF dsp_opzione = OPZIONE_FINEATTIVITA;
064600090414           tnsd4040i.rqsOpCode = SOCIETA_OPCODE_FINEATTIVITA;
064700090409         ENDIF;
064800090409
064900090409         tnsd4040i.idRiga = dsp_idRiga;
065000090409
065100090409         Societa_Manutenzione( SOCIETA_OPCODE_ANAGRAFICA
065200090409                             : rpyOpCode
065300090409                             : rpyIdMsg
065400090409                             : 'TNSD4040I'
065500090409                             : tnsd4040i
065600090409                             : %SIZE(tnsd4040i)
065700090409                             : 'TNSD4040O'
065800090409                             : tnsd4040o
065900090409                             : %SIZE(tnsd4040o)
066000090409                             );
066100090409
066200090409         CLEAR dsp_opzione;
066300090420         UPDATE societaS;
066400090409
066500090409         IF rpyOpCode = SOCIETA_OPCODE_FINE;
066600090409           RETURN ESITO_WARNING_F3;
066700090409         ELSEIF rpyOpCode = SOCIETA_OPCODE_RITORNO;
066800090420           RETURN ESITO_WARNING_F12;
066900090409           LEAVE;
067000090409         ENDIF;
067100090409
067200090409       ENDDO;
067300090409
067400090403       RETURN ESITO_OK;
067500090403
067600090403      /END-FREE
067700090403     P RunElenco       E
067800090403
