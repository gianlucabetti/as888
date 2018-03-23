000100090409     ***************************************************************************
000200090409     **
000300090409     ** Anagrafico società.
000400090409     ** Manutenzione.
000500090409     **
000600090409     ***************************************************************************
000700090420     H DFTACTGRP(*NO) ACTGRP('AZSOC') BNDDIR('PJXBND')
000800090409
000900090409     ***************************************************************************
001000090409     **
001100090409     ** Dichiarazione files.
001200090409     **
001300090409     ***************************************************************************
001400090409     Ftnsd4040d CF   E             WORKSTN
001500090409     F                                     PREFIX(dsp_)
001600090409     F                                     INDDS(dspInd)
001700090409     F                                     INFDS(dspInf)
001800090409
001900090409     ***************************************************************************
002000090409     **
002100090409     ** Definizione costanti.
002200090409     **
002300090409     ***************************************************************************
002400090409     D/COPY GAITRASRC/SRCCONST,TNSD4000R
002500090415     D DSPATR_NONPROTECT_UNDERSCORE...
002600090415     D                 C                   X'24'
002700090415     D DSPATR_PROTECT_NORMAL...
002800090415     D                 C                   X'A0'
002900090409     D ESITO_ERROR...
003000090409     D                 C                   -1
003100090409     D ESITO_OK...
003200090409     D                 C                   0
003300090409     D ESITO_WARNING...
003400090409     D                 C                   1
003500090415     D ESITO_WARNING_PERIODINONCONTIGUI...
003600090415     D                 C                   2
003700090409     D ESITO_WARNING_ENTER...
003800090409     D                 C                   241
003900090409     D ESITO_WARNING_F3...
004000090409     D                 C                   51
004100090414     D ESITO_WARNING_F4...
004200090414     D                 C                   52
004300090409     D ESITO_WARNING_F5...
004400090409     D                 C                   53
004500090409     D ESITO_WARNING_F6...
004600090409     D                 C                   54
004700090409     D ESITO_WARNING_F8...
004800090409     D                 C                   56
004900090409     D ESITO_WARNING_F12...
005000090409     D                 C                   60
005100090409     D ESITO_WARNING_ROLLDOWN...
005200090409     D                 C                   244
005300090409     D ESITO_WARNING_ROLLUP...
005400090409     D                 C                   245
005500090416     D ITALIA...
005600090416     D                 C                   'IT'
005700090414     D NOT_NULL...
005800090414     D                 C                   0
005900090416     D ORDINAMENTO_DESCRIZIONE...
006000090416     D                 C                   '1'
006100090416     D PROJ_CHAIN...
006200090416     D                 C                   '1'
006300090410     D PROJ_ENTER...
006400090410     D                 C                   ' '
006500090410     D PROJ_F3...
006600090410     D                 C                   '1'
006700090410     D PROJ_F12...
006800090410     D                 C                   '2'
006900090410     D PROJ_OPZIONE_RICERCA...
007000090410     D                 C                   '01'
007100090409     D SOCIETA_OPCODE_ANAGRAFICA_CHK...
007200090409     D                 C                   'CHKANAGRAF'
007300090409     D SOCIETA_OPCODE_ANAGRAFICA_GET...
007400090409     D                 C                   'GETANAGRAF'
007500090409     D SOCIETA_OPCODE_ANAGRAFICA_INZ...
007600090409     D                 C                   'INZANAGRAF'
007700090409     D SOCIETA_OPCODE_ANAGRAFICA_PUT...
007800090409     D                 C                   'PUTANAGRAF'
007900090409     D SOCIETA_OPCODE_ANAGRAFICA_RUN...
008000090409     D                 C                   'RUNANAGRAF'
008100090409     D SOCIETA_OPCODE_ANAGRAFICA_SET...
008200090409     D                 C                   'SETANAGRAF'
008300090410     D SOCIETA_OPCODE_FINEATTIVITA_CHK...
008400090410     D                 C                   'CHKFINEATT'
008500090410     D SOCIETA_OPCODE_FINEATTIVITA_GET...
008600090410     D                 C                   'GETFINEATT'
008700090410     D SOCIETA_OPCODE_FINEATTIVITA_INZ...
008800090410     D                 C                   'INZFINEATT'
008900090410     D SOCIETA_OPCODE_FINEATTIVITA_PUT...
009000090410     D                 C                   'PUTFINEATT'
009100090410     D SOCIETA_OPCODE_FINEATTIVITA_RUN...
009200090410     D                 C                   'RUNFINEATT'
009300090410     D SOCIETA_OPCODE_FINEATTIVITA_SET...
009400090410     D                 C                   'SETFINEATT'
009500090416     D TABELLA_NAZIONI...
009600090416     D                 C                   '15'
009700090416     D TABELLA_PROVINCIE...
009800090416     D                 C                   'PR'
009900090409
010000090409     ***************************************************************************
010100090409     **
010200090409     ** Dichiarazione prototipi.
010300090409     **
010400090409     ***************************************************************************
010500090410     D/COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
010600090415     D/COPY GAITRASRC/SRCPROTOPR,QUILNGTX
010700090414     D/COPY GAITRASRC/SRCPROTOPR,TIBS02R
010800090414     D/COPY GAITRASRC/SRCPROTOPR,TNSD4000R
010900090416     D/COPY GAITRASRC/SRCPROTOPR,TRUL19R
011000090428     D/COPY GAITRASRC/SRCPROTOPR,TRULCALER
011100090416     D/COPY GAITRASRC/SRCPROTOPR,XATB
011200090416     D/COPY GAITRASRC/SRCPROTOPR,XCHKAUT
011300090416     D/COPY GAITRASRC/SRCPROTOPR,XSOC
011400090409
011500090409     ***************************************************************************
011600090409     **
011700090409     ** Dichiarazione strutture dati.
011800090409     **
011900090409     ***************************************************************************
012000090415     D/COPY QSYSINC/QRPGLESRC,QUSEC
012100090410     D ana007Ds...
012200090410     D               E DS
012300090410     D                                     QUALIFIED
012400090410     D                                     INZ
012500090410     D  opz007...
012600090410     D               E                     INZ(PROJ_OPZIONE_RICERCA)
012700090416     D angG16Ds...
012800090416     D               E DS
012900090416     D                                     QUALIFIED
013000090416     D                                     INZ
013100090416     D dCtt...
013200090416     D               E DS
013300090416     D                                     QUALIFIED
013400090416     D                                     INZ
013500090409     D dspInd...
013600090409     D                 DS
013700090409     D                                     QUALIFIED
013800090409     D                                     INZ
013900090416     D  errMsgCodiceFiscaleGiaUsato...
014000090415     D                         1      1N
014100090416     D  errMsgDataValiditaSequenza...
014200090415     D                         2      2N
014300090416     D  errMsgDataValiditaInizioSovrapposta...
014400090415     D                         3      3N
014500090416     D  errMsgDataValiditaFineSovrapposta...
014600090415     D                         4      4N
014700090416     D  errMsgIdSocieta...
014800090415     D                         5      5N
014900090416     D  errMsgAteco2007...
015000090415     D                         6      6N
015100090416     D  errMsgCatastoSedeLegale...
015200090416     D                         7      7N
015300090416     D  errMsgCatastoSedeAmministrativa...
015400090416     D                         8      8N
015500090416     D  errMsgProvinciaSedeLegale...
015600090416     D                         9      9N
015700090416     D  errMsgProvinciaSedeAmministrativa...
015800090416     D                        10     10N
015900090416     D  errMsgNazioneSedeLegale...
016000090416     D                        11     11N
016100090416     D  errMsgNazioneSedeAmministrativa...
016200090416     D                        12     12N
016300090416     D  errMsgNaturaGiuridica...
016400090416     D                        13     13N
016500090416     D  errMsgPartitaIvaObbligatoria...
016600090416     D                        14     14N
016700090416     D  errMsgPartitaIvaErrata...
016800090416     D                        15     15N
016900090420     D  errMsgTipoCapitaleSociale...
017000090420     D                        16     16N
017100090429     D  errMsgImportoCapitaleSociale...
017200090429     D                        17     17N
017300090430     D  errMsgDataFineAttivita...
017400090430     D                        18     18N
017500090415     D  erroreGenerico...
017600090409     D                        99     99N
017700090409     D dspInf...
017800090409     D                 DS
017900090409     D                                     QUALIFIED
018000090409     D  dsp_aid              369    369A
018100090409     D  dsp_aid_num          369    369U 0
018200090409     D  cursorRow            370    370I 0
018300090409     D  cursorCol            371    371I 0
018400090409     D  actCsrRow            382    382I 0
018500090409     D  actCsrCol            383    383I 0
018600090416     D dsPR_sedeLegale...
018700090416     D               E DS                  EXTNAME(dsPR)
018800090416     D                                     QUALIFIED
018900090416     D                                     INZ
019000090416     D  tblUni...
019100090416     D                         1     89A
019200090416     D dsPR_sedeAmministrativa...
019300090416     D               E DS                  EXTNAME(dsPR)
019400090416     D                                     QUALIFIED
019500090416     D                                     INZ
019600090416     D  tblUni...
019700090416     D                         1     89A
019800090416     D ds15_sedeLegale...
019900090416     D               E DS                  EXTNAME(ds15)
020000090416     D                                     QUALIFIED
020100090416     D                                     INZ
020200090416     D  tblUni...
020300090416     D                         1     89A
020400090416     D ds15_sedeAmministrativa...
020500090416     D               E DS                  EXTNAME(ds15)
020600090416     D                                     QUALIFIED
020700090416     D                                     INZ
020800090416     D  tblUni...
020900090416     D                         1     89A
021000090409     D kpjba...
021100090409     D               E DS
021200090409     D                                     QUALIFIED
021300090409     D                                     INZ
021400090409     D  knmus...
021500090409     D               E                     INZ(*USER)
021600090414     D tibs02ds...
021700090414     D               E DS
021800090414     D                                     QUALIFIED
021900090414     D                                     INZ
022000090409     D tnsd4040i...
022100090409     D               E DS
022200090409     D                                     QUALIFIED
022300090409     D                                     INZ
022400090409     D tnsd4040o...
022500090409     D               E DS
022600090409     D                                     QUALIFIED
022700090409     D                                     INZ
022800090416     D xAtbDs...
022900090416     D               E DS
023000090416     D                                     QUALIFIED
023100090416     D                                     INZ
023200090416     D  xtbRic...
023300090416     D               E                     INZ(PROJ_CHAIN)
023400090416     D xChkAutDs...
023500090416     D               E DS
023600090416     D                                     QUALIFIED
023700090416     D                                     INZ
023800090416     D  xcaFnc...
023900090416     D               E                     INZ('YAZSOC')
024000090416     D  xcaVfu...
024100090416     D               E                     INZ(*ZERO)
024200090416     D  xcaTck...
024300090416     D               E                     INZ('CK')
024400090416     D  xcaNau...
024500090416     D               E                     INZ(*ON)
024600090420     D xSoc001Ds_lavoro...
024700090420     D               E DS
024800090420     D                                     EXTNAME(xSoc001Ds)
024900090420     D                                     QUALIFIED
025000090420     D                                     INZ
025100090410     D xSoc001Ds...
025200090410     D               E DS
025300090410     D                                     QUALIFIED
025400090410     D                                     INZ
025500090416     D xTabDs...
025600090416     D               E DS
025700090416     D                                     QUALIFIED
025800090416     D                                     INZ
025900090416     D  xtaOpz...
026000090416     D               E                     INZ(PROJ_OPZIONE_RICERCA)
026100090409
026200090409     ***************************************************************************
026300090409     **
026400090409     ** Dichiarazione variabili.
026500090409     **
026600090409     ***************************************************************************
026700090415     D count...
026800090415     D                 S             10I 0
026900090430     D dataEUR0...
027000090430     D                 S              8A
027100090409     D esito...
027200090409     D                 S             10I 0
027300090416     D esitoPartitaIva...
027400090416     D                 S              1P 0
027500090410     D esitoProj...
027600090410     D                 S              1A
027700090416     D idElemento...
027800090416     D                 S              8A
027900090409     D opCodeCur...
028000090409     D                 S             10A
028100090409     D opCodeNxt...
028200090409     D                 S                   LIKE(opCodeCur)
028300090409     D                                     INZ(SOCIETA_OPCODE_ANAGRAFICA_INZ)
028400090409     D opCodePrv...
028500090409     D                 S                   LIKE(opCodeCur)
028600090409     D pannello...
028700090409     D                 S              3U 0 INZ(1)
028800090416     D partitaIvaProj...
028900090416     D                 S             20A
029000090416     D tastoFunzionaleUscita...
029100090416     D                 S              1A
029200090415     D textString...
029300090415     D                 S            256A
029400090409
029500090409     ***************************************************************************
029600090409     **
029700090409     ** Dichiarazione interfaccia procedura principale.
029800090409     **
029900090409     ***************************************************************************
030000090409     D Societa_Manutenzione...
030100090409     D                 PI
030200090409     D  piRqsOpCode...
030300090409     D                               10A
030400090409     D                                     CONST
030500090409     D  piRpyOpCode...
030600090409     D                               10A
030700090409     D  piRpyIdMsg...
030800090409     D                               10I 0
030900090409     D  piRqsFormato...
031000090409     D                               10A
031100090409     D                                     CONST
031200090409     D                                     OPTIONS(*NOPASS:*OMIT)
031300090409     D  piRqsData...
031400090409     D                            65535A   OPTIONS(*VARSIZE:*NOPASS:*OMIT)
031500090409     D                                     CONST
031600090409     D  piRqsDataSize...
031700090409     D                               10I 0
031800090409     D                                     CONST
031900090409     D                                     OPTIONS(*NOPASS:*OMIT)
032000090409     D  piRpyFormato...
032100090409     D                               10A
032200090409     D                                     CONST
032300090409     D                                     OPTIONS(*NOPASS)
032400090409     D  piRpyData...
032500090409     D                            65535A   OPTIONS(*VARSIZE:*NOPASS)
032600090409     D  piRpyDataSize...
032700090409     D                               10I 0
032800090409     D                                     CONST
032900090409     D                                     OPTIONS(*NOPASS)
033000090409
033100090409     D*--------------------------------------------------
033200090409     D* Procedure name: InzAnagrafica
033300090409     D* Purpose:        Inizializza finestra elenco.
033400090409     D* Returns:        Esito.
033500090409     D*--------------------------------------------------
033600090409     D InzAnagrafica   PR            10I 0
033700090409
033800090409     D*--------------------------------------------------
033900090409     D* Procedure name: SetAnagrafica
034000090409     D* Purpose:        Imposta dati finestra elenco.
034100090409     D* Returns:        Esito.
034200090409     D*--------------------------------------------------
034300090409     D SetAnagrafica   PR            10I 0
034400090409
034500090409     D*--------------------------------------------------
034600090409     D* Procedure name: PutAnagrafica
034700090409     D* Purpose:        Emette finestra elenco.
034800090409     D* Returns:        Esito.
034900090409     D*--------------------------------------------------
035000090409     D PutAnagrafica   PR            10I 0
035100090409
035200090409     D*--------------------------------------------------
035300090409     D* Procedure name: GetAnagrafica
035400090409     D* Purpose:        Legge la finestra elenco.
035500090409     D* Returns:        Esito.
035600090409     D*--------------------------------------------------
035700090409     D GetAnagrafica   PR            10I 0
035800090409
035900090409     D*--------------------------------------------------
036000090409     D* Procedure name: ChkAnagrafica
036100090409     D* Purpose:        Controlla i dati inseriti nella finestra elenco.
036200090409     D* Returns:        Esito.
036300090409     D*--------------------------------------------------
036400090409     D ChkAnagrafica   PR            10I 0
036500090409
036600090409     D*--------------------------------------------------
036700090409     D* Procedure name: RunAnagrafica
036800090409     D* Purpose:        Elabora i dati inseriti nella finestra elenco.
036900090409     D* Returns:        Esito.
037000090409     D*--------------------------------------------------
037100090409     D RunAnagrafica   PR            10I 0
037200090414
037300090414     D*--------------------------------------------------
037400090414     D* Procedure name: InzFineAttivita
037500090414     D* Purpose:        Inizializza la finestra di fine attività società.
037600090414     D* Returns:        Esito.
037700090414     D*--------------------------------------------------
037800090414     DInzFineAttivita  PR            10I 0
037900090414
038000090414     D*--------------------------------------------------
038100090414     D* Procedure name: SetFineAttivita
038200090414     D* Purpose:        Impstazione dati variabili finestra fine attività s...
038300090414     D*                          ocietà.
038400090414     D* Returns:        Esito.
038500090414     D*--------------------------------------------------
038600090414     DSetFineAttivita  PR            10I 0
038700090414
038800090414     D*--------------------------------------------------
038900090414     D* Procedure name: PutFineAttivita
039000090414     D* Purpose:        Emetto finestra fine attività società
039100090414     D* Returns:        Esito.
039200090414     D*--------------------------------------------------
039300090414     DPutFineAttivita  PR            10I 0
039400090414
039500090414     D*--------------------------------------------------
039600090414     D* Procedure name: GetFineAttivita
039700090414     D* Purpose:        Leggo finestra fine attività società.
039800090414     D* Returns:        Esito.
039900090414     D*--------------------------------------------------
040000090414     DGetFineAttivita  PR            10I 0
040100090414
040200090414     D*--------------------------------------------------
040300090414     D* Procedure name: ChkFineAttivita
040400090414     D* Purpose:
040500090414     D* Returns:        Esito.
040600090414     D*--------------------------------------------------
040700090414     DChkFineAttivita  PR            10I 0
040800090414
040900090414     D*--------------------------------------------------
041000090414     D* Procedure name: RunFineAttivita
041100090414     D* Purpose:        Elaborazione finestra fine attività società.
041200090414     D* Returns:        Esito.
041300090414     D*--------------------------------------------------
041400090414     DRunFineAttivita  PR            10I 0
041500090414
041600090414     D*--------------------------------------------------
041700090414     D* Procedure name: RunRichiesta
041800090414     D* Purpose:        Gestione F4=Richiesta.
041900090414     D* Returns:        Esito.
042000090414     D*--------------------------------------------------
042100090414     D RunRichiesta    PR            10I 0
042200090414
042300090414
042400090409      /FREE
042500090416
042600090409       IF piRqsOpCode = SOCIETA_OPCODE_ANAGRAFICA;
042700090416
042800090416         // Controllo autorizzazione.
042900090416
043000090416         RESET xChkAutDs;
043100090420         xChkAutDs.xcaPrf = kpjba.knmus;
043200090416         Proj_Autorizzazione( xChkAutDs );
043300090416
043400090416         IF xChkAutDs.xcaRtn <> *ZERO;
043500090416           RETURN;
043600090416         ENDIF;
043700090416
043800090416         // Ricezione parametri.
043900090416
044000090409         IF piRqsFormato = 'TNSD4040I';
044100090409           tnsd4040i = %SUBST(piRqsData : 1 : piRqsDataSize);
044200090409         ENDIF;
044300090409
044400090409         IF tnsd4040i.rqsOpCode <> SOCIETA_OPCODE_VISUALIZZARE;
044500090409           EXEC SQL
044600090409             SET TRANSACTION ISOLATION LEVEL REPEATABLE READ
044700090409           ;
044800090409         ENDIF;
044900090409
045000090409         RESET opCodeNxt;
045100090409         RESET pannello;
045200090420         RESET tnsd4040o;
045300090409
045400090414         IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_FINEATTIVITA;
045500090414           opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_INZ;
045600090414         ENDIF;
045700090414
045800090409         DOU opCodeCur = SOCIETA_OPCODE_FINE;
045900090409
046000090409           opCodeCur = opCodeNxt;
046100090409
046200090414           // Gestione della anagrafica.
046300090414
046400090409           IF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_INZ;
046500090414
046600090409             InzAnagrafica();
046700090414             opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_CHK;
046800090414
046900090409           ELSEIF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_SET;
047000090414
047100090409             SetAnagrafica();
047200090409             opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_PUT;
047300090414
047400090409           ELSEIF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_PUT;
047500090414
047600090409             PutAnagrafica();
047700090409             opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_GET;
047800090414
047900090409           ELSEIF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_GET;
048000090414
048100090409             esito = GetAnagrafica();
048200090414
048300090409             IF esito = ESITO_WARNING_ENTER;
048400090409               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_CHK;
048500090414             ELSEIF esito = ESITO_WARNING_F4;
048600090414               opCodeNxt = SOCIETA_OPCODE_RICHIESTA;
048700090414               opCodePrv = SOCIETA_OPCODE_ANAGRAFICA_SET;
048800090409             ELSEIF esito = ESITO_WARNING_F5;
048900090409               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_INZ;
049000090409             ELSEIF esito = ESITO_WARNING_F6;
049100090409               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_RUN;
049200090409             ELSEIF esito = ESITO_WARNING_F8;
049300090409               opCodeNxt = SOCIETA_OPCODE_SUCCESSIVO;
049400090409             ELSEIF esito = ESITO_WARNING_F12;
049500090409               opCodeNxt = SOCIETA_OPCODE_RITORNO;
049600090409             ELSEIF esito = ESITO_WARNING_ROLLUP;
049700090416               RESET dsp_rtnCsrRow;
049800090416               RESET dsp_rtnCsrCol;
049900090416               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_SET;
050000090409               pannello = 2;
050100090409             ELSEIF esito = ESITO_WARNING_ROLLDOWN;
050200090416               RESET dsp_rtnCsrRow;
050300090416               RESET dsp_rtnCsrCol;
050400090416               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_SET;
050500090409               pannello = 1;
050600090409             ELSE;
050700090409               opCodeNxt = SOCIETA_OPCODE_FINE;
050800090409               piRpyOpCode = opCodeNxt;
050900090409             ENDIF;
051000090414
051100090409           ELSEIF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_CHK;
051200090414
051300090414             ChkAnagrafica();
051400090414             opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_SET;
051500090414
051600090409           ELSEIF opCodeCur = SOCIETA_OPCODE_ANAGRAFICA_RUN;
051700090414
051800090409             IF ChkAnagrafica() = ESITO_OK;
051900090409               esito = RunAnagrafica();
052000090409               IF esito < ESITO_OK;
052100090409                 EXEC SQL
052200090409                   ROLLBACK
052300090409                 ;
052400090409                 piRpyOpCode = SOCIETA_OPCODE_ERRORE;
052500090409                 piRpyIdMsg = esito;
052600090409               ELSE;
052700090409                 EXEC SQL
052800090409                   COMMIT
052900090409                 ;
053000090409                 piRpyOpCode = SOCIETA_OPCODE_DONE;
053100090415                 IF esito = ESITO_WARNING_PERIODINONCONTIGUI;
053200090415                   RESET qusec;
053300090415                   textString = 'ATTENZIONE: le anagrafiche della +
053400090415                                 società c.f. ' + dsp_codFiscale + ' id ' +
053500090415                                 dsp_idSocieta + ' non sono contigue dal +
053600090415                                 01.01.0001 al 31.12.9999. +
053700090415                                 Aggiornamento eseguito.';
053800090415                   DisplayLongText( textString
053900090415                                  : %SIZE(textString)
054000090415                                  : *BLANK
054100090415                                  : *BLANK
054200090415                                  : qusec
054300090415                                  );
054400090415                 ENDIF;
054500090415               ENDIF;
054600090409               opCodeNxt = SOCIETA_OPCODE_FINE;
054700090409               piRpyOpCode = SOCIETA_OPCODE_DONE;
054800090409             ELSE;
054900090409               opCodeNxt = SOCIETA_OPCODE_ANAGRAFICA_SET;
055000090409             ENDIF;
055100090414
055200090414           // Gestione della finestra di fine attività.
055300090414
055400090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_INZ;
055500090414
055600090414             InzFineAttivita();
055700090414             opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_CHK;
055800090414
055900090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_SET;
056000090414
056100090414             SetFineAttivita();
056200090414             opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_PUT;
056300090414
056400090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_PUT;
056500090414
056600090414             PutFineAttivita();
056700090414             opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_GET;
056800090414
056900090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_GET;
057000090414
057100090414             esito = GetFineAttivita();
057200090414
057300090414             IF esito = ESITO_WARNING_ENTER;
057400090414               opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_CHK;
057500090414             ELSEIF esito = ESITO_WARNING_F5;
057600090414               opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_INZ;
057700090414             ELSEIF esito = ESITO_WARNING_F6;
057800090414               opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_RUN;
057900090414             ELSEIF esito = ESITO_WARNING_F8;
058000090414               opCodeNxt = SOCIETA_OPCODE_SUCCESSIVO;
058100090414             ELSEIF esito = ESITO_WARNING_F12;
058200090414               opCodeNxt = SOCIETA_OPCODE_RITORNO;
058300090414             ELSE;
058400090414               opCodeNxt = SOCIETA_OPCODE_FINE;
058500090414               piRpyOpCode = opCodeNxt;
058600090414             ENDIF;
058700090414
058800090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_CHK;
058900090414
059000090414             ChkFineAttivita();
059100090414             opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_SET;
059200090414
059300090414           ELSEIF opCodeCur = SOCIETA_OPCODE_FINEATTIVITA_RUN;
059400090414
059500090414             IF ChkFineAttivita() = ESITO_OK;
059600090414               esito = RunFineAttivita();
059700090414               IF esito < ESITO_OK;
059800090414                 EXEC SQL
059900090414                   ROLLBACK
060000090414                 ;
060100090414                 piRpyOpCode = SOCIETA_OPCODE_ERRORE;
060200090414                 piRpyIdMsg = esito;
060300090414               ELSE;
060400090414                 EXEC SQL
060500090414                   COMMIT
060600090414                 ;
060700090414                 piRpyOpCode = SOCIETA_OPCODE_DONE;
060800090414               ENDIF;
060900090414               opCodeNxt = SOCIETA_OPCODE_FINE;
061000090414               piRpyOpCode = SOCIETA_OPCODE_DONE;
061100090414             ELSE;
061200090414               opCodeNxt = SOCIETA_OPCODE_FINEATTIVITA_SET;
061300090414             ENDIF;
061400090414
061500090414           // Gestione uscita dal programma.
061600090414
061700090414           ELSEIF opCodeCur = SOCIETA_OPCODE_RICHIESTA;
061800090414
061900090414             RunRichiesta();
062000090414             opCodeNxt = opCodePrv;
062100090414
062200090409           ELSEIF opCodeCur = SOCIETA_OPCODE_SUCCESSIVO;
062300090414
062400090409             piRpyOpCode = opCodeCur;
062500090409             LEAVE;
062600090414
062700090409           ELSEIF opCodeCur = SOCIETA_OPCODE_RITORNO;
062800090414
062900090409             piRpyOpCode = opCodeCur;
063000090409             LEAVE;
063100090414
063200090409           ELSEIF opCodeCur = SOCIETA_OPCODE_FINE;
063300090414
063400090409             LEAVE;
063500090414
063600090409           ELSE;
063700090414
063800090409             opCodeNxt = SOCIETA_OPCODE_FINE;
063900090409             piRpyOpCode = opCodeNxt;
064000090414
064100090409           ENDIF;
064200090409
064300090409         ENDDO;
064400090414
064500090414         WRITE frcDta;
064600090420
064700090420         IF piRpyFormato = 'TNSD4040O';
064800090420           %SUBST(piRpyData : 1 : piRpyDataSize) = tnsd4040o;
064900090420         ENDIF;
065000090409
065100090409       ELSEIF piRqsOpCode = SOCIETA_OPCODE_INIT;
065200090409
065300090409         IF piRqsFormato = 'KPJBA';
065400090409           kpjba = piRqsData;
065500090409         ENDIF;
065600090420
065700090420         xSoc001Ds.xscSoc = kpjba.kpjbu;
065800090420
065900090420         Proj_Societa( 'SOC001'
066000090420                     : xSoc001Ds_lavoro.xscSoc
066100090420                     : *ZERO
066200090420                     : *BLANK
066300090420                     : esitoProj
066400090420                     : xSoc001Ds_lavoro
066500090420                     : kpjba
066600090420                     );
066700090420
066800090420         IF esitoProj = *ON;
066900090420           RETURN;
067000090420         ENDIF;
067100090420
067200090409       ELSEIF piRqsOpCode = SOCIETA_OPCODE_FINALIZE;
067300090409
067400090409         *INLR = *ON;
067500090409
067600090409       ENDIF;
067700090414
067800090409       RETURN;
067900090409
068000090415       //***********************************************************************
068100090415       //
068200090415       //
068300090415       //
068400090415       //***********************************************************************
068500090415
068600090415       BEGSR *INZSR;
068700090415
068800090415         CLEAR qusec;
068900090415         qusbprv = %SIZE(qusec);
069000090415         dsp_da_codFisc = DSPATR_NONPROTECT_UNDERSCORE;
069100090415         dsp_da_valDatI = DSPATR_NONPROTECT_UNDERSCORE;
069200090415         dsp_da_valDatF = DSPATR_NONPROTECT_UNDERSCORE;
069300090415
069400090415       ENDSR;
069500090415
069600090409      /END-FREE
069700090409
069800090409
069900090409     P*--------------------------------------------------
070000090409     P* Procedure name: InzAnagrafica
070100090409     P* Purpose:        Inizializza finestra elenco.
070200090409     P* Returns:        Esito.
070300090409     P*--------------------------------------------------
070400090409     P InzAnagrafica   B
070500090409     D InzAnagrafica   PI            10I 0
070600090409
070700090409      /FREE
070800090409
070900090409       RESET dspInd;
071000090409       RESET testata;
071100090409       RESET societa1;
071200090409       RESET societa2;
071300090409
071400090409       IF tnsd4040i.idRiga <> *ZERO;
071500090409
071600090409         EXEC SQL
071700090409           SELECT AZSOC00F.ID_SOCIETA
071800090409                , AZSOC00F.VALIDITA_DATA_INIZIO
071900090409                , AZSOC00F.VALIDITA_DATA_FINE
072000090409                , AZSOC00F.RAGIONE_SOCIALE
072100090409                , AZSOC00F.RAGIONE_SOCIALE_BREVE
072200090409                , AZSOC00F.ATTIVITA_DATA_INIZIO
072300090409                , IFNULL(CHAR(AZSOC00F.ATTIVITA_DATA_FINE , EUR) , 'Attiva')
072400090409                , AZSOC00F.ATTIVITA_ATECO2007
072500090409                , AZSOC00F.CODICE_FISCALE
072600090409                , IFNULL(AZSOC00F.PARTITA_IVA , '')
072700090409                , AZSOC00F.NATURA_GIURIDICA
072800090409                , CASE AZSOC00F.SOCIO_UNICO
072900090409                  WHEN '0' THEN 'N'
073000090409                  WHEN '1' THEN 'S'
073100090409                  ELSE ' '
073200090409                  END
073300090409                , AZSOC00F.CAPITALE_SOCIALE_IMPORTO
073400090409                , AZSOC00F.CAPITALE_SOCIALE_TIPO
073500090409                , CASE AZSOC00F.CAPITALE_SOCIALE_IV
073600090409                  WHEN '0' THEN 'N'
073700090409                  WHEN '1' THEN 'S'
073800090409                  ELSE ' '
073900090409                  END
074000090409                , AZSOC00F.REA_LUOGO
074100090409                , AZSOC00F.REA_NUMERO
074200090409                , AZSOC00F.REGISTRO_IMPRESE_LUOGO
074300090409                , AZSOC00F.REGISTRO_IMPRESE_DATA
074400090409                , AZSOC00F.SEDE_LEGALE_NAZIONE
074500090409                , AZSOC00F.SEDE_LEGALE_PROVINCIA
074600090409                , AZSOC00F.SEDE_LEGALE_COMUNE
074700090409                , AZSOC00F.SEDE_LEGALE_CAP
074800090409                , AZSOC00F.SEDE_LEGALE_INDIRIZZO
074900090409                , AZSOC00F.SEDE_LEGALE_TELEFONO
075000090409                , AZSOC00F.SEDE_LEGALE_TELEFAX
075100090409                , AZSOC00F.SEDE_LEGALE_COMUNE_CATASTO
075200090409                , AZSOC00F.SEDE_AMMIN_NAZIONE
075300090409                , AZSOC00F.SEDE_AMMIN_PROVINCIA
075400090409                , AZSOC00F.SEDE_AMMIN_COMUNE
075500090409                , AZSOC00F.SEDE_AMMIN_CAP
075600090409                , AZSOC00F.SEDE_AMMIN_INDIRIZZO
075700090409                , AZSOC00F.SEDE_AMMIN_TELEFONO
075800090409                , AZSOC00F.SEDE_AMMIN_TELEFAX
075900090409                , AZSOC00F.SEDE_AMMIN_COMUNE_CATASTO
076000090409                , IFNULL(AZSOC00F.CODICE_FISCALE_INCORPORANTE , '')
076100091119                , IFNULL(AZSOC00F.CASELLA_POSTALE , '')
076200090409             INTO :dsp_idSocieta
076300090409                , :dsp_valDatIniz
076400090409                , :dsp_valDatFine
076500090409                , :dsp_ragSociale
076600090409                , :dsp_ragSocBrev
076700090409                , :dsp_attDatIniz
076800090409                , :dsp_attDatFine
076900090409                , :dsp_ateco2007
077000090409                , :dsp_codFiscale
077100090409                , :dsp_partitaIva
077200090409                , :dsp_naturaGiur
077300090409                , :dsp_socioUnico
077400090409                , :dsp_capSocImp
077500090409                , :dsp_capSocTip
077600090409                , :dsp_capSocIv
077700090409                , :dsp_reaLuogo
077800090409                , :dsp_reaNumero
077900090409                , :dsp_rimLuogo
078000090409                , :dsp_rimDatIscr
078100090409                , :dsp_sedLegNaz
078200090409                , :dsp_sedLegPro
078300090409                , :dsp_sedLegLoc
078400090409                , :dsp_sedLegCap
078500090409                , :dsp_sedLegInd
078600090409                , :dsp_sedLegTel
078700090409                , :dsp_sedLegFax
078800090409                , :dsp_sedLegCat
078900090409                , :dsp_sedAmmNaz
079000090409                , :dsp_sedAmmPro
079100090409                , :dsp_sedAmmLoc
079200090409                , :dsp_sedAmmCap
079300090409                , :dsp_sedAmmInd
079400090409                , :dsp_sedAmmTel
079500090409                , :dsp_sedAmmFax
079600090409                , :dsp_sedAmmCat
079700090409                , :dsp_codFiscInc
079800091119                , :dsp_casellaPt
079900090409             FROM AZSOC00F
080000090409             WHERE AZSOC00F.ID_RIGA = :tnsd4040i.idRiga
080100090409         ;
080200090409
080300090409         IF sqlCode < *ZERO;
080400090409           RETURN sqlCode;
080500090409         ENDIF;
080600090410
080700090410       ENDIF;
080800090409
080900090410       // Inizializzazioni specifiche per operazione.
081000090410
081100090409       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_MODIFICARE;
081200090410
081300090409         %SUBST(dsp_subTitolo : 36) = 'Modifica';
081400090415         dsp_da_codFisc = DSPATR_PROTECT_NORMAL;
081500090410
081600090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE;
081700090410
081800090409         %SUBST(dsp_subTitolo : 32) = 'Visualizzazione';
081900090410
082000090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVAANAGRAFICA;
082100090410
082200090409         %SUBST(dsp_subTitolo : 32) = 'Nuova anagrafica';
082300090410
082400090409         IF dsp_valDatFine < *HIVAL;
082500090409           dsp_valDatIniz = dsp_valDatFine + %DAYS(1);
082600090409         ELSE;
082700090409           dsp_valDatIniz = *LOVAL;
082800090409         ENDIF;
082900090410
083000090409         dsp_valDatFine = *HIVAL;
083100090415         dsp_da_valDatF = DSPATR_PROTECT_NORMAL;
083200090410
083300090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
083400090410
083500090409         %SUBST(dsp_subTitolo : 28) = 'Cancellazione anagrafica';
083600090410
083700090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVASOCIETA;
083800090410
083900090409         %SUBST(dsp_subTitolo : 33) = 'Nuova società';
084000090409         dsp_valDatFine = *HIVAL;
084100090409         dsp_sedLegTel = '+39';
084200090409         dsp_sedLegFax = '+39';
084300090410
084400090410         RESET ana007Ds;
084500090410         kpjba.kpjbu = ana007Ds;
084600090410         Proj_RicercaSocieta( kpjba );
084700090410         ana007Ds = kpjba.kpjbu;
084800090410         IF ana007Ds.ret007 = PROJ_ENTER;
084900090410           Proj_Societa( 'SOC002'
085000090410                       : ana007Ds.soc007
085100090410                       : *ZERO
085200090410                       : *BLANK
085300090410                       : esitoProj
085400090410                       : xSoc001Ds
085500090410                       : kpjba
085600090410                       );
085700090410           IF esitoProj = *OFF;
085800090410             dsp_idSocieta = xSoc001Ds.xscSoc;
085900090410             dsp_codFiscale = xSoc001Ds.xscCdf;
086000090410             dsp_ragSociale = xSoc001Ds.xscRgs;
086100090410             dsp_ragSocBrev = xSoc001Ds.xscDsi;
086200090410             dsp_sedLegInd = xSoc001Ds.xscVia;
086300090410             dsp_sedLegLoc = xSoc001Ds.xscCit;
086400090410             dsp_sedLegCap = xSoc001Ds.xscCap;
086500090410             dsp_sedLegPro = xSoc001Ds.xscPrv;
086600090410             dsp_sedLegTel = xSoc001Ds.xscTel;
086700090410             dsp_sedLegFax = xSoc001Ds.xscFax;
086800090410             dsp_sedLegFax = xSoc001Ds.xscFax;
086900090410             dsp_sedAmmInd = xSoc001Ds.xscVsc;
087000090410             dsp_sedAmmLoc = xSoc001Ds.xscCsc;
087100090410             dsp_sedAmmCap = xSoc001Ds.xscCas;
087200090410             dsp_sedAmmPro = xSoc001Ds.xscPrs;
087300090410             dsp_sedAmmTel = xSoc001Ds.xscTls;
087400090410             IF xSoc001Ds.xscTel = xSoc001Ds.xscTls;
087500090410               dsp_sedAmmFax = xSoc001Ds.xscFax;
087600090410             ENDIF;
087700090410             dsp_naturaGiur = xSoc001Ds.xscNcr;
087800090410             dsp_partitaIva = 'IT' + xSoc001Ds.xscIva;
087900090410             IF xSoc001Ds.xscDti > *LOVAL;
088000090410               dsp_rimDatIscr = xSoc001Ds.xscDti;
088100090410               dsp_rimLuogo = xSoc001Ds.xscCti;
088200090410             ENDIF;
088300090410             IF xSoc001Ds.xscCct <> *BLANK;
088400090410               dsp_reaNumero = xSoc001Ds.xscCct;
088500090410               dsp_reaLuogo = xSoc001Ds.xscCti;
088600090410             ENDIF;
088700090410             IF xSoc001Ds.xscAtt <> *BLANK;
088800090410               dsp_ateco2007 = %SUBST(xSoc001Ds.xscAtt : 1 : 2)
088900090410                             + '.'
089000090410                             + %SUBST(xSoc001Ds.xscAtt : 3 : 2)
089100090410                             + '.'
089200090410                             + %SUBST(xSoc001Ds.xscAtt : 5 : 2);
089300090410             ENDIF;
089400090410           ENDIF;
089500090410         ENDIF;
089600090410
089700090409       ENDIF;
089800090409
089900090409       RETURN ESITO_OK;
090000090409
090100090409      /END-FREE
090200090409     P InzAnagrafica   E
090300090409
090400090409
090500090409     P*--------------------------------------------------
090600090409     P* Procedure name: SetAnagrafica
090700090409     P* Purpose:        Imposta dati finestra elenco.
090800090409     P* Returns:        Esito.
090900090409     P*--------------------------------------------------
091000090409     P SetAnagrafica   B
091100090409     D SetAnagrafica   PI            10I 0
091200090409
091300090409      /FREE
091400090409
091500090416       dsp_csrLocRow = dsp_rtnCsrRow;
091600090416       dsp_csrLocCol = dsp_rtnCsrCol;
091700090416
091800090409       RETURN ESITO_OK;
091900090409
092000090409      /END-FREE
092100090409     P SetAnagrafica   E
092200090409
092300090409
092400090409     P*--------------------------------------------------
092500090409     P* Procedure name: PutAnagrafica
092600090409     P* Purpose:        Emette finestra elenco.
092700090409     P* Returns:        Esito.
092800090409     P*--------------------------------------------------
092900090409     P PutAnagrafica   B
093000090409     D PutAnagrafica   PI            10I 0
093100090409
093200090409      /FREE
093300090415
093400090415       WRITE testata;
093500090409       WRITE tasti;
093600090415
093700090415       IF dspInd.erroreGenerico;
093800090415         dspInd.erroreGenerico = *OFF;
093900090415         IF pannello = 2;
094000090415           WRITE societa2;
094100090415         ELSE;
094200090415           WRITE societa1;
094300090415         ENDIF;
094400090415         dspInd.erroreGenerico = *ON;
094500090415       ENDIF;
094600090409
094700090409       IF pannello = 2;
094800090409         WRITE societa2;
094900090409       ELSE;
095000090409         WRITE societa1;
095100090409       ENDIF;
095200090409
095300090409       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE
095400090409       OR tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
095500090409         WRITE protect;
095600090409       ENDIF;
095700090409
095800090409       RETURN ESITO_OK;
095900090409
096000090409      /END-FREE
096100090409     P PutAnagrafica   E
096200090409
096300090409
096400090409     P*--------------------------------------------------
096500090409     P* Procedure name: GetAnagrafica
096600090409     P* Purpose:        Legge la finestra elenco.
096700090409     P* Returns:        Esito.
096800090409     P*--------------------------------------------------
096900090409     P GetAnagrafica   B
097000090409     D GetAnagrafica   PI            10I 0
097100090409
097200090409      /FREE
097300090409
097400090409       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE
097500090409       OR tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
097600090409         READ protect;
097700090409       ELSE;
097800090409         IF pannello = 2;
097900090409           READ societa2;
098000090409         ELSE;
098100090409           READ societa1;
098200090409         ENDIF;
098300090409       ENDIF;
098400090409
098500090409       RETURN dspInf.dsp_aid_num;
098600090409
098700090409      /END-FREE
098800090409     P GetAnagrafica   E
098900090409
099000090409
099100090409     P*--------------------------------------------------
099200090409     P* Procedure name: ChkAnagrafica
099300090409     P* Purpose:        Controlla i dati inseriti nella finestra elenco.
099400090409     P* Returns:        Esito.
099500090409     P*--------------------------------------------------
099600090409     P ChkAnagrafica   B
099700090409     D ChkAnagrafica   PI            10I 0
099800090415
099900090415      /FREE
100000090409
100100090409       RESET dspInd.erroreGenerico;
100200090416       RESET dspInd.errMsgCodiceFiscaleGiaUsato;
100300090416       RESET dspInd.errMsgDataValiditaSequenza;
100400090416       RESET dspInd.errMsgDataValiditaInizioSovrapposta;
100500090416       RESET dspInd.errMsgDataValiditaFineSovrapposta;
100600090416       RESET dspInd.errMsgIdSocieta;
100700090416       RESET dspInd.errMsgAteco2007;
100800090416       RESET dspInd.errMsgCatastoSedeLegale;
100900090416       RESET dspInd.errMsgCatastoSedeAmministrativa;
101000090416       RESET dspInd.errMsgProvinciaSedeLegale;
101100090416       RESET dspInd.errMsgProvinciaSedeAmministrativa;
101200090416       RESET dspInd.errMsgNazioneSedeLegale;
101300090416       RESET dspInd.errMsgNazioneSedeAmministrativa;
101400090416       RESET dspInd.errMsgNaturaGiuridica;
101500090416       RESET dspInd.errMsgPartitaIvaObbligatoria;
101600090416       RESET dspInd.errMsgPartitaIvaErrata;
101700090420       RESET dspInd.errMsgTipoCapitaleSociale;
101800090429       RESET dspInd.errMsgImportoCapitaleSociale;
101900090416
102000090415       // Codice fiscale già usato.
102100090415
102200090415       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVASOCIETA;
102300090415         EXEC SQL
102400090415           SELECT COUNT(*)
102500090415             INTO :count
102600090415             FROM AZSOC00F
102700090415             WHERE AZSOC00F.CODICE_FISCALE = :dsp_codFiscale
102800090415         ;
102900090415         IF sqlCode < *ZERO;
103000090415           DUMP(A);
103100090415         ELSEIF sqlCode <> 100 AND count > *ZERO;
103200090415           dspInd.erroreGenerico = *ON;
103300090416           dspInd.errMsgCodiceFiscaleGiaUsato = *ON;
103400090415         ENDIF;
103500090415       ENDIF;
103600090415
103700090415       // Date in sequenza.
103800090415
103900090416       IF dsp_valDatFine = *LOVAL;
104000090416         dsp_valDatFine = *HIVAL;
104100090416       ENDIF;
104200090416
104300090415       IF dsp_valDatIniz > dsp_valDatFine;
104400090415         dspInd.erroreGenerico = *ON;
104500090416         dspInd.errMsgDataValiditaSequenza = *ON;
104600090415       ENDIF;
104700090415
104800090415       // Periodo sovrapposto.
104900090415
105000090415       EXEC SQL
105100090415         SELECT COUNT(*)
105200090415           INTO :count
105300090415           FROM AZSOC00F
105400090415           WHERE CODICE_FISCALE = :dsp_codFiscale
105500090415             AND ID_RIGA <> :tnsd4040i.idRiga
105600090415             AND :dsp_valDatIniz BETWEEN VALIDITA_DATA_INIZIO
105700090415                                     AND VALIDITA_DATA_FINE
105800090415           GROUP BY CODICE_FISCALE
105900090415       ;
106000090415
106100090415       IF sqlCode < *ZERO;
106200090415         DUMP(A);
106300090415       ELSEIF sqlCode <> 100 AND count > *ZERO;
106400090415         dspInd.erroreGenerico = *ON;
106500090416         dspInd.errMsgDataValiditaInizioSovrapposta = *ON;
106600090415       ENDIF;
106700090415
106800090415       EXEC SQL
106900090415         SELECT COUNT(*)
107000090415           INTO :count
107100090415           FROM AZSOC00F
107200090415           WHERE CODICE_FISCALE = :dsp_codFiscale
107300090415             AND ID_RIGA <> :tnsd4040i.idRiga
107400090415             AND :dsp_valDatFine BETWEEN VALIDITA_DATA_INIZIO
107500090415                                     AND VALIDITA_DATA_FINE
107600090415           GROUP BY CODICE_FISCALE
107700090415       ;
107800090415
107900090415       IF sqlCode < *ZERO;
108000090415         DUMP(A);
108100090415       ELSEIF sqlCode <> 100 AND count > *ZERO;
108200090415         dspInd.erroreGenerico = *ON;
108300090416         dspInd.errMsgDataValiditaFineSovrapposta = *ON;
108400090415       ENDIF;
108500090416
108600090416       // Codice catastale comune sede legale.
108700090416
108800090416       RESET dsp_desLegCat;
108900090416       RESET tibs02ds;
109000090416       tibs02ds.t02Mod = 'C';
109100090416       tibs02ds.t02Cod = 'CTT';
109200090416       tibs02ds.t02Ke1 = dsp_sedLegCat;
109300090416       TNTBE_RicercaControllo( kpjba : tibs02ds );
109400090416       IF tibs02ds.t02Err <> *BLANK;
109500090416         dspInd.erroreGenerico = *ON;
109600090416         dspInd.errMsgCatastoSedeLegale = *ON;
109700090416       ELSE;
109800090416         dCtt = tibs02ds.t02Uni;
109900090416         dsp_desLegCat = %TRIMR(dCtt.comune) + ' ' + dCtt.provincia;
110000090416       ENDIF;
110100090416
110200090416       // Codice catastale comune sede amministrativa.
110300090416
110400090416       RESET dsp_desAmmCat;
110500090416       RESET tibs02ds;
110600090416       tibs02ds.t02Mod = 'C';
110700090416       tibs02ds.t02Cod = 'CTT';
110800090416       tibs02ds.t02Ke1 = dsp_sedAmmCat;
110900090416       TNTBE_RicercaControllo( kpjba : tibs02ds );
111000090416       IF tibs02ds.t02Err <> *BLANK;
111100090416         dspInd.erroreGenerico = *ON;
111200090416         dspInd.errMsgCatastoSedeAmministrativa = *ON;
111300090416       ELSE;
111400090416         dCtt = tibs02ds.t02Uni;
111500090416         dsp_desAmmCat = %TRIMR(dCtt.comune) + ' ' + dCtt.provincia;
111600090416       ENDIF;
111700090415
111800090414       // ATECO 2007.
111900090414
112000090414       RESET dsp_desAteco;
112100090414       RESET tibs02ds;
112200090414       tibs02ds.t02Mod = 'C';
112300090414       tibs02ds.t02Cod = 'AE7';
112400090420       tibs02ds.t02Ke1 = dsp_ateco2007;
112500090414       TNTBE_RicercaControllo( kpjba : tibs02ds );
112600090414       dsp_desAteco = tibs02ds.t02Uni;
112700090414       IF tibs02ds.t02Err <> *BLANK;
112800090414         dspInd.erroreGenerico = *ON;
112900090416         dspInd.errMsgAteco2007 = *ON;
113000090414       ENDIF;
113100090429
113200090429       // Importo capitale sociale.
113300090420
113400090429       IF dsp_capSocImp <= *ZERO;
113500090429         dspInd.erroreGenerico = *ON;
113600090429         dspInd.errMsgImportoCapitaleSociale = *ON;
113700090429       ENDIF;
113800090429
113900090420       // Tipo capitale sociale.
114000090420
114100090420       RESET dsp_desCapSocT;
114200090420       RESET tibs02ds;
114300090420       tibs02ds.t02Mod = 'C';
114400090420       tibs02ds.t02Cod = '003';
114500090420       tibs02ds.t02Ke1 = dsp_capSocTip;
114600090420       TNTBE_RicercaControllo( kpjba : tibs02ds );
114700090420       dsp_desCapSocT = tibs02ds.t02Uni;
114800090420       IF tibs02ds.t02Err <> *BLANK;
114900090420         dspInd.erroreGenerico = *ON;
115000090420         dspInd.errMsgTipoCapitaleSociale = *ON;
115100090420       ENDIF;
115200090415
115300090415       // Id società Proj.
115400090409
115500090415       RESET dsp_desSocieta;
115600090415       RESET esitoProj;
115700090415
115800090415       Proj_Societa( 'SOC002'
115900090415                   : dsp_idSocieta
116000090415                   : *ZERO
116100090415                   : *BLANK
116200090415                   : esitoProj
116300090415                   : xSoc001Ds
116400090415                   : kpjba
116500090415                   );
116600090415
116700090415       IF esitoProj = *ON OR dsp_idSocieta = *BLANK;
116800090415         dspInd.erroreGenerico = *ON;
116900090416         dspInd.errMsgIdSocieta = *ON;
117000090415       ELSE;
117100090415         dsp_desSocieta = xSoc001Ds.xscDsi;
117200090415       ENDIF;
117300090416
117400090416       // Nazione sede legale.
117500090416
117600090416       RESET dsp_desLegNaz;
117700090416       RESET ds15_sedeLegale;
117800090416
117900090416       EXEC SQL
118000090416         SELECT TBLUNI
118100090416           INTO :ds15_sedeLegale.tblUni
118200090416           FROM TABEL00F
118300090416           WHERE TBLKUT = 1
118400090416             AND TBLCOD = '15'
118500090416             AND TBLKEY = :dsp_sedLegNaz
118600090416             AND TBLFLG = ''
118700090416       ;
118800090415
118900090416       IF sqlCode = 100;
119000090416         dspInd.erroreGenerico = *ON;
119100090416         dspInd.errMsgNazioneSedeLegale = *ON;
119200090416       ELSEIF sqlCode < *ZERO;
119300090416         DUMP(A);
119400090416       ELSE;
119500090416         dsp_desLegNaz = ds15_sedeLegale.§15Des;
119600090416       ENDIF;
119700090416
119800090416       // Provincia sede legale.
119900090416
120000090416       RESET dsp_desLegPro;
120100090416       RESET dsPR_sedeLegale;
120200090416
120300090416       IF ds15_sedeLegale.§15Ita = 'I' OR dsp_sedLegPro <> *BLANK;
120400090416
120500090416         EXEC SQL
120600090416           SELECT TBLUNI
120700090416             INTO :dsPR_sedeLegale.tblUni
120800090416             FROM TABEL00F
120900090416             WHERE TBLKUT = 1
121000090416               AND TBLCOD = 'PR'
121100090416               AND TBLKEY = :dsp_sedLegPro
121200090416               AND TBLFLG = ''
121300090416         ;
121400090416
121500090416         IF sqlCode = 100;
121600090416           dspInd.erroreGenerico = *ON;
121700090416           dspInd.errMsgProvinciaSedeLegale = *ON;
121800090416         ELSEIF sqlCode < *ZERO;
121900090416           DUMP(A);
122000090416         ELSE;
122100090416           dsp_desLegPro = dsPR_sedeLegale.§PRDes;
122200090416         ENDIF;
122300090416
122400090416       ENDIF;
122500090416
122600090416       // Nazione sede amministrativa.
122700090416
122800090416       RESET dsp_desAmmNaz;
122900090416       RESET ds15_sedeAmministrativa;
123000090416
123100090416       EXEC SQL
123200090416         SELECT TBLUNI
123300090416           INTO :ds15_sedeAmministrativa.tblUni
123400090416           FROM TABEL00F
123500090416           WHERE TBLKUT = 1
123600090416             AND TBLCOD = '15'
123700090416             AND TBLKEY = :dsp_sedAmmNaz
123800090416             AND TBLFLG = ''
123900090416       ;
124000090416
124100090416       IF sqlCode = 100;
124200090416         dspInd.erroreGenerico = *ON;
124300090416         dspInd.errMsgNazioneSedeAmministrativa = *ON;
124400090416       ELSEIF sqlCode < *ZERO;
124500090416         DUMP(A);
124600090416       ELSE;
124700090416         dsp_desAmmNaz = ds15_sedeAmministrativa.§15Des;
124800090416       ENDIF;
124900090416
125000090416       // Provincia sede amministrativa.
125100090416
125200090416       RESET dsp_desAmmPro;
125300090416       RESET dsPR_sedeAmministrativa;
125400090416
125500090416       IF ds15_sedeAmministrativa.§15Ita = 'I' OR dsp_sedAmmPro <> *BLANK;
125600090416
125700090416         EXEC SQL
125800090416           SELECT TBLUNI
125900090416             INTO :dsPR_sedeAmministrativa.tblUni
126000090416             FROM TABEL00F
126100090416             WHERE TBLKUT = 1
126200090416               AND TBLCOD = 'PR'
126300090416               AND TBLKEY = :dsp_sedAmmPro
126400090416               AND TBLFLG = ''
126500090416         ;
126600090416
126700090416         IF sqlCode = 100;
126800090416           dspInd.erroreGenerico = *ON;
126900090416           dspInd.errMsgProvinciaSedeAmministrativa = *ON;
127000090416         ELSEIF sqlCode < *ZERO;
127100090416           DUMP(A);
127200090416         ELSE;
127300090416           dsp_desAmmPro = dsPR_sedeAmministrativa.§PRDes;
127400090416         ENDIF;
127500090416
127600090416       ENDIF;
127700090416
127800090416       // Natura giuridica.
127900090416
128000090416       RESET dsp_desNatGiur;
128100090416       RESET angG16Ds;
128200090416       RESET xAtbDs;
128300090416       xAtbDs.xtbCod = 'G16';
128400090420       xAtbDs.xtbAzi = xSoc001Ds_lavoro.xscSoc;
128500090416       xAtbDs.xtbKey = '0000000000000' + dsp_naturaGiur;
128600090416
128700090416       Proj_Tabella( xAtbDs );
128800090416
128900090416       IF xAtbDs.xtbErr = *OFF;
129000090416         angG16Ds = xAtbDs.xtbUni;
129100090416         dsp_desNatGiur = angG16Ds.de1G16;
129200090416       ELSE;
129300090416         dspInd.erroreGenerico = *ON;
129400090416         dspInd.errMsgNaturaGiuridica = *ON;
129500090416       ENDIF;
129600090416
129700090416       // Partita IVA:
129800090416
129900090416       IF dsp_partitaIva = *BLANK;
130000090416         IF angG16Ds.pIvaG16 = *ON;
130100090416           dspInd.erroreGenerico = *ON;
130200090416           dspInd.errMsgPartitaIvaObbligatoria = *ON;
130300090416           dsp_partitaIva = ds15_sedeLegale.§15Iso;
130400090416         ENDIF;
130500090416       ELSE;
130600090416         IF %SUBST(dsp_partitaIva : 1 : 2) = ITALIA;
130700090416           partitaIvaProj = %SUBST(dsp_partitaIva : 3);
130800090416         ELSE;
130900090416           partitaIvaProj = dsp_partitaIva;
131000090416         ENDIF;
131100090416         Proj_PartitaIVA( partitaIvaProj
131200090416                        : esitoPartitaIva
131300090416                        : dsp_idSocieta
131400090416                        );
131500090416         IF esitoPartitaIva < ESITO_OK;
131600090416           dspInd.erroreGenerico = *ON;
131700090416           dspInd.errMsgPartitaIvaErrata = *ON;
131800090416         ENDIF;
131900090416       ENDIF;
132000090416
132100090416       // Restituisco la presenza di errori;
132200090416
132300090416       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE
132400090416       OR tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
132500090416         RETURN ESITO_OK;
132600090416       ENDIF;
132700090416
132800090416       IF dspInd.erroreGenerico;
132900090415         RETURN ESITO_ERROR;
133000090415       ENDIF;
133100090415
133200090415       RETURN ESITO_OK;
133300090409
133400090409      /END-FREE
133500090409     P ChkAnagrafica   E
133600090409
133700090409
133800090409     P*--------------------------------------------------
133900090409     P* Procedure name: RunAnagrafica
134000090409     P* Purpose:        Elabora i dati inseriti nella finestra elenco.
134100090409     P* Returns:        Esito.
134200090409     P*--------------------------------------------------
134300090409     P RunAnagrafica   B
134400090409     D RunAnagrafica   PI            10I 0
134500090415
134600090415     D giorniCodiceFiscale...
134700090415     D                 S             10I 0
134800090415     D giorniSocieta...
134900090415     D                 S             10I 0
135000090409
135100090409      /FREE
135200090416
135300090416       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_VISUALIZZARE;
135400090416         RETURN ESITO_OK;
135500090416       ENDIF;
135600090409
135700090409       IF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_MODIFICARE;
135800090409
135900090409         EXEC SQL
136000090409           UPDATE AZSOC00F
136100090409             SET AZSOC00F.ID_SOCIETA = :dsp_idSocieta
136200090409               , AZSOC00F.VALIDITA_DATA_INIZIO = :dsp_valDatIniz
136300090409               , AZSOC00F.VALIDITA_DATA_FINE = DATE(:dsp_valDatFine)
136400090409               , AZSOC00F.RAGIONE_SOCIALE = RTRIM(:dsp_ragSociale)
136500090409               , AZSOC00F.RAGIONE_SOCIALE_BREVE = RTRIM(:dsp_ragSocBrev )
136600090409               , AZSOC00F.ATTIVITA_ATECO2007 = :dsp_ateco2007
136700090409               , AZSOC00F.CODICE_FISCALE = :dsp_codFiscale
136800090409               , AZSOC00F.PARTITA_IVA = CASE :dsp_partitaIva
136900090409                                        WHEN '' THEN NULL
137000090409                                        ELSE :dsp_partitaIva
137100090409                                        END
137200090409               , AZSOC00F.NATURA_GIURIDICA = :dsp_naturaGiur
137300090409               , AZSOC00F.SOCIO_UNICO = CASE :dsp_socioUnico
137400090409                                        WHEN 'N' THEN '0'
137500090409                                        WHEN 'S' THEN '1'
137600090409                                        ELSE '0'
137700090409                                        END
137800090409               , AZSOC00F.CAPITALE_SOCIALE_IMPORTO = :dsp_capSocImp
137900090409               , AZSOC00F.CAPITALE_SOCIALE_TIPO = :dsp_capSocTip
138000090409               , AZSOC00F.CAPITALE_SOCIALE_IV = CASE :dsp_capSocIv
138100090409                                                WHEN 'N' THEN '0'
138200090409                                                WHEN 'S' THEN '1'
138300090409                                                ELSE '0'
138400090409                                                END
138500090409               , AZSOC00F.REA_LUOGO = RTRIM(:dsp_reaLuogo)
138600090409               , AZSOC00F.REA_NUMERO = RTRIM(:dsp_reaNumero)
138700090409               , AZSOC00F.REGISTRO_IMPRESE_LUOGO = RTRIM(:dsp_rimLuogo)
138800090409               , AZSOC00F.REGISTRO_IMPRESE_DATA = :dsp_rimDatIscr
138900090409               , AZSOC00F.SEDE_LEGALE_NAZIONE = :dsp_sedLegNaz
139000090409               , AZSOC00F.SEDE_LEGALE_PROVINCIA = :dsp_sedLegPro
139100090409               , AZSOC00F.SEDE_LEGALE_COMUNE = RTRIM(:dsp_sedLegLoc)
139200090409               , AZSOC00F.SEDE_LEGALE_CAP = RTRIM(:dsp_sedLegCap)
139300090409               , AZSOC00F.SEDE_LEGALE_INDIRIZZO = RTRIM(:dsp_sedLegInd)
139400090409               , AZSOC00F.SEDE_LEGALE_TELEFONO = RTRIM(:dsp_sedLegTel)
139500090409               , AZSOC00F.SEDE_LEGALE_TELEFAX = RTRIM(:dsp_sedLegFax)
139600090409               , AZSOC00F.SEDE_LEGALE_COMUNE_CATASTO = :dsp_sedLegCat
139700090409               , AZSOC00F.SEDE_AMMIN_NAZIONE = :dsp_sedAmmNaz
139800090409               , AZSOC00F.SEDE_AMMIN_PROVINCIA = :dsp_sedAmmPro
139900090416               , AZSOC00F.SEDE_AMMIN_COMUNE = CASE :dsp_sedAmmLoc
140000090416                                              WHEN '' THEN RTRIM(:dsp_sedLegLoc)
140100090416                                              ELSE RTRIM(:dsp_sedAmmLoc)
140200090416                                              END
140300090416               , AZSOC00F.SEDE_AMMIN_CAP = CASE :dsp_sedAmmCap
140400090416                                           WHEN '' THEN RTRIM(:dsp_sedLegCap)
140500090416                                           ELSE RTRIM(:dsp_sedAmmCap)
140600090416                                           END
140700090416               , AZSOC00F.SEDE_AMMIN_INDIRIZZO = CASE :dsp_sedAmmInd
140800090416                                                 WHEN ''
140900090416                                                 THEN RTRIM(:dsp_sedLegInd)
141000090416                                                 ELSE RTRIM(:dsp_sedAmmInd)
141100090416                                                 END
141200090416               , AZSOC00F.SEDE_AMMIN_TELEFONO = CASE :dsp_sedAmmTel
141300090416                                                WHEN ''
141400090416                                                THEN RTRIM(:dsp_sedLegTel)
141500090416                                                ELSE RTRIM(:dsp_sedAmmTel)
141600090416                                                END
141700090416               , AZSOC00F.SEDE_AMMIN_TELEFAX = CASE :dsp_sedAmmFax
141800090416                                               WHEN ''
141900090416                                               THEN RTRIM(:dsp_sedLegFax)
142000090416                                               ELSE RTRIM(:dsp_sedAmmFax)
142100090416                                               END
142200090409               , AZSOC00F.SEDE_AMMIN_COMUNE_CATASTO = :dsp_sedAmmCat
142300090409               , AZSOC00F.CODICE_FISCALE_INCORPORANTE = CASE :dsp_codFiscInc
142400090409                                                        WHEN '' THEN NULL
142500090409                                                        ELSE :dsp_codFiscInc
142600090409                                                        END
142700091119               , AZSOC00F.CASELLA_POSTALE = CASE :dsp_casellaPt
142800091119                                            WHEN '' THEN NULL
142900091119                                            ELSE RTRIM(:dsp_casellaPt)
143000091119                                            END
143100090409             WHERE AZSOC00F.ID_RIGA = :tnsd4040i.idRiga
143200090409         ;
143300090409
143400090409         IF sqlCode < *ZERO;
143500090409           DUMP(A);
143600090409           RETURN sqlCode;
143700090409         ENDIF;
143800090415
143900090415         // Cambio la data inizio attività su tutte le anagrafiche della società.
144000090415
144100090415         EXEC SQL
144200090415           UPDATE AZSOC00F
144300090415             SET AZSOC00F.ATTIVITA_DATA_INIZIO = :dsp_attDatIniz
144400090415             WHERE AZSOC00F.CODICE_FISCALE = :dsp_codFiscale
144500090415         ;
144600090415
144700090415         IF sqlCode < *ZERO;
144800090415           DUMP(A);
144900090415           RETURN sqlCode;
145000090415         ENDIF;
145100090409
145200090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVAANAGRAFICA;
145300090409
145400090409         EXEC SQL
145500090409           UPDATE AZSOC00F
145600090409             SET AZSOC00F.VALIDITA_DATA_FINE = :dsp_valDatIniz - 1 DAY
145700090409             WHERE AZSOC00F.ID_RIGA = :tnsd4040i.idRiga
145800090409         ;
145900090409
146000090409         IF sqlCode < *ZERO;
146100090409           DUMP(A);
146200090409           RETURN sqlCode;
146300090409         ENDIF;
146400090409
146500090409         EXEC SQL
146600090409           INSERT INTO AZSOC00F
146700090409             ( AZSOC00F.ID_SOCIETA
146800090409             , AZSOC00F.VALIDITA_DATA_INIZIO
146900090409             , AZSOC00F.RAGIONE_SOCIALE
147000090409             , AZSOC00F.RAGIONE_SOCIALE_BREVE
147100090409             , AZSOC00F.ATTIVITA_DATA_INIZIO
147200090409             , AZSOC00F.ATTIVITA_ATECO2007
147300090409             , AZSOC00F.CODICE_FISCALE
147400090409             , AZSOC00F.PARTITA_IVA
147500090409             , AZSOC00F.NATURA_GIURIDICA
147600090409             , AZSOC00F.SOCIO_UNICO
147700090409             , AZSOC00F.CAPITALE_SOCIALE_IMPORTO
147800090409             , AZSOC00F.CAPITALE_SOCIALE_TIPO
147900090409             , AZSOC00F.CAPITALE_SOCIALE_IV
148000090409             , AZSOC00F.REA_LUOGO
148100090409             , AZSOC00F.REA_NUMERO
148200090409             , AZSOC00F.REGISTRO_IMPRESE_LUOGO
148300090409             , AZSOC00F.REGISTRO_IMPRESE_DATA
148400090409             , AZSOC00F.SEDE_LEGALE_NAZIONE
148500090409             , AZSOC00F.SEDE_LEGALE_PROVINCIA
148600090409             , AZSOC00F.SEDE_LEGALE_COMUNE
148700090409             , AZSOC00F.SEDE_LEGALE_CAP
148800090409             , AZSOC00F.SEDE_LEGALE_INDIRIZZO
148900090409             , AZSOC00F.SEDE_LEGALE_TELEFONO
149000090409             , AZSOC00F.SEDE_LEGALE_TELEFAX
149100090409             , AZSOC00F.SEDE_LEGALE_COMUNE_CATASTO
149200090409             , AZSOC00F.SEDE_AMMIN_NAZIONE
149300090409             , AZSOC00F.SEDE_AMMIN_PROVINCIA
149400090409             , AZSOC00F.SEDE_AMMIN_COMUNE
149500090409             , AZSOC00F.SEDE_AMMIN_CAP
149600090409             , AZSOC00F.SEDE_AMMIN_INDIRIZZO
149700090409             , AZSOC00F.SEDE_AMMIN_TELEFONO
149800090409             , AZSOC00F.SEDE_AMMIN_TELEFAX
149900090409             , AZSOC00F.SEDE_AMMIN_COMUNE_CATASTO
150000091119             , AZSOC00F.CASELLA_POSTALE
150100090409             )
150200090409             VALUES( :dsp_idSocieta
150300090409                   , :dsp_valDatIniz
150400090409                   , RTRIM(:dsp_ragSociale)
150500090409                   , RTRIM(:dsp_ragSocBrev)
150600090409                   , :dsp_attDatIniz
150700090409                   , :dsp_ateco2007
150800090409                   , :dsp_codFiscale
150900090409                   , CASE :dsp_partitaIva
151000090409                     WHEN '' THEN NULL
151100090409                     ELSE :dsp_partitaIva
151200090409                     END
151300090409                   , :dsp_naturaGiur
151400090409                   , CASE :dsp_socioUnico
151500090409                     WHEN 'N' THEN '0'
151600090409                     WHEN 'S' THEN '1'
151700090409                     ELSE '0'
151800090409                     END
151900090409                   , :dsp_capSocImp
152000090409                   , :dsp_capSocTip
152100090409                   , CASE :dsp_capSocIv
152200090409                     WHEN 'N' THEN '0'
152300090409                     WHEN 'S' THEN '1'
152400090409                     ELSE '0'
152500090409                     END
152600090409                   , RTRIM(:dsp_reaLuogo)
152700090409                   , RTRIM(:dsp_reaNumero)
152800090409                   , RTRIM(:dsp_rimLuogo)
152900090409                   , :dsp_rimDatIscr
153000090409                   , :dsp_sedLegNaz
153100090409                   , :dsp_sedLegPro
153200090409                   , RTRIM(:dsp_sedLegLoc)
153300090409                   , RTRIM(:dsp_sedLegCap)
153400090409                   , RTRIM(:dsp_sedLegInd)
153500090409                   , RTRIM(:dsp_sedLegTel)
153600090409                   , RTRIM(:dsp_sedLegFax)
153700090409                   , :dsp_sedLegCat
153800090409                   , CASE :dsp_sedAmmNaz
153900090409                     WHEN '' THEN :dsp_sedLegNaz
154000090409                     ELSE :dsp_sedAmmNaz
154100090409                     END
154200090409                   , CASE :dsp_sedAmmPro
154300090409                     WHEN '' THEN :dsp_sedLegPro
154400090409                     ELSE :dsp_sedAmmPro
154500090409                     END
154600090409                   , CASE :dsp_sedAmmLoc
154700090409                     WHEN '' THEN RTRIM(:dsp_sedLegLoc)
154800090409                     ELSE RTRIM(:dsp_sedAmmLoc)
154900090409                     END
155000090409                   , CASE :dsp_sedAmmCap
155100090409                     WHEN '' THEN RTRIM(:dsp_sedLegCap)
155200090409                     ELSE RTRIM(:dsp_sedAmmCap)
155300090409                     END
155400090409                   , CASE :dsp_sedAmmInd
155500090409                     WHEN '' THEN RTRIM(:dsp_sedLegInd)
155600090409                     ELSE RTRIM(:dsp_sedAmmInd)
155700090409                     END
155800090409                   , CASE :dsp_sedAmmTel
155900090409                     WHEN '' THEN RTRIM(:dsp_sedLegTel)
156000090409                     ELSE RTRIM(:dsp_sedAmmTel)
156100090409                     END
156200090409                   , CASE :dsp_sedAmmFax
156300090409                     WHEN '' THEN RTRIM(:dsp_sedLegFax)
156400090409                     ELSE RTRIM(:dsp_sedAmmFax)
156500090409                     END
156600090409                   , CASE :dsp_sedAmmCat
156700090409                     WHEN '' THEN :dsp_sedLegCat
156800090409                     ELSE :dsp_sedAmmCat
156900090409                     END
157000091119                   , CASE :dsp_casellaPt
157100091119                     WHEN '' THEN NULL
157200091119                     ELSE RTRIM(:dsp_casellaPt)
157300091119                     END
157400090409                   )
157500090409         ;
157600090409
157700090409         IF sqlCode < *ZERO;
157800090409           DUMP(A);
157900090409           RETURN sqlCode;
158000090409         ENDIF;
158100090409
158200090420         EXEC SQL
158300090420           VALUES IDENTITY_VAL_LOCAL()
158400090420             INTO :tnsd4040o.idRiga
158500090420         ;
158600090420
158700090420       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_NUOVASOCIETA;
158800090409
158900090409         EXEC SQL
159000090409           INSERT INTO AZSOC00F
159100090409             ( AZSOC00F.ID_SOCIETA
159200090409             , AZSOC00F.RAGIONE_SOCIALE
159300090409             , AZSOC00F.RAGIONE_SOCIALE_BREVE
159400090409             , AZSOC00F.ATTIVITA_DATA_INIZIO
159500090409             , AZSOC00F.ATTIVITA_ATECO2007
159600090409             , AZSOC00F.CODICE_FISCALE
159700090409             , AZSOC00F.PARTITA_IVA
159800090409             , AZSOC00F.NATURA_GIURIDICA
159900090409             , AZSOC00F.SOCIO_UNICO
160000090409             , AZSOC00F.CAPITALE_SOCIALE_IMPORTO
160100090409             , AZSOC00F.CAPITALE_SOCIALE_TIPO
160200090409             , AZSOC00F.CAPITALE_SOCIALE_IV
160300090409             , AZSOC00F.REA_LUOGO
160400090409             , AZSOC00F.REA_NUMERO
160500090409             , AZSOC00F.REGISTRO_IMPRESE_LUOGO
160600090409             , AZSOC00F.REGISTRO_IMPRESE_DATA
160700090409             , AZSOC00F.SEDE_LEGALE_NAZIONE
160800090409             , AZSOC00F.SEDE_LEGALE_PROVINCIA
160900090409             , AZSOC00F.SEDE_LEGALE_COMUNE
161000090409             , AZSOC00F.SEDE_LEGALE_CAP
161100090409             , AZSOC00F.SEDE_LEGALE_INDIRIZZO
161200090409             , AZSOC00F.SEDE_LEGALE_TELEFONO
161300090409             , AZSOC00F.SEDE_LEGALE_TELEFAX
161400090409             , AZSOC00F.SEDE_LEGALE_COMUNE_CATASTO
161500090409             , AZSOC00F.SEDE_AMMIN_NAZIONE
161600090409             , AZSOC00F.SEDE_AMMIN_PROVINCIA
161700090409             , AZSOC00F.SEDE_AMMIN_COMUNE
161800090409             , AZSOC00F.SEDE_AMMIN_CAP
161900090409             , AZSOC00F.SEDE_AMMIN_INDIRIZZO
162000090409             , AZSOC00F.SEDE_AMMIN_TELEFONO
162100090409             , AZSOC00F.SEDE_AMMIN_TELEFAX
162200090409             , AZSOC00F.SEDE_AMMIN_COMUNE_CATASTO
162300091119             , AZSOC00F.CASELLA_POSTALE
162400090409             )
162500090409             VALUES( :dsp_idSocieta
162600090409                   , RTRIM(:dsp_ragSociale)
162700090409                   , RTRIM(:dsp_ragSocBrev)
162800090409                   , :dsp_attDatIniz
162900090409                   , :dsp_ateco2007
163000090409                   , :dsp_codFiscale
163100090409                   , CASE :dsp_partitaIva
163200090409                     WHEN '' THEN NULL
163300090409                     ELSE :dsp_partitaIva
163400090409                     END
163500090409                   , :dsp_naturaGiur
163600090409                   , CASE :dsp_socioUnico
163700090409                     WHEN 'N' THEN '0'
163800090409                     WHEN 'S' THEN '1'
163900090409                     ELSE '0'
164000090409                     END
164100090409                   , :dsp_capSocImp
164200090409                   , :dsp_capSocTip
164300090409                   , CASE :dsp_capSocIv
164400090409                     WHEN 'N' THEN '0'
164500090409                     WHEN 'S' THEN '1'
164600090409                     ELSE '0'
164700090409                     END
164800090409                   , RTRIM(:dsp_reaLuogo)
164900090409                   , RTRIM(:dsp_reaNumero)
165000090409                   , RTRIM(:dsp_rimLuogo)
165100090409                   , :dsp_rimDatIscr
165200090409                   , :dsp_sedLegNaz
165300090409                   , :dsp_sedLegPro
165400090409                   , RTRIM(:dsp_sedLegLoc)
165500090409                   , RTRIM(:dsp_sedLegCap)
165600090409                   , RTRIM(:dsp_sedLegInd)
165700090409                   , RTRIM(:dsp_sedLegTel)
165800090409                   , RTRIM(:dsp_sedLegFax)
165900090409                   , :dsp_sedLegCat
166000090409                   , CASE :dsp_sedAmmNaz
166100090409                     WHEN '' THEN :dsp_sedLegNaz
166200090409                     ELSE :dsp_sedAmmNaz
166300090409                     END
166400090409                   , CASE :dsp_sedAmmPro
166500090409                     WHEN '' THEN :dsp_sedLegPro
166600090409                     ELSE :dsp_sedAmmPro
166700090409                     END
166800090409                   , CASE :dsp_sedAmmLoc
166900090409                     WHEN '' THEN RTRIM(:dsp_sedLegLoc)
167000090409                     ELSE RTRIM(:dsp_sedAmmLoc)
167100090409                     END
167200090409                   , CASE :dsp_sedAmmCap
167300090409                     WHEN '' THEN RTRIM(:dsp_sedLegCap)
167400090409                     ELSE RTRIM(:dsp_sedAmmCap)
167500090409                     END
167600090409                   , CASE :dsp_sedAmmInd
167700090409                     WHEN '' THEN RTRIM(:dsp_sedLegInd)
167800090409                     ELSE RTRIM(:dsp_sedAmmInd)
167900090409                     END
168000090409                   , CASE :dsp_sedAmmTel
168100090409                     WHEN '' THEN RTRIM(:dsp_sedLegTel)
168200090409                     ELSE RTRIM(:dsp_sedAmmTel)
168300090409                     END
168400090409                   , CASE :dsp_sedAmmFax
168500090409                     WHEN '' THEN RTRIM(:dsp_sedLegFax)
168600090409                     ELSE RTRIM(:dsp_sedAmmFax)
168700090409                     END
168800090409                   , CASE :dsp_sedAmmCat
168900090409                     WHEN '' THEN :dsp_sedLegCat
169000090409                     ELSE :dsp_sedAmmCat
169100090409                     END
169200091119                   , CASE :dsp_casellaPt
169300091119                     WHEN '' THEN NULL
169400091119                     ELSE RTRIM(:dsp_casellaPt)
169500091119                     END
169600090409                   )
169700090409         ;
169800090409
169900090409         IF sqlCode < *ZERO;
170000090409           DUMP(A);
170100090409           RETURN sqlCode;
170200090409         ENDIF;
170300090420
170400090420         EXEC SQL
170500090420           VALUES IDENTITY_VAL_LOCAL()
170600090420             INTO :tnsd4040o.idRiga
170700090420         ;
170800090409
170900090409       ELSEIF tnsd4040i.rqsOpCode = SOCIETA_OPCODE_CANCELLAREANAGRAFICA;
171000090409
171100090409         EXEC SQL
171200090409           DELETE FROM AZSOC00F
171300090409             WHERE AZSOC00F.ID_RIGA = :tnsd4040i.idRiga
171400090409         ;
171500090409
171600090409         IF sqlCode < *ZERO;
171700090409           DUMP(A);
171800090409           RETURN sqlCode;
171900090409         ENDIF;
172000090409
172100090409       ENDIF;
172200090409
172300090415       // Controllo che i periodi di validità delle anagrafiche siano contigui.
172400090415
172500090415       EXEC SQL
172600090415         SELECT SUM(DAYS(A.VALIDITA_DATA_FINE) - DAYS(A.VALIDITA_DATA_INIZIO))
172700090415                + ( SELECT COUNT(*) - 1
172800090415                    FROM AZSOC00F AS B
172900090415                    WHERE B.CODICE_FISCALE = A.CODICE_FISCALE
173000090415                   )
173100090415           INTO :giorniCodiceFiscale
173200090415           FROM AZSOC00F AS A
173300090415           WHERE A.CODICE_FISCALE = :dsp_codFiscale
173400090415           GROUP BY A.CODICE_FISCALE
173500090415       ;
173600090415
173700090415       IF sqlCode < *ZERO;
173800090415         DUMP(A);
173900090415         RETURN sqlCode;
174000090415       ENDIF;
174100090415
174200090415       EXEC SQL
174300090415         SELECT SUM(DAYS(A.VALIDITA_DATA_FINE) - DAYS(A.VALIDITA_DATA_INIZIO))
174400090415                + ( SELECT COUNT(*) - 1
174500090415                    FROM AZSOC00F AS B
174600090415                    WHERE B.ID_SOCIETA = A.ID_SOCIETA
174700090415                   )
174800090415           INTO :giorniSocieta
174900090415           FROM AZSOC00F AS A
175000090415           WHERE A.ID_SOCIETA = :dsp_idSocieta
175100090415           GROUP BY A.ID_SOCIETA
175200090415       ;
175300090415
175400090415       IF sqlCode < *ZERO;
175500090415         DUMP(A);
175600090415         RETURN sqlCode;
175700090415       ENDIF;
175800090415
175900090415       IF giorniCodiceFiscale < 3652058 AND giorniSocieta < 3652058;
176000090415         RETURN ESITO_WARNING_PERIODINONCONTIGUI;
176100090415       ENDIF;
176200090415
176300090409       RETURN ESITO_OK;
176400090409
176500090409      /END-FREE
176600090409     P RunAnagrafica   E
176700090409
176800090414
176900090414     P*--------------------------------------------------
177000090414     P* Procedure name: InzFineAttivita
177100090414     P* Purpose:        Inizializza la finestra di fine attività società.
177200090414     P* Returns:        Esito.
177300090414     P*--------------------------------------------------
177400090414     PInzFineAttivita  B
177500090414     DInzFineAttivita  PI            10I 0
177600090414
177700090414     D attDatFine...
177800090414     D                 S               D
177900090414     D attDatFineNull...
178000090414     D                 S              5I 0
178100090414
178200090414      /FREE
178300090414
178400090414       RESET fineAttivi;
178500090414
178600090414       IF tnsd4040i.idRiga = *ZERO;
178700090414         RETURN ESITO_ERROR;
178800090414       ENDIF;
178900090414
179000090414       EXEC SQL
179100090414         SELECT AZSOC00F.RAGIONE_SOCIALE
179200090414              , AZSOC00F.ATTIVITA_DATA_FINE
179300090414              , AZSOC00F.CODICE_FISCALE
179400090414              , IFNULL(AZSOC00F.CODICE_FISCALE_INCORPORANTE , '')
179500090414           INTO :dsp_ragSociale
179600090414              , :attDatFine:attDatFineNull
179700090414              , :dsp_codFiscale
179800090414              , :dsp_codFiscInc
179900090414           FROM AZSOC00F
180000090414           WHERE AZSOC00F.ID_RIGA = :tnsd4040i.idRiga
180100090414       ;
180200090414
180300090414       IF sqlCode < *ZERO OR sqlCode = 100;
180400090414         RETURN sqlCode;
180500090414       ENDIF;
180600090414
180700090414       IF attDatFineNull = NOT_NULL;
180800090414         dsp_attDatFine = %CHAR(attDatFine : *EUR);
180900090414       ENDIF;
181000090414
181100090414       RETURN ESITO_OK;
181200090414
181300090414      /END-FREE
181400090414     PInzFineAttivita  E
181500090414
181600090414
181700090414     P*--------------------------------------------------
181800090414     P* Procedure name: SetFineAttivita
181900090414     P* Purpose:        Impstazione dati variabili finestra fine attività s...
182000090414     P*                          ocietà.
182100090414     P* Returns:        Esito.
182200090414     P*--------------------------------------------------
182300090414     PSetFineAttivita  B
182400090414     DSetFineAttivita  PI            10I 0
182500090414
182600090414      /FREE
182700090414
182800090414       RETURN ESITO_OK;
182900090414
183000090414      /END-FREE
183100090414     PSetFineAttivita  E
183200090414
183300090414
183400090414     P*--------------------------------------------------
183500090414     P* Procedure name: PutFineAttivita
183600090414     P* Purpose:        Emetto finestra fine attività società
183700090414     P* Returns:        Esito.
183800090414     P*--------------------------------------------------
183900090414     PPutFineAttivita  B
184000090414     DPutFineAttivita  PI            10I 0
184100090414
184200090414      /FREE
184300090414
184400090414       WRITE fineAttivi;
184500090414
184600090414       RETURN ESITO_OK;
184700090414
184800090414      /END-FREE
184900090414     PPutFineAttivita  E
185000090414
185100090414
185200090414     P*--------------------------------------------------
185300090414     P* Procedure name: GetFineAttivita
185400090414     P* Purpose:        Leggo finestra fine attività società.
185500090414     P* Returns:        Esito.
185600090414     P*--------------------------------------------------
185700090414     PGetFineAttivita  B
185800090414     DGetFineAttivita  PI            10I 0
185900090414
186000090414      /FREE
186100090414
186200090414       READ fineAttivi;
186300090414
186400090414       RETURN dspInf.dsp_aid_num;
186500090414
186600090414      /END-FREE
186700090414     PGetFineAttivita  E
186800090414
186900090414
187000090414     P*--------------------------------------------------
187100090414     P* Procedure name: ChkFineAttivita
187200090414     P* Purpose:
187300090414     P* Returns:        Esito.
187400090414     P*--------------------------------------------------
187500090414     PChkFineAttivita  B
187600090414     DChkFineAttivita  PI            10I 0
187700090414
187800090414      /FREE
187900090430
188000090430       RESET dspInd.erroreGenerico;
188100090430       RESET dspInd.errMsgDataFineAttivita;
188200090414
188300090414       // Controllo e decodifica società incorporante.
188400090414
188500090414       RESET dsp_ragSocInc;
188600090414
188700090414       IF dsp_codFiscInc <> *BLANK;
188800090414
188900090414         EXEC SQL
189000090414           SELECT AZSOC00F.RAGIONE_SOCIALE
189100090414             INTO :dsp_ragSocInc
189200090414             FROM AZSOC00F
189300090414             WHERE AZSOC00F.CODICE_FISCALE = :dsp_codFiscInc
189400090414             ORDER BY AZSOC00F.VALIDITA_DATA_FINE DESC
189500090414             FETCH FIRST ROW ONLY
189600090414         ;
189700090414
189800090414         IF sqlCode < *ZERO;
189900090414           DUMP(A);
190000090414           RETURN sqlCode;
190100090414         ELSEIF sqlCode = 100;
190200090414           // Errore.
190300090414         ENDIF;
190400090414
190500090414       ENDIF;
190600090430
190700090430       // Controllo data fine attività.
190800090430
190900090430       IF dsp_attDatFine <> *BLANK;
191000090430         TEST(DE) *EUR dsp_attDatFine;
191100090430         IF %ERROR();
191200090430           dataEUR0 = dsp_attDatFine;
191300090430           TEST(DE) *EUR0 dataEUR0;
191400090430           IF %ERROR();
191500090430             dspInd.erroreGenerico = *ON;
191600090430             dspInd.errMsgDataFineAttivita = *ON;
191700090430           ELSE;
191800090430             dsp_attDatFine = %CHAR(%DATE(dataEUR0 : *EUR0) : *EUR);
191900090430           ENDIF;
192000090430         ENDIF;
192100090430       ENDIF;
192200090430
192300090430       // Restituisco la presenza di errori;
192400090430
192500090430       IF dspInd.erroreGenerico;
192600090430         RETURN ESITO_ERROR;
192700090430       ENDIF;
192800090414
192900090414       RETURN ESITO_OK;
193000090414
193100090414      /END-FREE
193200090414     PChkFineAttivita  E
193300090414
193400090414
193500090414     P*--------------------------------------------------
193600090414     P* Procedure name: RunFineAttivita
193700090414     P* Purpose:        Elaborazione finestra fine attività società.
193800090414     P* Returns:        Esito.
193900090414     P*--------------------------------------------------
194000090414     PRunFineAttivita  B
194100090414     DRunFineAttivita  PI            10I 0
194200090414
194300090414      /FREE
194400090414
194500090414       EXEC SQL
194600090414         UPDATE AZSOC00F
194700090414           SET AZSOC00F.ATTIVITA_DATA_FINE =
194800090414               CASE :dsp_attDatFine
194900090414               WHEN '' THEN NULL
195000090430               ELSE DATE(:dsp_attDatFine)
195100090414               END
195200090414             , AZSOC00F.CODICE_FISCALE_INCORPORANTE =
195300090414               CASE :dsp_codFiscInc
195400090414               WHEN '' THEN NULL
195500090414               ELSE :dsp_codFiscInc
195600090414               END
195700090414           WHERE AZSOC00F.CODICE_FISCALE = :dsp_codFiscale
195800090414       ;
195900090414
196000090414       IF sqlCode < *ZERO;
196100090414         DUMP(A);
196200090414         RETURN sqlCode;
196300090414       ENDIF;
196400090414
196500090414       RETURN ESITO_OK;
196600090414
196700090414      /END-FREE
196800090414     PRunFineAttivita  E
196900090414
197000090414
197100090414     P*--------------------------------------------------
197200090414     P* Procedure name: RunRichiesta
197300090414     P* Purpose:        Gestione F4=Richiesta.
197400090414     P* Returns:        Esito.
197500090414     P*--------------------------------------------------
197600090414     P RunRichiesta    B
197700090414     D RunRichiesta    PI            10I 0
197800090428
197900090428     D dataInput...
198000090428     D                 S               D
198100090428     D dataOutput...
198200090428     D                 S               D
198300090428     D cEsito...
198400090428     D                 S              1A
198500090428
198600090414      /FREE
198700090414
198800090416       IF dsp_rtnCsrFld = 'ATECO2007';
198900090414
199000090414         RESET tibs02ds;
199100090414         tibs02ds.t02Mod = 'R';
199200090414         tibs02ds.t02Cod = 'AE7';
199300090414         TNTBE_RicercaControllo( kpjba : tibs02ds );
199400090414         IF tibs02ds.t02Ke1 <> *BLANK;
199500090414           dsp_ateco2007 = tibs02ds.t02Ke1;
199600090414           dsp_desAteco = tibs02ds.t02Uni;
199700090414         ENDIF;
199800090420
199900090420       ELSEIF dsp_rtnCsrFld = 'CAPSOCTIP';
200000090420
200100090420         RESET tibs02ds;
200200090420         tibs02ds.t02Mod = 'R';
200300090420         tibs02ds.t02Cod = '003';
200400090420         TNTBE_RicercaControllo( kpjba : tibs02ds );
200500090420         IF tibs02ds.t02Ke1 <> *BLANK;
200600090420           dsp_capSocTip = tibs02ds.t02Ke1;
200700090420           dsp_desCapSocT = tibs02ds.t02Uni;
200800090420         ENDIF;
200900090416
201000090416       ELSEIF dsp_rtnCsrFld = 'SEDLEGCAT' OR dsp_rtnCsrFld = 'SEDAMMCAT';
201100090416
201200090416         RESET tibs02ds;
201300090416         tibs02ds.t02Mod = 'R';
201400090416         tibs02ds.t02Cod = 'CTT';
201500090416         TNTBE_RicercaControllo( kpjba : tibs02ds );
201600090416         IF tibs02ds.t02Ke1 <> *BLANK;
201700090416           dCtt = tibs02ds.t02Uni;
201800090416           IF dsp_rtnCsrFld = 'SEDLEGCAT';
201900090416             dsp_sedLegCat = tibs02ds.t02Ke1;
202000090416             dsp_desLegCat = dCtt.comune;
202100090416           ELSEIF dsp_rtnCsrFld = 'SEDAMMCAT';
202200090416             dsp_sedAmmCat = tibs02ds.t02Ke1;
202300090416             dsp_desAmmCat = dCtt.comune;
202400090416           ENDIF;
202500090416         ENDIF;
202600090416
202700090416       ELSEIF dsp_rtnCsrFld = 'SEDLEGPRO' OR dsp_rtnCsrFld = 'SEDAMMPRO';
202800090416
202900090416         TABEL_Ricerca( TABELLA_PROVINCIE
203000090416                      : ORDINAMENTO_DESCRIZIONE
203100090416                      : idElemento
203200090416                      : tastoFunzionaleUscita
203300090416                      );
203400090416
203500090416         IF tastoFunzionaleUscita = *BLANK;
203600090416           IF dsp_rtnCsrFld = 'SEDLEGPRO';
203700090416             dsp_sedLegPro = idElemento;
203800090416           ELSEIF dsp_rtnCsrFld = 'SEDAMMPRO';
203900090416             dsp_sedAmmPro = idElemento;
204000090416           ENDIF;
204100090416         ENDIF;
204200090416
204300090416       ELSEIF dsp_rtnCsrFld = 'SEDLEGNAZ' OR dsp_rtnCsrFld = 'SEDAMMNAZ';
204400090416
204500090416         TABEL_Ricerca( TABELLA_NAZIONI
204600090416                      : ORDINAMENTO_DESCRIZIONE
204700090416                      : idElemento
204800090416                      : tastoFunzionaleUscita
204900090416                      );
205000090415
205100090416         IF tastoFunzionaleUscita = *BLANK;
205200090416           IF dsp_rtnCsrFld = 'SEDLEGNAZ';
205300090416             dsp_sedLegNaz = idElemento;
205400090416           ELSEIF dsp_rtnCsrFld = 'SEDAMMNAZ';
205500090416             dsp_sedAmmNaz = idElemento;
205600090416           ENDIF;
205700090416         ENDIF;
205800090416
205900090416       ELSEIF dsp_rtnCsrFld = 'IDSOCIETA';
206000090414
206100090415         RESET ana007Ds;
206200090415         kpjba.kpjbu = ana007Ds;
206300090415         Proj_RicercaSocieta( kpjba );
206400090415         ana007Ds = kpjba.kpjbu;
206500090415         IF ana007Ds.ret007 = PROJ_ENTER;
206600090415           dsp_idSocieta = ana007Ds.soc007;
206700090415         ENDIF;
206800090416
206900090416       ELSEIF dsp_rtnCsrFld = 'NATURAGIUR';
207000090416
207100090416         RESET xTabDs;
207200090416         kpjba.kpjbu = xTabDs;
207300090416         Proj_Tabella_NaturaGiuridica( kpjba );
207400090416         xTabDs = kpjba.kpjbu;
207500090416         IF xTabDs.xtaRet = PROJ_ENTER;
207600090416           dsp_naturaGiur = %SUBST(xTabDs.xtaKey : 14);
207700090416         ENDIF;
207800090428
207900090428       ELSEIF dsp_rtnCsrFld = 'VALDATINIZ' OR dsp_rtnCsrFld = 'VALDATFINE'
208000090428       OR dsp_rtnCsrFld = 'ATTDATINIZ';
208100090428
208200090428         IF dsp_rtnCsrFld = 'VALDATINIZ';
208300090428           dataInput = dsp_valDatIniz;
208400090428         ELSEIF dsp_rtnCsrFld = 'VALDATFINE';
208500090428           dataInput = dsp_valDatFine;
208600090428         ELSEIF dsp_rtnCsrFld = 'ATTDATINIZ';
208700090428           dataInput = dsp_attDatIniz;
208800090428         ENDIF;
208900090428
209000090428         Calendario( dataInput : dataOutput : cEsito );
209100090428
209200090428         IF cEsito = *OFF;
209300090428           IF dsp_rtnCsrFld = 'VALDATINIZ';
209400090428             dsp_valDatIniz = dataOutput;
209500090428           ELSEIF dsp_rtnCsrFld = 'VALDATFINE';
209600090428             dsp_valDatFine = dataOutput;
209700090428           ELSEIF dsp_rtnCsrFld = 'ATTDATINIZ';
209800090428             dsp_attDatIniz = dataOutput;
209900090428           ENDIF;
210000090428         ENDIF;
210100090428
210200090414       ENDIF;
210300090414
210400090414       RETURN ESITO_OK;
210500090414
210600090414      /END-FREE
210700090414     P RunRichiesta    E
210800090414
