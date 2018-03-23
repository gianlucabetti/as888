000100100125     ***************************************************************************
000200100125     **
000300100125     ** Manutenzione generica TNTBE00F in lingua.
000400100125     **
000500100125     ***************************************************************************
000600100127     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700100125
000800100125     ***************************************************************************
000900100125     **
001000100125     ** File.
001100100125     **
001200100125     ***************************************************************************
001300100125     Ftntb___d  CF   E             WORKSTN PREFIX(dsp_)
001400100125     F                                     INDDS(dspInd)
001500100125     F                                     INFDS(dspInf)
001600100125     F                                     SFILE(elencoS:elencoRrn)
001700100125
001800100125     ***************************************************************************
001900100125     **
002000100125     ** Costanti.
002100100125     **
002200100125     ***************************************************************************
002300100125     D ESITO_ERROR...
002400100125     D                 C                   -1
002500100125     D ESITO_OK...
002600100125     D                 C                   0
002700100126     D ESITO_WARNING_F3...
002800100126     D                 C                   51
002900100126     D ESITO_WARNING_F5...
003000100126     D                 C                   53
003100100126     D ESITO_WARNING_F7...
003200100126     D                 C                   55
003300100126     D ESITO_WARNING_F10...
003400100126     D                 C                   58
003500100126     D ESITO_WARNING_F12...
003600100126     D                 C                   60
003700100125     D F1              C                   X'31'
003800100125     D F2              C                   X'32'
003900100125     D F3              C                   X'33'
004000100125     D F4              C                   X'34'
004100100125     D F5              C                   X'35'
004200100125     D F6              C                   X'36'
004300100125     D F7              C                   X'37'
004400100125     D F8              C                   X'38'
004500100125     D F9              C                   X'39'
004600100125     D F10             C                   X'3A'
004700100125     D F11             C                   X'3B'
004800100125     D F12             C                   X'3C'
004900100125     D F13             C                   X'B1'
005000100125     D F14             C                   X'B2'
005100100125     D F15             C                   X'B3'
005200100125     D F16             C                   X'B4'
005300100125     D F17             C                   X'B5'
005400100125     D F18             C                   X'B6'
005500100125     D F19             C                   X'B7'
005600100125     D F20             C                   X'B8'
005700100125     D F21             C                   X'B9'
005800100125     D F22             C                   X'BA'
005900100125     D F23             C                   X'BB'
006000100125     D F24             C                   X'BC'
006100100125     D ENTER           C                   X'F1'
006200100129     D ITALIANO...
006300100129     D                 C                   ''
006400100125     D OPCODE_ELENCO_GET...
006500100125     D                 C                   'GETELENCO'
006600100125     D OPCODE_ELENCO_INZ...
006700100125     D                 C                   'INZELENCO'
006800100125     D OPCODE_ELENCO_PUT...
006900100125     D                 C                   'PUTELENCO'
007000100125     D OPCODE_ELENCO_SET...
007100100125     D                 C                   'SETELENCO'
007200100125     D OPCODE_FINE...
007300100125     D                 C                   'FINE'
007400100126     D OPZIONE_NUOVO...
007500100126     D                 C                   1
007600100126     D OPZIONE_MODIFICARE...
007700100126     D                 C                   2
007800100126     D OPZIONE_COPIARE...
007900100126     D                 C                   3
008000100126     D OPZIONE_ANNULLARE...
008100100126     D                 C                   4
008200100126     D OPZIONE_VISUALIZZARE...
008300100126     D                 C                   5
008400100126     D OPZIONE_CANCELLARE...
008500100126     D                 C                   44
008600100127     D SQLSTATE_DUPLICATE_KEY_VALUE_SPECIFIED...
008700100127     D                 C                   '23505'
008800100127     D SQLSTATE_EMPTY_TABLE...
008900100127     D                 C                   '02000'
009000100127     D TUTTO...
009100100127     D                 C                   '*'
009200100125
009300100125     ***************************************************************************
009400100125     **
009500100125     ** Strutture dati.
009600100125     **
009700100125     ***************************************************************************
009800100125     D dspInd          DS                  QUALIFIED
009900100125     D  subFileDisplay...
010000100125     D                         1      1N
010100100125     D  subFileClear...
010200100125     D                         2      2N
010300100125     D  subFileNextChange...
010400100125     D                         3      3N
010500100125     D  coloreRiga...
010600100125     D                         4      4N
010700100126     D  chiaveElementoProtect...
010800100126     D                         5      5N
010900100126     D  datiProtect...
011000100126     D                         6      6N
011100100127     D  elementoGiaEsistente...
011200100127     D                         7      7N
011300100127     D  mancaElementoItaliano...
011400100127     D                         8      8N
011500100127     D  attivoFiltroKe1...
011600100127     D                         9      9N
011700100127     D  attivoFiltroKe2...
011800100127     D                        10     10N
011900100127     D  attivoFiltroLin...
012000100127     D                        11     11N
012100100127     D  attivoFiltroAtb...
012200100127     D                        13     13N
012300100127     D  attivoFiltroUni...
012400100127     D                        14     14N
012500100129     D  elencoCompresso...
012600100129     D                        15     15N
012700100127     D  erroreGenerico...
012800100127     D                        99     99N
012900100125     D dspInf          DS                  QUALIFIED
013000100125     D  aid                  369    369A
013100100126     D  aidInt               369    369U 0
013200100125     D dTbe01        E DS                  QUALIFIED
013300100125     D                                     INZ
013400100125     D  tbeUni                 1    256A
013500100125     D kpjba         E DS                  QUALIFIED
013600100126     D tntbe00f      E DS                  QUALIFIED
013700100126     D                                     BASED(nullPtr)
013800100125     D tntb___i      E DS                  QUALIFIED
013900100125     D                                     INZ
014000100125     D tntb___o      E DS                  QUALIFIED
014100100125     D                                     INZ
014200100125
014300100125     ***************************************************************************
014400100125     **
014500100125     ** Campi.
014600100125     **
014700100125     ***************************************************************************
014800100127     D count...
014900100127     D                 S             10I 0
015000100126     D elencoRrn       S              5U 0
015100100125     D esito           S             10I 0
015200100125     D opCodeCur       S             10A
015300100125     D opCodeNxt       S             10A   INZ(OPCODE_ELENCO_INZ)
015400100125     D opCodePrv       S             10A
015500100127     D righelloD...
015600100127     D                 S            256A   INZ('         1         2         3 -
015700100127     D                                             4         5         6       -
015800100127     D                                       7         8         9        10   -
015900100127     D                                          11        12        13        1-
016000100127     D                                     4        15        16        17     -
016100100127     D                                        18        19        20        21 -
016200100127     D                                            22        23        24       -
016300100127     D                                      25      ')
016400100127     D righelloU...
016500100127     D                 S            256A   INZ('1234567890123456789012345678901-
016600100127     D                                     234567890123456789012345678901234567-
016700100127     D                                     890123456789012345678901234567890123-
016800100127     D                                     456789012345678901234567890123456789-
016900100127     D                                     012345678901234567890123456789012345-
017000100127     D                                     678901234567890123456789012345678901-
017100100127     D                                     234567890123456789012345678901234567-
017200100127     D                                     890123456')
017300100125     D stm             S          32740A   VARYING
017400100125     D stmFrom         S          32740A   VARYING
017500100125     D stmOrderBy      S          32740A   VARYING
017600100125     D stmSelect       S          32740A   VARYING
017700100125     D stmWhere        S          32740A   VARYING
017800100126     D tbeAtb          S                   LIKE(tntbe00f.tbeAtb)
017900100126     D tbeKe1          S                   LIKE(tntbe00f.tbeKe1)
018000100126     D tbeKe2          S                   LIKE(tntbe00f.tbeKe2)
018100100125
018200100125     D*--------------------------------------------------
018300100125     D* Procedure name: ChkParametri
018400100125     D* Purpose:        Controllo dei parametri ricevuti.
018500100125     D* Returns:        Esito.
018600100125     D*--------------------------------------------------
018700100125     D ChkParametri    PR            10I 0
018800100125
018900100125     D*--------------------------------------------------
019000100125     D* Procedure name: InzElenco
019100100125     D* Purpose:        Inizializza elenco.
019200100125     D* Returns:        Esito.
019300100125     D*--------------------------------------------------
019400100125     D InzElenco       PR            10I 0
019500100125
019600100125     D*--------------------------------------------------
019700100125     D* Procedure name: SetElenco
019800100125     D* Purpose:        Imposta elenco.
019900100125     D* Returns:        Esito.
020000100125     D*--------------------------------------------------
020100100125     D SetElenco       PR            10I 0
020200100125
020300100125     D*--------------------------------------------------
020400100125     D* Procedure name: PutElenco
020500100125     D* Purpose:        Emissione pannello elenco.
020600100125     D* Returns:        Esito.
020700100125     D*--------------------------------------------------
020800100125     D PutElenco       PR            10I 0
020900100125
021000100125     D*--------------------------------------------------
021100100125     D* Procedure name: GetElenco
021200100125     D* Purpose:        Lettura pannello elenco.
021300100125     D* Returns:        Esito.
021400100125     D*--------------------------------------------------
021500100125     D GetElenco       PR            10I 0
021600100125
021700100125     D*--------------------------------------------------
021800100125     D* Procedure name: RunElemento
021900100125     D* Purpose:        Elaborazione dell'elemento scelto.
022000100125     D* Returns:        Esito.
022100100125     D* Parameter:      piOpzione => Codice operativo.
022200100125     D* Parameter:      piKe1 => Chiave 1.
022300100125     D* Parameter:      piKe2 => Chiave 2.
022400100125     D* Parameter:      piLin => Lingua.
022500100125     D* Parameter:      piSistemaInformativo => Sistema Informativo.
022600100125     D*--------------------------------------------------
022700100125     D RunElemento     PR            10I 0
022800100125     D  piOpzione                     2A   CONST
022900100125     D  piKe1                        15A   OPTIONS(*NOPASS)
023000100125     D  piKe2                        15A   OPTIONS(*NOPASS)
023100100125     D  piLin                         1A   OPTIONS(*NOPASS)
023200100125     D  piSistemaInformativo...
023300100125     D                               10A   OPTIONS(*NOPASS)
023400100125
023500100125
023600100125     ***************************************************************************
023700100125     **
023800100125     ** Parametri.
023900100125     **
024000100125     ***************************************************************************
024100100125     C     *ENTRY        PLIST
024200100125     C                   PARM                    kpjba
024300100125
024400100125     ***************************************************************************
024500100125     **
024600100125     ** Main.
024700100125     **
024800100125     ***************************************************************************
024900100125
025000100125      /FREE
025100100125
025200100125       *INLR = *ON;
025300100125
025400100125       tntb___o.esito = ChkParametri();
025500100125
025600100125       IF tntb___o.esito < ESITO_OK;
025700100125         kpjba.kpjbu = tntb___o;
025800100125         RETURN;
025900100125       ENDIF;
026000100125
026100100125       DOU opCodeCur = OPCODE_FINE;
026200100125
026300100125         opCodeCur = opCodeNxt;
026400100125
026500100125         SELECT;
026600100125
026700100125           WHEN opCodeCur = OPCODE_ELENCO_INZ;
026800100125
026900100125             tntb___o.esito = InzElenco();
027000100125
027100100125             IF tntb___o.esito < ESITO_OK;
027200100125               opCodeNxt = OPCODE_FINE;
027300100125             ELSE;
027400100125               opCodeNxt = OPCODE_ELENCO_SET;
027500100125             ENDIF;
027600100125
027700100125           WHEN opCodeCur = OPCODE_ELENCO_SET;
027800100125
027900100125             tntb___o.esito = SetElenco();
028000100125
028100100125             IF tntb___o.esito < ESITO_OK;
028200100125               opCodeNxt = OPCODE_FINE;
028300100125             ELSE;
028400100125               opCodeNxt = OPCODE_ELENCO_PUT;
028500100125             ENDIF;
028600100125
028700100125           WHEN opCodeCur = OPCODE_ELENCO_PUT;
028800100125
028900100125             tntb___o.esito = PutElenco();
029000100125
029100100125             IF tntb___o.esito < ESITO_OK;
029200100125               opCodeNxt = OPCODE_FINE;
029300100125             ELSE;
029400100125               opCodeNxt = OPCODE_ELENCO_GET;
029500100125             ENDIF;
029600100125
029700100125           WHEN opCodeCur = OPCODE_ELENCO_GET;
029800100125
029900100125             tntb___o.esito = GetElenco();
030000100125
030100100126             SELECT;
030200100126               WHEN tntb___o.esito < ESITO_OK;
030300100126                 opCodeNxt = OPCODE_FINE;
030400100126               WHEN tntb___o.esito = ESITO_WARNING_F3;
030500100126                 opCodeNxt = OPCODE_FINE;
030600100126               WHEN tntb___o.esito = ESITO_WARNING_F5;
030700100127                 opCodeNxt = OPCODE_ELENCO_SET;
030800100126               WHEN tntb___o.esito = ESITO_WARNING_F7;
030900100126                 opCodeNxt = OPCODE_ELENCO_SET;
031000100126               WHEN tntb___o.esito = ESITO_WARNING_F12;
031100100127                 opCodeNxt = OPCODE_ELENCO_PUT;
031200100126               OTHER;
031300100126                 opCodeNxt = OPCODE_ELENCO_PUT;
031400100126             ENDSL;
031500100125
031600100125           WHEN opCodeCur = OPCODE_FINE;
031700100125
031800100125             LEAVE;
031900100125
032000100125           OTHER;
032100100125
032200100125             tntb___o.esito = ESITO_ERROR;
032300100125             opCodeNxt = OPCODE_FINE;
032400100125
032500100125         ENDSL;
032600100125
032700100125       ENDDO;
032800100125
032900100125       kpjba.kpjbu = tntb___o;
033000100125
033100100125      /END-FREE
033200100125
033300100125     P*--------------------------------------------------
033400100125     P* Procedure name: ChkParametri
033500100125     P* Purpose:        Controllo dei parametri ricevuti.
033600100125     P* Returns:        Esito.
033700100125     P*--------------------------------------------------
033800100125     P ChkParametri    B
033900100125     D ChkParametri    PI            10I 0
034000100125
034100100125      /FREE
034200100125
034300100125       tntb___i = kpjba.kpjbu;
034400100215
034500100215       IF %SUBST(kpjba.kpjbu : 14 : 3) = *BLANK;
034600100215         CLEAR tntb___i.desPos;
034700100215       ENDIF;
034800100215
034900100215       IF %SUBST(kpjba.kpjbu : 17 : 3) = *BLANK;
035000100215         CLEAR tntb___i.desLen;
035100100215       ENDIF;
035200100126
035300100127       // Parametri obbligatori.
035400100127
035500100127       EXEC SQL
035600100127         SELECT TBEUNI
035700100127           INTO :dTbe01.tbeUni
035800100127           FROM TNTBE00F
035900100127           WHERE TBECOD = ''
036000100127             AND TBEKE1 = '000000000000' CONCAT :tntb___i.tbeCod
036100100127             AND TBEKE2 = ''
036200100127             AND TBELIN = ''
036300100127             AND TBESIF = :tntb___i.tbeSif
036400100127       ;
036500100127
036600100127       SELECT;
036700100127         WHEN sqlState = SQLSTATE_EMPTY_TABLE;
036800100127           RETURN ESITO_ERROR;
036900100127         WHEN sqlState > '03';
037000100127           DUMP(A);
037100100127           RETURN sqlCode;
037200100127       ENDSL;
037300100126
037400100127       // Aggiusto i parametri che mi sono stati passati "ad minchiam".
037500100127
037600100127       SELECT;
037700100127         WHEN tntb___i.desPos < 1;
037800100127           tntb___i.desPos = 1;
037900100127         WHEN tntb___i.desPos > %SIZE(tntbe00f.tbeUni);
038000100127           tntb___i.desPos = %SIZE(tntbe00f.tbeUni);
038100100127       ENDSL;
038200100127
038300100127       SELECT;
038400100127         WHEN tntb___i.desLen < 1;
038500100127           tntb___i.desLen = %SIZE(tntbe00f.tbeUni);
038600100127         WHEN tntb___i.desLen > %SIZE(tntbe00f.tbeUni);
038700100127           tntb___i.desLen = %SIZE(tntbe00f.tbeUni);
038800100127       ENDSL;
038900100127
039000100127       IF tntb___i.desLen > %SIZE(tntbe00f.tbeUni) - tntb___i.desPos + 1;
039100100127         tntb___i.desLen = %SIZE(tntbe00f.tbeUni) - tntb___i.desPos + 1;
039200100127       ENDIF;
039300100125
039400100125       RETURN ESITO_OK;
039500100125
039600100125      /END-FREE
039700100125     P ChkParametri    E
039800100125
039900100125
040000100125     P*--------------------------------------------------
040100100125     P* Procedure name: InzElenco
040200100125     P* Purpose:        Inizializza elenco.
040300100125     P* Returns:        Esito.
040400100125     P*--------------------------------------------------
040500100125     P InzElenco       B
040600100125     D InzElenco       PI            10I 0
040700100125
040800100125      /FREE
040900100125
041000100127       RESET elencoC;
041100100125       dsp_tbeCod = tntb___i.tbeCod;
041200100125       dsp_§TbeDesc = dTbe01.§TbeDesc;
041300111117
041400111117       IF dTbe01.§TbeKe1Hdg <> *BLANK;
041500111117         dsp_ke1Hdg = dTbe01.§TbeKe1Hdg;
041600111117       ELSE;
041700111117         dsp_ke1Hdg = 'Chiave 1';
041800111117       ENDIF;
041900111117
042000111117       IF dTbe01.§TbeKe2Hdg <> *BLANK;
042100111117         dsp_ke2Hdg = dTbe01.§TbeKe2Hdg;
042200111117       ELSE;
042300111117         dsp_ke2Hdg = 'Chiave 2';
042400111117       ENDIF;
042500111117
042600100127       dsp_righelloU = %SUBST(righelloU : tntb___i.desPos : tntb___i.desLen);
042700100127       dsp_righelloD = %SUBST(righelloD : tntb___i.desPos : tntb___i.desLen);
042800100127
042900100127       dsp_tbeKe1Ftr = TUTTO;
043000100127       dsp_tbeKe2Ftr = TUTTO;
043100100127       dsp_tbeLinFtr = TUTTO;
043200100127       dsp_tbeAtbFtr = TUTTO;
043300100127       dsp_tbeUniFtr = TUTTO;
043400100127
043500100125       RETURN ESITO_OK;
043600100125
043700100125      /END-FREE
043800100125     P InzElenco       E
043900100125
044000100125
044100100125     P*--------------------------------------------------
044200100125     P* Procedure name: SetElenco
044300100125     P* Purpose:        Imposta elenco.
044400100125     P* Returns:        Esito.
044500100125     P*--------------------------------------------------
044600100125     P SetElenco       B
044700100125     D SetElenco       PI            10I 0
044800100125
044900100125      /FREE
045000100125
045100100125       RESET elencoRrn;
045200100129       RESET dsp_sflRcdNbr;
045300100125       RESET dspInd.coloreRiga;
045400100125       RESET dspInd.subFileDisplay;
045500100125       RESET dspInd.subFileNextChange;
045600100125       dspInd.subFileClear = *ON;
045700100125       WRITE elencoC;
045800100125       dspInd.subFileClear = *OFF;
045900100125
046000100212       stmSelect = 'SELECT TBEATB, TBEKE1, TBEKE2, TBELIN, +
046100100125                   SUBSTR(TBEUNI, '
046200100125                 + %CHAR(tntb___i.desPos)
046300100125                 + ', '
046400100125                 + %CHAR(tntb___i.desLen)
046500100125                 + ')'
046600100125                 ;
046700100125
046800100125       stmFrom = ' FROM TNTBE00F';
046900100125
047000100125       stmWhere = ' WHERE TBECOD = '''
047100100125                + tntb___i.tbeCod
047200100125                + ''''
047300100212                + ' AND TBESIF = '''
047400100212                + tntb___i.tbeSif
047500100212                + ''''
047600100212                ;
047700100125
047800100127       SELECT;
047900100127         WHEN dsp_tbeKe1Ftr = TUTTO;
048000100127           dspInd.attivoFiltroKe1 = *OFF;
048100100127         WHEN dsp_tbeKe1Ftr = *BLANK;
048200100127           dspInd.attivoFiltroKe1 = *ON;
048300100127           stmWhere += ' AND TBEKE1 = ''''';
048400100127         OTHER;
048500100127           dspInd.attivoFiltroKe1 = *ON;
048600100129           stmWhere += ' AND TBEKE1 >= '''
048700100129                    + dsp_tbeKe1Ftr
048800100129                    + ''''
048900100127                    ;
049000100127       ENDSL;
049100100127
049200100127       SELECT;
049300100127         WHEN dsp_tbeKe2Ftr = TUTTO;
049400100127           dspInd.attivoFiltroKe2 = *OFF;
049500100127         WHEN dsp_tbeKe2Ftr = *BLANK;
049600100127           dspInd.attivoFiltroKe2 = *ON;
049700100127           stmWhere += ' AND TBEKE2 = ''''';
049800100127         OTHER;
049900100127           dspInd.attivoFiltroKe2 = *ON;
050000100127           stmWhere += ' AND TBEKE2 LIKE '''
050100100127                    + %TRIMR(dsp_tbeKe2Ftr)
050200100127                    + '%'''
050300100127                    ;
050400100127       ENDSL;
050500100127
050600100127       IF dsp_tbeLinFtr <> TUTTO;
050700100127         dspInd.attivoFiltroLin = *ON;
050800100127         stmWhere += ' AND TBELIN = '''
050900100127                  + dsp_tbeLinFtr
051000100127                  + ''''
051100100127                  ;
051200100127       ELSE;
051300100127         dspInd.attivoFiltroLin = *OFF;
051400100127       ENDIF;
051500100127
051600100127       IF dsp_tbeAtbFtr <> TUTTO;
051700100127         dspInd.attivoFiltroAtb = *ON;
051800100127         stmWhere += ' AND TBEATB = '''
051900100127                  + dsp_tbeAtbFtr
052000100127                  + ''''
052100100127                  ;
052200100127       ELSE;
052300100127         dspInd.attivoFiltroAtb = *OFF;
052400100127       ENDIF;
052500100127
052600100127       SELECT;
052700100127         WHEN dsp_tbeUniFtr = TUTTO;
052800100127           dspInd.attivoFiltroUni = *OFF;
052900100127         WHEN dsp_tbeUniFtr = *BLANK;
053000100127           dspInd.attivoFiltroUni = *ON;
053100100127           stmWhere += ' AND TBEUNI = ''''';
053200100127         OTHER;
053300100127           dspInd.attivoFiltroUni = *ON;
053400100127           stmWhere += ' AND LCASE(TBEUNI) LIKE LCASE(''%'
053500100127                    + %TRIMR(dsp_tbeUniFtr)
053600100127                    + '%'')'
053700100127                    ;
053800100127       ENDSL;
053900100125
054000100129       IF dspInd.elencoCompresso;
054100100126         stmWhere += ' AND TBELIN = ''''';
054200100126       ENDIF;
054300100126
054400100212       stmOrderBy = ' ORDER BY TBECOD, TBEKE1, TBEKE2, TBELIN';
054500100125
054600100125       stm = stmSelect + stmFrom + stmWhere + stmOrderBy;
054700100125       stm += ' FETCH FIRST 9999 ROWS ONLY FOR FETCH ONLY WITH CS';
054800100125
054900100125       EXEC SQL
055000100125         PREPARE TNTBE00F FROM :stm
055100100125       ;
055200100125
055300100125       EXEC SQL
055400100125         DECLARE TNTBE00F NO SCROLL CURSOR FOR TNTBE00F
055500100125       ;
055600100125
055700100125       EXEC SQL
055800100125         OPEN TNTBE00F
055900100125       ;
056000100125
056100100125       SELECT;
056200100125         WHEN sqlState > '03';
056300100125           DUMP(A);
056400100125           RETURN sqlCode;
056500100125       ENDSL;
056600100125
056700100126       RESET elencoS;
056800100126
056900100125       DOU SqlState > '03';
057000100125
057100100125         EXEC SQL
057200100125           FETCH NEXT FROM TNTBE00F
057300100125             INTO :dsp_tbeAtb
057400100125                , :tbeKe1
057500100125                , :tbeKe2
057600100125                , :dsp_tbeLin
057700100125                , :dsp_descrizion
057800100125         ;
057900100125
058000100125         SELECT;
058100100125           WHEN sqlState = '02000';
058200100125             LEAVE;
058300100125           WHEN sqlState > '03';
058400100125             DUMP(A);
058500100125             RETURN sqlCode;
058600100125         ENDSL;
058700100125
058800100125         IF dsp_tbeKe1 <> tbeKe1
058900100212         OR dsp_tbeKe2 <> tbeKe2;
059000100212           IF dspInd.coloreRiga;
059100100127             dsp_hIn04 = *OFF;
059200100125           ELSE;
059300100127             dsp_hIn04 = *ON;
059400100125           ENDIF;
059500100125         ENDIF;
059600100125
059700100127         dspInd.coloreRiga = dsp_hIn04;
059800100125         dsp_tbeKe1 = tbeKe1;
059900100125         dsp_tbeKe2 = tbeKe2;
060000100125         elencoRrn += 1;
060100100125         WRITE elencoS;
060200100125
060300100125       ENDDO;
060400100125
060500100125       EXEC SQL
060600100125         CLOSE TNTBE00F
060700100125       ;
060800100125
060900100129       IF elencoRrn > *ZERO;
061000100129         dspInd.subFileDisplay = *ON;
061100100129         dsp_sflRcdNbr = 1;
061200100129       ENDIF;
061300100125
061400100125       RETURN ESITO_OK;
061500100125
061600100125      /END-FREE
061700100125     P SetElenco       E
061800100125
061900100125
062000100125     P*--------------------------------------------------
062100100125     P* Procedure name: PutElenco
062200100125     P* Purpose:        Emissione pannello elenco.
062300100125     P* Returns:        Esito.
062400100125     P*--------------------------------------------------
062500100125     P PutElenco       B
062600100125     D PutElenco       PI            10I 0
062700100125
062800100125      /FREE
062900100125
063000100205       SELECT;
063100100205         WHEN dsp_sflRcdNbr > elencoRrn;
063200100205           dsp_sflRcdNbr = elencoRrn;
063300100205         WHEN dsp_sflRcdNbr = *ZERO;
063400100205           dsp_sflRcdNbr = 1;
063500100205       ENDSL;
063600100205
063700100125       WRITE testata;
063800100125       WRITE tastiFunz;
063900100125       WRITE elencoC;
064000100125
064100100125       RETURN ESITO_OK;
064200100125
064300100125      /END-FREE
064400100125     P PutElenco       E
064500100125
064600100125
064700100125     P*--------------------------------------------------
064800100125     P* Procedure name: GetElenco
064900100125     P* Purpose:        Lettura pannello elenco.
065000100125     P* Returns:        Esito.
065100100125     P*--------------------------------------------------
065200100125     P GetElenco       B
065300100125     D GetElenco       PI            10I 0
065400100125
065500100127     D esito           S             10I 0
065600100127
065700100125      /FREE
065800100127
065900100125       READ elencoC;
066000100125
066100100125       SELECT;
066200100125         WHEN dspInf.aid = F3;
066300100125           RETURN dspInf.aidInt;
066400100125         WHEN dspInf.aid = F5;
066500100125           RETURN dspInf.aidInt;
066600100126         WHEN dspInf.aid = F7;
066700100129           IF dspInd.elencoCompresso;
066800100129             dspInd.elencoCompresso = *OFF;
066900100126           ELSE;
067000100129             dspInd.elencoCompresso = *ON;
067100100126           ENDIF;
067200100126           RETURN dspInf.aidInt;
067300100125         WHEN dspInf.aid = F10;
067400100126           RETURN RunElemento( '01' );
067500100125         WHEN dspInf.aid = F12;
067600100125           RETURN dspInf.aidInt;
067700100125       ENDSL;
067800100125
067900100125       DOU %EOF(tntb___d);
068000100125
068100100125         READC elencoS;
068200100125
068300100125         IF %EOF(tntb___d);
068400100125           LEAVE;
068500100125         ENDIF;
068600100125
068700100129         dsp_sflRcdNbr = dsp_sflCsrRrn;
068800100129
068900100125         esito = RunElemento( dsp_opzione
069000100125                            : dsp_tbeKe1
069100100125                            : dsp_tbeKe2
069200100125                            : dsp_tbeLin
069300100212                            : tntb___i.tbeSif
069400100125                            );
069500100127
069600100127         RESET dsp_opzione;
069700100127         dspInd.coloreRiga = dsp_hIn04;
069800100127         UPDATE elencoS;
069900100125
070000100127         SELECT;
070100100127           WHEN esito < ESITO_OK;
070200100127             LEAVE;
070300100127           WHEN esito = ESITO_WARNING_F3;
070400100127             LEAVE;
070500100127           WHEN esito = ESITO_WARNING_F12;
070600100127             LEAVE;
070700100127         ENDSL;
070800100127
070900100125       ENDDO;
071000100125
071100100127       RETURN esito;
071200100125
071300100125      /END-FREE
071400100125     P GetElenco       E
071500100125
071600100125
071700100125     P*--------------------------------------------------
071800100125     P* Procedure name: RunElemento
071900100125     P* Purpose:        Elaborazione dell'elemento scelto.
072000100125     P* Returns:        Esito.
072100100125     P* Parameter:      piOpzione => Codice operativo.
072200100125     P* Parameter:      piKe1 => Chiave 1.
072300100125     P* Parameter:      piKe2 => Chiave 2.
072400100125     P* Parameter:      piLin => Lingua.
072500100125     P* Parameter:      piSistemaInformativo => Sistema Informativo.
072600100125     P*--------------------------------------------------
072700100125     P RunElemento     B
072800100125     D RunElemento     PI            10I 0
072900100125     D  piOpzione                     2A   CONST
073000100125     D  piKe1                        15A   OPTIONS(*NOPASS)
073100100125     D  piKe2                        15A   OPTIONS(*NOPASS)
073200100125     D  piLin                         1A   OPTIONS(*NOPASS)
073300100125     D  piSistemaInformativo...
073400100125     D                               10A   OPTIONS(*NOPASS)
073500100125
073600100126     D nOpzione        S              3U 0
073700100127     D inzElemento     S               N   INZ(*ON)
073800100126
073900100125      /FREE
074000100125
074100100126       MONITOR;
074200100126         nOpzione = %INT(piOpzione);
074300100126       ON-ERROR;
074400100126       ENDMON;
074500100126
074600100126       RESET elemento;
074700100129
074800100129       IF %PARMS() > 1;
074900100129         dsp_tbeKe1New = piKe1;
075000100129         dsp_tbeKe2New = piKe2;
075100100129         dsp_tbeLinNew = piLin;
075200100129       ENDIF;
075300100126
075400100126       DOU dspInf.aid = F3;
075500100127
075600100127         IF inzElemento;
075700100127
075800100127           IF nOpzione = OPZIONE_NUOVO;
075900100127
076000100127             RESET dsp_tbeUni;
076100100127             RESET tbeAtb;
076200100127
076300100127           ELSE;
076400100127
076500100127             EXEC SQL
076600100127               SELECT TBEUNI
076700100127                    , TBEATB
076800100127                 INTO :dsp_tbeUni
076900100127                    , :tbeAtb
077000100127                 FROM TNTBE00F
077100100127                 WHERE TBECOD = :tntb___i.tbeCod
077200100127                   AND TBEKE1 = :piKe1
077300100127                   AND TBEKE2 = :piKe2
077400100127                   AND TBELIN = :piLin
077500100127                   AND TBESIF = :piSistemaInformativo
077600100127             ;
077700100127
077800100127             SELECT;
077900100127               WHEN sqlState = SQLSTATE_EMPTY_TABLE;
078000100127                 nOpzione = OPZIONE_NUOVO;
078100100127                 RESET dsp_tbeUni;
078200100127                 RESET tbeAtb;
078300100127               WHEN sqlState > '03';
078400100127                 DUMP(A);
078500100127                 RETURN sqlCode;
078600100127             ENDSL;
078700100129
078800100129             IF piLin = ITALIANO;
078900100129
079000100129               dsp_tbeUniIt1 = %SUBST(dsp_tbeUni : 1);
079100100129               dsp_tbeUniIt2 = %SUBST(dsp_tbeUni : 101);
079200100129               dsp_tbeUniIt3 = %SUBST(dsp_tbeUni : 201);
079300100129
079400100129             ELSE;
079500100129
079600100129               EXEC SQL
079700100129                 SELECT SUBSTR(TBEUNI, 1, 100)
079800100129                      , SUBSTR(TBEUNI, 101, 100)
079900100129                      , SUBSTR(TBEUNI, 201)
080000100129                   INTO :dsp_tbeUniIt1
080100100129                      , :dsp_tbeUniIt2
080200100129                      , :dsp_tbeUniIt3
080300100129                   FROM TNTBE00F
080400100129                   WHERE TBECOD = :tntb___i.tbeCod
080500100129                     AND TBEKE1 = :piKe1
080600100129                     AND TBEKE2 = :piKe2
080700100129                     AND TBELIN = ''
080800100129                     AND TBESIF = :piSistemaInformativo
080900100129               ;
081000100129
081100100129               SELECT;
081200100129                 WHEN sqlState = SQLSTATE_EMPTY_TABLE;
081300100129                   nOpzione = OPZIONE_NUOVO;
081400100129                   RESET dsp_tbeUniIt1;
081500100129                   RESET dsp_tbeUniIt2;
081600100129                   RESET dsp_tbeUniIt3;
081700100129                 WHEN sqlState > '03';
081800100129                   DUMP(A);
081900100129                   RETURN sqlCode;
082000100129               ENDSL;
082100100127
082200100129             ENDIF;
082300100129
082400100127           ENDIF;
082500100129
082600100129           SELECT;
082700100129             WHEN nOpzione = OPZIONE_NUOVO;
082800100129               dsp_titolo = 'Nuovo elemento';
082900100129               dspInd.chiaveElementoProtect = *OFF;
083000100129               dspInd.datiProtect = *OFF;
083100100129             WHEN nOpzione = OPZIONE_MODIFICARE;
083200100129               dspInd.chiaveElementoProtect = *ON;
083300100129               IF tbeAtb = *BLANK;
083400100129                 dsp_titolo = 'Modifica elemento';
083500100129                 dspInd.datiProtect = *OFF;
083600100129               ELSE;
083700100212                 dsp_titolo = 'Elemento annullato';
083800100129                 dspInd.datiProtect = *ON;
083900100129               ENDIF;
084000100129             WHEN nOpzione = OPZIONE_COPIARE;
084100100129               dsp_titolo = 'Copia elemento';
084200100129               dspInd.chiaveElementoProtect = *OFF;
084300100129               dspInd.datiProtect = *OFF;
084400100129             WHEN nOpzione = OPZIONE_ANNULLARE;
084500100129               IF tbeAtb = *BLANK;
084600100129                 dsp_titolo = 'Annullamento gruppo';
084700100129               ELSE;
084800100129                 dsp_titolo = 'Ripristino gruppo';
084900100129               ENDIF;
085000100129               dsp_tbeLinNew = TUTTO;
085100100129               dspInd.chiaveElementoProtect = *ON;
085200100129               dspInd.datiProtect = *ON;
085300100129             WHEN nOpzione = OPZIONE_VISUALIZZARE;
085400100129               dsp_titolo = 'Visualizzazione elemento';
085500100129               dspInd.chiaveElementoProtect = *ON;
085600100129               dspInd.datiProtect = *ON;
085700100129             WHEN nOpzione = OPZIONE_CANCELLARE;
085800100129               dsp_titolo = 'Cancellazione gruppo';
085900100129               dsp_tbeLinNew = TUTTO;
086000100129               dspInd.chiaveElementoProtect = *ON;
086100100129               dspInd.datiProtect = *ON;
086200100129             OTHER;
086300100129               dsp_titolo = 'Opzione ' + piOpzione + ' sconosciuta';
086400100129               dspInd.chiaveElementoProtect = *ON;
086500100129               dspInd.datiProtect = *ON;
086600100129           ENDSL;
086700100127
086800100127           inzElemento = *OFF;
086900100127
087000100127         ENDIF;
087100100126
087200100129         IF dsp_tbeLinNew = TUTTO;
087300100129           dsp_linDesIta = 'Tutte';
087400100129         ELSE;
087500100129           EXEC SQL
087600100129             SELECT LINDESITA
087700100129               INTO :dsp_linDesIta
087800100129               FROM AZLIN00F
087900100129               WHERE LINTNTBE = :dsp_tbeLinNew
088000100129           ;
088100100129           SELECT;
088200100129             WHEN sqlState > '03';
088300100129               DUMP(A);
088400100129           ENDSL;
088500100129         ENDIF;
088600100126
088700100126         EXFMT elemento;
088800100127
088900100127         RESET dspInd.elementoGiaEsistente;
089000100127         RESET dspInd.mancaElementoItaliano;
089100100127         RESET dspInd.erroreGenerico;
089200100126
089300100126         SELECT;
089400100126           WHEN dspInf.aid = F3;
089500100126             RETURN dspInf.aidInt;
089600100126           WHEN dspInf.aid = F5;
089700100127             inzElemento = *ON;
089800100126           WHEN dspInf.aid = F8;
089900100126             RETURN dspInf.aidInt;
090000100126           WHEN dspInf.aid = F12;
090100100126             RETURN dspInf.aidInt;
090200100126         ENDSL;
090300100127
090400100127         IF nOpzione = OPZIONE_NUOVO
090500100127         OR nOpzione = OPZIONE_COPIARE;
090600100127           IF dsp_tbeLinNew <> *BLANK;
090700100127             EXEC SQL
090800100127               SELECT COUNT(*)
090900100127                 INTO :count
091000100127                 FROM TNTBE00F
091100100127                 WHERE TBECOD = :tntb___i.tbeCod
091200100127                   AND TBEKE1 = :dsp_tbeKe1New
091300100127                   AND TBEKE2 = :dsp_tbeKe2New
091400100127                   AND TBELIN = ''
091500100212                   AND TBESIF = :tntb___i.tbeSif
091600100127             ;
091700100127             SELECT;
091800100127               WHEN sqlState > '03';
091900100127                 DUMP(A);
092000100127                 RETURN sqlCode;
092100100127             ENDSL;
092200100127             IF count = *ZERO;
092300100127               dspInd.mancaElementoItaliano = *ON;
092400100127               dspInd.erroreGenerico = *ON;
092500100127             ENDIF;
092600100127           ENDIF;
092700100127         ENDIF;
092800100127
092900100127         IF dspInf.aid = F6 AND NOT dspInd.erroreGenerico;
093000100127
093100100126           SELECT;
093200100127
093300100127             WHEN nOpzione = OPZIONE_NUOVO
093400100126               OR nOpzione = OPZIONE_COPIARE;
093500100127
093600100126               EXEC SQL
093700100126                 INSERT
093800100126                   INTO TNTBE00F
093900100126                      ( TBESIF
094000100126                      , TBELIN
094100100126                      , TBECOD
094200100126                      , TBEKE1
094300100126                      , TBEKE2
094400100126                      , TBEUNI
094500100126                      )
094600100212                   VALUES ( :tntb___i.tbeSif
094700100126                          , :dsp_tbeLinNew
094800100126                          , :tntb___i.tbeCod
094900100126                          , :dsp_tbeKe1New
095000100126                          , :dsp_tbeKe2New
095100100126                          , :dsp_tbeUni
095200100126                          )
095300100126               ;
095400100127
095500100126             WHEN nOpzione = OPZIONE_MODIFICARE;
095600100127
095700100126               EXEC SQL
095800100126                 UPDATE TNTBE00F
095900100126                   SET TBEUNI = :dsp_tbeUni
096000100126                     , TBEFTR = ''
096100100126                     , TBEDTR = 0
096200100126                   WHERE TBECOD = :tntb___i.tbeCod
096300100126                     AND TBEKE1 = :piKe1
096400100126                     AND TBEKE2 = :piKe2
096500100126                     AND TBELIN = :piLin
096600100126                     AND TBESIF = :piSistemaInformativo
096700100126               ;
096800100127
096900100126             WHEN nOpzione = OPZIONE_ANNULLARE;
097000100127
097100100126               EXEC SQL
097200100126                 UPDATE TNTBE00F
097300100126                   SET TBEATB = CASE TBEATB
097400100126                                WHEN ' ' THEN 'A'
097500100126                                ELSE ' '
097600100126                                END
097700100126                     , TBEFTR = ''
097800100126                     , TBEDTR = 0
097900100126                   WHERE TBECOD = :tntb___i.tbeCod
098000100126                     AND TBEKE1 = :piKe1
098100100126                     AND TBEKE2 = :piKe2
098200100126                     AND TBESIF = :piSistemaInformativo
098300100126               ;
098400100127
098500100126             WHEN nOpzione = OPZIONE_CANCELLARE;
098600100127
098700100126               EXEC SQL
098800100126                 DELETE
098900100126                   FROM TNTBE00F
099000100126                   WHERE TBECOD = :tntb___i.tbeCod
099100100126                     AND TBEKE1 = :piKe1
099200100126                     AND TBEKE2 = :piKe2
099300100126                     AND TBESIF = :piSistemaInformativo
099400100126               ;
099500100127
099600100126           ENDSL;
099700100127
099800100126           SELECT;
099900100127             WHEN sqlState = SQLSTATE_DUPLICATE_KEY_VALUE_SPECIFIED;
100000100127               dspInd.elementoGiaEsistente = *ON;
100100100127               dspInd.erroreGenerico = *ON;
100200100127             WHEN sqlState > '03';
100300100126               DUMP(A);
100400100126               RETURN sqlCode;
100500100126             OTHER;
100600100126               RETURN dspInf.aidInt;
100700100126           ENDSL;
100800100127
100900100126         ENDIF;
101000100125
101100100126       ENDDO;
101200100126
101300100125       RETURN ESITO_OK;
101400100125
101500100125      /END-FREE
101600100125     P RunElemento     E
101700100125
