000100070329      //********************************************************************************************
000200070329      //
000300070329      // Oggetto bollo: lettura.
000400070329      //
000500070329      //********************************************************************************************
000600070328     H NOMAIN
000700070330     H DECEDIT(*JOBRUN)
000800070328
000900070328      //********************************************************************************************
001000070328      //
001100070328      // Definizione costanti.
001200070328      //
001300070328      //********************************************************************************************
001400070328     D ESITO_OK...
001500070328     D                 C                   0
001600070330     D ESITO_WARNING_GENERICO...
001700070328     D                 C                   1
001800070330     D ESITO_WARNING_ESENZIONEIVA_NOBOLLO...
001900070330     D                 C                   2
002000070330     D ESITO_WARNING_IMPORTOBOLLO_NONTROVATO...
002100070330     D                 C                   3
002200070330     D ESITO_WARNING_SOCIETA_SENZA_BOLLOVIRTUALE...
002300070330     D                 C                   4
002400070330     D ESITO_ERRORE_GENERICO...
002500070328     D                 C                   -1
002600070330     D ESITO_ERRORE_TIPOBOLLO_NONTROVATO...
002700070330     D                 C                   -2
002800070329     D BOLLO_FATTURA...
002900070329     D                 C                   'F'
003000070328
003100070328      //********************************************************************************************
003200070328      //
003300070328      // Definizione prototipi.
003400070328      //
003500070328      //********************************************************************************************
003600070328      /COPY GAITRASRC/SRCPROTOPR,YCOBOLLOI
003700070328
003800070328      //********************************************************************************************
003900070328      //
004000070328      // Definizione strutture dati.
004100070328      //
004200070328      //********************************************************************************************
004300070328     D ynbotp00f     E DS                  INZ QUALIFIED
004400070328     D ynbosc00f     E DS                  INZ QUALIFIED
004500070328     D ynbofe00f     E DS                  INZ QUALIFIED
004600070328     D ynbovi00f     E DS                  INZ QUALIFIED
004700070328
004800070328      //********************************************************************************************
004900070328      //
005000070328      // Definizione variabili globali.
005100070328      //
005200070328      //********************************************************************************************
005300070329     D idBollo...
005400070329     D                 S                   LIKE(ynbotp00f.idBollo)
005500070329     D dataValidita...
005600070329     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
005700070329     D societa...
005800070329     D                 S                   LIKE(ynbovi00f.societa)
005900070329     D imponibileBollo...
006000070329     D                 S                   LIKE(ynbosc00f.impInfer)
006100070330
006200080729     D*--------------------------------------------------
006300080729     D* Procedure name: Bollo_i_newTipoBolloByIdBollo
006400080729     D* Purpose:        Nuovo bollo reperito con ID bollo e data validità.
006500080729     D* Returns:        Esito.
006600080729     D*--------------------------------------------------
006700080729     D Bollo_i_newTipoBolloByIdBollo...
006800080729     D                 PR            10I 0
006900080729     D  piIdBollo                     1A   CONST
007000080729     D piDataValidita                10D   DATFMT(*ISO)
007100080729     D                                     CONST
007200080729
007300080729     D*--------------------------------------------------
007400080729     D* Procedure name: Bollo_i_newEsenzioneIVAByRiferimento
007500080729     D* Purpose:        Nuova esenzione IVA.
007600080729     D* Returns:        Esito.
007700080729     D* Parameter:      piRiferimentoIVA => Riferimento esenzione IVA
007800080729     D*--------------------------------------------------
007900080729     D Bollo_i_newEsenzioneIVAByRiferimento...
008000080729     D                 PR            10I 0
008100080729     D  piIdBollo                     1A   CONST
008200080729     D piDataValidita                10D   DATFMT(*ISO)
008300080729     D                                     CONST
008400080729     D  piRiferimentoIVA...
008500080729     D                                3A
008600080729
008700080729     D*--------------------------------------------------
008800080729     D* Procedure name: Bollo_i_newImportoBolloByImponibile
008900080729     D* Purpose:        Nuovo importo bollo per imponibile.
009000080729     D* Returns:        Esito.
009100080729     D* Parameter:      piIdBollo => ID bollo
009200080729     D* Parameter:      piDataValidita => Data validità.
009300080729     D* Parameter:      piImponibileBollo => Imponibile bollo.
009400080729     D*--------------------------------------------------
009500080729     D Bollo_i_newImportoBolloByImponibile...
009600080729     D                 PR            10I 0
009700080729     D  piIdBollo                     1A   CONST
009800080729     D piDataValidita                10D   DATFMT(*ISO)
009900080729     D                                     CONST
010000080729     D  piImponibileBollo...
010100080729     D                               15P 2
010200080729
010300080729     D*--------------------------------------------------
010400080729     D* Procedure name: Bollo_i_setIdBollo
010500080729     D* Purpose:        Imposta l' ID bollo.
010600080729     D* Returns:        Esito.
010700080729     D* Parameter:      piIdBollo => ID bollo.
010800080729     D*--------------------------------------------------
010900080729     D Bollo_i_setIdBollo...
011000080729     D                 PR            10I 0
011100080729     D  piIdBollo                     1A
011200080729
011300080729     D*--------------------------------------------------
011400080729     D* Procedure name: Bollo_i_setDataValidita
011500080729     D* Purpose:        Imposta la data per controlli validità.
011600080729     D* Returns:        Esito.
011700080729     D* Parameter:      piDataValidita => Data validita
011800080729     D*--------------------------------------------------
011900080729     D Bollo_i_setDataValidita...
012000080729     D                 PR            10I 0
012100080729     D piDataValidita                10D   DATFMT(*ISO)
012200080729
012300080729     D*--------------------------------------------------
012400080729     D* Procedure name: Bollo_i_setSocieta
012500080729     D* Purpose:        Imposta la societa.
012600080729     D* Returns:        Esito.
012700080729     D* Parameter:      piSocieta
012800080729     D*--------------------------------------------------
012900080729     D Bollo_i_setSocieta...
013000080729     D                 PR            10I 0
013100080729     D  piSocieta                     3A
013200080729
013300080729     D*--------------------------------------------------
013400080729     D* Procedure name: Bollo_i_getIdBollo
013500080729     D* Purpose:        Reperisce l' ID bollo in uso.
013600080729     D* Returns:        ID bollo.
013700080729     D*--------------------------------------------------
013800080729     D Bollo_i_getIdBollo...
013900080729     D                 PR             1A
014000080729
014100080729     D*--------------------------------------------------
014200080729     D* Procedure name: Bollo_i_getDataValidità
014300080729     D* Purpose:
014400080729     D* Returns:        Data validità.
014500080729     D*--------------------------------------------------
014600080729     D Bollo_i_getDataValidita...
014700080729     D                 PR            10D
014800080729
014900080729     D*--------------------------------------------------
015000080729     D* Procedure name: Bollo_i_getSocieta
015100080729     D* Purpose:        Restituisce la societa.
015200080729     D* Returns:        Società.
015300080729     D*--------------------------------------------------
015400080729     D Bollo_i_getSocieta...
015500080729     D                 PR             3A
015600080729
015700080729
015800070328     P*--------------------------------------------------
015900070328     P* Procedure name: Bollo_i_init
016000070328     P* Purpose:        Inizializzazioni.
016100070328     P* Returns:        Esito.
016200070328     P*--------------------------------------------------
016300070328     P Bollo_i_init    B                   EXPORT
016400070328     D Bollo_i_init    PI            10I 0
016500070329     D  piIdBollo                     1A   CONST
016600070329     D                                     OPTIONS(*NOPASS:*OMIT)
016700070329     D  piDataValidita...
016800070329     D                               10D   DATFMT(*ISO)
016900070329     D                                     CONST
017000070329     D                                     OPTIONS(*NOPASS:*OMIT)
017100070329     D  piSocieta                     3A
017200070329     D                                     OPTIONS(*NOPASS:*OMIT)
017300070330     D                                     CONST
017400070328
017500070329     D lclIdBollo...
017600070329     D                 S                   LIKE(ynbotp00f.idBollo)
017700070329     D lclDataValidita...
017800070329     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
017900070330     D lclSocieta...
018000070330     D                 S                   LIKE(ynbovi00f.societa)
018100070329
018200070328      /FREE
018300070329
018400070329       RESET idBollo;
018500070329       RESET dataValidita;
018600070329       RESET societa;
018700070329       RESET imponibileBollo;
018800070329       RESET ynbotp00f;
018900070329       RESET ynbosc00f;
019000070329       RESET ynbofe00f;
019100070329       RESET ynbovi00f;
019200070329
019300070329       IF %PARMS() > 0 AND %ADDR( piIdBollo ) <> *NULL;
019400070329         lclIdBollo = piIdBollo;
019500070329         IF Bollo_i_setIdBollo( lclIdBollo ) < *ZERO;
019600070330           RETURN ESITO_ERRORE_GENERICO;
019700070329         ENDIF;
019800070329       ENDIF;
019900070329
020000070329       IF %PARMS() > 1 AND %ADDR( piDataValidita ) <> *NULL;
020100070329         lclDataValidita = piDataValidita;
020200070329         IF Bollo_i_setDataValidita( lclDataValidita ) < *ZERO;
020300070330           RETURN ESITO_ERRORE_GENERICO;
020400070329         ENDIF;
020500070329       ENDIF;
020600070329
020700070329       IF %PARMS() > 2 AND %ADDR( piSocieta ) <> *NULL;
020800070330         lclSocieta = piSocieta;
020900070330         IF Bollo_i_setSocieta( lclSocieta ) < *ZERO;
021000070330           RETURN ESITO_ERRORE_GENERICO;
021100070329         ENDIF;
021200070329       ENDIF;
021300070329
021400070330       IF Bollo_i_getIdBollo() <> *BLANK
021500070330       AND Bollo_i_newTipoBolloByIdBollo( Bollo_i_getIdBollo()
021600070330                                        : Bollo_i_getDataValidita() ) < 0;
021700070330         RETURN ESITO_ERRORE_GENERICO;
021800070329       ENDIF;
021900070329
022000070330       IF Bollo_i_getSocieta() <> *BLANK
022100070330       AND Bollo_i_newVirtualeBySocieta( Bollo_i_getSocieta()
022200070330                                       : Bollo_i_getDataValidita() ) < 0;
022300070330         RETURN ESITO_ERRORE_GENERICO;
022400070329       ENDIF;
022500070328
022600070328       RETURN ESITO_OK;
022700070328
022800070328      /END-FREE
022900070328     P Bollo_i_init    E
023000070328
023100070328     P*--------------------------------------------------
023200070328     P* Procedure name: Bollo_i_finalize
023300070328     P* Purpose:        Operazioni finali.
023400070328     P* Returns:        Esito.
023500070328     P*--------------------------------------------------
023600070328     P Bollo_i_finalize...
023700070328     P                 B                   EXPORT
023800070328     D Bollo_i_finalize...
023900070328     D                 PI            10I 0
024000070328
024100070328      /FREE
024200070328
024300070328       RETURN ESITO_OK;
024400070328
024500070328      /END-FREE
024600070328     P Bollo_i_finalize...
024700070328     P                 E
024800070328
024900070329     P*--------------------------------------------------
025000070329     P* Procedure name: Bollo_i_newDocumento
025100070329     P* Purpose:        Nuovo documento.
025200070329     P* Returns:        Esito.
025300070329     P* Parameter:      piDataDocumento => Data documento
025400070329     P*--------------------------------------------------
025500070329     P Bollo_i_newDocumento...
025600070329     P                 B                   EXPORT
025700070329     D Bollo_i_newDocumento...
025800070329     D                 PI            10I 0
025900070329     D  piIdBollo                     1A   CONST
026000070329     D                                     OPTIONS(*NOPASS:*OMIT)
026100070329     D  piDataValidita...
026200070329     D                               10D   DATFMT(*ISO)
026300070329     D                                     CONST
026400070329     D                                     OPTIONS(*NOPASS:*OMIT)
026500070330     D  piSocieta                     3A   CONST
026600070329     D                                     OPTIONS(*NOPASS:*OMIT)
026700070330
026800070330     D lclIdBollo...
026900070330     D                 S                   LIKE(ynbotp00f.idBollo)
027000070330     D lclDataValidita...
027100070330     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
027200070330     D lclSocieta...
027300070330     D                 S                   LIKE(ynbovi00f.societa)
027400070329
027500070329      /FREE
027600070329
027700070330       // Per ogni documento l'utente può cambiare il tipo bollo, oppure la data
027800070330       // del documento, oppure la società.
027900070330
028000070329       IF %PARMS() > 0 AND %ADDR( piIdBollo ) <> *NULL;
028100070330         lclIdBollo = piIdBollo;
028200070330         IF Bollo_i_setIdBollo( lclIdBollo ) < *ZERO;
028300070330           RETURN ESITO_ERRORE_GENERICO;
028400070329         ENDIF;
028500070329       ENDIF;
028600070329
028700070329       IF %PARMS() > 1 AND %ADDR( piDataValidita ) <> *NULL;
028800070330         lclDataValidita = piDataValidita;
028900070330         IF Bollo_i_setDataValidita( lclDataValidita ) < *ZERO;
029000070330           RETURN ESITO_ERRORE_GENERICO;
029100070329         ENDIF;
029200070329       ENDIF;
029300070329
029400070329       IF %PARMS() > 2 AND %ADDR( piSocieta ) <> *NULL;
029500070330         lclSocieta = piSocieta;
029600070330         IF Bollo_i_setSocieta( lclSocieta ) < *ZERO;
029700070330           RETURN ESITO_ERRORE_GENERICO;
029800070329         ENDIF;
029900070329       ENDIF;
030000070330
030100070330       IF Bollo_i_getIdBollo() <> *BLANK
030200070330       AND Bollo_i_newTipoBolloByIdBollo( Bollo_i_getIdBollo()
030300070330                                        : Bollo_i_getDataValidita() ) < 0;
030400070330         RETURN ESITO_ERRORE_GENERICO;
030500070330       ENDIF;
030600070330
030700070330       IF Bollo_i_getSocieta() <> *BLANK
030800070330       AND Bollo_i_newVirtualeBySocieta( Bollo_i_getSocieta()
030900070330                                       : Bollo_i_getDataValidita() ) < 0;
031000070330         RETURN ESITO_ERRORE_GENERICO;
031100070330       ENDIF;
031200070329
031300070329       RESET imponibileBollo;
031400070329
031500070329       RETURN ESITO_OK;
031600070329
031700070329      /END-FREE
031800070329     P Bollo_i_newDocumento...
031900070329     P                 E
032000070329
032100070329     P*--------------------------------------------------
032200070329     P* Procedure name: Bollo_i_endDocumento
032300070329     P* Purpose:        Fine documento.
032400070329     P* Returns:        Esito.
032500070329     P*--------------------------------------------------
032600070329     P Bollo_i_endDocumento...
032700070329     P                 B                   EXPORT
032800070329     D Bollo_i_endDocumento...
032900070329     D                 PI            10I 0
033000070329
033100070329      /FREE
033200070329
033300070330       // Il documento (p.e. fattura) è finito, quindi ho l'imponibile
033400070330       // su cui calcolare il bollo.
033500070330       IF Bollo_i_newImportoBolloByImponibile( Bollo_i_getIdBollo()
033600070330       : Bollo_i_getDataValidita(): imponibileBollo ) < *ZERO;
033700070330         RETURN ESITO_ERRORE_GENERICO;
033800070329       ENDIF;
033900070329
034000070329       RETURN ESITO_OK;
034100070329
034200070329      /END-FREE
034300070329     P Bollo_i_endDocumento...
034400070329     P                 E
034500070329
034600070329     P*--------------------------------------------------
034700070329     P* Procedure name: Bollo_i_newTipoBolloByIdBollo
034800070329     P* Purpose:        Nuovo bollo reperito con ID bollo e data validità.
034900070329     P* Returns:        Esito.
035000070329     P*--------------------------------------------------
035100070329     P Bollo_i_newTipoBolloByIdBollo...
035200070329     P                 B
035300070329     D Bollo_i_newTipoBolloByIdBollo...
035400070329     D                 PI            10I 0
035500070330     D  piIdBollo                     1A   CONST
035600070330     D piDataValidita                10D   DATFMT(*ISO) CONST
035700070329
035800070329      /FREE
035900070329
036000070329       IF piIdBollo = *BLANK;
036100070330         RETURN ESITO_ERRORE_GENERICO;
036200070329       ENDIF;
036300070329
036400070329       IF piIdBollo = ynbotp00f.idBollo AND piDataValidita >= ynbotp00f.dtInVal
036500070329       AND piDataValidita <= ynbotp00f.dtFiVal;
036600070329         RETURN ESITO_OK;
036700070329       ENDIF;
036800070329
036900070329      /END-FREE
037000070329     C/EXEC SQL
037100070329     C+ SELECT *
037200070329     C+ INTO :ynbotp00f
037300070329     C+ FROM YNBOTP00F
037400070329     C+ WHERE IDBOLLO = :piIdBollo
037500070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
037600070329     C/END-EXEC
037700070329      /FREE
037800070329
037900070330       SELECT;
038000070330         WHEN sqlCode < 0;
038100070330           RESET ynbotp00f;
038200070330           RETURN sqlCode;
038300070330         WHEN sqlCode = 100;
038400070330           RESET ynbotp00f;
038500070330           RETURN ESITO_ERRORE_TIPOBOLLO_NONTROVATO;
038600070330       ENDSL;
038700070329
038800070329       RETURN ESITO_OK;
038900070329
039000070329      /END-FREE
039100070329     P Bollo_i_newTipoBolloByIdBollo...
039200070329     P                 E
039300070329
039400070329     P*--------------------------------------------------
039500070329     P* Procedure name: Bollo_i_newVirtualeBySocieta
039600070329     P* Purpose:        Nuova autorizzazione bollo virtuale per società.
039700070329     P* Returns:        Esito.
039800070329     P*--------------------------------------------------
039900070329     P Bollo_i_newVirtualeBySocieta...
040000070329     P                 B                   EXPORT
040100070329     D Bollo_i_newVirtualeBySocieta...
040200070329     D                 PI            10I 0
040300070330     D  piSocieta                     3A   CONST
040400070329     D  piDataValidita...
040500070330     D                               10D   DATFMT(*ISO) CONST
040600070329
040700070329      /FREE
040800070329
040900070329       IF piSocieta = ynbovi00f.societa AND piDataValidita >= ynbovi00f.dtInVal
041000070329       AND piDataValidita <= ynbovi00f.dtFiVal;
041100070329         RETURN ESITO_OK;
041200070329       ENDIF;
041300070329
041400070329      /END-FREE
041500070329     C/EXEC SQL
041600070329     C+ SELECT *
041700070329     C+ INTO :ynbovi00f
041800070329     C+ FROM YNBOVI00F
041900070329     C+ WHERE SOCIETA = :piSocieta
042000070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
042100070329     C/END-EXEC
042200070329      /FREE
042300070329
042400070329       SELECT;
042500070329         WHEN sqlCode < 0;
042600070329           RESET ynbovi00f;
042700070330           RETURN sqlCode;
042800070329         WHEN sqlCode = 100;
042900070329           RESET ynbovi00f;
043000070330           RETURN ESITO_WARNING_SOCIETA_SENZA_BOLLOVIRTUALE;
043100070329       ENDSL;
043200070329
043300070329       RETURN ESITO_OK;
043400070329
043500070329      /END-FREE
043600070329     P Bollo_i_newVirtualeBySocieta...
043700070329     P                 E
043800070329
043900070329     P*--------------------------------------------------
044000070329     P* Procedure name: Bollo_i_newEsenzioneIVAByRiferimento
044100070329     P* Purpose:        Nuova esenzione IVA per riferimento.
044200070329     P* Returns:        Esito.
044300070329     P* Parameter:      piRiferimentoIVA => Riferimento esenzione IVA
044400070329     P*--------------------------------------------------
044500070329     P Bollo_i_newEsenzioneIVAByRiferimento...
044600070329     P                 B
044700070329     D Bollo_i_newEsenzioneIVAByRiferimento...
044800070329     D                 PI            10I 0
044900070330     D  piIdBollo                     1A   CONST
045000070330     D piDataValidita                10D   DATFMT(*ISO) CONST
045100070329     D  piRiferimentoIVA...
045200070329     D                                3A
045300070329
045400070329      /FREE
045500070329
045600070329       IF piRiferimentoIVA <> ynbofe00f.rifIVA
045700070329       OR piDataValidita < ynbofe00f.dtInVal
045800070329       OR piDataValidita > ynbofe00f.dtFiVal
045900070329       OR piIdBollo <> ynbofe00f.idBollo;
046000070329      /END-FREE
046100070329     C/EXEC SQL
046200070329     C+ SELECT *
046300070329     C+ INTO :ynbofe00f
046400070329     C+ FROM YNBOFE00F
046500070329     C+ WHERE IDBOLLO = :idBollo
046600070329     C+   AND :dataValidita BETWEEN DTINVAL AND DTFIVAL
046700070329     C+   AND RIFIVA = :piRiferimentoIVA
046800070329     C/END-EXEC
046900070329      /FREE
047000070329         SELECT;
047100070329           WHEN sqlCode < 0;
047200070329             RESET ynbofe00f;
047300070330             RETURN sqlCode;
047400070329           WHEN sqlCode = 100;
047500070330             RETURN ESITO_WARNING_ESENZIONEIVA_NOBOLLO;
047600070329         ENDSL;
047700070329       ENDIF;
047800070329
047900070329       RETURN ESITO_OK;
048000070329
048100070329      /END-FREE
048200070329     P Bollo_i_newEsenzioneIVAByRiferimento...
048300070329     P                 E
048400070329
048500070329     P*--------------------------------------------------
048600070329     P* Procedure name: Bollo_i_newImportoBolloByImponibile
048700070329     P* Purpose:        Nuovo importo bollo per imponibile.
048800070329     P* Returns:        Esito.
048900070329     P* Parameter:      piIdBollo => ID bollo
049000070329     P* Parameter:      piDataValidita => Data validità.
049100070329     P* Parameter:      piImponibileBollo => Imponibile bollo.
049200070329     P*--------------------------------------------------
049300070329     P Bollo_i_newImportoBolloByImponibile...
049400070329     P                 B
049500070329     D Bollo_i_newImportoBolloByImponibile...
049600070329     D                 PI            10I 0
049700070330     D  piIdBollo                     1A   CONST
049800070330     D piDataValidita                10D   DATFMT(*ISO) CONST
049900070329     D  piImponibileBollo...
050000070329     D                               15P 2
050100070329
050200070329      /FREE
050300070329
050400070329       IF piImponibileBollo < ynbosc00f.impInfer
050500070329       OR piImponibileBollo > ynbosc00f.impSuper
050600070329       OR piDataValidita < ynbosc00f.dtInVal
050700070329       OR piDataValidita > ynbosc00f.dtFiVal
050800070329       OR piIdBollo <> ynbosc00f.idBollo;
050900070329      /END-FREE
051000070329     C/EXEC SQL
051100070329     C+ SELECT *
051200070329     C+ INTO :ynbosc00f
051300070329     C+ FROM YNBOSC00F
051400070329     C+ WHERE IDBOLLO = :piIdBollo
051500070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
051600070329     C+   AND :piImponibileBollo BETWEEN IMPINFER AND IMPSUPER
051700070329     C/END-EXEC
051800070329      /FREE
051900070329         SELECT;
052000070329           WHEN sqlCode < 0;
052100070329             RESET ynbosc00f;
052200070330             RETURN sqlCode;
052300070329           WHEN sqlCode = 100;
052400070329             RESET ynbosc00f;
052500070330             RETURN ESITO_WARNING_IMPORTOBOLLO_NONTROVATO;
052600070329         ENDSL;
052700070329       ENDIF;
052800070329
052900070329       RETURN ESITO_OK;
053000070329
053100070329      /END-FREE
053200070329     P Bollo_i_newImportoBolloByImponibile...
053300070329     P                 E
053400070329
053500070329     P*--------------------------------------------------
053600070329     P* Procedure name: Bollo_i_addImponibileEsenteIVA
053700070329     P* Purpose:        Aggiunge un imponibile esente IVA.
053800070329     P* Returns:        Esito.
053900070329     P* Parameter:      idRiferimentoIVA => Riferimento IVA
054000070329     P* Parameter:      piImponibileIVA => Imponibile IVA.
054100070329     P*--------------------------------------------------
054200070329     P Bollo_i_addImponibileEsenteIVA...
054300070329     P                 B                   EXPORT
054400070329     D Bollo_i_addImponibileEsenteIVA...
054500070329     D                 PI            10I 0
054600070329     D  piRiferimentoIVA...
054700070329     D                                3A
054800070329     D  piImponibileIVA...
054900070330     D                               15P 2 VALUE
055000070329
055100070329      /FREE
055200070329
055300070330       IF Bollo_i_newEsenzioneIVAByRiferimento( BOLLO_FATTURA
055400070330       : Bollo_i_getDataValidita() : piRiferimentoIVA ) = *ZERO;
055500070329         imponibileBollo += piImponibileIVA;
055600070329       ENDIF;
055700070329
055800070329       RETURN ESITO_OK;
055900070329
056000070329      /END-FREE
056100070329     P Bollo_i_addImponibileEsenteIVA...
056200070329     P                 E
056300070329
056400070329     P*--------------------------------------------------
056500070329     P* Procedure name: Bollo_i_getImportoBollo
056600070329     P* Purpose:        Reperisce l' importo del bollo.
056700070329     P* Returns:        Importo bollo.
056800070329     P*--------------------------------------------------
056900070329     P Bollo_i_getImportoBollo...
057000070329     P                 B                   EXPORT
057100070329     D Bollo_i_getImportoBollo...
057200070329     D                 PI            15P 2
057300070329
057400070329      /FREE
057500070329
057600070329       RETURN ynbosc00f.impBollo;
057700070329
057800070329      /END-FREE
057900070329     P Bollo_i_getImportoBollo...
058000070329     P                 E
058100070329
058200070329     P*--------------------------------------------------
058300070329     P* Procedure name: Bollo_i_isVirtuale
058400070329     P* Purpose:        Dice se la società ha il bollo virtuale.
058500070329     P* Returns:        *ON = bollo virtuale.
058600070329     P* Parameter:      piSocieta => Società.
058700070329     P* Parameter:      piEsito => Esito.
058800070329     P*--------------------------------------------------
058900070329     P Bollo_i_isVirtuale...
059000070329     P                 B                   EXPORT
059100070329     D Bollo_i_isVirtuale...
059200070329     D                 PI              N
059300070329
059400070329      /FREE
059500070329
059600070329       RETURN ((%LEN(ynbovi00f.autorNum) > *ZERO)
059700070329                AND (ynbovi00f.autorNum <> *BLANK));
059800070330
059900070329      /END-FREE
060000070329     P Bollo_i_isVirtuale...
060100070329     P                 E
060200070329
060300070329     P*--------------------------------------------------
060400070329     P* Procedure name: Bollo_i_setIdBollo
060500070329     P* Purpose:        Imposta l' ID bollo.
060600070329     P* Returns:        Esito.
060700070329     P* Parameter:      piIdBollo => ID bollo.
060800070329     P*--------------------------------------------------
060900070329     P Bollo_i_setIdBollo...
061000070329     P                 B
061100070329     D Bollo_i_setIdBollo...
061200070329     D                 PI            10I 0
061300070329     D  piIdBollo                     1A
061400070329
061500070329      /FREE
061600070329
061700070329       idBollo = piIdBollo;
061800070329
061900070329       RETURN ESITO_OK;
062000070329
062100070329      /END-FREE
062200070329     P Bollo_i_setIdBollo...
062300070329     P                 E
062400070329
062500070329     P*--------------------------------------------------
062600070329     P* Procedure name: Bollo_i_setDataValidita
062700070329     P* Purpose:        Imposta la data per controlli validità.
062800070329     P* Returns:        Esito.
062900070329     P* Parameter:      piDataValidita => Data validita
063000070329     P*--------------------------------------------------
063100070329     P Bollo_i_setDataValidita...
063200070329     P                 B
063300070329     D Bollo_i_setDataValidita...
063400070329     D                 PI            10I 0
063500070329     D piDataValidita                10D   DATFMT(*ISO)
063600070329
063700070329      /FREE
063800070329
063900070329       dataValidita = piDataValidita;
064000070329
064100070329       RETURN ESITO_OK;
064200070329
064300070329      /END-FREE
064400070329     P Bollo_i_setDataValidita...
064500070329     P                 E
064600070329
064700070329     P*--------------------------------------------------
064800070329     P* Procedure name: Bollo_i_setSocieta
064900070329     P* Purpose:        Imposta la societa.
065000070329     P* Returns:        Esito.
065100070329     P* Parameter:      piSocieta
065200070329     P*--------------------------------------------------
065300070329     P Bollo_i_setSocieta...
065400070329     P                 B
065500070329     D Bollo_i_setSocieta...
065600070329     D                 PI            10I 0
065700070329     D  piSocieta                     3A
065800070329
065900070329      /FREE
066000070329
066100070329       societa = piSocieta;
066200070329
066300070329       RETURN ESITO_OK;
066400070329
066500070329      /END-FREE
066600070329     P Bollo_i_setSocieta...
066700070329     P                 E
066800070329
066900070330
067000070330     P*--------------------------------------------------
067100070330     P* Procedure name: Bollo_i_getIdBollo
067200070330     P* Purpose:        Reperisce l' ID bollo in uso.
067300070330     P* Returns:        ID bollo.
067400070330     P*--------------------------------------------------
067500070330     P Bollo_i_getIdBollo...
067600070330     P                 B
067700070330     D Bollo_i_getIdBollo...
067800070330     D                 PI             1A
067900070330
068000070330      /FREE
068100070330
068200070330       RETURN idBollo;
068300070330
068400070330      /END-FREE
068500070330     P Bollo_i_getIdBollo...
068600070330     P                 E
068700070330
068800070330
068900070330     P*--------------------------------------------------
069000070330     P* Procedure name: Bollo_i_getDataValidità
069100070330     P* Purpose:
069200070330     P* Returns:        Data validità.
069300070330     P*--------------------------------------------------
069400070330     P Bollo_i_getDataValidita...
069500070330     P                 B
069600070330     D Bollo_i_getDataValidita...
069700070330     D                 PI            10D
069800070330
069900070330      /FREE
070000070330
070100070330       RETURN dataValidita;
070200070330
070300070330      /END-FREE
070400070330     P Bollo_i_getDataValidita...
070500070330     P                 E
070600070330
070700070330
070800070330     P*--------------------------------------------------
070900070330     P* Procedure name: Bollo_i_getSocieta
071000070330     P* Purpose:        Restituisce la societa.
071100070330     P* Returns:        Società.
071200070330     P*--------------------------------------------------
071300070330     P Bollo_i_getSocieta...
071400070330     P                 B
071500070330     D Bollo_i_getSocieta...
071600070330     D                 PI             3A
071700070330
071800070330      /FREE
071900070330
072000070330       RETURN societa;
072100070330
072200070330      /END-FREE
072300070330     P Bollo_i_getSocieta...
072400070330     P                 E
072500070330
072600070330     P*--------------------------------------------------
072700070330     P* Procedure name: Bollo_i_getDichiarazioneBolloVirtuale
072800070330     P* Purpose:        Reperisce il testo della dichiarazione del bollo vi...
072900070330     P*                          rtuale.
073000070330     P* Returns:        Testo dichiarazione bollo virtuale.
073100070330     P*--------------------------------------------------
073200070330     P Bollo_i_getDichiarazioneBolloVirtuale...
073300070330     P                 B                   EXPORT
073400070330     D Bollo_i_getDichiarazioneBolloVirtuale...
073500070330     D                 PI           255A   VARYING
073600070330
073700070330      /FREE
073800070330
073900070330       IF Bollo_i_isVirtuale();
074000080904         RETURN 'Imposta di bollo assolta in modo virtuale.'
074200080904                + ' Autorizzazione '
074300070330                + ynbovi00f.autorNum + ' '
074400080904                + %CHAR(ynbovi00f.autorData : *EUR) + ' '
074500070330                + ynbovi00f.autorProv + '.';
074600070330       ELSE;
074700070330         RETURN *BLANK;
074800070330       ENDIF;
074900070330
075000070330      /END-FREE
075100070330     P Bollo_i_getDichiarazioneBolloVirtuale...
075200070330     P                 E
075300070330
075400070330     P*--------------------------------------------------
075500070330     P* Procedure name: Bollo_i_getDatiRegistrazioneCosto
075600070330     P* Purpose:        Restituisce i dati contabili per registrare il cost...
075700070330     P*                          o del bollo.
075800070330     P* Returns:        Esito.
075900070330     P* Parameter:      piCapoContoCosto => Capo conto costo.
076000070330     P* Parameter:      piSottoContoCosto => Sotto conto costo.
076100070330     P* Parameter:      piCausaleCosto => Causale costo.
076200070330     P* Parameter:      piVoceCosto => Voce co.an. costo.
076300070330     P*--------------------------------------------------
076400070330     P Bollo_i_getDatiRegistrazioneCosto...
076500070330     P                 B                   EXPORT
076600070330     D Bollo_i_getDatiRegistrazioneCosto...
076700070330     D                 PI            10I 0
076800070330     D  piCapoContoCosto...
076900070330     D                                6A
077000070330     D  piSottoContoCosto...
077100070330     D                                8A
077200070330     D piCausaleCosto                 4A
077300070330     D  piVoceCosto                   8A
077400070330
077500070330      /FREE
077600070330
077700070330       piCapoContoCosto = ynbotp00f.costoKcc;
077800070330       piSottoContoCosto = ynbotp00f.costoKsc;
077900070330       piCausaleCosto = ynbotp00f.costoCaus;
078000070330       piVoceCosto = ynbotp00f.costoVoce;
078100070330
078200070330       RETURN ESITO_OK;
078300070330
078400070330      /END-FREE
078500070330     P Bollo_i_getDatiRegistrazioneCosto...
078600070330     P                 E
078700070330
078800070330     P*--------------------------------------------------
078900070330     P* Procedure name: Bollo_i_getDatiRegistrazioneRicavo
079000070330     P* Purpose:        Restituisce i dati per registrare il recupero del b...
079100070330     P*                          ollo.
079200070330     P* Returns:        Esito.
079300070330     P* Parameter:      piCapoContoRicavo => Capo conto ricavo.
079400070330     P* Parameter:      piSottoContoRicavo => Sotto conto ricavo.
079500070330     P* Parameter:      piCausaleRicavo => Causale ricavo.
079600070330     P* Parameter:      piVoceRicavo => Voce co.an. ricavo.
079700070330     P*--------------------------------------------------
079800070330     P Bollo_i_getDatiRegistrazioneRicavo...
079900070330     P                 B                   EXPORT
080000070330     D Bollo_i_getDatiRegistrazioneRicavo...
080100070330     D                 PI            10I 0
080200070330     D  piCapoContoRicavo...
080300070330     D                                6A
080400070330     D  piSottoContoRicavo...
080500070330     D                                8A
080600070330     D  piCausaleRicavo...
080700070330     D                                4A
080800070330     D  piVoceRicavo                  8A
080900070330     D  piEsenzioneIVA...
081000070330     D                                3A
081100070330
081200070330      /FREE
081300070330
081400070330       piCapoContoRicavo = ynbotp00f.ricavoKcc;
081500070330       piSottoContoRicavo = ynbotp00f.ricavoKsc;
081600070330       piCausaleRicavo = ynbotp00f.ricavoCaus;
081700070330       piVoceRicavo = ynbotp00f.ricavoVoce;
081800070330       piEsenzioneIVA = ynbotp00f.rifIva;
081900070330
082000070330       RETURN ESITO_OK;
082100070330
082200070330      /END-FREE
082300070330     P Bollo_i_getDatiRegistrazioneRicavo...
082400070330     P                 E
082500070330
082600070330     P*--------------------------------------------------
082700070330     P* Procedure name: Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale
082800070330     P* Purpose:        Restituisce i dati per la registrazione di debito v...
082900070330     P*                          erso l'erario per il bollo virtuale.
083000070330     P* Returns:        Esito.
083100070330     P* Parameter:      piCapoContoDebito => Capo conto debito verso erario.
083200070330     P* Parameter:      piSottoContoDebito => Sotto conto debito verso erario.
083300070330     P* Parameter:      piCausaleDebito => Causale debito verso erario.
083400070330     P*--------------------------------------------------
083500070330     P Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale...
083600070330     P                 B                   EXPORT
083700070330     D Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale...
083800070330     D                 PI            10I 0
083900070330     D  piCapoContoDebito...
084000070330     D                                6A
084100070330     D  piSottoContoDebito...
084200070330     D                                8A
084300070330     D  piCausaleDebito...
084400070330     D                                4A
084500070330
084600070330      /FREE
084700070330
084800070330       IF NOT Bollo_i_isVirtuale();
084900070330         CLEAR piCapoContoDebito;
085000070330         CLEAR piSottoContoDebito;
085100070330         CLEAR piCausaleDebito;
085200070330         RETURN ESITO_WARNING_SOCIETA_SENZA_BOLLOVIRTUALE;
085300070330       ENDIF;
085400070330
085500070330       piCapoContoDebito = ynbovi00f.debitoKcc;
085600070330       piSottoContoDebito = ynbovi00f.debitoKsc;
085700070330       piCausaleDebito = ynbovi00f.debitoCaus;
085800070330
085900070330       RETURN ESITO_OK;
086000070330
086100070330      /END-FREE
086200070330     P Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale...
086300070330     P                 E
086400070330
