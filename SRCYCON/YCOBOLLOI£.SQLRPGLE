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
001600070328     D ESITO_WARNING...
001700070328     D                 C                   1
001800070328     D ESITO_ERROR...
001900070328     D                 C                   -1
002000070329     D BOLLO_FATTURA...
002100070329     D                 C                   'F'
002200070328
002300070328      //********************************************************************************************
002400070328      //
002500070328      // Definizione prototipi.
002600070328      //
002700070328      //********************************************************************************************
002800070328      /COPY GAITRASRC/SRCPROTOPR,YCOBOLLOI
002900070328
003000070328      //********************************************************************************************
003100070328      //
003200070328      // Definizione strutture dati.
003300070328      //
003400070328      //********************************************************************************************
003500070328     D ynbotp00f     E DS                  INZ QUALIFIED
003600070328     D ynbosc00f     E DS                  INZ QUALIFIED
003700070328     D ynbofe00f     E DS                  INZ QUALIFIED
003800070328     D ynbovi00f     E DS                  INZ QUALIFIED
003900070328
004000070328      //********************************************************************************************
004100070328      //
004200070328      // Definizione variabili globali.
004300070328      //
004400070328      //********************************************************************************************
004500070329     D idBollo...
004600070329     D                 S                   LIKE(ynbotp00f.idBollo)
004700070329     D dataValidita...
004800070329     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
004900070329     D societa...
005000070329     D                 S                   LIKE(ynbovi00f.societa)
005100070329     D imponibileBollo...
005200070329     D                 S                   LIKE(ynbosc00f.impInfer)
005300070330
005400070328     P*--------------------------------------------------
005500070328     P* Procedure name: Bollo_i_init
005600070328     P* Purpose:        Inizializzazioni.
005700070328     P* Returns:        Esito.
005800070328     P*--------------------------------------------------
005900070328     P Bollo_i_init    B                   EXPORT
006000070328     D Bollo_i_init    PI            10I 0
006100070329     D  piIdBollo                     1A   CONST
006200070329     D                                     OPTIONS(*NOPASS:*OMIT)
006300070329     D  piDataValidita...
006400070329     D                               10D   DATFMT(*ISO)
006500070329     D                                     CONST
006600070329     D                                     OPTIONS(*NOPASS:*OMIT)
006700070329     D  piSocieta                     3A
006800070329     D                                     OPTIONS(*NOPASS:*OMIT)
006900070330     D                                     CONST
007000070328
007100070329     D lclIdBollo...
007200070329     D                 S                   LIKE(ynbotp00f.idBollo)
007300070329     D lclDataValidita...
007400070329     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
007500070330     D lclSocieta...
007600070330     D                 S                   LIKE(ynbovi00f.societa)
007700070329
007800070328      /FREE
007900070329
008000070329       RESET idBollo;
008100070329       RESET dataValidita;
008200070329       RESET societa;
008300070329       RESET imponibileBollo;
008400070329       RESET ynbotp00f;
008500070329       RESET ynbosc00f;
008600070329       RESET ynbofe00f;
008700070329       RESET ynbovi00f;
008800070329
008900070329       IF %PARMS() > 0 AND %ADDR( piIdBollo ) <> *NULL;
009000070329         lclIdBollo = piIdBollo;
009100070329         IF Bollo_i_setIdBollo( lclIdBollo ) < *ZERO;
009200070329           RETURN ESITO_ERROR;
009300070329         ENDIF;
009400070329       ENDIF;
009500070329
009600070329       IF %PARMS() > 1 AND %ADDR( piDataValidita ) <> *NULL;
009700070329         lclDataValidita = piDataValidita;
009800070329         IF Bollo_i_setDataValidita( lclDataValidita ) < *ZERO;
009900070329           RETURN ESITO_ERROR;
010000070329         ENDIF;
010100070329       ENDIF;
010200070329
010300070329       IF %PARMS() > 2 AND %ADDR( piSocieta ) <> *NULL;
010400070330         lclSocieta = piSocieta;
010500070330         IF Bollo_i_setSocieta( lclSocieta ) < *ZERO;
010600070329           RETURN ESITO_ERROR;
010700070329         ENDIF;
010800070329       ENDIF;
010900070329
011000070330       IF Bollo_i_getIdBollo() <> *BLANK
011100070330       AND Bollo_i_newTipoBolloByIdBollo( Bollo_i_getIdBollo()
011200070330                                        : Bollo_i_getDataValidita() ) < 0;
011300070329         RETURN ESITO_ERROR;
011400070329       ENDIF;
011500070329
011600070330       IF Bollo_i_getSocieta() <> *BLANK
011700070330       AND Bollo_i_newVirtualeBySocieta( Bollo_i_getSocieta()
011800070330                                       : Bollo_i_getDataValidita() ) < 0;
011900070329         RETURN ESITO_ERROR;
012000070329       ENDIF;
012100070328
012200070328       RETURN ESITO_OK;
012300070328
012400070328      /END-FREE
012500070328     P Bollo_i_init    E
012600070328
012700070328     P*--------------------------------------------------
012800070328     P* Procedure name: Bollo_i_finalize
012900070328     P* Purpose:        Operazioni finali.
013000070328     P* Returns:        Esito.
013100070328     P*--------------------------------------------------
013200070328     P Bollo_i_finalize...
013300070328     P                 B                   EXPORT
013400070328     D Bollo_i_finalize...
013500070328     D                 PI            10I 0
013600070328
013700070328      /FREE
013800070328
013900070328       RETURN ESITO_OK;
014000070328
014100070328      /END-FREE
014200070328     P Bollo_i_finalize...
014300070328     P                 E
014400070328
014500070329     P*--------------------------------------------------
014600070329     P* Procedure name: Bollo_i_newDocumento
014700070329     P* Purpose:        Nuovo documento.
014800070329     P* Returns:        Esito.
014900070329     P* Parameter:      piDataDocumento => Data documento
015000070329     P*--------------------------------------------------
015100070329     P Bollo_i_newDocumento...
015200070329     P                 B                   EXPORT
015300070329     D Bollo_i_newDocumento...
015400070329     D                 PI            10I 0
015500070329     D  piIdBollo                     1A   CONST
015600070329     D                                     OPTIONS(*NOPASS:*OMIT)
015700070329     D  piDataValidita...
015800070329     D                               10D   DATFMT(*ISO)
015900070329     D                                     CONST
016000070329     D                                     OPTIONS(*NOPASS:*OMIT)
016100070330     D  piSocieta                     3A   CONST
016200070329     D                                     OPTIONS(*NOPASS:*OMIT)
016300070330
016400070330     D lclIdBollo...
016500070330     D                 S                   LIKE(ynbotp00f.idBollo)
016600070330     D lclDataValidita...
016700070330     D                 S             10D   DATFMT(*ISO) INZ(*SYS)
016800070330     D lclSocieta...
016900070330     D                 S                   LIKE(ynbovi00f.societa)
017000070329
017100070329      /FREE
017200070329
017300070330       // Per ogni documento l'utente può cambiare il tipo bollo, oppure la data
017400070330       // del documento, oppure la società.
017500070330
017600070329       IF %PARMS() > 0 AND %ADDR( piIdBollo ) <> *NULL;
017700070330         lclIdBollo = piIdBollo;
017800070330         IF Bollo_i_setIdBollo( lclIdBollo ) < *ZERO;
017900070329           RETURN ESITO_ERROR;
018000070329         ENDIF;
018100070329       ENDIF;
018200070329
018300070329       IF %PARMS() > 1 AND %ADDR( piDataValidita ) <> *NULL;
018400070330         lclDataValidita = piDataValidita;
018500070330         IF Bollo_i_setDataValidita( lclDataValidita ) < *ZERO;
018600070329           RETURN ESITO_ERROR;
018700070329         ENDIF;
018800070329       ENDIF;
018900070329
019000070329       IF %PARMS() > 2 AND %ADDR( piSocieta ) <> *NULL;
019100070330         lclSocieta = piSocieta;
019200070330         IF Bollo_i_setSocieta( lclSocieta ) < *ZERO;
019300070329           RETURN ESITO_ERROR;
019400070329         ENDIF;
019500070329       ENDIF;
019600070330
019700070330       IF Bollo_i_getIdBollo() <> *BLANK
019800070330       AND Bollo_i_newTipoBolloByIdBollo( Bollo_i_getIdBollo()
019900070330                                        : Bollo_i_getDataValidita() ) < 0;
020000070330         RETURN ESITO_ERROR;
020100070330       ENDIF;
020200070330
020300070330       IF Bollo_i_getSocieta() <> *BLANK
020400070330       AND Bollo_i_newVirtualeBySocieta( Bollo_i_getSocieta()
020500070330                                       : Bollo_i_getDataValidita() ) < 0;
020600070330         RETURN ESITO_ERROR;
020700070330       ENDIF;
020800070329
020900070329       RESET imponibileBollo;
021000070329
021100070329       RETURN ESITO_OK;
021200070329
021300070329      /END-FREE
021400070329     P Bollo_i_newDocumento...
021500070329     P                 E
021600070329
021700070329     P*--------------------------------------------------
021800070329     P* Procedure name: Bollo_i_endDocumento
021900070329     P* Purpose:        Fine documento.
022000070329     P* Returns:        Esito.
022100070329     P*--------------------------------------------------
022200070329     P Bollo_i_endDocumento...
022300070329     P                 B                   EXPORT
022400070329     D Bollo_i_endDocumento...
022500070329     D                 PI            10I 0
022600070329
022700070329      /FREE
022800070329
022900070330       // Il documento (p.e. fattura) è finito, quindi ho l'imponibile
023000070330       // su cui calcolare il bollo.
023100070330       IF Bollo_i_newImportoBolloByImponibile( Bollo_i_getIdBollo()
023200070330       : Bollo_i_getDataValidita(): imponibileBollo ) < *ZERO;
023300070329         RETURN ESITO_ERROR;
023400070329       ENDIF;
023500070329
023600070329       RETURN ESITO_OK;
023700070329
023800070329      /END-FREE
023900070329     P Bollo_i_endDocumento...
024000070329     P                 E
024100070329
024200070329     P*--------------------------------------------------
024300070329     P* Procedure name: Bollo_i_newTipoBolloByIdBollo
024400070329     P* Purpose:        Nuovo bollo reperito con ID bollo e data validità.
024500070329     P* Returns:        Esito.
024600070329     P*--------------------------------------------------
024700070329     P Bollo_i_newTipoBolloByIdBollo...
024800070329     P                 B
024900070329     D Bollo_i_newTipoBolloByIdBollo...
025000070329     D                 PI            10I 0
025100070330     D  piIdBollo                     1A   CONST
025200070330     D piDataValidita                10D   DATFMT(*ISO) CONST
025300070329
025400070329      /FREE
025500070329
025600070329       IF piIdBollo = *BLANK;
025700070329         RETURN ESITO_ERROR;
025800070329       ENDIF;
025900070329
026000070329       IF piIdBollo = ynbotp00f.idBollo AND piDataValidita >= ynbotp00f.dtInVal
026100070329       AND piDataValidita <= ynbotp00f.dtFiVal;
026200070329         RETURN ESITO_OK;
026300070329       ENDIF;
026400070329
026500070329      /END-FREE
026600070329     C/EXEC SQL
026700070329     C+ SELECT *
026800070329     C+ INTO :ynbotp00f
026900070329     C+ FROM YNBOTP00F
027000070329     C+ WHERE IDBOLLO = :piIdBollo
027100070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
027200070329     C/END-EXEC
027300070329      /FREE
027400070329
027500070329       IF sqlCode = 100 OR sqlCode < 0;
027600070329         RESET ynbotp00f;
027700070329         RETURN ESITO_ERROR;
027800070329       ENDIF;
027900070329
028000070329       RETURN ESITO_OK;
028100070329
028200070329      /END-FREE
028300070329     P Bollo_i_newTipoBolloByIdBollo...
028400070329     P                 E
028500070329
028600070329     P*--------------------------------------------------
028700070329     P* Procedure name: Bollo_i_newVirtualeBySocieta
028800070329     P* Purpose:        Nuova autorizzazione bollo virtuale per società.
028900070329     P* Returns:        Esito.
029000070329     P*--------------------------------------------------
029100070329     P Bollo_i_newVirtualeBySocieta...
029200070329     P                 B                   EXPORT
029300070329     D Bollo_i_newVirtualeBySocieta...
029400070329     D                 PI            10I 0
029500070330     D  piSocieta                     3A   CONST
029600070329     D  piDataValidita...
029700070330     D                               10D   DATFMT(*ISO) CONST
029800070329
029900070329      /FREE
030000070329
030100070329       IF piSocieta = ynbovi00f.societa AND piDataValidita >= ynbovi00f.dtInVal
030200070329       AND piDataValidita <= ynbovi00f.dtFiVal;
030300070329         RETURN ESITO_OK;
030400070329       ENDIF;
030500070329
030600070329      /END-FREE
030700070329     C/EXEC SQL
030800070329     C+ SELECT *
030900070329     C+ INTO :ynbovi00f
031000070329     C+ FROM YNBOVI00F
031100070329     C+ WHERE SOCIETA = :piSocieta
031200070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
031300070329     C/END-EXEC
031400070329      /FREE
031500070329
031600070329       SELECT;
031700070329         WHEN sqlCode < 0;
031800070329           RESET ynbovi00f;
031900070329           RETURN ESITO_ERROR;
032000070329         WHEN sqlCode = 100;
032100070329           RESET ynbovi00f;
032200070329       ENDSL;
032300070329
032400070329       RETURN ESITO_OK;
032500070329
032600070329      /END-FREE
032700070329     P Bollo_i_newVirtualeBySocieta...
032800070329     P                 E
032900070329
033000070329     P*--------------------------------------------------
033100070329     P* Procedure name: Bollo_i_newEsenzioneIVAByRiferimento
033200070329     P* Purpose:        Nuova esenzione IVA per riferimento.
033300070329     P* Returns:        Esito.
033400070329     P* Parameter:      piRiferimentoIVA => Riferimento esenzione IVA
033500070329     P*--------------------------------------------------
033600070329     P Bollo_i_newEsenzioneIVAByRiferimento...
033700070329     P                 B
033800070329     D Bollo_i_newEsenzioneIVAByRiferimento...
033900070329     D                 PI            10I 0
034000070330     D  piIdBollo                     1A   CONST
034100070330     D piDataValidita                10D   DATFMT(*ISO) CONST
034200070329     D  piRiferimentoIVA...
034300070329     D                                3A
034400070329
034500070329      /FREE
034600070329
034700070329       IF piRiferimentoIVA <> ynbofe00f.rifIVA
034800070329       OR piDataValidita < ynbofe00f.dtInVal
034900070329       OR piDataValidita > ynbofe00f.dtFiVal
035000070329       OR piIdBollo <> ynbofe00f.idBollo;
035100070329      /END-FREE
035200070329     C/EXEC SQL
035300070329     C+ SELECT *
035400070329     C+ INTO :ynbofe00f
035500070329     C+ FROM YNBOFE00F
035600070329     C+ WHERE IDBOLLO = :idBollo
035700070329     C+   AND :dataValidita BETWEEN DTINVAL AND DTFIVAL
035800070329     C+   AND RIFIVA = :piRiferimentoIVA
035900070329     C/END-EXEC
036000070329      /FREE
036100070329         SELECT;
036200070329           WHEN sqlCode < 0;
036300070329             RESET ynbofe00f;
036400070329             RETURN ESITO_ERROR;
036500070329           WHEN sqlCode = 100;
036600070329             RETURN ESITO_WARNING;
036700070329         ENDSL;
036800070329       ENDIF;
036900070329
037000070329       RETURN ESITO_OK;
037100070329
037200070329      /END-FREE
037300070329     P Bollo_i_newEsenzioneIVAByRiferimento...
037400070329     P                 E
037500070329
037600070329     P*--------------------------------------------------
037700070329     P* Procedure name: Bollo_i_newImportoBolloByImponibile
037800070329     P* Purpose:        Nuovo importo bollo per imponibile.
037900070329     P* Returns:        Esito.
038000070329     P* Parameter:      piIdBollo => ID bollo
038100070329     P* Parameter:      piDataValidita => Data validità.
038200070329     P* Parameter:      piImponibileBollo => Imponibile bollo.
038300070329     P*--------------------------------------------------
038400070329     P Bollo_i_newImportoBolloByImponibile...
038500070329     P                 B
038600070329     D Bollo_i_newImportoBolloByImponibile...
038700070329     D                 PI            10I 0
038800070330     D  piIdBollo                     1A   CONST
038900070330     D piDataValidita                10D   DATFMT(*ISO) CONST
039000070329     D  piImponibileBollo...
039100070329     D                               15P 2
039200070329
039300070329      /FREE
039400070329
039500070329       IF piImponibileBollo < ynbosc00f.impInfer
039600070329       OR piImponibileBollo > ynbosc00f.impSuper
039700070329       OR piDataValidita < ynbosc00f.dtInVal
039800070329       OR piDataValidita > ynbosc00f.dtFiVal
039900070329       OR piIdBollo <> ynbosc00f.idBollo;
040000070329      /END-FREE
040100070329     C/EXEC SQL
040200070329     C+ SELECT *
040300070329     C+ INTO :ynbosc00f
040400070329     C+ FROM YNBOSC00F
040500070329     C+ WHERE IDBOLLO = :piIdBollo
040600070329     C+   AND :piDataValidita BETWEEN DTINVAL AND DTFIVAL
040700070329     C+   AND :piImponibileBollo BETWEEN IMPINFER AND IMPSUPER
040800070329     C/END-EXEC
040900070329      /FREE
041000070329         SELECT;
041100070329           WHEN sqlCode < 0;
041200070329             RESET ynbosc00f;
041300070329             RETURN ESITO_ERROR;
041400070329           WHEN sqlCode = 100;
041500070329             RESET ynbosc00f;
041600070329             RETURN ESITO_WARNING;
041700070329         ENDSL;
041800070329       ENDIF;
041900070329
042000070329       RETURN ESITO_OK;
042100070329
042200070329      /END-FREE
042300070329     P Bollo_i_newImportoBolloByImponibile...
042400070329     P                 E
042500070329
042600070329     P*--------------------------------------------------
042700070329     P* Procedure name: Bollo_i_addImponibileEsenteIVA
042800070329     P* Purpose:        Aggiunge un imponibile esente IVA.
042900070329     P* Returns:        Esito.
043000070329     P* Parameter:      idRiferimentoIVA => Riferimento IVA
043100070329     P* Parameter:      piImponibileIVA => Imponibile IVA.
043200070329     P*--------------------------------------------------
043300070329     P Bollo_i_addImponibileEsenteIVA...
043400070329     P                 B                   EXPORT
043500070329     D Bollo_i_addImponibileEsenteIVA...
043600070329     D                 PI            10I 0
043700070329     D  piRiferimentoIVA...
043800070329     D                                3A
043900070329     D  piImponibileIVA...
044000070330     D                               15P 2 VALUE
044100070329
044200070329      /FREE
044300070329
044400070330       IF Bollo_i_newEsenzioneIVAByRiferimento( BOLLO_FATTURA
044500070330       : Bollo_i_getDataValidita() : piRiferimentoIVA ) = *ZERO;
044600070329         imponibileBollo += piImponibileIVA;
044700070329       ENDIF;
044800070329
044900070329       RETURN ESITO_OK;
045000070329
045100070329      /END-FREE
045200070329     P Bollo_i_addImponibileEsenteIVA...
045300070329     P                 E
045400070329
045500070329     P*--------------------------------------------------
045600070329     P* Procedure name: Bollo_i_getImportoBollo
045700070329     P* Purpose:        Reperisce l' importo del bollo.
045800070329     P* Returns:        Importo bollo.
045900070329     P*--------------------------------------------------
046000070329     P Bollo_i_getImportoBollo...
046100070329     P                 B                   EXPORT
046200070329     D Bollo_i_getImportoBollo...
046300070329     D                 PI            15P 2
046400070329
046500070329      /FREE
046600070329
046700070329       RETURN ynbosc00f.impBollo;
046800070329
046900070329      /END-FREE
047000070329     P Bollo_i_getImportoBollo...
047100070329     P                 E
047200070329
047300070329     P*--------------------------------------------------
047400070329     P* Procedure name: Bollo_i_isVirtuale
047500070329     P* Purpose:        Dice se la società ha il bollo virtuale.
047600070329     P* Returns:        *ON = bollo virtuale.
047700070329     P* Parameter:      piSocieta => Società.
047800070329     P* Parameter:      piEsito => Esito.
047900070329     P*--------------------------------------------------
048000070329     P Bollo_i_isVirtuale...
048100070329     P                 B                   EXPORT
048200070329     D Bollo_i_isVirtuale...
048300070329     D                 PI              N
048400070329
048500070329      /FREE
048600070329
048700070329       RETURN ((%LEN(ynbovi00f.autorNum) > *ZERO)
048800070329                AND (ynbovi00f.autorNum <> *BLANK));
048900070330
049000070329      /END-FREE
049100070329     P Bollo_i_isVirtuale...
049200070329     P                 E
049300070329
049400070329     P*--------------------------------------------------
049500070329     P* Procedure name: Bollo_i_setIdBollo
049600070329     P* Purpose:        Imposta l' ID bollo.
049700070329     P* Returns:        Esito.
049800070329     P* Parameter:      piIdBollo => ID bollo.
049900070329     P*--------------------------------------------------
050000070329     P Bollo_i_setIdBollo...
050100070329     P                 B
050200070329     D Bollo_i_setIdBollo...
050300070329     D                 PI            10I 0
050400070329     D  piIdBollo                     1A
050500070329
050600070329      /FREE
050700070329
050800070329       idBollo = piIdBollo;
050900070329
051000070329       RETURN ESITO_OK;
051100070329
051200070329      /END-FREE
051300070329     P Bollo_i_setIdBollo...
051400070329     P                 E
051500070329
051600070329     P*--------------------------------------------------
051700070329     P* Procedure name: Bollo_i_setDataValidita
051800070329     P* Purpose:        Imposta la data per controlli validità.
051900070329     P* Returns:        Esito.
052000070329     P* Parameter:      piDataValidita => Data validita
052100070329     P*--------------------------------------------------
052200070329     P Bollo_i_setDataValidita...
052300070329     P                 B
052400070329     D Bollo_i_setDataValidita...
052500070329     D                 PI            10I 0
052600070329     D piDataValidita                10D   DATFMT(*ISO)
052700070329
052800070329      /FREE
052900070329
053000070329       dataValidita = piDataValidita;
053100070329
053200070329       RETURN ESITO_OK;
053300070329
053400070329      /END-FREE
053500070329     P Bollo_i_setDataValidita...
053600070329     P                 E
053700070329
053800070329     P*--------------------------------------------------
053900070329     P* Procedure name: Bollo_i_setSocieta
054000070329     P* Purpose:        Imposta la societa.
054100070329     P* Returns:        Esito.
054200070329     P* Parameter:      piSocieta
054300070329     P*--------------------------------------------------
054400070329     P Bollo_i_setSocieta...
054500070329     P                 B
054600070329     D Bollo_i_setSocieta...
054700070329     D                 PI            10I 0
054800070329     D  piSocieta                     3A
054900070329
055000070329      /FREE
055100070329
055200070329       societa = piSocieta;
055300070329
055400070329       RETURN ESITO_OK;
055500070329
055600070329      /END-FREE
055700070329     P Bollo_i_setSocieta...
055800070329     P                 E
055900070329
056000070330
056100070330     P*--------------------------------------------------
056200070330     P* Procedure name: Bollo_i_getIdBollo
056300070330     P* Purpose:        Reperisce l' ID bollo in uso.
056400070330     P* Returns:        ID bollo.
056500070330     P*--------------------------------------------------
056600070330     P Bollo_i_getIdBollo...
056700070330     P                 B
056800070330     D Bollo_i_getIdBollo...
056900070330     D                 PI             1A
057000070330
057100070330      /FREE
057200070330
057300070330       RETURN idBollo;
057400070330
057500070330      /END-FREE
057600070330     P Bollo_i_getIdBollo...
057700070330     P                 E
057800070330
057900070330
058000070330     P*--------------------------------------------------
058100070330     P* Procedure name: Bollo_i_getDataValidità
058200070330     P* Purpose:
058300070330     P* Returns:        Data validità.
058400070330     P*--------------------------------------------------
058500070330     P Bollo_i_getDataValidita...
058600070330     P                 B
058700070330     D Bollo_i_getDataValidita...
058800070330     D                 PI            10D
058900070330
059000070330      /FREE
059100070330
059200070330       RETURN dataValidita;
059300070330
059400070330      /END-FREE
059500070330     P Bollo_i_getDataValidita...
059600070330     P                 E
059700070330
059800070330
059900070330     P*--------------------------------------------------
060000070330     P* Procedure name: Bollo_i_getSocieta
060100070330     P* Purpose:        Restituisce la societa.
060200070330     P* Returns:        Società.
060300070330     P*--------------------------------------------------
060400070330     P Bollo_i_getSocieta...
060500070330     P                 B
060600070330     D Bollo_i_getSocieta...
060700070330     D                 PI             3A
060800070330
060900070330      /FREE
061000070330
061100070330       RETURN societa;
061200070330
061300070330      /END-FREE
061400070330     P Bollo_i_getSocieta...
061500070330     P                 E
061600070330
061700070330     P*--------------------------------------------------
061800070330     P* Procedure name: Bollo_i_getDichiarazioneBolloVirtuale
061900070330     P* Purpose:        Reperisce il testo della dichiarazione del bollo vi...
062000070330     P*                          rtuale.
062100070330     P* Returns:        Testo dichiarazione bollo virtuale.
062200070330     P*--------------------------------------------------
062300070330     P Bollo_i_getDichiarazioneBolloVirtuale...
062400070330     P                 B                   EXPORT
062500070330     D Bollo_i_getDichiarazioneBolloVirtuale...
062600070330     D                 PI           255A   VARYING
062700070330
062800070330      /FREE
062900070330
063000070330       RETURN 'Imposta di bollo '
063100070330              + %TRIML(%EDITC(Bollo_i_getImportoBollo : '1'))
063200070330              + ' € assolta in modo virtuale. Autorizzazione '
063300070330              + ynbovi00f.autorNum + ' '
063400070330              + %CHAR(ynbovi00f.autorData) + ' '
063500070330              + ynbovi00f.autorProv + '.';
063600070330
063700070330      /END-FREE
063800070330     P Bollo_i_getDichiarazioneBolloVirtuale...
063900070330     P                 E
064000070330
