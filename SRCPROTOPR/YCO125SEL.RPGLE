000100080218     D*--------------------------------------------------
000200140724     D* Procedure name: PropostaBlocco_sel_init
000300080218     D* Purpose:        Inizializza modulo.
000400080218     D* Returns:        Esito.
000500080609     D* Parameter:      piKpjba => KPJBA
000600080218     D*--------------------------------------------------
000700140724     D PropostaBlocco_sel_init...
000800080218     D                 PR            10I 0
000900080609     D  piKpjba                            LIKEDS(kpjba)
001000080609     D                                     OPTIONS(*NOPASS)
001100080218
001200080218     D*--------------------------------------------------
001300140724     D* Procedure name: PropostaBlocco_sel_finalize
001400080218     D* Purpose:        Chiusura modulo.
001500080218     D* Returns:        Esito.
001600080218     D*--------------------------------------------------
001700140724     D PropostaBlocco_sel_finalize...
001800080218     D                 PR            10I 0
001900080218
002000080218     D*--------------------------------------------------
002100140724     D* Procedure name: PropostaBlocco_sel_newStatement
002200080505     D* Purpose:        Nuova istruzione SQL.
002300080218     D* Returns:        Esito.
002400080218     D*--------------------------------------------------
002500140724     D PropostaBlocco_sel_newStatement...
002600080218     D                 PR            10I 0
002700080218
002800080219     D*--------------------------------------------------
002900140724     D* Procedure name: PropostaBlocco_sel_endStatement
003000080505     D* Purpose:        Fine istruzione SQL.
003100080219     D* Returns:        Esito.
003200080219     D*--------------------------------------------------
003300140724     D PropostaBlocco_sel_endStatement...
003400080219     D                 PR            10I 0
003500080219
004601140828     D*--------------------------------------------------
004602140828     D* Procedure name: PropostaBlocco_sel_setWhereDataProposta
004603140828     D* Purpose:        Imposta la WHERE
004604140828     D* Returns:        Esito.
004605140828     D* Parameter:
004606140828     D*--------------------------------------------------
004607140828     D PropostaBlocco_sel_setWhereDataProposta...
004608140828     D                 PR            10I 0
004609140828     D  piDataProposta...
004610140828     D                                 D   DATFMT(*ISO)
004611140828
004700080218     D*--------------------------------------------------
004800140828     D* Procedure name: PropostaBlocco_sel_setWhereIdProposta
004900140828     D* Purpose:        Imposta la WHERE
005000080218     D* Returns:        Esito.
005100140828     D* Parameter:
005200140828     D*--------------------------------------------------
005400140828     D PropostaBlocco_sel_setWhereIdProposta...
005500080218     D                 PR            10I 0
005600140828     D  piIdProposta...
005700140828     D                               10I 0 CONST
005800140828
006100080219     D*--------------------------------------------------
006200140724     D* Procedure name: PropostaBlocco_sel_setWhereUtenteInserimento
006300080219     D* Purpose:        Imposta la WHERE sul profilo utente che ha inserito...
006400080219     D*                           la richiesta.
006500080219     D* Returns:        Esito.
006600080219     D* Parameter:      piUtenteInserimento => Profilo utente inserimento.
006700080219     D*--------------------------------------------------
006800140724     D PropostaBlocco_sel_setWhereUtenteInserimento...
006900080219     D                 PR            10I 0
007000080219     D  piUtenteInserimento...
007100080219     D                               10A
007200080219
010500080219     D*--------------------------------------------------
010600140724     D* Procedure name: PropostaBlocco_sel_setWhereSocieta
010700080219     D* Purpose:        Imposta la WHERE sulla società.
010800080219     D* Returns:        Esito.
010900080219     D* Parameter:      piSocieta => Società.
011000080219     D*--------------------------------------------------
011100140724     D PropostaBlocco_sel_setWhereSocieta...
011200080219     D                 PR            10I 0
011300080219     D  piSocieta                     3A
011400080219
011500080219     D*--------------------------------------------------
011600140724     D* Procedure name: PropostaBlocco_sel_setWhereUnita
011700080219     D* Purpose:        Imposta la WHERE sulla unità.
011800080219     D* Returns:        Esito.
011900080219     D* Parameter:      piUnita => Unità.
012000080219     D*--------------------------------------------------
012100140724     D PropostaBlocco_sel_setWhereUnita...
012200080219     D                 PR            10I 0
012300140929     D  piUnita                       3A
012301140829
012302140829     D*--------------------------------------------------
012303140829     D* Procedure name: PropostaBlocco_sel_setWhereCodEvento
012304140829     D* Purpose:        Imposta la WHERE sul codice cliente.
012305140829     D* Returns:        Esito.
012306140829     D* Parameter:      piKscCliente => Codice cliente.
012307140829     D*--------------------------------------------------
012308140829     D PropostaBlocco_sel_setWhereCodEvento...
012309140829     D                 PR            10I 0
012310140829     D  piCodEvento                   2A   CONST
012311140829
012500080219     D*--------------------------------------------------
012600140829     D* Procedure name: PropostaBlocco_sel_setWhereIdCliente
012700080219     D* Purpose:        Imposta la WHERE sul codice cliente.
012800080219     D* Returns:        Esito.
012900080219     D* Parameter:      piKscCliente => Codice cliente.
013000080219     D*--------------------------------------------------
013100140829     D PropostaBlocco_sel_setWhereIdCliente...
013200080219     D                 PR            10I 0
013300140829     D  piIdCliente                   8A   CONST
013301140829     D  piOperatore                   2A   CONST
013400140828
014500080221     D*--------------------------------------------------
014600140724     D* Procedure name: PropostaBlocco_sel_setWhereAnnullata
014700080221     D* Purpose:        Imposta la WHERE sul flag di annullamento.
014800080221     D* Returns:        Esito.
014900080221     D* Parameter:      piAnnullata => *ON = annullate, *OFF = non annullate.
015000080221     D*--------------------------------------------------
015100140724     D PropostaBlocco_sel_setWhereAnnullata...
015200080221     D                 PR            10I 0
015300080221     D  piAnnullata                    N   CONST
015400140828
029700080222     D*--------------------------------------------------
029800140724     D* Procedure name: PropostaBlocco_sel_newWhere
029900080222     D* Purpose:        Pulisce la clausola WHERE.
030000080222     D* Returns:        Esito.
030100080222     D*--------------------------------------------------
030200140724     D PropostaBlocco_sel_newWhere...
030300080222     D                 PR            10I 0
030400080505
030500080505     D*--------------------------------------------------
030600140724     D* Procedure name: PropostaBlocco_sel_endWhere
030700080505     D* Purpose:        Fine clausola WHERE.
030800080505     D* Returns:        Esito.
030900080505     D*--------------------------------------------------
031000140724     D PropostaBlocco_sel_endWhere...
031100080505     D                 PR            10I 0
031200080505
031300080505     D*--------------------------------------------------
031400140724     D* Procedure name: PropostaBlocco_sel_newWhereOr
031500080505     D* Purpose:        Inizia una clausola WHERE con almeno due condizioni...
031600080505     D*                           in OR.
031700080505     D* Returns:        Esito.
031800080505     D*--------------------------------------------------
031900140724     D PropostaBlocco_sel_newWhereOr...
032000080505     D                 PR            10I 0
032100080505
032200080505     D*--------------------------------------------------
032300140724     D* Procedure name: PropostaBlocco_sel_endWhereOr
032400080505     D* Purpose:        Chiude una clausola WHERE con almeno due condizioni...
032500080505     D*                           in OR.
032600080505     D* Returns:        Esito.
032700080505     D*--------------------------------------------------
032800140724     D PropostaBlocco_sel_endWhereOr...
032900080505     D                 PR            10I 0
033000080505
033100080218     D*--------------------------------------------------
033200140724     D* Procedure name: PropostaBlocco_sel_prepare
033300080218     D* Purpose:        Preparazione istruzione SQL.
033400080221     D* Parameter:      piMultipleRowFetch => Numero di righe da reperire ad ogni FETCH.
033500080218     D* Returns:        Esito.
033600080218     D*--------------------------------------------------
033700140724     D PropostaBlocco_sel_prepare...
033800080218     D                 PR            10I 0
033900080221     D  piMultipleRowFetch...
034000080221     D                               10I 0 CONST
034100080218
034200080218     D*--------------------------------------------------
034300140724     D* Procedure name: PropostaBlocco_sel_declareCursor
034400080218     D* Purpose:        Definizione del cursore SQL.
034500080218     D* Returns:        Esito.
034600080218     D*--------------------------------------------------
034700140724     D PropostaBlocco_sel_declareCursor...
034800080218     D                 PR            10I 0
034900080218
035000080218     D*--------------------------------------------------
035100140724     D* Procedure name: PropostaBlocco_sel_open
035200080218     D* Purpose:        Apertura cursore SQL.
035300080218     D* Returns:        Esito.
035400080218     D*--------------------------------------------------
035500140724     D PropostaBlocco_sel_open...
035600080218     D                 PR            10I 0
035700080218
035800080218     D*--------------------------------------------------
035900140724     D* Procedure name: PropostaBlocco_sel_close
036000080218     D* Purpose:        Chiusura cursore SQL.
036100080218     D* Returns:        Esito.
036200080218     D*--------------------------------------------------
036300140724     D PropostaBlocco_sel_close...
036400080218     D                 PR            10I 0
036500080218
036600080218     D*--------------------------------------------------
036700140724     D* Procedure name: PropostaBlocco_sel_fetchNext
036800080218     D* Purpose:        Posiziona il cursore sulla riga seguente.
036900080221     D* Parameter:      piRowCount => Number of rows retrieved.
037000080221     D* Parameter:      piDb2LastRow => Contains +100 if the last row was fetched.
037100080221     D* Returns:        Esito.
037200080218     D*--------------------------------------------------
037300140724     D PropostaBlocco_sel_fetchNext...
037400080218     D                 PR            10I 0
037500080219     D  piRowCount                   10I 0
037600080219     D  piDb2LastRow                 10I 0
037700080219
037800080220     D*--------------------------------------------------
037900140724     D* Procedure name: PropostaBlocco_sel_getNextRow
038000080220     D* Purpose:        Restituisce i dati di una fattura.
038100080220     D* Returns:        Esito.
038200080220     D* Parameter:      piNumeroRichiesta => Numero richiesta.
038300080220     D*--------------------------------------------------
038400140724     D PropostaBlocco_sel_getNextRow...
038500080220     D                 PR            10I 0
038600080220     D  piNumeroRichiesta...
038700080220     D                               10I 0
038701140929     P*--------------------------------------------------
038702140929     P* Procedure name: PropostaBlocco_sel_setWhere
038703140929     P* Purpose:        Imposta la WHERE
038704140929     P* Returns:        Esito.
038705140929     P* Parameter:
038706140929     P*--------------------------------------------------
038707140929     D PropostaBlocco_sel_setWhereMoraBloc...
038710140929     D                 PR            10I 0
038711140930     P*--------------------------------------------------
038713140929     P* Procedure name: PropostaBlocco_sel_setWhere
038714140929     P* Purpose:        Imposta la .
038715140929     P* Returns:        Esito.
038716140929     P* Parameter:
038717140929     P*--------------------------------------------------
038718140929     D PropostaBlocco_sel_setWhereCdFisc...
038721140929     D                 PR            10I 0
038722140929     D  piCdFisc                     20A   CONST
038723140929     P*--------------------------------------------------
038724140929     P* Procedure name: PropostaBlocco_sel_setWhere
038725140929     P* Purpose:        Imposta la .
038726140929     P* Returns:        Esito.
038727140929     P* Parameter:
038728140929     P*--------------------------------------------------
038729140929     D PropostaBlocco_sel_setWherePIva...
038730140929     D                 PR            10I 0
038731140929     D  piPIva                       20A   CONST
038732140929     P*--------------------------------------------------
038733140929     P* Procedure name: PropostaBlocco_sel_setWhere
038734140929     P* Purpose:        Imposta la WHERE
038735140929     P* Returns:        Esito.
038736140929     P* Parameter:
038737140929     P*--------------------------------------------------
038738140930     D PropostaBlocco_sel_setWhereDataScadenza...
038739140929     D                 PR            10I 0
038740140930     D  piDaDataProposta...
038741140929     D                                 D   DATFMT(*ISO)
038742140930     D  piADataProposta...
038751140930     D                                 D   DATFMT(*ISO)
038752140929     P*--------------------------------------------------
038753140929     P* Procedure name: PropostaBlocco_sel_setWhere
038754140929     P* Purpose:        Imposta la WHERE
038755140929     P* Returns:        Esito.
038756140929     P* Parameter:
038757140929     P*--------------------------------------------------
038758140929     D PropostaBlocco_sel_setWherePropAnn...
038759140930     D                 PR            10I 0
038762140929     P*--------------------------------------------------
038763140929     P* Procedure name: PropostaBlocco_sel_setWhere
038764140929     P* Purpose:        Imposta la WHERE
038765140929     P* Returns:        Esito.
038766140929     P* Parameter:
038767140929     P*--------------------------------------------------
038768140929     D PropostaBlocco_sel_setWhereUlEvUte...
038769140929     D                 PR            10I 0
038770140929     D  piUlEvUte                    10A   CONST
038771140929     P*--------------------------------------------------
038772140929     P* Procedure name: PropostaBlocco_sel_setWhere
038773140929     P* Purpose:        Imposta la WHERE
038774140929     P* Returns:        Esito.
038775140929     P* Parameter:
038776140929     P*--------------------------------------------------
038777140929     D PropostaBlocco_sel_setWhereUlEvCaus...
038778140929     D                 PR            10I 0
038779140929     D  piUlEvUte                     3A   CONST
038780140929     P*--------------------------------------------------
038781140929     P* Procedure name: PropostaBlocco_sel_setWhere
038782140929     P* Purpose:        Imposta la WHERE
038783140929     P* Returns:        Esito.
038784140929     P* Parameter:
038785140929     P*--------------------------------------------------
038786140930     D PropostaBlocco_sel_setWhereUlEvDt...
038787140929     D                 PR            10I 0
038788140930     D  piUlEvDaDt...
038789140929     D                                 D   DATFMT(*ISO)
038790140930     D  piUlEvADt...
038799140929     D                                 D   DATFMT(*ISO)
038800140929     P*--------------------------------------------------
038801140929     P* Procedure name: PropostaBlocco_sel_setWhere
038802140929     P* Purpose:        Imposta la WHERE
038803140929     P* Returns:        Esito.
038804140929     P* Parameter:
038805140929     P*--------------------------------------------------
038806141003     D PropostaBlocco_sel_setWhereABC...
038807141003     D                 PR            10I 0
038808141003     D  piABC                         1A   CONST
038809141016     P*--------------------------------------------------
038810141016     P* Procedure name: PropostaBlocco_sel_setWhere
038811141016     P* Purpose:        Imposta la WHERE
038812141016     P* Returns:        Esito.
038813141016     P* Parameter:
038814141016     P*--------------------------------------------------
038815141016     D PropostaBlocco_sel_setWhereCodAgente...
038816141016     D                 PR            10I 0
038818141016     D  piCodAge                      7  0 CONST
038819141118     P*--------------------------------------------------
038820141118     P* Procedure name: PropostaBlocco_sel_setWhere
038821141118     P* Purpose:        Imposta la WHERE
038822141118     P* Returns:        Esito.
038823141118     P* Parameter:
038824141118     P*--------------------------------------------------
038825141118     D PropostaBlocco_sel_setWhereTipoProp...
038826141118     D                 PR            10I 0
038827141118     D  piTipoProp                    1A   CONST
038828150408     P*--------------------------------------------------
038829150408     P* Procedure name: PropostaBlocco_sel_setWhere
038830150408     P* Purpose:        Imposta la WHERE
038831150408     P* Returns:        Esito.
038832150408     P* Parameter:
038833150408     P*--------------------------------------------------
038834150408     D PropostaBlocco_sel_setWhereStatoCredito...
038835150408     D                 PR            10I 0
038836150408     D  piStatoCredito...
038838150408     D                                3    CONST
038839150408
038840150408
