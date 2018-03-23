000100080218     D*--------------------------------------------------
000200170119     D* Procedure name: YCO185sel_init
000300080218     D* Purpose:        Inizializza modulo.
000400080218     D* Returns:        Esito.
000500080609     D* Parameter:      piKpjba => KPJBA
000600080218     D*--------------------------------------------------
000700170119     D YCO185sel_init...
000800080218     D                 PR            10I 0
000900080609     D  piKpjba                            LIKEDS(kpjba)
001000080609     D                                     OPTIONS(*NOPASS)
001100080218
001200080218     D*--------------------------------------------------
001300170119     D* Procedure name: YCO185sel_finalize
001400080218     D* Purpose:        Chiusura modulo.
001500080218     D* Returns:        Esito.
001600080218     D*--------------------------------------------------
001700170119     D YCO185sel_finalize...
001800080218     D                 PR            10I 0
001900080218
002000080218     D*--------------------------------------------------
002100170119     D* Procedure name: YCO185sel_newStatement
002200080505     D* Purpose:        Nuova istruzione SQL.
002300080218     D* Returns:        Esito.
002400080218     D*--------------------------------------------------
002500170119     D YCO185sel_newStatement...
002600080218     D                 PR            10I 0
002700080218
002800080219     D*--------------------------------------------------
002900170119     D* Procedure name: YCO185sel_endStatement
003000080505     D* Purpose:        Fine istruzione SQL.
003100080219     D* Returns:        Esito.
003200080219     D*--------------------------------------------------
003300170119     D YCO185sel_endStatement...
003400080219     D                 PR            10I 0
003500170120
004700080218     D*--------------------------------------------------
004800170120     D* Procedure name: YCO185sel_setWhereidRiga
004900140828     D* Purpose:        Imposta la WHERE
005000080218     D* Returns:        Esito.
005100140828     D* Parameter:
005200140828     D*--------------------------------------------------
005400170120     D YCO185sel_setWhereidRiga...
005500080218     D                 PR            10I 0
005600170120     D  piidRiga...
005700140828     D                               10I 0 CONST
005800140828
010500080219     D*--------------------------------------------------
010600170119     D* Procedure name: YCO185sel_setWhereSocieta
010700080219     D* Purpose:        Imposta la WHERE sulla società.
010800080219     D* Returns:        Esito.
010900080219     D* Parameter:      piSocieta => Società.
011000080219     D*--------------------------------------------------
011100170119     D YCO185sel_setWhereSocieta...
011200080219     D                 PR            10I 0
011300080219     D  piSocieta                     3A
011400080219
011500080219     D*--------------------------------------------------
011600170119     D* Procedure name: YCO185sel_setWhereUnita
011700080219     D* Purpose:        Imposta la WHERE sulla unità.
011800080219     D* Returns:        Esito.
011900080219     D* Parameter:      piUnita => Unità.
012000080219     D*--------------------------------------------------
012100170119     D YCO185sel_setWhereUnita...
012200080219     D                 PR            10I 0
012300140929     D  piUnita                       3A
012301170120
012500080219     D*--------------------------------------------------
012600170119     D* Procedure name: YCO185sel_setWhereIdCliente
012700080219     D* Purpose:        Imposta la WHERE sul codice cliente.
012800080219     D* Returns:        Esito.
012900080219     D* Parameter:      piKscCliente => Codice cliente.
013000080219     D*--------------------------------------------------
013100170120     D YCO185sel_setWhereIdFornitore...
013200080219     D                 PR            10I 0
013300170206     D  piIdFornitore                 8A   CONST
013400170120
029700080222     D*--------------------------------------------------
029800170119     D* Procedure name: YCO185sel_newWhere
029900080222     D* Purpose:        Pulisce la clausola WHERE.
030000080222     D* Returns:        Esito.
030100080222     D*--------------------------------------------------
030200170119     D YCO185sel_newWhere...
030300080222     D                 PR            10I 0
030400080505
030500080505     D*--------------------------------------------------
030600170119     D* Procedure name: YCO185sel_endWhere
030700080505     D* Purpose:        Fine clausola WHERE.
030800080505     D* Returns:        Esito.
030900080505     D*--------------------------------------------------
031000170119     D YCO185sel_endWhere...
031100080505     D                 PR            10I 0
031200080505
031300080505     D*--------------------------------------------------
031400170119     D* Procedure name: YCO185sel_newWhereOr
031500080505     D* Purpose:        Inizia una clausola WHERE con almeno due condizioni...
031600080505     D*                           in OR.
031700080505     D* Returns:        Esito.
031800080505     D*--------------------------------------------------
031900170119     D YCO185sel_newWhereOr...
032000080505     D                 PR            10I 0
032100080505
032200080505     D*--------------------------------------------------
032300170119     D* Procedure name: YCO185sel_endWhereOr
032400080505     D* Purpose:        Chiude una clausola WHERE con almeno due condizioni...
032500080505     D*                           in OR.
032600080505     D* Returns:        Esito.
032700080505     D*--------------------------------------------------
032800170119     D YCO185sel_endWhereOr...
032900080505     D                 PR            10I 0
033000080505
033100080218     D*--------------------------------------------------
033200170119     D* Procedure name: YCO185sel_prepare
033300080218     D* Purpose:        Preparazione istruzione SQL.
033400080221     D* Parameter:      piMultipleRowFetch => Numero di righe da reperire ad ogni FETCH.
033500080218     D* Returns:        Esito.
033600080218     D*--------------------------------------------------
033700170119     D YCO185sel_prepare...
033800080218     D                 PR            10I 0
033900080221     D  piMultipleRowFetch...
034000080221     D                               10I 0 CONST
034100080218
034200080218     D*--------------------------------------------------
034300170119     D* Procedure name: YCO185sel_declareCursor
034400080218     D* Purpose:        Definizione del cursore SQL.
034500080218     D* Returns:        Esito.
034600080218     D*--------------------------------------------------
034700170119     D YCO185sel_declareCursor...
034800080218     D                 PR            10I 0
034900080218
035000080218     D*--------------------------------------------------
035100170119     D* Procedure name: YCO185sel_open
035200080218     D* Purpose:        Apertura cursore SQL.
035300080218     D* Returns:        Esito.
035400080218     D*--------------------------------------------------
035500170119     D YCO185sel_open...
035600080218     D                 PR            10I 0
035700080218
035800080218     D*--------------------------------------------------
035900170119     D* Procedure name: YCO185sel_close
036000080218     D* Purpose:        Chiusura cursore SQL.
036100080218     D* Returns:        Esito.
036200080218     D*--------------------------------------------------
036300170119     D YCO185sel_close...
036400080218     D                 PR            10I 0
036500080218
036600080218     D*--------------------------------------------------
036700170119     D* Procedure name: YCO185sel_fetchNext
036800080218     D* Purpose:        Posiziona il cursore sulla riga seguente.
036900080221     D* Parameter:      piRowCount => Number of rows retrieved.
037000080221     D* Parameter:      piDb2LastRow => Contains +100 if the last row was fetched.
037100080221     D* Returns:        Esito.
037200080218     D*--------------------------------------------------
037300170119     D YCO185sel_fetchNext...
037400080218     D                 PR            10I 0
037500170207     D  piRowCount                   10I 0
037600170207     D  piDb2LastRow                 10I 0
037700080219
037800080220     D*--------------------------------------------------
037900170119     D* Procedure name: YCO185sel_getNextRow
038000080220     D* Purpose:        Restituisce i dati di una fattura.
038100080220     D* Returns:        Esito.
038200080220     D* Parameter:      piNumeroRichiesta => Numero richiesta.
038300080220     D*--------------------------------------------------
038400170119     D YCO185sel_getNextRow...
038500080220     D                 PR            10I 0
038600080220     D  piNumeroRichiesta...
038700170207     D                               10I 0
038701170120     P*--------------------------------------------------
038733170119     P* Procedure name: YCO185sel_setWhere
038734140929     P* Purpose:        Imposta la WHERE
038735140929     P* Returns:        Esito.
038736140929     P* Parameter:
038737140929     P*--------------------------------------------------
038738170120     D YCO185sel_setWhereDataConsegna...
038739140929     D                 PR            10I 0
038740170120     D  piDaDataConsegna...
038741170207     D                                 D   DATFMT(*ISO) const
038742170120     D  piADataConsegna...
038751170207     D                                 D   DATFMT(*ISO) const
038752170207
038753170207     P*--------------------------------------------------
038820170119     P* Procedure name: YCO185sel_setWhere
038821141118     P* Purpose:        Imposta la WHERE
038822141118     P* Returns:        Esito.
038823141118     P* Parameter:
038824141118     P*--------------------------------------------------
038825170120     D YCO185sel_setWhereTipoInserimento...
038826141118     D                 PR            10I 0
038827170120     D  piTipoInserimento...
038828170207     D                                1A   CONST
038839170120
038840170206      // --------------------------------------------------
038841170206      // Procedure name: YCO185sel_setWhereDataOrdine
038842170206      // Purpose:        Imposta la WHERE sulla Data Ordine
038843170206      // --------------------------------------------------
038844170206       dcl-pr YCO185sel_setWhereDataOrdine INT(10);
038846170207           piDaDataOrdine date(*iso) const;
038847170207           piADataOrdine date(*iso) const;
038848170206       END-Pr;
038849170206
038850170322      // --------------------------------------------------
038851170322      // Procedure name: YCO185sel_setWhereDataDDT
038852170322      // Purpose:        Imposta la WHERE sulla Data DDT
038853170322      // --------------------------------------------------
038854170322       dcl-pr YCO185sel_setWhereDataDDT INT(10);
038855170322           piDaDataDDT date(*iso) const;
038856170322           piADataDDT date(*iso) const;
038857170322       END-Pr;
038858170322
038859170322      // --------------------------------------------------
038860170322      // Procedure name: YCO185sel_setWhereDataFAT
038861170322      // Purpose:        Imposta la WHERE sulla Data FAT
038862170322      // --------------------------------------------------
038863170322       dcl-pr YCO185sel_setWhereDataFAT INT(10);
038864170322           piDaDataFAT date(*iso) const;
038865170322           piADataFAT date(*iso) const;
038866170322       END-Pr;
038867170322
038868170322      // --------------------------------------------------
038869170206      // Procedure name: YCO185sel_setWhereDataInserimento
038870170206      // Purpose:        Imposta la WHERE sulla Data Ins.
038871170206      // --------------------------------------------------
038872170206       dcl-pr YCO185sel_setWhereDataInserimento INT(10);
038873170207           piDaDataInserimento date(*iso) const;
038874170207           piADataInserimento date(*iso) const;
038875170206       END-Pr;
038876170206
038877170206      // --------------------------------------------------
038878170206      // Procedure name: YCO185sel_setWhereUfficioInserimento
038879170206      // --------------------------------------------------
038880170206       dcl-pr YCO185sel_setWhereUfficioInserimento INT(10);
038881170207         piUfficioInserimento char(2) const;
038882170206       END-Pr;
038883170206
038884170206      // --------------------------------------------------
038885170206      // Procedure name: YCO185sel_setWhereUtenteInserimento
038886170206      // --------------------------------------------------
038888170206       dcl-pr YCO185sel_setWhereUtenteInserimento INT(10);
038889170207         piUtenteInserimento char(10) const;
038890170206       END-Pr;
038891170206
038892170206      // --------------------------------------------------
038893170206      // Procedure name: YCO185sel_setWhereCodFiscaleFornitore
038894170206      // --------------------------------------------------
038899170206       dcl-pr YCO185sel_setWhereCodiceFiscaleFornitore  INT(10);
038900170207         piCodiceFiscaleFornitore char(20) const;
038901170206       end-pr;
038905170206
038906170206      // --------------------------------------------------
038907170206      // Procedure name: YCO185sel_setWhereCodFiscaleFornitore
038908170206      // --------------------------------------------------
038909170206       dcl-pr YCO185sel_setWherePartitaIvaFornitore  INT(10);
038910170207         piPartitaIvaFornitore char(20) const;
038911170206       end-pr;
038912170206
038913170206      // --------------------------------------------------
038914170206      // Procedure name: YCO185sel_setWhereStato
038915170206      // --------------------------------------------------
038916170206       dcl-pr YCO185sel_setWhereStato INT(10);
038917170228         piStato char(1) const;
038918170206       END-Pr;
038919170206
038920170206      // --------------------------------------------------
038921170206      // Procedure name: YCO185sel_setWhereFase
038922170206      // --------------------------------------------------
038923170207       dcl-pr YCO185sel_setWhereFase INT(10);
038924170207         piFase char(2) const;
038925170206       END-Pr;
038926170206
038927170206      // --------------------------------------------------
038928170206      // Procedure name: YCO185sel_setWhereImponibile
038929170206      // --------------------------------------------------
038930170207       dcl-pr YCO185sel_setWhereImponibile INT(10);
038931170207         piimponibile zoned(14:2) const;
038932170206       END-Pr;
038933170206
038934170206      // --------------------------------------------------
038935170206      // Procedure name: YCO185sel_setWhereCodiceProdotto
038936170206      // --------------------------------------------------
038937170206       dcl-pr YCO185sel_setWhereCodiceProdotto INT(10);
038938170207         piProdotto char(10) const;
038939170206       END-Pr;
038940170206
038941170207      // --------------------------------------------------
038942170207      // Procedure name: YCO185sel_setWhereCodiceAmbito
038943170207      // --------------------------------------------------
038944170207       dcl-pr YCO185sel_setWhereCodiceAmbito INT(10);
038945170207         piAmbito char(10) const;
038946170207       END-Pr;
038947170207
038948170210      // --------------------------------------------------
038949170831      // Procedure name: YCO185sel_setWhere
038950170210      // --------------------------------------------------
038951170210       dcl-pr YCO185sel_setWhereDocumentoSelezionato INT(10);
038952170210         piidrigauto zoned(9) const;
038953170210       END-Pr;
038954170210
038955170313      // --------------------------------------------------
038956170831      // Procedure name: YCO185sel_setWhere
038957170313      // --------------------------------------------------
038958170313       dcl-pr YCO185sel_setWhereNotDocumentiSelezionati INT(10);
038959170313         piidrigauto zoned(9) const;
038960170313       END-Pr;
038961170313
038963170831      // --------------------------------------------------
038964170831      // Procedure name: YCO185sel_setWhereCdc
038965170831      // --------------------------------------------------
038966170831       dcl-pr YCO185sel_setWhereCdc INT(10);
038967170831         piIdOrdine char(8) const;
038968170831       END-Pr;
038969170831
038970170831      // --------------------------------------------------
038971170831      // Procedure name: YCO185sel_setWhereIdOrdine
038972170208      // --------------------------------------------------
038973170831       dcl-pr YCO185sel_setWhereIdOrdine INT(10);
038974170831         piIdOrdine char(22) const;
038975170208       END-Pr;
038976170208
038977171003      // --------------------------------------------------
038978171003      // Procedure name: YCO185sel_setWhereDivisa
038979171003      // --------------------------------------------------
038980171003       dcl-pr YCO185sel_setWhereDivisa INT(10);
038981171003         piDivisa char(4) const;
038982171003       END-Pr;
038983171003
038984171016      // --------------------------------------------------
038985171016      // Procedure name: YCO185sel_setWhereIdFiliale
038986171016      // --------------------------------------------------
038987171016       dcl-pr YCO185sel_setWhereIdFiliale INT(10);
038988171016         piidrigauto packed(3:0) const;
038989171016       END-Pr;
038990171016
038991171027             // --------------------------------------------------
038992171027      // Procedure name: YCO185sel_setWhereDataBenestare
038993171027      // Purpose:        Imposta la WHERE sulla Data Benestare
038994171027      // --------------------------------------------------
038995171027       dcl-pr YCO185sel_setWhereDataBenestare INT(10);
038996171027           piDaDataBenestare date(*iso) const;
038997171027           piADataBenestare date(*iso) const;
038998171027       END-Pr;
038999171027
039000171027      // --------------------------------------------------
039001171027      // Procedure name: YCO185sel_setWhereIdBenestare
039002171027      // --------------------------------------------------
039003171027       dcl-pr YCO185sel_setWhereIdBenestare INT(10);
039004171027         piIdOrdine char(22) const;
039005171027       END-Pr;
039006171027
