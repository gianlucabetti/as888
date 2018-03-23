000100080218     D*--------------------------------------------------
000200170307     D* Procedure name: YCO189sel_init
000300080218     D* Purpose:        Inizializza modulo.
000400080218     D* Returns:        Esito.
000500080609     D* Parameter:      piKpjba => KPJBA
000600080218     D*--------------------------------------------------
000700170307     D YCO189sel_init...
000800080218     D                 PR            10I 0
000900080609     D  piKpjba                            LIKEDS(kpjba)
001000080609     D                                     OPTIONS(*NOPASS)
001100080218
001200080218     D*--------------------------------------------------
001300170307     D* Procedure name: YCO189sel_finalize
001400080218     D* Purpose:        Chiusura modulo.
001500080218     D* Returns:        Esito.
001600080218     D*--------------------------------------------------
001700170307     D YCO189sel_finalize...
001800080218     D                 PR            10I 0
001900080218
002000080218     D*--------------------------------------------------
002100170307     D* Procedure name: YCO189sel_newStatement
002200080505     D* Purpose:        Nuova istruzione SQL.
002300080218     D* Returns:        Esito.
002400080218     D*--------------------------------------------------
002500170307     D YCO189sel_newStatement...
002600080218     D                 PR            10I 0
002700080218
002800080219     D*--------------------------------------------------
002900170307     D* Procedure name: YCO189sel_endStatement
003000080505     D* Purpose:        Fine istruzione SQL.
003100080219     D* Returns:        Esito.
003200080219     D*--------------------------------------------------
003300170307     D YCO189sel_endStatement...
003400080219     D                 PR            10I 0
003500170120
004700080218     D*--------------------------------------------------
004800170307     D* Procedure name: YCO189sel_setWhereidRiga
004900140828     D* Purpose:        Imposta la WHERE
005000080218     D* Returns:        Esito.
005100140828     D* Parameter:
005200140828     D*--------------------------------------------------
005400170307     D YCO189sel_setWhereidRiga...
005500080218     D                 PR            10I 0
005600170120     D  piidRiga...
005700140828     D                               10I 0 CONST
005800140828
010500080219     D*--------------------------------------------------
010600170307     D* Procedure name: YCO189sel_setWhereSocieta
010700080219     D* Purpose:        Imposta la WHERE sulla società.
010800080219     D* Returns:        Esito.
010900080219     D* Parameter:      piSocieta => Società.
011000080219     D*--------------------------------------------------
011100170307     D YCO189sel_setWhereSocieta...
011200080219     D                 PR            10I 0
011300171102     D  piSocieta                     3A   const
011400080219
011500080219     D*--------------------------------------------------
011600170307     D* Procedure name: YCO189sel_setWhereUnita
011700080219     D* Purpose:        Imposta la WHERE sulla unità.
011800080219     D* Returns:        Esito.
011900080219     D* Parameter:      piUnita => Unità.
012000080219     D*--------------------------------------------------
012100170307     D YCO189sel_setWhereUnita...
012200080219     D                 PR            10I 0
012300171102     D  piUnita                       8A   const
012301170120
012302171102     D*--------------------------------------------------
012303171102     D* Procedure name: YCO189sel_setWhereUnita
012304171102     D* Purpose:        Imposta la WHERE sulla unità.
012305171102     D* Returns:        Esito.
012306171102     D* Parameter:      piUnita => Unità.
012307171102     D*--------------------------------------------------
012308171102     D YCO189sel_setWhereUnitaAutorizzate...
012309171102     D                 PR            10I 0
012310171102
012500080219     D*--------------------------------------------------
012600170307     D* Procedure name: YCO189sel_setWhereIdCliente
012700080219     D* Purpose:        Imposta la WHERE sul codice cliente.
012800080219     D* Returns:        Esito.
012900080219     D* Parameter:      piKscCliente => Codice cliente.
013000080219     D*--------------------------------------------------
013100170307     D YCO189sel_setWhereIdFornitore...
013200080219     D                 PR            10I 0
013300170206     D  piIdFornitore                 8A   CONST
013400170120
029700080222     D*--------------------------------------------------
029800170307     D* Procedure name: YCO189sel_newWhere
029900080222     D* Purpose:        Pulisce la clausola WHERE.
030000080222     D* Returns:        Esito.
030100080222     D*--------------------------------------------------
030200170307     D YCO189sel_newWhere...
030300080222     D                 PR            10I 0
030400080505
030500080505     D*--------------------------------------------------
030600170307     D* Procedure name: YCO189sel_endWhere
030700080505     D* Purpose:        Fine clausola WHERE.
030800080505     D* Returns:        Esito.
030900080505     D*--------------------------------------------------
031000170307     D YCO189sel_endWhere...
031100080505     D                 PR            10I 0
031200080505
031300080505     D*--------------------------------------------------
031400170307     D* Procedure name: YCO189sel_newWhereOr
031500080505     D* Purpose:        Inizia una clausola WHERE con almeno due condizioni...
031600080505     D*                           in OR.
031700080505     D* Returns:        Esito.
031800080505     D*--------------------------------------------------
031900170307     D YCO189sel_newWhereOr...
032000080505     D                 PR            10I 0
032100080505
032200080505     D*--------------------------------------------------
032300170307     D* Procedure name: YCO189sel_endWhereOr
032400080505     D* Purpose:        Chiude una clausola WHERE con almeno due condizioni...
032500080505     D*                           in OR.
032600080505     D* Returns:        Esito.
032700080505     D*--------------------------------------------------
032800170307     D YCO189sel_endWhereOr...
032900080505     D                 PR            10I 0
033000080505
033100080218     D*--------------------------------------------------
033200170307     D* Procedure name: YCO189sel_prepare
033300080218     D* Purpose:        Preparazione istruzione SQL.
033400080221     D* Parameter:      piMultipleRowFetch => Numero di righe da reperire ad ogni FETCH.
033500080218     D* Returns:        Esito.
033600080218     D*--------------------------------------------------
033700170307     D YCO189sel_prepare...
033800080218     D                 PR            10I 0
033900080221     D  piMultipleRowFetch...
034000080221     D                               10I 0 CONST
034100080218
034200080218     D*--------------------------------------------------
034300170307     D* Procedure name: YCO189sel_declareCursor
034400080218     D* Purpose:        Definizione del cursore SQL.
034500080218     D* Returns:        Esito.
034600080218     D*--------------------------------------------------
034700170307     D YCO189sel_declareCursor...
034800080218     D                 PR            10I 0
034900080218
035000080218     D*--------------------------------------------------
035100170307     D* Procedure name: YCO189sel_open
035200080218     D* Purpose:        Apertura cursore SQL.
035300080218     D* Returns:        Esito.
035400080218     D*--------------------------------------------------
035500170307     D YCO189sel_open...
035600080218     D                 PR            10I 0
035700080218
035800080218     D*--------------------------------------------------
035900170307     D* Procedure name: YCO189sel_close
036000080218     D* Purpose:        Chiusura cursore SQL.
036100080218     D* Returns:        Esito.
036200080218     D*--------------------------------------------------
036300170307     D YCO189sel_close...
036400080218     D                 PR            10I 0
036500080218
036600080218     D*--------------------------------------------------
036700170307     D* Procedure name: YCO189sel_fetchNext
036800080218     D* Purpose:        Posiziona il cursore sulla riga seguente.
036900080221     D* Parameter:      piRowCount => Number of rows retrieved.
037000080221     D* Parameter:      piDb2LastRow => Contains +100 if the last row was fetched.
037100080221     D* Returns:        Esito.
037200080218     D*--------------------------------------------------
037300170307     D YCO189sel_fetchNext...
037400080218     D                 PR            10I 0
037500170207     D  piRowCount                   10I 0
037600170207     D  piDb2LastRow                 10I 0
037700080219
037800080220     D*--------------------------------------------------
037900170307     D* Procedure name: YCO189sel_getNextRow
038000080220     D* Purpose:        Restituisce i dati di una fattura.
038100080220     D* Returns:        Esito.
038200080220     D* Parameter:      piNumeroRichiesta => Numero richiesta.
038300080220     D*--------------------------------------------------
038400170307     D YCO189sel_getNextRow...
038500080220     D                 PR            10I 0
038600080220     D  piNumeroRichiesta...
038700170207     D                               10I 0
038701170120     P*--------------------------------------------------
038733170307     P* Procedure name: YCO189sel_setWhere
038734140929     P* Purpose:        Imposta la WHERE
038735140929     P* Returns:        Esito.
038736140929     P* Parameter:
038737140929     P*--------------------------------------------------
038738170307     D YCO189sel_setWhereDataRegistrazione...
038739140929     D                 PR            10I 0
038740170307     D  piDaDataRegistrazione...
038741170207     D                                 D   DATFMT(*ISO) const
038742170307     D  piADataRegistrazione...
038751170207     D                                 D   DATFMT(*ISO) const
038752170207
038753170207     P*--------------------------------------------------
038820170307     P* Procedure name: YCO189sel_setWhere
038821141118     P* Purpose:        Imposta la WHERE
038822141118     P* Returns:        Esito.
038823141118     P* Parameter:
038824141118     P*--------------------------------------------------
038825170307     D YCO189sel_setWhereTipoInserimento...
038826141118     D                 PR            10I 0
038827170120     D  piTipoInserimento...
038828170207     D                                1A   CONST
038839170120
038840170206      // --------------------------------------------------
038841170307      // Procedure name: YCO189sel_setWhereDataDocumento
038842170307      // Purpose:        Imposta la WHERE sulla Data Documento
038843170206      // --------------------------------------------------
038844170307       dcl-pr YCO189sel_setWhereDataDocumento INT(10);
038846170307           piDaDataDocumento date(*iso) const;
038847170307           piADataDocumento date(*iso) const;
038848170206       END-Pr;
038849170206
038850170206      // --------------------------------------------------
038851170307      // Procedure name: YCO189sel_setWhereDataFattura
038852170307      // Purpose:        Imposta la WHERE sulla Data Fattura
038853170206      // --------------------------------------------------
038854170307       dcl-pr YCO189sel_setWhereDataFattura INT(10);
038856170307           piDaDataFattura date(*iso) const;
038857170307           piADataFattura date(*iso) const;
038858170206       END-Pr;
038859170206
038860170206      // --------------------------------------------------
038861170307      // Procedure name: YCO189sel_setWhereDataInserimento
038862170206      // Purpose:        Imposta la WHERE sulla Data Ins.
038863170206      // --------------------------------------------------
038864170307       dcl-pr YCO189sel_setWhereDataInserimento INT(10);
038866170207           piDaDataInserimento date(*iso) const;
038867170207           piADataInserimento date(*iso) const;
038868170206       END-Pr;
038869170206
038870170206      // --------------------------------------------------
038871170307      // Procedure name: YCO189sel_setWhereUfficioInserimento
038872170206      // --------------------------------------------------
038877170307       dcl-pr YCO189sel_setWhereUfficioInserimento INT(10);
038880170207         piUfficioInserimento char(2) const;
038882170206       END-Pr;
038883170206
038884170206      // --------------------------------------------------
038885170307      // Procedure name: YCO189sel_setWhereUtenteInserimento
038886170206      // --------------------------------------------------
038888170307       dcl-pr YCO189sel_setWhereUtenteInserimento INT(10);
038889170207         piUtenteInserimento char(10) const;
038890170206       END-Pr;
038891170206
038892170206      // --------------------------------------------------
038893170307      // Procedure name: YCO189sel_setWhereCodFiscaleFornitore
038894170206      // --------------------------------------------------
038899170307       dcl-pr YCO189sel_setWhereCodiceFiscaleFornitore  INT(10);
038900170207         piCodiceFiscaleFornitore char(20) const;
038901170206       end-pr;
038905170206
038906170206      // --------------------------------------------------
038907170307      // Procedure name: YCO189sel_setWhereCodFiscaleFornitore
038908170206      // --------------------------------------------------
038909170307       dcl-pr YCO189sel_setWherePartitaIvaFornitore  INT(10);
038910170207         piPartitaIvaFornitore char(20) const;
038911170206       end-pr;
038912170307
038927170206      // --------------------------------------------------
038928170307      // Procedure name: YCO189sel_setWhereImponibile
038929170206      // --------------------------------------------------
038930170307       dcl-pr YCO189sel_setWhereImponibile INT(10);
038931170207         piimponibile zoned(14:2) const;
038932170206       END-Pr;
038933170307
038934171110      // --------------------------------------------------
038935171110      // Procedure name: YCO189sel_setWhereImporto
038936171110      // --------------------------------------------------
038937171110       dcl-pr YCO189sel_setWhereImporto INT(10);
038938171110         piimporto zoned(14:2) const;
038939171110       END-Pr;
038940171110
038941170207      // --------------------------------------------------
038942170307      // Procedure name: YCO189sel_setWhereCodiceAmbito
038943170207      // --------------------------------------------------
038944170307       dcl-pr YCO189sel_setWhereCodiceAmbito INT(10);
038945170207         piAmbito char(10) const;
038946170207       END-Pr;
038947170307
038948170519      // --------------------------------------------------
038949170519      // Procedure name: YCO189sel_setWhereIdDocFlow
038950170519      // --------------------------------------------------
038951170519       dcl-pr YCO189sel_setWhereIdDocFlow INT(10);
038952170522         piId_DocFlow char(20) const;
038953170519       END-Pr;
038954170519
038955170522      // --------------------------------------------------
038956170522      // Procedure name: YCO189sel_setWhereFase
038957170522      // --------------------------------------------------
038958170522       dcl-pr YCO189sel_setWhereFase INT(10);
038959170522         piIdFase char(2) const;
038960170522       END-Pr;
038961170522
038962170522      // --------------------------------------------------
038963170522      // Procedure name: YCO189sel_setWhereUtenteApprovatore
038964170522      // --------------------------------------------------
038965170522       dcl-pr YCO189sel_setWhereUtenteApprovatore INT(10);
038966170522         piUtenteApprovatore char(10) const;
038967170522       END-Pr;
038968170522
038969170522      // --------------------------------------------------
038970170522      // Procedure name: YCO189sel_setWhereDataApprovazione
038971170522      // --------------------------------------------------
038972170522       dcl-pr YCO189sel_setWhereDataApprovazione INT(10);
038973170522         piDaDataApprovazione date const;
038974170522         piADataApprovazione date const;
038975170522       END-Pr;
038976170522
038977170522      // --------------------------------------------------
038978170522      // Procedure name: YCO189sel_setWhereUtenteRegistrazione
038979170522      // --------------------------------------------------
038980170522       dcl-pr YCO189sel_setWhereUtenteRegistrazione INT(10);
038981170522         piUtenteRegistrazione char(10) const;
038982170522       END-Pr;
038983170522
038984170522      // --------------------------------------------------
038985170522      // Procedure name: YCO189sel_setWhereStato
038986170522      // --------------------------------------------------
038987170522       dcl-pr YCO189sel_setWhereStato INT(10);
038988170522         piStato char(1) const;
038989170522       END-Pr;
038990170522
038991170522      // --------------------------------------------------
038992170522      // Procedure name: YCO189sel_setWhereIdReg
038993170522      // --------------------------------------------------
038994170522       dcl-pr YCO189sel_setWhereIdReg INT(10);
038995170522           piIdreg zoned(9:0) const;
038996170522           piSerieReg char(4) const;
038997170522       END-Pr;
038998170522
038999170522      // --------------------------------------------------
039000170522      // Procedure name: YCO189sel_setWhereIdDoc
039001170522      // --------------------------------------------------
039002170522       dcl-pr YCO189sel_setWhereIdDoc INT(10);
039003170522         piIdDoc char(20) const;
039004170522       END-Pr;
039005170522
039006170522      // --------------------------------------------------
039007170522      // Procedure name: YCO189sel_setWhereIdFat
039008170522      // --------------------------------------------------
039009170522       dcl-pr YCO189sel_setWhereIdFat INT(10);
039010170522         piIdFat char(20) const;
039011170522       END-Pr;
039012170522
039013170522      // --------------------------------------------------
039014170522      // Procedure name: YCO189sel_setWhereTipoDocumento
039015170522      // --------------------------------------------------
039016170522       dcl-pr YCO189sel_setWhereTipoDocumento INT(10);
039017170522           piTipoDocumento char(1) const;
039018170522       END-Pr;
039019170522
039020170524      // --------------------------------------------------
039021170524      // Procedure name: YCO189sel_setWhereTipoFornitore
039022170524      // --------------------------------------------------
039023170524       dcl-pr YCO189sel_setWhereTipoFornitore INT(10);
039024170524           piTipoFornitore char(1) const;
039025170524       END-Pr;
039026170524
039027170524      // --------------------------------------------------
039028170524      // Procedure name: YCO189sel_setWhereNaturaContropartita
039029170524      // --------------------------------------------------
039030170524       dcl-pr YCO189sel_setWhereNaturaContropartita INT(10);
039031170524           piNatura char(1) const;
039032170524       END-Pr;
039033170524
039034170718      // --------------------------------------------------
039035170718      // Procedure name: YCO189sel_setWhereUtenteAssegnato
039036170718      // --------------------------------------------------
039037170718       dcl-pr YCO189sel_setWhereUtenteAssegnato INT(10);
039038170718         piUtenteAssegnato char(10) const;
039039170718       END-Pr;
039040170718
039041171009      // --------------------------------------------------
039042171009      // Procedure name: YCO189sel_setWhereBloccati
039043171009      // --------------------------------------------------
039044171009       dcl-pr YCO189sel_setWhereBloccati INT(10);
039045171010         piStatoBlocco char(1) const;
039046171009       END-Pr;
039047171009
