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
003600080218     D*--------------------------------------------------
003700140724     D* Procedure name: PropostaBlocco_sel_setWhereNumeroRichiesta
003800080218     D* Purpose:        Imposta la WHERE sul numero richiesta.
003900080218     D* Returns:        Esito.
004000080218     D* Parameter:      piNumeroRichiesta => Numero richiesta.
004100080218     D*--------------------------------------------------
004200140724     D PropostaBlocco_sel_setWhereNumeroRichiesta...
004300080218     D                 PR            10I 0
004400080218     D  piNumeroRichiesta...
004500080430     D                               10I 0 CONST
004600080218
004700080218     D*--------------------------------------------------
004800140724     D* Procedure name: PropostaBlocco_sel_setWhereDataRichiesta
004900080218     D* Purpose:        Imposta la WHERE sulla data richiesta.
005000080218     D* Returns:        Esito.
005100080218     D* Parameter:      piDataRichiestaIniziale => Data richiesta iniziale.
005200080218     D* Parameter:      piDataRichiestaFinale => Data richiesta finale.
005300080218     D*--------------------------------------------------
005400140724     D PropostaBlocco_sel_setWhereDataRichiesta...
005500080218     D                 PR            10I 0
005600080218     D  piDataRichiestaIniziale...
005700080218     D                                 D   DATFMT(*ISO)
005800080218     D  piDataRichiestaFinale...
005900080218     D                                 D   DATFMT(*ISO)
006000080218
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
007300080219     D*--------------------------------------------------
007400140724     D* Procedure name: PropostaBlocco_sel_setWhereUfficioRichiedente
007500080219     D* Purpose:        Imposta la WHERE su ufficio richiedente.
007600080219     D* Returns:        Esito.
007700080219     D* Parameter:      piUfficioRichiedente => Ufficio richiedente.
007800080219     D*--------------------------------------------------
007900140724     D PropostaBlocco_sel_setWhereUfficioRichiedente...
008000080219     D                 PR            10I 0
008100080219     D  piUfficioRichiedente...
008200080219     D                                2A
008300080219
008400080219     D*--------------------------------------------------
008500140724     D* Procedure name: PropostaBlocco_sel_setWhereFilialeRichiedente
008600080219     D* Purpose:        Imposta la WHERE sulla filiale richiedente.
008700080219     D* Returns:        Esito.
008800080219     D* Parameter:      piFilialeRichiedente => Filiale richiedente.
008900080219     D*--------------------------------------------------
009000140724     D PropostaBlocco_sel_setWhereFilialeRichiedente...
009100080219     D                 PR            10I 0
009200080219     D  piFilialeRichiedente...
009300080430     D                                3P 0 CONST
009400080219
009500080219     D*--------------------------------------------------
009600140724     D* Procedure name: PropostaBlocco_sel_setWhereAmbito
009700080219     D* Purpose:        Imposta la WHERE sul codice ambito.
009800080219     D* Returns:        Esito.
009900080219     D* Parameter:      piAmbito => Ambito.
010000080219     D*--------------------------------------------------
010100140724     D PropostaBlocco_sel_setWhereAmbito...
010200080219     D                 PR            10I 0
010300080219     D  piAmbito                     10A
010400080219
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
012300080219     D  piUnita                       8A
012400080219
012500080219     D*--------------------------------------------------
012600140724     D* Procedure name: PropostaBlocco_sel_setWhereCliente
012700080219     D* Purpose:        Imposta la WHERE sul codice cliente.
012800080219     D* Returns:        Esito.
012900080219     D* Parameter:      piKscCliente => Codice cliente.
013000080219     D*--------------------------------------------------
013100140724     D PropostaBlocco_sel_setWhereCliente...
013200080219     D                 PR            10I 0
013300080505     D  piKscCliente                  8A   CONST
013400080219
013500080220     D*--------------------------------------------------
013600140724     D* Procedure name: PropostaBlocco_sel_setWhereCongelata
013700080220     D* Purpose:        Imposta la WHERE sul flag di congelamento.
013800080220     D* Returns:        Esito.
013900080221     D* Parameter:      piCongelata => *ON = congelate, *OFF = non congelate
014000080220     D*--------------------------------------------------
014100140724     D PropostaBlocco_sel_setWhereCongelata...
014200080220     D                 PR            10I 0
014300080221     D  piCongelata                    N   CONST
014400080220
014500080221     D*--------------------------------------------------
014600140724     D* Procedure name: PropostaBlocco_sel_setWhereAnnullata
014700080221     D* Purpose:        Imposta la WHERE sul flag di annullamento.
014800080221     D* Returns:        Esito.
014900080221     D* Parameter:      piAnnullata => *ON = annullate, *OFF = non annullate.
015000080221     D*--------------------------------------------------
015100140724     D PropostaBlocco_sel_setWhereAnnullata...
015200080221     D                 PR            10I 0
015300080221     D  piAnnullata                    N   CONST
015400080221
015500080626     D*--------------------------------------------------
015600140724     D* Procedure name: PropostaBlocco_sel_setWhereProtocollata
015700080626     D* Purpose:        Imposta la WHERE sullo stato di fattura protocollata.
015800080626     D* Returns:        Esito.
015900080626     D* Parameter:      piProtocollata => *ON = fatture protocollate, *OFF ...
016000080626     D*                          = fatture non protocollate.
016100080626     D*--------------------------------------------------
016200140724     D PropostaBlocco_sel_setWhereProtocollata...
016300080626     D                 PR            10I 0
016400080626     D piProtocollata                  N   CONST
016500080626
016600080221     D*--------------------------------------------------
016700140724     D* Procedure name: PropostaBlocco_sel_setWhereRegistrata
016800080221     D* Purpose:        Imposta la WHERE sul flag fattura registrata.
016900080221     D* Returns:        Esito.
017000080221     D* Parameter:      piRegistrata => *ON = registrate, *OFF = non regist...
017100080221     D*                          rate.
017200080221     D*--------------------------------------------------
017300140724     D PropostaBlocco_sel_setWhereRegistrata...
017400080221     D                 PR            10I 0
017500080221     D  piRegistrata                   N   CONST
017600080221
017700080221     D*--------------------------------------------------
017800140724     D* Procedure name: PropostaBlocco_sel_setWhereRichiestaStampata
017900080221     D* Purpose:        Imposta la WHERE sul flag di richiesta stampata.
018000080221     D* Returns:        Esito.
018100080221     D* Parameter:      piRichiestaStampata => *ON = richieste stampate, *O...
018200080221     D*                          FF = richieste non stampate.
018300080221     D*--------------------------------------------------
018400140724     D PropostaBlocco_sel_setWhereRichiestaStampata...
018500080221     D                 PR            10I 0
018600080221     D  piRichiestaStampata...
018700080221     D                                 N   CONST
018800080221
018900080221     D*--------------------------------------------------
019000140724     D* Procedure name: PropostaBlocco_sel_setWhereFatturaClienteStampata
019100080221     D* Purpose:        Imposta la WHERE sul flag di stampa copia fattura p...
019200080221     D*                          er il cliente.
019300080221     D* Returns:        Esito.
019400080221     D* Parameter:      piFatturaClienteStampata => *ON = fattura cliente s...
019500080221     D*                          tampata, *OFF = fattura cliente non stampata.
019600080221     D*--------------------------------------------------
019700140724     D PropostaBlocco_sel_setWhereFatturaClienteStampata...
019800080221     D                 PR            10I 0
019900080221     D  piFatturaClienteStampata...
020000080221     D                                 N   CONST
020100080221
020200080221     D*--------------------------------------------------
020300140724     D* Procedure name: PropostaBlocco_sel_setWhereFatturaInternaStampata
020400080221     D* Purpose:        Imposta la WHERE sul flag di copia fattura interna ...
020500080221     D*                          stampata.
020600080221     D* Returns:        Esito.
020700080221     D* Parameter:      piFatturaInternaStampata => *ON = fattura interna s...
020800080221     D*                          tampata, *OFF = fattura interna non stampata.
020900080221     D*--------------------------------------------------
021000140724     D PropostaBlocco_sel_setWhereFatturaInternaStampata...
021100080221     D                 PR            10I 0
021200080221     D  piFatturaInternaStampata...
021300080221     D                                 N   CONST
021400080221
021500080221     D*--------------------------------------------------
021600140724     D* Procedure name: PropostaBlocco_sel_setWhereAutorizzata
021700080221     D* Purpose:        Imposta la WHERE sul flag di fattura autorizzata.
021800080221     D* Returns:        Esito.
021900080221     D* Parameter:      piAutorizzata => *ON = autorizzata, *OFF = non auto...
022000080221     D*                          rizzata.
022100080221     D*--------------------------------------------------
022200140724     D PropostaBlocco_sel_setWhereAutorizzata...
022300080221     D                 PR            10I 0
022400080221     D  piAutorizzata                  N   CONST
022500080221
022600080221     D*--------------------------------------------------
022700140724     D* Procedure name: PropostaBlocco_sel_setWhereControllata
022800080221     D* Purpose:        Imposta la WHERE sul flag di fattura controllata.
022900080221     D* Returns:        Esito.
023000080221     D* Parameter:      piControllata => *ON = controllata, *OFF = non cont...
023100080221     D*                          rollata.
023200080221     D*--------------------------------------------------
023300140724     D PropostaBlocco_sel_setWhereControllata...
023400080221     D                 PR            10I 0
023500080221     D  piControllata                  N   CONST
023600080221
023700080221     D*--------------------------------------------------
023800140724     D* Procedure name: PropostaBlocco_sel_setWhereNumeroRegistrazione
023900080221     D* Purpose:        Imposta la WHERE su ID registrazione.
024000080221     D* Returns:        Esito.
024100080221     D* Parameter:      piNumeroRegistrazione => Numero registrazione.
024200080221     D*--------------------------------------------------
024300140724     D PropostaBlocco_sel_setWhereNumeroRegistrazione...
024400080221     D                 PR            10I 0
024500080221     D  piNumeroRegistrazione...
024600080626     D                               10I 0 CONST
024700080228
024800080626     D*--------------------------------------------------
024900140724     D* Procedure name: PropostaBlocco_sel_setWhereDataRegistrazione
025000080626     D* Purpose:        Imposta la WHERE sulla data registrazione.
025100080626     D* Returns:        Esito.
025200080626     D* Parameter:      piDataRegistrazioneIniziale => Data registrazione i...
025300080626     D*                          niziale.
025400080626     D* Parameter:      piDataRegistrazioneFinale => Data registrazione finale.
025500080626     D*--------------------------------------------------
025600140724     D PropostaBlocco_sel_setWhereDataRegistrazione...
025700080626     D                 PR            10I 0
025800080626     D  piDataRegistrazioneIniziale...
025900080626     D                                 D   DATFMT(*ISO)
026000080626     D  piDataRegistrazioneFinale...
026100080626     D                                 D   DATFMT(*ISO)
026200080626
026300080626     D*--------------------------------------------------
026400140724     D* Procedure name: PropostaBlocco_sel_setWhereSerieRegistrazione
026500080626     D* Purpose:        Imposta la WHERE sulla serie registrazione.
026600080626     D* Returns:        Esito.
026700080626     D* Parameter:      piSerieRegistrazione => Serie registrazione.
026800080626     D*--------------------------------------------------
026900140724     D PropostaBlocco_sel_setWhereSerieRegistrazione...
027000080626     D                 PR            10I 0
027100080626     D  piSerieRegistrazione...
027200080626     D                                4A
027300080626
027400080519     D*--------------------------------------------------
027500140724     D* Procedure name: PropostaBlocco_sel_setWhereStatoAvanzamento
027600080519     D* Purpose:        Imposta la WHERE sulla fase.
027700080519     D* Returns:        Esito.
027800080529     D* Parameter:      piStatoAvanzamento => Fase.
027900080519     D*--------------------------------------------------
028000140724     D PropostaBlocco_sel_setWhereStatoAvanzamento...
028100080519     D                 PR            10I 0
028200080529     D  piStatoAvanzamento...
028300080529     D                                2A   CONST
028400080519
028500080526     D*--------------------------------------------------
028600140724     D* Procedure name: PropostaBlocco_sel_setWhereListaAutorizzazione
028700080526     D* Purpose:        Imposta la WHERE sulla lista autorizzazione.
028800080526     D* Returns:        Esito.
028900080526     D* Parameter:      piNumeroListaAutorizzazione => Numero lista autoriz...
029000080526     D*                          zazione
029100080526     D*--------------------------------------------------
029200140724     D PropostaBlocco_sel_setWhereListaAutorizzazione...
029300080526     D                 PR            10I 0
029400080526     D  piNumeroListaAutorizzazione...
029500080526     D                               10I 0 CONST
029600080526
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
038800080220
038900081105     D*--------------------------------------------------
039000140724     D* Procedure name: PropostaBlocco_sel_setWhereFatturaRiemessa
039100081105     D* Purpose:        Imposta la WHERE su ID partita fattura riemessa.
039200081105     D* Returns:        Esito.
039300081105     D* Parameter:      piSocieta => Società.
039400081105     D* Parameter:      piSottoContoCliente => Sotto conto cliente.
039500081105     D* Parameter:      piDataPartita => Data partita.
039600081105     D* Parameter:      piNumeroPartita => Numero partita.
039700081105     D* Parameter:      piSeriePartita => Serie partita.
039800081105     D*--------------------------------------------------
039900140724     D PropostaBlocco_sel_setWhereFatturaRiemessa...
040000081105     D                 PR            10I 0
040100081105     D  piSocieta                     3A
040200081105     D  piCapoContoCliente...
040300081105     D                                6A   CONST OPTIONS(*OMIT)
040400081105     D  piSottoContoCliente...
040500081105     D                                8A
040600081105     D  piDataPartita                  D   DATFMT(*ISO)
040700081105     D                                     CONST
040800081105     D  piNumeroPartita...
040900081105     D                               10I 0 CONST
041000081105     D piSeriePartita                 4A   CONST
041100081105
041200081126     D*--------------------------------------------------
041300140724     D* Procedure name: PropostaBlocco_sel_setWhereCondizionePagamento
041400081126     D* Purpose:        Imppsta la WHERE sulla condizione pagamento.
041500081126     D* Returns:        Esito.
041600081126     D* Parameter:      piCondizionePagamento => Condizione pagamento.
041700081126     D*--------------------------------------------------
041800140724     D PropostaBlocco_sel_setWhereCondizionePagamento...
041900081126     D                 PR            10I 0
042000081126     D  piCondizionePagamento...
042100081126     D                                3A
042200081126
042300091204     D*--------------------------------------------------
042400140724     D* Procedure name: PropostaBlocco_sel_setWhereDataControllo
042500091204     D* Purpose:        Imposta la WHERE sulla data di controllo.
042600091204     D* Returns:        Esito.
042700091204     D* Parameter:      piDataControlloIniziale => Data controllo iniziale
042800091204     D* Parameter:      piDataControlloFinale => Data controllo finale.
042900091204     D*--------------------------------------------------
043000140724     D PropostaBlocco_sel_setWhereDataControllo...
043100091204     D                 PR            10I 0
043200091204     D  piDataControlloIniziale...
043300091204     D                                 D   DATFMT(*ISO)
043400091204     D                                     VALUE
043500091204     D  piDataControlloFinale...
043600091204     D                                 D   DATFMT(*ISO)
043700091204     D                                     VALUE
043800091204
