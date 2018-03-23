000100070314     D*--------------------------------------------------
000200080609     D* Procedure name: FatturaManuale_out_init
000300070509     P* Purpose:        Inizializza una nuova sessione di inserimento/aggiornamento.
000400070509     P*                 Da chiamare per primo, una sola volta.
000500070509     P* Returns:        Esito.
000600070509     D* Parameter:      pisStartCommitControl
000700070509     D* Parameter:      pisCommitControlFiles
000800070509     D* Parameter:      pisDoCommit
000900070509     D* Parameter:      piUnSoloCommitFinale
001000070509     P* Parameter:      pisLibreriaFiles
001100070509     P* Parameter:      piKpjba
001200070509     D*--------------------------------------------------
001300080609     D FatturaManuale_out_init...
001400070314     D                 PR            10I 0
001500070316     D  piStartCommitControl...
001600070315     D                                 N   OPTIONS(*NOPASS:*OMIT) CONST
001700070316     D  piCommitControlFiles...
001800070315     D                                 N   OPTIONS(*NOPASS:*OMIT) CONST
001900070316     D  piLibreriaFiles...
002000070315     D                               10A   OPTIONS(*NOPASS:*OMIT) CONST
002100070509     D  piKpjba...
002200070509     D                                     OPTIONS(*NOPASS:*OMIT)
002300070509     D                                     LIKE(kpjba)
002400070509     D  piSocietaLavoro...
002500070509     D                                3A   OPTIONS(*NOPASS)
002600070509
002700070314     D*--------------------------------------------------
002800080609     D* Procedure name: FatturaManuale_out_finalize
002900070314     D* Purpose:
003000070314     D* Returns:        Esito.
003100070608     D* Parameter:      piDoCommit => *ON = commit, *OFF = rollback, *O...
003200070608     D*                          MIT = niente.
003300070314     D*--------------------------------------------------
003400080609     D FatturaManuale_out_finalize...
003500070314     D                 PR            10I 0
003600070608     D  piDoCommit...
003700070608     D                                 N   OPTIONS(*NOPASS:*OMIT) CONST
003800070315
003900070315     D*--------------------------------------------------
004000070611     D* Procedure name: FatturaManuale_out_commitRollback
004100070315     D* Purpose:        Esegue COMMIT/ROLLBACK.
004200070315     D* Returns:        Esito.
004300070316     D* Parameter:      piCommitRollback => *ON = Commit; *OFF = RollBack.
004400070315     D*--------------------------------------------------
004500070611     D FatturaManuale_out_commitRollback...
004600070315     D                 PR            10I 0
004700070316     D  piCommitRollback...
004800070315     D                                 N   CONST
004900070622
005000070622     D*--------------------------------------------------
005100080522     D* Procedure name: FatturaManuale_out_newFattura
005200070622     D* Purpose:
005300070622     D* Returns:        Esito.
005400070622     D*--------------------------------------------------
005500080522     D FatturaManuale_out_newFattura...
005600070622     D                 PR            10I 0
005700070622     D  piOperazione...
005800070622     D                               15A   CONST
005900070730     D  piNumeroRichiesta...
006000080522     D                               10I 0 OPTIONS(*NOPASS) CONST
006100070622
006200070315     D*--------------------------------------------------
006300070611     D* Procedure name: FatturaManuale_out_endFattura
006400070315     D* Purpose:        Chiude una fattura.
006500070315     D* Returns:        Esito.
006600070608     D* Parameter:      piDoCommit => *ON = commit, *OFF = rollback, *O...
006700070315     D*                          MIT = niente.
006800070315     D*--------------------------------------------------
006900070611     D FatturaManuale_out_endFattura...
007000070315     D                 PR            10I 0
007100070608     D  piDoCommit...
007200070608     D                                 N   OPTIONS(*NOPASS:*OMIT) CONST
007300070611
007400070622     D*--------------------------------------------------
007500080522     D* Procedure name: FatturaManuale_out_newTestata
007600070622     D* Purpose:
007700070622     D* Returns:        Esito.
007800070622     D*--------------------------------------------------
007900080522     D FatturaManuale_out_newTestata...
008000070622     D                 PR            10I 0
008100080522     D  piOperazione...
008200080522     D                               15A   CONST
008300070622     D  piUtenteRichiedente...
008400080526     D                               10A   OPTIONS(*NOPASS:*OMIT)
008500070622     D  piSocietaFattura...
008600080526     D                                3A   OPTIONS(*NOPASS:*OMIT)
008700070622     D  piUnitaFattura...
008800080526     D                                8A   OPTIONS(*NOPASS:*OMIT)
008900070622     D  piAmbito...
009000080526     D                               10A   OPTIONS(*NOPASS:*OMIT)
009100070622     D  piTipoInserimentoFattura...
009200080526     D                                1A   OPTIONS(*NOPASS:*OMIT) CONST
009300070622
009400070316     D*--------------------------------------------------
009500070611     D* Procedure name: FatturaManuale_out_endTestata
009600070316     D* Purpose:        Scrive la testata.
009700070316     D* Returns:        Esito.
009800070316     D* Parameter:      controllareValori
009900070316     D*--------------------------------------------------
010000070611     D FatturaManuale_out_endTestata...
010100070316     D                 PR            10I 0
010200070618     D piAggiornareDataBase...
010300070618     D                                 N   CONST
010400070618     D piGravitaMassima...
010500070618     D                                3U 0 CONST
010600070628
010700080716     D*--------------------------------------------------
010800080716     D* Procedure name: FatturaManuale_out_newRigaDettaglio
010900080716     D* Purpose:        Inizia una nuova operazione su una riga di dettaglio
011000080716     D* Returns:        Esito.
011100080716     D* Parameter:      piOperazione => Operazione
011200080716     D* Parameter:      piIdDettaglio => ID dettaglio
011300080716     D*--------------------------------------------------
011400080716     D FatturaManuale_out_newRigaDettaglio...
011500080716     D                 PR            10I 0
011600080716     D  piOperazione                 15A   CONST
011700080716     D  piIdDettaglio                10I 0 CONST
011800080716     D                                     OPTIONS(*NOPASS:*OMIT)
011900080716
012000080716     D*--------------------------------------------------
012100080716     D* Procedure name: FatturaManuale_out_endRigaDettaglio
012200080716     D* Purpose:        Aggiorna una riga di dettaglio
012300080716     D* Returns:        Esito.
012400080716     D* Parameter:      piAggiornareDataBase => *ON = aggiornare il database.
012500080716     D* Parameter:      piGravitaMassima => Gravità massima.
012600080716     D*--------------------------------------------------
012700080716     D FatturaManuale_out_endRigaDettaglio...
012800080716     D                 PR            10I 0
012900080716     D  piAggiornareDataBase...
013000080716     D                                 N   CONST
013100080716     D  piGravitaMassima...
013200080716     D                                3U 0 CONST
013300080716
013400070628     D*--------------------------------------------------
013500070628     D* Procedure name: FatturaManuale_out_setCondizionePagamento
013600070628     D* Purpose:        Imposta la condizione pagamento della fattura.
013700070628     D* Returns:        Esito.
013800070628     D* Parameter:      piCondizionePagamento => Condizione pagamento
013900070628     D*--------------------------------------------------
014000070628     D FatturaManuale_out_setCondizionePagamento...
014100070628     D                 PR            10I 0
014200070628     D  piCondizionePagamento...
014300070628     D                                3A
014400070628
014500070316     D*--------------------------------------------------
014600070611     D* Procedure name: FatturaManuale_out_endDettaglio
014700070316     D* Purpose:        Fine dettaglio.
014800070316     D* Returns:        Esito.
014900070316     D* Parameter:      piEseguireControlli
015000070316     D*--------------------------------------------------
015100070611     D FatturaManuale_out_endDettaglio...
015200070316     D                 PR            10I 0
015300070315
015400080521     D*--------------------------------------------------
015500080521     D* Procedure name: FatturaManuale_out_setFatturaRiemessa
015600080521     D* Purpose:        Imposta il riferimento della fattura riemessa.
015700080521     D* Returns:        Esito.
015800080521     D* Parameter:      piSocieta => Società.
015900080521     D* Parameter:      piCtb => Contabilità.
016000080521     D* Parameter:      piCapoConto => Capo Conto
016100080521     D* Parameter:      piSottoConto => Sotto conto
016200080521     D* Parameter:      piDataPartita => Data partita.
016300080521     D* Parameter:      piNumeroPartita => Numero partita.
016400080521     D* Parameter:      piSeriePartita => Serie partita.
016500080521     D*--------------------------------------------------
016600080521     D FatturaManuale_out_setFatturaRiemessa...
016700080521     D                 PR            10I 0
016800080521     D  piSocieta                     3A
016900080521     D  piCtb                         2A   CONST
017000080521     D  piCapoConto                   6A
017100080521     D  piSottoConto                  8A
017200080521     D  piDataPartita                10D   DATFMT(*ISO)
017300080521     D  piNumeroPartita...
017400080521     D                                9P 0
017500080521     D piSeriePartita                 4A
017600080521
017700070315     D*--------------------------------------------------
017800070611     D* Procedure name: FatturaManuale_out_setSocieta
017900070315     D* Purpose:        Imposta l' ID società.
018000070315     D* Returns:        Esito.
018100070315     D* Parameter:      rqsSocieta
018200070315     D* Parameter:      controllaValore => Controllare il valore passato.
018300070315     D*--------------------------------------------------
018400070611     D FatturaManuale_out_setSocietaFattura...
018500070315     D                 PR            10I 0
018600070510     D  piSocietaFattura...
018700070510     D                                3A
018800070315
018900070315     D*--------------------------------------------------
019000070611     D* Procedure name: FatturaManuale_out_setIdCliente
019100070315     D* Purpose:        Imposta l' ID cliente.
019200070315     D* Returns:        Esito.
019300070315     D* Parameter:      idCliente
019400070315     D* Parameter:      controllareValore => Eseguire i controlli sul valor...
019500070315     D*                          e passato.
019600070315     D*--------------------------------------------------
019700070611     D FatturaManuale_out_setIdCliente...
019800070315     D                 PR            10I 0
019900070321     D  piIdCliente                   8A
020000080908     D  piSottoNatura                 1A   CONST OPTIONS(*NOPASS:*OMIT)
020100080908     D  piDataRiferimento...
020200080908     D                                 D   CONST OPTIONS(*NOPASS)
020300080908
020400070315     D*--------------------------------------------------
020500070629     D* Procedure name: FatturaManuale_out_getNRichiesta
020600070315     D* Purpose:        Restituisce l' ID fattura.
020700070315     D* Returns:        ID fattura.
020800070315     D*--------------------------------------------------
020900070629     D FatturaManuale_out_getNRichiesta...
021000070315     D                 PR            10I 0
021100070510
021200070510     D*--------------------------------------------------
021300070611     D* Procedure name: FatturaManuale_out_setUnitaFattura
021400070510     D* Purpose:        Imposta l'unità della fattura.
021500070510     D* Returns:        Esito.
021600070510     D* Parameter:      piUnitaFattura
021700070510     D*--------------------------------------------------
021800070611     D FatturaManuale_out_setUnitaFattura...
021900070510     D                 PR            10I 0
022000070511     D piUtente                      10A
022100070511     D piSocieta                      3A
022200070511     D piUnita                        8A
022300070704
022400070704     D*--------------------------------------------------
022500070704     D* Procedure name: FatturaManuale_out_setDescrizioneLibera
022600070704     D* Purpose:        Imposta la descrizone libera di inizio/fine fattura.
022700070704     D* Returns:        Esito.
022800070704     D* Parameter:      piDescrizioneLiberaInizio => Descrizione libera ini...
022900070704     D*                          zio fattura.
023000070704     D* Parameter:      piDescrizioneLiberaFine => Descrizione libera fine ...
023100070704     D*                          fattura.
023200070704     D*--------------------------------------------------
023300070704     D FatturaManuale_out_setDescrizioneLibera...
023400070704     D                 PR            10I 0
023500070704     D  piDescrizioneLiberaInizio...
023600081023     D                             1000A   CONST
023700070704     D                                     OPTIONS(*OMIT)
023800070704     D  piDescrizioneLiberaFine...
023900081023     D                             1000A   CONST
024000070704     D                                     OPTIONS(*OMIT)
024100070730
024200070730     D*--------------------------------------------------
024300070730     D* Procedure name: FatturaManuale_out_isFatturaModificata
024400070730     D* Purpose:        Restituisce *ON se la fattura è stata modificata o ...
024500070730     D*                          parzialmente inserita.
024600070730     D* Returns:        *ON = fattura modificata o parzialmente inserita.
024700070730     D*--------------------------------------------------
024800070730     D FatturaManuale_out_isFatturaModificata...
024900070730     D                 PR              N
025000070730
025100080314     D*--------------------------------------------------
025200080314     D* Procedure name: FatturaManuale_out_annullareRipristinare
025300080314     D* Purpose:        Annulla o ripristina una richiesta.
025400080314     D* Returns:        Esito.
025500080314     D* Parameter:      piNumeroRichiesta => Numero richiesta.
025600080314     D* Parameter:      piVisualizzareMessaggioErrore => Visualizzare il me...
025700080314     D*                          ssaggio di errore.
025800080314     D*--------------------------------------------------
025900080314     D FatturaManuale_out_annullareRipristinare...
026000080314     D                 PR            10I 0
026100080314     D  piNumeroRichiesta...
026200080314     D                               10I 0 CONST
026300080314     D  piVisualizzareMessaggioErrore...
026400080314     D                                 N   CONST
026500080314
026600080530     D*--------------------------------------------------
026700080530     D* Procedure name: FatturaManuale_out_annullareRegistrazione
026800080530     D* Purpose:        Annulla la registrazione contabile di una fattura.
026900080530     D* Returns:        Esito.
027000080530     D* Parameter:      piNumeroRichiesta => Numero richiesta.
027100080530     D*--------------------------------------------------
027200080530     D FatturaManuale_out_annullareRegistrazione...
027300080530     D                 PR            10I 0
027400080530     D  piNumeroRichiesta...
027500080530     D                               10I 0 CONST
027600080530
027700080317     D*--------------------------------------------------
027800080317     D* Procedure name: FatturaManuale_out_congelareRilasciare
027900080317     D* Purpose:        Congela/Rilascia una richiesta.
028000080317     D* Returns:        Esito.
028100080317     D* Parameter:      piNumeroRichiesta => Numero richiesta.
028200080317     D* Parameter:      piVisualizzareMessaggioErrore => *ON = Visualizzare...
028300080317     D*                           il messaggio di errore.
028400080317     D*--------------------------------------------------
028500080317     D FatturaManuale_out_congelareRilasciare...
028600080317     D                 PR            10I 0
028700080317     D  piNumeroRichiesta...
028800080317     D                               10I 0 CONST
028900080317     D  piVisualizzareMessaggioErrore...
029000080317     D                                 N   CONST
029100080317
029200080318     D*--------------------------------------------------
029300080318     D* Procedure name: FatturaManuale_out_modificareSocieta
029400080318     D* Purpose:        Cambia la società di una richiesta.
029500080318     D* Returns:        Esito.
029600080318     D* Parameter:      piNumeroRichiesta => Numero richiesta.
029700080318     D* Parameter:      piNuovaSocieta => Nuova società.
029800080318     D*--------------------------------------------------
029900080318     D FatturaManuale_out_modificareSocieta...
030000080318     D                 PR            10I 0
030100080318     D  piNumeroRichiesta...
030200080318     D                               10I 0 CONST
030300080318     D piNuovaSocieta                 3A
030400080318     D  piVisualizzareMessaggioErrore...
030500080318     D                                 N   CONST
030600080318
030700080520     D*--------------------------------------------------
030800080520     D* Procedure name: FatturaManuale_out_setModalitaSpedizione
030900080520     D* Purpose:        Imposta la modalità di spedizione della fattura.
031000080520     D* Returns:        Esito.
031100080520     D* Parameter:      piModalitaSpedizione => Modalità di spedizione.
031200080520     D*--------------------------------------------------
031300080520     D FatturaManuale_out_setModalitaSpedizione...
031400080520     D                 PR            10I 0
031500080520     D  piModalitaSpedizione...
031600080520     D                                1A
031700080520
031800080609     D*--------------------------------------------------
031900080609     D* Procedure name: FatturaManuale_out_setListaAutorizzazione
032000080609     D* Purpose:        Imposta il numero lista autorizzazione.
032100080609     D* Returns:        Esito.
032200080609     D* Parameter:      piNumeroListaAutorizzazione => Numero lista autoriz...
032300080609     D*                          zazione
032400080609     D*--------------------------------------------------
032500080609     D FatturaManuale_out_setListaAutorizzazione...
032600080609     D                 PR            10I 0
032700080609     D  piNumeroListaAutorizzazione...
032800080609     D                               10I 0 CONST
032900080609
033000080716     D*--------------------------------------------------
033100080716     D* Procedure name: FatturaManuale_out_setCodiceProdotto
033200080716     D* Purpose:        Imposta il codice prodotto.
033300080716     D* Returns:        Esito.
033400080716     D* Parameter:      piCodiceProdotto => Codice prodotto
033500080716     D*--------------------------------------------------
033600080716     D FatturaManuale_out_setCodiceProdotto...
033700080716     D                 PR            10I 0
033800080716     D  piCodiceProdotto...
033900080716     D                               10A
034000080716
034100080716     D*--------------------------------------------------
034200080716     D* Procedure name: FatturaManuale_out_getIdDettaglio
034300080716     D* Purpose:        Restituisce l'ID della riga di dettaglio su cui si ...
034400080716     D*                          sta lavorando.
034500080716     D* Returns:        ID dettaglio.
034600080716     D*--------------------------------------------------
034700080716     D FatturaManuale_out_getIdDettaglio...
034800080716     D                 PR            10I 0
034900080716
035000080716     D*--------------------------------------------------
035100080716     D* Procedure name: FatturaManuale_out_setQuantita
035200080716     D* Purpose:        Imposta la quantità.
035300080716     D* Returns:        Quantità.
035400080716     D* Parameter:      piQuantita => Quantità.
035500080716     D*--------------------------------------------------
035600080716     D FatturaManuale_out_setQuantita...
035700080716     D                 PR            10I 0
035800080922     D  piUnitaMisura                 2A
035900080922     D  piQuantita                   15P 5 CONST
036000080716
036100080716     D*--------------------------------------------------
036200080716     D* Procedure name: FatturaManuale_out_setPrezzoUnitario
036300080716     D* Purpose:        Imposta il prezzo unitario.
036400080716     D* Returns:        Esito.
036500080716     D* Parameter:      piPrezzoUnitario => Prezzo unitario.
036600080716     D*--------------------------------------------------
036700080716     D FatturaManuale_out_setPrezzoUnitario...
036800080716     D                 PR            10I 0
036900080922     D  piDivisa...
037000080922     D                                4A
037100080716     D  piPrezzoUnitario...
037200080716     D                               15P 5 CONST
037300080716
037400080716     D*--------------------------------------------------
037500080716     D* Procedure name: FatturaManuale_out_setImporto
037600080716     D* Purpose:        Imposta l'importo.
037700080716     D* Returns:        Esito.
037800080716     D* Parameter:      piImporto => Importo riga.
037900080716     D*--------------------------------------------------
038000080716     D FatturaManuale_out_setImporto...
038100080716     D                 PR            10I 0
038200080716     D  piImporto                    15P 5 CONST
038300080716
038400080716     D*--------------------------------------------------
038500080716     D* Procedure name: FatturaManuale_out_setAliquotaIva
038600080716     D* Purpose:        Imposta aliquota/riferimento IVA.
038700080716     D* Returns:        Esito.
038800081003     D* Parameter:      piAliquotaIva => Aliquota IVA.
038900081003     D* Parameter:      piRiferimentoIva => Rifarimento IVA.
039000080716     D*--------------------------------------------------
039100080716     D FatturaManuale_out_setAliquotaIva...
039200080716     D                 PR            10I 0
039300081003     D  piAliquotaIva                 5P 2 CONST
039400081003     D  piRiferimentoIva...
039500081003     D                                3A   CONST
039600080716
039700080716     D*--------------------------------------------------
039800080716     D* Procedure name: FatturaManuale_out_setUlterioreDescrizione
039900080716     D* Purpose:        Imposta l'ulteriore descrizione del prodotto.
040000080716     D* Returns:        Esito.
040100080716     D* Parameter:      piUlterioreDescrizione => Ulteriore descrizione.
040200080716     D*--------------------------------------------------
040300080716     D FatturaManuale_out_setUlterioreDescrizione...
040400080716     D                 PR            10I 0
040500080716     D  piUlterioreDescrizione...
040600080919     D                              255A   OPTIONS(*VARSIZE)
040700080919     D  piUlterioreDescrizioneLen...
040800080919     D                                3U 0 CONST
040900080716
041000080716     D*--------------------------------------------------
041100080716     D* Procedure name: FatturaManuale_out_setRiferimento
041200080716     D* Purpose:        Imposta il riferimento.
041300080716     D* Returns:        Esito.
041400080716     D* Parameter:      piRiferimento => Riferimento.
041500080716     D*--------------------------------------------------
041600080716     D FatturaManuale_out_setRiferimento...
041700080716     D                 PR            10I 0
041800080716     D  piRiferimento                20A
041900080716
042000080716     D*--------------------------------------------------
042100080716     D* Procedure name: FatturaManuale_out_setPeriodoCompetenza
042200080716     D* Purpose:        Imposta il periodo di competenza del ricavo.
042300080716     D* Returns:        Esito.
042400080716     D* Parameter:      piDataCompetenzaInizio => Data competenza iniziale.
042500080716     D* Parameter:      piDataCompetenzaFine => Data competenza finale.
042600080716     D*--------------------------------------------------
042700080716     D FatturaManuale_out_setPeriodoCompetenza...
042800080716     D                 PR            10I 0
042900080716     D  piDataCompetenzaInizio...
043000080716     D                                 D   DATFMT(*ISO)
043100080716     D  piDataCompetenzaFine...
043200080716     D                                 D   DATFMT(*ISO)
043300080716
043400080716     D*--------------------------------------------------
043500080716     D* Procedure name: FatturaManuale_out_setSequenza
043600080716     D* Purpose:        Imposta la sequenza della riga.
043700080716     D* Returns:        Esito.
043800080716     D* Parameter:      piSequenza => Sequenza riga.
043900080716     D*--------------------------------------------------
044000080716     D FatturaManuale_out_setSequenza...
044100080716     D                 PR            10I 0
044200080716     D  piSequenza                    5I 0 CONST
044300080716
044400080717     D*--------------------------------------------------
044500080717     D* Procedure name: FatturaManuale_out_setTipoInserimentoDettaglio
044600080717     D* Purpose:        Imposta il tipo inserimento riga dettaglio.
044700080717     D* Returns:        Esito.
044800080717     D* Parameter:      piTipoInserimentoDettaglio => Tipo inserimento Dettaglio.
044900080717     D*--------------------------------------------------
045000080717     D FatturaManuale_out_setTipoInserimentoDettaglio...
045100080717     D                 PR            10I 0
045200080717     D  piTipoInserimentoDettaglio...
045300080717     D                                1A   CONST
045400080717
045500080725     D*--------------------------------------------------
045600080725     D* Procedure name: FatturaManuale_out_setRiferimentoPrimaNotaBatch
045700080725     D* Purpose:        Imposta il riferimento prima nota batch.
045800080725     D* Returns:        Esito.
045900080725     D* Parameter:      piRiferimentoPrimaNotaBatch => Riferimento prima no...
046000080725     D*                          ta batch.
046100080725     D*--------------------------------------------------
046200080725     D FatturaManuale_out_setRiferimentoPrimaNotaBatch...
046300080725     D                 PR            10I 0
046400080725     D  piRiferimentoPrimaNotaBatch...
046500080725     D                                9A   CONST
046600080725
046700080725     D*--------------------------------------------------
046800080725     D* Procedure name: FatturaManuale_out_setNumeroAssolutoRegistrazione
046900080725     D* Purpose:        Imposta il numero assoluto registrazione.
047000080725     D* Returns:        Esito.
047100080725     D* Parameter:      piSistema => Sistema.
047200080725     D* Parameter:      piNumeroAssolutoRegistrazione => Numero assoluto re...
047300080725     D*                          gistrazione fattura.
047400080725     D*--------------------------------------------------
047500080725     D FatturaManuale_out_setNumeroAssolutoRegistrazione...
047600080725     D                 PR            10I 0
047700080725     D  piSistema                     3P 0 CONST
047800080725     D  piNumeroAssolutoRegistrazione...
047900080725     D                                9P 0 CONST
048000080725
048100080728     D*--------------------------------------------------
048200080728     D* Procedure name: FatturaManuale_out_addebitoSpeseInvio
048300080728     D* Purpose:        Addebita le spese di invio della fattura.
048400080728     D* Returns:        Esito.
048500080728     D*--------------------------------------------------
048600080728     D FatturaManuale_out_addebitoSpeseInvio...
048700080728     D                 PR            10I 0
048800080728
048900080728     D*--------------------------------------------------
049000080728     D* Procedure name: FatturaManuale_out_protocollareFattura
049100080728     D* Purpose:        Protocolla una fattura.
049200080728     D* Returns:        Esito.
049300080728     D* Parameter:      piNumeroRichiesta => Numero richiesta.
049400080728     D*--------------------------------------------------
049500080728     D FatturaManuale_out_protocollareFattura...
049600080728     D                 PR            10I 0
049700080728     D  piNumeroRichiesta...
049800080728     D                               10I 0 CONST
049900080728     D  piDataProtocollo...
050000080728     D                                 D   CONST
050100080807     D  piSerieRegistrazione...
050200080807     D                                4A   CONST
050300091203     D  piForzareErrori...
050400091203     D                                 N   CONST
050500091203
050600080728     D*--------------------------------------------------
050700080728     D* Procedure name: FatturaManuale_out_setBollo
050800080728     D* Purpose:        Calcola e imposta il bollo (virtuale).
050900080728     D* Returns:        Esito.
051000080728     D*--------------------------------------------------
051100080728     D FatturaManuale_out_setBollo...
051200080728     D                 PR            10I 0
051300080728     D  piNumeroRichiesta...
051400080728     D                               10I 0 CONST
051500080729
051600080729     D*--------------------------------------------------
051700080729     D* Procedure name: FatturaManuale_out_setContoVoce
051800080729     D* Purpose:        Imposta i conti e le voci.
051900080729     D* Returns:        Esito.
052000080729     D* Parameter:      piTipoConto => 'N' = Normale; 'E' = fatture da Emet...
052100080729     D*                          tere; 'S' = Sopravvenienze.
052200080729     D* Parameter:      piCapoConto => Capo conto.
052300080729     D* Parameter:      piSottoConto => Sotto conto.
052400080729     D* Parameter:      piVoce => Voce.
052500080729     D*--------------------------------------------------
052600080729     D FatturaManuale_out_setContoVoce...
052700080729     D                 PR            10I 0
052800080729     D  piTipoConto                   1A   CONST
052900080729     D  piCapoConto                   6A
053000080729     D  piSottoConto                  8A
053100080729     D  piVoce                        8A   OPTIONS(*NOPASS) CONST
053200080729
053300080729     D*--------------------------------------------------
053400080729     D* Procedure name: FatturaManuale_out_setEntita
053500080729     D* Purpose:        Imposta le entità di analitica.
053600080729     D* Returns:        Esito.
053700080729     D* Parameter:      piEntitaNumero => Numero entità (1-8).
053800080729     D* Parameter:      piEntitaTipo => Tipo entità.
053900080729     D* Parameter:      piEntitaCodice => Codice entità.
054000080729     D*--------------------------------------------------
054100080729     D FatturaManuale_out_setEntita...
054200080729     D                 PR            10I 0
054300080729     D piEntitaNumero                 3U 0 CONST
054400080729     D  piEntitaTipo                  3A
054500080729     D piEntitaCodice                 8A
054600080729
054700080923     D*--------------------------------------------------
054800080923     D* Procedure name: FatturaManuale_out_setRegolaRipartizioneValori
054900080923     D* Purpose:        Imposta la regola ripartizione valori.
055000080923     D* Returns:        Esito.
055100080923     D* Parameter:      piRegolaRipartizioneValori => Regola ripartizione v...
055200080923     D*                          alori co.an.
055300080923     D*--------------------------------------------------
055400080923     D FatturaManuale_out_setRegolaRipartizioneValori...
055500080923     D                 PR            10I 0
055600080923     D  piRegolaRipartizioneValori...
055700080923     D                                6A   CONST
055800080923
055900080804     D*--------------------------------------------------
056000080804     D* Procedure name: FatturaManuale_out_autorizzare
056100080804     D* Purpose:        Autorizza una richiesta.
056200080804     D* Returns:        Esito.
056300080804     D* Parameter:      piNumeroRichiesta => Numero richiesta.
056400080804     D*--------------------------------------------------
056500080804     D FatturaManuale_out_autorizzare...
056600080804     D                 PR            10I 0
056700080804     D  piNumeroRichiesta...
056800080804     D                               10I 0 CONST
056900080910
057000080910     D*--------------------------------------------------
057100080910     D* Procedure name: FatturaManuale_out_annullareRipristinareDettaglio
057200080910     D* Purpose:        Annulla o ripristina una riga di dettaglio.
057300080910     D* Returns:        Esito.
057400080910     D* Parameter:      piIdDettaglio => ID dettaglio.
057500080910     D*--------------------------------------------------
057600080910     D FatturaManuale_out_annullareRipristinareDettaglio...
057700080910     D                 PR            10I 0
057800080910     D  piIdDettaglio...
057900080910     D                               10I 0 CONST
058000081010
058100090401     D*--------------------------------------------------
058200090401     D* Procedure name: FatturaManuale_out_getOperazioneFattura
058300090401     D* Purpose:        Restituisce l'operazione in corso sulla fattura.
058400090401     D* Returns:        Operazione fattura.
058500090401     D*--------------------------------------------------
058600090401     D FatturaManuale_out_getOperazioneFattura...
058700090401     D                 PR            15A
058800090401
058801160726       // --------------------------------------------------
058802160726       // Prototype for procedure: FatturaManuale_out_undo
058803160726       // --------------------------------------------------
058804160726
058805160726       DCL-PR FatturaManuale_out_undo INT(10);
058806160726         piNumeroRichiesta INT(10) CONST;
058807160726       END-PR ;
058808160726
