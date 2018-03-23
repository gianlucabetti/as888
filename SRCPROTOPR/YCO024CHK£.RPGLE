000100070622     D TIPOINSERIMENTO_QUALSIASI...
000200070622     D                 C                   '*'
000300070622     D TIPOINSERIMENTO_MANUALE...
000400070622     D                 C                   'M'
000500070622     D TIPOINSERIMENTO_AUTOMATICO...
000600070622     D                 C                   'A'
000700070622     D UFFICIO_FATTURAZIONE...
000800070622     D                 C                   'F'
000900070622
001000080609     D*--------------------------------------------------
001100080609     D* Procedure name: FatturaManuale_chk_init
001200080609     D* Purpose:        Inizializza il modulo.
001300080609     D* Returns:        Esito.
001400080609     D* Parameter:      piKpjba
001500080609     D*--------------------------------------------------
001600080609     D FatturaManuale_chk_init...
001700080609     D                 PR            10I 0
001800080609     D  piKpjba                            LIKEDS(kpjba)
001900080609     D                                     OPTIONS(*NOPASS)
002000080609
002100080609     D*--------------------------------------------------
002200080609     D* Procedure name: FatturaManuale_chk_finalize
002300080609     D* Purpose:        Chiude il modulo.
002400080609     D* Returns:
002500080609     D*--------------------------------------------------
002600080609     D FatturaManuale_chk_finalize...
002700080609     D                 PR
002800080609
002900070509     D*--------------------------------------------------
003000070611     D* Procedure name: FatturaManuale_chk_chkAutOperazione
003100070608     D* Purpose:        Controllo autorizzazione all'azione.
003200070509     D* Returns:        Esito.
003300070509     D* Parameter:      piSocieta => Società.
003400070509     D* Parameter:      piUnita => Unità.
003500070509     D* Parameter:      piGruppo => Gruppo utenti Proj
003600070510     D* Parameter:      piUtente => Profilo utente.
003700070611     D* Parameter:      piOperazione => Operazione.
003800070509     D*--------------------------------------------------
003900070611     D FatturaManuale_chk_chkAutOperazione...
004000070509     D                 PR            10I 0
004100070509     D  piSocieta                     3A
004200070509     D  piUnita                       8A
004300070510     D  piGruppo                      3P 0 CONST
004400070510     D  piUtente                     10A
004500070611     D  piOperazione                 15A   CONST
004600070510
004700070510     D*--------------------------------------------------
004800070511     D* Procedure name: FatturaManuale_chk_chkAutAmbito
004900070510     D* Purpose:        Controllo autorizzazione all'ambito.
005000070510     D* Returns:        Esito.
005100070510     D* Parameter:      piSocieta => Società.
005200070510     D* Parameter:      piUnita => Unità.
005300070510     D* Parameter:      piGruppo => Gruppo utenti Proj
005400070510     D* Parameter:      piUtente => Profilo utente.
005500070510     D* Parameter:      piAmbito => Ambito.
005600070510     D*--------------------------------------------------
005700070511     D FatturaManuale_chk_chkAutAmbito...
005800070510     D                 PR            10I 0
005900070510     D  piSocieta                     3A
006000070510     D  piUnita                       8A
006100070510     D  piGruppo                      3P 0 CONST
006200070510     D  piUtente                     10A
006300070510     D  piAmbito                     10A   CONST
006400070516
006500070516     D*--------------------------------------------------
006600070516     D* Procedure name: FatturaManuale_chk_chkAutCliente
006700070516     D* Purpose:        Controllo autorizzazione al cliente.
006800070516     D* Returns:        Esito.
006900070516     D*--------------------------------------------------
007000070516     D FatturaManuale_chk_chkAutCliente...
007100070516     D                 PR            10I 0
007200070516
007300070515     D*--------------------------------------------------
007400070515     D* Procedure name: FatturaManuale_chk_chkAutSocieta
007500070515     D* Purpose:        Controllo autorizzazione alla società.
007600070515     D* Returns:        Esito.
007700070515     D* Parameter:      piUtente => Profilo utente.
007800070515     D* Parameter:      piSocieta => Società.
007900070515     D* Parameter:      piSistemaInformativo => Sistema informativo.
008000070515     D*--------------------------------------------------
008100070515     D FatturaManuale_chk_chkAutSocieta...
008200070515     D                 PR            10I 0
008300070515     D  piUtente                     10A
008400070515     D  piSocieta                     3A
008500070515     D  piSistemaInformativo...
008600070515     D                               10A
008700070511
008800070510     D*--------------------------------------------------
008900070511     D* Procedure name: FatturaManuale_chk_chkAutUnita
009000070510     D* Purpose:        Controlla autorizzazione all'unità.
009100070510     D* Returns:        Esito.
009200070510     D* Parameter:      piUnitaFattura => Unità fattura.
009300070510     D*--------------------------------------------------
009400070511     D FatturaManuale_chk_chkAutUnita...
009500070510     D                 PR            10I 0
009600070510     D piUtente                      10A
009700070510     D piSocieta                      3A
009800070510     D piUnita                        8A
009900070516
010000070516     D*--------------------------------------------------
010100070516     D* Procedure name: FatturaManuale_chk_chkAmbito
010200070516     D* Purpose:        Controlla l'esistenza e la validità dell'ambito
010300070516     D* Returns:        Esito.
010400070516     D* Parameter:      piCdAmbito => Codice ambito
010500070516     D* Parameter:      piDataControlloValidita => Data riferimento per con...
010600070516     D*                          trollo validità.
010700070516     D*--------------------------------------------------
010800070516     D FatturaManuale_chk_chkAmbito...
010900070516     D                 PR            10I 0
011000070516     D  piCdAmbito                   10A
011100070516     D  piDataControlloValidita...
011200070516     D                               10D   DATFMT(*ISO)
011300070516     D                                     CONST OPTIONS(*NOPASS:*OMIT)
011400070626
011500070626     D*--------------------------------------------------
011600070626     D* Procedure name: FatturaManuale_chk_chkAmbitoSocietaImposta
011700070626     D* Purpose:        Controlla che la società della fattura sia uguale alla
011800070626     D*                          società imposta dell'ambito.
011900070626     D* Returns:        Esito.
012000070626     D*--------------------------------------------------
012100070626     D FatturaManuale_chk_chkAmbitoSocietaImposta...
012200070626     D                 PR            10I 0
012300070626     D  piSocieta                     3A
012400070626
012500070626     D*--------------------------------------------------
012600070626     D* Procedure name: FatturaManuale_chk_chkAmbitoUnitaImposta
012700070626     D* Purpose:        Controlla che l'unità della fattura sia uguale all'...
012800070626     D*                          unità imposta dell'ambito.
012900070626     D* Returns:        Esito.
013000070626     D*--------------------------------------------------
013100070626     D FatturaManuale_chk_chkAmbitoUnitaImposta...
013200070626     D                 PR            10I 0
013300070626     D  piUnita                       8A
013400070626
013500070516     D*--------------------------------------------------
013600070516     D* Procedure name: FatturaManuale_chk_chkCliente
013700070516     D* Purpose:        Controlli su ID cliente.
013800070516     D* Returns:        Esito.
013900080529     D* Parameter:      piSocieta => Società.
014000080529     D* Parameter:      piKscCliente => Cliente.
014100080529     D* Parameter:      piDataRiferimento => Data riferimento per controlli.
014200080529     D* Parameter:      piSottoNatura => 'C' = Cliente, 'F' = Fornitore.
014300070516     D*--------------------------------------------------
014400070516     D FatturaManuale_chk_chkCliente...
014500070516     D                 PR            10I 0
014600070516     D  piSocieta                     3A
014700070516     D  piKscCliente                  8A
014800070516     D  piDataRiferimento...
014900070516     D                                 D   CONST
015000080529     D  piSottoNatura...
015100080529     D                                1A   CONST OPTIONS(*NOPASS)
015200070517
015300070517     D*--------------------------------------------------
015400070517     D* Procedure name: FatturaManuale_chk_chkClienteCodificato
015500070517     D* Purpose:        Controlla cliente codificato.
015600070517     D* Returns:        Esito.
015700070517     D* Parameter:      piSocieta => Società.
015800070517     D* Parameter:      piKscCliente
015900070517     D*--------------------------------------------------
016000070517     D FatturaManuale_chk_chkClienteCodificato...
016100070517     D                 PR            10I 0
016200070517     D  piSocieta                     3A
016300070517     D  piKscCliente                  8A
016400070517
016500070517     D*--------------------------------------------------
016600070517     D* Procedure name: FatturaManuale_chk_chkClienteObbligatorio
016700070517     D* Purpose:        Controlla obbligatorietà codice cliente.
016800070517     D* Returns:        Esito.
016900070517     D* Parameter:      piKscCliente => Codice cliente.
017000070517     D*--------------------------------------------------
017100070517     D FatturaManuale_chk_chkClienteObbligatorio...
017200070517     D                 PR            10I 0
017300070517     D  piKscCliente                  8A
017400070628
017500070628     D*--------------------------------------------------
017600070628     D* Procedure name: FatturaManuale_chk_chkCondizionePagamento
017700070628     D* Purpose:        Controlla la condizione di pagamento
017800070628     D* Returns:        Esito.
017900070628     D* Parameter:      piCondizionePagamento
018000070628     D*--------------------------------------------------
018100070628     D FatturaManuale_chk_chkCondizionePagamento...
018200070628     D                 PR            10I 0
018300070628     D  piCondizionePagamento...
018400070628     D                                3A
018500070628
018600070517     D*--------------------------------------------------
018700070517     D* Procedure name: FatturaManuale_chk_chkFatturaRiemessa
018800070517     D* Purpose:        Controllo esistenza riferimento fattura riemessa.
018900070517     D* Returns:        Esito.
019000070517     D* Parameter:      piSocieta => Società.
019100070517     D* Parameter:      piCtb => Contabilità.
019200070517     D* Parameter:      piCapoConto => Capo Conto
019300070517     D* Parameter:      piSottoConto => Sotto conto
019400070517     D* Parameter:      piDataPartita => Data partita.
019500070517     D* Parameter:      piNumeroPartita => Numero partita.
019600070517     D* Parameter:      piSeriePartita => Serie partita.
019700070517     D*--------------------------------------------------
019800070517     D FatturaManuale_chk_chkFatturaRiemessa...
019900070517     D                 PR            10I 0
020000070517     D  piSocieta                     3A
020100070517     D  piCtb                         2A   CONST
020200070517     D  piCapoConto                   6A
020300070517     D  piSottoConto                  8A
020400070517     D  piDataPartita                10D   DATFMT(*ISO)
020500070517     D  piNumeroPartita...
020600070517     D                                9P 0
020700070517     D piSeriePartita                 4A
020800070517
020900080520     D*--------------------------------------------------
021000080520     D* Procedure name: FatturaManuale_chk_chkModalitaSpedizione
021100080520     D* Purpose:        Controlla la modalità di spedizione della fattura.
021200080520     D* Returns:        Esito.
021300080520     D* Parameter:      piModalitaSpedizione => Modalità di spedizione
021400080520     D*--------------------------------------------------
021500080520     D FatturaManuale_chk_chkModalitaSpedizione...
021600080520     D                 PR            10I 0
021700080520     D  piModalitaSpedizione...
021800080520     D                                1A
021900080520
022000070517     D*--------------------------------------------------
022100070611     D* Procedure name: FatturaManuale_chk_chkOperazioneDettaglio
022200070608     D* Purpose:        Controllo azione riga dettaglio.
022300070517     D* Returns:        Esito.
022400070611     D* Parameter:      piOperazione => Operazione
022500070517     D*--------------------------------------------------
022600070611     D FatturaManuale_chk_chkOperazioneDettaglio...
022700070517     D                 PR            10I 0
022800070611     D  piOperazione                 15A   CONST
022900070516
023000070516     D*--------------------------------------------------
023100070611     D* Procedure name: FatturaManuale_chk_chkOperazioneTestata
023200070608     D* Purpose:        Controllo azione testata fattura.
023300070516     D* Returns:        Esito.
023400070611     D* Parameter:      piOperazione => Operazione
023500070516     D*--------------------------------------------------
023600070611     D FatturaManuale_chk_chkOperazioneTestata...
023700070516     D                 PR            10I 0
023800070611     D  piOperazione                 15A   CONST
023900080528     D  piNrRichiesta                10I 0 CONST
024000080528
024100070515     D*--------------------------------------------------
024200070613     D* Procedure name: FatturaManuale_chk_chkSocietaFattura
024300070613     D* Purpose:        Controlli su società fattura.
024400070515     D* Returns:        Esito.
024500070613     D* Parameter:      piSocieta
024600070515     D*--------------------------------------------------
024700070613     D FatturaManuale_chk_chkSocietaFattura...
024800070515     D                 PR            10I 0
024900070614     D  piSocietaFattura...
025000070515     D                                3A
025100070613
025200070613     D*--------------------------------------------------
025300070613     D* Procedure name: FatturaManuale_chk_chkSocietaLavoro
025400070613     D* Purpose:        Controlli su società lavoro.
025500070613     D* Returns:        Esito.
025600070613     D* Parameter:      piSocieta
025700070613     D*--------------------------------------------------
025800070613     D FatturaManuale_chk_chkSocietaLavoro...
025900070613     D                 PR            10I 0
026000070614     D  piSocietaLavoro...
026100070613     D                                3A
026200070613
026300070517     D*--------------------------------------------------
026400070517     D* Procedure name: FatturaManuale_chk_chkTipoInserimentoFattura
026500070517     D* Purpose:        Controllo la congruenza del tipo inserimento fattur...
026600070517     D*                          a-ambito.
026700070517     D* Returns:        Esito.
026800070622     D* Parameter:      piTipoInserimentoFattura => Tipo inserimento.
026900070517     D*--------------------------------------------------
027000070517     D FatturaManuale_chk_chkTipoInserimentoFattura...
027100070517     D                 PR            10I 0
027200070622     D  piTipoInserimentoFattura...
027300070622     D                                1A   CONST
027400070613
027500070613     D*--------------------------------------------------
027600070613     D* Procedure name: FatturaManuale_chk_chkUfficioRichiedente
027700070613     D* Purpose:        Controlla ufficio richiedente.
027800070613     D* Returns:        Esito.
027900070613     D* Parameter:      PiUfficio => Ufficio.
028000070613     D*--------------------------------------------------
028100070613     D FatturaManuale_chk_chkUfficioRichiedente...
028200070613     D                 PR            10I 0
028300070613     D  piUfficio                     2A
028400070613
028500070613     D*--------------------------------------------------
028600070614     D* Procedure name: FatturaManuale_chk_chkUfficioLavoro
028700070613     D* Purpose:        Controlla ufficio richiedente.
028800070613     D* Returns:        Esito.
028900070613     D* Parameter:      PiUfficio => Ufficio.
029000070613     D*--------------------------------------------------
029100070614     D FatturaManuale_chk_chkUfficioLavoro...
029200070613     D                 PR            10I 0
029300070613     D  piUfficio                     2A
029400070613
029500070510     D*--------------------------------------------------
029600070511     D* Procedure name: FatturaManuale_chk_chkUnita
029700070510     D* Purpose:        Controllo unità.
029800070510     D* Returns:        Esito.
029900070510     D* Parameter:      piUnita => Unità.
030000070510     D*--------------------------------------------------
030100070511     D FatturaManuale_chk_chkUnita...
030200070510     D                 PR            10I 0
030300070511     D  piSocieta                     3A
030400070511     D  piUnita                       8A
030500070517
030600070517     D*--------------------------------------------------
030700070517     D* Procedure name: FatturaManuale_chk_chkUnitaAmbitoCliente
030800070517     D* Purpose:        Controllo unità ambito-cliente
030900070517     D* Returns:        Esito.
031000070517     D*--------------------------------------------------
031100070517     D FatturaManuale_chk_chkUnitaAmbitoCliente...
031200070517     D                 PR            10I 0
031300070613
031400070614     P*--------------------------------------------------
031500070614     P* Procedure name: FatturaManuale_chk_chkUtenteLavoro
031600070614     P* Purpose:        Controlla utente lavoro.
031700070614     P* Returns:        Esito.
031800070614     P* Parameter:      piUtente => Utente lavoro.
031900070614     P*--------------------------------------------------
032000070614     D FatturaManuale_chk_chkUtenteLavoro...
032100070614     D                 PR            10I 0
032200070614     D  piUtenteLavoro...
032300070614     D                               10A
032400070614
032500070614     P*--------------------------------------------------
032600070614     P* Procedure name: FatturaManuale_chk_chkUtenteRichiedente
032700070614     P* Purpose:        Controlla utente richiedente.
032800070614     P* Returns:        Esito.
032900070614     P* Parameter:      piUtente => Utente richiedente.
033000070614     P*--------------------------------------------------
033100070614     D FatturaManuale_chk_chkUtenteRichiedente...
033200070614     D                 PR            10I 0
033300070614     D  piUtenteRichiedente...
033400070614     D                               10A
033500070628
033600080523     D*--------------------------------------------------
033700080523     D* Procedure name: FatturaManuale_chk_getAnagraficaAmbito
033800080523     D* Purpose:        Restituisce l'anagrafica dell'ambito.
033900080523     D* Returns:
034000080523     D*--------------------------------------------------
034100080523     D FatturaManuale_chk_getAnagraficaAmbito...
034200080523     D                 PR                  LIKEDS(ynfmaa00f)
034300080523
034400070628     D*--------------------------------------------------
034500070628     D* Procedure name: FatturaManuale_chk_getAnagraficaCliente
034600070628     D* Purpose:        Restituisce l'anagrafica cliente Proj.
034700070628     D* Returns:        Anagrafica cliente Proj.
034800070628     D*--------------------------------------------------
034900070628     D FatturaManuale_chk_getAnagraficaCliente...
035000070628     D                 PR                  LIKEDS(dvaCli)
035100080805
035200080805     D*--------------------------------------------------
035300080805     D* Procedure name: FatturaManuale_chk_getAnagraficaClienteFatturazione
035400080805     D* Purpose:        Restituisce l'anagrafica del cliente di fatturazione.
035500080805     D* Returns:        Anagrafica cliente di fatturazione.
035600080805     D*--------------------------------------------------
035700080805     D FatturaManuale_chk_getAnagraficaClienteFatturazione...
035800080805     D                 PR                  LIKEDS(dvaCli)
035900080805
036000080529     D*--------------------------------------------------
036100080529     D* Procedure name: FatturaManuale_chk_getAnagraficaFornitore
036200080529     D* Purpose:        Restituisce l'anagrafica fornitore Proj.
036300080529     D* Returns:        Anagrafica cliente Proj.
036400080529     D*--------------------------------------------------
036500080529     D FatturaManuale_chk_getAnagraficaFornitore...
036600080529     D                 PR                  LIKEDS(dvaFor)
036700070628
036800070516     D*--------------------------------------------------
036900070516     D* Procedure name: FatturaManuale_chk_getDescrizioneAmbito
037000070516     D* Purpose:        Restituisce la descrizione dell'ambito.
037100070516     D* Returns:        Descrizione ambito.
037200070516     D*--------------------------------------------------
037300070516     D FatturaManuale_chk_getDescrizioneAmbito...
037400070516     D                 PR            50A
037500070628
037600070628     D*--------------------------------------------------
037700070628     D* Procedure name: FatturaManuale_chk_getDescrizioneCondizionePagamento
037800070628     D* Purpose:        Restituisce la descrizione condizione pagamento
037900070628     D* Returns:        Descrizione condizione pagamento.
038000070628     D*--------------------------------------------------
038100070628     D FatturaManuale_chk_getDescrizioneCondizionePagamento...
038200070628     D                 PR            44A
038300070628
038400070628     D*--------------------------------------------------
038500070628     D* Procedure name: FatturaManuale_chk_getDescrizioneFilialeRichiedente
038600070628     D* Purpose:        Restituisce la descrizione della filiale richiedente.
038700070628     D* Returns:        Descrizione filiale richiedente.
038800070628     D*--------------------------------------------------
038900070628     D FatturaManuale_chk_getDescrizioneFilialeRichiedente...
039000070628     D                 PR            20A
039100070628
039200080520     D*--------------------------------------------------
039300080520     D* Procedure name: FatturaManuale_chk_getDescrizioneModalitaSpedizione
039400080520     D* Purpose:        Restituisce la descrizione della modalità di spediz...
039500080520     D*                          ione della fattura.
039600080520     D* Returns:        Descrizione modalità spedizione fattura.
039700080520     D*--------------------------------------------------
039800080520     D FatturaManuale_chk_getDescrizioneModalitaSpedizione...
039900080520     D                 PR            30A
040000080520
040100070614     D*--------------------------------------------------
040200070614     D* Procedure name: FatturaManuale_chk_getDescrizioneUfficioRichiedente
040300070614     D* Purpose:        Restituisce la descrizione dell'ufficio richiedente.
040400070614     D* Returns:        Descrizione ufficio richiedente.
040500070614     D*--------------------------------------------------
040600070614     D FatturaManuale_chk_getDescrizioneUfficioRichiedente...
040700070614     D                 PR            30A
040800070614
040900070511     D*--------------------------------------------------
041000070511     D* Procedure name: FatturaManuale_chk_getDescrizioneUnita
041100070511     D* Purpose:        Restituisce la descrizione dell'unità.
041200070511     D* Returns:        Descrizione unità.
041300070511     D*--------------------------------------------------
041400070511     D FatturaManuale_chk_getDescrizioneUnita...
041500070511     D                 PR            20A
041600070613
041700070613     D*--------------------------------------------------
041800070614     D* Procedure name: FatturaManuale_chk_getDescrizioneUtenteRichiedente
041900070613     D* Purpose:        Restituisce la descrizione dell'utente.
042000070613     D* Returns:        Descrizione utente.
042100070613     D*--------------------------------------------------
042200070614     D FatturaManuale_chk_getDescrizioneUtenteRichiedente...
042300070613     D                 PR            50A
042400070614
042500070614     D*--------------------------------------------------
042600070614     D* Procedure name: FatturaManuale_chk_getUfficioRichiedente
042700070614     D* Purpose:        Restituisce l'ufficio dell'utente richiedente.
042800070614     D* Returns:        Ufficio utente richiedente.
042900070614     D*--------------------------------------------------
043000070614     D FatturaManuale_chk_getUfficioRichiedente...
043100070614     D                 PR             2A
043200070614
043300070614     D*--------------------------------------------------
043400070614     D* Procedure name: FatturaManuale_chk_getFilialeRichiedente
043500070614     D* Purpose:        Restituisce la filiale dell'utente richiedente.
043600070614     D* Returns:        Filiale utente richiedente.
043700070614     D*--------------------------------------------------
043800070614     D FatturaManuale_chk_getFilialeRichiedente...
043900070614     D                 PR             3P 0
044000070614
044100070516     D*--------------------------------------------------
044200070516     D* Procedure name: FatturaManuale_chk_getRagioneSocialeCliente
044300070516     D* Purpose:        Restituisce la descrizione del cliente.
044400070516     D* Returns:        Ragione sociale cliente.
044500070516     D*--------------------------------------------------
044600070516     D FatturaManuale_chk_getRagioneSocialeCliente...
044700070516     D                 PR            44A
044800070517
044900070517     D*--------------------------------------------------
045000070517     D* Procedure name: FatturaManuale_chk_getUnitaImposta
045100070517     D* Purpose:        Restituisce l'unità imposta per l'ambito.
045200070517     D* Returns:
045300070517     D*--------------------------------------------------
045400070517     D FatturaManuale_chk_getUnitaImposta...
045500070517     D                 PR             8A
045600070703
045700070703     D*--------------------------------------------------
045800070703     D* Procedure name: FatturaManuale_chk_getTestoInizioFattura
045900070703     D* Purpose:        Restituisce il codice testo da stampare all'inizio ...
046000070703     D*                          della fattura.
046100070703     D* Returns:        Codice testo inizio fattura.
046200070703     D*--------------------------------------------------
046300070703     D FatturaManuale_chk_getTestoInizioFattura...
046400070703     D                 PR             2A
046500070703
046600070703     D*--------------------------------------------------
046700070703     D* Procedure name: FatturaManuale_chk_getTestoFineFattura
046800070703     D* Purpose:        Restituisce il codice testo da stampare alla fine ...
046900070703     D*                          della fattura.
047000070703     D* Returns:        Codice testo inizio fattura.
047100070703     D*--------------------------------------------------
047200070703     D FatturaManuale_chk_getTestoFineFattura...
047300070703     D                 PR             2A
047400070703
047500070517     D*--------------------------------------------------
047600070517     D* Procedure name: FatturaManuale_chk_isClienteObbligatorio
047700070517     D* Purpose:        Restituisce *ON se il clente è obbligatorio.
047800070517     D* Returns:        *ON/*OFF
047900070517     D*--------------------------------------------------
048000070517     D FatturaManuale_chk_isClienteObbligatorio...
048100070517     D                 PR              N
048200070702
048300070702     D*--------------------------------------------------
048400070702     D* Procedure name: FatturaManuale_chk_isCommentoFatturaObbligatorio
048500070702     D* Purpose:        Restituisce *ON se il commento fattura è obbligatorio.
048600070702     D* Returns:        *ON = il commento fattura è obbligatorio.
048700070702     D*--------------------------------------------------
048800070702     D FatturaManuale_chk_isCommentoFatturaObbligatorio...
048900070702     D                 PR              N
049000070702
049100070517     D*--------------------------------------------------
049200070517     D* Procedure name: FatturaManuale_chk_isRiemissioneFattura
049300070517     D* Purpose:        Restituisce *ON se è una riemissione di una fattura...
049400070517     D*                           stornata.
049500070517     D* Returns:        *ON = E' una riemissione di fattura stornata.
049600070517     D*--------------------------------------------------
049700070517     D FatturaManuale_chk_isRiemissioneFattura...
049800070517     D                 PR              N
049900070614
050000070614     P*--------------------------------------------------
050100070614     P* Procedure name: FatturaManuale_chk_isUtenteLavoroUfficioFatturazione
050200070614     P* Purpose:        Restituisce *ON se l'utente lavoro appartiene all'ufficio fatturazione
050300070614     P* Returns:        *ON = ufficio fatturazione.
050400070614     P*--------------------------------------------------
050500070614     D FatturaManuale_chk_isUtenteLavoroUfficioFatturazione...
050600070614     D                 PR              N
050700070614
050800070614     D*--------------------------------------------------
050900070614     D* Procedure name: FatturaManuale_chk_setKpjba
051000070614     D* Purpose:        Imposta KPJBA.
051100070614     D* Returns:
051200070614     D* Parameter:      piKpjba
051300070614     D*--------------------------------------------------
051400070614     D FatturaManuale_chk_setKpjba...
051500070614     D                 PR
051600070614     D  piKpjba                            LIKEDS(kpjba)
051700070614
051800080716     D*--------------------------------------------------
051900080716     D* Procedure name: FatturaManuale_chk_chkCodiceProdotto
052000080716     D* Purpose:        Controlla il codice prodotto.
052100080716     D* Returns:        Esito.
052200080716     D* Parameter:      piCodiceProdotto => Codice prodotto.
052300080716     D* Parameter:      piDataValidita => Data validità prodotto
052400080717     D* Parameter:      piAmbito => Ambito di fatturazione
052500080716     D*--------------------------------------------------
052600080716     D FatturaManuale_chk_chkCodiceProdotto...
052700080716     D                 PR            10I 0
052800080716     D  piCodiceProdotto...
052900080716     D                               10A
053000080716     D piDataValidita                  D   DATFMT(*ISO)
053100080716     D                                     CONST
053200080717     D  piAmbito...
053300080717     D                               10A
053400080716
053500080716     D*--------------------------------------------------
053600080716     D* Procedure name: FatturaManuale_chk_getAnagraficaProdotto
053700080716     D* Purpose:        Restituisce l'anagrafica del prodotto.
053800080716     D* Returns:        Anagrafica del prodotto.
053900080716     D*--------------------------------------------------
054000080716     D FatturaManuale_chk_getAnagraficaProdotto...
054100080716     D                 PR                  LIKEDS(ynfmap00f)
054200080716
054300080717     D*--------------------------------------------------
054400080717     D* Procedure name: FatturaManuale_chk_chkTipoInserimentoDettaglio
054500080717     D* Purpose:        Controlla il tipo inserimento riga dettaglio.
054600080717     D* Returns:        Esito.
054700080717     D* Parameter:      piTipoInserimentoDettaglio => Tipo inserimento.
054800080717     D*--------------------------------------------------
054900080717     D FatturaManuale_chk_chkTipoInserimentoDettaglio...
055000080717     D                 PR            10I 0
055100080717     D  piTipoInserimentoDettaglio...
055200080717     D                                1A   CONST
055300080717
055400080731     D*--------------------------------------------------
055500080731     D* Procedure name: FatturaManuale_chk_chkDataRegistrazione
055600080731     D* Purpose:        Controlla la data registrazione.
055700080731     D* Returns:        Esito.
055800080731     D* Parameter:      piSocieta => Società
055900080731     D* Parameter:      piSerie => Serie registrazione.
056000080731     D* Parameter:      piDataRegistrazione => Data registrazione.
056100080731     D*--------------------------------------------------
056200080731     D FatturaManuale_chk_chkDataRegistrazione...
056300080731     D                 PR            10I 0
056400080731     D  piSocieta                     3A
056500080731     D  piSerie                       4A
056600080731     D  piDataRegistrazione...
056700080731     D                                 D   DATFMT(*ISO)
056800080731
056900080828     D*--------------------------------------------------
057000080828     D* Procedure name: FatturaManuale_chk_chkContoVoce
057100080828     D* Purpose:        Imposta i conti e le voci.
057200080828     D* Returns:        Esito.
057300080828     D* Parameter:      piTipoConto => 'N' = Normale; 'E' = fatture da Emet...
057400080828     D*                          tere; 'S' = Sopravvenienze.
057500080828     D* Parameter:      piCapoConto => Capo conto.
057600080828     D* Parameter:      piSottoConto => Sotto conto.
057700080828     D* Parameter:      piVoce => Voce.
057800080828     D*--------------------------------------------------
057900080828     D FatturaManuale_chk_chkContoVoce...
058000080828     D                 PR            10I 0
058100080828     D  piSocieta                     3A
058200080828     D  piTipoConto                   1A   CONST
058300080828     D  piCapoConto                   6A
058400080829     D  piSottoConto                  8A   VALUE
058500081104     D  piVoce                        8A   OPTIONS(*NOPASS:*OMIT) CONST
058600081002     D  piFatturaInContoEconomico...
058700081002     D                                1A   OPTIONS(*NOPASS) CONST
058800081002     D  piDataCompetenza...
058900081002     D                                 D   OPTIONS(*NOPASS) CONST
059000081002
059100080829     D*--------------------------------------------------
059200080829     D* Procedure name: FatturaManuale_chk_getAnagraficaConto
059300080829     D* Purpose:        Restituisce l'anagrafica del conto.
059400080829     D* Returns:        Anagrafica conto contabile.
059500080829     D* Parameter:      piTipoConto => Tipo conto: 'N' = Normale, 'E' = fat...
059600080829     D*                          ture da Emettere, 'S' = Sopravvenienze.
059700080829     D*--------------------------------------------------
059800080829     D FatturaManuale_chk_getAnagraficaConto...
059900080829     D                 PR                  LIKEDS(nd002ds)
060000080829     D  piTipoConto                   1A   CONST
060100080829     D piNd002ds_kcc...
060200080829     D                                     LIKEDS(nd002ds) OPTIONS(*NOPASS)
060300080901
060400080901     D*--------------------------------------------------
060500080901     D* Procedure name: FatturaManuale_chk_getAnagraficaVoce
060600080901     D* Purpose:        Restituisce l'anagrafica della voce.
060700080901     D* Returns:        Anagrafica conto contabile.
060800080901     D* Parameter:      piTipoConto => Tipo conto: 'N' = Normale,
060900080901     D*                                            'S' = Sopravvenienze.
061000080901     D*--------------------------------------------------
061100080901     D FatturaManuale_chk_getAnagraficaVoce...
061200080901     D                 PR                  LIKEDS(anvci00f)
061300080901     D  piTipoConto                   1A   CONST
061400080829
061500080908     D*--------------------------------------------------
061600080908     D* Procedure name: FatturaManuale_chk_getDichiarazioneIntento
061700080908     D* Purpose:        Restituisce il riferimento dichiarazione intento.
061800080908     D* Returns:        Riferimento dichiarazione intento.
061900080908     D*--------------------------------------------------
062000080908     D FatturaManuale_chk_getDichiarazioneIntento...
062100080908     D                 PR                  LIKEDS(pjxx005ds)
062200080918
062300080918     D*--------------------------------------------------
062400080918     D* Procedure name: FatturaManuale_chk_chkImporto
062500080918     D* Purpose:        Controlla l'importo riga dettaglio.
062600080918     D* Returns:        Esito.
062700080918     D* Parameter:      piImporto => Importo.
062800080918     D*--------------------------------------------------
062900080918     D FatturaManuale_chk_chkImporto...
063000080918     D                 PR            10I 0
063100080918     D  piImporto                    15P 5 CONST
063200080918
063300080918     D*--------------------------------------------------
063400080918     D* Procedure name: FatturaManuale_chk_chkAliquotaIva
063500080918     D* Purpose:        Controlla aliquota/riferimento IVA.
063600080918     D* Returns:        Esito.
063700081003     D* Parameter:      piAliquotaIva => Aliquota IVA.
063800081003     D* Parameter:      piRiferimentoIva => Riferimento IVA.
063900080918     D*--------------------------------------------------
064000080918     D FatturaManuale_chk_chkAliquotaIva...
064100080918     D                 PR            10I 0
064200081003     D  piAliquotaIva                 5P 2 CONST
064300081003     D  piRiferimentoIva...
064400081003     D                                3A   CONST
064500080918     D  piSerie                       4A   OPTIONS(*NOPASS)
064600080918
064700080918     D*--------------------------------------------------
064800080918     D* Procedure name: FatturaManuale_chk_getDescrizioneRiferimentoIva
064900080918     D* Purpose:        Restituisce la descrizione del riferimento IVA.
065000080918     D* Returns:        Descrizione riferimento IVA.
065100080918     D* Parameter:      piTipoDescrizione => 'L' = lunga, 'B' = breve.
065200080918     D*--------------------------------------------------
065300080918     D FatturaManuale_chk_getDescrizioneRiferimentoIva...
065400080918     D                 PR            44A
065500080918     D  piTipoDescrizione...
065600080918     D                                1A   CONST
065700080918     D                                     OPTIONS(*NOPASS)
065800080918
065900080919     D*--------------------------------------------------
066000080919     D* Procedure name: FatturaManuale_chk_chkPeriodoCompetenza
066100080919     D* Purpose:        Controlla il periodo di competenza del ricavo.
066200080919     D* Returns:        Esito.
066300080919     D* Parameter:      piDataCompetenzaInizio => Data competenza iniziale.
066400080919     D* Parameter:      piDataCompetenzaFine => Data competenza finale.
066500080919     D*--------------------------------------------------
066600080919     D FatturaManuale_chk_chkPeriodoCompetenza...
066700080919     D                 PR            10I 0
066800080919     D  piDataCompetenzaInizio...
066900080919     D                                 D   DATFMT(*ISO)
067000080919     D  piDataCompetenzaFine...
067100080919     D                                 D   DATFMT(*ISO)
067200080926     D  piTipoInserimentoDettaglio...
067300080926     D                                1A   CONST
067400080926
067500080922     D*--------------------------------------------------
067600080922     D* Procedure name: FatturaManuale_chk_chkEntita
067700080922     D* Purpose:        Controlla l'entità co.an.
067800080922     D* Returns:        Esito.
067900080922     D* Parameter:      piEntitaTipo => Tipo entità.
068000080922     D* Parameter:      piEntitaCodice => Codice entità.
068100080922     D*--------------------------------------------------
068200080922     D FatturaManuale_chk_chkEntita...
068300080922     D                 PR            10I 0
068400080922     D piEntitaNumero                 3U 0 CONST
068500080922     D  piEntitaTipo                  3A
068600080922     D piEntitaCodice                 8A
068700080922
068800080922     D*--------------------------------------------------
068900080922     D* Procedure name: FatturaManuale_chk_getAnagraficaEntita
069000080922     D* Purpose:        Restituisce l'anagrafica di una entità co.an.
069100080922     D* Returns:        Anagrafica entità.
069200080922     D* Parameter:      piEntitaNumero => Numero entità 1-8
069300080922     D*--------------------------------------------------
069400080922     D FatturaManuale_chk_getAnagraficaEntita...
069500080922     D                 PR                  LIKEDS(ndDvoEntDs)
069600080922     D piEntitaNumero                 3U 0 CONST
069700080923     D piAnagraficaTipoEntita...
069800080923     D                                     LIKEDS(angG80Ds)
069900080923     D                                     OPTIONS(*NOPASS)
070000080923
070100080923     D*--------------------------------------------------
070200080923     D* Procedure name: FatturaManuale_chk_chkRegolaRipartizioneValori
070300080923     D* Purpose:        Controllo regola ripartizione valori.
070400080923     D* Returns:        Esito.
070500080923     D* Parameter:      piRegolaRipartizioneValori => Regola ripartizione v...
070600080923     D*                          alori co.an.
070700080923     D*--------------------------------------------------
070800080923     D FatturaManuale_chk_chkRegolaRipartizioneValori...
070900080923     D                 PR            10I 0
071000080923     D  piRegolaRipartizioneValori...
071100080923     D                                6A   CONST
071200080923
071300080923     D*--------------------------------------------------
071400080923     D* Procedure name: FatturaManuale_chk_getDescrizioneRegolaRipartizione...
071500080923     D*                          Valori
071600080923     D* Purpose:        Restituisce la descrizione della regola ripartizion...
071700080923     D*                          e valori.
071800080923     D* Returns:        Descrizione.
071900080923     D* Parameter:      piTipoDescrizione
072000080923     D*--------------------------------------------------
072100080923     D FatturaManuale_chk_getDescrizioneRegolaRipartizioneValori...
072200080923     D                 PR            44A
072300080923     D  piTipoDescrizione...
072400080923     D                                1A   CONST
072500080923     D                                     OPTIONS(*NOPASS)
072600080923
072700081003     D*--------------------------------------------------
072800081003     D* Procedure name: FatturaManuale_chk_chkRiferimento
072900081003     D* Purpose:        Controlla il riferimento.
073000081003     D* Returns:        Esito.
073100081003     D* Parameter:      piRiferimento => Riferimento.
073200081003     D*--------------------------------------------------
073300081003     D FatturaManuale_chk_chkRiferimento...
073400081003     D                 PR            10I 0
073500081003     D  piRiferimento                20A
073600081003
073700081007     D*--------------------------------------------------
073800081007     D* Procedure name: FatturaManuale_chk_chkFinaleFattura
073900081007     D* Purpose:        Controlli finali sulla fattura.
074000081007     D* Returns:        Esito.
074100081007     D*--------------------------------------------------
074200081007     D FatturaManuale_chk_chkFinaleFattura...
074300081007     D                 PR            10I 0
074400081007
074500081029     D*--------------------------------------------------
074600081029     D* Procedure name: FatturaManuale_chk_getAnagraficaCondizionePagamento
074700081029     D* Purpose:        Restituisce l'anagrafica della condizione pagamento.
074800081029     D* Returns:        Anagrafica condizione pagamento.
074900081029     D*--------------------------------------------------
075000081029     D FatturaManuale_chk_getAnagraficaCondizionePagamento...
075100081029     D                 PR                  LIKEDS(dvoCdpDs)
075200081029
075300081119     D*--------------------------------------------------
075400081119     D* Procedure name: FatturaManuale_chk_chkCommentoObbligatorio
075500081119     D* Purpose:        Restituisce *ON se esiste almeno un commento.
075600081119     D* Returns:        Esito.
075700081119     D* Parameter:      piNumeroRichiesta => ID richiesta
075800081119     D* Parameter:      piIdDettaglio => ID dettaglio
075900081119     D*--------------------------------------------------
076000081119     D FatturaManuale_chk_chkCommentoObbligatorio...
076100081119     D                 PR            10I 0
076200081119     D  piSocietaFattura...
076300081119     D                                3A
076400081119     D  piNumeroRichiesta...
076500081119     D                               10I 0 CONST
076600081119     D  piIdDettaglio                10I 0 CONST
076700081119     D                                     OPTIONS(*NOPASS)
076800081119
