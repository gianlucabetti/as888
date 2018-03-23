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
010000120905     D*--------------------------------------------------
010100120905     D* Procedure name: FatturaManuale_chk_chkAutUfficio
010200120905     D* Purpose:        Controlla se l'utente è autorizzato all'ufficio
010300120905     D* Returns:        Esito.
010400120905     D* Parameter:      piSocieta
010500120905     D* Parameter:      piUnita
010600120905     D* Parameter:      piGruppo
010700120905     D* Parameter:      piUtente
010800120905     D* Parameter:      piUfficio
010900120905     D*--------------------------------------------------
011000120905     D FatturaManuale_chk_chkAutUfficio...
011100120905     D                 PR            10I 0
011200120905     D  piSocieta                     3A
011300120905     D  piUnita                       8A
011400120905     D  piGruppo                      3P 0 CONST
011500120905     D  piUtente                     10A
011600120905     D  piUfficio                     2A   CONST
011700120905
011800070516     D*--------------------------------------------------
011900070516     D* Procedure name: FatturaManuale_chk_chkAmbito
012000070516     D* Purpose:        Controlla l'esistenza e la validità dell'ambito
012100070516     D* Returns:        Esito.
012200070516     D* Parameter:      piCdAmbito => Codice ambito
012300070516     D* Parameter:      piDataControlloValidita => Data riferimento per con...
012400070516     D*                          trollo validità.
012500070516     D*--------------------------------------------------
012600070516     D FatturaManuale_chk_chkAmbito...
012700070516     D                 PR            10I 0
012800070516     D  piCdAmbito                   10A
012900070516     D  piDataControlloValidita...
013000070516     D                               10D   DATFMT(*ISO)
013100070516     D                                     CONST OPTIONS(*NOPASS:*OMIT)
013101161110     D  piRegistroIva...
013102161110     D                                1A   CONST OPTIONS(*NOPASS)
013200070626
013300070626     D*--------------------------------------------------
013400070626     D* Procedure name: FatturaManuale_chk_chkAmbitoSocietaImposta
013500070626     D* Purpose:        Controlla che la società della fattura sia uguale alla
013600070626     D*                          società imposta dell'ambito.
013700070626     D* Returns:        Esito.
013800070626     D*--------------------------------------------------
013900070626     D FatturaManuale_chk_chkAmbitoSocietaImposta...
014000070626     D                 PR            10I 0
014100070626     D  piSocieta                     3A
014200070626
014300070626     D*--------------------------------------------------
014400070626     D* Procedure name: FatturaManuale_chk_chkAmbitoUnitaImposta
014500070626     D* Purpose:        Controlla che l'unità della fattura sia uguale all'...
014600070626     D*                          unità imposta dell'ambito.
014700070626     D* Returns:        Esito.
014800070626     D*--------------------------------------------------
014900070626     D FatturaManuale_chk_chkAmbitoUnitaImposta...
015000070626     D                 PR            10I 0
015100070626     D  piUnita                       8A
015200070626
015300070516     D*--------------------------------------------------
015400070516     D* Procedure name: FatturaManuale_chk_chkCliente
015500070516     D* Purpose:        Controlli su ID cliente.
015600070516     D* Returns:        Esito.
015700080529     D* Parameter:      piSocieta => Società.
015800080529     D* Parameter:      piKscCliente => Cliente.
015900080529     D* Parameter:      piDataRiferimento => Data riferimento per controlli.
016000080529     D* Parameter:      piSottoNatura => 'C' = Cliente, 'F' = Fornitore.
016100070516     D*--------------------------------------------------
016200070516     D FatturaManuale_chk_chkCliente...
016300070516     D                 PR            10I 0
016400070516     D  piSocieta                     3A
016500070516     D  piKscCliente                  8A
016600070516     D  piDataRiferimento...
016700070516     D                                 D   CONST
016800080529     D  piSottoNatura...
016900080529     D                                1A   CONST OPTIONS(*NOPASS)
017000070517
017100070517     D*--------------------------------------------------
017200070517     D* Procedure name: FatturaManuale_chk_chkClienteCodificato
017300070517     D* Purpose:        Controlla cliente codificato.
017400070517     D* Returns:        Esito.
017500070517     D* Parameter:      piSocieta => Società.
017600070517     D* Parameter:      piKscCliente
017700070517     D*--------------------------------------------------
017800070517     D FatturaManuale_chk_chkClienteCodificato...
017900070517     D                 PR            10I 0
018000070517     D  piSocieta                     3A
018100070517     D  piKscCliente                  8A
018200070517
018300070517     D*--------------------------------------------------
018400070517     D* Procedure name: FatturaManuale_chk_chkClienteObbligatorio
018500070517     D* Purpose:        Controlla obbligatorietà codice cliente.
018600070517     D* Returns:        Esito.
018700070517     D* Parameter:      piKscCliente => Codice cliente.
018800070517     D*--------------------------------------------------
018900070517     D FatturaManuale_chk_chkClienteObbligatorio...
019000070517     D                 PR            10I 0
019100070517     D  piKscCliente                  8A
019200070628
019300070628     D*--------------------------------------------------
019400070628     D* Procedure name: FatturaManuale_chk_chkCondizionePagamento
019500070628     D* Purpose:        Controlla la condizione di pagamento
019600070628     D* Returns:        Esito.
019700070628     D* Parameter:      piCondizionePagamento
019800070628     D*--------------------------------------------------
019900070628     D FatturaManuale_chk_chkCondizionePagamento...
020000070628     D                 PR            10I 0
020100070628     D  piCondizionePagamento...
020200070628     D                                3A
020300070628
020400070517     D*--------------------------------------------------
020500070517     D* Procedure name: FatturaManuale_chk_chkFatturaRiemessa
020600070517     D* Purpose:        Controllo esistenza riferimento fattura riemessa.
020700070517     D* Returns:        Esito.
020800070517     D* Parameter:      piSocieta => Società.
020900070517     D* Parameter:      piCtb => Contabilità.
021000070517     D* Parameter:      piCapoConto => Capo Conto
021100070517     D* Parameter:      piSottoConto => Sotto conto
021200070517     D* Parameter:      piDataPartita => Data partita.
021300070517     D* Parameter:      piNumeroPartita => Numero partita.
021400070517     D* Parameter:      piSeriePartita => Serie partita.
021500070517     D*--------------------------------------------------
021600070517     D FatturaManuale_chk_chkFatturaRiemessa...
021700070517     D                 PR            10I 0
021800070517     D  piSocieta                     3A
021900070517     D  piCtb                         2A   CONST
022000070517     D  piCapoConto                   6A
022100070517     D  piSottoConto                  8A
022200070517     D  piDataPartita                10D   DATFMT(*ISO)
022300070517     D  piNumeroPartita...
022400070517     D                                9P 0
022500070517     D piSeriePartita                 4A
022600070517
022700080520     D*--------------------------------------------------
022800080520     D* Procedure name: FatturaManuale_chk_chkModalitaSpedizione
022900080520     D* Purpose:        Controlla la modalità di spedizione della fattura.
023000080520     D* Returns:        Esito.
023100080520     D* Parameter:      piModalitaSpedizione => Modalità di spedizione
023200080520     D*--------------------------------------------------
023300080520     D FatturaManuale_chk_chkModalitaSpedizione...
023400080520     D                 PR            10I 0
023500080520     D  piModalitaSpedizione...
023600080520     D                                1A
023700080520
023800070517     D*--------------------------------------------------
023900070611     D* Procedure name: FatturaManuale_chk_chkOperazioneDettaglio
024000070608     D* Purpose:        Controllo azione riga dettaglio.
024100070517     D* Returns:        Esito.
024200070611     D* Parameter:      piOperazione => Operazione
024300070517     D*--------------------------------------------------
024400070611     D FatturaManuale_chk_chkOperazioneDettaglio...
024500070517     D                 PR            10I 0
024600070611     D  piOperazione                 15A   CONST
024700070516
024800070516     D*--------------------------------------------------
024900070611     D* Procedure name: FatturaManuale_chk_chkOperazioneTestata
025000070608     D* Purpose:        Controllo azione testata fattura.
025100070516     D* Returns:        Esito.
025200070611     D* Parameter:      piOperazione => Operazione
025300070516     D*--------------------------------------------------
025400070611     D FatturaManuale_chk_chkOperazioneTestata...
025500070516     D                 PR            10I 0
025600070611     D  piOperazione                 15A   CONST
025700080528     D  piNrRichiesta                10I 0 CONST
025800080528
025900070515     D*--------------------------------------------------
026000070613     D* Procedure name: FatturaManuale_chk_chkSocietaFattura
026100070613     D* Purpose:        Controlli su società fattura.
026200070515     D* Returns:        Esito.
026300070613     D* Parameter:      piSocieta
026400070515     D*--------------------------------------------------
026500070613     D FatturaManuale_chk_chkSocietaFattura...
026600070515     D                 PR            10I 0
026700070614     D  piSocietaFattura...
026800070515     D                                3A
026900070613
027000070613     D*--------------------------------------------------
027100070613     D* Procedure name: FatturaManuale_chk_chkSocietaLavoro
027200070613     D* Purpose:        Controlli su società lavoro.
027300070613     D* Returns:        Esito.
027400070613     D* Parameter:      piSocieta
027500070613     D*--------------------------------------------------
027600070613     D FatturaManuale_chk_chkSocietaLavoro...
027700070613     D                 PR            10I 0
027800070614     D  piSocietaLavoro...
027900070613     D                                3A
028000070613
028100070517     D*--------------------------------------------------
028200070517     D* Procedure name: FatturaManuale_chk_chkTipoInserimentoFattura
028300070517     D* Purpose:        Controllo la congruenza del tipo inserimento fattur...
028400070517     D*                          a-ambito.
028500070517     D* Returns:        Esito.
028600070622     D* Parameter:      piTipoInserimentoFattura => Tipo inserimento.
028700070517     D*--------------------------------------------------
028800070517     D FatturaManuale_chk_chkTipoInserimentoFattura...
028900070517     D                 PR            10I 0
029000070622     D  piTipoInserimentoFattura...
029100070622     D                                1A   CONST
029200070613
029300070613     D*--------------------------------------------------
029400070613     D* Procedure name: FatturaManuale_chk_chkUfficioRichiedente
029500070613     D* Purpose:        Controlla ufficio richiedente.
029600070613     D* Returns:        Esito.
029700070613     D* Parameter:      PiUfficio => Ufficio.
029800070613     D*--------------------------------------------------
029900070613     D FatturaManuale_chk_chkUfficioRichiedente...
030000070613     D                 PR            10I 0
030100070613     D  piUfficio                     2A
030200070613
030300070613     D*--------------------------------------------------
030400070614     D* Procedure name: FatturaManuale_chk_chkUfficioLavoro
030500070613     D* Purpose:        Controlla ufficio richiedente.
030600070613     D* Returns:        Esito.
030700070613     D* Parameter:      PiUfficio => Ufficio.
030800070613     D*--------------------------------------------------
030900070614     D FatturaManuale_chk_chkUfficioLavoro...
031000070613     D                 PR            10I 0
031100070613     D  piUfficio                     2A
031200070613
031300070510     D*--------------------------------------------------
031400070511     D* Procedure name: FatturaManuale_chk_chkUnita
031500070510     D* Purpose:        Controllo unità.
031600070510     D* Returns:        Esito.
031700070510     D* Parameter:      piUnita => Unità.
031800070510     D*--------------------------------------------------
031900070511     D FatturaManuale_chk_chkUnita...
032000070510     D                 PR            10I 0
032100070511     D  piSocieta                     3A
032200070511     D  piUnita                       8A
032300070517
032400070517     D*--------------------------------------------------
032500070517     D* Procedure name: FatturaManuale_chk_chkUnitaAmbitoCliente
032600070517     D* Purpose:        Controllo unità ambito-cliente
032700070517     D* Returns:        Esito.
032800070517     D*--------------------------------------------------
032900070517     D FatturaManuale_chk_chkUnitaAmbitoCliente...
033000070517     D                 PR            10I 0
033100070613
033200070614     P*--------------------------------------------------
033300070614     P* Procedure name: FatturaManuale_chk_chkUtenteLavoro
033400070614     P* Purpose:        Controlla utente lavoro.
033500070614     P* Returns:        Esito.
033600070614     P* Parameter:      piUtente => Utente lavoro.
033700070614     P*--------------------------------------------------
033800070614     D FatturaManuale_chk_chkUtenteLavoro...
033900070614     D                 PR            10I 0
034000070614     D  piUtenteLavoro...
034100070614     D                               10A
034200070614
034300070614     P*--------------------------------------------------
034400070614     P* Procedure name: FatturaManuale_chk_chkUtenteRichiedente
034500070614     P* Purpose:        Controlla utente richiedente.
034600070614     P* Returns:        Esito.
034700070614     P* Parameter:      piUtente => Utente richiedente.
034800070614     P*--------------------------------------------------
034900070614     D FatturaManuale_chk_chkUtenteRichiedente...
035000070614     D                 PR            10I 0
035100070614     D  piUtenteRichiedente...
035200070614     D                               10A
035300070628
035400080523     D*--------------------------------------------------
035500080523     D* Procedure name: FatturaManuale_chk_getAnagraficaAmbito
035600080523     D* Purpose:        Restituisce l'anagrafica dell'ambito.
035700080523     D* Returns:
035800080523     D*--------------------------------------------------
035900080523     D FatturaManuale_chk_getAnagraficaAmbito...
036000080523     D                 PR                  LIKEDS(ynfmaa00f)
036100080523
036200070628     D*--------------------------------------------------
036300070628     D* Procedure name: FatturaManuale_chk_getAnagraficaCliente
036400070628     D* Purpose:        Restituisce l'anagrafica cliente Proj.
036500070628     D* Returns:        Anagrafica cliente Proj.
036600070628     D*--------------------------------------------------
036700070628     D FatturaManuale_chk_getAnagraficaCliente...
036800070628     D                 PR                  LIKEDS(dvaCli)
036900080805
037000080805     D*--------------------------------------------------
037100080805     D* Procedure name: FatturaManuale_chk_getAnagraficaClienteFatturazione
037200080805     D* Purpose:        Restituisce l'anagrafica del cliente di fatturazione.
037300080805     D* Returns:        Anagrafica cliente di fatturazione.
037400080805     D*--------------------------------------------------
037500080805     D FatturaManuale_chk_getAnagraficaClienteFatturazione...
037600080805     D                 PR                  LIKEDS(dvaCli)
037700080805
037800080529     D*--------------------------------------------------
037900080529     D* Procedure name: FatturaManuale_chk_getAnagraficaFornitore
038000080529     D* Purpose:        Restituisce l'anagrafica fornitore Proj.
038100080529     D* Returns:        Anagrafica cliente Proj.
038200080529     D*--------------------------------------------------
038300080529     D FatturaManuale_chk_getAnagraficaFornitore...
038400080529     D                 PR                  LIKEDS(dvaFor)
038500070628
038600070516     D*--------------------------------------------------
038700070516     D* Procedure name: FatturaManuale_chk_getDescrizioneAmbito
038800070516     D* Purpose:        Restituisce la descrizione dell'ambito.
038900070516     D* Returns:        Descrizione ambito.
039000070516     D*--------------------------------------------------
039100070516     D FatturaManuale_chk_getDescrizioneAmbito...
039200070516     D                 PR            50A
039300070628
039400070628     D*--------------------------------------------------
039500070628     D* Procedure name: FatturaManuale_chk_getDescrizioneCondizionePagamento
039600070628     D* Purpose:        Restituisce la descrizione condizione pagamento
039700070628     D* Returns:        Descrizione condizione pagamento.
039800070628     D*--------------------------------------------------
039900070628     D FatturaManuale_chk_getDescrizioneCondizionePagamento...
040000070628     D                 PR            44A
040100070628
040200070628     D*--------------------------------------------------
040300070628     D* Procedure name: FatturaManuale_chk_getDescrizioneFilialeRichiedente
040400070628     D* Purpose:        Restituisce la descrizione della filiale richiedente.
040500070628     D* Returns:        Descrizione filiale richiedente.
040600070628     D*--------------------------------------------------
040700070628     D FatturaManuale_chk_getDescrizioneFilialeRichiedente...
040800070628     D                 PR            20A
040900070628
041000080520     D*--------------------------------------------------
041100080520     D* Procedure name: FatturaManuale_chk_getDescrizioneModalitaSpedizione
041200080520     D* Purpose:        Restituisce la descrizione della modalità di spediz...
041300080520     D*                          ione della fattura.
041400080520     D* Returns:        Descrizione modalità spedizione fattura.
041500080520     D*--------------------------------------------------
041600080520     D FatturaManuale_chk_getDescrizioneModalitaSpedizione...
041700080520     D                 PR            30A
041800080520
041900070614     D*--------------------------------------------------
042000070614     D* Procedure name: FatturaManuale_chk_getDescrizioneUfficioRichiedente
042100070614     D* Purpose:        Restituisce la descrizione dell'ufficio richiedente.
042200070614     D* Returns:        Descrizione ufficio richiedente.
042300070614     D*--------------------------------------------------
042400070614     D FatturaManuale_chk_getDescrizioneUfficioRichiedente...
042500070614     D                 PR            30A
042600070614
042700070511     D*--------------------------------------------------
042800070511     D* Procedure name: FatturaManuale_chk_getDescrizioneUnita
042900070511     D* Purpose:        Restituisce la descrizione dell'unità.
043000070511     D* Returns:        Descrizione unità.
043100070511     D*--------------------------------------------------
043200070511     D FatturaManuale_chk_getDescrizioneUnita...
043300070511     D                 PR            20A
043400070613
043500070613     D*--------------------------------------------------
043600070614     D* Procedure name: FatturaManuale_chk_getDescrizioneUtenteRichiedente
043700070613     D* Purpose:        Restituisce la descrizione dell'utente.
043800070613     D* Returns:        Descrizione utente.
043900070613     D*--------------------------------------------------
044000070614     D FatturaManuale_chk_getDescrizioneUtenteRichiedente...
044100070613     D                 PR            50A
044200070614
044300070614     D*--------------------------------------------------
044400070614     D* Procedure name: FatturaManuale_chk_getUfficioRichiedente
044500070614     D* Purpose:        Restituisce l'ufficio dell'utente richiedente.
044600070614     D* Returns:        Ufficio utente richiedente.
044700070614     D*--------------------------------------------------
044800070614     D FatturaManuale_chk_getUfficioRichiedente...
044900070614     D                 PR             2A
045000070614
045100070614     D*--------------------------------------------------
045200070614     D* Procedure name: FatturaManuale_chk_getFilialeRichiedente
045300070614     D* Purpose:        Restituisce la filiale dell'utente richiedente.
045400070614     D* Returns:        Filiale utente richiedente.
045500070614     D*--------------------------------------------------
045600070614     D FatturaManuale_chk_getFilialeRichiedente...
045700070614     D                 PR             3P 0
045800070614
045900070516     D*--------------------------------------------------
046000070516     D* Procedure name: FatturaManuale_chk_getRagioneSocialeCliente
046100070516     D* Purpose:        Restituisce la descrizione del cliente.
046200070516     D* Returns:        Ragione sociale cliente.
046300070516     D*--------------------------------------------------
046400070516     D FatturaManuale_chk_getRagioneSocialeCliente...
046500070516     D                 PR            44A
046600070517
046700070517     D*--------------------------------------------------
046800070517     D* Procedure name: FatturaManuale_chk_getUnitaImposta
046900070517     D* Purpose:        Restituisce l'unità imposta per l'ambito.
047000070517     D* Returns:
047100070517     D*--------------------------------------------------
047200070517     D FatturaManuale_chk_getUnitaImposta...
047300070517     D                 PR             8A
047400070703
047500070703     D*--------------------------------------------------
047600070703     D* Procedure name: FatturaManuale_chk_getTestoInizioFattura
047700070703     D* Purpose:        Restituisce il codice testo da stampare all'inizio ...
047800070703     D*                          della fattura.
047900070703     D* Returns:        Codice testo inizio fattura.
048000070703     D*--------------------------------------------------
048100070703     D FatturaManuale_chk_getTestoInizioFattura...
048200070703     D                 PR             2A
048300070703
048400070703     D*--------------------------------------------------
048500070703     D* Procedure name: FatturaManuale_chk_getTestoFineFattura
048600070703     D* Purpose:        Restituisce il codice testo da stampare alla fine ...
048700070703     D*                          della fattura.
048800070703     D* Returns:        Codice testo inizio fattura.
048900070703     D*--------------------------------------------------
049000070703     D FatturaManuale_chk_getTestoFineFattura...
049100070703     D                 PR             2A
049200070703
049300070517     D*--------------------------------------------------
049400070517     D* Procedure name: FatturaManuale_chk_isClienteObbligatorio
049500070517     D* Purpose:        Restituisce *ON se il clente è obbligatorio.
049600070517     D* Returns:        *ON/*OFF
049700070517     D*--------------------------------------------------
049800070517     D FatturaManuale_chk_isClienteObbligatorio...
049900070517     D                 PR              N
050000070702
050100070702     D*--------------------------------------------------
050200070702     D* Procedure name: FatturaManuale_chk_isCommentoFatturaObbligatorio
050300070702     D* Purpose:        Restituisce *ON se il commento fattura è obbligatorio.
050400070702     D* Returns:        *ON = il commento fattura è obbligatorio.
050500070702     D*--------------------------------------------------
050600070702     D FatturaManuale_chk_isCommentoFatturaObbligatorio...
050700070702     D                 PR              N
050800070702
050900070517     D*--------------------------------------------------
051000070517     D* Procedure name: FatturaManuale_chk_isRiemissioneFattura
051100070517     D* Purpose:        Restituisce *ON se è una riemissione di una fattura...
051200070517     D*                           stornata.
051300070517     D* Returns:        *ON = E' una riemissione di fattura stornata.
051400070517     D*--------------------------------------------------
051500070517     D FatturaManuale_chk_isRiemissioneFattura...
051600070517     D                 PR              N
051700070614
051800070614     P*--------------------------------------------------
051900070614     P* Procedure name: FatturaManuale_chk_isUtenteLavoroUfficioFatturazione
052000070614     P* Purpose:        Restituisce *ON se l'utente lavoro appartiene all'ufficio fatturazione
052100070614     P* Returns:        *ON = ufficio fatturazione.
052200070614     P*--------------------------------------------------
052300070614     D FatturaManuale_chk_isUtenteLavoroUfficioFatturazione...
052400070614     D                 PR              N
052500070614
052600070614     D*--------------------------------------------------
052700070614     D* Procedure name: FatturaManuale_chk_setKpjba
052800070614     D* Purpose:        Imposta KPJBA.
052900070614     D* Returns:
053000070614     D* Parameter:      piKpjba
053100070614     D*--------------------------------------------------
053200070614     D FatturaManuale_chk_setKpjba...
053300070614     D                 PR
053400070614     D  piKpjba                            LIKEDS(kpjba)
053500070614
053600080716     D*--------------------------------------------------
053700080716     D* Procedure name: FatturaManuale_chk_chkCodiceProdotto
053800080716     D* Purpose:        Controlla il codice prodotto.
053900080716     D* Returns:        Esito.
054000080716     D* Parameter:      piCodiceProdotto => Codice prodotto.
054100080716     D* Parameter:      piDataValidita => Data validità prodotto
054200080717     D* Parameter:      piAmbito => Ambito di fatturazione
054300080716     D*--------------------------------------------------
054400080716     D FatturaManuale_chk_chkCodiceProdotto...
054500080716     D                 PR            10I 0
054600080716     D  piCodiceProdotto...
054700080716     D                               10A
054800080716     D piDataValidita                  D   DATFMT(*ISO)
054900080716     D                                     CONST
055000080717     D  piAmbito...
055100080717     D                               10A
055200080716
055300080716     D*--------------------------------------------------
055400080716     D* Procedure name: FatturaManuale_chk_getAnagraficaProdotto
055500080716     D* Purpose:        Restituisce l'anagrafica del prodotto.
055600080716     D* Returns:        Anagrafica del prodotto.
055700080716     D*--------------------------------------------------
055800080716     D FatturaManuale_chk_getAnagraficaProdotto...
055900080716     D                 PR                  LIKEDS(ynfmap00f)
056000080716
056100080717     D*--------------------------------------------------
056200080717     D* Procedure name: FatturaManuale_chk_chkTipoInserimentoDettaglio
056300080717     D* Purpose:        Controlla il tipo inserimento riga dettaglio.
056400080717     D* Returns:        Esito.
056500080717     D* Parameter:      piTipoInserimentoDettaglio => Tipo inserimento.
056600080717     D*--------------------------------------------------
056700080717     D FatturaManuale_chk_chkTipoInserimentoDettaglio...
056800080717     D                 PR            10I 0
056900080717     D  piTipoInserimentoDettaglio...
057000080717     D                                1A   CONST
057100080717
057200080731     D*--------------------------------------------------
057300080731     D* Procedure name: FatturaManuale_chk_chkDataRegistrazione
057400080731     D* Purpose:        Controlla la data registrazione.
057500080731     D* Returns:        Esito.
057600080731     D* Parameter:      piSocieta => Società
057700080731     D* Parameter:      piSerie => Serie registrazione.
057800080731     D* Parameter:      piDataRegistrazione => Data registrazione.
057900080731     D*--------------------------------------------------
058000080731     D FatturaManuale_chk_chkDataRegistrazione...
058100080731     D                 PR            10I 0
058200080731     D  piSocieta                     3A
058300080731     D  piSerie                       4A
058400080731     D  piDataRegistrazione...
058500080731     D                                 D   DATFMT(*ISO)
058600080731
058700080828     D*--------------------------------------------------
058800080828     D* Procedure name: FatturaManuale_chk_chkContoVoce
058900080828     D* Purpose:        Imposta i conti e le voci.
059000080828     D* Returns:        Esito.
059100080828     D* Parameter:      piTipoConto => 'N' = Normale; 'E' = fatture da Emet...
059200080828     D*                          tere; 'S' = Sopravvenienze.
059300080828     D* Parameter:      piCapoConto => Capo conto.
059400080828     D* Parameter:      piSottoConto => Sotto conto.
059500080828     D* Parameter:      piVoce => Voce.
059600080828     D*--------------------------------------------------
059700080828     D FatturaManuale_chk_chkContoVoce...
059800080828     D                 PR            10I 0
059900080828     D  piSocieta                     3A
060000080828     D  piTipoConto                   1A   CONST
060100080828     D  piCapoConto                   6A
060200080829     D  piSottoConto                  8A   VALUE
060300081104     D  piVoce                        8A   OPTIONS(*NOPASS:*OMIT) CONST
060400081002     D  piFatturaInContoEconomico...
060500081002     D                                1A   OPTIONS(*NOPASS) CONST
060600081002     D  piDataCompetenza...
060700081002     D                                 D   OPTIONS(*NOPASS) CONST
060800081002
060900080829     D*--------------------------------------------------
061000080829     D* Procedure name: FatturaManuale_chk_getAnagraficaConto
061100080829     D* Purpose:        Restituisce l'anagrafica del conto.
061200080829     D* Returns:        Anagrafica conto contabile.
061300080829     D* Parameter:      piTipoConto => Tipo conto: 'N' = Normale, 'E' = fat...
061400080829     D*                          ture da Emettere, 'S' = Sopravvenienze.
061500080829     D*--------------------------------------------------
061600080829     D FatturaManuale_chk_getAnagraficaConto...
061700080829     D                 PR                  LIKEDS(nd002ds)
061800080829     D  piTipoConto                   1A   CONST
061900080829     D piNd002ds_kcc...
062000080829     D                                     LIKEDS(nd002ds) OPTIONS(*NOPASS)
062100080901
062200080901     D*--------------------------------------------------
062300080901     D* Procedure name: FatturaManuale_chk_getAnagraficaVoce
062400080901     D* Purpose:        Restituisce l'anagrafica della voce.
062500080901     D* Returns:        Anagrafica conto contabile.
062600080901     D* Parameter:      piTipoConto => Tipo conto: 'N' = Normale,
062700080901     D*                                            'S' = Sopravvenienze.
062800080901     D*--------------------------------------------------
062900080901     D FatturaManuale_chk_getAnagraficaVoce...
063000080901     D                 PR                  LIKEDS(anvci00f)
063100080901     D  piTipoConto                   1A   CONST
063200080829
063300080908     D*--------------------------------------------------
063400080908     D* Procedure name: FatturaManuale_chk_getDichiarazioneIntento
063500080908     D* Purpose:        Restituisce il riferimento dichiarazione intento.
063600080908     D* Returns:        Riferimento dichiarazione intento.
063700080908     D*--------------------------------------------------
063800080908     D FatturaManuale_chk_getDichiarazioneIntento...
063900080908     D                 PR                  LIKEDS(pjxx005ds)
064000080918
064100080918     D*--------------------------------------------------
064200080918     D* Procedure name: FatturaManuale_chk_chkImporto
064300080918     D* Purpose:        Controlla l'importo riga dettaglio.
064400080918     D* Returns:        Esito.
064500080918     D* Parameter:      piImporto => Importo.
064600080918     D*--------------------------------------------------
064700080918     D FatturaManuale_chk_chkImporto...
064800080918     D                 PR            10I 0
064900080918     D  piImporto                    15P 5 CONST
065000080918
065100080918     D*--------------------------------------------------
065200080918     D* Procedure name: FatturaManuale_chk_chkAliquotaIva
065300080918     D* Purpose:        Controlla aliquota/riferimento IVA.
065400080918     D* Returns:        Esito.
065500081003     D* Parameter:      piAliquotaIva => Aliquota IVA.
065600081003     D* Parameter:      piRiferimentoIva => Riferimento IVA.
065700080918     D*--------------------------------------------------
065800080918     D FatturaManuale_chk_chkAliquotaIva...
065900080918     D                 PR            10I 0
066000081003     D  piAliquotaIva                 5P 2 CONST
066100081003     D  piRiferimentoIva...
066200081003     D                                3A   CONST
066300080918     D  piSerie                       4A   OPTIONS(*NOPASS)
066400080918
066500080918     D*--------------------------------------------------
066600080918     D* Procedure name: FatturaManuale_chk_getDescrizioneRiferimentoIva
066700080918     D* Purpose:        Restituisce la descrizione del riferimento IVA.
066800080918     D* Returns:        Descrizione riferimento IVA.
066900080918     D* Parameter:      piTipoDescrizione => 'L' = lunga, 'B' = breve.
067000080918     D*--------------------------------------------------
067100080918     D FatturaManuale_chk_getDescrizioneRiferimentoIva...
067200080918     D                 PR            44A
067300080918     D  piTipoDescrizione...
067400080918     D                                1A   CONST
067500080918     D                                     OPTIONS(*NOPASS)
067600080918
067700080919     D*--------------------------------------------------
067800080919     D* Procedure name: FatturaManuale_chk_chkPeriodoCompetenza
067900080919     D* Purpose:        Controlla il periodo di competenza del ricavo.
068000080919     D* Returns:        Esito.
068100080919     D* Parameter:      piDataCompetenzaInizio => Data competenza iniziale.
068200080919     D* Parameter:      piDataCompetenzaFine => Data competenza finale.
068300080919     D*--------------------------------------------------
068400080919     D FatturaManuale_chk_chkPeriodoCompetenza...
068500080919     D                 PR            10I 0
068600080919     D  piDataCompetenzaInizio...
068700080919     D                                 D   DATFMT(*ISO)
068800080919     D  piDataCompetenzaFine...
068900080919     D                                 D   DATFMT(*ISO)
069000080926     D  piTipoInserimentoDettaglio...
069100080926     D                                1A   CONST
069200080926
069300080922     D*--------------------------------------------------
069400080922     D* Procedure name: FatturaManuale_chk_chkEntita
069500080922     D* Purpose:        Controlla l'entità co.an.
069600080922     D* Returns:        Esito.
069700080922     D* Parameter:      piEntitaTipo => Tipo entità.
069800080922     D* Parameter:      piEntitaCodice => Codice entità.
069900080922     D*--------------------------------------------------
070000080922     D FatturaManuale_chk_chkEntita...
070100080922     D                 PR            10I 0
070200080922     D piEntitaNumero                 3U 0 CONST
070300080922     D  piEntitaTipo                  3A
070400080922     D piEntitaCodice                 8A
070500080922
070600080922     D*--------------------------------------------------
070700080922     D* Procedure name: FatturaManuale_chk_getAnagraficaEntita
070800080922     D* Purpose:        Restituisce l'anagrafica di una entità co.an.
070900080922     D* Returns:        Anagrafica entità.
071000080922     D* Parameter:      piEntitaNumero => Numero entità 1-8
071100080922     D*--------------------------------------------------
071200080922     D FatturaManuale_chk_getAnagraficaEntita...
071300080922     D                 PR                  LIKEDS(ndDvoEntDs)
071400080922     D piEntitaNumero                 3U 0 CONST
071500080923     D piAnagraficaTipoEntita...
071600080923     D                                     LIKEDS(angG80Ds)
071700080923     D                                     OPTIONS(*NOPASS)
071800080923
071900080923     D*--------------------------------------------------
072000080923     D* Procedure name: FatturaManuale_chk_chkRegolaRipartizioneValori
072100080923     D* Purpose:        Controllo regola ripartizione valori.
072200080923     D* Returns:        Esito.
072300080923     D* Parameter:      piRegolaRipartizioneValori => Regola ripartizione v...
072400080923     D*                          alori co.an.
072500080923     D*--------------------------------------------------
072600080923     D FatturaManuale_chk_chkRegolaRipartizioneValori...
072700080923     D                 PR            10I 0
072800080923     D  piRegolaRipartizioneValori...
072900080923     D                                6A   CONST
073000080923
073100080923     D*--------------------------------------------------
073200080923     D* Procedure name: FatturaManuale_chk_getDescrizioneRegolaRipartizione...
073300080923     D*                          Valori
073400080923     D* Purpose:        Restituisce la descrizione della regola ripartizion...
073500080923     D*                          e valori.
073600080923     D* Returns:        Descrizione.
073700080923     D* Parameter:      piTipoDescrizione
073800080923     D*--------------------------------------------------
073900080923     D FatturaManuale_chk_getDescrizioneRegolaRipartizioneValori...
074000080923     D                 PR            44A
074100080923     D  piTipoDescrizione...
074200080923     D                                1A   CONST
074300080923     D                                     OPTIONS(*NOPASS)
074400080923
074500081003     D*--------------------------------------------------
074600081003     D* Procedure name: FatturaManuale_chk_chkRiferimento
074700081003     D* Purpose:        Controlla il riferimento.
074800081003     D* Returns:        Esito.
074900081003     D* Parameter:      piRiferimento => Riferimento.
075000081003     D*--------------------------------------------------
075100081003     D FatturaManuale_chk_chkRiferimento...
075200081003     D                 PR            10I 0
075300081003     D  piRiferimento                20A
075400081003
075500081007     D*--------------------------------------------------
075600081007     D* Procedure name: FatturaManuale_chk_chkFinaleFattura
075700081007     D* Purpose:        Controlli finali sulla fattura.
075800081007     D* Returns:        Esito.
075900081007     D*--------------------------------------------------
076000081007     D FatturaManuale_chk_chkFinaleFattura...
076100081007     D                 PR            10I 0
076200081007
076300081029     D*--------------------------------------------------
076400081029     D* Procedure name: FatturaManuale_chk_getAnagraficaCondizionePagamento
076500081029     D* Purpose:        Restituisce l'anagrafica della condizione pagamento.
076600081029     D* Returns:        Anagrafica condizione pagamento.
076700081029     D*--------------------------------------------------
076800081029     D FatturaManuale_chk_getAnagraficaCondizionePagamento...
076900081029     D                 PR                  LIKEDS(dvoCdpDs)
077000081029
077100081119     D*--------------------------------------------------
077200081119     D* Procedure name: FatturaManuale_chk_chkCommentoObbligatorio
077300081119     D* Purpose:        Restituisce *ON se esiste almeno un commento.
077400081119     D* Returns:        Esito.
077500081119     D* Parameter:      piNumeroRichiesta => ID richiesta
077600081119     D* Parameter:      piIdDettaglio => ID dettaglio
077700081119     D*--------------------------------------------------
077800081119     D FatturaManuale_chk_chkCommentoObbligatorio...
077900081119     D                 PR            10I 0
078000081119     D  piSocietaFattura...
078100081119     D                                3A
078200081119     D  piNumeroRichiesta...
078300081119     D                               10I 0 CONST
078400081119     D  piIdDettaglio                10I 0 CONST
078500081119     D                                     OPTIONS(*NOPASS)
078600091201
078700091201     D*--------------------------------------------------
078800091201     D* Procedure name: FatturaManuale_chk_chkTrattamentoIva
078900091201     D* Purpose:        Controlla il trattamento IVA della fattura.
079000091201     D* Returns:        Esito.
079100091201     D* Parameter:      piNumeroRichiesta => ID richiesta.
079200091201     D* Parameter:      piSocieta => ID società.
079300091201     D* Parameter:      piDataProtocollo => Data protocollo IVA.
079400091201     D* Parameter:      piKscCliente => ID cliente.
079500091201     D* Parameter:      piForzareErrori => *ON = forzare gli errori.
079600091201     D*--------------------------------------------------
079700091201     D FatturaManuale_chk_chkTrattamentoIva...
079800091201     D                 PR            10I 0
079900091201     D  piNumeroRichiesta...
080000091201     D                               10I 0 CONST
080100091202     D  piSocieta                     3A
080200091201     D  piDataProtocollo...
080300091201     D                                 D   DATFMT(*ISO)
080400091201     D                                     CONST
080500091202     D  piKscCliente                  8A
080600091201     D  piForzareErrori...
080700091201     D                                 N   CONST
080800091201
