000100080314     D*--------------------------------------------------
000200080609     D* Procedure name: FatturaManuale_in_init
000300080314     P* Purpose:        Inizializza una nuova sessione di inserimento/aggiornamento.
000400080314     P*                 Da chiamare per primo, una sola volta.
000500080314     P* Returns:        Esito.
000600080314     P* Parameter:      pisLibreriaFiles
000700080314     P* Parameter:      piKpjba
000800080314     D*--------------------------------------------------
000900080609     D FatturaManuale_in_init...
001000080314     D                 PR            10I 0
001100080314     D  piLibreriaFiles...
001200080314     D                               10A   OPTIONS(*NOPASS:*OMIT) CONST
001300080314     D  piKpjba...
001400080314     D                                     OPTIONS(*NOPASS:*OMIT)
001500080314     D                                     LIKE(kpjba)
001600080314     D  piSocietaLavoro...
001700080314     D                                3A   OPTIONS(*NOPASS)
001800080314
001900080314     D*--------------------------------------------------
002000080609     D* Procedure name: FatturaManuale_in_finalize
002100080314     D* Purpose:
002200080314     D* Returns:        Esito.
002300080314     D*--------------------------------------------------
002400080609     D FatturaManuale_in_finalize...
002500080314     D                 PR            10I 0
002600080314
002700080314     D*--------------------------------------------------
002800080314     D* Procedure name: FatturaManuale_in_newFattura
002900080314     D* Purpose:
003000080314     D* Returns:        Esito.
003100080616     D* Parameter:      piNumeroRichiesta => Numero richiesta.
003200080616     D* Parameter:      piVoglioAncheIlDettaglio => *ON = Voglio anche il dettaglio.
003300080617     D* Parameter:      piVoglioAncheIlRiepilogoIVA => *ON = Voglio anche il riepilogo IVA.
003400080314     D*--------------------------------------------------
003500080314     D FatturaManuale_in_newFattura...
003600080314     D                 PR            10I 0
003700080314     D  piNumeroRichiesta...
003800080314     D                               10I 0 CONST
003900080616     D  piVoglioAncheIlDettaglio...
004000080617     D                                 N   CONST OPTIONS(*NOPASS:*OMIT)
004100080617     D  piVoglioAncheIlRiepilogoIVA...
004200080617     D                                 N   CONST OPTIONS(*NOPASS)
004300080617
004400080617     D*--------------------------------------------------
004500080617     D* Procedure name: FatturaManuale_in_endFattura
004600080617     D* Purpose:        Chiude l'oggetto fattura.
004700080617     D* Returns:
004800080617     D*--------------------------------------------------
004900080617     D FatturaManuale_in_endFattura...
005000080617     D                 PR
005100080314
005200080528     D*--------------------------------------------------
005300080528     D* Procedure name: FatturaManuale_in_newTestata
005400080528     D* Purpose:        Costruttore oggetto testata.
005500080528     D* Returns:        Esito.
005600080528     D*--------------------------------------------------
005700080528     D FatturaManuale_in_newTestata...
005800080528     D                 PR            10I 0
005900080528
006000080528     D*--------------------------------------------------
006100080528     D* Procedure name: FatturaManuale_in_getNRichiesta
006200080528     D* Purpose:        Restituisce l' ID fattura.
006300080528     D* Returns:        ID fattura.
006400080528     D*--------------------------------------------------
006500080528     D FatturaManuale_in_getNRichiesta...
006600080528     D                 PR            10I 0
006700080528
006800080314     D*--------------------------------------------------
006900080314     D* Procedure name: FatturaManuale_in_getTestata
007000080314     D* Purpose:
007100080314     D* Returns:        Esito.
007200080314     D*--------------------------------------------------
007300080314     D FatturaManuale_in_getTestata...
007400080314     D                 PR            10I 0
007500080317     D  piYnfmft00f                        LIKEDS(in_ynfmft00f)
007600080317
007700080317     D*--------------------------------------------------
007800080317     D* Procedure name: FatturaManuale_in_getStatoAvanzamento
007900080317     D* Purpose:        Restituisce lo stato avanzamento richiesta.
008000080317     D* Returns:        Stato avanzamento richiesta.
008100080317     D*--------------------------------------------------
008200080317     D FatturaManuale_in_getStatoAvanzamento...
008300080317     D                 PR             2A
008400080317
008500080618     D*--------------------------------------------------
008600080618     D* Procedure name: FatturaManuale_in_newDettaglio
008700080618     D* Purpose:        Prepara il dettaglio della fattura.
008800080618     D* Returns:        Esito.
008900080717     D* Parameter:      piSoloRigheAttive => *ON = solo le righe attive.
009000080717     D*--------------------------------------------------
009100080618     D FatturaManuale_in_newDettaglio...
009200080618     D                 PR            10I 0
009300080717     D  piSoloRigheAttive...
009400080717     D                                 N   OPTIONS(*NOPASS) CONST
009500080717
009600080618     D*--------------------------------------------------
009700080618     D* Procedure name: FatturaManuale_in_endDettaglio
009800080618     D* Purpose:        Termina l'oggetto dettaglio.
009900080618     D* Returns:        Esito.
010000080618     D*--------------------------------------------------
010100080618     D FatturaManuale_in_endDettaglio...
010200080618     D                 PR            10I 0
010300080618
010400080613     D*--------------------------------------------------
010500080618     D* Procedure name: FatturaManuale_in_newRigaDettaglio
010600080613     D* Purpose:        Costruttore oggetto dettaglio.
010700080613     D* Returns:        Esito.
010800080613     D* Parameter:      piIdDettaglio => ID dettaglio.
010900080613     D*--------------------------------------------------
011000080618     D FatturaManuale_in_newRigaDettaglio...
011100080613     D                 PR            10I 0
011200080613     D  piIdDettaglio                10I 0 CONST
011300080613
011400080613     D*--------------------------------------------------
011500080613     D* Procedure name: FatturaManuale_in_getDettaglio
011600080613     D* Purpose:        Restituisce una riga di dettaglio
011700080613     D* Returns:        Esito.
011800080613     D* Parameter:      piYnfmfd00f => Record di dettaglio
011900080613     D*--------------------------------------------------
012000080613     D FatturaManuale_in_getDettaglio...
012100080613     D                 PR            10I 0
012200080613     D  piYnfmfd00f                        LIKE(in_ynfmfd00f)
012300080613
012400080613     D*--------------------------------------------------
012500080613     D* Procedure name: FatturaManuale_in_getIdDettaglio
012600080613     D* Purpose:        Restituisce l'ID dettaglio.
012700080613     D* Returns:        ID dettaglio.
012800080613     D*--------------------------------------------------
012900080613     D FatturaManuale_in_getIdDettaglio...
013000080613     D                 PR            10I 0
013100080613
013200080613     D*--------------------------------------------------
013300080613     D* Procedure name: FatturaManuale_in_getTotaleImponibile
013400080613     D* Purpose:        Restituisce il totale imponibile della fattura.
013500080613     D* Returns:        Totale imponibile fattura.
013600080613     D*--------------------------------------------------
013700080613     D FatturaManuale_in_getTotaleImponibile...
013800080613     D                 PR            15P 5
013900080613
014000080616     D*--------------------------------------------------
014100080616     D* Procedure name: FatturaManuale_in_getNextDettaglio
014200080616     D* Purpose:        Restituisce la riga di dettaglio successiva.
014300080616     D* Returns:        Esito.
014400080616     D* Parameter:      piYnfmfd00f => Record dettaglio fattura.
014500080616     D*--------------------------------------------------
014600080616     D FatturaManuale_in_getNextDettaglio...
014700080616     D                 PR            10I 0
014800080616     D  piYnfmfd00f                        LIKEDS(in_ynfmfd00f)
014900080616
015000080618     D*--------------------------------------------------
015100080618     D* Procedure name: FatturaManuale_in_newRiepilogoIVA
015200080618     D* Purpose:        Crea l'oggetto riepilogo IVA.
015300080618     D* Returns:        Esito.
015400080618     D*--------------------------------------------------
015500080618     D FatturaManuale_in_newRiepilogoIVA...
015600080618     D                 PR            10I 0
015700080618
015800080618     D*--------------------------------------------------
015900080618     D* Procedure name: FatturaManuale_in_endRiepilogoIVA
016000080618     D* Purpose:        Termina l'oggetto riepilogo IVA.
016100080618     D* Returns:        Esito.
016200080618     D*--------------------------------------------------
016300080618     D FatturaManuale_in_endRiepilogoIVA...
016400080618     D                 PR            10I 0
016500080618
016600080617     D*--------------------------------------------------
016700080617     D* Procedure name: FatturaManuale_in_getNextRiepilogoIVA
016800080617     D* Purpose:        Restituisce il successivo riepilogo IVA.
016900080617     D* Returns:        Esito.
017000080617     D* Parameter:      piAliquota => Aliquota IVA.
017100080617     D* Parameter:      piRiferimentoIVA => Riferimento IVA.
017200080617     D* Parameter:      piImponibile => Imponibile.
017300080617     D* Parameter:      piImposta => Imposta.
017400080617     D*--------------------------------------------------
017500080617     D FatturaManuale_in_getNextRiepilogoIVA...
017600080617     D                 PR            10I 0
017700080617     D  piAliquota                    5P 2
017800080617     D  piRiferimentoIVA...
017900080617     D                                3A
018000080617     D  piImponibile                 15P 2
018100080617     D  piImposta                    15P 2
018200080617
018300080717     D*--------------------------------------------------
018400080717     D* Procedure name: FatturaManuale_in_getTotaliFattura
018500080717     D* Purpose:        Restituisce i totali fattura.
018600080717     D* Returns:        Esito.
018700080717     D* Parameter:      piTotaleImponibileSoggetto => Totale imponibile sog...
018800080717     D*                          getto a IVA.
018900080717     D* Parameter:      piTotaleImponibileNonSoggetto => Totale imponibile ...
019000080717     D*                          non soggetto a IVA.
019100080717     D* Parameter:      piTotaleImposta => Totale imposta.
019200080717     D* Parameter:      piBollo => Bollo.
019300080717     D*--------------------------------------------------
019400080717     D FatturaManuale_in_getTotaliFattura...
019500080717     D                 PR            10I 0
019600080717     D  piTotaleImponibileSoggetto...
019700080717     D                               15P 2
019800080717     D  piTotaleImponibileNonSoggetto...
019900080717     D                               15P 2
020000080717     D  piTotaleImposta...
020100080717     D                               15P 2
020200080717     D  piBollo                       3P 2
020300080717
020400080805     D*--------------------------------------------------
020500080805     D* Procedure name: FatturaManuale_in_getAnagraficaClienteFatturazione
020600080805     D* Purpose:        Restituisce l'anagrafica del cliente di fatturazione.
020700080805     D* Returns:        Anagrafica cliente di fatturazione.
020800080805     D*--------------------------------------------------
020900080805     D FatturaManuale_in_getAnagraficaClienteFatturazione...
021000080805     D                 PR                  LIKEDS(dvaCli)
021001170718
021100170718       // --------------------------------------------------
021101170718       // Prototype for procedure: FatturaManuale_in_isFatturaSplitPaymentIva
021102170718       // --------------------------------------------------
021103170718
021104170718       DCL-PR FatturaManuale_in_isFatturaSplitPaymentIva IND;
021105170718       END-PR ;
021106170718
