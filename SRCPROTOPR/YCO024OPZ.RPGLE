000100070518
000200070518     D*--------------------------------------------------
000300070608     D* Procedure name: FatturaManuale_opz_newOpzioniTestata
000400070608     D* Purpose:        Imposta le azioni usabili sulla testata.
000500070518     D* Returns:        Esito.
000600070518     D*--------------------------------------------------
000700070608     D FatturaManuale_opz_newOpzioniTestata...
000800070518     D                 PR            10I 0
000900070611     D piOperazioniDisabilitate...
001000070608     D                               15A   DIM(50)
001100070522     D                                     OPTIONS(*NOPASS)
001200070522
001300070522     D*--------------------------------------------------
001400070608     D* Procedure name: FatturaManuale_opz_getOpzioniTestata
001500070522     D* Purpose:        Restituisce 2 variabili contenenti la descrizione d...
001600070522     D*                          elle opzioni.
001700070522     D* Returns:        Esito.
001800070522     D* Parameter:      opz1 => Variabile contenente la prima riga opzioni.
001900070522     D* Parameter:      opz2 => Variabile contenente la seconda riga opzioni.
002000070522     D* Parameter:      ff23 => Variabile contenente flag controllo F23
002100070522     D*--------------------------------------------------
002200070608     D FatturaManuale_opz_getOpzioniTestata...
002300070522     D                 PR            10I 0
002400070522     D  opz1                         76A
002500070522     D  opz2                         76A
002600070522     D  ff23                          1A
002700070523
002800070523     D*--------------------------------------------------
002900070608     D* Procedure name: FatturaManuale_opz_isOpzioneTestataAttiva
003000070523     D* Purpose:        Restituisce *ON se l'opzione fattura è attiva.
003100070523     D* Returns:        *ON = opzione attiva.
003200070608     D* Parameter:      piOpzione => Numero opzione.
003300070523     D*--------------------------------------------------
003400070608     D FatturaManuale_opz_isOpzioneTestataAttiva...
003500070523     D                 PR              N
003600070608     D  piOpzione                     2A
003700070608
003800070608     D*--------------------------------------------------
003900070608     D* Procedure name: FatturaManuale_opz_newOpzioniDettaglio
004000070608     D* Purpose:        Imposta le azioni usabili sulla testata.
004100070608     D* Returns:        Esito.
004200070608     D*--------------------------------------------------
004300070608     D FatturaManuale_opz_newOpzioniDettaglio...
004400070608     D                 PR            10I 0
004500070611     D piOperazioniDisabilitate...
004600070608     D                               15A   DIM(50)
004700070608     D                                     OPTIONS(*NOPASS)
004800070608
004900070608     D*--------------------------------------------------
005000070608     D* Procedure name: FatturaManuale_opz_getOpzioniDettaglio
005100070608     D* Purpose:        Restituisce 2 variabili contenenti la descrizione d...
005200070608     D*                          elle opzioni.
005300070608     D* Returns:        Esito.
005400070608     D* Parameter:      opz1 => Variabile contenente la prima riga opzioni.
005500070608     D* Parameter:      opz2 => Variabile contenente la seconda riga opzioni.
005600070608     D* Parameter:      ff23 => Variabile contenente flag controllo F23
005700070608     D*--------------------------------------------------
005800070608     D FatturaManuale_opz_getOpzioniDettaglio...
005900070608     D                 PR            10I 0
006000070608     D  opz1                         76A
006100070608     D  opz2                         76A
006200070608     D  ff23                          1A
006300070608
006400070608     D*--------------------------------------------------
006500070608     D* Procedure name: FatturaManuale_opz_isOpzioneDettaglioAttiva
006600070608     D* Purpose:        Restituisce *ON se l'opzione fattura è attiva.
006700070608     D* Returns:        *ON = opzione attiva.
006800070608     D* Parameter:      piOpzione => Numero opzione.
006900070608     D*--------------------------------------------------
007000070608     D FatturaManuale_opz_isOpzioneDettaglioAttiva...
007100070608     D                 PR              N
007200070608     D  piOpzione                     2A
007300070523
007400080227     D*--------------------------------------------------
007500080227     D* Procedure name: FatturaManuale_opz_getOperazioneOpzioneTestata
007600080227     D* Purpose:        Reperisce l'operazione associata ad una opzione di ...
007700080227     D*                          testata.
007800080227     D* Returns:        Operazione.
007900080227     D* Parameter:      piOpzione => Opzione.
008000080227     D*--------------------------------------------------
008100080227     D FatturaManuale_opz_getOperazioneOpzioneTestata...
008200080227     D                 PR            15A
008300080227     D  piOpzione                     2A
008400080901
008500080901     D*--------------------------------------------------
008600080901     D* Procedure name: FatturaManuale_opz_getOperazioneOpzioneDettaglio
008700080901     D* Purpose:        Reperisce l'operazione associata ad una opzione di ...
008800080901     D*                          Dettaglio.
008900080901     D* Returns:        Operazione.
009000080901     D* Parameter:      piOpzione => Opzione.
009100080901     D*--------------------------------------------------
009200080901     D FatturaManuale_opz_getOperazioneOpzioneDettaglio...
009300080901     D                 PR            15A
009400080901     D  piOpzione                     2A
009500080901
