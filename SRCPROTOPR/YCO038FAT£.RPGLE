000100080605     D*--------------------------------------------------
000200080723     D* Procedure name: FatturaManuale_fat_init
000300080605     D* Purpose:        Inizializza il modulo.
000400080605     D* Parameter:      piKpjba => kpjba.
000500080605     D* Returns:        Esito.
000600080605     D*--------------------------------------------------
000700080723     D FatturaManuale_fat_init...
000800080605     D                 PR            10I 0
000900080605     D  piKpjba                            LIKEDS(kpjba)
001000080605
001100080605     D*--------------------------------------------------
001200080723     D* Procedure name: FatturaManuale_fat_finalize
001300080605     D* Purpose:        Chiude il modulo.
001400080605     D* Returns:
001500080605     D*--------------------------------------------------
001600080723     D FatturaManuale_fat_finalize...
001700080605     D                 PR
001800080605
001900080605     D*--------------------------------------------------
002000080723     D* Procedure name: FatturaManuale_fat_newSpoolFile
002100080605     D* Purpose:        Inizia un nuovo spool file.
002200080605     D* Returns:        Esito.
002300080611     D* Parameter:      piSocietaJob => Società lavoro.
002400080611     D* Parameter:      piFilialeRichiedente => Filiale richiedente.
002500080611     D* Parameter:      piUfficioRichiedente => Ufficio richiedente.
002600080611     D* Parameter:      piNumeroLista => Numero lista.
002700080612     D* Parameter:      piSiglaFirma => Sigla firma.
002800080605     D* Parameter:      piTotaleDocumenti => Numero totale dei documenti da...
002900080605     D*                           stampare.
003000080605     D* Parameter:      piParametriOVRPRTF => Parametri comando OVRPRTF.
003100080605     D*--------------------------------------------------
003200080723     D FatturaManuale_fat_newSpoolFile...
003300080605     D                 PR            10I 0
003400080611     D  piSocietaJob...
003500080611     D                                3A
003600080611     D  piFilialeRichiedente...
003700080611     D                                3P 0 CONST
003800080611     D  piUfficioRichiedente...
003900080611     D                                2A
004000080611     D  piNumeroLista...
004100080611     D                               10I 0 CONST
004200080612     D  piSiglaFirma...
004300080612     D                                2A   CONST
004400080605     D  piTotaleDocumenti...
004500080611     D                                5I 0 CONST
004600080605     D                                     OPTIONS(*NOPASS:*OMIT)
004700080605     D  piParametriOVRPRTF...
004800080605     D                              255A   VARYING
004900080605     D                                     CONST
005000080605     D                                     OPTIONS(*NOPASS:*OMIT)
005100080605
005200080605     D*--------------------------------------------------
005300080723     D* Procedure name: FatturaManuale_fat_endSpoolFile
005400080605     D* Purpose:        Chiude uno spool file.
005500080605     D* Returns:        Esito.
005600080605     D*--------------------------------------------------
005700080723     D FatturaManuale_fat_endSpoolFile...
005800080605     D                 PR            10I 0
005900080605
006000080605     D*--------------------------------------------------
006100080723     D* Procedure name: FatturaManuale_fat_newFattura
006200080605     D* Purpose:        Inizia la stampa di una nuova fattura.
006300080605     D* Returns:        Esito.
006400080605     D* Parameter:      piNumeroRichiesta => Numero richiesta.
006500080605     D*--------------------------------------------------
006600080723     D FatturaManuale_fat_newFattura...
006700080605     D                 PR            10I 0
006800080605     D  piNumeroRichiesta...
006900080605     D                               10I 0
007000080605
007100080605     D*--------------------------------------------------
007200080723     D* Procedure name: FatturaManuale_fat_prtFattura
007300080605     D* Purpose:        Stampa la fattura.
007400080605     D* Returns:        Esito.
007500080605     D*--------------------------------------------------
007600080723     D FatturaManuale_fat_prtFattura...
007700080605     D                 PR            10I 0
007800080605
