000100080605     D*--------------------------------------------------
000200080606     D* Procedure name: FatturaManuale_lau_init
000300080605     D* Purpose:        Inizializza il modulo.
000400080605     D* Parameter:      piKpjba => kpjba.
000500080605     D* Returns:        Esito.
000600080605     D*--------------------------------------------------
000700080606     D FatturaManuale_lau_init...
000800080605     D                 PR            10I 0
000900080605     D  piKpjba                            LIKEDS(kpjba)
001000080605
001100080605     D*--------------------------------------------------
001200080606     D* Procedure name: FatturaManuale_lau_finalize
001300080605     D* Purpose:        Chiude il modulo.
001400080605     D* Returns:
001500080605     D*--------------------------------------------------
001600080606     D FatturaManuale_lau_finalize...
001700080605     D                 PR
001800080605
001900080605     D*--------------------------------------------------
002000080606     D* Procedure name: FatturaManuale_lau_newSpoolFile
002100080605     D* Purpose:        Inizia un nuovo spool file.
002200080605     D* Returns:        Esito.
002300080611     D* Parameter:      piSocietaJob => Società lavoro.
002400080611     D* Parameter:      piFilialeRichiedente => Filiale richiedente.
002500080611     D* Parameter:      piUfficioRichiedente => Ufficio richiedente.
002600080611     D* Parameter:      piNumeroLista => Numero lista.
002601080612     D* Parameter:      piSiglaFirma => Sigla firma.
002700080605     D* Parameter:      piTotaleDocumenti => Numero totale dei documenti da...
002800080605     D*                           stampare.
002900080605     D* Parameter:      piParametriOVRPRTF => Parametri comando OVRPRTF.
003000080605     D*--------------------------------------------------
003100080606     D FatturaManuale_lau_newSpoolFile...
003200080605     D                 PR            10I 0
003300080611     D  piSocietaJob...
003400080611     D                                3A
003500080611     D  piFilialeRichiedente...
003600080611     D                                3P 0 CONST
003700080611     D  piUfficioRichiedente...
003800080611     D                                2A
003900080611     D  piNumeroLista...
004000080611     D                               10I 0 CONST
004001080612     D  piSiglaFirma...
004002081118     D                                3A   CONST
004100080605     D  piTotaleDocumenti...
004200080611     D                                5I 0 CONST
004300080605     D                                     OPTIONS(*NOPASS:*OMIT)
004400080605     D  piParametriOVRPRTF...
004500080605     D                              255A   VARYING
004600080605     D                                     CONST
004700080605     D                                     OPTIONS(*NOPASS:*OMIT)
004800080605
004900080605     D*--------------------------------------------------
005000080606     D* Procedure name: FatturaManuale_lau_endSpoolFile
005100080605     D* Purpose:        Chiude uno spool file.
005200080605     D* Returns:        Esito.
005300080605     D*--------------------------------------------------
005400080606     D FatturaManuale_lau_endSpoolFile...
005500080605     D                 PR            10I 0
005600080605
005700080605     D*--------------------------------------------------
005800080606     D* Procedure name: FatturaManuale_lau_newFattura
005900080605     D* Purpose:        Inizia la stampa di una nuova fattura.
006000080605     D* Returns:        Esito.
006100080605     D* Parameter:      piNumeroRichiesta => Numero richiesta.
006200080605     D*--------------------------------------------------
006300080606     D FatturaManuale_lau_newFattura...
006400080605     D                 PR            10I 0
006500080605     D  piNumeroRichiesta...
006600081023     D                               10I 0 CONST
006700080605
006800080605     D*--------------------------------------------------
006900080606     D* Procedure name: FatturaManuale_lau_prtFattura
007000080605     D* Purpose:        Stampa la fattura.
007100080605     D* Returns:        Esito.
007200080605     D*--------------------------------------------------
007300080606     D FatturaManuale_lau_prtFattura...
007400080605     D                 PR            10I 0
007500080605
