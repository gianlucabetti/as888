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
002400080605     D* Parameter:      piParametriOVRPRTF => Parametri comando OVRPRTF.
002500080605     D*--------------------------------------------------
002600080723     D FatturaManuale_fat_newSpoolFile...
002700080605     D                 PR            10I 0
002800080611     D  piSocietaJob...
002900080611     D                                3A
003000080605     D  piParametriOVRPRTF...
003100080923     D                            32740A   VARYING
003200080605     D                                     CONST
003300080605     D                                     OPTIONS(*NOPASS:*OMIT)
003400081127     D  piTipoStampa...
003500081127     D                                1A
003600081127     D                                     CONST
003700081127     D                                     OPTIONS(*NOPASS:*OMIT)
003800080605
003900080605     D*--------------------------------------------------
004000080723     D* Procedure name: FatturaManuale_fat_endSpoolFile
004100160608     P* Purpose:        Chiude uno spool file e lo conferma/cancella.
004101160608     P* Returns:        Esito.
004102160608     P* Parameter:      piConfermaSpoolFile => *ON = spool file confermato;
004103160608     P*                                        *OFF = cancellare spool file.
004104160608     P*                                        *NOPASS = vale *ON
004105160608     D*--------------------------------------------------
004400080723     D FatturaManuale_fat_endSpoolFile...
004500080605     D                 PR            10I 0
004600160608     D  piConfermaSpoolFile...
004601160608     D                                 N   CONST OPTIONS(*NOPASS)
004602160608
004700080605     D*--------------------------------------------------
004800080723     D* Procedure name: FatturaManuale_fat_newFattura
004900080605     D* Purpose:        Inizia la stampa di una nuova fattura.
005000080605     D* Returns:        Esito.
005100080605     D* Parameter:      piNumeroRichiesta => Numero richiesta.
005200080605     D*--------------------------------------------------
005300080723     D FatturaManuale_fat_newFattura...
005400080605     D                 PR            10I 0
005500080605     D  piNumeroRichiesta...
005600080902     D                               10I 0 CONST
005700080605
005800080605     D*--------------------------------------------------
005900080723     D* Procedure name: FatturaManuale_fat_prtFattura
006000080605     D* Purpose:        Stampa la fattura.
006100080605     D* Returns:        Esito.
006200080605     D*--------------------------------------------------
006300080723     D FatturaManuale_fat_prtFattura...
006400080605     D                 PR            10I 0
006500080605
