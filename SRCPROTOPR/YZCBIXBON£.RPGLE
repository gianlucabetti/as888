000100110525     D*--------------------------------------------------
000200110525     D* Procedure name: YZCBI2BON_Init
000300110525     D* Purpose:        Inizializzazione modulo.
000400110525     D* Returns:        Esito.
000500110525     D* Parameter:      priKpjba
000600110525     D*--------------------------------------------------
000700110525     D YZCBI2BON_Init  PR            10I 0
000800110525     D  priKpjba                           LIKEDS(kpjba)
000900110525
001000110525     D*--------------------------------------------------
001100110525     D* Procedure name: YZCBI2BON_Finalize
001200110525     D* Purpose:
001300110525     D* Returns:
001400110525     D*--------------------------------------------------
001500110525     D YZCBI2BON_Finalize...
001600110601     D                 PR            10I 0
001700110525
001800110526     D*--------------------------------------------------
001900110526     D* Procedure name: YZCBI2BON_RunRigaCBI
002000110526     D* Purpose:        Elabora una riga CBI.
002100110526     D* Returns:        Esito.
002200110526     D* Parameter:      priRigaCBI
002300110526     D*--------------------------------------------------
002400110526     D YZCBI2BON_RunRigaCBI...
002500110526     D                 PR            10I 0
002600110526     D  priRigaCBI                  120A
002700110526
002800110607     D*--------------------------------------------------
002900110607     D* Procedure name: YZCBI2BON_NewFileCBI
003000110607     D* Purpose:        Nuovo file CBI.
003100110607     D* Returns:        Esito.
003200110607     D* Parameter:      priStreamFile => Nome file di flusso.
003300110607     D*--------------------------------------------------
003400110607     D YZCBI2BON_NewFileCBI...
003500110607     D                 PR            10I 0
003600110607     D  priStreamFile               255A   VARYING
003700110607     D                                     CONST
003800110607
003900110607     D*--------------------------------------------------
004000110607     D* Procedure name: YZCBI2BON_RunFileCBI
004100110607     D* Purpose:        Elabora il file CBI già aperto.
004200110607     D* Returns:        Esito
004300110607     D*--------------------------------------------------
004400110607     D YZCBI2BON_RunFileCBI...
004500110607     D                 PR            10I 0
004600110607
004700110607     D*--------------------------------------------------
004800110607     D* Procedure name: YZCBI2BON_EndFileCBI
004900110607     D* Purpose:        Fine file CBI.
005000110607     D* Returns:        Esito
005100110607     D* Parameter:      priCommit => *ON = commit; *OFF = rollback;...
005200110607     D*                           *NOPASS = niente.
005300110607     D*--------------------------------------------------
005400110607     D YZCBI2BON_EndFileCBI...
005500110607     D                 PR            10I 0
005600110607     D  priCommit...
005700110607     D                                 N   CONST
005800110607     D                                     OPTIONS(*NOPASS)
005900110607
006000110610     D*--------------------------------------------------
006100110610     D* Procedure name: YZCBI2BON_GetMessaggio
006200110610     D* Purpose:        Restituisce il messaggio di errore.
006300110610     D* Returns:        Messaggio di errore.
006400110610     D*--------------------------------------------------
006500110610     D YZCBI2BON_GetMessaggio...
006600110610     D                 PR           255A   VARYING DIM(255)
006700110610     D  priNumeroMessaggi...
006800110610     D                                3U 0
006900110613     D  priPulireMessaggi...
007000110613     D                                 N   CONST OPTIONS(*NOPASS)
007100110613
007200110613     D*--------------------------------------------------
007300110614     D* Procedure name: YZCBI2BON_RunAllFilesCBI
007400110613     D* Purpose:        Elabora tutti i files CBI presenti nella directory.
007500110613     D* Returns:        Esito
007600110613     D* Parameter:      priDirectory
007700110613     D*--------------------------------------------------
007800110614     D YZCBI2BON_RunAllFilesCBI...
007900110613     D                 PR            10I 0
008000110613     D  priDirectory                255A   VARYING
008100110613     D                                     CONST
008200110613     D                                     OPTIONS(*NOPASS)
008300110613
