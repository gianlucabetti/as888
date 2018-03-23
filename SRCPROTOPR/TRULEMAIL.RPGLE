000100070413     D*--------------------------------------------------
000200070413     D* Procedure name: email_setAddressFromUser
000300070413     D* Purpose:        Imposta indirizzo associato all'utente
000400070413     D* Returns:        Esito.
000500070413     D* Parameter:      utente => Utente
000600070413     D* Parameter:      rpyOpCode => Codice operativo.
000700070413     D* Parameter:      rpyStatus => Codice stato.
000800070413     D* Parameter:      rpyEsito => Esito.
000900070413     D*--------------------------------------------------
001000070413     Demail_setAddressFromUser...
001100070413     D                 PR            10I 0
001200070413     D  rqsUtente                    10A   CONST
001300070413     D  rqsAddReplace                 1A   CONST
001400070413     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
001500070413     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
001600070413     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
001700070413
001800070413     D*--------------------------------------------------
001900070413     D* Procedure name: email_addAddress
002000070413     D* Purpose:        Aggiunge un indirizzo e-mail.
002100070413     D* Returns:        Esito.
002200070413     D* Parameter:      rqsIndirizzo
002300070413     D*--------------------------------------------------
002400070413     D email_addAddress...
002500070413     D                 PR            10I 0
002600070413     D  rqsIndirizzo                   *   VALUE
002700070413     D                                     OPTIONS(*STRING:*TRIM)
002800070413     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002900070413     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
003000070413     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003100070413
003200070413     D*--------------------------------------------------
003300070413     D* Procedure name: email_send
003400070413     D* Purpose:        Invia e-mail ad un utente.
003500070413     D* Returns:        Esito.
003600070413     D* Parameter:      rqsOggetto => Oggetto
003700070413     D* Parameter:      rqsMessaggio => Messaggio.
003800070413     D* Parameter:      rpyOpCode => Codice operativo.
003900070413     D* Parameter:      rpyStatus => Codice stato.
004000070413     D* Parameter:      rpyEsito => Esito.
004100070413     D*--------------------------------------------------
004200070413     D email_send      PR            10I 0
004300070413     D  rqsOggetto                     *   VALUE OPTIONS(*STRING:*TRIM)
004400070413     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
004500070413     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
004600070413     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
004700070413     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
004800070413
004900070413     D*--------------------------------------------------
005000070413     D* Procedure name: email_sndBrkMsg
005100070413     D* Purpose:        Invia un messaggio interruzione agli utenti.
005200070413     D* Returns:        Esito.
005300070413     D* Parameter:      messaggio => Testo del messaggio
005400070413     D*--------------------------------------------------
005500070413     Demail_sndBrkMsg  PR            10I 0
005600070413     D  rqsMessaggio                   *   VALUE OPTIONS(*STRING:*TRIM)
005700070413     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
005800070413     D  rpyStatus                    10I 0 OPTIONS(*NOPASS:*OMIT)
005900070413     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
006000070413
