000100080627     D*--------------------------------------------------
000200080627     D* Procedure name: ISST_init
000300080627     D* Purpose:        Inizializza il modulo.
000400080627     D* Returns:        Esito.
000500080627     D*--------------------------------------------------
000600080627     D ISST_init       PR            10I 0
000700080627
000800080627     D*--------------------------------------------------
000900080627     D* Procedure name: ISST_finalize
001000080627     D* Purpose:        Chiude il modulo.
001100080627     D* Returns:
001200080627     D*--------------------------------------------------
001300080627     D ISST_finalize   PR
001400080627
001500080627     D*--------------------------------------------------
001600080627     D* Procedure name: ISST_newStringa
001700080627     D* Purpose:        Riceve la stringa da sottostringare.
001800080627     D* Returns:        Esito.
001900080627     D* Parameter:      piStringa => Stringa da sottostringare
002000080627     D* Parameter:      piStringaLen => Lunghezza stringa.
002100080627     D*--------------------------------------------------
002200080627     DISST_newStringa  PR            10I 0
002300080627     D  piStringa                 65535A   OPTIONS(*VARSIZE)
002400080627     D  piStringaLen                 10I 0 CONST
002500080627
002600080627     D*--------------------------------------------------
002700080627     D* Procedure name: ISST_getSottoStringa
002800080627     D* Purpose:        Restituisce la sottostringa.
002900080627     D* Returns:        Esito.
003000080627     D* Parameter:      piSottoStringaLen => Lunghezza sottostringa.
003100080627     D* Parameter:      piPosizioneProgressiva => Posizione progressiva all...
003200080627     D*                          'interno della stringa.
003300080627     D*--------------------------------------------------
003400080627     D ISST_getSottoStringa...
003500080627     D                 PR         65535A
003600080627     D  piSottoStringaLen...
003700080627     D                               10I 0 CONST
003800080714     D  piPosizioneProgressiva...
003900080714     D                               10I 0
004000080627
