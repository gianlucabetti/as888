000100080827     D*--------------------------------------------------
000200080827     P*?Procedure name: TIBS6920R Recapito Postale  ?
000300080827     D* Purpose:        Restituisce la descrizione del cliente.
000400080827     D* Returns:        Descrizione profilo utente.
000500080827     D* Parameter:      piUtente => Profilo utente.
000600080827     D*--------------------------------------------------
000700080827     D tibs6920r       PR                  EXTPGM('TIBS6920R')
000800080827     D  rqsSocieta                    3a
000900080827     D  rqsSNatura                    1a   CONST
001000080827     D  rqsKsc                        8a
001100080827     D  rqsDtRif                       D   CONST
001200080827     D  rqsDtPar                       D
001300080827     D  rqsNrPar                      9p 0
001400080827     D  rqsSerPar                     4a
001500080827     D  rpyEsito                     10I 0
001600080827     D  rpyOpCode                    10A
001700080827     D  rpyStatus                    10I 0
001800080827     D  rpyRagioneSociale...
001900080827     D                               44a
002000080827     D  rpyDescrizioneAggiuntiva...
002100080827     D                               44a
002200080827     D  rpyIndirizzo...
002300080827     D                               44a
002400080827     D  rpyCap...
002500080827     D                                9a
002600080827     D  rpyLocalita...
002700080827     D                               35a
002800080827     D  rpySiglaProvincia...
002900080827     D                                2a
003000080827     D  rpySiglaNazione...
003100080827     D                                3a
