000100090827     D*--------------------------------------------------
000200090902     D* Program name:   Selettore_record_TIVSS
000300090902     D* Purpose:        Esegue la selezione di un record d TIVSS in base al filtro ricevuto
000400090827     D* Returns:
000500090902     D* Parameter:      rqsOpCode     => codice operativo richiesta.
000600090902     D* Parameter:      rqsDeterminis => flag comportamento deterministico o meno
000700090902     D* Parameter:      rqsKsu        => cliente unificante
000800090902     D* Parameter:      rqsIsv        => tipo servizio
000900090902     D* Parameter:      rqsSun        => strategi user number
001000090902     D* Parameter:      rqsData       => data di riferimento
001100090902     D* Parameter:      rpyTIVSS      => buffer formato record TIVSS
001200090902     D* Parameter:      rpyEsito      => esito richiesta
001300090827     D*--------------------------------------------------
001400090902     D Selettore_record_TIVSS...
001500090902     D                 PR                  EXTPGM('TIS7701R')
001600090902     D  rqsOpCode                    10A   CONST
001700090902     D                                     OPTIONS(*NOPASS)
001800090902     D  rqsDeterminis                 1A   CONST
001900090902     D                                     OPTIONS(*NOPASS)
002000090902     D  rqsKsu                             CONST LIKE(tivssds.vssksu)
002100090902     D                                     OPTIONS(*NOPASS)
002200090902     D  rqsIsv                             CONST LIKE(tivssds.vssisv)
002300090902     D                                     OPTIONS(*NOPASS)
002400090902     D  rqsSun                             CONST LIKE(tivssds.vsssun)
002500090902     D                                     OPTIONS(*NOPASS)
002600090902     D  rqsData                            CONST LIKE(tivssds.vssdde)
002700090902     D                                     OPTIONS(*NOPASS)
002800090902     D  rpyTIVSS                           LIKEDS(tivssds)
002900090902     D                                     OPTIONS(*NOPASS)
003000090902     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
