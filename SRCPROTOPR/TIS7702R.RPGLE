000100090827     D*--------------------------------------------------
000200090914     D* Program name:   Verifica_gestione_giacenza
000300090914     D* Purpose:        Restituisce se x la bolla/ksu corrente è consentito gestire la giacenza
000400090827     D* Returns:
000500090902     D* Parameter:      rqsOpCode     => codice operativo richiesta.
000600090914     D* Parameter:      rqsAas        => anno           bolla
000700090914     D* Parameter:      rqsLnp        => linea partenza bolla
000800090914     D* Parameter:      rqsNrs        => numero serie   bolla
000900090914     D* Parameter:      rqsNsp        => numero         bolla
001000090914     D* Parameter:      rqsTbl        => tipo           bolla
001100090902     D* Parameter:      rqsKsu        => cliente unificante
001200090902     D* Parameter:      rqsSun        => strategi user number
001300090914     D* Parameter:      rqsFlgKscCcm  => flag KSC/CCM
001400090914     D* Parameter:      rpyFlgGiac    => flag Sì/No gestione giacenza
001500090914     D* Parameter:      rpyEsito      => esito richiesta
001600090827     D*--------------------------------------------------
001700090914     D Verifica_gestione_giacenza...
001800090914     D                 PR                  EXTPGM('TIS7702R')
001900090914     D  rqsOpCode                    10A   CONST
002000090914     D  rqsAas                             CONST LIKE(cndizion.$aas)
002100090914     D                                     OPTIONS(*NOPASS)
002200090914     D  rqsLnp                             CONST LIKE(cndizion.$lnp)
002300090914     D                                     OPTIONS(*NOPASS)
002400090914     D  rqsNrs                             CONST LIKE(cndizion.$nrs)
002500090914     D                                     OPTIONS(*NOPASS)
002600090914     D  rqsNsp                             CONST LIKE(cndizion.$nsp)
002700090914     D                                     OPTIONS(*NOPASS)
002800090914     D  rqsTbl                             CONST LIKE(cndizion.$tbl)
002900090914     D                                     OPTIONS(*NOPASS)
003000090914     D  rqsKsu                             CONST LIKE(cndizion.$ksu)
003100090914     D                                     OPTIONS(*NOPASS)
003200090914     D  rqsSun                             CONST LIKE(cndizion.$sun)
003300090914     D                                     OPTIONS(*NOPASS)
003400090914     D  rqsFlgKscCcm                  1A   CONST
003500090914     D                                     OPTIONS(*NOPASS)
003600090914     D  rpyFlgGiac                    1A   OPTIONS(*NOPASS)
003700090902     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
