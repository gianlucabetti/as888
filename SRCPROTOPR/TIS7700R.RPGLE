000100090827     D*--------------------------------------------------
000200090827     D* Program name: Selettore_bolla_subUnificante
000300090827     D* Purpose:        Esegue la selezione di una bolla per subunificante.
000400090827     D* Returns:
000500090827     D* Parameter:      rqsOpCode => Codice operativo richiesta.
000600090827     D* Parameter:      rpyEsito => Esito.
000700090827     D* Parameter:      rqsFormatName => Nome formato richiesta.
000800090827     D* Parameter:      rqsData => Dati richiesta.
000900090827     D* Parameter:      rqsDataSize => Dimensione dati richiesta.
001000090827     D* Parameter:      rpyFormatName => Nome formato risposta.
001100090827     D* Parameter:      rpyData => Dati risposta.
001200090827     D* Parameter:      rpyDataSize => Dimensione dati risposta.
001300090827     D*--------------------------------------------------
001400090827     D Selettore_bolla_subUnificante...
001500090827     D                 PR                  EXTPGM('TIS7700R')
001600090827     D  rqsOpCode                    10A   CONST
001700090827     D  rpyEsito                     10I 0 OPTIONS(*NOPASS)
001800090827     D  rqsFormatName                10A   CONST
001900090827     D                                     OPTIONS(*NOPASS)
002000090828     D  rqsData                      38A   OPTIONS(*NOPASS:*VARSIZE)
002100090827     D  rqsDataSize                  10I 0 CONST
002200090827     D                                     OPTIONS(*NOPASS)
002300090827     D  rpyFormatName                10A   CONST
002400090827     D                                     OPTIONS(*NOPASS)
002500090901     D  rpyData                      72A   OPTIONS(*NOPASS:*VARSIZE)
002600090827     D  rpyDataSize                  10I 0 CONST
002700090827     D                                     OPTIONS(*NOPASS)
