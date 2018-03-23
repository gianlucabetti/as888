000100080515     D*--------------------------------------------------
000200080515     D* Procedure name: Unita_init
000300080515     D* Purpose:        Inizializza il modulo.
000400080515     D* Returns:        Esito.
000500080515     D*--------------------------------------------------
000600080515     D Unita_init...
000700080515     D                 PR            10I 0
000800150511     D  piSocieta                     3
000900080515     D                                     CONST
001000080515     D                                     OPTIONS(*NOPASS:*OMIT)
001100080515     D  piDataValiditaSocieta...
001200080515     D                                 D   CONST
001300080515     D                                     OPTIONS(*NOPASS:*OMIT)
001400080515
001500080515     D*--------------------------------------------------
001600080515     D* Procedure name: Unita_isInit
001700080515     D* Purpose:        Restituisce *ON se il modulo è già stato inizializzato.
001800080515     D* Returns:        *ON  = modulo già inizializzato.
001900080515     D*                 *OFF = modulo non inizializzato.
002000080515     D*--------------------------------------------------
002100080515     D Unita_isInit...
002200080515     D                 PR              N
002300080515
002400080515     D*--------------------------------------------------
002500080515     D* Procedure name: Unita_getAnagraficaUnita
002600080515     D* Purpose:        Restituisce l'anagrafica di una filiale.
002700080515     D* Returns:        Esito.
002800080515     D* Parameter:      piUnita => Unita.
002900080515     D* Parameter:      piAnuni00f => Record AZORG00F.
003000080515     D*--------------------------------------------------
003100080515     D Unita_getAnagraficaUnita...
003200080515     D                 PR            10I 0
003300080515     D  piSocieta                          LIKE(anuni00f.uniSocieta)
003400080515     D                                     CONST
003500080515     D  piUnita                            LIKE(anuni00f.uniUnita)
003600080515     D                                     CONST
003700080515     D  piAnuni00f                         LIKEDS(anuni00f)
003800080515
003900080515     D*--------------------------------------------------
004000080515     D* Procedure name: Unita_getDescrizioneUnita
004100080515     D* Purpose:        Restituisce la descrizione della filiale.
004200080515     D* Returns:        Esito.
004300080515     D* Parameter:      piUnita => Unita.
004400080515     D* Parameter:      piDescrizioneUnita => Descrizione filiale.
004500080515     D*--------------------------------------------------
004600080515     D Unita_getDescrizioneUnita...
004700080515     D                 PR            10I 0
004800080515     D  piSocieta                          LIKE(anuni00f.uniSocieta)
004900080515     D                                     CONST
005000080515     D  piUnita                            LIKE(anuni00f.uniUnita)
005100080515     D                                     CONST
005200080515     D  piDescrizioneUnita...
005300080515     D                                     LIKE(anuni00f.uniDesBrev)
005400080515
005500080515     D*--------------------------------------------------
005600080515     D* Procedure name: Unita_finalize
005700080515     D* Purpose:        Chiude il modulo
005900080515     D*--------------------------------------------------
006000080515     D Unita_finalize...
006100080515     D                 PR
006200080515
006300080515     D*--------------------------------------------------
006400080515     D* Procedure name: Unita
006500080515     D* Purpose:        Per chiamare il modulo come programma.
006600080515     D*--------------------------------------------------
006700080515     D Unita...
006800080515     D                 PR                  EXTPGM('TIBSUNI00')
006900080515     D  rqsOpCode                    10A   CONST
007000080515     D  rpyEsito                     10I 0
007100080515     D  rqsData                      13A   OPTIONS(*NOPASS:*VARSIZE)
007200080515     D  rqsDataLen                   10I 0 CONST OPTIONS(*NOPASS)
007300080515     D  rpyData                      31A   OPTIONS(*NOPASS:*VARSIZE)
007400080515     D  rpyDataLen                   10I 0 CONST OPTIONS(*NOPASS)
007500080515
