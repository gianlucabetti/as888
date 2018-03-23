000100080430     D*--------------------------------------------------
000200080430     D* Procedure name: Organigramma_init
000300080430     D* Purpose:        Inizializza il modulo.
000400080430     D* Returns:        Esito.
000500080430     D*--------------------------------------------------
000600080430     D Organigramma_init...
000700080430     D                 PR            10I 0
000800091028     D  piKpjba                            LIKEDS(kpjba)
000900091028     D                                     OPTIONS(*NOPASS)
001000080430
001100080430     D*--------------------------------------------------
001200080430     D* Procedure name: Organigramma_isInit
001300080430     D* Purpose:        Restituisce *ON se il modulo è già stato inizializzato.
001400080430     D* Returns:        *ON  = modulo già inizializzato.
001500080430     D*                 *OFF = modulo non inizializzato.
001600080430     D*--------------------------------------------------
001700080430     D Organigramma_isInit...
001800080430     D                 PR              N
001900080430
002000080430     D*--------------------------------------------------
002100080430     D* Procedure name: Organigramma_getAnagraficaFiliale
002200080430     D* Purpose:        Restituisce l'anagrafica di una filiale.
002300080430     D* Returns:        Esito.
002400080430     D* Parameter:      piFiliale => Filiale.
002500080430     D* Parameter:      piAzorg00f => Record AZORG00F.
002600080430     D*--------------------------------------------------
002700080430     D Organigramma_getAnagraficaFiliale...
002800080430     D                 PR            10I 0
002900080430     D  piFiliale                          LIKE(azorg00f.orgFil)
003000080430     D                                     CONST
003100080430     D  piAzorg00f                         LIKEDS(azorg00f)
003200080430
003300080509     D*--------------------------------------------------
003400080509     D* Procedure name: Organigramma_getDescrizioneFiliale
003500080509     D* Purpose:        Restituisce la descrizione della filiale.
003600080509     D* Returns:        Esito.
003700080509     D* Parameter:      piFiliale => Filiale.
003800080509     D* Parameter:      piDescrizioneFiliale => Descrizione filiale.
003900080509     D*--------------------------------------------------
004000080509     D Organigramma_getDescrizioneFiliale...
004100080509     D                 PR            10I 0
004200080509     D  piFiliale                          LIKE(azorg00f.orgFil)
004300080509     D                                     CONST
004400080509     D  piDescrizioneFiliale...
004500080509     D                                     LIKE(azorg00f.orgDes)
004600091028
004700091028     D*--------------------------------------------------
004800091028     D* Procedure name: Organigramma_getIndirizzoFiliale
004900091028     D* Purpose:        Restituisce l'indirizzo di una filiale.
005000091028     D* Returns:        Esito.
005100091028     D* Parameter:      piFiliale => ID filiale.
005200091028     D* Parameter:      piIndirizzo => Indirizzo
005300091028     D* Parameter:      piCap => Codice avviamento postale.
005400091028     D* Parameter:      piLocalita => Località.
005500091028     D* Parameter:      piSiglaProvincia => Sigla provincia.
005600091028     D*--------------------------------------------------
005700091028     D Organigramma_getIndirizzoFiliale...
005800091028     D                 PR            10I 0
005900091028     D  piFiliale                          LIKE(azorg00f.orgFil)
006000091028     D                                     CONST
006100091028     D  piIndirizzo                        LIKE(azorg00f.orgInd)
006200091028     D  piCap                              LIKE(azorg00f.orgCpf)
006300091028     D  piLocalita                         LIKE(azorg00f.orgLoc)
006400091028     D  piSiglaProvincia...
006500091028     D                                     LIKE(azorg00f.orgPro)
006600080509
006700091028     D*--------------------------------------------------
006800091028     D* Procedure name: Organigramma_getRecapitiFiliale
006900091028     D* Purpose:        Restituisce telefono, fax, telex, indirizzo e-mail ...
007000091028     D*                          generico.
007100091028     D* Returns:        Esito
007200091028     D* Parameter:      piFiliale => ID filiale
007300091028     D* Parameter:      piTelefono => Telefono.
007400091028     D* Parameter:      piTelefax => Telefax.
007500091028     D* Parameter:      piTelex => Telex.
007600091028     D* Parameter:      piIndirizzoEmailInfo => Indirizzo e-mail info.
007700091028     D*--------------------------------------------------
007800091028     D Organigramma_getRecapitiFiliale...
007900091028     D                 PR            10I 0
008000091028     D  piFiliale                          LIKE(azorg00f.orgFil)
008100091028     D                                     CONST
008200091028     D  piTelefono                         LIKE(azorg00f.orgTel)
008300091029     D                                     OPTIONS(*OMIT)
008400091029     D  piTelefax                          LIKE(azorg00f.orgFax)
008500091029     D                                     OPTIONS(*OMIT)
008600091028     D  piTelex                            LIKE(azorg00f.orgTlx)
008700091029     D                                     OPTIONS(*OMIT)
008800091028     D  piIndirizzoEmailInfo...
008900091028     D                              255A   VARYING
009000091029     D                                     OPTIONS(*OMIT)
009100150415     D  piIndirizzoEmailAmministrazione...
009101150415     D                              255A   VARYING
009102150415     D                                     OPTIONS(*NOPASS)
009103150415
009200080430     D*--------------------------------------------------
009300080430     D* Procedure name: Organigramma_finalize
009400080430     D* Purpose:        Chiude il modulo
009500080430     D*--------------------------------------------------
009600080430     D Organigramma_finalize...
009700080515     D                 PR
009800080430
009900080430     D*--------------------------------------------------
010000080430     D* Procedure name: Organigramma
010100080430     D* Purpose:        Per chiamare il modulo come programma.
010200080430     D*--------------------------------------------------
010300080430     D Organigramma...
010400080430     D                 PR                  EXTPGM('TIBSORG00')
010500080430     D  rqsOpCode                    10A   CONST
010600080430     D  rpyEsito                     10I 0
010700080430     D  rqsData                       2A   OPTIONS(*NOPASS:*VARSIZE)
010800080430     D  rqsDataLen                   10I 0 CONST OPTIONS(*NOPASS)
010900080515     D  rpyData                    4282A   OPTIONS(*NOPASS:*VARSIZE)
011000080515     D  rpyDataLen                   10I 0 CONST OPTIONS(*NOPASS)
011100091028
