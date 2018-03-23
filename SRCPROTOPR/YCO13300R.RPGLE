000100090226     D*--------------------------------------------------
000200090226     D* Procedure name: EmailAvvisoScadenza_Init
000300090226     D* Purpose:        Inizializza il modulo.
000400090226     D* Returns:        Esito.
000500090226     D* Parameter:      piKpjba
000600090227     D* Parameter:      piCodaEmissioneForzata => Coda emissione forzata do...
000700090227     D*                          ve creare lo spool.
000800090227     D*                 *NOPASS = coda emissione per e-mail specificata della
000900090227     D*                           tabella MRA.
001000090227     D*                 *BLANK  = come *NOPASS
001100090227     D*                 '*JOB'  = coda emissione del lavoro.
001200090227     D*                 valore  = nome qualificato coda di emissione.
001300090226     D*--------------------------------------------------
001400150213     D EmailSollecito_Init...
001500090226     D                 PR            10I 0
001600090226     D  piKpjba                            LIKEDS(kpjba)
001601141013     D  piCodaEmissioneForzata...
001602141013     D                               21A   CONST
001603141013     D                                     OPTIONS(*NOPASS)
001700141013
002100090226     D*--------------------------------------------------
002200090226     D* Procedure name: EmailAvvisoScadenza_Finalize
002300090226     D* Purpose:        Chiude il modulo.
002400090226     D* Returns:        Esito.
002500090226     D*--------------------------------------------------
002600150213     D EmailSollecito_Finalize...
002700090226     D                 PR            10I 0
002800090226
002900090226     D*--------------------------------------------------
003000090226     D* Procedure name: EmailAvvisoScadenza_newEmail
003100090226     D* Purpose:        Inizia una nuova e-mail.
003200090226     D* Returns:        Esito.
003300150217     D* Parameter:      piCodiceCliente => Codice cliente.
003500090226     D*--------------------------------------------------
003600150213     D EmailSollecito_newEmail...
003700090226     D                 PR            10I 0
003800150225     D  piCodiceCliente...
004300150225     D                                8A   CONST
004301150218     D piTiposollecito...
004302150225     D                                1A   CONST
004303150227     D  piIndirizzoEmailDestinatario...
004304150227     D                              255A   CONST
004305150227     D                                     VARYING
004306150227     D                                     OPTIONS(*NOPASS:*OMIT)
004307150227     D  piIndirizzoEmailMittente...
004308150227     D                              255A   CONST
004309150227     D                                     VARYING
004310150227     D                                     OPTIONS(*NOPASS:*OMIT)
004400150217     D*--------------------------------------------------
004600090227     D* Procedure name: EmailAvvisoScadenza_newScadenza
004700090227     D* Purpose:        Nuova riga di scadenza.
004800090227     D* Returns:        Esito.
004900150217     D*--------------------------------------------------
005400150217     D EmailSollecito_newScadenza...
005500090227     D                 PR            10I 0
005600150217     D  piNumeroPartita...
005601150217     D                                9P 0 CONST
005602150217     D  piDataPartita                  D   DATFMT(*ISO)
005603150217     D                                     CONST
005604150217     D  piSeriePartita...
005605150217     D                                4    CONST
005606150217     D  piDataScadenza...
005607150217     D                                 D   DATFMT(*ISO)
005608150217     D                                     CONST
005609150217     D  piImporto...
005610150217     D                               15P 2 CONST
005613150217     D*--------------------------------------------------
006600090227     D* Procedure name: EmailAvvisoScadenza_endEmail
006700090227     D* Purpose:        Chiude la e-mail.
006800090227     D* Returns:        Esito.
006900090227     D* Parameter:      piCreareEmail => *ON = creare e-mail;
007000090227     D*                                  *OFF = non creare e-mail.
007100090227     D* Parameter:      piNumeroTotaleScadenze => Numero totale delle scade...
007200090227     D*                          nze nella e-mail.
007300090227     D* Parameter:      piImportoTotaleScadenze => Numero totale delle scad...
007400090227     D*                          enze nella e-mail.
007500090227     D*--------------------------------------------------
007600150213     D EmailSollecito_endEmail...
007700090227     D                 PR            10I 0
007800090227     D  piCreareEmail...
007900090227     D                                 N   CONST
008000090227     D  piNumeroTotaleScadenze...
008100090227     D                               10U 0 OPTIONS(*NOPASS)
008200090227     D  piImportoTotaleScadenze...
008300090227     D                               15P 2 OPTIONS(*NOPASS)
008400150217     D*--------------------------------------------------
011504150213     D* Procedure name: EmailSollecito_PrtBancheBonifico
011505150213     D* Purpose:        Stampa le banche per il bonifico.
011506150213     D* Returns:        Esito.
011507150213     D*--------------------------------------------------
011508150213     D EmailSollecito_PrtBancheBonifico...
011509150213     D                 PR            10I 0
