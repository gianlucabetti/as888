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
001400090226     D EmailAvvisoScadenza_Init...
001500090226     D                 PR            10I 0
001600090226     D  piKpjba                            LIKEDS(kpjba)
001700090227     D  piCodaEmissioneForzata...
001800090227     D                               21A   CONST
001900090227     D                                     OPTIONS(*NOPASS)
002000090226
002100090226     D*--------------------------------------------------
002200090226     D* Procedure name: EmailAvvisoScadenza_Finalize
002300090226     D* Purpose:        Chiude il modulo.
002400090226     D* Returns:        Esito.
002500090226     D*--------------------------------------------------
002600090226     D EmailAvvisoScadenza_Finalize...
002700090226     D                 PR            10I 0
002800090226
002900090226     D*--------------------------------------------------
003000090226     D* Procedure name: EmailAvvisoScadenza_newEmail
003100090226     D* Purpose:        Inizia una nuova e-mail.
003200090226     D* Returns:        Esito.
003300090227     D* Parameter:      piFilialeCliente => Filiale.
003400090227     D* Parameter:      piCodiceCliente => Codice cliente.
003500090226     D*--------------------------------------------------
003600090226     D EmailAvvisoScadenza_newEmail...
003700090226     D                 PR            10I 0
003800090227     D  piFilialeCliente...
003900090227     D                                     LIKE(cndizion.$fil)
004000090227     D                                     CONST
004100090227     D  piCodiceCliente...
004200090227     D                                     LIKE(andiz00f.ksc)
004300090227     D                                     VALUE
004400090226
004500090227     D*--------------------------------------------------
004600090227     D* Procedure name: EmailAvvisoScadenza_newScadenza
004700090227     D* Purpose:        Nuova riga di scadenza.
004800090227     D* Returns:        Esito.
004900090227     D* Parameter:      piNumeroFattura => Numero fattura.
005000090227     D* Parameter:      piDataFattura => Data fattura.
005100090227     D* Parameter:      piDataScadenza => Data scadenza.
005200090227     D* Parameter:      piImportoScadenza => Importo scadenza.
005300090227     D*--------------------------------------------------
005400090227     D EmailAvvisoScadenza_newScadenza...
005500090227     D                 PR            10I 0
005600090227     D  piNumeroFattura...
005700090227     D                                9P 0 CONST
005800090227     D  piDataFattura                  D   DATFMT(*ISO)
005900090227     D                                     CONST
006000090227     D piDataScadenza                  D   DATFMT(*ISO)
006100090227     D                                     CONST
006200090227     D  piImportoScadenza...
006300090227     D                               15P 2 CONST
006400090227
006500090227     D*--------------------------------------------------
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
007600090227     D EmailAvvisoScadenza_endEmail...
007700090227     D                 PR            10I 0
007800090227     D  piCreareEmail...
007900090227     D                                 N   CONST
008000090227     D  piNumeroTotaleScadenze...
008100090227     D                               10U 0 OPTIONS(*NOPASS)
008200090227     D  piImportoTotaleScadenze...
008300090227     D                               15P 2 OPTIONS(*NOPASS)
008400090227
008500090302     D*--------------------------------------------------
008600090302     D* Procedure name: EmailAvvisoScadenza_NewScadenzarioProj
008700090302     D* Purpose:        Esegue il calcolo dello scadenzario di Proj di tutt...
008800090302     D*                          i i clienti.
008900090302     D* Returns:        Esito.
009000090302     D*--------------------------------------------------
009100090302     D EmailAvvisoScadenza_NewScadenzarioProj...
009200090302     D                 PR            10I 0
009300090302
009400090302     D*--------------------------------------------------
009500090302     D* Procedure name: EmailAvvisoScadenza_RunAvvisi
009600090302     D* Purpose:        Esegue l'elaborazione degli avvisi di scadenza di t...
009700090302     D*                          utti i clienti validi.
009800090302     D* Returns:        Esito.
009900090302     D*--------------------------------------------------
010000090302     D EmailAvvisoScadenza_RunAvvisi...
010100090302     D                 PR            10I 0
010200090302
010300090303     D*--------------------------------------------------
010400090303     D* Procedure name: EmailAvvisoScadenza_RunAvviso
010500090303     D* Purpose:        Elabora l'avviso di un cliente.
010600090303     D* Returns:        Esito.
010700090303     D* Parameter:      piSocieta => Società.
010800090303     D* Parameter:      piContabilita => Contabilità.
010900090303     D* Parameter:      piTipoScadenzario => Tipo scadenzario.
011000090303     D* Parameter:      piCapoContoCliente => Capo conto cliente.
011100090303     D* Parameter:      piSottoContoCliente => Sotto conto cliente.
011200090303     D*--------------------------------------------------
011300090303     D EmailAvvisoScadenza_RunAvviso...
011400090303     D                 PR            10I 0
011500090303     D  piSocieta                          LIKE(andiz00f.societa)
011600090303     D  piContabilita                      LIKE(andiz00f.ctb)
011700090303     D                                     CONST
011800090303     D  piTipoScadenzario...
011900090303     D                                1A   CONST
012000090303     D  piCapoContoCliente...
012100090303     D                                     LIKE(andiz00f.kcc)
012200090303     D  piSottoContoCliente...
012300090303     D                                     LIKE(andiz00f.ksc)
012400090303
012500090305     D*--------------------------------------------------
012600090305     D* Procedure name: EmailAvvisoScadenza_GetNumeroAvvisiTotale
012700090305     D* Purpose:        Restituisce il numero totale degli avvisi.
012800090305     D* Returns:        Numero avvisi totale.
012900090305     D*--------------------------------------------------
013000090305     D EmailAvvisoScadenza_GetNumeroAvvisiTotale...
013100090305     D                 PR            10I 0
013200090305
013300090305     D*--------------------------------------------------
013400090305     D* Procedure name: EmailAvvisoScadenza_GetNumeroAvvisiCreati
013500090305     D* Purpose:        Restituisce il numero degli avvisi creati.
013600090305     D* Returns:        Numero avvisi creati.
013700090305     D*--------------------------------------------------
013800090305     D EmailAvvisoScadenza_GetNumeroAvvisiCreati...
013900090305     D                 PR            10I 0
014000090305
