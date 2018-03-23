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
001400141013     D EmailMessaInMora_Init...
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
002600141013     D EmailMessaInMora_Finalize...
002700090226     D                 PR            10I 0
002800090226
002900090226     D*--------------------------------------------------
003000090226     D* Procedure name: EmailAvvisoScadenza_newEmail
003100090226     D* Purpose:        Inizia una nuova e-mail.
003200090226     D* Returns:        Esito.
003300090227     D* Parameter:      piFilialeCliente => Filiale.
003400090227     D* Parameter:      piCodiceCliente => Codice cliente.
003500090226     D*--------------------------------------------------
003600141013     D EmailMessaInMora_newEmail...
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
005400141013     D EmailMessaInMora_newPartita...
005500090227     D                 PR            10I 0
005600141013     D  piCodiceCliente...
005601141013     D                                8    CONST
005602141013     D  piNumeroPartita...
005603141013     D                                9P 0 CONST
005604141013     D  piDataPartita                  D   DATFMT(*ISO)
005605141013     D                                     CONST
005606141013     D  piSeriePartita...
005607141013     D                                4    CONST
005608141013     D  piImporto...
005609141013     D                               15P 2 CONST
005610141020     D  picip...
005611141020     D                                1  0 const
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
007600141013     D EmailMessaInMora_endEmail...
007700090227     D                 PR            10I 0
007800090227     D  piCreareEmail...
007900090227     D                                 N   CONST
008000090227     D  piNumeroTotaleScadenze...
008100150403     D                               10U 0 OPTIONS(*NOPASS:*omit)
008200090227     D  piImportoTotaleScadenze...
008300150403     D                               15P 2 OPTIONS(*NOPASS:*omit)
008301150403     D  piflag_logistica...
008302150403     D                                1    OPTIONS(*NOPASS:*omit)
008303150403
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
011300141013     D EmailMessaInMora_RunAvviso...
011400090303     D                 PR            10I 0
011500141013     D  piidproposta                 10I 0
012400090303
012500090305     D*--------------------------------------------------
012600090305     D* Procedure name: EmailAvvisoScadenza_GetNumeroAvvisiTotale
012700090305     D* Purpose:        Restituisce il numero totale degli avvisi.
012800090305     D* Returns:        Numero avvisi totale.
012900090305     D*--------------------------------------------------
013000141013     D EmailMessaInMora_GetNumeroAvvisiTotale...
013100090305     D                 PR            10I 0
013200090305
013300090305     D*--------------------------------------------------
013400090305     D* Procedure name: EmailAvvisoScadenza_GetNumeroAvvisiCreati
013500090305     D* Purpose:        Restituisce il numero degli avvisi creati.
013600090305     D* Returns:        Numero avvisi creati.
013700090305     D*--------------------------------------------------
013800141013     D EmailMessaInMora_GetNumeroAvvisiCreati...
013900090305     D                 PR            10I 0
014000090305
014001141118     D*--------------------------------------------------
014002141118     D* Procedure name: EmailAvvisoScadenza_GetNumeroAvvisiCreati
014003141118     D* Purpose:
014004141118     D* Returns:        Numero avvisi creati.
014005141118     D*--------------------------------------------------
014006141118     D EmailMessaInMora_GetMora...
014007141118     D                 PR            10I 0
014008141118
