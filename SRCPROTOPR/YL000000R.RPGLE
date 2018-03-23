000100090521     D*--------------------------------------------------
000200090521     D* Procedure name: Sollecito_Init
000300090521     D* Purpose:        Inizializza il modulo.
000400090521     D* Returns:        Esito.
000500090521     D* Parameter:      piKpjba
000600090521     D*--------------------------------------------------
000700090521     D Sollecito_Init...
000800090521     D                 PR            10I 0
000900090521     D  piKpjba                            LIKEDS(kpjba)
001000090521     D  piSocieta...
001100090522     D                                     LIKE(andiz00f.societa)
001200090521
001300090521     D*--------------------------------------------------
001400090521     D* Procedure name: Sollecito_Finalize
001500090521     D* Purpose:        Chiude il modulo.
001600090521     D* Returns:        Esito.
001700090521     D*--------------------------------------------------
001800090521     D Sollecito_Finalize...
001900090521     D                 PR            10I 0
002000090521
002100090521     D*--------------------------------------------------
002200090521     D* Procedure name: Sollecito_newSollecito
002300090521     D* Purpose:        Inizia una nuova e-mail.
002400090521     D* Returns:        Esito.
002500090521     D* Parameter:      piFilialeCliente => Filiale.
002600090521     D* Parameter:      piCodiceCliente => Codice cliente.
002700090521     D* Parameter:      piCodaEmissioneForzata => Coda emissione forzata do...
002800090521     D*                          ve creare lo spool.
002900090521     D*                 *NOPASS = coda emissione per e-mail specificata della
003000090521     D*                           tabella MRA.
003100090521     D*                 *BLANK  = come *NOPASS
003200090521     D*                 '*JOB'  = coda emissione del lavoro.
003300090521     D*                 valore  = nome qualificato coda di emissione.
003400090521     D*--------------------------------------------------
003500090521     D Sollecito_newSollecito...
003600090521     D                 PR            10I 0
003700090521     D  piFilialeCliente...
003800090521     D                                     LIKE(cndizion.$fil)
003900090521     D                                     CONST
004000090521     D  piCodiceCliente...
004100090521     D                                     LIKE(andiz00f.ksc)
004200090521     D                                     VALUE
004300090522     D  piCodaEmissioneForzata...
004400090522     D                               21A   CONST
004500090522     D                                     OPTIONS(*NOPASS:*OMIT)
004600090529     D  piIndirizzoEmailDestinatario...
004700090529     D                              255A   CONST
004800090529     D                                     VARYING
004900090529     D                                     OPTIONS(*NOPASS:*OMIT)
005000090605     D  piIndirizzoEmailMittente...
005100090605     D                              255A   CONST
005200090605     D                                     VARYING
005300090605     D                                     OPTIONS(*NOPASS:*OMIT)
005400090522     D  piIndirizzoRiga1...
005500090522     D                               50A
005600090522     D                                     CONST
005700090522     D                                     OPTIONS(*NOPASS)
005800090522     D  piIndirizzoRiga2...
005900090522     D                               50A
006000090522     D                                     CONST
006100090522     D                                     OPTIONS(*NOPASS)
006200090522     D  piIndirizzoRiga3...
006300090522     D                               50A
006400090522     D                                     CONST
006500090522     D                                     OPTIONS(*NOPASS)
006600090522     D  piIndirizzoRiga4...
006700090522     D                               50A
006800090522     D                                     CONST
006900090522     D                                     OPTIONS(*NOPASS)
007000090522     D  piIndirizzoRiga5...
007100090522     D                               50A
007200090522     D                                     CONST
007300090522     D                                     OPTIONS(*NOPASS)
007400090522     D  piIndirizzoRiga6...
007500090522     D                               50A
007600090522     D                                     CONST
007700090522     D                                     OPTIONS(*NOPASS)
007800090522
007900090521     D*--------------------------------------------------
008000090521     D* Procedure name: Sollecito_newScadenza
008100090521     D* Purpose:        Nuova riga di scadenza.
008200090521     D* Returns:        Esito.
008300090521     D* Parameter:      piNumeroFattura => Numero fattura.
008400090521     D* Parameter:      piDataFattura => Data fattura.
008500090521     D* Parameter:      piDataScadenza => Data scadenza.
008600090521     D* Parameter:      piImportoScadenza => Importo scadenza.
008700090522     D* Parameter:      piCausale => Causale co.ge.
008800090521     D*--------------------------------------------------
008900090521     D Sollecito_newScadenza...
009000090521     D                 PR            10I 0
009100090521     D  piNumeroFattura...
009200090521     D                                9P 0 CONST
009300090521     D  piDataFattura                  D   DATFMT(*ISO)
009400090521     D                                     CONST
009500090521     D piDataScadenza                  D   DATFMT(*ISO)
009600090521     D                                     CONST
009700090521     D  piImportoScadenza...
009800090521     D                               15P 2 CONST
009900090522     D  piCausale...
010000090522     D                                     LIKE(andiz00f.causale)
010100090521
010200090521     D*--------------------------------------------------
010300090521     D* Procedure name: Sollecito_endSollecito
010400090521     D* Purpose:        Chiude la e-mail.
010500090521     D* Returns:        Esito.
010600090522     D* Parameter:      piCreareSpool => *ON = creare spool;
010700090522     D*                                  *OFF = non creare spool.
010800090521     D* Parameter:      piNumeroTotaleScadenze => Numero totale delle scade...
010900090521     D*                          nze nella e-mail.
011000090521     D* Parameter:      piImportoTotaleScadenze => Numero totale delle scad...
011100090521     D*                          enze nella e-mail.
011200090521     D*--------------------------------------------------
011300090521     D Sollecito_endSollecito...
011400090521     D                 PR            10I 0
011500090522     D  piCreareSpool...
011600090521     D                                 N   CONST
011700090521     D  piNumeroTotaleScadenze...
011800090521     D                               10U 0 OPTIONS(*NOPASS)
011900090521     D  piImportoTotaleScadenze...
012000090521     D                               15P 2 OPTIONS(*NOPASS)
012100090521
012200090605     D*--------------------------------------------------
012300090605     D* Procedure name: Sollecito_getIndirizzoEmailDestinatario
012400090605     D* Purpose:        Restituisce l'indirizzo e-mail del destinatario.
012500090605     D* Returns:        Indirizzo e-mail destinatario.
012600090605     D*--------------------------------------------------
012700090605     D Sollecito_getIndirizzoEmailDestinatario...
012800090605     D                 PR           121A
012900090605
013000090605     D*--------------------------------------------------
013100090605     D* Procedure name: Sollecito_getIndirizzoEmailMittente
013200090605     D* Purpose:        Restituisce l'indirizzo e-mail del mittente.
013300090605     D* Returns:        Indirizzo e-mail mittente.
013400090605     D*--------------------------------------------------
013500090605     D Sollecito_getIndirizzoEmailMittente...
013600090605     D                 PR            30A
013700090605
