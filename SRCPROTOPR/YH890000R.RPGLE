000100090521     D*--------------------------------------------------
000200091117     D* Procedure name: AvvisoBonificoDanni_Init
000300090521     D* Purpose:        Inizializza il modulo.
000400090521     D* Returns:        Esito.
000500090521     D* Parameter:      piKpjba
000600090521     D*--------------------------------------------------
000700091117     D AvvisoBonificoDanni_Init...
000800090521     D                 PR            10I 0
000900090521     D  piKpjba                            LIKEDS(kpjba)
001000091117     D  piSocietaLavoro...
001100090522     D                                     LIKE(andiz00f.societa)
001200090521
001300090521     D*--------------------------------------------------
001400091117     D* Procedure name: AvvisoBonificoDanni_Finalize
001500090521     D* Purpose:        Chiude il modulo.
001600090521     D* Returns:        Esito.
001700090521     D*--------------------------------------------------
001800091117     D AvvisoBonificoDanni_Finalize...
001900090521     D                 PR            10I 0
002000090521
002100090521     D*--------------------------------------------------
002200091117     D* Procedure name: AvvisoBonificoDanni_newAvviso
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
003500091117     D AvvisoBonificoDanni_newAvviso...
003600090521     D                 PR            10I 0
003700091117     D  piSocietaMittente...
003800091117     D                                     LIKE(cndizion.$soc)
003900091117     D                                     CONST
004000091117     D  piCapoContoDebito...
004100091117     D                                     LIKE(andiz00f.kcc)
004200091117     D                                     CONST
004300091117     D  piCodiceCliente...
004400090521     D                                     LIKE(andiz00f.ksc)
004500090521     D                                     VALUE
004600091117     D  piDataBonifico...
004700091117     D                                 D
004800091117     D                                     CONST
004900091117     D  piCodaEmissioneForzata...
005000090522     D                               21A   CONST
005100090522     D                                     OPTIONS(*NOPASS:*OMIT)
005200090529     D  piIndirizzoEmailDestinatario...
005300090529     D                              255A   CONST
005400090529     D                                     VARYING
005500090529     D                                     OPTIONS(*NOPASS:*OMIT)
005600090605     D  piIndirizzoEmailMittente...
005700090605     D                              255A   CONST
005800090605     D                                     VARYING
005900090605     D                                     OPTIONS(*NOPASS:*OMIT)
006000090522     D  piIndirizzoRiga1...
006100090522     D                               50A
006200090522     D                                     CONST
006300090522     D                                     OPTIONS(*NOPASS)
006400090522     D  piIndirizzoRiga2...
006500090522     D                               50A
006600090522     D                                     CONST
006700090522     D                                     OPTIONS(*NOPASS)
006800090522     D  piIndirizzoRiga3...
006900090522     D                               50A
007000090522     D                                     CONST
007100090522     D                                     OPTIONS(*NOPASS)
007200090522     D  piIndirizzoRiga4...
007300090522     D                               50A
007400090522     D                                     CONST
007500090522     D                                     OPTIONS(*NOPASS)
007600090522     D  piIndirizzoRiga5...
007700090522     D                               50A
007800090522     D                                     CONST
007900090522     D                                     OPTIONS(*NOPASS)
008000090522     D  piIndirizzoRiga6...
008100090522     D                               50A
008200090522     D                                     CONST
008300090522     D                                     OPTIONS(*NOPASS)
008400090522
008500090521     D*--------------------------------------------------
008600091117     D* Procedure name: AvvisoBonificoDanni_newAnomalia
008700091117     D* Purpose:        Nuova riga di anomalia.
008800090521     D* Returns:        Esito.
008900090521     D*--------------------------------------------------
009000091117     D AvvisoBonificoDanni_newAnomalia...
009100090521     D                 PR            10I 0
009200091117     D  piCaData...
009300091117     D                                 D   CONST
009400091117     D  piCaFiliale...
009500091117     D                                     LIKE(cndizion.$fil)
009600091117     D                                     CONST
009700091117     D  piCaNumero...
009800091117     D                                     LIKE(cndizion.$nca)
009900091117     D                                     CONST
010000091117     D  piPraticaAssicurativaDitta...
010100091117     D                                3A   CONST
010200091117     D  piPraticaAssicurativaAnno...
010300091117     D                                     LIKE(cndizion.$aas)
010400091117     D                                     CONST
010500091117     D  piPraticaAssicurativaNumero...
010600091117     D                                     LIKE(cndizion.$prn)
010700091117     D                                     CONST
010800091117     D  piNotaAddebitoNumero...
010900091117     D                                     LIKE(cndizion.$ndc)
011000091117     D                                     CONST
011100091117     D  piNotaAddebitoData...
011200091117     D                                     LIKE(cndizion.$ddc)
011300091117     D                                     CONST
011400091117     D  piImportoLiquidato...
011500091117     D                               15P 2 CONST
011600091117     D  piDivisa...
011700091117     D                                     LIKE(cndizion.$div)
011800091117     D                                     CONST
011900091117     D  piDestinatario...
012000091117     D                                     LIKE(cndizion.$rsd)
012100091117     D                                     CONST
012200091117     D  piRiferimentoMittenteNumerico...
012300091117     D                                     LIKE(cndizion.$rmn)
012400091117     D                                     CONST
012500091117     D  piPesoKili...
012600091117     D                                     LIKE(cndizion.$pkl)
012700091117     D                                     CONST
012800091117
012900090521     D*--------------------------------------------------
013000091117     D* Procedure name: AvvisoBonificoDanni_endAvviso
013100090521     D* Purpose:        Chiude la e-mail.
013200090521     D* Returns:        Esito.
013300090522     D* Parameter:      piCreareSpool => *ON = creare spool;
013400090522     D*                                  *OFF = non creare spool.
013500091117     D* Parameter:      piNumeroTotaleAnomalie => Numero totale delle scade...
013600090521     D*                          nze nella e-mail.
013700091117     D* Parameter:      piImportoTotaleAnomalie => Numero totale delle scad...
013800090521     D*                          enze nella e-mail.
013900090521     D*--------------------------------------------------
014000091117     D AvvisoBonificoDanni_endAvviso...
014100090521     D                 PR            10I 0
014200090522     D  piCreareSpool...
014300090521     D                                 N   CONST
014400091117     D  piNumeroTotaleAnomalie...
014500090521     D                               10U 0 OPTIONS(*NOPASS)
014600091117     D  piImportoTotaleAnomalie...
014700090521     D                               15P 2 OPTIONS(*NOPASS)
014800090521
014900090605     D*--------------------------------------------------
015000091117     D* Procedure name: AvvisoBonificoDanni_getIndirizzoEmailDestinatario
015100090605     D* Purpose:        Restituisce l'indirizzo e-mail del destinatario.
015200090605     D* Returns:        Indirizzo e-mail destinatario.
015300090605     D*--------------------------------------------------
015400091117     D AvvisoBonificoDanni_getIndirizzoEmailDestinatario...
015500090605     D                 PR           121A
015600090605
015700090605     D*--------------------------------------------------
015800091117     D* Procedure name: AvvisoBonificoDanni_getIndirizzoEmailMittente
015900090605     D* Purpose:        Restituisce l'indirizzo e-mail del mittente.
016000090605     D* Returns:        Indirizzo e-mail mittente.
016100090605     D*--------------------------------------------------
016200091117     D AvvisoBonificoDanni_getIndirizzoEmailMittente...
016300090605     D                 PR            30A
016400090605
