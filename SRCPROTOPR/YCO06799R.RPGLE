000100100129     D*--------------------------------------------------
000200100129     D* Procedure name: IntrastatServizi_Init
000300100129     D* Purpose:        Inizializza il modulo.
000400100129     D* Returns:        Esito.
000500100129     D* Parameter:      piKpjba
000600100129     D*--------------------------------------------------
000700100129     D IntrastatServizi_Init...
000800100129     D                 PR            10I 0
000900100129     D  piKpjba                            LIKEDS(kpjba)
001000100129     D  piAvviareControlloSincronizzazione...
001100100129     D                                 N   CONST
001200100129     D                                     OPTIONS(*NOPASS)
001300100129
001400100129     D*--------------------------------------------------
001500100129     D* Procedure name: IntrastatServizi_Finalize
001600100129     D* Purpose:        Chiusura modulo.
001700100129     D* Returns:
001800100129     D* Parameter:      piCommit => *ON = COMMIT; *OFF = ROLLBACK.
001900100129     D*--------------------------------------------------
002000100129     D IntrastatServizi_Finalize...
002100100129     D                 PR
002200100129     D  piCommit                       N   CONST
002300100129
002400100129     D*--------------------------------------------------
002500100129     D* Procedure name: IntrastatServizi_NewDettaglio
002600100129     D* Purpose:        Nuovo dettaglio.
002700100129     D* Returns:        Esito.
002800100129     D* Parameter:      piRegistro => Registro.
002900100129     D* Parameter:      piSezione => Sezione.
003000100129     D* Parameter:      piSerieDocumento => Serie documento.
003100100129     D* Parameter:      piNumeroDocumento => Numero documento.
003200100129     D* Parameter:      piDataDocumento => Data documento.
003300100208     D* Parameter:      piNazioneIvaSoggetto => Nazione ISO2 soggetto.
003400100129     D* Parameter:      piCodiceIvaSoggetto => Codice IVA soggetto.
003500100129     D* Parameter:      piImportoEuro => Importo in euro.
003600100129     D* Parameter:      piImportoDivisa => Importo in divisa.
003700100129     D* Parameter:      piCodiceServizio => Codice servizio.
003800100129     D* Parameter:      piSegno => Segno (+/-).
003900100129     D* Parameter:      piNumeroAssolutoRegistrazione => Numero assoluto re...
004000100129     D*                          gistrazione.
004100100129     D* Parameter:      piSerieFattura => Serie fattura.
004200100129     D* Parameter:      piNumeroFattura => Numero fattura.
004300100129     D* Parameter:      piDataFattura => Data fattura.
004400100129     D* Parameter:      piModalitaErogazione => Modalità di erogazione.
004500100129     D* Parameter:      piModalitaIncasso => Modalità di incasso.
004600100129     D* Parameter:      piPaesePagamento => Paese di pagamento.
004700100129     D*--------------------------------------------------
004800100129     D IntrastatServizi_NewDettaglio...
004900100129     D                 PR            10I 0
005000100129     D  piRegistro                    1A   CONST
005100100129     D  piSezione                     1A   CONST
005200100129     D  piSerieDocumento...
005300100129     D                                     LIKE(andiz00f.serieNm)
005400100129     D                                     CONST
005500100129     D  piNumeroDocumento...
005600100129     D                                     LIKE(andiz00f.nr)
005700100129     D                                     CONST
005800100129     D  piDataDocumento...
005900100129     D                                 D   DATFMT(*ISO)
006000100129     D                                     CONST
006100100208     D  piNazioneIvaSoggetto...
006200100204     D                                2A   VALUE
006300100129     D  piCodiceIvaSoggetto...
006400100129     D                               12A   CONST
006500100129     D  piImportoEuro                15P 2 CONST
006600100129     D  piImportoDivisa...
006700100129     D                               15P 2 VALUE
006800100129     D  piCodiceServizio...
006900100129     D                                6S 0 CONST
007000100129     D  piSegno                       1A   CONST
007100100129     D  piNumeroAssolutoRegistrazione...
007200100129     D                                     LIKE(andiz00f.nr)
007300100129     D                                     CONST
007400100129     D piSerieFattura                      LIKE(andiz00f.serieNm)
007500100129     D                                     VALUE
007600100129     D  piNumeroFattura...
007700100129     D                                     LIKE(andiz00f.nr)
007800100129     D                                     VALUE
007900100129     D  piDataFattura                  D   DATFMT(*ISO)
008000100129     D                                     VALUE
008100100129     D  piModalitaErogazione...
008200100129     D                                1A   CONST
008300100129     D  piModalitaIncasso...
008400100129     D                                1A   CONST
008500100129     D  piPaesePagamento...
008600100129     D                                2A   CONST
008700100202
008800100202     D*--------------------------------------------------
008900100202     D* Procedure name: IntrastatServizi_GetInfoByRegistrazione
009000100202     D* Purpose:        Restituisce informazioni Intrastat da una registraz...
009100100202     D*                          ione IVA.
009200100202     D* Returns:        Esito.
009300100202     D* Parameter:      piSistema => Sistema registrazione.
009400100202     D* Parameter:      piNumeroAssolutoRegistrazione => Numero assoluto re...
009500100202     D*                          gistrazione.
009600100202     D* Parameter:      piSerieRegistrazione => Serie registrazione.
009700100202     D* Parameter:      piDataRegistrazione => Data registrazione.
009800100202     D* Parameter:      piNumeroRegistrazione => Numero registrazione.
009900100202     D* Parameter:      piTipoDocumento => Tipo documento: 'F'=Fattura; 'A'...
010000100202     D*                          = nota Accredito.
010100100202     D* Parameter:      piAmmontareDelleOperazioniEuro => Ammontare delle o...
010200100202     D*                          perazioni in euro
010300100202     D* Parameter:      piAmmontareDelleOperazioniDivisa => Ammontare delle...
010400100202     D*                           operazioni in divisa.
010500100202     D* Parameter:      piDivisa => Divisa.
010600100202     D* Parameter:      piServizio => Codice servizio.
010700100202     D* Parameter:      piSerieRegistrazioneFattura => Serie registrazione ...
010800100202     D*                          fattura.
010900100202     D* Parameter:      piDataRegistrazioneFattura => Data registrazione fa...
011000100202     D*                          ttura.
011100100202     D* Parameter:      piNumeroRegistrazioneFattura => Numero registrazion...
011200100202     D*                          e fattura.
011300100202     D* Parameter:      piNazioneIvaSoggetto => Nazione IVA del soggetto
011400100202     D* Parameter:      piCodiceIvaSoggetto => Codice IVA del soggetto
011500100202     D* Parameter:      piModalitaIncasso => Modalità di incasso.
011600100202     D* Parameter:      piPaesePagamento => Paese di pagamento.
011700100202     D*--------------------------------------------------
011800100202     D IntrastatServizi_GetInfoByRegistrazione...
011900100202     D                 PR            10I 0
012000100202     D  piSistemaRegistrazione...
012100100202     D                                     LIKE(andiz00f.sys)
012200100202     D                                     CONST
012300100202     D  piNumeroAssolutoRegistrazione...
012400100202     D                                     LIKE(andiz00f.nr)
012500100202     D                                     CONST
012600100202     D  piSerieRegistrazione...
012700100202     D                                     LIKE(andiz00f.serieNm)
012800100202     D  piDataRegistrazione...
012900100202     D                                 D   DATFMT(*ISO)
013000100202     D  piNumeroRegistrazione...
013100100202     D                                     LIKE(andiz00f.nr)
013200100202     D  piTipoDocumento...
013300100202     D                                1A
013400100202     D  piAmmontareDelleOperazioniEuro...
013500100202     D                               15P 2
013600100202     D  piAmmontareDelleOperazioniDivisa...
013700100202     D                               15P 2
013800100202     D  piDivisa                           LIKE(andiz00f.divisa)
013900100202     D  piServizio                    6S 0
014000100202     D  piSerieRegistrazioneFattura...
014100100202     D                                     LIKE(andiz00f.serieNm)
014200100202     D  piDataRegistrazioneFattura...
014300100202     D                                 D   DATFMT(*ISO)
014400100202     D  piNumeroRegistrazioneFattura...
014500100202     D                                     LIKE(andiz00f.nr)
014600100202     D  piNazioneIvaSoggetto...
014700100202     D                                2A
014800100202     D  piCodiceIvaSoggetto...
014900100202     D                               12A
015000100202     D  piModalitaIncasso...
015100100202     D                                1A
015200100202     D  piPaesePagamento...
015300100202     D                                2A
015400100205     D  piModalitaErogazione...
015500100205     D                                1A
015600100205     D  piSezione...
015700100205     D                                1A
015800100205     D  piSegno...
015900100205     D                                1A
016000100205
016100100202     D*--------------------------------------------------
016200100202     D* Procedure name: IntrastatServizi_GetIdRegistrazioneFattura
016300100202     D* Purpose:        Restituisce l'ID della registrazione fattura.
016400100202     D* Returns:        Esito.
016500100202     D*--------------------------------------------------
016600100202     D IntrastatServizi_GetIdRegistrazioneFattura...
016700100202     D                 PR            10I 0
016800100202     D  piTipoDocumento...
016900100202     D                                1A
017000100202     D  piSistemaRegistrazione...
017100100202     D                                     LIKE(andiz00f.sys)
017200100202     D                                     CONST
017300100202     D  piNumeroAssolutoRegistrazione...
017400100202     D                                     LIKE(andiz00f.nr)
017500100202     D                                     CONST
017600100202     D  piSerieRegistrazione...
017700100202     D                                     LIKE(andiz00f.serieNm)
017800100202     D  piDataRegistrazione...
017900100202     D                                 D   DATFMT(*ISO)
018000100202     D  piNumeroRegistrazione...
018100100202     D                                     LIKE(andiz00f.nr)
018200100202     D  piSistemaRegistrazioneFattura...
018300100202     D                                     LIKE(andiz00f.sys)
018400100202     D  piNumeroAssolutoRegistrazioneFattura...
018500100202     D                                     LIKE(andiz00f.nr)
018600100202     D  piSerieRegistrazioneFattura...
018700100202     D                                     LIKE(andiz00f.serieNm)
018800100202     D  piDataRegistrazioneFattura...
018900100202     D                                 D   DATFMT(*ISO)
019000100202     D  piNumeroRegistrazioneFattura...
019100100202     D                                     LIKE(andiz00f.nr)
019200100202
019300100202     D*--------------------------------------------------
019400100202     D* Procedure name: IntrastatServizi_GetServizioByAssoluto
019500100202     D* Purpose:        Restituisce il codice servizio di un documento.
019600100202     D* Returns:        Esito.
019700100202     D* Parameter:      piSistemaRegistrazione => Sistema registrazi...
019800100202     D*                          one.
019900100202     D* Parameter:      piNumeroAssolutoRegistrazione => Numero asso...
020000100202     D*                          luto registrazione.
020100100202     D* Parameter:      piServizio => Codice servizio.
020200100202     D*--------------------------------------------------
020300100202     D IntrastatServizi_GetServizioByAssoluto...
020400100202     D                 PR            10I 0
020500100202     D  piSistemaRegistrazione...
020600100202     D                                     LIKE(andiz00f.sys)
020700100202     D  piNumeroAssolutoRegistrazione...
020800100202     D                                     LIKE(andiz00f.nr)
020900100202     D  piServizio                    6S 0
021000100202
021100100202     D*--------------------------------------------------
021200100202     D* Procedure name: IntrastatServizi_GetAmmontareOperazioniByAssoluto
021300100202     D* Purpose:        Restituisce il codice servizio di un documento.
021400100202     D* Returns:        Esito.
021500100202     D* Parameter:      piSistemaRegistrazione => Sistema registrazi...
021600100202     D*                          one.
021700100202     D* Parameter:      piNumeroAssolutoRegistrazione => Numero asso...
021800100202     D*                          luto registrazione.
021900100202     D*--------------------------------------------------
022000100202     D IntrastatServizi_GetAmmontareOperazioniByAssoluto...
022100100202     D                 PR            10I 0
022200100202     D  piSistemaRegistrazione...
022300100202     D                                     LIKE(andiz00f.sys)
022400100202     D  piNumeroAssolutoRegistrazione...
022500100202     D                                     LIKE(andiz00f.nr)
022600100202     D  piAmmontareDelleOperazioniEuro...
022700100202     D                               15P 2
022800100202     D  piAmmontareDelleOperazioniDivisa...
022900100202     D                               15P 2
023000100204
023100100204     D*--------------------------------------------------
023200100204     D* Procedure name: IntrastatServizi_GetMessaggioErrore
023300100204     D* Purpose:        Restituisce il messaggio dell'ultimo errore.
023400100204     D* Returns:        Messaggio di errore.
023500100204     D*--------------------------------------------------
023600100204     D IntrastatServizi_GetMessaggioErrore...
023700100204     D                 PR           198A   VARYING
023800100204
