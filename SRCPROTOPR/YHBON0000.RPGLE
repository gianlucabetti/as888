000100110530     D*--------------------------------------------------
000200110530     D* Procedure name: YHBON_Init
000300110530     D* Purpose:        Inizializza il modulo.
000400110530     D* Returns:        Esito.
000500110530     D* Parameter:      priKpjba
000600110530     D*--------------------------------------------------
000700110530     D YHBON_Init      PR            10I 0
000800110530     D  priKpjba                           LIKEDS(kpjba)
000900110530     D  priSocietaLavoro...
001000110530     D                                     LIKE(andiz00f.societa)
001100110530     D                                     CONST
001200110530
001300110530     D*--------------------------------------------------
001400110530     D* Procedure name: YHBON_Finalize
001500110530     D* Purpose:        Chiude il modulo.
001600110530     D* Returns:
001700110530     D*--------------------------------------------------
001800110531     D YHBON_Finalize  PR            10I 0
001900110530
002000110530     D*--------------------------------------------------
002100110530     D* Procedure name: YHBON_NewDisposizione
002200110530     D* Purpose:        Nuova disposizione.
002300110530     D* Returns:        Esito.
002400110530     D* Parameter:      priCapoContoEstrazione => Capo conto estrazione (ta...
002500110530     D*                          bella YP1).
002600110530     D*--------------------------------------------------
002700110530     D YHBON_NewDisposizione...
002800110530     D                 PR            10I 0
002900110530     D  priCapoContoEstrazione...
003000110530     D                                     LIKE(andiz00f.kcc)
003100110530     D                                     CONST
003200110530     D  priSocietaDisposizione...
003300110530     D                                     LIKE(andiz00f.societa)
003400110530     D                                     CONST
003500110530
003600110530     D*--------------------------------------------------
003700110530     D* Procedure name: YHBON_EndDisposizione
003800110530     D* Purpose:        Fine disposizione.
003900110530     D* Returns:        Esito.
004000110530     D* Parameter:      priInserireDisposizione => *ON = inserire disposizi...
004100110530     D*                          one; *OFF = annullare disposizione
004200110530     D*--------------------------------------------------
004300110530     D YHBON_EndDisposizione...
004400110530     D                 PR            10I 0
004500110530     D  priInserireDisposizione...
004600110530     D                                 N   CONST
004700110530
004800110530     D*--------------------------------------------------
004900110530     D* Procedure name: YHBON_SetUnita
005000110530     D* Purpose:        Imposta l'unità.
005100110530     D* Returns:        Esito.
005200110530     D* Parameter:      priUnita
005300110530     D*--------------------------------------------------
005400110530     D YHBON_SetUnita  PR            10I 0
005500110530     D  priUnita                           LIKE(andiz00f.unita)
005600110530     D                                     CONST
005700110530
005800110530     D*--------------------------------------------------
005900110530     D* Procedure name: YHBON_SetConti
006000110530     D* Purpose:        Imposta conto creditore, beneficiario e intestazione.
006100110530     D* Returns:        Esito.
006200110530     D* Parameter:      priSottoContoCreditore => Sotto conto creditore
006300110530     D* Parameter:      priCapoContoBeneficiario => Capo conto beneficiario
006400110530     D* Parameter:      priSottoContoBeneficiario => Sotto conto beneficiario
006500110530     D* Parameter:      priSottoContoIntestazione => Sotto conto intestazione
006600110530     D*--------------------------------------------------
006700110530     D YHBON_SetConti  PR            10I 0
006800110530     D  priSottoContoCreditore...
006900110530     D                                     LIKE(andiz00f.ksc)
007000110530     D                                     VALUE
007100110530     D  priCapoContoBeneficiario...
007200110530     D                                     LIKE(andiz00f.kcc)
007300110530     D                                     VALUE
007400110530     D  priSottoContoBeneficiario...
007500110530     D                                     LIKE(andiz00f.ksc)
007600110530     D                                     VALUE
007700110530     D  priSottoContoIntestazione...
007800110530     D                                     LIKE(andiz00f.ksc)
007900110530     D                                     VALUE
008000110530
008100110530     D*--------------------------------------------------
008200110530     D* Procedure name: YHBON_SetBeneficiario
008300110530     D* Purpose:        Imposta i dati del beneficiario.
008400110530     D* Returns:        Esito.
008500110530     D* Parameter:      priRagioneSociale => Ragione sociale
008600110530     D* Parameter:      priIndirizzo => Indirizzo
008700110530     D* Parameter:      priCap => Codice avviamento postale
008800110530     D* Parameter:      priLocalita => Località
008900110530     D* Parameter:      priSiglaProvinciaItalia => Sigla provincia Italia
009000110530     D* Parameter:      priSiglaNazione => Sigla nazione
009100110530     D*--------------------------------------------------
009200110530     D YHBON_SetBeneficiario...
009300110530     D                 PR            10I 0
009400110530     D  priDescrizioneBeneficiario...
009500110531     D                                     LIKE(andiz00f.descr)
009600110530     D                                     CONST
009700110530     D  priIndirizzoBeneficiario...
009800110531     D                                     LIKE(andiz00f.indiriz)
009900110530     D                                     CONST
010000110530     D  priCapBeneficiario...
010100110531     D                                     LIKE(andiz00f.cap)
010200110530     D                                     CONST
010300110530     D  priLocalitaBeneficiario...
010400110531     D                                     LIKE(andiz00f.localit)
010500110530     D                                     CONST
010600110530     D  priSiglaNazioneBeneficiario...
010700110531     D                                     LIKE(andiz00f.stato)
010800110530     D                                     CONST
010900110530     D                                     OPTIONS(*NOPASS)
011000110530     D  priSiglaProvinciaItaliaBeneficiario...
011100110531     D                                     LIKE(andiz00f.prov)
011200110530     D                                     CONST
011300110530     D                                     OPTIONS(*NOPASS)
011400110530
011500110530     D*--------------------------------------------------
011600110530     D* Procedure name: YHBON_SetPartitaContabile
011700110530     D* Purpose:        Imposta ID partita contabile.
011800110530     D* Returns:        Esito.
011900110530     D* Parameter:      priDataPartita => Data partita
012000110530     D* Parameter:      priNumeroPartita => Numero partita
012100110530     D* Parameter:      priSeriePartita => Serie partita
012200110530     D*--------------------------------------------------
012300110530     D YHBON_SetPartitaContabile...
012400110530     D                 PR            10I 0
012500110530     D priDataPartita                  D   DATFMT(*ISO)
012600110530     D                                     CONST
012700110530     D  priNumeroPartita...
012800110530     D                                     LIKE(andiz00f.nr)
012900110530     D                                     CONST
013000110530     D  priSeriePartita...
013100110530     D                                     LIKE(andiz00f.serieNm)
013200110530     D                                     CONST
013300110530
013400110530     D*--------------------------------------------------
013500110530     D* Procedure name: YHBON_SetImporti
013600110530     D* Purpose:        Imposta gli importi.
013700110530     D* Returns:        Esito.
013800110530     D* Parameter:      priImportoInMonetaCorrente => Importo in moneta cor...
013900110530     D*                          rente (positivo = pagare, negativo = tratt...
014000110530     D*                          enere)
014100110530     D* Parameter:      priDivisa => Divisa
014200110530     D* Parameter:      priImportoInDivisa => Importo in divisa
014300110530     D*--------------------------------------------------
014400110530     D YHBON_SetImporti...
014500110530     D                 PR            10I 0
014600110530     D  priImportoInMonetaCorrente...
014700110530     D                               15P 2 CONST
014800110530     D  priDivisa                          LIKE(andiz00f.divisa)
014900110530     D                                     CONST
015000110530     D                                     OPTIONS(*NOPASS)
015100110530     D  priImportoInDivisa...
015200110530     D                               15P 2 CONST
015300110530     D                                     OPTIONS(*NOPASS)
015400110530
015500110530     D*--------------------------------------------------
015600110530     D* Procedure name: YHBON_SetCoordinateBancarie
015700110530     D* Purpose:        Imposta le coordinate bancarie
015800110530     D* Returns:        Esito.
015900110530     D* Parameter:      priTipoDisposizione => *OMIT = automatico; 'B' = Bo...
016000110530     D*                          nifico nazionale; 'E' = bonifico Estero; '...
016100110530     D*                          T' = assegno Traenza; 'A' = Assegno bancario.
016200110530     D* Parameter:      priIban => International Bank Account Number
016300110530     D* Parameter:      priNumeroConto => Numero conto.
016301150407     D* Parameter:      priDivisa      => Codice divisa pagamento.
016400110530     D*--------------------------------------------------
016500110530     D YHBON_SetCoordinateBancarie...
016600110530     D                 PR            10I 0
016700110530     D  priTipoDisposizione...
016800110530     D                                1A   CONST
016900110530     D                                     OPTIONS(*OMIT)
017000110530     D  priIban                      34A   CONST
017100110530     D                                     OPTIONS(*NOPASS:*OMIT)
017200110530     D priSwiftBic                   11A   CONST
017300150407     D                                     OPTIONS(*NOPASS:*OMIT)
017400110530     D priNumeroConto                18A   CONST
017500150407     D                                     OPTIONS(*NOPASS:*OMIT)
017501150407     D priDivisa                      4A   CONST
017502150407     D                                     OPTIONS(*NOPASS)
017600110530
017700110531     D*--------------------------------------------------
017800110531     D* Procedure name: YHBON_SetBancaPagamento
017900110531     D* Purpose:        Imposta la banca di pagamento.
018000110531     D* Returns:        Esito.
018100110531     D* Parameter:      priBanca => Banca
018200110531     D* Parameter:      priLinea => Linea di credito
018300110531     D*--------------------------------------------------
018400110531     D YHBON_SetBancaPagamento...
018500110531     D                 PR            10I 0
018600110531     D  priBanca                           LIKE(andiz00f.banca)
018700110531     D                                     CONST
018800110531     D  priLinea                           LIKE(andiz00f.linea)
018900110531     D                                     CONST
019000110531     D                                     OPTIONS(*NOPASS)
019100110531
019200110601     D*--------------------------------------------------
019300110601     D* Procedure name: YHBON_GetMessaggio
019400110601     D* Purpose:        Restituisce il messaggio di errore.
019500110601     D* Returns:        Messaggio di errore.
019600110601     D*--------------------------------------------------
019700110601     D YHBON_GetMessaggio...
019800110601     D                 PR           255A   VARYING
019900110601
020000110601     D*--------------------------------------------------
020100110601     D* Procedure name: YHBON_SetDataValutaBancaBeneficiario
020200110601     D* Purpose:        Imposta la data valuta banca beneficiario (regolamento)
020300110601     D* Returns:        Esito.
020400110601     D* Parameter:      priDataValutaBancaBeneficiario
020500110601     D*--------------------------------------------------
020600110601     D YHBON_SetDataValutaBancaBeneficiario...
020700110601     D                 PR            10I 0
020800110601     D  priDataValutaBancaBeneficiario...
020900110601     D                                 D   DATFMT(*ISO)
021000110601     D                                     CONST
021100110601
021200110601     D*--------------------------------------------------
021300110601     D* Procedure name: YHBON_SetRaggruppamento
021400110601     D* Purpose:        Imposta il raggruppamento disposizione.
021500110601     D* Returns:        Esito
021600110601     D* Parameter:      priRaggruppamento
021700110601     D*--------------------------------------------------
021800110601     D YHBON_SetRaggruppamento...
021900110601     D                 PR            10I 0
022000110601     D  priRaggruppamento...
022100110601     D                               15P 0 CONST
022200110601
