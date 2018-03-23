000100100617     D*--------------------------------------------------
000200100617     D* Procedure name: DateDisposizione_Init
000300100617     D* Purpose:        Inizializza il modulo.
000400100617     D* Returns:        Esito.
000500100617     D* Parameter:      piKpjba
000600100617     D*--------------------------------------------------
000700100617     D DateDisposizione_Init...
000800100617     D                 PR            10I 0
000900100617     D  piKpjba                            LIKEDS(kpjba)
001000100617
001100100617     D*--------------------------------------------------
001200100617     D* Procedure name: DateDisposizione_Finalize
001300100617     D* Purpose:        Chiude il modulo.
001400100617     D* Returns:
001500100617     D*--------------------------------------------------
001600100617     D DateDisposizione_Finalize...
001700100617     D                 PR
001800100617
001900100617     D*--------------------------------------------------
002000100617     D* Procedure name: DateDisposizione_NewDisposizione
002100100617     D* Purpose:        Nuova disposizione di pagamento.
002200100617     D* Returns:        Esito.
002300100617     D* Parameter:      piSocieta => Società.
002400100617     D* Parameter:      piCapoContoCreditore => Capo conto creditore.
002500100617     D* Parameter:      piTipoPagamento => Tipo pagamento.
002600100617     D* Parameter:      piBanca => Banca.
002700100617     D* Parameter:      piLinea => Linea.
002800100617     D* Parameter:      piOperazioneBancaria => Operazione bancaria.
002900100617     D* Parameter:      piAbiBeneficiario => ABI beneficiario.
003000100617     D*--------------------------------------------------
003100100617     D DateDisposizione_NewDisposizione...
003200100617     D                 PR            10I 0
003300100618     D  piSocieta                          LIKE(andiz00f.societa)
003400100617     D                                     CONST
003500100617     D  piCapoContoCreditore...
003600100618     D                                     LIKE(andiz00f.kcc)
003700100617     D                                     CONST
003800100617     D  piTipoPagamento...
003900100617     D                                1A   CONST
004000100618     D  piBanca                            LIKE(andiz00f.banca)
004100100617     D                                     CONST
004200100618     D  piLinea                            LIKE(andiz00f.linea)
004300100617     D                                     CONST
004400100617     D  piOperazioneBancaria...
004500100618     D                                     LIKE(andiz00f.cauBan)
004600100617     D                                     CONST
004700100617     D  piAbiBeneficiario...
004800100618     D                                     LIKE(andiz00f.abi)
004900100617     D                                     CONST
005000100617
005100100618     D*--------------------------------------------------
005200100618     D* Procedure name: DateDisposizione_GetDataEsecuzioneByDataValutaBanca...
005300100618     D*                          Beneficiario
005400100618     D* Purpose:        Restituisce la data esecuzione disposizione calcola...
005500100618     D*                          ta dalla data valuta banca beneficiario.
005600100618     D* Returns:
005700100618     D* Parameter:      piDataValutaBancaBeneficiario => Data valuta banca ...
005800100618     D*                          beneficiario.
005900100618     D*--------------------------------------------------
006000100618     D DateDisposizione_GetDataEsecuzioneByDataValutaBancaBeneficiario...
006100100618     D                 PR              D   DATFMT(*ISO)
006200100618     D  piDataValutaBancaBeneficiario...
006300100618     D                                 D   DATFMT(*ISO)
006400100618     D                                     CONST
006500100618     D  piEsito...
006600100618     D                               10I 0
006700100618
006800100618     D*--------------------------------------------------
006900100618     D* Procedure name: DateDisposizione_GetDataValutaBanca...
007000100618     D*                          BeneficiarioByDataEsecuzione
007100100618     D* Purpose:        Restituisce la data valuta banca beneficiario.
007200100618     D*                 calcolata dalla data esecuzione disposizione.
007300100618     D* Returns:
007400100618     D* Parameter:      piDataEsecuzione => Data esecuzione disposizione
007500100618     D*--------------------------------------------------
007600100618     D DateDisposizione_GetDataValutaBancaBeneficiarioByDataEsecuzione...
007700100618     D                 PR              D   DATFMT(*ISO)
007800100618     D  piDataEsecuzione...
007900100618     D                                 D   DATFMT(*ISO)
008000100618     D                                     CONST
008100100618     D  piEsito...
008200100618     D                               10I 0
008300100618
008400100618     D*--------------------------------------------------
008500100618     D* Procedure name: DateDisposizione_AggiornaTutteLeDisposizioni
008600100618     D* Purpose:        Aggiorna tutte le disposizioni che soddisfano i par...
008700100618     D*                          ametri passati.
008800100618     D* Returns:        Esito.
008900100618     D* Parameter:      piSocieta => Società.
009000100618     D* Parameter:      piCapoContoCreditore => Capo conto creditore.
009100100618     D* Parameter:      piBanca => Banca.
009200100618     D* Parameter:      piLinea => Linea.
009300100618     D* Parameter:      piDivisa => Divisa.
009400100618     D* Parameter:      piTipoPagamento => Tipo pagamento.
009500100618     D*--------------------------------------------------
009600100618     D DateDisposizione_AggiornaTutteLeDisposizioni...
009700100618     D                 PR            10I 0
009800100618     D  piSocieta                          LIKE(andiz00f.societa)
009900100618     D                                     CONST
010000100618     D  piCapoContoCreditore...
010100100618     D                                     LIKE(andiz00f.kcc)
010200100618     D                                     CONST
010300100618     D  piBanca                            LIKE(andiz00f.banca)
010400100618     D                                     CONST
010500100618     D  piLinea                            LIKE(andiz00f.linea)
010600100618     D                                     CONST
010700100618     D  piOperazioneBancaria...
010800100618     D                                     LIKE(andiz00f.cauBan)
010900100618     D                                     CONST
011000100618     D  piDivisa                           LIKE(andiz00f.divisa)
011100100618     D                                     CONST
011200100618     D  piTipoPagamento...
011300100618     D                                1A   CONST
011400100618     D  piDataEsecuzione...
011500100618     D                                 D   DATFMT(*ISO)
011600100618     D                                     CONST
011700100621     D                                     OPTIONS(*OMIT)
011800100621     D  piDataValutaBancaBeneficiario...
011900100618     D                                 D   DATFMT(*ISO)
012000100618     D                                     CONST
012100100621     D                                     OPTIONS(*OMIT)
012200100621
