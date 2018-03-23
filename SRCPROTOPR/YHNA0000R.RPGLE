000100100920     D*--------------------------------------------------
000101100920     D* Procedure name: PagamentoNA_Init
000102100920     D* Purpose:        Inizializzazione.
000103100920     D* Returns:        Esito.
000104100920     D* Parameter:      piKpjba
000105100920     D*--------------------------------------------------
000106100920     D PagamentoNA_Init...
000107100920     D                 PR            10I 0
000108100920     D  piKpjba                            LIKEDS(kpjba)
000109100920
000110100920     D*--------------------------------------------------
000111100920     D* Procedure name: PagamentoNA_Finalize
000112100920     D* Purpose:        Chiusura service program.
000113100920     D* Returns:
000114100920     D*--------------------------------------------------
000115100920     D PagamentoNA_Finalize...
000116100920     D                 PR
000117100920
000118100920     D*--------------------------------------------------
000119100920     D* Procedure name: PagamentoNA_NewCliente
000120100920     D* Purpose:        Nuovo cliente.
000121100920     D* Returns:        Esito.
000122100920     D* Parameter:      piSocieta => ID società.
000123100920     D* Parameter:      piKscCliente => ID cliente.
000124100920     D*--------------------------------------------------
000125100920     D PagamentoNA_NewCliente...
000126100920     D                 PR            10I 0
000127100920     D  piSocieta                          LIKE(andiz00f.societa)
000128100921     D                                     VALUE
000129100920     D  piKscCliente                       LIKE(andiz00f.ksc)
000130100921     D                                     VALUE
000131100920
000132100921     D*--------------------------------------------------
000133100921     D* Procedure name: PagamentoNA_IsTipoPagamentoBonifico
000134100921     D* Purpose:        Restituisce *ON se il tipo pagamento n.a. è bonific...
000135100921     D*                          o bancario.
000136100921     D* Returns:        *ON = bonifico; *OFF = no bonifico.
000137100921     D*--------------------------------------------------
000138100921     D PagamentoNA_IsTipoPagamentoBonifico...
000139100921     D                 PR              N
000140100921
000141100921     D*--------------------------------------------------
000142100921     D* Procedure name: PagamentoNA_IsCoordinataBancariaEsistente
000143100921     D* Purpose:        Restituisce *ON se esiste la coordinata bancaria pe...
000144100921     D*                          r il bonifico.
000145100921     D* Returns:        *ON = Esiste; *OFF = non esiste.
000146100921     D*--------------------------------------------------
000147100921     D PagamentoNA_IsCoordinataBancariaEsistente...
000148100921     D                 PR              N
000149100921
000150100922     D*--------------------------------------------------
000151100922     D* Procedure name: PagamentoNA_GetCoordinataBancaria
000152100922     D* Purpose:        Restituisce la coordinata bancaria per il bonifico.
000153100922     D* Returns:        Esito.
000154100922     D* Parameter:      piIbanBeneficiario => IBAN beneficiario.
000155100922     D* Parameter:      piBicBeneficiario => Swift BIC beneficiario.
000156100922     D* Parameter:      piCinBeneficiario => CIN beneficiario.
000157100922     D* Parameter:      piAbiBeneficiario => ABI banca beneficiario.
000158100922     D* Parameter:      piCabBeneficiario => CAB banca beneficiario.
000159100922     D* Parameter:      piCcbBeneficiario => Numero conto beneficiario.
000160100922     D* Parameter:      piAbiOrdinante => ABI banca ordinante.
000161100922     D* Parameter:      piCabOrdinante => CAB banca ordinante.
000162100922     D* Parameter:      piBancaOrdinante => Banca ordinante.
000163100922     D* Parameter:      piLineaOrdinante => Linea ordinante.
000164100922     D* Parameter:      piKccBancaOrdinante => Capo conto banca ordinante.
000165100922     D* Parameter:      piKscBancaOrdinante => Sotto conto banca ordinante.
000166100922     D*--------------------------------------------------
000167100922     D PagamentoNA_GetCoordinataBancaria...
000168100922     D                 PR            10I 0
000169100922     D  piIbanBeneficiario...
000170100922     D                               34A   VARYING
000171100922     D  piBicBeneficiario...
000172100922     D                               11A   VARYING
000173100922     D  piCinBeneficiario...
000174100922     D                                1A
000175100922     D  piAbiBeneficiario...
000176100922     D                                5A
000177100922     D  piCabBeneficiario...
000178100922     D                                5A
000179100922     D  piCcbBeneficiario...
000180100922     D                               18A   VARYING
000183100922     D  piBancaOrdinante...
000184100922     D                                     LIKE(andiz00f.banca)
000185100922     D  piLineaOrdinante...
000186100922     D                                     LIKE(andiz00f.linea)
000187100922     D  piKccBancaOrdinante...
000188100922     D                                     LIKE(andiz00f.kcc)
000189100922     D  piKscBancaOrdinante...
000190100922     D                                     LIKE(andiz00f.ksc)
000191100922     D  piTipoPagamento...
000192100922     D                                1A
000193100922
