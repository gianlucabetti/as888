000100131230     D*--------------------------------------------------
000101131230     D* Procedure name: SEPA_Init
000102131230     D* Purpose:        Inizializzazione oggetto.
000103131230     D* Returns:        Esito.
000104131230     D* Parameter:      priKpjba
000105131230     D*--------------------------------------------------
000106131230     D SEPA_Init       PR            10I 0
000107131230     D  priKpjba                           LIKEDS(kpjba)
000108131230     D                                     OPTIONS(*NOPASS)
000109131230
000110131230     D*--------------------------------------------------
000111131230     D* Procedure name: SEPA_Finalize
000112131230     D* Purpose:        Chiude l'oggetto.
000113131230     D* Returns:
000114131230     D*--------------------------------------------------
000115131230     D SEPA_Finalize   PR
000116131230
000117131230     D*--------------------------------------------------
000118131230     D* Procedure name: SEPA_IsBonificoSEPA
000119131230     D* Purpose:        Restituisce *ON se il bonifico è SEPA.
000120131230     D* Returns:        *ON = SEPA
000121131230     D* Parameter:      priIbanBeneficiario => IBAN beneficiario.
000122131230     D* Parameter:      priDivisa => Codice divisa.
000123131230     D*--------------------------------------------------
000124131230     D SEPA_IsBonificoSEPA...
000125131230     D                 PR              N
000126131230     D  priIbanBeneficiario...
000127131230     D                               34A   CONST
000128131230     D  priDivisa                     4A   CONST
000129131230
000130131230     D*--------------------------------------------------
000131131230     D* Procedure name: SEPA_IsNazioneIso2InSEPA
000132131230     D* Purpose:        Restituisce *ON se la nazione ISO ALPHA-2 appartien...
000133131230     D*                          e alla SEPA
000134131230     D* Returns:        *ON = nazione in SEPA
000135131230     D* Parameter:      priNazioneIso2 => Codice nazione ISO ALPHA-2.
000136131230     D*--------------------------------------------------
000137131230     D SEPA_IsNazioneIso2InSEPA...
000138131230     D                 PR              N
000139131230     D priNazioneIso2                 2A   CONST
000140131230
