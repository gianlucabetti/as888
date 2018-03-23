000100070705     D*--------------------------------------------------
000200070705     D* Procedure name: FatturaManuale_var_mailto
000300070705     D* Purpose:        Chiama il programma di posta elettronica.
000400070705     D* Returns:        Esito.
000500070705     D* Parameter:      piUtenteDestinatario => Profilo utente destinatario.
000600070705     D* Parameter:      piNumeroRichiesta => Numero richiesta.
000700070705     D* Parameter:      piDataRichiesta => Data richiesta.
000800070705     D*--------------------------------------------------
000900070705     D FatturaManuale_var_mailto...
001000070705     D                 PR            10I 0
001100070705     D  piUtenteDestinatario...
001200070705     D                               10A   CONST
001300070705     D  piNumeroRichiesta...
001400070705     D                               10I 0 CONST
001500080306
001600080310     D*--------------------------------------------------
001700080310     D* Procedure name: FatturaManuale_var_inAltraSocieta
001800080310     D* Purpose:        Esegue un programma in una società diversa.
001900080310     D* Returns:        Esito.
002000080310     D* Parameter:      piNomeProgramma => Nome del programma da eseguire.
002100080310     D* Parameter:      piSocietaDestinazione => Società di destinazione
002200080310     D*--------------------------------------------------
002300080310     D FatturaManuale_var_inAltraSocieta...
002400080310     D                 PR            10I 0
002500080310     D  piNomeProgramma...
002600080310     D                               10A   CONST
002700080310     D  piSocietaDestinazione...
002800080310     D                                3A
002900080310     D  piKpjba...
003000080311     D                                     LIKEDS(kpjba)
003100080310     D  piParametro1...
003200080310     D                             5000A   OPTIONS(*VARSIZE:*NOPASS)
003300080310     D  piParametro1Len...
003400080310     D                               10I 0 CONST OPTIONS(*NOPASS)
003500080310     D  piParametro2...
003600080310     D                             5000A   OPTIONS(*VARSIZE:*NOPASS)
003700080310     D  piParametro2Len...
003800080310     D                               10I 0 CONST OPTIONS(*NOPASS)
003900080310     D  piParametro3...
004000080310     D                             5000A   OPTIONS(*VARSIZE:*NOPASS)
004100080310     D  piParametro3Len...
004200080310     D                               10I 0 CONST OPTIONS(*NOPASS)
004300080310     D  piParametro4...
004400080310     D                             5000A   OPTIONS(*VARSIZE:*NOPASS)
004500080310     D  piParametro4Len...
004600080310     D                               10I 0 CONST OPTIONS(*NOPASS)
004700080311
004800080311     D*--------------------------------------------------
004900080311     D* Procedure name: FatturaManuale_var_estrattoConto
005000080311     D* Purpose:        Estratto conto di Proj.
005100080311     D* Returns:        Esito.
005200080311     D* Parameter:      piKpjba
005300080311     D*--------------------------------------------------
005400080311     D FatturaManuale_var_estrattoConto...
005500080311     D                 PR            10I 0
005600080311     D  piKpjba                            LIKEDS(kpjba)
005700080311     D  piSocietaFattura...
005800080311     D                                3A
005900080311
006000080311     D*--------------------------------------------------
006100080311     D* Procedure name: FatturaManuale_var_interrogazioneConto
006200080311     D* Purpose:
006300080311     D* Returns:        Esito.
006400080311     D* Parameter:      piKpjba
006500080311     D*--------------------------------------------------
006600080311     D FatturaManuale_var_interrogazioneConto...
006700080311     D                 PR            10I 0
006800080311     D  piKpjba                            LIKEDS(kpjba)
006900080311
007000080311     D*--------------------------------------------------
007100080311     D* Procedure name: FatturaManuale_var_commentiFattura
007200080311     D* Purpose:        Commenti legati alla fattura.
007300080311     D* Returns:        Esito.
007400080311     D* Parameter:      piKpjba
007500080311     D*--------------------------------------------------
007600080311     D FatturaManuale_var_commentiFattura...
007700080311     D                 PR            10I 0
007800080311     D  piKpjba                            LIKEDS(kpjba)
007900080311
008000080306     D*--------------------------------------------------
008100080306     D* Procedure name: FatturaManuale_var_contabilita
008200080306     D* Purpose:        Visualizza la registrazione della fattura.
008300080306     D* Returns:        Esito.
008400080306     D*--------------------------------------------------
008500080306     D FatturaManuale_var_contabilita...
008600080306     D                 PR            10I 0
008700080307     D  piKpjba...
008800080307     D                                     LIKEDS(kpjba)
008900080313     D  piNdc001Ds...
009000080313     D                                     LIKEDS(ndc001Ds)
009100080313     D  piNdc077Ds...
009200080313     D                                     LIKEDS(ndc077Ds)
009300080313
009400080313     D*--------------------------------------------------
009500080313     D* Procedure name: FatturaManuale_var_esisteRegistrazioneContabile
009600080313     D* Purpose:        Restituisce *ON se la registrazione contabile esiste.
009700080313     D* Returns:        *ON = Esiste; *OFF = non esiste.
009800080313     D* Parameter:      piSistema => Sistema.
009900080313     D* Parameter:      piNumeroAssolutoRegistrazione => Numero assoluto re...
010000080313     D*                          gistrazione.
010100080313     D*--------------------------------------------------
010200080313     D FatturaManuale_var_esisteRegistrazioneContabile...
010300080313     D                 PR              N
010400080313     D  piSistema                     5I 0 CONST
010500080313     D  piNumeroAssolutoRegistrazione...
010600080313     D                               10I 0 CONST
010700080730     D  piSocieta...
010800080730     D                                3A   OPTIONS(*NOPASS)
010900080730     D  piRiferimentoPrimaNotaBatch...
011000080730     D                                9A   OPTIONS(*NOPASS)
011100080730
011200080718     D*--------------------------------------------------
011300080718     D* Procedure name: FatturaManuale_var_esisteDichiarazioneIntento
011400080718     D* Purpose:        Restituisce *ON se il cliente ha (o ha avuto) una d...
011500080718     D*                          ichiarazione di intento.
011600080718     D* Returns:        *ON = il cliente ha dichiarazioni di intento.
011700080718     D* Parameter:      piSocieta => Società
011800080718     D* Parameter:      piKscCliente => Codice cliente.
011900080718     D* Parameter:      piDataRiferimento => Data riferimento.
012000080718     D*--------------------------------------------------
012100080718     D FatturaManuale_var_esisteDichiarazioneIntento...
012200080718     D                 PR              N
012300080718     D  piSocieta                     3A
012400080718     D  piKscCliente                  8A
012500080718     D  piDataRiferimento...
012600080718     D                                 D   DATFMT(*ISO)
012700080718     D                                     CONST
012800080718     D                                     OPTIONS(*NOPASS)
012900080718
013000080718     D*--------------------------------------------------
013100080718     D* Procedure name: FatturaManuale_var_dichiarazioniIntento
013200080718     D* Purpose:        Visualizza le dichiarazioni di intento del cliente.
013300080718     D* Returns:        Esito.
013400080718     D* Parameter:      piKpjba
013500080718     D*--------------------------------------------------
013600080718     D FatturaManuale_var_dichiarazioniIntento...
013700080718     D                 PR            10I 0
013800080718     D  piKpjba                            LIKEDS(kpjba)
013900080718     D  piSocieta                     3A   OPTIONS(*NOPASS)
014000080718
014100080721     D*--------------------------------------------------
014200080721     D* Procedure name: FatturaManuale_var_riepilogoIVA
014300080721     D* Purpose:        Visualizza il riepilogo IVA.
014400080721     D* Returns:        Esito.
014500080721     D* Parameter:      piNumeroRichiesta => Numero richiesta.
014600080721     D*--------------------------------------------------
014700080721     D FatturaManuale_var_riepilogoIVA...
014800080721     D                 PR            10I 0
014900080721     D  piNumeroRichiesta...
015000080721     D                               10I 0 VALUE
015100080721
