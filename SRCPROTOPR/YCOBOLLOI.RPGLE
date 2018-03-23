000100070328     D*--------------------------------------------------
000200070328     D* Procedure name: Bollo_i_init
000300070328     D* Purpose:        Inizializzazioni.
000400070328     D* Returns:        Esito.
000500070328     D*--------------------------------------------------
000600070328     D Bollo_i_init    PR            10I 0
000700070329     D  piIdBollo                     1A   CONST
000800070329     D                                     OPTIONS(*NOPASS:*OMIT)
000900070329     D  piDataValidita...
001000070329     D                               10D   DATFMT(*ISO)
001100070329     D                                     CONST
001200070329     D                                     OPTIONS(*NOPASS:*OMIT)
001300070330     D  piSocieta                     3A   CONST
001400070329     D                                     OPTIONS(*NOPASS:*OMIT)
001500070329
001600070328     D*--------------------------------------------------
001700070328     D* Procedure name: Bollo_i_finalize
001800070328     D* Purpose:        Operazioni finali.
001900070328     D* Returns:        Esito.
002000070328     D*--------------------------------------------------
002100070328     D Bollo_i_finalize...
002200070328     D                 PR            10I 0
002300070329
002400070329     D*--------------------------------------------------
002500070329     D* Procedure name: Bollo_i_newDocumento
002600070329     D* Purpose:        Nuovo documento.
002700070329     D* Returns:        Esito.
002800070329     D* Parameter:      piDataDocumento => Data documento
002900070329     D*--------------------------------------------------
003000070329     D Bollo_i_newDocumento...
003100070329     D                 PR            10I 0
003200070329     D  piIdBollo                     1A   CONST
003300070329     D                                     OPTIONS(*NOPASS:*OMIT)
003400070329     D  piDataValidita...
003500070329     D                               10D   DATFMT(*ISO)
003600070329     D                                     CONST
003700070329     D                                     OPTIONS(*NOPASS:*OMIT)
003800070330     D  piSocieta                     3A   CONST
003900070329     D                                     OPTIONS(*NOPASS:*OMIT)
004000070329
004100070329     D*--------------------------------------------------
004200070329     D* Procedure name: Bollo_i_endDocumento
004300070329     D* Purpose:        Fine documento.
004400070329     D* Returns:        Esito.
004500070329     D*--------------------------------------------------
004600070329     D Bollo_i_endDocumento...
004700070329     D                 PR            10I 0
004800070328
004900070329     D*--------------------------------------------------
005000070329     D* Procedure name: Bollo_i_newVirtualeBySocieta
005100070329     D* Purpose:        Nuova autorizzazione bollo virtuale per società.
005200070329     D* Returns:        Esito.
005300070329     D*--------------------------------------------------
005400070329     D Bollo_i_newVirtualeBySocieta...
005500070329     D                 PR            10I 0
005600070330     D  piSocieta                     3A   CONST
005700070330     D  piDataValidita...
005800070329     D                               10D   DATFMT(*ISO)
005900070330     D                                     CONST
006000070329
006100070329     D*--------------------------------------------------
006200070329     D* Procedure name: Bollo_i_addImponibileEsenteIVA
006300070329     D* Purpose:        Aggiunge un imponibile esente IVA.
006400070329     D* Returns:        Esito.
006500070329     D* Parameter:      idRiferimentoIVA => Riferimento IVA
006600070329     D* Parameter:      piImponibileIVA => Imponibile IVA.
006700070329     D*--------------------------------------------------
006800070329     D Bollo_i_addImponibileEsenteIVA...
006900070329     D                 PR            10I 0
007000070329     D  piRiferimentoIVA...
007100070329     D                                3A
007200070329     D  piImponibileIVA...
007300070330     D                               15P 2 VALUE
007400070329
007500070329     D*--------------------------------------------------
007600070329     D* Procedure name: Bollo_i_getImportoBollo
007700070329     D* Purpose:        Reperisce l' importo del bollo.
007800070329     D* Returns:        Importo bollo.
007900070329     D*--------------------------------------------------
008000070329     D Bollo_i_getImportoBollo...
008100070329     D                 PR            15P 2
008200070329
008300070329     D*--------------------------------------------------
008400070329     D* Procedure name: Bollo_i_isVirtuale
008500070329     D* Purpose:        Dice se la società ha il bollo virtuale.
008600070329     D* Returns:        *ON = bollo virtuale.
008700070329     D* Parameter:      piSocieta => Società.
008800070329     D* Parameter:      piEsito => Esito.
008900070329     D*--------------------------------------------------
009000070329     D Bollo_i_isVirtuale...
009100070329     D                 PR              N
009200070330
009300070330     D*--------------------------------------------------
009400070330     D* Procedure name: Bollo_i_getDichiarazioneBolloVirtuale
009500070330     D* Purpose:        Reperisce il testo della dichiarazione del bollo vi...
009600070330     D*                          rtuale.
009700070330     D* Returns:        Testo dichiarazione bollo virtuale.
009800070330     D*--------------------------------------------------
009900070330     D Bollo_i_getDichiarazioneBolloVirtuale...
010000070330     D                 PR           255A   VARYING
010100070330
010200070330     D*--------------------------------------------------
010300070330     D* Procedure name: Bollo_i_getDatiRegistrazioneCosto
010400070330     D* Purpose:        Restituisce i dati contabili per registrare il cost...
010500070330     D*                          o del bollo.
010600070330     D* Returns:        Esito.
010700070330     D* Parameter:      piCapoContoCosto => Capo conto costo.
010800070330     D* Parameter:      piSottoContoCosto => Sotto conto costo.
010900070330     D* Parameter:      piCausaleCosto => Causale costo.
011000070330     D* Parameter:      piVoceCosto => Voce co.an. costo.
011100070330     D*--------------------------------------------------
011200070330     D Bollo_i_getDatiRegistrazioneCosto...
011300070330     D                 PR            10I 0
011400070330     D  piCapoContoCosto...
011500070330     D                                6A
011600070330     D  piSottoContoCosto...
011700070330     D                                8A
011800070330     D piCausaleCosto                 4A
011900070330     D  piVoceCosto                   8A
012000070330
012100070330     D*--------------------------------------------------
012200070330     D* Procedure name: Bollo_i_getDatiRegistrazioneRicavo
012300070330     D* Purpose:        Restituisce i dati per registrare il recupero del b...
012400070330     D*                          ollo.
012500070330     D* Returns:        Esito.
012600070330     D* Parameter:      piCapoContoRicavo => Capo conto ricavo.
012700070330     D* Parameter:      piSottoContoRicavo => Sotto conto ricavo.
012800070330     D* Parameter:      piCausaleRicavo => Causale ricavo.
012900070330     D* Parameter:      piVoceRicavo => Voce co.an. ricavo.
013000070330     D*--------------------------------------------------
013100070330     D Bollo_i_getDatiRegistrazioneRicavo...
013200070330     D                 PR            10I 0
013300070330     D  piCapoContoRicavo...
013400070330     D                                6A
013500070330     D  piSottoContoRicavo...
013600070330     D                                8A
013700070330     D  piCausaleRicavo...
013800070330     D                                4A
013900070330     D  piVoceRicavo                  8A
014000070330     D  piEsenzioneIVA...
014100070330     D                                3A
014200070330
014300070330     D*--------------------------------------------------
014400070330     D* Procedure name: Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale
014500070330     D* Purpose:        Restituisce i dati per la registrazione di debito v...
014600070330     D*                          erso l'erario per il bollo virtuale.
014700070330     D* Returns:        Esito.
014800070330     D* Parameter:      piCapoContoDebito => Capo conto debito verso erario.
014900070330     D* Parameter:      piSottoContoDebito => Sotto conto debito verso erario.
015000070330     D* Parameter:      piCausaleDebito => Causale debito verso erario.
015100070330     D*--------------------------------------------------
015200070330     D Bollo_i_getDatiRegistrazioneDebitoBolloVirtuale...
015300070330     D                 PR            10I 0
015400070330     D  piCapoContoDebito...
015500070330     D                                6A
015600070330     D  piSottoContoDebito...
015700070330     D                                8A
015800070330     D  piCausaleDebito...
015900070330     D                                4A
016000070330
