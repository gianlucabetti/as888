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
004900070328     D*--------------------------------------------------
005000070329     D* Procedure name: Bollo_i_newTipoBolloByIdBollo
005100070329     D* Purpose:        Nuovo bollo reperito con ID bollo e data validità.
005200070328     D* Returns:        Esito.
005300070328     D*--------------------------------------------------
005400070329     D Bollo_i_newTipoBolloByIdBollo...
005500070328     D                 PR            10I 0
005600070330     D  piIdBollo                     1A   CONST
005700070329     D piDataValidita                10D   DATFMT(*ISO)
005800070330     D                                     CONST
005900070329
006000070329     D*--------------------------------------------------
006100070329     D* Procedure name: Bollo_i_newVirtualeBySocieta
006200070329     D* Purpose:        Nuova autorizzazione bollo virtuale per società.
006300070329     D* Returns:        Esito.
006400070329     D*--------------------------------------------------
006500070329     D Bollo_i_newVirtualeBySocieta...
006600070329     D                 PR            10I 0
006700070330     D  piSocieta                     3A   CONST
006800070330     D  piDataValidita...
006900070329     D                               10D   DATFMT(*ISO)
007000070330     D                                     CONST
007100070329
007200070329     D*--------------------------------------------------
007300070329     D* Procedure name: Bollo_i_newEsenzioneIVAByRiferimento
007400070329     D* Purpose:        Nuova esenzione IVA.
007500070329     D* Returns:        Esito.
007600070329     D* Parameter:      piRiferimentoIVA => Riferimento esenzione IVA
007700070329     D*--------------------------------------------------
007800070329     D Bollo_i_newEsenzioneIVAByRiferimento...
007900070329     D                 PR            10I 0
008000070330     D  piIdBollo                     1A   CONST
008100070329     D piDataValidita                10D   DATFMT(*ISO)
008200070330     D                                     CONST
008300070329     D  piRiferimentoIVA...
008400070330     D                                3A
008500070329
008600070329     D*--------------------------------------------------
008700070329     D* Procedure name: Bollo_i_newImportoBolloByImponibile
008800070329     D* Purpose:        Nuovo importo bollo per imponibile.
008900070329     D* Returns:        Esito.
009000070329     D* Parameter:      piIdBollo => ID bollo
009100070329     D* Parameter:      piDataValidita => Data validità.
009200070329     D* Parameter:      piImponibileBollo => Imponibile bollo.
009300070329     D*--------------------------------------------------
009400070329     D Bollo_i_newImportoBolloByImponibile...
009500070329     D                 PR            10I 0
009600070330     D  piIdBollo                     1A   CONST
009700070329     D piDataValidita                10D   DATFMT(*ISO)
009800070330     D                                     CONST
009900070329     D  piImponibileBollo...
010000070329     D                               15P 2
010100070329
010200070329     D*--------------------------------------------------
010300070329     D* Procedure name: Bollo_i_addImponibileEsenteIVA
010400070329     D* Purpose:        Aggiunge un imponibile esente IVA.
010500070329     D* Returns:        Esito.
010600070329     D* Parameter:      idRiferimentoIVA => Riferimento IVA
010700070329     D* Parameter:      piImponibileIVA => Imponibile IVA.
010800070329     D*--------------------------------------------------
010900070329     D Bollo_i_addImponibileEsenteIVA...
011000070329     D                 PR            10I 0
011100070329     D  piRiferimentoIVA...
011200070329     D                                3A
011300070329     D  piImponibileIVA...
011400070330     D                               15P 2 VALUE
011500070329
011600070329     D*--------------------------------------------------
011700070329     D* Procedure name: Bollo_i_getImportoBollo
011800070329     D* Purpose:        Reperisce l' importo del bollo.
011900070329     D* Returns:        Importo bollo.
012000070329     D*--------------------------------------------------
012100070329     D Bollo_i_getImportoBollo...
012200070329     D                 PR            15P 2
012300070329
012400070329     D*--------------------------------------------------
012500070329     D* Procedure name: Bollo_i_setIdBollo
012600070329     D* Purpose:        Imposta l' ID bollo.
012700070329     D* Returns:        Esito.
012800070329     D* Parameter:      piIdBollo => ID bollo.
012900070329     D*--------------------------------------------------
013000070329     D Bollo_i_setIdBollo...
013100070329     D                 PR            10I 0
013200070329     D  piIdBollo                     1A
013300070329
013400070329     D*--------------------------------------------------
013500070329     D* Procedure name: Bollo_i_setDataValidita
013600070329     D* Purpose:        Imposta la data per controlli validità.
013700070329     D* Returns:        Esito.
013800070329     D* Parameter:      piDataValidita => Data validita
013900070329     D*--------------------------------------------------
014000070329     D Bollo_i_setDataValidita...
014100070329     D                 PR            10I 0
014200070329     D piDataValidita                10D   DATFMT(*ISO)
014300070329
014400070329     D*--------------------------------------------------
014500070329     D* Procedure name: Bollo_i_setSocieta
014600070329     D* Purpose:        Imposta la societa.
014700070329     D* Returns:        Esito.
014800070329     D* Parameter:      piSocieta
014900070329     D*--------------------------------------------------
015000070329     D Bollo_i_setSocieta...
015100070329     D                 PR            10I 0
015200070329     D  piSocieta                     3A
015300070329
015400070329     D*--------------------------------------------------
015500070329     D* Procedure name: Bollo_i_isVirtuale
015600070329     D* Purpose:        Dice se la società ha il bollo virtuale.
015700070329     D* Returns:        *ON = bollo virtuale.
015800070329     D* Parameter:      piSocieta => Società.
015900070329     D* Parameter:      piEsito => Esito.
016000070329     D*--------------------------------------------------
016100070329     D Bollo_i_isVirtuale...
016200070329     D                 PR              N
016300070330
016400070330     D*--------------------------------------------------
016500070330     D* Procedure name: Bollo_i_getIdBollo
016600070330     D* Purpose:        Reperisce l' ID bollo in uso.
016700070330     D* Returns:        ID bollo.
016800070330     D*--------------------------------------------------
016900070330     D Bollo_i_getIdBollo...
017000070330     D                 PR             1A
017100070330
017200070330     D*--------------------------------------------------
017300070330     D* Procedure name: Bollo_i_getDataValidità
017400070330     D* Purpose:
017500070330     D* Returns:        Data validità.
017600070330     D*--------------------------------------------------
017700070330     D Bollo_i_getDataValidita...
017800070330     D                 PR            10D
017900070330
018000070330     D*--------------------------------------------------
018100070330     D* Procedure name: Bollo_i_getSocieta
018200070330     D* Purpose:        Restituisce la societa.
018300070330     D* Returns:        Società.
018400070330     D*--------------------------------------------------
018500070330     D Bollo_i_getSocieta...
018600070330     D                 PR             3A
018700070330
018800070330     D*--------------------------------------------------
018900070330     D* Procedure name: Bollo_i_getDichiarazioneBolloVirtuale
019000070330     D* Purpose:        Reperisce il testo della dichiarazione del bollo vi...
019100070330     D*                          rtuale.
019200070330     D* Returns:        Testo dichiarazione bollo virtuale.
019300070330     D*--------------------------------------------------
019400070330     D Bollo_i_getDichiarazioneBolloVirtuale...
019500070330     D                 PR           255A   VARYING
019600070330
