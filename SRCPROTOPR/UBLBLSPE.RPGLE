000100080612     P*--------------------------------------------------
000200080612     P* Procedure name: UBLBLSPE_Init
000300080612     P* Purpose:        Inizializzazioni
000400080612     P* Returns:        Integer (Status_Code)
000500080612     P*--------------------------------------------------
000600080612     D*
000700080612     D UBLBLSPE_Init...
000800080612     D                 PR            10I 0
000900080612     D*
001000080612     P*--------------------------------------------------
001100080612     P* Procedure name: UBLBLSPE_Finalize
001200080612     P* Purpose:        Finalizzazioni
001300080612     P* Returns:        Integer (Status_Code)
001400080612     P*--------------------------------------------------
001500080612     D*
001600080612     D UBLBLSPE_Finalize...
001700080612     D                 PR            10I 0
001800080612     D*
001900080612     P*--------------------------------------------------
002000080612     P* Procedure name: UBLBLSPE_GetLblTyp
002100080612     P* Purpose:        Verifica il tipo di legame della bolla corrente
002200080612     P*                 e ritorna la chiave della relativa bolla originale
002300080612     P* Returns:        Integer (Status_Code)
002400080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
002500080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
002600080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
002700080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
002800080612     P* Parameter:      pOutLblTyp     => Tipo del legame
002900080612     P* Parameter:      pOutAnnoBO     => Anno           sped. originale
003000080612     P* Parameter:      pOutLineaParBO => Linea partenza sped. originale
003100080612     P* Parameter:      pOutSerieBO    => Serie          sped. originale
003200080612     P* Parameter:      pOutNumSpedBO  => Numero         sped. originale
003300080613     P* Parameter:      pOutDcmBO      => Data consegna  sped. originale
003400080613     P* Parameter:      pOutCcaBO      => Cod. anomalia  sped. originale
003500080806     P* Parameter:      pOutRblBO      => Raggr. TBL     sped. originale
003600080612     P*--------------------------------------------------
003700080612     D*
003800080612     D UBLBLSPE_GetLblTyp...
003900080612     D                 PR            10I 0
004000080612     D pInAnno                        4S 0 CONST
004100080612     D pInLineaPar                    3S 0 CONST
004200080612     D pInSerie                       2S 0 CONST
004300080612     D pInNumSped                     7S 0 CONST
004400080612     D pOutLblTyp                     1A
004500080612     D pOutAnnoBO                     4S 0
004600080612     D pOutLineaParBO                 3S 0
004700080612     D pOutSerieBO                    2S 0
004800080612     D pOutNumSpedBO                  7S 0
004900080613     D pOutDcmBO                      8S 0
005000080613     D pOutCcaBO                      1A
005100080806     D pOutRblBO                      1A
005200080612     D*
005300080612     P*--------------------------------------------------
005400080612     P* Procedure name: UBLBLSPE_GetLastChild
005500080612     P* Purpose:        Rispetto alla bolla corrente reperisce l'ultima bolla
005600080612     P*                 figlia ed il relativo stato attuale
005700080612     P* Returns:        Integer (Status_Code)
005800080612     P* Parameter:      pInAnno        => Anno           sped. da verificare
005900080612     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
006000080612     P* Parameter:      pInSerie       => Serie          sped. da verificare
006100080612     P* Parameter:      pInNumSped     => Numero         sped. da verificare
006200080612     P* Parameter:      pOutAnnoFI     => Anno           sped. ultima figlia
006300080612     P* Parameter:      pOutLineaParFI => Linea partenza sped. ultima figlia
006400080612     P* Parameter:      pOutSerieFI    => Serie          sped. ultima figlia
006500080612     P* Parameter:      pOutNumSpedFI  => Numero         sped. ultima figlia
006600080612     P* Parameter:      pOutDcmFI      => Data consegna dell'ultima bolla figlia
006700080612     P* Parameter:      pOutCcaFI      => Codice consegna anomala dell'ultima bolla figlia
006800080612     P*--------------------------------------------------
006900080612     D*
007000080612     D UBLBLSPE_GetLastChild...
007100080612     D                 PR            10I 0
007200080612     D pInAnno                        4S 0 CONST
007300080612     D pInLineaPar                    3S 0 CONST
007400080612     D pInSerie                       2S 0 CONST
007500080612     D pInNumSped                     7S 0 CONST
007600080612     D pOutAnnoFI                     4S 0
007700080612     D pOutLineaParFI                 3S 0
007800080612     D pOutSerieFI                    2S 0
007900080612     D pOutNumSpedFI                  7S 0
008000080612     D pOutDcmFI                      8S 0
008100080612     D pOutCcaFI                      1A
008200080612     D*
008300100107     P*--------------------------------------------------
008400100107     P* Procedure name: UBLBLSPE_GetFirstChild
008500100107     P* Purpose:        Rispetto alla bolla corrente reperisce la prima bolla
008600100107     P*                 figlia ed il relativo stato attuale
008700100107     P* Returns:        Integer (Status_Code)
008800100107     P* Parameter:      pInAnno        => Anno           sped. da verificare
008900100107     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
009000100107     P* Parameter:      pInSerie       => Serie          sped. da verificare
009100100107     P* Parameter:      pInNumSped     => Numero         sped. da verificare
009200100107     P* Parameter:      pOutAnnoFFI    => Anno           sped. prima  figlia
009300100107     P* Parameter:      pOutLineaPaFFI => Linea partenza sped. prima  figlia
009400100107     P* Parameter:      pOutSerieFFI   => Serie          sped. prima  figlia
009500100107     P* Parameter:      pOutNumSpedFFI => Numero         sped. prima  figlia
009600100107     P* Parameter:      pOutDcmFFI     => Data consegna della prima bolla figlia
009700100107     P* Parameter:      pOutCcaFFI     => Codice consegna anomala della prima bolla figlia
009800100107     P*--------------------------------------------------
009900100107     D*
010000100107     D UBLBLSPE_GetFirstChild...
010100100107     D                 PR            10I 0
010200100107     D pInAnno                        4S 0 CONST
010300100107     D pInLineaPar                    3S 0 CONST
010400100107     D pInSerie                       2S 0 CONST
010500100107     D pInNumSped                     7S 0 CONST
010600100107     D pOutAnnoFFI                    4S 0
010700100107     D pOutLineaPaFFI                 3S 0
010800100107     D pOutSerieFFI                   2S 0
010900100107     D pOutNumSpedFFI                 7S 0
011000100107     D pOutDcmFFI                     8S 0
011100100107     D pOutCcaFFI                     1A
011200100107     D*
011300130919     P*--------------------------------------------------
011400130919     P* Procedure name: UBLBLSPE_GetMother
011500130919     P* Purpose:        Rispetto alla bolla corrente reperisce la propria
011600130919     P*                 bolla mamma ed il relativo stato attuale
011700130919     P* Returns:        Integer (Status_Code)
011800130919     P* Parameter:      pInAnno        => Anno           sped. da verificare
011900130919     P* Parameter:      pInLineaPar    => Linea partenza sped. da verificare
012000130919     P* Parameter:      pInSerie       => Serie          sped. da verificare
012100130919     P* Parameter:      pInNumSped     => Numero         sped. da verificare
012200130919     P* Parameter:      pOutAnnoMAM    => Anno           sped. mamma
012300130919     P* Parameter:      pOutLineaPaMAM => Linea partenza sped. mamma
012400130919     P* Parameter:      pOutSerieMAM   => Serie          sped. mamma
012500130919     P* Parameter:      pOutNumSpedMAM => Numero         sped. mamma
012600130919     P* Parameter:      pOutDcmMAM     => Data consegna della propria bolla mamma
012700130919     P* Parameter:      pOutCcaMAM     => Codice consegna anomala della propria bolla mamma
012800130919     P*--------------------------------------------------
012900130919     D*
013000130919     D UBLBLSPE_GetMother...
013100130919     D                 PR            10I 0
013200130919     D pInAnno                        4S 0 CONST
013300130919     D pInLineaPar                    3S 0 CONST
013400130919     D pInSerie                       2S 0 CONST
013500130919     D pInNumSped                     7S 0 CONST
013600130919     D pOutAnnoMAM                    4S 0
013700130919     D pOutLineaPaMAM                 3S 0
013800130919     D pOutSerieMAM                   2S 0
013900130919     D pOutNumSpedMAM                 7S 0
014000130919     D pOutDcmMAM                     8S 0
014100130919     D pOutCcaMAM                     1A
014200130919     D*
