000100070531
000200070531     D*--------------------------------------------------
000300070531     D* Procedure name: BFP_declareCursorImporti
000400070531     D* Purpose:        Prepara e dichiara il cursore SQL
000500070531     D* Returns:        Esito.
000600070531     D*--------------------------------------------------
000700070531     D BFP_declareCursorImporti...
000800070531     D                 PR            10I 0
000900070601     D piAnno                        10I 0 VALUE
001000070601     D piMese                        10I 0 VALUE
001100070531     D piSocieta                      3A
001200070531     D piFondo                        2A
001300070601
001400070601     D*--------------------------------------------------
001500070601     D* Procedure name: BFP_openCursorImporti
001600070601     D* Purpose:        Apre il cursore degli importi.
001700070601     D* Returns:        Esito.
001800070601     D*--------------------------------------------------
001900070601     D BFP_openCursorImporti...
002000070601     D                 PR            10I 0
002100070601
002200070601     D*--------------------------------------------------
002300070601     D* Procedure name: BFP_fetchNextCursorImporti
002400070601     D* Purpose:        Legge la riga successiva dal cursore degli importi.
002500070601     D* Returns:        Esito.
002600070601     D*--------------------------------------------------
002700070601     D BFP_fetchNextCursorImporti...
002800070601     D                 PR            10I 0
002900070601
003000070601     D*--------------------------------------------------
003100070601     D* Procedure name: BFP_fetchBeforeCursorImporti
003200070601     D* Purpose:        Posiziona il cursore all'inizio della tabella degli...
003300070601     D*                           importi.
003400070601     D* Returns:        Esito.
003500070601     D*--------------------------------------------------
003600070601     D BFP_fetchBeforeCursorImporti...
003700070601     D                 PR            10I 0
003800070601
003900070601     D*--------------------------------------------------
004000070601     D* Procedure name: BFP_closeCursorImporti
004100070601     D* Purpose:        Chiude il cursore degli importi.
004200070601     D* Returns:        Esito.
004300070601     D*--------------------------------------------------
004400070601     D BFP_closeCursorImporti...
004500070601     D                 PR            10I 0
004600070601
004700070601     D*--------------------------------------------------
004800070601     D* Procedure name: BFP_getSocietaBonifico
004900070601     D* Purpose:        Restituisce la società dell'importo da versare al f...
005000070601     D*                          ondo.
005100070601     D* Returns:        Società.
005200070601     D*--------------------------------------------------
005300070601     D BFP_getSocietaBonifico...
005400070601     D                 PR             3A
005500070601
005600070601     D*--------------------------------------------------
005700070601     D* Procedure name: BFP_getFondoBonifico
005800070601     D* Purpose:        Restituisce il codice del fondo a cui versare l'imp...
005900070601     D*                          orto.
006000070601     D* Returns:        Fondo.
006100070601     D*--------------------------------------------------
006200070601     D BFP_getFondoBonifico...
006300070601     D                 PR             2A
006400070601
006500070601     D*--------------------------------------------------
006600070601     D* Procedure name: BFP_getDescrizioneFondoBonifico
006700070601     D* Purpose:        Restituisce la descrizione del fondo a cui versare.
006800070601     D* Returns:        Descrizione fondo.
006900070601     D*--------------------------------------------------
007000070601     D BFP_getDescrizioneFondoBonifico...
007100070601     D                 PR            60A
007200070601
007300070601     D*--------------------------------------------------
007400070601     D* Procedure name: BFP_getImportoBonifico
007500070601     D* Purpose:        Restituisce l'importo da versareal fondo.
007600070601     D* Returns:        Importo totale da versare.
007700070601     D*--------------------------------------------------
007800070601     D BFP_getImportoBonifico...
007900070601     D                 PR            15P 2
008000070601
008100070601     D*--------------------------------------------------
008200070601     D* Procedure name: BFP_getDataValutaBonifico
008300070601     D* Purpose:        Restituisce la data valuta accredito fondo.
008400070601     D* Returns:        Data valuta accredito.
008500070601     D*--------------------------------------------------
008600070601     D BFP_getDataValutaBonifico...
008700070601     D                 PR            10D   DATFMT(*ISO)
008800070601
008900070601     D*--------------------------------------------------
009000070601     D* Procedure name: BFP_getContoCoGeFondoBonifico
009100070601     D* Purpose:        Restituisce il conto contabile del fondo.
009200070601     D* Returns:
009300070601     D* Parameter:      capoConto => Capo conto.
009400070601     D* Parameter:      sottoConto => Sotto conto
009500070601     D*--------------------------------------------------
009600070601     D BFP_getContoCoGeFondoBonifico...
009700070601     D                 PR
009800070601     D  piCapoConto                   6A
009900070601     D  piSottoConto                  8A
010000070601
010100070601     D*--------------------------------------------------
010200070601     D* Procedure name: BFP_getIbanBonifico
010300070601     D* Purpose:        Restituisce IBAN del fondo.
010400070601     D* Returns:        IBAN fondo.
010500070601     D*--------------------------------------------------
010600070601     D BFP_getIbanBonifico...
010700070601     D                 PR            34A
010800070604
010900070604     D*--------------------------------------------------
011000070604     D* Procedure name: BFP_getSedeLegaleFondo
011100070604     D* Purpose:        Restituisce la sede legale del fondo.
011200070604     D* Returns:        Esito.
011300070604     D* Parameter:      piSocieta => Societa
011400070604     D* Parameter:      piKcc => Capo conto
011500070604     D* Parameter:      piKsc => Sotto conto
011600070604     D* Parameter:      piRagioneSociale => Ragione sociale.
011700070604     D* Parameter:      piIndirizzo => Indirizzo
011800070604     D* Parameter:      piCAP => CAP
011900070604     D* Parameter:      piLocalita => Località
012000070604     D* Parameter:      piSiglaProvincia => Sigla provincia
012100070604     D* Parameter:      piSiglaNazione => Sigla nazione.
012200070604     D*--------------------------------------------------
012300070604     D BFP_getSedeLegaleFondo...
012400070604     D                 PR            10I 0
012500070604     D  piSocieta                     3A
012600070604     D  piKcc                         6A
012700070604     D  piKsc                         8A
012800070604     D  piRagioneSociale...
012900070605     D                               44A   VARYING
013000070605     D  piIndirizzo                  44A   VARYING
013100070604     D  piCAP                         9A
013200070605     D  piLocalita                   34A   VARYING
013300070604     D  piSiglaProvincia...
013400070604     D                                2A
013500070604     D piSiglaNazione                 4A
013600070604
