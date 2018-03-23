000100000000          PGM        PARM(&KPJBA)
000200000000
000300841109  /*-ANG990C-------------------------------------------------------*/
000400000000  /*                                                               */
000500841109  /*           - LANCIO STAMPA ANAGR.MATERIE PRIME -               */
000600841109  /*           -------------------------------------    AZ.: G990  */
000700891127  /*         PGM.:    CNC0M0R   - RICERCA INTESTAZIONE DITTA       */
000800990317  /*                  TII104R   - RICERCA ANAGR.MATERIE PRIME      */
000900990317  /*                  TII110R   - RICERCA CLASSI MERCEOLOGICHE     */
001000990317  /*                  TII111R   - RICERCA S/CLASSI MERCEOLOGICHE   */
001100990317  /*                  TII118R   - RICERCA CATEGORIE FISCALI        */
001200840907  /*                  BCH09     - IMPOSTAZIONE PARAMETRI BATCH     */
001300840907  /*                  BCH10     - LANCIO AZIONE BATCH              */
001400000000  /*                                                               */
001500000000  /*---------------------------------------------------------------*/
001600000000
001700990317DCLF       FILE(*LIBL/TII014D)
001800000000
001900000000             DCL        VAR(&KPJBA)    TYPE(*CHAR)  LEN(502)
002000000000             DCL        VAR(&KNMUS)    TYPE(*CHAR)  LEN(10)
002100840516             DCL        VAR(&KCOAZ)    TYPE(*CHAR)  LEN(4)
002200840516             DCL        VAR(&KQEBI)    TYPE(*CHAR)  LEN(7)
002300860825             DCL        VAR(&KMDST)    TYPE(*CHAR)  LEN(10)
002400000000             DCL        VAR(&CODDIT)   TYPE(*DEC)   LEN(3)
002500000000
002600871019             DCL        VAR(&COD7)     TYPE(*CHAR)  LEN(12)
002700841112             DCL        VAR(&COD5)     TYPE(*CHAR)  LEN(5)
002800841112             DCL        VAR(&COD3)     TYPE(*CHAR)  LEN(3)
002900840907             DCL        VAR(&COD2)     TYPE(*CHAR)  LEN(2)
003000841026             DCL        VAR(&DES30)    TYPE(*CHAR)  LEN(30)
003100840907             DCL        VAR(&DES25)    TYPE(*CHAR)  LEN(25)
003200840907
003300000000 /*--- RICERCA DESCRIZIONE DITTA ------------------------------------*/
003400000000             CHGVAR     VAR(&CODDIT)   VALUE(0)
003500891127CALL       PGM(*LIBL/CNC0M0R) PARM(&KPJBA &CODDIT &DESDIT)
003600000000             IF         COND(&CODDIT *EQ -1) THEN(GOTO CMDLBL(FINE))
003700000000
003800841109 /*--- EMISSIONE FORMATO VIDEO --------------------------------------*/
003900000000             CHGVAR     VAR(&KNMUS)    VALUE(%SST(&KPJBA   1 10))
004000000000
004100841109 TORNA:      CHGVAR     VAR(&ARTINF) VALUE(' ')
004200841109             CHGVAR     VAR(&ARTSUP) VALUE(' ')
004300841112             CHGVAR     VAR(&CLAINF) VALUE(' ')
004400841112             CHGVAR     VAR(&CLASUP) VALUE(' ')
004500841112             CHGVAR     VAR(&SOTINF) VALUE(' ')
004600841112             CHGVAR     VAR(&SOTSUP) VALUE(' ')
004700841113             CHGVAR     VAR(&CATINF) VALUE(' ')
004800841113             CHGVAR     VAR(&CATSUP) VALUE(' ')
004900860825             CHGVAR     VAR(&KMDST)  VALUE(' ')
005000860905             CHGVAR     VAR(&UBICAZ) VALUE('9')
005100860924             CHGVAR     VAR(&ANNULL) VALUE('N')
005200860924
005300860825             CHGVAR     VAR(&TIPSTA) VALUE('P')
005400840907             CHGVAR     VAR(&ORDINE) VALUE('A')
005500840907
005600841109 EMISS:      SNDRCVF    RCDFMT(ANG9901)
005700990409             IF         COND(&IN03) THEN(GOTO CMDLBL(FINE))
005800840907
005900841026             CHGVAR     VAR(&COD7)   VALUE(' ')
006000841112             CHGVAR     VAR(&COD5)   VALUE(' ')
006100841112             CHGVAR     VAR(&COD3)   VALUE(' ')
006200840907             CHGVAR     VAR(&COD2)   VALUE(' ')
006300841026             CHGVAR     VAR(&DES30)  VALUE(' ')
006400840907             CHGVAR     VAR(&DES25)  VALUE(' ')
006500840907
006600840907             CHGVAR     VAR(&ERRORE) VALUE(' ')
006700840907             CHGVAR     VAR(&IN90)   VALUE('0')
006800840907             CHGVAR     VAR(&IN91)   VALUE('0')
006900840907             CHGVAR     VAR(&IN92)   VALUE('0')
007000840907             CHGVAR     VAR(&IN93)   VALUE('0')
007100840907             CHGVAR     VAR(&IN94)   VALUE('0')
007200840907             CHGVAR     VAR(&IN95)   VALUE('0')
007300840907             CHGVAR     VAR(&IN96)   VALUE('0')
007400841112             CHGVAR     VAR(&IN97)   VALUE('0')
007500841112             CHGVAR     VAR(&IN98)   VALUE('0')
007600841112
007700840907 /*--- CONTROLLO SE SCELTA QUALCHE RICERCA --------------------------*/
007800841112             IF         COND((&RICART *NE '?') *AND (&RICCLA *NE '?') +
007900841112                          *AND (&RICSOT *NE '?') *AND (&RICCAT *NE +
008000841112                          '?')) THEN(GOTO CMDLBL(SCELTA))
008100841112
008200841112 /*--- RICERCA ALFABETICA ARTICOLI ----------------------------------*/
008300841109             IF         COND(&RICART *EQ '?') THEN(DO)
008400990317CALL       PGM(*LIBL/TII104R) PARM(&COD7 &DES30)
008500841109             CHGVAR     VAR(&ARTINF) VALUE(&COD7)
008600871019             CHGVAR     VAR(&ARTSUP) VALUE('999999999999')
008700841109             CHGVAR     VAR(&RICART) VALUE(' ')
008800840907             GOTO       CMDLBL(EMISS)
008900840907             ENDDO
009000840907
009100841112 /*--- RICERCA CODIFICA CLASSE MERCEOLOGICA -------------------------*/
009200841112             IF         COND(&RICCLA *EQ '?') THEN(DO)
009300990317CALL       PGM(*LIBL/TII110R) PARM(&COD3 &DES25)
009400841112             CHGVAR     VAR(&CLAINF) VALUE(&COD3)
009500841112             CHGVAR     VAR(&CLASUP) VALUE('999')
009600841112             CHGVAR     VAR(&RICCLA) VALUE(' ')
009700840907             GOTO       CMDLBL(EMISS)
009800840907             ENDDO
009900840907
010000841112 /*--- RICERCA CODIFICA S/CLASSE MERCEOLOGICA -----------------------*/
010100841112             IF         COND(&RICSOT *EQ '?') THEN(DO)
010200990317CALL       PGM(*LIBL/TII111R) PARM(&COD5 &DES25)
010300841112             CHGVAR     VAR(&SOTINF) VALUE(&COD5)
010400841112             CHGVAR     VAR(&SOTSUP) VALUE('99999')
010500841112             CHGVAR     VAR(&RICSOT) VALUE(' ')
010600840907             GOTO       CMDLBL(EMISS)
010700840907             ENDDO
010800840907
010900841112 /*--- RICERCA CODIFICA CATEGORIE FISCALI ---------------------------*/
011000841112             IF         COND(&RICCAT *EQ '?') THEN(DO)
011100990317CALL       PGM(*LIBL/TII118R) PARM(&COD2 &DES25)
011200841112             CHGVAR     VAR(&CATINF) VALUE(&COD2)
011300841112             CHGVAR     VAR(&CATSUP) VALUE('99')
011400841112             CHGVAR     VAR(&RICCAT) VALUE(' ')
011500841112             GOTO       CMDLBL(EMISS)
011600841112             ENDDO
011700841112
011800000000 /*--- NON DIGITATA NESSUNA SCELTA ----------------------------------*/
011900841112 SCELTA:     IF         COND((&ARTINF *EQ ' ') *AND (&ARTSUP *EQ ' ') +
012000841112                          *AND (&CLAINF *EQ ' ') *AND (&CLASUP *EQ ' +
012100841112                          ') *AND (&SOTINF *EQ ' ') *AND (&SOTSUP *EQ +
012200841112                          ' ') *AND (&CATINF *EQ ' ') *AND (&CATSUP +
012300841112                          *EQ ' ')) THEN(DO)
012400000000             CHGVAR     VAR(&IN90) VALUE('1')
012500840516             CHGVAR     VAR(&ERRORE) VALUE('*- Nessun intervallo di +
012600840516                          ricerca battuto -*')
012700000000             GOTO       CMDLBL(EMISS)
012800000000             ENDDO
012900000000
013000841112 /*--- ARTICOLI SUPERIORE/INFERIORE INVERTITI -----------------------*/
013100841109             IF         COND(&ARTINF *GT &ARTSUP) THEN(DO)
013200000000             CHGVAR     VAR(&IN91) VALUE('1')
013300840516             CHGVAR     VAR(&ERRORE) VALUE('*- Intervallo di ricerca +
013400841112                          Prodotti errato -*')
013500000000             GOTO       CMDLBL(EMISS)
013600000000             ENDDO
013700000000
013800841112 /*--- CLASSE SUPERIORE/INFERIORE INVERTITI -------------------------*/
013900841112             IF         COND(&CLAINF *GT &CLASUP) THEN(DO)
014000000000             CHGVAR     VAR(&IN92) VALUE('1')
014100840516             CHGVAR     VAR(&ERRORE) VALUE('*- Intervallo di ricerca +
014200841112                          Classi Merceologiche errato -*')
014300000000             GOTO       CMDLBL(EMISS)
014400000000             ENDDO
014500000000
014600841112 /*--- S/CLASSE SUPERIORE/INFERIORE INVERTITI -----------------------*/
014700841112             IF         COND(&SOTINF *GT &SOTSUP) THEN(DO)
014800000000             CHGVAR     VAR(&IN93) VALUE('1')
014900841112             CHGVAR     VAR(&ERRORE) VALUE('*- Intervallo di ricerca +
015000841112                          Sotto Classi Merceologiche errato -*')
015100000000             GOTO       CMDLBL(EMISS)
015200000000             ENDDO
015300000000
015400841112 /*--- CATEGORIE FISCALI SUPERIORE/INFERIORE INVERTITE --------------*/
015500841112             IF         COND(&CATINF *GT &CATSUP) THEN(DO)
015600841112             CHGVAR     VAR(&IN97) VALUE('1')
015700841112             CHGVAR     VAR(&ERRORE) VALUE('*- Intervallo di ricerca +
015800841112                          Categorie Fiscali errato -*')
015900841112             GOTO       CMDLBL(EMISS)
016000841112             ENDDO
016100841112
016200840517 /*--- TIPO STAMPA ERRATO -------------------------------------------*/
016300840517             IF         COND((&TIPSTA *NE 'C') *AND (&TIPSTA *NE +
016400860825                          'P') *AND (&TIPSTA *NE 'E')) THEN(DO)
016500840517             CHGVAR     VAR(&IN95) VALUE('1')
016600840517             CHGVAR     VAR(&ERRORE) VALUE('*- Tipo stampa errato -*')
016700840517             GOTO       CMDLBL(EMISS)
016800840517             ENDDO
016900840517
017000841112 /*--- ORDINE ALFABETICO SU ARTICOLO CON PARZIALIZZAZIONE -----------*/
017100840522             IF         COND(&ORDINE *NE 'A') THEN(GOTO CMDLBL(NOALFA))
017200871019             IF         COND((&ARTINF *EQ '            ') *AND +
017300871019                          (&ARTSUP *EQ '999999999999')) THEN(GOTO +
017400871019                          CMDLBL(NOALFA))
017500871019             IF         COND((&ARTINF *EQ '            ') *AND +
017600871019                          (&ARTSUP *EQ '            ')) THEN(GOTO +
017700871019                          CMDLBL(NOALFA))
017800840516             CHGVAR     VAR(&IN94) VALUE('1')
017900841112             CHGVAR     VAR(&ERRORE) VALUE('*- Ordinamento Anagrafico +
018000841112                          Prodotti con intervallo di parzializzazione +
018100841112                          -*')
018200840522             GOTO       CMDLBL(EMISS)
018300840522
018400840517 /*--- ORDINAMENTO STAMPA ERRATO ------------------------------------*/
018500840522 NOALFA:     IF         COND((&ORDINE *NE 'A') *AND (&ORDINE *NE +
018600840522                          'C')) THEN(DO)
018700840517             CHGVAR     VAR(&IN96) VALUE('1')
018800840517             CHGVAR     VAR(&ERRORE) VALUE('*- Ordinamento stampa +
018900840517                          errato -*')
019000840517             GOTO       CMDLBL(EMISS)
019100840517             ENDDO
019200840517
019300841112 /*--- ORDINAMENTO STAMPA X CODICE SOLO X PRODOTTO ------------------*/
019400841112             IF         COND(&ORDINE *EQ 'A') THEN(GOTO CMDLBL(SOTALF))
019500841112             IF         COND((&CLAINF *NE ' ') *OR (&CLASUP *NE ' ')) +
019600841112                          THEN(DO)
019700841112             CHGVAR     VAR(&IN98) VALUE('1')
019800841112             CHGVAR     VAR(&ERRORE) VALUE('*- Ordinamento stampa +
019900841112                          consentito solamente per Prodotto -*')
020000841112             GOTO       CMDLBL(EMISS)
020100841112             ENDDO
020200841112
020300841112             IF         COND((&SOTINF *NE ' ') *OR (&SOTSUP *NE ' ')) +
020400841112                          THEN(DO)
020500841112             CHGVAR     VAR(&IN98) VALUE('1')
020600841112             CHGVAR     VAR(&ERRORE) VALUE('*- Ordinamento stampa +
020700841112                          consentito solamente per Prodotto -*')
020800841112             GOTO       CMDLBL(EMISS)
020900841112             ENDDO
021000841112
021100841112             IF         COND((&CATINF *NE ' ') *OR (&CATSUP *NE ' ')) +
021200841112                          THEN(DO)
021300841112             CHGVAR     VAR(&IN98) VALUE('1')
021400841112             CHGVAR     VAR(&ERRORE) VALUE('*- Ordinamento stampa +
021500841112                          consentito solamente per Prodotto -*')
021600841112             GOTO       CMDLBL(EMISS)
021700841112             ENDDO
021800841112
021900841112
022000990317 /*--- PREPARAZIONE PARAMETRI PER LANCIO STAMPA --TII013R------------*/
022100990317SOTALF:      CHGVAR     VAR(&KCOAZ) VALUE('I013')
022200840516             CHGVAR     VAR(&KQEBI) VALUE('KSDIFFE')
022300860825             IF         COND(&TIPSTA *EQ 'E') THEN(CHGVAR VAR(&KMDST) +
022400860825                          VALUE('ETICHETTE'))
022500891127             CHGVAR     VAR(%SST(&KPJBA 135 4)) VALUE(&KCOAZ)
022600891127             CHGVAR     VAR(%SST(&KPJBA 156 7)) VALUE(&KQEBI)
022700860826             CHGVAR     VAR(%SST(&KPJBA 166 10)) VALUE(&KMDST)
022800860826
022900871019             CHGVAR     VAR(%SST(&KPJBA 285 12)) VALUE(&ARTINF)
023000871019             CHGVAR     VAR(%SST(&KPJBA 297 12)) VALUE(&ARTSUP)
023100841112             CHGVAR     VAR(%SST(&KPJBA 261 3)) VALUE(&CLAINF)
023200841112             CHGVAR     VAR(%SST(&KPJBA 264 3)) VALUE(&CLASUP)
023300841112             CHGVAR     VAR(%SST(&KPJBA 267 5)) VALUE(&SOTINF)
023400841112             CHGVAR     VAR(%SST(&KPJBA 272 5)) VALUE(&SOTSUP)
023500841112             CHGVAR     VAR(%SST(&KPJBA 277 2)) VALUE(&CATINF)
023600841112             CHGVAR     VAR(%SST(&KPJBA 279 2)) VALUE(&CATSUP)
023700841112             CHGVAR     VAR(%SST(&KPJBA 281 1)) VALUE(&TIPSTA)
023800841112             CHGVAR     VAR(%SST(&KPJBA 282 1)) VALUE(&ORDINE)
023900860905             CHGVAR     VAR(%SST(&KPJBA 283 1)) VALUE(&UBICAZ)
024000860924             CHGVAR     VAR(%SST(&KPJBA 284 1)) VALUE(&ANNULL)
024100860924
024200840517 /*--- IMPOSTAZIONE PARAMETRI BATCH E LANCIO AZIONE STAMPA ----------*/
024300840517LANCIO:    CALL PGM(*LIBL/BCH09) PARM(&KPJBA)
024400840517CALL       PGM(*LIBL/BCH10) PARM(&KPJBA)
024500840517
024600840906             GOTO       CMDLBL(TORNA)
024700840906
024800000000 /*--- USCITA PROGRAMMA ---------------------------------------------*/
024900000000 FINE:       ENDPGM
