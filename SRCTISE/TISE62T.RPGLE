000100000000     C*------------------------------------------------------------------------*
000200000000     C*?ROUTINE DI CONTROLLO SALDO CLIENTE CON I PARAMETRI GENERALI RICHIESTI
000300000000     C*------------------------------------------------------------------------*
000400000000     C* INPUT :  CAMPI DEL RECORD LETTO DA SDC O DA SDA E L'INDICAZIONE SU QUALE
000500000000     C*          TIPO DI FILE SI STA FACENDO IL CONTROLLO
000600000000     C*          $ROK - RECORD GIUSTO  ('S')
000700000000     C*          $SDX - FILE CHE SI DEVE CONTROLLARE (SDC/SDA)
000800000000     C*          $TSP - TIPO SERVIZIO
000900000000     C*          $TPO - TIPO PORTO
001000000000     C***        $TPR - TIPO PRODOTTO
001100000000     C*          $FPE - FASCIA DI PESO
001200000000     C*          $RBL - RAGGRUPPAMENTO BOLLE
001300000000     C*
001400000000     C* OUTPUT:  RECORD PASSATO AI CONTROLLI OPPPURE NO
001500000000     C*          $ROK - RECORD GIUSTO O SBAGLIATO  ('S'/'N')
001600000000     C*------------------------------------------------------------------------*
001700000000     C*?MAIN LINE
001800000000     C*------------------------------------------------------------------------*
001900000000     C*--------------------
002000000000     C* TIPO SERVIZIO
002100000000     C*--------------------
002200000000     C* SE NON HO SELEZIONATO TIPI SERVIZIO, NON FACCIO IL CONTR. DI ESCLUSIONE
002300000000IF  1C     SELSER        IFNE      'T'
002400000000IF  2C     $TSP          IFEQ      D60SE1                                       *FA PARTE DEI SELEZ
002500000000     C     $TSP          OREQ      D60SE2
002600000000     C     $TSP          OREQ      D60SE3
002700000000     C     $TSP          OREQ      D60SE4
002800000000     C     $TSP          OREQ      D60SE5
002900000000X   2C                   ELSE
003000000000     C                   MOVEL     'N'           $ROK                           *RECORD NON VALIDO
003100000000E   2C                   ENDIF
003200000000E   1C                   ENDIF
003300000000     C*
003400000000     C* CONTROLLO PORTO, PRODOTTO, TARIFFE, FASCE E RAGGRUPPAMENTO BOLLA SOLO SE
003500000000     C* STO LEGGENDO RECORD DI "SDC" PERCHE' IN "SDA" QUESTI CAMPI NON ESISTONO
003600000000IF  1C     $SDX          IFEQ      'SDC'
003700000000     C*--------------------
003800000000     C* RAGGRUPPAMENTO BOLLA
003900000000     C*--------------------
004000000000     C* ESCLUDO LE BOLLE CHE NON SONO VALIDE COME N° DI SPEDIZIONE E COME RICAVI
004100000000     C*----------          Z-ADD1         G
004200000000     C*----------          SETOF                         28
004300000000     C*----------$RBL      LOKUPFLAB,G                   28
004400000000IF  1C*----------*IN28     IFEQ *OFF                       *TIPO NON TROVATO -->
004500000000     C*----------          MOVEL'N'       $ROK             *RECORD NON VALIDO
004600000000X   1C*----------          ELSE
004700000000IF  2C*----------FSP,G     IFNE 'S'                        BOLLA CON SPED.VALIDE
004800000000     C*----------FRI,G     ANDNE'S'                        BOLLA CON RIC .VALIDI
004900000000     C*----------          MOVEL'N'       $ROK
005000000000E   2C*----------          ENDIF
005100000000E   1C*----------          ENDIF
005200000000     C*--------------------
005300000000     C* TIPO PORTO
005400000000     C*--------------------
005500000000     C* SE NON HO SELEZIONATO TIPI PORTO, NON FACCIO IL CONTROLLO DI ESCLUSIONE
005600000000IF  2C     SELPOR        IFNE      'T'
005700000000IF  3C     $TPO          IFEQ      D60PO1                                       *FA PARTE DELLA SELE
005800000000     C     $TPO          OREQ      D60PO2
005900000000X   3C                   ELSE
006000000000     C                   MOVEL     'N'           $ROK                           *RECORD NON VALIDO
006100000000E   3C                   ENDIF
006200000000E   2C                   ENDIF
006300000000     C*--------------------
006400000000     C* TIPO PRODOTTO
006500000000     C*--------------------
006600000000     C* SE NON HO SELEZIONATO TIPI PRODOTTO, NON FACCIO IL CONTR. DI ESCLUSIONE
006700000000IF  2C***        SELPRO    IFNE 'T'
006800000000IF  3C***        $TPR      IFEQ D60PR1                     *FA PARTE DELLA SELEZ
006900000000     C***        $TPR      OREQ D60PR2
007000000000     C***        $TPR      OREQ D60PR3
007100000000     C***        $TPR      OREQ D60PR4
007200000000     C***        $TPR      OREQ D60PR5
007300000000X   3C***                  ELSE
007400000000     C***                  MOVEL'N'       $ROK             *RECORD NON VALIDO
007500000000E   3C***                  ENDIF
007600000000E   2C***                  ENDIF
007700000000     C*--------------------
007800000000     C* CODICE TARIFFA
007900000000     C*--------------------
008000000000     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
008100000000IF  2C     SELCTR        IFNE      'T'
008200000000IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
008300000000     C     $CTR          OREQ      D60CT2
008400000000     C     $CTR          OREQ      D60CT3
008500000000     C     $CTR          OREQ      D60CT4
008600000000     C     $CTR          OREQ      D60CT5
008700000000     C     $CTR          OREQ      D60CT6
008800000000     C     $CTR          OREQ      D60CT7
008900000000     C     $CTR          OREQ      D60CT8
009000000000     C     $CTR          OREQ      D60CT9
009100000000     C     $CTR          OREQ      D60CT0
009200000000X   3C                   ELSE
009300000000     C                   MOVEL     'N'           $ROK                           *RECORD NON VALIDO
009400000000E   3C                   ENDIF
009500000000E   2C                   ENDIF
009600000000     C*--------------------
009700000000     C* FASCIA DI PESO
009800000000     C*--------------------
009900000000     C* SE NON HO SELEZIONATO FASCE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
010000000000IF  2C     SELFPE        IFNE      'T'
010100000000IF  3C     $FPE          IFEQ      D60FP1                                       *FA PARTE DELLA SELE
010200000000     C     $FPE          OREQ      D60FP2
010300000000     C     $FPE          OREQ      D60FP3
010400000000     C     $FPE          OREQ      D60FP4
010500000000     C     $FPE          OREQ      D60FP5
010600000000     C     $FPE          OREQ      D60FP6
010700000000     C     $FPE          OREQ      D60FP7
010800000000     C     $FPE          OREQ      D60FP8
010900000000     C     $FPE          OREQ      D60FP9
011000000000     C     $FPE          OREQ      D60FP0
011100000000     C     $FPE          OREQ      D60FPA
011200000000X   3C                   ELSE
011300000000     C                   MOVEL     'N'           $ROK                           *RECORD NON VALIDO
011400000000E   3C                   ENDIF
011500000000E   2C                   ENDIF
011600000000     C*
011700000000E   1C                   ENDIF
