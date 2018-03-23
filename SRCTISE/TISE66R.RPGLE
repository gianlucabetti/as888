000100000000     H*------------------------------------------------------------------------*
000200000000     H*                   - REPERIMENTO DATI CLIENTE -                         *
000300000000     H*------------------------------------------------------------------------*
000400020117     H* INPUT  --> CODICE CLIENTE NELLA DS DI PROCEDURA 'TISE66DS'
000500000000     H* OUTPUT --> DATI GENERALI DEL CLIENTE NELLA DS DI PROCEDURA
000600000000     H*------------------------------------------------------------------------*
000700000000     H DECEDIT('0,') DATEDIT(*DMY.)
000800000000     H*------------------------------------------------------------------------*
000900000000     H* INDICATORI USATI
001000000000     H*------------------------------------------------------------------------*
001100000000     H* 99 - DI COMODO
001200000000     F*------------------------------------------------------------------------*
001300000000     F* DATA BASE
001400000000     F*------------------------------------------------------------------------*
001500000000     FTABEL00F  IF   E           K DISK
001600000307     F*---CNACO00FIF  E           K        DISK
001700000307     F*---CNIND00FIF  E           K        DISK
001800000307     F*---CNCLP00FIF  E           K        DISK
001900140616     FAZCMM01L  IF   E           K DISK
002000000000     D*------------------------------------------------------------------------*
002100000000     D* INPUT
002200000000     D*------------------------------------------------------------------------*
002300000000     D*------------------
002400000000     D* DS PROCEDURA
002500000000     D*------------------
002600020117     D DSCL          E DS                  EXTNAME(TISE66DS)
002700000000     D*------------------
002800000000     D* DS CONDIZIONI PAGAMENTO
002900000000     D*------------------
003000000000     D DSFA          E DS
003100000000     D*------------------
003200000000     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
003300000000     D*------------------
003400000000     D DSCDP           DS
003500000000     D  WCDP                   1      3
003600000000     D  WLIN                   4      4
003700000000     D  WBLK                   5      8
003800000000     D*------------------
003900000000     D* DS CLIENTI CON SOLUZIONI
004000000000     D*------------------
004100000000     D DS3C          E DS
004200000000     D*------------------
004300000000     D* DS TRATTAMENTO MERCI
004400000000     D*------------------
004500000000     D DS1B          E DS
004600000000     D*------------------
004700000000     D* DS PROVINCE
004800000000     D*------------------
004900000000     D DSPR          E DS
005000000000     D*------------------
005100000000     D* DS SETTORI MERCEOLOGICI
005200000000     D*------------------
005300951006     D DS1L          E DS
005400000307     D*------------------
005500000307     D* DS REPERIMENTO DATI UTENTE
005600000307     D*-------------------
005700000307     D BS69DS        E DS                  EXTNAME(TIBS69DS)
005800000307     D ACODS         E DS                  EXTNAME(CNACO00F)
005900000307     D INDDS         E DS                  EXTNAME(CNIND00F)
006000000307     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006100000307     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006200000000     C*------------------------------------------------------------------------*
006300000000     C*?MAIN - REPERIMENTO DATI DEL CLIENTE
006400000000     C*------------------------------------------------------------------------*
006500000000     C*
006600000307     C*---                 Z-ADDDCLKUT    KACKUT           *UTENTE
006700000307     C*---                 Z-ADDDCLKCC    KACKCC           *CAPO CONTO
006800000307     C*---                 MOVELDCLKSC    KACKSC           *SOTTOCONTO
006900000307     C*---
007000000307     C*---       KEYCLI    CHAINCNACO00F             99
007100000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
007200000307     C*---                 CLEARCNACO000                   *IMPOSTO CAMPI VUOTI
007300000307E   1C*---                 ENDIF
007400000307     C*---
007500000307     C*---       KEYCLI    CHAINCNIND00F             99
007600000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
007700000307     C*---                 CLEARCNIND000                   *IMPOSTO CAMPI VUOTI
007800000307E   1C*---                 ENDIF
007900000307     C*---
008000000307     C*---       KEYCLI    CHAINCNCLP00F             99
008100000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
008200000307     C*---                 CLEARCNCLP000                   *IMPOSTO CAMPI VUOTI
008300000307E   1C*---                 ENDIF
008400000307     C*
008500000307     C                   CLEAR                   BS69DS
008600000307     C                   CLEAR                   ACODS
008700000307     C                   CLEAR                   CLPDS
008800000307     C                   CLEAR                   INDDS
008900000307     C                   Z-ADD     DCLKCC        I69KCC                         *CAPOCONTO
009000000307     C                   MOVEL     DCLKSC        I69KAC                         *CODICE X CNACO
009100000307     C                   MOVEL     DCLKSC        I69KIN                         *CODICE X CNNDO
009200000307     C                   MOVEL     DCLKSC        I69KCP                         *CODICE X CNCLP
009300000307     C                   CALL      'TIBS69R'
009400000307     C                   PARM                    BS69DS
009500000307     C                   PARM                    ACODS
009600000307     C                   PARM                    INDDS
009700000307     C                   PARM                    CLPDS
009800000307     C                   PARM                    CLSDS
009900000000     C*
010000000000     C* RIEMPO DA DS DI RITORNO
010100000000     C                   MOVEL     ACORAG        DCLRAG
010200951006     C                   Z-ADD     ACOITC        DCLITC
010300000000     C                   MOVEL     INDVIA        DCLVIA
010400000000     C                   MOVEL     INDCIT        DCLCIT
010500000000     C                   MOVEL     INDCAE        DCLCAP
010600000000     C                   MOVEL     INDPRV        DCLPRV
010700000000     C                   MOVEL     INDSTA        DCLSTA
010800000000     C                   MOVEL     INDTEL        DCLTEL
010900000000     C                   MOVEL     CLPCLV        DCLCLV
011000000000IF  1C     DCLCLV        IFEQ      *BLANKS                                      *SE QUALITA' VUOTA
011100000000     C                   MOVEL     'C'           DCLCLV                         *E' CLIENTE DI QL 'C
011200000000E   1C                   ENDIF
011300000000     C*
011400000000     C                   Z-ADD     CLPDPS        $DATA
011500000000     C                   EXSR      ROVDAT                                       *ROVESCIA DATA
011600000000     C                   Z-ADD     $DATA         DCLDPS
011700000000     C*
011800000000     C                   Z-ADD     CLPAGE        DCLAGE
011900000000     C                   MOVE      INDCDP        DCLCDP
012000000000     C*
012100000000     C                   MOVEL     DCLKSC        $KSC              7
012200000000     C                   MOVEL     *BLANKS       $SOL              1
012300000000     C                   MOVEL     *BLANKS       $CTM              2
012400000000     C                   EXSR      REPSOL                                       *REPERISCE SOLUZIONE
012500000000     C                   MOVEL     $SOL          DCLSOL
012600000000     C**
012700000000     C* D E C O D I F I C H E
012800000000     C***
012900000000     C*--------------------
013000000000     C* PROVINCIA
013100000000     C*--------------------
013200000000IF  1C     DCLPRV        IFNE      *BLANKS
013300000000     C                   Z-ADD     DCLKUT        TBLKUT
013400000000     C                   MOVEL     'PR'          TBLCOD
013500000000     C                   MOVEL     *BLANKS       TBLKEY
013600000000     C                   MOVEL     DCLPRV        TBLKEY
013700000000     C     KEYTAB        CHAIN     TABEL00F                           99
013800000000IF  2C     *IN99         IFEQ      *OFF
013900000000     C                   MOVEL     TBLUNI        DSPR
014000000000     C                   MOVEL     §PRDES        DCLDPR
014100000000X   2C                   ELSE
014200000000     C                   MOVEL     *ALL'*'       DCLDPR
014300000000E   2C                   ENDIF
014400000000E   1C                   ENDIF
014500000000     C*--------------------
014600000000     C* SETTORE MERCEOLOGICO
014700000000     C*--------------------
014800951006IF  1C     DCLITC        IFNE      *ZEROS
014900000000     C                   Z-ADD     DCLKUT        TBLKUT
015000951006     C                   MOVEL     '1L'          TBLCOD
015100951006     C                   MOVE      DCLITC        NUM5              5 0
015200951006     C                   MOVEL     *BLANKS       TBLKEY
015300951006     C                   MOVEL     NUM5          TBLKEY
015400000000     C     KEYTAB        CHAIN     TABEL00F                           99
015500000000IF  2C     *IN99         IFEQ      *OFF
015600951006     C                   MOVEL     TBLUNI        DS1L
015700951006     C                   MOVEL     §1LDES        DCLDIT
015800000000X   2C                   ELSE
015900951006     C                   MOVEL     *ALL'*'       DCLDIT
016000000000E   2C                   ENDIF
016100000000E   1C                   ENDIF
016200000000     C*--------------------
016300000000     C* AGENTE COMMERCIALE
016400000000     C*--------------------
016500000000IF  1C     DCLAGE        IFGT      *ZEROS
016600140616     C* Reperisco le informazioni dell'agente commerciale
016700140616     C                   EVAL      CMMCod = dclage
016800140616     C     CMMCod        CHAIN     AZCMM01l
016900140616     C                   IF        %found(AZCMM01L)
017000140616     C                   MOVEL     cmmdes        DCLDAG
017100000000X   2C                   ELSE
017200000000     C                   MOVEL     *ALL'*'       DCLDAG
017300000000E   2C                   ENDIF
017400000000E   1C                   ENDIF
017500000000     C*--------------------
017600000000     C* CONDIZIONI DI PAGAMENTO
017700000000     C*--------------------
017800000000IF  1C     DCLCDP        IFNE      *BLANKS
017900000000     C                   Z-ADD     DCLKUT        TBLKUT
018000000000     C                   MOVEL     'FA'          TBLCOD
018100000000     C                   MOVEL     *BLANKS       TBLKEY
018200000000     C                   MOVEL     DCLCDP        WCDP                           *CONDIZIONE PAGAMENT
018300000323     C                   MOVEL     '1'           WLIN                           *LINGUA
018400000000     C                   MOVEL     DSCDP         TBLKEY                         *COND + LINGUA + ' '
018500000000     C     KEYTAB        CHAIN     TABEL00F                           99
018600000000IF  2C     *IN99         IFEQ      *OFF
018700000000     C                   MOVEL     TBLUNI        DSFA
018800000000     C                   MOVEL     §FADES        DCLDCD
018900000000X   2C                   ELSE
019000000000     C                   MOVEL     *ALL'*'       DCLDCD
019100000000E   2C                   ENDIF
019200000000E   1C                   ENDIF
019300000000     C*--------------------
019400000000     C* PASSAGGIO DATI
019500000000     C*--------------------
019600000000IF  1C     DCLSOL        IFNE      *BLANKS
019700000000     C                   Z-ADD     DCLKUT        TBLKUT
019800000000     C                   MOVEL     '1B'          TBLCOD
019900000000     C                   MOVEL     *BLANKS       TBLKEY
020000000000     C                   MOVEL     $CTM          TBLKEY                         *TRATTAMENTO MERCE
020100000000     C     KEYTAB        CHAIN     TABEL00F                           99
020200000000IF  2C     *IN99         IFEQ      *OFF
020300000000     C                   MOVEL     TBLUNI        DS1B
020400000000     C                   MOVEL     §1BDES        DCLDSO
020500000000X   2C                   ELSE
020600000000     C                   MOVEL     *ALL'*'       DCLDSO
020700000000E   2C                   ENDIF
020800000000X   1C                   ELSE
020900000000     C                   MOVEL     'NESSUNO'     DCLDSO
021000000000E   1C                   ENDIF
021100000000     C*
021200000307     C* OPERAZIONI FINALI
021300000307     C                   EXSR      FINSR
021400000307     C*
021500000000     C                   SETON                                        LR
021600000000     C*------------------------------------------------------------------------*
021700000000     C*?ROVDAT - ROVESCIA LE DATE
021800000000     C*          INPUT  --> $DATA: DATA
021900000000     C*          OUTPUT --> $DATA :DATA ROVESCIATA
022000000000     C*------------------------------------------------------------------------*
022100000000     C     ROVDAT        BEGSR
022200000000     C*
022300000000     C                   MOVEL     $DATA         DD1               2 0
022400000000     C                   MOVE      $DATA         DD2               2 0
022500000000     C                   MOVE      DD1           $DATA             6 0
022600000000     C                   MOVEL     DD2           $DATA
022700000000     C*
022800000000     C                   ENDSR
022900000000     C*------------------------------------------------------------------------*
023000000000     C*?REPSOL - REPERISCE LA SOLUZIONE DI PASSAGGIO DATI ATTUATA DAL CLIENTE
023100000000     C*          INPUT  --> $KSC : CODICE CLIENTE
023200000000     C*                     $SOL : CODICE SOLUZIONE VUOTO
023300000000     C*                     $CTM : CODICE TRATTAMENTO MERCE VUOTO
023400000000     C*          OUTPUT --> $SOL : CODICE SOLUZIONE (A/B)
023500000000     C*                     $CTM : CODICE TRATTAMENTO MERCE DEL CLIENTE
023600000000     C*------------------------------------------------------------------------*
023700000000     C     REPSOL        BEGSR
023800000000     C*
023900000000     C* CLIENTE PRESENTE IN TABELLA 3C --> PARTITO CON LA PROCEDURA SCAMBIO DATI
024000000000     C* NON HA IL NUMERO DI SERIE --> PROCEDURA A --> STAMPA LE BOLLE
024100000000     C*     HA IL NUMERO DI SERIE --> PROCEDURA B --> STAMPA BOLLE + SEGNACOLLI
024200000000     C*
024300000000     C                   Z-ADD     DCLKUT        TBLKUT
024400000000     C                   MOVEL     '3C'          TBLCOD
024500000000     C                   MOVEL     *BLANKS       TBLKEY
024600000000     C                   MOVEL     $KSC          TBLKEY
024700000000     C     KEYTAB        CHAIN     TABEL00F                           99
024800000000     C*
024900000000IF  1C     *IN99         IFEQ      *ON
025000000000     C                   MOVEL     *BLANKS       $SOL                           *PROCED NON AVVIATA
025100000000X   1C                   ELSE
025200000000     C                   MOVEL     TBLUNI        DS3C
025300000000     C*
025400000000IF  2C     §3COKD        IFNE      '*'
025500000000     C                   MOVEL     *BLANKS       $SOL                           *PROCED NON AVVIATA
025600000000X   2C                   ELSE
025700000000     C*
025800000000IF  3C     §3CNRS        IFEQ      *ZEROS
025900000000     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
026000000000X   3C                   ELSE
026100000000     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
026200000000E   3C                   ENDIF
026300000000     C*
026400000000     C                   MOVEL     §3CCTM        $CTM                           *TRATTAMENTO MERCE
026500000000E   2C                   ENDIF
026600000000E   1C                   ENDIF
026700000000     C*
026800000000     C                   ENDSR
026900000307     C*------------------------------------------------------------------------*
027000000307     C* FINSR - OPERAZIONI FINALI
027100000307     C*------------------------------------------------------------------------*
027200000307     C     FINSR         BEGSR
027300000307     C*
027400000307     C* CHIUDE I FILE DEI PGM CHIAMATI
027500000307     C                   CLEAR                   BS69DS
027600000307     C                   CLEAR                   ACODS
027700000307     C                   CLEAR                   INDDS
027800000307     C                   CLEAR                   CLPDS
027900000307     C                   CLEAR                   CLSDS
028000000307     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
028100000307     C                   CALL      'TIBS69R'
028200000307     C                   PARM                    BS69DS
028300000307     C                   PARM                    ACODS
028400000307     C                   PARM                    INDDS
028500000307     C                   PARM                    CLPDS
028600000307     C                   PARM                    CLSDS
028700000307     C*
028800000307     C                   ENDSR
028900000000     C*------------------------------------------------------------------------*
029000000000     C*?*INZSR - ROUTINE INIZIALE
029100000000     C*------------------------------------------------------------------------*
029200000000     C     *INZSR        BEGSR
029300000000     C*
029400000000     C* RICEVIMENTO PARAMETRI
029500000000     C     *ENTRY        PLIST
029600000000     C                   PARM                    DSCL
029700000000     C*
029800000000     C* DEFINIZIONI VARIABILI
029900000000     C     *LIKE         DEFINE    ACOKUT        KACKUT
030000000000     C     *LIKE         DEFINE    ACOKCC        KACKCC
030100000000     C     *LIKE         DEFINE    ACOKSC        KACKSC
030200000000     C*
030300000000     C* CHIAVE LETTURA CNACO00F/CNIND00F/CNCLP00F - COMPETA
030400000307     C*---       KEYCLI    KLIST
030500000307     C*---                 KFLD           KACKUT           *CODICE UTENTE
030600000307     C*---                 KFLD           KACKCC           *CODICE CAPOCONTO
030700000307     C*---                 KFLD           KACKSC           *CODICE SOTTOCONTO
030800000000     C*
030900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
031000000000     C     KEYTAB        KLIST
031100000000     C                   KFLD                    TBLKUT
031200000000     C                   KFLD                    TBLCOD
031300000000     C                   KFLD                    TBLKEY
031400000000     C*
031500000000     C                   ENDSR
