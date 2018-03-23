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
005400000000     D*------------------
005500000000     D* DS AGENTI COMMERCIALI
005600000000     D*------------------
005700000000     D DS01          E DS
005800000307     D*------------------
005900000307     D* DS REPERIMENTO DATI UTENTE
006000000307     D*-------------------
006100000307     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006200000307     D ACODS         E DS                  EXTNAME(CNACO00F)
006300000307     D INDDS         E DS                  EXTNAME(CNIND00F)
006400000307     D CLPDS         E DS                  EXTNAME(CNCLP00F)
006500000307     D CLSDS         E DS                  EXTNAME(FNCLS00F)
006600000000     C*------------------------------------------------------------------------*
006700000000     C*?MAIN - REPERIMENTO DATI DEL CLIENTE
006800000000     C*------------------------------------------------------------------------*
006900000000     C*
007000000307     C*---                 Z-ADDDCLKUT    KACKUT           *UTENTE
007100000307     C*---                 Z-ADDDCLKCC    KACKCC           *CAPO CONTO
007200000307     C*---                 MOVELDCLKSC    KACKSC           *SOTTOCONTO
007300000307     C*---
007400000307     C*---       KEYCLI    CHAINCNACO00F             99
007500000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
007600000307     C*---                 CLEARCNACO000                   *IMPOSTO CAMPI VUOTI
007700000307E   1C*---                 ENDIF
007800000307     C*---
007900000307     C*---       KEYCLI    CHAINCNIND00F             99
008000000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
008100000307     C*---                 CLEARCNIND000                   *IMPOSTO CAMPI VUOTI
008200000307E   1C*---                 ENDIF
008300000307     C*---
008400000307     C*---       KEYCLI    CHAINCNCLP00F             99
008500000307IF  1C*---       *IN99     IFEQ *ON                        *NON TROVATO
008600000307     C*---                 CLEARCNCLP000                   *IMPOSTO CAMPI VUOTI
008700000307E   1C*---                 ENDIF
008800000307     C*
008900000307     C                   CLEAR                   BS69DS
009000000307     C                   CLEAR                   ACODS
009100000307     C                   CLEAR                   CLPDS
009200000307     C                   CLEAR                   INDDS
009300000307     C                   Z-ADD     DCLKCC        I69KCC                         *CAPOCONTO
009400000307     C                   MOVEL     DCLKSC        I69KAC                         *CODICE X CNACO
009500000307     C                   MOVEL     DCLKSC        I69KIN                         *CODICE X CNNDO
009600000307     C                   MOVEL     DCLKSC        I69KCP                         *CODICE X CNCLP
009700000307     C                   CALL      'TIBS69R'
009800000307     C                   PARM                    BS69DS
009900000307     C                   PARM                    ACODS
010000000307     C                   PARM                    INDDS
010100000307     C                   PARM                    CLPDS
010200000307     C                   PARM                    CLSDS
010300000000     C*
010400000000     C* RIEMPO DA DS DI RITORNO
010500000000     C                   MOVEL     ACORAG        DCLRAG
010600951006     C                   Z-ADD     ACOITC        DCLITC
010700000000     C                   MOVEL     INDVIA        DCLVIA
010800000000     C                   MOVEL     INDCIT        DCLCIT
010900000000     C                   MOVEL     INDCAE        DCLCAP
011000000000     C                   MOVEL     INDPRV        DCLPRV
011100000000     C                   MOVEL     INDSTA        DCLSTA
011200000000     C                   MOVEL     INDTEL        DCLTEL
011300000000     C                   MOVEL     CLPCLV        DCLCLV
011400000000IF  1C     DCLCLV        IFEQ      *BLANKS                                      *SE QUALITA' VUOTA
011500000000     C                   MOVEL     'C'           DCLCLV                         *E' CLIENTE DI QL 'C
011600000000E   1C                   ENDIF
011700000000     C*
011800000000     C                   Z-ADD     CLPDPS        $DATA
011900000000     C                   EXSR      ROVDAT                                       *ROVESCIA DATA
012000000000     C                   Z-ADD     $DATA         DCLDPS
012100000000     C*
012200000000     C                   Z-ADD     CLPAGE        DCLAGE
012300000000     C                   MOVE      INDCDP        DCLCDP
012400000000     C*
012500000000     C                   MOVEL     DCLKSC        $KSC              7
012600000000     C                   MOVEL     *BLANKS       $SOL              1
012700000000     C                   MOVEL     *BLANKS       $CTM              2
012800000000     C                   EXSR      REPSOL                                       *REPERISCE SOLUZIONE
012900000000     C                   MOVEL     $SOL          DCLSOL
013000000000     C**
013100000000     C* D E C O D I F I C H E
013200000000     C***
013300000000     C*--------------------
013400000000     C* PROVINCIA
013500000000     C*--------------------
013600000000IF  1C     DCLPRV        IFNE      *BLANKS
013700000000     C                   Z-ADD     DCLKUT        TBLKUT
013800000000     C                   MOVEL     'PR'          TBLCOD
013900000000     C                   MOVEL     *BLANKS       TBLKEY
014000000000     C                   MOVEL     DCLPRV        TBLKEY
014100000000     C     KEYTAB        CHAIN     TABEL00F                           99
014200000000IF  2C     *IN99         IFEQ      *OFF
014300000000     C                   MOVEL     TBLUNI        DSPR
014400000000     C                   MOVEL     §PRDES        DCLDPR
014500000000X   2C                   ELSE
014600000000     C                   MOVEL     *ALL'*'       DCLDPR
014700000000E   2C                   ENDIF
014800000000E   1C                   ENDIF
014900000000     C*--------------------
015000000000     C* SETTORE MERCEOLOGICO
015100000000     C*--------------------
015200951006IF  1C     DCLITC        IFNE      *ZEROS
015300000000     C                   Z-ADD     DCLKUT        TBLKUT
015400951006     C                   MOVEL     '1L'          TBLCOD
015500951006     C                   MOVE      DCLITC        NUM5              5 0
015600951006     C                   MOVEL     *BLANKS       TBLKEY
015700951006     C                   MOVEL     NUM5          TBLKEY
015800000000     C     KEYTAB        CHAIN     TABEL00F                           99
015900000000IF  2C     *IN99         IFEQ      *OFF
016000951006     C                   MOVEL     TBLUNI        DS1L
016100951006     C                   MOVEL     §1LDES        DCLDIT
016200000000X   2C                   ELSE
016300951006     C                   MOVEL     *ALL'*'       DCLDIT
016400000000E   2C                   ENDIF
016500000000E   1C                   ENDIF
016600000000     C*--------------------
016700000000     C* AGENTE COMMERCIALE
016800000000     C*--------------------
016900000000IF  1C     DCLAGE        IFGT      *ZEROS
017000140616     C* Reperisco le informazioni dell'agente commerciale
017100140616     C                   EVAL      CMMCod = dclage
017200140616     C     CMMCod        CHAIN     AZCMM01l
017300140616     C                   IF        %found(AZCMM01L)
017400140616     C                   MOVEL     cmmdes        DCLDAG
017500000000X   2C                   ELSE
017600000000     C                   MOVEL     *ALL'*'       DCLDAG
017700000000E   2C                   ENDIF
017800000000E   1C                   ENDIF
017900000000     C*--------------------
018000000000     C* CONDIZIONI DI PAGAMENTO
018100000000     C*--------------------
018200000000IF  1C     DCLCDP        IFNE      *BLANKS
018300000000     C                   Z-ADD     DCLKUT        TBLKUT
018400000000     C                   MOVEL     'FA'          TBLCOD
018500000000     C                   MOVEL     *BLANKS       TBLKEY
018600000000     C                   MOVEL     DCLCDP        WCDP                           *CONDIZIONE PAGAMENT
018700000323     C                   MOVEL     '1'           WLIN                           *LINGUA
018800000000     C                   MOVEL     DSCDP         TBLKEY                         *COND + LINGUA + ' '
018900000000     C     KEYTAB        CHAIN     TABEL00F                           99
019000000000IF  2C     *IN99         IFEQ      *OFF
019100000000     C                   MOVEL     TBLUNI        DSFA
019200000000     C                   MOVEL     §FADES        DCLDCD
019300000000X   2C                   ELSE
019400000000     C                   MOVEL     *ALL'*'       DCLDCD
019500000000E   2C                   ENDIF
019600000000E   1C                   ENDIF
019700000000     C*--------------------
019800000000     C* PASSAGGIO DATI
019900000000     C*--------------------
020000000000IF  1C     DCLSOL        IFNE      *BLANKS
020100000000     C                   Z-ADD     DCLKUT        TBLKUT
020200000000     C                   MOVEL     '1B'          TBLCOD
020300000000     C                   MOVEL     *BLANKS       TBLKEY
020400000000     C                   MOVEL     $CTM          TBLKEY                         *TRATTAMENTO MERCE
020500000000     C     KEYTAB        CHAIN     TABEL00F                           99
020600000000IF  2C     *IN99         IFEQ      *OFF
020700000000     C                   MOVEL     TBLUNI        DS1B
020800000000     C                   MOVEL     §1BDES        DCLDSO
020900000000X   2C                   ELSE
021000000000     C                   MOVEL     *ALL'*'       DCLDSO
021100000000E   2C                   ENDIF
021200000000X   1C                   ELSE
021300000000     C                   MOVEL     'NESSUNO'     DCLDSO
021400000000E   1C                   ENDIF
021500000000     C*
021600000307     C* OPERAZIONI FINALI
021700000307     C                   EXSR      FINSR
021800000307     C*
021900000000     C                   SETON                                        LR
022000000000     C*------------------------------------------------------------------------*
022100000000     C*?ROVDAT - ROVESCIA LE DATE
022200000000     C*          INPUT  --> $DATA: DATA
022300000000     C*          OUTPUT --> $DATA :DATA ROVESCIATA
022400000000     C*------------------------------------------------------------------------*
022500000000     C     ROVDAT        BEGSR
022600000000     C*
022700000000     C                   MOVEL     $DATA         DD1               2 0
022800000000     C                   MOVE      $DATA         DD2               2 0
022900000000     C                   MOVE      DD1           $DATA             6 0
023000000000     C                   MOVEL     DD2           $DATA
023100000000     C*
023200000000     C                   ENDSR
023300000000     C*------------------------------------------------------------------------*
023400000000     C*?REPSOL - REPERISCE LA SOLUZIONE DI PASSAGGIO DATI ATTUATA DAL CLIENTE
023500000000     C*          INPUT  --> $KSC : CODICE CLIENTE
023600000000     C*                     $SOL : CODICE SOLUZIONE VUOTO
023700000000     C*                     $CTM : CODICE TRATTAMENTO MERCE VUOTO
023800000000     C*          OUTPUT --> $SOL : CODICE SOLUZIONE (A/B)
023900000000     C*                     $CTM : CODICE TRATTAMENTO MERCE DEL CLIENTE
024000000000     C*------------------------------------------------------------------------*
024100000000     C     REPSOL        BEGSR
024200000000     C*
024300000000     C* CLIENTE PRESENTE IN TABELLA 3C --> PARTITO CON LA PROCEDURA SCAMBIO DATI
024400000000     C* NON HA IL NUMERO DI SERIE --> PROCEDURA A --> STAMPA LE BOLLE
024500000000     C*     HA IL NUMERO DI SERIE --> PROCEDURA B --> STAMPA BOLLE + SEGNACOLLI
024600000000     C*
024700000000     C                   Z-ADD     DCLKUT        TBLKUT
024800000000     C                   MOVEL     '3C'          TBLCOD
024900000000     C                   MOVEL     *BLANKS       TBLKEY
025000000000     C                   MOVEL     $KSC          TBLKEY
025100000000     C     KEYTAB        CHAIN     TABEL00F                           99
025200000000     C*
025300000000IF  1C     *IN99         IFEQ      *ON
025400000000     C                   MOVEL     *BLANKS       $SOL                           *PROCED NON AVVIATA
025500000000X   1C                   ELSE
025600000000     C                   MOVEL     TBLUNI        DS3C
025700000000     C*
025800000000IF  2C     §3COKD        IFNE      '*'
025900000000     C                   MOVEL     *BLANKS       $SOL                           *PROCED NON AVVIATA
026000000000X   2C                   ELSE
026100000000     C*
026200000000IF  3C     §3CNRS        IFEQ      *ZEROS
026300000000     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
026400000000X   3C                   ELSE
026500000000     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
026600000000E   3C                   ENDIF
026700000000     C*
026800000000     C                   MOVEL     §3CCTM        $CTM                           *TRATTAMENTO MERCE
026900000000E   2C                   ENDIF
027000000000E   1C                   ENDIF
027100000000     C*
027200000000     C                   ENDSR
027300000307     C*------------------------------------------------------------------------*
027400000307     C* FINSR - OPERAZIONI FINALI
027500000307     C*------------------------------------------------------------------------*
027600000307     C     FINSR         BEGSR
027700000307     C*
027800000307     C* CHIUDE I FILE DEI PGM CHIAMATI
027900000307     C                   CLEAR                   BS69DS
028000000307     C                   CLEAR                   ACODS
028100000307     C                   CLEAR                   INDDS
028200000307     C                   CLEAR                   CLPDS
028300000307     C                   CLEAR                   CLSDS
028400000307     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
028500000307     C                   CALL      'TIBS69R'
028600000307     C                   PARM                    BS69DS
028700000307     C                   PARM                    ACODS
028800000307     C                   PARM                    INDDS
028900000307     C                   PARM                    CLPDS
029000000307     C                   PARM                    CLSDS
029100000307     C*
029200000307     C                   ENDSR
029300000000     C*------------------------------------------------------------------------*
029400000000     C*?*INZSR - ROUTINE INIZIALE
029500000000     C*------------------------------------------------------------------------*
029600000000     C     *INZSR        BEGSR
029700000000     C*
029800000000     C* RICEVIMENTO PARAMETRI
029900000000     C     *ENTRY        PLIST
030000000000     C                   PARM                    DSCL
030100000000     C*
030200000000     C* DEFINIZIONI VARIABILI
030300000000     C     *LIKE         DEFINE    ACOKUT        KACKUT
030400000000     C     *LIKE         DEFINE    ACOKCC        KACKCC
030500000000     C     *LIKE         DEFINE    ACOKSC        KACKSC
030600000000     C*
030700000000     C* CHIAVE LETTURA CNACO00F/CNIND00F/CNCLP00F - COMPETA
030800000307     C*---       KEYCLI    KLIST
030900000307     C*---                 KFLD           KACKUT           *CODICE UTENTE
031000000307     C*---                 KFLD           KACKCC           *CODICE CAPOCONTO
031100000307     C*---                 KFLD           KACKSC           *CODICE SOTTOCONTO
031200000000     C*
031300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
031400000000     C     KEYTAB        KLIST
031500000000     C                   KFLD                    TBLKUT
031600000000     C                   KFLD                    TBLCOD
031700000000     C                   KFLD                    TBLKEY
031800000000     C*
031900000000     C                   ENDSR
