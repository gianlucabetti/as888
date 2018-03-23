000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300991027
000400041015     FTNTBE01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600090323     FTISTB02L  UF   E           K DISK    commit
000700090323     FTIVAWWWT  UF A E             DISK    commit
000800031103
000900031103     D*-----------
001000031103     D* SCHIERE A PROGRAMMA
001100031103     D*-----------
001200080916     D CAUBAR          S             10    DIM(42) CTDATA PERRCD(1)
001300080916     D CAUCLI          S             11    DIM(42) CTDATA PERRCD(1)
001400041015     D*-----------
001500041015     D* VARIABILI D WRK
001600041015     D*-----------
001700031103     D prmppi          S             50
001800030924     D prmesito        S              1
001900030924     D wrkesito        S                   like(prmesito)
002000080901     D j               S              2  0
002100031103
002200031103     D*-----------
002300031103     D* VARIABILI RIFERITE AL DATA-BASE
002400031103     D*-----------
002500080618     D wKstbKSU        S                   like(stbKSU)
002600080618     D wKstbFTR        S                   like(stbFTR)
002700030924
002800030924     D*-----------
002900030924     D* DEFINISCO LA DS DELLA STRUTTURA DELL'ARCHIVIO FATTURE DEL CLIENTE
003000030924     D*-----------
003100030925     D C_DS_Cli        DS
003200080901     D  C_Filler001                  10    INZ(*zeros)
003300080901     D  C_DDT                        10    INZ(*zeros)
003400080901     D  C_DataStato                   8    INZ(*zeros)
003500080901     D  C_OraStato                    6    INZ(*zeros)
003600080901     D  C_CodStato                    1    INZ(*blanks)
003700080904     D  C_Filler002                  20    INZ(*blanks)
003800080901     D  C_IDcorriere                 10    INZ(*blanks)
003900080901     D  C_DCMprevista                 8    INZ(*zeros)
004000080901     D  C_GiacDesc                   30    INZ(*blanks)
004100080901     D  C_Filler003                  10    INZ(*blanks)
004200080901     D  C_Filler004                  20    INZ(*blanks)
004300080901     D  C_Filler005                  20    INZ(*blanks)
004400080901     D  C_Filler006                  20    INZ(*blanks)
004500080901     D  C_Logico01                    2    INZ(*blanks)
004600080901     D  C_Logico02                    2    INZ(*blanks)
004700031103
004800031103     D*-----------
004900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
005000031103     D*-----------
005100031103     D DSPPI           DS
005200080618     D  w§PPIKSU                      7  0
005300080901
005400080901     D*-----------
005500080901     D* RIDEFINIZIONE DATA BOLLA IN FORMATO ALFA
005600080901     D*-----------
005700080901     D                 DS
005800080901     D  dataAAS                1      4
005900080901     D  dataMGS                5      8
006000080901     D  dataMM                 5      6
006100080901     D  dataGG                 7      8
006200080901     D DS_DATA                 1      8
006300031103
006400031103     D*------------------
006500031103     D* DS "XSRDA8" - CONTROLLA DATA (8)
006600031103     D*------------------
006700031103     D WLBDA8          DS                  INZ
006800031103     D  G08DAT                 1      8  0
006900031103     D  G08INV                 9     16  0
007000031103     D  G08ERR                17     17
007100031103     D  G08TGI                18     22  0
007200030924
007300030325
007400030924     C* MAIN/
007500041015     C*
007600041015     C* Esegue traduzione
007700991027     C                   EXSR      traduci
007800921023     C                   SETON                                        LR
007900991027
008000030924
008100030924
008200991027     C     traduci       BEGSR
008300031103     C*
008400031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
008500031103     C* di traduzione (trattasi d "post-traduzione")
008600031104     C                   EVAL      wKstbKSU = w§PPIKSU
008700031103     C                   EVAL      wKstbFTR = *blanks
008800031103     C     KEYstb02P     SETLL     tistb02l
008900031103     C                   IF        %equal(tistb02l)
009000031103     C     KEYstb02P     READE     tistb02l
009100991027     C*
009200031103     C* Ciclo x tuttii record del cliente da trasmettere
009300031103     C                   DOW       not %eof(tistb02l)
009400031014     C*
009500030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
009600031014     C                   RESET                   C_DS_Cli
009700080618     C*
009800080618     C                   SETON                                        50
009900031103     C*
010000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
010100031103     C* deve restituire al cliente nel file tradotto
010200031103     C                   IF        stbFTR = *blanks                             * ridondante
010300031103     C*
010400031103     C* Innanzitutto chaino la bolla direttamente su TITAS
010500031103     C     KEYtas30P     CHAIN     titas30c
010600031103     C                   IF        %found(titas30c)
010700031103     C*
010800080618     C* Gestisco solamente gli stati standard
010900080618     C                   IF        stbTIS = '1'
011000080618     C*
011100080901     C                   SETON                                        50
011200080902     C*
011300080902     C* Occorre ignorare i RESI e i DIROTTAMENTI
011400080902     C                   IF        tasCCA = '1' OR
011500080902     C                             tasCCA = '2'
011600080902     C                   ELSE
011700080901     C*
011800080916     C* Gestione stati cliente 1 (consegne OK)
011900080901     C                   IF        tasDCM > *zeros AND tasCCA = *blanks
012000080901     C                   EVAL      C_CodStato = '1'
012100080901     C                   SETOFF                                       50
012200080901     C                   ELSE
012300080901     C*
012400080916     C* Gestione stati cliente 2/3
012500080901     C* - Effettuo l'abbinamento tra causali Bartolini e causali cliente
012600031103     C                   Z-ADD     1             j
012700031103     C     stbCOS        LOOKUP    CAUBAR(j)                              55
012800080618     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
012900080901     C                   EVAL      C_CodStato = %trim(CAUCLI(j))
013000080901     C                   SETOFF                                       50
013100031103     C                   ELSE
013200080901     C*
013300080916     C* Gestione stati cliente 0
013400080916     C                   EVAL      C_CodStato = '0'
013500080901     C                   SETOFF                                       50
013600080618     C                   ENDIF
013700080901     C                   ENDIF
013800080618     C*
013900080618     C* Scarico il buffer d output
014000080618     C  N50              EXSR      WRIREC
014100080901     C*
014200080901     C                   ENDIF
014300080902     C                   ENDIF
014400031103     C*
014500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
014600031103     C                   EVAL      stbFTR = 'T'
014700031103     C                   UPDATE    tistb000
014800041015     C                   ENDIF
014900080618     C                   ENDIF
015000991027     C*
015100031103     C     KEYstb02P     READE     tistb02l
015200031103     C                   SETOFF                                       50
015300030325     C                   ENDDO
015400031103     C                   ENDIF
015500991027     C*
015600030325     C                   EVAL      wrkesito = '0'
015700991027     C*
015800910830     C                   ENDSR
015900031103
016000031103
016100031103
016200080618     C     wrirec        BEGSR
016300031103     C*
016400080901     C                   MOVE(P)   stbRMN        C_DDT
016500080618     C                   MOVEL(P)  STBDAS        DS_DATA
016600080904     C                   EVAL      C_DataStato   = dataAAS+dataMM+dataGG
016700080904     C                   MOVEL     STBORS        C_OraStato
016800031103     C*
016900031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
017000031103     C                   EVAL      VAWDTA = C_DS_Cli
017100031103     C                   WRITE     TIVAW000
017200031103     C*
017300031103     C                   ENDSR
017400031103
017500031103
017600991027
017700991027      /TITLE Operazioni iniziali.
017800991027     C     *inzsr        BEGSR
017900991027     C*
018000991027     C     *ENTRY        PLIST
018100031103     C                   parm                    prmppi
018200991027     C     wrkesito      parm      wrkesito      prmesito
018300031103     C*
018400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
018500031103     C                   EVAL      DSPPI = prmppi
018600031103     C*
018700031103     C* Calcola la data corrente
018800031103     C                   time                    wn14             14 0
018900031103     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
019000031103     C                   z-add     wn8           g08dat
019100031103     C                   z-add     *zeros        g08inv
019200031103     C                   movel     '0'           g08err
019300031103     C                   call      'XSRDA8'
019400031103     C                   parm                    wlbda8
019500031103     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
019600030709     C*
019700030709     C* Definizione chiavi
019800030924     C*
019900031103     C* Chiave su TISTB02L - Parziale
020000031103     C     KEYstb02P     KLIST
020100031104     C                   KFLD                    wKstbKSU
020200031103     C                   KFLD                    wKstbFTR
020300031103     C*
020400031103     C* Chiave su TITAS30C - Parziale
020500031103     C     KEYtas30P     KLIST
020600031103     C                   KFLD                    stbAAS
020700031103     C                   KFLD                    stbLNP
020800031103     C                   KFLD                    stbNRS
020900031103     C                   KFLD                    stbNSP
021000991027     C*
021100991027     C                   ENDSR
021200031103** CAUBAR - CAUSALI BARTOLINI
021300080618001
021400080618002
021500080618003
021600080618004
021700080618005
021800080618006
021900080618007
022000080618008
022100080618009
022200080618010
022300080618012
022400080618013
022500080618014
022600080618027
022700080618030
022800080904RESO
022900080904DIR
023000080904MIC
023100080904N
023200080904NIC
023300080904T
023400080916016
023500080916017
023600080916018
023700080916019
023800080916020
023900080916021
024000080916022
024100080916023
024200080916024
024300080916026
024400080916028
024500080916029
024600080916031
024700080916032
024800080916033
024900080916034
025000080916035
025100080916036
025200080916037
025300080916100
025400080916101
025500080901** CAUCLI - CAUSALI CLIENTE
0256000809012
0257000809012
0258000809012
0259000809012
0260000809012
0261000809012
0262000809012
0263000809012
0264000809012
0265000809012
0266000809012
0267000809012
0268000809012
0269000809012
0270000809012
027100080904NON INVIARE
027200080904NON INVIARE
0273000809040
0274000809040
0275000809040
0276000809160
0277000809163
0278000809163
0279000809163
0280000809163
0281000809163
0282000809163
0283000809163
0284000809163
0285000809163
0286000809163
0287000809163
0288000809163
0289000809163
0290000809163
0291000809163
0292000809163
0293000809163
0294000809163
0295000809163
0296000809163
0297000809163
