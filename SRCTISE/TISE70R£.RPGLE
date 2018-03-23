000100040220      *------------------------------------------------------------------------*
000200041220      * REPERIMENTO SALDI CLIENTE                                              *
000300040220      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500040220
000600041221     FSISDC01L  IF   E           K DISK
000700041222     FTABEL00F  IF   E           K DISK
000800040220
000900041220     D*-------------------
001000041220     D* DS DI PROCEDURA
001100041220     D*-------------------
001200041220     D TISE70DS      E DS
001300041222     D*-------------------
001400041222     D* DS FLAG RICAVI/SPEDIZIONI RAGGRUPPAMENTI TIPI BOLLA
001500041222     D*-------------------
001600041222     D DS2W          E DS
001700041222     D*-------------------
001800041222     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA
001900041222     D*-------------------
002000041222     D FRBL            S              1    DIM(10)
002100041222     D FSP             S              1    DIM(10)
002200041222     D FRI             S              1    DIM(10)
002300041220     D*-------------------
002400041220     D* DS REPERIMENTI UNIFICANTI
002500040220     D*------------------
002600040622     D TIBS10DS      E DS
002700041221     D  skCLI                 21   5520  0 DIM(500) INZ(*zeros)                 *SCHIERA CLIENTI PADRI/FIGLI
002800041220     D*------------------
002900041220     D* DS SUDDIVISIONE DATA INIZIALE
003000041220     D*------------------
003100041220     D                 DS
003200041220     D  ANNINI                 1      4  0
003300041220     D  MESINI                 5      6  0
003400041220     D  DATINI                 1      6  0
003500040220
003600040622
003700041220     C*------------------
003800041220     C* MAIN
003900041220     C*------------------
004000041222     C*
004100041222     C* CARICO I DATI TABELLATI
004200041222     C                   EXSR      CARTAB
004300041220     C*
004400041220     C* INNANZITUTTO VERIFICO SE RICHIESTA ELABORAZIONE CLIENTE X UNIFICANTE
004500041220     C                   IF        I70TLA = 'U'
004600041220     C                   EXSR      RTVPARENT
004700041220     C                   ELSE
004800041220     C                   Z-ADD     I70KSC        skCLI(1)
004900041220     C                   ENDIF
005000041220     C*
005100041220     C* A QUESTO PUNTO CALCOLO I SALDI DEL/DEI CLIENTI COSI REPERITI
005200041220     C                   Z-ADD     I70DAI        DATINI                         *DS DATA INIZIALE
005300041220     C*
005400041220     C     1             DO        500           jCLI              3 0
005500041220     C*
005600041220     C                   IF        skCLI(jCLI) = *zeros
005700041220     C                   LEAVE
005800041220     C                   ELSE
005900041221     C                   Z-ADD     skCLI(jCLI)   SDCKSC
006000041222     C                   Z-ADD     ANNINI        SDCANN                         *ANNO INIZIALE
006100041222     C                   Z-ADD     MESINI        SDCMES                         *MESE INIZIALE
006200041220     C*
006300041220     C* POSIZIONAMENTO INIZIALE X KSC/ANN/MES
006400041221     C     KEYSDC        SETLL     SISDC01L
006500041220     C*
006600041220     C* CICLO X KSC X TOTALIZZARE PERIODO RICHIESTO NEL LANCIO
006700041220     C                   DO        *HIVAL
006800041221     C     KEYSDC_KSC    READE     SISDC01L
006900041221     C                   IF        %eof(SISDC01L)
007000041221     C                             OR  SDCANN*100+SDCMES > I70DAF
007100041220     C                   LEAVE
007200041220     C                   ELSE
007300041221     C*
007400041222     C* AI FINI DEL CONTEGGIO DI: SPEDIZIONI, COLLI, PESO, VOLUME E RICAVI SEGUO QUANTO INDICATO NELLA TABELLA '2W'
007500041222     C                   SETOFF                                           28
007600041222     C                   Z-ADD     1             I
007700041222     C     SDCRBL        LOOKUP    FRBL(I)                                28
007800041222     C     *IN28         IFEQ      *ON
007900041222     C*
008000041222     C* SOMMATORIA VALORI RELATIVI ALLA SPEDIZIONE (SPED. COLLI PESO VOLUME) => SOLO RAGGR. VALIDI COME SPEDIZIONE
008100041222     C                   IF        FSP(I) = 'S'
008200041222     C                   EVAL      O70NSP = O70NSP + SDCNSP
008300041222     C                   EVAL      O70NCO = O70NCO + SDCNCO
008400041222     C                   EVAL      O70PKG = O70PKG + SDCPKG
008500041222     C                   EVAL      O70VOL = O70VOL + SDCVOL
008600041222     C                   ENDIF
008700041222     C*
008800041222     C* SOMMATORIA VALORI RELATIVI AI RICAVI => SOLO RAGGR. VALIDI COME RICAVO
008900041222     C* ...RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
009000041222     C                   IF        FRI(I) = 'S'
009100041222     C                   EVAL      O70RIC = O70RIC + SDCIRB + SDCIRR + SDCINC
009200041222     C                   ENDIF
009300041222     C                   ENDIF
009400041222     C*
009500041222     C* PER LA "DELTA" INVECE VANNO BENE TUTTI I TIPI RAGGRUPPAMENTO
009600041222     C* ...RICAVO REALE DELTA = RICAVO REALE DELTA + RETTIFICHE + NOTE DI CREDITO
009700041222     C                   EVAL      O70RIR = O70RIR + SDCIRD + SDCIRR + SDCINC
009800041222     C* ...RICAVO PRESUNTO DELTA
009900041222     C                   EVAL      O70RIP = O70RIP + SDCIRP
010000041222     C*
010100041220     C                   ENDIF
010200041220     C                   ENDDO
010300000000     C*
010400041220     C                   ENDIF
010500040622     C*
010600041220     C                   ENDDO
010700040622     C*
010800041220     C                   SETON                                        LR
010900041220     C*------------------------------------------------------------------------*
011000041220     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
011100041220     C*------------------------------------------------------------------------*
011200041220     C     RTVPARENT     BEGSR
011300041220     C*
011400041220     C* 1° STEP: considero il codice corrente come figlio e cerco il padre e i fratelli
011500041220     C                   CLEAR                   TIBS10DS
011600041220     C                   EVAL      D10TLE = I70TLE
011700041220     C                   EVAL      D10PAF = 'P'
011800041220     C                   EVAL      D10COD = I70KSC
011900041220     C                   CALL      'TIBS10R'
012000041220     C                   PARM                    TIBS10DS
012100041220     C                   PARM      'E'           TIPEXE            1
012200041220     C*
012300041220     C* 2° STEP: se non è figlio allora lo considero padre e cerco i suoi figli
012400041220     C                   IF        D10ERR = '1'
012500041220     C                   CLEAR                   TIBS10DS
012600041220     C                   EVAL      D10TLE = I70TLE
012700041220     C                   EVAL      D10PAF = 'F'
012800041220     C                   EVAL      D10COD = I70KSC
012900041220     C                   CALL      'TIBS10R'
013000041220     C                   PARM                    TIBS10DS
013100041220     C                   PARM      'E'           TIPEXE
013200041220     C*
013300041220     C* A questo punto traggo le conclusioni e valorizzo la schiera finale
013400041220     C                   IF        D10ERR = '1'
013500041220     C                   MOVE(P)   D10COD        skCLI(1)
013600041220     C                   ENDIF
013700041220     C                   ENDIF
013800041220     C*
013900041220     C                   ENDSR
014000041222     C*------------------------------------------------------------------------*
014100041222     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
014200041222     C*------------------------------------------------------------------------*
014300041222     C     CARTAB        BEGSR
014400041222     C*
014500041222     C* FLAG RICAVI/SPEDIZIONI RAGGRUPPAMENTO TIPI BOLLA
014600041222     C                   Z-ADD     *ZEROS        I                 4 0
014700041222     C                   Z-ADD     1             TBLKUT
014800041222     C                   MOVEL     '2W'          TBLCOD
014900041222     C     KEYTAB        SETLL     TABEL00F
015000041222     C     KEYTAB        READE     TABEL00F                               99
015100041222     C     *IN99         DOWEQ     *OFF
015200041222     C     TBLFLG        IFEQ      *BLANKS
015300041222     C                   ADD       1             I
015400041222     C                   MOVEL     TBLUNI        DS2W
015500041222     C                   MOVEL     TBLKEY        FRBL(I)                        *RAGGRUPP.TIPO BOLLA
015600041222     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
015700041222     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
015800041222     C                   ENDIF
015900041222     C     KEYTAB        READE     TABEL00F                               99
016000041222     C                   ENDDO
016100041222     C*
016200041222     C                   ENDSR
016300041220     C*------------------------------------------------------------------------*
016400041220     C*?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
016500041220     C*------------------------------------------------------------------------*
016600040219     C     *INZSR        BEGSR
016700041220     C*
016800041220     C* RICEVIMENTO PARAMETRI
016900040219     C     *ENTRY        PLIST
017000041220     C                   PARM                    TISE70DS
017100041220     C*
017200041220     C* PULISCO SUBITO I CAMPI D OUTPUT DELLA DS D PROCEDURA
017300041220     C                   CLEAR                   O70NSP
017400041220     C                   CLEAR                   O70NCO
017500041220     C                   CLEAR                   O70PKG
017600041220     C                   CLEAR                   O70VOL
017700041220     C                   CLEAR                   O70RIC
017800041222     C                   CLEAR                   O70RIR
017900041222     C                   CLEAR                   O70RIP
018000041220     C*
018100041221     C* CHIAVE LETTURA SISDC01L - COMPLETA
018200041221     C     KEYSDC        KLIST
018300041221     C                   KFLD                    SDCKSC                         *CLIENTE
018400041221     C                   KFLD                    SDCANN                         *ANNO
018500041221     C                   KFLD                    SDCMES                         *MESE
018600041220     C*
018700041221     C* CHIAVE LETTURA SISDC01L - PARZIALE (SOLO CLIENTE)
018800041221     C     KEYSDC_KSC    KLIST
018900041221     C                   KFLD                    SDCKSC                         *CLIENTE
019000041222     C*
019100041222     C* CHAIVE LETTURA TABEL00F - PARZIALE
019200041222     C     KEYTAB        KLIST
019300041222     C                   KFLD                    TBLKUT                          *UTENTE
019400041222     C                   KFLD                    TBLCOD                          *TABELLA
019500041220     C*
019600040219     C                   ENDSR
