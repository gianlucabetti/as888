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
003500070516
003600070516     d  wctr           s                   like(sdcctr) inz
003700070516
003800041220     C*------------------
003900041220     C* MAIN
004000041220     C*------------------
004100070515      *
004200070516      * verifico se passato codice tariffa
004300070516     c                   if        i70ctr <> *blanks
004400070516     c                   move      i70ctr        wctr
004500070516     c                   endif
004600041222     C*
004700041222     C* CARICO I DATI TABELLATI
004800041222     C                   EXSR      CARTAB
004900041220     C*
005000041220     C* INNANZITUTTO VERIFICO SE RICHIESTA ELABORAZIONE CLIENTE X UNIFICANTE
005100041220     C                   IF        I70TLA = 'U'
005200041220     C                   EXSR      RTVPARENT
005300041220     C                   ELSE
005400041220     C                   Z-ADD     I70KSC        skCLI(1)
005500041220     C                   ENDIF
005600041220     C*
005700041220     C* A QUESTO PUNTO CALCOLO I SALDI DEL/DEI CLIENTI COSI REPERITI
005800041220     C                   Z-ADD     I70DAI        DATINI                         *DS DATA INIZIALE
005900041220     C*
006000041220     C     1             DO        500           jCLI              3 0
006100041220     C*
006200041220     C                   IF        skCLI(jCLI) = *zeros
006300041220     C                   LEAVE
006400041220     C                   ELSE
006500041221     C                   Z-ADD     skCLI(jCLI)   SDCKSC
006600041222     C                   Z-ADD     ANNINI        SDCANN                         *ANNO INIZIALE
006700041222     C                   Z-ADD     MESINI        SDCMES                         *MESE INIZIALE
006800041220     C*
006900041220     C* POSIZIONAMENTO INIZIALE X KSC/ANN/MES
007000041221     C     KEYSDC        SETLL     SISDC01L
007100041220     C*
007200041220     C* CICLO X KSC X TOTALIZZARE PERIODO RICHIESTO NEL LANCIO
007300041220     C                   DO        *HIVAL
007400041221     C     KEYSDC_KSC    READE     SISDC01L
007500041221     C                   IF        %eof(SISDC01L)
007600041221     C                             OR  SDCANN*100+SDCMES > I70DAF
007700041220     C                   LEAVE
007800041220     C                   ELSE
007900070516      * se richiesto codice tariffa ignoro record se codice tariffa <>
008000070516     c                   if        i70ctr <> *blanks and wctr <> sdcctr
008100070516     c                   iter
008200070516     c                   endif
008300041221     C*
008400041222     C* AI FINI DEL CONTEGGIO DI: SPEDIZIONI, COLLI, PESO, VOLUME E RICAVI SEGUO QUANTO INDICATO NELLA TABELLA '2W'
008500041222     C                   SETOFF                                           28
008600041222     C                   Z-ADD     1             I
008700041222     C     SDCRBL        LOOKUP    FRBL(I)                                28
008800041222     C     *IN28         IFEQ      *ON
008900041222     C*
009000041222     C* SOMMATORIA VALORI RELATIVI ALLA SPEDIZIONE (SPED. COLLI PESO VOLUME) => SOLO RAGGR. VALIDI COME SPEDIZIONE
009100041222     C                   IF        FSP(I) = 'S'
009200041222     C                   EVAL      O70NSP = O70NSP + SDCNSP
009300041222     C                   EVAL      O70NCO = O70NCO + SDCNCO
009400041222     C                   EVAL      O70PKG = O70PKG + SDCPKG
009500041222     C                   EVAL      O70VOL = O70VOL + SDCVOL
009600041222     C                   ENDIF
009700041222     C*
009800041222     C* SOMMATORIA VALORI RELATIVI AI RICAVI => SOLO RAGGR. VALIDI COME RICAVO
009900041222     C* ...RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
010000041222     C                   IF        FRI(I) = 'S'
010100041222     C                   EVAL      O70RIC = O70RIC + SDCIRB + SDCIRR + SDCINC
010200041222     C                   ENDIF
010300041222     C                   ENDIF
010400041222     C*
010500041222     C* PER LA "DELTA" INVECE VANNO BENE TUTTI I TIPI RAGGRUPPAMENTO
010600041222     C* ...RICAVO REALE DELTA = RICAVO REALE DELTA + RETTIFICHE + NOTE DI CREDITO
010700041222     C                   EVAL      O70RIR = O70RIR + SDCIRD + SDCIRR + SDCINC
010800041222     C* ...RICAVO PRESUNTO DELTA
010900041222     C                   EVAL      O70RIP = O70RIP + SDCIRP
011000041222     C*
011100041220     C                   ENDIF
011200041220     C                   ENDDO
011300000000     C*
011400041220     C                   ENDIF
011500040622     C*
011600041220     C                   ENDDO
011700040622     C*
011800041220     C                   SETON                                        LR
011900041220     C*------------------------------------------------------------------------*
012000041220     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
012100041220     C*------------------------------------------------------------------------*
012200041220     C     RTVPARENT     BEGSR
012300041220     C*
012400041220     C* 1° STEP: considero il codice corrente come figlio e cerco il padre e i fratelli
012500041220     C                   CLEAR                   TIBS10DS
012600041220     C                   EVAL      D10TLE = I70TLE
012700041220     C                   EVAL      D10PAF = 'P'
012800041220     C                   EVAL      D10COD = I70KSC
012900041220     C                   CALL      'TIBS10R'
013000041220     C                   PARM                    TIBS10DS
013100041220     C                   PARM      'E'           TIPEXE            1
013200041220     C*
013300041220     C* 2° STEP: se non è figlio allora lo considero padre e cerco i suoi figli
013400041220     C                   IF        D10ERR = '1'
013500041220     C                   CLEAR                   TIBS10DS
013600041220     C                   EVAL      D10TLE = I70TLE
013700041220     C                   EVAL      D10PAF = 'F'
013800041220     C                   EVAL      D10COD = I70KSC
013900041220     C                   CALL      'TIBS10R'
014000041220     C                   PARM                    TIBS10DS
014100041220     C                   PARM      'E'           TIPEXE
014200041220     C*
014300041220     C* A questo punto traggo le conclusioni e valorizzo la schiera finale
014400041220     C                   IF        D10ERR = '1'
014500041220     C                   MOVE(P)   D10COD        skCLI(1)
014600041220     C                   ENDIF
014700041220     C                   ENDIF
014800041220     C*
014900041220     C                   ENDSR
015000041222     C*------------------------------------------------------------------------*
015100041222     C* CARTAB - CARICAMENTO TABELLE OCCORRENTI
015200041222     C*------------------------------------------------------------------------*
015300041222     C     CARTAB        BEGSR
015400041222     C*
015500041222     C* FLAG RICAVI/SPEDIZIONI RAGGRUPPAMENTO TIPI BOLLA
015600041222     C                   Z-ADD     *ZEROS        I                 4 0
015700041222     C                   Z-ADD     1             TBLKUT
015800041222     C                   MOVEL     '2W'          TBLCOD
015900041222     C     KEYTAB        SETLL     TABEL00F
016000041222     C     KEYTAB        READE     TABEL00F                               99
016100041222     C     *IN99         DOWEQ     *OFF
016200041222     C     TBLFLG        IFEQ      *BLANKS
016300041222     C                   ADD       1             I
016400041222     C                   MOVEL     TBLUNI        DS2W
016500041222     C                   MOVEL     TBLKEY        FRBL(I)                        *RAGGRUPP.TIPO BOLLA
016600041222     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONI
016700041222     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVI
016800041222     C                   ENDIF
016900041222     C     KEYTAB        READE     TABEL00F                               99
017000041222     C                   ENDDO
017100041222     C*
017200041222     C                   ENDSR
017300041220     C*------------------------------------------------------------------------*
017400041220     C*?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
017500041220     C*------------------------------------------------------------------------*
017600040219     C     *INZSR        BEGSR
017700041220     C*
017800041220     C* RICEVIMENTO PARAMETRI
017900040219     C     *ENTRY        PLIST
018000041220     C                   PARM                    TISE70DS
018100041220     C*
018200041220     C* PULISCO SUBITO I CAMPI D OUTPUT DELLA DS D PROCEDURA
018300041220     C                   CLEAR                   O70NSP
018400041220     C                   CLEAR                   O70NCO
018500041220     C                   CLEAR                   O70PKG
018600041220     C                   CLEAR                   O70VOL
018700041220     C                   CLEAR                   O70RIC
018800041222     C                   CLEAR                   O70RIR
018900041222     C                   CLEAR                   O70RIP
019000041220     C*
019100041221     C* CHIAVE LETTURA SISDC01L - COMPLETA
019200041221     C     KEYSDC        KLIST
019300041221     C                   KFLD                    SDCKSC                         *CLIENTE
019400041221     C                   KFLD                    SDCANN                         *ANNO
019500041221     C                   KFLD                    SDCMES                         *MESE
019600041220     C*
019700041221     C* CHIAVE LETTURA SISDC01L - PARZIALE (SOLO CLIENTE)
019800041221     C     KEYSDC_KSC    KLIST
019900041221     C                   KFLD                    SDCKSC                         *CLIENTE
020000041222     C*
020100041222     C* CHAIVE LETTURA TABEL00F - PARZIALE
020200041222     C     KEYTAB        KLIST
020300041222     C                   KFLD                    TBLKUT                          *UTENTE
020400041222     C                   KFLD                    TBLCOD                          *TABELLA
020500041220     C*
020600040219     C                   ENDSR
