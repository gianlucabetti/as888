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
001200170221     D trec17ds      E DS
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
002700041221     D  skCLI                 21   5520  0 DIM(500) INZ(*zeros)
002800041220     D*------------------
002900041220     D* DS SUDDIVISIONE DATA INIZIALE
003000041220     D*------------------
003100041220     D                 DS
003200041220     D  ANNINI                 1      4  0
003300041220     D  MESINI                 5      6  0
003400041220     D  DATINI                 1      6  0
003700070516
003800041220     C*------------------
003900041220     C* MAIN
004000041220     C*------------------
004600041222     C*
004700041222     C* CARICO I DATI TABELLATI
004800041222     C                   EXSR      CARTAB
004900041220     C*
005200041220     C                   EXSR      RTVPARENT
005600041220     C*
005700041220     C* A QUESTO PUNTO CALCOLO I SALDI DEL/DEI CLIENTI COSI REPERITI
005800170221     C                   Z-ADD     I17DAI        DATINI
005900041220     C*
006000041220     C     1             DO        500           jCLI              3 0
006100041220     C*
006200041220     C                   IF        skCLI(jCLI) = *zeros
006300041220     C                   LEAVE
006400041220     C                   ELSE
006500041221     C                   Z-ADD     skCLI(jCLI)   SDCKSC
006600041222     C                   Z-ADD     ANNINI        SDCANN
006700041222     C                   Z-ADD     MESINI        SDCMES
006800041220     C*
006900041220     C* POSIZIONAMENTO INIZIALE X KSC/ANN/MES
007000041221     C     KEYSDC        SETLL     SISDC01L
007100041220     C*
007200041220     C* CICLO X KSC X TOTALIZZARE PERIODO RICHIESTO NEL LANCIO
007300041220     C                   DO        *HIVAL
007400041221     C     KEYSDC_KSC    READE     SISDC01L
007500041221     C                   IF        %eof(SISDC01L)
007600170221     C                             OR  SDCANN*100+SDCMES > I17DAF
007700041220     C                   LEAVE
007800041220     C                   ELSE
008201170222      * a rottura di fascia di peso o di tipo porto o periodo
008202170221     c                   if        (sdcfpe <> sdcfpes and sdcfpes <> ' ') or
008203170222     c                             (sdctpo <> sdctpos and sdctpos <> ' ') or
008204170222     c                             (sdcmes <> sdcmess and sdcmess <> ' ') or
008205170222     c                             (sdcann <> sdcanns and sdcanns <> ' ')
008206170221     c                   exsr      srwtr
008207170221     C                   clear                   O70NSP
008208170221     C                   clear                   O70NCO
008209170221     C                   clear                   O70PKG
008210170221     C                   clear                   O70VOL
008214170221     C                   clear                   O70irb                     C
008215170221     C                   clear                   O70irr                     C
008216170221     C                   clear                   O70inc                     C
008217170221     C                   clear                   sdcfpes                    C
008218170221     C                   clear                   sdctpos                    C
008219170222     C                   clear                   sdcanns                    C
008220170222     C                   clear                   sdcmess                    C
008221170221     c                   endif
008300041221     C*
008400041222     C* AI FINI DEL CONTEGGIO DI: SPEDIZIONI, COLLI, PESO, VOLUME E RICAVI SEGUO
008500041222     C                   SETOFF                                           28
008600041222     C                   Z-ADD     1             I
008700041222     C     SDCRBL        LOOKUP    FRBL(I)                                28
008800041222     C     *IN28         IFEQ      *ON
008900041222     C*
009000041222     C* SOMMATORIA VALORI RELATIVI ALLA SPEDIZIONE (SPED. COLLI PESO VOLUME) =>
009100041222     C                   IF        FSP(I) = 'S'
009200041222     C                   EVAL      O70NSP = O70NSP + SDCNSP
009300041222     C                   EVAL      O70NCO = O70NCO + SDCNCO
009400041222     C                   EVAL      O70PKG = O70PKG + SDCPKG
009500041222     C                   EVAL      O70VOL = O70VOL + SDCVOL
009600041222     C                   ENDIF
009601170221     C* SOMMATORIA IMPORTI RELATIVI AI RICAVI
009602170221     C                   IF        FRI(I) = 'S'
009603170221     C                   EVAL      O70irb = O70irb + SDCirb
009604170221     C                   EVAL      O70irb = O70irr + SDCirr
009605170221     C                   EVAL      O70inc = O70inc + SDCinc
009606170221     C                   ENDIF
009607170221     C                   ENDIF
011000041222     C*
011100041220     C                   ENDIF
011200041220     C                   ENDDO
011203170222      * a rottura di fascia di peso o di tipo porto scrivo
011204170222     c                   if        (sdcfpe <> sdcfpes and sdcfpes <> ' ') or
011205170222     c                             (sdctpo <> sdctpos and sdctpos <> ' ') or
011206170222     c                             (sdcmes <> sdcmess and sdcmess <> ' ') or
011207170222     c                             (sdcann <> sdcanns and sdcanns <> ' ')
011208170222     c                   exsr      srwtr
011300000000     C*
011400041220     C                   ENDIF
011500040622     C*
011600041220     C                   ENDDO
011700040622     C*
011800041220     C                   SETON                                        LR
011900041220     C*------------------------------------------------------------------------*
012000170221     C* scrive il record per clinte
012100041220     C*------------------------------------------------------------------------*
012200170221     C     srwtr         BEGSR
012202170221     c     kksc          chain     ecrcu01l
012203170222     C                   EVAL      rcuNSP = O70NSP + rcuNSP
012204170222     C                   EVAL      rcuNCO = O70NCO + rcuNCO
012205170222     C                   EVAL      rcuPKG = O70PKG + rcuPKG
012206170222     C                   EVAL      rcuVOL = O70VOL + rcuVOL
012207170222     C                   EVAL      rcuirb = O70irb + rcuirb
012208170222     C                   EVAL      rcuirb = O70irr + rcuirr
012209170222     C                   EVAL      rcuinc = O70inc + rcuinc
012210170221     c                   if        %found(ecrcu01l)
012211170222     c                   update    ecrcu000
012220170221     c                   else
012221170222     c                   eval      rcuksc = i17ksc
012222170222     c                   eval      rcuaas = sdcanns
012223170222     c                   eval      rcumes = sdcmess
012224170222     c                   eval      rcutpo = sdctpos
012225170222     c                   eval      rcufpe = sdcfpes
012226170222     c                   write     ecrcu000
012227170221     c                   end
012228170221     C                   endsr
012229170221     C*------------------------------------------------------------------------*
012230170221     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
012231170221     C*------------------------------------------------------------------------*
012232170221     C     RTVPARENT     BEGSR
012300041220     C*
012400041220     C* 1° STEP: considero il codice corrente come figlio e cerco il padre e i f
012500041220     C                   CLEAR                   TIBS10DS
012600170221     C                   EVAL      D10TLE = 'ST'
012700041220     C                   EVAL      D10PAF = 'P'
012800170221     C                   EVAL      D10COD = I17KSC
012900041220     C                   CALL      'TIBS10R'
013000041220     C                   PARM                    TIBS10DS
013100041220     C                   PARM      'E'           TIPEXE            1
013200041220     C*
013300041220     C* 2° STEP: se non è figlio allora lo considero padre e cerco i suoi figli
013400041220     C                   IF        D10ERR = '1'
013500041220     C                   CLEAR                   TIBS10DS
013600170221     C                   EVAL      D10TLE = 'ST'
013700041220     C                   EVAL      D10PAF = 'F'
013800170221     C                   EVAL      D10COD = I17KSC
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
016500041222     C                   MOVEL     TBLKEY        FRBL(I)
016600041222     C                   MOVEL     §2WFSP        FSP(I)
016700041222     C                   MOVEL     §2WFRI        FRI(I)
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
018000170221     C                   PARM                    Trec17DS
018100041220     C*
018200041220     C* PULISCO SUBITO I CAMPI D OUTPUT DELLA DS D PROCEDURA
018300041220     C                   CLEAR                   O70NSP
018400041220     C                   CLEAR                   O70NCO
018500041220     C                   CLEAR                   O70PKG
018600041220     C                   CLEAR                   O70VOL
018700170221     C                   CLEAR                   O70irr
018800170221     C                   CLEAR                   O70irb
018900170221     C                   CLEAR                   O70inc
019000041220     C*
019100041221     C* CHIAVE LETTURA SISDC01L - COMPLETA
019200041221     C     KEYSDC        KLIST
019300041221     C                   KFLD                    SDCKSC
019400041221     C                   KFLD                    SDCANN
019500041221     C                   KFLD                    SDCMES
019600041220     C*
019700041221     C* CHIAVE LETTURA SISDC01L - PARZIALE (SOLO CLIENTE)
019800041221     C     KEYSDC_KSC    KLIST
019900041221     C                   KFLD                    SDCKSC
020000041222     C*
020100041222     C* CHAIVE LETTURA TABEL00F - PARZIALE
020200041222     C     KEYTAB        KLIST
020300041222     C                   KFLD                    TBLKUT
020400041222     C                   KFLD                    TBLCOD
020500041220     C*
020600040219     C                   ENDSR
