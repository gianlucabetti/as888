000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011115     H* FICND5R *----------------------------------------------------*
000300000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500111003     FFICND00d  CF   E             WORKSTN
000600021203     FFIAPD01L  IF   E           K DISK
000700991228     FTABEL00F  IF   E           K DISK
000800950125     D* SCHIERE PER CARICAMENTO L1 E L6
000900950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001000950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
001100920810     D KPJBA         E DS
001200111003     D ficnd00ds     E DS
001300091112     D ddatiute      e ds
001400091112     D azuteds       e ds                  extname(AZUTE00F)
001500091112     D tibs34ds      E DS                  inz
001600950125     D* DS PER TRUL06R - CARICAMENTO £1
001700950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001800950125     D  LIN                    1     90  0
001900950125     D                                     DIM(30)                              SKI COMODO
002000910423     D WLBDAT          DS
002100950125     D  G02DAT                 1      8  0
002200950125     D  G02INV                 9     16  0
002300950125     D  G02ERR                17     17
002400950125     D  G02TGI                18     22  0
002500931015     D                 DS
002600931015     D  VI1PF1                 1      3  0
002700931116     D  VI1PD1                 4      7
002800931115     D  VI1P1                  1      7  0
002900931126     D                 DS
003000991228     D DS5A          E DS
003100021203     D FNLV24DS      E DS
003200900521     C*****************************************************************
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500091112     c     *dtaara       define    §azute        azuteds
003600091112     c     *dtaara       define    §datiute      ddatiute
003700091112     C                   in(E)     *dtaara
003800091112     C                   IF        %Error  or  RSUT = *blanks
003900091112     C                   call      'TIBS34R'
004000091112     C                   parm                    Tibs34Ds
004100091112     C                   in        *dtaara
004200091112     c                   ENDIF
004300031023     C                   MOVEL     knmus         Primi_tre         3
004400900511     C*---------------------------------------------------------------*
004500991228     C     KTAB          KLIST
004600091112     C                   KFLD                    tblkut
004700991228     C                   KFLD                    COD
004800991228     C                   KFLD                    KEY
004900091112     C                   Z-ADD     1             tblkut
005000021203     C     KAPDV         KLIST
005100021203     C                   KFLD                    APDTIP
005200021203     C                   KFLD                    VI1P1
005300021203     C                   MOVEL     'A'           APDTIP
005400000808     C*
005500991228     C     *LIKE         DEFINE    TBLCOD        COD
005600991228     C     *LIKE         DEFINE    TBLKEY        KEY
005700991228     C     *LIKE         DEFINE    G02TGI        DD1GIO
005800991228     C     *LIKE         DEFINE    G02TGI        DD2GIO
005900991228     C     *LIKE         DEFINE    G02TGI        WRESTO
006000931116     C*---------------------------------------------------------------*
006100910419     C*
006200950411     C* OPERAZIONI INIZIALI
006300931111     C                   EXSR      PUL01
006400930426     C*
006500091112     C                   do        *hival
006600950125     C                   EXFMT     LRD5D01
006700931116     C*
006800920323     C** CMD3 - FINE LAVORO
006900091112     C   KC              leave
007000931015     C* CONTROLLI FORMATO1
007100900528     C                   EXSR      CONTR1
007200950125     C  NKF
007300091112     COR 90              iter
007400931116     C*
007500931202     C     VI1PD1        IFEQ      *BLANKS
007600931207     C                   MOVEL     *ZEROS        VI1PD1
007700931202     C                   ENDIF
007800111003     C                   MOVE      VI1P1         PARPdr
007900091113     C                   MOVE      V1cclr        PARclr
008000111005     C                   MOVEL(P)  ficnd00ds     KPJBU
008100931202     C*
008200011115     c     knmus         ifeq      *all'1'
008300111003     C                   call      'FICND0R'
008400011115     C                   parm                    Kpjba
008500011115     c                   else
008600111003     C                   MOVEL     'FND0'        KCOAZ
008700931116     C                   CALL      'BCH10'
008800931116     C                   PARM                    KPJBA
008900011115     c                   end
009000900611     C*
009100111005     C                   EXSR      PUL01
009200091112     C                   iter
009300111005     C*
009400091112     C                   enddo
009500091112     C*
009600931027     C                   SETON                                        LR
009700931111     C*
009800931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
009900931111     C     PUL01         BEGSR
010000091113     c                   movel     'N'           V1cclr
010100931126     C* CARICO L1
010200950125     C                   CLEAR                   DSUL06
010300950125     C                   MOVE      '£1'          D06COD
010400950125     C                   MOVEL     SIMFEL        D06KEY
010500950125     C                   MOVEL     DSUL06        KPJBU
010600950125     C*
010700950125     C                   CALL      'TRUL06R'
010800950125     C                   PARM                    KPJBA
010900950125     C                   MOVEL     KPJBU         DSUL06
011000950125     C                   MOVEA     LIN           L1
011100931126     C*
011200950125     C***
011300950125     C* VEDO SE SONO SIMFEL O REMOTO
011400950125     C***
011500091112     C                   EVAL      Vi1PF1 = dutPOU
011600091112     C                   IF        dutlpo = '2' OR dutlpo = *BLANK
011700950125     C                   SETON                                        89
011800950125     C*
011900950125     C* CARICO TABELLA FILIALI GESTITE £6
012000950125     C                   CLEAR                   DSUL06
012100950125     C                   MOVE      '£6'          D06COD
012200020701     C*                  MOVEL     REMFIL        D06KEY
012300091112     C                   MOVEL     dutPOU        D06KEY
012400950125     C                   MOVEL     DSUL06        KPJBU
012500950125     C*
012600950125     C                   CALL      'TRUL06R'
012700950125     C                   PARM                    KPJBA
012800950125     C                   MOVEL     KPJBU         DSUL06
012900950125     C                   MOVEA     LIN           L6
013000950125     C*
013100950125     C                   ELSE
013200950125     C* SONO SIMFEL
013300950125     C                   MOVEL     SIMFEL        VI1PF1
013400950125     C                   ENDIF
013500991228     C                   MOVEL     '5A'          COD
013600991228     C                   MOVEL(P)  '1'           KEY
013700991228     C     KTAB          CHAIN     TABEL                              30
013800991228     C  N30              MOVEL     TBLUNI        DS5A
013900991228     C   30              CLEAR                   DS5A
014000000203     C                   TIME                    W0140            14 0
014100000203     C* UDATE IN GGMMAAAA
014200000203     C                   MOVE      W0140         WDTGIO            8 0
014300000203     C                   MOVE      WDTGIO        ANNOCO            4 0
014400000203     C     ANNOCO        ADD       1             ANNOPI            4 0
014500000203     C     ANNOCO        SUB       1             ANNOME            4 0
014600950125     C*
014700111005     C                   CLEAR                   VI1DP1
014800111005     C                   CLEAR                   VI1PD1
014900111005     C                   CLEAR                   VI1DD1
015000111005     C                   CLEAR                   VI1DD2
015100931111     C                   ENDSR
015200900528     C*
015300900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
015400900528     C     CONTR1        BEGSR
015500931111     C                   SETOFF                                       90
015600111005     C                   CLEAR                   VI1DP1
015700931126     C*
015800931126     C* D A T A   D I S T I N T A: DAL
015900931126     C                   MOVE      VI1DD1        G02DAT
016000931126     C                   MOVEL     *BLANK        G02ERR
016100950125     C                   CALL      'XSRDA8'
016200931126     C                   PARM                    WLBDAT
016300931126    1C     G02ERR        IFEQ      '1'
016400931126     C                   SETON                                        43  90
016500091112     C                   leavesr
016600931126    1C                   END
016700091112     C                   Z-ADD     G02INV        parDD1
016800950125     C                   Z-ADD     G02DAT        VI1DD1
016900991228     C                   Z-ADD     G02TGI        DD1GIO
017000011115     C                   z-add     vi1dd1        an1               4 0
017100000203     C* CONTROLLO L'ANNO
017200000203     C                   Z-ADD     G02DAT        W0040             4 0
017300000203     C     W0040         IFGT      ANNOPI
017400000203     C     W0040         ORLT      ANNOME
017500000203     C                   SETON                                        43  90
017600091112     C                   leavesr
017700000203    1C                   END
017800931126     C*
017900021106     C* comunque imposta il 2 campo data
018000091112     C                   Z-ADD     pardd1        pardd2
018100931126    1C     VI1DD2        IFGT      0
018200931126     C                   MOVE      VI1DD2        G02DAT
018300931126     C                   MOVEL     *BLANK        G02ERR
018400950125     C                   CALL      'XSRDA8'
018500931126     C                   PARM                    WLBDAT
018600931126    2C     G02ERR        IFEQ      '1'
018700931126     C                   SETON                                        44  90
018800091112     C                   leavesr
018900931126    2C                   END
019000091112     C                   Z-ADD     G02INV        parDD2
019100950125     C                   Z-ADD     G02DAT        VI1DD2
019200991228     C                   Z-ADD     G02TGI        DD2GIO
019300011115     C                   z-add     vi1dd2        an2               4 0
019400000203     C* CONTROLLO L'ANNO
019500091112     C                   Z-ADD     G02DAT        w0040             4 0
019600011115     c     an1           ifne      an2
019700011115     C                   SETON                                        56  90
019800091112     C                   leavesr
019900011115     c                   end
020000000203     C     W0040         IFGT      ANNOPI
020100000203     C     W0040         ORLT      ANNOME
020200000203     C                   SETON                                        44  90
020300091112     C                   leavesr
020400000203    1C                   END
020500931126     C* DATA AL NO < DATA DAL
020600091112    2C     parDD2        IFLT      parDD1
020700931126     C                   SETON                                        45  90
020800091112     C                   leavesr
020900931126    2C                   ENDIF
021000991228     C* I GIORNI TRA UNA DATA E L'ALTRA NON DEVONO SUPERARE
021100991228     Q* QUELLI DI PULIZIA + 10
021200991228     C     §5AFTT        ADD       10            WGIORN            4 0
021300991228     C**
021400991228     C     DD2GIO        SUB       DD1GIO        WRESTO
021500991228     C**
021600991228    2C     WRESTO        IFGT      WGIORN
021700991228     C                   SETON                                        55  90
021800091112     C                   leavesr
021900991228    2C                   ENDIF
022000991228     C**
022100991228    1C                   ENDIF
022200931123     C*
022300031023     C* Se non è un utente EDPxxx controlla la filiale inserita
022400091116     c                   if        primi_tre <> 'EDP' and dutlpo <> 'S'
022500031023     C*
022600111005     C* F I L I A L E    P A D R O N C I N O
022700950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
022800950125    1C     *IN89         IFEQ      *OFF
022900950125     C     VI1PF1        IFNE      SIMFEL
023000080109     C* non può essere (0) la filiale
023100080109     c                   if        VI1PF1 = 0
023200080109     c                   z-add     simfel        VI1PF1
023300080109     c                   end
023400950125     C     VI1PF1        LOOKUP    L1                                     35
023500950125     C  N35              SETON                                        5390
023600091112     C  N35              leavesr
023700950125     C                   ENDIF
023800950125     C*
023900950125   X1C                   ELSE
024000020701     C                   MOVEL     VI1PF1        WFIL              3 0
024100091112     C     WFIL          IFNE      dutPOU
024200950125     C     VI1PF1        LOOKUP    L6                                     35
024300950125     C  N35              SETON                                        5390
024400091112     C  N35              leavesr
024500950125     C                   ENDIF
024600950125    1C                   ENDIF
024700031023     c                   end
024800931116     C* P A D R O N C I N O
024900931116     C* RICERCA
025000931116     C     '?'           SCAN      VI1PD1                                 90
025100931116     C*
025200931126    1C     *IN90         IFEQ      *ON
025300021203     C                   Z-ADD     VI1PF1        D24FIL
025400931126     C                   EXSR      RICPAD
025500931126     C*
025600021203     C     D24PDR        IFNE      0
025700021203     C                   MOVEL     D24PDR        VI1PF1
025800931116     C                   MOVEL     *ZEROS        VI1PD1
025900021203     C                   MOVE      D24PDR        VI1PD1
026000021203     C                   MOVEL     D24RSC        VI1DP1
026100950125     C                   ENDIF
026200091112     C                   leavesr
026300931126    1C                   ENDIF
026400931116     C*
026500931126    1C     VI1PD1        IFNE      *BLANKS
026600931126     C     VI1PD1        ANDNE     *ZEROS
026700931126     C                   TESTN                   VI1PD1               30
026800111005     C                   IF        NOT *IN30
026900111005     C                   SETON                                        4190
027000111005     C                   leavesr
027100111005     C                   END
027200910320     C*
027300111005     C     KAPDV         CHAIN     FIAPD000                           30
027400111005     C*
027500111005    3C     *IN30         IFEQ      *ON
027600111005     C* Tolto il controllo sul padroncino annullato
027700111005     C                   SETON                                        4190
027800111005     C                   leavesr
027900111005    3C                   ENDIF
028000111005     C*
028100111005     C                   MOVEL     APDRSf        VI1DP1
028200111005     C                   ENDIF
028300111005     C*
028400091112     C                   ENDSR
028500931126     C*
028600931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
028700931126     C     RICPAD        BEGSR
028800021203     C                   MOVEL     *BLANKS       D24RSC
028900021203     C                   Z-ADD     0             D24PDR
029000021203     C                   MOVEL     'R'           D24FLG
029100021203     C                   MOVEL     'A'           D24TIP
029200021203     C                   MOVEL(P)  FNLV24DS      KPJBU
029300950125     C                   CALL      'FNLV24R'
029400931126     C                   PARM                    KPJBA
029500021203     C                   MOVEL     KPJBU         FNLV24DS
029600931126     C                   ENDSR
