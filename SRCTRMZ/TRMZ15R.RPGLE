000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011212     F* TRMZ15R * -----------------------------------------*
000300000000     F*----------------*                                   *
000400000000     F*          INTERVENTI DI MANUTENZIONE                *
001000000000     F*----------------------------------------------------*
001100011212     FmzTIM01L  UF A E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300011212     FTRmz15D   CF   E             WORKSTN
001400000000     F                                     SFILE(GA15DSF1:NRR)
001500000000     D PARAM2          DS
001600000000     D  SCEL                   1      1
001700000000     D  VIDINT                 2      3
001800000000     D KPJBA         E DS
001900000000     D  LIBSYS                92    100
002000000000     D  MSGERR               453    502
002100000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002200000000     D  TCU                  398    697
002300000000     D                                     DIM(50)                              TAB FLAG CNT
002400000000     D  KCU                  698    847P 0
002500000000     D                                     DIM(50)                              TAB CAPO CONTO
002600000000     D                                     PACKEVEN
002700000000     D  DTU                  848    895P 0
002800000000     D                                     DIM(12)                              DATE PARAM
002900000000     D                                     PACKEVEN
003000000000     I/SPACE 3
003100000000     C*---------------------------------------------------------------*
003200000000     C     *ENTRY        PLIST
003300000000     C                   PARM                    KPJBA
003400000000     C                   Z-ADD     1             CODUT
003500000000     C                   CALL      'XPARUT'
003600000000     C                   PARM                    UTEDSE
003700000000     C     CODUT         CABEQ     -1            FINE
003800000000     C                   MOVEL     RAGUT         RSUT             20
003900000000     C                   MOVEL     RAGUT         RSUT2            25
004000000000     C*----------------------------------------------------*
004100000000     C* POSIZIONAMENTO TABELLE                             *
004200000000     C     KTAB          KLIST                                                  *
004300000000     C                   KFLD                    CODUT                          *
004400000000     C                   KFLD                    COD               2            *
004500000000     C                   KFLD                    KEY               8            *
004600000000     C* LETTURA TABELLE X CARIC.SUBFILE                    *
004700000000     C     KTAB1         KLIST                                                  *
004800000000     C                   KFLD                    CODUT                          *
004900000000     C                   KFLD                    COD1              2            *
005000000000     C     KTIM          KLIST                                                  *
005100000000     C                   KFLD                    TIP               1            *
005200000000     C                   KFLD                    VIDINT                         *
005300000000     C                   KFLD                    SFMAN                          *
005400000000     C     KTIM1         KLIST                                                  *
005500000000     C                   KFLD                    TIP                            *
005600000000     C                   KFLD                    VIDINT                         *
005700000000     C*----------------------------------------------------*
005800000000     C                   MOVEL     *BLANKS       PARAM2
006600000000     C*
006700000000     C     INIZIO        TAG
006800000000     C*          -------------
006900000000     C                   MOVEL     *BLANKS       SCELP
007000000000     C                   MOVEL     *BLANKS       SCELG
007100000000     C     CON100        TAG
007200000000     C                   EXFMT     GA15D001
007300000000     C* CMD12 FINE LAVORO
007400011214     C   Kc              GOTO      FINE
007800000000     C     SCELP         IFEQ      *BLANKS
007900000000     C     SCELG         ANDEQ     *BLANKS
008000000000     C                   GOTO      INIZIO
008100000000     C                   END
008500000000     C                   MOVEL     *BLANKS       VIDINT
008600000000     C                   MOVEL     *BLANKS       VIDDES
008700000000     C     CON200        TAG
008800000000     C                   EXFMT     GA15D002
008900000000     C                   SETOFF                                       99
009000000000     C* CMD12 FINE LAVORO
009100011214     C   Kc              GOTO      fine
009501011214     C   kd§fld          IFEQ      'VIDINT'
009502011214     c                   movel     vidint        parkey
009503011214     C     SCELP         IFEQ      'X'
009504011214     C                   MOVEL     'XM'          PARCOD            2
009506011214     C                   ELSE
009507011214     C                   MOVEL     'XE'          PARCOD
009509011214     C                   END
009510011214     c                   exsr      srtab
009511011214     c                   movel     parkey        vidint
009512011214     C                   SETON                                        99
009513011214     C                   GOTO      CON200
009514011214     C                   END
009515011214     C     VIDINT        IFEQ      *BLANKS
009516011214     C                   SETON                                        40
009517011214     C                   GOTO      CON200
009518011214     C                   END
010800000000     C     SCELP         IFEQ      'X'
010900000000     C                   MOVEL     'XM'          COD
011000000000     C                   ELSE
011100000000     C                   MOVEL     'XE'          COD
011200000000     C                   END
011300000000     C                   MOVEL     *BLANKS       KEY
011400000000     C                   MOVEL     VIDINT        KEY
011500000000     C     KTAB          CHAIN     TABEL                              40
011600000000     C   40              GOTO      CON200
011700000000     C  N40              MOVEL     TBLUNI        VIDDES
012100000000     C* PULIZIA SUBFILE
012200000000     C                   EXSR      PULIZ
012300000000     C* CARICAMENTO SUBFILE
012400000000     C                   EXSR      CARIC
012500000000     C                   WRITE     GA15D003
012600000000     C     CON300        TAG
012700000000     C                   EXFMT     GA15DSC1
012800011214     C   Kc              GOTO      CON100
012900011214     C   Kf              DO
013000000000     C                   EXSR      AGGIOR
013100000000     C                   GOTO      INIZIO
013200000000     C                   END
013300000000     C   KW              DO
013400000000     C                   EXSR      ANNULL
013500000000     C                   GOTO      INIZIO
013600000000     C                   END
013700011214     C  NKf              GOTO      CON300
013800000000     C     FINE          TAG
013900000000     C                   SETON                                        LR
013901011214     C**********************************************************************
013902011214     C** Controlli e decodifiche campi con tabel00f
013903011214     C**********************************************************************
013904011214     C     srtab         BEGSR
013906011214     c                   movel     parkey        parkeys           8
013907011214     C                   CALL      'TRUL19R'
013908011214     c                   parm                    parcod            2
013909011214     c                   parm      '1'           parord            1
013910011214     c                   parm      *blanks       parkey            8
013911011214     c                   parm      *blanks       cmd               1
013912011214     c* ritorno senza scelta
013913011214     c                   if        cmd = 'L'
013916011214     c                   movel     parkeys       parkey
013917011214     C                   END
013923011214     c*
013924011214     C                   ENDSR
014000000000     C******************************************************
014100000000     C     PULIZ         BEGSR
014200000000     C**----------------------
014300000000     C** PULIZIA DEL SUBFILE
014400000000     C**----------------------
014500000000     C                   SETON                                        70
014600000000     C                   WRITE     GA15DSC1
014700000000     C                   SETOFF                                       70
014800000000     C                   ENDSR
014900000000     C******************************************************
015000000000     C     CARIC         BEGSR
015100000000     C**----------------------
015200000000     C** CARICAMENTO SUBFILE
015300000000     C** CARICO RECORDS DA FILE
015400000000     C**----------------------
015500000000     C                   SETOFF                                       4142
015600000000     C                   Z-ADD     0             NRR               4 0
015700000000     C     SCELP         IFEQ      'X'
015800000000     C                   MOVEL     'XL'          COD1
015900000000     C                   MOVEL     'P'           TIP
016000000000     C                   SETON                                        42
016100000000     C                   ELSE
016200000000     C                   MOVEL     'XC'          COD1
016300000000     C                   MOVEL     'G'           TIP
016400000000     C                   SETON                                        41
016500000000     C                   END
016600000000     C     KTAB1         SETLL     TABEL
016700000000     C     CAR100        TAG
016800000000     C                   DO        *HIVAL
016900000000     C     KTAB1         READE     TABEL                                  01
017000000000     C   01              GOTO      END2
017100000000     C                   MOVEL     *BLANKS       SFMAN
017200000000     C                   MOVEL     *BLANKS       SFDES
017300000000     C  N01              DO
017400000000     C                   MOVEL     TBLKEY        SFMAN
017500000000     C                   MOVEL     TBLUNI        SFDES
017600000000     C                   END
017700011212     C     KTIM          CHAIN     mzTIM000                           02
017800000000     C                   Z-ADD     0             SFPER
017900000000     C                   MOVEL     *BLANKS       SFUNI
018000000000     C                   Z-ADD     0             SFTOL
018100000000     C  N02              DO
018200000000     C                   Z-ADD     TIMPER        SFPER
018300000000     C                   MOVEL     TIMUNI        SFUNI
018400000000     C                   Z-ADD     TIMTOL        SFTOL
018500000000     C                   END
018600000000     C                   ADD       1             NRR
018700000000     C                   WRITE     GA15DSF1
018800000000     C                   END
018900000000     C     END2          TAG
019000000000     C                   ENDSR
019100000000     C******************************************************
019200000000     C     AGGIOR        BEGSR
019300000000     C**----------------------
019400000000     C** AGGIORNAMENTO FILE
019500000000     C**----------------------
019600000000     C                   DO        *HIVAL        A                 3 0
019700000000     C     A             CHAIN     GA15DSF1                           05
019800000000     C   05              GOTO      END1
019900011212     C     KTIM          CHAIN     mzTIM000                           02
020000000000     C     SCELP         COMP      'X'                                    20
020100000000     C   20              MOVEL     'P'           TIMTIP
020200000000     C  N20              MOVEL     'G'           TIMTIP
020300000000     C                   MOVEL     SFMAN         TIMMAN
020400000000     C                   MOVEL     VIDINT        TIMINT
020500000000     C                   Z-ADD     SFPER         TIMPER
020600000000     C                   MOVEL     SFUNI         TIMUNI
020700000000     C                   Z-ADD     SFTOL         TIMTOL
020800000000     C     TIMPER        IFNE      0
020900011212     C   02              WRITE     mzTIM000
021000011212     C  N02              UPDATE    mzTIM000
021100000000     C                   ELSE
021200011212     C  N02              DELETE    mzTIM000
021300000000     C                   END
021400000000     C                   END
021500000000     C     END1          TAG
021600000000     C                   ENDSR
021700000000     C******************************************************
021800000000     C     ANNULL        BEGSR
021900000000     C**----------------------
022000000000     C** ANNULLAMENTO X TIPO
022100000000     C** INTERVENTO
022200000000     C**----------------------
022300000000     C                   DO        *HIVAL
022400011212     C     KTIM1         DELETE    mzTIM000                           11
022500000000     C  N11              END
022600000000     C                   ENDSR
