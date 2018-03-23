000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200941115     F* TNSD51R *-----------------------------------------------------*
000300921103     F* VISUALIZZAZIONE E ANNULLAMENTO BOLLE FILIALE DI BOLOGNA       *
000400921019     F*---------------------------------------------------------------*
000500941115     FTNSD51D   CF   E             WORKSTN
000600941115     F                                     SFILE(TNSD51S:NRR)
000700021029     FFIVABWKL  UF   E           K DISK
000800921019     D KPJBA         E DS
000900921019     D CNCR80        E DS
001000921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001100131024     D                 DS
001200131024     D  vabaas                 1      4  0
001300131024     D  vabmgs                 5      8  0
001400131024     D  vabdsp                 1      8  0
001500131024     D WLBDAT          DS                  INZ
001600131024     D  G02DAT                 1      8  0
001700131024     D  G02INV                 9     16  0
001800131024     D  G02ERR                17     17
001900131024     D  G02TGI                18     22  0
002000921019     C****************************************************************
002100921019     C*  RIEPILOGO INDICATORI
002200921019     C***************************************************************
002300921103     C* 60/62 - GESTIONE SUBFILE
002400921103     C* 50    - INDICATORE LETTURA FLBLC11L
002500921019     C*
002600921019     C*****************************************************************
002700921019     C     *ENTRY        PLIST
002800921019     C                   PARM                    KPJBA
002900921019     C                   Z-ADD     1             CODUT
003000921019     C                   CALL      'X§PARUT'
003100921019     C                   PARM                    UT§DSE
003200921019     C                   MOVEL     RAGUT         RSUT             20
003300921019     C                   MOVEL     REC80         CNCR80
003400960104     C*
003500921103     C                   Z-ADD     *ZEROS        NUMBOL
003600960104     C     *LIKE         DEFINE    VABRMN        COMRMN
003700960104     C     *LIKE         DEFINE    VABRMN        NUMBOL
003800921019     C*----------------------------------------------------*
003900921103     C*
004000921103     C* CARICAMENTO SUBFILE
004100921103     C                   EXSR      CARSBF
004200921103     C*
004300921103     C     NRR           COMP      *ZEROS                             60
004400921103     C                   SETON                                        61
004500921103     C*
004600941115     C                   WRITE     TNSD511
004700921103     C*
004800921103     C                   DO        *HIVAL
004900921103     C* 90 INDICATORE CHE INDICA UN EVEMTUALE ANNULLAMENTO
005000921103     C                   SETOFF                                       9051
005100921103     C*
005200941115     C                   EXFMT     TNSD51C
005300921103     C*
005400921103     C  NKL
005500921103     CAN 60              DO        *HIVAL
005600921103     C*
005700941115     C                   READC     TNSD51S                                51
005800960104     C*  SELEZIONE
005900921103     C  N51VIDOPZ        IFEQ      'X'
006000921103     C     NUMBOL        ANDEQ     *ZEROS
006100960104     C                   MOVEL     VIDBOL        NUMBOL
006200921103     C                   MOVEL     NUMBOL        KPJBU
006300921103     C                   SETON                                            51
006400921103     C                   GOTO      FINE
006500921103     C                   END
006600960104     C*  ANNULLAMENTO
006700921103     C  N51VIDOPZ        IFEQ      'A'
006800921103     C                   Z-ADD     VIDBOL        COMRMN
006900021029     C     COMRMN        DELETE    FIVABWKL                           52
007000921103     C                   SETON                                        90
007100921103     C                   END
007200921103     C*
007300921103     C  N51              MOVEL     *BLANKS       VIDOPZ
007400941115     C  N51              UPDATE    TNSD51S
007500921103     C*
007600921103     C  N51              END
007700921103     C*
007800921103     C* SE INDICATORE 90 ACCESO E NUMBOL = A ZERO RIEMETTO IL NUOVO SUB
007900921103     C* FILE
008000921103     C*
008100921103     C   90
008200921103     CANNKLNUMBOL        IFEQ      *ZEROS
008300921103     C                   EXSR      CARSBF
008400921103     C     NRR           COMP      *ZEROS                             60
008500921103     C                   SETON                                        61
008600921103     C                   END
008700921103     C*
008800921103     C  NKL              END
008900921103     C*
009000921103     C     FINE          TAG
009100921103     C                   SETON                                        LR
009200921103     C*---------------------------------------------------------------
009300921103     C* CARICAMENTO SUBFILE BOLLE ANCORA DA STAMPARE
009400921103     C*---------------------------------------------------------------
009500921103     C*
009600921103     C     CARSBF        BEGSR
009700921103     C*
009800921103     C                   SETOFF                                       6061
009900921103     C*
010000921103     C                   SETON                                        62
010100941115     C                   WRITE     TNSD51C
010200921103     C                   SETOFF                                       62
010300921103     C*
010400921103     C                   Z-ADD     *ZEROS        NRR               4 0
010500921103     C*
010600921103     C*
010700021029     C     *LOVAL        SETLL     FIVABWKL
010800021029     C                   READ(N)   FIVABWKL                               54
010900921103     C*
011000960104     C     *IN54         DOWEQ     *OFF
011100921103     C*
011200921103     C                   ADD       1             NRR
011300921103     C                   MOVE      *BLANKS       VIDOPZ
011400960104     C                   Z-ADD     VABRMN        VIDBOL
011500960104     C                   MOVEL     VABRSD        VIDRAG
011600960104     C                   MOVEL     VABLOD        VIDLOC
011700960104     C                   MOVEL     VABPRD        VIDPRV
011800960104     C                   MOVEL     VABCAD        VIDCAP
011900131024     c* data spedizione
012000131024     C*
012100131024     C                   Z-ADD     vabdsp        G02inv
012200131024     C                   MOVEL     '3'           G02ERR
012300131024     C                   CALL      'XSRDA8'
012400131024     C                   PARM                    WLBDAT
012500131024     c                   movel     g02dat        viddsp
012600131024     c                   move      g02dat        w0020             2 0
012700131024     c                   move      w0020         viddsp
012800921103     C*
012900941115     C                   WRITE     TNSD51S
013000921103     C*
013100021029     C                   READ(N)   FIVABWKL                               54
013200960104     C                   ENDDO
013300921103     C*
013400921103     C                   ENDSR
