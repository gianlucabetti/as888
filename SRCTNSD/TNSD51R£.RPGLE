000100921019     H DECEDIT('0,') DATEDIT(*DMY.)
000200941115     F* TNSD51R *-----------------------------------------------------*
000300921019     F*----------------*                                              *
000400921103     F* VISUALIZZAZIONE E ANNULLAMENTO BOLLE FILIALE DI BOLOGNA       *
000500921019     F*---------------------------------------------------------------*
000600941115     FTNSD51D   CF   E             WORKSTN
000700941115     F                                     SFILE(TNSD51S:NRR)
000800021029     FFIVABWKL  UF   E           K DISK
000900921019     D KPJBA         E DS
001000921019     D CNCR80        E DS
001100921019     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200921019     C****************************************************************
001300921019     C*  RIEPILOGO INDICATORI
001400921019     C***************************************************************
001500921103     C* 60/62 - GESTIONE SUBFILE
001600921103     C* 50    - INDICATORE LETTURA FLBLC11L
001700921019     C*
001800921019     C*****************************************************************
001900921019     C     *ENTRY        PLIST
002000921019     C                   PARM                    KPJBA
002100921019     C                   Z-ADD     1             CODUT
002200921019     C                   CALL      'X§PARUT'
002300921019     C                   PARM                    UT§DSE
002400921019     C                   MOVEL     RAGUT         RSUT             20
002500921019     C                   MOVEL     REC80         CNCR80
002600960104     C*
002700921103     C                   Z-ADD     *ZEROS        NUMBOL
002800960104     C     *LIKE         DEFINE    VABRMN        COMRMN
002900960104     C     *LIKE         DEFINE    VABRMN        NUMBOL
003000921019     C*----------------------------------------------------*
003100921103     C*
003200921103     C* CARICAMENTO SUBFILE
003300921103     C                   EXSR      CARSBF
003400921103     C*
003500921103     C     NRR           COMP      *ZEROS                             60
003600921103     C                   SETON                                        61
003700921103     C*
003800941115     C                   WRITE     TNSD511
003900921103     C*
004000921103     C                   DO        *HIVAL
004100921103     C* 90 INDICATORE CHE INDICA UN EVEMTUALE ANNULLAMENTO
004200921103     C                   SETOFF                                       9051
004300921103     C*
004400941115     C                   EXFMT     TNSD51C
004500921103     C*
004600921103     C  NKL
004700921103     CAN 60              DO        *HIVAL
004800921103     C*
004900941115     C                   READC     TNSD51S                                51
005000960104     C*  SELEZIONE
005100921103     C  N51VIDOPZ        IFEQ      'X'
005200921103     C     NUMBOL        ANDEQ     *ZEROS
005300960104     C                   MOVEL     VIDBOL        NUMBOL
005400921103     C                   MOVEL     NUMBOL        KPJBU
005500921103     C                   SETON                                            51
005600921103     C                   GOTO      FINE
005700921103     C                   END
005800960104     C*  ANNULLAMENTO
005900921103     C  N51VIDOPZ        IFEQ      'A'
006000921103     C                   Z-ADD     VIDBOL        COMRMN
006100021029     C     COMRMN        DELETE    FIVABWKL                           52
006200921103     C                   SETON                                        90
006300921103     C                   END
006400921103     C*
006500921103     C  N51              MOVEL     *BLANKS       VIDOPZ
006600941115     C  N51              UPDATE    TNSD51S
006700921103     C*
006800921103     C  N51              END
006900921103     C*
007000921103     C* SE INDICATORE 90 ACCESO E NUMBOL = A ZERO RIEMETTO IL NUOVO SUB
007100921103     C* FILE
007200921103     C*
007300921103     C   90
007400921103     CANNKLNUMBOL        IFEQ      *ZEROS
007500921103     C                   EXSR      CARSBF
007600921103     C     NRR           COMP      *ZEROS                             60
007700921103     C                   SETON                                        61
007800921103     C                   END
007900921103     C*
008000921103     C  NKL              END
008100921103     C*
008200921103     C     FINE          TAG
008300921103     C                   SETON                                        LR
008400921103     C*---------------------------------------------------------------
008500921103     C* CARICAMENTO SUBFILE BOLLE ANCORA DA STAMPARE
008600921103     C*---------------------------------------------------------------
008700921103     C*
008800921103     C     CARSBF        BEGSR
008900921103     C*
009000921103     C                   SETOFF                                       6061
009100921103     C*
009200921103     C                   SETON                                        62
009300941115     C                   WRITE     TNSD51C
009400921103     C                   SETOFF                                       62
009500921103     C*
009600921103     C                   Z-ADD     *ZEROS        NRR               4 0
009700921103     C*
009800921103     C*
009900021029     C     *LOVAL        SETLL     FIVABWKL
010000021029     C                   READ(N)   FIVABWKL                               54
010100921103     C*
010200960104     C     *IN54         DOWEQ     *OFF
010300921103     C*
010400921103     C                   ADD       1             NRR
010500921103     C                   MOVE      *BLANKS       VIDOPZ
010600960104     C                   Z-ADD     VABRMN        VIDBOL
010700960104     C                   MOVEL     VABRSD        VIDRAG
010800960104     C                   MOVEL     VABLOD        VIDLOC
010900960104     C                   MOVEL     VABPRD        VIDPRV
011000960104     C                   MOVEL     VABCAD        VIDCAP
011100921103     C*
011200941115     C                   WRITE     TNSD51S
011300921103     C*
011400021029     C                   READ(N)   FIVABWKL                               54
011500960104     C                   ENDDO
011600921103     C*
011700921103     C                   ENDSR
