000100921104     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000200921104?    F* CMZM05R *----------------------------------------------------*
000300921103?    F*---------*                                                    *
000400921104?    F*               - CARICA DOCUMENTO -                           *
000500921103?    F*               --------------------                           *
000600921103?    F*                                                              *
000700921103?    F*                                                 AZ.          *
000800921103?    F*                                                              *
000900921103?    F*--------------------------------------------------------------*
001000921103?    F* I N D I C A T O R I                                          *
001100921103?    F*--------------------------------------------------------------*
001200921103     F*                                                              *
001300921104     F* -01- FLAG    PER GESTIONE LOCK AREA DATI                     *
001400921103     F*                                                              *
001500921103     F* -30- LETTURA CMTSM04L                                        *
001600921103     F* -31- LETTURA CMRGM01L                                        *
001700921103?    F*
001800921103?    F*--------------------------------------------------------------*
001900921103?    F*  DATA BASE                                                   *
002000921103?    F*--------------------------------------------------------------*
002100921103     F*
002200921103?    F*------------
002300921104     FCMTSM04L  UF A E           K DISK
002400921103?    F*------------
002500921104     FCMRGM01L  UF A E           K DISK
002600921104     D*
002700921104?    D*------------------------------------------------------------
002800921104?    D*  DS
002900921104?    D*------------------------------------------------------------
003000921104      *---
003100921104     D                SDS
003200921104     D  SYSUTL               254    263
003300961114     D  USRXXX               254    256
003400961114     D  REMFIL               257    259
003500921104      *---
003600921104     D CMZM01        E DS                  EXTNAME(CMZM01PF)
003700921218     D CMZ001        E DS                  EXTNAME(CMZ001DS)
003800021113     D TIBS36        E DS                  EXTNAME(TIBS36DS)
003900921218     D DATA            DS
004000921218     D  GG                     1      2  0
004100921218     D  MM                     3      4  0
004200921218     D  AA                     5      6  0
004300921103?     *
004400921103?     *------------------------------------------------------------
004500921103?     *  FLUSSO PRINCIPALE
004600921103?     *------------------------------------------------------------
004700921103?     *
004800921103?    C                   EXSR      INIZIO
004900921103     C*
005000921104     C*  Aggiorno il file delle testate solo in prima chiamata.
005100921103     C*
0052009211041    C     M05FLG        IFEQ      '0'
005300921104?    C                   EXSR      AGGTST
005400921104     C*
005500921104     C*  Cancello il vecchio documento se in prima chiamata e se
005600921104     C*  non è nuovo *IN30 = '0'.
005700921104     C*
0058009211052    C  N30              DO        *HIVAL
005900921104     C     M05DCR        DELETE    CMRGM01L                           31
0060009211042-   C  N31              END
006100921104     C*
0062009211041-   C                   END
006300921104     C*
006400921104     C*  Aggiorno il file delle righe.
006500921103     C*
006600921104?    C                   EXSR      AGGRIG
006700921103     C*
006800921103     C                   SETON                                            LR
006900921103?     *
007000921103?     *------------------------------------------------------------
007100921103?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
007200921103?     *------------------------------------------------------------
007300921103?     *
007400921103     C*
007500921103?    C     INIZIO        BEGSR
007600921103     C*
007700921103     C     *ENTRY        PLIST
007800921104     C                   PARM                    M05TXT           70
007900921104     C                   PARM                    M05DCR            7 0
008000921104     C                   PARM                    M05FCC            1
008100921118     C                   PARM                    M05RIG          198
008200921104     C                   PARM                    M05FLG            1
008300921110     C                   PARM                    M05DOS            7 0
008400921103     C*
008500921103     C                   MOVE      '0'           *IN
008600921104     C     *DTAARA       DEFINE                  CMZM01
008700921218     C     *DTAARA       DEFINE                  CMZ001
008800921218     C                   SETON                                        01
0089009212182    C     *IN01         DOWEQ     '1'
009000921218     C                   IN        CMZ001                               01
0091009212182-   C                   END
009200921104     C*
009300921104     C* Se prima chiamata e devo creare un documento prelevo Numerato-
009400921104     C* re dalla dataarea CMZM01.
009500921104     C*
0096009211041    C     M05FLG        IFEQ      '0'
009700921104     C     M05DCR        ANDEQ     *ZERO
009800921104     C*
009900921104     C                   SETON                                        01
0100009211132    C     *IN01         DOWEQ     '1'
010100921113     C     *LOCK         IN        CMZM01                               01
0102009211132-   C                   END
010300921104     C                   MOVE      M01NRR        NUMNRR            7 0
010400921104     C                   ADD       1             NUMNRR
010500921104     C                   MOVE      NUMNRR        M01NRR
010600921104     C                   OUT       CMZM01
010700921104     C                   MOVE      M01NRR        M05DCR
010800921104     C*
0109009211041-   C                   END
011000921103     C*
011100921103     C                   ENDSR
011200921104?     *
011300921104?     *------------------------------------------------------------
011400921104?     *  A G G T S T   - AGGIORNAMENTO FILE DELLE TESTATE
011500921104?     *------------------------------------------------------------
011600921104?     *
011700921104     C*
011800921104?    C     AGGTST        BEGSR
011900921104     C*
012000921104     C     M05DCR        CHAIN     CMTSM04L                           30
0121009211041    C     *IN30         IFEQ      '0'
012200921104     C                   MOVE      *BLANK        TSMTXT
012300921104     C                   MOVEL     M05TXT        TSMTXT
012400921104     C                   UPDATE    CMTSM000
0125009211041E   C                   ELSE
012600921104     C                   MOVE      'P'           TSMTIP
012700921104     C                   MOVE      *ZERO         TSMFIR
012800921104     C                   MOVE      *ZERO         TSMDSR
012900921104     C                   MOVE      *BLANK        TSMUTR
013000921104     C                   MOVE      SYSUTL        TSMUTL
013100921104     C                   MOVE      *ZERO         TSMDOR
013200921104     C                   MOVE      M05DCR        TSMDOL
013300921104     C                   MOVE      M05DCR        TSMDOC
013400921104     C                   MOVEL     M05TXT        TSMTXT
013500921218     C                   TIME                    W012             12 0
013600921218     C                   MOVEL     W012          W006              6 0
013700921218     C                   Z-ADD     W006          TSMORR
013800921218     C***                  MOVE UDATE     TSMDTR
013900921218     C                   MOVE      W012          DATA
014000921218     C                   MOVE      AA            AAA               2
014100921218     C                   MOVE      GG            AA
014200921218     C                   MOVE      AAA           GG
014300921218     C                   MOVE      DATA          TSMDTR
014400921104     C                   MOVE      *ZERO         TSMDTI
014500921104     C                   MOVE      *ZERO         TSMORI
014600921104     C                   MOVE      *BLANK        TSMMSG
014700921104     C                   MOVE      *BLANK        TSMSTP
014800921104     C                   MOVE      *BLANK        TSMRIS
014900921104     C                   MOVE      *BLANK        TSMMSR
015000921104     C                   MOVE      *ZERO         TSMDTL
015100921104     C                   MOVE      *ZERO         TSMDTS
015200921104     C                   MOVE      *ZERO         TSMDTP
015300921104     C                   MOVE      *BLANK        TSMURG
015400921104     C                   MOVE      *BLANK        TSMURR
015500921110     C                   MOVE      M05DOS        TSMDOS
015600921104     C                   MOVE      '9'           TSMSTS
015700921104     C                   MOVE      *BLANK        TSMTRA
015800021004     C** INSERIMENTO USO AZUTE
015900021113     C                   CLEAR                   TIBS36
016000021113     C                   MOVEL     sysutl        I36ute
016100021113     C                   MOVEL     'L'           I36TLA
016200021113     C                   CALL      'TIBS36R'                            31
016300021113     C                   PARM                    TIBS36
016400021113     c     o36err        ifeq      *blank
016500021113     c                   movel     O36pou        TSMFIL
016600021004     c                   endif
016700021004     C     TSMFIL        IFle      *zeros
016800021004     C                   MOVE      £DST          TSMFIL
016900021004     C                   END
017000921104     C                   WRITE     CMTSM000
0171009211041-   C                   END
017200921104     C*
017300921104     C                   ENDSR
017400921104?     *
017500921104?     *------------------------------------------------------------
017600921104?     *  A G G R I G   - AGGIORNAMENTO FILE DELLE RIGHE
017700921104?     *------------------------------------------------------------
017800921104?     *
017900921104     C*
018000921104?    C     AGGRIG        BEGSR
018100921104     C*
018200921104     C     M05DCR        CHAIN     CMRGM01L                           31
018300921104     C*
018400921104     C                   Z-ADD     M05DCR        RGMDOC
018500921104     C                   MOVE      M05FCC        RGMFCC
018600921104     C                   MOVEL     M05RIG        RGMRIG
018700921104     C*
018800921104     C                   WRITE     CMRGM000
018900921104     C*
019000921104     C                   ENDSR
