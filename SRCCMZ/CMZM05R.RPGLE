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
002300051207     fCMTSM03L  if   e           k disk    rename(CMTSM000:CMTSM003)
002400921104     FCMTSM04L  UF A E           K DISK
002500921103?    F*------------
002600921104     FCMRGM01L  UF A E           K DISK
002700921104     D*
002800921104?    D*------------------------------------------------------------
002900921104?    D*  DS
003000921104?    D*------------------------------------------------------------
003100921104      *---
003200921104     D                SDS
003300921104     D  SYSUTL               254    263
003400961114     D  USRXXX               254    256
003500961114     D  REMFIL               257    259
003600921104      *---
003700921104     D CMZM01        E DS                  EXTNAME(CMZM01PF)
003800921218     D CMZ001        E DS                  EXTNAME(CMZ001DS)
003900021113     D TIBS36        E DS                  EXTNAME(TIBS36DS)
004000921218     D DATA            DS
004100921218     D  GG                     1      2  0
004200921218     D  MM                     3      4  0
004300921218     D  AA                     5      6  0
004400921103?     *
004500921103?     *------------------------------------------------------------
004600921103?     *  FLUSSO PRINCIPALE
004700921103?     *------------------------------------------------------------
004800921103?     *
004900921103?    C                   EXSR      INIZIO
005000921103     C*
005100921104     C*  Aggiorno il file delle testate solo in prima chiamata.
005200921103     C*
0053009211041    C     M05FLG        IFEQ      '0'
005400921104?    C                   EXSR      AGGTST
005500921104     C*
005600921104     C*  Cancello il vecchio documento se in prima chiamata e se
005700921104     C*  non è nuovo *IN30 = '0'.
005800921104     C*
0059009211052    C  N30              DO        *HIVAL
006000921104     C     M05DCR        DELETE    CMRGM01L                           31
0061009211042-   C  N31              END
006200921104     C*
0063009211041-   C                   END
006400921104     C*
006500921104     C*  Aggiorno il file delle righe.
006600921103     C*
006700921104?    C                   EXSR      AGGRIG
006800921103     C*
006900921103     C                   SETON                                            LR
007000921103?     *
007100921103?     *------------------------------------------------------------
007200921103?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
007300921103?     *------------------------------------------------------------
007400921103?     *
007500921103     C*
007600921103?    C     INIZIO        BEGSR
007700921103     C*
007800921103     C     *ENTRY        PLIST
007900921104     C                   PARM                    M05TXT           70
008000921104     C                   PARM                    M05DCR            7 0
008100921104     C                   PARM                    M05FCC            1
008200921118     C                   PARM                    M05RIG          198
008300921104     C                   PARM                    M05FLG            1
008400921110     C                   PARM                    M05DOS            7 0
008500921103     C*
008600921103     C                   MOVE      '0'           *IN
008700921104     C     *DTAARA       DEFINE                  CMZM01
008800921218     C     *DTAARA       DEFINE                  CMZ001
008900921218     C                   SETON                                        01
0090009212182    C     *IN01         DOWEQ     '1'
009100921218     C                   IN        CMZ001                               01
0092009212182-   C                   END
009300921104     C*
009400921104     C* Se prima chiamata e devo creare un documento prelevo Numerato-
009500921104     C* re dalla dataarea CMZM01.
009600921104     C*
0097009211041    C     M05FLG        IFEQ      '0'
009800921104     C     M05DCR        ANDEQ     *ZERO
009900921104     C*
010000921104     C                   SETON                                        01
0101009211132    C     *IN01         DOWEQ     '1'
010200921113     C     *LOCK         IN        CMZM01                               01
0103009211132-   C                   END
010400921104     C                   MOVE      M01NRR        NUMNRR            7 0
010500051207     ***C                   ADD       1             NUMNRR
010600051207     c                   exsr      Chk_Free
010700921104     C                   MOVE      NUMNRR        M01NRR
010800921104     C                   OUT       CMZM01
010900921104     C                   MOVE      M01NRR        M05DCR
011000921104     C*
0111009211041-   C                   END
011200921103     C*
011300921103     C                   ENDSR
011400051207      *
011500051207?     *------------------------------------------------------------
011600051207      * CHK_FREE - Ricerca primo progressivo libero
011700051207?     *------------------------------------------------------------
011800051207      *
011900051207     c     Chk_Free      BEGSR
012000051207      *
012100051207do  1c                   DO        *hival
012200051207      *
012300051207do  2c                   DOU       NOT %equal(CMTSM04L)
012400051207     c                   add       1             NumNRR
012500051207if  3c                   if        NumNRR <= *zeros
012600051207     c                   eval      NumNRR =  1
012700051207e   3c                   endif
012800051207     c     NumNRR        setll     CMTSM000
012900051207e   2c                   ENDDO
013000051207      *
013100051207     c     NumNRR        setll     CMTSM003
013200051207if  2c                   if        NOT %equal(CMTSM03L)
013300051207     c                   leave
013400051207e   2c                   endif
013500051207      *
013600051207e   1c                   ENDDO
013700051207      *
013800051207     c                   ENDSR
013900921104?     *
014000921104?     *------------------------------------------------------------
014100921104?     *  A G G T S T   - AGGIORNAMENTO FILE DELLE TESTATE
014200921104?     *------------------------------------------------------------
014300921104?     *
014400921104     C*
014500921104?    C     AGGTST        BEGSR
014600921104     C*
014700921104     C     M05DCR        CHAIN     CMTSM04L                           30
0148009211041    C     *IN30         IFEQ      '0'
014900921104     C                   MOVE      *BLANK        TSMTXT
015000921104     C                   MOVEL     M05TXT        TSMTXT
015100921104     C                   UPDATE    CMTSM000
0152009211041E   C                   ELSE
015300921104     C                   MOVE      'P'           TSMTIP
015400921104     C                   MOVE      *ZERO         TSMFIR
015500921104     C                   MOVE      *ZERO         TSMDSR
015600921104     C                   MOVE      *BLANK        TSMUTR
015700921104     C                   MOVE      SYSUTL        TSMUTL
015800921104     C                   MOVE      *ZERO         TSMDOR
015900921104     C                   MOVE      M05DCR        TSMDOL
016000921104     C                   MOVE      M05DCR        TSMDOC
016100921104     C                   MOVEL     M05TXT        TSMTXT
016200921218     C                   TIME                    W012             12 0
016300921218     C                   MOVEL     W012          W006              6 0
016400921218     C                   Z-ADD     W006          TSMORR
016500921218     C                   MOVE      W012          DATA
016600921218     C                   MOVE      AA            AAA               2
016700921218     C                   MOVE      GG            AA
016800921218     C                   MOVE      AAA           GG
016900921218     C                   MOVE      DATA          TSMDTR
017000921104     C                   MOVE      *ZERO         TSMDTI
017100921104     C                   MOVE      *ZERO         TSMORI
017200921104     C                   MOVE      *BLANK        TSMMSG
017300921104     C                   MOVE      *BLANK        TSMSTP
017400921104     C                   MOVE      *BLANK        TSMRIS
017500921104     C                   MOVE      *BLANK        TSMMSR
017600921104     C                   MOVE      *ZERO         TSMDTL
017700921104     C                   MOVE      *ZERO         TSMDTS
017800921104     C                   MOVE      *ZERO         TSMDTP
017900921104     C                   MOVE      *BLANK        TSMURG
018000921104     C                   MOVE      *BLANK        TSMURR
018100921110     C                   MOVE      M05DOS        TSMDOS
018200921104     C                   MOVE      '9'           TSMSTS
018300921104     C                   MOVE      *BLANK        TSMTRA
018400021004     C** INSERIMENTO USO AZUTE
018500021113     C                   CLEAR                   TIBS36
018600021113     C                   MOVEL     sysutl        I36ute
018700021113     C                   MOVEL     'L'           I36TLA
018800021113     C                   CALL      'TIBS36R'                            31
018900021113     C                   PARM                    TIBS36
019000021113     c     o36err        ifeq      *blank
019100021113     c                   movel     O36pou        TSMFIL
019200021004     c                   endif
019300021004     C     TSMFIL        IFle      *zeros
019400021004     C                   MOVE      £DST          TSMFIL
019500021004     C                   END
019600921104     C                   WRITE     CMTSM000
0197009211041-   C                   END
019800921104     C*
019900921104     C                   ENDSR
020000921104?     *
020100921104?     *------------------------------------------------------------
020200921104?     *  A G G R I G   - AGGIORNAMENTO FILE DELLE RIGHE
020300921104?     *------------------------------------------------------------
020400921104?     *
020500921104     C*
020600921104?    C     AGGRIG        BEGSR
020700921104     C*
020800921104     C     M05DCR        CHAIN     CMRGM01L                           31
020900921104     C*
021000921104     C                   Z-ADD     M05DCR        RGMDOC
021100921104     C                   MOVE      M05FCC        RGMFCC
021200921104     C                   MOVEL     M05RIG        RGMRIG
021300921104     C*
021400921104     C                   WRITE     CMRGM000
021500921104     C*
021600921104     C                   ENDSR
