000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200011221     F* TRmz02R4 *-----------------------------------------*
000300000000     F*----------------*                                   *
000400000000     F*          RICERCA PER N.TARGA                       *
000500000000     F*          --------------------                      *
000800000000     F*----------------------------------------------------*
000900011221     FmzAME02L  IF   E           K DISK
001000011221     FTRmz02D4  CF   E             WORKSTN
001100000000     F                                     SFILE(GA02D4SF:NRR)
001200000000     C*----------------------------------------------------*
001300000000     C     *ENTRY        PLIST                                                  *
001400000000     C                   PARM                    CODUT             1 0          *
001500000000     C                   PARM                    RSUT             20            *
001600000000     C                   PARM                    VTGA              2            *
001700000000     C                   PARM                    VTGN              9            *
001800000000     C*----------------------------------------------------*
001900000000     C*---------  POSIZIONAMENTO X TIPO TARGA              *
002000000000     C     KTAR          KLIST                                                  *
002100000000     C                   KFLD                    VTGA                           *
002200000000     C*----------------------------------------------------*
002300000000     C* PULIZIA SUBFILE
002400000000     C                   EXSR      PULIZ
002500000000     C* CARICAMENTO SUBFILE
002600000000     C                   EXSR      CARIC
002700000000     C                   WRITE     GA02D401
002800000000     C     C1TAG         TAG
002900000000     C     NRR           IFEQ      0
003000000000     C                   GOTO      FINE
003100000000     C                   END
003200000000      *
003300000000     C                   EXFMT     GA02D4SC
003400000000     C                   SETON                                        71
003500011221     C   Kc              GOTO      FINE
003600000000     C                   READC     GA02D4SF                               21    *
003700000000     C   21              SETOFF                                       71
003800000000     C*
003900000000     C     *IN21         CABEQ     '1'           C1TAG
004000000000     C     S1SEL         CABEQ     *BLANK        C1TAG
004100000000     C*
004200000000     C                   MOVE      S1TGA         VTGA
004300000000     C                   MOVEL     S1TGN         VTGN
004400000000     C     FINE          TAG
004500000000     C                   SETON                                        LR
004600000000     C**----------------------
004700000000     C** PULIZIA DEL SUBFILE
004800000000     C**----------------------
004900000000     C     PULIZ         BEGSR
005000000000     C                   SETON                                        70
005100000000     C                   SETOFF                                       71
005200000000     C                   WRITE     GA02D4SC
005300000000     C                   SETOFF                                       70
005400000000     C                   SETON                                        71
005500000000     C                   ENDSR
005600000000     C**----------------------
005700000000     C** CARICAMENTO SUBFILE
005800000000     C**----------------------
005900000000     C     CARIC         BEGSR
006000000000     C                   Z-ADD     0             NRR               4 0
006100011221     C     KTAR          SETLL     mzAME000
006200000000     C     SUTAR         TAG
006300000000     C                   DO        *HIVAL
006400011221     C                   READ      mzAME000                               01
006500000000     C  N01              DO
006501011221     c                   if        vtga <> *blanks and ametga > vtga
006502011221     c                   leave
006503011221     c                   end
006600000000     C                   MOVEL     AMETGA        S1TGA
006700000000     C                   MOVEL     AMETGN        S1TGN
006800000000     C                   ADD       1             NRR
006900000000     C                   WRITE     GA02D4SF
007000000000     C                   END
007100000000     C  N01              END
007200000000     C                   ENDSR
007300000000     C******************************************************
