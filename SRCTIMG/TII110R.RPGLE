000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* ANU155R  *-AN/38-*-------------------------------------------*
000300000000     F*------------------*                                           *
000400000000     F*       - RICERCA CODIFICA CLASSI MERCEOLOGICHE M.P. -         *
000500000000     F*       ----------------------------------------------         *
000600000000     F*         FILE : ANTAB00F                                      *
000700000000     F*--------------------------------------------------------------*
000800000000     FANTAB00F  IF   E           K DISK
000900000000     F*---------
001000990317     FTII110D   CF   E             WORKSTN
001100000000     F                                     SFILE(ANU55SF1:REC1)
001200000000     C*----------------------------------------------------*
001300000000     C     *ENTRY        PLIST                                                  *
001400000000     C                   PARM                    COCLAM            3            *
001500000000     C                   PARM                    DECLAM           25            *
001600000000     C*----------                                          *
001700000000     C     KEYTAB        KLIST                                                  *
001800000000     C                   KFLD                    TABGRU                         *
001900000000     C                   KFLD                    TABCOD                         *
002000000000     C                   WRITE     ANU1551                                      *
002100000000     C*----------------------------------------------------*
002200000000      /SPACE 2
002300000000     C*----------------------------------------------------*
002400000000     C*---------- GESTIONE SUBFILE ------------------------*
002500000000     C*----------------------------------------------------*
002600000000     C                   MOVE      '155'         TABGRU                         *
002700000000     C                   MOVE      *ZEROS        TABCOD                         *
002800000000     C     TABGRU        SETLL     ANTAB00F                           90  91    *
002900000000     C  N91              SETON                                        90        *
003000000000     C   90              GOTO      FINE                                         *
003100000000     C                   Z-ADD     0             REC1              3 0          *
003200000000     C                   MOVE      *BLANKS       SCELTA                         *
003300000000     C*---------- PULIZIA SLFCT :                          *
003400000000     C                   SETON                                            70    *
003500000000     C                   WRITE     ANU55SC1                                     *
003600000000     C                   SETOFF                                       7071      *
003700000000     C*---------- CARICAMENTO SFL :                        *
003800000000     C     SOPRA         TAG                                                    *
003900000000     C     TABGRU        READE     ANTAB000                               91    *
004000000000     C   91              GOTO      EMISF1                                       *
004100000000     C                   MOVE      TABCOD        CODCLM                         *
004200900109     C                   MOVEL     TABDES        DESCLM           25            *
004300000000     C     REC1          ADD       1             REC1                 71        *
004400000000     C                   WRITE     ANU55SF1                                     *
004500000000     C                   GOTO      SOPRA                                        *
004600000000     C*---------- EMISSIONE SFL :                          *
004700000000     C     EMISF1        TAG                                                    *
004800000000     C                   EXFMT     ANU55SC1                                     *
004900990114     C   Kc              SETON                                            90    *
005000990114     C   Kc              GOTO      FINE                                         *
005100000000     C*---------- LETTURA RECORD VARIATI :                 *
005200000000     C   71              READC     ANU55SF1                               91    *
005300000000     C  N71                                                                     *
005400000000     COR 91              GOTO      EMISF1                                       *
005500000000     C     SCELTA        CABNE     'X'           EMISF1                         *
005600000000     C                   MOVE      CODCLM        COCLAM                         *
005700000000     C                   MOVE      DESCLM        DECLAM                         *
005800000000     C*----------------------------------------------------*
005900000000      /SPACE 2
006000000000     C*----------------------------------------------------*
006100000000     C*     USCITA PROGRAMMA                               *
006200000000     C*----------------------------------------------------*
006300000000     C     FINE          TAG                                                    *
006400000000     C   90              MOVEL     '999'         COCLAM                         *
006500000000     C   90              MOVEL     'ERRORE'      DECLAM                         *
006600000000     C                   SETON                                            LR    *
006700000000     C*----------------------------------------------------*
