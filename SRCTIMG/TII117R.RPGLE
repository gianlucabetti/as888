000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* ANU162R  *-AN/38-*-------------------------------------------*
000300000000     F*------------------*                                           *
000400000000     F*       - RICERCA CODIFICA TIPOLOGIE ORDINI            -       *
000500000000     F*       ------------------------------------------------       *
000600000000     F*         FILE : ANTAB00F                                      *
000700000000     F*                                                              *
000800000000     F*--------------------------------------------------------------*
000900000000     FANTAB00F  IF   E           K DISK
001000000000     F*---------
001100990317     FTII117D   CF   E             WORKSTN
001200000000     F                                     SFILE(ANU62SF1:REC1)
001300000000      /SPACE 3
001400000000     D DSORD         E DS                  EXTNAME(ANTAB162)
001500000000     I*--- TABELLA TIPOLOGIE ORDINI
001600000000      /SPACE 3
001700000000     C*----------------------------------------------------*
001800000000     C     *ENTRY        PLIST                                                  *
001900000000     C                   PARM                    COTIOR            2            *
002000000000     C                   PARM                    DETIOR           25            *
002100000000     C*----------                                          *
002200000000     C     KEYTAB        KLIST                                                  *
002300000000     C                   KFLD                    TABGRU                         *
002400000000     C                   KFLD                    TABCOD                         *
002500000000     C                   WRITE     ANU1621                                      *
002600000000     C*----------------------------------------------------*
002700000000      /SPACE 2
002800000000     C*----------------------------------------------------*
002900000000     C*---------- GESTIONE SUBFILE ------------------------*
003000000000     C*----------------------------------------------------*
003100000000     C                   MOVE      '162'         TABGRU                         *
003200000000     C                   MOVE      *ZEROS        TABCOD                         *
003300000000     C     TABGRU        SETLL     ANTAB00F                           90  91    *
003400000000     C  N91              SETON                                        90        *
003500000000     C   90              GOTO      FINE                                         *
003600000000     C                   Z-ADD     0             REC1              3 0          *
003700000000     C                   MOVE      *BLANKS       SCELTA                         *
003800000000     C*---------- PULIZIA SLFCT :                          *
003900000000     C                   SETON                                            70    *
004000000000     C                   WRITE     ANU62SC1                                     *
004100000000     C                   SETOFF                                       7071      *
004200000000     C*---------- CARICAMENTO SFL :                        *
004300000000     C     SOPRA         TAG                                                    *
004400000000     C     TABGRU        READE     ANTAB000                               91    *
004500000000     C   91              GOTO      EMISF1                                       *
004600000000     C                   MOVE      TABCOD        CODTIP                         *
004700000000     C                   MOVEL     TABDES        DSORD                          *
004800000000     C     REC1          ADD       1             REC1                 71        *
004900000000     C                   WRITE     ANU62SF1                                     *
005000000000     C                   GOTO      SOPRA                                        *
005100000000     C*---------- EMISSIONE SFL :                          *
005200000000     C     EMISF1        TAG                                                    *
005300000000     C                   EXFMT     ANU62SC1                                     *
005400990114     C   Kc              SETON                                            90    *
005500990114     C   Kc              GOTO      FINE                                         *
005600000000     C*---------- LETTURA RECORD VARIATI :                 *
005700000000     C   71              READC     ANU62SF1                               91    *
005800000000     C  N71                                                                     *
005900000000     COR 91              GOTO      EMISF1                                       *
006000000000     C     SCELTA        CABNE     'X'           EMISF1                         *
006100000000     C                   MOVE      CODTIP        COTIOR                         *
006200000000     C                   MOVE      DES162        DETIOR                         *
006300000000     C*----------------------------------------------------*
006400000000      /SPACE 2
006500000000     C*----------------------------------------------------*
006600000000     C*     USCITA PROGRAMMA                               *
006700000000     C*----------------------------------------------------*
006800000000     C     FINE          TAG                                                    *
006900000000     C   90              MOVEL     '99'          COTIOR                         *
007000000000     C   90              MOVEL     'ERRORE'      DETIOR                         *
007100000000     C                   SETON                                            LR    *
007200000000     C*----------------------------------------------------*
