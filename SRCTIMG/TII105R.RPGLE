000100000000     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F* ANU016R  *-AN/38-*-------------------------------------------*
000300000000     F*------------------*                                           *
000400000000     F*       - RICERCA CODIFICA PAGAMENTI                 -         *
000500000000     F*       ----------------------------------------------         *
000600860521     F*         FILE : TABEL00F - BFTAB01L                           *
000700000000     F*                                                              *
000800000000     F*--------------------------------------------------------------*
000900860521     F****************************************************************
001000860521RIC  FTABEL00F  IF   E           K DISK
001100860521ADB  F*FTAB01LIF  E           K        DISK
001200860521     F****************************************************************
001300000000     F*---------
001400990317     Ftii105D   CF   E             WORKSTN
001500000000     F                                     SFILE(ANU16SF1:REC1)
001600000000     C     *ENTRY        PLIST                                                  *
001700000000     C                   PARM                    PAGCOD            3            *
001800000000     C                   PARM                    PAGDES           30            *
001900000000     C*----------                                          *
002000860521     C     KEYTAB        KLIST                                                  *
002100860521     C******************************************************
002200860521RIC  C                   KFLD                    TBLKUT                         *
002300860521RIC  C                   KFLD                    TBLCOD                         *
002400860521ADB  C******               KFLD           XCDTB            *
002500860521     C******************************************************
002600860521     C/EJECT
002700000000     C*---------- GESTIONE SUBFILE ------------------------*
002800860521     C                   WRITE     ANU0161                                      *
002900860521     C******************************************************
003000860521RIC  C                   Z-ADD     1             TBLKUT                         *
003100860521RIC  C                   MOVE      'FA'          TBLCOD                         *
003200860521RIC  C     KEYTAB        SETLL     TABEL00F                           90  91    *
003300860521ADB  C******               MOVE 'PAG'     XCDTB            *
003400860521ADB  C******     KEYTAB    SETLLBFTAB01L             90  91*
003500860521     C******************************************************
003600000000     C  N91              SETON                                        90        *
003700000000     C   90              GOTO      FINE                                         *
003800000000     C                   Z-ADD     0             REC1              3 0          *
003900000000     C                   MOVE      *BLANKS       SCELTA                         *
004000000000     C*---------- PULIZIA SLFCT :                          *
004100000000     C                   SETON                                            70    *
004200000000     C                   WRITE     ANU16SC1                                     *
004300000000     C                   SETOFF                                       7071      *
004400000000     C*---------- CARICAMENTO SFL :                        *
004500000000     C     SOPRA         TAG                                                    *
004600860521     C******************************************************
004700860521RIC  C     KEYTAB        READE     TABEL                                  91    *
004800860521ADB  C******     KEYTAB    READEBFTAB                    91*
004900860521     C******************************************************
005000000000     C   91              GOTO      EMISF1                                       *
005100860521     C******************************************************
005200860521RIC  C                   MOVEL     TBLKEY        CODPAG                         *
005300860521RIC  C                   MOVEL     TBLUNI        DESPAG                         *
005400860521ADB  C******               MOVE XCDEL     CODPAG           *
005500860521ADB  C******               MOVELXDTAB     DESPAG           *
005600860521     C******************************************************
005700000000     C     REC1          ADD       1             REC1                 71        *
005800000000     C                   WRITE     ANU16SF1                                     *
005900000000     C                   GOTO      SOPRA                                        *
006000000000     C*---------- EMISSIONE SFL :                          *
006100000000     C     EMISF1        TAG                                                    *
006200000000     C                   EXFMT     ANU16SC1                                     *
006300990114     C   Kc              SETON                                            90    *
006400990114     C   Kc              GOTO      FINE                                         *
006500000000     C*---------- LETTURA RECORD VARIATI :                 *
006600000000     C   71              READC     ANU16SF1                               91    *
006700000000     C  N71                                                                     *
006800000000     COR 91              GOTO      EMISF1                                       *
006900000000     C     SCELTA        CABNE     'X'           EMISF1                         *
007000000000     C                   MOVE      CODPAG        PAGCOD                         *
007100000000     C                   MOVE      DESPAG        PAGDES                         *
007200000000     C*----------------------------------------------------*
007300000000     C     FINE          TAG                                                    *
007400000000     C   90              MOVE      '999'         PAGCOD                         *
007500000000     C   90              MOVEL     'ERRORE'      PAGDES                         *
007600000000     C                   SETON                                            LR    *
007700000000     C*----------------------------------------------------*
