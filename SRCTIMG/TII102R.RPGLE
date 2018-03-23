000100990121     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990121     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* ANU012R  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500990121     F*       - RICERCA CODIFICA CENTRI DI COSTO   -                 *
000600000000     F*       --------------------------------------                 *
000700000000     F*                                                              *
000800860521     F*         FILE : CNACO00F - ANTAB00F                           *
000900000000     F*                                                              *
001000000000     F*--------------------------------------------------------------*
001100870209     F****************************************************************
001200990121     Fanent01l  IF   E           K DISK
001300870209     F****************************************************************
001400000000     F*---------
001500990317     Ftii102D   CF   E             WORKSTN
001600000000     F                                     SFILE(ANU12SF1:REC1)
001700000000     C     *ENTRY        PLIST                                                  *
001800990121     c                   parm                    soce              3
001900990121RIC  C                   PARM                    CODCDC            8            *
002000000000     C                   PARM                    DESCDC           48            *
002100860521     C                   MOVE      *ZEROS        CODCDC                         *
002200860521     C                   MOVE      *BLANKS       DESCDC                         *
002300860521RIC  C******************************************************
002400860521RIC  C     KEYCD1        KLIST                                                  *
002500990121RIC  C                   KFLD                    soce                           *
002600990121RIC  C                   KFLD                    tipocdc           3            *
002700860521RIC  C                   KFLD                    CODCDC                         *
002800990121     c                   move      'CDC'         tipocdc
002900860521RIC  C*----------                                          *
003000860521RIC  C     KEYCDC        KLIST                                                  *
003100990121RIC  C                   KFLD                    soce                           *
003200990121RIC  C                   KFLD                    tipocdc                        *
003300990121RIC  C*
003400000000     C                   WRITE     ANU0121                                      *TESTATA
003500000000     C     SU            TAG                                                    *
003600990121     C*
003700990121RIC  C     KEYCD1        SETLL     anent01l                                     *
003800860521     C                   Z-ADD     1             REC1              3 0          *
003900860521     C                   MOVE      *BLANKS       SCELTA                         *
004000000000     C     SU1           TAG                                                    *
004100000000     C                   SETON                                            70    *
004200000000     C                   WRITE     ANU12SC1                                     *
004300000000     C                   SETOFF                                       7071      *
004400000000     C     1             DO        45            REC1                           *
004500990121     C*
004600990121RIC  C     KEYCDC        READE     anent01l                               72    *
004700000000     C  N72              DO                                                     *
004800990121RIC  C                   MOVE      ententita     CODVID                         *
004900990121RIC  C                   MOVEL     entdescr      DESVID                         *
005000000000     C                   WRITE     ANU12SF1                                     *
005100000000     C                   SETON                                        71        *
005200000000     C                   END                                                    *
005300000000     C  N72              END                                                    *
005400000000     C*---------- EMISSIONE SFL :                          *
005500000000     C     SU2           TAG                                                    *
005600000000     C                   EXFMT     ANU12SC1                                     *
005700000000     C   KG              GOTO      SU                                           *
005800990114     C   Kc              SETON                                            90    *
005900990114     C   Kc              GOTO      FINE                                         *
006000860521     C*---------- LETTURA RECORD VARIATI ------------------*
006100000000     C   71              READC     ANU12SF1                               91    *
006200000000     C  N71                                                                     *
006300000000     COR 91
006400000000     CANN72              GOTO      SU1                                          *
006500000000     C   91
006600000000     CAN 72              GOTO      SU2                                          *
006700000000     C     SCELTA        CABNE     'X'           SU2                            *
006800860521     C                   MOVE      CODVID        CODCDC                         *
006900860521     C                   MOVEL     DESVID        DESCDC                         *
007000000000     C*----------------------------------------------------*
007100000000     C     FINE          TAG                                                    *
007200990121     C   90              MOVE      '99999999'    CODCDC                         *
007300000000     C   90              MOVEL     'ERRORE'      DESCDC                         *
007400000000     C                   SETON                                            LR    *
007500000000     C*----------------------------------------------------*
