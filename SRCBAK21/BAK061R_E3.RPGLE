000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES) CVTOPT(*NONE)
000200000000     H DECEDIT('0,') DATEDIT(*YMD.)
000300000000      *------------------------------------------------------------------*
000400000000      *  BAK061R -  PULISCE AZIONI NON PIÙ ESISTENTI                     *
000500000000      *------------------------------------------------------------------*
000600000000      *--- File Input
000700000000X0197X*****KPLOG02L  UF   E           K DISK
000800000000X0197FKPLOG04L  UF   E           K DISK
000900000000X0210 *---
001000000000  "  D KPJBA         E DS
001100000000X0210 *---
001200000000     D* PARAMETRI DI I/O di BAK036R
001300000000X0195D BAK036DS      E DS                  INZ
001400000000  "  X***** DS036           DS
001500000000  "  X*****  JNAM36                 1     10
001600000000  "  X*****  UNAM36                11     20
001700000000  "  X*****  JNBR36                21     26
001800000000  "  X*****  JOBQ36                27     36
001900000000  "  X*****  JOBP36                37     37
002000000000X0195X*****  JSTS36                38     47
002100000000      *--- DS Data
002200000000     D                 DS
002300000000     D  DATA                   1      6  0
002400000000     D  AA                     1      2  0
002500000000     D  MM                     3      4  0
002600000000     D  GG                     5      6  0
002700000000      *------------------------------------------------------------------*
002800000000     C*
002900000000     C* LEGGE LE AZIONI ATTIVE E IN CODA (SU KPLOG)
003000000000     C*
003100000000     C                   TIME                    ORA               6 0
003200000000     C                   Z-ADD     UDATE         DATA
003300000000     C                   Z-ADD     0             LOGDAF
003400000000X0197X*****LOGDAF        SETLL     KPLOG02L
003500000000  "  X*****LOGDAF        READE     KPLOG02L                               21
003600000000  "  C     LOGDAF        SETLL     KPLOG04L
003700000000X0197C     LOGDAF        READE     KPLOG04L                               21
003800000000     C*
003900000000     C     *IN21         DOWEQ     '0'
004000000000     C     LOGATT        IFNE      'A'
004100000000X0210 * KPJBA azione
004200000000X0210C                   MoveL     LOGKPJ        KPJBA
004300000000     C* REPERIMENTO INFORMAZIONI JOB
004400000000X0195X*****              CLEAR                   DS036
004500000000  "  X*****              MOVEL     LOGNME        JNAM36
004600000000  "  X*****              MOVEL     '*ALL'        UNAM36
004700000000  "  X*****              MOVEL     '*ALL'        JNBR36
004800000000  "  C                   CLEAR                   BAK036DS
004900000000X0195C                   MOVEL     LOGNME        JOBNAM036
005000000000X0210X***** ex-X0195     MOVEL     '*ALL'        USRNAM036
005100000000  "  X***** ex-X0195     MOVEL     '*ALL'        JOBNBR036
005200000000  "  C                   MOVEL     KNMUS         USRNAM036
005300000000X0210C                   MOVEL     KNRJO         JOBNBR036
005400000000     C                   CALL      'BAK036R'                            99
005500000000X0195X*****              PARM                    DS036
005600000000X0195C                   PARM                    BAK036DS
005700000000     C* SE JOB NON TROVATO --> AZIONE TERMINATA IN ERRORE
005800000000     C     *IN99         IFEQ      '1'
005900000000X0195X*****JNBR36        OREQ      '*ALL  '
006000000000X0210X***** ex-X0195 USRNAM036     OREQ      '*ALL  '
006100000000  "  X***** ex-X0195 JOBNBR036     OREQ      '*ALL  '
006200000000  "  C     ERR036        OREQ      '1'
006300000000X0210C     JOBSTS036     OREQ      '*OUTQ'
006400000000     C                   Z-ADD     ORA           LOGORF
006500000000     C                   Z-ADD     DATA          LOGDAF
006600000000      * SE ANNO < 40 SIAMO NEL 2000 !!!
006700000000     C     AA            IFLT      40
006800000000     C                   MOVEL     1             LOGDAF
006900000000     C                   ENDIF
0070000000001    C                   MOVE      'E'           LOGATT
007100000000     C                   UPDATE    KPLOG000
007200000000     C                   ENDIF
007300000000     C                   ENDIF
007400000000     C*
007500000000     C                   Z-ADD     0             LOGDAF
007600000000X0197X*****LOGDAF        READE     KPLOG02L                               21
007700000000X0197C     LOGDAF        READE     KPLOG04L                               21
007800000000     C                   ENDDO
007900000000     C*
008000000000     C                   SETON                                            LR
