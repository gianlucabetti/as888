000100990316     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* ANG947R *-------*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*        - STAMPA MODULO CEDE RICHIESTA STAMPATI SEDE-         *
000600000000     H*          BATCH                                               *
000700000000     H*                                                              *
000800000000     H*--------------------------------------------------------------*
000900990115     FtmAMP04L  IF   E           K DISK
001000890928     FLETSTD2   O    F  132        PRINTER
001100000000     D*--------------------------------------------------------------*
001200000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 1
001300000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 2
001400000000     D*--------------------------------------------------------------*
001500000000     D PARAM2          DS
001600000000     D  VSTM                   1      2
001700000000     D NUMRIC          DS
001800000000     D*--- DATA AREA NUMERAZIONE RICHIESTA
001900000000     D  NUM                    1      6  0
002000000000     D*
002100000000     D KPJBA         E DS
002200000000     I*---- PASSAGGIO PARAMETRI
002300000000     C*----------------------------------------------------*
002400000000     C     *ENTRY        PLIST                                                  *
002500000000     C                   PARM                    KPJBA                          *
002600000000     C******************************************************
002700000000     C                   MOVEL     KPJBU         PARAM2
002800000000     C                   Z-ADD     0             NUM
002900000000     C                   Z-ADD     60            CONRIG            3 0
003000000000     C     *DTAARA       DEFINE                  NUMRIC                         *
003100000000     C                   MOVEL     KNSIF         SYS               3 0
003200000000     C** N. RICHIESTA MODULO
003300000000     C     *LOCK         IN        NUMRIC                               46
003400000000     C                   ADD       1             NUM
003500000000     C                   OUT       NUMRIC
003600000000     C                   Z-ADD     0             PAGE
003700000000     C                   EXCEPT    TESTA
003800000000     C                   Z-ADD     11            CONRIG
003900000000     C                   EXSR      ELAMAT
004000000000     C     FINE          TAG
004100000000     C                   EXCEPT    FONDO
004200000000     C                   SETON                                            LR
004300000000     C******************************************************
004400000000     C     ELAMAT        BEGSR
004500000000     C**----------------------------------
004600000000     C** ELABORAZIONE MATERIALI
004700000000     C**----------------------------------
004800000000     C     SU            TAG
004900990115     C                   READ      tmAMP000                               13
005000000000     C   13              GOTO      ENDELA
005100000000     C** SOLO MATERIALI DELLE CLASSI 1 E 2
005200000000     C     AMPCLM        IFNE      '1'
005300000000     C     AMPCLM        IFNE      '2'
005400000000     C                   GOTO      SU
005500000000     C                   END
005600000000     C                   END
005700000000     C** ESCLUSIONE DEGLI ARTICOLI CHE HANNO S/CLASSE = 8
005800000000     C     AMPSCM        CABEQ     '8'           SU
005900000000     C     CONRIG        IFGT      60
006000000000     C                   EXCEPT    TESTA1
006100000000     C                   Z-ADD     3             CONRIG
006200000000     C                   END
006300000000     C                   EXCEPT    DETAIL
006400000000     C                   ADD       1             CONRIG
006500000000     C                   GOTO      SU
006600000000     C     ENDELA        ENDSR
006700890928     OLETSTD2   E            TESTA            01
006800000000     O                                           66 'PAG.'
006900000000     O                       PAGE          Z     71
007000000000     O          E            TESTA            02
007100990115     O*                      DITRSC              37
007200000000     O          E            TESTA            03
007300000000     O                                           46 'SPETT.LE'
007400000000     O          E            TESTA            04
007500000000     O                                           53 'SEDE DI BOLOGNA'
007600000000     O          E            TESTA            05
007700000000     O                                            8 'C.E.D.'
007800000000     O                                           51 'UFF. ACQUISTI'
007900000000     O          E            TESTA            06
008000000000     O                                           53 'ALL''ATT.NE SIG.'
008100000000     O                                           69 'CASCARDO LORENZO'
008200000000     O          E            TESTA            07
008300000000     O                                            4 'LI'
008400000000     O                                           18 '.............'
008500000000     O          E            TESTA            09
008600000000     O                                           25 'Richieste Stampati Mese'
008700000000     O                                           28 'di'
008800000000     O                                           49 '....................'
008900000000     O                                           53 'N.'
009000000000     O                       NUM           Z     62
009100000000     O          E            TESTA          1 10
009200000000     O                                           10 'ARTICOLO'
009300000000     O                                           33 'DESCRIZIONE'
009400000000     O                                           70 'QUANTITA'''
009500000000     O          E            TESTA1            1
009600000000     O                                           66 'PAG.'
009700000000     O                       PAGE          Z     71
009800000000     O          E            TESTA1      1  1
009900000000     O                                           10 'ARTICOLO'
010000000000     O                                           33 'DESCRIZIONE'
010100000000     O                                           70 'QUANTITA'''
010200000000     O          E            DETAIL      1
010300000000     O                       AMPMAT              14
010400000000     O                       AMPDMT              52
010500000000     O                                           70 '..........'
010600000000     O          E            FONDO       2
010700000000     O                       SK1                132
010800000000     O          E            FONDO       2
010900000000     O                       SK2                132
011000000000     O          E            FONDO       2
011100000000     O                                           45 'C.E.D.'
011200000000**   SK1
011300000000  NOTE ...........................................................SK01
011400000000..................                                                SK01
011500000000**   SK2
011600000000       ...........................................................SK02
011700000000.................                                                 SK02
