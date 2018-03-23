000001990115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000002990115     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H* ANG946R *-------*--------------------------------------------*
000300000000     H*-----------------*                                            *
000400890711     H*        - STAMPA MODULO FILIALI RICHIESTA STAMPATI SEDE -     *
000500890703     H*          BATCH                                               *
000600000000     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800990115     FtmCLP06L  IF   E           K DISK
000900990115     FtmAMP04L  IF   E           K DISK
001000890919     FLETSTD2   O    F  132        PRINTER
001100890704     D*--------------------------------------------------------------*
001200890704     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 1
001300890704     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 2
001400890704     D*--------------------------------------------------------------*
001500000000     D PARAM2          DS
001600000000     D  VSTM                   1      2
001700900320     D  SCM                    3    102
001800900320     D                                     DIM(20)                              S/CLASSI MERC.
001801990115     D  Socpas               103    105
001900000000     D ARTDS           DS
002000890705     D  ART                    2      4
002100000000     D CLIDS           DS
002200990115     D  CLI                    6      8
002300890704     D NUMRIC          DS
002400890704     D*--- DATA AREA NUMERAZIONE RICHIESTA
002500890704     D  NUM                    1      6  0
002600000000     D*
004100000000     D KPJBA         E DS
004200000000     I*---- PASSAGGIO PARAMETRI
004300000000     C*----------------------------------------------------*
004400000000     C     *ENTRY        PLIST                                                  *
004500000000     C                   PARM                    KPJBA                          *
005700900320     C                   MOVEL     *BLANKS       PARAM2
005800000000     C                   MOVEL     KPJBU         PARAM2
005900900320     C                   MOVEA     SCM           SCMCOM          100
006000890705     C                   Z-ADD     0             NUM
006100900404     C                   MOVEL     *BLANKS       SCMPRE
006200890705     C                   Z-ADD     60            CONRIG            3 0
006300890705     C     *DTAARA       DEFINE                  NUMRIC                         *
006400990115     C     VSTM          SETLL     tmCLP000                               11
006500000000     C  N11              GOTO      FINE
006600000000     C     STAR          TAG
006700000000     C** LETTURA X SETTORE MERCEOLOGICO DIGITATO A VIDEO
006800990115     C     VSTM          READE     tmCLP000                               12
006900000000     C   12              GOTO      FINE
007000000000     C** ELABORAZIONE DEI CLIENTI CON GRUPPPO MERCEOLOGICO = 8
007100000000     C     CLPGPM        CABNE     '8'           STAR
007200890704     C** ROTTURA CLIENTE
007300890704     C**
007400890704     C     CLPKSC        IFNE      KSCPRE
007500890705     C   99              DO
007600890705     C     RIG           IFGT      0
007700890705     C                   EXCEPT    FONDO
007800890705     C                   END
007900890705     C                   END
008000890704     C                   SETON                                          99
008100890704     C                   SETOFF                                         7071
008200890705     C** N. RICHIESTA MODULO
008300890705     C     *LOCK         IN        NUMRIC                               46
008400890704     C                   ADD       1             NUM
008500890704     C                   OUT       NUMRIC
008700890705     C                   MOVEL     CLPRSI        RAGSOC           30
009200990115     C                   MOVE      201           SYS               3 0
009400890704     C     CLPSTM        COMP      '52'                                   70
009500890704     C     CLPSTM        COMP      '53'                                   71
009600890705     C                   Z-ADD     0             RIG               4 0
009700890706     C                   Z-ADD     0             PAGE
009800990115     C                   MOVEL     CLPKSC        KSCPRE            8
009900890706     C                   EXCEPT    TESTA
010000890706     C                   Z-ADD     11            CONRIG
010100890704     C                   END
010200900320     C     SCMCOM        IFEQ      *BLANKS
010300900320     C                   EXSR      LETAR
010400900320     C                   ELSE
010500900320     C                   EXSR      LETARP
010600900320     C                   END
010700890704     C                   GOTO      STAR
010800890704     C     FINE          TAG
010900890704     C                   EXCEPT    FONDO
011000890704     C                   SETON                                            LR
011100000000     C******************************************************
011200900320     C     LETAR         BEGSR
011300000000     C**----------------------------------
011400900320     C** LETTURA ARTICOLI
011500000000     C**----------------------------------
011600900320     C                   SETOFF                                           13
011700900320     C                   MOVEL     *BLANKS       KSCM
011800990115     C     KSCM          SETLL     tmAMP04L
011900000000     C     SU            TAG
012000990115     C                   READ      tmAMP04L                               13
012100900320     C   13              GOTO      END1
012200900404     C     AMPSCM        IFNE      SCMPRE
012300900404     C     CONRIG        IFGT      58
012400900404     C                   EXCEPT    TESTA1
012500900404     C                   Z-ADD     3             CONRIG
012600900404     C                   END
012700900404     C                   EXCEPT    SALTO
012800900404     C                   ADD       2             CONRIG
012900900404     C                   MOVEL     AMPSCM        SCMPRE            5
013000900404     C                   END
013100900320     C                   EXSR      CONTR
013200900320     C                   GOTO      SU
013300900320     C     END1          ENDSR
013400900320     C******************************************************
013500900320     C     LETARP        BEGSR
013600900320     C**----------------------------------
013700900320     C** LETTURA ARTICOLI X S/CLASSI
013800900320     C** IMPOSTATE A VIDEO
013900900320     C**----------------------------------
014000900320     C                   DO        20            A                 2 0
014100900320     C     SCM(A)        IFNE      *BLANKS
014200900320     C                   MOVEL     SCM(A)        KSCM              5
014300990115     C     KSCM          SETLL     tmAMP04L
014400900320     C     SET           TAG
014500990115     C     KSCM          READE     tmAMP04L                               13
014600900320     C   13              GOTO      END2
014700900404     C     AMPSCM        IFNE      SCMPRE
014800900404     C     CONRIG        IFGT      58
014900900404     C                   EXCEPT    TESTA1
015000900404     C                   Z-ADD     3             CONRIG
015100900404     C                   END
015200900404     C                   EXCEPT    SALTO
015300900404     C                   ADD       2             CONRIG
015400900404     C                   MOVEL     AMPSCM        SCMPRE            5
015500900404     C                   END
015600900320     C                   EXSR      CONTR
015700900320     C                   GOTO      SET
015800900320     C                   END
015900900320     C     END2          TAG
016000900320     C                   END
016100900320     C                   ENDSR
016200900320     C******************************************************
016300900320     C     CONTR         BEGSR
016400900320     C**----------------------------------
016500900320     C** CONTROLLI
016600900320     C**----------------------------------
016700900510     C** SOLO MATERIALI DELLE CLASSI 1 E 2 E 4
016800900510     C     AMPCLM        IFNE      '1'
016900890705     C     AMPCLM        IFNE      '2'
017000900510     C     AMPCLM        IFNE      '4'
017100900320     C                   GOTO      ENDCON
017200000000     C                   END
017300890705     C                   END
017400900510     C                   END
017500890907     C** SE LA S/CLASSE è = 8 STAMPARE SOLO L'ARTICOLO
017600000000     C** SE IL CODICE DALLA POSIZIONE 3 ALLA 4 E UGUALE
017700000000     **  AL CODICE CLIENTE DALLA POSIZIONE 5 ALLA 7
017800890704     C     AMPSCM        IFEQ      '8'
017900000000     C                   MOVEL     AMPMAT        ARTDS
018000890704     C                   MOVEL     CLPKSC        CLIDS
018100890703     C     ART           IFNE      CLI
018200900320     C                   GOTO      ENDCON
018300890703     C                   END
018400890704     C     CLPSTM        IFEQ      '54'
018500900320     C                   GOTO      ENDCON
018700000000     C                   END
018800890704     C     AMPCTF        IFNE      '1'
019000900320     C                   GOTO      ENDCON
019100890704     C                   END
019200890705     C                   END
019300900320     C                   EXSR      STAMPA
019400900320     C                   GOTO      ENDCON
019500900320     C     ENDCON        ENDSR
019600900320     C******************************************************
019700900320     C     STAMPA        BEGSR
019800900320     C**------------
019900900404     C     CONRIG        IFGT      58
020000890706     C                   EXCEPT    TESTA1
020100890706     C                   Z-ADD     3             CONRIG
020200890704     C                   END
020300890704     C                   EXCEPT    DETAIL
020400890704     C                   ADD       1             CONRIG
020500890705     C                   ADD       1             RIG
020600900320     C                   ENDSR
020700900320     C******************************************************
020800890919     OLETSTD2   E            TESTA            01
020900900430     O               70                          50 '*** MODULO B ***'
021000900430     O               71                          50 '*** MODULO A ***'
021100890706     O                                           66 'PAG.'
021200890706     O                       PAGE          Z     71
021300900430     O          E            TESTA       0  0
021400900430     O               70                          50 '*** MODULO B ***'
021500900430     O               71                          50 '*** MODULO A ***'
021600890706     O          E            TESTA            02
021700990115     O*                      DITRSC              37
021800890704     O          E            TESTA            03
021900890706     O                                           46 'SPETT.LE'
022000890704     O          E            TESTA            04
022100890706     O                                           53 'SEDE DI BOLOGNA'
022200890704     O          E            TESTA            05
022300890704     O                                           13 'Cod.Cliente'
022400890704     O                       CLPKSC              23
022500890706     O                                           51 'UFF. ACQUISTI'
022600890704     O          E            TESTA            06
022700890706     O                       RAGSOC              32
022800890706     O                                           53 'ALL''ATT.NE SIG.'
022900900320     O                                           69 ' LOLLI GIOVANNI '
023000890706     O          E            TESTA            07
023100890706     O                                            4 'LI'
023200890706     O                                           18 '.............'
023300890706     O          E            TESTA            09
023400890704     O                                           25 'Richieste Stampati Mese'
023500890704     O                                           28 'di'
023600890706     O                                           49 '....................'
023700890704     O                                           53 'N.'
023800890705     O                       NUM           Z     62
023900890706     O          E            TESTA          1 10
024000890704     O                                           10 'ARTICOLO'
024100890704     O                                           33 'DESCRIZIONE'
024200890705     O                                           70 'QUANTITA'''
024300890706     O          E            TESTA1            1
024400890706     O                                           66 'PAG.'
024500890706     O                       PAGE          Z     71
024600890706     O          E            TESTA1      1  1
024700890706     O                                           10 'ARTICOLO'
024800890706     O                                           33 'DESCRIZIONE'
024900890706     O                                           70 'QUANTITA'''
025000890704     O          E            DETAIL      1
025100890704     O                       AMPMAT              14
025200890704     O                       AMPDMT              52
025300890706     O                                           70 '..........'
025400890704     O          E            FONDO       2
025500890704     O                       SK1                132
025600890704     O          E            FONDO       2
025700890704     O                       SK2                132
025800890706     O          E            FONDO       2
025900900430     O                                           62 'FIRMA DEL RESPONSABILE'
026000900404     O          E            SALTO       2
026100890706**   SK1
026200890706  NOTE ...........................................................SK01
026300890706..................                                                SK01
026400890704**   SK2
026500890706       ...........................................................SK02
026600890706.................                                                 SK02
