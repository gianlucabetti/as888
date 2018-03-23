000100990115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990115     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* ANG945R *-------*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*             - STAMPA MODULO RIGHIESTA STAMPATI SEDE          *
000600000000     H*             - FILTRO -                                       *
000700000000     H*             -------------------------------------------      *
000800000000     H*                                                              *
000900990317     H*         FILE : tii010D  - ANTAB00F                           *
001000000000     H*--------------------------------------------------------------*
001100990317     Ftii010D   CF   E             WORKSTN
001200890629     FANTAB00F  IF   E           K DISK
001300000000     D PARAM2          DS
001400000000     D  VSTM                   1      2
001500900319     D  SCM                    3    102
001600900319     D                                     DIM(20)                              S/CLASSI MERC.
001700990115     d  socpas               103    105
001800900319     D SCMDS           DS
001900900319     D  VSCM01                 1      5
002000900319     D  VSCM02                 6     10
002100900319     D  VSCM03                11     15
002200900319     D  VSCM04                16     20
002300900319     D  VSCM05                21     25
002400900319     D  VSCM06                26     30
002500900319     D  VSCM07                31     35
002600900319     D  VSCM08                36     40
002700900319     D  VSCM09                41     45
002800900319     D  VSCM10                46     50
002900900319     D  VSCM11                51     55
003000900319     D  VSCM12                56     60
003100900319     D  VSCM13                61     65
003200900319     D  VSCM14                66     70
003300900319     D  VSCM15                71     75
003400900319     D  VSCM16                76     80
003500900319     D  VSCM17                81     85
003600900319     D  VSCM18                86     90
003700900319     D  VSCM19                91     95
003800900319     D  VSCM20                96    100
003900900319     D  SCMX                   1    100
004000900319     D                                     DIM(20)
004100000000     D*
004200000000     D PARAM           DS
004300000000     D*---- PARAMETRI VARI
004400000000     D  CODUTE                 1      1  0
004500000000     D  RAGUTE                 2     49
004600000000     D  RAG                   50     65
004700000000     D  CLIITA                66     69  0
004800000000     D  CLIEST                70     73  0
004900000000     D  FORITA                74     77  0
005000000000     D  FOREST                78     81  0
005100000000     D  AGEITA                82     85  0
005200000000     D  AGEEST                86     89  0
005300000000     D  CCNIVA                90     93  0
005400000000     D  CCNBAN                94     97  0
005500000000     D  CCNIND                98    101  0
005600000000     D KPJBA         E DS
005700990115     D soc001        E DS                  EXTNAME(xsoc001ds)
005800990115     D xsocds          DS          1000
005900000000     I*---- PASSAGGIO PARAMETRI
006000000000     C*----------------------------------------------------*
006100000000     C     *ENTRY        PLIST                                                  *
006200000000     C                   PARM                    KPJBA                          *
006300990115     C*---------- RICERCA DITTA :
006400990115     C                   MOVEL     'SOC001'      TIPXSC
006500990115     C                   MOVEL     *blank        SOCXSC
006600990115     C                   EXSR      REPSOC
006700990115     C     RTNXSC        IFNE      '1'
006800990115     C                   MOVEL     XSOCDS        SOC001
006900990115     C                   MOVEL     xscrgs        desdit
007000990115     c                   movel     xscsoc        socpas
007100990115     c                   end
007200000000RIC  C                   MOVE      *ZEROS        PARAM                          *
007300000000RIC  C                   MOVE      1             CODUTE                         *
007400000000     C******************************************************
007500900319     C*---                                    -------------*
007600900319     C     KTAB          KLIST                                                  *
007700890629     C                   KFLD                    GRU               3            *
007800890629     C                   KFLD                    COD               7            *
007900000000     C*----------------------------------------------------*
008000000000     C     STAR          TAG
008100000000     C                   MOVEL     *BLANKS       VSTM
008200900319     C                   MOVEL     *BLANKS       SCM
008300900319     C                   MOVEL     *BLANKS       SCMX
008400000000     C     SU            TAG
008500890629     C                   EXFMT     ANG9451
008600990409     C   Kc              GOTO      FINE
008700900319     C** CONTROLLO SETTORE MERCEOLOGICO
008800900319     C                   MOVEL     '160'         GRU                            *
008900890629     C                   MOVE      *ZEROS        COD
009000890629     C                   MOVE      VSTM          COD
009100900319     C     KTAB          CHAIN     ANTAB00F                           50
009200890629     C   50              GOTO      SU
009300900319     C** CONTROLLO S/CLASSE MERCEOLOGICA
009400900319     C                   DO        20            A                 2 0
009500900319     C                   Z-ADD     59            IN                2 0
009600900319     C                   ADD       A             IN
009700900319     C     SCMX(A)       IFGT      *BLANKS
009800900319     C                   MOVEL     '156'         GRU
009900900319     C                   MOVE      *ZEROS        COD
010000900319     C                   MOVE      SCMX(A)       COD
010100900319     C     KTAB          CHAIN     ANTAB00F                           96
010200900319     C   96              DO
010300900319     C                   MOVE      '1'           *IN(IN)
010400900319     C                   GOTO      SU
010500900319     C                   END
010600900319     C                   END
010700900319     C                   END
010800000000     C*CONTROLLI SUPERATI
010900000000     C                   SETON                                        98
011000000000     C   98
011100000000     CANNKA              GOTO      SU
011200990317     C                   MOVEL     'I011'        KCOAZ
011300900319     C                   MOVEL     SCMX          SCM
011400000000     C                   MOVEL     PARAM2        KPJBU
011500000000     C                   CALL      'BCH10'
011600000000     C                   PARM                    KPJBA
011700000000     C     FINE          TAG
011800000000     C                   SETON                                        LR
011900990115     C*----------------------------------------------------*
012000990115     C* Reperimento dati società
012100990115     C*----------------------------------------------------*
012200990115     C     REPSOC        BEGSR
012300990115     C*
012400990115     C                   CALLB     'XSOC'
012500990115     C                   PARM                    TIPXSC            6
012600990115     C                   PARM                    SOCXSC            3
012700990115     C                   PARM                    CDSXSC            9 0
012800990115     C                   PARM                    MODXSC            3
012900990115     C                   PARM      *blanks       RTNXSC            1
013000990115     C                   PARM                    XSOCDS
013100990115     C                   PARM                    KPJBA
013200990115     C*
013300990115     C                   ENDSR
