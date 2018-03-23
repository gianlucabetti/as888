000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941216     H* FNLR64R *----------------------------------------------------*
000300960215     H*         LISTA SPEDIZIONI NON PARTITE
000400000000     H*--------------------------------------------------------------*
000500920907     FTABEL00F  IF   E           K DISK
000600941216     FFNFVA01L  IF   E           K DISK
000700000503     FFNFWA01L  IF   E           K DISK
000800941216     FFNLR64D   CF   E             WORKSTN
000900020422
001000020422     d wa              s              2  0
001100020422     d wb              s              2  0
001200020422     d wc              s              2  0
001300020422
001400920907     D L1              S              3  0 DIM(30)                              FIL GESTITE
001500920907     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
001600920907     D L6S             S              3  0 DIM(30)                              FIL GEST SIMFE
001700921201     D*
001800941220     D FVA             S              9  0 DIM(70)                              FVA SELEZ
001900921201     D*LINEE   DI ARRIVO
002000921201     D                 DS
002100921201     D  FVALNA                 1      3  0
002200921201     D  FVAFFV                 4    243
002300921201     D  FVAFF2               244    450
002400000503     D  FWAFF3               451    690
002500000503     D  FWAFF4               691    900
002600000503     D  VUOTO                901    903
002700000503     D  FFV                    1    903
002800000503     D                                     DIM(301)                             LINEE FOGLIO
002900950626     D PARAM           DS                  INZ
003000950626     D  PARLIP                 1     87
003100950626     D  PARZNC                88    107
003200950626     D  PARDAT               108    115  0
003300950626     D  PARABI               116    117
003400950626     D** QUESTI I DATI CHE SERVONO
003500941219     D  VIDLNP               118    120  0
003600941220     D  PARNFV               121    125  0
003700941219     D  VIDFIL               126    128  0
003800950215     D  PAROUT               129    129
003900950626     D**
004000950626     D  PARINC               130    131
004100950626     D  PARIMA               132    133
004200960321     D* DATA FOGLIO ARRIVI, SERVE SOLO PER LA PRELISTA DI CONSEGNA
004300960321     D  PARDFA               134    141  0
004400941220     D PARA26          DS
004500941220     D* CHAIAMATA AL PGM DI RICERCA FOGLI PARTENZA ALL'ARRIVO -FNLR26R-
004600941220     D  P26PGM                 1      7
004700941220     D  P26LAI                 8     10  0
004800941220     D  P26FEL                11     13  0
004900941220     D  P26ABB                14     14
005000941220     D  P26DSD                15     22  0
005100941220     D  P26DSA                23     30  0
005200941220     D  P26FLG                31     31
005300941220     D  P26LNP                32     34  0
005400980205     D  P26LAN                35     35
005500980205     D  P26MSG                36    113
005600941220     D WLBDAT          DS
005700941220     D  G02DAT                 1      8  0
005800941220     D  G02INV                 9     16  0
005900941220     D  G02ERR                17     17
006000941220     D  G02TGI                18     22  0
006100941220     D WGIDAT          DS
006200941220     D  GIODAT                 1      8  0
006300941220     D  GIOINV                 9     16  0
006400941220     D  GIOTGI                17     21  0
006500920907     D CNCR80        E DS
006600920907     D KPJBA         E DS
006700020513     D* REM                    1      3
006800020513     D* REMFIL                 4      6
006900900523     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007000020419      * ds per trul06r
007100020419     d trul06ds      e ds
007200020419     d  lin                    1     90  0 dim(30)
007300900523     C****************************************************************
007400900523     C*  RIEPILOGO INDICATORI
007500900523     C***************************************************************
007600900523     C* 32    - DI COMODO RIUTILIZZABILI
007700900523     C* 90    - TROVATO ERRORE
007800900523     C*
007900900523     C*****************************************************************
008000000000     C     *ENTRY        PLIST
008100000000     C                   PARM                    KPJBA
008200000000     C                   Z-ADD     1             CODUT
008300900523     C                   CALL      'X§PARUT'
008400900523     C                   PARM                    UT§DSE
008500941216     C                   MOVEL     RAGUT         RSUT
008600920907     C                   MOVEL     REC80         CNCR80
008700900511     C*---------------------------------------------------------------*
008800920316     C     KFVA          KLIST
008900920316     C                   KFLD                    VIDLNP
009000941220     C                   KFLD                    PARNFV
009100000503     C     KFWA          KLIST
009200000503     C                   KFLD                    FVALNP
009300000503     C                   KFLD                    FVANFV
009400000503     C                   KFLD                    FVALAI
009500930211     C     KTAB          KLIST
009600930211     C                   KFLD                    CODUT
009700941220     C                   KFLD                    COD
009800920907     C     KTAB2         KLIST
009900920907     C                   KFLD                    CODUT
010000920907     C                   KFLD                    COD               2
010100920907     C                   KFLD                    KEY               8
010200920907     C*
010300920907     C                   EXSR      OPERAZ
010400900523     C*
010500900509     C     INIZIO        TAG
010600920316     C                   Z-ADD     0             VIDLNP
010700941220     C                   MOVEL     *ZERO         VIDNFV
010800941220     C                   Z-ADD     *ZERO         PARNFV
010900900509     C     FOR01         TAG
011000941216     C                   EXFMT     LR64D01
011100920309     C** CMD3 - FINE LAVORO
011200900509     C   KC              GOTO      FINE
011300920907     C** CMD18- CAMBIO FILIALE
011400920907     C   KS              SETON                                        04
011500920907     C   KS              GOTO      FOR01
011600920907     C*
011700920907     C   04              DO
011800930211     C     VIDFIL        IFNE      SIMFEL
011900920907     C     VIDFIL        LOOKUP    L1                                     30
012000920907     C  N30              SETON                                        50
012100920907     C  N30              GOTO      FOR01
012200920907     C*
012300920907     C* NON POSSO CAMBIARE SE E' IN MIA L6
012400920907     C     VIDFIL        LOOKUP    L6S                                    49
012500920907     C   49              GOTO      FOR01
012600920907     C                   END
012700920907     C*
012800920907     C                   EXSR      CARL6
012900920907     C                   END
013000941220     C*
013100941220     C* NR.FOGLIO VIAGGIO VALIDO
013200941220     C     '?'           SCAN      VIDNFV                                 30     ON=TROVATO
013300941220     C     *IN30         IFEQ      *ON
013400941220     C                   CLEAR                   PARA26
013500941220     C                   MOVEL     'FNLR64R '    P26PGM
013600941220     C                   MOVEL     VIDFIL        P26FEL
013700941227     C                   Z-ADD     SAVDI3        P26DSD                         ULTIMI 3 GIORNI
013800941220     C                   Z-ADD     99991231      P26DSA
013900941220     C                   MOVEL     'I'           P26FLG                         'I'=INTERROGAZIONE
014000941220     C                   Z-ADD     VIDLNP        P26LNP
014100941220     C                   MOVEL     PARA26        KPJBU
014200941220     C                   MOVEA     *ZEROS        FVA
014300941220     C*
014400941220     C                   CALL      'FNLR26R'
014500941220     C                   PARM                    KPJBA
014600941220     C                   PARM                    FVA
014700941220     C                   MOVEL     KPJBU         PARA26
014800941220     C                   MOVE      FVA(1)        VIDNFV
014900941220     C                   GOTO      FOR01
015000941220     C                   ENDIF
015100941220     C*
015200941220     C     VIDNFV        IFGT      '00000'
015300941220     C                   MOVEL     VIDNFV        PARNFV                         NR.FOGLIO VIAGGIO
015400941220     C                   ELSE
015500941220     C                   SETON                                        40
015600941220     C                   GOTO      FOR01
015700941220     C                   ENDIF
015800941220     C*
015900900509     C* CONTROLLI FORMATO1
016000941216     C     KFVA          SETLL     FNFVA01L
016100941216     C     KFVA          READE     FNFVA01L                               40
016200921201     C*
016300000503     C     *IN40         DOWEQ     *OFF
016400000503     C     KFWA          CHAIN     FNFWA01L                           31
016500000503     C*
016600000503     C     *IN31         IFEQ      *ON
016700000503     C     FWAATB        ORNE      ' '
016800000503     C                   CLEAR                   FWAFF3
016900000503     C                   CLEAR                   FWAFF4
017000000503     C                   ENDIF
017100000503     C*
017200921201     C                   MOVEL     VIDFIL        ALF3              3
017300921201     C     ALF3          LOOKUP    FFV                                    30
017400921201     C   30              SETON                                            40
017500921201     C*
017600941216     C  N40KFVA          READE     FNFVA01L                               40
017700921201     C                   END
017800921201     C*
017900921201     C   30              SETOFF                                           40
018000921201     C*
018100941216     C  NKF
018200921201     COR 40              GOTO      FOR01
018300941216     C* F6 - ELABORAZIONE
018400950215     C                   CLEAR                   PAROUT                         OUTQ DEL TERMINALE
018500900523     C                   MOVEL     PARAM         KPJBU
018600941216     C                   MOVEL     'LR52'        KCOAZ
018700900523     C                   CALL      'BCH10'
018800900523     C                   PARM                    KPJBA
018900900522     C*
019000000000     C     FINE          TAG
019100000000     C                   SETON                                        LR
019200941216     C**************************************************************************
019300941216     C* OPERAZIONI INIZIALI
019400941216     C**************************************************************************
019500920907     C     OPERAZ        BEGSR
019600920907     C*
019700920907     C* VEDO SE SONO SIMFEL O REMOTO
019800920907     C*
019900020513     C*    REM           IFEQ      'REM'
020000020513     C*    REMFIL        ANDGT     *ZEROS
020100020513     C*                  MOVEL     REMFIL        VIDFIL
020200020513     c     simtpp        ifeq      '2'
020300020513     c     simtpp        oreq      *blanks
020400020513     c                   movel     simpou        vidfil
020500920907     C                   SETON                                        05
020600930211     C                   ELSE
020700930211     C                   MOVEL     SIMFEL        VIDFIL
020800920907     C                   END
020900020422
021000930211     C                   EXSR      CARL6
021100920907     C*
021200020422     C* CARICO FILIALI GESTITE £1
021300920907     C*
021400020422     c                   clear                   trul06ds
021500020422     c                   eval      d06cod = '£1'
021600020422     c                   movel     simfel        d06key
021700020422     c                   movel     trul06ds      kpjbu
021800020422     c                   call      'TRUL06R'
021900020422     c                   parm                    kpjba
022000020422     c                   movel     kpjbu         trul06ds
022100020422     c                   movel     lin           l1
022200020422
022300020422     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE
022400020422     c                   clear                   wc
022500020422 b1  c                   do        30            wa
022600020422     c                   if        l1(wa) = *zeros
022700020422     c                   leave
022800020422     c                   endif
022900020422     c                   clear                   kpjbu
023000020422     c                   clear                   trul06ds
023100020422     c                   eval      d06cod = '£6'
023200020422     c                   movel     l1(wa)        d06key
023300020422     c                   eval      d06esc = 'S'
023400020422     c                   movel     trul06ds      kpjbu
023500020422     c                   call      'TRUL06R'
023600020422     c                   parm                    kpjba
023700020422     c                   movel     kpjbu         trul06ds
023800020422 b2  c                   do        30            wb
023900020422     c                   if        lin(wb) = *zeros
024000020422     c                   leave
024100020422     c                   endif
024200020422     c                   add       1             wc
024300020422     c                   z-add     lin(wb)       l6s(wc)
024400020422 e2  c                   enddo
024500020422 e1  c                   enddo
024600941220     C*
024700941220     C* RILEVO LA DATA DEL GIORNO E TORLGO TRE GIORNI
024800941220     C                   TIME                    WTIME            14 0          ORA E DATA
024900941220     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
025000941220     C*
025100941220     C                   Z-ADD     WDATE         G02DAT                         GG/MM/AAAA
025200941220     C                   Z-ADD     *ZERO         G02INV
025300941220     C                   MOVEL     *BLANKS       G02ERR
025400941220     C                   CALL      'XSRDA8'
025500941220     C                   PARM                    WLBDAT
025600941220     C                   SUB       3             G02TGI                         MENO 3 GIORNI
025700941220     C                   Z-ADD     *ZEROS        GIODAT
025800941220     C                   Z-ADD     *ZEROS        GIOINV
025900941220     C                   Z-ADD     G02TGI        GIOTGI
026000941220     C                   CALL      'XSRGI8'
026100941220     C                   PARM                    WGIDAT
026200941220     C                   Z-ADD     GIOINV        SAVDI3            8 0          AAAA/MM/GG
026300941216     C*
026400920907     C                   ENDSR
026500941216     C**************************************************************************
026600941216     C* CARICO L6
026700941216     C**************************************************************************
026800920907     C     CARL6         BEGSR
026900941216     C*
027000020422     c                   clear                   trul06ds
027100020422     c                   eval      d06cod = '£6'
027200020422     c                   movel     vidfil        d06key
027300020422     c                   movel     trul06ds      kpjbu
027400020422     c                   call      'TRUL06R'
027500020422     c                   parm                    kpjba
027600020422     c                   movel     kpjbu         trul06ds
027700020422     c                   movel     lin           l6
027800941216     C*
027900920907     C                   ENDSR
