000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*----------------------------------------------------------------H
000300941102     F*        TNSC90R -  LISTA CONTRASSEGNI  - FILTRO -              -
000400000000     F*                                                               -
000500000000     F*----------------------------------------------------------------
000600941102     FTNSC90D   CF   E             WORKSTN
000700000000     F/EJECT
000800000000     D*----------------------------------------------------------------
000900941102      *
001000941102     D WLBDA8          DS
001100941102     D  G08DAT                 1      8  0
001200941102     D  G08INV                 9     16  0
001300941102     D  G08ERR                17     17
001400941102     D  G08TGI                18     22  0
001500000000     D PARAM           DS
001600910902     D  VSEL1                  1      1
001700910902     D  VSEL2                  2      2
001800911003     D  VSEL3                  3      3
001900911003     D  VORD                   4      4
002000911003     D  VDET                   5      6
002100941102     D  DASPE                  7     14  0
002200941102     D  ASPE                  15     22  0
002300941102     D  DAINC                 23     30  0
002400941102     D  AINC                  31     38  0
002500941103     D  LIBERO                39     40
002600941102     D  DAPAG                 41     48  0
002700941102     D  APAG                  49     56  0
002800941102     D  VDAFIL                57     59  0
002900941102     D  VAFIL                 60     62  0
003000960909     D  VSEL4                 63     63
003100040309     D  VFILE1                65     65
003200040309     D  VSCOP1                66     66
003300040913     D  VFILi1                67     67
003400050711     D  vclida                68     74  0
003500050711     D  vclia                 75     81  0
003600050707     D tibs69        E DS                  extname(tibs69ds)
003700050707     D cnaco         E DS                  extname(cnaco00f)
003800050707     D cnind         E DS                  extname(cnind00f)
003900050707     D cnclp         E DS                  extname(cnclp00f)
004000050707     D fncls         E DS                  extname(fncls00f)
004100000000     D KPJBA         E DS
004200000000     D  LIBSYS                92    100
004300000000     D  MSGERR               453    502
004400910902     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004500000000     D  TCU                  398    697
004600000000     D                                     DIM(50)
004700000000     D  KCU                  698    847P 0
004800000000     D                                     DIM(50)
004900000000     D                                     PACKEVEN
005000000000     D  DTU                  848    895P 0
005100000000     D                                     DIM(12)
005200000000     D                                     PACKEVEN
005300941102     D                SDS
005400941102     D  §PGM                   1     10
005500050707     D vcli            s                   like(vclida)
005600000000     I/SPACE 3
005700000000     C*---------------------------------------------------------------*
005800000000     C     *ENTRY        PLIST
005900000000     C                   PARM                    KPJBA
006000000000     C                   Z-ADD     1             CODUT
006100000000     C                   CALL      'XPARUT'
006200000000     C                   PARM                    UTEDSE
006300000000     C     CODUT         CABEQ     -1            FINE
006400000000     C                   MOVEL     RAGUT         RSUT             20
006500000000     C                   MOVEL     RAGUT         RSUT2            25
006600941102     C*§§
006700941102     C                   TIME                    WHHUDT           14 0
006800941102     C                   MOVEL     WHHUDT        ORA               6 0
006900941102     C                   MOVE      WHHUDT        G08DAT
007000941102     C                   MOVE      *ZEROS        G08INV
007100941102     C                   MOVEL     *BLANK        G08ERR
007200941102     C                   CALL      'XSRDA8'
007300941102     C                   PARM                    WLBDA8
007400941102     C                   Z-ADD     G08INV        AMGUDT            8 0
007500941102     C                   Z-ADD     G08DAT        GMAUDT            8 0
007600000000     C*---------------------------------------------------------------*
007700891114     C*
007800000000     C     INIZIO        TAG
007900000000     C*
008000910902     C                   MOVEL     *BLANKS       VSEL1
008100910902     C                   MOVEL     *BLANKS       VSEL2
008200911003     C                   MOVEL     *BLANKS       VSEL3
008300960909     C                   MOVEL     *BLANKS       VSEL4
008400910902     C                   MOVEL     'A'           VORD
008500040309     C                   MOVEL     'N'           Vfile1
008600040913     C                   MOVEL     'N'           Vfili1
008700040309     C                   MOVEL     'N'           Vscop1
008800910902     C                   MOVEL     *ZEROS        VDASPE
008900910902     C                   MOVEL     *ZEROS        VASPE
009000910902     C                   MOVEL     *ZEROS        VDAINC
009100910902     C                   MOVEL     *ZEROS        VAINC
009200911003     C                   MOVEL     *ZEROS        VDAPAG
009300050707     C                   MOVEL     *ZEROS        Vclida
009400050707     C                   MOVEL     *ZEROS        Vclia
009500911003     C                   MOVEL     *ZEROS        VAPAG
009600911009     C                   MOVEL     *ZEROS        DAPAG
009700911009     C                   MOVEL     *ZEROS        APAG
009800911003     C                   MOVEL     *ZEROS        VDAFIL
009900911003     C                   MOVEL     *ZEROS        VAFIL
010000910905     C                   MOVEL     'SI'          VDET
010100000000     C*--- EMISSIONE VIDEO
010200910902     C     VIDEO         TAG
010300941102     C                   EXFMT     SC90D01
010400941102     C   KC              GOTO      FINE
010500910412     C*
010600910902     C     VSEL1         IFEQ      *BLANKS
010700910902     C     VSEL2         ANDEQ     *BLANKS
010800911003     C     VSEL3         ANDEQ     *BLANKS
010900960909     C     VSEL4         ANDEQ     *BLANKS
011000910412     C                   GOTO      INIZIO
011100910412     C                   END
011200910412     C*
011300910902     C*---- CONTRLLO LIMITI DATE SPEDIZIONI
011400910902     C     VDASPE        IFNE      0
011500941102     C                   MOVE      VDASPE        G08DAT
011600941102     C                   MOVE      *ZEROS        G08INV
011700941102     C                   MOVEL     *BLANK        G08ERR
011800941102     C                   CALL      'XSRDA8'
011900941102     C                   PARM                    WLBDA8
012000941102     C     G08ERR        IFEQ      '1'
012100000000     C                   SETON                                        82
012200000000     C                   END
012300000000     C   82              GOTO      VIDEO
012400941102     C                   Z-ADD     G08INV        DASPE
012500941102     C                   MOVE      G08DAT        VDASPE
012600000000     C                   ELSE
012700981023     C                   Z-ADD     19400101      DASPE
012800000000     C                   END
012900910902     C     VASPE         IFNE      0
013000941102     C                   Z-ADD     VASPE         G08DAT
013100941102     C                   MOVE      *ZEROS        G08INV
013200941102     C                   MOVEL     *BLANK        G08ERR
013300941102     C                   CALL      'XSRDA8'
013400941102     C                   PARM                    WLBDA8
013500941102     C     G08ERR        IFEQ      '1'
013600000000     C                   SETON                                        83
013700000000     C                   END
013800000000     C   83              GOTO      VIDEO
013900941102     C                   Z-ADD     G08INV        ASPE
014000941102     C                   MOVE      G08DAT        VASPE
014100000000     C                   ELSE
014200981023     C                   Z-ADD     99991231      ASPE
014300000000     C                   END
014400910905     C     ASPE          CABLT     DASPE         VIDEO                  81
014500040309     C*---- CONTROLLO LIMITI DATE  INCASSO
014600040309     C     VSEL2         IFEQ      *BLANK
014700040309     C     VDAINC        IFNE      *ZEROS
014800040309     C     VAINC         ORNE      *ZEROS
014900040309     C                   SETON                                        87
015000040309     C                   GOTO      VIDEO
015100040309     C                   END
015200040309     C                   END
015300910902     C     VDAINC        IFNE      0
015400941102     C                   Z-ADD     VDAINC        G08DAT
015500941102     C                   MOVE      *ZEROS        G08INV
015600941102     C                   MOVEL     *BLANK        G08ERR
015700941102     C                   CALL      'XSRDA8'
015800941102     C                   PARM                    WLBDA8
015900941102     C     G08ERR        IFEQ      '1'
016000910902     C                   SETON                                        84
016100910902     C                   END
016200910902     C   84              GOTO      VIDEO
016300941102     C                   Z-ADD     G08INV        DAINC
016400941102     C                   MOVE      G08DAT        VDAINC
016500910902     C                   ELSE
016600981130     C     VSEL2         IFEQ      'X'
016700981023     C                   Z-ADD     00010101      DAINC
016800981130     C                   ELSE
016900981130     C                   Z-ADD     0             DAINC
017000910902     C                   END
017100981130     C                   END
017200910902     C     VAINC         IFNE      0
017300941102     C                   Z-ADD     VAINC         G08DAT
017400941102     C                   MOVE      *ZEROS        G08INV
017500941102     C                   MOVEL     *BLANK        G08ERR
017600941102     C                   CALL      'XSRDA8'
017700941102     C                   PARM                    WLBDA8
017800941102     C     G08ERR        IFEQ      '1'
017900910902     C                   SETON                                        85
018000910902     C                   END
018100910902     C   85              GOTO      VIDEO
018200941102     C                   Z-ADD     G08INV        AINC
018300941102     C                   MOVE      G08DAT        VAINC
018400910902     C                   ELSE
018500981130     C     VSEL2         IFEQ      'X'
018600981023     C                   Z-ADD     99991231      AINC
018700981130     C                   ELSE
018800981130     C                   Z-ADD     0             AINC
018900910902     C                   END
019000981130     C                   END
019100910905     C     AINC          CABLT     DAINC         VIDEO                  86
019200911003     C* CONTROLLO LIMITI DATA PAGAMENTO
019300911003     C     VSEL3         IFEQ      *BLANK
019400960909     C     VSEL4         ANDEQ     *BLANK
019500911003     C     VDAPAG        IFNE      *ZEROS
019600911003     C     VAPAG         ORNE      *ZEROS
019700911003     C                   SETON                                        91
019800911003     C                   GOTO      VIDEO
019900911003     C                   END
020000911003     C                   END
020100911003     C     VDAPAG        IFNE      0
020200941102     C                   Z-ADD     VDAPAG        G08DAT
020300941102     C                   MOVE      *ZEROS        G08INV
020400941102     C                   MOVEL     *BLANK        G08ERR
020500941102     C                   CALL      'XSRDA8'
020600941102     C                   PARM                    WLBDA8
020700941102     C     G08ERR        IFEQ      '1'
020800911003     C                   SETON                                        88
020900911003     C                   END
021000911003     C   88              GOTO      VIDEO
021100941102     C                   Z-ADD     G08INV        DAPAG
021200941102     C                   MOVE      G08DAT        VDAPAG
021300911009     C                   END
021400911009     C     VSEL3         IFNE      *BLANKS
021500911009     C     VDAPAG        ANDEQ     0
021600960909     C     VSEL4         ORNE      *BLANKS
021700960909     C     VDAPAG        ANDEQ     0
021800911008     C                   SETON                                        88
021900911008     C                   GOTO      VIDEO
022000911003     C                   END
022100911003     C     VAPAG         IFNE      0
022200941102     C                   Z-ADD     VAPAG         G08DAT
022300941102     C                   MOVE      *ZEROS        G08INV
022400941102     C                   MOVEL     *BLANK        G08ERR
022500941102     C                   CALL      'XSRDA8'
022600941102     C                   PARM                    WLBDA8
022700941102     C     G08ERR        IFEQ      '1'
022800911003     C                   SETON                                        89
022900911003     C                   END
023000911003     C   89              GOTO      VIDEO
023100941102     C                   Z-ADD     G08INV        APAG
023200941102     C                   MOVE      G08DAT        VAPAG
023300911003     C                   ELSE
023400911003     C                   Z-ADD     999999        APAG
023500911003     C                   END
023600911003     C     APAG          CABLT     DAPAG         VIDEO                  90
023700911003     C* CONTROLLO FILIALI IMMESSE
023800911003     C     VDAFIL        IFNE      0
023900911003     C     VAFIL         CABLT     VDAFIL        VIDEO                  92
024000911003     C                   ELSE
024100911003     C                   Z-ADD     1             VDAFIL
024200911003     C                   Z-ADD     999           VAFIL
024300911003     C                   END
024400050707     C*---- CONTRLLO LIMITI cliente
024500050707     C     Vclida        IFNE      0
024600050707     c                   eval      vcli = vclida
024700050707     c                   exsr      srcli
024800050707     C     O69ERR        IFne      *BLANK
024900050707     C                   SETON                                        78
025000050707     C                   GOTO      VIDEO
025100050707     C                   END
025200050707     C                   END
025300050707     C     Vclia         IFNE      0
025400050707     c                   eval      vcli = vclia
025500050707     c                   exsr      srcli
025600050707     C     O69ERR        IFne      *BLANK
025700050707     C                   SETON                                        79
025800050707     C                   GOTO      VIDEO
025900050707     C                   END
026000050707     C                   END
026100050707     C     vclia         CABlT     vclida        VIDEO                  77
026200911003     C*
026300941102     C  NKF
026400941102     CANNKV              GOTO      VIDEO
026500941102     C* CMD21
026600941102     C   KV              DO
026700910902     C                   CALL      'BCH09'
026800910902     C                   PARM                    KPJBA
026900910902     C                   END
027000000000     C                   MOVEL     PARAM         KPJBU
027100941102     C                   MOVE      'SC91'        KCOAZ
027200000000     C                   CALL      'BCH10'
027300000000     C                   PARM                    KPJBA
027400000000     C     FINE          TAG
027500000000     C                   SETON                                        LR
027600050707      *---------------------------------------------------------------*
027700050707      *  srcli controllo cliente
027800050707      *---------------------------------------------------------------*
027900050707     C     srcli         BEGSR
028000050707     c                   clear                   tibs69
028100050707     C                   MOVEL     ' '           I69TLA
028200050707     C                   MOVEL     KNSIF         I69SIF
028300050707     C                   move      vcli          I69KAC
028400050707     C                   CALL      'TIBS69R'
028500050707     C                   PARM                    TIBS69
028600050707     C                   PARM                    CNACO
028700050707     C                   PARM                    CNIND
028800050707     C                   PARM                    CNCLP
028900050707     C                   PARM                    FNCLS
029000050707     c                   endsr
