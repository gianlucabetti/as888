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
003100010105     D  VPARZ                 64     64
003101040309     D  VFILE1                65     65
003102040309     D  VSCOP1                66     66
003103040913     D  VFILi1                67     67
003105050711     D  vclida                68     74  0
003106050711     D  vclia                 75     81  0
003107050707     D tibs69        E DS                  extname(tibs69ds)
003108050707     D cnaco         E DS                  extname(cnaco00f)
003109050707     D cnind         E DS                  extname(cnind00f)
003110050707     D cnclp         E DS                  extname(cnclp00f)
003111050707     D fncls         E DS                  extname(fncls00f)
003200000000     D KPJBA         E DS
003300000000     D  LIBSYS                92    100
003400000000     D  MSGERR               453    502
003500910902     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003600000000     D  TCU                  398    697
003700000000     D                                     DIM(50)
003800000000     D  KCU                  698    847P 0
003900000000     D                                     DIM(50)
004000000000     D                                     PACKEVEN
004100000000     D  DTU                  848    895P 0
004200000000     D                                     DIM(12)
004300000000     D                                     PACKEVEN
004400941102     D                SDS
004500941102     D  §PGM                   1     10
004501050707     D vcli            s                   like(vclida)
004600000000     I/SPACE 3
004700000000     C*---------------------------------------------------------------*
004800000000     C     *ENTRY        PLIST
004900000000     C                   PARM                    KPJBA
005000000000     C                   Z-ADD     1             CODUT
005100000000     C                   CALL      'XPARUT'
005200000000     C                   PARM                    UTEDSE
005300000000     C     CODUT         CABEQ     -1            FINE
005400000000     C                   MOVEL     RAGUT         RSUT             20
005500000000     C                   MOVEL     RAGUT         RSUT2            25
005600941102     C*§§
005700941102     C                   TIME                    WHHUDT           14 0
005800941102     C                   MOVEL     WHHUDT        ORA               6 0
005900941102     C                   MOVE      WHHUDT        G08DAT
006000941102     C                   MOVE      *ZEROS        G08INV
006100941102     C                   MOVEL     *BLANK        G08ERR
006200941102     C                   CALL      'XSRDA8'
006300941102     C                   PARM                    WLBDA8
006400941102     C                   Z-ADD     G08INV        AMGUDT            8 0
006500941102     C                   Z-ADD     G08DAT        GMAUDT            8 0
006600000000     C*---------------------------------------------------------------*
006700891114     C*
006800000000     C     INIZIO        TAG
006900000000     C*
007000910902     C                   MOVEL     *BLANKS       VSEL1
007100910902     C                   MOVEL     *BLANKS       VSEL2
007200911003     C                   MOVEL     *BLANKS       VSEL3
007300960909     C                   MOVEL     *BLANKS       VSEL4
007400910902     C                   MOVEL     'A'           VORD
007401040309     C                   MOVEL     'N'           Vfile1
007402040913     C                   MOVEL     'N'           Vfili1
007403040309     C                   MOVEL     'N'           Vscop1
007500910902     C                   MOVEL     *ZEROS        VDASPE
007600910902     C                   MOVEL     *ZEROS        VASPE
007700910902     C                   MOVEL     *ZEROS        VDAINC
007800910902     C                   MOVEL     *ZEROS        VAINC
007900911003     C                   MOVEL     *ZEROS        VDAPAG
007902050707     C                   MOVEL     *ZEROS        Vclida
007903050707     C                   MOVEL     *ZEROS        Vclia
008000911003     C                   MOVEL     *ZEROS        VAPAG
008100911009     C                   MOVEL     *ZEROS        DAPAG
008200911009     C                   MOVEL     *ZEROS        APAG
008300911003     C                   MOVEL     *ZEROS        VDAFIL
008400911003     C                   MOVEL     *ZEROS        VAFIL
008500910905     C                   MOVEL     'SI'          VDET
008600000000     C*--- EMISSIONE VIDEO
008700910902     C     VIDEO         TAG
008800941102     C                   EXFMT     SC90D01
008900941102     C   KC              GOTO      FINE
009000910412     C*
009100910902     C     VSEL1         IFEQ      *BLANKS
009200910902     C     VSEL2         ANDEQ     *BLANKS
009300911003     C     VSEL3         ANDEQ     *BLANKS
009400960909     C     VSEL4         ANDEQ     *BLANKS
009500910412     C                   GOTO      INIZIO
009600910412     C                   END
009700910412     C*
009800910902     C*---- CONTRLLO LIMITI DATE SPEDIZIONI
009900910902     C     VDASPE        IFNE      0
010000941102     C                   MOVE      VDASPE        G08DAT
010100941102     C                   MOVE      *ZEROS        G08INV
010200941102     C                   MOVEL     *BLANK        G08ERR
010300941102     C                   CALL      'XSRDA8'
010400941102     C                   PARM                    WLBDA8
010500941102     C     G08ERR        IFEQ      '1'
010600000000     C                   SETON                                        82
010700000000     C                   END
010800000000     C   82              GOTO      VIDEO
010900941102     C                   Z-ADD     G08INV        DASPE
011000941102     C                   MOVE      G08DAT        VDASPE
011100000000     C                   ELSE
011200981023     C                   Z-ADD     19400101      DASPE
011300000000     C                   END
011400910902     C     VASPE         IFNE      0
011500941102     C                   Z-ADD     VASPE         G08DAT
011600941102     C                   MOVE      *ZEROS        G08INV
011700941102     C                   MOVEL     *BLANK        G08ERR
011800941102     C                   CALL      'XSRDA8'
011900941102     C                   PARM                    WLBDA8
012000941102     C     G08ERR        IFEQ      '1'
012100000000     C                   SETON                                        83
012200000000     C                   END
012300000000     C   83              GOTO      VIDEO
012400941102     C                   Z-ADD     G08INV        ASPE
012500941102     C                   MOVE      G08DAT        VASPE
012600000000     C                   ELSE
012700981023     C                   Z-ADD     99991231      ASPE
012800000000     C                   END
012900910905     C     ASPE          CABLT     DASPE         VIDEO                  81
013701040309     C*---- CONTROLLO LIMITI DATE  INCASSO
013702040309     C     VSEL2         IFEQ      *BLANK
013703040309     C     VDAINC        IFNE      *ZEROS
013704040309     C     VAINC         ORNE      *ZEROS
013705040309     C                   SETON                                        87
013706040309     C                   GOTO      VIDEO
013707040309     C                   END
013708040309     C                   END
013800910902     C     VDAINC        IFNE      0
013900941102     C                   Z-ADD     VDAINC        G08DAT
014000941102     C                   MOVE      *ZEROS        G08INV
014100941102     C                   MOVEL     *BLANK        G08ERR
014200941102     C                   CALL      'XSRDA8'
014300941102     C                   PARM                    WLBDA8
014400941102     C     G08ERR        IFEQ      '1'
014500910902     C                   SETON                                        84
014600910902     C                   END
014700910902     C   84              GOTO      VIDEO
014800941102     C                   Z-ADD     G08INV        DAINC
014900941102     C                   MOVE      G08DAT        VDAINC
015000910902     C                   ELSE
015100981130     C     VSEL2         IFEQ      'X'
015200981023     C                   Z-ADD     00010101      DAINC
015300981130     C                   ELSE
015400981130     C                   Z-ADD     0             DAINC
015500910902     C                   END
015600981130     C                   END
015700910902     C     VAINC         IFNE      0
015800941102     C                   Z-ADD     VAINC         G08DAT
015900941102     C                   MOVE      *ZEROS        G08INV
016000941102     C                   MOVEL     *BLANK        G08ERR
016100941102     C                   CALL      'XSRDA8'
016200941102     C                   PARM                    WLBDA8
016300941102     C     G08ERR        IFEQ      '1'
016400910902     C                   SETON                                        85
016500910902     C                   END
016600910902     C   85              GOTO      VIDEO
016700941102     C                   Z-ADD     G08INV        AINC
016800941102     C                   MOVE      G08DAT        VAINC
016900910902     C                   ELSE
017000981130     C     VSEL2         IFEQ      'X'
017100981023     C                   Z-ADD     99991231      AINC
017200981130     C                   ELSE
017300981130     C                   Z-ADD     0             AINC
017400910902     C                   END
017500981130     C                   END
017600910905     C     AINC          CABLT     DAINC         VIDEO                  86
017700911003     C* CONTROLLO LIMITI DATA PAGAMENTO
017800911003     C     VSEL3         IFEQ      *BLANK
017900960909     C     VSEL4         ANDEQ     *BLANK
018000911003     C     VDAPAG        IFNE      *ZEROS
018100911003     C     VAPAG         ORNE      *ZEROS
018200911003     C                   SETON                                        91
018300911003     C                   GOTO      VIDEO
018400911003     C                   END
018500911003     C                   END
018600911003     C     VDAPAG        IFNE      0
018700941102     C                   Z-ADD     VDAPAG        G08DAT
018800941102     C                   MOVE      *ZEROS        G08INV
018900941102     C                   MOVEL     *BLANK        G08ERR
019000941102     C                   CALL      'XSRDA8'
019100941102     C                   PARM                    WLBDA8
019200941102     C     G08ERR        IFEQ      '1'
019300911003     C                   SETON                                        88
019400911003     C                   END
019500911003     C   88              GOTO      VIDEO
019600941102     C                   Z-ADD     G08INV        DAPAG
019700941102     C                   MOVE      G08DAT        VDAPAG
019800911009     C                   END
019900911009     C     VSEL3         IFNE      *BLANKS
020000911009     C     VDAPAG        ANDEQ     0
020100960909     C     VSEL4         ORNE      *BLANKS
020200960909     C     VDAPAG        ANDEQ     0
020300911008     C                   SETON                                        88
020400911008     C                   GOTO      VIDEO
020500911003     C                   END
020600911003     C     VAPAG         IFNE      0
020700941102     C                   Z-ADD     VAPAG         G08DAT
020800941102     C                   MOVE      *ZEROS        G08INV
020900941102     C                   MOVEL     *BLANK        G08ERR
021000941102     C                   CALL      'XSRDA8'
021100941102     C                   PARM                    WLBDA8
021200941102     C     G08ERR        IFEQ      '1'
021300911003     C                   SETON                                        89
021400911003     C                   END
021500911003     C   89              GOTO      VIDEO
021600941102     C                   Z-ADD     G08INV        APAG
021700941102     C                   MOVE      G08DAT        VAPAG
021800911003     C                   ELSE
021900911003     C                   Z-ADD     999999        APAG
022000911003     C                   END
022100911003     C     APAG          CABLT     DAPAG         VIDEO                  90
022200911003     C* CONTROLLO FILIALI IMMESSE
022300911003     C     VDAFIL        IFNE      0
022400911003     C     VAFIL         CABLT     VDAFIL        VIDEO                  92
022500911003     C                   ELSE
022600911003     C                   Z-ADD     1             VDAFIL
022700911003     C                   Z-ADD     999           VAFIL
022800911003     C                   END
022823050707     C*---- CONTRLLO LIMITI cliente
022824050707     C     Vclida        IFNE      0
022825050707     c                   eval      vcli = vclida
022826050707     c                   exsr      srcli
022827050707     C     O69ERR        IFne      *BLANK
022831050707     C                   SETON                                        78
022832050707     C                   GOTO      VIDEO
022833050707     C                   END
022834050707     C                   END
022839050707     C     Vclia         IFNE      0
022840050707     c                   eval      vcli = vclia
022841050707     c                   exsr      srcli
022842050707     C     O69ERR        IFne      *BLANK
022846050707     C                   SETON                                        79
022848050707     C                   GOTO      VIDEO
022853050707     C                   END
022854050707     C                   END
022855050707     C     vclia         CABlT     vclida        VIDEO                  77
022900911003     C*
023000941102     C  NKF
023100941102     CANNKV              GOTO      VIDEO
023200941102     C* CMD21
023300941102     C   KV              DO
023400910902     C                   CALL      'BCH09'
023500910902     C                   PARM                    KPJBA
023600910902     C                   END
023700000000     C                   MOVEL     PARAM         KPJBU
023800941102     C                   MOVE      'SC91'        KCOAZ
023900000000     C                   CALL      'BCH10'
024000000000     C                   PARM                    KPJBA
024100000000     C     FINE          TAG
024200000000     C                   SETON                                        LR
024300050707      *---------------------------------------------------------------*
024400050707      *  srcli controllo cliente
024500050707      *---------------------------------------------------------------*
024600050707     C     srcli         BEGSR
024601050707     c                   clear                   tibs69
024630050707     C                   MOVEL     ' '           I69TLA
024631050707     C                   MOVEL     KNSIF         I69SIF
024632050707     C                   move      vcli          I69KAC
024634050707     C                   CALL      'TIBS69R'
024635050707     C                   PARM                    TIBS69
024636050707     C                   PARM                    CNACO
024637050707     C                   PARM                    CNIND
024638050707     C                   PARM                    CNCLP
024639050707     C                   PARM                    FNCLS
024700050707     c                   endsr
