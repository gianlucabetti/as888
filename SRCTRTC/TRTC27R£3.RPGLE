000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200921126     H*----------------------------------------------------------*
000300921126     H**  PREP. ARCHIVIO BOLLE FATTURATE  AL CLIENTE  - FILTRO   *
000301101026     h**   anche lancio per revisori
000400921126     H*----------------------------------------------------------*
000500941122     FTRTC27D   CF   E             WORKSTN
000600890921     D KPJBA         E DS
000700040806     D  NETW                  98    100
000800900315     D PARAM           DS
000900941122     D  DAL                    1      8  0
001000941122     D  AL                     9     16  0
001100941122     D  CLIU                  17     23  0
001101101026     D  WREVISORI             24     24
001200921126     D WLBDAT          DS
001300941122     D  G02DAT                 1      8  0
001400941122     D  G02INV                 9     16  0
001500941122     D  G02ERR                17     17
001600941122     D  G02TGI                18     22  0
001700921126     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800090626     D DSBS69        E DS                  EXTNAME(TIBS69DS)
001900090626     D DSACO         E DS                  EXTNAME(CNACO00F)
002000090626     D DSIND         E DS                  EXTNAME(CNIND00F)
002100090626     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002200090626     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002300921126     C*---------------------------------------------------------------*
002400921126     C     *ENTRY        PLIST
002500921126     C                   PARM                    KPJBA
002600921126     C                   Z-ADD     1             CODUT
002700921126     C                   CALL      'X§PARUT'
002800921126     C                   PARM                    UT§DSE
003000921126     C                   MOVEL     RAGUT         RSUT             20
003001101026     c                   movel     kpjbu         WREVISORI         1
003002101026     c
003003101026     c                   if        wrevisori='S'
003004101026     c                   seton                                        10
003005101026     c                   endif
003100921126     C*
003200921126     C     VIDEO         TAG
003300921126     C*
003400040806     C* NON ESEGUO IN SDI
003500040806     C     NETW          IFEQ      '101'
003600040806     C                   SETON                                        33
003700040806     C                   ENDIF
003800040806     C*
003900941122     C                   EXFMT     TC27D01
004000090626     C                   SETOFF                                       929394
004100921126     C                   SETOFF                                       9897
004200921126     C                   SETOFF                                       9695
004300090626     C                   EVAL      VIDCLIDES = *blanks
004400921126     C   KC              GOTO      FINE
004500040806     C   33              GOTO      FINE
004600921126     C*
004700101026     C  n10VIDDAL        IFEQ      *ZERO
004800941122     C     VIDALL        OREQ      *ZERO
004900921126     C                   SETON                                        98         DATE OBBLIGATORIE
004901101026     C                   GOTO      VIDEO
005000921126     C                   END
005001101026     c
005002101026     C   10VIDDAL        IFEQ      *ZERO
005003101026     C                   SETON                                        98         DATE OBBLIGATORIE
005004101026     C                   GOTO      VIDEO
005005101026     C                   END
005006101026
005200921126      *
005300941122      * GIRO E CONTROLLO DATE
005400941122      *
005500941122     C     VIDDAL        IFNE      *ZEROS
005600941122     C                   Z-ADD     VIDDAL        G02DAT
005700941122     C                   Z-ADD     *ZERO         G02INV
005800941122     C                   MOVEL     *BLANKS       G02ERR
005900941122     C                   CALL      'XSRDA8'
006000941122     C                   PARM                    WLBDAT
006100941122     C                   Z-ADD     G02DAT        VIDDAL
006200941122     C     G02ERR        IFEQ      '1'
006300921126     C                   SETON                                        97        Formato data errato
006400941122     C                   MOVE      *ZERO         DAL
006500941122     C                   GOTO      VIDEO
006600941122     C                   ELSE
006700941122     C                   Z-ADD     G02INV        DAL
006800921126     C                   END
006900941122     C                   END
007000921126      *
007100941122     C     VIDALL        IFNE      *ZEROS
007200941122     C                   Z-ADD     VIDALL        G02DAT
007300941122     C                   Z-ADD     *ZERO         G02INV
007400941122     C                   MOVEL     *BLANKS       G02ERR
007500941122     C                   CALL      'XSRDA8'
007600941122     C                   PARM                    WLBDAT
007700941122     C                   Z-ADD     G02DAT        VIDALL
007800941122     C     G02ERR        IFEQ      '1'
007900941122     C                   SETON                                        96        Formato data errato
008000941122     C                   MOVE      *ZERO         AL
008100941122     C                   GOTO      VIDEO
008200941122     C                   ELSE
008300941122     C                   Z-ADD     G02INV        AL
008400941122     C                   END
008500941122     C                   END
008600941122     C*
008700101026     C  n10DAL           IFGT      AL
008800921126     C                   SETON                                        95          1°DATA > 2°DATA
008900921126     C                   GOTO      VIDEO
009000921126     C                   END
009100090626     C*
009200090626     C* CONTROLLO CODICE CLIENTE
009300090626     C                   IF        VIDCLI > *zeros
009400090626     C                   CLEAR                   DSBS69
009500090626     C                   MOVEL     VIDCLI        I69KAC
009600090626     C                   MOVEL     KNSIF         I69SIF
009700090626     C                   CALL      'TIBS69R'
009800090626     C                   PARM                    DSBS69
009900090626     C                   PARM                    DSACO
010000090626     C                   PARM                    DSIND
010100090626     C                   PARM                    DSCLP
010200090626     C                   PARM                    DSCLS
010300090626     C*
010400090626     C     O69ERR        IFEQ      '1'
010500090626     C                   SETON                                        92        Cliente errato
010600090626     C                   ELSE
010700090626     C                   EVAL      VIDCLIDES = ACORAG
010800090626     C                   ENDIF
010900090626     C                   ENDIF
011000921126      *
011100090626     C     *INKF         IFNE      '0'
011200941122     C*
011300090626     C     VIDCLI        IFGT      *ZEROS
011400941122     C                   Z-ADD     VIDCLI        CLIU
011500921126     C                   MOVEL     PARAM         KPJBU
011600040901     C                   MOVE      'TC28'        KCOAZ
011700030519     C                   MOVE      '3'           KRITB
011800060203     C                   CALL      'BCH09'
011900060203     C                   PARM                    KPJBA
012000060203     C     KRITB         CABEQ     '2'           VIDEO
012100060203     C                   CALL      'BCH10'
012200060203     C                   PARM                    KPJBA
012300060203     C*                    CALL 'TRTC28C'
012400060203     C*                    PARM           KPJBA
012500090626     C                   GOTO      FINE
012600090626     C                   ELSE
012700090626     C                   SETON                                        94        Richiesto codice cli
012800090626     C                   ENDIF
012900090626     C                   ENDIF
013000090626      *
013100090626     C     *INKQ         IFNE      '0'
013200090626     C*
013300090626     C     VIDCLI        IFEQ      *ZEROS
013400090626     C                   Z-ADD     VIDCLI        CLIU
013500090626     C                   MOVEL     PARAM         KPJBU
013600090626     C                   MOVE      'TC28'        KCOAZ
013700090626     C                   MOVE      '3'           KRITB
013800090626     C                   CALL      'BCH09'
013900090626     C                   PARM                    KPJBA
014000090626     C     KRITB         CABEQ     '2'           VIDEO
014100090626     C                   CALL      'BCH10'
014200090626     C                   PARM                    KPJBA
014300090626     C*                    CALL 'TRTC28C'
014400090626     C*                    PARM           KPJBA
014500090626     C                   GOTO      FINE
014600090626     C                   ELSE
014700090626     C                   SETON                                        93        Presente codice clie
014800090626     C                   ENDIF
014900090626     C                   ENDIF
015000090626     C*
015100090626     C                   GOTO      VIDEO
015200921126      *
015300921126     C     FINE          TAG
015400090626     C**
015500090626     C                   CLEAR                   DSBS69
015600090626     C                   MOVEL     'C'           I69TLA
015700090626     C                   CALL      'TIBS69R'
015800090626     C                   PARM                    DSBS69
015900090626     C                   PARM                    DSACO
016000090626     C                   PARM                    DSIND
016100090626     C                   PARM                    DSCLP
016200090626     C                   PARM                    DSCLS
016300090626      *
016400921126     C                   SETON                                        LR
