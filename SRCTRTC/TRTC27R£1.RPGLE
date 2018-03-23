000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200921126     H*----------------------------------------------------------*
000300921126     H**  PREP. ARCHIVIO BOLLE FATTURATE  AL CLIENTE  - FILTRO   *
000400921126     H*----------------------------------------------------------*
000500941122     FTRTC27D   CF   E             WORKSTN
000600890921     D KPJBA         E DS
000700040806     D  NETW                  98    100
000800900315     D PARAM           DS
000900941122     D  DAL                    1      8  0
001000941122     D  AL                     9     16  0
001100941122     D  CLIU                  17     23  0
001200921126     D WLBDAT          DS
001300941122     D  G02DAT                 1      8  0
001400941122     D  G02INV                 9     16  0
001500941122     D  G02ERR                17     17
001600941122     D  G02TGI                18     22  0
001700921126     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800921126     C*---------------------------------------------------------------*
001900921126     C     *ENTRY        PLIST
002000921126     C                   PARM                    KPJBA
002100921126     C                   Z-ADD     1             CODUT
002200921126     C                   CALL      'X§PARUT'
002300921126     C                   PARM                    UT§DSE
002400921126     C     CODUT         CABEQ     -1            FINE
002500921126     C                   MOVEL     RAGUT         RSUT             20
002600921126     C*
002700921126     C     VIDEO         TAG
002800921126     C*
002900040806     C* NON ESEGUO IN SDI
003000040806     C     NETW          IFEQ      '101'
003100040806     C                   SETON                                        33
003200040806     C                   ENDIF
003300040806     C*
003400941122     C                   EXFMT     TC27D01
003500921126     C                   SETOFF                                       9897
003600921126     C                   SETOFF                                       9695
003700921126     C   KC              GOTO      FINE
003800040806     C   33              GOTO      FINE
003900921126     C*
004000941122     C     VIDDAL        IFEQ      *ZERO
004100941122     C     VIDALL        OREQ      *ZERO
004200921126     C                   SETON                                        98         DATE OBBLIGATORIE
004300921126     C                   END
004400921126     C   98              GOTO      VIDEO
004500921126      *
004600941122      * GIRO E CONTROLLO DATE
004700941122      *
004800941122     C     VIDDAL        IFNE      *ZEROS
004900941122     C                   Z-ADD     VIDDAL        G02DAT
005000941122     C                   Z-ADD     *ZERO         G02INV
005100941122     C                   MOVEL     *BLANKS       G02ERR
005200941122     C                   CALL      'XSRDA8'
005300941122     C                   PARM                    WLBDAT
005400941122     C                   Z-ADD     G02DAT        VIDDAL
005500941122     C     G02ERR        IFEQ      '1'
005600921126     C                   SETON                                        97        Formato data errato
005700941122     C                   MOVE      *ZERO         DAL
005800941122     C                   GOTO      VIDEO
005900941122     C                   ELSE
006000941122     C                   Z-ADD     G02INV        DAL
006100921126     C                   END
006200941122     C                   END
006300921126      *
006400941122     C     VIDALL        IFNE      *ZEROS
006500941122     C                   Z-ADD     VIDALL        G02DAT
006600941122     C                   Z-ADD     *ZERO         G02INV
006700941122     C                   MOVEL     *BLANKS       G02ERR
006800941122     C                   CALL      'XSRDA8'
006900941122     C                   PARM                    WLBDAT
007000941122     C                   Z-ADD     G02DAT        VIDALL
007100941122     C     G02ERR        IFEQ      '1'
007200941122     C                   SETON                                        96        Formato data errato
007300941122     C                   MOVE      *ZERO         AL
007400941122     C                   GOTO      VIDEO
007500941122     C                   ELSE
007600941122     C                   Z-ADD     G02INV        AL
007700941122     C                   END
007800941122     C                   END
007900941122     C*
008000921126     C     DAL           IFGT      AL
008100921126     C                   SETON                                        95          1°DATA > 2°DATA
008200921126     C                   GOTO      VIDEO
008300921126     C                   END
008400921126      *
008500921126     C     *INKF         CABEQ     '0'           VIDEO
008600941122     C*
008700090626     C     VIDCLI        IFGT      *ZEROS
008800941122     C                   Z-ADD     VIDCLI        CLIU
008900921126     C                   MOVEL     PARAM         KPJBU
009000040901     C                   MOVE      'TC28'        KCOAZ
009100030519     C                   MOVE      '3'           KRITB
009200060203     C                   CALL      'BCH09'
009300060203     C                   PARM                    KPJBA
009400060203     C     KRITB         CABEQ     '2'           VIDEO
009500060203     C                   CALL      'BCH10'
009600060203     C                   PARM                    KPJBA
009700060203     C*                    CALL 'TRTC28C'
009800060203     C*                    PARM           KPJBA
009900090626     C                   ELSE
010000090626     C                   SETON                                        94        Richiesto codice cli
010100090626     C                   ENDIF
010200090626      *
010300090626     C     *INKQ         CABEQ     '0'           VIDEO
010400090626     C*
010500090626     C     VIDCLI        IFEQ      *ZEROS
010600090626     C                   Z-ADD     VIDCLI        CLIU
010700090626     C                   MOVEL     PARAM         KPJBU
010800090626     C                   MOVE      'TC28'        KCOAZ
010900090626     C                   MOVE      '3'           KRITB
011000090626     C                   CALL      'BCH09'
011100090626     C                   PARM                    KPJBA
011200090626     C     KRITB         CABEQ     '2'           VIDEO
011300090626     C                   CALL      'BCH10'
011400090626     C                   PARM                    KPJBA
011500090626     C*                    CALL 'TRTC28C'
011600090626     C*                    PARM           KPJBA
011700090626     C                   ELSE
011800090626     C                   SETON                                        93        Presente codice clie
011900090626     C                   ENDIF
012000921126      *
012100921126     C     FINE          TAG
012200921126     C                   SETON                                        LR
