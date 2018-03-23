000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200921126     H*----------------------------------------------------------*
000300921126     H**  PREP. ARCHIVIO BOLLE FATTURATE  AL CLIENTE  - FILTRO   *
000400101026     h**   anche lancio per revisori
000500921126     H*----------------------------------------------------------*
000600941122     FTRTC27D   CF   E             WORKSTN
000700890921     D KPJBA         E DS
000800040806     D  NETW                  98    100
000900900315     D PARAM           DS
001000941122     D  DAL                    1      8  0
001100941122     D  AL                     9     16  0
001200941122     D  CLIU                  17     23  0
001300101026     D  WREVISORI             24     24
001400140108     D  NFT                   25     30  0
001500921126     D WLBDAT          DS
001600941122     D  G02DAT                 1      8  0
001700941122     D  G02INV                 9     16  0
001800941122     D  G02ERR                17     17
001900941122     D  G02TGI                18     22  0
002000921126     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100090626     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002200090626     D DSACO         E DS                  EXTNAME(CNACO00F)
002300090626     D DSIND         E DS                  EXTNAME(CNIND00F)
002400090626     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002500090626     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002600921126     C*---------------------------------------------------------------*
002700921126     C     *ENTRY        PLIST
002800921126     C                   PARM                    KPJBA
002900921126     C                   Z-ADD     1             CODUT
003000921126     C                   CALL      'X§PARUT'
003100921126     C                   PARM                    UT§DSE
003200921126     C                   MOVEL     RAGUT         RSUT             20
003300101026     c                   movel     kpjbu         WREVISORI         1
003400101026     c
003500101026     c                   if        wrevisori='S'
003600101026     c                   seton                                        10
003700101026     c                   endif
003800921126     C*
003900921126     C     VIDEO         TAG
004000921126     C*
004100040806     C* NON ESEGUO IN SDI
004200040806     C     NETW          IFEQ      '101'
004300040806     C                   SETON                                        33
004400040806     C                   ENDIF
004500040806     C*
004600941122     C                   EXFMT     TC27D01
004700090626     C                   SETOFF                                       929394
004800921126     C                   SETOFF                                       9897
004900921126     C                   SETOFF                                       9695
005000090626     C                   EVAL      VIDCLIDES = *blanks
005100921126     C   KC              GOTO      FINE
005200040806     C   33              GOTO      FINE
005300921126     C*
005400101026     C  n10VIDDAL        IFEQ      *ZERO
005500941122     C     VIDALL        OREQ      *ZERO
005600921126     C                   SETON                                        98         DATE OBBLIGATORIE
005700101026     C                   GOTO      VIDEO
005800921126     C                   END
005900101026     c
006000101026     C   10VIDDAL        IFEQ      *ZERO
006100101026     C                   SETON                                        98         DATE OBBLIGATORIE
006200101026     C                   GOTO      VIDEO
006300101026     C                   END
006400101026
006500921126      *
006600941122      * GIRO E CONTROLLO DATE
006700941122      *
006800941122     C     VIDDAL        IFNE      *ZEROS
006900941122     C                   Z-ADD     VIDDAL        G02DAT
007000941122     C                   Z-ADD     *ZERO         G02INV
007100941122     C                   MOVEL     *BLANKS       G02ERR
007200941122     C                   CALL      'XSRDA8'
007300941122     C                   PARM                    WLBDAT
007400941122     C                   Z-ADD     G02DAT        VIDDAL
007500941122     C     G02ERR        IFEQ      '1'
007600921126     C                   SETON                                        97        Formato data errato
007700941122     C                   MOVE      *ZERO         DAL
007800941122     C                   GOTO      VIDEO
007900941122     C                   ELSE
008000941122     C                   Z-ADD     G02INV        DAL
008100921126     C                   END
008200941122     C                   END
008300921126      *
008400941122     C     VIDALL        IFNE      *ZEROS
008500941122     C                   Z-ADD     VIDALL        G02DAT
008600941122     C                   Z-ADD     *ZERO         G02INV
008700941122     C                   MOVEL     *BLANKS       G02ERR
008800941122     C                   CALL      'XSRDA8'
008900941122     C                   PARM                    WLBDAT
009000941122     C                   Z-ADD     G02DAT        VIDALL
009100941122     C     G02ERR        IFEQ      '1'
009200941122     C                   SETON                                        96        Formato data errato
009300941122     C                   MOVE      *ZERO         AL
009400941122     C                   GOTO      VIDEO
009500941122     C                   ELSE
009600941122     C                   Z-ADD     G02INV        AL
009700941122     C                   END
009800941122     C                   END
009900941122     C*
010000101026     C  n10DAL           IFGT      AL
010100921126     C                   SETON                                        95          1°DATA > 2°DATA
010200921126     C                   GOTO      VIDEO
010300921126     C                   END
010400090626     C*
010500090626     C* CONTROLLO CODICE CLIENTE
010600090626     C                   IF        VIDCLI > *zeros
010700090626     C                   CLEAR                   DSBS69
010800090626     C                   MOVEL     VIDCLI        I69KAC
010900090626     C                   MOVEL     KNSIF         I69SIF
011000090626     C                   CALL      'TIBS69R'
011100090626     C                   PARM                    DSBS69
011200090626     C                   PARM                    DSACO
011300090626     C                   PARM                    DSIND
011400090626     C                   PARM                    DSCLP
011500090626     C                   PARM                    DSCLS
011600090626     C*
011700090626     C     O69ERR        IFEQ      '1'
011800090626     C                   SETON                                        92        Cliente errato
011900090626     C                   ELSE
012000090626     C                   EVAL      VIDCLIDES = ACORAG
012100090626     C                   ENDIF
012200140108     C*
012300140108     C* se è stato indicato un numero fattura, è obbligatorio anche il cod.cliente
012400140108     C                   ELSE
012500140108     C                   IF        VidNFt > 0
012600140108     C                   SETON                                        94        Richiesto codice cli
012700140108     C                   ENDIF
012800090626     C                   ENDIF
012900921126      *
013000090626     C     *INKF         IFNE      '0'
013100941122     C*
013200090626     C     VIDCLI        IFGT      *ZEROS
013300941122     C                   Z-ADD     VIDCLI        CLIU
013400140108     C                   Z-ADD     VIDNFT        NFT
013500921126     C                   MOVEL     PARAM         KPJBU
013600040901     C                   MOVE      'TC28'        KCOAZ
013700030519     C                   MOVE      '3'           KRITB
013800060203     C                   CALL      'BCH09'
013900060203     C                   PARM                    KPJBA
014000060203     C     KRITB         CABEQ     '2'           VIDEO
014100060203     C                   CALL      'BCH10'
014200060203     C                   PARM                    KPJBA
014300060203     C*                    CALL 'TRTC28C'
014400060203     C*                    PARM           KPJBA
014500090626     C                   GOTO      FINE
014600090626     C                   ELSE
014700090626     C                   SETON                                        94        Richiesto codice cli
014800090626     C                   ENDIF
014900090626     C                   ENDIF
015000090626      *
015100090626     C     *INKQ         IFNE      '0'
015200090626     C*
015300090626     C     VIDCLI        IFEQ      *ZEROS
015400090626     C                   Z-ADD     VIDCLI        CLIU
015500090626     C                   MOVEL     PARAM         KPJBU
015600090626     C                   MOVE      'TC28'        KCOAZ
015700090626     C                   MOVE      '3'           KRITB
015800090626     C                   CALL      'BCH09'
015900090626     C                   PARM                    KPJBA
016000090626     C     KRITB         CABEQ     '2'           VIDEO
016100090626     C                   CALL      'BCH10'
016200090626     C                   PARM                    KPJBA
016300090626     C*                    CALL 'TRTC28C'
016400090626     C*                    PARM           KPJBA
016500090626     C                   GOTO      FINE
016600090626     C                   ELSE
016700090626     C                   SETON                                        93        Presente codice clie
016800090626     C                   ENDIF
016900090626     C                   ENDIF
017000090626     C*
017100090626     C                   GOTO      VIDEO
017200921126      *
017300921126     C     FINE          TAG
017400090626     C**
017500090626     C                   CLEAR                   DSBS69
017600090626     C                   MOVEL     'C'           I69TLA
017700090626     C                   CALL      'TIBS69R'
017800090626     C                   PARM                    DSBS69
017900090626     C                   PARM                    DSACO
018000090626     C                   PARM                    DSIND
018100090626     C                   PARM                    DSCLP
018200090626     C                   PARM                    DSCLS
018300090626      *
018400921126     C                   SETON                                        LR
