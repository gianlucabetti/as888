000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200921126     H*----------------------------------------------------------*
000300921126     H**  PREP. ARCHIVIO BOLLE FATTURATE  AL CLIENTE  - FILTRO   *
000400101026     h**   anche lancio per revisori
000500921126     H*----------------------------------------------------------*
000600941122     FTRTC27D   CF   E             WORKSTN
000700140113     D KPJBA         E DS
000800040806     D  NETW                  98    100
000900900315     D PARAM           DS
001000941122     D  DAL                    1      8  0
001100941122     D  AL                     9     16  0
001200941122     D  CLIU                  17     23  0
001300101026     D  WREVISORI             24     24
001400140108     D  NFT                   25     30  0
001500140113     D  FILESEP               31     31
001600921126     D WLBDAT          DS
001700941122     D  G02DAT                 1      8  0
001800941122     D  G02INV                 9     16  0
001900941122     D  G02ERR                17     17
002000941122     D  G02TGI                18     22  0
002100921126     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200090626     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002300090626     D DSACO         E DS                  EXTNAME(CNACO00F)
002400090626     D DSIND         E DS                  EXTNAME(CNIND00F)
002500090626     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002600090626     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002700921126     C*---------------------------------------------------------------*
002800921126     C     *ENTRY        PLIST
002900921126     C                   PARM                    KPJBA
003000921126     C                   Z-ADD     1             CODUT
003100921126     C                   CALL      'X§PARUT'
003200921126     C                   PARM                    UT§DSE
003300921126     C                   MOVEL     RAGUT         RSUT             20
003400101026     c                   movel     kpjbu         WREVISORI         1
003500101026     c
003600101026     c                   if        wrevisori='S'
003700101026     c                   seton                                        10
003800101026     c                   endif
003900140113     C*
004000140113     C                   EVAL      VIDFileSep= 'S'
004100921126     C*
004200921126     C     VIDEO         TAG
004300921126     C*
004400040806     C* NON ESEGUO IN SDI
004500040806     C     NETW          IFEQ      '101'
004600040806     C                   SETON                                        33
004700040806     C                   ENDIF
004800140113     C*
004900941122     C                   EXFMT     TC27D01
005000090626     C                   SETOFF                                       929394
005100921126     C                   SETOFF                                       9897
005200921126     C                   SETOFF                                       9695
005300140113     C                   EVAL      VIDCLIDES = *blanks
005400921126     C   KC              GOTO      FINE
005500040806     C   33              GOTO      FINE
005600921126     C*
005700101026     C  n10VIDDAL        IFEQ      *ZERO
005800941122     C     VIDALL        OREQ      *ZERO
005900921126     C                   SETON                                        98         DATE OBBLIGATORIE
006000101026     C                   GOTO      VIDEO
006100921126     C                   END
006200101026     c
006300101026     C   10VIDDAL        IFEQ      *ZERO
006400101026     C                   SETON                                        98         DATE OBBLIGATORIE
006500101026     C                   GOTO      VIDEO
006600101026     C                   END
006700101026
006800921126      *
006900941122      * GIRO E CONTROLLO DATE
007000941122      *
007100941122     C     VIDDAL        IFNE      *ZEROS
007200941122     C                   Z-ADD     VIDDAL        G02DAT
007300941122     C                   Z-ADD     *ZERO         G02INV
007400941122     C                   MOVEL     *BLANKS       G02ERR
007500941122     C                   CALL      'XSRDA8'
007600941122     C                   PARM                    WLBDAT
007700941122     C                   Z-ADD     G02DAT        VIDDAL
007800941122     C     G02ERR        IFEQ      '1'
007900921126     C                   SETON                                        97        Formato data errato
008000941122     C                   MOVE      *ZERO         DAL
008100941122     C                   GOTO      VIDEO
008200941122     C                   ELSE
008300941122     C                   Z-ADD     G02INV        DAL
008400921126     C                   END
008500941122     C                   END
008600921126      *
008700941122     C     VIDALL        IFNE      *ZEROS
008800941122     C                   Z-ADD     VIDALL        G02DAT
008900941122     C                   Z-ADD     *ZERO         G02INV
009000941122     C                   MOVEL     *BLANKS       G02ERR
009100941122     C                   CALL      'XSRDA8'
009200941122     C                   PARM                    WLBDAT
009300941122     C                   Z-ADD     G02DAT        VIDALL
009400941122     C     G02ERR        IFEQ      '1'
009500941122     C                   SETON                                        96        Formato data errato
009600941122     C                   MOVE      *ZERO         AL
009700941122     C                   GOTO      VIDEO
009800941122     C                   ELSE
009900941122     C                   Z-ADD     G02INV        AL
010000941122     C                   END
010100941122     C                   END
010200941122     C*
010300101026     C  n10DAL           IFGT      AL
010400921126     C                   SETON                                        95          1°DATA > 2°DATA
010500921126     C                   GOTO      VIDEO
010600921126     C                   END
010700090626     C*
010800090626     C* CONTROLLO CODICE CLIENTE
010900090626     C                   IF        VIDCLI > *zeros
011000090626     C                   CLEAR                   DSBS69
011100090626     C                   MOVEL     VIDCLI        I69KAC
011200090626     C                   MOVEL     KNSIF         I69SIF
011300090626     C                   CALL      'TIBS69R'
011400090626     C                   PARM                    DSBS69
011500090626     C                   PARM                    DSACO
011600090626     C                   PARM                    DSIND
011700090626     C                   PARM                    DSCLP
011800090626     C                   PARM                    DSCLS
011900090626     C*
012000090626     C     O69ERR        IFEQ      '1'
012100090626     C                   SETON                                        92        Cliente errato
012200090626     C                   ELSE
012300090626     C                   EVAL      VIDCLIDES = ACORAG
012400090626     C                   ENDIF
012500140108     C*
012600140108     C* se è stato indicato un numero fattura, è obbligatorio anche il cod.cliente
012700140108     C                   ELSE
012800140108     C                   IF        VidNFt > 0
012900140108     C                   SETON                                        94        Richiesto codice cli
013000140108     C                   ENDIF
013100090626     C                   ENDIF
013200921126      *
013300090626     C     *INKF         IFNE      '0'
013400941122     C*
013500090626     C     VIDCLI        IFGT      *ZEROS
013600941122     C                   Z-ADD     VIDCLI        CLIU
013700140108     C                   Z-ADD     VIDNFT        NFT
013800140113     C                   MOVEL     VIDFILESEP    FILESEP
013900921126     C                   MOVEL     PARAM         KPJBU
014000040901     C                   MOVE      'TC28'        KCOAZ
014100030519     C                   MOVE      '3'           KRITB
014200060203     C                   CALL      'BCH09'
014300060203     C                   PARM                    KPJBA
014400060203     C     KRITB         CABEQ     '2'           VIDEO
014500060203     C                   CALL      'BCH10'
014600060203     C                   PARM                    KPJBA
014700060203     C*                    CALL 'TRTC28C'
014800060203     C*                    PARM           KPJBA
014900090626     C                   GOTO      FINE
015000090626     C                   ELSE
015100090626     C                   SETON                                        94        Richiesto codice cli
015200090626     C                   ENDIF
015300090626     C                   ENDIF
015400090626      *
015500090626     C     *INKQ         IFNE      '0'
015600090626     C*
015700090626     C     VIDCLI        IFEQ      *ZEROS
015800090626     C                   Z-ADD     VIDCLI        CLIU
015900140115     C                   Z-ADD     VIDNFT        NFT
016000140115     C                   MOVEL     VIDFILESEP    FILESEP
016100090626     C                   MOVEL     PARAM         KPJBU
016200090626     C                   MOVE      'TC28'        KCOAZ
016300090626     C                   MOVE      '3'           KRITB
016400090626     C                   CALL      'BCH09'
016500090626     C                   PARM                    KPJBA
016600090626     C     KRITB         CABEQ     '2'           VIDEO
016700090626     C                   CALL      'BCH10'
016800090626     C                   PARM                    KPJBA
016900090626     C*                    CALL 'TRTC28C'
017000090626     C*                    PARM           KPJBA
017100090626     C                   GOTO      FINE
017200090626     C                   ELSE
017300090626     C                   SETON                                        93        Presente codice clie
017400090626     C                   ENDIF
017500090626     C                   ENDIF
017600090626     C*
017700090626     C                   GOTO      VIDEO
017800921126      *
017900921126     C     FINE          TAG
018000090626     C**
018100090626     C                   CLEAR                   DSBS69
018200090626     C                   MOVEL     'C'           I69TLA
018300090626     C                   CALL      'TIBS69R'
018400090626     C                   PARM                    DSBS69
018500090626     C                   PARM                    DSACO
018600090626     C                   PARM                    DSIND
018700090626     C                   PARM                    DSCLP
018800090626     C                   PARM                    DSCLS
018900090626      *
019000921126     C                   SETON                                        LR
