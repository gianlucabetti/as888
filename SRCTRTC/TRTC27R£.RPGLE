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
001600140319     D  FlgUni                32     32
001700140319     D  CliVAS                33     40
001800921126     D WLBDAT          DS
001900941122     D  G02DAT                 1      8  0
002000941122     D  G02INV                 9     16  0
002100941122     D  G02ERR                17     17
002200941122     D  G02TGI                18     22  0
002300921126     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400090626     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002500090626     D DSACO         E DS                  EXTNAME(CNACO00F)
002600090626     D DSIND         E DS                  EXTNAME(CNIND00F)
002700090626     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002800090626     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002900921126     C*---------------------------------------------------------------*
003000921126     C     *ENTRY        PLIST
003100921126     C                   PARM                    KPJBA
003200921126     C                   Z-ADD     1             CODUT
003300921126     C                   CALL      'X§PARUT'
003400921126     C                   PARM                    UT§DSE
003500921126     C                   MOVEL     RAGUT         RSUT             20
003600101026     c                   movel     kpjbu         WREVISORI         1
003700101026     c
003800101026     c                   if        wrevisori='S'
003900101026     c                   seton                                        10
004000101026     c                   endif
004100140113     C*
004200140113     C                   EVAL      VIDFileSep= 'S'
004300140319     C                   EVAL      VIDFlgUni= *blank
004400140319     C                   EVAL      VIDCliVAS= *all'0'
004500921126     C*
004600921126     C     VIDEO         TAG
004700921126     C*
004800040806     C* NON ESEGUO IN SDI
004900040806     C     NETW          IFEQ      '101'
005000040806     C                   SETON                                        33
005100040806     C                   ENDIF
005200140113     C*
005300941122     C                   EXFMT     TC27D01
005400090626     C                   SETOFF                                       929394
005500140319     C                   SETOFF                                       989799
005600921126     C                   SETOFF                                       9695
005700140113     C                   EVAL      VIDCLIDES = *blanks
005800921126     C   KC              GOTO      FINE
005900040806     C   33              GOTO      FINE
006000921126     C*
006100101026     C  n10VIDDAL        IFEQ      *ZERO
006200941122     C     VIDALL        OREQ      *ZERO
006300921126     C                   SETON                                        98         DATE OBBLIGATORIE
006400101026     C                   GOTO      VIDEO
006500921126     C                   END
006600101026     c
006700101026     C   10VIDDAL        IFEQ      *ZERO
006800101026     C                   SETON                                        98         DATE OBBLIGATORIE
006900101026     C                   GOTO      VIDEO
007000101026     C                   END
007100101026
007200921126      *
007300941122      * GIRO E CONTROLLO DATE
007400941122      *
007500941122     C     VIDDAL        IFNE      *ZEROS
007600941122     C                   Z-ADD     VIDDAL        G02DAT
007700941122     C                   Z-ADD     *ZERO         G02INV
007800941122     C                   MOVEL     *BLANKS       G02ERR
007900941122     C                   CALL      'XSRDA8'
008000941122     C                   PARM                    WLBDAT
008100941122     C                   Z-ADD     G02DAT        VIDDAL
008200941122     C     G02ERR        IFEQ      '1'
008300921126     C                   SETON                                        97        Formato data errato
008400941122     C                   MOVE      *ZERO         DAL
008500941122     C                   GOTO      VIDEO
008600941122     C                   ELSE
008700941122     C                   Z-ADD     G02INV        DAL
008800921126     C                   END
008900941122     C                   END
009000921126      *
009100941122     C     VIDALL        IFNE      *ZEROS
009200941122     C                   Z-ADD     VIDALL        G02DAT
009300941122     C                   Z-ADD     *ZERO         G02INV
009400941122     C                   MOVEL     *BLANKS       G02ERR
009500941122     C                   CALL      'XSRDA8'
009600941122     C                   PARM                    WLBDAT
009700941122     C                   Z-ADD     G02DAT        VIDALL
009800941122     C     G02ERR        IFEQ      '1'
009900941122     C                   SETON                                        96        Formato data errato
010000941122     C                   MOVE      *ZERO         AL
010100941122     C                   GOTO      VIDEO
010200941122     C                   ELSE
010300941122     C                   Z-ADD     G02INV        AL
010400941122     C                   END
010500941122     C                   END
010600941122     C*
010700101026     C  n10DAL           IFGT      AL
010800921126     C                   SETON                                        95          1°DATA > 2°DATA
010900921126     C                   GOTO      VIDEO
011000921126     C                   END
011100090626     C*
011200090626     C* CONTROLLO CODICE CLIENTE
011300090626     C                   IF        VIDCLI > *zeros
011400090626     C                   CLEAR                   DSBS69
011500090626     C                   MOVEL     VIDCLI        I69KAC
011600090626     C                   MOVEL     KNSIF         I69SIF
011700090626     C                   CALL      'TIBS69R'
011800090626     C                   PARM                    DSBS69
011900090626     C                   PARM                    DSACO
012000090626     C                   PARM                    DSIND
012100090626     C                   PARM                    DSCLP
012200090626     C                   PARM                    DSCLS
012300090626     C*
012400090626     C     O69ERR        IFEQ      '1'
012500090626     C                   SETON                                        92        Cliente errato
012600090626     C                   ELSE
012700090626     C                   EVAL      VIDCLIDES = ACORAG
012800090626     C                   ENDIF
012900140319     C*
013000140319     C* se è stato indicato un cliente, il flag unificante deve essere valorizzato
013100140319     C                   IF        VIDFlgUni = *blank
013200140319     C                   SETON                                        99        Richiesto flag unif.
013300140319     C                   ENDIF
013400140108     C*
013500140108     C* se è stato indicato un numero fattura, è obbligatorio anche il cod.cliente
013600140108     C                   ELSE
013700140108     C                   IF        VidNFt > 0
013800140108     C                   SETON                                        94        Richiesto codice cli
013900140108     C                   ENDIF
014000140319     C*
014100140319     C* se è stato indicato nel flag che il cliente è unificante, il cliente deve esserci
014200140319     C                   IF        VIDFlgUni = 'S'
014300140319     C                   SETON                                        94        Richiesto codice cli
014400140319     C                   ENDIF
014500090626     C                   ENDIF
014600921126      *
014700090626     C     *INKF         IFNE      '0'
014800941122     C*
014900090626     C     VIDCLI        IFGT      *ZEROS
015000941122     C                   Z-ADD     VIDCLI        CLIU
015100140108     C                   Z-ADD     VIDNFT        NFT
015200140113     C                   MOVEL     VIDFILESEP    FILESEP
015300140319     C                   MOVEL     VIDFlgUni     FlgUni
015400140319     C                   MOVEL     VIDCliVAS     CliVAS
015500921126     C                   MOVEL     PARAM         KPJBU
015600040901     C                   MOVE      'TC28'        KCOAZ
015700030519     C                   MOVE      '3'           KRITB
015800060203     C                   CALL      'BCH09'
015900060203     C                   PARM                    KPJBA
016000060203     C     KRITB         CABEQ     '2'           VIDEO
016100060203     C                   CALL      'BCH10'
016200060203     C                   PARM                    KPJBA
016300060203     C*                    CALL 'TRTC28C'
016400060203     C*                    PARM           KPJBA
016500090626     C                   GOTO      FINE
016600090626     C                   ELSE
016700090626     C                   SETON                                        94        Richiesto codice cli
016800090626     C                   ENDIF
016900090626     C                   ENDIF
017000090626      *
017100090626     C     *INKQ         IFNE      '0'
017200090626     C*
017300090626     C     VIDCLI        IFEQ      *ZEROS
017400090626     C                   Z-ADD     VIDCLI        CLIU
017500140115     C                   Z-ADD     VIDNFT        NFT
017600140115     C                   MOVEL     VIDFILESEP    FILESEP
017700140319     C                   MOVEL     VIDFlgUni     FlgUni
017800140319     C                   MOVEL     VIDCliVAS     CliVAS
017900090626     C                   MOVEL     PARAM         KPJBU
018000090626     C                   MOVE      'TC28'        KCOAZ
018100090626     C                   MOVE      '3'           KRITB
018200090626     C                   CALL      'BCH09'
018300090626     C                   PARM                    KPJBA
018400090626     C     KRITB         CABEQ     '2'           VIDEO
018500090626     C                   CALL      'BCH10'
018600090626     C                   PARM                    KPJBA
018700090626     C*                    CALL 'TRTC28C'
018800090626     C*                    PARM           KPJBA
018900090626     C                   GOTO      FINE
019000090626     C                   ELSE
019100090626     C                   SETON                                        93        Presente codice clie
019200090626     C                   ENDIF
019300090626     C                   ENDIF
019400090626     C*
019500090626     C                   GOTO      VIDEO
019600921126      *
019700921126     C     FINE          TAG
019800090626     C**
019900090626     C                   CLEAR                   DSBS69
020000090626     C                   MOVEL     'C'           I69TLA
020100090626     C                   CALL      'TIBS69R'
020200090626     C                   PARM                    DSBS69
020300090626     C                   PARM                    DSACO
020400090626     C                   PARM                    DSIND
020500090626     C                   PARM                    DSCLP
020600090626     C                   PARM                    DSCLS
020700090626      *
020800921126     C                   SETON                                        LR
