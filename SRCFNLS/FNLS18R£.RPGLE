000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941110     H* FNLS18R *----------------------------------------------------*
000300990701     H*         - RISTAMPA PREPAATI               FILTRO
000400000000     H*--------------------------------------------------------------*
000500941110     FFNLS18D   CF   E             WORKSTN
000600920930     D L1              S              3  0 DIM(30)
000700030625
000800030625      * PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - FNLS19R -
000900030625     d Fnls19ds      e ds
001000030625
001100941110     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
001200941110     D WLBDAT          DS                  INZ
001300941110     D  G02DAT                 1      8  0
001400941110     D  G02INV                 9     16  0
001500941110     D  G02ERR                17     17
001600941110     D  G02TGI                18     22  0
001700920930     D CNCR80        E DS
001800920930     D KPJBA         E DS
001900030625
002000030625      * DS PER TRUL90R - RICHIESTA STAMPANTI
002100030625     d Trul90ds      e ds
002200030625
002300941110     D* DS PER TRUL06R - CARICAMENTO £X
002400941110     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002500950314     D  T06                    1     90  0
002600950314     D                                     DIM(30)
002700920930     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002800000000     C*---------------------------------------------------------------*
002900030625     C* SE SONO IN MANUTENZIONE RICHIEDO LE STAMPANTI
003000030625     C                   CLEAR                   Trul90ds
003100941110     C*
003200030625     C                   MOVEL     'FAT888 '     D90MDB
003300030625     C                   MOVEL     'FAT888 '     D90MDB4
003400030625     C                   MOVEL     'FAT888 '     D90MDB5
003500030625     C                   MOVEL     'S'           D90RSB
003600030625     C                   CALL      'TRUL90R'
003700941110     C                   PARM                    KPJBA
003800030625     c                   Parm                    Trul90ds
003900941110     C*
004000941110     C**
004100941110     C* F3 - FINE
004200030625     C     D90F3         IFEQ      '1'
004300941110     C                   GOTO      FINE
004400941110     C                   ENDIF
004500920930     C*
004600900509     C     FOR01         TAG
004700941110     C                   EXFMT     LS18D01
004800920930     C** CMD 3 - FINE LAVORO
004900900509     C   KC              GOTO      FINE
005000900524     C* CONTROLLO
005100900524     C                   EXSR      CONTR
005200941111     C  NKF
005300900524     COR 90              GOTO      FOR01
005400900511     C* ELABORAZIONE
005500030625     C                   MOVEL     VIDDSD        i19GDA
005600030625     C                   MOVEL     VIDDSA        i19GAL
005700030625     C                   MOVEL     VIDFIL        i19FGS
005800941228     C********             MOVELSIMFEL    DS1FEL
005900030625     C                   MOVEL     VIDNFT        i19NFT
006000030625     C                   MOVEL     VIDLNP        i19LNP
006100030625     C                   MOVEL     D90MDB        i19MDB
006200030625     C                   MOVEL     D90PRB        i19PRB
006300030625     c                   Eval      i19mdb4 = d90mdb4
006400030625     c                   Eval      i19mdb5 = d90mdb5
006500030625     c                   Eval      i19psl  = d90psl
006600030625     c                   Eval      i19prb4 = d90prb4
006700030625     c                   Eval      i19prb5 = d90prb5
006800941229     C********             MOVEL'FNLS19R' DS1PGM
006900941228     C********             MOVEL'B'       DS1FTM
007000030625     C                   MOVEL     *BLANKS       i19FPG
007100941110     C*
007200030625     C**!!!              MOVEL     DSLS01        KPJBU
007300030625     c                   Movel(p)  Fnls19ds      Kpjbu
007400941110     C                   MOVEL     'LS19'        KCOAZ
007500030627     C                   CALL      'BCH10'
007600030627     c**!!!              Call      'FNLS19C'
007700900523     C                   PARM                    KPJBA
007800900523     C*
007900000000     C     FINE          TAG
008000000000     C                   SETON                                        LR
008100941228     C*
008200941228     C*--- OPERAZONI INIZIALI ----------------------------------------*
008300941110     C     *INZSR        BEGSR
008400941111     C*
008500941111     C     *ENTRY        PLIST
008600941111     C                   PARM                    KPJBA
008700941111     C                   Z-ADD     1             CODUT
008800941111     C                   CALL      'X§PARUT'
008900941111     C                   PARM                    UT§DSE
009000941111     C                   MOVEL     RAGUT         RSUT             20
009100941111     C                   MOVEL     REC80         CNCR80
009200941110     C*
009300941110     C                   Z-ADD     0             VIDDSD
009400941110     C                   Z-ADD     0             VIDDSA
009500941110     C                   Z-ADD     0             VIDNFT
009600941110     C***
009700941110     C* CARICO TABELLA FILIALI GESTITE £1
009800941110     C***
009900941110     C                   CLEAR                   DSUL06
010000941110     C                   MOVE      '£1'          D06COD
010100941110     C                   MOVEL     SIMFEL        D06KEY
010200941110     C                   MOVEL     'L'           D06TLA
010300941110     C                   MOVEL     DSUL06        KPJBU
010400941110     C*
010500941110     C                   CALL      'TRUL06R'
010600941110     C                   PARM                    KPJBA
010700941110     C                   MOVEL     KPJBU         DSUL06
010800950314     C                   MOVEA     T06           L1
010900941110     C*
011000020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
011100941110     C*
011200020429     C***        REM       IFEQ 'REM'
011300020429     C***        REMFIL    ANDGT*ZEROS
011400020429     C***                  MOVELREMFIL    VIDFIL  30
011500020429     C***                  SETON                     05
011600020429     C***                  ELSE
011700020429     C***                  MOVELSIMFEL    VIDFIL
011800020429     C***                  END
011900020429     C                   Z-ADD     SIMPOU        VIDFIL            3 0
012000020429     C     SIMTPP        IFEQ      '2'
012100020429     C     SIMTPP        OREQ      *BLANKS
012200020429     C                   SETON                                        05
012300020429     C                   ENDIF
012400941110     C                   MOVEL     VIDFIL        VIDLNP
012500941110     C*
012600941110     C                   ENDSR
012700900524     C*
012800900524     C*--- CONTROLLI FORMATO1 ----------------------------------------*
012900900524     C     CONTR         BEGSR
013000900524     C                   SETOFF                                       90
013100920930     C* DATA SPEDIZIONE DAL OBBLIGATORIA
013200920320     C                   MOVE      VIDDSD        G02DAT
013300920320     C                   MOVEL     *BLANK        G02ERR
013400941110     C                   CALL      'XSRDA8'
013500920320     C                   PARM                    WLBDAT
013600920320     C     G02ERR        IFEQ      '1'
013700920320     C                   SETON                                        40  90
013800920320     C                   GOTO      ENDCTR
013900920320     C                   END
014000030625     C                   Z-ADD     G02INV        i19ADA
014100941111     C                   Z-ADD     G02DAT        VIDDSD
014200900524     C*
014300900525     C* DATA SPEDIZIONE  DAL OBBLIGATORIA
014400030625     C                   CLEAR                   i19AAL
014500900524     C     VIDDSA        IFNE      0
014600920320     C                   MOVE      VIDDSA        G02DAT
014700920320     C                   MOVEL     *BLANK        G02ERR
014800941110     C                   CALL      'XSRDA8'
014900920320     C                   PARM                    WLBDAT
015000920320     C     G02ERR        IFEQ      '1'
015100920320     C                   SETON                                        41  90
015200920320     C                   GOTO      ENDCTR
015300920320     C                   END
015400030625     C                   Z-ADD     G02INV        i19AAL
015500941111     C                   Z-ADD     G02DAT        VIDDSA
015600900524     C* DATA DAL NO MAGGIORE DATA AL
015700030625     C     i19ADA        IFGT      i19AAL
015800900524     C                   SETON                                        4290
015900900524     C                   GOTO      ENDCTR
016000900524     C                   END
016100900524     C*
016200900524     C                   END
016300920930     C*
016400920930     C* CONTROLLO LINEA DI PARTENZA SE IMMESSA
016500920930     C     VIDLNP        IFNE      0
016600920930     C     VIDLNP        LOOKUP    L1                                     30
016700920930     C  N30              SETON                                        4390
016800920930     C  N30              GOTO      ENDCTR
016900920930     C                   END
017000920930     C*
017100900524     C     ENDCTR        ENDSR
