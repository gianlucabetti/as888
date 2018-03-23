000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300030708      * FNLRF7R3*                                                    *
000400950524      *---------*                                                    *
000500030708      *         - CALCOLO STOP                    -                  *
000600950524      *--------------------------------------------------------------*
000700020916     FFNARB70L  IF   E           K DISK
000800990921     FFIAR601L  IF   E           K DISK
000900950525     FFNARBY1L  IF A E           K DISK
001000950526     FFNARBZ0F  IF A E           K DISK
001100951211     FTABEL00F  IF   E           K DISK    USROPN
001200951211     D*--------------------------------------------------------------*
001300951211     D*  SCHIERE
001400951211     D*--------------------------------------------------------------*
001500951211     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
001600951211     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
001700950524     D*--------------------------------------------------------------*
001800950524     D*  DS
001900950524     D*--------------------------------------------------------------*
002000950524     D KPJBA         E DS
002100950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
002200950526     D DS3A          E DS
002300950524     C*--------------------------------------------------------------*
002400950524     C*  CICLO PRINCIPALE
002500950524     C*--------------------------------------------------------------*
002600950524     C     *ENTRY        PLIST
002700950524     C                   PARM                    KPJBA
002800950524     C                   MOVEL     KPJBU         FNLR59
002900030721     c*
003000030721     C     D59TPR        IFeq      'U'
003100030721     C                   SETON                                        LR
003200030721     c                   return
003300030721     C                   END
003400030721     c*
003500950526     C                   Z-ADD     0             D59STO
003600950524     C*
003700030708     C*  Controllo ed eventualmente sfleggo i record di FNARB che hanno
003800950524     C*  il numero distinta uguale a quella che sto preparando.
003900020916     C                   Z-ADD     D59FGS        KFGS
004000020916     C                   Z-ADD     D59NDT        KNDC
004100960702     C                   Z-ADD     D59DDT        KDDC
004200030721     C     KARB1         setll     FNARB70L
004300030721     C                   DO        *hival
004400030721     C     KARB1         READE     FNARB70L
004500030721     c                   if        %eof(fnarb70l)
004600030721     c                   leave
004700030721     c                   end
004800950524     C                   Z-ADD     0             ARBNSS
005000070530     C*  Se la spedizione è stata inserita a mano oppure
005001070530     C*  in automatica ma non dal pgm "spunte"  devo scriverla
005100950524     C*  sul file di Work
005200950524     C                   Z-ADD     ARBAAS        KAAS
005300950524     C                   Z-ADD     ARBLNP        KLNP
005400950524     C                   Z-ADD     ARBNRS        KNRS
005500950524     C                   Z-ADD     ARBNSP        KNSP
005600950525     C     KARB          CHAIN     FNARBY1L                           32
005700950524     C     *IN32         IFEQ      '1'
005800950524     C                   WRITE     FNARBY00
005900950524     C                   CLEAR                   FNARBY00
006000950524     C                   END
006200070601     C*  carico gli stop nel file
006300950526     C     ARBSTP        IFNE      0
006400950526     C                   EXSR      WRTSTP
006600950526     C                   END
006700030721     C                   ENDdo
006800950524     C*
006900950526     C                   MOVEL     FNLR59        KPJBU
007000950524     C                   SETON                                        RT
007100950526     C*--------------------------------------------------------------*
007200950526     C*  Scrittura degli stop su apposito archivio
007300950526     C*--------------------------------------------------------------*
007400950526     C     WRTSTP        BEGSR
007500951211     C*
007600951211     C                   Z-ADD     0             ARBKCS
007700990921     C                   Z-ADD     0             AR6KSC
007800951211     C                   Z-ADD     1             X                 2 0
007900951211     C                   MOVEL     *BLANKS       WTBOL1            1
008000951211     C                   CLEAR                   DS3A
008100951211     C*  Controllo se cliente è assegnato
008200951211     C     ARBCBO        LOOKUP    C3A(X)                                 13
008300951211     C     *IN13         IFEQ      '1'
008400951211     C                   MOVEL     D3A(X)        DS3A
008500951211     C                   MOVEL     §3ATB1        WTBOL1            1
008600951211     C                   END
008700951211     C                   Z-ADD     ARBKSC        KKSC              7 0
008800990921     C     WTBOL1        IFEQ      'A'
008900990921     C     §3ATB2        ORNE      *BLANKS
009000990921     C     KARB          CHAIN     FIAR601L                           74
009100990921     C  N74              Z-ADD     AR6KSC        KKSC
009200951211     C                   END
009300030708     C*  Calcolo stop in base cod.cliente assegnato
009400951211     C                   Z-ADD     KKSC          APPOG             4 0
009500951211     C     APPOG         IFNE      8888
009600951211     C     APPOG         ANDNE     9999
009700951211     C     WTBOL1        ANDNE     'F'
009800951211     C     APPOG         ORNE      8888
009900951211     C     APPOG         ANDNE     9999
010000990921     C     AR6KSC        ANDNE     0
010100951211     C                   MOVEL     KKSC          KRAG
010200951211     C                   Z-ADD     ARBSTP        KSTP
010300951211     C     KARBZ         CHAIN     FNARBZ0F                           32
010400951211     C*  Se non trovato scrivo nuovo stop
010500951211     C     *IN32         IFEQ      '1'
010600951211     C                   Z-ADD     KKSC          ARBKCS
010700951211     C                   MOVEL     KKSC          ARBRAG
010800951211     C                   Z-ADD     ARBSTP        ARBSTP
010900951211     C                   WRITE     FNARBZ00
011000951211     C                   END
011100030708     C*  Calcolo stop in base rag.sociale + indirizzo destinatario
011200951211     C                   ELSE
011300950526     C                   Z-ADD     ARBSTP        KSTP
011400951121     C                   MOVEL     ARBRSD        KRAG
011500951121     C                   MOVE      ARBIND        KRAG
011600950526     C     KARBZ         CHAIN     FNARBZ0F                           32
011700950526     C*  Se non trovato scrivo nuovo stop
011800950526     C     *IN32         IFEQ      '1'
011900951121     C                   MOVEL     KRAG          ARBRAG
012000950526     C                   Z-ADD     ARBSTP        ARBSTP
012100950526     C                   WRITE     FNARBZ00
012200950526     C                   END
012300951211     C                   END
012400950526     C*  Memorizzo stop + alto
012500950526     C     ARBSTP        IFGT      D59STO
012600950526     C                   Z-ADD     ARBSTP        D59STO
012700950526     C                   END
012800950526     C*
012900950526     C                   ENDSR
013000950524     C*--------------------------------------------------------------*
013100950524     C*  Operazioni iniziali
013200950524     C*--------------------------------------------------------------*
013300950524     C     *INZSR        BEGSR
013400950524     C*
013500950524     C     KARB          KLIST
013600950524     C                   KFLD                    KAAS
013700950524     C                   KFLD                    KLNP
013800950524     C                   KFLD                    KNRS
013900950524     C                   KFLD                    KNSP
014000960702     C     KARB1         KLIST
014100020916     C                   KFLD                    KFGS
014200020916     C                   KFLD                    KNDC
014300960702     C                   KFLD                    KDDC
014400950526     C     KARBZ         KLIST
014500951121     C                   KFLD                    KRAG
014600950526     C                   KFLD                    KSTP
014700951211     C     KTAB          KLIST
014800951211     C                   KFLD                    KKUT
014900951211     C                   KFLD                    KCOD
015000950524     C*
015100960702     C     *LIKE         DEFINE    ARBDDC        KDDC
015200960702     C     *LIKE         DEFINE    ARBNDC        KNDC
015300020916     C     *LIKE         DEFINE    ARBIFP        KFGS
015400950524     C     *LIKE         DEFINE    ARBAAS        KAAS
015500950524     C     *LIKE         DEFINE    ARBLNP        KLNP
015600950524     C     *LIKE         DEFINE    ARBNRS        KNRS
015700950524     C     *LIKE         DEFINE    ARBNSP        KNSP
015800951121     C     *LIKE         DEFINE    ARBRAG        KRAG
015900950526     C     *LIKE         DEFINE    ARBSTP        KSTP
016000951211     C     *LIKE         DEFINE    TBLKUT        KKUT
016100951211     C     *LIKE         DEFINE    TBLCOD        KCOD
016200950526     c*
016300950526     C                   CLEAR                   FNARBY00
016400951211     C*
016500951211     C*  Carico dati tabella 3A
016600951211     C                   OPEN      TABEL00F
016700951211     C                   Z-ADD     1             KKUT
016800951211     C                   MOVEL     '3A'          KCOD
016900951211     C                   Z-ADD     0             X
017000951211     C     KTAB          CHAIN     TABEL00F                           31
017100951211     C     *IN31         DOWEQ     '0'
017200951211     C     X             ANDLT     100
017300951211     C                   ADD       1             X
017400951211     C                   MOVEL     TBLKEY        C3A(X)
017500951211     C                   MOVEL     TBLUNI        D3A(X)
017600951211     C     KTAB          READE     TABEL00F                               31
017700951211     C                   END
017800951211     C                   CLOSE     TABEL00F
017900950524     C*
018000950524     C                   ENDSR
