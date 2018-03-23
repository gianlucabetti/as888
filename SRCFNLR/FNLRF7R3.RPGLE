000100950524     H DECEDIT('0,') DATEDIT(*DMY.)
000200950524      *---------*----------------------------------------------------*
000300030708      * FNLRF7R3*                                                    *
000400950524      *---------*                                                    *
000500030708      *         - CALCOLO STOP                    -                  *
000600950524      *--------------------------------------------------------------*
000700020916     FFNARB70L  IF   E           K DISK
000800950525     FFNARBY1L  IF A E           K DISK
000900140507      *--
001000140507     FFIAR601L  IF   E           K DISK    usropn
001100140507     FFNARBZ0F  IF A E           K DISK    usropn
001200140507      *--
001300951211     FTABEL00F  IF   E           K DISK    USROPN
001400951211     D*--------------------------------------------------------------*
001500951211     D*  SCHIERE
001600951211     D*--------------------------------------------------------------*
001700951211     D C3A             S              2    DIM(100)                             CODICE TABELLA 3A
001800951211     D D3A             S             73    DIM(100)                             FLAG. TBLUNI  3A
001900950524     D*--------------------------------------------------------------*
002000950524     D*  DS
002100950524     D*--------------------------------------------------------------*
002200950524     D KPJBA         E DS
002300950524     D FNLR59        E DS                  EXTNAME(FNLR59DS)
002400950526     D DS3A          E DS
002500950524     C*--------------------------------------------------------------*
002600950524     C*  CICLO PRINCIPALE
002700950524     C*--------------------------------------------------------------*
002800950524     C     *ENTRY        PLIST
002900950524     C                   PARM                    KPJBA
003000950524     C                   MOVEL     KPJBU         FNLR59
003100030721     c*
003200030721     C     D59TPR        IFeq      'U'
003300030721     C                   SETON                                        LR
003400030721     c                   return
003500030721     C                   END
003600030721     c*
003700950526     C                   Z-ADD     0             D59STO
003800950524     C*
003900030708     C*  Controllo ed eventualmente sfleggo i record di FNARB che hanno
004000950524     C*  il numero distinta uguale a quella che sto preparando.
004100020916     C                   Z-ADD     D59FGS        KFGS
004200020916     C                   Z-ADD     D59NDT        KNDC
004300960702     C                   Z-ADD     D59DDT        KDDC
004400030721     C     KARB1         setll     FNARB70L
004500030721     C                   DO        *hival
004600030721     C     KARB1         READE     FNARB70L
004700030721     c                   if        %eof(fnarb70l)
004800030721     c                   leave
004900030721     c                   end
005000950524     C                   Z-ADD     0             ARBNSS
005100070530     C*  Se la spedizione è stata inserita a mano oppure
005200070530     C*  in automatica ma non dal pgm "spunte"  devo scriverla
005300950524     C*  sul file di Work
005400950524     C                   Z-ADD     ARBAAS        KAAS
005500950524     C                   Z-ADD     ARBLNP        KLNP
005600950524     C                   Z-ADD     ARBNRS        KNRS
005700950524     C                   Z-ADD     ARBNSP        KNSP
005800950525     C     KARB          CHAIN     FNARBY1L                           32
005900950524     C     *IN32         IFEQ      '1'
006000950524     C                   WRITE     FNARBY00
006100950524     C                   CLEAR                   FNARBY00
006200950524     C                   END
006300140507      *
006400140507     c*  Se richiamato da azione LR59 o LR60 DEVE  utilizzare e fare il calcolo degli STOP.
006500140507   <-c                   if        kcDAZ = 'LR59'  or
006600140507   <-c                             kcDAZ = 'LR60'
006700140507      *
006800070601     C*  carico gli stop nel file
006900950526     C     ARBSTP        IFNE      0
007000950526     C                   EXSR      WRTSTP
007100950526     C                   END
007200140507     c*
007300140507   <-c                   end
007400140507     c*
007500030721     C                   ENDdo
007600950524     C*
007700950526     C                   MOVEL     FNLR59        KPJBU
007800950524     C                   SETON                                        RT
007900950526     C*--------------------------------------------------------------*
008000950526     C*  Scrittura degli stop su apposito archivio
008100950526     C*--------------------------------------------------------------*
008200950526     C     WRTSTP        BEGSR
008300951211     C*
008400951211     C                   Z-ADD     0             ARBKCS
008500990921     C                   Z-ADD     0             AR6KSC
008600951211     C                   Z-ADD     1             X                 2 0
008700951211     C                   MOVEL     *BLANKS       WTBOL1            1
008800951211     C                   CLEAR                   DS3A
008900951211     C*  Controllo se cliente è assegnato
009000951211     C     ARBCBO        LOOKUP    C3A(X)                                 13
009100951211     C     *IN13         IFEQ      '1'
009200951211     C                   MOVEL     D3A(X)        DS3A
009300951211     C                   MOVEL     §3ATB1        WTBOL1            1
009400951211     C                   END
009500951211     C                   Z-ADD     ARBKSC        KKSC              7 0
009600990921     C     WTBOL1        IFEQ      'A'
009700990921     C     §3ATB2        ORNE      *BLANKS
009800990921     C     KARB          CHAIN     FIAR601L                           74
009900990921     C  N74              Z-ADD     AR6KSC        KKSC
010000951211     C                   END
010100030708     C*  Calcolo stop in base cod.cliente assegnato
010200951211     C                   Z-ADD     KKSC          APPOG             4 0
010300951211     C     APPOG         IFNE      8888
010400951211     C     APPOG         ANDNE     9999
010500951211     C     WTBOL1        ANDNE     'F'
010600951211     C     APPOG         ORNE      8888
010700951211     C     APPOG         ANDNE     9999
010800990921     C     AR6KSC        ANDNE     0
010900951211     C                   MOVEL     KKSC          KRAG
011000951211     C                   Z-ADD     ARBSTP        KSTP
011100951211     C     KARBZ         CHAIN     FNARBZ0F                           32
011200951211     C*  Se non trovato scrivo nuovo stop
011300951211     C     *IN32         IFEQ      '1'
011400951211     C                   Z-ADD     KKSC          ARBKCS
011500951211     C                   MOVEL     KKSC          ARBRAG
011600951211     C                   Z-ADD     ARBSTP        ARBSTP
011700951211     C                   WRITE     FNARBZ00
011800951211     C                   END
011900030708     C*  Calcolo stop in base rag.sociale + indirizzo destinatario
012000951211     C                   ELSE
012100950526     C                   Z-ADD     ARBSTP        KSTP
012200951121     C                   MOVEL     ARBRSD        KRAG
012300951121     C                   MOVE      ARBIND        KRAG
012400950526     C     KARBZ         CHAIN     FNARBZ0F                           32
012500950526     C*  Se non trovato scrivo nuovo stop
012600950526     C     *IN32         IFEQ      '1'
012700951121     C                   MOVEL     KRAG          ARBRAG
012800950526     C                   Z-ADD     ARBSTP        ARBSTP
012900950526     C                   WRITE     FNARBZ00
013000950526     C                   END
013100951211     C                   END
013200950526     C*  Memorizzo stop + alto
013300950526     C     ARBSTP        IFGT      D59STO
013400950526     C                   Z-ADD     ARBSTP        D59STO
013500950526     C                   END
013600950526     C*
013700950526     C                   ENDSR
013800950524     C*--------------------------------------------------------------*
013900950524     C*  Operazioni iniziali
014000950524     C*--------------------------------------------------------------*
014100950524     C     *INZSR        BEGSR
014200950524     C*
014300950524     C     KARB          KLIST
014400950524     C                   KFLD                    KAAS
014500950524     C                   KFLD                    KLNP
014600950524     C                   KFLD                    KNRS
014700950524     C                   KFLD                    KNSP
014800960702     C     KARB1         KLIST
014900020916     C                   KFLD                    KFGS
015000020916     C                   KFLD                    KNDC
015100960702     C                   KFLD                    KDDC
015200950526     C     KARBZ         KLIST
015300951121     C                   KFLD                    KRAG
015400950526     C                   KFLD                    KSTP
015500951211     C     KTAB          KLIST
015600951211     C                   KFLD                    KKUT
015700951211     C                   KFLD                    KCOD
015800950524     C*
015900960702     C     *LIKE         DEFINE    ARBDDC        KDDC
016000960702     C     *LIKE         DEFINE    ARBNDC        KNDC
016100020916     C     *LIKE         DEFINE    ARBIFP        KFGS
016200950524     C     *LIKE         DEFINE    ARBAAS        KAAS
016300950524     C     *LIKE         DEFINE    ARBLNP        KLNP
016400950524     C     *LIKE         DEFINE    ARBNRS        KNRS
016500950524     C     *LIKE         DEFINE    ARBNSP        KNSP
016600951121     C     *LIKE         DEFINE    ARBRAG        KRAG
016700950526     C     *LIKE         DEFINE    ARBSTP        KSTP
016800951211     C     *LIKE         DEFINE    TBLKUT        KKUT
016900951211     C     *LIKE         DEFINE    TBLCOD        KCOD
017000950526     c*
017100950526     C                   CLEAR                   FNARBY00
017200951211     C*
017300140507     c*  Se richiamato da azione LR59 o LR60 DEVE utilizzare e fare il calcolo degli STOP.
017400140512     c*   Quindi utilizza la tabella e apriamo anche i files.
017500140507   <-c                   if        kcDAZ = 'LR59'  or
017600140507   <-c                             kcDAZ = 'LR60'
017700140512      *
017800140512   <-c                   open      fnarbZ0F                             99
017900140512   <-c                   open      fiar601L                             99
018000140507      *
018100951211     C*  Carico dati tabella 3A
018200951211     C                   OPEN      TABEL00F
018300951211     C                   Z-ADD     1             KKUT
018400951211     C                   MOVEL     '3A'          KCOD
018500951211     C                   Z-ADD     0             X
018600951211     C     KTAB          CHAIN     TABEL00F                           31
018700951211     C     *IN31         DOWEQ     '0'
018800951211     C     X             ANDLT     100
018900951211     C                   ADD       1             X
019000951211     C                   MOVEL     TBLKEY        C3A(X)
019100951211     C                   MOVEL     TBLUNI        D3A(X)
019200951211     C     KTAB          READE     TABEL00F                               31
019300951211     C                   END
019400951211     C                   CLOSE     TABEL00F
019500950524     C*
019600140507   <-c                   end
019700140507     C*
019800950524     C                   ENDSR
