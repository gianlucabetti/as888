000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500020722     FTEETC00F  UF   E             DISK
000600020722     D*------------
000700020722     D* VARIABILI DI WRK
000800020722     D*------------
000900020722     D YESUPD          S              1
001000020722     D CNTUPD          S              9  0 INZ
001100000000     C*------------------------------------------------------------------------*
001200000000     C* MAIN LINE
001300000000     C*------------------------------------------------------------------------*
001400010808     C*
001500020722     C                   EXSR      EXEUPD
001600010829     C*
001700010808     C                   SETON                                            LR
001800010808     C*------------------------------------------------------------------------*
001900020722     C* EXEUPD - ESEGUE OPERAZIONI DI AGGIORNAMENTO
002000010808     C*------------------------------------------------------------------------*
002100020722     C     EXEUPD        BEGSR
002200010808     C*
002300020722     C                   READ      TEETC00F
002400010808     C*
002500020722     C                   DOW       not %eof(TEETC00F)
002600011029     C*
002700011029     C*******************
002800011029     C* T E E T C 0 0 F
002900011029     C*******************
003000020722     C                   MOVEL     '0'           YESUPD
003100011029     C*
003200020722     C* Verifico quali campi sono da aggiornare
003300020722     C                   IF        ETCSIN = *blanks
003400020722     C                   EVAL      ETCSIN = 'N'
003500020722     C                   EVAL      YESUPD = '1'
003600020722     C                   ENDIF
003700020722     C                   IF        ETCIES = *blanks
003800020722     C                   EVAL      ETCIES = 'N'
003900020722     C                   EVAL      YESUPD = '1'
004000020722     C                   ENDIF
004100020722     C                   IF        ETCICA = *blanks
004200020722     C                   EVAL      ETCICA = 'N'
004300020722     C                   EVAL      YESUPD = '1'
004400020722     C                   ENDIF
004500020722     C                   IF        ETCIAS = *blanks
004600020722     C                   EVAL      ETCIAS = 'N'
004700020722     C                   EVAL      YESUPD = '1'
004800020722     C                   ENDIF
004900020722     C                   IF        ETCIGI = *blanks
005000020722     C                   EVAL      ETCIGI = 'N'
005100020722     C                   EVAL      YESUPD = '1'
005200020722     C                   ENDIF
005300020722     C                   IF        ETCIAP = *blanks
005400020722     C                   EVAL      ETCIAP = 'N'
005500020722     C                   EVAL      YESUPD = '1'
005600020722     C                   ENDIF
005700020722     C                   IF        ETCIPI = *blanks
005800020722     C                   EVAL      ETCIPI = 'N'
005900020722     C                   EVAL      YESUPD = '1'
006000020722     C                   ENDIF
006100020722     C                   IF        ETCISU = *blanks
006200020722     C                   EVAL      ETCISU = 'N'
006300020722     C                   EVAL      YESUPD = '1'
006400020722     C                   ENDIF
006500020722     C                   IF        ETCIFU = *blanks
006600020722     C                   EVAL      ETCIFU = 'N'
006700020722     C                   EVAL      YESUPD = '1'
006800020722     C                   ENDIF
006900020722     C                   IF        ETCIDD = *blanks
007000020722     C                   EVAL      ETCIDD = 'N'
007100020722     C                   EVAL      YESUPD = '1'
007200020722     C                   ENDIF
007300020722     C                   IF        ETCICE = *blanks
007400020722     C                   EVAL      ETCICE = 'N'
007500020722     C                   EVAL      YESUPD = '1'
007600020722     C                   ENDIF
007700020722     C                   IF        ETCILO = *blanks
007800020722     C                   EVAL      ETCILO = 'N'
007900020722     C                   EVAL      YESUPD = '1'
008000020722     C                   ENDIF
008100020722     C                   IF        ETCISO = *blanks
008200020722     C                   EVAL      ETCISO = 'N'
008300020722     C                   EVAL      YESUPD = '1'
008400020722     C                   ENDIF
008500020722     C                   IF        ETCINR = *blanks
008600020722     C                   EVAL      ETCINR = 'N'
008700020722     C                   EVAL      YESUPD = '1'
008800020722     C                   ENDIF
008900020722     C                   IF        ETCIX1 = 'N'
009000020722     C                   EVAL      ETCIX1 = *blanks
009100020722     C                   EVAL      YESUPD = '1'
009200020722     C                   ENDIF
009300020722     C                   IF        ETCIX2 = 'N'
009400020722     C                   EVAL      ETCIX2 = *blanks
009500020722     C                   EVAL      YESUPD = '1'
009600020722     C                   ENDIF
009700020722     C*
009800020722     C                   IF        YESUPD = '1'
009900011029     C                   UPDATE    TEETC000
010000020722     C                   ADD       1             CNTUPD
010100020722     C                   ENDIF
010200011029     C*
010300020722     C                   READ      TEETC00F
010400010808     C*
010500020722     C                   ENDDO
010600010808     C*
010700010808     C                   ENDSR
010800010808     C*------------------------------------------------------------------------*
010900000000     C* *INZSR - OPERAZIONI INIZIALI
011000000000     C*------------------------------------------------------------------------*
011100000000     C     *INZSR        BEGSR
011200010829     C*
011300020722     C*
011400010829     C                   ENDSR
