000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900605     F* TRTR80R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400900605     F* - AGGIORNA IL FILE CNABI00F RICEVUTO                         *
000500000000     F*--------------------------------------------------------------*
000600900605     FCNABI00R  UF   E             DISK    USROPN
000700900605     F                                     RENAME(CNABI000:CNABIR)
000800080326     FCNABI00F  UF A E           K DISK    USROPN
000900900517     FCMRIC01L  UF   E           K DISK
001000900514     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
001100900514     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
001200900514     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
001300900514     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)
001400000000     D KPJBA         E DS
001500900514     D  LIBSYS                92    101
001600900517     D  LEGFIL               483    492
001700900514     D  MBRFIL               493    502
001800000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
001900000000     D  TCU                  398    697
002000000000     D                                     DIM(50)                              TAB FLAG CNT
002100000000     D  KCU                  698    847P 0
002200000000     D                                     DIM(50)                              TAB CAPO CONTO
002300000000     D                                     PACKEVEN
002400000000     D  DTU                  848    895P 0
002500000000     D                                     DIM(12)                              DATE PARAM
002600000000     D                                     PACKEVEN
002700080326     D epoca           S               D   DATFMT(*YMD)
002800080326
002900900605     ICNABIR
003000900605     I              ABIABI                      RBIABI
003100900605     I              ABICAB                      RBICAB
003200900605     I              ABIIST                      RBIIST
003300900605     I              ABIAGE                      RBIAGE
003400900605     I              ABIND                       RBIND
003500900605     I              ABILOC                      RBILOC
003600900605     I              ABICOM                      RBICOM
003700900605     I              ABICAP                      RBICAP
003800900605     I              ABIPRO                      RBIPRO
003900900605     I              ABIDTR                      RBIDTR
004000900605     I              ABIFTR                      RBIFTR
004100900605     I              ABIANN                      RBIANN
004200080326
004300080326     C/EXEC SQL
004400080326     C+ SET OPTION DATFMT = *YMD, DATSEP = '/'
004500080326     C/END-EXEC
004600080326
004700900514     C                   EXSR      DEFCAM
004800080326     C                   EXSR      delAnnullate
004900900411     C*
005000080326     C                   OPEN      cnabi00f
005100900514     C                   DO        *HIVAL
005200900605     C                   READ      CNABI00R                               21
005300900514     C  N21              DO
005400900605     C     KTAB          CHAIN     CNABI00F                           54
005500900605     C                   MOVE      RBIABI        ABIABI
005600900605     C                   MOVE      RBICAB        ABICAB
005700900605     C                   MOVE      RBIIST        ABIIST
005800900605     C                   MOVE      RBIAGE        ABIAGE
005900900605     C                   MOVE      RBIND         ABIND
006000900605     C                   MOVE      RBILOC        ABILOC
006100900605     C                   MOVE      RBICOM        ABICOM
006200900605     C                   MOVE      RBICAP        ABICAP
006300900605     C                   MOVE      RBIPRO        ABIPRO
006400911023     C                   MOVE      RBIANN        ABIANN
006500940825     C                   Z-ADD     UDATE         ABIDTR
006600940825     C                   MOVEL     UYEAR         ABIDTR
006700940825     C                   MOVE      UDAY          ABIDTR
006800900605     C                   MOVE      'R'           ABIFTR
006900900605     C     *IN(54)       IFEQ      '1'
007000900605     C                   WRITE     CNABI000
007100900605     C                   DELETE    CNABIR
007200900514     C                   ADD       1             NUMREC            7 0
007300900514     C                   ELSE
007400080319     C*    RBIANN        IFNE      '*'
007500900605     C                   UPDATE    CNABI000
007600080319     C*                  ELSE
007700080319     C*                  DELETE    CNABI000
007800080319     C*                  END
007900900605     C                   DELETE    CNABIR
008000900514     C                   ADD       1             NUMREC
008100900514     C                   END
008200900411     C                   END
008300900514     C  N21              END
008400900514     C*
008500900514     C*
008600900514     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
008700900514     C*
008800900514     C                   EXSR      CAEXB
008900900514     C*
009000900517     C* AGGIORNA ARCHIVIO DI LOG
009100900517     C     LEGFIL        CHAIN     CMRIC01L                           20
009200900517     C  N20              Z-ADD     NUMREC        RICNR1
009300900517     C  N20              MOVE      '1'           RICFL2                          AGG.TO
009400900517     C  N20              UPDATE    CMRIC000
009500900514     C     STOP          TAG
009600900514     C                   SETON                                        LR
009700900411     C*
009800900411     C/EJECT
009900900411     C*---------------------------------------------------------------*
010000900411     C     DEFCAM        BEGSR
010100000000     C*---------------------------------------------------------------*
010200000000     C     *ENTRY        PLIST
010300000000     C                   PARM                    KPJBA
010400900524     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
010500900524     C* ASSUMO IL PRIMO MEMBRO DEL FILE
010600900524     C     MBRFIL        IFEQ      *BLANK
010700900524     C                   MOVEL     '*FIRST'      MBRFIL
010800900524     C                   END
010900000000     C                   Z-ADD     1             CODUT
011000000000     C                   CALL      'XPARUT'
011100000000     C                   PARM                    UTEDSE
011200000000     C     CODUT         CABEQ     -1            FINE
011300000000     C                   MOVEL     RAGUT         RSUT             20
011400891209     C*
011500891213     C* KLIST
011600891213     C*
011700900514     C     KTAB          KLIST
011800900605     C                   KFLD                    RBIABI
011900900605     C                   KFLD                    RBICAB
012000900514     C*
012100900514     C* APERTURA MEMBRO DA TRASCODIFICARE
012200900514     C*
012300900514     C                   EXSR      CAEXA
012400900514     C*
012500900412     C     FINE          ENDSR
012600900514     C/SPACE
012700900514     C**---------------------------------------------------------
012800900605     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI CNABI00R      -
012900900514     C**---------------------------------------------------------
013000900514     C     CAEXA         BEGSR
013100900514     C                   MOVE      ')'           VAR11            11
013200900514     C                   MOVEL     MBRFIL        VAR11
013300900514     C                   MOVE      '))'          VAR12            12
013400900514     C                   MOVEL     MBRFIL        VAR12
013500900514     C                   Z-ADD     55            LUNG             15 5
013600900514     C*
013700900514     C                   SETOFF                                       20
013800900514     C                   MOVE      VAR11         CMD1
013900900514     C                   MOVEL     *BLANKS       COMMAN
014000900514     C                   MOVEA     CMD1(1)       COMMAN           80
014100900514     C                   CALL      'QCMDEXC'                            20
014200900514     C                   PARM                    COMMAN
014300900514     C                   PARM                    LUNG
014400900514     C   20              GOTO      STOP
014500900514     C** CONTROLLO SE ESISTE IL MEMBRO
014600900514     C** SE NON ESISTE CHIUDO IL PROGRAMMA
014700900514     C                   SETOFF                                       20
014800900514     C                   MOVE      VAR12         CMD2
014900900514     C                   MOVEL     *BLANKS       COMMAN
015000900514     C                   MOVEA     CMD2(1)       COMMAN
015100900514     C                   CALL      'QCMDEXC'                            20
015200900514     C                   PARM                    COMMAN
015300900514     C                   PARM                    LUNG
015400900514     C   20              GOTO      STOP
015500900514     C** ALLOCO MEMBRO
015600900514     C** SE NON RIESCE CHIUDO IL PROGRAMMA
015700900514     C                   SETOFF                                       20
015800900514     C                   MOVE      VAR11         CMD3
015900900514     C                   MOVEL     *BLANKS       COMMAN
016000900514     C                   MOVEA     CMD3(1)       COMMAN
016100900514     C                   CALL      'QCMDEXC'                            20
016200900514     C                   PARM                    COMMAN
016300900514     C                   PARM                    LUNG
016400900514     C   20              GOTO      STOP
016500900514     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
016600900514     C** SE NON RIESCE CHIUDO IL PROGRAMMA
016700900605     C                   OPEN      CNABI00R
016800900514     C                   ENDSR
016900900514     C/SPACE 005
017000900514     C**---------------------------------------------------------
017100900605     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI CNABI00R      -
017200900514     C**---------------------------------------------------------
017300900514     C     CAEXB         BEGSR
017400900514     C                   MOVE      ')'           VAR11            11
017500900514     C                   MOVEL     MBRFIL        VAR11
017600900514     C                   MOVE      '))'          VAR12            12
017700900514     C                   MOVEL     MBRFIL        VAR12
017800900514     C                   Z-ADD     55            LUNG             15 5
017900900514     C*
018000940826     C                   CLOSE     CNABI00R                             20
018100940825     C** RIORGANIZZA IL MEMBRO  ELIMINATE SPECIFICHE
018200940826     C  N20              DO
018300900514     C                   MOVE      VAR12         CMD5
018400900514     C                   MOVEL     *BLANKS       COMMAN
018500900514     C                   MOVEA     CMD5(1)       COMMAN
018600900514     C                   CALL      'QCMDEXC'
018700900514     C                   PARM                    COMMAN
018800900514     C                   PARM                    LUNG
018900940826     C                   END
019000900514     C** DISALLOCA IL MEMBRO AGG.TO
019100900514     C                   ENDSR
019200080326
019300080326     ***************************************************************************
019400080326     **
019500080326     ** Cancellazione delle righe annullate da almeno 1 mese.
019600080326     **
019700080326     ***************************************************************************
019800080326     C     delAnnullate  BEGSR
019900080326     C                   EVAL      epoca = %DATE() - %MONTHS(1)
020000080326     C/EXEC SQL
020100080326     C+ DELETE FROM CNABI00F
020200080326     C+ WHERE ABIANN <> ''
020300080326     C+   AND ABIFTR = 'R'
020400080326     C+   AND ABIDTR > 0
020500080326     C+   AND DATE(SUBSTR(DIGITS(ABIDTR), 1, 2) CONCAT '/' CONCAT
020600080326     C+            SUBSTR(DIGITS(ABIDTR), 3, 2) CONCAT '/' CONCAT
020700080326     C+            SUBSTR(DIGITS(ABIDTR), 5, 2)
020800080326     C+           ) < :epoca
020900080326     C/END-EXEC
021000080326     C                   IF        sqlCode < 0
021100080326     C                   DUMP(A)
021200080326     C                   ENDIF
021300080326     C                   ENDSR
021400080326
021500900605**         CMD1
021600900605CHKOBJ OBJ(CNABI00R) OBJTYPE(*FILE)     MBR(M234567890)
021700900514**         CMD2
021800900605ALCOBJ OBJ((CNABI00R *FILE *EXCL           M234567890))
021900900514**         CMD3
022000900605OVRDBF FILE(CNABI00R)                   MBR(M234567890)
022100900514**         CMD5
022200900605DLCOBJ OBJ((CNABI00R *FILE *EXCL           M234567890))
