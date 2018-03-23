000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900514     F* TRTR56R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400900514     F* - AGGIORNA IL FILE TABELLE RICEVUTO                          *
000500000000     F*--------------------------------------------------------------*
000600900514     FTABEL00R  UF   E             DISK    USROPN
000700900514     F                                     RENAME(TABEL:TABELR)
000800900514     FTABEL00F  UF A E           K DISK
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
002700900514     ITABELR
002800900514     I              TBLKUT                      RBLKUT
002900900514     I              TBLCOD                      RBLCOD
003000900514     I              TBLKEY                      RBLKEY
003100900514     I              TBLFLG                      RBLFLG
003200900514     I              TBLUNI                      RBLUNI
003300900514     I              TBLFTT                      RBLFTT
003400900514     I              TBLFLT                      RBLFLT
003500900514     I              TBLFTR                      RBLFTR
003600900514     I              TBLDTR                      RBLDTR
003700000000     I/SPACE 3
003800900411     C*
003900900514     C                   EXSR      DEFCAM
004000900411     C*
004100900514     C                   DO        *HIVAL
004200900514     C                   READ      TABEL00R                               21
004300900514     C  N21              DO
004400941018     C*
004500941018     C* TABELLA DELLE TABELLE:
004600941018     C  N58RBLCOD        COMP      '  '                                   58
004700941018     C*
004800900514     C     KTAB          CHAIN     TABEL                              54
004900900514     C     *IN(54)       IFEQ      '1'
005000980420     C* NEL CASO IN CUI SI TRATTI DI TABELLA 8B O 8C CONTROLLO SE ESISTE
005100980420     C* GIA' QUEL RECORD PER L'AREA METTENDO NELLA SECONDA PARTE DELLA
005200980420     C* CHIAVE LA FILIALE 000
005300980420     C     RBLCOD        IFEQ      '8B'
005400980420     C     RBLCOD        OREQ      '8C'
005500980420     C                   MOVEL     RBLKEY        BLKEY2
005600980420     C                   MOVE      '000'         BLKEY2
005700980420     C     KTAB2         CHAIN     TABEL                              54
005800980420     C                   END
005900980420     C                   END
006000980420     C     *IN(54)       IFEQ      '1'
006100980417     C*
006200900629     C                   MOVEL     RBLKUT        TBLKUT
006300900514     C                   MOVEL     RBLCOD        TBLCOD
006400900514     C                   MOVEL     RBLKEY        TBLKEY
006500900514     C                   MOVEL     RBLFLG        TBLFLG
006600900514     C                   MOVEL     RBLUNI        TBLUNI
006700900514     C                   MOVEL     RBLFTT        TBLFTT
006800900514     C                   MOVEL     RBLFLT        TBLFLT
006900900514     C                   MOVEL     'R'           TBLFTR
007000900514     C                   MOVEL     NUMTRA        TBLDTR
007100941013     C     RBLFLG        IFNE      '*'                                          ANN.TA
007200900514     C                   WRITE     TABEL
007300941013     C                   END
007400900514     C                   DELETE    TABELR
007500900514     C                   ADD       1             NUMREC            7 0
007600900514     C                   ELSE
007700910411     C     RBLFLG        IFNE      '*'                                          ANN.TA
007800900514     C                   MOVEL     RBLFLG        TBLFLG
007900900514     C                   MOVEL     RBLUNI        TBLUNI
008000900514     C                   MOVEL     RBLFTT        TBLFTT
008100900514     C                   MOVEL     RBLFLT        TBLFLT
008200900514     C                   MOVEL     'R'           TBLFTR
008300900514     C                   MOVEL     NUMTRA        TBLDTR
008400900514     C                   UPDATE    TABEL
008500910411     C                   ELSE
008600910411     C                   DELETE    TABEL
008700910411     C                   END
008800900514     C                   DELETE    TABELR
008900900514     C                   ADD       1             NUMREC
009000900514     C                   END
009100900411     C                   END
009200900514     C  N21              END
009300900514     C*
009400900514     C*
009500900514     C* CHIUSURA E RIORGANIZZAZIONE MEMBRO TRASCODIFICATO
009600900514     C*
009700900514     C                   EXSR      CAEXB
009800900514     C*
009900900517     C* AGGIORNA ARCHIVIO DI LOG
010000900517     C     LEGFIL        CHAIN     CMRIC01L                           20
010100900517     C  N20              Z-ADD     NUMREC        RICNR1
010200900517     C  N20              MOVE      '1'           RICFL2                          AGG.TO
010300900517     C  N20              UPDATE    CMRIC000
010400900514     C     STOP          TAG
010500941018     C   58              EXSR      DELANN
010600900514     C                   SETON                                        LR
010700900411     C*
010800900411     C/EJECT
010900900411     C*---------------------------------------------------------------*
011000900411     C     DEFCAM        BEGSR
011100000000     C*---------------------------------------------------------------*
011200000000     C     *ENTRY        PLIST
011300000000     C                   PARM                    KPJBA
011400900524     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
011500900524     C* ASSUMO IL PRIMO MEMBRO DEL FILE
011600900524     C     MBRFIL        IFEQ      *BLANK
011700900524     C                   MOVEL     '*FIRST'      MBRFIL
011800900524     C                   END
011900000000     C                   Z-ADD     1             CODUT
012000000000     C                   CALL      'XPARUT'
012100000000     C                   PARM                    UTEDSE
012200000000     C     CODUT         CABEQ     -1            FINE
012300000000     C                   MOVEL     RAGUT         RSUT             20
012400891209     C*
012500891213     C* KLIST
012600891213     C*
012700900514     C     KTAB          KLIST
012800900514     C                   KFLD                    CODUT
012900900514     C                   KFLD                    RBLCOD
013000900514     C                   KFLD                    RBLKEY
013100980420     C*
013200980420     C     KTAB2         KLIST
013300980420     C                   KFLD                    CODUT
013400980420     C                   KFLD                    RBLCOD
013500980420     C                   KFLD                    BLKEY2            8
013600900514     C*
013700900514     C* APERTURA MEMBRO DA TRASCODIFICARE
013800900514     C*
013900900514     C                   EXSR      CAEXA
014000900514     C*
014100941013     C*
014200941013     C                   MOVE      *ZERO         NUMTRA            6 0
014300941013     C                   Z-ADD     UDATE         NUMTRA                         *
014400941013     C                   MOVE      UDAY          NUMTRA                         *
014500941013     C                   MOVEL     UYEAR         NUMTRA                         *
014600941013     C*NUMERO TRASMISSIONE = UDATE USA
014700900412     C     FINE          ENDSR
014800900514     C/SPACE
014900900514     C**---------------------------------------------------------
015000900514     C** ESEGUE QCAEXEC DI APERTURA SUL MEMBRO DI TABEL00R      -
015100900514     C**---------------------------------------------------------
015200900514     C     CAEXA         BEGSR
015300900514     C                   MOVE      ')'           VAR11            11
015400900514     C                   MOVEL     MBRFIL        VAR11
015500900514     C                   MOVE      '))'          VAR12            12
015600900514     C                   MOVEL     MBRFIL        VAR12
015700900514     C                   Z-ADD     55            LUNG             15 5
015800900514     C*
015900900514     C                   SETOFF                                       20
016000900514     C                   MOVE      VAR11         CMD1
016100900514     C                   MOVEL     *BLANKS       COMMAN
016200900514     C                   MOVEA     CMD1(1)       COMMAN           80
016300900514     C                   CALL      'QCMDEXC'                            20
016400900514     C                   PARM                    COMMAN
016500900514     C                   PARM                    LUNG
016600900514     C   20              GOTO      STOP
016700900514     C** CONTROLLO SE ESISTE IL MEMBRO
016800900514     C** SE NON ESISTE CHIUDO IL PROGRAMMA
016900900514     C                   SETOFF                                       20
017000900514     C                   MOVE      VAR12         CMD2
017100900514     C                   MOVEL     *BLANKS       COMMAN
017200900514     C                   MOVEA     CMD2(1)       COMMAN
017300900514     C                   CALL      'QCMDEXC'                            20
017400900514     C                   PARM                    COMMAN
017500900514     C                   PARM                    LUNG
017600900514     C   20              GOTO      STOP
017700900514     C** ALLOCO MEMBRO
017800900514     C** SE NON RIESCE CHIUDO IL PROGRAMMA
017900900514     C                   SETOFF                                       20
018000900514     C                   MOVE      VAR11         CMD3
018100900514     C                   MOVEL     *BLANKS       COMMAN
018200900514     C                   MOVEA     CMD3(1)       COMMAN
018300900514     C                   CALL      'QCMDEXC'                            20
018400900514     C                   PARM                    COMMAN
018500900514     C                   PARM                    LUNG
018600900514     C   20              GOTO      STOP
018700900514     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
018800900514     C** SE NON RIESCE CHIUDO IL PROGRAMMA
018900900514     C                   OPEN      TABEL00R
019000900514     C                   ENDSR
019100900514     C/SPACE 005
019200900514     C**---------------------------------------------------------
019300900514     C** ESEGUE QCAEXEC DI CHIUSURA SUL MEMBRO DI TABEL00R      -
019400900514     C**---------------------------------------------------------
019500900514     C     CAEXB         BEGSR
019600900514     C                   MOVE      ')'           VAR11            11
019700900514     C                   MOVEL     MBRFIL        VAR11
019800900514     C                   MOVE      '))'          VAR12            12
019900900514     C                   MOVEL     MBRFIL        VAR12
020000900514     C                   Z-ADD     55            LUNG             15 5
020100900514     C*
020200900627     C                   CLOSE     TABEL00R
020300900627     C*
020400900514     C                   MOVE      VAR12         CMD5
020500900514     C                   MOVEL     *BLANKS       COMMAN
020600900514     C                   MOVEA     CMD5(1)       COMMAN
020700900627     C                   CALL      'QCMDEXC'                            20
020800900514     C                   PARM                    COMMAN
020900900514     C                   PARM                    LUNG
021000900514     C** DISALLOCA IL MEMBRO AGG.TO
021100900514     C                   ENDSR
021200941018     C/SPACE 005
021300941018     C**---------------------------------------------------------
021400941018     C** CANCELLA RECORDS ANNULLATI                             -
021500941018     C**---------------------------------------------------------
021600941018     C     DELANN        BEGSR
021700941018     C                   SETOFF                                       58
021800941018     C     *LOVAL        SETLL     TABEL
021900941018     C                   DO        *HIVAL
022000941018     C                   READ      TABEL                                  58
022100941018     C     TBLFLG        IFEQ      '*'                                          ANN.TA
022200941018     C                   DELETE    TABEL
022300941018     C                   END
022400941018     C  N58              END
022500941018     C                   ENDSR
022600900514**         CMD1
022700900514CHKOBJ OBJ(TABEL00R) OBJTYPE(*FILE)     MBR(M234567890)
022800900514**         CMD2
022900900514ALCOBJ OBJ((TABEL00R *FILE *EXCL           M234567890))
023000900514**         CMD3
023100900514OVRDBF FILE(TABEL00R)                   MBR(M234567890)
023200900514**         CMD5
023300900514DLCOBJ OBJ((TABEL00R *FILE *EXCL           M234567890))
