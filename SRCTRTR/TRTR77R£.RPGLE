000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200971111     F* TRTR77R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400900605     F* - TRASMISSIONE ANAGRAFICO BANCHE ABI
000500000000     F*--------------------------------------------------------------*
000600900605     FCNABI00F  UF   E           K DISK
000700020919     FCNABI00T  UF A E             DISK    USROPN
000800900605     F                                     RENAME(CNABI000:CNABIT)
000900020912     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001000020912     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001100020912     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
001200020912     D* DS TIBS55DS X RICERCA LIBRERIA FILE DA METTERE IN TRASMISS.
001300020912     D TIBS55        E DS                  EXTNAME(TIBS55DS)
001400981230     D DSCNAB        E DS                  EXTNAME(CNABI00F)
001500971111     D DSCMZ         E DS
001600000000     D KPJBA         E DS
001700020912     D CNCR80        E DS
001800020912     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900020806     D RowsDelAcT      S              9B 0
002000020806     D RowsDelAcR      S              9B 0
002100020912     D I               S              5I 0
002200020919     D WrkCurDate      S               D
002300020912
002400020912     D Tibs56ds      E DS
002500020912     D                                     INZ
002600020912     D  Ski                           3  0
002700020912     D                                     DIM(250)
002800020912     D                                     OVERLAY(O56Ski)
002900020912
003000000000     C*---------------------------------------------------------------*
003100000000     C     *ENTRY        PLIST
003200000000     C                   PARM                    KPJBA
003300020919
003400020919     C                   TIME                    WrkCurDate
003500020919
003600000000     C                   Z-ADD     1             CODUT
003700020912     C                   CALL      'X§PARUT'
003800020912     C                   PARM                    UT§DSE
003900000000     C     CODUT         CABEQ     -1            FINE
004000020912     C                   MOVEL     REC80         CNCR80
004100000000     C                   MOVEL     RAGUT         RSUT             20
004200020919
004300020912     ** Reperisco la lista degli AS/400.
004400020912     C                   CLEAR                   Tibs56ds
004500020912     C                   EVAL      I56Ppo = SimFEl
004600020912     C                   EVAL      I56Mod = 'GRU'
004700020912     C                   CALL      'TIBS56R'
004800020912     C                   PARM                    Tibs56ds
004900000000     C*---------------------------------------------------------------*
005000000000     C*NUMERO TRASMISSIONE
005100000000     C     SU            TAG
005200900605     C                   READ      CNABI000                               02
005300900605     C   02              GOTO      STOP
005400900605     C*
005500900605     C     ABIFTR        CABNE     *BLANKS       SU
005600911024     C** RECORD GIA' TRASFERITO
005700981230     C* AGGIUNTA CALL PROGRAMMA ALLINEAMENTO PJ ANABI00F
005800981230     C                   CALL      'YCOAB1R'
005900981230     C                   PARM                    DSCNAB
006000900618     C                   EXSR      CREREC
006100900605     C*
006200080319     C*    ABIANN        IFNE      '*'
006300900605     C                   MOVEL     'T'           ABIFTR
006400020919     C     *YMD          MOVE      WrkCurDate    ABIDTR
006500900605     C                   UPDATE    CNABI000
006600080319     C*                  ELSE
006700080319     C*                  DELETE    CNABI000
006800080319     C*                  END
006900000000     C                   GOTO      SU
007000900605     C     STOP          TAG
007100971111     C*
007200900618     C     NUREC         IFGT      *ZERO
007300900605     C                   EXSR      X25
007400900618     C                   END
007500020806
007600020806     ** Allineo ANACT00F a ANACR00F.
007700020806     C                   CALL      'YCOAB2R'
007800020806     C                   PARM      0             RowsDelAcT
007900020806     C                   PARM      0             RowsDelAcR
008000020806
008100000000     C     FINE          TAG
008200000000     C                   SETON                                        LR
008300900605     C/SPACE 005
008400000000     C**----------------------------------
008500000000     C** CREA RECORD ATAB00F
008600000000     C**----------------------------------
008700900618     C     CREREC        BEGSR
008800900605     C                   EXSR      CAEX                                         APERTURA MBR
008900900605     C                   ADD       1             NUREC             7 0
009000900605     C                   WRITE     CNABIT
009100000000     C                   ENDSR
009200900605     C/SPACE 005
009300900605     C**-------------------------------------
009400900605     C** PREPARA SOTTOMISSIONE TRASMISSIONE
009500900605     C**-------------------------------------
009600900605     C     X25           BEGSR
009700020919
009800900605     C   99              CLOSE     CNABI00T
009900020919
010000020919     ** Sottometto una trasmissione per ogni terminal.
010100020912     C                   EVAL      I = 1
010200020912     C                   DOU       Ski(i) = 0
010300900605     C                   EXSR      X25T
010400020912     C                   EVAL      I = I + 1
010500020912     C                   ENDDO
010600020919
010700020919     ** Cancello i record nel file di trasmissione.
010800020919     C                   IF        NOT %OPEN(CnAbi00T)
010900020919     C                   OPEN      CnAbi00T
011000020919     C                   ENDIF
011100020919
011200020919     C                   DOU       %EOF(CnAbi00T)
011300020919     C                   READ      CnAbi00T
011400020919     C                   IF        NOT %EOF
011500020919     C                   DELETE    CnAbiT
011600020919     C                   ENDIF
011700020919     C                   ENDDO
011800020919
011900020919     C                   CLOSE     CnAbi00T
012000020919
012100900605     C                   ENDSR
012200900605     C/SPACE 005
012300900605     C**----------------------------------
012400900605     C** SOTTOMETTE TRASMISSIONE
012500900605     C**----------------------------------
012600900605     C     X25T          BEGSR
012700020912     C* REPERISCO LIBRERIA DI GRUPPO DOVE E' RESIDENTE IL FILE
012800020912     C                   CLEAR                   TIBS55
012900020912     C                   MOVEL     KNSIF         I50PSI                         SIF PART.
013000020912     C                   Z-ADD     SIMFEL        I50PPO
013100020912     C                   MOVEL     KNSIF         I50ASI                         SIF.ARR.
013200020912     C                   Z-ADD     Ski(i)        I50APO
013300020912     C                   CALL      'TIBS55R'
013400020912     C                   PARM                    TIBS55
013500020912
013600020912     C                   MOVEL     Ski(i)        CMZDST                          DESTINAT.
013700900605     C                   MOVEL     'CNABI00T'    CMZFLD                          DA: >>>
013800020912     C                   MOVEL     'M000000'     CMZMBD
013900971111     C                   MOVEL     O50PLG        CMZLBD                         LIB.GRUPPO PART.
014000900605     C                   MOVEL     'CNABI00R'    CMZFLA                          A: >>>
014100020913     C                   EVAL      CMZMBA = 'M'
014200020913     C                             + %EDITC(I50PPO:'X')
014300020913     C                             + %EDITC(I50APO:'X')
014400971111     C                   MOVEL     O50ALG        CMZLBA                         LIB.GRUPPO ARR.
014500900605     C                   MOVE      '0000'        CMZORI                          ORA
014600900605     C                   MOVE      '0000000'     CMZDTI                          DATA
014700900605     C                   MOVE      'T'           CMZURG                          NOTTE
014800900605     C                   MOVE      'A'           CMZADD                          IN ADD
014900020912     C                   MOVE      'N'           CMZCLR                          CLEAR
015000900605     C                   MOVE      'S'           CMZFTR                          DA TRANS.
015100900605     C                   MOVE      *BLANK        CMZERR                          1= ERR.
015200900605     C                   MOVE      'TR80'        CMZAZN                          AZIONE
015300020912     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
015400900605     C                   MOVE      NUREC         CMZNRR                          N.REC.
015500900605     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
015600020912     C                   CALL      'TIBS53C'
015700020912     C                   PARM                    O50LCL
015800900605     C                   PARM                    CMZDST
015900900605     C                   PARM                    CMZFLD
016000900605     C                   PARM                    CMZMBD
016100900605     C                   PARM                    CMZLBD
016200900605     C                   PARM                    CMZFLA
016300900605     C                   PARM                    CMZMBA
016400900605     C                   PARM                    CMZLBA
016500900605     C                   PARM                    CMZORI
016600900605     C                   PARM                    CMZDTI
016700900605     C                   PARM                    CMZURG
016800900605     C                   PARM                    CMZADD
016900900605     C                   PARM                    CMZCLR
017000900605     C                   PARM                    CMZFTR
017100900605     C                   PARM                    CMZERR
017200900605     C                   PARM                    CMZAZN
017300900605     C                   PARM                    CMZSYF
017400900605     C                   PARM                    CMZNRR
017500900605     C                   PARM                    CMZPRR
017600020912     C                   PARM      *BLANK        CMZ236          236
017700020912
017800900605     C                   ENDSR
017900900605     C/SPACE
018000900605     C**---------------------------------------------
018100900605     C** ESEGUE QCAEXEC SUL MEMBRO DI CNABI00T      -
018200900605     C**---------------------------------------------
018300900605     C     CAEX          BEGSR
018400020912     C                   Z-ADD     48            LUNG             15 5
018500900605     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
018600900605     C   99              CLOSE     CNABI00T
018700900605     C  N99              SETON                                        99
018800900605     C*+ CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
018900900605     C                   SETOFF                                       20
019000900605     C                   MOVEL     *BLANKS       COMMAN
019100900605     C                   MOVEA     CMD3(1)       COMMAN           80
019200900605     C                   CALL      'QCMDEXC'                            20
019300900605     C                   PARM                    COMMAN
019400900605     C                   PARM                    LUNG
019500900605     C** CONTROLLO SE ESISTE IL MEMBRO
019600900605     C   20              DO
019700900605     C                   MOVEL     *BLANKS       COMMAN
019800900605     C                   MOVEA     CMD1(1)       COMMAN
019900900605     C                   CALL      'QCMDEXC'
020000900605     C                   PARM                    COMMAN
020100900605     C                   PARM                    LUNG
020200900605     C                   END
020300900605     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
020400900605     C                   MOVEL     *BLANKS       COMMAN
020500900605     C                   MOVEA     CMD2(1)       COMMAN
020600900605     C                   CALL      'QCMDEXC'
020700900605     C                   PARM                    COMMAN
020800900605     C                   PARM                    LUNG
020900900605     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
021000900605     C                   OPEN      CNABI00T
021100900605     C                   ENDSR
021200900605     C/SPACE
021300900605**         CMD1
021400020912ADDPFM FILE(CNABI00T)               MBR(M000000)
021500900605**         CMD2
021600020912OVRDBF FILE(CNABI00T)               MBR(M000000)
021700900605**         CMD3
021800020912CHKOBJ OBJ(CNABI00T) OBJTYPE(*FILE) MBR(M000000)
