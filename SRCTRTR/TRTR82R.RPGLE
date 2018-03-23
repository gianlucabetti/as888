000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900607      * TRTR82R  *---------------------------------------------------*
000300900607      *------------------*                                           *
000400900607      *                                                              *
000500900607      * - PREPARAZIONE ARCHIVIO FLBCO00F DA TRASMETTERE              *
000600900607      *                                                              *
000700900607      *--------------------------------------------------------------*
000800020930     FFNBCO01L  IF   E           K DISK
000900991122     FFNBCO00T  O    E             DISK    USROPN
001000991122     F                                     RENAME(FNBCO:FNBCOT)
001100900607      *
001200020930     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC
001300020930     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC
001400020930     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC
001500900607      *
001600900608     D PARAM           DS
001700900608     D  §DIT                   1      1
001800900608     D  §NRDIS                 2      8  0
001900020930     D  PrmFle
002000020930     D                                     LIKE(BcoFle)
002100900516     D DSCMZ         E DS
002200020930     D TIBS55        E DS                  EXTNAME(TIBS55DS)
002300000000     D KPJBA         E DS
002400900516     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002500020930
002600020930     C                   EXSR      DEFCAM
002700900607      *
002800900606     C                   EXSR      TRASM
002900900607      *
003000900607      * TRASMETTO     FLBCO00T
003100020930     C                   CLOSE     FNBCO00T
003200020930     C     NRMOV         IFNE      *ZEROS
003300020930     C                   EXSR      X25MOV
003400020930     C                   END
003500020930
003600020930     C                   EVAL      *INLR = *ON
003700020930
003800900607      *---------------------------------------------------------------*
003900900411     C     DEFCAM        BEGSR
004000900607      *---------------------------------------------------------------*
004100000000     C     *ENTRY        PLIST
004200000000     C                   PARM                    KPJBA
004300900608     C                   MOVEL     KPJBU         PARAM
004400000000     C                   Z-ADD     1             CODUT
004500900516     C                   CALL      'X§PARUT'
004600000000     C                   PARM                    UTEDSE
004700000000     C     CODUT         CABEQ     -1            FINE
004800000000     C                   MOVEL     RAGUT         RSUT             20
004900900607      *
005000900607      *  APERTURA FILE DA TRASMETTERE
005100900516     C                   EXSR      CAEX
005200900607      *
005300900516     C                   MOVE      *ZEROS        NRMOV             7 0
005400900607      *
005500020930
005600020930     C     K03Bco01      KLIST
005700020930     C                   KFLD                    PrmFle
005800020930     C                   KFLD                    §DIT
005900020930     C                   KFLD                    §NRDIS
006000020930
006100900412     C     FINE          ENDSR
006200900516     C/SPACE 3
006300900607      **---------------------------------------------
006400991122      ** ESEGUE QCAEXEC SUL MEMBRO DI FNBCO00T      -
006500900607      **---------------------------------------------
006600900516     C     CAEX          BEGSR
006700900607      ** FILIALE ELABORATORE   PER APERTURA MEMBRO
006800020930     C                   MOVE      '046)'        VAR               7
006900020930     C                   MOVEL     PrmFle        VAR
007000020930     C                   Z-ADD     48            LUNG             15 5
007100991122      ** CONTROLLO SE ESISTE IL MEMBRO X FNBCO00T
007200900516     C                   SETOFF                                       20
007300900516     C                   MOVE      VAR           CMD3
007400900516     C                   MOVEL     *BLANKS       COMMAN
007500900516     C                   MOVEA     CMD3(1)       COMMAN           80
007600900516     C                   CALL      'QCMDEXC'                            20
007700900516     C                   PARM                    COMMAN
007800900516     C                   PARM                    LUNG
007900900607      ** ESEGUE ADDPFM DEL MEMBRO NUOVO
008000900516     C   20              DO
008100900516     C                   MOVE      VAR           CMD1
008200900516     C                   MOVEL     *BLANKS       COMMAN
008300900516     C                   MOVEA     CMD1(1)       COMMAN
008400900516     C                   CALL      'QCMDEXC'
008500900516     C                   PARM                    COMMAN
008600900516     C                   PARM                    LUNG
008700900516     C                   END
008800900607      *+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
008900900516     C                   MOVE      VAR           CMD2
009000900516     C                   MOVEL     *BLANKS       COMMAN
009100900516     C                   MOVEA     CMD2(1)       COMMAN
009200900516     C                   CALL      'QCMDEXC'
009300900516     C                   PARM                    COMMAN
009400900516     C                   PARM                    LUNG
009500991122     C                   OPEN      FNBCO00T
009600900606      *
009700900516     C                   ENDSR
009800900607      *----------------------------------------------------*
009900891209     C/SPACE
010000900607      *---------------------------------------------------------------*
010100900411     C     TRASM         BEGSR
010200020930
010300020930     C     K03Bco01      SETLL     FnBco01L
010400020930     C                   DOU       %EOF(FnBco01L)
010500020930     C     K03Bco01      READE     FnBco01L
010600020930     C                   IF        NOT %EOF(FnBco01L)
010700020930
010800900607      * SCRIVO RECORD DA TRASMETTERE
010900900517     C                   ADD       1             NRMOV
011000991122     C                   WRITE     FNBCOT
011100020930
011200020930     C                   ENDIF
011300020930     C                   ENDDO
011400020930
011500900411     C                   ENDSR
011600900607      *---------------------------------------------------------------*
011700900516     C/SPACE 005
011800900607      **----------------------------------
011900900607      ** SOTTOMETTE TRASMISSIONE
012000900607      **----------------------------------
012100900516     C     X25MOV        BEGSR
012200971128     C* PRELEVO LIBRERIE DEI FILE DI TRASMISSIONE E S.I.
012300971128     C*  A CUI TRASMETTERE
012400020930     C                   CLEAR                   TIBS55
012500020930     C                   MOVEL     'L'           I50TLA
012600010312     C                   MOVEL     KNSIF         I50PSI
012700020930     C                   Z-ADD     PrmFle        I50PPO
012800020930     C                   Z-ADD     046           I50APO
012900020930     C                   CALL      'TIBS55R'
013000020930     C                   PARM                    TIBS55
013100971128     C* TRASMISSIONE
013200900516     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
013300991122     C                   MOVEL     'FNBCO00T'    CMZFLD                          DA: >>>
013400020930     C                   EVAL      CmzMbD = 'M' + %EDITC(PrmFle:'X') + CmzDst
013500971128     C                   MOVEL     O50PLA        CMZLBD
013600991122     C                   MOVEL     'FNBCO00R'    CMZFLA                          A: >>>
013700020930     C                   MOVEL     CmzMbD        CMZMBA
013800010312     C                   MOVEL     O50ALA        CMZLBA
013900021001     C                   MOVEL     'GAITRA'      CMZLBA
014000021001     C                   MOVE      O50ALA        WQUATR            4
014100021001     C     WQUATR        IFEQ      '101 '
014200021001     C                   MOVE      '201 '        CMZLBA
014300021001     C                   ENDIF
014400900516     C                   MOVE      '0000'        CMZORI                          ORA
014500900516     C                   MOVE      '0000000'     CMZDTI                          DATA
014600991124     C                   MOVE      'N'           CMZURG                          NOTTE
014700900516     C                   MOVE      'A'           CMZADD                          IN ADD
014800900607     C                   MOVE      'S'           CMZCLR                          CLEAR
014900900516     C                   MOVE      'S'           CMZFTR                          DA TRANS.
015000900516     C                   MOVE      *BLANK        CMZERR                          1= ERR.
015100991122     C                   MOVE      'TR84'        CMZAZN                          AZIONE
015200010312     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
015300021001     C                   MOVEL     'GAITRA'      CMZSYF
015400021001     C                   MOVE      O50ASI        WQUATR
015500021001     C     WQUATR        IFEQ      '101 '
015600021001     C                   MOVE      '201 '        CMZSYF
015700021001     C                   ENDIF
015800900516     C                   MOVE      NRMOV         CMZNRR                          N.REC.
015900900517     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
016000971128     C                   MOVE      *BLANKS       W236            236
016100020930     C                   CALL      'TIBS53C'
016200971128     C                   PARM                    O50LCL
016300971128     C                   PARM                    CMZDST
016400900516     C                   PARM                    CMZFLD
016500900516     C                   PARM                    CMZMBD
016600900516     C                   PARM                    CMZLBD
016700900516     C                   PARM                    CMZFLA
016800900516     C                   PARM                    CMZMBA
016900900516     C                   PARM                    CMZLBA
017000900516     C                   PARM                    CMZORI
017100900516     C                   PARM                    CMZDTI
017200900516     C                   PARM                    CMZURG
017300900516     C                   PARM                    CMZADD
017400900516     C                   PARM                    CMZCLR
017500900516     C                   PARM                    CMZFTR
017600900516     C                   PARM                    CMZERR
017700900516     C                   PARM                    CMZAZN
017800900516     C                   PARM                    CMZSYF
017900900516     C                   PARM                    CMZNRR
018000900516     C                   PARM                    CMZPRR
018100971128     C                   PARM                    W236
018200900516     C                   ENDSR
018300020930
018400991122**         CMD1
018500020930ADDPFM FILE(FNBCO00T)               MBR(M000000)
018600900516**         CMD2
018700020930OVRDBF FILE(FNBCO00T)               MBR(M000000)
018800900516**         CMD3
018900020930CHKOBJ OBJ(FNBCO00T) OBJTYPE(*FILE) MBR(M000000)
