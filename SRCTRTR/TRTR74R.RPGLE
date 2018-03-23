000100900430     H DECEDIT('0,') DATEDIT(*DMY.)
000200900529     F* TRTR74R  *---------------------------------------------------*
000300900430     F*------------------*                                           *
000400900529     F* - TRASMISSIONE PARAMETRI UTENTE
000500900430     F*--------------------------------------------------------------*
000600900529     FPARUT00F  IF   E           K DISK
000700900529     FPARUT00T  O  A E             DISK    USROPN
000800900529     F                                     RENAME(PARUT:PARUTT)
000900020724     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001000020724     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001100020724     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
001200020724     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
001300930906     D DTA             S              4    DIM(12)                              DATE ALFA
001400020729     D TIBS55ds      E DS
001500020724     D TIBS56ds      E DS
001600020923     D                                     INZ
001700020923     D  FLM                           3  0 DIM(250)                             FIL.CON AS
001800020923     D                                     OVERLAY(o56ski)
001900900511     D DSCMZ         E DS
002000900430     D KPJBA         E DS
002100020923     D UTEDSE        E DS                  EXTNAME(UT伶SE0F)
002200020923     D CNCR80        E DS
002300930906     D                 DS
002400930906     D  PAR1                   1      6
002500930906     D  PAR2                   7     12
002600930906     D  PAR3                  13     18
002700930906     D  PAR4                  19     24
002800930906     D  PAR5                  25     30
002900930906     D  PAR6                  31     36
003000930906     D  PAR7                  37     42
003100930906     D  PAR8                  43     48
003200930906     D  DATE                   1     48
003300900430     I/SPACE 3
003400900430     C*---------------------------------------------------------------*
003500900430     C     *ENTRY        PLIST
003600900430     C                   PARM                    KPJBA
003700900430     C                   Z-ADD     1             CODUT
003800020923     C                   CALL      'X判ARUT'
003900900430     C                   PARM                    UTEDSE
004000900430     C     CODUT         CABEQ     -1            FINE
004100900430     C                   MOVEL     RAGUT         RSUT             20
004200020923     C                   EVAL      CNCR80 = Rec80
004300900514     C*
004400900622     C*---------------------------------------------------------------*
004500900622     C     KTAB          KLIST
004600900622     C                   KFLD                    CODUT
004700900622     C                   KFLD                    COD               2
004800900622     C*
004900900622     C*CARICA SCHIERA AS INSTALLATI
005000900622     C                   EXSR      CARTAB
005100900622     C*
005200900430     C*---------------------------------------------------------------*
005300900529     C     *LOVAL        SETLL     PARUT
005400900430     C*
005500020923     C                   MOVE      *ZERO         FILCOM            6
005600020923     C                   MOVEL     SimFEl        FILCOM
005700900622     C                   EXSR      CAEX                                         APERTURA MBR
005800900622     C*
005900900430     C     SU            TAG
006000900529     C                   READ      PARUT                                  02
006100900529     C   02              GOTO      STOP
006200900529     C                   EXSR      CREREC
006300900430     C                   GOTO      SU
006400900529     C     STOP          TAG
006500900514     C                   EXSR      X25
006600900529     C     FINE          TAG
006700971202     C* Imposto i dati in BS50
006800020729     C                   CLEAR                   TIBS55ds
006900971202     C                   MOVEL     'C'           I50TLA
007000020729     C                   CALL      'TIBS55R'
007100020729     C                   PARM                    TIBS55ds
007200971202     C*
007300900430     C                   SETON                                        LR
007400900622     C/SPACE 005
007500900622     C**-------------------------------
007600900622     C** CARICAMENTO SCHIERE:   FLM   *
007700900622     C***------------------------------
007800900622     C     CARTAB        BEGSR
007900020923     c                   eval      i56ppo=simfel
008000020924     c                   eval      i56mod='AZI'
008100020724     c                   call      'TIBS56R'
008200020724     c                   parm                    tibs56ds
008300900622     C     ENDTAB        ENDSR
008400900430     C/SPACE 005
008500900430     C**----------------------------------
008600900529     C** CREA RECORD PARUT00T
008700900430     C**----------------------------------
008800900529     C     CREREC        BEGSR
008900900529     C                   ADD       1             MOV               7 0
009000900529     C                   WRITE     PARUTT
009100900430     C                   ENDSR
009200900511     C/SPACE 005
009300900514     C**-------------------------------------
009400900514     C** PREPARA SOTTOMISSIONE TRASMISSIONE
009500900514     C**-------------------------------------
009600900511     C     X25           BEGSR
009700900622     C                   CLOSE     PARUT00T
009800900529     C                   MOVE      *ZERO         你S4              3             DESTINAT.
009900900529     C                   MOVE      *ZERO         冶BR              3             MEMBRO
010000900622     C                   MOVE      'N'           低LR              1             CLEAR
010100020724     C                   DO        250           C                 3 0
010200900622     C     FLM(C)        IFGT      *ZERO
010300900622     C                   MOVE      FLM(C)        你S4
010400971202     C* Richiamo pgm x richiesta librerie - S.Informativi
010500971202     C* da/a cui spedire
010600971202     C                   EXSR      RICLIB
010700900622     C                   EXSR      X25T
010800900622     C                   END
010900900622     C                   END
011000900622     C*
011100900622     C                   MOVE      ')'           VAR
011200900622     C                   MOVEL     FILCOM        VAR
011300900622     C                   MOVEL     *BLANKS       COMMAN
011400900622     C                   MOVE      VAR           CMD4
011500900622     C                   MOVEA     CMD4(1)       COMMAN           80
011600900622     C                   CALL      'QCMDEXC'                            20
011700900622     C                   PARM                    COMMAN
011800900622     C                   PARM                    LUNG
011900900511     C                   ENDSR
012000900514     C/SPACE 005
012100971202     C**---------------------------------------------------------*
012200971202     C** Richiamo pgm x reperimento Sif. e lib a cui trasmettere
012300971202     C**---------------------------------------------------------*
012400971202     C     RICLIB        BEGSR
012500971202     C*
012600020729     C                   CLEAR                   TIBS55ds
012700971202     C* PRELEVO LIBRERIE DEI FILE DI TRASMISSIONE E S.I.
012800971202     C*  A CUI TRASMETTERE
012900971202     C                   MOVEL     KNSIF         I50PSI
013000020923     C                   MOVEL     SimFEl        I50PPO
013100971202     C                   MOVEL     你S4          I50APO
013200971202     C                   MOVEL     ' '           I50TLA
013300020729     C                   CALL      'TIBS55R'
013400020729     C                   PARM                    TIBS55ds
013500971202     C*
013600971202     C                   ENDSR
013700900514     C**----------------------------------
013800900515     C** SOTTOMETTE TRASMISSIONE
013900900514     C**----------------------------------
014000900514     C     X25T          BEGSR
014100020923     C                   MOVEL     你S4          CMZDST                          DESTINAT.
014200900529     C                   MOVEL     'PARUT00T'    CMZFLD                          DA: >>>
014300020923     C                   EVAL      CMZMBD = 'M' + FILCOM
014400021007     C                   MOVEL     O50PLG        CMZLBD
014500900529     C                   MOVEL     'PARUT00R'    CMZFLA                          A: >>>
014600020923     C                   EVAL      CMZMBA = 'M' + %EDITC(SimFEl:'X') + 你S4
014700020924     C                   MOVEL     O50ALA        CMZLBA
014800900514     C                   MOVE      '0000'        CMZORI                          ORA
014900900514     C                   MOVE      '0000000'     CMZDTI                          DATA
015000900514     C                   MOVE      'T'           CMZURG                          NOTTE
015100900514     C                   MOVE      'A'           CMZADD                          IN ADD
015200900515     C                   MOVE      低LR          CMZCLR                          CLEAR
015300900514     C                   MOVE      'S'           CMZFTR                          DA TRANS.
015400900514     C                   MOVE      *BLANK        CMZERR                          1= ERR.
015500900529     C                   MOVE      'TR75'        CMZAZN                          AZIONE
015600020923     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
015700900529     C                   MOVE      MOV           CMZNRR                          N.REC.
015800900514     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
015900020724     C                   CALL      'TIBS53C'
016000971202     C                   PARM                    O50LCL
016100971202     C                   PARM                    CMZDST
016200900514     C                   PARM                    CMZFLD
016300900514     C                   PARM                    CMZMBD
016400900514     C                   PARM                    CMZLBD
016500900514     C                   PARM                    CMZFLA
016600900514     C                   PARM                    CMZMBA
016700900514     C                   PARM                    CMZLBA
016800900514     C                   PARM                    CMZORI
016900900514     C                   PARM                    CMZDTI
017000900514     C                   PARM                    CMZURG
017100900514     C                   PARM                    CMZADD
017200900514     C                   PARM                    CMZCLR
017300900514     C                   PARM                    CMZFTR
017400900514     C                   PARM                    CMZERR
017500900514     C                   PARM                    CMZAZN
017600900514     C                   PARM                    CMZSYF
017700900514     C                   PARM                    CMZNRR
017800900514     C                   PARM                    CMZPRR
017900020923     C                   PARM                    W236            236
018000900514     C                   ENDSR
018100900510     C/SPACE
018200900510     C**---------------------------------------------
018300900529     C** ESEGUE QCAEXEC SUL MEMBRO DI PARUT00T      -
018400900510     C**---------------------------------------------
018500900510     C     CAEX          BEGSR
018600020724     C                   MOVE      ')'           VAR               7
018700900510     C                   MOVEL     FILCOM        VAR
018800020724     C                   Z-ADD     48            LUNG             15 5
018900900510     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
019000900622     C*
019100900510     C                   SETOFF                                       20
019200900510     C                   MOVE      VAR           CMD3
019300900510     C                   MOVEL     *BLANKS       COMMAN
019400900510     C                   MOVEA     CMD3(1)       COMMAN           80
019500900510     C                   CALL      'QCMDEXC'                            20
019600900510     C                   PARM                    COMMAN
019700900510     C                   PARM                    LUNG
019800900510     C** CONTROLLO SE ESISTE IL MEMBRO
019900900510     C   20              DO
020000900510     C                   MOVE      VAR           CMD1
020100900511     C                   MOVEL     *BLANKS       COMMAN
020200900510     C                   MOVEA     CMD1(1)       COMMAN
020300900510     C                   CALL      'QCMDEXC'
020400900510     C                   PARM                    COMMAN
020500900510     C                   PARM                    LUNG
020600900510     C                   END
020700900510     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
020800900510     C                   MOVE      VAR           CMD2
020900900510     C                   MOVEL     *BLANKS       COMMAN
021000900510     C                   MOVEA     CMD2(1)       COMMAN
021100900510     C                   CALL      'QCMDEXC'
021200900510     C                   PARM                    COMMAN
021300900510     C                   PARM                    LUNG
021400900510     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
021500900529     C                   OPEN      PARUT00T
021600900510     C                   ENDSR
021700900510     C/SPACE
021800900622**         CMD1
021900020724ADDPFM FILE(PARUT00T)               MBR(M000000)
022000900510**         CMD2
022100020724OVRDBF FILE(PARUT00T)               MBR(M000000)
022200900510**         CMD3
022300020724CHKOBJ OBJ(PARUT00T) OBJTYPE(*FILE) MBR(M000000)
022400900622**         CMD4
022500020724CLRPFM FILE(PARUT00T)               MBR(M000000)
