000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606     F* TRTRZ1R  *---------------------------------------------------*
000300000000     F*------------------*                                           *
000400900606     F* - TRASMISSIONE ORGANIGRAMMA AZIENDALE
000500000000     F*--------------------------------------------------------------*
000600900614     FAZORG01L  UF   E           K DISK
000700900614     FAZORG0AT  O  A E             DISK    USROPN
000800900614     FAZORG0BT  O  A E             DISK    USROPN
000900020802     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001000020802     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001100020802     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
001200020802     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
001300020802     D CMB1            S             48    DIM(1) CTDATA PERRCD(1)
001400020802     D CMB2            S             48    DIM(1) CTDATA PERRCD(1)
001500020802     D CMB3            S             48    DIM(1) CTDATA PERRCD(1)
001600020802     D CMB4            S             48    DIM(1) CTDATA PERRCD(1)
001700900605     D DSCMZ         E DS
001800020802     D TIBS55DS      E DS
001900020802     D TIBS56DS      E DS
002000020802     d  as4                    7    756  0 dim(250)
002100000000     D KPJBA         E DS
002200000000     C*---------------------------------------------------------------*
002300000000     C     *ENTRY        PLIST
002400971229     C                   PARM                    KPJBA
002500971117     C*
002600971117     C* REPERIMENTO DATA TRASMISSIONE
002700971117     C                   Z-ADD     *ZEROS        NUMTRA            6 0
002800971117     C                   Z-ADD     UDATE         NUMTRA
002900971117     C                   MOVE      UDAY          NUMTRA
003000971117     C                   MOVEL     UYEAR         NUMTRA
003100900614     C*
003200900614     C*CARICA SCHIERA AS INSTALLATI
003300900614     C                   EXSR      CARTAB
003400900614     C*
003500900614     C* APERTURA FILES TRASMISSIONE
003600971229     C                   EXSR      CAEX
003700900614     C*
003800020802     C* IL LOOP DI LETTURA NON CAMBIA TUTTO A TUTTI COME ORA
003900900614     C     *LOVAL        SETLL     AZORG
004000900606     C                   READ      AZORG                                  02
004100971229DO  1C     *IN02         DOWEQ     *OFF
004200971229IF  2C     ORGFTR        IFEQ      *BLANKS
004300000000     C                   EXSR      CRETAB
004400900606     C                   MOVEL     'T'           ORGFTR
004500971117     C                   Z-ADD     NUMTRA        ORGNTR
004600900606     C                   UPDATE    AZORG
004700971229E   2C                   ENDIF
004800971229     C                   READ      AZORG                                  02
004900971229E   1C                   ENDDO
005000900614     C* TRASMISSIONE
005100900622     C     NUREC         IFGT      *ZERO
005200900605     C                   EXSR      X25
005300900622     C                   END
005400900614     C*
005500000000     C                   SETON                                        LR
005600900614     C**-------------------------------
005700971229     C** CARICAMENTO SCHIER
005800900614     C***------------------------------
005900900614     C     CARTAB        BEGSR
006000020802     C* CHIAMO TIBS56R IN MODALITA' "GRU"
006100971229     C                   Z-ADD     *ZEROS        C                 4 0
006200020802     C                   CLEAR                   TIBS56DS
006300020802     C                   MOVEL     *zeros        I56PPO
006400020802     C                   MOVEL     'GRU'         I56MOD
006500020802     C                   CALL      'TIBS56R'
006600020802     C                   PARM                    TIBS56DS
006700971229     C                   ENDSR
006800000000     C**----------------------------------
006900900606     C** CREA RECORD AZORG
007000000000     C**----------------------------------
007100000000     C     CRETAB        BEGSR
007200900605     C                   ADD       1             NUREC             7 0
007300900614     C                   WRITE     AZORGA
007400900614     C                   WRITE     AZORGB
007500000000     C                   ENDSR
007600900605     C/SPACE 005
007700900605     C**-------------------------------------
007800900605     C** PREPARA SOTTOMISSIONE TRASMISSIONE
007900900605     C**-------------------------------------
008000900605     C     X25           BEGSR
008100971229     C*
008200971229     C* CHIUDE I FILE
008300900614     C                   CLOSE     AZORG0AT
008400900614     C                   CLOSE     AZORG0BT
008500971229     C*
008600971229     C* TRASMETTE I MEMBRI CREATI A TUTTI I PRIMI LIVELLO
008700971229     C                   Z-ADD     1             C                 4 0
008800971229DO  1C     AS4(C)        DOWGT     *ZEROS
008900020802     C                   CLEAR                   TIBS55DS
009000020802     C                   z-add     046           I50PPO
009100020802     C                   z-add     AS4(C)        I50APO
009200020802     C                   CALL      'TIBS55R'
009300020802     C                   PARM                    TIBS55DS
009400020802      *
009500971229     C                   EXSR      X25A                                          1°PARTE
009600971229     C                   EXSR      X25B                                          2°PARTE
009700971229     C                   ADD       1             C
009800971229E   1C                   ENDDO
009900900614     C*
010000971229     C* PULISCE IL FILE TRASMESSO
010100971229     C                   MOVEA(P)  CMD4(1)       COMMAN           80
010200900614     C                   CALL      'QCMDEXC'                            20
010300900614     C                   PARM                    COMMAN
010400900614     C                   PARM                    LUNG
010500900614     C*
010600971229     C                   MOVEA(P)  CMB4(1)       COMMAN           80
010700900614     C                   CALL      'QCMDEXC'                            20
010800900614     C                   PARM                    COMMAN
010900900614     C                   PARM                    LUNG
011000971229     C*
011100900605     C                   ENDSR
011200900605     C**----------------------------------
011300900614     C** SOTTOMETTE TRASMISSIONE 1° PARTE
011400900605     C**----------------------------------
011500900614     C     X25A          BEGSR
011600971229     C                   MOVEL     AS4(C)        CMZDST                          DESTINAT.
011700900614     C                   MOVEL     'AZORG0AT'    CMZFLD                          DA: >>>
011800020802     C                   MOVEL     'M000000'     CMZMBD
011900971112     C                   MOVEL     O50PLG        CMZLBD
012000900614     C                   MOVEL     'AZORG0AR'    CMZFLA                          A: >>>
012100020802     C                   MOVEL     'M000000'     CMZMBA
012200020802     C                   MOVEL     O50ALG        CMZLBA
012300900605     C                   MOVE      '0000'        CMZORI                          ORA
012400900605     C                   MOVE      '0000000'     CMZDTI                          DATA
012500900605     C                   MOVE      'T'           CMZURG                          NOTTE
012600900605     C                   MOVE      'A'           CMZADD                          IN ADD
012700971229     C                   MOVE      'N'           CMZCLR                          CLEAR
012800900605     C                   MOVE      'S'           CMZFTR                          DA TRANS.
012900900605     C                   MOVE      *BLANK        CMZERR                          1= ERR.
013000900614     C                   MOVE      *BLANK        CMZAZN                          AZIONE
013100020802     C                   MOVEL     O50ASI        CMZSYF                          SIST.INFORMATIVO
013200900605     C                   MOVE      NUREC         CMZNRR                          N.REC.
013300900605     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
013400020802     C                   MOVE      *BLANK        CMZ236          236             PRE-REQ.
013500020802     C                   CALL      'TIBS53C'
013600020802     C                   PARM                    O50LCL
013700020802     C                   PARM                    CMZDST
013800900605     C                   PARM                    CMZFLD
013900900605     C                   PARM                    CMZMBD
014000900605     C                   PARM                    CMZLBD
014100900605     C                   PARM                    CMZFLA
014200900605     C                   PARM                    CMZMBA
014300900605     C                   PARM                    CMZLBA
014400900605     C                   PARM                    CMZORI
014500900605     C                   PARM                    CMZDTI
014600900605     C                   PARM                    CMZURG
014700900605     C                   PARM                    CMZADD
014800900605     C                   PARM                    CMZCLR
014900900605     C                   PARM                    CMZFTR
015000900605     C                   PARM                    CMZERR
015100900605     C                   PARM                    CMZAZN
015200900605     C                   PARM                    CMZSYF
015300900605     C                   PARM                    CMZNRR
015400900605     C                   PARM                    CMZPRR
015500020802     C                   PARM                    CMZ236
015600900605     C                   ENDSR
015700900614     C/SPACE 005
015800900614     C**----------------------------------
015900900614     C** SOTTOMETTE TRASMISSIONE 2° PARTE
016000900614     C**----------------------------------
016100900614     C     X25B          BEGSR
016200971229     C                   MOVEL     AS4(C)        CMZDST                          DESTINAT.
016300900614     C                   MOVEL     'AZORG0BT'    CMZFLD                          DA: >>>
016400020802     C                   MOVEL     'M000000'     CMZMBD
016500971112     C                   MOVEL     O50PLG        CMZLBD
016600900614     C                   MOVEL     'AZORG0BR'    CMZFLA                          A: >>>
016700020802     C                   MOVEL     'M000000'     CMZMBA
016800020802     C                   MOVEL     O50ALG        CMZLBA
016900900614     C                   MOVE      '0000'        CMZORI                          ORA
017000900614     C                   MOVE      '0000000'     CMZDTI                          DATA
017100900614     C                   MOVE      'T'           CMZURG                          NOTTE
017200900614     C                   MOVE      'A'           CMZADD                          IN ADD
017300971229     C                   MOVE      'N'           CMZCLR                          CLEAR
017400900614     C                   MOVE      'S'           CMZFTR                          DA TRANS.
017500900614     C                   MOVE      *BLANK        CMZERR                          1= ERR.
017600900614     C                   MOVE      'TRZ3'        CMZAZN                          AZIONE
017700020802     C                   MOVEL     O50ASI        CMZSYF                          SIST.INFORMATIVO
017800900614     C                   MOVE      NUREC         CMZNRR                          N.REC.
017900020802     C                   MOVE      *BLANK        CMZ236                          PRE-REQ.
018000020802     C                   CALL      'TIBS53C'
018100020802     C                   PARM                    O50LCL
018200900614     C                   PARM                    CMZDST
018300900614     C                   PARM                    CMZFLD
018400900614     C                   PARM                    CMZMBD
018500900614     C                   PARM                    CMZLBD
018600900614     C                   PARM                    CMZFLA
018700900614     C                   PARM                    CMZMBA
018800900614     C                   PARM                    CMZLBA
018900900614     C                   PARM                    CMZORI
019000900614     C                   PARM                    CMZDTI
019100900614     C                   PARM                    CMZURG
019200900614     C                   PARM                    CMZADD
019300900614     C                   PARM                    CMZCLR
019400900614     C                   PARM                    CMZFTR
019500900614     C                   PARM                    CMZERR
019600900614     C                   PARM                    CMZAZN
019700900614     C                   PARM                    CMZSYF
019800900614     C                   PARM                    CMZNRR
019900900614     C                   PARM                    CMZPRR
020000020802     C                   PARM                    CMZ236
020100900614     C                   ENDSR
020200900605     C/SPACE
020300900605     C**---------------------------------------------
020400020802     C** ESEGUE QCAEXEC SUL MEMBRO DI AZORG0AT      -
020500900614     C**                SUL MEMBRO DI AZORG0BT      -
020600900605     C**---------------------------------------------
020700900605     C     CAEX          BEGSR
020800020802     C                   Z-ADD     48            LUNG             15 5
020900900614     C*
021000900614     C* 1° PARTE DEL FILE
021100900605     C                   SETOFF                                       20
021200971229     C                   MOVEA(P)  CMD3(1)       COMMAN           80
021300900605     C                   CALL      'QCMDEXC'                            20
021400900605     C                   PARM                    COMMAN
021500900605     C                   PARM                    LUNG
021600900605     C** CONTROLLO SE ESISTE IL MEMBRO
021700900605     C   20              DO
021800971229     C                   MOVEA(P)  CMD1(1)       COMMAN
021900900605     C                   CALL      'QCMDEXC'
022000900605     C                   PARM                    COMMAN
022100900605     C                   PARM                    LUNG
022200900605     C                   END
022300900605     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
022400971229     C                   MOVEA(P)  CMD2(1)       COMMAN
022500900605     C                   CALL      'QCMDEXC'
022600900605     C                   PARM                    COMMAN
022700900605     C                   PARM                    LUNG
022800971229     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
022900900614     C                   OPEN      AZORG0AT
023000900614     C*
023100900614     C* 2° PARTE DEL FILE
023200900614     C                   SETOFF                                       20
023300971229     C                   MOVEA(P)  CMB3(1)       COMMAN           80
023400900614     C                   CALL      'QCMDEXC'                            20
023500900614     C                   PARM                    COMMAN
023600900614     C                   PARM                    LUNG
023700900614     C** CONTROLLO SE ESISTE IL MEMBRO
023800900614     C   20              DO
023900971229     C                   MOVEA(P)  CMB1(1)       COMMAN
024000900614     C                   CALL      'QCMDEXC'
024100900614     C                   PARM                    COMMAN
024200900614     C                   PARM                    LUNG
024300900614     C                   END
024400900614     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
024500971229     C                   MOVEA(P)  CMB2(1)       COMMAN
024600900614     C                   CALL      'QCMDEXC'
024700900614     C                   PARM                    COMMAN
024800900614     C                   PARM                    LUNG
024900900614     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
025000900614     C                   OPEN      AZORG0BT
025100900605     C                   ENDSR
025200971229     C*------------------------------------------------------------------------*
025300020802**         CMD1
025400020802ADDPFM FILE(AZORG0AT)               MBR(M000000)
025500900605**         CMD2
025600020802OVRDBF FILE(AZORG0AT)               MBR(M000000)
025700900605**         CMD3
025800020802CHKOBJ OBJ(AZORG0AT) OBJTYPE(*FILE) MBR(M000000)
025900900614**         CMD4
026000020802CLRPFM FILE(AZORG0AT)               MBR(M000000)
026100900614**         CMB1
026200020802ADDPFM FILE(AZORG0BT)               MBR(M000000)
026300900614**         CMB2
026400020802OVRDBF FILE(AZORG0BT)               MBR(M000000)
026500900614**         CMB3
026600020802CHKOBJ OBJ(AZORG0BT) OBJTYPE(*FILE) MBR(M000000)
026700900614**         CMB4
026800020802CLRPFM FILE(AZORG0BT)               MBR(M000000)
