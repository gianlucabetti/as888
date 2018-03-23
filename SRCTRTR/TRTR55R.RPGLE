000100900430     H DECEDIT('0,') DATEDIT(*DMY.)
000200020807     F* TRTR55R  *-----------------------------------------------------*
000300020807     F*------------------*                                             *
000400020807     F* PREPARAZIONE ARCHIVIO TABELLE DA TRASMETTERE  DA SEDE A FILIALE*
000500020807     F*----------------------------------------------------------------*
000600900510     FTABEL00F  UF   E           K DISK
000700930224     FTABEL00T  UF A E             DISK    USROPN
000800900510     F                                     RENAME(TABEL:TABELT)
000900900510     FAZORG01L  IF   E           K DISK
001000020806     d*
001100020806     d* DS ESTERNE
001200020806     d*
001300020806     D DSCMZ         E DS
001400020806     D KPJBA         E DS
001500020806     D TIBS50        E DS                  EXTNAME(TIBS55DS)
001600020807     D cncr80        E DS
001700020807     D UT§DSE0F      E DS
001800020806     D tibs56ds      E DS
001900020806     d  ski                    7    756  0 dim(250)
002000020806     d*
002100020806     d* SCHIERE
002200020807     d*
002300020807     D MOV             S              7  0 DIM(1000)                            MOV. X FILIALE
002400020808     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC
002500020808     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)              QCAEXEC
002600020808     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
002700980224     D*---------------------
002800980224     D* DS PARAMETRI "TIBS76C"
002900980224     D*------------------
003000980224     D PARAM           DS                  INZ
003100980224     D  PARSIM                 1      1
003200980224     D  PARCOD                 2      3
003300980224     D  PARFGE                 4      4
003400980224     D  PARLBX                 5     14
003500020806     D filcom          s              6    inz('000000')
003600900430     C*---------------------------------------------------------------*
003700900430     C     *ENTRY        PLIST
003800900430     C                   PARM                    KPJBA
003900020807     C                   Z-ADD     1             CODUT
004000020807     C                   CALL      'X§PARUT'
004100020807     C                   PARM                    UT§DSE0F
004200020807     C                   MOVEL     REC80         CNCR80
004300900430     C*---------------------------------------------------------------*
004400900510     C     KTAB          KLIST
004500900510     C                   KFLD                    CODUT
004600900510     C                   KFLD                    COD               2
004700020806     C* carica nella schiera FIL i p.o presenti sull'as
004800900510     C                   EXSR      CARTAB
004900020807     C*NUMERO TRASMISSIONE = UDATE USA
005000920212     C                   MOVE      *ZERO         NUMTRA            6 0
005100920212     C                   Z-ADD     UDATE         NUMTRA                         *
005200920414     C                   MOVE      UDAY          NUMTRA                         *
005300920414     C                   MOVEL     UYEAR         NUMTRA                         *
005400020807     C                   XFOOT     SKI           TOTFIL           15 0
005500920212     C*
005600900510     C     *LOVAL        SETLL     TABEL
005700900430     C*
005800900430     C     SU            TAG
005900900430     C                   READ      TABEL                                  02
006000900430     C   02              GOTO      FINE
006100910513     C* CANCELLA FISICAMENTE I DELETATI LOGICI GIA' TRASMESSI
006200910513     C* O DA NON TRASMETTERE
006300910513     C     TBLFLG        IFEQ      '*'                                          ANN.TO
006400910513     C     TBLFTT        IFNE      ' '                                          DA TRASM.
006500910513     C     TBLFTR        ANDNE     ' '                                          TRASM/RICEV.
006600910513     C     TBLFTT        OREQ      ' '                                          DA NON TRASM.
006700020807     C     TBLFTT        ORNE      *BLANKS                                      DA TRASMETTERE
006800020807     C     TOTFIL        ANDEQ     *ZEROS                                       NO FIL IN TRASM
006900910513     C                   DELETE    TABEL
007000980226     C                   GOTO      SU
007100910513     C                   END
007200910513     C                   END
007300020806     C** TABELLA DA NON TRASMETTERE
007400900430     C     TBLFTT        CABEQ     ' '           SU
007500020806     C** TABELLA GIA TRASMESSA
007600900430     C     TBLFTR        CABNE     ' '           SU
007700020806     C** N06 FILIALE NON TROVATA
007800900510     C     TBLFLT        IFGT      *ZERO
007900020806     C     TBLFLT        CHAIN     AZORG01L                           06
008000020806     C     *IN06         IFEQ      '1'
008100980114     C     TBLFLG        IFEQ      '*'                                          ANN.TO
008200980114     C                   DELETE    TABEL
008300980114     C                   END
008400980114     C                   GOTO      SU
008500900510     C                   END
008600980114     C                   END
008700900510     C** SE LA FILIALE E' A ZERO VA' TRASMESSA A TUTTI
008800900430     C                   EXSR      CRETAB
008900020806     c*
009000900430     C                   MOVEL     'T'           TBLFTR
009100900430     C                   Z-ADD     NUMTRA        TBLDTR
009200900430     C                   UPDATE    TABEL
009300900430     C                   GOTO      SU
009400900430     C     FINE          TAG
009500900514     C                   EXSR      X25
009600971202     C* Imposto i dati in BS50
009700971202     C                   CLEAR                   TIBS50
009800971202     C                   MOVEL     'C'           I50TLA
009900020806     C                   CALL      'TIBS55R'
010000971202     C                   PARM                    TIBS50
010100971202     C*
010200900430     C                   SETON                                        LR
010300900430     C/SPACE 005
010400900430     C**----------------------------------
010500900430     C** CREA RECORD TABEL00T
010600900430     C**----------------------------------
010700900430     C     CRETAB        BEGSR
010800020806     C*
010900020806     C                   MOVEL     SIMFEL        FILCOM
011000020806     C*
011100020806     C     TBLFLT        ifne      *ZERO                                        FILIALE 0
011200020806     C                   move      tblflt        FILCOM
011300020806     C                   Z-ADD     TBLFLT        B
011400020806     c                   else
011500020807     C                   move      *ZEROS        FILCOM
011600020806     C                   Z-ADD     1000          B
011700020806     c                   end
011800020807     C                   EXSR      CAEX                                         APERTURA MBR
011900020807     C                   ADD       1             MOV(B)
012000020807     C                   WRITE     TABELT
012100900430     C                   ENDSR
012200900511     C/SPACE 005
012300900514     C**-------------------------------------
012400900514     C** PREPARA SOTTOMISSIONE TRASMISSIONE
012500900514     C**-------------------------------------
012600900511     C     X25           BEGSR
012700900514     C   99              CLOSE     TABEL00T
012800900514     C                   DO        999           B
012900900511     C     MOV(B)        IFGT      *ZERO
013000900511     C     B             COMP      1000                                   10     MEMBRO 0
013100900514     C  N10              DO
013200900517     C                   MOVE      B             §AS4              3             DESTINAT.
013300900515     C                   MOVE      'S'           §CLR              1             CLEAR
013400971201     C* Richiamo pgm x richiesta librerie - S.Informativi
013500971201     C* da/a cui spedire
013600971201     C                   EXSR      RICLIB
013700971201     C                   EXSR      X25T
013800900514     C                   END
013900900511     C                   END
014000900514     C                   END
014100930224     C*
014200900514     C* TRASMETTE A TUTTI IL MEMBRO 0
014300020807     C                   Z-ADD     1000          B                 4 0
014400900514     C     MOV(B)        IFGT      *ZERO
014500900514     C                   SETON                                        10
014600020807     C                   DO        250           C                 3 0
014700020807     C     SKI(C)        IFGT      *ZERO
014800020807     C                   MOVE      SKI(C)        §AS4
014900900515     C                   MOVE      'N'           §CLR                            CLEAR
015000971202     C* Richiamo pgm x richiesta librerie - S.Informativi
015100971202     C* da/a cui spedire
015200971202     C                   EXSR      RICLIB
015300900514     C                   EXSR      X25T
015400900514     C                   END
015500900514     C                   END
015600930224     C*
015700930224     C* CANCELLO RECORDS DEL MEMBRO ZERO
015800930224     C*
015900930224     C                   SETOFF                                       99
016000930224     C                   MOVE      *ZERO         FILCOM
016100930224     C                   EXSR      CAEX                                         APERTURA MBR
016200930224     C                   DO        *HIVAL
016300930224     C                   READ      TABEL00T                               90
016400930224     C  N90              DELETE    TABELT
016500930224     C  N90              END
016600930224     C*
016700900517     C                   END
016800900511     C                   ENDSR
016900900514     C/SPACE 005
017000971201     C**---------------------------------------------------------*
017100971201     C** Richiamo pgm x reperimento Sif. e lib a cui trasmettere
017200971201     C**---------------------------------------------------------*
017300971201     C     RICLIB        BEGSR
017400971201     C*
017500971201     C                   CLEAR                   TIBS50
017600971201     C* PRELEVO LIBRERIE DEI FILE DI TRASMISSIONE E S.I.
017700971201     C*  A CUI TRASMETTERE
017800971201     C                   MOVEL     KNSIF         I50PSI
017900971201     C                   MOVEL     '046'         I50PPO
018000971202     C                   MOVEL     §AS4          I50APO
018100971201     C                   MOVEL     ' '           I50TLA
018200020806     C                   CALL      'TIBS55R'
018300971201     C                   PARM                    TIBS50
018400971201     C*
018500971201     C                   ENDSR
018600900514     C**----------------------------------
018700900515     C** SOTTOMETTE TRASMISSIONE
018800900514     C**----------------------------------
018900900514     C     X25T          BEGSR
019000020807     C                   MOVEL     §AS4          CMZDST                          DESTINAT.
019100900514     C                   MOVEL     'TABEL00T'    CMZFLD                          DA: >>>
019200020808     c* imposto il nome del membro solo se non è da spedire a tutti
019300020808     c                   if        not *in10
019400020808     C                   MOVE      §AS4          filcom                          DESTINAT.
019500020808     c                   else
019600020808     c                   move      '000'         filcom
019700020808     c                   end
019800020807     C                   EVAL      CMZMBD = 'M' + FILCOM
019900020808     C                   MOVEL     O50plg        CMZLBD
020000900514     C                   MOVEL     'TABEL00R'    CMZFLA                          A: >>>
020100020807     C                   EVAL      CMZMBA = 'M' + FILCOM
020200020808     C                   MOVEL     O50alg        CMZLBA
020300900514     C                   MOVE      '0000'        CMZORI                          ORA
020400900514     C                   MOVE      '0000000'     CMZDTI                          DATA
020500900514     C                   MOVE      'T'           CMZURG                          NOTTE
020600900514     C                   MOVE      'A'           CMZADD                          IN ADD
020700900515     C                   MOVE      §CLR          CMZCLR                          CLEAR
020800900514     C                   MOVE      'S'           CMZFTR                          DA TRANS.
020900900514     C                   MOVE      *BLANK        CMZERR                          1= ERR.
021000900514     C                   MOVE      'TR56'        CMZAZN                          AZIONE
021100020808     C                   MOVEL     O50asi        CMZSYF                          SIST.INF.
021200900514     C                   MOVE      MOV(B)        CMZNRR                          N.REC.
021300900514     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
021400971202     C                   MOVE      *BLANK        W236            236
021500020807     C                   CALL      'TIBS53C'
021600971202     C                   PARM                    O50LCL
021700971202     C                   PARM                    CMZDST
021800900514     C                   PARM                    CMZFLD
021900900514     C                   PARM                    CMZMBD
022000900514     C                   PARM                    CMZLBD
022100900514     C                   PARM                    CMZFLA
022200900514     C                   PARM                    CMZMBA
022300900514     C                   PARM                    CMZLBA
022400900514     C                   PARM                    CMZORI
022500900514     C                   PARM                    CMZDTI
022600900514     C                   PARM                    CMZURG
022700900514     C                   PARM                    CMZADD
022800900514     C                   PARM                    CMZCLR
022900900514     C                   PARM                    CMZFTR
023000900514     C                   PARM                    CMZERR
023100900514     C                   PARM                    CMZAZN
023200900514     C                   PARM                    CMZSYF
023300900514     C                   PARM                    CMZNRR
023400900514     C                   PARM                    CMZPRR
023500971202     C                   PARM                    W236
023600900514     C                   ENDSR
023700900510     C/SPACE
023800900514     C**-----------------------------------------
023900020806     C** CARICAMENTO SCHIERE: FIL e AS4
024000900514     C***----------------------------------------
024100900510     C     CARTAB        BEGSR
024200020806     C****  CARICO GLI AS A CUI TRASMETTERE
024300020806     c                   clear                   tibs56ds
024400020806     c                   eval      i56ppo=simfel
024500020806     c                   eval      i56mod='GRU'
024600020806     c                   call      'TIBS56R'
024700020806     c                   parm                    tibs56ds
024800020806     c                   endsr
024900900510     C**---------------------------------------------
025000900510     C** ESEGUE QCAEXEC SUL MEMBRO DI TABEL00T      -
025100900510     C**---------------------------------------------
025200900510     C     CAEX          BEGSR
025300020806     C                   Z-ADD     48            LUNG             15 5
025400900510     C** FILIALE DI COMPETENZA PER APERTURA MEMBRO
025500900510     C   99              CLOSE     TABEL00T
025600900510     C  N99              SETON                                        99
025700900510     C*+ CHIUSURA FILE SOLO DA SECONDO GIRO IN POI
025800900510     C                   SETOFF                                       20
025900900510     C                   MOVEL     *BLANKS       COMMAN
026000900510     C                   MOVEA     CMD3(1)       COMMAN           80
026100020807     C                   eval      %subst(comman: 42: 6) = FILCOM
026200900510     C                   CALL      'QCMDEXC'                            20
026300900510     C                   PARM                    COMMAN
026400900510     C                   PARM                    LUNG
026500900510     C** CONTROLLO SE ESISTE IL MEMBRO
026600900510     C   20              DO
026700900511     C                   MOVEL     *BLANKS       COMMAN
026800900510     C                   MOVEA     CMD1(1)       COMMAN
026900020807     C                   eval      %subst(comman: 42: 6) = FILCOM
027000900510     C                   CALL      'QCMDEXC'
027100900510     C                   PARM                    COMMAN
027200900510     C                   PARM                    LUNG
027300900510     C                   END
027400900510     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
027500900510     C                   MOVEL     *BLANKS       COMMAN
027600900510     C                   MOVEA     CMD2(1)       COMMAN
027700020807     C                   eval      %subst(comman: 42: 6) = FILCOM
027800900510     C                   CALL      'QCMDEXC'
027900900510     C                   PARM                    COMMAN
028000900510     C                   PARM                    LUNG
028100900510     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
028200900510     C                   OPEN      TABEL00T
028300900510     C                   ENDSR
028400900510     C/SPACE
028500900510**         CMD1
028600020806ADDPFM FILE(TABEL00T)               MBR(M000000)
028700900510**         CMD2
028800020806OVRDBF FILE(TABEL00T)               MBR(M000000)
028900900510**         CMD3
029000020806CHKOBJ OBJ(TABEL00T) OBJTYPE(*FILE) MBR(M000000)
