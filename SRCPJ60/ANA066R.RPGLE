000100941222     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000200941222     H*--------INIZIO SPECIFICHE --------------------------*
000300941222     H DECEDIT('0,') DATEDIT(*DMY.)
000400941222      *
000500941230    > *        ABILITAZIONE DA CAPOFILA A SOC.
000600941222      *        -------------------------------
000700941222      *
000800941222      *
000900941222      * DOCUMENTI MICROANALISI :
001000941222      *
001100941222      * NOTE TECNICHE :
001200941222      *
001300941222      *
001400941222      * PERSONALIZZAZIONI
001500941222      *
001600941222      *
001700941222      *
001800941222      *               MAPPA INDICATORI
001900941230      *
002000941230      *  21           GENERICO OPERAZIONI I/O
002100941230      *  22           GENERICO ERRORE OPERAZIONI I/O
002200941230      *  30           SFLDSP
002300941230      * N31           SFLCLR
002400941230      *  31           SFLDSPCTL
002500941230      *  32           SFLNXTCHG
002600941230      *  33           SFLEND
002700941230      *  39           OF PRTF
002800941230      *  40 <---> 49  DSPATR ERRORI SU SFL
002900941230    > *  Specificare l'uso dei singoli indicatori
003000941230      *  50 <---> 98  ERRORI SU VIDEO
003100941230    > *  Specificare l'uso dei singoli indicatori
003200941230      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003300941230      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003400941230      *  99           INDIC. GENERALE DI ERRORE
003500941230     F*----------------------------------------------------*
003600941230     F*------------
003700950428     FANDLI01L  IF A E           K DISK
003800980828     FANDLS01L  IF A E           K DISK
003801180105R275 F*------------
003802180105 "   F* Regole per detrazione IVA a Credito
003803180105 "   F* Key: Soc/Reg/Libro/DtInVal Unique
003804180105R275 FSDGRIC01L IF A E           K DISK
003900950428     D*
004000941230     D*-------------
004100941222     D* Passaggio Parametri
004200941222     D KPJBA         E DS
004300941222     D*-------------
004400950331     D*DS per passaggio dati pgm User Exit
004500950331     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) INZ
004600950331     D*-------------
004700941222     D* Parametri in ricezione
004800950428     D A066DS        E DS                  EXTNAME(ANA066DS)
004900941230     D*-------------
005000950331     D* Reperimento nome PGM
005100950331     D STATUS         SDS           333
005200950331     D  DSPGM            *PROC
005300950331     D  PARMS            *PARMS
005400941230     I/EJECT
005500941230     I*----------------------------------------------------*
005600941230     I*-------------
005700950428     I* Anagrafica libri IVA
005800950428     IANDLI000
005900950428     I              DLISOCIETA                  DLISOC
006000950428     I              DLISERIE                    DLISER
006100950428     I              DLIDESCR                    DLIDES
006200950428     I              DLIDESBREV                  DLIDSB
006300950428     I              DLITPREG                    DLITPR
006400950428     I              DLILIBRO                    DLILIB
006500950428     I              DLIKCCIVAN                  DLICIN
006600950428     I              DLIKSCIVAN                  DLISIN
006700950428     I              DLIKCCIVAI                  DLICII
006800950428     I              DLIKSCIVAI                  DLISII
006900950428     I              DLIIVAIND                   DLITII
007000950428     I              DLIAMBITO                   DLIAMB
007100950428     I              DLITERRITO                  DLITER
007200950428     I              DLISOSPENS                  DLISOS
007300950428     I              DLITPDOCUM                  DLITDC
007400950428     I              DLIVENTILA                  DLIVEN
007500950428     I              DLITPLIBRO                  DLITPL
007600980828     I*-------------
007700980828     I* Anagrafica slittamenti libri IVA
007800980828     IANDLS000
007900980828     I              DLSSOCIETA                  DLSSOC
008000980828     I              DLSTPREG                    DLSTPR
008100980828     I              DLSLIBRO                    DLSLIB
008200980828     I              DLSSLITIPO                  DLSTIP
008300980828     I              DLSSLITEMP                  DLSTEM
008400980828     I              DLSDTINVAL                  DLSDTI
008500980828     I              DLSDTFIVAL                  DLSDTF
008600950331     I*-------------
008700941222     C/EJECT
008800941230     C*----------------------------------------------------*
008900941230     C*                MAIN LINE PROGRAM
009000941230     C*----------------------------------------------------*
009100941230     C*
009200941230     C* inizializzazione variabili
009300941222     C                   EXSR      INZVAR
009400941230     C*
009500941230     C* solo se richiesta opzione di abilitazione
009600950428     C     OPZ066        IFEQ      '11'
009700941230     C*
009800941230     C                   DO
009900980828     C* copia FILE ANDLI00F
010000950428     C                   EXSR      CPYDLI
010100950428     C     ERR066        IFEQ      *ON
010200941230     C                   LEAVE
010300941230     C                   ENDIF
010400941230     C                   ENDDO
010500980828     C*
010600980828     C                   DO
010700980828     C* copia FILE ANDLS00F
010800980828     C                   EXSR      CPYDLS
010900980828     C     ERR066        IFEQ      *ON
011000980828     C                   LEAVE
011100980828     C                   ENDIF
011200980828     C                   ENDDO
011201180105R275 C*
011202180105 "   C                   DO
011203180105 "   C* copia FILE SDGRIC00F
011204180105 "   C                   EXSR      CPYRIC
011205180105 "   C     ERR066        IFEQ      *ON
011206180105 "   C                   LEAVE
011207180105 "   C                   ENDIF
011208180105R275 C                   ENDDO
011300941222     C*
011400950428     C     ERR066        IFEQ      *OFF
011500950403     C*
011600950403     C* richiama pgm User Exit
011700950403     C*
011800950403     C     USRERR        IFEQ      *BLANK
011900950403     C* imposta chiave univoca  per user exit
012000950428     C                   MOVE      SOC066        XUKSOC
012100950403     C                   MOVE      *BLANK        XUKDUN
012200950403     C                   MOVEL     DSPGM         XUKPGM
012300950428    >C                   MOVEL     TPR066        XUKFL1
012400950403    >C                   MOVEL     XUKDS         KPJBU
012500950403     C     DSPGM         CAT       'U':0         USREXT           10
012600950403     C                   CALL      USREXT                               21
012700950403     C                   PARM                    KPJBA
012800950403     C     *IN21         IFEQ      *ON
012900950403     C                   MOVE      *ON           USRERR            1
013000950403     C                   ENDIF
013100950403     C                   ENDIF
013200950428     C                   MOVE      '1'           OPR066
013300941222     C                   ELSE
013400950428     C***                  ROLBK
013500941230     C                   ENDIF
013600941222     C*
013700941222     C                   ENDIF
013800941222     C*
013900950428     C                   MOVEL     A066DS        KPJBU
014000941222     C                   SETON                                        RT
014100941222     C*
014200941222     C/EJECT
014300941222     C************************************************************
014400980828     C* Copia i record da capofila a società per ANDLI00F
014500941222     C************************************************************
014600950428     C     CPYDLI        BEGSR
014700941222     C*
014800950428     C                   MOVEL     CAP066        DLISOC
014900950428     C                   MOVEL     TPR066        DLITPR
015000950428     C                   MOVEL     LIB066        DLILIB
015100941222     C*
015200941222     C* aggancio record capofila
015300950428     C     K3DLI1        SETLL     ANDLI01L                           21
015400950428     C     K3DLI1        READE     ANDLI01L                               21
015500941222     C*
015600941222     C     *IN21         DOWEQ     *OFF
015700950428     C                   MOVE      SOC066        DLISOC
015800950428     C     K3DLI1        SETLL     ANDLI01L                               22
015900950428     C*
016000950428     C     *IN22         IFEQ      *OFF
016100950428     C                   WRITE     ANDLI000                             22
016200950428     C                   ENDIF
016300950428     C*
016400950428     C                   MOVEL     CAP066        DLISOC
016500950428     C     K3DLI1        READE     ANDLI01L                               21
016600950428     C                   ENDDO
016700941222     C*
016800941222     C     *IN22         IFEQ      *ON
016900950428     C                   MOVE      '1'           ERR066
017000941222     C                   ENDIF
017100941222     C                   ENDSR
017200941223     C/EJECT
017300980828     C************************************************************
017400980828     C* Copia i record da capofila a società per ANDLS00F
017500980828     C************************************************************
017600980828     C     CPYDLS        BEGSR
017700980828     C*
017800980828     C                   MOVEL     CAP066        DLSSOC
017900980828     C                   MOVEL     TPR066        DLSTPR
018000980828     C                   MOVEL     LIB066        DLSLIB
018100980828     C*
018200980828     C* aggancio record capofila
018300980828     C     K3DLS1        SETLL     ANDLS01L                           21
018400980828     C     K3DLS1        READE     ANDLS01L                               21
018500980828     C*
018600980828     C     *IN21         DOWEQ     *OFF
018700980828     C                   MOVE      SOC066        DLSSOC
018800980828     C     K3DLS1        SETLL     ANDLS01L                               22
018900980828     C*
019000980828     C     *IN22         IFEQ      *OFF
019100980828     C                   WRITE     ANDLS000                             22
019200980828     C                   ENDIF
019300980828     C*
019400980828     C                   MOVEL     CAP066        DLSSOC
019500980828     C     K3DLS1        READE     ANDLS01L                               21
019600980828     C                   ENDDO
019700980828     C*
019800980828     C     *IN22         IFEQ      *ON
019900980828     C                   MOVE      '1'           ERR066
020000980828     C                   ENDIF
020100980828     C                   ENDSR
020200980828     C/EJECT
020201180105R275 C************************************************************
020202180105 "   C* Copia i record da capofila a società per ANDLS00F
020203180105 "   C************************************************************
020204180105R275 C     CPYRIC        BEGSR
020205180105     C*
020206180105     C                   MOVEL     CAP066        RICSocieta
020207180105     C                   MOVEL     TPR066        RICTpReg
020208180105     C                   MOVEL     LIB066        RICLibro
020209180105     C*
020210180105     C* aggancio record capofila
020211180105     C     K03RIC01      SETLL     SDGRIC01L                          21
020212180105     C     K03RIC01      READE     SDGRIC01L                              21
020213180105     C*
020214180105     C     *IN21         DOWEQ     *OFF
020215180108     C                   MOVE      SOC066        RICSocieta
020216180108     C     K03RIC01      SETLL     SDGRIC01L                          22
020217180105     C*
020218180108     C     *IN22         IFEQ      *OFF
020219180108     C                   WRITE     SDGRIC000                            22
020220180105     C                   ENDIF
020221180105     C*
020222180105     C                   MOVEL     CAP066        RICSocieta
020223180105     C     K03RIC01      READE     SDGRIC01L                              21
020224180105     C                   ENDDO
020225180105     C*
020226180105     C     *IN22         IFEQ      *ON
020227180105     C                   MOVE      '1'           ERR066
020228180105     C                   ENDIF
020229180105R275 C                   ENDSR
020230180105R275 C/EJECT
020300941222     C************************************************************
020400941222     C* INIZIALIZZAZIONE VARIABILI
020500941222     C************************************************************
020600941222     C     INZVAR        BEGSR
020700941222     C*
020800941222     C     *ENTRY        PLIST
020900941222     C                   PARM                    KPJBA
021000950428    >C                   MOVEL     KPJBU         A066DS
021100941222     C*
021200941222     C* Dati da PGM chiamante
021300941222     C*
021400941222     C                   ENDSR
021500941222     C/EJECT
021600941222     C************************************************************
021700941222     C* INIZIALIZZAZIONE VARIABILI
021800941222     C************************************************************
021900941222     C     DEFCAM        BEGSR
022000950428     C* ANDLI01L
022100950428     C     K3DLI1        KLIST
022200950428     C                   KFLD                    DLISOC
022300950428     C                   KFLD                    DLITPR
022400950428     C                   KFLD                    DLILIB
022500980828     C*
022600980828     C* ANDLS01L
022700980828     C     K3DLS1        KLIST
022800980828     C                   KFLD                    DLSSOC
022900980828     C                   KFLD                    DLSTPR
023000980828     C                   KFLD                    DLSLIB
023001180105R275 C*
023002180105 "   C* SDGRIC01L
023003180105 "   C     K03RIC01      KLIST
023004180105 "   C                   KFLD                    RICSocieta
023005180105 "   C                   KFLD                    RICTpReg
023006180105R275 C                   KFLD                    RICLibro
023100941230     C*
023200941223     C                   ENDSR
