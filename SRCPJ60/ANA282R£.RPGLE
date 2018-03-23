000100981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200951002     H*PARMS CVTOPT(*DATETIME)
000300951002     H DECEDIT('0,') DATEDIT(*DMY.)
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
003700950517     FANVCI02L  IF A E           K DISK    COMMIT
003800951002     F                                     USROPN
003900010221a0871x***ANTCM04L  IF A E           K DISK    COMMIT
004000010221a0871x***                                  USROPN
004100941230     D*-------------
004200941222     D* Passaggio Parametri
004300941222     D KPJBA         E DS
004400941222     D*-------------
004500950331     D*DS per passaggio dati pgm User Exit
004600950331     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) INZ
004700950331     D*-------------
004800941222     D* Parametri in ricezione
004900950517    >D A282DS        E DS                  EXTNAME(ANA282DS)
005000010221a0871D*-------------
005100010221  "  D* DRIVER copia commenti
005200010221  "  D Ana723ds      E DS                  INZ
005300010221a0871D  Cmt723       E                     INZ('0')
005400121205B1203D*-------------
005500121205  "  D* DRIVER copia traduzioni
005600121205  "  D ANA107DS1     E DS                  Inz
005700121205B1203D  Err1071      E                     Inz('0')
005800110408B1155x*-------------
005900110408  "  x* Parametri del PGM di gestione traduzioni
006000110408B1155x*XtraDs        E DS                  Inz
006100941230     D*-------------
006200950331     D* Reperimento nome PGM
006300950331     D STATUS         SDS           333
006400950517     D  STSUSR               254    263
006500950331     D  DSPGM            *PROC
006600950331     D  PARMS            *PARMS
006700000114A0735D ZDateISO        S               D
006800941230     I/EJECT
006900941230     I*----------------------------------------------------*
007000950517     IANVCI000
007100950517     I              VCISOCIETA                  VCISOC
007200950517     I              VCIUNITA                    VCIUNI
007300950517     I              VCIVOCE                     VCIVOC
007400950517     I              VCIKCC                      VCIKCC
007500950517     I              VCIKSC                      VCIKSC
007600950517     I              VCIDESCR                    VCIDES
007700950517     I              VCIDESBREV                  VCIDSB
007800950517     I              VCIUTEIMM                   VCIUTE
007900950517     I              VCIDTIMM                    VCIDTI
008000950517     I              VCIUTEULMO                  VCIULM
008100950517     I              VCIDTULMO                   VCIDTU
008200950517     I              VCIDTINVAL                  VCIDIV
008300950517     I              VCIDTFIVAL                  VCIDFV
008400950517     I              VCIUNMIS                    VCIUMS
008500950517     I              VCIRICQTA                   VCIQTA
008600950517     I              VCINATURA                   VCINAT
008700950517     I              VCILISTIN1                  VCILI1
008800950517     I              VCILISTIN2                  VCILI2
008900950517     I              VCILISTIN3                  VCILI3
009000950517     I              VCILISTIN4                  VCILI4
009100950517     I              VCICTGAN01                  VCIC01
009200950517     I              VCICTGAN02                  VCIC02
009300950517     I              VCICTGAN03                  VCIC03
009400950517     I              VCICTGAN04                  VCIC04
009500950517     I              VCICTGAN05                  VCIC05
009600950517     I              VCICTGAN06                  VCIC06
009700950517     I              VCICTGAN07                  VCIC07
009800950517     I              VCICTGAN08                  VCIC08
009900950517     I              VCICTGAN09                  VCIC09
010000950517     I              VCICTGAN10                  VCIC10
010100950517     I              VCICTGAN11                  VCIC11
010200950517     I              VCICTGAN12                  VCIC12
010300950517     I              VCICTGAN13                  VCIC13
010400950517     I              VCICTGAN14                  VCIC14
010500950517     I              VCICTGAN15                  VCIC15
010600950517     I              VCICTGAN16                  VCIC16
010700950517     I              VCICTGAN17                  VCIC17
010800950517     I              VCICTGAN18                  VCIC18
010900950517     I              VCICTGAN19                  VCIC19
011000950517     I              VCICTGAN20                  VCIC20
011100950517     I              VCICTGAN21                  VCIC21
011200950517     I              VCICTGAN22                  VCIC22
011300950517     I              VCICTGAN23                  VCIC23
011400950517     I              VCICTGAN24                  VCIC24
011500950517     I              VCICTGAN25                  VCIC25
011600950517     I              VCICTGAN26                  VCIC26
011700950517     I              VCICTGAN27                  VCIC27
011800950517     I              VCICTGAN28                  VCIC28
011900950517     I              VCICTGAN29                  VCIC29
012000950517     I              VCICTGAN30                  VCIC30
012100950517     I              VCILIVRIS                   VCILIV
012200950517     I              VCIRGRIPVA                  VCIRGR
012300950517     I              VCIRGABENT                  VCIRGA
012400950517     I              VCIRIFANA1                  VCIRA1
012500950517     I              VCITPRIFA1                  VCITA1
012600950517     I              VCIRIFANA2                  VCIRA2
012700950517     I              VCITPRIFA2                  VCITA2
012800950517     I              VCIRIFFRE1                  VCIRF1
012900950517     I              VCIRIFFRE2                  VCIRF2
013000950517     I              VCIRIFFRE3                  VCIRF3
013100950517     I              VCISESSTRA                  VCITRA
013200950517     I              VCISESSRIC                  VCIRIC
013300010221a0871x*-------------
013400010221  "  x* Anagrafica commenti
013500010221  "  x***ANTCM000
013600010221  "  x***           TCMSOCIETA                  TCMSOC
013700010221a0871x***           TCMUNITA                    TCMUNT
013800950331     I*-------------
013900941222     C/EJECT
014000941230     C*----------------------------------------------------*
014100941230     C*                MAIN LINE PROGRAM
014200941230     C*----------------------------------------------------*
014300941230     C*
014400941230     C* inizializzazione variabili
014500941222     C                   EXSR      INZVAR
014600941230     C*
014700941230     C* solo se richiesta opzione di abilitazione
014800950517     C     OPZ282        IFEQ      '11'
014900941230     C*
015000941230     C                   DO
015100941223     C* copia FILE
015200950517     C                   EXSR      CPYVCI
015300950517     C     ERR282        IFEQ      *ON
015400941230     C                   LEAVE
015500941230     C                   ENDIF
015600950123     C* copia FILE
015700950123     C                   EXSR      CPYTCM
015800950517     C     ERR282        IFEQ      *ON
015900950123     C                   LEAVE
016000950123     C                   ENDIF
016100121205B1203C* copia FILE
016200121205  "  C                   EXSR      CPYTRA
016300121205  "  C     ERR282        IFEQ      *ON
016400121205  "  C                   LEAVE
016500121205B1203C                   ENDIF
016600941230     C*
016700941230     C                   ENDDO
016800941222     C*
016900950517     C     ERR282        IFEQ      *OFF
017000950403     C*
017100950403     C* richiama pgm User Exit
017200950403     C*
017300950403     C     USRERR        IFEQ      *BLANK
017400950403     C* imposta chiave univoca  per user exit
017500950517     C                   MOVE      SOC282        XUKSOC
017600950403     C                   MOVE      *BLANK        XUKDUN
017700950403     C                   MOVEL     DSPGM         XUKPGM
017800950517    >C                   MOVEL     VOC282        XUKFL1
017900950517    >C                   MOVEL     KCC282        XUKFL2
018000950517    >C                   MOVEL     KSC282        XUKFL3
018100950403    >C                   MOVEL     XUKDS         KPJBU
018200950403     C     DSPGM         CAT       'U':0         USREXT           10
018300950403     C                   CALL      USREXT                               21
018400950403     C                   PARM                    KPJBA
018500950403     C     *IN21         IFEQ      *ON
018600950403     C                   MOVE      *ON           USRERR            1
018700950403     C                   ENDIF
018800950403     C                   ENDIF
018900941222     C                   COMMIT
019000950517     C                   MOVE      '1'           OPR282
019100941222     C                   ELSE
019200941222     C                   ROLBK
019300941230     C                   ENDIF
019400941222     C*
019500941222     C                   ENDIF
019600941222     C*
019700950517     C                   MOVEL     A282DS        KPJBU
019800941222     C                   SETON                                        RT
019900941222     C*
020000941222     C/EJECT
020100941222     C************************************************************
020200950517     C* Copia i record da capofila a società per ANVCI00F
020300941222     C************************************************************
020400950517     C     CPYVCI        BEGSR
020500941222     C*
020600950517     C                   MOVEL     CAP282        VCISOC
020700950517     C                   MOVEL     KCC282        VCIKCC
020800950517     C                   MOVEL     KSC282        VCIKSC
020900950517     C                   MOVEL     VOC282        VCIVOC
021000941222     C*
021100941222     C* aggancio record capofila
021200950517     C     K4VCI2        SETLL     ANVCI02L                           21
021300950517     C     K4VCI2        READE     ANVCI02L                               21
021400941222     C*
021500941222     C     *IN21         DOWEQ     *OFF
021600950517     C                   MOVE      SOC282        VCISOC
021700950517     C                   MOVE      WDATE         VCIDIV
021800950517     C                   MOVE      STSUSR        VCIUTE
021900950517     C                   MOVE      STSUSR        VCIULM
022000110408B1155** Reperisco traduzione per la società abilitata
022100110408  "  x*******            Exsr      RepTrad
022200110408B1155**      no, vanno scritti nella lingua della capofila
022300950517     C                   WRITE     ANVCI000                             22
022400950517     C                   MOVEL     CAP282        VCISOC
022500950517     C     K4VCI2        READE     ANVCI02L                               21
022600941222     C                   ENDDO
022700941222     C*
022800941222     C     *IN22         IFEQ      *ON
022900950517     C                   MOVE      '1'           ERR282
023000941222     C                   ENDIF
023100941222     C                   ENDSR
023200050630     C/EJECT
023300110408B1155*************************************************************
023400050630     ** Reperimento traduzione
023500110408B1155*************************************************************
023600110408     x*    RepTrad       BegSr
023700110408     x*
023800110408     x*                  Reset                   XtraDs
023900110408     x*
024000110408     x*                  Eval      XtrRic     = '1'
024100110408     x*                  Eval      XtrSoc     =  Soc282
024200110408     x*                  Eval      XtrFil     =  'ANVCI'
024300110408     x*                  Eval      XtrAlc     = *off
024400110408     x*                  Eval      XtrErr     = *off
024500110408     x*                  Eval      XtrPar     = 'ANVCI00F'
024600110408     x*                  Eval      XtrKey     = Voc282
024700110408     x* Indico al pgm di reperire le traduzione per la lingua della società
024800110408     x*                  Eval      XtrUtiLin  = 'S'
024900110408     x*
025000110408     x* Faccio due giri uno per la descrizione e uno per la descrizione breve
025100110408     x*                  Eval      x = *zeros
025200110408     x*                  Do        2             x                 2 0
025300110408     x*
025400110408     ** Chiamata al Pgm le le due descrizioni senza la lingua
025500110408     x*                  Eval      XtrLin     =  *blanks
025600110408     x*                  Select
025700110408     x*                  When      x = 1
025800110408     x*                  Eval      XtrFld = 'VCIVOCE'
025900110408     x*                  When      x = 2
026000110408     x*                  Eval      XtrFld = 'VCIDESBREV'
026100110408     x*                  Endsl
026200110408     x*                  Callb     'XTRA'
026300110408     x*                  Parm                    XtraDs
026400110408     x*
026500110408     x* Se vi sono errori pulisco traduzione
026600110408     x*                  If        XtrErr = *off
026700110408     x*                  Select
026800110408     x*                  When      x = 1
026900110408     x*                  Eval      VciDes = XtrTxt
027000110408     x*                  When      x = 2
027100110408     x*                  Eval      VciDsB = XtrTxt
027200110408     x*                  Endsl
027300110408     x*                  Endif
027400110408     x*
027500110408     x*                  Enddo
027600110408     x*
027700110408B1155x*                  EndSr
027800950123     C/EJECT
027900950123     C************************************************************
028000950123     C* Copia i record da capofila a società per ANTCM00F
028100950123     C************************************************************
028200950123     C     CPYTCM        BEGSR
028300950123     C*
028400010221a0871x***                MOVEL     CAP282        TCMSOC
028500010221  "  x***                MOVEL     *BLANK        TCMUNT
028600010221  "  x***                MOVEL     'VCA'         TCMTPA
028700010221  "  x***                MOVEL     VOC282        TCMKY1
028800010221  "  x***                MOVEL     KCC282        TCMKY2
028900010221  "  x***                MOVEL     KSC282        TCMKY3
029000010221  "  x*
029100010221  "  x* aggancio record capofila
029200010221  "  x***  K4TCM4        SETLL     ANTCM04L                           21
029300010221  "  x***  K4TCM4        READE     ANTCM04L                               21
029400010221  "  x*
029500010221  "  x***  *IN21         DOWEQ     *OFF
029600010221  "  x***                MOVE      SOC282        TCMSOC
029700010221  "  x***                WRITE     ANTCM000                             22
029800010221  "  x***                MOVEL     CAP282        TCMSOC
029900010221  "  x***  K4TCM4        READE     ANTCM04L                               21
030000010221  "  x***                ENDDO
030100010221  "  x*
030200010221  "  x***  *IN22         IFEQ      *ON
030300010221  "  x***                MOVE      '1'           ERR282
030400010221  "  x***                ENDIF
030500010221  "  C*
030600010221  "  C                   RESET                   ANA723DS
030700010221  "  C                   EVAL      OPZ723 = '03'
030800010301  "  C                   MOVEL     'VCA'         TPA723O
030900010221  "  C                   MOVEL     CAP282        SOC723O
031000010221  "  C                   MOVEL     VOC282        KY1723O
031100010221  "  C                   MOVEL     KCC282        KY2723O
031200010221  "  C                   MOVEL     KSC282        KY3723O
031300010221  "  C                   MOVEL     SOC282        SOC723N
031400010221  "  C                   MOVEL     VOC282        KY1723N
031500010221  "  C                   MOVEL     KCC282        KY2723N
031600010221  "  C                   MOVEL     KSC282        KY3723N
031700010221  "  C                   CALL      'ANA723R'
031800010221  "  C                   PARM                    KPJBA
031900010222  "  C                   PARM                    ana723ds
032000010221  "  C                   IF        ERR723 = *ON
032100010221  "  C                   MOVE      '1'           ERR282
032200010221a0871C                   ENDIF
032300950123     C                   ENDSR
032400941223     C/EJECT
032500121205B1203C************************************************************
032600121205  "  C* Copia i record da capofila a società per ANTRA00F
032700121205  "  C************************************************************
032800121205B1203C     CPYTRA        BEGSR
032900121205     C*
033000121205     C                   Reset                   ANA107DS1
033100121205     C*
033200121205     C                   Eval      SOP1071 = Cap282
033300121205     C                   Eval      SOA1071 = Soc282
033400121205     C                   Eval      FIL1071 = 'ANVCI'
033500121205     C                   Eval      KEY1071 = Voc282
033600121205     C                   Eval      FLD1071 = *Blanks
033700121205     C                   Eval      LIN1071 = *Blanks
033800121205     C*
033900121205     C                   Eval      KPJBU = ANA107DS1
034000121205     C                   Call      'ANA107R1'
034100121205     C                   Parm                    KPJBA
034200121205     C                   Eval      ANA107DS1 = Kpjbu
034300121205     C*
034400121205     C                   If        Err1071 <> *Off
034500121205     C                   Eval      Err282 = '1'
034600121205     C                   EndIf
034700121205     C*
034800121205B1203C                   ENDSR
034900121205B1203C/EJECT
035000941222     C************************************************************
035100941222     C* INIZIALIZZAZIONE VARIABILI
035200941222     C************************************************************
035300941222     C     INZVAR        BEGSR
035400941222     C*
035500941222     C     *ENTRY        PLIST
035600941222     C                   PARM                    KPJBA
035700950517    >C                   MOVEL     KPJBU         A282DS
035800941222     C*
035900961004     C                   CALLb     'XDT4000'
036000941223     C                   PARM                    XDTAMG           10
036100000114A0735C*****              PARM      UDATE         XDTGMA            6 0
036200000114A0735C                   PARM      ZDate         XDTGMA            6 0
036300941223     C                   PARM      4             XDTSTA            1 0
036400941230     C                   MOVEL     XDTAMG        WDATE
036500941222     C* Dati da PGM chiamante
036600941222     C*
036700941222     C                   ENDSR
036800941222     C/EJECT
036900941222     C************************************************************
037000941222     C* INIZIALIZZAZIONE VARIABILI
037100941222     C************************************************************
037200941222     C     DEFCAM        BEGSR
037300950517     C* AnVCI02l
037400950517     C     K4VCI2        KLIST
037500950517     C                   KFLD                    VCISOC
037600950517     C                   KFLD                    VCIKCC
037700950517     C                   KFLD                    VCIKSC
037800950517     C                   KFLD                    VCIVOC
037900010221a0871x* ANTCM04L
038000010221  "  x***  K4TCM4        KLIST
038100010221  "  x***                KFLD                    TCMSOC
038200010221  "  x***                KFLD                    TCMUNT
038300010221  "  x***                KFLD                    TCMTPA
038400010221a0871x***                KFLD                    TCMKY1
038500941230     C*
038600941230     C* Variabili appoggio specifiche della singola anagrafica
038700941230     C*
038800950517     C     *LIKE         DEFINE    VCIDFV        WDATE
038900941230     C*
039000941223     C                   ENDSR
039100951002     C/EJECT
039200951002     C************************************************************
039300951002     C* OPERAZIONI INIZIALI
039400951002     C************************************************************
039500951002     C     *INZSR        BEGSR
039600000114A0735C     *DMY          Move      Udate         ZDateISO
039700000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
039800951002     C*
039900951002     C* Eseguo strcmtctl
040000951002     C                   CALLb     'XSTRCMT'
040100951002     C                   OPEN      ANVCI02L
040200010221a0871x***                OPEN      ANTCM04L
040300951002     C*
040400951002     C                   ENDSR
