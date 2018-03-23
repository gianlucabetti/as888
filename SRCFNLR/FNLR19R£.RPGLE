000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200021008      * FNLR19R *-----------------------------------------------------*
000300021008      *    RICEZIONE FOGLI VIAGGIO PARTENZE ALLA FILIALE DI ARRIVO    *
000400021008      *---------------------------------------------------------------*
000500021008      *  LEGGENDO SOLO FNFVA01R DEVO AGGIORNARE SIA FNFVA00F CHE
000600021008      *  FNFVV00F, POICHE' FNFVV VIENE TRASMESSO (DA FNLR76R) COME
000700021008      *  SE FOSSE FVA (ANCHE SE MANCANO DEI CAMPI)
000800021008      *---------------------------------------------------------------*
000900950109     FFNFVA01L  UF A E           K DISK
001000000428     FFNFWA01L  UF A E           K DISK
001100951113     FFNFVA01R  UF   E           K DISK    USROPN
001200950109     F                                     RENAME(FNFVA000:FNFVA0RR)
001300000428     FFNFWA00R  UF   E           K DISK    USROPN
001400000428     F                                     RENAME(FNFWA000:FNFWA0RR)
001500950109     D*
001600960129     D* DEFINIZIONE SCHIERE
001700021008     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
001800021008     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
001900021008     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
002000021008     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
002100021008     D CMD5            S             48    DIM(1) CTDATA PERRCD(1)
002200021008     D CMD6            S             48    DIM(1) CTDATA PERRCD(1)
002300950109     D*
002400911011     D WLBDAT          DS
002500950109     D  G02DAT                 1      8  0
002600950109     D  G02INV                 9     16  0
002700950109     D  G02ERR                17     17
002800950109     D  G02TGI                18     22  0
002900021008     D DSmembro        ds             7    inz
003000021008     D  MbrM                   1      1    inz('M')
003100021008     D  MbrPOP                 2      4  0 inz
003200021008     D  MbrPOA                 5      7  0 inz
003300021008     D*
003400000000     D KPJBA         E DS
003500911001     D  LEGFIL               483    492
003600911001     D  MBRFIL               493    502
003700040428     D* PER RIESPLOSIONE fogli
003800040428     D PARAM5          DS
003900040428     D  PA5FIL               237    246    INZ('FXXX999999')
004000040428     D  PA5MBR4              247    250    INZ
004100040428     D  PA5MBR               247    256    Inz
004200911001     D CNCR80        E DS
004300021008     D UT§DSE0F      E DS
004400021008     D* DS PER TIBS55R - RICERCA S.I. IN CUI SI E'
004500021008     D TIBS55DS      E DS
004600951114     D*
004700951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL22R - CONTROLLO ABBINAB.
004800951114     D DSTL22        E DS                  EXTNAME(TNTL22DS)
004900951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
005000951114     D DSTL35        E DS                  EXTNAME(TNTL35DS)
005100021008     D*
005200000000     C*---------------------------------------------------------------*
005300000000     C     *ENTRY        PLIST
005400000000     C                   PARM                    KPJBA
005500950109     C*
005600000000     C                   Z-ADD     1             CODUT
005700911001     C                   CALL      'X§PARUT'
005800021008     C                   PARM                    UT§DSE0F
005900911001     C                   MOVEL     REC80         CNCR80
006000951114     C*
006100900618     C*---------------------------------------------------------------*
006200911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
006300911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
006400911001     C     MBRFIL        IFEQ      *BLANK
006500021008     c                   reset                   DSmembro
006600021008     c                   z-add     SIMfel        MbrPOP
006700021008     c                   z-add     888           MbrPOA
006800021008     c                   movel     DSmembro      MBRfil
006900911001     C                   END
007000021008     C*
007100021008     C                   movel     MBRfil        DSmembro
007200911001     C*
007300911001     C* ALLOCO MEMBRI
007400911001     C                   EXSR      CAEX
007500000428     C   21
007600000428     COR 20              GOTO      FINE
007700911001     C*
007800951114     C     *LOVAL        SETLL     FNFVA01R
007900951114     C                   READ      FNFVA01R                               31
008000951114    1C     *IN31         DOWEQ     '0'
008100950123     C*
008200950123     C* TESTANDO LA CATEGORIA, VEDO SE DEVO SCARICARE SU FNFVV O FNFVA
008300041124     c* 24/11/04 --> ora riceviamo solo delle cat 1 per cui eliminato
008400041124     c*              pezzo su fvv
008500951114    2C     FVANPG        IFEQ      1                                            1=PARTENZE
008600911001     C*
008700921123     C* 21 ON - GIA' ESISTENTE:    AGGIORNO
008800950109     C     KFVA          CHAIN     FNFVA01L                           21
008900000428     C     KFVA          CHAIN     FNFWA01L                           25
009000000428     C  N25              MOVEL     'S'           WESFWA            1
009100000428     C   25              CLEAR                   WESFWA
009200000428     C*
009300960206     C  N21              MOVEL     FVANPG        WSNPG             1 0
009400951114     C  N21KFVA          CHAIN     FNFVA01R                           31
009500000428     C**
009600000428     C     KFVA          CHAIN     FNFWA00R                           26
009700000428     C*
009800911108     C*
009900960129     C* CONTROLLO SE DEVO SBINARE IL FOGLIO VIAGGIO
010000960129     C     FVASBN        IFEQ      'S'
010100911108     C                   Z-ADD     0             FVADFA
010200911108     C                   Z-ADD     0             FVANFA
010300950123     C                   Z-ADD     *ZERO         FVANPG
010400960206     C                   ELSE
010500960206     C  N21              MOVEL     WSNPG         FVANPG
010600960129     C                   ENDIF
010700960129     C* SE RIRICEVO UN FV LO SBINO (SE DEVE ESSERE SBINATO) MA LASCIO
010800960129     C*   LA DATA E ORA REALE CHE ERA STATA IMPOSTATA LA PRIMA VOLTA
010900921117     C   21              Z-ADD     0             FVAAED
011000921117     C   21              Z-ADD     0             FVAAEH
011100951204     C   21              CLEAR                   FVARTA
011200960206     C   21              Z-ADD     0             FVADFA
011300960206     C   21              Z-ADD     0             FVANFA
011400960206     C   21              Z-ADD     *ZERO         FVANPG
011500951127     C*
011600951127     C* PULIZIA CAMPI CHE PROTEBBERO RIMANERE IMPOSTATI IN MODO ERRATO
011700951127     C*   IN QUANTO NON SONO PRESENTI NEL FILE DI RICEZIONE FNFVA01R
011800951127     C     *IN21         IFEQ      *ON
011900951127     C     *IN21         OREQ      *OFF
012000951127     C     FVATRN        ANDEQ     0
012100951127     C                   CLEAR                   FVADTN
012200951127     C                   CLEAR                   FVADTA
012300951127     C                   CLEAR                   FVAHTA
012400960201     C                   CLEAR                   FVADTP
012500960201     C                   CLEAR                   FVAHTP
012600951127     C                   ENDIF
012700951127     C*
012800950109     C                   Z-ADD     DATEU         FVADTR                         DATA TRASMISSIONE
012900950109     C                   MOVEL     'R'           FVAFTR                         'R'=RICEZIONE
013000950109     C   21              WRITE     FNFVA000
013100950109     C  N21              UPDATE    FNFVA000
013200000428     C*
013300000428     C*     ESISTE FWA DI RICEZIONE
013400000428     C  N26WESFWA        IFEQ      'S'
013500000428     C                   UPDATE    FNFWA000
013600000428     C                   ELSE
013700000428     C                   WRITE     FNFWA000
013800000428     C                   ENDIF
013900000428     C* NON ESISTE FWA DI RICEZIONE
014000000428     C   26WESFWA        IFEQ      'S'
014100000428     C                   DELETE    FNFWA000
014200000428     C                   ENDIF
014300950123     C*
014400951114     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
014500951114    3C     FVATRN        IFGT      0
014600951114     C                   EXSR      CTRTRN
014700951114    3C                   ENDIF
014800951114    2C                   ENDIF
014900911108     C*
015000950109     C                   DELETE    FNFVA0RR
015100000428     C  N26              DELETE    FNFWA0RR
015200000502     C  N26              ADD       1             NUMFWA            7 0
015300911001     C                   ADD       1             NUMREC            7 0
015400911001     C*
015500951114     C                   READ      FNFVA01R                               31
015600951114    1C                   END
015700900911     C*
015800041124     c                   setoff                                       202122
015900900409     C     FINE          TAG
016000040428     c* Se allocazione, risottometto ricezione
016100040428     c                   if        not *in22  and *in20   or *in21
016200040428     c
016300040428     c                   Eval      Pa5Mbr = MbrFil
016400040428     c                   Movel     Pa5Mbr4       PA5FIL
016500040428     C                   MOVEL     'K'           PA5FIL
016600040428     C                   MOVE      PARAM5        KPJBU
016700040428     c
016800040428     C                   MOVEL     'LR19'        KCOAZ
016900040428     C                   CALL      'BCH10'
017000040428     C                   PARM                    KPJBA
017100040428     c                   endif
017200951114     C*
017300951114     C* CALL A VUOTO AL PGM TNTL22R PER CHIUDERE I FILES
017400951114     C     WTL22         IFEQ      '1'
017500951114     C                   MOVEL     'C'           D22TLA                         *TIPO LANCIO
017600951114     C                   MOVEL     DSTL22        KPJBU
017700951114     C                   CALL      'TNTL22R'
017800951114     C                   PARM                    KPJBA
017900951114     C                   ENDIF
018000951114     C* CALL A VUOTO AL PGM TNTL35R PER CHIUDERE I FILES
018100951114     C     WTL35         IFEQ      '1'
018200951114     C                   MOVEL     'C'           D35TLA                         *TIPO LANCIO
018300951114     C                   MOVEL     DSTL35        KPJBU
018400951114     C                   CALL      'TNTL35R'
018500951114     C                   PARM                    KPJBA
018600951114     C                   ENDIF
018700951114     C*
018800021008     C                   MOVEL     *ON           *INLR
018900951114     C*
019000951114     C*--- ESEGUE QCAEXEC DI APERTURA SUI MEMBRI ---------------------*
019100911001     C     CAEX          BEGSR
019200040428     c                   setoff                                       22
019300000428     C** S.I. IN CUI SONO
019400021008     C                   CLEAR                   TIBS55DS
019500000428     C                   MOVEL     KNSIF         I50PSI
019600000428     C                   MOVEL     'L'           I50TLA
019700021008     C                   CALL      'TIBS55R'
019800021008     C                   PARM                    TIBS55DS
019900950109     C*
020000021008     C                   MOVE      *BLANK        VAR11             8
020100021008     C                   MOVE      *BLANK        VAR12             9
020200911001     C                   MOVE      ')'           VAR11
020300021008     C                   MOVEL     DSmembro      VAR11
020400911001     C                   MOVE      '))'          VAR12
020500021008     C                   MOVEL     DSmembro      VAR12
020600021008     C                   Z-ADD     48            LUNG             15 5
020700911001     C*
020800911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
020900911001     C                   SETOFF                                       20
021000951114     C                   MOVE      VAR11         CMD1                            FNFVA01R
021100911001     C                   MOVEL     *BLANKS       COMMAN
021200911001     C                   MOVEA     CMD1(1)       COMMAN           80
021300911001     C                   CALL      'QCMDEXC'                            20
021400911001     C                   PARM                    COMMAN
021500911001     C                   PARM                    LUNG
021600040428     c
021700040428     c* Se non esiste membro in fnfva non risottometto nemmeno
021800040428     c                   if        *in20
021900040428     c                   seton                                        22
022000040428     c                   endif
022100040428     c
022200911001     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
022300000428    1C  N20              DO
022400911001     C                   SETOFF                                       20
022500911001     C                   MOVE      VAR12         CMD2
022600911001     C                   MOVEL     *BLANKS       COMMAN
022700911001     C                   MOVEA     CMD2(1)       COMMAN
022800911001     C                   CALL      'QCMDEXC'                            20
022900911001     C                   PARM                    COMMAN
023000911001     C                   PARM                    LUNG
023100911001     C* OVRDBF
023200000428    2C  N20              DO
023300911001     C                   SETOFF                                       20
023400960129     C                   MOVE      VAR11         CMD3
023500911001     C                   MOVEL     *BLANKS       COMMAN
023600960129     C                   MOVEA     CMD3(1)       COMMAN
023700911001     C                   CALL      'QCMDEXC'                            20
023800911001     C                   PARM                    COMMAN
023900911001     C                   PARM                    LUNG
024000911001     C*
024100951114     C  N20              OPEN      FNFVA01R
024200000428    2C                   END
024300000428     C**
024400000428    2C     *IN20         IFEQ      *OFF
024500000428     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE LO CREO
024600000428     C                   SETOFF                                       21
024700000428     C                   MOVE      VAR11         CMD4                            FNFWA00R
024800000428     C                   MOVEL     *BLANKS       COMMAN
024900000428     C                   MOVEA     CMD4(1)       COMMAN           80
025000000428     C                   CALL      'QCMDEXC'                            21
025100000428     C                   PARM                    COMMAN
025200000428     C                   PARM                    LUNG
025300000428     C*
025400000428    3C     *IN21         IFEQ      '1'
025500000428     C                   MOVEL     'FNFWA00R'    FISICO
025600000428     C                   EXSR      CRTMBR
025700000428    3C                   END
025800000428     C**
025900000428     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
026000000428    3C  N21              DO
026100000428     C                   MOVE      VAR12         CMD5
026200000428     C                   MOVEL     *BLANKS       COMMAN
026300000428     C                   MOVEA     CMD5(1)       COMMAN
026400000428     C                   CALL      'QCMDEXC'                            21
026500000428     C                   PARM                    COMMAN
026600000428     C                   PARM                    LUNG
026700000428     C* OVRDBF
026800000428    4C  N21              DO
026900000428     C                   MOVE      VAR11         CMD6
027000000428     C                   MOVEL     *BLANKS       COMMAN
027100000428     C                   MOVEA     CMD6(1)       COMMAN
027200000428     C                   CALL      'QCMDEXC'                            21
027300000428     C                   PARM                    COMMAN
027400000428     C                   PARM                    LUNG
027500000428     C*
027600000428     C  N21              OPEN      FNFWA00R
027700000428    4C                   END
027800000428    3C                   END
027900000428    2C                   END
028000000428    1C                   END
028100950109     C*
028200911001     C                   ENDSR
028300000428     C*
028400000428     C**-----------------------------------------------*
028500000428     C     CRTMBR        BEGSR
028600000428     C*
028700000428     C                   MOVEL     MBRFIL        FISMBR
028800000428     C                   CALL      'TRUL50C'
028900000428     C                   PARM                    FISICO           10
029000000428     C                   PARM                    FISMBR           10
029100000428     C                   PARM                    O50PLA
029200000428     C                   PARM                    LOGICO           10
029300000428     C                   PARM                    LOGMBR           10
029400000428     C                   PARM                    ULFLG             1
029500000428     C* RITENTO L'ALLOCAZIONE
029600000428     C                   CALL      'QCMDEXC'                            21
029700000428     C                   PARM                    COMMAN
029800000428     C                   PARM                    LUNG
029900000428     C*
030000000428     C                   ENDSR
030100951114     C*
030200951114     C*--- CONTROLLI SUL TRAINO ABBINATO -----------------------------*
030300951114     C     CTRTRN        BEGSR
030400951114     C*
030500951114     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
030600951114     C                   CLEAR                   DSTL22
030700951114     C                   Z-ADD     FVALNP        D22LNP
030800951114     C                   Z-ADD     FVANFV        D22NFV
030900951114     C                   Z-ADD     FVALAI        D22LAI
031000951114     C                   MOVEL     'F'           D22FFS
031100951114     C                   MOVEL     DSTL22        KPJBU
031200951114     C                   CALL      'TNTL22R'
031300951114     C                   PARM                    KPJBA
031400951114     C                   MOVEL     KPJBU         DSTL22
031500951114     C                   MOVEL     '1'           WTL22             1
031600951114     C*
031700951114     C* D22ERR <> " "  ---> TRAINO NON ABBINABILE: AZZERO I CAMPI DEL
031800951114     C*                     TRAINO SUL FILE FNFVA00F
031900951114    1C     D22ERR        IFNE      ' '
032000951114     C*
032100951114     C     KFVA          CHAIN     FNFVA01L                           30
032200951114    2C     *IN30         IFEQ      *OFF
032300951114     C                   CLEAR                   FVATRN
032400951114     C                   CLEAR                   FVADTN
032500951114     C                   CLEAR                   FVATFM
032600951127     C                   CLEAR                   FVADTA
032700951127     C                   CLEAR                   FVAHTA
032800960201     C                   CLEAR                   FVADTP
032900960201     C                   CLEAR                   FVAHTP
033000951114     C                   UPDATE    FNFVA000
033100951114    2C                   ENDIF
033200951114     C*
033300951114   X1C                   ELSE
033400951123     C*
033500960129     C     KFVA          CHAIN     FNFVA01L                           30
033600960129     C*
033700960130     C* CALCOLO LA DATA/ORA TEORICA DI ARRIVO DEL F.VIAGGIO SOLO SE
033800960130     C*   IL F.VIAGGIO NON E' ILLUMINATO
033900960130    2C     FVAATR        IFEQ      ' '
034000960130     C*
034100960129     C* CALCOLO LA DATA TEORICA DI ARRIVO   DEL FOGLIO VIAGGIO E LA
034200960129     C*   SCRIVO SU FNFVA00F
034300951114     C                   CLEAR                   DSTL35
034400951123     C                   MOVEL     'A'           D35EPA
034500960129     C                   MOVEL     FVALAI        D35FIL
034600960129     C                   MOVEL     FVATFM        D35TFM
034700960129     C                   EXSR      CALT35
034800960129    3C     D35ERR        IFEQ      ' '
034900960129     C                   MOVEL     D35DTE        FVADTA
035000960129     C                   MOVEL     D35HTE        FVAHTA
035100960129     C                   MOVEL     D22DTN        FVADTN
035200960129    3C                   ENDIF
035300960130    2C                   ENDIF
035400960129     C*
035500960129     C* CALCOLO LA DATA TEORICA DI PARTENZA DEL FOGLIO VIAGGIO E LA
035600960129     C*   SCRIVO SU FNFVA00F
035700960129     C                   CLEAR                   DSTL35
035800960129     C                   MOVEL     'P'           D35EPA
035900960129     C                   MOVEL     D22NFM        D35NFM
036000960129     C                   EXSR      CALT35
036100960130    2C     D35ERR        IFEQ      ' '
036200960129     C                   MOVEL     D35DTE        FVADTP
036300960129     C                   MOVEL     D35HTE        FVAHTP
036400960129     C                   MOVEL     D22DTN        FVADTN
036500960130    2C                   ENDIF
036600951114     C*
036700960129     C* AGGIORNO FNFVA00F
036800951123     C                   UPDATE    FNFVA000
036900951114    1C                   ENDIF
037000951114     C*
037100951114     C                   ENDSR
037200951114     C*
037300960129     C*--- CALL PGM TNTL35R ------------------------------------------*
037400960129     C     CALT35        BEGSR
037500960129     C                   MOVEL     'D'           D35TUP
037600960129     C                   MOVEL     FVATRN        D35TRN
037700960129     C                   MOVEL     D22DDE        D35DDE
037800960129     C                   MOVEL     D22DTN        D35DTN
037900960129     C                   MOVEL     DSTL35        KPJBU
038000960129     C                   CALL      'TNTL35R'
038100960129     C                   PARM                    KPJBA
038200960129     C                   MOVEL     KPJBU         DSTL35
038300960129     C                   MOVEL     '1'           WTL35             1
038400960129     C                   ENDSR
038500960129     C*
038600951114     C*--- SR INIZIALE -----------------------------------------------*
038700951114     C     *INZSR        BEGSR
038800951114     C*
038900951114     C****  KLIST  ****
039000951114     C* ACCESSO FNFVA01L / FNFVA01R
039100951114     C     KFVA          KLIST
039200951114     C                   KFLD                    FVALNP
039300951114     C                   KFLD                    FVANFV
039400951114     C                   KFLD                    FVALAI
039500961016     C*
039600951114     C                   TIME                    WTIME            14 0          ORA & DATA
039700951114     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
039800951114     C*
039900951114     C* GIRO DATA DEL GIORNO
040000951114     C                   Z-ADD     WDATE         G02DAT
040100951114     C                   MOVEL     *BLANK        G02ERR
040200951114     C                   CALL      'XSRDA8'
040300951114     C                   PARM                    WLBDAT
040400951114     C                   Z-ADD     G02INV        DATEU             8 0
040500951114     C                   ENDSR
040600951114     C*
040700951114     C*---------------------------------------------------------------*
040800000428**         CMD1
040900021008CHKOBJ OBJ(FNFVA01R) OBJTYPE(*FILE) MBR(M000000)
041000911001**         CMD2
041100021008ALCOBJ OBJ((FNFVA01R *FILE *EXCL       M000000))
041200960129**         CMD3
041300021008OVRDBF FILE(FNFVA01R)               MBR(M890000)
041400000428**         CMD4
041500021008CHKOBJ OBJ(FNFWA00R) OBJTYPE(*FILE) MBR(M000000)
041600000428**         CMD5
041700021008ALCOBJ OBJ((FNFWA00R *FILE *EXCL       M000000))
041800000428**         CMD6
041900021008OVRDBF FILE(FNFWA00R)               MBR(M890000)
