000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200021008      * FNLR19R *-----------------------------------------------------*
000300021008      *    RICEZIONE FOGLI VIAGGIO PARTENZE ALLA FILIALE DI ARRIVO    *
000400021008      *---------------------------------------------------------------*
000500021008      *  LEGGENDO SOLO FNFVA01R DEVO AGGIORNARE SIA FNFVA00F CHE
000600021008      *  FNFVV00F, POICHE' FNFVV VIENE TRASMESSO (DA FNLR76R) COME
000700021008      *  SE FOSSE FVA (ANCHE SE MANCANO DEI CAMPI)
000800021008      *---------------------------------------------------------------*
000900950109     FFNFVA01L  UF A E           K DISK
001000061019     FFNFgv01L  UF   E           K DISK
001100061019     FFNFv201L  UF a E           K DISK
001200061019     FFNFv203L  UF   E           K DISK    RENAME(FNFV2000:FNFV2003)
001300000428     FFNFWA01L  UF A E           K DISK
001400951113     FFNFVA01R  UF   E           K DISK    USROPN
001500950109     F                                     RENAME(FNFVA000:FNFVA0RR)
001600000428     FFNFWA00R  UF   E           K DISK    USROPN
001700000428     F                                     RENAME(FNFWA000:FNFWA0RR)
001800950109     D*
001900960129     D* DEFINIZIONE SCHIERE
002000021008     D CMD1            S             48    DIM(1) CTDATA PERRCD(1)
002100021008     D CMD2            S             48    DIM(1) CTDATA PERRCD(1)
002200021008     D CMD3            S             48    DIM(1) CTDATA PERRCD(1)
002300021008     D CMD4            S             48    DIM(1) CTDATA PERRCD(1)
002400021008     D CMD5            S             48    DIM(1) CTDATA PERRCD(1)
002500021008     D CMD6            S             48    DIM(1) CTDATA PERRCD(1)
002600950109     D*
002700911011     D WLBDAT          DS
002800950109     D  G02DAT                 1      8  0
002900950109     D  G02INV                 9     16  0
003000950109     D  G02ERR                17     17
003100950109     D  G02TGI                18     22  0
003200021008     D DSmembro        ds             7    inz
003300021008     D  MbrM                   1      1    inz('M')
003400021008     D  MbrPOP                 2      4  0 inz
003500021008     D  MbrPOA                 5      7  0 inz
003600021008     D*
003700000000     D KPJBA         E DS
003800911001     D  LEGFIL               483    492
003900911001     D  MBRFIL               493    502
004000040428     D* PER RIESPLOSIONE fogli
004100040428     D PARAM5          DS
004200040428     D  PA5FIL               237    246    INZ('FXXX999999')
004300040428     D  PA5MBR4              247    250    INZ
004400040428     D  PA5MBR               247    256    Inz
004500911001     D CNCR80        E DS
004600021008     D UT§DSE0F      E DS
004700021008     D* DS PER TIBS55R - RICERCA S.I. IN CUI SI E'
004800021008     D TIBS55DS      E DS
004900951114     D*
005000951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL22R - CONTROLLO ABBINAB.
005100951114     D DSTL22        E DS                  EXTNAME(TNTL22DS)
005200951114     D* DS PER PASSAGGIO PARAMETRI A PGM TNTL35R - CALCOLO DATA TEORICA
005300951114     D DSTL35        E DS                  EXTNAME(TNTL35DS)
005400021008     D*
005500061019     d wepa            s                   like(fv2epa)
005600061019     d wtdh            s                   like(fv2tdh)
005700061019     d wlai            s                   like(fv2lai)
005800000000     C*---------------------------------------------------------------*
005900000000     C     *ENTRY        PLIST
006000000000     C                   PARM                    KPJBA
006100950109     C*
006200000000     C                   Z-ADD     1             CODUT
006300911001     C                   CALL      'X§PARUT'
006400021008     C                   PARM                    UT§DSE0F
006500911001     C                   MOVEL     REC80         CNCR80
006600951114     C*
006700900618     C*---------------------------------------------------------------*
006800911001     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
006900911001     C* ASSUMO IL PRIMO MEMBRO DEL FILE
007000911001     C     MBRFIL        IFEQ      *BLANK
007100021008     c                   reset                   DSmembro
007200021008     c                   z-add     SIMfel        MbrPOP
007300021008     c                   z-add     888           MbrPOA
007400021008     c                   movel     DSmembro      MBRfil
007500911001     C                   END
007600021008     C*
007700021008     C                   movel     MBRfil        DSmembro
007800911001     C*
007900911001     C* ALLOCO MEMBRI
008000911001     C                   EXSR      CAEX
008100000428     C   21
008200000428     COR 20              GOTO      FINE
008300911001     C*
008400951114     C     *LOVAL        SETLL     FNFVA01R
008500951114     C                   READ      FNFVA01R                               31
008600951114    1C     *IN31         DOWEQ     '0'
008700950123     C*
008800950123     C* TESTANDO LA CATEGORIA, VEDO SE DEVO SCARICARE SU FNFVV O FNFVA
008900041124     c* 24/11/04 --> ora riceviamo solo delle cat 1 per cui eliminato
009000041124     c*              pezzo su fvv
009100951114    2C     FVANPG        IFEQ      1                                            1=PARTENZE
009200911001     C*
009300921123     C* 21 ON - GIA' ESISTENTE:    AGGIORNO
009400950109     C     KFVA          CHAIN     FNFVA01L                           21
009500000428     C     KFVA          CHAIN     FNFWA01L                           25
009600000428     C  N25              MOVEL     'S'           WESFWA            1
009700000428     C   25              CLEAR                   WESFWA
009800000428     C*
009900960206     C  N21              MOVEL     FVANPG        WSNPG             1 0
010000951114     C  N21KFVA          CHAIN     FNFVA01R                           31
010100000428     C**
010200000428     C     KFVA          CHAIN     FNFWA00R                           26
010300000428     C*
010400911108     C*
010500960129     C* CONTROLLO SE DEVO SBINARE IL FOGLIO VIAGGIO
010600960129     C     FVASBN        IFEQ      'S'
010700911108     C                   Z-ADD     0             FVADFA
010800911108     C                   Z-ADD     0             FVANFA
010900950123     C                   Z-ADD     *ZERO         FVANPG
011000960206     C                   ELSE
011100960206     C  N21              MOVEL     WSNPG         FVANPG
011200960129     C                   ENDIF
011300960129     C* SE RIRICEVO UN FV LO SBINO (SE DEVE ESSERE SBINATO) MA LASCIO
011400960129     C*   LA DATA E ORA REALE CHE ERA STATA IMPOSTATA LA PRIMA VOLTA
011500921117     C   21              Z-ADD     0             FVAAED
011600921117     C   21              Z-ADD     0             FVAAEH
011700951204     C   21              CLEAR                   FVARTA
011800960206     C   21              Z-ADD     0             FVADFA
011900960206     C   21              Z-ADD     0             FVANFA
012000960206     C   21              Z-ADD     *ZERO         FVANPG
012100951127     C*
012200951127     C* PULIZIA CAMPI CHE PROTEBBERO RIMANERE IMPOSTATI IN MODO ERRATO
012300951127     C*   IN QUANTO NON SONO PRESENTI NEL FILE DI RICEZIONE FNFVA01R
012400951127     C     *IN21         IFEQ      *ON
012500951127     C     *IN21         OREQ      *OFF
012600951127     C     FVATRN        ANDEQ     0
012700951127     C                   CLEAR                   FVADTN
012800951127     C                   CLEAR                   FVADTA
012900951127     C                   CLEAR                   FVAHTA
013000960201     C                   CLEAR                   FVADTP
013100960201     C                   CLEAR                   FVAHTP
013200951127     C                   ENDIF
013300951127     C*
013400950109     C                   Z-ADD     DATEU         FVADTR                         DATA TRASMISSIONE
013500950109     C                   MOVEL     'R'           FVAFTR                         'R'=RICEZIONE
013600950109     C   21              WRITE     FNFVA000
013700950109     C  N21              UPDATE    FNFVA000
013800000428     C*
013900000428     C*     ESISTE FWA DI RICEZIONE
014000000428     C  N26WESFWA        IFEQ      'S'
014100000428     C                   UPDATE    FNFWA000
014200000428     C                   ELSE
014300000428     C                   WRITE     FNFWA000
014400000428     C                   ENDIF
014500000428     C* NON ESISTE FWA DI RICEZIONE
014600000428     C   26WESFWA        IFEQ      'S'
014700000428     C                   DELETE    FNFWA000
014800000428     C                   ENDIF
014900950123     C*
015000951114     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
015100951114    3C     FVATRN        IFGT      0
015200951114     C                   EXSR      CTRTRN
015300951114    3C                   ENDIF
015400951114    2C                   ENDIF
015500911108     C*
015600950109     C                   DELETE    FNFVA0RR
015700000428     C  N26              DELETE    FNFWA0RR
015800000502     C  N26              ADD       1             NUMFWA            7 0
015900911001     C                   ADD       1             NUMREC            7 0
016000911001     C*
016100951114     C                   READ      FNFVA01R                               31
016200951114    1C                   END
016300900911     C*
016400041124     c                   setoff                                       202122
016500900409     C     FINE          TAG
016600040428     c* Se allocazione, risottometto ricezione
016700040428     c                   if        not *in22  and *in20   or *in21
016800040428     c
016900040428     c                   Eval      Pa5Mbr = MbrFil
017000040428     c                   Movel     Pa5Mbr4       PA5FIL
017100040428     C                   MOVEL     'K'           PA5FIL
017200040428     C                   MOVE      PARAM5        KPJBU
017300040428     c
017400040428     C                   MOVEL     'LR19'        KCOAZ
017500040428     C                   CALL      'BCH10'
017600040428     C                   PARM                    KPJBA
017700040428     c                   endif
017800951114     C*
017900951114     C* CALL A VUOTO AL PGM TNTL22R PER CHIUDERE I FILES
018000951114     C     WTL22         IFEQ      '1'
018100951114     C                   MOVEL     'C'           D22TLA                         *TIPO LANCIO
018200951114     C                   MOVEL     DSTL22        KPJBU
018300951114     C                   CALL      'TNTL22R'
018400951114     C                   PARM                    KPJBA
018500951114     C                   ENDIF
018600951114     C* CALL A VUOTO AL PGM TNTL35R PER CHIUDERE I FILES
018700951114     C     WTL35         IFEQ      '1'
018800951114     C                   MOVEL     'C'           D35TLA                         *TIPO LANCIO
018900951114     C                   MOVEL     DSTL35        KPJBU
019000951114     C                   CALL      'TNTL35R'
019100951114     C                   PARM                    KPJBA
019200951114     C                   ENDIF
019300951114     C*
019400021008     C                   MOVEL     *ON           *INLR
019500951114     C*
019600951114     C*--- ESEGUE QCAEXEC DI APERTURA SUI MEMBRI ---------------------*
019700911001     C     CAEX          BEGSR
019800040428     c                   setoff                                       22
019900000428     C** S.I. IN CUI SONO
020000021008     C                   CLEAR                   TIBS55DS
020100000428     C                   MOVEL     KNSIF         I50PSI
020200000428     C                   MOVEL     'L'           I50TLA
020300021008     C                   CALL      'TIBS55R'
020400021008     C                   PARM                    TIBS55DS
020500950109     C*
020600021008     C                   MOVE      *BLANK        VAR11             8
020700021008     C                   MOVE      *BLANK        VAR12             9
020800911001     C                   MOVE      ')'           VAR11
020900021008     C                   MOVEL     DSmembro      VAR11
021000911001     C                   MOVE      '))'          VAR12
021100021008     C                   MOVEL     DSmembro      VAR12
021200021008     C                   Z-ADD     48            LUNG             15 5
021300911001     C*
021400911001     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
021500911001     C                   SETOFF                                       20
021600951114     C                   MOVE      VAR11         CMD1                            FNFVA01R
021700911001     C                   MOVEL     *BLANKS       COMMAN
021800911001     C                   MOVEA     CMD1(1)       COMMAN           80
021900911001     C                   CALL      'QCMDEXC'                            20
022000911001     C                   PARM                    COMMAN
022100911001     C                   PARM                    LUNG
022200040428     c
022300040428     c* Se non esiste membro in fnfva non risottometto nemmeno
022400040428     c                   if        *in20
022500040428     c                   seton                                        22
022600040428     c                   endif
022700040428     c
022800911001     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
022900000428    1C  N20              DO
023000911001     C                   SETOFF                                       20
023100911001     C                   MOVE      VAR12         CMD2
023200911001     C                   MOVEL     *BLANKS       COMMAN
023300911001     C                   MOVEA     CMD2(1)       COMMAN
023400911001     C                   CALL      'QCMDEXC'                            20
023500911001     C                   PARM                    COMMAN
023600911001     C                   PARM                    LUNG
023700911001     C* OVRDBF
023800000428    2C  N20              DO
023900911001     C                   SETOFF                                       20
024000960129     C                   MOVE      VAR11         CMD3
024100911001     C                   MOVEL     *BLANKS       COMMAN
024200960129     C                   MOVEA     CMD3(1)       COMMAN
024300911001     C                   CALL      'QCMDEXC'                            20
024400911001     C                   PARM                    COMMAN
024500911001     C                   PARM                    LUNG
024600911001     C*
024700951114     C  N20              OPEN      FNFVA01R
024800000428    2C                   END
024900000428     C**
025000000428    2C     *IN20         IFEQ      *OFF
025100000428     C** CONTROLLO ESISTENZA MEMBRO: SE NON ESISTE LO CREO
025200000428     C                   SETOFF                                       21
025300000428     C                   MOVE      VAR11         CMD4                            FNFWA00R
025400000428     C                   MOVEL     *BLANKS       COMMAN
025500000428     C                   MOVEA     CMD4(1)       COMMAN           80
025600000428     C                   CALL      'QCMDEXC'                            21
025700000428     C                   PARM                    COMMAN
025800000428     C                   PARM                    LUNG
025900000428     C*
026000000428    3C     *IN21         IFEQ      '1'
026100000428     C                   MOVEL     'FNFWA00R'    FISICO
026200000428     C                   EXSR      CRTMBR
026300000428    3C                   END
026400000428     C**
026500000428     C* ALLOCO MEMBRO: SE NON ESISTE ESCO DA PROGRAMMA
026600000428    3C  N21              DO
026700000428     C                   MOVE      VAR12         CMD5
026800000428     C                   MOVEL     *BLANKS       COMMAN
026900000428     C                   MOVEA     CMD5(1)       COMMAN
027000000428     C                   CALL      'QCMDEXC'                            21
027100000428     C                   PARM                    COMMAN
027200000428     C                   PARM                    LUNG
027300000428     C* OVRDBF
027400000428    4C  N21              DO
027500000428     C                   MOVE      VAR11         CMD6
027600000428     C                   MOVEL     *BLANKS       COMMAN
027700000428     C                   MOVEA     CMD6(1)       COMMAN
027800000428     C                   CALL      'QCMDEXC'                            21
027900000428     C                   PARM                    COMMAN
028000000428     C                   PARM                    LUNG
028100000428     C*
028200000428     C  N21              OPEN      FNFWA00R
028300000428    4C                   END
028400000428    3C                   END
028500000428    2C                   END
028600000428    1C                   END
028700950109     C*
028800911001     C                   ENDSR
028900000428     C*
029000000428     C**-----------------------------------------------*
029100000428     C     CRTMBR        BEGSR
029200000428     C*
029300000428     C                   MOVEL     MBRFIL        FISMBR
029400000428     C                   CALL      'TRUL50C'
029500000428     C                   PARM                    FISICO           10
029600000428     C                   PARM                    FISMBR           10
029700000428     C                   PARM                    O50PLA
029800000428     C                   PARM                    LOGICO           10
029900000428     C                   PARM                    LOGMBR           10
030000000428     C                   PARM                    ULFLG             1
030100000428     C* RITENTO L'ALLOCAZIONE
030200000428     C                   CALL      'QCMDEXC'                            21
030300000428     C                   PARM                    COMMAN
030400000428     C                   PARM                    LUNG
030500000428     C*
030600000428     C                   ENDSR
030700951114     C*
030800951114     C*--- CONTROLLI SUL TRAINO ABBINATO -----------------------------*
030900951114     C     CTRTRN        BEGSR
031000951114     C*
031100951114     C* SE F.VIAGGIO ABBINATO CONTROLLO ABBINABILITA' AL TRAINO
031200951114     C                   CLEAR                   DSTL22
031300951114     C                   Z-ADD     FVALNP        D22LNP
031400951114     C                   Z-ADD     FVANFV        D22NFV
031500951114     C                   Z-ADD     FVALAI        D22LAI
031600951114     C                   MOVEL     'F'           D22FFS
031700951114     C                   MOVEL     DSTL22        KPJBU
031800951114     C                   CALL      'TNTL22R'
031900951114     C                   PARM                    KPJBA
032000951114     C                   MOVEL     KPJBU         DSTL22
032100951114     C                   MOVEL     '1'           WTL22             1
032200951114     C*
032300951114     C* D22ERR <> " "  ---> TRAINO NON ABBINABILE: AZZERO I CAMPI DEL
032400951114     C*                     TRAINO SUL FILE FNFVA00F
032500951114    1C     D22ERR        IFNE      ' '
032600951114     C*
032700951114     C     KFVA          CHAIN     FNFVA01L                           30
032800951114    2C     *IN30         IFEQ      *OFF
032900951114     C                   CLEAR                   FVATRN
033000951114     C                   CLEAR                   FVADTN
033100951114     C                   CLEAR                   FVATFM
033200951127     C                   CLEAR                   FVADTA
033300951127     C                   CLEAR                   FVAHTA
033400960201     C                   CLEAR                   FVADTP
033500960201     C                   CLEAR                   FVAHTP
033600951114     C                   UPDATE    FNFVA000
033700951114    2C                   ENDIF
033800061019     c
033900061019     c* Aggiorno anche fogli viaggio in partenza FNFGv01L
034000061019     c     kfgv          chain(E)  fnfgv01l
034100061019    2c                   if        not %error and %found(fnfgv01l)
034200061019     C                   CLEAR                   FGVTRN
034300061019     C                   CLEAR                   FGVFRQ
034400061019     C                   UPDATE    FNFGV000
034500061019     C*
034600061019     C* ANNULLO DATA/ORA TEORICA ARRIVO DAL FILE FNFV200F
034700061019     C                   MOVEL     'A'           WEPA
034800061019     C                   MOVEL     'T'           WTDH
034900061019     C     KFV3          SETLL     FNFV203L
035000061019     C     KFV3          READE     FNFV203L                               32
035100061019    3C     *IN32         DOWEQ     *OFF
035200061019     C                   DELETE    FNFV2003
035300061019     C     KFV3          READE     FNFV203L                               32
035400061019    3C                   ENDDO
035500061019     C*
035600061019     C* ANNULLO DATA/ORA TEORICA PARTENZA DAL FILE FNFV200F
035700061019     C                   MOVEL     'P'           WEPA
035800061019     C                   CLEAR                   WLAI
035900061019     C     KFV2          delete    FNFV201L                           32
036000061019     C*
036100061019    2c                   endif
036200951114     C*
036300951114   X1C                   ELSE
036400951123     C*
036500960129     C     KFVA          CHAIN     FNFVA01L                           30
036600960129     C*
036700960130     C* CALCOLO LA DATA/ORA TEORICA DI ARRIVO DEL F.VIAGGIO SOLO SE
036800960130     C*   IL F.VIAGGIO NON E' ILLUMINATO
036900960130    2C     FVAATR        IFEQ      ' '
037000960130     C*
037100960129     C* CALCOLO LA DATA TEORICA DI ARRIVO   DEL FOGLIO VIAGGIO E LA
037200960129     C*   SCRIVO SU FNFVA00F
037300951114     C                   CLEAR                   DSTL35
037400951123     C                   MOVEL     'A'           D35EPA
037500960129     C                   MOVEL     FVALAI        D35FIL
037600960129     C                   MOVEL     FVATFM        D35TFM
037700960129     C                   EXSR      CALT35
037800960129    3C     D35ERR        IFEQ      ' '
037900960129     C                   MOVEL     D35DTE        FVADTA
038000960129     C                   MOVEL     D35HTE        FVAHTA
038100960129     C                   MOVEL     D22DTN        FVADTN
038200061019     c* Aggiorno anche record di fnfv2
038300061019     C                   MOVEL     'A'           WEPA
038400061019     C                   MOVEL     FVaLAI        WLAI
038500061019     C                   MOVEL     'T'           WTDH
038600061019     C     KFV2          CHAIN     FNFV201L                           32
038700061019     C*
038800061019     C                   CLEAR                   FV2ATB
038900061019     C                   CLEAR                   FV2RTC
039000061019     C                   CLEAR                   FV2ABN
039100061019     c                   movel     fvalnp        fv2lnp
039200061019     c                   movel     fvanfv        fv2nfv
039300061019     c                   movel     fvalai        fv2lai
039400061019     C                   MOVEL     'A'           FV2EPA
039500061019     C                   MOVEL     'T'           FV2TDH
039600061019     C                   MOVEL     D35DTE        FV2DPA
039700061019     C                   MOVEL     D35HTE        FV2HPA
039800061019     C                   MOVEL     'R'           FV2FTR
039900061019     C                   MOVEL     dateu         FV2DTR
040000061019     C*
040100061019    4C     *IN32         IFEQ      *ON
040200061019     C                   WRITE     FNFV2000
040300061019   X3C                   ELSE
040400061019     C                   UPDATE    FNFV2000
040500061019    4C                   ENDIF
040600960129    3C                   ENDIF
040700960130    2C                   ENDIF
040800960129     C*
040900960129     C* CALCOLO LA DATA TEORICA DI PARTENZA DEL FOGLIO VIAGGIO E LA
041000960129     C*   SCRIVO SU FNFVA00F
041100960129     C                   CLEAR                   DSTL35
041200960129     C                   MOVEL     'P'           D35EPA
041300960129     C                   MOVEL     D22NFM        D35NFM
041400960129     C                   EXSR      CALT35
041500960130    2C     D35ERR        IFEQ      ' '
041600960129     C                   MOVEL     D35DTE        FVADTP
041700960129     C                   MOVEL     D35HTE        FVAHTP
041800960129     C                   MOVEL     D22DTN        FVADTN
041900960130    2C                   ENDIF
042000951114     C*
042100960129     C* AGGIORNO FNFVA00F
042200951123     C                   UPDATE    FNFVA000
042300951114    1C                   ENDIF
042400951114     C*
042500951114     C                   ENDSR
042600951114     C*
042700960129     C*--- CALL PGM TNTL35R ------------------------------------------*
042800960129     C     CALT35        BEGSR
042900960129     C                   MOVEL     'D'           D35TUP
043000960129     C                   MOVEL     FVATRN        D35TRN
043100960129     C                   MOVEL     D22DDE        D35DDE
043200960129     C                   MOVEL     D22DTN        D35DTN
043300960129     C                   MOVEL     DSTL35        KPJBU
043400960129     C                   CALL      'TNTL35R'
043500960129     C                   PARM                    KPJBA
043600960129     C                   MOVEL     KPJBU         DSTL35
043700960129     C                   MOVEL     '1'           WTL35             1
043800960129     C                   ENDSR
043900960129     C*
044000951114     C*--- SR INIZIALE -----------------------------------------------*
044100951114     C     *INZSR        BEGSR
044200951114     C*
044300951114     C****  KLIST  ****
044400951114     C* ACCESSO FNFVA01L / FNFVA01R
044500951114     C     KFVA          KLIST
044600951114     C                   KFLD                    FVALNP
044700951114     C                   KFLD                    FVANFV
044800951114     C                   KFLD                    FVALAI
044900061019     C     Kfgv          KLIST
045000061019     C                   KFLD                    FVANFV
045100061019     C                   KFLD                    FVALNP
045200061019     C     KFV3          KLIST
045300061019     C                   KFLD                    FVaLNP
045400061019     C                   KFLD                    FVaNFV
045500061019     C                   KFLD                    WEPA
045600061019     C                   KFLD                    WTDH
045700061019     C     KFV2          KLIST
045800061019     C                   KFLD                    FVaLNP
045900061019     C                   KFLD                    FVaNFV
046000061019     C                   KFLD                    WEPA
046100061019     C                   KFLD                    WLAI
046200061019     C                   KFLD                    WTDH
046300961016     C*
046400951114     C                   TIME                    WTIME            14 0          ORA & DATA
046500951114     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
046600951114     C*
046700951114     C* GIRO DATA DEL GIORNO
046800951114     C                   Z-ADD     WDATE         G02DAT
046900951114     C                   MOVEL     *BLANK        G02ERR
047000951114     C                   CALL      'XSRDA8'
047100951114     C                   PARM                    WLBDAT
047200951114     C                   Z-ADD     G02INV        DATEU             8 0
047300951114     C                   ENDSR
047400951114     C*
047500951114     C*---------------------------------------------------------------*
047600000428**         CMD1
047700021008CHKOBJ OBJ(FNFVA01R) OBJTYPE(*FILE) MBR(M000000)
047800911001**         CMD2
047900021008ALCOBJ OBJ((FNFVA01R *FILE *EXCL       M000000))
048000960129**         CMD3
048100021008OVRDBF FILE(FNFVA01R)               MBR(M890000)
048200000428**         CMD4
048300021008CHKOBJ OBJ(FNFWA00R) OBJTYPE(*FILE) MBR(M000000)
048400000428**         CMD5
048500021008ALCOBJ OBJ((FNFWA00R *FILE *EXCL       M000000))
048600000428**         CMD6
048700021008OVRDBF FILE(FNFWA00R)               MBR(M890000)
