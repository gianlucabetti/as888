000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS73R *----------------------------------------------------*
000300910205     H*         - STAMPA SEGNACOLLI
000400000000     H*--------------------------------------------------------------*
000500941228     FFNLS73D   CF   E             WORKSTN
000600941228     F                                     SFILE(LS73DSF:NRR)
000700941228     F                                     SFILE(LS73SF2:NR2)
000800021203     F*FNAPD01L  IF   E           K DISK
000801021203     FFIAPD01L  IF   E           K DISK
000900910412     FCNACO00F  IF   E           K DISK
001000020801     F**!!!FNBLD04J  IF   E           K DISK
001100020801     ffnbld06j  if   e           k disk
001200941228     FFNBLD02J  IF   E           K DISK
001300941228     F                                     RENAME(FNBLD000:FNBLD002)
001400001124     FFISGN06L  IF   E           K DISK
001500001124     FFISGN07L  IF   E           K DISK
001600001124     F                                     RENAME(FISGN000:FISGN007)
001700010523     FFISGN01L  IF   E           K DISK
001800010523     F                                     RENAME(FISGN000:FISGN001)
001900020703
002000020703     d sav_linpdr      s              3  0
002100020703     d xx              s              2  0
002200020801     d woksgn          s              1
002300020703
002400951102     D K15             S              1    DIM(15)
002500950313RM*  D L1              S              3  0 DIM(30)
002600900523     D PARAM           DS
002700001128     D* FISGN/FNBLD
002800001128     D  WFISGN                 1      1
002900001128     D* CODICE MITTNETE
003000001128     D  VIDKSC                11     17
003100960320     D* CODICE PADRONCINO
003200950313RM*  D  VIDPDR                18     24
003300960320     D* SCELTA ORDINAMENTO PER CODICE PADRONCINO
003400960320     D  V1COR1                25     25
003500960320     D* DATA SPEDIZIONE DI RISTAMPA
003600941228     D  COMDSP                26     33  0
003700960320     D* RISTAMPA  S/N
003800941228     D  VIDRSN                34     34
003900960320     D* RAGIONE SOCIALE MITTETE
004000951102     D  V1CRSM                35     49
004100960320     D* SCELTA ORDINAMENTO PER CODICE MITTENTE
004200960320     D  V1COR2                50     50
004300941228     D* PGM STAMPA SEGNACOLLI
004400941228     D  PSS                  227    236
004500941228     D* MODULO    SEGNACOLLI
004600941228     D  ETI                  237    246
004700941228     D* STAMPANTE SEGNACOLLI
004800941228     D  PRE                  247    256
004900010523     D* CODICE "CHI SONO"
005000010523     D  VIDWHO                60     94
005100021203     D*PARAM2          DS
005200021203     D* PARFIL                 1      3  0
005300021203     D* PARPAD                 4     10  0
005400021203     D* PARFLG                11     11
005500021203     D* PARDES                12     46
005501021203      * Ds esterna per ricerca codice padroncino
005502021203     D FNLV24DS      E DS
005600941228     D***
005700941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005800941228     D***
005900941228     D WLBDAT          DS                  INZ
006000941228     D  G02DAT                 1      8  0
006100941228     D  G02INV                 9     16  0
006200941228     D  G02ERR                17     17
006300941228     D  G02TGI                18     22  0
006400900517     D KPJBA         E DS
006500020430RM*  D*                                       1   3 REM
006600020430RM*  D*                                       4   6 REMFIL
006700910205     D TCUDS           DS
006800910205     D  F1                     1      1
006900910205     D  F3                     3      3
007000910205     D  F2                     2      2
007100910205     D  F4                     4      4
007200910205     D  F56                    5      6
007300950313RM*  D* DS PER TRUL06R - CARICAMENTO £X
007400950313RM*  D DSUL06        E DS                  EXTNAME(TRUL06DS)
007500950313RM*  D  LIN                    1     90  0
007600950313     D                                     DIM(30)
007700941228     D* DS PER TRUL01R - RICHIESTA STAMPANTI
007800941228     D DSUL01        E DS                  EXTNAME(TRUL01DS)
007900941228     D*
008000950313     D CNCR80        E DS
008100950313     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008200000000     D  TCU                  398    697
008300000000     D                                     DIM(50)
008400000000     D  KCU                  698    847P 0
008500000000     D                                     DIM(50)
008600000000     D                                     PACKEVEN
008700991207     D* DS PER LANCIO LISTA MC BOLLETTATI PER AUTTRASPORTATORE
008800991207     D DSLS85        E DS                  EXTNAME(FNLS85DS)
008801021203
008802021203     D kpdpdr          S                   LIKE(APDpdr)
008803021203     D kpdtip          S                   LIKE(APDtip)
008900910412     C****************************************************************
009000910412     C*  RIEPILOGO INDICATORI
009100910412     C***************************************************************
009200910412     C* 10    - RICHIESTA RISTAMPA
009300960320     C* 15    - POSIZIONAMENTO CURSORE SUL CAMPO CODICE CLIENTE MITT
009400010523     C* 30-33 - DI COMODO
009500910412     C* 35    - PULIZIA SUBFILES
009600960320     C* 40-56 - ERRORI NEI CAMPI
009700010523     C* 60-61 - ERRORI NEI CAMPI
009800001123     C* 77    - ON=GESTIONE FISGN  OFF=GESTIONE FNBLD
009900001124     C* 88    - LETTURA FISGN
010000001124     C* 90    - INDICATORE GENERICO DI ERRORE
010100910412     C*****************************************************************
010200000000     C     *ENTRY        PLIST
010300000000     C                   PARM                    KPJBA
010400001123     C                   MOVEL     KPJBU         WFISGN            1
010500001123      *
010600001123      *  VERIFICO SE GESTIRE FISGN O FNBLD
010700001123      *
010800001123     C     WFISGN        IFNE      ' '
010900001123     C                   SETON                                        77
011000001123     C                   ELSE
011100001123     C                   SETOFF                                       77
011200001123     C                   ENDIF
011300001123      *
011400000000     C                   Z-ADD     1             CODUT
011500950313     C                   CALL      'X§PARUT'
011600950313     C                   PARM                    UT§DSE
011700000000     C                   MOVEL     RAGUT         RSUT             20
011800950313     C                   MOVEL     REC80         CNCR80
011900910205     C*--- RICERCA CAPOCONTI------------------------------------------*
012000910205     C                   DO        50            X                 2 0
012100910205     C                   MOVE      TCU(X)        TCUDS
012200910205     C     F56           CABNE     'CG'          END1
012300910205     C     F4            COMP      '1'                                    21
012400910205     C     F4            COMP      '2'                                    22
012500910205     C     F4            COMP      '3'                                    23
012600910205     C     F4            COMP      '6'                                    27
012700910205     C** 1 CLIENTI   21
012800910205     C** 2 FORNITORI 22
012900910205     C** 3 AGENTI    23
013000910205     C     F3            COMP      '0'                                242425
013100910205     C     F3            COMP      'I'                                    26
013200910205     C** 0 ITALIA   25
013300910205     C** 1 ESTERO   24
013400910205     ** I CAPO CONTO IVA
013500910205     C   21
013600910205     CAN 24              Z-ADD     KCU(X)        KCE               4 0
013700910205     C   21
013800910205     CAN 25              Z-ADD     KCU(X)        KCI               4 0
013900910205     C   22
014000910205     CAN 24              Z-ADD     KCU(X)        KFE               4 0
014100910205     C   22
014200910205     CAN 25              Z-ADD     KCU(X)        KFI               4 0
014300910205     C   23
014400910205     CAN 24              Z-ADD     KCU(X)        KAE               4 0
014500910205     C   23
014600910205     CAN 25              Z-ADD     KCU(X)        KAI               4 0
014700910205     C   26              Z-ADD     KCU(X)        KIVA              4 0
014800910205     C   27              Z-ADD     KCU(X)        KBNA              4 0
014900910205     C     END1          TAG
015000910205     C                   END
015100900511     C*---------------------------------------------------------------*
015200910205     C* ACCESSO   CNACO                                    *
015300910205     C     KACO          KLIST
015400910205     C                   KFLD                    CODUT
015500910205     C                   KFLD                    CCC               4 0
015600910205     C                   KFLD                    KSC               7 0
015700020801     C* ACCESSO   FNBLD06                                  *
015800910207     C     KBLD1         KLIST
015900020801     c                   kfld                    lnp
016000910207     C                   KFLD                    KSC
016100910207     C                   KFLD                    FST               1
016200020801     C     KBLD3         KLIST
016300020801     c                   kfld                    lnp
016400020801     C                   KFLD                    KSC
016500941228     C* ACCESSO   FNBLD02                                  *
016600910207     C     KBLD2         KLIST
016700910207     C                   KFLD                    CPR
016800910207     C                   KFLD                    FST               1
016900971217     C     *LIKE         DEFINE    BLDFST        FST2
017000001124     C* ACCESSO   FISGN06                                  *
017100001124     C     KSGN1         KLIST
017200010122     C                   KFLD                    SIMFEL
017300010122     C                   KFLD                    KSC
017400001124     C                   KFLD                    ST2               1 0
017500010122     C* ACCESSO   FISGN06                                  *
017600010122     C     KSGN1B        KLIST
017700010122     C                   KFLD                    SIMFEL
017800010122     C                   KFLD                    KSC
017900001124     C* ACCESSO   FISGN07                                  *
018000001124     C     KSGN2         KLIST
018100001124     C                   KFLD                    LNP               3 0
018200001124     C                   KFLD                    PDR               3 0
018300001124     C                   KFLD                    ST2               1 0
018400001124     C* ACCESSO   FISGN07                                  *
018500001124     C     KSEI          KLIST
018600001124     C                   KFLD                    LNP               3 0
018700001124     C                   KFLD                    PDR               3 0
018800010523     C* ACCESSO   FISGN01                                  *
018900010523     C     KFIS          KLIST
019000010523     C                   KFLD                    SIMFEL
019100010523     C                   KFLD                    VIDWHO
019101021203     C     Kfapd         klist
019102021203     C                   kfld                    kpdtip
019103021203     C                   kfld                    kpdpdr
019200910205     C*---------------------------------------------------------------*
019300941228     C* RICHIESTA STAMPANTI
019400941228     C                   MOVEL     'S'           D01RSE
019500941228     C                   MOVEL     DSUL01        KPJBU
019600941228     C                   CALL      'TRUL01R'
019700941228     C                   PARM                    KPJBA
019800950104     C                   MOVEL     KPJBU         DSUL01
019900950104     C**
020000950104     C* F3 - FINE
020100950104     C     D01F3         IFEQ      '1'
020200950104     C                   GOTO      FINE
020300950104     C                   ENDIF
020400941228     C*
020500020703     c                   clear                   l1
020600950313RM*  C* CARICO £1
020700020703      * se p.o. di secondo livello carico il £1 solo se stesso
020800020703     c                   if        simtpp = '2' or simtpp = *blanks
020900020703     c                   eval      l1(1) = simpou
021000020703     c                   else
021100020703      * se p.o. di primo livello carico tutti i p.o. del terminal
021200950313RM*  C                   CLEAR                   DSUL06
021300950313RM*  C                   MOVE      '£1'          D06COD
021400950313RM*  C                   MOVEL     SIMFEL        D06KEY
021500950313RM*  C                   MOVEL     'L'           D06TLA
021600950313RM*  C                   MOVEL     DSUL06        KPJBU
021700950313RM*  C                   CALL      'TRUL06R'
021800950313RM*  C                   PARM                    KPJBA
021900950313RM*  C                   MOVEL     KPJBU         DSUL06
022000950313RM*  C                   MOVEA     LIN           L1
022100020703     c                   endif
022200950313RM*  C*
022300941228     C                   MOVEL     D01MDE        ETI
022400941228     C                   MOVEL     D01PRE        PRE
022500941228     C                   MOVEL     D01PSS        PSS
022600900517     C*
022700910205     C                   Z-ADD     KCI           CCC
022800900509     C     INIZIO        TAG
022900910205     C                   MOVEL     *BLANKS       VIDKSC
023000910205     C                   MOVEL     *BLANKS       DESKSC
023100910205     C                   MOVEL     *BLANKS       VIDCPR
023200910205     C                   MOVEL     *BLANKS       DESCPR
023300960320     C                   MOVEL     'M'           V1COR1
023400960320     C                   MOVEL     'P'           V1COR2
023500910412     C                   MOVEL     'N'           VIDRSN
023600991207     C                   MOVEL     'N'           VIDVLB
023700980203AG*  C                   Z-ADD     0             V10DSP
023800910412     C                   Z-ADD     0             VIDDSP
023900980204     C                   Z-ADD     0             COMDSP
024000010525     C                   MOVEL     *BLANKS       VIDWHO
024100951102     C                   CLEAR                   V1CRKS
024200951102     C                   CLEAR                   V1CRSM
024300950313RM*  C* Imposto codice filiale padroncino
024400020430RM*  C*          REM       IFEQ 'REM'
024500020430RM*  C*          REMFIL    ANDGT*ZEROS
024600020430RM*  C*                    MOVELREMFIL    VIDCFL
024700020430RM*  C*                    ELSE
024800020430RM*  C*                    MOVELSIMFEL    VIDCFL
024900020430RM*  C*                    END
025000020430     C                   Z-ADD     SIMPOU        VIDCFL
025100960320     C**
025200900509     C     FOR01         TAG
025300010525     C   77              EXFMT     LS73D01
025400010525     C  N77              EXFMT     LS73D03
025500960320     C                   SETOFF                                       15
025600941228     C** F3 - FINE LAVORO
025700900509     C   KC              GOTO      FINE
025800910205     C* CONTROLLI FORMATO
025900910205     C                   EXSR      CONTR
026000941228     C  NKF
026100941228     COR 90              GOTO      FOR01
026200901214     C*
026300900511     C* ELABORAZIONE
026400950313RM*  C     VIDCPR        IFGT      *ZEROS
026500950313RM*  C                   MOVEL     VIDCFL        VIDPDR
026600950313RM*  C                   MOVE      VIDCPR        VIDPDR
026700950313RM*  C                   ELSE
026800950313RM*  C                   MOVE      *BLANKS       VIDPDR
026900950313RM*  C                   END
027000900523     C                   MOVEL     PARAM         KPJBU
027100941228     C                   MOVEL     'LS74'        KCOAZ
027200900523     C                   CALL      'BCH10'
027300900523     C                   PARM                    KPJBA
027400991210     C*
027500991210     C* RICHIAMO PGM LISTA MC BOLLETTATI PER AUTOTRASPORT. SE  RICHIEST
027600991210     C     VIDVLB        IFEQ      'S'
027700991210     C                   CLEAR                   DSLS85
027800991210     C                   MOVEL     '1'           D85MOD
027900991210     C     VIDPDR        IFGT      *ZEROS
028000991210     C                   MOVE      VIDPDR        D85PDR
028100991210     C                   ELSE
028200991210     C                   MOVE      VIDKSC        D85KSC
028300991210     C                   ENDIF
028400991210     C                   MOVEL     DSLS85        KPJBU
028500991210     C                   CALL      'FNLS85R'
028600991210     C                   PARM                    KPJBA
028700991210     C                   ENDIF
028800941228     C*
028900950411     C                   GOTO      INIZIO
029000000000     C     FINE          TAG
029100000000     C                   SETON                                        LR
029200941228     C*
029300910306     C* CONTROLLO FORMATO -------------------------------------------
029400910205     C     CONTR         BEGSR
029500910205     C                   SETOFF                                       90
029600910411     C                   MOVEL     *BLANKS       DESCPR
029700951102     C                   CLEAR                   W0070
029800910412     C* CONTROLLO SE RICHIESTA LA RISTAMPA
029900910412     C     VIDRSN        IFEQ      'N'
030000910412     C                   SETOFF                                       10
030100910412     C                   ELSE
030200910412     C                   SETON                                        10
030300910412     C                   END
030400951102     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA
030500951102     C     RICKSC        IFNE      *BLANKS
030600951102     C     V1CRKS        ANDNE     *BLANKS
030700951102     C                   SETON                                        5290
030800951102     C                   GOTO      ENDCTR
030900951102     C                   ENDIF
031000010122     C* CONTROLLO ORDINAMENTI COME SE FOSSERO VALUES
031100010122     C* "CHI SONO" E' VALIDO SOLO PER FISGN
031200010122     C  N77V1COR1        IFEQ      'C'
031300010122     C                   SETON                                        5890
031400010122     C                   GOTO      ENDCTR
031500010122     C                   ENDIF
031600010122     C  N77V1COR2        IFEQ      'C'
031700010122     C                   SETON                                        5990
031800010122     C                   GOTO      ENDCTR
031900010122     C                   ENDIF
032000951102     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA
032100951102     C     RICKSC        IFNE      *BLANKS
032200951102     C     V1CRKS        ORNE      *BLANKS
032300951102     C     RICCPR        IFNE      *BLANKS
032400951102     C                   SETON                                        4590
032500951102     C                   GOTO      ENDCTR
032600951102     C                   ENDIF
032700951102     C                   ENDIF
032800980203     C* CONTROLLO LA DATA SPEDIZIONE SE DIVERSA DA ZERO
032900980203AG*  C     V10DSP        IFNE      *ZEROS
033000980203AG*  C                   Z-ADD     V10DSP        G02DAT
033100980203AG*  C                   MOVEL     *BLANK        G02ERR
033200980203AG*  C                   CALL      'XSRDA8'
033300980203AG*  C                   PARM                    WLBDAT
033400980203     C* ERRATA
033500980203AG* 2C     G02ERR        IFEQ      '1'
033600980203AG*  C                   SETON                                        4990
033700980203AG*  C                   GOTO      ENDCTR
033800980203AG* 2C                   ENDIF
033900980205AG*  C   10              SETON                                        3990
034000980205AG*  C   10              GOTO      ENDCTR
034100980203AG*  C                   Z-ADD     G02INV        COMDSP
034200980204AG*  C                   Z-ADD     G02DAT        V10DSP
034300980203AG*  C                   END
034400991207     C* SE RICHIESTA RISTAMPA NON E' POSSIBILE RICHIEDERE LA LISTA MC
034500991207     C   10VIDVLB        IFEQ      'S'
034600991207     C                   SETON                                        4690
034700991207     C                   GOTO      ENDCTR
034800991207     C                   ENDIF
034900951102     C*  --------------------------------------------------
035000951102     C* R I C E R C A   P A D R O N C I N I  CON SEGNACOLLI DA STAMPARE
035100951102     C     RICCPR        IFNE      *BLANKS
035200951102     C                   EXSR      CTRCPR
035300951102     C* SE NR2 = 0 NON CI SONO SEGNACOLLI DA STAMPARE
035400951102     C     NR2           IFEQ      0
035500951102     C                   SETON                                        4390
035600951102     C                   GOTO      ENDCTR
035700951102     C                   END
035800951102     C* EMETTO VIDEATA
035900951102     C                   WRITE     LS73D02
036000951102     C     FORCT2        TAG
036100951102     C                   EXFMT     LS73CT2
036200951102     C* CMD12 RITORNO
036300951102     C   KL              DO
036400951102     C                   MOVEL     *BLANKS       RICCPR
036500951102     C                   SETON                                        90
036600951102     C                   GOTO      ENDCTR
036700951102     C                   END
036800951102     C*
036900951102     C                   DO        *HIVAL
037000951102     C                   READC     LS73SF2                                31
037100951102     C  N31VI5SCE        IFEQ      '1'
037200951102RM*  C                   MOVE      VI5CPR        VIDCPR
037300951102RM*  C                   MOVEL     VI5CPR        VIDCFL
037400951102     C                   MOVEL     *BLANKS       RICCPR
037500951102     C                   MOVEL     DE5CPR        DESCPR
037600951102     C                   MOVEL     *BLANKS       VI5SCE
037700951102     C                   UPDATE    LS73SF2
037800951102     C                   SETON                                        90
037900951102     C                   END
038000951102     C  N90
038100951102     CANN31              END
038200951102     C  N31              GOTO      ENDCTR
038300951102     C   31              GOTO      FORCT2
038400951102     C*
038500951102     C                   END
038600951102     C*  --------------------------------------------------
038700951102     C**
038800951102     C* CONTROLLO PADRONCINO
038900951102RM*  C* F I L I A L E  P A D R O N C I N O
039000951102RM*  C     VIDCPR        IFGT      *ZEROS
039100951102RM*  C     VIDCFL        IFEQ      *ZEROS
039200951102RM*  C                   SETON                                        4890
039300951102RM*  C                   GOTO      ENDCTR
039400951102RM*  C                   ELSE
039500951102RM*  C     VIDCFL        LOOKUP    L1                                     30
039600951102RM*  C  N30              SETON                                        4890
039700951102RM*  C  N30              GOTO      ENDCTR
039800951102RM*  C                   END
039900951102RM*  C                   END
040000951102     C* C O D I C E   P A D R O N C I N O
040100951102     C* RICERCA ALFABETICA
040200951102    1C     VIDCPR        IFNE      *BLANKS
040300951102     C     VIDCPR        ANDNE     *ZEROS
040400951102     C     '?'           SCAN      VIDCPR                                 90
040500951102    2C     *IN90         IFEQ      *ON
040600021203     C*                  MOVEL     *BLANKS       PARDES
040700021203     C*                  Z-ADD     0             PARPAD
040800021203RM*  C*                  Z-ADD     VIDCFL        PARFIL
040900021203     C*                  MOVEL     'R'           PARFLG
041000021203     C*                  MOVEL     PARAM2        KPJBU
041001021203      *
041002021203      * imposto i campi della ds esterna fnlv24ds
041003021203     C                   clear                   fnlv24ds
041004021203     C                   eval      d24fil   = vidcfl
041005021203     C                   eval      d24tip   = 'A'
041006021203     C                   eval      d24flg   = 'R'
041007021203     C                   movel     fnlv24ds      kpjbu
041100951102     C                   CALL      'FNLV24R'
041200951102     C                   PARM                    KPJBA
041300021203     C*                  MOVEL     KPJBU         PARAM2
041400951102RM*  C*§                   MOVELPARPAD    VIDCPR
041500021203RM*  C*                  MOVE      PARPAD        VIDCPR
041600021203RM* 3C*    PARPAD        IFNE      *ZEROS
041700021203RM*  C*                  MOVEL     PARPAD        VIDCFL
041800021203RM* 3C*                  END
041900021203     C*                  MOVEL     PARDES        DESCPR
041901021203      *
041902021203      * valorizzo i campi a video
041903021203     C                   movel     kpjbu         fnlv24ds
041904021203     C                   move      d24pdr        VIDCPR
041905021203     C                   if        d24pdr  <>  *zeros
041906021203     C                   movel     d24pdr        VIDCFL
041907021203     C                   endif
041911021203     C                   movel     d24rsc        DESCPR
042000951102     C                   SETON                                        90
042100951102     C                   GOTO      ENDCTR
042200951102    2C                   END
042300951102     C* ESEGUO TESTN
042400951102     C                   TESTN                   VIDCPR               30
042500951102     C                   MOVE      VIDCPR        W001A             1
042600951102    2C     *IN30         IFEQ      *OFF
042700951102     C     W001A         ORLT      '0'
042800951102     C                   SETON                                        4290
042900951102     C                   GOTO      ENDCTR
043000951102    2C                   ENDIF
043100951102     C*
043200951102     C                   MOVE      VIDCPR        W0070             7 0
043300951102RM*  C                   MOVEL     VIDCFL        W0070
043400021203     C*    W0070         CHAIN     FNAPD000                           30
043401021205     C                   move      w0070         kpdpdr
043402021203     C                   move      'A'           kpdtip
043403021203     C     kfapd         chain     fiapd01l                           30
043500951102    2C     *IN30         IFEQ      *ON
043600020517     C**!!!*IN30         OREQ      *OFF
043700020517     C**!!!APDATB        ANDNE     *BLANKS
043800951102     C                   SETON                                        4290
043900951102     C                   GOTO      ENDCTR
044000951102    2C                   ENDIF
044100951102     C*
044200951102     C                   MOVEL     APDRSC        DESCPR
044300960320     C* SE IMMESSO IL CODICE PADRONCINO OBBLIGATORIO INDICARE L'ORDINAM
044400960320     C     V1COR1        IFEQ      ' '
044500960320     C                   SETON                                        5490
044600960320     C                   GOTO      ENDCTR
044700951102    1C                   ENDIF
044800960320    1C                   ENDIF
044900951102     C*  --------------------------------------------------
045000910205     C*
045100910205     C* R I C E R C A   C L I E N T I  CON SEGNACOLLI DA STAMPARE
045200951102     C     V1CRKS        IFNE      *BLANKS
045300951102     C     RICKSC        ORNE      *BLANKS
045400951102     C*
045500910412     C                   EXSR      CTRKSC
045600910205     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
045700910205     C     NRR           IFEQ      0
045800960320     C     RICKSC        IFEQ      '?'
045900951102     C                   SETON                                        5090
046000960320     C                   CLEAR                   RICKSC
046100951102     C                   ELSE
046200951102     C                   SETON                                        5190
046300960320     C                   CLEAR                   V1CRKS
046400951102     C                   ENDIF
046500910205     C                   GOTO      ENDCTR
046600910205     C                   END
046700910205     C* EMETTO VIDEATA
046800941228     C                   WRITE     LS73D02
046900910205     C*
047000910205     C     FORCT         TAG
047100941228     C                   EXFMT     LS73DCT
047200910205     C* CMD12 RITORNO
047300910205     C   KL              DO
047400960320     C                   CLEAR                   V1CRKS
047500910205     C                   MOVEL     *BLANKS       RICKSC
047600910205     C                   SETON                                        90
047700910205     C                   GOTO      ENDCTR
047800910205     C                   END
047900910205     C*
048000910205     C                   DO        *HIVAL
048100941228     C                   READC     LS73DSF                                31
048200941228     C  N31VI5SCE        IFEQ      '1'
048300960320     C     RICKSC        IFEQ      '?'
048400910205     C                   MOVEL     VI5KSC        VIDKSC
048500910205     C                   MOVEL     *BLANKS       RICKSC
048600910205     C                   MOVEL     DE5KSC        DESKSC
048700960320     C                   SETON                                        15
048800951102     C                   ELSE
048900951102     C                   MOVEL     *BLANKS       V1CRKS
049000951102     C                   MOVEL     DE5KSC        V1CRSM
049100951102     C     VIDCPR        IFLE      *ZEROS
049200951102     C                   MOVEL     VI5PDR        VIDCFL
049300951102     C                   MOVE      VI5PDR        VIDCPR
049400951102     C                   ENDIF
049500951102     C                   ENDIF
049600951102     C*
049700910205     C                   MOVEL     *BLANKS       VI5SCE
049800941228     C                   UPDATE    LS73DSF
049900910205     C                   SETON                                        90
050000910205     C                   END
050100910205     C  N90
050200910205     CANN31              END
050300910205     C  N31              GOTO      ENDCTR
050400910205     C   31              GOTO      FORCT
050500910205     C*
050600910205     C                   END
050700010523     C*  --------------------------------------------------
050800010523     C*
050900010523     C* CONTROLLO ESISTENZA CODICE "CHI SONO" IN CASO DI IMMISSIONE
051000010525     C   77VIDWHO        IFNE      *BLANKS
051100010523     C     VIDWHO        ORNE      *BLANKS
051200010523     C     KFIS          CHAIN     FISGN01L                           33
051300010523     C     *IN33         IFEQ      *ON
051400010523     C                   SETON                                        60
051500010523     C                   GOTO      ENDCTR
051600010523     C                   ELSE
051700010523     C     SGNFL2        IFEQ      'N'
051800010523     C                   SETON                                        61
051900010523     C                   GOTO      ENDCTR
052000010523     C                   ENDIF
052100010523     C                   ENDIF
052200010523     C                   ENDIF
052300951102     C*  --------------------------------------------------
052400910205     C*
052500910205     C* EFFETTUARE ALMENO UNA SCELTA
052600010525     C   77VIDKSC        IFEQ      *BLANKS
052700910205     C     VIDKSC        OREQ      *ZEROS
052800910205     C     VIDCPR        IFEQ      *BLANKS
052900910205     C     VIDCPR        OREQ      *ZEROS
053000010525     C     VIDWHO        IFEQ      *BLANKS
053100010525     C     VIDWHO        OREQ      *ZEROS
053200910205     C                   SETON                                        9041
053300910205     C                   GOTO      ENDCTR
053400951102     C                   ENDIF
053500951102     C                   ENDIF
053600010523     C                   ENDIF
053700910205     C*
053800010601     C  N77VIDKSC        IFEQ      *BLANKS
053900010525     C     VIDKSC        OREQ      *ZEROS
054000010525     C     VIDCPR        IFEQ      *BLANKS
054100010525     C     VIDCPR        OREQ      *ZEROS
054200010525     C                   SETON                                        9041
054300010525     C                   GOTO      ENDCTR
054400010525     C                   ENDIF
054500010525     C                   ENDIF
054600010525     C*
054700010601     C* C O D I C E   C H I   S O N O
054800010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE PADRONCINO O
054900010601     C   77VIDCPR        IFNE      *BLANKS
055000010601     C     VIDCPR        ANDNE     *ZEROS
055100010601     C     VIDWHO        IFNE      *BLANKS
055200010601     C     VIDWHO        ANDNE     *ZEROS
055300010601     C                   SETON                                        9041
055400010601     C                   GOTO      ENDCTR
055500010601     C                   END
055600010601     C                   END
055700010601      *
055800010601     C* C O D I C E   C L I E N T E
055900010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE PADRONCINO
056000010601     C     VIDKSC        IFNE      *BLANKS
056100010601     C     VIDKSC        ANDNE     *ZEROS
056200010601     C     VIDCPR        IFNE      *BLANKS
056300010601     C     VIDCPR        ANDNE     *ZEROS
056400010601     C                   SETON                                        9041
056500010601     C                   GOTO      ENDCTR
056600010601     C                   END
056700010601     C*
056800010601     C   77VIDKSC        IFNE      *BLANKS
056900910205     C     VIDKSC        ANDNE     *ZEROS
057000910205     C     VIDCPR        IFNE      *BLANKS
057100910205     C     VIDCPR        ANDNE     *ZEROS
057200010601     C     VIDWHO        IFNE      *BLANKS
057300010525     C     VIDWHO        ANDNE     *ZEROS
057400910205     C                   SETON                                        9041
057500910205     C                   GOTO      ENDCTR
057600010525     C                   END
057700010525     C                   END
057800010601     C                   END
057900010601     C*
058000010601     C* C O D I C E   C H I   S O N O
058100010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE CLIENTE
058200010601     C*
058300010601     C   77VIDKSC        IFNE      *BLANKS
058400010601     C     VIDKSC        ANDNE     *ZEROS
058500010601     C     VIDWHO        IFNE      *BLANKS
058600010601     C     VIDWHO        ANDNE     *ZEROS
058700010601     C                   SETON                                        9041
058800010601     C                   GOTO      ENDCTR
058900010601     C                   END
059000010601     C                   END
059100010601      *
059200910205     C* RICERCA ALFABETICA
059300010601     C*
059400941228     C     '?'           SCAN      VIDKSC                                 90
059500941228     C     *IN90         IFEQ      *ON
059600910205     C                   MOVEL     RSUT          PARDUT           30
059700910205     C* PARSTA=9 ESCLUDO ANNULLATI
059800910205     C                   MOVEL     DESKSC        DESCDS
059900910205     C                   Z-ADD     9             PARSTA
060000961016     C                   Z-ADD     CCC           PARCCC
060100981109     C                   CLEAR                   PARDIT
060200000907     C                   Z-ADD     1             PAXNUM
060300000907     C                   CALL      'XALFA3BR'
060400910205     C                   PARM                    PARDUT
060500910205     C                   PARM                    CODUT
060600910205     C                   PARM                    DESCDS           48
060700961016     C                   PARM                    PARCCC            4 0
060800910205     C                   PARM                    PARSTA            1 0
060900961016     C                   PARM                    PARFLR           90
061000981109     C                   PARM                    PARDIT            3
061100000907     C                   PARM                    PAXNUM            2 0
061200000907     C                   PARM                    PAXKCM           80
061300000907     C                   PARM                    PAXKSM          140
061400000907     C                   PARM                    PAXKDM           60
061500910205     C     PARSTA        IFGT      -1
061600000907     C                   MOVEL     PAXKSM        VIDKSC
061700910205     C                   MOVEL     DESCDS        DESKSC
061800910205     C                   END
061900910205     C                   GOTO      ENDCTR
062000910205     C                   END
062100910205     C* CONTROLLO
062200910205     C                   TESTN                   VIDKSC               30
062300910205     C  N30              SETON                                        4090
062400910205     C  N30              GOTO      ENDCTR
062500910205     C*
062600910205     C     VIDKSC        IFGT      *ZEROS
062700910205     C                   MOVEL     VIDKSC        KSC
062800910205     C     KACO          CHAIN     CNACO000                           30
062900910205     C   30              SETON                                        4090
063000910205     C   30              GOTO      ENDCTR
063100910205     C                   ELSE
063200910205     C                   SETON                                        4090
063300910205     C                   GOTO      ENDCTR
063400910205     C                   END
063500960320     C                   MOVEL     ACORAG        DESKSC
063600001124      *
063700001124      * CONTROLLO SE ESISTE ALMENO UN SEGNACOLLO DA STAMPARE PER
063800001124      * QUEL CLIENTE.
063900001124      *
064000001124      *  F I S G N
064100001124      *  =========
064200001124     C     *IN77         IFEQ      *ON
064300001124     C                   SETOFF                                       30
064400010405     C                   Z-ADD     0             ST2
064500010405     C  N10KSGN1         SETLL     FISGN06L
064600010405     C   10KSGN1         SETGT     FISGN06L
064700001124     C                   DO        *HIVAL
064800010405     C  N10KSGN1         READE     FISGN06L                               88
064900010405     C   10KSGN1B        READE     FISGN06L                               88
065000001124     C   88              LEAVE
065100001124     C     SGNATB        IFEQ      ' '
065200010405     C     SGNFL2        ANDNE     'N'
065300001124     C                   SETON                                        30
065400001124     C                   LEAVE
065500001124     C                   ENDIF
065600001124     C                   ENDDO
065700020801
065800020801     C  N30              SETON                                        4490
065900020801     C  N30              GOTO      ENDCTR
066000001124      *
066100001124     C                   ELSE
066200001124      *
066300001124      *  F N B L D
066400001124      *  =========
066500020801      *
066600020801     c                   clear                   woksgn
066700020801      * Leggo per tutte le lnp del terminal
066800020801  b2 c                   do        30            xx
066900020801     c                   if        l1(xx) = *zeros
067000020801     c                   leave
067100020801     c                   endif
067200020801     c                   movel     l1(xx)        lnp
067300020801
067400001124      *  DEVO FARE 2 TENTATIVI:
067500001124      *  IN   STAMPA CON ' ' E CON 'E' -SOLO ETICHETTE ESTERE
067600001124      *  IN RISTAMPA CON 'S' E CON 'Z' -SOLO ETICHETTE ESTERE
067700971217     C  N10              MOVEL     ' '           FST
067800971217     C   10              MOVEL     'S'           FST
067900020801
068000020801     C     KBLD1         SETLL     FNBLD06J
068100020801  b3 c                   do        *Hival
068200020801     c     kbld1         reade     fnbld06j
068300020801     c                   if        %eof(fnbld06j)
068400020801     c                   leave
068500020801     c                   endif
068600020801      * ok trovato un segnacollo
068700020801     c                   eval      woksgn = '1'
068800020801     c                   leave
068900020801 e3  c                   enddo
069000020801      * se trovato un segnacollo esco dal ciclo delle linee di partenza
069100020801     c                   if        woksgn <> *blanks
069200020801     c                   leave
069300020801     c                   endif
069400020801      * secondo tentativo se non ho trovato nessun segnacollo
069500971217     C**
069600020801     C  N10              movel     'E'           fst
069700020801     C**!!!ANN30              MOVEL     'E'           FST
069800020801     C   10              movel     'Z'           fst
069900020801     C**!!!ANN30              MOVEL     'Z'           FST
070000020801     C     KBLD1         SETLL     FNBLD06J
070100020801  b3 c                   do        *Hival
070200020801     c     kbld1         reade     fnbld06j
070300020801     c                   if        %eof(fnbld06j)
070400020801     c                   leave
070500020801     c                   endif
070600020801      * ok trovato un segnacollo
070700020801     c                   eval      woksgn = '1'
070800020801     c                   leave
070900020801  e3 c                   enddo
071000020801      * se trovato un segnacollo esco dal ciclo delle linee di partenza
071100020801     c                   if        woksgn <> *blanks
071200020801     c                   leave
071300020801     c                   endif
071400020801  b2 c                   enddo
071500020801      * errore se non ci sono segnacolli da stampare
071600020801     c                   if        woksgn = *blanks
071700020801     c                   seton                                        4490
071800020801     c                   goto      endctr
071900020801     c                   endif
072000001124      *
072100001124     C                   ENDIF
072200971217     C**
072300020801     C*!N30              SETON                                        4490
072400020801     C*!N30              GOTO      ENDCTR
072500971217     C**
072600960320     C* OBBLIGATORIO IMMETTERE L'ORDINMANETO
072700960320     C     V1COR2        IFEQ      ' '
072800960320     C                   SETON                                        5590
072900960320     C                   GOTO      ENDCTR
073000910205     C                   END
073100960320     C                   END
073200910205     C*
073300910205     C* CONTROLLO SE ESISTE ALMENO UN SEGNACOLLO DA STAMPARE PER
073400910205     C*   QUEL PADRONCINO
073500951102    1C     VIDCPR        IFNE      *BLANKS
073600951102     C     VIDCPR        ANDNE     *ZEROS
073700950313RM*  C                   MOVEL     VIDCFL        CPR               7 0
073800950313RM*  C                   MOVE      VIDCPR        CPR               7 0
073900001124     C                   MOVEL     CPR           LNP
074000001124     C                   MOVE      CPR           PDR
074100001124      *  F I S G N
074200001124      *  =========
074300001124     C     *IN77         IFEQ      *ON
074400001124     C                   SETOFF                                       30
074500001124     C                   Z-ADD     0             ST2
074600010405     C  N10KSGN2         SETLL     FISGN07L
074700010405     C   10KSGN2         SETGT     FISGN07L
074800001124     C                   DO        *HIVAL
074900010405     C  N10KSGN2         READE     FISGN07L                               88
075000010405     C   10KSEI          READE     FISGN07L                               88
075100001124     C   88              LEAVE
075200001124     C     SGNATB        IFEQ      ' '
075300010405     C     SGNFL2        ANDNE     'N'
075400001124     C                   SETON                                        30
075500001124     C                   LEAVE
075600001124     C                   ENDIF
075700001124     C                   ENDDO
075800001124      *
075900001124     C                   ELSE
076000001124      *  F N B L D
076100001124      *  =========
076200001124      *  DEVO FARE 2 TENTATIVI:
076300001124      *  IN   STAMPA CON ' ' E CON 'E' -SOLO ETICHETTE ESTERE
076400001124      *  IN RISTAMPA CON 'S' E CON 'Z' -SOLO ETICHETTE ESTERE
076500971217     C  N10              MOVEL     ' '           FST
076600971217     C   10              MOVEL     'S'           FST
076700941228     C     KBLD2         SETLL     FNBLD02J                               30
076800971217     C  N10
076900971217     CANN30              MOVEL     'E'           FST
077000971217     C   10
077100971217     CANN30              MOVEL     'Z'           FST
077200971217     C  N30KBLD2         SETLL     FNBLD02J                               30
077300001124      *
077400001124     C                   ENDIF
077500971217     C**
077600910205     C  N30              SETON                                        4390
077700910205     C  N30              GOTO      ENDCTR
077800951102     C* SE IMMESSA RAGIONE SOCIALE CONTROLLO CHE ESISTE ALMENO
077900951102     C* UN SEGNACOLLO DA STAMPARE CON QUELLA RAGIONE SOCIALE
078000951102    2C     V1CRSM        IFNE      *BLANKS
078100951102     C                   EXSR      CTRMIT
078200951102     C   90              GOTO      ENDCTR
078300951102    2C                   ENDIF
078400951102    1C                   ENDIF
078500910412     C*
078600910412     C* SE RICHIESTA RISTAMPA
078700910412     C     VIDRSN        IFEQ      'S'
078800910412     C*
078900910412     C* DATA SPEDIZIONE OBBLIGATORIA
079000941228     C                   Z-ADD     VIDDSP        G02DAT
079100941228     C                   MOVEL     *BLANK        G02ERR
079200941228     C                   CALL      'XSRDA8'
079300941228     C                   PARM                    WLBDAT
079400941228     C* ERRATA
079500941228    2C     G02ERR        IFEQ      '1'
079600941228     C                   SETON                                        4790
079700941228    2C                   ENDIF
079800941228     C                   Z-ADD     G02INV        COMDSP
079900941228     C                   Z-ADD     G02DAT        VIDDSP
080000910412     C                   END
080100960320     C* NON SI PUO' IMMETTERE LA RAGIONE SOCIALE SE STAMPA PER SOLO
080200960320     C*  DESTINATARIO
080300960320     C     V1COR1        IFEQ      'D'
080400960320     C     V1CRSM        ANDNE     *BLANKS
080500960320     C                   SETON                                        5690
080600960320    2C                   ENDIF
080700910412     C*
080800910205     C     ENDCTR        ENDSR
080900910205     C*
081000910205     C* RICERCA CLIENTI CON SEGNACOLLI DA STAMPARE -------------------*
081100910205     C     CTRKSC        BEGSR
081200910205     C                   EXSR      PULSF
081300910205     C                   Z-ADD     0             NRR               4 0
081400910205     C                   Z-ADD     1             KSC
081500001124      * F I S G N
081600001124      * =========
081700001124     C     *IN77         IFEQ      *ON
081800010122     C     KSGN1B        SETLL     FISGN06L
081900001124     C*
082000001124     C                   DO        *HIVAL
082100001124     C                   SETOFF                                       32
082200001124     C                   READ      FISGN06L                               30
082300010122     C* ESCO A CAMBIO TNP
082400010122     C  N30SGNTNP        IFNE      SIMFEL
082500010122     C                   SETON                                        30
082600010122     C                   ENDIF
082700010406     C* SCARTO RECORD SE GIA' STAMPATO
082800010406     C  N30SGNFL2        IFNE      'N'
082900010406     C* PER GIA' STAMPATO SALTO
083000010406     C     SGNST2        IFEQ      0
083100001124     C*
083200001124     C                   MOVEL     W0070         WLINEA            3 0
083300001124     C                   MOVE      W0070         WPADRO            3 0
083400001124     C*
083500001124     C* SE RICERCA CLIENTI DI UNO PADRONCINO CARICO SOLO QUELLI
083600001124     C     V1CRKS        IFEQ      '?'
083700001124     C     W0070         ANDGT     *ZEROS
083800001124     C     WLINEA        ANDEQ     SGNLNP
083900001124     C     WPADRO        ANDEQ     SGNCPD
084000001124     C* SE E' RICERCA CLIENTI PER PADRONCINO MA NON IMMESSO CODICE PAD
084100001124     C     V1CRKS        OREQ      '?'
084200001124     C     W0070         ANDEQ     0
084300001124     C* SE NON E' RICERCA CLIENTI PER PADRONCINO
084400001124     C     RICKSC        OREQ      '?'
084500001124     C*  --> C A R I C O
084600001124     C                   MOVEL     *BLANKS       VI5SCE
084700001124     C                   MOVEL     SGNKSC        VI5KSC
084800001124     C                   MOVEL     SGNKSC        KSC
084900001124     C     KACO          CHAIN     CNACO000                           31
085000001124     C  N31              MOVEL(P)  ACORAG        DE5KSC
085100001124     C   31              MOVEL(P)  SGNRSM        DE5KSC
085200001124     C                   SETOFF                                       31
085300001124     C                   MOVE      *ALL'0'       VI5PDR
085400001124     C                   MOVEL     SGNLNP        VI5PDR
085500001124     C                   MOVE      SGNCPD        VI5PDR
085600001124     C* SCRIVO SUBFILE
085700001124     C                   SETON                                        32
085800001124     C                   ADD       1             NRR
085900001124     C                   WRITE     LS73DSF
086000001124     C                   ENDIF
086100001124     C                   ENDIF
086200001124     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
086300001124     C* SOLO SE NON RICERCA PER CODICE PADRONCINO ANCHE
086400001124     C     RICKSC        IFNE      *BLANKS
086500001124     C     V1CRKS        ORNE      *BLANKS
086600001124     C     W0070         ANDEQ     0
086700001124     C     *IN32         OREQ      *ON
086800001124     C  N30              Z-ADD     SGNKSC        KSC
086900001124     C  N30              ADD       1             KSC
087000010122     C  N30KSGN1B        SETLL     FISGN06L
087100001124     C                   ENDIF
087200001124     C*
087300010406     C                   ENDIF
087400001124     C  N30              END
087500001124      *
087600001124     C                   ELSE
087700001124      * F N B L D
087800001124      * =========
087900020801      * Leggo per tutte le lnp del terminal
088000020801  b2 c                   do        30            xx
088100020801     c                   if        l1(xx) = *zeros
088200020801     c                   leave
088300020801     c                   endif
088400020801     c                   movel     l1(xx)        lnp
088500020801
088600020912     c                   Z-Add     1             Ksc
088700020801     C     kbld3         SETLL     FNBLD06J
088800910412     C*
088900910205     C                   DO        *HIVAL
089000951102     C                   SETOFF                                       32
089100020801     C     lnp           READe     FNBLD06J                               30
089200910205     C* PER FINE FILE O GIA' STAMPATO SALTO
089300910412     C  N30BLDFST        IFEQ      *BLANKS
089400971217     C     BLDFST        OREQ      'E'
089500951102     C*
089600951102     C* SE RICERCA CLIENTI DI UNO PADRONCINO CARICO SOLO QUELLI
089700951102     C     V1CRKS        IFEQ      '?'
089800951102     C     W0070         ANDGT     *ZEROS
089900951102     C     W0070         ANDEQ     BLDPDR
090000951102     C* SE E' RICERCA CLIENTI PER PADRONCINO MA NON IMMESSO CODICE PAD
090100951102     C     V1CRKS        OREQ      '?'
090200951102     C     W0070         ANDEQ     0
090300951102     C* SE NON E' RICERCA CLIENTI PER PADRONCINO
090400951102     C     RICKSC        OREQ      '?'
090500951102     C*  --> C A R I C O
090600910205     C                   MOVEL     *BLANKS       VI5SCE
090700910205     C                   MOVEL     BLDCCM        VI5KSC
090800910205     C                   MOVEL     BLDCCM        KSC
090900910205     C     KACO          CHAIN     CNACO000                           31
091000910205     C  N31              MOVEL     ACORAG        DE5KSC
091100951102     C   31              MOVEL     BLPRSM        DE5KSC
091200951102     C                   SETOFF                                       31
091300951102     C                   MOVEL     BLDPDR        VI5PDR
091400910205     C* SCRIVO SUBFILE
091500951102     C                   SETON                                        32
091600910205     C                   ADD       1             NRR
091700941228     C                   WRITE     LS73DSF
091800951102     C                   ENDIF
091900951102     C                   ENDIF
092000910412     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
092100951102     C* SOLO SE NON RICERCA PER CODICE PADRONCINO ANCHE
092200951102     C     RICKSC        IFNE      *BLANKS
092300951102     C     V1CRKS        ORNE      *BLANKS
092400951102     C     W0070         ANDEQ     0
092500951102     C     *IN32         OREQ      *ON
092600910313     C  N30              Z-ADD     BLDCCM        KSC
092700910306     C  N30              ADD       1             KSC
092800020801     C  N30kbld3         SETLL     FNBLD06J
092900951102     C                   ENDIF
093000910205     C*
093100910205     C  N30              END
093200020801
093300020801     c                   enddo
093400020801
093500910205     C*
093600001124     C                   ENDIF
093700001124     C*
093800910205     C                   ENDSR
093900910205     C*
094000910205     C* RICERCA PADRON. CON SEGNACOLLI DA STAMPARE -------------------*
094100910205     C     CTRCPR        BEGSR
094200941228     C*
094300910205     C                   EXSR      PULSF2
094400941228     C*
094500910205     C                   Z-ADD     0             NR2               4 0
094600910205     C                   Z-ADD     1             CPR
094700020703     C**!!!              Z-ADD     1             WSEI              6 0
094800001124      * F I S G N
094900001124      * =========
095000001124     C     *IN77         IFEQ      *ON
095100001124      *
095200020703      * Leggo per tutte le lnp del terminal
095300020703  b1 c                   do        30            xx
095400020703     c                   if        l1(xx) = *zeros
095500020703     c                   leave
095600020703     c                   endif
095700020703     c                   movel     l1(xx)        lnp
095800020703
095900020703     C**!!!              MOVEL     WSEI          LNP
096000020703     C**!!!              MOVE      WSEI          PDR
096100020703     C**!!!KSEI          SETLL     FISGN07L
096200020703     c     lnp           setll     fisgn07l
096300001124    1C                   DO        *HIVAL
096400020703     C**!!!              READ      FISGN07L                               30
096500020703     c     lnp           reade     fisgn07l                               30
096600010406     C* IGNORO RECORD SE DI SOLA RISTAMPA E SE TER-PART <> SIMFEL
096700010406     C  N30SGNFL2        IFNE      'N'
096800010406     C     SGNTNP        ANDEQ     SIMFEL
096900010406     C* PER GIA' STAMPATO SALTO
097000010406    2C     SGNST2        IFEQ      0
097100001124     C                   MOVEL     *BLANKS       VI5SCE
097200001124     C                   MOVE      *ALL'0'       VI5CPR
097300001124     C                   MOVEL     SGNLNP        VI5CPR
097400001124     C                   MOVE      SGNCPD        VI5CPR
097500021203     C*    VI5CPR        CHAIN     FNAPD000                           31
097501021205     C                   move      VI5CPR        kpdpdr
097502021203     C                   move      'A'           kpdtip
097503021203     C     kfapd         chain     fiapd01l                           31
097600001124     C  N31              MOVEL     APDRSC        DE5CPR
097700001124     C   31              MOVEL     *BLANKS       DE5CPR
097800001124     C* SCRIVO SUBFILE
097900001124     C                   ADD       1             NR2
098000001124     C                   WRITE     LS73SF2
098100001124    2C                   END
098200001124     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
098300020703     C**!!!              MOVEL     SGNLNP        WSEI
098400020703     C**!!!              MOVE      SGNCPD        WSEI
098500020703     C**!!!              ADD       1             WSEI
098600020703     C**!!!              MOVEL     WSEI          LNP
098700020703     C**!!!              MOVE      WSEI          PDR
098800020703     C**!!!KSEI          SETLL     FISGN07L
098900010406     C                   ENDIF
099000001124     C  N30              END
099100020703 e1  c                   enddo
099200001124      *
099300001124     C                   ELSE
099400001124      * F N B L D
099500001124      * =========
099600020703      *
099700020703      * Leggo per tutte le lnp del terminal
099800020703  b1 c                   do        30            xx
099900020703     c                   if        l1(xx) = *zeros
100000020703     c                   leave
100100020703     c                   endif
100200020912     c                   clear                   cpr
100300020703     c                   movel     l1(xx)        cpr
100400020703
100500941228     C     CPR           SETLL     FNBLD02J
100600941228    1C                   DO        *HIVAL
100700941228     C                   READ      FNBLD02J                               30
100800020703      * controllo se la linea dell'autotrasportatore è la stessa che ho
100900020703      * impostato dalla l1
101000020703     c                   if        not *in30
101100020703     c                   movel     bldpdr        sav_linpdr
101200020703     c                   if        l1(xx) <> sav_linpdr
101300020703     c                   leave
101400020703     c                   endif
101500020703     c                   endif
101600910205     C* PER FINE FILE O GIA' STAMPATO SALTO
101700941228    2C  N30BLDFST        IFEQ      *BLANKS
101800971217     C     BLDFST        OREQ      'E'
101900910205     C                   MOVEL     *BLANKS       VI5SCE
102000941228     C                   MOVEL     BLDPDR        VI5CPR
102100021203     C*    BLDPDR        CHAIN     FNAPD000                           31
102101021205     C                   move      BldPdr        kpdpdr
102102021203     C                   move      'A'           kpdtip
102103021203     C     kfapd         chain     fiapd01l                           31
102200941228     C  N31              MOVEL     APDRSC        DE5CPR
102300910205     C   31              MOVEL     *BLANKS       DE5CPR
102400910205     C* SCRIVO SUBFILE
102500910205     C                   ADD       1             NR2
102600941228     C                   WRITE     LS73SF2
102700941228    2C                   END
102800910313     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
102900020801     C     1             ADD       BLDPDR        CPR
103000020801     C     CPR           SETLL     FNBLD02J
103100910205     C  N30              END
103200020703 e1  c                   enddo
103300910205     C*
103400001124     C                   ENDIF
103500001124     C*
103600910205     C                   ENDSR
103700910205     C*
103800910205     C* PULIZIA SUBFILE ---------------------------------------------
103900910205     C     PULSF         BEGSR
104000910205     C                   SETON                                        35
104100941228     C                   WRITE     LS73DCT
104200910205     C                   SETOFF                                       35
104300910205     C                   ENDSR
104400910205     C*
104500910205     C* PULIZIA SUBFILE2 --------------------------------------------
104600910205     C     PULSF2        BEGSR
104700910205     C                   SETON                                        35
104800941228     C                   WRITE     LS73CT2
104900910205     C                   SETOFF                                       35
105000910205     C                   ENDSR
105100951102     C*
105200960320     C* CONTROLLO SE ESISTE UN SEGNACOLLO DA STAMPARE  --------------
105300951102     C     CTRMIT        BEGSR
105400951102     C*
105500951102     C                   CLEAR                   WPOS
105600951102     C     ' '           CHECKR    V1CRSM        WPOS              2 0
105700951102     C     WPOS          ADD       1             X                 2 0
105800001124     C** FISGN
105900001124     C** =====
106000001124     C     *IN77         IFEQ      *ON
106100001124     C                   Z-ADD     0             ST2
106200010406     C  N10KSGN2         SETLL     FISGN07L
106300010406     C   10KSGN2         SETGT     FISGN07L
106400010406     C  N10KSGN2         READE     FISGN07L                               30
106500010406     C   10KSEI          READE     FISGN07L                               30
106600001124     C     *IN30         DOWEQ     *OFF
106700010405     C                   CLEAR                   W015A
106800010405     C     SGNATB        IFEQ      *BLANKS
106900010405     C     SGNFL2        ANDNE     'N'
107000001124     C*
107100001124     C                   MOVEA     SGNRSM        K15
107200001124     C     X             IFNE      16
107300001124     C                   MOVEA     *BLANKS       K15(X)
107400001124     C                   ENDIF
107500001124     C                   MOVEA     K15           W015A            15
107600010405     C                   ENDIF
107700001124     C     W015A         IFNE      V1CRSM
107800010406     C  N10KSGN2         READE     FISGN07L                               30
107900010406     C   10KSEI          READE     FISGN07L                               30
108000001124     C                   ELSE
108100001124     C                   SETON                                        30
108200001124     C                   ENDIF
108300001124     C*
108400001124     C                   ENDDO
108500001124     C                   ELSE
108600001124     C** FNBLD
108700001124     C** =====
108800971217     C  N10              MOVEL     ' '           FST
108900971217     C  N10              MOVEL     'E'           FST2
109000971217     C   10              MOVEL     'S'           FST
109100971217     C   10              MOVEL     'Z'           FST2
109200971217     C**
109300971217     C                   DO        2
109400951102     C     KBLD2         SETLL     FNBLD02J
109500951102     C     KBLD2         READE     FNBLD02J                               30
109600951102     C     *IN30         DOWEQ     *OFF
109700951102     C*
109800951102     C                   MOVEA     BLPRSM        K15
109900951102     C     X             IFNE      16
110000951102     C                   MOVEA     *BLANKS       K15(X)
110100951102     C                   ENDIF
110200951102     C                   MOVEA     K15           W015A            15
110300951102     C     W015A         IFNE      V1CRSM
110400951102     C     KBLD2         READE     FNBLD02J                               30
110500951102     C                   ELSE
110600951102     C                   SETON                                        30
110700951102     C                   ENDIF
110800951102     C*
110900951102     C                   ENDDO
111000971217     C                   MOVEL     FST2          FST
111100971217     C                   ENDDO
111200001124      *
111300001124     C                   ENDIF
111400001124     C* -----
111500951102     C     W015A         IFNE      V1CRSM
111600951102     C                   SETON                                        5390
111700951102     C                   ENDIF
111800001124     C* -----
111900951102     C                   ENDSR
