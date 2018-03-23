000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLS73R *----------------------------------------------------*
000300910205     H*         - STAMPA SEGNACOLLI
000400000000     H*--------------------------------------------------------------*
000500941228     FFNLS73D   CF   E             WORKSTN
000600941228     F                                     SFILE(LS73DSF:NRR)
000700941228     F                                     SFILE(LS73SF2:NR2)
000800021203     FFIAPD01L  IF   E           K DISK
000900910412     FCNACO00F  IF   E           K DISK
001000020801     ffnbld06j  if   e           k disk
001100941228     FFNBLD02J  IF   E           K DISK
001200941228     F                                     RENAME(FNBLD000:FNBLD002)
001300001124     FFISGN06L  IF   E           K DISK
001400001124     FFISGN07L  IF   E           K DISK
001500001124     F                                     RENAME(FISGN000:FISGN007)
001600010523     FFISGN01L  IF   E           K DISK
001700010523     F                                     RENAME(FISGN000:FISGN001)
001800020703
001900020703     d sav_linpdr      s              3  0
002000020703     d xx              s              2  0
002100020801     d woksgn          s              1
002200020703
002300951102     D K15             S              1    DIM(15)
002400950313RM*  D L1              S              3  0 DIM(30)
002500900523     D PARAM           DS
002600001128     D* FISGN/FNBLD
002700001128     D  WFISGN                 1      1
002800001128     D* CODICE MITTNETE
002900001128     D  VIDKSC                11     17
003000960320     D* CODICE PADRONCINO
003100950313RM*  D  VIDPDR                18     24
003200960320     D* SCELTA ORDINAMENTO PER CODICE PADRONCINO
003300960320     D  V1COR1                25     25
003400960320     D* DATA SPEDIZIONE DI RISTAMPA
003500941228     D  COMDSP                26     33  0
003600960320     D* RISTAMPA  S/N
003700941228     D  VIDRSN                34     34
003800960320     D* RAGIONE SOCIALE MITTETE
003900951102     D  V1CRSM                35     49
004000960320     D* SCELTA ORDINAMENTO PER CODICE MITTENTE
004100960320     D  V1COR2                50     50
004200941228     D* PGM STAMPA SEGNACOLLI
004300941228     D  PSS                  227    236
004400941228     D* MODULO    SEGNACOLLI
004500941228     D  ETI                  237    246
004600941228     D* STAMPANTE SEGNACOLLI
004700941228     D  PRE                  247    256
004800010523     D* CODICE "CHI SONO"
004900010523     D  VIDWHO                60     94
005000021203      * Ds esterna per ricerca codice padroncino
005100021203     D FNLV24DS      E DS
005200941228     D***
005300941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
005400941228     D***
005500941228     D WLBDAT          DS                  INZ
005600941228     D  G02DAT                 1      8  0
005700941228     D  G02INV                 9     16  0
005800941228     D  G02ERR                17     17
005900941228     D  G02TGI                18     22  0
006000900517     D KPJBA         E DS
006100020430RM*  D*                                       1   3 REM
006200020430RM*  D*                                       4   6 REMFIL
006300910205     D TCUDS           DS
006400910205     D  F1                     1      1
006500910205     D  F3                     3      3
006600910205     D  F2                     2      2
006700910205     D  F4                     4      4
006800910205     D  F56                    5      6
006900950313RM*  D* DS PER TRUL06R - CARICAMENTO £X
007000950313RM*  D DSUL06        E DS                  EXTNAME(TRUL06DS)
007100950313RM*  D  LIN                    1     90  0
007200950313     D                                     DIM(30)
007300030722     D* DS PER TRUL90R - RICHIESTA STAMPANTI
007400030722     D Trul90ds      e ds                  inz
007500941228     D*
007600950313     D CNCR80        E DS
007700950313     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007800000000     D  TCU                  398    697
007900000000     D                                     DIM(50)
008000000000     D  KCU                  698    847P 0
008100000000     D                                     DIM(50)
008200000000     D                                     PACKEVEN
008300991207     D* DS PER LANCIO LISTA MC BOLLETTATI PER AUTTRASPORTATORE
008400991207     D DSLS85        E DS                  EXTNAME(FNLS85DS)
008500021203
008600021203     D kpdpdr          S                   LIKE(APDpdr)
008700021203     D kpdtip          S                   LIKE(APDtip)
008800910412     C****************************************************************
008900910412     C*  RIEPILOGO INDICATORI
009000910412     C***************************************************************
009100910412     C* 10    - RICHIESTA RISTAMPA
009200960320     C* 15    - POSIZIONAMENTO CURSORE SUL CAMPO CODICE CLIENTE MITT
009300010523     C* 30-33 - DI COMODO
009400910412     C* 35    - PULIZIA SUBFILES
009500960320     C* 40-56 - ERRORI NEI CAMPI
009600010523     C* 60-61 - ERRORI NEI CAMPI
009700001123     C* 77    - ON=GESTIONE FISGN  OFF=GESTIONE FNBLD
009800001124     C* 88    - LETTURA FISGN
009900001124     C* 90    - INDICATORE GENERICO DI ERRORE
010000910412     C*****************************************************************
010100000000     C     *ENTRY        PLIST
010200000000     C                   PARM                    KPJBA
010300001123     C                   MOVEL     KPJBU         WFISGN            1
010400001123      *
010500001123      *  VERIFICO SE GESTIRE FISGN O FNBLD
010600001123      *
010700001123     C     WFISGN        IFNE      ' '
010800001123     C                   SETON                                        77
010900001123     C                   ELSE
011000001123     C                   SETOFF                                       77
011100001123     C                   ENDIF
011200001123      *
011300000000     C                   Z-ADD     1             CODUT
011400950313     C                   CALL      'X§PARUT'
011500950313     C                   PARM                    UT§DSE
011600000000     C                   MOVEL     RAGUT         RSUT             20
011700950313     C                   MOVEL     REC80         CNCR80
011800910205     C*--- RICERCA CAPOCONTI------------------------------------------*
011900910205     C                   DO        50            X                 2 0
012000910205     C                   MOVE      TCU(X)        TCUDS
012100910205     C     F56           CABNE     'CG'          END1
012200910205     C     F4            COMP      '1'                                    21
012300910205     C     F4            COMP      '2'                                    22
012400910205     C     F4            COMP      '3'                                    23
012500910205     C     F4            COMP      '6'                                    27
012600910205     C** 1 CLIENTI   21
012700910205     C** 2 FORNITORI 22
012800910205     C** 3 AGENTI    23
012900910205     C     F3            COMP      '0'                                242425
013000910205     C     F3            COMP      'I'                                    26
013100910205     C** 0 ITALIA   25
013200910205     C** 1 ESTERO   24
013300910205     ** I CAPO CONTO IVA
013400910205     C   21
013500910205     CAN 24              Z-ADD     KCU(X)        KCE               4 0
013600910205     C   21
013700910205     CAN 25              Z-ADD     KCU(X)        KCI               4 0
013800910205     C   22
013900910205     CAN 24              Z-ADD     KCU(X)        KFE               4 0
014000910205     C   22
014100910205     CAN 25              Z-ADD     KCU(X)        KFI               4 0
014200910205     C   23
014300910205     CAN 24              Z-ADD     KCU(X)        KAE               4 0
014400910205     C   23
014500910205     CAN 25              Z-ADD     KCU(X)        KAI               4 0
014600910205     C   26              Z-ADD     KCU(X)        KIVA              4 0
014700910205     C   27              Z-ADD     KCU(X)        KBNA              4 0
014800910205     C     END1          TAG
014900910205     C                   END
015000900511     C*---------------------------------------------------------------*
015100910205     C* ACCESSO   CNACO                                    *
015200910205     C     KACO          KLIST
015300910205     C                   KFLD                    CODUT
015400910205     C                   KFLD                    CCC               4 0
015500910205     C                   KFLD                    KSC               7 0
015600020801     C* ACCESSO   FNBLD06                                  *
015700910207     C     KBLD1         KLIST
015800020801     c                   kfld                    lnp
015900910207     C                   KFLD                    KSC
016000910207     C                   KFLD                    FST               1
016100020801     C     KBLD3         KLIST
016200020801     c                   kfld                    lnp
016300020801     C                   KFLD                    KSC
016400941228     C* ACCESSO   FNBLD02                                  *
016500910207     C     KBLD2         KLIST
016600910207     C                   KFLD                    CPR
016700910207     C                   KFLD                    FST               1
016800971217     C     *LIKE         DEFINE    BLDFST        FST2
016900001124     C* ACCESSO   FISGN06                                  *
017000001124     C     KSGN1         KLIST
017100010122     C                   KFLD                    SIMFEL
017200010122     C                   KFLD                    KSC
017300001124     C                   KFLD                    ST2               1 0
017400010122     C* ACCESSO   FISGN06                                  *
017500010122     C     KSGN1B        KLIST
017600010122     C                   KFLD                    SIMFEL
017700010122     C                   KFLD                    KSC
017800001124     C* ACCESSO   FISGN07                                  *
017900001124     C     KSGN2         KLIST
018000001124     C                   KFLD                    LNP               3 0
018100001124     C                   KFLD                    PDR               3 0
018200001124     C                   KFLD                    ST2               1 0
018300001124     C* ACCESSO   FISGN07                                  *
018400001124     C     KSEI          KLIST
018500001124     C                   KFLD                    LNP               3 0
018600001124     C                   KFLD                    PDR               3 0
018700010523     C* ACCESSO   FISGN01                                  *
018800010523     C     KFIS          KLIST
018900010523     C                   KFLD                    SIMFEL
019000010523     C                   KFLD                    VIDWHO
019100021203     C     Kfapd         klist
019200021203     C                   kfld                    kpdtip
019300021203     C                   kfld                    kpdpdr
019400910205     C*---------------------------------------------------------------*
019500941228     C* RICHIESTA STAMPANTI
019600030722     C                   movel     'S'           D90rse
019700030722     C                   call      'TRUL90R'
019800941228     C                   PARM                    KPJBA
019900030722     C                   parm                    Trul90ds
020000950104     C**
020100950104     C* F3 - FINE
020200030722     C                   IF        D90f3 = *on
020300950104     C                   GOTO      FINE
020400950104     C                   ENDIF
020500941228     C*
020600020703     c                   clear                   l1
020700950313RM*  C* CARICO £1
020800020703      * se p.o. di secondo livello carico il £1 solo se stesso
020900020703     c                   if        simtpp = '2' or simtpp = *blanks
021000020703     c                   eval      l1(1) = simpou
021100020703     c                   else
021200020703      * se p.o. di primo livello carico tutti i p.o. del terminal
021300950313RM*  C                   CLEAR                   DSUL06
021400950313RM*  C                   MOVE      '£1'          D06COD
021500950313RM*  C                   MOVEL     SIMFEL        D06KEY
021600950313RM*  C                   MOVEL     'L'           D06TLA
021700950313RM*  C                   MOVEL     DSUL06        KPJBU
021800950313RM*  C                   CALL      'TRUL06R'
021900950313RM*  C                   PARM                    KPJBA
022000950313RM*  C                   MOVEL     KPJBU         DSUL06
022100950313RM*  C                   MOVEA     LIN           L1
022200020703     c                   endif
022300950313RM*  C*
022400030722     C                   movel     D90mde        ETI
022500030722     C                   movel     D90pre        PRE
022600030722     C                   movel     D90pss        PSS
022700900517     C*
022800910205     C                   Z-ADD     KCI           CCC
022900900509     C     INIZIO        TAG
023000910205     C                   MOVEL     *BLANKS       VIDKSC
023100910205     C                   MOVEL     *BLANKS       DESKSC
023200910205     C                   MOVEL     *BLANKS       VIDCPR
023300910205     C                   MOVEL     *BLANKS       DESCPR
023400960320     C                   MOVEL     'M'           V1COR1
023500960320     C                   MOVEL     'P'           V1COR2
023600910412     C                   MOVEL     'N'           VIDRSN
023700991207     C                   MOVEL     'N'           VIDVLB
023800980203AG*  C                   Z-ADD     0             V10DSP
023900910412     C                   Z-ADD     0             VIDDSP
024000980204     C                   Z-ADD     0             COMDSP
024100010525     C                   MOVEL     *BLANKS       VIDWHO
024200951102     C                   CLEAR                   V1CRKS
024300951102     C                   CLEAR                   V1CRSM
024400950313RM*  C* Imposto codice filiale padroncino
024500020430     C                   Z-ADD     SIMPOU        VIDCFL
024600960320     C**
024700900509     C     FOR01         TAG
024800010525     C   77              EXFMT     LS73D01
024900010525     C  N77              EXFMT     LS73D03
025000960320     C                   SETOFF                                       15
025100941228     C** F3 - FINE LAVORO
025200900509     C   KC              GOTO      FINE
025300910205     C* CONTROLLI FORMATO
025400910205     C                   EXSR      CONTR
025500941228     C  NKF
025600941228     COR 90              GOTO      FOR01
025700901214     C*
025800900511     C* ELABORAZIONE
025900950313RM*  C     VIDCPR        IFGT      *ZEROS
026000950313RM*  C                   MOVEL     VIDCFL        VIDPDR
026100950313RM*  C                   MOVE      VIDCPR        VIDPDR
026200950313RM*  C                   ELSE
026300950313RM*  C                   MOVE      *BLANKS       VIDPDR
026400950313RM*  C                   END
026500900523     C                   MOVEL     PARAM         KPJBU
026600941228     C                   MOVEL     'LS74'        KCOAZ
026700900523     C                   CALL      'BCH10'
026800900523     C                   PARM                    KPJBA
026900991210     C*
027000991210     C* RICHIAMO PGM LISTA MC BOLLETTATI PER AUTOTRASPORT. SE  RICHIEST
027100991210     C     VIDVLB        IFEQ      'S'
027200991210     C                   CLEAR                   DSLS85
027300991210     C                   MOVEL     '1'           D85MOD
027400991210     C     VIDPDR        IFGT      *ZEROS
027500991210     C                   MOVE      VIDPDR        D85PDR
027600991210     C                   ELSE
027700991210     C                   MOVE      VIDKSC        D85KSC
027800991210     C                   ENDIF
027900991210     C                   MOVEL     DSLS85        KPJBU
028000991210     C                   CALL      'FNLS85R'
028100991210     C                   PARM                    KPJBA
028200991210     C                   ENDIF
028300941228     C*
028400950411     C                   GOTO      INIZIO
028500000000     C     FINE          TAG
028600000000     C                   SETON                                        LR
028700941228     C*
028800910306     C* CONTROLLO FORMATO -------------------------------------------
028900910205     C     CONTR         BEGSR
029000910205     C                   SETOFF                                       90
029100910411     C                   MOVEL     *BLANKS       DESCPR
029200951102     C                   CLEAR                   W0070
029300910412     C* CONTROLLO SE RICHIESTA LA RISTAMPA
029400910412     C     VIDRSN        IFEQ      'N'
029500910412     C                   SETOFF                                       10
029600910412     C                   ELSE
029700910412     C                   SETON                                        10
029800910412     C                   END
029900951102     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA
030000951102     C     RICKSC        IFNE      *BLANKS
030100951102     C     V1CRKS        ANDNE     *BLANKS
030200951102     C                   SETON                                        5290
030300951102     C                   GOTO      ENDCTR
030400951102     C                   ENDIF
030500010122     C* CONTROLLO ORDINAMENTI COME SE FOSSERO VALUES
030600010122     C* "CHI SONO" E' VALIDO SOLO PER FISGN
030700010122     C  N77V1COR1        IFEQ      'C'
030800010122     C                   SETON                                        5890
030900010122     C                   GOTO      ENDCTR
031000010122     C                   ENDIF
031100010122     C  N77V1COR2        IFEQ      'C'
031200010122     C                   SETON                                        5990
031300010122     C                   GOTO      ENDCTR
031400010122     C                   ENDIF
031500951102     C* CONTROLLO SE SI VUOLE EFFETTUARE UNA RICERCA
031600951102     C     RICKSC        IFNE      *BLANKS
031700951102     C     V1CRKS        ORNE      *BLANKS
031800951102     C     RICCPR        IFNE      *BLANKS
031900951102     C                   SETON                                        4590
032000951102     C                   GOTO      ENDCTR
032100951102     C                   ENDIF
032200951102     C                   ENDIF
032300980203     C* CONTROLLO LA DATA SPEDIZIONE SE DIVERSA DA ZERO
032400980203AG*  C     V10DSP        IFNE      *ZEROS
032500980203AG*  C                   Z-ADD     V10DSP        G02DAT
032600980203AG*  C                   MOVEL     *BLANK        G02ERR
032700980203AG*  C                   CALL      'XSRDA8'
032800980203AG*  C                   PARM                    WLBDAT
032900980203     C* ERRATA
033000980203AG* 2C     G02ERR        IFEQ      '1'
033100980203AG*  C                   SETON                                        4990
033200980203AG*  C                   GOTO      ENDCTR
033300980203AG* 2C                   ENDIF
033400980205AG*  C   10              SETON                                        3990
033500980205AG*  C   10              GOTO      ENDCTR
033600980203AG*  C                   Z-ADD     G02INV        COMDSP
033700980204AG*  C                   Z-ADD     G02DAT        V10DSP
033800980203AG*  C                   END
033900991207     C* SE RICHIESTA RISTAMPA NON E' POSSIBILE RICHIEDERE LA LISTA MC
034000991207     C   10VIDVLB        IFEQ      'S'
034100991207     C                   SETON                                        4690
034200991207     C                   GOTO      ENDCTR
034300991207     C                   ENDIF
034400951102     C*  --------------------------------------------------
034500951102     C* R I C E R C A   P A D R O N C I N I  CON SEGNACOLLI DA STAMPARE
034600951102     C     RICCPR        IFNE      *BLANKS
034700951102     C                   EXSR      CTRCPR
034800951102     C* SE NR2 = 0 NON CI SONO SEGNACOLLI DA STAMPARE
034900951102     C     NR2           IFEQ      0
035000951102     C                   SETON                                        4390
035100951102     C                   GOTO      ENDCTR
035200951102     C                   END
035300951102     C* EMETTO VIDEATA
035400951102     C                   WRITE     LS73D02
035500951102     C     FORCT2        TAG
035600951102     C                   EXFMT     LS73CT2
035700951102     C* CMD12 RITORNO
035800951102     C   KL              DO
035900951102     C                   MOVEL     *BLANKS       RICCPR
036000951102     C                   SETON                                        90
036100951102     C                   GOTO      ENDCTR
036200951102     C                   END
036300951102     C*
036400951102     C                   DO        *HIVAL
036500951102     C                   READC     LS73SF2                                31
036600951102     C  N31VI5SCE        IFEQ      '1'
036700951102RM*  C                   MOVE      VI5CPR        VIDCPR
036800951102RM*  C                   MOVEL     VI5CPR        VIDCFL
036900951102     C                   MOVEL     *BLANKS       RICCPR
037000951102     C                   MOVEL     DE5CPR        DESCPR
037100951102     C                   MOVEL     *BLANKS       VI5SCE
037200951102     C                   UPDATE    LS73SF2
037300951102     C                   SETON                                        90
037400951102     C                   END
037500951102     C  N90
037600951102     CANN31              END
037700951102     C  N31              GOTO      ENDCTR
037800951102     C   31              GOTO      FORCT2
037900951102     C*
038000951102     C                   END
038100951102     C*  --------------------------------------------------
038200951102     C**
038300951102     C* CONTROLLO PADRONCINO
038400951102RM*  C* F I L I A L E  P A D R O N C I N O
038500951102RM*  C     VIDCPR        IFGT      *ZEROS
038600951102RM*  C     VIDCFL        IFEQ      *ZEROS
038700951102RM*  C                   SETON                                        4890
038800951102RM*  C                   GOTO      ENDCTR
038900951102RM*  C                   ELSE
039000951102RM*  C     VIDCFL        LOOKUP    L1                                     30
039100951102RM*  C  N30              SETON                                        4890
039200951102RM*  C  N30              GOTO      ENDCTR
039300951102RM*  C                   END
039400951102RM*  C                   END
039500951102     C* C O D I C E   P A D R O N C I N O
039600951102     C* RICERCA ALFABETICA
039700951102    1C     VIDCPR        IFNE      *BLANKS
039800951102     C     VIDCPR        ANDNE     *ZEROS
039900951102     C     '?'           SCAN      VIDCPR                                 90
040000951102    2C     *IN90         IFEQ      *ON
040100021203      *
040200021203      * imposto i campi della ds esterna fnlv24ds
040300021203     C                   clear                   fnlv24ds
040400021203     C                   eval      d24fil   = vidcfl
040500021203     C                   eval      d24tip   = 'A'
040600021203     C                   eval      d24flg   = 'R'
040700021203     C                   movel     fnlv24ds      kpjbu
040800951102     C                   CALL      'FNLV24R'
040900951102     C                   PARM                    KPJBA
041000021203      *
041100021203      * valorizzo i campi a video
041200021203     C                   movel     kpjbu         fnlv24ds
041300021203     C                   move      d24pdr        VIDCPR
041400021203     C                   if        d24pdr  <>  *zeros
041500021203     C                   movel     d24pdr        VIDCFL
041600021203     C                   endif
041700021203     C                   movel     d24rsc        DESCPR
041800951102     C                   SETON                                        90
041900951102     C                   GOTO      ENDCTR
042000951102    2C                   END
042100951102     C* ESEGUO TESTN
042200951102     C                   TESTN                   VIDCPR               30
042300951102     C                   MOVE      VIDCPR        W001A             1
042400951102    2C     *IN30         IFEQ      *OFF
042500951102     C     W001A         ORLT      '0'
042600951102     C                   SETON                                        4290
042700951102     C                   GOTO      ENDCTR
042800951102    2C                   ENDIF
042900951102     C*
043000951102     C                   MOVE      VIDCPR        W0070             7 0
043100951102RM*  C                   MOVEL     VIDCFL        W0070
043200021203     C*    W0070         CHAIN     FNAPD000                           30
043300021205     C                   move      w0070         kpdpdr
043400021203     C                   move      'A'           kpdtip
043500021203     C     kfapd         chain     fiapd01l                           30
043600951102    2C     *IN30         IFEQ      *ON
043700951102     C                   SETON                                        4290
043800951102     C                   GOTO      ENDCTR
043900951102    2C                   ENDIF
044000951102     C*
044100951102     C                   MOVEL     APDRSC        DESCPR
044200960320     C* SE IMMESSO IL CODICE PADRONCINO OBBLIGATORIO INDICARE L'ORDINAM
044300960320     C     V1COR1        IFEQ      ' '
044400960320     C                   SETON                                        5490
044500960320     C                   GOTO      ENDCTR
044600951102    1C                   ENDIF
044700960320    1C                   ENDIF
044800951102     C*  --------------------------------------------------
044900910205     C*
045000910205     C* R I C E R C A   C L I E N T I  CON SEGNACOLLI DA STAMPARE
045100951102     C     V1CRKS        IFNE      *BLANKS
045200951102     C     RICKSC        ORNE      *BLANKS
045300951102     C*
045400910412     C                   EXSR      CTRKSC
045500910205     C* SE NRR = 0 NON CI SONO SEGNACOLLI DA STAMPARE
045600910205     C     NRR           IFEQ      0
045700960320     C     RICKSC        IFEQ      '?'
045800951102     C                   SETON                                        5090
045900960320     C                   CLEAR                   RICKSC
046000951102     C                   ELSE
046100951102     C                   SETON                                        5190
046200960320     C                   CLEAR                   V1CRKS
046300951102     C                   ENDIF
046400910205     C                   GOTO      ENDCTR
046500910205     C                   END
046600910205     C* EMETTO VIDEATA
046700941228     C                   WRITE     LS73D02
046800910205     C*
046900910205     C     FORCT         TAG
047000941228     C                   EXFMT     LS73DCT
047100910205     C* CMD12 RITORNO
047200910205     C   KL              DO
047300960320     C                   CLEAR                   V1CRKS
047400910205     C                   MOVEL     *BLANKS       RICKSC
047500910205     C                   SETON                                        90
047600910205     C                   GOTO      ENDCTR
047700910205     C                   END
047800910205     C*
047900910205     C                   DO        *HIVAL
048000941228     C                   READC     LS73DSF                                31
048100941228     C  N31VI5SCE        IFEQ      '1'
048200960320     C     RICKSC        IFEQ      '?'
048300910205     C                   MOVEL     VI5KSC        VIDKSC
048400910205     C                   MOVEL     *BLANKS       RICKSC
048500910205     C                   MOVEL     DE5KSC        DESKSC
048600960320     C                   SETON                                        15
048700951102     C                   ELSE
048800951102     C                   MOVEL     *BLANKS       V1CRKS
048900951102     C                   MOVEL     DE5KSC        V1CRSM
049000951102     C     VIDCPR        IFLE      *ZEROS
049100951102     C                   MOVEL     VI5PDR        VIDCFL
049200951102     C                   MOVE      VI5PDR        VIDCPR
049300951102     C                   ENDIF
049400951102     C                   ENDIF
049500951102     C*
049600910205     C                   MOVEL     *BLANKS       VI5SCE
049700941228     C                   UPDATE    LS73DSF
049800910205     C                   SETON                                        90
049900910205     C                   END
050000910205     C  N90
050100910205     CANN31              END
050200910205     C  N31              GOTO      ENDCTR
050300910205     C   31              GOTO      FORCT
050400910205     C*
050500910205     C                   END
050600010523     C*  --------------------------------------------------
050700010523     C*
050800010523     C* CONTROLLO ESISTENZA CODICE "CHI SONO" IN CASO DI IMMISSIONE
050900010525     C   77VIDWHO        IFNE      *BLANKS
051000010523     C     VIDWHO        ORNE      *BLANKS
051100010523     C     KFIS          CHAIN     FISGN01L                           33
051200010523     C     *IN33         IFEQ      *ON
051300010523     C                   SETON                                        60
051400010523     C                   GOTO      ENDCTR
051500010523     C                   ELSE
051600010523     C     SGNFL2        IFEQ      'N'
051700010523     C                   SETON                                        61
051800010523     C                   GOTO      ENDCTR
051900010523     C                   ENDIF
052000010523     C                   ENDIF
052100010523     C                   ENDIF
052200951102     C*  --------------------------------------------------
052300910205     C*
052400910205     C* EFFETTUARE ALMENO UNA SCELTA
052500010525     C   77VIDKSC        IFEQ      *BLANKS
052600910205     C     VIDKSC        OREQ      *ZEROS
052700910205     C     VIDCPR        IFEQ      *BLANKS
052800910205     C     VIDCPR        OREQ      *ZEROS
052900010525     C     VIDWHO        IFEQ      *BLANKS
053000010525     C     VIDWHO        OREQ      *ZEROS
053100910205     C                   SETON                                        9041
053200910205     C                   GOTO      ENDCTR
053300951102     C                   ENDIF
053400951102     C                   ENDIF
053500010523     C                   ENDIF
053600910205     C*
053700010601     C  N77VIDKSC        IFEQ      *BLANKS
053800010525     C     VIDKSC        OREQ      *ZEROS
053900010525     C     VIDCPR        IFEQ      *BLANKS
054000010525     C     VIDCPR        OREQ      *ZEROS
054100010525     C                   SETON                                        9041
054200010525     C                   GOTO      ENDCTR
054300010525     C                   ENDIF
054400010525     C                   ENDIF
054500010525     C*
054600010601     C* C O D I C E   C H I   S O N O
054700010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE PADRONCINO O
054800010601     C   77VIDCPR        IFNE      *BLANKS
054900010601     C     VIDCPR        ANDNE     *ZEROS
055000010601     C     VIDWHO        IFNE      *BLANKS
055100010601     C     VIDWHO        ANDNE     *ZEROS
055200010601     C                   SETON                                        9041
055300010601     C                   GOTO      ENDCTR
055400010601     C                   END
055500010601     C                   END
055600010601      *
055700010601     C* C O D I C E   C L I E N T E
055800010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE PADRONCINO
055900010601     C     VIDKSC        IFNE      *BLANKS
056000010601     C     VIDKSC        ANDNE     *ZEROS
056100010601     C     VIDCPR        IFNE      *BLANKS
056200010601     C     VIDCPR        ANDNE     *ZEROS
056300010601     C                   SETON                                        9041
056400010601     C                   GOTO      ENDCTR
056500010601     C                   END
056600010601     C*
056700010601     C   77VIDKSC        IFNE      *BLANKS
056800910205     C     VIDKSC        ANDNE     *ZEROS
056900910205     C     VIDCPR        IFNE      *BLANKS
057000910205     C     VIDCPR        ANDNE     *ZEROS
057100010601     C     VIDWHO        IFNE      *BLANKS
057200010525     C     VIDWHO        ANDNE     *ZEROS
057300910205     C                   SETON                                        9041
057400910205     C                   GOTO      ENDCTR
057500010525     C                   END
057600010525     C                   END
057700010601     C                   END
057800010601     C*
057900010601     C* C O D I C E   C H I   S O N O
058000010601     C*   NON SI PUO' IMMETTERE ANCHE IL CODICE CLIENTE
058100010601     C*
058200010601     C   77VIDKSC        IFNE      *BLANKS
058300010601     C     VIDKSC        ANDNE     *ZEROS
058400010601     C     VIDWHO        IFNE      *BLANKS
058500010601     C     VIDWHO        ANDNE     *ZEROS
058600010601     C                   SETON                                        9041
058700010601     C                   GOTO      ENDCTR
058800010601     C                   END
058900010601     C                   END
059000010601      *
059100910205     C* RICERCA ALFABETICA
059200010601     C*
059300941228     C     '?'           SCAN      VIDKSC                                 90
059400941228     C     *IN90         IFEQ      *ON
059500910205     C                   MOVEL     RSUT          PARDUT           30
059600910205     C* PARSTA=9 ESCLUDO ANNULLATI
059700910205     C                   MOVEL     DESKSC        DESCDS
059800910205     C                   Z-ADD     9             PARSTA
059900961016     C                   Z-ADD     CCC           PARCCC
060000981109     C                   CLEAR                   PARDIT
060100000907     C                   Z-ADD     1             PAXNUM
060200000907     C                   CALL      'XALFA3BR'
060300910205     C                   PARM                    PARDUT
060400910205     C                   PARM                    CODUT
060500910205     C                   PARM                    DESCDS           48
060600961016     C                   PARM                    PARCCC            4 0
060700910205     C                   PARM                    PARSTA            1 0
060800961016     C                   PARM                    PARFLR           90
060900981109     C                   PARM                    PARDIT            3
061000000907     C                   PARM                    PAXNUM            2 0
061100000907     C                   PARM                    PAXKCM           80
061200000907     C                   PARM                    PAXKSM          140
061300000907     C                   PARM                    PAXKDM           60
061400910205     C     PARSTA        IFGT      -1
061500000907     C                   MOVEL     PAXKSM        VIDKSC
061600910205     C                   MOVEL     DESCDS        DESKSC
061700910205     C                   END
061800910205     C                   GOTO      ENDCTR
061900910205     C                   END
062000910205     C* CONTROLLO
062100910205     C                   TESTN                   VIDKSC               30
062200910205     C  N30              SETON                                        4090
062300910205     C  N30              GOTO      ENDCTR
062400910205     C*
062500910205     C     VIDKSC        IFGT      *ZEROS
062600910205     C                   MOVEL     VIDKSC        KSC
062700910205     C     KACO          CHAIN     CNACO000                           30
062800910205     C   30              SETON                                        4090
062900910205     C   30              GOTO      ENDCTR
063000910205     C                   ELSE
063100910205     C                   SETON                                        4090
063200910205     C                   GOTO      ENDCTR
063300910205     C                   END
063400960320     C                   MOVEL     ACORAG        DESKSC
063500001124      *
063600001124      * CONTROLLO SE ESISTE ALMENO UN SEGNACOLLO DA STAMPARE PER
063700001124      * QUEL CLIENTE.
063800001124      *
063900001124      *  F I S G N
064000001124      *  =========
064100001124     C     *IN77         IFEQ      *ON
064200001124     C                   SETOFF                                       30
064300010405     C                   Z-ADD     0             ST2
064400010405     C  N10KSGN1         SETLL     FISGN06L
064500010405     C   10KSGN1         SETGT     FISGN06L
064600001124     C                   DO        *HIVAL
064700010405     C  N10KSGN1         READE     FISGN06L                               88
064800010405     C   10KSGN1B        READE     FISGN06L                               88
064900001124     C   88              LEAVE
065000001124     C     SGNATB        IFEQ      ' '
065100010405     C     SGNFL2        ANDNE     'N'
065200001124     C                   SETON                                        30
065300001124     C                   LEAVE
065400001124     C                   ENDIF
065500001124     C                   ENDDO
065600020801
065700020801     C  N30              SETON                                        4490
065800020801     C  N30              GOTO      ENDCTR
065900001124      *
066000001124     C                   ELSE
066100001124      *
066200001124      *  F N B L D
066300001124      *  =========
066400020801      *
066500020801     c                   clear                   woksgn
066600020801      * Leggo per tutte le lnp del terminal
066700020801  b2 c                   do        30            xx
066800020801     c                   if        l1(xx) = *zeros
066900020801     c                   leave
067000020801     c                   endif
067100020801     c                   movel     l1(xx)        lnp
067200020801
067300001124      *  DEVO FARE 2 TENTATIVI:
067400001124      *  IN   STAMPA CON ' ' E CON 'E' -SOLO ETICHETTE ESTERE
067500001124      *  IN RISTAMPA CON 'S' E CON 'Z' -SOLO ETICHETTE ESTERE
067600971217     C  N10              MOVEL     ' '           FST
067700971217     C   10              MOVEL     'S'           FST
067800020801
067900020801     C     KBLD1         SETLL     FNBLD06J
068000020801  b3 c                   do        *Hival
068100020801     c     kbld1         reade     fnbld06j
068200020801     c                   if        %eof(fnbld06j)
068300020801     c                   leave
068400020801     c                   endif
068500020801      * ok trovato un segnacollo
068600020801     c                   eval      woksgn = '1'
068700020801     c                   leave
068800020801 e3  c                   enddo
068900020801      * se trovato un segnacollo esco dal ciclo delle linee di partenza
069000020801     c                   if        woksgn <> *blanks
069100020801     c                   leave
069200020801     c                   endif
069300020801      * secondo tentativo se non ho trovato nessun segnacollo
069400971217     C**
069500020801     C  N10              movel     'E'           fst
069600020801     C   10              movel     'Z'           fst
069700020801     C     KBLD1         SETLL     FNBLD06J
069800020801  b3 c                   do        *Hival
069900020801     c     kbld1         reade     fnbld06j
070000020801     c                   if        %eof(fnbld06j)
070100020801     c                   leave
070200020801     c                   endif
070300020801      * ok trovato un segnacollo
070400020801     c                   eval      woksgn = '1'
070500020801     c                   leave
070600020801  e3 c                   enddo
070700020801      * se trovato un segnacollo esco dal ciclo delle linee di partenza
070800020801     c                   if        woksgn <> *blanks
070900020801     c                   leave
071000020801     c                   endif
071100020801  b2 c                   enddo
071200020801      * errore se non ci sono segnacolli da stampare
071300020801     c                   if        woksgn = *blanks
071400020801     c                   seton                                        4490
071500020801     c                   goto      endctr
071600020801     c                   endif
071700001124      *
071800001124     C                   ENDIF
071900971217     C**
072000960320     C* OBBLIGATORIO IMMETTERE L'ORDINMANETO
072100960320     C     V1COR2        IFEQ      ' '
072200960320     C                   SETON                                        5590
072300960320     C                   GOTO      ENDCTR
072400910205     C                   END
072500960320     C                   END
072600910205     C*
072700910205     C* CONTROLLO SE ESISTE ALMENO UN SEGNACOLLO DA STAMPARE PER
072800910205     C*   QUEL PADRONCINO
072900951102    1C     VIDCPR        IFNE      *BLANKS
073000951102     C     VIDCPR        ANDNE     *ZEROS
073100950313RM*  C                   MOVEL     VIDCFL        CPR               7 0
073200950313RM*  C                   MOVE      VIDCPR        CPR               7 0
073300001124     C                   MOVEL     CPR           LNP
073400001124     C                   MOVE      CPR           PDR
073500001124      *  F I S G N
073600001124      *  =========
073700001124     C     *IN77         IFEQ      *ON
073800001124     C                   SETOFF                                       30
073900001124     C                   Z-ADD     0             ST2
074000010405     C  N10KSGN2         SETLL     FISGN07L
074100010405     C   10KSGN2         SETGT     FISGN07L
074200001124     C                   DO        *HIVAL
074300010405     C  N10KSGN2         READE     FISGN07L                               88
074400010405     C   10KSEI          READE     FISGN07L                               88
074500001124     C   88              LEAVE
074600001124     C     SGNATB        IFEQ      ' '
074700010405     C     SGNFL2        ANDNE     'N'
074800001124     C                   SETON                                        30
074900001124     C                   LEAVE
075000001124     C                   ENDIF
075100001124     C                   ENDDO
075200001124      *
075300001124     C                   ELSE
075400001124      *  F N B L D
075500001124      *  =========
075600001124      *  DEVO FARE 2 TENTATIVI:
075700001124      *  IN   STAMPA CON ' ' E CON 'E' -SOLO ETICHETTE ESTERE
075800001124      *  IN RISTAMPA CON 'S' E CON 'Z' -SOLO ETICHETTE ESTERE
075900971217     C  N10              MOVEL     ' '           FST
076000971217     C   10              MOVEL     'S'           FST
076100941228     C     KBLD2         SETLL     FNBLD02J                               30
076200971217     C  N10
076300971217     CANN30              MOVEL     'E'           FST
076400971217     C   10
076500971217     CANN30              MOVEL     'Z'           FST
076600971217     C  N30KBLD2         SETLL     FNBLD02J                               30
076700001124      *
076800001124     C                   ENDIF
076900971217     C**
077000910205     C  N30              SETON                                        4390
077100910205     C  N30              GOTO      ENDCTR
077200951102     C* SE IMMESSA RAGIONE SOCIALE CONTROLLO CHE ESISTE ALMENO
077300951102     C* UN SEGNACOLLO DA STAMPARE CON QUELLA RAGIONE SOCIALE
077400951102    2C     V1CRSM        IFNE      *BLANKS
077500951102     C                   EXSR      CTRMIT
077600951102     C   90              GOTO      ENDCTR
077700951102    2C                   ENDIF
077800951102    1C                   ENDIF
077900910412     C*
078000910412     C* SE RICHIESTA RISTAMPA
078100910412     C     VIDRSN        IFEQ      'S'
078200910412     C*
078300910412     C* DATA SPEDIZIONE OBBLIGATORIA
078400941228     C                   Z-ADD     VIDDSP        G02DAT
078500941228     C                   MOVEL     *BLANK        G02ERR
078600941228     C                   CALL      'XSRDA8'
078700941228     C                   PARM                    WLBDAT
078800941228     C* ERRATA
078900941228    2C     G02ERR        IFEQ      '1'
079000941228     C                   SETON                                        4790
079100941228    2C                   ENDIF
079200941228     C                   Z-ADD     G02INV        COMDSP
079300941228     C                   Z-ADD     G02DAT        VIDDSP
079400910412     C                   END
079500960320     C* NON SI PUO' IMMETTERE LA RAGIONE SOCIALE SE STAMPA PER SOLO
079600960320     C*  DESTINATARIO
079700960320     C     V1COR1        IFEQ      'D'
079800960320     C     V1CRSM        ANDNE     *BLANKS
079900960320     C                   SETON                                        5690
080000960320    2C                   ENDIF
080100910412     C*
080200910205     C     ENDCTR        ENDSR
080300910205     C*
080400910205     C* RICERCA CLIENTI CON SEGNACOLLI DA STAMPARE -------------------*
080500910205     C     CTRKSC        BEGSR
080600910205     C                   EXSR      PULSF
080700910205     C                   Z-ADD     0             NRR               4 0
080800910205     C                   Z-ADD     1             KSC
080900001124      * F I S G N
081000001124      * =========
081100001124     C     *IN77         IFEQ      *ON
081200010122     C     KSGN1B        SETLL     FISGN06L
081300001124     C*
081400001124     C                   DO        *HIVAL
081500001124     C                   SETOFF                                       32
081600001124     C                   READ      FISGN06L                               30
081700010122     C* ESCO A CAMBIO TNP
081800010122     C  N30SGNTNP        IFNE      SIMFEL
081900010122     C                   SETON                                        30
082000010122     C                   ENDIF
082100010406     C* SCARTO RECORD SE GIA' STAMPATO
082200010406     C  N30SGNFL2        IFNE      'N'
082300010406     C* PER GIA' STAMPATO SALTO
082400010406     C     SGNST2        IFEQ      0
082500001124     C*
082600001124     C                   MOVEL     W0070         WLINEA            3 0
082700001124     C                   MOVE      W0070         WPADRO            3 0
082800001124     C*
082900001124     C* SE RICERCA CLIENTI DI UNO PADRONCINO CARICO SOLO QUELLI
083000001124     C     V1CRKS        IFEQ      '?'
083100001124     C     W0070         ANDGT     *ZEROS
083200001124     C     WLINEA        ANDEQ     SGNLNP
083300001124     C     WPADRO        ANDEQ     SGNCPD
083400001124     C* SE E' RICERCA CLIENTI PER PADRONCINO MA NON IMMESSO CODICE PAD
083500001124     C     V1CRKS        OREQ      '?'
083600001124     C     W0070         ANDEQ     0
083700001124     C* SE NON E' RICERCA CLIENTI PER PADRONCINO
083800001124     C     RICKSC        OREQ      '?'
083900001124     C*  --> C A R I C O
084000001124     C                   MOVEL     *BLANKS       VI5SCE
084100001124     C                   MOVEL     SGNKSC        VI5KSC
084200001124     C                   MOVEL     SGNKSC        KSC
084300001124     C     KACO          CHAIN     CNACO000                           31
084400001124     C  N31              MOVEL(P)  ACORAG        DE5KSC
084500001124     C   31              MOVEL(P)  SGNRSM        DE5KSC
084600001124     C                   SETOFF                                       31
084700001124     C                   MOVE      *ALL'0'       VI5PDR
084800001124     C                   MOVEL     SGNLNP        VI5PDR
084900001124     C                   MOVE      SGNCPD        VI5PDR
085000001124     C* SCRIVO SUBFILE
085100001124     C                   SETON                                        32
085200001124     C                   ADD       1             NRR
085300001124     C                   WRITE     LS73DSF
085400001124     C                   ENDIF
085500001124     C                   ENDIF
085600001124     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
085700001124     C* SOLO SE NON RICERCA PER CODICE PADRONCINO ANCHE
085800001124     C     RICKSC        IFNE      *BLANKS
085900001124     C     V1CRKS        ORNE      *BLANKS
086000001124     C     W0070         ANDEQ     0
086100001124     C     *IN32         OREQ      *ON
086200001124     C  N30              Z-ADD     SGNKSC        KSC
086300001124     C  N30              ADD       1             KSC
086400010122     C  N30KSGN1B        SETLL     FISGN06L
086500001124     C                   ENDIF
086600001124     C*
086700010406     C                   ENDIF
086800001124     C  N30              END
086900001124      *
087000001124     C                   ELSE
087100001124      * F N B L D
087200001124      * =========
087300020801      * Leggo per tutte le lnp del terminal
087400020801  b2 c                   do        30            xx
087500020801     c                   if        l1(xx) = *zeros
087600020801     c                   leave
087700020801     c                   endif
087800020801     c                   movel     l1(xx)        lnp
087900020801
088000020912     c                   Z-Add     1             Ksc
088100020801     C     kbld3         SETLL     FNBLD06J
088200910412     C*
088300910205     C                   DO        *HIVAL
088400951102     C                   SETOFF                                       32
088500020801     C     lnp           READe     FNBLD06J                               30
088600910205     C* PER FINE FILE O GIA' STAMPATO SALTO
088700910412     C  N30BLDFST        IFEQ      *BLANKS
088800971217     C     BLDFST        OREQ      'E'
088900951102     C*
089000951102     C* SE RICERCA CLIENTI DI UNO PADRONCINO CARICO SOLO QUELLI
089100951102     C     V1CRKS        IFEQ      '?'
089200951102     C     W0070         ANDGT     *ZEROS
089300951102     C     W0070         ANDEQ     BLDPDR
089400951102     C* SE E' RICERCA CLIENTI PER PADRONCINO MA NON IMMESSO CODICE PAD
089500951102     C     V1CRKS        OREQ      '?'
089600951102     C     W0070         ANDEQ     0
089700951102     C* SE NON E' RICERCA CLIENTI PER PADRONCINO
089800951102     C     RICKSC        OREQ      '?'
089900951102     C*  --> C A R I C O
090000910205     C                   MOVEL     *BLANKS       VI5SCE
090100910205     C                   MOVEL     BLDCCM        VI5KSC
090200910205     C                   MOVEL     BLDCCM        KSC
090300910205     C     KACO          CHAIN     CNACO000                           31
090400910205     C  N31              MOVEL     ACORAG        DE5KSC
090500951102     C   31              MOVEL     BLPRSM        DE5KSC
090600951102     C                   SETOFF                                       31
090700951102     C                   MOVEL     BLDPDR        VI5PDR
090800910205     C* SCRIVO SUBFILE
090900951102     C                   SETON                                        32
091000910205     C                   ADD       1             NRR
091100941228     C                   WRITE     LS73DSF
091200951102     C                   ENDIF
091300951102     C                   ENDIF
091400910412     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
091500951102     C* SOLO SE NON RICERCA PER CODICE PADRONCINO ANCHE
091600951102     C     RICKSC        IFNE      *BLANKS
091700951102     C     V1CRKS        ORNE      *BLANKS
091800951102     C     W0070         ANDEQ     0
091900951102     C     *IN32         OREQ      *ON
092000910313     C  N30              Z-ADD     BLDCCM        KSC
092100910306     C  N30              ADD       1             KSC
092200020801     C  N30kbld3         SETLL     FNBLD06J
092300951102     C                   ENDIF
092400910205     C*
092500910205     C  N30              END
092600020801
092700020801     c                   enddo
092800020801
092900910205     C*
093000001124     C                   ENDIF
093100001124     C*
093200910205     C                   ENDSR
093300910205     C*
093400910205     C* RICERCA PADRON. CON SEGNACOLLI DA STAMPARE -------------------*
093500910205     C     CTRCPR        BEGSR
093600941228     C*
093700910205     C                   EXSR      PULSF2
093800941228     C*
093900910205     C                   Z-ADD     0             NR2               4 0
094000910205     C                   Z-ADD     1             CPR
094100001124      * F I S G N
094200001124      * =========
094300001124     C     *IN77         IFEQ      *ON
094400001124      *
094500020703      * Leggo per tutte le lnp del terminal
094600020703  b1 c                   do        30            xx
094700020703     c                   if        l1(xx) = *zeros
094800020703     c                   leave
094900020703     c                   endif
095000020703     c                   movel     l1(xx)        lnp
095100020703
095200020703     c     lnp           setll     fisgn07l
095300001124    1C                   DO        *HIVAL
095400020703     c     lnp           reade     fisgn07l                               30
095500010406     C* IGNORO RECORD SE DI SOLA RISTAMPA E SE TER-PART <> SIMFEL
095600010406     C  N30SGNFL2        IFNE      'N'
095700010406     C     SGNTNP        ANDEQ     SIMFEL
095800010406     C* PER GIA' STAMPATO SALTO
095900010406    2C     SGNST2        IFEQ      0
096000001124     C                   MOVEL     *BLANKS       VI5SCE
096100001124     C                   MOVE      *ALL'0'       VI5CPR
096200001124     C                   MOVEL     SGNLNP        VI5CPR
096300001124     C                   MOVE      SGNCPD        VI5CPR
096400021203     C*    VI5CPR        CHAIN     FNAPD000                           31
096500021205     C                   move      VI5CPR        kpdpdr
096600021203     C                   move      'A'           kpdtip
096700021203     C     kfapd         chain     fiapd01l                           31
096800001124     C  N31              MOVEL     APDRSC        DE5CPR
096900001124     C   31              MOVEL     *BLANKS       DE5CPR
097000001124     C* SCRIVO SUBFILE
097100001124     C                   ADD       1             NR2
097200001124     C                   WRITE     LS73SF2
097300001124    2C                   END
097400010406     C                   ENDIF
097500001124     C  N30              END
097600020703 e1  c                   enddo
097700001124      *
097800001124     C                   ELSE
097900001124      * F N B L D
098000001124      * =========
098100020703      *
098200020703      * Leggo per tutte le lnp del terminal
098300020703  b1 c                   do        30            xx
098400020703     c                   if        l1(xx) = *zeros
098500020703     c                   leave
098600020703     c                   endif
098700020912     c                   clear                   cpr
098800020703     c                   movel     l1(xx)        cpr
098900020703
099000941228     C     CPR           SETLL     FNBLD02J
099100941228    1C                   DO        *HIVAL
099200941228     C                   READ      FNBLD02J                               30
099300020703      * controllo se la linea dell'autotrasportatore è la stessa che ho
099400020703      * impostato dalla l1
099500020703     c                   if        not *in30
099600020703     c                   movel     bldpdr        sav_linpdr
099700020703     c                   if        l1(xx) <> sav_linpdr
099800020703     c                   leave
099900020703     c                   endif
100000020703     c                   endif
100100910205     C* PER FINE FILE O GIA' STAMPATO SALTO
100200941228    2C  N30BLDFST        IFEQ      *BLANKS
100300971217     C     BLDFST        OREQ      'E'
100400910205     C                   MOVEL     *BLANKS       VI5SCE
100500941228     C                   MOVEL     BLDPDR        VI5CPR
100600021203     C*    BLDPDR        CHAIN     FNAPD000                           31
100700021205     C                   move      BldPdr        kpdpdr
100800021203     C                   move      'A'           kpdtip
100900021203     C     kfapd         chain     fiapd01l                           31
101000941228     C  N31              MOVEL     APDRSC        DE5CPR
101100910205     C   31              MOVEL     *BLANKS       DE5CPR
101200910205     C* SCRIVO SUBFILE
101300910205     C                   ADD       1             NR2
101400941228     C                   WRITE     LS73SF2
101500941228    2C                   END
101600910313     C* MI RIPOSIZIONO SU UN CODICE PIU' ALTO
101700020801     C     1             ADD       BLDPDR        CPR
101800020801     C     CPR           SETLL     FNBLD02J
101900910205     C  N30              END
102000020703 e1  c                   enddo
102100910205     C*
102200001124     C                   ENDIF
102300001124     C*
102400910205     C                   ENDSR
102500910205     C*
102600910205     C* PULIZIA SUBFILE ---------------------------------------------
102700910205     C     PULSF         BEGSR
102800910205     C                   SETON                                        35
102900941228     C                   WRITE     LS73DCT
103000910205     C                   SETOFF                                       35
103100910205     C                   ENDSR
103200910205     C*
103300910205     C* PULIZIA SUBFILE2 --------------------------------------------
103400910205     C     PULSF2        BEGSR
103500910205     C                   SETON                                        35
103600941228     C                   WRITE     LS73CT2
103700910205     C                   SETOFF                                       35
103800910205     C                   ENDSR
103900951102     C*
104000960320     C* CONTROLLO SE ESISTE UN SEGNACOLLO DA STAMPARE  --------------
104100951102     C     CTRMIT        BEGSR
104200951102     C*
104300951102     C                   CLEAR                   WPOS
104400951102     C     ' '           CHECKR    V1CRSM        WPOS              2 0
104500951102     C     WPOS          ADD       1             X                 2 0
104600001124     C** FISGN
104700001124     C** =====
104800001124     C     *IN77         IFEQ      *ON
104900001124     C                   Z-ADD     0             ST2
105000010406     C  N10KSGN2         SETLL     FISGN07L
105100010406     C   10KSGN2         SETGT     FISGN07L
105200010406     C  N10KSGN2         READE     FISGN07L                               30
105300010406     C   10KSEI          READE     FISGN07L                               30
105400001124     C     *IN30         DOWEQ     *OFF
105500010405     C                   CLEAR                   W015A
105600010405     C     SGNATB        IFEQ      *BLANKS
105700010405     C     SGNFL2        ANDNE     'N'
105800001124     C*
105900001124     C                   MOVEA     SGNRSM        K15
106000001124     C     X             IFNE      16
106100001124     C                   MOVEA     *BLANKS       K15(X)
106200001124     C                   ENDIF
106300001124     C                   MOVEA     K15           W015A            15
106400010405     C                   ENDIF
106500001124     C     W015A         IFNE      V1CRSM
106600010406     C  N10KSGN2         READE     FISGN07L                               30
106700010406     C   10KSEI          READE     FISGN07L                               30
106800001124     C                   ELSE
106900001124     C                   SETON                                        30
107000001124     C                   ENDIF
107100001124     C*
107200001124     C                   ENDDO
107300001124     C                   ELSE
107400001124     C** FNBLD
107500001124     C** =====
107600971217     C  N10              MOVEL     ' '           FST
107700971217     C  N10              MOVEL     'E'           FST2
107800971217     C   10              MOVEL     'S'           FST
107900971217     C   10              MOVEL     'Z'           FST2
108000971217     C**
108100971217     C                   DO        2
108200951102     C     KBLD2         SETLL     FNBLD02J
108300951102     C     KBLD2         READE     FNBLD02J                               30
108400951102     C     *IN30         DOWEQ     *OFF
108500951102     C*
108600951102     C                   MOVEA     BLPRSM        K15
108700951102     C     X             IFNE      16
108800951102     C                   MOVEA     *BLANKS       K15(X)
108900951102     C                   ENDIF
109000951102     C                   MOVEA     K15           W015A            15
109100951102     C     W015A         IFNE      V1CRSM
109200951102     C     KBLD2         READE     FNBLD02J                               30
109300951102     C                   ELSE
109400951102     C                   SETON                                        30
109500951102     C                   ENDIF
109600951102     C*
109700951102     C                   ENDDO
109800971217     C                   MOVEL     FST2          FST
109900971217     C                   ENDDO
110000001124      *
110100001124     C                   ENDIF
110200001124     C* -----
110300951102     C     W015A         IFNE      V1CRSM
110400951102     C                   SETON                                        5390
110500951102     C                   ENDIF
110600001124     C* -----
110700951102     C                   ENDSR
