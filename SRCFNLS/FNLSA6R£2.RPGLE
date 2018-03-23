000100980717     H DECEDIT('0,') DATEDIT(*DMY.)
000200941228     H* FNLSA6R *----------------------------------------------------*
000300920908     H*             QUADRATURA ENTRATA A MAGAZZINO       FILTRO      *
000400000000     H*--------------------------------------------------------------*
000500941228     FFNLSA6D   CF   E             WORKSTN
000600941228     F                                     SFILE(LSA6DSF:NRR)
000700920730     FCNACO00F  IF   E           K DISK
000800021203     FFIAPD01L  IF   E           K DISK
000900941228     FFNFVV01L  IF   E           K DISK
001000070117     FFNBRV01L  IF   E           K DISK
001100920901     FTABEL00F  IF   E           K DISK
001200920901     FAZORG01L  IF   E           K DISK
001300961025     FEDTAB01L  IF   E           K DISK    USROPN
001400920804     D L1              S              3  0 DIM(30)
001500920901     D L6              S              3  0 DIM(30)
001600980107     D*
001700020221     D******              K3N       900  7 0             KEY    TAB 3N
001800020221     D******              T3N       900  7 0             TBLUNI TAB 3N
001900961025     D*
002000961025     D EDK             S             35    DIM(99)
002100961025     D EDC             S              7  0 DIM(99)
002200060310     D EDS             S              2    DIM(99)
002300980107     D EDL             S              3  0 DIM(99)
002400980717     D KCL             S              7  0 DIM(100)
002500980717     D TCL             S              7  0 DIM(100)
002600950530     D*
002700961025     D TE2             S             21    DIM(3) CTDATA PERRCD(1)
002800060310     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
002900980108     D*
003000980108     D* SCHIERA DI COMODO X CARICAMENTO LNP
003100980108     D LP              S              3  0 DIM(12)
003200980108     D CLP             S              3  0 DIM(12)
003300950530     D**
003400950530     D* PASSAGGIO PARAMETRI AL BATCH - FNLSA7R -
003500950530     D**
003600900523     D PARAM           DS
003700920831     D  VIDNFV                 1      5
003800950323     D  COMANS                 6      9  0
003900950323     D  COMMGS                10     13  0
004000950323     D  COMDSP                 6     13  0
004100950323     D  LNP                   14     37P 0
004200950323     D                                     DIM(12)
004300950323     D  CPC                   38     85P 0
004400950323     D                                     DIM(12)
004500950323     D  CLI                   86    133P 0
004600950323     D                                     DIM(12)
004700950323     D  DRT                  134    193P 0
004800950323     D                                     DIM(12)
004900060310     D  NRS                  194    217
005000950323     D                                     DIM(12)
005100950323     D  MOP                  218    229
005200950323     D                                     DIM(12)
005300950323     D  NRT                  230    241
005400961030     D                                     DIM(12)
005500961025     D* SOLO PER EDI: FLAG DI CREAZIONE DEL FIL EDBRV00F
005600961025     D  EDSEL                242    242
005700080626     D  VID9000              243    243
005800980415     D  COMDSA               246    250P 0
005900980108     D  PARTQU               251    251
006000980108     D  VIDSN                252    252
006100980108     D  VIDSTA               253    253
006200980108     D  VIDFIL               254    256  0
006300950530     D**
006400950530     D* RICERCA CODICE PADRONCINO     -FNLV24R -
006500021203     D FNLV24DS      E DS
006600950530     D**
006700021203     D*PARAM2          DS
006800021203     D* PARFAP                 1      3  0
006900021203     D* PARPAD                 4     10  0
007000021203     D* PARFLG                11     11
007100021203     D* PARDES                12     46
007200920730     D                 DS
007300961017     D  WCLI                   1      7  0
007400941228     D  VIDDRT                 8     15  0
007500941228     D  RECORD                 1     15  0
007600961017     D                 DS
007700961017     D  VIDKLP                 1      3  0
007800961017     D  VIDKSC                 4      7  0
007900961017     D  VIDCLI                 1      7  0
008000950530     D**
008100950530     D* RICEZIONA PARAMETRI DA KPJBU
008200950530     D**
008300950530     D* TQU: TIPO QUADRATURA: ' ' - CON FOGLIO ENTRATA
008400950530     D*                       'P' - CON SPUNTE PARTENZA ED IMP
008500950530     D PARAM3          DS
008600950530     D  PA3TQU                 1      1
008700950530     D*
008800930125     D                 DS
008900930125     D  COMDAT                 7     12
009000930125     D  COMDF0                 1     25
009100941228     D* RICERCA FOGLI VARI
009200941228     D DSLR01        E DS
009300941228     D*
009400920831     D CNCR80        E DS
009500930520     D DS3C          E DS
009600920421     D KPJBA         E DS
009700020429     D***                                     1   3 REM
009800020429     D***                                     4   6 REMFIL
009900941228     D***
010000941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
010100941228     D***
010200941228     D WLBDAT          DS                  INZ
010300941228     D  G02DAT                 1      8  0
010400941228     D  G02INV                 9     16  0
010500941228     D  G02ERR                17     17
010600941228     D  G02TGI                18     22  0
010700941228     D***
010800941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
010900941228     D***
011000941228     D WGIDAT          DS                  INZ
011100941228     D  GIODAT                 1      8  0
011200941228     D  GIOINV                 9     16  0
011300941228     D  GIOTGI                17     21  0
011400920901     D TCUDS           DS
011500920901     D  F1                     1      1
011600920901     D  F3                     3      3
011700920901     D  F2                     2      2
011800920901     D  F4                     4      4
011900920901     D  F56                    5      6
012000941228     D* DS PER TRUL06R - CARICAMENTO £X
012100941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
012200941228     D  LIN                    1     90  0
012300941228     D                                     DIM(30)
012400941228     D*
012500941228     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
012600941228     D DSLV50        E DS                  EXTNAME(FNLV50DS)
012700961025     D*
012800961025     D* DS PER EDI
012900961025     D EDSPT         E DS                  EXTNAME(EDIDSPT)
013000961025     D EDSPU         E DS                  EXTNAME(EDIDSPU)
013100980717     D EDSCL         E DS                  EXTNAME(EDIDSCL)
013200021203
013300021203     D kpdpdr          S                   LIKE(APDpdr)
013400021203     D kpdtip          S                   LIKE(APDtip)
013500060310     D alfa3cnrs       S              2
013600941228     D*
013700920804     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
013800920901     D  TCU                  398    697
013900920901     D                                     DIM(50)
014000920901     D  KCU                  698    847P 0
014100920901     D                                     DIM(50)
014200920901     D                                     PACKEVEN
014300900702     C****************************************************************
014400900702     C*  RIEPILOGO INDICATORI
014500900702     C***************************************************************
014600950530     C* 01    - ON - QUADRATURA CON SPUNTE PARTENZA 'P'
014700961025     C* 02    - ON - QUADRATURA CON SPUNTE ENTRATA PER E.D.I.
014800950530     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
014900020429     C* 05    - SONO UN SECONDO LIVELLO
015000920804     C* 36    - SFLNXTCHG
015100920804     C* 37    - PULIZIA SUBFILE
015200920522     C* 30/35 - DI COMODO
015300920901     C* 40/57 - ERRORE
015400920804     C* 98/99 - ERRORE GENERICO
015500900702     C*****************************************************************
015600000000     C     *ENTRY        PLIST
015700000000     C                   PARM                    KPJBA
015800950530     C                   MOVEL     KPJBU         PARAM3
015900950530     C*
016000000000     C                   Z-ADD     1             CODUT
016100920804     C                   CALL      'X§PARUT'
016200920804     C                   PARM                    UT§DSE
016300000000     C                   MOVEL     RAGUT         RSUT             20
016400920901     C                   MOVEL     RAGUT         PARDUT           30
016500920831     C                   MOVEL     REC80         CNCR80
016600920901     C*
016700920901     C*--- RICERCA CAPOCONTI------------------------------------------*
016800920901     C                   DO        50            X                 3 0
016900920901     C                   MOVE      TCU(X)        TCUDS
017000920901     C     F56           CABNE     'CG'          END1
017100920901     C     F4            COMP      '1'                                    21
017200920901     C     F4            COMP      '2'                                    22
017300920901     C     F4            COMP      '3'                                    23
017400920901     C     F4            COMP      '6'                                    27
017500920901     C** 1 CLIENTI   21
017600920901     C** 2 FORNITORI 22
017700920901     C** 3 AGENTI    23
017800920901     C     F3            COMP      '0'                                242425
017900920901     C     F3            COMP      'I'                                    26
018000920901     C** 0 ITALIA   25
018100920901     C** 1 ESTERO   24
018200920901     ** I CAPO CONTO IVA
018300920901     C   21
018400920901     CAN 24              Z-ADD     KCU(X)        KCE               4 0
018500920901     C   21
018600920901     CAN 25              Z-ADD     KCU(X)        KCI               4 0
018700920901     C   22
018800920901     CAN 24              Z-ADD     KCU(X)        KFE               4 0
018900920901     C   22
019000920901     CAN 25              Z-ADD     KCU(X)        KFI               4 0
019100920901     C   23
019200920901     CAN 24              Z-ADD     KCU(X)        KAE               4 0
019300920901     C   23
019400920901     CAN 25              Z-ADD     KCU(X)        KAI               4 0
019500920901     C   26              Z-ADD     KCU(X)        KIVA              4 0
019600920901     C   27              Z-ADD     KCU(X)        KBNA              4 0
019700920901     C     END1          TAG
019800920901     C                   END
019900920901     C*---------------------------------------------------------------*
020000920730     C* ACCESSO CNACO
020100920730     C     KACO          KLIST
020200920730     C                   KFLD                    CODUT
020300920901     C                   KFLD                    KCI
020400920901     C                   KFLD                    KSC               7 0
020500920730     C* ACCESSO TABEL
020600920730     C     KTAB          KLIST
020700920730     C                   KFLD                    CODUT
020800920730     C                   KFLD                    COD               2
020900920730     C                   KFLD                    KEY               8
021000961030     C     KTAB2         KLIST
021100961030     C                   KFLD                    CODUT
021200961030     C                   KFLD                    COD               2
021300961025     C     KEDTAB        KLIST
021400961025     C                   KFLD                    COD
021500961025     C                   KFLD                    EDKEY            35
021600920731     C* ACCESSO FLFVV
021700910320     C     KFVV          KLIST
021800911015     C                   KFLD                    COMNPG            1 0
021900911015     C                   KFLD                    COMNFV            5 0
022000941228     C                   KFLD                    VIDFIL
022100000131     C* ACCESSO FIBRV
022200000131     C     KBRV          KLIST
022300000131     C                   KFLD                    COMNPG            1 0
022400000131     C                   KFLD                    WNFV
022500000131     C                   KFLD                    VIDFIL
022600000131     C*
022700021203     C     Kfapd         klist
022800021203     C                   kfld                    kpdtip
022900021203     C                   kfld                    kpdpdr
023000021203      *
023100000131     C     *LIKE         DEFINE    BRVNFV        WNFV
023200941228     C*---------------------------------------------------------------*
023300920423     C*
023400920423     C* CAMPI DI PASSAGGIO
023500941228     C                   CLEAR                   DSLR01
023600941228     C                   MOVEL     5             DLRNPG
023700941228     C                   MOVEL     'FNLSA6R'     DLRPGM
023800941228     C                   MOVEL     'FNLSA6R'     VIDPGM
023900950530     C*
024000961025     C* 01 ON - QUADRATURA 'P'  SPUNTE PARTENZA
024100961025     C* 02 ON - QUADRATURA 'E'  SPUNTE ENTRATA CON EDI
024200961025     C                   SELECT
024300961025     C     PA3TQU        WHENEQ    'P'
024400950530     C                   SETON                                        01
024500950530     C                   MOVEL     TE2(2)        VIDTE2
024600961025     C     PA3TQU        WHENEQ    'E'
024700961025     C                   SETON                                        02
024800961025     C                   MOVEL     TE2(3)        VIDTE2
024900961025     c*
025000961025     C                   OPEN      EDTAB01L                             97
025100961025     C     *IN97         IFEQ      *ON
025200961030     C                   MOVEL     MSG(6)        VIDMSG
025300961025     C                   SETON                                          28
025400961025     C                   ENDIF
025500961025     C**
025600961025     C                   OTHER
025700950530     C                   MOVEL     TE2(1)        VIDTE2
025800961028     C                   ENDSL
025900920424     C*
026000941228     C* DATA GIORNO - 1
026100950703     C*
026200941228     C                   TIME                    W0140            14 0
026300941228     C* UDATE IN GGMMAAAA
026400941228     C                   MOVE      W0140         WDTGIO            8 0
026500941228     C*
026600941228     C* UDATE IN AAAAMMGG
026700941228     C                   Z-ADD     WDTGIO        G02DAT
026800941228     C                   MOVEL     *BLANK        G02ERR
026900941228     C                   CALL      'XSRDA8'
027000941228     C                   PARM                    WLBDAT
027100941228     C                   MOVEL     G02INV        DATEU             8 0
027200941228     C* SUB        1
027300941228     C     G02TGI        SUB       1             GIOTGI
027400941228     C*
027500941228     C                   CALL      'XSRGI8'
027600941228     C                   PARM                    WGIDAT
027700920424     C*
027800941228     C                   MOVEL     GIODAT        SAVDIN            8 0
027900920730     C*
028000020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
028100020429     C***                  MOVELSIMFEL    VIDFIL
028200020429     C                   Z-ADD     SIMPOU        VIDFIL
028300920831     C*
028400020429     C***        REM       IFEQ 'REM'
028500020429     C***        REMFIL    ANDGT*ZEROS
028600020429     C***                  MOVELREMFIL    VIDFIL
028700020429     C     SIMTPP        IFEQ      '2'
028800020429     C     SIMTPP        OREQ      *BLANKS
028900920831     C                   EXSR      CARL6
029000920831     C                   SETON                                        05
029100920831     C                   END
029200920831     C*
029300930125     C* CONTROLLO SE SI TRATTA DI FILIALE CON "CML"
029400930125     C     VIDFIL        CHAIN     AZORG                              30
029500930125     C  N30              MOVEL     ORGDF0        COMDF0
029600930125     C  N30COMDAT        IFNE      *BLANKS
029700930125     C* SI TRATTA DI FILIALE CON "CML"
029800930125     C                   MOVEL     'N'           VIDSTA
029900930125     C                   ELSE
030000930125     C* SI TRATTA DI FILIALE SENZA "CML"
030100930125     C                   MOVEL     'S'           VIDSTA
030200930125     C                   END
030300961025     C*
030400961025     C     *IN02         IFEQ      *ON
030500961025     C                   MOVEL     'N'           VIDSTA
030600961025     C                   ENDIF
030700930125     C*
030800920730     C* CARICO TABELLA FILIALI GESTITE £1
030900920730     C                   EXSR      CARTAB
031000920730     C*
031100920424     C     INIZIO        TAG
031200920730     C* PULIZIA DATI SUBFILE CONTROL
031300920901     C                   SETOFF                                       04
031400920730     C                   Z-ADD     0             VIDDSP
031500980415     C                   Z-ADD     0             VIDDSA
031600920730     C                   MOVEL     *BLANKS       VIDNFV
031700920424     C                   MOVEL     *BLANKS       VIDDES
031800920730     C                   Z-ADD     5             COMNPG
031900920916     C                   MOVEL     'N'           VIDSN
032000080626     C   01              MOVEL     'N'           VID9000
032100920423     C*
032200920730     C                   Z-ADD     0             NRR               4 0
032300920730     C* CARICO SUBFILE
032400920831     C                   EXSR      CARICA
032500920730     C*
032600900509     C     FOR01         TAG
032700941228     C                   WRITE     LSA6D01
032800920730     C     FORCT         TAG
032900941228     C                   EXFMT     LSA6DCT
033000941228     C*
033100961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
033200961017     C                   CLEAR                   VIDMSG
033300941228     C                   SETOFF                                       289055
033400961025     C                   SETOFF                                       444556
033500961028     C                   SETOFF                                       59
033600920730     C*
033700910320     C** CMD3 - FINE LAVORO
033800961025     C**        O ERRORE: NON TROVATO FILE EDTAB01L
033900961025     C   97
034000961025     COR KC              GOTO      FINE
034100920831     C*
034200920831     C** CMD18- CAMBIO FILIALE
034300920831     C   KS              SETON                                        04
034400920831     C   KS              GOTO      FORCT
034500920831     C*
034600941228     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
034700941228     C   04              EXSR      CTRFGS
034800941228     C   04
034900941228     CAN 90              GOTO      FOR01
035000920730     C*
035100920730     C* CONTROLLO DATI SUBFILE
035200920730     C                   EXSR      CONSFL
035300920831     C   98              GOTO      FOR01
035400920831     C*
035500920730     C   99              GOTO      FORCT
035600920730     C*
035700920730     C* CONTROLLO DATI SUBFILE CONTROL
035800920730     C                   EXSR      CONCTL
035900920730     C   98              GOTO      FOR01
036000920730     C*
036100950113     C  NKF
036200920831     COR 99              GOTO      FORCT
036300920730     C*
036400920807     C* CARICO SCHIERE PER PASSAGGIO
036500920807     C                   MOVEA     *ZEROS        LNP
036600920807     C                   MOVEA     *ZEROS        CPC
036700920807     C                   MOVEA     *ZEROS        CLI
036800920807     C                   MOVEA     *ZEROS        DRT
036900930520     C                   MOVEA     *ZEROS        NRS
037000950323     C                   MOVEA     *BLANKS       MOP
037100950323     C                   MOVEA     *BLANKS       NRT
037200920901     C                   Z-ADD     1             X                 3 0
037300920807     C                   Z-ADD     1             NRR
037400920807     C*
037500950323     C     NRR           DOWLE     12
037600920807     C*
037700941228     C     NRR           CHAIN     LSA6DSF                            31
037800920831     C     *IN31         IFEQ      '0'
037900920831     C     VIDLNP        ANDNE     0
038000920807     C                   MOVEL     VIDLNP        LNP(X)
038100920914     C     VIDCPC        IFGT      *ZEROS
038200950313RM*  C*§                   MOVELVIDCPC    CPC,X
038300950313RM*  C                   MOVEL     VIDLNP        CPC(X)
038400950313RM*  C                   MOVE      VIDCPC        CPC(X)
038500920914     C                   END
038600930520     C                   MOVEL     VIDNRS        NRS(X)
038700920807     C                   MOVEL     VIDCLI        CLI(X)
038800941228     C                   Z-ADD     COMDRT        DRT(X)
038900950323     C                   MOVEL     VIDMOP        MOP(X)
039000950323     C                   MOVEL     VIDNRT        NRT(X)
039100920901     C                   ADD       1             X
039200920807     C                   END
039300920807     C*
039400920807     C                   ADD       1             NRR
039500920807     C                   END
039600961025     c* IMPOSTO DATI PER EDI
039700961025     C     *IN02         IFEQ      *ON
039800961025     C                   MOVEL     'S'           EDSEL
039900961025     C                   ENDIF
040000920831     C*
040100980415     C  N01COMDSA        IFEQ      0
040200980415     C                   Z-ADD     COMDSP        COMDSA
040300980415     C                   END
040400980415     C*
040500950530     C                   MOVEL     PA3TQU        PARTQU
040600900702     C                   MOVEL     PARAM         KPJBU
040700941228     C                   MOVEL     'LSA7'        KCOAZ
040800070109     c******             call      'FNLSA7R'
040900070109     C                   CALL      'BCH10'
041000900702     C                   PARM                    KPJBA
041100920522     C*
041200950411     C                   GOTO      INIZIO
041300000000     C     FINE          TAG
041400000000     C                   SETON                                        LR
041500920730     C*
041600920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
041700920730     C     CARTAB        BEGSR
041800941228     C***
041900941228     C* CARICO TABELLA FILIALI GESTITE £1
042000941228     C***
042100941228     C                   CLEAR                   DSUL06
042200941228     C                   MOVE      '£1'          D06COD
042300941228     C                   MOVEL     SIMFEL        D06KEY
042400941228     C                   MOVEL     DSUL06        KPJBU
042500941228     C*
042600950323     C                   CALL      'TRUL06R'
042700941228     C                   PARM                    KPJBA
042800941228     C                   MOVEL     KPJBU         DSUL06
042900941228     C                   MOVEA     LIN           L1
043000961025     C**
043100961025     C* CARICO TABELLA 'PT' DA EDITAB01L
043200961025     C**
043300980717     C     *IN02         IFEQ      *ON
043400980717     C     *IN97         ANDEQ     *OFF
043500961025     C                   MOVEL     'PT'          COD
043600961025     C                   Z-ADD     1             YY                3 0
043700961025     C     COD           SETLL     EDTAB01L
043800961025     C     COD           READE     EDTAB01L                               30
043900961025     C     *IN30         DOWEQ     *OFF
044000961025     C     TABFLG        IFEQ      ' '
044100020917     C                   MOVEL     TABUNI        EDSPT
044200020917      * solo se compresa nelle linee di partenza del terminal
044300020917     c     §ptlnp        lookup    L1                                     32
044400020917     c                   if        *in32 = *on
044500961025     C                   MOVEL     TABKEY        EDK(YY)
044600961025     C                   MOVEL     §PTKSC        EDC(YY)
044700961025     C                   MOVEL     §PTNRS        EDS(YY)
044800980107     C                   MOVEL     §PTLNP        EDL(YY)
044900961025     C                   ADD       1             YY
045000020917     c                   end
045100961025     C                   ENDIF
045200961025     C*
045300961025     C     COD           READE     EDTAB01L                               30
045400961025     C                   ENDDO
045500980107     C**
045600980107     C** Scorro tabella PU e se il codice corrispondente
045700980107     C** non è un partner ma un cliente azzero lnp
045800980108     C                   DO        YY            YW                4 0
045900980107     C                   MOVEL     'PU'          COD
046000980108     C                   MOVEL     EDK(YW)       EDKEY
046100980107     C     KEDTAB        CHAIN     EDTAB01L                           30
046200980108    3C     *IN30         IFEQ      *OFF
046300980108     C     TABFLG        ANDEQ     ' '
046400980107     C                   MOVEL     TABUNI        EDSPU
046500980107     C     §PUTPC        IFNE      'P'
046600980108     C                   Z-ADD     0             EDL(YW)
046700980107     C                   ENDIF
046800980107     C                   ELSE
046900980108     C                   Z-ADD     0             EDL(YW)
047000980107     C                   ENDIF
047100980107     C                   ENDDO
047200980717     C**
047300980717     C* CARICO TABELLA 'CL' DA EDITAB01L
047400980717     C**
047500980717     C                   MOVEL     'CL'          COD
047600980717     C                   Z-ADD     1             YY                3 0
047700980717     C     COD           SETLL     EDTAB01L
047800980717     C     COD           READE     EDTAB01L                               30
047900980717     C     *IN30         DOWEQ     *OFF
048000980717     C     TABFLG        IFEQ      ' '
048100980717     C                   MOVEL     TABKEY        KCL(YY)
048200980717     C                   MOVEL     TABUNI        EDSCL
048300980717     C                   MOVEL     §CLKSC        TCL(YY)
048400980717     C                   ADD       1             YY
048500980717     C                   ENDIF
048600980717     C*
048700980717     C     COD           READE     EDTAB01L                               30
048800980717     C                   ENDDO
048900961030     C**
049000961030     C* CARICO LA TABELLA 3N --> TBLKEY R TBLUNI
049100020221     C*******              MOVEL'3N'      COD
049200020221     C*******              Z-ADD1         YY
049300020221     C*******    KTAB2     SETLLTABEL00F
049400020221     C*******    KTAB2     READETABEL00F                 30
049500020221     C*******    *IN30     DOWEQ*OFF
049600020221     C*******    TBLFLG    IFEQ ' '
049700020221     C*******              MOVELTBLKEY    K3N,YY
049800020221     C*******              MOVELTBLUNI    T3N,YY
049900020221     C*******              ADD  1         YY
050000020221     C*******              ENDIF
050100020221     C*
050200020221     C*******    KTAB2     READETABEL00F                 30
050300020221     C*******              ENDDO
050400961030     C*
050500961030     C                   ENDIF
050600920730     C                   ENDSR
050700920730     C*
050800920831     C* CARICO L6 ------------------------------------------***********
050900920831     C*
051000920831     C     CARL6         BEGSR
051100941228     C*
051200941228     C                   CLEAR                   DSUL06
051300941228     C                   MOVE      '£6'          D06COD
051400941228     C                   MOVEL     VIDFIL        D06KEY
051500941228     C                   MOVEL     DSUL06        KPJBU
051600941228     C                   CALL      'TRUL06R'
051700941228     C                   PARM                    KPJBA
051800941228     C                   MOVEL     KPJBU         DSUL06
051900941228     C                   MOVEA     LIN           L6
052000920831     C                   ENDSR
052100920831     C*
052200920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
052300920730     C*
052400920831     C     CARICA        BEGSR
052500920831     C* PULIZIA SUBFILE
052600920804     C                   SETON                                        37
052700941228     C                   WRITE     LSA6DCT
052800920804     C                   SETOFF                                       37
052900920730     C*
053000920831     C* CARICO DATI IN SUBFILE
053100920730     C*
053200920730     C                   Z-ADD     0             VIDLNP
053300920730     C                   MOVEL     *BLANKS       DESLNP
053400920914     C                   MOVEL     *BLANKS       VIDCPC
053500920730     C                   MOVEL     *BLANKS       DESCPC
053600060310     C                   movel     99            VIDNRS
053700920730     C                   Z-ADD     0             VIDCLI
053800920730     C                   MOVEL     *BLANKS       DESCLI
053900920730     C                   Z-ADD     0             VIDDRT
054000941228     C                   Z-ADD     0             COMDRT
054100950323     C                   MOVE      *BLANKS       VIDMOP
054200950323     C                   MOVE      *BLANKS       VIDNRT
054300920730     C*
054400950323     C                   DO        12            NRR
054500941228     C                   WRITE     LSA6DSF
054600941228     C                   ENDDO
054700920730     C*
054800920831     C                   Z-ADD     1             REC
054900920730     C                   ENDSR
055000920730     C*
055100920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
055200920730     C*
055300920730     C     CONSFL        BEGSR
055400920831     C                   SETOFF                                       9899
055500920730     C                   Z-ADD     1             NRR
055600980108     C                   CLEAR                   UNO
055700980108     C                   Z-ADD     0             LP
055800980108     C                   Z-ADD     0             CLP
055900980108     C                   Z-ADD     0             W                 4 0
056000980108     C                   Z-ADD     0             WW                4 0
056100920730     C*
056200941228     C                   READC     LSA6DSF                                33
056300920730     C     *IN33         DOWEQ     '0'
056400961028     C* PER EDI CON ELABORAZIONE DEFINITIVA, POSSO METTERE SOLTANTO
056500961025     C* 1 RICHIESTA E SOLO IL CODICE CLIENTE
056600930520     C*
056700961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
056800961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
056900961017    2C     VIDKLP        IFGT      0
057000961017     C     VIDKLP        CHAIN     AZORG01L                           34
057100961017    3C     *IN34         IFEQ      *ON
057200961017     C                   SETON                                        284499
057300961017     C                   MOVEL     MSG(1)        VIDMSG
057400961017     C                   GOTO      ENDSFL
057500961017    3C                   ENDIF
057600961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
057700961017    3C     VIDKSC        IFEQ      0
057800961017     C     DESCLI        ANDEQ     *BLANKS
057900961017     C                   SETON                                        284599
058000961017     C                   MOVEL     MSG(3)        VIDMSG
058100961017     C                   GOTO      ENDSFL
058200961017    3C                   ENDIF
058300961017     C*
058400961017   X2C                   ELSE
058500961017    3C     VIDKSC        IFGT      0
058600961017     C                   SETON                                        284499
058700961017     C                   MOVEL     MSG(1)        VIDMSG
058800961017     C                   GOTO      ENDSFL
058900961017    3C                   ENDIF
059000961017    2C                   ENDIF
059100961017     C*
059200920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
059300920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
059400961017     C     VIDKSC        IFEQ      0
059500920730     C     DESCLI        ANDNE     *BLANKS
059600981109     C                   CLEAR                   PARDIT
059700920730     C*
059800920730     C* PARSTA = 9  ESCLUDO ANNULLATI
059900961016     C                   Z-ADD     KCI           PARKCC            4 0
060000920730     C                   Z-ADD     9             PARSTA
060100920902     C                   MOVEL     *BLANKS       DES              48
060200920902     C                   MOVEL     DESCLI        DES
060300961017     C                   MOVEL     VIDKLP        PARFLR
060400981109     C     VIDKLP        CHAIN     AZORG01L                           31
060500981109     C  N31              MOVEL     ORGDIT        PARDIT
060600000907     C                   Z-ADD     1             PAXNUM
060700000907     C                   CALL      'XALFA3BR'
060800920730     C                   PARM                    PARDUT
060900920730     C                   PARM                    CODUT
061000920902     C                   PARM                    DES
061100961016     C                   PARM                    PARKCC
061200920730     C                   PARM                    PARSTA            1 0
061300961016     C                   PARM                    PARFLR           90
061400981109     C                   PARM                    PARDIT            3
061500000907     C                   PARM                    PAXNUM            2 0
061600000907     C                   PARM                    PAXKCM           80
061700000907     C                   PARM                    PAXKSM          140
061800000907     C                   PARM                    PAXKDM           60
061900920831     C     PARSTA        IFNE      -1
062000000907     C                   MOVEL     PAXKSM        VIDCLI
062100920902     C                   MOVEL     DES           DESCLI
062200920831     C                   END
062300920831     C*
062400920831     C                   SETON                                        9899
062500920730     C                   GOTO      ENDSFL
062600920730     C                   END
062700920730     C*
062800920901     C*** LINEA DI PARTENZA
062900961025    1C     VIDLNP        IFNE      0
063000920901     C*
063100920901     C* CONTROLLO SE LINEA VALIDA
063200920901     C     VIDLNP        LOOKUP    L1                                     31
063300061212     c
063400061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
063500061212     c*  per poterla accettare
063600061212     c                   if        not *in31
063700061212     c                   if        *in01    and (vidnrs='00' or vidcli=0
063800061212     c                             or vidnrs='SI' or vidnrs='99')
063900061212     c                             or not *in01
064000061212     C                   SETON                                        4099
064100061212     C                   GOTO      ENDSFL
064200061212     c                   endif
064300070109     c
064400070109     c* Se lnp non in £1 impossibile richiedere anomalie manca bolla
064500070109     c*  di selezioni non richieste
064600070109     c                   if        vidsta='S'
064700070109     C                   SETON                                        6099
064800070109     C                   GOTO      ENDSFL
064900070109     c                   endif
065000061212     c                   endif
065100920901     C*
065200920901     C* DECODIFICO LINEA DI PARTENZA
065300920901     C     VIDLNP        CHAIN     AZORG                              30
065400920901     C  N30              MOVEL     ORGDES        DESLNP
065500920901     C   30              MOVEL     *BLANKS       DESLNP
065600061212     c
065700061212     c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
065800061212     c                             or orgfva<>' '
065900061212     C                   SETON                                        4099
066000061212     C                   GOTO      ENDSFL
066100061212     c                   endif
066200920901     C*
066300961025   X1C                   ELSE
066400920902     C* LINEA DI PARTENZA = 0
066500920902     C                   MOVEL     *BLANKS       DESLNP
066600961025     C                   MOVEL     VIDCLI        WCLI
066700961025     C*
066800920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
066900950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
067000920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
067100961028    2C     RECORD        IFNE      *ZEROS
067200100823     C     VIDCPC        ORne      *ZEROS
067300100823     C     VIDCPC        andne     *blanks
067400060310     C     VIDNRS        ORNE      '99'
067500950323     C     VIDMOP        ORNE      *BLANKS
067600950323     C     VIDNRT        ORNE      *BLANKS
067700961025     C* ERRORE: NON INSERIBILE
067800961028    3C     *IN02         IFEQ      *ON
067900961025     C     VIDSN         ANDEQ     'S'
068000980805   X C                   ELSE
068100961025     C* ERRORE : IMMETTERE ANCHE LA LINEA PARTENZA
068200920901     C                   SETON                                        4199
068300920901     C                   GOTO      ENDSFL
068400961028    3C                   END
068500961025    2C                   END
068600961025     C**
068700961025    1C                   END
068800060310     C*** Numero serie
068900060310     C* ESEGUO TESTN
069000060310     c                   if        vidnrs<>'SI'
069100060310     C                   TESTN                   VIDnrs               30
069200060310     C                   MOVE      VIDnrs        W001A             1
069300060310    2C     *IN30         IFEQ      *OFF
069400060310     C     W001A         ORLT      '0'
069500060310     C                   SETON                                        599928
069600060310     C                   MOVEL     MSG(17)       VIDMSG
069700060310     C                   GOTO      ENDSFL
069800060310    2C                   ENDIF
069900060310    2C                   ENDIF
070000920901     C*
070100920730     C*** CODICE PADRONCINO
070200941228    1C     VIDCPC        IFNE      *BLANKS
070300921012     C     VIDCPC        ANDNE     *ZEROS
070400941228     C*
070500941228     C     '?'           SCAN      VIDCPC                                 98
070600941228     C* RICERCA
070700941228    2C     *IN98         IFEQ      '1'
070800021203     C*                  MOVEL     *BLANKS       PARDES
070900021203     C*                  Z-ADD     0             PARPAD
071000021203     C*                  Z-ADD     VIDLNP        PARFAP
071100021203     C*                  MOVEL     'R'           PARFLG
071200021203     C*                  MOVEL     PARAM2        KPJBU
071300021203      *
071400021203      * imposto i campi della ds esterna fnlv24ds
071500021203     C                   clear                   fnlv24ds
071600021203     C                   eval      d24fil   = vidlnp
071700021203     C                   eval      d24tip   = 'A'
071800021203     C                   eval      d24flg   = 'R'
071900021203     C                   movel     fnlv24ds      kpjbu
072000021203
072100941228     C                   CALL      'FNLV24R'
072200921012     C                   PARM                    KPJBA
072300021203     C*                  MOVEL     KPJBU         PARAM2
072400950313RM*  C*§                   MOVELPARPAD    VIDCPC
072500021203RM*  C*                  MOVE      PARPAD        VIDCPC
072600021203RM*  C*    PARPAD        IFNE      *ZEROS
072700021203RM*  C*                  MOVEL     PARPAD        VIDLNP
072800021203RM*  C*                  END
072900021203     C*                  MOVEL     PARDES        DESCPC
073000021203      *
073100021203      * valorizzo i campi a video
073200021203     C                   movel     kpjbu         fnlv24ds
073300021203     C                   move      d24pdr        vidcpc
073400021203     C                   if        d24pdr   <> *zeros
073500021205     C                   movel     d24pdr        vidlnp
073600021203     c                   endif
073700021203     C                   movel     d24rsc        descpc
073800921012     C                   SETON                                        99
073900950314RM*  C* DECOD.LINEA DI PARTENZA
074000950314RM*  C     VIDLNP        CHAIN     AZORG                              30
074100950314RM*  C  N30              MOVEL     ORGDES        DESLNP
074200950314RM*  C   30              MOVEL     *BLANKS       DESLNP
074300921012     C                   GOTO      ENDSFL
074400941228    2C                   END
074500921012     C**
074600941228     C* ESEGUO TESTN
074700100521     c                   clear                   descpc
074800941228     C                   TESTN                   VIDCPC               30
074900941228     C                   MOVE      VIDCPC        W001A             1
075000941228    2C     *IN30         IFEQ      *OFF
075100941228     C     W001A         ORLT      '0'
075200100521     C                   SETON                                        4299
075300941228     C                   GOTO      ENDSFL
075400941228    2C                   ENDIF
075500941228     C*
075600941228     C                   MOVE      VIDCPC        W0070             7 0
075700950313RM*  C                   MOVEL     VIDLNP        W0070
075800021205     C                   move      w0070         kpdpdr
075900021203     C                   move      'A'           kpdtip
076000021203     C     kfapd         chain     fiapd01l                           30
076100941228    2C     *IN30         IFEQ      *ON
076200100521     C     *IN30         OREQ      *OFF
076300100521     C     APDATB        ANDNE     *BLANKS
076400100521     C                   SETON                                        4299
076500941228     C                   GOTO      ENDSFL
076600941228    2C                   ENDIF
076700941228     C*
076800941228     C                   MOVEL     APDRSC        DESCPC
076900920902     C*
077000920902     C* CODICE PADRONCINO = 0
077100941228   X1C                   ELSE
077200920902     C                   MOVEL     *BLANKS       DESCPC
077300941228    1C                   END
077400920730     C*
077500961025    1C     VIDCLI        IFNE      0
077600930520     C*** NUMERO SERIE
077700060310    2C     VIDNRS        IFNE      '99'
077800060605     c                   clear                   ds3c
077900930521     C                   MOVEL     '3C'          COD
078000930521     C                   MOVEL     *BLANKS       KEY
078100930521     C                   MOVEL     VIDCLI        KEY
078200930521     C     KTAB          CHAIN     TABEL                              30
078300930521     C  N30              MOVEL     TBLUNI        DS3C
078400060605     c  N30              movel     §3cnrs        alfa3cnrs
078500930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
078600970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
078700980805     C     *IN30         IFEQ      *ON
078800980805     C     §3COKD        OREQ      ' '
078900060310    3C     alfa3cnrs     ORNE      VIDNRS
079000930521     C                   SETON                                        5899
079100930521     C                   GOTO      ENDSFL
079200961025    3C                   END
079300961025    2C                   END
079400930521     C*
079500961017     C****  CODICE CLIENTE  ****
079600920730     C                   MOVEL     VIDCLI        KSC
079700920901     C     KACO          CHAIN     CNACO000                           30
079800961025    2C  N30ACOFLG        IFNE      ' '
079900920730     C* CODICE CLIENTE ANNULLATO
080000961017     C                   SETON                                        284599
080100961017     C                   MOVEL     MSG(2)        VIDMSG
080200920730     C                   GOTO      ENDSFL
080300920730     C*
080400961025   X2C                   ELSE
080500920730     C* DECODIFICO CODICE CLIENTE
080600920730     C                   MOVEL     ACORAG        DESCLI
080700961025    2C                   END
080800920730     C*
080900920730     C* CODICE CLIENTE INESISTENTE
081000961025    2C     *IN30         IFEQ      *ON
081100961017     C                   SETON                                        284599
081200961017     C                   MOVEL     MSG(3)        VIDMSG
081300961017     C                   GOTO      ENDSFL
081400961025    2C                   ENDIF
081500920730     C*
081600920730     C* CODICE CLIENTE BLOCCATO
081700961025    2C     ACOABL        IFEQ      '8'
081800971021    2C     ACOABL        OREQ      '*'
081900961017     C                   SETON                                        284599
082000961017     C                   MOVEL     MSG(4)        VIDMSG
082100920730     C                   GOTO      ENDSFL
082200961025    2C                   END
082300961025     C*
082400961025     C**
082500961025    1C                   END
082600920730     C*
082700920903     C*** DATA RITIRO
082800941228    2C     VIDDRT        IFNE      0
082900920903     C* GIRO DATA RITIRO
083000920903     C                   MOVE      VIDDRT        G02DAT
083100920903     C                   MOVEL     *BLANKS       G02ERR
083200941228     C                   CALL      'XSRDA8'
083300920903     C                   PARM                    WLBDAT
083400941228    3C     G02ERR        IFEQ      '1'
083500920903     C                   SETON                                        4799
083600920903     C                   GOTO      ENDSFL
083700941228    3C                   END
083800941228     C                   Z-ADD     G02INV        COMDRT
083900950113     C                   Z-ADD     G02DAT        VIDDRT
084000941228     C*
084100920903     C* DATA RITIRO NON DEVE ESSERE > UDATE
084200941228    3C     COMDRT        IFGT      DATEU
084300920903     C                   SETON                                        4799
084400920903     C                   GOTO      ENDSFL
084500941228    3C                   END
084600941228     C*
084700941228     C                   ELSE
084800941228     C*
084900941228     C                   CLEAR                   COMDRT
085000941228    2C                   END
085100980107     C* Se elaborazione x EDI effettuo realtivi controlli
085200980107     C   02              EXSR      CTREDI
085300920730     C*
085400920730     C     ENDSFL        TAG
085500920730     C   99              Z-ADD     NRR           REC
085600920730     C*
085700920730     C* 36 - SFLNXTCHG
085800920730     C                   SETON                                        36
085900941228     C                   UPDATE    LSA6DSF
086000920730     C                   SETOFF                                       36
086100920730     C*
086200920730     C   99              SETON                                        33
086300941228     C  N99              READC     LSA6DSF                                33
086400920730     C                   END
086500961028     C**
086600961028     C* SE NON E' STATA EFFETTUATA NESSUNA SELEZIONE, PER EDI ERRORE
086700961028    1C  N99*IN02         IFEQ      *ON
086800961028     C     VIDSN         ANDEQ     'S'
086900980108     C     UNO           ANDEQ     *BLANKS
087000980108     C     1             CHAIN     LSA6DSF                            33
087100980108     C                   SETON                                        285699
087200980108     C                   MOVEL     MSG(5)        VIDMSG
087300980108     C  N33              UPDATE    LSA6DSF
087400961028    1C                   ENDIF
087500920730     C*
087600920730     C                   ENDSR
087700920730     C*
087800920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
087900920730     C*
088000920730     C     CONCTL        BEGSR
088100920730     C                   SETOFF                                       999831
088200920730     C*
088300920730     C*** NUMERO FOGLIO ENTRATA OBBLIGATORIO
088400950530     C     *IN01         IFEQ      *OFF
088500950530     C*
088600950530     C     VIDNFV        IFEQ      *BLANKS
088700920730     C     VIDNFV        OREQ      *ZEROS
088800920730     C                   SETON                                        4999
088900920730     C                   GOTO      ENDCTL
089000920730     C                   END
089100920730     C*
089200920730     C* NUMERO FOGLIO ENTRATA >< 0
089300920730     C     VIDNFV        IFNE      *BLANKS
089400920730     C     VIDNFV        ANDNE     *ZEROS
089500920730     C*
089600920730     C* RICERCA
089700941228     C     '?'           SCAN      VIDNFV                                 31
089800920730     C   31              DO
089900941228     C                   Z-ADD     0             DLRNFV
090000941228     C                   Z-ADD     SAVDIN        DLRGDA
090100941228     C                   Z-ADD     31129999      DLRGAL
090200941228     C                   MOVEL     VIDFIL        DLRFGS
090300941228     C                   MOVEL     'S'           DLRRIC
090400941228     C                   MOVEL     DSLR01        KPJBU
090500941228     C                   CALL      'FNLR02R'
090600920730     C                   PARM                    KPJBA
090700941228     C                   MOVEL     KPJBU         DSLR01
090800941228     C                   MOVEL     DLRNFV        VIDNFV
090900920730     C*
091000920730     C                   SETON                                        9899
091100920730     C                   GOTO      ENDCTL
091200920730     C                   END
091300920730     C*  C O N T R O L L I
091400920730     C                   TESTN                   VIDNFV               35
091500920730     C  N35              SETON                                        5099
091600920730     C  N35              GOTO      ENDCTL
091700920730     C*
091800920731     C                   MOVEL     VIDNFV        COMNFV
091900920731     C* CONTROLLO SE NUMERO FOGLIO ENTRATA ESISTE
092000941228     C     KFVV          CHAIN     FNFVV000                           30
092100920731     C     *IN30         IFEQ      '0'
092200920731     C*
092300920731     C     FVVATB        IFNE      ' '
092400920731     C* NUMERO FOGLIO ENTRATA ANNULLATO
092500920731     C                   SETON                                        5199
092600920731     C                   GOTO      ENDCTL
092700920731     C*
092800920731     C                   ELSE
092900920731     C* CONTROLLO SE ESISTONO SPUNTE PER QUEL FOGLIO
093000980409     C     PA3TQU        IFNE      'E'
093100000131     C                   Z-ADD     COMNFV        WNFV
093200070117     C     KBRV          CHAIN     FNBRV000                           34
093300920904     C   34              SETON                                        5299
093400920904     C   34              GOTO      ENDCTL
093500980409     C                   END
093600920904     C*
093700920904     C* DESCRIZIONE FOGLIO ENTRATA
093800941228     C                   MOVEL     FVVDSF        VIDDES
093900920904     C                   END
094000920904     C*
094100920731     C                   ELSE
094200920731     C* 30 ON - NUMERO FOGLIO ENTRATA INESISTENTE
094300920731     C                   SETON                                        5399
094400920731     C                   GOTO      ENDCTL
094500920731     C                   END
094600920901     C*
094700920731     C                   END
094800950530     C                   END
094900920731     C*
095000950313     C*** DATA SPEDIZIONE OBBLIGATORIA
095100920901     C     VIDDSP        IFEQ      0
095200920901     C                   SETON                                        4899
095300920901     C                   GOTO      ENDCTL
095400920901     C*
095500920901     C                   ELSE
095600920903     C                   MOVE      VIDDSP        G02DAT
095700920903     C                   MOVEL     *BLANKS       G02ERR
095800941228     C                   CALL      'XSRDA8'
095900920903     C                   PARM                    WLBDAT
096000920903     C     G02ERR        IFEQ      '1'
096100920903     C                   SETON                                        5499
096200920903     C                   GOTO      ENDCTL
096300920903     C                   END
096400920903     C                   Z-ADD     G02INV        COMDSP
096500950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
096600950113     C                   Z-ADD     G02DAT        VIDDSP
096700920902     C                   END
096800980415     C*** DATA SPEDIZIONE AL
096900980415     C                   CLEAR                   COMDSA
097000980415     C  N01VIDDSA        IFNE      0
097100980415     C                   MOVE      VIDDSA        G02DAT
097200980415     C                   MOVEL     *BLANKS       G02ERR
097300980415     C                   CALL      'XSRDA8'
097400980415     C                   PARM                    WLBDAT
097500980415     C     G02ERR        IFEQ      '1'
097600980415     C                   SETON                                        5799
097700980415     C                   GOTO      ENDCTL
097800980415     C                   END
097900980415     C                   Z-ADD     G02INV        COMDSA
098000980415     C*** DATA SPEDIZIONE AL MAGGIORE DI DATA SPEDIZIONE DAL
098100980415     C     COMDSA        IFLT      COMDSP
098200980415     C                   SETON                                        5799
098300980415     C                   GOTO      ENDCTL
098400980415     C                   END
098500980415     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
098600980415     C                   Z-ADD     G02DAT        VIDDSA
098700980415     C                   END
098800920901     C*
098900920731     C     ENDCTL        ENDSR
099000941228      *-----------------------------------------------------***********
099100941228      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
099200941228      *-----------------------------------------------------***********
099300941228     C     CTRFGS        BEGSR
099400941228     C                   SETOFF                                       90
099500941228     C                   CLEAR                   DSLV50
099600941228     C                   MOVEL     KNMUS         D50PRU
099700941228     C                   MOVEL     VIDFIL        D50FGS
099800941228     C                   CALL      'FNLV50R'
099900941228     C                   PARM                    DSLV50
100000941228     C*
100100941228     C     D50ERR        IFNE      ' '
100200941228     C                   MOVEL     D50MSG        VIDMSG
100300941228     C                   SETON                                        559028
100400941228     C                   GOTO      ENDCTF
100500941228     C                   ENDIF
100600941228     C*
100700941228     C                   EXSR      CARL6
100800941228     C*
100900941228     C     ENDCTF        ENDSR
101000980107      *-----------------------------------------------------***********
101100980107      * CONTROLLI PER SOTTOMISSIONE EDI                        CTREDI
101200980107      *-----------------------------------------------------***********
101300980107     C     CTREDI        BEGSR
101400980107     C*-----------------------------------------------------*
101500980107     C* Se ho selezionato il cliente .....
101600980107    1C     VIDCLI        IFNE      0
101700980107     C* .... è obbligatoria la serie
101800060310    2C     VIDNRS        IFEQ      '99'
101900980107     C                   SETON                                        285999
102000980107     C                   MOVEL     MSG(11)       VIDMSG
102100980107     C                   GOTO      ENDEDI
102200980107    2C                   ENDIF
102300980717      *
102400980717     C* Se ho selezionato il cliente aggancio la tabella PT
102500980107     C                   Z-ADD     1             YY
102600980107     C     VIDCLI        LOOKUP    EDC(YY)                                30
102700980107     C* Se non lo ho trovato vedo se è in tabella 3N
102800020221    2C******     *IN30     IFEQ *OFF
102900020221     C******               Z-ADD1         XX      30
103000020221     C******     VIDCLI    LOKUPT3N,XX                   30
103100020221    3C******     *IN30     IFEQ *ON
103200020221     C******               Z-ADD1         YY
103300020221     C******     K3N,XX    LOKUPEDC,YY                   30
103400020221    3C******               ENDIF
103500020221    2C******               ENDIF
103600980717     C* Se non lo ho trovato vedo se è in tabella CL
103700980717    2C     *IN30         IFEQ      *OFF
103800980717     C                   Z-ADD     1             XX                3 0
103900980717     C     VIDCLI        LOOKUP    TCL(XX)                                30
104000980717    3C     *IN30         IFEQ      *ON
104100980717     C                   Z-ADD     1             YY
104200980717     C     KCL(XX)       LOOKUP    EDC(YY)                                30
104300980717    3C                   ENDIF
104400980717    2C                   ENDIF
104500980717      * Codice cliente non trovato nemmeno utilizzando la CL
104600980107    2C     *IN30         IFEQ      *OFF
104700980107     C                   SETON                                        284599
104800980107     C                   MOVEL     MSG(7)        VIDMSG
104900980107     C                   GOTO      ENDEDI
105000980107    2C                   ENDIF
105100980107     C* Controllo la serie
105200980107    2C     EDS(YY)       IFNE      VIDNRS
105300980107     C                   SETON                                        285999
105400980107     C                   MOVEL     MSG(8)        VIDMSG
105500980107     C                   GOTO      ENDEDI
105600980107    2C                   ENDIF
105700980108     C* Controllo se già selezionata lnp del cliente
105800980108     C     VIDKLP        LOOKUP    LP                                     32
105900980108    2C     *IN32         IFEQ      *ON
106000980108     C                   SETON                                        284499
106100980108     C                   MOVEL     MSG(16)       VIDMSG
106200980108     C                   GOTO      ENDEDI
106300980108    2C                   ENDIF
106400980107     C*-----------------------------------------------------*
106500980108    1C                   ELSE
106600980107     C* .. altrimenti controllo se è stata selezionata la lnp
106700980108    2C     VIDLNP        IFEQ      0
106800980108     C     UNO           IFEQ      *BLANKS
106900980108     C                   SETON                                        285699
107000980107     C                   MOVEL     MSG(12)       VIDMSG
107100980108     C                   END
107200980107     C                   GOTO      ENDEDI
107300980107    2C                   ENDIF
107400980108     C* Lnp non trovata o non appartiene ad un partner
107500980107     C                   Z-ADD     1             YY
107600980107     C     VIDLNP        LOOKUP    EDL(YY)                                30
107700980108    2C     *IN30         IFEQ      *OFF
107800980108     C                   SETON                                        285699
107900980107     C                   MOVEL     MSG(13)       VIDMSG
108000980107     C                   GOTO      ENDEDI
108100980108    2C                   ENDIF
108200980108     C* Controllo se già selezionata lnp
108300980108     C     VIDLNP        LOOKUP    LP                                     32
108400980108    2C     *IN32         IFEQ      *ON
108500980108     C                   SETON                                        285699
108600980108     C                   MOVEL     MSG(14)       VIDMSG
108700980108     C                   GOTO      ENDEDI
108800980108    2C                   ENDIF
108900980108     C* Controllo se già selezionato cliente con quella lnp
109000980108     C     VIDLNP        LOOKUP    CLP                                    32
109100980108    2C     *IN32         IFEQ      *ON
109200980108     C                   SETON                                        285699
109300980108     C                   MOVEL     MSG(15)       VIDMSG
109400980108     C                   GOTO      ENDEDI
109500980108    2C                   ENDIF
109600980107    1C                   ENDIF
109700980107     C*-----------------------------------------------------*
109800980107     c* CHAIN DSPU PER REPERIRE GLI ALTRI DATI
109900980107     C                   MOVEL     'PU'          COD
110000980107     C                   MOVEL     EDK(YY)       EDKEY
110100980107     C     KEDTAB        CHAIN     EDTAB01L                           30
110200980108    1C     *IN30         IFEQ      *ON
110300980107     C     TABFLG        ORNE      ' '
110400980107     C                   SETON                                        284599
110500980107     C                   MOVEL     MSG(9)        VIDMSG
110600980107     C                   GOTO      ENDEDI
110700980108    1C                   ENDIF
110800980107     C**
110900980107     C                   MOVEL     TABUNI        EDSPU
111000980108    1C     §PUDTA        IFNE      'S'
111100980108    2C     VIDCLI        IFEQ      0
111200980108     C                   SETON                                        285699
111300980108     C                   ELSE
111400980107     C                   SETON                                        284599
111500980108     C                   END
111600980107     C                   MOVEL     MSG(10)       VIDMSG
111700980107     C                   GOTO      ENDEDI
111800980108    1C                   ENDIF
111900980108     C*
112000980108     C                   MOVEL     '1'           UNO               1
112100980108     C*
112200980108     C* Se non ci sono errori carico lnp selez. in schiera di comodo
112300980112     C     VIDKLP        IFEQ      0
112400980108     C                   ADD       1             W
112500980108     C                   Z-ADD     VIDLNP        LP(W)
112600980112     C                   END
112700980108     C* se ho il cliente carico lnp cliente in schiera di comodo
112800980108     C     VIDKLP        IFNE      0
112900980112     C***        VIDKLP    ANDNEVIDLNP
113000980108     C                   ADD       1             WW
113100980108     C                   Z-ADD     VIDKLP        CLP(WW)
113200980108     C                   END
113300920731     C*
113400980107     C     ENDEDI        ENDSR
113500980108**
113600950530 CON SPUNTE  ENTRATA
113700950530 CON SPUNTE PARTENZA
113800961028    PER   E. D. I.
113900961017**  MSG
114000961017Immettere linea di partenza valida                                            1
114100961017Codice Cliente annullato                                                      2
114200961017Codice Cliente errato                                                         3
114300961017Codice Cliente bloccato                                                       4
114400980108Se conferma E.D.I. immettere almeno UNA lin.partenza/cod.cliente/serie        5
114500961028IMPOSSIBILE  ESEGUIRE  QUADRATURA  CON   E . D . I . !!!!                     6
114600961028Il codice cliente immesso non e'  E.D.I.                                      7
114700961028La serie non e' quella assegnata al cliente per la conferma con E.D.I.        8
114800961028Impossibile la trasmissione EDI: manca la tabella "PU": TELEFONARE IN SEDE!!  9
114900980108Impossibile trasmissione EDI: il cliente/Partner non ritorna le date di arrivo10
115000961028Se conferma E.D.I. immettere obbligatoriamente il codice cliente e la serie   1
115100980107Se conferma E.D.I. immettere o codice cliente e serie o linea di partenza     2
115200980107La Linea di partenza non esiste su tabella EDI o non appartiene ad un partner 3
115300980108Se conferma E.D.I. impossibile selez. più volte la stessa linea di partenza   4
115400980108Se conferma E.D.I. impossibile indicare lnp relativa ad un cliente già selez. 5
115500980108Se conferma E.D.I. impossibile indicare cliente di una lnp già selezionata    6
115600060310Serie errata                                                                  7
