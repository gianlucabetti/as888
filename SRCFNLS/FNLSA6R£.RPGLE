000100110718     /*END
000200110718     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000300941228     H* FNLSA6R *----------------------------------------------------*
000400920908     H*             QUADRATURA ENTRATA A MAGAZZINO       FILTRO      *
000500000000     H*--------------------------------------------------------------*
000600941228     FFNLSA6D   CF   E             WORKSTN
000700941228     F                                     SFILE(LSA6DSF:NRR)
000800920730     FCNACO00F  IF   E           K DISK
000900021203     FFIAPD01L  IF   E           K DISK
001000941228     FFNFVV01L  IF   E           K DISK
001100070117     FFNBRV01L  IF   E           K DISK
001200920901     FTABEL00F  IF   E           K DISK
001300920901     FAZORG01L  IF   E           K DISK
001400961025     FEDTAB01L  IF   E           K DISK    USROPN
001401140717     Ffnque01L  IF   E           K DISK    USROPN
001500920804     D L1              S              3  0 DIM(30)
001600920901     D L6              S              3  0 DIM(30)
001700961025     D*
001800110712     D EDK             S             35    DIM(999)
001900110712     D EDC             S              7  0 DIM(999)
002000110712     D EDS             S              2    DIM(999)
002100110712     D EDL             S              3  0 DIM(999)
002200110712     D KCL             S              7  0 DIM(999)
002300110712     D TCL             S              7  0 DIM(999)
002400950530     D*
002500961025     D TE2             S             21    DIM(3) CTDATA PERRCD(1)
002600140717     D MSG             S             78    DIM(18) CTDATA PERRCD(1)
002700980108     D*
002800980108     D* SCHIERA DI COMODO X CARICAMENTO LNP
002900980108     D LP              S              3  0 DIM(12)
003000980108     D CLP             S              3  0 DIM(12)
003100950530     D**
003200950530     D* PASSAGGIO PARAMETRI AL BATCH - FNLSA7R -
003300950530     D**
003400900523     D PARAM           DS
003500110718     D  COMNFV                 1      3P 0
003600110715     d  VIDORD                 4      4
003700110715      * ...libero              5      5
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
005800130226     D  VIDmanca             244    244
005900980415     D  COMDSA               246    250P 0
006000980108     D  PARTQU               251    251
006100980108     D  VIDSN                252    252
006200980108     D  VIDSTA               253    253
006300980108     D  VIDFIL               254    256  0
006400950530     D**
006500950530     D* RICERCA CODICE PADRONCINO     -FNLV24R -
006600021203     D FNLV24DS      E DS
006700950530     D**
006800920730     D                 DS
006900961017     D  WCLI                   1      7  0
007000941228     D  VIDDRT                 8     15  0
007100941228     D  RECORD                 1     15  0
007200961017     D                 DS
007300961017     D  VIDKLP                 1      3  0
007400961017     D  VIDKSC                 4      7  0
007500961017     D  VIDCLI                 1      7  0
007600950530     D**
007700950530     D* RICEZIONA PARAMETRI DA KPJBU
007800950530     D**
007900950530     D* TQU: TIPO QUADRATURA: ' ' - CON FOGLIO ENTRATA
008000950530     D*                       'P' - CON SPUNTE PARTENZA ED IMP
008100950530     D PARAM3          DS
008200950530     D  PA3TQU                 1      1
008300950530     D*
008400930125     D                 DS
008500930125     D  COMDAT                 7     12
008600930125     D  COMDF0                 1     25
008700941228     D***
008800941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008900941228     D***
009000941228     D WLBDAT          DS                  INZ
009100941228     D  G02DAT                 1      8  0
009200941228     D  G02INV                 9     16  0
009300941228     D  G02ERR                17     17
009400941228     D  G02TGI                18     22  0
009500941228     D***
009600941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
009700941228     D***
009800941228     D WGIDAT          DS                  INZ
009900941228     D  GIODAT                 1      8  0
010000941228     D  GIOINV                 9     16  0
010100941228     D  GIOTGI                17     21  0
010200110715      *
010300110715     D KPJBA         E DS
010400110715      *
010500110715      * - Parametri per richiamo al pgm di controllo profilo utenti
010600110715     d TIBS34ds      e ds
010700110715      * - Ds di riferimento al file esterno AzUte00F
010800110715     d AZUTEds       e ds                  ExtName(AzUte00F)
010900110715      * - Ds per dati organigramma
011000110715     d dDatiUte      e ds
011100110715      * RICERCA FOGLI VARI
011200110715     D DSLR01        E DS
011300110715     D*
011400110715     D DS3C          E DS
011500110715     D*
011600941228     D* DS PER TRUL06R - CARICAMENTO £X
011700941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011800941228     D  LIN                    1     90  0
011900941228     D                                     DIM(30)
012000941228     D*
012100941228     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
012200941228     D DSLV50        E DS                  EXTNAME(FNLV50DS)
012300961025     D*
012400961025     D* DS PER EDI
012500961025     D EDSPT         E DS                  EXTNAME(EDIDSPT)
012600961025     D EDSPU         E DS                  EXTNAME(EDIDSPU)
012700980717     D EDSCL         E DS                  EXTNAME(EDIDSCL)
012800021203
012900021203     D kpdpdr          S                   LIKE(APDpdr)
013000021203     D kpdtip          S                   LIKE(APDtip)
013100060310     D alfa3cnrs       S              2
013200900702     C****************************************************************
013300900702     C*  RIEPILOGO INDICATORI
013400900702     C***************************************************************
013500950530     C* 01    - ON - QUADRATURA CON SPUNTE PARTENZA 'P'
013600961025     C* 02    - ON - QUADRATURA CON SPUNTE ENTRATA PER E.D.I.
013700950530     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
013800020429     C* 05    - SONO UN SECONDO LIVELLO
013900920804     C* 36    - SFLNXTCHG
014000920804     C* 37    - PULIZIA SUBFILE
014100920522     C* 30/35 - DI COMODO
014200920901     C* 40/57 - ERRORE
014300920804     C* 98/99 - ERRORE GENERICO
014400900702     C*****************************************************************
014500000000     C     *ENTRY        PLIST
014600000000     C                   PARM                    KPJBA
014700950530     C                   MOVEL     KPJBU         PARAM3
014800950530     C*
014900110715     C                   Z-ADD     1             CODUT
015000110715      * Reperimento dati utente/aziendali
015100110715     c                   exsr      sr_DatiJob
015200110715     C                   MOVEL     RSUT          PARDUT           30
015300920901     C*
015400920901     C*---------------------------------------------------------------*
015500920730     C* ACCESSO CNACO
015600920730     C     KACO          KLIST
015700110715     C                   KFLD                    CODUT             1 0
015800110715     C                   KFLD                    dutKCI
015900920901     C                   KFLD                    KSC               7 0
016000920730     C* ACCESSO TABEL
016100920730     C     KTAB          KLIST
016200920730     C                   KFLD                    CODUT
016300920730     C                   KFLD                    COD               2
016400920730     C                   KFLD                    KEY               8
016500961030     C     KTAB2         KLIST
016600961030     C                   KFLD                    CODUT
016700961030     C                   KFLD                    COD               2
016800961025     C     KEDTAB        KLIST
016900961025     C                   KFLD                    COD
017000961025     C                   KFLD                    EDKEY            35
017100920731     C* ACCESSO FLFVV
017200910320     C     KFVV          KLIST
017300911015     C                   KFLD                    COMNPG            1 0
017400911015     C                   KFLD                    COMNFV            5 0
017500941228     C                   KFLD                    VIDFIL
017501140717     C* ACCESSO FNQUE
017502140717     C     KQUE          KLIST
017503140717     C                   KFLD                    COMNPG            1 0
017504140717     C                   KFLD                    wnfv
017505140717     C                   KFLD                    VIDFIL
017506140717     C                   KFLD                    VIDlnp
017600000131     C* ACCESSO FIBRV
017700000131     C     KBRV          KLIST
017800000131     C                   KFLD                    COMNPG            1 0
017900000131     C                   KFLD                    WNFV
018000000131     C                   KFLD                    VIDFIL
018100000131     C*
018200021203     C     Kfapd         klist
018300021203     C                   kfld                    kpdtip
018400021203     C                   kfld                    kpdpdr
018500021203      *
018600000131     C     *LIKE         DEFINE    BRVNFV        WNFV
018700941228     C*---------------------------------------------------------------*
018800920423     C*
018900920423     C* CAMPI DI PASSAGGIO
019000941228     C                   CLEAR                   DSLR01
019100941228     C                   MOVEL     5             DLRNPG
019200941228     C                   MOVEL     'FNLSA6R'     DLRPGM
019300941228     C                   MOVEL     'FNLSA6R'     VIDPGM
019400950530     C*
019500961025     C* 01 ON - QUADRATURA 'P'  SPUNTE PARTENZA
019600961025     C* 02 ON - QUADRATURA 'E'  SPUNTE ENTRATA CON EDI
019700961025     C                   SELECT
019800961025     C     PA3TQU        WHENEQ    'P'
019900950530     C                   SETON                                        01
020000950530     C                   MOVEL     TE2(2)        VIDTE2
020100961025     C     PA3TQU        WHENEQ    'E'
020200961025     C                   SETON                                        02
020300961025     C                   MOVEL     TE2(3)        VIDTE2
020400961025     c*
020500961025     C                   OPEN      EDTAB01L                             97
020600961025     C     *IN97         IFEQ      *ON
020700961030     C                   MOVEL     MSG(6)        VIDMSG
020800961025     C                   SETON                                          28
020900961025     C                   ENDIF
020901140717     C                   OPEN      fnque01l
021000961025     C**
021100961025     C                   OTHER
021200950530     C                   MOVEL     TE2(1)        VIDTE2
021300961028     C                   ENDSL
021400920424     C*
021500941228     C* DATA GIORNO - 1
021600950703     C*
021700941228     C                   TIME                    W0140            14 0
021800941228     C* UDATE IN GGMMAAAA
021900941228     C                   MOVE      W0140         WDTGIO            8 0
022000941228     C*
022100941228     C* UDATE IN AAAAMMGG
022200941228     C                   Z-ADD     WDTGIO        G02DAT
022300941228     C                   MOVEL     *BLANK        G02ERR
022400941228     C                   CALL      'XSRDA8'
022500941228     C                   PARM                    WLBDAT
022600941228     C                   MOVEL     G02INV        DATEU             8 0
022700941228     C* SUB        1
022800941228     C     G02TGI        SUB       1             GIOTGI
022900941228     C*
023000941228     C                   CALL      'XSRGI8'
023100941228     C                   PARM                    WGIDAT
023200920424     C*
023300941228     C                   MOVEL     GIODAT        SAVDIN            8 0
023400920730     C*
023500020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
023600110718     C                   Z-ADD     dutPOU        VIDFIL
023700920831     C*
023800110718     C     dutLPO        IFEQ      '2'
023900110718     C     dutLPO        OREQ      *BLANKS
024000920831     C                   EXSR      CARL6
024100920831     C                   SETON                                        05
024200920831     C                   END
024300920831     C*
024400930125     C* CONTROLLO SE SI TRATTA DI FILIALE CON "CML"
024500930125     C     VIDFIL        CHAIN     AZORG                              30
024600930125     C  N30              MOVEL     ORGDF0        COMDF0
024700930125     C  N30COMDAT        IFNE      *BLANKS
024800930125     C* SI TRATTA DI FILIALE CON "CML"
024900930125     C                   MOVEL     'N'           VIDSTA
025000930125     C                   ELSE
025100930125     C* SI TRATTA DI FILIALE SENZA "CML"
025200930125     C                   MOVEL     'S'           VIDSTA
025300930125     C                   END
025400961025     C*
025500961025     C     *IN02         IFEQ      *ON
025600961025     C                   MOVEL     'N'           VIDSTA
025700961025     C                   ENDIF
025800930125     C*
025900920730     C* CARICO TABELLA FILIALI GESTITE £1
026000920730     C                   EXSR      CARTAB
026100920730     C*
026200920424     C     INIZIO        TAG
026300920730     C* PULIZIA DATI SUBFILE CONTROL
026400920901     C                   SETOFF                                       04
026500920730     C                   Z-ADD     0             VIDDSP
026600980415     C                   Z-ADD     0             VIDDSA
026700920730     C                   MOVEL     *BLANKS       VIDNFV
026800920424     C                   MOVEL     *BLANKS       VIDDES
026900920730     C                   Z-ADD     5             COMNPG
027000920916     C                   MOVEL     'N'           VIDSN
027100120208     C                   MOVEL     'N'           VID9000
027200130226     C                   MOVEL     'N'           VIDmanca
027300110718     c                   eval      VidORD = '1'
027400920423     C*
027500920730     C                   Z-ADD     0             NRR               4 0
027600920730     C* CARICO SUBFILE
027700920831     C                   EXSR      CARICA
027800920730     C*
027900900509     C     FOR01         TAG
028000941228     C                   WRITE     LSA6D01
028100920730     C     FORCT         TAG
028200941228     C                   EXFMT     LSA6DCT
028300941228     C*
028400961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
028500961017     C                   CLEAR                   VIDMSG
028600941228     C                   SETOFF                                       289055
028700961025     C                   SETOFF                                       444556
028800961028     C                   SETOFF                                       59
028900920730     C*
029000910320     C** CMD3 - FINE LAVORO
029100961025     C**        O ERRORE: NON TROVATO FILE EDTAB01L
029200961025     C   97
029300961025     COR KC              GOTO      FINE
029400920831     C*
029500920831     C** CMD18- CAMBIO FILIALE
029600920831     C   KS              SETON                                        04
029700920831     C   KS              GOTO      FORCT
029800920831     C*
029900941228     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
030000941228     C   04              EXSR      CTRFGS
030100941228     C   04
030200941228     CAN 90              GOTO      FOR01
030300920730     C*
030400920730     C* CONTROLLO DATI SUBFILE
030500920730     C                   EXSR      CONSFL
030600920831     C   98              GOTO      FOR01
030700920831     C*
030800920730     C   99              GOTO      FORCT
030900920730     C*
031000920730     C* CONTROLLO DATI SUBFILE CONTROL
031100920730     C                   EXSR      CONCTL
031200920730     C   98              GOTO      FOR01
031300920730     C*
031400950113     C  NKF
031500920831     COR 99              GOTO      FORCT
031600920730     C*
031700920807     C* CARICO SCHIERE PER PASSAGGIO
031800920807     C                   MOVEA     *ZEROS        LNP
031900920807     C                   MOVEA     *ZEROS        CPC
032000920807     C                   MOVEA     *ZEROS        CLI
032100920807     C                   MOVEA     *ZEROS        DRT
032200930520     C                   MOVEA     *ZEROS        NRS
032300950323     C                   MOVEA     *BLANKS       MOP
032400950323     C                   MOVEA     *BLANKS       NRT
032500920901     C                   Z-ADD     1             X                 3 0
032600920807     C                   Z-ADD     1             NRR
032700920807     C*
032800950323     C     NRR           DOWLE     12
032900920807     C*
033000941228     C     NRR           CHAIN     LSA6DSF                            31
033100920831     C     *IN31         IFEQ      '0'
033200920831     C     VIDLNP        ANDNE     0
033300920807     C                   MOVEL     VIDLNP        LNP(X)
033400920914     C     VIDCPC        IFGT      *ZEROS
033500950313RM*  C                   MOVEL     VIDLNP        CPC(X)
033600950313RM*  C                   MOVE      VIDCPC        CPC(X)
033700920914     C                   END
033800930520     C                   MOVEL     VIDNRS        NRS(X)
033900920807     C                   MOVEL     VIDCLI        CLI(X)
034000941228     C                   Z-ADD     COMDRT        DRT(X)
034100950323     C                   MOVEL     VIDMOP        MOP(X)
034200950323     C                   MOVEL     VIDNRT        NRT(X)
034300920901     C                   ADD       1             X
034400920807     C                   END
034500920807     C*
034600920807     C                   ADD       1             NRR
034700920807     C                   END
034800961025     c* IMPOSTO DATI PER EDI
034900961025     C     *IN02         IFEQ      *ON
035000961025     C                   MOVEL     'S'           EDSEL
035100961025     C                   ENDIF
035200920831     C*
035300980415     C  N01COMDSA        IFEQ      0
035400980415     C                   Z-ADD     COMDSP        COMDSA
035500980415     C                   END
035600980415     C*
035700950530     C                   MOVEL     PA3TQU        PARTQU
035800900702     C                   MOVEL     PARAM         KPJBU
035900941228     C                   MOVEL     'LSA7'        KCOAZ
036000110718     c                   if        knmus = *all'1'
036100110718     c                   call      'FNLSA7C'
036200110718     c                   parm                    KPJBA
036300110718     c                   else
036400070109     C                   CALL      'BCH10'
036500900702     C                   PARM                    KPJBA
036600110718     c                   endif
036700920522     C*
036800950411     C                   GOTO      INIZIO
036900000000     C     FINE          TAG
037000000000     C                   SETON                                        LR
037100920730     C*
037200920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
037300920730     C     CARTAB        BEGSR
037400941228     C***
037500941228     C* CARICO TABELLA FILIALI GESTITE £1
037600941228     C***
037700941228     C                   CLEAR                   DSUL06
037800941228     C                   MOVE      '£1'          D06COD
037900941228     C                   MOVEL     SIMFEL        D06KEY
038000941228     C                   MOVEL     DSUL06        KPJBU
038100941228     C*
038200950323     C                   CALL      'TRUL06R'
038300941228     C                   PARM                    KPJBA
038400941228     C                   MOVEL     KPJBU         DSUL06
038500941228     C                   MOVEA     LIN           L1
038600961025     C**
038700961025     C* CARICO TABELLA 'PT' DA EDITAB01L
038800961025     C**
038900980717     C     *IN02         IFEQ      *ON
039000980717     C     *IN97         ANDEQ     *OFF
039100961025     C                   MOVEL     'PT'          COD
039200961025     C                   Z-ADD     1             YY                3 0
039300961025     C     COD           SETLL     EDTAB01L
039400961025     C     COD           READE     EDTAB01L                               30
039500961025     C     *IN30         DOWEQ     *OFF
039600961025     C     TABFLG        IFEQ      ' '
039700020917     C                   MOVEL     TABUNI        EDSPT
039800020917      * solo se compresa nelle linee di partenza del terminal
039900130403     c***  §ptlnp        lookup    L1                                     32
040000130403     c***                if        *in32 = *on
040100961025     C                   MOVEL     TABKEY        EDK(YY)
040200961025     C                   MOVEL     §PTKSC        EDC(YY)
040300961025     C                   MOVEL     §PTNRS        EDS(YY)
040400980107     C                   MOVEL     §PTLNP        EDL(YY)
040500961025     C                   ADD       1             YY
040600130403     c***                end
040700961025     C                   ENDIF
040800961025     C*
040900961025     C     COD           READE     EDTAB01L                               30
041000961025     C                   ENDDO
041100980107     C**
041200980107     C** Scorro tabella PU e se il codice corrispondente
041300980107     C** non è un partner ma un cliente azzero lnp
041400980108     C                   DO        YY            YW                4 0
041500980107     C                   MOVEL     'PU'          COD
041600980108     C                   MOVEL     EDK(YW)       EDKEY
041700980107     C     KEDTAB        CHAIN     EDTAB01L                           30
041800980108    3C     *IN30         IFEQ      *OFF
041900980108     C     TABFLG        ANDEQ     ' '
042000980107     C                   MOVEL     TABUNI        EDSPU
042100980107     C     §PUTPC        IFNE      'P'
042200980108     C                   Z-ADD     0             EDL(YW)
042300980107     C                   ENDIF
042400980107     C                   ELSE
042500980108     C                   Z-ADD     0             EDL(YW)
042600980107     C                   ENDIF
042700980107     C                   ENDDO
042800980717     C**
042900980717     C* CARICO TABELLA 'CL' DA EDITAB01L
043000980717     C**
043100980717     C                   MOVEL     'CL'          COD
043200980717     C                   Z-ADD     1             YY                3 0
043300980717     C     COD           SETLL     EDTAB01L
043400980717     C     COD           READE     EDTAB01L                               30
043500980717     C     *IN30         DOWEQ     *OFF
043600980717     C     TABFLG        IFEQ      ' '
043700980717     C                   MOVEL     TABKEY        KCL(YY)
043800980717     C                   MOVEL     TABUNI        EDSCL
043900980717     C                   MOVEL     §CLKSC        TCL(YY)
044000980717     C                   ADD       1             YY
044100980717     C                   ENDIF
044200980717     C*
044300980717     C     COD           READE     EDTAB01L                               30
044400980717     C                   ENDDO
044500961030     C*
044600961030     C                   ENDIF
044700920730     C                   ENDSR
044800920730     C*
044900920831     C* CARICO L6 ------------------------------------------***********
045000920831     C*
045100920831     C     CARL6         BEGSR
045200941228     C*
045300941228     C                   CLEAR                   DSUL06
045400941228     C                   MOVE      '£6'          D06COD
045500941228     C                   MOVEL     VIDFIL        D06KEY
045600941228     C                   MOVEL     DSUL06        KPJBU
045700941228     C                   CALL      'TRUL06R'
045800941228     C                   PARM                    KPJBA
045900941228     C                   MOVEL     KPJBU         DSUL06
046000941228     C                   MOVEA     LIN           L6
046100920831     C                   ENDSR
046200920831     C*
046300920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
046400920730     C*
046500920831     C     CARICA        BEGSR
046600920831     C* PULIZIA SUBFILE
046700920804     C                   SETON                                        37
046800941228     C                   WRITE     LSA6DCT
046900920804     C                   SETOFF                                       37
047000920730     C*
047100920831     C* CARICO DATI IN SUBFILE
047200920730     C*
047300920730     C                   Z-ADD     0             VIDLNP
047400920730     C                   MOVEL     *BLANKS       DESLNP
047500920914     C                   MOVEL     *BLANKS       VIDCPC
047600920730     C                   MOVEL     *BLANKS       DESCPC
047700060310     C                   movel     99            VIDNRS
047800920730     C                   Z-ADD     0             VIDCLI
047900920730     C                   MOVEL     *BLANKS       DESCLI
048000920730     C                   Z-ADD     0             VIDDRT
048100941228     C                   Z-ADD     0             COMDRT
048200950323     C                   MOVE      *BLANKS       VIDMOP
048300950323     C                   MOVE      *BLANKS       VIDNRT
048400920730     C*
048500950323     C                   DO        12            NRR
048600941228     C                   WRITE     LSA6DSF
048700941228     C                   ENDDO
048800920730     C*
048900920831     C                   Z-ADD     1             REC
049000920730     C                   ENDSR
049100920730     C*
049200920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
049300920730     C*
049400920730     C     CONSFL        BEGSR
049500920831     C                   SETOFF                                       9899
049600920730     C                   Z-ADD     1             NRR
049700980108     C                   CLEAR                   UNO
049800980108     C                   Z-ADD     0             LP
049900980108     C                   Z-ADD     0             CLP
050000980108     C                   Z-ADD     0             W                 4 0
050100980108     C                   Z-ADD     0             WW                4 0
050200920730     C*
050300941228     C                   READC     LSA6DSF                                33
050400920730     C     *IN33         DOWEQ     '0'
050500961028     C* PER EDI CON ELABORAZIONE DEFINITIVA, POSSO METTERE SOLTANTO
050600961025     C* 1 RICHIESTA E SOLO IL CODICE CLIENTE
050700930520     C*
050800961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
050900961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
051000961017    2C     VIDKLP        IFGT      0
051100961017     C     VIDKLP        CHAIN     AZORG01L                           34
051200961017    3C     *IN34         IFEQ      *ON
051300961017     C                   SETON                                        284499
051400961017     C                   MOVEL     MSG(1)        VIDMSG
051500961017     C                   GOTO      ENDSFL
051600961017    3C                   ENDIF
051700961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
051800961017    3C     VIDKSC        IFEQ      0
051900961017     C     DESCLI        ANDEQ     *BLANKS
052000961017     C                   SETON                                        284599
052100961017     C                   MOVEL     MSG(3)        VIDMSG
052200961017     C                   GOTO      ENDSFL
052300961017    3C                   ENDIF
052400961017     C*
052500961017   X2C                   ELSE
052600961017    3C     VIDKSC        IFGT      0
052700961017     C                   SETON                                        284499
052800961017     C                   MOVEL     MSG(1)        VIDMSG
052900961017     C                   GOTO      ENDSFL
053000961017    3C                   ENDIF
053100961017    2C                   ENDIF
053200961017     C*
053300920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
053400920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
053500961017     C     VIDKSC        IFEQ      0
053600920730     C     DESCLI        ANDNE     *BLANKS
053700981109     C                   CLEAR                   PARDIT
053800920730     C*
053900920730     C* PARSTA = 9  ESCLUDO ANNULLATI
054000110715     C                   Z-ADD     dutKCI        PARKCC            4 0
054100920730     C                   Z-ADD     9             PARSTA
054200920902     C                   MOVEL     *BLANKS       DES              48
054300920902     C                   MOVEL     DESCLI        DES
054400961017     C                   MOVEL     VIDKLP        PARFLR
054500981109     C     VIDKLP        CHAIN     AZORG01L                           31
054600981109     C  N31              MOVEL     ORGDIT        PARDIT
054700000907     C                   Z-ADD     1             PAXNUM
054800000907     C                   CALL      'XALFA3BR'
054900920730     C                   PARM                    PARDUT
055000920730     C                   PARM                    CODUT
055100920902     C                   PARM                    DES
055200961016     C                   PARM                    PARKCC
055300920730     C                   PARM                    PARSTA            1 0
055400961016     C                   PARM                    PARFLR           90
055500981109     C                   PARM                    PARDIT            3
055600000907     C                   PARM                    PAXNUM            2 0
055700000907     C                   PARM                    PAXKCM           80
055800000907     C                   PARM                    PAXKSM          140
055900000907     C                   PARM                    PAXKDM           60
056000920831     C     PARSTA        IFNE      -1
056100000907     C                   MOVEL     PAXKSM        VIDCLI
056200920902     C                   MOVEL     DES           DESCLI
056300920831     C                   END
056400920831     C*
056500920831     C                   SETON                                        9899
056600920730     C                   GOTO      ENDSFL
056700920730     C                   END
056800920730     C*
056900920901     C*** LINEA DI PARTENZA
057000961025    1C     VIDLNP        IFNE      0
057100920901     C*
057200920901     C* CONTROLLO SE LINEA VALIDA
057300920901     C     VIDLNP        LOOKUP    L1                                     31
057400061212     c
057500061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
057600061212     c*  per poterla accettare
057700061212     c                   if        not *in31
057800061212     c                   if        *in01    and (vidnrs='00' or vidcli=0
057900061212     c                             or vidnrs='SI' or vidnrs='99')
058000061212     c                             or not *in01
058100061212     C                   SETON                                        4099
058200061212     C                   GOTO      ENDSFL
058300061212     c                   endif
058400070109     c
058500070109     c* Se lnp non in £1 impossibile richiedere anomalie manca bolla
058600070109     c*  di selezioni non richieste
058700070109     c                   if        vidsta='S'
058800070109     C                   SETON                                        6099
058900070109     C                   GOTO      ENDSFL
059000070109     c                   endif
059100061212     c                   endif
059200920901     C*
059300920901     C* DECODIFICO LINEA DI PARTENZA
059400920901     C     VIDLNP        CHAIN     AZORG                              30
059500920901     C  N30              MOVEL     ORGDES        DESLNP
059600920901     C   30              MOVEL     *BLANKS       DESLNP
059700061212     c
059800061212     c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
059900061212     c                             or orgfva<>' '
060000061212     C                   SETON                                        4099
060100061212     C                   GOTO      ENDSFL
060200061212     c                   endif
060300920901     C*
060400961025   X1C                   ELSE
060500920902     C* LINEA DI PARTENZA = 0
060600920902     C                   MOVEL     *BLANKS       DESLNP
060700961025     C                   MOVEL     VIDCLI        WCLI
060800961025     C*
060900920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
061000950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
061100920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
061200961028    2C     RECORD        IFNE      *ZEROS
061300100823     C     VIDCPC        ORne      *ZEROS
061400100823     C     VIDCPC        andne     *blanks
061500060310     C     VIDNRS        ORNE      '99'
061600950323     C     VIDMOP        ORNE      *BLANKS
061700950323     C     VIDNRT        ORNE      *BLANKS
061800961025     C* ERRORE: NON INSERIBILE
061900961028    3C     *IN02         IFEQ      *ON
062000961025     C     VIDSN         ANDEQ     'S'
062100980805   X C                   ELSE
062200961025     C* ERRORE : IMMETTERE ANCHE LA LINEA PARTENZA
062300920901     C                   SETON                                        4199
062400920901     C                   GOTO      ENDSFL
062500961028    3C                   END
062600961025    2C                   END
062700961025     C**
062800961025    1C                   END
062900060310     C*** Numero serie
063000060310     C* ESEGUO TESTN
063100060310     c                   if        vidnrs<>'SI'
063200060310     C                   TESTN                   VIDnrs               30
063300060310     C                   MOVE      VIDnrs        W001A             1
063400060310    2C     *IN30         IFEQ      *OFF
063500060310     C     W001A         ORLT      '0'
063600060310     C                   SETON                                        599928
063700060310     C                   MOVEL     MSG(17)       VIDMSG
063800060310     C                   GOTO      ENDSFL
063900060310    2C                   ENDIF
064000060310    2C                   ENDIF
064100920901     C*
064200920730     C*** CODICE PADRONCINO
064300941228    1C     VIDCPC        IFNE      *BLANKS
064400921012     C     VIDCPC        ANDNE     *ZEROS
064500941228     C*
064600941228     C     '?'           SCAN      VIDCPC                                 98
064700941228     C* RICERCA
064800941228    2C     *IN98         IFEQ      '1'
064900021203      *
065000021203      * imposto i campi della ds esterna fnlv24ds
065100021203     C                   clear                   fnlv24ds
065200021203     C                   eval      d24fil   = vidlnp
065300021203     C                   eval      d24tip   = 'A'
065400021203     C                   eval      d24flg   = 'R'
065500021203     C                   movel     fnlv24ds      kpjbu
065600021203
065700941228     C                   CALL      'FNLV24R'
065800921012     C                   PARM                    KPJBA
065900021203      *
066000021203      * valorizzo i campi a video
066100021203     C                   movel     kpjbu         fnlv24ds
066200021203     C                   move      d24pdr        vidcpc
066300021203     C                   if        d24pdr   <> *zeros
066400021205     C                   movel     d24pdr        vidlnp
066500021203     c                   endif
066600021203     C                   movel     d24rsc        descpc
066700921012     C                   SETON                                        99
066800950314RM*  C* DECOD.LINEA DI PARTENZA
066900950314RM*  C     VIDLNP        CHAIN     AZORG                              30
067000950314RM*  C  N30              MOVEL     ORGDES        DESLNP
067100950314RM*  C   30              MOVEL     *BLANKS       DESLNP
067200921012     C                   GOTO      ENDSFL
067300941228    2C                   END
067400921012     C**
067500941228     C* ESEGUO TESTN
067600100521     c                   clear                   descpc
067700941228     C                   TESTN                   VIDCPC               30
067800941228     C                   MOVE      VIDCPC        W001A             1
067900941228    2C     *IN30         IFEQ      *OFF
068000941228     C     W001A         ORLT      '0'
068100100521     C                   SETON                                        4299
068200941228     C                   GOTO      ENDSFL
068300941228    2C                   ENDIF
068400941228     C*
068500941228     C                   MOVE      VIDCPC        W0070             7 0
068600950313RM*  C                   MOVEL     VIDLNP        W0070
068700021205     C                   move      w0070         kpdpdr
068800021203     C                   move      'A'           kpdtip
068900021203     C     kfapd         chain     fiapd01l                           30
069000941228    2C     *IN30         IFEQ      *ON
069100100521     C     *IN30         OREQ      *OFF
069200100521     C     APDATB        ANDNE     *BLANKS
069300100521     C                   SETON                                        4299
069400941228     C                   GOTO      ENDSFL
069500941228    2C                   ENDIF
069600941228     C*
069700941228     C                   MOVEL     APDRSC        DESCPC
069800920902     C*
069900920902     C* CODICE PADRONCINO = 0
070000941228   X1C                   ELSE
070100920902     C                   MOVEL     *BLANKS       DESCPC
070200941228    1C                   END
070300920730     C*
070400961025    1C     VIDCLI        IFNE      0
070500930520     C*** NUMERO SERIE
070600060310    2C     VIDNRS        IFNE      '99'
070700060605     c                   clear                   ds3c
070800930521     C                   MOVEL     '3C'          COD
070900930521     C                   MOVEL     *BLANKS       KEY
071000930521     C                   MOVEL     VIDCLI        KEY
071100930521     C     KTAB          CHAIN     TABEL                              30
071200930521     C  N30              MOVEL     TBLUNI        DS3C
071300060605     c  N30              movel     §3cnrs        alfa3cnrs
071400930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
071500970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
071600980805     C     *IN30         IFEQ      *ON
071700980805     C     §3COKD        OREQ      ' '
071800060310    3C     alfa3cnrs     ORNE      VIDNRS
071900930521     C                   SETON                                        5899
072000930521     C                   GOTO      ENDSFL
072100961025    3C                   END
072200961025    2C                   END
072300930521     C*
072400961017     C****  CODICE CLIENTE  ****
072500920730     C                   MOVEL     VIDCLI        KSC
072600920901     C     KACO          CHAIN     CNACO000                           30
072700961025    2C  N30ACOFLG        IFNE      ' '
072800920730     C* CODICE CLIENTE ANNULLATO
072900961017     C                   SETON                                        284599
073000961017     C                   MOVEL     MSG(2)        VIDMSG
073100920730     C                   GOTO      ENDSFL
073200920730     C*
073300961025   X2C                   ELSE
073400920730     C* DECODIFICO CODICE CLIENTE
073500920730     C                   MOVEL     ACORAG        DESCLI
073600961025    2C                   END
073700920730     C*
073800920730     C* CODICE CLIENTE INESISTENTE
073900961025    2C     *IN30         IFEQ      *ON
074000961017     C                   SETON                                        284599
074100961017     C                   MOVEL     MSG(3)        VIDMSG
074200961017     C                   GOTO      ENDSFL
074300961025    2C                   ENDIF
074400920730     C*
074500920730     C* CODICE CLIENTE BLOCCATO
074600130315    2C**   ACOABL        IFEQ      '8'
074700130315    2C**   ACOABL        OREQ      '*'
074800130315    2C     ACOABL        ifne      *blanks
074900961017     C                   SETON                                        284599
075000961017     C                   MOVEL     MSG(4)        VIDMSG
075100920730     C                   GOTO      ENDSFL
075200961025    2C                   END
075300961025     C*
075400961025     C**
075500961025    1C                   END
075600920730     C*
075700920903     C*** DATA RITIRO
075800941228    2C     VIDDRT        IFNE      0
075900920903     C* GIRO DATA RITIRO
076000920903     C                   MOVE      VIDDRT        G02DAT
076100920903     C                   MOVEL     *BLANKS       G02ERR
076200941228     C                   CALL      'XSRDA8'
076300920903     C                   PARM                    WLBDAT
076400941228    3C     G02ERR        IFEQ      '1'
076500920903     C                   SETON                                        4799
076600920903     C                   GOTO      ENDSFL
076700941228    3C                   END
076800941228     C                   Z-ADD     G02INV        COMDRT
076900950113     C                   Z-ADD     G02DAT        VIDDRT
077000941228     C*
077100920903     C* DATA RITIRO NON DEVE ESSERE > UDATE
077200941228    3C     COMDRT        IFGT      DATEU
077300920903     C                   SETON                                        4799
077400920903     C                   GOTO      ENDSFL
077500941228    3C                   END
077600941228     C*
077700941228     C                   ELSE
077800941228     C*
077900941228     C                   CLEAR                   COMDRT
078000941228    2C                   END
078100980107     C* Se elaborazione x EDI effettuo realtivi controlli
078200980107     C   02              EXSR      CTREDI
078300920730     C*
078400920730     C     ENDSFL        TAG
078500920730     C   99              Z-ADD     NRR           REC
078600920730     C*
078700920730     C* 36 - SFLNXTCHG
078800920730     C                   SETON                                        36
078900941228     C                   UPDATE    LSA6DSF
079000920730     C                   SETOFF                                       36
079100920730     C*
079200920730     C   99              SETON                                        33
079300941228     C  N99              READC     LSA6DSF                                33
079400920730     C                   END
079500961028     C**
079600961028     C* SE NON E' STATA EFFETTUATA NESSUNA SELEZIONE, PER EDI ERRORE
079700961028    1C  N99*IN02         IFEQ      *ON
079800961028     C     VIDSN         ANDEQ     'S'
079900980108     C     UNO           ANDEQ     *BLANKS
080000980108     C     1             CHAIN     LSA6DSF                            33
080100980108     C                   SETON                                        285699
080200980108     C                   MOVEL     MSG(5)        VIDMSG
080300980108     C  N33              UPDATE    LSA6DSF
080400961028    1C                   ENDIF
080500920730     C*
080600920730     C                   ENDSR
080700920730     C*
080800920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
080900920730     C*
081000920730     C     CONCTL        BEGSR
081100920730     C                   SETOFF                                       999831
081200920730     C*
081300920730     C*** NUMERO FOGLIO ENTRATA OBBLIGATORIO
081400950530     C     *IN01         IFEQ      *OFF
081500950530     C*
081600950530     C     VIDNFV        IFEQ      *BLANKS
081700920730     C     VIDNFV        OREQ      *ZEROS
081800920730     C                   SETON                                        4999
081900920730     C                   GOTO      ENDCTL
082000920730     C                   END
082100920730     C*
082200920730     C* NUMERO FOGLIO ENTRATA >< 0
082300920730     C     VIDNFV        IFNE      *BLANKS
082400920730     C     VIDNFV        ANDNE     *ZEROS
082500920730     C*
082600920730     C* RICERCA
082700941228     C     '?'           SCAN      VIDNFV                                 31
082800920730     C   31              DO
082900941228     C                   Z-ADD     0             DLRNFV
083000941228     C                   Z-ADD     SAVDIN        DLRGDA
083100941228     C                   Z-ADD     31129999      DLRGAL
083200941228     C                   MOVEL     VIDFIL        DLRFGS
083300941228     C                   MOVEL     'S'           DLRRIC
083400941228     C                   MOVEL     DSLR01        KPJBU
083500941228     C                   CALL      'FNLR02R'
083600920730     C                   PARM                    KPJBA
083700941228     C                   MOVEL     KPJBU         DSLR01
083800941228     C                   MOVEL     DLRNFV        VIDNFV
083900920730     C*
084000920730     C                   SETON                                        9899
084100920730     C                   GOTO      ENDCTL
084200920730     C                   END
084300920730     C*  C O N T R O L L I
084400920730     C                   TESTN                   VIDNFV               35
084500920730     C  N35              SETON                                        5099
084600920730     C  N35              GOTO      ENDCTL
084700920730     C*
084800920731     C                   MOVEL     VIDNFV        COMNFV
084900920731     C* CONTROLLO SE NUMERO FOGLIO ENTRATA ESISTE
085000941228     C     KFVV          CHAIN     FNFVV000                           30
085100920731     C     *IN30         IFEQ      '0'
085200920731     C*
085300920731     C     FVVATB        IFNE      ' '
085400920731     C* NUMERO FOGLIO ENTRATA ANNULLATO
085500920731     C                   SETON                                        5199
085600920731     C                   GOTO      ENDCTL
085700920731     C*
085800920731     C                   ELSE
085900920731     C* CONTROLLO SE ESISTONO SPUNTE PER QUEL FOGLIO
086000980409     C     PA3TQU        IFNE      'E'
086100000131     C                   Z-ADD     COMNFV        WNFV
086200070117     C     KBRV          CHAIN     FNBRV000                           34
086300920904     C   34              SETON                                        5299
086400920904     C   34              GOTO      ENDCTL
086500980409     C                   END
086600920904     C*
086700920904     C* DESCRIZIONE FOGLIO ENTRATA
086800941228     C                   MOVEL     FVVDSF        VIDDES
086900920904     C                   END
087000920904     C*
087100920731     C                   ELSE
087200920731     C* 30 ON - NUMERO FOGLIO ENTRATA INESISTENTE
087300920731     C                   SETON                                        5399
087400920731     C                   GOTO      ENDCTL
087500920731     C                   END
087600920901     C*
087700920731     C                   END
087800950530     C                   END
087900920731     C*
088000950313     C*** DATA SPEDIZIONE OBBLIGATORIA
088100920901     C     VIDDSP        IFEQ      0
088200920901     C                   SETON                                        4899
088300920901     C                   GOTO      ENDCTL
088400920901     C*
088500920901     C                   ELSE
088600920903     C                   MOVE      VIDDSP        G02DAT
088700920903     C                   MOVEL     *BLANKS       G02ERR
088800941228     C                   CALL      'XSRDA8'
088900920903     C                   PARM                    WLBDAT
089000920903     C     G02ERR        IFEQ      '1'
089100920903     C                   SETON                                        5499
089200920903     C                   GOTO      ENDCTL
089300920903     C                   END
089400920903     C                   Z-ADD     G02INV        COMDSP
089500950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
089600950113     C                   Z-ADD     G02DAT        VIDDSP
089700920902     C                   END
089800980415     C*** DATA SPEDIZIONE AL
089900980415     C                   CLEAR                   COMDSA
090000980415     C  N01VIDDSA        IFNE      0
090100980415     C                   MOVE      VIDDSA        G02DAT
090200980415     C                   MOVEL     *BLANKS       G02ERR
090300980415     C                   CALL      'XSRDA8'
090400980415     C                   PARM                    WLBDAT
090500980415     C     G02ERR        IFEQ      '1'
090600980415     C                   SETON                                        5799
090700980415     C                   GOTO      ENDCTL
090800980415     C                   END
090900980415     C                   Z-ADD     G02INV        COMDSA
091000980415     C*** DATA SPEDIZIONE AL MAGGIORE DI DATA SPEDIZIONE DAL
091100980415     C     COMDSA        IFLT      COMDSP
091200980415     C                   SETON                                        5799
091300980415     C                   GOTO      ENDCTL
091400980415     C                   END
091500980415     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
091600980415     C                   Z-ADD     G02DAT        VIDDSA
091700980415     C                   END
091800920901     C*
091900920731     C     ENDCTL        ENDSR
092000941228      *-----------------------------------------------------***********
092100941228      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
092200941228      *-----------------------------------------------------***********
092300941228     C     CTRFGS        BEGSR
092400941228     C                   SETOFF                                       90
092500941228     C                   CLEAR                   DSLV50
092600941228     C                   MOVEL     KNMUS         D50PRU
092700941228     C                   MOVEL     VIDFIL        D50FGS
092800941228     C                   CALL      'FNLV50R'
092900941228     C                   PARM                    DSLV50
093000941228     C*
093100941228     C     D50ERR        IFNE      ' '
093200941228     C                   MOVEL     D50MSG        VIDMSG
093300941228     C                   SETON                                        559028
093400941228     C                   GOTO      ENDCTF
093500941228     C                   ENDIF
093600941228     C*
093700941228     C                   EXSR      CARL6
093800941228     C*
093900941228     C     ENDCTF        ENDSR
094000980107      *-----------------------------------------------------***********
094100980107      * CONTROLLI PER SOTTOMISSIONE EDI                        CTREDI
094200980107      *-----------------------------------------------------***********
094300980107     C     CTREDI        BEGSR
094400980107     C*-----------------------------------------------------*
094500980107     C* Se ho selezionato il cliente .....
094600980107    1C     VIDCLI        IFNE      0
094700980107     C* .... è obbligatoria la serie
094800060310    2C     VIDNRS        IFEQ      '99'
094900980107     C                   SETON                                        285999
095000980107     C                   MOVEL     MSG(11)       VIDMSG
095100980107     C                   GOTO      ENDEDI
095200980107    2C                   ENDIF
095300980717      *
095400980717     C* Se ho selezionato il cliente aggancio la tabella PT
095500980107     C                   Z-ADD     1             YY
095600980107     C     VIDCLI        LOOKUP    EDC(YY)                                30
095700980717     C* Se non lo ho trovato vedo se è in tabella CL
095800980717    2C     *IN30         IFEQ      *OFF
095900980717     C                   Z-ADD     1             XX                3 0
096000980717     C     VIDCLI        LOOKUP    TCL(XX)                                30
096100980717    3C     *IN30         IFEQ      *ON
096200980717     C                   Z-ADD     1             YY
096300980717     C     KCL(XX)       LOOKUP    EDC(YY)                                30
096400980717    3C                   ENDIF
096500980717    2C                   ENDIF
096600980717      * Codice cliente non trovato nemmeno utilizzando la CL
096700980107    2C     *IN30         IFEQ      *OFF
096800980107     C                   SETON                                        284599
096900980107     C                   MOVEL     MSG(7)        VIDMSG
097000980107     C                   GOTO      ENDEDI
097100980107    2C                   ENDIF
097200980107     C* Controllo la serie
097300980107    2C     EDS(YY)       IFNE      VIDNRS
097400980107     C                   SETON                                        285999
097500980107     C                   MOVEL     MSG(8)        VIDMSG
097600980107     C                   GOTO      ENDEDI
097700980107    2C                   ENDIF
097800980108     C* Controllo se già selezionata lnp del cliente
097900980108     C     VIDKLP        LOOKUP    LP                                     32
098000980108    2C     *IN32         IFEQ      *ON
098100980108     C                   SETON                                        284499
098200980108     C                   MOVEL     MSG(16)       VIDMSG
098300980108     C                   GOTO      ENDEDI
098400980108    2C                   ENDIF
098500980107     C*-----------------------------------------------------*
098600980108    1C                   ELSE
098700980107     C* .. altrimenti controllo se è stata selezionata la lnp
098800980108    2C     VIDLNP        IFEQ      0
098900980108     C     UNO           IFEQ      *BLANKS
099000980108     C                   SETON                                        285699
099100980107     C                   MOVEL     MSG(12)       VIDMSG
099200980108     C                   END
099300980107     C                   GOTO      ENDEDI
099400980107    2C                   ENDIF
099500980108     C* Lnp non trovata o non appartiene ad un partner
099600980107     C                   Z-ADD     1             YY
099700980107     C     VIDLNP        LOOKUP    EDL(YY)                                30
099800980108    2C     *IN30         IFEQ      *OFF
099900980108     C                   SETON                                        285699
100000980107     C                   MOVEL     MSG(13)       VIDMSG
100100980107     C                   GOTO      ENDEDI
100200980108    2C                   ENDIF
100300980108     C* Controllo se già selezionata lnp
100400980108     C     VIDLNP        LOOKUP    LP                                     32
100500980108    2C     *IN32         IFEQ      *ON
100600980108     C                   SETON                                        285699
100700980108     C                   MOVEL     MSG(14)       VIDMSG
100800980108     C                   GOTO      ENDEDI
100900980108    2C                   ENDIF
101000980108     C* Controllo se già selezionato cliente con quella lnp
101100980108     C     VIDLNP        LOOKUP    CLP                                    32
101200980108    2C     *IN32         IFEQ      *ON
101300980108     C                   SETON                                        285699
101400980108     C                   MOVEL     MSG(15)       VIDMSG
101500980108     C                   GOTO      ENDEDI
101600980108    2C                   ENDIF
101700980107    1C                   ENDIF
101800980107     C*-----------------------------------------------------*
101900980107     c* CHAIN DSPU PER REPERIRE GLI ALTRI DATI
102000980107     C                   MOVEL     'PU'          COD
102100980107     C                   MOVEL     EDK(YY)       EDKEY
102200980107     C     KEDTAB        CHAIN     EDTAB01L                           30
102300980108    1C     *IN30         IFEQ      *ON
102400980107     C     TABFLG        ORNE      ' '
102500980107     C                   SETON                                        284599
102600980107     C                   MOVEL     MSG(9)        VIDMSG
102700980107     C                   GOTO      ENDEDI
102800980108    1C                   ENDIF
102900980107     C**
103000980107     C                   MOVEL     TABUNI        EDSPU
103100980108    1C     §PUDTA        IFNE      'S'
103200980108    2C     VIDCLI        IFEQ      0
103300980108     C                   SETON                                        285699
103400980108     C                   ELSE
103500980107     C                   SETON                                        284599
103600980108     C                   END
103700980107     C                   MOVEL     MSG(10)       VIDMSG
103800980107     C                   GOTO      ENDEDI
103900980108    1C                   ENDIF
104000980108     C*
104100980108     C                   MOVEL     '1'           UNO               1
104101140717     c* se elaborazione definitiva non devo aver già fatto un lancio
104102140717     c* uguale o per la stessa lnp
104103140717     c* cotnrollo mediante file FNQUE che viene scritto dal batch
104104140717    1c                   if        vidsn='S'
104105140717     C                   move      vidnfV        WNFV
104106140717    2c                   select
104107140717     c* richiesta solo lnp
104108140717     c                   when      vidnrs='99' and vidcli = *zeros
104109140717     c     kque          chain     fnque01l
104110140717    3c                   if        %found(fnque01l)
104111140717     C                   SETON                                        285699
104112140717     C                   MOVEL     MSG(18)       VIDMSG
104113140717     c                   eval      vidmsg=%trim(vidmsg) + ' per lnp ' +
104114140717     c                             %editc(quelnp:'X')
104115140717     c                   if        quenrs<>'99'
104116140717     c                   eval      vidmsg=%trim(vidmsg) + ' serie ' +
104117140717     c                             quenrs
104118140717     c                   endif
104119140717     c                   if        queccm>*zeros
104120140717     c                   eval      vidmsg=%trim(vidmsg) + ' cliente ' +
104121140717     c                             %editc(queccm:'X')
104122140717     c                   endif
104123140717     C                   GOTO      ENDEDI
104124140717    3c                   endif
104125140717     c                   other
104126140717     c* richiesta lnp e serie   oppure
104127140717     c* richiesta lnp serie e cliente
104128140717     c     kque          setll     fnque01l
104129140717     c     kque          reade     fnque01l
104130140717    3c                   dow       not %eof(fnque01l)
104131140717     c                   select
104132140717     c* richiesta lnp e serie
104133140717     c                   when      vidnrs<>'99'   and vidcli = *zeros
104134140717    4c                   if        vidnrs=quenrs or quenrs='99'
104135140717     C                   SETON                                        285699
104136140717     C                   MOVEL     MSG(18)       VIDMSG
104137140717     c                   eval      vidmsg=%trim(vidmsg) + ' per lnp ' +
104138140717     c                             %editc(quelnp:'X') + ' e serie '   +
104139140717     c                             quenrs
104140140717     C                   GOTO      ENDEDI
104141140717     c                   leave
104142140717    4c                   endif
104143140717     c* richiesta lnp serie e cliente
104144140717     c                   when      vidnrs<>'99' and vidcli>*zeros
104145140717    4c                   if        (vidnrs=quenrs and (vidcli=queccm or
104146140717    4c                             queccm=0)) or quenrs='99'
104148140717     C                   SETON                                        285699
104149140717     C                   MOVEL     MSG(18)       VIDMSG
104150140717     c                   eval      vidmsg=%trim(vidmsg) + ' per lnp ' +
104151140717     c                             %editc(quelnp:'X') + ' serie '   +
104152140717     c                             quenrs
104153140717     c                   if        queccm=vidcli
104154140717     c                   eval      vidmsg=%trim(vidmsg) + ' e cliente ' +
104155140717     c                             %editc(queccm:'X')
104156140717     c                   endif
104157140717     C                   GOTO      ENDEDI
104158140717     c                   leave
104159140717     c                   endif
104160140717     c                   endsl
104161140717     c     kque          reade     fnque01l
104162140717    3c                   enddo
104163140717    2c                   endsl
104164140717    1c                   endif
104165140717     c
104200980108     C*
104300980108     C* Se non ci sono errori carico lnp selez. in schiera di comodo
104400980112     C     VIDKLP        IFEQ      0
104500980108     C                   ADD       1             W
104600980108     C                   Z-ADD     VIDLNP        LP(W)
104700980112     C                   END
104800980108     C* se ho il cliente carico lnp cliente in schiera di comodo
104900980108     C     VIDKLP        IFNE      0
105000980108     C                   ADD       1             WW
105100980108     C                   Z-ADD     VIDKLP        CLP(WW)
105200980108     C                   END
105300920731     C*
105400980107     C     ENDEDI        ENDSR
105500110715
105600110715      *---------------------------------------------------------------*
105700110715      *?Reperimento dati del job (Utente/Operativi)                  ?*
105800110715      *---------------------------------------------------------------*
105900110715     c     sr_DatiJob    BEGSR
106000110715      *
106100110715     c     *dtaara       define    §azute        AZUTEds
106200110715     c     *dtaara       define    §datiute      dDatiUte
106300110715      *
106400110715     c                   in(E)     *dtaara
106500110715     c                   IF        %Error or RSUT = *blanks
106600110715     c                   clear                   TIBS34ds
106700110715     c                   call      'TIBS34R'
106800110715     c                   parm                    TIBS34ds
106900110715     c                   in        *dtaara
107000110715     c                   ENDIF
107100110715      *
107200110715     c                   ENDSR
107300980108**
107400950530 CON SPUNTE  ENTRATA
107500950530 CON SPUNTE PARTENZA
107600961028    PER   E. D. I.
107700961017**  MSG
107800961017Immettere linea di partenza valida                                            1
107900961017Codice Cliente annullato                                                      2
108000961017Codice Cliente errato                                                         3
108100961017Codice Cliente bloccato                                                       4
108200980108Se conferma E.D.I. immettere almeno UNA lin.partenza/cod.cliente/serie        5
108300961028IMPOSSIBILE  ESEGUIRE  QUADRATURA  CON   E . D . I . !!!!                     6
108400961028Il codice cliente immesso non e'  E.D.I.                                      7
108500961028La serie non e' quella assegnata al cliente per la conferma con E.D.I.        8
108600961028Impossibile la trasmissione EDI: manca la tabella "PU": TELEFONARE IN SEDE!!  9
108700980108Impossibile trasmissione EDI: il cliente/Partner non ritorna le date di arrivo10
108800961028Se conferma E.D.I. immettere obbligatoriamente il codice cliente e la serie   1
108900980107Se conferma E.D.I. immettere o codice cliente e serie o linea di partenza     2
109000980107La Linea di partenza non esiste su tabella EDI o non appartiene ad un partner 3
109100980108Se conferma E.D.I. impossibile selez. più volte la stessa linea di partenza   4
109200980108Se conferma E.D.I. impossibile indicare lnp relativa ad un cliente già selez. 5
109300980108Se conferma E.D.I. impossibile indicare cliente di una lnp già selezionata    6
109400060310Serie errata                                                                  7
109500140717Conferma E.D.I.: già elaborato                                                8
