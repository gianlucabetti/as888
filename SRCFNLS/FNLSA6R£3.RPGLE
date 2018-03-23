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
002600060310     D MSG             S             78    DIM(17) CTDATA PERRCD(1)
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
005800980415     D  COMDSA               246    250P 0
005900980108     D  PARTQU               251    251
006000980108     D  VIDSN                252    252
006100980108     D  VIDSTA               253    253
006200980108     D  VIDFIL               254    256  0
006300950530     D**
006400950530     D* RICERCA CODICE PADRONCINO     -FNLV24R -
006500021203     D FNLV24DS      E DS
006600950530     D**
006700920730     D                 DS
006800961017     D  WCLI                   1      7  0
006900941228     D  VIDDRT                 8     15  0
007000941228     D  RECORD                 1     15  0
007100961017     D                 DS
007200961017     D  VIDKLP                 1      3  0
007300961017     D  VIDKSC                 4      7  0
007400961017     D  VIDCLI                 1      7  0
007500950530     D**
007600950530     D* RICEZIONA PARAMETRI DA KPJBU
007700950530     D**
007800950530     D* TQU: TIPO QUADRATURA: ' ' - CON FOGLIO ENTRATA
007900950530     D*                       'P' - CON SPUNTE PARTENZA ED IMP
008000950530     D PARAM3          DS
008100950530     D  PA3TQU                 1      1
008200950530     D*
008300930125     D                 DS
008400930125     D  COMDAT                 7     12
008500930125     D  COMDF0                 1     25
008600941228     D***
008700941228     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
008800941228     D***
008900941228     D WLBDAT          DS                  INZ
009000941228     D  G02DAT                 1      8  0
009100941228     D  G02INV                 9     16  0
009200941228     D  G02ERR                17     17
009300941228     D  G02TGI                18     22  0
009400941228     D***
009500941228     D* PARAMETRI PER TRASFORM GIORNI IN DATA             - XSRDGI -
009600941228     D***
009700941228     D WGIDAT          DS                  INZ
009800941228     D  GIODAT                 1      8  0
009900941228     D  GIOINV                 9     16  0
010000941228     D  GIOTGI                17     21  0
010100110715      *
010200110715     D KPJBA         E DS
010300110715      *
010400110715      * - Parametri per richiamo al pgm di controllo profilo utenti
010500110715     d TIBS34ds      e ds
010600110715      * - Ds di riferimento al file esterno AzUte00F
010700110715     d AZUTEds       e ds                  ExtName(AzUte00F)
010800110715      * - Ds per dati organigramma
010900110715     d dDatiUte      e ds
011000110715      * RICERCA FOGLI VARI
011100110715     D DSLR01        E DS
011200110715     D*
011300110715     D DS3C          E DS
011400110715     D*
011500941228     D* DS PER TRUL06R - CARICAMENTO £X
011600941228     D DSUL06        E DS                  EXTNAME(TRUL06DS)
011700941228     D  LIN                    1     90  0
011800941228     D                                     DIM(30)
011900941228     D*
012000941228     D* DS PER FNLV50F - CONTROLLO FILIALE GESTIONE
012100941228     D DSLV50        E DS                  EXTNAME(FNLV50DS)
012200961025     D*
012300961025     D* DS PER EDI
012400961025     D EDSPT         E DS                  EXTNAME(EDIDSPT)
012500961025     D EDSPU         E DS                  EXTNAME(EDIDSPU)
012600980717     D EDSCL         E DS                  EXTNAME(EDIDSCL)
012700021203
012800021203     D kpdpdr          S                   LIKE(APDpdr)
012900021203     D kpdtip          S                   LIKE(APDtip)
013000060310     D alfa3cnrs       S              2
013100900702     C****************************************************************
013200900702     C*  RIEPILOGO INDICATORI
013300900702     C***************************************************************
013400950530     C* 01    - ON - QUADRATURA CON SPUNTE PARTENZA 'P'
013500961025     C* 02    - ON - QUADRATURA CON SPUNTE ENTRATA PER E.D.I.
013600950530     C* 04    - CAMBIO FILIALE IN GESTIONE PER CMD18
013700020429     C* 05    - SONO UN SECONDO LIVELLO
013800920804     C* 36    - SFLNXTCHG
013900920804     C* 37    - PULIZIA SUBFILE
014000920522     C* 30/35 - DI COMODO
014100920901     C* 40/57 - ERRORE
014200920804     C* 98/99 - ERRORE GENERICO
014300900702     C*****************************************************************
014400000000     C     *ENTRY        PLIST
014500000000     C                   PARM                    KPJBA
014600950530     C                   MOVEL     KPJBU         PARAM3
014700950530     C*
014800110715     C                   Z-ADD     1             CODUT
014900110715      * Reperimento dati utente/aziendali
015000110715     c                   exsr      sr_DatiJob
015100110715     C                   MOVEL     RSUT          PARDUT           30
015200920901     C*
015300920901     C*---------------------------------------------------------------*
015400920730     C* ACCESSO CNACO
015500920730     C     KACO          KLIST
015600110715     C                   KFLD                    CODUT             1 0
015700110715     C                   KFLD                    dutKCI
015800920901     C                   KFLD                    KSC               7 0
015900920730     C* ACCESSO TABEL
016000920730     C     KTAB          KLIST
016100920730     C                   KFLD                    CODUT
016200920730     C                   KFLD                    COD               2
016300920730     C                   KFLD                    KEY               8
016400961030     C     KTAB2         KLIST
016500961030     C                   KFLD                    CODUT
016600961030     C                   KFLD                    COD               2
016700961025     C     KEDTAB        KLIST
016800961025     C                   KFLD                    COD
016900961025     C                   KFLD                    EDKEY            35
017000920731     C* ACCESSO FLFVV
017100910320     C     KFVV          KLIST
017200911015     C                   KFLD                    COMNPG            1 0
017300911015     C                   KFLD                    COMNFV            5 0
017400941228     C                   KFLD                    VIDFIL
017500000131     C* ACCESSO FIBRV
017600000131     C     KBRV          KLIST
017700000131     C                   KFLD                    COMNPG            1 0
017800000131     C                   KFLD                    WNFV
017900000131     C                   KFLD                    VIDFIL
018000000131     C*
018100021203     C     Kfapd         klist
018200021203     C                   kfld                    kpdtip
018300021203     C                   kfld                    kpdpdr
018400021203      *
018500000131     C     *LIKE         DEFINE    BRVNFV        WNFV
018600941228     C*---------------------------------------------------------------*
018700920423     C*
018800920423     C* CAMPI DI PASSAGGIO
018900941228     C                   CLEAR                   DSLR01
019000941228     C                   MOVEL     5             DLRNPG
019100941228     C                   MOVEL     'FNLSA6R'     DLRPGM
019200941228     C                   MOVEL     'FNLSA6R'     VIDPGM
019300950530     C*
019400961025     C* 01 ON - QUADRATURA 'P'  SPUNTE PARTENZA
019500961025     C* 02 ON - QUADRATURA 'E'  SPUNTE ENTRATA CON EDI
019600961025     C                   SELECT
019700961025     C     PA3TQU        WHENEQ    'P'
019800950530     C                   SETON                                        01
019900950530     C                   MOVEL     TE2(2)        VIDTE2
020000961025     C     PA3TQU        WHENEQ    'E'
020100961025     C                   SETON                                        02
020200961025     C                   MOVEL     TE2(3)        VIDTE2
020300961025     c*
020400961025     C                   OPEN      EDTAB01L                             97
020500961025     C     *IN97         IFEQ      *ON
020600961030     C                   MOVEL     MSG(6)        VIDMSG
020700961025     C                   SETON                                          28
020800961025     C                   ENDIF
020900961025     C**
021000961025     C                   OTHER
021100950530     C                   MOVEL     TE2(1)        VIDTE2
021200961028     C                   ENDSL
021300920424     C*
021400941228     C* DATA GIORNO - 1
021500950703     C*
021600941228     C                   TIME                    W0140            14 0
021700941228     C* UDATE IN GGMMAAAA
021800941228     C                   MOVE      W0140         WDTGIO            8 0
021900941228     C*
022000941228     C* UDATE IN AAAAMMGG
022100941228     C                   Z-ADD     WDTGIO        G02DAT
022200941228     C                   MOVEL     *BLANK        G02ERR
022300941228     C                   CALL      'XSRDA8'
022400941228     C                   PARM                    WLBDAT
022500941228     C                   MOVEL     G02INV        DATEU             8 0
022600941228     C* SUB        1
022700941228     C     G02TGI        SUB       1             GIOTGI
022800941228     C*
022900941228     C                   CALL      'XSRGI8'
023000941228     C                   PARM                    WGIDAT
023100920424     C*
023200941228     C                   MOVEL     GIODAT        SAVDIN            8 0
023300920730     C*
023400020429     C* VEDO SE SONO PRIMO O SECONDO LIVELLO
023500110718     C                   Z-ADD     dutPOU        VIDFIL
023600920831     C*
023700110718     C     dutLPO        IFEQ      '2'
023800110718     C     dutLPO        OREQ      *BLANKS
023900920831     C                   EXSR      CARL6
024000920831     C                   SETON                                        05
024100920831     C                   END
024200920831     C*
024300930125     C* CONTROLLO SE SI TRATTA DI FILIALE CON "CML"
024400930125     C     VIDFIL        CHAIN     AZORG                              30
024500930125     C  N30              MOVEL     ORGDF0        COMDF0
024600930125     C  N30COMDAT        IFNE      *BLANKS
024700930125     C* SI TRATTA DI FILIALE CON "CML"
024800930125     C                   MOVEL     'N'           VIDSTA
024900930125     C                   ELSE
025000930125     C* SI TRATTA DI FILIALE SENZA "CML"
025100930125     C                   MOVEL     'S'           VIDSTA
025200930125     C                   END
025300961025     C*
025400961025     C     *IN02         IFEQ      *ON
025500961025     C                   MOVEL     'N'           VIDSTA
025600961025     C                   ENDIF
025700930125     C*
025800920730     C* CARICO TABELLA FILIALI GESTITE £1
025900920730     C                   EXSR      CARTAB
026000920730     C*
026100920424     C     INIZIO        TAG
026200920730     C* PULIZIA DATI SUBFILE CONTROL
026300920901     C                   SETOFF                                       04
026400920730     C                   Z-ADD     0             VIDDSP
026500980415     C                   Z-ADD     0             VIDDSA
026600920730     C                   MOVEL     *BLANKS       VIDNFV
026700920424     C                   MOVEL     *BLANKS       VIDDES
026800920730     C                   Z-ADD     5             COMNPG
026900920916     C                   MOVEL     'N'           VIDSN
027000080626     C   01              MOVEL     'N'           VID9000
027100110718     c                   eval      VidORD = '1'
027200920423     C*
027300920730     C                   Z-ADD     0             NRR               4 0
027400920730     C* CARICO SUBFILE
027500920831     C                   EXSR      CARICA
027600920730     C*
027700900509     C     FOR01         TAG
027800941228     C                   WRITE     LSA6D01
027900920730     C     FORCT         TAG
028000941228     C                   EXFMT     LSA6DCT
028100941228     C*
028200961017     C* PULIZIA CAMPO MESSAGGIO E RELATIVO INDICATORE (*IN28)
028300961017     C                   CLEAR                   VIDMSG
028400941228     C                   SETOFF                                       289055
028500961025     C                   SETOFF                                       444556
028600961028     C                   SETOFF                                       59
028700920730     C*
028800910320     C** CMD3 - FINE LAVORO
028900961025     C**        O ERRORE: NON TROVATO FILE EDTAB01L
029000961025     C   97
029100961025     COR KC              GOTO      FINE
029200920831     C*
029300920831     C** CMD18- CAMBIO FILIALE
029400920831     C   KS              SETON                                        04
029500920831     C   KS              GOTO      FORCT
029600920831     C*
029700941228     C* 04 ON - CONTROLLO FILIALE IN GESTIONE
029800941228     C   04              EXSR      CTRFGS
029900941228     C   04
030000941228     CAN 90              GOTO      FOR01
030100920730     C*
030200920730     C* CONTROLLO DATI SUBFILE
030300920730     C                   EXSR      CONSFL
030400920831     C   98              GOTO      FOR01
030500920831     C*
030600920730     C   99              GOTO      FORCT
030700920730     C*
030800920730     C* CONTROLLO DATI SUBFILE CONTROL
030900920730     C                   EXSR      CONCTL
031000920730     C   98              GOTO      FOR01
031100920730     C*
031200950113     C  NKF
031300920831     COR 99              GOTO      FORCT
031400920730     C*
031500920807     C* CARICO SCHIERE PER PASSAGGIO
031600920807     C                   MOVEA     *ZEROS        LNP
031700920807     C                   MOVEA     *ZEROS        CPC
031800920807     C                   MOVEA     *ZEROS        CLI
031900920807     C                   MOVEA     *ZEROS        DRT
032000930520     C                   MOVEA     *ZEROS        NRS
032100950323     C                   MOVEA     *BLANKS       MOP
032200950323     C                   MOVEA     *BLANKS       NRT
032300920901     C                   Z-ADD     1             X                 3 0
032400920807     C                   Z-ADD     1             NRR
032500920807     C*
032600950323     C     NRR           DOWLE     12
032700920807     C*
032800941228     C     NRR           CHAIN     LSA6DSF                            31
032900920831     C     *IN31         IFEQ      '0'
033000920831     C     VIDLNP        ANDNE     0
033100920807     C                   MOVEL     VIDLNP        LNP(X)
033200920914     C     VIDCPC        IFGT      *ZEROS
033300950313RM*  C                   MOVEL     VIDLNP        CPC(X)
033400950313RM*  C                   MOVE      VIDCPC        CPC(X)
033500920914     C                   END
033600930520     C                   MOVEL     VIDNRS        NRS(X)
033700920807     C                   MOVEL     VIDCLI        CLI(X)
033800941228     C                   Z-ADD     COMDRT        DRT(X)
033900950323     C                   MOVEL     VIDMOP        MOP(X)
034000950323     C                   MOVEL     VIDNRT        NRT(X)
034100920901     C                   ADD       1             X
034200920807     C                   END
034300920807     C*
034400920807     C                   ADD       1             NRR
034500920807     C                   END
034600961025     c* IMPOSTO DATI PER EDI
034700961025     C     *IN02         IFEQ      *ON
034800961025     C                   MOVEL     'S'           EDSEL
034900961025     C                   ENDIF
035000920831     C*
035100980415     C  N01COMDSA        IFEQ      0
035200980415     C                   Z-ADD     COMDSP        COMDSA
035300980415     C                   END
035400980415     C*
035500950530     C                   MOVEL     PA3TQU        PARTQU
035600900702     C                   MOVEL     PARAM         KPJBU
035700941228     C                   MOVEL     'LSA7'        KCOAZ
035800110718     c                   if        knmus = *all'1'
035900110718     c                   call      'FNLSA7C'
036000110718     c                   parm                    KPJBA
036100110718     c                   else
036200070109     C                   CALL      'BCH10'
036300900702     C                   PARM                    KPJBA
036400110718     c                   endif
036500920522     C*
036600950411     C                   GOTO      INIZIO
036700000000     C     FINE          TAG
036800000000     C                   SETON                                        LR
036900920730     C*
037000920730     C*--- CARICO TABELLA FILIALI GESTITE £1 -------------------------*
037100920730     C     CARTAB        BEGSR
037200941228     C***
037300941228     C* CARICO TABELLA FILIALI GESTITE £1
037400941228     C***
037500941228     C                   CLEAR                   DSUL06
037600941228     C                   MOVE      '£1'          D06COD
037700941228     C                   MOVEL     SIMFEL        D06KEY
037800941228     C                   MOVEL     DSUL06        KPJBU
037900941228     C*
038000950323     C                   CALL      'TRUL06R'
038100941228     C                   PARM                    KPJBA
038200941228     C                   MOVEL     KPJBU         DSUL06
038300941228     C                   MOVEA     LIN           L1
038400961025     C**
038500961025     C* CARICO TABELLA 'PT' DA EDITAB01L
038600961025     C**
038700980717     C     *IN02         IFEQ      *ON
038800980717     C     *IN97         ANDEQ     *OFF
038900961025     C                   MOVEL     'PT'          COD
039000961025     C                   Z-ADD     1             YY                3 0
039100961025     C     COD           SETLL     EDTAB01L
039200961025     C     COD           READE     EDTAB01L                               30
039300961025     C     *IN30         DOWEQ     *OFF
039400961025     C     TABFLG        IFEQ      ' '
039500020917     C                   MOVEL     TABUNI        EDSPT
039600020917      * solo se compresa nelle linee di partenza del terminal
039700020917     c     §ptlnp        lookup    L1                                     32
039800020917     c                   if        *in32 = *on
039900961025     C                   MOVEL     TABKEY        EDK(YY)
040000961025     C                   MOVEL     §PTKSC        EDC(YY)
040100961025     C                   MOVEL     §PTNRS        EDS(YY)
040200980107     C                   MOVEL     §PTLNP        EDL(YY)
040300961025     C                   ADD       1             YY
040400020917     c                   end
040500961025     C                   ENDIF
040600961025     C*
040700961025     C     COD           READE     EDTAB01L                               30
040800961025     C                   ENDDO
040900980107     C**
041000980107     C** Scorro tabella PU e se il codice corrispondente
041100980107     C** non è un partner ma un cliente azzero lnp
041200980108     C                   DO        YY            YW                4 0
041300980107     C                   MOVEL     'PU'          COD
041400980108     C                   MOVEL     EDK(YW)       EDKEY
041500980107     C     KEDTAB        CHAIN     EDTAB01L                           30
041600980108    3C     *IN30         IFEQ      *OFF
041700980108     C     TABFLG        ANDEQ     ' '
041800980107     C                   MOVEL     TABUNI        EDSPU
041900980107     C     §PUTPC        IFNE      'P'
042000980108     C                   Z-ADD     0             EDL(YW)
042100980107     C                   ENDIF
042200980107     C                   ELSE
042300980108     C                   Z-ADD     0             EDL(YW)
042400980107     C                   ENDIF
042500980107     C                   ENDDO
042600980717     C**
042700980717     C* CARICO TABELLA 'CL' DA EDITAB01L
042800980717     C**
042900980717     C                   MOVEL     'CL'          COD
043000980717     C                   Z-ADD     1             YY                3 0
043100980717     C     COD           SETLL     EDTAB01L
043200980717     C     COD           READE     EDTAB01L                               30
043300980717     C     *IN30         DOWEQ     *OFF
043400980717     C     TABFLG        IFEQ      ' '
043500980717     C                   MOVEL     TABKEY        KCL(YY)
043600980717     C                   MOVEL     TABUNI        EDSCL
043700980717     C                   MOVEL     §CLKSC        TCL(YY)
043800980717     C                   ADD       1             YY
043900980717     C                   ENDIF
044000980717     C*
044100980717     C     COD           READE     EDTAB01L                               30
044200980717     C                   ENDDO
044300961030     C*
044400961030     C                   ENDIF
044500920730     C                   ENDSR
044600920730     C*
044700920831     C* CARICO L6 ------------------------------------------***********
044800920831     C*
044900920831     C     CARL6         BEGSR
045000941228     C*
045100941228     C                   CLEAR                   DSUL06
045200941228     C                   MOVE      '£6'          D06COD
045300941228     C                   MOVEL     VIDFIL        D06KEY
045400941228     C                   MOVEL     DSUL06        KPJBU
045500941228     C                   CALL      'TRUL06R'
045600941228     C                   PARM                    KPJBA
045700941228     C                   MOVEL     KPJBU         DSUL06
045800941228     C                   MOVEA     LIN           L6
045900920831     C                   ENDSR
046000920831     C*
046100920831     C*--- PULISCO E CARICO SUBFILE ----------------------------------*
046200920730     C*
046300920831     C     CARICA        BEGSR
046400920831     C* PULIZIA SUBFILE
046500920804     C                   SETON                                        37
046600941228     C                   WRITE     LSA6DCT
046700920804     C                   SETOFF                                       37
046800920730     C*
046900920831     C* CARICO DATI IN SUBFILE
047000920730     C*
047100920730     C                   Z-ADD     0             VIDLNP
047200920730     C                   MOVEL     *BLANKS       DESLNP
047300920914     C                   MOVEL     *BLANKS       VIDCPC
047400920730     C                   MOVEL     *BLANKS       DESCPC
047500060310     C                   movel     99            VIDNRS
047600920730     C                   Z-ADD     0             VIDCLI
047700920730     C                   MOVEL     *BLANKS       DESCLI
047800920730     C                   Z-ADD     0             VIDDRT
047900941228     C                   Z-ADD     0             COMDRT
048000950323     C                   MOVE      *BLANKS       VIDMOP
048100950323     C                   MOVE      *BLANKS       VIDNRT
048200920730     C*
048300950323     C                   DO        12            NRR
048400941228     C                   WRITE     LSA6DSF
048500941228     C                   ENDDO
048600920730     C*
048700920831     C                   Z-ADD     1             REC
048800920730     C                   ENDSR
048900920730     C*
049000920730     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
049100920730     C*
049200920730     C     CONSFL        BEGSR
049300920831     C                   SETOFF                                       9899
049400920730     C                   Z-ADD     1             NRR
049500980108     C                   CLEAR                   UNO
049600980108     C                   Z-ADD     0             LP
049700980108     C                   Z-ADD     0             CLP
049800980108     C                   Z-ADD     0             W                 4 0
049900980108     C                   Z-ADD     0             WW                4 0
050000920730     C*
050100941228     C                   READC     LSA6DSF                                33
050200920730     C     *IN33         DOWEQ     '0'
050300961028     C* PER EDI CON ELABORAZIONE DEFINITIVA, POSSO METTERE SOLTANTO
050400961025     C* 1 RICHIESTA E SOLO IL CODICE CLIENTE
050500930520     C*
050600961017     C****  LINEA PARTENZA CODICE CLIENTE  ****
050700961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
050800961017    2C     VIDKLP        IFGT      0
050900961017     C     VIDKLP        CHAIN     AZORG01L                           34
051000961017    3C     *IN34         IFEQ      *ON
051100961017     C                   SETON                                        284499
051200961017     C                   MOVEL     MSG(1)        VIDMSG
051300961017     C                   GOTO      ENDSFL
051400961017    3C                   ENDIF
051500961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
051600961017    3C     VIDKSC        IFEQ      0
051700961017     C     DESCLI        ANDEQ     *BLANKS
051800961017     C                   SETON                                        284599
051900961017     C                   MOVEL     MSG(3)        VIDMSG
052000961017     C                   GOTO      ENDSFL
052100961017    3C                   ENDIF
052200961017     C*
052300961017   X2C                   ELSE
052400961017    3C     VIDKSC        IFGT      0
052500961017     C                   SETON                                        284499
052600961017     C                   MOVEL     MSG(1)        VIDMSG
052700961017     C                   GOTO      ENDSFL
052800961017    3C                   ENDIF
052900961017    2C                   ENDIF
053000961017     C*
053100920730     C*** SE E' STATA DIGITATA UNA CHIAVE DI RICERCA E NON E' STATO
053200920730     C***    IMMESSO IL CODICE CLIENTE CALL PGM
053300961017     C     VIDKSC        IFEQ      0
053400920730     C     DESCLI        ANDNE     *BLANKS
053500981109     C                   CLEAR                   PARDIT
053600920730     C*
053700920730     C* PARSTA = 9  ESCLUDO ANNULLATI
053800110715     C                   Z-ADD     dutKCI        PARKCC            4 0
053900920730     C                   Z-ADD     9             PARSTA
054000920902     C                   MOVEL     *BLANKS       DES              48
054100920902     C                   MOVEL     DESCLI        DES
054200961017     C                   MOVEL     VIDKLP        PARFLR
054300981109     C     VIDKLP        CHAIN     AZORG01L                           31
054400981109     C  N31              MOVEL     ORGDIT        PARDIT
054500000907     C                   Z-ADD     1             PAXNUM
054600000907     C                   CALL      'XALFA3BR'
054700920730     C                   PARM                    PARDUT
054800920730     C                   PARM                    CODUT
054900920902     C                   PARM                    DES
055000961016     C                   PARM                    PARKCC
055100920730     C                   PARM                    PARSTA            1 0
055200961016     C                   PARM                    PARFLR           90
055300981109     C                   PARM                    PARDIT            3
055400000907     C                   PARM                    PAXNUM            2 0
055500000907     C                   PARM                    PAXKCM           80
055600000907     C                   PARM                    PAXKSM          140
055700000907     C                   PARM                    PAXKDM           60
055800920831     C     PARSTA        IFNE      -1
055900000907     C                   MOVEL     PAXKSM        VIDCLI
056000920902     C                   MOVEL     DES           DESCLI
056100920831     C                   END
056200920831     C*
056300920831     C                   SETON                                        9899
056400920730     C                   GOTO      ENDSFL
056500920730     C                   END
056600920730     C*
056700920901     C*** LINEA DI PARTENZA
056800961025    1C     VIDLNP        IFNE      0
056900920901     C*
057000920901     C* CONTROLLO SE LINEA VALIDA
057100920901     C     VIDLNP        LOOKUP    L1                                     31
057200061212     c
057300061212     c* Se lnp non in £1 devo aver immesso codice cliente e serie
057400061212     c*  per poterla accettare
057500061212     c                   if        not *in31
057600061212     c                   if        *in01    and (vidnrs='00' or vidcli=0
057700061212     c                             or vidnrs='SI' or vidnrs='99')
057800061212     c                             or not *in01
057900061212     C                   SETON                                        4099
058000061212     C                   GOTO      ENDSFL
058100061212     c                   endif
058200070109     c
058300070109     c* Se lnp non in £1 impossibile richiedere anomalie manca bolla
058400070109     c*  di selezioni non richieste
058500070109     c                   if        vidsta='S'
058600070109     C                   SETON                                        6099
058700070109     C                   GOTO      ENDSFL
058800070109     c                   endif
058900061212     c                   endif
059000920901     C*
059100920901     C* DECODIFICO LINEA DI PARTENZA
059200920901     C     VIDLNP        CHAIN     AZORG                              30
059300920901     C  N30              MOVEL     ORGDES        DESLNP
059400920901     C   30              MOVEL     *BLANKS       DESLNP
059500061212     c
059600061212     c                   if        *in30 or (orgfag<>'F' and orgfag<>'A')
059700061212     c                             or orgfva<>' '
059800061212     C                   SETON                                        4099
059900061212     C                   GOTO      ENDSFL
060000061212     c                   endif
060100920901     C*
060200961025   X1C                   ELSE
060300920902     C* LINEA DI PARTENZA = 0
060400920902     C                   MOVEL     *BLANKS       DESLNP
060500961025     C                   MOVEL     VIDCLI        WCLI
060600961025     C*
060700920901     C* SE DIGITATI : PADRONCINO  O  CLIENTE  O  DATA RITIRO
060800950323     C* O MATT/POMERIGGIO O NUMERO RITIRO
060900920901     C*    SI DEVE INSERIRE ANCHE LA LINEA DI PARTENZA
061000961028    2C     RECORD        IFNE      *ZEROS
061100100823     C     VIDCPC        ORne      *ZEROS
061200100823     C     VIDCPC        andne     *blanks
061300060310     C     VIDNRS        ORNE      '99'
061400950323     C     VIDMOP        ORNE      *BLANKS
061500950323     C     VIDNRT        ORNE      *BLANKS
061600961025     C* ERRORE: NON INSERIBILE
061700961028    3C     *IN02         IFEQ      *ON
061800961025     C     VIDSN         ANDEQ     'S'
061900980805   X C                   ELSE
062000961025     C* ERRORE : IMMETTERE ANCHE LA LINEA PARTENZA
062100920901     C                   SETON                                        4199
062200920901     C                   GOTO      ENDSFL
062300961028    3C                   END
062400961025    2C                   END
062500961025     C**
062600961025    1C                   END
062700060310     C*** Numero serie
062800060310     C* ESEGUO TESTN
062900060310     c                   if        vidnrs<>'SI'
063000060310     C                   TESTN                   VIDnrs               30
063100060310     C                   MOVE      VIDnrs        W001A             1
063200060310    2C     *IN30         IFEQ      *OFF
063300060310     C     W001A         ORLT      '0'
063400060310     C                   SETON                                        599928
063500060310     C                   MOVEL     MSG(17)       VIDMSG
063600060310     C                   GOTO      ENDSFL
063700060310    2C                   ENDIF
063800060310    2C                   ENDIF
063900920901     C*
064000920730     C*** CODICE PADRONCINO
064100941228    1C     VIDCPC        IFNE      *BLANKS
064200921012     C     VIDCPC        ANDNE     *ZEROS
064300941228     C*
064400941228     C     '?'           SCAN      VIDCPC                                 98
064500941228     C* RICERCA
064600941228    2C     *IN98         IFEQ      '1'
064700021203      *
064800021203      * imposto i campi della ds esterna fnlv24ds
064900021203     C                   clear                   fnlv24ds
065000021203     C                   eval      d24fil   = vidlnp
065100021203     C                   eval      d24tip   = 'A'
065200021203     C                   eval      d24flg   = 'R'
065300021203     C                   movel     fnlv24ds      kpjbu
065400021203
065500941228     C                   CALL      'FNLV24R'
065600921012     C                   PARM                    KPJBA
065700021203      *
065800021203      * valorizzo i campi a video
065900021203     C                   movel     kpjbu         fnlv24ds
066000021203     C                   move      d24pdr        vidcpc
066100021203     C                   if        d24pdr   <> *zeros
066200021205     C                   movel     d24pdr        vidlnp
066300021203     c                   endif
066400021203     C                   movel     d24rsc        descpc
066500921012     C                   SETON                                        99
066600950314RM*  C* DECOD.LINEA DI PARTENZA
066700950314RM*  C     VIDLNP        CHAIN     AZORG                              30
066800950314RM*  C  N30              MOVEL     ORGDES        DESLNP
066900950314RM*  C   30              MOVEL     *BLANKS       DESLNP
067000921012     C                   GOTO      ENDSFL
067100941228    2C                   END
067200921012     C**
067300941228     C* ESEGUO TESTN
067400100521     c                   clear                   descpc
067500941228     C                   TESTN                   VIDCPC               30
067600941228     C                   MOVE      VIDCPC        W001A             1
067700941228    2C     *IN30         IFEQ      *OFF
067800941228     C     W001A         ORLT      '0'
067900100521     C                   SETON                                        4299
068000941228     C                   GOTO      ENDSFL
068100941228    2C                   ENDIF
068200941228     C*
068300941228     C                   MOVE      VIDCPC        W0070             7 0
068400950313RM*  C                   MOVEL     VIDLNP        W0070
068500021205     C                   move      w0070         kpdpdr
068600021203     C                   move      'A'           kpdtip
068700021203     C     kfapd         chain     fiapd01l                           30
068800941228    2C     *IN30         IFEQ      *ON
068900100521     C     *IN30         OREQ      *OFF
069000100521     C     APDATB        ANDNE     *BLANKS
069100100521     C                   SETON                                        4299
069200941228     C                   GOTO      ENDSFL
069300941228    2C                   ENDIF
069400941228     C*
069500941228     C                   MOVEL     APDRSC        DESCPC
069600920902     C*
069700920902     C* CODICE PADRONCINO = 0
069800941228   X1C                   ELSE
069900920902     C                   MOVEL     *BLANKS       DESCPC
070000941228    1C                   END
070100920730     C*
070200961025    1C     VIDCLI        IFNE      0
070300930520     C*** NUMERO SERIE
070400060310    2C     VIDNRS        IFNE      '99'
070500060605     c                   clear                   ds3c
070600930521     C                   MOVEL     '3C'          COD
070700930521     C                   MOVEL     *BLANKS       KEY
070800930521     C                   MOVEL     VIDCLI        KEY
070900930521     C     KTAB          CHAIN     TABEL                              30
071000930521     C  N30              MOVEL     TBLUNI        DS3C
071100060605     c  N30              movel     §3cnrs        alfa3cnrs
071200930521     C* SE LA SERIE NON CORRISPONDE A QUELLA DEL CODICE CLIENTE
071300970321     C*   IMMESSO DO UN MESSAGGIO BLOCCANTE
071400980805     C     *IN30         IFEQ      *ON
071500980805     C     §3COKD        OREQ      ' '
071600060310    3C     alfa3cnrs     ORNE      VIDNRS
071700930521     C                   SETON                                        5899
071800930521     C                   GOTO      ENDSFL
071900961025    3C                   END
072000961025    2C                   END
072100930521     C*
072200961017     C****  CODICE CLIENTE  ****
072300920730     C                   MOVEL     VIDCLI        KSC
072400920901     C     KACO          CHAIN     CNACO000                           30
072500961025    2C  N30ACOFLG        IFNE      ' '
072600920730     C* CODICE CLIENTE ANNULLATO
072700961017     C                   SETON                                        284599
072800961017     C                   MOVEL     MSG(2)        VIDMSG
072900920730     C                   GOTO      ENDSFL
073000920730     C*
073100961025   X2C                   ELSE
073200920730     C* DECODIFICO CODICE CLIENTE
073300920730     C                   MOVEL     ACORAG        DESCLI
073400961025    2C                   END
073500920730     C*
073600920730     C* CODICE CLIENTE INESISTENTE
073700961025    2C     *IN30         IFEQ      *ON
073800961017     C                   SETON                                        284599
073900961017     C                   MOVEL     MSG(3)        VIDMSG
074000961017     C                   GOTO      ENDSFL
074100961025    2C                   ENDIF
074200920730     C*
074300920730     C* CODICE CLIENTE BLOCCATO
074400961025    2C     ACOABL        IFEQ      '8'
074500971021    2C     ACOABL        OREQ      '*'
074600961017     C                   SETON                                        284599
074700961017     C                   MOVEL     MSG(4)        VIDMSG
074800920730     C                   GOTO      ENDSFL
074900961025    2C                   END
075000961025     C*
075100961025     C**
075200961025    1C                   END
075300920730     C*
075400920903     C*** DATA RITIRO
075500941228    2C     VIDDRT        IFNE      0
075600920903     C* GIRO DATA RITIRO
075700920903     C                   MOVE      VIDDRT        G02DAT
075800920903     C                   MOVEL     *BLANKS       G02ERR
075900941228     C                   CALL      'XSRDA8'
076000920903     C                   PARM                    WLBDAT
076100941228    3C     G02ERR        IFEQ      '1'
076200920903     C                   SETON                                        4799
076300920903     C                   GOTO      ENDSFL
076400941228    3C                   END
076500941228     C                   Z-ADD     G02INV        COMDRT
076600950113     C                   Z-ADD     G02DAT        VIDDRT
076700941228     C*
076800920903     C* DATA RITIRO NON DEVE ESSERE > UDATE
076900941228    3C     COMDRT        IFGT      DATEU
077000920903     C                   SETON                                        4799
077100920903     C                   GOTO      ENDSFL
077200941228    3C                   END
077300941228     C*
077400941228     C                   ELSE
077500941228     C*
077600941228     C                   CLEAR                   COMDRT
077700941228    2C                   END
077800980107     C* Se elaborazione x EDI effettuo realtivi controlli
077900980107     C   02              EXSR      CTREDI
078000920730     C*
078100920730     C     ENDSFL        TAG
078200920730     C   99              Z-ADD     NRR           REC
078300920730     C*
078400920730     C* 36 - SFLNXTCHG
078500920730     C                   SETON                                        36
078600941228     C                   UPDATE    LSA6DSF
078700920730     C                   SETOFF                                       36
078800920730     C*
078900920730     C   99              SETON                                        33
079000941228     C  N99              READC     LSA6DSF                                33
079100920730     C                   END
079200961028     C**
079300961028     C* SE NON E' STATA EFFETTUATA NESSUNA SELEZIONE, PER EDI ERRORE
079400961028    1C  N99*IN02         IFEQ      *ON
079500961028     C     VIDSN         ANDEQ     'S'
079600980108     C     UNO           ANDEQ     *BLANKS
079700980108     C     1             CHAIN     LSA6DSF                            33
079800980108     C                   SETON                                        285699
079900980108     C                   MOVEL     MSG(5)        VIDMSG
080000980108     C  N33              UPDATE    LSA6DSF
080100961028    1C                   ENDIF
080200920730     C*
080300920730     C                   ENDSR
080400920730     C*
080500920730     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
080600920730     C*
080700920730     C     CONCTL        BEGSR
080800920730     C                   SETOFF                                       999831
080900920730     C*
081000920730     C*** NUMERO FOGLIO ENTRATA OBBLIGATORIO
081100950530     C     *IN01         IFEQ      *OFF
081200950530     C*
081300950530     C     VIDNFV        IFEQ      *BLANKS
081400920730     C     VIDNFV        OREQ      *ZEROS
081500920730     C                   SETON                                        4999
081600920730     C                   GOTO      ENDCTL
081700920730     C                   END
081800920730     C*
081900920730     C* NUMERO FOGLIO ENTRATA >< 0
082000920730     C     VIDNFV        IFNE      *BLANKS
082100920730     C     VIDNFV        ANDNE     *ZEROS
082200920730     C*
082300920730     C* RICERCA
082400941228     C     '?'           SCAN      VIDNFV                                 31
082500920730     C   31              DO
082600941228     C                   Z-ADD     0             DLRNFV
082700941228     C                   Z-ADD     SAVDIN        DLRGDA
082800941228     C                   Z-ADD     31129999      DLRGAL
082900941228     C                   MOVEL     VIDFIL        DLRFGS
083000941228     C                   MOVEL     'S'           DLRRIC
083100941228     C                   MOVEL     DSLR01        KPJBU
083200941228     C                   CALL      'FNLR02R'
083300920730     C                   PARM                    KPJBA
083400941228     C                   MOVEL     KPJBU         DSLR01
083500941228     C                   MOVEL     DLRNFV        VIDNFV
083600920730     C*
083700920730     C                   SETON                                        9899
083800920730     C                   GOTO      ENDCTL
083900920730     C                   END
084000920730     C*  C O N T R O L L I
084100920730     C                   TESTN                   VIDNFV               35
084200920730     C  N35              SETON                                        5099
084300920730     C  N35              GOTO      ENDCTL
084400920730     C*
084500920731     C                   MOVEL     VIDNFV        COMNFV
084600920731     C* CONTROLLO SE NUMERO FOGLIO ENTRATA ESISTE
084700941228     C     KFVV          CHAIN     FNFVV000                           30
084800920731     C     *IN30         IFEQ      '0'
084900920731     C*
085000920731     C     FVVATB        IFNE      ' '
085100920731     C* NUMERO FOGLIO ENTRATA ANNULLATO
085200920731     C                   SETON                                        5199
085300920731     C                   GOTO      ENDCTL
085400920731     C*
085500920731     C                   ELSE
085600920731     C* CONTROLLO SE ESISTONO SPUNTE PER QUEL FOGLIO
085700980409     C     PA3TQU        IFNE      'E'
085800000131     C                   Z-ADD     COMNFV        WNFV
085900070117     C     KBRV          CHAIN     FNBRV000                           34
086000920904     C   34              SETON                                        5299
086100920904     C   34              GOTO      ENDCTL
086200980409     C                   END
086300920904     C*
086400920904     C* DESCRIZIONE FOGLIO ENTRATA
086500941228     C                   MOVEL     FVVDSF        VIDDES
086600920904     C                   END
086700920904     C*
086800920731     C                   ELSE
086900920731     C* 30 ON - NUMERO FOGLIO ENTRATA INESISTENTE
087000920731     C                   SETON                                        5399
087100920731     C                   GOTO      ENDCTL
087200920731     C                   END
087300920901     C*
087400920731     C                   END
087500950530     C                   END
087600920731     C*
087700950313     C*** DATA SPEDIZIONE OBBLIGATORIA
087800920901     C     VIDDSP        IFEQ      0
087900920901     C                   SETON                                        4899
088000920901     C                   GOTO      ENDCTL
088100920901     C*
088200920901     C                   ELSE
088300920903     C                   MOVE      VIDDSP        G02DAT
088400920903     C                   MOVEL     *BLANKS       G02ERR
088500941228     C                   CALL      'XSRDA8'
088600920903     C                   PARM                    WLBDAT
088700920903     C     G02ERR        IFEQ      '1'
088800920903     C                   SETON                                        5499
088900920903     C                   GOTO      ENDCTL
089000920903     C                   END
089100920903     C                   Z-ADD     G02INV        COMDSP
089200950113     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
089300950113     C                   Z-ADD     G02DAT        VIDDSP
089400920902     C                   END
089500980415     C*** DATA SPEDIZIONE AL
089600980415     C                   CLEAR                   COMDSA
089700980415     C  N01VIDDSA        IFNE      0
089800980415     C                   MOVE      VIDDSA        G02DAT
089900980415     C                   MOVEL     *BLANKS       G02ERR
090000980415     C                   CALL      'XSRDA8'
090100980415     C                   PARM                    WLBDAT
090200980415     C     G02ERR        IFEQ      '1'
090300980415     C                   SETON                                        5799
090400980415     C                   GOTO      ENDCTL
090500980415     C                   END
090600980415     C                   Z-ADD     G02INV        COMDSA
090700980415     C*** DATA SPEDIZIONE AL MAGGIORE DI DATA SPEDIZIONE DAL
090800980415     C     COMDSA        IFLT      COMDSP
090900980415     C                   SETON                                        5799
091000980415     C                   GOTO      ENDCTL
091100980415     C                   END
091200980415     C* IMPOSTO A VIDEO LA DATA A 8 SE IMMESSA A 6
091300980415     C                   Z-ADD     G02DAT        VIDDSA
091400980415     C                   END
091500920901     C*
091600920731     C     ENDCTL        ENDSR
091700941228      *-----------------------------------------------------***********
091800941228      * CONTROLLO LA FILIALE IN GESTIONE                       CTRFGS
091900941228      *-----------------------------------------------------***********
092000941228     C     CTRFGS        BEGSR
092100941228     C                   SETOFF                                       90
092200941228     C                   CLEAR                   DSLV50
092300941228     C                   MOVEL     KNMUS         D50PRU
092400941228     C                   MOVEL     VIDFIL        D50FGS
092500941228     C                   CALL      'FNLV50R'
092600941228     C                   PARM                    DSLV50
092700941228     C*
092800941228     C     D50ERR        IFNE      ' '
092900941228     C                   MOVEL     D50MSG        VIDMSG
093000941228     C                   SETON                                        559028
093100941228     C                   GOTO      ENDCTF
093200941228     C                   ENDIF
093300941228     C*
093400941228     C                   EXSR      CARL6
093500941228     C*
093600941228     C     ENDCTF        ENDSR
093700980107      *-----------------------------------------------------***********
093800980107      * CONTROLLI PER SOTTOMISSIONE EDI                        CTREDI
093900980107      *-----------------------------------------------------***********
094000980107     C     CTREDI        BEGSR
094100980107     C*-----------------------------------------------------*
094200980107     C* Se ho selezionato il cliente .....
094300980107    1C     VIDCLI        IFNE      0
094400980107     C* .... è obbligatoria la serie
094500060310    2C     VIDNRS        IFEQ      '99'
094600980107     C                   SETON                                        285999
094700980107     C                   MOVEL     MSG(11)       VIDMSG
094800980107     C                   GOTO      ENDEDI
094900980107    2C                   ENDIF
095000980717      *
095100980717     C* Se ho selezionato il cliente aggancio la tabella PT
095200980107     C                   Z-ADD     1             YY
095300980107     C     VIDCLI        LOOKUP    EDC(YY)                                30
095400980717     C* Se non lo ho trovato vedo se è in tabella CL
095500980717    2C     *IN30         IFEQ      *OFF
095600980717     C                   Z-ADD     1             XX                3 0
095700980717     C     VIDCLI        LOOKUP    TCL(XX)                                30
095800980717    3C     *IN30         IFEQ      *ON
095900980717     C                   Z-ADD     1             YY
096000980717     C     KCL(XX)       LOOKUP    EDC(YY)                                30
096100980717    3C                   ENDIF
096200980717    2C                   ENDIF
096300980717      * Codice cliente non trovato nemmeno utilizzando la CL
096400980107    2C     *IN30         IFEQ      *OFF
096500980107     C                   SETON                                        284599
096600980107     C                   MOVEL     MSG(7)        VIDMSG
096700980107     C                   GOTO      ENDEDI
096800980107    2C                   ENDIF
096900980107     C* Controllo la serie
097000980107    2C     EDS(YY)       IFNE      VIDNRS
097100980107     C                   SETON                                        285999
097200980107     C                   MOVEL     MSG(8)        VIDMSG
097300980107     C                   GOTO      ENDEDI
097400980107    2C                   ENDIF
097500980108     C* Controllo se già selezionata lnp del cliente
097600980108     C     VIDKLP        LOOKUP    LP                                     32
097700980108    2C     *IN32         IFEQ      *ON
097800980108     C                   SETON                                        284499
097900980108     C                   MOVEL     MSG(16)       VIDMSG
098000980108     C                   GOTO      ENDEDI
098100980108    2C                   ENDIF
098200980107     C*-----------------------------------------------------*
098300980108    1C                   ELSE
098400980107     C* .. altrimenti controllo se è stata selezionata la lnp
098500980108    2C     VIDLNP        IFEQ      0
098600980108     C     UNO           IFEQ      *BLANKS
098700980108     C                   SETON                                        285699
098800980107     C                   MOVEL     MSG(12)       VIDMSG
098900980108     C                   END
099000980107     C                   GOTO      ENDEDI
099100980107    2C                   ENDIF
099200980108     C* Lnp non trovata o non appartiene ad un partner
099300980107     C                   Z-ADD     1             YY
099400980107     C     VIDLNP        LOOKUP    EDL(YY)                                30
099500980108    2C     *IN30         IFEQ      *OFF
099600980108     C                   SETON                                        285699
099700980107     C                   MOVEL     MSG(13)       VIDMSG
099800980107     C                   GOTO      ENDEDI
099900980108    2C                   ENDIF
100000980108     C* Controllo se già selezionata lnp
100100980108     C     VIDLNP        LOOKUP    LP                                     32
100200980108    2C     *IN32         IFEQ      *ON
100300980108     C                   SETON                                        285699
100400980108     C                   MOVEL     MSG(14)       VIDMSG
100500980108     C                   GOTO      ENDEDI
100600980108    2C                   ENDIF
100700980108     C* Controllo se già selezionato cliente con quella lnp
100800980108     C     VIDLNP        LOOKUP    CLP                                    32
100900980108    2C     *IN32         IFEQ      *ON
101000980108     C                   SETON                                        285699
101100980108     C                   MOVEL     MSG(15)       VIDMSG
101200980108     C                   GOTO      ENDEDI
101300980108    2C                   ENDIF
101400980107    1C                   ENDIF
101500980107     C*-----------------------------------------------------*
101600980107     c* CHAIN DSPU PER REPERIRE GLI ALTRI DATI
101700980107     C                   MOVEL     'PU'          COD
101800980107     C                   MOVEL     EDK(YY)       EDKEY
101900980107     C     KEDTAB        CHAIN     EDTAB01L                           30
102000980108    1C     *IN30         IFEQ      *ON
102100980107     C     TABFLG        ORNE      ' '
102200980107     C                   SETON                                        284599
102300980107     C                   MOVEL     MSG(9)        VIDMSG
102400980107     C                   GOTO      ENDEDI
102500980108    1C                   ENDIF
102600980107     C**
102700980107     C                   MOVEL     TABUNI        EDSPU
102800980108    1C     §PUDTA        IFNE      'S'
102900980108    2C     VIDCLI        IFEQ      0
103000980108     C                   SETON                                        285699
103100980108     C                   ELSE
103200980107     C                   SETON                                        284599
103300980108     C                   END
103400980107     C                   MOVEL     MSG(10)       VIDMSG
103500980107     C                   GOTO      ENDEDI
103600980108    1C                   ENDIF
103700980108     C*
103800980108     C                   MOVEL     '1'           UNO               1
103900980108     C*
104000980108     C* Se non ci sono errori carico lnp selez. in schiera di comodo
104100980112     C     VIDKLP        IFEQ      0
104200980108     C                   ADD       1             W
104300980108     C                   Z-ADD     VIDLNP        LP(W)
104400980112     C                   END
104500980108     C* se ho il cliente carico lnp cliente in schiera di comodo
104600980108     C     VIDKLP        IFNE      0
104700980108     C                   ADD       1             WW
104800980108     C                   Z-ADD     VIDKLP        CLP(WW)
104900980108     C                   END
105000920731     C*
105100980107     C     ENDEDI        ENDSR
105200110715
105300110715      *---------------------------------------------------------------*
105400110715      *?Reperimento dati del job (Utente/Operativi)                  ?*
105500110715      *---------------------------------------------------------------*
105600110715     c     sr_DatiJob    BEGSR
105700110715      *
105800110715     c     *dtaara       define    §azute        AZUTEds
105900110715     c     *dtaara       define    §datiute      dDatiUte
106000110715      *
106100110715     c                   in(E)     *dtaara
106200110715     c                   IF        %Error or RSUT = *blanks
106300110715     c                   clear                   TIBS34ds
106400110715     c                   call      'TIBS34R'
106500110715     c                   parm                    TIBS34ds
106600110715     c                   in        *dtaara
106700110715     c                   ENDIF
106800110715      *
106900110715     c                   ENDSR
107000980108**
107100950530 CON SPUNTE  ENTRATA
107200950530 CON SPUNTE PARTENZA
107300961028    PER   E. D. I.
107400961017**  MSG
107500961017Immettere linea di partenza valida                                            1
107600961017Codice Cliente annullato                                                      2
107700961017Codice Cliente errato                                                         3
107800961017Codice Cliente bloccato                                                       4
107900980108Se conferma E.D.I. immettere almeno UNA lin.partenza/cod.cliente/serie        5
108000961028IMPOSSIBILE  ESEGUIRE  QUADRATURA  CON   E . D . I . !!!!                     6
108100961028Il codice cliente immesso non e'  E.D.I.                                      7
108200961028La serie non e' quella assegnata al cliente per la conferma con E.D.I.        8
108300961028Impossibile la trasmissione EDI: manca la tabella "PU": TELEFONARE IN SEDE!!  9
108400980108Impossibile trasmissione EDI: il cliente/Partner non ritorna le date di arrivo10
108500961028Se conferma E.D.I. immettere obbligatoriamente il codice cliente e la serie   1
108600980107Se conferma E.D.I. immettere o codice cliente e serie o linea di partenza     2
108700980107La Linea di partenza non esiste su tabella EDI o non appartiene ad un partner 3
108800980108Se conferma E.D.I. impossibile selez. più volte la stessa linea di partenza   4
108900980108Se conferma E.D.I. impossibile indicare lnp relativa ad un cliente già selez. 5
109000980108Se conferma E.D.I. impossibile indicare cliente di una lnp già selezionata    6
109100060310Serie errata                                                                  7
