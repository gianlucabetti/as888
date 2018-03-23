000100921104     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000200921109?    F* CMZM06R *----------------------------------------------------*
000300921103?    F*---------*                                                    *
000400921109?    F*               - INVIO  DOCUMENTO -                           *
000500921103?    F*               --------------------                           *
000600021015?    F*  per far andare la filiale è rimasto il test per SDI         *
000700021015?    F*  il GAICMZ ha in lista la 201 e quindi parut è della 201     *
000800921103?    F*                                                 AZ.          *
000900921103?    F*--------------------------------------------------------------*
001000921103?    F* I N D I C A T O R I                                          *
001100921103?    F*--------------------------------------------------------------*
001200921103     F*                                                              *
001300921104     F* -01- FLAG    PER GESTIONE LOCK AREA DATI                     *
001400921110     F* -02- FLAG    I° VOLTA                                        *
001500921103     F*                                                              *
001600921112     F* -30- LETTURA CMLSR02L                                        *
001700921109     F* -31- LETTURA CMTSM04L                                        *
001800921110     F* -32- LETTURA CMRGM01L                                        *
001900921112     F* -33- LETTURA AZORG01L                                        *
002000921109     F*                                                              *
002100921109     F*  90 - ERRORE QCMDEXC                                         *
002200921103?    F*
002300921103?    F*--------------------------------------------------------------*
002400921103?    F*  DATA BASE                                                   *
002500921103?    F*--------------------------------------------------------------*
002600921103     F*
002700921103?    F*------------
002800921112     FCMTSM04L  UF A E           K DISK
002900921109?    F*------------
003000921109     FCMRGM01L  IF   E           K DISK
003100921109?    F*------------
003200921112     FCMLSR02L  IF   E           K DISK
003300921109?    F*------------
003400921112     FAZORG01L  IF   E           K DISK
003500921109?    F*------------
003600921109     FINVIO     O    F  199        DISK    USROPN
003700921109     D*
003800921109?    D*------------------------------------------------------------
003900921109?    D*  SCHIERE
004000921109?    D*------------------------------------------------------------
004100921109     D CMD1            S             70    DIM(1) CTDATA PERRCD(1)
004200921109     D CMD2            S             70    DIM(1) CTDATA PERRCD(1)
004300040405     D DOL             S              7  0 DIM(200)
004400921104     D*
004500921104?    D*------------------------------------------------------------
004600921104?    D*  DS
004700921104?    D*------------------------------------------------------------
004800921104      *---
004900930623     D M06UTL          DS
005000930623     D  SYSUTL                 1     10
005100930623     D  USRXXX                 1      3
005200930623     D  REMFIL                 4      6
005300921109?     *-- Definizione dei campi di INVIO
005400921109     D STAMP           DS
005500921110     D  $FCFC                  1      1
005600921110     D  $TIP                   2      2
005700921110     D  $FIR                   3      5  0
005800921110     D  $DSR                   6      8  0
005900921110     D  $UTR                   9     18
006000921110     D  $UTL                  19     28
006100921110     D  $DOR                  29     35  0
006200921110     D  $DOL                  36     42  0
006300921110     D  $DOC                  43     49  0
006400921110     D  $TXT                  50    119
006500921110     D  $DTR                 120    126  0
006600921110     D  $ORR                 127    132  0
006700921110     D  $DTI                 133    139  0
006800921110     D  $ORI                 140    145  0
006900921110     D  $MSG                 146    146
007000921110     D  $STP                 147    147
007100921110     D  $RIS                 148    148
007200921110     D  $MSR                 149    149
007300921110     D  $DTL                 150    156  0
007400921110     D  $DTS                 157    163  0
007500921110     D  $DTP                 164    170  0
007600921110     D  $URG                 171    171
007700921110     D  $URR                 172    172
007800921110     D  $DOS                 173    179  0
007900921110     D  $STS                 180    180
008000921110     D  $TRA                 181    190
008100921112     D  $FIL                 191    193
008200921109     D  TXT                    1    199
008300921111?     *-- Definizione dei campi di CMD3
008400921109     D                 DS
008500921111     D  $MBR3                 47     54
008600921111     D  CMD3                   1    160
008700921111     D                                     DIM(2) CTDATA PERRCD(1)
008800921109      *---
008900921109     D CMZM01        E DS                  EXTNAME(CMZM01PF)
009000921104      *---
009100921109     D CMZ001        E DS                  EXTNAME(CMZ001DS)
009200921109      *---
009300921109     D CMZLDA        E DS                  EXTNAME(CMZLDAPF)
009400921103?     *
009500021113     D TIBS36        E DS                  EXTNAME(TIBS36DS)
009600921103?     *------------------------------------------------------------
009700921103?     *  FLUSSO PRINCIPALE
009800921103?     *------------------------------------------------------------
009900921103?     *
010000921103?    C                   EXSR      INIZIO
010100921103     C*
0102009211091    C     M06LIS        IFNE      *BLANK
010300921109     C*
010400921109     C* Gestione lista di distribuzione.
010500921109     C*
010600921109?    C                   EXSR      GESLIS
010700921109     C*
010800921109     C* Gestione singola filiale.
010900921109     C*
0110009211091E   C                   ELSE
011100921109     C                   MOVE      M06FIL        WRKFIL
011200921112     C     M06FIL        CHAIN     AZORG01L                           33
011300921112     C* Annullato
011400921112     C  N33ORGFVA        COMP      *BLANK                             3333
011500921111     C                   MOVE      *ZERO         WRKDSR
011600921118     C     *IN33         IFEQ      *OFF
011700021004     c* in questo caso va bene ORGFEL perchè il GAICMZ è UNICO
011800921118     C     ORGFEL        ANDNE     0
011900921118     C                   MOVE      ORGFEL        WRKDSR
012000921118?    C                   EXSR      WRTTSM
012100921118?    C                   EXSR      WRTRGM
012200921118C    C                   EXSR      SEND
0123009211091-   C                   END
0124009211181-   C                   END
012500921109     C*
012600921109     C*
012700930617     C                   UNLOCK    CMTSM04L
012800930413     C     FINE          TAG
012900930617     C                   SETON                                            RT
013000921103?     *
013100921103?     *------------------------------------------------------------
013200921103?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
013300921103?     *------------------------------------------------------------
013400921103?     *
013500921103     C*
013600930617?    C     INIZIO        BEGSR
013700921103     C*
013800921103     C     *ENTRY        PLIST
013900921112     C                   PARM                    M06DCR            7 0
014000921109     C                   PARM                    M06FLG            1
014100921109     C                   PARM                    M06FIL            3 0
014200921109     C                   PARM                    M06LIS           10
014300921109     C                   PARM                    M06UTI           10
014400921109     C                   PARM                    M06URG            1
014500921109     C                   PARM                    M06DAT            6 0
014600921109     C                   PARM                    M06ORR            6 0
014700921109     C                   PARM                    M06STP            1
014800921109     C                   PARM                    M06MSG            1
014900921109     C                   PARM                    M06RIS            1
015000921109     C                   PARM                    M06URR            1
015100921109     C                   PARM                    M06MSR            1
015200930623     C                   PARM                    M06UTL           10
015300921103     C*
015400921109     C*
015500921118     C                   Z-ADD     0             I                 3 0
015600921103     C                   MOVE      '0'           *IN
015700921109     C     *LIKE         DEFINE    TSMDSR        SAVDSR
015800921111     C     *LIKE         DEFINE    TSMDOL        SAVDOL
015900921111     C     *LIKE         DEFINE    TSMDOC        SAVDOC
016000921104     C     *DTAARA       DEFINE                  CMZM01
016100921109     C     *DTAARA       DEFINE                  CMZ001
016200921109     C     *DTAARA       DEFINE    *LDA          CMZLDA
016300921111     C                   MOVE      *ZERO         SAVDSR
016400921111     C                   MOVE      *ZERO         SAVDOC
016500921111     C                   MOVE      *ZERO         SAVDOL
016600930617     C                   ENDSR
016700930617     C*
016800930617?    C     *INZSR        BEGSR
016900921109     C*
017000921109     C* Prelevo il codice dell'AS locale.
017100921109     C*
017200921109     C                   SETON                                        01
0173009211092    C     *IN01         DOWEQ     '1'
017400921109     C                   IN        CMZ001                               01
0175009211092-   C                   END
017600921109     C*
017700921109     C* Creo e pulisco file INVIO in QTEMP lungo 199.
017800921109     C*
017900921110     C                   CALL      'QCMDEXC'                            90
018000921109     C                   PARM                    CMD1
018100921109     C                   PARM      70            WLUCMD           15 5
018200921109     C*
018300921110     C                   CALL      'QCMDEXC'                            90
018400921109     C                   PARM                    CMD2
018500921109     C                   PARM      70            WLUCMD
018600921109     C                   OPEN      INVIO
018700921103     C*
018800921103     C                   ENDSR
018900921109?     *
019000921109?     *------------------------------------------------------------
019100921109?     *  G E S L I S   - GESTIONE LISTA DI DISTRIBUZIONE
019200921109?     *------------------------------------------------------------
019300921109?     *
019400921109?    C     GESLIS        BEGSR
019500921109     C*
019600921112     C     M06LIS        SETLL     CMLSR02L
019700921112     C     M06LIS        READE     CMLSR02L                               30
019800921109     C     *IN30         DOWEQ     '0'
019900921118     C                   Z-ADD     LSRDSR        SAVDSR
020000970409     C                   MOVE      *ZERO         I
020100970409     C                   MOVE      *ZERO         DOL
020200921118     C     *IN30         DOWEQ     *OFF
020300921118     C     LSRDSR        ANDEQ     SAVDSR
020400921118     C                   MOVE      LSRFIL        WRKFIL            3 0
020500921118     C                   MOVE      LSRDSR        WRKDSR            3 0
020600921118     C                   EXSR      WRTTSM
020700921118     C     M06LIS        READE     CMLSR02L                               30
020800921118     C                   END
020900921118?    C                   EXSR      WRTRGM
021000921118C    C                   EXSR      SEND
021100921109     C                   END
021200921109     C*
021300921109     C                   ENDSR
021400921109?     *
021500921118?     *------------------------------------------------------------
021600921118?     *  SEND SPEDISCE
021700921118?     *------------------------------------------------------------
021800921118?     *
021900921118?    C     SEND          BEGSR
022000921118     C*
022100921118     C                   CLOSE     INVIO
0222009211182    C     WRKDSR        IFNE      £DST
022300921118?    C                   EXSR      GESNLC
0224009211182E   C                   ELSE
022500921118?    C                   EXSR      GESLCL
0226009211182-   C                   END
022700921118     C*
022800921118     C                   CALL      'QCMDEXC'                            90
022900921118     C                   PARM                    CMD2
023000921118     C                   PARM      70            WLUCMD
023100921118     C                   OPEN      INVIO
023200921118     C                   ENDSR
023300921118     C*
023400921109?     *
023500921109?     *------------------------------------------------------------
023600921118?     *  W R T T S M   - GESTIONE TESTATA
023700921109?     *------------------------------------------------------------
023800921109?     *
023900921118?    C     WRTTSM        BEGSR
024000921109     C*
024100921109     C     M06DCR        CHAIN     CMTSM04L                           31
024200930413     C   31              GOTO      FINE
024300921109     C*
024400921118     C                   MOVE      TSMDOC        SAVDOC
024500921109     C* Se il doc. è in stato <> 9 (non inviato) duplico la testata.
024600921112     C*
024700921109     C*
0248009211132    C     M06FLG        IFEQ      'I'
024900921109     C     TSMSTS        IFNE      '9'
025000921109?    C                   EXSR      DUPTSM
025100921109     C                   END
025200921109     C*
025300921109     C* Se il doc. è in stato 9 (non inviato) aggiorno il file
025400921109     C* delle testate portando lo stato a :
025500921109     C*       3  se TSMDSR <> $DST (cod. AS locale)
025600921109     C*       4  se TSMDSR =  $DST (cod. AS locale)
025700921109     C*
025800921109     C     TSMSTS        IFEQ      '9'
025900921109?    C                   EXSR      AGGTSM
026000921109     C                   END
026100921109     C*
026200921113     C                   END
026300921110     C* Aggiorno file INVIO con rec. di testata.
026400921110     C*
026500921110?    C                   EXSR      AGGINV
026600921118C    C* MEMORIZZO DOL DELLA TESTATA SCRITTA
026700921118     C                   ADD       1             I
026800921118     C                   Z-ADD     TSMDOL        DOL(I)
026900921109     C*
027000930413     C     ENDTSM        ENDSR
027100921109?     *
027200921109?     *------------------------------------------------------------
027300921109?     *  D U P T S M   - DUPLICO LA TESTATA
027400921109?     *------------------------------------------------------------
027500921109?     *
027600921109?    C     DUPTSM        BEGSR
027700921109     C*
027800921109     C* Creo documento, prelevo Numeratore dall dataarea CMZM01.
027900921109     C*
028000921109     C                   SETON                                        01
0281009211091    C     *IN01         DOWEQ     '1'
028200921109     C     *LOCK         IN        CMZM01                               01
0283009211091-   C                   END
028400921109     C                   MOVE      M01NRR        NUMNRR            7 0
028500921109     C                   ADD       1             NUMNRR
028600921109     C                   MOVE      NUMNRR        M01NRR
028700921109     C                   OUT       CMZM01
028800921109     C*
028900921112     C     NUMNRR        CHAIN     CMTSM04L                           31
029000921109     C*
029100921118     C                   MOVE      *ZERO         TSMDOR
029200921118     C                   MOVE      *ZERO         TSMDOS
029300921118     C                   MOVE      *ZERO         TSMDTI
029400921118     C                   MOVE      *ZERO         TSMORI
029500921118     C                   MOVE      *ZERO         TSMDTL
029600921118     C                   MOVE      *ZERO         TSMDTS
029700921112     C                   MOVE      'P'           TSMTIP
029800921111     C                   MOVE      NUMNRR        TSMDOL
029900921112     C                   MOVE      M06UTI        TSMUTR
030000921118     C                   MOVE      WRKFIL        TSMFIR
030100921112     C                   MOVE      WRKDSR        TSMDSR
030200921118     C                   Z-ADD     M06DAT        TSMDTR
030300921112     C                   MOVE      M06ORR        TSMORR
030400921112     C                   MOVE      M06MSG        TSMMSG
030500921112     C                   MOVE      M06STP        TSMSTP
030600921112     C                   MOVE      M06RIS        TSMRIS
030700921112     C                   MOVE      M06MSR        TSMMSR
030800921112     C                   MOVE      M06URG        TSMURG
030900921112     C                   MOVE      M06URR        TSMURR
031000021004     C** INSERIMENTO USO AZUTE
031100021113     C                   CLEAR                   TIBS36
031200021113     C                   MOVEL     sysutl        I36ute
031300021113     C                   MOVEL     'L'           I36TLA
031400021113     C                   CALL      'TIBS36R'                            31
031500021113     C                   PARM                    TIBS36
031600021113     c     o36err        ifeq      *blank
031700021113     c                   movel     O36pou        TSMFIL
031800021004     c                   endif
031900021015     c* pezza per SDI in Filiale
032000021015     C     USRXXX        ifeq      'SDI'
032100021015     C                   MOVE      REMFIL        TSMFIL
032200021015     c                   endif
032300021004     C     TSMFIL        IFle      *zeros
032400921112     C                   MOVE      £DST          TSMFIL
032500930322     C                   END
032600930322     C     WRKDSR        IFNE      £DST
032700921112     C                   MOVE      '3'           TSMSTS
032800921112     C                   ELSE
032900921112     C                   MOVE      '4'           TSMSTS
033000921123     C                   Z-ADD     M06DAT        TSMDTI
033100921123     C                   TIME                    TSMORI
033200921123     C                   END
033300921123     C                   WRITE     CMTSM000
033400921109     C*
033500921109     C                   ENDSR
033600921104?     *
033700921104?     *------------------------------------------------------------
033800921109?     *  A G G T S M   - AGGIORNAMENTO FILE DELLE TESTATE
033900921104?     *------------------------------------------------------------
034000921104?     *
034100921109?    C     AGGTSM        BEGSR
034200921109     C*
034300921109     C* Se il doc. è in stato 9 (non inviato) aggiorno il file
034400921109     C* delle testate portando lo stato a :
034500921109     C*       3  se TSMDSR <> $DST (cod. AS locale)
034600921109     C*       4  se TSMDSR =  $DST (cod. AS locale)
034700921104     C*
0348009211091    C     *IN31         IFEQ      '0'
034900921109     C                   MOVE      M06UTI        TSMUTR
035000921118     C                   MOVE      WRKFIL        TSMFIR
035100921109     C                   MOVE      WRKDSR        TSMDSR
035200921118     C                   Z-ADD     M06DAT        TSMDTR
035300921109     C                   MOVE      M06ORR        TSMORR
035400921109     C                   MOVE      M06MSG        TSMMSG
035500921109     C                   MOVE      M06STP        TSMSTP
035600921109     C                   MOVE      M06RIS        TSMRIS
035700921109     C                   MOVE      M06MSR        TSMMSR
035800921109     C                   MOVE      M06URG        TSMURG
035900921109     C                   MOVE      M06URR        TSMURR
036000021004     C** INSERIMENTO USO AZUTE
036100021113     C                   CLEAR                   TIBS36
036200021113     C                   MOVEL     sysutl        I36ute
036300021113     C                   MOVEL     'L'           I36TLA
036400021113     C                   CALL      'TIBS36R'                            31
036500021113     C                   PARM                    TIBS36
036600021113     c     o36err        ifeq      *blank
036700021113     c                   movel     O36pou        TSMFIL
036800021004     c                   endif
036900021015     c* pezza per SDI in Filiale
037000021015     C     USRXXX        ifeq      'SDI'
037100021015     C                   MOVE      REMFIL        TSMFIL
037200021015     c                   endif
037300021004     C     TSMFIL        IFle      *zeros
037400021004     C                   MOVE      £DST          TSMFIL
037500021004     C                   END
037600930322     C     WRKDSR        IFNE      £DST
037700921109     C                   MOVE      '3'           TSMSTS
037800921109     C                   ELSE
037900921109     C                   MOVE      '4'           TSMSTS
038000921118     C                   Z-ADD     M06DAT        TSMDTI
038100921118     C                   TIME                    TSMORI
038200921109     C                   END
038300921109     C                   UPDATE    CMTSM000
0384009211041-   C                   END
038500921104     C*
038600921104     C                   ENDSR
038700921110?     *
038800921110?     *------------------------------------------------------------
038900921110?     *  A G G I N V   - AGGIORNO FILE INVIO CON REC. DI TESTATA
039000921110?     *------------------------------------------------------------
039100921110?     *
039200921110?    C     AGGINV        BEGSR
039300921110     C*
039400921110     C                   MOVE      *BLANK        TXT
039500921110     C*
039600921110     C                   MOVE      '*'           $FCFC
039700921110     C                   MOVE      TSMTIP        $TIP
039800921110     C                   MOVE      TSMFIR        $FIR
039900921112     C                   MOVE      TSMFIL        $FIL
040000921110     C                   MOVE      TSMDSR        $DSR
040100921110     C                   MOVE      TSMUTR        $UTR
040200921112     C                   MOVE      SYSUTL        $UTL
040300921110     C                   MOVE      TSMDOR        $DOR
040400921110     C                   MOVE      TSMDOL        $DOL
040500921110     C                   MOVE      TSMDOC        $DOC
040600921110     C                   MOVE      TSMTXT        $TXT
040700921110     C                   MOVE      TSMDTR        $DTR
040800921110     C                   MOVE      TSMORR        $ORR
040900921110     C                   MOVE      TSMDTI        $DTI
041000921110     C                   MOVE      TSMORI        $ORI
041100921110     C                   MOVE      TSMMSG        $MSG
041200921110     C                   MOVE      TSMSTP        $STP
041300921110     C                   MOVE      TSMRIS        $RIS
041400921110     C                   MOVE      TSMMSR        $MSR
041500921110     C                   MOVE      TSMDTL        $DTL
041600921110     C                   MOVE      TSMDTS        $DTS
041700921110     C                   MOVE      TSMDTP        $DTP
041800921110     C                   MOVE      TSMURG        $URG
041900921110     C                   MOVE      TSMURR        $URR
042000921110     C                   MOVE      TSMDOS        $DOS
042100921110     C                   MOVE      TSMSTS        $STS
042200921110     C                   MOVE      TSMTRA        $TRA
042300921110     C*
042400921110?    C                   WRITE     INVIO         STAMP
042500921110     C*
042600921110     C                   ENDSR
042700921109?     *
042800921109?     *------------------------------------------------------------
042900921109?     *  G E S R G M   - GESTIONE RIGHE
043000921109?     *------------------------------------------------------------
043100921109?     *
043200921118?    C     WRTRGM        BEGSR
043300921109     C*
043400921118     C     M06FLG        IFEQ      'I'
043500921111     C     SAVDOC        SETLL     CMRGM01L
043600921111     C     SAVDOC        READE     CMRGM01L                               32
0437009211101    C     *IN32         DOWEQ     '0'
043800921109     C*
043900921109     C                   MOVEL     RGMFCC        TXT
044000921109     C                   MOVE      RGMRIG        TXT
044100921109?    C                   WRITE     INVIO         STAMP
044200921109     C*
044300921111     C     SAVDOC        READE     CMRGM01L                               32
0444009211101-   C                   END
0445009211181-   C                   END
044600921109     C*
044700921109     C                   ENDSR
044800921109?     *
044900921109?     *------------------------------------------------------------
045000921109?     *  G E S N L C   - GESTIONE SPEDIZIONE PER FILIALI NON LOCALI
045100921109?     *------------------------------------------------------------
045200921109?     *
045300921109?    C     GESNLC        BEGSR
045400921109     C*
045500921112     C                   MOVEL     WRKDSR        PDST
045600921112     C                   MOVEL     M06ORR        PORI
045700921118     C                   MOVE      M06DAT        PDTI
045800930323     C                   CALL      'CMZ002R'
045900921112     C                   PARM                    PDST              3
046000921109     C                   PARM      'INVIO'       PFLD             10
046100921109     C                   PARM      'INVIO'       PMBO             10
046200921109     C                   PARM      'QTEMP'       PLBD             10
046300921113     C                   PARM      'CMRCM00F'    PFLA             10
046400921109     C                   PARM      '*GEN'        PMBA             10
046500921113     C                   PARM      'GAICMZ'      PLBA             10
046600921112     C                   PARM                    PORI              4
046700921112     C                   PARM                    PDTI              7
046800921109     C                   PARM      M06URG        PURG              1
046900921109     C                   PARM      'R'           PADD              1
047000931007     C                   PARM      '1'           PCLR              1
047100921109     C                   PARM      'S'           PFTR              1
047200921109     C                   PARM      ' '           PRRR              1
047300921109     C                   PARM      '0   '        PAZN              4
047400921109     C                   PARM      'CMZM07R'     PSYF             10
047500921109     C                   PARM      *BLANK        PRRC              7
047600921109     C                   PARM      *BLANK        PPRR             10
047700921109     C*
047800921109     C* Riferimento Spedizione
047900921109     C*
048000040405     C                   DO        199           I
048100921118     C     DOL(I)        IFNE      0
048200921118     C     DOL(I)        CHAIN     CMTSM04L                           31
048300921109     C                   MOVE      PPRR          TSMTRA
048400930511     C  N31              UPDATE    CMTSM000
048500921118     C                   END
048600921118     C     DOL(I)        IFEQ      0
048700921118     C                   LEAVE
048800921118     C                   END
048900921118     C                   END
049000921109     C*
049100921109     C                   ENDSR
049200921109?     *
049300921109?     *------------------------------------------------------------
049400921109?     *  G E S L C L   - GESTIONE SPEDIZIONE PER FILIALI     LOCALI
049500921109?     *------------------------------------------------------------
049600921109?     *
049700921109?    C     GESLCL        BEGSR
049800921109     C*
049900921109     C* Imposto parametri nella lcldtaara
050000921109     C*
050100921109     C                   SETON                                        01
0502009211092    C     *IN01         DOWEQ     '1'
050300921109     C                   IN        CMZLDA                               01
0504009211092-   C                   END
050500921110     C                   MOVE      *BLANK        LDALIB
050600921110     C                   MOVE      *BLANK        LDAFIL
050700921110     C                   MOVE      *BLANK        LDAMBR
050800921112     C                   MOVEL     '*LIBL '      LDALIB
050900921110     C                   MOVEL     'CMRCM00F'    LDAFIL
051000921109     C                   MOVEL     'M'           WA8               8
051100921109     C                   MOVE      TSMDOL        WA8
051200921110     C                   MOVEL     WA8           LDAMBR
051300921109     C                   OUT       CMZLDA
051400921109     C*
051500921109     C* Aggiungo il documento al file CMRCM00F
051600921109     C*
051700921111     C                   MOVEL     WA8           $MBR3
051800921111     C                   CALL      'QCMDEXC'
051900921111     C                   PARM                    CMD3
052000921110     C                   PARM      160           WLUCMD
052100921109     C*
052200921109     C                   CALL      'CMZM07R'
052300921109     C*
052400921109     C                   ENDSR
052500921109     C**************************************************************
052600970409     O*..1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
052700921111**CTDATA CMD1
052800921109CRTPF  FILE(QTEMP/INVIO) RCDLEN(199) LVLCHK(*NO)
052900921109**CTDATA CMD2
053000921109CLRPFM FILE(QTEMP/INVIO)
053100921111**CTDATA CMD3
053200921111CPYF FROMFILE(INVIO) TOFILE(CMRCM00F)   TOMBR(        ) MBROPT(*REPLACE) FMTOPT(
053300921111*NOCHK)
