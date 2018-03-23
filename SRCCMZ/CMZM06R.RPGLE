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
002800051207     fCMTSM03L  if   e           k disk    rename(CMTSM000:CMTSM003)
002900921112     FCMTSM04L  UF A E           K DISK
003000921109?    F*------------
003100921109     FCMRGM01L  IF   E           K DISK
003200921109?    F*------------
003300921112     FCMLSR02L  IF   E           K DISK
003400921109?    F*------------
003500921112     FAZORG01L  IF   E           K DISK
003600921109?    F*------------
003700921109     FINVIO     O    F  199        DISK    USROPN
003800921109     D*
003900921109?    D*------------------------------------------------------------
004000921109?    D*  SCHIERE
004100921109?    D*------------------------------------------------------------
004200921109     D CMD1            S             70    DIM(1) CTDATA PERRCD(1)
004300921109     D CMD2            S             70    DIM(1) CTDATA PERRCD(1)
004400040405     D DOL             S              7  0 DIM(200)
004500921104     D*
004600921104?    D*------------------------------------------------------------
004700921104?    D*  DS
004800921104?    D*------------------------------------------------------------
004900921104      *---
005000930623     D M06UTL          DS
005100930623     D  SYSUTL                 1     10
005200930623     D  USRXXX                 1      3
005300930623     D  REMFIL                 4      6
005400921109?     *-- Definizione dei campi di INVIO
005500921109     D STAMP           DS
005600921110     D  $FCFC                  1      1
005700921110     D  $TIP                   2      2
005800921110     D  $FIR                   3      5  0
005900921110     D  $DSR                   6      8  0
006000921110     D  $UTR                   9     18
006100921110     D  $UTL                  19     28
006200921110     D  $DOR                  29     35  0
006300921110     D  $DOL                  36     42  0
006400921110     D  $DOC                  43     49  0
006500921110     D  $TXT                  50    119
006600921110     D  $DTR                 120    126  0
006700921110     D  $ORR                 127    132  0
006800921110     D  $DTI                 133    139  0
006900921110     D  $ORI                 140    145  0
007000921110     D  $MSG                 146    146
007100921110     D  $STP                 147    147
007200921110     D  $RIS                 148    148
007300921110     D  $MSR                 149    149
007400921110     D  $DTL                 150    156  0
007500921110     D  $DTS                 157    163  0
007600921110     D  $DTP                 164    170  0
007700921110     D  $URG                 171    171
007800921110     D  $URR                 172    172
007900921110     D  $DOS                 173    179  0
008000921110     D  $STS                 180    180
008100921110     D  $TRA                 181    190
008200921112     D  $FIL                 191    193
008300921109     D  TXT                    1    199
008400921111?     *-- Definizione dei campi di CMD3
008500921109     D                 DS
008600921111     D  $MBR3                 47     54
008700921111     D  CMD3                   1    160
008800921111     D                                     DIM(2) CTDATA PERRCD(1)
008900921109      *---
009000921109     D CMZM01        E DS                  EXTNAME(CMZM01PF)
009100921104      *---
009200921109     D CMZ001        E DS                  EXTNAME(CMZ001DS)
009300921109      *---
009400921109     D CMZLDA        E DS                  EXTNAME(CMZLDAPF)
009500921103?     *
009600021113     D TIBS36        E DS                  EXTNAME(TIBS36DS)
009700921103?     *------------------------------------------------------------
009800921103?     *  FLUSSO PRINCIPALE
009900921103?     *------------------------------------------------------------
010000921103?     *
010100921103?    C                   EXSR      INIZIO
010200921103     C*
0103009211091    C     M06LIS        IFNE      *BLANK
010400921109     C*
010500921109     C* Gestione lista di distribuzione.
010600921109     C*
010700921109?    C                   EXSR      GESLIS
010800921109     C*
010900921109     C* Gestione singola filiale.
011000921109     C*
0111009211091E   C                   ELSE
011200921109     C                   MOVE      M06FIL        WRKFIL
011300921112     C     M06FIL        CHAIN     AZORG01L                           33
011400921112     C* Annullato
011500921112     C  N33ORGFVA        COMP      *BLANK                             3333
011600921111     C                   MOVE      *ZERO         WRKDSR
011700921118     C     *IN33         IFEQ      *OFF
011800021004     c* in questo caso va bene ORGFEL perchè il GAICMZ è UNICO
011900921118     C     ORGFEL        ANDNE     0
012000921118     C                   MOVE      ORGFEL        WRKDSR
012100921118?    C                   EXSR      WRTTSM
012200921118?    C                   EXSR      WRTRGM
012300921118C    C                   EXSR      SEND
0124009211091-   C                   END
0125009211181-   C                   END
012600921109     C*
012700921109     C*
012800930617     C                   UNLOCK    CMTSM04L
012900930413     C     FINE          TAG
013000930617     C                   SETON                                            RT
013100921103?     *
013200921103?     *------------------------------------------------------------
013300921103?     *  INIZIALIZZAZIONE CAMPI DI LAVORO E DEFINIZIONE CHIAVI
013400921103?     *------------------------------------------------------------
013500921103?     *
013600921103     C*
013700930617?    C     INIZIO        BEGSR
013800921103     C*
013900921103     C     *ENTRY        PLIST
014000921112     C                   PARM                    M06DCR            7 0
014100921109     C                   PARM                    M06FLG            1
014200921109     C                   PARM                    M06FIL            3 0
014300921109     C                   PARM                    M06LIS           10
014400921109     C                   PARM                    M06UTI           10
014500921109     C                   PARM                    M06URG            1
014600921109     C                   PARM                    M06DAT            6 0
014700921109     C                   PARM                    M06ORR            6 0
014800921109     C                   PARM                    M06STP            1
014900921109     C                   PARM                    M06MSG            1
015000921109     C                   PARM                    M06RIS            1
015100921109     C                   PARM                    M06URR            1
015200921109     C                   PARM                    M06MSR            1
015300930623     C                   PARM                    M06UTL           10
015400921103     C*
015500921109     C*
015600921118     C                   Z-ADD     0             I                 3 0
015700921103     C                   MOVE      '0'           *IN
015800921109     C     *LIKE         DEFINE    TSMDSR        SAVDSR
015900921111     C     *LIKE         DEFINE    TSMDOC        SAVDOC
016000921104     C     *DTAARA       DEFINE                  CMZM01
016100921109     C     *DTAARA       DEFINE                  CMZ001
016200921109     C     *DTAARA       DEFINE    *LDA          CMZLDA
016300921111     C                   MOVE      *ZERO         SAVDSR
016400921111     C                   MOVE      *ZERO         SAVDOC
016500930617     C                   ENDSR
016600930617     C*
016700930617?    C     *INZSR        BEGSR
016800921109     C*
016900921109     C* Prelevo il codice dell'AS locale.
017000921109     C*
017100921109     C                   SETON                                        01
0172009211092    C     *IN01         DOWEQ     '1'
017300921109     C                   IN        CMZ001                               01
0174009211092-   C                   END
017500921109     C*
017600921109     C* Creo e pulisco file INVIO in QTEMP lungo 199.
017700921109     C*
017800921110     C                   CALL      'QCMDEXC'                            90
017900921109     C                   PARM                    CMD1
018000921109     C                   PARM      70            WLUCMD           15 5
018100921109     C*
018200921110     C                   CALL      'QCMDEXC'                            90
018300921109     C                   PARM                    CMD2
018400921109     C                   PARM      70            WLUCMD
018500921109     C                   OPEN      INVIO
018600921103     C*
018700921103     C                   ENDSR
018800921109?     *
018900921109?     *------------------------------------------------------------
019000921109?     *  G E S L I S   - GESTIONE LISTA DI DISTRIBUZIONE
019100921109?     *------------------------------------------------------------
019200921109?     *
019300921109?    C     GESLIS        BEGSR
019400921109     C*
019500921112     C     M06LIS        SETLL     CMLSR02L
019600921112     C     M06LIS        READE     CMLSR02L                               30
019700921109     C     *IN30         DOWEQ     '0'
019800921118     C                   Z-ADD     LSRDSR        SAVDSR
019900970409     C                   MOVE      *ZERO         I
020000970409     C                   MOVE      *ZERO         DOL
020100921118     C     *IN30         DOWEQ     *OFF
020200921118     C     LSRDSR        ANDEQ     SAVDSR
020300921118     C                   MOVE      LSRFIL        WRKFIL            3 0
020400921118     C                   MOVE      LSRDSR        WRKDSR            3 0
020500921118     C                   EXSR      WRTTSM
020600921118     C     M06LIS        READE     CMLSR02L                               30
020700921118     C                   END
020800921118?    C                   EXSR      WRTRGM
020900921118C    C                   EXSR      SEND
021000921109     C                   END
021100921109     C*
021200921109     C                   ENDSR
021300921109?     *
021400921118?     *------------------------------------------------------------
021500921118?     *  SEND SPEDISCE
021600921118?     *------------------------------------------------------------
021700921118?     *
021800921118?    C     SEND          BEGSR
021900921118     C*
022000921118     C                   CLOSE     INVIO
0221009211182    C     WRKDSR        IFNE      £DST
022200921118?    C                   EXSR      GESNLC
0223009211182E   C                   ELSE
022400921118?    C                   EXSR      GESLCL
0225009211182-   C                   END
022600921118     C*
022700921118     C                   CALL      'QCMDEXC'                            90
022800921118     C                   PARM                    CMD2
022900921118     C                   PARM      70            WLUCMD
023000921118     C                   OPEN      INVIO
023100921118     C                   ENDSR
023200921118     C*
023300921109?     *
023400921109?     *------------------------------------------------------------
023500921118?     *  W R T T S M   - GESTIONE TESTATA
023600921109?     *------------------------------------------------------------
023700921109?     *
023800921118?    C     WRTTSM        BEGSR
023900921109     C*
024000921109     C     M06DCR        CHAIN     CMTSM04L                           31
024100930413     C   31              GOTO      FINE
024200921109     C*
024300921118     C                   MOVE      TSMDOC        SAVDOC
024400921109     C* Se il doc. è in stato <> 9 (non inviato) duplico la testata.
024500921112     C*
024600921109     C*
0247009211132    C     M06FLG        IFEQ      'I'
024800921109     C     TSMSTS        IFNE      '9'
024900921109?    C                   EXSR      DUPTSM
025000921109     C                   END
025100921109     C*
025200921109     C* Se il doc. è in stato 9 (non inviato) aggiorno il file
025300921109     C* delle testate portando lo stato a :
025400921109     C*       3  se TSMDSR <> $DST (cod. AS locale)
025500921109     C*       4  se TSMDSR =  $DST (cod. AS locale)
025600921109     C*
025700921109     C     TSMSTS        IFEQ      '9'
025800921109?    C                   EXSR      AGGTSM
025900921109     C                   END
026000921109     C*
026100921113     C                   END
026200921110     C* Aggiorno file INVIO con rec. di testata.
026300921110     C*
026400921110?    C                   EXSR      AGGINV
026500921118C    C* MEMORIZZO DOL DELLA TESTATA SCRITTA
026600921118     C                   ADD       1             I
026700921118     C                   Z-ADD     TSMDOL        DOL(I)
026800921109     C*
026900930413     C     ENDTSM        ENDSR
027000921109?     *
027100921109?     *------------------------------------------------------------
027200921109?     *  D U P T S M   - DUPLICO LA TESTATA
027300921109?     *------------------------------------------------------------
027400921109?     *
027500921109?    C     DUPTSM        BEGSR
027600921109     C*
027700921109     C* Creo documento, prelevo Numeratore dall dataarea CMZM01.
027800921109     C*
027900921109     C                   SETON                                        01
0280009211091    C     *IN01         DOWEQ     '1'
028100921109     C     *LOCK         IN        CMZM01                               01
0282009211091-   C                   END
028300921109     C                   MOVE      M01NRR        NUMNRR            7 0
028400051207     ***C                   ADD       1             NUMNRR
028500051207     c                   exsr      Chk_Free
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
033300051207      *
033400921123     C                   WRITE     CMTSM000
033500921109     C*
033600921109     C                   ENDSR
033700051207      *
033800051207?     *------------------------------------------------------------
033900051207?     * CHK_FREE - Ricerca primo progressivo libero
034000051207?     *------------------------------------------------------------
034100051207      *
034200051207     c     Chk_Free      BEGSR
034300051207      *
034400051207do  1c                   DO        *hival
034500051207      *
034600051207do  2c                   DOU       NOT %equal(CMTSM04L)
034700051207     c                   add       1             NumNRR
034800051207if  3c                   if        NumNRR <= *zeros
034900051207     c                   eval      NumNRR =  1
035000051207e   3c                   endif
035100051207     c     NumNRR        setll     CMTSM000
035200051207e   2c                   ENDDO
035300051207      *
035400051207     c     NumNRR        setll     CMTSM003
035500051207if  2c                   if        NOT %equal(CMTSM03L)
035600051207     c                   leave
035700051207e   2c                   endif
035800051207      *
035900051207e   1c                   ENDDO
036000051207      *
036100051207     c                   ENDSR
036200921104?     *
036300921104?     *------------------------------------------------------------
036400921109?     *  A G G T S M   - AGGIORNAMENTO FILE DELLE TESTATE
036500921104?     *------------------------------------------------------------
036600921104?     *
036700921109?    C     AGGTSM        BEGSR
036800921109     C*
036900921109     C* Se il doc. è in stato 9 (non inviato) aggiorno il file
037000921109     C* delle testate portando lo stato a :
037100921109     C*       3  se TSMDSR <> $DST (cod. AS locale)
037200921109     C*       4  se TSMDSR =  $DST (cod. AS locale)
037300921104     C*
0374009211091    C     *IN31         IFEQ      '0'
037500921109     C                   MOVE      M06UTI        TSMUTR
037600921118     C                   MOVE      WRKFIL        TSMFIR
037700921109     C                   MOVE      WRKDSR        TSMDSR
037800921118     C                   Z-ADD     M06DAT        TSMDTR
037900921109     C                   MOVE      M06ORR        TSMORR
038000921109     C                   MOVE      M06MSG        TSMMSG
038100921109     C                   MOVE      M06STP        TSMSTP
038200921109     C                   MOVE      M06RIS        TSMRIS
038300921109     C                   MOVE      M06MSR        TSMMSR
038400921109     C                   MOVE      M06URG        TSMURG
038500921109     C                   MOVE      M06URR        TSMURR
038600021004     C** INSERIMENTO USO AZUTE
038700021113     C                   CLEAR                   TIBS36
038800021113     C                   MOVEL     sysutl        I36ute
038900021113     C                   MOVEL     'L'           I36TLA
039000021113     C                   CALL      'TIBS36R'                            31
039100021113     C                   PARM                    TIBS36
039200021113     c     o36err        ifeq      *blank
039300021113     c                   movel     O36pou        TSMFIL
039400021004     c                   endif
039500021015     c* pezza per SDI in Filiale
039600021015     C     USRXXX        ifeq      'SDI'
039700021015     C                   MOVE      REMFIL        TSMFIL
039800021015     c                   endif
039900021004     C     TSMFIL        IFle      *zeros
040000021004     C                   MOVE      £DST          TSMFIL
040100021004     C                   END
040200930322     C     WRKDSR        IFNE      £DST
040300921109     C                   MOVE      '3'           TSMSTS
040400921109     C                   ELSE
040500921109     C                   MOVE      '4'           TSMSTS
040600921118     C                   Z-ADD     M06DAT        TSMDTI
040700921118     C                   TIME                    TSMORI
040800921109     C                   END
040900921109     C                   UPDATE    CMTSM000
0410009211041-   C                   END
041100921104     C*
041200921104     C                   ENDSR
041300921110?     *
041400921110?     *------------------------------------------------------------
041500921110?     *  A G G I N V   - AGGIORNO FILE INVIO CON REC. DI TESTATA
041600921110?     *------------------------------------------------------------
041700921110?     *
041800921110?    C     AGGINV        BEGSR
041900921110     C*
042000921110     C                   MOVE      *BLANK        TXT
042100921110     C*
042200921110     C                   MOVE      '*'           $FCFC
042300921110     C                   MOVE      TSMTIP        $TIP
042400921110     C                   MOVE      TSMFIR        $FIR
042500921112     C                   MOVE      TSMFIL        $FIL
042600921110     C                   MOVE      TSMDSR        $DSR
042700921110     C                   MOVE      TSMUTR        $UTR
042800921112     C                   MOVE      SYSUTL        $UTL
042900921110     C                   MOVE      TSMDOR        $DOR
043000921110     C                   MOVE      TSMDOL        $DOL
043100921110     C                   MOVE      TSMDOC        $DOC
043200921110     C                   MOVE      TSMTXT        $TXT
043300921110     C                   MOVE      TSMDTR        $DTR
043400921110     C                   MOVE      TSMORR        $ORR
043500921110     C                   MOVE      TSMDTI        $DTI
043600921110     C                   MOVE      TSMORI        $ORI
043700921110     C                   MOVE      TSMMSG        $MSG
043800921110     C                   MOVE      TSMSTP        $STP
043900921110     C                   MOVE      TSMRIS        $RIS
044000921110     C                   MOVE      TSMMSR        $MSR
044100921110     C                   MOVE      TSMDTL        $DTL
044200921110     C                   MOVE      TSMDTS        $DTS
044300921110     C                   MOVE      TSMDTP        $DTP
044400921110     C                   MOVE      TSMURG        $URG
044500921110     C                   MOVE      TSMURR        $URR
044600921110     C                   MOVE      TSMDOS        $DOS
044700921110     C                   MOVE      TSMSTS        $STS
044800921110     C                   MOVE      TSMTRA        $TRA
044900921110     C*
045000921110?    C                   WRITE     INVIO         STAMP
045100921110     C*
045200921110     C                   ENDSR
045300921109?     *
045400921109?     *------------------------------------------------------------
045500921109?     *  G E S R G M   - GESTIONE RIGHE
045600921109?     *------------------------------------------------------------
045700921109?     *
045800921118?    C     WRTRGM        BEGSR
045900921109     C*
046000921118     C     M06FLG        IFEQ      'I'
046100921111     C     SAVDOC        SETLL     CMRGM01L
046200921111     C     SAVDOC        READE     CMRGM01L                               32
0463009211101    C     *IN32         DOWEQ     '0'
046400921109     C*
046500921109     C                   MOVEL     RGMFCC        TXT
046600921109     C                   MOVE      RGMRIG        TXT
046700921109?    C                   WRITE     INVIO         STAMP
046800921109     C*
046900921111     C     SAVDOC        READE     CMRGM01L                               32
0470009211101-   C                   END
0471009211181-   C                   END
047200921109     C*
047300921109     C                   ENDSR
047400921109?     *
047500921109?     *------------------------------------------------------------
047600921109?     *  G E S N L C   - GESTIONE SPEDIZIONE PER FILIALI NON LOCALI
047700921109?     *------------------------------------------------------------
047800921109?     *
047900921109?    C     GESNLC        BEGSR
048000921109     C*
048100921112     C                   MOVEL     WRKDSR        PDST
048200921112     C                   MOVEL     M06ORR        PORI
048300921118     C                   MOVE      M06DAT        PDTI
048400930323     C                   CALL      'CMZ002R'
048500921112     C                   PARM                    PDST              3
048600921109     C                   PARM      'INVIO'       PFLD             10
048700921109     C                   PARM      'INVIO'       PMBO             10
048800921109     C                   PARM      'QTEMP'       PLBD             10
048900921113     C                   PARM      'CMRCM00F'    PFLA             10
049000921109     C                   PARM      '*GEN'        PMBA             10
049100921113     C                   PARM      'GAICMZ'      PLBA             10
049200921112     C                   PARM                    PORI              4
049300921112     C                   PARM                    PDTI              7
049400921109     C                   PARM      M06URG        PURG              1
049500921109     C                   PARM      'R'           PADD              1
049600931007     C                   PARM      '1'           PCLR              1
049700921109     C                   PARM      'S'           PFTR              1
049800921109     C                   PARM      ' '           PRRR              1
049900921109     C                   PARM      '0   '        PAZN              4
050000921109     C                   PARM      'CMZM07R'     PSYF             10
050100921109     C                   PARM      *BLANK        PRRC              7
050200921109     C                   PARM      *BLANK        PPRR             10
050300921109     C*
050400921109     C* Riferimento Spedizione
050500921109     C*
050600040405     C                   DO        199           I
050700921118     C     DOL(I)        IFNE      0
050800921118     C     DOL(I)        CHAIN     CMTSM04L                           31
050900921109     C                   MOVE      PPRR          TSMTRA
051000930511     C  N31              UPDATE    CMTSM000
051100921118     C                   END
051200921118     C     DOL(I)        IFEQ      0
051300921118     C                   LEAVE
051400921118     C                   END
051500921118     C                   END
051600921109     C*
051700921109     C                   ENDSR
051800921109?     *
051900921109?     *------------------------------------------------------------
052000921109?     *  G E S L C L   - GESTIONE SPEDIZIONE PER FILIALI     LOCALI
052100921109?     *------------------------------------------------------------
052200921109?     *
052300921109?    C     GESLCL        BEGSR
052400921109     C*
052500921109     C* Imposto parametri nella lcldtaara
052600921109     C*
052700921109     C                   SETON                                        01
0528009211092    C     *IN01         DOWEQ     '1'
052900921109     C                   IN        CMZLDA                               01
0530009211092-   C                   END
053100921110     C                   MOVE      *BLANK        LDALIB
053200921110     C                   MOVE      *BLANK        LDAFIL
053300921110     C                   MOVE      *BLANK        LDAMBR
053400921112     C                   MOVEL     '*LIBL '      LDALIB
053500921110     C                   MOVEL     'CMRCM00F'    LDAFIL
053600921109     C                   MOVEL     'M'           WA8               8
053700921109     C                   MOVE      TSMDOL        WA8
053800921110     C                   MOVEL     WA8           LDAMBR
053900921109     C                   OUT       CMZLDA
054000921109     C*
054100921109     C* Aggiungo il documento al file CMRCM00F
054200921109     C*
054300921111     C                   MOVEL     WA8           $MBR3
054400921111     C                   CALL      'QCMDEXC'
054500921111     C                   PARM                    CMD3
054600921110     C                   PARM      160           WLUCMD
054700921109     C*
054800921109     C                   CALL      'CMZM07R'
054900921109     C*
055000921109     C                   ENDSR
055100921109     C**************************************************************
055200970409     O*..1....+....2....+....3....+....4....+....5....+....6....+....7....+....8
055300921111**CTDATA CMD1
055400921109CRTPF  FILE(QTEMP/INVIO) RCDLEN(199) LVLCHK(*NO)
055500921109**CTDATA CMD2
055600921109CLRPFM FILE(QTEMP/INVIO)
055700921111**CTDATA CMD3
055800921111CPYF FROMFILE(INVIO) TOFILE(CMRCM00F)   TOMBR(        ) MBROPT(*REPLACE) FMTOPT(
055900921111*NOCHK)
