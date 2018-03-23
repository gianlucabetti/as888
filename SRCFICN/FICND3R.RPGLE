000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011114     H* FICND3R *----------------------------------------------------*
000300931129     H*         CREAZIONE FILE DI SIMULAZIONE PADRONCINI  - FILTRO -
000400000000     H*--------------------------------------------------------------*
000500011114     FFICND3D   CF   E             WORKSTN
000600931015     FTABEL00F  IF   E           K DISK
000700021202     Ffiapd01L  IF   E           K DISK
000800020918     FFNFVV08L  IF   E           K DISK
000900020918     FFNARB78L  IF   E           K DISK
001000950125     FFNBLP34L  IF   E           K DISK
001100011114     Ffiftt04L  IF   E           K DISK
001200930707     F*
001300950125     D* CPD --> CONTIENE TUTTI I PADRONCINI PRESENTI IN ANAGRAFICA
001400061010     D CPD             S              7  0 DIM(10999)
001500940623     D* CP2 --> CONTIENE I PADRONCINI ESISTENTI NEL RANGE RICHIESTO
001600061010     D CP2             S              7  0 DIM(10999)
001700950125     D* SCHIERE PER CARICAMENTO L1 E L6
001800950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001900950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
002000950125     D* MESSAGGI A VIDEO
002100940613     D ERR             S             55    DIM(5) CTDATA PERRCD(1)              MSG ERRORE
002200950125     D*
002300020918     D wfgs            S                   like(arbifp)                         FIL GESTITE
002400011114     D* PASSAGGIO DATI ALL'ELABORAZIONE BATCH             - FICND4R -
002500931129     D PARAM           DS
002600931129     D  VI1TSR                 1      1
002700931129     D  VI1TVL                 2      2
002800931129     D  VI1PF1                 3      5  0
002900931129     D  VI1PD1                 6      9
003000931129     D  PARCPD                 3      9  0
003100931129     D  VI1PF2                10     12  0
003200931129     D  VI1PD2                13     16
003300931129     D  PARCPA                10     16  0
003400950125     D  W01DD1                17     24  0
003500950125     D  W01DD2                25     32  0
003600931129     D*
003700910423     D WLBDAT          DS
003800950125     D  G02DAT                 1      8  0
003900950125     D  G02INV                 9     16  0
004000950125     D  G02ERR                17     17
004100950125     D  G02TGI                18     22  0
004200940613     D WGIDAT          DS
004300950125     D  GIODAT                 1      8  0
004400950125     D  GIOINV                 9     16  0
004500950125     D  GIOTGI                17     21  0
004600940613     D                 DS
004700950125     D  AA                     1      4  0
004800950125     D  MM                     5      6  0
004900950125     D  GG                     7      8  0
005000950125     D  DATA                   1      8  0
005100931015     D                 DS
005200931129     D  COMPF1                 1      3  0
005300950125     D  COMPD1                 4      7  0
005400950125     D  WPDAL                  1      7  0
005500950125     D                 DS
005600950125     D  COMPF2                 1      3  0
005700950125     D  COMPD2                 4      7  0
005800950125     D  WPAL                   1      7  0
005900931129     D KPJBA         E DS
006000950125     D* DS PER TRUL06R - CARICAMENTO £1
006100950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006200950125     D  LIN                    1     90  0
006300950125     D                                     DIM(30)                              SKI COMODO
006400931129     D DS1Z          E DS
006500931021     D CNCR80        E DS
006600021202     D fnlv24ds      E DS
006700900615     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006800000000     I/SPACE 3
006900900521     C****************************************************************
007000900521     C*  RIEPILOGO INDICATORI
007100900521     C***************************************************************
007200931116     C* 30    - DI COMODO
007300950125     C* 35    - TESTN E LOKUP
007400940414     C* 40/46 - ERRORI
007500950125     C* 47/49 - PER MESSAGGI A VIDEO
007600950403     C* 51/54 - ERRORI
007700020717     C* 89    - SONO 2° liv.
007800900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
007900900521     C*****************************************************************
008000000000     C     *ENTRY        PLIST
008100000000     C                   PARM                    KPJBA
008200000000     C                   Z-ADD     1             CODUT
008300900615     C                   CALL      'X§PARUT'
008400900615     C                   PARM                    UT§DSE
008500000000     C                   MOVEL     RAGUT         RSUT             20
008600900615     C                   MOVEL     REC80         CNCR80
008700900511     C*---------------------------------------------------------------*
008800940610     C* ACCESSO TABEL00F
008900931015     C     KTAB          KLIST
009000900706     C                   KFLD                    CODUT
009100940610     C                   KFLD                    COD
009200940610     C                   KFLD                    KEY
009300020918     C* ACCESSO FNFVV08L
009400940610     C     KFVV          KLIST
009500020918     C                   KFLD                    vi1pf1
009600940610     C                   KFLD                    WNPG
009700020918     C                   KFLD                    WDD1
009800020918     C     KFVV1         KLIST
009900020918     C                   KFLD                    vi1pf1
010000020918     C                   KFLD                    WNPG
010100020918     C* ACCESSO FNarb78L
010200020918     C     Karb78        KLIST
010300020918     C                   KFLD                    wfgs
010400020918     C                   KFLD                    Wnum7
010500020918     C* ACCESSO FNBLP34L
010600020918     C     KBLP          KLIST
010700020918     C                   KFLD                    WPDAL
010800020918     C                   KFLD                    WDD1
010900011114     C* ACCESSO fiftt04L
011000940613     C     KFTT          KLIST
011100940613     C                   KFLD                    WFVL
011200940613     C                   KFLD                    WDD1
011300940613     C                   KFLD                    WPDR
011400940613     C                   KFLD                    WTSR
011500021202     C* ACCESSO fiapd01L
011600021202     C     Kapd          KLIST
011700021202     C                   KFLD                    apdtip
011800021202     C                   KFLD                    parcpd
011900940610     C*
012000021202     C     Kapdw         KLIST
012100021202     C                   KFLD                    apdtip
012200021202     C                   KFLD                    wpdal
012300021202     C*
012400021202     c                   movel     'A'           apdtip
012500940610     C* DEFINIZIONE CAMPI
012600950403     C     *LIKE         DEFINE    G02TGI        WTGI
012700950403     C     *LIKE         DEFINE    TBLCOD        COD
012800940610     C     *LIKE         DEFINE    TBLKEY        KEY
012900940610     C     *LIKE         DEFINE    FVVNPG        WNPG
013000940613     C     *LIKE         DEFINE    FTTFVL        WFVL
013100940613     C     *LIKE         DEFINE    FTTTSR        WTSR
013200940613     C     *LIKE         DEFINE    W01DD1        WDD1
013300940613     C     *LIKE         DEFINE    W01DD2        WDD2
013400940613     C     *LIKE         DEFINE    W01DD1        SAVDD1
013500940613     C     *LIKE         DEFINE    W01DD2        SAVDD2
013600940613     C     *LIKE         DEFINE    FTTPDR        WPDR
013700940610     C*
013800940613     C                   MOVEL     'C'           WFVL
013900940613     C                   Z-ADD     4             WNPG
014000931015     C*---------------------------------------------------------------*
014100950125     C* CARICAMENTO TABELLE
014200940623     C                   EXSR      CARTAB
014300931129     C* PULIZIA FORMATO
014400931111     C                   EXSR      PUL01
014500930426     C*
014600931015     C     FOR01         TAG
014700950125     C                   EXFMT     LRD3D01
014800931116     C*
014900920323     C** CMD3 - FINE LAVORO
015000931112     C   KC              GOTO      FINE
015100931129     C* CONTROLLI FORMATO
015200900528     C                   EXSR      CONTR1
015300950125     C  NKF
015400931116     COR 90              GOTO      FOR01
015500931116     C*
015600931129     C                   MOVEL     PARAM         KPJBU
015700011116     C                   MOVEL     'FND4'        KCOAZ
015800931116     C                   CALL      'BCH10'
015900931116     C                   PARM                    KPJBA
016000900611     C*
016100000000     C     FINE          TAG
016200931027     C                   SETON                                        LR
016300931111     C*
016400950125     C*--- CARICAMENTO TABELLE ---------------------------------------*
016500940623     C     CARTAB        BEGSR
018000950125     C***
018100950125     C* VEDO SE SONO SIMFEL O REMOTO
018200950125     C***
018300020717     C                   MOVEL     simpou        VI1PF1
018400020717     C                   MOVEL     simpou        VI1PF2
018500020717     c                   if        simtpp = '2'
018600950125     C                   SETON                                        89
018700950125     C*
018800950125     C* CARICO TABELLA FILIALI GESTITE £6
018900950125     C                   CLEAR                   DSUL06
019000950125     C                   MOVE      '£6'          D06COD
019100020717     C                   MOVEL     simpou        D06KEY
019200950125     C                   MOVEL     DSUL06        KPJBU
019300950125     C*
019400950125     C                   CALL      'TRUL06R'
019500950125     C                   PARM                    KPJBA
019600950125     C                   MOVEL     KPJBU         DSUL06
019700950125     C                   MOVEA     LIN           L6
019800950125     C*
019900950125     C                   ELSE
020000950125     C* SONO SIMFEL
020100950125     C*
020200950125     C* CARICO TABELLA FILIALI GESTITE £1
020300950125     C                   CLEAR                   DSUL06
020400950125     C                   MOVE      '£1'          D06COD
020500950125     C                   MOVEL     SIMFEL        D06KEY
020600950125     C                   MOVEL     DSUL06        KPJBU
020700950125     C*
020800950125     C                   CALL      'TRUL06R'
020900950125     C                   PARM                    KPJBA
021000950125     C                   MOVEL     KPJBU         DSUL06
021100950125     C                   MOVEA     LIN           L1
021200950125     C                   ENDIF
021201061027     C*
021202061027     C****  CODICI PADRONCINO  ****
021203061027     C                   Z-ADD     1             X                 5 0
021204061027     C     apdtip        SETLL     fiapd000
021205061027     C     apdtip        READe     fiapd000                               30
021206061027     C*
021207061027    1C     *IN30         DOWEQ     *OFF
021208061027     C     APDATB        IFEQ      ' '
021209061027      **
021210061027      ** Carico solo i Padroncini appartenenti alle linee interessate
021211061027     c                   move      'NO'          Padr_Ok           2
021213061027     c     apdpdr        div       10000         apd_FIL           3 0
021214061027     c                   if        simtpp = '2'
021215061027     c     apd_FIL       lookup    L6                                     55
021216061027     c                   if        %Found
021217061027     c                   move      'OK'          Padr_Ok           2
021218061027     c                   end
021219061027     c                   else
021220061027     c     apd_FIL       lookup    L1                                     55
021221061027     c                   if        %Found
021222061027     c                   move      'OK'          Padr_Ok           2
021223061027     c                   end
021224061027     c                   end
021225061027      **
021226061027      ** Solo se appartiene
021227061027     c                   if        Padr_Ok = 'OK'
021228061027     C                   Z-ADD     APDPDR        CPD(X)
021229061027     C                   ADD       1             X
021230061027     c                   end
021231061027      **
021232061027     C                   ENDIF
021233061027     C     apdtip        READe     fiapd000                               30
021234061027    1C                   ENDDO
021300950125     C*
021400950403     C* SOMMO 3 GIORNI A UDATE PER CONTROLLO DATE
021500950403     C                   TIME                    WTIME            14 0          ORA E DATA
021600950403     C                   MOVE      WTIME         UDATE8            8 0          GG/MM/AAAA
021700950403     C                   Z-ADD     UDATE8        G02DAT
021800950403     C                   Z-ADD     *ZERO         G02INV
021900950403     C                   MOVEL     *BLANKS       G02ERR
022000950403     C                   CALL      'XSRDA8'
022100950403     C                   PARM                    WLBDAT
022200950403     C                   Z-ADD     G02INV        DATEU8            8 0          AAAA/MM/GG
022300950403     C*
022400950403     C                   ADD       3             G02TGI
022500950403     C                   Z-ADD     *ZEROS        GIODAT
022600950403     C                   Z-ADD     *ZEROS        GIOINV
022700950403     C                   Z-ADD     G02TGI        GIOTGI
022800950403     C                   CALL      'XSRGI8'
022900950403     C                   PARM                    WGIDAT
023000950403     C* WDAT3  --->  UDATE + 3 GIORNI
023100950403     C                   Z-ADD     GIOINV        WDAT3             8 0          AAAA/MM/GG
023200950403     C*
023300940623     C                   ENDSR
023400940623     C*
023500940623     C*--- PULIZIA CAMPI FORMATO 1 -----------------------------------*
023600931111     C     PUL01         BEGSR
023700931116     C                   CLEAR                   VI1PD1
023800931123     C                   CLEAR                   VI1PD2
023900931123     C                   CLEAR                   VI1DD1
024000931123     C                   CLEAR                   VI1DD2
024100931123     C                   CLEAR                   VI1TSR
024200931123     C                   MOVEL     'T'           VI1TVL
024300931111     C                   ENDSR
024400900528     C*
024500900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
024600900528     C     CONTR1        BEGSR
024700931111     C                   SETOFF                                       90
024800931129     C*
024900931123     C* T I P O   P R E S T A Z I O N E
025000931123    1C     VI1TSR        IFNE      ' '
025100931123    2C     VI1TSR        IFEQ      '?'
025200931129     C                   MOVEL     '1Z'          COD
025300931123     C                   MOVE      ' '           VI1TSR
025400931129     C                   MOVEL     *BLANKS       KEY
025500931123     C                   CALL      'X§TABER'
025600931129     C                   PARM                    CODUT
025700931129     C                   PARM                    COD
025800931129     C                   PARM                    KEY
025900931129     C                   PARM                    DES              25
026000931129     C                   MOVEL     KEY           VI1TSR
026100931123     C                   SETON                                        90
026200931123     C                   GOTO      ENDCT1
026300931123    2C                   ENDIF
026400931123     C*
026500931123     C* 30 ON - INESISTENTE O ANNULLATO
026600931123     C                   MOVEL     '1Z'          COD
026700931123     C                   MOVEL     *BLANKS       KEY
026800931123     C                   MOVEL     VI1TSR        KEY
026900931123     C     KTAB          CHAIN     TABEL                              30
027000931123     C  N30              MOVEL     TBLUNI        DS1Z
027100931123     C*
027200931123    2C     *IN30         IFEQ      *ON
027300931123     C     *IN30         OREQ      *OFF
027400931123     C     §1ZUPD        ANDEQ     ' '
027500931123     C                   SETON                                        4090
027600931123     C                   GOTO      ENDCT1
027700931123    2C                   ENDIF
027800931123    1C                   ENDIF
027900931123     C*
028000950125     C* F I L I A L E    P A D R O N C I N O    D A L
028100950125     C* FILIALE OBBLIGATORIA
028200950125     C     VI1PF1        IFEQ      0
028300950125     C                   SETON                                        5190
028400950125     C                   GOTO      ENDCT1
028500950125     C                   ENDIF
028600950125     C*
028700950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
028800950125    1C     *IN89         IFEQ      *OFF
028900950125     C     VI1PF1        IFNE      SIMFEL
029000950125     C     VI1PF1        LOOKUP    L1                                     35
029100950125     C  N35              SETON                                        5190
029200950125     C  N35              GOTO      ENDCT1
029300950125     C                   ENDIF
029400950125     C*
029500950125   X1C                   ELSE
029600020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
029700020717     C     vi1pf1        IFNE      simpou
029800950125     C     VI1PF1        LOOKUP    L6                                     35
029900950125     C  N35              SETON                                        5190
030000950125     C  N35              GOTO      ENDCT1
030100950125     C                   ENDIF
030200950125    1C                   ENDIF
030300020918     c                   z-add     vi1pf1        vi1pf2
030400950125     C*
030500931116     C* P A D R O N C I N O
030600931116     C* RICERCA
030700931116     C     '?'           SCAN      VI1PD1                                 90
030800931116     C*
030900931123    1C     *IN90         IFEQ      *ON
031000021202     C                   MOVEL     *BLANKS       d24rsc
031100021202     C                   Z-ADD     0             d24pdr
031200021202     C                   Z-ADD     VI1PF1        d24FIL
031300021202     C                   MOVEL     'R'           d24FLG
031400021202     C                   MOVEL     'A'           d24tip
031500021202     C                   MOVEL(p)  fnlv24ds      KPJBU
031600950125     C                   CALL      'FNLV24R'
031700931116     C                   PARM                    KPJBA
031800021202     C                   MOVEL     KPJBU         fnlv24ds
031900950125     C*
032000950125     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
032100021202     C     d24pdr        IFNE      0
032200021202     C                   MOVEL     d24pdr        VI1PF1
032300931116     C                   MOVEL     *ZEROS        VI1PD1
032400021202     C                   MOVE      d24pdr        VI1PD1
032500021202     C                   MOVEL     d24rsc        VI1DP1
032600950125     C                   ENDIF
032700950125     C*
032800931116     C                   GOTO      ENDCT1
032900931123    1C                   ENDIF
033000940414     C*
033100940414     C     '?'           SCAN      VI1PD2                                 90
033200931116     C*
033300940414    1C     *IN90         IFEQ      *ON
033400021202     C                   MOVEL     *BLANKS       d24rsc
033500021202     C                   Z-ADD     0             d24pdr
033600021202     C                   Z-ADD     VI1PF2        d24FIL
033700021202     C                   MOVEL     'R'           d24FLG
033800021202     C                   MOVEL     'A'           d24tip
033900021202     C                   MOVEL(p)  fnlv24ds      KPJBU
034000950125     C                   CALL      'FNLV24R'
034100940414     C                   PARM                    KPJBA
034200021202     C                   MOVEL     KPJBU         fnlv24ds
034300950125     C*
034400950125     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
034500021202     C     d24pdr        IFNE      0
034600021202     C                   MOVEL     d24pdr        VI1PF2
034700940414     C                   MOVEL     *ZEROS        VI1PD2
034800021202     C                   MOVE      d24pdr        VI1PD2
034900950125     C                   ENDIF
035000950125     C*
035100940414     C                   GOTO      ENDCT1
035200940414    1C                   ENDIF
035300940414     C*
035400950125     C* PADRONCINO DAL
035500931123    1C     VI1PD1        IFLE      *ZEROS
035600931123     C                   SETON                                        4190
035700931112     C                   GOTO      ENDCT1
035800931123    1C                   ENDIF
035900940623     C*
036000950206     C                   TESTN                   VI1PD1               35
036100950206     C     *IN35         IFEQ      *ON
036200950206     C                   MOVE      VI1PD1        W001              1
036300950206     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
036400950206     C     W001          IFLT      '0'
036500950206     C                   SETOFF                                       35
036600950206     C                   ENDIF
036700950206     C                   ENDIF
036800950206     C*
036900950206     C  N35              SETON                                        4190
037000950206     C  N35              GOTO      ENDCT1
037100950206     C*
037200950125     C* PADRONCINO AL
037300950125    1C     VI1PD2        IFNE      *BLANKS
037400950125     C     VI1PD2        ANDNE     *ZEROS
037500950125     C     VI1PF2        ANDEQ     0
037600950125     C                   SETON                                        5290
037700950125     C                   GOTO      ENDCT1
037800950125    1C                   ENDIF
037900950125     C*
038000931129     C* SE IMMESSO SOLO IL PADRONCINO DAL CONTROLLO LA SUA ESISTENZA
038100931129    1C     VI1PD2        IFEQ      *BLANKS
038200931129     C     VI1PD2        OREQ      *ZEROS
038300021202     C     kapd          CHAIN     fiapd000                           30
038400931015     C*
038500931123    2C     *IN30         IFEQ      *ON
038600931028     C     *IN30         OREQ      *OFF
038700950125     C     APDATB        ANDNE     ' '
038800931123     C                   SETON                                        4190
038900931028     C                   GOTO      ENDCT1
039000931123    2C                   ENDIF
039100100223     C                   MOVEL     APDRSf        VI1DP1
039200931123     C*
039300931123   X2C                   ELSE
039400940414     C*
039500940414     C* CONTROLLO PADRONCINO AL
039600940414     C                   TESTN                   VI1PD2               35
039700950206     C*
039800950206     C     *IN35         IFEQ      *ON
039900950206     C                   MOVE      VI1PD2        W001              1
040000950206     C* CONTROLLO SE L'ULTIMO CARATTERE E' UNA LETTERA
040100950206     C     W001          IFLT      '0'
040200950206     C                   SETOFF                                       35
040300950206     C                   ENDIF
040400950206     C                   ENDIF
040500950206     C*
040600940414     C  N35              SETON                                        4690
040700940414     C  N35              GOTO      ENDCT1
040800940414     C*
040900931129     C* PADRONCINO AL  <  PADRONCINO DAL
041000950125     C     PARCPA        IFLT      PARCPD
041100931123     C                   SETON                                        4290
041200931123     C                   GOTO      ENDCT1
041300931123    2C                   ENDIF
041400931123    1C                   ENDIF
041500931115     C*
041600931123     C* D A T A   D I S T I N T A: DAL
041700931123     C                   MOVE      VI1DD1        G02DAT
041800931018     C                   MOVEL     *BLANK        G02ERR
041900950125     C                   CALL      'XSRDA8'
042000931018     C                   PARM                    WLBDAT
042100931021    2C     G02ERR        IFEQ      '1'
042200931123     C                   SETON                                        43  90
042300931018     C                   GOTO      ENDCT1
042400931129    2C                   ENDIF
042500950125     C                   Z-ADD     G02INV        W01DD1
042600950125     C                   Z-ADD     G02DAT        VI1DD1
042700950403     C*
042800950403     C* DATA DAL NON PUO' ESSERE MAGGIORE DI UDATE + 3 GIORNI
042900950403     C     W01DD1        IFGT      WDAT3
043000950403     C                   SETON                                        53  90
043100950403     C                   GOTO      ENDCT1
043200950403     C                   ENDIF
043300931123     C*
043400931123     C     VI1DD2        IFGT      0
043500931123     C                   MOVE      VI1DD2        G02DAT
043600931123     C                   MOVEL     *BLANK        G02ERR
043700950125     C                   CALL      'XSRDA8'
043800931123     C                   PARM                    WLBDAT
043900931123    2C     G02ERR        IFEQ      '1'
044000931123     C                   SETON                                        44  90
044100931123     C                   GOTO      ENDCT1
044200931129    2C                   ENDIF
044300950125     C                   Z-ADD     G02INV        W01DD2
044400950403     C                   Z-ADD     G02DAT        VI1DD2
044500950403     C*
044600950403     C* DATA AL  NON PUO' ESSERE MAGGIORE DI UDATE + 3 GIORNI
044700950403     C     W01DD2        IFGT      WDAT3
044800950403     C                   SETON                                        54  90
044900950403     C                   GOTO      ENDCT1
045000950403     C                   ENDIF
045100950403     C*
045200931123     C* DATA AL NO < DATA DAL
045300931123     C     W01DD2        IFLT      W01DD1
045400931123     C                   SETON                                        45  90
045500931123     C                   GOTO      ENDCT1
045600931129     C                   ENDIF
045700931129     C*
045800931129     C                   ELSE
045900931129     C                   CLEAR                   W01DD2
046000931129     C                   ENDIF
046100910320     C*
046200940613     C* C O N T R O L L O :
046300940613     C* 1) CHE NON ESISTANO DISTINTE ANCORA DA CHIUDERE
046400940613     C* 2) CHE NON ESISTANO DISTINTE DI CUI GIA' CONFERMATI I CONTEGGI
046500940613     C* 3) CHE NON ESISTANO IN PARTENZA SPEDIZIONI DI QUEL PADRONCINO
046600940613     C*    CON QUELLA DATA RITIRO
046700940610     C                   EXSR      CTRDIS
046800940610     C*
046900900614     C     ENDCT1        ENDSR
047000940610     C*
047100940613     C*--- CONTROLLI DISTINTE ----------------------------------------*
047200940610     C     CTRDIS        BEGSR
047300940613     C                   SETOFF                                       05
047400940613     C                   SETOFF                                       474849
047500940623     C                   CLEAR                   CP2
047600950125     C*
047700950125     C                   MOVEL     VI1PF1        COMPF1
047800950125     C                   MOVEL     VI1PD1        COMPD1
047900950125     C                   MOVEL     VI1PF2        COMPF2
048000950125     C                   MOVEL     VI1PD2        COMPD2
048100940623     C*
048200940613     C                   Z-ADD     W01DD1        WDD1
048300940613     C                   Z-ADD     W01DD2        WDD2
048400940613     C                   Z-ADD     W01DD1        SAVDD1
048500940613     C                   Z-ADD     W01DD2        SAVDD2
048600940610     C*
048700950125     C     COMPD2        IFEQ      0
048800950125     C                   Z-ADD     WPDAL         WPAL
048900940613     C                   ENDIF
049000940613     C     WDD2          IFEQ      0
049100940613     C                   Z-ADD     WDD1          WDD2
049200940613     C                   Z-ADD     WDD1          SAVDD2
049300940613     C                   ENDIF
049400940613     C*
049500940610     C* 05 ON  - IMMESSA ANCHE DATA DISTINTA/RITIRI AL
049600940613     C     WDD2          IFGT      0
049700940613     C     WDD1          ANDNE     WDD2
049800940610     C                   SETON                                        05
049900940610     C                   ENDIF
050000940613     C*
050100940613     C***
050200940613     C** 1) CONTROLLO CHE NON ESISTANO DISTINTE ANCORA DA CHIUDERE
050300940613     C***
050400950125     C     KFVV          SETLL     FNFVV000
050500950125     C  N05KFVV          READE     FNFVV000                               30
050600020918     C   05kfvv1         READE     FNFVV000                               30
050700940610     C*
050800940610    1C     *IN30         DOWEQ     *OFF
050900940610     C*
051000940623     C   05FVVDFV        IFGT      WDD2
051100940610     C                   SETON                                        30
051200940610     C                   ENDIF
051300940610     C*
051400940610    2C     *IN30         IFEQ      *OFF
051500940610     C*
051600940610     C* CONTROLLO SOLTANTO GLI APERTI
051700940610    3C     FVVFCF        IFNE      'S'
051800940610     C                   Z-ADD     FVVNFV        WNUM7             7 0
051900020918     C                   Z-ADD     FVVfgs        Wfgs
052000020918     C     karb78        CHAIN     FNARB000                           31
052100940610     C*
052200950126     C* CONTROLLO CHE NON SIA CONSEGNATA
052300940610    4C     *IN31         IFEQ      *OFF
052400950126     C     ARBDCM        ANDEQ     0
052500940610     C*
052600950125    5C     ARBPDC        IFGE      WPDAL
052700950125     C     ARBPDC        ANDLE     WPAL
052800940610     C*
052900940610     C                   SETON                                        473090
053000940610     C                   MOVEL     ERR(1)        VI1ERR
053100020923     C                   Z-ADD     ARBDDC        WDD1
053200020923     C                   Z-ADD     ARBPDC        WPDAL
053300020923     c                   leave
053400940610    5C                   ENDIF
053500940610    4C                   ENDIF
053600940610    3C                   ENDIF
053700940610    2C                   ENDIF
053800940610     C*
053900020918     c                   if        not *in30
054000020918     C  N05KFVV          READE     FNFVV000                               30
054100020918     C   05kfvv1         READE     FNFVV000                               30
054200020918    2C                   ENDIF
054300940610    1C                   ENDDO
054400940610     C*
054500940613     C*
054600940613     C* 90 ON  - SE TROVATO ERRORE VADO A FINE SUBROUTINE
054700940613    0C     *IN90         IFEQ      *OFF
054800940610     C*
054900940623     C* CARICO SOLO I PADRONCINI ESISTENTI NEL RANGE RICHIESTO
055000031117     C                   Z-ADD     1             X                 5 0
055100031117     C                   Z-ADD     1             Y                 5 0
055200950125     C     WPDAL         LOOKUP    CPD(X)                                 35
055300940623     C*
055400940623     C* SE NON LO TROVO CERCO QUELLO IMMEDIATAMENTE SUCESSIVO
055500940623     C     *IN35         IFEQ      *OFF
055600021202     C     kapdw         SETGT     fiapd000
055700021202     C     apdtip        READe     fiapd000                               30
055800940623     C     *IN30         IFEQ      *OFF
055900950125     C     APDPDR        LOOKUP    CPD(X)                                 35
056000940623     C                   ENDIF
056100940623     C                   ENDIF
056200940623     C*
056300940623     C     *IN35         IFEQ      *ON
056400950125     C     CPD(X)        DOWLE     WPAL
056500940623     C     CPD(X)        ANDGT     0
056600940623     C                   MOVEL     CPD(X)        CP2(Y)
056700940623     C                   ADD       1             Y
056800940623     C                   ADD       1             X
056900940623     C                   ENDDO
057000940623     C                   ENDIF
057100940623     C*
057200940613     C***
057300940613     C** 2) CONTROLLO CHE NON ESISTANO DISTINTE DI CUI GIA' CONFERMATI
057400940613     C**    I CONTEGGI
057500940613     C** 3) CONTROLLO CHE NON ESISTANO IN PARTENZA SPEDIZIONI DI QUEL
057600940613     C**    PADRONCINO CON QUELLA DATA RITIRO
057700940613     C***
057800940613     C                   SETOFF                                       30
057900950403     C*
058000950403     C                   MOVE      WDD1          G02INV
058100950403     C                   MOVEL     '3'           G02ERR
058200950403     C                   CALL      'XSRDA8'
058300950403     C                   PARM                    WLBDAT
058400950403     C                   Z-ADD     G02TGI        WTGI
058500940623     C*
058600940623     C                   Z-ADD     1             X
058700940623    1C     CP2(X)        DOWNE     0
058800940613     C     *IN30         ANDEQ     *OFF
058900950125     C                   MOVEL     CP2(X)        WPDAL
059000940613     C*
059100940613     C* REIMPOSTO TUTTE LE VOLTE LA DATA DISTINTA/RITIRI
059200940613     C                   Z-ADD     SAVDD1        WDD1
059300940613     C                   Z-ADD     SAVDD2        WDD2
059400940613     C*
059500940613    2C     WDD1          DOWLE     WDD2
059600940613     C     *IN30         ANDEQ     *OFF
059700940613     C*
059800940613     C* CONTROLLO CHE NON ESISTANO DISTINTE DI CUI GIA' CONFERMATI I
059900940613     C*  CONTEGGI
060000940613     C                   Z-ADD     1             WCONT             1 0
060100940613     C                   MOVEL     'C'           WTSR
060200940613     C*
060300940613    3C     WCONT         DOWLE     2
060400950125     C                   MOVEL     WPDAL         WPDR
060500940613     C*
060600011114     C     KFTT          SETLL     fiftt000                               30
060700940613    4C     *IN30         IFEQ      *ON
060800940613     C                   SETON                                        4890
060900940613     C                   MOVEL     ERR(2)        VI1ERR
061000940613     C                   Z-ADD     3             WCONT
061100940613   X4C                   ELSE
061200940613     C                   MOVEL     'R'           WTSR
061300940613    4C                   ENDIF
061400940613     C*
061500940613     C                   ADD       1             WCONT
061600940613    3C                   ENDDO
061700940613     C*
061800940613     C*
061900940613    3C     *IN30         IFEQ      *OFF
062000940613     C* CONTROLLO CHE NON ESISTANO IN PARTENZA SPEDIZIONI DI QUEL
062100940613     C*   PADRONCINO CON QUELLA DATA RITIRO
062200950126     C     KBLP          SETLL     FNBLP000
062300950126     C     KBLP          READE     FNBLP000                               32
062400950126     C*
062500950126    4C     *IN32         DOWEQ     *OFF
062600950126    5C     BLPFT2        IFEQ      ' '
062700940613     C                   SETON                                        4990
062800950126     C                   SETON                                        3230
062900940613     C                   MOVEL     ERR(3)        VI1ERR
063000950126   X5C                   ELSE
063100950126     C     KBLP          READE     FNBLP000                               32
063200950126    5C                   ENDIF
063300950126    4C                   ENDDO
063400940613     C*
063500950126    4C     *IN30         IFEQ      *OFF
063600950403     C     WTGI          ADD       1             GIOTGI
063700950403     C                   ADD       1             WTGI
063800950125     C                   CALL      'XSRGI8'
063900940613     C                   PARM                    WGIDAT
064000940613     C                   Z-ADD     GIOINV        WDD1
064100940613    4C                   ENDIF
064200950126     C*
064300940613    3C                   ENDIF
064400940613     C*
064500940613    2C                   ENDDO
064600940613     C*
064700940623     C                   ADD       1             X
064800940613    1C                   ENDDO
064900940613     C*
065000940613    0C                   ENDIF
065100940613     C*
065200940613     C*
065300940613     C* GIRO LA DATA
065400940613    1C     *IN90         IFEQ      *ON
065500950125     C                   Z-ADD     WDD1          DATA
065600950125     C                   Z-ADD     MM            GGMM              4 0
065700950125     C                   MOVEL     GG            GGMM
065800950125     C                   Z-ADD     AA            VI1DDE
065900950125     C                   MOVEL     GGMM          VI1DDE
066000940613     C*
066100950125     C                   MOVEL     WPDAL         VI1PDE
066200940613     C*
066300940613     C   47              MOVEL     'C'           VI1TSE
066400940613     C   48              MOVEL     WTSR          VI1TSE
066500940613     C   49              MOVEL     'R'           VI1TSE
066600940613    1C                   ENDIF
066700940613     C*
066800940610     C                   ENDSR
066900940613     C*---------------------------------------------------------------*
067000940610     C*
067100940613**   ERR
067200940610Esistono ancora distinte da chiudere con data distinta
067300940613Esistono distinte gia' confermate con data distinta
067400940613Esistono ancora spedizioni non partite con data ritiro
