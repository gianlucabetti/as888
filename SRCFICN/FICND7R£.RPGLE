000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011115     H* FICND5R *----------------------------------------------------*
000300000808     H*         - STAMPA RIEPILOGO CONTEGGI PADRONCINI
000400000000     H*--------------------------------------------------------------*
000500091112     FFICND7D   CF   E             WORKSTN
000600021203     FFIAPD01L  IF   E           K DISK
000700991228     FTABEL00F  IF   E           K DISK
000800950125     D* SCHIERE PER CARICAMENTO L1 E L6
000900950125     D L1              S              3  0 DIM(30)                              FIL GESTITE
001000950125     D L6              S              3  0 DIM(30)                              FIL GESTITE
001100920810     D KPJBA         E DS
001200091112     D ficnd7ds      E DS
001300091112     D ddatiute      e ds
001400091112     D azuteds       e ds                  extname(AZUTE00F)
001500091112     D tibs34ds      E DS                  inz
001600950125     D* DS PER TRUL06R - CARICAMENTO £1
001700950125     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001800950125     D  LIN                    1     90  0
001900950125     D                                     DIM(30)                              SKI COMODO
002000910423     D WLBDAT          DS
002100950125     D  G02DAT                 1      8  0
002200950125     D  G02INV                 9     16  0
002300950125     D  G02ERR                17     17
002400950125     D  G02TGI                18     22  0
002500931015     D                 DS
002600931015     D  VI1PF1                 1      3  0
002700931116     D  VI1PD1                 4      7
002800931115     D  VI1P1                  1      7  0
002900931126     D                 DS
003000931126     D  VI1PF2                 1      3  0
003100931126     D  VI1PD2                 4      7
003200931126     D  VI1P2                  1      7  0
003300991228     D DS5A          E DS
003400021203     D FNLV24DS      E DS
003500900521     C*****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800091112     c     *dtaara       define    §azute        azuteds
003900091112     c     *dtaara       define    §datiute      ddatiute
004000091112     C                   in(E)     *dtaara
004100091112     C                   IF        %Error  or  RSUT = *blanks
004200091112     C                   call      'TIBS34R'
004300091112     C                   parm                    Tibs34Ds
004400091112     C                   in        *dtaara
004500091112     c                   ENDIF
004600091112     c                   movel     kpjbu         ficnd7ds
004700031023     C                   MOVEL     knmus         Primi_tre         3
004800900511     C*---------------------------------------------------------------*
004900991228     C     KTAB          KLIST
005000091112     C                   KFLD                    tblkut
005100991228     C                   KFLD                    COD
005200991228     C                   KFLD                    KEY
005300091112     C                   Z-ADD     1             tblkut
005400021203     C     KAPDV         KLIST
005500021203     C                   KFLD                    APDTIP
005600021203     C                   KFLD                    VI1P1
005700021203     C                   MOVEL     'A'           APDTIP
005800000808     C*
005900991228     C     *LIKE         DEFINE    TBLCOD        COD
006000991228     C     *LIKE         DEFINE    TBLKEY        KEY
006100991228     C     *LIKE         DEFINE    G02TGI        DD1GIO
006200991228     C     *LIKE         DEFINE    G02TGI        DD2GIO
006300991228     C     *LIKE         DEFINE    G02TGI        WRESTO
006400931116     C*---------------------------------------------------------------*
006500910419     C*
006600950411     C* OPERAZIONI INIZIALI
006700931111     C                   EXSR      PUL01
006800930426     C*
006900091112     C                   do        *hival
007000950125     C                   EXFMT     LRD5D01
007100931116     C*
007200920323     C** CMD3 - FINE LAVORO
007300091112     C   KC              leave
007400931015     C* CONTROLLI FORMATO1
007500900528     C                   EXSR      CONTR1
007600950125     C  NKF
007700091112     COR 90              iter
007800931116     C*
007900931202     C     VI1PD1        IFEQ      *BLANKS
008000931207     C                   MOVEL     *ZEROS        VI1PD1
008100931202     C                   ENDIF
008200091116     C     VI1Pf2        IFEQ      0
008300091116     C                   MOVE      999           VI1Pf2
008400091116     c                   end
008500091116     C     VI1PD2        IFEQ      *BLANKS
008600091116     C                   MOVEL     '9999'        VI1PD2
008700931202     C                   ENDIF
008800931126     C                   MOVE      VI1P1         PARP1
008900091116     C                   MOVE      VI1P1         PARP1
009000091112     C                   MOVE      VI1P2         PARP2
009100091112     C                   MOVE      VI1tsp        PARtsp
009200091113     C                   MOVE      V1cclr        PARclr
009300110822     C                   MOVE      Vi1CMG        PARcmg
009400091112     C                   MOVEL     ficnd7ds      KPJBU
009500931202     C*
009600011115     c     knmus         ifeq      *all'1'
009700091112     C                   call      'FICND8R'
009800011115     C                   parm                    Kpjba
009900011115     c                   else
010000091112     C                   MOVEL     'FND8'        KCOAZ
010100931116     C                   CALL      'BCH10'
010200931116     C                   PARM                    KPJBA
010300011115     c                   end
010400900611     C*
010500091112     C                   MOVEL     '0000'        VI1PD1
010600091112     C                   MOVEL     '9999'        VI1PD2
010700091112     C                   CLEAR                   VI1DD1
010800091112     C                   CLEAR                   VI1DD2
010900091112     C                   MOVEL     '1'           VI1TSP
011000091112     C                   iter
011100091112     C                   enddo
011200091112     C*
011300931027     C                   SETON                                        LR
011400931111     C*
011500931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
011600931111     C     PUL01         BEGSR
011700091113     c                   movel     '1'           VI1TSP
011800091113     c                   movel     'N'           V1cclr
011900110822     c                   movel     'N'           Vi1CMG
012000931126     C* CARICO L1
012100950125     C                   CLEAR                   DSUL06
012200950125     C                   MOVE      '£1'          D06COD
012300950125     C                   MOVEL     SIMFEL        D06KEY
012400950125     C                   MOVEL     DSUL06        KPJBU
012500950125     C*
012600950125     C                   CALL      'TRUL06R'
012700950125     C                   PARM                    KPJBA
012800950125     C                   MOVEL     KPJBU         DSUL06
012900950125     C                   MOVEA     LIN           L1
013000931126     C*
013100950125     C***
013200950125     C* VEDO SE SONO SIMFEL O REMOTO
013300950125     C***
013400091112     C                   EVAL      Vi1PF1 = dutPOU
013500091112     C                   EVAL      Vi1PF2 = dutPOU
013600091112     C                   IF        dutlpo = '2' OR dutlpo = *BLANK
013700950125     C                   SETON                                        89
013800950125     C*
013900950125     C* CARICO TABELLA FILIALI GESTITE £6
014000950125     C                   CLEAR                   DSUL06
014100950125     C                   MOVE      '£6'          D06COD
014200020701     C*                  MOVEL     REMFIL        D06KEY
014300091112     C                   MOVEL     dutPOU        D06KEY
014400950125     C                   MOVEL     DSUL06        KPJBU
014500950125     C*
014600950125     C                   CALL      'TRUL06R'
014700950125     C                   PARM                    KPJBA
014800950125     C                   MOVEL     KPJBU         DSUL06
014900950125     C                   MOVEA     LIN           L6
015000950125     C*
015100950125     C                   ELSE
015200950125     C* SONO SIMFEL
015300950125     C                   MOVEL     SIMFEL        VI1PF1
015400991228     C                   MOVEL     SIMFEL        VI1PF2
015500950125     C                   ENDIF
015600991228     C                   MOVEL     '5A'          COD
015700991228     C                   MOVEL(P)  '1'           KEY
015800991228     C     KTAB          CHAIN     TABEL                              30
015900991228     C  N30              MOVEL     TBLUNI        DS5A
016000991228     C   30              CLEAR                   DS5A
016100000203     C                   TIME                    W0140            14 0
016200000203     C* UDATE IN GGMMAAAA
016300000203     C                   MOVE      W0140         WDTGIO            8 0
016400000203     C                   MOVE      WDTGIO        ANNOCO            4 0
016500000203     C     ANNOCO        ADD       1             ANNOPI            4 0
016600000203     C     ANNOCO        SUB       1             ANNOME            4 0
016700950125     C*
016800931111     C                   ENDSR
016900900528     C*
017000900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
017100900528     C     CONTR1        BEGSR
017200931111     C                   SETOFF                                       90
017300931126     C*
017400931126     C* D A T A   D I S T I N T A: DAL
017500931126     C                   MOVE      VI1DD1        G02DAT
017600931126     C                   MOVEL     *BLANK        G02ERR
017700950125     C                   CALL      'XSRDA8'
017800931126     C                   PARM                    WLBDAT
017900931126    1C     G02ERR        IFEQ      '1'
018000931126     C                   SETON                                        43  90
018100091112     C                   leavesr
018200931126    1C                   END
018300091112     C                   Z-ADD     G02INV        parDD1
018400950125     C                   Z-ADD     G02DAT        VI1DD1
018500991228     C                   Z-ADD     G02TGI        DD1GIO
018600011115     C                   z-add     vi1dd1        an1               4 0
018700000203     C* CONTROLLO L'ANNO
018800000203     C                   Z-ADD     G02DAT        W0040             4 0
018900000203     C     W0040         IFGT      ANNOPI
019000000203     C     W0040         ORLT      ANNOME
019100000203     C                   SETON                                        43  90
019200091112     C                   leavesr
019300000203    1C                   END
019400931126     C*
019500021106     C* comunque imposta il 2 campo data
019600091112     C                   Z-ADD     pardd1        pardd2
019700931126    1C     VI1DD2        IFGT      0
019800931126     C                   MOVE      VI1DD2        G02DAT
019900931126     C                   MOVEL     *BLANK        G02ERR
020000950125     C                   CALL      'XSRDA8'
020100931126     C                   PARM                    WLBDAT
020200931126    2C     G02ERR        IFEQ      '1'
020300931126     C                   SETON                                        44  90
020400091112     C                   leavesr
020500931126    2C                   END
020600091112     C                   Z-ADD     G02INV        parDD2
020700950125     C                   Z-ADD     G02DAT        VI1DD2
020800991228     C                   Z-ADD     G02TGI        DD2GIO
020900011115     C                   z-add     vi1dd2        an2               4 0
021000000203     C* CONTROLLO L'ANNO
021100091112     C                   Z-ADD     G02DAT        w0040             4 0
021200011115     c     an1           ifne      an2
021300011115     C                   SETON                                        56  90
021400091112     C                   leavesr
021500011115     c                   end
021600000203     C     W0040         IFGT      ANNOPI
021700000203     C     W0040         ORLT      ANNOME
021800000203     C                   SETON                                        44  90
021900091112     C                   leavesr
022000000203    1C                   END
022100931126     C* DATA AL NO < DATA DAL
022200091112    2C     parDD2        IFLT      parDD1
022300931126     C                   SETON                                        45  90
022400091112     C                   leavesr
022500931126    2C                   ENDIF
022600991228     C* I GIORNI TRA UNA DATA E L'ALTRA NON DEVONO SUPERARE
022700991228     Q* QUELLI DI PULIZIA + 10
022800991228     C     §5AFTT        ADD       10            WGIORN            4 0
022900991228     C**
023000991228     C     DD2GIO        SUB       DD1GIO        WRESTO
023100991228     C**
023200991228    2C     WRESTO        IFGT      WGIORN
023300991228     C                   SETON                                        55  90
023400091112     C                   leavesr
023500991228    2C                   ENDIF
023600991228     C**
023700991228    1C                   ENDIF
023800931123     C*
023900031023     C* Se non è un utente EDPxxx controlla la filiale inserita
024000091116     c                   if        primi_tre <> 'EDP' and dutlpo <> 'S'
024100031023     C*
024200950125     C* F I L I A L E    P A D R O N C I N O    D A L
024300950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
024400950125    1C     *IN89         IFEQ      *OFF
024500950125     C     VI1PF1        IFNE      SIMFEL
024600080109     C* non può essere (0) la filiale
024700080109     c                   if        VI1PF1 = 0
024800080109     c                   z-add     simfel        VI1PF1
024900080109     c                   end
025000950125     C     VI1PF1        LOOKUP    L1                                     35
025100950125     C  N35              SETON                                        5390
025200091112     C  N35              leavesr
025300950125     C                   ENDIF
025400950125     C*
025500950125   X1C                   ELSE
025600020701     C                   MOVEL     VI1PF1        WFIL              3 0
025700091112     C     WFIL          IFNE      dutPOU
025800950125     C     VI1PF1        LOOKUP    L6                                     35
025900950125     C  N35              SETON                                        5390
026000091112     C  N35              leavesr
026100950125     C                   ENDIF
026200950125    1C                   ENDIF
026300031023     c                   end
026400021015     c                   z-add     vi1pf1        vi1pf2
026500031023     C* Se non è un utente EDPxxx controlla la filiale inserita
026600091116     c                   if        primi_tre <> 'EDP' and dutlpo <> 'S'
026700950125     C* F I L I A L E    P A D R O N C I N O    A L
026800950125    1C     *IN89         IFEQ      *OFF
026900950125     C     VI1PF2        IFNE      SIMFEL
027000950125     C     VI1PF2        LOOKUP    L1                                     35
027100950125     C  N35              SETON                                        5490
027200091112     C  N35              leavesr
027300950125     C                   ENDIF
027400950125   X1C                   ELSE
027500020701     C                   MOVEL     VI1PF2        WFIL
027600091112     C     WFIL          IFNE      dutPOU
027700950125     C     VI1PF2        LOOKUP    L6                                     35
027800950125     C  N35              SETON                                        5490
027900091112     C  N35              leavesr
028000950125     C                   ENDIF
028100950125    1C                   ENDIF
028200031023     c                   end
028300950131     C* CONTROLLO CONGRUENZA FILIALI DAL-AL
028400950131    1C     VI1PF1        IFNE      VI1PF2
028500950131     C                   SETON                                        4890
028600091112     C                   leavesr
028700950131    1C                   ENDIF
028800931116     C* P A D R O N C I N O
028900931116     C* RICERCA
029000931116     C     '?'           SCAN      VI1PD1                                 90
029100931116     C*
029200931126    1C     *IN90         IFEQ      *ON
029300021203     C                   Z-ADD     VI1PF1        D24FIL
029400931126     C                   EXSR      RICPAD
029500931126     C*
029600021203     C     D24PDR        IFNE      0
029700021203     C                   MOVEL     D24PDR        VI1PF1
029800931116     C                   MOVEL     *ZEROS        VI1PD1
029900021203     C                   MOVE      D24PDR        VI1PD1
030000021203     C                   MOVEL     D24RSC        VI1DP1
030100950125     C                   ENDIF
030200091112     C                   leavesr
030300931126    1C                   ENDIF
030400931126     C* PADRONCINO AL
030500931126     C* RICERCA
030600931126     C     '?'           SCAN      VI1PD2                                 90
030700931126     C*
030800931126    1C     *IN90         IFEQ      *ON
030900021203     C                   Z-ADD     VI1PF2        D24FIL
031000931126     C                   EXSR      RICPAD
031100931126     C*
031200021203     C     D24PDR        IFNE      0
031300021203     C                   MOVEL     D24PDR        VI1PF2
031400931126     C                   MOVEL     *ZEROS        VI1PD2
031500021203     C                   MOVE      D24PDR        VI1PD2
031600950125     C                   ENDIF
031700091112     C                   leavesr
031800931126    1C                   ENDIF
031900931126     C*
032000931126     C*
032100931126    1C     VI1PD2        IFNE      *BLANKS
032200931126     C     VI1PD2        ANDNE     *ZEROS
032300931126     C                   TESTN                   VI1PD2               30
032400931126     C  N30              SETON                                        4790
032500931126     C                   ENDIF
032600931116     C*
032700931126    1C     VI1PD1        IFNE      *BLANKS
032800931126     C     VI1PD1        ANDNE     *ZEROS
032900931126     C                   TESTN                   VI1PD1               30
033000931126     C  N30              SETON                                        4190
033100931116     C*
033200931123     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
033300931126    2C     VI1PD2        IFEQ      *BLANKS
033400931126     C     VI1PD2        OREQ      *ZEROS
033500931126     C*
033600021203     C     KAPDV         CHAIN     FIAPD000                           30
033700931015     C*
033800931126    3C     *IN30         IFEQ      *ON
033900020520     C* Tolto il controllo sul padroncino annullato
034000020520     C                   SETON                                        4190
034100091112     C                   leavesr
034200931126    3C                   ENDIF
034300931129     C*
034400100223     C                   MOVEL     APDRSf        VI1DP1
034500931123     C*
034600931123   X2C                   ELSE
034700931126     C*
034800931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
034900950125    3C     VI1P2         IFLT      VI1P1
035000931123     C                   SETON                                        4290
035100091112     C                   leavesr
035200931126    3C                   ENDIF
035300931126    2C                   ENDIF
035400931123    1C                   ENDIF
035500010906     C*
035600010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
035700010906     C     VI1PD1        IFEQ      *ZEROS
035800010906     C     VI1PD2        ANDEQ     *ZEROS
035900010906     C                   SETON                                        4090
036000091112     C                   leavesr
036100010906     C                   ENDIF
036200110822     C*
036300110822     C* x il calcolo del Costo Medio Giornaliero si deve pulire il WFile
036400110822     C                   IF        vi1CMG ='S' and v1cCLR <> 'S'
036500110822     C                   SETON                                        5990
036600110822     C                   leavesr
036700110822     C                   ENDIF
036800910320     C*
036900091112     C                   ENDSR
037000931126     C*
037100931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
037200931126     C     RICPAD        BEGSR
037300021203     C                   MOVEL     *BLANKS       D24RSC
037400021203     C                   Z-ADD     0             D24PDR
037500021203     C                   MOVEL     'R'           D24FLG
037600021203     C                   MOVEL     'A'           D24TIP
037700021203     C                   MOVEL(P)  FNLV24DS      KPJBU
037800950125     C                   CALL      'FNLV24R'
037900931126     C                   PARM                    KPJBA
038000021203     C                   MOVEL     KPJBU         FNLV24DS
038100931126     C                   ENDSR
