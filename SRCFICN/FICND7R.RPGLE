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
009100120411     C                   MOVE      '1'           PARtsp
009200120412     C                   MOVE      'S'           PARclr
009300120411      * se richiesto il Sintetico genera il secondo file a totale
009400120411     C                   MOVE      'S'           PARcmg
009500120411     C                   MOVEL(p)  ficnd7ds      KPJBU
009600931202     C*
009700011115     c     knmus         ifeq      *all'1'
009800120524     C                   call      'FICND10R'
009900011115     C                   parm                    Kpjba
010000011115     c                   else
010100120524     C                   MOVEL     'FND1'        KCOAZ
010200931116     C                   CALL      'BCH10'
010300931116     C                   PARM                    KPJBA
010400011115     c                   end
010500900611     C*
010600091112     C                   MOVEL     '0000'        VI1PD1
010700091112     C                   MOVEL     '9999'        VI1PD2
010800091112     C                   CLEAR                   VI1DD1
010900091112     C                   CLEAR                   VI1DD2
011000091112     C                   iter
011100091112     C                   enddo
011200091112     C*
011300931027     C                   SETON                                        LR
011400931111     C*
011500931111     C* PULIZIA CAMPI FORMATO 1 --------------------------------------*
011600931111     C     PUL01         BEGSR
011700120412     c                   movel     'S'           V1cclr
011800110822     c                   movel     'N'           Vi1CMG
011900931126     C* CARICO L1
012000950125     C                   CLEAR                   DSUL06
012100950125     C                   MOVE      '£1'          D06COD
012200950125     C                   MOVEL     SIMFEL        D06KEY
012300950125     C                   MOVEL     DSUL06        KPJBU
012400950125     C*
012500950125     C                   CALL      'TRUL06R'
012600950125     C                   PARM                    KPJBA
012700950125     C                   MOVEL     KPJBU         DSUL06
012800950125     C                   MOVEA     LIN           L1
012900931126     C*
013000950125     C***
013100950125     C* VEDO SE SONO SIMFEL O REMOTO
013200950125     C***
013300091112     C                   EVAL      Vi1PF1 = dutPOU
013400091112     C                   EVAL      Vi1PF2 = dutPOU
013500091112     C                   IF        dutlpo = '2' OR dutlpo = *BLANK
013600950125     C                   SETON                                        89
013700950125     C*
013800950125     C* CARICO TABELLA FILIALI GESTITE £6
013900950125     C                   CLEAR                   DSUL06
014000950125     C                   MOVE      '£6'          D06COD
014100020701     C*                  MOVEL     REMFIL        D06KEY
014200091112     C                   MOVEL     dutPOU        D06KEY
014300950125     C                   MOVEL     DSUL06        KPJBU
014400950125     C*
014500950125     C                   CALL      'TRUL06R'
014600950125     C                   PARM                    KPJBA
014700950125     C                   MOVEL     KPJBU         DSUL06
014800950125     C                   MOVEA     LIN           L6
014900950125     C*
015000950125     C                   ELSE
015100950125     C* SONO SIMFEL
015200950125     C                   MOVEL     SIMFEL        VI1PF1
015300991228     C                   MOVEL     SIMFEL        VI1PF2
015400950125     C                   ENDIF
015500991228     C                   MOVEL     '5A'          COD
015600991228     C                   MOVEL(P)  '1'           KEY
015700991228     C     KTAB          CHAIN     TABEL                              30
015800991228     C  N30              MOVEL     TBLUNI        DS5A
015900991228     C   30              CLEAR                   DS5A
016000000203     C                   TIME                    W0140            14 0
016100000203     C* UDATE IN GGMMAAAA
016200000203     C                   MOVE      W0140         WDTGIO            8 0
016300000203     C                   MOVE      WDTGIO        ANNOCO            4 0
016400000203     C     ANNOCO        ADD       1             ANNOPI            4 0
016500000203     C     ANNOCO        SUB       1             ANNOME            4 0
016600950125     C*
016700931111     C                   ENDSR
016800900528     C*
016900900608     C*--- CONTROLLI FORMATO1 ----------------------------------------*
017000900528     C     CONTR1        BEGSR
017100931111     C                   SETOFF                                       90
017200931126     C*
017300931126     C* D A T A   D I S T I N T A: DAL
017400931126     C                   MOVE      VI1DD1        G02DAT
017500931126     C                   MOVEL     *BLANK        G02ERR
017600950125     C                   CALL      'XSRDA8'
017700931126     C                   PARM                    WLBDAT
017800931126    1C     G02ERR        IFEQ      '1'
017900931126     C                   SETON                                        43  90
018000091112     C                   leavesr
018100931126    1C                   END
018200091112     C                   Z-ADD     G02INV        parDD1
018300950125     C                   Z-ADD     G02DAT        VI1DD1
018400991228     C                   Z-ADD     G02TGI        DD1GIO
018500011115     C                   z-add     vi1dd1        an1               4 0
018600000203     C* CONTROLLO L'ANNO
018700000203     C                   Z-ADD     G02DAT        W0040             4 0
018800000203     C     W0040         IFGT      ANNOPI
018900000203     C     W0040         ORLT      ANNOME
019000000203     C                   SETON                                        43  90
019100091112     C                   leavesr
019200000203    1C                   END
019300931126     C*
019400021106     C* comunque imposta il 2 campo data
019500091112     C                   Z-ADD     pardd1        pardd2
019600931126    1C     VI1DD2        IFGT      0
019700931126     C                   MOVE      VI1DD2        G02DAT
019800931126     C                   MOVEL     *BLANK        G02ERR
019900950125     C                   CALL      'XSRDA8'
020000931126     C                   PARM                    WLBDAT
020100931126    2C     G02ERR        IFEQ      '1'
020200931126     C                   SETON                                        44  90
020300091112     C                   leavesr
020400931126    2C                   END
020500091112     C                   Z-ADD     G02INV        parDD2
020600950125     C                   Z-ADD     G02DAT        VI1DD2
020700991228     C                   Z-ADD     G02TGI        DD2GIO
020800011115     C                   z-add     vi1dd2        an2               4 0
020900000203     C* CONTROLLO L'ANNO
021000091112     C                   Z-ADD     G02DAT        w0040             4 0
021100011115     c     an1           ifne      an2
021200011115     C                   SETON                                        56  90
021300091112     C                   leavesr
021400011115     c                   end
021500000203     C     W0040         IFGT      ANNOPI
021600000203     C     W0040         ORLT      ANNOME
021700000203     C                   SETON                                        44  90
021800091112     C                   leavesr
021900000203    1C                   END
022000931126     C* DATA AL NO < DATA DAL
022100091112    2C     parDD2        IFLT      parDD1
022200931126     C                   SETON                                        45  90
022300091112     C                   leavesr
022400931126    2C                   ENDIF
022500991228     C* I GIORNI TRA UNA DATA E L'ALTRA NON DEVONO SUPERARE
022600991228     Q* QUELLI DI PULIZIA + 10
022700991228     C     §5AFTT        ADD       10            WGIORN            4 0
022800991228     C**
022900991228     C     DD2GIO        SUB       DD1GIO        WRESTO
023000991228     C**
023100991228    2C     WRESTO        IFGT      WGIORN
023200991228     C                   SETON                                        55  90
023300091112     C                   leavesr
023400991228    2C                   ENDIF
023500991228     C**
023600991228    1C                   ENDIF
023700931123     C*
023800031023     C* Se non è un utente EDPxxx controlla la filiale inserita
023900091116     c                   if        primi_tre <> 'EDP' and dutlpo <> 'S'
024000031023     C*
024100950125     C* F I L I A L E    P A D R O N C I N O    D A L
024200950125     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
024300950125    1C     *IN89         IFEQ      *OFF
024400950125     C     VI1PF1        IFNE      SIMFEL
024500080109     C* non può essere (0) la filiale
024600080109     c                   if        VI1PF1 = 0
024700080109     c                   z-add     simfel        VI1PF1
024800080109     c                   end
024900950125     C     VI1PF1        LOOKUP    L1                                     35
025000950125     C  N35              SETON                                        5390
025100091112     C  N35              leavesr
025200950125     C                   ENDIF
025300950125     C*
025400950125   X1C                   ELSE
025500020701     C                   MOVEL     VI1PF1        WFIL              3 0
025600091112     C     WFIL          IFNE      dutPOU
025700950125     C     VI1PF1        LOOKUP    L6                                     35
025800950125     C  N35              SETON                                        5390
025900091112     C  N35              leavesr
026000950125     C                   ENDIF
026100950125    1C                   ENDIF
026200031023     c                   end
026300021015     c                   z-add     vi1pf1        vi1pf2
026400031023     C* Se non è un utente EDPxxx controlla la filiale inserita
026500091116     c                   if        primi_tre <> 'EDP' and dutlpo <> 'S'
026600950125     C* F I L I A L E    P A D R O N C I N O    A L
026700950125    1C     *IN89         IFEQ      *OFF
026800950125     C     VI1PF2        IFNE      SIMFEL
026900950125     C     VI1PF2        LOOKUP    L1                                     35
027000950125     C  N35              SETON                                        5490
027100091112     C  N35              leavesr
027200950125     C                   ENDIF
027300950125   X1C                   ELSE
027400020701     C                   MOVEL     VI1PF2        WFIL
027500091112     C     WFIL          IFNE      dutPOU
027600950125     C     VI1PF2        LOOKUP    L6                                     35
027700950125     C  N35              SETON                                        5490
027800091112     C  N35              leavesr
027900950125     C                   ENDIF
028000950125    1C                   ENDIF
028100031023     c                   end
028200950131     C* CONTROLLO CONGRUENZA FILIALI DAL-AL
028300950131    1C     VI1PF1        IFNE      VI1PF2
028400950131     C                   SETON                                        4890
028500091112     C                   leavesr
028600950131    1C                   ENDIF
028700931116     C* P A D R O N C I N O
028800931116     C* RICERCA
028900931116     C     '?'           SCAN      VI1PD1                                 90
029000931116     C*
029100931126    1C     *IN90         IFEQ      *ON
029200021203     C                   Z-ADD     VI1PF1        D24FIL
029300931126     C                   EXSR      RICPAD
029400931126     C*
029500021203     C     D24PDR        IFNE      0
029600021203     C                   MOVEL     D24PDR        VI1PF1
029700931116     C                   MOVEL     *ZEROS        VI1PD1
029800021203     C                   MOVE      D24PDR        VI1PD1
029900021203     C                   MOVEL     D24RSC        VI1DP1
030000950125     C                   ENDIF
030100091112     C                   leavesr
030200931126    1C                   ENDIF
030300931126     C* PADRONCINO AL
030400931126     C* RICERCA
030500931126     C     '?'           SCAN      VI1PD2                                 90
030600931126     C*
030700931126    1C     *IN90         IFEQ      *ON
030800021203     C                   Z-ADD     VI1PF2        D24FIL
030900931126     C                   EXSR      RICPAD
031000931126     C*
031100021203     C     D24PDR        IFNE      0
031200021203     C                   MOVEL     D24PDR        VI1PF2
031300931126     C                   MOVEL     *ZEROS        VI1PD2
031400021203     C                   MOVE      D24PDR        VI1PD2
031500950125     C                   ENDIF
031600091112     C                   leavesr
031700931126    1C                   ENDIF
031800931126     C*
031900931126     C*
032000931126    1C     VI1PD2        IFNE      *BLANKS
032100931126     C     VI1PD2        ANDNE     *ZEROS
032200931126     C                   TESTN                   VI1PD2               30
032300931126     C  N30              SETON                                        4790
032400931126     C                   ENDIF
032500931116     C*
032600931126    1C     VI1PD1        IFNE      *BLANKS
032700931126     C     VI1PD1        ANDNE     *ZEROS
032800931126     C                   TESTN                   VI1PD1               30
032900931126     C  N30              SETON                                        4190
033000931116     C*
033100931123     C* SE NE HO IMMESSO SOLTANTO UNO CONTROLLO L'ESISTENZ
033200931126    2C     VI1PD2        IFEQ      *BLANKS
033300931126     C     VI1PD2        OREQ      *ZEROS
033400931126     C*
033500021203     C     KAPDV         CHAIN     FIAPD000                           30
033600931015     C*
033700931126    3C     *IN30         IFEQ      *ON
033800020520     C* Tolto il controllo sul padroncino annullato
033900020520     C                   SETON                                        4190
034000091112     C                   leavesr
034100931126    3C                   ENDIF
034200931129     C*
034300100223     C                   MOVEL     APDRSf        VI1DP1
034400931123     C*
034500931123   X2C                   ELSE
034600931126     C*
034700931123     C* PADRONCINO AL  <  MINORE PADRONCINO DAL
034800950125    3C     VI1P2         IFLT      VI1P1
034900931123     C                   SETON                                        4290
035000091112     C                   leavesr
035100931126    3C                   ENDIF
035200931126    2C                   ENDIF
035300931123    1C                   ENDIF
035400010906     C*
035500010906     C* PADRONCINO AL E PADRONCINO DAL A ZERO
035600010906     C     VI1PD1        IFEQ      *ZEROS
035700010906     C     VI1PD2        ANDEQ     *ZEROS
035800010906     C                   SETON                                        4090
035900091112     C                   leavesr
036000010906     C                   ENDIF
036100110822     C*
036200110822     C* x il calcolo del Costo Medio Giornaliero si deve pulire il WFile
036300120411     C**********         IF        vi1CMG ='S' and v1cCLR <> 'S'
036400120411     C*********          SETON                                        5990
036500120411     C*********          leavesr
036600120411     C**********         ENDIF
036700910320     C*
036800091112     C                   ENDSR
036900931126     C*
037000931126     C* RICERCA CODICE PADRONCINO ------------------------------------*
037100931126     C     RICPAD        BEGSR
037200021203     C                   MOVEL     *BLANKS       D24RSC
037300021203     C                   Z-ADD     0             D24PDR
037400021203     C                   MOVEL     'R'           D24FLG
037500021203     C                   MOVEL     'A'           D24TIP
037600021203     C                   MOVEL(P)  FNLV24DS      KPJBU
037700950125     C                   CALL      'FNLV24R'
037800931126     C                   PARM                    KPJBA
037900021203     C                   MOVEL     KPJBU         FNLV24DS
038000931126     C                   ENDSR
