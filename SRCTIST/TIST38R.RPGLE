000100980304     H*------------------------------------------------------------------------*
000200050704     H* STATISTICA UTILIZZO SERVIZI ON-LINE
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800050704     FAZORG01L  IF   E           K DISK
000900050711     FTABEL00F  IF   E           K DISK
001000050711     FTNTBE01L  IF   E           K DISK
001100130902     fAZCMM01L  if   e           k disk
001200050721     FWFSUV00F  IF   E           K DISK
001300050721     FSISTL02L  IF   E           K DISK
001400050704     FTIST38D   CF   E             WORKSTN INFDS(DEVDS1)
001500050704     F                                     SFILE(ST38DMS:MSGULT)
001600980304     D*------------------------------------------------------------------------*
001700980304     D* SCHIERE
001800980304     D*------------------------------------------------------------------------*
001900041220     D MSG             S             80    DIM(10) CTDATA PERRCD(1)
002000980304     D*------------
002100980304     D* DESCRIZIONI TASTI FUNZIONALI
002200980304     D*------------
002300980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002400980304     D*------------
002500980304     D* FUNZIONI DA CARICARE A VIDEO
002600980304     D*------------
002700980304     D FUV             S             19    DIM(24)
002800980304     D*------------------------------------------------------------------------*
002900980304     D* INPUT
003000980304     D*------------------------------------------------------------------------*
003100980304     D*------------
003200980304     D* RIDEFINIZIONE FUNZIONI VIDEO
003300980304     D*------------
003400980304     D FUDDS           DS
003500980304     D  FUDDE1                 1      1
003600980304     D  FUDFUN                 2      3
003700980304     D  FUDDE2                 4     19
003800980304     D  FUDFI0                20     34
003900980304     D  FUDATT                35     35
004000980304     D  FUDFI1                36     80
004100980304     D  FUDDES                 1     19
004200980304     D  FUDRIG                 1     80
004300980304     D*------------
004400980304     D* DS GESTIONE TASTI FUNZIONALI
004500980304     D*------------
004600980304     D DSKEY         E DS
004700980304     D DEVDS1          DS
004800980304     D  KEY                  369    369
004900980304     D*------------
005000980304     D* DS "XMSG" - GESTIONE MESSAGGI
005100980304     D*------------
005200980304     D DSMSG         E DS
005300041221     D*------------------
005400041221     D* ARCHITETTURA
005500041221     D*------------------
005600041221     D KPJBA         E DS
005700050711     D*------------------
005800050711     D* TABELLA DATE "DIVERSE"
005900050711     D*------------------
006000050711     D DSDD          E DS
006100050714     D*------------------
006200050714     D* TABELLA STRUTTURA CPMMERCIALE (DISTRETTO/AREA/FILIALE/COMMERCIALE)
006300050714     D*------------------
006400050714     D DS05          E DS
006500050714     D DS17          E DS
006600050704     D*-------------------
006700050704     D* DS REPERIMENTO NUMERO GG LAVORATIVI TRA 2 DATE
006800050704     D*-------------------
006900050704     D WL08            DS
007000050704     D  L08DA                  1      8  0
007100050704     D  L08A                   9     16  0
007200050704     D  L08L                  17     21  0
007300050704     D*------------------
007400050704     D* DS "XSRDA8" - CONTROLLA DATA (8)
007500050704     D*------------------
007600050704     D WLBDA8          DS                  INZ
007700050704     D  G08DAT                 1      8  0
007800050704     D  G08INV                 9     16  0
007900050704     D  G08ERR                17     17
008000050704     D  G08TGI                18     22  0
008100050704     D*-------------------
008200050704     D* REPERIMENTO DATI UTENTE
008300050704     D*-------------------
008400050704     D TIBS34DS      E DS                                                       *Profili utente
008500050704     D DDATIUTE      E DS                                                       *Dati utente
008600050704     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008700050711     D DUTE01        E DS
008800050711     D TRUL31DS      E DS
008900050711     D wAbi            S                   like(UTEAUT)
009000050711     D skDIG           S              1    dim(20)
009100050711     D skARG           S              3    dim(50)
009200050711     D skPOG           S              3    dim(250)
009300980304     D*------------------
009400041221     D* DS PASSAGGIO PARAMETRI
009500980304     D*------------------
009600050706     D PARAM           DS
009700050706     D  PARDAI                        8  0
009800050706     D  PARDAF                        8  0
009900050706     D  PARCAT                        1
010000050706     D  PARCATFLG                     1
010100050706     D  PARTIP                        2
010200050706     D  PARSPED                       6  0
010300050706     D  PARGIO                        3  0
010400050706     D  PARFILE                       1
010500050706     D  PARLAY                        1
010600050706     D  PARDIV                        1
010700050706     D  PARARE                        3  0
010800050706     D  PARFIL                        3  0
010900050721     D  PARAGE                        7  0
011000050721     D*------------------
011100050721     D* VARIABILI D WRK
011200050721     D*------------------
011300050721     D dataMimDFT      S               D   datfmt(*iso)
011400050721     D depVIDDAI       S                   like(VIDDAI) inz(*zeros)
011500050721     D depVIDDAF       S                   like(VIDDAF) inz(*zeros)
011600050721     D depPERIODODA    S              6    inz(*zeros)
011700050721     D depPERIODOA     S              6    inz(*zeros)
011800050721     D depDataMin      S                   like(VIDDAI) inz(*zeros)
011900050721     D savMinDat       S                   like(STLDAT) inz(*zeros)
012000050721     D savMaxDat       S                   like(STLDAT) inz(*zeros)
012100060503     d w0030           s              3  0
012200060503     d xx              s              3  0
012300060503     d yy              s              3  0
012400980304     C*------------------------------------------------------------------------*
012500980304     C* MAIN LINES
012600980304     C*------------------------------------------------------------------------*
012700041231     C*
012800041231     C* INIZIALIZZAZIONI INIZIALI
012900041231     C                   EXSR      AZZVID
013000980304     C*
013100980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013200041221     C     F03L00        DOUEQ     '1'
013300980304     C     F12L00        OREQ      '1'
013400980304     C     FINL00        OREQ      '1'
013500980304     C*
013600980304     C* EMETTE FORMATO VIDEO
013700980304     C                   EXSR      BUTFMR
013800980304     C*
013900980304     C* GESTIONE TASTI FUNZIONALI
014000980304     C                   EXSR      FUNCON
014100041221     C                   ENDDO
014200980304     C*
014300980304     C                   SETON                                        LR
014400980304     C*------------------------------------------------------------------------*
014500980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014600980304     C*------------------------------------------------------------------------*
014700980304     C     BUTFMR        BEGSR
014800980304     C*
014900980304     C* EMETTE LA TESTATA
015000050704     C                   WRITE     ST38D01
015100980304     C*
015200980304     C* SPEDISCE EVENTUALE MESSAGGIO
015300980304     C                   EXSR      SNDMSG
015400980304     C*
015500980304     C* EMETTE LE FUNZIONI
015600050704     C                   WRITE     ST38D02
015700980304     C*
015800980304     C* EMETTE IL VIDEO
015900050704     C                   EXFMT     ST38D03
016000980304     C*
016100980304     C* REIMPOSTA FLAG DI ERRORE
016200980304     C                   MOVEL     '0'           ERRL00
016300050107     C*
016400050107     C* SPEGNE INDICATORI D ERRORE A VIDEO
016500050704     C                   SETOFF                                       507071
016600980304     C*
016700980304     C                   ENDSR
016800980304     C*------------------------------------------------------------------------*
016900980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017000980304     C*------------------------------------------------------------------------*
017100980304     C     SNDMSG        BEGSR
017200980304     C*
017300980304     C* PULISCE IL SFILE MESSAGGI
017400980304     C                   MOVEL     '1'           *IN40
017500050704     C                   WRITE     ST38DMC
017600980304     C                   MOVEL     '0'           *IN40
017700980304     C*
017800980304     C* VISUALIZZA IL MESSAGGIO
017900980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
018000980304     C                   CALL      'XMSG'        XMSGPA
018100041220     C     DSMSER        IFEQ      *ZEROS
018200980304     C                   MOVEL     DSMSKE        MSGKEY
018300041220     C                   ENDIF
018400980304     C                   Z-ADD     1             MSGULT
018500050704     C                   WRITE     ST38DMS
018600050704     C                   WRITE     ST38DMC
018700980304     C*
018800980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018900980304     C                   MOVEL     *BLANKS       DSMSMS
019000980304     C*
019100980304     C                   ENDSR
019200980304     C*------------------------------------------------------------------------*
019300980304     C* ENTGES - GESTIONE ENTER -> INVIO
019400980304     C*------------------------------------------------------------------------*
019500980304     C     ENTGES        BEGSR
019600980304     C*
019700050107     C                   EXSR      DECODI
019800041220     C*
019900980304     C                   ENDSR
020000980304     C*------------------------------------------------------------------------*
020100980304     C* F03GES - GESTIONE F03 -> FINE
020200980304     C*------------------------------------------------------------------------*
020300980304     C     F03GES        BEGSR
020400980304     C*
020500980304     C* IMPOSTA "PREMUTO F03"
020600980304     C                   MOVEL     '1'           F03L00
020700980304     C*
020800980304     C                   ENDSR
020900041221     C*------------------------------------------------------------------------*
021000041221     C* F06GES - GESTIONE F06 -> CONFERMA ELABORAZIONE
021100041221     C*------------------------------------------------------------------------*
021200041221     C     F06GES        BEGSR
021300050107     C*
021400050107     C* ESEGUO ROUTINE D VERIFICA DATI A VIDEO
021500050107     C                   EXSR      DECODI
021600041221     C*
021700050107     C* SE NO ERRORI => SOTTOMETTO L'ELABORAZOINE
021800050107     C                   IF        ERRL00 = '0'
021900050704     C                   MOVEL     'ST39'        KCOAZ
022000050704     C                   EVAL      PARDAI    = DAIVID
022100050704     C                   EVAL      PARDAF    = DAFVID
022200041231     C                   EVAL      PARCAT    = VIDCAT
022300041231     C                   EVAL      PARCATFLG = VIDCATFLG
022400041231     C                   EVAL      PARTIP    = VIDTIP
022500041231     C                   EVAL      PARSPED   = VIDSPED
022600050704     C                   EVAL      PARFILE   = VIDFILE
022700050706     C                   EVAL      PARLAY    = VIDLAY
022800050706     C                   EVAL      PARDIV    = VIDDIV
022900050706     C                   EVAL      PARARE    = VIDARE
023000050706     C                   EVAL      PARFIL    = VIDFIL
023100050706     C                   EVAL      PARAGE    = VIDAGE
023200041221     C                   MOVEL     PARAM         KPJBU
023300060503     C***                CALL      'TIST39C'
023400060503 MOM C                   CALL      'BCH10'
023500041221     C                   PARM                    KPJBA
023600041221     C*
023700041221     C* DOPO LA SOTTOMISSION ESCO DAL *PGM
023800041221     C                   MOVEL     '1'           F03L00
023900050107     C                   ENDIF
024000041221     C*
024100041221     C                   ENDSR
024200980304     C*------------------------------------------------------------------------*
024300980304     C* F12GES - GESTIONE F12 -> RITORNO
024400980304     C*------------------------------------------------------------------------*
024500980304     C     F12GES        BEGSR
024600980304     C*
024700980304     C* IMPOSTA "PREMUTO F12"
024800980304     C                   MOVEL     '1'           F12L00
024900980304     C*
025000980304     C                   ENDSR
025100980304     C*------------------------------------------------------------------------*
025200980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025300980304     C*------------------------------------------------------------------------*
025400980304     C     F24GES        BEGSR
025500980304     C*
025600980304     C                   EXSR      FUNGES
025700980304     C*
025800980304     C                   ENDSR
025900050107     C*------------------------------------------------------------------------*
026000050107     C* DECODI - CONTROLLO DATI A VIDEO
026100050107     C*------------------------------------------------------------------------*
026200050107     C     DECODI        BEGSR
026300050107     C*
026400050107     C* RE-INIZIALIZZAZIONE FLAG ERRORE
026500050107     C                   MOVEL     '0'           ERRL00
026600050107     C*
026700050107     C* SPENGO INDICATORI D ERRORE A VIDEO
026800050704     C                   SETOFF                                       507071
026900050711     C                   SETOFF                                       545556
027000050711     C                   SETOFF                                       53
027100050107     C*
027200050107     C* CONTROLLO PARAMETRO INCLISIONE/ESCLUSIONE CATEGORIA SELEZIONATA
027300050107     C                   IF        VIDCAT    <> *blanks AND
027400050107     C                             VIDCATFLG =  *blanks
027500050107     C                   MOVEL     '1'           ERRL00
027600050107     C                   SETON                                        50
027700050107     C                   MOVEL     MSG(2)        DSMSMS
027800050704     C                   ENDIF
027900050704     C*
028000050711     C* CONTROLLO DATA INIZIALE
028100050704     C     *LIKE         DEFINE    VIDDAI        DAIVID
028200050704     C     ERRL00        IFEQ      '0'
028300050714     C                   IF        VIDDAI = *zeros
028400050714     C                   EVAL      VIDDAI = depVIDDAI
028500050714     C                   ELSE
028600050704     C                   Z-ADD     VIDDAI        WDA8
028700050704     C                   Z-ADD     *ZEROS        DA8W
028800050704     C                   EXSR      CHKDA8
028900050704     C                   Z-ADD     WDA8          VIDDAI
029000050704     C                   Z-ADD     DA8W          DAIVID
029100050704     C     ERRL00        IFEQ      '1'
029200050711     C                   MOVEL     '1'           ERRL00
029300050704     C                   SETON                                        70
029400050711     C                   MOVEL     MSG(3)        DSMSMS
029500050711     C                   ELSE
029600050711     C* VERIFICA SE DATA COMPRESA NEL RANGE PILOTATO DA TABELLA
029700050711     C                   EVAL      tbeCOD = 'SDD'
029800050711     C                   EVAL      tbeKE1 = '1'
029900050711     C     KEYTBE01_P    CHAIN     TNTBE01L
030000050711     C                   IF        %found(TNTBE01L)
030100050711     C                   EVAL      DSDD = tbeUNI
030200050711     C                   IF        DAIVID < §SDDVAS1
030300050711     C                   MOVEL     '1'           ERRL00
030400050711     C                   SETON                                        70
030500050711     C                   MOVEL     MSG(8)        DSMSMS
030600050711     C                   EVAL      DSMSMS = %trim(DSMSMS) + ' ' +
030700050711     C                                      %editw(§SDDVAS1:'    /  /  ')
030800050704     C                   ENDIF
030900050704     C                   ENDIF
031000050714     C                   ENDIF
031100050711     C                   ENDIF
031200050711     C                   ENDIF
031300050704     C*
031400050711     C* CONTROLLO DATA FINALE
031500050704     C     *LIKE         DEFINE    VIDDAF        DAFVID
031600050704     C     ERRL00        IFEQ      '0'
031700050714     C                   IF        VIDDAF = *zeros
031800050714     C                   EVAL      VIDDAF = depVIDDAF
031900050714     C                   ELSE
032000050704     C                   Z-ADD     VIDDAF        WDA8
032100050704     C                   Z-ADD     *ZEROS        DA8W
032200050704     C                   EXSR      CHKDA8
032300050704     C                   Z-ADD     WDA8          VIDDAF
032400050704     C                   Z-ADD     DA8W          DAFVID
032500050704     C     ERRL00        IFEQ      '1'
032600050711     C                   MOVEL     '1'           ERRL00
032700050704     C                   SETON                                        71
032800050711     C                   MOVEL     MSG(3)        DSMSMS
032900050711     C                   ELSE
033000050711     C* VERIFICA SE DATA COMPRESA NEL RANGE PILOTATO DA TABELLA
033100050711     C                   EVAL      tbeCOD = 'SDD'
033200050711     C                   EVAL      tbeKE1 = '1'
033300050711     C     KEYTBE01_P    CHAIN     TNTBE01L
033400050711     C                   IF        %found(TNTBE01L)
033500050711     C                   EVAL      DSDD = tbeUNI
033600050711     C                   IF        DAIVID > §SDDVAS2
033700050711     C                   MOVEL     '1'           ERRL00
033800050711     C                   SETON                                        71
033900050711     C                   MOVEL     MSG(9)        DSMSMS
034000050711     C                   EVAL      DSMSMS = %trim(DSMSMS) + ' ' +
034100050711     C                                      %editw(§SDDVAS2:'    /  /  ')
034200050711     C                   ENDIF
034300050711     C                   ENDIF
034400050714     C                   ENDIF
034500050704     C                   ENDIF
034600050704     C                   ENDIF
034700050704     C*
034800050704     C* CONTROLLO RANGE DATE
034900050704     C     ERRL00        IFEQ      '0'
035000050704     C                   IF        DAIVID > DAFVID
035100050711     C                   MOVEL     '1'           ERRL00
035200050704     C                   SETON                                        71
035300050704     C                   SETON                                        70
035400050711     C                   MOVEL     MSG(3)        DSMSMS
035500050704     C                   ENDIF
035600050704     C                   ENDIF
035700050704     C*
035800050711     C* CALCOLO GIORNI LAVORATIVI
035900050704     C     ERRL00        IFEQ      '0'
036000050704     C                   CLEAR                   WL08
036100050704     C                   Z-ADD     DAIVID        L08DA
036200050704     C                   Z-ADD     DAFVID        L08A
036300050704     C                   CALL      'XSRLAV8'
036400050704     C                   PARM                    WL08
036500050704     C                   MOVE      L08L          PARGIO
036600050704     C                   ENDIF
036700050711     C*
036800050711     C* CONTROLLO STRUTTURA COMMERCIALE
036900050711     C     ERRL00        IFEQ      '0'
037000050714     C                   EVAL      VIDDIVD = *blanks
037100050714     C                   IF        VIDDIV <> *blanks
037200050711     C     VIDDIV        LOOKUP    skDIG                                  30
037300050711     C                   IF        not %found
037400050711     C                   MOVEL     '1'           ERRL00
037500050711     C                   SETON                                        54
037600050711     C                   MOVEL     MSG(4)        DSMSMS
037700050714     C                   ELSE
037800050714     C                   EXSR      DECDIV
037900050711     C                   ENDIF
038000050711     C                   ENDIF
038100050711     C                   ENDIF
038200050107     C*
038300050711     C     ERRL00        IFEQ      '0'
038400050714     C                   EVAL      VIDARED = *blanks
038500050711     C                   IF        VIDARE <> *zeros
038600050711     C                   MOVEL(P)  VIDARE        wAlfaARE          3
038700050711     C     wAlfaARE      LOOKUP    skARG                                  30
038800050711     C                   IF        not %found
038900050711     C                   MOVEL     '1'           ERRL00
039000050711     C                   SETON                                        55
039100050711     C                   MOVEL     MSG(5)        DSMSMS
039200050714     C                   ELSE
039300050714     C                   EXSR      DECARE
039400050711     C                   ENDIF
039500050711     C                   ENDIF
039600050711     C                   ENDIF
039700050711     C*
039800050711     C     ERRL00        IFEQ      '0'
039900050714     C                   EVAL      VIDFILD = *blanks
040000050711     C                   IF        VIDFIL <> *zeros
040100050711     C                   MOVEL(P)  VIDFIL        wAlfaFIL          3
040200050711     C     wAlfaFIL      LOOKUP    skPOG                                  30
040300050711     C                   IF        not %found
040400050711     C                   MOVEL     '1'           ERRL00
040500050711     C                   SETON                                        56
040600050711     C                   MOVEL     MSG(6)        DSMSMS
040700050714     C                   ELSE
040800050714     C                   EXSR      DECFIL
040900050711     C                   ENDIF
041000050711     C                   ENDIF
041100050711     C                   ENDIF
041200050711     C*
041300050711     C     ERRL00        IFEQ      '0'
041400050714     C                   EVAL      VIDAGED = *blanks
041500050711     C                   IF        VIDAGE <> *zeros
041600130902     C     VIDage        CHAIN     AZCMM000
041700130902     C                   IF        not %found(AZCMM01L)
041800050711     C                   MOVEL     '1'           ERRL00
041900050711     C                   SETON                                        53
042000050711     C                   MOVEL     MSG(7)        DSMSMS
042100050711     C                   ELSE
042200050711     C*
042300050711     C                   MOVEL(P)  VIDAGE        wPoAge            3
042400050711     C     wPoAge        LOOKUP    skPOG                                  30
042500050711     C                   IF        not %found
042600050711     C                   MOVEL     '1'           ERRL00
042700050711     C                   SETON                                        53
042800050711     C                   MOVEL     MSG(7)        DSMSMS
042900050714     C                   ELSE
043000050714     C                   EXSR      DECAGE
043100050711     C                   ENDIF
043200050711     C                   ENDIF
043300050711     C                   ENDIF
043400050711     C                   ENDIF
043500050711     C*
043600050107     C                   ENDSR
043700050704     C*------------------------------------------------------------------------*
043800050704     C* CHKDA8 - CONTROLLA LA DATA (8)
043900050704     C*------------------------------------------------------------------------*
044000050704     C     CHKDA8        BEGSR
044100050704     C*
044200050704     C     *LIKE         DEFINE    DAIVID        WDA8
044300050704     C     *LIKE         DEFINE    DAFVID        DA8W
044400050704     C*
044500050704     C                   Z-ADD     WDA8          G08DAT
044600050704     C                   Z-ADD     *ZEROS        G08INV
044700050704     C                   MOVEL     '0'           G08ERR
044800050704     C                   CALL      'XSRDA8'
044900050704     C                   PARM                    WLBDA8
045000050704     C                   Z-ADD     G08DAT        WDA8
045100050704     C                   Z-ADD     G08INV        DA8W
045200050704     C     G08ERR        IFEQ      '1'
045300050704     C                   MOVEL     '1'           ERRL00
045400050704     C                   ENDIF
045500050704     C*
045600050704     C                   ENDSR
045700050714     C*------------------------------------------------------------------------*
045800050714     C* DECDIV - DECODIFICA DIVISIONE
045900050714     C*------------------------------------------------------------------------*
046000050714     C     DECDIV        BEGSR
046100050714     C*
046200050714     C                   MOVEL     1             TBLKUT
046300050714     C                   MOVE      '17'          TBLCOD
046400050714     C                   MOVE      *BLANKS       TBLKEY
046500050714     C                   MOVEL     VIDDIV        TBLKEY
046600050714     C     KEYTAB        CHAIN     TABEL                              97
046700050714     C     *IN97         IFEQ      '1'
046800050714     C     TBLFLG        OREQ      '*'
046900050714     C                   MOVEL     *ALL'*'       VIDDIVD
047000050714     C                   ELSE
047100050714     C                   MOVEL(P)  TBLUNI        DS17
047200050714     C                   MOVEL(P)  §17DES        VIDDIVD
047300050714     C                   ENDIF
047400050714     C*
047500050714     C                   ENDSR
047600050714     C*------------------------------------------------------------------------*
047700050714     C* DECARE - DECODIFICA AREA
047800050714     C*------------------------------------------------------------------------*
047900050714     C     DECARE        BEGSR
048000050714     C*
048100050714     C                   MOVEL     1             TBLKUT
048200050714     C                   MOVE      '05'          TBLCOD
048300050714     C                   MOVE      *BLANKS       TBLKEY
048400050714     C                   MOVEL     VIDARE        TBLKEY
048500050714     C     KEYTAB        CHAIN     TABEL                              97
048600050714     C     *IN97         IFEQ      '1'
048700050714     C     TBLFLG        OREQ      '*'
048800050714     C                   MOVEL     *ALL'*'       VIDARED
048900050714     C                   ELSE
049000050714     C                   MOVEL(P)  TBLUNI        DS05
049100050714     C                   MOVEL(P)  §05DES        VIDARED
049200050714     C                   ENDIF
049300050714     C*
049400050714     C                   ENDSR
049500050714     C*------------------------------------------------------------------------*
049600050714     C* DECFIL - DECODIFICA FILIALE/TERMINAL
049700050714     C*------------------------------------------------------------------------*
049800050714     C     DECFIL        BEGSR
049900050714     C*
050000050714     C                   Z-ADD     1             I
050100050714     C     VIDFIL        CHAIN     AZORG01L
050200050714     C                   IF        not %found(AZORG01L)
050300050714     C                   MOVEL     *ALL'*'       VIDFILD
050400050714     C                   ELSE
050500050714     C                   MOVEL(P)  ORGDES        VIDFILD
050600050714     C                   ENDIF
050700050714     C*
050800050714     C                   ENDSR
050900050714     C*------------------------------------------------------------------------*
051000050714     C* DECAGE - DECODIFICA COMMERCIALE
051100050714     C*------------------------------------------------------------------------*
051200050714     C     DECAGE        BEGSR
051300050714     C*
051400130902     C     VIDage        CHAIN     AZCMM000                           97
051500050714     C     *IN97         IFEQ      '1'
051600050714     C                   MOVEL     *ALL'*'       VIDAGED
051700050714     C                   ELSE
051800130902     C                   MOVEL(P)  CMMdes        VIDAGED
051900050714     C                   ENDIF
052000050714     C*
052100050714     C                   ENDSR
052200050704     C*--------------------------------------------------------------------------------------------*
052300050704     C* REPERISCE I DATI UTENTE
052400050704     C*--------------------------------------------------------------------------------------------*
052500050704     C     REPDATIUTE    BEGSR
052600050704     C*
052700050704     C* INIZIALIZZA VARIABILI DI WRK
052800050704     C                   CLEAR                   TIBS34DS
052900050704     C                   CLEAR                   AZUTEDS
053000050704     C                   CLEAR                   DDATIUTE
053100050704     C*
053200050704     C     *DTAARA       DEFINE    §azute        azuteds
053300050704     C     *DTAARA       DEFINE    §datiute      ddatiute
053400050704     C                   IN(E)     *DTAARA
053500050704     C                   IF        %Error
053600050704     C                   EVAL      I34Tla = 'L'
053700050704     C                   CALL      'TIBS34R'
053800050704     C                   PARM                    Tibs34Ds
053900050704     C                   IN        *DTAARA
054000050704     C                   ENDIF
054100050711     C*
054200050711     C* Verifica errori e autorità profilo
054300050711     C                   IF        DUTERR = 'E'
054400050711     C                   EVAL      FINL00 = '1'
054500050711     C                   ELSE
054600050711     C                   MOVEL(P)  UTEFAF        DUTE01
054700050711     C                   IF        §UTECLI <> *blanks
054800050711     C                   EVAL      wAbi = §UTECLI
054900050711     C                   ELSE
055000050711     C* se non c'è l'abilitazione
055100050711     C* --> se 1° livello, abilitazioni al terminal
055200050711     C*     se 2° livello, abilitazioni al punto operativo
055300050711     C*     se sede è impossibile ma se capita mando a fine il pgm
055400050711     C                   IF        UTEAUT = *blanks
055500050711     C                   IF        DUTLPO = '1'
055600050711     C                   EVAL      wAbi   = 'TP'
055700050711     C                   ENDIF
055800050711     C                   IF        DUTLPO = '2'
055900050711     C                   EVAL      wAbi   = 'PO'
056000050711     C                   ENDIF
056100050711     C                   IF        DUTLPO = 'S'
056200050711     C                   EVAL      wAbi   = 'AZ'
056300050711     C                   ENDIF
056400050711     C                   ELSE
056500050711     C                   EVAL      wAbi = UTEAUT
056600050711     C                   ENDIF
056700050711     C                   ENDIF
056800050711     C                   ENDIF
056900050711     C*
057000050711     C* Reperimento dei P.O. gestibili dall'utente
057100050711     C                   clear                   TRUL31DS
057200060503     C**!!!              clear                   TRUL31DS2
057300050711     C                   eval      I31abi = wAbi
057400050711     C                   eval      I31cdi = DUTdis
057500050711     C                   eval      I31car = DUTare
057600050711     C                   eval      I31cpo = DUTpou
057700050711     C                   call      'TRUL31R'
057800050711     C                   parm                    KPJBA
057900050711     C                   parm                    TRUL31DS
058000060503     C**!!!              parm                    TRUL31DS2
058100050711     C                   if        O31pog > *zeros
058200060503     C**!!!              movea     O31dig        skDIG
058300060503     C**!!!              movea     O31arg        skARG
058400050711     C                   movea     O31pog        skPOG
058500050711     C                   else
058600050711     C                   movel     '1'           FINL00
058700060503     c                   leavesr
058800050711     C                   endif
058900060503
059000060503     c                   clear                   skdig
059100060503     c                   clear                   skarg
059200060503      * mi carico la sk dei distretti e aree relativi ai p.o. gestiti dall'utente
059300060503     c                   do        250           xx
059400060503     c                   move      skpog(xx)     w0030
059500060503     c                   if        skpog(xx) = *zeros
059600060503     c                   leave
059700060503     c                   endif
059800060503     c     w0030         chain     azorg01l
059900060503     c                   if        %found(azorg01l)
060000060503      * memorizzo il distretto se non è già stato memorizzato
060100060503     c     orgfl3        lookup    skdig                                  30
060200060503     c                   if        not %found
060300060503     c                   z-add     1             yy
060400060503     c     *blanks       lookup    skdig(yy)                              30
060500060503     c                   if        %found
060600060503     c                   movel     orgfl3        skdig(yy)
060700060503     c                   endif
060800060503     c                   endif
060900060503      * memorizzo l'area se non è già stata memorizzata
061000060503     c                   movel(p)  orgcar        walfaare
061100060503     c     walfaare      lookup    skarg                                  30
061200060503     c                   if        not %found
061300060503     c                   z-add     1             yy
061400060503     c     *blanks       lookup    skarg(yy)                              30
061500060503     c                   if        %found
061600060503     c                   movel     walfaare      skarg(yy)
061700060503     c                   endif
061800060503     c                   endif
061900060503     c                   endif
062000060503     c                   enddo
062100060503
062200050711     C*
062300050711     C                   ENDSR
062400980304     C*------------------------------------------------------------------------*
062500980304     C* *INZSR - OPERAZIONI INIZIALI
062600980304     C*------------------------------------------------------------------------*
062700980304     C     *INZSR        BEGSR
062800980304     C*---
062900980304     C* RICEVIMENTO PARAMETRI
063000980304     C*---
063100980304     C     *ENTRY        PLIST
063200980304     C                   PARM                    KPJBA
063300980304     C*
063400980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
063500980304     C     XMSGPA        PLIST
063600980304     C                   PARM                    DSMSMS
063700980304     C                   PARM                    DSMSPG
063800980304     C                   PARM                    DSMSKE
063900980304     C                   PARM                    DSMSER
064000980304     C*
064100980304     C* IMPOSTA LA DS -TASTI PREMUTI-
064200980304     C                   CALL      'XKEY'
064300980304     C                   PARM                    DSKEY
064400980304     C*---
064500980304     C* VARIABILI NON RIFERITE AL DATA BASE
064600980304     C*---
064700980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
064800980304     C*---
064900980304     C* VARIABILI DI CONTROLLO
065000980304     C*---
065100980304     C                   MOVEL     '0'           FINL00            1
065200980304     C                   MOVEL     '0'           F03L00            1
065300980304     C                   MOVEL     '0'           F12L00            1
065400980304     C                   MOVEL     '0'           ERRL00            1
065500980304     C*
065600980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
065700050704     C                   MOVEL     'TIST38R'     DSMSPG
065800050704     C                   MOVEL     'TIST38R'     MSGQUE
065900980304     C                   MOVEL     *BLANKS       MSGKEY
066000980304     C                   MOVEL     *BLANKS       DSMSMS
066100980304     C*
066200980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
066300980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
066400980304     C                   EXSR      FUNGES
066500050711     C*
066600050711     C* DEFINIZIONE CHIAVI
066700050711     C* TABEL00F - COMPLETA
066800050711     C     KEYTAB        KLIST
066900050711     C                   KFLD                    tblKUT
067000050711     C                   KFLD                    tblCOD
067100050711     C                   KFLD                    tblKEY
067200050711     C* TNTBE01L - PARZIALE
067300050711     C     KEYTBE01_P    KLIST
067400050711     C                   KFLD                    tbeCOD
067500050711     C                   KFLD                    tbeKE1
067600050711     C*
067700050711     C* EFFETTUO SUBUTI TEST SE UTENTE E' EDP*
067800050711     C                   IF        %subst(KNMUS:1:3) = 'EDP'
067900050711     C                   SETON                                        52
068000050711     C                   ENDIF
068100050711     C*
068200050711     C* REPERIMENTO DATI UTENTE CORRENTE
068300050711     C                   EXSR      REPDATIUTE
068400980304     C*
068500980304     C                   ENDSR
068600980304     C*------------------------------------------------------------------------*
068700980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
068800980304     C*------------------------------------------------------------------------*
068900980304     C     FUNGES        BEGSR
069000980304     C*
069100980304     C* PULISCO CAMPI DI LAVORO
069200980304     C                   Z-ADD     0             W                 5 0
069300980304     C*
069400980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
069500980304     C* E CARICO TABELLA FUNZIONI ATTIVE
069600980304     C     1             DO        24            I
069700980304     C                   MOVEL     FUD(I)        FUDRIG
069800980304     C*
069900980304     C* PULIZIA FUNZIONI PRECEDENTI
070000980304     C                   MOVEL     *BLANKS       FUV(I)
070100980304     C*
070200980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
070300980304     C* (SALTA F24)
070400980304     C     FUDATT        IFEQ      'S'
070500980304     C     FUDFUN        ANDNE     '24'
070600980304     C                   ADD       1             W
070700980304     C                   MOVEL     FUDDES        FUV(W)
070800050704     C                   ENDIF
070900050704     C                   ENDDO
071000980304     C*
071100980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
071200980304     C* ALTRIMENTI DISATTIVO F24
071300980304     C     1             DO        24            I
071400980304     C                   MOVEL     FUD(I)        FUDRIG
071500980304     C*
071600980304     C     FUDFUN        IFEQ      '24'
071700980304     C     W             IFGT      6
071800980304     C                   MOVEL     'S'           FUDATT
071900980304     C                   ELSE
072000980304     C                   MOVEL     *BLANKS       FUDATT
072100050704     C                   ENDIF
072200980304     C                   MOVEL     FUDRIG        FUD(I)
072300050704     C                   ENDIF
072400050704     C                   ENDDO
072500980304     C*
072600980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
072700980304     C     W             DIV       6             I
072800980304     C                   MVR                     Z
072900980304     C*
073000980304     C     Z             IFNE      0
073100980304     C     I             ADD       1             Z
073200980304     C                   ELSE
073300980304     C                   Z-ADD     I             Z                 5 0
073400050704     C                   ENDIF
073500980304     C*
073600980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
073700980304     C                   ADD       1             FUNGRU
073800980304     C     FUNGRU        IFGT      Z
073900980304     C     FUNGRU        ORLT      1
074000980304     C                   Z-ADD     1             FUNGRU
074100050704     C                   ENDIF
074200980304     C*
074300980304     C* PRIMO GRUPPO DI 6 FUNZIONI
074400980304     C     FUNGRU        IFEQ      1
074500980304     C                   MOVEL     FUV(1)        FUN001
074600980304     C                   MOVEL     FUV(2)        FUN002
074700980304     C                   MOVEL     FUV(3)        FUN003
074800980304     C                   MOVEL     FUV(4)        FUN004
074900980304     C                   MOVEL     FUV(5)        FUN005
075000980304     C                   MOVEL     FUV(6)        FUN006
075100050704     C                   ENDIF
075200980304     C*
075300980304     C* SECONDO GRUPPO DI 6 FUNZIONI
075400980304     C     FUNGRU        IFEQ      2
075500980304     C                   MOVEL     FUV(7)        FUN001
075600980304     C                   MOVEL     FUV(8)        FUN002
075700980304     C                   MOVEL     FUV(9)        FUN003
075800980304     C                   MOVEL     FUV(10)       FUN004
075900980304     C                   MOVEL     FUV(11)       FUN005
076000980304     C                   MOVEL     FUV(12)       FUN006
076100050704     C                   ENDIF
076200980304     C*
076300980304     C* TERZO GRUPPO DI 6 FUNZIONI
076400980304     C     FUNGRU        IFEQ      3
076500980304     C                   MOVEL     FUV(13)       FUN001
076600980304     C                   MOVEL     FUV(14)       FUN002
076700980304     C                   MOVEL     FUV(15)       FUN003
076800980304     C                   MOVEL     FUV(16)       FUN004
076900980304     C                   MOVEL     FUV(17)       FUN005
077000980304     C                   MOVEL     FUV(18)       FUN006
077100050704     C                   ENDIF
077200980304     C*
077300980304     C* QUARTO GRUPPO DI 6 FUNZIONI
077400980304     C     FUNGRU        IFEQ      4
077500980304     C                   MOVEL     FUV(19)       FUN001
077600980304     C                   MOVEL     FUV(20)       FUN002
077700980304     C                   MOVEL     FUV(21)       FUN003
077800980304     C                   MOVEL     FUV(22)       FUN004
077900980304     C                   MOVEL     *BLANKS       FUN005
078000980304     C                   MOVEL     *BLANKS       FUN006
078100050704     C                   ENDIF
078200980304     C*
078300980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
078400980304     C                   MOVEL     *BLANKS       FUN007
078500980304     C                   MOVEL     *BLANKS       FUN008
078600980304     C*
078700980304     C     1             DO        24            I
078800980304     C                   MOVEL     FUD(I)        FUDRIG
078900980304     C     FUDFUN        IFEQ      '24'
079000980304     C     FUDATT        ANDEQ     'S'
079100980304     C                   MOVEL     FUDDES        FUN008
079200050704     C                   ENDIF
079300050704     C                   ENDDO
079400980304     C*
079500980304     C                   ENDSR
079600980304     C*------------------------------------------------------------------------*
079700980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
079800980304     C*------------------------------------------------------------------------*
079900980304     C     FUNCON        BEGSR
080000980304     C*
080100980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
080200980304     C                   EXSR      FUNVAR
080300980304     C*
080400980304     C* PULISCO CAMPI DI LAVORO
080500980304     C                   Z-ADD     0             I                 5 0
080600980304     C                   Z-ADD     0             W                 5 0
080700980304     C*
080800980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
080900980304     C     FUNKEY        IFNE      *BLANKS
081000980304     C     1             DO        24            I
081100980304     C                   MOVEL     FUD(I)        FUDRIG
081200980304     C*
081300980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
081400980304     C     FUNKEY        IFEQ      FUDFUN
081500980304     C     FUDATT        ANDEQ     'S'
081600980304     C                   Z-ADD     I             W
081700980304     C                   END
081800980304     C                   END
081900980304     C*
082000980304     C* FUNZIONE NON ATTIVA (ERRORE)
082100980304     C     W             IFEQ      0
082200980304     C*
082300980304     C* EMISSIONE MESSAGGIO DI ERRORE
082400980304     C                   MOVEL     MSG(1)        DSMSMS
082500980304     C*
082600980304     C                   END
082700980304     C                   END
082800980304     C*
082900980304     C* FUNZIONE ATTIVA (PROCEDI ...)
083000980304     C     W             IFNE      0
083100980304     C     FUNKEY        OREQ      *BLANKS
083200980304     C     FUNKEY        IFEQ      *BLANKS
083300980304     C                   EXSR      ENTGES
083400980304     C                   END
083500980304     C     FUNKEY        IFEQ      '03'
083600980304     C                   EXSR      F03GES
083700980304     C                   END
083800041221     C     FUNKEY        IFEQ      '06'
083900041221     C                   EXSR      F06GES
084000041221     C                   END
084100980304     C     FUNKEY        IFEQ      '12'
084200980304     C                   EXSR      F12GES
084300980304     C                   END
084400980304     C                   END
084500980304     C*
084600980304     C                   ENDSR
084700980304     C*------------------------------------------------------------------------*
084800980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
084900980304     C*------------------------------------------------------------------------*
085000980304     C     AZZVID        BEGSR
085100050721     C*
085200050721     C     *loval        setll     sistl02l
085300050721     C                   read      sistl02l
085400050721     C                   if        not %eof(sistl02l)
085500050721     C                   z-add     stlDAT        savMinDat
085600050721     C                   z-add     *ZEROS        G08DAT
085700050721     C                   z-add     stlDAT        G08INV
085800050721     C                   movel     '3'           G08ERR
085900050721     C                   call      'XSRDA8'
086000050721     C                   parm                    WLBDA8
086100050721     C                   z-add     G08DAT        depVIDDAI
086200050721     C                   z-add     G08DAT        VIDDAI
086300050721     C                   endif
086400050721     C*
086500050721     C     *hival        setgt     sistl02l
086600050721     C                   readp     sistl02l
086700050721     C                   if        not %eof(sistl02l)
086800050721     C                   z-add     stlDAT        savMaxDat
086900050721     C                   z-add     *ZEROS        G08DAT
087000050721     C                   z-add     stlDAT        G08INV
087100050721     C                   movel     '3'           G08ERR
087200050721     C                   call      'XSRDA8'
087300050721     C                   parm                    WLBDA8
087400050721     C                   z-add     G08DAT        depVIDDAF
087500050721     C                   z-add     G08DAT        VIDDAF
087600050721     C*
087700050721     C* DALLA DATA FINALE SOTTRAE 30 GIORNO X DETERMINARE LA DATA D INIZIO ELABORAZIONE
087800050721     C     *iso          move      stlDAT        dataMimDFT
087900050721     C                   subdur    30:*d         dataMimDFT
088000050721     C                   move      dataMimDFT    depDataMin
088100050721     C                   endif
088200050721     C*
088300050721     C                   if        depDataMin > savMinDat
088400050721     C                   Z-ADD     *ZEROS        G08DAT
088500050721     C                   MOVEL(P)  depDataMin    G08INV
088600050721     C                   MOVEL     '3'           G08ERR
088700050721     C                   CALL      'XSRDA8'
088800050721     C                   PARM                    WLBDA8
088900050721     C                   Z-ADD     G08DAT        depVIDDAI
089000050721     C                   Z-ADD     G08DAT        VIDDAI
089100050721     C                   endif
089200050721     C*
089300050721     C* REPERISCO DIRETTAMENTE DAL WRK-FILE IL PERIODO DA/A RELATIVO A SPEDIZIONI E FATTURATO
089400050721     C                   read      wfsuv00f
089500050721     C                   if        not %eof(wfsuv00f)
089600050721     C                   movel(p)  wfsDAI        depPERIODODA
089700050721     C                   movel(p)  wfsDAF        depPERIODOA
089800050721     C                   endif
089900050721     C*
090000050721     C                   EVAL      VIDTXT1 = '(Maggiore di nel periodo dal '   +
090100050721     C                                       %subst(depPERIODODA:5:2) + '/'    +
090200050721     C                                       %subst(depPERIODODA:1:4) + ' al ' +
090300050721     C                                       %subst(depPERIODOA:5:2)  + '/'    +
090400050721     C                                       %subst(depPERIODOA:1:4)  + ')'
090500050715     C*
090600050715     C                   EVAL      VIDRANGE = '(Periodo dal ' +
090700050715     C                                        %editw(depVIDDAI:'  /  /    ') +
090800050715     C                                        ' al ' +
090900050715     C                                        %editw(depVIDDAF:'  /  /    ') +
091000050715     C                                        ')'
091100041231     C*
091200050715     C***                EVAL      VIDCAT    = 'C'
091300050715     C                   EVAL      VIDCAT    = *blanks
091400050715     C***                EVAL      VIDCATFLG = 'E'
091500050715     C                   EVAL      VIDCATFLG = *blanks
091600050715     C***                EVAL      VIDTIP    = 'NA'
091700050715     C                   EVAL      VIDTIP    = *blanks
091800050715     C                   EVAL      VIDSPED   = 50
091900050706     C                   EVAL      VIDLAY    = 'D'
092000050714     C                   EVAL      VIDDIV    = *blanks
092100050711     C                   EVAL      VIDARE    = *zeros
092200050711     C                   EVAL      VIDFIL    = *zeros
092300050711     C                   EVAL      VIDAGE    = *zeros
092400050714     C                   EVAL      VIDDIVD   = *blanks
092500050714     C                   EVAL      VIDARED   = *blanks
092600050714     C                   EVAL      VIDFILD   = *blanks
092700050714     C                   EVAL      VIDAGED   = *blanks
092800050711     C*
092900050704     C                   EVAL      VIDFILE   = 'N'
093000980304     C*
093100980304     C                   ENDSR
093200980304     C*------------------------------------------------------------------------*
093300980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
093400980304     C*------------------------------------------------------------------------*
093500980304     C     FUNVAR        BEGSR
093600980304     C*
093700980304     C* INIZIALIZZA VARIABILE -FUNKEY-
093800980304     C*
093900980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
094000980304     C     KEY           IFEQ      ENTER
094100980304     C                   MOVEL     *BLANKS       FUNKEY
094200980304     C                   END
094300980304     C     KEY           IFEQ      F1
094400980304     C                   MOVEL     '01'          FUNKEY
094500980304     C                   END
094600980304     C     KEY           IFEQ      F2
094700980304     C                   MOVEL     '02'          FUNKEY
094800980304     C                   END
094900980304     C     KEY           IFEQ      F3
095000980304     C                   MOVEL     '03'          FUNKEY
095100980304     C                   END
095200980304     C     KEY           IFEQ      F4
095300980304     C                   MOVEL     '04'          FUNKEY
095400980304     C                   END
095500980304     C     KEY           IFEQ      F5
095600980304     C                   MOVEL     '05'          FUNKEY
095700980304     C                   END
095800980304     C     KEY           IFEQ      F6
095900980304     C                   MOVEL     '06'          FUNKEY
096000980304     C                   END
096100980304     C     KEY           IFEQ      F7
096200980304     C                   MOVEL     '07'          FUNKEY
096300980304     C                   END
096400980304     C     KEY           IFEQ      F8
096500980304     C                   MOVEL     '08'          FUNKEY
096600980304     C                   END
096700980304     C     KEY           IFEQ      F9
096800980304     C                   MOVEL     '09'          FUNKEY
096900980304     C                   END
097000980304     C     KEY           IFEQ      F10
097100980304     C                   MOVEL     '10'          FUNKEY
097200980304     C                   END
097300980304     C     KEY           IFEQ      F11
097400980304     C                   MOVEL     '11'          FUNKEY
097500980304     C                   END
097600980304     C     KEY           IFEQ      F12
097700980304     C                   MOVEL     '12'          FUNKEY
097800980304     C                   END
097900980304     C     KEY           IFEQ      F13
098000980304     C                   MOVEL     '13'          FUNKEY
098100980304     C                   END
098200980304     C     KEY           IFEQ      F14
098300980304     C                   MOVEL     '14'          FUNKEY
098400980304     C                   END
098500980304     C     KEY           IFEQ      F15
098600980304     C                   MOVEL     '15'          FUNKEY
098700980304     C                   END
098800980304     C     KEY           IFEQ      F16
098900980304     C                   MOVEL     '16'          FUNKEY
099000980304     C                   END
099100980304     C     KEY           IFEQ      F17
099200980304     C                   MOVEL     '17'          FUNKEY
099300980304     C                   END
099400980304     C     KEY           IFEQ      F18
099500980304     C                   MOVEL     '18'          FUNKEY
099600980304     C                   END
099700980304     C     KEY           IFEQ      F19
099800980304     C                   MOVEL     '19'          FUNKEY
099900980304     C                   END
100000980304     C     KEY           IFEQ      F20
100100980304     C                   MOVEL     '20'          FUNKEY
100200980304     C                   END
100300980304     C     KEY           IFEQ      F21
100400980304     C                   MOVEL     '21'          FUNKEY
100500980304     C                   END
100600980304     C     KEY           IFEQ      F22
100700980304     C                   MOVEL     '22'          FUNKEY
100800980304     C                   END
100900980304     C     KEY           IFEQ      F23
101000980304     C                   MOVEL     '23'          FUNKEY
101100980304     C                   END
101200980304     C     KEY           IFEQ      F24
101300980304     C                   MOVEL     '24'          FUNKEY
101400980304     C                   END
101500980304     C     KEY           IFEQ      ROLLUP
101600980304     C                   MOVEL     'RU'          FUNKEY
101700980304     C                   END
101800980304     C     KEY           IFEQ      HELP
101900980304     C                   MOVEL     'HP'          FUNKEY
102000980304     C                   END
102100980304     C                   ENDSR
102200980304     C*------------------------------------------------------------------------*
102300980304     C* NOTE COSTRUZIONE SCHIERE
102400980304     C*------------------------------------------------------------------------*
102500980304     C*
102600980304     C* -MSG-MESSAGGI
102700980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
102800980304     C* (NON TOCCARE MSG,1 E MSG,2)
102900980304     C*
103000980304     C* -CMD-COMANDI
103100980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
103200980304     C*
103300980304     C* -OPD-OPZIONI
103400980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
103500980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
103600980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
103700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
103800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
103900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
104000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
104100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
104200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
104300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
104400980304     C*
104500980304     C* -FUD-FUNZIONI
104600980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
104700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
104800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
104900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
105000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
105100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
105200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
105300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
105400980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
105500980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
105600980304     o*-------------------------------------------------------------------------
105700980304** msg - messaggi
105800050704TIST38R- Funzione non attiva
105900050704TIST38R- Se indicata categoria cliente specificare se inclusa o esclusa
106000050704TIST38R- Data impostata non valida.
106100050704TIST38R- Divisione non accettabile.
106200050704TIST38R- Area non accettabile.
106300050704TIST38R- Punto operativo non accettabile.
106400050711TIST38R- Commerciale non accettabile.
106500050711TIST38R- Data oltre limite. Limite minimo:
106600050711TIST38R- Data oltre limite. Limite massimo:
106700050704TIST38R- 10
106800980304** FUD - FUNZIONI
106900980304                   -        (S/N)=N                        funz.01¬
107000980304                   -        (S/N)=N                        funz.02¬
107100980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
107200980304                   -        (S/N)=N                        funz.04¬
107300980304                   -        (S/N)=N                        funz.05¬
107400041221F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
107500980304                   -        (S/N)=N                        funz.07¬
107600980304                   -        (S/N)=N                        funz.08¬
107700980304                   -        (S/N)=N                        funz.09¬
107800980304                   -        (S/N)=N                        funz.10¬
107900980304                   -        (S/N)=N                        funz.11¬
108000980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
108100980304                   -        (S/N)=N                        funz.13¬
108200980304                   -        (S/N)=N                        funz.14¬
108300980304                   -        (S/N)=N                        funz.15¬
108400980304                   -        (S/N)=N                        funz.16¬
108500980304                   -        (S/N)=N                        funz.17¬
108600980304                   -        (S/N)=N                        funz.18¬
108700980304                   -        (S/N)=N                        funz.19¬
108800980304                   -        (S/N)=N                        funz.20¬
108900980304                   -        (S/N)=N                        funz.21¬
109000980304                   -        (S/N)=N                        funz.22¬
109100980304                   -        (S/N)=N                        funz.23¬
109200980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
