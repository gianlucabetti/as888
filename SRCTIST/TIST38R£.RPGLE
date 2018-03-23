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
001100050721     FWFSUV00F  IF   E           K DISK
001200050721     FSISTL02L  IF   E           K DISK
001300050704     FTIST38D   CF   E             WORKSTN INFDS(DEVDS1)
001400050704     F                                     SFILE(ST38DMS:MSGULT)
001500980304     D*------------------------------------------------------------------------*
001600980304     D* SCHIERE
001700980304     D*------------------------------------------------------------------------*
001800041220     D MSG             S             80    DIM(10) CTDATA PERRCD(1)
001900980304     D*------------
002000980304     D* DESCRIZIONI TASTI FUNZIONALI
002100980304     D*------------
002200980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300980304     D*------------
002400980304     D* FUNZIONI DA CARICARE A VIDEO
002500980304     D*------------
002600980304     D FUV             S             19    DIM(24)
002700980304     D*------------------------------------------------------------------------*
002800980304     D* INPUT
002900980304     D*------------------------------------------------------------------------*
003000980304     D*------------
003100980304     D* RIDEFINIZIONE FUNZIONI VIDEO
003200980304     D*------------
003300980304     D FUDDS           DS
003400980304     D  FUDDE1                 1      1
003500980304     D  FUDFUN                 2      3
003600980304     D  FUDDE2                 4     19
003700980304     D  FUDFI0                20     34
003800980304     D  FUDATT                35     35
003900980304     D  FUDFI1                36     80
004000980304     D  FUDDES                 1     19
004100980304     D  FUDRIG                 1     80
004200980304     D*------------
004300980304     D* DS GESTIONE TASTI FUNZIONALI
004400980304     D*------------
004500980304     D DSKEY         E DS
004600980304     D DEVDS1          DS
004700980304     D  KEY                  369    369
004800980304     D*------------
004900980304     D* DS "XMSG" - GESTIONE MESSAGGI
005000980304     D*------------
005100980304     D DSMSG         E DS
005200041221     D*------------------
005300041221     D* ARCHITETTURA
005400041221     D*------------------
005500041221     D KPJBA         E DS
005600050711     D*------------------
005700050711     D* TABELLA DATE "DIVERSE"
005800050711     D*------------------
005900050711     D DSDD          E DS
006000050714     D*------------------
006100050714     D* TABELLA STRUTTURA CPMMERCIALE (DISTRETTO/AREA/FILIALE/COMMERCIALE)
006200050714     D*------------------
006300050714     D DS01          E DS
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
008900060503     D**!!! TRUL31DS2     E DS
009000050711     D wAbi            S                   like(UTEAUT)
009100050711     D skDIG           S              1    dim(20)
009200050711     D skARG           S              3    dim(50)
009300050711     D skPOG           S              3    dim(250)
009400980304     D*------------------
009500041221     D* DS PASSAGGIO PARAMETRI
009600980304     D*------------------
009700050706     D PARAM           DS
009800050706     D  PARDAI                        8  0
009900050706     D  PARDAF                        8  0
010000050706     D  PARCAT                        1
010100050706     D  PARCATFLG                     1
010200050706     D  PARTIP                        2
010300050706     D  PARSPED                       6  0
010400050706     D  PARGIO                        3  0
010500050706     D  PARFILE                       1
010600050706     D  PARLAY                        1
010700050706     D  PARDIV                        1
010800050706     D  PARARE                        3  0
010900050706     D  PARFIL                        3  0
011000050721     D  PARAGE                        7  0
011100050721     D*------------------
011200050721     D* VARIABILI D WRK
011300050721     D*------------------
011400050721     D dataMimDFT      S               D   datfmt(*iso)
011500050721     D depVIDDAI       S                   like(VIDDAI) inz(*zeros)
011600050721     D depVIDDAF       S                   like(VIDDAF) inz(*zeros)
011700050721     D depPERIODODA    S              6    inz(*zeros)
011800050721     D depPERIODOA     S              6    inz(*zeros)
011900050721     D depDataMin      S                   like(VIDDAI) inz(*zeros)
012000050721     D savMinDat       S                   like(STLDAT) inz(*zeros)
012100050721     D savMaxDat       S                   like(STLDAT) inz(*zeros)
012200060503     d w0030           s              3  0
012300060503     d xx              s              3  0
012400060503     d yy              s              3  0
012500980304     C*------------------------------------------------------------------------*
012600980304     C* MAIN LINES
012700980304     C*------------------------------------------------------------------------*
012800041231     C*
012900041231     C* INIZIALIZZAZIONI INIZIALI
013000041231     C                   EXSR      AZZVID
013100980304     C*
013200980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013300041221     C     F03L00        DOUEQ     '1'
013400980304     C     F12L00        OREQ      '1'
013500980304     C     FINL00        OREQ      '1'
013600980304     C*
013700980304     C* EMETTE FORMATO VIDEO
013800980304     C                   EXSR      BUTFMR
013900980304     C*
014000980304     C* GESTIONE TASTI FUNZIONALI
014100980304     C                   EXSR      FUNCON
014200041221     C                   ENDDO
014300980304     C*
014400980304     C                   SETON                                        LR
014500980304     C*------------------------------------------------------------------------*
014600980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014700980304     C*------------------------------------------------------------------------*
014800980304     C     BUTFMR        BEGSR
014900980304     C*
015000980304     C* EMETTE LA TESTATA
015100050704     C                   WRITE     ST38D01
015200980304     C*
015300980304     C* SPEDISCE EVENTUALE MESSAGGIO
015400980304     C                   EXSR      SNDMSG
015500980304     C*
015600980304     C* EMETTE LE FUNZIONI
015700050704     C                   WRITE     ST38D02
015800980304     C*
015900980304     C* EMETTE IL VIDEO
016000050704     C                   EXFMT     ST38D03
016100980304     C*
016200980304     C* REIMPOSTA FLAG DI ERRORE
016300980304     C                   MOVEL     '0'           ERRL00
016400050107     C*
016500050107     C* SPEGNE INDICATORI D ERRORE A VIDEO
016600050704     C                   SETOFF                                       507071
016700980304     C*
016800980304     C                   ENDSR
016900980304     C*------------------------------------------------------------------------*
017000980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
017100980304     C*------------------------------------------------------------------------*
017200980304     C     SNDMSG        BEGSR
017300980304     C*
017400980304     C* PULISCE IL SFILE MESSAGGI
017500980304     C                   MOVEL     '1'           *IN40
017600050704     C                   WRITE     ST38DMC
017700980304     C                   MOVEL     '0'           *IN40
017800980304     C*
017900980304     C* VISUALIZZA IL MESSAGGIO
018000980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
018100980304     C                   CALL      'XMSG'        XMSGPA
018200041220     C     DSMSER        IFEQ      *ZEROS
018300980304     C                   MOVEL     DSMSKE        MSGKEY
018400041220     C                   ENDIF
018500980304     C                   Z-ADD     1             MSGULT
018600050704     C                   WRITE     ST38DMS
018700050704     C                   WRITE     ST38DMC
018800980304     C*
018900980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
019000980304     C                   MOVEL     *BLANKS       DSMSMS
019100980304     C*
019200980304     C                   ENDSR
019300980304     C*------------------------------------------------------------------------*
019400980304     C* ENTGES - GESTIONE ENTER -> INVIO
019500980304     C*------------------------------------------------------------------------*
019600980304     C     ENTGES        BEGSR
019700980304     C*
019800050107     C                   EXSR      DECODI
019900041220     C*
020000980304     C                   ENDSR
020100980304     C*------------------------------------------------------------------------*
020200980304     C* F03GES - GESTIONE F03 -> FINE
020300980304     C*------------------------------------------------------------------------*
020400980304     C     F03GES        BEGSR
020500980304     C*
020600980304     C* IMPOSTA "PREMUTO F03"
020700980304     C                   MOVEL     '1'           F03L00
020800980304     C*
020900980304     C                   ENDSR
021000041221     C*------------------------------------------------------------------------*
021100041221     C* F06GES - GESTIONE F06 -> CONFERMA ELABORAZIONE
021200041221     C*------------------------------------------------------------------------*
021300041221     C     F06GES        BEGSR
021400050107     C*
021500050107     C* ESEGUO ROUTINE D VERIFICA DATI A VIDEO
021600050107     C                   EXSR      DECODI
021700041221     C*
021800050107     C* SE NO ERRORI => SOTTOMETTO L'ELABORAZOINE
021900050107     C                   IF        ERRL00 = '0'
022000050704     C                   MOVEL     'ST39'        KCOAZ
022100050704     C                   EVAL      PARDAI    = DAIVID
022200050704     C                   EVAL      PARDAF    = DAFVID
022300041231     C                   EVAL      PARCAT    = VIDCAT
022400041231     C                   EVAL      PARCATFLG = VIDCATFLG
022500041231     C                   EVAL      PARTIP    = VIDTIP
022600041231     C                   EVAL      PARSPED   = VIDSPED
022700050704     C                   EVAL      PARFILE   = VIDFILE
022800050706     C                   EVAL      PARLAY    = VIDLAY
022900050706     C                   EVAL      PARDIV    = VIDDIV
023000050706     C                   EVAL      PARARE    = VIDARE
023100050706     C                   EVAL      PARFIL    = VIDFIL
023200050706     C                   EVAL      PARAGE    = VIDAGE
023300041221     C                   MOVEL     PARAM         KPJBU
023400060503     C***                CALL      'TIST39C'
023500060503 MOM C                   CALL      'BCH10'
023600041221     C                   PARM                    KPJBA
023700041221     C*
023800041221     C* DOPO LA SOTTOMISSION ESCO DAL *PGM
023900041221     C                   MOVEL     '1'           F03L00
024000050107     C                   ENDIF
024100041221     C*
024200041221     C                   ENDSR
024300980304     C*------------------------------------------------------------------------*
024400980304     C* F12GES - GESTIONE F12 -> RITORNO
024500980304     C*------------------------------------------------------------------------*
024600980304     C     F12GES        BEGSR
024700980304     C*
024800980304     C* IMPOSTA "PREMUTO F12"
024900980304     C                   MOVEL     '1'           F12L00
025000980304     C*
025100980304     C                   ENDSR
025200980304     C*------------------------------------------------------------------------*
025300980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
025400980304     C*------------------------------------------------------------------------*
025500980304     C     F24GES        BEGSR
025600980304     C*
025700980304     C                   EXSR      FUNGES
025800980304     C*
025900980304     C                   ENDSR
026000050107     C*------------------------------------------------------------------------*
026100050107     C* DECODI - CONTROLLO DATI A VIDEO
026200050107     C*------------------------------------------------------------------------*
026300050107     C     DECODI        BEGSR
026400050107     C*
026500050107     C* RE-INIZIALIZZAZIONE FLAG ERRORE
026600050107     C                   MOVEL     '0'           ERRL00
026700050107     C*
026800050107     C* SPENGO INDICATORI D ERRORE A VIDEO
026900050704     C                   SETOFF                                       507071
027000050711     C                   SETOFF                                       545556
027100050711     C                   SETOFF                                       53
027200050107     C*
027300050107     C* CONTROLLO PARAMETRO INCLISIONE/ESCLUSIONE CATEGORIA SELEZIONATA
027400050107     C                   IF        VIDCAT    <> *blanks AND
027500050107     C                             VIDCATFLG =  *blanks
027600050107     C                   MOVEL     '1'           ERRL00
027700050107     C                   SETON                                        50
027800050107     C                   MOVEL     MSG(2)        DSMSMS
027900050704     C                   ENDIF
028000050704     C*
028100050711     C* CONTROLLO DATA INIZIALE
028200050704     C     *LIKE         DEFINE    VIDDAI        DAIVID
028300050704     C     ERRL00        IFEQ      '0'
028400050714     C                   IF        VIDDAI = *zeros
028500050714     C                   EVAL      VIDDAI = depVIDDAI
028600050714     C                   ELSE
028700050704     C                   Z-ADD     VIDDAI        WDA8
028800050704     C                   Z-ADD     *ZEROS        DA8W
028900050704     C                   EXSR      CHKDA8
029000050704     C                   Z-ADD     WDA8          VIDDAI
029100050704     C                   Z-ADD     DA8W          DAIVID
029200050704     C     ERRL00        IFEQ      '1'
029300050711     C                   MOVEL     '1'           ERRL00
029400050704     C                   SETON                                        70
029500050711     C                   MOVEL     MSG(3)        DSMSMS
029600050711     C                   ELSE
029700050711     C* VERIFICA SE DATA COMPRESA NEL RANGE PILOTATO DA TABELLA
029800050711     C                   EVAL      tbeCOD = 'SDD'
029900050711     C                   EVAL      tbeKE1 = '1'
030000050711     C     KEYTBE01_P    CHAIN     TNTBE01L
030100050711     C                   IF        %found(TNTBE01L)
030200050711     C                   EVAL      DSDD = tbeUNI
030300050711     C                   IF        DAIVID < §SDDVAS1
030400050711     C                   MOVEL     '1'           ERRL00
030500050711     C                   SETON                                        70
030600050711     C                   MOVEL     MSG(8)        DSMSMS
030700050711     C                   EVAL      DSMSMS = %trim(DSMSMS) + ' ' +
030800050711     C                                      %editw(§SDDVAS1:'    /  /  ')
030900050704     C                   ENDIF
031000050704     C                   ENDIF
031100050714     C                   ENDIF
031200050711     C                   ENDIF
031300050711     C                   ENDIF
031400050704     C*
031500050711     C* CONTROLLO DATA FINALE
031600050704     C     *LIKE         DEFINE    VIDDAF        DAFVID
031700050704     C     ERRL00        IFEQ      '0'
031800050714     C                   IF        VIDDAF = *zeros
031900050714     C                   EVAL      VIDDAF = depVIDDAF
032000050714     C                   ELSE
032100050704     C                   Z-ADD     VIDDAF        WDA8
032200050704     C                   Z-ADD     *ZEROS        DA8W
032300050704     C                   EXSR      CHKDA8
032400050704     C                   Z-ADD     WDA8          VIDDAF
032500050704     C                   Z-ADD     DA8W          DAFVID
032600050704     C     ERRL00        IFEQ      '1'
032700050711     C                   MOVEL     '1'           ERRL00
032800050704     C                   SETON                                        71
032900050711     C                   MOVEL     MSG(3)        DSMSMS
033000050711     C                   ELSE
033100050711     C* VERIFICA SE DATA COMPRESA NEL RANGE PILOTATO DA TABELLA
033200050711     C                   EVAL      tbeCOD = 'SDD'
033300050711     C                   EVAL      tbeKE1 = '1'
033400050711     C     KEYTBE01_P    CHAIN     TNTBE01L
033500050711     C                   IF        %found(TNTBE01L)
033600050711     C                   EVAL      DSDD = tbeUNI
033700050711     C                   IF        DAIVID > §SDDVAS2
033800050711     C                   MOVEL     '1'           ERRL00
033900050711     C                   SETON                                        71
034000050711     C                   MOVEL     MSG(9)        DSMSMS
034100050711     C                   EVAL      DSMSMS = %trim(DSMSMS) + ' ' +
034200050711     C                                      %editw(§SDDVAS2:'    /  /  ')
034300050711     C                   ENDIF
034400050711     C                   ENDIF
034500050714     C                   ENDIF
034600050704     C                   ENDIF
034700050704     C                   ENDIF
034800050704     C*
034900050704     C* CONTROLLO RANGE DATE
035000050704     C     ERRL00        IFEQ      '0'
035100050704     C                   IF        DAIVID > DAFVID
035200050711     C                   MOVEL     '1'           ERRL00
035300050704     C                   SETON                                        71
035400050704     C                   SETON                                        70
035500050711     C                   MOVEL     MSG(3)        DSMSMS
035600050704     C                   ENDIF
035700050704     C                   ENDIF
035800050704     C*
035900050711     C* CALCOLO GIORNI LAVORATIVI
036000050704     C     ERRL00        IFEQ      '0'
036100050704     C                   CLEAR                   WL08
036200050704     C                   Z-ADD     DAIVID        L08DA
036300050704     C                   Z-ADD     DAFVID        L08A
036400050704     C                   CALL      'XSRLAV8'
036500050704     C                   PARM                    WL08
036600050704     C                   MOVE      L08L          PARGIO
036700050704     C                   ENDIF
036800050711     C*
036900050711     C* CONTROLLO STRUTTURA COMMERCIALE
037000050711     C     ERRL00        IFEQ      '0'
037100050714     C                   EVAL      VIDDIVD = *blanks
037200050714     C                   IF        VIDDIV <> *blanks
037300050711     C     VIDDIV        LOOKUP    skDIG                                  30
037400050711     C                   IF        not %found
037500050711     C                   MOVEL     '1'           ERRL00
037600050711     C                   SETON                                        54
037700050711     C                   MOVEL     MSG(4)        DSMSMS
037800050714     C                   ELSE
037900050714     C                   EXSR      DECDIV
038000050711     C                   ENDIF
038100050711     C                   ENDIF
038200050711     C                   ENDIF
038300050107     C*
038400050711     C     ERRL00        IFEQ      '0'
038500050714     C                   EVAL      VIDARED = *blanks
038600050711     C                   IF        VIDARE <> *zeros
038700050711     C                   MOVEL(P)  VIDARE        wAlfaARE          3
038800050711     C     wAlfaARE      LOOKUP    skARG                                  30
038900050711     C                   IF        not %found
039000050711     C                   MOVEL     '1'           ERRL00
039100050711     C                   SETON                                        55
039200050711     C                   MOVEL     MSG(5)        DSMSMS
039300050714     C                   ELSE
039400050714     C                   EXSR      DECARE
039500050711     C                   ENDIF
039600050711     C                   ENDIF
039700050711     C                   ENDIF
039800050711     C*
039900050711     C     ERRL00        IFEQ      '0'
040000050714     C                   EVAL      VIDFILD = *blanks
040100050711     C                   IF        VIDFIL <> *zeros
040200050711     C                   MOVEL(P)  VIDFIL        wAlfaFIL          3
040300050711     C     wAlfaFIL      LOOKUP    skPOG                                  30
040400050711     C                   IF        not %found
040500050711     C                   MOVEL     '1'           ERRL00
040600050711     C                   SETON                                        56
040700050711     C                   MOVEL     MSG(6)        DSMSMS
040800050714     C                   ELSE
040900050714     C                   EXSR      DECFIL
041000050711     C                   ENDIF
041100050711     C                   ENDIF
041200050711     C                   ENDIF
041300050711     C*
041400050711     C     ERRL00        IFEQ      '0'
041500050714     C                   EVAL      VIDAGED = *blanks
041600050711     C                   IF        VIDAGE <> *zeros
041700050711     C                   Z-ADD     1             tblKUT
041800050711     C                   MOVEL(P)  '01'          tblCOD
041900050711     C                   MOVEL(P)  VIDAGE        tblKEY
042000050711     C     KEYTAB        CHAIN     TABEL00F
042100050711     C                   IF        not %found(TABEL00F)
042200050711     C                   MOVEL     '1'           ERRL00
042300050711     C                   SETON                                        53
042400050711     C                   MOVEL     MSG(7)        DSMSMS
042500050711     C                   ELSE
042600050711     C*
042700050711     C                   MOVEL(P)  VIDAGE        wPoAge            3
042800050711     C     wPoAge        LOOKUP    skPOG                                  30
042900050711     C                   IF        not %found
043000050711     C                   MOVEL     '1'           ERRL00
043100050711     C                   SETON                                        53
043200050711     C                   MOVEL     MSG(7)        DSMSMS
043300050714     C                   ELSE
043400050714     C                   EXSR      DECAGE
043500050711     C                   ENDIF
043600050711     C                   ENDIF
043700050711     C                   ENDIF
043800050711     C                   ENDIF
043900050711     C*
044000050107     C                   ENDSR
044100050704     C*------------------------------------------------------------------------*
044200050704     C* CHKDA8 - CONTROLLA LA DATA (8)
044300050704     C*------------------------------------------------------------------------*
044400050704     C     CHKDA8        BEGSR
044500050704     C*
044600050704     C     *LIKE         DEFINE    DAIVID        WDA8
044700050704     C     *LIKE         DEFINE    DAFVID        DA8W
044800050704     C*
044900050704     C                   Z-ADD     WDA8          G08DAT
045000050704     C                   Z-ADD     *ZEROS        G08INV
045100050704     C                   MOVEL     '0'           G08ERR
045200050704     C                   CALL      'XSRDA8'
045300050704     C                   PARM                    WLBDA8
045400050704     C                   Z-ADD     G08DAT        WDA8
045500050704     C                   Z-ADD     G08INV        DA8W
045600050704     C     G08ERR        IFEQ      '1'
045700050704     C                   MOVEL     '1'           ERRL00
045800050704     C                   ENDIF
045900050704     C*
046000050704     C                   ENDSR
046100050714     C*------------------------------------------------------------------------*
046200050714     C* DECDIV - DECODIFICA DIVISIONE
046300050714     C*------------------------------------------------------------------------*
046400050714     C     DECDIV        BEGSR
046500050714     C*
046600050714     C                   MOVEL     1             TBLKUT
046700050714     C                   MOVE      '17'          TBLCOD
046800050714     C                   MOVE      *BLANKS       TBLKEY
046900050714     C                   MOVEL     VIDDIV        TBLKEY
047000050714     C     KEYTAB        CHAIN     TABEL                              97
047100050714     C     *IN97         IFEQ      '1'
047200050714     C     TBLFLG        OREQ      '*'
047300050714     C                   MOVEL     *ALL'*'       VIDDIVD
047400050714     C                   ELSE
047500050714     C                   MOVEL(P)  TBLUNI        DS17
047600050714     C                   MOVEL(P)  §17DES        VIDDIVD
047700050714     C                   ENDIF
047800050714     C*
047900050714     C                   ENDSR
048000050714     C*------------------------------------------------------------------------*
048100050714     C* DECARE - DECODIFICA AREA
048200050714     C*------------------------------------------------------------------------*
048300050714     C     DECARE        BEGSR
048400050714     C*
048500050714     C                   MOVEL     1             TBLKUT
048600050714     C                   MOVE      '05'          TBLCOD
048700050714     C                   MOVE      *BLANKS       TBLKEY
048800050714     C                   MOVEL     VIDARE        TBLKEY
048900050714     C     KEYTAB        CHAIN     TABEL                              97
049000050714     C     *IN97         IFEQ      '1'
049100050714     C     TBLFLG        OREQ      '*'
049200050714     C                   MOVEL     *ALL'*'       VIDARED
049300050714     C                   ELSE
049400050714     C                   MOVEL(P)  TBLUNI        DS05
049500050714     C                   MOVEL(P)  §05DES        VIDARED
049600050714     C                   ENDIF
049700050714     C*
049800050714     C                   ENDSR
049900050714     C*------------------------------------------------------------------------*
050000050714     C* DECFIL - DECODIFICA FILIALE/TERMINAL
050100050714     C*------------------------------------------------------------------------*
050200050714     C     DECFIL        BEGSR
050300050714     C*
050400050714     C                   Z-ADD     1             I
050500050714     C     VIDFIL        CHAIN     AZORG01L
050600050714     C                   IF        not %found(AZORG01L)
050700050714     C                   MOVEL     *ALL'*'       VIDFILD
050800050714     C                   ELSE
050900050714     C                   MOVEL(P)  ORGDES        VIDFILD
051000050714     C                   ENDIF
051100050714     C*
051200050714     C                   ENDSR
051300050714     C*------------------------------------------------------------------------*
051400050714     C* DECAGE - DECODIFICA COMMERCIALE
051500050714     C*------------------------------------------------------------------------*
051600050714     C     DECAGE        BEGSR
051700050714     C*
051800050714     C                   MOVEL     1             TBLKUT
051900050714     C                   MOVE      '01'          TBLCOD
052000050714     C                   MOVE      *BLANKS       TBLKEY
052100050714     C                   MOVEL     VIDAGE        TBLKEY
052200050714     C     KEYTAB        CHAIN     TABEL                              97
052300050714     C     *IN97         IFEQ      '1'
052400050714     C     TBLFLG        OREQ      '*'
052500050714     C                   MOVEL     *ALL'*'       VIDAGED
052600050714     C                   ELSE
052700050714     C                   MOVEL(P)  TBLUNI        DS01
052800050714     C                   MOVEL(P)  §01AGE        VIDAGED
052900050714     C                   ENDIF
053000050714     C*
053100050714     C                   ENDSR
053200050704     C*--------------------------------------------------------------------------------------------*
053300050704     C* REPERISCE I DATI UTENTE
053400050704     C*--------------------------------------------------------------------------------------------*
053500050704     C     REPDATIUTE    BEGSR
053600050704     C*
053700050704     C* INIZIALIZZA VARIABILI DI WRK
053800050704     C                   CLEAR                   TIBS34DS
053900050704     C                   CLEAR                   AZUTEDS
054000050704     C                   CLEAR                   DDATIUTE
054100050704     C*
054200050704     C     *DTAARA       DEFINE    §azute        azuteds
054300050704     C     *DTAARA       DEFINE    §datiute      ddatiute
054400050704     C                   IN(E)     *DTAARA
054500050704     C                   IF        %Error
054600050704     C                   EVAL      I34Tla = 'L'
054700050704     C                   CALL      'TIBS34R'
054800050704     C                   PARM                    Tibs34Ds
054900050704     C                   IN        *DTAARA
055000050704     C                   ENDIF
055100050711     C*
055200050711     C* Verifica errori e autorità profilo
055300050711     C                   IF        DUTERR = 'E'
055400050711     C                   EVAL      FINL00 = '1'
055500050711     C                   ELSE
055600050711     C                   MOVEL(P)  UTEFAF        DUTE01
055700050711     C                   IF        §UTECLI <> *blanks
055800050711     C                   EVAL      wAbi = §UTECLI
055900050711     C                   ELSE
056000050711     C* se non c'è l'abilitazione
056100050711     C* --> se 1° livello, abilitazioni al terminal
056200050711     C*     se 2° livello, abilitazioni al punto operativo
056300050711     C*     se sede è impossibile ma se capita mando a fine il pgm
056400050711     C                   IF        UTEAUT = *blanks
056500050711     C                   IF        DUTLPO = '1'
056600050711     C                   EVAL      wAbi   = 'TP'
056700050711     C                   ENDIF
056800050711     C                   IF        DUTLPO = '2'
056900050711     C                   EVAL      wAbi   = 'PO'
057000050711     C                   ENDIF
057100050711     C                   IF        DUTLPO = 'S'
057200050711     C                   EVAL      wAbi   = 'AZ'
057300050711     C                   ENDIF
057400050711     C                   ELSE
057500050711     C                   EVAL      wAbi = UTEAUT
057600050711     C                   ENDIF
057700050711     C                   ENDIF
057800050711     C                   ENDIF
057900050711     C*
058000050711     C* Reperimento dei P.O. gestibili dall'utente
058100050711     C                   clear                   TRUL31DS
058200060503     C**!!!              clear                   TRUL31DS2
058300050711     C                   eval      I31abi = wAbi
058400050711     C                   eval      I31cdi = DUTdis
058500050711     C                   eval      I31car = DUTare
058600050711     C                   eval      I31cpo = DUTpou
058700050711     C                   call      'TRUL31R'
058800050711     C                   parm                    KPJBA
058900050711     C                   parm                    TRUL31DS
059000060503     C**!!!              parm                    TRUL31DS2
059100050711     C                   if        O31pog > *zeros
059200060503     C**!!!              movea     O31dig        skDIG
059300060503     C**!!!              movea     O31arg        skARG
059400050711     C                   movea     O31pog        skPOG
059500050711     C                   else
059600050711     C                   movel     '1'           FINL00
059700060503     c                   leavesr
059800050711     C                   endif
059900060503
060000060503     c                   clear                   skdig
060100060503     c                   clear                   skarg
060200060503      * mi carico la sk dei distretti e aree relativi ai p.o. gestiti dall'utente
060300060503     c                   do        250           xx
060400060503     c                   move      skpog(xx)     w0030
060500060503     c                   if        skpog(xx) = *zeros
060600060503     c                   leave
060700060503     c                   endif
060800060503     c     w0030         chain     azorg01l
060900060503     c                   if        %found(azorg01l)
061000060503      * memorizzo il distretto se non è già stato memorizzato
061100060503     c     orgfl3        lookup    skdig                                  30
061200060503     c                   if        not %found
061300060503     c                   z-add     1             yy
061400060503     c     *blanks       lookup    skdig(yy)                              30
061500060503     c                   if        %found
061600060503     c                   movel     orgfl3        skdig(yy)
061700060503     c                   endif
061800060503     c                   endif
061900060503      * memorizzo l'area se non è già stata memorizzata
062000060503     c                   movel(p)  orgcar        walfaare
062100060503     c     walfaare      lookup    skarg                                  30
062200060503     c                   if        not %found
062300060503     c                   z-add     1             yy
062400060503     c     *blanks       lookup    skarg(yy)                              30
062500060503     c                   if        %found
062600060503     c                   movel     walfaare      skarg(yy)
062700060503     c                   endif
062800060503     c                   endif
062900060503     c                   endif
063000060503     c                   enddo
063100060503
063200050711     C*
063300050711     C                   ENDSR
063400980304     C*------------------------------------------------------------------------*
063500980304     C* *INZSR - OPERAZIONI INIZIALI
063600980304     C*------------------------------------------------------------------------*
063700980304     C     *INZSR        BEGSR
063800980304     C*---
063900980304     C* RICEVIMENTO PARAMETRI
064000980304     C*---
064100980304     C     *ENTRY        PLIST
064200980304     C                   PARM                    KPJBA
064300980304     C*
064400980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
064500980304     C     XMSGPA        PLIST
064600980304     C                   PARM                    DSMSMS
064700980304     C                   PARM                    DSMSPG
064800980304     C                   PARM                    DSMSKE
064900980304     C                   PARM                    DSMSER
065000980304     C*
065100980304     C* IMPOSTA LA DS -TASTI PREMUTI-
065200980304     C                   CALL      'XKEY'
065300980304     C                   PARM                    DSKEY
065400980304     C*---
065500980304     C* VARIABILI NON RIFERITE AL DATA BASE
065600980304     C*---
065700980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065800980304     C*---
065900980304     C* VARIABILI DI CONTROLLO
066000980304     C*---
066100980304     C                   MOVEL     '0'           FINL00            1
066200980304     C                   MOVEL     '0'           F03L00            1
066300980304     C                   MOVEL     '0'           F12L00            1
066400980304     C                   MOVEL     '0'           ERRL00            1
066500980304     C*
066600980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
066700050704     C                   MOVEL     'TIST38R'     DSMSPG
066800050704     C                   MOVEL     'TIST38R'     MSGQUE
066900980304     C                   MOVEL     *BLANKS       MSGKEY
067000980304     C                   MOVEL     *BLANKS       DSMSMS
067100980304     C*
067200980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
067300980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
067400980304     C                   EXSR      FUNGES
067500050711     C*
067600050711     C* DEFINIZIONE CHIAVI
067700050711     C* TABEL00F - COMPLETA
067800050711     C     KEYTAB        KLIST
067900050711     C                   KFLD                    tblKUT
068000050711     C                   KFLD                    tblCOD
068100050711     C                   KFLD                    tblKEY
068200050711     C* TNTBE01L - PARZIALE
068300050711     C     KEYTBE01_P    KLIST
068400050711     C                   KFLD                    tbeCOD
068500050711     C                   KFLD                    tbeKE1
068600050711     C*
068700050711     C* EFFETTUO SUBUTI TEST SE UTENTE E' EDP*
068800050711     C                   IF        %subst(KNMUS:1:3) = 'EDP'
068900050711     C                   SETON                                        52
069000050711     C                   ENDIF
069100050711     C*
069200050711     C* REPERIMENTO DATI UTENTE CORRENTE
069300050711     C                   EXSR      REPDATIUTE
069400980304     C*
069500980304     C                   ENDSR
069600980304     C*------------------------------------------------------------------------*
069700980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
069800980304     C*------------------------------------------------------------------------*
069900980304     C     FUNGES        BEGSR
070000980304     C*
070100980304     C* PULISCO CAMPI DI LAVORO
070200980304     C                   Z-ADD     0             W                 5 0
070300980304     C*
070400980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
070500980304     C* E CARICO TABELLA FUNZIONI ATTIVE
070600980304     C     1             DO        24            I
070700980304     C                   MOVEL     FUD(I)        FUDRIG
070800980304     C*
070900980304     C* PULIZIA FUNZIONI PRECEDENTI
071000980304     C                   MOVEL     *BLANKS       FUV(I)
071100980304     C*
071200980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
071300980304     C* (SALTA F24)
071400980304     C     FUDATT        IFEQ      'S'
071500980304     C     FUDFUN        ANDNE     '24'
071600980304     C                   ADD       1             W
071700980304     C                   MOVEL     FUDDES        FUV(W)
071800050704     C                   ENDIF
071900050704     C                   ENDDO
072000980304     C*
072100980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
072200980304     C* ALTRIMENTI DISATTIVO F24
072300980304     C     1             DO        24            I
072400980304     C                   MOVEL     FUD(I)        FUDRIG
072500980304     C*
072600980304     C     FUDFUN        IFEQ      '24'
072700980304     C     W             IFGT      6
072800980304     C                   MOVEL     'S'           FUDATT
072900980304     C                   ELSE
073000980304     C                   MOVEL     *BLANKS       FUDATT
073100050704     C                   ENDIF
073200980304     C                   MOVEL     FUDRIG        FUD(I)
073300050704     C                   ENDIF
073400050704     C                   ENDDO
073500980304     C*
073600980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
073700980304     C     W             DIV       6             I
073800980304     C                   MVR                     Z
073900980304     C*
074000980304     C     Z             IFNE      0
074100980304     C     I             ADD       1             Z
074200980304     C                   ELSE
074300980304     C                   Z-ADD     I             Z                 5 0
074400050704     C                   ENDIF
074500980304     C*
074600980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
074700980304     C                   ADD       1             FUNGRU
074800980304     C     FUNGRU        IFGT      Z
074900980304     C     FUNGRU        ORLT      1
075000980304     C                   Z-ADD     1             FUNGRU
075100050704     C                   ENDIF
075200980304     C*
075300980304     C* PRIMO GRUPPO DI 6 FUNZIONI
075400980304     C     FUNGRU        IFEQ      1
075500980304     C                   MOVEL     FUV(1)        FUN001
075600980304     C                   MOVEL     FUV(2)        FUN002
075700980304     C                   MOVEL     FUV(3)        FUN003
075800980304     C                   MOVEL     FUV(4)        FUN004
075900980304     C                   MOVEL     FUV(5)        FUN005
076000980304     C                   MOVEL     FUV(6)        FUN006
076100050704     C                   ENDIF
076200980304     C*
076300980304     C* SECONDO GRUPPO DI 6 FUNZIONI
076400980304     C     FUNGRU        IFEQ      2
076500980304     C                   MOVEL     FUV(7)        FUN001
076600980304     C                   MOVEL     FUV(8)        FUN002
076700980304     C                   MOVEL     FUV(9)        FUN003
076800980304     C                   MOVEL     FUV(10)       FUN004
076900980304     C                   MOVEL     FUV(11)       FUN005
077000980304     C                   MOVEL     FUV(12)       FUN006
077100050704     C                   ENDIF
077200980304     C*
077300980304     C* TERZO GRUPPO DI 6 FUNZIONI
077400980304     C     FUNGRU        IFEQ      3
077500980304     C                   MOVEL     FUV(13)       FUN001
077600980304     C                   MOVEL     FUV(14)       FUN002
077700980304     C                   MOVEL     FUV(15)       FUN003
077800980304     C                   MOVEL     FUV(16)       FUN004
077900980304     C                   MOVEL     FUV(17)       FUN005
078000980304     C                   MOVEL     FUV(18)       FUN006
078100050704     C                   ENDIF
078200980304     C*
078300980304     C* QUARTO GRUPPO DI 6 FUNZIONI
078400980304     C     FUNGRU        IFEQ      4
078500980304     C                   MOVEL     FUV(19)       FUN001
078600980304     C                   MOVEL     FUV(20)       FUN002
078700980304     C                   MOVEL     FUV(21)       FUN003
078800980304     C                   MOVEL     FUV(22)       FUN004
078900980304     C                   MOVEL     *BLANKS       FUN005
079000980304     C                   MOVEL     *BLANKS       FUN006
079100050704     C                   ENDIF
079200980304     C*
079300980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
079400980304     C                   MOVEL     *BLANKS       FUN007
079500980304     C                   MOVEL     *BLANKS       FUN008
079600980304     C*
079700980304     C     1             DO        24            I
079800980304     C                   MOVEL     FUD(I)        FUDRIG
079900980304     C     FUDFUN        IFEQ      '24'
080000980304     C     FUDATT        ANDEQ     'S'
080100980304     C                   MOVEL     FUDDES        FUN008
080200050704     C                   ENDIF
080300050704     C                   ENDDO
080400980304     C*
080500980304     C                   ENDSR
080600980304     C*------------------------------------------------------------------------*
080700980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
080800980304     C*------------------------------------------------------------------------*
080900980304     C     FUNCON        BEGSR
081000980304     C*
081100980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
081200980304     C                   EXSR      FUNVAR
081300980304     C*
081400980304     C* PULISCO CAMPI DI LAVORO
081500980304     C                   Z-ADD     0             I                 5 0
081600980304     C                   Z-ADD     0             W                 5 0
081700980304     C*
081800980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
081900980304     C     FUNKEY        IFNE      *BLANKS
082000980304     C     1             DO        24            I
082100980304     C                   MOVEL     FUD(I)        FUDRIG
082200980304     C*
082300980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
082400980304     C     FUNKEY        IFEQ      FUDFUN
082500980304     C     FUDATT        ANDEQ     'S'
082600980304     C                   Z-ADD     I             W
082700980304     C                   END
082800980304     C                   END
082900980304     C*
083000980304     C* FUNZIONE NON ATTIVA (ERRORE)
083100980304     C     W             IFEQ      0
083200980304     C*
083300980304     C* EMISSIONE MESSAGGIO DI ERRORE
083400980304     C                   MOVEL     MSG(1)        DSMSMS
083500980304     C*
083600980304     C                   END
083700980304     C                   END
083800980304     C*
083900980304     C* FUNZIONE ATTIVA (PROCEDI ...)
084000980304     C     W             IFNE      0
084100980304     C     FUNKEY        OREQ      *BLANKS
084200980304     C     FUNKEY        IFEQ      *BLANKS
084300980304     C                   EXSR      ENTGES
084400980304     C                   END
084500980304     C     FUNKEY        IFEQ      '03'
084600980304     C                   EXSR      F03GES
084700980304     C                   END
084800041221     C     FUNKEY        IFEQ      '06'
084900041221     C                   EXSR      F06GES
085000041221     C                   END
085100980304     C     FUNKEY        IFEQ      '12'
085200980304     C                   EXSR      F12GES
085300980304     C                   END
085400980304     C                   END
085500980304     C*
085600980304     C                   ENDSR
085700980304     C*------------------------------------------------------------------------*
085800980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
085900980304     C*------------------------------------------------------------------------*
086000980304     C     AZZVID        BEGSR
086100050721     C*
086200050721     C     *loval        setll     sistl02l
086300050721     C                   read      sistl02l
086400050721     C                   if        not %eof(sistl02l)
086500050721     C                   z-add     stlDAT        savMinDat
086600050721     C                   z-add     *ZEROS        G08DAT
086700050721     C                   z-add     stlDAT        G08INV
086800050721     C                   movel     '3'           G08ERR
086900050721     C                   call      'XSRDA8'
087000050721     C                   parm                    WLBDA8
087100050721     C                   z-add     G08DAT        depVIDDAI
087200050721     C                   z-add     G08DAT        VIDDAI
087300050721     C                   endif
087400050721     C*
087500050721     C     *hival        setgt     sistl02l
087600050721     C                   readp     sistl02l
087700050721     C                   if        not %eof(sistl02l)
087800050721     C                   z-add     stlDAT        savMaxDat
087900050721     C                   z-add     *ZEROS        G08DAT
088000050721     C                   z-add     stlDAT        G08INV
088100050721     C                   movel     '3'           G08ERR
088200050721     C                   call      'XSRDA8'
088300050721     C                   parm                    WLBDA8
088400050721     C                   z-add     G08DAT        depVIDDAF
088500050721     C                   z-add     G08DAT        VIDDAF
088600050721     C*
088700050721     C* DALLA DATA FINALE SOTTRAE 30 GIORNO X DETERMINARE LA DATA D INIZIO ELABORAZIONE
088800050721     C     *iso          move      stlDAT        dataMimDFT
088900050721     C                   subdur    30:*d         dataMimDFT
089000050721     C                   move      dataMimDFT    depDataMin
089100050721     C                   endif
089200050721     C*
089300050721     C                   if        depDataMin > savMinDat
089400050721     C                   Z-ADD     *ZEROS        G08DAT
089500050721     C                   MOVEL(P)  depDataMin    G08INV
089600050721     C                   MOVEL     '3'           G08ERR
089700050721     C                   CALL      'XSRDA8'
089800050721     C                   PARM                    WLBDA8
089900050721     C                   Z-ADD     G08DAT        depVIDDAI
090000050721     C                   Z-ADD     G08DAT        VIDDAI
090100050721     C                   endif
090200050721     C*
090300050721     C* REPERISCO DIRETTAMENTE DAL WRK-FILE IL PERIODO DA/A RELATIVO A SPEDIZIONI E FATTURATO
090400050721     C                   read      wfsuv00f
090500050721     C                   if        not %eof(wfsuv00f)
090600050721     C                   movel(p)  wfsDAI        depPERIODODA
090700050721     C                   movel(p)  wfsDAF        depPERIODOA
090800050721     C                   endif
090900050721     C*
091000050721     C                   EVAL      VIDTXT1 = '(Maggiore di nel periodo dal '   +
091100050721     C                                       %subst(depPERIODODA:5:2) + '/'    +
091200050721     C                                       %subst(depPERIODODA:1:4) + ' al ' +
091300050721     C                                       %subst(depPERIODOA:5:2)  + '/'    +
091400050721     C                                       %subst(depPERIODOA:1:4)  + ')'
091500050715     C*
091600050715     C                   EVAL      VIDRANGE = '(Periodo dal ' +
091700050715     C                                        %editw(depVIDDAI:'  /  /    ') +
091800050715     C                                        ' al ' +
091900050715     C                                        %editw(depVIDDAF:'  /  /    ') +
092000050715     C                                        ')'
092100041231     C*
092200050715     C***                EVAL      VIDCAT    = 'C'
092300050715     C                   EVAL      VIDCAT    = *blanks
092400050715     C***                EVAL      VIDCATFLG = 'E'
092500050715     C                   EVAL      VIDCATFLG = *blanks
092600050715     C***                EVAL      VIDTIP    = 'NA'
092700050715     C                   EVAL      VIDTIP    = *blanks
092800050715     C                   EVAL      VIDSPED   = 50
092900050706     C                   EVAL      VIDLAY    = 'D'
093000050714     C                   EVAL      VIDDIV    = *blanks
093100050711     C                   EVAL      VIDARE    = *zeros
093200050711     C                   EVAL      VIDFIL    = *zeros
093300050711     C                   EVAL      VIDAGE    = *zeros
093400050714     C                   EVAL      VIDDIVD   = *blanks
093500050714     C                   EVAL      VIDARED   = *blanks
093600050714     C                   EVAL      VIDFILD   = *blanks
093700050714     C                   EVAL      VIDAGED   = *blanks
093800050711     C*
093900050704     C                   EVAL      VIDFILE   = 'N'
094000980304     C*
094100980304     C                   ENDSR
094200980304     C*------------------------------------------------------------------------*
094300980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
094400980304     C*------------------------------------------------------------------------*
094500980304     C     FUNVAR        BEGSR
094600980304     C*
094700980304     C* INIZIALIZZA VARIABILE -FUNKEY-
094800980304     C*
094900980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
095000980304     C     KEY           IFEQ      ENTER
095100980304     C                   MOVEL     *BLANKS       FUNKEY
095200980304     C                   END
095300980304     C     KEY           IFEQ      F1
095400980304     C                   MOVEL     '01'          FUNKEY
095500980304     C                   END
095600980304     C     KEY           IFEQ      F2
095700980304     C                   MOVEL     '02'          FUNKEY
095800980304     C                   END
095900980304     C     KEY           IFEQ      F3
096000980304     C                   MOVEL     '03'          FUNKEY
096100980304     C                   END
096200980304     C     KEY           IFEQ      F4
096300980304     C                   MOVEL     '04'          FUNKEY
096400980304     C                   END
096500980304     C     KEY           IFEQ      F5
096600980304     C                   MOVEL     '05'          FUNKEY
096700980304     C                   END
096800980304     C     KEY           IFEQ      F6
096900980304     C                   MOVEL     '06'          FUNKEY
097000980304     C                   END
097100980304     C     KEY           IFEQ      F7
097200980304     C                   MOVEL     '07'          FUNKEY
097300980304     C                   END
097400980304     C     KEY           IFEQ      F8
097500980304     C                   MOVEL     '08'          FUNKEY
097600980304     C                   END
097700980304     C     KEY           IFEQ      F9
097800980304     C                   MOVEL     '09'          FUNKEY
097900980304     C                   END
098000980304     C     KEY           IFEQ      F10
098100980304     C                   MOVEL     '10'          FUNKEY
098200980304     C                   END
098300980304     C     KEY           IFEQ      F11
098400980304     C                   MOVEL     '11'          FUNKEY
098500980304     C                   END
098600980304     C     KEY           IFEQ      F12
098700980304     C                   MOVEL     '12'          FUNKEY
098800980304     C                   END
098900980304     C     KEY           IFEQ      F13
099000980304     C                   MOVEL     '13'          FUNKEY
099100980304     C                   END
099200980304     C     KEY           IFEQ      F14
099300980304     C                   MOVEL     '14'          FUNKEY
099400980304     C                   END
099500980304     C     KEY           IFEQ      F15
099600980304     C                   MOVEL     '15'          FUNKEY
099700980304     C                   END
099800980304     C     KEY           IFEQ      F16
099900980304     C                   MOVEL     '16'          FUNKEY
100000980304     C                   END
100100980304     C     KEY           IFEQ      F17
100200980304     C                   MOVEL     '17'          FUNKEY
100300980304     C                   END
100400980304     C     KEY           IFEQ      F18
100500980304     C                   MOVEL     '18'          FUNKEY
100600980304     C                   END
100700980304     C     KEY           IFEQ      F19
100800980304     C                   MOVEL     '19'          FUNKEY
100900980304     C                   END
101000980304     C     KEY           IFEQ      F20
101100980304     C                   MOVEL     '20'          FUNKEY
101200980304     C                   END
101300980304     C     KEY           IFEQ      F21
101400980304     C                   MOVEL     '21'          FUNKEY
101500980304     C                   END
101600980304     C     KEY           IFEQ      F22
101700980304     C                   MOVEL     '22'          FUNKEY
101800980304     C                   END
101900980304     C     KEY           IFEQ      F23
102000980304     C                   MOVEL     '23'          FUNKEY
102100980304     C                   END
102200980304     C     KEY           IFEQ      F24
102300980304     C                   MOVEL     '24'          FUNKEY
102400980304     C                   END
102500980304     C     KEY           IFEQ      ROLLUP
102600980304     C                   MOVEL     'RU'          FUNKEY
102700980304     C                   END
102800980304     C     KEY           IFEQ      HELP
102900980304     C                   MOVEL     'HP'          FUNKEY
103000980304     C                   END
103100980304     C                   ENDSR
103200980304     C*------------------------------------------------------------------------*
103300980304     C* NOTE COSTRUZIONE SCHIERE
103400980304     C*------------------------------------------------------------------------*
103500980304     C*
103600980304     C* -MSG-MESSAGGI
103700980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
103800980304     C* (NON TOCCARE MSG,1 E MSG,2)
103900980304     C*
104000980304     C* -CMD-COMANDI
104100980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
104200980304     C*
104300980304     C* -OPD-OPZIONI
104400980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
104500980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
104600980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
104700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
104800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
104900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
105000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
105100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
105200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
105300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
105400980304     C*
105500980304     C* -FUD-FUNZIONI
105600980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
105700980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
105800980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
105900980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
106000980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
106100980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
106200980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
106300980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
106400980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
106500980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
106600980304     o*-------------------------------------------------------------------------
106700980304** msg - messaggi
106800050704TIST38R- Funzione non attiva
106900050704TIST38R- Se indicata categoria cliente specificare se inclusa o esclusa
107000050704TIST38R- Data impostata non valida.
107100050704TIST38R- Divisione non accettabile.
107200050704TIST38R- Area non accettabile.
107300050704TIST38R- Punto operativo non accettabile.
107400050711TIST38R- Commerciale non accettabile.
107500050711TIST38R- Data oltre limite. Limite minimo:
107600050711TIST38R- Data oltre limite. Limite massimo:
107700050704TIST38R- 10
107800980304** FUD - FUNZIONI
107900980304                   -        (S/N)=N                        funz.01¬
108000980304                   -        (S/N)=N                        funz.02¬
108100980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
108200980304                   -        (S/N)=N                        funz.04¬
108300980304                   -        (S/N)=N                        funz.05¬
108400041221F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
108500980304                   -        (S/N)=N                        funz.07¬
108600980304                   -        (S/N)=N                        funz.08¬
108700980304                   -        (S/N)=N                        funz.09¬
108800980304                   -        (S/N)=N                        funz.10¬
108900980304                   -        (S/N)=N                        funz.11¬
109000980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
109100980304                   -        (S/N)=N                        funz.13¬
109200980304                   -        (S/N)=N                        funz.14¬
109300980304                   -        (S/N)=N                        funz.15¬
109400980304                   -        (S/N)=N                        funz.16¬
109500980304                   -        (S/N)=N                        funz.17¬
109600980304                   -        (S/N)=N                        funz.18¬
109700980304                   -        (S/N)=N                        funz.19¬
109800980304                   -        (S/N)=N                        funz.20¬
109900980304                   -        (S/N)=N                        funz.21¬
110000980304                   -        (S/N)=N                        funz.22¬
110100980304                   -        (S/N)=N                        funz.23¬
110200980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
