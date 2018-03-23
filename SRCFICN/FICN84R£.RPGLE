000100100317     H DECEDIT('0,') DATEDIT(*DMY.)
000200011030     H* ficn84R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400090430     H*              DUPLICAZIONE TARIFFA COOP                       *
000500000000     H*--------------------------------------------------------------*
000600011030     Fficn84D   CF   E             WORKSTN
000700011030     Ffifgt01L  IF   E           K DISK
000800931018     FTABEL00F  IF   E           K DISK
000900021203     Ffiapd01L  IF   E           K DISK
001000011109     Ftntbe01L  IF   E           K DISK
001100931018     D TTR             S              1    DIM(50)                              COD. TARIFFA
001200931018     D DTR             S             15    DIM(50)                              DESCR.COD. TARIFFA
001300931104     D TSR             S              1    DIM(50)                              TIPO PRESTAZIONE
001400931104     D DSR             S             15    DIM(50)                              DESCR.TIPO PRESTAZIO
001500931104     D CDT             S              3    DIM(50)                              COD.TAR. DEL TIPO PR
001600070307     D TRUL72DS      E DS
001700070307     D  L72                           3s 0 DESCEND
001800070307     D                                     DIM(60)
001900070307     D                                     OVERLAY(O72Lia)
002000950113     D* SCHIERE PER CARICAMENTO L1 E L6
002100950113     D L6              S              3  0 DIM(30)                              FIL GESTITE
002200940922     D*
002300931018     D PARAM           DS
002400931018     D  FILOLD                 1      3  0
002500931018     D  PDROLD                 4      7
002600931018     D  COMOLD                 1      7  0
002700931018     D  SMLOLD                 8      8
002800931018     D  TSROLD                 9      9
002900931018     D  CTROLD                10     12  0
003000931018     D  PRGOLD                13     15  0
003100931018     D  FILNEW                16     18  0
003200931018     D  PDRNEW                19     22
003300931018     D  COMNEW                16     22  0
003400931018     D  SMLNEW                23     23
003500931018     D  TSRNEW                24     24
003600931018     D  CTRNEW                25     27  0
003700011126     D  divold                28     30
003701090116     D* ds parametri controllo esistenza altra tariffa valida coop
003702090116     D paramt          DS
003703090116     D  datax                         8s 0
003704090116     D  pogesx                        3s 0
003705090116     D  tipopre                       1
003706090116     d  error                         1
003800931018     D*
003900021203     D fnlv24ds      E DS
003901100317     D ficn70ds      E DS
003902100317     D daut          E DS
004000931018     D*
004100000000     D KPJBA         E DS
004200940922     D*
004300940922     D* DS PER TRUL06R - CARICAMENTO £1
004400940922     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004500940922     D  LIN                    1     90  0
004600940922     D                                     DIM(30)                              SKI COMODO
004700940922     D*
004800931018     D DS1Z          E DS
004900940922     D DSTR          E DS
005100931018     D CNCR80        E DS
005200921030     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300011109     D WLBDAT          DS
005400011109     D  G02DAT                 1      8  0
005500011109     D  G02INV                 9     16  0
005600011109     D  G02ERR                17     17
005700011109     D  G02TGI                18     22  0
005800000000     I/SPACE 3
005900931018     C*****************************************************************
006000931018     C* RIEPILOGO INDICATORI
006100931018     C*****************************************************************
006200931018     C* 30    - DI COMODO
006300931018     C* 35    - USATO PER LOKUP SU SCHIERE E TESTN
006400940923     C* 40/52 - INDICATORI PER MESSAGGI DI ERRORE
006500020717     C* 89    - SONO UN 2° liv.
006600931018     C* 90    - SEGNALA LA PRESENZA DI ERRORI
006700931018     C*****************************************************************
006800000000     C     *ENTRY        PLIST
006900000000     C                   PARM                    KPJBA
007000021216     c                   movel     kpjbu         parana            1
007100000000     C                   Z-ADD     1             CODUT
007200921030     C                   CALL      'X§PARUT'
007300921030     C                   PARM                    UT§DSE
007400000000     C                   MOVEL     RAGUT         RSUT             20
007500931018     C                   MOVEL     REC80         CNCR80
007600011109     C***
007700011109     C* GIRO DATA DEL GIORNO: LA PRENDO DA TIME
007800011109     C***
007900011109     C                   TIME                    W0120            14 0
008000011109     C                   MOVE      W0120         WDAT              8 0
008100011109     C*
008200011109     C                   Z-ADD     WDAT          G02DAT
008300011109     C                   MOVEL     *BLANK        G02ERR
008400011109     C                   CALL      'XSRDA8'
008500011109     C                   PARM                    WLBDAT
008600011109     C* UDATE A 8 IN AAAA/MM/GG
008700011109     C                   Z-ADD     G02INV        WUDA              8 0
008800921030     C*---------------------------------------------------------------*
008900931018     C* ACCESSO TABEL
009000931018     C     KTAB          KLIST
009100931018     C                   KFLD                    CODUT
009200940922     C                   KFLD                    COD
009300940922     C                   KFLD                    KEY
009400931018     C     KTAB2         KLIST
009500931018     C                   KFLD                    CODUT
009600931018     C                   KFLD                    COD
009700011030     C* ACCESSO fifgt
009800931018     C     KFGT          KLIST
009900931018     C                   KFLD                    COMOLD
010000931018     C                   KFLD                    SMLOLD
010100931018     C                   KFLD                    TSROLD
010200931018     C                   KFLD                    CTROLD
010300931018     C                   KFLD                    PRGOLD
010400011109     C* ACCESSO Tntbe
010500011109     C     Ktbe          KLIST
010600011109     C                   KFLD                    kcod1
010700011109     C                   KFLD                    kke1
010800011109     C*
010900011126     C* ACCESSO fifgt
011000011126     C     KFGT2         KLIST
011100011126     C                   KFLD                    COMnew
011200011126     C                   KFLD                    SMLnew
011300011126     C                   KFLD                    TSRnew
011400011126     C                   KFLD                    CTRnew
011500021203     C* ACCESSO fiapd
011600021203     C     Kapdn         KLIST
011700021216     C                   KFLD                    parana
011800021203     C                   KFLD                    comnew
011900021203     C     Kapdo         KLIST
012000021216     C                   KFLD                    parana
012100021203     C                   KFLD                    comold
012200940922     C*
012300940922     C* DEFINIZIONE CAMPI
012400011109     C     *LIKE         DEFine    TBECOD        KCOD1
012500011109     C     *LIKE         DEFine    TBEKE1        KKE1
012600940922     C     *LIKE         DEFINE    TBLCOD        COD
012700940922     C     *LIKE         DEFINE    TBLKEY        KEY
012800940922     C*
012900000000     C*---------------------------------------------------------------*
013000931018     C* CARICAMENTO TABELLE
013100931018     C                   EXSR      CARTAB
013200931018     C*
013300931018     C* PULIZIA CAMPI FORMATO
013400931018     C                   EXSR      PULIZ
013500931018     C*
013600931018     C     FOR01         TAG
013700011030     C                   EXFMT     cn84D01
013800931018     C** CMD3 - FINE LAVORO
013900931018     C   KC              GOTO      FINE
014000940922     C*
014100931018     C* CONTROLLI FORMATO1
014200931018     C                   EXSR      CONTR
014300940922     C  NKF
014400931018     COR 90              GOTO      FOR01
014500931018     C*
014600940922     C** CMD6 - CONFERMA ELABORAZIONE
014700931018     C                   MOVEL     PARAM         KPJBU
014800090109     C                   MOVEL     'FN13'        KCOAZ
014900931018     C                   CALL      'BCH10'
015000931018     C                   PARM                    KPJBA
015100931018     C*
015200931018     C     FINE          TAG
015300931018     C                   SETON                                        LR
015400931018     C*
015500931018     C*--- CARICAMENTO TABELLE ---------------------------------------*
015600931018     C     CARTAB        BEGSR
015700931018     C****  CODICI TARIFFA E DESCRIZIONI: TTR / DTR / MIN  ****
015800931018     C                   Z-ADD     1             X                 3 0
015900931018     C                   MOVEL     'TR'          COD
016000931018     C     KTAB2         SETLL     TABEL
016100931018     C     KTAB2         READE     TABEL                                  30
016200931018     C*
016300931018     C     *IN30         DOWEQ     *OFF
016400931018     C     TBLFLG        IFEQ      ' '
016500940922     C                   MOVEL     TBLUNI        DSTR
016600940922     C*
016700090116     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE coop
017400021216     c                   if        parana = 'C' and §trutp = 'C'
017500021216     C                   MOVEL     TBLKEY        TTR(X)
017600021216     C                   MOVEL     TBLUNI        DTR(X)
017700021216     C                   ADD       1             X
017800021216     C                   ENDIF
017900021216     C*
018000931018     C                   ENDIF
018100931018     C     KTAB2         READE     TABEL                                  30
018200931018     C                   ENDDO
018300931018     C*
018400931104     C****  TIPI PRESTAZIONE E DESCRIZIONI: TSR / DSR / CDT  ****
018500931018     C                   Z-ADD     1             X
018600931018     C                   MOVEL     '1Z'          COD
018700931018     C     KTAB2         SETLL     TABEL
018800931018     C     KTAB2         READE     TABEL                                  30
018900931018     C*
019000931018     C     *IN30         DOWEQ     *OFF
019100931018     C     TBLFLG        IFEQ      ' '
019200931018     C                   MOVEL     TBLUNI        DS1Z
019400090116     c                   if        parana = 'C' and §1zfac ='C'
019500021216     C                   MOVEL     TBLKEY        TSR(X)
019600931018     C                   MOVEL     §1ZDES        DSR(X)
019700931104     C* CODICE TARIFFA DEL TIPO PRESTAZIONE
019800931018     C                   MOVEL     §1ZCTR        CDT(X)
019900931018     C                   ADD       1             X
020000931018     C                   ENDIF
020100021216     C                   ENDIF
020200931018     C     KTAB2         READE     TABEL                                  30
020300931018     C                   ENDDO
020400931018     C*
020500940922     C***
020600070307     C* CARICO TABELLA FILIALI contabili GESTITE
020700940922     C***
020800070307     c                   clear                   trul72ds
020900070307     c                   eval      i72tla = 'L'
021000070307     c                   eval      I72POU = simpou
021100070307     c                   eval      kpjbu = trul72ds
021200070307     C                   CALL      'TRUL72R'
021300070307     C                   PARM                    KPJBA
021400070307     c                   movel     kpjbu         trul72ds
021500940922     C*
021600020717     C                   MOVEL     simpou        FILNEW
021700020717     C                   MOVEL     simpou        FILOLD
021800950113     C***
021900950113     C* VEDO SE SONO SIMFEL O REMOTO
022000950113     C***
022100020717     c                   if        simtpp = '2'
022200950113     C                   SETON                                        89
022300950113     C***
022400950113     C* CARICO TABELLA FILIALI GESTITE £6
022500950113     C***
022600950113     C                   CLEAR                   DSUL06
022700950113     C                   MOVE      '£6'          D06COD
022800020717     C                   MOVEL     simpou        D06KEY
022900950113     C                   MOVEL     DSUL06        KPJBU
023000950113     C*
023100950113     C                   CALL      'TRUL06R'
023200950113     C                   PARM                    KPJBA
023300950113     C                   MOVEL     KPJBU         DSUL06
023400950113     C                   MOVEA     LIN           L6
023500950113     C                   ENDIF
024600950113     C*
024700931018     C                   ENDSR
024800931018     C*
024900931018     C*--- PULIZIA CAMPI DEL FORMATO ---------------------------------*
025000931018     C     PULIZ         BEGSR
025100931018     C                   MOVEL     *BLANKS       PDRNEW
025200931018     C                   MOVEL     *BLANKS       SMLNEW
025300931018     C                   MOVEL     *BLANKS       TSRNEW
025400931018     C                   Z-ADD     0             CTRNEW
025500931018     C                   MOVEL     *BLANKS       PDROLD
025600931018     C                   MOVEL     *BLANKS       SMLOLD
025700931018     C                   MOVEL     *BLANKS       TSROLD
025800931018     C                   Z-ADD     0             CTROLD
025900931018     C                   Z-ADD     0             PRGOLD
026000931018     C                   MOVEL     *BLANKS       DPDNEW
026100931018     C                   MOVEL     *BLANKS       DTSNEW
026200931018     C                   MOVEL     *BLANKS       DCTNEW
026300931018     C                   MOVEL     *BLANKS       DPDOLD
026400931018     C                   MOVEL     *BLANKS       DTSOLD
026500931018     C                   MOVEL     *BLANKS       DCTOLD
026600931018     C                   ENDSR
026700931018     C*
026800931018     C*--- CONTROLLI FORMATO -----------------------------------------*
026900931018     C     CONTR         BEGSR
027000931018     C                   SETOFF                                       90
027100940922     C*
027200940922     C****  CODICE FILIALE NUOVA  ****
027300950113     C* FILIALE OBBLIGATORIA
027400950113     C     FILNEW        IFEQ      0
027500950113     C                   SETON                                        5190
027600950113     C                   GOTO      ENDCTR
027700950113     C                   ENDIF
027701130813      *controllo se utente autorizzato alla funzione
027702130813     c                   movel     simpou        i70pog
027703130813     c                   movel     simpou        i70pge
027704130813     c                   movel     knmus         i70prf
027705130813     c                   call      'FICN70R'
027706130813     c                   parm                    ficn70ds
027707130813     c                   movel     o70uni        daut
027708130813     c                   if        §autmtcc <>'S'
027709130813     C                   SETON                                        5790
027710130813     C                   GOTO      ENDCTR
027712130813     c                   endif
027713130813     c
027800950113     C*
027900950113     C* 89 OFF - SONO SIMFEL QUINDI CONTROLLO LA FILIALE NELLA L1
028000950113     C     *IN89         IFEQ      *OFF
028100021216     C     FILNEW        IFNE      SIMpou
028200021216     C     simtpp        andeq     '1'
028300070307     C     FILNEW        LOOKUP    L72                                    35
028400940922     C  N35              SETON                                        5190
028500940922     C  N35              GOTO      ENDCTR
028600940922     C                   ENDIF
028700931018     C*
028800950113     C                   ELSE
028900020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
029000020717     C     filnew        IFNE      simpou
029100950113     C     FILNEW        LOOKUP    L6                                     35
029200950113     C  N35              SETON                                        5190
029300950113     C  N35              GOTO      ENDCTR
029400950113     C                   ENDIF
029500950113     C                   ENDIF
029600950113     C*
029700931018     C****  CODICE PADRONCINO NUOVO  ****
029800931018     C     '?'           SCAN      PDRNEW                                 90
029900931018     C*
030000931018     C     *IN90         IFEQ      *ON
030100021203     c                   clear                   fnlv24ds
030200021203     C                   Z-ADD     FILNEW        d24FIL
030300021203     C                   MOVEL     'R'           d24FLG
030400030107     C                   MOVEL     parana        d24tip
030500021203     C                   MOVEL(p)  fnlv24ds      KPJBU
030600940926     C                   CALL      'FNLV24R'
030700931018     C                   PARM                    KPJBA
030800021203     C                   MOVEL     KPJBU         fnlv24ds
030900950113     C*
031000950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
031100021203     C     d24pdr        IFNE      0
031200021203     C                   MOVEL     d24pdr        FILNEW
031300931018     C                   MOVEL     *ZEROS        PDRNEW
031400021203     C                   MOVE      d24pdr        PDRNEW
031500021203     C                   MOVEL     d24rsc        DPDNEW
031600950113     C                   ENDIF
031700950113     C*
031800931018     C                   GOTO      ENDCTR
031900931018     C                   ENDIF
032000931018     C*
032100931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
032200931018     C                   TESTN                   PDRNEW               35
032300931018     C  N35              SETON                                        4090
032400931018     C  N35              GOTO      ENDCTR
032500931018     C*
032600940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
032700021203     C     kapdn         CHAIN     fiapd000                           30
032800940922     C  N30APDATB        IFNE      ' '
032900940922     C                   SETON                                        30
033000940922     C                   ENDIF
033100940922     C* CODICE PADRONCINO ERRATO
033200940922     C   30              SETON                                        4090
033300940922     C   30              GOTO      ENDCTR
033400100223     C                   MOVEL     APDRSf        DPDNEW
033500931018     C*
033600931104     C****  TIPO PRESTAZIONE NUOVO  ****
033700931018     C     TSRNEW        IFEQ      '?'
033800931018     C                   MOVEL     '1Z'          COD
033900931018     C                   MOVEL     ' '           TSRNEW
034000931018     C                   MOVEL     *BLANKS       DES              25
034100931018     C                   CALL      'X§TABER'
034200931018     C                   PARM                    CODUT
034300931018     C                   PARM                    COD
034400931018     C                   PARM                    KEY
034500931018     C                   PARM                    DES
034600931018     C                   MOVEL     KEY           TSRNEW
034700931018     C                   MOVEL     DES           DTSNEW
034800931018     C                   SETON                                        90
034900931018     C                   GOTO      ENDCTR
035000931018     C                   ENDIF
035100931018     C*
035200931104     C* CONTROLLO VALIDITA' TIPO PRESTAZIONE
035300931018     C     TSRNEW        IFEQ      ' '
035400931018     C                   SETON                                        4190
035500931018     C                   GOTO      ENDCTR
035600931018     C                   ENDIF
035700931018     C*
035800931018     C                   Z-ADD     1             X
035900931018     C     TSRNEW        LOOKUP    TSR(X)                                 35
036000931104     C* TIPO PRESTAZIONE ERRATO
036100931018     C  N35              SETON                                        4190
036200931018     C  N35              GOTO      ENDCTR
036300931018     C                   MOVEL     DSR(X)        DTSNEW
036400931018     C                   MOVEL     CDT(X)        COMCDT            3
036600931019     C****  CODICE TARIFFA NUOVO  ****
036700931019     C                   MOVEL     *BLANKS       DCTNEW
036800931105     C                   MOVEL     CTRNEW        COMCTR            3
036900931105     C*
038500090116     C* TIPO PRESTAZIONE
038600931105     C*
038700931105     C                   Z-ADD     1             X
038800090116     C                   MOVEL     CTRNEW        UNOCTR            1
038900931105     C     UNOCTR        LOOKUP    TTR(X)                                 35
039001090116     C* CODICE TARIFFA ERRATO
039100931105     C     COMCTR        IFNE      COMCDT
039300090116     C                   SETON                                        4390
039400090116     C                   GOTO      ENDCTR
039600931105     C*
039700931105     C                   ELSE
039800931105     C   35              MOVEL     DTR(X)        DCTNEW
039900931105     C                   ENDIF
040300090116     C* controllo che se esiste già un progressivo con scada il 31/12/2039
040301090116     c     kfgt2         setgt     fifgt01l
040302090116     c     kfgt2         readpe    fifgt01l
040303090116     c                   if        not %eof(fifgt01l) and
040304090116     c                             fgtdst = 20391231
040305090116     C                   SETON                                        5690
040306090116     C                   GOTO      ENDCTR
040307090116     c                   end
040317090116     c
040400940923     C****  CODICE FILIALE  ****
040500950113     C* FILIALE OBBLIGATORIA
040600950113     C     FILOLD        IFEQ      0
040700950113     C                   SETON                                        5290
040800950113     C                   GOTO      ENDCTR
040900950113     C                   ENDIF
041000950113     C*
041100070307     C* 89 OFF -  CONTROLLO LA FILIALE NELLA L72  filiali contabili
041200950113     C     *IN89         IFEQ      *OFF
041300021216     C     FILOLD        IFNE      SIMpou
041400021216     C     simtpp        andeq     '1'
041500070307     C     FILOLD        LOOKUP    L72                                    35
041600940923     C  N35              SETON                                        5290
041700940923     C  N35              GOTO      ENDCTR
041800940923     C                   ENDIF
041900950113     C*
042000950113     C                   ELSE
042100020717     C* 89 ON  - SONO UN 2° liv.  QUINDI CONTROLLO LA FILIALE NELLA L6
042200020717     C     filold        IFNE      simpou
042300950113     C     FILOLD        LOOKUP    L6                                     35
042400950113     C  N35              SETON                                        5290
042500950113     C  N35              GOTO      ENDCTR
042600950113     C                   ENDIF
042700950113     C                   ENDIF
042800940923     C*
042900931018     C****  CODICE PADRONCINO VECCHIO  ****
043000931018     C     '?'           SCAN      PDROLD                                 90
043100931018     C*
043200931018     C     *IN90         IFEQ      *ON
043300021203     c                   clear                   fnlv24ds
043400021203     C                   Z-ADD     FILOLD        d24FIL
043500021203     C                   MOVEL     'R'           d24FLG
043600030107     C                   MOVEL     parana        d24tip
043700021203     C                   MOVEL(p)  fnlv24ds      KPJBU
043800940926     C                   CALL      'FNLV24R'
043900931018     C                   PARM                    KPJBA
044000021203     C                   MOVEL     KPJBU         fnlv24ds
044100950113     C*
044200950113     C* CONTROLLO SE E' STATO SELEZIONATO UN CODICE PADRONCINO
044300021203     C     d24pdr        IFNE      0
044400021203     C                   MOVEL     d24pdr        FILOLD
044500931018     C                   MOVEL     *ZEROS        PDROLD
044600021203     C                   MOVE      d24pdr        PDROLD
044700021203     C                   MOVEL     d24rsc        DPDOLD
044800950113     C                   ENDIF
044900950113     C*
045000931018     C                   GOTO      ENDCTR
045100931018     C                   ENDIF
045200931018     C*
045300931018     C* CONTROLLO VALIDITA' DEL CODICE PADRONCINO
045400931018     C                   TESTN                   PDROLD               35
045500931018     C  N35              SETON                                        4490
045600931018     C  N35              GOTO      ENDCTR
045700931018     C*
045800940922     C* CONTROLLO ESISTENZA DEL PADRONCINO IN ANAGRAFICA
045900021203     C     kapdo         CHAIN     fiapd000                           30
046000940922     C  N30APDATB        IFNE      ' '
046100940922     C                   SETON                                        30
046200940922     C                   ENDIF
046300940922     C* CODICE PADRONCINO ERRATO
046400940922     C   30              SETON                                        4490
046500940922     C   30              GOTO      ENDCTR
046600100223     C                   MOVEL     APDRSf        DPDOLD
046700931018     C*
046800931104     C****  TIPO PRESTAZIONE VECCHIO  ****
046900931018     C     TSROLD        IFEQ      '?'
047000931018     C                   MOVEL     '1Z'          COD
047100931018     C                   MOVEL     ' '           TSROLD
047200931018     C                   MOVEL     *BLANKS       DES
047300931018     C                   CALL      'X§TABER'
047400931018     C                   PARM                    CODUT
047500931018     C                   PARM                    COD
047600931018     C                   PARM                    KEY
047700931018     C                   PARM                    DES
047800931018     C                   MOVEL     KEY           TSROLD
047900931018     C                   MOVEL     DES           DTSOLD
048000931018     C                   SETON                                        90
048100931018     C                   GOTO      ENDCTR
048200931018     C                   ENDIF
048300931018     C*
048400931104     C* CONTROLLO VALIDITA' TIPO PRESTAZIONE
048500931018     C     TSROLD        IFEQ      ' '
048600931018     C                   SETON                                        4590
048700931018     C                   GOTO      ENDCTR
048800931018     C                   ENDIF
048900931018     C*
049000931018     C                   Z-ADD     1             X
049100931018     C     TSROLD        LOOKUP    TSR(X)                                 35
049200931104     C* TIPO PRESTAZIONE ERRATO
049300931018     C  N35              SETON                                        4590
049400931018     C  N35              GOTO      ENDCTR
049500931018     C                   MOVEL     DSR(X)        DTSOLD
049600931019     C                   MOVEL     CDT(X)        COMCDT
049700931018     C*
049800931104     C****  CONTROLLO CONGRUENZA TIPI PRESTAZIONE  ****
049900931019     C     TSRNEW        IFNE      TSROLD
050000931019     C                   SETON                                        4890
050100931019     C                   GOTO      ENDCTR
050200931019     C                   ENDIF
050300931019     C*
050400931019     C****  CODICE TARIFFA VECCHIO  ****
050500931019     C                   MOVEL     *BLANKS       DCTOLD
050600931105     C                   MOVEL     CTROLD        COMCTR
050700931105     C*
052300090116     C* TIPO PRESTAZIONE
052400931105     C*
052500931105     C                   Z-ADD     1             X
052600931105     C                   MOVEL     CTROLD        UNOCTR
052700931105     C     UNOCTR        LOOKUP    TTR(X)                                 35
052801090116     C* CODICE TARIFFA ERRATO
052900931105     C     COMCTR        IFNE      COMCDT
053100090116     C                   SETON                                        4790
053200090116     C                   GOTO      ENDCTR
053400931105     C*
053500931105     C                   ELSE
053600931105     C   35              MOVEL     DTR(X)        DCTOLD
053700931105     C                   ENDIF
054000931019     C*
054100931018     C****  CONTROLLO CONGRUENZA CODICI TARIFFA  ****
054200931018     C     CTROLD        IFNE      999
054300931018     C                   MOVEL     CTROLD        UNOOLD            1
054400931018     C                   MOVEL     CTRNEW        UNONEW            1
054500931018     C     UNOOLD        IFNE      UNONEW
054600931019     C                   SETON                                        4990
054700931018     C                   GOTO      ENDCTR
054800931018     C                   ENDIF
054900931018     C                   ELSE
055000931018     C     CTROLD        IFNE      CTRNEW
055100931019     C                   SETON                                        4990
055200931018     C                   GOTO      ENDCTR
055300931018     C                   ENDIF
055400931018     C                   ENDIF
055500931018     C*
055600931018     C****  CONTROLLO VALIDITA' DELLA TARIFFA DA CUI COPIARE  ****
055700090116     C     KFGT          CHAIN     fifgt000                           30
055800090116     c                   if        *in30
055801090116     C                   SETON                                        5090
055802090116     c                   goto      endctr
055803090116     c                   else
055900011109     C     fgtatb        IFNE      ' '
056000011109     C                   SETON                                        5090
056100011109     c                   goto      endctr
056200011109     C                   END
057400011126      *se non è stata passata imposto la divisa
057500011126     C                   IF        divold = *blank
057600011126     c                   move      fgtdiv        divold
057700011126     c                   end
057910090116     c                   end
059200931018     C*
059300931018     C     ENDCTR        ENDSR
