000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200911022     H* FLFG82R *----------------------------------------------------*
000300080522     H*                                                              *
000400911022     H*          - IMMISSIONE/ESECUZIONE DISPOSIZIONI -              *
000500940920     H*                  PARTENZA/ARRIVO                             *
000600900418     H*  03 FINE LAVORO                                              *
000700940920     H*  28 ERRORE GENERICO                                          *
000800040112     H*--------------------------------------------------------------*
000900940920     FFNLG82D   CF   E             WORKSTN
001000911022     F                                     SFILE(FG82SF:NRR)
001100940920      * PARTENZA
001200940920      *---------
001300940920     FFNBLP01L  IF   E           K DISK    USROPN
001400050215     Ftigcp06L  IF   E           K DISK    USROPN
001500050215     Ftigcp02L  IF   E           K DISK    USROPN
001600050215     F                                     RENAME(tigcp000:tigcp002)
001700050215     Ftigcp07L  IF   E           K DISK    USROPN
001800050215     F                                     RENAME(tigcp000:tigcp007)
001900990730     FTITGC01L  IF   E           K DISK    USROPN
002000940920     FCNACO00F  IF   E           K DISK    USROPN
002100940920      *
002200940920      * ARRIVO
002300940920      *---------
002400940920     FFNARB01L  IF   E           K DISK    USROPN
002500050215     Ftignp01L  IF   E           K DISK    USROPN
002600050215     Ftigcp21L  IF   E           K DISK    USROPN
002700050215     F                                     RENAME(tigcp000:tigca021)
002800050215     Ftigcp24L  IF   E           K DISK    USROPN
002900050215     F                                     RENAME(tigcp000:tigcp024)
003000080603     Ffiar401l  IF   E           K DISK
003100911010     FTABEL00F  IF   E           K DISK
003200020227     F*ZORG01LIF  F    5000  2PI     4 DISK
003300080424     Fazorg01L  IF   E           K DISK
003400941223     FFNLV22P   O    F  132        PRINTER USROPN
003500911010      *
003600020228     E* ? COD. KSC
003700911023     D KSC§            S              1    DIM(7)
003800020228     E* FILIALI IN GESTIONE
003900940922     D F£6             S              3    DIM(29)
004000020228     E* FIL. GEST. SU AS
004100020821     D F£1             S              3    DIM(29)
004200020228     E* CODICI PART.GIAC.
004300940926     D C7Q             S              2    DIM(100)
004400020228     E* TAB.COD.PART. GIAC.
004500940926     D D7Q             S             89    DIM(100)
004600090819     E* Clienti con Particolarità
004700090819     D K7U             S              7s 0 DIM(999)
004800020228     E* ERRORI
004900090819     D MSG             S             70    DIM(22) CTDATA PERRCD(1)
005000911115     D*
005100900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
005200900418     D  TCU                  398    697
005300900509     D                                     DIM(50)
005400900418     D  KCU                  698    847P 0
005500900509     D                                     DIM(50)
005600900509     D                                     PACKEVEN
005700900509     D TCUDS           DS
005800900509     D  F1                     1      1
005900900509     D  F3                     3      3
006000900509     D  F2                     2      2
006100900509     D  F4                     4      4
006200900509     D  F56                    5      6
006300060925     D**NCR80        E DS
006400160302     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
006500160302     D TRULC7DS      E DS
006600940920     D KPJBA         E DS
006700020503     d trul06ds      e ds
006800020503     d  lin                    1     87    dim(29)
006900050701     d fnlg09ds      e ds
007000950215     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
007100161118     D*PARAM0          DS
007200161118     D* PA0AAS                 1      4  0
007300161118     D* PA0LNP                 5      7  0
007400161118     D* PA0NRS                 8      9  0
007500161118     D* PA0NSP                10     16  0
007600161118     D* PA0FLG                18     18
007700161118     D* PA0SIM                44     46  0
007800161118     D* PA0F03                48     48
007900161118     D*PARAM1          DS
008000161118     D* PA1AAS                14     17  0
008100161118     D* PA1LNP                18     20  0
008200161118     D* PA1NRS                21     22  0
008300161118     D* PA1NSP                23     29  0
008400161118     D* PA1F03                30     30
008500161118     D* PA1FLG                31     31
008600161118     d fnlru6ds      e ds
008700161118      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
008800161118     D PARAMu6ds       DS
008900161118     d* CAMPI DI OUTPUT:
009000161118     d* . Key spedizione
009100161118     D  pa1AAS                14     17  0
009200161118     D  pa1LNP                18     20  0
009300161118     D  pa1NRS                21     22  0
009400161118     D  pa1NSP                23     29  0
009500161118     d* . Flag '1'=Premuto F3=Fine
009600161118     D  PA1F03                30     30
009700161118     d* CAMPI DI INPUT:
009800161118     d* . Modalità di richiamo
009900161118     D  PA1FLG                31     31
010000161118     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
010100161118     d*                                in interrogazione della bolla ed evitare
010200161118     d*                                errore di chiamata ricorsiva
010300161118     D  PA1GIA               144    144
010400161118     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
010500161118     d  pa1rma               149    163
010600161118     d  pa1xco               164    164
010700161118     d* non utilizzato
010800161118     d  pa1flo               165    256
010900161118      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
011000161118     D PARAMU6ds1      DS
011100161118     d** INPUT/OUTPUT (se richiesta la selezione bolla)
011200161118     D  pa1AAS_               14     17  0
011300161118     D  pa1LNP_               18     20  0
011400161118     D  pa1NRS_               21     22  0
011500161118     D  pa1NSP_               23     29  0
011600161118     d** OUTPUT
011700161118     D  PA1F03_               30     30
011800161118     d** INPUT
011900161118     D  PA1FLG_               31     31
012000161118     D*    SE RICH DA PGM GIAC. = 1
012100161118     D  PA1GIA_              144    144
012200161118     d** OUTPUT
012300161118     d  pa1rma_              149    163
012400161118     d  pa1xco_              164    164
012500161118     d** INPUT
012600161118     d*   Tipo lancio: "C" -           CHIUSO CON LR
012700161118     d*                "L" - ELABORO E CHIUDO CON LR
012800161118     d*                " " - ELABORO E CHIUDO IN RETRN
012900161118     d  pa1tla_              165    165
013000161118     d  pa1flo_              166    256
013100031217     d param80         ds
013200031217     d  fldlnpa                       3
013300031217     d  fldaasa                       4
013400031217     d  fldnrsa                       2
013500031217     d  fldnspa                       7
013600031217     d  fldcca                        1
013700031217     d  fldgga                        2
013800940919     D FNLG82        E DS                  EXTNAME(FNLG82DS)
013900941223     D FNLG30        E DS                  EXTNAME(FNLG30DS)
014000960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
014100940920     D                 DS
014200940920     D  §SEI                   1      6
014300940920     D  §PRF                   1      3
014400940920     D  §FLE                   4      6  0
014500140123     d                 ds
014600140123     d  §mussei                1      6
014700140123     d  knmus2                 1     10
014800140123      *
014900080424     D og143         E DS
015000080603     D DSbl4a        E DS
015100940920     D DS2G          E DS
015200050210     D DS20          E DS
015300940926     D DS7Q          E DS
015400040112     D DS7w          E DS
015500090819     D DS7U          E DS
015600020305     D*DSQT1         E DS
015700950116     D DS15          E DS
015800051005     D Dgcpflr       E DS
015900150514     D fnlv59ds      E DS
016000160302     d  KS_CT                 74    118    dim(15)
016100160302     d  KS_pr                119    163    dim(15)
016200160302     d  KS_ie                164    178    dim(15)
016300160302     d
016400060804     D TIBS34DS      E DS
016500060804     d Azuteds       e ds                  extname(Azute00f)
016600060804     d dDatiute      e ds
016700060804     d dLat          e ds
016800060804     d dute01        e ds
016900940920      * GESTIONE DATE
017000940920     D WLBDA8          DS
017100940920     D  G08DAT                 1      8  0
017200940920     D  G08INV                 9     16  0
017300940920     D  G08ERR                17     17
017400940920     D  G08TGI                18     22  0
017500911010      *
017600940920     D WGIDA8          DS
017700940920     D  GI8DAT                 1      8  0
017800940920     D  GI8INV                 9     16  0
017900940920     D  GI8TGI                17     21  0
018000911115     D*
018100940920      * PARAM X RICEZIONE BOLLE LOCALI - IN ARRIVO
018200930511     D FRB3DS          DS
018300930511     D  FRB3BD                 1      1
018400930511     D  FRB3BK                 2      2
018500930511     D  FRB3BT                 3      3
018600930511     D  FRB3T2                 4      4
018700930511     D  FRB3BG                 5      5
018800930511     D  FRB3EL                 6      8
018900940919     D                SDS
019000940919     D  NOMPGM                 1     10
019100940920      * DATA GIACENZA
019200940920     D                 DS
019300940920     D  DGC                    1      8  0
019400940920     D  DAGC                   1      4  0
019500940920     D  DMGC                   5      8  0
019600940920      * DATA GIACENZA
019700940920     D                 DS
019800940920     D  GCPMGC                 1      4  0
019900940920     D  MESE                   1      2  0
020000940920     D  GIOR                   3      4  0
020100080424      *
020200080424     D  Network_DPD    s              1    inz('N')
020300080424     D  Pulisce_DPD    s              1
020400050524      **********************************************************************
020500050524      * Constants
020600050524      *
020700050524      *     MaxKey - Maximum number of key fields allowed by this program
020800050524      *
020900050524      **********************************************************************
021000050525     D MaxKey          C                    2
021100050525     D mittente        C                   '1'
021200050525     D giacenza        C                   '2'
021300050524     D Ascendente      C                   1
021400050524     D Carattere       C                   6
021500050524     D Put             C                   1
021600050524     D EndPut          C                   2
021700050524     D Get             C                   3
021800050524      **********************************************************************
021900050524      * Standalone fields
022000050524      *
022100050524      *     nrr        - Relative record number for subfile
022200050524      *     SizeList   - Size of list
022300050524      *     ReturnSize - Size of list returned by sort API
022400050524      **********************************************************************
022500050524     D NotUsed         S             16A
022600050524     D ReturnSize      S              9B 0
022700050524     D SizeList        S              9B 0
022800050524     D RrnLast         S              5i 0
022900050524     D WrkSort         S              1
023000050524     D Vrsm1w          S                   LIKE(Vrsm1)
023100050524      **********************************************************************
023200050524      * Data Structures
023300050524      *     SflRcd     - The entire subfile record to pass to the sort
023400050524      *     QLGSCB     - The sort request block for the QLGSORT API
023500050524      *     QLGSCB00   - The sort request block for the QLGSRTIO API
023600050524      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
023700050524      *     QUSEC      - Error structure for the QLGSORT API
023800050524      **********************************************************************
023900050524     D SflRcd          DS
024000050524     d VSEL
024100050524     d VLNP1
024200050524     d VFGC1
024300050524     d VAGC1
024400050524     d VNGC1
024500050524     d VMGC1
024600050524     d VCMC1
024700050524     d VDIS1
024800050524     d VAAS1
024900050524     d VNRS1
025000050524     d VNSP1
025100050524     D VRSM1
025200050524     d Vmsg1
025300050524     D VRSd1
025400080603     D Vddts
025500090819     D  h1in52
025600050524     D Selected                       1A
025700050524
025800050524      /COPY QSYSINC/QRPGLESRC,QLGSORT
025900050524     D QLGKL                         16    DIM(MaxKey)
026000050524     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
026100050524     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
026200050524     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
026300050524     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
026400050524
026500050524      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
026600050524      /COPY QSYSINC/QRPGLESRC,QUSEC
026700050524      *---------------------------------------------------------------------
026800900418     C*
026900911010     C     *INKC         DOWEQ     '0'
027000940926     C     *IN28         IFEQ      '0'
027100940926     C                   Z-ADD     0             VFGC
027200940926     C                   Z-ADD     ANNO          VAGC
027300940926     C                   Z-ADD     0             VNGC
027400950215     C     WSCE          IFNE      'S'
027500940926     C                   Z-ADD     0             VLNP
027600940926     C                   Z-ADD     ANNO          VAAS
027700940926     C                   Z-ADD     0             VNRS
027800940926     C                   Z-ADD     0             VNSP
027900950215     C                   ELSE
028000950215     C                   MOVEL     '2'           VPAR
028100950215     C                   END
028200940926     C                   MOVEL     *BLANK        VDIS
028300940926     C                   Z-ADD     GMAUDT        VDED
028400940926     C                   ENDIF
028500940926     C     D82GES        IFEQ      'A'
028600940926      *ARRIVO
028700051006     c                   if        vpar = 4
028800051006     c                   clear                   vdis
028900051006     c                   end
029000950215     C                   MOVEL     'N'           WSCE              1
029100080424      *
029200940919     C                   EXFMT     FG82D3
029300080424      *
029400940923     C* Se anno a 2 lo porto a  4
029500940923     C     VNGC          IFNE      *ZEROS
029600940923     C                   MOVEL     VAGC          WCOM2             2 0
029700940923     C     WCOM2         IFEQ      0
029800940923     C     VAGC          IFGT      60
029900940923     C                   ADD       1900          VAGC
030000940923     C                   ELSE
030100940923     C                   ADD       2000          VAGC
030200940923     C                   END
030300940923     C                   END
030400940923     C                   ENDIF
030500940923     C     VNSP          IFNE      *ZEROS
030600940923     C                   MOVEL     VAAS          WCOM2             2 0
030700940923     C     WCOM2         IFEQ      0
030800940923     C     VAAS          IFGT      60
030900940923     C                   ADD       1900          VAAS
031000940923     C                   ELSE
031100940923     C                   ADD       2000          VAAS
031200940923     C                   END
031300940923     C                   END
031400940923     C                   ENDIF
031500940920      * INTERROGAZ. BOLLE ARRIVI
031600950215     C     *INKG         IFEQ      '1'
031700161118     C*                  Z-ADD     VAAS          PA1AAS
031800161118     C*                  Z-ADD     VLNP          PA1LNP
031900161118     C*                  Z-ADD     VNRS          PA1NRS
032000161118     C*                  Z-ADD     VNSP          PA1NSP
032100161118     C*                  MOVEL     ' '           PA1F03
032200161118     C*                  MOVEL     '2'           PA1FLG
032300161118     C*    PA1NSP        IFGT      0
032400161118     C*                  MOVEL     '1'           PA1FLG
032500161118     C*                  END
032600161118     C*                  MOVEL     PARAM1        KPJBU
032700161118     C*                  CALL      'FNLR36R'
032800161118     C*                  PARM                    KPJBA
032900161118     C*                  MOVEL     KPJBU         PARAM1
033000161118     C*    PA1F03        IFNE      '1'
033100161118     C*                  MOVEL     'S'           WSCE
033200161118     C*                  Z-ADD     PA1AAS        VAAS
033300161118     C*                  Z-ADD     PA1LNP        VLNP
033400161118     C*                  Z-ADD     PA1NRS        VNRS
033500161118     C*                  Z-ADD     PA1NSP        VNSP
033600161118     C*                  END
033700161118     C     Vnsp          IFGT      0
033800161118     C                   CLEAR                   paramu6ds1
033900161118     C                   Z-ADD     Vaas          pa1AAS_
034000161118     C                   Z-ADD     Vlnp          Pa1LNP_
034100161118     C                   Z-ADD     Vnrs          Pa1NRS_
034200161118     C                   Z-ADD     Vnsp          Pa1NSP_
034300161118     c                   eval      pa1tla_='L'
034400161118     C                   MOVEL     paramu6ds1    KPJBU
034500161118     C                   CALL      'FNLRU6R1'
034600161118     C                   PARM                    KPJBA
034700161118     C*
034800161118     C                   ELSE
034900161118     C*
035000161118     C                   CLEAR                   PARAmu6ds
035100161118     c                   clear                   fnlru6ds
035200161118     C                   MOVEL     '2'           Pa1FLG
035300161118     C                   MOVEL     PARAmu6ds     KPJBU
035400161118     c                   eval      ilru6lna=§fle
035500161118     C                   CALL      'FNLRU6R'
035600161118     C                   PARM                    KPJBA
035700161118     c                   parm                    fnlru6ds
035800161118     C                   MOVEL     KPJBU         PARAMu6ds
035900161118     C     Pa1F03        IFEQ      *BLANKS
036000161118     C                   Z-ADD     Pa1AAS        VAAS
036100161118     C                   Z-ADD     Pa1LNP        VLNP
036200161118     C                   Z-ADD     Pa1NRS        VNRS
036300161118     C                   Z-ADD     Pa1NSP        VNSP
036400161118     C                   MOVEL     'S'           WSCE
036500161118     C                   ENDIF
036600161118     C                   ENDIF
036700950215     C                   END
036800950215     C*
036900940926     C                   ELSE
037000080424      *
037100080424     c     ritorno_D2    tag
037200080424      *?  Controlla se si tratta di NTW DPD...  ?
037300080424     c                   exsr      Check_se_DPD
037400080424      *
037500940926      *PARTENZA
037600940919     C                   EXFMT     FG82D2
037700080424      *
037800940926     C* Se anno a 2 lo porto a  4
037900940926     C     VNGC          IFNE      *ZEROS
038000940926     C                   MOVEL     VAGC          WCOM2             2 0
038100940926     C     WCOM2         IFEQ      0
038200940926     C     VAGC          IFGT      60
038300940926     C                   ADD       1900          VAGC
038400940926     C                   ELSE
038500940926     C                   ADD       2000          VAGC
038600940926     C                   END
038700940926     C                   END
038800940926     C                   ENDIF
038900940920      * INTERROGAZ. BOLLE PARTENZE
039000950215     C     *INKG         IFEQ      '1'
039100161118     C***                CLEAR                   PARAM0
039200161118     C**                 MOVEL     PARAM0        KPJBU
039300161118     C*                  CALL      'FNLS04R'
039400161118     C*                  PARM                    KPJBA
039500161118     C*                  MOVEL     KPJBU         PARAM0
039600161118     C                   CLEAR                   PARAmu6ds
039700161118     c                   clear                   fnlru6ds
039800161118     C                   MOVEL     '4'           Pa1FLG
039900161118     C                   MOVEL     PARAmu6ds     KPJBU
040000161118     c                   eval      ilru6lp1=§fle
040100161118     C                   CALL      'FNLRU6R'
040200161118     C                   PARM                    KPJBA
040300161118     c                   parm                    fnlru6ds
040400950215     C                   END
040500950215     C*
040600940926     C                   ENDIF
040700940920      * CAMBIO FILIALE
040800080424     C   KS              Do
040900080424     C                   EXSR      CMBFIL
041000080522     C                   IF        d82ges <> 'A'
041100080424     c                   goto      ritorno_D2
041200080522     c                   endif
041300080424     c                   endDo
041400940920      * INTERROGAZ. GIACENZE
041500941223     C   KN              DO
041600941223     C                   CLEAR                   FNLG30
041700941223     C                   MOVEL     D82GES        D30POA
041800941223     C                   MOVEL     FNLG30        KPJBU
041900950103     C     D82GES        IFEQ      'A'
042000941228     C                   CALL      'FNLG31C'
042100920427     C                   PARM                    KPJBA
042200950103     C                   ELSE
042300950103     C                   CALL      'FNLG30C'
042400950103     C                   PARM                    KPJBA
042500941223     C                   END
042600950103     C                   END
042700940919      * CONTROLLI VIDEATA INIZIALE
042800940919     C  NKC
042900940919     CANNKG
043000940919     CANNKN              EXSR      CTRVID
043100940919      *
043200940920     C  N21
043300940920     CANNKS
043400940920     CANNKN
043500940919     CANNKC
043600940919     CANN28
043700940919     CANNKG              DO
043800911023     C                   Z-ADD     0             FRG
043900940920      *
044000940919      * PARTENZA
044100940919      *---------
044200940920      * ----> IMMESSO NR.GIACENZA
044300940920     C     D82GES        IFEQ      'P'
044400940919     C     VPAR          ANDEQ     1
044500080424      *
044600080424      * Oppure se immesso il Parcel -> ha tradotto la giacenza
044700080424     C     d82ges        orEQ      'P'
044800080424     C     Vpar          andEQ     6
044900080424      *
045000050215     C     KGCA1         CHAIN     tigcp02L                           48
045100940920     C     *IN48         IFEQ      '0'
045200940919      * CONTROLLO SE GIACENZA IN GESTIONE
045300060804     c*mm                if        kcdaz = 'XG83'
045400060804     c*mm                seton                                        89
045500060804     c*mm                else
045600940919     C     §FLE          COMP      GCPLNP                                 89
045700940919     C  N89              MOVE      GCPLNP        LNP§              3
045800940922     C  N89LNP§          LOOKUP    F£6                                    89
045900060804     c*mm                endif
046000940928     C   89              MOVE      GCPAGC        D82AGC
046100940928     C   89              MOVE      GCPFGC        D82FGC
046200940928     C   89              MOVE      GCPNGC        D82NGC
046300940922     C   89              MOVEL     *BLANKS       D82DIS
046400941215     C   89              MOVEL     §FLE          D82FGS
046500050316     C   89              MOVE      'S'           D82comm
046600050726     c   89              clear                   d82ntaut
046700050726     c   89              clear                   d82nodsp
046800940920     C   89              MOVEL     FNLG82        KPJBU
046900940920     C   89              CALL      'FNLG10R'
047000911030     C                   PARM                    KPJBA
047100080424      *
047200080424      * Di ritorno dal chiamante deve ripulire sia l'opz.che il Parcel
047300080424     c                   if        vPar = 6 and Vparcel <> ' '
047400080424     c                   eval      pulisce_DPD = 'S'
047500080424     c                   end
047600080424      *
047700940920     C  N89              SETON                                        4828
047800940920     C   48              MOVEL     MSG(1)        $MSG
047900940919     C                   ELSE
048000940919     C                   SETON                                        4828
048100940919     C                   MOVEL     MSG(8)        $MSG
048200940920     C                   ENDIF
048300940920     C                   ENDIF
048400940919      * ARRIVO :
048500940919      *---------
048600940920     C     D82GES        IFEQ      'A'
048700940920      * ----> IMMESSO NR. GIACENZA
048800940920     C     VPAR          IFEQ      1
048900050630      * calcolo numero riaperture
049000050630     c                   exsr      srconta
049100050630     c*
049200050215     C     KGCA1         CHAIN     tigcp02L                           48
049300941012     C   48              MOVEL     MSG(8)        $MSG
049400941012     C   48              SETON                                        28
049500031029      *
049600940920     C     *IN48         IFEQ      '0'
049700940919      * CONTROLLO SE GIACENZA IN GESTIONE
049800940919     C     VFGC          COMP      §FLE                                   89
049900940919     C  N89              MOVE      VFGC          LNP§
050000940922     C  N89LNP§          LOOKUP    F£6                                    89
050100940920     C  N89              SETON                                        4828
050200940920     C   48              MOVEL     MSG(1)        $MSG
050300031029      *
050400940920      * IMMISS.DISPOSIZ.
050500940920     C  N28VSEL1         IFEQ      'X'
050600031029      *
050700040705      * per non immettere disposizioni oltre le riaperture consentite + 1
050800040705     c                   z-add     0             limite            1 0
050900040705     c                   z-add     1             xy                3 0
051000050215     c     karb          chain     fnarb01l
051100040705     c                   if        arbgga <> *blank
051200040705     c     arbgga        lookup    c7q(xy)                                55
051300040705     c                   else
051400040705     c     'GR'          lookup    c7q(xy)                                55
051500040705     c                   endif
051600040705     c   55              movel     d7q(xy)       ds7q
051700040705     c                   eval      limite = §7q5rp + 1
051800040705     c                   if        savFRG > limite
051900031022     C                   SETON                                        4828
052000031022     C                   MOVEL     MSG(17)       $MSG
052100031022     c                   end
052200031029      *
052300050215     C  n28GCPFAS        IFEQ      20
052400940928     C                   MOVE      GCPAGC        D82AGC
052500940928     C                   MOVE      GCPFGC        D82FGC
052600940928     C                   MOVE      GCPNGC        D82NGC
052700940922     C                   MOVEL     *BLANKS       D82DIS
052800941215     C                   MOVEL     §FLE          D82FGS
052900050316     C                   MOVE      'S'           D82comm
053000050726     c                   clear                   d82nodsp
053100050726     c                   clear                   d82ntaut
053200940920     C                   MOVEL     FNLG82        KPJBU
053300940920     C                   CALL      'FNLG10R'
053400911030     C                   PARM                    KPJBA
053500911121     C                   ELSE
053600940920     C                   SETON                                        4828
053700940920     C                   MOVEL     MSG(9)        $MSG
053800911023     C                   END
053900940920     C                   ENDIF
054000031029      *
054100940920      * ESECUZ. DISPOSIZ.
054200940920     C  N28VSEL2         IFEQ      'X'
054300050215     C     GCPFAS        IFEQ      30
054400050215     C     GCPFAS        OREQ      31
054500050215     C     GCPFAS        OREQ      36
054600941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
054700941012     C     GCPDED        IFGT      AMGUDT
054800941012     C                   SETON                                        4828
054900941012     C                   MOVEL     MSG(15)       $MSG
055000941012     C                   ELSE
055100040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
055200040112     c                   CLEAR                   ds7w
055300050215     c     karb          chain     fnarb01l
055400040112     c                   if        arbgga <>*blank
055500040112     c                   move      '7W'          cod
055600040112     c                   movel     *blank        key
055700040112     c                   movel     arbgga        key
055800040112     c     ktab          chain     tabel00f
055900040112     c                   if        %found(tabel00f)
056000040112     c                   movel     tbluni        ds7w
056100040112     c                   endif
056200040112     c                   endif
056300031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
056400040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
056500040112     c                             gcpdis = §7w7t3
056600031217     c                   movel     arblnp        fldlnpa
056700031217     c                   movel     arbaas        fldaasa
056800031217     c                   movel     arbnrs        fldnrsa
056900031217     c                   movel     arbnsp        fldnspa
057000040112     c                   select
057100040112     c                   when      gcpdis = §7w7t1
057200040112     c                   movel     §7w7ca1       fldcca
057300040112     c                   when      gcpdis = §7w7t2
057400040112     c                   movel     §7w7ca2       fldcca
057500040112     c                   when      gcpdis = §7w7t3
057600040112     c                   movel     §7w7ca3       fldcca
057700040112     c                   endsl
057800031217     c                   movel     arbgga        fldgga
057900031217     c                   movel(p)  param80       kpjbu
058000031217     c                   call      'FNLG80R1'
058100031219     c                   parm                    kpjba
058200031217     c                   else
058300940928     C                   MOVE      GCPAGC        D82AGC
058400940928     C                   MOVE      GCPFGC        D82FGC
058500940928     C                   MOVE      GCPNGC        D82NGC
058600940922     C                   MOVEL     *BLANKS       D82DIS
058700941215     C                   MOVEL     §FLE          D82FGS
058800050316     C                   MOVE      'S'           D82comm
058900050726     c                   clear                   d82nodsp
059000050726     c                   clear                   d82ntaut
059100940920     C                   MOVEL     FNLG82        KPJBU
059200940920     C                   CALL      'FNLG20R'
059300911030     C                   PARM                    KPJBA
059400031217     C                   Endif
059500941012     C                   END
059600911121     C                   ELSE
059700940920     C                   SETON                                        4828
059800940920     C                   MOVEL     MSG(11)       $MSG
059900911023     C                   END
060000940920     C                   ENDIF
060100940919     C                   ELSE
060200940920     C                   SETON                                        4828
060300940919     C                   MOVEL     MSG(8)        $MSG
060400940920     C                   ENDIF
060500940920     C                   ENDIF
060600940919      *
060700940919      *         ----> IMMESSO NR. SPEDIZIONE
060800940920     C     VPAR          IFEQ      2
060900050630      * calcolo numero riaperture
061000050630     c                   exsr      srconta
061100031029      *
061200050215     C     KGCA2         CHAIN     tigcp21L                           46
061300031029      *
061400941012     C   46              MOVEL     MSG(8)        $MSG
061500941012     C   46              SETON                                        28
061600031029      *
061700940920     C     *IN46         IFEQ      '0'
061800940920     C     §FLE          COMP      GCPFGC                                 89
061900050215     C  N89              MOVE      GCPLNAa       LNP§
062000940922     C  N89LNP§          LOOKUP    F£6                                    89
062100940920     C  N89              SETON                                        4628
062200940920     C   46              MOVEL     MSG(1)        $MSG
062300940920      * IMMISS. DISPOSIZ.
062400940920     C  N28VSEL1         IFEQ      'X'
062500031029      *
062600040705      * per non immettere disposizioni oltre le riaperture consentite + 1
062700040705     c                   z-add     0             limite
062800040705     c                   z-add     1             xy
062900050215     c     karb          chain     fnarb01l
063000040705     c                   if        arbgga <> *blank
063100040705     c     arbgga        lookup    c7q(xy)                                55
063200040705     c                   else
063300040705     c     'GR'          lookup    c7q(xy)                                55
063400040705     c                   endif
063500040705     c   55              movel     d7q(xy)       ds7q
063600040705     c                   eval      limite = §7q5rp + 1
063700040705     c                   if        savFRG > limite
063800031029     C                   SETON                                        4628
063900031029     C                   MOVEL     MSG(17)       $MSG
064000031029     c                   end
064100031029      *
064200050215     C  n28GCPFAS        IFEQ      20
064300940928     C                   MOVE      GCPAGC        D82AGC
064400940928     C                   MOVE      GCPFGC        D82FGC
064500940928     C                   MOVE      GCPNGC        D82NGC
064600940922     C                   MOVEL     *BLANKS       D82DIS
064700941215     C                   MOVEL     §FLE          D82FGS
064800050316     C                   MOVE      'S'           D82comm
064900050726     c                   clear                   d82nodsp
065000050726     c                   clear                   d82ntaut
065100940920     C                   MOVEL     FNLG82        KPJBU
065200940920     C                   CALL      'FNLG10R'
065300911030     C                   PARM                    KPJBA
065400911105     C                   ELSE
065500940920     C                   SETON                                        4628
065600940919     C                   MOVEL     MSG(9)        $MSG
065700940920     C                   END
065800940920     C                   ENDIF
065900940920      * ESECUZ. DISPOSIZ.
066000940920     C  N28VSEL2         IFEQ      'X'
066100050215     C     GCPFAS        IFEQ      30
066200050215     C     GCPFAS        OREQ      31
066300050215     C     GCPFAS        OREQ      36
066400941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
066500941012     C     GCPDED        IFGT      AMGUDT
066600941012     C                   SETON                                        4828
066700941012     C                   MOVEL     MSG(15)       $MSG
066800941012     C                   ELSE
066900031217      * verifico la bolla
067000040112     c                   CLEAR                   ds7w
067100050215     c     karb          chain     fnarb01l
067200040112     c                   if        arbgga <>*blank
067300040112     c                   move      '7W'          cod
067400040112     c                   movel     *blank        key
067500040112     c                   movel     arbgga        key
067600040112     c     ktab          chain     tabel00f
067700040112     c                   if        %found(tabel00f)
067800040112     c                   movel     tbluni        ds7w
067900040112     c                   endif
068000040112     c                   endif
068100031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
068200040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
068300040112     c                             gcpdis = §7w7t3
068400031217     c                   movel     arblnp        fldlnpa
068500031217     c                   movel     arbaas        fldaasa
068600031217     c                   movel     arbnrs        fldnrsa
068700031217     c                   movel     arbnsp        fldnspa
068800040112     c                   select
068900040112     c                   when      gcpdis = §7w7t1
069000040112     c                   movel     §7w7ca1       fldcca
069100040112     c                   when      gcpdis = §7w7t2
069200040112     c                   movel     §7w7ca2       fldcca
069300040112     c                   when      gcpdis = §7w7t3
069400040112     c                   movel     §7w7ca3       fldcca
069500040112     c                   endsl
069600031217     c                   movel     arbgga        fldgga
069700031217     c                   movel(p)  param80       kpjbu
069800031217     c                   call      'FNLG80R1'
069900031219     c                   parm                    kpjba
070000031217     c                   else
070100940928     C                   MOVE      GCPAGC        D82AGC
070200940928     C                   MOVE      GCPFGC        D82FGC
070300940928     C                   MOVE      GCPNGC        D82NGC
070400940922     C                   MOVEL     *BLANKS       D82DIS
070500941215     C                   MOVEL     §FLE          D82FGS
070600050316     C                   MOVE      'S'           D82comm
070700050726     c                   clear                   d82nodsp
070800050726     c                   clear                   d82ntaut
070900940920     C                   MOVEL     FNLG82        KPJBU
071000940920     C                   CALL      'FNLG20R'
071100911030     C                   PARM                    KPJBA
071200031217     C                   Endif
071300941012     C                   END
071400911105     C                   ELSE
071500940920     C                   SETON                                        4628
071600940920     C                   MOVEL     MSG(11)       $MSG
071700940920     C                   END
071800940920     C                   ENDIF
071900941012     C                   ENDIF
072000940920     C                   ENDIF
072100940920     C                   ENDIF
072200940920      *
072300940919      * NESSUN SCELTA SPECIFICA, CARICO SFL
072400940919      *
072500940919     C  N28D82GES        IFEQ      'P'
072600940919     C     VPAR          ANDNE     1
072700080424     C     vpar          andNE     6
072800080424      *
072900940919     C     D82GES        OREQ      'A'
073000940922     C     VPAR          ANDEQ     3
073100080424      *
073200010920     C     D82GES        OREQ      'A'
073300010920     C     VPAR          ANDEQ     4
073400080424      *
073500940919     C                   EXSR      CARICA
073600080424      *
073700940919     C     NRR           IFEQ      0
073800940919     C                   SETON                                        28
073900940920     C                   MOVEL     MSG(10)       $MSG
074000911030     C                   ELSE
074100050512     c     riemetti      tag
074200911105     C                   WRITE     FG82D4
074300911105     C                   EXFMT     FG82CT
074400911105     C                   SETOFF                                       30
074500950207     C                   MOVEL     'N'           WSEL              1
074600050525      * ordina per mittente o nr. giacenza
074700050524     c                   if        *inkk
074800050525     c                   setoff                                       32
074900050525      * scelta ordinamento
075000050525     c                   select
075100050525     c                   when      wrksort = giacenza or wrksort = *blank
075200050525     c                   eval      wrksort = mittente
075300050525     c                   seton                                        07
075400050525     c                   when      wrksort = mittente
075500050525     c                   eval      wrksort = giacenza
075600050525     c                   setoff                                       07
075700050525     c                   endsl
075800050525      *
075900050613     c                   eval      rrnlast = wnrr
076000050524     c                   exsr      sort
076100050524     c                   goto      riemetti
076200050524     c                   end
076300911105     C*
076400050527     c                   if        not *inkc and not *inkl and not *inkf
076500051005     c                             and not *inka and not *inkg
076600050527     c                   goto      riemetti
076700050527     c                   endif
076800050527     c  NKL
076900050527     CANNKC*IN30         DOWEQ     '0'
077000911105     C                   READC     FG82SF                                 30
077100050513      *verifico se scelta opzione 7 non per i resi
077200050513     c                   if        vsel = '7' and (vpar <> 3 and vpar <> 5)
077300050517     c                             or vsel = '7' and d82ges = 'A'
077400050517     c                   goto      riemetti
077500050513     c                   endif
077600050811      * periodo resi bloccati
077700050811     c                   if        amgudt >= §2gdpi and amgudt <= §2gdpf
077800050811     c                             and d82ges = 'P' and (vpar = 3 or vpar = 5 )
077900050811     c                   goto      riemetti
078000050811     c                   else
078100050811      * cmd1 spalma opzione 1 su tutte le righe
078200050811     c                   if        *inka and *in30
078300050525     c                   setoff                                       32
078400050512     c                   do        wnrr          nrr
078500050613     c     nrr           chain     fg82sf                             27
078600050613     c   27              leave
078700050512     c                   move      '1'           vsel
078800050512     c                   seton                                        72
078900090819     c                   eval        *in52 = H1IN52
079000050512     c                   update    fg82sf
079100050512     c                   enddo
079200050512     c                   goto      riemetti
079300050512     c                   endif
079400050512      *
079500050525      * cmd7 spalma opzione 7 su tutte le righe
079600050811     c                   if        *inkg and *in30
079700050525     c                   setoff                                       32
079800050512     c                   do        wnrr          nrr
079900050613     c     nrr           chain     fg82sf                             27
080000050613     c   27              leave
080100050512     c                   move      '7'           vsel
080200050512     c                   seton                                        72
080300090819     c                   eval        *in52 = H1IN52
080400050512     c                   update    fg82sf
080500050512     c                   enddo
080600050512     c                   goto      riemetti
080700050512     c                   endif
080800050811      *resi bloccati
080900050811     c                   endif
081000050527     C
081100050513     c                   clear                   d82nodsp
081200050527     c                   clear                   d82ntaut
081300050527     C   KF
081400050527     CANN30VSEL          IFEQ      '1'
081500050513     C     VSEL          oreq      '7'
081600941007     C                   MOVE      VAGC1         D82AGC
081700941007     C                   MOVE      VFGC1         D82FGC
081800941007     C                   MOVE      VNGC1         D82NGC
081900941222     C                   MOVEL     VDIS1         D82DIS
082000941215     C                   MOVEL     §FLE          D82FGS
082100050316     C                   MOVE      'S'           D82comm
082200050513      *forza parametro per non emettere videate pgm chiamanti con opz=7
082300050513     c                   if        vsel = '7'
082400050513     c                   move      'X'           d82nodsp
082500050726     c                   move      'S'           d82ntaut
082600050513     c                   endif
082700941221     C                   MOVEL     FNLG82        KPJBU
082800940922     C     D82GES        IFEQ      'A'
082900940922     C     VSEL1         IFEQ      'X'
083000950207     C                   MOVEL     'S'           WSEL              1
083100940920     C                   CALL      'FNLG10R'
083200911128     C                   PARM                    KPJBA
083300911128     C                   ELSE
083400950207     C                   MOVEL     'S'           WSEL              1
083500040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
083600040112     c                   CLEAR                   ds7w
083700031217     c     kblp2         chain     fnarb01l
083800040112     c                   if        arbgga <>*blank
083900040112     c                   move      '7W'          cod
084000040112     c                   movel     *blank        key
084100040112     c                   movel     arbgga        key
084200040112     c     ktab          chain     tabel00f
084300040112     c                   if        %found(tabel00f)
084400040112     c                   movel     tbluni        ds7w
084500040112     c                   endif
084600040112     c                   endif
084700040112      * se disposizione di reso e sulla bolla c'è la particolarità GS
084800040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
084900040112     c                             gcpdis = §7w7t3
085000031217     c                   movel     arblnp        fldlnpa
085100031217     c                   movel     arbaas        fldaasa
085200031217     c                   movel     arbnrs        fldnrsa
085300031217     c                   movel     arbnsp        fldnspa
085400040112     c                   select
085500040112     c                   when      gcpdis = §7w7t1
085600040112     c                   movel     §7w7ca1       fldcca
085700040112     c                   when      gcpdis = §7w7t2
085800040112     c                   movel     §7w7ca2       fldcca
085900040112     c                   when      gcpdis = §7w7t3
086000040112     c                   movel     §7w7ca3       fldcca
086100040112     c                   endsl
086200031217     c                   movel     arbgga        fldgga
086300031217     c                   movel(p)  param80       kpjbu
086400031217     c                   call      'FNLG80R1'
086500031219     c                   parm                    kpjba
086600031217     c                   else
086700051006     c                   if        vpar <> 4
086800940920     C                   CALL      'FNLG20R'
086900911128     C                   PARM                    KPJBA
087000051006     C                   Endif
087100031217     C                   Endif
087200051005     c* Appuntamento
087300051005     c                   if        vpar = 4
087400051005     C                   CALL      'FNLG88R'
087500051005     C                   PARM                    KPJBA
087600051005     c                   end
087700940922     C                   END
087800911128     C                   ELSE
087900051005     c* PARTENZA
088000950207     C                   MOVEL     'S'           WSEL              1
088100940920     C                   CALL      'FNLG10R'
088200911105     C                   PARM                    KPJBA
088300940922     C                   END
088400940922     C                   END
088500911105     C                   Z-ADD     0             VFGC
088600911105     C                   Z-ADD     0             VAGC
088700911105     C                   Z-ADD     0             VNGC
088800940922     C                   END
088900950207     C*
089000950207     C     WSEL          IFEQ      'S'
089100950207     C                   EXSR      CARICA
089200950207     C                   END
089300950207     C*
089400950207     C                   END
089500940919     C                   ENDIF
089600940922     C                   END
089700911023     C                   END
089800940919      *----------------------------------------------------
089900020306     C                   CLEAR                   TRUL27
090000020306     C                   MOVEL     'C'           I27TLA
090100160302     C                   CALL      'TRUL27R1'
090200020306     C                   PARM                    TRUL27
090300160302     c
090400160302     C                   CLEAR                   TRULC7DS
090500160302     C                   MOVEL     'C'           IC7TLA
090600160302     C                   CALL      'TRULC7R'
090700160302     C                   PARM                    TRULC7DS
090800160302     c
090900160302     C                   CLEAR                   fnlv59ds
091000160302     C                   MOVEL     'C'           Ilv59tla
091100160302     C                   CALL      'FNLV59R'
091200160302     C                   PARM                    ilv59tla
091300161118     c*                  eval      pa1tla_='C'
091400161118     c*                  eval      kpjbu=paramu6ds1
091500161118     c*                  call      'FNLRU6R1'
091600161118     c*                  parm                    kpjba
091700900418     C                   SETON                                        LR
091800080424     C*---------------------------------------------------------------------*
091900080424     c*  Controlla la filiale su AZorg per controllare il NTW
092000080424     C*---------------------------------------------------------------------*
092100080424     c     Check_se_DPD  begsr
092200080424      *
092300080424      *
092400080424      * se DPD deve dare la possibilità di inserire direttamente il Parcel
092500080424     c                   eval      Network_DPD ='N'
092600080424     c     §FLE          chain     azorg01l
092700080424     c                   if        %Found(Azorg01l)
092800080424     c                   movel     orgDE3        Og143
092900140108     c                   if        §OGNTW = 'DPD' or
093000140123     c                             %subst(knmus:1:3) = 'INT'
093100080424     c                   eval      Network_DPD ='S'
093200080424     c                   end
093300080424     c                   end
093400080424      *
093500080424      * per 84 = *on Protegge l'inserimento del Nr. Parcel a video Opz.6
093600080424      *
093700080424     c     Network_DPD   comp      'N'                                    84
093800080424      *
093900080424     c                   if        Network_DPD = 'N' or
094000080424     c                             pulisce_DPD = 'S'
094100080424     c                   clear                   vParcel
094200080424     c                   if        vPar = 6
094300080424     c                   clear                   vPar
094400080424     c                   end
094500080424     c                   end
094600080424      *
094700080424     c                   eval      pulisce_DPD = 'N'
094800080424      *
094900080424     c                   endsr
095000080424     C*---------------------------------------------------------------------*
095100940923     C*--------------------*
095200050630     C* Conta numero riaperture
095300940923     C*--------------------*
095400050630     C     srconta       BEGSR
095500050630     c*
095600050630     c                   clear                   fnlg09ds
095700050630     c                   eval      d09tla = 'R'
095800050630     c                   select
095900050630     C     VPAR          wheneq    1
096000050630     c                   eval      d09AGC = VAGC
096100050630     c                   eval      d09FGC = VFGC
096200050630     c                   eval      d09NGC = VNGC
096300050630     C     VPAR          wheneq    2
096400050630     c                   eval      d09aas = VAAS
096500050630     c                   eval      d09lnp = VLNP
096600050630     c                   eval      d09nrs = VNRS
096700050630     c                   eval      d09nsp = VNSP
096800050630     c                   endsl
096900050630     c                   movel(p)  fnlg09ds      kpjbu
097000050630     C                   CALL      'FNLG09R'
097100050630     C                   PARM                    kpjba
097200050630     c                   movel(p)  kpjbu         fnlg09ds
097300050630     c     d09cnt        sub       d09ced        savfrg
097400050630     c*
097500050630     c                   endsr
097600050630     C*--------------------*
097700050630     C* CAMBIO FILIALE
097800050630     C*--------------------*
097900050630     C     CMBFIL        BEGSR
098000940926     C                   SETOFF                                       28
098100940926     C                   MOVEL     *BLANK        $MSG
098200940923     C                   MOVE      §FLE          VD1FLE
098300940923     C     *IN28         DOUEQ     '0'
098400940923     C                   EXFMT     FG82D1
098500940926     C                   SETOFF                                       28
098600940923     C   KL              SETOFF                                       28
098700940923     C  NKL              DO
098800940923     C                   MOVE      VD1FLE        LNP§              3
098900940923     C     LNP§          LOOKUP    F£1                                    31
099000940923     C     *IN31         IFEQ      '0'
099100940923     C                   SETON                                        28
099200940923     C                   MOVEL     MSG(12)       $MSG
099300940923     C                   ELSE
099400940923      * CARICO FILIALI IN GESTIONE
099500940923     C                   MOVE      VD1FLE        §FLE
099600940923     C                   MOVEL     *BLANKS       KEY
099700940923     C                   MOVEL     §FLE          KEY
099800940923     C                   MOVEA     *ZEROS        F£6
099900940923     C     D82GES        IFEQ      'A'
100000020503     c                   clear                   trul06ds
100100020503     c                   eval      d06cod = '£6'
100200020503     c                   movel     §fle          d06key
100300020503     c                   movel     trul06ds      kpjbu
100400020503     c                   call      'TRUL06R'
100500020503     c                   parm                    kpjba
100600020503     c                   movel     kpjbu         trul06ds
100700020503     c                   movea     lin           f£6
100800020503     C*                  MOVE      '£6'          COD
100900940923     C                   ELSE
101000140123     c                   clear                   trovata           1
101100940923     C                   MOVE      '£7'          COD
101200140102      * forzo caricamento filiali dpd per profilo mirato
101300140123     c                   if        %subst(knmus:1:3) = 'INT'
101400140123     c                   movel(p)  §mussei       key
101500140123     C     KTAB          CHAIN     TABEL                              14
101600140123     C  N14              MOVEA     TBLUNI        F£6
101700140123     c  n14              move      'X'           trovata
101800140123     c   14              movel(p)  §fle          key
101900140102     c                   endif
102000140102      *
102100140123     c                   if        trovata = *blank
102200940923     C     KTAB          CHAIN     TABEL                              15
102300940923     C  N15              MOVEA     TBLUNI        F£6
102400940923     C   15              MOVE      §FLE          F£6(1)
102500140123     c                   endif
102600020503     C                   ENDIF
102700020503      *
102800940923     C                   ENDIF
102900940923     C                   ENDDO
103000940923     C                   ENDDO
103100940923     C                   ENDSR
103200911017     C*--------------------*
103300911017     C* CARICAMENTO SUBFILE
103400911017     C*--------------------*
103500911010     C     CARICA        BEGSR
103600051006     c* per appuntamento forzo la disposizione = consegna
103700051006     c                   if        vpar = 4 and d82ges = 'A'
103800051006     c                   eval      vdis = 'C'
103900051006     c                   end
104000051006     c*
104100911017     C                   EXSR      PULIZ
104200940922     C* MI POSIZIONO X OGNI FILIALE IN GESTIONE
104300940922     C                   DO        29            I                 2 0
104400940922     C                   MOVE      F£6(I)        FLE
104500940922     C     FLE           IFGT      *ZEROS
104600940919      *  POSIZIONAMENTO
104700940922     C     D82GES        IFEQ      'P'
104800950208      * SE GESTISCO CARICAMENTO X CLIENTE CARICO SOLO
104900950208      * SE CLIENTE E' DELLA FILIALE CHE STO ESAMINANDO
105000940922      * PARTENZA
105100940922     C     VPAR          IFEQ      2
105200950208     C                   MOVEL     SCM           WFILC             3 0
105300050215     C     kSCM          SETLL     tigcp07L                           20
105400940919     C                   ELSE
105500050324     C     kflefas       SETLL     tigcp06L                           20
105600940919     C                   ENDIF
105700940922     C                   ELSE
105800940922      * ARRIVO PER DISPOSIZIONE
105900050215     C     KGCA3         SETLL     tigcp24L                           20
106000940922     C                   ENDIF
106100940919      *  LETTURA
106200940922     C     *IN20         DOWEQ     '0'
106300940922     C     D82GES        IFEQ      'P'
106400940922      * PARTENZA
106500940922     C     VPAR          IFEQ      2
106600050215     C     kSCM          READE     tigcp07L                             1620
106700050215     c                   move      gcplnp        gcplnpx           3
106800050215     c     gcplnpx       lookup    f£6                                    35
106900020821     c  n35              iter
107000940919     C                   ELSE
107100050215     C     FLE           READE     tigcp06L                             1620
107200050324      * forzata uscita dal loop per non leggere rekord inutili e ottimizzare
107300050324      * i tempi di risposta
107400050324     c                   if        gcpfas >= 30
107500050324     c                   leave
107600050324     c                   endif
107700050324
107800940919     C                   ENDIF
107900940919      *
108000940919     C                   ELSE
108100940922      * ARRIVO  X DISPOSIZIONE
108200050215     C     KGCA3         READE     tigcp24L                             1620
108300940922     C                   ENDIF
108400940922     C  N20
108500940922     CANN16              DO
108600051005     c                   movel     gcpflr        dgcpflr
108700940919     C* SE SIAMO IN ARR. ED ESECUZIONE CONSIDERO LE GIACENZE CON
108800940919     C* FASE = 31 O 32 E CON DATA ESEGUIBILITA DISPOSIZ. <= VDED
108900940919     C     D82GES        IFEQ      'A'
109000050215     C     GCPFAS        IFEQ      30
109100911030     C     GCPFRG        ANDEQ     0
109200050215     C     GCPFAS        OREQ      31
109300911023     C     GCPFRG        ANDEQ     0
109400050215     C     GCPFAS        OREQ      36
109500050215     C     GCPFRG        ANDEQ     0
109600050408     C     GCPDED        IFLE      AMGDED
109700950216     C     GCPDED        ANDNE     0
109800950216     C     GCPDDM        ORLE      AMGDED
109900050408     C     GCPDED        ANDEQ     0
110000051005     C     D§GCPAPP      ANDne     'A'
110100051006     C     vpar          ANDne     4
110200051006     c* appuntamento
110300051006     C     GCPDDM        ORnE      0
110400051006     C     GCPDED        ANDEQ     0
110500051006     C     D§GCPAPP      ANDeq     'A'
110600051006     C     vpar          ANDeq     4
110700911023     C                   EXSR      WTR
110800940922     C                   END
110900940922     C                   END
111000940922     C                   ENDIF
111100941007     C* SE SIAMO IN PART. CONSIDERO LE GIACENZE CON FASE >= 20 E <=30
111200990628     C* DATA DISPOSIZIONE MITTENTE = 0   VEDI V.L. FIGCP09L
111300940922     C     D82GES        IFEQ      'P'
111400020821      * SOLO QUELLE DI UN CLIENTE e 35 on linea di partenza in gestione
111500940922     C     VPAR          IFEQ      2
111600950208     C     GCPFAS        IFEQ      20
111700950208     C     GCPDDM        ANDEQ     0
111800960606     C     GCPFRG        ANDEQ     0
111900950327     C     GCPFAS        OREQ      25
112000950327     C     GCPDDM        ANDEQ     0
112100960606     C     GCPFRG        ANDEQ     0
112200950327     C     GCPFAS        OREQ      26
112300950327     C     GCPDDM        ANDEQ     0
112400960606     C     GCPFRG        ANDEQ     0
112500911023     C                   EXSR      WTR
112600950208     C                   END
112700940922     C                   ELSE
112800940922      * TUTTE
112900911023     C                   EXSR      WTR
113000940922     C                   ENDIF
113100940919     C                   ENDIF
113200940922     C                   END
113300940922     C                   END
113400940922     C                   ENDIF
113500940922     C                   ENDDO
113600911010     C                   ENDSR
113700911017     C*-----------------------------*
113800911017     C* SCRITTURA RECORD NEL SUBFILE
113900911017     C*-----------------------------*
114000911010     C     WTR           BEGSR
114100990702     C*
114200990702     C                   SETOFF                                       52
114300950118     C                   MOVEL     *BLANKS       VMSG1
114400911023     C                   Z-ADD     GCPFGC        VFGC1
114500911023     C                   Z-ADD     GCPAGC        VAGC1
114600911023     C                   Z-ADD     GCPNGC        VNGC1
114700920110     C                   MOVEL     GIOR          VMGC1
114800920110     C                   MOVE      MESE          VMGC1
114900911023     C                   MOVEL     GCPCMC        VCMC1
115000920318     C* PRELEVO MITT. E DESTINATARIO DALLA BOLLA IN PARTENZA
115100940922     C     D82GES        IFEQ      'P'
115200050215     C                   Z-ADD     GCPAAS        VAAS1
115300050215     C                   Z-ADD     GCPNRS        VNRS1
115400050215     C                   Z-ADD     GCPNSP        VNSP1
115500050215     C                   Z-ADD     GCPLNP        VLNP1
115600940920     C     KBLP          CHAIN     FNBLP01L                           71
115700941019     C  N71              MOVEL     BLPAAS        AMGDSP            8 0
115800941019     C  N71              MOVE      BLPMGS        AMGDSP            8 0
115900940919     C  N71              MOVEL     BLPRSM        VRSM1
116000080603     c  n71              exsr      vediddt
116100960123     C*  CONTROLLO SE ESISTE DESTINATARIO FINALE
116200960123     C     *IN71         IFEQ      '0'
116300960123     C                   CLEAR                   FNLSN0
116400960123     C                   Z-ADD     BLPAAS        DN0AAS
116500960123     C                   Z-ADD     BLPLNP        DN0LNP
116600960123     C                   Z-ADD     BLPNRS        DN0NRS
116700960123     C                   Z-ADD     BLPNSP        DN0NSP
116800960123     C                   MOVEL     'R'           DN0TRC
116900960123     C                   MOVEL     KPJBU         WSVKPJ          256
117000960123     C                   MOVEL     FNLSN0        KPJBU
117100960123     C                   CALL      'FNLSN0R'
117200960123     C                   PARM                    KPJBA
117300960123     C                   MOVEL     KPJBU         FNLSN0
117400960123     C                   MOVEL     WSVKPJ        KPJBU
117500960123     C     DN0TRV        IFEQ      'S'
117600960123     C                   MOVEL     DN0RSD        VRSD1
117700960123     C                   ELSE
117800960123     C                   MOVEL     BLPRSD        VRSD1
117900960123     C                   END
118000960123     C                   END
118100940922     C* SE SIAMO IN PARTENZA CALCOLO CON I GIORNI DI RIENTRO SE LA
118200940922     C* SPEDIZIONE DEVE RIENTRARE
118300940922     C                   EXSR      RIENTR
118400960209      * GESTIONE PARTICOLARITA GIACENZE
118500960209      *                        LA VISUALIZZO SEMPRE
118600950102     C                   SETOFF                                       32
118700031022      * forza "GR" nella particolarità per poter gestire
118800031022      * il reso automaticamente
118900031024      * e si memorizza se non c'era sul file
119000031024     c                   move      '*'           c_eraGGA          1
119100031022     C                   if        BLPGGA = *blank
119200031024     c                   clear                   c_eraGGA
119300031022     c                   eval      BLPGGA = 'GR'
119400031022     c                   end
119500940926     C     BLPGGA        IFNE      *BLANK
119600940926     C                   Z-ADD     1             X
119700940926     C     BLPGGA        LOOKUP    C7Q(X)                                 32
119800940926     C     *IN32         IFEQ      '1'
119900960209     C                   MOVEL     BLPGGA        VMSG1
120000031023      *  Non deve visualizzare "GR" se non è in fase 26
120100031023      *  avendo sostituito il blank con il GR per poter
120200031023      *  decodificare la tabella.
120300031023     c                   if        vmsg1 = 'GR' and gcpFAS <> 26
120400031023     C                   clear                   VMSG1
120500031024     c                   setoff                                       32
120600031023     c                   end
120700031024     c                   if        vmsg1 ='GR' and c_eraGGA = *blank
120800031024     C                   clear                   VMSG1
120900031024     c                   end
121000940926     C                   MOVE      D7Q(X)        DS7Q
121100940926     C                   END
121200940926     C                   ENDIF
121300940926      * DISPOSIZ. RICEVUTE DAL CLIENTE :SONO NELLE NOTE
121400940922     C     GCPFAS        IFEQ      25
121500940926     C                   MOVEL     '*'           VDIS1
121600940922     C                   END
121700940926      * SE FASE =26 HO RICEVUTO UNA RIAPERTURA "FINTA" X IL
121800940926      * CODICE PARTIC. 5A =NON RIAPRIRE MA RENDERE ...
121900940926     C     GCPFAS        IFEQ      26
122000940926     C                   MOVEL     §7Q5DI        VDIS1
122100950104     C                   SETON                                        32
122200940926     C                   END
122300940922     C                   ELSE
122400961220     C*  CONTROLLO SE CI SONO NOTE GIACENZA PER FAX
122500961220     C                   MOVEL     *BLANKS       VDIS1
122600050215     C                   Z-ADD     GCPAASa       VAAS1
122700050215     C                   Z-ADD     GCPNRSa       VNRS1
122800050215     C                   Z-ADD     GCPNSPa       VNSP1
122900050215     C                   Z-ADD     GCPLNPa       VLNP1
123000961220     C                   Z-ADD     GCPAGC        KAGC
123100961220     C                   Z-ADD     GCPFGC        KFGC
123200961220     C                   Z-ADD     GCPNGC        KNGC
123300961220     C                   Z-ADD     GCPFRG        KFRG
123400961220     C                   Z-ADD     35            KFAS
123500050215     C     KGNA          CHAIN     tignp01L                           36
123600961220     C  N36              MOVEL     'F'           VDIS1
123700941007      *    A R R I V O
123800050215     C     Karb          CHAIN     FNARB01L                           71
123900940919     C  N71              MOVEL     ARBRSM        VRSM1
124000940919     C  N71              MOVEL     ARBRSD        VRSD1
124100080603     c  n71              exsr      vediddt
124200940926      * GESTIONE PARTICOLARITA GIACENZE
124300950102     C                   SETOFF                                       32
124400940926     C     ARBGGA        IFNE      *BLANK
124500940926     C                   Z-ADD     1             X
124600940926     C     ARBGGA        LOOKUP    C7Q(X)                                 32
124700940926     C     *IN32         IFEQ      '1'
124800940926     C                   MOVE      D7Q(X)        DS7Q
124900960209     C                   MOVEL     ARBGGA        VMSG1
125000941007      *
125100940926     C                   END
125200940926     C                   ENDIF
125300940922     C                   ENDIF
125400940919     C   71              MOVEL     *ALL'?'       VRSM1
125500940919     C   71              MOVEL     *ALL'?'       VRSD1
125600010920      *
125700010920      * FILTRI SOLO IN PARTENZA
125800050512 1   C     D82GES        IFEQ      'P'
125900090819      *
126000050512 2   C     VPAR          IFNE      3
126100010920     C     VDIS1         OREQ      'R'
126200090819      *
126300050512 3   C     VPAR          IFEQ      4
126400010920     C     VDIS1         ANDEQ     '*'
126500090819      *
126600050512mm   C     VPAR          orEQ      3
126700050512mm   C     gcpfas        ANDEQ     20
126800090819      *
126900050512mm   C     VPAR          orEQ      5
127000050512mm   C     gcpfas        ANDEQ     26
127100090819      *
127200010920     C     VPAR          ORNE      4
127300050512mm   C     VPAR          andne     3
127400050512mm   C     VPAR          andne     5
127500090819      *
127600090819      *   Attenzione  SOLO per la scelta "2" x Specifico CLIENTE:
127700090819      **     se si tratta di disposizioni di RESO e siamo
127800090819      **    nel periodo delle trazioni ridotte, deve essere
127900090819      **    protetto il campo di scelta a meno che non ci sia l'eccezione
128000090819      **    sul cliente nella tab.7U. (il cliente vuole i resi anche nel
128100090819      **    periodo delle trazioni ridotte)
128200090819     C                   IF          VDIS1 = 'R' and Vpar = 2
128300090819     c                                and
128400090819     c                               AMGUDT >= §2GDPI and AMGUDT <= §2GDPF
128500090819      *
128600090819      * Protegge x default il campo scelta
128700090819     C                   SETON                                        52
128800090819      *
128900090819      * Controlla se il cliente vuole comunque il reso
129000090819     C     BLPksc        lookup    k7u                                    88
129100090819      * se non l'ho trovato con KSC provo con CCM
129200090819     c  n88BLPccm        ifne      *zeros
129300090819     C     BLPccm        lookup    k7u                                    88
129400090819     c                   endif
129500090819      * Se l'ha trovato sprotegge la scelta per eseguire il reso    ======
129600090819     C   88              SETOFF                                       52
129700090819      *                                                             ======
129800090819     C                   END
129900090819      *
130000090819      * Memorizza se la scelta deve essere protetta o no
130100090819     c                   eval      H1IN52 = *in52
130200090819      *
130300950224     C                   ADD       1             NRR
130400050512     C                   WRITE     FG82SF
130500050512 3   C                   END
130600050512 2   C                   END
130700090819      *
130800050512 x1  C                   ELSE
130900090819      * IN ARRIVO
131000090819      * Memorizza se la scelta deve essere protetta o no
131100090819     c                   eval      H1IN52 = *in52
131200090819      *
131300950224     C                   ADD       1             NRR
131400950224     C                   WRITE     FG82SF
131500050512 1   C                   END
131600090819      *
131700040922      * forzato limite caricamento a 9000 rekord
131800040922     c                   if        nrr > 9000
131900040922     c                   seton                                        20
132000040922     c                   endif
132100950224     C*
132200050512     c                   z-add     nrr           wnrr              4 0
132300900509     C                   ENDSR
132400911017     C*----------------*
132500911010     C* PULIZIA SUBFILE
132600911017     C*----------------*
132700911010     C     PULIZ         BEGSR
132800911010     C                   SETON                                        70
132900911023     C                   WRITE     FG82CT
133000911023     C                   Z-ADD     0             NRR               4 0
133100911010     C                   SETOFF                                       70
133200911010     C                   EXSR      PULVID
133300911010     C                   ENDSR
133400911010     C*
133500940922     C*----------------*
133600940922     C* CALCOLO SE SPEDIZ. DA RIENTRARE
133700940922     C*----------------*
133800940922     C     RIENTR        BEGSR
133900940922     C** RICERCO SU TARIFFA I GG DI RIENTRO DEL CLIENTE
134000940922     C                   Z-ADD     0             WGGR
134100940922     C                   Z-ADD     GCPSCM        APPO              4 0
134200950116     C                   MOVE      GCPSCM        KKSC
134300950116     C                   MOVE      BLPCTR        KCTR
134400950116     C                   MOVEL     ' '           WTROVA            1
134500020227     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" E TARIFFA DI
134600020227     C* CARTELLO
134700160302     C                   CLEAR                   fnlv59ds
134800020227     C                   CLEAR                   TRUL27
134900021118     C                   MOVE      BLPctr        I27ctb
135000020227     C                   MOVEL     BLPTSP        I27TSP
135100020227     C                   MOVE      BLPLNA        I27LNA
135200020227     C                   MOVE      BLPLNP        I27LNP
135300020227     C                   MOVE      KKSC          I27CLI
135400160302     C                   CALL      'TRUL27R1'
135500020227     C                   PARM                    TRUL27
135600020227     C*
135700950116     C                   Z-ADD     1             Y                 3 0
135800940922     C     APPO          IFNE      8888
135900940922     C     APPO          ANDNE     9999
136000950116     C     APPO          ANDNE     *ZEROS
136100950116     C*  Cerco progressivo tariffa in vigore su
136200150514     C                   MOVEL     ' '           ilv59TBO
136300150514     C                   Z-ADD     AMGDSP        ilv59DSP
136400150514     C                   MOVE      BLPCTR        ilv59CTR
136500150514     C                   MOVE      KKSC          ilv59KSC
136600150514     C                   MOVE      KCI           ilv59KCI
136700000202     C                   MOVEL     KKSC          W0030             3 0
136800150514     C                   MOVEL     O27FIE        ilv59FIE
136900150514     C                   MOVEL     BLPTSP        ilv59TSP
137000150514     C                   MOVEL     'S'           ilv59CON
137100150514     c                   eval      ilv59ta2='B'
137200150514     c                   eval      ilv59blo='S'
137300150514     C                   CALL      'FNLV59R'
137400150514     C                   PARM                    fnlv59ds
137500950116     C*
137600150514     C     olv59ERR      IFEQ      *BLANKS
137700150514     C     olv59tks      ANDEQ     'S'
137800950116     C*  Se ho trovato tariffa in vigore mi posiziono sul file
137900950116     C*  delle tariffe giacenze con il proggressivo giusto
138000150514     C                   MOVE      olv59PRG      KPRG
138100150514     C                   MOVE      ilv59KSC      KKSC
138200150514     C                   MOVE      olv59CTR      KCTR
138300990730     C     KTGC          CHAIN     TITGC01L                           31
138400950116     C     *IN31         IFEQ      '0'
138500950116     C     TGCGGR        ANDNE     *ZEROS
138600950116     C                   MOVE      TGCGGR        WGGR
138700950116     C                   MOVEL     'S'           WTROVA
138800950116     C                   END
138900950116     C                   END
139000950116     C                   END
139100950116     C*  Se non trovato gg. rientro su tariffa cliente li cerco
139200950116     C*  sulla tariffa di cartello
139300950116     C     WTROVA        IFNE      'S'
139400160302     c* Carico la cartello :
139500160302     c*  - per 8888 col fie della bolla
139600160302     c*  - per tariffe cliente esistente col fie della tariffa
139700160302     c                   clear                   trulc7ds
139800160302    2C     olv59ERR      ifeq      *blanks
139900160302     C     olv59tks      andeq     'S'
140000160302     c                   z-add     1             indx              3 0
140100160302     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
140200160302    3c                   if        indx>0 and %editc(olv59prg:'X')=
140300160302     c                             ks_pr(indx)
140400160302     c                   eval      ic7tfie=ks_ie(indx)
140500160302   x3c                   else
140600160302     c                   eval      ic7tntw=o27ntw
140700160302    3c                   endif
140800160302     c
140900160302   x2c                   else
141000160302     c*
141100160302     c                   eval      ic7tntw=o27ntw
141200160302    2c                   endif
141300160302     c
141400160302     C                   move      blpctr        Ic7ctb
141500160302     C                   MOVE      amgdsp        Ic7DTA
141600160302     c                   call      'TRULC7R'
141700160302     c                   parm                    trulc7ds
141800160302     c
141900160302     C                   MOVE      Oc7KSCc       KKSC
142000160302     C                   MOVE      Oc7CTRc       KCTR
142100160302     C                   MOVE      Oc7PRGc       KPRG
142200990730     C     KTGC          CHAIN     TITGC01L                           31
142300950116     C     *IN31         IFEQ      '0'
142400950116     C     TGCGGR        ANDNE     *ZEROS
142500950116     C                   MOVE      TGCGGR        WGGR
142600950116     C                   MOVEL     'S'           WTROVA
142700950116     C                   END
142800950116     C                   END
142900970619     C*  Se non ho impostato i gg rientro li prendo dalla tabella 2G
143000950116     C     WTROVA        IFNE      'S'
143100950116     C                   MOVE      §2GGGL        WGGR
143200950116     C                   END
143300020227     C* Per Estero prendo minore fra gg.rientro estero e tariffa
143400020227     C* Occorre però distinguere fra Estero, DPD, E FEDEX in quanto c'è
143500020227     C* tabella "2G" CON i LORO gg.
143600020227     C     O27FIE        IFNE      'I'
143700020227     C                   SELECT
143800020227      * Se è da DPD
143900050210     C     o27ntw        WHENEQ    'DPD'
144000010921     C     WGGR          IFGT      §2GGDP
144100010921     C                   MOVE      §2GGDP        WGGR
144200010921     C                   END
144300020227      * Se è FEDEX
144400050210     C     o27ntw        WHENEQ    'FED'
144500020227     C     WGGR          IFGT      §2GGFE
144600020227     C                   MOVE      §2GGFE        WGGR
144700020227     C                   END
144800160302      * Se è euroexpress
144900050210     C     o27ntw        WHENEQ    'EEX'
145000050210     C     o27ntw        oreq      'EUP'
145100961213     C     WGGR          IFGT      §2GGRE
145200961204     C                   MOVE      §2GGRE        WGGR
145300010921     C                   END
145400020227     C                   ENDSL
145500961213     C                   END
145600040217      *
145700050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
145800050210     C*   sui giorni da prendere
145900050210     C     o27fie        ifne      'I'
146000050210     C     o27ntw        andeq     'EEX'
146100050210     C                   MOVEL     '20'          COD
146200050210     C                   MOVEL     *blank        KEY
146300050210     C                   MOVEL     Kksc          KEY
146400050210     c     ktab          chain     tabel00f
146500050210     c                   if        %found(tabel00f)
146600050210     c                   movel     tbluni        ds20
146700050210     C                   Z-ADD     §20GRE        wGGR
146800050210     c                   endif
146900050210     c                   end
147000940922      *
147100940922     C* CALCOLO SE LA DATA E' INFERIORE ALLA DATA ODIERNA E QUINDI LA
147200940922     C* MERCE SAREBBE DA RIENTRARE
147300050215     C                   MOVE      GCPDUR        WDAT8             8 0
147400950622     C                   MOVE      WDAT8         G08INV
147500950622     C                   MOVE      '3'           G08ERR
147600950622     C                   CALL      'XSRDA8'
147700950622     C                   PARM                    WLBDA8
147800950622     C     G08TGI        ADD       WGGR          GI8TGI
147900950622     C                   CALL      'XSRGI8'
148000950622     C                   PARM                    WGIDA8
148100020228     c* data + gg rientro è < di oggi
148200950622     C                   Z-ADD     GI8INV        WDAT8
148300940922     C     WDAT8         IFLT      AMGUDT
148400940926     C                   MOVEL     'R'           VDIS1
148500990702     C* Se sono nel periodo di esclusione dei rientri automatici
148600990702     C* proteggo il record del subfile per la selezione
148700990729     C     WDAT8         IFGE      §2GDPI
148800990729     C     WDAT8         ANDLE     §2GDPF
148900990729     C     AMGUDT        ANDGE     §2GDPI
149000990729     C     AMGUDT        ANDLE     §2GDPF
149100990702     C     D82GES        ANDEQ     'P'
149200990729     C* OPPURE
149300990729     C     BLPGGA        OREQ      'G0'
149400990729     C     AMGUDT        ANDGE     §2GDPI
149500990729     C     AMGUDT        ANDLE     §2GDPF
149600990729     C     D82GES        ANDEQ     'P'
149700020108      *
149800020108      *  NON DEVE PROTEGGERE SE SI È ENTRATI SCEGLIENDO
149900020108      *   L'OPZIONE (4)
150000020108     C     VPAR          IFNE      4
150100990702     C                   SETON                                        52
150200020108     C                   END
150300020108      *
150400990702     C                   END
150500940922     C                   ELSE
150600940926     C                   MOVEL     *BLANKS       VDIS1
150700940922     C                   ENDIF
150800940922     C                   ENDSR
150900911017     C*--------------------*
151000911017     C* PULIZIA CAMPI VIDEO
151100911017     C*--------------------*
151200911017     C     PULVID        BEGSR
151300050512     C                   MOVEL     *BLANKS       VSEL
151400911023     C                   MOVE      *ZEROS        VFGC1
151500911023     C                   MOVE      *ZEROS        VAGC1
151600911023     C                   MOVE      *ZEROS        VMGC1
151700911023     C                   MOVE      *ZEROS        VNGC1
151800911023     C                   MOVE      *BLANKS       VCMC1
151900911023     C                   MOVE      *ZEROS        VAAS1
152000911023     C                   MOVE      *ZEROS        VNRS1
152100911023     C                   MOVE      *ZEROS        VNSP1
152200911023     C                   MOVE      *BLANKS       VRSM1
152300911023     C                   MOVE      *BLANKS       VRSD1
152400940926     C                   MOVE      *BLANKS       VDIS1
152500940926     C                   MOVE      *BLANKS       VMSG1
152600911017     C                   ENDSR
152700911022     C*----------------------*
152800911022     C* CONTROLLI 1° VIDEATA
152900911022     C*----------------------*
153000911022     C     CTRVID        BEGSR
153100080424      *
153200940926     C                   SETOFF                                       434445
153300940926     C                   SETOFF                                       464748
153400940926     C                   SETOFF                                       495051
153500080424     C                   SETOFF                                       280654
153600080424      *
153700050513      *accendo indicatore condizionamento opzione 7 vale solo per i resi
153800050513     c                   if        vpar = 3 or vpar = 5
153900050513     c                   seton                                        06
154000050513     c                   endif
154100080424      *
154200080424      * controlla se inserita opzione (6 - DPD Parcel) quando non è
154300080424      *  abilitato a farlo : errore
154400080424      *    Il parcel è solo utilizzabile in Partenza x P.O.DPD
154500080424     c                   if        vpar = 6
154600080424     c                   if         D82GES = 'A'  or
154700080424     c                             (D82GES = 'P' and *in84)
154800080424     C                   SETON                                        2843
154900080424     C                   MOVEL     MSG(19)       $MSG
155000080424     c                   endif
155100080424     c                   endif
155200080424      *
155300911022     C* IN PARTENZA
155400940919     C     D82GES        IFEQ      'P'
155500940919      *
155600911022     C     VPAR          IFEQ      2
155700911022     C* RICERCA CLIENTE
155800911022     C                   MOVEA     VSCM          KSC§
155900940920     C     '?'           LOOKUP    KSC§                                   21
156000940920     C   21              DO
156100911023     C                   Z-ADD     CODUT         CODUT§            1 0
156200911023     C                   Z-ADD     KCI           KCI§              4 0
156300911030     C                   MOVEL     *BLANKS       RICRAG           48
156400911022     C                   MOVEL     VRAG          RICRAG
156500911022     C                   Z-ADD     0             RICKSC            7 0
156600911022     C                   Z-ADD     0             PARSTA            1 0
156700911022     C                   CALL      'XALFA1'
156800911022     C                   PARM                    PARDUT
156900911022     C                   PARM                    CODUT§
157000911022     C                   PARM                    RICRAG
157100911022     C                   PARM                    KCI§
157200911022     C                   PARM                    RICKSC
157300911022     C                   PARM                    PARSTA
157400961016     C                   PARM                    PARFLR           90
157500911022     C     PARSTA        IFNE      -1
157600911030     C                   MOVE      RICKSC        VSCM
157700911030     C                   MOVEL     RICRAG        VRAG
157800911022     C                   ELSE
157900911220     C                   MOVE      *ZEROS        VSCM
158000911030     C                   MOVE      *BLANKS       VRAG
158100911022     C                   END
158200911022     C                   END
158300911022     C* CONTROLLO ESISTENZA CLIENTE
158400911030     C                   MOVE      VSCM          SCM
158500940926     C     KACO          CHAIN     CNACO000                           46
158600940926     C   46              MOVEL     MSG(2)        $MSG
158700940926     C   46              SETON                                        28
158800940919     C                   ENDIF
158900080424      *
159000940926      * SE IMMESSO NR.GIAC. O COD.CLIENTE OBBLIGO   VPAR
159100940926     C     VPAR          IFEQ      *ZEROS
159200940926     C     VNGC          IFNE      *ZEROS
159300940926     C     VSCM          ORNE      *BLANK
159400080424     C     Vparcel       orNE      *Blank
159500940926     C                   SETON                                        2843
159600940926     C                   MOVEL     MSG(4)        $MSG
159700940926     C                   ENDIF
159800940926     C                   ENDIF
159900080424      *
160000080424      * Se scelto il Parcel Nr.
160100080424     c                   if        Vpar = 6
160200080424      *  controlla esistenza del Parcel con il quale agganciare la giacenza
160300080424     C                   if        Vparcel = *blank
160400080424     C                   SETON                                        2854
160500080424     C                   MOVEL     MSG(20)       $MSG
160600080424     c                   else
160700080424      *
160800080424      * Controlla esistenza del Parcel immesso incorciando i dati
160900171109      * del FIPND con le giacenze - usando il numero spedizione
161000171109      * come chiave di Join:  (sostituito il vecchio FIAR4 "I")
161100080424     C/exec sql
161200080424     c+ Select gcpagc, gcpfgc, gcpngc into :vagc, :vfgc, :vngc
161300171109     c+   from Fipnd00f join tigcp00f
161400171109     c+   on  pndaas = gcpaas and pndlnp = gcplnp and
161500171109     c+       pndnrs = gcpnrs and pndnsp = gcpnsp
161600171113     c+   where pndIPN = :vParcel and pndDGS = 0
161700080424     c+     and GCPfrg = 0
161800080424     C/end-exec
161900080424     c                   if        SqlCOD < 0 or sqlCOD = 100
162000080424     C                   SETON                                        2854
162100080424     C                   MOVEL     MSG(20)       $MSG
162200080424     c                   else
162300080424      * se qui : ha trovato la giacenza con il Parcel
162400080424      *          e ha caricato la chiave della Giacenza
162500080424     c                   end
162600080424      *
162700080424     c                   endif
162800080424     c                   endif
162900080424      *
163000940919     C                   ENDIF
163100911022     C* IN ARRIVO
163200940919     C     D82GES        IFEQ      'A'
163300911022     C     VSEL1         IFEQ      *BLANKS
163400911022     C     VSEL2         ANDEQ     *BLANKS
163500940926     C                   SETON                                        444528
163600940919     C                   MOVEL     MSG(3)        $MSG
163700911022     C                   END
163800940919     C  N28VPAR          IFEQ      *ZEROS
163900010920     C     VPAR          ORGT      4
164000940926     C                   SETON                                        4328
164100940919     C                   MOVEL     MSG(4)        $MSG
164200911022     C                   END
164300940926     C  N28              DO
164400940926     C     VSEL1         COMP      'X'                                    50
164500940926     C     VSEL2         COMP      'X'                                    51
164600940926     C   50
164700940926     CAN 51              SETON                                        444528
164800941012     C   28              MOVEL     MSG(5)        $MSG
164900940926     C                   ENDDO
165000940919     C  N28
165100940919     CAN 50VPAR          IFEQ      3
165200940926     C                   SETON                                        4428
165300940926     C                   MOVEL     MSG(6)        $MSG
165400911022     C                   END
165500051006     C  N28
165600051006     CAN 50VPAR          IFEQ      4
165700051006     C                   SETON                                        4428
165800051006     C                   MOVEL     MSG(18)       $MSG
165900051006     C                   END
166000940919     C  N28VPAR          IFEQ      1
166100911022     C     VFGC          IFEQ      0
166200911022     C     VNGC          OREQ      0
166300940926     C                   SETON                                        4828
166400940926     C                   MOVEL     MSG(7)        $MSG
166500911022     C                   END
166600940926     C                   END
166700940919     C  N28VPAR          IFEQ      2
166800911022     C     VLNP          IFEQ      0
166900911022     C     VNSP          OREQ      0
167000940926     C                   SETON                                        4628
167100940926     C                   MOVEL     MSG(7)        $MSG
167200911022     C                   END
167300911022     C                   END
167400940919     C  N28VPAR          IFEQ      3
167500940919     C     VDIS          ANDEQ     *BLANKS
167600940926     C                   SETON                                        4728
167700940926     C                   MOVEL     MSG(7)        $MSG
167800911022     C                   END
167900940926      * CONTROLLO DATA ESEGUIBILTA DISP.
168000940926     C  N28VPAR          IFEQ      3
168100940926     C                   Z-ADD     VDED          G08DAT
168200940926     C                   MOVE      *ZEROS        G08INV
168300940926     C                   MOVEL     *BLANK        G08ERR
168400940926     C                   CALL      'XSRDA8'
168500940926     C                   PARM                    WLBDA8
168600940926     C     G08ERR        IFEQ      '1'
168700940926     C                   SETON                                        2849
168800940926     C                   MOVEL     MSG(13)       $MSG
168900940926     C                   ELSE
169000940926     C                   Z-ADD     G08INV        AMGDED            8 0
169100940926     C                   ENDIF
169200051006     c                   else
169300051006     c                   clear                   amgded
169400940926     C                   ENDIF
169500940919     C                   ENDIF
169600080424      *
169700911022     C                   ENDSR
169800050524      **********************************************************************
169900050524      * Subroutine - Sort
170000050524      *   This subroutine sorts the subfile records.
170100050524     C     Sort          BEGSR
170200050524     C                   CLEAR                   vrsm1
170300050524     C                   CLEAR                   vrsm1w
170400050613     C                   EVAL      nrr = 0
170500050524
170600050524      * Initialize the key fields to sort on. There is one extra field not in
170700050524      * the subfile -- Selected -- that is added to the record. This field
170800050524      * is used to place selected records in front of nonselected records.
170900050524
171000050524     C                   CLEAR                   QLGSCB
171100050524     C                   CLEAR                   QLGSCB00
171200050524
171300050524     ** Gestione della scelta ordinamento.
171400050524     C                   SELECT
171500050525     ** Ordinamento per mittente
171600050525     C                   WHEN      WrkSort = mittente
171700050524     ** 1 campo chiave.
171800050524     C                   EVAL      QLGNBRK    = 1
171900050525     ** il mittente è
172000050524     ** a posizone 42, 13 byte, char, ascending.
172100050525     C                   EVAL      QLGSP      = 40
172200050524     C                   EVAL      QLGSS      = %SIZE(vrsm1)
172300050524     C                   EVAL      QLGDT      = Carattere
172400050524     C                   EVAL      QLGSO      = Ascendente
172500050524     C                   EVAL      QLGKL(1)   = QLGSKL
172600050525     ** Al prossimo F11 ordino per giacenza
172700050525     C*                  EVAL      WrkSort = giacenza
172800050524     ** Ordinamento per Codice
172900050525     C                   WHEN      WrkSort = giacenza
173000050524     C                   EVAL      QLGNBRK    = 1
173100050524     ** Il codice è
173200050525     ** a posizione 12, 7 byte, ascending
173300050525     C                   EVAL      QLGSP      = 12
173400050525     C                   EVAL      QLGSS      = %SIZE(vngc1)
173500050524     C                   EVAL      QLGDT      = Carattere
173600050524     C                   EVAL      QLGSO      = Ascendente
173700050524     C                   EVAL      QLGKL(1)   = QLGSKL
173800050525     ** Al prossimo F11 ordino per mittente
173900050525     C*                  EVAL      WrkSort = Mittente
174000050524     C                   ENDSL
174100050524      * Load other sort parameters.
174200050524     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
174300050524     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
174400050524     C                   EVAL      QLGRT     = 8
174500050524     C                   EVAL      QLGOKL    = 80
174600050524     C                   EVAL      QLGLKE    = 16
174700050524     C                   EVAL      QLGLSS    = 290
174800050524      * Initialize Sort I/O API fields.
174900050524     C                   EVAL      QLGRL00  = QLGRL
175000050524     C                   EVAL      QLGRC00  = 1
175100050524     C                   CLEAR                   QUSEI
175200050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
175300050524      * First step - Initialize the sort routine.
175400050524     C                   CALL      'QLGSORT'
175500050524     C                   PARM                    QLGSCB
175600050524     C                   PARM                    NotUsed
175700050524     C                   PARM                    NotUsed
175800050524     C                   PARM                    SizeList
175900050524     C                   PARM                    ReturnSize
176000050524     C                   PARM                    QUSEC
176100050524      * Next step - Write records to I/O routine.
176200050524     C                   EVAL      QLGRT00 = Put
176300050613     C                   DO        RrnLast       nrr
176400050613     C     nrr           CHAIN     fg82sf
176500050524     ** Solo le righe con Selected = 'Y' sono riordinate,
176600050524     ** quindi per fare un ordinamento di tutte le righe
176700050524     ** metto 'Y' sempre.
176800050524     C                   EVAL      Selected = 'Y'
176900050524     C                   CLEAR                   QUSEI
177000050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
177100050524     C                   CALL      'QLGSRTIO'
177200050524     C                   PARM                    QLGSCB00
177300050524     C                   PARM                    SflRcd
177400050524     C                   PARM                    NotUsed
177500050524     C                   PARM                    SizeList
177600050524     C                   PARM                    NotUsed
177700050524     C                   PARM                    QUSEC
177800050524     C                   ENDDO
177900050524      * Next step - Signal end of input, clear subfile for reload.
178000050524     C                   EVAL      QLGRT00 = EndPut
178100050524     C                   CLEAR                   QUSEI
178200050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
178300050524
178400050524     C                   CALL      'QLGSRTIO'
178500050524     C                   PARM                    QLGSCB00
178600050524     C                   PARM                    SflRcd
178700050524     C                   PARM                    NotUsed
178800050524     C                   PARM                    SizeList
178900050524     C                   PARM                    NotUsed
179000050524     C                   PARM                    QUSEC
179100050524     c                   seton                                        70
179200050524     C                   WRITE     fg82ct
179300050524     C                   setoff                                       70
179400050524      * Final step - Write the records back to the subfile.
179500050524     C                   EVAL      QLGRT00 = Get
179600050613     C                   DO        RrnLast       nrr
179700050524     C                   CLEAR                   QUSEI
179800050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
179900050524     C                   CALL      'QLGSRTIO'
180000050524     C                   PARM                    QLGSCB00
180100050524     C                   PARM                    NotUsed
180200050524     C                   PARM                    SflRcd
180300050524     C                   PARM                    QLGRL00
180400050524     C                   PARM                    NotUsed
180500050524     C                   PARM                    QUSEC
180600050525     c                   if        vmsg1 <> *blank
180700050525     c                   seton                                        32
180800050525     c                   else
180900050525     c                   setoff                                       32
181000050525     c                   endif
181100090819     c                   eval        *in52 = H1IN52
181200050524     C                   WRITE     fg82sf
181300050524     C                   ENDDO
181400050524     C                   ENDSR
181500080603     c*-----------------------------------------------------------------
181600080603     c     vediddt       begsr
181700080603     c*-----------------------------------------------------------------
181800080603      * Flag DDT SI  ar4 trek A
181900080603     c                   clear                   vddts
182000080603     c                   clear                   dsbl4a
182100080603     c                   move      'A'           trek4
182200080603     c     kar4          chain     fiar401l
182300080603     c                   if        %found(fiar401l)
182400080603     c                   movel     AR4NOT        dsbl4a
182500080603     c                   if        §B4ABM = 'S' or §B4ABM = 'P' or
182600080603     c                             §B4ABM = 'C' or §B4ABM = 'Q'
182700080603     c                   eval      vddts = 'S'
182800080603     C                   endif
182900080603     C                   endif
183000080603     c                   endsr
183100050524     C*-----------------------------------------------------------------
183200940919     C*   I N Z S R
183300050524     C*-----------------------------------------------------------------
183400940919     C     *INZSR        BEGSR
183500940919     C     *ENTRY        PLIST
183600940919     C                   PARM                    KPJBA
183700940919      *
183800940919     C                   MOVEL     KPJBU         FNLG82
183900140123     C                   MOVEL     Knmus         knmus2
184000940920      *
184100940919     C                   MOVEL     NOMPGM        VPGM
184200940919     C                   Z-ADD     1             CODUT
184300940919     C                   CALL      'X§PARUT'
184400940919     C                   PARM                    UTEDSE
184500940919     C                   MOVEL     RAGUT         RSUT             20
184600940919     C                   MOVEL     RAGUT         PARDUT           30
184700060925     C**mm               MOVEL     REC80         CNCR80
184800140123     C                   MOVEL     KNMUS         §SEI
184900020228     c* una 2° livello
185000020730     C**** §PRF          COMP      'REM'                                  01
185100020730     C**N01              Z-ADD     SIMFEL        §FLE
185200060804     C**MM               Z-ADD     SIMpou        §FLE
185300940919      *
185400060804     c     *dtaara       define    §azute        azuteds
185500060804     c     *dtaara       define    §datiute      ddatiute
185600060804     c                   in(E)     *dtaara
185700060804    1c                   If        %error  or RSUT = *blanks
185800060804     c                   Clear                   Tibs34ds
185900060804     c                   Call      'TIBS34R'
186000060804     c                   Parm                    Tibs34ds
186100060804     c                   In        *dtaara
186200060804    1c                   EndIf
186300060804
186400060804     c                   Movel     UteFaf        Dute01
186500060804     c                   If        §Utefgia = *Blanks and kcdaz = 'XG83'
186600150911     c                   call      'XUTENOAB'
186700150911     c                   parm                    kpjba
186800060804     c                   seton                                        lr
186900060804     c                   Endif
187000060925     c                   z-add     dutpou        §fle
187100060925     c* TEST SE SONO IN PARTENZA O IN ARRIVO
187200940919     C     D82GES        IFEQ      'A'
187300940919     C                   SETON                                        05
187400050215     C                   OPEN      tignp01L
187500050215     C                   OPEN      tigcp21L
187600050215     C                   OPEN      tigcp02L
187700050215     C                   OPEN      tigcp24L
187800940920     C                   OPEN      FNARB01L
187900940920     C                   ELSE
188000050215     C                   OPEN      tigcp02L
188100050215     C                   OPEN      tigcp07L
188200050215     C                   OPEN      tigcp06L
188300941006     C                   OPEN      FNBLP01L
188400990730     C                   OPEN      TITGC01L
188500940920     C                   OPEN      CNACO00F
188600940920     C                   ENDIF
188700940919      * CARICO FILIALI IN GESTIONE
188800940919     C                   MOVEL     *BLANKS       KEY
188900940919     C                   MOVEL     §FLE          KEY
189000940922     C                   MOVEA     *ZEROS        F£6
189100020503     c                   if        *in05
189200020503     c                   clear                   trul06ds
189300020503     c                   eval      d06cod = '£6'
189400020503     c                   movel     §fle          d06key
189500020503     c                   movel     trul06ds      kpjbu
189600020503     c                   call      'TRUL06R'
189700020503     c                   parm                    kpjba
189800020503     c                   movel     kpjbu         trul06ds
189900020503     c                   movea     lin           f£6
190000020503     c                   else
190100140123     c                   clear                   trovata
190200940919     C  N05              MOVE      '£7'          COD
190300140102      * forzo caricamento filiali dpd per profilo mirato
190400140123     c                   if        %subst(knmus:1:3) = 'INT'
190500140123     c                   movel(p)  §mussei       key
190600140123     C     KTAB          CHAIN     TABEL                              14
190700140123     C  N14              MOVEA     TBLUNI        F£6
190800140123     c  n14              move      'X'           trovata
190900140123     c   14              movel(p)  §fle          key
191000140123     c                   endif
191100140123      *
191200140123     c                   if        trovata = *blank
191300940919     C     KTAB          CHAIN     TABEL                              15
191400940922     C  N15              MOVEA     TBLUNI        F£6
191500940922     C   15              MOVE      §FLE          F£6(1)
191600020503     c                   end
191700140123     c                   endif
191800940923      * CARICO FILIALI GESTITE SULL'AS
191900020503     C* N15              MOVEA     TBLUNI        F£1
192000020503     c                   clear                   trul06ds
192100020503     c                   eval      d06cod = '£1'
192200020503     c                   movel     §fle          d06key
192300020503     c                   movel     trul06ds      kpjbu
192400020503     c                   call      'TRUL06R'
192500020503     c                   parm                    kpjba
192600020503     c                   movel     kpjbu         trul06ds
192700020503     c                   movea     lin           f£1
192800950116      * CARICO DS 2G
192900950116     C                   MOVEL     *BLANKS       KEY
193000950116     C                   MOVEL     '1'           KEY
193100950116     C                   MOVE      '2G'          COD
193200950116     C                   CLEAR                   DS2G
193300950116     C     KTAB          CHAIN     TABEL                              15
193400950116     C  N15              MOVEL     TBLUNI        DS2G
193500940926      * CARICO TABELLE PARTICOLARITà GIACENZE
193600940926     C*---------------------------------------------------------------*
193700940926     C* Carico tabella 7Q
193800950116     C                   Z-ADD     0             X
193900950116     C                   MOVEL     '7Q'          COD
194000950116     C     KTAB2         CHAIN     TABEL                              31
194100950116     C     *IN31         DOWEQ     '0'
194200950116     C     X             ANDLE     50
194300950116     C     TBLFLG        IFEQ      ' '
194400950116     C                   ADD       1             X
194500950116     C                   MOVEL     TBLKEY        C7Q(X)
194600950118     C                   MOVEL     TBLUNI        D7Q(X)
194700950116     C                   END
194800950116     C     KTAB2         READE     TABEL                                  31
194900950116     C                   END
195000940926      *
195100940926     C*  Se non ho caricato tutti i dati gestisco errore
195200940926     C     *IN31         IFNE      '1'
195300940926     C                   SETON                                            28
195400950116     C                   MOVEL     MSG(14)       $MSG
195500940926     C                   END
195600950116      *
195700950116     C*  Se non ho caricato tutti i dati gestisco errore
195800950116     C     *IN31         IFNE      '1'
195900950116     C                   SETON                                            28
196000950116     C                   MOVEL     MSG(16)       $MSG
196100950116     C                   END
196200090819      *
196300090819     C*  Caricamento tabella 7U clienti che vogliono il reso anche nel
196400090819     C*                         periodo delle trazioni ridotte
196500090819     c                   clear                   SK_in_Capienza    1
196600090819     C                   Z-ADD     0             XX                4 0
196700090819     C                   MOVEL     '7U'          COD
196800090819     C     KTAB2         CHAIN     TABEL                              31
196900090819     C     *IN31         DOWEQ     '0'
197000090819     C     X             ANDLT     999
197100090819     C                   MOVE      TBLkey        tipoQ             1
197200090819     C                   MOVEL     TBLUNI        DS7U
197300090819     C     TBLFLG        IFEQ      ' '
197400090819     C     tipoQ         andeq     'Q'
197500090819     C     §7USRE        andeq     'S'
197600090819     C                   ADD       1             XX
197700090819     c                   movel     tblkey        alfa7             7
197800090819     C                   movel     alfa7         K7u(XX)
197900090819     C     XX            ifeq      950
198000090819     c                   move      'S'           SK_in_Capienza
198100090819     c                   end
198200090819     C                   END
198300090819     C     KTAB2         READE     TABEL                                  31
198400090819     C                   END
198500090819      *
198600090819     C*  Se non ho caricato tutti i dati gestisco errore
198700090819     c                   If         SK_in_Capienza = 'S'
198800090819     C     *IN31         IFNE      '1'
198900090819     C                   SETON                                            28
199000090819     C                   MOVEL     MSG(21)       $MSG
199100090819     C                   ElsE
199200090819     C                   SETON                                            28
199300090819     C                   MOVEL     MSG(22)       $MSG
199400090819     C                   END
199500090819     C                   END
199600090819     C*---------------------------------------------------------------*
199700940926      *
199800940919     C*--- RICERCA CAPOCONTI
199900940919     C                   DO        50            X                 2 0
200000940919     C                   MOVE      TCU(X)        TCUDS
200100940919     C     F56           CABNE     'CG'          END1
200200940919     C     F4            COMP      '1'                                    21
200300940919     C** 1 CLIENTI   21
200400940919     C     F3            COMP      '0'                                242425
200500940919     C** 0 ITALIA   25
200600940919     C** 1 ESTERO   24
200700940919     C   21
200800940919     CAN 24              Z-ADD     KCU(X)        KCE               4 0
200900940919     C   21
201000940919     CAN 25              Z-ADD     KCU(X)        KCI               4 0
201100940919     C     END1          TAG
201200940919     C                   END
201300940919     C*§
201400940920      * UDATE
201500940920     C                   TIME                    WHHUDT           14 0
201600940922     C                   MOVE      WHHUDT        G08DAT
201700940920     C                   MOVE      *ZEROS        G08INV
201800940920     C                   MOVEL     *BLANK        G08ERR
201900940920     C                   CALL      'XSRDA8'
202000940920     C                   PARM                    WLBDA8
202100940920     C                   Z-ADD     G08INV        AMGUDT            8 0
202200940920     C                   Z-ADD     G08DAT        GMAUDT            8 0
202300940920     C                   MOVEL     AMGUDT        ANNO              4 0
202400020228     C* gg t.cliente                                       *
202500940922     C     *LIKE         DEFINE    TGCGGR        WGGR
202600950116     C     *LIKE         DEFINE    TGCCTR        KCTR
202700950116     C     *LIKE         DEFINE    TGCKSC        KKSC
202800950116     C     *LIKE         DEFINE    TGCPRG        KPRG
202900940922     C     *LIKE         DEFINE    GCPFGC        FLE
203000940922     C     *LIKE         DEFINE    GCPSCM        SCM
203100961220     C     *LIKE         DEFINE    GNPAGC        KAGC
203200961220     C     *LIKE         DEFINE    GNPFGC        KFGC
203300961220     C     *LIKE         DEFINE    GNPNGC        KNGC
203400961220     C     *LIKE         DEFINE    GNPFRG        KFRG
203500961220     C     *LIKE         DEFINE    GNPFAS        KFAS
203600050324     C     *LIKE         DEFINE    GcPFAS        FAS
203700031029     c     *like         define    gcpFRG        savFRG
203800940919     C*  KLIST
203900940919     C*
204000940919     C     KGCA1         KLIST
204100940919     C                   KFLD                    VAGC
204200940919     C                   KFLD                    VFGC
204300940920     C                   KFLD                    VNGC
204400940919     C                   KFLD                    FRG
204500031029     C*
204600031029     C     KGCA1_GT      KLIST
204700031029     C                   KFLD                    VAGC
204800031029     C                   KFLD                    VFGC
204900031029     C                   KFLD                    VNGC
205000031029     C*
205100961220     C     KGNA          KLIST
205200961220     C                   KFLD                    KAGC
205300961220     C                   KFLD                    KFGC
205400961220     C                   KFLD                    KNGC
205500961220     C                   KFLD                    KFRG
205600961220     C                   KFLD                    KFAS
205700940919     C*
205800940919     C     KGCA2         KLIST
205900940919     C                   KFLD                    VAAS
206000940919     C                   KFLD                    VLNP
206100940919     C                   KFLD                    VNRS
206200940919     C                   KFLD                    VNSP
206300990628     C                   KFLD                    FRG               2 0
206400031029     C*
206500031029     C     KGCA2_GT      KLIST
206600031029     C                   KFLD                    VAAS
206700031029     C                   KFLD                    VLNP
206800031029     C                   KFLD                    VNRS
206900031029     C                   KFLD                    VNSP
207000940919     C*
207100940919     C     KGCA3         KLIST
207200940919     C                   KFLD                    FLE
207300940919     C                   KFLD                    VDIS
207400940919     C*
207500940919     C     KTAB          KLIST
207600940919     C                   KFLD                    CODUT
207700940919     C                   KFLD                    COD               2
207800940919     C                   KFLD                    KEY               8
207900940926     C*
208000940926     C     KTAB2         KLIST
208100940926     C                   KFLD                    CODUT
208200940926     C                   KFLD                    COD
208300940919     C*
208400940919     C     KBLP          KLIST
208500940919     C                   KFLD                    GCPAAS
208600940919     C                   KFLD                    GCPLNP
208700940919     C                   KFLD                    GCPNRS
208800940919     C                   KFLD                    GCPNSP
208900050215     C*
209000050215     C     Karb          KLIST
209100050215     C                   KFLD                    GCPAASa
209200050215     C                   KFLD                    GCPLNPa
209300050215     C                   KFLD                    GCPNRSa
209400050215     C                   KFLD                    GCPNSPa
209500940922     C*
209600940919     C     KACO          KLIST
209700940919     C                   KFLD                    CODUT
209800940919     C                   KFLD                    KCI
209900940919     C                   KFLD                    SCM
210000940922     C*
210100940922     C     KTGC          KLIST
210200940922     C                   KFLD                    KKSC
210300940922     C                   KFLD                    KCTR
210400940922     C                   KFLD                    KPRG
210500031217     C*
210600031217     C     KBLP2         KLIST
210700040108     C                   KFLD                    vAAS1
210800040108     C                   KFLD                    vLNP1
210900040108     C                   KFLD                    vNRS1
211000040108     C                   KFLD                    vNSP1
211100940922     C*
211200050215     C     Kscm          KLIST
211300050215     C                   KFLD                    FLE
211400050215     C                   KFLD                    scm
211500050215     C*
211600080603     c     kar4          klist
211700080603     c                   kfld                    vAAS1
211800080603     c                   kfld                    vLNP1
211900080603     c                   kfld                    vNRS1
212000080603     c                   kfld                    vNSP1
212100080603     c                   kfld                    trek4             1
212200080603     c                   move      'A'           trek4
212300080603      *
212400050324     C     Kflefas       KLIST
212500050324     C                   KFLD                    FLE
212600050324     C                   KFLD                    fas
212700050324     c                   z-add     20            fas
212800060824      * azione forzata XG83 forzatura campi
212900060824     c                   if        kcdaz = 'XG83'
213000060824     c                   seton                                        83
213100060824     c                   move      '1'           vpar
213200060824     c                   endif
213300050324     C*
213400940919     C* FINTO OPEN X RENDERE SHR YES  I FILE
213500941223     C                   OPEN      FNLV22P
213600940919     C   LR
213700940919     CANNLR              EXCEPT    FINTO
213800940919     C                   ENDSR
213900941223     OFNLV22P   E            FINTO
214000950116** MSG
214100940926Giacenza non in Gestione !                                                     1
214200940926Cliente non trovato nel Piano dei Conti                                        2
214300940926Immettere una scelta                                                           3
214400940926Nessuna parzializzazione immessa                                               4
214500940926Selezionare O Immissione Disposizioni  O  Esecuzione Disposizioni              5
214600940926Per questa scelta è possibile immettere o il Nr.Giacenza o il Nr.Spediz.       6
214700940926Per la parzializzazione scelta i dati non sono completi                        7
214800940926Giacenza non trovata !                                                         8
214900050808Giacenza con Disposizioni già immesse o ancora da stampare !                   9
215000940926Nessuna giacenza da visualizzare !                                            10
215100940926Giacenza con disposizioni ancora da Inserire o già  Eseguite                  11
215200961114Punto operativo non in gestione su questo Elaboratore                         12
215300940926Data Errata !                                                                 13
215400940926AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7Q"     14
215500941012Giacenza con data eseguibilità disposizioni > della data odierna              15
215600950116AVVISARE C.E.D. !! Impossibile caricare tutti i cod. NAZIONI  "15"            16
215700050805Impossibile immettere disposizioni oltre le riaperture consentite !!          17
215800051006Se si scegli opzione 4 è obbligatorio selezionare esecuzione disposizioni     18
215900080424Immettere una scelta da 1 a 5                                                 19
216000080424Il Nr.Parcel non è valido o non ha una giacenza collegata.                    20
216100090819AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7U"     21
216200090819AVVISARE C.E.D. !! Tab.: cod.Partic.Giacenza "7U" verso limite massimo.       22
