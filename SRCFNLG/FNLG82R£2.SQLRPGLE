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
006400020227     D TRUL27        E DS                  EXTNAME(TRUL27DS)
006500940920     D KPJBA         E DS
006600020503     d trul06ds      e ds
006700020503     d  lin                    1     87    dim(29)
006800050701     d fnlg09ds      e ds
006900950215     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
007000950215     D PARAM0          DS
007100950215     D  PA0AAS                 1      4  0
007200950215     D  PA0LNP                 5      7  0
007300950215     D  PA0NRS                 8      9  0
007400950215     D  PA0NSP                10     16  0
007500950215     D  PA0FLG                18     18
007600950215     D  PA0SIM                44     46  0
007700950215     D  PA0F03                48     48
007800950215     D PARAM1          DS
007900950215     D  PA1AAS                14     17  0
008000950215     D  PA1LNP                18     20  0
008100950215     D  PA1NRS                21     22  0
008200950215     D  PA1NSP                23     29  0
008300950215     D  PA1F03                30     30
008400950215     D  PA1FLG                31     31
008500031217     d param80         ds
008600031217     d  fldlnpa                       3
008700031217     d  fldaasa                       4
008800031217     d  fldnrsa                       2
008900031217     d  fldnspa                       7
009000031217     d  fldcca                        1
009100031217     d  fldgga                        2
009200940919     D FNLG82        E DS                  EXTNAME(FNLG82DS)
009300941223     D FNLG30        E DS                  EXTNAME(FNLG30DS)
009400960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
009500940920     D                 DS
009600940920     D  §SEI                   1      6
009700940920     D  §PRF                   1      3
009800940920     D  §FLE                   4      6  0
009900140123     d                 ds
010000140123     d  §mussei                1      6
010100140123     d  knmus2                 1     10
010200140123      *
010300080424     D og143         E DS
010400080603     D DSbl4a        E DS
010500940920     D DS2G          E DS
010600050210     D DS20          E DS
010700940926     D DS7Q          E DS
010800040112     D DS7w          E DS
010900090819     D DS7U          E DS
011000020305     D*DSQT1         E DS
011100950116     D DS15          E DS
011200051005     D Dgcpflr       E DS
011300150514     D fnlv59ds      E DS
011400060804     D TIBS34DS      E DS
011500060804     d Azuteds       e ds                  extname(Azute00f)
011600060804     d dDatiute      e ds
011700060804     d dLat          e ds
011800060804     d dute01        e ds
011900940920      * GESTIONE DATE
012000940920     D WLBDA8          DS
012100940920     D  G08DAT                 1      8  0
012200940920     D  G08INV                 9     16  0
012300940920     D  G08ERR                17     17
012400940920     D  G08TGI                18     22  0
012500911010      *
012600940920     D WGIDA8          DS
012700940920     D  GI8DAT                 1      8  0
012800940920     D  GI8INV                 9     16  0
012900940920     D  GI8TGI                17     21  0
013000911115     D*
013100940920      * PARAM X RICEZIONE BOLLE LOCALI - IN ARRIVO
013200930511     D FRB3DS          DS
013300930511     D  FRB3BD                 1      1
013400930511     D  FRB3BK                 2      2
013500930511     D  FRB3BT                 3      3
013600930511     D  FRB3T2                 4      4
013700930511     D  FRB3BG                 5      5
013800930511     D  FRB3EL                 6      8
013900940919     D                SDS
014000940919     D  NOMPGM                 1     10
014100940920      * DATA GIACENZA
014200940920     D                 DS
014300940920     D  DGC                    1      8  0
014400940920     D  DAGC                   1      4  0
014500940920     D  DMGC                   5      8  0
014600940920      * DATA GIACENZA
014700940920     D                 DS
014800940920     D  GCPMGC                 1      4  0
014900940920     D  MESE                   1      2  0
015000940920     D  GIOR                   3      4  0
015100080424      *
015200080424     D  Network_DPD    s              1    inz('N')
015300080424     D  Pulisce_DPD    s              1
015400050524      **********************************************************************
015500050524      * Constants
015600050524      *
015700050524      *     MaxKey - Maximum number of key fields allowed by this program
015800050524      *
015900050524      **********************************************************************
016000050525     D MaxKey          C                    2
016100050525     D mittente        C                   '1'
016200050525     D giacenza        C                   '2'
016300050524     D Ascendente      C                   1
016400050524     D Carattere       C                   6
016500050524     D Put             C                   1
016600050524     D EndPut          C                   2
016700050524     D Get             C                   3
016800050524      **********************************************************************
016900050524      * Standalone fields
017000050524      *
017100050524      *     nrr        - Relative record number for subfile
017200050524      *     SizeList   - Size of list
017300050524      *     ReturnSize - Size of list returned by sort API
017400050524      **********************************************************************
017500050524     D NotUsed         S             16A
017600050524     D ReturnSize      S              9B 0
017700050524     D SizeList        S              9B 0
017800050524     D RrnLast         S              5i 0
017900050524     D WrkSort         S              1
018000050524     D Vrsm1w          S                   LIKE(Vrsm1)
018100050524      **********************************************************************
018200050524      * Data Structures
018300050524      *     SflRcd     - The entire subfile record to pass to the sort
018400050524      *     QLGSCB     - The sort request block for the QLGSORT API
018500050524      *     QLGSCB00   - The sort request block for the QLGSRTIO API
018600050524      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
018700050524      *     QUSEC      - Error structure for the QLGSORT API
018800050524      **********************************************************************
018900050524     D SflRcd          DS
019000050524     d VSEL
019100050524     d VLNP1
019200050524     d VFGC1
019300050524     d VAGC1
019400050524     d VNGC1
019500050524     d VMGC1
019600050524     d VCMC1
019700050524     d VDIS1
019800050524     d VAAS1
019900050524     d VNRS1
020000050524     d VNSP1
020100050524     D VRSM1
020200050524     d Vmsg1
020300050524     D VRSd1
020400080603     D Vddts
020500090819     D  h1in52
020600050524     D Selected                       1A
020700050524
020800050524      /COPY QSYSINC/QRPGLESRC,QLGSORT
020900050524     D QLGKL                         16    DIM(MaxKey)
021000050524     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021100050524     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021200050524     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021300050524     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021400050524
021500050524      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021600050524      /COPY QSYSINC/QRPGLESRC,QUSEC
021700050524      *---------------------------------------------------------------------
021800900418     C*
021900911010     C     *INKC         DOWEQ     '0'
022000940926     C     *IN28         IFEQ      '0'
022100940926     C                   Z-ADD     0             VFGC
022200940926     C                   Z-ADD     ANNO          VAGC
022300940926     C                   Z-ADD     0             VNGC
022400950215     C     WSCE          IFNE      'S'
022500940926     C                   Z-ADD     0             VLNP
022600940926     C                   Z-ADD     ANNO          VAAS
022700940926     C                   Z-ADD     0             VNRS
022800940926     C                   Z-ADD     0             VNSP
022900950215     C                   ELSE
023000950215     C                   MOVEL     '2'           VPAR
023100950215     C                   END
023200940926     C                   MOVEL     *BLANK        VDIS
023300940926     C                   Z-ADD     GMAUDT        VDED
023400940926     C                   ENDIF
023500940926     C     D82GES        IFEQ      'A'
023600940926      *ARRIVO
023700051006     c                   if        vpar = 4
023800051006     c                   clear                   vdis
023900051006     c                   end
024000950215     C                   MOVEL     'N'           WSCE              1
024100080424      *
024200940919     C                   EXFMT     FG82D3
024300080424      *
024400940923     C* Se anno a 2 lo porto a  4
024500940923     C     VNGC          IFNE      *ZEROS
024600940923     C                   MOVEL     VAGC          WCOM2             2 0
024700940923     C     WCOM2         IFEQ      0
024800940923     C     VAGC          IFGT      60
024900940923     C                   ADD       1900          VAGC
025000940923     C                   ELSE
025100940923     C                   ADD       2000          VAGC
025200940923     C                   END
025300940923     C                   END
025400940923     C                   ENDIF
025500940923     C     VNSP          IFNE      *ZEROS
025600940923     C                   MOVEL     VAAS          WCOM2             2 0
025700940923     C     WCOM2         IFEQ      0
025800940923     C     VAAS          IFGT      60
025900940923     C                   ADD       1900          VAAS
026000940923     C                   ELSE
026100940923     C                   ADD       2000          VAAS
026200940923     C                   END
026300940923     C                   END
026400940923     C                   ENDIF
026500940920      * INTERROGAZ. BOLLE ARRIVI
026600950215     C     *INKG         IFEQ      '1'
026700950215     C                   Z-ADD     VAAS          PA1AAS
026800950215     C                   Z-ADD     VLNP          PA1LNP
026900950215     C                   Z-ADD     VNRS          PA1NRS
027000950215     C                   Z-ADD     VNSP          PA1NSP
027100950215     C                   MOVEL     ' '           PA1F03
027200950215     C                   MOVEL     '2'           PA1FLG
027300950215     C     PA1NSP        IFGT      0
027400950215     C                   MOVEL     '1'           PA1FLG
027500950215     C                   END
027600950215     C                   MOVEL     PARAM1        KPJBU
027700950215     C                   CALL      'FNLR36R'
027800950215     C                   PARM                    KPJBA
027900950215     C                   MOVEL     KPJBU         PARAM1
028000950215     C     PA1F03        IFNE      '1'
028100950215     C                   MOVEL     'S'           WSCE
028200950215     C                   Z-ADD     PA1AAS        VAAS
028300950215     C                   Z-ADD     PA1LNP        VLNP
028400950215     C                   Z-ADD     PA1NRS        VNRS
028500950215     C                   Z-ADD     PA1NSP        VNSP
028600950215     C                   END
028700950215     C                   END
028800950215     C*
028900940926     C                   ELSE
029000080424      *
029100080424     c     ritorno_D2    tag
029200080424      *?  Controlla se si tratta di NTW DPD...  ?
029300080424     c                   exsr      Check_se_DPD
029400080424      *
029500940926      *PARTENZA
029600940919     C                   EXFMT     FG82D2
029700080424      *
029800940926     C* Se anno a 2 lo porto a  4
029900940926     C     VNGC          IFNE      *ZEROS
030000940926     C                   MOVEL     VAGC          WCOM2             2 0
030100940926     C     WCOM2         IFEQ      0
030200940926     C     VAGC          IFGT      60
030300940926     C                   ADD       1900          VAGC
030400940926     C                   ELSE
030500940926     C                   ADD       2000          VAGC
030600940926     C                   END
030700940926     C                   END
030800940926     C                   ENDIF
030900940920      * INTERROGAZ. BOLLE PARTENZE
031000950215     C     *INKG         IFEQ      '1'
031100950215     C                   CLEAR                   PARAM0
031200950215     C                   MOVEL     PARAM0        KPJBU
031300950215     C                   CALL      'FNLS04R'
031400950215     C                   PARM                    KPJBA
031500950215     C                   MOVEL     KPJBU         PARAM0
031600950215     C                   END
031700950215     C*
031800940926     C                   ENDIF
031900940920      * CAMBIO FILIALE
032000080424     C   KS              Do
032100080424     C                   EXSR      CMBFIL
032200080522     C                   IF        d82ges <> 'A'
032300080424     c                   goto      ritorno_D2
032400080522     c                   endif
032500080424     c                   endDo
032600940920      * INTERROGAZ. GIACENZE
032700941223     C   KN              DO
032800941223     C                   CLEAR                   FNLG30
032900941223     C                   MOVEL     D82GES        D30POA
033000941223     C                   MOVEL     FNLG30        KPJBU
033100950103     C     D82GES        IFEQ      'A'
033200941228     C                   CALL      'FNLG31C'
033300920427     C                   PARM                    KPJBA
033400950103     C                   ELSE
033500950103     C                   CALL      'FNLG30C'
033600950103     C                   PARM                    KPJBA
033700941223     C                   END
033800950103     C                   END
033900940919      * CONTROLLI VIDEATA INIZIALE
034000940919     C  NKC
034100940919     CANNKG
034200940919     CANNKN              EXSR      CTRVID
034300940919      *
034400940920     C  N21
034500940920     CANNKS
034600940920     CANNKN
034700940919     CANNKC
034800940919     CANN28
034900940919     CANNKG              DO
035000911023     C                   Z-ADD     0             FRG
035100940920      *
035200940919      * PARTENZA
035300940919      *---------
035400940920      * ----> IMMESSO NR.GIACENZA
035500940920     C     D82GES        IFEQ      'P'
035600940919     C     VPAR          ANDEQ     1
035700080424      *
035800080424      * Oppure se immesso il Parcel -> ha tradotto la giacenza
035900080424     C     d82ges        orEQ      'P'
036000080424     C     Vpar          andEQ     6
036100080424      *
036200050215     C     KGCA1         CHAIN     tigcp02L                           48
036300940920     C     *IN48         IFEQ      '0'
036400940919      * CONTROLLO SE GIACENZA IN GESTIONE
036500060804     c*mm                if        kcdaz = 'XG83'
036600060804     c*mm                seton                                        89
036700060804     c*mm                else
036800940919     C     §FLE          COMP      GCPLNP                                 89
036900940919     C  N89              MOVE      GCPLNP        LNP§              3
037000940922     C  N89LNP§          LOOKUP    F£6                                    89
037100060804     c*mm                endif
037200940928     C   89              MOVE      GCPAGC        D82AGC
037300940928     C   89              MOVE      GCPFGC        D82FGC
037400940928     C   89              MOVE      GCPNGC        D82NGC
037500940922     C   89              MOVEL     *BLANKS       D82DIS
037600941215     C   89              MOVEL     §FLE          D82FGS
037700050316     C   89              MOVE      'S'           D82comm
037800050726     c   89              clear                   d82ntaut
037900050726     c   89              clear                   d82nodsp
038000940920     C   89              MOVEL     FNLG82        KPJBU
038100940920     C   89              CALL      'FNLG10R'
038200911030     C                   PARM                    KPJBA
038300080424      *
038400080424      * Di ritorno dal chiamante deve ripulire sia l'opz.che il Parcel
038500080424     c                   if        vPar = 6 and Vparcel <> ' '
038600080424     c                   eval      pulisce_DPD = 'S'
038700080424     c                   end
038800080424      *
038900940920     C  N89              SETON                                        4828
039000940920     C   48              MOVEL     MSG(1)        $MSG
039100940919     C                   ELSE
039200940919     C                   SETON                                        4828
039300940919     C                   MOVEL     MSG(8)        $MSG
039400940920     C                   ENDIF
039500940920     C                   ENDIF
039600940919      * ARRIVO :
039700940919      *---------
039800940920     C     D82GES        IFEQ      'A'
039900940920      * ----> IMMESSO NR. GIACENZA
040000940920     C     VPAR          IFEQ      1
040100050630      * calcolo numero riaperture
040200050630     c                   exsr      srconta
040300050630     c*
040400050215     C     KGCA1         CHAIN     tigcp02L                           48
040500941012     C   48              MOVEL     MSG(8)        $MSG
040600941012     C   48              SETON                                        28
040700031029      *
040800940920     C     *IN48         IFEQ      '0'
040900940919      * CONTROLLO SE GIACENZA IN GESTIONE
041000940919     C     VFGC          COMP      §FLE                                   89
041100940919     C  N89              MOVE      VFGC          LNP§
041200940922     C  N89LNP§          LOOKUP    F£6                                    89
041300940920     C  N89              SETON                                        4828
041400940920     C   48              MOVEL     MSG(1)        $MSG
041500031029      *
041600940920      * IMMISS.DISPOSIZ.
041700940920     C  N28VSEL1         IFEQ      'X'
041800031029      *
041900040705      * per non immettere disposizioni oltre le riaperture consentite + 1
042000040705     c                   z-add     0             limite            1 0
042100040705     c                   z-add     1             xy                3 0
042200050215     c     karb          chain     fnarb01l
042300040705     c                   if        arbgga <> *blank
042400040705     c     arbgga        lookup    c7q(xy)                                55
042500040705     c                   else
042600040705     c     'GR'          lookup    c7q(xy)                                55
042700040705     c                   endif
042800040705     c   55              movel     d7q(xy)       ds7q
042900040705     c                   eval      limite = §7q5rp + 1
043000040705     c                   if        savFRG > limite
043100031022     C                   SETON                                        4828
043200031022     C                   MOVEL     MSG(17)       $MSG
043300031022     c                   end
043400031029      *
043500050215     C  n28GCPFAS        IFEQ      20
043600940928     C                   MOVE      GCPAGC        D82AGC
043700940928     C                   MOVE      GCPFGC        D82FGC
043800940928     C                   MOVE      GCPNGC        D82NGC
043900940922     C                   MOVEL     *BLANKS       D82DIS
044000941215     C                   MOVEL     §FLE          D82FGS
044100050316     C                   MOVE      'S'           D82comm
044200050726     c                   clear                   d82nodsp
044300050726     c                   clear                   d82ntaut
044400940920     C                   MOVEL     FNLG82        KPJBU
044500940920     C                   CALL      'FNLG10R'
044600911030     C                   PARM                    KPJBA
044700911121     C                   ELSE
044800940920     C                   SETON                                        4828
044900940920     C                   MOVEL     MSG(9)        $MSG
045000911023     C                   END
045100940920     C                   ENDIF
045200031029      *
045300940920      * ESECUZ. DISPOSIZ.
045400940920     C  N28VSEL2         IFEQ      'X'
045500050215     C     GCPFAS        IFEQ      30
045600050215     C     GCPFAS        OREQ      31
045700050215     C     GCPFAS        OREQ      36
045800941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
045900941012     C     GCPDED        IFGT      AMGUDT
046000941012     C                   SETON                                        4828
046100941012     C                   MOVEL     MSG(15)       $MSG
046200941012     C                   ELSE
046300040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
046400040112     c                   CLEAR                   ds7w
046500050215     c     karb          chain     fnarb01l
046600040112     c                   if        arbgga <>*blank
046700040112     c                   move      '7W'          cod
046800040112     c                   movel     *blank        key
046900040112     c                   movel     arbgga        key
047000040112     c     ktab          chain     tabel00f
047100040112     c                   if        %found(tabel00f)
047200040112     c                   movel     tbluni        ds7w
047300040112     c                   endif
047400040112     c                   endif
047500031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
047600040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
047700040112     c                             gcpdis = §7w7t3
047800031217     c                   movel     arblnp        fldlnpa
047900031217     c                   movel     arbaas        fldaasa
048000031217     c                   movel     arbnrs        fldnrsa
048100031217     c                   movel     arbnsp        fldnspa
048200040112     c                   select
048300040112     c                   when      gcpdis = §7w7t1
048400040112     c                   movel     §7w7ca1       fldcca
048500040112     c                   when      gcpdis = §7w7t2
048600040112     c                   movel     §7w7ca2       fldcca
048700040112     c                   when      gcpdis = §7w7t3
048800040112     c                   movel     §7w7ca3       fldcca
048900040112     c                   endsl
049000031217     c                   movel     arbgga        fldgga
049100031217     c                   movel(p)  param80       kpjbu
049200031217     c                   call      'FNLG80R1'
049300031219     c                   parm                    kpjba
049400031217     c                   else
049500940928     C                   MOVE      GCPAGC        D82AGC
049600940928     C                   MOVE      GCPFGC        D82FGC
049700940928     C                   MOVE      GCPNGC        D82NGC
049800940922     C                   MOVEL     *BLANKS       D82DIS
049900941215     C                   MOVEL     §FLE          D82FGS
050000050316     C                   MOVE      'S'           D82comm
050100050726     c                   clear                   d82nodsp
050200050726     c                   clear                   d82ntaut
050300940920     C                   MOVEL     FNLG82        KPJBU
050400940920     C                   CALL      'FNLG20R'
050500911030     C                   PARM                    KPJBA
050600031217     C                   Endif
050700941012     C                   END
050800911121     C                   ELSE
050900940920     C                   SETON                                        4828
051000940920     C                   MOVEL     MSG(11)       $MSG
051100911023     C                   END
051200940920     C                   ENDIF
051300940919     C                   ELSE
051400940920     C                   SETON                                        4828
051500940919     C                   MOVEL     MSG(8)        $MSG
051600940920     C                   ENDIF
051700940920     C                   ENDIF
051800940919      *
051900940919      *         ----> IMMESSO NR. SPEDIZIONE
052000940920     C     VPAR          IFEQ      2
052100050630      * calcolo numero riaperture
052200050630     c                   exsr      srconta
052300031029      *
052400050215     C     KGCA2         CHAIN     tigcp21L                           46
052500031029      *
052600941012     C   46              MOVEL     MSG(8)        $MSG
052700941012     C   46              SETON                                        28
052800031029      *
052900940920     C     *IN46         IFEQ      '0'
053000940920     C     §FLE          COMP      GCPFGC                                 89
053100050215     C  N89              MOVE      GCPLNAa       LNP§
053200940922     C  N89LNP§          LOOKUP    F£6                                    89
053300940920     C  N89              SETON                                        4628
053400940920     C   46              MOVEL     MSG(1)        $MSG
053500940920      * IMMISS. DISPOSIZ.
053600940920     C  N28VSEL1         IFEQ      'X'
053700031029      *
053800040705      * per non immettere disposizioni oltre le riaperture consentite + 1
053900040705     c                   z-add     0             limite
054000040705     c                   z-add     1             xy
054100050215     c     karb          chain     fnarb01l
054200040705     c                   if        arbgga <> *blank
054300040705     c     arbgga        lookup    c7q(xy)                                55
054400040705     c                   else
054500040705     c     'GR'          lookup    c7q(xy)                                55
054600040705     c                   endif
054700040705     c   55              movel     d7q(xy)       ds7q
054800040705     c                   eval      limite = §7q5rp + 1
054900040705     c                   if        savFRG > limite
055000031029     C                   SETON                                        4628
055100031029     C                   MOVEL     MSG(17)       $MSG
055200031029     c                   end
055300031029      *
055400050215     C  n28GCPFAS        IFEQ      20
055500940928     C                   MOVE      GCPAGC        D82AGC
055600940928     C                   MOVE      GCPFGC        D82FGC
055700940928     C                   MOVE      GCPNGC        D82NGC
055800940922     C                   MOVEL     *BLANKS       D82DIS
055900941215     C                   MOVEL     §FLE          D82FGS
056000050316     C                   MOVE      'S'           D82comm
056100050726     c                   clear                   d82nodsp
056200050726     c                   clear                   d82ntaut
056300940920     C                   MOVEL     FNLG82        KPJBU
056400940920     C                   CALL      'FNLG10R'
056500911030     C                   PARM                    KPJBA
056600911105     C                   ELSE
056700940920     C                   SETON                                        4628
056800940919     C                   MOVEL     MSG(9)        $MSG
056900940920     C                   END
057000940920     C                   ENDIF
057100940920      * ESECUZ. DISPOSIZ.
057200940920     C  N28VSEL2         IFEQ      'X'
057300050215     C     GCPFAS        IFEQ      30
057400050215     C     GCPFAS        OREQ      31
057500050215     C     GCPFAS        OREQ      36
057600941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
057700941012     C     GCPDED        IFGT      AMGUDT
057800941012     C                   SETON                                        4828
057900941012     C                   MOVEL     MSG(15)       $MSG
058000941012     C                   ELSE
058100031217      * verifico la bolla
058200040112     c                   CLEAR                   ds7w
058300050215     c     karb          chain     fnarb01l
058400040112     c                   if        arbgga <>*blank
058500040112     c                   move      '7W'          cod
058600040112     c                   movel     *blank        key
058700040112     c                   movel     arbgga        key
058800040112     c     ktab          chain     tabel00f
058900040112     c                   if        %found(tabel00f)
059000040112     c                   movel     tbluni        ds7w
059100040112     c                   endif
059200040112     c                   endif
059300031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
059400040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
059500040112     c                             gcpdis = §7w7t3
059600031217     c                   movel     arblnp        fldlnpa
059700031217     c                   movel     arbaas        fldaasa
059800031217     c                   movel     arbnrs        fldnrsa
059900031217     c                   movel     arbnsp        fldnspa
060000040112     c                   select
060100040112     c                   when      gcpdis = §7w7t1
060200040112     c                   movel     §7w7ca1       fldcca
060300040112     c                   when      gcpdis = §7w7t2
060400040112     c                   movel     §7w7ca2       fldcca
060500040112     c                   when      gcpdis = §7w7t3
060600040112     c                   movel     §7w7ca3       fldcca
060700040112     c                   endsl
060800031217     c                   movel     arbgga        fldgga
060900031217     c                   movel(p)  param80       kpjbu
061000031217     c                   call      'FNLG80R1'
061100031219     c                   parm                    kpjba
061200031217     c                   else
061300940928     C                   MOVE      GCPAGC        D82AGC
061400940928     C                   MOVE      GCPFGC        D82FGC
061500940928     C                   MOVE      GCPNGC        D82NGC
061600940922     C                   MOVEL     *BLANKS       D82DIS
061700941215     C                   MOVEL     §FLE          D82FGS
061800050316     C                   MOVE      'S'           D82comm
061900050726     c                   clear                   d82nodsp
062000050726     c                   clear                   d82ntaut
062100940920     C                   MOVEL     FNLG82        KPJBU
062200940920     C                   CALL      'FNLG20R'
062300911030     C                   PARM                    KPJBA
062400031217     C                   Endif
062500941012     C                   END
062600911105     C                   ELSE
062700940920     C                   SETON                                        4628
062800940920     C                   MOVEL     MSG(11)       $MSG
062900940920     C                   END
063000940920     C                   ENDIF
063100941012     C                   ENDIF
063200940920     C                   ENDIF
063300940920     C                   ENDIF
063400940920      *
063500940919      * NESSUN SCELTA SPECIFICA, CARICO SFL
063600940919      *
063700940919     C  N28D82GES        IFEQ      'P'
063800940919     C     VPAR          ANDNE     1
063900080424     C     vpar          andNE     6
064000080424      *
064100940919     C     D82GES        OREQ      'A'
064200940922     C     VPAR          ANDEQ     3
064300080424      *
064400010920     C     D82GES        OREQ      'A'
064500010920     C     VPAR          ANDEQ     4
064600080424      *
064700940919     C                   EXSR      CARICA
064800080424      *
064900940919     C     NRR           IFEQ      0
065000940919     C                   SETON                                        28
065100940920     C                   MOVEL     MSG(10)       $MSG
065200911030     C                   ELSE
065300050512     c     riemetti      tag
065400911105     C                   WRITE     FG82D4
065500911105     C                   EXFMT     FG82CT
065600911105     C                   SETOFF                                       30
065700950207     C                   MOVEL     'N'           WSEL              1
065800050525      * ordina per mittente o nr. giacenza
065900050524     c                   if        *inkk
066000050525     c                   setoff                                       32
066100050525      * scelta ordinamento
066200050525     c                   select
066300050525     c                   when      wrksort = giacenza or wrksort = *blank
066400050525     c                   eval      wrksort = mittente
066500050525     c                   seton                                        07
066600050525     c                   when      wrksort = mittente
066700050525     c                   eval      wrksort = giacenza
066800050525     c                   setoff                                       07
066900050525     c                   endsl
067000050525      *
067100050613     c                   eval      rrnlast = wnrr
067200050524     c                   exsr      sort
067300050524     c                   goto      riemetti
067400050524     c                   end
067500911105     C*
067600050527     c                   if        not *inkc and not *inkl and not *inkf
067700051005     c                             and not *inka and not *inkg
067800050527     c                   goto      riemetti
067900050527     c                   endif
068000050527     c  NKL
068100050527     CANNKC*IN30         DOWEQ     '0'
068200911105     C                   READC     FG82SF                                 30
068300050513      *verifico se scelta opzione 7 non per i resi
068400050513     c                   if        vsel = '7' and (vpar <> 3 and vpar <> 5)
068500050517     c                             or vsel = '7' and d82ges = 'A'
068600050517     c                   goto      riemetti
068700050513     c                   endif
068800050811      * periodo resi bloccati
068900050811     c                   if        amgudt >= §2gdpi and amgudt <= §2gdpf
069000050811     c                             and d82ges = 'P' and (vpar = 3 or vpar = 5 )
069100050811     c                   goto      riemetti
069200050811     c                   else
069300050811      * cmd1 spalma opzione 1 su tutte le righe
069400050811     c                   if        *inka and *in30
069500050525     c                   setoff                                       32
069600050512     c                   do        wnrr          nrr
069700050613     c     nrr           chain     fg82sf                             27
069800050613     c   27              leave
069900050512     c                   move      '1'           vsel
070000050512     c                   seton                                        72
070100090819     c                   eval        *in52 = H1IN52
070200050512     c                   update    fg82sf
070300050512     c                   enddo
070400050512     c                   goto      riemetti
070500050512     c                   endif
070600050512      *
070700050525      * cmd7 spalma opzione 7 su tutte le righe
070800050811     c                   if        *inkg and *in30
070900050525     c                   setoff                                       32
071000050512     c                   do        wnrr          nrr
071100050613     c     nrr           chain     fg82sf                             27
071200050613     c   27              leave
071300050512     c                   move      '7'           vsel
071400050512     c                   seton                                        72
071500090819     c                   eval        *in52 = H1IN52
071600050512     c                   update    fg82sf
071700050512     c                   enddo
071800050512     c                   goto      riemetti
071900050512     c                   endif
072000050811      *resi bloccati
072100050811     c                   endif
072200050527     C
072300050513     c                   clear                   d82nodsp
072400050527     c                   clear                   d82ntaut
072500050527     C   KF
072600050527     CANN30VSEL          IFEQ      '1'
072700050513     C     VSEL          oreq      '7'
072800941007     C                   MOVE      VAGC1         D82AGC
072900941007     C                   MOVE      VFGC1         D82FGC
073000941007     C                   MOVE      VNGC1         D82NGC
073100941222     C                   MOVEL     VDIS1         D82DIS
073200941215     C                   MOVEL     §FLE          D82FGS
073300050316     C                   MOVE      'S'           D82comm
073400050513      *forza parametro per non emettere videate pgm chiamanti con opz=7
073500050513     c                   if        vsel = '7'
073600050513     c                   move      'X'           d82nodsp
073700050726     c                   move      'S'           d82ntaut
073800050513     c                   endif
073900941221     C                   MOVEL     FNLG82        KPJBU
074000940922     C     D82GES        IFEQ      'A'
074100940922     C     VSEL1         IFEQ      'X'
074200950207     C                   MOVEL     'S'           WSEL              1
074300940920     C                   CALL      'FNLG10R'
074400911128     C                   PARM                    KPJBA
074500911128     C                   ELSE
074600950207     C                   MOVEL     'S'           WSEL              1
074700040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
074800040112     c                   CLEAR                   ds7w
074900031217     c     kblp2         chain     fnarb01l
075000040112     c                   if        arbgga <>*blank
075100040112     c                   move      '7W'          cod
075200040112     c                   movel     *blank        key
075300040112     c                   movel     arbgga        key
075400040112     c     ktab          chain     tabel00f
075500040112     c                   if        %found(tabel00f)
075600040112     c                   movel     tbluni        ds7w
075700040112     c                   endif
075800040112     c                   endif
075900040112      * se disposizione di reso e sulla bolla c'è la particolarità GS
076000040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
076100040112     c                             gcpdis = §7w7t3
076200031217     c                   movel     arblnp        fldlnpa
076300031217     c                   movel     arbaas        fldaasa
076400031217     c                   movel     arbnrs        fldnrsa
076500031217     c                   movel     arbnsp        fldnspa
076600040112     c                   select
076700040112     c                   when      gcpdis = §7w7t1
076800040112     c                   movel     §7w7ca1       fldcca
076900040112     c                   when      gcpdis = §7w7t2
077000040112     c                   movel     §7w7ca2       fldcca
077100040112     c                   when      gcpdis = §7w7t3
077200040112     c                   movel     §7w7ca3       fldcca
077300040112     c                   endsl
077400031217     c                   movel     arbgga        fldgga
077500031217     c                   movel(p)  param80       kpjbu
077600031217     c                   call      'FNLG80R1'
077700031219     c                   parm                    kpjba
077800031217     c                   else
077900051006     c                   if        vpar <> 4
078000940920     C                   CALL      'FNLG20R'
078100911128     C                   PARM                    KPJBA
078200051006     C                   Endif
078300031217     C                   Endif
078400051005     c* Appuntamento
078500051005     c                   if        vpar = 4
078600051005     C                   CALL      'FNLG88R'
078700051005     C                   PARM                    KPJBA
078800051005     c                   end
078900940922     C                   END
079000911128     C                   ELSE
079100051005     c* PARTENZA
079200950207     C                   MOVEL     'S'           WSEL              1
079300940920     C                   CALL      'FNLG10R'
079400911105     C                   PARM                    KPJBA
079500940922     C                   END
079600940922     C                   END
079700911105     C                   Z-ADD     0             VFGC
079800911105     C                   Z-ADD     0             VAGC
079900911105     C                   Z-ADD     0             VNGC
080000940922     C                   END
080100950207     C*
080200950207     C     WSEL          IFEQ      'S'
080300950207     C                   EXSR      CARICA
080400950207     C                   END
080500950207     C*
080600950207     C                   END
080700940919     C                   ENDIF
080800940922     C                   END
080900911023     C                   END
081000940919      *----------------------------------------------------
081100020306     C                   CLEAR                   TRUL27
081200020306     C                   MOVEL     'C'           I27TLA
081300020306     C                   CALL      'TRUL27R'
081400020306     C                   PARM                    TRUL27
081500900418     C                   SETON                                        LR
081600080424     C*---------------------------------------------------------------------*
081700080424     c*  Controlla la filiale su AZorg per controllare il NTW
081800080424     C*---------------------------------------------------------------------*
081900080424     c     Check_se_DPD  begsr
082000080424      *
082100080424      *
082200080424      * se DPD deve dare la possibilità di inserire direttamente il Parcel
082300080424     c                   eval      Network_DPD ='N'
082400080424     c     §FLE          chain     azorg01l
082500080424     c                   if        %Found(Azorg01l)
082600080424     c                   movel     orgDE3        Og143
082700140108     c                   if        §OGNTW = 'DPD' or
082800140123     c                             %subst(knmus:1:3) = 'INT'
082900080424     c                   eval      Network_DPD ='S'
083000080424     c                   end
083100080424     c                   end
083200080424      *
083300080424      * per 84 = *on Protegge l'inserimento del Nr. Parcel a video Opz.6
083400080424      *
083500080424     c     Network_DPD   comp      'N'                                    84
083600080424      *
083700080424     c                   if        Network_DPD = 'N' or
083800080424     c                             pulisce_DPD = 'S'
083900080424     c                   clear                   vParcel
084000080424     c                   if        vPar = 6
084100080424     c                   clear                   vPar
084200080424     c                   end
084300080424     c                   end
084400080424      *
084500080424     c                   eval      pulisce_DPD = 'N'
084600080424      *
084700080424     c                   endsr
084800080424     C*---------------------------------------------------------------------*
084900940923     C*--------------------*
085000050630     C* Conta numero riaperture
085100940923     C*--------------------*
085200050630     C     srconta       BEGSR
085300050630     c*
085400050630     c                   clear                   fnlg09ds
085500050630     c                   eval      d09tla = 'R'
085600050630     c                   select
085700050630     C     VPAR          wheneq    1
085800050630     c                   eval      d09AGC = VAGC
085900050630     c                   eval      d09FGC = VFGC
086000050630     c                   eval      d09NGC = VNGC
086100050630     C     VPAR          wheneq    2
086200050630     c                   eval      d09aas = VAAS
086300050630     c                   eval      d09lnp = VLNP
086400050630     c                   eval      d09nrs = VNRS
086500050630     c                   eval      d09nsp = VNSP
086600050630     c                   endsl
086700050630     c                   movel(p)  fnlg09ds      kpjbu
086800050630     C                   CALL      'FNLG09R'
086900050630     C                   PARM                    kpjba
087000050630     c                   movel(p)  kpjbu         fnlg09ds
087100050630     c     d09cnt        sub       d09ced        savfrg
087200050630     c*
087300050630     c                   endsr
087400050630     C*--------------------*
087500050630     C* CAMBIO FILIALE
087600050630     C*--------------------*
087700050630     C     CMBFIL        BEGSR
087800940926     C                   SETOFF                                       28
087900940926     C                   MOVEL     *BLANK        $MSG
088000940923     C                   MOVE      §FLE          VD1FLE
088100940923     C     *IN28         DOUEQ     '0'
088200940923     C                   EXFMT     FG82D1
088300940926     C                   SETOFF                                       28
088400940923     C   KL              SETOFF                                       28
088500940923     C  NKL              DO
088600940923     C                   MOVE      VD1FLE        LNP§              3
088700940923     C     LNP§          LOOKUP    F£1                                    31
088800940923     C     *IN31         IFEQ      '0'
088900940923     C                   SETON                                        28
089000940923     C                   MOVEL     MSG(12)       $MSG
089100940923     C                   ELSE
089200940923      * CARICO FILIALI IN GESTIONE
089300940923     C                   MOVE      VD1FLE        §FLE
089400940923     C                   MOVEL     *BLANKS       KEY
089500940923     C                   MOVEL     §FLE          KEY
089600940923     C                   MOVEA     *ZEROS        F£6
089700940923     C     D82GES        IFEQ      'A'
089800020503     c                   clear                   trul06ds
089900020503     c                   eval      d06cod = '£6'
090000020503     c                   movel     §fle          d06key
090100020503     c                   movel     trul06ds      kpjbu
090200020503     c                   call      'TRUL06R'
090300020503     c                   parm                    kpjba
090400020503     c                   movel     kpjbu         trul06ds
090500020503     c                   movea     lin           f£6
090600020503     C*                  MOVE      '£6'          COD
090700940923     C                   ELSE
090800140123     c                   clear                   trovata           1
090900940923     C                   MOVE      '£7'          COD
091000140102      * forzo caricamento filiali dpd per profilo mirato
091100140123     c                   if        %subst(knmus:1:3) = 'INT'
091200140123     c                   movel(p)  §mussei       key
091300140123     C     KTAB          CHAIN     TABEL                              14
091400140123     C  N14              MOVEA     TBLUNI        F£6
091500140123     c  n14              move      'X'           trovata
091600140123     c   14              movel(p)  §fle          key
091700140102     c                   endif
091800140102      *
091900140123     c                   if        trovata = *blank
092000940923     C     KTAB          CHAIN     TABEL                              15
092100940923     C  N15              MOVEA     TBLUNI        F£6
092200940923     C   15              MOVE      §FLE          F£6(1)
092300140123     c                   endif
092400020503     C                   ENDIF
092500020503      *
092600940923     C                   ENDIF
092700940923     C                   ENDDO
092800940923     C                   ENDDO
092900940923     C                   ENDSR
093000911017     C*--------------------*
093100911017     C* CARICAMENTO SUBFILE
093200911017     C*--------------------*
093300911010     C     CARICA        BEGSR
093400051006     c* per appuntamento forzo la disposizione = consegna
093500051006     c                   if        vpar = 4 and d82ges = 'A'
093600051006     c                   eval      vdis = 'C'
093700051006     c                   end
093800051006     c*
093900911017     C                   EXSR      PULIZ
094000940922     C* MI POSIZIONO X OGNI FILIALE IN GESTIONE
094100940922     C                   DO        29            I                 2 0
094200940922     C                   MOVE      F£6(I)        FLE
094300940922     C     FLE           IFGT      *ZEROS
094400940919      *  POSIZIONAMENTO
094500940922     C     D82GES        IFEQ      'P'
094600950208      * SE GESTISCO CARICAMENTO X CLIENTE CARICO SOLO
094700950208      * SE CLIENTE E' DELLA FILIALE CHE STO ESAMINANDO
094800940922      * PARTENZA
094900940922     C     VPAR          IFEQ      2
095000950208     C                   MOVEL     SCM           WFILC             3 0
095100050215     C     kSCM          SETLL     tigcp07L                           20
095200940919     C                   ELSE
095300050324     C     kflefas       SETLL     tigcp06L                           20
095400940919     C                   ENDIF
095500940922     C                   ELSE
095600940922      * ARRIVO PER DISPOSIZIONE
095700050215     C     KGCA3         SETLL     tigcp24L                           20
095800940922     C                   ENDIF
095900940919      *  LETTURA
096000940922     C     *IN20         DOWEQ     '0'
096100940922     C     D82GES        IFEQ      'P'
096200940922      * PARTENZA
096300940922     C     VPAR          IFEQ      2
096400050215     C     kSCM          READE     tigcp07L                             1620
096500050215     c                   move      gcplnp        gcplnpx           3
096600050215     c     gcplnpx       lookup    f£6                                    35
096700020821     c  n35              iter
096800940919     C                   ELSE
096900050215     C     FLE           READE     tigcp06L                             1620
097000050324      * forzata uscita dal loop per non leggere rekord inutili e ottimizzare
097100050324      * i tempi di risposta
097200050324     c                   if        gcpfas >= 30
097300050324     c                   leave
097400050324     c                   endif
097500050324
097600940919     C                   ENDIF
097700940919      *
097800940919     C                   ELSE
097900940922      * ARRIVO  X DISPOSIZIONE
098000050215     C     KGCA3         READE     tigcp24L                             1620
098100940922     C                   ENDIF
098200940922     C  N20
098300940922     CANN16              DO
098400051005     c                   movel     gcpflr        dgcpflr
098500940919     C* SE SIAMO IN ARR. ED ESECUZIONE CONSIDERO LE GIACENZE CON
098600940919     C* FASE = 31 O 32 E CON DATA ESEGUIBILITA DISPOSIZ. <= VDED
098700940919     C     D82GES        IFEQ      'A'
098800050215     C     GCPFAS        IFEQ      30
098900911030     C     GCPFRG        ANDEQ     0
099000050215     C     GCPFAS        OREQ      31
099100911023     C     GCPFRG        ANDEQ     0
099200050215     C     GCPFAS        OREQ      36
099300050215     C     GCPFRG        ANDEQ     0
099400050408     C     GCPDED        IFLE      AMGDED
099500950216     C     GCPDED        ANDNE     0
099600950216     C     GCPDDM        ORLE      AMGDED
099700050408     C     GCPDED        ANDEQ     0
099800051005     C     D§GCPAPP      ANDne     'A'
099900051006     C     vpar          ANDne     4
100000051006     c* appuntamento
100100051006     C     GCPDDM        ORnE      0
100200051006     C     GCPDED        ANDEQ     0
100300051006     C     D§GCPAPP      ANDeq     'A'
100400051006     C     vpar          ANDeq     4
100500911023     C                   EXSR      WTR
100600940922     C                   END
100700940922     C                   END
100800940922     C                   ENDIF
100900941007     C* SE SIAMO IN PART. CONSIDERO LE GIACENZE CON FASE >= 20 E <=30
101000990628     C* DATA DISPOSIZIONE MITTENTE = 0   VEDI V.L. FIGCP09L
101100940922     C     D82GES        IFEQ      'P'
101200020821      * SOLO QUELLE DI UN CLIENTE e 35 on linea di partenza in gestione
101300940922     C     VPAR          IFEQ      2
101400950208     C     GCPFAS        IFEQ      20
101500950208     C     GCPDDM        ANDEQ     0
101600960606     C     GCPFRG        ANDEQ     0
101700950327     C     GCPFAS        OREQ      25
101800950327     C     GCPDDM        ANDEQ     0
101900960606     C     GCPFRG        ANDEQ     0
102000950327     C     GCPFAS        OREQ      26
102100950327     C     GCPDDM        ANDEQ     0
102200960606     C     GCPFRG        ANDEQ     0
102300911023     C                   EXSR      WTR
102400950208     C                   END
102500940922     C                   ELSE
102600940922      * TUTTE
102700911023     C                   EXSR      WTR
102800940922     C                   ENDIF
102900940919     C                   ENDIF
103000940922     C                   END
103100940922     C                   END
103200940922     C                   ENDIF
103300940922     C                   ENDDO
103400911010     C                   ENDSR
103500911017     C*-----------------------------*
103600911017     C* SCRITTURA RECORD NEL SUBFILE
103700911017     C*-----------------------------*
103800911010     C     WTR           BEGSR
103900990702     C*
104000990702     C                   SETOFF                                       52
104100950118     C                   MOVEL     *BLANKS       VMSG1
104200911023     C                   Z-ADD     GCPFGC        VFGC1
104300911023     C                   Z-ADD     GCPAGC        VAGC1
104400911023     C                   Z-ADD     GCPNGC        VNGC1
104500920110     C                   MOVEL     GIOR          VMGC1
104600920110     C                   MOVE      MESE          VMGC1
104700911023     C                   MOVEL     GCPCMC        VCMC1
104800920318     C* PRELEVO MITT. E DESTINATARIO DALLA BOLLA IN PARTENZA
104900940922     C     D82GES        IFEQ      'P'
105000050215     C                   Z-ADD     GCPAAS        VAAS1
105100050215     C                   Z-ADD     GCPNRS        VNRS1
105200050215     C                   Z-ADD     GCPNSP        VNSP1
105300050215     C                   Z-ADD     GCPLNP        VLNP1
105400940920     C     KBLP          CHAIN     FNBLP01L                           71
105500941019     C  N71              MOVEL     BLPAAS        AMGDSP            8 0
105600941019     C  N71              MOVE      BLPMGS        AMGDSP            8 0
105700940919     C  N71              MOVEL     BLPRSM        VRSM1
105800080603     c  n71              exsr      vediddt
105900960123     C*  CONTROLLO SE ESISTE DESTINATARIO FINALE
106000960123     C     *IN71         IFEQ      '0'
106100960123     C                   CLEAR                   FNLSN0
106200960123     C                   Z-ADD     BLPAAS        DN0AAS
106300960123     C                   Z-ADD     BLPLNP        DN0LNP
106400960123     C                   Z-ADD     BLPNRS        DN0NRS
106500960123     C                   Z-ADD     BLPNSP        DN0NSP
106600960123     C                   MOVEL     'R'           DN0TRC
106700960123     C                   MOVEL     KPJBU         WSVKPJ          256
106800960123     C                   MOVEL     FNLSN0        KPJBU
106900960123     C                   CALL      'FNLSN0R'
107000960123     C                   PARM                    KPJBA
107100960123     C                   MOVEL     KPJBU         FNLSN0
107200960123     C                   MOVEL     WSVKPJ        KPJBU
107300960123     C     DN0TRV        IFEQ      'S'
107400960123     C                   MOVEL     DN0RSD        VRSD1
107500960123     C                   ELSE
107600960123     C                   MOVEL     BLPRSD        VRSD1
107700960123     C                   END
107800960123     C                   END
107900940922     C* SE SIAMO IN PARTENZA CALCOLO CON I GIORNI DI RIENTRO SE LA
108000940922     C* SPEDIZIONE DEVE RIENTRARE
108100940922     C                   EXSR      RIENTR
108200960209      * GESTIONE PARTICOLARITA GIACENZE
108300960209      *                        LA VISUALIZZO SEMPRE
108400950102     C                   SETOFF                                       32
108500031022      * forza "GR" nella particolarità per poter gestire
108600031022      * il reso automaticamente
108700031024      * e si memorizza se non c'era sul file
108800031024     c                   move      '*'           c_eraGGA          1
108900031022     C                   if        BLPGGA = *blank
109000031024     c                   clear                   c_eraGGA
109100031022     c                   eval      BLPGGA = 'GR'
109200031022     c                   end
109300940926     C     BLPGGA        IFNE      *BLANK
109400940926     C                   Z-ADD     1             X
109500940926     C     BLPGGA        LOOKUP    C7Q(X)                                 32
109600940926     C     *IN32         IFEQ      '1'
109700960209     C                   MOVEL     BLPGGA        VMSG1
109800031023      *  Non deve visualizzare "GR" se non è in fase 26
109900031023      *  avendo sostituito il blank con il GR per poter
110000031023      *  decodificare la tabella.
110100031023     c                   if        vmsg1 = 'GR' and gcpFAS <> 26
110200031023     C                   clear                   VMSG1
110300031024     c                   setoff                                       32
110400031023     c                   end
110500031024     c                   if        vmsg1 ='GR' and c_eraGGA = *blank
110600031024     C                   clear                   VMSG1
110700031024     c                   end
110800940926     C                   MOVE      D7Q(X)        DS7Q
110900940926     C                   END
111000940926     C                   ENDIF
111100940926      * DISPOSIZ. RICEVUTE DAL CLIENTE :SONO NELLE NOTE
111200940922     C     GCPFAS        IFEQ      25
111300940926     C                   MOVEL     '*'           VDIS1
111400940922     C                   END
111500940926      * SE FASE =26 HO RICEVUTO UNA RIAPERTURA "FINTA" X IL
111600940926      * CODICE PARTIC. 5A =NON RIAPRIRE MA RENDERE ...
111700940926     C     GCPFAS        IFEQ      26
111800940926     C                   MOVEL     §7Q5DI        VDIS1
111900950104     C                   SETON                                        32
112000940926     C                   END
112100940922     C                   ELSE
112200961220     C*  CONTROLLO SE CI SONO NOTE GIACENZA PER FAX
112300961220     C                   MOVEL     *BLANKS       VDIS1
112400050215     C                   Z-ADD     GCPAASa       VAAS1
112500050215     C                   Z-ADD     GCPNRSa       VNRS1
112600050215     C                   Z-ADD     GCPNSPa       VNSP1
112700050215     C                   Z-ADD     GCPLNPa       VLNP1
112800961220     C                   Z-ADD     GCPAGC        KAGC
112900961220     C                   Z-ADD     GCPFGC        KFGC
113000961220     C                   Z-ADD     GCPNGC        KNGC
113100961220     C                   Z-ADD     GCPFRG        KFRG
113200961220     C                   Z-ADD     35            KFAS
113300050215     C     KGNA          CHAIN     tignp01L                           36
113400961220     C  N36              MOVEL     'F'           VDIS1
113500941007      *    A R R I V O
113600050215     C     Karb          CHAIN     FNARB01L                           71
113700940919     C  N71              MOVEL     ARBRSM        VRSM1
113800940919     C  N71              MOVEL     ARBRSD        VRSD1
113900080603     c  n71              exsr      vediddt
114000940926      * GESTIONE PARTICOLARITA GIACENZE
114100950102     C                   SETOFF                                       32
114200940926     C     ARBGGA        IFNE      *BLANK
114300940926     C                   Z-ADD     1             X
114400940926     C     ARBGGA        LOOKUP    C7Q(X)                                 32
114500940926     C     *IN32         IFEQ      '1'
114600940926     C                   MOVE      D7Q(X)        DS7Q
114700960209     C                   MOVEL     ARBGGA        VMSG1
114800941007      *
114900940926     C                   END
115000940926     C                   ENDIF
115100940922     C                   ENDIF
115200940919     C   71              MOVEL     *ALL'?'       VRSM1
115300940919     C   71              MOVEL     *ALL'?'       VRSD1
115400010920      *
115500010920      * FILTRI SOLO IN PARTENZA
115600050512 1   C     D82GES        IFEQ      'P'
115700090819      *
115800050512 2   C     VPAR          IFNE      3
115900010920     C     VDIS1         OREQ      'R'
116000090819      *
116100050512 3   C     VPAR          IFEQ      4
116200010920     C     VDIS1         ANDEQ     '*'
116300090819      *
116400050512mm   C     VPAR          orEQ      3
116500050512mm   C     gcpfas        ANDEQ     20
116600090819      *
116700050512mm   C     VPAR          orEQ      5
116800050512mm   C     gcpfas        ANDEQ     26
116900090819      *
117000010920     C     VPAR          ORNE      4
117100050512mm   C     VPAR          andne     3
117200050512mm   C     VPAR          andne     5
117300090819      *
117400090819      *   Attenzione  SOLO per la scelta "2" x Specifico CLIENTE:
117500090819      **     se si tratta di disposizioni di RESO e siamo
117600090819      **    nel periodo delle trazioni ridotte, deve essere
117700090819      **    protetto il campo di scelta a meno che non ci sia l'eccezione
117800090819      **    sul cliente nella tab.7U. (il cliente vuole i resi anche nel
117900090819      **    periodo delle trazioni ridotte)
118000090819     C                   IF          VDIS1 = 'R' and Vpar = 2
118100090819     c                                and
118200090819     c                               AMGUDT >= §2GDPI and AMGUDT <= §2GDPF
118300090819      *
118400090819      * Protegge x default il campo scelta
118500090819     C                   SETON                                        52
118600090819      *
118700090819      * Controlla se il cliente vuole comunque il reso
118800090819     C     BLPksc        lookup    k7u                                    88
118900090819      * se non l'ho trovato con KSC provo con CCM
119000090819     c  n88BLPccm        ifne      *zeros
119100090819     C     BLPccm        lookup    k7u                                    88
119200090819     c                   endif
119300090819      * Se l'ha trovato sprotegge la scelta per eseguire il reso    ======
119400090819     C   88              SETOFF                                       52
119500090819      *                                                             ======
119600090819     C                   END
119700090819      *
119800090819      * Memorizza se la scelta deve essere protetta o no
119900090819     c                   eval      H1IN52 = *in52
120000090819      *
120100950224     C                   ADD       1             NRR
120200050512     C                   WRITE     FG82SF
120300050512 3   C                   END
120400050512 2   C                   END
120500090819      *
120600050512 x1  C                   ELSE
120700090819      * IN ARRIVO
120800090819      * Memorizza se la scelta deve essere protetta o no
120900090819     c                   eval      H1IN52 = *in52
121000090819      *
121100950224     C                   ADD       1             NRR
121200950224     C                   WRITE     FG82SF
121300050512 1   C                   END
121400090819      *
121500040922      * forzato limite caricamento a 9000 rekord
121600040922     c                   if        nrr > 9000
121700040922     c                   seton                                        20
121800040922     c                   endif
121900950224     C*
122000050512     c                   z-add     nrr           wnrr              4 0
122100900509     C                   ENDSR
122200911017     C*----------------*
122300911010     C* PULIZIA SUBFILE
122400911017     C*----------------*
122500911010     C     PULIZ         BEGSR
122600911010     C                   SETON                                        70
122700911023     C                   WRITE     FG82CT
122800911023     C                   Z-ADD     0             NRR               4 0
122900911010     C                   SETOFF                                       70
123000911010     C                   EXSR      PULVID
123100911010     C                   ENDSR
123200911010     C*
123300940922     C*----------------*
123400940922     C* CALCOLO SE SPEDIZ. DA RIENTRARE
123500940922     C*----------------*
123600940922     C     RIENTR        BEGSR
123700940922     C** RICERCO SU TARIFFA I GG DI RIENTRO DEL CLIENTE
123800940922     C                   Z-ADD     0             WGGR
123900940922     C                   Z-ADD     GCPSCM        APPO              4 0
124000950116     C                   MOVE      GCPSCM        KKSC
124100950116     C                   MOVE      BLPCTR        KCTR
124200950116     C                   MOVEL     ' '           WTROVA            1
124300020227     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" E TARIFFA DI
124400020227     C* CARTELLO
124500020227     C                   CLEAR                   TRUL27
124600021118     C                   MOVE      BLPctr        I27ctb
124700020227     C                   MOVEL     BLPTSP        I27TSP
124800020227     C                   MOVE      BLPLNA        I27LNA
124900020227     C                   MOVE      BLPLNP        I27LNP
125000020227     C                   MOVE      KKSC          I27CLI
125100020227     C                   MOVE      AMGDSP        I27DTA
125200020227     C                   CALL      'TRUL27R'
125300020227     C                   PARM                    TRUL27
125400020227     C*
125500950116     C                   Z-ADD     1             Y                 3 0
125600940922     C     APPO          IFNE      8888
125700940922     C     APPO          ANDNE     9999
125800950116     C     APPO          ANDNE     *ZEROS
125900950116     C*  Cerco progressivo tariffa in vigore su
126000150514     C                   CLEAR                   fnlv59ds
126100150514     C                   MOVEL     ' '           ilv59TBO
126200150514     C                   Z-ADD     AMGDSP        ilv59DSP
126300150514     C                   MOVE      BLPCTR        ilv59CTR
126400150514     C                   MOVE      KKSC          ilv59KSC
126500150514     C                   MOVE      KCI           ilv59KCI
126600000202     C                   MOVEL     KKSC          W0030             3 0
126700150514     C                   MOVEL     O27FIE        ilv59FIE
126800150514     C                   MOVEL     BLPTSP        ilv59TSP
126900150514     C                   MOVEL     'S'           ilv59CON
127000150514     c                   eval      ilv59ta2='B'
127100150514     c                   eval      ilv59blo='S'
127200150514     C                   CALL      'FNLV59R'
127300150514     C                   PARM                    fnlv59ds
127400950116     C*
127500150514     C     olv59ERR      IFEQ      *BLANKS
127600150514     C     olv59tks      ANDEQ     'S'
127700950116     C*  Se ho trovato tariffa in vigore mi posiziono sul file
127800950116     C*  delle tariffe giacenze con il proggressivo giusto
127900150514     C                   MOVE      olv59PRG      KPRG
128000150514     C                   MOVE      ilv59KSC      KKSC
128100150514     C                   MOVE      olv59CTR      KCTR
128200990730     C     KTGC          CHAIN     TITGC01L                           31
128300950116     C     *IN31         IFEQ      '0'
128400950116     C     TGCGGR        ANDNE     *ZEROS
128500950116     C                   MOVE      TGCGGR        WGGR
128600950116     C                   MOVEL     'S'           WTROVA
128700950116     C                   END
128800950116     C                   END
128900950116     C                   END
129000950116     C*  Se non trovato gg. rientro su tariffa cliente li cerco
129100950116     C*  sulla tariffa di cartello
129200950116     C     WTROVA        IFNE      'S'
129300020227     C                   MOVE      O27KSC        KKSC
129400020227     C                   MOVE      O27CTR        KCTR
129500020227     C                   MOVE      O27PRG        KPRG
129600990730     C     KTGC          CHAIN     TITGC01L                           31
129700950116     C     *IN31         IFEQ      '0'
129800950116     C     TGCGGR        ANDNE     *ZEROS
129900950116     C                   MOVE      TGCGGR        WGGR
130000950116     C                   MOVEL     'S'           WTROVA
130100950116     C                   END
130200950116     C                   END
130300970619     C*  Se non ho impostato i gg rientro li prendo dalla tabella 2G
130400950116     C     WTROVA        IFNE      'S'
130500950116     C                   MOVE      §2GGGL        WGGR
130600950116     C                   END
130700020227     C* Per Estero prendo minore fra gg.rientro estero e tariffa
130800020227     C* Occorre però distinguere fra Estero, DPD, E FEDEX in quanto c'è
130900020227     C* tabella "2G" CON i LORO gg.
131000020227     C     O27FIE        IFNE      'I'
131100020227     C                   SELECT
131200020227      * Se è da DPD
131300050210     C***  O27FIE        WHENEQ    'D'
131400050210     C     o27ntw        WHENEQ    'DPD'
131500010921     C     WGGR          IFGT      §2GGDP
131600010921     C                   MOVE      §2GGDP        WGGR
131700010921     C                   END
131800020227      * Se è FEDEX
131900050210     C***  O27FIE        WHENEQ    'F'
132000050210     C     o27ntw        WHENEQ    'FED'
132100020227     C     WGGR          IFGT      §2GGFE
132200020227     C                   MOVE      §2GGFE        WGGR
132300020227     C                   END
132400020227      * Se è estero
132500050210     C*****              OTHER
132600050210     C     o27ntw        WHENEQ    'EEX'
132700050210     C     o27ntw        oreq      'EUP'
132800961213     C     WGGR          IFGT      §2GGRE
132900961204     C                   MOVE      §2GGRE        WGGR
133000010921     C                   END
133100020227     C                   ENDSL
133200961213     C                   END
133300040217      *
133400050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
133500050210     C*   sui giorni da prendere
133600050210     C     o27fie        ifne      'I'
133700050210     C     o27ntw        andeq     'EEX'
133800050210     C                   MOVEL     '20'          COD
133900050210     C                   MOVEL     *blank        KEY
134000050210     C                   MOVEL     Kksc          KEY
134100050210     c     ktab          chain     tabel00f
134200050210     c                   if        %found(tabel00f)
134300050210     c                   movel     tbluni        ds20
134400050210     C                   Z-ADD     §20GRE        wGGR
134500050210     c                   endif
134600050210     c                   end
134700940922      *
134800940922     C* CALCOLO SE LA DATA E' INFERIORE ALLA DATA ODIERNA E QUINDI LA
134900940922     C* MERCE SAREBBE DA RIENTRARE
135000050215     C                   MOVE      GCPDUR        WDAT8             8 0
135100950622     C                   MOVE      WDAT8         G08INV
135200950622     C                   MOVE      '3'           G08ERR
135300950622     C                   CALL      'XSRDA8'
135400950622     C                   PARM                    WLBDA8
135500950622     C     G08TGI        ADD       WGGR          GI8TGI
135600950622     C                   CALL      'XSRGI8'
135700950622     C                   PARM                    WGIDA8
135800020228     c* data + gg rientro è < di oggi
135900950622     C                   Z-ADD     GI8INV        WDAT8
136000940922     C     WDAT8         IFLT      AMGUDT
136100940926     C                   MOVEL     'R'           VDIS1
136200990702     C* Se sono nel periodo di esclusione dei rientri automatici
136300990702     C* proteggo il record del subfile per la selezione
136400990729     C     WDAT8         IFGE      §2GDPI
136500990729     C     WDAT8         ANDLE     §2GDPF
136600990729     C     AMGUDT        ANDGE     §2GDPI
136700990729     C     AMGUDT        ANDLE     §2GDPF
136800990702     C     D82GES        ANDEQ     'P'
136900990729     C* OPPURE
137000990729     C     BLPGGA        OREQ      'G0'
137100990729     C     AMGUDT        ANDGE     §2GDPI
137200990729     C     AMGUDT        ANDLE     §2GDPF
137300990729     C     D82GES        ANDEQ     'P'
137400020108      *
137500020108      *  NON DEVE PROTEGGERE SE SI È ENTRATI SCEGLIENDO
137600020108      *   L'OPZIONE (4)
137700020108     C     VPAR          IFNE      4
137800990702     C                   SETON                                        52
137900020108     C                   END
138000020108      *
138100990702     C                   END
138200940922     C                   ELSE
138300940926     C                   MOVEL     *BLANKS       VDIS1
138400940922     C                   ENDIF
138500940922     C                   ENDSR
138600911017     C*--------------------*
138700911017     C* PULIZIA CAMPI VIDEO
138800911017     C*--------------------*
138900911017     C     PULVID        BEGSR
139000050512     C                   MOVEL     *BLANKS       VSEL
139100911023     C                   MOVE      *ZEROS        VFGC1
139200911023     C                   MOVE      *ZEROS        VAGC1
139300911023     C                   MOVE      *ZEROS        VMGC1
139400911023     C                   MOVE      *ZEROS        VNGC1
139500911023     C                   MOVE      *BLANKS       VCMC1
139600911023     C                   MOVE      *ZEROS        VAAS1
139700911023     C                   MOVE      *ZEROS        VNRS1
139800911023     C                   MOVE      *ZEROS        VNSP1
139900911023     C                   MOVE      *BLANKS       VRSM1
140000911023     C                   MOVE      *BLANKS       VRSD1
140100940926     C                   MOVE      *BLANKS       VDIS1
140200940926     C                   MOVE      *BLANKS       VMSG1
140300911017     C                   ENDSR
140400911022     C*----------------------*
140500911022     C* CONTROLLI 1° VIDEATA
140600911022     C*----------------------*
140700911022     C     CTRVID        BEGSR
140800080424      *
140900940926     C                   SETOFF                                       434445
141000940926     C                   SETOFF                                       464748
141100940926     C                   SETOFF                                       495051
141200080424     C                   SETOFF                                       280654
141300080424      *
141400050513      *accendo indicatore condizionamento opzione 7 vale solo per i resi
141500050513     c                   if        vpar = 3 or vpar = 5
141600050513     c                   seton                                        06
141700050513     c                   endif
141800080424      *
141900080424      * controlla se inserita opzione (6 - DPD Parcel) quando non è
142000080424      *  abilitato a farlo : errore
142100080424      *    Il parcel è solo utilizzabile in Partenza x P.O.DPD
142200080424     c                   if        vpar = 6
142300080424     c                   if         D82GES = 'A'  or
142400080424     c                             (D82GES = 'P' and *in84)
142500080424     C                   SETON                                        2843
142600080424     C                   MOVEL     MSG(19)       $MSG
142700080424     c                   endif
142800080424     c                   endif
142900080424      *
143000911022     C* IN PARTENZA
143100940919     C     D82GES        IFEQ      'P'
143200940919      *
143300911022     C     VPAR          IFEQ      2
143400911022     C* RICERCA CLIENTE
143500911022     C                   MOVEA     VSCM          KSC§
143600940920     C     '?'           LOOKUP    KSC§                                   21
143700940920     C   21              DO
143800911023     C                   Z-ADD     CODUT         CODUT§            1 0
143900911023     C                   Z-ADD     KCI           KCI§              4 0
144000911030     C                   MOVEL     *BLANKS       RICRAG           48
144100911022     C                   MOVEL     VRAG          RICRAG
144200911022     C                   Z-ADD     0             RICKSC            7 0
144300911022     C                   Z-ADD     0             PARSTA            1 0
144400911022     C                   CALL      'XALFA1'
144500911022     C                   PARM                    PARDUT
144600911022     C                   PARM                    CODUT§
144700911022     C                   PARM                    RICRAG
144800911022     C                   PARM                    KCI§
144900911022     C                   PARM                    RICKSC
145000911022     C                   PARM                    PARSTA
145100961016     C                   PARM                    PARFLR           90
145200911022     C     PARSTA        IFNE      -1
145300911030     C                   MOVE      RICKSC        VSCM
145400911030     C                   MOVEL     RICRAG        VRAG
145500911022     C                   ELSE
145600911220     C                   MOVE      *ZEROS        VSCM
145700911030     C                   MOVE      *BLANKS       VRAG
145800911022     C                   END
145900911022     C                   END
146000911022     C* CONTROLLO ESISTENZA CLIENTE
146100911030     C                   MOVE      VSCM          SCM
146200940926     C     KACO          CHAIN     CNACO000                           46
146300940926     C   46              MOVEL     MSG(2)        $MSG
146400940926     C   46              SETON                                        28
146500940919     C                   ENDIF
146600080424      *
146700940926      * SE IMMESSO NR.GIAC. O COD.CLIENTE OBBLIGO   VPAR
146800940926     C     VPAR          IFEQ      *ZEROS
146900940926     C     VNGC          IFNE      *ZEROS
147000940926     C     VSCM          ORNE      *BLANK
147100080424     C     Vparcel       orNE      *Blank
147200940926     C                   SETON                                        2843
147300940926     C                   MOVEL     MSG(4)        $MSG
147400940926     C                   ENDIF
147500940926     C                   ENDIF
147600080424      *
147700080424      * Se scelto il Parcel Nr.
147800080424     c                   if        Vpar = 6
147900080424      *  controlla esistenza del Parcel con il quale agganciare la giacenza
148000080424     C                   if        Vparcel = *blank
148100080424     C                   SETON                                        2854
148200080424     C                   MOVEL     MSG(20)       $MSG
148300080424     c                   else
148400080424      *
148500080424      * Controlla esistenza del Parcel immesso incorciando i dati
148600080424      * del Fiar4 con le giacenze - usando il numero spedizione
148700080424      * come chiave di Join:
148800080424     C/exec sql
148900080424     c+ Select gcpagc, gcpfgc, gcpngc into :vagc, :vfgc, :vngc
149000080424     c+   from Fiar400f join tigcp00f
149100080424     c+   on  ar4aas = gcpaas and ar4lnp = gcplnp and
149200080424     c+       ar4nrs = gcpnrs and ar4nsp = gcpnsp
149300080424     c+   where ar4trc = 'I' and substr(ar4not, 1, 14) = :vParcel
149400080424     c+     and GCPfrg = 0
149500080424     C/end-exec
149600080424     c                   if        SqlCOD < 0 or sqlCOD = 100
149700080424     C                   SETON                                        2854
149800080424     C                   MOVEL     MSG(20)       $MSG
149900080424     c                   else
150000080424      * se qui : ha trovato la giacenza con il Parcel
150100080424      *          e ha caricato la chiave della Giacenza
150200080424     c                   end
150300080424      *
150400080424     c                   endif
150500080424     c                   endif
150600080424      *
150700940919     C                   ENDIF
150800911022     C* IN ARRIVO
150900940919     C     D82GES        IFEQ      'A'
151000911022     C     VSEL1         IFEQ      *BLANKS
151100911022     C     VSEL2         ANDEQ     *BLANKS
151200940926     C                   SETON                                        444528
151300940919     C                   MOVEL     MSG(3)        $MSG
151400911022     C                   END
151500940919     C  N28VPAR          IFEQ      *ZEROS
151600010920     C     VPAR          ORGT      4
151700940926     C                   SETON                                        4328
151800940919     C                   MOVEL     MSG(4)        $MSG
151900911022     C                   END
152000940926     C  N28              DO
152100940926     C     VSEL1         COMP      'X'                                    50
152200940926     C     VSEL2         COMP      'X'                                    51
152300940926     C   50
152400940926     CAN 51              SETON                                        444528
152500941012     C   28              MOVEL     MSG(5)        $MSG
152600940926     C                   ENDDO
152700940919     C  N28
152800940919     CAN 50VPAR          IFEQ      3
152900940926     C                   SETON                                        4428
153000940926     C                   MOVEL     MSG(6)        $MSG
153100911022     C                   END
153200051006     C  N28
153300051006     CAN 50VPAR          IFEQ      4
153400051006     C                   SETON                                        4428
153500051006     C                   MOVEL     MSG(18)       $MSG
153600051006     C                   END
153700940919     C  N28VPAR          IFEQ      1
153800911022     C     VFGC          IFEQ      0
153900911022     C     VNGC          OREQ      0
154000940926     C                   SETON                                        4828
154100940926     C                   MOVEL     MSG(7)        $MSG
154200911022     C                   END
154300940926     C                   END
154400940919     C  N28VPAR          IFEQ      2
154500911022     C     VLNP          IFEQ      0
154600911022     C     VNSP          OREQ      0
154700940926     C                   SETON                                        4628
154800940926     C                   MOVEL     MSG(7)        $MSG
154900911022     C                   END
155000911022     C                   END
155100940919     C  N28VPAR          IFEQ      3
155200940919     C     VDIS          ANDEQ     *BLANKS
155300940926     C                   SETON                                        4728
155400940926     C                   MOVEL     MSG(7)        $MSG
155500911022     C                   END
155600940926      * CONTROLLO DATA ESEGUIBILTA DISP.
155700940926     C  N28VPAR          IFEQ      3
155800940926     C                   Z-ADD     VDED          G08DAT
155900940926     C                   MOVE      *ZEROS        G08INV
156000940926     C                   MOVEL     *BLANK        G08ERR
156100940926     C                   CALL      'XSRDA8'
156200940926     C                   PARM                    WLBDA8
156300940926     C     G08ERR        IFEQ      '1'
156400940926     C                   SETON                                        2849
156500940926     C                   MOVEL     MSG(13)       $MSG
156600940926     C                   ELSE
156700940926     C                   Z-ADD     G08INV        AMGDED            8 0
156800940926     C                   ENDIF
156900051006     c                   else
157000051006     c                   clear                   amgded
157100940926     C                   ENDIF
157200940919     C                   ENDIF
157300080424      *
157400911022     C                   ENDSR
157500050524      **********************************************************************
157600050524      * Subroutine - Sort
157700050524      *   This subroutine sorts the subfile records.
157800050524     C     Sort          BEGSR
157900050524     C                   CLEAR                   vrsm1
158000050524     C                   CLEAR                   vrsm1w
158100050613     C                   EVAL      nrr = 0
158200050524
158300050524      * Initialize the key fields to sort on. There is one extra field not in
158400050524      * the subfile -- Selected -- that is added to the record. This field
158500050524      * is used to place selected records in front of nonselected records.
158600050524
158700050524     C                   CLEAR                   QLGSCB
158800050524     C                   CLEAR                   QLGSCB00
158900050524
159000050524     ** Gestione della scelta ordinamento.
159100050524     C                   SELECT
159200050525     ** Ordinamento per mittente
159300050525     C                   WHEN      WrkSort = mittente
159400050524     ** 1 campo chiave.
159500050524     C                   EVAL      QLGNBRK    = 1
159600050525     ** il mittente è
159700050524     ** a posizone 42, 13 byte, char, ascending.
159800050525     C                   EVAL      QLGSP      = 40
159900050524     C                   EVAL      QLGSS      = %SIZE(vrsm1)
160000050524     C                   EVAL      QLGDT      = Carattere
160100050524     C                   EVAL      QLGSO      = Ascendente
160200050524     C                   EVAL      QLGKL(1)   = QLGSKL
160300050525     ** Al prossimo F11 ordino per giacenza
160400050525     C*                  EVAL      WrkSort = giacenza
160500050524     ** Ordinamento per Codice
160600050525     C                   WHEN      WrkSort = giacenza
160700050524     C                   EVAL      QLGNBRK    = 1
160800050524     ** Il codice è
160900050525     ** a posizione 12, 7 byte, ascending
161000050525     C                   EVAL      QLGSP      = 12
161100050525     C                   EVAL      QLGSS      = %SIZE(vngc1)
161200050524     C                   EVAL      QLGDT      = Carattere
161300050524     C                   EVAL      QLGSO      = Ascendente
161400050524     C                   EVAL      QLGKL(1)   = QLGSKL
161500050525     ** Al prossimo F11 ordino per mittente
161600050525     C*                  EVAL      WrkSort = Mittente
161700050524     C                   ENDSL
161800050524      * Load other sort parameters.
161900050524     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
162000050524     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
162100050524     C                   EVAL      QLGRT     = 8
162200050524     C                   EVAL      QLGOKL    = 80
162300050524     C                   EVAL      QLGLKE    = 16
162400050524     C                   EVAL      QLGLSS    = 290
162500050524      * Initialize Sort I/O API fields.
162600050524     C                   EVAL      QLGRL00  = QLGRL
162700050524     C                   EVAL      QLGRC00  = 1
162800050524     C                   CLEAR                   QUSEI
162900050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
163000050524      * First step - Initialize the sort routine.
163100050524     C                   CALL      'QLGSORT'
163200050524     C                   PARM                    QLGSCB
163300050524     C                   PARM                    NotUsed
163400050524     C                   PARM                    NotUsed
163500050524     C                   PARM                    SizeList
163600050524     C                   PARM                    ReturnSize
163700050524     C                   PARM                    QUSEC
163800050524      * Next step - Write records to I/O routine.
163900050524     C                   EVAL      QLGRT00 = Put
164000050613     C                   DO        RrnLast       nrr
164100050613     C     nrr           CHAIN     fg82sf
164200050524     ** Solo le righe con Selected = 'Y' sono riordinate,
164300050524     ** quindi per fare un ordinamento di tutte le righe
164400050524     ** metto 'Y' sempre.
164500050524     C                   EVAL      Selected = 'Y'
164600050524     C                   CLEAR                   QUSEI
164700050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
164800050524     C                   CALL      'QLGSRTIO'
164900050524     C                   PARM                    QLGSCB00
165000050524     C                   PARM                    SflRcd
165100050524     C                   PARM                    NotUsed
165200050524     C                   PARM                    SizeList
165300050524     C                   PARM                    NotUsed
165400050524     C                   PARM                    QUSEC
165500050524     C                   ENDDO
165600050524      * Next step - Signal end of input, clear subfile for reload.
165700050524     C                   EVAL      QLGRT00 = EndPut
165800050524     C                   CLEAR                   QUSEI
165900050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
166000050524
166100050524     C                   CALL      'QLGSRTIO'
166200050524     C                   PARM                    QLGSCB00
166300050524     C                   PARM                    SflRcd
166400050524     C                   PARM                    NotUsed
166500050524     C                   PARM                    SizeList
166600050524     C                   PARM                    NotUsed
166700050524     C                   PARM                    QUSEC
166800050524     c                   seton                                        70
166900050524     C                   WRITE     fg82ct
167000050524     C                   setoff                                       70
167100050524      * Final step - Write the records back to the subfile.
167200050524     C                   EVAL      QLGRT00 = Get
167300050613     C                   DO        RrnLast       nrr
167400050524     C                   CLEAR                   QUSEI
167500050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
167600050524     C                   CALL      'QLGSRTIO'
167700050524     C                   PARM                    QLGSCB00
167800050524     C                   PARM                    NotUsed
167900050524     C                   PARM                    SflRcd
168000050524     C                   PARM                    QLGRL00
168100050524     C                   PARM                    NotUsed
168200050524     C                   PARM                    QUSEC
168300050525     c                   if        vmsg1 <> *blank
168400050525     c                   seton                                        32
168500050525     c                   else
168600050525     c                   setoff                                       32
168700050525     c                   endif
168800090819     c                   eval        *in52 = H1IN52
168900050524     C                   WRITE     fg82sf
169000050524     C                   ENDDO
169100050524     C                   ENDSR
169200080603     c*-----------------------------------------------------------------
169300080603     c     vediddt       begsr
169400080603     c*-----------------------------------------------------------------
169500080603      * Flag DDT SI  ar4 trek A
169600080603     c                   clear                   vddts
169700080603     c                   clear                   dsbl4a
169800080603     c                   move      'A'           trek4
169900080603     c     kar4          chain     fiar401l
170000080603     c                   if        %found(fiar401l)
170100080603     c                   movel     AR4NOT        dsbl4a
170200080603     c                   if        §B4ABM = 'S' or §B4ABM = 'P' or
170300080603     c                             §B4ABM = 'C' or §B4ABM = 'Q'
170400080603     c                   eval      vddts = 'S'
170500080603     C                   endif
170600080603     C                   endif
170700080603     c                   endsr
170800050524     C*-----------------------------------------------------------------
170900940919     C*   I N Z S R
171000050524     C*-----------------------------------------------------------------
171100940919     C     *INZSR        BEGSR
171200940919     C     *ENTRY        PLIST
171300940919     C                   PARM                    KPJBA
171400940919      *
171500940919     C                   MOVEL     KPJBU         FNLG82
171600140123     C                   MOVEL     Knmus         knmus2
171700940920      *
171800940919     C                   MOVEL     NOMPGM        VPGM
171900940919     C                   Z-ADD     1             CODUT
172000940919     C                   CALL      'X§PARUT'
172100940919     C                   PARM                    UTEDSE
172200940919     C                   MOVEL     RAGUT         RSUT             20
172300940919     C                   MOVEL     RAGUT         PARDUT           30
172400060925     C**mm               MOVEL     REC80         CNCR80
172500140123     C                   MOVEL     KNMUS         §SEI
172600020228     c* una 2° livello
172700020730     C**** §PRF          COMP      'REM'                                  01
172800020730     C**N01              Z-ADD     SIMFEL        §FLE
172900060804     C**MM               Z-ADD     SIMpou        §FLE
173000940919      *
173100060804     c     *dtaara       define    §azute        azuteds
173200060804     c     *dtaara       define    §datiute      ddatiute
173300060804     c                   in(E)     *dtaara
173400060804    1c                   If        %error  or RSUT = *blanks
173500060804     c                   Clear                   Tibs34ds
173600060804     c                   Call      'TIBS34R'
173700060804     c                   Parm                    Tibs34ds
173800060804     c                   In        *dtaara
173900060804    1c                   EndIf
174000060804
174100060804     c                   Movel     UteFaf        Dute01
174200060804     c                   If        §Utefgia = *Blanks and kcdaz = 'XG83'
174300150911     c                   call      'XUTENOAB'
174400150911     c                   parm                    kpjba
174500060804     c                   seton                                        lr
174600060804     c                   Endif
174700060925     c                   z-add     dutpou        §fle
174800060925     c* TEST SE SONO IN PARTENZA O IN ARRIVO
174900940919     C     D82GES        IFEQ      'A'
175000940919     C                   SETON                                        05
175100050215     C                   OPEN      tignp01L
175200050215     C                   OPEN      tigcp21L
175300050215     C                   OPEN      tigcp02L
175400050215     C                   OPEN      tigcp24L
175500940920     C                   OPEN      FNARB01L
175600940920     C                   ELSE
175700050215     C                   OPEN      tigcp02L
175800050215     C                   OPEN      tigcp07L
175900050215     C                   OPEN      tigcp06L
176000941006     C                   OPEN      FNBLP01L
176100990730     C                   OPEN      TITGC01L
176200940920     C                   OPEN      CNACO00F
176300940920     C                   ENDIF
176400940919      * CARICO FILIALI IN GESTIONE
176500940919     C                   MOVEL     *BLANKS       KEY
176600940919     C                   MOVEL     §FLE          KEY
176700940922     C                   MOVEA     *ZEROS        F£6
176800020503     c                   if        *in05
176900020503     c                   clear                   trul06ds
177000020503     c                   eval      d06cod = '£6'
177100020503     c                   movel     §fle          d06key
177200020503     c                   movel     trul06ds      kpjbu
177300020503     c                   call      'TRUL06R'
177400020503     c                   parm                    kpjba
177500020503     c                   movel     kpjbu         trul06ds
177600020503     c                   movea     lin           f£6
177700020503     c                   else
177800140123     c                   clear                   trovata
177900940919     C  N05              MOVE      '£7'          COD
178000140102      * forzo caricamento filiali dpd per profilo mirato
178100140123     c                   if        %subst(knmus:1:3) = 'INT'
178200140123     c                   movel(p)  §mussei       key
178300140123     C     KTAB          CHAIN     TABEL                              14
178400140123     C  N14              MOVEA     TBLUNI        F£6
178500140123     c  n14              move      'X'           trovata
178600140123     c   14              movel(p)  §fle          key
178700140123     c                   endif
178800140123      *
178900140123     c                   if        trovata = *blank
179000940919     C     KTAB          CHAIN     TABEL                              15
179100940922     C  N15              MOVEA     TBLUNI        F£6
179200940922     C   15              MOVE      §FLE          F£6(1)
179300020503     c                   end
179400140123     c                   endif
179500940923      * CARICO FILIALI GESTITE SULL'AS
179600020503     C* N15              MOVEA     TBLUNI        F£1
179700020503     c                   clear                   trul06ds
179800020503     c                   eval      d06cod = '£1'
179900020503     c                   movel     §fle          d06key
180000020503     c                   movel     trul06ds      kpjbu
180100020503     c                   call      'TRUL06R'
180200020503     c                   parm                    kpjba
180300020503     c                   movel     kpjbu         trul06ds
180400020503     c                   movea     lin           f£1
180500950116      * CARICO DS 2G
180600950116     C                   MOVEL     *BLANKS       KEY
180700950116     C                   MOVEL     '1'           KEY
180800950116     C                   MOVE      '2G'          COD
180900950116     C                   CLEAR                   DS2G
181000950116     C     KTAB          CHAIN     TABEL                              15
181100950116     C  N15              MOVEL     TBLUNI        DS2G
181200940926      * CARICO TABELLE PARTICOLARITà GIACENZE
181300940926     C*---------------------------------------------------------------*
181400940926     C* Carico tabella 7Q
181500950116     C                   Z-ADD     0             X
181600950116     C                   MOVEL     '7Q'          COD
181700950116     C     KTAB2         CHAIN     TABEL                              31
181800950116     C     *IN31         DOWEQ     '0'
181900950116     C     X             ANDLE     50
182000950116     C     TBLFLG        IFEQ      ' '
182100950116     C                   ADD       1             X
182200950116     C                   MOVEL     TBLKEY        C7Q(X)
182300950118     C                   MOVEL     TBLUNI        D7Q(X)
182400950116     C                   END
182500950116     C     KTAB2         READE     TABEL                                  31
182600950116     C                   END
182700940926      *
182800940926     C*  Se non ho caricato tutti i dati gestisco errore
182900940926     C     *IN31         IFNE      '1'
183000940926     C                   SETON                                            28
183100950116     C                   MOVEL     MSG(14)       $MSG
183200940926     C                   END
183300950116      *
183400950116     C*  Se non ho caricato tutti i dati gestisco errore
183500950116     C     *IN31         IFNE      '1'
183600950116     C                   SETON                                            28
183700950116     C                   MOVEL     MSG(16)       $MSG
183800950116     C                   END
183900090819      *
184000090819     C*  Caricamento tabella 7U clienti che vogliono il reso anche nel
184100090819     C*                         periodo delle trazioni ridotte
184200090819     c                   clear                   SK_in_Capienza    1
184300090819     C                   Z-ADD     0             XX                4 0
184400090819     C                   MOVEL     '7U'          COD
184500090819     C     KTAB2         CHAIN     TABEL                              31
184600090819     C     *IN31         DOWEQ     '0'
184700090819     C     X             ANDLT     999
184800090819     C                   MOVE      TBLkey        tipoQ             1
184900090819     C                   MOVEL     TBLUNI        DS7U
185000090819     C     TBLFLG        IFEQ      ' '
185100090819     C     tipoQ         andeq     'Q'
185200090819     C     §7USRE        andeq     'S'
185300090819     C                   ADD       1             XX
185400090819     c                   movel     tblkey        alfa7             7
185500090819     C                   movel     alfa7         K7u(XX)
185600090819     C     XX            ifeq      950
185700090819     c                   move      'S'           SK_in_Capienza
185800090819     c                   end
185900090819     C                   END
186000090819     C     KTAB2         READE     TABEL                                  31
186100090819     C                   END
186200090819      *
186300090819     C*  Se non ho caricato tutti i dati gestisco errore
186400090819     c                   If         SK_in_Capienza = 'S'
186500090819     C     *IN31         IFNE      '1'
186600090819     C                   SETON                                            28
186700090819     C                   MOVEL     MSG(21)       $MSG
186800090819     C                   ElsE
186900090819     C                   SETON                                            28
187000090819     C                   MOVEL     MSG(22)       $MSG
187100090819     C                   END
187200090819     C                   END
187300090819     C*---------------------------------------------------------------*
187400940926      *
187500940919     C*--- RICERCA CAPOCONTI
187600940919     C                   DO        50            X                 2 0
187700940919     C                   MOVE      TCU(X)        TCUDS
187800940919     C     F56           CABNE     'CG'          END1
187900940919     C     F4            COMP      '1'                                    21
188000940919     C** 1 CLIENTI   21
188100940919     C     F3            COMP      '0'                                242425
188200940919     C** 0 ITALIA   25
188300940919     C** 1 ESTERO   24
188400940919     C   21
188500940919     CAN 24              Z-ADD     KCU(X)        KCE               4 0
188600940919     C   21
188700940919     CAN 25              Z-ADD     KCU(X)        KCI               4 0
188800940919     C     END1          TAG
188900940919     C                   END
189000940919     C*§
189100940920      * UDATE
189200940920     C                   TIME                    WHHUDT           14 0
189300940922     C                   MOVE      WHHUDT        G08DAT
189400940920     C                   MOVE      *ZEROS        G08INV
189500940920     C                   MOVEL     *BLANK        G08ERR
189600940920     C                   CALL      'XSRDA8'
189700940920     C                   PARM                    WLBDA8
189800940920     C                   Z-ADD     G08INV        AMGUDT            8 0
189900940920     C                   Z-ADD     G08DAT        GMAUDT            8 0
190000940920     C                   MOVEL     AMGUDT        ANNO              4 0
190100020228     C* gg t.cliente                                       *
190200940922     C     *LIKE         DEFINE    TGCGGR        WGGR
190300950116     C     *LIKE         DEFINE    TGCCTR        KCTR
190400950116     C     *LIKE         DEFINE    TGCKSC        KKSC
190500950116     C     *LIKE         DEFINE    TGCPRG        KPRG
190600940922     C     *LIKE         DEFINE    GCPFGC        FLE
190700940922     C     *LIKE         DEFINE    GCPSCM        SCM
190800961220     C     *LIKE         DEFINE    GNPAGC        KAGC
190900961220     C     *LIKE         DEFINE    GNPFGC        KFGC
191000961220     C     *LIKE         DEFINE    GNPNGC        KNGC
191100961220     C     *LIKE         DEFINE    GNPFRG        KFRG
191200961220     C     *LIKE         DEFINE    GNPFAS        KFAS
191300050324     C     *LIKE         DEFINE    GcPFAS        FAS
191400031029     c     *like         define    gcpFRG        savFRG
191500940919     C*  KLIST
191600940919     C*
191700940919     C     KGCA1         KLIST
191800940919     C                   KFLD                    VAGC
191900940919     C                   KFLD                    VFGC
192000940920     C                   KFLD                    VNGC
192100940919     C                   KFLD                    FRG
192200031029     C*
192300031029     C     KGCA1_GT      KLIST
192400031029     C                   KFLD                    VAGC
192500031029     C                   KFLD                    VFGC
192600031029     C                   KFLD                    VNGC
192700031029     C*
192800961220     C     KGNA          KLIST
192900961220     C                   KFLD                    KAGC
193000961220     C                   KFLD                    KFGC
193100961220     C                   KFLD                    KNGC
193200961220     C                   KFLD                    KFRG
193300961220     C                   KFLD                    KFAS
193400940919     C*
193500940919     C     KGCA2         KLIST
193600940919     C                   KFLD                    VAAS
193700940919     C                   KFLD                    VLNP
193800940919     C                   KFLD                    VNRS
193900940919     C                   KFLD                    VNSP
194000990628     C                   KFLD                    FRG               2 0
194100031029     C*
194200031029     C     KGCA2_GT      KLIST
194300031029     C                   KFLD                    VAAS
194400031029     C                   KFLD                    VLNP
194500031029     C                   KFLD                    VNRS
194600031029     C                   KFLD                    VNSP
194700940919     C*
194800940919     C     KGCA3         KLIST
194900940919     C                   KFLD                    FLE
195000940919     C                   KFLD                    VDIS
195100940919     C*
195200940919     C     KTAB          KLIST
195300940919     C                   KFLD                    CODUT
195400940919     C                   KFLD                    COD               2
195500940919     C                   KFLD                    KEY               8
195600940926     C*
195700940926     C     KTAB2         KLIST
195800940926     C                   KFLD                    CODUT
195900940926     C                   KFLD                    COD
196000940919     C*
196100940919     C     KBLP          KLIST
196200940919     C                   KFLD                    GCPAAS
196300940919     C                   KFLD                    GCPLNP
196400940919     C                   KFLD                    GCPNRS
196500940919     C                   KFLD                    GCPNSP
196600050215     C*
196700050215     C     Karb          KLIST
196800050215     C                   KFLD                    GCPAASa
196900050215     C                   KFLD                    GCPLNPa
197000050215     C                   KFLD                    GCPNRSa
197100050215     C                   KFLD                    GCPNSPa
197200940922     C*
197300940919     C     KACO          KLIST
197400940919     C                   KFLD                    CODUT
197500940919     C                   KFLD                    KCI
197600940919     C                   KFLD                    SCM
197700940922     C*
197800940922     C     KTGC          KLIST
197900940922     C                   KFLD                    KKSC
198000940922     C                   KFLD                    KCTR
198100940922     C                   KFLD                    KPRG
198200031217     C*
198300031217     C     KBLP2         KLIST
198400040108     C                   KFLD                    vAAS1
198500040108     C                   KFLD                    vLNP1
198600040108     C                   KFLD                    vNRS1
198700040108     C                   KFLD                    vNSP1
198800940922     C*
198900050215     C     Kscm          KLIST
199000050215     C                   KFLD                    FLE
199100050215     C                   KFLD                    scm
199200050215     C*
199300080603     c     kar4          klist
199400080603     c                   kfld                    vAAS1
199500080603     c                   kfld                    vLNP1
199600080603     c                   kfld                    vNRS1
199700080603     c                   kfld                    vNSP1
199800080603     c                   kfld                    trek4             1
199900080603     c                   move      'A'           trek4
200000080603      *
200100050324     C     Kflefas       KLIST
200200050324     C                   KFLD                    FLE
200300050324     C                   KFLD                    fas
200400050324     c                   z-add     20            fas
200500060824      * azione forzata XG83 forzatura campi
200600060824     c                   if        kcdaz = 'XG83'
200700060824     c                   seton                                        83
200800060824     c                   move      '1'           vpar
200900060824     c                   endif
201000050324     C*
201100940919     C* FINTO OPEN X RENDERE SHR YES  I FILE
201200941223     C                   OPEN      FNLV22P
201300940919     C   LR
201400940919     CANNLR              EXCEPT    FINTO
201500940919     C                   ENDSR
201600941223     OFNLV22P   E            FINTO
201700950116** MSG
201800940926Giacenza non in Gestione !                                                     1
201900940926Cliente non trovato nel Piano dei Conti                                        2
202000940926Immettere una scelta                                                           3
202100940926Nessuna parzializzazione immessa                                               4
202200940926Selezionare O Immissione Disposizioni  O  Esecuzione Disposizioni              5
202300940926Per questa scelta è possibile immettere o il Nr.Giacenza o il Nr.Spediz.       6
202400940926Per la parzializzazione scelta i dati non sono completi                        7
202500940926Giacenza non trovata !                                                         8
202600050808Giacenza con Disposizioni già immesse o ancora da stampare !                   9
202700940926Nessuna giacenza da visualizzare !                                            10
202800940926Giacenza con disposizioni ancora da Inserire o già  Eseguite                  11
202900961114Punto operativo non in gestione su questo Elaboratore                         12
203000940926Data Errata !                                                                 13
203100940926AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7Q"     14
203200941012Giacenza con data eseguibilità disposizioni > della data odierna              15
203300950116AVVISARE C.E.D. !! Impossibile caricare tutti i cod. NAZIONI  "15"            16
203400050805Impossibile immettere disposizioni oltre le riaperture consentite !!          17
203500051006Se si scegli opzione 4 è obbligatorio selezionare esecuzione disposizioni     18
203600080424Immettere una scelta da 1 a 5                                                 19
203700080424Il Nr.Parcel non è valido o non ha una giacenza collegata.                    20
203800090819AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7U"     21
203900090819AVVISARE C.E.D. !! Tab.: cod.Partic.Giacenza "7U" verso limite massimo.       22
