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
007100950215     D PARAM0          DS
007200950215     D  PA0AAS                 1      4  0
007300950215     D  PA0LNP                 5      7  0
007400950215     D  PA0NRS                 8      9  0
007500950215     D  PA0NSP                10     16  0
007600950215     D  PA0FLG                18     18
007700950215     D  PA0SIM                44     46  0
007800950215     D  PA0F03                48     48
007900950215     D PARAM1          DS
008000950215     D  PA1AAS                14     17  0
008100950215     D  PA1LNP                18     20  0
008200950215     D  PA1NRS                21     22  0
008300950215     D  PA1NSP                23     29  0
008400950215     D  PA1F03                30     30
008500950215     D  PA1FLG                31     31
008600031217     d param80         ds
008700031217     d  fldlnpa                       3
008800031217     d  fldaasa                       4
008900031217     d  fldnrsa                       2
009000031217     d  fldnspa                       7
009100031217     d  fldcca                        1
009200031217     d  fldgga                        2
009300940919     D FNLG82        E DS                  EXTNAME(FNLG82DS)
009400941223     D FNLG30        E DS                  EXTNAME(FNLG30DS)
009500960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
009600940920     D                 DS
009700940920     D  §SEI                   1      6
009800940920     D  §PRF                   1      3
009900940920     D  §FLE                   4      6  0
010000140123     d                 ds
010100140123     d  §mussei                1      6
010200140123     d  knmus2                 1     10
010300140123      *
010400080424     D og143         E DS
010500080603     D DSbl4a        E DS
010600940920     D DS2G          E DS
010700050210     D DS20          E DS
010800940926     D DS7Q          E DS
010900040112     D DS7w          E DS
011000090819     D DS7U          E DS
011100020305     D*DSQT1         E DS
011200950116     D DS15          E DS
011300051005     D Dgcpflr       E DS
011400150514     D fnlv59ds      E DS
011500160302     d  KS_CT                 74    118    dim(15)
011600160302     d  KS_pr                119    163    dim(15)
011700160302     d  KS_ie                164    178    dim(15)
011800160302     d
011900060804     D TIBS34DS      E DS
012000060804     d Azuteds       e ds                  extname(Azute00f)
012100060804     d dDatiute      e ds
012200060804     d dLat          e ds
012300060804     d dute01        e ds
012400940920      * GESTIONE DATE
012500940920     D WLBDA8          DS
012600940920     D  G08DAT                 1      8  0
012700940920     D  G08INV                 9     16  0
012800940920     D  G08ERR                17     17
012900940920     D  G08TGI                18     22  0
013000911010      *
013100940920     D WGIDA8          DS
013200940920     D  GI8DAT                 1      8  0
013300940920     D  GI8INV                 9     16  0
013400940920     D  GI8TGI                17     21  0
013500911115     D*
013600940920      * PARAM X RICEZIONE BOLLE LOCALI - IN ARRIVO
013700930511     D FRB3DS          DS
013800930511     D  FRB3BD                 1      1
013900930511     D  FRB3BK                 2      2
014000930511     D  FRB3BT                 3      3
014100930511     D  FRB3T2                 4      4
014200930511     D  FRB3BG                 5      5
014300930511     D  FRB3EL                 6      8
014400940919     D                SDS
014500940919     D  NOMPGM                 1     10
014600940920      * DATA GIACENZA
014700940920     D                 DS
014800940920     D  DGC                    1      8  0
014900940920     D  DAGC                   1      4  0
015000940920     D  DMGC                   5      8  0
015100940920      * DATA GIACENZA
015200940920     D                 DS
015300940920     D  GCPMGC                 1      4  0
015400940920     D  MESE                   1      2  0
015500940920     D  GIOR                   3      4  0
015600080424      *
015700080424     D  Network_DPD    s              1    inz('N')
015800080424     D  Pulisce_DPD    s              1
015900050524      **********************************************************************
016000050524      * Constants
016100050524      *
016200050524      *     MaxKey - Maximum number of key fields allowed by this program
016300050524      *
016400050524      **********************************************************************
016500050525     D MaxKey          C                    2
016600050525     D mittente        C                   '1'
016700050525     D giacenza        C                   '2'
016800050524     D Ascendente      C                   1
016900050524     D Carattere       C                   6
017000050524     D Put             C                   1
017100050524     D EndPut          C                   2
017200050524     D Get             C                   3
017300050524      **********************************************************************
017400050524      * Standalone fields
017500050524      *
017600050524      *     nrr        - Relative record number for subfile
017700050524      *     SizeList   - Size of list
017800050524      *     ReturnSize - Size of list returned by sort API
017900050524      **********************************************************************
018000050524     D NotUsed         S             16A
018100050524     D ReturnSize      S              9B 0
018200050524     D SizeList        S              9B 0
018300050524     D RrnLast         S              5i 0
018400050524     D WrkSort         S              1
018500050524     D Vrsm1w          S                   LIKE(Vrsm1)
018600050524      **********************************************************************
018700050524      * Data Structures
018800050524      *     SflRcd     - The entire subfile record to pass to the sort
018900050524      *     QLGSCB     - The sort request block for the QLGSORT API
019000050524      *     QLGSCB00   - The sort request block for the QLGSRTIO API
019100050524      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
019200050524      *     QUSEC      - Error structure for the QLGSORT API
019300050524      **********************************************************************
019400050524     D SflRcd          DS
019500050524     d VSEL
019600050524     d VLNP1
019700050524     d VFGC1
019800050524     d VAGC1
019900050524     d VNGC1
020000050524     d VMGC1
020100050524     d VCMC1
020200050524     d VDIS1
020300050524     d VAAS1
020400050524     d VNRS1
020500050524     d VNSP1
020600050524     D VRSM1
020700050524     d Vmsg1
020800050524     D VRSd1
020900080603     D Vddts
021000090819     D  h1in52
021100050524     D Selected                       1A
021200050524
021300050524      /COPY QSYSINC/QRPGLESRC,QLGSORT
021400050524     D QLGKL                         16    DIM(MaxKey)
021500050524     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021600050524     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021700050524     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021800050524     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021900050524
022000050524      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
022100050524      /COPY QSYSINC/QRPGLESRC,QUSEC
022200050524      *---------------------------------------------------------------------
022300900418     C*
022400911010     C     *INKC         DOWEQ     '0'
022500940926     C     *IN28         IFEQ      '0'
022600940926     C                   Z-ADD     0             VFGC
022700940926     C                   Z-ADD     ANNO          VAGC
022800940926     C                   Z-ADD     0             VNGC
022900950215     C     WSCE          IFNE      'S'
023000940926     C                   Z-ADD     0             VLNP
023100940926     C                   Z-ADD     ANNO          VAAS
023200940926     C                   Z-ADD     0             VNRS
023300940926     C                   Z-ADD     0             VNSP
023400950215     C                   ELSE
023500950215     C                   MOVEL     '2'           VPAR
023600950215     C                   END
023700940926     C                   MOVEL     *BLANK        VDIS
023800940926     C                   Z-ADD     GMAUDT        VDED
023900940926     C                   ENDIF
024000940926     C     D82GES        IFEQ      'A'
024100940926      *ARRIVO
024200051006     c                   if        vpar = 4
024300051006     c                   clear                   vdis
024400051006     c                   end
024500950215     C                   MOVEL     'N'           WSCE              1
024600080424      *
024700940919     C                   EXFMT     FG82D3
024800080424      *
024900940923     C* Se anno a 2 lo porto a  4
025000940923     C     VNGC          IFNE      *ZEROS
025100940923     C                   MOVEL     VAGC          WCOM2             2 0
025200940923     C     WCOM2         IFEQ      0
025300940923     C     VAGC          IFGT      60
025400940923     C                   ADD       1900          VAGC
025500940923     C                   ELSE
025600940923     C                   ADD       2000          VAGC
025700940923     C                   END
025800940923     C                   END
025900940923     C                   ENDIF
026000940923     C     VNSP          IFNE      *ZEROS
026100940923     C                   MOVEL     VAAS          WCOM2             2 0
026200940923     C     WCOM2         IFEQ      0
026300940923     C     VAAS          IFGT      60
026400940923     C                   ADD       1900          VAAS
026500940923     C                   ELSE
026600940923     C                   ADD       2000          VAAS
026700940923     C                   END
026800940923     C                   END
026900940923     C                   ENDIF
027000940920      * INTERROGAZ. BOLLE ARRIVI
027100950215     C     *INKG         IFEQ      '1'
027200950215     C                   Z-ADD     VAAS          PA1AAS
027300950215     C                   Z-ADD     VLNP          PA1LNP
027400950215     C                   Z-ADD     VNRS          PA1NRS
027500950215     C                   Z-ADD     VNSP          PA1NSP
027600950215     C                   MOVEL     ' '           PA1F03
027700950215     C                   MOVEL     '2'           PA1FLG
027800950215     C     PA1NSP        IFGT      0
027900950215     C                   MOVEL     '1'           PA1FLG
028000950215     C                   END
028100950215     C                   MOVEL     PARAM1        KPJBU
028200950215     C                   CALL      'FNLR36R'
028300950215     C                   PARM                    KPJBA
028400950215     C                   MOVEL     KPJBU         PARAM1
028500950215     C     PA1F03        IFNE      '1'
028600950215     C                   MOVEL     'S'           WSCE
028700950215     C                   Z-ADD     PA1AAS        VAAS
028800950215     C                   Z-ADD     PA1LNP        VLNP
028900950215     C                   Z-ADD     PA1NRS        VNRS
029000950215     C                   Z-ADD     PA1NSP        VNSP
029100950215     C                   END
029200950215     C                   END
029300950215     C*
029400940926     C                   ELSE
029500080424      *
029600080424     c     ritorno_D2    tag
029700080424      *?  Controlla se si tratta di NTW DPD...  ?
029800080424     c                   exsr      Check_se_DPD
029900080424      *
030000940926      *PARTENZA
030100940919     C                   EXFMT     FG82D2
030200080424      *
030300940926     C* Se anno a 2 lo porto a  4
030400940926     C     VNGC          IFNE      *ZEROS
030500940926     C                   MOVEL     VAGC          WCOM2             2 0
030600940926     C     WCOM2         IFEQ      0
030700940926     C     VAGC          IFGT      60
030800940926     C                   ADD       1900          VAGC
030900940926     C                   ELSE
031000940926     C                   ADD       2000          VAGC
031100940926     C                   END
031200940926     C                   END
031300940926     C                   ENDIF
031400940920      * INTERROGAZ. BOLLE PARTENZE
031500950215     C     *INKG         IFEQ      '1'
031600950215     C                   CLEAR                   PARAM0
031700950215     C                   MOVEL     PARAM0        KPJBU
031800950215     C                   CALL      'FNLS04R'
031900950215     C                   PARM                    KPJBA
032000950215     C                   MOVEL     KPJBU         PARAM0
032100950215     C                   END
032200950215     C*
032300940926     C                   ENDIF
032400940920      * CAMBIO FILIALE
032500080424     C   KS              Do
032600080424     C                   EXSR      CMBFIL
032700080522     C                   IF        d82ges <> 'A'
032800080424     c                   goto      ritorno_D2
032900080522     c                   endif
033000080424     c                   endDo
033100940920      * INTERROGAZ. GIACENZE
033200941223     C   KN              DO
033300941223     C                   CLEAR                   FNLG30
033400941223     C                   MOVEL     D82GES        D30POA
033500941223     C                   MOVEL     FNLG30        KPJBU
033600950103     C     D82GES        IFEQ      'A'
033700941228     C                   CALL      'FNLG31C'
033800920427     C                   PARM                    KPJBA
033900950103     C                   ELSE
034000950103     C                   CALL      'FNLG30C'
034100950103     C                   PARM                    KPJBA
034200941223     C                   END
034300950103     C                   END
034400940919      * CONTROLLI VIDEATA INIZIALE
034500940919     C  NKC
034600940919     CANNKG
034700940919     CANNKN              EXSR      CTRVID
034800940919      *
034900940920     C  N21
035000940920     CANNKS
035100940920     CANNKN
035200940919     CANNKC
035300940919     CANN28
035400940919     CANNKG              DO
035500911023     C                   Z-ADD     0             FRG
035600940920      *
035700940919      * PARTENZA
035800940919      *---------
035900940920      * ----> IMMESSO NR.GIACENZA
036000940920     C     D82GES        IFEQ      'P'
036100940919     C     VPAR          ANDEQ     1
036200080424      *
036300080424      * Oppure se immesso il Parcel -> ha tradotto la giacenza
036400080424     C     d82ges        orEQ      'P'
036500080424     C     Vpar          andEQ     6
036600080424      *
036700050215     C     KGCA1         CHAIN     tigcp02L                           48
036800940920     C     *IN48         IFEQ      '0'
036900940919      * CONTROLLO SE GIACENZA IN GESTIONE
037000060804     c*mm                if        kcdaz = 'XG83'
037100060804     c*mm                seton                                        89
037200060804     c*mm                else
037300940919     C     §FLE          COMP      GCPLNP                                 89
037400940919     C  N89              MOVE      GCPLNP        LNP§              3
037500940922     C  N89LNP§          LOOKUP    F£6                                    89
037600060804     c*mm                endif
037700940928     C   89              MOVE      GCPAGC        D82AGC
037800940928     C   89              MOVE      GCPFGC        D82FGC
037900940928     C   89              MOVE      GCPNGC        D82NGC
038000940922     C   89              MOVEL     *BLANKS       D82DIS
038100941215     C   89              MOVEL     §FLE          D82FGS
038200050316     C   89              MOVE      'S'           D82comm
038300050726     c   89              clear                   d82ntaut
038400050726     c   89              clear                   d82nodsp
038500940920     C   89              MOVEL     FNLG82        KPJBU
038600940920     C   89              CALL      'FNLG10R'
038700911030     C                   PARM                    KPJBA
038800080424      *
038900080424      * Di ritorno dal chiamante deve ripulire sia l'opz.che il Parcel
039000080424     c                   if        vPar = 6 and Vparcel <> ' '
039100080424     c                   eval      pulisce_DPD = 'S'
039200080424     c                   end
039300080424      *
039400940920     C  N89              SETON                                        4828
039500940920     C   48              MOVEL     MSG(1)        $MSG
039600940919     C                   ELSE
039700940919     C                   SETON                                        4828
039800940919     C                   MOVEL     MSG(8)        $MSG
039900940920     C                   ENDIF
040000940920     C                   ENDIF
040100940919      * ARRIVO :
040200940919      *---------
040300940920     C     D82GES        IFEQ      'A'
040400940920      * ----> IMMESSO NR. GIACENZA
040500940920     C     VPAR          IFEQ      1
040600050630      * calcolo numero riaperture
040700050630     c                   exsr      srconta
040800050630     c*
040900050215     C     KGCA1         CHAIN     tigcp02L                           48
041000941012     C   48              MOVEL     MSG(8)        $MSG
041100941012     C   48              SETON                                        28
041200031029      *
041300940920     C     *IN48         IFEQ      '0'
041400940919      * CONTROLLO SE GIACENZA IN GESTIONE
041500940919     C     VFGC          COMP      §FLE                                   89
041600940919     C  N89              MOVE      VFGC          LNP§
041700940922     C  N89LNP§          LOOKUP    F£6                                    89
041800940920     C  N89              SETON                                        4828
041900940920     C   48              MOVEL     MSG(1)        $MSG
042000031029      *
042100940920      * IMMISS.DISPOSIZ.
042200940920     C  N28VSEL1         IFEQ      'X'
042300031029      *
042400040705      * per non immettere disposizioni oltre le riaperture consentite + 1
042500040705     c                   z-add     0             limite            1 0
042600040705     c                   z-add     1             xy                3 0
042700050215     c     karb          chain     fnarb01l
042800040705     c                   if        arbgga <> *blank
042900040705     c     arbgga        lookup    c7q(xy)                                55
043000040705     c                   else
043100040705     c     'GR'          lookup    c7q(xy)                                55
043200040705     c                   endif
043300040705     c   55              movel     d7q(xy)       ds7q
043400040705     c                   eval      limite = §7q5rp + 1
043500040705     c                   if        savFRG > limite
043600031022     C                   SETON                                        4828
043700031022     C                   MOVEL     MSG(17)       $MSG
043800031022     c                   end
043900031029      *
044000050215     C  n28GCPFAS        IFEQ      20
044100940928     C                   MOVE      GCPAGC        D82AGC
044200940928     C                   MOVE      GCPFGC        D82FGC
044300940928     C                   MOVE      GCPNGC        D82NGC
044400940922     C                   MOVEL     *BLANKS       D82DIS
044500941215     C                   MOVEL     §FLE          D82FGS
044600050316     C                   MOVE      'S'           D82comm
044700050726     c                   clear                   d82nodsp
044800050726     c                   clear                   d82ntaut
044900940920     C                   MOVEL     FNLG82        KPJBU
045000940920     C                   CALL      'FNLG10R'
045100911030     C                   PARM                    KPJBA
045200911121     C                   ELSE
045300940920     C                   SETON                                        4828
045400940920     C                   MOVEL     MSG(9)        $MSG
045500911023     C                   END
045600940920     C                   ENDIF
045700031029      *
045800940920      * ESECUZ. DISPOSIZ.
045900940920     C  N28VSEL2         IFEQ      'X'
046000050215     C     GCPFAS        IFEQ      30
046100050215     C     GCPFAS        OREQ      31
046200050215     C     GCPFAS        OREQ      36
046300941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
046400941012     C     GCPDED        IFGT      AMGUDT
046500941012     C                   SETON                                        4828
046600941012     C                   MOVEL     MSG(15)       $MSG
046700941012     C                   ELSE
046800040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
046900040112     c                   CLEAR                   ds7w
047000050215     c     karb          chain     fnarb01l
047100040112     c                   if        arbgga <>*blank
047200040112     c                   move      '7W'          cod
047300040112     c                   movel     *blank        key
047400040112     c                   movel     arbgga        key
047500040112     c     ktab          chain     tabel00f
047600040112     c                   if        %found(tabel00f)
047700040112     c                   movel     tbluni        ds7w
047800040112     c                   endif
047900040112     c                   endif
048000031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
048100040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
048200040112     c                             gcpdis = §7w7t3
048300031217     c                   movel     arblnp        fldlnpa
048400031217     c                   movel     arbaas        fldaasa
048500031217     c                   movel     arbnrs        fldnrsa
048600031217     c                   movel     arbnsp        fldnspa
048700040112     c                   select
048800040112     c                   when      gcpdis = §7w7t1
048900040112     c                   movel     §7w7ca1       fldcca
049000040112     c                   when      gcpdis = §7w7t2
049100040112     c                   movel     §7w7ca2       fldcca
049200040112     c                   when      gcpdis = §7w7t3
049300040112     c                   movel     §7w7ca3       fldcca
049400040112     c                   endsl
049500031217     c                   movel     arbgga        fldgga
049600031217     c                   movel(p)  param80       kpjbu
049700031217     c                   call      'FNLG80R1'
049800031219     c                   parm                    kpjba
049900031217     c                   else
050000940928     C                   MOVE      GCPAGC        D82AGC
050100940928     C                   MOVE      GCPFGC        D82FGC
050200940928     C                   MOVE      GCPNGC        D82NGC
050300940922     C                   MOVEL     *BLANKS       D82DIS
050400941215     C                   MOVEL     §FLE          D82FGS
050500050316     C                   MOVE      'S'           D82comm
050600050726     c                   clear                   d82nodsp
050700050726     c                   clear                   d82ntaut
050800940920     C                   MOVEL     FNLG82        KPJBU
050900940920     C                   CALL      'FNLG20R'
051000911030     C                   PARM                    KPJBA
051100031217     C                   Endif
051200941012     C                   END
051300911121     C                   ELSE
051400940920     C                   SETON                                        4828
051500940920     C                   MOVEL     MSG(11)       $MSG
051600911023     C                   END
051700940920     C                   ENDIF
051800940919     C                   ELSE
051900940920     C                   SETON                                        4828
052000940919     C                   MOVEL     MSG(8)        $MSG
052100940920     C                   ENDIF
052200940920     C                   ENDIF
052300940919      *
052400940919      *         ----> IMMESSO NR. SPEDIZIONE
052500940920     C     VPAR          IFEQ      2
052600050630      * calcolo numero riaperture
052700050630     c                   exsr      srconta
052800031029      *
052900050215     C     KGCA2         CHAIN     tigcp21L                           46
053000031029      *
053100941012     C   46              MOVEL     MSG(8)        $MSG
053200941012     C   46              SETON                                        28
053300031029      *
053400940920     C     *IN46         IFEQ      '0'
053500940920     C     §FLE          COMP      GCPFGC                                 89
053600050215     C  N89              MOVE      GCPLNAa       LNP§
053700940922     C  N89LNP§          LOOKUP    F£6                                    89
053800940920     C  N89              SETON                                        4628
053900940920     C   46              MOVEL     MSG(1)        $MSG
054000940920      * IMMISS. DISPOSIZ.
054100940920     C  N28VSEL1         IFEQ      'X'
054200031029      *
054300040705      * per non immettere disposizioni oltre le riaperture consentite + 1
054400040705     c                   z-add     0             limite
054500040705     c                   z-add     1             xy
054600050215     c     karb          chain     fnarb01l
054700040705     c                   if        arbgga <> *blank
054800040705     c     arbgga        lookup    c7q(xy)                                55
054900040705     c                   else
055000040705     c     'GR'          lookup    c7q(xy)                                55
055100040705     c                   endif
055200040705     c   55              movel     d7q(xy)       ds7q
055300040705     c                   eval      limite = §7q5rp + 1
055400040705     c                   if        savFRG > limite
055500031029     C                   SETON                                        4628
055600031029     C                   MOVEL     MSG(17)       $MSG
055700031029     c                   end
055800031029      *
055900050215     C  n28GCPFAS        IFEQ      20
056000940928     C                   MOVE      GCPAGC        D82AGC
056100940928     C                   MOVE      GCPFGC        D82FGC
056200940928     C                   MOVE      GCPNGC        D82NGC
056300940922     C                   MOVEL     *BLANKS       D82DIS
056400941215     C                   MOVEL     §FLE          D82FGS
056500050316     C                   MOVE      'S'           D82comm
056600050726     c                   clear                   d82nodsp
056700050726     c                   clear                   d82ntaut
056800940920     C                   MOVEL     FNLG82        KPJBU
056900940920     C                   CALL      'FNLG10R'
057000911030     C                   PARM                    KPJBA
057100911105     C                   ELSE
057200940920     C                   SETON                                        4628
057300940919     C                   MOVEL     MSG(9)        $MSG
057400940920     C                   END
057500940920     C                   ENDIF
057600940920      * ESECUZ. DISPOSIZ.
057700940920     C  N28VSEL2         IFEQ      'X'
057800050215     C     GCPFAS        IFEQ      30
057900050215     C     GCPFAS        OREQ      31
058000050215     C     GCPFAS        OREQ      36
058100941012     C* CONTROLLO SE SI POSSONO ESEGUIRE LE DISPOSIZ.
058200941012     C     GCPDED        IFGT      AMGUDT
058300941012     C                   SETON                                        4828
058400941012     C                   MOVEL     MSG(15)       $MSG
058500941012     C                   ELSE
058600031217      * verifico la bolla
058700040112     c                   CLEAR                   ds7w
058800050215     c     karb          chain     fnarb01l
058900040112     c                   if        arbgga <>*blank
059000040112     c                   move      '7W'          cod
059100040112     c                   movel     *blank        key
059200040112     c                   movel     arbgga        key
059300040112     c     ktab          chain     tabel00f
059400040112     c                   if        %found(tabel00f)
059500040112     c                   movel     tbluni        ds7w
059600040112     c                   endif
059700040112     c                   endif
059800031217      * se disposizione di reso e sulla bolla c'è la particolarità GS
059900040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
060000040112     c                             gcpdis = §7w7t3
060100031217     c                   movel     arblnp        fldlnpa
060200031217     c                   movel     arbaas        fldaasa
060300031217     c                   movel     arbnrs        fldnrsa
060400031217     c                   movel     arbnsp        fldnspa
060500040112     c                   select
060600040112     c                   when      gcpdis = §7w7t1
060700040112     c                   movel     §7w7ca1       fldcca
060800040112     c                   when      gcpdis = §7w7t2
060900040112     c                   movel     §7w7ca2       fldcca
061000040112     c                   when      gcpdis = §7w7t3
061100040112     c                   movel     §7w7ca3       fldcca
061200040112     c                   endsl
061300031217     c                   movel     arbgga        fldgga
061400031217     c                   movel(p)  param80       kpjbu
061500031217     c                   call      'FNLG80R1'
061600031219     c                   parm                    kpjba
061700031217     c                   else
061800940928     C                   MOVE      GCPAGC        D82AGC
061900940928     C                   MOVE      GCPFGC        D82FGC
062000940928     C                   MOVE      GCPNGC        D82NGC
062100940922     C                   MOVEL     *BLANKS       D82DIS
062200941215     C                   MOVEL     §FLE          D82FGS
062300050316     C                   MOVE      'S'           D82comm
062400050726     c                   clear                   d82nodsp
062500050726     c                   clear                   d82ntaut
062600940920     C                   MOVEL     FNLG82        KPJBU
062700940920     C                   CALL      'FNLG20R'
062800911030     C                   PARM                    KPJBA
062900031217     C                   Endif
063000941012     C                   END
063100911105     C                   ELSE
063200940920     C                   SETON                                        4628
063300940920     C                   MOVEL     MSG(11)       $MSG
063400940920     C                   END
063500940920     C                   ENDIF
063600941012     C                   ENDIF
063700940920     C                   ENDIF
063800940920     C                   ENDIF
063900940920      *
064000940919      * NESSUN SCELTA SPECIFICA, CARICO SFL
064100940919      *
064200940919     C  N28D82GES        IFEQ      'P'
064300940919     C     VPAR          ANDNE     1
064400080424     C     vpar          andNE     6
064500080424      *
064600940919     C     D82GES        OREQ      'A'
064700940922     C     VPAR          ANDEQ     3
064800080424      *
064900010920     C     D82GES        OREQ      'A'
065000010920     C     VPAR          ANDEQ     4
065100080424      *
065200940919     C                   EXSR      CARICA
065300080424      *
065400940919     C     NRR           IFEQ      0
065500940919     C                   SETON                                        28
065600940920     C                   MOVEL     MSG(10)       $MSG
065700911030     C                   ELSE
065800050512     c     riemetti      tag
065900911105     C                   WRITE     FG82D4
066000911105     C                   EXFMT     FG82CT
066100911105     C                   SETOFF                                       30
066200950207     C                   MOVEL     'N'           WSEL              1
066300050525      * ordina per mittente o nr. giacenza
066400050524     c                   if        *inkk
066500050525     c                   setoff                                       32
066600050525      * scelta ordinamento
066700050525     c                   select
066800050525     c                   when      wrksort = giacenza or wrksort = *blank
066900050525     c                   eval      wrksort = mittente
067000050525     c                   seton                                        07
067100050525     c                   when      wrksort = mittente
067200050525     c                   eval      wrksort = giacenza
067300050525     c                   setoff                                       07
067400050525     c                   endsl
067500050525      *
067600050613     c                   eval      rrnlast = wnrr
067700050524     c                   exsr      sort
067800050524     c                   goto      riemetti
067900050524     c                   end
068000911105     C*
068100050527     c                   if        not *inkc and not *inkl and not *inkf
068200051005     c                             and not *inka and not *inkg
068300050527     c                   goto      riemetti
068400050527     c                   endif
068500050527     c  NKL
068600050527     CANNKC*IN30         DOWEQ     '0'
068700911105     C                   READC     FG82SF                                 30
068800050513      *verifico se scelta opzione 7 non per i resi
068900050513     c                   if        vsel = '7' and (vpar <> 3 and vpar <> 5)
069000050517     c                             or vsel = '7' and d82ges = 'A'
069100050517     c                   goto      riemetti
069200050513     c                   endif
069300050811      * periodo resi bloccati
069400050811     c                   if        amgudt >= §2gdpi and amgudt <= §2gdpf
069500050811     c                             and d82ges = 'P' and (vpar = 3 or vpar = 5 )
069600050811     c                   goto      riemetti
069700050811     c                   else
069800050811      * cmd1 spalma opzione 1 su tutte le righe
069900050811     c                   if        *inka and *in30
070000050525     c                   setoff                                       32
070100050512     c                   do        wnrr          nrr
070200050613     c     nrr           chain     fg82sf                             27
070300050613     c   27              leave
070400050512     c                   move      '1'           vsel
070500050512     c                   seton                                        72
070600090819     c                   eval        *in52 = H1IN52
070700050512     c                   update    fg82sf
070800050512     c                   enddo
070900050512     c                   goto      riemetti
071000050512     c                   endif
071100050512      *
071200050525      * cmd7 spalma opzione 7 su tutte le righe
071300050811     c                   if        *inkg and *in30
071400050525     c                   setoff                                       32
071500050512     c                   do        wnrr          nrr
071600050613     c     nrr           chain     fg82sf                             27
071700050613     c   27              leave
071800050512     c                   move      '7'           vsel
071900050512     c                   seton                                        72
072000090819     c                   eval        *in52 = H1IN52
072100050512     c                   update    fg82sf
072200050512     c                   enddo
072300050512     c                   goto      riemetti
072400050512     c                   endif
072500050811      *resi bloccati
072600050811     c                   endif
072700050527     C
072800050513     c                   clear                   d82nodsp
072900050527     c                   clear                   d82ntaut
073000050527     C   KF
073100050527     CANN30VSEL          IFEQ      '1'
073200050513     C     VSEL          oreq      '7'
073300941007     C                   MOVE      VAGC1         D82AGC
073400941007     C                   MOVE      VFGC1         D82FGC
073500941007     C                   MOVE      VNGC1         D82NGC
073600941222     C                   MOVEL     VDIS1         D82DIS
073700941215     C                   MOVEL     §FLE          D82FGS
073800050316     C                   MOVE      'S'           D82comm
073900050513      *forza parametro per non emettere videate pgm chiamanti con opz=7
074000050513     c                   if        vsel = '7'
074100050513     c                   move      'X'           d82nodsp
074200050726     c                   move      'S'           d82ntaut
074300050513     c                   endif
074400941221     C                   MOVEL     FNLG82        KPJBU
074500940922     C     D82GES        IFEQ      'A'
074600940922     C     VSEL1         IFEQ      'X'
074700950207     C                   MOVEL     'S'           WSEL              1
074800940920     C                   CALL      'FNLG10R'
074900911128     C                   PARM                    KPJBA
075000911128     C                   ELSE
075100950207     C                   MOVEL     'S'           WSEL              1
075200040112      * verifico la bolla GESTIONE PARTICOLARE MERCE DA DISTRUGGERE
075300040112     c                   CLEAR                   ds7w
075400031217     c     kblp2         chain     fnarb01l
075500040112     c                   if        arbgga <>*blank
075600040112     c                   move      '7W'          cod
075700040112     c                   movel     *blank        key
075800040112     c                   movel     arbgga        key
075900040112     c     ktab          chain     tabel00f
076000040112     c                   if        %found(tabel00f)
076100040112     c                   movel     tbluni        ds7w
076200040112     c                   endif
076300040112     c                   endif
076400040112      * se disposizione di reso e sulla bolla c'è la particolarità GS
076500040112     c                   if        gcpdis = §7w7t1 or gcpdis = §7w7t2 or
076600040112     c                             gcpdis = §7w7t3
076700031217     c                   movel     arblnp        fldlnpa
076800031217     c                   movel     arbaas        fldaasa
076900031217     c                   movel     arbnrs        fldnrsa
077000031217     c                   movel     arbnsp        fldnspa
077100040112     c                   select
077200040112     c                   when      gcpdis = §7w7t1
077300040112     c                   movel     §7w7ca1       fldcca
077400040112     c                   when      gcpdis = §7w7t2
077500040112     c                   movel     §7w7ca2       fldcca
077600040112     c                   when      gcpdis = §7w7t3
077700040112     c                   movel     §7w7ca3       fldcca
077800040112     c                   endsl
077900031217     c                   movel     arbgga        fldgga
078000031217     c                   movel(p)  param80       kpjbu
078100031217     c                   call      'FNLG80R1'
078200031219     c                   parm                    kpjba
078300031217     c                   else
078400051006     c                   if        vpar <> 4
078500940920     C                   CALL      'FNLG20R'
078600911128     C                   PARM                    KPJBA
078700051006     C                   Endif
078800031217     C                   Endif
078900051005     c* Appuntamento
079000051005     c                   if        vpar = 4
079100051005     C                   CALL      'FNLG88R'
079200051005     C                   PARM                    KPJBA
079300051005     c                   end
079400940922     C                   END
079500911128     C                   ELSE
079600051005     c* PARTENZA
079700950207     C                   MOVEL     'S'           WSEL              1
079800940920     C                   CALL      'FNLG10R'
079900911105     C                   PARM                    KPJBA
080000940922     C                   END
080100940922     C                   END
080200911105     C                   Z-ADD     0             VFGC
080300911105     C                   Z-ADD     0             VAGC
080400911105     C                   Z-ADD     0             VNGC
080500940922     C                   END
080600950207     C*
080700950207     C     WSEL          IFEQ      'S'
080800950207     C                   EXSR      CARICA
080900950207     C                   END
081000950207     C*
081100950207     C                   END
081200940919     C                   ENDIF
081300940922     C                   END
081400911023     C                   END
081500940919      *----------------------------------------------------
081600020306     C                   CLEAR                   TRUL27
081700020306     C                   MOVEL     'C'           I27TLA
081800160302     C                   CALL      'TRUL27R1'
081900020306     C                   PARM                    TRUL27
082000160302     c
082100160302     C                   CLEAR                   TRULC7DS
082200160302     C                   MOVEL     'C'           IC7TLA
082300160302     C                   CALL      'TRULC7R'
082400160302     C                   PARM                    TRULC7DS
082500160302     c
082600160302     C                   CLEAR                   fnlv59ds
082700160302     C                   MOVEL     'C'           Ilv59tla
082800160302     C                   CALL      'FNLV59R'
082900160302     C                   PARM                    ilv59tla
083000900418     C                   SETON                                        LR
083100080424     C*---------------------------------------------------------------------*
083200080424     c*  Controlla la filiale su AZorg per controllare il NTW
083300080424     C*---------------------------------------------------------------------*
083400080424     c     Check_se_DPD  begsr
083500080424      *
083600080424      *
083700080424      * se DPD deve dare la possibilità di inserire direttamente il Parcel
083800080424     c                   eval      Network_DPD ='N'
083900080424     c     §FLE          chain     azorg01l
084000080424     c                   if        %Found(Azorg01l)
084100080424     c                   movel     orgDE3        Og143
084200140108     c                   if        §OGNTW = 'DPD' or
084300140123     c                             %subst(knmus:1:3) = 'INT'
084400080424     c                   eval      Network_DPD ='S'
084500080424     c                   end
084600080424     c                   end
084700080424      *
084800080424      * per 84 = *on Protegge l'inserimento del Nr. Parcel a video Opz.6
084900080424      *
085000080424     c     Network_DPD   comp      'N'                                    84
085100080424      *
085200080424     c                   if        Network_DPD = 'N' or
085300080424     c                             pulisce_DPD = 'S'
085400080424     c                   clear                   vParcel
085500080424     c                   if        vPar = 6
085600080424     c                   clear                   vPar
085700080424     c                   end
085800080424     c                   end
085900080424      *
086000080424     c                   eval      pulisce_DPD = 'N'
086100080424      *
086200080424     c                   endsr
086300080424     C*---------------------------------------------------------------------*
086400940923     C*--------------------*
086500050630     C* Conta numero riaperture
086600940923     C*--------------------*
086700050630     C     srconta       BEGSR
086800050630     c*
086900050630     c                   clear                   fnlg09ds
087000050630     c                   eval      d09tla = 'R'
087100050630     c                   select
087200050630     C     VPAR          wheneq    1
087300050630     c                   eval      d09AGC = VAGC
087400050630     c                   eval      d09FGC = VFGC
087500050630     c                   eval      d09NGC = VNGC
087600050630     C     VPAR          wheneq    2
087700050630     c                   eval      d09aas = VAAS
087800050630     c                   eval      d09lnp = VLNP
087900050630     c                   eval      d09nrs = VNRS
088000050630     c                   eval      d09nsp = VNSP
088100050630     c                   endsl
088200050630     c                   movel(p)  fnlg09ds      kpjbu
088300050630     C                   CALL      'FNLG09R'
088400050630     C                   PARM                    kpjba
088500050630     c                   movel(p)  kpjbu         fnlg09ds
088600050630     c     d09cnt        sub       d09ced        savfrg
088700050630     c*
088800050630     c                   endsr
088900050630     C*--------------------*
089000050630     C* CAMBIO FILIALE
089100050630     C*--------------------*
089200050630     C     CMBFIL        BEGSR
089300940926     C                   SETOFF                                       28
089400940926     C                   MOVEL     *BLANK        $MSG
089500940923     C                   MOVE      §FLE          VD1FLE
089600940923     C     *IN28         DOUEQ     '0'
089700940923     C                   EXFMT     FG82D1
089800940926     C                   SETOFF                                       28
089900940923     C   KL              SETOFF                                       28
090000940923     C  NKL              DO
090100940923     C                   MOVE      VD1FLE        LNP§              3
090200940923     C     LNP§          LOOKUP    F£1                                    31
090300940923     C     *IN31         IFEQ      '0'
090400940923     C                   SETON                                        28
090500940923     C                   MOVEL     MSG(12)       $MSG
090600940923     C                   ELSE
090700940923      * CARICO FILIALI IN GESTIONE
090800940923     C                   MOVE      VD1FLE        §FLE
090900940923     C                   MOVEL     *BLANKS       KEY
091000940923     C                   MOVEL     §FLE          KEY
091100940923     C                   MOVEA     *ZEROS        F£6
091200940923     C     D82GES        IFEQ      'A'
091300020503     c                   clear                   trul06ds
091400020503     c                   eval      d06cod = '£6'
091500020503     c                   movel     §fle          d06key
091600020503     c                   movel     trul06ds      kpjbu
091700020503     c                   call      'TRUL06R'
091800020503     c                   parm                    kpjba
091900020503     c                   movel     kpjbu         trul06ds
092000020503     c                   movea     lin           f£6
092100020503     C*                  MOVE      '£6'          COD
092200940923     C                   ELSE
092300140123     c                   clear                   trovata           1
092400940923     C                   MOVE      '£7'          COD
092500140102      * forzo caricamento filiali dpd per profilo mirato
092600140123     c                   if        %subst(knmus:1:3) = 'INT'
092700140123     c                   movel(p)  §mussei       key
092800140123     C     KTAB          CHAIN     TABEL                              14
092900140123     C  N14              MOVEA     TBLUNI        F£6
093000140123     c  n14              move      'X'           trovata
093100140123     c   14              movel(p)  §fle          key
093200140102     c                   endif
093300140102      *
093400140123     c                   if        trovata = *blank
093500940923     C     KTAB          CHAIN     TABEL                              15
093600940923     C  N15              MOVEA     TBLUNI        F£6
093700940923     C   15              MOVE      §FLE          F£6(1)
093800140123     c                   endif
093900020503     C                   ENDIF
094000020503      *
094100940923     C                   ENDIF
094200940923     C                   ENDDO
094300940923     C                   ENDDO
094400940923     C                   ENDSR
094500911017     C*--------------------*
094600911017     C* CARICAMENTO SUBFILE
094700911017     C*--------------------*
094800911010     C     CARICA        BEGSR
094900051006     c* per appuntamento forzo la disposizione = consegna
095000051006     c                   if        vpar = 4 and d82ges = 'A'
095100051006     c                   eval      vdis = 'C'
095200051006     c                   end
095300051006     c*
095400911017     C                   EXSR      PULIZ
095500940922     C* MI POSIZIONO X OGNI FILIALE IN GESTIONE
095600940922     C                   DO        29            I                 2 0
095700940922     C                   MOVE      F£6(I)        FLE
095800940922     C     FLE           IFGT      *ZEROS
095900940919      *  POSIZIONAMENTO
096000940922     C     D82GES        IFEQ      'P'
096100950208      * SE GESTISCO CARICAMENTO X CLIENTE CARICO SOLO
096200950208      * SE CLIENTE E' DELLA FILIALE CHE STO ESAMINANDO
096300940922      * PARTENZA
096400940922     C     VPAR          IFEQ      2
096500950208     C                   MOVEL     SCM           WFILC             3 0
096600050215     C     kSCM          SETLL     tigcp07L                           20
096700940919     C                   ELSE
096800050324     C     kflefas       SETLL     tigcp06L                           20
096900940919     C                   ENDIF
097000940922     C                   ELSE
097100940922      * ARRIVO PER DISPOSIZIONE
097200050215     C     KGCA3         SETLL     tigcp24L                           20
097300940922     C                   ENDIF
097400940919      *  LETTURA
097500940922     C     *IN20         DOWEQ     '0'
097600940922     C     D82GES        IFEQ      'P'
097700940922      * PARTENZA
097800940922     C     VPAR          IFEQ      2
097900050215     C     kSCM          READE     tigcp07L                             1620
098000050215     c                   move      gcplnp        gcplnpx           3
098100050215     c     gcplnpx       lookup    f£6                                    35
098200020821     c  n35              iter
098300940919     C                   ELSE
098400050215     C     FLE           READE     tigcp06L                             1620
098500050324      * forzata uscita dal loop per non leggere rekord inutili e ottimizzare
098600050324      * i tempi di risposta
098700050324     c                   if        gcpfas >= 30
098800050324     c                   leave
098900050324     c                   endif
099000050324
099100940919     C                   ENDIF
099200940919      *
099300940919     C                   ELSE
099400940922      * ARRIVO  X DISPOSIZIONE
099500050215     C     KGCA3         READE     tigcp24L                             1620
099600940922     C                   ENDIF
099700940922     C  N20
099800940922     CANN16              DO
099900051005     c                   movel     gcpflr        dgcpflr
100000940919     C* SE SIAMO IN ARR. ED ESECUZIONE CONSIDERO LE GIACENZE CON
100100940919     C* FASE = 31 O 32 E CON DATA ESEGUIBILITA DISPOSIZ. <= VDED
100200940919     C     D82GES        IFEQ      'A'
100300050215     C     GCPFAS        IFEQ      30
100400911030     C     GCPFRG        ANDEQ     0
100500050215     C     GCPFAS        OREQ      31
100600911023     C     GCPFRG        ANDEQ     0
100700050215     C     GCPFAS        OREQ      36
100800050215     C     GCPFRG        ANDEQ     0
100900050408     C     GCPDED        IFLE      AMGDED
101000950216     C     GCPDED        ANDNE     0
101100950216     C     GCPDDM        ORLE      AMGDED
101200050408     C     GCPDED        ANDEQ     0
101300051005     C     D§GCPAPP      ANDne     'A'
101400051006     C     vpar          ANDne     4
101500051006     c* appuntamento
101600051006     C     GCPDDM        ORnE      0
101700051006     C     GCPDED        ANDEQ     0
101800051006     C     D§GCPAPP      ANDeq     'A'
101900051006     C     vpar          ANDeq     4
102000911023     C                   EXSR      WTR
102100940922     C                   END
102200940922     C                   END
102300940922     C                   ENDIF
102400941007     C* SE SIAMO IN PART. CONSIDERO LE GIACENZE CON FASE >= 20 E <=30
102500990628     C* DATA DISPOSIZIONE MITTENTE = 0   VEDI V.L. FIGCP09L
102600940922     C     D82GES        IFEQ      'P'
102700020821      * SOLO QUELLE DI UN CLIENTE e 35 on linea di partenza in gestione
102800940922     C     VPAR          IFEQ      2
102900950208     C     GCPFAS        IFEQ      20
103000950208     C     GCPDDM        ANDEQ     0
103100960606     C     GCPFRG        ANDEQ     0
103200950327     C     GCPFAS        OREQ      25
103300950327     C     GCPDDM        ANDEQ     0
103400960606     C     GCPFRG        ANDEQ     0
103500950327     C     GCPFAS        OREQ      26
103600950327     C     GCPDDM        ANDEQ     0
103700960606     C     GCPFRG        ANDEQ     0
103800911023     C                   EXSR      WTR
103900950208     C                   END
104000940922     C                   ELSE
104100940922      * TUTTE
104200911023     C                   EXSR      WTR
104300940922     C                   ENDIF
104400940919     C                   ENDIF
104500940922     C                   END
104600940922     C                   END
104700940922     C                   ENDIF
104800940922     C                   ENDDO
104900911010     C                   ENDSR
105000911017     C*-----------------------------*
105100911017     C* SCRITTURA RECORD NEL SUBFILE
105200911017     C*-----------------------------*
105300911010     C     WTR           BEGSR
105400990702     C*
105500990702     C                   SETOFF                                       52
105600950118     C                   MOVEL     *BLANKS       VMSG1
105700911023     C                   Z-ADD     GCPFGC        VFGC1
105800911023     C                   Z-ADD     GCPAGC        VAGC1
105900911023     C                   Z-ADD     GCPNGC        VNGC1
106000920110     C                   MOVEL     GIOR          VMGC1
106100920110     C                   MOVE      MESE          VMGC1
106200911023     C                   MOVEL     GCPCMC        VCMC1
106300920318     C* PRELEVO MITT. E DESTINATARIO DALLA BOLLA IN PARTENZA
106400940922     C     D82GES        IFEQ      'P'
106500050215     C                   Z-ADD     GCPAAS        VAAS1
106600050215     C                   Z-ADD     GCPNRS        VNRS1
106700050215     C                   Z-ADD     GCPNSP        VNSP1
106800050215     C                   Z-ADD     GCPLNP        VLNP1
106900940920     C     KBLP          CHAIN     FNBLP01L                           71
107000941019     C  N71              MOVEL     BLPAAS        AMGDSP            8 0
107100941019     C  N71              MOVE      BLPMGS        AMGDSP            8 0
107200940919     C  N71              MOVEL     BLPRSM        VRSM1
107300080603     c  n71              exsr      vediddt
107400960123     C*  CONTROLLO SE ESISTE DESTINATARIO FINALE
107500960123     C     *IN71         IFEQ      '0'
107600960123     C                   CLEAR                   FNLSN0
107700960123     C                   Z-ADD     BLPAAS        DN0AAS
107800960123     C                   Z-ADD     BLPLNP        DN0LNP
107900960123     C                   Z-ADD     BLPNRS        DN0NRS
108000960123     C                   Z-ADD     BLPNSP        DN0NSP
108100960123     C                   MOVEL     'R'           DN0TRC
108200960123     C                   MOVEL     KPJBU         WSVKPJ          256
108300960123     C                   MOVEL     FNLSN0        KPJBU
108400960123     C                   CALL      'FNLSN0R'
108500960123     C                   PARM                    KPJBA
108600960123     C                   MOVEL     KPJBU         FNLSN0
108700960123     C                   MOVEL     WSVKPJ        KPJBU
108800960123     C     DN0TRV        IFEQ      'S'
108900960123     C                   MOVEL     DN0RSD        VRSD1
109000960123     C                   ELSE
109100960123     C                   MOVEL     BLPRSD        VRSD1
109200960123     C                   END
109300960123     C                   END
109400940922     C* SE SIAMO IN PARTENZA CALCOLO CON I GIORNI DI RIENTRO SE LA
109500940922     C* SPEDIZIONE DEVE RIENTRARE
109600940922     C                   EXSR      RIENTR
109700960209      * GESTIONE PARTICOLARITA GIACENZE
109800960209      *                        LA VISUALIZZO SEMPRE
109900950102     C                   SETOFF                                       32
110000031022      * forza "GR" nella particolarità per poter gestire
110100031022      * il reso automaticamente
110200031024      * e si memorizza se non c'era sul file
110300031024     c                   move      '*'           c_eraGGA          1
110400031022     C                   if        BLPGGA = *blank
110500031024     c                   clear                   c_eraGGA
110600031022     c                   eval      BLPGGA = 'GR'
110700031022     c                   end
110800940926     C     BLPGGA        IFNE      *BLANK
110900940926     C                   Z-ADD     1             X
111000940926     C     BLPGGA        LOOKUP    C7Q(X)                                 32
111100940926     C     *IN32         IFEQ      '1'
111200960209     C                   MOVEL     BLPGGA        VMSG1
111300031023      *  Non deve visualizzare "GR" se non è in fase 26
111400031023      *  avendo sostituito il blank con il GR per poter
111500031023      *  decodificare la tabella.
111600031023     c                   if        vmsg1 = 'GR' and gcpFAS <> 26
111700031023     C                   clear                   VMSG1
111800031024     c                   setoff                                       32
111900031023     c                   end
112000031024     c                   if        vmsg1 ='GR' and c_eraGGA = *blank
112100031024     C                   clear                   VMSG1
112200031024     c                   end
112300940926     C                   MOVE      D7Q(X)        DS7Q
112400940926     C                   END
112500940926     C                   ENDIF
112600940926      * DISPOSIZ. RICEVUTE DAL CLIENTE :SONO NELLE NOTE
112700940922     C     GCPFAS        IFEQ      25
112800940926     C                   MOVEL     '*'           VDIS1
112900940922     C                   END
113000940926      * SE FASE =26 HO RICEVUTO UNA RIAPERTURA "FINTA" X IL
113100940926      * CODICE PARTIC. 5A =NON RIAPRIRE MA RENDERE ...
113200940926     C     GCPFAS        IFEQ      26
113300940926     C                   MOVEL     §7Q5DI        VDIS1
113400950104     C                   SETON                                        32
113500940926     C                   END
113600940922     C                   ELSE
113700961220     C*  CONTROLLO SE CI SONO NOTE GIACENZA PER FAX
113800961220     C                   MOVEL     *BLANKS       VDIS1
113900050215     C                   Z-ADD     GCPAASa       VAAS1
114000050215     C                   Z-ADD     GCPNRSa       VNRS1
114100050215     C                   Z-ADD     GCPNSPa       VNSP1
114200050215     C                   Z-ADD     GCPLNPa       VLNP1
114300961220     C                   Z-ADD     GCPAGC        KAGC
114400961220     C                   Z-ADD     GCPFGC        KFGC
114500961220     C                   Z-ADD     GCPNGC        KNGC
114600961220     C                   Z-ADD     GCPFRG        KFRG
114700961220     C                   Z-ADD     35            KFAS
114800050215     C     KGNA          CHAIN     tignp01L                           36
114900961220     C  N36              MOVEL     'F'           VDIS1
115000941007      *    A R R I V O
115100050215     C     Karb          CHAIN     FNARB01L                           71
115200940919     C  N71              MOVEL     ARBRSM        VRSM1
115300940919     C  N71              MOVEL     ARBRSD        VRSD1
115400080603     c  n71              exsr      vediddt
115500940926      * GESTIONE PARTICOLARITA GIACENZE
115600950102     C                   SETOFF                                       32
115700940926     C     ARBGGA        IFNE      *BLANK
115800940926     C                   Z-ADD     1             X
115900940926     C     ARBGGA        LOOKUP    C7Q(X)                                 32
116000940926     C     *IN32         IFEQ      '1'
116100940926     C                   MOVE      D7Q(X)        DS7Q
116200960209     C                   MOVEL     ARBGGA        VMSG1
116300941007      *
116400940926     C                   END
116500940926     C                   ENDIF
116600940922     C                   ENDIF
116700940919     C   71              MOVEL     *ALL'?'       VRSM1
116800940919     C   71              MOVEL     *ALL'?'       VRSD1
116900010920      *
117000010920      * FILTRI SOLO IN PARTENZA
117100050512 1   C     D82GES        IFEQ      'P'
117200090819      *
117300050512 2   C     VPAR          IFNE      3
117400010920     C     VDIS1         OREQ      'R'
117500090819      *
117600050512 3   C     VPAR          IFEQ      4
117700010920     C     VDIS1         ANDEQ     '*'
117800090819      *
117900050512mm   C     VPAR          orEQ      3
118000050512mm   C     gcpfas        ANDEQ     20
118100090819      *
118200050512mm   C     VPAR          orEQ      5
118300050512mm   C     gcpfas        ANDEQ     26
118400090819      *
118500010920     C     VPAR          ORNE      4
118600050512mm   C     VPAR          andne     3
118700050512mm   C     VPAR          andne     5
118800090819      *
118900090819      *   Attenzione  SOLO per la scelta "2" x Specifico CLIENTE:
119000090819      **     se si tratta di disposizioni di RESO e siamo
119100090819      **    nel periodo delle trazioni ridotte, deve essere
119200090819      **    protetto il campo di scelta a meno che non ci sia l'eccezione
119300090819      **    sul cliente nella tab.7U. (il cliente vuole i resi anche nel
119400090819      **    periodo delle trazioni ridotte)
119500090819     C                   IF          VDIS1 = 'R' and Vpar = 2
119600090819     c                                and
119700090819     c                               AMGUDT >= §2GDPI and AMGUDT <= §2GDPF
119800090819      *
119900090819      * Protegge x default il campo scelta
120000090819     C                   SETON                                        52
120100090819      *
120200090819      * Controlla se il cliente vuole comunque il reso
120300090819     C     BLPksc        lookup    k7u                                    88
120400090819      * se non l'ho trovato con KSC provo con CCM
120500090819     c  n88BLPccm        ifne      *zeros
120600090819     C     BLPccm        lookup    k7u                                    88
120700090819     c                   endif
120800090819      * Se l'ha trovato sprotegge la scelta per eseguire il reso    ======
120900090819     C   88              SETOFF                                       52
121000090819      *                                                             ======
121100090819     C                   END
121200090819      *
121300090819      * Memorizza se la scelta deve essere protetta o no
121400090819     c                   eval      H1IN52 = *in52
121500090819      *
121600950224     C                   ADD       1             NRR
121700050512     C                   WRITE     FG82SF
121800050512 3   C                   END
121900050512 2   C                   END
122000090819      *
122100050512 x1  C                   ELSE
122200090819      * IN ARRIVO
122300090819      * Memorizza se la scelta deve essere protetta o no
122400090819     c                   eval      H1IN52 = *in52
122500090819      *
122600950224     C                   ADD       1             NRR
122700950224     C                   WRITE     FG82SF
122800050512 1   C                   END
122900090819      *
123000040922      * forzato limite caricamento a 9000 rekord
123100040922     c                   if        nrr > 9000
123200040922     c                   seton                                        20
123300040922     c                   endif
123400950224     C*
123500050512     c                   z-add     nrr           wnrr              4 0
123600900509     C                   ENDSR
123700911017     C*----------------*
123800911010     C* PULIZIA SUBFILE
123900911017     C*----------------*
124000911010     C     PULIZ         BEGSR
124100911010     C                   SETON                                        70
124200911023     C                   WRITE     FG82CT
124300911023     C                   Z-ADD     0             NRR               4 0
124400911010     C                   SETOFF                                       70
124500911010     C                   EXSR      PULVID
124600911010     C                   ENDSR
124700911010     C*
124800940922     C*----------------*
124900940922     C* CALCOLO SE SPEDIZ. DA RIENTRARE
125000940922     C*----------------*
125100940922     C     RIENTR        BEGSR
125200940922     C** RICERCO SU TARIFFA I GG DI RIENTRO DEL CLIENTE
125300940922     C                   Z-ADD     0             WGGR
125400940922     C                   Z-ADD     GCPSCM        APPO              4 0
125500950116     C                   MOVE      GCPSCM        KKSC
125600950116     C                   MOVE      BLPCTR        KCTR
125700950116     C                   MOVEL     ' '           WTROVA            1
125800020227     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" E TARIFFA DI
125900020227     C* CARTELLO
126000160302     C                   CLEAR                   fnlv59ds
126100020227     C                   CLEAR                   TRUL27
126200021118     C                   MOVE      BLPctr        I27ctb
126300020227     C                   MOVEL     BLPTSP        I27TSP
126400020227     C                   MOVE      BLPLNA        I27LNA
126500020227     C                   MOVE      BLPLNP        I27LNP
126600020227     C                   MOVE      KKSC          I27CLI
126700160302     C                   CALL      'TRUL27R1'
126800020227     C                   PARM                    TRUL27
126900020227     C*
127000950116     C                   Z-ADD     1             Y                 3 0
127100940922     C     APPO          IFNE      8888
127200940922     C     APPO          ANDNE     9999
127300950116     C     APPO          ANDNE     *ZEROS
127400950116     C*  Cerco progressivo tariffa in vigore su
127500150514     C                   MOVEL     ' '           ilv59TBO
127600150514     C                   Z-ADD     AMGDSP        ilv59DSP
127700150514     C                   MOVE      BLPCTR        ilv59CTR
127800150514     C                   MOVE      KKSC          ilv59KSC
127900150514     C                   MOVE      KCI           ilv59KCI
128000000202     C                   MOVEL     KKSC          W0030             3 0
128100150514     C                   MOVEL     O27FIE        ilv59FIE
128200150514     C                   MOVEL     BLPTSP        ilv59TSP
128300150514     C                   MOVEL     'S'           ilv59CON
128400150514     c                   eval      ilv59ta2='B'
128500150514     c                   eval      ilv59blo='S'
128600150514     C                   CALL      'FNLV59R'
128700150514     C                   PARM                    fnlv59ds
128800950116     C*
128900150514     C     olv59ERR      IFEQ      *BLANKS
129000150514     C     olv59tks      ANDEQ     'S'
129100950116     C*  Se ho trovato tariffa in vigore mi posiziono sul file
129200950116     C*  delle tariffe giacenze con il proggressivo giusto
129300150514     C                   MOVE      olv59PRG      KPRG
129400150514     C                   MOVE      ilv59KSC      KKSC
129500150514     C                   MOVE      olv59CTR      KCTR
129600990730     C     KTGC          CHAIN     TITGC01L                           31
129700950116     C     *IN31         IFEQ      '0'
129800950116     C     TGCGGR        ANDNE     *ZEROS
129900950116     C                   MOVE      TGCGGR        WGGR
130000950116     C                   MOVEL     'S'           WTROVA
130100950116     C                   END
130200950116     C                   END
130300950116     C                   END
130400950116     C*  Se non trovato gg. rientro su tariffa cliente li cerco
130500950116     C*  sulla tariffa di cartello
130600950116     C     WTROVA        IFNE      'S'
130700160302     c* Carico la cartello :
130800160302     c*  - per 8888 col fie della bolla
130900160302     c*  - per tariffe cliente esistente col fie della tariffa
131000160302     c                   clear                   trulc7ds
131100160302    2C     olv59ERR      ifeq      *blanks
131200160302     C     olv59tks      andeq     'S'
131300160302     c                   z-add     1             indx              3 0
131400160302     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
131500160302    3c                   if        indx>0 and %editc(olv59prg:'X')=
131600160302     c                             ks_pr(indx)
131700160302     c                   eval      ic7tfie=ks_ie(indx)
131800160302   x3c                   else
131900160302     c                   eval      ic7tntw=o27ntw
132000160302    3c                   endif
132100160302     c
132200160302   x2c                   else
132300160302     c*
132400160302     c                   eval      ic7tntw=o27ntw
132500160302    2c                   endif
132600160302     c
132700160302     C                   move      blpctr        Ic7ctb
132800160302     C                   MOVE      amgdsp        Ic7DTA
132900160302     c                   call      'TRULC7R'
133000160302     c                   parm                    trulc7ds
133100160302     c
133200160302     C                   MOVE      Oc7KSCc       KKSC
133300160302     C                   MOVE      Oc7CTRc       KCTR
133400160302     C                   MOVE      Oc7PRGc       KPRG
133500990730     C     KTGC          CHAIN     TITGC01L                           31
133600950116     C     *IN31         IFEQ      '0'
133700950116     C     TGCGGR        ANDNE     *ZEROS
133800950116     C                   MOVE      TGCGGR        WGGR
133900950116     C                   MOVEL     'S'           WTROVA
134000950116     C                   END
134100950116     C                   END
134200970619     C*  Se non ho impostato i gg rientro li prendo dalla tabella 2G
134300950116     C     WTROVA        IFNE      'S'
134400950116     C                   MOVE      §2GGGL        WGGR
134500950116     C                   END
134600020227     C* Per Estero prendo minore fra gg.rientro estero e tariffa
134700020227     C* Occorre però distinguere fra Estero, DPD, E FEDEX in quanto c'è
134800020227     C* tabella "2G" CON i LORO gg.
134900020227     C     O27FIE        IFNE      'I'
135000020227     C                   SELECT
135100020227      * Se è da DPD
135200050210     C     o27ntw        WHENEQ    'DPD'
135300010921     C     WGGR          IFGT      §2GGDP
135400010921     C                   MOVE      §2GGDP        WGGR
135500010921     C                   END
135600020227      * Se è FEDEX
135700050210     C     o27ntw        WHENEQ    'FED'
135800020227     C     WGGR          IFGT      §2GGFE
135900020227     C                   MOVE      §2GGFE        WGGR
136000020227     C                   END
136100160302      * Se è euroexpress
136200050210     C     o27ntw        WHENEQ    'EEX'
136300050210     C     o27ntw        oreq      'EUP'
136400961213     C     WGGR          IFGT      §2GGRE
136500961204     C                   MOVE      §2GGRE        WGGR
136600010921     C                   END
136700020227     C                   ENDSL
136800961213     C                   END
136900040217      *
137000050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
137100050210     C*   sui giorni da prendere
137200050210     C     o27fie        ifne      'I'
137300050210     C     o27ntw        andeq     'EEX'
137400050210     C                   MOVEL     '20'          COD
137500050210     C                   MOVEL     *blank        KEY
137600050210     C                   MOVEL     Kksc          KEY
137700050210     c     ktab          chain     tabel00f
137800050210     c                   if        %found(tabel00f)
137900050210     c                   movel     tbluni        ds20
138000050210     C                   Z-ADD     §20GRE        wGGR
138100050210     c                   endif
138200050210     c                   end
138300940922      *
138400940922     C* CALCOLO SE LA DATA E' INFERIORE ALLA DATA ODIERNA E QUINDI LA
138500940922     C* MERCE SAREBBE DA RIENTRARE
138600050215     C                   MOVE      GCPDUR        WDAT8             8 0
138700950622     C                   MOVE      WDAT8         G08INV
138800950622     C                   MOVE      '3'           G08ERR
138900950622     C                   CALL      'XSRDA8'
139000950622     C                   PARM                    WLBDA8
139100950622     C     G08TGI        ADD       WGGR          GI8TGI
139200950622     C                   CALL      'XSRGI8'
139300950622     C                   PARM                    WGIDA8
139400020228     c* data + gg rientro è < di oggi
139500950622     C                   Z-ADD     GI8INV        WDAT8
139600940922     C     WDAT8         IFLT      AMGUDT
139700940926     C                   MOVEL     'R'           VDIS1
139800990702     C* Se sono nel periodo di esclusione dei rientri automatici
139900990702     C* proteggo il record del subfile per la selezione
140000990729     C     WDAT8         IFGE      §2GDPI
140100990729     C     WDAT8         ANDLE     §2GDPF
140200990729     C     AMGUDT        ANDGE     §2GDPI
140300990729     C     AMGUDT        ANDLE     §2GDPF
140400990702     C     D82GES        ANDEQ     'P'
140500990729     C* OPPURE
140600990729     C     BLPGGA        OREQ      'G0'
140700990729     C     AMGUDT        ANDGE     §2GDPI
140800990729     C     AMGUDT        ANDLE     §2GDPF
140900990729     C     D82GES        ANDEQ     'P'
141000020108      *
141100020108      *  NON DEVE PROTEGGERE SE SI È ENTRATI SCEGLIENDO
141200020108      *   L'OPZIONE (4)
141300020108     C     VPAR          IFNE      4
141400990702     C                   SETON                                        52
141500020108     C                   END
141600020108      *
141700990702     C                   END
141800940922     C                   ELSE
141900940926     C                   MOVEL     *BLANKS       VDIS1
142000940922     C                   ENDIF
142100940922     C                   ENDSR
142200911017     C*--------------------*
142300911017     C* PULIZIA CAMPI VIDEO
142400911017     C*--------------------*
142500911017     C     PULVID        BEGSR
142600050512     C                   MOVEL     *BLANKS       VSEL
142700911023     C                   MOVE      *ZEROS        VFGC1
142800911023     C                   MOVE      *ZEROS        VAGC1
142900911023     C                   MOVE      *ZEROS        VMGC1
143000911023     C                   MOVE      *ZEROS        VNGC1
143100911023     C                   MOVE      *BLANKS       VCMC1
143200911023     C                   MOVE      *ZEROS        VAAS1
143300911023     C                   MOVE      *ZEROS        VNRS1
143400911023     C                   MOVE      *ZEROS        VNSP1
143500911023     C                   MOVE      *BLANKS       VRSM1
143600911023     C                   MOVE      *BLANKS       VRSD1
143700940926     C                   MOVE      *BLANKS       VDIS1
143800940926     C                   MOVE      *BLANKS       VMSG1
143900911017     C                   ENDSR
144000911022     C*----------------------*
144100911022     C* CONTROLLI 1° VIDEATA
144200911022     C*----------------------*
144300911022     C     CTRVID        BEGSR
144400080424      *
144500940926     C                   SETOFF                                       434445
144600940926     C                   SETOFF                                       464748
144700940926     C                   SETOFF                                       495051
144800080424     C                   SETOFF                                       280654
144900080424      *
145000050513      *accendo indicatore condizionamento opzione 7 vale solo per i resi
145100050513     c                   if        vpar = 3 or vpar = 5
145200050513     c                   seton                                        06
145300050513     c                   endif
145400080424      *
145500080424      * controlla se inserita opzione (6 - DPD Parcel) quando non è
145600080424      *  abilitato a farlo : errore
145700080424      *    Il parcel è solo utilizzabile in Partenza x P.O.DPD
145800080424     c                   if        vpar = 6
145900080424     c                   if         D82GES = 'A'  or
146000080424     c                             (D82GES = 'P' and *in84)
146100080424     C                   SETON                                        2843
146200080424     C                   MOVEL     MSG(19)       $MSG
146300080424     c                   endif
146400080424     c                   endif
146500080424      *
146600911022     C* IN PARTENZA
146700940919     C     D82GES        IFEQ      'P'
146800940919      *
146900911022     C     VPAR          IFEQ      2
147000911022     C* RICERCA CLIENTE
147100911022     C                   MOVEA     VSCM          KSC§
147200940920     C     '?'           LOOKUP    KSC§                                   21
147300940920     C   21              DO
147400911023     C                   Z-ADD     CODUT         CODUT§            1 0
147500911023     C                   Z-ADD     KCI           KCI§              4 0
147600911030     C                   MOVEL     *BLANKS       RICRAG           48
147700911022     C                   MOVEL     VRAG          RICRAG
147800911022     C                   Z-ADD     0             RICKSC            7 0
147900911022     C                   Z-ADD     0             PARSTA            1 0
148000911022     C                   CALL      'XALFA1'
148100911022     C                   PARM                    PARDUT
148200911022     C                   PARM                    CODUT§
148300911022     C                   PARM                    RICRAG
148400911022     C                   PARM                    KCI§
148500911022     C                   PARM                    RICKSC
148600911022     C                   PARM                    PARSTA
148700961016     C                   PARM                    PARFLR           90
148800911022     C     PARSTA        IFNE      -1
148900911030     C                   MOVE      RICKSC        VSCM
149000911030     C                   MOVEL     RICRAG        VRAG
149100911022     C                   ELSE
149200911220     C                   MOVE      *ZEROS        VSCM
149300911030     C                   MOVE      *BLANKS       VRAG
149400911022     C                   END
149500911022     C                   END
149600911022     C* CONTROLLO ESISTENZA CLIENTE
149700911030     C                   MOVE      VSCM          SCM
149800940926     C     KACO          CHAIN     CNACO000                           46
149900940926     C   46              MOVEL     MSG(2)        $MSG
150000940926     C   46              SETON                                        28
150100940919     C                   ENDIF
150200080424      *
150300940926      * SE IMMESSO NR.GIAC. O COD.CLIENTE OBBLIGO   VPAR
150400940926     C     VPAR          IFEQ      *ZEROS
150500940926     C     VNGC          IFNE      *ZEROS
150600940926     C     VSCM          ORNE      *BLANK
150700080424     C     Vparcel       orNE      *Blank
150800940926     C                   SETON                                        2843
150900940926     C                   MOVEL     MSG(4)        $MSG
151000940926     C                   ENDIF
151100940926     C                   ENDIF
151200080424      *
151300080424      * Se scelto il Parcel Nr.
151400080424     c                   if        Vpar = 6
151500080424      *  controlla esistenza del Parcel con il quale agganciare la giacenza
151600080424     C                   if        Vparcel = *blank
151700080424     C                   SETON                                        2854
151800080424     C                   MOVEL     MSG(20)       $MSG
151900080424     c                   else
152000080424      *
152100080424      * Controlla esistenza del Parcel immesso incorciando i dati
152200080424      * del Fiar4 con le giacenze - usando il numero spedizione
152300080424      * come chiave di Join:
152400080424     C/exec sql
152500080424     c+ Select gcpagc, gcpfgc, gcpngc into :vagc, :vfgc, :vngc
152600080424     c+   from Fiar400f join tigcp00f
152700080424     c+   on  ar4aas = gcpaas and ar4lnp = gcplnp and
152800080424     c+       ar4nrs = gcpnrs and ar4nsp = gcpnsp
152900080424     c+   where ar4trc = 'I' and substr(ar4not, 1, 14) = :vParcel
153000080424     c+     and GCPfrg = 0
153100080424     C/end-exec
153200080424     c                   if        SqlCOD < 0 or sqlCOD = 100
153300080424     C                   SETON                                        2854
153400080424     C                   MOVEL     MSG(20)       $MSG
153500080424     c                   else
153600080424      * se qui : ha trovato la giacenza con il Parcel
153700080424      *          e ha caricato la chiave della Giacenza
153800080424     c                   end
153900080424      *
154000080424     c                   endif
154100080424     c                   endif
154200080424      *
154300940919     C                   ENDIF
154400911022     C* IN ARRIVO
154500940919     C     D82GES        IFEQ      'A'
154600911022     C     VSEL1         IFEQ      *BLANKS
154700911022     C     VSEL2         ANDEQ     *BLANKS
154800940926     C                   SETON                                        444528
154900940919     C                   MOVEL     MSG(3)        $MSG
155000911022     C                   END
155100940919     C  N28VPAR          IFEQ      *ZEROS
155200010920     C     VPAR          ORGT      4
155300940926     C                   SETON                                        4328
155400940919     C                   MOVEL     MSG(4)        $MSG
155500911022     C                   END
155600940926     C  N28              DO
155700940926     C     VSEL1         COMP      'X'                                    50
155800940926     C     VSEL2         COMP      'X'                                    51
155900940926     C   50
156000940926     CAN 51              SETON                                        444528
156100941012     C   28              MOVEL     MSG(5)        $MSG
156200940926     C                   ENDDO
156300940919     C  N28
156400940919     CAN 50VPAR          IFEQ      3
156500940926     C                   SETON                                        4428
156600940926     C                   MOVEL     MSG(6)        $MSG
156700911022     C                   END
156800051006     C  N28
156900051006     CAN 50VPAR          IFEQ      4
157000051006     C                   SETON                                        4428
157100051006     C                   MOVEL     MSG(18)       $MSG
157200051006     C                   END
157300940919     C  N28VPAR          IFEQ      1
157400911022     C     VFGC          IFEQ      0
157500911022     C     VNGC          OREQ      0
157600940926     C                   SETON                                        4828
157700940926     C                   MOVEL     MSG(7)        $MSG
157800911022     C                   END
157900940926     C                   END
158000940919     C  N28VPAR          IFEQ      2
158100911022     C     VLNP          IFEQ      0
158200911022     C     VNSP          OREQ      0
158300940926     C                   SETON                                        4628
158400940926     C                   MOVEL     MSG(7)        $MSG
158500911022     C                   END
158600911022     C                   END
158700940919     C  N28VPAR          IFEQ      3
158800940919     C     VDIS          ANDEQ     *BLANKS
158900940926     C                   SETON                                        4728
159000940926     C                   MOVEL     MSG(7)        $MSG
159100911022     C                   END
159200940926      * CONTROLLO DATA ESEGUIBILTA DISP.
159300940926     C  N28VPAR          IFEQ      3
159400940926     C                   Z-ADD     VDED          G08DAT
159500940926     C                   MOVE      *ZEROS        G08INV
159600940926     C                   MOVEL     *BLANK        G08ERR
159700940926     C                   CALL      'XSRDA8'
159800940926     C                   PARM                    WLBDA8
159900940926     C     G08ERR        IFEQ      '1'
160000940926     C                   SETON                                        2849
160100940926     C                   MOVEL     MSG(13)       $MSG
160200940926     C                   ELSE
160300940926     C                   Z-ADD     G08INV        AMGDED            8 0
160400940926     C                   ENDIF
160500051006     c                   else
160600051006     c                   clear                   amgded
160700940926     C                   ENDIF
160800940919     C                   ENDIF
160900080424      *
161000911022     C                   ENDSR
161100050524      **********************************************************************
161200050524      * Subroutine - Sort
161300050524      *   This subroutine sorts the subfile records.
161400050524     C     Sort          BEGSR
161500050524     C                   CLEAR                   vrsm1
161600050524     C                   CLEAR                   vrsm1w
161700050613     C                   EVAL      nrr = 0
161800050524
161900050524      * Initialize the key fields to sort on. There is one extra field not in
162000050524      * the subfile -- Selected -- that is added to the record. This field
162100050524      * is used to place selected records in front of nonselected records.
162200050524
162300050524     C                   CLEAR                   QLGSCB
162400050524     C                   CLEAR                   QLGSCB00
162500050524
162600050524     ** Gestione della scelta ordinamento.
162700050524     C                   SELECT
162800050525     ** Ordinamento per mittente
162900050525     C                   WHEN      WrkSort = mittente
163000050524     ** 1 campo chiave.
163100050524     C                   EVAL      QLGNBRK    = 1
163200050525     ** il mittente è
163300050524     ** a posizone 42, 13 byte, char, ascending.
163400050525     C                   EVAL      QLGSP      = 40
163500050524     C                   EVAL      QLGSS      = %SIZE(vrsm1)
163600050524     C                   EVAL      QLGDT      = Carattere
163700050524     C                   EVAL      QLGSO      = Ascendente
163800050524     C                   EVAL      QLGKL(1)   = QLGSKL
163900050525     ** Al prossimo F11 ordino per giacenza
164000050525     C*                  EVAL      WrkSort = giacenza
164100050524     ** Ordinamento per Codice
164200050525     C                   WHEN      WrkSort = giacenza
164300050524     C                   EVAL      QLGNBRK    = 1
164400050524     ** Il codice è
164500050525     ** a posizione 12, 7 byte, ascending
164600050525     C                   EVAL      QLGSP      = 12
164700050525     C                   EVAL      QLGSS      = %SIZE(vngc1)
164800050524     C                   EVAL      QLGDT      = Carattere
164900050524     C                   EVAL      QLGSO      = Ascendente
165000050524     C                   EVAL      QLGKL(1)   = QLGSKL
165100050525     ** Al prossimo F11 ordino per mittente
165200050525     C*                  EVAL      WrkSort = Mittente
165300050524     C                   ENDSL
165400050524      * Load other sort parameters.
165500050524     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
165600050524     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
165700050524     C                   EVAL      QLGRT     = 8
165800050524     C                   EVAL      QLGOKL    = 80
165900050524     C                   EVAL      QLGLKE    = 16
166000050524     C                   EVAL      QLGLSS    = 290
166100050524      * Initialize Sort I/O API fields.
166200050524     C                   EVAL      QLGRL00  = QLGRL
166300050524     C                   EVAL      QLGRC00  = 1
166400050524     C                   CLEAR                   QUSEI
166500050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
166600050524      * First step - Initialize the sort routine.
166700050524     C                   CALL      'QLGSORT'
166800050524     C                   PARM                    QLGSCB
166900050524     C                   PARM                    NotUsed
167000050524     C                   PARM                    NotUsed
167100050524     C                   PARM                    SizeList
167200050524     C                   PARM                    ReturnSize
167300050524     C                   PARM                    QUSEC
167400050524      * Next step - Write records to I/O routine.
167500050524     C                   EVAL      QLGRT00 = Put
167600050613     C                   DO        RrnLast       nrr
167700050613     C     nrr           CHAIN     fg82sf
167800050524     ** Solo le righe con Selected = 'Y' sono riordinate,
167900050524     ** quindi per fare un ordinamento di tutte le righe
168000050524     ** metto 'Y' sempre.
168100050524     C                   EVAL      Selected = 'Y'
168200050524     C                   CLEAR                   QUSEI
168300050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
168400050524     C                   CALL      'QLGSRTIO'
168500050524     C                   PARM                    QLGSCB00
168600050524     C                   PARM                    SflRcd
168700050524     C                   PARM                    NotUsed
168800050524     C                   PARM                    SizeList
168900050524     C                   PARM                    NotUsed
169000050524     C                   PARM                    QUSEC
169100050524     C                   ENDDO
169200050524      * Next step - Signal end of input, clear subfile for reload.
169300050524     C                   EVAL      QLGRT00 = EndPut
169400050524     C                   CLEAR                   QUSEI
169500050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
169600050524
169700050524     C                   CALL      'QLGSRTIO'
169800050524     C                   PARM                    QLGSCB00
169900050524     C                   PARM                    SflRcd
170000050524     C                   PARM                    NotUsed
170100050524     C                   PARM                    SizeList
170200050524     C                   PARM                    NotUsed
170300050524     C                   PARM                    QUSEC
170400050524     c                   seton                                        70
170500050524     C                   WRITE     fg82ct
170600050524     C                   setoff                                       70
170700050524      * Final step - Write the records back to the subfile.
170800050524     C                   EVAL      QLGRT00 = Get
170900050613     C                   DO        RrnLast       nrr
171000050524     C                   CLEAR                   QUSEI
171100050524     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
171200050524     C                   CALL      'QLGSRTIO'
171300050524     C                   PARM                    QLGSCB00
171400050524     C                   PARM                    NotUsed
171500050524     C                   PARM                    SflRcd
171600050524     C                   PARM                    QLGRL00
171700050524     C                   PARM                    NotUsed
171800050524     C                   PARM                    QUSEC
171900050525     c                   if        vmsg1 <> *blank
172000050525     c                   seton                                        32
172100050525     c                   else
172200050525     c                   setoff                                       32
172300050525     c                   endif
172400090819     c                   eval        *in52 = H1IN52
172500050524     C                   WRITE     fg82sf
172600050524     C                   ENDDO
172700050524     C                   ENDSR
172800080603     c*-----------------------------------------------------------------
172900080603     c     vediddt       begsr
173000080603     c*-----------------------------------------------------------------
173100080603      * Flag DDT SI  ar4 trek A
173200080603     c                   clear                   vddts
173300080603     c                   clear                   dsbl4a
173400080603     c                   move      'A'           trek4
173500080603     c     kar4          chain     fiar401l
173600080603     c                   if        %found(fiar401l)
173700080603     c                   movel     AR4NOT        dsbl4a
173800080603     c                   if        §B4ABM = 'S' or §B4ABM = 'P' or
173900080603     c                             §B4ABM = 'C' or §B4ABM = 'Q'
174000080603     c                   eval      vddts = 'S'
174100080603     C                   endif
174200080603     C                   endif
174300080603     c                   endsr
174400050524     C*-----------------------------------------------------------------
174500940919     C*   I N Z S R
174600050524     C*-----------------------------------------------------------------
174700940919     C     *INZSR        BEGSR
174800940919     C     *ENTRY        PLIST
174900940919     C                   PARM                    KPJBA
175000940919      *
175100940919     C                   MOVEL     KPJBU         FNLG82
175200140123     C                   MOVEL     Knmus         knmus2
175300940920      *
175400940919     C                   MOVEL     NOMPGM        VPGM
175500940919     C                   Z-ADD     1             CODUT
175600940919     C                   CALL      'X§PARUT'
175700940919     C                   PARM                    UTEDSE
175800940919     C                   MOVEL     RAGUT         RSUT             20
175900940919     C                   MOVEL     RAGUT         PARDUT           30
176000060925     C**mm               MOVEL     REC80         CNCR80
176100140123     C                   MOVEL     KNMUS         §SEI
176200020228     c* una 2° livello
176300020730     C**** §PRF          COMP      'REM'                                  01
176400020730     C**N01              Z-ADD     SIMFEL        §FLE
176500060804     C**MM               Z-ADD     SIMpou        §FLE
176600940919      *
176700060804     c     *dtaara       define    §azute        azuteds
176800060804     c     *dtaara       define    §datiute      ddatiute
176900060804     c                   in(E)     *dtaara
177000060804    1c                   If        %error  or RSUT = *blanks
177100060804     c                   Clear                   Tibs34ds
177200060804     c                   Call      'TIBS34R'
177300060804     c                   Parm                    Tibs34ds
177400060804     c                   In        *dtaara
177500060804    1c                   EndIf
177600060804
177700060804     c                   Movel     UteFaf        Dute01
177800060804     c                   If        §Utefgia = *Blanks and kcdaz = 'XG83'
177900150911     c                   call      'XUTENOAB'
178000150911     c                   parm                    kpjba
178100060804     c                   seton                                        lr
178200060804     c                   Endif
178300060925     c                   z-add     dutpou        §fle
178400060925     c* TEST SE SONO IN PARTENZA O IN ARRIVO
178500940919     C     D82GES        IFEQ      'A'
178600940919     C                   SETON                                        05
178700050215     C                   OPEN      tignp01L
178800050215     C                   OPEN      tigcp21L
178900050215     C                   OPEN      tigcp02L
179000050215     C                   OPEN      tigcp24L
179100940920     C                   OPEN      FNARB01L
179200940920     C                   ELSE
179300050215     C                   OPEN      tigcp02L
179400050215     C                   OPEN      tigcp07L
179500050215     C                   OPEN      tigcp06L
179600941006     C                   OPEN      FNBLP01L
179700990730     C                   OPEN      TITGC01L
179800940920     C                   OPEN      CNACO00F
179900940920     C                   ENDIF
180000940919      * CARICO FILIALI IN GESTIONE
180100940919     C                   MOVEL     *BLANKS       KEY
180200940919     C                   MOVEL     §FLE          KEY
180300940922     C                   MOVEA     *ZEROS        F£6
180400020503     c                   if        *in05
180500020503     c                   clear                   trul06ds
180600020503     c                   eval      d06cod = '£6'
180700020503     c                   movel     §fle          d06key
180800020503     c                   movel     trul06ds      kpjbu
180900020503     c                   call      'TRUL06R'
181000020503     c                   parm                    kpjba
181100020503     c                   movel     kpjbu         trul06ds
181200020503     c                   movea     lin           f£6
181300020503     c                   else
181400140123     c                   clear                   trovata
181500940919     C  N05              MOVE      '£7'          COD
181600140102      * forzo caricamento filiali dpd per profilo mirato
181700140123     c                   if        %subst(knmus:1:3) = 'INT'
181800140123     c                   movel(p)  §mussei       key
181900140123     C     KTAB          CHAIN     TABEL                              14
182000140123     C  N14              MOVEA     TBLUNI        F£6
182100140123     c  n14              move      'X'           trovata
182200140123     c   14              movel(p)  §fle          key
182300140123     c                   endif
182400140123      *
182500140123     c                   if        trovata = *blank
182600940919     C     KTAB          CHAIN     TABEL                              15
182700940922     C  N15              MOVEA     TBLUNI        F£6
182800940922     C   15              MOVE      §FLE          F£6(1)
182900020503     c                   end
183000140123     c                   endif
183100940923      * CARICO FILIALI GESTITE SULL'AS
183200020503     C* N15              MOVEA     TBLUNI        F£1
183300020503     c                   clear                   trul06ds
183400020503     c                   eval      d06cod = '£1'
183500020503     c                   movel     §fle          d06key
183600020503     c                   movel     trul06ds      kpjbu
183700020503     c                   call      'TRUL06R'
183800020503     c                   parm                    kpjba
183900020503     c                   movel     kpjbu         trul06ds
184000020503     c                   movea     lin           f£1
184100950116      * CARICO DS 2G
184200950116     C                   MOVEL     *BLANKS       KEY
184300950116     C                   MOVEL     '1'           KEY
184400950116     C                   MOVE      '2G'          COD
184500950116     C                   CLEAR                   DS2G
184600950116     C     KTAB          CHAIN     TABEL                              15
184700950116     C  N15              MOVEL     TBLUNI        DS2G
184800940926      * CARICO TABELLE PARTICOLARITà GIACENZE
184900940926     C*---------------------------------------------------------------*
185000940926     C* Carico tabella 7Q
185100950116     C                   Z-ADD     0             X
185200950116     C                   MOVEL     '7Q'          COD
185300950116     C     KTAB2         CHAIN     TABEL                              31
185400950116     C     *IN31         DOWEQ     '0'
185500950116     C     X             ANDLE     50
185600950116     C     TBLFLG        IFEQ      ' '
185700950116     C                   ADD       1             X
185800950116     C                   MOVEL     TBLKEY        C7Q(X)
185900950118     C                   MOVEL     TBLUNI        D7Q(X)
186000950116     C                   END
186100950116     C     KTAB2         READE     TABEL                                  31
186200950116     C                   END
186300940926      *
186400940926     C*  Se non ho caricato tutti i dati gestisco errore
186500940926     C     *IN31         IFNE      '1'
186600940926     C                   SETON                                            28
186700950116     C                   MOVEL     MSG(14)       $MSG
186800940926     C                   END
186900950116      *
187000950116     C*  Se non ho caricato tutti i dati gestisco errore
187100950116     C     *IN31         IFNE      '1'
187200950116     C                   SETON                                            28
187300950116     C                   MOVEL     MSG(16)       $MSG
187400950116     C                   END
187500090819      *
187600090819     C*  Caricamento tabella 7U clienti che vogliono il reso anche nel
187700090819     C*                         periodo delle trazioni ridotte
187800090819     c                   clear                   SK_in_Capienza    1
187900090819     C                   Z-ADD     0             XX                4 0
188000090819     C                   MOVEL     '7U'          COD
188100090819     C     KTAB2         CHAIN     TABEL                              31
188200090819     C     *IN31         DOWEQ     '0'
188300090819     C     X             ANDLT     999
188400090819     C                   MOVE      TBLkey        tipoQ             1
188500090819     C                   MOVEL     TBLUNI        DS7U
188600090819     C     TBLFLG        IFEQ      ' '
188700090819     C     tipoQ         andeq     'Q'
188800090819     C     §7USRE        andeq     'S'
188900090819     C                   ADD       1             XX
189000090819     c                   movel     tblkey        alfa7             7
189100090819     C                   movel     alfa7         K7u(XX)
189200090819     C     XX            ifeq      950
189300090819     c                   move      'S'           SK_in_Capienza
189400090819     c                   end
189500090819     C                   END
189600090819     C     KTAB2         READE     TABEL                                  31
189700090819     C                   END
189800090819      *
189900090819     C*  Se non ho caricato tutti i dati gestisco errore
190000090819     c                   If         SK_in_Capienza = 'S'
190100090819     C     *IN31         IFNE      '1'
190200090819     C                   SETON                                            28
190300090819     C                   MOVEL     MSG(21)       $MSG
190400090819     C                   ElsE
190500090819     C                   SETON                                            28
190600090819     C                   MOVEL     MSG(22)       $MSG
190700090819     C                   END
190800090819     C                   END
190900090819     C*---------------------------------------------------------------*
191000940926      *
191100940919     C*--- RICERCA CAPOCONTI
191200940919     C                   DO        50            X                 2 0
191300940919     C                   MOVE      TCU(X)        TCUDS
191400940919     C     F56           CABNE     'CG'          END1
191500940919     C     F4            COMP      '1'                                    21
191600940919     C** 1 CLIENTI   21
191700940919     C     F3            COMP      '0'                                242425
191800940919     C** 0 ITALIA   25
191900940919     C** 1 ESTERO   24
192000940919     C   21
192100940919     CAN 24              Z-ADD     KCU(X)        KCE               4 0
192200940919     C   21
192300940919     CAN 25              Z-ADD     KCU(X)        KCI               4 0
192400940919     C     END1          TAG
192500940919     C                   END
192600940919     C*§
192700940920      * UDATE
192800940920     C                   TIME                    WHHUDT           14 0
192900940922     C                   MOVE      WHHUDT        G08DAT
193000940920     C                   MOVE      *ZEROS        G08INV
193100940920     C                   MOVEL     *BLANK        G08ERR
193200940920     C                   CALL      'XSRDA8'
193300940920     C                   PARM                    WLBDA8
193400940920     C                   Z-ADD     G08INV        AMGUDT            8 0
193500940920     C                   Z-ADD     G08DAT        GMAUDT            8 0
193600940920     C                   MOVEL     AMGUDT        ANNO              4 0
193700020228     C* gg t.cliente                                       *
193800940922     C     *LIKE         DEFINE    TGCGGR        WGGR
193900950116     C     *LIKE         DEFINE    TGCCTR        KCTR
194000950116     C     *LIKE         DEFINE    TGCKSC        KKSC
194100950116     C     *LIKE         DEFINE    TGCPRG        KPRG
194200940922     C     *LIKE         DEFINE    GCPFGC        FLE
194300940922     C     *LIKE         DEFINE    GCPSCM        SCM
194400961220     C     *LIKE         DEFINE    GNPAGC        KAGC
194500961220     C     *LIKE         DEFINE    GNPFGC        KFGC
194600961220     C     *LIKE         DEFINE    GNPNGC        KNGC
194700961220     C     *LIKE         DEFINE    GNPFRG        KFRG
194800961220     C     *LIKE         DEFINE    GNPFAS        KFAS
194900050324     C     *LIKE         DEFINE    GcPFAS        FAS
195000031029     c     *like         define    gcpFRG        savFRG
195100940919     C*  KLIST
195200940919     C*
195300940919     C     KGCA1         KLIST
195400940919     C                   KFLD                    VAGC
195500940919     C                   KFLD                    VFGC
195600940920     C                   KFLD                    VNGC
195700940919     C                   KFLD                    FRG
195800031029     C*
195900031029     C     KGCA1_GT      KLIST
196000031029     C                   KFLD                    VAGC
196100031029     C                   KFLD                    VFGC
196200031029     C                   KFLD                    VNGC
196300031029     C*
196400961220     C     KGNA          KLIST
196500961220     C                   KFLD                    KAGC
196600961220     C                   KFLD                    KFGC
196700961220     C                   KFLD                    KNGC
196800961220     C                   KFLD                    KFRG
196900961220     C                   KFLD                    KFAS
197000940919     C*
197100940919     C     KGCA2         KLIST
197200940919     C                   KFLD                    VAAS
197300940919     C                   KFLD                    VLNP
197400940919     C                   KFLD                    VNRS
197500940919     C                   KFLD                    VNSP
197600990628     C                   KFLD                    FRG               2 0
197700031029     C*
197800031029     C     KGCA2_GT      KLIST
197900031029     C                   KFLD                    VAAS
198000031029     C                   KFLD                    VLNP
198100031029     C                   KFLD                    VNRS
198200031029     C                   KFLD                    VNSP
198300940919     C*
198400940919     C     KGCA3         KLIST
198500940919     C                   KFLD                    FLE
198600940919     C                   KFLD                    VDIS
198700940919     C*
198800940919     C     KTAB          KLIST
198900940919     C                   KFLD                    CODUT
199000940919     C                   KFLD                    COD               2
199100940919     C                   KFLD                    KEY               8
199200940926     C*
199300940926     C     KTAB2         KLIST
199400940926     C                   KFLD                    CODUT
199500940926     C                   KFLD                    COD
199600940919     C*
199700940919     C     KBLP          KLIST
199800940919     C                   KFLD                    GCPAAS
199900940919     C                   KFLD                    GCPLNP
200000940919     C                   KFLD                    GCPNRS
200100940919     C                   KFLD                    GCPNSP
200200050215     C*
200300050215     C     Karb          KLIST
200400050215     C                   KFLD                    GCPAASa
200500050215     C                   KFLD                    GCPLNPa
200600050215     C                   KFLD                    GCPNRSa
200700050215     C                   KFLD                    GCPNSPa
200800940922     C*
200900940919     C     KACO          KLIST
201000940919     C                   KFLD                    CODUT
201100940919     C                   KFLD                    KCI
201200940919     C                   KFLD                    SCM
201300940922     C*
201400940922     C     KTGC          KLIST
201500940922     C                   KFLD                    KKSC
201600940922     C                   KFLD                    KCTR
201700940922     C                   KFLD                    KPRG
201800031217     C*
201900031217     C     KBLP2         KLIST
202000040108     C                   KFLD                    vAAS1
202100040108     C                   KFLD                    vLNP1
202200040108     C                   KFLD                    vNRS1
202300040108     C                   KFLD                    vNSP1
202400940922     C*
202500050215     C     Kscm          KLIST
202600050215     C                   KFLD                    FLE
202700050215     C                   KFLD                    scm
202800050215     C*
202900080603     c     kar4          klist
203000080603     c                   kfld                    vAAS1
203100080603     c                   kfld                    vLNP1
203200080603     c                   kfld                    vNRS1
203300080603     c                   kfld                    vNSP1
203400080603     c                   kfld                    trek4             1
203500080603     c                   move      'A'           trek4
203600080603      *
203700050324     C     Kflefas       KLIST
203800050324     C                   KFLD                    FLE
203900050324     C                   KFLD                    fas
204000050324     c                   z-add     20            fas
204100060824      * azione forzata XG83 forzatura campi
204200060824     c                   if        kcdaz = 'XG83'
204300060824     c                   seton                                        83
204400060824     c                   move      '1'           vpar
204500060824     c                   endif
204600050324     C*
204700940919     C* FINTO OPEN X RENDERE SHR YES  I FILE
204800941223     C                   OPEN      FNLV22P
204900940919     C   LR
205000940919     CANNLR              EXCEPT    FINTO
205100940919     C                   ENDSR
205200941223     OFNLV22P   E            FINTO
205300950116** MSG
205400940926Giacenza non in Gestione !                                                     1
205500940926Cliente non trovato nel Piano dei Conti                                        2
205600940926Immettere una scelta                                                           3
205700940926Nessuna parzializzazione immessa                                               4
205800940926Selezionare O Immissione Disposizioni  O  Esecuzione Disposizioni              5
205900940926Per questa scelta è possibile immettere o il Nr.Giacenza o il Nr.Spediz.       6
206000940926Per la parzializzazione scelta i dati non sono completi                        7
206100940926Giacenza non trovata !                                                         8
206200050808Giacenza con Disposizioni già immesse o ancora da stampare !                   9
206300940926Nessuna giacenza da visualizzare !                                            10
206400940926Giacenza con disposizioni ancora da Inserire o già  Eseguite                  11
206500961114Punto operativo non in gestione su questo Elaboratore                         12
206600940926Data Errata !                                                                 13
206700940926AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7Q"     14
206800941012Giacenza con data eseguibilità disposizioni > della data odierna              15
206900950116AVVISARE C.E.D. !! Impossibile caricare tutti i cod. NAZIONI  "15"            16
207000050805Impossibile immettere disposizioni oltre le riaperture consentite !!          17
207100051006Se si scegli opzione 4 è obbligatorio selezionare esecuzione disposizioni     18
207200080424Immettere una scelta da 1 a 5                                                 19
207300080424Il Nr.Parcel non è valido o non ha una giacenza collegata.                    20
207400090819AVVISARE C.E.D. !! Impossibile caricare tutti i cod. Partic.Giacenza "7U"     21
207500090819AVVISARE C.E.D. !! Tab.: cod.Partic.Giacenza "7U" verso limite massimo.       22
