000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200920113     H* FLFG43R *----------------------------------------------------*
000300900418     H*                                                              *
000400931119     H*       - LISTE CONTROLLO GIACENZE IN ARRIVO -                 *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700050223     Ftigcp21L  IF   E           K DISK
000800050223     Ftigcp24L  IF   E           K DISK
000900050223     F                                     RENAME(tigcp000:tigcp024)
001000050223     Ftignp01L  IF   E           K DISK
001100950112     FFNARB01L  IF   E           K DISK
001200990929     FFIAR601L  IF   E           K DISK
001300051115     FFiar901L  IF   E           K DISK
001400060213     FFiAR401L  IF   E           K DISK
001500990803     FFNEVB01L  IF   E           K DISK
001600900515     FTABEL00F  IF   E           K DISK
001700950112     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001800950406     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001900950406     F                                     USROPN
002000950111      *--------------------------------------------------------------*
002100980205     D ST              S             66    DIM(17) CTDATA PERRCD(1)
002200950406     D CMD             S             50    DIM(1) CTDATA PERRCD(1)
002300950112     D C7Q             S              2    DIM(100)
002400950112     D D7Q             S             89    DIM(100)
002500990929     D C3A             S              2    DIM(200)
002600990929     D T3A             S              1    DIM(200)
002700950112      *----------------------------------------------------------------
002800950112     D FNLG40        E DS                  EXTNAME(FNLG40DS)
002900950116     D TRUL52        E DS                  EXTNAME(TRUL52DS)
003000020717     D OG143         E DS
003100950112     D DS2A          E DS
003200990929     D DS3A          E DS
003300911220     D DS2G          E DS
003400950112     D DS7Q          E DS
003500900423     D TCUDS           DS
003600900423     D  F3                     3      3
003700900423     D  F4                     4      4
003800900423     D  F56                    5      6
003900950112     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004000950112     D  TCU                  398    697
004100900418     D                                     DIM(50)
004200950112     D  KCU                  698    847P 0
004300900418     D                                     DIM(50)
004400900418     D                                     PACKEVEN
004500950112     D CNCR80        E DS
004600020820     d trul06ds      e ds
004700020820     d  lin                    1     90  0 dim(30)
004800950112     D WLBDA8          DS
004900950112     D  G02DAT                 1      8  0
005000950112     D  G02INV                 9     16  0
005100950112     D  G02ERR                17     17
005200950112     D  G02TGI                18     22  0
005300900418     D KPJBA         E DS
005400940330     D*-------------
005500990906     D* DS PER ROUTINE 'XGGFEST'
005600940330     D*-------------
005700950112     D WDAT8           DS
005800950112     D  DATADA                 1      8  0
005900950112     D  DATAA                  9     16  0
006000950112     D  GGF                   17     21  0
006100961227     D*
006200970113     D COST1           C                   CONST('REDELIVER         ')
006300970113     D COST2           C                   CONST('SEND SHPMT BACK   ')
006400950112      *----------------------------------------------------------------
006500950112     IAZORG01L  AA
006600980520     I                                  1    3  ORGDIT
006700950112     I                             P    4    5 0ORGFIL
006800950112     I                                  6    6  ORGFVA
006900971113     I                                 11   11  ORGFL1
007000950112     I                                 14   34  ORGDES
007100020717     I                               3776 3800  ORGDE3
007200020717     I                               3801 3825  ORGDE4
007300950407     I                             P 4281 4282 0ORGFEL
007400900514     C*---------------------------------------------------------------*
007500950112     C*  Controllo il tipo di stampa scelta
007600950112     C     D40SC1        CASEQ     '1'           LISTA1
007700950112     C     D40SC2        CASEQ     '1'           LISTA2
007800950112     C     D40SC4        CASEQ     '1'           LISTA4
007900950112     C                   END
008000900514     C**
008100950112     C     FINE          TAG
008200900514     C                   SETON                                        LR
008300900514     C*---------------------------------------------------------------*
008400950112     C*  *INZSR: Operazioni iniziali                                  *
008500900514     C*---------------------------------------------------------------*
008600950112     C     *INZSR        BEGSR
008700950112     C*
008800900514     C     *ENTRY        PLIST
008900900514     C                   PARM                    KPJBA
009000950112     C                   MOVEL     KPJBU         FNLG40
009100950112     C*---------------------------------------------------------------*
009200950112     C                   Z-ADD     1             CODUT
009300950112     C                   CALL      'X§PARUT'
009400950112     C                   PARM                    UT§DSE
009500950112     C                   MOVEL     REC80         CNCR80
009600950112     C     CODUT         CABEQ     -1            FINE
009700950112     C                   MOVEL     RAGUT         RSUT             20
009800020820     c                   clear                   trul06ds
009900020820     c                   eval      d06cod = '£1'
010000020820     c                   movel     simfel        d06key
010100020820     c                   movel     trul06ds      kpjbu
010200020820     c                   call      'TRUL06R'
010300020820     c                   parm                    kpjba
010400020820     c                   movel     kpjbu         trul06ds
010500950112     C*---------------------------------------------------------------*
010600950112     C*  Definizione delle chiavi
010700980520     C     KAR4          KLIST
010800050223     C                   KFLD                    GCPAASa
010900050223     C                   KFLD                    GCPLNPa
011000050223     C                   KFLD                    GCPNRSa
011100050223     C                   KFLD                    GCPNSPa
011200990628     C                   KFLD                    KTRC
011300950112     C     KACO          KLIST
011400950112     C                   KFLD                    CODUT
011500950112     C                   KFLD                    KCI
011600950112     C                   KFLD                    GCPSCM
011700950112     C     KGAP          KLIST
011800050223     C                   KFLD                    GCPAASa
011900050223     C                   KFLD                    GCPLNPa
012000050223     C                   KFLD                    GCPNRSa
012100050223     C                   KFLD                    GCPNSPa
012200990929     C     KAR6          KLIST
012300050223     C                   KFLD                    GCPAASa
012400050223     C                   KFLD                    GCPLNPa
012500050223     C                   KFLD                    GCPNRSa
012600050223     C                   KFLD                    GCPNSPa
012700990929     C                   KFLD                    KTRC
012800950112     C     KTAB          KLIST
012900950112     C                   KFLD                    CODUT
013000950112     C                   KFLD                    COD               2
013100950112     C                   KFLD                    KEY               8
013200950112     C     KNOT          KLIST
013300950112     C                   KFLD                    GCPAGC
013400950112     C                   KFLD                    GCPFGC
013500950112     C                   KFLD                    GCPNGC
013600950112     C                   KFLD                    GCPFRG
013700950112     C                   KFLD                    §FAS
013800950112     C     KART          KLIST
013900050223     C                   KFLD                    GCPAASa
014000050223     C                   KFLD                    GCPLNPa
014100050223     C                   KFLD                    GCPNRSa
014200050223     C                   KFLD                    GCPNSPa
014300950112     C     KTAB1         KLIST
014400950112     C                   KFLD                    CODUT
014500950112     C                   KFLD                    COD               2
014600950307     C*
014700950307     C     KEVB          KLIST
014800990803     C                   KFLD                    KAAS
014900950307     C                   KFLD                    KLNP
015000950307     C                   KFLD                    KNRS
015100950307     C                   KFLD                    KNSP
015200950307     C                   KFLD                    KDEV
015300950609     C*
015400950609     C     KEVBS         KLIST
015500990803     C                   KFLD                    KAAS
015600950609     C                   KFLD                    KLNP
015700950609     C                   KFLD                    KNRS
015800950609     C                   KFLD                    KNSP
015900950112     C*---------------------------------------------------------------*
016000950112     C     *LIKE         DEFINE    G02TGI        §GGD
016100950112     C     *LIKE         DEFINE    G02TGI        §GGA
016200950112     C     *LIKE         DEFINE    G02TGI        §GGDI
016300950112     C     *LIKE         DEFINE    GCPFAS        §FAS
016400950112     C     *LIKE         DEFINE    G02DAT        §DTRIA
016500950307     C     *LIKE         DEFINE    EVBDEV        KDEV
016600990803     C     *LIKE         DEFINE    EVBAAS        KAAS
016700950307     C     *LIKE         DEFINE    ARBLNP        KLNP
016800950307     C     *LIKE         DEFINE    ARBNRS        KNRS
016900950307     C     *LIKE         DEFINE    ARBNSP        KNSP
017000950407     C     *LIKE         DEFINE    GCPLNP        WLNP
017100950407     C     *LIKE         DEFINE    GCPLNP        SAVLNP
017200980520     C     *LIKE         DEFINE    AR4TRC        KTRC
017300950112     C*---------------------------------------------------------------*
017400950406     C* Eseguo OVRPRTF sul file di stampa
017500950406     C                   Z-ADD     50            LUNG             15 5
017600950406     C                   MOVEL     *BLANKS       COMMAN           50
017700950406     C                   MOVEL     CMD(1)        COMMAN
017800950406     C                   CALL      'QCMDEXC'
017900950406     C                   PARM                    COMMAN
018000950406     C                   PARM                    LUNG
018100950406     C                   OPEN      PRTF198
018200950406     C*
018300950112     C                   SETOFF                                       404143
018400950112     C                   SETON                                        OF
018500950112     C                   Z-ADD     0             SAVCCR
018600950112     C                   TIME                    WTIME             6 0
018700950112     C                   TIME                    WHHDAT           14 0
018800950112     C                   MOVE      WHHDAT        OGGI              8 0
018900950112     C                   MOVE      *ALL'-'       CMP1            198
019000950112     C                   MOVE      *ALL'-'       CMP2            198
019100900514     C*--- RICERCA CAPOCONTI------------------------------------------*
019200900514     C                   DO        50            X                 2 0
019300900514     C                   MOVE      TCU(X)        TCUDS
019400950112     C     F56           IFEQ      'CG'
019500950112     C     F4            ANDEQ     '1'
019600950112     C     F3            ANDEQ     '0'
019700950112     C                   Z-ADD     KCU(X)        KCI               4 0
019800950112     C                   END
019900900514     C                   END
020000950112     C*
020100961219     C                   CLEAR                   AMGFAX
020200990929     C*
020300990929     C* Carico DATI TABELLA CODICI BOLLA
020400990929     C                   Z-ADD     0             §F                3 0
020500990929     C                   MOVEL     '3A'          COD
020600990929     C                   MOVEL     *BLANK        KEY
020700990929     C     KTAB1         CHAIN     TABEL                              21
020800990929     C     *IN21         DOWEQ     '0'
020900990929     C     §F            ANDLE     200
021000990929     C     TBLFLG        IFEQ      *BLANK
021100990929     C                   ADD       1             §F
021200990929     C                   MOVEL     TBLUNI        DS3A
021300990929     C                   MOVEL     §3ATB1        T3A(§F)
021400990929     C                   MOVEL     TBLKEY        C3A(§F)
021500990929     C                   END
021600990929     C     KTAB1         READE     TABEL                                  21
021700990929     C                   END
021800990929     C*
021900900514     C                   ENDSR
022000900514     C*---------------------------------------------------------------*
022100950112     C*   Stampa lista giacenze in attesa di disposiz.                *
022200900514     C*---------------------------------------------------------------*
022300950112     C     LISTA1        BEGSR
022400900515     C*
022500950116     C* Carico DATI TABELLA PARTICOLARITA'
022600950116     C                   Z-ADD     0             §F                3 0
022700950116     C                   MOVEL     '7Q'          COD
022800950116     C                   MOVEL     *BLANK        KEY
022900950116     C     KTAB1         CHAIN     TABEL                              21
023000950116     C     *IN21         DOWEQ     '0'
023100950116     C     §F            ANDLE     100
023200950116     C     TBLFLG        IFEQ      *BLANK
023300950116     C                   ADD       1             §F
023400950116     C                   MOVEL     TBLUNI        D7Q(§F)
023500950116     C                   MOVEL     TBLKEY        C7Q(§F)
023600950116     C                   END
023700950116     C     KTAB1         READE     TABEL                                  21
023800950116     C                   END
023900950116     C*
024000950112     C                   SETON                                        40
024100950112     C                   MOVE      WHHDAT        G02DAT
024200911220     C                   MOVEL     *BLANK        G02ERR
024300950112     C                   CALL      'XSRDA8'
024400950112     C                   PARM                    WLBDA8
024500911220     C                   Z-ADD     G02TGI        §GGD
024600940330     C                   Z-ADD     G02INV        DATAA
024700911220     C*
024800050223     C                   READ      tigcp21L                               21
024900950112     C     *IN21         DOWEQ     '0'
025000020820     c     gcpfgc        lookup    lin                                    22
025100020820     c  n22              goto      altro
025200050414     C     GCPFAS        IFlt      30
025300950112     C     GCPFRG        ANDEQ     0
025400950112     C     GCPATB        ANDEQ     ' '
025500961112     C*  Controllo se devo gestire l'estero
025600971113     C                   MOVEL     *BLANKS       ORGFL1
025700971113     C     GCPFGC        CHAIN     AZORG01L                           19
025800020717     C                   clear                   OG143
025900020717     C  N19              MOVEL     ORGDE3        OG143
026000020717      * sostituito con il campo network il vecchio flag estero
026100961112     C     D40ITA        IFEQ      'I'
026200020717     C**** ORGFL1        ANDNE     'E'
026300020717     C     §OgNTW        andne     'FED'
026400020717     C     §OgNTW        andne     'EEX'
026500020717     C     §OgNTW        andne     'EUP'
026600020717      *
026700961112     C     D40ITA        OREQ      'E'
026800020717     C**** ORGFL1        ANDEQ     'E'
026900020717     C     §OgNTW        andeq     'FED'
027000020717      *
027100020717     C     D40ITA        OREQ      'E'
027200020717     C     §OgNTW        andeq     'EEX'
027300020717      *
027400020717     C     D40ITA        OREQ      'E'
027500020911     C     §OgNTW        andeq     'EUP'
027600950112     C                   EXSR      STPL01
027700950112     C                   END
027800961112     C                   END
027900020820     c     altro         tag
028000050223     C                   READ      tigcp21L                               21
028100950112     C                   END
028200900515     C*
028300950112     C                   ENDSR
028400950112     C*---------------------------------------------------------------*
028500950112     C*   Stampa lista giacenze in ritardo esec.dispos.               *
028600950112     C*---------------------------------------------------------------*
028700950112     C     LISTA2        BEGSR
028800950112     C*
028900950112     C                   SETON                                        41
029000950112     C                   MOVE      WHHDAT        G02DAT
029100950112     C                   MOVEL     *BLANK        G02ERR
029200950112     C                   CALL      'XSRDA8'
029300950112     C                   PARM                    WLBDA8
029400950112     C                   Z-ADD     G02TGI        WGGDAY            5 0
029500950112     C                   Z-ADD     G02INV        AMGUDT            8 0
029600950112     C*
029700020820     c                   do        30            i                 3 0
029800020820     c                   if        lin(i) = 0
029900020820     c                   leave
030000020820     c                   end
030100050223     c     lin(i)        setll     tigcp24l
030200020820     c                   do        *hival
030300050223     C     lin(i)        READe     tigcp24L                               21
030400020820     C   21              leave
030500050414     C                   IF        (gcpfas=30 or gcpfas=31 or gcpfas=36) and
030600050414     C                             gcpfrg = 0
030700961112     C*  Controllo se devo gestire l'estero
030800971113     C                   MOVEL     *BLANKS       ORGFL1
030900971113     C     GCPFGC        CHAIN     AZORG01L                           19
031000020717     C                   clear                   OG143
031100020717     C  N19              MOVEL     ORGDE3        OG143
031200961112     C     D40ITA        IFEQ      'I'
031300050414     C**** GCPFAS        ANDGT     30
031400020717     C**** ORGFL1        ANDNE     'E'
031500020717     C     §OgNTW        andne     'FED'
031600020717     C     §OgNTW        andne     'EEX'
031700020717     C     §OgNTW        andne     'EUP'
031800020717      *
031900961112     C     D40ITA        OREQ      'E'
032000050414     C***  GCPFAS        ANDGT     30
032100020717     C***  ORGFL1        ANDEQ     'E'
032200020717     C     §OgNTW        andeq     'FED'
032300020717      *
032400020717     C     D40ITA        OREQ      'E'
032500050414     C***  GCPFAS        ANDGT     30
032600020717     C     §OgNTW        andeq     'EEX'
032700020717      *
032800020717     C     D40ITA        OREQ      'E'
032900050414     C***  GCPFAS        ANDGT     30
033000020717     C     §OgNTW        andeq     'EUP'
033100950112     C                   EXSR      STPL02
033200950112     C                   END
033300961112     C                   END
033400020820     C***
033500950112     C                   END
033600020820     C                   END
033700950112     C*
033800950112     C                   ENDSR
033900950112     C*---------------------------------------------------------------*
034000950112     C*   Stampa lista giacenze aperte in ritardo                     *
034100950112     C*---------------------------------------------------------------*
034200950112     C     LISTA4        BEGSR
034300950112     C*
034400950112     C                   SETON                                        43
034500950112     C* Carico DATI TABELLA PARTICOLARITA'
034600950112     C                   Z-ADD     0             §F                3 0
034700950112     C                   MOVEL     '7Q'          COD
034800950112     C                   MOVEL     *BLANK        KEY
034900950112     C     KTAB1         CHAIN     TABEL                              21
035000950112     C     *IN21         DOWEQ     '0'
035100950112     C     §F            ANDLE     100
035200950112     C     TBLFLG        IFEQ      *BLANK
035300950112     C                   ADD       1             §F
035400950112     C                   MOVEL     TBLUNI        D7Q(§F)
035500950112     C                   MOVEL     TBLKEY        C7Q(§F)
035600950112     C                   END
035700950112     C     KTAB1         READE     TABEL                                  21
035800950112     C                   END
035900950112     C*  GIRO DATE LIMITI
036000950112     C                   Z-ADD     D40DD4        G02INV
036100950112     C                   MOVEL     '3'           G02ERR
036200950112     C                   CALL      'XSRDA8'
036300950112     C                   PARM                    WLBDA8
036400950112     C                   Z-ADD     G02DAT        DRD4              8 0
036500950112     C                   Z-ADD     D40DA4        G02INV
036600950112     C                   MOVEL     '3'           G02ERR
036700950112     C                   CALL      'XSRDA8'
036800950112     C                   PARM                    WLBDA8
036900950112     C                   Z-ADD     G02DAT        DRA4              8 0
037000950112     C*
037100020820     c                   do        30            i
037200020820     c                   if        lin(i) = 0
037300020820     c                   leave
037400020820     c                   end
037500050223     c     lin(i)        setll     tigcp24l
037600020820     c                   do        *hival
037700050223     C     lin(i)        READe     tigcp24L                               21
037800020820     C   21              leave
037900950112     C     GCPDCG        IFEQ      0
038000950112     C     GCPFRG        ANDEQ     0
038100961112     C*  Controllo se devo gestire l'estero
038200971113     C                   MOVEL     *BLANKS       ORGFL1
038300971113     C     GCPFGC        CHAIN     AZORG01L                           19
038400020717     C                   clear                   OG143
038500020717     C  N19              MOVEL     ORGDE3        OG143
038600971113     C     D40ITA        IFEQ      'I'
038700020717     C**** ORGFL1        ANDNE     'E'
038800020717     C     §OgNTW        andne     'FED'
038900020717     C     §OgNTW        andne     'EEX'
039000020717     C     §OgNTW        andne     'EUP'
039100020717      *
039200020717     C     D40ITA        OREQ      'E'
039300020717     C**** ORGFL1        ANDEQ     'E'
039400020717     C     §OgNTW        andeq     'FED'
039500020717      *
039600020717     C     D40ITA        OREQ      'E'
039700020717     C     §OgNTW        andeq     'EEX'
039800020717      *
039900020717     C     D40ITA        OREQ      'E'
040000020717     C     §OgNTW        oreq      'EUP'
040100950112     C                   EXSR      STPL04
040200950112     C                   END
040300961112     C                   END
040400020820     C*
040500950112     C                   END
040600020820     C                   END
040700950112     C*
040800950112     C                   ENDSR
040900950112     C*---------------------------------------------------------------*
041000950112     C*   Giacenze in attesa di disposizioni                          *
041100950112     C*---------------------------------------------------------------*
041200950112     C     STPL01        BEGSR
041300950112     C*
041400950112     C                   Z-ADD     0             DATDUR
041500050223     C     GCPriap       IFEQ      *blank
041600950112     C                   MOVEL     GCPAGC        DATAGC            8 0
041700950112     C                   MOVE      GCPMGC        DATAGC
041800950112     C                   Z-ADD     DATAGC        G02INV
041900950112     C                   ELSE
042000950112     C                   MOVEL     GCPDUR        G02INV
042100950116     C                   MOVEL     '3'           G02ERR
042200950116     C                   CALL      'XSRDA8'
042300950116     C                   PARM                    WLBDA8
042400950116     C                   Z-ADD     G02DAT        DATDUR            8 0
042500950112     C                   END
042600950116     C*
042700950116     C** AGGANCIO FILE STORICO
042800950116     C                   Z-ADD     0             AR9CAS
042900950116     C                   Z-ADD     0             AR6IFT
043000950116     C                   MOVEL     *BLANKS       AR9VCA
043100991119     C                   MOVEL     *BLANKS       AR6DIV
043200051115     C     KGAP          CHAIN     FIAR901L                           22
043300950116     C     KGAP          CHAIN     FNARB01L                           22
043400990929     C* Verifico se esiste la spedizione è in porto assegnato
043500990929     C* per reperire eventuali dati di tassazione
043600990929     C     *IN22         IFEQ      '0'
043700990929     C                   Z-ADD     1             Y
043800990929     C     ARBCBO        LOOKUP    C3A(Y)                                 22
043900990929     C     T3A(Y)        IFEQ      'A'
044000990929     C     *IN22         ANDEQ     '1'
044100990929     C                   MOVEL     '1'           KTRC
044200990929     C     KAR6          CHAIN     FIAR601L                           22
044300990929     C                   END
044400990929     C                   END
044500950116     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
044600950116     C     ARBGGA        IFNE      *BLANKS
044700950116     C                   Z-ADD     1             Y
044800950406     C     ARBGGA        LOOKUP    C7Q(Y)                                 25
044900950406     C     *IN25         IFEQ      '1'
045000950116     C                   MOVEL     D7Q(Y)        DS7Q
045100950116     C     §7Q3EV        IFNE      *BLANKS
045200950116     C     §7Q3MF        ANDEQ     'A'
045300950116     C                   MOVEL     '2A'          COD
045400950116     C                   MOVEL     *BLANK        KEY
045500950116     C                   MOVEL     §7Q3EV        KEY
045600950116     C     KTAB          CHAIN     TABEL                              23
045700950116     C     *IN23         IFEQ      '0'
045800950116     C                   MOVEL     TBLUNI        DS2A
045900950116     C     §2AGLA        IFGT      0
046000950116     C                   CLEAR                   TRUL52
046100950116     C                   Z-ADD     §2AGLA        D52GIO
046200950116     C                   Z-ADD     G02INV        D52DIN
046300950116     C                   CALL      'TRUL52R'
046400950116     C                   PARM                    TRUL52
046500950116     C     D52ERR        IFEQ      *BLANKS
046600950116     C                   Z-ADD     D52DFI        G02INV            8 0
046700950116     C                   END
046800950116     C                   END
046900950116     C                   END
047000950116     C                   END
047100950116     C*
047200950116     C                   END
047300950116     C                   END
047400950116     C*
047500950116     C                   MOVEL     '3'           G02ERR
047600950116     C                   CALL      'XSRDA8'
047700950116     C                   PARM                    WLBDA8
047800950116     C                   Z-ADD     G02TGI        §GGA
047900950116     C                   Z-ADD     G02INV        DATADA
048000911220     C*
048100900515     C     §GGD          SUB       §GGA          §GGDI
048200950112     C     §GGDI         IFGT      D40GG1
048300950112     C                   MOVE      AR9CAS        COM3              3 0
048400950112     C     COM3          COMP      000                                    06
048500991119     C                   MOVE      AR6IFT        COM3              3 0
048600991119     C     COM3          COMP      000                                    07
048700900515     C                   EXSR      DECODI
048800950112     C                   EXSR      MOTAPE
048900900515     C                   EXSR      RIBDAT
049000900515     C                   EXSR      STAMP
049100900515     C                   END
049200950112     C*
049300950112     C                   ENDSR
049400950112     C*---------------------------------------------------------------*
049500950112     C*   Giacenze in ritardo di esecuzione                           *
049600950112     C*---------------------------------------------------------------*
049700950112     C     STPL02        BEGSR
049800970617     C* -------- ESTERO
049900970617     C* SE RICHIESTO GIAC RITARDO ESECUZ X ESTERO RAGIONO CON
050000970617     C*    DATA FAX E UDATE
050100970617     C     D40ITA        IFEQ      'E'
050200970617      * imposto data: DAL
050300970617     C                   EXSR      RICFAX
050400970617     C     AMGFAX        IFGT      0
050500970617     C                   MOVE      AMGFAX        G02INV
050600970617     C                   ELSE
050700970617     C     GCPDED        IFGT      GCPDLD
050800970617     C                   MOVE      GCPDED        G02INV
050900970617     C                   ELSE
051000970617     C     GCPDLD        IFGT      GCPDDM
051100970617     C                   MOVE      GCPDLD        G02INV
051200970617     C                   ELSE
051300970617     C                   MOVE      GCPDDM        G02INV
051400970617     C                   END
051500970617     C                   END
051600970617     C                   END
051700970617     C                   MOVEL     '3'           G02ERR
051800970617     C                   CALL      'XSRDA8'
051900970617     C                   PARM                    WLBDA8
052000970617     C                   Z-ADD     G02TGI        §GGA
052100970617     C                   Z-ADD     G02INV        DATADA
052200970617     C*
052300970617      * imposto data: AL = udate
052400970617     C                   Z-ADD     WGGDAY        §GGD
052500970617     C                   Z-ADD     AMGUDT        DATAA
052600970617      *
052700970617     C                   ELSE
052800970617     C* -------- ITALIA
052900950112     C     GCPDLD        IFEQ      0
053000950112     C                   MOVE      GCPDDM        G02INV
053100950112     C     GCPDED        IFGT      0
053200950112     C                   MOVE      GCPDED        G02INV
053300950112     C                   END
053400950112     C                   ELSE
053500950112     C     GCPDED        IFLT      GCPDLD
053600950112     C                   MOVE      GCPDLD        G02INV
053700950112     C                   ELSE
053800950112     C                   MOVE      GCPDED        G02INV
053900950112     C                   END
054000950112     C                   END
054100950112     C*
054200900517     C                   MOVEL     '3'           G02ERR
054300950112     C                   CALL      'XSRDA8'
054400950112     C                   PARM                    WLBDA8
054500900515     C                   Z-ADD     G02TGI        §GGA
054600940330     C                   Z-ADD     G02INV        DATADA
054700900515     C*
054800050223     C     GCPdeda       IFEQ      0
054900950112     C                   Z-ADD     WGGDAY        §GGD
055000950112     C                   Z-ADD     AMGUDT        DATAA
055100950112     C                   ELSE
055200050223     C                   Z-ADD     GCPDeda       G02INV
055300950112     C                   MOVEL     '3'           G02ERR
055400950112     C                   CALL      'XSRDA8'
055500950112     C                   PARM                    WLBDA8
055600950112     C                   Z-ADD     G02TGI        §GGD
055700950112     C                   Z-ADD     G02INV        DATAA
055800950112     C                   END
055900970617     C                   END
056000970617      * -------- Italia + Estero
056100970617     C* CALCOLO LA DIFFERENZA FRA LE DUE DATE
056200900515     C     §GGD          SUB       §GGA          §GGDI
056300950112     C     §GGDI         IFGT      D40GG2
056400940330     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
056500940330     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
056600940330     C                   EXSR      FEST
056700940330     C                   SUB       GGF           §GGDI
056800950112     C     §GGDI         IFGT      D40GG2
056900900515     C** AGGANCIO FILE STORICO
057000950112     C                   Z-ADD     0             DATDUR
057100950112     C                   Z-ADD     0             AR9CAS
057200950112     C                   Z-ADD     0             AR6IFT
057300950112     C                   MOVEL     *BLANKS       AR9VCA
057400991119     C                   MOVEL     *BLANKS       AR6DIV
057500051115     C     KGAP          CHAIN     FIAR901L                           22
057600950112     C     KGAP          CHAIN     FNARB01L                           22
057700990929     C* Verifico se esiste la spedizione è in porto assegnato
057800990929     C* per reperire eventuali dati di tassazione
057900990929     C     *IN22         IFEQ      '0'
058000990929     C                   Z-ADD     1             Y
058100990929     C     ARBCBO        LOOKUP    C3A(Y)                                 22
058200990929     C     T3A(Y)        IFEQ      'A'
058300990929     C     *IN22         ANDEQ     '1'
058400990929     C                   MOVEL     '1'           KTRC
058500990929     C     KAR6          CHAIN     FIAR601L                           22
058600990929     C                   END
058700990929     C                   END
058800950112     C                   MOVE      AR9CAS        COM3              3 0
058900950112     C     COM3          COMP      000                                    06
059000991119     C                   MOVE      AR6IFT        COM3              3 0
059100991119     C     COM3          COMP      000                                    07
059200950112     C                   EXSR      DECODI
059300950112     C                   EXSR      RICNOT
059400970113     C                   EXSR      RIBDAT
059500900515     C                   EXSR      STAMP1
059600900515     C                   END
059700900515     C                   END
059800950112     C*
059900950112     C                   ENDSR
060000950112     C*---------------------------------------------------------------*
060100950112     C*   Giacenze in ritardo di apertura giacenza                    *
060200950112     C*---------------------------------------------------------------*
060300950112     C     STPL04        BEGSR
060400950112     C*
060500950112     C                   MOVEL     GCPAGC        DATAGC
060600900515     C                   MOVE      GCPMGC        DATAGC
060700950307     C                   Z-ADD     0             DATDUR
060800050223     C     gcpriap       IFNE      *blank
060900950307     C                   Z-ADD     GCPDUR        DATAGC
061000950307     C                   Z-ADD     GCPDUR        G02INV
061100950307     C                   MOVEL     '3'           G02ERR
061200950307     C                   CALL      'XSRDA8'
061300950307     C                   PARM                    WLBDA8
061400950307     C                   Z-ADD     G02DAT        DATDUR
061500950307     C                   END
061600950112     C*
061700950112     C     DATAGC        IFGE      D40DD4
061800950112     C     DATAGC        ANDLE     D40DA4
061900950112     C                   Z-ADD     DATAGC        G02INV
062000950112     C                   MOVEL     '3'           G02ERR
062100950112     C                   CALL      'XSRDA8'
062200950112     C                   PARM                    WLBDA8
062300950112     C                   Z-ADD     G02TGI        §GGD
062400950112     C                   Z-ADD     G02INV        DATAA
062500950307     C*
062600950307     C                   Z-ADD     GCPDXD        G02INV
062700950307     C     ARBFFD        IFEQ      'S'
062800051107     C     ARBFFD        oreq      'M'
062900950307     C                   MOVEL     *BLANKS       KEY
063000950307     C                   MOVEL     'F  '         KEY
063100950307     C                   ELSE
063200950307     C** CONFRONTO DATA APERTURA CON DATA COMUNICAZIONE TENENDO CONTO
063300950307     C** DEGLI EVENTUALI GIORNI LASCIATO AVVISO
063400990803     C                   MOVE      DATAGC        DATAG1            8 0
063500050223     C                   Z-ADD     GCPAASa       KAAS
063600950307     C                   Z-ADD     GCPDXD        KDEV
063700050223     C                   Z-ADD     GCPLNPa       KLNP
063800050223     C                   Z-ADD     GCPNRSa       KNRS
063900050223     C                   Z-ADD     GCPNSPa       KNSP
064000990803     C     KEVB          SETLL     FNEVB01L                           25
064100950406     C     *IN25         DOWEQ     *OFF
064200990803     C     KEVBS         READE     FNEVB000                               25
064300950307     C* SOMMO TUTTI I GIORNI L.AVV FINO ALL'APERT.GIAC.
064400950609     C  N25EVBDEV        IFLT      DATAG1
064500950307     C                   MOVEL     *BLANKS       KEY
064600950307     C                   MOVEL     EVBCEV        KEY
064700950307     C                   END
064800950307     C                   ENDDO
064900950307     C                   ENDIF
065000950307     C*
065100950307     C                   MOVEL     '2A'          COD
065200950307     C     KTAB          CHAIN     TABEL                              39
065300950307     C  N39              MOVEL     TBLUNI        DS2A
065400950307     C  N39§2AGLA        IFGT      0
065500950307     C                   CLEAR                   TRUL52
065600950307     C                   Z-ADD     §2AGLA        D52GIO
065700950307     C                   Z-ADD     G02INV        D52DIN
065800950307     C                   CALL      'TRUL52R'
065900950307     C                   PARM                    TRUL52
066000950307     C     D52ERR        IFEQ      *BLANKS
066100950307     C                   Z-ADD     D52DFI        G02INV            8 0
066200950307     C                   END
066300950307     C                   ENDIF
066400950307     C*
066500950112     C                   MOVEL     '3'           G02ERR
066600950112     C                   CALL      'XSRDA8'
066700950112     C                   PARM                    WLBDA8
066800950112     C                   Z-ADD     G02TGI        §GGA
066900950112     C                   Z-ADD     G02INV        DATADA
067000950307     C*
067100950112     C** AGGANCIO FILE STORICO
067200950112     C                   Z-ADD     0             AR6IFT
067300950112     C                   Z-ADD     0             AR9CAS
067400950112     C                   MOVEL     *BLANKS       AR9VCA
067500991119     C                   MOVEL     *BLANKS       AR6DIV
067600051115     C     KGAP          CHAIN     FIAR901L                           22
067700950112     C                   MOVE      AR9CAS        COM3              3 0
067800950112     C     COM3          COMP      000                                    06
067900950112     C     KGAP          CHAIN     FNARB01L                           22
068000990929     C* Verifico se esiste la spedizione è in porto assegnato
068100990929     C* per reperire eventuali dati di tassazione
068200990929     C     *IN22         IFEQ      '0'
068300990929     C                   Z-ADD     1             Y
068400990929     C     ARBCBO        LOOKUP    C3A(Y)                                 22
068500990929     C     T3A(Y)        IFEQ      'A'
068600990929     C     *IN22         ANDEQ     '1'
068700990929     C                   MOVEL     '1'           KTRC
068800990929     C     KAR6          CHAIN     FIAR601L                           22
068900990929     C                   END
069000990929     C                   END
069100991119     C                   MOVE      AR6IFT        COM3              3 0
069200991119     C     COM3          COMP      000                                    07
069300950112     C*
069400950112     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
069500950112     C     ARBGGA        IFNE      *BLANKS
069600950112     C                   Z-ADD     1             Y                 3 0
069700950406     C     ARBGGA        LOOKUP    C7Q(Y)                                 25
069800950406     C     *IN25         IFEQ      '1'
069900950112     C                   MOVEL     D7Q(Y)        DS7Q
070000950112     C     §7Q2EV        IFNE      *BLANKS
070100950112     C     §7Q2MF        ANDEQ     'A'
070200950112     C                   MOVEL     '2A'          COD
070300950112     C                   MOVEL     *BLANK        KEY
070400950112     C                   MOVEL     §7Q3EV        KEY
070500950112     C     KTAB          CHAIN     TABEL                              23
070600950112     C     *IN23         IFEQ      '0'
070700950112     C                   MOVEL     TBLUNI        DS2A
070800950112     C                   ADD       §2AGLA        §GGA
070900950112     C                   END
071000950112     C                   END
071100950112     C                   END
071200950112     C                   END
071300900515     C*
071400950112     C     §GGD          SUB       §GGA          §GGDI
071500950112     C     §GGDI         IFGT      D40GG4
071600940330     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
071700940330     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
071800940330     C                   EXSR      FEST
071900940330     C                   SUB       GGF           §GGDI
072000950112     C     §GGDI         IFGT      D40GG4
072100900515     C                   EXSR      DECODI
072200950112     C                   EXSR      MOTAPE
072300900515     C                   EXSR      RIBDAT
072400900515     C                   EXSR      STAMP1
072500900515     C                   END
072600900515     C                   END
072700950112     C*
072800950112     C                   END
072900950112     C**
073000950112     C                   ENDSR
073100900514     C*---------------------------------------------------------------*
073200900514     C*   DECODIFICA MITTENTE
073300900514     C*---------------------------------------------------------------*
073400900514     C     DECODI        BEGSR
073500980205     C                   MOVEL     ARBRSM        RAGMIT           25
073600980205     C                   MOVEL     ARBRSD        RAGDES           25
073700920427     C                   MOVEL     ARBRSD        STPRSD           15
073800950112     C                   MOVEL     ARBRMN        RIFNUM           15 0
073900980205     C                   MOVEL     ARBCAD        CAPDES            9
074000900514     C                   ENDSR
074100900515     C*---------------------------------------------------------------*
074200900515     C*   RIBALTAMENTO DATE
074300900515     C*---------------------------------------------------------------*
074400900515     C     RIBDAT        BEGSR
074500900515     C* DATA GIACENZA
074600900515     C                   MOVEL     GCPAGC        DATAGC
074700900515     C                   MOVE      GCPMGC        DATAGC
074800950112     C                   Z-ADD     DATAGC        G02INV
074900950112     C                   MOVEL     '3'           G02ERR
075000950112     C                   CALL      'XSRDA8'
075100950112     C                   PARM                    WLBDA8
075200950112     C                   Z-ADD     G02DAT        DATAGC
075300900515     C* DATA SPEDIZIONE
075400970617     C     D40ITA        IFEQ      'I'
075500950112     C                   MOVEL     ARBAAS        DATSPE            8 0
075600920113     C                   MOVE      ARBMGS        DATSPE
075700970617     C                   ELSE
075800970617     C* DATA MANIFEST X ESTERO
075900970617     C                   Z-ADD     ARBDT3        DATSPE
076000970617     C                   END
076100970617      *
076200950112     C                   Z-ADD     DATSPE        G02INV
076300950112     C                   MOVEL     '3'           G02ERR
076400950112     C                   CALL      'XSRDA8'
076500950112     C                   PARM                    WLBDA8
076600950112     C                   Z-ADD     G02DAT        DATSPE
076700900516     C* DATA COMUNICAZIONE INTERNA
076800950112     C                   Z-ADD     GCPDXD        G02INV
076900950112     C                   MOVEL     '3'           G02ERR
077000950112     C                   CALL      'XSRDA8'
077100950112     C                   PARM                    WLBDA8
077200950112     C                   Z-ADD     G02DAT        DATDXD            8 0
077300900516     C* DATA DISPOSIZIONI
077400950112     C                   Z-ADD     GCPDDM        G02INV
077500950112     C                   MOVEL     '3'           G02ERR
077600950112     C                   CALL      'XSRDA8'
077700950112     C                   PARM                    WLBDA8
077800950112     C                   Z-ADD     G02DAT        DATDDM            8 0
077900950112     C* DATA esecuz. DISPOSIZIONI
078000950112     C                   Z-ADD     GCPDED        G02INV
078100950112     C                   MOVEL     '3'           G02ERR
078200950112     C                   CALL      'XSRDA8'
078300950112     C                   PARM                    WLBDA8
078400950112     C                   Z-ADD     G02DAT        DATDED            8 0
078500950112     C*
078600961219     C* DATA FAX PER ESTERO
078700970113     C                   CLEAR                   DATFAX
078800961219     C     AMGFAX        IFNE      *ZEROS
078900961219     C                   Z-ADD     AMGFAX        G02INV
079000961219     C                   MOVEL     '3'           G02ERR
079100961219     C                   CALL      'XSRDA8'
079200961219     C                   PARM                    WLBDA8
079300961219     C                   Z-ADD     G02DAT        DATFAX            8 0
079400961219     C                   END
079500900515     C                   ENDSR
079600961219     C*---------------------------------------------------------------*
079700961219     C*   RICERCA DATA INVIO FAX
079800961219     C*---------------------------------------------------------------*
079900961219     C     RICFAX        BEGSR
080000961219      *
080100961219     C                   CLEAR                   AMGFAX
080200961219     C                   Z-ADD     35            §FAS
080300050223     C     KNOT          SETLL     tignp01L
080400970108     C     *IN50         DOUEQ     *ON
080500050223     C     KNOT          READE     tignp01L                               50
080600050223     C  N50              MOVEL     GNPDif        AMGFAX            8 0
080700970108     C                   ENDDO
080800961219     C                   ENDSR
080900911209     C*---------------------------------------------------------------*
081000911209     C*   RICERCA NOTE
081100911209     C*---------------------------------------------------------------*
081200911209     C     RICNOT        BEGSR
081300950112     c*
081400961227     C     D40ITA        IFNE      'E'
081500950112     C                   Z-ADD     20            §FAS
081600050223     C     KNOT          CHAIN     tignp01L                           50
081700911209     C  N50              MOVEL     GNPDMC        DS1              40
081800920427     C   50              MOVEL     *BLANKS       DS1
081900920427     C     DS1           IFEQ      *BLANKS
082000920427     C                   MOVEL     '2D'          COD
082100920427     C                   MOVEL     *BLANKS       KEY
082200950112     C                   MOVEL     GCPDIS        KEY
082300920427     C     KTAB          CHAIN     TABEL                              66
082400920427     C                   MOVEL     *BLANKS       DESCR
082500920427     C  N66              MOVEL     TBLUNI        DESCR            20
082600920427     C  N66              MOVEL     DESCR         DS1
082700920427     C                   END
082800961227     C                   ELSE
082900961227     C                   CLEAR                   DS1
083000961227     C     GCPDIS        IFEQ      'C'
083100961227     C                   MOVEL     COST1         DS1
083200961227     C                   ELSE
083300970113     C     GCPDIS        IFEQ      'R'
083400961227     C                   MOVEL     COST2         DS1
083500970113     C                   ENDIF
083600961227     C                   ENDIF
083700961227     C                   ENDIF
083800911209     C                   ENDSR
083900900515     C*---------------------------------------------------------------*
084000930622     C*   STAMPA LISTA 1                                              *
084100900515     C*---------------------------------------------------------------*
084200900515     C     STAMP         BEGSR
084300950407     C*
084400950407     C* PONGO IN UN CAMPO DI COMODO LA LINEA DI PARTENZA
084500050223     C                   Z-ADD     GCPLNPa       WLNP
084600950112     C*
084700950406     C* CERCO LA BOLLA COME FIGLIA FRA I LEGAMI, PERCHE' SE LA TROVO
084800950406     C* STAMPO I RIFERIMENTI DELLA BOLLA ORIGINALE
084900950407     C                   SETOFF                                       44
085000050223     C     gcplnp        ifne      GCPLNPa
085100950407     C*
085200950407     C* LA DATA DI PARTENZA DEL RELEASE E' SOLO SUL 1° LIVELLO
085300950407     C                   SETON                                        44
085400950407     C*
085500950407     C* METTO LA "LNP ORIGINALE" NEL CAMPO DI COMODO WLNP PERCHE' SE
085600950407     C* DIVERSA DA "LNP GIACENZA" ---> SALTO PAGINA
085700050223     C                   Z-ADD     gcplnp        WLNP
085800950407     C*
085900950407     C                   ENDIF
086000950406     C*
086100950407     C* SE CAMBIA LA LINEA DI PARTENZA ---> SALTO PAGINA
086200950407     C     WLNP          IFNE      SAVLNP
086300930622     C* DECODIFICO LINEA DI PARTENZA DELLA SPEDIZIONE
086400950407     C     WLNP          CHAIN     AZORG01L                           19
086500930622     C  N19              MOVEL     ORGDES        FILIA            20
086600930622     C   19              MOVEL     *ALL'?'       FILIA
086700920107     C                   EXCEPT    TESTA
086800950407     C                   Z-ADD     WLNP          SAVLNP
086900920107     C                   END
087000950407     C*
087100950112     C   OF              EXCEPT    TESTA
087200900515     C                   EXCEPT    DET
087300950112     C*
087400900515     C                   ENDSR
087500900515     C*---------------------------------------------------------------*
087600900515     C*   STAMPA LISTA 2 3 4
087700900515     C*---------------------------------------------------------------*
087800900515     C     STAMP1        BEGSR
087900980520      *
088000980520      *  41ON = GIACENZE IN RITARDO DI ESECUZIONE DISPOSIZIONI
088100980520      *
088200900515     C     GCPCCR        IFNE      SAVCCR
088300980520      *
088400980520      * Se *in41 = ON ed estero determino se sono SDI, se positivo *in29 = *ON
088500950112     C                   EXSR      RICDES
088600980520      *
088700961219     C     D40ITA        IFEQ      'E'
088800961219     C     *IN41         ANDEQ     *ON
088900961219     C                   EXCEPT    TESTAI
089000961219     C                   ELSE
089100961219     C                   EXCEPT    TESTA
089200961219     C                   END
089300980520      *
089400900515     C                   Z-ADD     GCPCCR        SAVCCR            3 0
089500900515     C                   END
089600980520      *
089700961219     C     D40ITA        IFEQ      'E'
089800961219     C     *IN41         ANDEQ     *ON
089900980520      * Aggancio FNAR4 per AWB
090000980520     C                   MOVEL     *BLANKS       AWB              10
090100980520     C                   MOVEL     'E'           KTRC
090200060213     C     KAR4          CHAIN     FiAR4000                           22
090300980520     C  N22              MOVEL     AR4NOT        AWB
090400980520      *
090500961219     C   OF              EXCEPT    TESTAI
090600961219     C                   EXCEPT    DETI
090700961219     C                   ELSE
090800961219     C   OF              EXCEPT    TESTA
090900961219     C                   EXCEPT    DET
091000961219     C                   END
091100950112     C*
091200900515     C                   ENDSR
091300900515     C*---------------------------------------------------------------*
091400900515     C*   RICERCA DESCRIZIONI
091500900515     C*---------------------------------------------------------------*
091600900515     C     RICDES        BEGSR
091700901008     C                   SETOFF                                       24
091800980520      * LUOGO DEPOSITO MERCE
091900980520      *
092000900515     C     GCPCCR        CHAIN     AZORG01L                           23
092100980520      * Se estero determino se sono SDI
092200980520     C                   SETOFF                                       29
092300980520     C     D40ITA        IFEQ      'E'
092400980520     C     *IN41         ANDEQ     *ON
092500980520     C     ORGDIT        ANDEQ     'SDI'
092600980520     C                   SETON                                        29
092700980520     C                   ENDIF
092800980520      *
092900900515     C                   MOVEL     *BLANKS       DCCR             20
093000900515     C  N23ORGFVA        COMP      '*'                                    23
093100900515     C   23              MOVEL     *ALL'?'       DCCR
093200900515     C  N23              MOVEL     ORGDES        DCCR
093300900515     C* SE NON LO TROVO NELL'ORGANIGR. LO CERCO IN TAB. CORRISP.
093400900515     C     *IN23         IFEQ      '1'
093500900516     C                   MOVEL     '2B'          COD
093600900516     C                   MOVEL     *BLANK        KEY
093700900516     C                   MOVEL     GCPCCR        KEY
093800900515     C     KTAB          CHAIN     TABEL                              23
093900900515     C   23              MOVEL     *ALL'?'       DCCR
094000900515     C  N23              MOVEL     TBLUNI        DCCR
094100900515     C                   END
094200950112     C*
094300950112     C                   ENDSR
094400950112     C*--------------------------------------------------***
094500950112     C* Ricerco codice motivo apertura  giacenza
094600950112     C*--------------------------------------------------***
094700950112     C     MOTAPE        BEGSR
094800950112     C*
094900950112     C                   CLEAR                   DS2A
095000950112     C                   SETOFF                                       24
095100900516     C* CODICE MOTIVO GIACENZA
095200900516     C                   MOVEL     '2A'          COD
095300900516     C                   MOVEL     *BLANK        KEY
095400900516     C                   MOVEL     GCPCMC        KEY
095500900516     C     KTAB          CHAIN     TABEL                              23
095600950112     C     *IN23         IFEQ      '0'
095700950112     C     GCPCMC        ANDNE     'GEN'
095800950112     C                   MOVEL     TBLUNI        DS2A
095900950112     C                   ELSE
096000950112     C                   SETON                                        24
096100950112     C                   Z-ADD     10            §FAS
096200050223     C     KNOT          CHAIN     tignp01L                           50
096300950112     C  N50              MOVEL     GNPDMC        DMC              50
096400950112     C   50              MOVEL     *BLANKS       DMC
096500950112     C                   END
096600950112     C*
096700900515     C                   ENDSR
096800900515     C***------------------------------------------------***
096900940330     C* CALCOLO GIORNI FESTIVI INTERCORRENTE TRA LE DUE DATE IN ESAME
097000940330     C*-----------
097100940330     C     FEST          BEGSR
097200990906     C                   CALL      'XGGFEST'
097300950112     C                   PARM                    WDAT8
097400940330     C                   ENDSR
097500940330     C***------------------------------------------------***
097600950111     OPRTF198   E            TESTA            01
097700950111     O               40      ST(1)              132
097800950111     O               40      D40GG1        Z    132
097900950111     O               41      ST(2)              132
098000950111     O               41      D40GG2        Z    131
098100950111     O               43      ST(3)              128
098200950112     O               43      D40GG4        Z    108
098300950111     O               43      DRD4               124 '  /  /    '
098400950111     O               43      DRA4               138 '  /  /    '
098500950111     O                       RSUT                20
098600950111     O                                           21 '/'
098700950111     O                       SIMFEL              24
098800961001     O                                          176 'FNLG43R'
098900961001     O                       OGGI               188 '  /  /    '
099000950406     O                                          194 'PAG.'
099100950111     O                       PAGE          Z    198
099200950111     O          E            TESTA            02
099300950111     O                       KNSIF               10
099400950111     O                       KNMUS               21
099500961001     O                       WTIME              186 '0 :  :  '
099600950406     O*  41ON = GIACENZE IN RITARDO DI ESECUZIONE DISPOSIZIONI
099700950406     O          E    41      TESTA            03
099800961001     O                                              'PUNTO OPERATIVO'
099900961001     O                                           32 'APERT. GIACENZA:'
100000961001     O                       GCPCCR              38
100100961001     O                       DCCR                61
100200950406     O*  40ON = GIACENZE IN ATTESA DI DISPOSIZIONI MITTENTE
100300950111     O          E    40      TESTA            03
100400961001     O                                              'PUNTO OPERATIVO'
100500961001     O                                           25 'PARTENZA:'
100600961001     O                       WLNP                31
100700961001     O                       FILIA               54
100800950111     O          E            TESTA       2  1
100900950111     O                       CMP1               198
101000950111     O          E            TESTA          1
101100950111     O                       ST(4)               66
101200950113     O              N41      ST(5)              132
101300950113     O               41      ST(6)              132
101400950113     O               40      ST(7)              198
101500950113     O               41      ST(8)              198
101600950113     O               43      ST(9)              198
101700950111     O          E            TESTA
101800950111     O                       CMP2               198
101900950406     O*----------------------------------------------------------------
102000900515     O          E            DET            1
102100950407     O                       GCPFGC               3
102200950112     O                                            4 '-'
102300950112     O                       GCPNGC        Z     11
102400950112     O                       DATAGC              22 '  /  /    '
102500950112     O                       DATDXD              33 '  /  /    '
102600950112     O                       RAGMIT              59
102700950112     O                       RIFNUM        Z     75
102800950112     O                       GCPCMC              79
102900050223     O                       GCPLNPa             83
103000950112     O                                           84 '-'
103100050223     O                       GCPNRSa       Z     86
103200050223     O                       GCPNSPa       Z     94
103300950112     O                       DATSPE             105 '  /  /    '
103400950112     O                       ARBNCL        Z    111
103500950112     O                       ARBPKF             120 '      , '
103600950112     O              N41      STPRSD             136
103700950112     O              N41      AR9CAS        2    154
103800950112     O              N41 06                      154 '    '
103900950112     O              N41      AR6IFT        2    170
104000991119     O              N41 07                      170 '    '
104100991119     O              N41      AR9VCA             158
104200991119     O              N41      AR6DIV             174
104300991119     O              N41      DATDUR             185 '  /  /    '
104400950112     O               41      DS1                161
104500950112     O               41      DATDDM             181 '  /  /    '
104600950112     O               41      DATDED             192 '  /  /    '
104700991119     O               43      ARBGGA             194
104800950407     O          E    40 44   DET            1
104900950406     O                                          100 'APERTA IN PARTENZA SU SP'
105000950406     O                                         +  0 'EDIZIONE ORIGINALE:'
105100050223     O                       gcplnp             124
105200950407     O                                          125 '-'
105300050223     O                       gcpnrs        Z    128
105400050223     O                       gcpnsp        Z    137
105500950406     O                                          142 'DEL'
105600050223     O*                      DATORI             154 '  /  /    '
105700950406     O                                          159 'PER'
105800050223     O                       gcplna             164
105900950112     O          E   N41      DET            1
106000950112     O                    N24§2ADES             151
106100950112     O                     24DMC                126
106200961219     O          E            TESTAI           01
106300980205     O                       ST(15)             132
106400961219     O                       RSUT                20
106500961219     O                                           21 '/'
106600961219     O                       SIMFEL              24
106700961219     O                                          176 'FNLG43R'
106800961219     O                       OGGI               188 '  /  /    '
106900961219     O                                          194 'PAG.'
107000961219     O                       PAGE          Z    198
107100961227     O          E            TESTAI           02
107200980205     O                       ST(16)             132
107300961219     O                       KNSIF               10
107400961219     O                       KNMUS               21
107500961219     O                       WTIME              186 '0 :  :  '
107600961219     O          E            TESTAI           03
107700961219     O                                            1 '-'
107800961219     O                       GCPCCR               5
107900961219     O                       DCCR              +  1
108000980205     O                       ST(17)             132
108100961219     O          E            TESTAI      2  1
108200961219     O                       CMP1               198
108300961219     O          E            TESTAI         1
108400961219     O                       ST(12)              66
108500961219     O                       ST(13)             132
108600961219     O                       ST(14)             198
108700980520     O               29                         190 'A W B'
108800961219     O          E            TESTAI
108900980205     O                       CMP2               198
109000961219     O*----------------------------------------------------------------
109100961219     O          E            DETI           1
109200961219     O                       GCPFGC               3
109300961219     O                                            4 '-'
109400961219     O                       GCPNGC        Z     11
109500961219     O                       DATDXD              33 '  /  /    '
109600980205     O                       RAGDES              59
109700980205     O                       CAPDES              75
109800050223     O                       GCPLNPa             83
109900961219     O                                           84 '-'
110000050223     O                       GCPNRSa             86
110100050223     O                       GCPNSPa             94
110200961219     O                       DATSPE             105 '  /  /    '
110300961219     O                       ARBNCL        Z    111
110400961219     O                       ARBPKF             120 '      , '
110500961219     O                       DS1                161
110600961219     O                       DATFAX             181 '  /  /    '
110700980520     O               29      AWB                195
110800950406     O*----------------------------------------------------------------
110900950407**   ST
111000980205LISTA GIACENZE IN ATTESA DI DISPOSIZIONI MITTENTE DA OLTRE GG. XXX  1
111100950406LISTA GIACENZE IN RITARDO DI ESECUZ.DISPOSIZIONI DA OLTRE GG. XXX   2
111200950111LISTA GIACENZE APERTE IN RITARDO OLTRE GG. XXX  DAL            AL   3
111300950111NR.GIACENZA APERTA IL  DATA COMUN MITTENTE                     RIF  4
111400950113.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   DESTINATARIO 5
111500950113.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   D I S P O S  6
111600991119O      CONTRASSEGNO        TOT.FATTURA     RIAPERTURA               7
111700950113 I Z I O N I     M I T T E N T E       IMM. DISP. ESEC.DISP.        8
111800991119O      CONTRASSEGNO        TOT.FATTURA     RIAPERTURA  PARTICOLAR.  9
111900961219              REMAINDER UNDELIVERABLE SHIPMENTS LIST:              10
112000961219              - OUR INSTRUCTIONS NOT JET CURRIED US -              11
112100980205PROTOCOLLE NR.         OPEN  ON   CONSIGNEE                 POSTAL 12
112200980205 CODE          NUMBER / DATE SHIPMENT   COLLI     WGT  I N S T R U 13
112300980205 C T I O N S     GIVEN WITH ....        FAX DATED                  14
112400980205                  THESE SHPTS WERE UNDELIVERABLE                   15
112500980205                WE GAVE YOU DELIVERY INSTRUCTIONS                  16
112600980205              PLS GIVE US PRESENT STATUS OF SHIPMENT               17
112700961219**
112800950407OVRPRTF FILE(PRTF198) USRDTA('Lista Giac')
