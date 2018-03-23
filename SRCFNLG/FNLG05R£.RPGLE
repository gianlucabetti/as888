000100130315     /*PRM dbgview(*source)
000200130315     /*END
000300000809     H DECEDIT('0,') DATEDIT(*DMY.)
000400940914     H* FNLG05R *----------------------------------------------------*
000500940927     H****************************************************************
000600940927     H*          - IMPOSTAZIONE DEI PARAMETRI DS FNLG05DS -          *
000700940927     H****************************************************************
000800940927     H*  D05GES (Tipo gestione) = 'P' se sto manutenzionando le      *
000900940927     H*                           giacenze in partenza.              *
001000151204     H*                         = 'A' se sto manutenzionando/aprendo *
001100940927     H*                           le giacenze in arrivo.             *
001200940927     H*  D05RIC (Richiamo pgm.) = 'S' se la funzione di manutenzione *
001300940927     H*                           giacenze è richiamata da altri pgm *
001400940927     H*                         = ' ' se è richiamata direttamente   *
001500940927     H*                           da menù. In quest'ultimo caso i    *
001600940927     H*                           campi successivi non saranno impo- *
001700940927     H*                           stati.                             *
001800940927     H*--------------------------------------------------------------*
001900940927     H*  S O L O       S E        R I C H I A M A T O                *
002000940927     H*--------------------------------------------------------------*
002100940927     H*  D05TRI (Tipo richiamo) = 'P' il programma è richiato da al- *
002200940927     H*                           tri per la prima volta.            *
002300940927     H*                         = 'U' il programma è richiato da al- *
002400940927     H*                           tri per l'ulitma volta.            *
002500940927     H*                         = ' ' il programma è richiato da al- *
002600940927     H*                           tri per l'ennesima volta o è sta-  *
002700940927     H*                           to chiamato direttamente da menù.  *
002800940927     H*  D05AAS (Anno spedizi.) = Anno spedizione della bolla mamma  *
002900940927     H*                           della giacenza da manutenzionare.  *
003000940927     H*  D05LNP (Fil. Partenza) = Linea di partenza bolla mamma della*
003100940927     H*                           giacenza da manutenzionare.        *
003200940927     H*  D05NRS (Numero Serie)  = Numero di serie bolla mamma della  *
003300940927     H*                           giacenza da manutenzionare.        *
003400940927     H*  D05NSP (Numero Spedi.) = Numero spedizione bolla mamma      *
003500940927     H*                           della giacenza da manutenzionare.  *
003600940927     H*  D05AGC (Anno giacenza) = Anno di apertura della giacenza    *
003700940927     H*                           da manutenzionare.                 *
003800940927     H*  D05FGC (Fil. apertura) = Filiale che ha aperto la giacenza  *
003900940927     H*  D05NGC (Num. apertura) = Numero identificativo spedizione   *
004000940927     H*                           giacente.                          *
004100940927     H*  D05FRG (Proggr.apertu) = Proggressivo apertura giacenza.    *
004200940927     H*  D05AAS (Anno sp.figlia)= Anno spedizione della bolla figlia *
004300940927     H*                           della giacenza da manutenzionare.  *
004400940927     H*  D05LNP (Lnp. sp.figlia)= Linea di partenza bolla figlia     *
004500940927     H*                           della giacenza da manutenzionare.  *
004600940927     H*  D05NRS (Nrs. sp.figlia)= Numero di serie bolla figlia della *
004700940927     H*                           giacenza da manutenzionare.        *
004800940927     H*  D05NSP (Nsp. sp.filgia)= Numero spedizione bolla figlia     *
004900940927     H*                           della giacenza da manutenzionare.  *
005000940927     H****************************************************************
005100940927     H*             - APERTURA/ VARIAZIONE GIACENZA -                *
005200940927     H****************************************************************
005300940914     FFNLG05D   CF   E             WORKSTN
005400050408     FTITGC01L  IF   E           K DISK
005500050608     FFNBLP01L  UF   E           K DISK    commit
005600050517     FFNARB01L  UF   E           K DISK    USROPN commit
005700060213     FFIAR401L  uF   E           K DISK    commit
005800990929     FFIAR601L  IF   E           K DISK    USROPN
005900051116     FFIAR901L  IF   E           K DISK    USROPN
006000050217     FFNCLS01L  IF   E           K DISK
006100050315     Ftigcp01L  UF A E           K DISK    commit infds(gcpinf)
006200950102     FFNLBL02L  IF   E           K DISK
006300950102     FFNLBL01L  IF   E           K DISK
006400940914     F                                     RENAME(FNLBL000:FNLBL001)
006500900418     FTABEL00F  IF   E           K DISK
006600961217     FEDRDE01L  IF   E           K DISK    USROPN
006700970225     FEDTAB01L  IF   E           K DISK    USROPN
006800070320     FAZORG01L  IF   e           k DISK
006900050315     FtIGAP01L  UF A E           K DISK    commit
007000050315     FtIGNP01L  UF A E           K DISK    commit
007100900418     FCNACO00F  IF   E           K DISK
007200900418     FCNIND00F  IF   E           K DISK
007300050517     FFNEVB05L  iF A E           K DISK    commit
007400050404     FFLNUF01L  UF A E           K DISK
007500040713     FTfntc01l  IF   E           K DISK
007600990518     FFNANM01L  IF   E           K DISK
007700990518     F                                     RENAME(FNANM000:FNANM001)
007800990518     F                                     INFDS(FNANM1)
007900091021     FFIAR501L  IF   E           K DISK
008000050218     Ffigic00f  o    E             DISK
008100940914     H*--------------------------------------------------------------*
008200141029     d fnlry09ds2    e ds
008300050315     d trul82ds      e ds
008400050512     d fnlg07ds      e ds
008500170118     d fnlg071ds     e ds
008600050512     d fnlg08ds      e ds
008700050630     d fnlg09ds      e ds
008800160223     d fnlg24ds      e ds
008900090709     d fnlrg2ds      e ds
009000090701     d dslr02        e ds
009100091021     d tibs02ds      e ds
009200091021     d dar5gen       e ds
009300091021     d dcli          e ds
009400170922     d tigcp         e ds                  extname(tigcp00f)
009500170922     d fnlg33ds      e ds                  inz
009600170922     d tigcpa        e ds                  extname(tigcp00f)
009700170922     d                                     prefix(a)
009800170922     d tigcpb        e ds                  extname(tigcp00f)
009900170922     d                                     prefix(b)
010000050315     d gcpinf          ds
010100050315     d  gcpnrr               397    400i 0
010200940914     D*  Caricamento capoconto clienti
010300940914     D*  Chiavi per caricamento tabelle
010400020222     d* chiavi filiali
010500020430     D FI              S              3    DIM(30)
010600020222     d* filiali £1
010700020430     D £1              S              3  0 DIM(30)
010800020222     d* chiavi filiali
010900020430     D KEY             S              3  0 DIM(30)
011000020222     d* codie bolla
011100940919     D C3A             S              2    DIM(50)
011200020222     d* tipo  bolla
011300940914     D T3A             S              2    DIM(50)
011400020222     d* descr.bolla
011500940914     D D3A             S             25    DIM(50)
011600021031     d* codie bolla da non trasmettere in sede
011700021031     D g3A             S              1    DIM(50)
011800020222     d* codici eventi
011900080903     D C2A             S              3    DIM(200)
012000020222     d* gg.eventi
012100080903     D G2A             S              1  0 DIM(200)
012200020222     d* descriz.eventi
012300080903     D D2A             S             45    DIM(200)
012400020222     d* immissione s/n
012500080903     D F2A             S              1    DIM(200)
012600020222     d* tipologia evento
012700080903     D T2A             S              1    DIM(200)
012800020222     d* tipologia part.giac.
012900080903     D TPG             S              1    DIM(200)
013000090520     d* tipo servizio e descrizione
013100090520     D c5e             S              1    DIM(10)
013200090520     D d5e             S             15    DIM(10)
013300040115     d* motivo ammesso per data consegna richiesta
013400080903     D dcr             S              1    DIM(200)
013500020222     d* codici part.giac.
013600080903     D C7Q             S              2    DIM(200)
013700020222     d* descr. part.giac.
013800080903     D D7Q             S             89    DIM(200)
013900021031     d* codici part.giac. estensione
014000080903     D C7W             S              2    DIM(200)
014100021031     d* descr. part.giac. estensione
014200110531     D D7W             S             89    DIM(200)
014300020222     d* codici eventi
014400940920     D C2B             S              3  0 DIM(50)
014500020222     d* descriz.eventi
014600940920     D D2B             S             20    DIM(50)
014700020222     d* codici com.giac.
014800940919     D C2F             S              1    DIM(50)
014900020222     d* tipo comun.
015000960201     D T2F             S              1    DIM(50)
015100020222     d* invio comun.
015200960201     D F2F             S              1    DIM(50)
015300050512     d* testo estero
015400050512     D e2F             S              1    DIM(50)
015500020222     d* descrz. comun.
015600940920     D D2F             S             20    DIM(50)
015700050614     d* riservata ced
015800050614     D r2F             S              1    DIM(50)
015900050614     d* stampa automatica comunicaz. mittente
016000050614     D s2F             S              1    DIM(50)
016100050512     d* scrittura FNVAG00F
016200050512     D v2F             S              1    DIM(50)
016300020222     d* cod. naz.
016400011217     D CNZ             S              3    DIM(500)
016500020222     d* flag ita/est
016600011217     D ITA             S              1    DIM(500)
016700020222     d* filiale poste
016800000526     D FIP             S              3  0 DIM(50)
016900040329      *
017000040329     D w99             S              1    dim(99)
017100020222     d* messaggi errore
017200170530     D ERR             S             70    DIM(40) CTDATA PERRCD(1)
017300020222     d* QCAEXEC
017400970225     D CHK             S             45    DIM(2) CTDATA PERRCD(1)
017500040329     D CMA4            S             99    dim(1) ctdata perrcd(1)
017600040329     D CMA5            S             99    dim(1) ctdata perrcd(1)
017700970226     D* Schiere tabelle EDI
017800020222     D* EVENTI 2A SU EDTAB
017900080903     D E2A             S              3    DIM(200)
018000020222     D* FLG AGG.LISTA LAB.
018100080903     D FTL             S              1    DIM(200)
018200020222     D* FLG AGG.LISTA VGL
018300080903     D FTV             S              1    DIM(200)
018400020222     D* COD.ANAGR.PARTNER
018500080903     D KSC             S              7    DIM(200)
018600020222     D* IDENTIF.PARTNER
018700080903     D CPT             S             35    DIM(200)
018800020222     D* TIPO LISTA
018900080903     D TLI             S              1    DIM(200)
019000150512     D fnlv59ds      E DS
019100050217     D fnlblds       E DS                  EXTNAME(FNLbl00f) inz
019200050217     D lblsav        E DS                  EXTNAME(FNLbl00f) prefix(£) inz
019300940920     D* DATI X RICHIAMO PGM INTERR. BOLLE PARTENZE
019400940920     D PARAM0          DS
019500940920     D  PA0AAS                 1      4  0
019600940920     D  PA0LNP                 5      7  0
019700940920     D  PA0NRS                 8      9  0
019800940920     D  PA0NSP                10     16  0
019900940921     D  PA0FLG                18     18
020000940920     D  PA0SIM                44     46  0
020100940921     D  PA0F03                48     48
020200161114     D*PARAM1          DS
020300161114     D* PA1AAS                14     17  0
020400161114     D* PA1LNP                18     20  0
020500161114     D* PA1NRS                21     22  0
020600161114     D* PA1NSP                23     29  0
020700161114     D* PA1F03                30     30
020800161114     D* PA1FLG                31     31
020900161118     d fnlru6ds      e ds
021000161114      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R
021100161114     D PARAMu6ds       DS
021200161114     d* CAMPI DI OUTPUT:
021300161114     d* . Key spedizione
021400161114     D  pa1AAS                14     17  0
021500161114     D  pa1LNP                18     20  0
021600161114     D  pa1NRS                21     22  0
021700161114     D  pa1NSP                23     29  0
021800161114     d* . Flag '1'=Premuto F3=Fine
021900161114     D  PA1F03                30     30
022000161114     d* CAMPI DI INPUT:
022100161114     d* . Modalità di richiamo
022200161114     D  PA1FLG                31     31
022300161114     D* . Flag '1'= RICH DA PGM GIAC. (Serve per disabilitare tasto funzionale F
022400161114     d*                                in interrogazione della bolla ed evitare
022500161114     d*                                errore di chiamata ricorsiva
022600161114     D  PA1GIA               144    144
022700161114     d* . Solo dal chiamante FNLRU6R1 per il Perfect Order Wurth:
022800161114     d  pa1rma               149    163
022900161114     d  pa1xco               164    164
023000161114     d* non utilizzato
023100161114     d  pa1flo               165    256
023200161114      * ?Parametri pgm interrogaz. Bolle di Filiale FNLRU6R1
023300161114     D PARAMU6ds1      DS
023400161114     d** INPUT/OUTPUT (se richiesta la selezione bolla)
023500161114     D  pa1AAS_               14     17  0
023600161114     D  pa1LNP_               18     20  0
023700161114     D  pa1NRS_               21     22  0
023800161114     D  pa1NSP_               23     29  0
023900161114     d** OUTPUT
024000161114     D  PA1F03_               30     30
024100161114     d** INPUT
024200161114     D  PA1FLG_               31     31
024300161114     D*    SE RICH DA PGM GIAC. = 1
024400161114     D  PA1GIA_              144    144
024500161114     d** OUTPUT
024600161114     d  pa1rma_              149    163
024700161114     d  pa1xco_              164    164
024800161114     d** INPUT
024900161114     d*   Tipo lancio: "C" -           CHIUSO CON LR
025000161114     d*                "L" - ELABORO E CHIUDO CON LR
025100161114     d*                " " - ELABORO E CHIUDO IN RETRN
025200161114     d  pa1tla_              165    165
025300161124     d** OUTPUT - Codice di errore 1=Sped. non trovata
025400161124     d  pa1err_              166    166
025500161124     d  pa1flo_              167    256
025600900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
025700940919     D  TCU                  398    697
025800900418     D                                     DIM(50)
025900900418     D  KCU                  698    847P 0
026000900418     D                                     DIM(50)
026100900418     D                                     PACKEVEN
026200021114     D Tibs36ds      E DS
026300940914     D WLBDA8          DS
026400940914     D  G02DAT                 1      8  0
026500940914     D  G02INV                 9     16  0
026600940914     D  G02ERR                17     17
026700940914     D  G02TGI                18     22  0
026800950105      *
026900950105     D DATPAR          DS
027000950105     D  GIODAT                 1      8  0
027100950105     D  GIOINV                 9     16  0
027200950105     D  GIOTGI                17     21  0
027300950105     D GFES8           DS
027400950105     D  DATADA                 1      8  0
027500950105     D  DATAA                  9     16  0
027600950105     D  GGF                   17     21  0
027700040115     d Wdata8          DS
027800040115     d  dadata                 1      8  0
027900040115     d  adata                  9     16  0
028000040115     d  GioLav                17     21  0
028100040115     d dataiso         s               d   datfmt(*iso)
028200050609     d savind28        s              1    inz('0')
028300040115      *
028400160316     D TRUL27        E DS                  EXTNAME(TRUL27DS1)
028500020222     D DSTB69        E DS                  EXTNAME(DS7PQRS)
028600970401     D DSLR33        E DS                  EXTNAME(FNLR33DS)
028700950113     D DS15          E DS
028800941003     D DS3A          E DS
028900911028     D DS2A          E DS
029000050401     D DS2U          E DS
029100950104     D DS2Z          E DS
029200940920     D DS2F          E DS
029300990805     D DS2G          E DS
029400000202     D OG143         E DS
029500070320     D OG148         E DS
029600070410     D OG146         E DS
029700050217     D DS3k          E DS
029800140410     d ds7r          e ds
029900050418     D DS7Q          E DS
030000020222     d* tipologia cod.giac.7
030100950102     D  TIP                   78     82
030200950104     D                                     DIM(5)
030300950104     D  WTIP                  78     82
030400020222     d* descr. cod.giac.7
030500950102     D  DSP                   83     87
030600950104     D                                     DIM(5)
030700021031     D DS7W          E DS
030800110531     D  Tmt                   25     39
030900110531     D                                     DIM(5)
031000900418     D TCUDS           DS
031100900418     D  F3                     3      3
031200900418     D  F4                     4      4
031300900418     D  F56                    5      6
031400900418     D CNCR80        E DS
031500900418     D KPJBA         E DS
031600950104     D* Passo pgm di stmampa segnacolli
031700950104     D  WPGMSS               492    501
031800021106     D savKPJBu        S                   like(kpjbu)
031900070705     D §kpjbu          S                   like(kpjbu)
032000100614     D savfbc          S                   like(arbfbc)
032100021106
032200970226     D EDDSPT        E DS                  EXTNAME(EDIDSPT)
032300970226     D EDDSPU        E DS                  EXTNAME(EDIDSPU)
032400970226     D EDDS2A        E DS                  EXTNAME(EDIDS2A)
032500970226     D FNLG05        E DS                  EXTNAME(FNLG05DS)
032600970624     D FNLG03        E DS                  EXTNAME(FNLG03DS)
032700950102     D FNLG30        E DS                  EXTNAME(FNLG30DS)
032800950104     D FNLG82        E DS                  EXTNAME(FNLG82DS)
032900960201     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
033000960201     D TRUL42        E DS                  EXTNAME(TRUL42DS)
033100960306     D TRUL43        E DS                  EXTNAME(TRUL43DS)
033200171122     d TRUL43_1      E DS                  EXTNAME(TRUL43DS1)
033300020430     d trul06ds      e ds
033400020430     d  lin                    1     90  0 dim(30)
033500040310     d dsbl4a        e ds
033600070320     d dsbl4l        e ds
033700040329     d fnlsb5ds      e ds
033800130315     d FNLSB6ds1     e ds                  inz
033900130315     d  DB6pdf       e                     inz('X')
034000080618     d truldesds     e ds                  inz
034100040716     d fidn12ds      e ds
034200940915     D                 DS
034300940915     D  CDTAB                 52     53
034400940915     D  DSMSG                  1     70
034500940915     D                 DS
034600940915     D  WGG                    1      2  0
034700940915     D  WMM                    3      4  0
034800940915     D  WGGMM                  1      4  0
034900950206     D                 DS
035000950206     D  §SEI                   1      6
035100950206     D  §PRF                   1      3
035200950206     D  §FLE                   4      6  0
035300990518     D FNANM1          DS
035400970401     D  ANMNR1               397    400B 0
035500021031     d param80         ds
035600021031     d  fldlnpa                       3
035700021031     d  fldaasa                       4
035800021031     d  fldnrsa                       2
035900021031     d  fldnspa                       7
036000021031     d  fldcca                        1
036100021031     d  fldgga                        2
036200940914     C*--------------------------------------------------------------*
036300940914     C*  CICLO PRINCIPALE
036400940914     C*--------------------------------------------------------------*
036500940927     C     *ENTRY        PLIST
036600940927     C                   PARM                    KPJBA
036700940927     C                   MOVEL     KPJBU         FNLG05
036800961217     C*
036900941003     C                   MOVEL     'N'           WFINE             1
037000950104     C                   MOVEL     D05PSS        WPGMSS
037100950104     C                   CLEAR                   FNLG82
037200950104     C                   MOVE      D05MOB        D82MOB
037300950104     C                   MOVE      D05PRB        D82PRB
037400950104     C                   MOVE      D05MOE        D82MOE
037500950104     C                   MOVE      D05PRE        D82PRE
037600030207     C                   MOVE      d05pss        D82PSS
037700030716     c                   eval      d82mdb4 = d05mdb4
037800030716     c                   eval      d82mdb5 = d05mdb5
037900030716     c                   eval      d82psl = d05psl
038000030716     c                   eval      d82bsl = d05bsl
038100030716     c                   eval      d82prb4 = d05prb4
038200030716     c                   eval      d82prb5 = d05prb5
038300940927     C*--------------------------------------------------------------*
038400940927     C*  Se il programma è richiamato da altri per la prima volta
038500940927     C*  oppure la funzione di manutenzione giacenze è richiesta
038600940927     C*  direttamante da menù eseguo le operazioni di definizione
038700940927     C*  chiavi e caricamento tabelle
038800940927     C     D05RIC        IFNE      'S'
038900940927     C     D05TRI        OREQ      'P'
039000940927     C                   EXSR      DEFCAM
039100970226     C*  Se sono in partenza controllo se aprire EDTAB
039200970226     C     D05GES        IFEQ      'P'
039300970226     C                   EXSR      CHKTAB
039400970226     C*  Se sono in arrivo controllo se aprire EDRDE
039500970226     C                   ELSE
039600961217     C                   EXSR      CHKRDE
039700940927     C                   END
039800970227     C                   END
039900940927     C*--------------------------------------------------------------*
040000940927     C*  Se il pgm. è richiamato per l'ultima volta da altri devo
040100940927     C*  eseguire solo le operazioni di chiusura dei file
040200940927     C*--------------------------------------------------------------*
040300940927     C     D05TRI        IFNE      'U'
040400940927     C*--------------------------------------------------------------*
040500940927     C*  Inizio pgm
040600940927     C                   EXSR      INIPGM
040700940927     C*--------------------------------------------------------------*
040800940921     C     WFINE         DOWEQ     'N'
040900940914     C*  Loop di gestione prima videata
041000940914     C     WTPVID        CASEQ     '1'           GESD01
041100940914     C     WTPVID        CASEQ     '2'           GESD02
041200960123     C     WTPVID        CASEQ     '4'           GESD04
041300961025     C     WTPVID        CASEQ     '5'           GESD05
041400940914     C                   END
041500940914     C                   END
041600940927     C*--------------------------------------------------------------*
041700940927     C                   END
041800940927     C*--------------------------------------------------------------*
041900940919     C     FINE          TAG
042000970610     C     D05GES        IFEQ      'A'
042100970610     C     D05TRI        ANDNE     'U'
042200970606     C     *LOVAL        SETLL     FNARB01L
042300970610     C                   END
042400940927     C*--------------------------------------------------------------*
042500940927     C*  Se il pgm non è richiamato o viene richiamato per l'ulitima
042600940927     C*  volta chiudo con LR diversamente chiudo in RT
042700940927     C     D05TRI        IFEQ      'U'
042800940927     C     D05RIC        ORNE      'S'
042900020306     C                   CLEAR                   TRUL27
043000020306     C                   MOVEL     'C'           I27TLA
043100160316     C                   CALL      'TRUL27R1'
043200020306     C                   PARM                    TRUL27
043300160316      *
043400160316     C                   CLEAR                   fnlv59ds
043500160316     C                   MOVEL     'C'           Ilv59tla
043600160316     C                   CALL      'FNLV59R'
043700160316     C                   PARM                    fnlv59ds
043800160316
043900050630     C                   CLEAR                   fnlg09ds
044000050630     C                   MOVEL     'C'           d09TLA
044100050630     c                   movel     fnlg09ds      kpjbu
044200050630     C                   CALL      'FNLG09R'
044300050630     C                   PARM                    kpjba
044400161114
044500161118     c*                  eval      pa1tla_='C'
044600161118     c*                  eval      kpjbu=paramu6ds1
044700161118     c*                  call      'FNLRU6R1'
044800161118     c*                  parm                    kpjba
044900161114
045000940914     C                   SETON                                        LR
045100940927     C                   ELSE
045200940927     C                   SETON                                        RT
045300940927     C                   END
045400050609     c                   eval      savind28 = '0'
045500961217     C*-----------------------------------------------------*
045600961217     C*  Chkobj su EDRDE
045700961217     C*-----------------------------------------------------*
045800961217     C     CHKRDE        BEGSR
045900961217     C*
046000961217     C*  Controllo esistenza arcchivi
046100961217     C                   Z-ADD     45            LUNG             15 5
046200961217     C* EDRDE00F  Ordini Clienti
046300961217     C                   SETOFF                                         21
046400961217     C                   MOVEL     *BLANKS       COMMAN
046500040329     C                   MOVEA     CHK(1)        COMMAN           99
046600961217     C                   CALL      'QCMDEXC'                            21
046700961217     C                   PARM                    COMMAN
046800961217     C                   PARM                    LUNG
046900961217     C*
047000961217     C                   MOVEL     'N'           WTRRDE
047100961217     C     *IN21         IFEQ      '0'
047200961217     C                   OPEN      EDRDE01L
047300961217     C                   MOVEL     'S'           WTRRDE            1
047400961217     C                   END
047500961217     C*
047600961217     C                   ENDSR
047700970225     C*-----------------------------------------------------*
047800970225     C*  Chkobj su EDTAB
047900970225     C*-----------------------------------------------------*
048000970225     C     CHKTAB        BEGSR
048100970225     C*
048200970225     C*  Controllo esistenza arcchivi
048300970225     C                   Z-ADD     45            LUNG             15 5
048400970225     C* EDTAB00F  Ordini Clienti
048500970225     C                   SETOFF                                         21
048600970225     C                   MOVEL     *BLANKS       COMMAN
048700040329     C                   MOVEA     CHK(2)        COMMAN
048800970225     C                   CALL      'QCMDEXC'                            21
048900970225     C                   PARM                    COMMAN
049000970225     C                   PARM                    LUNG
049100970225     C*
049200970225     C                   MOVEL     'N'           WTRTAB
049300970225     C     *IN21         IFEQ      '0'
049400970225     C                   OPEN      EDTAB01L
049500970225     C                   MOVEL     'S'           WTRTAB            1
049600970311     C                   ELSE
049700970311     C                   GOTO      FINEDI
049800970225     C                   END
049900970226     C* Eseguo caricamento tab. EDI codici partner
050000970226     C                   MOVEL     'PT'          KCOD1
050100970226     C                   MOVEA     *BLANKS       CPT
050200970226     C                   MOVEA     *BLANKS       TLI
050300970227     C                   MOVEA     *BLANKS       KSC
050400071107     C                   Z-ADD     0             X                 3 0
050500970226     C     KCOD1         CHAIN     EDTAB01L                           31
050600970226     C     *IN31         DOWEQ     '0'
050700080903     C     X             ANDLT     200
050800970226     C     TABFLG        IFEQ      ' '
050900970226     C                   MOVEL     TABUNI        EDDSPT
051000970226     C                   ADD       1             X
051100970227     C                   MOVEL     TABKEY        CPT(X)
051200970226     C                   MOVEL     §PTKSC        KSC(X)
051300970226     C                   END
051400970227     C     KCOD1         READE     EDTAB01L                               31
051500970226     C                   END
051600970226     C*  Se non ho caricato tutti i dati gestisco errore
051700970226     C     *IN31         IFNE      '1'
051800970226     C                   EXSR      ERREDI
051900970226     C                   GOTO      FINEDI
052000970226     C                   END
052100970226     C*  Carico tabella correlata PU
052200970226     C                   MOVEL     'PU'          KCOD1
052300970226     C     KCOD1         CHAIN     EDTAB01L                           31
052400970226     C     *IN31         DOWEQ     '0'
052500970226     C                   MOVEL     TBLUNI        EDDSPU
052600970226     C     TABFLG        IFEQ      ' '
052700071107     C                   Z-ADD     1             X
052800970227     C                   MOVEL     TABKEY        WCCPT            35
052900970226     C     WCCPT         LOOKUP    CPT(X)                                 32
053000970226     C     *IN32         IFEQ      '1'
053100970227     C                   MOVEL     TABUNI        EDDSPU
053200970226     C                   MOVE      §PULAB        TLI(X)
053300970226     C                   END
053400970226     C                   END
053500970227     C     KCOD1         READE     EDTAB01L                               31
053600970226     C                   END
053700970226     C*---------------------------------------------------------------*
053800970226     C* Carico tabella 2a
053900071107     C                   Z-ADD     0             X
054000970226     C                   MOVEL     '2A'          KCOD1
054100970226     C     KCOD1         CHAIN     EDTAB01L                           31
054200970226     C     *IN31         DOWEQ     '0'
054300080903     C     X             ANDLT     200
054400970226     C     TABFLG        IFEQ      ' '
054500970226     C                   MOVEL     TABUNI        EDDS2A
054600970226     C                   ADD       1             X
054700970226     C                   MOVEL     TABKEY        E2A(X)
054800970226     C                   MOVEL     §2ADAG        FTL(X)
054900970226     C                   MOVEL     §2ADAV        FTV(X)
055000970226     C                   END
055100970226     C     KCOD1         READE     EDTAB01L                               31
055200970226     C                   END
055300970226     C*  Se non ho caricato tutti i dati gestisco errore
055400970226     C     *IN31         IFNE      '1'
055500970226     C                   EXSR      ERREDI
055600970226     C                   GOTO      FINEDI
055700970226     C                   END
055800970226     C*
055900970226     C     FINEDI        ENDSR
056000940914     C*-----------------------------------------------------*
056100940914     C*  Gestione della prima videata
056200940914     C*-----------------------------------------------------*
056300940919     C     GESD01        BEGSR
056400940914     C*
056500940914     C                   EXFMT     LG05D01
056600940915     C                   SETOFF                                       404142
056700940915     C                   SETOFF                                       28
056800940915     C                   MOVEL     *BLANKS       $MSG
056900940914     C*  Fine Lavoro
057000940914     C     *INKC         IFEQ      '1'
057100940914     C                   MOVEL     'S'           WFINE
057200940914     C                   GOTO      FINVD1
057300940914     C                   END
057400961025     C* Cambio Filiale
057500961025     C     *INKS         IFEQ      '1'
057600961028     C                   Z-ADD     §FLE          V5CFLE
057700961028     C                   MOVEL     V1CRSU        V5CRSU
057800961025     C                   MOVEL     '5'           WTPVID
057900961025     C                   GOTO      FINVD1
058000961025     C                   END
058100940920     C*  Richiamo interr. bolle
058200940915     C     *INKG         IFEQ      '1'
058300940920     C                   EXSR      RICHBL
058400940919     C                   GOTO      FINVD1
058500940915     C                   END
058600940920     C*  Richiamo interr. giacen
058700940920     C     *INKN         IFEQ      '1'
058800950102     C                   CLEAR                   FNLG30
058900950102     C                   MOVEL     D05GES        D30POA
059000950102     C                   Z-ADD     V1CAAS        D30AAS
059100950102     C                   Z-ADD     V1CLNP        D30LNP
059200950102     C                   Z-ADD     V1CNRS        D30NRS
059300950102     C                   Z-ADD     V1CNSP        D30NSP
059400950102     C                   MOVE      *ZEROS        D30FRG
059500950102     C                   MOVEL     FNLG30        KPJBU
059600950102     C     D05GES        IFEQ      'P'
059700950102     C                   CALL      'FNLG30C'
059800940920     C                   PARM                    KPJBA
059900950102     C                   ELSE
060000950102     C                   CALL      'FNLG31C'
060100950102     C                   PARM                    KPJBA
060200950102     C                   END
060300940920     C                   GOTO      FINVD1
060400940920     C                   END
060500940920     C*
060600940920     C                   Z-ADD     V1CAAS        KAAS
060700940920     C                   Z-ADD     V1CLNP        KLNP
060800940920     C                   Z-ADD     V1CNRS        KNRS
060900940920     C                   Z-ADD     V1CNSP        KNSP
061000940920     C* Se anno a 2 lo porto a  4
061100940920     C                   MOVEL     V1CAAS        WCOM2             2 0
061200940920     C     WCOM2         IFEQ      0
061300940920     C     V1CAAS        IFGT      60
061400940920     C                   ADD       1900          V1CAAS
061500940920     C                   ELSE
061600940920     C                   ADD       2000          V1CAAS
061700940920     C                   END
061800940920     C                   END
061900000526     C     V1CLNP        LOOKUP    FIP                                    41
062000000526     C   41              MOVEL     ERR(27)       $MSG
062100000526     C   41              SETON                                        28
062200000526     C   41              GOTO      FINVD1
062300940920     C*
062400950102     C* Se sono in arrivo reperisco codice mancata consegna su figlia
062500951229     C                   Z-ADD     0             D05AAF
062600951229     C                   Z-ADD     0             D05LNF
062700951229     C                   Z-ADD     0             D05NRF
062800951229     C                   Z-ADD     0             D05NSF
062900950102     C     D05GES        IFEQ      'A'
063000950102     C     KSPE1         CHAIN     FNLBL02L                           31
063100950224     C     *IN31         IFEQ      '0'
063200950102     C                   Z-ADD     LBLAAN        KAAS
063300950102     C                   Z-ADD     LBLLPN        KLNP
063400950102     C                   Z-ADD     LBLNRN        KNRS
063500950102     C                   Z-ADD     LBLNSN        KNSP
063600950224     C                   Z-ADD     LBLAAN        D05AAF
063700950224     C                   Z-ADD     LBLLPN        D05LNF
063800950224     C                   Z-ADD     LBLNRN        D05NRF
063900950224     C                   Z-ADD     LBLNSN        D05NSF
064000050404     C     KSPE1         CHAIN(n)  FNARB01L                           31
064100950706     C     *IN31         IFEQ      '0'
064200950706     C                   MOVE      ARBCMC        D05CMC
064300950706     C                   MOVE      ARBDDC        G02INV
064400950706     C                   MOVEL     '3'           G02ERR
064500950706     C                   Z-ADD     0             G02DAT
064600950706     C                   CALL      'XSRDA8'
064700950706     C                   PARM                    WLBDA8
064800950706     C                   MOVE      G02DAT        D05DXD
064900950102     C                   END
065000021031     C                   Z-ADD     1             X
065100950706     C*
065200950706     C                   END
065300950224     C                   END
065400950102     C*
065500940915     C                   Z-ADD     V1CAAS        KAAS
065600940915     C                   Z-ADD     V1CLNP        KLNP
065700940915     C                   Z-ADD     V1CNRS        KNRS
065800940915     C                   Z-ADD     V1CNSP        KNSP
065900050824     c                   clear                   wriape
066000050315     C     KSPE2         CHAIN(e)  tigcp01L                           30
066100050315     c                   if        %error
066200070521     C     KSPE2         CHAIN(n)  tigcp01L
066300050315     c                   exsr      geslck
066400050318     C     KSPE2         CHAIN(e)  tigcp01L                           30
066500050315     c                   endif
066600940915     C* Se sono in partenza eseguo routine controlli partenza
066700940919     C     D05GES        IFEQ      'P'
066800171017     c                   eval      tigcpb = tigcp
066900940915     C                   EXSR      CTRPAR
067000940915     C                   ELSE
067100960514     C* Se la giacenza esiste già e il programma è richiamato
067200960514     C* gestisco devo effettuare una riapertura
067300050408     C     *IN30         IFEQ      '0'
067400050615     C     GCPFAS        ANDeq     35
067500050615     c                   eval      wriape = 'S'
067600050615     c                   else
067700050615     C     *IN30         IFEQ      '0'
067800050615     C     GCPFAS        ANDGT     10
067900050408     C                   SETON                                        28
068000050408     C                   MOVEL     ERR(2)        $MSG
068100050408     C                   GOTO      FINVD1
068200050408     C                   end
068300050615     C                   end
068400940915     C* Se sono in partenza eseguo routine controlli arrivo
068500940915     C                   EXSR      CTRARR
068600940915     C                   END
068700940915     C*  Se ci sono errori vado a fine controlli + accendo ind.
068800940915     C     *IN28         IFEQ      '1'
068900940915     C                   SETON                                        404142
069000940915     C                   ELSE
069100940915     C                   EXSR      INZ02
069200940915     C                   END
069300940915     C*
069400940915     C     FINVD1        ENDSR
069500940919     C*-----------------------------------------------------*
069600940919     C*  Gestione della prima videata
069700940919     C*-----------------------------------------------------*
069800940919     C     GESD02        BEGSR
069900940919     C*
070000941227     C     WRIAPE        COMP      'S'                                2222
070100971112     C* Per arrivo attivo F14 x scrittura note arrivo se estero
070200971112     C                   SETOFF                                       14
070300971112     C     D05GES        IFEQ      'A'
070400980112     C     LNAFL1        ANDEQ     'E'
070500971112     C                   SETON                                        14
070600971112     C                   END
070700941227     C*
070800070705     c     vedibolla     tag
070900950105     C     *IN28         IFEQ      '1'
071000950105     C                   SETOFF                                       28
071100950105     C                   WRITE     LG05D02
071200950105     C                   SETON                                        28
071300950105     C                   END
071400050609     c* se esco dalla subroutine aggiornamento con errori proteggo il
071500050609     c* video
071600050609     c                   if        savind28 = '1'
071700050609     C                   write     LG05D02
071800050609     C                   exfmt     LG05D08
071900050609     c                   else
072000940919     C                   EXFMT     LG05D02
072100050609     c                   end
072200070705      * richiama interrogazione bolle
072300070705     c                   if        *inkg
072400070705     c                   exsr      richbla
072500070705     c                   goto      vedibolla
072600070705     c                   endif
072700090701      * richiama blocco sblocco
072800090701     c                   if        *inki
072900090701     c                   exsr      richblosbl
073000090707     c                   if        arbfbc <> *blank
073100090701     c                   goto      vedibolla
073200090707     c                   else
073300090707     c                   eval      *inkl = *on
073400090707     c                   goto      ritorno
073500090707     c                   endif
073600090701     c                   endif
073700000808      *CONTROLLO SE SUPERATO IL LIMITE DI APERTURE EMETTO VIDEO
073800000808      *SEGNALAZIONE
073900000808     C     D05GES        IFEQ      'A'
074000050629      * se particolarità giacenza = '  ' forzo il caricamento della 'GR'
074100050629     c                   if        arbgga = *blank
074200050629     c                   z-add     1             gr                3 0
074300050629     c     'GR'          lookup    c7q(gr)                                55
074400050629     c   55              movel     d7q(gr)       ds7q
074500050629     c                   endif
074600050629     c* conto quanti record di giacenza ho e quanti di questi sono motivi
074700050629     c* di aperture tecniche
074800050629     c                   exsr      srconta
074900050630     c* se non sto facendo una riapertura tecnica
075000050629     C                   if        v2ccmc <> '999' AND V2CCMC <> ' '
075100100614      * verifico il conteggio solo se per la particolarità è inserito un
075200100614      * valore minimo di aperture altrimenti salto l'emissione delle finestre
075300100614     c                   if        §7q5rp > 0
075400050629     c* ultima riapertura possibile
075500050629     C                   if        (cnt-cntced) = (§7q5rp - 1)
075600050629     C                   EXFMT     LG05D06
075700050629     c                   endif
075800050629     c* reso per numero massimo riaperture
075900050629     C                   if        (cnt-cntced) >= §7q5rp
076000050629     C                   EXFMT     LG05D07
076100050629     c                   endif
076200100614     c                   endif
076300050629     c*
076400050629     C     *INKL         IFEQ      '1'
076500050629     C                   MOVEL     'S'           WFINE
076600050629     C                   GOTO      FINVD2
076700050629     C                   END
076800050629     C                   END
076900050629     C                   END
077000940919     C*                    ------------
077100940919     C                   SETOFF                                       404142
077200940919     C                   SETOFF                                       434445
077300940919     C                   SETOFF                                       462819
077400050609     c*
077500050609     c                   eval      *in28 = savind28
077600940919     C*  Ritorno
077700090707     c     ritorno       tag
077800940919     C     *INKL         IFEQ      '1'
077900940920     C     D05RIC        IFEQ      'S'
078000940919     C                   MOVEL     'S'           WFINE
078100940919     C                   ELSE
078200950224     C                   MOVEL     D05GES        SAVGES
078300940920     C                   MOVEL     '1'           WTPVID
078400950224     C                   CLEAR                   FNLG05
078500950224     C                   MOVEL     SAVGES        D05GES
078600940919     C                   EXSR      PULVID
078700940919     C                   END
078800940919     C                   GOTO      FINVD2
078900940919     C                   END
079000940919     C* Controllo video
079100940919     C                   EXSR      CTR02
079200960123     C* Destinatario originale
079300960123     C     *INKM         IFEQ      '1'
079400960123     C                   MOVEL     '4'           WTPVID
079500960123     C                   GOTO      FINVD2
079600960123     C                   END
079700940919     C* aggiorna archivi
079800940919     C     *IN28         IFEQ      '0'
079900940919     C     *IN19         ANDEQ     '0'
080000050609     c                   eval      savind28  = *off
080100050516     c                   EXSR      GCPWTR
080200050516     c                   if        *in28
080300050609     c                   eval      savind28 = *in28
080400050516     C                   MOVEL     ERR(33)       $MSG
080500050516     c                   goto      finvd2
080600050516     c                   end
080700970624     C* Note di apertura giacenza
080800970626     C     *INKK         IFEQ      '1'
080900970624     C                   EXSR      CALL03
081000970624     C                   END
081100970624     C*
081200060403     C* variazione assegnazione spese/e cambio di porto
081300060403     C     *INKq         IFEQ      '1'
081400060403     C                   MOVEL(p)  kpjbu         savKPJBU
081500060403     C                   MOVEL     'P'           D82GES
081600060403     c                   move      'S'           d82gia
081700060403     C                   MOVE      gcpAGC        D82AGC
081800060403     C                   MOVE      gcpFGC        D82FGC
081900060403     C                   MOVE      gcpNGC        D82NGC
082000060403     C                   MOVE      gcpfrg        D82frg
082100060403     C                   MOVE      'S'           D82comm
082200060403     C                   MOVE      'S'           D82fspe
082300060403     C                   MOVEL(p)  FNLG82        KPJBU
082400060403     C                   CALL      'FNLG10R'
082500060403     C                   PARM                    KPJBA
082600060403     C                   MOVEL     KPJBU         FNLG82
082700060403     C                   MOVEL(p)  savkpjbu      KPJBU
082800060403     C                   END
082900060403     C*
083000940920     C     D05RIC        IFEQ      'S'
083100940920     C                   MOVEL     'S'           WFINE
083200940920     C                   ELSE
083300940920     C                   MOVEL     '1'           WTPVID
083400950224     C                   MOVEL     D05GES        SAVGES
083500950224     C                   CLEAR                   FNLG05
083600950224     C                   MOVEL     SAVGES        D05GES
083700940920     C                   EXSR      PULVID
083800940920     C                   END
083900940920     C                   END
084000940919     C*
084100940919     C     FINVD2        ENDSR
084200960123     C*-----------------------------------------------------*
084300050629     c* conto quanti record di giacenza ho e quanti di questi sono motivi
084400050629     c* di aperture tecniche
084500960123     C*-----------------------------------------------------*
084600050629     C     srconta       BEGSR
084700960123     C*
084800050630     c                   clear                   fnlg09ds
084900050630     c                   eval      d09tla = 'R'
085000050630     c                   eval      d09aas = kaas
085100050630     c                   eval      d09lnp = klnp
085200050630     c                   eval      d09nrs = knrs
085300050630     c                   eval      d09nsp = knsp
085400050630     c                   movel(p)  fnlg09ds      kpjbu
085500050630     C                   CALL      'FNLG09R'
085600050630     C                   PARM                    kpjba
085700050630     c                   movel(p)  kpjbu         fnlg09ds
085800050630     C                   Z-ADD     d09cnt        cnt               2 0
085900050630     C                   Z-ADD     d09ced        cntced            2 0
086000050629     C*
086100050629     C                   ENDSR
086200050629     C*-----------------------------------------------------*
086300050629     C*  Gestione della quarta videata
086400050629     C*-----------------------------------------------------*
086500050629     C     GESD04        BEGSR
086600050629     C*
086700960123     C                   EXFMT     LG05D04
086800960123     C*                    ------------
086900960123     C*  Ritorno
087000960123     C     *INKL         IFEQ      '1'
087100960123     C                   MOVEL     '2'           WTPVID
087200960123     C                   END
087300960123     C*
087400960123     C                   ENDSR
087500961025     C*------------------------------------------------------------*
087600961025     C*  Gestione QUINTA videata
087700961025     C*------------------------------------------------------------*
087800961025     C     GESD05        BEGSR
087900961025     C*
088000961025     C                   EXFMT     LG05D05
088100961025     C                   SETOFF                                       2840
088200961025     C* Ritorno
088300961028     C     *INKL         IFEQ      '1'
088400961025     C                   MOVEL     '1'           WTPVID
088500961028     C                   GOTO      FINVD5
088600961025     C                   END
088700961025     C* Controllo se filiale in £1
088800961025     C                   SETOFF                                       32
088900961025     C     V5CFLE        IFNE      0
089000020430     C                   MOVEL     V5CFLE        W£1               3 0
089100961025     C     W£1           LOOKUP    £1                                     32
089200961025     C                   END
089300961025     C     *IN32         IFEQ      '0'
089400961025     C                   SETON                                        2840
089500961028     C                   MOVEL     ERR(20)       $MSG
089600961025     C                   ELSE
089700961028     C                   Z-ADD     V5CFLE        §FLE
089800961025     C                   EXSR      CAR£6
089900961028     C                   MOVEL     '1'           WTPVID
090000961025     C                   END
090100961025     C*
090200961028     C     FINVD5        ENDSR
090300940915     C*-----------------------------------------------------*
090400940915     C*  Inizializzo seconda videata
090500940915     C*-----------------------------------------------------*
090600940915     C     INZ02         BEGSR
090700940919     C                   MOVEL     '2'           WTPVID
090800940919     C                   SETOFF                                       404142
090900940919     C                   SETOFF                                       434445
091000940919     C                   SETOFF                                       4628
091100940921     C                   SETOFF                                       050660
091200940915     C*  Se non ci sono errori passo alle videate successive
091300021031     c                   clear                   forzato           1
091400940919     C     D05GES        IFEQ      'P'
091500940915     C                   EXSR      BLPVID
091600940915     C                   ELSE
091700940915     C                   EXSR      ARBVID
091800940915     C                   END
091900040323     c     forzato       caseq     'X'           inipgm
092000040323     c                   end
092100940915     C* Se esiste giacenza carico i dati a video
092200940915     C     *IN30         IFEQ      '0'
092300940915     C                   EXSR      GCPVID
092400940915     C                   END
092500950301     C* Reperisco tipo mod.comunicaz. apertura giacenza
092600950301     C     D05GES        IFEQ      'P'
092700950301     C                   EXSR      APEGIA
092800050608     c                   else
092900050608     c                   clear                   flgagg            1 0
093000050608     C                   EXSR      APEGIAa
093100950301     C                   END
093200950301     C*
093300940919     C                   EXSR      GENVID
093400940915     C*
093500040323     C     endrut        ENDSR
093600940915     C*-----------------------------------------------------*
093700940915     C*  Controlli PARTENZA
093800940915     C*-----------------------------------------------------*
093900940915     C     CTRPAR        BEGSR
094000940915     C* PARTENZA !!!!!!
094100961204     C     KSPE1         CHAIN(N)  FNBLP01L                           31
094200940915     C     *IN31         IFEQ      '1'
094300940915     C                   SETON                                        28
094400940915     C                   MOVEL     ERR(6)        $MSG
094500940915     C                   GOTO      FINPAR
094600940915     C                   END
094700950217     C     BLPLNP        LOOKUP    KEY                                    32
094800940915     C* Profilo di 2° livello: Lnp deve essere ug. alla fil del prof.
094900940915     C*                         oppure non deve essere in £7
095000940922     C     D05RIC        IFNE      'S'
095100950217     C     *IN32         ANDEQ     '0'
095200940915     C* Profilo di 1° livello: deve essere in £7
095300940915     C                   SETON                                        28
095400940921     C                   MOVEL     ERR(16)       $MSG
095500940915     C                   GOTO      FINPAR
095600940922     C                   END
095700940915     C* Se sono in partenza non è ammessa l'apertura giacenza
095800940915     C     *IN30         IFEQ      '1'
095900940915     C                   SETON                                        28
096000940915     C                   MOVEL     ERR(3)        $MSG
096100940915     C                   GOTO      FINPAR
096200940915     C                   END
096300050217     C* Se esiste giacenza controllo se posso modificare stato
096400940915     C     GCPFAS        IFGE      20
096500940915     C                   SETON                                        28
096600940915     C                   MOVEL     ERR(5)        $MSG
096700940915     C                   GOTO      FINPAR
096800940915     C                   END
096900940915     C*
097000940915     C     FINPAR        ENDSR
097100940915     C*-----------------------------------------------------*
097200940915     C*  Controlli ARRIVO
097300940915     C*-----------------------------------------------------*
097400940915     C     CTRARR        BEGSR
097500940915     C* ARRIVO !!!!!!!!
097600141029     C     KSPE1         CHAIN(n)  FNARB01L                           3131
097700990203     C     *IN31         IFEQ      '1'
097800940915     C                   SETON                                        28
097900940915     C                   MOVEL     ERR(6)        $MSG
098000940915     C                   GOTO      FINARR
098100940915     C                   END
098200040113      * se da menu non si può aprire giacenza con particolarità C6
098300140410     c                   if        d05ric = ' ' and arbgma <> *blank
098400140410     c                   move      '7R'          kcod
098500140410     c                   movel(p)  arbgma        kkey
098600140410     c     ktbl1         chain     tabel00f
098700140410     c                   if        %found(tabel00f)
098800140410     c                   movel     tbluni        ds7r
098900140410     c                   if        §7rag = 'N'
099000040113     C                   SETON                                        28
099100040113     C                   MOVEL     ERR(29)       $MSG
099200040113     C                   GOTO      FINARR
099300140410     C                   endif
099400140410     C                   endif
099500040113     C                   END
099600040402      * se da menu non si può aprire giacenza se arbcmc valorizzato
099700040402     C                   IF        d05ric= ' ' and arbcmc <> *blank
099800040402     C                   SETON                                        28
099900040402     C                   MOVEL     ERR(31)       $MSG
100000040402     C                   GOTO      FINARR
100100040402     C                   END
100200940915     C* Se bolla di rientro non posso aprire giacenza
100300940915     C     ARBFBR        IFEQ      'S'
100400940915     C                   SETON                                        28
100500940915     C                   MOVEL     ERR(7)        $MSG
100600940915     C                   GOTO      FINARR
100700940915     C                   END
100800940915     C* Bolla con consegna parziale impossibile aprire giacenza
100900940915     C     ARBDCP        IFGT      0
101000940915     C                   SETON                                        28
101100940915     C                   MOVEL     ERR(8)        $MSG
101200940915     C                   GOTO      FINARR
101300940915     C                   END
101400940915     C* Bolla in consegna
101500940915     C     ARBNDC        IFNE      0
101600940915     C                   SETON                                        28
101700940915     C                   MOVEL     ERR(9)        $MSG
101800940915     C                   GOTO      FINARR
101900940915     C                   END
102000950228     C*
102100950228     C     ARBDCM        IFNE      0
102200950228     C                   SETON                                        28
102300950228     C                   MOVEL     ERR(17)       $MSG
102400950228     C                   GOTO      FINARR
102500950228     C                   END
102600940922     C* Profilo di 2° livello: Lnp deve essere ug. alla fil del prof.
102700940922     C*                         oppure non deve essere in £7
102800950217     C     ARBLNA        LOOKUP    KEY                                    32
102900940922     C     D05RIC        IFNE      'S'
103000950217     C     *IN32         ANDEQ     '0'
103100940922     C                   SETON                                        28
103200940922     C                   MOVEL     ERR(16)       $MSG
103300940922     C                   GOTO      FINARR
103400940922     C                   END
103500940915     C* Se la bolla digitata è una figlia con linea d'arrivo uguale
103600940915     C* alla bolla mamma mando messaggio (solo per arrivo)
103700050217     c                   clear                   fnlblds
103800050217     c                   clear                   lblsav
103900940919     C     KSPE1         CHAIN     FNLBL01L                           31
104000940915     C     *IN31         IFEQ      '0'
104100050217     C     LBLLAN        ifeq      LBLLAP
104200940915     C                   SETON                                        28
104300940915     C                   MOVEL     ERR(4)        $MSG
104400940915     C                   GOTO      FINARR
104500940915     C                   END
104600050217     c                   eval      lblsav = fnlblds
104700050217     C                   END
104800050517     c* controllo che esita la bolla in partenza
104900050517     c                   if        £LBLAAO = 0
105000050517     c     kspe1         chain(n)  fnblp01l
105100050517     c                   else
105200050517     c     kblp          chain(n)  fnblp01l
105300050517     c                   end
105400050517     C                   IF        not %found(fnblp01l)
105500050517     C                   SETON                                        28
105600050517     C                   MOVEL     ERR(6)        $MSG
105700050517     C                   GOTO      FINARR
105800050517     C                   END
105900940915     C* Se esiste giacenza ed è già trasmessa non ammessa manuten.
106000050217     C  N30GCPfas        IFgt      10
106100941223     C     WRIAPE        ANDEQ     ' '
106200940915     C                   SETON                                        28
106300940915     C                   MOVEL     ERR(2)        $MSG
106400940915     C                   GOTO      FINARR
106500940915     C                   END
106600050217      * Controllo c.a sia come mamma che come figlia                         ALL
106700040716     c                   clear                   fidn12ds
106800040716     c                   eval      i12aas = kaas
106900040716     c                   eval      i12lnp = klnp
107000040716     c                   eval      i12nrs = knrs
107100040716     c                   eval      i12nsp = knsp
107200040716     c                   eval      i12fel = 'E'
107300040716     c                   eval      i12fan = 'E'
107400040716     c                   eval      i12fch = 'E'
107500040716     c                   call      'FIDN12R'
107600040716     c                   parm                    fidn12ds
107700040716      **
107800040716      * se c'è almeno una C.A. aperta
107900040716      **
108000040716     c     o12nca        ifgt      0
108100990203     C                   SETON                                        28
108200990203     C                   MOVEL     ERR(25)       $MSG
108300990203     C                   GOTO      FINARR
108400990203     C                   END
108500141029      **
108600141029      * se c'è almeno una disposizione di consegna
108700141029      **
108800141029     c                   if        d05ric = *blank
108900141029      *  dirottamento
109000141029     c                   clear                   fnlry09ds2
109100141029     c                   eval      ilry09tdis= 'D'
109200151204     c                   eval      ilry09tch = 'T'
109300141029     c                   eval      ilry09aas = KAAS
109400141029     c                   eval      ilry09lnp = KLNP
109500141029     c                   eval      ilry09nrs = KNRS
109600141029     c                   eval      ilry09nsp = KNSP
109700141029     c                   eval      ilry09fgs = §fle
109800141029     c                   movel     fnlry09ds2    kpjbu
109900141029     c                   call      'FNLRY09R'
110000141029     c                   parm                    kpjba
110100141029     c                   movel     kpjbu         fnlry09ds2
110200141029     c                   if        olry09err = *blank and
110300141029     c                             olry09esdi = '1'
110400141029     c                   seton                                        28
110500141029     C                   MOVEL     ERR(37)       $MSG
110600141029     C                   GOTO      FINARR
110700141029     c                   endif
110800141029      *  altro
110900141029     c                   eval      ilry09tdis= 'E'
111000141029     c                   eval      ilry09tch = 'E'
111100141029     c                   movel     fnlry09ds2    kpjbu
111200141029     c                   call      'FNLRY09R'
111300141029     c                   parm                    kpjba
111400141029     c                   movel     kpjbu         fnlry09ds2
111500141029     c                   if        olry09err = *blank and
111600141029     c                             olry09esal = '1'
111700141029     c                   seton                                        28
111800141029     C                   MOVEL     ERR(36)       $MSG
111900141029     C                   GOTO      FINARR
112000141029     c                   endif
112100141029     c                   endif
112200940915     C*
112300940915     C     FINARR        ENDSR
112400940915     C*-----------------------------------------------------*
112500940915     C*  Controlli sulla giacenza
112600940915     C*-----------------------------------------------------*
112700940915     C     CTRGIA        BEGSR
112800050411     c                   clear                   tigcp000
112900940915     C*
113000050315     C     KSPE2         CHAIN(e)  tIGCP01L                           30
113100050315     c                   if        %error
113200070521     C     KSPE2         CHAIN(n)  tIGCP01L
113300050315     c                   exsr      geslck
113400050318     C     KSPE2         CHAIN(e)  tIGCP01L                           30
113500050315     c                   endif
113600170922      *per rekord trovato salvo il dato attuale per monitor avanzamento
113700170922     c  n30              eval      tigcpb = tigcp
113800940919     C* Se la giacenza esiste già e il programma è richiamato
113900940919     C* gestisco devo effettuare una riapertura
114000940915     C     *IN30         IFEQ      '0'
114100940919     C     D05GES        ANDEQ     'A'
114200960626     C                   Z-ADD     DATEU         G02INV
114300960626     C                   MOVEL     '3'           G02ERR
114400960626     C                   Z-ADD     0             G02DAT
114500960626     C                   CALL      'XSRDA8'
114600960626     C                   PARM                    WLBDA8
114700960626     C                   MOVE      G02DAT        V2CDUR
114800940920     C                   MOVEL     'S'           WRIAPE            1
114900940915     C                   END
115000940915     C* Se sono in partenza eseguo routine controlli partenza
115100940919     C     D05GES        IFEQ      'P'
115200940915     C                   EXSR      CTRPAR
115300940915     C                   ELSE
115400940915     C* Se sono in partenza eseguo routine controlli arrivo
115500940915     C                   EXSR      CTRARR
115600940920     C                   END
115700170530     C     *IN30         IFEQ      '0'
115800170530     C     D05GES        ANDEQ     'A'
115900170530     C     gcpfas        ANDlt     35
116000170530     c                   seton                                        28
116100170530     C                   MOVEL     ERR(40)       $MSG
116200170530     c                   endif
116300940915     C*  Se ci sono errori vado a fine controlli + accendo ind.
116400940915     C     *IN28         IFEQ      '1'
116500940915     C                   Z-ADD     D05AAS        V3CAAS
116600940915     C                   Z-ADD     D05LNP        V3CLNP
116700940915     C                   Z-ADD     D05NRS        V3CNRS
116800940915     C                   Z-ADD     D05NSP        V3CNSP
116900940915     C     *IN30         IFEQ      '0'
117000940915     C                   Z-ADD     GCPAGC        V3CAGC
117100940915     C                   Z-ADD     GCPFGC        V3CFGC
117200940915     C                   Z-ADD     GCPNGC        V3CNGC
117300940915     C                   END
117400940915     C                   EXFMT     LG05D03
117500940919     C                   MOVEL     'S'           WFINE
117600940915     C                   ELSE
117700940915     C                   EXSR      INZ02
117800940915     C                   END
117900920409     C*
118000940915     C                   ENDSR
118100940920     C*-----------------------------------------------------*
118200940920     C*  Routine per gestione richiamo interrogazione bolle
118300940920     C*-----------------------------------------------------*
118400940920     C     RICHBL        BEGSR
118500940920     C*
118600161114     C**   D05GES        IFEQ      'P'
118700161114     C**                 Z-ADD     V1CAAS        PA0AAS
118800161114     C**                 Z-ADD     V1CLNP        PA0LNP
118900161114     C**                 Z-ADD     V1CNRS        PA0NRS
119000161114     C**                 Z-ADD     V1CNSP        PA0NSP
119100161114     C**                 MOVEL     ' '           PA0F03
119200161114     C**                 MOVEL     ' '           PA0FLG
119300161114     C**                 MOVEL     PARAM0        KPJBU
119400161114     C**   PA0NSP        IFGT      0
119500161114     C**                 MOVEL     '1'           PA0FLG
119600161114     C**                 CALL      'FNLS05R'
119700161114     C**                 PARM                    KPJBA
119800161114     C**                 ELSE
119900161114     C**                 CALL      'FNLS04R'
120000161114     C**                 PARM                    KPJBA
120100161114     C**                 END
120200161114     C**                 MOVEL     KPJBU         PARAM0
120300161114     C**   PA0F03        IFNE      *BLANKS
120400161114     C**                 Z-ADD     PA0AAS        V1CAAS
120500161114     C**                 Z-ADD     PA0LNP        V1CLNP
120600161114     C**                 Z-ADD     PA0NRS        V1CNRS
120700161114     C**                 Z-ADD     PA0NSP        V1CNSP
120800161114     C**                 END
120900161114     C**                 ELSE
121000161114     C**                 Z-ADD     V1CAAS        PA1AAS
121100161114     C**                 Z-ADD     V1CLNP        PA1LNP
121200161114     C**                 Z-ADD     V1CNRS        PA1NRS
121300161114     C**                 Z-ADD     V1CNSP        PA1NSP
121400161114     C**                 Z-ADD     V1CNSP        PA1NSP
121500161114     C**                 MOVEL     ' '           PA1F03
121600161114     C**                 MOVEL     '2'           PA1FLG
121700161114     C**   PA1NSP        IFGT      0
121800161114     C**                 MOVEL     '1'           PA1FLG
121900161114     C**                 END
122000161114     C**                 MOVEL     PARAM1        KPJBU
122100161114     C**                 CALL      'FNLR36R'
122200161114     C**                 PARM                    KPJBA
122300161114     C**                 MOVEL     KPJBU         PARAM1
122400161114     C**   PA1F03        IFNE      '1'
122500161114     C**                 Z-ADD     PA1AAS        V1CAAS
122600161114     C**                 Z-ADD     PA1LNP        V1CLNP
122700161114     C**                 Z-ADD     PA1NRS        V1CNRS
122800161114     C**                 Z-ADD     PA1NSP        V1CNSP
122900161114     C**                 END
123000161114     C**                 END
123100161114     C     V1CNSP        IFGT      0
123200161114     C                   CLEAR                   paramu6ds1
123300161114     C                   Z-ADD     V1CAAS        pa1AAS_
123400161114     C                   Z-ADD     V1CLNP        Pa1LNP_
123500161114     C                   Z-ADD     V1CNRS        Pa1NRS_
123600161114     C                   Z-ADD     V1CNSP        Pa1NSP_
123700161118     C                   eval      PA1tla_='L'
123800161114     C                   MOVEL     paramu6ds1    KPJBU
123900161114     C                   CALL      'FNLRU6R1'
124000161114     C                   PARM                    KPJBA
124100161124     C                   eval      paramu6ds1=KPJBU
124200161124     c                   if        pa1err_ ='1'
124300161124     c                   seton                                        28
124400161124     c                   movel     err(6)        $msg
124500161124     c                   endif
124600161114     C*
124700161114     C                   ELSE
124800161114     C*
124900161114     C                   CLEAR                   PARAmu6ds
125000161118     c                   clear                   fnlru6ds
125100161114     C                   MOVEL     '2'           Pa1FLG
125200161114     C                   MOVEL     PARAmu6ds     KPJBU
125300161118     c                   if        d05ges='P'
125400161118     c                   eval      ilru6lp1=§fle
125500161118     c                   else
125600161118     c                   eval      ilru6lna=§fle
125700161118     c                   endif
125800161114     C                   CALL      'FNLRU6R'
125900161114     C                   PARM                    KPJBA
126000161118     c                   parm                    fnlru6ds
126100161114     C                   MOVEL     KPJBU         PARAMu6ds
126200161114     C     Pa1F03        IFEQ      *BLANKS
126300161114     C                   Z-ADD     Pa1AAS        V1CAAS
126400161114     C                   Z-ADD     Pa1LNP        V1CLNP
126500161114     C                   Z-ADD     Pa1NRS        V1CNRS
126600161114     C                   Z-ADD     Pa1NSP        V1CNSP
126700161114     C                   ENDIF
126800161114     C                   ENDIF
126900940920     C*
127000940920     C                   ENDSR
127100070705     C**************************************************************************
127200070705     C*  Routine per gestione richiamo interrogazione bolle arrivo
127300070705     C**************************************************************************
127400070705     C     richbla       begsr
127500070705      *
127600070705     c                   movel     kpjbu         §kpjbu
127700070705      *
127800161114     c*                  z-add     v2caas        pa1aas
127900161114     c*                  z-add     v2clnp        pa1lnp
128000161114     c*                  z-add     v2cnrs        pa1nrs
128100161114     c*                  z-add     v2cnsp        pa1nsp
128200161114     c*                  movel     ' '           pa1f03
128300161114     c*                  movel     '1'           pa1flg
128400161114     c*                  movel     param1        kpjbu
128500161114     c*                  call      'FNLR36R'
128600161114     c*                  parm                    kpjba
128700161114     c*                  movel     kpjbu         param1
128800161114     c                   clear                   paramu6ds1
128900161114     C                   Z-ADD     V2cAAS        Pa1AAS_
129000161114     C                   Z-ADD     V2cLNP        Pa1LNP_
129100161114     C                   Z-ADD     V2cNRS        Pa1NRS_
129200161114     C                   Z-ADD     V2cNSP        Pa1NSP_
129300161118     C                   eval      PA1tla_='L'
129400161114     C                   MOVEL     paramu6ds1    KPJBU
129500161114     C                   CALL      'FNLRU6R1'
129600161114     C                   PARM                    KPJBA
129700070705      *
129800070705     c                   movel     §kpjbu        kpjbu
129900070705      *
130000070705     c                   endsr
130100090701     C**************************************************************************
130200090701     C*  Routine per gestione BLOCCO SBLOCCO SPEDIZIONE
130300090701     C**************************************************************************
130400090701     C     richblosbl    begsr
130500090701     c                   movel     kpjbu         §kpjbu
130600090709     c                   unlock    fnarb01l
130700090709     c                   if        arbfbc = 'A'
130800090709     c                   clear                   fnlrg2ds
130900090709     C                   Z-ADD     kaas          ILRG2AAS
131000090709     C                   Z-ADD     klnp          ILRG2LNP
131100090709     C                   Z-ADD     knrs          ILRG2NRS
131200090709     C                   Z-ADD     knsp          ILRG2NSP
131300090709     C                   MOVE      'L'           ILRG2CAU
131400090709     C                   CALL      'FNLRG2R'
131500090709     C                   PARM                    KPJBA
131600090709     c                   parm                    fnlrg2ds
131700090709     c                   else
131800090701     c                   clear                   dslr02
131900090701     C                   Z-ADD     kaas          DR2AAS
132000090701     C                   Z-ADD     klnp          DR2LNP
132100090701     C                   Z-ADD     knrs          DR2NRS
132200090701     C                   Z-ADD     knsp          DR2NSP
132300090701     C                   MOVEL     '1'           DR2RIC
132400090701     C                   Z-ADD     arblna        DR2FGS
132500090701     C                   MOVEL     DSLR02        KPJBU
132600090701     C                   CALL      'FNLR39R'
132700090701     C                   PARM                    KPJBA
132800090701     c                   movel     kpjbu         dslr02
132900090709     c                   endif
133000090707     c                   movel     §kpjbu        kpjbu
133100090701     c     kspe1         chain     fnarb01l
133200090701      *
133300090701     C                   ENDSR
133400900418     C*---------------------------------------------------------------*
133500900418     C*   P U L V I D                                                 *
133600900418     C*---------------------------------------------------------------*
133700900418     C     PULVID        BEGSR
133800950224     C*
133900961001     C                   SETOFF                                       33
134000950104     C                   CLEAR                   DS7Q
134100950104     C                   MOVEL     ' '           WTPGIA
134200941223     C                   MOVEL     ' '           WRIAPE            1
134300940915     C                   MOVEL     *ZEROS        V1CNRS
134400940915     C                   MOVEL     *ZEROS        V1CNSP
134500940920     C                   MOVE      WHHDAT        V1CAAS
134600940915     C                   MOVEL     *ZEROS        V1CLNP
134700940915     C                   MOVEL     *ZEROS        V2CAAS
134800940915     C                   MOVEL     *ZEROS        V2CLNP
134900940915     C                   MOVEL     *ZEROS        V2CNRS
135000940915     C                   MOVEL     *ZEROS        V2CNSP
135100960123     C                   MOVEL     *ZEROS        V4CNSP
135200960123     C                   MOVEL     *ZEROS        V4CAGC
135300960123     C                   MOVEL     *ZEROS        V4CFGC
135400960123     C                   MOVEL     *ZEROS        V4CNGC
135500940915     C                   MOVEL     *ZEROS        V2CDSP
135600940915     C                   MOVEL     *BLANK        V2CTBL
135700940915     C                   MOVEL     *BLANK        V2DTBL
135800940915     C                   MOVEL     *ZEROS        V2CLNA
135900940915     C                   MOVEL     *BLANK        V2DLNA
136000940919     C                   MOVEL     *BLANK        V2DNAS
136100940915     C                   MOVEL     *BLANK        V2CNOT
136200940915     C                   MOVEL     *BLANK        V2CNO2
136300991119     C                   MOVEL     *BLANK        V2CDIV
136400940915     C                   Z-ADD     *ZEROS        V2CIFT
136500940915     C                   Z-ADD     *ZEROS        V2CNCL
136600940915     C                   Z-ADD     *ZEROS        V2CVLF
136700940915     C                   Z-ADD     *ZEROS        V2CPKF
136800940915     C                   Z-ADD     *ZEROS        V2CCAS
136900940915     C                   MOVEL     *BLANK        V2CRSD
137000940915     C                   MOVEL     *BLANK        V2CVCA
137100940915     C                   MOVEL     *BLANK        V2CDE2
137200940915     C                   MOVEL     *BLANK        V2CIND
137300940915     C                   MOVEL     *BLANK        V2CLOD
137400940915     C                   MOVEL     *BLANK        V2CPRD
137500940915     C                   MOVEL     *ZEROS        V2CCAD
137600940915     C                   MOVEL     *ZEROS        V2CNZD
137700960123     C                   MOVEL     *BLANK        V4CRSD
137800960123     C                   MOVEL     *BLANK        V4CDE2
137900960123     C                   MOVEL     *BLANK        V4CIND
138000960123     C                   MOVEL     *BLANK        V4CLOD
138100960123     C                   MOVEL     *BLANK        V4CPRD
138200960123     C                   MOVEL     *ZEROS        V4CCAD
138300960123     C                   MOVEL     *BLANK        V2CVCA
138400960123     C                   MOVEL     *ZEROS        V2CNZD
138500940919     C                   MOVEL     *ZEROS        V2CNZM
138600940915     C                   MOVEL     *ZEROS        V2CSCM
138700940915     C                   MOVEL     *ZEROS        V2CRMN
138800940915     C                   MOVEL     *BLANK        V2CRMA
138900940915     C                   MOVEL     *BLANK        V2CRSM
139000940915     C                   MOVEL     *BLANK        V2CINM
139100940915     C                   MOVEL     *BLANK        V2CLOM
139200940915     C                   MOVEL     *BLANK        V2CPRM
139300940915     C                   MOVEL     *ZEROS        V2CCAM
139400940919     C                   MOVEL     *BLANKS       V2CTCM
139500940919     C                   MOVEL     *BLANKS       V2DTCM
139600940915     C                   MOVEL     *BLANK        V2CCMC
139700940915     C                   MOVEL     *BLANK        V2DCMC
139800940915     C                   MOVEL     *BLANK        V2CDMC
139900940915     C                   MOVEL     *ZEROS        V2CCCR
140000940915     C                   MOVEL     *BLANK        V2DCCR
140100940915     C                   MOVEL     *ZEROS        V2CNRC
140200940915     C                   MOVEL     *ZEROS        V2CDXD
140300940915     C                   MOVEL     *ZEROS        V2CDTG
140400940915     C                   MOVEL     *ZEROS        V2CDUR
140500940915     C                   MOVEL     *ZEROS        V2CNGC
140600940915     C                   MOVEL     *ZEROS        V2CFGC
140700940915     C                   MOVEL     *BLANKS       V2DFGC
140800940920     C                   MOVEL     'N'           V2CFST
140900940920     C                   MOVEL     *BLANKS       V2CMSP
141000940920     C                   MOVEL     *BLANKS       V2CMSD
141100950104     C                   MOVEL     *BLANKS       V2CEVE
141200950104     C                   MOVEL     *BLANKS       WFEL              3 0
141300080604     c                   clear                   contrdest         1
141400940915     C*
141500900418     C                   ENDSR
141600940915     C*---------------------------------------------------------------*
141700940915     C*   G E N V I D                                                 *
141800940915     C*---------------------------------------------------------------*
141900940915     C     GENVID        BEGSR
142000940915     C*
142100941003     C                   MOVEL     ' '           V2CRPG
142200940921     C                   Z-ADD     KAAS          V2CAAS
142300940921     C                   Z-ADD     KLNP          V2CLNP
142400940921     C                   Z-ADD     KNRS          V2CNRS
142500940921     C                   Z-ADD     KNSP          V2CNSP
142600960123     C                   Z-ADD     KAAS          V4CAAS
142700960123     C                   Z-ADD     KLNP          V4CLNP
142800960123     C                   Z-ADD     KNRS          V4CNRS
142900960123     C                   Z-ADD     KNSP          V4CNSP
143000960123     C                   Z-ADD     KAAS          V4CAAS
143100960123     C                   Z-ADD     KLNP          V4CLNP
143200960123     C                   Z-ADD     KNRS          V4CNRS
143300960123     C                   Z-ADD     KNSP          V4CNSP
143400940915     C* Decodifico linea di arrivo
143500940915     C                   MOVEL     V2CLNA        KFIL
143600940915     C                   MOVEL     *ALL'?'       V2DLNA
143700940915     C     KFIL          CHAIN     AZORG01L                           31
143800940915     C     *IN31         IFEQ      '0'
143900940915     C     ORGFVA        ANDNE     '*'
144000940920     C                   MOVEL     *BLANKS       V2DLNA
144100940915     C                   MOVEL     ORGDES        V2DLNA
144200940915     C                   END
144300940915     C* Decodifico codice mancata consegna
144400950111     C                   MOVEL     *BLANKS       V2DCMC
144500940915     C     V2CCMC        IFNE      *BLANK
144600940915     C                   Z-ADD     1             X
144700940915     C     V2CCMC        LOOKUP    C2A(X)                                 32
144800940920     C     *IN32         IFEQ      '1'
144900950105     C     F2A(X)        ANDEQ     ' '
145000940920     C                   MOVEL     D2A(X)        V2DCMC
145100940919     C                   END
145200940920     C                   END
145300940919     C* Decodifico luogo deposito merce
145400940919     C                   MOVEL     *ALL'?'       V2DCCR
145500940919     C                   Z-ADD     1             X
145600940919     C     V2CCCR        LOOKUP    C2B(X)                                 32
145700940919     C     *IN32         IFEQ      '1'
145800940920     C                   MOVEL     *BLANKS       V2DCCR
145900940919     C                   MOVEL     D2B(X)        V2DCCR
146000940919     C                   ELSE
146100940919     C* Se non lo trovo in tab. corrispon. lo cerco nell'organigramma
146200940919     C                   MOVE      V2CCCR        KFIL
146300940919     C     KFIL          CHAIN     AZORG01L                           31
146400940919     C     *IN31         IFEQ      '0'
146500940919     C     ORGFVA        ANDEQ     ' '
146600940919     C                   MOVEL     *BLANKS       V2DCCR
146700940919     C                   MOVEL     ORGDES        V2DCCR
146800940919     C                   END
146900940919     C                   END
147000980112     C*  Controllo se lnp della giacenza è estera
147100980112     C                   MOVEL     *BLANKS       ORGFL1
147200980112     C                   MOVE      V2CLNP        KFIL
147300980112     C     KFIL          CHAIN     AZORG01L                           31
147400020729     C                   MOVEL     *blank        LNPFL1            1
147500020729     C                   clear                   OG143
147600020729     C  n31              MOVEL     ORGDE3        OG143
147700020729     C  N31§OgNTW        ifeq      'FED'
147800020729     C     §OgNTW        oreq      'EEX'
147900020729     C     §OgNTW        oreq      'EUP'
148000020729     C                   MOVEL     'E'           LNPFL1
148100020729     c                   end
148200040310     C                   clear                   wntwlnp           3
148300040310     C  N31§OgNTW        ifeq      'DPD'
148400040310     C                   MOVEL     §ogntw        wntwlnp
148500040310     c                   end
148600940919     C* Mi salvo i dati del mittente per scrivere o no il
148700940919     C* file d'appoggio
148800940919     C                   MOVEL     V2CSCM        SAVSCM
148900940919     C                   MOVEL     V2CRSM        SAVRSM
149000940919     C                   MOVEL     V2CINM        SAVINM
149100940919     C                   MOVEL     V2CLOM        SAVLOM
149200940919     C                   MOVEL     V2CPRM        SAVPRM
149300940919     C                   MOVEL     V2CCAM        SAVCAM
149400940921     C                   MOVEL     V2CNZM        SAVNZM
149500940919     C* Prelevo comunque num. giacenza anche se la lnp non è meccaniz.
149600940919     C     *IN30         IFEQ      '1'
149700940920     C                   MOVEL     DATEU         KAAA
149800940919     C                   MOVE      ARBLNA        KFGC
149900940919     C     KNUF          CHAIN     FLNUF01L                           31
150000940919     C     *IN31         IFEQ      '0'
150100940919     C                   ADD       1             NUFNUM
150200940919     C                   Z-ADD     NUFNUM        GCPNGC
150300940919     C                   Z-ADD     NUFNUM        V2CNGC
150400940919     C                   EXCEPT    AGGNUF
150500940919     C                   ELSE
150600940919     C* Se non trovato numeratore lo creo
150700940919     C                   Z-ADD     KAAA          NUFAAA
150800940919     C                   Z-ADD     2             NUFCNU
150900940920     C                   Z-ADD     V2CLNA        NUFFIL
151000940919     C                   Z-ADD     DATEU         NUFDAT
151100940919     C                   Z-ADD     0             NUFRIN
151200940919     C                   Z-ADD     0             NUFRFI
151300940919     C                   Z-ADD     1             NUFNUM
151400940919     C                   Z-ADD     NUFNUM        GCPNGC
151500940919     C                   Z-ADD     NUFNUM        V2CNGC
151600940919     C                   WRITE     FLNUF
151700940919     C                   END
151800940919     C* Visualizzo data apertura e filiale
151900940920     C                   MOVE      WHHDAT        V2CDTG
152000940919     C                   END
152100940920     C* Controllo se devo gestire forzatura da apertura
152200950105     C                   SETOFF                                       04
152300940920     C     WFORZA        IFEQ      'S'
152400940920     C                   SETON                                        04
152500940920     C                   END
152600940919     C*
152700940919     C                   ENDSR
152800900418     C*---------------------------------------------------------------*
152900900418     C*   B L P V I D                                                 *
153000900418     C*---------------------------------------------------------------*
153100900418     C     BLPVID        BEGSR
153200940915     C*
153300941003     C                   MOVE      BLPGGA        V2CGGA
153400941003     C                   MOVE      BLPMGS        AMGDSP
153500940919     C                   MOVEL     BLPAAS        AMGDSP
153600940915     C                   MOVE      BLPMGS        WGG
153700940915     C                   MOVEL     BLPMGS        WMM
153800940915     C                   MOVEL     WGGMM         V2CDSP
153900940915     C                   MOVE      BLPAAS        V2CDSP
154000940919     C                   MOVEL     BLPCBO        V2CTBL
154100940915     C                   MOVEL     BLPFST        V2CFST
154200940915     C                   MOVEL     BLPLNA        V2CLNA
154300940919     C                   MOVEL     BLPNAS        V2DNAS
154400940915     C                   Z-ADD     BLPNCL        V2CNCL
154500940915     C                   Z-ADD     BLPVLF        V2CVLF
154600940915     C                   Z-ADD     BLPPKF        V2CPKF
154700940920     C                   MOVE      BLPLNA        V2CCCR
154800940915     C* Tipo spedizione
154900940915     C                   MOVEL     *BLANKS       V2DTSP
155000090520     c                   z-add     1             x
155100090520     C     blptsp        LOOKUP    C5e(X)                                 32
155200090520     C     *IN32         IFEQ      '1'
155300090520     C                   MOVEL     D5e(X)        V2DTsp
155400090520     C                   END
155500940915     C* Decodifico Tipo bolla
155600940915     C                   MOVEL     *BLANKS       V2DTBL
155700940915     C                   MOVEL     *BLANKS       TIPBOL            1
155800950102     C                   Z-ADD     1             X
155900940919     C     V2CTBL        LOOKUP    C3A(X)                                 32
156000940915     C     *IN32         IFEQ      '1'
156100940915     C                   MOVEL     D3A(X)        V2DTBL
156200940919     C                   MOVEL     T3A(X)        TIPBOL            1
156300021031     C                   MOVE      g3A(X)        giacen            1
156400940915     C                   END
156500940919     C* Reperisco importi fatture e C/Assegno
156600990929     C* Se prima bolla è porto Franco aggancio dati tassaz.
156700990929     C* da tipo record 1
156800991001     C     TIPBOL        IFEQ      'F'
156900990929     C                   MOVEL     '1'           KTRC
157000990929     C     KAR6          CHAIN     FIAR601L                           31
157100941223     C     *IN31         IFEQ      '0'
157200990929     C                   Z-ADD     AR6IFT        V2CIFT
157300991119     C                   MOVEL     AR6DIV        V2CDIV
157400941223     C                   END
157500990929     C                   END
157600051116     C     KSPE1         CHAIN     Fiar901L                           31
157700941223     C     *IN31         IFEQ      '0'
157800051116     C                   Z-ADD     ar9CAS        V2CCAS
157900051116     C                   MOVEL     ar9VCA        V2CVCA
158000941223     C                   END
158100960123     C* Controllo se esiste estensione descrizione destinatario
158200040325     C                   MOVEL     'D'           KTRC
158300060213     C                   MOVEL     *BLANKS       ar4NOT
158400060213     C     KAR4          CHAIN(n)  Fiar401L                           31
158500960123     C* Controllo se esiste destinatario finale
158600960123     C                   SETOFF                                       13
158700960123     C                   CLEAR                   FNLSN0
158800960123     C                   Z-ADD     BLPAAS        DN0AAS
158900960123     C                   Z-ADD     BLPLNP        DN0LNP
159000960123     C                   Z-ADD     BLPNRS        DN0NRS
159100960123     C                   Z-ADD     BLPNSP        DN0NSP
159200960123     C                   MOVEL     *BLANKS       DN0TRC
159300960123     C                   MOVEL     KPJBU         WSVKPJ          256
159400960123     C                   MOVEL     FNLSN0        KPJBU
159500960123     C                   CALL      'FNLSN0R'
159600960123     C                   PARM                    KPJBA
159700960123     C                   MOVEL     KPJBU         FNLSN0
159800960123     C                   MOVEL     WSVKPJ        KPJBU
159900960123     C* Destinatario
160000960123     C     DN0TRV        IFEQ      'S'
160100960123     C                   SETON                                        13
160200960123     C                   MOVEL     DN0RSD        V2CRSD
160300960123     C                   MOVEL     DN0RS2        V2CDE2
160400960123     C                   MOVEL     DN0IND        V2CIND
160500960123     C                   MOVEL     DN0LOD        V2CLOD
160600960123     C                   MOVEL     DN0PRD        V2CPRD
160700960123     C                   MOVEL     DN0CAD        V2CCAD
160800960123     C                   MOVEL     DN0NZD        V2CNZD
160900960123     C                   MOVEL     BLPRSD        V4CRSD
161000060213     C                   MOVEL     ar4NOT        V4CDE2
161100960123     C                   MOVEL     BLPIND        V4CIND
161200960123     C                   MOVEL     BLPLOD        V4CLOD
161300960123     C                   MOVEL     BLPPRD        V4CPRD
161400960123     C                   MOVEL     BLPCAD        V4CCAD
161500960123     C                   MOVEL     BLPNZD        V4CNZD
161600960123     C                   ELSE
161700960123     C                   MOVEL     BLPRSD        V2CRSD
161800960123     C                   MOVEL     BLPIND        V2CIND
161900960123     C                   MOVEL     BLPLOD        V2CLOD
162000960123     C                   MOVEL     BLPPRD        V2CPRD
162100960123     C                   MOVEL     BLPCAD        V2CCAD
162200960123     C                   MOVEL     BLPNZD        V2CNZD
162300060213     C                   MOVEL     ar4NOT        V2CDE2
162400960123     C                   END
162500940915     C* Mittente
162600940919     C                   MOVEL     BLPRMA        V2CRMA
162700940919     C                   MOVEL     BLPRMN        V2CRMN
162800940919     C                   MOVEL     BLPRSM        V2CRSM
162900940919     C                   MOVEL     BLPINM        V2CINM
163000940919     C                   MOVEL     BLPLOM        V2CLOM
163100940919     C                   MOVEL     BLPPRM        V2CPRM
163200940919     C                   MOVEL     BLPCAM        V2CCAM
163300940919     C                   MOVEL     BLPNZM        V2CNZM
163400091102     c*verifica se forzato in arrivo per triangolazione nel caso non forza
163500091102     c* il ccm
163600900508     C     BLPCCM        IFNE      *ZEROS
163700940915     C                   MOVEL     BLPCCM        V2CSCM
163800940920     C                   Z-ADD     BLPCCM        KKSC
163900900508     C                   ELSE
164000940915     C                   MOVEL     BLPKSC        V2CSCM
164100940920     C                   Z-ADD     BLPKSC        KKSC
164200900508     C                   END
164300091102     c                   exsr      forzacli
164400920326     C*
164500940915     C                   MOVEL     'N'           WCLIGN            1
164600940915     C                   MOVEL     V2CSCM        §RIC              3
164700920326     C                   Z-ADD     1             I                 2 0
164800940915     C* Note su bolla
164900940915     C                   MOVEL     '8'           KTRC
165000060213     C                   MOVEL     *BLANKS       ar4NOT
165100060213     C     KAR4          CHAIN(n)  Fiar401L                           31
165200060213     C                   MOVEL     ar4NOT        V2CNOT
165300940915     C                   MOVEL     '9'           KTRC
165400060213     C                   MOVEL     *BLANKS       ar4NOT
165500060213     C     KAR4          CHAIN(n)  Fiar401L                           31
165600060213     C                   MOVEL     ar4NOT        V2CNO2
165700940920     C* Carico messaggio particolarità
165800940920     C                   MOVEL     'N'           WFORZA            1
165900940920     C     BLPGGA        IFNE      *BLANKS
166000940920     C                   Z-ADD     1             X
166100940920     C     BLPGGA        LOOKUP    C7Q(X)                                 32
166200940920     C     *IN32         IFEQ      '1'
166300940920     C                   MOVE      D7Q(X)        DS7Q
166400940920     C     §7Q3MF        IFEQ      'A'
166500940920     C     §7Q3MS        ANDNE     *BLANKS
166600940920     C     §7Q3MQ        IFEQ      'P'
166700950105     C                   MOVEL     'N'           WTREVE
166800950105     C                   MOVEL     §7Q3EV        V2CEVE
166900950105     C                   MOVEL     'N'           WFORZA
167000950105     C     §7Q3EV        IFNE      *BLANKS
167100950105     C                   EXSR      CTREVE
167200950105     C                   END
167300950105     C     WTREVE        IFEQ      'N'
167400950104     C                   MOVEL     §7Q3MS        V2CMSP
167500940920     C                   MOVEL     §7Q3MC        WFORZA
167600950104     C                   MOVEL     V2CLNP        WFLE
167700950105     C                   SETON                                        05
167800950105     C                   END
167900940920     C                   ELSE
168000940920     C                   SETON                                        06
168100950104     C                   MOVEL     §7Q3MS        V2CMSD
168200940920     C                   END
168300940920     C                   END
168400940920     C                   END
168500940920     C                   END
168600020222     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" SDE CLIENTE
168700020222     C* CODIFICATO
168800020222     C                   MOVE      KKSC          APPO              4 0
168900020222     C                   CLEAR                   TRUL27
169000020222     C     APPO          IFNE      8888
169100020222     C     APPO          ANDNE     9999
169200020222     C     APPO          ANDNE     *ZEROS
169300020306     C                   MOVEL     D05TRI        I27TLA
169400020222     C                   MOVEL     BLPTSP        I27TSP
169500020222     C                   MOVE      BLPLNA        I27LNA
169600020222     C                   MOVE      BLPLNP        I27LNP
169700021118     C                   MOVE      BLPctr        I27ctb
169800020222     C                   MOVE      KKSC          I27CLI
169900160316     C********           MOVE      AMGDSP        I27DTA
170000160316     C                   CALL      'TRUL27R1'
170100020222     C                   PARM                    TRUL27
170200020222     C                   END
170300940915     C*
170400021031     C                   ENDSR
170500911011     C*---------------------------------------------------------------*
170600911011     C*   A R B V I D                                                 *
170700911011     C*---------------------------------------------------------------*
170800911011     C     ARBVID        BEGSR
170900941003     C*
171000950712     C     D05RIC        IFEQ      'S'
171100950414     C                   MOVEL     'N'           WWTR              1
171200950712     C                   ELSE
171300950712     C                   MOVEL     'S'           WWTR              1
171400950712     C                   END
171500941003     C                   MOVE      ARBGGA        V2CGGA
171600940919     C                   MOVE      ARBMGS        AMGDSP
171700940919     C                   MOVEL     ARBAAS        AMGDSP
171800940915     C                   MOVE      ARBMGS        WGG
171900940915     C                   MOVEL     ARBMGS        WMM
172000940915     C                   MOVEL     WGGMM         V2CDSP
172100940915     C                   MOVE      ARBAAS        V2CDSP
172200940919     C                   MOVEL     ARBCBO        V2CTBL
172300940915     C                   MOVEL     ARBLNA        V2CLNA
172400940919     C                   MOVEL     ARBNAS        V2DNAS
172500940915     C                   Z-ADD     ARBNCL        V2CNCL
172600940915     C                   Z-ADD     ARBVLF        V2CVLF
172700940915     C                   Z-ADD     ARBPKF        V2CPKF
172800940920     C                   MOVE      ARBLNA        V2CCCR
172900940919     C* Tipo spedizione
173000940915     C                   MOVEL     *BLANKS       V2DTSP
173100090520     c                   z-add     1             x
173200090520     C     arbtsp        LOOKUP    C5e(X)                                 32
173300090520     C     *IN32         IFEQ      '1'
173400090520     C                   MOVEL     D5e(X)        V2DTsp
173500090520     C                   END
173600090520     c*
173700950224     C     ARBFFD        IFEQ      'S'
173800950224     C     D05CMC        IFEQ      *BLANKS
173900950224     C     D05AAF        ANDNE     *ZEROS
174000950224     C     ARBCMC        OREQ      *BLANKS
174100950224     C     D05AAF        ANDEQ     *ZEROS
174200940922     C                   MOVE      9999999       GCPNRC
174300940922     C                   MOVE      9999999       V2CNRC
174400940922     C                   END
174500950224     C                   END
174600940915     C* Decodifico Tipo bolla
174700940915     C                   Z-ADD     1             X
174800940915     C                   MOVEL     *BLANKS       V2DTBL
174900940919     C     V2CTBL        LOOKUP    C3A(X)                                 32
175000940915     C     *IN32         IFEQ      '1'
175100940915     C                   MOVEL     D3A(X)        V2DTBL
175200990929     C                   MOVEL     T3A(X)        TIPBOL
175300021031     C                   MOVE      g3A(X)        giacen            1
175400940915     C                   END
175500940919     C* Reperisco importi FAttura e C/Assegno
175600990929     C     TIPBOL        IFEQ      'A'
175700990929     C                   MOVEL     '1'           KTRC
175800990929     C     KAR6          CHAIN     FIAR601L                           31
175900941223     C     *IN31         IFEQ      '0'
176000940915     C                   Z-ADD     AR6IFT        V2CIFT
176100991119     C                   MOVEL     AR6DIV        V2CDIV
176200941223     C                   END
176300990929     C                   END
176400051116     C     KSPE1         CHAIN     FIAR901L                           31
176500941223     C     *IN31         IFEQ      '0'
176600940915     C                   Z-ADD     AR9CAS        V2CCAS
176700940915     C                   MOVEL     AR9VCA        V2CVCA
176800941223     C                   END
176900940915     C* Destinatario
177000940915     C                   MOVEL     ARBRSD        V2CRSD
177100940915     C                   MOVEL     ARBIND        V2CIND
177200940915     C                   MOVEL     ARBLOD        V2CLOD
177300940915     C                   MOVEL     ARBPRD        V2CPRD
177400940915     C                   MOVEL     ARBCAD        V2CCAD
177500940915     C                   MOVEL     ARBNZD        V2CNZD
177600940915     C* Controllo se esiste estensione descrizione destinatario
177700040325     C                   MOVEL     'D'           KTRC
177800940915     C                   MOVEL     *BLANKS       AR4NOT
177900060213     C     KAR4          CHAIN(n)  FiAR401L                           31
178000941223     C                   MOVEL     AR4NOT        V2CDE2
178100940915     C* Mittente
178200911011     C     ARBCCM        IFNE      *ZEROS
178300940915     C                   MOVEL     ARBCCM        V2CSCM
178400911011     C                   ELSE
178500940915     C                   MOVEL     ARBKSC        V2CSCM
178600911011     C                   END
178700091021     c*forzatura SCM in caso di triangolazione ORM se cliente particolare
178800091021     c* presente in tabella CLI
178900091021     c                   exsr      forzacli
179000091021      *
179100940915     C                   MOVEL     ARBRMN        V2CRMN
179200940915     C                   MOVEL     ARBRMA        V2CRMA
179300940915     C                   MOVEL     ARBRSM        V2CRSM
179400940915     C                   MOVEL     ARBINM        V2CINM
179500940915     C                   MOVEL     ARBLOM        V2CLOM
179600940915     C                   MOVEL     ARBPRM        V2CPRM
179700940915     C                   MOVEL     ARBCAM        V2CCAM
179800940915     C                   MOVEL     ARBNZM        V2CNZM
179900940919     C* Preimposto codice riapertura che c'è su bolla
180000940915     C                   MOVEL     ARBCMC        V2CCMC
180100950111     C     ARBFFD        IFEQ      'S'
180200950224     C     D05CMC        IFEQ      *BLANKS
180300950224     C     D05AAF        ANDNE     *ZEROS
180400950224     C     ARBCMC        OREQ      *BLANKS
180500950224     C     D05AAF        ANDEQ     *ZEROS
180600950111     C                   MOVE      '032'         V2CCMC
180700950414     C* Se fermo deposito devo scrivere evento
180800950414     C                   MOVEL     'S'           WWTR
180900950111     C                   END
181000950224     C                   END
181100950111     C*
181200950102     C     D05CMC        IFNE      *BLANKS
181300950102     C                   MOVEL     D05CMC        V2CCMC
181400950102     C                   END
181500050217     C* Se l.avviso devo scrivere evento
181600950414     C     WWTR          IFEQ      'N'
181700950414     C                   Z-ADD     1             X
181800950414     C     V2CCMC        LOOKUP    C2A(X)                                 32
181900950414     C     *IN32         IFEQ      '1'
182000950414     C     T2A(X)        ANDEQ     'A'
182100950414     C                   MOVEL     'S'           WWTR
182200950414     C                   END
182300950414     C                   END
182400940915     C* Data comunicazione interna = data dist. pad. se in arrivo
182500941223     C                   Z-ADD     WOGGI         G02INV
182600940915     C     ARBDDC        IFGT      0
182700940919     C                   Z-ADD     ARBDDC        G02INV
182800941223     C                   END
182900950613     C*
183000941223     C                   Z-ADD     0             G02DAT
183100940915     C                   MOVEL     '3'           G02ERR
183200940919     C                   CALL      'XSRDA8'
183300940915     C                   PARM                    WLBDA8
183400940915     C                   MOVE      G02DAT        V2CDXD
183500950613     C*
183600950613     C     D05DXD        IFNE      *ZEROS
183700950613     C                   MOVE      D05DXD        V2CDXD
183800950613     C                   END
183900940915     C* Note su bolla
184000940915     C                   MOVEL     '8'           KTRC
184100940915     C                   MOVEL     *BLANKS       AR4NOT
184200060213     C     KAR4          CHAIN(n)  FiAR401L                           31
184300941223     C                   MOVEL     AR4NOT        V2CNOT
184400940915     C                   MOVEL     '9'           KTRC
184500940915     C                   MOVEL     *BLANKS       AR4NOT
184600060213     C     KAR4          CHAIN(n)  FiAR401L                           31
184700940915     C                   MOVEL     AR4NOT        V2CNO2
184800040310      *
184900040310     C                   MOVEL     'A'           KTRC
185000040310     C                   MOVEL     *BLANKS       AR4NOT
185100060213     C     KAR4          CHAIN(n)  FiAR401L                           31
185200040310     C  n31              MOVEL     AR4NOT        dsbl4a
185300040310     C   31              clear                   dsbl4a
185400940920     C* Carico messaggio particolarità
185500940920     C                   MOVEL     'N'           WFORZA
185600940920     C     ARBGGA        IFNE      *BLANKS
185700940920     C                   Z-ADD     1             X
185800940920     C     ARBGGA        LOOKUP    C7Q(X)                                 32
185900940920     C     *IN32         IFEQ      '1'
186000940920     C                   MOVE      D7Q(X)        DS7Q
186100021031     C                   Z-ADD     1             X
186200021031     C     arbGGA        LOOKUP    C7W(X)                                 32
186300021031     C     *IN32         IFEQ      '1'
186400021031     C                   MOVEl     D7w(X)        DS7w
186500040323     c                   if        §7W6T1 = 'T'  or
186600040323     c                             §7W6T2  = 'T'  or
186700040323     c                             §7W6T3  = 'T'  or
186800040323     c                             §7W6T4  = 'T'  or
186900040323     c                             §7W6T5  = 'T'
187000021031     c                   except    liberaarb
187100050627    1c                   if        gescomm = 'S'
187200050627     c                   commit
187300050627     c                   end
187400050217     c* forzo le "disposizioni" senza passare dalla giacenza
187500050217     c* es: invece di aprire giacenza distruggere la merce
187600021031     c                   movel     arblnp        fldlnpa
187700021031     c                   movel     arbaas        fldaasa
187800021031     c                   movel     arbnrs        fldnrsa
187900021031     c                   movel     arbnsp        fldnspa
188000021031     c                   movel     §7wca1        fldcca
188100021031     c                   movel     arbgga        fldgga
188200021031     c                   movel(p)  param80       kpjbu
188300021031     c                   call      'FNLG80R1'
188400021031     c                   parm                    kpjba
188500021031     c                   move      'X'           forzato
188600021031     c                   goto      forza2
188700021031     c                   endif
188800021031     c                   endif
188900050217     c* se deve mandare un msg
189000040421     C     §7Q2MF        IFEQ      'A'
189100940920     C     §7Q2MS        ANDNE     *BLANKS
189200040421      * verifico data consegna richiesta se maggiore di oggi non eseguo
189300040421     c                   if        arbdcr < woggi
189400040421     C     §7Q2MQ        IFEQ      'P'
189500950105     C                   MOVEL     'N'           WTREVE
189600950105     C                   MOVEL     'N'           WFORZA
189700950105     C                   MOVEL     §7Q2EV        V2CEVE
189800040421     C     §7Q2EV        IFNE      *BLANKS
189900950105     C                   EXSR      CTREVE
190000040421     C                   END
190100040421     C     WTREVE        IFEQ      'N'
190200950104     C                   MOVEL     §7Q2MS        V2CMSP
190300940920     C                   MOVEL     §7Q2MC        WFORZA
190400950104     C                   MOVEL     V2CLNA        WFLE              3 0
190500940920     C                   SETON                                        05
190600040421     C                   END
190700040421     C                   ELSE
190800950104     C                   MOVEL     §7Q2MS        V2CMSD
190900940920     C                   SETON                                        06
191000040421     C                   END
191100040421     C                   endif
191200040421     C                   END
191300940920     C                   END
191400940920     C                   END
191500961113     C* Se sono in arrivo reperisco descrizione aggiuntiva
191600961113     C* motivazione apertura giacenza da EDRDE
191700980112     C                   SETON                                        37
191800980112     C                   MOVEL     *BLANKS       ORGFL1
191900980112     C                   MOVE      ARBLNA        KFIL
192000980112     C     KFIL          CHAIN     AZORG01L                           31
192100020729     C                   MOVEL     *blank        LNAFL1            1
192200020729     C                   clear                   OG143
192300070320     C                   clear                   OG148
192400070410     C                   clear                   OG146
192500020729     C  n31              MOVEL     ORGDE3        OG143
192600070320     C  n31              MOVEL     ORGDE8        OG148
192700070410     C  n31              MOVEL     ORGDE6        OG146
192800070424     c  n31              if        §ogdda <> *blank
192900070424     c                   move      §ogdda        ogddan            8 0
193000070424     c                   endif
193100020729     C  N31§OgNTW        ifeq      'FED'
193200020729     C     §OgNTW        oreq      'EEX'
193300020729     C     §OgNTW        oreq      'EUP'
193400020729     C                   MOVEL     'E'           LNAFL1
193500020729     c                   end
193600961113     C     D05GES        IFEQ      'A'
193700980112     C     ORGFVA        ANDEQ     ' '
193800961217     C     WTRRDE        ANDEQ     'S'
193900020729     C     LNAFL1        ANDEQ     'E'
194000961204     C                   MOVEL     'DD2380'      KNMC
194100961204     C     KRDE          SETGT     EDRDE01L
194200961204     C     KRDE          READPE    EDRDE01L                               37
194300961204     C  N37              MOVEL     RDEVAL        V2CDMC
194400961204     C                   MOVEL     'DD4440'      KNMC
194500961113     C     KRDE          SETGT     EDRDE01L
194600961113     C     KRDE          READPE    EDRDE01L                               37
194700961113     C  N37              MOVEL     RDEVAL        V2CDMC
194800961113     C                   END
194900940915     C*
195000150515     C* RICHIAMO FUNZIONE TRUL27R PER REPERIRE IL "FIE" SDE CLIENTE
195100150515     C* CODIFICATO
195200150515     C                   MOVE      v2cscm        APPO              4 0
195300150515     C                   CLEAR                   TRUL27
195400150515     C     APPO          IFNE      8888
195500150515     C     APPO          ANDNE     9999
195600150515     C     APPO          ANDNE     *ZEROS
195700150515     C                   MOVEL     D05TRI        I27TLA
195800150515     C                   MOVEL     arbTSP        I27TSP
195900150515     C                   MOVE      arbLNA        I27LNA
196000150515     C                   MOVE      arbLNP        I27LNP
196100150515     C                   MOVE      arbctr        I27ctb
196200150515     C                   MOVE      KKSC          I27CLI
196300160316     C*******            MOVE      AMGDSP        I27DTA
196400160316     C                   CALL      'TRUL27R1'
196500150515     C                   PARM                    TRUL27
196600150515     C                   END
196700021031     C     forza2        ENDSR
196800900418     C*---------------------------------------------------------------*
196900900418     C*   G C P V I D                                                 *
197000900418     C*---------------------------------------------------------------*
197100900418     C     GCPVID        BEGSR
197200940915     C*
197300960123     C                   Z-ADD     GCPAGC        V4CAGC
197400960123     C                   Z-ADD     GCPFGC        V4CFGC
197500960123     C                   Z-ADD     GCPNGC        V4CNGC
197600960123     C                   MOVEL     GCPFST        V2CFST
197700050805     c*                  if        gcptcm <> 'R'
197800050805     C*                  MOVEL     GCPtcm        V2Ctcm
197900050805     c*                  end
198000950908     C     D05GES        IFEQ      'P'
198100940125     C     GCPSCM        IFNE      *ZEROS
198200900423     C                   MOVE      GCPSCM        §QUATR            4
198300940919     C                   MOVE      GCPSCM        KKSC
198400900423     C     §QUATR        IFEQ      *ALL'8'
198500900423     C     §QUATR        OREQ      *ALL'9'
198600940919     C* mittente da appoggio
198700050217     C     KSPE1         CHAIN     tIGAP01L                           31
198800940915     C     *IN31         IFEQ      '0'
198900950213     C                   MOVE      GCPSCM        V2CSCM
199000940919     C                   MOVEL     GAPRSM        V2CRSM
199100940919     C                   MOVEL     GAPINM        V2CINM
199200940919     C                   MOVEL     GAPLOM        V2CLOM
199300940919     C                   MOVEL     GAPPRM        V2CPRM
199400940919     C                   MOVEL     GAPCAM        V2CCAM
199500940919     C                   MOVEL     GAPNZM        V2CNZM
199600900418     C                   END
199700900509     C*
199800900419     C                   ELSE
199900940915     C     KACO          CHAIN     CNACO000                           31
200000940915     C     *IN31         IFEQ      '0'
200100940915     C     ACOFLG        ANDEQ     ' '
200200130322     C     ACOABL        ANDeq     *blank
200300950213     C                   MOVE      GCPSCM        V2CSCM
200400940915     C                   MOVEL     ACORAG        V2CRSM
200500940919     C     KACO          CHAIN     CNIND000                           31
200600940915     C     *IN31         IFEQ      '0'
200700940915     C                   MOVEL     INDVIA        V2CINM
200800940915     C                   MOVEL     INDCIT        V2CLOM
200900940915     C                   MOVEL     INDPRV        V2CPRM
201000050223     C                   MOVEL     INDCAE        V2CCAM
201100940919     C                   MOVEL     INDSTA        V2CNZM
201200940919     C                   END
201300940125     C                   END
201400940915     C                   END
201500940919     C                   END
201600950908     C*
201700950908     C                   END
201800050217     C* apertura prendo motivo dalla giac.
201900050217     C     WRIAPE        IFeq      ' '
202000940915     C* Codice motivo giacenza
202100940915     C     V2CCMC        IFEQ      *BLANKS
202200940915     C                   MOVE      GCPCMC        V2CCMC
202300920210     C                   END
202400940919     C* Ulteriore motivazione
202500950103     C                   Z-ADD     GCPAGC        KAGC
202600950103     C                   Z-ADD     GCPFGC        KFGC
202700950103     C                   Z-ADD     GCPNGC        KNGC
202800950103     C                   Z-ADD     0             KFRG
202900960208     C                   Z-ADD     10            KFAS
203000940919     C                   MOVE      *BLANKS       GNPDMC
203100050217     C     KGNP1         CHAIN     tIGNP01L                           31
203200911014     C     GNPDMC        IFNE      *BLANKS
203300940919     C                   MOVE      GNPDMC        V2CDMC
203400911129     C                   END
203500900604     C                   END
203600940919     C* Luogo deposito merce
203700940919     C                   MOVE      GCPCCR        V2CCCR
203800940919     C* Nr. e data comunicazione
203900940919     C                   MOVE      GCPNRC        V2CNRC
204000950908     C     D05GES        IFEQ      'P'
204100940919     C                   Z-ADD     GCPDXD        G02INV
204200940920     C                   Z-ADD     GCPDXD        AMGDXD
204300940919     C                   Z-ADD     0             G02DAT
204400940919     C                   MOVEL     '3'           G02ERR
204500940919     C                   CALL      'XSRDA8'
204600940919     C                   PARM                    WLBDA8
204700940919     C                   Z-ADD     G02DAT        V2CDXD
204800940919     C                   END
204900940919     C* Data apertura giacenza
205000940919     C                   MOVE      GCPMGC        WGG
205100940919     C                   MOVEL     GCPMGC        WMM
205200940919     C                   MOVEL     WGGMM         V2CDTG
205300940919     C                   MOVE      GCPAGC        V2CDTG
205400940919     C                   MOVE      GCPNGC        V2CNGC
205500940919     C* Fil. che ha aperto la giacenza
205600900419     C     GCPFGC        IFNE      *ZEROS
205700940919     C                   MOVE      GCPFGC        V2CFGC
205800940919     C                   MOVE      GCPFGC        KFIL
205900940919     C                   MOVEL     *ALL'?'       V2DFGC
206000940919     C     KFIL          CHAIN     AZORG01L                           31
206100940919     C     *IN31         IFEQ      '0'
206200940919     C     ORGFVA        ANDEQ     ' '
206300940920     C                   MOVEL     *BLANKS       V2DFGC
206400940920     C                   MOVEL     ORGDES        V2DFGC
206500900419     C                   END
206600940919     C                   END
206700940919     C* Data ultima riapertura
206800050223     c                   if        gcpriap = 'R'
206900940919     C                   MOVE      GCPDUR        G02INV
207000940919     C                   Z-ADD     0             G02DAT
207100940919     C                   MOVEL     '3'           G02ERR
207200940919     C                   CALL      'XSRDA8'
207300940919     C                   PARM                    WLBDA8
207400940919     C                   Z-ADD     G02DAT        V2CDUR
207500050223     c                   else
207600050223     C                   Z-ADD     0             V2CDUR
207700050223     c                   end
207800940919     C*
207900900418     C                   ENDSR
208000940922     C*---------------------------------------------------------------*
208100940922     C*   C H I U G C                                                 *
208200940922     C*---------------------------------------------------------------*
208300950301     C     APEGIA        BEGSR
208400940922     C*
208500940922     C                   MOVEL     'P'           V2CTCM
208600940922     C                   MOVEL     'N'           WTRTCM            1
208700950116     C                   Z-ADD     1             Y                 3 0
208800950301     C                   MOVE      V2CSCM        APPO              4 0
208900950228     C     BLPNZD        LOOKUP    CNZ(Y)                                 39
209000940922     C     APPO          IFNE      8888
209100940922     C     APPO          ANDNE     9999
209200950116     C     APPO          ANDNE     *ZEROS
209300990730     C*  Cerco progressivo tariffa in vigore su TNTAM
209400150512     C                   CLEAR                   fnlv59ds
209500150512     C                   MOVE      BLPCTR        kctr
209600150512     C                   MOVE      BLPCTR        ilv59ctr
209700150514     C                   MOVEL     ' '           ilv59tbo
209800150512     C                   Z-ADD     AMGDSP        ilv59dsp
209900150512     C                   MOVE      KKSC          ilv59ksc
210000150512     C                   MOVE      KCI           ilv59kci
210100150512     C                   MOVEL     O27FIE        ilv59fie
210200150512     C                   MOVEL     BLPTSP        ilv59tsp
210300150512     C                   MOVEL     'S'           ilv59con
210400150512     c                   eval      ilv59ta2='B'
210500150512     c                   eval      ilv59blo='S'
210600150512     C                   CALL      'FNLV59R'
210700150512     C                   PARM                    fnlv59ds
210800950113     C*
210900150512     C     olv59err      IFEQ      *BLANKS
211000150512     C     olv59tks      ANDEQ     'S'
211100950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
211200950113     C*  delle tariffe giacenze con il proggressivo giusto
211300150512     C                   MOVE      olv59prg      KPRG
211400990730     C     KTGC          CHAIN     TITGC01L                           31
211500950113     C     *IN31         IFEQ      '0'
211600950116     C     TGCTCM        ANDNE     *BLANKS
211700950113     C                   MOVE      TGCTCM        V2CTCM
211800950113     C                   MOVEL     'S'           WTRTCM
211900950113     C                   END
212000950113     C                   END
212100950113     C                   END
212200940922     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
212300940922     C*  lo cerco sull'anagrafica cliente
212400050217     c                   exsr      srcli
212500940922     C*
212600940922     C                   ENDSR
212700050517     C*---------------------------------------------------------------*
212800050517     C*   C H I U G C                                                 *
212900050517     C*---------------------------------------------------------------*
213000050517     C     APEGIAa       BEGSR
213100050608     C*
213200050517     c                   if        £LBLAAO = 0
213300050608     c* sono in aggiornamento archivi
213400050608     c                   if        flgagg = 1
213500050608     C     kspe1         chain     fnblp01l
213600050608     c                   else
213700050608     c* sono nei controlli del video
213800050608     C     kspe1         chain(n)  fnblp01l
213900050608     c                   end
214000050517     c                   else
214100050608     c                   if        flgagg = 1
214200050608     C     kblp          chain     fnblp01l
214300050608     c                   else
214400050608     C     kblp          chain(n)  fnblp01l
214500050517     c                   end
214600050608     c                   end
214700050608     c*
214800050517     C                   IF        %FOUND(FNBLP01L)
214900050517     C     BLPCCM        IFNE      *ZEROS
215000050517     C                   MOVEL     BLPCCM        V2CSCM
215100050517     C                   Z-ADD     BLPCCM        KKSC
215200050517     C                   ELSE
215300050517     C                   MOVEL     BLPKSC        V2CSCM
215400050517     C                   Z-ADD     BLPKSC        KKSC
215500050517     C                   END
215600091102     c                   exsr      forzacli
215700050809     C                   MOVE      V2CSCM        APPO              4 0
215800050809     c* tipo comunicazione
215900050701     c                   if        v2ctcm = ' '
216000050517     C                   MOVEL     'P'           V2CTCM
216100050517     C                   MOVEL     'N'           WTRTCM            1
216200050517     C                   Z-ADD     1             Y                 3 0
216300050517     C     BLPNZD        LOOKUP    CNZ(Y)                                 39
216400050517     C     APPO          IFNE      8888
216500050517     C     APPO          ANDNE     9999
216600050517     C     APPO          ANDNE     *ZEROS
216700050517     C*  Cerco progressivo tariffa in vigore su TNTAM
216800150512     C                   CLEAR                   fnlv59ds
216900150512     C                   MOVE      BLPCTR        kctr
217000150512     C                   MOVE      BLPCTR        ilv59ctr
217100150514     C                   MOVEL     ' '           ilv59tbo
217200150512     C                   Z-ADD     AMGDSP        ilv59dsp
217300150512     C                   MOVE      KKSC          ilv59ksc
217400150512     C                   MOVE      KCI           ilv59kci
217500150512     C                   MOVEL     O27FIE        ilv59fie
217600150512     C                   MOVEL     BLPTSP        ilv59tsp
217700150512     C                   MOVEL     'S'           ilv59con
217800150512     c                   eval      ilv59ta2='B'
217900150512     c                   eval      ilv59blo='S'
218000150512     C                   CALL      'FNLV59R'
218100150512     C                   PARM                    fnlv59ds
218200050517     C*
218300150512     C     olv59ERR      IFEQ      *BLANKS
218400150512     C     olv59tks      ANDEQ     'S'
218500050517     C*  Se ho trovato tariffa in vigore mi posiziono sul file
218600050517     C*  delle tariffe giacenze con il proggressivo giusto
218700150512     C                   MOVE      olv59prg      KPRG
218800050517     C     KTGC          CHAIN     TITGC01L                           31
218900050517     C     *IN31         IFEQ      '0'
219000050517     C     TGCTCM        ANDNE     *BLANKS
219100050517     C                   MOVE      TGCTCM        V2CTCM
219200160316     C                   MOVEL     'S'           WTRTCM
219300050517     C                   END
219400050517     C                   END
219500050517     C                   END
219600050517     c                   else
219700050517     C                   MOVEL     'S'           WTRTCM            1
219800050517     c                   end
219900050517     C*  Se non ho trovato tipo comunicazione mitt. su tariffe
220000050517     C*  lo cerco sull'anagrafica cliente
220100050517     c                   exsr      srcli
220200050809     c* se cliente vario e cod. mancata consegna = 999 (riapertura tecnica)
220300050809     c* forzo la riapertura come autoconfermata
220400050809     c                   if        v2ccmc = '999' and (appo = 8888 or
220500050809     c                             appo = 9999)
220600050809     c                   eval      §2uape = 'S'
220700050608     C                   END
220800050809     C*
220900050809     C                   END
221000050517     C*
221100050517     C                   ENDSR
221200900418     C*---------------------------------------------------------------*
221300900418     C*   C T R V I D                                                 *
221400900418     C*---------------------------------------------------------------*
221500940919     C     CTR02         BEGSR
221600021031     c                   if        giacen = 'N'
221700021031     C                   MOVEL     ERR(28)       $MSG
221800021031     C                   SETON                                        28
221900021031     C                   GOTO      FIctr2
222000021031     c                   endif
222100941003     C* Cod.particolarità giacenza
222200941003     C     '?'           SCAN      V2CRPG                                 32
222300941003     C     *IN32         IFEQ      '1'
222400941003     C                   MOVEL     'S'           DS7RIC
222500941003     C                   MOVEL     V2CGGA        DS7COD
222600941003     C                   MOVEL     'V'           DS7GES
222700941003     C                   MOVEL     DSTB69        KPJBU
222800941003     C                   CALL      'TRTB69R'
222900941003     C                   PARM                    KPJBA
223000941003     C                   SETON                                        19
223100941003     C                   MOVEL     ' '           V2CRPG
223200941003     C                   GOTO      FICTR2
223300941003     C                   END
223400940919     C* Cod.giacenza
223500940919     C     '?'           SCAN      V2CCMC                                 32
223600940919     C     *IN32         IFEQ      '1'
223700940919     C*§Ricerca in Tabella
223800940919     C                   MOVEL     '2A'          KCOD
223900940919     C                   MOVEL     *BLANK        KKEY
224000940919     C                   MOVEL     *BLANK        §DES             60
224100940919     C                   MOVE      *BLANK        V2CCMC
224200940919     C                   CALL      'X§TB60R'
224300940919     C                   PARM                    CODUT
224400940919     C                   PARM                    KCOD
224500940919     C                   PARM                    KKEY
224600940919     C                   PARM                    §DES
224700940919     C                   MOVEL     KKEY          V2CCMC
224800940919     C                   MOVEL     §DES          V2DCMC
224900940919     C                   SETON                                        19
225000940920     C                   GOTO      FICTR2
225100940919     C                   END
225200110531     C*particolarità giacenza con motivi non ammessi per apertura giacenza
225300110601     c                   if        v2ccmc <> *blank and v2cgga <> *blank
225400110531     c     v2ccmc        lookup    tmt                                    32
225500110531     c                   if        *in32
225600110531     C                   SETON                                        2842
225700110531     C                   MOVEL     ERR(35)       $MSG
225800110531     C                   GOTO      FICTR2
225900110531     c                   endif
226000110531     c                   endif
226100110531      *
226200940919     C                   Z-ADD     1             X
226300940919     C     V2CCMC        LOOKUP    C2A(X)                                 32
226400040324     c     d05cmc        ifne      d05cmch
226500040115     c     d05cmc        ifne      *blank
226600040115     c     dcr(x)        andeq     *blank
226700040115     C     *IN32         andeq     '1'
226800040115     C                   MOVEL     *ALL'?'       V2DCMC
226900040115     C                   SETON                                        2842
227000040115     C                   MOVEL     ERR(13)       $MSG
227100040115     C                   GOTO      FICTR2
227200040115     c                   else
227300040115     c                   z-add     x             xdcr              3 0
227400040115     c                   endif
227500040324     c                   else
227600040324     c                   z-add     x             xdcr              3 0
227700040324     c                   endif
227800040202     c     d05cmc        ifeq      *blank
227900040202     c     v2ccmc        andeq     §2gmdr
228000040325     c     d05cmc        oreq      *blank
228100040325     c     v2ccmc        andeq     §2gadr
228200040202     C                   MOVEL     *ALL'?'       V2DCMC
228300040202     C                   SETON                                        2842
228400040202     C                   MOVEL     ERR(13)       $MSG
228500040202     C                   GOTO      FICTR2
228600040202     c                   endif
228700080604      * verifica se destinatario e indirizzo già frequentati x segnalazione
228800080604      * motivo di non trovato ecc.ecc.
228900080604     c                   if        contrdest = *blank
229000080604     c                   if        v2ccmc = '021' and d05ges = 'A' or
229100080604     c                             v2ccmc = '022' and d05ges = 'A'
229200080604     c                   clear                   truldesds
229300080604     c                   move      arbRSD        idsRSD
229400080604     c                   move      arbIND        idSIND
229500080604     c                   move      arbLOD        idSLOD
229600080604     c                   move      arbPRD        idSPRD
229700080604     c                   move      arbNZD        idSNZD
229800080604     c                   call      'TRULDESR'
229900080604     c                   parm                    truldesds
230000080604     c                   if        odsesito = 'S'
230100080604     c                   eval      $msg = 'Esistono già -
230200080604     c                             spedizioni -
230300080604     c                             consegnate a questo indirizzo ' +
230400080604     c                             odsbolla
230500080604     c                   eval      contrdest = 'X'
230600080604     C                   SETON                                        2842
230700080604     C                   GOTO      FICTR2
230800080604     c                   endif
230900080604     c                   endif
231000080604     c                   endif
231100170412      * motivo 008 non permesso se non presente valore contrasssegno
231200170412     c     v2ccmc        ifeq      '008'
231300170412     c     V2CCAS        andeq     0
231400170725     c     d82ges        andeq     'A'
231500170412     C                   SETON                                        2842
231600170412     C                   MOVEL     ERR(38)       $MSG
231700170412     C                   GOTO      FICTR2
231800170412     c                   endif
231900170412      * motivo 012 non permesso se spedizione in assegnato
232000170412     c     v2ccmc        ifeq      '012'
232100170412     c     tipbol        andeq     'F'
232200170412     C                   SETON                                        2842
232300170412     C                   MOVEL     ERR(39)       $MSG
232400170412     C                   GOTO      FICTR2
232500170412     c                   endif
232600040310      * motivo 035 non permesso se linea partenza italia e DDT=si
232700170410      *paking list dopo controllo DDT
232800040310     c     v2ccmc        ifeq      '035'
232900040310     c     lnpfl1        andeq     ' '
233000040310     c     wntwlnp       andeq     *blank
233100040310     c                   if        §b4abm = 'S' or §b4abm = 'P' or
233200040310     c                             §b4abm = 'C' or §b4abm = 'Q'
233300040310     C                   SETON                                        2842
233400050608     C                   MOVEL     ERR(30)       $MSG
233500040310     C                   GOTO      FICTR2
233600040310     c                   endif
233700170410      *stesso controllo viene effettuato anche per la presenza di un
233800170410      *paking list
233900170410     c                   if        §AR5ALM ='S' or
234000170410     c                             §AR5ALS ='S' or
234100170410     c                             §AR5ALX ='S'
234200170410     C                   SETON                                        2842
234300170410     C                   MOVEL     ERR(30)       $MSG
234400170410     C                   GOTO      FICTR2
234500170410     c                   endif
234600040310     c                   endif
234700050608      * motivo 999 non utilizzabile in partenza
234800050608     c     v2ccmc        ifeq      '999'
234900050608     c     d05ges        andeq     'P'
235000050608     C                   SETON                                        2842
235100050608     C                   MOVEL     ERR(13)       $MSG
235200050608     C                   GOTO      FICTR2
235300050608     c                   endif
235400040310      *
235500950104     C     V2CCMC        IFNE      'GEN'
235600940919     C     *IN32         IFEQ      '1'
235700941228     C     V2CCMC        ANDNE     *BLANKS
235800950105     C     F2A(X)        ANDEQ     ' '
235900940920     C                   MOVEL     *BLANKS       V2DCMC
236000940920     C                   MOVEL     D2A(X)        V2DCMC
236100940919     C                   ELSE
236200950105     C     WFORZA        IFNE      'S'
236300950105     C     *INKA         OREQ      '1'
236400940919     C                   MOVEL     *ALL'?'       V2DCMC
236500940919     C                   SETON                                        2842
236600940919     C                   MOVEL     ERR(13)       $MSG
236700940919     C                   GOTO      FICTR2
236800940919     C                   END
236900950105     C                   END
237000970226     C* Se sto gestendo giacenza in partenza di una bolla import
237100970226     C* ctrl se devo gestrie obbligatoriamente l'ulteriore motiv.
237200980112     C     LNPFL1        IFEQ      'E'
237300970226     C     D05GES        ANDEQ     'P'
237400970310     C     V2CDMC        IFEQ      *BLANKS
237500970310     C     *IN03         OREQ      '0'
237600970226     C                   EXSR      CTRFTX
237700970226     C   28              GOTO      FICTR2
237800970226     C                   END
237900970310     C                   END
238000940919     C* Se codice motiv. generico obbligo l'ulteriore motivaz.
238100940919     C                   ELSE
238200980112     C     LNPFL1        IFEQ      'E'
238300961113     C                   SETON                                        2842
238400961113     C                   MOVEL     ERR(21)       $MSG
238500961113     C                   GOTO      FICTR2
238600961113     C                   END
238700940919     C     V2CDMC        IFEQ      *BLANKS
238800940919     C                   SETON                                        2843
238900961113     C                   MOVEL     ERR(14)       $MSG
239000940919     C                   GOTO      FICTR2
239100940919     C                   END
239200940919     C                   END
239300950104     C   32              MOVEL     TPG(X)        WTPGIA            1
239400940919     C*--------------------------------------------------------------*
239500940919     C*  Partenza  !!!!
239600940919     C     D05GES        IFEQ      'P'
239700940920     C*--------------------------------------------------------------*
239800940919     C* Ricerca cliente
239900940919     C     '?'           SCAN      V2CSCM                                 32
240000940919     C     *IN32         IFEQ      '1'
240100940919     C                   Z-ADD     KCI           KKCC
240200940919     C                   MOVEL     *BLANKS       ACORAG
240300940919     C                   MOVEL     V2CRSM        ACORAG
240400940919     C                   Z-ADD     0             ACOKSC
240500940919     C                   Z-ADD     0             PARSTA            1 0
240600940919     C                   CALL      'XALFA1'
240700940919     C                   PARM                    PARDUT
240800940919     C                   PARM                    CODUT
240900940919     C                   PARM                    ACORAG
241000940919     C                   PARM                    ACOKCC
241100940919     C                   PARM                    ACOKSC
241200940919     C                   PARM                    PARSTA
241300961016     C                   PARM                    PARFLR           90
241400940919     C     PARSTA        IFNE      -1
241500940919     C                   MOVE      ACOKSC        V2CSCM
241600940919     C                   ELSE
241700940919     C                   MOVE      *ZEROS        V2CSCM
241800940919     C                   END
241900940919     C                   SETON                                        19
242000940919     C                   GOTO      FICTR2
242100940919     C                   END
242200940919     C* Controllo se cambiato il mittente
242300940919     C     V2CSCM        IFEQ      *ZEROS
242400940919     C                   SETON                                        4028
242500940919     C                   MOVEL     ERR(10)       $MSG
242600940919     C                   GOTO      FICTR2
242700940919     C                   END
242800940919     C*  Se cambiato codice cliente lo decodifico
242900940919     C                   MOVE      V2CSCM        §SCM
243000050606     C                   MOVE      V2CSCM        §QUATR
243100940921     C                   MOVE      V2CSCM        KKSC
243200940920     C     §QUATR        IFNE      *ALL'8'
243300940920     C     §QUATR        ANDNE     *ALL'9'
243400940920     C     §SCM          IFNE      SAVSCM
243500940919     C     KACO          CHAIN     CNACO000                           31
243600940919     C     *IN31         IFEQ      '0'
243700940919     C     ACOFLG        ANDEQ     ' '
243800130322     C     ACOABL        ANDeq     *blank
243900940919     C                   MOVEL     ACORAG        V2CRSM
244000940920     C  NKF              SETON                                        6019
244100940919     C     KACO          CHAIN     CNIND000                           21
244200940919     C     *IN31         IFEQ      '0'
244300940919     C     INDFLG        ANDEQ     ' '
244400940919     C                   MOVEL     INDVIA        V2CINM
244500940919     C                   MOVEL     INDCIT        V2CLOM
244600940919     C                   MOVEL     INDPRV        V2CPRM
244700050223     C                   MOVEL     INDCAE        V2CCAM
244800940920     C                   MOVEL     INDSTA        V2CNZM
244900940919     C                   END
245000940919     C                   ELSE
245100940919     C                   SETON                                        4028
245200940919     C                   MOVEL     ERR(10)       $MSG
245300940919     C                   GOTO      FICTR2
245400940919     C                   END
245500940920     C                   END
245600940920     C                   ELSE
245700940920     C     §SCM          IFNE      SAVSCM
245800940920     C     V2CRSM        ORNE      SAVRSM
245900940920     C     V2CINM        ORNE      SAVINM
246000940920     C     V2CLOM        ORNE      SAVLOM
246100940920     C     V2CPRM        ORNE      SAVPRM
246200940920     C     V2CCAM        ORNE      SAVCAM
246300940920     C     V2CNZM        ORNE      SAVNZM
246400940920     C  NKF              SETON                                        6019
246500940920     C                   END
246600940919     C                   END
246700940919     C*--------------------------------------------------------------*
246800050606     C*  Ricerca tipo comunicazione
246900050606    1C     V2CTCM        IFEQ      '?'
247000050606     C*§  RICERCA IN TABEL00F
247100050606     C                   MOVEL     '2F'          KCOD
247200050606     C                   MOVEL     *BLANK        KKEY
247300050606     C                   MOVEL     *BLANK        §DES
247400050606     C                   MOVE      *BLANK        V2CTCM
247500050606     C                   CALL      'X§TABER'
247600050606     C                   PARM                    CODUT
247700050606     C                   PARM                    KCOD
247800050606     C                   PARM                    KKEY
247900050606     C                   PARM                    §DES
248000050606     C                   MOVEL     KKEY          V2CTCM
248100050606     C                   MOVEL     §DES          V2DTCM
248200050606     C                   SETON                                        19
248300050606     C                   GOTO      FICTR2
248400050606    1C                   END
248500050608     c                   clear                   savs2f            1
248600050608     c                   clear                   savf2f            1
248700050608     c                   clear                   savv2f            1
248800050608     c                   clear                   save2f            1
248900050606     C                   Z-ADD     1             X
249000050606     C     V2CTCM        LOOKUP    C2F(X)                                 32
249100050606    1C     *IN32         IFEQ      '1'
249200050614     c     r2f(x)        andeq     *blank
249300050606     C                   MOVEL     D2F(X)        V2DTCM
249400050606     C*                  MOVEL     F2F(X)        TIPTCM            1
249500050608     c                   eval      savs2f = s2f(x)
249600050608     c                   eval      savf2f = f2f(x)
249700050608     c                   eval      savv2f = v2f(x)
249800050608     c                   eval      save2f = e2f(x)
249900050606     C*  Controllo Fax
250000050606    2C     T2F(X)        IFEQ      'F'
250100160223      *
250200050606    3C     §QUATR        IFEQ      *ALL'8'
250300050606     C     §QUATR        OREQ      *ALL'9'
250400050606     C                   SETON                                        2841
250500050606     C                   MOVEL     ERR(18)       $MSG
250600050606     C                   GOTO      FICTR2
250700050606     C                   ELSE
250800160223      *
250900050606     C                   CLEAR                   TRUL43
251000171122     C                   CLEAR                   TRUL43_1
251100050606     C                   MOVE      V2CSCM        D43KSC
251200050606     C                   CALL      'TRUL43R'
251300050606     C                   PARM                    TRUL43
251400171122     C                   PARM                    TRUL43_1
251500171122      *---
251600050606     C                   CLEAR                   TRUL42
251700050606     C                   MOVEL     INDTLF        D42FAX
251800050606    4C     D43TRV        IFEQ      'S'
251900050606     C                   MOVEL     D43FAX        D42FAX
252000050606    4C                   END
252100050606     C                   CALL      'TRUL42R'
252200050606     C                   PARM                    TRUL42
252300171122      *---
252400050606    4C     D42ERR        IFEQ      '1'
252500050606     C                   SETON                                        2841
252600050606     C                   MOVEL     ERR(19)       $MSG
252700050606     C                   GOTO      FICTR2
252800050606    4C                   END
252900050606    3C                   END
253000050606    2C                   END
253100050606     C*  Controllo invi tramite mail
253200050606    2C     T2F(X)        IFEQ      'M'
253300160223      *
253400050606    3C     §QUATR        IFEQ      *ALL'8'
253500050606     C     §QUATR        OREQ      *ALL'9'
253600050606     C                   SETON                                        2841
253700050606     C                   MOVEL     ERR(18)       $MSG
253800050606     C                   GOTO      FICTR2
253900050606     C                   ELSE
254000160223      *
254100160223      * controlla se si tratta di una bolla DPD con tanto di PARCEL
254200160223      *   da cui prendere il DEPOT
254300160223     c                   clear                   fnlg24ds
254400160223     c                   z-add     V1CAAS        i24aas
254500160223     c                   z-add     V1CLNP        i24lnp
254600160223     c                   z-add     V1CNRS        i24nrs
254700160223     c                   z-add     V1CNSP        i24nsp
254800160223      *
254900160223     c                   call      'FNLG24R'
255000160223     C                   parm                    fnlg24ds
255100160223      *
255200050606      * reperimento luogo e ind. e-mail
255300050606     C                   CLEAR                   TRUL43
255400171122     C                   CLEAR                   TRUL43_1
255500160223     c*
255600160223     c*  se c'è il parcel allora deve passare il Depot e verificare
255700160223     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
255800160223     c*  l'informazione di apertura giacenza.
255900160223     c                   if          o24npr <> *blank
256000160303     c                   if        %subst(o24npr:1:4)<>'0844' and
256100160303     c                             %subst(o24npr:1:4)<>'0845'
256200171122     c                   movel     o24dep4       D43DEPOT
256300171122     c                   if          D43DEPOT = *blank
256400171122     c                   movel     o24npr        D43DEPOT
256500171122     c                   end
256600171122     c                   movel     o24dep7       D43DEP7
256700160303     c                   end
256800160223     c                   end
256900160223     c*
257000050606     C                   MOVE      V2CSCM        D43KSC
257100050606     C                   CALL      'TRUL43R'
257200050606     C                   PARM                    TRUL43
257300171122     C                   PARM                    TRUL43_1
257400160223     c*
257500050606    4C     D43TRV        IFEQ      'S'
257600160223     c*
257700050606    5C     D43TRVm       ifne      'S'
257800050606     C                   SETON                                        2841
257900050606     C                   MOVEL     ERR(32)       $MSG
258000050606     C                   GOTO      FICTR2
258100050606    5c                   endif
258200160223     c*
258300050606     c                   else
258400050606     c* note reperimento nota 88 eventuale indirizzo mail in assenza
258500050606      * del luogo
258600050606     c                   move      v2cscm        kcnk1
258700050606     c     kntc          chain     tfntc01l
258800050606    5c                   if        not %found
258900050606     C                   SETON                                        2841
259000050606     C                   MOVEL     ERR(32)       $MSG
259100050606     C                   GOTO      FICTR2
259200050606    5c                   endif
259300050606    4c                   endif
259400050606    3c                   endif
259500050606    2c                   endif
259600050606      *
259700050606     C                   ELSE
259800050606     C                   SETON                                        2841
259900050606     C                   MOVEL     ERR(12)       $MSG
260000050606     C                   GOTO      FICTR2
260100050606    1C                   END
260200940919     C*--------------------------------------------------------------*
260300940919     C*   Arrivo !!!!!
260400940919     C                   ELSE
260500940919     C*--------------------------------------------------------------*
260600940919     C* Se sono in riapertura controllo se è possibile riaprire
260700940919     C     WRIAPE        IFEQ      'S'
260800050629     C     CNT           andeq     15
260900980114     C                   SETON                                        2819
261000940919     C                   MOVEL     ERR(11)       $MSG
261100940919     C                   GOTO      FICTR2
261200940919     C                   END
261300050629     c* ammetto l'apertura tecnica solo per xx volte (da tabella 2G)
261400050629     c                   if        v2ccmc = '999'
261500050629     c     cntced        add       1             comcnt            2 0
261600050701     c                   if        comcnt > §2glat
261700050629     C                   MOVEL     *ALL'?'       V2DCMC
261800050629     C                   SETON                                        2842
261900050629     C                   MOVEL     ERR(34)       $MSG
262000050629     C                   GOTO      FICTR2
262100050629     c                   end
262200050629     c                   end
262300950104     C* Controllo se posso aprire giacenza x motivo esaminato
262400960403     C     WRIAPE        IFNE      'S'
262500050607     c* codice giacenza 999 ammesso solo se riapertura
262600050607     c                   if        v2ccmc = '999'
262700050607     C                   MOVEL     *ALL'?'       V2DCMC
262800050607     C                   SETON                                        2842
262900050607     C                   MOVEL     ERR(13)       $MSG
263000050607     C                   GOTO      FICTR2
263100050607     c                   end
263200960605     C* Controllo se devo gestire forzatura da apertura
263300960605     C     V2CEVE        IFEQ      *BLANKS
263400960625     C     *IN07         OREQ      '0'
263500960605     C     *INKA         OREQ      '1'
263600960605     C*
263700960625     C     WFORZA        IFEQ      'S'
263800960625     C     *INKA         ANDEQ     '0'
263900960625     C                   SETON                                        19
264000960625     C                   ELSE
264100950104     C     V2CGGA        IFNE      *BLANKS
264200950104     C     WTIP          ANDNE     *BLANKS
264300950104     C                   SETOFF                                       32
264400950104     C     WTPGIA        IFNE      *BLANKS
264500950104     C                   Z-ADD     1             X
264600950104     C     WTPGIA        LOOKUP    TIP(X)                                 32
264700950104     C                   END
264800950104     C     *IN32         IFEQ      '0'
264900071107     C                   Z-ADD     1             X
265000950104     C     'T'           LOOKUP    TIP(X)                                 32
265100950104     C                   END
265200950104     C     *IN32         IFEQ      '1'
265300970624     C     *INKN         ANDEQ     '1'
265400970624     C                   SETON                                        28
265500970624     C                   MOVEL     ERR(24)       $MSG
265600970624     C                   GOTO      FICTR2
265700970624     C                   END
265800970624     C     *IN32         IFEQ      '1'
265900990805     C     DSP(X)        IFEQ      'R'
266000990805     C     WOGGI         ANDGE     §2GDPI
266100990805     C     WOGGI         ANDLE     §2GDPF
266200990805     C                   SETON                                        28
266300990805     C                   MOVEL     ERR(26)       $MSG
266400990805     C                   GOTO      FICTR2
266500990805     C                   END
266600950105     C                   SETON                                        19
266700950104     C     KSPE1         CHAIN     FNARB01L                           34
266800950104     C     *IN34         IFEQ      '0'
266900100614     C                   MOVEL     ARBFBC        savfbc
267000950104     C                   MOVEL     ' '           ARBFBC
267100950104     C                   EXCEPT    AGG
267200050627    1c                   if        gescomm = 'S'
267300050627     c                   commit
267400050627     c                   end
267500950104     C                   END
267600950104     C                   MOVE      V2CLNA        D82FGS
267700950104     C                   MOVE      *ZEROS        D82FRG
267800950104     C                   MOVEL     DSP(X)        D82DIS
267900950104     C                   MOVEL     'A'           D82GES
268000950104     C                   MOVEL     'N'           D82GIA
268100950104     C                   MOVE      KAAS          D82AAS
268200950104     C                   MOVE      KLNP          D82LNP
268300950104     C                   MOVE      KNRS          D82NRS
268400950104     C                   MOVE      KNSP          D82NSP
268500030716     C                   MOVEL     D05PSS        WPGMSS
268600021203     C                   MOVEL(p)  FNLG82        KPJBU
268700950104     C                   CALL      'FNLG20R'
268800950104     C                   PARM                    KPJBA
268900970204     C                   MOVEL     KPJBU         FNLG82
269000100614     C*  Se premuto F12 riMETTO IL BLOCCO A COME ERA PRIMA DELLA CHAIN
269100100614     C*IN MODO DA NON VARIARE COMUNQUE LA BOLLA PRIMA INSERIVA SEMPRE IL BLOCCO
269200970204     C     D82F12        IFEQ      '1'
269300970204     C     KSPE1         CHAIN     FNARB01L                           34
269400100614     C                   MOVEL     savFBC        ARBFBC
269500100614     C*mm                MOVEL     'G'           ARBFBC
269600970204     C  N34              EXCEPT    AGG
269700050627    1c  n34              if        gescomm = 'S'
269800050627     c                   commit
269900050627     c                   end
270000970204     C                   END
270100950104     C*
270200950104     C     D05RIC        IFEQ      'S'
270300950104     C                   MOVEL     'S'           WFINE
270400950104     C                   ELSE
270500950104     C                   MOVEL     '1'           WTPVID
270600950224     C                   MOVEL     D05GES        SAVGES
270700950224     C                   CLEAR                   FNLG05
270800950224     C                   MOVEL     SAVGES        D05GES
270900950104     C                   EXSR      PULVID
271000950104     C                   END
271100950104     C                   END
271200950104     C                   END
271300960403     C                   END
271400960625     C                   END
271500960605     C*
271600960605     C                   END
271700940919     C*--------------------------------------------------------------*
271800940919     C* Luogo deposito merce
271900940919     C                   Z-ADD     1             X
272000940919     C     V2CCCR        LOOKUP    C2B(X)                                 32
272100940919     C     *IN32         IFEQ      '1'
272200940920     C                   MOVEL     *BLANKS       V2DCCR
272300940920     C                   MOVEL     D2B(X)        V2DCCR
272400940919     C                   ELSE
272500940919     C* Se non lo trovo nella tabella corrisp. lo cerco in organigramma
272600940919     C                   MOVE      V2CCCR        KFIL
272700940919     C     KFIL          CHAIN     AZORG01L                           31
272800940919     C     *IN31         IFEQ      '0'
272900940919     C     ORGFVA        ANDEQ     ' '
273000940920     C                   MOVEL     *BLANKS       V2DCCR
273100940920     C                   MOVEL     ORGDES        V2DCCR
273200940919     C                   ELSE
273300940919     C                   SETON                                        2846
273400940919     C                   MOVEL     *ALL'?'       V2DCCR
273500940920     C                   MOVEL     ERR(15)       $MSG
273600940919     C                   GOTO      FICTR2
273700940919     C                   END
273800940919     C                   END
273900940919     C*--------------------------------------------------------------*
274000940919     C                   END
274100940920     C*--------------------------------------------------------------*
274200940920     C* Controllo se devo gestire forzatura da apertura
274300960605     C     WFORZA        IFNE      'S'
274400960605     C     *INKA         OREQ      '0'
274500950104     C     V2CEVE        IFNE      *BLANKS
274600950105     C     *IN07         ANDEQ     '1'
274700960610     C                   SETON                                        19
274800950104     C                   MOVEL     V2CEVE        EVBCEV
274900950104     C                   MOVEL     *BLANKS       EVBATB
275000950104     C                   Z-ADD     WOGGI         EVBDTV
275100950104     C                   TIME                    EVBORV
275200950104     C                   Z-ADD     WFLE          EVBFLE
275300021114     C                   Z-ADD     o36cou        EVBCDU
275400950104     C                   Z-ADD     V2CLNP        EVBLNP
275500950104     C                   Z-ADD     V2CAAS        EVBAAS
275600950104     C                   Z-ADD     V2CNRS        EVBNRS
275700950104     C                   Z-ADD     V2CNSP        EVBNSP
275800950104     C                   Z-ADD     WOGGI         EVBDEV
275900950113     C                   MOVEL     EVBORV        EVBHEV
276000950104     C                   MOVEL     *BLANKS       EVBFTR
276100950104     C                   Z-ADD     0             EVBDTR
276200990803     C                   MOVEL     *BLANKS       EVBNOT
276300990803     C                   Z-ADD     0             EVBDTC
276400990803     C                   WRITE     FNEVB000
276500051114    1c                   if        gescomm = 'S'
276600051114     c                   commit
276700051114     c                   end
276800950104     C*
276900950104     C     D05RIC        IFEQ      'S'
277000950104     C                   MOVEL     'S'           WFINE
277100950104     C                   ELSE
277200950104     C                   MOVEL     '1'           WTPVID
277300950224     C                   MOVEL     D05GES        SAVGES
277400950224     C                   CLEAR                   FNLG05
277500950224     C                   MOVEL     SAVGES        D05GES
277600950104     C                   EXSR      PULVID
277700950104     C                   END
277800961001     C                   ELSE
277900961001     C*  Se evento trovato ma non ancora trascorsi i giorni vado a fine
278000961001     C*  se non effettuata forzatura
278100961001     C     V2CEVE        IFNE      *BLANKS
278200961001     C     *IN33         ANDEQ     '0'
278300961001     C     WFORZA        ANDEQ     'S'
278400961001     C     *INKA         ANDEQ     '0'
278500961001     C*
278600961001     C                   SETON                                        19
278700961001     C     D05RIC        IFEQ      'S'
278800961001     C                   MOVEL     'S'           WFINE
278900961001     C                   ELSE
279000961001     C                   MOVEL     '1'           WTPVID
279100961001     C                   MOVEL     D05GES        SAVGES
279200961001     C                   CLEAR                   FNLG05
279300961001     C                   MOVEL     SAVGES        D05GES
279400961001     C                   EXSR      PULVID
279500961001     C                   END
279600961001     C                   END
279700961001     C*
279800960611     C                   END
279900950104     C*
280000940920     C                   END
280100940919     C*--------------------------------------------------------------*
280200940919     C     FICTR2        ENDSR
280300900418     C*---------------------------------------------------------------*
280400950105     C* Controllo esistenza evento
280500950105     C*---------------------------------------------------------------*
280600950105     C     CTREVE        BEGSR
280700950105     C*
280800950105     C                   SETON                                        07
280900950105     C                   MOVE      V2CEVE        KCEV
281000950105     C                   Z-ADD     1             X
281100950105     C     V2CEVE        LOOKUP    C2A(X)                                 32
281200950105     C* Posizionamento su file degli eventi x reperire data
281300950105     C* tentata consegna
281400950105     C                   Z-ADD     *HIVAL        KDEV
281500990803     C     KEVB1         SETLL     FNEVB05L
281600990803     C     KEVB2         READPE    FNEVB05L                               33
281700960625     C  N33KCEV          COMP      EVBCEV                             3333
281800960625     C*
281900950105     C     *IN33         IFEQ      '0'
282000950105     C                   SETOFF                                       07
282100950105     C                   Z-ADD     EVBDEV        DATADA
282200950105     C                   Z-ADD     0             GGF
282300950105     C     G2A(X)        IFNE      0
282400950105     C                   Z-ADD     WOGGI         DATAA
282500990906     C                   CALL      'XGGFEST'
282600950105     C                   PARM                    GFES8
282700950105     C                   END
282800950105     C*
282900950105     C                   Z-ADD     DATADA        G02INV
283000950105     C                   MOVE      '3'           G02ERR
283100950105     C                   CALL      'XSRDA8'
283200950105     C                   PARM                    WLBDA8
283300950105     C     G02TGI        ADD       G2A(X)        GIOTGI
283400950105     C                   ADD       GGF           GIOTGI
283500950105     C                   CALL      'XSRGI8'
283600950105     C                   PARM                    DATPAR
283700950105     C*
283800950105     C     GIOINV        IFLT      WOGGI
283900950105     C                   MOVEL     'S'           WTREVE            1
284000950105     C                   END
284100960625     C                   ELSE
284200960625     C* ARRIVO !!!!
284300960625     C     D05GES        IFEQ      'A'
284400960625     C                   Z-ADD     0             KDEV
284500960625     C* Se una riapertura controllo se è già stato immesso un
284600960625     C* evento di attesa successivo ad essa
284700960625     C     WRIAPE        IFEQ      'S'
284800961001     C                   Z-ADD     GCPDUR        G02INV
284900961001     C                   MOVE      *ZEROS        G02DAT
285000961001     C                   MOVE      '3'           G02ERR
285100960625     C                   CALL      'XSRDA8'
285200960625     C                   PARM                    WLBDA8
285300960625     C                   MOVE      G02INV        KDEV
285400960625     C                   END
285500960625     C*  Mi posiziono sugli eventi successivi alla data consegna per
285600960625     C*  verificare se ci sono degli eventi di attesa
285700990803     C     KEVB1         SETLL     FNEVB05L
285800990803     C     KEVB2         READE     FNEVB05L                               34
285900961001     C     *IN34         DOWEQ     '0'
286000960625     C     KCEV          ANDNE     EVBCEV
286100990803     C  N34KEVB2         READE     FNEVB05L                               34
286200960625     C                   END
286300960625     C* se dopo l'ultima apertura è stato emesso un evento di giac.
286400960625     C* non devo più accettarne un altro
286500960625     C     KCEV          IFEQ      EVBCEV
286600960625     C                   SETOFF                                       07
286700960625     C                   MOVEL     'S'           WTREVE            1
286800950105     C                   END
286900960625     C                   END
287000950105     C*
287100960625     C                   END
287200950105     C*
287300950105     C                   ENDSR
287400970226     C*---------------------------------------------------------------*
287500970226     C*   C T R F T X : Controllo se devo scrivere x ulteriore motiv. *
287600970226     C*---------------------------------------------------------------*
287700970226     C     CTRFTX        BEGSR
287800970226     C*
287900970226     C* Controllo se cliente usa lista labarta o VGL
288000970226     C                   Z-ADD     1             X
288100970226     C     V2CSCM        LOOKUP    KSC(X)                                 32
288200970226     C     *IN32         IFEQ      '1'
288300970226     C                   MOVEL     TLI(X)        WTPLIS            1
288400970226     C                   Z-ADD     1             X
288500970227     C     V2CCMC        LOOKUP    E2A(X)                                 32
288600970226     C* Se cliente usa lista Labrta verifico se flag x tale lista
288700970226     C* prevede la descrizione aggiuntiva obbligatoria altrimenti
288800970226     C* controllo se lista VGL vuole motivazione aggiun.obbligat.
288900970226     C     WTPLIS        IFEQ      'S'
289000970227     C     FTL(X)        ANDEQ     'S'
289100970226     C     WTPLIS        OREQ      ' '
289200970227     C     FTV(X)        ANDEQ     'S'
289300970310     C     V2CDMC        ORNE      *BLANKS
289400970310     C     *IN03         ANDEQ     '0'
289500970226     C                   SETON                                        2843
289600970226     C                   MOVEL     ERR(23)       $MSG
289700970226     C                   END
289800970226     C                   END
289900970226     C*
290000970226     C                   ENDSR
290100970624     C*---------------------------------------------------------------*
290200970624     C*   CALL03: Richiamo pgm per visualizzazione note apertura      *
290300970624     C*---------------------------------------------------------------*
290400970624     C     CALL03        BEGSR
290500970624     C*
290600970624     C* Imposto parametri
290700970624     C                   CLEAR                   FNLG03
290800970624     C                   Z-ADD     V2CLNP        D03LNP
290900970624     C                   Z-ADD     V2CNRS        D03NRS
291000970624     C                   Z-ADD     V2CNSP        D03NSP
291100970624     C                   Z-ADD     V2CDSP        D03DSP
291200970624     C                   MOVEL     V2CTBL        D03CBO
291300970624     C                   MOVEL     V2DTBL        D03DBO
291400970624     C                   Z-ADD     GCPFGC        D03FGC
291500970624     C                   MOVEL     V2DLNA        D03DFG
291600970624     C                   Z-ADD     V2CNGC        D03NGC
291700970624     C                   Z-ADD     V2CDTG        D03DGC
291800970624     C                   Z-ADD     V2CDUR        D03DUR
291900970624     C                   Z-ADD     GCPNRC        D03NRC
292000970624     C                   Z-ADD     V2CDXD        D03DXD
292100970624     C                   MOVEL     V2CCCR        D03CCR
292200970624     C                   MOVEL     V2DCCR        D03DCR
292300970624     C                   MOVE      V2CSCM        D03KSC
292400970624     C                   Z-ADD     V2CRMN        D03RMN
292500970624     C                   MOVEL     V2CRMA        D03RMA
292600970624     C                   MOVEL     V2CRSM        D03RSM
292700970624     C                   MOVEL     V2CINM        D03INM
292800970624     C                   MOVEL     V2CCAM        D03CAM
292900970624     C                   MOVEL     V2CLOM        D03LOM
293000970624     C                   MOVEL     V2CPRM        D03PRM
293100970624     C                   MOVEL     V2CNZM        D03NZM
293200970624     C                   MOVEL     V2CRSD        D03RSD
293300970624     C                   MOVEL     V2CDE2        D03RS2
293400970624     C                   MOVEL     V2CIND        D03IND
293500970624     C                   MOVEL     V2CCAD        D03CAD
293600970624     C                   MOVEL     V2CLOD        D03LOD
293700970624     C                   MOVEL     V2CPRD        D03PRD
293800970624     C                   MOVEL     V2CNZD        D03NZD
293900970624     C                   MOVEL     V2CCMC        D03CMC
294000970624     C                   MOVEL     V2DCMC        D03CMD
294100970624     C                   MOVEL     V2CDMC        D03DMC
294200970624     C                   MOVEL     'M'           D03TPV
294300970624     C                   CALL      'FNLG03R'
294400970624     C                   PARM                    FNLG03
294500970624     C*
294600970624     C                   ENDSR
294700950105     C*---------------------------------------------------------------*
294800900418     C*   G C P W T R                                                 *
294900900418     C*---------------------------------------------------------------*
295000900418     C     GCPWTR        BEGSR
295100940919     C*---------------------------------------------------------------*
295200940919     C*  Arrivo !!!
295300050512    1C     D05GES        IFEQ      'A'
295400050420     c                   clear                   gcpriap
295500050412     C                   ADD       1             CNT
295600050512    2C     WRIAPE        IFEQ      'S'
295700941223     C                   Z-ADD     GCPAGC        KAGC
295800941223     C                   Z-ADD     GCPNGC        KNGC
295900941223     C                   Z-ADD     GCPFGC        KFGC
296000050217     C     KGNP          CHAIN     tIGNP01L                           31
296100940919     C                   Z-ADD     CNT           GNPFRG
296200940919     C* Aggiorno motivo giacenza con nuovo prog.
296300050512    3C     *IN31         DOWEQ     '0'
296400050512    4C     GNPTPN        IFNE      'I'
296500970423     C     GNPFAS        OREQ      35
296600050217     C                   UPDATE    tIGNP000
296700050512    4C                   END
296800050217     C     KGNP          READE     tIGNP01L                               31
296900941223     C                   Z-ADD     CNT           GNPFRG
297000050512    3C                   ENDdo
297100050217     C* Chiudo giacenza mettendola a fase 40
297200050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
297300050512    3c                   if        %error
297400070521     C     KSPE2         CHAIN(n)  tIGCP01L
297500050315     c                   exsr      geslck
297600050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
297700050512    3c                   endif
297800170922     c                   eval      tigcpb = tigcp
297900941223     C                   Z-ADD     CNT           GCPFRG
298000050217     C                   Z-ADD     40            GCPFAS
298100050217     C                   WRITE     tIGCP000
298200170922     c                   eval      ILG33CTA = 'M'
298300170922     c                   eval      ILG33PRU = knmus
298400170922     c                   eval      ILG33NOJ = knmeb
298500170922     c                   eval      ILG33PGM = 'FNLG05R'
298600170922     c                   eval      tigcpa = tigcp
298700170922     c                   call      'FNLG33R'
298800170922     c                   parm                    fnlg33ds
298900170922     c                   parm                    tigcpb
299000170922     c                   parm                    tigcpa
299100050315     C     KSPE2         CHAIN(e)  tIGCP01L                           30
299200050512    3c                   if        %error
299300070521     C     KSPE2         CHAIN(n)  tIGCP01L
299400050315     c                   exsr      geslck
299500050318     C     KSPE2         CHAIN(e)  tIGCP01L                           30
299600050512    3c                   endif
299700170922     c   30              clear                   tigcpb
299800170922     c  n30              eval      tigcpb = tigcp
299900941223     C                   MOVE      WOGGI         GCPDUR
300000050217     c                   eval      gcpriap = 'R'
300100050607     c* se cod. giacenza 999 forzo tipo comunicazione = a Z
300200050607     c                   if        v2ccmc = '999'
300300050609     c                   eval      v2ctcm = 'R'
300400050607    2C                   END
300500050512    2C                   END
300600970401     C* Procedo all'eventuale chiusura anomalia 130
300700970401     C                   EXSR      CLO130
300800940919     C* Numero comunicazione interna
300900940919     C                   MOVE      GCPNGC        GCPNRC
301000950414     C* Abblenca comunque cod. motivo mancata consegna
301100940920     C     KSPE1         CHAIN     FNARB01L                           31
301200940919     C                   MOVEL     *BLANKS       ARBCMC
301300940919     C                   MOVEL     'G'           ARBFBC
301400940922     C  N31              EXCEPT    AGG
301500050512    2C     D05AAF        IFNE      0
301600940919     C                   Z-ADD     D05AAF        KAAS
301700940919     C                   Z-ADD     D05LNF        KLNP
301800940919     C                   Z-ADD     D05NRF        KNRS
301900940919     C                   Z-ADD     D05NSF        KNSP
302000940919     C     KSPE1         CHAIN     FNARB01L                           31
302100940919     C                   MOVEL     *BLANKS       ARBCMC
302200940919     C                   MOVEL     'G'           ARBFBC
302300940919     C  N31              EXCEPT    AGG
302400050512    2C                   END
302500050608     c                   eval      flgagg = 1
302600050517     c                   exsr      apegiaa
302700050608     c                   clear                   flgagg
302800050331     c* controllo che fase impostare 10/15 da FNCLS per apertura automatica
302900050331     c* imposto il apertura automatica = S se nell'anagrafica c'è
303000050331     c* blanks
303100050512    2c                   if        §2uape = 'S'
303200050512     c* per le autoconfermate se possibile (tab. 2F) stampa la
303300050512     c* comunicazione al mittente e porta la fase a 20
303400050513     c                   if        savs2f = 'S' or savf2f = 'N'
303500050512     c                   eval      gcpfas = 20
303600050512     c                   eval      gcpdmm = woggi
303700050513     C     D43TRV        IFEQ      'S'
303800050513     C                   MOVEL     'S'           v2cFST
303900050513     C                   END
304000050512     c                   else
304100050218     c                   eval      gcpfas = 15
304200050512     c                   end
304300050613     c                   eval      gcpapeaut = 'S'
304400050218     c                   else
304500050218     c                   eval      gcpfas = 10
304600050331     c                   eval      gcpapeaut = *blanks
304700050512    2c                   end
304800050517     C* Scrivo nuovo record giacenza con proggressivo a 0
304900050517    2C     *IN30         IFEQ      '1'
305000050517     C                   MOVEL     *BLANKS       GCPATB
305100050517     C                   MOVE      WHHDAT        GCPAGC
305200050517     C                   MOVE      DATEU         GCPMGC
305300050517     C                   Z-ADD     KFGC          GCPFGC
305400050517     C                   MOVEL     woggi         GCPDUR
305500050517     C* imposto i riferimenti della spedizione originale/partenza e arrivo.
305600050517     c* In caso di dirottamento. i campi della sped. orig./partenza li ho
305700050517     c* già memorizzati nei controlli
305800050517    3c                   if        £LBLAAO = 0
305900050517     C                   MOVEL     V2CLNP        GCPLNP
306000050517     C                   MOVEL     V2CAAS        GCPAAS
306100050517     C                   MOVEL     V2CNRS        GCPNRS
306200050517     C                   MOVEL     V2CNSP        GCPNSP
306300050517     C                   MOVEL     V2CLNA        GCPLNA
306400050517     c                   else
306500050517     C                   MOVEL     £LBLAAO       GCPAAS
306600050517     C                   MOVEL     £LBLLPO       GCPLNP
306700050517     C                   MOVEL     £LBLNRO       GCPNRS
306800050517     C                   MOVEL     £LBLNSO       GCPNSP
306900050517     C                   MOVEL     £LBLLAO       GCPLNA
307000050517    3c                   end
307100050517     C                   MOVEL     V2CLNP        GCPLNPa
307200050517     C                   MOVEL     V2CAAS        GCPAASa
307300050517     C                   MOVEL     V2CNRS        GCPNRSa
307400050517     C                   MOVEL     V2CNSP        GCPNSPa
307500050517     C                   MOVEL     V2CLNA        GCPLNAa
307600050517    2C                   END
307700040115      *in caso di data consegna richiesta oltre i limiti imposto la
307800050218      *data esecuzione disposizioni calcolando la data richiesta -1 gg lav.
307900090728      *se il flag della data consegna richiesta è diverso da 'D' = Dopo il
308000050512    2c                   if        d05cmc <> *blank and dcr(xdcr) <> *blank
308100040324     C                             AND arbdcr > woggi
308200040115     c                   move      arbdcr        dadata
308300040115     c                   move      arbdcr        adata
308400040115     c                   CALL      'XSRLAV8'
308500040115     c                   PARM                    Wdata8
308600040115     c                   z-add     giolav        giolavs           1 0
308700040115     c                   move      arbdcr        dataiso
308800090728     c                   if        arbtcr <> 'D'
308900050512    3c                   do        *hival
309000040115     c     dataiso       subdur    1:*d          dataiso
309100040115     c                   move      dataiso       dadata
309200040115     c                   move      arbdcr        adata
309300040115     c                   CALL      'XSRLAV8'
309400040115     c                   PARM                    Wdata8
309500050512    4c                   if        giolav = 0 or giolav > giolavs
309600040115     c                   leave
309700050512    4c                   endif
309800050512    3c                   enddo
309900090728     c                   endif
310000040115     c                   move      dataiso       gcpded
310100050405     C                   MOVE      V2CSCM        §QUATR
310200050512    3C     §QUATR        IFEQ      *ALL'8'
310300050405     C     §QUATR        OREQ      *ALL'9'
310400050405     C                   MOVEL     *ZEROS        GCPDDM
310500050405     c                   else
310600050405     C                   MOVEL     woggi         GCPDDM
310700050512    3c                   end
310800050512     c* se devo stampare la comunicazione al mittente forzo la fase
310900050512     c* 20 per poter chiamare il pgm FNLG07R in ristampa
311000050512     c* poi una volta stampata la comunicazione avanzo la fase
311100050513     c                   if        savs2f = 'S' or savf2f = 'N'
311200050512     c                   eval      gcpfas = 20
311300050512     c                   eval      gcpdmm = woggi
311400050513     C     D43TRV        IFEQ      'S'
311500050513     C                   MOVEL     'S'           v2cfst
311600050513     C                   END
311700050512     c                   else
311800050405     C                   z-add     15            GCPfas
311900050512     c                   end
312000050405     C                   MOVE      'C'           GCPDIS
312100050426     C                   MOVE      'S'           GCPapeaut
312200050405     C* imposto campi adebito spese in base al porto bolla
312300050405     c                   exsr      datarich
312400040329      * verifica se devo stampare la LDV
312500070410      * solo se non attivata gestione automatica distinte
312600070424     c                   if        §ogdda = *blank or §ogdda = '20391231' or
312700070424     c                             dateu < ogddan
312800070410    3c                   if        d05cmc = '100'
312900070410     c                   exsr      stampaLDV
313000070410    3c                   endif
313100070410    3c                   endif
313200040517     c                   else
313300040517     c                   z-add     0             gcpded
313400050405     c                   z-add     0             gcpddm
313500050405     C                   MOVE      *BLANKS       GCPDIS
313600050405     C                   MOVE      *BLANKS       GCPPSG
313700050405     C                   MOVE      *BLANKS       GCPASG
313800050405     C                   MOVE      *BLANKS       GCPPPC
313900050405     C                   MOVE      *BLANKS       GCPAPC
314000050512    2c                   endif
314100950414     C* Controllo se devo scrivere evento generico di giac.
314200050512    2C     WWTR          IFEQ      'S'
314300040401     C                   MOVEL     v2ccmc        EVBCEV
314400950414     C                   MOVEL     *BLANKS       EVBATB
314500950414     C                   Z-ADD     WOGGI         EVBDTV
314600950414     C                   TIME                    EVBORV
314700950414     C                   Z-ADD     GCPFGC        EVBFLE
314800021114     C                   Z-ADD     o36cou        EVBCDU
314900950414     C                   Z-ADD     V2CLNP        EVBLNP
315000950414     C                   Z-ADD     V2CAAS        EVBAAS
315100950414     C                   Z-ADD     V2CNRS        EVBNRS
315200950414     C                   Z-ADD     V2CNSP        EVBNSP
315300950414     C                   Z-ADD     WOGGI         EVBDEV
315400950414     C                   MOVEL     EVBORV        EVBHEV
315500950414     C                   MOVEL     *BLANKS       EVBFTR
315600950414     C                   Z-ADD     0             EVBDTR
315700990803     C                   MOVEL     *BLANKS       EVBNOT
315800990803     C                   Z-ADD     0             EVBDTC
315900990803     C                   WRITE     FNEVB000
316000050512    2C                   END
316100050512   x1C                   ELSE
316200050217     C*  Partenza !!!
316300940919     C                   Z-ADD     15            GCPFAS
316400050608     c* metto la fase a 20 se devo anche stampare
316500050608     c                   if        savs2f = 'S' or savf2f = 'N'
316600050608     c                   eval      gcpfas = 20
316700050608     c                   eval      gcpdmm = woggi
316800050608     C     D43TRV        IFEQ      'S'
316900050608     C                   MOVEL     'S'           v2cFST
317000050608     C                   END
317100050608     c                   end
317200050512    1C                   END
317300940919     C                   MOVE      V2CSCM        GCPSCM
317400940919     C                   MOVE      V2CCMC        GCPCMC
317500940919     C                   MOVE      V2CCCR        GCPCCR
317600940919     C                   MOVE      V2CNRC        GCPNRC
317700050512    1C     GCPNRC        IFEQ      0
317800941223     C                   MOVE      GCPNGC        GCPNRC
317900050512    1C                   END
318000941223     C                   MOVE      V2CDXD        G02DAT
318100941223     C                   MOVE      *ZEROS        G02INV
318200941223     C                   MOVE      ' '           G02ERR
318300941223     C                   CALL      'XSRDA8'
318400941223     C                   PARM                    WLBDA8
318500941223     C                   MOVE      G02INV        GCPDXD
318600940919     C                   MOVE      V2CFST        GCPFST
318700940919     C                   MOVE      V2CTCM        GCPTCM
318800050512    1C     GCPFAS        IFNE      20
318900940919     C                   MOVEL     *ZEROS        GCPDMM
319000050512    1C                   END
319100040520      * carico data apertura x confronti
319200040520     c                   clear                   gcpdape           8 0
319300040520     c                   movel     gcpagc        gcpdape
319400040520     c                   move      gcpmgc        gcpdape
319500050512    1c                   if        dcr(xdcr) = *blank or
319600040520     c                             gcpded <= gcpdape or
319700040520     c                             gcpded <= gcpdur
319800950530     C                   MOVEL     *ZEROS        GCPDED
319900050412     C                   MOVE      *BLANK        GCPDIS
320000050413     C                   MOVEL     *BLANK        GCPPPC
320100050413     C                   MOVEL     *BLANK        GCPAPC
320200050413     C                   MOVEL     *BLANK        GCPVCA
320300050413     C                   MOVEL     *ZEROS        GCPCAS
320400050413     C                   MOVEL     *BLANK        GCPNPC
320500050413     C                   MOVEL     *BLANK        GCPVCS
320600050413     C                   MOVEL     *BLANK        GCPPSG
320700050413     C                   MOVEL     *BLANK        GCPASG
320800050512    1c                   end
320900940919     C                   MOVEL     *ZEROS        GCPDLD
321000940919     C                   MOVEL     *ZEROS        GCPFRG
321100940919     C                   MOVEL     *ZEROS        GCPDCG
321200940919     C                   MOVEL     *BLANK        GCPCFG
321300940919     C                   MOVEL     *ZEROS        GCPDLM
321400940919     C                   MOVEL     *ZEROS        GCPSGS
321500940919     C                   MOVEL     *ZEROS        GCPSGR
321600940919     C                   MOVEL     *ZEROS        GCPSGD
321700990625     C                   MOVEL     *BLANKS       GCPDIV
321800940919     C                   MOVEL     *ZEROS        GCPDSD
321900940919     C                   MOVEL     *ZEROS        GCPDSA
322000940919     C                   MOVEL     *ZEROS        GCPGGS
322100940919     C                   MOVEL     *ZEROS        GCPSGF
322200940919     C                   MOVEL     *ZEROS        GCPSGA
322300940919     C                   MOVEL     *ZEROS        GCPTFT
322400940919     C                   MOVEL     *ZEROS        GCPDBG
322500940919     C                   MOVEL     *ZEROS        GCPLPW
322600940919     C                   MOVEL     *ZEROS        GCPASW
322700940919     C                   MOVEL     *ZEROS        GCPNSW
322800940919     C                   MOVEL     *ZEROS        GCPNBW
322900940919     C                   MOVEL     *ZEROS        GCPKSC
323000940919     C                   MOVEL     *ZEROS        GCPDFT
323100940919     C                   MOVEL     *ZEROS        GCPNFT
323200940919     C                   MOVEL     *ZEROS        GCPFIV
323300050217     C                   MOVEL     *BLANK        GCPSede
323400051020     C                   MOVEL     *BLANK        GCPflr
323500050414      *solo in arrivo
323600050512    1c                   if        d05ges = 'A'
323700050217     C                   MOVEL     *ZEROS        GCPDeda
323800050217     C                   MOVEL     *ZEROS        GCPDlma
323900050217     C                   MOVEL     *ZEROS        GCPDuma
324000050217     C                   MOVEL     *ZEROS        GCPLPWa
324100050217     C                   MOVEL     *ZEROS        GCPASWa
324200050217     C                   MOVEL     *ZEROS        GCPNSWa
324300050217     C                   MOVEL     *ZEROS        GCPNBWa
324400050420     C*                  MOVEL     *BLANK        GCPriap
324500050512    1c                   endif
324600940919     c*
324700940919     C* Scrive ulteriore motivo giacenza solo se non è blanks
324800950302     C                   Z-ADD     GCPAGC        KAGC
324900950302     C                   Z-ADD     GCPFGC        KFGC
325000950302     C                   Z-ADD     GCPNGC        KNGC
325100950302     C                   Z-ADD     GCPFRG        KFRG
325200050407     C                   Z-ADD     10            KFAS
325300050217     C     KGNP1         CHAIN     tIGNP01L                           31
325400940919     C                   MOVE      V2CDMC        GNPDMC
325500050512    1C     *IN31         IFEQ      '1'
325600050512    2C     GNPDMC        IFNE      *BLANKS
325700940919     C                   Z-ADD     GCPAGC        GNPAGC
325800940919     C                   Z-ADD     GCPFGC        GNPFGC
325900940919     C                   Z-ADD     GCPNGC        GNPNGC
326000940919     C                   Z-ADD     GCPFRG        GNPFRG
326100940919     C                   MOVEL     *BLANKS       GNPTPN
326200990625     C                   Z-ADD     0             GNPDIF
326300990625     C                   Z-ADD     0             GNPNRI
326400050407     C                   Z-ADD     10            GNPFAS
326500940919     C                   Z-ADD     1             GNPNPR
326600050217     C                   WRITE     TIGNP000
326700050512    2C                   END
326800911206     C                   ELSE
326900050217     C                   UPDATE    tIGNP000
327000050512    1C                   END
327100050218     c* controllo se devo fare il reso forzato per numero giacenze > di
327200050629     c* quello consentito dalla particolarità giacenza escludendo le
327300050629     c* riaperture tecniche
327400050629    1C                   if        gcpriap = 'R' and  gcpcmc <> '999' and
327500050629     C                             §7Q5RP <> 0 and (cnt-cntced-1) >= §7Q5RP
327600050405     C                   Z-ADD     woggi         gcpDMM
327700050218     C                   Z-ADD     26            gcpFAS
327800050809     C                   MOVEl     'M'           gcppsg
327900050809     C                   MOVEl     'S'           gcpasg
328000050809     C                   MOVEl     ' '           gcpppc
328100050809     C                   MOVEl     'N'           gcpapc
328200050809     C                   MOVE      'R'           gcpDIS
328300050512     C                   MOVE      'R'           gcpTCM
328400050218     C                   Z-ADD     0             gcpddm
328500050218     C                   Z-ADD     0             gcpded
328600050218     c                   clear                   gcpapeaut
328700050512    1c                   end
328800050217     C   30              WRITE     tIGCP000
328900050217     C  N30              UPDATE    tIGCP000
329000171010      * solo in arrivo verifica se apertura o riapertura
329100171010     c                   if        d05ges = 'A'
329200171010     c  n30              eval      ILG33CTA = 'R'
329300171010     c   30              eval      ILG33CTA = 'A'
329400171010     c                   eval      ILG33PRU = knmus
329500171010     c                   eval      ILG33NOJ = knmeb
329600171010     c                   eval      ILG33PGM = 'FNLG05R'
329700171010     c   30              clear                   tigcpb
329800171010     c                   eval      bgcpfas = 10
329900171010     c* forzo fase a 0 per scrittura testata riapertura
330000171010     c                   eval      tigcpa = tigcp
330100171010     c                   call      'FNLG33R'
330200171010     c                   parm                    fnlg33ds
330300171010     c                   parm                    tigcpb
330400171010     c                   parm                    tigcpa
330500171010      *se ho scritto un rekord di riapertura ma ho disposizioni forzate di reso
330600171010      * scrivo anche una riga in modifica per evidenziare
330700171010     c                   if        ilg33cta = 'R'
330800171010     c                             and gcpfas  < 35
330900171010     c                             and gcpfas  > 20
331000171010     c                   eval      bgcpfas = 10
331100171010     c                   eval      agcpfas = gcpfas
331200171010     c                   eval      ilg33cta = 'M'
331300171010     c                   call      'FNLG33R'
331400171010     c                   parm                    fnlg33ds
331500171010     c                   parm                    tigcpb
331600171010     c                   parm                    tigcpa
331700171010     c                   endif
331800171010      * partenza
331900171010     c                   else
332000171010     c                   if        bgcpfas = 10
332100171010     c                   eval      agcpfas = gcpfas
332200171010     c                   eval      ilg33cta = 'M'
332300171010     c                   eval      ILG33PRU = knmus
332400171010     c                   eval      ILG33NOJ = knmeb
332500171010     c                   eval      ILG33PGM = 'FNLG05R'
332600171010     c                   eval      tigcpa = tigcp
332700171010     c                   call      'FNLG33R'
332800171010     c                   parm                    fnlg33ds
332900171010     c                   parm                    tigcpb
333000171010     c                   parm                    tigcpa
333100171010     c                   endif
333200171010     c                   endif
333300070320      * se impostato flag abilitazione in anagrafica verifico se esistono
333400070321      * dei blocchi nel caso updato il rekord per abblencarli
333500070320     c                   if        §oggeot = 'S' and d05ges = 'A'
333600070321     c                   exsr      elibloc
333700070320     c                   endif
333800050614     C*  Stampo la comunicazione al mittente se se previsto nella tab. 2f
333900050608    1C                   if        gcpfas = 20
334000050512     c                   exsr      srstampa
334100170925      * richiamo forzato a pgm monitor variazioni per avanzamento automatico
334200170925      * fase e invio mail(forzo fase 15 rekord BEFORE) per scrivere variazione
334300171013     c                   if        d05ges = 'A'
334400170925     c                   eval      ILG33CTA = 'M'
334500170925     c                   eval      ILG33PRU = knmus
334600170925     c                   eval      ILG33NOJ = knmeb
334700170925     c                   eval      ILG33PGM = 'FNLG05R'
334800170925     c                   eval      tigcpb = tigcp
334900170925     c                   eval      bgcpfas = 15
335000170925     c                   eval      tigcpa = tigcp
335100170925     c                   call      'FNLG33R'
335200170925     c                   parm                    fnlg33ds
335300170925     c                   parm                    tigcpb
335400170925     c                   parm                    tigcpa
335500171013    1c                   endif
335600050512    1c                   end
335700050512     C*  Scrivo il FIGIC00F per aggiornare titas e tncsb solo per APERTURA
335800050512    1c                   if        *in30 and d05ges = 'A' and gcpriap = ' '
335900050516     c                             and not *in28
336000050512     c                   exsr      gicwtr
336100050512    1c                   end
336200950622     C*  Partenza !!
336300050512    1C     D05GES        IFEQ      'P'
336400050217     C     KSPE1         CHAIN     tIGAP01L                           31
336500940919     C                   MOVE      V2CSCM        §QUATR
336600050512    2C     §QUATR        IFEQ      *ALL'8'
336700900423     C     §QUATR        OREQ      *ALL'9'
336800940919     C                   MOVEL     V2CLNP        GAPLNP
336900940919     C                   MOVEL     V2CAAS        GAPAAS
337000940919     C                   MOVEL     V2CNRS        GAPNRS
337100940919     C                   MOVEL     V2CNSP        GAPNSP
337200940919     C                   MOVEL     V2CRSM        GAPRSM
337300940919     C                   MOVEL     V2CINM        GAPINM
337400940919     C                   MOVEL     V2CLOM        GAPLOM
337500940919     C                   MOVEL     V2CCAM        GAPCAM
337600940919     C                   MOVEL     V2CPRM        GAPPRM
337700050217     C  N31              UPDATE    tIGAP000
337800050217     C   31              WRITE     tIGAP000
337900050512   x2C                   ELSE
338000050217     C  N31              DELETE    tIGAP000
338100050512    2C                   END
338200050608     C*  Se bolla estero oppure se ho stampato la comunicazione al mit.
338300050608     C*  sfleggo la bolla per trasmettere informazione al cliente
338400050608c    c                   if        lnpfl1 = 'E' or
338500050615c    c                             (§3kcq5 = 'S' and savv2f <>'N' and
338600050615     C                             (§3KGQI = 'A' or §3KGQI ='E'))
338700050217     C     KSPE1         CHAIN     FNBLP01L                           31
338800050512    3C     *IN31         IFEQ      '0'
338900050217     C                   MOVEL     *BLANKS       BLPFT3
339000050217     C                   Z-ADD     0             BLPDT3
339100961204     C                   EXCEPT    SFLEGG
339200050512    3C                   END
339300050512    2C                   END
339400961204     C*
339500050512    1C                   END
339600921103     C*
339700050315      * gestione del commit se il flag
339800050516    1c                   if        gescomm = 'S'
339900050516     c                   if        not *in28
340000050315     c                   commit
340100050516     c                   else
340200050516     c                   rolbk
340300050512    1c                   endif
340400050516    1c                   endif
340500050315     C*
340600900418     C                   ENDSR
340700070321      *---------------------------------------------------------*
340800070321      *  elimina blocchi alla consegna da bolla data x consegnata
340900070321      *---------------------------------------------------------*
341000070321     c     elibloc       begsr
341100070321     C                   clear                   dsbl4l
341200070321     C                   move      'L'           KTRC
341300070321     C     Kar4l         chain     fiar401l
341400070321     C                   if        %found(fiar401l)
341500070321     C                   movel     ar4not        dsbl4l
341600070321     c                   move      *blank        §b4lbas
341700070321     c                   move      *blank        §b4lbge
341800070521      * solo se contrassegno già abilitato
341900070521     c                   if        arbacc = 'S'
342000070321     c                   move      *blank        §b4ltca
342100070521     c                   endif
342200070321     c                   move      *blank        §b4ltfd
342300070321     C                   movel     dsbl4l        ar4not
342400070321     c                   update    fiar4000
342500070321     C                   endif
342600070321     c
342700070321     C                   endsr
342800040329     C*--------------------------------------------------------------*
342900040329     C*  Richiamo pgm stampa lettera di vettura per ddt no mai stampati
343000040329     C*--------------------------------------------------------------*
343100040329     C     stampaldv     BEGSR
343200040329     c                   if        ovr = *blank
343300040329     c                   z-add     99            lung
343400040329      * eseguo ovrprtf - A4
343500040329     C                   movea(P)  cma4(1)       w99
343600040329     C                   movea     d05prb4       w99(24)
343700040329     C                   movea     d05mdb4       w99(46)
343800040329     C                   movea     w99           comman
343900040329     c                   move      'SHARE(*YES)' comman
344000040329     C                   call      'QCMDEXC'
344100040329     C                   parm                    comman
344200040329     C                   parm                    lung
344300040329      * eseguo ovrprtf - A5
344400040329     C                   movea(P)  cma5(1)       w99
344500040329     C                   movea     d05prb5       w99(24)
344600040329     C                   movea     d05mdb5       w99(46)
344700040329     C                   movea     w99           comman
344800040329     c                   move      'SHARE(*YES)' comman
344900040329     C                   call      'QCMDEXC'
345000040329     C                   parm                    comman
345100040329     C                   parm                    lung
345200040329     c                   move      'X'           ovr               1
345300040329     c                   endif
345400040329      *
345500040329     C                   clear                   dsbl4a
345600040329     C                   movel     'A'           ktrc
345700040329     C                   movel     *blanks       ar4not
345800060213     c     kar4          chain(n)  fiar401l                           31
345900040329     c                   if        not *in31
346000040329     C                   movel     ar4not        dsbl4a
346100040329     c                   if        §b4abm = 'N' or §b4abm = 'K'
346200130315     c                   reset                   FNLSB6ds1
346300040329     C                   CLEAR                   fnlsb5ds
346400040329     C                   z-add     kaas          DB0AAS
346500040329     C                   z-add     klnp          DB0LNP
346600040329     C                   z-add     knrs          DB0NRS
346700040329     C                   z-add     knsp          DB0NSP
346800040329     C                   MOVEL     'A'           DB0TBO
346900040329     C                   MOVEL     ' '           DB0RIS
347000040329     c                   call      D05psl
347100040329     c                   parm                    fnlsb5ds
347200130315     c                   parm                    FNLSB6ds1
347300060213     C     KAR4          CHAIN     FiAR401L                           31
347400040329     c                   if        not *in31
347500040329     c                   movel     ar4not        dsbl4a
347600040329     c                   Select
347700040329     c                   When      §B4Abm = 'N'
347800040329     c                   Eval      §B4Abm = 'J'
347900040329     c                   When      §B4Abm = 'K'
348000040329     c                   Eval      §B4Abm = 'W'
348100040329     c                   EndSl
348200040329
348300040329     c                   z-add     woggi         §b4asl
348400040329     c                   movel     dsbl4a        ar4not
348500060213     C                   update    FiAR4000
348600040329      *
348700040329     c                   endif
348800040329     c                   endif
348900040329     c                   endif
349000040329     C                   ENDSR
349100970401     C*--------------------------------------------------------------*
349200970401     C*  Call a pgm Chiusura anomalia 130
349300970401     C*--------------------------------------------------------------*
349400970401     C     CLO130        BEGSR
349500970401     C*
349600970401     C*  Se ho trovato i segnacolli vado a chiudere le anomalie
349700970401     C                   CLEAR                   DSLR33
349800970401     C                   MOVE      '4'           D33FSC
349900970401     C                   Z-ADD     WOGGI         D33DCH
350000970401     C*
350100970401     C                   Z-ADD     V2CLNP        KLNP2
350200970401     C                   Z-ADD     V2CNRS        KNRS2
350300970401     C                   Z-ADD     V2CNSP        KNSP2
350400970401     C                   Z-ADD     V2CAAS        KAAS2
350500970401     C                   Z-ADD     130           KCAA
350600990518     C     KANM          CHAIN     FNANM01L                           37
350700970401     C     *IN37         DOWEQ     '0'
350800970401     C     ANMFSA        IFEQ      'G'
350900970401     C                   Z-ADD     ANMNR1        D33NRR
351000970401     C                   CALL      'FNLR33R'
351100970401     C                   PARM                    DSLR33
351200970401     C                   END
351300990518     C     KANM          READE     FNANM01L                               37
351400970401     C                   END
351500970401     C*
351600970401     C                   ENDSR
351700961028     C*---------------------------------------------------------------*
351800050512     C* srstampa - stampa comincazione al mittente per le autoconfer. *
351900961028     C*---------------------------------------------------------------*
352000050512     C     srstampa      BEGSR
352100050516     c                   do
352200050512     c* se previsto (tab. 2F) chiamo anche il pgm di scrittura del VAG
352300050513     C                   if        savv2f <>'N' and (§3KGQI = 'A' or
352400050512     C                             §3KGQI ='E')
352500050512     c                   clear                   fnlg08ds
352600050512     c                   eval      D08AGC= gcpagc
352700050512     c                   eval      D08FGC= gcpfgc
352800050512     c                   eval      D08NGC= gcpngc
352900050512     c                   eval      D08uni= §3kgks
353000050606     c                   eval      D08GSD= §3kGSD
353100050513     c                   eval      D08est= save2f
353200050516     c                   eval      D08commit= 'N'
353300050608     c                   eval      D08pgm= 'FNLG05R'
353400050512     c                   movel(p)  fnlg08ds      kpjbu
353500050516     c                   call      'FNLG08R'                            28
353600050512     c                   parm                    kpjba
353700050516     c                   movel     kpjbu         fnlg08ds
353800050516     c                   if        d08ok = 'N'
353900050516     c                   seton                                        28
354000050516     c                   end
354100050516     c                   if        *in28
354200050516     c                   leave
354300050516     c                   end
354400050516     c                   end
354500050516     C* lancio il pgm di stampa in modalità ristampa
354600050516     c                   if        savs2f = 'S'
354700050516     c                   clear                   fnlg07ds
354800050516     c                   eval      D07RIS= 'S'
354900050516     c                   eval      D07RAC= 'N'
355000050516     c                   eval      D07AGC= gcpagc
355100050516     c                   eval      D07FGC= gcpfgc
355200050516     c                   eval      D07NGC= gcpngc
355300050518     c* attenzione al parametri d07fil: per fax e posta non è impostato
355400050518     c* correttamente verificare il pgm FNLG07R
355500050518     c                   eval      D07fil= gcplnp
355600050516     c                   eval      D07tip= v2ctcm
355700050516     c                   movel(p)  fnlg07ds      kpjbu
355800050516     c                   call      'FNLG07R'                            28
355900050516     c                   parm                    kpjba
356000050516     c                   movel     kpjbu         fnlg07ds
356100050516     c                   if        d07ok = 'N'
356200050516     c                   seton                                        28
356300050516     c                   end
356400050516     c                   if        *in28
356500050516     c                   leave
356600050516     c                   end
356700050516     c                   end
356800170214      *richiamo pgm invio avviso al destinatario
356900170214     c                   if        gcpriap = *blank
357000170214     c                   clear                   fnlg071ds
357100170214     c                   movel     gcpAAS        i071AAS
357200170214     c                   movel     gcpLNP        i071LNP
357300170214     c                   movel     gcpNRS        i071NRS
357400170214     c                   move      gcpNSP        i071NSP
357500170214     c                   movel     gcpLNa        i071LNa
357600170214     c                   move      'A'           i071CHI
357700170214     c                   movel     gcpcmc        i071cmc
357800170214     c                   call      'FNLG07R1'                           28
357900170214     c                   parm                    kpjba
358000170214     c                   parm                    fnlg071ds
358100170214     c                   endif
358200050516     c* per data consegna richiesta devo avanzare la fase a seconda
358300050516     c* che sia un codificato oppure no
358400050516     c                   if        dcr(xdcr) <> ' '  and
358500050516     c                             gcpdis <> *blank and gcpdis <> 'R'
358600050516     c* non ho ancora committato quindi il record è ancora mio
358700051125     C                   Z-ADD     0             Kfrg
358800051125     c                   if        d05ges = 'P'
358900051125     C                   Z-ADD     gcpAAS        KAAS
359000051125     C                   Z-ADD     gcpLNP        KLNP
359100051125     C                   Z-ADD     gcpNRS        KNRS
359200051125     C                   Z-ADD     gcpNSP        KNSP
359300051125     c                   else
359400051125     C                   Z-ADD     gcpAASa       KAAS
359500051125     C                   Z-ADD     gcpLNPa       KLNP
359600051125     C                   Z-ADD     gcpNRSa       KNRS
359700051125     C                   Z-ADD     gcpNSPa       KNSP
359800051125     c                   end
359900050516     C     KSPE2         CHAIN     tIGCP01L
360000170922     c                   eval      tigcpa = tigcp
360100050606     C                   MOVE      GCPSCM        §QUATR
360200050516    3C     §QUATR        IFEQ      *ALL'8'
360300050516     C     §QUATR        OREQ      *ALL'9'
360400050516     C                   Z-ADD     25            GCPFAS
360500050516     c                   else
360600050516     c                   z-add     30            gcpfas
360700050516     c                   end
360800050516     C                   UPDATE    tIGCP000
360900170922     c                   eval      tigcpb = tigcp
361000170922     c                   eval      ILG33CTA = 'M'
361100170922     c                   eval      ILG33PRU = knmus
361200170922     c                   eval      ILG33NOJ = knmeb
361300170922     c                   eval      ILG33PGM = 'FNLG05R'
361400170922     c                   eval      tigcpa = tigcp
361500170922     c                   call      'FNLG33R'
361600170922     c                   parm                    fnlg33ds
361700170922     c                   parm                    tigcpb
361800170922     c                   parm                    tigcpa
361900050516     c                   end
362000050516     C                   enddo
362100050516     c*
362200050512     C                   ENDSR
362300050512     C*---------------------------------------------------------------*
362400050512     C*   Caricamento £6-£7                                           *
362500050512     C*---------------------------------------------------------------*
362600050512     C     CAR£6         BEGSR
362700961028     C* Carico £7 se siamo in partenza, carico £6 se siamo in arrivo
362800961028     C     D05GES        IFEQ      'P'
362900961028     C                   MOVEL     '£7'          KCOD
363000961028     C                   CLEAR                   KEY
363100961028     C                   MOVEL     *BLANKS       KKEY
363200961028     C                   MOVEL     §FLE          KKEY
363300961028     C                   Z-ADD     0             §F
363400961028     C     KTBL1         CHAIN     TABEL                              31
363500961028     C     *IN31         IFEQ      '0'
363600961028     C     TBLFLG        ANDEQ     ' '
363700961028     C                   MOVEA     TBLUNI        FI
363800020430     C                   DO        30            H                 2 0
363900961028     C     FI(H)         IFGT      *ZERO
364000961028     C                   ADD       1             §F
364100961028     C                   MOVE      FI(H)         KEY(§F)           3 0
364200961028     C                   END
364300961028     C                   END
364400961028     C                   ELSE
364500961028     C                   Z-ADD     1             §F                3 0
364600961028     C                   MOVE      §FLE          KEY(§F)
364700961028     C                   END
364800020430     C                   ELSE
364900961028     C*
365000020430     c                   clear                   trul06ds
365100020430     c                   eval      d06cod = '£6'
365200020430     c                   movel     §fle          d06key
365300021106     c                   movel     kpjbu         savkpjbu
365400020430     c                   movel     trul06ds      kpjbu
365500020430     c                   call      'TRUL06R'
365600020430     c                   parm                    kpjba
365700020430     c                   movel     kpjbu         trul06ds
365800020430     c                   movea     lin           key
365900021106     c                   movel     savkpjbu      kpjbu
366000020430     C                   END
366100020430     C*
366200961028     C                   ENDSR
366300940915     C*---------------------------------------------------------------*
366400940915     C*   Caricamento tabelle                                         *
366500940915     C*---------------------------------------------------------------*
366600940915     C     CARTAB        BEGSR
366700940915     C*---------------------------------------------------------------*
366800961028     C                   EXSR      CAR£6
366900961017     C*
367000961025     C* Carico £1
367100020430     c                   clear                   trul06ds
367200020430     c                   eval      d06cod = '£1'
367300020430     c                   movel     simfel        d06key
367400021106     c                   movel     kpjbu         savkpjbu
367500020430     c                   movel     trul06ds      kpjbu
367600020430     c                   call      'TRUL06R'
367700020430     c                   parm                    kpjba
367800020430     c                   movel     kpjbu         trul06ds
367900020430     c                   movea     lin           £1
368000021106     c                   movel     savkpjbu      kpjbu
368100990805     C*---------------------------------------------------------------*
368200990805     C* Carico 2G
368300990805     C                   MOVEL     '2G'          KCOD
368400990805     C                   MOVEL     *BLANKS       KKEY
368500990805     C                   MOVEL     '1'           KKEY
368600990805     C                   Z-ADD     0             §F
368700990805     C     KTBL1         CHAIN     TABEL                              31
368800990805     C     *IN31         IFEQ      '0'
368900990805     C     TBLFLG        ANDEQ     ' '
369000990805     C                   MOVEL     TBLUNI        DS2G
369100990805     C                   END
369200940915     C*---------------------------------------------------------------*
369300940915     C* Carico tabella 3a
369400071107     C                   Z-ADD     0             X
369500940915     C                   MOVEL     '3A'          KCOD
369600940915     C     KTBL2         CHAIN     TABEL                              31
369700940915     C     *IN31         DOWEQ     '0'
369800940915     C     X             ANDLT     50
369900940915     C     TBLFLG        IFEQ      ' '
370000940919     C                   MOVEL     TBLUNI        DS3A
370100940915     C                   ADD       1             X
370200940915     C                   MOVEL     TBLKEY        C3A(X)
370300940915     C                   MOVEL     §3ADES        D3A(X)
370400940915     C                   MOVEL     §3ATB1        T3A(X)
370500021031     C                   MOVEL     §3Absd        g3A(X)
370600940915     C                   END
370700940915     C     KTBL2         READE     TABEL                                  31
370800940915     C                   END
370900940915     C*  Se non ho caricato tutti i dati gestisco errore
371000940915     C     *IN31         IFNE      '1'
371100940915     C                   EXSR      ERRTAB
371200940915     C                   GOTO      FINTAB
371300940915     C                   END
371400940915     C*---------------------------------------------------------------*
371500940915     C* Carico tabella 2a
371600071107     C                   Z-ADD     0             X
371700940915     C                   MOVEL     '2A'          KCOD
371800940915     C     KTBL2         CHAIN     TABEL                              31
371900940915     C     *IN31         DOWEQ     '0'
372000080903     C     X             ANDLT     200
372100940915     C     TBLFLG        IFEQ      ' '
372200940919     C                   MOVEL     TBLUNI        DS2A
372300940915     C                   ADD       1             X
372400940915     C                   MOVEL     TBLKEY        C2A(X)
372500940915     C                   MOVEL     §2ADES        D2A(X)
372600950105     C                   MOVEL     §2AGLA        G2A(X)
372700950105     C                   MOVEL     §2AFLG        F2A(X)
372800950414     C                   MOVEL     §2AFTC        T2A(X)
372900940919     C                   END
373000940915     C     KTBL2         READE     TABEL                                  31
373100940915     C                   END
373200940915     C*  Se non ho caricato tutti i dati gestisco errore
373300940915     C     *IN31         IFNE      '1'
373400940915     C                   EXSR      ERRTAB
373500940915     C                   GOTO      FINTAB
373600940915     C                   END
373700950104     C*  Carico tabella correlata 2Z
373800950104     C                   MOVEL     '2Z'          KCOD
373900950104     C     KTBL2         CHAIN     TABEL                              31
374000950104     C     *IN31         DOWEQ     '0'
374100950104     C                   MOVEL     TBLUNI        DS2A
374200950104     C     TBLFLG        IFEQ      ' '
374300071107     C                   Z-ADD     1             X
374400950104     C                   MOVEL     TBLKEY        WCD2A             3
374500950104     C     WCD2A         LOOKUP    C2A(X)                                 32
374600950104     C     *IN32         IFEQ      '1'
374700950104     C                   MOVEL     TBLUNI        DS2Z
374800950104     C                   MOVE      §2ZTIG        TPG(X)
374900040115     C                   MOVE      §2Zdcr        dcr(X)
375000950104     C                   END
375100950104     C                   END
375200950104     C     KTBL2         READE     TABEL                                  31
375300950104     C                   END
375400940915     C*---------------------------------------------------------------*
375500940915     C* Carico tabella 2b
375600980105     C                   Z-ADD     *HIVAL        C2B
375700071107     C                   Z-ADD     0             X
375800940915     C                   MOVEL     '2B'          KCOD
375900940915     C     KTBL2         CHAIN     TABEL                              31
376000940915     C     *IN31         DOWEQ     '0'
376100940915     C     X             ANDLT     50
376200940915     C     TBLFLG        IFEQ      ' '
376300940915     C                   ADD       1             X
376400940915     C                   MOVEL     TBLKEY        C2B(X)
376500940915     C                   MOVEL     TBLUNI        D2B(X)
376600940915     C                   END
376700940915     C     KTBL2         READE     TABEL                                  31
376800940915     C                   END
376900940915     C*  Se non ho caricato tutti i dati gestisco errore
377000940915     C     *IN31         IFNE      '1'
377100940915     C                   EXSR      ERRTAB
377200940915     C                   GOTO      FINTAB
377300940915     C                   END
377400940915     C*---------------------------------------------------------------*
377500940915     C* Carico tabella 2f
377600970110     C                   MOVEA     *HIVAL        C2F
377700071107     C                   Z-ADD     0             X
377800940915     C                   MOVEL     '2F'          KCOD
377900940915     C     KTBL2         CHAIN     TABEL                              31
378000940915     C     *IN31         DOWEQ     '0'
378100940915     C     X             ANDLT     50
378200940915     C     TBLFLG        IFEQ      ' '
378300031027     C                   MOVEL     TBLUNI        DS2F
378400031027      * solo se non è riservato al CED lo carica
378500050614     c*                  if        §2Fris = *blank
378600940915     C                   ADD       1             X
378700940915     C                   MOVEL     TBLKEY        C2F(X)
378800940919     C                   MOVEL     §2FDES        D2F(X)
378900940920     C                   MOVEL     §2FFTM        F2F(X)
379000960201     C                   MOVEL     §2FFTP        T2F(X)
379100050512     C                   MOVEL     §2Fsta        s2F(X)
379200050512     C                   MOVEL     §2FVAG        V2F(X)
379300050512     C                   MOVEL     §2Ftxt        e2F(X)
379400050614     C                   MOVEL     §2Fris        r2F(X)
379500050614     c*                  end
379600940915     C                   END
379700940915     C     KTBL2         READE     TABEL                                  31
379800940915     C                   END
379900940915     C*  Se non ho caricato tutti i dati gestisco errore
380000940915     C     *IN31         IFNE      '1'
380100940915     C                   EXSR      ERRTAB
380200940915     C                   GOTO      FINTAB
380300940915     C                   END
380400940920     C*---------------------------------------------------------------*
380500940920     C* Carico tabella 7Q
380600071107     C                   Z-ADD     0             X
380700940920     C                   MOVEL     '7Q'          KCOD
380800940920     C     KTBL2         CHAIN     TABEL                              31
380900940920     C     *IN31         DOWEQ     '0'
381000940920     C     X             ANDLT     50
381100940920     C     TBLFLG        IFEQ      ' '
381200940920     C                   ADD       1             X
381300940920     C                   MOVEL     TBLKEY        C7Q(X)
381400950102     C                   MOVEL     TBLUNI        D7Q(X)
381500940920     C                   END
381600940920     C     KTBL2         READE     TABEL                                  31
381700940920     C                   END
381800940920     C*  Se non ho caricato tutti i dati gestisco errore
381900940920     C     *IN31         IFNE      '1'
382000940920     C                   EXSR      ERRTAB
382100940920     C                   GOTO      FINTAB
382200940920     C                   END
382300021031     C*---------------------------------------------------------------*
382400021031     C* Carico tabella 7w
382500071107     C                   Z-ADD     0             X
382600021031     C                   MOVEL     '7W'          KCOD
382700021031     C     KTBL2         CHAIN     TABEL                              31
382800021031     C     *IN31         DOWEQ     '0'
382900021031     C     X             ANDLT     50
383000021031     C     TBLFLG        IFEQ      ' '
383100021031     C                   ADD       1             X
383200021031     C                   MOVEL     TBLKEY        C7w(X)
383300021031     C                   MOVEL     TBLUNI        D7w(X)
383400021031     C                   END
383500021031     C     KTBL2         READE     TABEL                                  31
383600021031     C                   END
383700021031     C*  Se non ho caricato tutti i dati gestisco errore
383800021031     C     *IN31         IFNE      '1'
383900021031     C                   EXSR      ERRTAB
384000021031     C                   GOTO      FINTAB
384100021031     C                   END
384200950113     C*---------------------------------------------------------------*
384300950113     C* Carico CAMBI/DIVISE
384400011221     C                   Z-ADD     0             Y
384500950116     C                   MOVEL     '15'          KCOD
384600950113     C     KTBL2         CHAIN     TABEL                              31
384700950113     C     *IN31         DOWEQ     '0'
384800011221     C     Y             ANDLT     500
384900950113     C     TBLFLG        IFEQ      *BLANK
385000011221     C                   ADD       1             Y
385100950113     C                   MOVEL     TBLUNI        DS15
385200011221     C                   MOVEL     TBLKEY        CNZ(Y)
385300011221     C                   MOVEL     §15ITA        ITA(Y)
385400950113     C                   END
385500950113     C     KTBL2         READE     TABEL                                  31
385600950113     C                   END
385700940915     C*---------------------------------------------------------------*
385800940915     C*  Se non ho caricato tutti i dati gestisco errore
385900940915     C     *IN31         IFNE      '1'
386000940915     C                   EXSR      ERRTAB
386100940915     C                   END
386200090520     C*---------------------------------------------------------------*
386300090520     C* Carico tabella 5e
386400090520     C                   Z-ADD     0             X
386500090520     C                   MOVEL     '5E'          KCOD
386600090520     C     KTBL2         CHAIN     TABEL                              31
386700090520     C     *IN31         DOWEQ     '0'
386800090520     C     X             ANDLT     50
386900090520     C     TBLFLG        IFEQ      ' '
387000090520     C                   ADD       1             X
387100090520     C                   MOVEL     TBLKEY        C5e(X)
387200090520     C                   MOVEL     TBLUNI        D5e(X)
387300090520     C                   END
387400090520     C     KTBL2         READE     TABEL                                  31
387500090520     C                   END
387600090520     C*  Se non ho caricato tutti i dati gestisco errore
387700090520     C     *IN31         IFNE      '1'
387800090520     C                   EXSR      ERRTAB
387900090520     C                   GOTO      FINTAB
388000090520     C                   END
388100940915     C     FINTAB        ENDSR
388200091021     C*---------------------------------------------------------------*
388300091021     C* verifica se cod.cliente da forzare
388400091021     C*---------------------------------------------------------------*
388500091021     C     forzacli      BEGSR
388600091021     c                   if        d05ges = 'P'
388700091021     C     kar5p         chain     fiar501l
388800091021     c                   else
388900091021     C     kar5          chain     fiar501l
389000091021     c                   endif
389100091021     c                   if        %found(fiar501l)
389200091021     c                   movel     ar5uni        dar5gen
389300091021     c                   if        §ar5ass = 'T'
389400091021     c                   clear                   tibs02ds
389500091021     c                   movel     'C'           t02mod
389600091021     c                   movel     knsif         t02sif
389700091021     c                   movel     'CLI'         t02cod
389800091021     c                   if        d05ges = 'P'
389900091102     c                   movel     gcpscm        t02ke1
390000091021     c                   else
390100091021     c                   movel     arbksc        t02ke1
390200091021     c                   endif
390300091021     c                   clear                   t02ke2
390400091021     c                   call      'TIBS02R'
390500091021     c                   parm                    KPJBA
390600091021     c                   parm                    TIBS02DS
390700091021     c                   if        t02err = *blanks
390800091021     c                   movel     t02uni        dcli
390900091021     c                   if        §cligcp = 'S'
391000091021     c                   if        d05ges = 'P'
391100091102     c                   movel     gcpscm        v2cscm
391200091102     c                   movel     gcpscm        kksc
391300091021     c                   else
391400091102     c                   movel     arbksc        v2cscm
391500091102     c                   movel     arbksc        kksc
391600091021     c                   endif
391700091021     c                   endif
391800091021     c                   endif
391900091021     c                   endif
392000091021     c                   endif
392100091021     c
392200091021     c                   endsr
392300050217     C*---------------------------------------------------------------*
392400050218     C* Srcivo figic00f per allineare saltuariamente la sede (TITAS+TNCSB)
392500940915     C*---------------------------------------------------------------*
392600050218     C     gicwtr        BEGSR
392700050218     C                   MOVEL     'APGC'        GICCAV
392800050408     C                   Z-ADD     woggi         GICDTV
392900050218     C                   TIME                    GICORV
393000050411     C                   Z-ADD     GCPLNPa       GICFEV
393100050411     C                   Z-ADD     GCPLNPa       GICLNP
393200050411     C                   Z-ADD     GCPLNAa       GICLNA
393300050411     C                   Z-ADD     GCPAASa       GICAAS
393400050411     C                   Z-ADD     GCPNRSa       GICNRS
393500050411     C                   Z-ADD     GCPNSPa       GICNSP
393600050218     C                   MOVEL     GCPAGC        GICDGC
393700050218     C                   MOVE      GCPMGC        GICDGC
393800050218     C                   Z-ADD     GCPFGC        GICFGC
393900050218     C                   Z-ADD     GCPNGC        GICNGC
394000050218     C                   Z-ADD     0             GICDCH
394100050218     C                   WRITE     FIGIC000
394200050218     c
394300050218     C                   ENDSR
394400050405     C**---------------------------------------------------------
394500050405     c     datarich      begsr
394600050405     C**---------------------------------------------------------
394700050405     C                   MOVE      '2Z'          KCOD
394800050405     C                   MOVEL(p)  V2Ccmc        KKEY
394900050405     C     KTBL1         CHAIN     TABEL00F
395000050405     C                   IF        %found(tabel00f)
395100050405     C                   MOVEL     TBLUNI        DS2z
395200050405     c                   if        §2zdcr <> *blank
395300050405     c                   move      'X'           azzer             1
395400050405      * controllo il tipo bolla per l'attribuzione delle spese e provvigioni
395500050405     C                   MOVE      '3A'          KCOD
395600050405     C                   MOVEL(p)  blpcbo        kKEY
395700050405     C     KTbl1         CHAIN     TABEL00F
395800050405     C                   IF        %found(tabel00f)
395900050405     C                   MOVEL     TBLUNI        DS3a
396000050405     C                   MOVEL     §3ATB1        PORT              1
396100050405      * spese trasporto
396200050405     c                   if        port = 'F'
396300050405     c                   move      'M'           gcppsg
396400050405     c                   move      'S'           gcpasg
396500050405     c                   else
396600050405     c                   move      'D'           gcppsg
396700050405     c                   move      'S'           gcpasg
396800050405     c                   endif
396900050405      * provv. contrassegno
397000051116     c     kSPE1         chain     fiar901l
397100051116     c                   if        %found(fiar901l)
397200050405     c                   if        port = 'F'
397300050405     c                   if        §3atb2 <> *blank
397400050405     c                   move      'D'           gcpppc
397500050405     c                   move      'S'           gcpapc
397600050405     c                   else
397700050405     c                   move      'M'           gcpppc
397800050405     c                   move      'S'           gcpapc
397900050405     c                   endif
398000050405     c                   else
398100050405     c                   move      'D'           gcpppc
398200050405     c                   move      'S'           gcpapc
398300050405     c                   endif
398400050405      *no contrassegno
398500050405     c                   else
398600050405     c                   clear                   gcpppc
398700050405     c                   clear                   gcpapc
398800050405     c                   endif
398900050405
399000050405     C                   END
399100050405     C                   END
399200050405     C                   END
399300050405     c                   endsr
399400050218     C*---------------------------------------------------------------*
399500050218     C*   FNCLS + 3K                                                  *
399600050218     C*---------------------------------------------------------------*
399700050218     C     srcli         BEGSR
399800050217     c*
399900050217     C                   MOVEL     *BLANKS       CLSFLO
400000050401     C                   clear                   ds2u
400100050217     C     KKSC          CHAIN     FNCLS01L                           31
400200050217     C     CLSFLO        IFNE      *BLANKS
400300050217     c* se non ho trovato il tipo di comunicazione nelle tariffe
400400050217     c                   if        WTRTCM = 'N'
400500050217     C                   MOVE      CLSFLO        V2CTCM
400600050217     c                   end
400700050217     c* apertura giacenza autoconfermata
400800050401     c                   eval      kkey= %subst(clsflo: 9: 1)
400900050401     C                   MOVEL     '2U'          KCOD
401000050401     C     KTBL1         CHAIN     TABEL00f
401100050401     C                   IF        %found(tabel00f) and tblflg = ' '
401200050401     C                   MOVEl     TBLUNI        ds2u
401300050401     c                   else
401400050401     c                   eval      §2uape='N'
401500050401     C                   END
401600050217     C                   END
401700050606     C* SE PREVISTO IN TABELLA INVIO INFORMAZIONI BOLLE GIACENTI AL CLIENTI
401800050608     C* SFLEGGO LA BOLLA se sono in aggiornamento
401900050606     C                   MOVEL     '3K'          kCOD
402000050606     C                   MOVEL     *BLANKS       kKEY
402100050606     C     BLPCCM        IFNE      0
402200050606     C                   MOVEL     BLPCCM        kKEY
402300050606     C                   ELSE
402400050606     C                   MOVEL     BLPKSC        kKEY
402500050606     C                   END
402600050606     C     KTbl1         CHAIN     TABEL                              35
402700050606     C*
402800050606     C  n35              MOVEL     TBLUNI        DS3K
402900050606     C   35              clear                   DS3K
403000050608     C     d05ges        ifeq      'A'
403100050606     C     §3KCQ5        IFEQ      'S'
403200050608     C     flgagg        andeq     1
403300050606     C                   MOVEL     *BLANKS       BLPFT3
403400050606     C                   Z-ADD     0             BLPDT3
403500050606     C                   EXCEPT    SFLEGG
403600050608     c                   endif
403700050513     c* reperisco i dati della tabella 2F
403800050513     c                   clear                   savs2f            1
403900050513     c                   clear                   savf2f            1
404000050513     c                   clear                   savv2f            1
404100050513     c                   clear                   save2f            1
404200050513     c                   if        v2ctcm <> ' '
404300050517     c                   z-add     1             x
404400050513     C     V2CTCM        LOOKUP    C2F(X)                                 32
404500050513     c                   if        *in32
404600050513     c                   eval      savs2f = s2f(x)
404700050513     c                   eval      savf2f = f2f(x)
404800050513     c                   eval      savv2f = v2f(x)
404900050513     c                   eval      save2f = e2f(x)
405000050614      * reperimento luogo e ind. e-mail  mi serve per impostare gcpfst
405100050614     C                   CLEAR                   TRUL43
405200171122     C                   CLEAR                   TRUL43_1
405300050614    2C     T2F(X)        IFEQ      'F'
405400050614    2C     T2F(X)        orEQ      'M'
405500160223      *solo x la Mail
405600160223    2C     T2F(X)        ifEQ      'M'
405700160223      *
405800160223      * controlla se si tratta di una bolla DPD con tanto di PARCEL
405900160223      *   da cui prendere il DEPOT
406000160223     c                   clear                   fnlg24ds
406100160223     c                   z-add     V1CAAS        i24aas
406200160223     c                   z-add     V1CLNP        i24lnp
406300160223     c                   z-add     V1CNRS        i24nrs
406400160223     c                   z-add     V1CNSP        i24nsp
406500160223      *
406600160223     c                   call      'FNLG24R'
406700160223     C                   parm                    fnlg24ds
406800160223      *
406900160223     c*  se c'è il parcel allora deve passare il Depot e verificare
407000160223     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
407100160223     c*  l'informazione di apertura giacenza.
407200160223     c                   if          o24npr <> *blank
407300160303     c                   if        %subst(o24npr:1:4)<>'0844' and
407400160303     c                             %subst(o24npr:1:4)<>'0845'
407500171122      *--
407600171122     c                   movel     o24dep4       D43DEPOT
407700171122     c                   if         D43DEPOT <> *blank
407800171122     c                   movel     o24npr        D43DEPOT
407900171122     c                   end
408000171122     c                   movel     o24dep7       D43DEP7
408100171122      *--
408200160303     c                   end
408300160223     c                   end
408400160223     c*
408500160223     c                   endIF
408600160223      *
408700050614     C                   MOVE      v2cscm        §QUATR
408800050614    3C                   IF        §quatr <> *ALL'8' and §quatr <> *all'9'
408900050614     C                   MOVE      V2CSCM        D43KSC
409000050614     C                   CALL      'TRUL43R'
409100050614     C                   PARM                    TRUL43
409200171122     C                   PARM                    TRUL43_1
409300171122      *---
409400050513     c                   end
409500050614     c                   end
409600050614     c                   end
409700050513     c                   end
409800050608     c                   end
409900050606     c*
410000050217     C                   ENDSR
410100050217     C*---------------------------------------------------------------*
410200050217     C*   ERRORE CARICAMENTO TABELLE                                  *
410300050217     C*---------------------------------------------------------------*
410400050217     C     ERRTAB        BEGSR
410500940915     C*
410600940915     C                   MOVEL     ERR(1)        DSMSG
410700940915     C                   MOVEL     KCOD          CDTAB
410800940915     C                   MOVEL     DSMSG         $MSG
410900940915     C                   Z-ADD     D05AAS        V3CAAS
411000940915     C                   Z-ADD     D05LNP        V3CLNP
411100940915     C                   Z-ADD     D05NRS        V3CNRS
411200940915     C                   Z-ADD     D05NSP        V3CNSP
411300940915     C     D05AGC        IFEQ      0
411400940920     C     D05RIC        ANDEQ     'S'
411500940915     C                   Z-ADD     D05AAS        KAAS
411600940915     C                   Z-ADD     D05LNP        KLNP
411700940915     C                   Z-ADD     D05NRS        KNRS
411800940915     C                   Z-ADD     D05NSP        KNSP
411900940915     C                   Z-ADD     0             GCPAGC
412000940915     C                   Z-ADD     0             GCPFGC
412100940915     C                   Z-ADD     0             GCPNGC
412200050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
412300050315     c                   if        %error
412400070521     C     KSPE2         CHAIN(n)  tIGCP01L
412500050315     c                   exsr      geslck
412600050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
412700050315     c                   endif
412800940915     C                   Z-ADD     GCPAGC        V3CAGC
412900940915     C                   Z-ADD     GCPFGC        V3CFGC
413000940915     C                   Z-ADD     GCPNGC        V3CNGC
413100940915     C                   ELSE
413200940915     C                   Z-ADD     D05AGC        V3CAGC
413300940915     C                   Z-ADD     D05FGC        V3CFGC
413400940915     C                   Z-ADD     D05NGC        V3CNGC
413500940915     C                   END
413600940915     C*
413700940915     C                   SETON                                        28
413800940915     C                   EXFMT     LG05D03
413900940915     C                   MOVEL     'S'           WFINE
414000940915     C                   ENDSR
414100970226     C*---------------------------------------------------------------*
414200970226     C*   ERRORE CARICAMENTO TABELLE  EDI                             *
414300970226     C*---------------------------------------------------------------*
414400970226     C     ERREDI        BEGSR
414500970226     C*
414600970226     C                   MOVEL     ERR(22)       DSMSG
414700970226     C                   MOVEL     KCOD1         CDTAB
414800970226     C                   MOVEL     DSMSG         $MSG
414900970226     C                   Z-ADD     D05AAS        V3CAAS
415000970226     C                   Z-ADD     D05LNP        V3CLNP
415100970226     C                   Z-ADD     D05NRS        V3CNRS
415200970226     C                   Z-ADD     D05NSP        V3CNSP
415300970226     C     D05AGC        IFEQ      0
415400970226     C     D05RIC        ANDEQ     'S'
415500970226     C                   Z-ADD     D05AAS        KAAS
415600970226     C                   Z-ADD     D05LNP        KLNP
415700970226     C                   Z-ADD     D05NRS        KNRS
415800970226     C                   Z-ADD     D05NSP        KNSP
415900970226     C                   Z-ADD     0             GCPAGC
416000970226     C                   Z-ADD     0             GCPFGC
416100970226     C                   Z-ADD     0             GCPNGC
416200050315     C     KSPE2         CHAIN(e)  tIGCP01L                           31
416300050315     c                   if        %error
416400070521     C     KSPE2         CHAIN(n)  tIGCP01L
416500050315     c                   exsr      geslck
416600050318     C     KSPE2         CHAIN(e)  tIGCP01L                           31
416700050315     c                   endif
416800970226     C                   Z-ADD     GCPAGC        V3CAGC
416900970226     C                   Z-ADD     GCPFGC        V3CFGC
417000970226     C                   Z-ADD     GCPNGC        V3CNGC
417100970226     C                   ELSE
417200970226     C                   Z-ADD     D05AGC        V3CAGC
417300970226     C                   Z-ADD     D05FGC        V3CFGC
417400970226     C                   Z-ADD     D05NGC        V3CNGC
417500970226     C                   END
417600970226     C*
417700970226     C                   SETON                                        28
417800970226     C                   EXFMT     LG05D03
417900970226     C                   MOVEL     'S'           WFINE
418000970226     C                   ENDSR
418100940915     C*---------------------------------------------------------------*
418200940915     C*   OPERAZIONI INIZIALI                                         *
418300940915     C*---------------------------------------------------------------*
418400940927     C     DEFCAM        BEGSR
418500940915     C*
418600020813     c                   exsr      repute
418700940915     C*
418800050315      *flag per sapere se devo eseguire il commit o lo esegue chi mi ha chiamato
418900050315     c                   move      d05comm       gescomm           1
419000940920     C                   MOVEL     D05GES        SAVGES            1
419100940920     C     D05RIC        IFNE      'S'
419200940919     C                   CLEAR                   FNLG05
419300940920     C                   MOVEL     SAVGES        D05GES
419400940915     C                   END
419500940915     C* Se sono in arrivo apro bolle in arrivo se sono in partenza
419600940915     C* apro bolle in partenza
419700940919     C                   SETOFF                                       01
419800940919     C     D05GES        IFEQ      'A'
419900940915     C                   OPEN      FNARB01L
420000990929     C                   OPEN      FIAR601L
420100051116     C                   OPEN      FIAR901L
420200940920     C                   MOVEL     ' IN ARR'     V1CGES
420300940920     C                   MOVE      'IVO '        V1CGES
420400940919     C                   SETON                                        01
420500940915     C                   ELSE
420600990929     C                   OPEN      FIAR601L
420700051116     C                   OPEN      Fiar901L
420800940920     C                   MOVEL     'IN PART'     V1CGES
420900940920     C                   MOVE      'ENZA'        V1CGES
421000940915     C                   END
421100940915     C* Richiamo XPARUT
421200940915     C                   Z-ADD     1             CODUT
421300940915     C                   CALL      'X§PARUT'
421400940915     C                   PARM                    UTEDSE
421500940915     C                   MOVEL     REC80         CNCR80
421600940919     C     CODUT         CABEQ     -1            FINE
421700940915     C                   MOVEL     RAGUT         V1CRSU
421800940920     C                   MOVEL     '* FNLG05'    V1CPGM
421900940915     C                   MOVE      'R *'         V1CPGM
422000940915     C                   MOVEL     RAGUT         PARDUT           30
422100940915     C*--- RICERCA CAPOCONTI------------------------------------------*
422200071107     C                   DO        50            X
422300940915     C                   MOVE      TCU(X)        TCUDS
422400940915     C     F56           IFEQ      'CG'
422500940915     C     F4            ANDEQ     '1'
422600940915     C     F3            ANDEQ     '0'
422700940915     C                   Z-ADD     KCU(X)        KCI               4 0
422800940915     C                   END
422900940915     C                   END
423000940915     C*---------------------------------------------------------------*
423100940915     C*  KLIST
423200940919     C     KSPE1         KLIST
423300940915     C                   KFLD                    KAAS
423400940915     C                   KFLD                    KLNP
423500940915     C                   KFLD                    KNRS
423600940915     C                   KFLD                    KNSP
423700050517     C     Kblp          KLIST
423800050517     C                   KFLD                    £LBLAAO
423900050517     C                   KFLD                    £LBLLPO
424000050517     C                   KFLD                    £LBLNRO
424100050517     C                   KFLD                    £LBLNSO
424200990929     C     KAR6          KLIST
424300990929     C                   KFLD                    KAAS
424400990929     C                   KFLD                    KLNP
424500990929     C                   KFLD                    KNRS
424600990929     C                   KFLD                    KNSP
424700990929     C                   KFLD                    KTRC
424800940915     C*
424900940919     C     KSPE2         KLIST
425000940915     C                   KFLD                    KAAS
425100940915     C                   KFLD                    KLNP
425200940915     C                   KFLD                    KNRS
425300940915     C                   KFLD                    KNSP
425400940915     C                   KFLD                    KFRG
425500940915     C*
425600940915     C     KTBL1         KLIST
425700940915     C                   KFLD                    KKUT
425800940915     C                   KFLD                    KCOD
425900940915     C                   KFLD                    KKEY
426000940915     C*
426100940915     C     KTBL2         KLIST
426200940915     C                   KFLD                    KKUT
426300940915     C                   KFLD                    KCOD
426400940915     C*
426500940915     C     KACO          KLIST
426600940915     C                   KFLD                    KKUT
426700940915     C                   KFLD                    KKCC
426800940915     C                   KFLD                    KKSC
426900940915     C*
427000940915     C     KGNP          KLIST
427100940915     C                   KFLD                    KAGC
427200940915     C                   KFLD                    KFGC
427300940915     C                   KFLD                    KNGC
427400940915     C                   KFLD                    KFRG
427500960208     C*
427600960208     C     KGNP1         KLIST
427700960208     C                   KFLD                    KAGC
427800960208     C                   KFLD                    KFGC
427900960208     C                   KFLD                    KNGC
428000960208     C                   KFLD                    KFRG
428100960208     C                   KFLD                    KFAS
428200940915     C*
428300940915     C     KNUF          KLIST
428400940915     C                   KFLD                    KAAA
428500940915     C                   KFLD                    KCNU
428600940915     C                   KFLD                    KFGC
428700940915     C                   Z-ADD     2             KCNU
428800940919     C*
428900940919     C     KAR4          KLIST
429000940919     C                   KFLD                    KAAS
429100940919     C                   KFLD                    KLNP
429200940919     C                   KFLD                    KNRS
429300940919     C                   KFLD                    KNSP
429400940919     C                   KFLD                    KTRC
429500940922     C*
429600070320     C     Kar4l         klist
429700070320     C                   kfld                    gcpAASa
429800070320     C                   kfld                    gcpLNPa
429900070320     C                   kfld                    gcpNRSa
430000070320     C                   kfld                    gcpNSPa
430100070320     C                   kfld                    KTRCl
430200070320     C                   move      'L'           KTRCl             1
430300070320     C*
430400940922     C     KTGC          KLIST
430500940922     C                   KFLD                    KKSC
430600940922     C                   KFLD                    KCTR
430700940922     C                   KFLD                    KPRG
430800950105     C*
430900950105     C     KEVB1         KLIST
431000990803     C                   KFLD                    KAAS
431100950105     C                   KFLD                    KLNP
431200950105     C                   KFLD                    KNRS
431300950105     C                   KFLD                    KNSP
431400960216     C                   KFLD                    KDEV
431500950105     C*
431600950105     C     KEVB2         KLIST
431700990803     C                   KFLD                    KAAS
431800950105     C                   KFLD                    KLNP
431900950105     C                   KFLD                    KNRS
432000950105     C                   KFLD                    KNSP
432100961113     C     KRDE          KLIST
432200961113     C                   KFLD                    KAAS
432300961113     C                   KFLD                    KLNP
432400961113     C                   KFLD                    KNRS
432500961113     C                   KFLD                    KNSP
432600961113     C                   KFLD                    KNMC
432700970401     C     KANM          KLIST
432800970401     C                   KFLD                    KAAS2
432900970401     C                   KFLD                    KLNP2
433000970401     C                   KFLD                    KNRS2
433100970401     C                   KFLD                    KNSP2
433200970401     C                   KFLD                    KCAA
433300040713     C     Kntc          KLIST
433400040713     C                   KFLD                    kcapl
433500040713     C                   KFLD                    kcnk1
433600040713     C                   KFLD                    kcnk2
433700040713     C                   KFLD                    kctnt
433800040713     c                   move      'C'           kcapl
433900040713     c                   movel     '0151'        kcnk1
434000040713     c                   move      '88'          kctnt
434100091021     C     Kar5          KLIST
434200091021     C                   KFLD                    ARbAAS
434300091021     C                   KFLD                    ARbLNP
434400091021     C                   KFLD                    ARbNRS
434500091021     C                   KFLD                    ARbNSP
434600091021     C                   KFLD                    k5trd             3
434700091021     C                   movel     'GEN'         k5trd
434800091021     C     Kar5p         KLIST
434900091021     C                   KFLD                    blpAAS
435000091021     C                   KFLD                    blpLNP
435100091021     C                   KFLD                    blpNRS
435200091021     C                   KFLD                    blpNSP
435300091021     C                   KFLD                    k5trd
435400940915     C*---------------------------------------------------------------*
435500940915     C*  LIKE DEFN
435600040713     C     *LIKE         DEFINE    NTCAPL        kcapl
435700040713     C     *LIKE         DEFINE    NTCNK1        kcnk1
435800040713     C     *LIKE         DEFINE    NTCNK2        kcnk2
435900040713     C     *LIKE         DEFINE    NTCTNT        kctnt
436000950105     C     *LIKE         DEFINE    EVBCEV        KCEV
436100950105     C     *LIKE         DEFINE    EVBDEV        KDEV
436200950105     C     *LIKE         DEFINE    GCPAAS        KAAS
436300940915     C     *LIKE         DEFINE    GCPLNP        KLNP
436400940915     C     *LIKE         DEFINE    GCPNRS        KNRS
436500940915     C     *LIKE         DEFINE    GCPNSP        KNSP
436600940915     C     *LIKE         DEFINE    GCPFRG        KFRG
436700940919     C     *LIKE         DEFINE    AR4TRC        KTRC
436800940915     C     *LIKE         DEFINE    ACOKUT        KKUT
436900940915     C     *LIKE         DEFINE    TBLCOD        KCOD
437000940915     C     *LIKE         DEFINE    TBLKEY        KKEY
437100970226     C     *LIKE         DEFINE    TABCOD        KCOD1
437200940915     C     *LIKE         DEFINE    ACOKCC        KKCC
437300940915     C     *LIKE         DEFINE    ACOKSC        KKSC
437400950113     C     *LIKE         DEFINE    TGCPRG        KPRG
437500950113     C     *LIKE         DEFINE    TGCCTR        KCTR
437600940915     C     *LIKE         DEFINE    GCPAGC        KAGC
437700940915     C     *LIKE         DEFINE    GCPFGC        KFGC
437800940915     C     *LIKE         DEFINE    GCPNGC        KNGC
437900940915     C     *LIKE         DEFINE    NUFAAA        KAAA
438000940915     C     *LIKE         DEFINE    NUFCNU        KCNU
438100940919     C     *LIKE         DEFINE    ORGFIL        KFIL
438200960208     C     *LIKE         DEFINE    GCPFAS        KFAS
438300940915     C     *LIKE         DEFINE    GCPSCM        §SCM
438400940919     C     *LIKE         DEFINE    GCPSCM        SAVSCM
438500940915     C     *LIKE         DEFINE    GAPRSM        SAVRSM
438600940915     C     *LIKE         DEFINE    GAPINM        SAVINM
438700940915     C     *LIKE         DEFINE    GAPLOM        SAVLOM
438800940915     C     *LIKE         DEFINE    GAPPRM        SAVPRM
438900940915     C     *LIKE         DEFINE    GAPCAM        SAVCAM
439000940919     C     *LIKE         DEFINE    GAPNZM        SAVNZM
439100940915     C     *LIKE         DEFINE    GCPDXD        AMGDTG
439200940915     C     *LIKE         DEFINE    GCPDXD        AMGDXD
439300940915     C     *LIKE         DEFINE    GCPDXD        AMGDSP
439400961113     C     *LIKE         DEFINE    RDENMC        KNMC
439500970401     C     *LIKE         DEFINE    ANMAAS        KAAS2
439600970401     C     *LIKE         DEFINE    ANMLNP        KLNP2
439700970401     C     *LIKE         DEFINE    ANMNRS        KNRS2
439800970401     C     *LIKE         DEFINE    ANMNSP        KNSP2
439900970401     C     *LIKE         DEFINE    ANMCAA        KCAA
440000940915     C*---------------------------------------------------
440100940915     C* Creo data del giorno
440200940920     C                   TIME                    WHHDAT           14 0
440300940920     C                   MOVE      WHHDAT        G02DAT
440400940920     C                   MOVE      *ZEROS        G02INV
440500940920     C                   MOVEL     *BLANK        G02ERR
440600940920     C                   CALL      'XSRDA8'
440700940920     C                   PARM                    WLBDA8
440800940920     C                   Z-ADD     G02INV        WOGGI             8 0
440900940920     C                   Z-ADD     G02INV        DATEU             6 0
441000940915     C* Inizializzo variabili
441100950206     C                   MOVEL     KNMUS         §SEI              6
441200020730     C**** §PRF          COMP      'REM'                                  02
441300020730     C**N02              Z-ADD     SIMFEL        §FLE
441400020730     C                   Z-ADD     SIMpou        §FLE
441500940919     C                   MOVEL     *BLANKS       §DES             60
441600940915     C                   Z-ADD     1             KKUT
441700940915     C                   Z-ADD     KCI           KKCC
441800940927     C                   Z-ADD     D05FRG        KFRG
441900940920     C                   MOVE      §FLE          PA0SIM
442000941003     C                   CLEAR                   DSTB69
442100940915     C*---------------------------------------------------
442200940915     C* Eseguo caricamento tabelle
442300940915     C                   EXSR      CARTAB
442400940915     C*---------------------------------------------------
442500940927     C                   ENDSR
442600020813     C*-------------------------------------------------------------------------
442700020813     C* Reperisce i dati dell'utente
442800020813     C*-------------------------------------------------------------------------
442900020813     C     Repute        BEGSR
443000020813     C*
443100020813     C* Azzera le variabili di lavoro
443200021114     C                   CLEAR                   Tibs36Ds
443300021114     c                   EVAL      I36ute = knmus
443400021114     c                   EVAL      I36Tla = 'L'
443500021114     C                   CALL      'TIBS36R'
443600021114     C                   PARM                    Tibs36Ds
443700020813     C*
443800020813     C                   ENDSR
443900940927     C*---------------------------------------------------------------*
444000940927     C*   INIZIO PGM.                                                 *
444100940927     C*---------------------------------------------------------------*
444200940927     C     INIPGM        BEGSR
444300940927     C*
444400940915     C     WFINE         IFNE      'S'
444500941223     C                   MOVEL     ' '           WRIAPE            1
444600941223     C                   SETOFF                                       28
444700941223     C                   MOVEL     *BLANKS       $MSG
444800940919     C*  Pulizia parametri a video
444900940919     C                   EXSR      PULVID
445000940919     C                   MOVEL     '1'           WTPVID
445100940915     C*  Se il programma è richiamato imposto gestione prima videata
445200940920     C     D05RIC        IFEQ      'S'
445300940919     C                   MOVEL     '2'           WTPVID            1
445400940915     C                   Z-ADD     D05AAS        KAAS
445500940915     C                   Z-ADD     D05LNP        KLNP
445600940915     C                   Z-ADD     D05NRS        KNRS
445700940915     C                   Z-ADD     D05NSP        KNSP
445800940915     C*  Richiamo routine controllo dati spedizione
445900940915     C                   EXSR      CTRGIA
446000940915     C                   END
446100940915     C                   END
446200940915     C*
446300000526     C* CARICO DA AZORG LE FILIALI POSTE PER TEST SUCCESSIVI
446400000526     C                   CLEAR                   FIP
446500000526     C     *LOVAL        SETLL     AZORG01L
446600000526     C                   DO        *HIVAL
446700000526     C                   READ      AZORG01L                               95
446800000526     C   95              LEAVE
446900000526     C                   MOVEL     ORGDE3        OG143
447000020729     C     §OgNTW        IFEQ      'PPT'
447100000526     C                   Z-ADD     1             X
447200000526     C     *ZEROS        LOOKUP    FIP(X)                                 23
447300000526     C   23              Z-ADD     ORGFIL        FIP(X)
447400000526     C                   END
447500000526     C                   END
447600940915     C                   ENDSR
447700050315     C*----------------------------------------------------*
447800050315     C     geslck        begsr
447900050315     C*----------------------------------------------------*
448000050315     c                   Eval      ul82§rrn = gcpnrr
448100050315     c                   Eval      UL82§FIL = 'TIGCP00F'
448200050315     c                   Eval      UL82§WIN = 'S'
448300050315     c                   Eval      UL82§F7  = 'S'
448400050315     c                   Eval      UL82§num = 3
448500050315     c                   Eval      UL82§att = 3
448600050404     c                   Eval      UL82§mss = 'Si sta bloccando la giacenza rel-
448700050404     c                             lativa alla spedizione '
448800050404     c                             + %editc(kaas:'Z') +  ' '+ %editc(klnp:'Z')
448900050404     c                             + ' ' +  %editc(knrs:'Z') + ' ' +
449000050405     c                             %editc(knsp:'Z')
449100050404     c                   Eval      UL82§msw = 'La giacenza relativa alla sped.'
449200050404     c                             + %editc(kaas:'Z') +  ' '+ %editc(klnp:'Z')
449300050404     c                             + ' ' +  %editc(knrs:'Z') + ' ' +
449400050404     c                             %editc(knsp:'Z') + ' non è manutenzionabile'
449500050315      * Effettuo la chiamata al *PGM d utilità
449600050315     c                   Call(e)   'TRUL82R'
449700050315     c                   Parm                    Trul82ds
449800050315     c                   if        ul82§sts = 'A'
449900050315     c                   goto      fine
450000050315     c                   endif
450100050315
450200050315     C                   ENDSR
450300900418     C*----------------------------------------------------*
450400940919     OFNARB000  E            AGG
450500911017     O                       ARBCMC
450600911107     O                       ARBFBC
450700021031     OFNARB000  E            liberaarb
450800940919     OFLNUF     E            AGGNUF
450900940919     O                       NUFNUM
451000961204     OFNBLP000  E            SFLEGG
451100961204     O                       BLPDT3
451200961204     O                       BLPFT3
451300970225**
451400940915Impossibile proseguire nr.elementi caricamento tab.   non sufficienti 01
451500050408Giacenza non manutenzionabile fase non congruente                     02
451600940920Impossibile aprire giacenza in partenza                               03
451700940915Bolla figlia !! Per aprire giacenza agire da distinta di consegna     04
451800940915Giacenza non manutenzionabile: già stampata lettera mittente          05
451900050404Bolla inesistente o non disponibile!!  Impossibile gestire giacenza   06
452000940915Attenzione !!!  Bolla di rientro non si può aprire giacenza           07
452100940915Bolla con consegna parziale !!! Impossibile aprire giacenza           08
452200940915Attenzione !!!  Bolla in consegna, non si può aprire giacenza         09
452300940919Codice cliente errato !!!                                             10
452400000404Attenzione !!!  Sedicesima riapertura, avvisare CED di sede !!!!      11
452500031027Tipo comunicazione non presente in tabella o non utilizzabile         12
452600940921Codice motivo giacenza non presente o non ammesso                     13
452700940919Ulteriore motivazione obbligatoria                                    14
452800940920Luogo deposito merce errato                                           15
452900961028Giacenza non manutenzionabile: Punto operativo non in gestione        16
453000950228Attenzione !!!  Merce già consegnata non si può aprire giacenza       17
453100960201Tp.invio comunicazione apertura non utilizzabile per clienti generici 18
453200960201Tp.invio comunicazione apertura non utilizzabile: numero fax errato   19
453300961028Punto operativo non in gestione                                       20
453400961113Motivo di giacenza generico non ammesso per bolle import              21
453500970226Impossibile proseguire numero elementi tabella EDI    non sufficienti 22
453600970310Modifica ulteriore motivazione obbligatoria per partner-evento EDI    23
453700970624La particolarità non permette l'apertura giacenza note non ammesse    24
453800990203Bolla con pratica C.A. aperta ! Impossibile aprire giacenza           25
453900990805Reso automatico bloccato per il periodo in corso                      26
454000000526FILIALE POSTE NON GESTIBILE !!!!                                      27
454100021031Il tipo bolla non permette l'apertura della giacenza                  28
454200040113La particolarità consegna non permette di aprire giacenza             29
454300170410Motivo giacenza non ammesso bolla con DDT = SI o PACKING LIST         30
454400040402Presente codice mancata consegna agire da aperture da distinta        31
454500040713Tp.invio comunicazione apertura non utilizzabile: IND.E-MAIL errato   32
454600050609ERRORI!!! La giacenza non è stata aperta o aggiornata F12=Ritorno     33
454700050629Si sta tentando di fare un'apertura tecnica oltre il limite consentito34
454800110531Codice motivo giacenza non ammesso con questa particolarità           35
454900141029Esistono delle disposizioni di consegna che devono essere risolte     36
455000151204PRESENTI disposizioni Dirottamento/Reso spedizione non gestibile      37
455100170412Motivo giacenza non ammesso se non presente contrassegno              38
455200170412Motivo giacenza non ammesso se spedizione in porto franco             39
455300170530Errore la giacenza è già stata aperta da un altro utente              40
455400961217**
455500961217CHKOBJ OBJ(EDRDE01L) OBJTYPE(*FILE)
455600970225CHKOBJ OBJ(EDTAB01L) OBJTYPE(*FILE)
455700040329**         CMA4
455800040329OVRPRTF FNLSB5PA4 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle-A4')
455900040329**         CMA5
456000040329OVRPRTF FNLSB5PA5 OUTQ(          ) FORMTYPE('          ') USRDTA('Bolle-A5')
